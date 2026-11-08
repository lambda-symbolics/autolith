(in-package #:autolith)

;;;; -- Tuning Experiments --

(defparameter *tuning-experiment-text-limit* 2000
  "Maximum journaled length of one tuning experiment text field.")

(defclass tuning-experiment ()
  ((identifier
    :initarg       :identifier
    :reader        tuning-experiment-identifier
    :type          non-empty-string
    :documentation "The stable identifier joining this experiment's journal records.")
   (mutation-identifier
    :initarg       :mutation-identifier
    :reader        tuning-experiment-mutation-identifier
    :type          non-empty-string
    :documentation "The one effective exploratory mutation under measurement.")
   (mutation-kind
    :initarg       :mutation-kind
    :reader        tuning-experiment-mutation-kind
    :type          keyword
    :documentation "The bound mutation kind, either SET or DEFINITION.")
   (mutation-target
    :initarg       :mutation-target
    :reader        tuning-experiment-mutation-target
    :type          non-empty-string
    :documentation "The semantic target of the bound exploratory mutation.")
   (hypothesis
    :initarg       :hypothesis
    :reader        tuning-experiment-hypothesis
    :type          non-empty-string
    :documentation "The proposed explanation motivating this tuning change.")
   (criterion
    :initarg       :criterion
    :reader        tuning-experiment-criterion
    :type          non-empty-string
    :documentation "The expected effect and measurement criterion.")
   (verdict
    :initarg       :verdict
    :accessor      tuning-experiment-verdict
    :type          keyword
    :documentation "The latest experiment verdict or OPEN before settlement.")
   (observation
    :initarg       :observation
    :initform      nil
    :accessor      tuning-experiment-observation
    :type          (option string)
    :documentation "The latest bounded settlement observation, when present."))
  (:documentation "One append-only evaluation of an exploratory self mutation."))

(-> tuning-experiment-open-p (tuning-experiment) boolean)
(defun tuning-experiment-open-p (experiment)
  "Return true when EXPERIMENT still accepts evidence or settlement."
  (and (member (tuning-experiment-verdict experiment)
               '(:open :too-early)
               :test #'eq)
       t))

(-> tuning-experiment--required-text (t string string) string)
(defun tuning-experiment--required-text (value field tool-name)
  "Return bounded nonblank VALUE or signal a protocol error naming FIELD."
  (unless (and (stringp value)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           value))))
    (error 'source-mutation-error
           :message   (format nil "Tuning experiment ~A must be nonempty." field)
           :tool-name tool-name
           :pathname  nil))
  (subseq value 0 (min (length value) *tuning-experiment-text-limit*)))

(-> tuning-experiment--verdict (string) keyword)
(defun tuning-experiment--verdict (name)
  "Return the supported tuning verdict named by NAME."
  (or (find name '(:better :worse :unchanged :too-early)
            :key  #'symbol-name
            :test #'string-equal)
      (error 'source-mutation-error
             :message
             "A tuning verdict must be better, worse, unchanged, or too-early."
             :tool-name "self.experiment-settle"
             :pathname  nil)))

(-> tuning-experiment--journal-record-p (t) boolean)
(defun tuning-experiment--journal-record-p (record)
  "Return true when RECORD claims to describe a tuning experiment."
  (and (listp record)
       (eq (first record) :mutation)
       (eq (getf (rest record) :kind) :tuning-experiment)
       t))

(-> tuning-experiment--record-valid-p (list) boolean)
(defun tuning-experiment--record-valid-p (record)
  "Return true when tuning experiment RECORD has a complete portable shape."
  (let* ((properties  (rest record))
         (verdict     (getf properties :result))
         (observation (getf properties :observation)))
    (and (non-empty-string-p (getf properties :id))
         (non-empty-string-p (getf properties :lineage))
         (non-empty-string-p (getf properties :mutation))
         (member (getf properties :mutation-kind)
                 '(:definition :set)
                 :test #'eq)
         (non-empty-string-p (getf properties :target))
         (non-empty-string-p (getf properties :hypothesis))
         (non-empty-string-p (getf properties :criterion))
         (member verdict
                 '(:open :better :worse :unchanged :too-early)
                 :test #'eq)
         (if (eq verdict :open)
             (null observation)
             (and (non-empty-string-p observation)
                  (<= (length observation) *tuning-experiment-text-limit*)))
         t)))

(-> tuning-experiment--record-matches-p (tuning-experiment list) boolean)
(defun tuning-experiment--record-matches-p (experiment properties)
  "Return true when PROPERTIES preserve EXPERIMENT's immutable identity."
  (and (string= (tuning-experiment-mutation-identifier experiment)
                (getf properties :mutation))
       (eq (tuning-experiment-mutation-kind experiment)
           (getf properties :mutation-kind))
       (string= (tuning-experiment-mutation-target experiment)
                (getf properties :target))
       (string= (tuning-experiment-hypothesis experiment)
                (getf properties :hypothesis))
       (string= (tuning-experiment-criterion experiment)
                (getf properties :criterion))
       t))

(-> tuning-experiment--query (configuration &key (:open-only boolean)) list)
(defun tuning-experiment--query (configuration &key open-only)
  "Reduce experiment lifecycles in journal order.
OPEN-ONLY retains one experiment and completed identity markers, rather than
reconstructing all historical experiment objects."
  (let ((experiments     (make-hash-table :test #'equal))
        (order           nil)
        (open-experiment nil))
    (mutation-journal-query
     configuration
     (lambda (record)
       (when (tuning-experiment--journal-record-p record)
         (unless (tuning-experiment--record-valid-p record)
           (error 'source-mutation-error
                  :message   "A tuning experiment journal record is invalid."
                  :tool-name "self.status"
                  :pathname  (configuration-journal-path configuration)))
         (let* ((properties (rest record))
                (identifier (getf properties :id))
                (verdict    (getf properties :result))
                (existing   (gethash identifier experiments)))
           (if (eq verdict :open)
               (progn
                 (when (or existing open-experiment)
                   (error 'source-mutation-error
                          :message
                          "The mutation journal contains overlapping tuning experiments."
                          :tool-name "self.status"
                          :pathname  (configuration-journal-path configuration)))
                 (let ((experiment
                         (make-instance
                          'tuning-experiment
                          :identifier          identifier
                          :mutation-identifier (getf properties :mutation)
                          :mutation-kind       (getf properties :mutation-kind)
                          :mutation-target     (getf properties :target)
                          :hypothesis          (getf properties :hypothesis)
                          :criterion           (getf properties :criterion)
                          :verdict             ':open)))
                   (setf (gethash identifier experiments) experiment
                         open-experiment experiment)
                   (unless open-only (push identifier order))))
               (progn
                 (unless (and existing
                              (eq existing open-experiment)
                              (tuning-experiment-open-p existing)
                              (tuning-experiment--record-matches-p
                               existing properties))
                   (error 'source-mutation-error
                          :message
                          "A tuning experiment journal transition is invalid."
                          :tool-name "self.status"
                          :pathname  (configuration-journal-path configuration)))
                 (setf (tuning-experiment-verdict existing) verdict
                       (tuning-experiment-observation existing)
                       (getf properties :observation))
                 (unless (eq verdict :too-early)
                   (when open-only
                     (setf (gethash identifier experiments) t))
                   (setf open-experiment nil))))))))
    (if open-only
        (when open-experiment (list open-experiment))
        (loop for identifier in (nreverse order)
              collect (gethash identifier experiments)))))

(-> tuning-experiment-list (configuration) list)
(defun tuning-experiment-list (configuration)
  "Return experiments in first-appearance order after validating their lifecycles."
  (tuning-experiment--query configuration))

(-> tuning-experiment-open (configuration) (option tuning-experiment))
(defun tuning-experiment-open (configuration)
  "Return CONFIGURATION's sole open tuning experiment, when present."
  (first (tuning-experiment--query configuration :open-only t)))

(-> tuning-experiment-for-mutation
    (configuration (option string))
    (option tuning-experiment))
(defun tuning-experiment-for-mutation (configuration mutation-identifier)
  "Return the open experiment bound to MUTATION-IDENTIFIER, when present."
  (let ((experiment (tuning-experiment-open configuration)))
    (and experiment
         mutation-identifier
         (string= mutation-identifier
                  (tuning-experiment-mutation-identifier experiment))
         experiment)))

(-> tuning-experiment-assert-mutation-installable (configuration string) null)
(defun tuning-experiment-assert-mutation-installable (configuration tool-name)
  "Reject TOOL-NAME when another exploratory mutation is under experiment."
  (let ((experiment (tuning-experiment-open configuration)))
    (when experiment
      (error 'source-mutation-error
             :message
             (format nil
                     "Tuning experiment ~A is open for mutation ~A; settle it before installing another exploratory mutation."
                     (tuning-experiment-identifier experiment)
                     (tuning-experiment-mutation-identifier experiment))
             :tool-name tool-name
             :pathname  (configuration-journal-path configuration))))
  nil)

(-> tuning-experiment-assert-settled (configuration string) null)
(defun tuning-experiment-assert-settled (configuration tool-name)
  "Reject TOOL-NAME while an exploratory mutation still lacks a terminal verdict."
  (let ((experiment (tuning-experiment-open configuration)))
    (when experiment
      (error 'source-mutation-error
             :message
             (format nil
                     "Tuning experiment ~A is still ~A for mutation ~A; record a terminal verdict before committing or discarding it."
                     (tuning-experiment-identifier experiment)
                     (tuning-experiment-verdict experiment)
                     (tuning-experiment-mutation-identifier experiment))
             :tool-name tool-name
             :pathname  (configuration-journal-path configuration))))
  nil)

(-> tuning-experiment-start
    (configuration string &key (:hypothesis string) (:criterion string))
    tuning-experiment)
(defun tuning-experiment-start
    (configuration mutation-identifier &key hypothesis criterion)
  "Start one tuning experiment bound to effective MUTATION-IDENTIFIER."
  (with-live-mutation
    (when (tuning-experiment-open configuration)
      (error 'source-mutation-error
             :message   "A tuning experiment is already open."
             :tool-name "self.experiment-start"
             :pathname  (configuration-journal-path configuration)))
    (let* ((hypothesis
             (tuning-experiment--required-text
              hypothesis "hypothesis" "self.experiment-start"))
           (criterion
             (tuning-experiment--required-text
              criterion "criterion" "self.experiment-start"))
           (record
             (find mutation-identifier
                   (image-commit-effective-diff-records configuration)
                   :key  (lambda (candidate)
                           (getf (rest candidate) :id))
                   :test #'string=)))
      (unless record
        (error 'source-mutation-error
               :message
               (format nil
                       "No effective pending self.set or self.redefine mutation is named ~A."
                       mutation-identifier)
               :tool-name "self.experiment-start"
               :pathname  (configuration-journal-path configuration)))
      (let* ((properties (rest record))
             (identifier (make-identifier)))
        (mutation-journal-append
         configuration
         (list :mutation
               :kind :tuning-experiment
               :id identifier
               :lineage       *active-image-lineage-identifier*
               :mutation      mutation-identifier
               :mutation-kind (getf properties :kind)
               :target        (getf properties :target)
               :hypothesis    hypothesis
               :criterion     criterion
               :result        ':open))
        (or (find identifier
                  (tuning-experiment-list configuration)
                  :key  #'tuning-experiment-identifier
                  :test #'string=)
            (error 'source-mutation-error
                   :message   "The tuning experiment journal did not round-trip."
                   :tool-name "self.experiment-start"
                   :pathname  (configuration-journal-path configuration)))))))

(-> tuning-experiment-settle
    (configuration (option string) &key (:verdict-name string) (:observation string))
    tuning-experiment)
(defun tuning-experiment-settle
    (configuration experiment-identifier &key verdict-name observation)
  "Settle the open experiment with VERDICT-NAME and bounded OBSERVATION."
  (with-live-mutation
    (let* ((experiment (tuning-experiment-open configuration))
           (verdict    (tuning-experiment--verdict verdict-name))
           (observation
             (tuning-experiment--required-text
              observation "observation" "self.experiment-settle")))
      (unless experiment
        (error 'source-mutation-error
               :message   "There is no open tuning experiment to settle."
               :tool-name "self.experiment-settle"
               :pathname  (configuration-journal-path configuration)))
      (when (and experiment-identifier
                 (not (string= experiment-identifier
                               (tuning-experiment-identifier experiment))))
        (error 'source-mutation-error
               :message
               (format nil "The open tuning experiment is ~A, not ~A."
                       (tuning-experiment-identifier experiment)
                       experiment-identifier)
               :tool-name "self.experiment-settle"
               :pathname  (configuration-journal-path configuration)))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :tuning-experiment
             :id (tuning-experiment-identifier experiment)
             :lineage       *active-image-lineage-identifier*
             :mutation      (tuning-experiment-mutation-identifier experiment)
             :mutation-kind (tuning-experiment-mutation-kind experiment)
             :target        (tuning-experiment-mutation-target experiment)
             :hypothesis    (tuning-experiment-hypothesis experiment)
             :criterion     (tuning-experiment-criterion experiment)
             :observation   observation
             :result        verdict))
      (or (find (tuning-experiment-identifier experiment)
                (tuning-experiment-list configuration)
                :key  #'tuning-experiment-identifier
                :test #'string=)
          (error 'source-mutation-error
                 :message   "The tuning experiment settlement did not round-trip."
                 :tool-name "self.experiment-settle"
                 :pathname  (configuration-journal-path configuration))))))

(-> tuning-experiment-render-open (configuration) (option string))
(defun tuning-experiment-render-open (configuration)
  "Render CONFIGURATION's open experiment for self.status."
  (let ((experiment (tuning-experiment-open configuration)))
    (when experiment
      (format nil
              "tuning experiment~%  open       ~A~%  mutation   ~A (~A ~A)~%  verdict    ~A~%  hypothesis ~A~%  criterion  ~A~@[~%  observation ~A~]"
              (tuning-experiment-identifier experiment)
              (tuning-experiment-mutation-identifier experiment)
              (tuning-experiment-mutation-kind experiment)
              (tuning-experiment-mutation-target experiment)
              (tuning-experiment-verdict experiment)
              (tuning-experiment-hypothesis experiment)
              (tuning-experiment-criterion experiment)
              (tuning-experiment-observation experiment)))))

(-> tuning-experiment--settlement-guidance (tuning-experiment) string)
(defun tuning-experiment--settlement-guidance (experiment)
  "Return explicit next-step guidance for EXPERIMENT's latest verdict."
  (case (tuning-experiment-verdict experiment)
    (:better
     (format nil
             "The better verdict is terminal. Commit mutation ~A after its broader checks pass."
             (tuning-experiment-mutation-identifier experiment)))
    ((:worse :unchanged)
     (format nil
             "The ~A verdict is terminal. Discard mutation ~A and rethink the tuning change."
             (tuning-experiment-verdict experiment)
             (tuning-experiment-mutation-identifier experiment)))
    (:too-early
     "The too-early verdict keeps this experiment open. Gather more evidence before another exploratory mutation.")))

(defmethod tool-execute ((tool self-experiment-start-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Start one explicit tuning experiment for an effective pending mutation."
  (declare (ignore tool))
  (let ((experiment
          (tuning-experiment-start
           (tool-context-configuration context)
           (tool-argument arguments "mutation" :required t)
           :hypothesis (tool-argument arguments "hypothesis" :required t)
           :criterion  (tool-argument arguments "criterion" :required t))))
    (tool-success
     (format nil
             "Started tuning experiment ~A for mutation ~A.~%Hypothesis: ~A~%Criterion: ~A"
             (tuning-experiment-identifier experiment)
             (tuning-experiment-mutation-identifier experiment)
             (tuning-experiment-hypothesis experiment)
             (tuning-experiment-criterion experiment)))))

(defmethod tool-execute ((tool self-experiment-settle-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Settle the open tuning experiment without committing or discarding it."
  (declare (ignore tool))
  (let ((experiment
          (tuning-experiment-settle
           (tool-context-configuration context)
           (tool-argument arguments "experiment")
           :verdict-name (tool-argument arguments "verdict" :required t)
           :observation  (tool-argument arguments "observation" :required t))))
    (tool-success
     (format nil
             "Recorded ~A for tuning experiment ~A.~%Observation: ~A~2%~A"
             (tuning-experiment-verdict experiment)
             (tuning-experiment-identifier experiment)
             (tuning-experiment-observation experiment)
             (tuning-experiment--settlement-guidance experiment)))))
