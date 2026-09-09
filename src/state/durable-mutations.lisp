(in-package #:autolith)

;;;; -- Durable Mutation Checks --

(defclass mutation-checker ()
  ()
  (:documentation "A strategy for checking live and source mutation states."))

(defclass standard-mutation-checker (mutation-checker)
  ()
  (:documentation
   "The production checker validating the rendered pending definitions.

Behavioral validation of a commit lives in the clean-process replay probe
that IMAGE-COMMIT-PUBLISH always runs, plus any self.exercise records; the
in-image gate only has to prove the rendered source is structurally sound."))

(defclass full-suite-mutation-checker (mutation-checker)
  ()
  (:documentation
   "A checker running the complete in-image Autolith test suite.

This is minutes of silent work, needs the test system loadable beside the
active image, and inherits whatever dynamic state the live session holds,
so it is opt-in rather than the default commit gate."))

(defclass callback-mutation-checker (mutation-checker)
  ((active-callback
    :initarg :active-callback
   :reader callback-mutation-checker-active-callback
   :type function
    :documentation "The injected active-image check callback."))
  (:documentation "A mutation checker backed by explicit boundary callbacks."))

(-> mutation-checker-check-active
    (mutation-checker configuration string)
    string)
(defgeneric mutation-checker-check-active (checker configuration definition-source)
  (:documentation
   "Check installed DEFINITION-SOURCE in the active image and return captured output."))

(defmethod mutation-checker-check-active
    ((checker full-suite-mutation-checker)
     (configuration configuration)
     (definition-source string))
  "Run Autolith's ASDF tests against the installed active-image definition."
  (declare (ignore checker configuration definition-source))
  (with-output-to-string (stream)
    (let ((*standard-output* stream)
          (*error-output* stream)
          (*trace-output* stream)
           (*skill-logical-turn-state* nil))
      (asdf:test-system :autolith))))

(defmethod mutation-checker-check-active
    ((checker callback-mutation-checker)
     (configuration configuration)
     (definition-source string))
  "Invoke CHECKER's injected active-image callback."
  (or (funcall (callback-mutation-checker-active-callback checker)
               configuration
               definition-source)
      ""))

(-> tool-context-effective-mutation-checker (tool-context) mutation-checker)
(defun tool-context-effective-mutation-checker (context)
  "Return CONTEXT's checker or a production checker when none was injected."
  (or (tool-context-mutation-checker context)
      (make-instance 'standard-mutation-checker)))


;;;; -- Durable Mutation State --

(defclass durable-mutation ()
  ((identifier
    :initarg :identifier
    :reader durable-mutation-identifier
    :type non-empty-string
    :documentation "The stable identifier joining this mutation's journal records.")
   (target
    :initarg :target
    :reader durable-mutation-target
    :type non-empty-string
    :documentation "The semantic definition signature being changed.")
   (pathname
    :initarg :pathname
    :reader durable-mutation-pathname
    :type non-empty-string
    :documentation "The private reconstruction artifact receiving the change.")
   (previous-source
    :initarg :previous-source
    :reader durable-mutation-previous-source
    :type string
    :documentation "The complete source definition preceding this mutation.")
   (proposed-source
    :initarg :proposed-source
    :reader durable-mutation-proposed-source
    :type string
    :documentation "The complete proposed source definition.")
   (base-commit
    :initarg :base-commit
    :initform nil
    :reader durable-mutation-base-commit
    :type (option string)
    :documentation "The tracked base revision preceding the mutation, when known.")
   (phase
    :initarg :phase
    :accessor durable-mutation-phase
    :type keyword
    :documentation "The latest journaled transaction phase.")
   (git-commit
    :initform nil
    :accessor durable-mutation-git-commit
    :type (option string)
    :documentation "A legacy Git commit recorded by an older journal format."))
  (:documentation "One checked live-to-private-reconstruction transaction."))

(defvar *durable-mutations* (make-hash-table :test #'equal)
  "Durable mutation transactions retained by the active Lisp image.")

(-> durable-mutation-journal
    (configuration durable-mutation &key (:detail t))
    list)
(defun durable-mutation-journal (configuration mutation &key detail)
  "Append MUTATION's current phase and optional DETAIL to its journal."
  (mutation-journal-append
   configuration
   (append
    (list :mutation
          :kind :durable-definition
          :id (durable-mutation-identifier mutation)
          :target (durable-mutation-target mutation)
          :pathname (durable-mutation-pathname mutation)
          :previous (durable-mutation-previous-source mutation)
          :proposed (durable-mutation-proposed-source mutation)
          :base-commit (durable-mutation-base-commit mutation)
          :result (durable-mutation-phase mutation))
    (when (durable-mutation-git-commit mutation)
      (list :git-commit (durable-mutation-git-commit mutation)))
    (when detail
      (list :detail (bounded-string detail :limit 2000))))))

(-> durable-mutation-transition-allowed-p (keyword keyword) boolean)
(defun durable-mutation-transition-allowed-p (from-phase to-phase)
  "Return true when FROM-PHASE may legally advance to TO-PHASE."
  (and (member
        to-phase
        (case from-phase
          (:pending '(:installed :failed :superseded))
          (:installed '(:checked :failed :superseded))
          (:checked '(:source-written :failed :superseded))
          (:source-written '(:durable :failed :superseded))
          (otherwise nil))
        :test #'eq)
       t))

(-> durable-mutation-transition
    (configuration durable-mutation keyword &key (:detail t) (:git-commit (option string)))
    durable-mutation)
(defun durable-mutation-transition
    (configuration mutation phase &key detail git-commit)
  "Validate, apply, and journal MUTATION's transition to PHASE."
  (unless (durable-mutation-transition-allowed-p
           (durable-mutation-phase mutation)
           phase)
      (error 'source-mutation-error
             :message (format nil "Invalid durable mutation transition from ~S to ~S."
                              (durable-mutation-phase mutation)
                              phase)
             :tool-name "self.persist-definition"
             :pathname (durable-mutation-pathname mutation)))
  (setf (durable-mutation-phase mutation) phase)
  (when git-commit
    (setf (durable-mutation-git-commit mutation) git-commit))
  (durable-mutation-journal configuration mutation :detail detail)
  mutation)

(-> durable-mutation-create
    (configuration list
     &key
     (:relative-pathname string)
     (:previous-source string)
     (:proposed-source string))
    durable-mutation)
(defun durable-mutation-create
    (configuration definition
     &key relative-pathname previous-source proposed-source)
  "Create and journal a pending durable transaction for DEFINITION."
  (let* ((target (definition-key definition))
         (mutation
           (make-instance 'durable-mutation
                          :identifier (make-identifier)
                          :target target
                          :pathname relative-pathname
                          :previous-source previous-source
                          :proposed-source proposed-source
                          :base-commit
                          (string-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           (self-git-command configuration '("rev-parse" "HEAD")))
                          :phase :pending)))
    (maphash
     (lambda (identifier existing)
       (declare (ignore identifier))
       (when (and (string= target (durable-mutation-target existing))
                  (member (durable-mutation-phase existing)
                          '(:pending :installed :checked :source-written)
                          :test #'eq))
         (durable-mutation-transition
          configuration
          existing
          :superseded
          :detail (format nil "Superseded by mutation ~A."
                          (durable-mutation-identifier mutation)))))
     *durable-mutations*)
    (setf (gethash (durable-mutation-identifier mutation)
                   *durable-mutations*)
          mutation)
    (durable-mutation-journal configuration mutation)
    mutation))

(-> durable-mutations-reconcile (configuration) list)
(defun durable-mutations-reconcile (configuration)
  "Finish source-written mutations already present in a private image commit."
  (let ((reconciled nil))
    (maphash
     (lambda (identifier mutation)
       (when (member (durable-mutation-phase mutation)
                     '(:checked :source-written)
                     :test #'eq)
         (when (image-commit-contains-mutation-p configuration identifier)
           (when (eq (durable-mutation-phase mutation) :checked)
             (durable-mutation-transition
              configuration
              mutation
              :source-written
              :detail
              "Reconciled published private image-commit source."))
           (durable-mutation-transition
            configuration
            mutation
            :durable
            :detail
            "Reconciled after private image-commit publication.")
           (push mutation reconciled))))
     *durable-mutations*)
    (nreverse reconciled)))

(-> mutation-journal-read-records (configuration) list)
(defun mutation-journal-read-records (configuration)
  "Read complete portable records from CONFIGURATION's mutation journal."
  (let ((pathname (configuration-journal-path configuration)))
    (if (probe-file pathname)
        (handler-case
            (multiple-value-bind (records incomplete-final-form-p)
                (log-read pathname)
              (declare (ignore incomplete-final-form-p))
              records)
          (store-error (condition)
            (error 'source-mutation-error
                   :message (format nil "Malformed mutation journal: ~A" condition)
                   :tool-name "self.status"
                   :pathname pathname)))
        nil)))

(-> durable-mutation-journal-record-p (t) boolean)
(defun durable-mutation-journal-record-p (record)
  "Return true when RECORD claims to be a durable-definition journal state."
  (and (listp record)
       (eq (first record) :mutation)
       (eq (getf (rest record) :kind) :durable-definition)
       t))

(-> durable-mutation-record-p (configuration t) boolean)
(defun durable-mutation-record-p (configuration record)
  "Return true when RECORD is a valid durable-definition journal state.

Historical journals may name tracked src/ files or retired overlay paths."
  (and (durable-mutation-journal-record-p record)
       (non-empty-string-p (getf (rest record) :id))
       (non-empty-string-p (getf (rest record) :target))
       (non-empty-string-p (getf (rest record) :pathname))
       (let ((pathname (merge-pathnames
                        (getf (rest record) :pathname)
                        (configuration-source-root configuration))))
         (or (uiop:subpathp pathname
                            (merge-pathnames
                             "src/"
                             (configuration-source-root configuration)))
             (uiop:subpathp pathname
                            (configuration-image-commit-root configuration))
             ;; Journals may be replayed under a different data root, so a
             ;; foreign overlays directory is still a recognizable location.
             (not (null
                   (find "overlays"
                         (pathname-directory pathname)
                         :test #'equal)))))
       (stringp (getf (rest record) :previous))
       (stringp (getf (rest record) :proposed))
       (or (null (getf (rest record) :base-commit))
           (non-empty-string-p (getf (rest record) :base-commit)))
       (member (getf (rest record) :result)
               '(:pending :installed :checked :source-written
                 :durable :failed :superseded)
               :test #'eq)
       t))

(-> durable-mutation-record-matches-p (durable-mutation list) boolean)
(defun durable-mutation-record-matches-p (mutation properties)
  "Return true when PROPERTIES preserve MUTATION's immutable identity."
  (and (string= (durable-mutation-target mutation)
                (getf properties :target))
       (string= (durable-mutation-pathname mutation)
                (getf properties :pathname))
       (string= (durable-mutation-previous-source mutation)
                (getf properties :previous))
       (string= (durable-mutation-proposed-source mutation)
                (getf properties :proposed))
       (equal (durable-mutation-base-commit mutation)
              (getf properties :base-commit))
       t))

(-> durable-mutations-load (configuration) hash-table)
(defun durable-mutations-load (configuration)
  "Reconstruct durable mutation state from CONFIGURATION's append-only journal."
  (clrhash *durable-mutations*)
  (dolist (record (mutation-journal-read-records configuration))
    (when (durable-mutation-journal-record-p record)
      (unless (durable-mutation-record-p configuration record)
        (error 'source-mutation-error
               :message "A durable mutation journal record is invalid."
               :tool-name "self.status"
               :pathname (configuration-journal-path configuration)))
      (let* ((properties (rest record))
             (identifier (getf properties :id))
             (phase (getf properties :result))
             (existing (gethash identifier *durable-mutations*)))
        (if existing
            (progn
              (unless (and (durable-mutation-record-matches-p existing properties)
                           (durable-mutation-transition-allowed-p
                            (durable-mutation-phase existing)
                            phase))
                (error 'source-mutation-error
                       :message "A durable mutation journal transition is invalid."
                       :tool-name "self.status"
                       :pathname (configuration-journal-path configuration)))
              (setf (durable-mutation-phase existing) phase
                    (durable-mutation-git-commit existing)
                    (getf properties :git-commit)))
            (progn
              (unless (eq phase :pending)
                (error 'source-mutation-error
                       :message "A durable mutation journal begins after its pending state."
                       :tool-name "self.status"
                       :pathname (configuration-journal-path configuration)))
              (let ((mutation
                      (make-instance 'durable-mutation
                                     :identifier identifier
                                     :target (getf properties :target)
                                     :pathname (getf properties :pathname)
                                     :previous-source (getf properties :previous)
                                     :proposed-source (getf properties :proposed)
                                     :base-commit (getf properties :base-commit)
                                     :phase phase)))
                (setf (gethash identifier *durable-mutations*) mutation)))))))
  (durable-mutations-reconcile configuration)
  *durable-mutations*)


(-> durable-mutation--fallback-source (configuration list) (option string))
(defun durable-mutation--fallback-source (configuration definition)
  "Return DEFINITION's tracked source when private history has no prior form."
  (handler-case
      (loop for tracked in (self-tracked-definitions configuration
                                                     (second definition))
            for form = (source-form-form
                        (tracked-definition-source-form tracked))
            when (eq (first form) (first definition))
              return (tracked-definition-source tracked))
    (error ()
      nil)))

(defmethod tool-execute ((tool self-persist-definition-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install, check, and persist one definition in a private image commit."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((definition-source
             (tool-argument arguments "definition" :required t))
           (configuration (tool-context-configuration context))
           (definition (self-read-form definition-source :read-eval nil)))
      (tuning-experiment-assert-settled
       configuration "self.persist-definition")
      (unless (definition-form-p definition)
        (error 'source-mutation-error
               :message "The durable source is not a supported complete definition."
               :tool-name "self.persist-definition"
               :pathname (configuration-image-commit-root configuration)))
      (let* ((target (definition-key definition))
             (home-package (definition-home-package-name definition))
             (commit-identifier (make-identifier))
             (commit-directory
               (image-commit--directory configuration commit-identifier))
             (commit-script (merge-pathnames "reconstruct.lisp"
                                             commit-directory))
             (previous-source (or (image-commit-definition-source
                                   configuration target)
                                  (durable-mutation--fallback-source
                                   configuration
                                   definition)
                                  ""))
             (undo-action
               (self--definition-state-undo-action
                definition
                (and (non-empty-string-p previous-source) previous-source)
                (find-package '#:autolith)))
             (mutation
               (durable-mutation-create configuration
                                        definition
                                        :relative-pathname
                                        (namestring commit-script)
                                        :previous-source previous-source
                                        :proposed-source definition-source))
             (checker (tool-context-effective-mutation-checker context))
             (published-p nil))
        (handler-case
            (progn
              (self-call-with-restarts
               (lambda ()
                 (self--install-definition definition definition-source))
               :restart-name (tool-argument arguments "restart")
               :restart-value-source (tool-argument arguments "restart-value"))
              (durable-mutation-transition configuration mutation :installed)
              (mutation-checker-check-active checker
                                             configuration
                                             definition-source)
              (durable-mutation-transition configuration mutation :checked)
              (let ((commit
                      (image-commit-publish
                       configuration
                       :title (format nil "Persist definition ~A" target)
                       :mutation-records nil
                       :additional-entries
                       (list (list :kind ':definition
                                   :id (durable-mutation-identifier mutation)
                                   :target target
                                   :package "AUTOLITH"
                                   :home-package home-package
                                   :source definition-source))
                       :identifier commit-identifier)))
                (setf published-p t)
                (durable-mutation-transition configuration mutation
                                             :source-written)
                (durable-mutation-transition configuration mutation :durable)
                (tool-success
                 (format nil
                         "Mutation ~A installed, checked, and persisted as ~
                          private image commit ~A.~%Private Git commit: ~A~%
                          Replay script: ~A"
                         (durable-mutation-identifier mutation)
                         commit-identifier
                         (image-commit-history-commit commit)
                         (namestring commit-script)))))
          (error (condition)
            (unless published-p
              (handler-case
                  (self--undo-failed-definition-installation
                   undo-action
                   condition)
                (active-image-corruption (corruption)
                  (durable-mutation-transition
                   configuration
                   mutation
                   :failed
                   :detail
                   (format nil "Mutation failed: ~A~%Restoration failed: ~A"
                           condition
                           (active-image-corruption-restoration-condition
                            corruption)))
                  (error corruption)))
              (unless (member (durable-mutation-phase mutation)
                              '(:failed :superseded)
                              :test #'eq)
                (durable-mutation-transition configuration
                                             mutation
                                             :failed
                                             :detail condition)))
            (error condition)))))))


;;;; -- Source Revision Boundary --

(-> self-git-command (configuration list &key (:ignore-error-status boolean)) string)
(defun self-git-command (configuration arguments &key ignore-error-status)
  "Run Git ARGUMENTS in CONFIGURATION's source root and return combined output."
  (uiop:run-program
   (append (list "git" "-C"
                 (namestring (configuration-source-root configuration)))
           arguments)
   :output ':string
   :error-output ':output
   :ignore-error-status ignore-error-status))

(-> self-validate-commit-title (string) string)
(defun self-validate-commit-title (title)
  "Return valid commit TITLE or signal a tool error."
  (unless (and (non-empty-string-p title)
               (< (length title) 72)
               (null (find #\Newline title))
               (null (find #\Return title)))
    (error 'tool-error
           :message "A commit title must be one non-empty line under 72 characters."
           :tool-name "self.commit"))
  title)
