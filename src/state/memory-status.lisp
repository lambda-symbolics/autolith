(in-package #:autolith)

;;;; -- Memory Status --

;;; A memory's status is derived on demand from the memory log and the files it
;;; cites, never stored, so a restart or rollback always reproduces it. A
;;; memory is current when every premise is current and unchanged and, if it
;;; cites files, at least one of them still supports it.

(defparameter *memory-premise-depth-limit* 16
  "The deepest premise chain followed when assessing one memory.")

(deftype memory-status ()
  "Whether a memory's evidence still supports it."
  '(member :current :stale :unverified))

(defclass memory-assessment ()
  ((memory
    :initarg :memory
    :reader memory-assessment-memory
    :type memory
    :documentation "The assessed memory.")
   (status
    :initarg :status
    :reader memory-assessment-status
    :type memory-status
    :documentation "Whether the memory's evidence still supports it.")
   (reasons
    :initarg :reasons
    :reader memory-assessment-reasons
    :type list
    :documentation "Reason plists explaining a stale or unverified status.")
   (source-outcomes
    :initarg :source-outcomes
    :reader memory-assessment-source-outcomes
    :type list
    :documentation "One (:source :outcome :reason) plist per cited source.")
   (premise-outcomes
    :initarg :premise-outcomes
    :reader memory-assessment-premise-outcomes
    :type list
    :documentation "One (:id :memory :outcome :assessment) plist per premise."))
  (:documentation "The derived evidence status of one memory and its explanation."))

(defclass memory-assessor ()
  ((memories
    :initarg :memories
    :reader memory-assessor-memories
    :type hash-table
    :documentation "Every active memory by identifier, for premise lookup.")
   (files
    :initform (make-hash-table :test #'equal)
    :reader memory-assessor-files
    :type hash-table
    :documentation "Each cited file's (TEXT . FAILURE), read at most once per pass.")
   (assessments
    :initform (make-hash-table :test #'equal)
    :reader memory-assessor-assessments
    :type hash-table
    :documentation "Completed assessments by memory identifier.")
   (pending
    :initform (make-hash-table :test #'equal)
    :reader memory-assessor-pending
    :type hash-table
    :documentation "Memories whose assessment is in progress, to stop cycles.")
   (view
    :initarg :view
    :reader memory-assessor-view
    :type knowledge-view
    :documentation "Structured knowledge evaluated at the same time, for claim premises."))
  (:documentation "One consistent pass assessing memories against the filesystem."))


;;;; -- Assessment --

(-> memory-assessor-create (configuration) memory-assessor)
(defun memory-assessor-create (configuration)
  "Return a fresh assessment pass over CONFIGURATION's active memories.

The pass evaluates CONFIGURATION's structured knowledge from its workspace at
the same time, so memories derived from claims follow those claims."
  (let* ((memories (make-hash-table :test #'equal))
         (all (memory-list configuration :visibility ':all))
         (view (knowledge-view-current configuration)))
    (dolist (memory all)
      (setf (gethash (memory-identifier memory) memories) memory))
    (make-instance 'memory-assessor :memories memories :view view)))

(-> memory-assessor-find (memory-assessor string) (option memory))
(defun memory-assessor-find (assessor identifier)
  "Return active memory IDENTIFIER known to ASSESSOR, or NIL."
  (values (gethash identifier (memory-assessor-memories assessor))))

(-> memory-assessor-dependents (memory-assessor string) list)
(defun memory-assessor-dependents (assessor identifier)
  "Return active memories derived directly from memory IDENTIFIER, newest first."
  (sort (loop for memory being the hash-values of (memory-assessor-memories assessor)
              when (find identifier (memory-premises memory)
                         :key (lambda (premise) (getf premise :id))
                         :test #'string=)
                collect memory)
        #'> :key #'memory-updated-at))

(-> memory-assessor--file (memory-assessor pathname) (values (option string) (option keyword)))
(defun memory-assessor--file (assessor pathname)
  "Return PATHNAME's text and read failure, reading it at most once per pass."
  (let* ((key (namestring pathname))
         (entry (or (gethash key (memory-assessor-files assessor))
                    (setf (gethash key (memory-assessor-files assessor))
                          (multiple-value-call #'cons
                            (memory-source-read-file pathname))))))
    (values (first entry) (rest entry))))

(-> memory-assessor--source-outcome (memory-assessor memory list) list)
(defun memory-assessor--source-outcome (assessor memory source)
  "Return the (:source :outcome :reason) plist for one SOURCE of MEMORY."
  (if (eq (first source) :file)
      (let ((path (memory-source-file-display source)))
        (multiple-value-bind (outcome why)
            (multiple-value-call #'memory-source-file-holds
              source
              (memory-assessor--file
               assessor
               (memory-source-file-pathname source (memory-workspace memory))))
          (list :source source
                :outcome outcome
                :reason (case why
                          ((nil)
                           nil)
                          (:changed
                           (list :kind ':file-changed :path path))
                          (:missing
                           (list :kind ':file-missing :path path))
                          (otherwise
                           (list :kind ':file-unchecked :path path :why why))))))
      ;; Statements and web pages are records of what was seen; they are
      ;; never refetched, so they continue to hold.
      (list :source source :outcome ':holds :reason nil)))

(-> memory-assessor--premise-outcome (memory-assessor list integer) list)
(defun memory-assessor--premise-outcome (assessor premise depth)
  "Return the (:id :memory :outcome :assessment :reason) plist for PREMISE."
  (let* ((identifier (getf premise :id))
         (memory (memory-assessor-find assessor identifier))
         (claim (and (eq (getf premise :kind) ':claim)
                     (knowledge-view-node (memory-assessor-view assessor) identifier))))
    (flet ((outcome (outcome reason &optional assessment)
             (list :id identifier
                   :memory memory
                   :claim claim
                   :outcome outcome
                   :assessment assessment
                   :reason reason)))
      (cond
        ((eq (getf premise :kind) ':claim)
         (cond
           ((null claim)
            (outcome ':failed (list :kind ':premise-claim :id identifier
                                    :title (or (getf premise :text) identifier)
                                    :applicability ':unknown)))
           ((knowledge-node-supported-p claim)
            (outcome ':holds nil))
           (t
            (let ((applicability (knowledge-node-applicability claim)))
              (outcome (if (eq applicability ':unverified) ':unverified ':failed)
                       (list :kind ':premise-claim :id identifier
                             :title (knowledge-node-text claim)
                             :applicability applicability))))))
        ((null memory)
         (outcome ':failed (list :kind ':premise-forgotten :id identifier)))
        ((gethash identifier (memory-assessor-pending assessor))
         (outcome ':unverified (list :kind ':premise-cycle :id identifier
                                     :title (memory-title memory))))
        ((>= depth *memory-premise-depth-limit*)
         (outcome ':unverified (list :kind ':premise-too-deep :id identifier
                                     :title (memory-title memory))))
        ((string/= (getf premise :digest)
                   (memory-content-digest (memory-title memory)
                                          (memory-content memory)))
         (outcome ':failed (list :kind ':premise-changed :id identifier
                                 :title (memory-title memory))
                  (memory-assessor--assess assessor memory (1+ depth))))
        (t
         (let ((assessment (memory-assessor--assess assessor memory (1+ depth))))
           (ecase (memory-assessment-status assessment)
             (:current
              (outcome ':holds nil assessment))
             (:stale
              (outcome ':failed (list :kind ':premise-stale :id identifier
                                      :title (memory-title memory))
                       assessment))
             (:unverified
              (outcome ':unverified (list :kind ':premise-unverified :id identifier
                                          :title (memory-title memory))
                       assessment)))))))))

(-> memory-assessor--assess (memory-assessor memory integer) memory-assessment)
(defun memory-assessor--assess (assessor memory depth)
  "Assess MEMORY at premise DEPTH, memoizing the result for this pass."
  (let ((identifier (memory-identifier memory))
        (assessments (memory-assessor-assessments assessor)))
    (or (gethash identifier assessments)
        (progn
          (setf (gethash identifier (memory-assessor-pending assessor)) t)
          (unwind-protect
               (let* ((sources
                        (mapcar (lambda (source)
                                  (memory-assessor--source-outcome assessor memory source))
                                (memory-sources memory)))
                      (premises
                        (mapcar (lambda (premise)
                                  (memory-assessor--premise-outcome assessor premise depth))
                                (memory-premises memory)))
                      ;; Only files can be checked again. Statements and web
                      ;; pages record provenance, so they decide nothing
                      ;; while checkable evidence exists.
                      (checked
                        (remove-if-not (lambda (outcome)
                                         (eq (first (getf outcome :source)) :file))
                                       sources))
                      (source-status
                        (cond
                          ((null checked)
                           ':current)
                          ((find ':holds checked :key (lambda (outcome)
                                                        (getf outcome :outcome)))
                           ':current)
                          ;; An unreadable alternative might still hold.
                          ((find ':unverified checked :key (lambda (outcome)
                                                             (getf outcome :outcome)))
                           ':unverified)
                          (t
                           ':stale)))
                      (failed-premises
                        (remove-if-not (lambda (outcome)
                                         (eq (getf outcome :outcome) ':failed))
                                       premises))
                      (unverified-premises
                        (remove-if-not (lambda (outcome)
                                         (eq (getf outcome :outcome) ':unverified))
                                       premises))
                      (status
                        (cond
                          ((or failed-premises (eq source-status ':stale))
                           ':stale)
                          ((or unverified-premises (eq source-status ':unverified))
                           ':unverified)
                          (t
                           ':current)))
                      (reasons
                        (unless (eq status ':current)
                          (remove nil
                                  (append
                                   (unless (eq source-status ':current)
                                     (mapcar (lambda (outcome)
                                               (getf outcome :reason))
                                             checked))
                                   (mapcar (lambda (outcome)
                                             (getf outcome :reason))
                                           (append failed-premises
                                                   unverified-premises)))))))
                 (setf (gethash identifier assessments)
                       (make-instance 'memory-assessment
                                      :memory memory
                                      :status status
                                      :reasons reasons
                                      :source-outcomes sources
                                      :premise-outcomes premises)))
            (remhash identifier (memory-assessor-pending assessor)))))))

(-> memory-assess (memory-assessor memory) memory-assessment)
(defun memory-assess (assessor memory)
  "Return MEMORY's status and explanation within ASSESSOR's pass."
  (memory-assessor--assess assessor memory 0))


;;;; -- Presentation --

(-> memory-status-reason-text (list) string)
(defun memory-status-reason-text (reason)
  "Return one short sentence fragment explaining REASON."
  (let ((path (getf reason :path))
        (title (getf reason :title))
        (identifier (getf reason :id)))
    (ecase (getf reason :kind)
      (:file-changed
       (format nil "~A changed" path))
      (:file-missing
       (format nil "~A is missing" path))
      (:file-unchecked
       (format nil "~A could not be checked (~A)"
               path
               (case (getf reason :why)
                 (:too-large "too large")
                 (:not-text "not text")
                 (otherwise "unreadable"))))
      (:premise-changed
       (format nil "\"~A\" changed" title))
      (:premise-forgotten
       (format nil "a premise (~A) was forgotten"
               (subseq identifier 0 (min *memory-short-identifier-length*
                                         (length identifier)))))
      (:premise-stale
       (format nil "\"~A\" is stale" title))
      (:premise-unverified
       (format nil "\"~A\" is unverified" title))
      (:premise-cycle
       (format nil "\"~A\" relies on this memory" title))
      (:premise-too-deep
       (format nil "\"~A\" is too deep to check" title))
      (:premise-claim
       (if (eq (getf reason :applicability) ':unknown)
           (format nil "\"~A\" no longer holds" title)
           (format nil "\"~A\" is ~(~A~)" title (getf reason :applicability)))))))

(-> memory-assessment-summary (memory-assessment) (option string))
(defun memory-assessment-summary (assessment)
  "Return a short explanation of a stale or unverified ASSESSMENT, or NIL."
  (let ((reasons (memory-assessment-reasons assessment)))
    (when reasons
      (format nil "~{~A~^; ~}" (mapcar #'memory-status-reason-text reasons)))))
