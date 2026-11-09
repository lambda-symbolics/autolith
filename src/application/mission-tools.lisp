(in-package #:autolith)

;;;; -- Mission Operations --

(defclass mission-tool (tool) ()
  (:documentation "A model operation over the current explicit session mission."))

(defclass mission-tool-result (tool-result)
  ((details :initarg :details :reader tool-result-details
            :documentation "Portable mission snapshot retained for transcript replay."))
  (:documentation "A mission result with its native durable presentation data."))

(defmethod tool-execution-policy ((tool mission-tool))
  "Serialize mission transitions with other tool mutations in one provider batch."
  (declare (ignore tool))
  ':exclusive)

(-> mission-register-tools (tool-registry) tool-registry)
(defun mission-register-tools (registry)
  "Register mission status, verification, blocker and invalidation operations."
  (flet ((register (name description properties required)
           (tool-registry-register
            registry (make-instance 'mission-tool :namespace "mission" :name name
                                    :description description
                                    :parameters  (tool-object-schema properties required)))))
    (register "status" "Read the durable mission state, budgets, criteria and gate evidence."
              (json-object) nil)
    (register "verify" "Request harness verification of the current mission. Model completion is not verification."
              (json-object) nil)
    (register "stop" "Persist a mission blocker or failure with evidence."
              (json-object "status" (cl-llm-provider-api:provider-enum-schema
                                     '("blocked" "failed") "Explicit stop state.")
                           "evidence" (tool-string-property "Concrete blocker or failure evidence."))
              '("status" "evidence"))
    (register "invalidate" "Invalidate a gate after relevant external state changes; does not reset its attempt budget."
              (json-object "id" (tool-string-property "Gate ID.")
                           "evidence" (tool-string-property "Reason the gate should run again."))
              '("id" "evidence")))
  registry)

(defmethod tool-execute ((tool mission-tool) (context tool-context) (arguments hash-table))
  "Apply one mission operation through its owning application's durable policy."
  (handler-case
      (let* ((mission     (mission-context-find (tool-context-conversation context)))
             (application (and mission (mission-context-application mission))))
        (unless mission (mission--reject ':missing "No mission is set."))
        (unless (or (string= (tool-name tool) "status")
                    (eq (tool-context-conversation context) (application-conversation application)))
          (mission--reject ':authority "Only the primary agent may change mission state."))
        (cond
          ((string= (tool-name tool) "verify")
           (application-mission-verify application))
          ((string= (tool-name tool) "stop")
           (application-mission-stop
            application
            (cond
              ((equal (tool-argument arguments "status") "blocked")
               ':blocked)
              ((equal (tool-argument arguments "status") "failed")
               ':failed)
              (t
               (mission--reject ':invalid "Stop status must be blocked or failed.")))
            (tool-argument arguments "evidence" :required t)))
          ((string= (tool-name tool) "invalidate")
           (application-mission-invalidate application
                                          (tool-argument arguments "id" :required t)
                                          (tool-argument arguments "evidence" :required t))))
        (let ((goal (mission-context-goal mission)))
          (make-instance 'mission-tool-result
                         :content (tool-result-content (tool-success (format nil "~S" goal)))
                         :success-p t :category ':success
                         :details (list :mission :goal (copy-tree goal)))))
    (mission-error (condition)
      (tool-failure (princ-to-string condition) :code (tool-failure-code condition)))))

(-> mission--read-data (string) t)
(defun mission--read-data (text)
  "Read exactly one data form with read-time evaluation disabled."
  (handler-case
      (let ((*read-eval* nil) (*package* (find-package '#:autolith))
            (end (gensym "END")))
        (with-input-from-string (stream text)
          (let ((value (read stream nil end)))
            (unless (and (not (eq value end)) (eq (read stream nil end) end))
              (mission--reject ':invalid "Expected exactly one mission data form."))
            value)))
    (mission-error (condition) (error condition))
    (error () (mission--reject ':invalid "Invalid mission data form."))))

(-> application-mission-slash-arguments (string) list)
(defun application-mission-slash-arguments (remainder)
  "Parse legacy /mission command text into action arguments."
  (let* ((text     (string-trim '(#\Space #\Tab #\Newline #\Return) remainder))
         (space    (position-if (lambda (character) (find character '(#\Space #\Tab #\Newline #\Return))) text))
         (name     (subseq text 0 space))
         (argument (and space (string-left-trim '(#\Space #\Tab) (subseq text (1+ space))))))
    (cond
      ((member name '("" "status" "run" "verify" "resume") :test #'string-equal)
       (when (and argument (plusp (length argument)))
         (mission--reject ':invalid "This mission action takes no arguments."))
       (list (if (equal name "") ':status
                 (find name '(:status :run :verify :resume) :test #'string-equal :key #'symbol-name))))
      ((string-equal name "start")
       (list ':start (mission--read-data (or argument ""))))
      ((string-equal name "cancel")
       (if argument (list ':cancel argument) (list ':cancel)))
      ((member name '("accept" "invalidate") :test #'string-equal)
       (let ((fields (mission--read-data (or argument ""))))
         (unless (and (proper-list-p fields) (= (length fields) 2))
           (mission--reject ':invalid "Expected (\"ID\" \"EVIDENCE\")."))
         (cons (if (string-equal name "accept") ':accept ':invalidate) fields)))
      (t
       (mission--reject ':invalid "Unknown mission action.")))))

(-> mission--command-specification-p (t) boolean)
(defun mission--command-specification-p (specification)
  "Check for proper lists and valid property lists before policy validation."
  (labels ((plist-p (value)
             (and (proper-list-p value) (evenp (length value))
                  (let ((keys (plist-keys value)))
                    (and (every #'keywordp keys)
                         (= (length keys) (length (remove-duplicates keys))))))))
    (and (plist-p specification)
         (proper-list-p (getf specification :criteria))
         (every (lambda (criterion)
                  (and (plist-p criterion) (proper-list-p (getf criterion :gates))))
                (getf specification :criteria))
         (proper-list-p (getf specification :gates))
         (every (lambda (gate)
                  (and (plist-p gate) (proper-list-p (getf gate :inputs))))
                (getf specification :gates))
         t)))

(-> application-mission-command (application &optional t &rest t) null)
(defun application-mission-command (application &optional (action ':status) &rest arguments)
  "Apply a native mission action, preserving specifications and literal evidence."
  (unless (and (member action '(:status :start :run :verify :resume :cancel :accept :invalidate))
               (case action
                 ((:status :run :verify :resume)
                  (null arguments))
                 (:start
                  (and (= (length arguments) 1)
                       (mission--command-specification-p (first arguments))))
                 (:cancel
                  (or (null arguments)
                      (and (= (length arguments) 1) (non-empty-string-p (first arguments)))))
                 ((:accept :invalidate)
                  (and (= (length arguments) 2) (every #'non-empty-string-p arguments)))))
    (mission--reject ':invalid "Invalid mission action or arguments."))
  (case action
    (:status
     (application-present application (format nil "~S" (application-goal application))))
    (:start
     (application-mission-start application (first arguments))
     (application-present application "Mission installed. Use (mission :run) to begin."))
    (:run
     (let ((goal (application-goal application)))
       (unless (and (mission-goal-p goal) (eq (getf goal :status) ':active))
         (mission--reject ':invalid "No active mission. Use (mission :resume) for paused or blocked missions."))
       (application--start-goal-work application)))
    (:verify
     (application-mission-verify application))
    (:resume
     (application-mission-resume application))
    (:cancel
     (application-mission-stop application ':cancelled
                               (or (first arguments) "User cancelled the mission.")))
    (:accept
     (application-mission-accept application (first arguments) (second arguments)))
    (:invalidate
     (application-mission-invalidate application (first arguments) (second arguments))))
  nil)

(define-application-command application--builtin-mission-command
    (:name              "/mission"
     :argument          "[OPERATION DATA]"
     :description       "configure, run and verify an explicitly budgeted session mission"
     :tip               "start with an objective, criteria and budgets for turns, tokens and wall-clock time; use status to see saved gate evidence."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :slash-parser      application-mission-slash-arguments
     :callable          t)
    (application &optional (action ':status) &rest arguments)
  (apply #'application-mission-command application action arguments)
  ':continue)
