(in-package #:autolith)

;;;; -- Mission Operations --

(defclass mission-tool (tool) ()
  (:documentation "A model operation over the current explicit session mission."))

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
                                    :parameters (tool-object-schema properties required)))))
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
      (let* ((mission (mission-context-find (tool-context-conversation context)))
             (application (and mission (mission-context-application mission))))
        (unless mission (mission--reject ':missing "No mission is set."))
        (unless (or (string= (tool-name tool) "status")
                    (eq (tool-context-conversation context) (application-conversation application)))
          (mission--reject ':authority "Only the primary agent may change mission state."))
        (cond
          ((string= (tool-name tool) "verify") (application-mission-verify application))
          ((string= (tool-name tool) "stop")
           (application-mission-stop
            application
            (cond ((equal (tool-argument arguments "status") "blocked") ':blocked)
                  ((equal (tool-argument arguments "status") "failed") ':failed)
                  (t (mission--reject ':invalid "Stop status must be blocked or failed.")))
            (tool-argument arguments "evidence" :required t)))
          ((string= (tool-name tool) "invalidate")
           (application-mission-invalidate application
                                          (tool-argument arguments "id" :required t)
                                          (tool-argument arguments "evidence" :required t))))
        (tool-success (format nil "~S" (mission-context-goal mission))))
    (mission-error (condition) (tool-failure (princ-to-string condition)))))

(-> mission--read-data (string) t)
(defun mission--read-data (text)
  "Read exactly one portable data form without read-time evaluation."
  (let ((*read-eval* nil) (*package* (find-package '#:autolith)))
    (multiple-value-bind (value position) (read-from-string text nil nil)
      (unless (and value
                   (every (lambda (character) (find character '(#\Space #\Tab #\Newline #\Return)))
                          (subseq text position)))
        (mission--reject ':invalid "Expected exactly one mission data form."))
      value)))

(-> application-mission-command (application string) null)
(defun application-mission-command (application remainder)
  "Apply /mission status, start, run, verify, resume, cancel, accept or invalidate."
  (handler-case
      (let* ((remainder (string-trim '(#\Space #\Tab) remainder))
             (space (position #\Space remainder))
             (operation (string-downcase (subseq remainder 0 space)))
             (argument (and space (string-trim '(#\Space #\Tab) (subseq remainder (1+ space))))))
        (cond
          ((member operation '("" "status") :test #'string=)
           (application-present application (format nil "~S" (application-goal application))))
          ((string= operation "start")
           (application-mission-start application (mission--read-data (or argument "")))
           (application-present application "Mission installed. Use /mission run to begin."))
          ((string= operation "run")
           (let ((goal (application-goal application)))
             (unless (and (mission-goal-p goal) (eq (getf goal :status) ':active))
               (mission--reject ':invalid "No active mission. Use /mission resume for paused or blocked missions."))
             (application--start-goal-work application)))
          ((string= operation "verify") (application-mission-verify application))
          ((string= operation "resume") (application-mission-resume application))
          ((string= operation "cancel")
           (application-mission-stop application ':cancelled (or argument "User cancelled the mission.")))
          ((member operation '("accept" "invalidate") :test #'string=)
           (let* ((parts (mission--read-data (or argument "")))
                  (id (first parts)) (evidence (second parts)))
             (unless (and (non-empty-string-p id) (non-empty-string-p evidence))
               (mission--reject ':invalid "Expected (\"ID\" \"EVIDENCE\")."))
             (if (string= operation "accept")
                 (application-mission-accept application id evidence)
                 (application-mission-invalidate application id evidence))))
          (t (mission--reject ':invalid "Usage: /mission [status|start PLIST|run|verify|resume|cancel REASON|accept (ID EVIDENCE)|invalidate (ID EVIDENCE)]"))))
    (error (condition) (application-present application (princ-to-string condition))))
  nil)

(define-application-command application--builtin-mission-command
    (:name "/mission"
     :argument "[OPERATION DATA]"
     :description "configure, run and verify an explicitly budgeted session mission"
     :tip "start with objective, criteria and turn/token/wall budgets; use status for durable gate evidence."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application &optional (remainder ""))
  (application-mission-command application remainder)
  ':continue)
