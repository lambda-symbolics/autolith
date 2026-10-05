(in-package #:autolith)

;;;; -- Primary Schedule Operations --

(defclass mission-schedule-tool (tool) ()
  (:documentation "Explicit primary-owned mission scheduling operations."))

(-> mission-schedule--application (tool-context) application)
(defun mission-schedule--application (context)
  "Require the current primary application, excluding delegated agents."
  (let* ((mission (mission-context-find (tool-context-conversation context)))
         (application (and mission (mission-context-application mission))))
    (unless (and application
                 (eq (tool-context-agent context) (application-agent application))
                 (not (typep (tool-context-agent context) 'task-child-agent)))
      (mission--reject ':schedule-authority "Mission schedules are owned by the primary agent."))
    application))

(defmethod tool-execution-policy ((tool mission-schedule-tool))
  "Serialize schedule changes with other mutations in the provider batch."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-execute ((tool mission-schedule-tool) (context tool-context) arguments)
  "Apply a primary schedule operation through ordinary registry authorization."
  (let* ((application (mission-schedule--application context))
         (name (tool-name tool))
         (result
           (cond
             ((string= name "add")
              (task--validate-tool-arguments arguments '("id" "content" "at" "interval" "event" "missed-policy") "mission-schedule.add")
              (application-mission-schedule-add
               application :id (tool-argument arguments "id" :required t)
               :content (tool-argument arguments "content" :required t)
               :at (tool-argument arguments "at") :interval (tool-argument arguments "interval")
               :event (tool-argument arguments "event")
               :missed-policy (let ((policy (or (tool-argument arguments "missed-policy") "latest")))
                                (cond ((equal policy "latest") ':latest)
                                      ((equal policy "all") ':all)
                                      ((equal policy "skip") ':skip)
                                      (t (mission--reject ':schedule-policy "Expected latest, all or skip."))))))
             ((string= name "cancel")
              (task--validate-tool-arguments arguments '("id") "mission-schedule.cancel")
              (application-mission-schedule-cancel application (tool-argument arguments "id" :required t)))
             ((string= name "inspect")
              (task--validate-tool-arguments arguments nil "mission-schedule.inspect")
              (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler (application-mission-schedules application))))
             ((string= name "event")
              (task--validate-tool-arguments arguments '("event" "id") "mission-schedule.event")
              (application-mission-schedule-event application (tool-argument arguments "event" :required t)
                                                 (tool-argument arguments "id" :required t)))
             ((string= name "resolve")
              (task--validate-tool-arguments arguments '("occurrence" "token" "action") "mission-schedule.resolve")
              (application-mission-schedule-resolve
               application (mission--read-data (tool-argument arguments "occurrence" :required t))
               :token (tool-argument arguments "token" :required t)
               :action (let ((action (tool-argument arguments "action" :required t)))
                         (cond ((equal action "retry") ':retry)
                               ((equal action "completed") ':completed)
                               ((equal action "failed") ':failed)
                               (t (mission--reject ':schedule-resolution "Expected retry, completed or failed."))))))
             (t (mission--reject ':schedule-operation "Unknown schedule operation.")))))
    (task-tool-result (task--write-readable-sexp result :pretty-p t) result)))

(-> mission-schedule-register-tools (tool-registry) tool-registry)
(defun mission-schedule-register-tools (registry)
  "Register primary-owned durable schedule operations."
  (tool-registry-describe-namespace registry "mission-schedule" "Explicit durable mission wakeups and recovery decisions.")
  (flet ((register (name description properties required)
           (tool-registry-register registry
                                   (make-instance 'mission-schedule-tool :namespace "mission-schedule" :name name
                                                  :description description
                                                  :parameters (tool-object-schema properties required)))))
    (register "add" "Attach one time, interval or named external event wakeup to the current mission version."
              (json-object "id" (tool-string-property "Stable immutable schedule ID.")
                           "content" (tool-string-property "Mission continuation instructions, at most 8192 characters.")
                           "at" (tool-integer-property "Absolute universal-time deadline.")
                           "interval" (tool-integer-property "Positive interval in seconds.")
                           "event" (tool-string-property "Named external event, excludes time/interval.")
                           "missed-policy" (json-object "type" "string" "enum" #("latest" "skip" "all"))) '("id" "content"))
    (register "cancel" "Cancel future/pending work while retaining claim and duplicate history."
              (json-object "id" (tool-string-property "Schedule ID.")) '("id"))
    (register "inspect" "Read schedules, stable occurrences and uncertain outcomes." (json-object) nil)
    (register "event" "Admit one explicitly identified observable event, suppressing duplicates."
              (json-object "event" (tool-string-property "Event name.") "id" (tool-string-property "Stable external event ID.")) '("event" "id"))
    (register "resolve" "Explicitly settle an interrupted occurrence or authorize safe retry."
              (json-object "occurrence" (tool-string-property "Readable occurrence ID from inspect.")
                           "token" (tool-integer-property "Current claim token.")
                           "action" (json-object "type" "string" "enum" #("retry" "completed" "failed"))) '("occurrence" "token" "action")))
  registry)

(-> application-mission-schedules-host (application t) null)
(defun application-mission-schedules-host (application runtime)
  "Attach wakeups to the existing daemon listener's event-driven timer."
  (let ((service (application-mission-schedules application)))
    (setf (mission-schedule-notify service) (lambda () (image-daemon:daemon-runtime-notify runtime)))
    (image-daemon:daemon-runtime-set-timer
     runtime
     :next-deadline-function (lambda (host) (declare (ignore host))
                              (application-mission-schedules-next-time application))
     :wake-function (lambda (host) (declare (ignore host))
                      (application-mission-schedules-wake application))))
  nil)
