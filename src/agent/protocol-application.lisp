;;;; -- Parent-backed Child Application --

(in-package #:autolith)

(defparameter *agent-application-scope-id* nil
  "The parent-created scope currently executing in this child thread.")

(defclass agent-protocol-application (application)
  ((session :initarg :session :reader agent-protocol-application-session
            :documentation "The isolated session that owns all local execution."))
  (:documentation "An explicit application projection with no private settings or file reads."))

(defmethod application-render-records
    ((application agent-protocol-application) &key streamed-assistant-text streamed-reasoning-text)
  "Leave durable transcript presentation to the parent that owns its records."
  (declare (ignore application streamed-assistant-text streamed-reasoning-text))
  nil)

(-> agent-protocol-application-create (agent-protocol-session) agent-protocol-application)
(defun agent-protocol-application-create (session)
  "Build an application directly from the parent-approved in-memory agent projection."
  (let* ((agent (agent-protocol-session-agent session))
         (application
           (make-instance 'agent-protocol-application
                          :session session :configuration (agent-configuration agent)
                          :conversation (agent-conversation agent) :provider (agent-provider agent)
                          :tool-registry (agent-tool-registry agent) :worker (agent-worker agent)
                          :agent agent :ui (agent-protocol-terminal-ui-create session))))
    (setf (agent-protocol-session-observer session) (application-agent-observer application))
    (application-input-controller-create application :load-pending-p nil :start-reader-p nil)
    (task-completion-connect
     agent (lambda () (agent-protocol-application-notify session ':ui-completion-ready)))
    application))

(-> agent-application--parent-call (agent-protocol-application keyword hash-table) hash-table)
(defun agent-application--parent-call (application kind arguments)
  "Call one fixed parent operation in the current durable execution scope."
  (let* ((session (agent-protocol-application-session application))
         (driver  (agent-protocol-session-driver session)))
    (agent-session--check-running session)
    (let* ((delivery
             (agent-protocol-driver-request
              driver
              (lambda (client)
                (agent-protocol--request
                 client
                 (agent-wire:make-envelope
                  :parent-operation
                  (agent-wire:make-parent-operation
                   :operation-id *agent-application-scope-id*
                    :kind        kind
                   :turn-id      (agent-session-conversation-turn-id
                                  (agent-protocol-session-conversation session))
                   :arguments    (autolith-supervisor::protocol-data->map arguments)))
                 (list 'agent-wire:parent-operation-result)))))
           (reply (agent-protocol-driver-next delivery)))
      (when (eq (agent-wire:envelope.body-case reply) 'agent-wire:failure)
        (error 'agent-protocol-error :reason ':parent-operation-failed))
      (agent-session--check-running session)
      (autolith-supervisor::protocol-map->data
       (agent-wire:parent-operation-result.fields
        (agent-wire:envelope.parent-operation-result reply))))))

(-> agent-application--parent-command-kind (application-command) (option keyword))
(defun agent-application--parent-command-kind (command)
  "Classify private-state commands after resolving the live registry alias."
  (let ((name (application-command-name command)))
    (cond
      ((member name '("/auth" "/providers" "/accounts") :test #'string=)
       ':auth)
      ((member name '("/model" "/models" "/effort" "/summary" "/reasoning" "/reasoning-summary") :test #'string=)
       ':model)
      ((string= name "/permissions")
       ':permissions)
      ((member name '("/settings" "/preferences" "/config" "/fast" "/trace" "/timestamps"
                      "/ste" "/titles" "/cache-misses" "/hurry-up") :test #'string=)
       ':preferences)
      ((member name '("/rollback" "/generations") :test #'string=)
       ':restart)
      ((member name '("/new" "/resume" "/replay" "/fork" "/conversations") :test #'string=)
       ':session)
      (t
       nil))))

(-> agent-application--argument-data (t) t)
(defun agent-application--argument-data (argument)
  "Encode evaluated command arguments without transmitting readable executable forms."
  (typecase argument
    (null
     nil)
    (keyword
     (let ((fields (make-hash-table :test #'equal)))
       (setf (gethash "keyword" fields) (string-downcase argument))
       fields))
    ((or string integer float)
     argument)
    (cons
     (map 'simple-vector #'agent-application--argument-data argument))
    (t
     (error 'agent-protocol-error :reason ':nonportable-command-argument))))

(defmethod application-command-execute :around
    ((command application-command) (application agent-protocol-application)
     (invocation application-command-invocation))
  "Route private-state commands to fixed parent operations; keep custom handlers isolated."
  (let ((kind (agent-application--parent-command-kind command)))
    (if kind
        (let ((arguments (make-hash-table :test #'equal)))
          (setf (gethash "command" arguments) (application-command-name command)
                (gethash "remainder" arguments) (application-command-invocation-remainder invocation)
                (gethash "arguments" arguments)
                (map 'simple-vector #'agent-application--argument-data
                     (application-command-invocation-arguments invocation)))
          (let ((result (agent-application--parent-call application kind arguments)))
            (when (hash-table-p (gethash "settings" result))
              (agent-protocol-application-settings-apply
                (application-configuration application) (gethash "settings" result)))
            (when (gethash "output" result)
              (application-present application (gethash "output" result)))
            (if (gethash "quit" result) ':quit ':continue)))
        (call-next-method))))

(-> agent-application--command-metadata (application-command) agent-wire:application-command)
(defun agent-application--command-metadata (command)
  "Project live command metadata to bounded inert protocol fields."
  (agent-wire:make-application-command
   :name              (application-command-name command)
   :aliases           (application-command-aliases command)
   :argument          (or (application-command-argument command) "")
   :description       (application-command-description command)
   :tip               (application-command-tip command)
   :busy-behavior     (application-command-busy-behavior command)
   :terminal-behavior (application-command-terminal-behavior command)
   :options           (mapcar (lambda (option)
                                (typecase option
                                  (string
                                   option)
                                  (symbol
                                   (string-downcase option))
                                  (t
                                   (princ-to-string option))))
                              (application-command-static-options command))
   :callable          (application-command-callable-p command)))

(-> agent-application--restart-selector (agent-protocol-application condition list) (values t t))
(defun agent-application--restart-selector (application condition restarts)
  "Keep restart extent and source evaluation in the child; ask the parent only for a choice."
  (let ((arguments (make-hash-table :test #'equal)))
    (setf (gethash "condition" arguments) (princ-to-string condition)
          (gethash "restarts" arguments)
          (map 'simple-vector
               (lambda (restart)
                 (let ((fields (make-hash-table :test #'equal)))
                   (setf (gethash "name" fields) (string (restart-name restart))
                         (gethash "report" fields) (application-lisp--restart-report restart))
                   fields))
               restarts))
    (let* ((result (agent-application--parent-call application ':restart arguments))
           (index  (gethash "index" result))
           (source (gethash "source" result)))
      (if (and (integerp index) (<= 0 index) (< index (length restarts))
               (or (null source) (stringp source)))
          (values (nth index restarts) source)
          (values nil nil)))))

(-> agent-application--evaluate
    (agent-protocol-application string) application-lisp-evaluation)
(defun agent-application--evaluate (application source)
  "Evaluate child Lisp with cancellation and a remote inert restart menu."
  (let* ((session (agent-protocol-application-session application))
         (thread  (bordeaux-threads:current-thread)))
    (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
      (push thread (agent-protocol-session-application-evaluators session)))
    (unwind-protect
         (progn
           (agent-session--check-running session)
           (application-lisp-evaluate source :application application
             :restart-selector                            (lambda (condition restarts)
                                                           (agent-application--restart-selector application condition restarts))))
      (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
        (setf (agent-protocol-session-application-evaluators session)
          (remove thread (agent-protocol-session-application-evaluators session)))))))

(-> agent-protocol-application-run
    (agent-protocol-session agent-wire:application-action) agent-wire:application-result)
(defun agent-protocol-application-run (session action)
  "Execute one parent-issued action in the isolated child, never in provider history."
  (let* ((application                                  (or (agent-protocol-session-application session)
                                                           (setf (agent-protocol-session-application session)
                                                                 (agent-protocol-application-create session))))
         (turn-id                                      (agent-session-conversation-turn-id
                                                        (agent-protocol-session-conversation session)))
         (source                                       (agent-wire:application-action.source action))
         (*application-command-presentation-capture-p* t)
         (*application-command-presentation-text*      nil))
    (autolith-supervisor::protocol-application-action-validate action)
    (agent-session--check-running session)
    (case (agent-wire:application-action.kind action)
      (:catalog
       (agent-wire:make-application-result
        :turn-id turn-id :status ':ok
        :commands (mapcar #'agent-application--command-metadata (application-command-list))))
      (:completion
       (agent-wire:make-application-result
        :turn-id turn-id :status ':ok
        :commands    (mapcar #'agent-application--command-metadata (application-command-list))
        :completion-entries
        (mapcar (lambda (entry)
                  (agent-wire:make-application-completion
                    :name (getf entry :name) :argument (or (getf entry :argument) "")
                    :description (or (getf entry :description) "") :primary (or (getf entry :primary) "")))
                (application-operation-completion-entries application))
        :completions (application-path-search-files application source :limit 100)))
      ((:lisp :async-lisp)
       (let ((result (agent-application--evaluate application source)))
         (application-present application (application-lisp--result-entry result))
         (agent-wire:make-application-result
          :turn-id turn-id :status (application-lisp-evaluation-status result)
          :output    (application-lisp-evaluation-output result)
          :values    (application-lisp-evaluation-values result)
          :condition (or (application-lisp-evaluation-condition result) "")
          :quit      (not (null (application-lisp-evaluation-loop-action result))))))
      (:configure
       (agent-application--configure application source)
       (agent-wire:make-application-result :turn-id turn-id :status ':ok))
      (:pump
       (agent-application--pump application)
       (agent-wire:make-application-result :turn-id turn-id :status ':ok))
      (:message
       (let* ((agent    (agent-protocol-session-agent session))
              (message  (user-message-input-create :text source
                          :image-pathnames               (mapcar #'pathname (agent-wire:application-action.image-paths action))))
              (observer (make-instance 'agent-session-observer :session session
                           :delegate                                    (agent-protocol-session-observer session))))
         (agent-run-user-turn agent message :observer observer)
         (agent-wire:make-application-result :turn-id turn-id :status ':ok)))
      (:run-job
       (agent-wire:make-application-result
        :turn-id turn-id :status ':ok
        :job (autolith-supervisor::protocol-data->map
              (agent-protocol-application-run-job application
                (autolith-supervisor::protocol-map->data (agent-wire:application-action.job action))))))
      ((:acp-prompt :acp-extension)
       (agent-wire:make-application-result :turn-id turn-id :status ':ok
        :fields (autolith-supervisor::protocol-data->map
                 (agent-protocol-application-acp application action))))
      (:command
       (let* ((invocation (application-command-invocation-parse source))
              (command    (application-command-invocation-command invocation)))
         (unless command
           (error 'agent-protocol-error :reason ':unknown-application-command))
         (let ((action (application-command-execute command application invocation)))
           (agent-wire:make-application-result
            :turn-id turn-id :status ':ok :quit (eq action ':quit)
             :output (format nil "~{~A~}" (nreverse *application-command-presentation-text*))))))
      (otherwise
       (error 'agent-protocol-error :reason ':invalid-application-action)))))


;;;; -- Durable Application Scope --

(-> agent-session--application-turn
    (agent-protocol-session agent-wire:turn-start) null)
(defun agent-session--application-turn (session start)
  "Execute one root application action and await the parent's durable result acknowledgement."
  (let* ((*agent-application-scope-id* (agent-wire:turn-start.turn-id start))
         (result
           (handler-case
               (agent-protocol-application-run session (agent-wire:turn-start.application start))
             (application-turn-cancelled (condition)
               (error condition))
             (error (condition)
               (agent-wire:make-application-result
                :turn-id *agent-application-scope-id* :status ':error :condition (princ-to-string condition))))))
    (agent-application-result-publish session result))
  nil)

(-> agent-application-result-publish (agent-protocol-session agent-wire:application-result) null)
(defun agent-application-result-publish (session result)
  "Wait for parent storage to commit the exact typed result before its scope can finish."
  (autolith-supervisor::protocol-application-result-validate result)
  (let* ((event-id (format nil "application/~A/result" (agent-wire:application-result.turn-id result)))
         (delivery
           (agent-protocol-driver-request
            (agent-protocol-session-driver session)
            (lambda (client)
              (agent-protocol--request
               client (agent-wire:make-envelope :application-result result)
               (list 'agent-wire:session-ack event-id)))))
         (reply    (agent-protocol-driver-next delivery)))
    (unless (eq (agent-wire:envelope.body-case reply) 'agent-wire:session-ack)
      (error 'agent-protocol-error :reason ':application-result-not-durable)))
  nil)

(-> agent-protocol-application-event (agent-protocol-session keyword hash-table) null)
(defun agent-protocol-application-event (session kind fields)
  "Send bounded inert presentation data."
  (agent-protocol--send (agent-protocol-session-client session)
    (lambda ()
      (agent-wire:make-envelope :application-event
        (agent-wire:make-application-event :kind kind
          :fields                                (autolith-supervisor::protocol-data->map fields)))))
  nil)

(-> agent-protocol-application-notify (agent-protocol-session keyword &optional string) null)
(defun agent-protocol-application-notify (session kind &optional (identifier ""))
  "Ask the parent to schedule a known pump in a new durable scope."
  (unless (and (member kind '(:acp-completion-ready :ui-completion-ready)) (<= (length identifier) 256))
    (error 'agent-protocol-error :reason ':invalid-application-notification))
  (agent-protocol--send (agent-protocol-session-client session)
    (lambda ()
      (agent-wire:make-envelope :application-notification
        (agent-wire:make-application-notification :kind kind :identifier identifier))))
  nil)

(-> agent-application--pump (agent-protocol-application) null)
(defun agent-application--pump (application)
  "Run one admitted completion batch inside the parent's current durable scope."
  (let ((controller (application-input-controller application))
        (session    (agent-protocol-application-session application)))
    (loop
      (agent-session--check-running session)
      (multiple-value-bind (work deadline)
          (with-lock-held ((application-input-controller-lock controller))
            (values (or (application-job-completions--take-work controller)
                        (application-job-completions--take-maintenance controller))
                    (application-job-completions--next-time controller)))
        (cond
          (work
           (ecase (first work)
             (:job-completion
              (application-job-completions-run controller (second work)))
             (:job-completion-maintenance
              (application-job-completions-maintain controller (second work))))
           (return))
          ((null deadline)
           (return))
          (t
           (sleep (min 0.05 (max 0.001 (- deadline (get-universal-time))))))))))
  nil)

(-> agent-application--configure (agent-protocol-application string) null)
(defun agent-application--configure (application source)
  "Load only parent-selected projected init and replay files in the child image."
  (let ((fields (agentcomms:json-decode source)))
    (unless (and (hash-table-p fields)
                 (loop for key being the hash-keys of fields
                       always (member key '("init_files" "replay_files") :test #'equal)))
      (error 'agent-protocol-error :reason ':invalid-application-configuration))
    (dolist (key '("replay_files" "init_files"))
      (let ((paths (gethash key fields)))
        (unless (and (typep paths 'sequence) (not (stringp paths)) (<= (length paths) 64)
                     (every (lambda (path)
                              (and (stringp path) (<= 1 (length path) 8192)
                                   (not (find #\Null path)) (uiop:absolute-pathname-p path))) paths))
          (error 'agent-protocol-error :reason ':invalid-application-configuration))
        (map nil (lambda (path)
                   (agent-session--check-running (agent-protocol-application-session application))
                   (let ((*package* (find-package '#:autolith)))
                     (load path :verbose nil :print nil))) paths))))
  nil)

(-> agent-protocol-application-settings-apply (configuration hash-table) null)
(defun agent-protocol-application-settings-apply (configuration settings)
  "Apply shared settings, excluding parent-only image selection state."
  (maphash
   (lambda (name value)
     (let ((key (find name autolith-supervisor::*protocol-application-setting-keys*
                      :test #'string-equal :key #'symbol-name)))
       (unless (and key (typep value '(or null string real (eql t))))
         (error 'agent-protocol-error :reason ':invalid-public-setting))
       (if (eq key ':pristine-p)
           (unless (typep value 'boolean)
             (error 'agent-protocol-error :reason ':invalid-public-setting))
           (setf (config key configuration) value)))) settings)
  nil)
