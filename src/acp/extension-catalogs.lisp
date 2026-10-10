(in-package #:autolith)

;;;; -- Operation Catalog and Invocation Extensions --

(-> acp-catalog--operation-terminal-p (application-operation) boolean)
(defun acp-catalog--operation-terminal-p (operation)
  "Return whether OPERATION always requires terminal ownership."
  (and (typep operation 'application-command-operation)
       (eq (application-command-terminal-behavior
            (application-operation-backend operation)) ':exclusive)))

(-> acp-catalog--operation-admitted-p
    (application-operation &optional hash-table) boolean)
(defun acp-catalog--operation-admitted-p (operation &optional (arguments nil supplied-p))
  "Return whether OPERATION supports headless use, checking supplied arguments."
  (or (typep operation 'application-tool-operation)
      (and (typep operation 'application-command-operation)
           (not (acp-catalog--operation-terminal-p operation))
           (or (not supplied-p)
               (not (application-command-terminal-owner-p
                     (application-operation-backend operation)
                     (application-operation--command-invocation
                      (application-operation-backend operation)
                      (acp-catalog--command-arguments
                       (application-operation-backend operation) arguments))))))))

(-> acp-catalog--operation-schema (application-operation) t)
(defun acp-catalog--operation-schema (operation)
  "Return OPERATION's structured argument schema from its registered metadata."
  (if (typep operation 'application-tool-operation)
      (or (tool-parameters (application-operation-backend operation))
          (agentcomms:json-object "type" "object"))
      (agentcomms:json-object
       "type" "object"
       "properties"
       (agentcomms:json-object
        "input" (agentcomms:json-object "type" "string"
                                          "description" "Native command arguments without the command name.")
        "text" (agentcomms:json-object "type" "string"
                                         "description" "Alias for input."))
       "additionalProperties" (argo:json-false))))

(-> acp-catalog--operation-row (application-operation boolean boolean) hash-table)
(defun acp-catalog--operation-row (operation idle-p own-turn-p)
  "Project OPERATION's headless support and native active-turn admission."
  (let* ((command (and (typep operation 'application-command-operation)
                       (application-operation-backend operation)))
         (active-p (and own-turn-p command
                        (member (application-command-busy-behavior command)
                                '(:inspect :execute :apply)))))
    (agentcomms:json-object
     "name" (application-operation-name operation)
     "kind" (string-downcase (symbol-name (application-operation-kind operation)))
     "description" (application-operation-description operation)
     "argumentSchema" (acp-extension-value (acp-catalog--operation-schema operation))
     "admitted" (if (and (or idle-p active-p) (acp-catalog--operation-admitted-p operation))
                    t (argo:json-false))
     "terminalOwning" (if (acp-catalog--operation-terminal-p operation) t (argo:json-false))
     "terminalWithoutArguments"
     (if (and command (eq (application-command-terminal-behavior command)
                          ':exclusive-without-arguments)) t (argo:json-false)))))

(-> acp-catalog--command-arguments (application-command hash-table) list)
(defun acp-catalog--command-arguments (command arguments)
  "Decode and validate native textual arguments for COMMAND."
  (declare (ignore command))
  (let ((keys (loop for key being the hash-keys of arguments collect key)))
    (unless (every (lambda (key) (member key '("input" "text") :test #'string=)) keys)
      (error 'configuration-error
             :message "Command arguments may contain only input or text."))
    (when (and (gethash "input" arguments)
               (gethash "text" arguments))
      (error 'configuration-error
             :message "Command arguments may specify input or text, not both."))
    (let ((input (or (agentcomms:acp-field arguments "input" :type ':string)
                     (agentcomms:acp-field arguments "text" :type ':string)
                     "")))
      (application-command--tokens input))))

(-> acp-extension-operations (acp-session hash-table) hash-table)
(defun acp-extension-operations (session params)
  "Return the registered operation catalog as a JSON vector."
  (declare (ignore params))
  (let* ((service (acp-session-service session))
         (owner (with-lock-held ((acp-service-lock service))
                  (acp-service-busy-owner service)))
         (rows (mapcar (lambda (operation)
                         (acp-catalog--operation-row operation (null owner) (eq owner session)))
                       (application-operation-list (acp-session-application session)))))
    (agentcomms:json-object "items" (coerce rows 'vector))))

(-> acp-catalog--invoke-presented
    (application-operation application t &key (:context (option tool-context))) t)
(defun acp-catalog--invoke-presented (operation application arguments &key context)
  "Return command presentation text or the native tool invocation result."
  (let* ((*application-command-presentation-capture-p* t)
         (*application-command-presentation-text* nil)
         (result (application-operation-invoke operation application arguments :context context)))
    (if (typep operation 'application-command-operation)
        (format nil "~{~A~^~%~}" (nreverse *application-command-presentation-text*))
        result)))

(-> acp-catalog--invoke-idle (acp-session application-operation t) hash-table)
(defun acp-catalog--invoke-idle (session operation arguments)
  "Invoke OPERATION while this request owns ACP primary admission."
  (let* ((application (acp-session-application session))
         (*acp-extension-invocation-p* t)
         (context (and (typep operation 'application-tool-operation)
                       (acp-extension-tool-context session)))
         (outcome ':condition)
         (claimed-p nil))
    (unwind-protect
         (handler-case
             (progn
               (with-lock-held ((acp-session-lock session))
                 (incf (acp-session-epoch session))
                 (setf (acp-session-cancelled-p session) nil
                       (acp-session-prompt-thread session) (current-thread)
                       (acp-session-prompt-interruptible-p session) t
                       (gethash (current-thread) (acp-session-tool-threads session))
                       (acp-session-epoch session)
                       claimed-p t))
               (acp-extension-turn-notify session "running" :source "invoke")
               (acp-session-check-cancelled session)
                (let ((text (acp-catalog--invoke-presented
                             operation application arguments :context context)))
                 (when context
                   (acp-session--call-with-finalization
                    session
                    (lambda ()
                      (acp-session-check-cancelled session)
                      (acp-observer-flush (tool-context-observer context)))))
                 (setf outcome ':end-turn)
                 (agentcomms:json-object "text" text)))
           (application-turn-cancelled (condition)
             (setf outcome ':cancelled)
             (error condition)))
      (when claimed-p
        (when context
          (acp-session--call-with-finalization
           session
           (lambda ()
             (unless (eq outcome ':end-turn)
               (handler-case (acp-observer-flush (tool-context-observer context))
                 (serious-condition () nil))))))
        (with-lock-held ((acp-session-lock session))
          (setf (acp-session-prompt-thread session) nil
                (acp-session-prompt-interruptible-p session) nil))
        (acp-extension-turn-notify session "idle" :source "invoke" :outcome outcome)
        (acp-extension-notify session "state")
        (acp-completion-wake session)))))

(-> acp-catalog--invoke-active (acp-session application-command-operation list) hash-table)
(defun acp-catalog--invoke-active (session operation arguments)
  "Honor a command's native active-turn policy without claiming another prompt."
  (let ((application (acp-session-application session)))
    (case (application-operation-active-turn-action operation arguments)
      (:execute
       (agentcomms:json-object
        "text" (acp-catalog--invoke-presented operation application arguments)))
      (:apply
       (let ((controller (application-input-controller application))
             (invocation (application-operation--command-invocation
                          (application-operation-backend operation) arguments))
             (*application-command-presentation-capture-p* t)
             (*application-command-presentation-text* nil))
         (unless controller (error 'acp-operation-busy))
         (application-input-controller--schedule-apply
          controller (application-command-invocation-input invocation))
         (acp-completion-wake session)
         (agentcomms:json-object
          "text" (format nil "~{~A~^~%~}" (nreverse *application-command-presentation-text*))
          "delivery" "scheduled")))
      (otherwise
       (error 'acp-operation-busy)))))

(-> acp-extension-invoke (acp-session hash-table) hash-table)
(defun acp-extension-invoke (session params)
  "Invoke an ordinary operation with native terminal and active-turn admission."
  (let* ((application (acp-session-application session))
         (name (agentcomms:acp-field params "operation" :type ':string :required-p t))
         (arguments (or (agentcomms:acp-field params "arguments" :type ':object)
                        (agentcomms:json-object)))
         (operation (application-operation-find application name))
         (service (acp-session-service session)))
    (unless (and operation (acp-catalog--operation-admitted-p operation arguments))
      (error 'acp-extension-unavailable
             :reason "This operation requires terminal ownership or has no headless adapter."))
    (let* ((command-p (typep operation 'application-command-operation))
           (decoded (if command-p
                        (acp-catalog--command-arguments
                         (application-operation-backend operation) arguments)
                        arguments))
           (owner (with-lock-held ((acp-service-lock service))
                    (acp-service-busy-owner service))))
      (if (and command-p (eq owner session))
          (acp-catalog--invoke-active session operation decoded)
          (acp-service--call-with-operation
           service session
           (lambda () (acp-catalog--invoke-idle session operation decoded)))))))
