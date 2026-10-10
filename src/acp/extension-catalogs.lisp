(in-package #:autolith)

;;;; -- Operation Catalog and Invocation Extensions --

(-> acp-catalog--operation-terminal-p (application-operation) boolean)
(defun acp-catalog--operation-terminal-p (operation)
  "Return whether OPERATION requires terminal ownership for headless invocation."
  (and (typep operation 'application-command-operation)
       (not (eq (application-command-terminal-behavior
                 (application-operation-backend operation))
                ':shared))))

(-> acp-catalog--operation-schema (application-operation) t)
(defun acp-catalog--operation-schema (operation)
  "Return OPERATION's structured argument schema from its registered metadata."
  (if (typep operation 'application-tool-operation)
      (or (tool-parameters (application-operation-backend operation))
          (agentcomms:json-object "type" "object"))
      (agentcomms:json-object "type" "object")))

(-> acp-catalog--operation-row (application-operation boolean) hash-table)
(defun acp-catalog--operation-row (operation idle-p)
  "Project OPERATION's headless availability and current primary admission."
  (agentcomms:json-object
   "name" (application-operation-name operation)
   "kind" (string-downcase (symbol-name (application-operation-kind operation)))
   "description" (application-operation-description operation)
   "argumentSchema" (acp-extension-value (acp-catalog--operation-schema operation))
   "admitted" (if (and idle-p (typep operation 'application-tool-operation)) t (argo:json-false))
   "terminalOwning" (if (acp-catalog--operation-terminal-p operation) t (argo:json-false))))

(-> acp-extension-operations (acp-session hash-table) hash-table)
(defun acp-extension-operations (session params)
  "Return the registered operation catalog as a JSON vector."
  (declare (ignore params))
  (let* ((service (acp-session-service session))
         (idle-p (with-lock-held ((acp-service-lock service))
                   (null (acp-service-busy-owner service))))
         (rows (mapcar (lambda (operation) (acp-catalog--operation-row operation idle-p))
                       (application-operation-list (acp-session-application session)))))
    (agentcomms:json-object "items" (coerce rows 'vector))))

(-> acp-extension-invoke (acp-session hash-table) hash-table)
(defun acp-extension-invoke (session params)
  "Invoke one registered tool under primary admission and ACP permission authority."
  (let* ((application (acp-session-application session))
         (name (agentcomms:acp-field params "operation" :type ':string :required-p t))
         (arguments (or (agentcomms:acp-field params "arguments" :type ':object)
                        (agentcomms:json-object)))
         (operation (application-operation-find application name)))
    (unless (typep operation 'application-tool-operation)
      (error 'acp-extension-unavailable :reason "This operation has no headless tool adapter."))
    (let ((*acp-extension-invocation-p* t)
          (context (acp-extension-tool-context session))
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
                 (let ((text (application-operation-invoke operation application arguments :context context)))
                   (acp-session--call-with-finalization
                    session
                    (lambda ()
                      (acp-session-check-cancelled session)
                      (acp-observer-flush (tool-context-observer context))))
                   (setf outcome ':end-turn)
                   (agentcomms:json-object "text" text)))
             (application-turn-cancelled (condition)
               (setf outcome ':cancelled)
               (error condition)))
        (when claimed-p
          (acp-session--call-with-finalization
           session
           (lambda ()
             (unless (eq outcome ':end-turn)
               (handler-case (acp-observer-flush (tool-context-observer context))
                 (serious-condition () nil)))))
          (with-lock-held ((acp-session-lock session))
            (setf (acp-session-prompt-thread session) nil))
          (acp-extension-turn-notify session "idle" :source "invoke" :outcome outcome)
          (acp-completion-wake session))))))
