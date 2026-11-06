(in-package #:autolith)

;;;; -- Operation Catalog and Invocation Extensions --

(-> acp-catalog--operation-terminal-p (application-operation) boolean)
(defun acp-catalog--operation-terminal-p (operation)
  "Return whether OPERATION always requires terminal ownership."
  (and (typep operation 'application-command-operation)
       (eq (application-command-terminal-behavior
            (application-operation-backend operation)) ':exclusive)))

(-> acp-catalog--operation-admitted-p
    (application-operation &optional t) boolean)
(defun acp-catalog--operation-admitted-p (operation &optional (arguments nil supplied-p))
  "Return whether OPERATION supports headless use of decoded ARGUMENTS."
  (or (typep operation 'application-tool-operation)
      (and (typep operation 'application-command-operation)
           (not (acp-catalog--operation-terminal-p operation))
           (or (not supplied-p)
               (not (application-command-terminal-owner-p
                     (application-operation-backend operation) arguments))))))

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
                                           "description" "Slash command arguments without the command name.")
        "text" (agentcomms:json-object "type" "string"
                                         "description" "Alias for input."))
       "additionalProperties" (argo:json-false))))

(-> acp-catalog--operation-row (application-operation boolean boolean) hash-table)
(defun acp-catalog--operation-row (operation idle-p own-turn-p)
  "Project OPERATION's headless support and native active-turn admission."
  (let* ((command  (and (typep operation 'application-command-operation)
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

(-> acp-catalog--command-invocation
    (application-command hash-table) application-command-invocation)
(defun acp-catalog--command-invocation (command arguments)
  "Decode ACP text at COMMAND's slash boundary, retaining the parsed invocation."
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
      (let ((invocation
              (application-command-invocation-parse
               (if (zerop (length input))
                   (application-command-name command)
                   (format nil "~A ~A" (application-command-name command) input)))))
        (application-operation--command-invocation command invocation)))))

(-> acp-extension-operations (application hash-table) hash-table)
(defun acp-extension-operations (application params)
  "Return the registered operation catalog as a JSON vector."
  (declare (ignore params))
  (let* ((controller (application-input-controller application))
         (busy-p     (and controller (application-input-controller-busy-p controller)))
         (rows       (mapcar (lambda (operation)
                               (acp-catalog--operation-row operation (not busy-p) busy-p))
                             (application-operation-list application))))
    (agentcomms:json-object "items" (coerce rows 'vector))))

(-> acp-catalog--invoke-presented
    (application-operation application t &key (:context (option tool-context))) t)
(defun acp-catalog--invoke-presented (operation application arguments &key context)
  "Return command presentation text or the native tool invocation result."
  (let* ((*application-command-presentation-capture-p* t)
         (*application-command-presentation-text*      nil)
         (result                                       (application-operation-invoke operation application arguments :context context)))
    (if (typep operation 'application-command-operation)
        (format nil "~{~A~^~%~}" (nreverse *application-command-presentation-text*))
        result)))

(-> acp-catalog--invoke-idle (application application-operation t) hash-table)
(defun acp-catalog--invoke-idle (application operation arguments)
  "Invoke OPERATION as one of the channel's work items, so it never overlaps a turn."
  (application-clients-call-as-work
   application
   (lambda ()
     (agentcomms:json-object
      "text" (acp-catalog--invoke-presented
              operation application arguments
              :context (and (typep operation 'application-tool-operation)
                            (application-tool-context application)))))))

(-> acp-catalog--invoke-active
    (application application-command-operation application-command-invocation) hash-table)
(defun acp-catalog--invoke-active (application operation arguments)
  "Honor a command's native active-turn policy while the channel works."
  (case (application-operation-active-turn-action operation arguments)
    (:execute
     (agentcomms:json-object
      "text" (acp-catalog--invoke-presented operation application arguments)))
    (:apply
     (let ((controller                                   (application-input-controller application))
           (invocation                                   (application-operation--command-invocation
                                                          (application-operation-backend operation) arguments))
           (*application-command-presentation-capture-p* t)
           (*application-command-presentation-text*      nil))
       (unless controller (error 'acp-operation-busy))
       (application-input-controller--schedule-apply
        controller (application-command-invocation-input invocation))
       (agentcomms:json-object
        "text" (format nil "~{~A~^~%~}" (nreverse *application-command-presentation-text*))
        "delivery" "scheduled")))
    (otherwise
     (error 'acp-operation-busy))))

(-> acp-extension-invoke (application hash-table) hash-table)
(defun acp-extension-invoke (application params)
  "Invoke an ordinary operation with native terminal and active-turn admission."
  (let* ((name       (agentcomms:acp-field params "operation" :type ':string :required-p t))
         (arguments  (or (agentcomms:acp-field params "arguments" :type ':object)
                         (agentcomms:json-object)))
         (operation  (application-operation-find application name))
         (controller (application-input-controller application)))
    (unless (and operation (acp-catalog--operation-admitted-p operation))
      (error 'acp-extension-unavailable
             :reason "This operation requires terminal ownership or has no headless adapter."))
    (let* ((command-p (typep operation 'application-command-operation))
           (decoded   (if command-p
                          (acp-catalog--command-invocation
                           (application-operation-backend operation) arguments)
                          arguments)))
      (unless (acp-catalog--operation-admitted-p operation decoded)
        (error 'acp-extension-unavailable
               :reason "This operation requires terminal ownership or has no headless adapter."))
      (cond
        ((not (and controller (application-input-controller-busy-p controller)))
         (acp-catalog--invoke-idle application operation decoded))
        (command-p
         (acp-catalog--invoke-active application operation decoded))
        (t
         (error 'acp-operation-busy))))))
