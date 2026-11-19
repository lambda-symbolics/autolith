(in-package #:autolith)

;;;; -- Operation Catalog and Invocation Extensions --

(-> application-client-catalog--operation-schema (application-operation) t)
(defun application-client-catalog--operation-schema (operation)
  "Return OPERATION's structured argument schema from its registered metadata."
  (if (typep operation 'application-tool-operation)
      (or (tool-parameters (application-operation-backend operation))
          (agentcomms:json-object "type" "object"))
      (agentcomms:json-object
       "type" "object"
       "properties"
       (agentcomms:json-object
        "input" (agentcomms:json-object "type" "string"
                                           "description" "Argument text for the selected operation.")
        "text" (agentcomms:json-object "type" "string"
                                         "description" "Alias for input."))
       "additionalProperties" (argo:json-false))))

(-> application-client-catalog--option-token ((or string keyword boolean)) string)
(defun application-client-catalog--option-token (option)
  "Spell OPTION for the structured client's text argument decoder."
  (let ((text (application-command--option-text option)))
    (if (find-if (lambda (character)
                   (or (char= character #\")
                       (find character *application-command-whitespace*)))
                 text)
        (format nil "~S" text)
        (copy-seq text))))

(-> application-client-catalog--operation-row (application-operation boolean boolean) hash-table)
(defun application-client-catalog--operation-row (operation idle-p own-turn-p)
  "Project OPERATION's registered help, choices, and headless admission."
  (let* ((command  (and (typep operation 'application-command-operation)
                        (application-operation-backend operation)))
         (active-p (and own-turn-p command
                        (member (application-command-busy-behavior command)
                                '(:inspect :execute :apply :steer)))))
    (agentcomms:json-object
     "name" (application-operation-name operation)
     "kind" (string-downcase (symbol-name (application-operation-kind operation)))
     "description" (application-operation-description operation)
     "argumentSchema" (application-client-value (application-client-catalog--operation-schema operation))
     "admitted" (if (and (or idle-p active-p) (application-client-operation-admitted-p operation))
                    t (argo:json-false))
     "terminalOwning" (if (application-client-operation-terminal-p operation) t (argo:json-false))
     "terminalWithoutArguments"
     (if (and command (eq (application-command-terminal-behavior command)
                          ':exclusive-without-arguments)) t (argo:json-false))
     "argumentHint" (if command (or (application-command-argument command) ':null) ':null)
     "help" (if command (application-command-tip command) ':null)
     "choices" (if command
                   (map 'vector #'application-client-catalog--option-token
                        (application-command-static-options command))
                   #())
     "aliases" (if command (coerce (application-command-aliases command) 'vector) #()))))

(-> application-client-catalog--command-invocation
    (application-command hash-table) application-command-invocation)
(defun application-client-catalog--command-invocation (command arguments)
  "Decode ARGUMENTS as text for the selected COMMAND."
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
      (application-client-command-invocation command input))))

(-> application-client-operations (application hash-table) hash-table)
(defun application-client-operations (application params)
  "Return the registered operation catalog as a JSON vector."
  (declare (ignore params))
  (let* ((controller (application-input-controller application))
         (busy-p     (and controller (application-input-controller-busy-p controller)))
         (rows       (mapcar (lambda (operation)
                               (application-client-catalog--operation-row operation (not busy-p) busy-p))
                             (application-operation-list application))))
    (agentcomms:json-object "items" (coerce rows 'vector))))

(-> application-client-invoke (application hash-table) hash-table)
(defun application-client-invoke (application params)
  "Decode client arguments and invoke registered command or tool admission."
  (let* ((name      (agentcomms:acp-field params "operation" :type ':string :required-p t))
         (arguments
          (or (agentcomms:acp-field params "arguments" :type ':object) (agentcomms:json-object)))
         (operation (application-client-operation-find application name))
         (decoded
          (if (typep operation 'application-command-operation)
              (application-client-catalog--command-invocation
               (application-operation-backend operation) arguments)
              arguments)))
    (multiple-value-bind (text delivery)
        (application-client-operation-invoke application operation decoded)
      (let ((result (agentcomms:json-object "text" text)))
        (when delivery (setf (gethash "delivery" result) delivery))
        result))))
