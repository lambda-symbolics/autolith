;;;; -- Parent-owned Web Requests --

(in-package #:autolith)

(defmethod web-parent-output
    ((conversation agent-session-conversation) action arguments &key configuration)
  "Send web work to the parent. A refusal never selects the legacy transport."
  (let* ((turn-id   (agent-session-conversation-turn-id conversation))
         (fields    (make-hash-table :test #'equal))
         (tool-name (if (equal action "run") "web.run" "web_extra.gist")))
    (unless turn-id
      (error 'tool-error :tool-name tool-name :message "Web requests require an active parent turn."))
    (setf (gethash "action" fields) action)
    (cond
      ((equal action "run")
       (setf (gethash "commands" fields) (json-encode arguments)
             (gethash "history" fields)
             (json-encode (web--search-history conversation configuration))))
      ((equal action "gist")
       (setf (gethash "url" fields) (tool-argument arguments "url" :required t)))
      (t
       (error 'tool-error :tool-name tool-name :message "Unsupported parent web action.")))
    (let* ((delivery
             (agent-protocol-driver-request
              (agent-session-conversation-driver conversation)
              (lambda (client)
                (agent-protocol--request
                 client
                 (agent-wire:make-envelope
                  :parent-operation
                  (agent-wire:make-parent-operation
                   :operation-id (or *agent-application-scope-id* turn-id)
                   :kind ':web :turn-id turn-id
                   :arguments    (autolith-supervisor::protocol-data->map fields)))
                 (list 'agent-wire:parent-operation-result)))))
           (reply (agent-protocol-driver-next delivery)))
      (when (eq (agent-wire:envelope.body-case reply) 'agent-wire:failure)
        (error 'tool-error :tool-name tool-name :message "The parent refused or failed the web request."))
      (let* ((data   (autolith-supervisor::protocol-map->data
                      (agent-wire:parent-operation-result.fields
                       (agent-wire:envelope.parent-operation-result reply))))
             (output (gethash "output" data)))
        (unless (stringp output)
          (error 'tool-error :tool-name tool-name :message "Invalid parent web response."))
        (values output t)))))
