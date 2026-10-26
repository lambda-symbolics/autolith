;;;; -- Agent Context Projection Checks --

(in-package #:autolith)

(-> agent-context-test--rejects (function) null)
(defun agent-context-test--rejects (function)
  "Require an explicit failure rather than a partial logical projection."
  (assert (handler-case (progn (funcall function) nil)
            (error () t)))
  nil)

(-> agent-context-test--tools () vector)
(defun agent-context-test--tools ()
  "Use the same namespace and function schema shape as the local tool registry."
  (json-array
   (json-object
    "type" "namespace" "name" "resource" "description" "Workspace data."
    "tools" (json-array
             (json-object "name" "read" "description" "Read data."
                          "defer_loading" t "strict" ':json-false
                          "parameters" (json-object
                                        "type" "object"
                                        "properties" (json-object
                                                      "uri" (json-object "type" "string"))
                                        "required" (json-array "uri")
                                        "additionalProperties" ':json-false))))))

(-> agent-context-test--literal () null)
(defun agent-context-test--literal ()
  "Check ordered input, tools, raw malformed arguments, and JSON value semantics."
  (let* ((configuration (make-configuration))
         (items         (list
                         (json-object "role" "user" "content"
                                      (json-array
                                       (json-object "type" "input_text" "text" "Inspect this.")
                                       (json-object "type" "input_image"
                                                    "image_url" "data:image/png;base64,AAEC")))
                         (json-object "type" "function_call" "call_id" "call"
                                      "namespace" "resource" "name" "read" "arguments" "{malformed")
                         (json-object "type" "function_call_output" "call_id" "call"
                                      "output" (json-object "text" "ok" "false" ':json-false
                                                            "null" nil "true" t))))
         (conversation  (make-instance 'conversation :identifier "literal" :input-items items))
         (projection    (agent-context-project configuration conversation (agent-context-test--tools)
                                               :profile-id "profile" :turn-id "turn"
                                               :stable-prefix       '("Instructions.")
                                               :max-output-tokens 1024 :temperature 0.5d0
                                               :reasoning-effort    "low"
                                               :continuation-handle "continuation"
                                               :turn-state-handle   "turn-state"))
         (start         (agent-context-projection-start projection))
         (transcript    (agent-wire:model-start.items start))
         (message       (agent-wire:transcript-item.message (first transcript)))
         (tool          (first (agent-wire:model-start.tools start)))
         (result        (autolith-supervisor::protocol-value->data
                         (agent-wire:tool-result.content
                          (agent-wire:transcript-item.tool-result (third transcript))))))
    (assert (= 3 (length transcript)))
    (assert (null (agent-wire:model-start.messages start)))
    (assert (eq ':user (agent-wire:conversation-message.role message)))
    (assert (equal '("Instructions.") (agent-wire:model-start.stable-prefix start)))
    (assert (equalp #(0 1 2) (agent-wire:media.data
                              (agent-wire:content-block.media
                               (second (agent-wire:conversation-message.content message))))))
    (assert (string= "{malformed" (agent-wire:tool-call.arguments-json
                                   (agent-wire:transcript-item.tool-call (second transcript)))))
    (assert (eq ':protocol-null (gethash "null" result)))
    (assert (null (gethash "false" result)))
    (assert (eq t (gethash "true" result)))
    (assert (string= "resource" (agent-wire:tool-definition.namespace tool)))
    (assert (string= "Workspace data." (agent-wire:tool-definition.namespace-description tool)))
    (assert (agent-wire:tool-definition.deferred tool))
    (assert (agent-wire:tool-definition.has-strict tool))
    (assert (null (agent-wire:tool-definition.strict tool)))
    (assert (= 1024 (agent-wire:model-start.max-output-tokens start)))
    (assert (= 0.5d0 (agent-wire:model-start.temperature start)))
    (assert (string= "continuation" (agent-wire:model-start.continuation-handle start)))
    (assert (string= "turn-state" (agent-wire:model-start.turn-state-handle start)))
    (let* ((discovery (agent-context-item->transcript
                       (json-object "type" "tool_search_output" "call_id" "search"
                                    "tools" (agent-context-test--tools))))
           (tools     (agent-wire:tool-discovery-result.tools
                       (agent-wire:transcript-item.tool-discovery-result discovery))))
      (assert (string= "read" (agent-wire:tool-definition.name (first tools)))))
    (let* ((call      (agent-context-item->transcript
                       (json-object "type" "function_call" "call_id" "object-call" "name" "run"
                                    "arguments" (json-object "ok" t))))
           (arguments (agent-wire:tool-call.arguments (agent-wire:transcript-item.tool-call call))))
      (assert (gethash "ok" (autolith-supervisor::protocol-value->data
                             (cl-protobufs.autolith.common.v1:make-value :map-value arguments)))))
    (dolist (item (list
                   (json-object "role" "assistant" "id" "provider-item" "content" "text")
                   (json-object "type" "reasoning" "encrypted_content" "private")
                   (json-object "role" "user" "content"
                                (json-array (json-object "type" "input_image"
                                                         "image_url" "https://example.invalid/image.png")))
                   (json-object "role" "unknown" "content" "text")
                   (json-object "role" "user" "content"
                                (json-array (json-object "type" "unknown")))))
      (agent-context-test--rejects (lambda () (agent-context-item->transcript item)))))
  nil)

(-> agent-context-test--output () null)
(defun agent-context-test--output ()
  "Keep every public output view detached and replay only its handle after JSON storage."
  (let* ((projection (make-instance 'agent-context-projection :start nil :delivery nil))
         (call       (agent-wire:make-tool-call :call-id "call" :name "run" :arguments-json "{malformed"))
         (items      (list
                      (agent-wire:make-output-item
                       :message (agent-wire:make-conversation-message
                                 :role ':assistant :content (list (agent-wire:make-content-block :text "Answer."))))
                      (agent-wire:make-output-item :tool-call call)
                      (agent-wire:make-output-item :tool-discovery-call call)
                      (agent-wire:make-output-item :reasoning (agent-wire:make-reasoning-view :text ""))
                      (agent-wire:make-output-item
                       :hosted-tool (agent-wire:make-hosted-tool-view :capability-id "search" :status "done"))
                      (agent-wire:make-output-item :compaction (agent-wire:make-compaction-view))))
         (complete   (agent-wire:make-model-complete :reason ':tool-calls :output-items items
                                                     :continuation-handle "continuation"
                                                     :turn-state-handle   "state")))
    (loop for item in items for index from 0 do
          (setf (agent-wire:output-item.item-index item) index
                (agent-wire:output-item.family item) "responses"
                (agent-wire:output-item.replay-handle item) (format nil "handle-~d" index)))
    (let* ((public (agent-context-accept projection complete))
           (stored (json-decode (json-encode (coerce public 'vector)))))
      (assert (= 6 (length public)))
      (assert (string= "{malformed"
                       (gethash "arguments_json" (gethash "tool_call" (gethash "view" (second public))))))
      (loop for item across stored for index from 0 do
            (setf (gethash "view" item) (json-object "forged" "not replay data"))
            (let ((transcript (agent-context-item->transcript item)))
              (assert (eq 'agent-wire:replay-handle (agent-wire:transcript-item.item-case transcript)))
              (assert (string= (format nil "handle-~d" index)
                               (agent-wire:transcript-item.replay-handle transcript)))))
      (let* ((conversation (make-instance 'conversation :identifier  "reopened"
                                                        :input-items (coerce stored 'list)))
             (next         (agent-context-projection-start
                            (agent-context-project (make-configuration) conversation #()
                                                   :profile-id "profile" :turn-id "next-turn"
                                                   :stable-prefix '("Instructions.")))))
        (assert (equal '("handle-0" "handle-1" "handle-2" "handle-3" "handle-4" "handle-5")
                       (mapcar #'agent-wire:transcript-item.replay-handle
                               (agent-wire:model-start.items next)))))
      (setf (char (gethash "replay_handle" (first public)) 0) #\X)
      (assert (string= "handle-0" (agent-wire:output-item.replay-handle (first items))))
      (assert (string= "handle-0" (gethash "replay_handle" (first (agent-context-accept projection complete)))))
      (remhash "replay_handle" (aref stored 0))
      (agent-context-test--rejects (lambda () (agent-context-item->transcript (aref stored 0)))))
    (setf (agent-wire:model-complete.reason complete) ':stop)
    (agent-context-test--rejects (lambda () (agent-context-accept projection complete))))
  nil)

(-> agent-context-test--delivery () null)
(defun agent-context-test--delivery ()
  "Select actual ephemeral context and consume next-request receipts only on acceptance."
  (let* ((configuration                 (make-configuration))
         (durable                       (json-object "role" "user" "content" "Durable."))
         (ephemeral                     (json-object "role" "user" "content" "Ephemeral."))
         (conversation                  (make-instance 'conversation :identifier "context" :input-items (list durable)))
         (*context-contributors*        nil)
         (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
         (*context-last-deliveries*     (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil)
         (*context-request-contributions*
           (list (make-context-contribution :identifier "once" :instruction "Use this once."
                                            :lifetime ':next-request)
                 (change-class
                  (make-context-contribution :identifier "anchor" :instruction "Stable advice.")
                  'anchored-context-contribution))))
    (conversation--append-ephemeral-input-item conversation ephemeral)
    (flet ((project (&rest options)
             (apply #'agent-context-project configuration conversation (agent-context-test--tools)
                    :profile-id "profile" :turn-id "turn" :stable-prefix '("Instructions.") options)))
      (let* ((before     (copy-list (conversation-input-items conversation)))
             (projection (project :goal-context "Goal."))
             (start      (agent-context-projection-start projection))
             (summary    (project :summary-p t :goal-context "Not in summary.")))
        (assert (= 2 (length (agent-wire:model-start.items start))))
        (assert (= 1 (length (agent-wire:model-start.ephemeral-items start))))
        (assert (string= "Ephemeral."
                         (agent-wire:content-block.text
                          (first (agent-wire:conversation-message.content
                                  (agent-wire:transcript-item.message
                                   (first (agent-wire:model-start.ephemeral-items start))))))))
        (assert (search "Stable advice."
                        (agent-wire:content-block.text
                         (first (agent-wire:conversation-message.content
                                 (agent-wire:transcript-item.message
                                  (second (agent-wire:model-start.items start))))))))
        (assert (search "Use this once." (first (last (agent-wire:model-start.request-suffix start)))))
        (assert (equal before (conversation-input-items conversation)))
        (assert (null (agent-context-projection-delivery summary)))
        (assert (= 1 (length (agent-wire:model-start.items (agent-context-projection-start summary)))))
        (assert (null (agent-wire:model-start.tools (agent-context-projection-start summary))))
        (assert (null (agent-wire:model-start.ephemeral-items (agent-context-projection-start summary))))
        (assert (eq ':ordinary-summary (agent-wire:model-start.purpose (agent-context-projection-start summary))))
        (assert (equal (list *compaction-instructions*)
                       (agent-wire:model-start.request-suffix (agent-context-projection-start summary))))
        (assert (context-delivery-contributions (agent-context-projection-delivery (project))))
        (let ((accept (symbol-function 'agent-context-accept)))
          (agent-context-test--rejects
           (lambda () (funcall accept projection (agent-wire:make-failure :code ':unavailable)))))
        (agent-context-test--rejects
         (lambda () (agent-context-accept projection (agent-wire:make-model-complete))))
        (assert (search "Use this once."
                        (first (last (agent-wire:model-start.request-suffix
                                      (agent-context-projection-start (project)))))))
        (agent-context-accept projection (agent-wire:make-model-complete :reason ':stop))
        (let ((after (agent-context-projection-start (project))))
          (assert (notany (lambda (text) (search "Use this once." text))
                          (agent-wire:model-start.request-suffix after))))
        (agent-context-test--rejects (lambda () (project :kind ':native-compaction)))
        (agent-context-test--rejects (lambda () (project :summary-p t :reasoning-summary "auto"))))))
  nil)

(-> run-agent-protocol-context-tests () boolean)
(defun run-agent-protocol-context-tests ()
  "Run credential-free logical context and accepted-handle projection checks."
  (let ((*context-contributors*          nil)
        (*context-request-contributions* nil)
        (*context-resolver*              (cl-llm-provider-api:make-context-resolver))
        (*context-last-deliveries*       (make-hash-table :test #'equal))
        (*context-last-delivery-order*   nil))
    (agent-context-test--literal)
    (agent-context-test--output)
    (agent-context-test--delivery))
  t)
