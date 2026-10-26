;;;; -- Agent Context Projection --

(in-package #:autolith)

(defclass agent-context-projection ()
  ((start :initarg :start :reader agent-context-projection-start
          :documentation "The credential-free typed request assembled for the parent.")
   (delivery :initarg :delivery :reader agent-context-projection-delivery
             :documentation "The existing context selection whose receipts await acceptance.")
   (accepted :initform nil :accessor agent-context-projection-accepted
             :documentation "Detached accepted completion bytes, or NIL before acceptance."))
  (:documentation "One explicit request projection. No provider or transport is installed."))

(-> agent-context--fail (keyword) nil)
(defun agent-context--fail (reason)
  "Reject an unsupported logical projection without printing its contents."
  (error 'agent-protocol-error :reason reason))

(-> agent-context--data (t) t)
(defun agent-context--data (value)
  "Convert agent JSON null and booleans to bounded protocol portable data."
  (let ((entries 0))
    (labels ((walk (item depth)
               (when (or (> depth 32) (> (incf entries) 65536))
                 (agent-context--fail ':context-value-limit))
               (cond
                 ((null item)
                  ':protocol-null)
                 ((member item (list ':json-false yason:false))
                  nil)
                 ((eq item yason:true)
                  t)
                 ((hash-table-p item)
                  (let ((result (make-hash-table :test #'equal)))
                    (maphash (lambda (key child)
                               (setf (gethash key result) (walk child (1+ depth))))
                             item)
                    result))
                 ((and (vectorp item) (not (stringp item)))
                  (map 'vector (lambda (child) (walk child (1+ depth))) item))
                 (t
                  item))))
      (walk value 1))))

(-> agent-context--json (t) t)
(defun agent-context--json (value)
  "Convert bounded public protocol data to the agent JSON representation."
  (cond
    ((eq value ':protocol-null)
     nil)
    ((null value)
     ':json-false)
    ((hash-table-p value)
     (let ((result (make-hash-table :test #'equal)))
       (maphash (lambda (key child)
                  (setf (gethash key result) (agent-context--json child)))
                value)
       result))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'agent-context--json value))
    (t
     value)))

(-> agent-context--value (t) cl-protobufs.autolith.common.v1:value)
(defun agent-context--value (value)
  "Encode logical JSON with the existing bounded protocol converter."
  (autolith-supervisor::protocol-data->value (agent-context--data value)))

(-> agent-context--map (hash-table) cl-protobufs.autolith.common.v1:value-map)
(defun agent-context--map (value)
  "Encode an object without accepting arbitrary request fields."
  (cl-protobufs.autolith.common.v1:value.map-value (agent-context--value value)))

(-> agent-context--call (hash-table) agent-wire:tool-call)
(defun agent-context--call (item)
  "Map a logical call and preserve malformed argument strings exactly."
  (let* ((arguments (json-get item "arguments"))
         (call      (agent-wire:make-tool-call
                     :call-id   (json-get item "call_id")
                     :name      (json-get item "name")
                     :namespace (or (json-get item "namespace") ""))))
    (cond
      ((stringp arguments)
       (setf (agent-wire:tool-call.arguments-json call) (copy-seq arguments)))
      ((hash-table-p arguments)
       (setf (agent-wire:tool-call.arguments call) (agent-context--map arguments)))
      (t
       (agent-context--fail ':invalid-context-arguments)))
    call))

(-> agent-context--block (t) agent-wire:content-block)
(defun agent-context--block (part)
  "Map a text or inline image block, never fetching a URL or file."
  (when (stringp part)
    (return-from agent-context--block (agent-wire:make-content-block :text part)))
  (unless (hash-table-p part)
    (agent-context--fail ':invalid-context-block))
  (let ((type (json-get part "type")))
    (cond
      ((member type '("input_text" "output_text" "text") :test #'equal)
       (agent-wire:make-content-block :text (json-get part "text")))
      ((equal type "reasoning_text")
       (agent-wire:make-content-block :reasoning-text (json-get part "text")))
      ((equal type "input_image")
       (let* ((url       (json-get part "image_url"))
              (comma     (and (stringp url) (position #\, url)))
              (semicolon (and comma (position #\; url :end comma))))
         (unless (and semicolon (uiop:string-prefix-p "data:" url)
                      (string= url ";base64" :start1 semicolon :end1 comma))
           (agent-context--fail ':non-inline-context-media))
         (agent-wire:make-content-block
          :media (agent-wire:make-media
                  :media-type (subseq url 5 semicolon)
                  :data       (cl-base64:base64-string-to-usb8-array (subseq url (1+ comma)))))))
      (t
       (agent-context--fail ':unsupported-context-block)))))

(-> agent-context--message (hash-table) agent-wire:conversation-message)
(defun agent-context--message (item)
  "Map the existing logical message representation, preserving block order."
  (let ((role    (json-get item "role"))
        (content (json-get item "content")))
    (agent-wire:make-conversation-message
     :role    (cond
                ((member role '("system" "developer") :test #'equal)
                 ':system)
                ((equal role "user")
                 ':user)
                ((equal role "assistant")
                 ':assistant)
                ((equal role "tool")
                 ':tool)
                (t
                 (agent-context--fail ':unsupported-context-role)))
     :content (if (stringp content)
                  (list (agent-wire:make-content-block :text content))
                  (map 'list #'agent-context--block content)))))

(-> agent-context--tool (hash-table &key (:namespace string) (:description string))
    agent-wire:tool-definition)
(defun agent-context--tool (tool &key (namespace "") (description ""))
  "Map one local function schema, with its namespace and optional settings."
  (unless (member (json-get tool "type") '(nil "function") :test #'equal)
    (agent-context--fail ':unsupported-context-tool))
  (let ((definition
          (agent-wire:make-tool-definition
           :name         (json-get tool "name")
           :description  (or (json-get tool "description") "")
           :input-schema (agent-context--map (json-get tool "parameters"))
           :namespace namespace :namespace-description description
           :deferred     (not (null (member (json-get tool "defer_loading") (list t yason:true)))))))
    (multiple-value-bind (strict present) (gethash "strict" tool)
      (when present
        (unless (member strict (list t yason:true yason:false ':json-false))
          (agent-context--fail ':invalid-context-strict))
        (setf (agent-wire:tool-definition.strict definition)
              (not (null (member strict (list t yason:true)))))))
    definition))

(-> agent-context--tools (vector) list)
(defun agent-context--tools (namespaces)
  "Flatten logical namespace schemas without provider-specific name encoding."
  (loop for entry across namespaces append
        (if (equal (json-get entry "type") "namespace")
            (map 'list
                 (lambda (tool)
                   (agent-context--tool tool :namespace  (json-get entry "name")
                                            :description (or (json-get entry "description") "")))
                 (json-get entry "tools"))
            (list (agent-context--tool entry)))))

(-> agent-context-item->transcript (hash-table) agent-wire:transcript-item)
(defun agent-context-item->transcript (item)
  "Project a logical input or a parent public output to a typed transcript item.
Parent outputs contain only public OutputItem data. Their views are never rebuilt
as new provider input: only the authoritative replay handle is sent. Legacy opaque
provider items without a parent handle are rejected."
  (unless (equal (json-get item "type") "parent_output")
    (dolist (key '("id" "status" "signature" "encrypted_content" "replay_handle"))
      (when (nth-value 1 (gethash key item))
        (agent-context--fail ':opaque-context-without-handle))))
  (let ((type (json-get item "type")))
    (cond
      ((equal type "parent_output")
       (let ((handle (json-get item "replay_handle")))
         (unless (non-empty-string-p handle)
           (agent-context--fail ':missing-context-replay-handle))
         (agent-wire:make-transcript-item :replay-handle handle)))
      ((or (null type) (equal type "message"))
       (agent-wire:make-transcript-item :message (agent-context--message item)))
      ((equal type "function_call")
       (agent-wire:make-transcript-item :tool-call (agent-context--call item)))
      ((equal type "function_call_output")
       (agent-wire:make-transcript-item
        :tool-result (agent-wire:make-tool-result
                      :call-id (json-get item "call_id")
                      :content (agent-context--value (json-get item "output")))))
      ((equal type "tool_search_output")
       (agent-wire:make-transcript-item
        :tool-discovery-result
        (agent-wire:make-tool-discovery-result
         :call-id (json-get item "call_id")
         :tools   (agent-context--tools (json-get item "tools")))))
      (t
       (agent-context--fail ':unsupported-context-item)))))

(-> agent-context-project
    (configuration conversation vector
     &key (:profile-id string) (:turn-id string) (:goal-context (option string))
          (:summary-p boolean) (:kind keyword) (:stable-prefix (option list))
          (:max-output-tokens (option integer)) (:temperature (option double-float))
          (:reasoning-effort (option string)) (:reasoning-summary (option string))
          (:continuation-handle (option string)) (:turn-state-handle (option string)))
    agent-context-projection)
(defun agent-context-project
    (configuration conversation tool-namespaces
     &key profile-id turn-id goal-context summary-p (kind ':generate) stable-prefix
       max-output-tokens temperature reasoning-effort reasoning-summary
       continuation-handle turn-state-handle)
  "Assemble an optional credential-free ModelStart using current context delivery.
Read the existing logical conversation projection, not provider requests. The caller
selects an opaque parent profile ID. Summary generation omits tools, ephemeral input,
and normal context delivery. Native compaction is not implemented by this service.
Keep the returned delivery unconsumed on send failure, retries, and terminal failure."
  (unless (and (eq kind ':generate) (non-empty-string-p profile-id)
               (non-empty-string-p turn-id) (not (and summary-p reasoning-summary)))
    (agent-context--fail ':invalid-context-request))
  (let* ((namespaces (if summary-p #() tool-namespaces))
         (delivery   (unless summary-p
                       (context-resolve-request configuration conversation namespaces
                                                :goal-context goal-context)))
         (durable    (conversation-input-items-for-request conversation :include-ephemeral-p nil))
         (ephemeral  (unless summary-p
                       (mapcar (lambda (entry) (getf entry :item))
                               (conversation-ephemeral-input-entries conversation)))))
    (multiple-value-bind (items stable volatile)
        (context-delivery-project-items delivery conversation durable)
      (let ((start
              (agent-wire:make-model-start
               :profile-id profile-id :turn-id turn-id :kind ':generate
               :purpose         (if summary-p ':ordinary-summary ':purpose-unspecified)
               :items           (mapcar #'agent-context-item->transcript items)
               :ephemeral-items (mapcar #'agent-context-item->transcript ephemeral)
               :tools           (agent-context--tools namespaces)
               :stable-prefix   (or stable-prefix (list (system-prompt configuration)))
               :request-suffix  (remove nil (if summary-p
                                               (list *compaction-instructions*)
                                               (append (list goal-context) stable (list volatile)))))))
        (when max-output-tokens
          (setf (agent-wire:model-start.max-output-tokens start) max-output-tokens))
        (when temperature
          (setf (agent-wire:model-start.temperature start) temperature))
        (when reasoning-effort
          (setf (agent-wire:model-start.reasoning-effort start) reasoning-effort))
        (when reasoning-summary
          (setf (agent-wire:model-start.reasoning-summary start) reasoning-summary))
        (when continuation-handle
          (setf (agent-wire:model-start.continuation-handle start) continuation-handle))
        (when turn-state-handle
          (setf (agent-wire:model-start.turn-state-handle start) turn-state-handle))
        (make-instance 'agent-context-projection :start start :delivery delivery)))))

(-> agent-context-accept (agent-context-projection agent-wire:model-complete) list)
(defun agent-context-accept (projection complete)
  "Return detached public output items and consume receipts for accepted COMPLETE.
The caller must supply the correlated terminal COMPLETE returned by the sole
AGENT-PROTOCOL-RECEIVE path, after stream validation. Provisional items, deltas,
CancelAck, and Failure are not completion inputs. This operation installs no reader.
An exact repeated completion is idempotent; a changed completion is rejected.
Public wrappers are JSON data suitable for the optional parent context projection,
not legacy provider originals. Keep continuation and turn state as opaque handles."
  (unless (and (null (agent-wire:model-complete.message complete))
               (member (agent-wire:model-complete.reason complete)
                       '(:stop :tool-calls :output-limit :filtered))
               (eq (agent-wire:model-complete.compaction-outcome complete)
                   ':compaction-outcome-unspecified))
    (agent-context--fail ':invalid-context-complete))
  (let* ((bytes (cl-protobufs:serialize-to-bytes complete))
         (old   (agent-context-projection-accepted projection))
         (items
           (loop for item in (agent-wire:model-complete.output-items complete)
                 for index from 0
                 collect
                 (progn
                   (unless (and (= index (agent-wire:output-item.item-index item))
                                (non-empty-string-p (agent-wire:output-item.replay-handle item))
                                (non-empty-string-p (agent-wire:output-item.family item))
                                (agent-wire:output-item.view-case item))
                     (agent-context--fail ':invalid-context-output))
                   (json-object
                    "type" "parent_output"
                    "replay_handle" (copy-seq (agent-wire:output-item.replay-handle item))
                    "family" (copy-seq (agent-wire:output-item.family item))
                    "view" (agent-context--json
                            (autolith-supervisor::protocol-message->data item)))))))
    (when (and old (not (equalp old bytes)))
      (agent-context--fail ':changed-context-complete))
    (unless old
      (context-delivery-complete (agent-context-projection-delivery projection))
      (setf (agent-context-projection-accepted projection) bytes))
    items))
