;;;; -- Logical Model Request Projection --


(in-package #:autolith-supervisor)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:model-wire '#:cl-llm-provider-api '#:autolith-supervisor))


(declaim (ftype (function (string keyword) string) model-projection--required-string))

(defun model-projection--required-string (value field)
  "Require a nonempty semantic identifier and return a copy."
  (when (zerop (length value)) (model-projection--fail field ':missing))
  (copy-seq value))

(declaim (ftype (function (t) t) model-projection--json-data))

(defun model-projection--json-data (data)
  "Translate bounded parent portable data to library JSON without reading paths."
  (cond ((eq data ':protocol-null)
         nil) ((null data)
               yason:false)
        ((hash-table-p data)
         (let ((result (make-hash-table :test #'equal)))
           (maphash
            (lambda (key value) (setf (gethash key result) (model-projection--json-data value)))
            data)
           result))
        ((typep data '(array (unsigned-byte 8) (*)))
         (model-projection--fail ':value ':bytes-not-json))
        ((and (vectorp data) (not (stringp data)))
         (map 'vector #'model-projection--json-data data))
        (t
         data)))

(declaim (ftype (function (common-proto:value-map) hash-table) model-projection--map))

(defun model-projection--map (map)
  "Decode a ValueMap through the bounded shared parent converter."
  (model-projection--json-data (protocol-value->data (common-proto:make-value :map-value map))))

(declaim (ftype (function (parent-proto:tool-call) hash-table) model-projection--call))

(defun model-projection--call (call)
  "Project exactly one argument representation, keeping raw JSON unchanged."
  (when
      (eq (not (null (parent-proto:tool-call.has-arguments call)))
          (not (null (parent-proto:tool-call.has-arguments-json call))))
    (model-projection--fail ':arguments ':ambiguous-presence))
  (let ((item
         (model-wire::json-object "type" "function_call" "call_id"
                                  (model-projection--required-string
                                   (parent-proto:tool-call.call-id call) ':call-id)
                                  "name"
                                  (model-projection--required-string
                                   (parent-proto:tool-call.name call) ':name)
                                  "arguments"
                                  (if (parent-proto:tool-call.has-arguments-json call)
                                      (copy-seq (parent-proto:tool-call.arguments-json call))
                                      (model-wire::json-encode
                                       (model-projection--map
                                        (parent-proto:tool-call.arguments call)))))))
    (unless (zerop (length (parent-proto:tool-call.namespace call)))
      (setf (gethash "namespace" item) (copy-seq (parent-proto:tool-call.namespace call))))
    item))

(declaim (ftype (function (parent-proto:tool-result) hash-table) model-projection--result))

(defun model-projection--result (result)
  "Project a tool result without implicit bounded printing or truncation."
  (unless (parent-proto:tool-result.has-content result)
    (model-projection--fail ':content ':missing))
  (when (parent-proto:tool-result.is-error result)
    (model-projection--fail ':is-error ':unsupported))
  (let ((data
         (model-projection--json-data
          (protocol-value->data (parent-proto:tool-result.content result)))))
    (model-wire::json-object "type" "function_call_output" "call_id"
                             (model-projection--required-string
                              (parent-proto:tool-result.call-id result) ':call-id)
                             "output"
                             (if (stringp data)
                                 data
                                 (model-wire::json-encode data)))))

(declaim (ftype (function (parent-proto:media model-profile) hash-table) model-projection--media))

(defun model-projection--media (media profile)
  "Accept only installed image MIME types and nonempty inline bytes."
  (let ((mime (parent-proto:media.media-type media)) (data (parent-proto:media.data media)))
    (unless
        (and (member ':media (model-profile-capabilities profile))
             (member mime (model-profile-media-types profile) :test #'string=)
             (member mime '("image/png" "image/jpeg" "image/gif" "image/webp") :test #'string=)
             (plusp (length data)))
      (model-projection--fail ':media ':unsupported))
    (model-wire::json-object "type" "input_image" "image_url"
                             (format nil "data:~a;base64,~a" mime
                                     (cl-base64:usb8-array-to-base64-string data)))))

(declaim
 (ftype (function (parent-proto:conversation-message model-profile) list)
  model-projection--message))

(defun model-projection--message (message profile)
  "Project content in order, splitting messages only at typed tool boundaries."
  (let ((role
         (case (parent-proto:conversation-message.role message)
           (:system
            "system")
           (:user
            "user")
           (:assistant
            "assistant")
           (:tool
            "tool")
           (otherwise
            (model-projection--fail ':role ':unsupported))))
        (items nil)
        (parts nil))
    (labels ((flush ()
               "Flush adjacent ordinary content without changing item order."
               (when parts
                 (push
                  (model-wire::json-object "type" "message" "role" role "content"
                                           (coerce (nreverse parts) 'vector))
                  items)
                 (setf parts nil))))
      (unless (parent-proto:conversation-message.content message)
        (model-projection--fail ':content ':missing))
      (dolist (block (parent-proto:conversation-message.content message))
        (case (parent-proto:content-block.content-case block)
          (parent-proto:text
           (when (string= role "tool") (model-projection--fail ':content ':role-mismatch))
           (push
            (model-wire::json-object "type"
                                     (if (string= role "assistant")
                                         "output_text"
                                         "input_text")
                                     "text" (copy-seq (parent-proto:content-block.text block)))
            parts))
          (parent-proto:media
           (unless (string= role "user") (model-projection--fail ':media ':role-mismatch))
           (push (model-projection--media (parent-proto:content-block.media block) profile) parts))
          (parent-proto:tool-call
           (unless (string= role "assistant") (model-projection--fail ':tool-call ':role-mismatch))
           (flush)
           (push (model-projection--call (parent-proto:content-block.tool-call block)) items))
          (parent-proto:tool-result
           (unless (string= role "tool") (model-projection--fail ':tool-result ':role-mismatch))
           (flush)
           (push (model-projection--result (parent-proto:content-block.tool-result block)) items))
          (otherwise
           (model-projection--fail ':content ':unsupported))))
      (flush)
      (nreverse items))))

(declaim
 (ftype (function (parent-proto:tool-definition model-profile) hash-table) model-projection--tool))

(defun model-projection--tool (tool profile)
  "Project one logical function, preserving optional strict presence."
  (unless (parent-proto:tool-definition.has-input-schema tool)
    (model-projection--fail ':input-schema ':missing))
  (let ((schema (model-projection--map (parent-proto:tool-definition.input-schema tool))))
    (unless (equal (gethash "type" schema) "object")
      (model-projection--fail ':input-schema ':not-object))
    (when
        (and (parent-proto:tool-definition.deferred tool)
             (not (member ':deferred-tools (model-profile-capabilities profile))))
      (model-projection--fail ':deferred ':unsupported))
    (when
        (and (parent-proto:tool-definition.has-strict tool)
             (parent-proto:tool-definition.strict tool)
             (not (eq (model-profile-family profile) ':responses)))
      (model-projection--fail ':strict ':unsupported))
    (let ((result
           (model-wire::json-object "type" "function" "name"
                                    (model-projection--required-string
                                     (parent-proto:tool-definition.name tool) ':name)
                                    "description"
                                    (copy-seq (parent-proto:tool-definition.description tool))
                                    "parameters" schema)))
      (when (parent-proto:tool-definition.has-strict tool)
        (setf (gethash "strict" result)
              (if (parent-proto:tool-definition.strict tool)
                  t
                  yason:false)))
      (when (parent-proto:tool-definition.deferred tool) (setf (gethash "defer_loading" result) t))
      result)))

(declaim (ftype (function (list model-profile) vector) model-projection--tools))

(defun model-projection--tools (tools profile)
  "Group contiguous namespace entries while preserving tool and namespace order."
  (let ((result nil) (seen (make-hash-table :test #'equal)) (namespace nil))
    (dolist (tool tools)
      (let* ((name        (parent-proto:tool-definition.name tool))
             (space       (parent-proto:tool-definition.namespace tool))
             (description (parent-proto:tool-definition.namespace-description tool))
             (key         (list space name))
             (entry       (model-projection--tool tool profile)))
        (when (gethash key seen) (model-projection--fail ':tools ':duplicate))
        (setf (gethash key seen) t)
        (if (zerop (length space))
            (progn
             (unless (zerop (length description))
               (model-projection--fail ':namespace-description ':missing-namespace))
             (setf namespace nil)
             (push entry result))
            (progn
             (unless (and namespace (string= (gethash "name" namespace) space))
               (when (gethash space seen) (model-projection--fail ':namespace ':noncontiguous))
               (setf (gethash space seen) t
                     namespace
                       (model-wire::json-object "type" "namespace" "name" (copy-seq space)
                                                "description" (copy-seq description) "tools" #()))
               (push namespace result))
             (unless (string= description (gethash "description" namespace))
               (model-projection--fail ':namespace-description ':conflict))
             (setf (gethash "tools" namespace)
                   (concatenate 'vector (gethash "tools" namespace) (vector entry)))))))
    (coerce (nreverse result) 'vector)))

(declaim
 (ftype (function (t string &key (:kind keyword) (:profile model-profile) (:turn-id string)) t)
  model-projection--resolve))

(defun model-projection--resolve (resolver handle &key kind profile turn-id)
  "Ask trusted RESOLVER to validate scope, acceptance, family, and handle kind.
RESOLVER receives HANDLE and keyword arguments :KIND, :PROFILE, :TURN-ID.
It must return two values: the parent-owned value and true for an authorized
handle. A false or missing second value rejects the reference. Item values
must be one library portable JSON object; routing values are parent-private."
  (unless (and resolver (plusp (length handle))) (model-projection--fail ':handle ':unresolved))
  (multiple-value-bind (value accepted-p)
      (funcall resolver handle :kind kind :profile profile :turn-id turn-id)
    (unless (and accepted-p value (or (not (eq kind ':item)) (hash-table-p value)))
      (model-projection--fail ':handle ':unresolved))
    value))

(declaim
 (ftype
  (function (parent-proto:transcript-item model-profile &key (:resolver t) (:turn-id string)) list)
  model-projection--item))

(defun model-projection--item (item profile &key resolver turn-id)
  "Project a typed input item or restore an authorized parent original."
  (case (parent-proto:transcript-item.item-case item)
    (parent-proto:message
     (model-projection--message (parent-proto:transcript-item.message item) profile))
    (parent-proto:tool-call
     (list (model-projection--call (parent-proto:transcript-item.tool-call item))))
    (parent-proto:tool-result
     (list (model-projection--result (parent-proto:transcript-item.tool-result item))))
    (parent-proto:tool-discovery-result
     (unless (member ':deferred-tools (model-profile-capabilities profile))
       (model-projection--fail ':tool-discovery-result ':unsupported))
     (let ((result (parent-proto:transcript-item.tool-discovery-result item)))
       (list
        (model-wire::json-object "type" "tool_search_output" "status" "completed" "execution"
                                 "client" "call_id"
                                 (model-projection--required-string
                                  (parent-proto:tool-discovery-result.call-id result) ':call-id)
                                 "tools"
                                 (model-projection--tools
                                  (parent-proto:tool-discovery-result.tools result) profile)))))
    (parent-proto:replay-handle
     (list
      (model-projection--resolve resolver (parent-proto:transcript-item.replay-handle item) :kind
                                 ':item :profile profile :turn-id turn-id)))
    (otherwise
     (model-projection--fail ':item ':missing-oneof))))

(declaim
 (ftype
  (function (parent-proto:model-start hash-table &key (:replay-resolver t))
   (values model-wire:wire-request vector model-profile keyword boolean t t))
  model-start->wire-request))

(defun model-start->wire-request (start registry &key replay-resolver)
  "Validate START against trusted REGISTRY and project library input.
Return seven values: wire-request, portable tool vector (pass through the
selected provider's PROVIDER-WIRE-TOOLS), profile, request kind, summary-p,
resolved continuation state, and resolved turn state. Native compaction needs
a separate backend operation; never send it as ordinary generation. Neither
this function nor the registry opens files, selects accounts, or creates a
provider. The trusted resolver owns all handle authority checks."
  (let* ((profile      (model-profile-lookup registry (parent-proto:model-start.profile-id start)))
         (family       (model-profile-family profile))
         (turn-id
          (model-projection--required-string (parent-proto:model-start.turn-id start) ':turn-id))
         (kind
          (case (parent-proto:model-start.kind start)
            (:generate
             ':generate)
            (:native-compaction
             ':native-compaction)
            (otherwise
             (model-projection--fail ':kind ':unsupported))))
         (summary-p
          (case (parent-proto:model-start.purpose start)
            (:purpose-unspecified
             ':ordinary)
            (:ordinary-summary
             ':summary)
            (otherwise
             (model-projection--fail ':purpose ':unsupported))))
         (options      (copy-list (model-profile-options profile)))
         (messages     (parent-proto:model-start.messages start))
         (typed-items  (parent-proto:model-start.items start))
         (items        nil)
         (ephemeral    nil)
         (tools        nil)
         (continuation nil)
         (turn-state   nil))
    (setf summary-p (eq summary-p ':summary))
    (when
        (> (length (cl-protobufs:serialize-to-bytes start))
           (model-profile-maximum-request-bytes profile))
      (model-projection--fail ':context ':limit))
    (when summary-p
      (remf options :reasoning-summary)
      (when (member :tool-discovery options) (setf (getf options :tool-discovery) nil)))
    (when (and summary-p (parent-proto:model-start.has-reasoning-summary start))
      (model-projection--fail ':reasoning-summary ':unexpected-for-summary))
    (unless (getf options :maximum-output-tokens)
      (setf (getf options :maximum-output-tokens) (model-profile-maximum-output-tokens profile)))
    (when (and messages typed-items) (model-projection--fail ':items ':mixed-history))
    (when
        (and (eq kind ':native-compaction)
             (or summary-p
                 (not (member ':native-compaction (model-profile-capabilities profile)))))
      (model-projection--fail ':kind ':unsupported))
    (when (parent-proto:model-start.has-temperature start)
      (model-projection--fail ':temperature ':unsupported-by-wire-library))
    (when (parent-proto:model-start.has-max-output-tokens start)
      (let ((limit (parent-proto:model-start.max-output-tokens start)))
        (unless (<= 1 limit (model-profile-maximum-output-tokens profile))
          (model-projection--fail ':max-output-tokens ':limit))
        (setf (getf options :maximum-output-tokens) limit)))
    (let ((limit (getf options :maximum-output-tokens)))
      (unless (and (typep limit '(integer 1))
                   (<= limit (model-profile-maximum-output-tokens profile)))
        (model-projection--fail ':max-output-tokens ':limit)))
    (when
        (and (eq family ':anthropic)
             (> (getf options :maximum-output-tokens)
                model-wire::*anthropic-maximum-output-tokens*))
      (model-projection--fail ':max-output-tokens ':unsupported-by-wire-library))
    (dolist
        (setting
         (list
          (list (parent-proto:model-start.has-reasoning-effort start)
                (parent-proto:model-start.reasoning-effort start)
                (model-profile-reasoning-efforts profile) ':reasoning-effort)
          (list (parent-proto:model-start.has-reasoning-summary start)
                (parent-proto:model-start.reasoning-summary start)
                (model-profile-reasoning-summaries profile) ':reasoning-summary)))
      (destructuring-bind
          (present-p value allowed key)
          setting
        (when present-p
          (unless
              (and (member family '(:responses :chat-completions))
                   (or (not (eq key ':reasoning-summary)) (eq family ':responses))
                   (member value allowed :test #'string=))
            (model-projection--fail key ':unsupported))
          (setf (getf options key) (copy-seq value)))))
    (when (and (getf options :reasoning-summary) (not (getf options :reasoning-effort)))
      (model-projection--fail ':reasoning-summary ':missing-effort))
    (when
        (and (parent-proto:model-start.has-reasoning-effort start) (eq family ':chat-completions)
             (not (getf options :reasoning-parameter)))
      (model-projection--fail ':reasoning-effort ':unsupported))
    (dolist (message messages)
      (setf items (nconc items (model-projection--message message profile))))
    (dolist (item typed-items)
      (setf items
              (nconc items
                     (model-projection--item item profile :resolver replay-resolver :turn-id
                                             turn-id))))
    (dolist (item (parent-proto:model-start.ephemeral-items start))
      (setf ephemeral
              (nconc ephemeral
                     (model-projection--item item profile :resolver replay-resolver :turn-id
                                             turn-id))))
    (setf tools (model-projection--tools (parent-proto:model-start.tools start) profile))
    (dolist (hosted (parent-proto:model-start.hosted-tools start))
      (let ((definition
             (gethash (parent-proto:hosted-tool.capability-id hosted)
                      (model-profile-hosted-tools profile))))
        (unless (hash-table-p definition) (model-projection--fail ':hosted-tools ':unsupported))
        (setf tools (concatenate 'vector tools (vector definition)))))
    (let ((enabled-p
           (if (parent-proto:model-start.has-tool-discovery start)
               (parent-proto:model-start.tool-discovery start)
               (and (eq kind ':generate) (not summary-p) (getf options :tool-discovery)))))
      (when (and enabled-p (not (member ':deferred-tools (model-profile-capabilities profile))))
        (model-projection--fail ':tool-discovery ':unsupported))
      (when (parent-proto:model-start.has-tool-discovery start)
        (setf (getf options :tool-discovery) enabled-p))
      (when enabled-p
        (setf tools (concatenate 'vector tools (vector (model-wire:provider-tool-search-tool))))))
    (when
        (and (some #'parent-proto:tool-definition.deferred (parent-proto:model-start.tools start))
             (not (getf options :tool-discovery)))
      (model-projection--fail ':deferred ':missing-discovery))
    (when (or summary-p (eq kind ':native-compaction))
      (when
          (or (plusp (length tools)) ephemeral
              (and (parent-proto:model-start.has-tool-discovery start)
                   (parent-proto:model-start.tool-discovery start)))
        (model-projection--fail ':purpose ':unexpected-tools))
      (when summary-p
        (remf options :reasoning-summary)
        (when (member :tool-discovery options) (setf (getf options :tool-discovery) nil)))
      (when (and summary-p (eq family ':anthropic)) (setf (getf options :cache-p) nil)))
    (when (parent-proto:model-start.has-continuation-handle start)
      (unless (member ':continuation (model-profile-capabilities profile))
        (model-projection--fail ':continuation-handle ':unsupported))
      (setf continuation
              (model-projection--resolve replay-resolver
                                         (parent-proto:model-start.continuation-handle start) :kind
                                         ':continuation :profile profile :turn-id turn-id)))
    (when (parent-proto:model-start.has-turn-state-handle start)
      (unless (member ':turn-state (model-profile-capabilities profile))
        (model-projection--fail ':turn-state-handle ':unsupported))
      (setf turn-state
              (model-projection--resolve replay-resolver
                                         (parent-proto:model-start.turn-state-handle start) :kind
                                         ':turn-state :profile profile :turn-id turn-id)))
    (if (eq family ':anthropic)
        (setf (getf options :ephemeral-items) ephemeral)
        (setf items (nconc items ephemeral)))
    (values
     (make-instance 'model-wire:wire-request :model (copy-seq (model-profile-model profile)) :items
                    items :prefix
                    (mapcar #'copy-seq (parent-proto:model-start.stable-prefix start)) :suffix
                    (mapcar #'copy-seq (parent-proto:model-start.request-suffix start)) :options
                    options)
     tools profile kind summary-p continuation turn-state)))
