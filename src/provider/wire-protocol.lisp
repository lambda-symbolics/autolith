(in-package #:autolith)


;;;; -- Request Tool Filtering --

(-> provider-hosted-web-search-tool-p (t) boolean)
(defun provider-hosted-web-search-tool-p (tool)
  "Return true when TOOL declares provider-hosted web or social search."
  (and (json-object-p tool)
       (let ((type (json-get tool "type")))
         (and (stringp type)
              (or (uiop:string-prefix-p "web_search" type)
                  (string= type "x_search"))))
       t))

(-> provider-hosted-web-search-tools-p (list) boolean)
(defun provider-hosted-web-search-tools-p (tools)
  "Return true when TOOLS contains a provider-hosted search declaration."
  (and (some #'provider-hosted-web-search-tool-p tools) t))

(-> provider-request--without-web-run (json-object) (option json-object))
(defun provider-request--without-web-run (entry)
  "Return a copy of namespace ENTRY without web.run, or NIL when empty.

web.run is the only provider-backed web search tool. Independent web
namespace tools such as web_extra.gist page retrieval keep working without
provider search and stay advertised."
  (if (and (json-object-p entry)
           (json-string= (json-get entry "type") "namespace")
           (json-string= (json-get entry "name") "web"))
      (let ((tools
              (remove "run"
                      (coerce (json-get entry "tools") 'list)
                      :key (lambda (tool)
                             (and (json-object-p tool)
                                  (json-get tool "name")))
                      :test #'json-string=)))
        (and tools
             (json-object
              "type" (json-get entry "type")
              "name" (json-get entry "name")
              "description" (json-get entry "description")
              "tools" (coerce tools 'vector))))
      entry))

(-> provider-request-tool-namespaces
    (configuration vector &key (:hosted-web-search-p boolean))
    vector)
(defun provider-request-tool-namespaces
    (configuration tool-namespaces &key hosted-web-search-p)
  "Omit local web.run when search is disabled or a hosted search tool is served.

Independent web namespace tools, such as web_extra.gist page retrieval,
stay available because they do not depend on provider web search."
  (if (or hosted-web-search-p
          (string= (config :web-search-mode configuration) "disabled"))
      (coerce
       (loop for entry across tool-namespaces
             for filtered = (provider-request--without-web-run entry)
             when filtered
               collect filtered)
       'vector)
      tool-namespaces))


;;;; -- Responses Protocol --

(-> provider-deferred-tool-loading-p (model-provider) boolean)
(defgeneric provider-deferred-tool-loading-p (provider)
  (:documentation
   "Return true when PROVIDER supports native deferred namespace discovery."))

(defmethod provider-deferred-tool-loading-p ((provider model-provider))
  "Disable deferred discovery for providers without an explicit capability."
  (declare (ignore provider))
  nil)

(-> provider-deferred-tool-model-p (string) boolean)
(defun provider-deferred-tool-model-p (model)
  "Return true when MODEL names documented GPT-5.4 or later.

The name carries a major version and an optional dotted minor version, so
gpt-5.6-terra and gpt-6.1-sol both qualify while gpt-5.3-codex does not. The
Codex model catalog at https://github.com/openai/codex commit
444da310e108da16aaeb18fd790b0ac464f08aca marks every GPT-5.6, GPT-6, and GPT-6.1
entry with supports_search_tool."
  (handler-case
      (let* ((major-start 4)
             (major-end (and (uiop:string-prefix-p "gpt-" model)
                             (or (position-if-not #'digit-char-p model
                                                  :start major-start)
                                 (length model))))
             (major (and major-end
                         (> major-end major-start)
                         (parse-integer model
                                        :start major-start
                                        :end major-end)))
             (minor-start (and major
                               (< major-end (length model))
                               (char= (char model major-end) #\.)
                               (1+ major-end)))
             (minor-end (and minor-start
                             (position-if-not #'digit-char-p model
                                              :start minor-start)))
             (minor (cond
                      ((null major)
                       nil)
                      ((null minor-start)
                       0)
                      ((> (or minor-end (length model)) minor-start)
                       (parse-integer model
                                      :start minor-start
                                      :end minor-end))
                      (t
                       nil))))
        (and major minor
             (or (> major 5)
                 (and (= major 5) (>= minor 4)))
             t))
    (error ()
      nil)))

(defmethod provider-deferred-tool-loading-p
    ((provider codex-subscription-provider))
  "Enable native tool search on documented GPT-5.4 and later Codex models."
  (provider-deferred-tool-model-p
   (config :model (provider-configuration provider))))

(defparameter *provider-history-trimming-p* nil
  "True while history is projected for a compaction request.

Consumed tool search expansions are replayed empty there, as the Codex
reference does when trimming, and intact everywhere else so the server keeps
the expanded tools loaded without another search.")

(defparameter *codex-tool-search-description*
  (format nil "# Tool discovery~2%Searches the deferred tool namespaces by namespace name, tool name, and description, and exposes the matching tools for the next model call. Some tools are not provided upfront; find them here before calling them. A namespace name such as resource or shell loads that whole namespace. Loaded tools stay available for the rest of the conversation, so search for each namespace once.")
  "The model-visible description of Autolith's client-executed tool search.")

(-> provider-tool-search-namespaces (model-provider vector) vector)
(defgeneric provider-tool-search-namespaces (provider tool-namespaces)
  (:documentation
   "Return the subset of TOOL-NAMESPACES a tool search may expose for PROVIDER."))

(defmethod provider-tool-search-namespaces
    ((provider model-provider) (tool-namespaces vector))
  "Expose every offered namespace for providers without request filtering."
  (declare (ignore provider))
  tool-namespaces)

(defmethod provider-tool-search-namespaces
    ((provider responses-api-provider) (tool-namespaces vector))
  "Apply the same local tool filtering a Responses request applies."
  (let ((configuration (provider-configuration provider)))
    (provider-responses-request-namespaces
     provider
     (provider-request-tool-namespaces
      configuration tool-namespaces
      :hosted-web-search-p
      (provider-hosted-web-search-tools-p
       (provider-responses-hosted-tools provider configuration))))))

(-> provider-answer-tool-search (model-provider json-object vector) json-object)
(defun provider-answer-tool-search (provider call tool-namespaces)
  "Return PROVIDER's tool_search_output for CALL over the offered TOOL-NAMESPACES."
  (provider-tool-search-output
   call
   (provider-tool-search-namespaces provider tool-namespaces)))

(defmethod provider-wire-tool-name
    ((provider codex-subscription-provider) (namespace string) (name string))
  "Encode one Codex tool name with the shared grammar-safe wire codec."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

(defmethod provider-wire-tools
    ((provider codex-subscription-provider) (tool-namespaces vector))
  "Use native deferred namespaces on capable Codex models, else eager tools."
  (if (provider-deferred-tool-loading-p provider)
      (provider-deferred-wire-tools tool-namespaces
                                    :description *codex-tool-search-description*)
      (call-next-method)))

(defmethod provider-wire-input-item
    ((provider codex-subscription-provider) item)
  "Preserve namespace calls and replay tool search expansions intact.

The server remembers which deferred tools are loaded only through the
tool_search_output items in the request history, so an expansion must
replay with its tools on every later request of the conversation; replaying
it empty made the model search the same namespace again on every round. The
Codex reference at commit 18194bfd3534ca567d886eac454028dafaa68b6c empties the
tools only when trimming history for compaction, which
*provider-history-trimming-p* marks here."
  (cond
    ((and (json-object-p item)
          (json-string= (json-get item "type") "tool_search_output"))
     (provider-tool-search-output-replay item :trimming-p *provider-history-trimming-p*))
    ((and (provider-deferred-tool-loading-p provider)
          (json-object-p item)
          (function-call-item-p item)
          (non-empty-string-p (json-get item "namespace")))
     item)
    (t
     (call-next-method))))

(defmethod provider-normalize-output-item
    ((provider codex-subscription-provider) (item hash-table))
  "Restore standard Codex Responses calls to their local namespace shape."
  (call-next-method)
  (when (function-call-item-p item)
    (multiple-value-bind (namespace name)
        (provider-wire-function-name--decode (json-get item "name"))
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name))))
  item)

(defmethod provider-responses-wire-effort
    ((provider codex-subscription-provider) configuration)
  "Return CONFIGURATION's Codex reasoning effort."
  (declare (ignore provider))
  (configuration-wire-effort configuration))

(defmethod provider-responses-reasoning-summary
    ((provider codex-subscription-provider) configuration)
  "Request automatic Codex summaries when visible reasoning is enabled."
  (declare (ignore configuration))
  (when (provider-reasoning-summaries-p provider)
    "auto"))

(defmethod provider-responses-hosted-tools
    ((provider codex-subscription-provider) configuration)
  "Return Codex's enabled hosted tool declarations."
  (declare (ignore provider))
  (let ((web-search-tool (provider-web-search-tool configuration)))
    (when web-search-tool
      (list web-search-tool))))

(defmethod provider-responses-instructions-placement
    ((provider codex-subscription-provider))
  "Place Codex's stable system prompt in the top-level instructions field."
  (declare (ignore provider))
  ':top-level)

(defmethod provider-responses-request-fields
    ((provider codex-subscription-provider)
     (conversation conversation)
     &key compaction-p)
  "Return the fields sent by one Codex Responses request."
  (provider--codex-responses-request-fields
   provider conversation :compaction-p compaction-p))


(defmethod provider-request-object
    ((provider responses-api-provider) (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Project product history, prompt policy, and context into a Responses request.

Return the request, its context delivery, and the actual suffix texts from this
assembly. The third value is private evidence for Codex placement validation."
  (let* ((configuration (provider-configuration provider))
         (hosted-tools
           (and (not compaction-p)
                (provider-responses-hosted-tools provider configuration)))
         (hosted-web-search-p (provider-hosted-web-search-tools-p hosted-tools))
         (request-namespaces
           (provider-request-tool-namespaces configuration tool-namespaces
                                             :hosted-web-search-p hosted-web-search-p))
         (effective-namespaces
           (if compaction-p
               #()
               (concatenate 'vector
                            (provider-responses-request-namespaces provider
                                                                  request-namespaces)
                            (coerce hosted-tools 'vector))))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation request-namespaces
                                      :goal-context goal-context)))
         (history
           (conversation-input-items-for-family
            conversation (provider-family provider)
            :include-ephemeral-p (not compaction-p))))
    (multiple-value-bind (items context-tail)
        (context-delivery-project-items delivery conversation history)
      (let* ((suffix (list (and (not compaction-p) goal-context)
                           context-tail
                           (and compaction-p *compaction-instructions*)))
             (projection
              (make-instance
               'cl-llm-provider-api:wire-request
               :model (config :model configuration)
               :items items
               :prefix (list
                        (let ((*system-prompt-hosted-web-search-p*
                                hosted-web-search-p))
                          (system-prompt configuration)))
               :suffix suffix
               :options
               (list :reasoning-effort
                     (provider-responses-wire-effort provider configuration)
                     :reasoning-summary
                     (and (not compaction-p)
                          (provider-responses-reasoning-summary provider configuration))
                     :maximum-output-tokens
                     (and (provider-output-ceiling-p provider)
                          *provider-maximum-output-tokens*)
                     :fields
                     (provider-responses-request-fields provider conversation
                                                        :compaction-p compaction-p)))))
        (values
         (provider-request-object provider projection effective-namespaces
                                  :compaction-p compaction-p)
         delivery
         ;; Private assembly evidence for Codex's exact current-suffix guard.
         ;; Anchored contributions are already in ITEMS, not CONTEXT-TAIL.
         suffix)))))


;;;; -- Codex Request-Context Placement --

(-> provider--codex-context-digest (vector) string)
(defun provider--codex-context-digest (items)
  "Hash context placement evidence without retaining prompt contents."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (json-encode-utf8 items))))

(defparameter *codex-context-anchor-states* (make-hash-table :test #'eq :weakness :key)
  "Hash-only request-context placement state, weakly keyed by conversation.")

(defparameter *codex-context-anchor-lock* (make-lock "Codex request-context placement")
  "Serialize placement-state updates without changing conversation history.")

(-> provider--codex-anchor-context (vector (integer 0) list)
    (values vector list))
(defun provider--codex-anchor-context (items suffix-count state)
  "Place identically resupplied context at its previous tool-round boundary.

STATE contains only positions and SHA-256 digests of the current context and
complete history. Reuse requires append-only history without new user or
higher-priority messages. Old context is never copied from STATE."
  (check-type suffix-count (integer 0))
  (assert (<= suffix-count (length items)))
  (when (zerop suffix-count)
    (return-from provider--codex-anchor-context (values items nil)))
  (let* ((history-count (- (length items) suffix-count))
         (history (subseq items 0 history-count))
         (history-digest (provider--codex-context-digest history))
         (context-digest
           (provider--codex-context-digest (subseq items history-count)))
         (previous-anchor (getf state :anchor))
         (previous-count (getf state :history-count))
         (reuse-p
           (and (typep previous-anchor '(integer 0))
                (typep previous-count '(integer 0))
                (<= previous-anchor previous-count history-count)
                (equal context-digest (getf state :context-digest))
                (equal (if (= previous-count history-count)
                           history-digest
                           (provider--codex-context-digest
                            (subseq history 0 previous-count)))
                       (getf state :history-digest))
                (loop for index from previous-anchor below history-count
                      never (member (json-get (aref items index) "role")
                                    '("user" "developer" "system")
                                    :test #'equal))))
         (anchor (if reuse-p previous-anchor history-count))
         (result items))
    (when (< anchor history-count)
      (setf result (make-array (length items)))
      (replace result items :end2 anchor)
      (replace result items :start1 anchor :start2 history-count)
      (replace result items :start1 (+ anchor suffix-count)
                            :start2 anchor :end2 history-count))
    (values result
            (list :anchor anchor :history-count history-count
                  :context-digest context-digest :history-digest history-digest))))

(defmethod provider-request-object :around
    ((provider codex-subscription-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Stabilize the placement of currently supplied Codex request context.

Identical context keeps its original tool-round boundary so later rounds
extend the full previous request. Only current request objects are reused;
changed or omitted context is never recovered from placement state."
  (declare (ignore tool-namespaces goal-context))
  (multiple-value-bind (request delivery suffix) (call-next-method)
    ;; This lock protects placement evidence, not conversation mutation or
    ;; network dispatch order. Concurrent constructors may reset one another's
    ;; evidence; full-history validation makes that a cache miss, not stale input.
    (with-lock-held (*codex-context-anchor-lock*)
      (if compaction-p
          ;; Detached compaction views have their own key; the original
          ;; conversation resets when its compacted history fails validation.
          (remhash conversation *codex-context-anchor-states*)
          (let* ((texts (remove-if-not #'non-empty-string-p suffix))
                 (expected (map 'vector #'responses-developer-message texts))
                 (input (json-get request "input"))
                 (count (length expected)))
            ;; Use the exported wire helper, also imported by this package, so
            ;; a changed serializer fails closed instead of moving unknown input.
            (if (and (vectorp input) (not (stringp input))
                     (<= count (length input))
                     (loop for item across expected
                           for index from (- (length input) count)
                           always (string= (json-encode item)
                                           (json-encode (aref input index)))))
                (multiple-value-bind (anchored state)
                    (provider--codex-anchor-context
                     input count (gethash conversation *codex-context-anchor-states*))
                  (setf (gethash "input" request) anchored)
                  (if state
                      (setf (gethash conversation *codex-context-anchor-states*) state)
                      (remhash conversation *codex-context-anchor-states*)))
                (remhash conversation *codex-context-anchor-states*)))))
    (values request delivery)))
