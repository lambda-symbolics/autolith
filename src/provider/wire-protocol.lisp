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
          (string= (configuration-web-search-mode configuration) "disabled"))
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
  "Return true when MODEL names documented GPT-5.4 or later."
  (handler-case
      (let* ((major-start 4)
             (major-end (and (uiop:string-prefix-p "gpt-" model)
                             (position #\. model :start major-start)))
             (minor-start (and major-end (1+ major-end)))
             (minor-end (and minor-start
                             (position-if-not #'digit-char-p model
                                              :start minor-start)))
             (major (and major-end
                         (parse-integer model
                                        :start major-start
                                        :end major-end)))
             (minor (and minor-start
                         (> (or minor-end (length model)) minor-start)
                         (parse-integer model
                                        :start minor-start
                                        :end minor-end))))
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
   (configuration-model (provider-configuration provider))))

(-> provider-deferred-namespace-tool (json-object) json-object)
(defun provider-deferred-namespace-tool (tool)
  "Convert one local TOOL schema to a deferred native namespace child."
  (json-object
   "type" "function"
   "name" (json-get tool "name")
   "description" (json-get tool "description")
   "strict" false
   "defer_loading" t
   "parameters" (json-get tool "parameters")))

(-> provider-deferred-namespace (json-object) json-object)
(defun provider-deferred-namespace (namespace)
  "Convert one local NAMESPACE to native deferred Responses wire form."
  (json-object
   "type" "namespace"
   "name" (json-get namespace "name")
   "description" (json-get namespace "description")
   "tools" (map 'vector #'provider-deferred-namespace-tool
                (json-get namespace "tools"))))
(defmethod provider-wire-tool-name
    ((provider codex-subscription-provider) (namespace string) (name string))
  "Encode one Codex tool name with the shared grammar-safe wire codec."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

(defmethod provider-wire-tools
    ((provider codex-subscription-provider) (tool-namespaces vector))
  "Use native deferred namespaces on capable Codex models, else eager tools."
  (if (provider-deferred-tool-loading-p provider)
      (let ((deferred-p nil))
        (concatenate
         'vector
         (map 'vector
              (lambda (entry)
                (if (and (json-object-p entry)
                         (json-string= (json-get entry "type") "namespace"))
                    (progn
                      (setf deferred-p t)
                      (provider-deferred-namespace entry))
                    entry))
              tool-namespaces)
         (if deferred-p
             (json-array (json-object "type" "tool_search"))
             #())))
      (call-next-method)))

(defmethod provider-wire-input-item
    ((provider codex-subscription-provider) item)
  "Preserve namespace calls and strip invalid tool search expansions.

The server emits tool_search_output items whose deferred functions carry
null parameters and output_schema fields, and its own input validator
rejects that shape verbatim (invalid_function_parameters on the first
child). The Codex reference replays these items with an empty tools
vector when trimming, which the server accepts; doing so on every replay
also keeps the already-consumed expansion from re-entering the prompt."
  (cond
    ((and (json-object-p item)
          (json-string= (json-get item "type") "tool_search_output"))
     (let ((copy (json-object-copy item)))
       (setf (gethash "tools" copy) (json-array))
       copy))
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
  "Project product history, prompt policy, and context into a Responses request."
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
             (context-resolve-request configuration conversation effective-namespaces
                                      :goal-context goal-context)))
         (projection
           (make-instance 'cl-llm-provider-api::wire-request :model
                          (configuration-model configuration) :items
                          (conversation-input-items-for-family conversation
                                                               (provider-family
                                                                provider)
                                                               :include-ephemeral-p
                                                               (not compaction-p))
                          :prefix
                          (list
                           (let ((*system-prompt-hosted-web-search-p*
                                   hosted-web-search-p))
                             (system-prompt configuration)))
                          :suffix
                          (list (and (not compaction-p) goal-context)
                                (and delivery (context-delivery-rendered delivery))
                                (and compaction-p *compaction-instructions*))
                          :options
                          (list :reasoning-effort
                                (provider-responses-wire-effort provider configuration)
                                :reasoning-summary
                                (and (not compaction-p)
                                     (provider-responses-reasoning-summary provider
                                                                           configuration))
                                :maximum-output-tokens
                                (and (provider-output-ceiling-p provider)
                                     *provider-maximum-output-tokens*)
                                :fields
                                (provider-responses-request-fields provider conversation
                                                                   :compaction-p
                                                                   compaction-p)))))
    (values
     (provider-request-object provider projection effective-namespaces :compaction-p
                              compaction-p)
     delivery)))
