(in-package #:autolith)

;;;; -- Shared Function Names --

(defparameter *provider-wire-function-name-maximum-length* 64
  "Maximum function name length accepted by the shared provider wire codec.")

(-> provider-wire-function-name--literal-character-p (character) boolean)
(defun provider-wire-function-name--literal-character-p (character)
  "Return true when CHARACTER can ride literally in one encoded component."
  (or (and (char<= #\a character) (char<= character #\z))
      (and (char<= #\A character) (char<= character #\Z))
      (and (char<= #\0 character) (char<= character #\9))
      (not (null (find character "_-")))))

(-> provider-wire-function-name--valid-p (t) boolean)
(defun provider-wire-function-name--valid-p (name)
  "Return true when NAME obeys the standard provider function-name grammar."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) *provider-wire-function-name-maximum-length*)
       (every #'provider-wire-function-name--literal-character-p name)
       t))

(-> provider-wire-function-name--escape-sequence-p (string integer) boolean)
(defun provider-wire-function-name--escape-sequence-p (text index)
  "Return true when TEXT has one complete _xHHHHHH_ escape at INDEX."
  (and (<= (+ index 9) (length text))
       (char= (char text index) #\_)
       (char= (char text (1+ index)) #\x)
       (loop for position from (+ index 2) below (+ index 8)
             always (digit-char-p (char text position) 16))
       (char= (char text (+ index 8)) #\_)
       t))

(-> provider-wire-function-name--encode-component (string) string)
(defun provider-wire-function-name--encode-component (component)
  "Escape non-grammar characters while retaining readable component text."
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length component))
          for character = (char component index)
          do (cond
               ((provider-wire-function-name--escape-sequence-p component index)
                (write-string "_x00005F_" output)
                (incf index))
               ((provider-wire-function-name--literal-character-p character)
                (write-char character output)
                (incf index))
               (t
                (let ((code (char-code character)))
                  (unless (<= code #xFFFFFF)
                    (error 'configuration-error
                           :message
                           "A provider tool name contains an unsupported character."))
                  (format output "_x~6,'0X_" code)
                  (incf index)))))))

(-> provider-wire-function-name--decode-component (string) (option string))
(defun provider-wire-function-name--decode-component (encoded)
  "Decode one readable wire-name component, or return NIL when malformed."
  (handler-case
      (with-output-to-string (output)
        (loop with index = 0
              while (< index (length encoded))
              for character = (char encoded index)
              do (cond
                   ((provider-wire-function-name--escape-sequence-p encoded index)
                    (let* ((code
                             (parse-integer encoded
                                            :start (+ index 2)
                                            :end (+ index 8)
                                            :radix 16
                                            :junk-allowed nil))
                           (decoded (code-char code)))
                      (unless decoded
                        (return-from provider-wire-function-name--decode-component nil))
                      (write-char decoded output)
                      (incf index 9)))
                   ((provider-wire-function-name--literal-character-p character)
                    (write-char character output)
                    (incf index))
                   (t
                    (return-from provider-wire-function-name--decode-component nil)))))
    (error ()
      nil)))

;; The length prefix lets the decoder find the namespace boundary without
;; ambiguity, so namespaces following the MCP convention of the form
;; mcp__server, which themselves contain a double underscore, survive decode.
(-> provider-wire-function-name--encode (string string) string)
(defun provider-wire-function-name--encode (namespace name)
  "Encode NAMESPACE and NAME as one readable reversible function name."
  (unless (and (non-empty-string-p namespace) (non-empty-string-p name))
    (error 'configuration-error
           :message "Provider tool namespaces and names must be nonempty strings."))
  (let* ((encoded-namespace
           (provider-wire-function-name--encode-component namespace))
         (encoded-name
           (provider-wire-function-name--encode-component name))
         (wire-name
           (format nil "t_~D_~A__~A"
                   (length encoded-namespace)
                   encoded-namespace
                   encoded-name)))
    (unless (provider-wire-function-name--valid-p wire-name)
      (error 'configuration-error
             :message
             (format nil
                     "Encoded tool name ~A.~A is ~D characters; the provider limit is ~D."
                     namespace name (length wire-name)
                     *provider-wire-function-name-maximum-length*)))
    wire-name))

(-> provider-wire-function-name--decode-readable
    (string)
    (values (option string) (option string)))
(defun provider-wire-function-name--decode-readable (wire-name)
  "Decode one length-prefixed readable Autolith function WIRE-NAME."
  (handler-case
      (let* ((length-end
               (and (uiop:string-prefix-p "t_" wire-name)
                    (position #\_ wire-name :start 2)))
             (encoded-namespace-length
               (and length-end
                    (> length-end 2)
                    (parse-integer wire-name
                                   :start 2
                                   :end length-end
                                   :junk-allowed nil)))
             (namespace-start (and length-end (1+ length-end)))
             (namespace-end
               (and namespace-start encoded-namespace-length
                    (+ namespace-start encoded-namespace-length))))
        (unless (and namespace-end
                     (plusp encoded-namespace-length)
                     (< (+ namespace-end 2) (length wire-name))
                     (char= (char wire-name namespace-end) #\_)
                     (char= (char wire-name (1+ namespace-end)) #\_))
          (return-from provider-wire-function-name--decode-readable
            (values nil nil)))
        (let ((namespace
                (provider-wire-function-name--decode-component
                 (subseq wire-name namespace-start namespace-end)))
              (name
                (provider-wire-function-name--decode-component
                 (subseq wire-name (+ namespace-end 2)))))
          (if (and (non-empty-string-p namespace) (non-empty-string-p name))
              (values namespace name)
              (values nil nil))))
    (error ()
      (values nil nil))))

(-> provider-wire-function-name--decode-legacy
    (string)
    (values (option string) (option string)))
(defun provider-wire-function-name--decode-legacy (wire-name)
  "Decode one legacy Base64 Autolith provider function WIRE-NAME."
  (handler-case
      (let* ((decoded
               (base64-string-to-string
                (padded-base64url (subseq wire-name 1))
                :uri t))
             (separator (position (code-char 0) decoded)))
        (if (and separator
                 (plusp separator)
                 (< (1+ separator) (length decoded)))
            (values (subseq decoded 0 separator)
                    (subseq decoded (1+ separator)))
            (values nil nil)))
    (error ()
      (values nil nil))))

(-> provider-wire-function-name--decode
    (string)
    (values (option string) (option string)))
(defun provider-wire-function-name--decode (wire-name)
  "Decode a readable or legacy Autolith provider function WIRE-NAME."
  (if (provider-wire-function-name--valid-p wire-name)
      (cond
        ((uiop:string-prefix-p "t_" wire-name)
         (provider-wire-function-name--decode-readable wire-name))
        ((char= (char wire-name 0) #\a)
         (provider-wire-function-name--decode-legacy wire-name))
        (t
         (values nil nil)))
      (values nil nil)))


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
namespace tools such as web.gist page retrieval keep working without
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

Independent web namespace tools, such as web.gist page retrieval,
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

(defmethod provider-wire-tool
    ((provider responses-api-provider) (namespace string) (tool hash-table))
  "Encode one namespaced tool as a standard Responses function tool."
  (json-object
   "type" "function"
   "name" (provider-wire-tool-name provider namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "strict" false
   "parameters" (json-get tool "parameters")))

(defmethod provider-wire-tools
    ((provider responses-api-provider) (tool-namespaces vector))
  "Flatten namespaced tools while retaining hosted tool declarations."
  (coerce
   (loop for entry across tool-namespaces
         if (and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (vectorp (json-get entry "tools"))
                 (non-empty-string-p (json-get entry "name")))
           append (loop for tool across (json-get entry "tools")
                        when (and (json-object-p tool)
                                  (non-empty-string-p
                                   (json-get tool "name")))
                          collect (provider-wire-tool
                                   provider
                                   (json-get entry "name")
                                   tool))
         else
           collect entry)
   'vector))

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

(defmethod provider-wire-input-item ((provider responses-api-provider) item)
  "Flatten a namespaced function-call ITEM for a standard Responses request."
  (if (and (json-object-p item)
           (function-call-item-p item)
           (non-empty-string-p (json-get item "namespace"))
           (non-empty-string-p (json-get item "name")))
      (let ((copy (json-object-copy item)))
        (setf (gethash "name" copy)
              (provider-wire-tool-name
               provider
               (json-get item "namespace")
               (json-get item "name")))
        (remhash "namespace" copy)
        copy)
      item))

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
    ((provider responses-api-provider) (item hash-table))
  "Strip server identifiers and restore flat wire calls to namespaced calls."
  (call-next-method)
  (when (function-call-item-p item)
    (let* ((name (json-get item "name"))
           (dot (and (stringp name) (position #\. name))))
      (when (and dot (plusp dot) (< (1+ dot) (length name)))
        (setf (gethash "namespace" item) (subseq name 0 dot)
              (gethash "name" item) (subseq name (1+ dot))))))
  item)

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
    ((provider responses-api-provider)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Build a standard stateless Responses API request for CONVERSATION.

Concrete providers specialize instruction placement, reasoning effort, hosted
tools, served namespaces, and extra request fields. The second value is the
context delivery consumed only after a completed response."
  (let* ((configuration (provider-configuration provider))
         (hosted-tools
           (and (not compaction-p)
                (provider-responses-hosted-tools provider configuration)))
         (hosted-web-search-p
           (provider-hosted-web-search-tools-p hosted-tools))
         (request-namespaces
           (provider-request-tool-namespaces
            configuration
            tool-namespaces
            :hosted-web-search-p hosted-web-search-p))
         (effective-namespaces
           (if compaction-p
               #()
               (concatenate 'vector
                            (provider-responses-request-namespaces
                             provider request-namespaces)
                            (coerce hosted-tools 'vector))))
         (delivery
           (unless compaction-p
             (context-resolve-request
              configuration
              conversation
              effective-namespaces
              :goal-context goal-context)))
         (rendered-context
           (and delivery (context-delivery-rendered delivery)))
         (instruction-prefix
           (list
            (let ((*system-prompt-hosted-web-search-p* hosted-web-search-p))
              (system-prompt configuration))))
         (instruction-suffix
           (list (and (not compaction-p) goal-context)
                 rendered-context
                 (and compaction-p *compaction-instructions*)))
         (instruction-placement
           (provider-responses-instructions-placement provider))
         (top-level-instructions
           (case instruction-placement
             (:input
              nil)
             (:top-level
              (responses-standard-instructions
               (append instruction-prefix
                       (and compaction-p instruction-suffix))))
             (otherwise
              (error 'configuration-error
                     :message
                     (format nil
                             "Provider ~S returned unsupported Responses instruction placement ~S."
                             (class-name (class-of provider))
                             instruction-placement)))))
         (input-prefix
           (when (eq instruction-placement ':input)
             (mapcar #'responses-developer-message
                     (remove-if-not #'non-empty-string-p instruction-prefix))))
         (input-suffix
           (when (or (eq instruction-placement ':input)
                     (and (eq instruction-placement ':top-level)
                          (not compaction-p)))
             (mapcar #'responses-developer-message
                     (remove-if-not #'non-empty-string-p instruction-suffix))))
         (input
           (coerce
            (append
             input-prefix
             (mapcar
              (lambda (item)
                (provider-wire-input-item provider item))
              (conversation-input-items-for-family
               conversation
               (provider-family provider)
               :include-ephemeral-p (not compaction-p)))
             input-suffix)
            'vector))
         (tools (provider-wire-tools provider effective-namespaces)))
    (values
     (apply #'json-object
            (append
             (list "model" (configuration-model configuration))
             (when top-level-instructions
               (list "instructions" top-level-instructions))
             (list "input" input
                   "tools" tools)
             (when (plusp (length tools))
               (list "tool_choice" "auto"))
             (list "parallel_tool_calls" false)
             ;; A NIL wire effort means the serving stack rejects the
             ;; reasoning parameter entirely; omit the reasoning object.
             (let ((effort
                     (provider-responses-wire-effort provider configuration))
                   (summary
                     (and (not compaction-p)
                          (provider-responses-reasoning-summary
                           provider configuration))))
               (when effort
                 (list "reasoning"
                       (apply #'json-object
                              (append
                               (list "effort" effort)
                               (when summary
                                 (list "summary" summary)))))))
             (when (and *provider-maximum-output-tokens*
                        (provider-output-ceiling-p provider))
               (list "max_output_tokens" *provider-maximum-output-tokens*))
             (list "store" false
                   "stream" t)
             (provider-responses-request-fields
              provider conversation :compaction-p compaction-p)))
     delivery)))
