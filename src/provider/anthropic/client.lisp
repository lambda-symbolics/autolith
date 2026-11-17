(in-package #:autolith)

;;;; -- Anthropic Account Adapter --

(defclass anthropic-product-provider-mixin ()
  ()
  (:documentation "Autolith history and context projection for Messages transports."))

(defclass anthropic-api-key-provider
    (anthropic-product-provider-mixin session-preserving-provider-mixin
     subscription-provider cl-llm-provider-api:anthropic-messages-provider)
  ()
  (:documentation "A static API key client for the Anthropic Messages API."))

(defmethod provider-prompt-cache-retention-estimate
    ((provider anthropic-api-key-provider))
  "Return the cache lifetime used to schedule idle compaction."
  (provider-prompt-cache-lifetime provider))

(defmethod provider-prompt-cache-lifetime ((provider anthropic-api-key-provider))
  "Return the requested cache lifetime in seconds."
  (let ((ttl (config :prompt-cache-ttl (provider-configuration provider))))
    (if (stringp ttl)
        (cond
          ((string= ttl "5m")
           300)
          ((string= ttl "1h")
           3600)
          (t
           nil))
        nil)))

(defmethod provider-account-label ((provider anthropic-api-key-provider))
  "Return the Anthropic service name for user-visible errors."
  (declare (ignore provider))
  "Anthropic")

(defmethod provider-family ((provider anthropic-api-key-provider))
  "Return the Anthropic model family."
  (declare (ignore provider))
  ':anthropic)

(defmethod provider-family-create
    ((family (eql ':anthropic))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the Anthropic API key provider."
  (declare (ignore reasoning-summaries-p))
  (anthropic-provider-create configuration))

(-> anthropic-provider-create (configuration) anthropic-api-key-provider)
(defun anthropic-provider-create (configuration)
  "Create the Anthropic API key provider for CONFIGURATION."
  (make-instance 'anthropic-api-key-provider
                 :configuration      configuration
                 :credential-manager (anthropic-credential-manager-create
                                      configuration)
                 :session-id         (make-identifier)))

(defparameter *anthropic-model-prices*
  '(("claude-fable-5-2"  8  1/5  40)
    ("claude-fable-5-1"  10 1/4  50)
    ("claude-fable-5"    10 1    50)
    ("claude-opus-5-5"   4  1/5  20)
    ("claude-opus-5"     5  1/2  25)
    ("claude-opus-4-8"   5  1/2  25)
    ("claude-opus-4-7"   5  1/2  25)
    ("claude-opus-4-6"   5  1/2  25)
    ("claude-sonnet-5-5" 2  1/10 10)
    ("claude-sonnet-5"   2  1/5  10)
    ("claude-sonnet-4-6" 3  3/10 15)
    ("claude-haiku-4-5"  1  1/10 5))
  "Anthropic API list prices as (MODEL INPUT CACHE-READ OUTPUT), US dollars per million tokens.

Taken from Anthropic's published prices as of 2026-11-15. Haiku 5.5 uses
prompt-length tiers selected from normalized request usage.")

(defmethod provider-model-price
    ((provider anthropic-api-key-provider) (configuration configuration) &key usage)
  "Use configured prices, else list prices at USAGE's prompt length and cache lifetime.

A cache write costs 1.25 times input for five minutes and twice input for one hour."
  (or (call-next-method)
      (let* ((model        (config :model configuration))
             (input-tokens (and (json-object-p usage) (json-get usage "input_tokens")))
             (entry
               (if (string= model "claude-haiku-5-5")
                   (when (typep input-tokens '(integer 0))
                     (if (<= input-tokens 100000)
                         (list model 1/10 1/100 1/2)
                         (list model 1/2 1/20 5/2)))
                   (assoc model *anthropic-model-prices* :test #'string=))))
        (when entry
          (destructuring-bind (model input cache-read output) entry
            (declare (ignore model))
            (make-model-price :input       input
                              :cache-read  cache-read
                              :cache-write (* input (if (provider-prompt-cache-ttl configuration)
                                                        2
                                                        5/4))
                              :output      output
                              :source      "Anthropic list prices, 2026-11-15"))))))

(defmethod provider-authenticate ((provider anthropic-api-key-provider)
                                  &key stream open-browser-p)
  "Prompt for, validate, and save the Anthropic API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


;;;; -- Anthropic Product Request Projection --

(defmethod provider-request-object
    ((provider anthropic-product-provider-mixin) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Project product prompts and history into the shared Anthropic wire request.
Return the encoded request and its unconsumed context delivery. A compaction
request keeps anchored context in its history, ends with the compaction
instructions and delivers no context."
  (let* ((configuration   (provider-configuration provider))
         (request-namespaces
           (provider-request-tool-namespaces configuration tool-namespaces))
         (effective-tools (provider-wire-tools provider request-namespaces))
         (delivery
           (context-resolve-request configuration conversation request-namespaces
                                    :goal-context goal-context
                                    :compaction-p compaction-p))
         (durable-items
           (conversation-input-items-for-family
            conversation (provider-family provider) :include-ephemeral-p nil))
         (ephemeral-items
           (unless compaction-p
             (remove-if
              (lambda (item) (member item durable-items :test #'eq))
              (conversation-input-items-for-family
               conversation (provider-family provider) :include-ephemeral-p t))))
         (projection
           ;; Anchored context joins the durable history before its cache
           ;; marker; the goal and the rest of the context follow it.
           (multiple-value-bind (shared items)
               (provider-shared-items-split
                (context-delivery-project-items delivery conversation durable-items)
                durable-items)
             (make-instance
              'cl-llm-provider-api:wire-request
              :model   (config :model configuration)
              :items   items
              :prefix  (list (let ((*system-prompt-hosted-web-search-p* nil))
                               (system-prompt configuration)))
              :suffix  (remove-if-not
                        #'non-empty-string-p
                        (if compaction-p
                            (list *compaction-instructions*)
                            (list goal-context (context-delivery-rendered-tail delivery))))
              :options (list :shared-items          shared
                             :ephemeral-items       ephemeral-items
                             ;; Forced tools reject thinking on current models and
                             ;; change the ordinary message-prefix cache key.
                             :tool-choice           (unless compaction-p *provider-tool-choice*)
                             :maximum-output-tokens *provider-maximum-output-tokens*
                             :cache-ttl             (provider-prompt-cache-ttl
                                                     configuration))))))
    (let ((request (provider-request-object provider projection effective-tools)))
      (anthropic--configure-effort request (config :reasoning-effort configuration))
      (anthropic--apply-binding-policy request conversation)
      (anthropic--preserve-tool-prefix
       request conversation :history durable-items
       :eligible-p                   (and (not compaction-p) (null ephemeral-items))
       :context                      (vector goal-context (and delivery
                                                               (context--render (context-delivery-contributions delivery))))
       :rebuild                      (lambda ()
                                       (let ((*provider-context-request*       nil)
                                             (*provider-cache-prefix-function* nil))
                                         (provider-request-object
                                          provider
                                          (make-instance 'cl-llm-provider-api:wire-request
                                                         :model   (cl-llm-provider-api:wire-request-model projection)
                                                         :items   (cl-llm-provider-api:wire-request-items projection)
                                                         :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                                         :options (cl-llm-provider-api:wire-request-options projection))
                                          effective-tools))))
      (values request (and (not compaction-p) delivery)))))


;;;; -- Dispatched Message Prefixes --

(-> anthropic--unmarked-messages (vector) vector)
(defun anthropic--unmarked-messages (messages)
  "Copy MESSAGES without cache placement metadata for chronological comparison."
  (let ((copy (json-decode (json-encode messages))))
    (loop for message across copy
          do (loop for block across (json-get message "content")
                   do (remhash "cache_control" block)))
    copy))

(-> anthropic--appended-messages (vector vector) (values vector boolean))
(defun anthropic--appended-messages (messages prefix)
  "Return only MESSAGES appended to unmarked PREFIX, plus whether it matches.

A new item may extend the final same-role message. Return those new blocks as
an independent message, without mutating a previously dispatched message."
  (let* ((plain (anthropic--unmarked-messages messages))
         (count (length prefix)))
    (cond
      ((zerop count)
       (values messages t))
      ((or (< (length plain) count)
           (not (loop for index below (1- count)
                      always (equalp (aref plain index) (aref prefix index)))))
       (values #() nil))
      (t
       (let* ((previous (aref prefix (1- count)))
              (current  (aref plain (1- count)))
              (before   (json-get previous "content"))
              (after    (json-get current "content")))
         (if (and (equal (json-get previous "role") (json-get current "role"))
                  (<= (length before) (length after))
                  (equalp before (subseq after 0 (length before))))
             (values
              (concatenate
               'vector
               (when (< (length before) (length after))
                 (vector (json-object "role" (json-get current "role")
                                      "content" (subseq (json-get (aref messages (1- count)) "content")
                                                        (length before)))))
               (subseq messages count))
              t)
             (values #() nil)))))))

(-> anthropic--bound-cache-markers (json-object) null)
(defun anthropic--bound-cache-markers (request)
  "Keep at most four explicit breakpoints, favoring the newest message boundaries."
  (let ((remaining
          (- 4 (loop for key in '("system" "tools")
                     sum (count-if (lambda (block) (json-get block "cache_control"))
                                   (or (json-get request key) #()))))))
    (loop for message across (reverse (json-get request "messages"))
          do (loop for block across (reverse (json-get message "content"))
                   when (json-get block "cache_control")
                     do (if (plusp remaining)
                            (decf remaining)
                            (remhash "cache_control" block)))))
  nil)

(-> anthropic--preserve-tool-prefix
    (json-object conversation &key (:history list) (:context t)
                                  (:eligible-p boolean) (:rebuild function))
    json-object)
(defun anthropic--preserve-tool-prefix (request conversation &key history context eligible-p rebuild)
  "Preserve dispatched messages across append-only history and context updates.

Render new durable blocks and changed request-local context after the dispatched
prefix. Changed headers, rewritten history, compaction and ephemeral exchanges
start a fresh projection. Only successful dispatch promotes a candidate; prompt
text is never persisted. Cache markers may move within the four-breakpoint budget."
  (let* ((state       (conversation-anthropic-request-state conversation))
         (prior       (getf state :prefix))
         (digests     (mapcar #'context-anchors-item-digest history))
         (context-key (context-anchors-item-digest context))
         (header      (context-anchors-item-digest
                       (map 'vector (lambda (key) (json-get request key))
                            '("model" "system" "tools" "tool_choice" "thinking" "output_config"))))
         (before      (getf prior :history))
         (base        (when eligible-p (json-get (funcall rebuild) "messages")))
         (canonical   (when base (anthropic--unmarked-messages base)))
         (compatible-p
           (and eligible-p prior (<= (length before) (length history))
                (equal before (subseq digests 0 (length before)))
                (equal header (getf prior :header)))))
    (when compatible-p
      (multiple-value-bind (tail matches-p)
          (anthropic--appended-messages base (getf prior :canonical))
        (multiple-value-bind (suffix suffix-matches-p)
            (anthropic--appended-messages (json-get request "messages") canonical)
          (setf compatible-p (and matches-p suffix-matches-p))
          (when compatible-p
            (setf (gethash "messages" request)
                  (concatenate 'vector
                               (json-decode (json-encode (getf prior :messages)))
                               tail
                               (unless (equal context-key (getf prior :context)) suffix)))))))
    (anthropic--bound-cache-markers request)
    (setf (conversation-anthropic-request-state conversation)
          (when eligible-p
            (list :prefix (and compatible-p prior)
                  :candidate
                  (list :request request :history digests :context context-key :header header
                        :canonical canonical
                        :messages  (json-decode (json-encode (json-get request "messages")))))))
    request))

(-> anthropic--note-dispatched-request (conversation json-object) null)
(defun anthropic--note-dispatched-request (conversation request)
  "Promote the dispatched candidate, allowing only admission's output-ceiling change."
  (let* ((candidate (getf (conversation-anthropic-request-state conversation) :candidate))
         (built     (getf candidate :request)))
    (when (and built
               (equalp (getf candidate :messages) (json-get request "messages"))
               (let ((original   (json-object-copy built))
                     (dispatched (json-object-copy request)))
                 (remhash "max_tokens" original)
                 (remhash "max_tokens" dispatched)
                 (equalp original dispatched)))
      (remf candidate :request)
      (setf (conversation-anthropic-request-state conversation) (list :prefix candidate))))
  nil)


;;;; -- Scoped Thinking Binding Recovery --

(-> anthropic--adaptive-model-p (t) boolean)
(defun anthropic--adaptive-model-p (model)
  "Recognize the direct models with documented default adaptive thinking."
  (not (null (member model '("claude-fable-5-2" "claude-opus-5-5"
                             "claude-sonnet-5-5" "claude-haiku-5-5") :test #'equal))))

(-> anthropic--configure-effort (json-object t) null)
(defun anthropic--configure-effort (request effort)
  "Apply supported configured EFFORT to current direct models, without overriding a mode."
  (when (and (anthropic--adaptive-model-p (json-get request "model"))
             (member effort '("low" "medium" "high" "xhigh" "max") :test #'equal)
             (or (null (json-get request "thinking"))
                 (anthropic--binding-supported-p request)))
    (unless (json-get request "thinking")
      (setf (gethash "thinking" request) (json-object "type" "adaptive")))
    (let ((output (or (json-get request "output_config") (json-object))))
      (setf (gethash "effort" output) effort
            (gethash "output_config" request) output)))
  nil)

(-> anthropic--thinking-signature-digest (t) (option string))
(defun anthropic--thinking-signature-digest (block)
  "Return a digest of BLOCK's signed thinking identity, without retaining its text."
  (when (and (json-object-p block) (equal (json-get block "type") "thinking")
             (non-empty-string-p (json-get block "signature")))
    (context-anchors--digest (json-get block "signature"))))

(-> anthropic--request-signatures (json-object) list)
(defun anthropic--request-signatures (request)
  "Return deduplicated signed thinking identities present in REQUEST."
  (remove-duplicates
   (loop for message across (or (json-get request "messages") #())
         append (let ((content (json-get message "content")))
                  (when (and (vectorp content) (not (stringp content)))
                    (loop for block across content
                          for digest = (anthropic--thinking-signature-digest block)
                          when digest collect digest))))
   :test #'equal))

(-> anthropic--binding-supported-p (json-object) boolean)
(defun anthropic--binding-supported-p (request)
  "Return true only for the thinking modes supporting binding controls."
  (let ((thinking (json-get request "thinking")))
    (if (json-object-p thinking)
        (not (null (member (json-get thinking "type") '("enabled" "adaptive")
                           :test #'equal)))
        (and (null thinking) (anthropic--adaptive-model-p (json-get request "model"))))))

(-> anthropic--apply-binding-policy (json-object conversation) null)
(defun anthropic--apply-binding-policy (request conversation)
  "Allow server-side mismatch removal only while a specifically rejected block is sent."
  (when (and (anthropic--binding-supported-p request)
             (intersection (anthropic--request-signatures request)
                           (conversation-anthropic-rejected-thinking conversation)
                           :test #'equal))
    (unless (json-get request "thinking")
      (setf (gethash "thinking" request) (json-object "type" "adaptive")))
    (setf (gethash "block_binding" (json-get request "thinking"))
          (json-object "prefix_mismatch_behavior" "drop_block")))
  nil)

(defmethod conversation--project-record
    ((kind (eql :anthropic-thinking-binding)) conversation properties)
  "Restore a bounded set of specifically rejected signature digests."
  (let ((digests (getf properties :signatures)))
    (when (and (proper-list-p digests) (<= (length digests) 32)
               (every (lambda (digest)
                        (and (stringp digest) (= (length digest) 64)
                             (every (lambda (character) (digit-char-p character 16)) digest)))
                      digests))
      (setf (conversation-anthropic-rejected-thinking conversation)
            (remove-duplicates digests :test #'equal)))))

(-> anthropic--note-binding-rejection (conversation string) null)
(defun anthropic--note-binding-rejection (conversation digest)
  "Persist DIGEST once, retaining at most the newest 32 rejected identities."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (unless (member digest (conversation-anthropic-rejected-thinking conversation) :test #'equal)
      (let* ((digests (cons digest (conversation-anthropic-rejected-thinking conversation)))
             (record  (list :anthropic-thinking-binding
                            :signatures (subseq digests 0 (min 32 (length digests))))))
        (conversation-append-record conversation record)
        (conversation--project-record (first record) conversation (rest record)))))
  nil)

(-> anthropic--binding-rejected-signature (provider-error json-object) (option string))
(defun anthropic--binding-rejected-signature (condition request)
  "Identify only the indexed thinking block in a documented prefix-binding HTTP 400."
  (let ((message (cl-llm-provider-api:provider-api-error-message condition)))
    (when (and (eql (provider-error-status condition) 400)
               (stringp message)
               (search "signature" message :test #'char-equal)
               (search "thinking.block_binding.prefix_mismatch_behavior" message)
               (search "thinking-binding-controls-2026-08-01" message))
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings "messages\\.([0-9]+)\\.content\\.([0-9]+)" message)
        (when match
          (let* ((messages    (json-get request "messages"))
                 (index       (parse-integer (aref groups 0)))
                 (content     (and (vectorp messages) (< index (length messages))
                                   (json-get (aref messages index) "content")))
                 (block-index (parse-integer (aref groups 1))))
            (when (and (vectorp content) (not (stringp content))
                       (< block-index (length content)))
              (anthropic--thinking-signature-digest (aref content block-index)))))))))


;;;; -- Anthropic Transport --

(-> anthropic--request-headers (oauth-credentials &optional json-object) list)
(defun anthropic--request-headers (credentials &optional request)
  "Return authenticated headers and the beta required by a scoped binding control."
  (append
   (list (cons "x-api-key" (oauth-credentials-access-token credentials))
         (cons "anthropic-version" *anthropic-api-version*)
         (cons "Content-Type" "application/json")
         (cons "Accept" "text/event-stream")
         (cons "User-Agent" (provider-user-agent)))
   (when (and request (json-object-p (json-get request "thinking"))
              (json-get (json-get request "thinking") "block_binding"))
     (list (cons "anthropic-beta" "thinking-binding-controls-2026-08-01")))))

(defmethod provider-open-response-stream
    ((provider anthropic-api-key-provider) (request hash-table) &key credentials conversation)
  "Open an SSE request, recovering once from an identified thinking-prefix mismatch."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (labels ((send ()
             "Normalize transport failures before applying the narrow recovery policy."
             (handler-case
                 (provider-post-event-stream
                  (config :provider-endpoint (provider-configuration provider))
                  (json-encode-utf8 request)
                  :headers (anthropic--request-headers credentials request))
               (http-request-failed (condition)
                 (provider-signal-transport-failure provider condition)))))
    (multiple-value-prog1
        (handler-case (send)
          (provider-error (condition)
            (let ((digest (anthropic--binding-rejected-signature condition request)))
              (unless (and digest (anthropic--binding-supported-p request)
                           (or (null (json-get request "thinking"))
                               (not (json-get (json-get request "thinking") "block_binding"))))
                (error condition))
              (anthropic--note-binding-rejection conversation digest)
              (anthropic--apply-binding-policy request conversation)
              (send))))
      (anthropic--note-dispatched-request conversation request))))
