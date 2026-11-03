;;;; -- Parent Family Wire Policies --

(in-package #:autolith-supervisor)

(defclass family-wire-policy ()
  ((account :initarg :account :reader family-wire-policy-account
            :documentation "The trusted account whose dialect this adapter implements."))
  (:documentation "Account-specific policy over the shared wire implementation."))

(defclass family-responses-provider (family-wire-policy cl-llm-provider-api:responses-api-provider) ()
  (:documentation "Responses dialect selected by a trusted parent account."))

(defclass family-chat-provider (family-wire-policy cl-llm-provider-api:chat-completions-provider) ()
  (:documentation "Chat Completions dialect selected by a trusted parent account."))

(defclass family-messages-provider (family-wire-policy cl-llm-provider-api:anthropic-messages-provider) ()
  (:documentation "Messages dialect selected by a trusted parent account."))

(defclass family-gemini-provider (family-wire-policy cl-llm-provider-api:gemini-generate-content-provider) ()
  (:documentation "Code Assist GenerateContent dialect selected by the parent."))

(defmethod cl-llm-provider-api:provider-account-label ((provider family-wire-policy))
  "Use the fixed account family name in sanitized library errors."
  (string-downcase (provider-account-family (family-wire-policy-account provider))))

(defmethod cl-llm-provider-api:provider-responses-instructions-placement
    ((provider family-responses-provider))
  "Use Codex top-level instructions and ordinary developer input elsewhere."
  (if (eq (provider-account-family (family-wire-policy-account provider)) ':codex)
      ':top-level
      ':input))

(defmethod cl-llm-provider-api:provider-wire-tool-name
    ((provider family-responses-provider) (namespace string) (name string))
  "Use the shared reversible restricted-name codec for public OpenAI and Copilot."
  (if (member (provider-account-family (family-wire-policy-account provider)) '(:openai-api :copilot))
      (cl-llm-provider-api:provider-wire-function-name--encode namespace name)
      (call-next-method)))

(defmethod cl-llm-provider-api:provider-wire-tool
    ((provider family-responses-provider) (namespace string) (tool hash-table))
  "Apply the public API parameter-root policy without changing the Responses envelope."
  (let ((wire (call-next-method)))
    (when (eq (provider-account-family (family-wire-policy-account provider)) ':openai-api)
      (setf (gethash "parameters" wire)
            (cl-llm-provider-api:openai-compatible--wire-parameters (gethash "parameters" tool))))
    wire))

(defmethod cl-llm-provider-api:provider-normalize-output-item
    ((provider family-responses-provider) (item hash-table))
  "Preserve signed replay data and apply only the serving account's output policy."
  (let ((family (provider-account-family (family-wire-policy-account provider))))
    (cond
      ((and (eq family ':grok) (clinker-transcript:backend-search-call-item-p item))
       item)
      ((and (eq family ':grok) (clinker-transcript:reasoning-item-p item))
       (remhash "status" item)
       item)
      (t
        (when (eq family ':grok) (remhash "status" item))
        (call-next-method)
        (when (and (member family '(:openai-api :copilot))
                   (clinker-transcript:function-call-item-p item))
          (multiple-value-bind (namespace name)
              (cl-llm-provider-api:provider-wire-function-name--decode (gethash "name" item))
            (when (and namespace name)
              (setf (gethash "namespace" item) namespace (gethash "name" item) name))))
        item))))

(defmethod cl-llm-provider-api:provider-gemini-stream-response
    ((provider family-gemini-provider) event)
  "Unwrap Code Assist without duplicating the shared Gemini stream parser."
  (declare (ignore provider))
  (values (gethash "response" event) (gethash "traceId" event)))

(declaim (ftype (function (provider-account) cl-llm-provider-api:model-provider)
                provider-family--wire-provider))

(defun provider-family--wire-provider (account)
  "Create only the library adapter selected by trusted account data."
  (make-instance (ecase (provider-account-protocol account)
                   (:responses
                    'family-responses-provider)
                   (:chat-completions
                    'family-chat-provider)
                   (:anthropic
                    'family-messages-provider)
                   (:gemini
                    'family-gemini-provider))
                 :account account))

(declaim (ftype (function (string string) string) provider-family--strip-model-prefix))

(defun provider-family--strip-model-prefix (model prefix)
  "Remove a required public namespace at the provider boundary."
  (unless (and (uiop:string-prefix-p prefix model) (< (length prefix) (length model)))
    (error 'provider-executor-error :reason ':invalid-model-namespace))
  (subseq model (length prefix)))

(declaim (ftype (function (provider-account cl-llm-provider-api:wire-request boolean)
                         cl-llm-provider-api:wire-request) provider-family--projection))

(defun provider-family--projection (account projection summary-p)
  "Apply account defaults beneath projected request options. Preserve opaque history."
  (let* ((family  (provider-account-family account))
         (policy  (provider-account-options account))
         (model   (cl-llm-provider-api:wire-request-model projection))
         (options (copy-tree (getf policy :wire-options))))
    (loop for (key value) on (cl-llm-provider-api:wire-request-options projection)
          by (lambda (tail) (rest (rest tail)))
          do (setf (getf options key) value))
    (case family
      (:fireworks
       (when (string= model "accounts/fireworks/models/qwen3p7-plus")
         (remf options :reasoning-effort)))
      (:openrouter
       (setf model (provider-family--strip-model-prefix model "openrouter/"))
       (setf (getf options :output-ceiling-field) "max_tokens")
       (when (getf policy :reasoning-supported-p t)
         (let ((effort (getf options :reasoning-effort)))
           (when (and effort (not (equal effort "none")))
             (setf (getf options :reasoning-object)
                   (cl-rfc8628:json-object "effort"
                                           (if (equal effort "ultra")
                                               "max"
                                               effort))))))
       (unless (getf policy :reasoning-supported-p t) (remf options :reasoning-object))
       (when (getf policy :provider-routing)
         (setf (getf options :provider-preferences) (getf policy :provider-routing))))
      (:opencode
       (setf model (provider-family--strip-model-prefix model "opencode/")))
      (:copilot
       (setf model (provider-family--strip-model-prefix model "copilot/")))
      (:mistral
       (setf (getf options :output-ceiling-field) "max_tokens"))
      (:gemini
       (setf model (family-domain-gemini-model-name model))))
    (when
        (and (eq family ':openrouter)
             (some (lambda (prefix) (uiop/utility:string-prefix-p prefix model))
                   '("anthropic/" "~anthropic/" "openai/")))
      (setf (getf options :cache-breakpoint-fields)
            (list "cache_control" (cl-rfc8628:json-object "type" "ephemeral"))))
    (when summary-p
      (setf (getf options :cache-p) nil)
      (remf options :cache-breakpoint-fields)
      (remf options :ephemeral-items)
      (remf options :reasoning-summary))
    (make-instance 'cl-llm-provider-api:wire-request :model model :items
                   (cl-llm-provider-api:wire-request-items projection) :prefix
                   (cl-llm-provider-api:wire-request-prefix projection) :suffix
                   (cl-llm-provider-api:wire-request-suffix projection) :options options)))(declaim
                                                                                            (ftype
                                                                                             (function
                                                                                              (provider-account &key (:provider cl-llm-provider-api:model-provider)
                                                                                               (:projection cl-llm-provider-api:wire-request) (:tools vector) (:summary-p boolean)
                                                                                               (:session-id string) (:continuation t))
                                                                                              hash-table)
                                                                                             provider-family--request))

(defun provider-family--request
       (account &key provider projection tools summary-p session-id continuation)
  "Encode through the one shared family encoder, then add trusted service fields."
  (when (eq (provider-account-family account) ':grok)
    (setf tools (provider-family--grok-tools tools)))
  (let* ((projection (provider-family--projection account projection summary-p))
         (family     (provider-account-family account))
         (protocol   (provider-account-protocol account))
         (effective-tools
          (if (member protocol '(:anthropic :chat-completions))
              (cl-llm-provider-api:provider-wire-tools provider tools)
              tools))
         (request
          (cl-llm-provider-api:provider-request-object provider projection effective-tools
                                                       :compaction-p summary-p)))
    (when (eq family ':openrouter)
      (let ((options (cl-llm-provider-api:wire-request-options projection)))
        (when (getf options :reasoning-object)
          (setf (gethash "reasoning" request) (getf options :reasoning-object)))
        (when (getf options :provider-preferences)
          (setf (gethash "provider" request) (getf options :provider-preferences)))))
    (when (eq protocol ':responses)
      (when continuation (setf (gethash "previous_response_id" request) continuation))
      (case family
        (:codex
         (unless (gethash "include" request)
           (setf (gethash "include" request) #("reasoning.encrypted_content"))))
        (:grok
         (setf (gethash "include" request)
               (if (getf (provider-account-options account) :hosted-search-p)
                   #("reasoning.encrypted_content" "no_inline_citations")
                   #("reasoning.encrypted_content"))))
        (:openai-api
         (setf (gethash "store" request) yason:false)))
      (when (member family '(:codex :grok :fireworks))
        (setf (gethash "prompt_cache_key" request) session-id)))
    (when
        (and (eq family ':grok) (not summary-p)
             (getf (provider-account-options account) :hosted-search-p t))
      (setf (gethash "tools" request)
            (concatenate 'vector (or (gethash "tools" request) #())
                         (vector (cl-rfc8628:json-object "type" "web_search")
                                 (cl-rfc8628:json-object "type" "x_search")))))
    (when (eq family ':gemini)
      (setf (gethash "session_id" request) session-id)
      (setf request
              (cl-rfc8628:json-object "model" (cl-llm-provider-api:wire-request-model projection)
                                      "project" (getf (provider-account-state account) :project)
                                      "user_prompt_id" (provider-account--identifier) "request"
                                      request)))
    request))

(defvar *provider-family-grok-resamples-remaining*
  nil
  "The invocation-local Grok reasoning loop retry budget.")

(declaim (ftype (function (string) boolean) provider-family--grok-loop-trigger-p))

(defun provider-family--grok-loop-trigger-p (trigger)
  "Recognize confident thinking-channel repetition reports."
  (let* ((at    (position #\@ trigger))
         (head
          (if at
              (subseq trigger 0 at)
              trigger))
         (channel
          (if at
              (subseq trigger (1+ at))
              ""))
         (colon (position #\: head)))
    (not
     (null
      (and (string= channel "thinking") colon (string= (subseq head 0 colon) "tail_repetition")
           (let ((threshold (parse-integer head :start (1+ colon) :junk-allowed t)))
             (and threshold (<= threshold 64))))))))

(defmethod cl-llm-provider-api:provider-note-doom-loop-event
           ((provider family-responses-provider) (event hash-table))
  "Request bounded resampling for confident Grok reasoning loops."
  (when
      (and (eq (provider-account-family (family-wire-policy-account provider)) ':grok)
           *provider-family-grok-resamples-remaining*
           (plusp *provider-family-grok-resamples-remaining*))
    (let* ((report   (gethash "doom_loop_check" event))
           (triggers (and (hash-table-p report) (gethash "triggers" report)))
           (confident
            (when (and (vectorp triggers) (not (stringp triggers)))
              (loop for trigger across triggers
                    when (and (stringp trigger) (provider-family--grok-loop-trigger-p trigger))
                    collect trigger))))
      (when confident
        (decf *provider-family-grok-resamples-remaining*)
        (error 'cl-llm-provider-api:provider-resample-requested :message
               "Grok reported a reasoning loop." :triggers confident :attempt
               (- 2 *provider-family-grok-resamples-remaining*) :maximum-attempts 2))))
  nil)

(declaim (ftype (function (vector) vector) provider-family--grok-tools))

(defun provider-family--grok-tools (tools)
  "Remove the local web.run tool from Grok requests."
  (coerce
   (loop for entry across tools
         for filtered = (if (and (hash-table-p entry) (equal (gethash "type" entry) "namespace")
                                 (equal (gethash "name" entry) "web"))
                            (let ((retained
                                   (remove "run" (coerce (gethash "tools" entry) 'list) :key
                                           (lambda (tool)
                                             (and (hash-table-p tool) (gethash "name" tool)))
                                           :test #'equal)))
                              (when retained
                                (cl-rfc8628:json-object "type" "namespace" "name" "web"
                                                        "description" (gethash "description" entry)
                                                        "tools" (coerce retained 'vector))))
                            entry)
         when filtered
         collect filtered)
   'vector))
