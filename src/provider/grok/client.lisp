(in-package #:autolith)

;;;; -- Grok Subscription Provider --

;;; The Grok subscription proxy speaks the standard streaming Responses API,
;;; as read from grok-build reference commit 5163763e. Tools ride in the
;;; request's flat tools array and function calls return one flat wire name, so
;;; this provider joins Autolith's namespaced tool names with a dot on the way
;;; out and splits them again on completed items. Conversations persist in the
;;; same namespaced shape regardless of the serving provider.
;;;
;;; Grok generation loops are stochastic, so the proxy offers a server-side
;;; detector: the x-grok-doom-loop-check request header opts in, and the
;;; stream then reports cumulative trigger labels through non-standard
;;; response.doom_loop_check events. Confident thinking-channel triggers
;;; abandon the looping stream for an immediate resample within a bounded
;;; per-turn budget; once the budget is spent the response is accepted as-is.

(defparameter *grok-doom-loop-window-tokens* 1024
  "The doom-loop detector window sent as the opt-in header value.")

(defparameter *grok-doom-loop-maximum-tail-threshold* 64
  "The loosest thinking-channel tail-repetition threshold treated as a loop.")

(defparameter *grok-doom-loop-resample-limit* 2
  "The per-turn resample budget before a looping response is accepted.")

(defvar *grok-doom-loop-resamples-remaining* nil
  "The armed per-turn resample budget, or NIL outside a Grok streaming turn.")

(defclass grok-subscription-provider
    (session-preserving-provider-mixin responses-api-provider)
  ()
  (:documentation "A direct Grok subscription client for the xAI Responses proxy."))

(defmethod provider-account-label ((provider grok-subscription-provider))
  "Name the Grok account service in user-visible failures."
  (declare (ignore provider))
  "Grok")

(defmethod provider-family ((provider grok-subscription-provider))
  "The Grok provider serves the Grok model family."
  (declare (ignore provider))
  ':grok)

(defmethod provider-family-create
    ((family (eql ':grok))
     (configuration configuration)
     &key reasoning-summaries-p)
  "Create the direct Grok subscription provider, which has no summary switch."
  (declare (ignore reasoning-summaries-p))
  (grok-provider-create configuration))

(defmethod provider-device-authentication-client
    ((provider grok-subscription-provider))
  "Return the Grok device authentication client."
  (declare (ignore provider))
  (grok-device-authentication-client-create))

(-> grok-provider-create (configuration) grok-subscription-provider)
(defun grok-provider-create (configuration)
  "Create the direct Grok subscription provider for CONFIGURATION."
  (make-instance 'grok-subscription-provider
                 :configuration configuration
                 :credential-manager (grok-credential-manager-create
                                      configuration)
                 :session-id (make-identifier)))


;;;; -- Grok Protocol Specializations --

(defmethod provider-responses-wire-effort
    ((provider grok-subscription-provider) (configuration configuration))
  "Return CONFIGURATION's Grok reasoning effort."
  (declare (ignore provider))
  (configuration-grok-wire-effort configuration))

(defmethod provider-responses-hosted-tools
    ((provider grok-subscription-provider) (configuration configuration))
  "Return Grok's backend web_search and x_search tools unless disabled.

The Grok proxy executes search server-side. Grok Build reference commit
5163763e splices these bare entries into the request tools array whenever
backend search is enabled."
  (declare (ignore provider))
  (unless (string= (configuration-web-search-mode configuration) "disabled")
    (list (json-object "type" "web_search")
          (json-object "type" "x_search"))))

(defmethod provider-responses-request-namespaces
    ((provider grok-subscription-provider) (tool-namespaces vector))
  "Exclude the local web.run tool, which Grok serves through backend search.

The standalone search endpoint behind web.run no longer exists on the Grok
proxy, so the local tool could only fail. Independent web namespace tools
such as web.gist page retrieval stay advertised."
  (declare (ignore provider))
  (coerce
   (loop for entry across tool-namespaces
         for filtered = (provider-request--without-web-run entry)
         when filtered
           collect filtered)
   'vector))

(defmethod provider-responses-reasoning-summary
    ((provider grok-subscription-provider) (configuration configuration))
  "Request concise reasoning summaries, matching Grok Build's dialect."
  (declare (ignore provider configuration))
  "concise")

(defmethod provider-responses-request-fields
    ((provider grok-subscription-provider)
     (conversation conversation)
     &key compaction-p)
  "Add Grok's encrypted reasoning, citation, and prompt-cache request fields.

The no_inline_citations include suppresses inline citation markup during
backend search, and the conversation identifier doubles as the prompt cache
key, both matching Grok Build reference commit 5163763e."
  (declare (ignore compaction-p))
  (let ((configuration (provider-configuration provider))
        (cache-key (conversation-prompt-cache-key conversation)))
    (append
     (list "include"
           (if (string= (configuration-web-search-mode configuration)
                        "disabled")
               (json-array "reasoning.encrypted_content")
               (json-array "reasoning.encrypted_content"
                           "no_inline_citations")))
     (when (non-empty-string-p cache-key)
       (list "prompt_cache_key" cache-key)))))

(defmethod provider-normalize-output-item
    ((provider grok-subscription-provider) (item hash-table))
  "Keep replayable server state and drop output-only status before replay.

Grok Build reference commit 5163763e replays backend search calls verbatim,
replays reasoning items with their server identifiers and encrypted content
while omitting status, and replays function calls through call_id alone."
  (cond
    ((backend-search-call-item-p item)
     item)
    ((reasoning-item-p item)
     (remhash "status" item)
     item)
    (t
     (remhash "status" item)
     (call-next-method))))


;;;; -- Grok Doom-Loop Recovery --

(-> grok--doom-loop-confident-trigger-p (string) boolean)
(defun grok--doom-loop-confident-trigger-p (trigger)
  "Return true for a tight thinking-channel tail-repetition TRIGGER label.

Trigger labels follow the grammar tail_repetition:{threshold}@{channel} or
low_logprob@{channel}. Only thinking-channel tail repetition at or below the
confidence threshold is worth acting on; loops in visible output are the
user's to judge."
  (let* ((at (position #\@ trigger))
         (head (if at (subseq trigger 0 at) trigger))
         (channel (if at (subseq trigger (1+ at)) ""))
         (colon (position #\: head)))
    (not
     (null
      (and (string= channel "thinking")
           colon
           (string= (subseq head 0 colon) "tail_repetition")
           (let ((threshold
                   (parse-integer head :start (1+ colon) :junk-allowed t)))
             (and threshold
                  (<= threshold *grok-doom-loop-maximum-tail-threshold*))))))))

(-> grok--doom-loop-confident-triggers (json-object) list)
(defun grok--doom-loop-confident-triggers (event)
  "Return EVENT's confident trigger labels from its cumulative report."
  (let* ((check (json-get event "doom_loop_check"))
         (triggers (and (json-object-p check)
                        (json-get check "triggers"))))
    (when (vectorp triggers)
      (loop for trigger across triggers
            when (and (stringp trigger)
                      (grok--doom-loop-confident-trigger-p trigger))
              collect trigger))))

(defmethod provider-note-doom-loop-event
    ((provider grok-subscription-provider) (event hash-table))
  "Abandon a confidently looping stream while resample budget remains."
  (declare (ignore provider))
  (let ((triggers (grok--doom-loop-confident-triggers event))
        (remaining *grok-doom-loop-resamples-remaining*))
    (when (and triggers
               (integerp remaining)
               (plusp remaining))
      (setf *grok-doom-loop-resamples-remaining* (1- remaining))
      (error 'provider-resample-requested
             :message (format nil
                              "Grok reported a reasoning loop (~{~A~^, ~}); resampling."
                              triggers)
             :status nil
             :request-id nil
             :response nil
             :triggers triggers
             :attempt (1+ (- *grok-doom-loop-resample-limit* remaining))
             :maximum-attempts *grok-doom-loop-resample-limit*)))
  nil)

(defmethod provider-stream-turn :around
    ((provider grok-subscription-provider)
     (conversation conversation)
     &key &allow-other-keys)
  "Arm Grok's per-turn doom-loop resample budget around one streamed turn."
  (let ((*grok-doom-loop-resamples-remaining* *grok-doom-loop-resample-limit*))
    (call-next-method)))

(-> grok--empty-response-p (provider-result) boolean)
(defun grok--empty-response-p (result)
  "Return true when RESULT carries no assistant message and no tool call."
  (not
   (null
    (and (null (provider-result-tool-calls result))
         (notany (lambda (item)
                   (and (json-object-p item)
                        (json-string= (json-get item "type") "message")))
                 (provider-result-output-items result))))))

(defmethod provider-attempt-turn :around
    ((provider grok-subscription-provider)
     (conversation conversation)
     &key compaction-p &allow-other-keys)
  "Retry a Grok response that completed without any visible output.

Grok Build treats a reasoning-only or truncated empty completion as a
transient failure, so an empty result becomes a bounded retryable error
instead of an empty assistant turn."
  (let ((result (call-next-method)))
    (when (and (not compaction-p)
               (grok--empty-response-p result))
      (error 'provider-retryable-error
             :message "Grok completed a response with no message or tool call."
             :status nil
             :request-id nil
             :response-id (provider-result-response-id result)
             :response nil))
    result))


;;;; -- Grok Transport --

(-> grok--request-headers
    (grok-subscription-provider oauth-credentials conversation
     &key (:accept string))
    list)
(defun grok--request-headers (provider credentials conversation &key accept)
  "Return authenticated Grok headers for one request to CONVERSATION."
  (let ((configuration (provider-configuration provider)))
    (list
     (cons "Authorization"
           (format nil "Bearer ~A"
                   (oauth-credentials-access-token credentials)))
     (cons "X-XAI-Token-Auth" "xai-grok-cli")
     (cons "x-authenticateresponse" "authenticate-response")
     (cons "Content-Type" "application/json")
     (cons "Accept" accept)
     (cons "User-Agent" (provider-user-agent))
     (cons "x-grok-client-version" *grok-client-protocol-version*)
     (cons "x-grok-client-mode" "interactive")
     (cons "x-grok-client-identifier" "autolith")
     (cons "x-grok-session-id" (provider-session-id provider))
     (cons "x-grok-conv-id" (conversation-identifier conversation))
     (cons "x-grok-req-id" (make-identifier))
     (cons "x-grok-model-override"
           (configuration-model configuration))
     (cons "x-grok-doom-loop-check"
           (format nil "~D" *grok-doom-loop-window-tokens*)))))

(defmethod provider-open-response-stream
    ((provider grok-subscription-provider)
     (request hash-table)
     &key credentials conversation)
  "Open a direct authenticated SSE request to the Grok CLI chat proxy."
  (declare (type oauth-credentials credentials)
           (type conversation conversation))
  (let ((configuration (provider-configuration provider)))
    (dexador:post
     (configuration-provider-endpoint configuration)
     :headers (grok--request-headers
               provider credentials conversation :accept "text/event-stream")
     :content (json-encode-utf8 request)
     :want-stream t
     :force-string t
     :keep-alive nil
     :connect-timeout 30
     :read-timeout 300)))
