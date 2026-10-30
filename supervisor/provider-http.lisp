;;;; -- Shared ChatGPT HTTP Transport --

(in-package #:autolith-supervisor)

(declaim (ftype (function (cl-rfc8628:oauth-credentials &key (:session-id string)
                           (:turn-state (or null string)) (:user-agent string) (:accept string)) list)
                chatgpt-request-headers))

(defun chatgpt-request-headers (credentials &key session-id turn-state user-agent
                                               (accept "text/event-stream"))
  "Build authenticated headers from explicit trusted session and turn metadata."
  (append
   (list (cons "Authorization" (format nil "Bearer ~A" (cl-rfc8628:oauth-credentials-access-token credentials)))
         (cons "ChatGPT-Account-ID" (cl-rfc8628:oauth-credentials-account-id credentials))
         (cons "Content-Type" "application/json") (cons "Accept" accept)
         (cons "originator" "autolith") (cons "User-Agent" user-agent)
         (cons "session-id" session-id) (cons "thread-id" session-id)
         (cons "x-client-request-id" session-id))
   (when turn-state (list (cons "x-codex-turn-state" turn-state)))))

(declaim (ftype (function (string hash-table cl-rfc8628:oauth-credentials
                           &key (:session-id string) (:turn-state (or null string))
                           (:user-agent string) (:deadline-seconds real) (:keep-alive-p boolean))
                         (values t t t &optional)) chatgpt-open-response-stream))

(defun chatgpt-open-response-stream (endpoint request credentials
                                     &key session-id turn-state user-agent
                                          (deadline-seconds 300) keep-alive-p)
  "Open the library HTTP transport with trusted endpoint and explicit request metadata.
The parent leaves KEEP-ALIVE-P false so strict cleanup closes the actual connection.
The legacy client can enable pooling to retain its conversation affinity."
  (cl-llm-provider-api:provider-post-event-stream
   endpoint (cl-llm-provider-api::json-encode request)
   :headers (chatgpt-request-headers credentials :session-id session-id :turn-state turn-state
                                               :user-agent user-agent)
   :keep-alive-p keep-alive-p :deadline-seconds deadline-seconds
   :connect-timeout (min 30 deadline-seconds) :read-timeout deadline-seconds))
