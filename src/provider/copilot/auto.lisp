(in-package #:autolith)

;;;; -- Copilot Auto Routing --

;;; Reference: https://github.com/microsoft/vscode-copilot-chat at
;;; 5863f5a7088958050792b5dccbe8b46c6e13eccc, automodeService.ts and
;;; autoChatEndpoint.ts; https://github.com/CopilotC-Nvim/CopilotChat.nvim at
;;; 004ced055d8db59561cfcddc5f141ccd8d5a033b, config/providers.lua.

(defclass copilot-auto-provider (copilot-chat-provider)
  ((preferred-model
    :initform nil
    :accessor copilot-auto-provider-preferred-model
    :type (option string)
    :documentation "The last successful concrete model ID, without credentials."))
  (:documentation "Resolve Copilot Auto to an entitled wire adapter for each request."))

(defmethod copilot-provider-protocol ((provider copilot-auto-provider))
  "Resolve the Auto pseudomodel before projecting a wire request."
  (declare (ignore provider))
  ':auto)

(defmethod provider-with-configuration
    ((provider copilot-auto-provider) (configuration configuration))
  "Preserve the preferred concrete model when reconfiguring Auto."
  (let ((replacement (call-next-method)))
    (when (typep replacement 'copilot-auto-provider)
      (setf (copilot-auto-provider-preferred-model replacement)
            (copilot-auto-provider-preferred-model provider)))
    replacement))

(-> copilot-auto--session (copilot-auto-provider oauth-credentials) json-object)
(defun copilot-auto--session (provider credentials)
  "Acquire a request-scoped Auto token without retaining secret-bearing failures."
  (handler-case
      (multiple-value-bind (body status)
          (provider-call-with-response-deadline
           30 (lambda ()
                (dexador:request
                 (concatenate 'string (copilot--base-url credentials) "/models/session")
                 :method ':post
                 :headers (append
                           (copilot--headers (oauth-credentials-access-token credentials))
                           (list (cons "Content-Type" "application/json")
                                 (cons "X-GitHub-Api-Version" "2026-06-01")
                                 (cons "X-Client-Session-Id" (provider-session-id provider))))
                 :content (json-encode (copilot-auto-session-request))
                 :force-string t :keep-alive nil :max-redirects 0
                 :connect-timeout 10 :read-timeout 30)))
        (unless (integerp status)
          (error 'configuration-error :message "Copilot Auto returned an invalid HTTP status."))
        (unless (<= 200 status 299)
          (provider--signal-http-status-failure provider status))
        (let ((document (handler-case (json-decode body)
                          (error ()
                            (error 'configuration-error
                                   :message "Copilot Auto returned invalid JSON.")))))
          (unless (json-object-p document)
            (error 'configuration-error :message "Copilot Auto returned invalid JSON."))
          (let ((expires-at (json-get document "expires_at")))
            (when (and (integerp expires-at)
                       (<= expires-at (- (get-universal-time) 2208988800)))
              (error 'configuration-error :message "Copilot Auto returned an expired session.")))
          document))
    (http-request-failed (condition)
      (provider--signal-http-status-failure provider (response-status condition)))))

(-> copilot-auto--catalog (configuration) json-object)
(defun copilot-auto--catalog (configuration)
  "Read secret-free metadata for hidden models which Auto may select."
  (handler-case
      (let* ((catalog (read-portable-form (copilot--catalog-path configuration)))
             (document (and (equal (getf catalog :domain) (copilot--domain))
                            (getf catalog :document))))
        (unless (stringp document)
          (error 'configuration-error
                 :message "Copilot Auto route cache is missing; refresh with (models)."))
        (json-decode document))
    (configuration-error (condition) (error condition))
    (error ()
      (error 'configuration-error
             :message "Copilot Auto route cache is invalid; refresh with (models)."))))

(-> copilot-auto--model
    (copilot-auto-provider json-object &key (:vision-p boolean)) (values list string))
(defun copilot-auto--model (provider session &key vision-p)
  "Resolve an entitled concrete model, refreshing stale catalog metadata once."
  (let ((configuration (provider-configuration provider)))
    (labels ((resolve-model ()
               (copilot-auto-session-model
                session (copilot-auto--catalog configuration)
                :model-prefix "copilot/" :vision-p vision-p
                :preferred-model (copilot-auto-provider-preferred-model provider))))
      (handler-case (resolve-model)
        (cl-llm-provider-api:provider-error ()
          (copilot--fetch-models configuration :enable-p nil)
          (handler-case (resolve-model)
            (cl-llm-provider-api:provider-error ()
              (error 'configuration-error
                     :message "Copilot Auto returned no usable tool-capable model or a malformed session."))))))))

(defmethod provider-call-with-request-adapter
    ((provider copilot-auto-provider) (credentials oauth-credentials) (function function)
     &key conversation)
  "Project Auto through the selected protocol with request-scoped session credentials."
  (multiple-value-bind (model token)
      (copilot-auto--model
       provider (copilot-auto--session provider credentials)
       :vision-p (copilot-vision-request-p
                  (conversation-input-items-for-family conversation ':copilot)))
    (let* ((name (getf model :name))
           (configuration (configuration-copy
                           (provider-configuration provider) :model name
                           :reasoning-effort "none" :provider-validation-p nil))
           (adapter (copilot-provider--make
                     configuration (provider-credential-manager provider)
                     (provider-session-id provider)
                     :registration (model-provider-registration provider)
                     :protocol (getf model :protocol)))
           (*copilot-auto-request* (list adapter token)))
      (prog1 (funcall function adapter (list token))
        (setf (copilot-auto-provider-preferred-model provider)
              (subseq name (length "copilot/")))))))
