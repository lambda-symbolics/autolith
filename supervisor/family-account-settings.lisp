;;;; -- Trusted Provider Account Configuration --

(in-package #:autolith-supervisor)

(define-condition family-account-settings-error (authentication-error)
  ((reason :initarg :reason :reader family-account-settings-error-reason
           :documentation "The invalid account setting category."))
  (:documentation "Trusted family-account settings failed validation."))

(defclass provider-account-configuration ()
  ((values :initarg :values :reader provider-account-configuration-values
           :documentation "Trusted configuration data. Never read from agent messages."))
  (:documentation "Explicit parent configuration without executable initialization forms."))

(declaim (ftype (function (list) provider-account-configuration) make-provider-account-configuration))

(defun make-provider-account-configuration (values)
  "Copy trusted account settings. Reject duplicate or unknown keys."
  (let ((allowed '(:state-root :api-keys-path :grok-bootstrap-auth-path :copilot-domain
                   :gemini-project :gemini-client-id :gemini-client-secret
                   :nous-portal-url :grok-issuer :grok-client-id))
        (seen    nil))
    (unless (let ((length (handler-case (list-length values) (type-error () nil))))
                 (and length (evenp length)))
      (error 'family-account-settings-error :reason ':invalid-account-configuration
             :message                               "Account settings must be a property list."))
    (loop for (key value) on values by (lambda (tail) (rest (rest tail)))
          do (unless (and (member key allowed) (not (member key seen))
                          (or (stringp value) (pathnamep value)))
               (error 'family-account-settings-error :reason ':invalid-account-configuration
                      :message                               "Account settings contain an invalid or repeated field."))
             (push key seen))
    (unless (getf values :state-root)
      (error 'family-account-settings-error :reason ':missing-state-root
             :message                               "Account settings require a state root."))
    (make-instance 'provider-account-configuration :values (loop for (key value) on values by (lambda (tail) (rest (rest tail)))
                                                            append (list key (if (stringp value) (copy-seq value) value))))))

(declaim (ftype (function (keyword provider-account-configuration) t) provider-account-config))

(defun provider-account-config (key configuration)
  "Read a trusted account setting without exposing the stored mutable string."
  (let ((value (getf (provider-account-configuration-values configuration) key)))
    (if (stringp value) (copy-seq value) value)))

(declaim (ftype (function (provider-account-configuration) pathname) configuration-grok-auth-path))

(defun configuration-grok-auth-path (configuration)
  "Return the parent-owned Grok credential store."
  (merge-pathnames "grok-auth.sexp" (provider-account-config ':state-root configuration)))

(declaim (ftype (function (provider-account-configuration) pathname) configuration-nous-auth-path))

(defun configuration-nous-auth-path (configuration)
  "Return the parent-owned Nous credential store."
  (merge-pathnames "nous-auth.sexp" (provider-account-config ':state-root configuration)))

(declaim (ftype (function (t) boolean) family-account-non-empty-string-p))

(defun family-account-non-empty-string-p (value)
  "Test a nonempty credential or account field."
  (cl-rfc8628:non-empty-string-p value))

(deftype family-account-non-empty-string ()
  "A nonempty account field."
  '(and string (satisfies family-account-non-empty-string-p)))

(declaim (ftype (function () string) provider-user-agent))

(defun provider-user-agent ()
  "Return the trusted authentication user agent."
  (accounts-user-agent))

(defparameter *grok-oauth-issuer* "https://auth.x.ai"
  "The trusted xAI OAuth issuer.")

(defparameter *grok-oauth-client-id* "b1a00492-073a-47ea-816f-4c329264a828"
  "The public xAI client identifier.")

(defparameter *nous-oauth-client-id* "hermes-cli"
  "The public Nous client identifier.")

(defparameter *nous-oauth-scope* "inference:invoke"
  "The required Nous inference scope.")

(defparameter *gemini-oauth-token-endpoint* "https://oauth2.googleapis.com/token"
  "The trusted Google token endpoint.")

(defparameter *provider-account-active-configuration* nil
  "Request-local trusted account settings used by OAuth adapters.")

;; Gemini CLI OAuth behavior inspected at google-gemini/gemini-cli commit
;; 0bd1d439751478771c45d3d0895a6a9760554bf4. The installed application uses
;; PKCE as a public client. Autolith deliberately does not embed its client
;; secret; deployments that require one may provide it through the environment.
(defparameter *gemini-oauth-client-id*
  "681255809395-oo8ft2oprdrnp9e3aqf6av3hmdib135j.apps.googleusercontent.com"
  "The public Google installed-application client identifier used by Gemini CLI.")


;; Nous Portal authentication and inference behavior verified against Hermes
;; Agent reference commit f293e7206b4ddd66042329442c6afebc19a8808d.
(defparameter *nous-portal-url* "https://portal.nousresearch.com"
  "The Nous Research portal serving OAuth device authentication.")

(declaim (ftype (function () string) gemini-oauth-client-id))

(defun gemini-oauth-client-id ()
  "Return the trusted Google installed-app client identifier."
  (or (and *provider-account-active-configuration*
           (provider-account-config ':gemini-client-id *provider-account-active-configuration*))
      *gemini-oauth-client-id*))

(declaim (ftype (function () (or null string)) gemini-oauth-client-secret))

(defun gemini-oauth-client-secret ()
  "Return the optional parent-selected Google client secret."
  (and *provider-account-active-configuration*
       (provider-account-config ':gemini-client-secret *provider-account-active-configuration*)))

(declaim (ftype (function () string) nous-portal-url))

(defun nous-portal-url ()
  "Return the trusted Nous portal URL."
  (string-right-trim "/"
                     (or (and *provider-account-active-configuration*
                              (provider-account-config ':nous-portal-url
                                                       *provider-account-active-configuration*))
                         *nous-portal-url*)))

(declaim (ftype (function (string pathname) pathname) environment-directory))

(defun environment-directory (name fallback)
  "Resolve a trusted parent environment directory for synthetic source labels."
  (let ((value (uiop:getenv name)))
    (if (family-account-non-empty-string-p value) (uiop:ensure-directory-pathname value) fallback)))

(declaim (ftype (function (string) (or null integer)) rfc3339->universal-time))

(defun rfc3339->universal-time (text)
  "Parse an external token timestamp with the shared time parser."
  (handler-case (local-time:timestamp-to-universal (local-time:parse-timestring text))
    (error () nil)))

(defvar *provider-account-copilot-json-function* nil
  "Request-local trusted Copilot authentication transport, or NIL outside an invocation.")
