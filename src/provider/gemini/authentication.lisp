(in-package #:autolith)

(-> configuration-gemini-auth-path (configuration) pathname)
(defun configuration-gemini-auth-path (configuration)
  "Return Autolith's private Gemini OAuth credential pathname."
  (merge-pathnames "gemini-auth.sexp" (config :state-root configuration)))

(-> gemini-credential-manager-create (configuration) gemini-credential-manager)
(defun gemini-credential-manager-create (configuration)
  "Create the shared Gemini manager with private paths and current trusted settings."
  (let ((state-root (config :state-root configuration)))
    (make-instance
     'gemini-credential-manager
     :primary-source (make-instance 'gemini-credential-source
                                    :pathname (configuration-gemini-auth-path configuration))
     :settings       (lambda ()
                       (make-provider-account-configuration
                        (append (list :state-root state-root :gemini-client-id (gemini-oauth-client-id))
                                (let ((secret (gemini-oauth-client-secret)))
                                  (when secret (list :gemini-client-secret secret)))))))))
