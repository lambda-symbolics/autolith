(in-package #:autolith)

(-> copilot--domain () string)
(defun copilot--domain ()
  "Read the shared trusted issuer and adapt invalid settings to the agent condition."
  (handler-case (autolith-supervisor:copilot--domain)
    (family-account-settings-error (condition)
      (error 'configuration-error :message (autolith-error-message condition)))))

(-> copilot-credential-manager-create (configuration) copilot-credential-manager)
(defun copilot-credential-manager-create (configuration)
  "Create a Copilot manager without retaining any tokens in the image."
  (make-instance 'copilot-credential-manager
                 :domain (copilot--domain)
                 :primary-source
                 (make-instance 'autolith-credential-source
                                :pathname (merge-pathnames "copilot-auth.sexp"
                                                           (config :state-root configuration)))))
