(in-package #:autolith)

(-> grok-credential-manager-create (configuration) grok-credential-manager)
(defun grok-credential-manager-create (configuration)
  "Create the Grok credential manager for CONFIGURATION's private paths."
  (make-instance 'grok-credential-manager
                 :primary-source
                 (make-instance
                  'autolith-credential-source
                  :pathname (configuration-grok-auth-path configuration))
                 :bootstrap-source
                 (make-instance
                  'grok-bootstrap-credential-source
                  :pathname (config :grok-bootstrap-auth-path
                             configuration))))
