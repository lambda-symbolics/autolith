(in-package #:autolith)

(-> nous-credential-manager-create
    (configuration &key (:refresh-request-function (option function)))
    nous-credential-manager)
(defun nous-credential-manager-create (configuration &key refresh-request-function)
  "Create the shared Nous manager with private paths and current trusted settings."
  (let ((state-root (config :state-root configuration)))
    (make-instance
     'nous-credential-manager
     :primary-source           (make-instance 'autolith-credential-source
                                              :pathname (configuration-nous-auth-path configuration))
     :settings                 (lambda ()
                                 (make-provider-account-configuration
                                  (list :state-root state-root :nous-portal-url (nous-portal-url))))
     :refresh-request-function refresh-request-function)))
