;;;; -- Shared Family Account Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () integer) run-family-account-tests))

(defun run-family-account-tests ()
  "Check settings ownership and the host-independent family credential engine."
  (let* ((checks    0)
         (directory (merge-pathnames
                     (format nil "autolith-family-accounts-~36R/" (random (expt 36 20)))
                     (uiop:temporary-directory)))
         (key-path  (merge-pathnames "keys.sexp" directory)))
    (labels ((check (value)
               (incf checks)
               (unless value
                 (error "Shared family account check ~D failed." checks)))

             (rejects (function type)
               (check (handler-case (progn (funcall function) nil)
                        (error (condition) (typep condition type)))))

             (settings (&rest values)
               (make-provider-account-configuration (append (list :state-root directory) values))))
      (unwind-protect
           (progn
             (dolist (invalid (list nil '(:state-root) '(:state-root . "root")
                                   '(:unknown "value" :state-root "root")
                                   '(:state-root "first" :state-root "second")
                                   '(:state-root "root" :gemini-client-id ("nested"))
                                   '(:state-root "root" :gemini-client-id #("nested"))))
               (rejects (lambda () (make-provider-account-configuration invalid))
                        'family-account-settings-error))
             (let ((circular (list :state-root "root")))
               (setf (rest (rest circular)) circular)
               (rejects (lambda () (make-provider-account-configuration circular))
                        'family-account-settings-error))
             (let* ((value         (copy-seq "initial"))
                    (input         (list :state-root directory :gemini-client-id value
                                         :nous-portal-url value))
                    (configuration (make-provider-account-configuration input)))
               (setf (char value 0) #\X
                     (getf input :gemini-client-id) "replacement")
               (check (string= "initial" (provider-account-config :gemini-client-id configuration)))
               (check (string= "initial" (provider-account-config :nous-portal-url configuration)))
               (setf (char (provider-account-config :gemini-client-id configuration) 0) #\Y)
               (check (string= "initial" (provider-account-config :gemini-client-id configuration))))
             (let ((marker "private-setting-value"))
               (handler-case
                   (make-provider-account-configuration
                    (list :state-root directory :unknown marker))
                 (family-account-settings-error (condition)
                   (check (eq :invalid-account-configuration (family-account-settings-error-reason condition)))
                   (check (not (search marker (princ-to-string condition)))))))
             (let* ((current (settings :gemini-client-id     "client-before"
                                       :gemini-client-secret "secret-before"))
                    (manager (make-instance 'gemini-credential-manager
                                            :primary-source (make-instance 'gemini-credential-source
                                                                           :pathname (merge-pathnames "gemini.sexp" directory))
                                            :settings       (lambda () current))))
               (check (string= "client-before" (cl-rfc8628:credential-manager-client-id manager)))
               (setf current (settings :gemini-client-id    "client-after"
                                      :gemini-client-secret "secret-after"))
               (check (string= "client-after" (cl-rfc8628:credential-manager-client-id manager)))
               (check (equal "secret-after"
                             (rest (assoc "client_secret"
                                          (cl-rfc8628:credential-manager-refresh-parameters manager "refresh")
                                          :test #'string=))))
               (setf current (settings :gemini-client-id "client-cleared"))
               (check (null (assoc "client_secret"
                                   (cl-rfc8628:credential-manager-refresh-parameters manager "refresh")
                                   :test #'string=)))
               (let ((*provider-account-active-configuration* (settings :gemini-client-id "request-client")))
                 (check (string= "request-client" (cl-rfc8628:credential-manager-client-id manager))))
               (check (string= "client-cleared" (cl-rfc8628:credential-manager-client-id manager))))
             (let* ((current (settings :nous-portal-url "https://first.invalid/"))
                    (manager (make-instance 'nous-credential-manager
                                            :primary-source (make-instance 'autolith-credential-source
                                                                           :pathname (merge-pathnames "nous.sexp" directory))
                                            :settings       (lambda () current))))
               (check (string= "https://first.invalid/api/oauth/token"
                               (cl-rfc8628:credential-manager-token-endpoint manager)))
               (setf current (settings :nous-portal-url "https://second.invalid"))
               (check (string= "https://second.invalid/api/oauth/token"
                               (cl-rfc8628:credential-manager-token-endpoint manager)))
               (let ((*nous-oauth-client-id* "current-client"))
                 (check (string= "current-client" (cl-rfc8628:credential-manager-client-id manager)))))
             (let ((*grok-oauth-issuer*    "https://issuer.invalid")
                   (*grok-oauth-client-id* "current-client")
                   (manager                (make-instance 'grok-credential-manager
                                                          :primary-source (make-instance 'autolith-credential-source
                                                                                         :pathname (merge-pathnames "grok.sexp" directory)))))
               (check (string= "https://issuer.invalid/oauth2/token"
                               (cl-rfc8628:credential-manager-token-endpoint manager)))
               (check (string= "current-client" (cl-rfc8628:credential-manager-client-id manager)))
               (check (string= "https://issuer.invalid::current-client" (grok-auth-json-scope))))
             (let ((*gemini-oauth-client-id* "dynamic-google-client")
                   (*nous-portal-url*        "https://dynamic-portal.invalid"))
               (check (string= "dynamic-google-client"
                               (cl-rfc8628:credential-manager-client-id
                                (gemini-credential-manager-create (settings)))))
               (check (string= "https://dynamic-portal.invalid/api/oauth/token"
                               (cl-rfc8628:credential-manager-token-endpoint
                                (nous-credential-manager-create (settings))))))
             (let ((manager (api-key-credential-manager-create :provider-name "Mixed-Case" :pathname key-path)))
               (check (not (api-key-credential-available-p manager)))
               (api-key-credential-manager-save-key manager "synthetic-key")
               (check (api-key-credential-available-p manager))
               (call-with-credentials
                (api-key-credential-manager-create :provider-name "mixed-case" :pathname key-path)
                (lambda (credentials)
                  (check (string= "synthetic-key" (cl-rfc8628:oauth-credentials-access-token credentials)))))
               (check (ls-compat.posix:file-information-private-p (ls-compat.posix:file-information key-path)))))
        (when (probe-file directory)
          (dolist (file (uiop:directory-files directory))
            (delete-file file))
          (uiop:delete-empty-directory directory))))
    checks))
