;;;; -- Standalone Shared Account Checks --

(in-package #:autolith-supervisor)

(defclass accounts-test-manager (chatgpt-credential-manager)
  ((request-function
    :initarg       :request-function
    :reader        accounts-test-manager-request-function
    :documentation "The test exchange, called with URL, headers, and content."))
  (:documentation "A shared ChatGPT manager with a controlled token endpoint."))

(defmethod cl-rfc8628:credential-manager-refresh-request
    ((manager accounts-test-manager) &key url headers content)
  "Replace the token endpoint while retaining shared deadline and parsing policy."
  (funcall (accounts-test-manager-request-function manager) url headers content))

(declaim (ftype (function () integer) run-accounts-tests))

(defun run-accounts-tests ()
  "Check shared private storage, bootstrap isolation, refresh, and secret accounting."
  (let* ((checks         0)
         (*random-state* (make-random-state t))
         (directory      (merge-pathnames
                          (format nil "autolith-accounts-~36R/" (random (expt 36 20)))
                          (uiop:temporary-directory)))
         (auth-path      (merge-pathnames "auth.sexp" directory))
         (bootstrap-path (merge-pathnames "codex.json" directory)))
    (labels ((check (value)
               (incf checks)
               (unless value
                 (error "Shared accounts check ~D failed." checks)))

             (rejects (function type)
               (check (handler-case (progn (funcall function) nil)
                        (error (condition)
                          (typep condition type)))))

             (credentials (&key (access "synthetic-access") (refresh "synthetic-refresh")
                                (account "account-a") (expires (+ (get-universal-time) 3600)))
               (make-instance 'cl-rfc8628:oauth-credentials
                              :access-token access :refresh-token refresh :id-token nil
                              :account-id account :expires-at expires :source-path auth-path))

             (manager (&optional request-function)
               (if request-function
                   (make-instance 'accounts-test-manager
                                  :primary-source (make-instance 'autolith-credential-source
                                                                 :pathname auth-path)
                                  :bootstrap-source nil :request-function request-function)
                   (credential-manager-create :auth-path       auth-path
                                              :codex-auth-path bootstrap-path)))

             (save (credentials)
               (cl-rfc8628:credential-source-save
                (make-instance 'autolith-credential-source :pathname auth-path) credentials))

             (write-bootstrap (mode account)
               (sexp-store:snapshot-write-text
                bootstrap-path
                (cl-rfc8628:json-encode
                 (cl-rfc8628:json-object
                  "auth_mode" mode
                  "tokens" (cl-rfc8628:json-object "account_id" account
                                                 "access_token" "bootstrap-access"
                                                 "id_token" "bootstrap-id"
                                                 "refresh_token" "bootstrap-refresh"))))))
      (unwind-protect
           (progn
             (ensure-directories-exist auth-path)
             (let* ((manager (manager))
                    (source  (cl-rfc8628:credential-manager-primary-source manager)))
               (rejects (lambda () (cl-rfc8628:credential-manager-load manager))
                        'credentials-unavailable)
               (check (null (cl-rfc8628:credential-manager-bootstrap-source
                             (credential-manager-create :auth-path auth-path))))
               (write-bootstrap "apikey" "account-a")
               (check (null (cl-rfc8628:credential-source-load
                             (cl-rfc8628:credential-manager-bootstrap-source manager))))
               (write-bootstrap "chatgpt" "account-a")
               (let* ((before   (uiop:read-file-string bootstrap-path))
                      (imported (cl-rfc8628:credential-manager-load manager)))
                 (check (equal auth-path (cl-rfc8628:oauth-credentials-source-path imported)))
                 (check (null (cl-rfc8628:oauth-credentials-refresh-token imported)))
                 (check (null (cl-rfc8628:oauth-credentials-id-token imported)))
                 (check (string= before (uiop:read-file-string bootstrap-path)))
                 (check (ls-compat.posix:file-information-private-p
                         (ls-compat.posix:file-information auth-path)))
                 (rejects (lambda ()
                            (cl-rfc8628:credential-source-save
                             (cl-rfc8628:credential-manager-bootstrap-source manager) imported))
                          'authentication-error))
               (write-bootstrap "chatgpt" "account-b")
               (check (string= "account-a"
                               (cl-rfc8628:oauth-credentials-account-id
                                (cl-rfc8628:credential-manager-load manager))))
               (let ((original (credentials)))
                 (save original)
                 (let ((loaded (cl-rfc8628:credential-source-load source)))
                   (check (string= "synthetic-access"
                                   (cl-rfc8628:oauth-credentials-access-token loaded)))
                   (check (string= "synthetic-refresh"
                                   (cl-rfc8628:oauth-credentials-refresh-token loaded))))
                 (with-credentials (loaded manager)
                   (check *credentials-in-request-scope*)
                   (check (secret-use-active-p))
                   (check (string= "account-a"
                                   (cl-rfc8628:oauth-credentials-account-id loaded))))
                (check (not *credentials-in-request-scope*))
                (check (not (secret-use-active-p)))))
             ;; Exercise the library exchange through our sole shared refresh adapter.
             (let* ((calls   0)
                    (manager
                      (manager
                       (lambda (url headers content)
                         (incf calls)
                         (check (string= url *openai-oauth-token-endpoint*))
                         (check (equal "application/json" (rest (assoc "Content-Type" headers
                                                                      :test #'string-equal))))
                         (let ((document (cl-rfc8628:json-decode content)))
                           (check (string= "synthetic-refresh"
                                           (cl-rfc8628:json-get document "refresh_token")))
                           (check (string= *openai-oauth-client-id*
                                           (cl-rfc8628:json-get document "client_id"))))
                         (check (secret-use-active-p))
                         (values "{\"access_token\":\"rotated-access\",\"refresh_token\":\"rotated-refresh\",\"expires_in\":3600}"
                                 200 nil)))))
               (save (credentials))
               (with-credentials (loaded manager :force-refresh t)
                 (check (string= "rotated-access"
                                 (cl-rfc8628:oauth-credentials-access-token loaded))))
               (check (= 1 calls))
               (let ((loaded (cl-rfc8628:credential-source-load
                              (cl-rfc8628:credential-manager-primary-source manager))))
                 (check (string= "rotated-refresh"
                                 (cl-rfc8628:oauth-credentials-refresh-token loaded)))))
             ;; Rejected refresh responses retain only bounded redacted metadata.
             (let ((manager
                     (manager
                      (lambda (url headers content)
                        (declare (ignore url headers content))
                        (values
                         (make-string-input-stream
                          "{\"error\":\"synthetic-refresh\",\"error_description\":\"synthetic-access\"}")
                         401 nil)))))
               (save (credentials))
               (let ((failure
                       (handler-case
                           (call-with-credentials manager (lambda (credentials)
                                                            (declare (ignore credentials)))
                                                  :force-refresh t)
                         (token-refresh-failed (condition)
                           condition))))
                 (check (typep failure 'token-refresh-failed))
                 (check (= 401 (token-refresh-failed-status failure)))
                 (check (not (search "synthetic-refresh" (princ-to-string failure))))
                 (check (not (search "synthetic-access" (princ-to-string failure))))))
             (let ((manager
                     (manager (lambda (url headers content)
                                (declare (ignore url headers content))
                                (error 'sb-sys:deadline-timeout :seconds 60)))))
               (save (credentials))
               (rejects (lambda ()
                          (call-with-credentials manager (constantly nil) :force-refresh t))
                        'token-refresh-failed))
             ;; A malformed private record never executes its contents.
             (sexp-store:snapshot-write-text auth-path "#.(error \"reader evaluation\")")
             (rejects (lambda ()
                        (cl-rfc8628:credential-source-load
                         (make-instance 'autolith-credential-source :pathname auth-path)))
                      'error)
             (check (not (secret-use-active-p)))
             ;; The shared owner protects every legacy and parent secret scope.
             (call-with-secret-use-quiescence
              (lambda ()
                (call-with-secret-use (lambda () (check (secret-use-active-p))))
                (let* ((result nil)
                       (thread
                         (bordeaux-threads:make-thread
                          (lambda ()
                            (setf result
                                  (handler-case
                                      (call-with-secret-use (lambda () ':unexpected))
                                    (authentication-error ()
                                      ':rejected)))))))
                  (bordeaux-threads:join-thread thread)
                  (check (eq ':rejected result)))))
             (rejects (lambda ()
                        (call-with-secret-use (lambda ()
                                                (check (secret-use-active-p))
                                                (error 'authentication-error :message "test"))))
                      'authentication-error)
             (check (not (secret-use-active-p)))
             (check (null *secret-use-quiescence-owner*))
             checks)
        (dolist (file (uiop:directory-files directory))
          (delete-file file))
        (uiop:delete-empty-directory directory)))))
