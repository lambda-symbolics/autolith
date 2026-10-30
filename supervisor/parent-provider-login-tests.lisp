;;;; -- Trusted Provider Login Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-provider-login-test--replace (symbol function function) t)
(defun parent-provider-login-test--replace (name replacement function)
  "Call FUNCTION with one process-local function replacement, then restore it."
  (let ((original (symbol-function name)))
    (unwind-protect
         (progn
           (setf (symbol-function name) replacement)
           (funcall function))
      (setf (symbol-function name) original))))

(serapeum/types:-> run-parent-provider-login-tests () integer)
(defun run-parent-provider-login-tests ()
  "Check login dispatch, key validation, publication, cancellation, and Gemini settings."
  (let* ((checks        0)
         (directory     (merge-pathnames
                         (format nil "autolith-login-checks-~36R/"
                                 (random (expt 36 20) (make-random-state t)))
                         (uiop:temporary-directory)))
         (configuration (make-provider-account-configuration (list :state-root directory)))
         (output        (make-string-output-stream))
         (requests      nil))
    (labels ((check (value)
               (incf checks)
               (unless value
                 (error "Provider login check ~D failed." checks)))

             (rejects (function type)
               (check (handler-case (progn (funcall function) nil)
                        (error (condition)
                          (typep condition type)))))

             (login (provider &rest arguments)
               (apply #'parent-provider-login configuration provider
                      :input (make-string-input-stream "fixture-key") :output output arguments))

             (request (url &rest options)
               (push (list url options) requests)
               "{}"))
      (unwind-protect
           (progn
             (check (equal '(:browser :device) (parent-provider-login-methods "chatgpt")))
             (rejects (lambda () (login "unknown")) 'parent-provider-login-error)
             (rejects (lambda () (login "gemini" :method ':device)) 'parent-provider-login-error)
             (rejects (lambda () (login "anthropic" :cancelled (constantly t)))
                      'parent-provider-login-cancelled)
             (check (null (uiop:directory-files directory)))
             (parent-provider-login-test--replace
              'cl-rfc8628:device-authentication-login
              (lambda (client manager &key stream open-browser-p)
                (declare (ignore client stream))
                (check (not open-browser-p))
                (make-instance 'cl-rfc8628:oauth-credentials
                               :access-token "fixture-token" :refresh-token nil :id-token nil
                               :expires-at nil :account-id nil
                               :source-path (cl-rfc8628:credential-source-pathname
                                             (cl-rfc8628:credential-manager-primary-source manager))))
              (lambda ()
                (dolist (family '(:codex :grok :nous :copilot))
                  (let ((result (login family :method ':device :open-browser-p nil)))
                    (check (eq ':device (getf result :method)))
                    (check (eq ':authenticated (getf result :status)))))))
             (parent-provider-login-test--replace
              'dexador:get #'request
              (lambda ()
                (parent-provider-login-test--replace
                 'dexador:post #'request
                 (lambda ()
                   (dolist (family '(:anthropic :mistral :openrouter :fireworks))
                     (let ((result (login family)))
                       (check (eq ':authenticated (getf result :status)))
                       (check (not (search "fixture-key" (prin1-to-string result))))))))))
             (check (= 4 (length requests)))
             (dolist (request requests)
               (let* ((options (second request))
                      (headers (getf options :headers)))
                 (check (or (equal "fixture-key" (rest (assoc "x-api-key" headers :test #'equal)))
                            (equal "Bearer fixture-key"
                                   (rest (assoc "Authorization" headers :test #'equal)))))
                 (check (= 60 (getf options :read-timeout)))))
             (check (uiop:directory-files directory))
             (let ((before (loop for file in (uiop:directory-files directory)
                                 collect (cons file (uiop:read-file-string file)))))
               (parent-provider-login-test--replace
                'dexador:get
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error "fixture-key rejected"))
                (lambda ()
                  (rejects (lambda () (login :anthropic)) 'parent-provider-login-error)))
               (check (every (lambda (entry)
                               (equal (rest entry) (uiop:read-file-string (first entry))))
                             before)))
             (let ((calls 0))
               (parent-provider-login-test--replace
                'parent-provider-login--validate-key
                (lambda (family key)
                  (declare (ignore family key))
                  (incf calls)
                  nil)
                (lambda ()
                  (rejects (lambda ()
                             (login :anthropic :cancelled (lambda () (plusp calls))))
                           'parent-provider-login-cancelled))))
             (let ((unwound   nil)
                   (cancelled nil))
               (parent-provider-login-test--replace
                'parent-provider-login--validate-key
                (lambda (family key)
                  (declare (ignore family key))
                  (unwind-protect
                       (progn (setf cancelled t) (sleep 5))
                    (setf unwound t)))
                (lambda ()
                  (rejects (lambda ()
                             (login :anthropic :cancelled (lambda () cancelled)))
                           'parent-provider-login-cancelled)))
               (check unwound)
               (check (= 42 (parent-provider-login--call (constantly 42) (constantly nil)))))
             (let* ((settings (make-provider-account-configuration
                               (list :state-root directory :gemini-client-id "manager-client"
                                     :gemini-client-secret "manager-secret")))
                    (manager  (gemini-credential-manager-create settings)))
               (parent-provider-login-test--replace
                'cl-rfc8252:browser-authentication-login
                (lambda (client received &rest options)
                  (declare (ignore client options))
                  (check (eq manager received))
                  (check (equal "manager-client" (gemini-oauth-client-id)))
                  (check (equal "manager-secret" (gemini-oauth-client-secret)))
                  (make-instance 'cl-rfc8628:oauth-credentials
                                 :access-token "fixture-token" :refresh-token nil :id-token nil
                                 :expires-at nil :account-id nil
                                 :source-path (merge-pathnames "gemini-auth.sexp" directory)))
                (lambda () (gemini-oauth-login manager :open-browser-p nil)))))
        (when (probe-file directory)
          (dolist (file (uiop:directory-files directory))
            (delete-file file))
          (uiop:delete-empty-directory directory))))
    checks))
