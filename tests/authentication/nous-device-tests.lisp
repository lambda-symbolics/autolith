(in-package #:autolith)

;;;; -- Nous Device Authentication Test Support --

(-> nous-device-test--login (nous-device-authentication-client &rest t) t)
(defun nous-device-test--login (client &rest arguments)
  "Log in with a recording source and a temporary OAuth lock directory."
  (test-call-with-temporary-root
   (lambda (root)
     (apply
      #'device-authentication-login
      client
      (make-instance
       'nous-credential-manager
       :primary-source
       (make-instance 'recording-autolith-credential-source
                      :pathname (merge-pathnames "nous-auth.sexp" root))
       :refresh-request-function
       (lambda (&key method url headers content)
         (declare (ignore method url headers content))
         (error "Unexpected Nous refresh request in a device-flow test.")))
      arguments))))

(-> nous-device-test--token-response
    (&key (:subject string) (:scope t) (:refresh-token string))
    string)
(defun nous-device-test--token-response
    (&key (subject "nous-user-1")
          (scope *nous-oauth-scope*)
          (refresh-token "nous-refresh-test"))
  "Return a successful Nous token response body."
  (json-encode
   (json-object
    "access_token" (nous-test--jwt :subject subject :scope scope)
    "refresh_token" refresh-token
    "expires_in" 900
    "scope" scope
    "token_type" "Bearer")))

(-> nous-device-test--request-code-response
    (&key (:user-code string) (:verification-url string))
    string)
(defun nous-device-test--request-code-response
    (&key (user-code "NOUS-CODE")
          (verification-url "https://portal.test/device"))
  "Return a Nous device authorization response body."
  (json-encode
   (json-object
    "device_code" "nous-device-code-1"
    "user_code" user-code
    "verification_uri" "https://portal.test/device"
    "verification_uri_complete" verification-url
    "expires_in" 600
    "interval" 2)))


;;;; -- Nous Device Authentication Tests --

(-> nous-device-test--complete-flow () null)
(defun nous-device-test--complete-flow ()
  "Exercise the Nous device request, display, and locked credential publication."
  (let ((requests                                       nil)
        (poll-count                                     0)
        (clock                                          0)
        (*device-authentication-test-saved-credentials* nil))
    (flet ((request (&key method url headers content)
             (push (list :method  method
                         :url     url
                         :headers headers
                         :content content)
                   requests)
             (cond
               ((uiop:string-suffix-p
                 url
                 "/api/oauth/device/code")
                (values
                 (nous-device-test--request-code-response
                  :verification-url
                  "https://portal.test/device?user_code=NOUS-CODE")
                 200
                 nil))
               ((uiop:string-suffix-p
                 url
                 "/api/oauth/token")
                (incf poll-count)
                (case poll-count
                  (1
                   (values
                    (json-encode (json-object "error" "authorization_pending"))
                    400
                    nil))
                  (2
                   (values
                    (json-encode (json-object "error" "slow_down"))
                    400
                    nil))
                  (t
                   (values (nous-device-test--token-response) 200 nil))))
               (t
                (error "Unexpected Nous device test URL."))))

           (pause (seconds)
             (incf clock seconds))

           (now ()
             clock))
      (let* ((client
               (nous-device-authentication-client-create
                :portal-url        "https://portal.test/"
                :request-function  #'request
                :sleep-function    #'pause
                :clock-function    #'now
                 :browser-function (constantly t)))
             (output (make-string-output-stream))
             (result
               (nous-device-test--login
                client
                :stream output)))
        (test-assert (eq result t)
                     "the Nous device login reports success")
        (let ((credentials *device-authentication-test-saved-credentials*))
          (test-assert
           (and credentials
                (string= (oauth-credentials-account-id credentials)
                         "nous-user-1")
                (string= (oauth-credentials-refresh-token credentials)
                         "nous-refresh-test"))
           "the Nous device flow publishes scoped renewable credentials"))
        (let ((displayed (get-output-stream-string output)))
          (test-assert
           (and (search "NOUS-CODE" displayed)
                (search "https://portal.test/device?user_code=NOUS-CODE"
                        displayed)
                (not (search "nous-device-code-1" displayed)))
           "the Nous display includes only the public URL and one-time code"))
        (let ((code-request
                (device-authentication-test--request
                 requests
                 "/api/oauth/device/code")))
          (test-assert
           (and code-request
                (search "client_id=hermes-cli"
                        (getf code-request :content))
                (search "scope=inference%3Ainvoke"
                        (getf code-request :content)))
           "the Nous device request carries the public client and inference scope")))))
  nil)

(-> nous-device-test--rejections () null)
(defun nous-device-test--rejections ()
  "Reject malformed Nous credential data and tokens without inference scope."
  (labels ((client-for (responder)
             (let ((clock 0))
               (nous-device-authentication-client-create
                :portal-url       "https://portal.test"
                :request-function responder
                :sleep-function   (lambda (seconds)
                                    (incf clock seconds))
                :clock-function   (lambda () clock)
                :browser-function (lambda (url)
                                    (declare (ignore url))
                                    t)
                :poll-timeout     10)))

           (poll-responder (body status)
             (lambda (&key method url headers content)
               (declare (ignore method headers content))
               (if (uiop:string-suffix-p
                    url
                    "/api/oauth/device/code")
                   (values (nous-device-test--request-code-response) 200 nil)
                   (values body status nil))))

           (login (responder)
             (nous-device-test--login
              (client-for responder)
              :stream         (make-string-output-stream)
              :open-browser-p nil)))
    (device-authentication-test--signals
     (lambda ()
       (login
        (poll-responder
         (nous-device-test--token-response :scope "profile")
         200)))
     ':credentials)
    (device-authentication-test--signals
     (lambda ()
       (login
        (poll-responder
         (json-encode
          (json-object
           "access_token" "not-a-jwt"
           "refresh_token" "refresh-secret"))
         200)))
     ':credentials)
   nil))

(-> nous-device-test--secret-redaction () null)
(defun nous-device-test--secret-redaction ()
  "Test that a malicious poll error cannot retain the secret device code."
  (let ((signaled-p nil))
    (handler-case
        (nous-device-test--login
         (nous-device-authentication-client-create
          :portal-url       "https://portal.test"
          :request-function
          (lambda (&key method url headers content)
            (declare (ignore method headers content))
            (if (uiop:string-suffix-p
                 url
                 "/api/oauth/device/code")
                (values (nous-device-test--request-code-response) 200 nil)
                (values
                 (json-encode
                  (json-object
                   "error"
                   "failure-nous-device-code-1-echo"))
                 400
                 nil)))
          :sleep-function   (lambda (seconds) (declare (ignore seconds)))
          :clock-function   (let ((clock 0))
                              (lambda () (incf clock)))
          :browser-function (lambda (url)
                              (declare (ignore url))
                              t)
          :poll-timeout     10)
         :stream         (make-string-output-stream)
         :open-browser-p nil)
      (device-authentication-error (condition)
        (setf signaled-p t)
        (test-assert
         (not (test-object-contains-string-p
               condition
               "nous-device-code-1"))
         "Nous device failures redact an echoed secret device code")))
    (test-assert signaled-p
                 "the malicious Nous poll response signals a device error"))
  nil)
