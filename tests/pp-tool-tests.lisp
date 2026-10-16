(in-package #:autolith)

;;;; -- Pricklypear Tool Tests --

(-> pp-tests--tool-error-p (function) boolean)
(defun pp-tests--tool-error-p (function)
  "Return true when FUNCTION signals TOOL-ERROR."
  (handler-case
      (progn (funcall function) nil)
    (tool-error () t)))

(-> test-pp-endpoint-and-timeout () null)
(defun test-pp-endpoint-and-timeout ()
  "Test /api/eval endpoint building and read-timeout resolution."
  (test-assert
   (string= (pp--endpoint "https://pricklypear.rocks")
            "https://pricklypear.rocks/api/eval")
   "endpoint appends /api/eval to a bare habitat URL")
  (test-assert
   (string= (pp--endpoint "http://127.0.0.1:18080/")
            "http://127.0.0.1:18080/api/eval")
   "endpoint strips a trailing slash before appending /api/eval")
  (let ((variable "PP_EVAL_TIMEOUT")
        (saved    (uiop:getenv "PP_EVAL_TIMEOUT")))
    (unwind-protect
         (progn
           (platform-setenv variable "45")
           (test-assert (= (pp--timeout) 45)
                        "PP_EVAL_TIMEOUT overrides the read timeout")
           (platform-setenv variable "later")
           (test-assert (= (pp--timeout) 120)
                        "a non-numeric PP_EVAL_TIMEOUT falls back to 120"))
      (tests--restore-environment variable saved)))
  nil)

(-> test-pp-env-file-parsing () null)
(defun test-pp-env-file-parsing ()
  "Test KEY=VALUE environment file parsing."
  (uiop:with-temporary-file (:stream stream :pathname path :suffix ".env")
    (write-line "export PP_URL=https://pricklypear.rocks" stream)
    (write-line "PP_URL_LOCAL=https://local.example" stream)
    (write-line "# a comment" stream)
    (write-line "" stream)
    (write-line "PP_USER = alice" stream)
    (write-line "PP_PASS=\"s3cret'quote\"" stream)
    (finish-output stream)
    (let ((entries (pp--env-entries path)))
      (test-assert
       (string= (cdr (assoc "PP_URL" entries :test #'string=))
                "https://pricklypear.rocks")
       "a plain entry parses")
      (test-assert
       (string= (cdr (assoc "PP_USER" entries :test #'string=)) "alice")
       "spacing around = is trimmed")
      (test-assert
       (string= (cdr (assoc "PP_PASS" entries :test #'string=))
                "s3cret'quote")
       "surrounding quotes are trimmed from values")
      (test-assert
       (null (assoc "COMMENT" entries :test #'string=))
       "comment and blank lines are skipped")))
  nil)

(-> test-pp-interpret-response () null)
(defun test-pp-interpret-response ()
  "Test /api/eval JSON response interpretation."
  (test-assert
   (string= (pp--interpret-response "{\"ok\":true,\"value\":\"roerick\"}")
            "roerick")
   "a string value passes through unchanged")
  (test-assert
   (string= (pp--interpret-response "{\"ok\":true,\"value\":null}") "null")
   "a null value encodes as JSON")
  (test-assert
   (pp-tests--tool-error-p
    (lambda () (pp--interpret-response "{\"ok\":false,\"error\":\"boom\"}")))
   "an ok:false response signals tool-error")
  (test-assert
   (pp-tests--tool-error-p
    (lambda () (pp--interpret-response "not json at all")))
   "an unparseable response signals tool-error")
  (test-assert
   (pp-tests--tool-error-p
    (lambda () (pp--interpret-response "[1,2,3]")))
   "a non-object response signals tool-error")
  nil)

(-> test-pp-config-fallback () null)
(defun test-pp-config-fallback ()
  "Test environment variables winning over the local env file."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (stream path :direction ':output
                                    :if-exists ':supersede)
      (write-line "PP_URL=https://file.example" stream)
      (write-line "PP_USER=file-user" stream)
      (write-line "PP_PASS=file-pass" stream))
    (let ((originals
            (list (cons "PP_URL" (uiop:getenv "PP_URL"))
                  (cons "PP_USER" (uiop:getenv "PP_USER"))
                  (cons "PP_PASS" (uiop:getenv "PP_PASS")))))
      (unwind-protect
           (progn
             (platform-setenv "PP_URL" "https://env.example")
             (multiple-value-bind (url user password) (pp--config path)
               (test-assert (string= url "https://env.example")
                            "PP_URL from the environment wins")
               (test-assert (string= user "file-user")
                            "PP_USER falls back to the env file")
               (test-assert (string= password "file-pass")
                            "PP_PASS falls back to the env file"))
             (platform-setenv "PP_URL" "")
             (multiple-value-bind (url) (pp--config path)
               (test-assert (string= url "https://file.example")
                            "an empty PP_URL falls back to the env file")))
        (dolist (original originals)
          (tests--restore-environment (car original) (cdr original))))))
  nil)
