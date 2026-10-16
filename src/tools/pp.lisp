(in-package #:autolith)

;;;; -- Pricklypear Habitat Evaluation --

(defclass pp-tool (tool)
  ()
  (:documentation "A tool for working with a Pricklypear habitat."))

(defclass pp-eval-tool (pp-tool)
  ()
  (:documentation "Evaluate one Nopales expression on the Pricklypear habitat."))

(defparameter *pp-env-file*
  (merge-pathnames ".config/pricklypear/env" (user-homedir-pathname))
  "The local Pricklypear environment file consulted when PP_* variables are unset.")

(-> pp--env-entries (pathname) list)
(defun pp--env-entries (pathname)
  "Return an alist of KEY/VALUE strings parsed from a KEY=VALUE environment file."
  (with-open-file (stream pathname :if-does-not-exist nil)
    (unless stream
      (return-from pp--env-entries nil))
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim " " line)
          when (uiop:string-prefix-p "export " trimmed)
          do (setf trimmed (subseq trimmed 7))
          unless (or (zerop (length trimmed)) (char= (char trimmed 0) #\#))
          when (find #\= trimmed)
          collect (let ((separator (position #\= trimmed)))
                    (cons (string-trim " " (subseq trimmed 0 separator))
                          (string-trim " \"'" (subseq trimmed (1+ separator))))))))

(-> pp--config (&optional pathname) (values (or null string) (or null string) (or null string)))
(defun pp--config (&optional (env-file *pp-env-file*))
  "Return habitat URL, user, and password from PP_URL, PP_USER, and PP_PASS.
Unset variables fall back to the local env file. Values never appear in messages."
  (let ((entries (pp--env-entries env-file)))
    (flet ((value (name)
             (or (let ((raw (uiop:getenv name))) (and (non-empty-string-p raw) raw))
                 (cdr (assoc name entries :test #'string=)))))
      (values (value "PP_URL") (value "PP_USER") (value "PP_PASS")))))

(-> pp--timeout () integer)
(defun pp--timeout ()
  "Return the /api/eval read timeout in seconds, from PP_EVAL_TIMEOUT or 120."
  (let ((raw (uiop:getenv "PP_EVAL_TIMEOUT")))
    (or (and (non-empty-string-p raw)
             (parse-integer raw :junk-allowed t))
        120)))

(-> pp--endpoint (string) string)
(defun pp--endpoint (url)
  "Return the /api/eval endpoint for habitat URL."
  (format nil "~A/api/eval" (string-right-trim "/" url)))

(-> pp--interpret-response (string) string)
(defun pp--interpret-response (body)
  "Decode one /api/eval JSON BODY and return its value text.
Signal TOOL-ERROR for malformed, failed, or incomplete responses."
  (flet ((fail (message)
           (error 'tool-error :message message :tool-name "pp.eval")))
    (let ((decoded
            (handler-case (json-decode body)
              (error ()
                (fail "pp.eval received an unparseable JSON response.")))))
      (unless (json-object-p decoded)
        (fail "pp.eval response was not a JSON object."))
      (let ((ok (json-get decoded "ok")))
        (cond
          ((json-true-p ok)
           (let ((value (json-get decoded "value")))
             (if (stringp value) value (json-encode value))))
          ((json-false-p ok)
           (fail (format nil "pp.eval evaluation failed: ~A"
                         (or (json-get decoded "error") "unknown error"))))
          (t
           (fail "pp.eval response lacked a usable ok field.")))))))

(-> pp--eval (string string string string) string)
(defun pp--eval (expr url user password)
  "Evaluate EXPR on the habitat at URL with Basic USER/PASSWORD credentials."
  (let ((authorization
          (format nil "Basic ~A"
                  (cl-base64:string-to-base64-string
                   (format nil "~A:~A" user password)))))
    (handler-case
        (multiple-value-bind (body status)
            (dexador:post (pp--endpoint url)
                          :headers (list (cons "authorization" authorization)
                                         (cons "content-type" "application/json"))
                          :content (json-encode (json-object "expr" expr))
                          :connect-timeout 10
                          :read-timeout (pp--timeout))
          (unless (= status 200)
            (error 'tool-error
                   :message (format nil "pp.eval received HTTP ~D from the habitat."
                                    status)
                   :tool-name "pp.eval"))
          (pp--interpret-response body))
      (tool-error (condition)
        (error condition))
      (error (condition)
        (error 'tool-error
               :message (format nil "pp.eval could not reach the habitat: ~A"
                                condition)
               :tool-name "pp.eval")))))

(defmethod tool-execute ((tool pp-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate one Nopales expression on the configured habitat."
  (declare (ignore tool context))
  (let ((expr (tool-argument arguments "expr" :required t)))
    (unless (non-empty-string-p expr)
      (error 'tool-error
             :message "pp.eval requires a non-empty string expr."
             :tool-name "pp.eval"))
    (multiple-value-bind (url user password) (pp--config)
      (unless (and (non-empty-string-p url)
                   (non-empty-string-p user)
                   (non-empty-string-p password))
        (error 'tool-error
               :message
               "pp.eval requires PP_URL, PP_USER, and PP_PASS, from the environment or ~/.config/pricklypear/env."
               :tool-name "pp.eval"))
      (pp--eval expr url user password))))
