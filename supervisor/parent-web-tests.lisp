;;;; -- Parent Web Boundary Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-web-test--map (&rest t) hash-table)
(defun parent-web-test--map (&rest pairs)
  "Build public fixture data without reading settings or credentials."
  (let ((data (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
          do (setf (gethash key data) value))
    data))

(serapeum/types:-> parent-web-test--application
    (&key (:family keyword) (:mode string) (:permission string) (:refreshable boolean)) parent-application)
(defun parent-web-test--application (&key (family ':codex) (mode "cached") (permission "full") (refreshable t))
  "Bind a synthetic private account to a trusted model, endpoint, and mode."
  (let* ((profile  (make-model-profile :id "web-profile" :family ':responses :model "trusted-model"))
         (profiles (make-hash-table :test #'equal))
         (accounts (make-hash-table :test #'eq))
         (configuration
           (make-instance 'parent-configuration
                          :values (list :profile-id "web-profile"
                                        :settings   (list :web-search-mode mode :permission-mode permission)))))
    (setf (gethash "web-profile" profiles) profile
          (gethash profile accounts)
          (make-provider-account :family family :protocol ':responses
                                 :credential-manager (make-instance 'provider-executor-test-manager
                                                                    :refreshable refreshable)
                                 :endpoint           "https://trusted.invalid/responses"))
    (make-instance 'parent-application :configuration configuration :profiles profiles
                                     :accounts accounts :session-id "trusted-session")))

(serapeum/types:-> parent-web-test--code (function) keyword)
(defun parent-web-test--code (function)
  "Return a typed failure, rejecting success and untyped transport errors."
  (handler-case (progn (funcall function) ':unexpected-success)
    (parent-application-operation-error (condition)
      (parent-application-operation-error-code condition))))

(serapeum/types:-> run-parent-web-history-tests () (integer 0))
(defun run-parent-web-history-tests ()
  "Bound recent context without rejecting long turns, and validate omitted messages."
  (let ((checks 0))
    (labels ((check (value)
               (assert value () "Web history check ~D failed." (1+ checks))
               (incf checks)))
      (dolist (count '(0 128 129 300))
        (let* ((history
                 (map 'vector
                      (lambda (index)
                        (parent-web-test--map
                         "type" "message" "role" "assistant"
                         "content" (vector (parent-web-test--map
                                            "type" "output_text" "text" (write-to-string index)))))
                      (loop for index below count collect index)))
               (application (parent-web-test--application))
               (arguments   (parent-web-test--map
                             "action" "run" "commands" "{}"
                             "history" (cl-rfc8628:json-encode history)))
               (calls       0)
               (*parent-web-open-function*
                 (lambda (url &key method headers content)
                   (declare (ignore url method headers))
                   (incf calls)
                   (let ((input (gethash "input" (cl-rfc8628:json-decode content))))
                     (check (equalp input (subseq history (max 0 (- count 128))))))
                   (values (make-string-input-stream "{\"output\":\"result\"}")
                           200 (make-hash-table :test #'equal)))))
          (check (equal (gethash "output" (parent-web-operation application arguments (constantly nil)))
                        "result"))
          (check (= calls 1))
          (when (> count 128)
            (setf (gethash "role" (aref history 0)) "system"
                  (gethash "history" arguments) (cl-rfc8628:json-encode history))
            (check (eq ':invalid-request
                       (parent-web-test--code
                        (lambda () (parent-web-operation application arguments (constantly nil))))))
            (check (= calls 1)))))
      checks)))

(serapeum/types:-> run-parent-web-authentication-tests () (integer 0))
(defun run-parent-web-authentication-tests ()
  "Retry one rejected credential, close each attempt, and keep public failures private."
  (let ((checks 0))
    (labels ((check (value)
               (assert value () "Web authentication check ~D failed." (1+ checks))
               (incf checks)))
      (dolist (case '((t 200 "run" 2 1 :success)
                      (t 401 "run" 2 1 :unavailable)
                      (nil 200 "run" 1 0 :unavailable)
                      (t 200 "gist" 1 0 :unavailable)))
        (destructuring-bind (refreshable next-status action attempts refreshes outcome) case
          (let* ((application    (parent-web-test--application :refreshable refreshable))
                 (profile        (gethash "web-profile" (parent-application-profiles application)))
                 (account        (gethash profile (parent-application-accounts application)))
                 (manager        (provider-account-credential-manager account))
                 (streams        nil)
                 (authorizations nil)
                 (*parent-web-open-function*
                   (lambda (url &key method headers content)
                     (declare (ignore url method content))
                     (check (every (lambda (stream) (not (open-stream-p stream))) streams))
                     (push (rest (assoc "Authorization" headers :test #'equalp)) authorizations)
                     (let* ((status (if streams next-status 401))
                            (stream (make-string-input-stream
                                     (if (= status 200) "{\"output\":\"renewed\"}" "private rejection"))))
                       (push stream streams)
                       (values stream status (make-hash-table :test #'equal))))))
            (flet ((request ()
                     (parent-web-operation
                      application
                      (if (equal action "run")
                          (parent-web-test--map "action" action "commands" "{}" "history" "[]")
                          (parent-web-test--map "action" action "url" "https://page.invalid/"))
                      (constantly nil))))
              (if (eq outcome ':success)
                  (check (equal (gethash "output" (request)) "renewed"))
                  (check (eq outcome (parent-web-test--code #'request)))))
            (check (= attempts (length streams)))
            (check (= refreshes (length (provider-executor-test-manager-refreshes manager))))
            (check (every (lambda (stream) (not (open-stream-p stream))) streams))
            (when (= attempts 2)
              (check (every #'stringp authorizations))
              (check (not (equal (first authorizations) (second authorizations))))))))
      checks)))

(serapeum/types:-> run-parent-web-tests () (integer 0))
(defun run-parent-web-tests ()
  "Check private credentials, public action fidelity, strict close, and refusal paths offline."
  (let ((checks      (+ (run-parent-web-history-tests) (run-parent-web-authentication-tests)))
        (application (parent-web-test--application))
        (commands    "{\"search_query\":[{\"q\":\"fixture\"}],\"image_query\":[{\"q\":\"image\"}],\"open\":[{\"ref_id\":\"turn0search0\"}],\"click\":[{\"ref_id\":\"turn0fetch0\",\"id\":2}],\"find\":[{\"ref_id\":\"turn0fetch0\",\"pattern\":\"word\"}],\"screenshot\":[{\"ref_id\":\"turn0fetch0\",\"pageno\":0}],\"finance\":[{\"ticker\":\"AMD\",\"type\":\"equity\"}],\"weather\":[{\"location\":\"Paris\"}],\"sports\":[{\"fn\":\"standings\",\"league\":\"nfl\"}],\"time\":[{\"utc_offset\":\"+03:00\"}],\"response_length\":\"long\"}"))
    (labels ((check (value)
               (assert value () "Web check ~D failed." (1+ checks))
               (incf checks)))
      (let ((opened nil) (request nil) (wire nil) (transport nil)
            (arguments (parent-web-test--map "action" "run" "commands" commands "history" "[]")))
        (let ((*parent-web-open-function*
                (lambda (url &key method headers content)
                  (setf opened url request (cl-rfc8628:json-decode content) wire content
                        transport (make-string-input-stream "{\"output\":\"public result\"}"))
                  (check (eq method ':post))
                  (check (assoc "Authorization" headers :test #'equalp))
                  (check (equal (rest (assoc "session-id" headers :test #'equalp)) "trusted-session"))
                  (values transport 200 (make-hash-table :test #'equal)))))
          (let ((result (parent-web-operation application arguments (constantly nil))))
            (check (equal (gethash "output" result) "public result"))
            (check (= (hash-table-count result) 1))))
        (check (equal opened "https://trusted.invalid/alpha/search"))
        (check (not (open-stream-p transport)))
        (check (equal (gethash "model" request) "trusted-model"))
        (check (equal (gethash "id" request) "trusted-session"))
        (check (search "\"external_web_access\":false" wire))
        (check (= (hash-table-count (gethash "commands" request)) 11))
        (check (not (gethash "headers" request)))
        (setf (gethash "endpoint" arguments) "https://agent.invalid/steal")
        (check (eq ':invalid-request
                   (parent-web-test--code
                    (lambda () (parent-web-operation application arguments (constantly nil)))))))
      (dolist (case '(("disabled" :permission-denied) ("broken" :unsupported)))
        (check (eq (second case)
                   (parent-web-test--code
                    (lambda ()
                      (parent-web-operation (parent-web-test--application :mode (first case))
                                            (parent-web-test--map "action" "run" "commands" "{}" "history" "[]")
                                            (constantly nil)))))))
      (check (eq ':unsupported
                 (parent-web-test--code
                  (lambda ()
                    (parent-web-operation (parent-web-test--application :family ':openai-api)
                                          (parent-web-test--map "action" "run" "commands" "{}" "history" "[]")
                                          (constantly nil))))))
      (dolist (case '((200 "{\"output\":\"\"}" :unavailable)
                      (500 "private provider body" :unavailable)
                      (200 "private non-JSON body" :unavailable)))
        (let ((transport (make-string-input-stream (second case))))
          (let ((*parent-web-open-function*
                  (lambda (&rest ignored)
                    (declare (ignore ignored))
                    (values transport (first case) (make-hash-table :test #'equal)))))
            (check (eq (third case)
                       (parent-web-test--code
                        (lambda ()
                          (parent-web-operation application
                                                (parent-web-test--map "action" "run" "commands" "{}" "history" "[]")
                                                (constantly nil)))))))
          (check (not (open-stream-p transport)))))
      (dolist (case '(("text/plain" "plain" "plain")
                      ("text/markdown; charset=utf-8" "# Heading" "# Heading")
                      ("text/html" "<h1>Heading</h1>" "Heading")))
        (let ((transport (make-string-input-stream (second case))))
          (let ((*parent-web-open-function*
                  (lambda (url &key method headers content)
                    (declare (ignore headers content))
                    (check (and (equal url "https://page.invalid/") (eq method ':get)))
                    (values transport 200 (parent-web-test--map "content-type" (first case))))))
            (check (search (third case)
                           (gethash "output"
                                    (parent-web-operation application
                                                          (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                                          (constantly nil))))))
          (check (not (open-stream-p transport)))))
      (let ((calls  0)
            (denied (parent-web-test--application :permission "sandbox")))
        (let ((*parent-web-open-function*
                (lambda (&rest ignored)
                  (declare (ignore ignored))
                  (incf calls)
                  (error "Denied network request reached transport"))))
          (check (eq ':permission-denied
                     (parent-web-test--code
                      (lambda ()
                        (parent-web-operation denied
                                              (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                              (constantly nil)))))))
        (check (zerop calls)))
      (dolist (deny-redirect '(nil t))
        (let* ((redirect-application (parent-web-test--application))
               (settings             (parent-configuration-get
                                      (parent-application-configuration redirect-application) ':settings))
               (urls                 nil)
               (streams              nil))
          (let ((*parent-web-open-function*
                  (lambda (url &key method headers content)
                    (declare (ignore method headers content))
                    (push url urls)
                    (let ((stream (make-string-input-stream "redirect result")))
                      (push stream streams)
                      (when (and deny-redirect (= (length urls) 1))
                        (setf (getf settings :permission-mode) "sandbox"))
                      (if (= (length urls) 1)
                          (values stream 302 (parent-web-test--map "location" "/next"))
                          (values stream 200 (parent-web-test--map "content-type" "text/plain")))))))
            (flet ((request ()
                     (parent-web-operation redirect-application
                                           (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                           (constantly nil))))
              (if deny-redirect
                  (check (eq ':permission-denied (parent-web-test--code #'request)))
                  (check (equal (gethash "output" (request)) "redirect result")))))
          (check (equal (reverse urls)
                        (if deny-redirect
                            '("https://page.invalid/")
                            '("https://page.invalid/" "https://page.invalid/next"))))
          (check (every (lambda (stream) (not (open-stream-p stream))) streams))))
      (dolist (case '(("application/json" "{}" :unavailable)
                      ("text/plain" "oversize" :resource-limit)))
        (let* ((*parent-web-body-limit* 4)
               (stream                  (make-string-input-stream (second case)))
               (*parent-web-open-function*
                 (lambda (&rest ignored)
                   (declare (ignore ignored))
                   (values stream 200 (parent-web-test--map "content-type" (first case))))))
          (check (eq (third case)
                     (parent-web-test--code
                      (lambda ()
                        (parent-web-operation application
                                              (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                              (constantly nil))))))
          (check (not (open-stream-p stream)))))
      (dolist (url '("file:///private" "https://user:password@page.invalid/" "not a URL"))
        (check (eq ':invalid-request
                   (parent-web-test--code
                    (lambda ()
                      (parent-web-operation application (parent-web-test--map "action" "gist" "url" url)
                                            (constantly nil)))))))
      (let ((*parent-web-open-function* (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         (error "transport details must not cross the boundary"))))
        (check (eq ':unavailable
                   (parent-web-test--code
                    (lambda ()
                      (parent-web-operation application
                                            (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                            (constantly nil))))))
        (check (eq ':cancelled
                   (parent-web-test--code
                    (lambda ()
                      (parent-web-operation application
                                            (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                                            (constantly t)))))))
      (let ((transport (make-instance 'provider-executor-test-stream)))
        (let ((*parent-web-open-function*
                (lambda (&rest ignored)
                  (declare (ignore ignored))
                  (values transport 200 (parent-web-test--map "content-type" "text/plain")))))
          (check (eq ':cancelled
                     (parent-web-test--code
                      (lambda ()
                        (parent-web-operation
                         application (parent-web-test--map "action" "gist" "url" "https://page.invalid/")
                         (lambda ()
                           (plusp (sb-thread:semaphore-count
                                   (provider-executor-test-stream-read-started transport))))))))))
        (check (not (open-stream-p transport)))))
    checks))
