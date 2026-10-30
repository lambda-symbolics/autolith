;;;; -- Trusted Web Credentials and Transport --

(in-package #:autolith-supervisor)

(defparameter *parent-web-body-limit* 2097152
  "Maximum response characters before conversion. Oversize responses fail without truncation.")

(serapeum/types:-> parent-web--reject (keyword) nil)
(defun parent-web--reject (code)
  "Signal a fixed public error without private transport data."
  (error 'parent-application-operation-error
         :code (case code
                 ((:web-disabled :denied)
                  ':permission-denied)
                 (:web-unsupported
                  ':unsupported)
                 ((:web-http-failed :invalid-web-response :web-content-type
                   :web-redirect-failed :web-failed)
                  ':unavailable)
                 (otherwise
                  code))))

(serapeum/types:-> parent-web--open
    (string &key (:method keyword) (:headers list) (:content t))
    (values t t t &optional))
(defun parent-web--open (url &key (method ':get) headers content)
  "Open an unpooled parent HTTP stream with a bounded deadline and no redirects."
  (dexador:request url :method method :headers headers :content content
                       :want-stream t :force-string t :keep-alive nil
                       :use-connection-pool nil :max-redirects 0
                       :connect-timeout 15 :read-timeout 60))

(defparameter *parent-web-open-function* #'parent-web--open
  "Trusted web transport. Tests bind a fake opener; the child cannot set it.")

(serapeum/types:-> parent-web--read (stream hash-table) string)
(defun parent-web--read (stream headers)
  "Read bounded text, including the charset of a binary HTTP response."
  (if (subtypep (stream-element-type stream) 'character)
      (with-output-to-string (output)
        (loop for character = (read-char stream nil nil)
              for count from 0 while character
              do (when (>= count *parent-web-body-limit*) (parent-web--reject ':resource-limit))
                 (write-char character output)))
      (let* ((bytes   (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (type    (or (gethash "content-type" headers) ""))
             (charset (loop for part in (rest (uiop:split-string type :separator ";"))
                            for pair = (uiop:split-string part :separator "=")
                            when (equalp (string-trim '(#\Space #\Tab) (first pair)) "charset")
                              return (string-trim '(#\Space #\Tab #\") (second pair)))))
        (loop for byte = (read-byte stream nil nil) while byte
              do (when (>= (length bytes) *parent-web-body-limit*) (parent-web--reject ':resource-limit))
                 (vector-push-extend byte bytes))
        (babel:octets-to-string bytes :encoding (if charset (intern (string-upcase charset) '#:keyword) ':utf-8)))))

(serapeum/types:-> parent-web--request
    (provider-http-state string &key (:method keyword) (:headers list) (:content t)
                                    (:redirect-p boolean))
    (values string integer t &optional))
(defun parent-web--request (state url &key (method ':get) headers content redirect-p)
  "Own response cleanup on success, refusal, cancellation, and decoding failure."
  (unwind-protect
       (multiple-value-bind (stream status response-headers)
           (provider-http--open
            state
            (lambda ()
              (provider-http--http-values
               (lambda ()
                 (funcall *parent-web-open-function* url :method method
                                                        :headers headers :content content))))
            #'provider-http--close-stream)
         (unless (and (streamp stream) (integerp status))
           (parent-web--reject ':invalid-web-response))
         (unless (or (<= 200 status 299)
                     (and redirect-p (member status '(301 302 303 307 308))))
           (parent-web--reject ':web-http-failed))
         (values (if (<= 200 status 299) (parent-web--read stream response-headers) "")
                 status response-headers))
    (provider-http--close state #'provider-http--close-stream)))

(serapeum/types:-> parent-web--with-lifecycle (function function) t)
(defun parent-web--with-lifecycle (cancelled function)
  "Bound web and credential refresh work, join the cancellation watcher, and close HTTP."
  (let* ((state    (make-instance 'provider-http-state))
         (owner    (bordeaux-threads:current-thread))
         (lock     (bordeaux-threads:make-lock "parent web lifetime"))
         (done     nil)
         (deadline (+ (session-runtime--clock) 60))
         (watcher  nil))
    (when (funcall cancelled)
      (parent-web--reject ':cancelled))
    (setf watcher
          (bordeaux-threads:make-thread
           (lambda ()
             (loop
               (when (bordeaux-threads:with-lock-held (lock) done)
                 (return))
               (when (or (funcall cancelled) (>= (session-runtime--clock) deadline))
                 (bordeaux-threads:with-lock-held (lock)
                   (unless done
                     (bordeaux-threads:interrupt-thread
                      owner (lambda () (parent-web--reject ':cancelled)))))
                 (return))
               (sleep 0.01)))
           :name "parent web cancellation"))
    (unwind-protect
         (cl-llm-provider-api:provider-call-with-response-deadline
          60 (lambda () (funcall function state deadline)))
      (bordeaux-threads:with-lock-held (lock) (setf done t))
      (bordeaux-threads:join-thread watcher)
      (provider-http--close state #'provider-http--close-stream))))

(serapeum/types:-> parent-web--fields-p (hash-table list) boolean)
(defun parent-web--fields-p (fields keys)
  "Accept only the fields for the selected fixed operation."
  (not (null (loop for key being the hash-keys of fields
                  always (member key keys :test #'equal)))))

(serapeum/types:-> parent-web--commands (string) hash-table)
(defun parent-web--commands (text)
  "Validate the fixed web command names without narrowing provider action schemas."
  (let ((commands (cl-rfc8628:json-decode text)))
    (unless (and (hash-table-p commands)
                 (parent-web--fields-p
                  commands '("search_query" "image_query" "open" "click" "find" "screenshot"
                             "finance" "weather" "sports" "time" "response_length"))
                 (loop for key being the hash-keys of commands using (hash-value value)
                       always (if (equal key "response_length")
                                  (member value '("short" "medium" "long") :test #'equal)
                                  (and (vectorp value) (not (stringp value))
                                       (<= (length value) 64)
                                       (every #'hash-table-p value)))))
      (parent-web--reject ':invalid-request))
    commands))

(serapeum/types:-> parent-web--history (string) vector)
(defun parent-web--history (text)
  "Accept only inert user and assistant text context, with no tool or provider state."
  (let ((history (cl-rfc8628:json-decode text)))
    (unless (and (vectorp history) (not (stringp history)) (<= (length history) 128)
                 (every (lambda (item)
                          (and (hash-table-p item)
                               (parent-web--fields-p item '("type" "role" "content"))
                               (equal (gethash "type" item) "message")
                               (member (gethash "role" item) '("user" "assistant") :test #'equal)
                               (vectorp (gethash "content" item))
                               (every (lambda (part)
                                        (and (hash-table-p part)
                                             (parent-web--fields-p part '("type" "text"))
                                             (member (gethash "type" part)
                                                     '("input_text" "output_text") :test #'equal)
                                             (stringp (gethash "text" part))))
                                      (gethash "content" item)))) history))
      (parent-web--reject ':invalid-request))
    history))

(serapeum/types:-> parent-web--search
    (parent-application hash-table &key (:state provider-http-state) (:deadline real)) string)
(defun parent-web--search (application arguments &key state deadline)
  "Select endpoint, model, credentials, and access mode only from trusted parent state."
  (unless (and (parent-web--fields-p arguments '("action" "commands" "history"))
               (stringp (gethash "commands" arguments))
               (stringp (gethash "history" arguments)))
    (parent-web--reject ':invalid-request))
  (let* ((configuration (parent-application-configuration application))
         (mode          (getf (parent-configuration-get configuration ':settings) :web-search-mode "cached"))
         (profile       (gethash (parent-configuration-get configuration ':profile-id)
                                 (parent-application-profiles application)))
         (account       (gethash profile (parent-application-accounts application))))
    (when (equal mode "disabled")
      (parent-web--reject ':web-disabled))
    (unless (and (typep account 'provider-account)
                 (member (provider-account-family account) '(:codex :grok))
                 (member mode '("cached" "indexed" "live") :test #'equal))
      (parent-web--reject ':web-unsupported))
    (let* ((endpoint   (string-right-trim "/" (provider-account-endpoint account)))
           (session-id (parent-application-session-id application))
           (request
             (let ((data     (make-hash-table :test #'equal))
                   (settings (make-hash-table :test #'equal)))
               (setf (gethash "allowed_callers" settings) #("direct")
                     (gethash "external_web_access" settings)
                     (cond ((equal mode "live")
                            t)
                           ((equal mode "indexed")
                            "indexed")
                           (t
                            yason:false))
                     (gethash "id" data) session-id
                     (gethash "model" data) (model-profile-model profile)
                     (gethash "input" data) (parent-web--history (gethash "history" arguments))
                     (gethash "commands" data) (parent-web--commands (gethash "commands" arguments))
                     (gethash "settings" data) settings
                     (gethash "max_output_tokens" data) 4000)
               data))
           (executor   (make-instance 'chatgpt-executor :clock #'session-runtime--clock
                                      :closer                  #'provider-http--close-stream))
           (invocation (make-instance 'model-invocation :deadline deadline)))
      (unless (uiop:string-suffix-p endpoint "/responses")
        (parent-web--reject ':web-unsupported))
      (provider-executor-with-credentials
       account :executor executor :state state :invocation invocation
       :function
       (lambda (credentials)
         (let ((*provider-family-http-accept* "application/json"))
           (multiple-value-bind (body status headers)
               (parent-web--request
                state (concatenate 'string (subseq endpoint 0 (- (length endpoint) 10)) "/alpha/search")
                :method  ':post
                :headers (provider-family--headers account credentials :session-id session-id
                                                                     :user-agent "autolith" :request request)
                :content (cl-rfc8628:json-encode request))
             (declare (ignore status headers))
             (let ((output (gethash "output" (cl-rfc8628:json-decode body))))
               (unless (and (stringp output) (plusp (length output)))
                 (parent-web--reject ':invalid-web-response))
               output))))))))

(serapeum/types:-> parent-web--url (string) string)
(defun parent-web--url (url)
  "Accept an absolute HTTP URL, including each redirect target."
  (let ((uri (quri:uri url)))
    (unless (and (member (quri:uri-scheme uri) '("http" "https") :test #'equal)
                 (quri:uri-host uri) (plusp (length (quri:uri-host uri)))
                 (not (quri:uri-userinfo uri)) (not (find #\Null url)))
      (parent-web--reject ':invalid-request)))
  url)

(serapeum/types:-> parent-web--gist
    (parent-application hash-table &key (:state provider-http-state) (:cancelled function)) string)
(defun parent-web--gist (application arguments &key state cancelled)
  "Retrieve text or HTML, with bounded redirects and parent-owned HTTP cleanup."
  (unless (and (parent-web--fields-p arguments '("action" "url"))
               (stringp (gethash "url" arguments)))
    (parent-web--reject ':invalid-request))
  (loop with url = (gethash "url" arguments)
        for redirects from 0 to 10
        do (parent-web--url url)
           (let ((request (make-hash-table :test #'equal)))
             (setf (gethash "url" request) url)
             (unless (parent-application-authorize-tool application "web_extra.gist" request
                                                       :cancelled cancelled)
               (parent-web--reject ':permission-denied)))
           (multiple-value-bind (body status headers)
               (parent-web--request
                state url :redirect-p t :headers
                '(("Accept" . "text/markdown, text/plain;q=0.9, text/html;q=0.9, application/xhtml+xml;q=0.9")))
             (if (<= 200 status 299)
                 (let ((type (string-downcase
                              (string-trim '(#\Space #\Tab)
                                           (first (uiop:split-string
                                                   (or (gethash "content-type" headers) "")
                                                   :separator ";"))))))
                   (return
                     (cond
                       ((member type '("text/markdown" "text/x-markdown" "application/markdown" "text/plain")
                                :test #'equal)
                        body)
                       ((member type '("text/html" "application/xhtml+xml") :test #'equal)
                        (fetch-gist:markdown-from-string body))
                       (t
                        (parent-web--reject ':web-content-type)))))
                 (let ((location (gethash "location" headers)))
                   (unless (and (stringp location) (< redirects 10))
                     (parent-web--reject ':web-redirect-failed))
                   (setf url (quri:render-uri (quri:merge-uris location url))))))))

(serapeum/types:-> parent-web-operation (parent-application hash-table function) hash-table)
(defun parent-web-operation (application arguments cancelled-p)
  "Execute one fixed, scope-admitted web request without exposing private failures."
  (handler-case
      (parent-web--with-lifecycle
       cancelled-p
       (lambda (state deadline)
         (let* ((output
                  (cond
                    ((equal (gethash "action" arguments) "run")
                     (parent-web--search application arguments :state state :deadline deadline))
                    ((equal (gethash "action" arguments) "gist")
                     (parent-web--gist application arguments :state state :cancelled cancelled-p))
                    (t
                     (parent-web--reject ':invalid-request))))
                (result (make-hash-table :test #'equal)))
           (when (funcall cancelled-p)
             (parent-web--reject ':cancelled))
           (when (> (length (babel:string-to-octets output :encoding ':utf-8)) 3145728)
             (parent-web--reject ':resource-limit))
           (setf (gethash "output" result) output)
           result)))
    (parent-application-operation-error (condition)
      (error condition))
    (error ()
      (parent-web--reject ':web-failed))))
