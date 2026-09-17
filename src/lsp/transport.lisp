(in-package #:autolith)

;;;; -- Bounded Stdio JSON-RPC --

(defparameter *lsp-maximum-header-bytes* 8192
  "Maximum bytes in a Content-Length header block.")

(defparameter *lsp-maximum-body-bytes* (* 4 1024 1024)
  "Maximum bytes in an incoming or outgoing JSON document.")

(defparameter *lsp-maximum-stderr-characters* 8192
  "Maximum retained diagnostic characters from server stderr.")

(defparameter *lsp-maximum-callbacks* 128
  "Maximum queued notifications and requests from one server.")

(define-condition lsp-error (autolith-error)
  ((message :initarg :message :reader lsp-error-message
            :documentation "Bounded explanation of the protocol failure."))
  (:report (lambda (condition stream) (write-string (lsp-error-message condition) stream)))
  (:documentation "A language server protocol or lifecycle failure."))

(define-condition lsp-rpc-error (lsp-error)
  ((code :initarg :code :reader lsp-rpc-error-code
         :documentation "JSON-RPC error code.")
   (data :initarg :data :initform nil :reader lsp-rpc-error-data
         :documentation "Optional structured server error data."))
  (:documentation "A remote JSON-RPC request failure."))

(define-condition lsp-timeout (lsp-error)
  ((request-id :initarg :request-id :initform nil :reader lsp-timeout-request-id
               :documentation "Request identifier when the timeout belongs to a request."))
  (:documentation "A bounded transport operation exceeded its deadline."))

(-> lsp--octets-to-string ((vector (unsigned-byte 8))) string)
(defun lsp--octets-to-string (octets)
  "Decode strict UTF-8, reporting malformed bytes as a protocol error."
  (handler-case (sb-ext:octets-to-string octets :external-format ':utf-8)
    (error () (error 'lsp-error :message "Invalid UTF-8 in LSP message."))))

(-> lsp--string-to-octets (string) (vector (unsigned-byte 8)))
(defun lsp--string-to-octets (string)
  "Encode one string as UTF-8 octets."
  (sb-ext:string-to-octets string :external-format ':utf-8))

(-> lsp--decode-message (string) json-object)
(defun lsp--decode-message (text)
  "Decode exactly one JSON object after bounding nesting before parser recursion."
  (let ((depth 0) (string-p nil) (escaped-p nil))
    (loop for character across text
          do (cond
               (escaped-p (setf escaped-p nil))
               ((and string-p (char= character #\\)) (setf escaped-p t))
               ((char= character #\") (setf string-p (not string-p)))
               ((not string-p)
                (case character
                  ((#\{ #\[)
                   (when (> (incf depth) 64)
                     (error 'lsp-error :message "LSP JSON nesting exceeds 64 levels.")))
                  ((#\} #\]) (decf depth)))))))
  (handler-case
      (progn
        (unless (json-object-source-p text)
          (error 'lsp-error :message "LSP message must contain exactly one JSON object."))
        (json-decode text))
    (error () (error 'lsp-error :message "Malformed LSP JSON object."))))

(-> lsp-read-message (stream) (option json-object))
(defun lsp-read-message (stream)
  "Read one framed JSON object; return NIL only for EOF before the first header byte."
  (let ((header (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (unless byte
          (if (zerop (length header))
              (return-from lsp-read-message nil)
              (error 'lsp-error :message "Unexpected EOF in LSP headers.")))
        (unless (< byte 128)
          (error 'lsp-error :message "LSP headers must contain ASCII bytes."))
        (vector-push-extend byte header)
        (when (> (length header) *lsp-maximum-header-bytes*)
          (error 'lsp-error :message "LSP headers exceed the configured limit."))
        (when (and (>= (length header) 4)
                   (equalp (subseq header (- (length header) 4)) #(13 10 13 10)))
          (return))))
    (let ((length-text nil))
      (dolist (line (uiop:split-string (lsp--octets-to-string header) :separator '(#\Newline)))
        (let* ((line (string-trim '(#\Return) line))
               (colon (position #\: line)))
          (unless (zerop (length line))
            (unless colon (error 'lsp-error :message "Malformed LSP header."))
            (when (string-equal (subseq line 0 colon) "Content-Length")
              (when length-text (error 'lsp-error :message "Duplicate LSP Content-Length header."))
              (setf length-text (string-trim '(#\Space #\Tab) (subseq line (1+ colon))))))))
      (unless (and length-text (plusp (length length-text))
                   (<= (length length-text) 10) (every #'digit-char-p length-text))
        (error 'lsp-error :message "Missing or invalid LSP Content-Length."))
      (let ((length (parse-integer length-text)))
        (unless (<= 1 length *lsp-maximum-body-bytes*)
          (error 'lsp-error :message "LSP body exceeds the configured limit."))
        (let ((body (make-array length :element-type '(unsigned-byte 8))))
          (loop with position = 0 while (< position length)
                do (let ((end (read-sequence body stream :start position)))
                     (when (= end position) (error 'lsp-error :message "Unexpected EOF in LSP message body."))
                     (setf position end)))
          (lsp--decode-message (lsp--octets-to-string body)))))))

(-> lsp-write-message (stream json-object) null)
(defun lsp-write-message (stream message)
  "Write a JSON object with its UTF-8 octet length and literal CRLF delimiters."
  (let ((body (json-encode-utf8 message)))
    (when (> (length body) *lsp-maximum-body-bytes*)
      (error 'lsp-error :message "Outgoing LSP message exceeds the configured limit."))
    (write-sequence (lsp--string-to-octets
                     (format nil "Content-Length: ~D~C~C~C~C" (length body)
                             #\Return #\Newline #\Return #\Newline)) stream)
    (write-sequence body stream)
    (finish-output stream))
  nil)

(defclass lsp-pending-request ()
  ((condition :initform (make-condition-variable) :reader lsp-pending-condition
              :documentation "Per-request wakeup, protected by the transport state lock.")
   (response :initform nil :accessor lsp-pending-response
             :documentation "The matching JSON-RPC response."))
  (:documentation "One admitted request, registered before sending its bytes."))

(defclass lsp-transport ()
  ((process :initarg :process :accessor lsp-transport-process :documentation "Owned UIOP subprocess.")
   (input :initarg :input :reader lsp-transport-input :documentation "Binary server stdout stream.")
   (output :initarg :output :reader lsp-transport-output :documentation "Binary server stdin stream.")
   (error-output :initarg :error-output :reader lsp-transport-error-output :documentation "Binary stderr drain.")
   (request-handler :initarg :request-handler :reader lsp-transport-request-handler :documentation "Server request callback.")
   (notification-handler :initarg :notification-handler :reader lsp-transport-notification-handler :documentation "Server notification callback.")
   (lock :initform (make-lock "LSP transport") :reader lsp-transport-lock :documentation "Response and callback state lock.")
   (write-lock :initform (make-lock "LSP writer") :reader lsp-transport-write-lock :documentation "Serializes complete framed writes.")
   (next-id :initform 0 :accessor lsp-transport-next-id :documentation "Monotonic outgoing request identifier.")
   (pending :initform (make-hash-table :test #'eql) :reader lsp-transport-pending :documentation "Admitted request waiters.")
   (live-p :initform t :accessor lsp-transport-live-p :documentation "False after any terminal transport failure.")
   (failure :initform nil :accessor lsp-transport-failure :documentation "First terminal failure condition.")
   (stderr :initform "" :accessor lsp-transport-stderr :documentation "Bounded printable stderr tail.")
   (callbacks :initform nil :accessor lsp-transport-callbacks :documentation "Bounded ordered server callback queue.")
   (callback-condition :initform (make-condition-variable) :reader lsp-transport-callback-condition :documentation "Callback queue wakeup.")
   (threads :initform nil :accessor lsp-transport-threads :documentation "Owned reader, stderr, and callback threads."))
  (:documentation "Concurrent, bounded stdio JSON-RPC transport with ordered callbacks."))

(-> lsp-transport--fail (lsp-transport condition) null)
(defun lsp-transport--fail (transport condition)
  "Retain the first terminal failure and wake every admitted request."
  (with-lock-held ((lsp-transport-lock transport))
    (setf (lsp-transport-live-p transport) nil)
    (unless (lsp-transport-failure transport) (setf (lsp-transport-failure transport) condition))
    (maphash (lambda (id pending)
               (declare (ignore id)) (condition-notify (lsp-pending-condition pending)))
             (lsp-transport-pending transport))
    (condition-notify (lsp-transport-callback-condition transport)))
  nil)

(-> lsp-transport--send (lsp-transport json-object &key (:timeout real)) null)
(defun lsp-transport--send (transport message &key (timeout 5))
  "Bound both writer-lock acquisition and pipe backpressure by TIMEOUT."
  (handler-case
      (sb-ext:with-timeout timeout
        (with-lock-held ((lsp-transport-write-lock transport))
          (unless (lsp-transport-live-p transport)
            (error 'lsp-error :message "LSP transport is closed."))
          (lsp-write-message (lsp-transport-output transport) message)))
    (sb-ext:timeout ()
      (let ((condition (make-condition 'lsp-timeout :message "LSP write timed out.")))
        (lsp-transport--fail transport condition) (error condition)))
    (error (condition)
      (lsp-transport--fail transport condition) (error condition)))
  nil)

(-> lsp--dispatch (lsp-transport json-object) null)
(defun lsp--dispatch (transport message)
  "Route matching responses directly; queue server callbacks in wire order."
  (unless (json-string= (json-get message "jsonrpc") "2.0")
    (error 'lsp-error :message "Invalid LSP JSON-RPC version."))
  (multiple-value-bind (id id-p) (json-get-present message "id")
    (let ((method (json-get message "method")))
      (with-lock-held ((lsp-transport-lock transport))
        (cond
          ((stringp method)
           (when (>= (length (lsp-transport-callbacks transport)) *lsp-maximum-callbacks*)
             (error 'lsp-error :message "LSP callback queue exceeded its limit."))
           (setf (lsp-transport-callbacks transport)
                 (nconc (lsp-transport-callbacks transport) (list message)))
           (condition-notify (lsp-transport-callback-condition transport)))
          ((and id-p (null method))
           (let ((pending (gethash id (lsp-transport-pending transport))))
             (when pending
               (setf (lsp-pending-response pending) message)
               (condition-notify (lsp-pending-condition pending)))))
          (t
           (error 'lsp-error :message "Invalid LSP request or response."))))))
  nil)

(-> lsp-transport--callback (lsp-transport json-object) null)
(defun lsp-transport--callback (transport message)
  "Run one server callback outside transport locks and reply to every request."
  (multiple-value-bind (id id-p) (json-get-present message "id")
    (let ((method (json-get message "method")) (params (json-get message "params")))
      (if id-p
          (let ((response
                  (handler-case
                      (json-object "jsonrpc" "2.0" "id" id "result"
                                   (funcall (lsp-transport-request-handler transport) method params))
                    (error (condition)
                      (json-object "jsonrpc" "2.0" "id" id "error"
                                   (json-object "code" (if (typep condition 'lsp-rpc-error)
                                                          (lsp-rpc-error-code condition) -32603)
                                                "message" (if (typep condition 'lsp-error)
                                                              (lsp-error-message condition)
                                                              "LSP client callback failed.")))))))
            (lsp-transport--send transport response))
          (funcall (lsp-transport-notification-handler transport) method params))))
  nil)

(-> lsp-transport--callback-loop (lsp-transport) null)
(defun lsp-transport--callback-loop (transport)
  "Drain ordered callbacks without blocking the response reader."
  (handler-case
      (loop
        (let ((message
                (with-lock-held ((lsp-transport-lock transport))
                  (loop while (and (lsp-transport-live-p transport)
                                   (null (lsp-transport-callbacks transport)))
                        do (condition-wait (lsp-transport-callback-condition transport)
                                           (lsp-transport-lock transport)))
                  (unless (lsp-transport-live-p transport) (return-from lsp-transport--callback-loop nil))
                  (pop (lsp-transport-callbacks transport)))))
          (lsp-transport--callback transport message)))
    (error (condition) (lsp-transport--fail transport condition)))
  nil)

(-> lsp--reader-loop (lsp-transport) null)
(defun lsp--reader-loop (transport)
  "Decode responses until EOF, propagating terminal failure to all waiters."
  (handler-case
      (loop while (lsp-transport-live-p transport)
            for message = (lsp-read-message (lsp-transport-input transport))
            do (unless message (error 'lsp-error :message "Language server closed stdout."))
               (lsp--dispatch transport message))
    (error (condition) (lsp-transport--fail transport condition)))
  nil)

(-> lsp--stderr-loop (lsp-transport) null)
(defun lsp--stderr-loop (transport)
  "Drain server stderr while retaining only a bounded printable tail."
  (handler-case
      (loop for byte = (read-byte (lsp-transport-error-output transport) nil nil)
            while byte
            do (with-lock-held ((lsp-transport-lock transport))
                 (let ((text (concatenate 'string (lsp-transport-stderr transport)
                                          (string (if (or (<= 32 byte 126) (member byte '(9 10 13)))
                                                      (code-char byte) #\?)))))
                   (setf (lsp-transport-stderr transport)
                         (subseq text (max 0 (- (length text) *lsp-maximum-stderr-characters*)))))))
    (error () nil))
  nil)

(-> lsp-transport-open (&key (:command string) (:arguments list) (:directory pathname)
                            (:request-handler function) (:notification-handler function)) lsp-transport)
(defun lsp-transport-open (&key command arguments directory request-handler notification-handler)
  "Start a trusted local command using binary pipes and three bounded worker threads."
  (let* ((process (uiop:launch-program (cons command arguments) :directory directory
                                     :input ':stream :output ':stream :error-output ':stream
                                     :element-type '(unsigned-byte 8)))
         (transport (make-instance 'lsp-transport :process process
                                    :input (uiop:process-info-output process)
                                    :output (uiop:process-info-input process)
                                    :error-output (uiop:process-info-error-output process)
                                    :request-handler request-handler :notification-handler notification-handler))
         (complete-p nil))
    (unwind-protect
         (progn
           (dolist (function (list #'lsp--reader-loop #'lsp--stderr-loop #'lsp-transport--callback-loop))
             (let ((function function))
               (push (make-thread (lambda () (funcall function transport)) :name "LSP transport")
                     (lsp-transport-threads transport))))
           (setf complete-p t)
           transport)
      (unless complete-p (lsp-transport-close transport)))))

(-> lsp-transport-request (lsp-transport string t &key (:timeout real)) t)
(defun lsp-transport-request (transport method params &key (timeout 30))
  "Send METHOD and await its matching reply, including writes in the total deadline."
  (let* ((pending (make-instance 'lsp-pending-request))
         (id (with-lock-held ((lsp-transport-lock transport))
               (when (>= (hash-table-count (lsp-transport-pending transport)) 128)
                 (error 'lsp-error :message "Too many concurrent LSP requests."))
               (let ((id (incf (lsp-transport-next-id transport))))
                 (setf (gethash id (lsp-transport-pending transport)) pending) id)))
         (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (unwind-protect
         (progn
           (lsp-transport--send transport (json-object "jsonrpc" "2.0" "id" id "method" method "params" params)
                                :timeout timeout)
           (let ((reply
                   (with-lock-held ((lsp-transport-lock transport))
                     (loop
                       (when (lsp-pending-response pending) (return (lsp-pending-response pending)))
                       (unless (lsp-transport-live-p transport)
                         (error (or (lsp-transport-failure transport)
                                    (make-condition 'lsp-error :message "LSP transport is closed."))))
                       (let ((remaining (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
                         (when (<= remaining 0) (return nil))
                         (condition-wait (lsp-pending-condition pending) (lsp-transport-lock transport)
                                          :timeout remaining))))))
             (unless reply
               (ignore-errors (lsp-transport--send transport
                                (json-object "jsonrpc" "2.0" "method" "$/cancelRequest"
                                             "params" (json-object "id" id)) :timeout 0.1))
               (error 'lsp-timeout :message "LSP request timed out." :request-id id))
             (let ((remote-error (json-get reply "error")))
               (when remote-error
                 (unless (hash-table-p remote-error) (error 'lsp-error :message "Malformed LSP error response."))
                 (error 'lsp-rpc-error :message (json-get remote-error "message" "Language server request failed.")
                                      :code (json-get remote-error "code" -32603) :data (json-get remote-error "data")))
               (multiple-value-bind (result present-p) (json-get-present reply "result")
                 (unless present-p (error 'lsp-error :message "LSP response has neither result nor error."))
                 result))))
      (with-lock-held ((lsp-transport-lock transport)) (remhash id (lsp-transport-pending transport))))))

(-> lsp-transport-notify (lsp-transport string t) null)
(defun lsp-transport-notify (transport method params)
  "Send a notification with bounded pipe backpressure."
  (lsp-transport--send transport (json-object "jsonrpc" "2.0" "method" method "params" params)))

(-> lsp-transport-close (lsp-transport) null)
(defun lsp-transport-close (transport)
  "Close pipes, terminate a stubborn child, and bound every wait and thread join."
  (lsp-transport--fail transport (make-condition 'lsp-error :message "LSP transport closed."))
  (let ((process (lsp-transport-process transport)))
    (when process
      (ignore-errors (close (lsp-transport-output transport) :abort t))
      (flet ((wait-briefly ()
               (let ((deadline (+ (get-internal-real-time) internal-time-units-per-second)))
                 (loop while (and (uiop:process-alive-p process)
                                  (< (get-internal-real-time) deadline))
                       do (sleep 0.01)))))
        (wait-briefly)
        (when (uiop:process-alive-p process)
          (ignore-errors (uiop:terminate-process process))
          (wait-briefly))
        (when (uiop:process-alive-p process)
          (ignore-errors (uiop:terminate-process process :urgent t))))
      (handler-case (sb-ext:with-timeout 1 (uiop:wait-process process))
        (sb-ext:timeout () (error 'lsp-error :message "Could not reap language server after termination.")))
      (setf (lsp-transport-process transport) nil)))
  (dolist (stream (list (lsp-transport-input transport) (lsp-transport-error-output transport)))
    (ignore-errors (close stream :abort t)))
  (dolist (thread (lsp-transport-threads transport))
    (unless (eq thread (current-thread))
      (sb-thread:join-thread thread :timeout 0.5 :default nil)
      (when (thread-alive-p thread)
        (destroy-thread thread)
        (sb-thread:join-thread thread :timeout 0.5 :default nil))))
  (setf (lsp-transport-threads transport) nil (lsp-transport-callbacks transport) nil)
  nil)

(-> lsp-transport-detach (lsp-transport) null)
(defun lsp-transport-detach (transport)
  "Close inherited descriptors without flush, locks, process signals, waits, or callbacks."
  (dolist (stream (list (lsp-transport-input transport) (lsp-transport-output transport)
                        (lsp-transport-error-output transport)))
    (ignore-errors (close stream :abort t)))
  (setf (lsp-transport-process transport) nil (lsp-transport-threads transport) nil
        (lsp-transport-callbacks transport) nil (lsp-transport-live-p transport) nil)
  (clrhash (lsp-transport-pending transport))
  nil)
