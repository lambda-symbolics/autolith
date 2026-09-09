(in-package #:autolith)

;;;; -- ACP JSON-RPC Transport --

;; The Agent Client Protocol (ACP) exchanges JSON-RPC 2.0 messages over
;; stdio, one message per line, in UTF-8 with no embedded newlines. This
;; file owns framing, correlation, and JSON-RPC error mapping for the agent
;; side of that transport. Session method handling lives in acp/session.lisp.

;;;; -- JSON-RPC Error Codes --

(defparameter *acp-parse-error-code* -32700
  "The JSON-RPC code for a message that failed to parse.")

(defparameter *acp-invalid-request-code* -32600
  "The JSON-RPC code for a message that is not a valid request.")

(defparameter *acp-method-not-found-code* -32601
  "The JSON-RPC code for a request naming an unknown method.")

(defparameter *acp-invalid-params-code* -32602
  "The JSON-RPC code for a request with invalid parameters.")

(defparameter *acp-internal-error-code* -32603
  "The JSON-RPC code for an internal failure while handling a request.")

;;;; -- Conditions --

(define-condition acp-method-error (autolith-error)
  ((code
    :initarg :code
    :reader acp-method-error-code
    :type integer
    :documentation "The JSON-RPC error code for the reply.")
   (data
    :initarg :data
    :initform nil
    :reader acp-method-error-data
    :type t
    :documentation "The optional JSON payload accompanying the reply."))
  (:documentation
   "A request handler failure that maps to one JSON-RPC error reply.")
  (:report (lambda (condition stream)
             (format stream "ACP request failed with JSON-RPC code ~D: ~A"
                     (acp-method-error-code condition)
                     (autolith-error-message condition)))))

(define-condition acp-remote-error (autolith-error)
  ((code
    :initarg :code
    :reader acp-remote-error-code
    :type integer
    :documentation "The JSON-RPC error code the peer returned.")
   (data
    :initarg :data
    :initform nil
    :reader acp-remote-error-data
    :type t
    :documentation "The optional JSON payload the peer returned."))
  (:documentation
   "The peer rejected one outbound ACP request with an error reply.")
  (:report (lambda (condition stream)
             (format stream "ACP request rejected with JSON-RPC code ~D: ~A"
                     (acp-remote-error-code condition)
                     (autolith-error-message condition)))))

(define-condition acp-connection-closed-error (autolith-error)
  ()
  (:documentation "The ACP connection ended before one request finished."))

;;;; -- Connection State --

(defstruct (acp-pending-request
            (:constructor acp-pending-request-create (condition-variable)))
  "State of one outbound ACP request waiting for its response."
  (condition-variable)
  (done-p nil :type boolean)
  (result nil :type t)
  (error-code nil :type (option integer))
  (error-message nil :type (option string))
  (error-data nil :type t)
  (closed-p nil :type boolean))

(defclass acp-connection ()
  ((input-stream
    :initarg :input-stream
    :reader acp-connection-input-stream
    :type stream
    :documentation "The character stream carrying peer messages.")
   (output-stream
    :initarg :output-stream
    :reader acp-connection-output-stream
    :type stream
    :documentation "The character stream receiving our messages.")
   (request-dispatcher
    :initarg :request-dispatcher
    :reader acp-connection-request-dispatcher
    :type function
    :documentation
    "The function handling one peer request: method and params, returning the JSON result.")
   (notification-dispatcher
    :initarg :notification-dispatcher
    :reader acp-connection-notification-dispatcher
    :type function
    :documentation
    "The function handling one peer notification: method and params.")
   (writer-lock
    :initform (make-lock "Autolith ACP writer")
    :reader acp-connection-writer-lock
    :documentation "The lock serializing writes to the output stream.")
   (state-lock
    :initform (make-lock "Autolith ACP state")
    :reader acp-connection-state-lock
    :documentation
    "The lock guarding request ids, pending requests, and the request queue.")
   (worker-condition-variable
    :initform (make-condition-variable)
    :reader acp-connection-worker-condition-variable
    :documentation "The condition variable waking the request worker.")
   (pending-requests
    :initform (make-hash-table :test #'equal)
    :reader acp-connection-pending-requests
    :type hash-table
    :documentation "The table from outbound request id to pending entry.")
   (next-request-id
    :initform 0
    :accessor acp-connection-next-request-id
    :type integer
    :documentation "The last outbound request id issued.")
   (queued-requests
    :initform nil
    :accessor acp-connection-queued-requests
    :type list
    :documentation "The inbound requests waiting for the worker, oldest last.")
   (reader-thread
    :initform nil
    :accessor acp-connection-reader-thread
    :type t
    :documentation "The thread reading the input stream, or NIL before start.")
   (worker-thread
    :initform nil
    :accessor acp-connection-worker-thread
    :type t
    :documentation "The thread running request handlers, or NIL before start.")
   (closed-p
    :initform nil
    :accessor acp-connection-closed-p
    :type boolean
    :documentation "Whether the input stream has ended or close was called."))
  (:documentation "One ACP JSON-RPC connection over two character streams."))

;;;; -- Public Interface --

(-> acp-connection-create
    (&key (:input-stream stream) (:output-stream stream)
     (:request-dispatcher function) (:notification-dispatcher function))
    acp-connection)
(defun acp-connection-create
    (&key ((:input-stream input-stream) *standard-input*)
          ((:output-stream output-stream) *standard-output*)
          ((:request-dispatcher request-dispatcher) nil
           request-dispatcher-supplied-p)
          ((:notification-dispatcher notification-dispatcher) nil
           notification-dispatcher-supplied-p))
  "Return an ACP connection over INPUT-STREAM and OUTPUT-STREAM."
  (unless (and (streamp input-stream) (streamp output-stream))
    (error 'configuration-error
           :message "ACP connections require character streams."))
  (unless request-dispatcher-supplied-p
    (error 'configuration-error
           :message "ACP connections require a request dispatcher."))
  (unless notification-dispatcher-supplied-p
    (error 'configuration-error
           :message "ACP connections require a notification dispatcher."))
  (make-instance 'acp-connection
                 :input-stream input-stream
                 :output-stream output-stream
                 :request-dispatcher request-dispatcher
                 :notification-dispatcher notification-dispatcher))

(-> acp-connection-start (acp-connection) null)
(defun acp-connection-start (connection)
  "Start CONNECTION's reader and request worker threads."
  (with-lock-held ((acp-connection-state-lock connection))
    (unless (acp-connection-closed-p connection)
      (unless (acp-connection-worker-thread connection)
        (setf (acp-connection-worker-thread connection)
              (make-thread (lambda () (acp--worker-loop connection))
                           :name "Autolith ACP request worker")))
      (unless (acp-connection-reader-thread connection)
        (setf (acp-connection-reader-thread connection)
              (make-thread (lambda () (acp--reader-loop connection))
                           :name "Autolith ACP reader")))))
  nil)

(-> acp-connection-notify (acp-connection string (option json-object)) null)
(defun acp-connection-notify (connection method params)
  "Send one JSON-RPC notification naming METHOD on CONNECTION."
  (acp--send-message connection (acp--notification-message method params)))

(-> acp-connection-request (acp-connection string (option json-object)) json-value)
(defun acp-connection-request (connection method params)
  "Send one JSON-RPC request naming METHOD and return the peer's result.

Signals acp-remote-error when the peer replies with an error and
acp-connection-closed-error when the connection ends first."
  (let ((entry (acp-pending-request-create (make-condition-variable)))
        id)
    (with-lock-held ((acp-connection-state-lock connection))
      (when (acp-connection-closed-p connection)
        (error 'acp-connection-closed-error
               :message "The ACP connection is closed."))
      (setf id (incf (acp-connection-next-request-id connection)))
      (setf (gethash id (acp-connection-pending-requests connection)) entry))
    (acp--send-message connection (acp--request-message id method params))
    (with-lock-held ((acp-connection-state-lock connection))
      (loop until (acp-pending-request-done-p entry)
            do (condition-wait (acp-pending-request-condition-variable entry)
                               (acp-connection-state-lock connection))))
    (cond
      ((acp-pending-request-closed-p entry)
       (error 'acp-connection-closed-error
              :message "The ACP connection closed while a request waited."))
      ((acp-pending-request-error-code entry)
       (error 'acp-remote-error
              :code (acp-pending-request-error-code entry)
              :message (or (acp-pending-request-error-message entry)
                           "The peer returned an error.")
              :data (acp-pending-request-error-data entry)))
      (t (acp-pending-request-result entry)))))

(-> acp-connection-reply (acp-connection t json-value) null)
(defun acp-connection-reply (connection id result)
  "Reply to the peer request ID with RESULT on CONNECTION."
  (acp--send-message connection
                     (json-object "jsonrpc" "2.0" "id" id "result" result)))

(-> acp-connection-reply-error
    (acp-connection t integer string &key (:data t))
    null)
(defun acp-connection-reply-error (connection id code message &key data)
  "Reply to the peer request ID with the JSON-RPC error CODE and MESSAGE."
  (let ((error-object (json-object "code" code "message" message)))
    (when data
      (setf (gethash "data" error-object) data))
    (acp--send-message connection
                       (json-object "jsonrpc" "2.0" "id" id
                                    "error" error-object))))

(-> acp-connection-close (acp-connection) null)
(defun acp-connection-close (connection)
  "Stop CONNECTION's threads and fail its waiting requests."
  (acp--finish connection))

(-> acp-connection-join (acp-connection) null)
(defun acp-connection-join (connection)
  "Wait for CONNECTION's threads to end."
  (dolist (thread (list (acp-connection-reader-thread connection)
                        (acp-connection-worker-thread connection)))
    (when (and thread (thread-alive-p thread))
      (handler-case (join-thread thread)
        (error (condition)
          (acp--log "The ACP thread join failed: ~A" condition)))))
  nil)

;;;; -- Private Implementation --

(-> acp--log (string &rest t) null)
(defun acp--log (control &rest arguments)
  "Write one diagnostic line to stderr; stdout stays protocol-pure."
  (format *error-output* "autolith acp: ~?~%" control arguments)
  (finish-output *error-output*))

(-> acp--send-message (acp-connection json-object) null)
(defun acp--send-message (connection message)
  "Write MESSAGE to CONNECTION's output stream as one ACP line."
  (unless (acp-connection-closed-p connection)
    (with-lock-held ((acp-connection-writer-lock connection))
      (write-string (json-encode message)
                    (acp-connection-output-stream connection))
      (write-char #\Newline (acp-connection-output-stream connection))
      (force-output (acp-connection-output-stream connection))))
  nil)

(-> acp--request-message (integer string (option json-object)) json-object)
(defun acp--request-message (id method params)
  "Return the JSON-RPC request object for ID, METHOD, and PARAMS."
  (let ((message (json-object "jsonrpc" "2.0" "id" id "method" method)))
    (when params
      (setf (gethash "params" message) params))
    message))

(-> acp--notification-message (string (option json-object)) json-object)
(defun acp--notification-message (method params)
  "Return the JSON-RPC notification object for METHOD and PARAMS."
  (let ((message (json-object "jsonrpc" "2.0" "method" method)))
    (when params
      (setf (gethash "params" message) params))
    message))

(-> acp--reader-loop (acp-connection) null)
(defun acp--reader-loop (connection)
  "Read and dispatch peer messages until CONNECTION's input stream ends."
  (handler-case
      (loop
        (let ((line (read-line (acp-connection-input-stream connection)
                               nil nil)))
          (if (null line)
              (return)
              (acp--handle-line connection line))))
    (error (condition)
      (acp--log "The ACP reader stopped: ~A" condition)))
  (acp--finish connection)
  nil)

(-> acp--handle-line (acp-connection string) null)
(defun acp--handle-line (connection line)
  "Decode and dispatch one ACP LINE, replying with JSON-RPC errors as needed."
  (block nil
    (let ((text (string-right-trim '(#\Space #\Tab #\Return) line)))
      (when (zerop (length text))
        (return))
      (let ((message
              (handler-case (json-decode text)
                (error (condition)
                  (acp--log "Discarded one undecodable ACP message: ~A" condition)
                  (acp-connection-reply-error
                   connection nil *acp-parse-error-code* "Parse error")
                  (return)))))
        (unless (json-object-p message)
          (acp-connection-reply-error
           connection nil *acp-invalid-request-code* "Invalid Request")
          (return))
        (multiple-value-bind (method method-present-p)
            (json-get-present message "method")
          (multiple-value-bind (id id-present-p)
              (json-get-present message "id")
            (cond
              ((and method-present-p id-present-p)
               (cond
                 ((and (stringp method) (or (stringp id) (numberp id)))
                  (acp--enqueue-request connection message))
                 (t
                  (acp-connection-reply-error
                   connection nil *acp-invalid-request-code*
                   "Invalid Request"))))
              ((and method-present-p (not id-present-p))
               (if (stringp method)
                   (acp--dispatch-notification connection message)
                   (acp--log
                    "Ignored an ACP notification with a non-string method.")))
              (id-present-p
               (acp--dispatch-response connection message))))))))
  nil)

(-> acp--dispatch-notification (acp-connection json-object) null)
(defun acp--dispatch-notification (connection message)
  "Dispatch one peer notification MESSAGE through CONNECTION's handler."
  (handler-case
      (funcall (acp-connection-notification-dispatcher connection)
               (json-get message "method")
               (json-get message "params" nil))
    (error (condition)
      (acp--log "The ACP notification handler failed: ~A" condition)))
  nil)

(-> acp--enqueue-request (acp-connection json-object) null)
(defun acp--enqueue-request (connection message)
  "Queue peer request MESSAGE for CONNECTION's worker thread."
  (with-lock-held ((acp-connection-state-lock connection))
    (push message (acp-connection-queued-requests connection))
    (condition-notify (acp-connection-worker-condition-variable connection)))
  nil)

(-> acp--worker-loop (acp-connection) null)
(defun acp--worker-loop (connection)
  "Run queued peer requests on CONNECTION until it closes."
  (loop
    (let ((message nil))
      (with-lock-held ((acp-connection-state-lock connection))
        (loop
          (let ((queue (acp-connection-queued-requests connection)))
            (cond
              (queue
               (setf message (first (last queue)))
               (setf (acp-connection-queued-requests connection)
                     (nbutlast queue))
               (return))
              ((acp-connection-closed-p connection)
               (return-from acp--worker-loop))
              (t
               (condition-wait
                (acp-connection-worker-condition-variable connection)
                (acp-connection-state-lock connection)))))))
      (acp--run-request connection message))))

(-> acp--run-request (acp-connection json-object) null)
(defun acp--run-request (connection message)
  "Handle one peer request MESSAGE and send its reply on CONNECTION."
  (let ((id (json-get message "id" nil))
        (method (json-get message "method"))
        (params (json-get message "params" nil)))
    (block nil
      (let ((result
              (handler-case
                  (funcall (acp-connection-request-dispatcher connection)
                           method params)
                (acp-method-error (condition)
                  (acp--guarded-reply-error
                   connection id
                   (acp-method-error-code condition)
                   (acp-method-error-message condition)
                   :data (acp-method-error-data condition))
                  (return))
                (error (condition)
                  (acp--guarded-reply-error
                   connection id *acp-internal-error-code* "Internal error"
                   :data (format nil "~A" condition))
                  (return)))))
        (handler-case
            (acp-connection-reply connection id result)
          (error (condition)
            (acp--log "The ACP reply failed: ~A" condition)
            (handler-case
                (acp-connection-reply-error
                 connection id *acp-internal-error-code* "Internal error")
              (error (reply-condition)
                (acp--log "The ACP error reply also failed: ~A"
                          reply-condition))))))))
  nil)

(-> acp--guarded-reply (acp-connection t json-value) null)
(defun acp--guarded-reply (connection id result)
  "Reply with RESULT while keeping the worker alive on stream failures."
  (handler-case (acp-connection-reply connection id result)
    (error (condition)
      (acp--log "The ACP reply failed: ~A" condition)))
  nil)

(-> acp--guarded-reply-error
    (acp-connection t integer string &key (:data t))
    null)
(defun acp--guarded-reply-error (connection id code message &key data)
  "Reply with one error while keeping the worker alive on stream failures."
  (handler-case (acp-connection-reply-error connection id code message
                                            :data data)
    (error (condition)
      (acp--log "The ACP error reply failed: ~A" condition)))
  nil)

(-> acp--error-fields (json-object) (values (option integer) (option string) t))
(defun acp--error-fields (error-object)
  "Return CODE, MESSAGE, and DATA parsed from ERROR-OBJECT."
  (values (let ((code (json-get error-object "code")))
            (if (integerp code) code *acp-internal-error-code*))
          (let ((text (json-get error-object "message")))
            (if (stringp text) text "The peer returned an error."))
          (json-get error-object "data" nil)))

(-> acp--resolve-entry
    (acp-connection acp-pending-request json-object)
    null)
(defun acp--resolve-entry (connection entry message)
  "Fill ENTRY from the peer response MESSAGE and wake its waiter."
  (multiple-value-bind (error-object error-present-p)
      (json-get-present message "error")
    (with-lock-held ((acp-connection-state-lock connection))
      (cond
        ((and error-present-p (json-object-p error-object))
         (multiple-value-bind (code text data)
             (acp--error-fields error-object)
           (setf (acp-pending-request-error-code entry) code
                 (acp-pending-request-error-message entry) text
                 (acp-pending-request-error-data entry) data
                 (acp-pending-request-done-p entry) t
                 (acp-pending-request-result entry) nil)))
        (error-present-p
         (setf (acp-pending-request-error-code entry)
               *acp-internal-error-code*
               (acp-pending-request-error-message entry)
               "The peer returned a malformed error."
               (acp-pending-request-done-p entry) t
               (acp-pending-request-result entry) nil))
        ((json-get-present message "result")
         (setf (acp-pending-request-result entry)
               (json-get message "result")
               (acp-pending-request-done-p entry) t))
        (t
         (setf (acp-pending-request-error-code entry)
               *acp-internal-error-code*
               (acp-pending-request-error-message entry)
               "The peer returned a response without a result or an error."
               (acp-pending-request-done-p entry) t
               (acp-pending-request-result entry) nil)))
      (condition-notify (acp-pending-request-condition-variable entry))))
  nil)

(-> acp--dispatch-response (acp-connection json-object) null)
(defun acp--dispatch-response (connection message)
  "Resolve one pending outbound request with the peer response MESSAGE."
  (block nil
    (let ((id (json-get message "id" nil))
          entry)
      (unless (or (stringp id) (numberp id))
        (acp--log "Ignored an ACP response without a usable id.")
        (return))
      (with-lock-held ((acp-connection-state-lock connection))
        (setf entry (gethash id (acp-connection-pending-requests connection)))
        (when entry
          (remhash id (acp-connection-pending-requests connection))))
      (unless entry
        (acp--log "Ignored an ACP response without a matching request.")
        (return))
      (acp--resolve-entry connection entry message)))
  nil)

(-> acp--finish (acp-connection) null)
(defun acp--finish (connection)
  "Mark CONNECTION closed, fail its waiting requests, and wake its worker."
  (with-lock-held ((acp-connection-state-lock connection))
    (unless (acp-connection-closed-p connection)
      (setf (acp-connection-closed-p connection) t)
      (setf (acp-connection-queued-requests connection) nil)
      (maphash
       (lambda (id entry)
         (declare (ignore id))
         (setf (acp-pending-request-done-p entry) t
               (acp-pending-request-closed-p entry) t)
         (condition-notify
          (acp-pending-request-condition-variable entry)))
       (acp-connection-pending-requests connection))
      (clrhash (acp-connection-pending-requests connection))
      (condition-notify
       (acp-connection-worker-condition-variable connection))))
  nil)
