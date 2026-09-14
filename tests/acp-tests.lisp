(in-package #:autolith)

;;;; -- ACP Integration Tests --

;; These tests exercise the ACP interface as a fake editor client. Each test
;; starts `acp-serve' in a background thread over a Unix pipe, exchanges
;; JSON-RPC 2.0 messages, and verifies that every wire message parses as
;; JSON-RPC.

(defparameter *acp-test-timeout-seconds* 10
  "How long to wait for one ACP response line from the server thread.")

(-> acp-test--make-pipe () (values stream stream))
(defun acp-test--make-pipe ()
  "Return a read stream and a write stream connected by a Unix pipe.
The read stream is suitable for the server's input; the write stream is
suitable for the test client to write to."
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream read-fd
                                   :input t
                                   :external-format ':utf-8
                                   :buffering ':line)
            (sb-sys:make-fd-stream write-fd
                                   :output t
                                   :external-format ':utf-8
                                   :buffering ':line))))

(-> acp-test--spawn (configuration) list)
(defun acp-test--spawn (configuration)
  "Start `acp-serve' in a background thread using a fresh pipe; return a client."
  (multiple-value-bind (server-input client-output)
      (acp-test--make-pipe)
    (multiple-value-bind (client-input server-output)
        (acp-test--make-pipe)
      (let ((thread (sb-thread:make-thread
                     (lambda ()
                       (handler-case
                           (acp-serve :configuration configuration
                                      :input-stream server-input
                                      :output-stream server-output)
                         (error (condition)
                           (format *error-output*
                                   "~&ACP server thread died: ~A~%" condition))))
                     :name "ACP test server")))
        (list :input client-output
              :output client-input
              :thread thread)))))

(-> acp-test--close (list) null)
(defun acp-test--close (client)
  "Close CLIENT's streams and wait for its server thread."
  (ignore-errors (close (getf client :input) :abort t))
  (ignore-errors (close (getf client :output) :abort t))
  (let ((thread (getf client :thread)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (ignore-errors
        (sb-thread:join-thread thread :timeout 10 :default nil))))
  nil)

(defmacro acp-test--with-client ((client) &body body)
  "Evaluate BODY with CLIENT bound to a fresh in-process ACP server."
  `(with-test-configuration (configuration)
     (let ((,client nil))
       (unwind-protect
            (progn
              (setf ,client (acp-test--spawn configuration))
              ,@body)
         (when ,client
           (acp-test--close ,client))))))


(-> acp-test--write-line (list string) null)
(defun acp-test--write-line (client line)
  "Write LINE to CLIENT's stdin and flush."
  (write-line line (getf client :input))
  (force-output (getf client :input))
  nil)

(-> acp-test--read-line-timeout (list &optional integer) (option string))
(defun acp-test--read-line-timeout (client &optional (timeout *acp-test-timeout-seconds*))
  "Read one line from CLIENT's stdout with a bounded timeout."
  (handler-case
      (sb-ext:with-timeout timeout
        (read-line (getf client :output) nil nil))
    (sb-ext:timeout ()
      nil)))

(-> acp-test--decode-line ((option string)) (option json-object))
(defun acp-test--decode-line (line)
  "Parse LINE as one JSON-RPC message, or signal a test failure."
  (when line
    (let ((message (json-decode line)))
      (test-assert (hash-table-p message)
                   "every stdout line parses as a JSON object")
      (test-assert (equal (json-get message "jsonrpc") "2.0")
                   "every JSON-RPC message carries jsonrpc 2.0")
      message)))

(-> acp-test--request (list integer string json-object) null)
(defun acp-test--request (client id method params)
  "Send one JSON-RPC request from CLIENT."
  (acp-test--write-line
   client
   (json-encode
    (json-object "jsonrpc" "2.0"
                 "id" id
                 "method" method
                 "params" params)))
  nil)

(-> acp-test--notification (list string json-object) null)
(defun acp-test--notification (client method params)
  "Send one JSON-RPC notification from CLIENT."
  (acp-test--write-line
   client
   (json-encode
    (json-object "jsonrpc" "2.0"
                 "method" method
                 "params" params)))
  nil)

(-> acp-test--expect-message (list) json-object)
(defun acp-test--expect-message (client)
  "Read and return the next JSON-RPC message from CLIENT's stdout."
  (let ((line (acp-test--read-line-timeout client)))
    (test-assert line "expected an ACP message on stdout")
    (acp-test--decode-line line)))

(-> acp-test--expect-response (list integer &key (:max-messages integer)) json-object)
(defun acp-test--expect-response (client id &key (max-messages 16))
  "Drain CLIENT's stdout until a response with ID is seen; return it."
  (loop for count from 0 below max-messages
        for message = (acp-test--expect-message client)
        do (when (equal (json-get message "id") id)
             (return message))
        finally (test-assert nil
                             (format nil "no response received for request id ~A" id))))

(-> acp-test--expect-error (list integer integer) json-object)
(defun acp-test--expect-error (client id code)
  "Assert that CLIENT returns a JSON-RPC error with CODE for request ID."
  (let ((message (acp-test--expect-response client id)))
    (test-assert (json-get message "error")
                 "expected a JSON-RPC error response")
    (test-assert (= (json-get (json-get message "error") "code") code)
                 (format nil "expected JSON-RPC error code ~D" code))
    message))

(-> acp-test--drain-stdout (list &optional integer) list)
(defun acp-test--drain-stdout (client &optional (timeout *acp-test-timeout-seconds*))
  "Read all remaining stdout lines from CLIENT and return them decoded.
Use TIMEOUT as the per-line wait bound."
  (let ((messages nil))
    (loop for line = (acp-test--read-line-timeout client timeout)
          while line
          do (push (acp-test--decode-line line) messages))
    (nreverse messages)))


(defun test-acp-initialize-handshake ()
  "The agent echoes initialize and advertises the expected capabilities."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (let* ((response (acp-test--expect-response client 1))
           (result (json-get response "result")))
      (test-assert (hash-table-p result) "initialize response has a result")
      (test-assert (= (json-get result "protocolVersion") 1)
                   "initialize echoes protocolVersion")
      (let ((capabilities (json-get result "agentCapabilities")))
        (test-assert (hash-table-p capabilities)
                     "initialize result has agentCapabilities")
        (test-assert (eq (json-get capabilities "loadSession") t)
                     "loadSession capability is advertised"))
      (let ((info (json-get result "agentInfo")))
        (test-assert (hash-table-p info) "initialize result has agentInfo")
        (test-assert (equal (json-get info "name") "Autolith")
                     "agentInfo name is Autolith"))
      (test-assert (vectorp (json-get result "authMethods"))
                   "initialize result has an authMethods array"))))

(defun test-acp-session-new ()
  "session/new returns a session id and a modes object."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/new"
     (json-object "cwd" (namestring (uiop:getcwd))
                  "mcpServers" (json-array)))
    (let* ((response (acp-test--expect-response client 2))
           (result (json-get response "result")))
      (test-assert (hash-table-p result) "session/new response has a result")
      (let ((session-id (json-get result "sessionId")))
        (test-assert (and (stringp session-id)
                          (>= (length session-id) 6)
                          (string= (subseq session-id 0 5) "sess_"))
                     "session/new returns a sess_ id"))
      (let ((modes (json-get result "modes")))
        (test-assert (hash-table-p modes) "session/new returns a modes object")
        (test-assert (equal (json-get modes "currentModeId") "ask")
                     "default currentModeId is ask")
        (test-assert (vectorp (json-get modes "availableModes"))
                     "modes object lists available modes")))))

(defun test-acp-unknown-method ()
  "Unknown methods return JSON-RPC -32601 method not found."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
      (acp-test--request client 2 "session/unknown" (json-object))
    (acp-test--expect-error client 2 *acp-method-not-found-code*)))

(defun test-acp-prompt-unknown-session ()
  "session/prompt with an unknown session returns -32602 and the connection survives."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/prompt"
     (json-object "sessionId" "sess_does-not-exist"
                  "prompt" (json-array
                            (json-object "type" "text"
                                         "text" "hello"))))
    (acp-test--expect-error client 2 *acp-invalid-params-code*)
    ;; Prove the connection is still healthy.
      (acp-test--request client 3 "session/unknown" (json-object))
    (acp-test--expect-error client 3 *acp-method-not-found-code*)))

(defun test-acp-cancel-notification ()
  "session/cancel is a notification and produces no reply line."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/new"
     (json-object "cwd" (namestring (uiop:getcwd))
                  "mcpServers" (json-array)))
    (let* ((response (acp-test--expect-response client 2))
           (session-id (json-get (json-get response "result") "sessionId")))
      (acp-test--notification
       client "session/cancel"
       (json-object "sessionId" session-id))
      ;; Wait briefly; a conforming notification gets no response.
      (let ((line (acp-test--read-line-timeout client 1)))
        (test-assert (null line)
                     "session/cancel produced no stdout reply line")))))
(defun test-acp-session-set-mode ()
  "session/set_mode changes the mode and emits a current_mode_update."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/new"
     (json-object "cwd" (namestring (uiop:getcwd))
                  "mcpServers" (json-array)))
    (let ((session-id (json-get (json-get (acp-test--expect-response client 2) "result")
                                "sessionId")))
      (acp-test--request
       client 3 "session/set_mode"
       (json-object "sessionId" session-id "modeId" "auto"))
      (let* ((update (acp-test--expect-message client))
             (params (json-get update "params"))
             (update-object (json-get params "update")))
        (test-assert (string= (json-get update "method") "session/update")
                     "set_mode emits a session/update notification")
        (test-assert (string= (json-get update-object "sessionUpdate")
                              "current_mode_update")
                     "update is current_mode_update")
        (test-assert (equal (json-get update-object "currentModeId") "auto")
                     "currentModeId is auto"))
      (let ((response (acp-test--expect-response client 3)))
        (test-assert (hash-table-p (json-get response "result"))
                     "session/set_mode returns a result")))))

(defun test-acp-session-list ()
  "session/list returns the active session ids."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/new"
     (json-object "cwd" (namestring (uiop:getcwd))
                  "mcpServers" (json-array)))
    (let ((session-id (json-get (json-get (acp-test--expect-response client 2) "result")
                                "sessionId")))
      (acp-test--request client 3 "session/list" (json-object))
      (let* ((response (acp-test--expect-response client 3))
             (result (json-get response "result")))
        (test-assert (vectorp result) "session/list result is an array")
        (test-assert (= (length result) 1) "session/list returns one session")
        (test-assert (equal (json-get (aref result 0) "sessionId") session-id)
                     "session/list contains the new session")))))


(defun test-acp-stdout-purity-and-clean-exit ()
  "Every line on stdout parses as JSON-RPC and the agent exits cleanly."
  (acp-test--with-client (client)
    (acp-test--request
     client 1 "initialize"
     (json-object "protocolVersion" 1
                  "clientCapabilities" (json-object)
                  "clientInfo" (json-object "name" "test" "version" "0")))
    (acp-test--expect-response client 1)
    (acp-test--request
     client 2 "session/new"
     (json-object "cwd" (namestring (uiop:getcwd))
                  "mcpServers" (json-array)))
    (acp-test--expect-response client 2)
      (acp-test--request client 3 "session/unknown" (json-object))
    (acp-test--expect-error client 3 *acp-method-not-found-code*)
    ;; Close stdin, drain stdout, and verify exit.
    (close (getf client :input))
    (let ((messages (acp-test--drain-stdout client 1)))
      (test-assert (every #'hash-table-p messages)
                   "every captured stdout line decoded as a JSON object"))
    (let ((thread (getf client :thread)))
      (test-assert (not (eq (sb-thread:join-thread thread :timeout 10 :default ':timeout)
                            ':timeout))
                   "ACP server thread exited cleanly after stdin closed"))))
