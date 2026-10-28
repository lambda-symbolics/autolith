(in-package #:autolith)

;;;; -- Remote ACP Endpoints --

(-> test-acp-endpoint-parse () null)
(defun test-acp-endpoint-parse ()
  "Parse [HOST:]PORT endpoints, defaulting to loopback and the default port."
  (dolist (case '((nil "127.0.0.1" 8737 t)
                  ("9000" "127.0.0.1" 9000 t)
                  ("0" "127.0.0.1" 0 t)
                  ("localhost:8080" "localhost" 8080 t)
                  ("127.0.0.2:8080" "127.0.0.2" 8080 t)
                  ("[::1]:8080" "::1" 8080 t)
                  ("0.0.0.0:8737" "0.0.0.0" 8737 nil)
                  ("[::]:8737" "::" 8737 nil)
                  ("192.168.1.20:65535" "192.168.1.20" 65535 nil)))
    (destructuring-bind (text host port loopback-p) case
      (let ((endpoint (acp-endpoint-parse ':websocket text)))
        (test-assert (equal host (acp-endpoint-host endpoint)) "the endpoint host is parsed")
        (test-assert (eql port (acp-endpoint-port endpoint)) "the endpoint port is parsed")
        (test-assert (eq loopback-p (acp-endpoint-loopback-p endpoint))
                     "loopback endpoints are told apart from reachable ones")
        (test-assert (eq (not loopback-p)
                         (not (null (acp-endpoint-warning endpoint "http://example/acp"))))
                     "only endpoints beyond loopback carry a warning"))))
  (dolist (text '("" ":8080" "host:" "host:port" "host:65536" "host:123456" "::1:8080" "[::1]" "[::1]8080"
                  "[::1:8080"))
    (test-assert (handler-case (progn (acp-endpoint-parse ':http text) nil)
                   (configuration-error () t))
                 "a malformed endpoint is a configuration error"))
  (test-assert (handler-case (progn (acp-endpoint-parse ':http "8080" :token "") nil)
                 (configuration-error () t))
               "an empty token is a configuration error")
  (test-assert (search "--token"
                       (acp-endpoint-warning (acp-endpoint-parse ':http "0.0.0.0:1") "http://0.0.0.0:1/acp"))
               "a reachable endpoint without a token suggests one")
  (test-assert (not (search "--token"
                            (acp-endpoint-warning (acp-endpoint-parse ':http "0.0.0.0:1" :token "t")
                                                  "http://0.0.0.0:1/acp")))
               "a reachable endpoint with a token warns about the cleartext token")
  nil)

(-> test-acp-command-rejects-endpoint-combinations () null)
(defun test-acp-command-rejects-endpoint-combinations ()
  "Reject endpoint options that do not form one remote transport before any bootstrap."
  (dolist (arguments '(("acp" "--http" "--websocket")
                       ("acp" "8080")
                       ("acp" "--token" "secret")
                       ("acp" "--http" "1" "2")
                       ("acp" "--websocket" "host:")))
    (test-assert (handler-case (progn (main-dispatch arguments) nil)
                   (configuration-error () t))
                 "an invalid ACP endpoint request is a configuration error"))
  nil)


;;;; -- Remote ACP Processes --

(-> acp-remote-tests--call-with-server (pathname list function) t)
(defun acp-remote-tests--call-with-server (root arguments function)
  "Run bin/autolith acp with ARGUMENTS under ROOT, call FUNCTION with its URL, then interrupt it.

Return what FUNCTION returns."
  (let* ((process (uiop:launch-program
                   (append (list (namestring (asdf:system-relative-pathname :autolith "bin/autolith")) "acp")
                           arguments)
                   :directory    root
                   :input        nil
                   :output       nil
                   :error-output ':stream))
         (errors  (uiop:process-info-error-output process))
         (result  nil))
    (unwind-protect
         (let ((url (loop for line = (read-line errors nil nil)
                          while line
                          do (let ((start (search "listening at " line)))
                               (when start
                                 (return (subseq line (+ start 13)
                                                 (or (search " and " line) (length line)))))))))
           (test-assert url "the server announces its endpoint")
           (setf result (funcall function url)))
      (uiop:run-program (list "kill" "-INT" "--" (format nil "-~D" (uiop:process-info-pid process)))
                        :ignore-error-status t)
      (let ((status (uiop:wait-process process)))
        (test-assert (eql 130 status) "an interrupted remote ACP server exits with status 130")))
    result))

(-> acp-remote-tests--initialize (agentcomms:acp-channel) hash-table)
(defun acp-remote-tests--initialize (channel)
  "Initialize an ACP client over CHANNEL and return the agent's answer."
  (let ((client (make-instance 'agentcomms:acp-client)))
    (agentcomms:acp-client-connect client channel)
    (unwind-protect (agentcomms:client-initialize client :timeout 60)
      (agentcomms:connection-close (agentcomms:acp-client-connection client)))))

(-> test-acp-remote-websocket-process () null)
(defun test-acp-remote-websocket-process ()
  "Serve ACP over WebSocket from a real process and end it with Ctrl-C."
  (with-test-fixture (':posix-shell "process-group interrupts")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (acp-launcher-tests--call-with-roots
       root
       (lambda ()
         (with-test-environment (("AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
           (acp-remote-tests--call-with-server
            root '("--websocket" "127.0.0.1:0" "--token" "s3cret")
            (lambda (url)
              (test-assert (eql 0 (search "ws://127.0.0.1:" url)) "the endpoint is a WebSocket URL")
              (test-assert (handler-case (progn (agentcomms:acp-websocket-connect url) nil)
                             (agentcomms:websocket-handshake-error () t))
                           "a client without the token is refused")
              (test-assert
               (equal "autolith"
                      (agentcomms:json-get
                       (agentcomms:json-get
                        (acp-remote-tests--initialize
                         (agentcomms:acp-websocket-connect (format nil "~A?token=s3cret" url)))
                        "agentInfo")
                       "name"))
               "a client with the token reaches the agent"))))))))
  nil)

(-> test-acp-remote-http-process () null)
(defun test-acp-remote-http-process ()
  "Serve ACP over Streamable HTTP from a real process."
  (with-test-fixture (':posix-shell "process-group interrupts")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (acp-launcher-tests--call-with-roots
       root
       (lambda ()
         (with-test-environment (("AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
           (acp-remote-tests--call-with-server
            root '("--http" "0")
            (lambda (url)
              (multiple-value-bind (body status headers)
                  (dexador:post url
                                :headers      '(("Content-Type" . "application/json"))
                                :content      "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"protocolVersion\":1}}"
                                :force-string t
                                :keep-alive   nil)
                (test-assert (eql 200 status) "initialize answers 200")
                (test-assert (gethash "acp-connection-id" headers) "initialize names the connection")
                (test-assert (search "\"autolith\"" body) "the agent answers initialize")))))))))
  nil)
