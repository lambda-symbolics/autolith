;;;; -- Trusted Parent Interface Adapter Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-adapter-test--cleanup (pathname) null)
(defun parent-adapter-test--cleanup (root)
  "Remove the ordinary fixture tree after all endpoint threads stop."
  (when (uiop:directory-exists-p root)
    (dolist (file (uiop:directory-files root)) (delete-file file))
    (dolist (directory (uiop:subdirectories root)) (parent-adapter-test--cleanup directory))
    (uiop:delete-empty-directory root))
  nil)

(serapeum/types:-> parent-adapter-test--configuration (pathname) parent-configuration)
(defun parent-adapter-test--configuration (root)
  "Build a complete parent configuration without starting a child."
  (flet ((path (name) (namestring (merge-pathnames name root))))
    (make-parent-configuration
     (list :version 1 :runtime "/usr/bin/false" :agent-core (path "agent.core")
           :worker-core (path "agent.core") :source-root (path "source/")
           :workspace (path "workspace/") :private-root (path "private/")
           :temporary-root (path "temporary/") :runtime-roots (list (path "source/"))
           :account-configuration (list :state-root (path "private/accounts/"))
           :profiles              '((:id "ordinary" :family :openai-api :model "fixture"))
           :profile-id "ordinary" :access-p nil))))

(serapeum/types:-> run-parent-detached-adapter-tests () integer)
(defun run-parent-detached-adapter-tests ()
  "Exercise real authenticated local requests and ordered parent callbacks."
  (let* ((root          (merge-pathnames (format nil "parent-adapter-~A/" (symbol-name (gensym)))
                                         (uiop:temporary-directory)))
         (configuration (parent-adapter-test--configuration root))
         (application   (make-parent-application configuration :session-id "adapter-test"
                                                 :input-source (constantly nil) :renderer (constantly nil)))
         (terminal      (parent-detached-terminal-create))
         (calls         nil)
         (runtime       nil))
    (unwind-protect
         (progn
           (setf runtime
                 (parent-detached-start
                  application terminal
                  :submit (lambda (text) (push (list ':tell text) calls))
                  :status (lambda () '(:state :ready))
                  :pause  (lambda () (push ':pause calls))
                  :detach (lambda () (push ':detach calls))
                  :kill   (lambda () (push ':kill calls))))
           (let ((entry (parent-detached--select configuration "adapter-test")))
             (assert (eq (first (parent-detached--query entry ':status)) ':ok))
             (assert (eq (first (parent-detached--query entry ':tell '(:message "hello"))) ':ok))
             (assert (eq (first (parent-detached--query entry ':pause)) ':ok))
             (assert (eq (first (parent-detached--query entry ':detach)) ':ok))
             (assert (eq (first (parent-detached--query entry ':kill)) ':ok))
             (assert (equal (reverse calls) '((:tell "hello") :pause :detach :kill)))
             (assert (eq (first (parent-detached--query entry ':tell '(:message ""))) ':error))
             (let ((record (rest (rest entry))))
               (assert (eq (first (image-daemon:daemon-call (getf record :port) "wrong-token" ':status)) ':error))))
           (parent-detached-stop runtime)
           (setf runtime nil)
           (assert (null (parent-detached--records configuration)))
           8)
      (when runtime (parent-detached-stop runtime))
      (parent-adapter-test--cleanup root))))

(serapeum/types:-> run-parent-acp-adapter-tests () integer)
(defun run-parent-acp-adapter-tests ()
  "Check bounded typed events, trusted approval isolation, and controller cleanup."
  (let* ((service (make-instance 'parent-acp-service
                                 :configuration (parent-adapter-test--configuration (uiop:temporary-directory))
                                 :request       nil))
         (session (make-instance 'parent-acp-session :service service :identifier "adapter-test")))
    (parent-acp-event
     session ':ui
     (agentcomms:json-object
      "kind" "pending-inputs"
      "payload" (vector (agentcomms:json-object "keyword" "steering") nil
                        (agentcomms:json-object "keyword" "queued") nil)))
    (assert (null (parent-acp-session-events session)))
    (parent-acp-notification session ':ui-completion-ready "terminal-only")
    (assert (null (parent-acp-session-completion-ready session)))
    (assert (handler-case
                (progn
                  (parent-acp-event session ':ui (agentcomms:json-object "kind" "unknown"))
                  nil)
              (parent-terminal-error () t)))
    (parent-acp--render session :region ':trusted :kind ':approval
                       :payload '(:operation-id "exact" :description "consent"))
    (assert (equal (getf (parent-acp-session-approval session) :operation-id) "exact"))
    (assert (null (parent-acp-session-events session)))
    (parent-acp--render session :region ':agent :kind ':diagnostic :payload '(:text "public"))
    (assert (= (length (parent-acp-session-events session)) 1))
    (parent-acp-notification session ':acp-completion-ready "wake")
    (assert (parent-acp-session-completion-ready session))
    (let ((server (make-instance 'parent-acp-mcp-server :identifier "opaque" :name "fixture"
                                :client nil :secrets '("synthetic-secret"))))
      (assert (equal (gethash "text" (parent-acp-mcp--public
                                      server (agentcomms:json-object "text" "synthetic-secret answer")))
                     "[credential] answer"))
      (handler-case
          (progn (parent-acp-mcp--public server (make-string 262145)) (error "Missing bound rejection."))
        (parent-application-operation-error () nil)))
    (parent-acp-session-close session)
    (assert (parent-acp-session-closed-p session))
    9))

(serapeum/types:-> parent-adapter-test--socket-pair (function) t)
(defun parent-adapter-test--socket-pair (function)
  "Call FUNCTION with two connected loopback character streams and close both on exit."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp))
        (client nil) (server nil) (local nil) (remote nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 1)
           (setf client (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp))
           (sb-bsd-sockets:socket-connect client #(127 0 0 1)
                                          (nth-value 1 (sb-bsd-sockets:socket-name listener)))
           (setf server (sb-bsd-sockets:socket-accept listener)
                 local (sb-bsd-sockets:socket-make-stream server :input t :output t :buffering ':none
                                                        :element-type 'character :external-format ':utf-8)
                 remote (sb-bsd-sockets:socket-make-stream client :input t :output t :buffering ':none
                                                         :element-type 'character :external-format ':utf-8))
           (funcall function local remote))
      (dolist (stream (list local remote))
        (when stream (close stream :abort t)))
      (sb-bsd-sockets:socket-close listener))))

(serapeum/types:-> run-parent-acp-permission-tests () integer)
(defun run-parent-acp-permission-tests ()
  "Check timeout and session cancellation across a real ACP socket, including request cleanup."
  (parent-adapter-test--socket-pair
   (lambda (local remote)
     (let* ((service    (make-instance 'parent-acp-service :request nil
                                       :configuration               (parent-adapter-test--configuration (uiop:temporary-directory))))
            (session    (make-instance 'parent-acp-session :service service :identifier "permission-test"))
            (channel    (agentcomms:make-acp-stream-channel :input local :output local))
            (connection (agentcomms:acp-agent-connect service channel)))
       (setf (gethash "permission-test" (parent-acp-sessions service)) session)
       (dolist (cancel-p '(nil t))
         (parent-acp--render session :region ':trusted :kind ':approval
                            :payload '(:operation-id "exact" :description "consent"))
         (let ((thread (bordeaux-threads:make-thread
                        (lambda () (multiple-value-list (parent-acp--permission-input session (if cancel-p 30 0.05))))
                        :name "parent permission fixture")))
           (let ((request (sb-ext:with-timeout 2 (agentcomms:json-decode (read-line remote)))))
             (assert (equal (gethash "method" request) "session/request_permission"))
             (when cancel-p (agentcomms:agent-cancel service "permission-test"))
             (assert (equal (sb-thread:join-thread thread :timeout 2 :default ':timeout) '("NO" :line)))
             (let ((cancel (sb-ext:with-timeout 2 (agentcomms:json-decode (read-line remote)))))
               (assert (equal (gethash "requestId" (gethash "params" cancel)) (gethash "id" request))))
             (assert (zerop (hash-table-count (agentcomms::acp-connection-pending connection))))
             (assert (null (parent-acp-session-permission-thread session)))
             (assert (null (parent-acp-session-approval session)))))))))
  12)

(serapeum/types:-> parent-adapter-test--attach (list keyword) (values t stream list))
(defun parent-adapter-test--attach (entry mode)
  "Attach to the authenticated parent endpoint and read the complete handshake."
  (let ((record (rest (rest entry))))
    (multiple-value-bind (socket stream) (image-daemon:daemon-connect (getf record :port))
      (image-daemon:daemon-write-packet
       stream (list :localgroup-request :version image-daemon:*daemon-protocol-version*
                    :token (getf record :token) :operation ':attach
                    :arguments          (list :mode mode :rows 24 :columns 80 :styled-p nil)))
      (values socket stream (sb-ext:with-timeout 2 (image-daemon:daemon-read-packet stream))))))

(serapeum/types:-> run-parent-detached-attachment-tests () integer)
(defun run-parent-detached-attachment-tests ()
  "Exercise authenticated control, read-only input rejection, takeover, and output delivery."
  (let* ((root          (merge-pathnames (format nil "parent-attach-~A/" (gensym)) (uiop:temporary-directory)))
         (configuration (parent-adapter-test--configuration root))
         (application   (make-parent-application configuration :session-id "attachment-test"
                                                :input-source (constantly nil) :renderer (constantly nil)))
         (terminal      (parent-detached-terminal-create))
         (runtime nil) (streams nil) (takeovers 0))
    (unwind-protect
         (progn
           (setf runtime (parent-detached-start application terminal :submit (constantly nil)
                                                :status (constantly nil) :pause (constantly nil)
                                                :kill (constantly nil) :detach (lambda () (incf takeovers))))
           (let ((entry (parent-detached--select configuration "attachment-test")))
             (multiple-value-bind (socket control handshake) (parent-adapter-test--attach entry ':control)
               (declare (ignore socket))
               (push control streams)
               (assert (eq (first handshake) ':attached))
               (multiple-value-bind (socket observer handshake) (parent-adapter-test--attach entry ':read-only)
                 (declare (ignore socket))
                 (push observer streams)
                 (assert (eq (first handshake) ':attached))
                 (image-daemon:daemon-write-packet observer '(:event (:line "forbidden")))
                 (image-daemon:daemon-write-packet control '(:event (:line "accepted")))
                 (assert (equal (sb-ext:with-timeout 2
                                  (loop until (terminal-input-ready-p terminal) do (sleep 0.001))
                                  (terminal-read-event terminal))
                                '(:line "accepted")))
                 (assert (not (terminal-input-ready-p terminal)))
                 (terminal--write terminal "trusted output")
                 (terminal-flush terminal)
                 (assert (equal (sb-ext:with-timeout 2 (image-daemon:daemon-read-packet observer))
                                '(:output "trusted output")))
                 (multiple-value-bind (socket replacement handshake) (parent-adapter-test--attach entry ':take-over)
                   (declare (ignore socket))
                   (push replacement streams)
                   (assert (eq (first handshake) ':attached))
                   (assert (= takeovers 1))
                   (image-daemon:daemon-write-packet replacement '(:event (:line "replacement")))
                   (assert (equal (sb-ext:with-timeout 2
                                    (loop until (terminal-input-ready-p terminal) do (sleep 0.001))
                                    (terminal-read-event terminal))
                                  '(:line "replacement")))))))
           8)
      (dolist (stream streams) (close stream :abort t))
      (when runtime (parent-detached-stop runtime))
      (parent-adapter-test--cleanup root))))

(serapeum/types:-> parent-adapter-test--http-request (stream) (values string list t))
(defun parent-adapter-test--http-request (stream)
  "Read one bounded ASCII HTTP fixture request."
  (let ((line (read-line stream)) (headers nil) (length 0))
    (loop for header = (string-right-trim '(#\Return) (read-line stream))
          until (zerop (length header))
          do (push header headers)
             (when (and (>= (length header) 15) (string-equal header "Content-Length:" :end1 15))
               (setf length (parse-integer header :start 15 :junk-allowed t))))
    (assert (<= 0 length 65536))
    (let ((body (make-string length)))
      (read-sequence body stream)
      (values line headers (unless (zerop length) (agentcomms:json-decode body))))))

(serapeum/types:-> parent-adapter-test--http-reply (stream t &key (:status (or null string))) null)
(defun parent-adapter-test--http-reply (stream body &key status)
  "Write one JSON response and close the HTTP fixture connection."
  (let ((text (if body (agentcomms:json-encode body) "")))
    (format stream "HTTP/1.1 ~A~C~CContent-Type: application/json~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
            (or status (if body "200 OK" "202 Accepted")) #\Return #\Newline #\Return #\Newline
            (length text) #\Return #\Newline #\Return #\Newline #\Return #\Newline text)
    (finish-output stream))
  nil)

(serapeum/types:-> run-parent-acp-mcp-http-tests () integer)
(defun run-parent-acp-mcp-http-tests ()
  "Exercise real MCP HTTP discovery, private headers, tool execution, redaction, and cleanup."
  (let* ((listener (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp))
         (service  (make-instance 'parent-acp-service :request nil
                                  :configuration               (parent-adapter-test--configuration (uiop:temporary-directory))))
         (session  (make-instance 'parent-acp-session :service service :identifier "http-test"))
         (thread nil) (failure nil) (stopping-p nil) (methods nil) (headers-seen nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 8)
           (setf thread
                 (bordeaux-threads:make-thread
                  (lambda ()
                     (catch 'fixture-stop
                       (handler-case
                        (loop until stopping-p
                              do (handler-case
                                     (let* ((socket (sb-sys:with-deadline (:seconds 0.1)
                                                      (sb-bsd-sockets:socket-accept listener)))
                                            (stream (sb-bsd-sockets:socket-make-stream
                                                     socket :input t :output t :element-type 'character
                                                     :external-format ':utf-8 :buffering ':none)))
                                       (unwind-protect
                                            (multiple-value-bind (line headers request)
                                                (parent-adapter-test--http-request stream)
                                              (declare (ignore line))
                                              (setf headers-seen (append headers headers-seen))
                                              (let* ((method (and request (gethash "method" request)))
                                                     (result
                                                       (cond
                                                         ((equal method "initialize")
                                                          (agentcomms:json-object
                                                           "protocolVersion" (gethash "protocolVersion" (gethash "params" request))
                                                           "capabilities" (agentcomms:json-object "tools" (agentcomms:json-object))
                                                           "serverInfo" (agentcomms:json-object "name" "fixture" "version" "1")))
                                                         ((equal method "tools/list")
                                                          (agentcomms:json-object
                                                           "tools" (vector (agentcomms:json-object
                                                                            "name" "echo" "description" "synthetic-secret description"
                                                                            "inputSchema" (agentcomms:json-object "type" "object")))))
                                                         ((equal method "tools/call")
                                                          (agentcomms:json-object "content" (vector (agentcomms:json-object
                                                                                                     "type" "text" "text" "synthetic-secret answer")))))))
                                                (push method methods)
                                                (parent-adapter-test--http-reply
                                                 stream (when result (agentcomms:json-object
                                                                      "jsonrpc" "2.0" "id" (gethash "id" request) "result" result))
                                                        :status (unless request "405 Method Not Allowed"))))
                                         (close stream :abort t)))
                                   (sb-sys:deadline-timeout () nil)))
                        (error (condition) (setf failure condition)))))
                  :name "parent MCP HTTP fixture"))
           (setf (parent-acp-session-mode session) "full-access")
           (let* ((descriptors (parent-acp-mcp-open
                                session (list (agentcomms:json-object
                                               "type" "http" "name" "fixture"
                                               "url" (format nil "http://127.0.0.1:~D/mcp"
                                                             (nth-value 1 (sb-bsd-sockets:socket-name listener)))
                                               "headers" (vector (agentcomms:json-object
                                                                  "name" "X-Test-Credential" "value" "synthetic-secret"))))))
                  (descriptor  (aref descriptors 0))
                  (reply       (parent-acp-mcp--operation
                                session (agentcomms:json-object "operation" "call" "server_id" (gethash "serverId" descriptor)
                                                               "tool_name" "echo" "arguments" (agentcomms:json-object))
                                (constantly nil))))
             (assert (equal (gethash "description" descriptor) "[credential] description"))
             (assert (search "[credential] answer" (agentcomms:json-encode reply)))
             (assert (not (search "synthetic-secret" (agentcomms:json-encode reply)))))
           (parent-acp-mcp-close session)
           (assert (zerop (hash-table-count (parent-acp-session-mcp session))))
           (setf stopping-p t)
           (when (sb-thread:thread-alive-p thread)
             (sb-thread:interrupt-thread thread (lambda () (throw 'fixture-stop nil))))
           (assert (not (eq (sb-thread:join-thread thread :timeout 2 :default ':timeout) ':timeout)))
           (when failure (error failure))
           (assert (member "tools/call" methods :test #'equal))
           (assert (some (lambda (header) (search "synthetic-secret" header)) headers-seen))
           7)
      (parent-acp-mcp-close session)
      (setf stopping-p t)
      (when (and thread (sb-thread:thread-alive-p thread))
        (sb-thread:interrupt-thread thread (lambda () (throw 'fixture-stop nil))))
      (when thread (sb-thread:join-thread thread :timeout 2 :default nil))
      (sb-bsd-sockets:socket-close listener))))

(serapeum/types:-> run-parent-interface-adapters-tests () integer)
(defun run-parent-interface-adapters-tests ()
  "Run real local endpoint and focused ACP boundary checks."
  (+ (run-parent-detached-adapter-tests) (run-parent-acp-adapter-tests)
     (run-parent-acp-permission-tests) (run-parent-detached-attachment-tests)
     (run-parent-acp-mcp-http-tests)))
