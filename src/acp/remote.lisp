(in-package #:autolith)

;;;; -- Remote ACP Endpoints --

(defparameter *acp-remote-default-host* "127.0.0.1"
  "The address remote ACP listens on when the endpoint names no host.")

(defclass acp-endpoint ()
  ((transport
    :initarg       :transport
    :reader        acp-endpoint-transport
    :type          (member :http :websocket)
    :documentation "Streamable HTTP with WebSocket on the same endpoint, or WebSocket alone.")
   (host
    :initarg       :host
    :reader        acp-endpoint-host
    :type          string
    :documentation "The address to listen on.")
   (port
    :initarg       :port
    :reader        acp-endpoint-port
    :type          (integer 0 65535)
    :documentation "The port to listen on; 0 lets the system choose.")
   (token
    :initarg       :token
    :initform      nil
    :reader        acp-endpoint-token
    :type          (or null string)
    :documentation "The bearer token clients must present, or NIL for none."))
  (:documentation "Where and how a remote ACP server listens."))

(-> acp-endpoint-parse (keyword (or null string) &key (:token (or null string))) acp-endpoint)
(defun acp-endpoint-parse (transport text &key token)
  "Return the TRANSPORT endpoint TEXT names as [HOST:]PORT.

Without TEXT, or without a host, the endpoint is loopback only. An IPv6
address goes in brackets, as in [::1]:8737."
  (flet ((fail (control &rest arguments)
           (error 'configuration-error :message (apply #'format nil control arguments))))
    (multiple-value-bind (host port-text)
        (cond
          ((null text)
           (values *acp-remote-default-host* nil))
          ((and (plusp (length text)) (char= (char text 0) #\[))
           (let ((close (position #\] text)))
             (unless (and close
                          (< (1+ close) (length text))
                          (char= (char text (1+ close)) #\:))
               (fail "Write an IPv6 endpoint as [ADDRESS]:PORT, not ~A." text))
             (values (subseq text 1 close) (subseq text (+ close 2)))))
          ((> (count #\: text) 1)
           (fail "Put the IPv6 address in ~A in brackets, as in [::1]:8737." text))
          ((find #\: text)
           (let ((colon (position #\: text)))
             (values (subseq text 0 colon) (subseq text (1+ colon)))))
          (t
           (values *acp-remote-default-host* text)))
      (when (zerop (length host))
        (fail "The endpoint ~A names no host before its colon." text))
      (let ((port (cond
                    ((null port-text)
                     agentcomms:*acp-remote-default-port*)
                    ((and (plusp (length port-text))
                          (<= (length port-text) 5)
                          (every #'digit-char-p port-text)
                          (<= (parse-integer port-text) 65535))
                     (parse-integer port-text))
                    (t
                     (fail "The endpoint ~A has no port from 0 to 65535." text)))))
        (when (and token (zerop (length token)))
          (fail "The token is empty."))
        (make-instance 'acp-endpoint :transport transport
                                     :host      host
                                     :port      port
                                     :token     token)))))

(-> acp-endpoint-loopback-p (acp-endpoint) boolean)
(defun acp-endpoint-loopback-p (endpoint)
  "Return whether ENDPOINT listens only on this machine's loopback interface."
  (let ((host (string-downcase (acp-endpoint-host endpoint))))
    (and (or (string= host "localhost")
             (string= host "::1")
             (eql 0 (search "127." host)))
         t)))

(-> acp-endpoint-warning (acp-endpoint string) (or null string))
(defun acp-endpoint-warning (endpoint url)
  "Return the warning for serving ENDPOINT at URL beyond loopback, or NIL on loopback."
  (unless (acp-endpoint-loopback-p endpoint)
    (if (acp-endpoint-token endpoint)
        (format nil "Warning: ~A is not secure. The token and all traffic cross the network ~
                     unencrypted. Put a TLS proxy in front of it."
                url)
        (format nil "Warning: ~A is not secure. Anyone who can reach it controls an agent that ~
                     runs commands as you, and the traffic is unencrypted. Pass --token and put a ~
                     TLS proxy in front of it."
                url))))

(-> acp-serve-remote (acp-endpoint function) null)
(defun acp-serve-remote (endpoint serve-function)
  "Serve remote clients at ENDPOINT, calling SERVE-FUNCTION with each client's channel, until interrupted.

Stopping waits until every client released its sessions."
  (let* ((http-p (eq (acp-endpoint-transport endpoint) ':http))
         (server (handler-case
                     (agentcomms:acp-remote-server-start
                      serve-function
                      :host         (acp-endpoint-host endpoint)
                      :port         (acp-endpoint-port endpoint)
                      :token        (acp-endpoint-token endpoint)
                      :transports   (if http-p '(:http :websocket) '(:websocket))
                      :log-function #'agentcomms:acp-standard-error-log)
                   (agentcomms:acp-error (condition)
                     (error 'configuration-error :message (agentcomms:acp-error-message condition))))))
    (unwind-protect
         (let ((url (agentcomms:acp-remote-server-url server :scheme (if http-p "http" "ws"))))
           (format *error-output* "Autolith ACP is listening at ~A~:[~; and ~A~]~%"
                   url http-p (agentcomms:acp-remote-server-url server :scheme "ws"))
           (let ((warning (acp-endpoint-warning endpoint url)))
             (when warning
               (format *error-output* "~A~%" warning)))
           (finish-output *error-output*)
           (loop (sleep 3600)))
      (agentcomms:acp-remote-server-stop server)))
  nil)
