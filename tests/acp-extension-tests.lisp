(in-package #:autolith)

;;;; -- Typed Extension Boundary Tests --

(-> test-acp-extension-portable-values () null)
(defun test-acp-extension-portable-values ()
  "Copy JSON values without sharing mutable containers or printing opaque objects."
  (let* ((source (agentcomms:json-object "array" (vector "a" (argo:json-false) ':null)))
         (copy (acp-extension-value source))
         (array (gethash "array" copy)))
    (test-assert (not (eq source copy)) "extension objects are copied")
    (test-assert (not (eq array (gethash "array" source))) "extension arrays are copied")
    (test-assert (argo:json-false-p (aref array 1)) "false is distinct from null")
    (test-assert (eq ':null (aref array 2)) "unknown data is null")
    (test-assert (stringp (agentcomms:json-encode copy)) "the projection is JSON encodable"))
  (let ((circular (list 1)))
    (setf (rest circular) circular)
    (dolist (invalid (list circular (cons 1 2) (make-string-output-stream)
                           (make-string 262145 :initial-element #\x)))
      (test-assert
       (handler-case (progn (acp-extension-value invalid) nil)
         (acp-extension-unavailable () t))
       "nonportable or oversized values signal a bounded typed outcome")))
  nil)

(-> test-acp-extension-wire-state-and-authority () null)
(defun test-acp-extension-wire-state-and-authority ()
  "Negotiate extensions over ACP and require exact session authority for state."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((capabilities (agentcomms:client-agent-request
                             client "_autolith/capabilities" (agentcomms:json-object)))
              (value (agentcomms:json-get capabilities "value"))
              (identifier (agentcomms:client-new-session client (namestring root)))
              (params (agentcomms:json-object "sessionId" identifier))
              (state (agentcomms:client-agent-request client "_autolith/state" params)))
         (test-assert (equal "ok" (agentcomms:json-get capabilities "outcome"))
                      "capability negotiation returns a typed success")
         (test-assert (= 1 (agentcomms:json-get value "schemaVersion")) "schema version is explicit")
         (test-assert (find "_autolith/state" (agentcomms:json-get value "methods") :test #'equal)
                      "installed state is advertised")
         (test-assert (equal "ok" (agentcomms:json-get state "outcome")) "an owned session is readable")
         (let* ((row (agentcomms:json-get state "value"))
                (usage (agentcomms:json-get row "usage")))
           (test-assert (equal "idle" (agentcomms:json-get row "turnState")) "new sessions are idle")
           (test-assert (null (agentcomms:json-get usage "input")) "unmeasured input is unknown")
           (test-assert (argo:json-false-p (gethash "contextMeasured" usage))
                        "unmeasured context is not reported as measured")
           (test-assert (argo:json-false-p (gethash "planPresent" row)) "a missing plan is false"))
         (dolist (bad (list (agentcomms:json-object)
                            (agentcomms:json-object "sessionId" "foreign-session")
                            (agentcomms:json-object "sessionId" 7)))
           (test-assert
            (equal "condition" (agentcomms:json-get
                                 (agentcomms:client-agent-request client "_autolith/state" bad)
                                 "outcome"))
            "missing, malformed and foreign session identifiers fail before reading state"))
         (let* ((session (acp-service--session service identifier))
                (usage (agentcomms:json-object "input_tokens" 0 "output_tokens" 3 "cached_input_tokens" 0)))
           (agent-observer-status (acp-observer-create session) ':provider-request-completed
                                  (list :usage usage))
           (let ((measured (agentcomms:json-get
                            (agentcomms:json-get
                             (agentcomms:client-agent-request client "_autolith/state" params) "value")
                            "usage")))
             (test-assert (= 0 (agentcomms:json-get measured "input")) "a measured zero is retained")
             (test-assert (= 3 (agentcomms:json-get measured "output")) "observed output usage is retained")
             (test-assert (eq t (agentcomms:json-get measured "contextMeasured")) "measured zero is known")))))))
  nil)

(-> test-acp-extension-busy-and-notification-order () null)
(defun test-acp-extension-busy-and-notification-order ()
  "Reject overlapping mutations and allocate advisory notification sequences per session."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (params (agentcomms:json-object "sessionId" identifier))
              (*acp-extension-methods* (cons '("_autolith/test-write" identity :write)
                                             *acp-extension-methods*))
              (events nil))
         (acp-service--call-with-operation
          service session
          (lambda ()
            (let ((result (agentcomms:agent-extension-request service "_autolith/test-write" params)))
              (test-assert (equal "busy" (agentcomms:json-get result "outcome"))
                           "a rejected writer never reaches its handler")
              (test-assert (eq session (acp-service-busy-owner service))
                           "rejected extension writes retain the owner's primary claim"))))
         (test-call-with-function-replacements
          (list (list 'agentcomms:agent-client-notify
                      (lambda (owner method payload)
                        (declare (ignore owner method))
                        (push payload events))))
          (lambda ()
            (acp-extension-notify session "state")
            (acp-extension-notify session "jobs")))
         (test-assert (= (1+ (agentcomms:json-get (second events) "sequence"))
                         (agentcomms:json-get (first events) "sequence"))
                      "notifications carry consecutive per-session sequence numbers")
         (test-assert (every (lambda (event) (equal identifier (agentcomms:json-get event "sessionId"))) events)
                      "notifications identify their owning session")))))
  nil)

(-> test-acp-extension-close-active-handler () null)
(defun test-acp-extension-close-active-handler ()
  "Interrupt admitted readers before retiring resources, including after an earlier cancellation."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (dolist (cancelled-p '(nil t))
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session (acp-service--session service identifier))
                (entered-p nil) (cleaned-p nil) (outcome nil)
                (worker nil))
           (setf (acp-session-cancelled-p session) cancelled-p)
           (unwind-protect
                (progn
                  (setf worker
                        (make-thread
                         (lambda ()
                           (handler-case
                               (acp-extension--call
                                service
                                (list "test-read"
                                      (lambda (owner params)
                                        (declare (ignore owner params))
                                        (unwind-protect
                                             (progn (setf entered-p t) (sleep 30))
                                          (setf cleaned-p t))) ':read)
                                (agentcomms:json-object "sessionId" identifier))
                             (application-turn-cancelled () (setf outcome ':cancelled))))
                         :name "ACP extension reader fixture"))
                  (loop repeat 500 until entered-p do (sleep 0.01))
                  (test-assert entered-p "the reader entered its owned resource boundary")
                  (with-lock-held ((acp-session-lock session))
                    (incf (acp-session-epoch session)))
                  (acp-session-close session)
                  (join-thread worker)
                  (test-assert (eq outcome ':cancelled) "close interrupts an older admitted reader")
                  (test-assert cleaned-p "the reader unwinds before resource retirement")
                  (test-assert (null (application-conversation-lease (acp-session-application session)))
                               "retirement releases the lease after all readers finish"))
             (when (and worker (thread-alive-p worker))
               (interrupt-thread worker (lambda () (error 'application-turn-cancelled)))
               (join-thread worker))))))))
  nil)

(-> test-acp-extension-shutdown-admission () null)
(defun test-acp-extension-shutdown-admission ()
  "Reject new readers after service shutdown starts, before session retirement finishes."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (entered-p nil)
              (rejected-p nil))
         (with-lock-held ((acp-service-lock service))
           (setf (acp-service-closed-p service) t))
         (handler-case
             (acp-extension--call
              service
              (list "test-read" (lambda (owner params)
                                  (declare (ignore owner params))
                                  (setf entered-p t)) ':read)
              (agentcomms:json-object "sessionId" identifier))
           (agentcomms:acp-connection-closed () (setf rejected-p t)))
         (test-assert (and rejected-p (not entered-p))
                      "shutdown rejects readers before accessing resources")
         (test-assert (zerop (hash-table-count (acp-session-tool-threads session)))
                      "failed admission leaves no handler registration")))))
  nil)
