(in-package #:autolith)

;;;; -- ACP Session Wire Tests --

(defclass acp-session-test-client (agentcomms:acp-client)
  ((updates           :initform      nil
                      :accessor      acp-session-test-updates
                      :documentation "Session updates received from the agent, newest first.")
   (wire-events       :initform      nil
                      :accessor      acp-session-test-wire-events
                      :documentation "Chronological boundary, stream and permission evidence, newest first.")
   (permission-choice :initform      "once"
                      :accessor      acp-session-test-permission-choice
                      :documentation "Permission option identifier; the request method always selects \"once\"."))
  (:documentation "A deterministic ACP client for session lifecycle tests."))

(defmethod agentcomms:client-session-update
    ((client acp-session-test-client) session-id update &key params)
  "Record UPDATE as both a received update and an ordered wire event."
  (declare (ignore session-id params))
  (push update (acp-session-test-updates client))
  (push (cons ':update update) (acp-session-test-wire-events client))
  nil)

(defmethod agentcomms:client-extension-notification
    ((client acp-session-test-client) method params)
  "Retain typed lifecycle notifications in their wire arrival order."
  (when (equal method "_autolith/event")
    (push (cons ':extension params) (acp-session-test-wire-events client)))
  nil)

(-> acp-session-test-turn-events (acp-session-test-client) list)
(defun acp-session-test-turn-events (client)
  "Return ordered turn boundary payloads received by CLIENT."
  (loop for (kind . params) in (reverse (acp-session-test-wire-events client))
        when (and (eq kind ':extension) (equal "turn" (agentcomms:json-get params "kind")))
          collect (agentcomms:json-get params "data")))

(-> acp-session-test-check-turns (acp-session-test-client list list) null)
(defun acp-session-test-check-turns (client sources outcomes)
  "Check ordered epoch ownership around streamed updates and permission callbacks."
  (let ((active nil) (seen-sources nil) (seen-outcomes nil) (last-epoch -1))
    (dolist (event (reverse (acp-session-test-wire-events client)))
      (let ((kind (first event)) (params (rest event)))
        (cond
          ((and (eq kind ':extension) (equal "turn" (agentcomms:json-get params "kind")))
           (let* ((data  (agentcomms:json-get params "data"))
                  (epoch (agentcomms:json-get data "epoch")))
             (if (equal "running" (agentcomms:json-get data "state"))
                 (progn
                   (test-assert (and (null active) (> epoch last-epoch))
                                "each running boundary owns a fresh nonoverlapping epoch")
                   (setf active data last-epoch epoch))
                 (progn
                   (test-assert (and active (= epoch (agentcomms:json-get active "epoch")))
                                "idle closes the exact active epoch")
                   (push (agentcomms:json-get data "source") seen-sources)
                   (push (agentcomms:json-get data "outcome") seen-outcomes)
                   (setf active nil)))))
          ((member kind '(:update :permission))
           (test-assert active "stream and permission callbacks occur inside an owned turn")))))
    (test-assert (null active) "all observed turns finish")
    (test-assert (equal sources (nreverse seen-sources)) "turn sources retain their admission order")
    (test-assert (equal outcomes (nreverse seen-outcomes)) "turn outcomes preserve cancellation and failure"))
  nil)

(defmethod agentcomms:client-request-permission
    ((client acp-session-test-client) session-id tool-call &key options params)
  "Record the permission request for TOOL-CALL and allow it once."
  (declare (ignore session-id options params))
  (push (cons ':permission tool-call) (acp-session-test-wire-events client))
  (values ':selected "once"))

(-> acp-session-test-connect (agentcomms:acp-agent acp-session-test-client) acp-session-test-client)
(defun acp-session-test-connect (service client)
  "Connect CLIENT to SERVICE over an in-memory ACP channel pair."
  (multiple-value-bind (server client-channel) (agentcomms:make-acp-channel-pair)
    (agentcomms:acp-agent-connect service server)
    (agentcomms:acp-client-connect client client-channel)
    (agentcomms:client-initialize client)
    client))

(-> acp-session-test-text-result (string) provider-result)
(defun acp-session-test-text-result (text)
  "Return a deterministic provider result containing TEXT."
  (agent-test-result "acp-test-response" (list (agent-test-message text))))

(defvar *acp-session-test-server* nil
  "The world server hosting the channels of the ACP test running now.")

(-> acp-session-test--application (acp-session) application)
(defun acp-session-test--application (session)
  "Return the application of the channel SESSION follows, as the test's world server hosts it."
  (world-host-application
   (or (world-server--find-host *acp-session-test-server* (channel-subscription-identifier session))
       (error "Channel ~A is not hosted." (channel-subscription-identifier session)))))

(-> acp-session-test--call-with-client
    (configuration function &key (:results list) (:provider (option model-provider))) t)
(defun acp-session-test--call-with-client (configuration function &key results provider)
  "Run FUNCTION with a served ACP endpoint whose channels a world server in this process hosts with a scripted provider."
  (let* ((provider (or provider (make-instance 'scripted-provider
                                               :configuration configuration :results results)))
         (service  (make-instance 'acp-service :configuration configuration))
         (client   (make-instance 'acp-session-test-client))
         (failure  nil))
    (test-call-with-function-replacements
     (list (list 'provider-create (lambda (ignored &rest options)
                                    (declare (ignore ignored options)) provider)))
     (lambda ()
       (world-server-tests--call-with-server
        configuration
        (lambda (server)
          (let ((*acp-session-test-server* server))
            (multiple-value-bind (endpoint client-channel) (agentcomms:make-acp-channel-pair)
              (let ((owner (make-thread
                            (lambda ()
                              (handler-case (acp-service-serve service endpoint)
                                (serious-condition (condition) (setf failure condition))))
                            :name "ACP test service")))
                (unwind-protect
                     (progn
                       (agentcomms:acp-client-connect client client-channel)
                       (agentcomms:client-initialize client)
                       (funcall function service client))
                  (when (agentcomms:acp-client-connection client)
                    (agentcomms:connection-close (agentcomms:acp-client-connection client)))
                  (agentcomms:channel-close endpoint)
                  (join-thread owner))
                (when failure (error failure))))))
        :permission-mode ':ask)))))

(defclass acp-session-test-gated-provider (scripted-provider)
  ((lock       :initform      (make-lock "ACP gated provider")
               :reader        acp-session-test-gated-lock
               :documentation "Lock guarding the gate state.")
   (condition  :initform      (make-condition-variable :name "ACP gated provider")
               :reader        acp-session-test-gated-condition
               :documentation "Condition variable signaled when the gate state changes.")
   (entered-p  :initform      nil
               :accessor      acp-session-test-gated-entered-p
               :documentation "True once the first provider request has reached the gate.")
   (released-p :initform      nil
               :accessor      acp-session-test-gated-released-p
               :documentation "True once the test has released the gated request."))
  (:documentation "A provider that waits at a deterministic cancellation boundary."))

(defmethod provider-stream-turn :around
    ((provider acp-session-test-gated-provider) conversation &rest options)
  "Hold the first provider request until the test releases its gate."
  (declare (ignore conversation))
  (with-lock-held ((acp-session-test-gated-lock provider))
    (unless (acp-session-test-gated-entered-p provider)
      (setf (acp-session-test-gated-entered-p provider) t)
      (condition-notify (acp-session-test-gated-condition provider))
      (loop until (acp-session-test-gated-released-p provider)
            do (condition-wait (acp-session-test-gated-condition provider)
                               (acp-session-test-gated-lock provider)))))
  (apply #'call-next-method provider conversation options))

(-> acp-session-test-gated-wait (acp-session-test-gated-provider) null)
(defun acp-session-test-gated-wait (provider)
  "Wait until PROVIDER has entered its blocking request."
  (with-lock-held ((acp-session-test-gated-lock provider))
    (loop until (acp-session-test-gated-entered-p provider)
          do (condition-wait (acp-session-test-gated-condition provider)
                             (acp-session-test-gated-lock provider)))))

(-> acp-session-test-gated-release (acp-session-test-gated-provider) null)
(defun acp-session-test-gated-release (provider)
  "Release PROVIDER's blocking request."
  (with-lock-held ((acp-session-test-gated-lock provider))
    (setf (acp-session-test-gated-released-p provider) t)
    (condition-notify (acp-session-test-gated-condition provider)))
  nil)

(-> test-acp-session-wire-new-prompt-replay-close () null)
(defun test-acp-session-wire-new-prompt-replay-close ()
  "Exercise durable prompt history, ordered replay, close, and conversation lease reuse."
  (with-test-configuration (configuration root)
    (let ((identifier nil))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (declare (ignore service))
         (setf identifier (agentcomms:client-new-session client (namestring root)))
         (test-assert (stringp identifier) "session/new returns a durable identifier")
         (agentcomms:client-prompt client identifier (list (agentcomms:acp-text-content "hello")))
         (test-assert (zerop (count ':user-message-chunk (acp-session-test-updates client)
                                   :key #'agentcomms:acp-update-kind))
                      "the editor's prompt is not echoed during its live turn")
         (test-assert (= 1 (count ':agent-message-chunk (acp-session-test-updates client)
                                  :key #'agentcomms:acp-update-kind))
                      "a nonstreamed provider response is emitted once")
         (agentcomms:client-close-session client identifier))
       :results (list (acp-session-test-text-result "replayed")))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (agentcomms:client-load-session client identifier (namestring root))
         (let ((history (reverse (acp-session-test-updates client))))
           (test-assert (equal '(:user-message-chunk :agent-message-chunk :plan)
                               (mapcar #'agentcomms:acp-update-kind history))
                        "load replays chronological messages followed by the current plan")
           (test-assert (equal "replayed" (agentcomms:acp-content-text
                                           (agentcomms:json-get (second history) "content")))
                        "load returns the durable assistant response"))
         (let ((session (acp-service--session service identifier)))
           (agentcomms:client-close-session client identifier)
           (test-assert (channel-subscription-closed-p session) "close leaves the channel"))
         (test-assert (task-tests--wait-until
                       (lambda ()
                         (notany (lambda (host) (equal (world-host-channel host) identifier))
                                 (with-lock-held ((world-server-lock *acp-session-test-server*))
                                   (copy-list (world-server-hosts *acp-session-test-server*)))))
                       10)
                      "closing a session closes the channel the server started for it")
         (let ((lease (conversation-lease-acquire configuration identifier)))
           (conversation-lease-release lease))))))
  nil)

(-> test-acp-session-cancel-blocked-provider-and-reuse () null)
(defun test-acp-session-cancel-blocked-provider-and-reuse ()
  "Cancel a blocked real-channel prompt and prove the next prompt is admitted."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                    ;; Cancellation may arrive after the first response is consumed.
                                    :results      (list (acp-session-test-text-result "first")
                                                        (acp-session-test-text-result "second")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (result     nil)
                (thread     (make-thread
                             (lambda ()
                               (setf result
                                     (handler-case
                                         (agentcomms:client-prompt
                                          client identifier
                                          (list (agentcomms:acp-text-content "blocked")))
                                       (agentcomms:acp-error (condition)
                                         condition)))))))
           (acp-session-test-gated-wait provider)
           (unwind-protect
                (progn
                  (agentcomms:client-cancel client identifier)
                  ;; The wire notification has no reply; observe server admission before releasing work.
                  (let ((session  (acp-service--session service identifier))
                        (deadline (+ (get-internal-real-time)
                                     (* 5 internal-time-units-per-second))))
                    (loop until (with-lock-held ((channel-subscription-lock session))
                                  (acp-session-cancelled-p session))
                          do (when (> (get-internal-real-time) deadline)
                               (error "The ACP service did not admit the cancellation notification."))
                             (sleep 0.001))))
             (acp-session-test-gated-release provider))
           (join-thread thread)
           (test-assert (or (eq result ':cancelled)
                            (and (typep result 'agentcomms:acp-error)
                                 (search "cancel" (string-downcase (format nil "~A" result)))))
                        "a blocked real-channel prompt reports cancellation")
           (test-assert (eq ':end-turn
                            (agentcomms:client-prompt
                             client identifier
                             (list (agentcomms:acp-text-content "after cancel"))))
                        "a cancelled session admits the next prompt")))
       :provider provider))))

(-> test-acp-session-cwd-mode-and-unsupported-content () null)
(defun test-acp-session-cwd-mode-and-unsupported-content ()
  "Exercise session-local cwd, mode changes, and unsupported prompt rejection."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier        (agentcomms:client-new-session client (namestring root)))
              (session           (acp-service--session service identifier))
              (working-directory (config :working-directory
                                         (application-configuration
                                          (acp-session-test--application session)))))
         (test-assert (equal (platform-truename *platform* root)
                             (platform-truename *platform* working-directory))
                      "new sessions retain their own working directory")
         (agentcomms:client-set-mode client identifier "full-access")
         (test-assert (eq ':full-access (acp-session-mode session))
                      "mode changes are applied to the live session")
         (test-assert
          (handler-case
              (progn (agentcomms:client-prompt
                      client identifier
                      (list (agentcomms:acp-image-content "AA==" "image/png")))
                     nil)
            (agentcomms:acp-error () t))
          "unsupported binary prompt content is rejected on the wire"))))))

(-> test-acp-session-disconnect-releases-lease () null)
(defun test-acp-session-disconnect-releases-lease ()
  "Verify the real service lifetime closes a disconnected session and releases its lease."
  (with-test-configuration (configuration root)
    (let ((identifier nil)
          (session    nil))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (setf identifier (agentcomms:client-new-session client (namestring root))
               session (acp-service--session service identifier))
         (agentcomms:connection-close (agentcomms:acp-client-connection client))))
      (test-assert (channel-subscription-closed-p session) "disconnect completes session cleanup")
      (let ((lease (conversation-lease-acquire configuration identifier)))
        (unwind-protect
             (test-assert lease "disconnect releases the durable conversation lease")
          (conversation-lease-release lease)))))
  nil)

(-> acp-session-test--tool-identifiers (list keyword) list)
(defun acp-session-test--tool-identifiers (updates kind)
  "Return chronological tool identities from updates of KIND."
  (mapcar (lambda (update) (agentcomms:json-get update "toolCallId"))
          (remove-if-not (lambda (update) (eq kind (agentcomms:acp-update-kind update)))
                         (reverse updates))))

(-> test-acp-session-live-and-replayed-tool-identities () null)
(defun test-acp-session-live-and-replayed-tool-identities ()
  "Compare live and durable replay identities across two actual tool turns."
  (with-test-configuration (configuration root)
    (let ((identifier nil)
          (live       nil)
          (titles     nil))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (declare (ignore service))
         (setf identifier (agentcomms:client-new-session client (namestring root)))
         (dolist (text '("first" "second"))
           (agentcomms:client-prompt client identifier (list (agentcomms:acp-text-content text))))
         (setf live (acp-session-test--tool-identifiers (acp-session-test-updates client) ':tool-call))
         (test-assert (= 2 (length live)) "both live turns emit a tool declaration")
         (test-assert (= 2 (length (remove-duplicates live :test #'equal)))
                      "live tool identities are distinct across turns")
         (setf titles
               (loop for update in (reverse (acp-session-test-updates client))
                     when (eq ':tool-call (agentcomms:acp-update-kind update))
                       collect (agentcomms:json-get update "title")))
         (test-assert (every (lambda (title) (search "workspace:." title)) titles)
                      "live tool titles identify the resource being read")
         (test-assert
          (equal live (acp-session-test--tool-identifiers (acp-session-test-updates client)
                                                          ':tool-call-update))
          "live completions reference their declared calls")
         (agentcomms:client-close-session client identifier))
       :results (loop for number from 1 to 2 append
                      (list (agent-test-result
                             (format nil "tool-response-~D" number)
                             (list (agent-test-call :call-id   (format nil "call-~D" number)
                                                    :namespace "resource" :name "read"
                                                    :arguments "{\"uri\":\"workspace:.\"}")))
                            (acp-session-test-text-result "done"))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (declare (ignore service))
         (agentcomms:client-load-session client identifier (namestring root))
         (test-assert
          (equal live (acp-session-test--tool-identifiers (acp-session-test-updates client) ':tool-call))
          "replayed tool declarations reuse live identities")
         (test-assert
          (equal titles
                 (loop for update in (reverse (acp-session-test-updates client))
                       when (eq ':tool-call (agentcomms:acp-update-kind update))
                         collect (agentcomms:json-get update "title")))
          "replayed tool titles use their persisted arguments")
         (test-assert
          (equal live (acp-session-test--tool-identifiers (acp-session-test-updates client)
                                                          ':tool-call-update))
          "replayed results reference their original calls")))))
  nil)

(-> test-acp-session-flushes-thoughts-on-prompt-exit () null)
(defun test-acp-session-flushes-thoughts-on-prompt-exit ()
  "Deliver pending thoughts before a success or error reply."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (declare (ignore service))
       (let ((identifier (agentcomms:client-new-session client (namestring root)))
             (run-turn   (symbol-function 'agent-run-user-turn)))
         (dolist (outcome '(:success :error))
           (setf (acp-session-test-updates client) nil)
           (test-call-with-function-replacements
            (list
             (list 'agent-run-user-turn
                   (lambda (agent text &rest options &key observer &allow-other-keys)
                     (agent-observer-reasoning observer "pending ")
                     (agent-observer-reasoning observer "thought")
                     (if (eq outcome ':error)
                         (error "Original turn failure")
                         (apply run-turn agent text options)))))
            (lambda ()
              (if (eq outcome ':error)
                  (test-assert
                   (handler-case
                       (progn
                         (agentcomms:client-prompt client identifier
                                                  (list (agentcomms:acp-text-content "turn")))
                         nil)
                     (agentcomms:acp-remote-error () t))
                   "a failed turn returns a protocol error")
                  (test-assert
                   (eq ':end-turn
                       (agentcomms:client-prompt client identifier
                                                (list (agentcomms:acp-text-content "turn"))))
                   "the prompt returns its terminal outcome"))))
           (let ((thoughts (remove ':agent-thought-chunk (reverse (acp-session-test-updates client))
                                   :key #'agentcomms:acp-update-kind :test-not #'eq)))
             (test-assert
              (and (= 1 (length thoughts))
                   (equal "pending thought"
                          (agentcomms:acp-content-text
                           (agentcomms:json-get (first thoughts) "content"))))
              (format nil "the editor has the final thought batch before the ~(~A~) reply" outcome))))))
     :results (list (acp-session-test-text-result "done"))))
  nil)

(-> test-acp-session-replay-metadata-and-tool-statuses () null)
(defun test-acp-session-replay-metadata-and-tool-statuses ()
  "Exercise replay filtering at the ACP update boundary, including tool outcomes."
  (with-test-configuration (configuration root)
    (let ((identifier nil))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (setf identifier (agentcomms:client-new-session client (namestring root)))
         (let ((conversation
                 (application-conversation
                  (acp-session-test--application (acp-service--session service identifier)))))
           (dolist (record
                     (list
                      (list :message :role ':user
                            :content "literal (:message :automatic-p t)")
                      (list :message :role ':user :automatic-p t
                            :content "(:message :automatic-p t)")
                      (list :provider-item
                            :wire-json
                            "{\"type\":\"function_call\",\"call_id\":\"ok-call\",\"name\":\"resource.read\",\"arguments\":\"{}\"}")
                      (list :provider-item
                            :wire-json
                            "{\"type\":\"function_call\",\"call_id\":\"legacy-call\",\"name\":\"resource.read\",\"arguments\":\"{}\"}")
                      (list :provider-item
                            :wire-json
                            "{\"type\":\"function_call\",\"call_id\":\"bad-call\",\"name\":\"shell.run\",\"arguments\":\"{}\"}")
                      (list :tool-result :call-id "ok-call" :tool "resource.read" :status ':ok
                            :output "completed output")
                      (list :tool-result :call-id "legacy-call" :tool "resource.read" :status ':success
                            :output "legacy output")
                      (list :tool-result :call-id "bad-call" :tool "shell.run" :status ':failure
                            :output "failed output")))
             (conversation-append-record
              conversation
              (if (eq (first record) ':tool-result)
                  (append record
                          (list :wire-json
                                (json-encode
                                 (function-call-output-item
                                  (getf (rest record) :call-id) (getf (rest record) :output)))))
                  record))))
         (agentcomms:client-close-session client identifier))
       :results nil)
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (declare (ignore service))
         (agentcomms:client-load-session client identifier (namestring root))
         (let* ((updates (reverse (acp-session-test-updates client)))
                (user-messages
                  (loop for update in updates
                        when (eq ':user-message-chunk (agentcomms:acp-update-kind update))
                          collect (agentcomms:acp-content-text
                                   (agentcomms:json-get update "content"))))
                (tool-calls
                  (remove-if-not
                   (lambda (update) (eq ':tool-call (agentcomms:acp-update-kind update)))
                   updates))
                (tool-updates
                  (remove-if-not
                   (lambda (update)
                     (eq ':tool-call-update (agentcomms:acp-update-kind update)))
                   updates)))
           (test-assert (equal '("literal (:message :automatic-p t)") user-messages)
                        "metadata, not internal-looking text, suppresses automatic messages")
           (test-assert
              (equal '("2:ok-call" "2:legacy-call" "2:bad-call")
                     (mapcar (lambda (update) (agentcomms:json-get update "toolCallId"))
                             tool-calls))
            "automatic turns advance the replay tool identity sequence")
           (test-assert
              (equal '("2:ok-call" "2:legacy-call" "2:bad-call")
                     (mapcar (lambda (update) (agentcomms:json-get update "toolCallId"))
                             tool-updates))
            "replayed tool results refer to their declared calls")
           (test-assert
              (equal '("completed" "completed" "failed")
                     (mapcar (lambda (update) (agentcomms:json-get update "status"))
                             tool-updates))
            "ok and legacy success statuses complete tools while failures fail")))
       :results nil))))

(-> test-acp-session-submission-sources () null)
(defun test-acp-session-submission-sources ()
  "Preserve queue and steering provenance through the channel and durable history."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                   :results       (loop repeat 4 collect (acp-session-test-text-result "answer")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session    (acp-service--session service identifier))
                (result     nil)
                (thread     (make-thread
                             (lambda ()
                               (setf result
                                     (handler-case
                                         (agentcomms:client-prompt
                                          client identifier (list (agentcomms:acp-text-content "prompt")))
                                       (error (condition) condition))))
                             :name "ACP provenance prompt")))
           (unwind-protect
                (progn
                  (acp-session-test-gated-wait provider)
                  (dolist (method '("_autolith/queue" "_autolith/steer"))
                    (test-assert
                     (equal "ok"
                            (agentcomms:json-get
                             (agentcomms:client-agent-request
                              client method (agentcomms:json-object "sessionId" identifier "text" method))
                             "outcome"))
                     "the channel admits queued and steering input while a prompt runs")))
             (acp-session-test-gated-release provider)
             (join-thread thread))
           (test-assert (eq result ':end-turn) "the prompt completes with its own submission")
           (with-lock-held ((channel-subscription-lock session))
             (test-assert
              (loop until (plusp (acp-session-idle-count session))
                    do (unless (condition-wait (channel-subscription-condition-variable session)
                                               (channel-subscription-lock session) :timeout 5)
                         (return nil))
                    finally (return t))
              "queued follow-up work reaches its terminal outcome"))
           (with-lock-held ((channel-subscription-event-lock session)))
           (let* ((conversation (application-conversation (acp-session-test--application session)))
                  (records      nil))
             (conversation-map-records
              conversation
              (lambda (record)
                (when (and (eq (first record) ':message)
                           (eq (getf (rest record) :role) ':user))
                  (push record records))))
             (setf records (nreverse records))
             (test-assert (equal '("prompt" "steering" "queue")
                                 (mapcar (lambda (record)
                                           (getf (getf (rest record) :submission) :source))
                                         records))
                          "durable records retain source across steering and follow-up queues")
             (test-assert (every (lambda (record)
                                   (equal (channel-subscription-client-identifier session)
                                          (getf (getf (rest record) :submission) :client)))
                                 records)
                          "all three records retain their submitting editor identity"))
           (test-assert (= 2 (count ':user-message-chunk (acp-session-test-updates client)
                                     :key #'agentcomms:acp-update-kind))
                        "only the live prompt is suppressed, not queued or steering input")
           (acp-session-test-check-turns
            client '("prompt" "steering" "queue") '("end-turn" "end-turn" "end-turn"))))
       :provider provider)))
  nil)

(-> test-acp-session-nonprose-prompts () null)
(defun test-acp-session-nonprose-prompts ()
  "Settle blank, command and Lisp prompts using their admitted outcomes."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (declare (ignore service))
       (let ((identifier (agentcomms:client-new-session client (namestring root))))
         (dolist (case '(("" :end-turn)
                         ("/status" :end-turn)
                         ("(+ 1 2)" :end-turn)
                         ("/no-such-acp-command" :end-turn)
                         ("(error \"local evaluation failed\")" :cancelled)))
           (destructuring-bind (text expected) case
             (let ((result (handler-case
                               (agentcomms:client-prompt client identifier
                                                        (list (agentcomms:acp-text-content text)))
                             (agentcomms:acp-remote-error () ':condition))))
               (test-assert (eq expected result)
                            (format nil "~S returns ~S, expected ~S" text result expected)))))))))
  nil)

(-> test-acp-session-busy-nonprose-prompts () null)
(defun test-acp-session-busy-nonprose-prompts ()
  "Complete responsive input immediately and defer held or boundary input until execution."
  (dolist (case '(("" :inline) ("/status" :inline) ("(status)" :inline)
                  ("/timestamps on" :apply) ("(+ 1 2)" :hold)))
    (destructuring-bind (text action) case
      (with-test-configuration (configuration root)
        (let ((provider (make-instance 'acp-session-test-gated-provider
                                       :configuration configuration
                                       :results       (list (acp-session-test-text-result "background answer")))))
          (acp-session-test--call-with-client
           configuration
           (lambda (service client)
             (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
                    (session     (acp-service--session service identifier))
                    (scheduled   (sb-thread:make-semaphore))
                    (enqueue     (symbol-function 'application-input-controller--enqueue))
                    (apply-input (symbol-function 'application-input-controller--schedule-apply))
                    (result      nil)
                    (thread      nil))
               (localgroup-client-call (channel-subscription-connection session) ':submit :text "background")
               (acp-session-test-gated-wait provider)
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list 'application-input-controller--enqueue
                            (lambda (controller kind input)
                              (prog1 (funcall enqueue controller kind input)
                                (when (equal text (user-message-input-text input))
                                  (sb-thread:signal-semaphore scheduled)))))
                      (list 'application-input-controller--schedule-apply
                            (lambda (controller input)
                              (prog1 (funcall apply-input controller input)
                                (sb-thread:signal-semaphore scheduled)))))
                     (lambda ()
                       (setf thread
                             (make-thread
                              (lambda ()
                                (setf result
                                      (handler-case
                                          (agentcomms:client-prompt client identifier
                                                                   (list (agentcomms:acp-text-content text)))
                                        (error (condition) condition))))
                              :name "ACP nonprose prompt"))
                       (unless (eq action ':inline)
                         (test-assert (sb-thread:wait-on-semaphore scheduled :timeout 5)
                                      "the input reaches its canonical deferred queue")
                         (test-assert (null result) "acceptance alone does not complete deferred input")
                         (acp-session-test-gated-release provider))
                       (test-assert (not (eq ':unfinished
                                            (sb-thread:join-thread thread :timeout 60 :default ':unfinished)))
                                    (format nil "~S reaches completion; queued ~S, pending ~S" text
                                            (application-input-controller--state
                                             (application-input-controller (acp-session-test--application session)) :work-items)
                                            (deque->list (application-input-controller-pending-apply-items
                                                          (application-input-controller (acp-session-test--application session))))))
                       (test-assert (eq result ':end-turn)
                                    (format nil "~S receives its own outcome, got ~S" text result))))
                 (acp-session-test-gated-release provider)
                 (when (and thread (sb-thread:thread-alive-p thread))
                   (channel-subscription-close session)
                   (sb-thread:join-thread thread :timeout 5 :default nil)))))
           :provider provider)))))
  nil)

(-> test-acp-session-disconnected-updates () null)
(defun test-acp-session-disconnected-updates ()
  "Handle editor closure between the service-state check and an update or flush."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session    (acp-service--session service identifier)))
         (test-assert (not (acp-service-closed-p service))
                      "the transport can close before service shutdown is observed")
         (test-call-with-function-replacements
          (list (list 'agentcomms:update-buffer-send
                      (lambda (buffer update)
                        (declare (ignore buffer update))
                        (error 'agentcomms:acp-connection-closed)))
                (list 'agentcomms:update-buffer-flush
                      (lambda (buffer)
                        (declare (ignore buffer))
                        (error 'agentcomms:acp-connection-closed))))
          (lambda ()
            (test-assert (null (acp-session--text session "late delta"))
                         "late stream delivery tolerates editor disconnection")
            (test-assert (null (acp-session-flush session))
                         "pending thought flush tolerates editor disconnection")))))))
  nil)

(-> test-acp-session-disconnect-cancels-active-submission () null)
(defun test-acp-session-disconnect-cancels-active-submission ()
  "Cancel an active submission over its installed transport after editor admission closes."
  (with-test-configuration (configuration root)
    (let ((provider     (make-instance 'acp-session-test-gated-provider
                                       :configuration configuration
                                       :results       (list (acp-session-test-text-result "interrupted"))))
          (cancel-count 0)
          (cancelled    (sb-thread:make-semaphore :count 0)))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (idle-id    (agentcomms:client-new-session client (namestring root)))
                (session    (acp-service--session service identifier))
                (transport  (channel-subscription-connection session))
                (call       (symbol-function 'localgroup-client-call))
                (thread     nil))
           (declare (ignore idle-id))
           (test-call-with-function-replacements
            (list (list 'localgroup-client-call
                        (lambda (connection command &rest arguments)
                          (when (eq command ':cancel)
                            (incf cancel-count)
                            (test-assert (eq connection transport)
                                         "shutdown cancels only the active editor's installed transport")
                            (test-assert (acp-service-closed-p service)
                                         "editor admission closes before cancellation is transmitted")
                            (test-assert
                             (handler-case
                                 (progn (channel-subscription-call session ':state) nil)
                               (agentcomms:acp-state-error () t))
                             "ordinary requests cannot use the shutdown cancellation path"))
                          (multiple-value-prog1 (apply call connection command arguments)
                            (when (eq command ':cancel)
                              (sb-thread:signal-semaphore cancelled))))))
            (lambda ()
              (unwind-protect
                   (progn
                     (setf thread
                           (make-thread
                            (lambda ()
                              (handler-case
                                  (agentcomms:client-prompt
                                   client identifier (list (agentcomms:acp-text-content "blocked")))
                                (agentcomms:acp-error () nil)))
                            :name "ACP disconnected prompt"))
                     (acp-session-test-gated-wait provider)
                     (agentcomms:connection-close (agentcomms:acp-client-connection client))
                     (test-assert (sb-thread:wait-on-semaphore cancelled :timeout 5)
                                  "disconnect transmits cancellation despite closed service admission"))
                (acp-session-test-gated-release provider)
                (when thread
                  (sb-thread:join-thread thread :timeout 5 :default nil)))))))
       :provider provider)
      (test-assert (= 1 cancel-count)
                   (format nil "Shutdown sent ~D cancellations; expected one for the active session."
                           cancel-count))))
  nil)

(-> test-acp-session-scheduled-mode-follows-state () null)
(defun test-acp-session-scheduled-mode-follows-state ()
  "Treat a busy turn's scheduled mode reply as acceptance, not an applied state."
  (with-test-configuration (configuration)
    (let* ((service  (make-instance 'acp-service :configuration configuration))
           (session  (make-instance 'acp-session :service service :identifier "channel"
                                                :configuration configuration :mode ':ask))
           (updates  nil)
           (requests nil))
      (setf (gethash "channel" (acp-service-sessions service)) session)
      (test-call-with-function-replacements
       (list (list 'channel-subscription-call
                   (lambda (recipient command &rest arguments)
                     (test-assert (eq recipient session) "mode admission uses the selected session")
                     (push (cons command arguments) requests)
                     '(:result "scheduled" :delivery "scheduled")))
             (list 'agentcomms:agent-send-update
                   (lambda (recipient identifier update)
                     (declare (ignore recipient identifier))
                     (push update updates)))
             (list 'acp-extension-notify
                   (lambda (&rest arguments) (declare (ignore arguments)) nil)))
       (lambda ()
         (agentcomms:agent-set-mode service "channel" "full-access")
         (test-assert (equal '((:mode :value :full-access)) requests)
                      "set-mode requests native mode admission once")
         (test-assert (eq ':ask (acp-session-mode session))
                      "scheduling does not change the editor's observed mode")
         (test-assert (null updates) "scheduling emits no optimistic current-mode update")
         (channel-subscription-event
          session '(:state :sequence 1 :state (:state :permission-mode :full-access)))
         (test-assert (eq ':full-access (acp-session-mode session))
                      "the actual state event changes the observed mode")
         (test-assert (and (= 1 (length updates))
                           (equal "full-access" (agentcomms:json-get (first updates) "currentModeId")))
                      "the applied state emits exactly one current-mode update")
         (channel-subscription-event
          session '(:state :sequence 2 :state (:state :permission-mode :full-access)))
         (test-assert (= 1 (length updates)) "repeated applied state does not duplicate the mode update")))))
  nil)
