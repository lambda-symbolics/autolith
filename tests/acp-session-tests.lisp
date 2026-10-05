(in-package #:autolith)

;;;; -- ACP Session Wire Tests --

(defclass acp-session-test-client (agentcomms:acp-client)
  ((updates :initform nil :accessor acp-session-test-updates)
   (permission-choice :initform "once" :accessor acp-session-test-permission-choice))
  (:documentation "A deterministic ACP client for session lifecycle tests."))

(defmethod agentcomms:client-session-update
    ((client acp-session-test-client) session-id update params)
  (declare (ignore session-id params))
  (push update (acp-session-test-updates client))
  nil)

(defmethod agentcomms:client-request-permission
    ((client acp-session-test-client) session-id tool-call options params)
  (declare (ignore client session-id tool-call options params))
  (values ':selected "once"))

(defun acp-session-test-connect (service client)
  "Connect CLIENT to SERVICE over an in-memory ACP channel pair."
  (multiple-value-bind (server client-channel) (agentcomms:make-acp-channel-pair)
    (agentcomms:acp-agent-connect service server)
    (agentcomms:acp-client-connect client client-channel)
    (agentcomms:client-initialize client)
    client))

(defun acp-session-test-text-result (text)
  "Return a deterministic provider result containing TEXT."
  (agent-test-result "acp-test-response" (list (agent-test-message text))))

(-> acp-session-test--call-with-client
    (configuration function &key (:results list) (:provider (option model-provider))) t)
(defun acp-session-test--call-with-client (configuration function &key results provider)
  "Run FUNCTION with the actual serving lifetime, a scripted provider, and owned cleanup."
  (let* ((provider (or provider (make-instance 'scripted-provider
                                               :configuration configuration :results results)))
         (service (make-instance 'acp-service :configuration configuration))
         (client (make-instance 'acp-session-test-client))
         (failure nil))
    (test-call-with-function-replacements
     (list (list 'provider-create (lambda (ignored &rest options)
                                    (declare (ignore ignored options)) provider)))
     (lambda ()
       (multiple-value-bind (server client-channel) (agentcomms:make-acp-channel-pair)
         (let ((owner (make-thread
                       (lambda ()
                         (handler-case (acp-service-serve service server)
                           (serious-condition (condition) (setf failure condition))))
                       :name "ACP test service")))
           (unwind-protect
                (progn
                  (agentcomms:acp-client-connect client client-channel)
                  (agentcomms:client-initialize client)
                  (funcall function service client))
             (when (agentcomms:acp-client-connection client)
               (agentcomms:connection-close (agentcomms:acp-client-connection client)))
             (agentcomms:channel-close server)
             (join-thread owner))
           (when failure (error failure))))))))

(defclass acp-session-test-gated-provider (scripted-provider)
  ((lock :initform (make-lock "ACP gated provider") :reader acp-session-test-gated-lock)
   (condition :initform (make-condition-variable :name "ACP gated provider")
              :reader acp-session-test-gated-condition)
   (entered-p :initform nil :accessor acp-session-test-gated-entered-p)
   (released-p :initform nil :accessor acp-session-test-gated-released-p))
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

(defun acp-session-test-gated-wait (provider)
  "Wait until PROVIDER has entered its blocking request."
  (with-lock-held ((acp-session-test-gated-lock provider))
    (loop until (acp-session-test-gated-entered-p provider)
          do (condition-wait (acp-session-test-gated-condition provider)
                             (acp-session-test-gated-lock provider)))))

(defun acp-session-test-gated-release (provider)
  "Release PROVIDER's blocking request."
  (with-lock-held ((acp-session-test-gated-lock provider))
    (setf (acp-session-test-gated-released-p provider) t)
    (condition-notify (acp-session-test-gated-condition provider))))

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
           (test-assert (acp-session-closed-p session) "close completes resource retirement"))
         (let ((lease (conversation-lease-acquire configuration identifier)))
           (conversation-lease-release lease))))))
  nil)

(-> test-acp-session-busy-admission-is-owner-safe () null)
(defun test-acp-session-busy-admission-is-owner-safe ()
  "Reject nested operations without releasing an existing claim, including the same owner."
  (with-test-configuration (configuration)
    (let ((service (make-instance 'acp-service :configuration configuration)) (owner (gensym)))
      (acp-service--call-with-operation
       service owner
       (lambda ()
         (dolist (candidate (list owner (gensym)))
           (test-assert
            (handler-case (progn (acp-service--call-with-operation service candidate (lambda () nil)) nil)
              (agentcomms:acp-method-error () t))
            "overlapping primary operations are rejected")
           (test-assert (eq owner (acp-service-busy-owner service))
                        "a rejected operation cannot release the active claim"))))
      (test-assert (null (acp-service-busy-owner service)) "the successful owner releases its claim")))
  nil)


(-> test-acp-session-cancel-blocked-provider-and-reuse () null)
(defun test-acp-session-cancel-blocked-provider-and-reuse ()
  "Cancel a blocked real-channel prompt and prove the next prompt is admitted."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                    ;; Cancellation may arrive after the first response is consumed.
                                    :results (list (acp-session-test-text-result "first")
                                                   (acp-session-test-text-result "second")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (result nil)
                (thread (make-thread
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
                   (let ((session (acp-service--session service identifier))
                         (deadline (+ (get-internal-real-time)
                                      (* 5 internal-time-units-per-second))))
                     (loop until (with-lock-held ((acp-session-lock session))
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
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (working-directory (config :working-directory
                                         (application-configuration
                                          (acp-session-application session)))))
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
          (session nil))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (setf identifier (agentcomms:client-new-session client (namestring root))
               session (acp-service--session service identifier))
         (agentcomms:connection-close (agentcomms:acp-client-connection client))))
      (test-assert (acp-session-closed-p session) "disconnect completes session cleanup")
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
          (live nil)
          (titles nil))
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
                             (list (agent-test-call :call-id (format nil "call-~D" number)
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

(-> test-acp-session-close-timeout-retains-ownership () null)
(defun test-acp-session-close-timeout-retains-ownership ()
  "Keep a non-cooperative owner and its lease until cancellation can complete."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (lock (make-lock "ACP non-cooperative owner"))
              (condition (make-condition-variable))
              (entered-p nil)
              (released-p nil)
              (owner
               (make-thread
                (lambda ()
                  (handler-case
                      (unwind-protect
                           (sb-sys:without-interrupts
                               (with-lock-held ((acp-session-lock session))
                                 (incf (acp-session-epoch session))
                                 (setf (acp-session-prompt-thread session) (current-thread)
                                       (acp-session-prompt-interruptible-p session) t))
                             (with-lock-held (lock)
                               (setf entered-p t)
                               (condition-notify condition)
                               (loop until released-p do (condition-wait condition lock))))
                        (with-lock-held ((acp-session-lock session))
                          (setf (acp-session-prompt-thread session) nil
                                (acp-session-prompt-interruptible-p session) nil)))
                    (application-turn-cancelled () nil)))
                :name "ACP non-cooperative prompt")))
         (unwind-protect
              (progn
                (with-lock-held (lock)
                  (loop until entered-p do (condition-wait condition lock)))
                (let ((*acp-session-close-seconds* 0.02))
                  (test-assert (handler-case (progn (acp-session-close session) nil)
                                 (agentcomms:acp-state-error () t))
                               "close reports its cooperative deadline"))
                (test-assert (eq session (acp-service--session service identifier))
                             "failed close retains service ownership")
                (test-assert
                 (handler-case
                     (let ((lease (conversation-lease-acquire configuration identifier)))
                       (conversation-lease-release lease)
                       nil)
                   (error () t))
                 "failed close retains the conversation lease"))
           (with-lock-held (lock)
             (setf released-p t)
             (condition-notify condition))
           (join-thread owner))
         (agentcomms:client-close-session client identifier)
         (test-assert (acp-session-closed-p session) "close succeeds after the owner unwinds")
         (let ((lease (conversation-lease-acquire configuration identifier)))
           (conversation-lease-release lease))))))
  nil)

(-> test-acp-session-delayed-cancel-during-finalization () null)
(defun test-acp-session-delayed-cancel-during-finalization ()
  "Deliver a queued cancellation during abort repair and then reuse the session."
  (with-test-configuration (configuration root)
    (let* ((provider (make-instance 'acp-session-test-gated-provider
                                    :configuration configuration
                                    :results (list (acp-session-test-text-result "discarded")
                                                   (acp-session-test-text-result "next"))))
           (lock (make-lock "ACP captured interrupt"))
           (condition (make-condition-variable))
           (target nil)
           (callback nil)
           (deliveries 0)
           (repairs 0)
           (interrupt (symbol-function 'interrupt-thread))
           (record-aborted (symbol-function 'application--record-turn-aborted)))
      (test-call-with-function-replacements
       (list
        (list 'interrupt-thread
              (lambda (thread function)
                (with-lock-held (lock)
                  (setf target thread callback function)
                  (condition-notify condition))))
        (list 'application--record-turn-aborted
              (lambda (application cause &rest options)
                (incf repairs)
                (funcall interrupt target (lambda () (incf deliveries) (funcall callback)))
                (apply record-aborted application cause options))))
       (lambda ()
         (acp-session-test--call-with-client
          configuration
          (lambda (service client)
            (declare (ignore service))
            (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                   (result nil)
                   (failure nil)
                   (owner (make-thread
                           (lambda ()
                             (handler-case
                                 (setf result (agentcomms:client-prompt
                                               client identifier
                                               (list (agentcomms:acp-text-content "cancelled"))))
                               (serious-condition (cause) (setf failure cause)))))))
              (unwind-protect
                   (progn
                     (acp-session-test-gated-wait provider)
                     (agentcomms:client-cancel client identifier)
                     (with-lock-held (lock)
                       (loop until callback do (condition-wait condition lock))))
                (acp-session-test-gated-release provider)
                (join-thread owner))
              (when failure (error failure))
              (test-assert (eq ':cancelled result) "queued cancellation returns the cancelled outcome")
              (test-assert (and (= 1 repairs) (= 1 deliveries))
                           "the queued interrupt runs without interrupting durable repair")
              (test-assert
               (eq ':end-turn (agentcomms:client-prompt
                               client identifier (list (agentcomms:acp-text-content "after repair"))))
               "the repaired session admits another prompt")))
          :provider provider)))))
  nil)


(-> test-acp-session-flushes-thoughts-on-prompt-exit () null)
(defun test-acp-session-flushes-thoughts-on-prompt-exit ()
  "Deliver pending thoughts before success, cancellation, or error replies."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let ((identifier (agentcomms:client-new-session client (namestring root)))
             (original (make-condition 'simple-error :format-control "Original turn failure")))
         (dolist (outcome '(:success :cancelled :error :flush-error))
           (setf (acp-session-test-updates client) nil)
           (test-call-with-function-replacements
            (append
             (list
              (list 'agent-run-user-turn
                    (lambda (agent text &key observer)
                      (declare (ignore agent text))
                      (agent-observer-reasoning observer "pending ")
                      (agent-observer-reasoning observer "thought")
                      (ecase outcome
                        (:success nil)
                        (:cancelled (error 'application-turn-cancelled))
                        ((:error :flush-error) (error original))))))
             (when (eq outcome ':flush-error)
               (list (list 'acp-observer-flush
                           (lambda (observer)
                             (declare (ignore observer))
                             (error "Flush failed"))))))
            (lambda ()
              (case outcome
                (:flush-error
                 (handler-case
                     (progn
                       (agentcomms:agent-prompt service identifier
                                               (list (agentcomms:acp-text-content "turn")) nil)
                       (test-assert nil "the original turn fails"))
                   (serious-condition (condition)
                     (test-assert (eq original condition)
                                  "cleanup failure does not replace the original condition"))))
                (:error
                 (test-assert
                  (handler-case
                      (progn
                        (agentcomms:client-prompt client identifier
                                                 (list (agentcomms:acp-text-content "turn")))
                        nil)
                    (agentcomms:acp-remote-error () t))
                  "a failed turn returns a protocol error"))
                (otherwise
                 (test-assert
                  (eq (if (eq outcome ':success) ':end-turn ':cancelled)
                      (agentcomms:client-prompt client identifier
                                               (list (agentcomms:acp-text-content "turn"))))
                  "the prompt returns its terminal outcome")))))
           (unless (eq outcome ':flush-error)
             (let ((updates (reverse (acp-session-test-updates client))))
               (test-assert
                (and (= 1 (length updates))
                     (eq ':agent-thought-chunk (agentcomms:acp-update-kind (first updates)))
                     (equal "pending thought"
                            (agentcomms:acp-content-text
                             (agentcomms:json-get (first updates) "content"))))
                "the client handles the final thought batch before the terminal reply"))))))))
  nil)
