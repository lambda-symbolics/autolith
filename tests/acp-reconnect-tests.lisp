(in-package #:autolith)

;;;; -- ACP Checkpoint Recovery --

(-> acp-reconnect-test--texts (acp-session-test-client) list)
(defun acp-reconnect-test--texts (client)
  "Return the assistant text chunks CLIENT received, oldest first."
  (loop for update in (reverse (acp-session-test-updates client))
        when (eq (agentcomms:acp-update-kind update) ':agent-message-chunk)
          collect (agentcomms:acp-content-text (agentcomms:json-get update "content"))))

(-> test-acp-session-checkpoint-reconnect () null)
(defun test-acp-session-checkpoint-reconnect ()
  "Reconnect the same editor session after successful and failed world checkpoints."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
              (session     (acp-service--session service identifier))
              (application (acp-session-test--application session)))
         (application-clients-publish application (list :present :text "before-restart"))
         (dolist (fail-p '(nil t))
           (let* ((previous (acp-session-connection session))
                  (during   (if fail-p "during-failed-restart" "during-restart"))
                  (after    (if fail-p "after-failed-restart" "after-restart")))
             (test-assert
              (eq (handler-case
                      (world-server-call-quiesced
                       *acp-session-test-server* application
                       (lambda ()
                        (funcall *checkpoint-thread-quiescer*
                                 (lambda ()
                                   (application-clients-publish application (list :present :text during))
                                   (when fail-p
                                     (error 'checkpoint-error :message "Injected save failure."
                                                              :stage ':fork :pathname nil))
                                   ':saved))))
                    (checkpoint-error () ':failed))
                  (if fail-p ':failed ':saved))
              "the checkpoint callback reports its actual outcome")
             (let ((reconnected-p
                     (task-tests--wait-until
                      (lambda ()
                        (let ((connection (acp-session-connection session)))
                          (and connection (not (eq previous connection))
                               (not (localgroup-client-connection-closed-p connection))
                               (not (acp-session-closed-p session)))))
                      10)))
              (test-assert reconnected-p
                           (format nil "the same ACP session reconnects automatically (closed ~S, restart ~S, failure ~S)"
                                   (acp-session-closed-p session)
                                   (acp-session-reconnect-id session)
                                   (acp-session-reconnect-failure session)))
              (unless reconnected-p
                (return-from test-acp-session-checkpoint-reconnect nil)))
             (test-assert (eq session (acp-service--session service identifier))
                          "reconnection retains the registered editor session")
             (application-clients-publish application (list :present :text after))
             (test-assert
              (task-tests--wait-until
               (lambda () (member after (acp-reconnect-test--texts client) :test #'equal)) 10)
              "the resumed subscription receives new output")))
         (agentcomms:client-prompt client identifier (list (agentcomms:acp-text-content "continue")))
         (test-assert (task-tests--wait-until
                       (lambda () (member "delta" (acp-reconnect-test--texts client) :test #'equal)) 2)
                      "the completed prompt's output reaches the editor")
         (let ((texts (acp-reconnect-test--texts client)))
           (dolist (text '("before-restart" "during-restart" "after-restart"
                           "during-failed-restart" "after-failed-restart" "delta"))
             (test-assert (= 1 (count text texts :test #'equal))
                          (format nil "~S is delivered exactly once, received ~S" text texts))))))
     :results (list (acp-session-test-text-result "delta"))))
  nil)

(-> test-acp-session-disconnected-registration-reload () null)
(defun test-acp-session-disconnected-registration-reload ()
  "Remove a disconnected session's stale registration so its channel can be loaded again."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session    (acp-service--session service identifier)))
         (conversation-append-user-message
          (application-conversation (acp-session-test--application session)) "saved")
         (localgroup-client-close (acp-session-connection session))
         (test-assert
          (task-tests--wait-until
           (lambda ()
             (with-lock-held ((acp-service-lock service))
               (not (nth-value 1 (gethash identifier (acp-service-sessions service))))))
           2)
          "unexpected EOF removes the stale session registration")
         (agentcomms:client-load-session client identifier (namestring root))
         (test-assert (not (eq session (acp-service--session service identifier)))
                      "loading the same channel admits a fresh editor session")))))
  nil)

(-> test-acp-session-reconnect-failure () null)
(defun test-acp-session-reconnect-failure ()
  "Forget a failed reconnect and permit an explicit load of the same channel."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session    (acp-service--session service identifier)))
         (conversation-append-user-message
          (application-conversation (acp-session-test--application session)) "saved")
         (test-call-with-function-replacements
          (list (list 'localgroup--wait-for-checkpoint-entry
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (error 'localgroup-error :message  "Injected reconnect refusal."
                                                :operation ':attach))))
          (lambda ()
            (acp-session-event session (list :checkpoint-reconnect :id (make-identifier)))
            (localgroup-client-close (acp-session-connection session))
            (test-assert
             (task-tests--wait-until (lambda () (acp-session-closed-p session)) 2)
             "a refused reconnect closes the session")
            (let ((worker (acp-session-reconnect-thread session)))
              (when worker (join-thread worker)))
            (test-assert (non-empty-string-p (acp-session-reconnect-failure session))
                         "reconnect failure retains a diagnostic")
            (test-assert
             (with-lock-held ((acp-service-lock service))
               (not (gethash identifier (acp-service-sessions service))))
             "failed reconnect removes the dead registration")))
         (agentcomms:client-load-session client identifier (namestring root))
         (test-assert (not (eq session (acp-service--session service identifier)))
                      "explicit load succeeds after reconnect failure")))))
  nil)

(-> test-acp-session-close-during-reconnect () null)
(defun test-acp-session-close-during-reconnect ()
  "Cancel endpoint discovery when an editor closes a reconnecting session."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session    (acp-service--session service identifier)))
         (acp-session-event session (list :checkpoint-reconnect :id (make-identifier)))
         (localgroup-client-close (acp-session-connection session))
         (test-assert (task-tests--wait-until
                       (lambda () (acp-session-reconnect-thread session)) 2)
                      "endpoint discovery starts after planned closure")
         (let ((worker (acp-session-reconnect-thread session)))
           (acp-session-close session)
           (test-assert (or (null worker) (not (thread-alive-p worker)))
                        "close joins the cancelled reconnect worker"))
         (test-assert (and (acp-session-closed-p session)
                           (null (acp-session-reconnect-id session)))
                      "closed session admits no replacement connection")
         (test-assert
          (with-lock-held ((acp-service-lock service))
            (not (gethash identifier (acp-service-sessions service))))
          "close removes the reconnecting registration")))))
  nil)

(-> test-localgroup-checkpoint-interrupted-request () null)
(defun test-localgroup-checkpoint-interrupted-request ()
  "Report a lost reply as interrupted without retransmitting its command."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier   (agentcomms:client-new-session client (namestring root)))
              (session      (acp-service--session service identifier))
              (application  (acp-session-test--application session))
              (connection   (acp-session-connection session))
              (original     (symbol-function 'localgroup--client-reply))
              (lock         (make-lock "Interrupted request test"))
              (condition    (make-condition-variable :name "Interrupted request test"))
              (entered-p    nil)
              (released-p   nil)
              (calls        0)
              (result       nil)
              (caller       nil)
              (reconnect-id (make-identifier)))
         (test-call-with-function-replacements
          (list (list 'localgroup--client-reply
                      (lambda (endpoint packet)
                        (when (eq (first packet) ':state)
                          (with-lock-held (lock)
                            (incf calls)
                            (setf entered-p t)
                            (condition-notify condition)
                            (loop until released-p do (condition-wait condition lock))))
                        (funcall original endpoint packet))))
          (lambda ()
            (unwind-protect
                 (progn
                   (setf caller
                         (make-thread
                          (lambda ()
                            (setf result
                                  (handler-case (localgroup-client-call connection ':state)
                                    (localgroup-request-interrupted (failure) failure))))
                          :name "Interrupted structured request"))
                   (test-assert (task-tests--wait-until (lambda () entered-p) 2)
                                "the request reaches the server before checkpoint closure")
                   (localgroup--finish-checkpoint-clients application reconnect-id)
                   (test-assert (task-tests--wait-until (lambda () result) 2)
                                "transport closure wakes the pending caller")
                   (test-assert (typep result 'localgroup-request-interrupted)
                                "the pending request has an explicit interrupted outcome")
                   (when (typep result 'localgroup-request-interrupted)
                     (test-assert (and (= 1 (localgroup-request-interrupted-identifier result))
                                       (eq ':state (localgroup-request-interrupted-command result))
                                       (equal reconnect-id
                                              (localgroup-request-interrupted-reconnect-id result)))
                                  "the interruption identifies the request and restart"))
                   (test-assert (= 1 calls) "the transmitted command is never retried"))
              (with-lock-held (lock)
                (setf released-p t)
                (condition-notify condition))
              (localgroup-client-close connection)
              (when caller (join-thread caller))
              (acp-session-close session))))))))
  nil)

(-> acp-reconnect-test--checkpoint (application function &key (:server world-server)) t)
(defun acp-reconnect-test--checkpoint (application function &key (server *acp-session-test-server*))
  "Run FUNCTION inside the fixture's real checkpoint quiescer."
  (world-server-call-quiesced
   server application
   (lambda () (funcall *checkpoint-thread-quiescer* function))))

(-> test-acp-session-consecutive-checkpoint-race () null)
(defun test-acp-session-consecutive-checkpoint-race ()
  "Retry subscription when a second checkpoint supersedes a discovered endpoint."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
              (session     (acp-service--session service identifier))
              (application (acp-session-test--application session))
              (previous    (acp-session-connection session))
              (original    (symbol-function 'localgroup-client-connect))
              (server      *acp-session-test-server*)
              (raced-p     nil))
         (test-call-with-function-replacements
          (list (list 'localgroup-client-connect
                      (lambda (entry &rest arguments)
                        (when (and (getf arguments :after-sequence) (not raced-p))
                          (setf raced-p t)
                          (acp-reconnect-test--checkpoint
                           application
                           (lambda ()
                             (application-clients-publish
                              application (list :present :text "second-checkpoint")))
                           :server server))
                        (apply original entry arguments))))
          (lambda ()
            (acp-reconnect-test--checkpoint application (lambda () nil))
            (test-assert
             (task-tests--wait-until
              (lambda ()
                (and raced-p (not (acp-session-closed-p session))
                     (not (eq previous (acp-session-connection session)))
                     (null (acp-session-reconnect-id session)))) 10)
             (format nil "checkpoint race reconnects (raced ~S closed ~S id ~S failure ~S worker ~S)"
                     raced-p (acp-session-closed-p session) (acp-session-reconnect-id session)
                     (acp-session-reconnect-failure session) (acp-session-reconnect-thread session)))
            (application-clients-publish application (list :present :text "after-race"))
            (test-assert
             (task-tests--wait-until
              (lambda () (member "after-race" (acp-reconnect-test--texts client) :test #'equal)) 2)
             "the replacement receives live output")
            (test-assert (= 1 (count "second-checkpoint" (acp-reconnect-test--texts client) :test #'equal))
                         "cursor replay includes the event retained during the second checkpoint once")))))))
  nil)

(-> test-acp-session-setup-disconnection () null)
(defun test-acp-session-setup-disconnection ()
  "Remove both registrations if EOF precedes either session registration."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (dolist (phase '(:attachment :protocol-registration))
         (let* ((name     (if (eq phase ':attachment)
                              'acp-session-finish-attachment 'agentcomms::agent--register-session))
                (original (symbol-function name)))
           (test-call-with-function-replacements
            (list (list name
                        (lambda (&rest arguments)
                          (when (eq phase ':protocol-registration)
                            (let ((session (acp-service--session service (second arguments))))
                              (localgroup-client-close (acp-session-connection session))
                              (test-assert (task-tests--wait-until
                                            (lambda () (acp-session-closed-p session)) 2)
                                           "EOF arrives before protocol registration")))
                          (multiple-value-prog1 (apply original arguments)
                            (when (eq phase ':attachment)
                              (let ((session (first arguments)))
                                (localgroup-client-close (acp-session-connection session))
                                (test-assert (task-tests--wait-until
                                              (lambda () (acp-session-closed-p session)) 2)
                                             "EOF arrives before application registration")))))))
            (lambda ()
              (test-assert
               (handler-case
                   (progn (agentcomms:client-new-session client (namestring root)) nil)
                 (agentcomms:acp-remote-error () t))
               "setup reports its lost channel")
              (test-assert (zerop (hash-table-count (acp-service-sessions service)))
                           "no disconnected application session is registered")
              (test-assert (null (agentcomms:acp-agent-session-ids service))
                           "no disconnected protocol session is registered"))))))))
  nil)

(-> test-acp-request-interrupted-wire-data () null)
(defun test-acp-request-interrupted-wire-data ()
  "Preserve unknown execution and request identity through the ACP wire error."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (declare (ignore service))
       (let ((identifier (agentcomms:client-new-session client (namestring root)))
             (original   (symbol-function 'localgroup-client-call)))
         (test-call-with-function-replacements
          (list (list 'localgroup-client-call
                      (lambda (connection command &rest arguments)
                        (when (eq command ':submit)
                          (error 'localgroup-request-interrupted
                                 :message "Injected lost reply." :operation ':client
                                 :identifier 7 :command command :reconnect-id "checkpoint-test"))
                        (apply original connection command arguments))))
          (lambda ()
            (let* ((failure (handler-case
                                (progn
                                  (agentcomms:client-prompt
                                   client identifier (list (agentcomms:acp-text-content "submit")))
                                  nil)
                              (agentcomms:acp-remote-error (condition) condition)))
                   (data    (and failure (agentcomms:acp-method-error-data failure))))
              (test-assert failure "the lost reply is an ACP request error")
              (test-assert
               (and data
                    (equal "interrupted" (agentcomms:json-get data "outcome"))
                    (equal "unknown" (agentcomms:json-get data "executionOutcome"))
                    (eql 7 (agentcomms:json-get data "requestId"))
                    (equal "submit" (agentcomms:json-get data "command"))
                    (equal "checkpoint-test" (agentcomms:json-get data "reconnectId")))
               "wire error data retains the transmitted request's uncertain execution"))))))))
  nil)

(-> test-channel-client-checkpoint-stop-failure () null)
(defun test-channel-client-checkpoint-stop-failure ()
  "Seal every client before draining and propagate an unreaped transport failure."
  (client-protocol-tests--call
   (lambda (application entry)
     (let* ((connections (loop repeat 2 collect (localgroup-client-connect entry :approves-p nil)))
            (clients     (application-clients application))
            (injected    (make-condition 'image-daemon:daemon-error
                                         :message "Injected unreaped writer." :operation ':stop-thread))
            (sealed-p    nil))
       (unwind-protect
            (test-call-with-function-replacements
             (list (list 'image-daemon:attachment-finish
                         (lambda (attachment event)
                           (declare (ignore attachment event))
                           (setf sealed-p
                                 (and (= 2 (length clients))
                                      (every (lambda (client)
                                               (eq (image-daemon:attachment-try-send
                                                    (channel-client-attachment client) '(:late))
                                                   ':closed))
                                             clients)))
                           (error injected))))
             (lambda ()
               (test-assert
                (eq injected
                    (handler-case
                        (localgroup--finish-checkpoint-clients application "test")
                      (image-daemon:daemon-error (condition)
                        condition)))
                "checkpoint preserves the transport's structured reaping failure")
               (test-assert sealed-p "every captured client refuses output before any drain begins")))
         (dolist (connection connections)
           (localgroup-client-close connection))))))
  nil)

(-> test-acp-session-close-during-greeting () null)
(defun test-acp-session-close-during-greeting ()
  "Cancel a replacement connection whose accepted socket has no greeting yet."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
              (session     (acp-service--session service identifier))
              (application (acp-session-test--application session))
              (original    (symbol-function 'application-clients-add))
              (lock        (make-lock "Held greeting test"))
              (condition   (make-condition-variable :name "Held greeting test"))
              (entered-p   nil)
              (released-p  nil))
         (test-call-with-function-replacements
          (list (list 'application-clients-add
                      (lambda (&rest arguments)
                        (with-lock-held (lock)
                          (setf entered-p t)
                          (condition-notify condition)
                          (loop until released-p do (condition-wait condition lock)))
                        (apply original arguments))))
          (lambda ()
            (unwind-protect
                 (progn
                   (acp-reconnect-test--checkpoint application (lambda () nil))
                   (test-assert (task-tests--wait-until (lambda () entered-p) 2)
                                "the replacement reaches the greeting barrier")
                   (let ((worker (acp-session-reconnect-thread session)))
                     ;; Closing the channel is independent of handshake cancellation.
                     (test-call-with-function-replacements
                      (list (list 'world-client-entry
                                  (lambda (&rest arguments) (declare (ignore arguments)) nil)))
                      (lambda () (acp-session-close session)))
                     (test-assert (and worker (not (thread-alive-p worker)))
                                  "close joins the worker before releasing its greeting")
                     (test-assert (acp-session-closed-p session)
                                  "the cancelled attachment cannot install a connection")))
              (with-lock-held (lock)
                (setf released-p t)
                (condition-notify condition)))))))))
  nil)

(-> test-acp-session-close-preserves-replacement () null)
(defun test-acp-session-close-preserves-replacement ()
  "Do not remove a replacement registration while an older close returns."
  (with-test-configuration (configuration)
    (let* ((service     (make-instance 'acp-service :configuration configuration))
           (identifier  (make-identifier))
           (previous    (make-instance 'acp-session :service service :identifier identifier
                                                    :configuration configuration))
           (replacement (make-instance 'acp-session :service service :identifier identifier
                                                    :configuration configuration))
           (original    (symbol-function 'acp-session-close)))
      (setf (gethash identifier (acp-service-sessions service)) previous)
      (test-call-with-function-replacements
       (list (list 'acp-session-close
                   (lambda (session)
                     (setf (gethash identifier (acp-service-sessions service)) replacement)
                     (funcall original session))))
       (lambda () (agentcomms:agent-close-session service identifier nil)))
      (test-assert (eq replacement (gethash identifier (acp-service-sessions service)))
                   "a returning older close cannot erase the new registration")
      (funcall original replacement)))
  nil)
