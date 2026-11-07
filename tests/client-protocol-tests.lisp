(in-package #:autolith)

;;;; -- Structured Client Protocol Tests --

(-> client-protocol-tests--event (list keyword &key (:test function)) (option list))
(defun client-protocol-tests--event (events kind &key (test (constantly t)))
  "Return the first of EVENTS of KIND that TEST accepts."
  (find-if (lambda (event)
             (and (eq (first event) kind) (funcall test event)))
           events))

(-> test-client-protocol-channel () null)
(defun test-client-protocol-channel ()
  "Test a structured client follows a hosted channel: state, commands, records, modes, approvals and leaving."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (channel     (getf (rest (world-server-tests--open entry (world-tests--workspace root "a")))
                               :session-id))
            (application (world-host-application (world-server--find-host server channel)))
            (lock        (make-lock "client protocol test"))
            (events      nil)
            (connection  nil))
       (labels ((seen ()
                  "Return the events seen so far, oldest first."
                  (with-lock-held (lock)
                    (reverse events)))

                (await (kind &optional (test (constantly t)))
                  "Wait for an event of KIND that TEST accepts and return it."
                  (let ((found nil))
                    (task-tests--wait-until
                     (lambda ()
                       (setf found (client-protocol-tests--event (seen) kind :test test)))
                     10)
                    found)))
         (setf connection
               (localgroup-client-connect (localgroup--find-record configuration channel)
                                          :name           "test"
                                          :event-function (lambda (event)
                                                            (with-lock-held (lock)
                                                              (push event events)))))
         (unwind-protect
              (let ((greeting (localgroup-client-connection-attached connection)))
                (test-assert (and (non-empty-string-p (getf (rest greeting) :client-id))
                                  (equal (getf (rest greeting) :session-id) channel)
                                  (eq (first (getf (rest greeting) :state)) ':localgroup-status))
                             "the greeting names the client, its channel and the channel's state")
                (test-assert (eq (first (getf (localgroup-client-call connection ':state) :state))
                                 ':localgroup-status)
                             "a state command answers with the channel's state")
                (localgroup-client-call connection ':submit :text "/status")
                (test-assert (and (await ':present)
                                  (await ':work-finished
                                         (lambda (event)
                                           (and (eq (getf (rest event) :kind) ':command)
                                                (eq (getf (rest event) :outcome) ':done)
                                                (getf (rest event) :idle-p)))))
                             "a submitted command's output arrives, then the end of its work with the channel idle")
                (localgroup-client-call connection ':mode :value ':ask)
                (test-assert (and (eq (application-permission-mode application) ':ask)
                                  (await ':state))
                             "a mode command sets the channel's permission mode and publishes its state")
                (test-assert (handler-case (progn (localgroup-client-call connection ':mode :value ':root) nil)
                               (localgroup-error (condition)
                                 (search "permission mode" (autolith-error-message condition))))
                             "an unknown mode is refused with the modes there are")
                (conversation-append-user-message (application-conversation application) "words for the client")
                (test-assert (await ':record
                                    (lambda (event)
                                      (search "words for the client"
                                              (prin1-to-string (getf (rest event) :record)))))
                             "a record the channel appends reaches the client")
                (let* ((choice nil)
                       (asker  (make-thread
                                (lambda ()
                                  (setf choice
                                        (application-select-approval
                                         application
                                         :kind  ':command
                                         :title "run true"
                                         :entry "Run true"
                                         :items (list (list :name "once" :description "allow once")
                                                      (list :name "deny" :description "do not run")))))
                                :name "client protocol approval")))
                  (let ((approval (await ':approval)))
                    (test-assert (and approval
                                      (equal (mapcar (lambda (option) (getf option :value))
                                                     (getf (rest approval) :options))
                                             '("once" "deny")))
                                 "an approval reaches the client with its choices")
                    (test-assert (null (getf (localgroup-client-call connection ':answer
                                                                     :approval (getf (rest approval) :id)
                                                                     :value    "always")
                                             :accepted-p))
                                 "a choice the approval does not offer is not taken")
                    (test-assert (getf (localgroup-client-call connection ':answer
                                                               :approval (getf (rest approval) :id)
                                                               :value    "once")
                                       :accepted-p)
                                 "the client's answer is taken")
                    (join-thread asker)
                    (test-assert (and (equal choice "once")
                                      (await ':approval-settled
                                             (lambda (event)
                                               (equal (getf (rest event) :value) "once"))))
                                 "the approval returns the client's choice and everyone hears it settled"))))
           (localgroup-client-close connection))
         (test-assert (task-tests--wait-until
                       (lambda () (not (application-clients-attached-p application)))
                       10)
                      "a client that disconnects leaves the channel")))))
  nil)

(-> client-protocol-tests--call (function) null)
(defun client-protocol-tests--call (function)
  "Call FUNCTION with a hosted application and its endpoint record."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((channel (getf (rest (world-server-tests--open
                                  (world-client-entry configuration)
                                  (world-tests--workspace root "clients")))
                          :session-id))
            (host    (world-server--find-host server channel)))
       (funcall function (world-host-application host)
                (localgroup--find-record configuration channel)))))
  nil)

(-> test-client-protocol-disconnect () null)
(defun test-client-protocol-disconnect ()
  "Test writer failure and overflow remove idle approvers and wake their observers."
  (dolist (failure '(:overflow :write))
    (client-protocol-tests--call
     (lambda (application entry)
       (let* ((approval   (sb-thread:make-semaphore))
              (settled    (sb-thread:make-semaphore))
              (waiting    (sb-thread:make-semaphore))
              (closed     (sb-thread:make-semaphore))
              (writing    (sb-thread:make-semaphore))
              (release    (sb-thread:make-semaphore))
              (connection (localgroup-client-connect
                           entry :name "failing"
                           :event-function
                           (lambda (event)
                             (case (first event)
                               (:approval
                                (sb-thread:signal-semaphore approval))
                               (:closed
                                (sb-thread:signal-semaphore closed))))))
              (client     (first (application-clients application)))
              (healthy    (localgroup-client-connect entry :name "healthy" :approves-p nil))
              (write      #'daemon-write-packet)
              (wait       #'condition-wait)
              (hub        (application-client-hub application))
              (choice     :unanswered)
              (asker      nil))
         (unwind-protect
              (test-call-with-function-replacements
               (list
                (list 'condition-wait
                      (lambda (condition-variable lock &key timeout)
                        (when (eq condition-variable (client-hub-condition-variable hub))
                          (sb-thread:signal-semaphore waiting))
                        (funcall wait condition-variable lock :timeout timeout)))
                (list 'daemon-write-packet
                      (lambda (stream event)
                        (if (and (eq stream (channel-client-stream client))
                                 (eq (first event) ':test-failure))
                            (progn
                              (sb-thread:signal-semaphore writing)
                              (when (eq failure ':overflow)
                                (sb-thread:wait-on-semaphore release :timeout 10))
                              (error 'end-of-file :stream stream))
                            (funcall write stream event)))))
               (lambda ()
                 (setf asker
                       (make-thread
                        (lambda ()
                          (unwind-protect
                               (let ((*channel-approval-poll-seconds* 60))
                                 (setf choice
                                       (application-select-approval
                                        application :kind ':command :title "test" :entry "test"
                                        :items (list (list :name "once" :description "allow")))))
                            (sb-thread:signal-semaphore settled)))
                        :name "client approval waiter"))
                 (test-assert (sb-thread:wait-on-semaphore approval :timeout 10)
                              "the approver receives the pending approval before it goes idle")
                 (test-assert (sb-thread:wait-on-semaphore waiting :timeout 10)
                              "the approval waiter releases the hub lock into its wait")
                 (channel-client-enqueue client '(:test-failure))
                 (test-assert (sb-thread:wait-on-semaphore writing :timeout 10)
                              "the writer reaches the injected transport boundary")
                 (when (eq failure ':overflow)
                   (let ((*channel-client-queue-limit* 1))
                     (channel-client-enqueue client '(:queued))
                     (channel-client-enqueue client '(:overflow))))
                 (test-assert (sb-thread:wait-on-semaphore closed :timeout 10)
                              "server shutdown wakes an idle peer without another request")
                 (test-assert (sb-thread:wait-on-semaphore settled :timeout 10)
                              "losing the last approver wakes the pending approval")
                 (test-assert (and (null choice)
                                   (not (application-clients-approving-p application))
                                   (not (member client (application-clients application))))
                              "the failed approver is unregistered before approval returns")
                 ;; Repeated closes must not remove a different client or touch its socket.
                 (channel-client-close client)
                 (test-assert (getf (localgroup-client-call healthy ':state) :state)
                              "a healthy observer still serves requests during teardown")
                 (sb-thread:signal-semaphore release)))
           (sb-thread:signal-semaphore release)
           (localgroup-client-close connection)
           (localgroup-client-close healthy)
           (when asker
             (join-thread asker)))))))
  nil)
