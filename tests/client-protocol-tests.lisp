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
