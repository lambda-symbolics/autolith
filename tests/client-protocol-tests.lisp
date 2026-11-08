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

(-> test-client-protocol-start-failure () null)
(defun test-client-protocol-start-failure ()
  "Report a writer-start refusal before unregistering and shutting down its transport."
  (client-protocol-tests--call
   (lambda (application entry)
     (let ((start    #'image-daemon:attachment-start)
           (injected (make-condition 'localgroup-error :operation ':client
                                      :message                    "Injected writer-start refusal.")))
       (test-call-with-function-replacements
        (list (list 'image-daemon:attachment-start
                    (lambda (attachment)
                      (if (typep attachment 'channel-client-transport)
                          (error injected)
                          (funcall start attachment)))))
        (lambda ()
          (test-assert
           (handler-case
               (let ((connection (localgroup-client-connect entry)))
                 (localgroup-client-close connection)
                 nil)
             (localgroup-error (condition)
               (equal (autolith-error-message condition)
                      (autolith-error-message injected))))
           "a registered client's startup refusal reaches the peer before shutdown")
          (test-assert
           (task-tests--wait-until (lambda () (not (application-clients-attached-p application))) 2)
           "startup failure removes the registered client"))))))
  nil)

(defclass client-protocol-test-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((delegate :initarg :delegate :reader client-protocol-test-stream-delegate
             :documentation "The daemon-owned output stream.")
   (entered :initarg :entered :reader client-protocol-test-stream-entered
            :documentation "Signaled when an armed writer reaches output.")
   (release :initarg :release :reader client-protocol-test-stream-release
            :documentation "Optional gate holding the failing write.")
   (armed-p :initform nil :accessor client-protocol-test-stream-armed-p
            :documentation "Whether the next write should fail."))
  (:documentation "A borrowed output boundary for structured-client failure tests."))

(defmethod trivial-gray-streams:stream-write-string
    ((stream client-protocol-test-stream) string &optional (start 0) end)
  "Delegate output unless the test has armed a gated writer failure."
  (when (client-protocol-test-stream-armed-p stream)
    (sb-thread:signal-semaphore (client-protocol-test-stream-entered stream))
    (when (client-protocol-test-stream-release stream)
      (sb-thread:wait-on-semaphore (client-protocol-test-stream-release stream)))
    (error 'stream-error :stream stream))
  (write-string string (client-protocol-test-stream-delegate stream) :start start :end end)
  string)

(defmethod trivial-gray-streams:stream-write-char ((stream client-protocol-test-stream) character)
  "Write CHARACTER through the same failure boundary as a string."
  (trivial-gray-streams:stream-write-string stream (string character))
  character)

(defmethod trivial-gray-streams:stream-finish-output ((stream client-protocol-test-stream))
  "Flush the borrowed output without closing it."
  (finish-output (client-protocol-test-stream-delegate stream)))

(-> test-client-protocol-disconnect () null)
(defun test-client-protocol-disconnect ()
  "Test writer failure and overflow remove idle approvers and wake their observers."
  (dolist (failure '(:overflow :write))
    (let ((output  nil)
          (create  #'channel-client-create)
          (writing (sb-thread:make-semaphore))
          (release (sb-thread:make-semaphore)))
      (test-call-with-function-replacements
       (list (list 'channel-client-create
                   (lambda (&rest arguments)
                     (when (equal (getf arguments :name) "failing")
                       (setf output (make-instance 'client-protocol-test-stream
                                                   :delegate (getf arguments :stream)
                                                   :entered  writing
                                                   :release  (when (eq failure ':overflow) release))
                             (getf arguments :stream) output))
                     (apply create arguments))))
       (lambda ()
         (client-protocol-tests--call
          (lambda (application entry)
            (let* ((approval   (sb-thread:make-semaphore))
                   (settled    (sb-thread:make-semaphore))
                   (waiting    (sb-thread:make-semaphore))
                   (closed     (sb-thread:make-semaphore))
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
                   (wait       #'condition-wait)
                   (hub        (application-client-hub application))
                   (choice     :unanswered)
                   (asker      nil))
              (unwind-protect
                   (test-call-with-function-replacements
                    (list (list 'condition-wait
                                (lambda (condition-variable lock &key timeout)
                                  (when (eq condition-variable (client-hub-condition-variable hub))
                                    (sb-thread:signal-semaphore waiting))
                                  (funcall wait condition-variable lock :timeout timeout))))
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
                      (setf (client-protocol-test-stream-armed-p output) t)
                      (channel-client-enqueue client '(:test-failure))
                      (test-assert (sb-thread:wait-on-semaphore writing :timeout 10)
                                   "the writer reaches the injected transport boundary")
                      (when (eq failure ':overflow)
                        (let ((image-daemon:*attachment-queue-character-limit*
                                (length (image-daemon:daemon-packet-string '(:queued)))))
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
                      (test-assert (open-stream-p output) "the attachment does not close the borrowed stream")
                      (channel-client-close client)
                      (test-assert (getf (localgroup-client-call healthy ':state) :state)
                                   "a healthy observer still serves requests during teardown")))
                (sb-thread:signal-semaphore release)
                (localgroup-client-close connection)
                (localgroup-client-close healthy)
                (when asker
                  (join-thread asker))))))))))
  nil)

(-> test-client-protocol-packet-limit () null)
(defun test-client-protocol-packet-limit ()
  "Test oversized replies fail their own call and unsolicited events report omission."
  (client-protocol-tests--call
   (lambda (application entry)
     (let* ((omitted    (sb-thread:make-semaphore))
            (completed  (sb-thread:make-semaphore))
            (notice     nil)
            (failure    nil)
            (caller     nil)
            (connection (localgroup-client-connect
                         entry :approves-p nil
                         :event-function
                         (lambda (event)
                           (when (eq (first event) ':omitted)
                             (setf notice event)
                             (sb-thread:signal-semaphore omitted)))))
            (command    #'localgroup--client-command)
            (frame      #'image-daemon:daemon-packet-string))
       (unwind-protect
            (test-call-with-function-replacements
             (list
              (list 'image-daemon:daemon-packet-string
                    (lambda (packet)
                      (let ((image-daemon:*daemon-packet-character-limit* 8192))
                        (funcall frame packet))))
              (list 'localgroup--client-command
                    (lambda (session operation arguments)
                      (if (eq operation ':oversized)
                          (list :data (make-string 32768 :initial-element #\x))
                          (funcall command session operation arguments)))))
             (lambda ()
               (setf caller
                     (make-thread
                      (lambda ()
                        (unwind-protect
                             (handler-case (localgroup-client-call connection ':oversized)
                               (localgroup-error (condition)
                                 (setf failure condition)))
                          (sb-thread:signal-semaphore completed)))
                      :name "oversized reply caller"))
               (test-assert (sb-thread:wait-on-semaphore completed :timeout 10)
                            "an oversized reply settles its correlated request")
               (test-assert (and failure (< (length (autolith-error-message failure)) 1000))
                            "the caller receives a bounded localgroup error")
               (test-assert
                (handler-case
                    (progn
                      (localgroup-client-call connection ':oversized :data (make-string 32768))
                      nil)
                  (image-daemon:daemon-error ()
                    t))
                "an oversized request fails before writing to the transport")
               (test-assert (getf (localgroup-client-call connection ':state) :state)
                            "the next request succeeds on the same connection")
               (application-clients-publish
                application (list :text :delta (make-string 32768 :initial-element #\x)))
               (test-assert (sb-thread:wait-on-semaphore omitted :timeout 10)
                            "an oversized unsolicited event reports omission")
               (test-assert (eq (getf (rest notice) :kind) ':text)
                            "the omission names the event kind")
               (test-assert (zerop (hash-table-count (localgroup-client-connection-replies connection)))
                            "settled calls leave no retained replies")))
         (localgroup-client-close connection)
         (when caller
           (join-thread caller))))))
  nil)

(-> test-client-protocol-write-failure () null)
(defun test-client-protocol-write-failure ()
  "Test a failed command write wakes another pending call and releases reply entries."
  (client-protocol-tests--call
   (lambda (application entry)
     (declare (ignore application))
     (let* ((received   (sb-thread:make-semaphore))
            (release    (sb-thread:make-semaphore))
            (completed  (sb-thread:make-semaphore))
            (connection (localgroup-client-connect entry :approves-p nil))
            (broken     (make-string-output-stream))
            (command    #'localgroup--client-command)
            (stream     #'localgroup-client-connection-stream)
            (writer     (current-thread))
            (failure    nil)
            (caller     nil))
       (close broken)
       (unwind-protect
            (test-call-with-function-replacements
             (list
              (list 'localgroup--client-command
                    (lambda (session operation arguments)
                      (if (and (eq operation ':operation)
                               (equal (getf arguments :method) "blocked"))
                          (progn
                            (sb-thread:signal-semaphore received)
                            (sb-thread:wait-on-semaphore release :timeout 10)
                            nil)
                          (funcall command session operation arguments))))
              (list 'localgroup-client-connection-stream
                    (lambda (candidate)
                      (if (and (eq candidate connection) (eq (current-thread) writer))
                          broken
                          (funcall stream candidate)))))
             (lambda ()
               (setf caller
                     (make-thread
                      (lambda ()
                        (unwind-protect
                             (handler-case
                                 (localgroup-client-call connection ':operation :method "blocked")
                               (localgroup-error (condition)
                                 (setf failure condition)))
                          (sb-thread:signal-semaphore completed)))
                      :name "pending client call"))
               (test-assert (sb-thread:wait-on-semaphore received :timeout 10)
                            "one call remains pending while another reaches a failed write")
               (test-assert (handler-case (progn (localgroup-client-call connection ':state) nil)
                              (localgroup-request-interrupted (condition)
                                (and (eq (localgroup-request-interrupted-command condition) ':state)
                                     (plusp (localgroup-request-interrupted-identifier condition)))))
                            "a failed write reports the interrupted request with an unknown execution outcome")
               (test-assert (sb-thread:wait-on-semaphore completed :timeout 10)
                            "transport failure wakes every pending call")
               (test-assert (and (typep failure 'localgroup-request-interrupted)
                                 (eq (localgroup-request-interrupted-command failure) ':operation)
                                 (localgroup-client-connection-closed-p connection)
                                 (zerop (hash-table-count (localgroup-client-connection-replies connection))))
                            "failed calls report interruption and release reply entries on the closed connection")))
         (sb-thread:signal-semaphore release)
         (localgroup-client-close connection)
         (when caller
           (join-thread caller))))))
  nil)

(-> test-client-protocol-cursor-tail () null)
(defun test-client-protocol-cursor-tail ()
  "Replay a retained event tail before live events and refuse unavailable cursors."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((world-entry (world-client-entry configuration))
            (channel     (getf (rest (world-server-tests--open world-entry (world-tests--workspace root "cursor")))
                               :session-id))
            (application (world-host-application (world-server--find-host server channel)))
            (hub         (application-client-hub application))
            (entry       (localgroup--find-record configuration channel))
            (events      nil)
            (connection  nil))
       (application-clients-publish application '(:present :text "retained"))
       (let ((cursor (with-lock-held ((client-hub-lock hub)) (client-hub-event-sequence hub))))
         (unwind-protect
              (progn
                (setf connection
                      (localgroup-client-connect entry :after-sequence (1- cursor)
                                                 :event-function       (lambda (event) (push event events))))
                (localgroup-client-call connection ':state)
                (test-assert (equal entry (localgroup-client-connection-entry connection))
                             "the authenticated endpoint survives registry changes")
                (test-assert (and (= cursor (getf (rest (localgroup-client-connection-attached connection)) :sequence))
                                  (integerp (getf (rest (localgroup-client-connection-attached connection)) :record-sequence)))
                             "the greeting identifies both snapshot watermarks")
                (application-clients-publish application '(:present :text "live"))
                (localgroup-client-call connection ':state)
                (test-assert (equal '("retained" "live")
                                    (loop for event in (reverse events)
                                          when (eq (first event) ':present) collect (getf (rest event) :text)))
                             "retained events arrive before live events without a gap or duplicate")
                (test-assert (equal (list cursor (1+ cursor))
                                    (loop for event in (reverse events)
                                          when (eq (first event) ':present) collect (getf (rest event) :sequence)))
                             "retained and live delivery share one increasing cursor")
                (localgroup-client-close connection)
                (setf connection nil)
                (let ((*channel-client-journal-limit* 2))
                  (dotimes (index 3)
                    (application-clients-publish application (list :present :text (princ-to-string index))))
                  (test-assert (handler-case
                                   (progn (setf connection (localgroup-client-connect entry :after-sequence cursor)) nil)
                                 (localgroup-error (condition)
                                   (search "cursor" (autolith-error-message condition))))
                               "an expired cursor fails explicitly instead of skipping missing events")
                  (test-assert (handler-case
                                   (progn (setf connection (localgroup-client-connect entry :after-sequence 1000000)) nil)
                                 (localgroup-error (condition)
                                   (search "cursor" (autolith-error-message condition))))
                               "a cursor ahead of the channel is refused")
                  (test-assert (= 2 (with-lock-held ((client-hub-lock hub))
                                     (deque-count (client-hub-event-journal hub))))
                               "journal retention is bounded"))
                (dolist (cursor '(-1 "invalid" 1.5))
                  (test-assert
                   (handler-case
                       (progn
                         (application-clients-add application (make-instance 'channel-client)
                                                  :after-sequence cursor)
                         nil)
                     (localgroup-error () t))
                   "malformed event cursors are refused before subscription"))
                (with-lock-held ((client-hub-lock hub)) (setf (client-hub-admitting-p hub) nil))
                (test-assert (handler-case
                                 (progn (setf connection (localgroup-client-connect entry)) nil)
                               (localgroup-error (condition) (localgroup-error-retry-p condition)))
                             "planned restart admission refusal is retryable"))
           (with-lock-held ((client-hub-lock hub)) (setf (client-hub-admitting-p hub) t))
           (when connection (localgroup-client-close connection)))))))
  nil)

(-> test-client-approval-terminal-arbitration () null)
(defun test-client-approval-terminal-arbitration ()
  "Test terminal selections and editor answers share one first-answer-wins decision."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (channel     (getf (rest (world-server-tests--open entry (world-tests--workspace root "race")))
                               :session-id))
            (application (world-host-application (world-server--find-host server channel)))
            (hub         (application-client-hub application))
            (settled     nil))
       (dolist (editor-choice '("deny" "once"))
         (let ((terminal-choice (if (string= editor-choice "deny") "once" "deny")))
           (test-call-with-function-replacements
            (list
             (list 'terminal-interactive-p (lambda (candidate)
                                            (declare (ignore candidate))
                                            t))
             (list 'application-clients-publish
                   (lambda (candidate event)
                     (declare (ignore candidate))
                     (when (eq (first event) ':approval-settled)
                       (setf settled (getf (rest event) :value)))))
             (list 'terminal-ui-select
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     ;; The editor answer lands after the picker last polled,
                     ;; but before the terminal's independent return.
                     (test-assert
                      (application-answer-approval
                       application (channel-approval-identifier (client-hub-approval hub)) editor-choice)
                      "the editor's first answer is accepted")
                     terminal-choice)))
            (lambda ()
              (test-assert
               (equal editor-choice
                      (application-select-approval
                       application :kind ':command :title "race" :entry "race"
                       :items '((:name "once") (:name "deny"))))
               "the accepted editor answer wins over the terminal return")
              (test-assert (equal settled editor-choice)
                           "settlement reports the same winning answer"))))))))
  nil)

(-> test-client-work-movement-cancellation () null)
(defun test-client-work-movement-cancellation ()
  "Test moving and dismissing an agent settle queued editor operations without stopping the channel."
  (dolist (operation '(:move :dismiss))
    (world-server-tests--call
     (lambda (server configuration root)
       (let* ((entry       (world-client-entry configuration))
              (channel     (getf (rest (world-server-tests--open entry (world-tests--workspace root "source")))
                                 :session-id))
              (application (world-host-application (world-server--find-host server channel)))
              (controller  (application-input-controller application))
              (completed   (sb-thread:make-semaphore))
              (failure     nil)
              (events      nil)
              (submission  '(:id "removed-prompt" :client "editor" :source "prompt"))
              (ran-p       nil)
              (waiter      nil))
         (conversation-append-user-message (application-conversation application) "source history")
         (let* ((world       (values (world-reconcile-sessions configuration)))
                (participant (find ':agent (world-channel-residents world channel) :key #'participant-kind))
                (destination nil))
           (when (eq operation ':move)
             (setf destination
                   (getf (rest (world-server-tests--open entry (world-tests--workspace root "destination")))
                         :session-id))
             (let* ((target (world-host-application (world-server--find-host server destination)))
                    (other  (progn
                              (conversation-append-user-message (application-conversation target) "target history")
                              (find ':agent (world-channel-residents
                                             (values (world-reconcile-sessions configuration)) destination)
                                    :key #'participant-kind))))
               (localgroup-query-record entry ':dismiss (list :participant (participant-name other)))))
           (application-localgroup-pause application)
           (unwind-protect
                (progn
                  (setf waiter
                        (make-thread
                         (lambda ()
                           (unwind-protect
                                (handler-case
                                    (application-clients-call-as-work application (lambda () (setf ran-p t)))
                                  (localgroup-error (condition)
                                    (setf failure condition)))
                             (sb-thread:signal-semaphore completed)))
                         :name "queued editor operation"))
                  (test-assert
                   (task-tests--wait-until
                    (lambda ()
                      (with-lock-held ((application-input-controller-lock controller))
                        (= 1 (deque-count (application-input-controller-work-items controller)))))
                    10)
                   "the editor operation is queued behind the paused agent")
                  (with-lock-held ((application-input-controller-lock controller))
                   (deque-push-back
                    (application-input-controller-work-items controller)
                    (list ':message (user-message-input-create :text "queued prompt" :submission submission)))
                   (dolist (kind '(:command :lisp))
                     (deque-push-back
                      (application-input-controller-work-items controller)
                      (list kind "queued non-prose" :submission
                            (list :id (symbol-name kind) :client "editor" :source "prompt")))))
                 (test-call-with-function-replacements
                  (list (list 'application-clients-publish
                              (lambda (candidate event)
                                (when (eq candidate application) (push event events)))))
                  (lambda ()
                    (localgroup-query-record
                     entry operation
                     (append (list :participant (participant-name participant))
                             (when destination (list :channel destination))))))
                 (test-assert (sb-thread:wait-on-semaphore completed :timeout 3)
                              "movement wakes the queued editor's waiter")
                 (test-assert (and (typep failure 'client-work-cancelled) (not ran-p)
                                   (eq (client-work-cancelled-reason failure)
                                       (if destination ':moved ':dismissed)))
                              "the removed operation reports useful cancellation and never runs")
                 (test-assert
                  (= 1 (count-if (lambda (event)
                                   (and (eq (first event) ':work-finished)
                                        (eq (getf (rest event) :outcome) ':cancelled)
                                        (equal (getf (rest event) :submission) submission)))
                                 events))
                  "the removed prompt receives one cancellation with its exact submission identity")
                 (dolist (kind '(:command :lisp))
                   (test-assert
                    (= 1 (count-if (lambda (event)
                                     (and (eq (first event) ':work-finished)
                                          (eq (getf (rest event) :kind) kind)
                                          (eq (getf (rest event) :outcome) ':cancelled)
                                          (equal (getf (getf (rest event) :submission) :id)
                                                 (symbol-name kind))))
                                   events))
                    "removed non-prose submissions receive their exact cancelled outcomes"))
                 (test-assert (and (not (application-input-controller-stopping-p controller))
                                   (eq ':alive (application-clients-call-as-work application (lambda () ':alive))))
                              "the channel accepts fresh administrative work after movement"))
             ;; A failing baseline waiter must not outlive the fixture.
             (when (and waiter (thread-alive-p waiter))
               (application-input-controller-stop controller))
             (when waiter
               (join-thread waiter))))))))
  nil)
