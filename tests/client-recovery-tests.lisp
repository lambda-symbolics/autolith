(in-package #:autolith)

(-> client-recovery-tests--call-with-session (function) null)
(defun client-recovery-tests--call-with-session (function)
  "Call FUNCTION with a configuration, a lingering application without a terminal of its own, and its session."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (terminal-ui-start (application-ui application))
             (funcall function configuration application
                      (localgroup-start application :linger-p t)))
        (localgroup-stop application)
        (application-input-controller-stop controller)
        (terminal-ui-stop (application-ui application))
        (application-release-conversation-lease application))))
  nil)

(-> test-localgroup-client-checkpoint-recovery () null)
(defun test-localgroup-client-checkpoint-recovery ()
  "End a view as a lost attachment when its checkpoint reconnect fails, and signal an initial refusal as itself."
  (dolist (phase '(:wait :reattach))
    (client-recovery-tests--call-with-session
     (lambda (configuration application session)
       (declare (ignore application))
       (let ((entry (cons (image-daemon:daemon-runtime-registry-pathname session)
                          (localgroup--registry-record session)))
             (cause (make-condition (if (eq phase ':wait) 'localgroup-error 'localgroup-client-rejected)
                                    :operation ':attach
                                    :message   "Injected checkpoint failure."))
             (waits nil)
             (calls nil))
         (world-server-tests--call-with-terminal-client
          configuration (image-daemon:daemon-runtime-identifier session)
          (lambda (type await-exit view)
            (declare (ignore type))
            ;; The reconnect runs on a thread of its own, so what it asks is checked afterwards.
            (test-call-with-function-replacements
             (list
              (list 'localgroup--wait-for-checkpoint-entry
                    (lambda (candidate transition &key &allow-other-keys)
                      (push (list (first candidate) transition) waits)
                      (when (eq phase ':wait)
                        (error cause))
                      entry))
              (list 'localgroup-client-connect
                    (lambda (candidate &key after-sequence &allow-other-keys)
                      (push (list (first candidate) (integerp after-sequence)) calls)
                      (error cause))))
             (lambda ()
               (channel-subscription-event view (list :checkpoint-reconnect :id "transition"))
               (localgroup-client-close (channel-subscription-connection view))
               (multiple-value-bind (exit ended-p) (funcall await-exit)
                 (test-assert
                  (and ended-p
                       (eq (first exit) ':attachment-lost)
                       (eq (getf (rest exit) :kind) ':transport-failure)
                       (search "Injected checkpoint failure." (getf (rest exit) :message)))
                  (format nil "a checkpoint reconnect that fails in its ~(~A~) ends the view as a lost attachment naming the failure"
                          phase)))
               (test-assert (equal waits (list (list (first entry) "transition")))
                            "only the announced checkpoint transition is rediscovered, once")
               (test-assert (equal calls (unless (eq phase ':wait)
                                           (list (list (first entry) t))))
                            "a replacement attachment keeps its endpoint and resumes from its cursor")))))))))
  (client-recovery-tests--call-with-session
   (lambda (configuration application session)
     (declare (ignore application))
     (let* ((screen (localgroup-screen-create
                     :terminal (make-instance 'queued-recording-terminal :rows 31 :columns 91)))
            (view   (channel-view-create
                     configuration (image-daemon:daemon-runtime-identifier session) screen))
            (cause  (make-condition 'localgroup-client-rejected
                                    :operation ':client
                                    :message   "Injected initial refusal."))
            (calls  0)
            (waits  0))
       (test-call-with-function-replacements
        (list (list 'terminal--terminal-mode-or-nil
                    (lambda (candidate)
                      (declare (ignore candidate))
                      t))
              (list 'localgroup-client-connect
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf calls)
                      (error cause)))
              (list 'localgroup--wait-for-checkpoint-entry
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf waits)
                      (error "An initial refusal must not wait for a checkpoint."))))
        (lambda ()
          (test-assert
           (and (eq (handler-case (channel-view-run view)
                      (error (condition)
                        condition))
                    cause)
                (= calls 1)
                (zerop waits))
           "an initial refusal is signalled as itself, without checkpoint recovery")))
       (localgroup-screen-close screen))))
  nil)

(-> test-main-world-client-cleanup-cause nil null)
(defun test-main-world-client-cleanup-cause ()
  "Preserve shared-screen owner exits and original errors through independent cleanup failures."
  (with-test-configuration (configuration)
   (dolist (phase '(:exit :detach :recovery-quit :primary-error))
     (let* ((terminal  (make-instance 'scripted-terminal :columns 80))
            (panel     (world-sidebar-create configuration))
            (screen    (localgroup-screen-create :terminal terminal :panel panel))
            (exit      (when (eq phase ':exit) '(:status 7 :message "finished")))
            (cause
             (make-condition 'localgroup-error :operation ':attach :message
                             "original attachment failure"))
            (attempts  nil)
            (relayed-p nil)
            (relayed   nil)
            (choice-p  nil))
       (test-call-with-function-replacements
        (list
         (list 'world-sidebar-create
               (lambda (&rest arguments) (declare (ignore arguments)) panel))
         (list 'localgroup-client-screen-create
               (lambda (&rest arguments) (declare (ignore arguments)) screen))
         (list 'world-sidebar-start
               (lambda (candidate) (declare (ignore candidate)) nil))
         (list 'terminal--terminal-mode-or-nil
               (lambda (candidate) (declare (ignore candidate)) t))
         (list 'main--call-with-channel-loading
               (lambda (candidate title function &key &allow-other-keys)
                 (declare (ignore candidate title))
                 (funcall function)))
         (list 'main--open-world-channel
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (values "bfirst1" nil)))
         (list 'main--attach-world-channel
               (lambda
                   (candidate identifier mode
                    &key attached-function &allow-other-keys)
                 (declare (ignore candidate identifier mode))
                 (funcall attached-function)
                 (when (eq phase ':primary-error) (error cause))
                 (if (eq phase ':recovery-quit)
                     '(:attachment-lost :kind :callback-failure :message
                       "rendering failed")
                     exit)))
         (list 'main--attachment-choice
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (setf choice-p t)
                 nil))
         (list 'localgroup-client-screen-release
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "client-screen" attempts)
                 (error "private screen cleanup payload")))
         (list 'world-sidebar-stop
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "sidebar" attempts)
                 (error "private sidebar cleanup payload")))
         (list 'localgroup-relay-exit
               (lambda (candidate result mode &key session-id)
                 (declare (ignore candidate mode))
                 (test-assert (equal session-id "bfirst1")
                  "cleanup retains the last attached channel")
                 (setf relayed-p t
                       relayed result))))
        (lambda ()
          (if (eq phase ':primary-error)
              (test-assert
               (and
                (eq
                 (handler-case (main--run-world-client configuration)
                               (error (condition) condition))
                 cause)
                (not relayed-p))
               "independent cleanup cannot replace the original primary condition")
              (multiple-value-bind (success metadata)
                  (main--run-world-client configuration)
                (test-assert
                 (and success relayed-p (eq relayed exit)
                      (eq choice-p (eq phase ':recovery-quit))
                      (= (length (getf metadata :cleanup-failures)) 2)
                      (not (search "private" (write-to-string metadata))))
                 "exit, detach and recovery quit reach relay exit unchanged with class-only diagnostics")))
          (test-assert (equal (reverse attempts) '("client-screen" "sidebar"))
           "each shared-screen owner cleanup stage is attempted independently"))))))
  nil)

(-> test-main-attachment-choice () null)
(defun test-main-attachment-choice ()
  "Offer manual retry, a different channel and detach without invoking a renderer again."
  (dolist (entry '(((#\r) (:reattach))
                   (((:insert "bsecnd2") :enter) (:switch :session-id "bsecnd2"))
                   ((#\q) nil)))
    (let* ((events   (copy-list (first entry)))
           (expected (second entry))
           (terminal (make-instance 'scripted-terminal :columns 80))
           (screen   (localgroup-screen-create :terminal terminal)))
      (test-call-with-function-replacements
       (list (list 'terminal-read-event
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (or (pop events) ':end-of-input)))
             (list 'localgroup-screen-close
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (error "renderer cleanup failed"))))
       (lambda ()
         (let ((*error-output* (make-broadcast-stream)))
           (test-assert
            (equal (main--attachment-choice
                    screen "bfirst1" '(:kind :callback-failure :message "Rendering failed."))
                   expected)
            "a rendering failure leaves explicit attachment recovery choices"))))))
  nil)

(-> test-localgroup-client-accepted-setup-failure () null)
(defun test-localgroup-client-accepted-setup-failure ()
  "Signal a view's setup failure after its channel accepted it as itself, and release the channel, whatever cleanup fails."
  (client-recovery-tests--call-with-session
   (lambda (configuration application session)
     (dolist (stage '(:terminal-start :resize-watch :attached-callback))
       (let* ((terminal       (make-instance 'queued-recording-terminal :rows 31 :columns 91))
              (screen         (localgroup-screen-create :terminal terminal))
              (view           (channel-view-create
                               configuration (image-daemon:daemon-runtime-identifier session) screen))
              (cause          (make-condition 'localgroup-error :operation ':attach
                                                                :message   "Injected setup failure."))
              (cleanup-cause  (make-condition 'simple-error :format-control "Injected cleanup failure."))
              (start-terminal (symbol-function 'terminal-start))
              (stop-ui        (symbol-function 'terminal-ui-stop))
              (accepted-p     nil)
              (cleanups       nil))
         (test-call-with-function-replacements
          (list
           (list 'terminal--terminal-mode-or-nil
                 (lambda (candidate)
                   (declare (ignore candidate))
                   t))
           (list 'terminal-start
                 (lambda (candidate)
                   (when (eq candidate terminal)
                     (setf accepted-p (application-clients-controlled-p application))
                     (when (eq stage ':terminal-start)
                       (error cause)))
                   (funcall start-terminal candidate)))
           (list 'platform-watch-terminal-resize
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (if (eq stage ':resize-watch)
                       (error cause)
                       ':watch)))
           (list 'platform-unwatch-terminal-resize
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (push ':resize-watch cleanups)
                   (error cleanup-cause)))
           (list 'terminal-ui-stop
                 (lambda (candidate)
                   (cond
                     ((eq candidate (channel-view-ui view))
                      (push ':ui cleanups)
                      (error cleanup-cause))
                     (t
                      (funcall stop-ui candidate))))))
          (lambda ()
            (test-assert
             (eq (handler-case
                     (channel-view-run view
                                       :attached-function
                                       (lambda ()
                                         (when (eq stage ':attached-callback)
                                           (error cause))))
                   (error (condition)
                     condition))
                 cause)
             (format nil "a failure of the ~(~A~) after acceptance is signalled as itself" stage))))
         (test-assert (and accepted-p
                           (member ':ui cleanups)
                           (eq (not (null (member ':resize-watch cleanups)))
                               (eq stage ':attached-callback)))
                      "the channel had accepted the view, and every cleanup the view owes is attempted")
         (test-assert (and (test-localgroup--await-served session)
                           (not (application-clients-attached-p application)))
                      "the failed view releases its channel")
         (localgroup-screen-close screen)))))
  nil)

(-> test-world-client-attachment-recovery () null)
(defun test-world-client-attachment-recovery ()
  "Retry a lost attachment with bounded discovery and no channel open or work resubmission."
  (with-test-configuration (configuration)
    (let ((opens        0)
          (attaches     nil)
          (choices      0)
          (reported     nil)
          (attach-count 0))
      (test-call-with-function-replacements
       (list
        (list 'main--open-world-channel
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf opens)
                (values "bfirst1" nil)))
        (list 'world-client-open
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "Attachment recovery must not open or rerun a channel.")))
        (list 'world-sidebar-start (lambda (panel) (declare (ignore panel))))
        (list 'world-sidebar-stop (lambda (panel) (declare (ignore panel))))
        (list 'channel-view-run
              (lambda (view &key attached-function notice client-version boot-p)
                (declare (ignore notice client-version boot-p))
                (push (channel-subscription-identifier view) attaches)
                (incf attach-count)
                (case attach-count
                  (1
                   (funcall attached-function)
                   (list ':attachment-lost :kind    ':transport-failure
                                           :message "The channel's connection closed."))
                  (2
                   (error 'localgroup-error :operation ':client :message "Endpoint not ready."))
                  (otherwise
                   (funcall attached-function)
                   nil))))
        (list 'main--attachment-choice
              (lambda (screen identifier termination)
                (declare (ignore screen))
                (incf choices)
                (test-assert (and (string= identifier "bfirst1") (getf termination :message))
                             "the recovery prompt retains identity and reports each failure")
                '(:reattach)))
        (list 'localgroup-relay-exit
              (lambda (candidate exit mode &key session-id)
                (declare (ignore candidate mode))
                (setf reported (list session-id exit)))))
       (lambda ()
         (let ((*error-output* (make-broadcast-stream)))
           (main--run-world-client configuration))))
      (test-assert (and (= opens 1) (= choices 2)
                        (equal attaches '("bfirst1" "bfirst1" "bfirst1"))
                        (equal reported '("bfirst1" nil)))
                   "loss and a failed retry retain the terminal until explicit retry or departure")))
  nil)


(-> test-client-input-queue-lifecycle () null)
(defun test-client-input-queue-lifecycle ()
  "Bound queued input by count and payload, discard it on close, and reap an idle worker."
  (let* ((queue (make-instance 'client-input-queue))
         (gate (sb-thread:make-semaphore))
         (started (sb-thread:make-semaphore))
         (*client-input-queue-count-limit* 2)
         (*client-input-queue-character-limit* 6)
         (late-calls 0))
    (unwind-protect
         (progn
           (client-input-queue-push queue
                                    (lambda ()
                                      (sb-thread:signal-semaphore started)
                                      (sb-thread:wait-on-semaphore gate :timeout 5)))
           (test-assert (sb-thread:wait-on-semaphore started :timeout 5) "The first input owns the worker.")
           (flet ((late () (incf late-calls)))
             (test-assert (eq (client-input-queue-push queue #'late :characters 7) ':overflow)
                          "One oversized input is refused before retention.")
             (dotimes (index 2)
               (declare (ignore index))
               (test-assert (eq (client-input-queue-push queue #'late :characters 3) ':accepted)
                            "Input within both budgets is admitted."))
             (test-assert (eq (client-input-queue-push queue #'late) ':overflow)
                          "The count bound also applies to small commands."))
           (client-input-queue-close queue)
           (test-assert (and (zerop (deque-count (client-input-queue-functions queue)))
                             (zerop (client-input-queue-characters queue))
                             (eq (client-input-queue-push queue (constantly nil)) ':closed))
                        "Close drops all waiting payloads and closes admission without joining active work."))
      (sb-thread:signal-semaphore gate)
      (client-input-queue-close queue)
      (when (client-input-queue-thread queue) (join-thread (client-input-queue-thread queue))))
    (test-assert (zerop late-calls) "No queued input runs after its attachment closes."))
  (let* ((queue (make-instance 'client-input-queue))
         (started (sb-thread:make-semaphore))
         (exit-gate (sb-thread:make-semaphore))
         (closer nil)
         (closed-p nil))
    (test-call-with-function-replacements
     (list (list 'client-input-queue--run
                 (lambda (candidate)
                   (sb-thread:signal-semaphore started)
                   (with-lock-held ((client-input-queue-lock candidate))
                     (loop until (client-input-queue-closed-p candidate)
                           do (condition-wait (client-input-queue-condition-variable candidate)
                                              (client-input-queue-lock candidate))))
                   (sb-thread:wait-on-semaphore exit-gate :timeout 5)
                   nil)))
     (lambda ()
       (unwind-protect
            (progn
              (client-input-queue-push queue (constantly nil))
              (test-assert (sb-thread:wait-on-semaphore started :timeout 5) "An idle input worker exists.")
              (setf closer (make-thread (lambda () (client-input-queue-close queue) (setf closed-p t))
                                        :name "idle input closer"))
              (test-assert (task-tests--wait-until (lambda () (client-input-queue-closed-p queue)) 5)
                           "The closer notifies idle work before waiting.")
              (test-assert (not closed-p) "Closure cannot finish while the idle worker has not exited.")
              (sb-thread:signal-semaphore exit-gate)
              (join-thread closer)
              (test-assert (and closed-p (not (thread-alive-p (client-input-queue-thread queue))))
                           "Idle input workers are gone when closure returns."))
         (sb-thread:signal-semaphore exit-gate)
         (when closer (join-thread closer))
         (client-input-queue-close queue)))))
  nil)

(-> test-channel-view-history-delivery () null)
(defun test-channel-view-history-delivery ()
  "Read a history reply behind a presentation delta, and settle closure during a history request."
  (with-test-configuration (configuration)
    (dolist (answer-p '(t nil))
      (let* ((screen (localgroup-screen-create :terminal (make-instance 'scripted-terminal :columns 80)))
             (view (channel-view-create configuration "history" screen))
             (wire (concatenate 'string
                                (image-daemon:daemon-packet-string '(:ui :operation :turn :active-p t))
                                (if answer-p
                                    (image-daemon:daemon-packet-string '(:reply :id 1 :entries ("older"))) "")))
             (connection (make-instance 'localgroup-client-connection :socket nil
                                        :stream (make-string-input-stream wire)))
             (reader nil))
        (setf (channel-subscription-connection view) connection)
        (channel-view-apply view ':history :entries '("recent") :offset 1 :paged-p t)
        (test-call-with-function-replacements
         (list (list 'localgroup-client--write-frame
                     (lambda (candidate frame)
                       (declare (ignore frame))
                       (setf reader (make-thread
                                     (lambda ()
                                       (localgroup-client--read
                                       candidate (lambda (event) (channel-subscription-receive-event view event)))))
                             (localgroup-client-connection-reader-thread candidate) reader)
                       nil)))
         (lambda ()
           (unwind-protect
                (sb-sys:with-deadline (:seconds 5)
                  (terminal-ui-process-event (channel-view-ui view) ':history-previous)
                  (terminal-ui-process-event (channel-view-ui view) ':history-previous)
                  (join-thread reader)
                  (test-assert (equal (line-editor-text (terminal-ui-editor (channel-view-ui view)))
                                      (if answer-p "older" "recent"))
                               "Paging finishes with its reply or the prior selection after close.")
                  (test-assert (not (channel-view-turn-active-p view))
                               "The transport reader never applies a delta under the UI's history lock.")
                  (channel-view--drain-events view)
                  (test-assert (and (channel-view-turn-active-p view)
                                    (channel-subscription-closed-p view))
                               "The UI owner applies the delta before its ordered closure event."))
             (channel-view--close-events view)
             (channel-subscription-close view)
             (when reader (join-thread reader))
             (localgroup-screen-close screen)))))))
  nil)

(-> test-channel-view-event-backpressure () null)
(defun test-channel-view-event-backpressure ()
  "Retire an overrun view rather than dropping an ordered delta, by count or storage-byte budget."
  (dolist (large-p '(nil t))
    (let* ((view (make-instance 'channel-view))
           (connection (make-instance 'localgroup-client-connection :socket nil
                                      :stream (make-string-output-stream)))
           (*channel-view-event-count-limit* 2)
           (*channel-view-event-byte-limit* 1024))
      (setf (channel-subscription-connection view) connection)
      (if large-p
          (channel-subscription-receive-event view
                                              (list :ui :operation :notice :text (make-string 1024)))
          (dotimes (index 3)
            (channel-subscription-receive-event view (list :ui :operation :turn :active-p (evenp index)))))
      (test-assert (and (channel-subscription-closed-p view)
                        (localgroup-client-connection-closed-p connection)
                        (client-evaluator-closed-p (localgroup-client-connection-evaluator connection))
                        (eq (getf (rest (channel-view-exit view)) :kind) ':queue-overflow)
                        (zerop (deque-count (channel-view-events view)))
                        (zerop (channel-view-event-bytes view)))
                   "Overflow closes transport, evaluation and ordered delivery and releases queued payloads.")
      (channel-subscription-receive-event view '(:ui :operation :turn :active-p t))
      (test-assert (zerop (deque-count (channel-view-events view))) "Late deltas cannot reopen a closed view.")))
  nil)

(-> test-channel-view-bounded-detach () null)
(defun test-channel-view-bounded-detach ()
  "A goodbye waiting behind a stalled writer cannot prevent retiring the view's connection."
  (let* ((view (make-instance 'channel-view))
         (connection (make-instance 'localgroup-client-connection :socket nil
                                    :stream (make-string-output-stream)))
         (gate (sb-thread:make-semaphore))
         (started (sb-thread:make-semaphore))
         (writer nil))
    (setf (channel-subscription-connection view) connection)
    (unwind-protect
         (progn
           (setf writer (make-thread
                         (lambda ()
                           (with-lock-held ((localgroup-client-connection-write-lock connection))
                             (sb-thread:signal-semaphore started)
                             (sb-thread:wait-on-semaphore gate :timeout 5)))
                         :name "stalled detach writer"))
           (test-assert (sb-thread:wait-on-semaphore started :timeout 5) "Another writer owns the transport.")
           (sb-sys:with-deadline (:seconds 2)
             (let ((*channel-view-detach-seconds* 0.05)) (channel-view--leave view)))
           (test-assert (and (localgroup-client-connection-closed-p connection)
                             (client-evaluator-closed-p (localgroup-client-connection-evaluator connection)))
                        "A bounded goodbye retires the connection and evaluator outside the writer lock."))
      (sb-thread:signal-semaphore gate)
      (when writer (join-thread writer))
      (channel-subscription-close view)))
  nil)


(-> test-channel-view-initial-replay () null)
(defun test-channel-view-initial-replay ()
  "Stream a large initial replay on the UI owner, then admit live delivery; reject broken prefixes."
  (with-test-configuration (configuration)
    (dolist (mode '(:replay :old :eof :closed :timeout))
      (let* ((screen (localgroup-screen-create :terminal (make-instance 'scripted-terminal :columns 80)))
             (view (channel-view-create configuration "replay" screen))
             (wire (with-output-to-string (output)
                     (write-string (image-daemon:daemon-packet-string '(:ui :operation :reset)) output)
                     (dotimes (index 1000)
                       (write-string (image-daemon:daemon-packet-string
                                      (list :ui :operation :turn :active-p (evenp index))) output))
                     (unless (eq mode ':eof)
                       (write-string (image-daemon:daemon-packet-string '(:ui :operation :replayed)) output))
                     (write-string (image-daemon:daemon-packet-string '(:ui :operation :turn :active-p t)) output)))
             (connection (make-instance 'localgroup-client-connection :socket nil
                                        :stream (make-string-input-stream wire)
                                        :attached (list :attached :sequence 0 :record-sequence 0
                                                        :presentation-replay-p (not (eq mode ':old)))))
             (*channel-view-event-count-limit* 2))
        (unwind-protect
             (progn
               (channel-subscription-begin-attachment view)
               (case mode
                 (:closed
                  (channel-subscription--mark-closed view)
                  (test-assert (handler-case (progn (channel-subscription-initialize-connection view connection) nil)
                                 (channel-subscription-unavailable () t))
                               "A closed view refuses replay initialization."))
                (:timeout
                 (let ((candidate nil)
                       (image-daemon:*daemon-connect-timeout-seconds* 0.02))
                   (test-call-with-function-replacements
                    (list (list 'daemon-connect
                                (lambda (port) (declare (ignore port))
                                  (values nil (localgroup-client-connection-stream connection))))
                          (list 'daemon-write-packet (lambda (&rest values) (declare (ignore values)) nil))
                          (list 'localgroup-client--read-greeting
                                (lambda (stream cancelled-p) (declare (ignore stream cancelled-p))
                                  (localgroup-client-connection-attached connection)))
                          (list 'daemon-read-decodable-packet
                                (lambda (stream) (declare (ignore stream)) (sleep 5))))
                    (lambda ()
                      (test-assert
                       (handler-case
                           (localgroup-client-connect
                            '("replay" :localgroup :port 1 :token "unused") :event-function (constantly nil)
                            :initialize-function
                            (lambda (created)
                              (setf candidate created)
                              (channel-subscription-initialize-connection view created)))
                         (sb-sys:deadline-timeout () t))
                       "A stalled initial prefix respects the handshake deadline.")))
                   (test-assert (and candidate (localgroup-client-connection-closed-p candidate)
                                     (client-evaluator-closed-p (localgroup-client-connection-evaluator candidate))
                                     (null (localgroup-client-connection-reader-thread candidate)))
                                "A failed prefix retires transport and evaluation without starting a reader.")))
                 (:eof
                  (test-assert (handler-case (progn (channel-subscription-initialize-connection view connection) nil)
                                 (channel-subscription-unavailable () t))
                               "EOF before REPLAYED refuses the incomplete attachment."))
                 (otherwise
                  (channel-subscription-initialize-connection view connection)
                  (test-assert (and (zerop (deque-count (channel-view-events view)))
                                    (not (channel-subscription-closed-p view)))
                               "The initial transcript never accumulates in the bounded live queue.")
                  (test-assert
                   (eq (getf (rest (daemon-read-decodable-packet (localgroup-client-connection-stream connection))) :operation)
                       (if (eq mode ':old) ':reset ':turn))
                   "Only an advertised replay prefix is consumed, stopping before the first live event.")
                  (let ((*channel-view-replaying* view))
                    (test-assert (handler-case (progn (channel-subscription-call view ':input-history) nil)
                                   (channel-subscription-unavailable () t))
                                 "Prefix presentation cannot wait for its not-yet-started reader."))
                  (when (eq mode ':replay)
                    (setf (channel-subscription-connection view) connection)
                    (channel-subscription-finish-attachment view (localgroup-client-connection-attached connection))
                    (channel-subscription-receive-event view '(:ui :operation :turn :active-p t))
                    (test-assert (not (channel-view-turn-active-p view)) "The last replayed state precedes live delivery.")
                    (channel-view--drain-events view)
                    (test-assert (channel-view-turn-active-p view) "Live delivery follows the replay boundary.")))))
          (localgroup-client--disconnect connection)
          (channel-subscription-close view)
          (localgroup-screen-close screen)))))
  nil)
