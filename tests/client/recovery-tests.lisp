(in-package #:autolith)

(-> client-recovery-tests--call-with-session (function) null)
(defun client-recovery-tests--call-with-session (function)
  "Call FUNCTION with a configuration, a lingering application without a terminal of its own, and its session."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-world-terminal--application configuration :presented-p t)
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (terminal-ui-start (application-ui application))
             (funcall function configuration application
                      (world-terminal-start application)))
        (world-terminal-stop application)
        (application-input-controller-stop controller)
        (terminal-ui-stop (application-ui application))
        (application-release-conversation-lease application))))
  nil)

(-> test-world-terminal-client-checkpoint-recovery () null)
(defun test-world-terminal-client-checkpoint-recovery ()
  "End a view as a lost attachment when its checkpoint reconnect fails, and signal an initial refusal as itself."
  (dolist (phase '(:wait :reattach))
    (client-recovery-tests--call-with-session
     (lambda (configuration application session)
       (declare (ignore application))
       (let ((entry (cons (image-daemon:daemon-runtime-registry-pathname session)
                          (world-terminal--registry-record session)))
             (cause (make-condition (if (eq phase ':wait) 'world-terminal-error 'world-terminal-client-rejected)
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
              (list 'world-terminal--wait-for-checkpoint-entry
                    (lambda (candidate transition &key &allow-other-keys)
                      (push (list (first candidate) transition) waits)
                      (when (eq phase ':wait)
                        (error cause))
                      entry))
              (list 'world-terminal-client-connect
                    (lambda (candidate &key after-sequence &allow-other-keys)
                      (push (list (first candidate) (integerp after-sequence)) calls)
                      (error cause))))
             (lambda ()
               (channel-subscription-event view (list :checkpoint-reconnect :id "transition"))
               (world-terminal-client-close (channel-subscription-connection view))
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
     (let* ((screen (world-terminal-screen-create
                     :terminal (make-instance 'queued-recording-terminal :rows 31 :columns 91)))
            (view   (channel-view-create
                     configuration (image-daemon:daemon-runtime-identifier session) screen))
            (cause  (make-condition 'world-terminal-client-rejected
                                    :operation ':client
                                    :message   "Injected initial refusal."))
            (calls  0)
            (waits  0))
       (test-call-with-function-replacements
        (list (list 'terminal--terminal-mode-or-nil
                    (lambda (candidate)
                      (declare (ignore candidate))
                      t))
              (list 'world-terminal-client-connect
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf calls)
                      (error cause)))
              (list 'world-terminal--wait-for-checkpoint-entry
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
       (world-terminal-screen-close screen))))
  nil)

(-> test-main-world-client-cleanup-cause nil null)
(defun test-main-world-client-cleanup-cause ()
  "Preserve shared-screen owner exits and original errors through independent cleanup failures."
  (with-test-configuration (configuration)
   (dolist (phase '(:exit :detach :primary-error))
     (let* ((terminal  (make-instance 'scripted-terminal :columns 80))
            (panel     (world-sidebar-create configuration))
            (screen    (world-terminal-screen-create :terminal terminal :panel panel))
            (exit      (when (eq phase ':exit) '(:status 7 :message "finished")))
            (cause
             (make-condition 'world-terminal-error :operation ':attach :message
                             "original attachment failure"))
            (attempts  nil)
            (relayed-p nil)
            (relayed   nil))
       (test-call-with-function-replacements
        (list
         (list 'world-sidebar-create
               (lambda (&rest arguments) (declare (ignore arguments)) panel))
         (list 'world-terminal-client-screen-create
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
                 exit))
         (list 'world-terminal-client-screen-release
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "client-screen" attempts)
                 (error "private screen cleanup payload")))
         (list 'world-sidebar-stop
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "sidebar" attempts)
                 (error "private sidebar cleanup payload")))
         (list 'world-terminal-relay-exit
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
                      (= (length (getf metadata :cleanup-failures)) 2)
                      (not (search "private" (write-to-string metadata))))
                 "exit and detach reach relay exit unchanged with class-only diagnostics")))
          (test-assert (equal (reverse attempts) '("client-screen" "sidebar"))
           "each shared-screen owner cleanup stage is attempted independently"))))))
  nil)

(-> test-world-terminal-client-accepted-setup-failure () null)
(defun test-world-terminal-client-accepted-setup-failure ()
  "Signal a view's setup failure after its channel accepted it as itself, and release the channel, whatever cleanup fails."
  (client-recovery-tests--call-with-session
   (lambda (configuration application session)
     (dolist (stage '(:terminal-start :resize-watch :attached-callback))
       (let* ((terminal       (make-instance 'queued-recording-terminal :rows 31 :columns 91))
              (screen         (world-terminal-screen-create :terminal terminal))
              (view           (channel-view-create
                               configuration (image-daemon:daemon-runtime-identifier session) screen))
              (cause          (make-condition 'world-terminal-error :operation ':attach
                                                                :message       "Injected setup failure."))
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
         (test-assert (and (test-world-terminal--await-served session)
                           (not (application-clients-attached-p application)))
                      "the failed view releases its channel")
         (world-terminal-screen-close screen)))))
  nil)

(-> test-client-input-queue-lifecycle () null)
(defun test-client-input-queue-lifecycle ()
  "Bound queued input by count and payload, discard it on close, and reap an idle worker."
  (let* ((queue                                (make-instance 'client-input-queue))
         (gate                                 (sb-thread:make-semaphore))
         (started                              (sb-thread:make-semaphore))
         (*client-input-queue-count-limit*     2)
         (*client-input-queue-character-limit* 6)
         (late-calls                           0))
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
             (loop repeat 2
                   do (test-assert (eq (client-input-queue-push queue #'late :characters 3) ':accepted)
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
  (let* ((queue     (make-instance 'client-input-queue))
         (started   (sb-thread:make-semaphore))
         (exit-gate (sb-thread:make-semaphore))
         (closer    nil)
         (closed-p  nil))
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
      (let* ((screen     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 80)))
             (view       (channel-view-create configuration "history" screen))
             (wire       (concatenate 'string
                                      (image-daemon:daemon-packet-string '(:ui :operation :turn :active-p t))
                                      (if answer-p
                                          (image-daemon:daemon-packet-string '(:reply :id 1 :entries ("older"))) "")))
             (connection (make-instance 'world-terminal-client-connection :socket nil
                                        :stream                                   (make-string-input-stream wire)))
             (reader     nil))
        (setf (channel-subscription-connection view) connection)
        (channel-view-apply view ':history :entries '("recent") :offset 1 :paged-p t)
        (test-call-with-function-replacements
         (list (list 'world-terminal-client--write-frame
                     (lambda (candidate frame)
                       (declare (ignore frame))
                       (setf reader (make-thread
                                     (lambda ()
                                       (world-terminal-client--read
                                        candidate (lambda (event) (channel-subscription-receive-event view event)))))
                             (world-terminal-client-connection-reader-thread candidate) reader)
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
             (world-terminal-screen-close screen)))))))
  nil)

(-> test-channel-view-detach () null)
(defun test-channel-view-detach ()
  "Detach through transport shutdown without acquiring a stalled writer's lock."
  (let* ((view       (make-instance 'channel-view))
         (connection (make-instance 'world-terminal-client-connection :socket nil
                                    :stream                                   (make-string-output-stream)))
         (gate       (sb-thread:make-semaphore))
         (started    (sb-thread:make-semaphore))
         (writer     nil))
    (setf (channel-subscription-connection view) connection)
    (unwind-protect
         (progn
           (setf writer (make-thread
                         (lambda ()
                           (with-lock-held ((world-terminal-client-connection-write-lock connection))
                             (sb-thread:signal-semaphore started)
                             (sb-thread:wait-on-semaphore gate)))
                         :name "stalled detach writer"))
           (test-assert (sb-thread:wait-on-semaphore started :timeout 5) "Another writer owns the transport.")
           (channel-view--leave view)
           (test-assert (and (thread-alive-p writer)
                             (world-terminal-client-connection-closed-p connection)
                             (client-evaluator-closed-p (world-terminal-client-connection-evaluator connection)))
                        "Detach retires the connection and evaluator without waiting for the writer."))
      (sb-thread:signal-semaphore gate)
      (when writer (join-thread writer))
      (channel-subscription-close view)))
  nil)

(-> test-channel-view-initial-replay () null)
(defun test-channel-view-initial-replay ()
  "Stream a large initial replay on the UI owner, then admit live delivery; reject broken prefixes."
  (with-test-configuration (configuration)
    (dolist (mode '(:replay :old :eof :closed))
      (let* ((screen     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 80)))
             (view       (channel-view-create configuration "replay" screen))
             (wire       (with-output-to-string (output)
                           (write-string (image-daemon:daemon-packet-string '(:ui :operation :reset)) output)
                           (dotimes (index 1000)
                             (write-string (image-daemon:daemon-packet-string
                                            (list :ui :operation :turn :active-p (evenp index))) output))
                           (unless (eq mode ':eof)
                             (write-string (image-daemon:daemon-packet-string '(:ui :operation :replayed)) output))
                           (write-string (image-daemon:daemon-packet-string '(:ui :operation :turn :active-p t)) output)))
             (connection (make-instance 'world-terminal-client-connection :socket nil
                                        :stream                                   (make-string-input-stream wire)
                                        :attached                                 (list :attached :sequence 0 :record-sequence 0
                                                                                         :presentation-replay-p (not (eq mode ':old))))))
        (unwind-protect
             (progn
               (channel-subscription-begin-attachment view)
               (case mode
                 (:closed
                  (channel-subscription--mark-closed view)
                  (test-assert (handler-case (progn (channel-subscription-initialize-connection view connection) nil)
                                 (channel-subscription-unavailable () t))
                               "A closed view refuses replay initialization."))
                (:eof
                 (test-assert (handler-case (progn (channel-subscription-initialize-connection view connection) nil)
                                (channel-subscription-unavailable () t))
                              "EOF before REPLAYED refuses the incomplete attachment."))
                (otherwise
                 (channel-subscription-initialize-connection view connection)
                 (test-assert (and (zerop (deque-count (channel-view-events view)))
                                   (not (channel-subscription-closed-p view)))
                              "The initial transcript is delivered before the live queue is used.")
                 (test-assert
                  (eq (getf (rest (daemon-read-decodable-packet (world-terminal-client-connection-stream connection))) :operation)
                      (if (eq mode ':old) ':reset ':turn))
                  "Only an advertised replay prefix is consumed, stopping before the first live event.")
                 (let ((*channel-view-replaying* view))
                   (test-assert (handler-case (progn (channel-subscription-call view ':input-history) nil)
                                  (channel-subscription-unavailable () t))
                                "Prefix presentation cannot wait for its not-yet-started reader."))
                 (when (eq mode ':replay)
                   (setf (channel-subscription-connection view) connection)
                   (channel-subscription-finish-attachment view (world-terminal-client-connection-attached connection))
                   (channel-subscription-receive-event view '(:ui :operation :turn :active-p t))
                   (test-assert (not (channel-view-turn-active-p view)) "The last replayed state precedes live delivery.")
                   (channel-view--drain-events view)
                   (test-assert (channel-view-turn-active-p view) "Live delivery follows the replay boundary.")))))
          (world-terminal-client--disconnect connection)
          (channel-subscription-close view)
          (world-terminal-screen-close screen)))))
  nil)

(-> client-history-tests--text (channel-view) string)
(defun client-history-tests--text (view)
  "Return all resident transcript rows of VIEW in display order."
  (let ((viewport (fullscreen-terminal-ui-viewport (channel-view-ui view))))
    (format nil "~{~A~%~}"
            (loop for row below (clinedi:transcript-viewport-row-count viewport)
                  collect (clinedi:transcript-viewport-row-text viewport row)))))

(-> client-history-tests--settle (channel-view) null)
(defun client-history-tests--settle (view)
  "Read a transport barrier and apply all preceding presentation to VIEW."
  (channel-subscription-call view ':state)
  (channel-view--drain-events view)
  nil)

(-> test-channel-view-durable-history () null)
(defun test-channel-view-durable-history ()
  "Seed a second real view after cache eviction and page without changing its peer."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((identifier
              (getf (rest (world-server-tests--open
                           (world-client-entry configuration)
                           (world-tests--workspace root "client-history")))
                    :session-id))
            (application  (world-host-application (world-server--find-host server identifier)))
            (conversation (application-conversation application))
            (screen-a     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (screen-b     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (view-a       (channel-view-create configuration identifier screen-a))
            (view-b       (channel-view-create configuration identifier screen-b)))
       (unwind-protect
            (progn
              (channel-subscription-attach view-a)
              (let ((*channel-presentation-log-character-limit* 2048))
                (dotimes (index 300)
                  (conversation-append-user-message
                   conversation (format nil "history-item-~3,'0D ~A" (1+ index)
                                        (make-string 80 :initial-element #\x)))
                  (application-render-records application)
                  (when (zerop (mod index 32))
                    (client-history-tests--settle view-a))))
              (client-history-tests--settle view-a)
              (test-assert (and (search "history-item-001" (client-history-tests--text view-a))
                                (not (search "history-item-060" (test-world-terminal--presented-text application))))
                           "The first view retains entries evicted from the server replay cache.")
              ;; Persist just before attachment, and publish the same record just after it.
              (conversation-append-user-message conversation "history-boundary-301")
              (channel-subscription-attach view-b)
              (application-render-records application)
              (conversation-append-user-message conversation "history-live-302")
              (application-render-records application)
              (client-history-tests--settle view-a)
              (client-history-tests--settle view-b)
              (let ((text-a (client-history-tests--text view-a))
                    (text-b (client-history-tests--text view-b)))
                (test-assert (and (search "history-item-060" text-b)
                                  (not (search "history-item-001" text-b)))
                             "A fresh second view receives the newest durable page, not the evicted cache.")
                (dolist (text (list text-a text-b))
                  (test-assert (and (= 1 (test-substring-count "history-boundary-301" text))
                                    (= 1 (test-substring-count "history-live-302" text)))
                               "The durable/live cut presents both boundary and later records exactly once.")))
              (let ((floor-a      (channel-view-transcript-history-floor view-a))
                    (text-a       (client-history-tests--text view-a))
                    (shared-floor (application-history-floor-sequence application)))
                (let ((ui (channel-view-ui view-b)))
                  (terminal-ui-set-input ui "paging draft")
                  (terminal-ui-process-event ui ':scroll-top)
                  (terminal-ui-process-event ui ':page-up)
                  (test-assert (string= "paging draft" (line-editor-text (terminal-ui-editor ui)))
                               "Automatic disk paging preserves the composer draft."))
                (client-history-tests--settle view-a)
                (test-assert (and (search "history-item-001" (client-history-tests--text view-b))
                                  (= floor-a (channel-view-transcript-history-floor view-a))
                                  (equal text-a (client-history-tests--text view-a))
                                  (eql shared-floor (application-history-floor-sequence application)))
                             "Paging changes only the requesting view and its floor."))
              (let* ((text  (client-history-tests--text view-b))
                     (older (search "history-item-001" text))
                     (newer (search "history-item-060" text)))
                (test-assert (and older newer (< older newer))
                             "Scrolling beyond the resident edge prepends disk history in transcript order."))
              (let ((image-daemon:*daemon-packet-character-limit* 8192)
                    (presentation                                 (application-presentation application)))
                (with-terminal-ui-locked (presentation)
                  (with-recursive-lock-held ((conversation-append-lock conversation))
                    (multiple-value-bind (events fields)
                        (application-client-history-page application (conversation-next-sequence conversation))
                      (declare (ignore fields))
                      (test-assert (> (length events) 1) "A durable page partitions into bounded transport batches.")
                      (test-assert (every (lambda (event)
                                             (< (length (image-daemon:daemon-packet-string (application-client-value event))) 8192))
                                          events)
                                   "Every durable page batch fits the actual packet serializer."))
                   (let ((before (conversation-next-sequence conversation)))
                     (multiple-value-bind (events fields)
                         (application-client-history-page application before :character-limit 6000)
                       (let* ((floor (getf fields :before-sequence))
                              (ids   (loop for event in events
                                           append (mapcar (lambda (record) (third (first record)))
                                                          (getf (rest event) :presentations)))))
                         (test-assert (and events (getf fields :more-p) (< floor before)
                                           (eql floor (first ids)) (eql (1- before) (first (last ids))))
                                      "An aggregate-limited page returns a contiguous newest suffix and its true floor.")
                         (test-assert (<= (reduce #'+ events :key #'application-client-history--frame-size) 6000)
                                      "The aggregate is bounded even when each packet fits separately."))))))))
         (channel-subscription-close view-b)
         (channel-subscription-close view-a)
         (world-terminal-screen-close screen-b)
         (world-terminal-screen-close screen-a)))))
  nil)

(-> test-channel-view-stream-history-cut () null)
(defun test-channel-view-stream-history-cut ()
  "Attach across a persisted multipart response and replay completed text only once."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((identifier
              (getf (rest (world-server-tests--open
                           (world-client-entry configuration)
                           (world-tests--workspace root "stream-history"))) :session-id))
            (application  (world-host-application (world-server--find-host server identifier)))
            (conversation (application-conversation application))
            (screen-a     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (screen-b     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (screen-c     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (view-a       (channel-view-create configuration identifier screen-a))
            (view-b       (channel-view-create configuration identifier screen-b))
            (view-c       (channel-view-create configuration identifier screen-c)))
       (unwind-protect
            (let ((observer (application-agent-observer application))
                  (first    "stream-alpha")
                  (second   "stream-bravo")
                  (summary  "stream-reasoning-fixture")
                  (later    "stream-charlie")
                  (last     "stream-delta"))
              (channel-subscription-attach view-a)
              (setf (application-reasoning-traces-p application) t
                    (application-cache-miss-notices-p application) t)
              (application-render-records application)
              (agent-observer-status observer ':provider-request-started nil)
              (agent-observer-status observer ':provider-request-completed
                                     (list :usage (prompt-cache-tests--usage 50000 40000)))
              (agent-observer-status observer ':provider-request-started nil)
              (agent-observer-reasoning observer summary)
              (agent-observer-text observer (format nil "~A~%~A" first second))
              (conversation-append-provider-item
               conversation (json-object "type" "reasoning" "summary"
                                         (json-array (json-object "type" "summary_text" "text" summary))))
              (dolist (text (list first second))
                (conversation-append-provider-item conversation (agent-test-message text)))
              (channel-subscription-attach view-b)
              (let ((inputs (mapcar #'json-encode (conversation-input-items-for-request conversation))))
                (agent-observer-status observer ':provider-request-completed
                                       (list :usage (prompt-cache-tests--usage 51000 0)))
                (test-assert (equal inputs (mapcar #'json-encode (conversation-input-items-for-request conversation)))
                             "Persisting a cache notice does not change model input."))
              (agent-observer-status observer ':provider-request-started nil)
              (agent-observer-status observer ':provider-request-completed
                                     (list :usage (prompt-cache-tests--usage 52000 40000)))
              (agent-observer-status observer ':provider-request-started nil)
              (conversation-append-provider-item conversation (agent-test-message later))
              (agent-observer-status observer ':provider-request-completed
                                     (list :usage (prompt-cache-tests--usage 53000 0)))
              (conversation-append-user-message conversation last)
              (application-render-records application)
              (channel-subscription-attach view-c)
              (dolist (view (list view-a view-b view-c))
                (client-history-tests--settle view)
                (let ((display (client-history-tests--text view)))
                  (dolist (text (list first second summary))
                    (test-assert (= 1 (test-substring-count text display))
                                 "Each multipart or reasoning fragment appears once across the stream cut"))
                  (let* ((reply        (search second display))
                         (first-notice (search "cache miss" display))
                         (later-reply  (search later display))
                         (last-notice  (and first-notice
                                            (search "cache miss" display :start2 (1+ first-notice))))
                         (last-input   (search last display)))
                    (test-assert (and reply first-notice later-reply last-notice last-input
                                      (< reply first-notice later-reply last-notice last-input)
                                      (= 2 (test-substring-count "cache miss" display)))
                                 "Live and fresh views show both cache notices once, in request order.")))))
         (dolist (view (list view-c view-b view-a)) (channel-subscription-close view))
         (dolist (screen (list screen-c screen-b screen-a)) (world-terminal-screen-close screen))))))
  nil)

(-> test-channel-view-history-admission () null)
(defun test-channel-view-history-admission ()
  "Respect exact builtin identity and keep page failures local without resubmission."
  (with-test-configuration (configuration)
    (let* ((screen     (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
           (view       (channel-view-create configuration "history-admission" screen))
           (page-calls 0)
           (sent       nil)
           (mode       ':builtin))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'channel-subscription-call
                   (lambda (candidate command &rest arguments)
                     (declare (ignore candidate arguments))
                     (test-assert (eq command ':transcript-history) "Local paging requests the scoped transcript endpoint.")
                     (incf page-calls)
                     (ecase mode
                       (:builtin
                         (list :history-page-p t :builtin-p t :conversation "history-admission"
                               :before-sequence 42 :events nil))
                       (:overridden
                        (list :history-page-p t :builtin-p nil))
                       (:failure
                        (error 'world-terminal-error :operation ':transcript-history :message "fixture page failure"))
                       (:malformed
                        nil))))
             (list 'channel-view--send-input
                   (lambda (candidate action payload)
                     (declare (ignore candidate action))
                     (push payload sent)
                     t)))
            (lambda ()
              (channel-view-apply view ':transcript-history
                                  :conversation "history-admission" :before-sequence 42)
              (test-assert (and (input-port-apply view ':submit "(history)")
                                (= page-calls 1) (null sent))
                           "Only the advertised canonical form uses local paging.")
              (dolist (input '("'(history)" "(progn (history))" "(history nil)" "(HISTORY)" "( history)" "/history"))
                (input-port-apply view ':submit input))
              (test-assert (and (= page-calls 1) (= (length sent) 6))
                           "Quoted, nested, alternate and noncanonical forms follow ordinary input.")
              (setf mode ':overridden)
              (input-port-apply view ':submit "(history)")
              (test-assert (and (= page-calls 2) (= (length sent) 7))
                           "A pager overridden after advertisement follows its effective operation.")
              (setf mode ':builtin)
              (input-port-apply view ':submit "(history)")
              (test-assert (and (= page-calls 3) (= (length sent) 7))
                           "Restoring the built-in pager restores scoped paging without reconnecting.")
              (dolist (failure-mode '(:failure :malformed))
                (setf mode failure-mode)
                (let ((prior (length sent)))
                  (test-assert (input-port-apply view ':submit "(history)") "A failed scoped page is handled locally.")
                  (test-assert (= prior (length sent)) "Page failures cannot invoke shared history as a fallback.")))
              (test-assert (and (search "fixture page failure" (client-history-tests--text view))
                                (search "did not return a transcript history page" (client-history-tests--text view)))
                           "Both transport and malformed-page failures are visible in this view.")))
        (world-terminal-screen-close screen))))
  nil)

(-> test-channel-view-drafts () null)
(defun test-channel-view-drafts ()
  "Exercise real editor drafts across view replacement, failed attachment, submission and client isolation."
  (with-test-configuration (configuration)
    (let* ((view              nil)
           (initial           nil)
           (seen-p            nil)
           (attach-fails-p    nil)
           (replay-input      nil)
           (modal-at-attach-p nil)
           (sent              nil)
           (terminal          nil)
           (screen            nil))
      (setf terminal
            (make-instance 'scripted-terminal :columns 90 :rows 30
                           :read-callback
                           (lambda ()
                             (unless seen-p
                               (let ((editor (terminal-ui-editor (channel-view-ui view))))
                                 (setf initial (list (line-editor-text editor) (line-editor-cursor editor))
                                       seen-p t)))
                             (when (eq (first (scripted-terminal-events terminal)) ':draft-loss)
                               (error 'world-terminal-error :operation ':client :message "Injected input loss."))))
            screen (world-terminal-screen-create :terminal terminal))
      (test-call-with-function-replacements
       (list
        (list 'terminal--terminal-mode-or-nil (lambda (candidate) (declare (ignore candidate)) ':test))
        (list 'terminal-input-ready-p (lambda (candidate) (declare (ignore candidate)) t))
        (list 'platform-watch-terminal-resize (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (list 'channel-subscription-attach
              (lambda (candidate)
                (when attach-fails-p
                  (error 'world-terminal-error :operation ':attach :message "Injected attach refusal."))
                (when modal-at-attach-p
                  (channel-view-apply candidate ':modal :open-p t))
                (when replay-input
                  (terminal-ui-set-input (channel-view-ui candidate) replay-input))))
        (list 'channel-view--server-version (lambda (candidate) (declare (ignore candidate)) *autolith-version*))
        (list 'channel-view--leave (lambda (candidate) (declare (ignore candidate)) nil))
        (list 'channel-subscription-close (lambda (candidate) (declare (ignore candidate)) nil))
        (list 'channel-subscription-send
              (lambda (candidate operation &rest arguments)
                (declare (ignore candidate))
                (when (eq operation ':input) (push arguments sent)))))
       (lambda ()
          (labels ((run (screen identifier events &key refuse-p replay modal-p initial-input)
                    "Run a fresh view with real Clinedi input and only its transport stubbed."
                    (setf view (channel-view-create configuration identifier screen)
                          initial nil seen-p nil attach-fails-p refuse-p
                          replay-input replay modal-at-attach-p modal-p
                          (scripted-terminal-events terminal) (copy-tree events))
                    (let ((failure (handler-case (progn (channel-view-run view :initial-input initial-input) nil)
                                     (world-terminal-error (condition) condition))))
                      (test-assert (eq (not (null failure)) (not (null (or refuse-p (member ':draft-loss events)))))
                                   "only deliberately injected transport failures signal from a view")
                      (values initial failure))))
           (unwind-protect
                (progn
                  (test-assert (equal (run screen "a" '((:paste "alpha") :left (:switch :session-id "b")))
                                      '("" 0))
                               "the first channel has no client draft")
                  (test-assert (equal (run screen "b" '((:paste "bravo") (:switch :session-id "a"))) '("" 0))
                               "a new channel does not inherit the source draft")
                  (test-assert (equal (run screen "a" '((:paste "X") (:switch :session-id "b"))) '("alpha" 4))
                               "returning restores actual unsent input and cursor before the first key")
                  (test-assert (equal (run screen "b" '(:submit (:switch :session-id "a"))) '("bravo" 5))
                               "each channel keeps its own independent input")
                  (test-assert (and (= 1 (length sent)) (equal (getf (first sent) :text) "bravo"))
                               "submitting the restored draft sends it exactly once")
                  (test-assert (equal (run screen "a" '((:switch :session-id "missing"))) '("alphXa" 5))
                               "edits after restoration belong to their original channel")
                  (multiple-value-bind (input failure) (run screen "a" nil :refuse-p t)
                    (declare (ignore input))
                    (test-assert failure "an unsuccessful reattachment is observed as an attach refusal"))
                  (multiple-value-bind (input failure) (run screen "missing" nil :refuse-p t)
                    (declare (ignore input))
                    (test-assert failure "an unsuccessful switch never creates a usable target view"))
                  (test-assert (equal (run screen "a" '((:paste "!") :draft-loss)) '("alphXa" 5))
                               "returning after a failed switch preserves the source draft")
                  (test-assert (equal (run screen "a" '((:switch :session-id "b"))) '("alphX!a" 6))
                               "reconnecting the same channel restores the draft captured on input failure")
                  (test-assert (equal (run screen "a" '((:switch :session-id "b")) :replay "shared input")
                                      '("alphX!a" 6))
                               "initial server replay cannot overwrite a returning client's own draft")
                  (test-assert (equal (first (run screen "a" '((:switch :session-id "b"))
                                                  :modal-p t :replay "server question"))
                                      "server question")
                               "a replayed modal prompt keeps its own composer while the draft is stashed")
                  (test-assert (equal (first (run screen "a" '((:switch :session-id "b")))) "alphX!a")
                               "switching during a modal prompt retains the user's draft, not the prompt text")
                  (test-assert (equal (run screen "b" '((:switch :session-id "new"))) '("" 0))
                               "submitted input is not resurrected on return")
                  (test-assert (equal (run screen "new" '((:switch :session-id "a"))) '("" 0))
                               "opening another fresh channel starts with an empty composer")
                 (dolist (text '("First request" "(+ 1 2)" "? (+ 1 2)"))
                   (let ((before (length sent)))
                     (test-assert
                      (and (equal (run screen "first" '(:detach)
                                       :initial-input (list :action ':submit :payload text)) '("" 0))
                           (= (length sent) (1+ before))
                           (equal (getf (first sent) :text) text)
                           (eq (getf (first sent) :action) ':submit))
                      "a welcome submission crosses the actual view transport once and clears its composer")))
                 (test-call-with-function-replacements
                  (list (list 'channel-subscription-send
                              (lambda (&rest arguments)
                                (declare (ignore arguments))
                                (error 'world-terminal-error :operation ':input :message "Injected send refusal."))))
                  (lambda ()
                    (test-assert
                     (equal (run screen "refused-first" '(:detach)
                                 :initial-input (list :action ':submit :payload "Recover this request"))
                            '("Recover this request" 20))
                     "a refused local send preserves the welcome submission as an editable channel draft")))
                 (let ((other (world-terminal-screen-create :terminal terminal)))
                   (unwind-protect
                        (test-assert (equal (run other "a" '(:detach)) '("" 0))
                                     "another terminal client cannot reuse this client's drafts")
                     (world-terminal-screen-close other))))
             (world-terminal-screen-close screen)))))))
  nil)

(-> test-channel-view-interleaved-history () null)
(defun test-channel-view-interleaved-history ()
  "Restore command results and notices by durable boundary, including older pages."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((identifier   (getf (rest (world-server-tests--open
                                       (world-client-entry configuration)
                                       (world-tests--workspace root "interleaved-history")))
                               :session-id))
            (application  (world-host-application (world-server--find-host server identifier)))
            (conversation (application-conversation application))
            (screen       (world-terminal-screen-create :terminal (make-instance 'scripted-terminal :columns 90)))
            (view         (channel-view-create configuration identifier screen))
            (markers      '("chronology-empty-channel" "chronology-earlier-input" "(permissions :full)"
                            "chronology-command-result" "chronology-later-input"
                            "chronology-relevant-memories" "chronology-shell-result" "chronology-assistant")))
       (labels ((ordered ()
                  "Check visible chronological positions without pinning presentation wording."
                  (let ((text (client-history-tests--text view)))
                    (loop for (earlier later) on markers while later
                          do (let ((left (search earlier text)) (right (search later text)))
                               (test-assert (and left right (< left right))
                                            "Local and durable output retain their relative positions."))))
                  nil)

                (reattach ()
                  "Return to the same channel with a new frontend and another pane width."
                  (channel-subscription-close view)
                  (setf view (channel-view-create configuration identifier screen))
                  (channel-subscription-attach view)
                  nil))
         (unwind-protect
              (progn
                (application-present application "chronology-empty-channel")
                (channel-subscription-attach view)
                (test-assert (search "chronology-empty-channel" (client-history-tests--text view))
                             "A channel with only local output replays its boundary-zero publication.")
                ;; Use the same persisted second for every durable event.
                (sb-ext:without-package-locks
                  (test-call-with-function-replacements
                   (list (list 'get-universal-time (constantly 4000000000)))
                   (lambda ()
                     (conversation-append-user-message conversation "chronology-earlier-input")
                     (application-render-records application)
                     (application-run-lisp-input application "(permissions :full)")
                     (application-present application "chronology-command-result")
                     (conversation-append-user-message conversation "chronology-later-input")
                     (application-render-records application)
                     (application-present application "chronology-relevant-memories")
                     (conversation-append-tool-result conversation "chronology-call" :tool-name "shell.run"
                                                     :output                                    "chronology-shell-result")
                     (conversation-append-provider-item conversation (agent-test-message "chronology-assistant"))
                     (application-render-records application))))
                (client-history-tests--settle view)
                (ordered)
                (channel-presentation-set-geometry (application-presentation application) :columns 72)
                (reattach)
                (ordered)
                (test-call-with-function-replacements
                 (list (list 'application--history-page-size (constantly 2)))
                 (lambda ()
                   (reattach)
                   (test-assert (not (search "(permissions :full)" (client-history-tests--text view)))
                                "A retained command older than the newest page waits for its own page.")
                   (let ((reply (channel-subscription-call
                                 view ':transcript-history
                                 :conversation    (channel-view-transcript-history-conversation view)
                                 :before-sequence (channel-view-transcript-history-floor view))))
                     (channel-view--apply-history-page view reply)
                     (channel-view--apply-history-page view reply)
                     (ordered)
                     (let ((text (client-history-tests--text view)))
                       (dolist (marker markers)
                         (test-assert (= 1 (test-substring-count marker text))
                                      "Overlapping history pages do not duplicate local or durable output."))))))
                (let ((cut (application-client-history--cut application)))
                  (multiple-value-bind (events fields)
                      (application-client-history-page application cut :character-limit 1)
                    (test-assert (and (null events) (= cut (getf fields :before-sequence))
                                      (getf fields :more-p))
                                 "An overflow notice that cannot fit preserves the no-progress cursor."))))
           (channel-subscription-close view)
           (world-terminal-screen-close screen))))))
  nil)
