(in-package #:autolith)

;;;; -- Localgroup Tests --

(-> test-localgroup--application (configuration &key (:presented-p boolean))
    (values application application-input-controller))
(defun test-localgroup--application (configuration &key presented-p)
  "Return a minimal APPLICATION and responsive controller for localgroup tests.

With PRESENTED-P it has no terminal of its own, as a detached session: its UI
is a presentation, which the clients of its endpoint render. Otherwise it
shows on a test terminal."
  (let* ((conversation (conversation-create configuration))
         (ui           (if presented-p
                           (application-terminal-ui-create
                            :client-pane   '(:rows 24 :columns 80)
                            :configuration configuration)
                           (terminal-ui-create
                            :terminal (make-instance 'recording-terminal :columns 80))))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (values application controller)))

(-> test-localgroup--read-packet (stream) list)
(defun test-localgroup--read-packet (stream)
  "Return one packet after a bounded wait for STREAM input."
  (test-assert
   (task-tests--wait-until (lambda () (listen stream)) 10)
   "the localgroup connection produces its next packet promptly")
  (or (daemon-read-packet stream)
      (error "The localgroup connection closed before its next packet.")))

(-> test-localgroup--present
    ((or localgroup-session cons)
     &key (:rows integer) (:columns integer) (:styled-p boolean) (:evaluates-p boolean)
          (:watches-p boolean) (:client-pid (option integer)))
    (values localgroup-client-connection function function))
(defun test-localgroup--present
    (endpoint &key (rows 31) (columns 91) (styled-p t) evaluates-p watches-p client-pid)
  "Attach a client that presents to the channel at ENDPOINT, a session or its endpoint entry.

The client states a pane of ROWS by COLUMNS and, when given, the process
CLIENT-PID, as a terminal client does. With WATCHES-P it only watches. It has
answered one command when this returns, so the channel has finished what it
does for a terminal that just attached.

Return its connection, a function returning the events it has seen, oldest
first, and a function that waits for a presentation event. That function
takes an operation and optionally a test of the event; it returns the first
(:ui :operation OPERATION ...) event the test accepts, or NIL when none came
in time. The caller closes the connection with LOCALGROUP-CLIENT-CLOSE."
  (let* ((lock       (make-lock "presenting test client"))
         (events     nil)
         (entry      (if (typep endpoint 'localgroup-session)
                         (cons (image-daemon:daemon-runtime-registry-pathname endpoint)
                               (image-daemon:daemon-runtime-record endpoint))
                         endpoint))
         (connection (localgroup-client-connect
                      entry
                      :name           "terminal"
                      :evaluates-p    evaluates-p
                      :arguments      (list :presents-p t
                                            :watches-p  watches-p
                                            :rows       rows
                                            :columns    columns
                                            :styled-p   styled-p
                                            :client-pid client-pid)
                      :event-function (lambda (event)
                                        (with-lock-held (lock)
                                          (push event events))))))
    (flet ((seen ()
             "Return the events seen so far, oldest first."
             (with-lock-held (lock)
               (reverse events))))
      ;; The channel reads this client's commands only after it settled the attachment.
      (localgroup-client-call connection ':state)
      (values connection
              #'seen
              (lambda (operation &optional (test (constantly t)))
                (let ((found nil))
                  (task-tests--wait-until
                   (lambda ()
                     (setf found
                           (find-if (lambda (event)
                                      (and (eq (first event) ':ui)
                                           (eq (getf (rest event) :operation) operation)
                                           (funcall test event)))
                                    (seen))))
                   20)
                  found))))))

(-> test-localgroup--await-served (localgroup-session &optional (integer 0)) boolean)
(defun test-localgroup--await-served (session &optional (remaining 0))
  "Wait until SESSION serves only REMAINING connections and return whether it came to that.

A connection's serving thread ends after what the session does for a client that left."
  (task-tests--wait-until
   (lambda ()
     (with-lock-held ((image-daemon:daemon-runtime-lock session))
       (= remaining (length (image-daemon:daemon-runtime-client-threads session)))))
   10))

(-> test-localgroup--status (localgroup-session) list)
(defun test-localgroup--status (session)
  "Return the fields of the status SESSION's endpoint answers a request with."
  (rest (getf (rest (daemon-call (image-daemon:daemon-runtime-port session)
                                 (image-daemon:daemon-runtime-token session) ':status))
              :status)))

(-> test-localgroup--event-text (list) string)
(defun test-localgroup--event-text (event)
  "Return the plain text transcript presentation EVENT shows, or the empty string for any other event.

An :APPEND shows its entry and a :RECORDS each record's entry on a line of its own."
  (flet ((entry-text (entry)
           "Return styled wire ENTRY without its styles."
           (if (stringp entry)
               entry
               (format nil "~{~A~}" (mapcar #'second entry)))))
    (let ((fields (rest event)))
      (case (and (eq (first event) ':ui) (getf fields :operation))
        (:append
         (entry-text (getf fields :entry)))
        (:records
         (format nil "~{~A~^~%~}"
                 (mapcar (lambda (presentation) (entry-text (second presentation)))
                         (getf fields :presentations))))
        (otherwise
         "")))))

(-> test-localgroup--presented-text (application) string)
(defun test-localgroup--presented-text (application)
  "Return the transcript text a client attaching to APPLICATION's hosted channel now would be replayed.

Each transcript event takes a line of its own."
  (let ((presentation (application-presentation application)))
    (format nil "~{~A~%~}"
            (remove "" (mapcar #'test-localgroup--event-text
                               (with-terminal-ui-locked (presentation)
                                 (channel-presentation-replay presentation)))
                    :test #'string=))))

(-> test-localgroup-stop-retry () null)
(defun test-localgroup-stop-retry ()
  "Attempt independent cleanup stages and retain endpoint ownership for retry."
  (dolist (failing-stage '(:runtime :registry))
    (let* ((application (make-instance 'application))
           (session     (make-instance 'localgroup-session :application application))
           (cause       (make-condition 'simple-error :format-control "cleanup failed"))
           (attempts    nil)
           (fail-p      t))
      (setf (application-localgroup-session application) session)
      (flet ((attempt (stage)
               (push stage attempts)
               (when (and fail-p (eq stage failing-stage))
                 (error cause))))
        (test-call-with-function-replacements
         (list (list 'image-daemon:daemon-runtime-stop
                     (lambda (runtime) (declare (ignore runtime)) (attempt ':runtime)))
               (list 'localgroup--delete-owned-registry
                     (lambda (runtime) (declare (ignore runtime)) (attempt ':registry))))
         (lambda ()
           (let ((failure (handler-case (localgroup-stop application)
                            (localgroup-stop-error (condition) condition))))
             (test-assert
              (and (typep failure 'localgroup-stop-error)
                   (eq session (application-localgroup-session application))
                    (equal (reverse attempts) '(:runtime :registry))
                   (eq cause (second (first (localgroup-stop-error-failures failure)))))
              "cleanup preserves the original stage failure, attempts every stage, and retains ownership"))
           (setf fail-p nil attempts nil)
           (localgroup-stop application)
           (test-assert
            (and (null (application-localgroup-session application))
                  (equal (reverse attempts) '(:runtime :registry)))
            "retry completes owned cleanup before releasing the application slot"))))))
  nil)

(-> test-localgroup-endpoint-failure-isolation () null)
(defun test-localgroup-endpoint-failure-isolation ()
  "Exercise the installed daemon failure hook, pending storage and hosted-channel isolation."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (failed-id
              (getf (rest (world-server-tests--open
                           entry (world-tests--workspace root "failed-endpoint"))) :session-id))
            (healthy-id
              (getf (rest (world-server-tests--open
                           entry (world-tests--workspace root "healthy-endpoint"))) :session-id))
            (host        (world-server--find-host server failed-id))
            (application (world-host-application host))
            (controller  (application-input-controller application))
            (session     (application-localgroup-session application))
            (healthy     (world-server-tests--session server healthy-id))
            (runtime     (world-server-runtime server))
            (failure     (make-condition 'simple-error :format-control "Injected channel accept failure.")))
       (with-lock-held ((image-daemon:daemon-runtime-lock session))
         (setf (localgroup-session-paused-p session) t))
       (test-assert (and (application-input-controller--enqueue controller ':message "accepted before failure")
                         (application-input-controller--enqueue controller ':command "help"))
                    "the paused channel accepts input before its endpoint fails")
       (interrupt-thread (image-daemon:daemon-runtime-server-thread session)
                         (lambda () (error failure)))
       (test-assert
        (task-tests--wait-until
         (lambda () (null (world-server--find-host server failed-id))) 10)
        "the installed endpoint hook ends its failed channel")
       (test-assert
        (and (eq (application-input-controller-failure controller) failure)
             (application-input-controller-failure-backtrace controller)
             (eq runtime (world-server-runtime server))
             (not (world-server-stopping-p server))
             (world-server-running-p configuration)
             (eq (first (localgroup-query-record
                         (cons (image-daemon:daemon-runtime-registry-pathname healthy)
                               (image-daemon:daemon-runtime-record healthy)) ':status)) ':ok))
        "failure keeps its original condition and frames while the other channel and owner answer")
       (let ((restored (make-instance 'application-input-controller
                                      :application application :main-thread (current-thread))))
         (application-input-controller--load-pending restored)
         (test-assert
          (equal (application-input-controller--state restored :work-items)
                 '((:message "accepted before failure") (:command "help")))
          "a fresh controller restores accepted pending input once and in order")))))
  nil)

(-> test-localgroup-message-dispatch () null)
(defun test-localgroup-message-dispatch ()
  "Test daemon messages reach a channel's inbox or its mission events, and nothing else is accepted."
  (with-test-configuration (configuration)
    (let ((application (make-instance 'application
                                      :configuration configuration
                                      :conversation  (conversation-create configuration :identifier "bself11"))))
      (flet ((rejected-p (envelope)
               "Return true when ENVELOPE is rejected as a daemon message."
               (handler-case (progn (localgroup--message-dispatch application envelope) nil)
                 (image-daemon:daemon-message-error ()
                   t))))
        (test-assert (equal (localgroup--message-dispatch application (list :action ':channel :id "m1"))
                            '(:ok :queued 0))
                     "a channel notice delivers the inbox, empty before the conversation persists")
        (test-assert (rejected-p (list :action ':event :id "e1" :receiver "bother11" :payload "tick"))
                     "a mission event for another conversation is rejected")
        (test-assert (rejected-p (list :action ':send :id "p1" :receiver "bself11" :payload "hello"))
                     "a message of any other kind is rejected"))))
  nil)

(-> test-localgroup-orphan-reconciliation () null)
(defun test-localgroup-orphan-reconciliation ()
  "Verify server and client startup reconcile their configured registry root."
  (with-test-configuration (configuration)
    (let ((application nil) (controller nil) (session nil) (directories nil))
      (unwind-protect
           (progn
             (multiple-value-setq (application controller)
               (test-localgroup--application configuration))
             (test-call-with-function-replacements
              (list (list 'image-daemon:daemon-registry-reconcile
                          (lambda (directory) (push directory directories) 0))
                    (list 'configuration-create
                          (lambda (&rest arguments) (declare (ignore arguments)) configuration)))
              (lambda ()
                (setf session (localgroup-start application))
                (test-assert (eq (localgroup--client-configuration) configuration)
                             "client startup uses its configured state")))
             (test-assert (and (= (length directories) 2)
                               (every (lambda (directory)
                                        (equal directory (localgroup-registry-directory configuration)))
                                      directories))
                          "both startup paths reconcile the configured discovery directory"))
        (when session (localgroup-stop application))
        (when controller (application-input-controller-stop controller))
        (when application (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup--show-rows
    (localgroup-screen list
     &key (:height integer) (:width integer) (:cursor-row integer) (:cursor-column integer)
          (:cursor-visible-p t) (:whole-screen-p t))
    null)
(defun test-localgroup--show-rows
    (screen rows
     &key height width (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p)
  "Show ROWS, a pane's display rows from its top, as SCREEN's frame of HEIGHT by WIDTH."
  (localgroup-screen-show-frame screen
                                (clinedi:make-frame-snapshot rows :height height :width width)
                                :cursor-row       cursor-row
                                :cursor-column    cursor-column
                                :cursor-visible-p cursor-visible-p
                                :whole-screen-p   whole-screen-p))

(-> test-localgroup-screen-paints-frames () null)
(defun test-localgroup-screen-paints-frames ()
  "Test a client painting its pane's frames on its own terminal."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 8))
         (screen   (localgroup-screen-create :terminal terminal)))
    (flet ((output ()
             "Return everything written to the client terminal so far."
             (apply #'concatenate 'string (reverse (recording-terminal-chunks terminal))))

           (show (&rest rows)
             "Show ROWS as the pane's frame."
             (test-localgroup--show-rows screen rows :height 8 :width 40
                                                     :cursor-row 3 :cursor-column 1)))
      (test-assert (and (not (localgroup-screen-active-p screen))
                        (zerop (length (output))))
                   "a screen without a frame leaves the terminal alone")
      (show "hello" "" "world")
      (test-assert (and (localgroup-screen-active-p screen)
                        (search (alternate-screen-enter-sequence) (output))
                        (search "hello" (output))
                        (search "world" (output)))
                   "the first frame takes the screen and paints its rows")
      (setf (recording-terminal-chunks terminal) nil)
      (show "bye" "" "world")
      (test-assert (and (search "bye" (output)) (not (search "world" (output))))
                   "a later frame repaints only the rows that changed")
      (setf (recording-terminal-chunks terminal) nil)
      (localgroup-screen-resized screen)
      (test-assert (and (search "bye" (output)) (search "world" (output)))
                   "a resize repaints the whole frame")
      (localgroup-screen-close screen)
      (test-assert (and (not (localgroup-screen-active-p screen))
                        (search (alternate-screen-leave-sequence) (output)))
                   "closing gives the screen back")
      (localgroup-screen-close screen)
      (test-assert (= 1 (count-if (lambda (chunk) (search (alternate-screen-leave-sequence) chunk))
                                  (recording-terminal-chunks terminal)))
                   "closing an inactive screen writes nothing")))
  (dolist (failure '(:enter :leave))
    (let* ((terminal       (make-instance 'recording-terminal :columns 40 :rows 8))
           (screen         (localgroup-screen-create :terminal terminal))
           (write-function (symbol-function 'terminal--write))
           (condition      (make-condition 'terminal-error :operation ':write
                                                          :message    "Injected screen transition failure."
                                                          :cause      nil))
           (restored       nil)
           (leave-attempts 0))
      (test-call-with-function-replacements
       (list
        (list 'platform-terminal-enable-fullscreen
              (lambda (platform) (declare (ignore platform)) ':screen-test-token))
        (list 'platform-terminal-restore-fullscreen
              (lambda (platform token) (declare (ignore platform)) (push token restored)))
        (list 'terminal--write
              (lambda (destination text)
                (when (and (eq failure ':leave)
                           (search (alternate-screen-leave-sequence) text)
                           (= 1 (incf leave-attempts)))
                  (error condition))
                (funcall write-function destination text)
                (when (and (eq failure ':enter)
                           (search (alternate-screen-enter-sequence) text))
                  (error condition)))))
       (lambda ()
         (let ((failed-p nil))
           (handler-case
               (progn
                 (test-localgroup--show-rows screen '("hello") :height 8 :width 40)
                 (when (eq failure ':leave)
                   (localgroup-screen-close screen)))
             (terminal-error (failure) (setf failed-p (eq failure condition))))
           (test-assert failed-p "The injected terminal failure reaches its caller"))
         (when (eq failure ':leave)
           (test-assert (and (localgroup-screen-active-p screen)
                             (null (localgroup-screen-token screen)))
                        "Failed physical leave retains retry ownership but consumes the native token"))
         (localgroup-client-screen-release screen)
         (let ((bytes (apply #'concatenate 'string (reverse (recording-terminal-chunks terminal)))))
           (test-assert (and (search (alternate-screen-enter-sequence) bytes)
                             (search (alternate-screen-leave-sequence) bytes)
                             (not (localgroup-screen-active-p screen)))
                        "Failed entry is compensated and failed leave is retried by client finalization"))
         (test-assert (= 1 (count ':screen-test-token restored))
                      "Native fullscreen state is restored once even when physical leave is retried")
         (when (eq failure ':leave)
           (test-assert (= 2 leave-attempts) "The finalizer retries the failed physical leave once"))))))
  nil)

(defclass localgroup-tests-panel ()
  ((rows
    :initarg       :rows
    :reader        localgroup-tests-panel-rows
    :type          list
    :documentation "The fixed rows the test panel shows."))
  (:documentation "A side panel of fixed rows for client screen tests."))

(defmethod screen-panel-width ((panel localgroup-tests-panel) columns)
  "Ten columns, however wide the terminal."
  (declare (ignore columns))
  10)

(defmethod screen-panel-click ((panel localgroup-tests-panel) column row)
  "Ask to switch from the first row, redraw from the second, and ignore the rest."
  (declare (ignore column))
  (case row
    (1
     (list ':switch :session-id "bnext1"))
    (2
     ':redraw)
    (otherwise
     nil)))

(defmethod screen-panel-minimum-columns ((panel localgroup-tests-panel))
  "Sixty columns."
  60)

(defmethod screen-panel-refresh ((panel localgroup-tests-panel))
  "Nothing changes."
  nil)

(defmethod screen-panel-rows ((panel localgroup-tests-panel) terminal &key height width focused-p)
  "Return HEIGHT rows padded to WIDTH."
  (declare (ignore terminal focused-p))
  (loop for index below height
        collect (format nil "~vA" width (or (nth index (localgroup-tests-panel-rows panel)) ""))))

(-> test-localgroup-screen-loading () null)
(defun test-localgroup-screen-loading ()
  "Paint pending navigation in the channel pane and require the next channel's full frame."
  (dolist (columns '(90 40))
    (let* ((terminal (make-instance 'recording-terminal :columns columns :rows 8 :styled-p t))
           (panel    (make-instance 'localgroup-tests-panel :rows '("sidebar")))
           (screen   (localgroup-screen-create :terminal terminal :panel panel))
           (text     "Loading channel")
           (name     "Ada 雪"))
      (unwind-protect
           (progn
             (test-localgroup--show-rows screen '("previous" "" "" "old row")
                                         :height 8 :width columns :whole-screen-p t)
             (setf (recording-terminal-chunks terminal) nil)
             (localgroup-screen-show-loading screen text :agent-name name)
             (test-assert (and (localgroup-screen-active-p screen)
                               (null (localgroup-screen-frame screen))
                               (not (third (localgroup-screen-cursor screen))))
                          "loading keeps terminal ownership but drops the old frame and cursor")
             (test-assert (eq (localgroup-screen-painted-layout screen)
                              (if (> columns 70) ':beside ':session))
                          "loading reveals the docked sidebar or the narrow channel pane")
             (test-assert (some (lambda (chunk) (search text chunk))
                                (recording-terminal-chunks terminal))
                          "the pending operation is painted before a channel can answer")
             (localgroup-screen-show-loading screen text :step 4 :description "initializing"
                                                        :agent-name name)
             (let* ((rows    (clinedi:frame-snapshot-rows (localgroup-screen--loading-frame screen)))
                    (heading (find-if (lambda (row) (search text row)) rows)))
               (test-assert (some (lambda (row) (find #\━ row)) rows)
                            "completed milestones fill the loading bar")
               (test-assert (and heading
                                 (search (concatenate 'string text " " name)
                                         (clinedi:ansi-strip heading))
                                 (search (concatenate 'string (terminal-style-sequence ':brand) text)
                                         heading)
                                 (search (concatenate 'string (terminal-style-sequence ':agent-name) name)
                                         heading)
                                 (not (equal (terminal-style-sequence ':brand)
                                             (terminal-style-sequence ':agent-name))))
                            "the loading heading uses a distinct color for the destination name"))
             (setf (terminal-columns terminal) 24
                   (recording-terminal-chunks terminal) nil)
             (localgroup-screen-resized screen)
             (test-assert (and (recording-terminal-chunks terminal)
                               (null (localgroup-screen-frame screen))
                               (= 24 (clinedi:frame-snapshot-width
                                      (localgroup-screen--loading-frame screen))))
                          "resizing repaints loading without establishing a channel frame")
             (multiple-value-bind (height width) (localgroup-screen-pane-size screen)
               (test-localgroup--show-rows screen '("ready") :height height :width width)
               (test-assert (equalp (clinedi:frame-snapshot-rows (localgroup-screen-frame screen))
                                    #("ready" "" "" "" "" "" "" ""))
                            "the new channel replaces loading without rows from the previous channel")
               (test-assert (null (localgroup-screen-loading screen))
                            "the destination's frame ends loading")))
        (localgroup-screen-close screen))))
  (let* ((terminal (make-instance 'recording-terminal))
         (screen   (localgroup-screen-create :terminal terminal)))
    (localgroup-screen-show-loading screen "pending navigation")
    (test-assert (null (recording-terminal-chunks terminal))
                 "loading paints only a screen the client already owns")
    (unwind-protect
         (progn
           (localgroup-screen-show-loading screen "startup" :start-p t)
           (test-assert (and (localgroup-screen-active-p screen)
                             (localgroup-screen-loading screen))
                        "initial fullscreen loading can explicitly take the terminal"))
      (localgroup-screen-close screen)))
  nil)

(-> test-localgroup-screen-panel () null)
(defun test-localgroup-screen-panel ()
  "Test a client composing its side panel beside the session's frame and keeping the keys apart."
  (let* ((terminal (make-instance 'recording-terminal :columns 80 :rows 6))
         (panel    (make-instance 'localgroup-tests-panel :rows '("PANEL-A" "PANEL-B")))
         (screen   (localgroup-screen-create :terminal terminal :panel panel)))
    (flet ((output ()
             "Return everything written to the client terminal so far."
             (apply #'concatenate 'string (reverse (recording-terminal-chunks terminal))))
           (pane ()
             "Return the session's pane size as a list."
             (multiple-value-list (localgroup-screen-pane-size screen))))
      (test-assert (equal (pane) '(6 80))
                   "a streamed attachment initially receives the whole terminal width")
      (test-assert (and (equal (localgroup-screen-filter-event screen '(:control #\s)) '(:control #\s))
                        (equal (localgroup-screen-filter-event screen '(:click 20 2)) '(:click 20 2)))
                   "streamed input bypasses the fullscreen panel")
      (localgroup-screen-toggle-panel screen)
      (test-assert (and (localgroup-screen-panel-shown-p screen)
                        (not (localgroup-screen-drawer-open-p screen))
                        (not (localgroup-screen-take-resize screen))
                        (equal (pane) '(6 80)))
                   "a streamed toggle changes neither panel preference nor prompt geometry")
      (test-localgroup--show-rows screen '("initial fullscreen") :height 6 :width 80)
      (test-assert (and (localgroup-screen-active-p screen)
                        (equal (pane) '(6 69))
                        (localgroup-screen-take-resize screen))
                   "entering fullscreen reserves the sidebar and requests matching session dimensions")
      (test-localgroup--show-rows screen '("host row" "second")
                                  :height 6 :width 69 :cursor-row 1 :cursor-column 2)
      (let ((painted (output)))
        (test-assert (and (search "PANEL-A   │host row" painted)
                          (search "PANEL-B   │second" painted))
                     "frames are painted beside the panel rows with a rule between"))
      (test-assert (equal (localgroup-screen-filter-event screen '(:click 5 1))
                          '(:switch :session-id "bnext1"))
                   "a click the panel answers with a switch hands the request on")
      (let ((before (length (output))))
        (test-assert (and (eq (localgroup-screen-filter-event screen '(:click 5 2)) ':ignore)
                          (> (length (output)) before))
                     "a click that changes the panel repaints it and reaches no session"))
      (test-assert (eq (localgroup-screen-filter-event screen '(:click 5 3)) ':ignore)
                   "a click the panel ignores reaches no session")
      (test-assert (equal (localgroup-screen-filter-event screen '(:click 20 2)) '(:click 9 2))
                   "a click in the session's pane is moved to its columns")
      (test-assert (and (equal (localgroup-screen-filter-event screen '(:drag 20 3)) '(:drag 9 3))
                        (equal (localgroup-screen-filter-event screen '(:drag 5 3)) '(:drag 1 3))
                        (equal (localgroup-screen-filter-event screen '(:release 4 3)) '(:release 1 3)))
                   "a drag goes to the pane, and over the panel stays on the pane's edge")
      (test-assert (eq (localgroup-screen-filter-event screen ':page-up) ':page-up)
                   "other events pass through")
      (test-assert (and (eq (localgroup-screen-filter-event screen ':focus-next) ':ignore)
                        (localgroup-screen-panel-focused-p screen)
                        (not (localgroup-screen-take-resize screen))
                        (eq (localgroup-screen-filter-event screen ':page-up) ':ignore)
                        (eq (localgroup-screen-filter-event screen ':interrupt) ':interrupt))
                   "the focus key moves the keyboard to the panel, which keeps every key but an interrupt")
      (test-assert (and (eq (localgroup-screen-filter-event screen ':focus-next) ':ignore)
                        (not (localgroup-screen-panel-focused-p screen))
                        (eq (localgroup-screen-filter-event screen ':page-up) ':page-up))
                   "the focus key gives the keyboard back to the session")
      (localgroup-screen-filter-event screen ':focus-next)
      (test-assert (and (eq (localgroup-screen-filter-event screen '(:control #\s)) ':ignore)
                        (localgroup-screen-take-resize screen)
                        (not (localgroup-screen-take-resize screen))
                        (not (localgroup-screen-panel-focused-p screen))
                        (equal (pane) '(6 80)))
                   "the toggle key hides the panel, focused or not, and notes the pane's new size once")
      (test-assert (and (eq (localgroup-screen-filter-event screen ':focus-next) ':ignore)
                        (localgroup-screen-panel-focused-p screen)
                        (equal (pane) '(6 69))
                        (eq (localgroup-screen-filter-event screen '(:control #\s)) ':ignore))
                   "the focus key shows a hidden panel and moves to it")
      (localgroup-screen-take-resize screen)
      (test-localgroup--show-rows screen '("wide channel")
                                  :height 6 :width 80 :cursor-row 1 :cursor-column 2)
      (test-assert (equal (localgroup-screen-filter-event screen '(:click 5 2)) '(:click 5 2))
                   "with the panel hidden clicks keep their columns")
      (localgroup-screen-filter-event screen '(:control #\s))
      (localgroup-screen-take-resize screen)
      (test-localgroup--show-rows screen '("boot screen")
                                  :height 6 :width 69 :cursor-visible-p nil :whole-screen-p t)
      (test-assert (and (equal (pane) '(6 80))
                        (localgroup-screen-take-resize screen))
                   "a frame asking for the whole screen hides the panel and resizes the pane")
      (test-localgroup--show-rows screen '("listener") :height 6 :width 80)
      (test-assert (and (equal (pane) '(6 69))
                        (localgroup-screen-take-resize screen))
                   "the next ordinary frame brings the panel back")
      (test-localgroup--show-rows screen '("docked channel") :height 6 :width 69)
      (localgroup-screen-close screen)
      (test-assert (and (equal (pane) '(6 80))
                        (localgroup-screen-take-resize screen)
                        (not (localgroup-screen-take-resize screen))
                        (equal (localgroup-screen-filter-event screen '(:control #\s)) '(:control #\s))
                        (equal (localgroup-screen-filter-event screen '(:click 20 2)) '(:click 20 2)))
                   "leaving fullscreen restores full-width streamed input despite the cached painted layout")
      (test-localgroup--show-rows screen '("fullscreen again") :height 6 :width 80)
      (test-assert (and (equal (pane) '(6 69)) (localgroup-screen-take-resize screen))
                   "a later fullscreen attachment restores the saved sidebar preference")
      (terminal-set-dimensions terminal 50 :rows 6)
      (test-assert (and (equal (pane) '(6 50))
                        (eq (localgroup-screen-layout screen) ':session))
                   "a narrow terminal keeps the channel visible until the drawer is explicitly opened")
      (localgroup-screen-close screen)))
  nil)

(-> test-localgroup-screen-geometry () null)
(defun test-localgroup-screen-geometry ()
  "Test frame, layout and cursor installation across boot hints, toggles and terminal resizes."
  (with-test-configuration (configuration)
    (let* ((terminal (make-instance 'recording-terminal :columns 120 :rows 8))
           (sidebar  (world-sidebar-create configuration))
           (screen   (localgroup-screen-create :terminal terminal :panel sidebar)))
      (labels ((send-frame (width text &key whole-p (height 8))
                 "Show a frame with an explicit geometry and whole-screen hint."
                 (test-localgroup--show-rows screen (list text)
                                             :height           height
                                             :width            width
                                             :cursor-row       1
                                             :cursor-column    2
                                             :cursor-visible-p (not whole-p)
                                             :whole-screen-p   whole-p))

               (output ()
                 "Return the physical terminal output since its last reset."
                 (apply #'concatenate 'string (reverse (recording-terminal-chunks terminal))))

               (reset-output ()
                 "Start a fresh physical-output observation."
                 (setf (recording-terminal-chunks terminal) nil)))
        (test-assert (equal (multiple-value-list (localgroup-screen-pane-size screen)) '(8 120))
                     "a client reserves no sidebar columns before fullscreen entry")
        (send-frame 93 "undersized boot" :whole-p t)
        (test-assert (and (zerop (length (output)))
                          (not (localgroup-screen-active-p screen))
                          (not (localgroup-screen-take-resize screen)))
                     "an undersized whole-screen frame cannot paint or change the initial full-width geometry")
        (send-frame 120 "matching boot" :whole-p t)
        (test-assert (and (search "matching boot" (output))
                          (not (search "Channels" (output))))
                     "the matching whole-screen boot frame is painted without a sidebar")
        (reset-output)
        (send-frame 120 "oversized channel")
        (test-assert (and (zerop (length (output))) (localgroup-screen-take-resize screen))
                     "boot completion requests pane geometry without shifting an oversized channel frame")
        (send-frame 93 "matching channel")
        (test-assert (and (search "│matching channel" (output))
                          (search (format nil "~C[2;30H" #\Escape) (output)))
                     "the pane frame, sidebar and offset composer cursor arrive together")
        (reset-output)
        (localgroup-screen-toggle-panel screen)
        (test-assert (and (zerop (length (output)))
                          (localgroup-screen-take-resize screen)
                          (equal (localgroup-screen-filter-event screen '(:click 50 2)) '(:click 23 2)))
                     "a pending toggle preserves the painted layout and its pointer coordinates")
        (send-frame 120 "full-width channel")
        (test-assert (and (search "full-width channel" (output))
                          (not (search "│" (output)))
                          (search (format nil "~C[2;3H" #\Escape) (output))
                          (equal (localgroup-screen-filter-event screen '(:click 50 2)) '(:click 50 2)))
                     "a matching full-width frame installs its unshifted cursor and pointer coordinates")
        (reset-output)
        (terminal-set-dimensions terminal 140 :rows 9)
        (localgroup-screen-resized screen)
        (test-assert (zerop (length (output)))
                     "terminal resizing never repaints a cached frame at a different geometry")
        (send-frame 120 "late old-size frame")
        (test-assert (zerop (length (output)))
                     "late frames from before the resize are not painted")
        (send-frame 140 "resized channel" :height 9)
        (test-assert (search "resized channel" (output))
                     "the matching resized frame is painted immediately")
        (localgroup-screen-close screen))))
  nil)

(-> test-localgroup-picker-waits-for-relayed-input () null)
(defun test-localgroup-picker-waits-for-relayed-input ()
  "Test a modal picker blocks until the client that controls its channel sends a fresh event."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let ((ui            (application-ui application))
            (result        ':pending)
            (connection    nil)
            (picker-thread nil))
        (unwind-protect
             (progn
               (configuration-ensure-directories configuration)
               (terminal-ui-start ui)
               (multiple-value-bind (client seen await)
                   (test-localgroup--present (localgroup-start application))
                 (declare (ignore seen))
                 (setf connection    client
                       picker-thread (make-thread
                                      (lambda ()
                                        (setf result
                                              (terminal-ui-select
                                               ui
                                               :title "pick one"
                                               :items '((:name        "default"
                                                         :argument    nil
                                                         :description "the default choice")))))
                                      :name "Autolith relayed picker test"))
                 (test-assert (funcall await ':modal (lambda (event) (getf (rest event) :open-p)))
                              "the picker opens on its client before receiving input")
                 (test-assert (and (not (task-tests--wait-until
                                         (lambda () (not (eq result ':pending)))
                                         (* 2 *presentation-terminal-poll-seconds*)))
                                   (thread-alive-p picker-thread))
                              "an attached client that sends nothing neither submits nor cancels the picker")
                 (test-assert (getf (localgroup-client-call connection ':event :event ':submit) :accepted-p)
                              "the open picker takes its client's key")
                 (test-assert (task-tests--wait-until (lambda () (not (eq result ':pending))) 10)
                              "a fresh submit event from the client completes the picker")
                 (test-assert (equal result "default")
                              "the fresh submit accepts the selected item")))
          (when connection (localgroup-client-close connection))
          (when picker-thread (join-thread picker-thread))
          (localgroup-stop application)
          (application-input-controller-stop controller)
          (terminal-ui-stop ui)))))
  nil)

(-> test-localgroup-remote-detach-never-pauses-reader () null)
(defun test-localgroup-remote-detach-never-pauses-reader ()
  "Test a remote detach tells the controlling terminal client to leave without joining the input reader."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
        (flet ((refusal (session)
                 "Return the message a detach of SESSION is refused with, or NIL when it is taken."
                 (handler-case (progn (localgroup--detach-terminal session) nil)
                   (localgroup-error (condition)
                     (autolith-error-message condition)))))
          (unwind-protect
               (progn
                 (configuration-ensure-directories configuration)
                 (terminal-ui-start (application-ui application))
                 (let* ((session    (localgroup-start application))
                        (identifier (image-daemon:daemon-runtime-identifier session))
                        (entry      (cons (image-daemon:daemon-runtime-registry-pathname session)
                                          (localgroup--registry-record session))))
                   (test-assert (search "No terminal shows" (refusal session))
                                "a channel nobody shows has no terminal to detach")
                   (let ((watcher (test-localgroup--present session :watches-p t)))
                     (unwind-protect
                          (test-assert (search "No terminal shows" (refusal session))
                                       "a client that only watches is no terminal to detach")
                       (localgroup-client-close watcher)))
                   (world-server-tests--call-with-terminal-client
                    configuration identifier
                    (lambda (type await-exit view)
                      (declare (ignore type view))
                      (test-call-with-function-replacements
                       (list
                        (list 'application-input-controller-call-with-reader-paused
                              (lambda (&rest arguments)
                                (declare (ignore arguments))
                                (error "Remote detach tried to pause the responsive reader."))))
                       (lambda ()
                         (let ((result (localgroup-query-record entry ':detach)))
                           (test-assert (and (eq (first result) ':ok)
                                             (not (getf (rest result) :scheduled-p)))
                                        "a remote detach is answered at once"))))
                      (test-assert (multiple-value-bind (exit ended-p) (funcall await-exit)
                                     (and ended-p (null exit)))
                                   "the controlling terminal client leaves as after its own detach")))
                   (test-assert (test-localgroup--await-served session)
                                "the session finishes serving the client that left")
                   (test-assert (and (not (application-terminal-attached-p application))
                                     (null (application-input-controller-exit-reason controller)))
                                "a session whose terminal was told to leave keeps running without it")))
            (localgroup-stop application)
            (application-input-controller-stop controller)
            (terminal-ui-stop (application-ui application))))))
  nil)

(-> test-localgroup-session-identifiers () null)
(defun test-localgroup-session-identifiers ()
  "Test canonical timestamp-bearing IDs and retained legacy discovery behavior."
  (let* ((timestamp (encode-universal-time 5 4 3 2 1 2025 0))
         (canonical (identifier-from-seed timestamp 0)))
    (let ((configuration (test-configuration)))
      (unwind-protect
          (progn
           (configuration-ensure-directories configuration)
           (let ((pathname (localgroup-registry-pathname configuration "abcdef012345")))
             (snapshot-write pathname
                             (list :localgroup-endpoint :version
                                   image-daemon:*daemon-registry-version* :session-id
                                   "abcdef012345" :pid 1 :address "127.0.0.1" :port 1
                                   :token "legacy-token" :created-at timestamp))
             (test-assert
              (string=
               (localgroup--record-session-id
                (rest (localgroup--find-record configuration "ABCDEF012345")))
               "abcdef012345")
              "legacy hexadecimal identifiers remain discoverable through normalized input")))
        (test-assert
         (localgroup-handoff--record-p
          (list :localgroup-handoff :version *localgroup-handoff-version* :session-id
                "abcdef012345" :token "legacy-token" :created-at timestamp :mode ':detach
                :state ':pending :fresh-conversation-p nil :old-pid 1 :replacement-pid
                nil :conversation-id canonical :draft ""))
         "legacy session identifiers remain valid in detached handoff records")
        (uiop/filesystem:delete-directory-tree (test-configuration-root configuration)
                                               :validate t :if-does-not-exist
                                               ':ignore))))
  nil)

(-> test-localgroup-cli-targets () null)
(defun test-localgroup-cli-targets ()
  "Exercise top-level operations, optional targets, discovery, and CLI validation."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (configuration-ensure-directories configuration)
    (let ((calls             nil)
          (probes            nil)
          (live-ports        nil)
          (*standard-output* (make-string-output-stream)))
      (labels ((publish (session-id port)
                 "Publish a private endpoint using PORT as its test identity."
                 (snapshot-write
                  (localgroup-registry-pathname configuration session-id)
                  (list :localgroup-endpoint
                        :version image-daemon:*daemon-registry-version*
                        :session-id session-id :pid 1 :address "127.0.0.1"
                        :port port :token "cli-test-token"
                        :created-at (get-universal-time))))

               (invoke (arguments)
                 "Dispatch ARGUMENTS through the actual top-level CLI tree."
                 (setf calls nil probes nil)
                 (let ((command (parse-command-line (main--top-level-command) arguments)))
                   (funcall (command-handler command) command))
                 (reverse calls))

               (reject (arguments operation candidates)
                 "Require structured failure and the applicable discovery candidates."
                 (let ((condition
                         (handler-case (progn (invoke arguments) nil)
                           (localgroup-error (condition) condition))))
                   (test-assert (and condition
                                     (eq (daemon-error-operation condition) operation)
                                     (null calls))
                                "invalid or ambiguous commands do not perform operations")
                   (when condition
                     (test-assert
                      (every (lambda (candidate)
                               (search (session-identifier-display candidate)
                                       (autolith-error-message condition)))
                             candidates)
                      "discovery failures identify the reachable candidate sessions"))
                   (when (eq operation ':arguments)
                     (test-assert (null probes)
                                  "argument errors fail before discovering sessions"))))

               (check-targets (selector)
                 "Exercise every targeted command with an omitted or explicit selector."
                 (dolist (case '(("pause" :pause) ("detach" :detach)))
                   (destructuring-bind (name operation) case
                     (test-assert
                      (equal (invoke (cons name (when selector (list selector))))
                             (list (list 1 operation nil)))
                      "top-level operations and optional selectors route each request")))))
        (test-call-with-function-replacements
         (list
          (list 'localgroup--client-configuration (lambda () configuration))
          ;; Routing tests model already exited fixture processes.
          (list 'ls-compat.posix:process-state
                (lambda (pid) (declare (ignore pid)) ':dead))
          (list 'daemon-call
                (lambda (port token operation &key arguments)
                  (declare (ignore token))
                  (when (eq operation ':status) (push port probes))
                  (unless (member port live-ports)
                    (error "Test endpoint is unreachable."))
                  (unless (eq operation ':status)
                    (push (list port operation arguments) calls))
                  (list :ok         :operation operation
                        :session-id (nth (1- port) '("b6uouHJ" "c6uouHJ" "d6uouHJ")))))
          (list 'localgroup--run-status
                (lambda (&key sexp-p) (push (list :status sexp-p) calls) nil)))
         (lambda ()
           (test-assert (equal (invoke '("status")) '((:status nil)))
                        "status dispatches directly from the root")
           (test-assert (equal (invoke '("status" "--sexp")) '((:status t)))
                        "top-level status accepts its output option")
           (dolist (arguments '(("tell") ("tell" "a" "b" "c")
                                ("pause" "a" "b") ("detach" "a" "b")
                                ("status" "extra")
                                ("start" "extra") ("stop" "extra")))
             (reject arguments ':arguments nil))
           (dolist (arguments '(("localgroup" "status") ("lg" "status")
                                ("kill") ("kill" "b6uouHJ")
                                ("attach") ("unknown")))
             (test-assert
              (handler-case (progn (invoke arguments) nil)
                (configuration-error () (and (null calls) (null probes))))
              "unknown top-level commands fail before contacting a server"))
           ;; No registered endpoints, then only a stale endpoint.
           (dolist (arguments '(("pause") ("detach")))
             (reject arguments ':discover nil))
           (publish "d6uouHJ" 3)
           (reject '("pause") ':discover nil)
           (test-assert (not (probe-file (localgroup-registry-pathname configuration "d6uouHJ")))
                        "implicit discovery prunes stale endpoints")
           ;; A stale endpoint does not make a sole live endpoint ambiguous.
           (publish "d6uouHJ" 3)
           (publish "b6uouHJ" 1)
           (setf live-ports '(1))
           (world-tests--persisted-conversation configuration "b6uouHJ" 1000)
           (world-tests--persisted-conversation configuration "c6uouHJ" 2000)
           (world-tests--persisted-conversation configuration "e6uouHJ" 3000)
           (let ((inbox (conversation-create configuration
                                             :identifier "h777777" :inbox-p t)))
             (conversation-append-record inbox
                                         (list :title  :value *world-inbox-designator*
                                               :source ':initial)))
           (world-participant-rename configuration "b6uouHJ" "builder")
           (world-participant-rename configuration "e6uouHJ" "cold-agent")
           (check-targets nil)
           (dolist (selector '("b6uouHJ" "b-6uouHJ" "b-6" "builder" "BUILDER"
                               "@builder" "@BUILDER"))
             (check-targets selector))
           (test-assert (not (probe-file (localgroup-registry-pathname configuration "d6uouHJ")))
                        "a stale endpoint is removed while selecting the sole live endpoint")
           ;; Multiple reachable endpoints require a selector for every operation.
           (publish "c6uouHJ" 2)
           (setf live-ports '(1 2))
           (dolist (arguments '(("pause") ("detach")))
             (reject arguments ':discover '("b6uouHJ" "c6uouHJ")))
           (check-targets "b")
           (reject '("pause" "6uouHJ") ':discover '("b6uouHJ" "c6uouHJ" "e6uouHJ"))
           (dolist (selector '("cold-agent" "@cold-agent" "@nobody" "inbox"))
             (reject (list "pause" selector) ':discover nil))
           ;; Explicit unavailable targets fail rather than falling back to a live one.
           (publish "d6uouHJ" 3)
           (reject '("pause" "d") ':pause '("d6uouHJ"))
           (test-assert (not (probe-file (localgroup-registry-pathname configuration "d6uouHJ")))
                        "explicit operations prune an unreachable target")
           (reject '("pause" "z") ':discover '("z")))))))
  nil)

(-> test-localgroup-session-prefixes () null)
(defun test-localgroup-session-prefixes ()
  "Test CLI session selection by unique canonical, displayed, and legacy prefixes."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (configuration-ensure-directories configuration)
    (let ((calls             nil)
          (*standard-output* (make-string-output-stream)))
      (labels ((publish (session-id)
                 "Publish a private discovery record for SESSION-ID."
                 (snapshot-write
                  (localgroup-registry-pathname configuration session-id)
                  (list :localgroup-endpoint
                        :version image-daemon:*daemon-registry-version*
                        :session-id session-id :pid 1 :address "127.0.0.1"
                        :port 1 :token "prefix-test-token"
                        :created-at (get-universal-time))))

               (invoke (arguments)
                 "Run one CLI command and return its single endpoint call."
                 (setf calls nil)
                 (let ((command (parse-command-line (main--top-level-command)
                                                    arguments)))
                   (funcall (command-handler command) command))
                 (test-assert (= (length calls) 1)
                              "a selected session receives exactly one operation")
                 (first calls))

               (select (selector session-id)
                 "Verify SELECTOR routes a pause request to SESSION-ID."
                 (test-assert
                  (equal (invoke (list "pause" selector))
                         (list session-id ':pause nil))
                  "a unique selector routes the request to its session"))

               (reject (selector candidates)
                 "Verify SELECTOR fails before an endpoint call, naming CANDIDATES."
                 (let ((condition
                         (handler-case
                             (progn
                               (invoke (list "pause" selector))
                               nil)
                           (localgroup-error (condition)
                             condition))))
                   (test-assert
                    (and condition
                         (eq (daemon-error-operation condition) ':discover)
                         (string= (daemon-error-session-id condition) selector)
                         (null calls)
                         (every (lambda (session-id)
                                  (search (session-identifier-display session-id)
                                          (autolith-error-message condition)))
                                candidates))
                    "a failed selector reports discovery details without an endpoint call"))))
        (test-call-with-function-replacements
         (list
          (list 'localgroup--client-configuration (lambda () configuration))
          ;; Prefix routing uses fixture records whose processes have exited.
          (list 'ls-compat.posix:process-state
                (lambda (pid) (declare (ignore pid)) ':dead))
          (list 'localgroup-query-record
                (lambda (entry operation &optional arguments)
                  (let ((session-id (localgroup--record-session-id (rest entry))))
                    (push (list session-id operation arguments) calls)
                    (list :ok :operation operation :session-id session-id)))))
         (lambda ()
           (reject "b" nil)
           (publish "b6uouHJ")
           (dolist (selector '("b" "B" "b-" "b6" "b-6" "b6uou" "b-6uou" "uou" "UOUHJ"
                               "b6uouHJ" "b-6uouHJ"))
             (select selector "b6uouHJ"))
           (dolist (selector '("" "z" "b--6" "b6uouHJx" "-"))
             (reject selector nil))
           (dolist (session-id '("b6uouHK" "c6uouHJ" "abcdef1"
                                 "abcdef123456" "abcdef234567"))
             (publish session-id))
           (dolist (selector '("b" "b-" "b6" "b-6" "b6uouH" "b-6uouH"))
             (reject selector '("b6uouHJ" "b6uouHK")))
           (dolist (selector '("a" "abc" "ABC" "a-b"))
             (reject selector '("abcdef1" "abcdef123456" "abcdef234567")))
           (dolist (case '(("b6uouHJ" "b6uouHJ")
                           ("b-6uouHJ" "b6uouHJ")
                           ("abcdef1" "abcdef1")
                           ("a-bcdef1" "abcdef1")
                           ("abcdef12" "abcdef123456")
                           ("DEF12" "abcdef123456")
                           ("ABCDEF12" "abcdef123456")
                           ("ABCDEF123456" "abcdef123456")))
             (select (first case) (second case)))
           (reject "c" '("c6uouHJ" "abcdef1" "abcdef123456" "abcdef234567"))
           (dolist (case '((("pause" "c-6") ("c6uouHJ" :pause nil))
                           (("detach" "C6") ("c6uouHJ" :detach nil))))
             (test-assert (equal (invoke (first case)) (second case))
                          "each CLI operation resolves its session fragment")))))))
  nil)

(-> test-localgroup-protocol () null)
(defun test-localgroup-protocol ()
  "Test bounded safe packets, private discovery, status, and control routing."
  (test-localgroup-session-identifiers)
  (let* ((timestamp     (encode-universal-time 5 4 3 2 1 2025 0))
         (status
          (list :localgroup-status :session-id (identifier-from-seed timestamp 0) :pid
                41234 :terminal-client-pid 51234 :state ':idle :created-at timestamp :conversation-display-id
                "n-ew1234" :queued-input-count 0 :steering-input-count 0 :task-live-count
                0 :cwd "/tmp/example"))
         (titled-status (append status (list :conversation-title "Named local session")))
         (plain-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status titled-status) :stream stream :styled-p nil
                                       :columns 100)))
         (styled-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status) :stream stream :styled-p t :columns
                                       100)))
         (title-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list titled-status) :stream stream :styled-p nil
                                       :columns 80)))
         (fallback-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status) :stream stream :styled-p nil
                                       :columns 52)))
         (narrow-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list titled-status status) :stream stream :styled-p nil
                                       :columns 24))))
    (test-assert
     (and (search (identifier-display (getf (rest status) :session-id)) plain-output)
          (search "51234" plain-output) (search "/tmp/example" plain-output)
          (not (search (string #\Esc) plain-output)))
     "localgroup status renders session fields without ANSI controls")
    (test-assert
     (and (search "Named local session" title-output)
          (search "Named local session" narrow-output)
          (search "n-ew1234" fallback-output)
          (eq (localgroup--status-field-style titled-status ':conversation) ':plain)
          (eq (localgroup--status-field-style status ':conversation) ':code))
     "localgroup status shows titles at every width and falls back to coded IDs")
    (labels ((separator-positions (line separator)
               "Return the character offsets of SEPARATOR within LINE."
               (loop for character across line
                     for index from 0
                     when (char= character separator)
                       collect index)))
      (dolist (case (list (cons 100 plain-output) (cons 80 title-output)
                         (cons 52 fallback-output)))
        (let* ((columns   (first case))
               (output    (rest case))
               (lines     (remove "" (uiop/utility:split-string output :separator '(#\Newline))
                                  :test #'string=))
               (table     (rest lines))
               (positions (separator-positions (first table) #\Box_Drawings_Light_Vertical)))
          (test-assert
           (and positions
                (find #\Box_Drawings_Light_Vertical_And_Horizontal (second table))
                (find #\Box_Drawings_Light_Vertical (first (last table)))
                (notany (lambda (character) (find character "┌┐└┘├┤┬┴")) output)
                (every
                 (lambda (line)
                   (and (= (text-cell-width line) columns)
                        (not (char= (char line 0) #\Box_Drawings_Light_Vertical))
                        (not (char= (char line (1- (length line)))
                                    #\Box_Drawings_Light_Vertical))
                        (equal positions
                               (separator-positions
                                line (if (find #\Box_Drawings_Light_Vertical_And_Horizontal line)
                                         #\Box_Drawings_Light_Vertical_And_Horizontal
                                         #\Box_Drawings_Light_Vertical)))))
                 table))
           (format nil "localgroup columns align without outer borders at ~D columns" columns)))))
    (test-assert
     (and (search (terminal-style-sequence ':brand) styled-output)
          (search (terminal-style-sequence ':success) styled-output))
     "localgroup status applies semantic ANSI styles only when requested")
    (test-assert
     (and (search "2025-01-02T03:04Z" styled-output)
          (notany (lambda (character) (find character "│┌┐└┘├┤┬┴┼")) narrow-output)
          (search (make-string 24 :initial-element #\Box_Drawings_Light_Horizontal)
                  narrow-output)
          (every (lambda (line) (<= (text-cell-width line) 24))
                 (uiop/utility:split-string narrow-output :separator '(#\Newline))))
     "localgroup status exposes start times and fits separated borderless narrow cards"))
  (with-test-configuration (configuration)
    (let ((screen (localgroup-screen-create
                   :terminal (make-instance 'recording-terminal :columns 80))))
      (test-call-with-function-replacements
       (list
        (list 'terminal--terminal-mode-or-nil
              (lambda (terminal)
                (declare (ignore terminal))
                nil))
        (list 'daemon-connect
              (lambda (port)
                (declare (ignore port))
                (error "Noninteractive attach reached the network."))))
       (lambda ()
         (test-assert
          (handler-case
              (progn
                (channel-view-run (channel-view-create configuration "NONTTY" screen :read-only-p t))
                nil)
            (localgroup-error (condition)
              (and (eq (daemon-error-operation condition) ':attach)
                   (search "interactive terminal" (autolith-error-message condition)))))
          "a terminal client rejects noninteractive input before connecting")))))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil)
         (session       nil))
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (multiple-value-setq (application controller)
           (test-localgroup--application configuration))
         (conversation-append-user-message (application-conversation application)
                                           "named localgroup session")
         (setf session (localgroup-start application))
         (let* ((record-pathname (image-daemon:daemon-runtime-registry-pathname session))
                (record          (image-daemon:daemon-registry-read record-pathname))
                (response
                 (daemon-call (image-daemon:daemon-runtime-port session)
                              (image-daemon:daemon-runtime-token session) ':status))
                (status          (getf (rest response) :status)))
           (test-assert (image-daemon:daemon-registry-record-p record)
            "localgroup start publishes one valid private record")
           (test-assert
            (and (eq (first response) ':ok)
                 (identifier-p (image-daemon:daemon-runtime-identifier session))
                 (string= (image-daemon:daemon-runtime-identifier session)
                          (conversation-identifier
                           (application-conversation application)))
                 (string= (getf (rest status) :session-id)
                          (image-daemon:daemon-runtime-identifier session))
                 (string= (getf (rest status) :conversation-title)
                          "Named localgroup session")
                 (getf (rest status) :idle-p) (getf (rest status) :waiting-for-input-p)
                 (zerop (getf (rest status) :task-live-count)))
            "new localgroup endpoints publish their active conversation identity")
           (dolist (case '((0 0 :idle t) (1 0 :working nil) (0 1 :working nil)))
             (destructuring-bind (children executions expected-state idle-p) case
               (test-call-with-function-replacements
                (list (list 'localgroup--task-counts
                            (lambda (owner)
                              (declare (ignore owner))
                              (values children 0 (+ children executions)))))
                (lambda ()
                  (let ((properties (rest (localgroup-status-snapshot session))))
                    (test-assert
                     (and (eq (getf properties :state) expected-state)
                          (eq (getf properties :idle-p) idle-p)
                          (getf properties :waiting-for-input-p)
                          (= (getf properties :job-live-count) (+ children executions)))
                     "status distinguishes truly idle channels from unfinished child or tool jobs"))))))
           (test-assert
            (eq
             (first
              (daemon-call (image-daemon:daemon-runtime-port session) "wrong-token"
                           ':status))
             ':error)
            "an invalid capability token receives no successful status"))
         (let ((identifier (image-daemon:daemon-runtime-identifier session))
               (token      (image-daemon:daemon-runtime-token session))
               (created-at (image-daemon:daemon-runtime-created-at session)))
           (test-assert
            (eq
             (application-call-with-localgroup-quiesced application
                                                        (lambda ()
                                                          (and
                                                           (null
                                                            (application-localgroup-session
                                                             application))
                                                           ':quiesced)))
             ':quiesced)
            "checkpoint quiescence removes every localgroup runtime thread")
           (setf session (application-localgroup-session application))
           (test-assert
            (and (string= (image-daemon:daemon-runtime-identifier session) identifier)
                 (string= (image-daemon:daemon-runtime-token session) token)
                 (= (image-daemon:daemon-runtime-created-at session) created-at))
            "checkpoint quiescence preserves the active conversation identity"))
         (daemon-call (image-daemon:daemon-runtime-port session)
                      (image-daemon:daemon-runtime-token session) ':pause)
         (test-assert (application-localgroup-paused-p application)
          "localgroup pause holds queued primary work")
         (let ((kill-response-state nil)
               (write-packet        (symbol-function 'daemon-write-packet)))
           (test-call-with-function-replacements
            (list
             (list 'daemon-write-packet
                   (lambda (stream packet)
                     (let ((result (funcall write-packet stream packet)))
                       (when (and (eq (first packet) ':ok)
                                  (eq (getf (rest packet) :operation) ':kill))
                         (setf kill-response-state
                               (cons :written
                                     (not
                                      (application-input-controller-stopping-p
                                       controller)))))
                       result))))
            (lambda ()
              (let ((response
                      (daemon-call (image-daemon:daemon-runtime-port session)
                                   (image-daemon:daemon-runtime-token session) ':kill)))
                (test-assert (eq (first response) ':ok)
                             "localgroup kill returns its acknowledgement")
                (test-assert
                 (task-tests--wait-until (lambda () kill-response-state) 10)
                 "localgroup kill writes its acknowledgement")
                (test-assert (rest kill-response-state)
                             "localgroup kill writes its acknowledgement before shutdown")
                (test-assert
                 (task-tests--wait-until
                  (lambda () (application-input-controller-stopping-p controller)) 10)
                 "localgroup kill requests ordinary graceful shutdown after acknowledgement"))))))
      (when application (localgroup-stop application))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(-> test-localgroup-checkpoint-reconnect-transition () null)
(defun test-localgroup-checkpoint-reconnect-transition ()
  "Test repeated checkpoint reconnects of a controlling and a watching terminal client over real sockets."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let ((presentation (application-ui application)))
        (unwind-protect
             (progn
               (configuration-ensure-directories configuration)
               (terminal-ui-start presentation)
               (let* ((session    (localgroup-start application))
                      (identifier (image-daemon:daemon-runtime-identifier session))
                      (pathname   (image-daemon:daemon-runtime-registry-pathname session))
                      (entry      (cons pathname (localgroup--registry-record session))))
                 (world-server-tests--call-with-terminal-client
                  configuration identifier
                  (lambda (type await-controlling-exit controlling)
                    (declare (ignore type))
                    (world-server-tests--call-with-terminal-client
                     configuration identifier
                     (lambda (type await-watching-exit watching)
                       (declare (ignore type))
                       (labels ((present (marker iteration)
                                  "Show a line naming MARKER and ITERATION in the channel's transcript."
                                  (terminal-ui-append-finalized
                                   presentation (list ':checkpoint-test marker iteration)
                                   (format nil "~A-~D" marker iteration)))

                                (attached-p ()
                                  "Return true while the controlling and the watching client follow the channel."
                                  (and (application-controlling-client application)
                                       (= 2 (length (application-clients application)))))

                                (disconnected-p (view)
                                  "Return true once VIEW's connection to the old endpoint ended."
                                  (localgroup-client-connection-closed-p
                                   (channel-subscription-connection view)))

                                (shown (view text)
                                  "Return how many rows of VIEW's pane show TEXT."
                                  (presentation-tests--shown (channel-view-ui view) text)))
                         (dotimes (iteration 2)
                           (test-assert (task-tests--wait-until #'attached-p 10)
                                        "the controlling and the watching client attach before each checkpoint")
                           (test-assert
                            (equal (multiple-value-list (application-presenting-client-pids application))
                                   (list (current-process-id) (list (current-process-id))))
                            "the terminal clients state their processes on the first attach and on reconnect")
                           (present "old" iteration)
                           (test-assert
                            (eq (handler-case
                                    (application-call-with-localgroup-quiesced
                                     application
                                     (lambda ()
                                       (test-assert (null (application-localgroup-session application))
                                                    "the endpoint is absent in the saver context")
                                       (present "gap" iteration)
                                       (test-assert (task-tests--wait-until
                                                     (lambda ()
                                                       (and (disconnected-p controlling)
                                                            (disconnected-p watching)))
                                                     10)
                                                    "both clients lose the endpoint that stopped for the checkpoint")
                                       (when (= iteration 1)
                                         (error 'localgroup-error :message   "Synthetic save failure."
                                                                  :operation ':checkpoint))
                                       ':saved))
                                  (localgroup-error ()
                                    ':failed))
                                (if (zerop iteration) ':saved ':failed))
                            "checkpoint success and failure restore the parent endpoint")
                           (test-assert (task-tests--wait-until #'attached-p 10)
                                        "both clients reconnect to the parent endpoint")
                           (setf session (application-localgroup-session application))
                           (let ((status (rest (localgroup-status-snapshot session))))
                             (test-assert
                              (and (eql (getf status :terminal-client-pid) (current-process-id))
                                   (equal (getf status :observer-client-pids) (list (current-process-id)))
                                   (eq (getf status :terminal-attachment) ':remote))
                              "successful and failed checkpoints restore both attached clients' PIDs"))
                           (test-assert (null (application-input-controller-exit-reason controller))
                                        "a controlling client that leaves for a checkpoint is no lost terminal")
                           (test-assert
                            (equal (localgroup--wait-for-checkpoint-entry
                                    entry (localgroup-session-checkpoint-reconnect-id session))
                                   (cons pathname (localgroup--registry-record session)))
                            "the persistent transition accepts another authenticated client")
                           (present "new" iteration))
                         (dolist (view (list controlling watching))
                           (test-assert (task-tests--wait-until (lambda () (plusp (shown view "new-1"))) 10)
                                        "each client follows the channel after the last reconnect")
                           (dolist (marker '("old-0" "gap-0" "new-0" "old-1" "gap-1" "new-1"))
                             (test-assert (= 1 (shown view marker))
                                          "each client shows queued, reconnect and live output exactly once")))
                         (test-assert (and (<= (shown controlling "version mismatch") 1)
                                           (zerop (shown watching "version mismatch")))
                                      "a reconnect shows a client of another version no further version note")
                         (localgroup-finish-attachments application :status 0 :message "done")
                         (dolist (await-exit (list await-controlling-exit await-watching-exit))
                           (test-assert (equal (funcall await-exit) '(:status 0 :message "done" :details nil))
                                        "the reconnected client receives the normal application exit"))))
                     :read-only-p t))
                  :client-version "0.0.0")
                 (localgroup-stop application)
                 (test-assert (null (image-daemon:daemon-registry-read pathname))
                              "stopping a checkpoint endpoint removes its extended registry record")))
          (localgroup-stop application)
          (application-input-controller-stop controller)
          (terminal-ui-stop presentation)
          (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-checkpoint-reconnect-boundaries () null)
(defun test-localgroup-checkpoint-reconnect-boundaries ()
  "Test bounded rejection of a checkpoint endpoint with another identity or authentication."
  (with-test-configuration (configuration)
   (multiple-value-bind (application controller)
       (test-localgroup--application configuration)
     (unwind-protect
         (let* ((session
                 (localgroup-start application :checkpoint-reconnect-id
                                   "expected"))
                (record                                     (localgroup--registry-record session))
                (entry
                 (cons (image-daemon:daemon-runtime-registry-pathname session)
                       record))
                (*localgroup-handoff-start-timeout-seconds* 0.1))
           (dolist
               (change
                (list (list :checkpoint-reconnect-id "stale")
                      (list :session-id "different") (list :token "different")
                      (list :created-at
                            (1+
                             (image-daemon:daemon-runtime-created-at session)))
                      (list :missing nil) (list :authentication "invalid")))
             (let ((candidate (copy-list record)) (old-entry entry))
               (case (first change)
                 (:missing
                  (setf candidate nil))
                 (:authentication
                  (setf (getf (rest candidate) :token) (second change)
                        old-entry (cons (first entry) (copy-list candidate))))
                 (otherwise
                  (setf (getf (rest candidate) (first change))
                        (second change))))
               (test-call-with-function-replacements
                (list
                 (list 'image-daemon:daemon-registry-read
                       (lambda (pathname)
                         (declare (ignore pathname))
                         candidate)))
                (lambda ()
                  (let ((start (get-internal-real-time)))
                    (test-assert
                     (handler-case
                      (progn
                       (localgroup--wait-for-checkpoint-entry old-entry
                                                              "expected")
                       nil)
                      (localgroup-error nil t))
                     "a missing, mismatched or unauthenticated endpoint is rejected")
                    (test-assert
                     (<
                      (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)
                      1)
                     "endpoint rejection has a bounded wait")))))))
       (localgroup-stop application)
       (application-input-controller-stop controller)
       (application-release-conversation-lease application))))
  nil)

(-> test-localgroup-checkpoint-source-precheck-order () null)
(defun test-localgroup-checkpoint-source-precheck-order ()
  "Test a source-validation failure leaves the attached client connected."
  (unless (or (platform-supports-p *platform* ':forked-image-saver)
              (platform-supports-p *platform* ':restartable-image-saver))
    (test-withheld ':forked-image-saver
                   "checkpoint source validation with an attached client")
    (return-from test-localgroup-checkpoint-source-precheck-order nil))
  (with-test-configuration (configuration)
    (let* ((application   nil)
           (controller    nil)
           (session       nil)
           (connection    nil)
           (quiesce-count 0))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (multiple-value-setq (application controller)
               (test-localgroup--application configuration :presented-p t))
             (terminal-ui-start (application-ui application))
             (setf session (localgroup-start application))
             (multiple-value-bind (client seen await)
                 (test-localgroup--present session)
               (declare (ignore seen))
               (setf connection client)
               (let ((*checkpoint-thread-quiescer*
                       (lambda (function)
                         (declare (ignore function))
                         (incf quiesce-count)
                         (error "The quiescer must not run after precheck failure")))
                     (backend (checkpoint-backend-create configuration nil)))
                 (test-assert
                  (handler-case
                      (test-call-with-function-replacements
                       (list
                        (list
                         'checkpoint--source-snapshot
                         (lambda (active-configuration &key allow-dirty-p)
                           (declare (ignore active-configuration allow-dirty-p))
                           (error 'checkpoint-error
                                  :message  "Synthetic source check failure."
                                  :stage    ':validation
                                  :pathname nil))))
                       (lambda ()
                         (checkpoint-create backend)))
                    (checkpoint-error (condition)
                      (and (eq (checkpoint-error-stage condition) ':validation)
                           (search "Synthetic source check failure."
                                   (autolith-error-message condition)))))
                  "source-check failure is reported before session quiescence")
                 (test-assert (zerop quiesce-count)
                              "source-check failure does not invoke session quiescence")
                 (test-assert (eq (application-localgroup-session application) session)
                              "source-check failure preserves the active localgroup session"))
               (terminal-ui-append-finalized (application-ui application) ':after-source-failure
                                             "after-source-failure")
               (test-assert (and (funcall await ':append
                                          (lambda (event)
                                            (search "after-source-failure"
                                                    (test-localgroup--event-text event))))
                                 (not (localgroup-client-connection-closed-p connection)))
                            "the original connection receives output after the failure")))
        (when connection (localgroup-client-close connection))
        (when application (localgroup-stop application))
        (when controller (application-input-controller-stop controller))
        (when application
          (terminal-ui-stop (application-ui application))
          (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-detached-terminal-lifecycle () null)
(defun test-localgroup-detached-terminal-lifecycle ()
  "Test a detached session meeting its first terminal client and bounded shutdown with idle clients."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let ((ui            (application-ui application))
            (session       nil)
            (connection    nil)
            (stop-thread   nil)
            (wedged-thread nil)
            (stopped-p     nil)
            (stop-failure  nil))
        (unwind-protect
             (progn
               (configuration-ensure-directories configuration)
               (terminal-ui-start ui)
               (setf session (localgroup-start application))
               (test-assert (and (not (application-terminal-attached-p application))
                                 (eq (getf (test-localgroup--status session) :terminal-attachment)
                                     ':detached))
                            "a detached session is at no terminal before a client attaches")
               (setf connection (test-localgroup--present session))
               (test-assert (and (application-terminal-attached-p application)
                                 (eq (getf (test-localgroup--status session) :terminal-attachment)
                                     ':remote))
                            "the first controlling client puts the detached session at a terminal")
               (setf wedged-thread
                     (make-thread (lambda () (loop (sleep 60)))
                                  :name "Autolith wedged localgroup test client"))
               (with-lock-held ((image-daemon:daemon-runtime-lock session))
                 (push wedged-thread (image-daemon:daemon-runtime-client-threads session)))
               (setf stop-thread
                     (make-thread
                      (lambda ()
                        (handler-case (localgroup-stop application)
                          (error (condition)
                            (setf stop-failure condition)))
                        (setf stopped-p t))
                      :name "Autolith localgroup stop test"))
               (test-assert (task-tests--wait-until (lambda () stopped-p) 3)
                            "localgroup shutdown never waits indefinitely for idle clients")
               (test-assert
                (and (null stop-failure) (not (thread-alive-p wedged-thread))
                     (null (application-localgroup-session application))
                     (not (probe-file (image-daemon:daemon-runtime-registry-pathname session))))
                "bounded shutdown reaps clients and unpublishes the session")
               (test-assert (task-tests--wait-until
                             (lambda () (localgroup-client-connection-closed-p connection))
                             10)
                            "the idle client's connection ends with the endpoint")
               (test-assert (null (application-input-controller-exit-reason controller))
                            "a client the stopping endpoint let go is no lost terminal"))
          (when connection (localgroup-client-close connection))
          (image-daemon:daemon-stop-thread stop-thread)
          (image-daemon:daemon-stop-thread wedged-thread)
          (when (application-localgroup-session application) (localgroup-stop application))
          (application-input-controller-stop controller)
          (terminal-ui-stop ui)
          (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-client-pids () null)
(defun test-localgroup-client-pids ()
  "Exercise client PID and attachment reporting through watching, control, detach and connection loss."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let ((session     nil)
            (connections nil))
        (labels ((attach (&rest arguments)
                   "Attach a presenting test client and retain its connection for cleanup."
                   (multiple-value-bind (connection seen await)
                       (apply #'test-localgroup--present session arguments)
                     (declare (ignore seen))
                     (push connection connections)
                     (values connection await)))

                 (reported-p (pid observers attachment)
                   "Check the controlling PID, observer PIDs, attachment kind and CLI projection together."
                   (let ((status (test-localgroup--status session)))
                     (and (eql (getf status :terminal-client-pid) pid)
                          (equal (getf status :observer-client-pids) observers)
                          (= (getf status :observer-count) (length observers))
                          (eq (getf status :terminal-attachment) attachment)
                          (eq (getf status :terminal-attached-p) (eq attachment ':remote))
                          (string= (localgroup--status-field-text (cons :localgroup-status status) ':pid)
                                   (if pid (princ-to-string pid) "-")))))

                 (await-report (pid observers attachment)
                   "Wait for the endpoint to report exactly these clients and this attachment."
                   (test-assert (task-tests--wait-until
                                 (lambda () (reported-p pid observers attachment)) 5)
                                "status reports exactly the active controlling and observing clients")))
          (unwind-protect
               (progn
                 (configuration-ensure-directories configuration)
                 (terminal-ui-start (application-ui application))
                 (setf session (localgroup-start application))
                 (await-report nil nil ':detached)
                 (let ((watcher (attach :watches-p t :client-pid 1201)))
                   (await-report nil '(1201) ':detached)
                   (attach :watches-p t :client-pid 1202)
                   (await-report nil '(1201 1202) ':detached)
                   (test-assert (null (application-controlling-client application))
                                "clients that only watch are never the controlling client")
                   (let ((earlier (attach :client-pid 2201)))
                     (await-report 2201 '(1201 1202) ':remote)
                     (test-assert
                      (and (= (getf (test-localgroup--status session) :pid) (current-process-id))
                           (eq (localgroup-owner-reachability
                                (image-daemon:daemon-runtime-registry-pathname session)
                                (image-daemon:daemon-runtime-identifier session) (current-process-id))
                               ':reachable))
                      "the endpoint's process identity and owner reachability use the server PID")
                     (let ((later (attach :client-pid 3301)))
                       (await-report 3301 '(1201 1202 2201) ':remote)
                       (localgroup-client-call earlier ':state)
                       (await-report 2201 '(1201 1202 3301) ':remote)
                       (localgroup-client-call watcher ':state)
                       (await-report 2201 '(1201 1202 3301) ':remote)
                       (localgroup-client-close later)
                       (await-report 2201 '(1201 1202) ':remote))
                     (localgroup-client-call earlier ':detach)
                     (localgroup-client-close earlier)
                     (await-report nil '(1201 1202) ':detached))
                   (multiple-value-bind (control await) (attach :client-pid 4401)
                     (await-report 4401 '(1201 1202) ':remote)
                     (localgroup--detach-terminal session)
                     (test-assert (funcall await ':detach)
                                  "a remote detach tells the controlling client to leave")
                     (localgroup-client-close control)
                     (await-report nil '(1201 1202) ':detached))
                   (let ((control (attach :client-pid 5501)))
                     (await-report 5501 '(1201 1202) ':remote)
                     (localgroup-client-close control)
                     (await-report nil '(1201 1202) ':detached))
                   (localgroup-client-close watcher)
                   (await-report nil '(1202) ':detached))
                 (dolist (pid '(nil 0 -1 "123" 1.5))
                   (let ((connection (localgroup-client-connect
                                      (cons (image-daemon:daemon-runtime-registry-pathname session)
                                            (image-daemon:daemon-runtime-record session))
                                      :name      "terminal"
                                      :arguments (list :presents-p t :rows 31 :columns 91
                                                       :client-pid pid))))
                     (push connection connections)
                     (test-assert (task-tests--wait-until
                                   (lambda () (reported-p nil '(1202) ':remote)) 5)
                                  (format nil "a controlling client that states the process ~S is reported without one"
                                          pid))
                     (localgroup-client-close connection)
                     (await-report nil '(1202) ':detached))))
            (mapc #'localgroup-client-close connections)
            (when session (localgroup-stop application))
            (application-input-controller-stop controller)
            (terminal-ui-stop (application-ui application))
            (application-release-conversation-lease application))))))
  nil)

(-> test-localgroup-attachments () null)
(defun test-localgroup-attachments ()
  "Test a client that watches and a client that controls a session's presentation over the endpoint."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let* ((ui       (application-ui application))
             (terminal (terminal-ui-terminal ui))
             (session  nil)
             (watcher  nil)
             (control  nil))
        (flet ((shows-p (await text)
                 "Wait for a transcript event that shows TEXT and return whether one came."
                 (not (null (funcall await ':append
                                     (lambda (event)
                                       (search text (test-localgroup--event-text event)))))))

               (refusal (connection command &rest arguments)
                 "Return the message COMMAND is refused with, or NIL when the channel takes it."
                 (handler-case (progn (apply #'localgroup-client-call connection command arguments) nil)
                   (localgroup-error (condition)
                     (autolith-error-message condition))))

               (work-items ()
                 "Return the work the session's controller has queued."
                 (with-lock-held ((application-input-controller-lock controller))
                   (application-input-controller--state controller :work-items))))
          (unwind-protect
               (progn
                 (configuration-ensure-directories configuration)
                 (terminal-ui-start ui)
                 (terminal-ui-append-finalized ui ':before-attachment "before attachment")
                 (setf session (localgroup-start application))
                 (multiple-value-bind (connection seen await)
                     (test-localgroup--present session :rows 11 :columns 41 :watches-p t)
                   (declare (ignore seen))
                   (setf watcher connection)
                   (test-assert (shows-p await "before attachment")
                                "a watching client is replayed what the channel shows")
                   (terminal-ui-append-finalized ui ':observer-output "observer output")
                   (test-assert (shows-p await "observer output")
                                "a watching client receives what the channel presents")
                   (test-assert (and (= (terminal-rows terminal) 24) (= (terminal-columns terminal) 80))
                                "the channel is not laid out for a pane that only watches")
                   (test-assert (and (not (terminal-interactive-p terminal))
                                     (not (application-terminal-attached-p application))
                                     (null (application-controlling-client application)))
                                "a channel that is only watched has no controlling client and is at no terminal")
                   (dolist (command '((:input :action :submit :text "ignored")
                                      (:submit :text "ignored")
                                      (:event :event :submit)
                                      (:geometry :rows 50 :columns 150)
                                      (:cancel)
                                      (:mode :value :ask)
                                      (:operation :method "status")
                                      (:detach)))
                     (test-assert (search "only watches" (apply #'refusal watcher command))
                                  (format nil "a watching client's ~(~A~) command is refused" (first command))))
                   (test-assert (and (null (work-items))
                                    (= (terminal-columns terminal) 80))
                                "what a watching client was refused changed nothing")
                   (test-assert (and (eq (first (getf (localgroup-client-call watcher ':state) :state))
                                         ':localgroup-status)
                                     (null (refusal watcher ':input-history :start 0 :limit 5)))
                                "a watching client may ask for the channel's state and input history"))
                 (multiple-value-bind (connection seen await)
                     (test-localgroup--present session :rows 31 :columns 91)
                   (declare (ignore seen))
                   (setf control connection)
                   (test-assert (and (terminal-interactive-p terminal)
                                     (application-terminal-attached-p application)
                                     (= (terminal-rows terminal) 31)
                                     (= (terminal-columns terminal) 91)
                                     (eq (getf (test-localgroup--status session) :terminal-attachment)
                                         ':remote))
                                "a controlling client puts the channel at a terminal and lays it out for its pane")
                   (test-assert (shows-p await "observer output")
                                "a controlling client is replayed what the channel showed before it")
                   (localgroup-client-call control ':input :action ':submit :text "remote")
                   (test-assert (equal (work-items) (list (list ':message "remote")))
                                "a controlling client's submission uses the ordinary input queue")
                   (localgroup-client-call control ':geometry :rows 44 :columns 120)
                   (test-assert (and (= (terminal-rows terminal) 44) (= (terminal-columns terminal) 120))
                                "a controlling client's new pane size lays the channel out again")
                   (localgroup-client-call control ':detach)
                   (localgroup-client-close control)
                   (test-assert (test-localgroup--await-served session 1)
                                "the session finishes serving the client that left"))
                 (test-assert (and (eq (getf (test-localgroup--status session) :terminal-attachment)
                                       ':detached)
                                   (null (application-input-controller-exit-reason controller)))
                              "a client that says it leaves on purpose leaves the session running without a terminal")
                 ;; More history than a pane holds, as a channel in use has.
                 (dotimes (index 60)
                   (terminal-ui-append-finalized ui (list ':history-line index)
                                                 (format nil "history line ~D" index)))
                 (dolist (case (list (list nil nil) (list "" nil) (list *autolith-version* nil)
                                     (list "0.0.0" t) (list "99.0.0" t)))
                   (destructuring-bind (client-version noted-p) case
                     (world-server-tests--call-with-terminal-client
                      configuration (image-daemon:daemon-runtime-identifier session)
                      (lambda (type await-exit view)
                        (declare (ignore await-exit))
                        (let ((view-ui (channel-view-ui view))
                              (marker  (format nil "attached as ~S" client-version)))
                          ;; The channel's replay comes before what it presents afterwards,
                          ;; and the view reads a key only once it runs.
                          (funcall type '(:insert "typed"))
                          (terminal-ui-append-finalized ui (list ':attached-marker client-version) marker)
                          (test-assert
                           (and (task-tests--wait-until
                                 (lambda ()
                                   (string= "typed" (line-editor-text (terminal-ui-editor view-ui))))
                                 10)
                                (task-tests--wait-until
                                 (lambda () (plusp (presentation-tests--shown view-ui marker))) 10))
                           "the view follows its channel and reads its keys")
                          (when noted-p
                            (task-tests--wait-until
                             (lambda () (plusp (presentation-tests--shown view-ui "version mismatch"))) 5))
                          (let* ((rows    (with-terminal-ui-locked (view-ui)
                                            (let ((viewport (fullscreen-terminal-ui-viewport view-ui)))
                                              (loop for row below (clinedi:transcript-viewport-row-count viewport)
                                                    collect (clinedi:transcript-viewport-row-text viewport row)))))
                                 (notes   (loop for row in rows
                                                for index from 0
                                                when (search "version mismatch" row)
                                                  collect index))
                                 (history (position-if (lambda (row) (search "history line 59" row)) rows)))
                            (test-assert
                             (if noted-p
                                 (and history
                                      (= 1 (length notes))
                                      (< history (first notes))
                                      (search client-version (nth (first notes) rows))
                                      (search *autolith-version* (nth (first notes) rows)))
                                 (null notes))
                             (format nil "a terminal client of version ~S shows ~:[no version note~;one note of both versions below the channel's transcript~]"
                                     client-version noted-p)))))
                      :client-version client-version))))
            (when control (localgroup-client-close control))
            (when watcher (localgroup-client-close watcher))
            (when session (localgroup-stop application))
            (application-input-controller-stop controller)
            (terminal-ui-stop ui)
            (application-release-conversation-lease application))))))
  nil)

(-> test-localgroup-open-instruction-relay () null)
(defun test-localgroup-open-instruction-relay ()
  "Test the terminal client reports reopen advice after its channel told it that the session exits."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let* ((conversation (application-conversation application))
             (identifier   (conversation-identifier conversation)))
        (unwind-protect
             (progn
               (configuration-ensure-directories configuration)
               (conversation-append-user-message conversation "keep this conversation")
               (terminal-ui-start (application-ui application))
               (localgroup-start application)
               (world-server-tests--call-with-terminal-client
                configuration identifier
                (lambda (type await-exit view)
                  (declare (ignore type view))
                  (test-assert (null (application--present-open-instruction application))
                               "The server leaves attached-channel reopen advice to the client")
                  (test-assert
                   (not (search (application--open-command application)
                                (application-input-controller--forced-exit-message
                                 controller ':turn-cancellation)))
                   "The server's forced-exit notice leaves reopen advice to the client")
                  (localgroup-finish-attachments application :status 0 :message "Channel closed.")
                  (multiple-value-bind (exit ended-p) (funcall await-exit)
                    (let* ((command  (application--open-command application))
                           (output   (with-output-to-string (*error-output*)
                                       (localgroup-relay-exit configuration exit ':read-only
                                                              :session-id identifier)))
                           (position (search command output)))
                      (test-assert (and ended-p
                                        (eql (getf exit :status) 0)
                                        (equal (getf exit :message) "Channel closed."))
                                   "The exit the client is told carries no server-generated reopen command")
                      (test-assert (and position
                                        (not (search command output :start2 (+ position (length command)))))
                                   "The client reports the attached channel's reopen command exactly once"))))))
          (when (application-localgroup-session application) (localgroup-stop application))
          (application-input-controller-stop controller)
          (terminal-ui-stop (application-ui application))
          (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-session-exit-relay () null)
(defun test-localgroup-session-exit-relay ()
  "Test a detached session's launcher exit reaches its controlling terminal client."
  (with-test-configuration (configuration)
    (multiple-value-bind (application controller)
        (test-localgroup--application configuration :presented-p t)
      (let ((message "Autolith will update to 9.9.9 after restoring the terminal.")
            (details (list :crash-capsule "/tmp/capsule.sexp")))
        (unwind-protect
             (progn
               (configuration-ensure-directories configuration)
               (terminal-ui-start (application-ui application))
               (world-server-tests--call-with-terminal-client
                configuration (image-daemon:daemon-runtime-identifier (localgroup-start application))
                (lambda (type await-exit view)
                  (declare (ignore type view))
                  (test-assert (localgroup-finish-attachments application
                                                              :status  76
                                                              :message message
                                                              :details details)
                               "a detached session tells its terminal clients that it exits")
                  (test-assert (equal (funcall await-exit)
                                      (list :status 76 :message message :details details))
                               "the controlling terminal receives the launcher status, message and details")))
               (multiple-value-bind (plain plain-controller)
                   (test-localgroup--application configuration)
                 (unwind-protect
                      (test-assert
                       (not (localgroup-finish-attachments plain :status 76 :message message))
                       "a session on its own terminal has no clients to tell")
                   (application-input-controller-stop plain-controller)
                   (application-release-conversation-lease plain))))
          (when (application-localgroup-session application) (localgroup-stop application))
          (application-input-controller-stop controller)
          (terminal-ui-stop (application-ui application))
          (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-relay-exit () null)
(defun test-localgroup-relay-exit ()
  "Test the relaying process exits like its session, and observers only report it."
  (with-test-configuration (configuration)
    (let* ((statuses nil)
           (exit     (list :status 76 :message "Autolith will update to 9.9.9."))
           (state    (config :state-root configuration))
           (pointer  (merge-pathnames "crash-pointers/launcher-1.path" state))
           (capsule  (merge-pathnames "crashes/capsule.sexp" state))
           (crash    (list :status  70
                           :message "Autolith entered recovery after a fatal error."
                           :details (list :crash-capsule (namestring capsule)))))
      (ensure-directories-exist pointer)
      (test-call-with-function-replacements
       (list (list 'uiop:quit (lambda (&optional (status 0) &rest ignored)
                                (declare (ignore ignored))
                                (push status statuses))))
       (lambda ()
         (with-test-environment (("AUTOLITH_CRASH_POINTER" (namestring pointer)))
            (dolist (case (list (list exit ':control) (list exit ':read-only)
                               (list nil ':control) (list nil ':read-only)))
              (destructuring-bind (result mode) case
                (let* ((report   (with-output-to-string (*error-output*)
                                   (localgroup-relay-exit configuration result mode :session-id "bopen1")))
                       (command  (channel-open-command "bopen1"))
                       (position (search command report)))
                  (test-assert (and position
                                    (not (search command report :start2 (+ position (length command)))))
                               "Controllers and observers report one reopen command, including detach or connection loss")
                  (test-assert (eq (not (null (search (getf exit :message) report)))
                                   (not (null result)))
                               "An attachment's exit message is reported only when one was received"))))
            (test-assert (equal statuses '(76))
                         "Only the controlling terminal exits with the session's status")
            (let* ((command  (channel-open-command "bopen1"))
                   (message  (format nil "Channel closed. Run ~A" command))
                   (report   (with-output-to-string (*error-output*)
                               (localgroup-relay-exit configuration (list :status 78 :message message)
                                                      ':control :session-id "bopen1")))
                   (position (search command report)))
              (test-assert (and position
                                (not (search command report :start2 (+ position (length command)))))
                           "A legacy server's reopen command is reported once")
              (test-assert (eql (first statuses) 78)
                           "Already-reported advice preserves the controlling exit status"))
           (with-output-to-string (*error-output*)
             (localgroup-relay-exit configuration crash ':read-only))
           (test-assert (not (probe-file pointer))
                        "an observer leaves the session's crash capsule to the controller")
           (with-output-to-string (*error-output*)
             (localgroup-relay-exit configuration crash ':control))
           (test-assert (and (eql (first statuses) 70)
                             (string= (uiop:read-file-line pointer) (namestring capsule)))
                        "the controlling terminal hands the session's crash capsule to its launcher")
           (delete-file pointer)
           (with-output-to-string (*error-output*)
             (localgroup-relay-exit
              configuration
              (list :status 70 :details (list :crash-capsule "/tmp/elsewhere.sexp"))
              ':control))
           (test-assert (not (probe-file pointer))
                        "a capsule outside private crash state is never handed to recovery"))))))
  (dolist (case '((:update 76 "update to 1.2.3" :notice)
                  (:rollback 75 "retained generation G1" :notice)
                  (:fatal 70 "Capsule: " :failure)))
    (destructuring-bind (kind status text style) case
      (multiple-value-bind (actual-status message actual-style details)
          (main--launcher-exit
           (ecase kind
             (:update
              (make-condition 'update-requested :message "Update." :tag "v1.2.3"))
             (:rollback
              (make-condition 'rollback-requested :message       "Roll back."
                                                  :generation-id "G1"))
             (:fatal
              (make-condition 'fatal-control-path-error
                              :message          "Fatal."
                              :cause            (make-condition 'simple-error)
                              :capsule-pathname #P"/tmp/capsule.sexp"))))
        (test-assert (and (= actual-status status) (search text message)
                          (eq actual-style style))
                     (format nil "a ~(~A~) exit maps to launcher status ~D in ~(~A~) style"
                             kind status style))
        (test-assert (equal details (and (eq kind :fatal)
                                         '(:crash-capsule "/tmp/capsule.sexp")))
                     (format nil "a ~(~A~) exit relays ~:[no details~;its crash capsule~]"
                             kind (eq kind :fatal))))))
  nil)

(-> test-localgroup-query-keeps-slow-endpoint () null)
(defun test-localgroup-query-keeps-slow-endpoint ()
  "Test a late answer keeps an endpoint record while a refused connection prunes it."
  (with-test-configuration (configuration)
    (let* ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                    :type     ':stream
                                    :protocol ':tcp))
           (pathname (localgroup-registry-pathname configuration "slowendpoint"))
           (record   nil))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind
              listener (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
             (sb-bsd-sockets:socket-listen listener 4)
             (setf record (list :localgroup-endpoint
                                :version    1
                                :session-id "slowendpoint"
                                :pid        (current-process-id)
                                :address    "127.0.0.1"
                                :port       (nth-value 1 (sb-bsd-sockets:socket-name listener))
                                :token      "token"
                                :created-at 1))
             (ensure-directories-exist pathname)
             (image-daemon:daemon-registry-publish pathname record)
             (flet ((query-fails-p ()
                      (let ((image-daemon:*daemon-connect-timeout-seconds* 0.2))
                        (handler-case
                            (progn (localgroup-query-record (cons pathname record) ':status) nil)
                          (localgroup-error () t)))))
               (test-assert (query-fails-p) "an endpoint that never answers fails the request")
               (test-assert (probe-file pathname)
                            "a live process that answers late keeps its record")
               (sb-bsd-sockets:socket-close listener)
               (test-assert (query-fails-p) "a refused connection fails the request")
               (test-assert (null (probe-file pathname))
                            "a refused connection prunes the record")))
        (sb-bsd-sockets:socket-close listener))))
  nil)
