(in-package #:autolith)

;;;; -- CLI Conversation Selection Tests --

(-> test-cli-resume-prefixes () null)
(defun test-cli-resume-prefixes ()
  "Test exact and prefix selection across stored and live conversations."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (configuration-ensure-directories configuration)
    (let ((statuses nil))
      (labels ((save (identifier title)
                 "Persist a selectable conversation with a distinguishing title."
                 (let ((conversation (conversation-create configuration :identifier identifier)))
                   (conversation-append-user-message conversation (format nil "Work on ~A" title))
                   (conversation-set-title conversation title)))

               (select (prefix identifier)
                 "Assert PREFIX selects IDENTIFIER."
                 (test-assert (string= (main-resolve-conversation configuration prefix) identifier)
                              "a unique prefix or exact ID selects the canonical stored ID"))

               (reject (prefix expected)
                 "Assert PREFIX reports every EXPECTED detail as a selection error."
                 (let ((condition
                         (handler-case
                             (progn (main-resolve-conversation configuration prefix) nil)
                           (configuration-error (condition) condition))))
                   (test-assert
                    (and condition
                         (every (lambda (detail) (search detail (autolith-error-message condition)))
                                expected))
                    "a failed selector reports every matching full ID and available context"))))
        (test-call-with-function-replacements
         (list (list 'localgroup-statuses
                     (lambda (client-configuration)
                       (declare (ignore client-configuration))
                       statuses)))
         (lambda ()
           (reject "b" nil)
           (save "b6uouHJ" "First workspace task")
           (dolist (prefix '("b" "B" "b-" "b6" "b-6" "uou" "UOUHJ" "b6uouHJ" "b-6uouHJ"))
             (select prefix "b6uouHJ"))
           (dolist (prefix '("" "b--6" "b6uouHJx" "../b" "*"))
             (reject prefix nil))
           (save "b6uouHK" "Second workspace task")
           (let ((empty (conversation-create configuration :identifier "b6uouHL")))
             (test-assert
              (not (find (conversation-pathname empty) (conversation-list configuration) :test #'equal))
              "a fresh live conversation can be absent from the saved-conversation picker")
             (setf statuses
                   (list (list :localgroup-status :conversation-id "b6uouHL" :session-id "b6uouHL"
                               :conversation-title "Live empty task" :pid 424242
                               :cwd (namestring (config :working-directory configuration))))))
           (select "b6uouHL" "b6uouHL")
           (select "b-6uouHL" "b6uouHL")
           (dolist (prefix '("b" "b-6" "b6uouH"))
             (reject prefix (list "b-6uouHJ" "b-6uouHK" "b-6uouHL"
                                  "First workspace task" "Second workspace task"
                                  "Live empty task" "424242"
                                  (namestring (config :working-directory configuration)))))
           (select "b6uouHJ" "b6uouHJ")
           (setf statuses
                 (append statuses
                         (list (list :localgroup-status :conversation-id "b6uouHJ" :pid 424243))))
           (select "b-6uouHJ" "b6uouHJ")
           (save "abcdef1" "Short exact ID")
           (save "abcdef123456" "Legacy first task")
           (save "abcdef234567" "Legacy second task")
           (dolist (case '(("abcdef1" "abcdef1")
                           ("a-bcdef1" "abcdef1")
                           ("abcdef12" "abcdef123456")
                           ("DEF12" "abcdef123456")
                           ("ABCDEF12" "abcdef123456")
                           ("ABCDEF123456" "abcdef123456")))
             (select (first case) (second case)))
           (reject "ABC" '("abcdef1" "abcdef123456" "abcdef234567"))
           (reject "a-b" '("abcdef1" "abcdef123456" "abcdef234567"))
           (let ((identifiers
                   (loop for character across "ABCDEFGHJKLMNPQR"
                         collect (format nil "z1234~Cz" character))))
             (dolist (identifier identifiers)
               (save identifier (format nil "Task ~A" identifier)))
             (reject "z" (mapcar #'conversation-identifier-display identifiers)))
           (world-reconcile-sessions configuration)
           (world-participant-rename configuration "al-b6uouHJ" "builder")
           (dolist (designator '("builder" "BUILDER" "@builder" "al-b6uouHJ" "/builder"))
            (select designator "b6uouHJ"))
           (reject "@nobody" '("No participant"))
           (let* ((world (world-load configuration))
                  (home  (world-channel-create configuration :id "bworld1"
                          :owner                                 (participant-identifier (world-human-participant world))
                          :workspace                             (namestring (config :working-directory configuration)))))
             (world-participant-create configuration :kind ':agent :name "unloaded"
                                       :residence (channel-identifier home))
             (dolist (designator '("/unloaded" "unloaded" "bworld1" "world1"))
               (select designator "bworld1")))
           (world-participant-park configuration "al-b6uouHJ")
           (reject "builder" '("parked")))))))
  nil)


;;;; -- Startup Dispatch --

(-> test-cli-world-client-start () null)
(defun test-cli-world-client-start ()
  "Test client-owned welcome, explicit starts, failures and channel navigation."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((identifier     "b6uouHJ")
           (record         (list #p"unused.sexp" :localgroup-endpoint
                                 :session-id identifier :pid 424242))
           (opened         nil)
           (attached       nil)
           (queried        nil)
           (open-error     nil)
           (attach-error   nil)
           (started-p      t)
           (switch-to      nil)
           (welcomed       0)
           (welcome-events nil)
           (direct-p       nil))
      (test-call-with-function-replacements
       (list
        (list 'configuration-create
              (lambda (&rest arguments) (declare (ignore arguments)) configuration))
        (list 'main--client-session-p
              (lambda (&rest arguments) (declare (ignore arguments)) t))
        (list 'main--interactive-input-p
              (lambda () t))
        (list 'main-resolve-conversation
              (lambda (resolve-configuration prefix)
                (declare (ignore resolve-configuration prefix))
                identifier))
        (list 'main--welcome-world-client
              (lambda (screen welcome-configuration directory &rest arguments)
                (declare (ignore welcome-configuration))
                (incf welcomed)
                (test-assert (and (null (world-sidebar-current (localgroup-screen-panel screen)))
                                  (equal directory (namestring (uiop:getcwd))))
                             "welcome has no selected channel and uses the client's launch directory")
                (let ((event (pop welcome-events)))
                  (if (functionp event) (funcall event arguments) event))))
        (list 'localgroup-endpoint-records
              (lambda (&rest arguments) (declare (ignore arguments)) (list record)))
        (list 'world-client-open
              (lambda (open-configuration &rest arguments)
                (declare (ignore open-configuration))
                (when open-error (error open-error))
                (push arguments opened)
                (values identifier started-p)))
        (list 'channel-view-run
                (lambda (view &key attached-function notice client-version boot-p wait-function)
                  (declare (ignore notice client-version boot-p wait-function))
                (let ((channel (channel-subscription-identifier view)))
                  (test-assert (member channel (list identifier "c6uouHJ") :test #'string=)
                               "the client attaches to the channel the server opened")
                  (test-assert (localgroup-screen-panel (channel-view-screen view))
                               "plain starts and explicit opens attach with the same world navigation UI")
                  (push (list channel (if (channel-view-read-only-p view) ':read-only ':control))
                        attached)
                  (when attach-error (error attach-error))
                  (when attached-function (funcall attached-function))
                  (when switch-to
                    (list ':switch :session-id (shiftf switch-to nil))))))
        (list 'world-client-entry
              (lambda (candidate)
                (declare (ignore candidate))
                (cons "world" (list :session-id "world"))))
        (list 'localgroup-query-record
              (lambda (entry operation &optional arguments)
                (declare (ignore entry))
                ;; The sidebar's own thread asks for the channels it shows.
                (unless (eq operation ':channels)
                  (push (if (eq operation ':close)
                            (list operation (getf arguments :unattached-only-p))
                            operation)
                        queried))
                (list :ok)))
        (list 'localgroup-relay-exit
              (lambda (&rest arguments) (declare (ignore arguments))))
        (list 'main--connect-application
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (setf direct-p t)
                (error 'configuration-error :message "direct startup"))))
       (lambda ()
         (let ((*error-output* (make-string-output-stream)))
           (main-dispatch '())
            (test-assert (and (= welcomed 1) (= (length opened) 1) (null attached)
                              (getf (first opened) :inbox-p) (not (getf (first opened) :attach-expected-p))
                              (null queried) (not direct-p))
                         "a bare launch preloads home without attaching and shows the welcome banner")
            (setf opened nil attached nil welcome-events (list (list ':switch :session-id identifier)))
            (main-dispatch '())
            (test-assert (and (= welcomed 2) (= (length opened) 2) (= (length attached) 1)
                              (getf (second opened) :inbox-p)
                              (equal (getf (first opened) :channel) identifier))
                         "selecting home from the welcome screen attaches to the preloaded channel")
           (setf opened nil attached nil)
           (main-dispatch (list "open" identifier))
           (main-dispatch (list "open" identifier))
           (test-assert (and (= (length opened) 2)
                             (string= (getf (first opened) :channel) identifier)
                             (= (length attached) 2))
                        "resuming a conversation opens that channel in the server")
           (main-dispatch '("open"))
           (test-assert (and (= (length opened) 3)
                             (null (getf (first opened) :channel))
                             (getf (first opened) :latest-p)
                             (not (getf (first opened) :pick-channel-p)))
                        "open without an ID asks the server for the latest workspace channel, not a picker")
           (setf attach-error (make-condition 'localgroup-error
                                              :operation ':attach
                                              :message   "The attachment was rejected."))
           (test-assert (handler-case
                             (progn (main-dispatch (list "open" identifier)) nil)
                          (localgroup-error (condition)
                            (and (search "rejected" (autolith-error-message condition))
                                 (equal queried '((:close t))))))
                        "a failed attach asks the server to close the channel started for this terminal while nobody controls it")
           (setf started-p nil
                 queried   nil)
           (test-assert (handler-case
                             (progn (main-dispatch (list "open" identifier)) nil)
                          (localgroup-error (condition)
                            (and (search "rejected" (autolith-error-message condition))
                                 (null queried))))
                        "a failed attach leaves a channel the server already served")
           (setf attach-error nil
                 open-error   (make-condition 'localgroup-error
                                              :operation ':spawn
                                              :message   "No world server could start."))
           (test-assert (handler-case
                             (progn (main-dispatch (list "open" identifier)) nil)
                          (localgroup-error (condition)
                            (and (not direct-p)
                                 (search "The world server is not available: No world server could start"
                                         (autolith-error-message condition)))))
                        "a failed open ends the start with its reason instead of starting directly")
           (setf open-error nil
                 opened     nil
                 attached   nil
                 switch-to  "c6uouHJ")
           (main-dispatch (list "open" identifier))
           (test-assert (and (= (length opened) 2)
                             (string= (getf (second opened) :channel) identifier)
                             (string= (getf (first opened) :channel) "c6uouHJ")
                             (= (length attached) 2)
                             (null switch-to))
                        "a switch request opens the named channel and attaches to it")
           (dolist (arguments (list (list "open" identifier "--read-only")
                                    '("open" "--read-only")))
             (setf opened nil attached nil)
             (main-dispatch arguments)
             (test-assert (and (= (length attached) 1)
                               (eq (second (first attached)) ':read-only))
                          "read-only open observes an explicit channel or the latest workspace channel"))
           (setf opened nil attached nil switch-to "c6uouHJ")
           (main-dispatch (list "open" identifier "--read-only"))
           (test-assert (and (= (length attached) 2)
                             (every (lambda (attachment) (eq (second attachment) ':read-only)) attached))
                        "read-only navigation observes each channel without acquiring control")
           (test-call-with-function-replacements
            (list (list 'main--interactive-input-p (lambda () nil)))
            (lambda ()
              (test-assert (handler-case
                              (progn (main-dispatch (list "open" identifier "--read-only")) nil)
                             (configuration-error () t))
                           "read-only open rejects noninteractive input instead of submitting it")))
           (test-call-with-function-replacements
            (list (list 'main--client-session-p (lambda (&rest arguments) (declare (ignore arguments)) nil)))
            (lambda ()
              (test-assert (handler-case
                              (progn (main-dispatch (list "open" identifier "--read-only")) nil)
                             (configuration-error () (not direct-p)))
                           "read-only open never falls back to a controlling direct session")))
           (setf opened nil)
           (main-dispatch (list "open" identifier "--immutable"))
           (test-assert (equal (getf (first opened) :launch) '(:immutable-p t))
                        "the options a start states explicitly travel with the open")
           (setf opened nil)
           (main-dispatch (list "open" identifier "--model" "model" "--permissions" "full"))
           (let ((launch (getf (first opened) :launch)))
             (test-assert (and (equal (getf launch :model) "model")
                               (eq (getf launch :permission-mode) ':full-access))
                          "explicit live opens forward the same launch options as other starts"))
           (setf opened nil attached nil)
           (test-call-with-function-replacements
            (list (list 'application-recovery-state (lambda (candidate)
                                                     (declare (ignore candidate)) (values identifier nil)))
                  (list 'application-recovery-diagnosis-prompt (lambda (candidate)
                                                                (declare (ignore candidate)) "Internal crash instructions")))
            (lambda ()
              (main-dispatch '())
              (test-assert (and (equal (getf (first opened) :channel) identifier)
                                (not (getf (first opened) :inbox-p))
                                (equal (getf (first opened) :recovery-diagnosis) "Internal crash instructions")
                                (= (length attached) 1))
                           "recovery opens the recovered channel instead of the inbox"))))))))
  nil)

(-> test-client-welcome-layout () null)
(defun test-client-welcome-layout ()
  "Check welcome geometry and durable participant counts independently of channel liveness."
  (with-test-configuration (configuration root)
    (let ((directory (namestring (config :working-directory configuration))))
      (multiple-value-bind (human home)
          (world-participant-create configuration :kind ':human :name "user"
                                    :home (list :workspace directory))
        (let ((remote (world-channel-create configuration :owner (participant-identifier human)
                                            :workspace           (namestring (merge-pathnames "other/" root)))))
          (dolist (residence (list (channel-identifier home) (channel-identifier home)
                                  (channel-identifier remote) nil))
            (let ((agent (world-participant-create configuration :kind ':agent :name (make-identifier)
                          :residence (or residence (channel-identifier home)))))
              (unless residence
                (world-participant-park configuration (participant-identifier agent)))))
          (multiple-value-bind (total local) (main--welcome-agent-counts (world-load configuration) directory)
            (test-assert (and (= total 4) (= local 2))
                         "count agents rather than channels, include parked agents, and compare residences")))))
    (test-call-with-function-replacements
     (list (list 'terminal--render-spans
                 (lambda (terminal spans) (declare (ignore terminal)) (terminal--spans-text spans))))
     (lambda ()
       (dolist (geometry '((120 32) (120 10) (60 10) (44 12) (20 8) (1 1)))
         (destructuring-bind (width height) geometry
           (let* ((terminal (make-instance 'recording-terminal :columns width :rows height))
                  (screen   (localgroup-screen-create :terminal terminal))
                  (frame    (main--welcome-frame screen "/launch/" :total 4 :local 2
                             :immutable-p t :recovered-p t :recovery-conversation-id "b6uouHJ"))
                  (rows     (coerce (clinedi:frame-snapshot-rows frame) 'list))
                  (nonempty (remove-if (lambda (row) (zerop (length (string-trim " " row)))) rows)))
             (test-assert (and (= (clinedi:frame-snapshot-height frame) height)
                               (= (clinedi:frame-snapshot-width frame) width)
                               (every (lambda (row) (<= (text-cell-width row) width)) rows))
                          "welcome fits wide, compact and one-cell geometry")
             (when (= width 44)
               (let ((text (format nil "~{~A~%~}" rows)))
                 (test-assert (and (search "Recovery available" text) (search "Sidebar:" text))
                              "drop the logo before hiding recovery or navigation in a compact pane")))
             (when (= width 120)
               (let* ((block-width (loop for row in nonempty
                                         maximize (text-cell-width (string-left-trim " " row))))
                      (left        (loop for row in nonempty minimize (position-if-not (lambda (char) (char= char #\Space)) row))))
                 (test-assert (= left (floor (- width block-width) 2))
                              "center the actual content block within the pane")))
             (when (= height 32)
               (let ((text (format nil "~{~A~%~}" rows)))
                 (test-assert (and (search "/launch/" text) (search "4 (2 here)" text)
                                   (search "immutable, recovered" text) (search "autolith open" text))
                              "welcome shows launch workspace, durable counts, modes and explicit recovery action")))))))))
  nil)

(-> test-client-welcome-input () null)
(defun test-client-welcome-input ()
  "Check input startup, physical resize, quit and screen-preserving selection handoff."
  (with-test-configuration (configuration)
    (dolist (last-event (list ':interrupt ':enter (list ':switch :session-id "b6uouHJ")))
      (let* ((terminal                    (make-instance 'recording-terminal :columns 80 :rows 24))
             (screen                      (localgroup-screen-create :terminal terminal))
             (events                      (list ':resize last-event))
             (started-p                   nil)
             (stopped-p                   nil)
             (unwatched-p                 nil)
             (last-frame                  nil)
             (*terminal-resize-pending-p* nil))
        (test-call-with-function-replacements
         (list
          (list 'terminal-start (lambda (candidate) (declare (ignore candidate)) (setf started-p t)))
          (list 'terminal-stop (lambda (candidate) (declare (ignore candidate)) (setf stopped-p t)))
          (list 'platform-watch-terminal-resize
                (lambda (platform function) (declare (ignore platform function)) ':welcome-watch))
          (list 'platform-unwatch-terminal-resize
                (lambda (platform watch) (declare (ignore platform))
                  (setf unwatched-p (eq watch ':welcome-watch))))
          (list 'terminal-current-size (lambda () (values 12 44)))
          (list 'terminal-input-ready-p (lambda (candidate) (declare (ignore candidate)) t))
          (list 'terminal-read-event (lambda (candidate) (declare (ignore candidate)) (pop events)))
          (list 'localgroup-screen-show-frame
                (lambda (candidate frame &rest arguments)
                  (declare (ignore arguments))
                  (setf last-frame frame (localgroup-screen-frame candidate) frame))))
         (lambda ()
           (let ((exit (main--welcome-world-client screen configuration "/launch/"
                         :recovery-diagnosis "INTERNAL CRASH INSTRUCTIONS")))
             (test-assert (and started-p unwatched-p (not stopped-p)
                               (eq last-frame (localgroup-screen-frame screen))
                               (= (clinedi:frame-snapshot-width last-frame) 44)
                               (= (clinedi:frame-snapshot-height last-frame) 12))
                          "welcome starts input, follows physical resize and releases only its watch on handoff")
             (test-assert (equal exit (case last-event
                                       (:interrupt
                                        nil)
                                       (:enter
                                        (list ':switch :new-p t))
                                       (otherwise
                                        last-event)))
                          "quit, explicit creation and sidebar selection return distinct results")
             (test-assert (not (search "INTERNAL CRASH INSTRUCTIONS"
                                      (format nil "~{~A~}" (coerce (clinedi:frame-snapshot-rows last-frame) 'list))))
                          "render safe recovery status without raw crash instructions")))))))
  nil)

(-> test-cli-startup-failure-line () null)
(defun test-cli-startup-failure-line ()
  "Test a failed start reports one marked line, styled only for a styled terminal."
  (dolist (styled-p '(nil t))
    (test-call-with-function-replacements
     (list (list 'main--output-styled-p
                 (lambda (stream)
                   (declare (ignore stream))
                   styled-p)))
     (lambda ()
       (let ((line     (main--startup-failure-line "World server response timeout" *error-output*))
             (expected "! Autolith could not start: World server response timeout")
             (sequence (terminal-style-sequence ':failure)))
         (if (and styled-p sequence)
             (test-assert (string= line (concatenate 'string sequence expected *terminal-style-reset*))
                          "a styled terminal shows the whole line in the failure style")
             (test-assert (string= line expected)
                          "other output shows the plain marked line"))))))
  nil)
