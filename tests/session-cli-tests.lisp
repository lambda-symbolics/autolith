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
           (dolist (designator '("builder" "BUILDER" "@builder" "al-b6uouHJ"))
             (select designator "b6uouHJ"))
           (reject "@nobody" '("No participant"))
           (world-participant-park configuration "al-b6uouHJ")
           (reject "builder" '("parked")))))))
  nil)


;;;; -- Startup Dispatch --

(-> test-cli-detached-dispatch () null)
(defun test-cli-detached-dispatch ()
  "Exercise detached world-server and rescue starts, receipts, and rejected options."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((identifier  "b6uouHJ")
           (record      (list #p"unused.sexp" :daemon-endpoint
                              :session-id identifier :pid 424242))
           (live-p      nil)
           (spawned     nil)
           (opened      nil)
           (attached    nil)
           (operations  nil)
           (spawn-error nil)
           (invalid-p   nil))
      (labels ((status ()
                 "Return an authenticated status payload for the fake endpoint."
                 (list :localgroup-status :session-id identifier
                       :conversation-id   (if invalid-p "b6uouHK" identifier)
                       :pid 424242 :cwd "workspace"))

               (reject (arguments condition-type)
                 "Assert a rejected invocation never enters direct application startup."
                 (test-assert
                  (handler-case
                      (progn (main-dispatch arguments) nil)
                    (error (condition) (typep condition condition-type)))
                  (format nil "~S signals ~A" arguments condition-type))))
        (let ((*standard-output* (make-string-output-stream)))
          (test-call-with-function-replacements
           (list
            (list 'configuration-create
                  (lambda (&rest arguments) (declare (ignore arguments)) configuration))
            (list 'localgroup-statuses
                  (lambda (configuration)
                    (declare (ignore configuration))
                    (when live-p (list (status)))))
            (list 'localgroup-endpoint-records
                  (lambda (configuration)
                    (declare (ignore configuration))
                    (when (or live-p spawned opened) (list record))))
            (list 'world-client-open
                  (lambda (configuration &rest arguments)
                    (declare (ignore configuration))
                    (when spawn-error (error spawn-error))
                    (push arguments opened)
                    (values identifier t)))
            (list 'localgroup-handoff-spawn-fresh
                  (lambda (configuration &rest arguments)
                    (declare (ignore configuration))
                    (when spawn-error (error spawn-error))
                    (push arguments spawned)
                    (when (getf arguments :ready-function)
                      (funcall (getf arguments :ready-function) identifier))
                    identifier))
            (list 'localgroup-query-record
                  (lambda (entry operation &optional arguments)
                    (declare (ignore entry arguments))
                    (push operation operations)
                    (list :ok :status (status))))
            (list 'channel-view-run
                  (lambda (view &rest arguments)
                    (declare (ignore arguments))
                    (push (channel-subscription-identifier view) attached)
                    nil))
            (list 'localgroup-relay-exit
                  (lambda (&rest arguments) (declare (ignore arguments))))
            (list 'main--interactive-input-p (lambda () t))
            (list 'main--connect-application
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (test-assert nil "explicit detached startup never falls back to direct")
                    (error 'configuration-error :message "Unexpected direct startup."))))
           (lambda ()
             (let ((*main-direct-start-p* t))
               (main-dispatch '("--detached" "--pristine" "--permissions" "full"))
               (test-assert
                (and (= (length spawned) 1)
                     (null (getf (first spawned) :attach-expected-p))
                     (getf (first spawned) :pristine-p)
                     (getf (first spawned) :immutable-p)
                     (eq (getf (first spawned) :permission-mode) ':full-access)
                     (null attached))
                "a pristine detach is a rescue process that gets pristine and permissions without attaching")
               (main-dispatch '("--detached" "--permissions" "full"))
               (test-assert
                (and (= (length spawned) 1)
                     (= (length opened) 1)
                     (getf (first opened) :new-p)
                     (null (getf (first opened) :channel))
                     (eq (getf (getf (first opened) :launch) :permission-mode) ':full-access)
                     (null attached))
                "a detach opens a new channel in the world server with its launch options, without attaching")
               (let ((output (get-output-stream-string *standard-output*)))
                 (test-assert
                  (and (search "b-6uouHJ" output) (search "424242" output))
                  "the receipt includes the full displayed ID and published PID"))
               (setf live-p t operations nil)
               (reject '("open" "b-6" "--detached") 'configuration-error)
               (reject '("--detached" "open" "b-6") 'configuration-error)
               (test-assert (and (= (length spawned) 1) (null attached) (null operations))
                            "detached open fails before querying or attaching a running channel")
               (let ((styled (with-output-to-string (stream)
                               (main--print-detached-receipt record :stream stream :styled-p t))))
                 (test-assert (and (find #\Escape styled) (search "424242" styled))
                              "a styled receipt uses terminal styles and the actual PID"))
               (dolist (arguments '(("open" "--detached")
                                    ("open" "" "--detached")
                                    ("open" "no-such-id" "--detached")
                                    ("open" "../b" "--detached")
                                    ("--detached" "--image" "image.png")
                                    ("--detached" "--simulate-crash")
                                    ("--detached" "auth")
                                    ("open" "b" "--detached" "--immutable")))
                 (reject arguments 'configuration-error))
               (test-assert (and (= (length spawned) 1) (= (length opened) 1))
                            "invalid options do not launch sessions")
               (setf live-p nil
                     spawned nil
                     opened nil
                     spawn-error (make-condition 'platform-capability-unavailable
                                                 :capability ':detached-sessions
                                                 :message    "Unavailable for test."))
               (reject '("--detached" "--pristine") 'platform-capability-unavailable)
               (setf spawn-error (make-condition 'localgroup-error
                                                 :operation ':spawn :message "Launch failed."))
               (reject '("--detached" "--pristine") 'localgroup-error)
               (reject '("--detached") 'localgroup-error)
               (setf spawn-error nil
                     invalid-p t)
               (reject '("--detached") 'localgroup-error))))))))
  nil)

(-> test-cli-detached-receipt-cleanup () null)
(defun test-cli-detached-receipt-cleanup ()
  "Cancel a ready rescue launch if endpoint discovery or receipt authentication fails."
  (dolist (failure '(:lookup :status))
    (with-test-configuration (configuration)
      (configuration-ensure-directories configuration)
      (let* ((process           (list ':test-process))
             (ticket            nil)
             (ready-p           nil)
             (stopped-p         nil)
             (released-p        nil)
             (failure-condition (make-condition 'localgroup-error
                                                :operation ':status
                                                :message   "Endpoint unavailable.")))
        (let ((*localgroup-fresh-launch-function*
                (lambda (configuration &key handoff-pathname &allow-other-keys)
                  (declare (ignore configuration))
                  (setf ticket handoff-pathname)
                  process))
              (*localgroup-fresh-wait-function*
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (setf ready-p t)
                  "b6uouHJ"))
              (*localgroup-handoff-stop-function*
                (lambda (actual-process pathname)
                  (test-assert (and (eq process actual-process)
                                    (equal pathname ticket)
                                    (probe-file pathname))
                               "failed startup retains its process and ticket for cancellation")
                  (setf stopped-p t))))
          (test-call-with-function-replacements
           (list
            (list 'localgroup--find-record
                  (lambda (configuration identifier)
                    (declare (ignore configuration identifier))
                    (when (eq failure ':lookup) (error failure-condition))
                    (list #p"unused.sexp" :daemon-endpoint :session-id "b6uouHJ" :pid 42)))
            (list 'localgroup-query-record
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (error failure-condition)))
            (list 'platform-release-process
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (setf released-p t))))
           (lambda ()
             (test-assert
              (handler-case
                  (progn (main--start-detached configuration :permission-mode ':ask :pristine-p t) nil)
                (localgroup-error (condition) (eq condition failure-condition)))
              (format nil "~A receipt failure propagates its original condition" failure))
             (test-assert (and ready-p stopped-p (not released-p)
                               ticket (not (probe-file ticket)))
                          "failed receipt cancels the ready process and deletes its launch ticket")))))))
  nil)


;;;; -- Saved-Core Process Boundary --

(-> session-cli-tests--call-with-roots (pathname function) t)
(defun session-cli-tests--call-with-roots (root function)
  "Call FUNCTION with isolated CLI roots and the saved active core."
  (test-call-with-environment
   (append
    (loop for (variable directory) in '(("XDG_CONFIG_HOME" "config/")
                                        ("XDG_DATA_HOME" "data/")
                                        ("XDG_STATE_HOME" "state/")
                                        ("XDG_CACHE_HOME" "cache/"))
          collect (list variable (namestring (merge-pathnames directory root))))
    (list (list "AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE"))
          (list "AUTOLITH_SOURCE_ROOT" (namestring (asdf:system-source-directory :autolith)))
          (list "AUTOLITH_SBCL" (lisp-worker-sbcl-command))
          (list "AUTOLITH_SITE_CONFIG_ROOT" nil)
          (list "AUTOLITH_RECOVERED" nil)))
   function))

(-> test-cli-detached-process () null)
(defun test-cli-detached-process ()
  "Launch a real detached channel and reject detached opens without changing its owner."
  (with-platform-capability (':detached-sessions "explicit detached CLI process lifecycle")
    (with-test-configuration (unused root)
      (declare (ignore unused))
      (session-cli-tests--call-with-roots
       root
       (lambda ()
         (let* ((configuration (configuration-create :defer-provider-validation-p t))
                (command       (platform-session-launch-command
                                *platform* (config :source-root configuration)))
                (records       nil))
           (configuration-ensure-directories configuration)
           (with-open-file (stream (configuration-user-init-path configuration)
                                   :direction ':output :if-exists ':supersede
                                   :external-format ':utf-8)
             (write-string "(setf (config :onboarding-p) nil
      (config :telemetry-enabled-p) nil
      *localgroup-first-attach-timeout-seconds* 0.2)" stream))
           (labels ((run (arguments)
                      "Invoke the real CLI with closed stdin and capture its result."
                      (uiop:run-program (append command arguments)
                                        :directory root :input nil :output ':string
                                        :error-output ':string :ignore-error-status t)))
             (unwind-protect
                  (multiple-value-bind (output diagnostics status) (run '("--detached"))
                    (setf records (localgroup-endpoint-records configuration))
                    (test-assert (and (zerop status) (= (length records) 1))
                                 (format nil "non-TTY detached startup is ready: ~A ~A" output diagnostics))
                    (when records
                      (let* ((record   (first records))
                             (snapshot (getf (rest (localgroup-query-record record ':status)) :status))
                             (id       (getf (rest snapshot) :conversation-id))
                             (pid      (getf (rest snapshot) :pid)))
                        (test-assert
                         (and (search (conversation-identifier-display id) output)
                              (search (princ-to-string pid) output)
                              (/= pid (current-process-id))
                              (platform-process-alive-p *platform* pid))
                         "the receipt identifies the running localgroup process, not its launcher")
                        (sleep 0.7)
                        (test-assert
                         (equal id (getf (rest (getf (rest (localgroup-query-record record ':status))
                                                    :status)) :conversation-id))
                         "an unattended session survives its first-attach deadline")
                        (multiple-value-bind (resumed errors resume-status)
                            (run (list "open" (conversation-identifier-display id) "--detached"))
                          (declare (ignore resumed))
                          (test-assert
                           (and (= resume-status 64) (search "--detached" errors)
                                (platform-process-alive-p *platform* pid)
                                (= (length (localgroup-endpoint-records configuration)) 1))
                           "a detached open rejects the flag without replacing the live owner"))
                        (multiple-value-bind (ignored errors missing-status)
                            (run '("open" "--detached"))
                          (declare (ignore ignored))
                          (test-assert (and (= missing-status 64) (search "--detached" errors))
                                       "a detached open without an ID rejects the flag at the process boundary"))
                       (let ((saved (conversation-create configuration :identifier "b6uouHJ")))
                         (conversation-append-user-message saved "Saved resume fixture")
                         (multiple-value-bind (resumed errors resume-status)
                             (run '("open" "b-6uouHJ" "--detached"))
                           (declare (ignore resumed))
                           (setf records (localgroup-endpoint-records configuration))
                           (test-assert
                            (and (= resume-status 64) (search "--detached" errors)
                                 (not (find "b6uouHJ" records :test #'string=
                                            :key                    (lambda (entry)
                                                                      (localgroup--record-session-id (rest entry)))))
                                 (= (length records) 1))
                            "a detached open of saved history does not start another channel"))))))
               ;; The channels live in the world server the first start launched.
               (let ((world (world-client-entry configuration)))
                 (when world
                   ; mallet:suppress no-ignore-errors -- cleanup a server which may have exited already
                   (ignore-errors (localgroup-query-record world ':stop))))
               (dolist (record (or records (localgroup-endpoint-records configuration)))
                 (let ((pid (getf (rest (rest record)) :pid)))
                   (unless (task-tests--wait-until
                            (lambda () (not (platform-process-alive-p *platform* pid))) 5)
                     (platform-terminate-process *platform* pid :force t)))))))))))
  nil)

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
              (lambda (view &key attached-function notice client-version boot-p)
                (declare (ignore notice client-version boot-p))
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
           (test-assert (and (= welcomed 1) (null opened) (null attached)
                             (null queried) (not direct-p))
                        "a bare interactive launch neither opens nor attaches a channel")
           (main-dispatch (list "open" identifier))
           (main-dispatch (list "open" identifier))
           (test-assert (and (= (length opened) 2)
                             (string= (getf (first opened) :channel) identifier)
                             (= (length attached) 2))
                        "resuming a conversation opens that channel in the server")
           (main-dispatch '("open"))
           (test-assert (and (= (length opened) 3)
                             (null (getf (first opened) :channel))
                             (getf (first opened) :pick-channel-p))
                        "a bare resume opens a channel that picks the conversation itself")
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
                          "read-only open observes an explicit cold channel or the picker result"))
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
           (main-dispatch (list "open" identifier "--immutable" "--fullscreen"))
           (test-assert (equal (getf (first opened) :launch) '(:immutable-p t :fullscreen-p t))
                        "the options a start states explicitly travel with the open")
           (setf opened nil)
           (main-dispatch (list "open" identifier "--model" "model" "--permissions" "full"
                                "--fullscreen"))
           (let ((launch (getf (first opened) :launch)))
             (test-assert (and (equal (getf launch :model) "model")
                               (eq (getf launch :permission-mode) ':full-access)
                               (eq (getf launch :fullscreen-p) t))
                          "explicit live opens forward the same launch options as plain starts"))
           (setf opened nil)
           (main-dispatch '())
           (test-assert (and (null opened) (= welcomed 2))
                        "a later bare start returns to welcome rather than selecting the last channel")
           (setf opened nil attached nil queried nil started-p t
                 attach-error (make-condition 'localgroup-error :operation ':attach
                               :message                                    "The attachment was rejected.")
                 welcome-events
                 (list (list ':switch :session-id identifier)
                       (lambda (arguments)
                         (test-assert (search "rejected" (getf arguments :notice))
                                      "a refused welcome attachment returns with its diagnostic")
                         (setf attach-error nil)
                         (list ':switch :new-p t))))
           (main-dispatch '())
           (test-assert (and (= (length opened) 2) (= (length attached) 2)
                             (getf (first opened) :new-p)
                             (every (lambda (arguments)
                                      (equal (getf arguments :directory) (namestring (uiop:getcwd))))
                                    opened)
                             (equal queried '((:close t))))
                        "welcome cleans up a refused new attachment and creates a channel on explicit retry in the original directory")
           (setf opened nil attached nil)
           (test-call-with-function-replacements
            (list (list 'application-recovery-state (lambda (candidate)
                                                     (declare (ignore candidate)) (values identifier nil)))
                  (list 'application-recovery-diagnosis-prompt (lambda (candidate)
                                                                (declare (ignore candidate)) "Internal crash instructions")))
            (lambda ()
              (main-dispatch '())
              (test-assert (and (null opened) (null attached))
                           "implicit recovery supplies welcome information without selecting a channel"))))))))
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
