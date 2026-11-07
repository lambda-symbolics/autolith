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
             (reject "z" (mapcar #'conversation-identifier-display identifiers))))))))
  nil)


;;;; -- Startup Dispatch --

(-> test-cli-detached-dispatch () null)
(defun test-cli-detached-dispatch ()
  "Exercise parsed detached starts in the world server and in a rescue process, live reuse, receipts, and rejected options."
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
            (list 'localgroup-attach-record
                  (lambda (configuration entry mode &key panel)
                    (declare (ignore configuration entry mode panel))
                    (push identifier attached)
                    nil))
            (list 'localgroup-relay-exit
                  (lambda (&rest arguments) (declare (ignore arguments))))
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
               (setf live-p t)
               (main-dispatch '("resume" "b-6" "--detached"))
               (test-assert (and (= (length spawned) 1) (null attached)
                                 (every (lambda (operation) (eq operation ':status)) operations))
                            "detached live reuse only queries status, without spawning or detaching a client")
               (main-dispatch '("resume" "b6"))
               (test-assert (and (= (length spawned) 1) (= (length attached) 1))
                            "ordinary live resume attaches even for a direct start")
               (let ((styled (with-output-to-string (stream)
                               (main--print-detached-receipt record :stream stream :styled-p t))))
                 (test-assert (and (find #\Escape styled) (search "424242" styled))
                              "a styled receipt uses terminal styles and the actual PID"))
               (dolist (arguments '(("resume" "--detached")
                                    ("resume" "" "--detached")
                                    ("resume" "no-such-id" "--detached")
                                    ("resume" "../b" "--detached")
                                    ("--detached" "--image" "image.png")
                                    ("--detached" "--simulate-crash")
                                    ("--detached" "auth")
                                    ("resume" "b" "--detached" "--immutable")
                                    ("resume" "b" "--model" "model")
                                    ("resume" "b" "--permissions" "full")))
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
  "Launch real detached channels in a saved-core world server and verify readiness, PID, persistence, and reuse."
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
                            (run (list "resume" (conversation-identifier-display id) "--detached"))
                          (test-assert
                           (and (zerop resume-status) (search (princ-to-string pid) resumed)
                                (= (length (localgroup-endpoint-records configuration)) 1))
                           (format nil "live resume reuses the actual PID: ~A ~A" resumed errors)))
                        (multiple-value-bind (ignored errors missing-status)
                            (run '("resume" "--detached"))
                          (declare (ignore ignored))
                          (test-assert (and (= missing-status 64) (search "conversation ID" errors))
                                       "a detached resume without an ID fails at the process boundary"))
                       (let ((saved (conversation-create configuration :identifier "b6uouHJ")))
                         (conversation-append-user-message saved "Saved resume fixture")
                         (multiple-value-bind (resumed errors resume-status)
                             (run '("resume" "b-6uouHJ" "--detached"))
                           (setf records (localgroup-endpoint-records configuration))
                           (let* ((entry (main--live-conversation-record configuration "b6uouHJ"))
                                  (pid   (and entry (getf (rest (rest entry)) :pid))))
                             (test-assert
                              (and (zerop resume-status) entry (= (length records) 2)
                                   (search "b-6uouHJ" resumed)
                                   (search (princ-to-string pid) resumed))
                              (format nil "saved detached resume publishes its original ID and actual PID: ~A ~A"
                                      resumed errors))))))))
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
  "Test a client-first start opening its channel in the world server and attaching."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((identifier   "b6uouHJ")
           (record       (list #p"unused.sexp" :localgroup-endpoint
                               :session-id identifier :pid 424242))
           (opened       nil)
           (attached     nil)
           (queried      nil)
           (open-error   nil)
           (attach-error nil)
           (started-p    t)
           (switch-to    nil)
           (direct-p     nil))
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
        (list 'main--live-conversation-record
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (list 'world-client-open
              (lambda (open-configuration &rest arguments)
                (declare (ignore open-configuration))
                (when open-error (error open-error))
                (push arguments opened)
                (values identifier started-p)))
        (list 'localgroup--find-record
              (lambda (find-configuration session-id)
                (declare (ignore find-configuration))
                (test-assert (member session-id (list identifier "c6uouHJ") :test #'string=)
                             "the client attaches to the channel the server opened")
                record))
        (list 'localgroup-attach-record
              (lambda (attach-configuration entry mode &key panel)
                (declare (ignore attach-configuration panel))
                (push (list (rest entry) mode) attached)
                (when attach-error (error attach-error))
                (when switch-to
                  (list :switch :session-id (shiftf switch-to nil)))))
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
           (let ((arguments (first opened)))
             (test-assert (and (= (length opened) 1)
                               (equal (getf arguments :directory)
                                      (config :working-directory configuration))
                               (null (getf arguments :channel))
                               (member (getf arguments :permission-mode)
                                       '(:ask :auto :sandboxed :full-access))
                               (not (getf arguments :resume-command-p))
                               (= (length attached) 1)
                               (eq (second (first attached)) ':control)
                               (null queried)
                               (not direct-p))
                          "a plain start opens a channel for its directory and attaches to it"))
           (main-dispatch (list "resume" identifier))
           (test-assert (and (= (length opened) 2)
                             (string= (getf (first opened) :channel) identifier)
                             (= (length attached) 2))
                        "resuming a conversation opens that channel in the server")
           (main-dispatch '("resume"))
           (test-assert (and (= (length opened) 3)
                             (null (getf (first opened) :channel))
                             (getf (first opened) :resume-command-p))
                        "a bare resume opens a channel that picks the conversation itself")
           (setf attach-error (make-condition 'localgroup-error
                                              :operation ':attach
                                              :message   "The attachment was rejected."))
           (test-assert (handler-case
                            (progn (main-dispatch '()) nil)
                          (localgroup-error (condition)
                            (and (search "rejected" (autolith-error-message condition))
                                 (equal queried '((:close t))))))
                        "a failed attach asks the server to close the channel started for this terminal while nobody controls it")
           (setf started-p nil
                 queried   nil)
           (test-assert (handler-case
                            (progn (main-dispatch '()) nil)
                          (localgroup-error (condition)
                            (and (search "rejected" (autolith-error-message condition))
                                 (null queried))))
                        "a failed attach leaves a channel the server already served")
           (setf attach-error nil
                 open-error   (make-condition 'localgroup-error
                                              :operation ':spawn
                                              :message   "No world server could start."))
           (test-assert (handler-case
                            (progn (main-dispatch '()) nil)
                          (localgroup-error (condition)
                            (and (not direct-p)
                                 (search "The world server is not available: No world server could start"
                                         (autolith-error-message condition)))))
                        "a failed open ends the start with its reason instead of starting directly")
           (setf open-error nil
                 opened     nil
                 attached   nil
                 switch-to  "c6uouHJ")
           (main-dispatch '())
           (test-assert (and (= (length opened) 2)
                             (null (getf (second opened) :channel))
                             (string= (getf (first opened) :channel) "c6uouHJ")
                             (= (length attached) 2)
                             (null switch-to))
                        "a switch request opens the named channel and attaches to it")
           (setf opened nil)
           (main-dispatch '("--immutable" "--fullscreen"))
           (test-assert (equal (getf (first opened) :launch) '(:immutable-p t :fullscreen-p t))
                        "the options a start states explicitly travel with the open")
           (setf opened nil)
           (main-dispatch '())
           (test-assert (null (getf (first opened) :launch))
                        "a start that states nothing sends no launch options"))))))
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
