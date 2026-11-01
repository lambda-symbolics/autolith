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
           (dolist (prefix '("b" "b-" "b6" "b-6" "b6uouHJ" "b-6uouHJ"))
             (select prefix "b6uouHJ"))
           (dolist (prefix '("" "B" "b--6" "b6uouHJx" "../b" "*"))
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
                           ("a-b" "abcdef1")
                           ("abcdef12" "abcdef123456")
                           ("ABCDEF12" "abcdef123456")
                           ("ABCDEF123456" "abcdef123456")))
             (select (first case) (second case)))
           (reject "ABC" '("abcdef123456" "abcdef234567"))
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
  "Exercise parsed detached starts, live reuse, receipts, and rejected options."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((identifier  "b6uouHJ")
           (record      (list #p"unused.sexp" :daemon-endpoint
                              :session-id identifier :pid 424242))
           (live-p      nil)
           (spawned     nil)
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
                    (when (or live-p spawned) (list record))))
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
                  (lambda (configuration entry mode)
                    (declare (ignore configuration entry mode))
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
             (with-test-environment (("AUTOLITH_SESSION_STYLE" "direct"))
               (main-dispatch '("--detached" "--pristine" "--permissions" "full"))
               (test-assert
                (and (= (length spawned) 1)
                     (null (getf (first spawned) :attach-expected-p))
                     (getf (first spawned) :pristine-p)
                     (getf (first spawned) :immutable-p)
                     (eq (getf (first spawned) :permission-mode) ':full-access)
                     (null attached))
                "noninteractive explicit detach forwards pristine and permissions without attaching")
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
                            "ordinary live resume attaches even with direct session style")
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
               (test-assert (= (length spawned) 1) "invalid options do not launch sessions")
               (setf live-p nil
                     spawned nil
                     spawn-error (make-condition 'platform-capability-unavailable
                                                 :capability ':detached-sessions
                                                 :message    "Unavailable for test."))
               (reject '("--detached") 'platform-capability-unavailable)
               (setf spawn-error (make-condition 'localgroup-error
                                                 :operation ':spawn :message "Launch failed."))
               (reject '("--detached") 'localgroup-error)
               (setf spawn-error nil
                     invalid-p t)
               (reject '("--detached") 'localgroup-error))))))))
  nil)

(-> test-cli-detached-receipt-cleanup () null)
(defun test-cli-detached-receipt-cleanup ()
  "Cancel a ready launch if endpoint discovery or receipt authentication fails."
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
                  (progn (main--start-detached configuration :permission-mode ':ask) nil)
                (localgroup-error (condition) (eq condition failure-condition)))
              (format nil "~A receipt failure propagates its original condition" failure))
             (test-assert (and ready-p stopped-p (not released-p)
                               ticket (not (probe-file ticket)))
                          "failed receipt cancels the ready process and deletes its launch ticket")))))))
  nil)


;;;; -- Saved-Core Process Boundary --

(-> session-cli-tests--call-with-roots (pathname function) t)
(defun session-cli-tests--call-with-roots (root function)
  "Call FUNCTION with isolated CLI roots, the saved active core, and direct style."
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
          (list "AUTOLITH_SESSION_STYLE" "direct")
          (list "AUTOLITH_SITE_CONFIG_ROOT" nil)
          (list "AUTOLITH_RECOVERED" nil)))
   function))

(-> test-cli-detached-process () null)
(defun test-cli-detached-process ()
  "Launch a real saved-core session and verify readiness, PID, persistence, and reuse."
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
                          (test-assert (and (= missing-status 64) (search "prefix" errors))
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
               (dolist (record (or records (localgroup-endpoint-records configuration)))
                 ; mallet:suppress no-ignore-errors -- cleanup an endpoint which may have exited already
                 (ignore-errors (localgroup-query-record record ':kill))
                 (let ((pid (getf (rest (rest record)) :pid)))
                   (unless (task-tests--wait-until
                            (lambda () (not (platform-process-alive-p *platform* pid))) 5)
                     (platform-terminate-process *platform* pid :force t)))))))))))
  nil)
