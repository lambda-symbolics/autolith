(in-package #:autolith)

;;;; -- World-Terminal Process Handoff Tests --

(-> test-world-terminal--leased-application (configuration &key (:persisted-p boolean))
    (values application application-input-controller conversation))
(defun test-world-terminal--leased-application (configuration &key persisted-p)
  "Return a leased application without a terminal of its own, its controller and its conversation.

With PERSISTED-P its conversation holds a message."
  (multiple-value-bind (application controller)
      (test-world-terminal--application configuration :presented-p t)
    (let ((conversation (application-conversation application)))
      (when persisted-p
        (conversation-append-user-message conversation "persisted"))
      (setf (application-conversation-lease application)
            (conversation-lease-acquire
             configuration (conversation-identifier conversation)))
      (values application controller conversation))))

(-> test-world-startup-site-arguments () null)
(defun test-world-startup-site-arguments ()
  "Test detached replacements retain the configured site root."
  (let* ((site-container
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-handoff-site-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (site-root     (merge-pathnames "site/" site-container))
         (configuration nil)
         (root          nil))
    (unwind-protect
         (progn
           (ensure-directories-exist site-root)
           (setf configuration
                 (test-configuration
                  :site-config-root
                  (uiop:ensure-directory-pathname
                    (platform-truename *platform* site-root)))
                 root (test-configuration-root configuration))
           (let ((handoff-pathname (merge-pathnames "handoff.sexp" root)))
             (dolist (enabled-p '(nil t))
               (let ((arguments
                       (world-startup--arguments
                        configuration handoff-pathname
                        :permission-mode ':auto
                        :immutable-p     enabled-p
                        :pristine-p      enabled-p)))
                 (test-assert
                  (equal arguments
                         (append
                          (platform-session-launch-command
                             *platform* (config :source-root configuration))
                          (list "--permissions" "auto")
                          (when enabled-p
                            (list "--immutable" "--pristine"))
                          (list "--site-config-root"
                                (namestring
                                   (config :site-config-root configuration))
                                "--world-startup"
                                (namestring handoff-pathname))))
                  "detached launch arguments preserve the site root and launch modes")))))
      (when root
        (platform-delete-directory-tree *platform* root
                                        :validate          t
                                        :if-does-not-exist ':ignore))
      (platform-delete-directory-tree *platform* site-container
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-world-terminal--abandonment-fixture
    (configuration &key (:persisted-p boolean))
    (values application application-input-controller channel-endpoint))
(defun test-world-terminal--abandonment-fixture (configuration &key persisted-p)
  "Return a started application without a terminal of its own, its controller and its session."
  (configuration-ensure-directories configuration)
  (multiple-value-bind (application controller)
      (test-world-terminal--leased-application configuration :persisted-p persisted-p)
    (terminal-ui-start (application-ui application))
    (values application controller (world-terminal-start application))))

(-> test-world-terminal-lingering-session () null)
(defun test-world-terminal-lingering-session ()
  "Test a lingering session surviving lost clients and checkpoint restarts."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller session)
            (test-world-terminal--abandonment-fixture configuration :persisted-p t)
          (setf application fixture-application
                controller fixture-controller)
          (application-input-controller--enqueue controller ':message "queued input")
          (world-terminal-client-close (test-world-terminal--present session))
          (test-assert
           (and (test-world-terminal--await-served session)
                (null (application-input-controller-exit-reason controller)))
           "a lingering session outlives a client that vanished without detaching")
          (application-call-with-world-terminal-quiesced application (lambda () nil))
          (let ((restarted (application-channel-endpoint application)))
            (test-assert
             (and restarted
                  (not (eq restarted session))
                  (null (application-input-controller-exit-reason controller)))
             "a checkpoint restart keeps the session lingering")))
      (test-world-terminal--abandonment-cleanup application controller root)))
  nil)

(-> test-world-terminal--abandonment-cleanup
    ((option application) (option application-input-controller) pathname)
    null)
(defun test-world-terminal--abandonment-cleanup (application controller root)
  "Stop APPLICATION's endpoint, UI and CONTROLLER, then delete ROOT."
  (when application
    (world-terminal-stop application)
    (application-release-conversation-lease application))
  (when controller
    (application-input-controller-stop controller))
  (when application
    (terminal-ui-stop (application-ui application)))
  (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)
  nil)

(-> test-world-terminal-conversation-identity () null)
(defun test-world-terminal-conversation-identity ()
  "Test that the endpoint names the active conversation through switches and conflicts."
  (with-test-configuration (configuration)
   (let ((application nil) (controller nil) (connection nil) (await nil) (session nil))
     (unwind-protect
         (progn
          (multiple-value-setq (application controller)
            (test-world-terminal--leased-application configuration))
          (let* ((first    (application-conversation application))
                 (provider (provider-create configuration))
                 (registry (make-instance 'tool-registry))
                 (saved    (conversation-create configuration :identifier "abcdef012345")))
            (setf (application-provider application) provider
                  (application-tool-registry application) registry
                  (application-worker application) nil
                  (application-agent application)
                  (agent-create :configuration configuration :provider provider
                                :conversation first :tool-registry registry :worker
                                nil))
            (terminal-ui-start (application-ui application))
            (setf session (world-terminal-start application))
            (labels ((assert-identity (conversation)
                       (let* ((identifier (conversation-identifier conversation))
                              (status     (world-terminal-status-snapshot session))
                              (entry      (world-terminal--find-record configuration identifier)))
                         (test-assert
                          (and
                           (string= (image-daemon:daemon-runtime-identifier session)
                                    identifier)
                           (string= (getf (rest status) :session-id) identifier)
                           (string= (getf (rest status) :conversation-id) identifier)
                           (string= (getf (rest (rest entry)) :session-id) identifier)
                           (= (length (world-terminal-endpoint-records configuration)) 1))
                          "one endpoint names the exact active conversation"))))
              (assert-identity first)
              (test-assert (not (conversation-persisted-p first))
               "publishing a fresh endpoint does not persist unused conversation data")
              (multiple-value-bind (client seen client-await)
                  (test-world-terminal--present session :watches-p t)
                (declare (ignore seen))
                (setf connection client
                      await      client-await)
                (test-assert (eq (first (world-terminal-client-connection-attached client)) ':attached)
                 "the initial picker conversation accepts a client"))
              (conversation-append-user-message saved "saved")
              (let ((port       (image-daemon:daemon-runtime-port session))
                    (token      (image-daemon:daemon-runtime-token session))
                    (first-path (image-daemon:daemon-runtime-registry-pathname session)))
                (application-resume-conversation application
                                                 (conversation-identifier saved))
                (assert-identity saved)
                (test-assert (not (probe-file first-path))
                 "resume picker selection retires the provisional conversation endpoint")
                (let ((new (conversation-create configuration)))
                  (application-install-conversation application new)
                  (assert-identity new))
                (test-assert
                 (and (eq session (application-channel-endpoint application))
                      (= port (image-daemon:daemon-runtime-port session))
                      (string= token (image-daemon:daemon-runtime-token session)))
                 "conversation changes preserve the endpoint transport")
                (terminal-ui-append-finalized (application-ui application) ':conversation-switched
                                              "conversation switched")
                (test-assert
                 (and (funcall await ':append
                               (lambda (event)
                                 (search "conversation switched" (test-world-terminal--event-text event))))
                      (not (world-terminal-client-connection-closed-p connection)))
                 "an attached client receives output after conversation changes"))
              (let* ((before     (application-conversation application))
                     (blocked    (conversation-create configuration))
                     (identifier (conversation-identifier blocked))
                     (pathname   (world-channel-endpoint-pathname configuration identifier))
                     (record
                      (image-daemon:daemon-registry-record :identifier identifier :pid
                       (sb-posix:getpid) :port 12345 :token
                       (format nil "token-~A" identifier) :created-at
                       (get-universal-time))))
                (snapshot-write pathname record)
                (test-assert
                 (handler-case
                  (progn (application-install-conversation application blocked) nil)
                  (application-runtime-replacement-error (condition)
                    (and (search "already owns this conversation"
                                 (autolith-error-message condition))
                         t)))
                 "a conflicting live registry owner aborts the switch and says why")
                (test-assert
                 (and (eq before (application-conversation application))
                      (equal record (image-daemon:daemon-registry-read pathname)))
                 "failed rekey restores the active conversation and preserves its competitor")
                (conversation-lease-release
                 (conversation-lease-acquire configuration identifier))
                (delete-file pathname)
                (assert-identity before)
                (let ((delete-record (symbol-function 'image-daemon:daemon-registry-delete-matching))
                      (old-pathname  (image-daemon:daemon-runtime-registry-pathname session)))
                  (test-call-with-function-replacements
                   (list
                    (list 'image-daemon:daemon-registry-delete-matching
                          (lambda (target record &key errorp)
                            (if (equal target old-pathname)
                                (error 'file-error :pathname target)
                                (funcall delete-record target record :errorp errorp)))))
                   (lambda ()
                     (test-assert
                      (handler-case
                          (progn (application-install-conversation application blocked) nil)
                        (application-runtime-replacement-error () t))
                      "failed endpoint retirement aborts the conversation transaction")))
                  (test-assert (and (eq before (application-conversation application))
                                    (null (image-daemon:daemon-registry-read pathname)))
                               "failed retirement restores identity and removes its replacement")
                  (assert-identity before))
                (test-call-with-function-replacements
                 (list
                  (list 'application-publish-recovery-session
                        (lambda (ignored)
                          (declare (ignore ignored))
                          (error "Injected failure after endpoint rekey."))))
                 (lambda ()
                   (test-assert
                    (handler-case
                     (progn (application-install-conversation application blocked) nil)
                     (application-runtime-replacement-error nil t))
                    "a failure after successful rekey rolls back the conversation")))
                (test-assert (eq before (application-conversation application))
                 "late failure restores the previous active conversation")
                (assert-identity before))
              (application-resume-conversation application
                                               (conversation-identifier saved))
              (assert-identity saved)
              (world-terminal-client-close (shiftf connection nil))
              (let ((old-record (world-terminal--registry-record session))
                    (pathname   (image-daemon:daemon-runtime-registry-pathname session)))
                (world-terminal-stop application)
                (setf session (world-terminal-start application))
                (world-terminal--remove-stale-record pathname old-record)
                (assert-identity saved))
              (application-call-with-world-terminal-quiesced application (lambda () t))
              (setf session (application-channel-endpoint application))
              (assert-identity saved))))
       (when connection (world-terminal-client-close connection))
       (when application
         (world-terminal-stop application)
         (application-release-conversation-lease application)
         (application-disconnect-task-presentation application)
         (when (slot-boundp application 'tool-registry)
           (tool-registry-close-runtime-state (application-tool-registry application))))
       (when controller (application-input-controller-stop controller))
       (when application (terminal-ui-stop (application-ui application))))))
  nil)

(-> test-world-startup-spawn () null)
(defun test-world-startup-spawn ()
  "Test a world server launch recording its kind and resolving the world endpoint."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((launched nil)
           (waited   nil)
           (process  (make-symbol "TEST-WORLD-STARTUP-PROCESS"))
           (released nil)
           (*world-startup-launch-function*
             (lambda (launch-configuration &key handoff-pathname &allow-other-keys)
               (declare (ignore launch-configuration))
               (push handoff-pathname launched)
               process))
           (*world-startup-wait-function*
             (lambda (wait-configuration token old-pid &key alive-p)
               (declare (ignore wait-configuration alive-p))
               (setf waited (list token old-pid))
               "world")))
      (test-call-with-function-replacements
       (list (list 'platform-release-process
                   (lambda (platform launch-process)
                     (declare (ignore platform))
                     (setf released launch-process))))
       (lambda ()
         (let* ((identifier (world-startup-spawn-fresh configuration))
                (record     (world-startup--read configuration (first launched))))
           (test-assert (and (string= identifier "world")
                             (eq released process)
                             (equal waited (list (getf (rest record) :token) (current-process-id))))
                        "A world launch resolves its endpoint under its token and releases its process.")))))
    (dolist (case '((t nil nil nil t)
                    (nil 11 nil (11) t)
                    (nil nil 22 (22) t)
                    (nil 11 nil (-11) t)
                    (nil nil 22 (-22) t)
                    (nil nil nil nil nil)))
      (destructuring-bind (root-alive-p launcher-pid replacement-pid live-pids expected) case
        (test-call-with-function-replacements
         (list (list 'world-startup--process-object-alive-p
                     (lambda (process) (declare (ignore process)) root-alive-p))
               (list 'image-daemon:handoff-ticket-launcher-pid
                     (lambda (pathname) (declare (ignore pathname)) launcher-pid))
               (list 'image-daemon:handoff-ticket-replacement-pid
                     (lambda (pathname &key validate)
                       (declare (ignore pathname validate)) replacement-pid))
               (list 'world-startup--pid-alive-p
                     (lambda (pid) (not (null (member pid live-pids))))))
         (lambda ()
           (test-assert (eq (world-startup--replacement-alive-p ':process #P"handoff") expected)
                        "Launch ownership includes the process, launcher and replacement groups.")))))
    (let ((probes 0))
      (test-call-with-function-replacements
       (list (list 'image-daemon:daemon-registry-discover
                   (lambda (directory) (declare (ignore directory)) (incf probes) nil)))
       (lambda ()
         (test-assert
          (and (null (world-startup--wait-for-fresh configuration "unpublished" 0
                                                  :alive-p (lambda () (< probes 2))))
               (= probes 2))
          "An unpublished launch ends when its owned processes have exited."))))
    (let* ((token   (daemon-random-token))
           (runtime (image-daemon:daemon-runtime-create
                      :class           'channel-endpoint
                     :directory        (world-endpoint-directory configuration)
                     :identifier       "world"
                     :token            token
                     :request-function (lambda (runtime request &key socket stream)
                                         (declare (ignore runtime request socket))
                                         (daemon-write-packet
                                          stream (list :ok :status (list :world-status)))))))
      (image-daemon:daemon-runtime-start runtime)
      (unwind-protect
           (progn
             (test-assert (string= (world-startup--wait-for-fresh configuration token 0) "world")
                          "the world wait resolves a published endpoint carrying the launch token")
             (let ((discover (symbol-function 'image-daemon:daemon-registry-discover))
                   (probes   0))
               (test-call-with-function-replacements
                (list (list 'world-server-running-p (constantly t))
                      (list 'image-daemon:daemon-registry-discover
                            (lambda (directory)
                              (when (> (incf probes) 2) (funcall discover directory)))))
                (lambda ()
                  (test-assert
                   (and (string= (world-startup--wait-for-fresh
                                  configuration "losing-token" 0 :alive-p (constantly nil))
                                 "world")
                        (= probes 3))
                   "A losing launch joins the concurrent server after it publishes its endpoint."))))
             (test-assert (string= (world-startup--wait-for-fresh configuration "other-token" 0
                                                                 :alive-p (constantly nil))
                                   "world")
                         "the world wait takes an answering endpoint under another token: that launch lost the race to it"))
        (image-daemon:daemon-runtime-stop runtime))))
  nil)
