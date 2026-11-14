(in-package #:autolith)

;;;; -- Localgroup Process Handoff Tests --

(-> test-localgroup--relay-application
    (configuration &key (:persisted-p boolean) (:main-thread sb-thread:thread))
    (values application application-input-controller localgroup-terminal conversation))
(defun test-localgroup--relay-application
    (configuration &key persisted-p (main-thread (current-thread)))
  "Return a leased application and terminal relay owned by MAIN-THREAD."
  (let* ((direct
           (stream-terminal-create
            :input-stream          (make-string-input-stream "")
            :output-stream         (make-broadcast-stream)
            :input-file-descriptor 0
            :rows                  24
            :columns               80))
         (relay        (localgroup-terminal-create :direct-terminal direct))
         (conversation (conversation-create configuration))
         (ui           (terminal-ui-create :terminal relay))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :main-thread main-thread)))
    (when persisted-p
      (conversation-append-user-message conversation "persisted"))
    (setf (application-input-controller application) controller
          (application-conversation-lease application)
          (conversation-lease-acquire
           configuration (conversation-identifier conversation)))
    (values application controller relay conversation)))

(-> test-localgroup-handoff-site-arguments () null)
(defun test-localgroup-handoff-site-arguments ()
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
               (configuration-set configuration (configuration-setting configuration :fullscreen-p)
                                  enabled-p :source ':override)
               (let ((arguments
                       (localgroup-handoff--arguments
                        configuration handoff-pathname
                        :permission-argument "auto"
                        :immutable-p         enabled-p
                        :pristine-p          enabled-p)))
                 (test-assert
                  (equal arguments
                         (append
                          (platform-session-launch-command
                             *platform* (config :source-root configuration))
                          (list "--permissions" "auto")
                          (if enabled-p
                              (list "--immutable" "--pristine" "--fullscreen")
                              (list "--plain"))
                          (list "--site-config-root"
                                (namestring
                                   (config :site-config-root configuration))
                                "--localgroup-handoff"
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

(-> test-localgroup--abandonment-fixture
    (configuration &key (:persisted-p boolean) (:linger-p boolean))
    (values application application-input-controller localgroup-terminal
            localgroup-session))
(defun test-localgroup--abandonment-fixture (configuration &key persisted-p linger-p)
  "Return a started relay APPLICATION whose foreground terminal was released."
  (configuration-ensure-directories configuration)
  (multiple-value-bind (application controller relay)
      (test-localgroup--relay-application configuration :persisted-p persisted-p)
    (let ((session (localgroup-start application :linger-p linger-p)))
      (image-daemon:relay-release-direct relay)
      (values application controller relay session))))

(-> test-localgroup-lingering-session () null)
(defun test-localgroup-lingering-session ()
  "Test a lingering session surviving lost clients and checkpoint restarts."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller relay session)
            (test-localgroup--abandonment-fixture configuration :persisted-p t :linger-p t)
          (declare (ignore relay))
          (setf application fixture-application
                controller fixture-controller)
          (application-input-controller--enqueue controller ':message "queued input")
          (localgroup--note-controller-attached session)
          (test-assert
           (and (not (localgroup--controller-lost session))
                (null (application-input-controller-exit-reason controller)))
           "a lingering session outlives a client that vanished without detaching")
          (application-call-with-localgroup-quiesced application (lambda () nil))
          (let ((restarted (application-localgroup-session application)))
            (test-assert
             (and restarted
                  (not (eq restarted session))
                  (localgroup-session-linger-p restarted)
                  (not (localgroup--controller-lost restarted))
                  (null (application-input-controller-exit-reason controller)))
             "a checkpoint restart keeps the session lingering")))
      (test-localgroup--abandonment-cleanup application controller root)))
  nil)

(-> test-localgroup--abandonment-cleanup
    ((option application) (option application-input-controller) pathname)
    null)
(defun test-localgroup--abandonment-cleanup (application controller root)
  "Stop APPLICATION's endpoint and CONTROLLER, then delete ROOT."
  (when application
    (localgroup-stop application)
    (application-release-conversation-lease application))
  (when controller
    (application-input-controller-stop controller))
  (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)
  nil)

(-> test-localgroup-abandoned-session-exit () null)
(defun test-localgroup-abandoned-session-exit ()
  "Test sessions exiting when their client vanishes without an explicit detach."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller relay session)
            (test-localgroup--abandonment-fixture configuration :persisted-p t)
          (declare (ignore relay))
          (setf application fixture-application
                controller fixture-controller)
          (setf (application-goal application)
                (list :objective "keep working" :status ':active :continuations 0))
          (application-input-controller--enqueue controller ':message "queued input")
          (test-assert
           (and (localgroup--controller-lost session)
                (eq (application-input-controller-exit-reason controller)
                    ':localgroup-abandoned))
           "losing the client without a detach exits a persisted busy session"))
      (test-localgroup--abandonment-cleanup application controller root)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil)
         (socket        nil)
         (attachment    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller relay session)
            (test-localgroup--abandonment-fixture configuration)
          (setf application fixture-application
                controller fixture-controller
                socket
                  (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol
                                 ':tcp)
                attachment
                  (make-instance 'image-daemon:attachment :socket socket :stream
                                 (make-string-output-stream) :mode ':control))
          (test-assert
           (and
            (image-daemon:relay-attach relay attachment :rows 24 :columns 80 :styled-p
             nil :session-id "ABANDON")
            (eq (image-daemon:relay-attachment-kind relay) ':remote))
           "a controlling attachment owns the released relay")
          (localgroup--note-controller-attached session)
          (localgroup--mark-explicit-detach session)
          (image-daemon:relay-release-control relay)
          (test-assert
           (and
            (with-lock-held ((image-daemon:daemon-runtime-lock session))
              (localgroup-session-detached-explicitly-p session))
            (eq (image-daemon:relay-attachment-kind relay) ':detached)
            (not (localgroup--controller-lost session))
            (null (application-input-controller-exit-reason controller)))
           "an explicit detach lets a client-less session linger")
          (localgroup--note-controller-attached session)
          (test-assert
           (and
            (not
             (with-lock-held ((image-daemon:daemon-runtime-lock session))
               (localgroup-session-detached-explicitly-p session)))
            (localgroup--controller-lost session)
            (eq (application-input-controller-exit-reason controller)
                ':localgroup-abandoned))
           "a later client consumes the detach so its loss exits the session"))
      (when attachment (image-daemon:attachment-close attachment))
      (when socket (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (test-localgroup--abandonment-cleanup application controller root)))
  (test-assert
   (and (localgroup--initially-detached-p '(:mode :detach))
        (not (localgroup--initially-detached-p '(:mode :detach :attach-expected-p t)))
        (not (localgroup--initially-detached-p '(:mode :take-over)))
        (not (localgroup--initially-detached-p nil))
        (localgroup--attach-expected-p '(:mode :detach :attach-expected-p t))
        (localgroup--attach-expected-p '(:mode :take-over))
        (not (localgroup--attach-expected-p '(:mode :detach))))
   "only a detach handoff starts deliberately detached")
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller relay session)
            (test-localgroup--abandonment-fixture configuration)
          (declare (ignore relay))
          (setf application fixture-application
                controller fixture-controller)
          (let ((*localgroup-first-attach-timeout-seconds* 0))
            (localgroup--attach-watchdog session))
          (test-assert
           (eq (application-input-controller-exit-reason controller)
               ':localgroup-abandoned)
           "a launch nobody attaches to exits after the first-attach timeout"))
      (test-localgroup--abandonment-cleanup application controller root)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (controller    nil))
    (unwind-protect
        (multiple-value-bind (fixture-application fixture-controller relay session)
            (test-localgroup--abandonment-fixture configuration)
          (declare (ignore relay))
          (setf application fixture-application
                controller fixture-controller)
          (localgroup--note-controller-attached session)
          (let ((*localgroup-first-attach-timeout-seconds* 0))
            (localgroup--attach-watchdog session))
          (test-assert (null (application-input-controller-exit-reason controller))
           "the watchdog leaves a session alone once a client has attached"))
      (test-localgroup--abandonment-cleanup application controller root)))
  nil)

(-> test-localgroup-client-first-resume () null)
(defun test-localgroup-client-first-resume ()
  "Test which starts open a channel in the world server and which run their session directly."
  (with-platform-capability (':detached-sessions "client-first starts")
    (dolist (case '((()                                                   t
                     "an ordinary start, a resume or crash recovery is a client of the world server")
                    ((:handoff-record (:localgroup-handoff :version 1)) nil
                     "the spawned session itself runs direct")
                    ((:pristine-p t)                                       nil
                     "a pristine rescue session runs apart from the world server")
                    ((:simulate-crash-p t)                                 nil
                     "a simulated crash runs direct")))
      (destructuring-bind (arguments expected label) case
        (test-assert (eq (apply #'main--client-session-p arguments) expected) label))))
  nil)

(-> test-localgroup-fresh-session-spawn () null)
(defun test-localgroup-fresh-session-spawn ()
  "Test client-first spawn records, launch options, and start-failure cleanup."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (launches      nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (let* ((*localgroup-fresh-launch-function*
                    (lambda (launch-configuration &key session-id handoff-pathname
                                                       permission-argument immutable-p pristine-p)
                      (declare (ignore launch-configuration pristine-p))
                      (push (list session-id handoff-pathname permission-argument
                                  immutable-p)
                            launches)))
                  (*localgroup-fresh-wait-function*
                    (lambda (configuration token old-pid)
                      (declare (ignore configuration token old-pid))
                      "active-conversation"))
                  (session-id
                    (localgroup-handoff-spawn-fresh
                     configuration
                     :permission-mode    ':sandboxed
                     :immutable-p        t
                     :recovery-diagnosis "diagnose the crash")))
             (destructuring-bind (launched-id pathname permission immutable-p)
                 (first launches)
               (test-assert (and (= (length launches) 1)
                                 (string= session-id "active-conversation")
                                 (not (string= launched-id session-id))
                                 (string= permission "sandbox")
                                 immutable-p)
                            "a fresh spawn launches one replacement with its options")
               (let ((record (localgroup-handoff--read configuration pathname)))
                 (test-assert
                  (and (eq (getf (rest record) :state) ':pending)
                       (getf (rest record) :fresh-conversation-p)
                       (null (getf (rest record) :conversation-id))
                       (string= (getf (rest record) :recovery-diagnosis)
                                "diagnose the crash")
                       (typep (getf (rest record) :rows) '(integer 1))
                       (typep (getf (rest record) :columns) '(integer 1))
                       (typep (getf (rest record) :styled-p) 'boolean)
                       (null (getf (rest record) :session-id))
                       (string= (getf (rest record) :launch-id) launched-id))
                  "a fresh spawn records terminal presentation and session state")))
             (localgroup-handoff-spawn-fresh configuration
                                             :conversation-id   "open-id"
                                             :attach-expected-p nil)
             (let ((record (localgroup-handoff--read
                            configuration (second (first launches)))))
               (test-assert
                (null (getf (rest record) :attach-expected-p))
                "an explicit detached spawn disables the first-attach watchdog")))
           (let* ((*localgroup-startup-record*
                    '(:localgroup-handoff
                      :rows 41 :columns 93 :styled-p t))
                  (terminal (localgroup-terminal-create)))
             (test-assert
              (and (= (terminal-rows terminal) 41)
                   (= (terminal-columns terminal) 93)
                   (terminal-styled-p terminal))
              "a detached startup inherits dimensions and styling before attachment"))
           (let ((*localgroup-fresh-launch-function*
                   (lambda (&rest arguments)
                     (declare (ignore arguments))))
                 (*localgroup-fresh-wait-function*
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     nil)))
             (test-assert
              (handler-case
                  (progn (localgroup-handoff-spawn-fresh configuration) nil)
                (localgroup-error (condition)
                  (search "did not start"
                          (autolith-error-message condition))))
              "a replacement that never starts signals instead of attaching")
             (test-assert
              (= (length (directory
                          (merge-pathnames
                           "*.sexp"
                           (localgroup-handoff-directory configuration))))
                 2)
              "a failed fresh spawn removes its own pending record")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-conversation-identity () null)
(defun test-localgroup-conversation-identity ()
  "Test active IDs through resume selection, new, fork, reconnect and conflicts."
  (with-test-configuration (configuration)
   (let ((application nil) (controller nil) (relay nil) (stream nil) (session nil))
     (unwind-protect
         (progn
          (multiple-value-setq (application controller relay)
            (test-localgroup--relay-application configuration))
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
            (terminal-start relay)
            (setf session (localgroup-start application))
            (labels ((assert-identity (conversation)
                       (let* ((identifier (conversation-identifier conversation))
                              (status     (localgroup-status-snapshot session))
                              (entry      (localgroup--find-record configuration identifier)))
                         (test-assert
                          (and
                           (string= (image-daemon:daemon-runtime-identifier session)
                                    identifier)
                           (string= (getf (rest status) :session-id) identifier)
                           (string= (getf (rest status) :conversation-id) identifier)
                           (string= (getf (rest (rest entry)) :session-id) identifier)
                           (= (length (localgroup-endpoint-records configuration)) 1))
                          "one endpoint names the exact active conversation"))))
              (assert-identity first)
              (test-assert (not (conversation-persisted-p first))
               "publishing a fresh endpoint does not persist unused conversation data")
              (test-assert
               (string=
                (localgroup-handoff--wait-for-fresh configuration
                                                    (image-daemon:daemon-runtime-token
                                                     session)
                                                    0)
                (conversation-identifier first))
               "client-first readiness discovers the actual conversation by its capability")
              (let ((record (localgroup--registry-record session)))
                (test-assert
                 (and
                  (null
                   (localgroup-handoff--ready-conversation-id record "wrong-token" 0))
                  (null
                   (localgroup-handoff--ready-conversation-id record
                                                              (image-daemon:daemon-runtime-token
                                                               session)
                                                              (sb-posix:getpid))))
                 "readiness rejects another capability and the launching process"))
              (multiple-value-bind (socket new-stream response)
                  (test-localgroup--attach session ':read-only)
                (declare (ignore socket))
                (setf stream new-stream)
                (test-assert (eq (first response) ':attached)
                 "the initial picker conversation accepts a relay attachment"))
              (conversation-append-user-message first "source")
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
                (let ((fork
                       (conversation-fork configuration (conversation-identifier first))))
                  (application-install-conversation application fork)
                  (assert-identity fork))
                (test-assert
                 (and (eq session (application-localgroup-session application))
                      (= port (image-daemon:daemon-runtime-port session))
                      (string= token (image-daemon:daemon-runtime-token session)))
                 "conversation changes preserve the endpoint transport")
                (terminal--write relay "conversation switched")
                (test-assert
                 (loop repeat 30
                       thereis (let ((packet (test-localgroup--read-packet stream)))
                                 (and (eq (first packet) ':output)
                                      (search "conversation switched" (second packet)))))
                 "an attached terminal receives output after conversation changes"))
              (let* ((before     (application-conversation application))
                     (blocked    (conversation-create configuration))
                     (identifier (conversation-identifier blocked))
                     (pathname   (localgroup-registry-pathname configuration identifier))
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
              (close stream)
              (setf stream nil)
              (let ((old-record (localgroup--registry-record session))
                    (pathname   (image-daemon:daemon-runtime-registry-pathname session)))
                (localgroup-stop application)
                (setf session (localgroup-start application))
                (localgroup--remove-stale-record pathname old-record)
                (assert-identity saved))
              (application-call-with-localgroup-quiesced application (lambda () t))
              (setf session (application-localgroup-session application))
              (assert-identity saved))))
       (when stream (ignore-errors (close stream)))
       (when application
         (localgroup-stop application)
         (application-release-conversation-lease application)
         (application-disconnect-task-presentation application)
         (when (slot-boundp application 'tool-registry)
           (tool-registry-close-runtime-state (application-tool-registry application))))
       (when controller (application-input-controller-stop controller))
       (when relay (terminal-stop relay)))))
  nil)

(-> test-localgroup-world-spawn () null)
(defun test-localgroup-world-spawn ()
  "Test a world server launch recording its kind and resolving the world endpoint."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((launched   nil)
           (waited     nil)
           (*localgroup-fresh-launch-function*
             (lambda (launch-configuration &key handoff-pathname &allow-other-keys)
               (declare (ignore launch-configuration))
               (push handoff-pathname launched)))
           (*localgroup-fresh-wait-function*
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (error "A world launch must not wait for a session endpoint.")))
           (*localgroup-world-wait-function*
             (lambda (wait-configuration token old-pid)
               (declare (ignore wait-configuration))
               (setf waited (list token old-pid))
               "world"))
           (identifier (localgroup-handoff-spawn-fresh configuration :world-p t))
           (record     (localgroup-handoff--read configuration (first launched))))
      (test-assert (and (string= identifier "world")
                        (getf (rest record) :world-p)
                        (equal waited (list (getf (rest record) :token) (current-process-id))))
                   "a world launch records its kind and resolves the world endpoint under its token"))
    (let* ((token   (daemon-random-token))
           (runtime (image-daemon:daemon-runtime-create
                     :directory        (localgroup-world-directory configuration)
                     :identifier       "world"
                     :token            token
                     :request-function (lambda (runtime request &key socket stream)
                                         (declare (ignore runtime request socket))
                                         (daemon-write-packet
                                          stream (list :ok :status (list :world-status)))))))
      (image-daemon:daemon-runtime-start runtime)
      (unwind-protect
           (progn
             (test-assert (string= (localgroup-world--wait-for-fresh configuration token 0) "world")
                          "the world wait resolves a published endpoint carrying the launch token")
             (test-assert (string= (let ((*localgroup-handoff-start-timeout-seconds* 0))
                                     (localgroup-world--wait-for-fresh configuration "other-token" 0))
                                   "world")
                          "the world wait takes an answering endpoint under another token: that launch lost the race to it"))
        (image-daemon:daemon-runtime-stop runtime))))
  nil)
