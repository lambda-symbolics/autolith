(in-package #:autolith)

;;;; -- Localgroup Process Handoff Tests --

(-> test-localgroup--relay-application
    (configuration &key (:persisted-p boolean))
    (values application application-input-controller localgroup-terminal conversation))
(defun test-localgroup--relay-application (configuration &key persisted-p)
  "Return a leased APPLICATION with a foreground localgroup terminal relay."
  (let* ((direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream (make-broadcast-stream)
            :input-file-descriptor 0
            :rows 24
            :columns 80))
         (relay (localgroup-terminal-create direct))
         (conversation (conversation-create configuration))
         (ui (terminal-ui-create :terminal relay))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :main-thread (current-thread))))
    (when persisted-p
      (conversation-append-user-message conversation "persisted"))
    (setf (application-input-controller application) controller
          (application-conversation-lease application)
          (conversation-lease-acquire
           configuration (conversation-identifier conversation)))
    (values application controller relay conversation)))

(-> test-localgroup-handoff-records () null)

(defun test-localgroup-handoff-records ()
  "Test private handoff records, startup identity, drafts, and registry ownership."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-application nil)
         (second-application nil)
         (first-session nil)
         (second-session nil)
         (handoff-pathname nil))
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (multiple-value-bind (application controller relay conversation)
             (test-localgroup--relay-application configuration :persisted-p t)
           (declare (ignore controller relay))
           (setf first-application application
                 first-session (localgroup-start application))
           (terminal-ui-set-input (application-ui application) "draft survives")
           (setf handoff-pathname
                   (localgroup-handoff--write application first-session ':detach))
           (let* ((record (localgroup-handoff--read configuration handoff-pathname))
                  (restored (localgroup-handoff-initial-input record)))
             (test-assert
              (and
               (string= (getf (rest record) :conversation-id)
                        (conversation-identifier conversation))
               (typep restored 'user-message-input)
               (string= (user-message-input-text restored) "draft survives"))
              "handoff records preserve durable conversation identity and draft")
             (setf (getf (rest record) :session-id) "abcdef012345")
             (localgroup-handoff--write-record handoff-pathname record)
             (let ((*localgroup-handoff-setsid-function* (lambda () 0)))
               (localgroup-handoff-begin-startup record))
             (multiple-value-bind (pid-record complete-p)
                 (snapshot-read (localgroup-handoff--pid-pathname handoff-pathname))
               (test-assert
                (and complete-p (probe-file (getf (rest record) :pathname))
                     (= (getf (rest pid-record) :pid) (sb-posix:getpid)))
                "replacement startup acknowledges its detached process identity"))
             (multiple-value-bind (application controller relay conversation)
                 (test-localgroup--relay-application configuration)
               (declare (ignore controller relay conversation))
               (application-release-conversation-lease first-application)
               (application-release-conversation-lease application)
               (let ((identifier (getf (rest record) :conversation-id)))
                 (setf (application-conversation-lease application)
                         (conversation-lease-acquire configuration identifier)
                       (application-conversation application)
                         (conversation-load
                          (conversation-pathname
                           (application-conversation first-application)))))
               (setf second-application application)
               (let ((*localgroup-startup-record* record))
                 (setf second-session (localgroup-start application))
                 (application-call-with-localgroup-quiesced application (lambda () t))
                 (setf second-session (application-localgroup-session application))))))
         (test-assert
          (and
           (string= (image-daemon:daemon-runtime-identifier first-session)
                    (image-daemon:daemon-runtime-identifier second-session))
           (string= (image-daemon:daemon-runtime-token first-session)
                    (image-daemon:daemon-runtime-token second-session))
           (= (image-daemon:daemon-runtime-created-at first-session)
              (image-daemon:daemon-runtime-created-at second-session))
           (not (probe-file handoff-pathname)))
          "replacement startup preserves localgroup identity and consumes its record")
         (localgroup-stop first-application)
         (test-assert
          (equal
           (image-daemon:daemon-registry-read
            (image-daemon:daemon-runtime-registry-pathname second-session))
           (localgroup--registry-record second-session))
          "old shutdown cannot delete a replacement endpoint record"))
      (when first-application
        (localgroup-stop first-application)
        (application-release-conversation-lease first-application))
      (when second-application
        (localgroup-stop second-application)
        (application-release-conversation-lease second-application))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(-> test-localgroup-handoff-scheduling () null)

(defun test-localgroup-handoff-scheduling ()
  "Test foreground handoff admission after queued work and live child jobs."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (relay nil)
         (session nil)
         (socket nil)
         (stream nil))
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (multiple-value-bind (new-application new-controller new-relay conversation)
             (test-localgroup--relay-application configuration)
           (declare (ignore conversation))
           (setf application new-application
                 controller new-controller
                 relay new-relay
                 session (localgroup-start new-application)))
         (application-input-controller--enqueue controller ':message "first")
         (multiple-value-setq (socket stream)
           (multiple-value-bind (new-socket new-stream response)
               (test-localgroup--attach session ':take-over)
             (test-assert
              (and (eq (first response) ':handoff)
                   (eq (image-daemon:relay-attachment-kind relay) ':foreground)
                   (application-localgroup-handoff-pending-p application))
              "foreground take-over schedules process handoff without dropping the terminal")
             (values new-socket new-stream)))
         (let ((status (localgroup-status-snapshot session)))
           (test-assert
            (and (eq (getf (rest status) :state) ':detaching)
                 (not (getf (rest status) :idle-p)))
            "pending handoff is visible and never reported as strict idle"))
         (test-assert
          (equal (application-input-controller--next-work controller)
                 (list ':localgroup-handoff ':take-over))
          "a ready handoff preempts queued follow-up work")
         (test-assert
          (equal (deque->list (application-input-controller-work-items controller))
                 (list (list ':message "first")))
          "preempted follow-up work stays queued for the replacement")
         (application-input-controller--finish-work controller)
         (application-localgroup-request-handoff application ':take-over)
         (let ((orchestrator
                (make-instance 'task-orchestrator :pool
                               (make-job-pool :name "Autolith handoff test" :job-class
                                              'task-job :maximum-concurrency 1
                                              :maximum-batch-size 1 :maximum-live-jobs 1
                                              :maximum-runtime-milliseconds 0
                                              :start-threads-p nil)
                               :maximum-depth 1)))
           (setf (application-task-presentation-orchestrator application) orchestrator
                 (cl-jobpond::job-pool--live-count (task-orchestrator-pool orchestrator))
                   1)
           (test-assert (null (application-localgroup-take-ready-handoff application))
            "live child work prevents handoff admission")
           (setf (cl-jobpond::job-pool--live-count (task-orchestrator-pool orchestrator))
                   0)
           (test-assert
            (equal (application-input-controller--next-work controller)
                   (list ':localgroup-handoff ':take-over))
            "handoff becomes main-thread work after children finish")))
      (when stream (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller (application-input-controller-stop controller))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(-> test-localgroup-detach-preempts-active-work () null)
(defun test-localgroup-detach-preempts-active-work ()
  "Test detach leaving the running agent alone and never waiting on queues."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (session nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application new-controller relay conversation)
               (test-localgroup--relay-application configuration)
             (declare (ignore relay conversation))
             (setf application new-application
                   controller new-controller
                   session (localgroup-start new-application)))
           (application-input-controller--enqueue
            controller ':message "the turn holding the terminal")
           (application-input-controller--next-work controller)
           (application-input-controller--enqueue
            controller ':message "queued follow-up")
           (application-input-controller-submit-primary-prompt
            controller "steering for the running turn")
           (test-assert
            (and (application-input-controller-turn-active-p controller)
                 (not (deque-empty-p
                       (application-input-controller-steering-items
                        controller))))
            "the session holds an active turn and unconsumed steering")
           (application-localgroup-request-handoff application ':detach)
           (test-assert
            (and (not (application-input-controller-turn-cancellation-p
                       controller))
                 (application-input-controller-turn-active-p controller))
            "detach never interrupts the agent that is running")
           (test-assert
            (localgroup-handoff--primary-ready-p controller)
            "steering and queued work never hold a detach")
           (application-input-controller--finish-work controller)
           (test-assert
            (equal (application-input-controller--next-work controller)
                   (list ':localgroup-handoff ':detach))
            "the detach the terminal asked for is the next thing taken")
           (test-assert
            (equal (deque->list
                    (application-input-controller-work-items controller))
                   (list (list ':message "steering for the running turn")
                         (list ':message "queued follow-up")))
            "detaching leaves steering and follow-ups for the replacement"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (application-input-controller-stop controller))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup--abandonment-fixture
    (configuration &key (:persisted-p boolean))
    (values application application-input-controller localgroup-terminal
            localgroup-session))

(defun test-localgroup--abandonment-fixture (configuration &key persisted-p)
  "Return a started relay APPLICATION whose foreground terminal was released."
  (configuration-ensure-directories configuration)
  (multiple-value-bind (application controller relay)
      (test-localgroup--relay-application configuration :persisted-p persisted-p)
    (let ((session (localgroup-start application)))
      (image-daemon:relay-release-direct relay)
      (values application controller relay session))))

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
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil))
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
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (socket nil)
         (attachment nil))
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
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil))
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
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil))
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
  "Test every resume keeping the client path whose detach is instant."
  (flet ((client-p (&rest arguments)
           (apply #'main--client-session-p
                  :handoff-record nil
                  :authenticate-p nil
                  :recovery-conversation-id nil
                  :recovery-diagnosis nil
                  :image-values nil
                  :simulate-crash-p nil
                  arguments)))
    (let ((ordinary (client-p :resume-requested-p nil :resume-id nil)))
      (test-assert
       (eq (client-p :resume-requested-p t :resume-id "K-8vQ2mp") ordinary)
       "resuming an exact conversation is not a special case")
      (test-assert
       (eq (client-p :resume-requested-p t :resume-id nil) ordinary)
       "resuming through the picker is not a special case either")))
  (test-assert
   (eq (main--client-session-p
        :handoff-record nil
        :authenticate-p nil
        :resume-requested-p nil
        :resume-id nil
        :recovery-conversation-id "K-8vQ2mp"
        :recovery-diagnosis "diagnose the crash"
        :image-values nil
        :simulate-crash-p nil)
       (main--client-session-p
        :handoff-record nil
        :authenticate-p nil
        :resume-requested-p nil
        :resume-id nil
        :recovery-conversation-id nil
        :recovery-diagnosis nil
        :image-values nil
        :simulate-crash-p nil))
   "crash recovery keeps the same client-first path as an ordinary start")
  (test-assert
   (not (main--client-session-p
         :handoff-record (list :localgroup-handoff :version 1)
         :authenticate-p nil
         :resume-requested-p nil
         :resume-id nil
         :recovery-conversation-id nil
         :recovery-diagnosis nil
         :image-values nil
         :simulate-crash-p nil))
   "the spawned session itself runs direct")
  nil)

(-> test-localgroup-fresh-session-spawn () null)
(defun test-localgroup-fresh-session-spawn ()
  "Test client-first spawn records, launch options, and start-failure cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (launches nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (let* ((*localgroup-fresh-launch-function*
                    (lambda (launch-configuration session-id pathname
                             permission-argument immutable-p)
                      (declare (ignore launch-configuration))
                      (push (list session-id pathname permission-argument
                                  immutable-p)
                            launches)))
                  (*localgroup-fresh-wait-function*
                    (lambda (configuration token old-pid)
                      (declare (ignore configuration token old-pid))
                      "active-conversation"))
                  (session-id
                    (localgroup-handoff-spawn-fresh
                     configuration
                     :permission-mode ':sandboxed
                     :immutable-p t
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
                  "a fresh spawn records terminal presentation and session state"))))
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
                 1)
              "a failed fresh spawn removes its own pending record")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-process-handoff () null)

(defun test-localgroup-process-handoff ()
  "Test successful and failed process handoff lease and snapshot behavior."
  (labels ((run-success (persisted-p)
             "Run one successful fake replacement for PERSISTED-P."
             (let* ((configuration (test-configuration))
                    (root (test-configuration-root configuration))
                    (application nil)
                    (controller nil)
                    (session nil)
                    (captured-record nil))
               (unwind-protect
                   (progn
                    (configuration-ensure-directories configuration)
                    (multiple-value-bind
                        (new-application new-controller relay conversation)
                        (test-localgroup--relay-application configuration :persisted-p
                         persisted-p)
                      (declare (ignore relay))
                      (setf application new-application
                            controller new-controller
                            session (localgroup-start new-application))
                      (terminal-ui-set-input (application-ui application)
                                             "handoff draft")
                      (application-localgroup-request-handoff application ':detach)
                      (let ((work (application-input-controller--next-work controller)))
                        (test-assert (equal work (list ':localgroup-handoff ':detach))
                         "idle detach becomes explicit main-thread work")
                        (let ((*localgroup-handoff-launch-function*
                               (lambda (ignored-application pathname)
                                 (declare (ignore ignored-application))
                                 (multiple-value-bind (record complete-p)
                                     (snapshot-read pathname)
                                   (test-assert complete-p
                                    "handoff snapshot is complete before launch")
                                   (setf captured-record record))
                                 ':fake-process))
                              (*localgroup-handoff-wait-function*
                               (lambda (configuration session-id token old-pid)
                                 (declare
                                  (ignore configuration session-id token old-pid))
                                 t)))
                          (application-input-controller--run-work controller work)))
                      (test-assert
                       (and (application-input-controller-stopping-p controller)
                            (null (application-conversation-lease application))
                            (string= (getf (rest captured-record) :draft)
                                     "handoff draft")
                            (string= (getf (rest captured-record) :session-id)
                                     (image-daemon:daemon-runtime-identifier session))
                            (string= (getf (rest captured-record) :token)
                                     (image-daemon:daemon-runtime-token session))
                            (string= (getf (rest captured-record) :conversation-id)
                                     (conversation-identifier conversation))
                            (conversation-persisted-p conversation)
                            (string=
                             (conversation-identifier
                              (conversation-load (conversation-pathname conversation)))
                             (conversation-identifier conversation)))
                       "successful handoff transfers lease, identity, conversation, and draft")))
                 (when application
                   (localgroup-stop application)
                   (application-release-conversation-lease application))
                 (when controller (application-input-controller-stop controller))
                 (uiop/filesystem:delete-directory-tree root :validate t
                                                        :if-does-not-exist ':ignore)))))
    (run-success nil)
    (run-success t))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (session nil)
         (stopped-p nil))
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (multiple-value-bind (new-application new-controller relay conversation)
             (test-localgroup--relay-application configuration :persisted-p t)
           (declare (ignore relay conversation))
           (setf application new-application
                 controller new-controller
                 session (localgroup-start new-application)))
         (setf (application-input-controller-pause-depth controller) 1
               (application-input-controller-reader-paused-p controller) t)
         (application-localgroup-request-handoff application ':detach)
         (let ((work (application-input-controller--next-work controller)))
           (test-assert
            (handler-case
             (let ((*localgroup-handoff-launch-function*
                    (lambda (ignored-application pathname)
                      (declare (ignore ignored-application pathname))
                      ':fake-process))
                   (*localgroup-handoff-wait-function*
                    (lambda (configuration session-id token old-pid)
                      (declare (ignore configuration session-id token old-pid))
                      nil))
                   (*localgroup-handoff-stop-function*
                    (lambda (process pathname)
                      (declare (ignore process pathname))
                      (setf stopped-p t))))
               (application-localgroup-run-handoff application (second work) controller)
               nil)
             (localgroup-error nil t))
            "failed replacement reports a structured localgroup error"))
         (test-assert
          (and stopped-p (application-conversation-lease application)
               (not (application-input-controller-stopping-p controller))
               (not (application-input-controller-localgroup-handoff-p controller))
               (not (localgroup-session-handoff-running-p session))
               (null
                (uiop/filesystem:directory-files
                 (localgroup-handoff-directory configuration) "*.sexp")))
          "failed replacement is stopped and the old leased session remains usable"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (setf (application-input-controller-pause-depth controller) 0)
        (application-input-controller-stop controller))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(defun test-localgroup-conversation-identity ()
  "Test active IDs through resume selection, new, fork, reconnect and conflicts."
  (with-test-configuration (configuration)
   (let ((application nil) (controller nil) (relay nil) (stream nil) (session nil))
     (unwind-protect
         (progn
          (multiple-value-setq (application controller relay)
            (test-localgroup--relay-application configuration))
          (let* ((first (application-conversation application))
                 (provider (provider-create configuration))
                 (registry (make-instance 'tool-registry))
                 (saved (conversation-create configuration :identifier "abcdef012345")))
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
                              (status (localgroup-status-snapshot session))
                              (entry (localgroup--find-record configuration identifier)))
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
              (let ((port (image-daemon:daemon-runtime-port session))
                    (token (image-daemon:daemon-runtime-token session))
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
                       for packet = (test-localgroup--read-packet stream)
                       thereis (and (eq (first packet) ':output)
                                    (search "conversation switched" (second packet))))
                 "an attached terminal receives output after conversation changes"))
              (let* ((before (application-conversation application))
                     (blocked (conversation-create configuration))
                     (identifier (conversation-identifier blocked))
                     (pathname (localgroup-registry-pathname configuration identifier))
                     (record
                      (image-daemon:daemon-registry-record :identifier identifier :pid
                       (sb-posix:getpid) :port 12345 :token
                       (format nil "token-~A" identifier) :created-at
                       (get-universal-time))))
                (snapshot-write pathname record)
                (test-assert
                 (handler-case
                  (progn (application-install-conversation application blocked) nil)
                  (application-runtime-replacement-error nil t))
                 "a conflicting live registry owner aborts the conversation switch")
                (test-assert
                 (and (eq before (application-conversation application))
                      (equal record (image-daemon:daemon-registry-read pathname)))
                 "failed rekey restores the active conversation and preserves its competitor")
                (conversation-lease-release
                 (conversation-lease-acquire configuration identifier))
                (delete-file pathname)
                (assert-identity before)
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
                    (pathname (image-daemon:daemon-runtime-registry-pathname session)))
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
