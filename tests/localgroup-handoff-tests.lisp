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

(-> test-localgroup-handoff-site-arguments () null)
(defun test-localgroup-handoff-site-arguments ()
  "Test detached replacements retain the configured site root."
  (let* ((site-container
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-handoff-site-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (site-root (merge-pathnames "site/" site-container))
         (configuration nil)
         (root nil))
    (unwind-protect
         (progn
           (ensure-directories-exist site-root)
           (setf configuration
                 (test-configuration
                  :site-config-root
                  (uiop:ensure-directory-pathname (truename site-root)))
                 root (test-configuration-root configuration))
           (let* ((application
                    (make-instance 'application
                                   :configuration configuration
                                   :permission-mode ':auto))
                  (handoff-pathname (merge-pathnames "handoff.sexp" root))
                  (arguments
                    (localgroup-handoff--arguments application handoff-pathname)))
             (test-assert
              (equal arguments
                     (list
                      (namestring
                       (merge-pathnames
                        "bin/autolith"
                        (configuration-source-root configuration)))
                      "--permissions" "auto"
                      "--site-config-root"
                      (namestring
                       (configuration-site-config-root configuration))
                      "--localgroup-handoff" (namestring handoff-pathname)))
              "a detached replacement preserves the exact site configuration root")))
      (when root
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))
      (uiop:delete-directory-tree site-container
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

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
               (test-localgroup--relay-application
                configuration :persisted-p t)
             (declare (ignore controller relay))
             (setf first-application application
                   first-session (localgroup-start application))
             (terminal-ui-set-input (application-ui application) "draft survives")
             (setf handoff-pathname
                   (localgroup-handoff--write
                    application first-session ':detach))
             (let* ((record
                      (localgroup-handoff--read configuration handoff-pathname))
                    (restored (localgroup-handoff-initial-input record)))
               (test-assert
                (and (string=
                      (getf (rest record) :conversation-id)
                      (conversation-identifier conversation))
                     (typep restored 'user-message-input)
                     (string= (user-message-input-text restored) "draft survives"))
                "handoff records preserve durable conversation identity and draft")
               (let ((*localgroup-handoff-setsid-function* (lambda () 0)))
                 (localgroup-handoff-begin-startup record))
               (multiple-value-bind (pid-record complete-p)
                   (snapshot-read
                    (localgroup-handoff--pid-pathname handoff-pathname))
                 (test-assert
                  (and complete-p
                       (probe-file (getf (rest record) :pathname))
                       (= (getf (rest pid-record) :pid)
                          (sb-posix:getpid)))
                  "replacement startup acknowledges its detached process identity"))
               (multiple-value-bind (application controller relay conversation)
                   (test-localgroup--relay-application configuration)
                 (declare (ignore controller relay conversation))
                 (setf second-application application)
                 (let ((*localgroup-startup-record* record))
                   (setf second-session (localgroup-start application))))))
           (test-assert
            (and (string= (localgroup-session-identifier first-session)
                          (localgroup-session-identifier second-session))
                 (string= (localgroup-session-token first-session)
                          (localgroup-session-token second-session))
                 (= (localgroup-session-created-at first-session)
                    (localgroup-session-created-at second-session))
                 (not (probe-file handoff-pathname)))
            "replacement startup preserves localgroup identity and consumes its record")
           (localgroup-stop first-application)
           (test-assert
            (equal
             (localgroup--read-endpoint-record
              (localgroup-session-registry-pathname second-session))
             (localgroup--registry-record second-session))
            "old shutdown cannot delete a replacement endpoint record"))
      (when first-application
        (localgroup-stop first-application)
        (application-release-conversation-lease first-application))
      (when second-application
        (localgroup-stop second-application)
        (application-release-conversation-lease second-application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
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
           (application-input-controller--enqueue
            controller ':message "first")
           (multiple-value-setq (socket stream)
             (multiple-value-bind (new-socket new-stream response)
                 (test-localgroup--attach session ':take-over)
               (test-assert
                (and (eq (first response) ':handoff)
                     (eq (localgroup-terminal-attachment-kind relay) ':foreground)
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
            (equal (deque->list
                    (application-input-controller-work-items controller))
                   (list (list ':message "first")))
            "preempted follow-up work stays queued for the replacement")
           (application-input-controller--finish-work controller)
           ;; Re-arm the consumed handoff; the child-job gate below must
           ;; defer it back to the still-queued follow-up.
           (application-localgroup-request-handoff application ':take-over)
           (let ((orchestrator
                   (make-instance 'task-orchestrator
                                  :pool (make-job-pool :name "Autolith handoff test"
                                                       :job-class 'task-job
                                                       :maximum-concurrency 1
                                                       :maximum-batch-size 1
                                                       :maximum-live-jobs 1
                                                       :maximum-runtime-milliseconds 0
                                                       :start-threads-p nil)
                                  :maximum-depth 1)))
             (setf (application-task-presentation-orchestrator application)
                   orchestrator
                   (cl-jobpond::job-pool--live-count
                    (task-orchestrator-pool orchestrator))
                   1)
             (test-assert
              (null (application-localgroup-take-ready-handoff application))
              "live child work prevents handoff admission")
             (setf (cl-jobpond::job-pool--live-count
                    (task-orchestrator-pool orchestrator))
                   0)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     (list ':localgroup-handoff ':take-over))
              "handoff becomes main-thread work after children finish")))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-abandoned-session-reap () null)
(defun test-localgroup-abandoned-session-reap ()
  "Test pristine sessions exiting when their only client vanishes."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (relay nil)
         (session nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-setq (application controller relay)
             (test-localgroup--relay-application configuration))
           (setf session (localgroup-start application))
           (test-assert
            (not (localgroup--session-pristine-p session))
            "a session with a foreground terminal is never pristine")
           (localgroup-terminal-release-control relay)
           (test-assert
            (localgroup--session-pristine-p session)
            "an untouched ownerless session is pristine")
           (setf (application-goal application)
                 (list :objective "keep me" :status ':active
                       :continuations 0))
           (test-assert
            (not (localgroup--session-pristine-p session))
            "a session goal keeps an abandoned session alive")
           (setf (application-goal application) nil)
           (application-input-controller--enqueue
            controller ':message "queued input")
           (test-assert
            (not (localgroup--session-pristine-p session))
            "queued work keeps an abandoned session alive")
           (application-input-controller--next-work controller)
           (test-assert
            (not (localgroup--session-pristine-p session))
            "an active turn keeps an abandoned session alive")
           (application-input-controller--finish-work controller)
           (test-assert
            (localgroup--session-pristine-p session)
            "a drained untouched session is pristine again")
           (localgroup--reap-abandoned-session session)
           (test-assert
            (eq (application-input-controller-exit-reason controller)
                ':localgroup-kill)
            "losing the only client exits a pristine session"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (relay nil)
         (session nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-setq (application controller relay)
             (test-localgroup--relay-application configuration
                                                 :persisted-p t))
           (setf session (localgroup-start application))
           (localgroup-terminal-release-control relay)
           (test-assert
            (not (localgroup--session-pristine-p session))
            "a persisted conversation keeps an abandoned session alive")
           (localgroup--reap-abandoned-session session)
           (test-assert
            (null (application-input-controller-exit-reason controller))
            "reaping never touches a session with durable history"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
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
                  (*localgroup-handoff-wait-function*
                    (lambda (configuration session-id token old-pid)
                      (declare (ignore configuration session-id token old-pid))
                      t))
                  (session-id
                    (localgroup-handoff-spawn-fresh
                     configuration
                     :permission-mode ':sandboxed
                     :immutable-p t
                     :recovery-diagnosis "diagnose the crash")))
             (destructuring-bind (launched-id pathname permission immutable-p)
                 (first launches)
               (test-assert (and (= (length launches) 1)
                                 (string= launched-id session-id)
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
                       (string= (getf (rest record) :session-id) session-id))
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
                 (*localgroup-handoff-wait-function*
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                          (test-localgroup--relay-application
                           configuration :persisted-p persisted-p)
                        (declare (ignore relay))
                        (setf application new-application
                              controller new-controller
                              session (localgroup-start new-application))
                        (terminal-ui-set-input
                         (application-ui application) "handoff draft")
                        (application-localgroup-request-handoff
                         application ':detach)
                        (let ((work
                                (application-input-controller--next-work
                                 controller)))
                          (test-assert
                           (equal work (list ':localgroup-handoff ':detach))
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
                                    (declare (ignore configuration session-id token old-pid))
                                    t)))
                            (application-input-controller--run-work
                             controller work)))
                        (test-assert
                         (and (application-input-controller-stopping-p controller)
                              (null (application-conversation-lease application))
                              (string=
                               (getf (rest captured-record) :draft)
                               "handoff draft")
                              (string=
                               (getf (rest captured-record) :session-id)
                               (localgroup-session-identifier session))
                              (string=
                               (getf (rest captured-record) :token)
                               (localgroup-session-token session))
                              (if persisted-p
                                  (string=
                                   (getf (rest captured-record) :conversation-id)
                                   (conversation-identifier conversation))
                                  (null
                                   (getf (rest captured-record) :conversation-id))))
                         "successful handoff transfers lease, identity, conversation, and draft")))
                 (when application
                   (localgroup-stop application)
                   (application-release-conversation-lease application))
                 (when controller
                   (application-input-controller-stop controller))
                 (uiop:delete-directory-tree root
                                             :validate t
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
               (test-localgroup--relay-application
                configuration :persisted-p t)
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
                    (application-localgroup-run-handoff
                     application (second work) controller)
                    nil)
                (localgroup-error () t))
              "failed replacement reports a structured localgroup error"))
           (test-assert
            (and stopped-p
                 (application-conversation-lease application)
                 (not (application-input-controller-stopping-p controller))
                 (not
                  (application-input-controller-localgroup-handoff-p controller))
                 (not (localgroup-session-handoff-running-p session))
                 (null
                  (uiop:directory-files
                   (localgroup-handoff-directory configuration) "*.sexp")))
            "failed replacement is stopped and the old leased session remains usable"))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (when controller
        (setf (application-input-controller-pause-depth controller) 0)
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)
