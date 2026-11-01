(in-package #:autolith)

;;;; -- Detached Process Handoff --

(defparameter *localgroup-handoff-version* 1
  "The detached-process handoff snapshot version.")

(defparameter *localgroup-handoff-start-timeout-seconds* 20
  "The maximum time an old process waits for its detached replacement.")

(defparameter *localgroup-handoff-stop-grace-seconds* 1/5
  "The grace period between terminating and killing a failed replacement.")

(defparameter *localgroup-handoff-launch-function*
  (lambda (application handoff-pathname)
    (localgroup-handoff--launch application handoff-pathname))
  "The detached replacement launch boundary used by process handoff.")

(defparameter *localgroup-handoff-stop-function*
  (lambda (process handoff-pathname)
    (localgroup-handoff--stop-replacement process handoff-pathname))
  "The failed replacement termination boundary used by process handoff.")

(defparameter *localgroup-handoff-wait-function*
  (lambda (configuration session-id token old-pid)
    (localgroup-handoff--wait-for-replacement configuration session-id
                                              :token   token
                                              :old-pid old-pid))
  "The authenticated replacement readiness boundary used by process handoff.")

(defparameter *localgroup-handoff-setsid-function*
  (lambda ()
    (platform-detach-session *platform*))
  "The session-detachment boundary used during replacement startup.")

(defparameter *localgroup-fresh-launch-function*
  (lambda (configuration &key session-id handoff-pathname permission-argument
                             immutable-p pristine-p)
    (localgroup-handoff--launch-for
     configuration session-id
     :handoff-pathname    handoff-pathname
     :permission-argument permission-argument
     :immutable-p         immutable-p
     :pristine-p          pristine-p))
  "The detached launch boundary used by client-first session starts.")

(defparameter *localgroup-fresh-wait-function*
  (lambda (configuration token old-pid)
    (localgroup-handoff--wait-for-fresh configuration token old-pid))
  "The authenticated discovery boundary returning a fresh launch's conversation ID.")


;;;; -- Private Records --

(-> localgroup-handoff-directory (configuration) pathname)
(defun localgroup-handoff-directory (configuration)
  "Return CONFIGURATION's private detached-process handoff directory."
  (merge-pathnames "localgroup/handoffs/"
                   (config :state-root configuration)))

(-> localgroup-handoff-log-pathname (configuration string) pathname)
(defun localgroup-handoff-log-pathname (configuration session-id)
  "Return the private detached process log for SESSION-ID."
  (merge-pathnames
   (make-pathname :name session-id :type "log")
   (merge-pathnames "localgroup/logs/"
                    (config :state-root configuration))))

(-> localgroup-handoff--pathname (configuration string) pathname)
(defun localgroup-handoff--pathname (configuration session-id)
  "Return a fresh private handoff ticket pathname for SESSION-ID."
  (image-daemon:handoff-ticket-pathname (localgroup-handoff-directory configuration)
                                        session-id))

(-> localgroup-handoff--timestamp-p (t) boolean)
(defun localgroup-handoff--timestamp-p (value)
  "Return true when VALUE is a handoff timestamp."
  (typep value 'timestamp))

(-> localgroup-handoff--mode-p (t) boolean)
(defun localgroup-handoff--mode-p (value)
  "Return true when VALUE names a supported handoff mode."
  (not (null (member value '(:detach :take-over)))))

(-> localgroup-handoff--state-p (t) boolean)
(defun localgroup-handoff--state-p (value)
  "Return true when VALUE names a supported handoff state."
  (not (null (member value '(:pending :claimed :cancelled)))))

(-> localgroup-handoff--positive-integer-p (t) boolean)
(defun localgroup-handoff--positive-integer-p (value)
  "Return true when VALUE is a positive process identifier."
  (typep value '(integer 1)))

(-> localgroup-handoff--optional-positive-integer-p (t) boolean)
(defun localgroup-handoff--optional-positive-integer-p (value)
  "Return true when VALUE is NIL or a positive process identifier."
  (or (null value) (localgroup-handoff--positive-integer-p value)))

(-> localgroup-handoff--conversation-id-p (t) boolean)
(defun localgroup-handoff--conversation-id-p (value)
  "Return true when VALUE is NIL or a valid conversation identifier."
  (or (null value)
      (and (stringp value)
           (handler-case
               (progn
                 (conversation-identifier-validate-path-component value)
                 t)
             (conversation-identifier-error ()
               nil)))))

(-> localgroup-handoff--record-p (t) boolean)
(defun localgroup-handoff--record-p (record)
  "Return true when RECORD is one supported detached-process handoff."
  (and
   (record-check
    record :tag ':localgroup-handoff :versions (list *localgroup-handoff-version*)
    :allow-other-keys nil :keyword-keys-p t :maximum-length 36
    :fields
    `((:indicator :launch-id :validate ,(lambda (value) (or (null value) (stringp value))))
      (:indicator :session-id :validate ,(lambda (value) (or (null value) (stringp value))))
      (:indicator :token :required t :validate non-empty-string-p)
      (:indicator :created-at :required t
       :validate localgroup-handoff--timestamp-p)
      (:indicator :mode :required t :validate localgroup-handoff--mode-p)
      (:indicator :state :required t :validate localgroup-handoff--state-p)
      (:indicator :fresh-conversation-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :attach-expected-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :resume-command-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :styled-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :rows :validate localgroup-handoff--positive-integer-p)
      (:indicator :columns :validate localgroup-handoff--positive-integer-p)
      (:indicator :recovery-diagnosis)
      (:indicator :old-pid :required t
       :validate localgroup-handoff--positive-integer-p)
      (:indicator :replacement-pid
       :validate  localgroup-handoff--optional-positive-integer-p)
      (:indicator :conversation-id :validate localgroup-handoff--conversation-id-p)
      (:indicator :draft :validate ,(lambda (value) (or (null value) (stringp value))))))
   ;; Client-first launches use a private nonce until a conversation exists.
   (or (non-empty-string-p (record-property record :launch-id))
       (non-empty-string-p (record-property record :session-id)))))

(-> localgroup-handoff--disk-record (list) list)
(defun localgroup-handoff--disk-record (record)
  "Return RECORD without its in-memory canonical pathname field."
  (let ((copy (copy-list record)))
    (remf (rest copy) :pathname)
    (remf (rest copy) :pending-pathname)
    copy))

(-> localgroup-handoff--write-record (pathname list) null)
(defun localgroup-handoff--write-record (pathname record)
  "Atomically write private handoff RECORD to PATHNAME."
  (image-daemon:handoff-ticket-write pathname (localgroup-handoff--disk-record record))
  nil)

(-> localgroup-handoff--write
    (application localgroup-session keyword)
    pathname)
(defun localgroup-handoff--write (application session mode)
  "Publish APPLICATION's detached-process handoff for SESSION and MODE."
  (let ((conversation (application-conversation application)))
    (unless (conversation-persisted-p conversation)
      (conversation-append-record conversation (list :localgroup-handoff :mode mode))))
  (let* ((configuration (application-configuration application))
         (conversation  (application-conversation application))
         (pathname
          (localgroup-handoff--pathname configuration
                                        (image-daemon:daemon-runtime-identifier
                                         session)))
         (draft         (line-editor-text (terminal-ui-editor (application-ui application))))
         (record
          (list :localgroup-handoff :version *localgroup-handoff-version* :session-id
                (image-daemon:daemon-runtime-identifier session) :token
                (image-daemon:daemon-runtime-token session) :created-at
                (image-daemon:daemon-runtime-created-at session) :mode mode :state
                ':pending :fresh-conversation-p nil :old-pid (current-process-id)
                :replacement-pid nil :conversation-id
                (conversation-identifier conversation) :draft draft)))
    (localgroup-handoff--write-record pathname record)
    pathname))

(-> localgroup-handoff--read (configuration pathname) list)
(defun localgroup-handoff--read (configuration pathname)
  "Read and validate PATHNAME beneath CONFIGURATION's private handoff root."
  (handler-case
      (let* ((root-pathname (localgroup-handoff-directory configuration))
             (root
               (and (probe-file root-pathname)
                    (uiop:ensure-directory-pathname (platform-truename *platform* root-pathname))))
             (canonical     (and (probe-file pathname) (platform-truename *platform* pathname))))
        (unless (and root canonical (uiop:subpathp canonical root))
          (error 'localgroup-error
                 :message   "The localgroup handoff path is unavailable or outside private state."
                 :operation ':handoff))
        (multiple-value-bind (record complete-p)
            (snapshot-read canonical)
          (unless (and complete-p (localgroup-handoff--record-p record))
            (error 'localgroup-error
                   :message   "The localgroup handoff record is malformed."
                   :operation ':handoff))
          (append record (list :pathname canonical))))
    (localgroup-error (condition)
      (error condition))
    (error (condition)
      (error 'localgroup-error
             :message   "The localgroup handoff record could not be read."
             :operation ':handoff
             :cause     condition))))

(-> localgroup-handoff-selection (configuration (option string)) (option list))
(defun localgroup-handoff-selection (configuration pathname-value)
  "Return the detached-process handoff named by the internal PATHNAME-VALUE."
  (when pathname-value
    (unless (non-empty-string-p pathname-value)
      (error 'localgroup-error
             :message   "The internal localgroup handoff option requires a pathname."
             :operation ':arguments))
    (let ((record
            (localgroup-handoff--read configuration (pathname pathname-value))))
      (unless (eq (getf (rest record) :state) ':pending)
        (error 'localgroup-error
               :message    "The localgroup handoff is no longer pending."
               :operation  ':handoff
               :session-id (getf (rest record) :session-id)))
      record)))

(-> localgroup-handoff-begin-startup (list) null)
(defun localgroup-handoff-begin-startup (record)
  "Atomically claim RECORD, detach the replacement, and acknowledge its PID."
  (let* ((pending-pathname (getf (rest record) :pathname))
         (claimed-pathname (image-daemon:handoff-ticket-claim pending-pathname)))
    (setf (getf (rest record) :state) ':claimed
          (getf (rest record) :pending-pathname) pending-pathname
          (getf (rest record) :pathname) claimed-pathname
          (getf (rest record) :replacement-pid) (current-process-id))
    (handler-case
        (funcall *localgroup-handoff-setsid-function*)
      (error (condition)
        (error 'localgroup-error
               :message    "The detached localgroup replacement could not create a new session."
               :operation  ':handoff
               :session-id (getf (rest record) :session-id)
               :cause      condition))))
  nil)

(-> localgroup-handoff-assert-startup-active () null)
(defun localgroup-handoff-assert-startup-active ()
  "Require the current replacement to retain its claimed handoff record."
  (when *localgroup-startup-record*
    (let* ((expected         (rest *localgroup-startup-record*))
           (pending-pathname (getf expected :pending-pathname))
           (record
             (and pending-pathname
                  (image-daemon:handoff-ticket-owned-record
                   pending-pathname
                   :token    (getf expected :token)
                   :validate #'localgroup-handoff--record-p))))
      (unless (and record
                   (equal (or (getf (rest record) :launch-id)
                              (getf (rest record) :session-id))
                          (or (getf expected :launch-id)
                              (getf expected :session-id))))
        (error 'localgroup-error
               :message    "The localgroup handoff was cancelled during startup."
               :operation  ':handoff
               :session-id (getf expected :session-id)))))
  nil)

(-> localgroup-handoff-initial-input (list) (option user-message-input))
(defun localgroup-handoff-initial-input (record)
  "Return RECORD's restored composer draft, when nonempty."
  (let ((draft (getf (rest record) :draft)))
    (when (non-empty-string-p draft)
      (user-message-input-create :text draft))))


;;;; -- Replacement Process --

(-> localgroup-handoff--permission-argument (application) string)
(defun localgroup-handoff--permission-argument (application)
  "Return APPLICATION's command-line permission mode."
  (localgroup-handoff-permission-string
   (application-permission-mode application)))

(-> localgroup-handoff-permission-string (keyword) string)
(defun localgroup-handoff-permission-string (mode)
  "Return permission MODE as the launcher --permissions argument."
  (ecase mode
    (:ask
     "ask")
    (:auto
     "auto")
    (:sandboxed
     "sandbox")
    (:full-access
     "full")))

(-> localgroup-handoff--launch-supervised
    (&key (:arguments list)
          (:handoff-pathname pathname)
          (:directory pathname)
          (:output stream))
    t)
(defun localgroup-handoff--launch-supervised
    (&key arguments handoff-pathname directory output)
  "Launch ARGUMENTS behind the platform's gated process-tree supervisor.

Signal PLATFORM-CAPABILITY-UNAVAILABLE on hosts without detached sessions
before any shell is involved."
  (unless (platform-supports-p *platform* ':detached-sessions)
    (error 'platform-capability-unavailable
           :capability ':detached-sessions
           :message    "This host cannot supervise a detached session process, so localgroup handoff is withheld."))
  (platform-launch-detached-process *platform* arguments
                                    :directory directory
                                    :output    output
                                    :ticket    handoff-pathname))

(-> localgroup-handoff--arguments
    (configuration pathname &key (:permission-argument string) (:immutable-p boolean)
                                 (:pristine-p boolean))
    list)
(defun localgroup-handoff--arguments
    (configuration handoff-pathname &key (permission-argument "ask") immutable-p pristine-p)
  "Return the platform launch command and handoff arguments for CONFIGURATION."
  (append
   (platform-session-launch-command
    *platform* (config :source-root configuration))
   (list "--permissions" permission-argument)
   (when (config :model-selection-override-p configuration)
     (append (list "--model" (config :model configuration))
             (when (non-empty-string-p (config :model-provider configuration))
               (list "--provider" (config :model-provider configuration)))))
   (when immutable-p
     (list "--immutable"))
   (when pristine-p
     (list "--pristine"))
   (when (config :fullscreen-p configuration)
     (list "--fullscreen"))
   (when (eq (config :terminal-theme configuration) ':almighty)
     (list "--almighty"))
   (let ((site-config-root (config :site-config-root configuration)))
     (when site-config-root
       (list "--site-config-root" (namestring site-config-root))))
   (list "--localgroup-handoff" (namestring handoff-pathname))))

(-> localgroup-handoff--launch (application pathname) t)
(defun localgroup-handoff--launch (application handoff-pathname)
  "Launch APPLICATION's detached replacement from HANDOFF-PATHNAME."
  (localgroup-handoff--launch-for (application-configuration application)
                                  (image-daemon:daemon-runtime-identifier
                                   (application-localgroup-session application))
                                  :handoff-pathname    handoff-pathname
                                  :permission-argument (localgroup-handoff--permission-argument
                                                        application)
                                  :immutable-p         (config :immutable-p
                                                        (application-configuration application))))

(-> localgroup-handoff--launch-for
    (configuration string &key (:handoff-pathname pathname) (:permission-argument string)
                               (:immutable-p boolean) (:pristine-p boolean))
    t)
(defun localgroup-handoff--launch-for
    (configuration session-id &key handoff-pathname permission-argument immutable-p pristine-p)
  "Launch one detached session process from HANDOFF-PATHNAME."
  (let ((arguments
          (localgroup-handoff--arguments
           configuration handoff-pathname
           :permission-argument permission-argument
            :immutable-p        immutable-p
            :pristine-p         pristine-p))
        (log-pathname
          (localgroup-handoff-log-pathname configuration session-id)))
    (ensure-directories-exist log-pathname)
    (with-open-file (output log-pathname
                            :direction         ':output
                            :if-exists         ':append
                            :if-does-not-exist ':create
                            :external-format   ':utf-8)
      (platform-make-private *platform* log-pathname)
      (localgroup-handoff--launch-supervised
       :arguments        arguments
       :handoff-pathname handoff-pathname
       :directory        (config :working-directory configuration)
       :output           output))))

(-> localgroup-handoff-spawn-fresh
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:conversation-id (option string))
                        (:resume-command-p boolean)
                        (:recovery-diagnosis t)
                        (:attach-expected-p boolean)
                        (:pristine-p boolean))
    string)
(defun localgroup-handoff-spawn-fresh
    (configuration &key (permission-mode ':ask) immutable-p conversation-id
                        resume-command-p recovery-diagnosis pristine-p
                        (attach-expected-p t))
  "Launch a detached session and return its ready identifier.

CONVERSATION-ID resumes that conversation instead of starting a fresh
one, RESUME-COMMAND-P lets the session pick one itself through the
attached terminal, and RECOVERY-DIAGNOSIS preserves crash diagnosis
across the client-first launch. When ATTACH-EXPECTED-P is false, the
session is explicitly detached and its first-attach watchdog is disabled."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (let* ((created-at  (get-universal-time))
           (launch-id   (daemon-random-nonce))
           (token       (daemon-random-token))
           (pathname    (localgroup-handoff--pathname configuration launch-id))
           (record
             (list :localgroup-handoff
                   :version *localgroup-handoff-version*
                   :launch-id            launch-id
                   :token                token
                   :created-at           created-at
                   :mode                 ':detach
                   :state                ':pending
                   :attach-expected-p    (not (null attach-expected-p))
                   :fresh-conversation-p (and (null conversation-id)
                                              (not resume-command-p))
                   :resume-command-p     (not (null resume-command-p))
                   :old-pid              (current-process-id)
                   :replacement-pid      nil
                   :conversation-id      conversation-id
                    :recovery-diagnosis  recovery-diagnosis
                   :draft                ""
                   :rows                 rows
                   :columns              columns
                   :styled-p             (not (null (terminal-environment-styling-p)))))
           (completed-p nil)
           (process     nil)
           (session-id  nil))
      (localgroup-handoff--write-record pathname record)
      (unwind-protect
           (progn
             (setf process
                    (funcall *localgroup-fresh-launch-function*
                             configuration
                             :session-id          launch-id
                             :handoff-pathname    pathname
                             :permission-argument (localgroup-handoff-permission-string permission-mode)
                             :immutable-p         immutable-p
                             :pristine-p          pristine-p))
             (setf session-id
                   (funcall *localgroup-fresh-wait-function*
                            configuration token (current-process-id)))
             (unless session-id
               (error 'localgroup-error
                      :message
                      (format nil
                              "The detached session did not start within ~D seconds. See ~A."
                              *localgroup-handoff-start-timeout-seconds*
                              (namestring
                               (localgroup-handoff-log-pathname
                                configuration launch-id)))
                      :operation  ':spawn
                      :session-id session-id))
             (platform-release-process *platform* process)
             (setf completed-p t)
             session-id)
        (unless completed-p
          (when process
            ; mallet:suppress no-ignore-errors -- best-effort stop of a failed launch before deleting its ticket
            (ignore-errors
              (funcall *localgroup-handoff-stop-function* process pathname)))
          (image-daemon:handoff-ticket-delete pathname))))))

(-> localgroup-handoff--ready-conversation-id
    (list string integer) (option string))
(defun localgroup-handoff--ready-conversation-id (record token old-pid)
  "Return RECORD's authenticated active conversation ID for a new process."
  (let* ((status     (image-daemon:daemon-endpoint-status record token old-pid))
         (identifier (and (consp status) (getf (rest status) :session-id))))
    (when (and (stringp identifier)
               (equal identifier (getf (rest status) :conversation-id))
               (equal identifier (getf (rest record) :session-id))
               (eql (getf (rest status) :pid) (getf (rest record) :pid)))
      identifier)))

(-> localgroup-handoff--wait-for-fresh (configuration string integer) (option string))
(defun localgroup-handoff--wait-for-fresh (configuration token old-pid)
  "Resolve a private launch token to its published active conversation ID."
  (image-daemon:daemon-wait-until
   (lambda ()
     (loop for entry in (localgroup-endpoint-records configuration)
           thereis (localgroup-handoff--ready-conversation-id (rest entry) token old-pid)))
   *localgroup-handoff-start-timeout-seconds*))

(-> localgroup-handoff--wait-for-replacement
    (configuration string &key (:token string) (:old-pid integer))
    boolean)
(defun localgroup-handoff--wait-for-replacement
    (configuration session-id &key token old-pid)
  "Wait boundedly for a live replacement endpoint."
  (not (null
        (image-daemon:daemon-wait-until
         (lambda ()
           (image-daemon:daemon-endpoint-status
            (image-daemon:daemon-registry-read
             (localgroup-registry-pathname configuration session-id))
            token old-pid))
         *localgroup-handoff-start-timeout-seconds*))))

(-> localgroup-handoff--signal-pid (integer boolean) null)
(defun localgroup-handoff--signal-pid (pid force-p)
  "Best-effort terminate PID or process group -PID, forcibly when FORCE-P."
  (handler-case
      (if (minusp pid)
          (platform-terminate-process-group *platform* (- pid) :force force-p)
          (platform-terminate-process *platform* pid :force force-p))
    (error () nil))
  nil)

(-> localgroup-handoff--cancel (pathname) pathname)
(defun localgroup-handoff--cancel (pathname)
  "Atomically invalidate PATHNAME's pending or claimed startup ownership."
  (image-daemon:handoff-ticket-cancel pathname :validate #'localgroup-handoff--record-p))

(-> localgroup-handoff--pid-alive-p (integer) boolean)
(defun localgroup-handoff--pid-alive-p (pid)
  "Return true when PID or process group -PID is still alive."
  (handler-case
      (if (minusp pid)
          (platform-process-group-alive-p *platform* (- pid))
          (platform-process-alive-p *platform* pid))
    (error () nil)))

(-> localgroup-handoff--process-object-pid (t) (option integer))
(defun localgroup-handoff--process-object-pid (process)
  "Return PROCESS's adapter PID."
  (platform-process-object-pid *platform* process))

(-> localgroup-handoff--process-object-alive-p (t) boolean)
(defun localgroup-handoff--process-object-alive-p (process)
  "Return whether PROCESS is alive through its platform adapter."
  (and (platform-process-object-alive-p *platform* process) t))

(-> localgroup-handoff--terminate-process-object (t t) null)
(defun localgroup-handoff--terminate-process-object (process force-p)
  "Terminate PROCESS through its platform adapter, forcefully when FORCE-P."
  (platform-terminate-process-object *platform* process :force force-p)
  nil)

(-> localgroup-handoff--stop-replacement (t pathname) boolean)
(defun localgroup-handoff--stop-replacement (process handoff-pathname)
  "Cancel, terminate, and positively reap PROCESS and its replacement session."
  (localgroup-handoff--cancel handoff-pathname)
  (let* ((root-pid   (localgroup-handoff--process-object-pid process))
         (known-pids nil)
         (started-at (get-internal-real-time))
         (kill-at
           (+ started-at
              (* *localgroup-handoff-stop-grace-seconds*
                 internal-time-units-per-second)))
         (deadline   (+ started-at (* 5 internal-time-units-per-second))))
    (loop
      (let* ((now     (get-internal-real-time))
             (force-p (>= now kill-at))
             (launcher-pid
               (image-daemon:handoff-ticket-launcher-pid handoff-pathname))
             (replacement-pid
               (image-daemon:handoff-ticket-replacement-pid
                handoff-pathname :validate #'localgroup-handoff--record-p)))
        (when root-pid
          (setf known-pids
                (remove-duplicates
                 (append (descendant-process-ids root-pid) known-pids)
                 :test #'=)))
        (when launcher-pid
          (localgroup-handoff--signal-pid launcher-pid force-p)
          (localgroup-handoff--signal-pid (- launcher-pid) force-p))
        (when replacement-pid
          (localgroup-handoff--signal-pid replacement-pid force-p)
          (localgroup-handoff--signal-pid (- replacement-pid) force-p))
        (dolist (pid known-pids)
          (localgroup-handoff--signal-pid pid force-p))
        (when root-pid
          (localgroup-handoff--signal-pid root-pid force-p))
        ;; Discover descendants before forcefully closing the platform object.
        (when (and force-p
                   (localgroup-handoff--process-object-alive-p process))
          (localgroup-handoff--terminate-process-object process t))
        (setf known-pids
              (remove-if-not #'localgroup-handoff--pid-alive-p known-pids))
        (let ((root-alive-p
                  (or (and root-pid (localgroup-handoff--pid-alive-p root-pid))
                      (localgroup-handoff--process-object-alive-p process)))
              (launcher-alive-p
                (and launcher-pid
                     (or (localgroup-handoff--pid-alive-p launcher-pid)
                         (localgroup-handoff--pid-alive-p (- launcher-pid)))))
              (replacement-alive-p
                (and replacement-pid
                     (or (localgroup-handoff--pid-alive-p replacement-pid)
                         (localgroup-handoff--pid-alive-p
                          (- replacement-pid))))))
          (unless (or root-alive-p launcher-alive-p replacement-alive-p
                      known-pids)
                (platform-wait-process *platform* process)
            (return t)))
        (when (>= now deadline)
          (error 'localgroup-error
                 :message   "The failed detached replacement could not be terminated safely."
                 :operation ':handoff))
        (sleep 0.05)))))


;;;; -- Main-Thread Handoff --

(-> application-localgroup-request-handoff (application keyword) list)
(defun application-localgroup-request-handoff (application mode)
  "Schedule MODE process handoff without disturbing the running agent.

Detaching is a terminal-side event: the agent keeps working and never
observes it. Queued work and steering stay in the durable snapshot the
replacement restores, so detaching changes nothing about the session."
  (unless (member mode '(:detach :take-over))
    (error 'localgroup-error :message "The localgroup handoff mode is invalid."
           :operation                 ':handoff))
  (let ((session    (application-localgroup-session application))
        (controller (application-input-controller application)))
    (unless (and session controller)
      (error 'localgroup-error :message
             "The localgroup session is not ready for handoff." :operation ':handoff))
    (with-lock-held ((image-daemon:daemon-runtime-lock session))
      (setf (localgroup-session-paused-p session) nil)
      (unless (localgroup-session-handoff-running-p session)
        (setf (localgroup-session-handoff-mode session)
              (if (or (eq mode ':take-over)
                      (null (localgroup-session-handoff-mode session)))
                  mode
                  (localgroup-session-handoff-mode session)))))
    (with-lock-held ((application-input-controller-lock controller))
      (sb-thread:condition-broadcast
       (application-input-controller-condition-variable controller)))
    (list :ok :operation mode :scheduled-p t :session-id
          (image-daemon:daemon-runtime-identifier session) :old-pid (current-process-id))))

(-> application-localgroup-handoff-pending-p (application) boolean)
(defun application-localgroup-handoff-pending-p (application)
  "Return true while APPLICATION has pending or running process handoff."
  (let ((session (application-localgroup-session application)))
    (and session
         (not
          (null
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (or (localgroup-session-handoff-mode session)
                 (localgroup-session-handoff-running-p session))))))))

(-> application-localgroup-ready-handoff-p (application) boolean)
(defun application-localgroup-ready-handoff-p (application)
  "Return true when a scheduled handoff could be taken right now.

A non-consuming mirror of APPLICATION-LOCALGROUP-TAKE-READY-HANDOFF for
scheduler tests that must not disturb the armed mode."
  (let ((session (application-localgroup-session application)))
    (and session
         (multiple-value-bind (live active)
             (localgroup--task-counts application)
           (declare (ignore active))
           (zerop live))
         (not
          (null
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (and (not (localgroup-session-handoff-running-p session))
                  (localgroup-session-handoff-mode session))))))))

(-> application-localgroup-take-ready-handoff (application) (option keyword))
(defun application-localgroup-take-ready-handoff (application)
  "Consume APPLICATION's pending handoff when no child jobs remain."
  (let ((session (application-localgroup-session application)))
    (unless session (return-from application-localgroup-take-ready-handoff nil))
    (multiple-value-bind (live active)
        (localgroup--task-counts application)
      (declare (ignore active))
      (when (plusp live) (return-from application-localgroup-take-ready-handoff nil)))
    (with-lock-held ((image-daemon:daemon-runtime-lock session))
      (unless (localgroup-session-handoff-running-p session)
        (prog1 (localgroup-session-handoff-mode session)
          (setf (localgroup-session-handoff-mode session) nil))))))

(-> localgroup-handoff--primary-ready-p (application-input-controller) boolean)
(defun localgroup-handoff--primary-ready-p (controller)
  "Return true when CONTROLLER has no work that must precede handoff.

Queued follow-up work and steering never block a handoff: both live in
the durable pending snapshot the detached replacement restores, so
detaching changes nothing about them. Only cancellation, failure, and
shutdown must resolve in this process, because those decide whether a
replacement may exist at all. An open follow-up edit also holds it: the
recalled draft lives only in this editor, not in the snapshot."
  (and (null (application-input-controller-follow-up-edit-work controller))
       (null (application-input-controller-turn-cancellation-p controller))
       (null (application-input-controller-failure controller))
       (not (application-input-controller-stopping-p controller))))

(-> localgroup-handoff--restore-lease (application string) null)
(defun localgroup-handoff--restore-lease (application identifier)
  "Restore APPLICATION's conversation lease for IDENTIFIER after failed handoff."
  (unless (application-conversation-lease application)
    (let ((deadline (+ (get-internal-real-time)
                       (* 2 internal-time-units-per-second))))
      (loop
        (handler-case
            (progn
              (setf (application-conversation-lease application)
                    (conversation-lease-acquire
                     (application-configuration application) identifier))
              (return))
          (conversation-in-use (condition)
            (when (>= (get-internal-real-time) deadline)
              (error 'localgroup-error
                     :message   "The failed replacement still owns the conversation lease."
                     :operation ':handoff
                     :cause     condition))
            (sleep 0.05))))))
  nil)

(-> localgroup-handoff--remove-replacement-registry
    (configuration string &key (:token string) (:old-pid integer))
    null)
(defun localgroup-handoff--remove-replacement-registry
       (configuration session-id &key token old-pid)
  "Remove a failed replacement registry record without deleting the old endpoint."
  (let* ((pathname (localgroup-registry-pathname configuration session-id))
         (record   (image-daemon:daemon-registry-read pathname)))
    (when
        (and record (/= (getf (rest record) :pid) old-pid)
             (string= (getf (rest record) :token) token))
      (localgroup--remove-stale-record pathname record)))
  nil)

(-> application-localgroup-run-handoff (application keyword t) null)
(defun application-localgroup-run-handoff (application mode controller)
  "Launch APPLICATION's detached replacement and stop after authenticated readiness."
  (application-input-controller-call-with-reader-paused controller
                                                        (lambda ()
                                                          (let* ((session
                                                                  (application-localgroup-session
                                                                   application))
                                                                 (configuration
                                                                  (application-configuration
                                                                   application))
                                                                 (conversation-id
                                                                  (conversation-identifier
                                                                   (application-conversation
                                                                    application)))
                                                                 (session-id
                                                                  (image-daemon:daemon-runtime-identifier
                                                                   session))
                                                                 (token
                                                                  (image-daemon:daemon-runtime-token
                                                                   session))
                                                                 (old-pid
                                                                  (current-process-id))
                                                                 (handoff-pathname nil)
                                                                 (process          nil)
                                                                 (lease-released-p nil)
                                                                 (held-lease-p
                                                                  (and
                                                                   (slot-boundp
                                                                    application
                                                                    'conversation-lease)
                                                                   (application-conversation-lease
                                                                    application)))
                                                                 (gate-p           nil)
                                                                 (completed-p      nil)
                                                                 (primary-ready-p  nil))
                                                            (with-lock-held ((application-input-controller-lock
                                                                              controller))
                                                              (setf primary-ready-p
                                                                      (localgroup-handoff--primary-ready-p
                                                                       controller))
                                                              (when primary-ready-p
                                                                (setf (application-input-controller-localgroup-handoff-p
                                                                       controller)
                                                                      t
                                                                      gate-p t)))
                                                            (unless primary-ready-p
                                                              (application-localgroup-request-handoff
                                                               application mode)
                                                              (return-from
                                                                  application-localgroup-run-handoff
                                                                nil))
                                                            (multiple-value-bind
                                                                (live active)
                                                                (localgroup--task-counts
                                                                 application)
                                                              (declare (ignore active))
                                                              (when (plusp live)
                                                                (with-lock-held ((application-input-controller-lock
                                                                                  controller))
                                                                  (setf (application-input-controller-localgroup-handoff-p
                                                                         controller)
                                                                        nil
                                                                        gate-p nil))
                                                                (application-localgroup-request-handoff
                                                                 application mode)
                                                                (return-from
                                                                    application-localgroup-run-handoff
                                                                  nil)))
                                                            (with-lock-held ((image-daemon:daemon-runtime-lock
                                                                              session))
                                                              (setf (localgroup-session-handoff-running-p
                                                                     session)
                                                                    t))
                                                            (unwind-protect
                                                                (progn
                                                                 (setf handoff-pathname
                                                                         (localgroup-handoff--write
                                                                          application
                                                                          session mode))
                                                                 (application-release-conversation-lease
                                                                  application)
                                                                 (setf lease-released-p
                                                                         (not
                                                                          (null
                                                                           held-lease-p))
                                                                       process
                                                                         (funcall
                                                                          *localgroup-handoff-launch-function*
                                                                          application
                                                                          handoff-pathname))
                                                                 (unless
                                                                     (funcall
                                                                      *localgroup-handoff-wait-function*
                                                                      configuration
                                                                      session-id token
                                                                      old-pid)
                                                                   (error
                                                                    'localgroup-error
                                                                    :message
                                                                    (format nil
                                                                            "The detached replacement did not start within ~D seconds. See ~A."
                                                                            *localgroup-handoff-start-timeout-seconds*
                                                                            (namestring
                                                                             (localgroup-handoff-log-pathname
                                                                              configuration
                                                                              session-id)))
                                                                    :operation ':handoff
                                                                    :session-id
                                                                    session-id))
                                                                 (platform-release-process *platform* process)
                                                                 (setf completed-p t)
                                                                 (application-input-controller--prepare-shutdown
                                                                  controller
                                                                  ':localgroup-detach))
                                                              (unless completed-p
                                                                (when process
                                                                  (handler-case
                                                                   (funcall
                                                                    *localgroup-handoff-stop-function*
                                                                    process
                                                                    handoff-pathname)
                                                                   (localgroup-error
                                                                    (condition)
                                                                    (application-input-controller--prepare-shutdown
                                                                     controller
                                                                     ':localgroup-handoff-failed)
                                                                    (error condition))))
                                                                (localgroup-handoff--remove-replacement-registry
                                                                 configuration session-id
                                                                 :token   token
                                                                 :old-pid old-pid)
                                                                (when lease-released-p
                                                                  (localgroup-handoff--restore-lease
                                                                   application
                                                                   conversation-id))
                                                                (when handoff-pathname
                                                                  (image-daemon:handoff-ticket-delete
                                                                   handoff-pathname))
                                                                (with-lock-held ((image-daemon:daemon-runtime-lock
                                                                                  session))
                                                                  (setf (localgroup-session-handoff-running-p
                                                                         session)
                                                                        nil))
                                                                (when gate-p
                                                                  (with-lock-held ((application-input-controller-lock
                                                                                    controller))
                                                                    (setf (application-input-controller-localgroup-handoff-p
                                                                           controller)
                                                                          nil))))))))
  nil)

(-> localgroup-handoff-finish-startup (application) null)
(defun localgroup-handoff-finish-startup (application)
  "Delete the consumed startup handoff after APPLICATION publishes its endpoint."
  (declare (ignore application))
  (let ((pathname
          (and *localgroup-startup-record*
               (or (getf (rest *localgroup-startup-record*) :pending-pathname)
                   (getf (rest *localgroup-startup-record*) :pathname)))))
    (when pathname
      (image-daemon:handoff-ticket-delete pathname)))
  nil)
