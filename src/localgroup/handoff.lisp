(in-package #:autolith)

;;;; -- Detached Process Handoff --

(defparameter *localgroup-handoff-version* 1
  "The detached-process handoff snapshot version.")

(defparameter *localgroup-handoff-start-timeout-seconds* 20
  "The maximum time an old process waits for its detached replacement.")

(defparameter *localgroup-handoff-stop-grace-seconds* 1/5
  "The grace period between terminating and killing a failed replacement.")

(defparameter *localgroup-handoff-stop-function*
  (lambda (process handoff-pathname)
    (localgroup-handoff--stop-replacement process handoff-pathname))
  "The failed replacement termination boundary used by process handoff.")

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

(defparameter *localgroup-world-wait-function*
  (lambda (configuration token old-pid)
    (localgroup-world--wait-for-fresh configuration token old-pid))
  "The authenticated discovery boundary returning a fresh world server's identifier.")


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
    :allow-other-keys nil :keyword-keys-p t :maximum-length 38
    :fields
    `((:indicator :launch-id :validate ,(lambda (value) (or (null value) (stringp value))))
      (:indicator :session-id :validate ,(lambda (value) (or (null value) (stringp value))))
      (:indicator :token :required t :validate non-empty-string-p)
      (:indicator :created-at :required t
       :validate localgroup-handoff--timestamp-p)
      (:indicator :mode :required t :validate localgroup-handoff--mode-p)
      (:indicator :state :required t :validate localgroup-handoff--state-p)
      (:indicator :world-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :fresh-conversation-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :attach-expected-p
       :validate  ,(lambda (value) (typep value 'boolean)))
      (:indicator :pick-channel-p
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
   (if (config :fullscreen-p configuration)
       (list "--fullscreen")
       (list "--plain"))
   (when (eq (config :terminal-theme configuration) ':almighty)
     (list "--almighty"))
   (let ((site-config-root (config :site-config-root configuration)))
     (when site-config-root
       (list "--site-config-root" (namestring site-config-root))))
   (list "--localgroup-handoff" (namestring handoff-pathname))))

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
                        (:pick-channel-p boolean)
                        (:recovery-diagnosis t)
                        (:attach-expected-p boolean)
                        (:ready-function (option function))
                        (:pristine-p boolean)
                        (:world-p boolean))
    string)
(defun localgroup-handoff-spawn-fresh
    (configuration &key (permission-mode ':ask) immutable-p conversation-id
                        pick-channel-p recovery-diagnosis pristine-p
                        (attach-expected-p t) ready-function world-p)
  "Launch a detached session and return its ready identifier.

CONVERSATION-ID resumes that conversation instead of starting a fresh
one, PICK-CHANNEL-P lets the session pick one itself through the
attached terminal, and RECOVERY-DIAGNOSIS preserves crash diagnosis
across the client-first launch. When ATTACH-EXPECTED-P is false, the
session is explicitly detached and its first-attach watchdog is disabled.
With WORLD-P the process is a world server instead of one session, and
the identifier returned is the world endpoint's. Call READY-FUNCTION,
when supplied, with the ready identifier before releasing startup
ownership. A nonlocal exit from it cancels the launch."
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
                   :world-p              (not (null world-p))
                   :attach-expected-p    (not (null attach-expected-p))
                   :fresh-conversation-p (and (null conversation-id)
                                              (not pick-channel-p))
                   :pick-channel-p       (not (null pick-channel-p))
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
                   (funcall (if world-p
                                *localgroup-world-wait-function*
                                *localgroup-fresh-wait-function*)
                            configuration token (current-process-id)))
             (unless session-id
               (error 'localgroup-error
                      :message
                      (format nil
                              "The detached ~:[session~;world server~] did not start within ~D seconds. See ~A."
                              world-p
                              *localgroup-handoff-start-timeout-seconds*
                              (namestring
                               (localgroup-handoff-log-pathname
                                configuration launch-id)))
                      :operation  ':spawn
                      :session-id session-id))
             (when ready-function
               (funcall ready-function session-id))
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

(-> localgroup-world--wait-for-fresh (configuration string integer) (option string))
(defun localgroup-world--wait-for-fresh (configuration token old-pid)
  "Resolve a private launch TOKEN to the world endpoint a new process published.

Two terminals starting at once both spawn a server, and only one takes the
world's lease; the other process ends without publishing. The world has one
endpoint whichever process serves it, so a client whose own launch lost the
race takes the endpoint that answers instead of waiting out its token."
  (image-daemon:daemon-wait-until
   (lambda ()
     (loop for entry in (image-daemon:daemon-registry-discover
                         (localgroup-world-directory configuration))
           thereis (and (or (image-daemon:daemon-endpoint-status (rest entry) token old-pid)
                            (handler-case
                                (localgroup-query-record entry ':status)
                              (localgroup-error ()
                                nil)))
                        (getf (rest (rest entry)) :session-id))))
   *localgroup-handoff-start-timeout-seconds*))

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

(-> localgroup-handoff-finish-startup ((option application)) null)
(defun localgroup-handoff-finish-startup (application)
  "Delete the consumed startup handoff after APPLICATION, or the world server, publishes its endpoint."
  (declare (ignore application))
  (let ((pathname
          (and *localgroup-startup-record*
               (or (getf (rest *localgroup-startup-record*) :pending-pathname)
                   (getf (rest *localgroup-startup-record*) :pathname)))))
    (when pathname
      (image-daemon:handoff-ticket-delete pathname)))
  nil)
