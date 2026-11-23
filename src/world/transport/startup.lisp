(in-package #:autolith)

;;;; -- Detached Process Handoff --

(defparameter *world-startup-version* 1
  "The detached-process handoff snapshot version.")

(defparameter *world-startup-stop-grace-seconds* 1/5
  "The grace period between terminating and killing a failed replacement.")

(defparameter *world-startup-stop-function*
  (lambda (process handoff-pathname)
    (world-startup--stop-replacement process handoff-pathname))
  "The failed replacement termination boundary used by process handoff.")

(defparameter *world-startup-setsid-function*
  (lambda ()
    (platform-detach-session *platform*))
  "The session-detachment boundary used during replacement startup.")

(defparameter *world-startup-launch-function*
  (lambda (configuration &key session-id handoff-pathname permission-mode
                             immutable-p pristine-p)
    (world-startup--launch-for
     configuration session-id
     :handoff-pathname handoff-pathname
     :permission-mode  permission-mode
     :immutable-p      immutable-p
     :pristine-p       pristine-p))
  "The detached launch boundary used by client-first session starts.")

(defparameter *world-startup-wait-function*
  (lambda (configuration token old-pid &key alive-p)
    (world-startup--wait-for-fresh configuration token old-pid :alive-p alive-p))
  "The authenticated discovery boundary returning a fresh world server's identifier.")


;;;; -- Private Records --

(-> world-startup-directory (configuration) pathname)
(defun world-startup-directory (configuration)
  "Return CONFIGURATION's private detached-process handoff directory."
  (merge-pathnames "startups/" (world-endpoint-directory configuration)))

(-> world-startup-log-pathname (configuration string) pathname)
(defun world-startup-log-pathname (configuration session-id)
  "Return the private detached process log for SESSION-ID."
  (merge-pathnames
   (make-pathname :name session-id :type "log")
   (merge-pathnames "logs/" (world-endpoint-directory configuration))))

(-> world-startup--pathname (configuration string) pathname)
(defun world-startup--pathname (configuration session-id)
  "Return a fresh private handoff ticket pathname for SESSION-ID."
  (image-daemon:handoff-ticket-pathname (world-startup-directory configuration)
                                        session-id))

(-> world-startup--timestamp-p (t) boolean)
(defun world-startup--timestamp-p (value)
  "Return true when VALUE is a handoff timestamp."
  (typep value 'timestamp))

(-> world-startup--state-p (t) boolean)
(defun world-startup--state-p (value)
  "Return true when VALUE names a supported handoff state."
  (not (null (member value '(:pending :claimed :cancelled)))))

(-> world-startup--positive-integer-p (t) boolean)
(defun world-startup--positive-integer-p (value)
  "Return true when VALUE is a positive process identifier."
  (typep value '(integer 1)))

(-> world-startup--optional-positive-integer-p (t) boolean)
(defun world-startup--optional-positive-integer-p (value)
  "Return true when VALUE is NIL or a positive process identifier."
  (or (null value) (world-startup--positive-integer-p value)))

(-> world-startup--record-p (t) boolean)
(defun world-startup--record-p (record)
  "Return true when RECORD is one supported world startup ticket."
  (record-check
   record :tag ':world-startup :versions (list *world-startup-version*)
   :allow-other-keys nil :keyword-keys-p t :maximum-length 24
   :fields
   `((:indicator :launch-id :required t :validate non-empty-string-p)
     (:indicator :token :required t :validate non-empty-string-p)
     (:indicator :created-at :required t :validate world-startup--timestamp-p)
     (:indicator :state :required t :validate world-startup--state-p)
     (:indicator :attach-expected-p :validate ,(lambda (value) (typep value 'boolean)))
     (:indicator :styled-p :validate ,(lambda (value) (typep value 'boolean)))
     (:indicator :rows :validate world-startup--positive-integer-p)
     (:indicator :columns :validate world-startup--positive-integer-p)
     (:indicator :old-pid :required t :validate world-startup--positive-integer-p)
     (:indicator :replacement-pid :validate world-startup--optional-positive-integer-p))))

(-> world-startup--disk-record (list) list)
(defun world-startup--disk-record (record)
  "Return RECORD without its in-memory canonical pathname field."
  (let ((copy (copy-list record)))
    (remf (rest copy) :pathname)
    (remf (rest copy) :pending-pathname)
    copy))

(-> world-startup--write-record (pathname list) null)
(defun world-startup--write-record (pathname record)
  "Atomically write private handoff RECORD to PATHNAME."
  (image-daemon:handoff-ticket-write pathname (world-startup--disk-record record))
  nil)

(-> world-startup--read (configuration pathname) list)
(defun world-startup--read (configuration pathname)
  "Read and validate PATHNAME beneath CONFIGURATION's private handoff root."
  (handler-case
      (let* ((root-pathname (world-startup-directory configuration))
             (root
               (and (probe-file root-pathname)
                    (uiop:ensure-directory-pathname (platform-truename *platform* root-pathname))))
             (canonical     (and (probe-file pathname) (platform-truename *platform* pathname))))
        (unless (and root canonical (uiop:subpathp canonical root))
          (error 'world-terminal-error
                 :message   "The world-terminal handoff path is unavailable or outside private state."
                 :operation ':handoff))
        (multiple-value-bind (record complete-p)
            (snapshot-read canonical)
          (unless (and complete-p (world-startup--record-p record))
            (error 'world-terminal-error
                   :message   "The world-terminal handoff record is malformed."
                   :operation ':handoff))
          (append record (list :pathname canonical))))
    (world-terminal-error (condition)
      (error condition))
    (error (condition)
      (error 'world-terminal-error
             :message   "The world-terminal handoff record could not be read."
             :operation ':handoff
             :cause     condition))))

(-> world-startup-selection (configuration (option string)) (option list))
(defun world-startup-selection (configuration pathname-value)
  "Return the detached-process handoff named by the internal PATHNAME-VALUE."
  (when pathname-value
    (unless (non-empty-string-p pathname-value)
      (error 'world-terminal-error
             :message   "The internal world-terminal handoff option requires a pathname."
             :operation ':arguments))
    (let ((record
            (world-startup--read configuration (pathname pathname-value))))
      (unless (eq (getf (rest record) :state) ':pending)
        (error 'world-terminal-error
               :message    "The world-terminal handoff is no longer pending."
               :operation  ':handoff
               :session-id (getf (rest record) :session-id)))
      record)))

(-> world-startup-begin-startup (list) null)
(defun world-startup-begin-startup (record)
  "Atomically claim RECORD, detach the replacement, and acknowledge its PID."
  (let* ((pending-pathname (getf (rest record) :pathname))
         (claimed-pathname (image-daemon:handoff-ticket-claim pending-pathname)))
    (setf (getf (rest record) :state) ':claimed
          (getf (rest record) :pending-pathname) pending-pathname
          (getf (rest record) :pathname) claimed-pathname
          (getf (rest record) :replacement-pid) (current-process-id))
    (handler-case
        (funcall *world-startup-setsid-function*)
      (error (condition)
        (error 'world-terminal-error
               :message    "The detached world-terminal replacement could not create a new session."
               :operation  ':handoff
               :session-id (getf (rest record) :session-id)
               :cause      condition))))
  nil)

(-> world-startup-assert-startup-active () null)
(defun world-startup-assert-startup-active ()
  "Require the current replacement to retain its claimed handoff record."
  (when *world-startup-record*
    (let* ((expected         (rest *world-startup-record*))
           (pending-pathname (getf expected :pending-pathname))
           (record
             (and pending-pathname
                  (image-daemon:handoff-ticket-owned-record
                   pending-pathname
                   :token    (getf expected :token)
                   :validate #'world-startup--record-p))))
      (unless (and record
                   (equal (or (getf (rest record) :launch-id)
                              (getf (rest record) :session-id))
                          (or (getf expected :launch-id)
                              (getf expected :session-id))))
        (error 'world-terminal-error
               :message    "The world-terminal handoff was cancelled during startup."
               :operation  ':handoff
               :session-id (getf expected :session-id)))))
  nil)


;;;; -- Replacement Process --


(-> world-startup-permission-string (keyword) string)
(defun world-startup-permission-string (mode)
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

(-> world-startup--launch-supervised
    (&key (:arguments list)
          (:handoff-pathname pathname)
          (:directory pathname)
          (:output stream))
    t)
(defun world-startup--launch-supervised
    (&key arguments handoff-pathname directory output)
  "Launch ARGUMENTS behind the platform's gated process-tree supervisor.

Signal PLATFORM-CAPABILITY-UNAVAILABLE on hosts without detached sessions
before any shell is involved."
  (unless (platform-supports-p *platform* ':detached-sessions)
    (error 'platform-capability-unavailable
           :capability ':detached-sessions
           :message    "This host cannot supervise a detached session process, so world-terminal handoff is withheld."))
  (platform-launch-detached-process *platform* arguments
                                    :directory directory
                                    :output    output
                                    :ticket    handoff-pathname))

(-> world-startup--arguments
    (configuration pathname &key (:permission-mode keyword) (:immutable-p boolean)
                                 (:pristine-p boolean))
    list)
(defun world-startup--arguments
    (configuration handoff-pathname &key (permission-mode ':ask) immutable-p pristine-p)
  "Return the platform launch command and handoff arguments for CONFIGURATION."
  (append
   (generation-session-launch-command configuration)
   (list "--permissions" (world-startup-permission-string permission-mode))
   (when (config :model-selection-override-p configuration)
     (append (list "--model" (config :model configuration))
             (when (non-empty-string-p (config :model-provider configuration))
               (list "--provider" (config :model-provider configuration)))))
   (when immutable-p
     (list "--immutable"))
   (when pristine-p
     (list "--pristine"))
   (when-let ((site-config-root (config :site-config-root configuration)))
     (list "--site-config-root" (namestring site-config-root)))
   (list "--world-startup" (namestring handoff-pathname))))

(-> world-startup--launch-for
    (configuration string &key (:handoff-pathname pathname) (:permission-mode keyword)
                               (:immutable-p boolean) (:pristine-p boolean))
    t)
(defun world-startup--launch-for
    (configuration session-id &key handoff-pathname (permission-mode ':ask) immutable-p pristine-p)
  "Launch one detached session process from HANDOFF-PATHNAME."
  (let ((arguments
          (world-startup--arguments
           configuration handoff-pathname
           :permission-mode permission-mode
           :immutable-p     immutable-p
           :pristine-p      pristine-p))
        (log-pathname
          (world-startup-log-pathname configuration session-id)))
    (ensure-directories-exist log-pathname)
    (with-open-file (output log-pathname
                            :direction         ':output
                            :if-exists         ':append
                            :if-does-not-exist ':create
                            :external-format   ':utf-8)
      (platform-make-private *platform* log-pathname)
      (world-startup--launch-supervised
       :arguments        arguments
       :handoff-pathname handoff-pathname
       :directory        (config :working-directory configuration)
       :output           output))))

(-> world-startup-spawn-fresh
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:attach-expected-p boolean)
                        (:ready-function (option function))
                        (:pristine-p boolean))
    string)
(defun world-startup-spawn-fresh
    (configuration &key (permission-mode ':ask) immutable-p pristine-p
                        (attach-expected-p t) ready-function)
  "Start a supervised world server and return its authenticated ready identifier.

Call READY-FUNCTION with the identifier before releasing startup ownership.
A nonlocal exit from that callback cancels the launch."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (let* ((created-at  (get-universal-time))
           (launch-id   (daemon-random-nonce))
           (token       (daemon-random-token))
           (pathname    (world-startup--pathname configuration launch-id))
           (record
             (list :world-startup
                   :version *world-startup-version*
                   :launch-id         launch-id
                   :token             token
                   :created-at        created-at
                   :state             ':pending
                   :attach-expected-p (not (null attach-expected-p))
                   :old-pid           (current-process-id)
                   :replacement-pid   nil
                   :rows              rows
                   :columns           columns
                   :styled-p          (not (null (terminal-environment-styling-p)))))
           (completed-p nil)
           (process     nil)
           (session-id  nil))
      (world-startup--write-record pathname record)
      (unwind-protect
           (progn
             (setf process
                    (funcall *world-startup-launch-function*
                             configuration
                             :session-id       launch-id
                             :handoff-pathname pathname
                             :permission-mode  permission-mode
                             :immutable-p      immutable-p
                             :pristine-p       pristine-p))
             (setf session-id
                   (funcall *world-startup-wait-function*
                             configuration token (current-process-id)
                             :alive-p (lambda () (world-startup--replacement-alive-p process pathname))))
             (unless session-id
               (error 'world-terminal-error
                      :message
                      (format nil
                               "The world server ended before publishing an endpoint. See ~A."
                              (namestring
                               (world-startup-log-pathname
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
              (funcall *world-startup-stop-function* process pathname)))
          (image-daemon:handoff-ticket-delete pathname))))))

(-> world-startup--wait-for-fresh
    (configuration string integer &key (:alive-p (option function))) (option string))
(defun world-startup--wait-for-fresh (configuration token old-pid &key alive-p)
  "Resolve a private launch TOKEN while its replacement or the world server is alive.

Two terminals starting at once both spawn a server, and only one takes the
world's lease. Accept the endpoint that answers even when the client's own
replacement has ended after losing that race. ALIVE-P reports whether any
process belonging to this launch is still alive."
  (let ((image-daemon:*daemon-connect-timeout-seconds* nil))
    (loop
      (when-let ((identifier
                  (loop for entry in (image-daemon:daemon-registry-discover
                                      (world-endpoint-directory configuration))
                        thereis (and (or (image-daemon:daemon-endpoint-status (rest entry) token old-pid)
                                         (handler-case
                                             (world-terminal-query-record entry ':status)
                                           (world-terminal-error () nil)))
                                     (getf (rest (rest entry)) :session-id)))))
        (return identifier))
      (when (and alive-p (not (funcall alive-p))
                 (not (world-server-running-p configuration)))
        (return nil))
      (sleep 0.05))))

(-> world-startup--signal-pid (integer boolean) null)
(defun world-startup--signal-pid (pid force-p)
  "Best-effort terminate PID or process group -PID, forcibly when FORCE-P."
  (handler-case
      (if (minusp pid)
          (platform-terminate-process-group *platform* (- pid) :force force-p)
          (platform-terminate-process *platform* pid :force force-p))
    (error () nil))
  nil)

(-> world-startup--cancel (pathname) pathname)
(defun world-startup--cancel (pathname)
  "Atomically invalidate PATHNAME's pending or claimed startup ownership."
  (image-daemon:handoff-ticket-cancel pathname :validate #'world-startup--record-p))

(-> world-startup--pid-alive-p (integer) boolean)
(defun world-startup--pid-alive-p (pid)
  "Return true when PID or process group -PID is still alive."
  (handler-case
      (if (minusp pid)
          (platform-process-group-alive-p *platform* (- pid))
          (platform-process-alive-p *platform* pid))
    (error () nil)))

(-> world-startup--process-object-pid (t) (option integer))
(defun world-startup--process-object-pid (process)
  "Return PROCESS's adapter PID."
  (platform-process-object-pid *platform* process))

(-> world-startup--process-object-alive-p (t) boolean)
(defun world-startup--process-object-alive-p (process)
  "Return whether PROCESS is alive through its platform adapter."
  (and (platform-process-object-alive-p *platform* process) t))

(-> world-startup--replacement-alive-p (t pathname) boolean)
(defun world-startup--replacement-alive-p (process handoff-pathname)
  "Return whether PROCESS or its recorded launcher or replacement is alive."
  (or (world-startup--process-object-alive-p process)
      (some (lambda (pid)
              (and pid
                   (or (world-startup--pid-alive-p pid)
                       (world-startup--pid-alive-p (- pid)))))
            (list (image-daemon:handoff-ticket-launcher-pid handoff-pathname)
                  (image-daemon:handoff-ticket-replacement-pid
                   handoff-pathname :validate #'world-startup--record-p)))))

(-> world-startup--terminate-process-object (t t) null)
(defun world-startup--terminate-process-object (process force-p)
  "Terminate PROCESS through its platform adapter, forcefully when FORCE-P."
  (platform-terminate-process-object *platform* process :force force-p)
  nil)

(-> world-startup--stop-replacement (t pathname) boolean)
(defun world-startup--stop-replacement (process handoff-pathname)
  "Cancel, terminate, and positively reap PROCESS and its replacement session."
  (world-startup--cancel handoff-pathname)
  (let* ((root-pid   (world-startup--process-object-pid process))
         (known-pids nil)
         (started-at (get-internal-real-time))
         (kill-at
           (+ started-at
              (* *world-startup-stop-grace-seconds*
                 internal-time-units-per-second)))
         (deadline   (+ started-at (* 5 internal-time-units-per-second))))
    (loop
      (let* ((now     (get-internal-real-time))
             (force-p (>= now kill-at))
             (launcher-pid
               (image-daemon:handoff-ticket-launcher-pid handoff-pathname))
             (replacement-pid
               (image-daemon:handoff-ticket-replacement-pid
                handoff-pathname :validate #'world-startup--record-p)))
        (when root-pid
          (setf known-pids
                (remove-duplicates
                 (append (descendant-process-ids root-pid) known-pids)
                 :test #'=)))
        (when launcher-pid
          (world-startup--signal-pid launcher-pid force-p)
          (world-startup--signal-pid (- launcher-pid) force-p))
        (when replacement-pid
          (world-startup--signal-pid replacement-pid force-p)
          (world-startup--signal-pid (- replacement-pid) force-p))
        (dolist (pid known-pids)
          (world-startup--signal-pid pid force-p))
        (when root-pid
          (world-startup--signal-pid root-pid force-p))
        ;; Discover descendants before forcefully closing the platform object.
        (when (and force-p
                   (world-startup--process-object-alive-p process))
          (world-startup--terminate-process-object process t))
        (setf known-pids
              (remove-if-not #'world-startup--pid-alive-p known-pids))
        (let ((root-alive-p
                  (or (and root-pid (world-startup--pid-alive-p root-pid))
                      (world-startup--process-object-alive-p process)))
              (launcher-alive-p
                (and launcher-pid
                     (or (world-startup--pid-alive-p launcher-pid)
                         (world-startup--pid-alive-p (- launcher-pid)))))
              (replacement-alive-p
                (and replacement-pid
                     (or (world-startup--pid-alive-p replacement-pid)
                         (world-startup--pid-alive-p
                          (- replacement-pid))))))
          (unless (or root-alive-p launcher-alive-p replacement-alive-p
                      known-pids)
                (platform-wait-process *platform* process)
            (return t)))
        (when (>= now deadline)
          (error 'world-terminal-error
                 :message   "The failed detached replacement could not be terminated safely."
                 :operation ':handoff))
        (sleep 0.05)))))

(-> world-startup-finish-startup ((option application)) null)
(defun world-startup-finish-startup (application)
  "Delete the consumed startup handoff after APPLICATION, or the world server, publishes its endpoint."
  (declare (ignore application))
  (when-let ((pathname
               (and *world-startup-record*
                    (or (getf (rest *world-startup-record*) :pending-pathname)
                        (getf (rest *world-startup-record*) :pathname)))))
    (image-daemon:handoff-ticket-delete pathname))
  nil)
