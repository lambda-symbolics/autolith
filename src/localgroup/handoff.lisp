(in-package #:autolith)

;;;; -- Detached Process Handoff --

(defparameter *localgroup-handoff-version* 1
  "The detached-process handoff snapshot version.")

(defparameter *localgroup-handoff-start-timeout-seconds* 20
  "The maximum time an old process waits for its detached replacement.")

(defparameter *localgroup-handoff-stop-grace-seconds* 1/5
  "The grace period between terminating and killing a failed replacement.")

(defparameter *localgroup-handoff-supervisor-script*
  "set -u
launcher=$1
pidfile=$2
gate=$3
shift 3
child=
cleanup()
{
  status=$?
  trap - EXIT HUP INT TERM
  if [[ -n \"$child\" ]]; then
    kill -TERM -- \"-$child\" 2>/dev/null || true
    wait \"$child\" 2>/dev/null || true
  fi
  rm -f -- \"$gate\"
  exit \"$status\"
}
trap cleanup EXIT HUP INT TERM
rm -f -- \"$gate\" \"$pidfile\"
mkfifo -m 600 -- \"$gate\"
set -m
(
  IFS= read -r ready < \"$gate\" || exit 70
  exec \"$launcher\" \"$@\"
) &
child=$!
printf '%s\\n' \"$child\" > \"$pidfile\"
chmod 600 \"$pidfile\"
printf 'start\\n' > \"$gate\"
rm -f -- \"$gate\"
wait \"$child\"
status=$?
child=
exit \"$status\""
  "The Bash supervisor that gates and owns the replacement process group.")

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
    (localgroup-handoff--wait-for-replacement
     configuration session-id token old-pid))
  "The authenticated replacement readiness boundary used by process handoff.")

(defparameter *localgroup-handoff-setsid-function* #'sb-posix:setsid
  "The session-detachment boundary used during replacement startup.")

(defparameter *localgroup-fresh-launch-function*
  (lambda (configuration session-id handoff-pathname permission-argument
           immutable-p)
    (localgroup-handoff--launch-for
     configuration session-id handoff-pathname permission-argument
     immutable-p))
  "The detached launch boundary used by client-first session starts.")


;;;; -- Private Records --

(-> localgroup-handoff-directory (configuration) pathname)
(defun localgroup-handoff-directory (configuration)
  "Return CONFIGURATION's private detached-process handoff directory."
  (merge-pathnames "localgroup/handoffs/"
                   (configuration-state-root configuration)))

(-> localgroup-handoff-log-pathname (configuration string) pathname)
(defun localgroup-handoff-log-pathname (configuration session-id)
  "Return the private detached process log for SESSION-ID."
  (merge-pathnames
   (make-pathname :name session-id :type "log")
   (merge-pathnames "localgroup/logs/"
                    (configuration-state-root configuration))))

(-> localgroup-handoff--pathname (configuration string) pathname)
(defun localgroup-handoff--pathname (configuration session-id)
  "Return a fresh private handoff pathname for SESSION-ID."
  (merge-pathnames
   (make-pathname
    :name (format nil "~A-~A" session-id (daemon-random-nonce))
    :type "sexp")
   (localgroup-handoff-directory configuration)))

(-> localgroup-handoff--state-pathname (pathname keyword) pathname)
(defun localgroup-handoff--state-pathname (pathname state)
  "Return PATHNAME's sibling handoff pathname for STATE."
  (make-pathname
   :name (format nil "~A-~(~A~)" (pathname-name pathname) state)
   :defaults pathname))

(-> localgroup-handoff--claimed-pathname (pathname) pathname)
(defun localgroup-handoff--claimed-pathname (pathname)
  "Return PATHNAME's atomically claimed replacement pathname."
  (localgroup-handoff--state-pathname pathname ':claimed))

(-> localgroup-handoff--cancelled-pathname (pathname) pathname)
(defun localgroup-handoff--cancelled-pathname (pathname)
  "Return PATHNAME's atomically cancelled replacement pathname."
  (localgroup-handoff--state-pathname pathname ':cancelled))

(-> localgroup-handoff--pid-pathname (pathname) pathname)
(defun localgroup-handoff--pid-pathname (pathname)
  "Return PATHNAME's replacement-process acknowledgement pathname."
  (localgroup-handoff--state-pathname pathname ':pid))

(-> localgroup-handoff--launcher-pid-pathname (pathname) pathname)
(defun localgroup-handoff--launcher-pid-pathname (pathname)
  "Return PATHNAME's supervised launcher process-group acknowledgement."
  (localgroup-handoff--state-pathname pathname ':launcher-pid))

(-> localgroup-handoff--gate-pathname (pathname) pathname)
(defun localgroup-handoff--gate-pathname (pathname)
  "Return PATHNAME's temporary launcher start-gate pathname."
  (localgroup-handoff--state-pathname pathname ':gate))

(-> localgroup-handoff--record-p (t) boolean)
(defun localgroup-handoff--record-p (record)
  "Return true when RECORD is one supported detached-process handoff."
  (and (localgroup--proper-list-p record)
       (eq (first record) ':localgroup-handoff)
       (= (or (getf (rest record) :version) 0)
          *localgroup-handoff-version*)
       (non-empty-string-p (getf (rest record) :session-id))
       (non-empty-string-p (getf (rest record) :token))
       (typep (getf (rest record) :created-at) 'timestamp)
       (member (getf (rest record) :mode) '(:detach :take-over))
       (member (getf (rest record) :state) '(:pending :claimed :cancelled))
       (typep (getf (rest record) :fresh-conversation-p) 'boolean)
       (typep (getf (rest record) :old-pid) '(integer 1))
       (let ((replacement-pid (getf (rest record) :replacement-pid)))
         (or (null replacement-pid)
             (typep replacement-pid '(integer 1))))
       (let ((conversation-id (getf (rest record) :conversation-id)))
         (or (null conversation-id)
             (identifier-p conversation-id)))
       (stringp (or (getf (rest record) :draft) ""))))

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
  (ensure-directories-exist pathname)
  (sb-posix:chmod (namestring (uiop:pathname-directory-pathname pathname)) #o700)
  (snapshot-write pathname (localgroup-handoff--disk-record record))
  (sb-posix:chmod (namestring pathname) #o600)
  nil)

(-> localgroup-handoff--write
    (application localgroup-session keyword)
    pathname)
(defun localgroup-handoff--write (application session mode)
  "Publish APPLICATION's detached-process handoff for SESSION and MODE."
  (let* ((configuration (application-configuration application))
         (conversation (application-conversation application))
         (pathname
           (localgroup-handoff--pathname
            configuration (localgroup-session-identifier session)))
         (draft
           (line-editor-text
            (terminal-ui-editor (application-ui application))))
         (record
           (list :localgroup-handoff
                 :version *localgroup-handoff-version*
                 :session-id (localgroup-session-identifier session)
                 :token (localgroup-session-token session)
                 :created-at (localgroup-session-created-at session)
                 :mode mode
                 :state ':pending
                 :fresh-conversation-p
                 (not (conversation-persisted-p conversation))
                 :old-pid (sb-posix:getpid)
                 :replacement-pid nil
                 :conversation-id
                 (and (conversation-persisted-p conversation)
                      (conversation-identifier conversation))
                 :draft draft)))
    (localgroup-handoff--write-record pathname record)
    pathname))

(-> localgroup-handoff--read (configuration pathname) list)
(defun localgroup-handoff--read (configuration pathname)
  "Read and validate PATHNAME beneath CONFIGURATION's private handoff root."
  (handler-case
      (let* ((root-pathname (localgroup-handoff-directory configuration))
             (root
               (and (probe-file root-pathname)
                    (uiop:ensure-directory-pathname (truename root-pathname))))
             (canonical (and (probe-file pathname) (truename pathname))))
        (unless (and root canonical (uiop:subpathp canonical root))
          (error 'localgroup-error
                 :message "The localgroup handoff path is unavailable or outside private state."
                 :operation ':handoff))
        (multiple-value-bind (record complete-p)
            (snapshot-read canonical)
          (unless (and complete-p (localgroup-handoff--record-p record))
            (error 'localgroup-error
                   :message "The localgroup handoff record is malformed."
                   :operation ':handoff))
          (append record (list :pathname canonical))))
    (localgroup-error (condition)
      (error condition))
    (error (condition)
      (error 'localgroup-error
             :message "The localgroup handoff record could not be read."
             :operation ':handoff
             :cause condition))))

(-> localgroup-handoff-selection (configuration (option string)) (option list))
(defun localgroup-handoff-selection (configuration pathname-value)
  "Return the detached-process handoff named by the internal PATHNAME-VALUE."
  (when pathname-value
    (unless (non-empty-string-p pathname-value)
      (error 'localgroup-error
             :message "The internal localgroup handoff option requires a pathname."
             :operation ':arguments))
    (let ((record
            (localgroup-handoff--read configuration (pathname pathname-value))))
      (unless (eq (getf (rest record) :state) ':pending)
        (error 'localgroup-error
               :message "The localgroup handoff is no longer pending."
               :operation ':handoff
               :session-id (getf (rest record) :session-id)))
      record)))

(-> localgroup-handoff-begin-startup (list) null)
(defun localgroup-handoff-begin-startup (record)
  "Atomically claim RECORD, detach the replacement, and acknowledge its PID."
  (let* ((pending-pathname (getf (rest record) :pathname))
         (claimed-pathname
           (localgroup-handoff--claimed-pathname pending-pathname)))
    (handler-case
        (rename-file pending-pathname claimed-pathname)
      (error (condition)
        (error 'localgroup-error
               :message "The localgroup handoff was cancelled before startup."
               :operation ':handoff
               :session-id (getf (rest record) :session-id)
               :cause condition)))
    (setf (getf (rest record) :state) ':claimed
          (getf (rest record) :pending-pathname) pending-pathname
          (getf (rest record) :pathname) claimed-pathname
          (getf (rest record) :replacement-pid) (sb-posix:getpid))
    (localgroup-handoff--write-record
     (localgroup-handoff--pid-pathname pending-pathname)
     (list :localgroup-handoff-pid
           :pid (sb-posix:getpid)))
    (handler-case
        (funcall *localgroup-handoff-setsid-function*)
      (error (condition)
        (error 'localgroup-error
               :message "The detached localgroup replacement could not create a new session."
               :operation ':handoff
               :session-id (getf (rest record) :session-id)
               :cause condition))))
  nil)

(-> localgroup-handoff-assert-startup-active () null)
(defun localgroup-handoff-assert-startup-active ()
  "Require the current replacement to retain its claimed handoff record."
  (when *localgroup-startup-record*
    (let* ((expected (rest *localgroup-startup-record*))
           (pathname (getf expected :pathname))
           (pending-pathname (getf expected :pending-pathname))
           (record
             (and pathname
                  (probe-file pathname)
                  (localgroup-handoff--record-at pathname)))
           (pid-record
             (and pending-pathname
                  (probe-file (localgroup-handoff--pid-pathname pending-pathname))
                  (handler-case
                      (multiple-value-bind (value complete-p)
                          (snapshot-read
                           (localgroup-handoff--pid-pathname pending-pathname))
                        (and complete-p value))
                    (error () nil)))))
      (unless (and record
                   (member (getf (rest record) :state) '(:pending :claimed))
                   (string= (getf (rest record) :session-id)
                            (getf expected :session-id))
                   (string= (getf (rest record) :token)
                            (getf expected :token))
                   (eq (first pid-record) ':localgroup-handoff-pid)
                   (= (or (getf (rest pid-record) :pid) 0)
                      (sb-posix:getpid)))
        (error 'localgroup-error
               :message "The localgroup handoff was cancelled during startup."
               :operation ':handoff
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
    (:ask "ask")
    (:auto "auto")
    (:sandboxed "sandbox")
    (:full-access "full")))

(-> localgroup-handoff--launcher-pathname (configuration) pathname)
(defun localgroup-handoff--launcher-pathname (configuration)
  "Return CONFIGURATION's executable stable source launcher pathname."
  (let ((pathname
          (merge-pathnames "bin/autolith"
                           (configuration-source-root configuration))))
    (unless (and (probe-file pathname)
                 (handler-case
                     (zerop (sb-posix:access (namestring pathname) sb-posix:x-ok))
                   (error () nil)))
      (error 'localgroup-error
             :message "The stable Autolith launcher is unavailable for detach."
             :operation ':handoff))
    pathname))

(-> localgroup-handoff--launch-supervised
    (&key (:arguments list)
          (:handoff-pathname pathname)
          (:directory pathname)
          (:output stream))
    t)
(defun localgroup-handoff--launch-supervised
    (&key arguments handoff-pathname directory output)
  "Launch ARGUMENTS behind a gated Bash process-group supervisor."
  (let ((launcher-pid-pathname
          (localgroup-handoff--launcher-pid-pathname handoff-pathname))
        (gate-pathname (localgroup-handoff--gate-pathname handoff-pathname)))
    (dolist (pathname (list launcher-pid-pathname gate-pathname))
      (when (probe-file pathname)
        (ignore-errors (delete-file pathname))))
    (uiop:launch-program
     (append
      (list "bash" "-c" *localgroup-handoff-supervisor-script*
            "autolith-localgroup-handoff"
            (first arguments)
            (namestring launcher-pid-pathname)
            (namestring gate-pathname))
      (rest arguments))
     :input nil
     :output output
     :error-output ':output
     :directory directory
     :wait nil)))

(-> localgroup-handoff--arguments (application pathname) list)
(defun localgroup-handoff--arguments (application handoff-pathname)
  "Return replacement launcher arguments for APPLICATION and HANDOFF-PATHNAME."
  (let* ((configuration (application-configuration application))
         (launcher (localgroup-handoff--launcher-pathname configuration)))
    (append
     (list (namestring launcher)
           "--permissions"
           (localgroup-handoff--permission-argument application))
     (when (configuration-immutable-p configuration)
       (list "--immutable"))
     (let ((site-config-root
             (configuration-site-config-root configuration)))
       (when site-config-root
         (list "--site-config-root"
               (namestring site-config-root))))
     (list "--localgroup-handoff" (namestring handoff-pathname)))))

(-> localgroup-handoff--launch (application pathname) t)
(defun localgroup-handoff--launch (application handoff-pathname)
  "Launch APPLICATION's detached replacement from HANDOFF-PATHNAME."
  (let* ((configuration (application-configuration application))
         (session-id
           (localgroup-session-identifier
            (application-localgroup-session application)))
         (log-pathname
           (localgroup-handoff-log-pathname configuration session-id))
         (arguments
           (localgroup-handoff--arguments application handoff-pathname)))
    (ensure-directories-exist log-pathname)
    (with-open-file (output log-pathname
                            :direction ':output
                            :if-exists ':append
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (sb-posix:chmod (namestring log-pathname) #o600)
      (localgroup-handoff--launch-supervised
       :arguments arguments
       :handoff-pathname handoff-pathname
       :directory (configuration-working-directory configuration)
       :output output))))

(-> localgroup-handoff--launch-for
    (configuration string pathname string boolean)
    t)
(defun localgroup-handoff--launch-for
    (configuration session-id handoff-pathname permission-argument immutable-p)
  "Launch one detached session process from HANDOFF-PATHNAME."
  (let ((launcher (localgroup-handoff--launcher-pathname configuration))
        (log-pathname
          (localgroup-handoff-log-pathname configuration session-id)))
    (let ((arguments
            (append
             (list (namestring launcher)
                   "--permissions" permission-argument)
             (when immutable-p
               (list "--immutable"))
             (let ((site-config-root
                     (configuration-site-config-root configuration)))
               (when site-config-root
                 (list "--site-config-root"
                       (namestring site-config-root))))
             (list "--localgroup-handoff" (namestring handoff-pathname)))))
      (ensure-directories-exist log-pathname)
      (with-open-file (output log-pathname
                              :direction ':output
                              :if-exists ':append
                              :if-does-not-exist ':create
                              :external-format ':utf-8)
        (sb-posix:chmod (namestring log-pathname) #o600)
        (localgroup-handoff--launch-supervised
         :arguments arguments
         :handoff-pathname handoff-pathname
         :directory (configuration-working-directory configuration)
         :output output)))))

(-> localgroup-handoff-spawn-fresh
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:conversation-id (option string))
                        (:resume-command-p boolean)
                        (:recovery-diagnosis t))
    string)
(defun localgroup-handoff-spawn-fresh
    (configuration &key (permission-mode ':ask) immutable-p conversation-id
                        resume-command-p recovery-diagnosis)
  "Launch a detached session and return its ready identifier.

CONVERSATION-ID resumes that conversation instead of starting a fresh
one, RESUME-COMMAND-P lets the session pick one itself through the
attached terminal, and RECOVERY-DIAGNOSIS preserves crash diagnosis
across the client-first launch. The session process starts shell-independent
from the outset, so the calling terminal can attach as a thin relay whose
detach is immediate and never interrupts session work."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (let* ((created-at (get-universal-time))
           (session-id
             (session-identifier-normalize
              (localgroup-session-identifier-generate configuration created-at)))
           (token (daemon-random-token))
           (pathname (localgroup-handoff--pathname configuration session-id))
           (record
             (list :localgroup-handoff
                   :version *localgroup-handoff-version*
                   :session-id session-id
                   :token token
                   :created-at created-at
                   :mode ':detach
                   :state ':pending
                   :fresh-conversation-p (and (null conversation-id)
                                              (not resume-command-p))
                   :resume-command-p (not (null resume-command-p))
                   :old-pid (sb-posix:getpid)
                   :replacement-pid nil
                   :conversation-id conversation-id
                    :recovery-diagnosis recovery-diagnosis
                   :draft ""
                   :rows rows
                   :columns columns
                   :styled-p (not (null (terminal-environment-styling-p)))))
           (completed-p nil)
           (process nil))
      (localgroup-handoff--write-record pathname record)
      (unwind-protect
           (progn
             (setf process
                   (funcall *localgroup-fresh-launch-function*
                            configuration session-id pathname
                            (localgroup-handoff-permission-string permission-mode)
                            immutable-p))
             (unless (funcall *localgroup-handoff-wait-function*
                              configuration session-id token (sb-posix:getpid))
               (error 'localgroup-error
                      :message
                      (format nil
                              "The detached session did not start within ~D seconds. See ~A."
                              *localgroup-handoff-start-timeout-seconds*
                              (namestring
                               (localgroup-handoff-log-pathname
                                configuration session-id)))
                      :operation ':spawn
                      :session-id session-id))
             (setf completed-p t)
             session-id)
        (unless completed-p
          (when process
            (ignore-errors
              (funcall *localgroup-handoff-stop-function* process pathname)))
          (localgroup-handoff--delete-state-pathnames pathname))))))

(-> localgroup-handoff--replacement-ready-p
    (configuration string string integer)
    boolean)
(defun localgroup-handoff--replacement-ready-p
    (configuration session-id token old-pid)
  "Return true when SESSION-ID names a live replacement distinct from OLD-PID."
  (let* ((pathname (localgroup-registry-pathname configuration session-id))
         (record (localgroup--read-endpoint-record pathname)))
    (and record
         (/= (getf (rest record) :pid) old-pid)
         (string= (getf (rest record) :token) token)
         (handler-case
             (eq
              (first
               (daemon-call
                (getf (rest record) :port)
                token
                ':status))
              ':ok)
           (error () nil)))))

(-> localgroup-handoff--wait-for-replacement
    (configuration string string integer)
    boolean)
(defun localgroup-handoff--wait-for-replacement
    (configuration session-id token old-pid)
  "Wait boundedly for a live replacement endpoint."
  (let ((deadline
          (+ (get-internal-real-time)
             (* *localgroup-handoff-start-timeout-seconds*
                internal-time-units-per-second))))
    (loop
      (when (localgroup-handoff--replacement-ready-p
             configuration session-id token old-pid)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.05))))

(-> localgroup-handoff--process-pairs () list)
(defun localgroup-handoff--process-pairs ()
  "Return best-effort process PID and parent-PID pairs from the host."
  (handler-case
      (let ((output
              (uiop:run-program
               '("ps" "-ax" "-o" "pid=" "-o" "ppid=")
               :output ':string
               :ignore-error-status t)))
        (loop for line in (uiop:split-string output :separator '(#\Newline))
              for fields =
                (remove ""
                        (uiop:split-string line :separator '(#\Space #\Tab))
                        :test #'string=)
              when (= (length fields) 2)
                collect (cons (parse-integer (first fields))
                              (parse-integer (second fields)))))
    (error () nil)))

(-> localgroup-handoff--descendant-pids (integer list) list)
(defun localgroup-handoff--descendant-pids (root-pid pairs)
  "Return ROOT-PID's descendants ordered deepest first."
  (labels ((collect-children (parent)
             "Return PARENT's recursive descendants with children first."
             (loop for (pid . parent-pid) in pairs
                   when (= parent-pid parent)
                     append (append (collect-children pid) (list pid)))))
    (remove-duplicates (collect-children root-pid) :test #'=)))

(-> localgroup-handoff--signal-pid (integer integer) null)
(defun localgroup-handoff--signal-pid (pid signal)
  "Best-effort send SIGNAL to PID or process group -PID."
  (handler-case
      (sb-posix:kill pid signal)
    (error () nil))
  nil)

(-> localgroup-handoff--record-at (pathname) (option list))
(defun localgroup-handoff--record-at (pathname)
  "Return PATHNAME's complete valid handoff record, when present."
  (handler-case
      (multiple-value-bind (record complete-p)
          (snapshot-read pathname)
        (and complete-p
             (localgroup-handoff--record-p record)
             record))
    (error () nil)))

(-> localgroup-handoff--state-pathnames (pathname) list)
(defun localgroup-handoff--state-pathnames (pathname)
  "Return PATHNAME and its claimed and cancelled siblings."
  (list pathname
        (localgroup-handoff--claimed-pathname pathname)
        (localgroup-handoff--cancelled-pathname pathname)
        (localgroup-handoff--pid-pathname pathname)
        (localgroup-handoff--launcher-pid-pathname pathname)
        (localgroup-handoff--gate-pathname pathname)))

(-> localgroup-handoff--plain-pid-at (pathname) (option integer))
(defun localgroup-handoff--plain-pid-at (pathname)
  "Return PATHNAME's one positive decimal PID, when complete."
  (handler-case
      (with-open-file (stream pathname
                              :direction ':input
                              :external-format ':utf-8)
        (let ((line (read-line stream nil nil)))
          (and (non-empty-string-p line)
               (every #'digit-char-p line)
               (let ((pid (parse-integer line)))
                 (and (plusp pid) pid)))))
    (error () nil)))

(-> localgroup-handoff--record-replacement-pid (pathname) (option integer))
(defun localgroup-handoff--record-replacement-pid (pathname)
  "Return PATHNAME family's acknowledged replacement PID, when available."
  (let ((pid-pathname (localgroup-handoff--pid-pathname pathname)))
    (or
     (and (probe-file pid-pathname)
          (handler-case
              (multiple-value-bind (record complete-p)
                  (snapshot-read pid-pathname)
                (and complete-p
                     (eq (first record) ':localgroup-handoff-pid)
                     (typep (getf (rest record) :pid) '(integer 1))
                     (getf (rest record) :pid)))
            (error () nil)))
     (loop for candidate in (localgroup-handoff--state-pathnames pathname)
           for record = (and (probe-file candidate)
                             (localgroup-handoff--record-at candidate))
           for pid = (and record (getf (rest record) :replacement-pid))
           when pid
             return pid))))

(-> localgroup-handoff--cancel (pathname) pathname)
(defun localgroup-handoff--cancel (pathname)
  "Atomically invalidate PATHNAME's pending or claimed startup ownership."
  (let ((claimed (localgroup-handoff--claimed-pathname pathname))
        (cancelled (localgroup-handoff--cancelled-pathname pathname)))
    (labels ((cancel-source (source)
               "Rename SOURCE to CANCELLED and publish its cancelled state."
               (let ((record (localgroup-handoff--record-at source)))
                 (when record
                   (handler-case
                       (progn
                         (rename-file source cancelled)
                         (setf (getf (rest record) :state) ':cancelled)
                         (localgroup-handoff--write-record cancelled record)
                         t)
                     (error () nil))))))
      (or (and (probe-file cancelled) cancelled)
          (and (probe-file pathname)
               (cancel-source pathname)
               cancelled)
          (and (probe-file claimed)
               (cancel-source claimed)
               cancelled)
          (progn
            (sleep 0.05)
            (cond ((probe-file cancelled) cancelled)
                  ((and (probe-file pathname)
                        (cancel-source pathname))
                   cancelled)
                  ((and (probe-file claimed)
                        (cancel-source claimed))
                   cancelled)
                  (t
                   cancelled)))))))

(-> localgroup-handoff--pid-alive-p (integer) boolean)
(defun localgroup-handoff--pid-alive-p (pid)
  "Return true when PID or process group -PID still accepts signal zero."
  (handler-case
      (progn
        (sb-posix:kill pid 0)
        t)
    (error () nil)))

(-> localgroup-handoff--delete-state-pathnames (pathname) null)
(defun localgroup-handoff--delete-state-pathnames (pathname)
  "Delete every pending, claimed, or cancelled record in PATHNAME's family."
  (dolist (candidate (localgroup-handoff--state-pathnames pathname))
    (when (probe-file candidate)
      (ignore-errors (delete-file candidate))))
  nil)

(-> localgroup-handoff--stop-replacement (t pathname) boolean)
(defun localgroup-handoff--stop-replacement (process handoff-pathname)
  "Cancel, terminate, and positively reap PROCESS and its replacement session."
  (localgroup-handoff--cancel handoff-pathname)
  (let* ((root-pid (ignore-errors (uiop:process-info-pid process)))
         (known-pids nil)
         (started-at (get-internal-real-time))
         (kill-at
           (+ started-at
              (* *localgroup-handoff-stop-grace-seconds*
                 internal-time-units-per-second)))
         (deadline (+ started-at (* 5 internal-time-units-per-second))))
    (loop
      (let* ((now (get-internal-real-time))
             (signal (if (>= now kill-at) sb-posix:sigkill sb-posix:sigterm))
             (pairs (localgroup-handoff--process-pairs))
             (launcher-pid
               (localgroup-handoff--plain-pid-at
                (localgroup-handoff--launcher-pid-pathname handoff-pathname)))
             (replacement-pid
               (localgroup-handoff--record-replacement-pid handoff-pathname)))
        (when root-pid
          (setf known-pids
                (remove-duplicates
                 (append
                  (localgroup-handoff--descendant-pids root-pid pairs)
                  known-pids)
                 :test #'=)))
        (when launcher-pid
          (localgroup-handoff--signal-pid launcher-pid signal)
          (localgroup-handoff--signal-pid (- launcher-pid) signal))
        (when replacement-pid
          (localgroup-handoff--signal-pid replacement-pid signal)
          (localgroup-handoff--signal-pid (- replacement-pid) signal))
        (dolist (pid known-pids)
          (localgroup-handoff--signal-pid pid signal))
        (when root-pid
          (localgroup-handoff--signal-pid root-pid signal))
        (setf known-pids
              (remove-if-not #'localgroup-handoff--pid-alive-p known-pids))
        (let ((root-alive-p
                (or (and root-pid (localgroup-handoff--pid-alive-p root-pid))
                    (ignore-errors (uiop:process-alive-p process))))
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
            (ignore-errors (uiop:wait-process process))
            (return t)))
        (when (>= now deadline)
          (error 'localgroup-error
                 :message "The failed detached replacement could not be terminated safely."
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
    (error 'localgroup-error
           :message "The localgroup handoff mode is invalid."
           :operation ':handoff))
  (let ((session (application-localgroup-session application))
        (controller (application-input-controller application)))
    (unless (and session controller)
      (error 'localgroup-error
             :message "The localgroup session is not ready for handoff."
             :operation ':handoff))
    (with-lock-held ((localgroup-session-lock session))
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
    (list :ok :operation mode
          :scheduled-p t
          :session-id (localgroup-session-identifier session)
          :old-pid (sb-posix:getpid))))

(-> application-localgroup-handoff-pending-p (application) boolean)
(defun application-localgroup-handoff-pending-p (application)
  "Return true while APPLICATION has pending or running process handoff."
  (let ((session (application-localgroup-session application)))
    (and session
         (not
          (null
           (with-lock-held ((localgroup-session-lock session))
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
           (with-lock-held ((localgroup-session-lock session))
             (and (not (localgroup-session-handoff-running-p session))
                  (localgroup-session-handoff-mode session))))))))

(-> application-localgroup-take-ready-handoff (application) (option keyword))
(defun application-localgroup-take-ready-handoff (application)
  "Consume APPLICATION's pending handoff when no child jobs remain."
  (let ((session (application-localgroup-session application)))
    (unless session
      (return-from application-localgroup-take-ready-handoff nil))
    (multiple-value-bind (live active)
        (localgroup--task-counts application)
      (declare (ignore active))
      (when (plusp live)
        (return-from application-localgroup-take-ready-handoff nil)))
    (with-lock-held ((localgroup-session-lock session))
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
                     :message "The failed replacement still owns the conversation lease."
                     :operation ':handoff
                     :cause condition))
            (sleep 0.05))))))
  nil)

(-> localgroup-handoff--remove-replacement-registry
    (configuration string string integer)
    null)
(defun localgroup-handoff--remove-replacement-registry
    (configuration session-id token old-pid)
  "Remove a failed replacement registry record without deleting the old endpoint."
  (let* ((pathname (localgroup-registry-pathname configuration session-id))
         (record (localgroup--read-endpoint-record pathname)))
    (when (and record
               (/= (getf (rest record) :pid) old-pid)
               (string= (getf (rest record) :token) token))
      (localgroup--remove-stale-record pathname record)))
  nil)

(-> application-localgroup-run-handoff (application keyword t) null)
(defun application-localgroup-run-handoff (application mode controller)
  "Launch APPLICATION's detached replacement and stop after authenticated readiness."
  (application-input-controller-call-with-reader-paused
   controller
   (lambda ()
     (let* ((session (application-localgroup-session application))
            (configuration (application-configuration application))
            (conversation-id
              (conversation-identifier (application-conversation application)))
            (session-id (localgroup-session-identifier session))
            (token (localgroup-session-token session))
            (old-pid (sb-posix:getpid))
            (handoff-pathname nil)
            (process nil)
            (lease-released-p nil)
            (held-lease-p
              (and (slot-boundp application 'conversation-lease)
                   (application-conversation-lease application)))
            (gate-p nil)
            (completed-p nil)
            (primary-ready-p nil))
       (with-lock-held ((application-input-controller-lock controller))
         (setf primary-ready-p
               (localgroup-handoff--primary-ready-p controller))
         (when primary-ready-p
           (setf (application-input-controller-localgroup-handoff-p controller) t
                 gate-p t)))
       (unless primary-ready-p
         (application-localgroup-request-handoff application mode)
         (return-from application-localgroup-run-handoff nil))
       (multiple-value-bind (live active)
           (localgroup--task-counts application)
         (declare (ignore active))
         (when (plusp live)
           (with-lock-held ((application-input-controller-lock controller))
             (setf (application-input-controller-localgroup-handoff-p controller) nil
                   gate-p nil))
           (application-localgroup-request-handoff application mode)
           (return-from application-localgroup-run-handoff nil)))
       (with-lock-held ((localgroup-session-lock session))
         (setf (localgroup-session-handoff-running-p session) t))
       (unwind-protect
            (progn
              (setf handoff-pathname
                    (localgroup-handoff--write application session mode))
              (application-release-conversation-lease application)
              (setf lease-released-p (not (null held-lease-p))
                    process
                    (funcall *localgroup-handoff-launch-function*
                             application handoff-pathname))
              (unless
                  (funcall *localgroup-handoff-wait-function*
                           configuration session-id token old-pid)
                (error 'localgroup-error
                       :message
                       (format nil
                               "The detached replacement did not start within ~D seconds. See ~A."
                               *localgroup-handoff-start-timeout-seconds*
                               (namestring
                                (localgroup-handoff-log-pathname
                                 configuration session-id)))
                       :operation ':handoff
                       :session-id session-id))
              (setf completed-p t)
              (application-input-controller--prepare-shutdown
               controller ':localgroup-detach))
         (unless completed-p
           (when process
             (handler-case
                 (funcall *localgroup-handoff-stop-function*
                          process handoff-pathname)
               (localgroup-error (condition)
                 (application-input-controller--prepare-shutdown
                  controller ':localgroup-handoff-failed)
                 (error condition))))
           (localgroup-handoff--remove-replacement-registry
            configuration session-id token old-pid)
           (when lease-released-p
             (localgroup-handoff--restore-lease application conversation-id))
           (when handoff-pathname
             (localgroup-handoff--delete-state-pathnames handoff-pathname))
           (with-lock-held ((localgroup-session-lock session))
             (setf (localgroup-session-handoff-running-p session) nil))
           (when gate-p
             (with-lock-held ((application-input-controller-lock controller))
               (setf (application-input-controller-localgroup-handoff-p controller)
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
      (localgroup-handoff--delete-state-pathnames pathname)))
  nil)
