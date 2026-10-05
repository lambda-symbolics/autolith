(in-package #:autolith)

;;;; -- Platform Adapter Protocol --

;;; Autolith keeps every operating-system dependency behind this protocol.
;;; The generic functions below are the contract; platform-posix.lisp and
;;; platform-win32.lisp implement it, and autolith.asd loads exactly one of
;;; them for the host. Callers pass *PLATFORM* and never use host reader
;;; conditionals themselves.

(defclass platform ()
  ()
  (:documentation
   "The host operating-system adapter behind Autolith's platform protocol."))

(defvar *platform* nil
  "The host platform adapter installed by the loaded implementation file.")

(deftype platform-file-kind ()
  "The filesystem object kinds a platform file status distinguishes."
  '(member :file :directory :symbolic-link :socket :other))

(deftype platform-root-kind ()
  "The kinds of per-user directory Autolith keeps."
  '(member :config :data :state :cache))

(deftype platform-failure-reason ()
  "The portable discriminators a platform failure carries."
  '(member :missing :exists :not-regular :not-directory :symbolic-link
    :refused :failed))

(defclass platform-file-status ()
  ((kind
    :initarg :kind
    :reader platform-file-status-kind
    :type platform-file-kind
    :documentation "The kind of filesystem object observed.")
   (identity
    :initarg :identity
    :reader platform-file-status-identity
    :type t
    :documentation
    "An EQUAL-comparable value naming the object, such as a device and inode pair.")
   (size
    :initarg :size
    :reader platform-file-status-size
    :type (integer 0)
    :documentation "The object's size in octets.")
   (modification-time
    :initarg :modification-time
    :reader platform-file-status-modification-time
    :type integer
    :documentation
    "The content modification time in platform units, compared only for equality.")
   (change-time
    :initarg :change-time
    :reader platform-file-status-change-time
    :type integer
    :documentation
    "The metadata change time in platform units, compared only for equality.")
   (owned-p
    :initarg :owned-p
    :reader platform-file-status-owned-p
    :type boolean
    :documentation "Whether the current user owns the object.")
   (private-p
    :initarg :private-p
    :reader platform-file-status-private-p
    :type boolean
    :documentation
    "Whether the current user owns the object and no other user may access it.")
   (read-only-p
    :initarg :read-only-p
    :reader platform-file-status-read-only-p
    :type boolean
    :documentation "Whether the owner may not write the object's content."))
  (:documentation
   "One observation of a filesystem object's kind, identity, size, times, and privacy."))


;;;; -- Capabilities and Processes --

(defgeneric platform-supports-p (platform capability)
  (:documentation
   "Return true when PLATFORM provides CAPABILITY.

The capabilities are :LOCAL-SOCKETS, stream sockets addressed by a filesystem
pathname; :PROCESS-GROUPS, signalling every process of a group at once;
:DETACHED-SESSIONS, leaving the controlling session so a process outlives its
terminal; and :FORKED-IMAGE-SAVER, saving an image from a forked copy of this
process. Operations behind an absent capability signal
PLATFORM-CAPABILITY-UNAVAILABLE."))

(defgeneric platform-process-alive-p (platform process-id)
  (:documentation
   "Return true when PROCESS-ID names a live process.

A process that exists but may not be signaled by the current user counts as
alive. A vanished process yields NIL without signaling."))

(defgeneric platform-process-group-alive-p (platform process-group-id)
  (:documentation
   "Return true when PROCESS-GROUP-ID still has a live member, as for
PLATFORM-PROCESS-ALIVE-P."))

(defgeneric platform-terminate-process (platform process-id &key force)
  (:documentation
   "Ask PROCESS-ID to stop, or end it immediately when FORCE is true.

Signal PLATFORM-ERROR with operation :TERMINATE when the request cannot be
delivered, including to a vanished process."))

(defgeneric platform-terminate-process-group (platform process-group-id
                                              &key force)
  (:documentation
   "Ask every member of PROCESS-GROUP-ID to stop, or end them when FORCE is true,
as for PLATFORM-TERMINATE-PROCESS."))

(defgeneric platform-detach-session (platform)
  (:documentation
   "Detach this process from its controlling session so it outlives that session.

Signal PLATFORM-ERROR with operation :DETACH when the process cannot detach."))

(defgeneric platform-session-launch-command (platform source-root)
  (:documentation "Return the stable launcher's executable and prefix arguments."))

(defgeneric platform-launch-detached-process
    (platform arguments &key directory output ticket)
  (:documentation
   "Launch ARGUMENTS with a gated, owned process tree, writing OUTPUT.

Write the launcher PID in handoff TICKET's launcher-pid sibling before releasing
the startup gate. POSIX uses image-daemon's gated supervisor; Windows uses a
suspended initial thread and a native Job Object. Return an owned process object."))

(defgeneric platform-release-process (platform process)
  (:documentation "Release caller resources without terminating the process."))

(defmethod platform-release-process ((platform platform) process)
  "Leave UIOP's process lifetime management to its runtime."
  (declare (ignore platform process))
  nil)

(defgeneric platform-wait-process (platform process)
  (:documentation "Wait for PROCESS and release its operating-system resources."))

(defgeneric platform-process-object-pid (platform process)
  (:documentation "Return PROCESS's operating-system process ID."))

(defgeneric platform-process-object-alive-p (platform process)
  (:documentation "Return whether PROCESS is still running."))

(defgeneric platform-terminate-process-object (platform process &key force)
  (:documentation "Terminate PROCESS."))

(defmethod platform-process-object-pid ((platform platform) process)
  "Return a UIOP process object's operating-system process ID."
  (declare (ignore platform))
  (uiop:process-info-pid process))

(defmethod platform-process-object-alive-p ((platform platform) process)
  "Return whether a UIOP process object is still running."
  (declare (ignore platform))
  (uiop:process-alive-p process))

(defmethod platform-terminate-process-object ((platform platform) process &key force)
  "Terminate a UIOP process object."
  (declare (ignore platform))
  (uiop:terminate-process process :urgent force))

(defmethod platform-wait-process ((platform platform) process)
  "Wait for a UIOP process object."
  (declare (ignore platform))
  (uiop:wait-process process))


;;;; -- Environment --

(defgeneric platform-set-environment-variable (platform name value)
  (:documentation
   "Set environment variable NAME to VALUE for this process and its children.

A NIL VALUE removes the variable. The change is visible to UIOP:GETENV, to C
libraries that read the environment, and to processes started afterwards."))

(-> platform-setenv (string string) null)
(defun platform-setenv (name value)
  "Set environment variable NAME to VALUE through the platform."
  (platform-set-environment-variable *platform* name value)
  nil)

(-> platform-unsetenv (string) null)
(defun platform-unsetenv (name)
  "Remove environment variable NAME through the platform."
  (platform-set-environment-variable *platform* name nil)
  nil)


;;;; -- Files --

(defgeneric platform-application-root (platform kind)
  (:documentation
   "Return Autolith's per-user directory of KIND as an absolute directory pathname.

KIND is :CONFIG, :DATA, :STATE, or :CACHE. An absolute XDG_CONFIG_HOME,
XDG_DATA_HOME, XDG_STATE_HOME, or XDG_CACHE_HOME variable places the root under
that base on every host; otherwise the host convention applies. The
subdirectories below each root are the same on every host."))

(defgeneric platform-parse-namestring (platform string)
  (:documentation
   "Parse STRING, a pathname a user or configuration supplied, into a pathname.

POSIX hosts read it as a Unix namestring, in which * ? and [ are wild; Windows
hosts read it natively, so drive letters and backslashes are understood and no
character is wild."))

(defgeneric platform-truename (platform pathname)
  (:documentation
   "Return PATHNAME's canonical name with every symbolic link resolved.

This is TRUENAME's contract on POSIX hosts, where TRUENAME resolves links
itself; SBCL's Windows TRUENAME leaves links in place, so that adapter asks the
kernel for the final path. Signal PLATFORM-ERROR with operation :RESOLVE and
reason :MISSING when PATHNAME does not exist."))

(-> platform-pathname ((or string pathname)) pathname)
(defun platform-pathname (designator)
  "Return DESIGNATOR as a pathname, parsing a string through PLATFORM-PARSE-NAMESTRING."
  (if (stringp designator)
      (platform-parse-namestring *platform* designator)
      designator))

(-> platform-file-uri (platform pathname) string)
(defgeneric platform-file-uri (platform pathname)
  (:documentation "Encode an absolute native pathname as a percent-escaped file URI."))

(-> platform--encode-uri-path (string) string)
(defun platform--encode-uri-path (name)
  "Percent-encode each UTF-8 path segment while preserving URI separators."
  (format nil "~{~A~^/~}"
          (mapcar (lambda (part) (quri:url-encode part :encoding ':utf-8))
                  (uiop:split-string name :separator "/"))))

(defgeneric platform-path-status (platform pathname &key follow-links-p)
  (:documentation
   "Return PATHNAME's current PLATFORM-FILE-STATUS, or NIL when nothing exists there.

With FOLLOW-LINKS-P a symbolic link is reported as its target and a dangling
link as absent; otherwise the link itself is reported with kind :SYMBOLIC-LINK.
Signal PLATFORM-ERROR with operation :STATUS for any other failure."))

(defgeneric platform-stream-status (platform stream)
  (:documentation
   "Return the PLATFORM-FILE-STATUS of the open file behind STREAM.

STREAM must have been returned by PLATFORM-OPEN-REGULAR-FILE or
PLATFORM-CREATE-PRIVATE-FILE."))

(defgeneric platform-open-regular-file (platform pathname &key follow-links-p)
  (:documentation
   "Open regular file PATHNAME for non-blocking octet input.

Return two values: an octet input stream whose CLOSE releases the file, and
the status observed on the opened object. Unless FOLLOW-LINKS-P, refuse to
open through a symbolic link. Signal PLATFORM-ERROR with operation :OPEN, using
reason :NOT-REGULAR for anything but a regular file, :SYMBOLIC-LINK for a
refused link, and :MISSING for an absent path."))

(defgeneric platform-list-directory (platform pathname &key limit)
  (:documentation
   "Return the entry names in directory PATHNAME without opening any entry.

Return two values: at most LIMIT names, excluding the current and parent
directory entries and in the order the host enumerates them, and whether more
entries remained. Signal PLATFORM-ERROR with operation :LIST when PATHNAME
cannot be enumerated."))

(defgeneric platform-make-private (platform pathname &key read-only-p)
  (:documentation
   "Restrict existing PATHNAME to the current user.

A directory stays usable only by its owner; a file becomes owner-readable and,
unless READ-ONLY-P, owner-writable. Signal PLATFORM-ERROR with operation
:PROTECT when the restriction cannot be applied."))

(defgeneric platform-make-read-only (platform pathname)
  (:documentation
   "Make PATHNAME readable by everyone who can reach it and writable by nobody.

Signal PLATFORM-ERROR with operation :PROTECT when the change cannot be applied."))

(defgeneric platform-file-uri-pathname (platform decoded-path)
  (:documentation "Parse a decoded local file-URI path using the host's pathname syntax."))

(defgeneric platform-file-permissions (platform pathname)
  (:documentation "Return the host's restorable access-permission value for PATHNAME."))

(defgeneric (setf platform-file-permissions) (permissions platform pathname)
  (:documentation "Restore PERMISSIONS obtained from the same host onto PATHNAME."))

(defgeneric platform-delete-file (platform pathname)
  (:documentation
   "Remove one file, handling host read-only attributes and restoring them on failure."))

(defgeneric platform-copy-file-permissions (platform source target)
  (:documentation
   "Give TARGET the access permissions SOURCE currently has.

Signal PLATFORM-ERROR with operation :PROTECT when either file is unavailable."))

(defgeneric platform-set-file-times (platform pathname universal-time)
  (:documentation
   "Set PATHNAME's access and modification times to UNIVERSAL-TIME.

Signal PLATFORM-ERROR with operation :TIMES when the times cannot be set."))

(defgeneric platform-publish-new-file (platform source target)
  (:documentation
   "Atomically make SOURCE's content appear at absent TARGET.

Signal PLATFORM-ERROR with operation :PUBLISH and reason :EXISTS when TARGET is
already occupied, leaving it untouched. Whether SOURCE remains afterwards is
platform-specific, so callers delete it only when it still exists."))

(defgeneric platform-replace-file (platform source target)
  (:documentation
   "Atomically rename SOURCE to exact TARGET, replacing any existing TARGET.

Neither pathname is merged with defaults. Signal PLATFORM-ERROR with operation
:REPLACE when the rename fails."))

(defgeneric platform-executable-file-p (platform pathname)
  (:documentation
   "Return true when PATHNAME names a file the current user may execute."))

(defgeneric platform-make-temporary-directory (platform parent prefix)
  (:documentation
   "Atomically create a private directory named PREFIX plus a random suffix
beneath PARENT and return it as a directory pathname.

Signal PLATFORM-ERROR with operation :CREATE when no directory could be made."))

(defgeneric platform-delete-directory-tree (platform pathname
                                            &key validate if-does-not-exist)
  (:documentation
   "Delete the directory tree at PATHNAME as UIOP:DELETE-DIRECTORY-TREE does.

VALIDATE and IF-DOES-NOT-EXIST keep their UIOP meanings. Windows first clears
the read-only attribute that git and other tools leave on files, since deleting
such a file is refused there while POSIX consults only the directory."))


;;;; -- Terminal and Shell --

(defgeneric platform-terminal-enable-fullscreen (platform)
  (:documentation "Enable native VT output and return an opaque restoration token."))

(defgeneric platform-terminal-restore-fullscreen (platform token)
  (:documentation "Restore the native output mode captured before fullscreen entry."))

(defmethod platform-terminal-enable-fullscreen ((platform platform))
  "Use the terminal emulator's existing VT output support."
  nil)

(defmethod platform-terminal-restore-fullscreen ((platform platform) token)
  "No native output mode changes are needed on this host."
  (declare (ignore token))
  nil)

(defgeneric platform-watch-terminal-resize (platform function)
  (:documentation
   "Call FUNCTION with no arguments whenever the controlling terminal changes size.

Return a token for PLATFORM-UNWATCH-TERMINAL-RESIZE. FUNCTION may run in an
interrupt context and must do no more than record the event."))

(defgeneric platform-unwatch-terminal-resize (platform token)
  (:documentation
   "Stop the resize notifications identified by TOKEN."))

(defgeneric platform-shell-command-line (platform command)
  (:documentation
   "Return the program and arguments that run shell COMMAND on this host."))

(defgeneric platform-open-url (platform url)
  (:documentation
   "Hand web URL to the host's default browser without waiting for it.

Return true when the launcher started and NIL when it could not. Callers
validate that URL is an http or https URL before asking."))

(-> platform--launch-quietly (list) boolean)
(defun platform--launch-quietly (command)
  "Start COMMAND detached from this process's streams, reporting whether it began."
  (handler-case
      (progn
        (uiop:launch-program command
                             :input nil
                             :output nil
                             :error-output nil)
        t)
    (error ()
      nil)))

(defgeneric platform-source-check-command (platform source-root)
  (:documentation
   "Return the argv that runs the repository check for SOURCE-ROOT on this host."))

(defgeneric platform-call-with-command-sandbox (platform workspace function)
  (:documentation
   "Call FUNCTION with a workspace sandbox policy and optional child environment.

Own temporary scopes and serialize overlapping host ACL changes until FUNCTION
returns or unwinds. FUNCTION accepts POLICY and ENVIRONMENT and must wait for
its command and descendants before returning."))

(defmethod platform-call-with-command-sandbox ((platform platform) workspace function)
  "Use the whole-host read-only, workspace-write policy on POSIX backends."
  (funcall function
           (workspace-write-sandbox-policy :workspace-roots (list workspace))
           nil))


;;;; -- Local Sockets --

(defgeneric platform-local-listener (platform pathname &key backlog)
  (:documentation
   "Bind a private listening stream socket to filesystem PATHNAME.

Return the socket after it accepts up to BACKLOG pending connections. Signal
PLATFORM-CAPABILITY-UNAVAILABLE without the :LOCAL-SOCKETS capability."))

(defgeneric platform-connect-local (platform pathname)
  (:documentation
   "Connect a stream socket to the filesystem endpoint at PATHNAME and return it.

Signal PLATFORM-ERROR with operation :CONNECT, using reason :REFUSED when
nothing is listening there, and PLATFORM-CAPABILITY-UNAVAILABLE without the
:LOCAL-SOCKETS capability."))


;;;; -- Status Comparison --

(-> platform-file-status-same-object-p
    (platform-file-status platform-file-status)
    boolean)
(defun platform-file-status-same-object-p (left right)
  "Return true when LEFT and RIGHT observed the same filesystem object."
  (and (equal (platform-file-status-identity left)
              (platform-file-status-identity right))
       t))


;;;; -- Shared File Methods --

;;; File status, opening, listing, publication, and link resolution are the
;;; same protocol on every host through ls-compat, whose POSIX and Win32
;;; backends carry the host differences. Each adapter inherits these methods.

(-> platform--file-status (file-information) platform-file-status)
(defun platform--file-status (information)
  "Return the platform file status ls-compat observation INFORMATION describes."
  (make-instance 'platform-file-status
                 :kind (file-information-kind information)
                 :identity (file-information-identity information)
                 :size (file-information-size information)
                 :modification-time (file-information-modification-time information)
                 :change-time (file-information-change-time information)
                 :owned-p (file-information-owned-p information)
                 :private-p (file-information-private-p information)
                 :read-only-p (file-information-read-only-p information)))

(-> platform--file-failure (keyword t file-error) nil)
(defun platform--file-failure (operation pathname condition)
  "Signal PLATFORM-ERROR for ls-compat file CONDITION raised by OPERATION on PATHNAME."
  (error 'platform-error
         :message (princ-to-string condition)
         :operation operation
         :pathname (and pathname (pathname pathname))
         :reason (typecase condition
                   (file-operation-failed
                    (file-operation-failed-reason condition))
                   (not-regular-file
                    (if (eq (not-regular-file-kind condition) ':symbolic-link)
                        ':symbolic-link
                        ':not-regular))
                   (link-target-exists
                    ':exists)
                   (t
                    ':failed))
         :code (and (typep condition 'file-operation-failed)
                    (file-operation-failed-code condition))))

(defmethod platform-path-status ((platform platform) pathname &key follow-links-p)
  "Observe PATHNAME through ls-compat, reporting a missing object as NIL."
  (declare (ignore platform))
  (handler-case
      (platform--file-status (file-information pathname :follow-links-p follow-links-p))
    (file-operation-failed (condition)
      (if (eq (file-operation-failed-reason condition) ':missing)
          nil
          (platform--file-failure ':status pathname condition)))))

(defmethod platform-stream-status ((platform platform) stream)
  "Observe the open file behind STREAM through ls-compat."
  (declare (ignore platform))
  (handler-case
      (platform--file-status (stream-file-information stream))
    (file-error (condition)
      (platform--file-failure ':status nil condition))))

(defmethod platform-open-regular-file ((platform platform) pathname &key follow-links-p)
  "Open regular file PATHNAME through ls-compat for non-blocking octet input."
  (declare (ignore platform))
  (handler-case
      (multiple-value-bind (stream information)
          (ls-compat.posix:open-regular-file pathname :follow-links-p follow-links-p)
        (values stream (platform--file-status information)))
    (file-error (condition)
      (platform--file-failure ':open pathname condition))))

(defmethod platform-list-directory ((platform platform) pathname
                                    &key (limit most-positive-fixnum))
  "Enumerate PATHNAME's entry names through ls-compat without inspecting them."
  (declare (ignore platform))
  (handler-case
      (directory-names pathname :limit limit)
    (file-error (condition)
      (platform--file-failure ':list pathname condition))))

(defmethod platform-publish-new-file ((platform platform) source target)
  "Hard-link SOURCE to absent TARGET through ls-compat, refusing an occupied TARGET."
  (declare (ignore platform))
  (handler-case
      (link-file source target)
    (file-error (condition)
      (platform--file-failure ':publish target condition)))
  nil)

(defmethod platform-truename ((platform platform) pathname)
  "Resolve PATHNAME through every symbolic link, junction included, with ls-compat."
  (declare (ignore platform))
  (handler-case
      (resolve-pathname pathname)
    (file-error (condition)
      (platform--file-failure ':resolve pathname condition))))


;;;; -- Conditions --

(define-condition platform-error (autolith-error)
  ((operation
    :initarg :operation
    :reader platform-error-operation
    :type keyword
    :documentation "The platform operation that failed, such as :OPEN or :PUBLISH.")
   (pathname
    :initarg :pathname
    :initform nil
    :reader platform-error-pathname
    :type (option pathname)
    :documentation "The pathname involved, when the operation named one.")
   (reason
    :initarg :reason
    :initform ':failed
    :reader platform-error-reason
    :type platform-failure-reason
    :documentation "The portable discriminator callers recover on.")
   (code
    :initarg :code
    :initform nil
    :reader platform-error-code
    :type (option integer)
    :documentation "The operating-system error number, when one was reported."))
  (:documentation "A platform operation failed in a way callers distinguish by reason."))

(define-condition platform-capability-unavailable (autolith-error)
  ((capability
    :initarg :capability
    :reader platform-capability-unavailable-capability
    :type keyword
    :documentation "The capability this host withholds."))
  (:documentation "A capability is withheld on this host for a user-visible reason."))
