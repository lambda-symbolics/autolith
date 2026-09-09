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

(defgeneric platform-run-image-saver (platform child-function)
  (:documentation
   "Run CHILD-FUNCTION in a saver child sharing this image's heap and wait for it.

Return true when the child exited successfully. CHILD-FUNCTION must end the
child process itself, normally through SAVE-LISP-AND-DIE; returning from it
counts as failure. Signal PLATFORM-ERROR with operation :FORK when no child
could be started and :WAIT when its exit could not be observed."))

(defgeneric platform-unique-identifier (platform)
  (:documentation
   "Return a fresh identifier string drawn from operating-system randomness.

Signal PLATFORM-CAPABILITY-UNAVAILABLE when the host offers no such source."))


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

(defgeneric platform-create-private-file (platform pathname)
  (:documentation
   "Create PATHNAME exclusively as a private file and return an octet output stream.

Signal PLATFORM-ERROR with operation :CREATE and reason :EXISTS when anything
already occupies PATHNAME."))

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

(defgeneric platform-shared-library-file-name (platform base-name)
  (:documentation
   "Return the host file name of shared library BASE-NAME, such as libfff_c.so."))

(defgeneric platform-delete-directory-tree (platform pathname
                                            &key validate if-does-not-exist)
  (:documentation
   "Delete the directory tree at PATHNAME as UIOP:DELETE-DIRECTORY-TREE does.

VALIDATE and IF-DOES-NOT-EXIST keep their UIOP meanings. Windows first clears
the read-only attribute that git and other tools leave on files, since deleting
such a file is refused there while POSIX consults only the directory."))


;;;; -- Terminal and Shell --

(defgeneric platform-interactive-descriptor-p (platform descriptor)
  (:documentation
   "Return true when file DESCRIPTOR is attached to an interactive terminal."))

(defgeneric platform-disable-input-echo (platform descriptor)
  (:documentation
   "Stop echoing input typed on terminal DESCRIPTOR.

Return the state PLATFORM-RESTORE-INPUT-ECHO needs. Signal PLATFORM-ERROR with
operation :TERMINAL when DESCRIPTOR's mode cannot be changed."))

(defgeneric platform-restore-input-echo (platform descriptor state)
  (:documentation
   "Restore terminal DESCRIPTOR to STATE returned by PLATFORM-DISABLE-INPUT-ECHO."))

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

(-> platform-file-status-unchanged-p
    (platform-file-status platform-file-status)
    boolean)
(defun platform-file-status-unchanged-p (before after)
  "Return true when one object stayed unchanged between observations BEFORE and AFTER."
  (and (platform-file-status-same-object-p before after)
       (= (platform-file-status-size before)
          (platform-file-status-size after))
       (= (platform-file-status-modification-time before)
          (platform-file-status-modification-time after))
       (= (platform-file-status-change-time before)
          (platform-file-status-change-time after))))


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
