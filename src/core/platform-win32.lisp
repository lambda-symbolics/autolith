(in-package #:autolith)

;;;; -- Win32 Platform Adapter --

;;; This file implements the platform protocol for Windows 10 and 11 on
;;; x86-64. It binds the wide kernel32 and advapi32 entry points it needs
;;; with SB-ALIEN and never touches SBCL's internal SB-WIN32 package, so an
;;; SBCL upgrade cannot change its behaviour silently. Descriptors are
;;; HANDLE values, which is also what SBCL's Windows fd-streams report.

(defclass win32-platform (platform)
  ()
  (:documentation "The adapter for Windows hosts built on the Win32 API."))


;;;; -- Alien Bindings --

(sb-alien:define-alien-type win32-wide-string
    (sb-alien:c-string :external-format :ucs-2le))

(sb-alien:define-alien-type win32-handle (sb-alien:signed 64))

(sb-alien:define-alien-type win32-dword (sb-alien:unsigned 32))

(sb-alien:define-alien-type win32-bool sb-alien:int)

(defmacro win32--define (lisp-name c-name result &rest arguments)
  "Bind C-NAME as LISP-NAME returning RESULT with ARGUMENTS."
  `(sb-alien:define-alien-routine (,c-name ,lisp-name) ,result ,@arguments))

(win32--define win32--get-last-error "GetLastError" win32-dword)
(win32--define win32--format-message "FormatMessageW" win32-dword
  (flags win32-dword) (source (* t)) (message-id win32-dword)
  (language win32-dword) (buffer (* t)) (size win32-dword) (arguments (* t)))
(win32--define win32--local-free "LocalFree" (* t) (memory (* t)))
(win32--define win32--close-handle "CloseHandle" win32-bool (handle win32-handle))
(win32--define win32--create-file "CreateFileW" win32-handle
  (name win32-wide-string) (access win32-dword) (share win32-dword)
  (security (* t)) (disposition win32-dword) (flags win32-dword)
  (template win32-handle))
(win32--define win32--get-file-information-by-handle "GetFileInformationByHandle"
    win32-bool
  (handle win32-handle) (information (* t)))
(win32--define win32--get-file-information-by-handle-ex
    "GetFileInformationByHandleEx" win32-bool
  (handle win32-handle) (class sb-alien:int) (information (* t))
  (size win32-dword))
(win32--define win32--get-file-attributes "GetFileAttributesW" win32-dword
  (name win32-wide-string))
(win32--define win32--set-file-attributes "SetFileAttributesW" win32-bool
  (name win32-wide-string) (attributes win32-dword))
(win32--define win32--set-environment-variable "SetEnvironmentVariableW" win32-bool
  (name win32-wide-string) (value win32-wide-string))
(win32--define win32--wputenv "_wputenv" sb-alien:int (entry win32-wide-string))
(win32--define win32--get-file-type "GetFileType" win32-dword (handle win32-handle))
(win32--define win32--get-final-path-name-by-handle "GetFinalPathNameByHandleW"
    win32-dword
  (handle win32-handle) (buffer (* t)) (size win32-dword) (flags win32-dword))
(win32--define win32--create-hard-link "CreateHardLinkW" win32-bool
  (new-name win32-wide-string) (existing-name win32-wide-string) (security (* t)))
(win32--define win32--move-file-ex "MoveFileExW" win32-bool
  (source win32-wide-string) (target win32-wide-string) (flags win32-dword))
(win32--define win32--set-file-time "SetFileTime" win32-bool
  (handle win32-handle) (creation (* t)) (access (* t)) (write (* t)))
(win32--define win32--create-directory "CreateDirectoryW" win32-bool
  (name win32-wide-string) (security (* t)))
(win32--define win32--find-first-file "FindFirstFileW" win32-handle
  (pattern win32-wide-string) (data (* t)))
(win32--define win32--find-next-file "FindNextFileW" win32-bool
  (handle win32-handle) (data (* t)))
(win32--define win32--find-close "FindClose" win32-bool (handle win32-handle))
(win32--define win32--open-process "OpenProcess" win32-handle
  (access win32-dword) (inherit win32-bool) (process-id win32-dword))
(win32--define win32--get-exit-code-process "GetExitCodeProcess" win32-bool
  (handle win32-handle) (code (* win32-dword)))
(win32--define win32--terminate-process "TerminateProcess" win32-bool
  (handle win32-handle) (code win32-dword))
(win32--define win32--get-current-process "GetCurrentProcess" win32-handle)
(win32--define win32--get-current-process-id "GetCurrentProcessId" win32-dword)
(win32--define win32--get-std-handle "GetStdHandle" win32-handle (which win32-dword))
(win32--define win32--get-console-mode "GetConsoleMode" win32-bool
  (handle win32-handle) (mode (* win32-dword)))
(win32--define win32--set-console-mode "SetConsoleMode" win32-bool
  (handle win32-handle) (mode win32-dword))
(win32--define win32--get-console-screen-buffer-info "GetConsoleScreenBufferInfo"
    win32-bool
  (handle win32-handle) (information (* t)))
(win32--define win32--open-process-token "OpenProcessToken" win32-bool
  (process win32-handle) (access win32-dword) (token (* win32-handle)))
(win32--define win32--get-token-information "GetTokenInformation" win32-bool
  (token win32-handle) (class sb-alien:int) (buffer (* t)) (size win32-dword)
  (returned (* win32-dword)))
(win32--define win32--get-length-sid "GetLengthSid" win32-dword (sid (* t)))
(win32--define win32--get-security-info "GetSecurityInfo" win32-dword
  (handle win32-handle) (type sb-alien:int) (information win32-dword)
  (owner (* (* t))) (group (* (* t))) (dacl (* (* t))) (sacl (* (* t)))
  (descriptor (* (* t))))
(win32--define win32--get-acl-information "GetAclInformation" win32-bool
  (acl (* t)) (information (* t)) (size win32-dword) (class sb-alien:int))
(win32--define win32--get-ace "GetAce" win32-bool
  (acl (* t)) (index win32-dword) (ace (* (* t))))
(win32--define win32--generate-random "SystemFunction036" (sb-alien:unsigned 8)
  (buffer (* t)) (size win32-dword))


;;;; -- Win32 Constants --

(defparameter *win32-invalid-handle* -1
  "The handle value CreateFileW and FindFirstFileW return on failure.")

(defparameter *win32-generic-read* #x80000000
  "GENERIC_READ, which includes READ_CONTROL for security queries.")

(defparameter *win32-generic-write* #x40000000
  "GENERIC_WRITE, which includes READ_CONTROL for security queries.")

(defparameter *win32-file-read-attributes* #x80
  "FILE_READ_ATTRIBUTES, enough access to inspect an object.")

(defparameter *win32-file-write-attributes* #x100
  "FILE_WRITE_ATTRIBUTES, enough access to change file times.")

(defparameter *win32-read-control* #x20000
  "READ_CONTROL, the access needed to read an object's security descriptor.")

(defparameter *win32-share-all* 7
  "FILE_SHARE_READ, FILE_SHARE_WRITE, and FILE_SHARE_DELETE together.")

(defparameter *win32-create-new* 1
  "The CreateFileW disposition that fails when the file exists.")

(defparameter *win32-open-existing* 3
  "The CreateFileW disposition that fails when the file is absent.")

(defparameter *win32-file-attribute-readonly* #x1
  "FILE_ATTRIBUTE_READONLY.")

(defparameter *win32-file-attribute-directory* #x10
  "FILE_ATTRIBUTE_DIRECTORY.")

(defparameter *win32-file-attribute-normal* #x80
  "FILE_ATTRIBUTE_NORMAL.")

(defparameter *win32-file-attribute-reparse-point* #x400
  "FILE_ATTRIBUTE_REPARSE_POINT, carried by symbolic links and junctions.")

(defparameter *win32-file-flag-backup-semantics* #x02000000
  "FILE_FLAG_BACKUP_SEMANTICS, required to open a directory handle.")

(defparameter *win32-file-flag-open-reparse-point* #x00200000
  "FILE_FLAG_OPEN_REPARSE_POINT, which opens a link itself instead of its target.")

(defparameter *win32-invalid-file-attributes* #xFFFFFFFF
  "The GetFileAttributesW result meaning failure.")

(defparameter *win32-error-environment-variable-not-found* 203
  "ERROR_ENVVAR_NOT_FOUND, reported when removing a variable that is absent.")

(defparameter *win32-file-type-disk* 1
  "The GetFileType result for a regular disk file.")

(defparameter *win32-move-file-replace-existing* 1
  "MOVEFILE_REPLACE_EXISTING.")

(defparameter *win32-process-query-limited-information* #x1000
  "The OpenProcess access right that reads a process's exit state.")

(defparameter *win32-process-terminate* #x1
  "The OpenProcess access right that allows TerminateProcess.")

(defparameter *win32-still-active* 259
  "The exit code GetExitCodeProcess reports for a running process.")

(defparameter *win32-standard-input-handle* #xFFFFFFF6
  "STD_INPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *win32-standard-output-handle* #xFFFFFFF5
  "STD_OUTPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *win32-enable-echo-input* #x4
  "ENABLE_ECHO_INPUT in a console input mode.")

(defparameter *win32-token-query* #x8
  "TOKEN_QUERY access for OpenProcessToken.")

(defparameter *win32-token-user* 1
  "The TokenUser information class.")

(defparameter *win32-se-file-object* 1
  "SE_FILE_OBJECT for the security information functions.")

(defparameter *win32-owner-and-dacl-information* 5
  "OWNER_SECURITY_INFORMATION and DACL_SECURITY_INFORMATION together.")

(defparameter *win32-file-write-data* #x2
  "FILE_WRITE_DATA, the access right an unwritable object withholds from its owner.")

(defparameter *win32-administrators-sid-octets*
  (coerce '(1 2 0 0 0 0 0 5 32 0 0 0 32 2 0 0) '(simple-array (unsigned-byte 8) (16)))
  "S-1-5-32-544, the Administrators group that owns what an elevated process creates.")

(defparameter *win32-token-elevation* 20
  "The TokenElevation information class.")

(defparameter *win32-system-sid-octets*
  (coerce '(1 1 0 0 0 0 0 5 18 0 0 0) '(simple-array (unsigned-byte 8) (*)))
  "The octets of the well-known NT AUTHORITY\\SYSTEM identifier S-1-5-18.")

(defparameter *win32-filetime-epoch-offset* 9435484800
  "Seconds from the FILETIME epoch of 1601 to the universal-time epoch of 1900.")

(defparameter *win32-resize-poll-seconds* 0.25
  "How often the resize watcher compares the console window size.")

(defparameter *win32-error-file-not-found* 2
  "ERROR_FILE_NOT_FOUND.")

(defparameter *win32-error-path-not-found* 3
  "ERROR_PATH_NOT_FOUND.")

(defparameter *win32-error-access-denied* 5
  "ERROR_ACCESS_DENIED.")

(defparameter *win32-error-invalid-parameter* 87
  "ERROR_INVALID_PARAMETER, which OpenProcess reports for a missing process.")

(defparameter *win32-error-file-exists* 80
  "ERROR_FILE_EXISTS.")

(defparameter *win32-error-already-exists* 183
  "ERROR_ALREADY_EXISTS.")

(defparameter *win32-error-directory* 267
  "ERROR_DIRECTORY, reported when a path component is not a directory.")


;;;; -- Failure Translation --

(-> win32--system-message (integer) string)
(defun win32--system-message (code)
  "Return the operating system's text for error CODE."
  (sb-alien:with-alien ((buffer (sb-alien:array (sb-alien:unsigned 16) 1024)))
    (let ((length (win32--format-message (logior #x1000 #x200) nil code 0
                                         (sb-alien:alien-sap buffer) 1024 nil)))
      (if (zerop length)
          (format nil "Windows error ~D" code)
          (string-right-trim
           '(#\Return #\Newline #\Space #\.)
           (coerce (loop for index below length
                         collect (code-char (sb-sys:sap-ref-16 (sb-alien:alien-sap buffer)
                                                               (* 2 index))))
                   'string))))))

(-> win32--failure-reason (integer) platform-failure-reason)
(defun win32--failure-reason (code)
  "Return the portable failure reason for Windows error CODE."
  (cond
    ((or (= code *win32-error-file-not-found*)
         (= code *win32-error-path-not-found*))
     ':missing)
    ((or (= code *win32-error-file-exists*)
         (= code *win32-error-already-exists*))
     ':exists)
    ((= code *win32-error-directory*)
     ':not-directory)
    (t
     ':failed)))

(-> win32--signal (keyword (option pathname) integer) nil)
(defun win32--signal (operation pathname code)
  "Signal PLATFORM-ERROR for Windows error CODE raised by OPERATION on PATHNAME."
  (error 'platform-error
         :message (format nil "~A (Windows error ~D)" (win32--system-message code) code)
         :operation operation
         :pathname pathname
         :reason (win32--failure-reason code)
         :code code))

(-> win32--fail (keyword (option pathname)) nil)
(defun win32--fail (operation pathname)
  "Signal PLATFORM-ERROR for the last Windows error of OPERATION on PATHNAME."
  (win32--signal operation pathname (win32--get-last-error)))

(-> win32--namestring (pathname) string)
(defun win32--namestring (pathname)
  "Return PATHNAME as the native namestring the wide entry points expect.

A directory pathname loses its trailing separator, since object handles are
opened by the directory's own name."
  (let ((native (uiop:native-namestring pathname)))
    (if (and (> (length native) 3)
             (char= (char native (1- (length native))) #\\))
        (subseq native 0 (1- (length native)))
        native)))

(-> win32--unavailable (keyword string) nil)
(defun win32--unavailable (capability message)
  "Signal that CAPABILITY is withheld on Windows, explained by MESSAGE."
  (error 'platform-capability-unavailable
         :message message
         :capability capability))


;;;; -- Capabilities and Processes --

(defmethod platform-supports-p ((platform win32-platform) capability)
  "Report that Windows provides none of the optional capabilities."
  (declare (ignore capability))
  nil)

(-> win32--process-state (integer) (member :alive :dead :unknown))
(defun win32--process-state (process-id)
  "Classify PROCESS-ID through OpenProcess and GetExitCodeProcess."
  (let ((handle (win32--open-process *win32-process-query-limited-information*
                                     0 process-id)))
    (if (zerop handle)
        (let ((code (win32--get-last-error)))
          (cond
            ((= code *win32-error-invalid-parameter*)
             ':dead)
            ((= code *win32-error-access-denied*)
             ':alive)
            (t
             ':unknown)))
        (unwind-protect
             (sb-alien:with-alien ((exit-code win32-dword))
               (cond
                 ((zerop (win32--get-exit-code-process handle (sb-alien:addr exit-code)))
                  ':unknown)
                 ((= exit-code *win32-still-active*)
                  ':alive)
                 (t
                  ':dead)))
          (win32--close-handle handle)))))

(defmethod platform-process-alive-p ((platform win32-platform) process-id)
  "Open PROCESS-ID and read whether it is still active."
  (eq (win32--process-state process-id) ':alive))

(defmethod platform-process-group-alive-p ((platform win32-platform)
                                           process-group-id)
  "Refuse, since Windows has no process groups to probe."
  (declare (ignore process-group-id))
  (win32--unavailable
   ':process-groups
   "Windows has no process groups, so a process group cannot be probed."))

(defmethod platform-terminate-process ((platform win32-platform) process-id
                                       &key force)
  "End PROCESS-ID with TerminateProcess.

Windows offers no cooperative termination request for another process, so
FORCE makes no difference; cooperative shutdown belongs to the loopback
protocol."
  (declare (ignore force))
  (let ((handle (win32--open-process *win32-process-terminate* 0 process-id)))
    (when (zerop handle)
      (win32--fail ':terminate nil))
    (unwind-protect
         (when (zerop (win32--terminate-process handle 1))
           (win32--fail ':terminate nil))
      (win32--close-handle handle)))
  nil)

(defmethod platform-terminate-process-group ((platform win32-platform)
                                             process-group-id &key force)
  "Refuse, since Windows has no process groups to terminate."
  (declare (ignore process-group-id force))
  (win32--unavailable
   ':process-groups
   "Windows has no process groups, so a process group cannot be terminated."))

(defmethod platform-detach-session ((platform win32-platform))
  "Refuse, since Windows has no controlling session to leave."
  (win32--unavailable
   ':detached-sessions
   "Windows cannot detach a running process from its console session; localgroup handoff is withheld on Windows."))

(defmethod platform-run-image-saver ((platform win32-platform) child-function)
  "Refuse, since Windows cannot fork a saver that shares this heap."
  (declare (ignore child-function))
  (win32--unavailable
   ':forked-image-saver
   "Windows cannot fork a process that shares this image's heap; image saves run in a fresh process instead."))

(-> win32--random-octets ((integer 1)) (simple-array (unsigned-byte 8) (*)))
(defun win32--random-octets (count)
  "Return COUNT octets from the system random number generator."
  (sb-alien:with-alien ((buffer (sb-alien:array (sb-alien:unsigned 8) 64)))
    (when (zerop (win32--generate-random (sb-alien:alien-sap buffer) count))
      (error 'platform-capability-unavailable
             :message "The Windows random number generator is unavailable."
             :capability ':unique-identifiers))
    (let ((octets (make-array count :element-type '(unsigned-byte 8))))
      (dotimes (index count octets)
        (setf (aref octets index) (sb-alien:deref buffer index))))))

(defmethod platform-unique-identifier ((platform win32-platform))
  "Return a random version 4 UUID string from the system generator."
  (let ((octets (win32--random-octets 16)))
    (setf (aref octets 6) (logior #x40 (logand (aref octets 6) #x0F))
          (aref octets 8) (logior #x80 (logand (aref octets 8) #x3F)))
    (format nil "~(~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}~)"
            (coerce (subseq octets 0 4) 'list)
            (coerce (subseq octets 4 6) 'list)
            (coerce (subseq octets 6 8) 'list)
            (coerce (subseq octets 8 10) 'list)
            (coerce (subseq octets 10 16) 'list))))


;;;; -- Security --

(defvar *win32-user-sid* nil
  "The current user's SID octets paired with the process identifier that read them.")

(-> win32--sid-octets (sb-sys:system-area-pointer) (simple-array (unsigned-byte 8) (*)))
(defun win32--sid-octets (sid)
  "Copy the security identifier at SID into a fresh octet vector."
  (let* ((length (win32--get-length-sid (sb-alien:sap-alien sid (* t))))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length octets)
      (setf (aref octets index) (sb-sys:sap-ref-8 sid index)))))

(-> win32--current-user-sid () (simple-array (unsigned-byte 8) (*)))
(defun win32--current-user-sid ()
  "Return the current process token's user SID octets, cached per process."
  (let ((process-id (win32--get-current-process-id)))
    (unless (and *win32-user-sid* (= (first *win32-user-sid*) process-id))
      (sb-alien:with-alien ((token win32-handle))
        (when (zerop (win32--open-process-token (win32--get-current-process)
                                                *win32-token-query*
                                                (sb-alien:addr token)))
          (win32--fail ':security nil))
        (unwind-protect
             (sb-alien:with-alien ((buffer (sb-alien:array (sb-alien:unsigned 8) 256))
                                   (returned win32-dword))
               (when (zerop (win32--get-token-information
                             token *win32-token-user* (sb-alien:alien-sap buffer)
                             256 (sb-alien:addr returned)))
                 (win32--fail ':security nil))
               (setf *win32-user-sid*
                     (list process-id
                           (win32--sid-octets
                            (sb-sys:int-sap
                             (sb-sys:sap-ref-64 (sb-alien:alien-sap buffer) 0))))))
          (win32--close-handle token))))
    (second *win32-user-sid*)))

(defvar *win32-elevated-p* ':unknown
  "Whether this process runs elevated, read from its token once.")

(-> win32--elevated-p () boolean)
(defun win32--elevated-p ()
  "Return whether this process runs with an elevated token.

Windows makes the Administrators group the owner of what an elevated process
creates, so ownership checks treat that group as the user then."
  (when (eq *win32-elevated-p* ':unknown)
    (sb-alien:with-alien ((token win32-handle))
      (when (zerop (win32--open-process-token (win32--get-current-process)
                                              *win32-token-query*
                                              (sb-alien:addr token)))
        (win32--fail ':security nil))
      (unwind-protect
           (sb-alien:with-alien ((elevation win32-dword)
                                 (returned win32-dword))
             (setf *win32-elevated-p*
                   (and (not (zerop (win32--get-token-information
                                     token *win32-token-elevation*
                                     (sb-alien:addr elevation) 4
                                     (sb-alien:addr returned))))
                        (not (zerop elevation)))))
        (win32--close-handle token))))
  *win32-elevated-p*)

(-> win32--owner-p ((simple-array (unsigned-byte 8) (*))) boolean)
(defun win32--owner-p (owner)
  "Return whether OWNER, a SID, counts as the current user."
  (or (equalp owner (win32--current-user-sid))
      (and (equalp owner *win32-administrators-sid-octets*)
           (win32--elevated-p))))

(-> win32--acl-entries (sb-sys:system-area-pointer) list)
(defun win32--acl-entries (acl)
  "Return ACL's entries as (TYPE MASK . SID-OCTETS), TYPE being :ALLOW or :OTHER."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 12)))
    (when (zerop (win32--get-acl-information (sb-alien:sap-alien acl (* t))
                                             (sb-alien:alien-sap information) 12 2))
      (win32--fail ':security nil))
    (loop for index below (sb-sys:sap-ref-32 (sb-alien:alien-sap information) 0)
          collect (sb-alien:with-alien ((ace (* t)))
                    (when (zerop (win32--get-ace (sb-alien:sap-alien acl (* t))
                                                 index (sb-alien:addr ace)))
                      (win32--fail ':security nil))
                    (let ((sap (sb-alien:alien-sap ace)))
                      (list* (if (zerop (sb-sys:sap-ref-8 sap 0)) ':allow ':other)
                             (sb-sys:sap-ref-32 sap 4)
                             (win32--sid-octets (sb-sys:sap+ sap 8))))))))

(-> win32--handle-privacy (integer) (values boolean boolean boolean))
(defun win32--handle-privacy (handle)
  "Return whether the object behind HANDLE is owned by the user, private to the
user, and withholds write access from the user.

Every value is false when the security descriptor cannot be read, which
happens for objects the user may see but not inspect. Write access is judged
only for private objects, whose access control list ls-compat wrote."
  (sb-alien:with-alien ((owner (* t)) (dacl (* t)) (descriptor (* t)))
    (let ((status (win32--get-security-info handle *win32-se-file-object*
                                            *win32-owner-and-dacl-information*
                                            (sb-alien:addr owner) nil
                                            (sb-alien:addr dacl) nil
                                            (sb-alien:addr descriptor))))
      (if (not (zerop status))
          (values nil nil nil)
          (unwind-protect
               (let* ((user (win32--current-user-sid))
                      (owned-p (win32--owner-p (win32--sid-octets (sb-alien:alien-sap owner))))
                      (entries (if (zerop (sb-sys:sap-int (sb-alien:alien-sap dacl)))
                                   ':unrestricted
                                   (win32--acl-entries (sb-alien:alien-sap dacl))))
                      (private-p
                        (and owned-p
                             (listp entries)
                             (every (lambda (entry)
                                      (and (eq (first entry) ':allow)
                                           (or (equalp (cddr entry) user)
                                               (equalp (cddr entry)
                                                       *win32-system-sid-octets*))))
                                    entries)
                             t)))
                 (values owned-p
                         private-p
                         (and private-p
                              (notany (lambda (entry)
                                        (and (equalp (cddr entry) user)
                                             (logtest (second entry)
                                                      *win32-file-write-data*)))
                                      entries))))
            (win32--local-free descriptor))))))

(-> win32--mode (pathname) (integer 0 #o777))
(defun win32--mode (pathname)
  "Return the permission bits ls-compat derives from PATHNAME's access control list."
  (handler-case
      (ls-compat.posix:file-mode pathname)
    (ls-compat.posix:mode-failed (condition)
      (error 'platform-error
             :message (ls-compat.posix:mode-failed-message condition)
             :operation ':protect
             :pathname pathname
             :reason ':failed
             :code nil))))

(-> win32--set-mode (pathname (integer 0 #o777)) null)
(defun win32--set-mode (pathname mode)
  "Express MODE on PATHNAME through ls-compat's access control list mapping."
  (handler-case
      (setf (ls-compat.posix:file-mode pathname) mode)
    (ls-compat.posix:mode-failed (condition)
      (error 'platform-error
             :message (ls-compat.posix:mode-failed-message condition)
             :operation ':protect
             :pathname pathname
             :reason ':failed
             :code nil)))
  nil)


;;;; -- Environment --

(defmethod platform-set-environment-variable ((platform win32-platform)
                                              name value)
  "Set or remove NAME in the process environment block and the C runtime's copy.

SetEnvironmentVariableW changes what UIOP:GETENV and child processes see, and
_wputenv keeps the C runtime's environ, which getenv reads, in step: the
runtime propagates its own changes to the process block only for variables the
process did not inherit."
  (declare (ignore platform))
  (win32--wputenv (format nil "~A=~A" name (or value "")))
  (when (and (zerop (win32--set-environment-variable name value))
             (or value
                 (/= (win32--get-last-error)
                     *win32-error-environment-variable-not-found*)))
    (win32--fail ':environment nil))
  nil)


;;;; -- Files --

(-> win32--absolute-environment-directory (string) (option pathname))
(defun win32--absolute-environment-directory (variable)
  "Return the absolute directory named by VARIABLE, or NIL when unset or relative."
  (let ((value (uiop:getenv variable)))
    (when (non-empty-string-p value)
      (let ((pathname (uiop:parse-native-namestring value)))
        (when (uiop:absolute-pathname-p pathname)
          (uiop:ensure-directory-pathname pathname))))))

(-> win32--known-folder (string) pathname)
(defun win32--known-folder (variable)
  "Return the known folder the shell exposes through environment VARIABLE."
  (or (win32--absolute-environment-directory variable)
      (error 'platform-error
             :message (format nil "The ~A environment variable does not name an absolute directory." variable)
             :operation ':roots
             :reason ':missing)))

(defmethod platform-parse-namestring ((platform win32-platform) string)
  "Read STRING natively, so drive letters and backslashes mean what Windows means."
  (declare (ignore platform))
  (uiop:parse-native-namestring string))

(-> win32--final-path-namestring (string) string)
(defun win32--final-path-namestring (final)
  "Return FINAL, a kernel final path, without its verbatim prefix."
  (cond ((uiop:string-prefix-p "\\\\?\\UNC\\" final)
         (concatenate 'string "\\\\" (subseq final 8)))
        ((uiop:string-prefix-p "\\\\?\\" final)
         (subseq final 4))
        (t
         final)))

(defmethod platform-truename ((platform win32-platform) pathname)
  "Ask the kernel for PATHNAME's final path, which resolves links and junctions.

GetFinalPathNameByHandleW reports the normalized on-disk name of what a handle
opened with backup semantics refers to, so directories and files alike resolve
through every symbolic link on the way."
  (declare (ignore platform))
  (let ((handle (win32--create-file (win32--namestring pathname) 0 *win32-share-all*
                                    nil *win32-open-existing*
                                    *win32-file-flag-backup-semantics* 0)))
    (when (= handle *win32-invalid-handle*)
      (win32--fail ':resolve pathname))
    (unwind-protect
         (sb-alien:with-alien ((buffer (sb-alien:array (sb-alien:unsigned 16) 32768)))
           (let ((length (win32--get-final-path-name-by-handle
                          handle (sb-alien:alien-sap buffer) 32768 0)))
             (when (or (zerop length) (> length 32768))
               (win32--fail ':resolve pathname))
             (let* ((final
                      (win32--final-path-namestring
                       (coerce (loop for index below length
                                     collect (code-char
                                              (sb-sys:sap-ref-16 (sb-alien:alien-sap buffer)
                                                                 (* 2 index))))
                               'string)))
                    (attributes (win32--get-file-attributes final)))
               (uiop:parse-native-namestring
                final
                :ensure-directory (and (/= attributes *win32-invalid-file-attributes*)
                                       (logtest attributes
                                                *win32-file-attribute-directory*))))))
      (win32--close-handle handle))))

(defmethod platform-application-root ((platform win32-platform) kind)
  "Honour an absolute XDG variable, and otherwise use the application data folders.

Configuration lives under the roaming application data folder, and data,
state, and cache under the local one, each in its own subdirectory."
  (ecase kind
    (:config
     (merge-pathnames "autolith/"
                      (or (win32--absolute-environment-directory "XDG_CONFIG_HOME")
                          (win32--known-folder "APPDATA"))))
    (:data
     (let ((xdg (win32--absolute-environment-directory "XDG_DATA_HOME")))
       (if xdg
           (merge-pathnames "autolith/" xdg)
           (merge-pathnames "autolith/data/" (win32--known-folder "LOCALAPPDATA")))))
    (:state
     (let ((xdg (win32--absolute-environment-directory "XDG_STATE_HOME")))
       (if xdg
           (merge-pathnames "autolith/" xdg)
           (merge-pathnames "autolith/state/" (win32--known-folder "LOCALAPPDATA")))))
    (:cache
     (let ((xdg (win32--absolute-environment-directory "XDG_CACHE_HOME")))
       (if xdg
           (merge-pathnames "autolith/" xdg)
           (merge-pathnames "autolith/cache/" (win32--known-folder "LOCALAPPDATA")))))))

(-> win32--filetime->seconds (integer) integer)
(defun win32--filetime->seconds (filetime)
  "Return FILETIME as whole seconds, the resolution compared across observations."
  (floor filetime 10000000))

(-> win32--handle-status (integer &key (:link-p boolean)) platform-file-status)
(defun win32--handle-status (handle &key link-p)
  "Return the status of the object behind HANDLE, reported as a link when LINK-P."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 64))
                        (basic (sb-alien:array (sb-alien:unsigned 8) 40)))
    (let ((sap (sb-alien:alien-sap information))
          (basic-sap (sb-alien:alien-sap basic)))
      (when (zerop (win32--get-file-information-by-handle handle sap))
        (win32--fail ':status nil))
      (when (zerop (win32--get-file-information-by-handle-ex handle 0 basic-sap 40))
        (win32--fail ':status nil))
      (let* ((attributes (sb-sys:sap-ref-32 sap 0))
             (kind (cond
                     ((or link-p (logtest attributes *win32-file-attribute-reparse-point*))
                      ':symbolic-link)
                     ((logtest attributes *win32-file-attribute-directory*)
                      ':directory)
                     ((= (win32--get-file-type handle) *win32-file-type-disk*)
                      ':file)
                     (t
                      ':other))))
        (multiple-value-bind (owned-p private-p unwritable-p)
            (win32--handle-privacy handle)
          (make-instance 'platform-file-status
                         :kind kind
                         :identity (cons (sb-sys:sap-ref-32 sap 28)
                                         (logior (ash (sb-sys:sap-ref-32 sap 44) 32)
                                                 (sb-sys:sap-ref-32 sap 48)))
                         :size (logior (ash (sb-sys:sap-ref-32 sap 32) 32)
                                       (sb-sys:sap-ref-32 sap 36))
                         :modification-time (sb-sys:sap-ref-64 basic-sap 16)
                         :change-time (sb-sys:sap-ref-64 basic-sap 24)
                         :owned-p owned-p
                         :private-p private-p
                         :read-only-p (or unwritable-p
                                          (logtest attributes
                                                   *win32-file-attribute-readonly*))))))))

(-> win32--open-for-status (pathname boolean) integer)
(defun win32--open-for-status (pathname follow-links-p)
  "Open PATHNAME for inspection, returning its handle or signaling for absence."
  (let ((native (win32--namestring pathname))
        (flags (logior *win32-file-flag-backup-semantics*
                       (if follow-links-p 0 *win32-file-flag-open-reparse-point*))))
    (let ((handle (win32--create-file native
                                      (logior *win32-file-read-attributes*
                                              *win32-read-control*)
                                      *win32-share-all* nil *win32-open-existing*
                                      flags 0)))
      (when (and (= handle *win32-invalid-handle*)
                 (= (win32--get-last-error) *win32-error-access-denied*))
        (setf handle (win32--create-file native *win32-file-read-attributes*
                                         *win32-share-all* nil *win32-open-existing*
                                         flags 0)))
      (when (= handle *win32-invalid-handle*)
        (win32--fail ':status pathname))
      handle)))

(defmethod platform-path-status ((platform win32-platform) pathname
                                 &key follow-links-p)
  "Open PATHNAME for inspection, following reparse points only when asked."
  (handler-case
      (let ((handle (win32--open-for-status pathname follow-links-p)))
        (unwind-protect
             (win32--handle-status handle)
          (win32--close-handle handle)))
    (platform-error (condition)
      (if (eq (platform-error-reason condition) ':missing)
          nil
          (error condition)))))

(defmethod platform-stream-status ((platform win32-platform) stream)
  "Inspect the handle behind STREAM."
  (win32--handle-status (sb-sys:fd-stream-fd stream)))

(defmethod platform-open-regular-file ((platform win32-platform) pathname
                                       &key follow-links-p)
  "Open PATHNAME for reading, refusing reparse points unless FOLLOW-LINKS-P."
  (let ((handle (win32--create-file (win32--namestring pathname)
                                    *win32-generic-read* *win32-share-all* nil
                                    *win32-open-existing*
                                    (logior *win32-file-attribute-normal*
                                            (if follow-links-p
                                                0
                                                *win32-file-flag-open-reparse-point*))
                                    0)))
    (when (= handle *win32-invalid-handle*)
      (win32--fail ':open pathname))
    (unwind-protect
         (let ((status (win32--handle-status handle)))
           (case (platform-file-status-kind status)
             (:symbolic-link
              (error 'platform-error
                     :message (format nil "~A is a symbolic link or junction."
                                      (win32--namestring pathname))
                     :operation ':open
                     :pathname pathname
                     :reason ':symbolic-link))
             (:file
              nil)
             (t
              (error 'platform-error
                     :message (format nil "~A is not a regular file."
                                      (win32--namestring pathname))
                     :operation ':open
                     :pathname pathname
                     :reason ':not-regular)))
           (let ((stream (sb-sys:make-fd-stream handle
                                                :input t
                                                :element-type '(unsigned-byte 8)
                                                :auto-close t)))
             (setf handle nil)
             (values stream status)))
      (when handle
        (win32--close-handle handle)))))

(defmethod platform-create-private-file ((platform win32-platform) pathname)
  "Create PATHNAME exclusively and restrict it to the current user."
  (let ((handle (win32--create-file (win32--namestring pathname)
                                    *win32-generic-write* *win32-share-all* nil
                                    *win32-create-new* *win32-file-attribute-normal* 0)))
    (when (= handle *win32-invalid-handle*)
      (win32--fail ':create pathname))
    (unwind-protect
         (progn
           (win32--set-mode pathname #o600)
           (let ((stream (sb-sys:make-fd-stream handle
                                                :output t
                                                :element-type '(unsigned-byte 8)
                                                :auto-close t)))
             (setf handle nil)
             stream))
      (when handle
        (win32--close-handle handle)))))

(-> win32--attributes (pathname) integer)
(defun win32--attributes (pathname)
  "Return PATHNAME's file attributes."
  (let ((attributes (win32--get-file-attributes (win32--namestring pathname))))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--fail ':protect pathname))
    attributes))

(-> win32--set-attributes (pathname integer) null)
(defun win32--set-attributes (pathname attributes)
  "Set PATHNAME's file attributes to ATTRIBUTES."
  (when (zerop (win32--set-file-attributes (win32--namestring pathname) attributes))
    (win32--fail ':protect pathname))
  nil)

(defmethod platform-make-private ((platform win32-platform) pathname
                                  &key read-only-p)
  "Grant PATHNAME to the owner and SYSTEM only through its access control list.

A read-only file withholds write access from the owner instead of carrying
the read-only attribute, so it can still be deleted and replaced, as an
unwritable POSIX file in a writable directory can. Administrators can take
ownership of any object, so this restricts ordinary access rather than
defeating an administrator."
  (win32--set-mode pathname
                   (cond
                     ((logtest (win32--attributes pathname)
                               *win32-file-attribute-directory*)
                      #o700)
                     (read-only-p
                      #o400)
                     (t
                      #o600))))

(defmethod platform-make-read-only ((platform win32-platform) pathname)
  "Withhold write access from PATHNAME's owner while keeping it private.

Windows has no world-readable bit, so the file stays owner-only; every file
Autolith publishes read-only lives below a private root anyway."
  (win32--set-mode pathname #o400))

(defmethod platform-copy-file-permissions ((platform win32-platform)
                                           source target)
  "Give TARGET the permissions SOURCE's access control list expresses."
  (win32--set-mode target (win32--mode source)))

(defmethod platform-set-file-times ((platform win32-platform) pathname
                                    universal-time)
  "Set PATHNAME's access and write times with SetFileTime."
  (let ((handle (win32--create-file (win32--namestring pathname)
                                    *win32-file-write-attributes* *win32-share-all*
                                    nil *win32-open-existing*
                                    *win32-file-flag-backup-semantics* 0)))
    (when (= handle *win32-invalid-handle*)
      (win32--fail ':times pathname))
    (unwind-protect
         (sb-alien:with-alien ((filetime (sb-alien:unsigned 64)))
           (setf filetime (* (max 0 (+ universal-time *win32-filetime-epoch-offset*))
                             10000000))
           (when (zerop (win32--set-file-time handle nil (sb-alien:addr filetime)
                                              (sb-alien:addr filetime)))
             (win32--fail ':times pathname)))
      (win32--close-handle handle)))
  nil)

(defmethod platform-publish-new-file ((platform win32-platform) source target)
  "Hard-link SOURCE to TARGET, which fails atomically when TARGET exists.

Hard links need an NTFS volume; other filesystems report the failure typed."
  (when (zerop (win32--create-hard-link (win32--namestring target)
                                        (win32--namestring source) nil))
    (win32--fail ':publish target))
  nil)

(-> win32--clear-read-only-attribute (pathname) null)
(defun win32--clear-read-only-attribute (pathname)
  "Clear PATHNAME's read-only attribute when it carries one."
  (let ((attributes (win32--get-file-attributes (win32--namestring pathname))))
    (when (and (/= attributes *win32-invalid-file-attributes*)
               (logtest attributes *win32-file-attribute-readonly*))
      (win32--set-attributes pathname
                             (logandc2 attributes *win32-file-attribute-readonly*))))
  nil)

(defmethod platform-replace-file ((platform win32-platform) source target)
  "Move SOURCE over TARGET, clearing a read-only attribute Windows would refuse."
  (win32--clear-read-only-attribute target)
  (when (zerop (win32--move-file-ex (win32--namestring source)
                                    (win32--namestring target)
                                    *win32-move-file-replace-existing*))
    (win32--fail ':replace target))
  nil)

(-> win32--executable-extensions () list)
(defun win32--executable-extensions ()
  "Return the lowercase extensions PATHEXT declares executable."
  (remove "" (mapcar #'string-downcase
                     (uiop:split-string (or (uiop:getenv "PATHEXT")
                                            ".COM;.EXE;.BAT;.CMD")
                                        :separator '(#\;)))
          :test #'string=))

(defmethod platform-executable-file-p ((platform win32-platform) pathname)
  "Accept a regular file whose name carries a PATHEXT extension."
  (let ((status (platform-path-status platform pathname :follow-links-p t))
        (lowercase (string-downcase (win32--namestring pathname))))
    (and status
         (eq (platform-file-status-kind status) ':file)
         (some (lambda (extension)
                 (uiop:string-suffix-p lowercase extension))
               (win32--executable-extensions))
         t)))

(defmethod platform-make-temporary-directory ((platform win32-platform)
                                              parent prefix)
  "Create a private directory named PREFIX plus random characters below PARENT."
  (loop repeat 100
        for suffix = (map 'string
                          (lambda (octet)
                            (char "abcdefghijklmnopqrstuvwxyz0123456789"
                                  (mod octet 36)))
                          (win32--random-octets 8))
        for directory = (uiop:ensure-directory-pathname
                         (merge-pathnames (concatenate 'string prefix suffix) parent))
        do (cond
             ((not (zerop (win32--create-directory (win32--namestring directory) nil)))
              (win32--set-mode directory #o700)
              (return directory))
             ((not (= (win32--get-last-error) *win32-error-already-exists*))
              (win32--fail ':create directory)))
        finally (error 'platform-error
                       :message "Could not find an unused temporary directory name."
                       :operation ':create
                       :pathname parent
                       :reason ':exists)))

(defmethod platform-list-directory ((platform win32-platform) pathname
                                    &key (limit most-positive-fixnum))
  "Enumerate PATHNAME with FindFirstFileW and FindNextFileW."
  (sb-alien:with-alien ((data (sb-alien:array (sb-alien:unsigned 8) 640)))
    (let* ((sap (sb-alien:alien-sap data))
           (handle (win32--find-first-file
                    (concatenate 'string (win32--namestring pathname) "\\*") sap))
           (names nil)
           (count 0)
           (more-p nil))
      (when (= handle *win32-invalid-handle*)
        (win32--fail ':list pathname))
      (unwind-protect
           (loop
             (let ((name (coerce (loop for index from 0 below 260
                                       for code = (sb-sys:sap-ref-16 sap (+ 44 (* 2 index)))
                                       until (zerop code)
                                       collect (code-char code))
                                 'string)))
               (unless (member name '("." "..") :test #'string=)
                 (if (< count limit)
                     (progn
                       (push name names)
                       (incf count))
                     (progn
                       (setf more-p t)
                       (return)))))
             (when (zerop (win32--find-next-file handle sap))
               (return)))
        (win32--find-close handle))
      (values (nreverse names) more-p))))

(defmethod platform-shared-library-file-name ((platform win32-platform)
                                              base-name)
  "Add the Windows dynamic library extension to BASE-NAME."
  (format nil "~A.dll" base-name))

(defmethod platform-delete-directory-tree ((platform win32-platform) pathname
                                           &rest arguments
                                           &key (validate nil validate-p)
                                             if-does-not-exist)
  "Delete PATHNAME's tree after clearing the read-only attributes Windows refuses.

Git marks its object files read-only, and DeleteFileW refuses such a file
whatever its directory allows, so every entry is made writable first once
PATHNAME passes the validation UIOP applies."
  (declare (ignore platform if-does-not-exist))
  (when (and validate-p
             (pathnamep pathname)
             (uiop:directory-pathname-p pathname)
             (not (wild-pathname-p pathname))
             (uiop:call-function validate pathname)
             (uiop:directory-exists-p pathname))
    (uiop:collect-sub*directories
     pathname
     (constantly t)
     (constantly t)
     (lambda (directory)
       (win32--clear-read-only-attribute directory)
       (map nil #'win32--clear-read-only-attribute
            (uiop:directory-files directory)))))
  (apply #'uiop:delete-directory-tree pathname arguments))


;;;; -- Terminal and Shell --

(-> win32--console-mode (integer) (option integer))
(defun win32--console-mode (handle)
  "Return HANDLE's console mode, or NIL when it is not a console."
  (sb-alien:with-alien ((mode win32-dword))
    (if (zerop (win32--get-console-mode handle (sb-alien:addr mode)))
        nil
        mode)))

(defmethod platform-interactive-descriptor-p ((platform win32-platform)
                                              descriptor)
  "Treat DESCRIPTOR as interactive when it is a console handle."
  (and (integerp descriptor)
       (not (minusp descriptor))
       (not (null (win32--console-mode descriptor)))))

(defmethod platform-disable-input-echo ((platform win32-platform) descriptor)
  "Clear ENABLE_ECHO_INPUT on console DESCRIPTOR and return the previous mode."
  (let ((mode (win32--console-mode descriptor)))
    (unless mode
      (win32--fail ':terminal nil))
    (when (zerop (win32--set-console-mode
                  descriptor (logandc2 mode *win32-enable-echo-input*)))
      (win32--fail ':terminal nil))
    mode))

(defmethod platform-restore-input-echo ((platform win32-platform)
                                        descriptor state)
  "Reinstall console mode STATE on DESCRIPTOR."
  (when (zerop (win32--set-console-mode descriptor state))
    (win32--fail ':terminal nil))
  nil)

(-> win32--console-window-size () (option cons))
(defun win32--console-window-size ()
  "Return the standard output console window as (ROWS . COLUMNS), or NIL."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 24)))
    (let ((sap (sb-alien:alien-sap information)))
      (if (zerop (win32--get-console-screen-buffer-info
                  (win32--get-std-handle *win32-standard-output-handle*) sap))
          nil
          (cons (1+ (- (sb-sys:signed-sap-ref-16 sap 16)
                       (sb-sys:signed-sap-ref-16 sap 12)))
                (1+ (- (sb-sys:signed-sap-ref-16 sap 14)
                       (sb-sys:signed-sap-ref-16 sap 10))))))))

(defclass win32-resize-watch ()
  ((thread
    :initarg :thread
    :accessor win32-resize-watch-thread
    :type t
    :documentation "The polling thread comparing console window sizes.")
   (stop-p
    :initform nil
    :accessor win32-resize-watch-stop-p
    :type boolean
    :documentation "Whether the watcher has been asked to finish."))
  (:documentation "A polling watcher standing in for SIGWINCH on Windows."))

(defmethod platform-watch-terminal-resize ((platform win32-platform) function)
  "Poll the console window size on a thread and call FUNCTION when it changes."
  (let ((watch (make-instance 'win32-resize-watch :thread nil)))
    (setf (win32-resize-watch-thread watch)
          (make-thread
           (lambda ()
             (loop with previous = (win32--console-window-size)
                   until (win32-resize-watch-stop-p watch)
                   do (sleep *win32-resize-poll-seconds*)
                      (let ((current (win32--console-window-size)))
                        (unless (equal current previous)
                          (setf previous current)
                          (funcall function)))))
           :name "Autolith console resize watcher"))
    watch))

(defmethod platform-unwatch-terminal-resize ((platform win32-platform) token)
  "Stop TOKEN's polling thread and wait for it to finish."
  (setf (win32-resize-watch-stop-p token) t)
  (let ((thread (win32-resize-watch-thread token)))
    (when (and thread (thread-alive-p thread))
      (join-thread thread)))
  nil)

(-> win32--git-shell () (option pathname))
(defun win32--git-shell ()
  "Return Git for Windows' POSIX shell when it is installed, or NIL."
  (let ((program-files (or (uiop:getenv "ProgramFiles") "C:\\Program Files")))
    (loop for candidate in (list (merge-pathnames "Git/usr/bin/sh.exe"
                                                  (uiop:ensure-directory-pathname
                                                   (uiop:parse-native-namestring
                                                    program-files)))
                                 (merge-pathnames "Git/bin/sh.exe"
                                                  (uiop:ensure-directory-pathname
                                                   (uiop:parse-native-namestring
                                                    program-files))))
          when (uiop:file-exists-p candidate)
            return candidate)))

(defmethod platform-shell-command-line ((platform win32-platform) command)
  "Run COMMAND through Git for Windows' shell, or PowerShell without it."
  (let ((shell (win32--git-shell)))
    (if shell
        (list (uiop:native-namestring shell) "-c" command)
        (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command" command))))


;;;; -- Local Sockets --

(defmethod platform-local-listener ((platform win32-platform) pathname
                                    &key backlog)
  "Refuse, since Windows SBCL has no filesystem sockets."
  (declare (ignore pathname backlog))
  (win32--unavailable
   ':local-sockets
   "Windows SBCL has no filesystem sockets; use the TCP transport."))

(defmethod platform-connect-local ((platform win32-platform) pathname)
  "Refuse, since Windows SBCL has no filesystem sockets."
  (declare (ignore pathname))
  (win32--unavailable
   ':local-sockets
   "Windows SBCL has no filesystem sockets; use the TCP transport."))


;;;; -- Installation --

(setf *platform* (make-instance 'win32-platform))
