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

(defmethod platform-file-uri ((platform win32-platform) pathname)
  "Encode drive and UNC paths using URI slashes rather than native separators."
  (let ((name (substitute #\/ #\\ (uiop:native-namestring pathname))))
    (if (uiop:string-prefix-p "//" name)
        (concatenate 'string "file:" (platform--encode-uri-path name))
        (concatenate 'string "file:///" (subseq name 0 2)
                     (platform--encode-uri-path (subseq name 2))))))


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
(win32--define win32--close-handle "CloseHandle" win32-bool (handle win32-handle))
(win32--define win32--create-job-object "CreateJobObjectW" win32-handle
  (security (* t)) (name win32-wide-string))
(win32--define win32--assign-process-to-job-object "AssignProcessToJobObject" win32-bool
  (job win32-handle) (process win32-handle))
(win32--define win32--terminate-job-object "TerminateJobObject" win32-bool
  (job win32-handle) (code win32-dword))
(win32--define win32--resume-thread "ResumeThread" win32-dword
  (thread win32-handle))
(win32--define win32--initialize-attributes "InitializeProcThreadAttributeList"
    win32-bool
  (attributes (* t)) (count win32-dword) (flags win32-dword)
  (size (* (sb-alien:unsigned 64))))
(win32--define win32--update-attribute "UpdateProcThreadAttribute" win32-bool
  (attributes (* t)) (flags win32-dword) (attribute (sb-alien:unsigned 64))
  (value (* t)) (size (sb-alien:unsigned 64)) (previous (* t)) (returned (* t)))
(win32--define win32--delete-attributes "DeleteProcThreadAttributeList" sb-alien:void
  (attributes (* t)))
(win32--define win32--query-job "QueryInformationJobObject" win32-bool
  (job win32-handle) (class sb-alien:int) (information (* t))
  (size win32-dword) (returned (* t)))
(win32--define win32--set-job "SetInformationJobObject" win32-bool
  (job win32-handle) (class sb-alien:int) (information (* t)) (size win32-dword))
(win32--define win32--create-process "CreateProcessW" win32-bool
  (application-name win32-wide-string) (command-line win32-wide-string)
  (process-security (* t)) (thread-security (* t)) (inherit-handles win32-bool)
  (creation-flags win32-dword) (environment (* t)) (directory win32-wide-string)
  (startup-info (* t)) (process-information (* t)))
(win32--define win32--get-current-directory "GetCurrentDirectoryW" win32-dword
  (length win32-dword) (buffer (* t)))
(win32--define win32--create-mutex "CreateMutexW" win32-handle
  (security (* t)) (owner win32-bool) (name win32-wide-string))
(win32--define win32--wait-for-single-object "WaitForSingleObject" win32-dword
  (handle win32-handle) (milliseconds win32-dword))
(win32--define win32--release-mutex "ReleaseMutex" win32-bool
  (handle win32-handle))
(win32--define win32--create-file "CreateFileW" win32-handle
  (name win32-wide-string) (access win32-dword) (share win32-dword)
  (security (* t)) (disposition win32-dword) (flags win32-dword)
  (template win32-handle))
(win32--define win32--get-file-attributes "GetFileAttributesW" win32-dword
  (name win32-wide-string))
(win32--define win32--set-file-attributes "SetFileAttributesW" win32-bool
  (name win32-wide-string) (attributes win32-dword))
(win32--define win32--set-environment-variable "SetEnvironmentVariableW" win32-bool
  (name win32-wide-string) (value win32-wide-string))
(win32--define win32--wputenv "_wputenv" sb-alien:int (entry win32-wide-string))
(win32--define win32--move-file-ex "MoveFileExW" win32-bool
  (source win32-wide-string) (target win32-wide-string) (flags win32-dword))
(win32--define win32--set-file-time "SetFileTime" win32-bool
  (handle win32-handle) (creation (* t)) (access (* t)) (write (* t)))
(win32--define win32--create-directory "CreateDirectoryW" win32-bool
  (name win32-wide-string) (security (* t)))
(win32--define win32--open-process "OpenProcess" win32-handle
  (access win32-dword) (inherit win32-bool) (process-id win32-dword))
(win32--define win32--terminate-process "TerminateProcess" win32-bool
  (handle win32-handle) (code win32-dword))
(win32--define win32--get-current-process "GetCurrentProcess" win32-handle)
(win32--define win32--duplicate-handle "DuplicateHandle" win32-bool
  (source-process win32-handle) (source-handle win32-handle)
  (target-process win32-handle) (target-handle (* win32-handle))
  (desired-access win32-dword) (inherit-handle win32-bool)
  (options win32-dword))
(win32--define win32--get-std-handle "GetStdHandle" win32-handle (which win32-dword))
(win32--define win32--get-console-mode "GetConsoleMode" win32-bool
  (handle win32-handle) (mode (* win32-dword)))
(win32--define win32--set-console-mode "SetConsoleMode" win32-bool
  (handle win32-handle) (mode win32-dword))
(win32--define win32--get-console-screen-buffer-info "GetConsoleScreenBufferInfo"
    win32-bool
  (handle win32-handle) (information (* t)))


;;;; -- Win32 Constants --

(defparameter *win32-invalid-handle* -1
  "The handle value CreateFileW and FindFirstFileW return on failure.")

(defparameter *win32-generic-read* #x80000000
  "GENERIC_READ, which includes READ_CONTROL for security queries.")

(defparameter *win32-generic-write* #x40000000
  "GENERIC_WRITE, which includes READ_CONTROL for security queries.")

(defparameter *win32-file-write-attributes* #x100
  "FILE_WRITE_ATTRIBUTES, enough access to change file times.")

(defparameter *win32-share-all* 7
  "FILE_SHARE_READ, FILE_SHARE_WRITE, and FILE_SHARE_DELETE together.")

(defparameter *win32-open-existing* 3
  "The CreateFileW disposition that fails when the file is absent.")

(defparameter *win32-file-attribute-readonly* #x1
  "FILE_ATTRIBUTE_READONLY.")

(defparameter *win32-file-attribute-directory* #x10
  "FILE_ATTRIBUTE_DIRECTORY.")

(defparameter *win32-file-attribute-normal* #x80
  "FILE_ATTRIBUTE_NORMAL.")

(defparameter *win32-file-flag-backup-semantics* #x02000000
  "FILE_FLAG_BACKUP_SEMANTICS, required to open a directory handle.")

(defparameter *win32-invalid-file-attributes* #xFFFFFFFF
  "The GetFileAttributesW result meaning failure.")

(defparameter *win32-error-environment-variable-not-found* 203
  "ERROR_ENVVAR_NOT_FOUND, reported when removing a variable that is absent.")

(defparameter *win32-move-file-replace-existing* 1
  "MOVEFILE_REPLACE_EXISTING.")

(defparameter *win32-process-terminate* #x1
  "The OpenProcess access right that allows TerminateProcess.")

(defparameter *win32-process-synchronize* #x100000
  "The OpenProcess access right that allows waiting for process termination.")

(defparameter *win32-create-new-process-group* #x200
  "CREATE_NEW_PROCESS_GROUP for a detached child.")

(defparameter *win32-detached-process* #x8
  "DETACHED_PROCESS for a child without the parent's console.")

(defparameter *win32-startf-use-std-handles* #x100
  "STARTF_USESTDHANDLES in STARTUPINFO.dwFlags.")

(defparameter *win32-handle-flag-inherit* 1
  "HANDLE_FLAG_INHERIT for child standard handles.")

(defparameter *win32-duplicate-same-access* 2
  "DUPLICATE_SAME_ACCESS for inherited handle copies.")

(defparameter *win32-standard-input-handle* #xFFFFFFF6
  "STD_INPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *win32-standard-output-handle* #xFFFFFFF5
  "STD_OUTPUT_HANDLE as the unsigned argument GetStdHandle takes.")

(defparameter *win32-filetime-epoch-offset* 9435484800
  "Seconds from the FILETIME epoch of 1601 to the universal-time epoch of 1900.")

(defparameter *win32-resize-poll-seconds* 0.25
  "How often the resize watcher compares the console window size.")

(defparameter *win32-error-file-not-found* 2
  "ERROR_FILE_NOT_FOUND.")

(defparameter *win32-error-path-not-found* 3
  "ERROR_PATH_NOT_FOUND.")

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
  "Report the optional capabilities implemented by the Windows adapter."
  (declare (ignore platform))
  (not (null (member capability '(:detached-sessions :restartable-image-saver)))))


(defclass win32-detached-process ()
  ((handle
    :initarg :handle
    :reader win32-detached-process-handle
    :documentation "The owned process handle.")
   (job-handle
    :initarg :job-handle
    :reader win32-detached-process-job-handle
    :documentation "The job containing the replacement and its descendants.")
   (process-id
    :initarg :process-id
    :reader win32-detached-process-id
    :documentation "The native process identifier.")
   (closed-p
    :initform nil
    :accessor win32-detached-process-closed-p
    :documentation "Whether the process handle has been closed."))
  (:documentation "A native Windows process launched without a console attachment."))

(defmethod platform-session-launch-command ((platform win32-platform) source-root)
  "Run the stable Lisp launcher directly, without cmd.exe argument parsing."
  (declare (ignore platform))
  (let ((launcher (merge-pathnames "script/launcher.lisp" source-root)))
    (unless (probe-file launcher)
      (error 'platform-error :operation ':launch :pathname launcher
             :message "The stable Autolith launcher is unavailable."))
    (list (namestring sb-ext:*runtime-pathname*) "--noinform"
          "--no-userinit" "--no-sysinit" "--script" (namestring launcher))))
(defun win32--command-line-argument (argument)
  "Quote ARGUMENT for the Windows command-line parser."
  (if (and (> (length argument) 0)
           (not (find-if (lambda (character)
                           (find character '(#\Space #\Tab #\")))
                         argument)))
      argument
      (with-output-to-string (stream)
        (write-char #\" stream)
        (let ((slashes 0))
          (loop for character across argument do
            (if (char= character #\\)
                (incf slashes)
                (progn
                  (dotimes (ignored (if (char= character #\")
                                        (1+ (* 2 slashes))
                                        slashes))
                    (declare (ignore ignored))
                    (write-char #\\ stream))
                  (setf slashes 0)
                  (write-char character stream))))
          (dotimes (ignored (* 2 slashes))
            (declare (ignore ignored))
            (write-char #\\ stream)))
        (write-char #\" stream))))

(defun win32--command-line (arguments)
  "Build a mutable CreateProcessW command line from ARGUMENTS."
  (format nil "~{~A~^ ~}" (mapcar #'win32--command-line-argument arguments)))

(defun win32--stream-handle (stream)
  "Return STREAM's native handle, or zero when it has none."
  (if (and stream (find-symbol "FD-STREAM-FD" "SB-SYS")
           (typep stream 'sb-sys:fd-stream))
      (funcall (symbol-function (find-symbol "FD-STREAM-FD" "SB-SYS")) stream)
      0))

(defun win32--sap-uint64 (sap offset)
  "Read an unsigned 64-bit value from SAP at byte OFFSET."
  (logior (sb-sys:sap-ref-32 sap offset)
          (ash (sb-sys:sap-ref-32 sap (+ offset 4)) 32)))

(defun win32--duplicate-inheritable-handle (handle)
  "Return an inheritable duplicate of HANDLE."
  (sb-alien:with-alien ((copy win32-handle))
    (unless
        (plusp
         (win32--duplicate-handle
          (win32--get-current-process) handle
          (win32--get-current-process) (sb-alien:addr copy)
          0 1 *win32-duplicate-same-access*))
      (win32--fail ':launch nil))
    copy))

(defun win32--null-handle ()
  "Open NUL for use as valid child standard handles."
  (let ((handle
          (win32--create-file "NUL"
                              (logior *win32-generic-read* *win32-generic-write*)
                              *win32-share-all* nil *win32-open-existing*
                              *win32-file-attribute-normal* 0)))
    (when (= handle *win32-invalid-handle*)
      (win32--fail ':launch nil))
    handle))

(defun win32--call-with-inherited-handles (handles function)
  "Call FUNCTION with a native attribute list inheriting only HANDLES."
  (sb-alien:with-alien ((size (sb-alien:unsigned 64) 0)
                       (list (sb-alien:array win32-handle 3)))
    (win32--initialize-attributes nil 1 0 (sb-alien:addr size))
    (let ((attributes (sb-alien:make-alien (sb-alien:unsigned 8) size))
          (initialized-p nil))
      (unwind-protect
           (progn
             (unless (plusp (win32--initialize-attributes attributes 1 0
                                                         (sb-alien:addr size)))
               (win32--fail ':launch nil))
             (setf initialized-p t)
             (loop for handle in handles for index from 0
                   do (setf (sb-alien:deref list index) handle))
             ;; PROC_THREAD_ATTRIBUTE_HANDLE_LIST, exactly three standard handles.
             (unless (plusp (win32--update-attribute
                             attributes 0 #x20002 (sb-alien:alien-sap list)
                             24 nil nil))
               (win32--fail ':launch nil))
             (funcall function (sb-alien:alien-sap attributes)))
        (when initialized-p (win32--delete-attributes attributes))
        (sb-alien:free-alien attributes)))))

(defun win32--job-kill-on-close (job enabled-p)
  "Make JOB die with its launch owner until ownership is explicitly released."
  (sb-alien:with-alien ((limits (sb-alien:array (sb-alien:unsigned 8) 144)))
    (let ((sap (sb-alien:alien-sap limits)))
      (dotimes (index 144) (setf (sb-sys:sap-ref-8 sap index) 0))
      (setf (sb-sys:sap-ref-32 sap 16) (if enabled-p #x2000 0))
      (unless (plusp (win32--set-job job 9 sap 144))
        (win32--fail ':launch nil)))))
(defmethod platform-launch-detached-process
    ((platform win32-platform) arguments
     &key directory output ticket)
  "Create a suspended replacement, assign its entire tree, then release it.

The suspended initial thread is the Windows startup gate. The job owns every
child before any user code runs. Kill-on-close covers abandonment; a successful
handoff explicitly releases ownership without terminating the detached session."
  (declare (ignore platform))
  (let ((launcher-pid-pathname
          (and ticket
               (image-daemon:handoff-ticket-sibling ticket ':launcher-pid)))
        (handles nil)
        (process-handle 0)
        (thread-handle 0)
        (job-handle 0)
        (completed-p nil))
    (unwind-protect
         (let* ((null-source (win32--null-handle))
                (source-output (win32--stream-handle output)))
           (push null-source handles)
           (let* ((input (win32--duplicate-inheritable-handle null-source))
                  (out (progn
                         (push input handles)
                         (win32--duplicate-inheritable-handle
                          (if (plusp source-output) source-output null-source))))
                  (err (progn
                         (push out handles)
                         (win32--duplicate-inheritable-handle
                          (if (plusp source-output) source-output null-source)))))
             (push err handles)
             (setf job-handle (win32--create-job-object nil nil))
             (when (zerop job-handle) (win32--fail ':launch nil))
             (win32--job-kill-on-close job-handle t)
             (win32--call-with-inherited-handles
              (list input out err)
              (lambda (attributes)
                (sb-alien:with-alien
                    ((startup (sb-alien:array (sb-alien:unsigned 8) 112))
                     (information (sb-alien:array (sb-alien:unsigned 8) 24)))
                  (let ((start (sb-alien:alien-sap startup))
                        (info (sb-alien:alien-sap information)))
                    (dotimes (index 112) (setf (sb-sys:sap-ref-8 start index) 0))
                    (dotimes (index 24) (setf (sb-sys:sap-ref-8 info index) 0))
                    (setf (sb-sys:sap-ref-32 start 0) 112
                          (sb-sys:sap-ref-32 start 60) *win32-startf-use-std-handles*
                          (sb-sys:sap-ref-64 start 80) input
                          (sb-sys:sap-ref-64 start 88) out
                          (sb-sys:sap-ref-64 start 96) err
                          (sb-sys:sap-ref-sap start 104) attributes)
                    ;; CREATE_SUSPENDED | EXTENDED_STARTUPINFO_PRESENT.
                    (unless (plusp (win32--create-process
                                    (first arguments) (win32--command-line arguments)
                                    nil nil 1
                                    (logior #x4 #x80000 *win32-detached-process*
                                            *win32-create-new-process-group*)
                                    nil (win32--namestring directory) start info))
                      (win32--fail ':launch nil))
                    (setf process-handle (win32--sap-uint64 info 0)
                          thread-handle (win32--sap-uint64 info 8))
                    (unless (plusp (win32--assign-process-to-job-object
                                    job-handle process-handle))
                      (win32--fail ':launch nil))
                    (let* ((pid (sb-sys:sap-ref-32 info 16))
                           (process (make-instance 'win32-detached-process
                                                   :handle process-handle
                                                   :job-handle job-handle
                                                   :process-id pid)))
                      (when launcher-pid-pathname
                        (with-open-file (stream launcher-pid-pathname
                                                :direction ':output
                                                :if-exists ':supersede)
                          (format stream "~D~%" pid))
                        (platform-make-private *platform* launcher-pid-pathname))
                      (when (= (win32--resume-thread thread-handle) #xFFFFFFFF)
                        (win32--fail ':launch nil))
                      (setf completed-p t)
                      process)))))))
      (unless completed-p
        (when (plusp job-handle) (win32--terminate-job-object job-handle 1))
        (when (plusp process-handle)
          (win32--terminate-process process-handle 1)
          (win32--wait-for-single-object process-handle #xFFFFFFFF)
          (win32--close-handle process-handle))
        (when (plusp job-handle) (win32--close-handle job-handle)))
      (when (plusp thread-handle) (win32--close-handle thread-handle))
      (dolist (handle handles) (win32--close-handle handle)))))

(defmethod platform-process-object-pid ((platform win32-platform)
                                        (process win32-detached-process))
  "Return the detached root's native process identifier."
  (declare (ignore platform))
  (win32-detached-process-id process))

(defmethod platform-process-object-alive-p ((platform win32-platform)
                                            (process win32-detached-process))
  "Return true while any process in the owned job is still active."
  (declare (ignore platform))
  (and (not (win32-detached-process-closed-p process))
       (sb-alien:with-alien ((accounting (sb-alien:array (sb-alien:unsigned 8) 48)))
         (unless (plusp (win32--query-job
                         (win32-detached-process-job-handle process) 1
                         (sb-alien:alien-sap accounting) 48 nil))
           (win32--fail ':process-state nil))
         (plusp (sb-sys:sap-ref-32 (sb-alien:alien-sap accounting) 40)))))

(defmethod platform-terminate-process-object
    ((platform win32-platform) (process win32-detached-process) &key force)
  "Terminate the entire owned tree, including children of an exited root."
  (declare (ignore platform force))
  (unless (win32-detached-process-closed-p process)
    (unless (plusp (win32--terminate-job-object
                    (win32-detached-process-job-handle process) 1))
      (win32--fail ':terminate nil)))
  nil)

(defmethod platform-release-process ((platform win32-platform)
                                    (process win32-detached-process))
  "Release this caller's handles without terminating a successful handoff."
  (declare (ignore platform))
  (unless (win32-detached-process-closed-p process)
    (win32--job-kill-on-close (win32-detached-process-job-handle process) nil)
    (win32--close-handle (win32-detached-process-handle process))
    (win32--close-handle (win32-detached-process-job-handle process))
    (setf (win32-detached-process-closed-p process) t))
  nil)

(defmethod platform-wait-process ((platform win32-platform)
                                 (process win32-detached-process))
  "Wait for the entire owned tree, then release both native handles."
  (loop while (platform-process-object-alive-p platform process) do (sleep 0.01))
  (platform-release-process platform process)
  t)

(defmethod platform-process-alive-p ((platform win32-platform) process-id)
  "Read through ls-compat whether PROCESS-ID is still active."
  (process-alive-p process-id))

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
  (let ((handle (win32--open-process (logior *win32-process-terminate*
                                             *win32-process-synchronize*)
                                     0 process-id)))
    (when (zerop handle)
      (win32--fail ':terminate nil))
    (unwind-protect
         (progn
           (when (zerop (win32--terminate-process handle 1))
             (win32--fail ':terminate nil))
           ;; TerminateProcess returns before all file handles owned by the
           ;; target have been released.  Wait on the process handle before
           ;; closing it so callers can safely remove files opened by it.
           (unless (zerop (win32--wait-for-single-object handle #xFFFFFFFF))
             (win32--fail ':terminate nil)))
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
  "Do nothing: native detached children have no controlling console."
  (declare (ignore platform))
  nil)


;;;; -- Security --

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

(defmethod platform-file-uri-pathname ((platform win32-platform) decoded-path)
  "Remove a file URI's drive-prefix slash before parsing a native Windows pathname."
  (uiop:parse-native-namestring
   (if (and (> (length decoded-path) 2)
            (char= (char decoded-path 0) #\/)
            (char= (char decoded-path 2) #\:))
       (subseq decoded-path 1)
       decoded-path)))

(defmethod platform-file-permissions ((platform win32-platform) pathname)
  "Capture the ACL mode mapping and native read-only attribute."
  (let ((attributes (win32--get-file-attributes (win32--namestring pathname))))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--fail ':protect pathname))
    (list (win32--mode pathname)
          (not (zerop (logand attributes *win32-file-attribute-readonly*))))))

(defmethod (setf platform-file-permissions) (permissions (platform win32-platform) pathname)
  "Restore the captured mode mapping and read-only attribute."
  (win32--set-mode pathname (first permissions))
  (let ((attributes (win32--get-file-attributes (win32--namestring pathname))))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--fail ':protect pathname))
    (win32--set-attributes
     pathname (if (second permissions)
                  (logior attributes *win32-file-attribute-readonly*)
                  (logandc2 attributes *win32-file-attribute-readonly*))))
  permissions)

(defmethod platform-delete-file ((platform win32-platform) pathname)
  "Clear a native read-only attribute for deletion and restore it if deletion fails."
  (let ((attributes (win32--get-file-attributes (win32--namestring pathname)))
        (complete-p nil))
    (unwind-protect
         (progn
           (win32--clear-read-only-attribute pathname)
           (delete-file pathname)
           (setf complete-p t))
      (when (and (not complete-p)
                 (/= attributes *win32-invalid-file-attributes*)
                 (platform-path-status platform pathname))
        (win32--set-attributes pathname attributes))))
  nil)

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
                          (random-data 8))
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

(-> win32--directory-entry-pathname (win32-platform pathname string) pathname)
(defun win32--directory-entry-pathname (platform directory name)
  "Return native child NAME below DIRECTORY without portable pathname reinterpretation."
  (platform-parse-namestring
   platform (format nil "~A\\~A" (win32--namestring directory) name)))

(-> win32--make-directory-tree-writable (win32-platform pathname) null)
(defun win32--make-directory-tree-writable (platform directory)
  "Recursively clear read-only attributes below DIRECTORY, including dot trees."
  (multiple-value-bind (names more-p)
      (platform-list-directory platform directory)
    (declare (ignore more-p))
    (dolist (name names)
      (let* ((entry (win32--directory-entry-pathname platform directory name))
             (status (platform-path-status platform entry)))
        (when status
          (when (eq (platform-file-status-kind status) ':directory)
            (win32--make-directory-tree-writable
             platform (uiop:ensure-directory-pathname entry)))
          (win32--clear-read-only-attribute entry)))))
  (win32--clear-read-only-attribute directory)
  nil)

(-> win32--delete-directory-tree-with-retries
    (win32-platform pathname list) t)
(defun win32--delete-directory-tree-with-retries (platform pathname arguments)
  "Delete PATHNAME, retrying transient Windows sharing and access failures."
  (loop for attempt from 1 to 100
        do (handler-case
               (return (apply #'uiop:delete-directory-tree pathname arguments))
             (file-error (condition)
               (when (= attempt 100)
                 (error condition))
               (when (uiop:directory-exists-p pathname)
                 (ignore-errors
                   (win32--make-directory-tree-writable platform pathname)))
               (sleep 0.1)))))

(defmethod platform-delete-directory-tree ((platform win32-platform) pathname
                                           &rest arguments
                                           &key (validate nil validate-p)
                                             if-does-not-exist)
  "Delete PATHNAME after recursively clearing attributes Windows refuses.

Native enumeration includes dot-prefixed Git trees that UIOP wildcard traversal
can omit. Bounded retries cover handles released just after a child process exits."
  (declare (ignore if-does-not-exist))
  (when (and validate-p
             (pathnamep pathname)
             (uiop:directory-pathname-p pathname)
             (not (wild-pathname-p pathname))
             (uiop:call-function validate pathname)
             (uiop:directory-exists-p pathname))
    (win32--make-directory-tree-writable platform pathname))
  (win32--delete-directory-tree-with-retries platform pathname arguments))


;;;; -- Terminal and Shell --

(-> win32--console-mode (integer) (option integer))
(defun win32--console-mode (handle)
  "Return HANDLE's console mode, or NIL when it is not a console."
  (sb-alien:with-alien ((mode win32-dword))
    (if (zerop (win32--get-console-mode handle (sb-alien:addr mode)))
        nil
        mode)))

(defmethod platform-terminal-enable-fullscreen ((platform win32-platform))
  "Enable VT output on the Windows console and retain its exact previous mode."
  (let* ((handle (win32--get-std-handle *win32-standard-output-handle*))
         (mode (win32--console-mode handle)))
    (when mode
      ;; ENABLE_PROCESSED_OUTPUT and ENABLE_VIRTUAL_TERMINAL_PROCESSING.
      (when (zerop (win32--set-console-mode handle (logior mode #x0001 #x0004)))
        (win32--fail ':terminal nil))
      (cons handle mode))))

(defmethod platform-terminal-restore-fullscreen ((platform win32-platform) token)
  "Restore the original console output mode after leaving the alternate buffer."
  (when token
    (when (zerop (win32--set-console-mode (first token) (rest token)))
      (win32--fail ':terminal nil)))
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

(defmethod platform-shell-command-line ((platform win32-platform) command)
  "Run COMMAND through native PowerShell in both sandboxed and full-access modes."
  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command" command))

(defmethod platform-open-url ((platform win32-platform) url)
  "Hand URL to the shell's protocol handler, which opens the default browser."
  (declare (ignore platform))
  (platform--launch-quietly (list "rundll32.exe" "url.dll,FileProtocolHandler" url)))

(defmethod platform-source-check-command ((platform win32-platform) source-root)
  "Run the PowerShell repository check script on Windows."
  (declare (ignore platform))
  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-File"
        (uiop:native-namestring (merge-pathnames "script/check.ps1" source-root))))

(defparameter *win32-sandbox-read-roots* nil
  "Additional existing directories to expose read-only to sandboxed Windows commands.
System application directories are available through AppContainer's system access.")

(-> win32--call-with-sandbox-lock (function) t)
(defun win32--call-with-sandbox-lock (function)
  "Serialize sandbox scope lifetimes across Autolith processes and threads."
  (let ((mutex (win32--create-mutex nil 0
                                   "Global\\AutolithCommandSandbox-v1"))
        (owned-p nil))
    (when (zerop mutex)
      (win32--fail ':command-sandbox-lock nil))
    (unwind-protect
         (progn
           (loop until owned-p
                 do (sb-sys:without-interrupts
                      (case (win32--wait-for-single-object mutex 100)
                        ((0 #x80)
                         (setf owned-p t))
                        (#x102
                         nil)
                        (otherwise
                         (win32--fail ':command-sandbox-lock nil)))))
           (funcall function))
      (when owned-p
        (win32--release-mutex mutex))
      (win32--close-handle mutex))))

(-> win32--sandbox-read-roots (win32-platform pathname) list)
(defun win32--sandbox-read-roots (platform workspace)
  "Return configured tool roots outside WORKSPACE and the AppContainer system surface."
  (let ((system-roots
          (remove nil (mapcar #'win32--absolute-environment-directory
                              '("SystemRoot" "ProgramFiles" "ProgramFiles(x86)")))))
    (remove-duplicates
     (loop for value in *win32-sandbox-read-roots*
           for path = (and (or (pathnamep value) (non-empty-string-p value))
                           (uiop:ensure-directory-pathname
                             (if (pathnamep value) value
                                 (uiop:parse-native-namestring value))))
           when (and path (uiop:absolute-pathname-p path)
                     (uiop:directory-exists-p path))
             append
             (let ((canonical (platform-truename platform path)))
               (unless (or (uiop:subpathp canonical workspace)
                           (some (lambda (root) (uiop:subpathp canonical root)) system-roots))
                 (list canonical))))
     :test #'equal)))

(-> win32--sandbox-environment (pathname) list)
(defun win32--sandbox-environment (temporary)
  "Return a child-only environment with private temporary and user cache locations."
  (let* ((path (uiop:native-namestring temporary))
         (names '("TEMP" "TMP" "TMPDIR" "HOME" "USERPROFILE" "APPDATA" "LOCALAPPDATA")))
    (append
     (mapcar (lambda (name) (format nil "~A=~A" name path)) names)
     (remove-if
      (lambda (binding)
        (let ((equals (position #\= binding)))
          (and equals (member (subseq binding 0 equals) names :test #'string-equal))))
      (sb-ext:posix-environ)))))

(defmethod platform-call-with-command-sandbox ((platform win32-platform) workspace function
                                               &key writable-roots)
  "Run FUNCTION with explicit workspace/tool scopes and a private Windows scratch directory."
  (unless (sandbox-supported-p ':network-isolated)
    (win32--unavailable ':command-sandbox
                        "The native Windows sandbox helper is missing; rebuild or reinstall Autolith."))
  (win32--call-with-sandbox-lock
   (lambda ()
     (let ((temporary (platform-make-temporary-directory
                       platform (uiop:temporary-directory) "autolith-command-")))
       (unwind-protect
            (funcall function
                     (cl-exec-sandbox:appcontainer-sandbox-policy
                      :workspace-roots (append (or writable-roots (list workspace))
                                               (list temporary))
                      :protected-metadata-names (if writable-roots
                                                    '(".agents" ".codex")
                                                    '(".git" ".agents" ".codex"))
                      :read-roots (append (list workspace)
                                          (win32--sandbox-read-roots platform workspace)))
                     (win32--sandbox-environment temporary))
         (platform-delete-directory-tree platform temporary
                                         :validate t :if-does-not-exist ':ignore))))))


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
