(in-package #:autolith-supervisor)

;;;; -- Private Inherited Process Connection --

(sb-alien:define-alien-type supervisor-wide-string
    (sb-alien:c-string :external-format :ucs-2le))

(sb-alien:define-alien-type supervisor-handle (sb-alien:signed 64))

(sb-alien:define-alien-type supervisor-dword (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("GetLastError" supervisor--last-error) supervisor-dword)

(sb-alien:define-alien-routine ("CloseHandle" supervisor--close-handle) sb-alien:int
  (handle supervisor-handle))

(sb-alien:define-alien-routine ("GetCurrentProcess" supervisor--current-process)
    supervisor-handle)

(sb-alien:define-alien-routine ("GetCurrentProcessId" supervisor--current-process-id)
    supervisor-dword)

(sb-alien:define-alien-routine ("GetStdHandle" supervisor--standard-handle)
    supervisor-handle (which supervisor-dword))

(sb-alien:define-alien-routine ("DuplicateHandle" supervisor--duplicate-handle) sb-alien:int
  (source supervisor-handle) (handle supervisor-handle) (target supervisor-handle)
  (copy (* supervisor-handle)) (access supervisor-dword) (inherit sb-alien:int)
  (options supervisor-dword))

(sb-alien:define-alien-routine ("CreateNamedPipeW" supervisor--named-pipe)
    supervisor-handle
  (name supervisor-wide-string) (mode supervisor-dword) (type supervisor-dword)
  (instances supervisor-dword) (output-size supervisor-dword) (input-size supervisor-dword)
  (timeout supervisor-dword) (security (* t)))

(sb-alien:define-alien-routine ("ConnectNamedPipe" supervisor--connect-pipe) sb-alien:int
  (pipe supervisor-handle) (overlapped (* t)))

(sb-alien:define-alien-routine ("DisconnectNamedPipe" supervisor--disconnect-pipe) sb-alien:int
  (pipe supervisor-handle))

(sb-alien:define-alien-routine ("CancelIoEx" supervisor--cancel-pipe-io) sb-alien:int
  (pipe supervisor-handle) (overlapped (* t)))

(sb-alien:define-alien-routine ("CreateFileW" supervisor--open-pipe) supervisor-handle
  (name supervisor-wide-string) (access supervisor-dword) (share supervisor-dword)
  (security (* t)) (disposition supervisor-dword) (flags supervisor-dword)
  (template supervisor-handle))

(sb-alien:define-alien-routine ("CreatePipe" supervisor--diagnostic-pipe) sb-alien:int
  (reader (* supervisor-handle)) (writer (* supervisor-handle)) (security (* t))
  (size supervisor-dword))

(sb-alien:define-alien-routine ("CreateJobObjectW" supervisor--create-job) supervisor-handle
  (security (* t)) (name supervisor-wide-string))

(sb-alien:define-alien-routine ("SetInformationJobObject" supervisor--set-job) sb-alien:int
  (job supervisor-handle) (class sb-alien:int) (information (* t)) (size supervisor-dword))

(sb-alien:define-alien-routine ("QueryInformationJobObject" supervisor--query-job) sb-alien:int
  (job supervisor-handle) (class sb-alien:int) (information (* t)) (size supervisor-dword)
  (returned (* t)))

(sb-alien:define-alien-routine ("TerminateJobObject" supervisor--terminate-job) sb-alien:int
  (job supervisor-handle) (code supervisor-dword))

(sb-alien:define-alien-routine ("AssignProcessToJobObject" supervisor--assign-job) sb-alien:int
  (job supervisor-handle) (process supervisor-handle))

(sb-alien:define-alien-routine ("TerminateProcess" supervisor--terminate-native) sb-alien:int
  (process supervisor-handle) (code supervisor-dword))

(sb-alien:define-alien-routine ("WaitForSingleObject" supervisor--wait-native) supervisor-dword
  (handle supervisor-handle) (milliseconds supervisor-dword))

(sb-alien:define-alien-routine ("GetExitCodeProcess" supervisor--exit-code) sb-alien:int
  (process supervisor-handle) (code (* supervisor-dword)))

(sb-alien:define-alien-routine ("ResumeThread" supervisor--resume-native) supervisor-dword
  (thread supervisor-handle))

(sb-alien:define-alien-routine ("InitializeProcThreadAttributeList" supervisor--initialize-attributes)
    sb-alien:int
  (attributes (* t)) (count supervisor-dword) (flags supervisor-dword)
  (size (* (sb-alien:unsigned 64))))

(sb-alien:define-alien-routine ("UpdateProcThreadAttribute" supervisor--update-attributes)
    sb-alien:int
  (attributes (* t)) (flags supervisor-dword) (attribute (sb-alien:unsigned 64))
  (value (* t)) (size (sb-alien:unsigned 64)) (previous (* t)) (returned (* t)))

(sb-alien:define-alien-routine ("DeleteProcThreadAttributeList" supervisor--delete-attributes)
    sb-alien:void (attributes (* t)))

(sb-alien:define-alien-routine ("CreateProcessW" supervisor--create-native) sb-alien:int
  (application supervisor-wide-string) (command supervisor-wide-string)
  (process-security (* t)) (thread-security (* t)) (inherit sb-alien:int)
  (flags supervisor-dword) (environment (* t)) (directory supervisor-wide-string)
  (startup (* t)) (information (* t)))

(defstruct (supervisor-private-native (:constructor supervisor--make-private-native))
  "Native process and job handles, owned until all members exit."
  process
  job
  exit-code)

(declaim (ftype (function (t) null) supervisor--private-win32-fail))

(defun supervisor--private-win32-fail (operation)
  "Report the current native error without exposing command or environment data."
  (error 'private-process-error :operation operation :detail (supervisor--last-error)))

(declaim (ftype (function (integer boolean) integer) supervisor--private-copy-handle))

(defun supervisor--private-copy-handle (handle inherit-p)
  "Duplicate HANDLE with an explicit inheritance bit."
  (sb-alien:with-alien ((copy supervisor-handle))
    (unless (plusp (supervisor--duplicate-handle
                    (supervisor--current-process) handle (supervisor--current-process)
                    (sb-alien:addr copy) 0 (if inherit-p 1 0) 2))
      (supervisor--private-win32-fail ':duplicate-handle))
    copy))

(declaim (ftype (function (integer &key (:input t) (:output t)) stream)
                supervisor--private-byte-stream))

(defun supervisor--private-byte-stream (handle &key input output)
  "Own a synchronous pipe HANDLE in a binary SBCL stream.
Pinned SBCL 2.6.6 Windows fd-streams use native HANDLE values, not CRT indices."
  (sb-sys:make-fd-stream handle :input input :output output
                               :element-type '(unsigned-byte 8) :dual-channel-p t
                               :buffering ':none :serve-events nil :auto-close t))

(declaim (ftype (function () (values stream integer)) supervisor--private-socketpair))

(defun supervisor--private-socketpair ()
  "Create and connect a local byte-mode duplex named pipe before process launch."
  (let* ((name       (format nil "\\\\.\\pipe\\autolith-parent-~D-~36R"
                             (supervisor--current-process-id)
                             (random (ash 1 128) (make-random-state t))))
         ;; First instance, byte mode, reject remote clients, noninheritable server.
         (server     (supervisor--named-pipe name #x80003 #x8 1 65536 65536 1000 nil))
         (client     nil)
         (complete-p nil))
    (when (= server -1)
      (supervisor--private-win32-fail ':named-pipe))
    (unwind-protect
         (progn
           (setf client (supervisor--open-pipe name #xc0000000 0 nil 3 #x80 0))
           (when (= client -1)
             (setf client nil)
             (supervisor--private-win32-fail ':connect))
           (unless (or (plusp (supervisor--connect-pipe server nil))
                       (= (supervisor--last-error) 535))
             (supervisor--private-win32-fail ':connect))
           (let ((inherited (supervisor--private-copy-handle client t)))
             (unwind-protect
                  (let ((stream (supervisor--private-byte-stream server :input t :output t)))
                    (setf complete-p t)
                    (values stream inherited))
               (unless complete-p (supervisor--close-handle inherited)))))
      (when client (supervisor--close-handle client))
      (unless complete-p (supervisor--close-handle server)))))

(declaim (ftype (function () (values stream integer)) supervisor--private-output-pipe))

(defun supervisor--private-output-pipe ()
  "Return a binary diagnostic reader and an inheritable child writer."
  (sb-alien:with-alien ((reader supervisor-handle) (writer supervisor-handle))
    (unless (plusp (supervisor--diagnostic-pipe
                    (sb-alien:addr reader) (sb-alien:addr writer) nil 65536))
      (supervisor--private-win32-fail ':diagnostics))
    (let ((copy nil) (complete-p nil))
      (unwind-protect
           (progn
             (setf copy (supervisor--private-copy-handle writer t))
             (let ((stream (supervisor--private-byte-stream reader :input t)))
               (setf complete-p t)
               (values stream copy)))
        (supervisor--close-handle writer)
        (unless complete-p
          (supervisor--close-handle reader)
          (when copy (supervisor--close-handle copy)))))))

(declaim (ftype (function (string) string) supervisor--private-quote-argument))

(defun supervisor--private-quote-argument (argument)
  "Quote an argv element for CommandLineToArgvW-compatible child parsing."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (let ((slashes 0))
      (loop for character across argument
            do (if (char= character #\\)
                   (incf slashes)
                   (progn
                     (dotimes (index (if (char= character #\")
                                         (1+ (* 2 slashes)) slashes))
                       (declare (ignorable index))
                       (write-char #\\ stream))
                     (setf slashes 0)
                     (write-char character stream))))
      (dotimes (index (* 2 slashes))
        (declare (ignorable index))
        (write-char #\\ stream)))
    (write-char #\" stream)))

(declaim (ftype (function (list function) t) supervisor--private-with-handle-list))

(defun supervisor--private-with-handle-list (handles function)
  "Call FUNCTION with a startup attribute list containing only three handles."
  (sb-alien:with-alien ((size (sb-alien:unsigned 64) 0)
                        (list (sb-alien:array supervisor-handle 3)))
    (supervisor--initialize-attributes nil 1 0 (sb-alien:addr size))
    (let ((attributes    (sb-alien:make-alien (sb-alien:unsigned 8) size))
          (initialized-p nil))
      (unwind-protect
           (progn
             (unless (plusp (supervisor--initialize-attributes
                             attributes 1 0 (sb-alien:addr size)))
               (supervisor--private-win32-fail ':attributes))
             (setf initialized-p t)
             (loop for handle in handles for index from 0
                   do (setf (sb-alien:deref list index) handle))
             (unless (plusp (supervisor--update-attributes
                             attributes 0 #x20002 (sb-alien:alien-sap list) 24 nil nil))
               (supervisor--private-win32-fail ':attributes))
             (funcall function (sb-alien:alien-sap attributes)))
        (when initialized-p (supervisor--delete-attributes attributes))
        (sb-alien:free-alien attributes)))))

(declaim (ftype (function (list &key (:environment list) (:directory t)
                           (:handles list) (:job integer)) supervisor-private-native)
                supervisor--private-create-process))

(defun supervisor--private-create-process (command &key environment directory handles job)
  "Create suspended, assign the job before any child code runs, then resume."
  (let* ((command-line (format nil "~{~A~^ ~}"
                               (mapcar #'supervisor--private-quote-argument command)))
         (environment-block
           (sb-ext:string-to-octets
            (format nil "~{~A~C~}~C"
                    (loop for entry in (sort (copy-list environment) #'string-lessp)
                          append (list entry #\Null))
                    #\Null)
            :external-format ':ucs-2le))
         (process nil) (thread nil) (complete-p nil))
    (unwind-protect
         (supervisor--private-with-handle-list
          handles
          (lambda (attributes)
            (sb-alien:with-alien ((startup (sb-alien:array (sb-alien:unsigned 8) 112))
                                  (information (sb-alien:array (sb-alien:unsigned 8) 24)))
              (let ((startup-sap     (sb-alien:alien-sap startup))
                    (information-sap (sb-alien:alien-sap information)))
                (dotimes (index 112) (setf (sb-sys:sap-ref-8 startup-sap index) 0))
                (dotimes (index 24) (setf (sb-sys:sap-ref-8 information-sap index) 0))
                (setf (sb-sys:sap-ref-32 startup-sap 0) 112
                      (sb-sys:sap-ref-32 startup-sap 60) #x100
                      (sb-sys:sap-ref-64 startup-sap 80) (first handles)
                      (sb-sys:sap-ref-64 startup-sap 88) (second handles)
                      (sb-sys:sap-ref-64 startup-sap 96) (third handles)
                      (sb-sys:sap-ref-sap startup-sap 104) attributes)
                (sb-sys:with-pinned-objects (environment-block)
                  (unless (plusp (supervisor--create-native
                                  (first command) command-line nil nil 1 #x8080404
                                  (sb-sys:vector-sap environment-block)
                                  (and directory (uiop:native-namestring directory))
                                  startup-sap information-sap))
                    (supervisor--private-win32-fail ':launch)))
                (setf process (sb-sys:sap-ref-64 information-sap 0)
                      thread (sb-sys:sap-ref-64 information-sap 8))
                (unless (plusp (supervisor--assign-job job process))
                  (supervisor--private-win32-fail ':job-assignment))
                (when (= (supervisor--resume-native thread) #xffffffff)
                  (supervisor--private-win32-fail ':resume))
                (setf complete-p t)
                (supervisor--make-private-native :process process :job job)))))
      (when thread (supervisor--close-handle thread))
      (when (and process (not complete-p))
        (supervisor--terminate-native process 1)
        (unless (zerop (supervisor--wait-native process 5000))
          (supervisor--private-win32-fail ':cleanup))
        (supervisor--close-handle process)))))

(declaim (ftype (function (list list t) (values t stream list))
                supervisor--private-launch))

(defun supervisor--private-launch (command environment directory)
  "Launch an owned Windows job with three explicit inherited standard handles."
  (let ((job (supervisor--create-job nil nil))
        (connection nil) (input nil) (output nil) (errors nil)
        (output-reader nil) (error-reader nil) (complete-p nil))
    (when (zerop job)
      (supervisor--private-win32-fail ':job))
    (unwind-protect
         (progn
           (sb-alien:with-alien ((limits (sb-alien:array (sb-alien:unsigned 8) 144)))
             (let ((sap (sb-alien:alien-sap limits)))
               (dotimes (index 144) (setf (sb-sys:sap-ref-8 sap index) 0))
               ;; JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE covers abandoned ownership.
               (setf (sb-sys:sap-ref-32 sap 16) #x2000)
               (unless (plusp (supervisor--set-job job 9 sap 144))
                 (supervisor--private-win32-fail ':job))))
           (multiple-value-setq (connection input) (supervisor--private-socketpair))
           (multiple-value-setq (output-reader output) (supervisor--private-output-pipe))
           (multiple-value-setq (error-reader errors) (supervisor--private-output-pipe))
           (let ((native (supervisor--private-create-process
                          command :environment environment :directory directory
                          :handles (list input output errors) :job job)))
             (setf complete-p t)
             (values native connection (list output-reader error-reader))))
      (dolist (handle (list input output errors))
        (when handle (supervisor--close-handle handle)))
      (unless complete-p
        (dolist (stream (list connection output-reader error-reader))
          (when stream (close stream :abort t)))
        (supervisor--close-handle job)))))

(declaim (ftype (function (t) (values boolean t)) supervisor--private-status))

(defun supervisor--private-status (native)
  "Return direct child exit state and the native exit code."
  (if (supervisor-private-native-exit-code native)
      (values t (supervisor-private-native-exit-code native))
      (let ((status (supervisor--wait-native (supervisor-private-native-process native) 0)))
        (case status
          (0
           (sb-alien:with-alien ((code supervisor-dword))
             (unless (plusp (supervisor--exit-code
                             (supervisor-private-native-process native) (sb-alien:addr code)))
               (supervisor--private-win32-fail ':status))
             (setf (supervisor-private-native-exit-code native) code)
             (values t code)))
          (258
           (values nil nil))
          (otherwise
           (supervisor--private-win32-fail ':status))))))

(declaim (ftype (function (t stream) null) supervisor--private-disconnect))

(defun supervisor--private-disconnect (native stream)
  "Disconnect the server pipe to release pending protocol I/O, then close it."
  (declare (ignore native))
  (when (open-stream-p stream)
    (supervisor--cancel-pipe-io (sb-sys:fd-stream-fd stream) nil)
    (supervisor--disconnect-pipe (sb-sys:fd-stream-fd stream))
    (close stream :abort t))
  nil)

(declaim (ftype (function (t) null) supervisor--private-stop))

(defun supervisor--private-stop (native)
  "Terminate all members of the owned job, even after the root exits."
  (unless (plusp (supervisor--terminate-job (supervisor-private-native-job native) 1))
    (supervisor--private-win32-fail ':terminate))
  nil)

(declaim (ftype (function (t real) boolean) supervisor--private-reap))

(defun supervisor--private-reap (native deadline)
  "Confirm root termination and zero active job members before the deadline."
  (loop
    (when (supervisor--private-status native)
      (sb-alien:with-alien ((accounting (sb-alien:array (sb-alien:unsigned 8) 48)))
        (unless (plusp (supervisor--query-job
                        (supervisor-private-native-job native) 1
                        (sb-alien:alien-sap accounting) 48 nil))
          (supervisor--private-win32-fail ':job-state))
        (when (zerop (sb-sys:sap-ref-32 (sb-alien:alien-sap accounting) 40))
          (return t))))
    (when (>= (/ (get-internal-real-time) internal-time-units-per-second) deadline)
      (return nil))
    (sleep 0.01)))

(declaim (ftype (function (t) list) supervisor--private-scope))

(defun supervisor--private-scope (native)
  "Identify the Windows job whose active member count reached zero."
  (declare (ignore native))
  (list :kind ':windows-job))

(declaim (ftype (function (t) null) supervisor--private-release))

(defun supervisor--private-release (native)
  "Close the handles of a confirmed empty job."
  (supervisor--close-handle (supervisor-private-native-process native))
  (supervisor--close-handle (supervisor-private-native-job native))
  nil)

(declaim (ftype (function () stream) supervisor--private-child-connection))

(defun supervisor--private-child-connection ()
  "Open a binary, noninheritable copy of the duplex stdin HANDLE."
  (supervisor--private-byte-stream
   (supervisor--private-copy-handle (supervisor--standard-handle #xfffffff6) nil)
   :input t :output t))
