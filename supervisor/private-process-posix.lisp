(in-package #:autolith-supervisor)

;;;; -- Private Inherited Process Connection --

(sb-alien:define-alien-routine ("socketpair" supervisor--socketpair) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int)
  (descriptors (* sb-alien:int)))

;; FCNTL's third argument is variadic. Darwin arm64 passes it on the stack.
(sb-alien:define-alien-routine ("fcntl" supervisor--descriptor-flags) sb-alien:int
  (descriptor sb-alien:int) (command sb-alien:int) &optional (flags sb-alien:int))

(sb-alien:define-alien-routine ("shutdown" supervisor--socket-shutdown) sb-alien:int
  (descriptor sb-alien:int) (how sb-alien:int))

(declaim (ftype (function (integer &key (:input t) (:output t)) stream)
                supervisor--private-byte-stream))

(defun supervisor--private-byte-stream (descriptor &key input output)
  "Own DESCRIPTOR in an unbuffered binary stream with explicit directions."
  (sb-sys:make-fd-stream descriptor :input input :output output
                                  :element-type '(unsigned-byte 8)
                                  :dual-channel-p t :serve-events nil
                                  :buffering ':none :auto-close t))

(declaim (ftype (function (stream) stream) supervisor--private-diagnostic-stream))

(defun supervisor--private-diagnostic-stream (stream)
  "Replace a RUN-PROGRAM diagnostic stream with an owned binary duplicate."
  (multiple-value-bind (descriptor error-number)
      (sb-unix:unix-dup (sb-sys:fd-stream-fd stream))
    (unless descriptor
      (error 'private-process-error :operation ':diagnostics :detail error-number))
    (unwind-protect
         (progn
           (when (minusp (supervisor--descriptor-flags descriptor 2 1))
             (error 'private-process-error :operation ':diagnostics
                    :detail                           "Cannot set close-on-exec."))
           (prog1 (supervisor--private-byte-stream descriptor :input t)
             (setf descriptor nil)))
      (when descriptor (sb-unix:unix-close descriptor))
      (close stream))))

(declaim (ftype (function (list list t) (values t stream list))
                supervisor--private-launch))

(defun supervisor--private-launch (command environment directory)
  "Launch with a socketpair on stdin and two separate diagnostic pipes.
SBCL spawn closes unrelated descriptors and establishes a fresh process group."
  (let ((parent nil) (child nil) (process nil) (diagnostics nil) (complete-p nil))
    (unwind-protect
         (progn
           (sb-alien:with-alien ((descriptors (sb-alien:array sb-alien:int 2)))
             ;; AF_UNIX and SOCK_STREAM are both 1 on supported POSIX hosts.
             (unless (zerop (supervisor--socketpair
                             1 1 0 (sb-alien:cast descriptors (* sb-alien:int))))
               (error 'private-process-error :operation ':socketpair
                      :detail                           "Cannot create the private connection."))
             (let ((first  (sb-alien:deref descriptors 0))
                   (second (sb-alien:deref descriptors 1)))
               (unwind-protect
                    (progn
                      (unless (and (zerop (supervisor--descriptor-flags first 2 1))
                                   (zerop (supervisor--descriptor-flags second 2 1)))
                        (error 'private-process-error :operation ':socketpair
                               :detail                           "Cannot set close-on-exec."))
                      (setf parent (supervisor--private-byte-stream first :input t :output t)
                            first nil
                            child (supervisor--private-byte-stream second :input t :output t)
                            second nil))
                 (when first (sb-unix:unix-close first))
                 (when second (sb-unix:unix-close second)))))
           (setf process (sb-ext:run-program
                          (first command) (rest command)
                          :environment environment :directory directory
                          :wait nil :search nil :preserve-fds nil
                          :input child :output ':stream :error ':stream))
           (close child)
           (setf child nil)
           (push (supervisor--private-diagnostic-stream (sb-ext:process-output process))
                 diagnostics)
           (push (supervisor--private-diagnostic-stream (sb-ext:process-error process))
                 diagnostics)
           (setf complete-p t)
           (values process parent (nreverse diagnostics)))
      (when child (close child))
      (unless complete-p
        (when parent (close parent))
        (dolist (stream diagnostics) (close stream))
        (when process
          (supervisor--private-stop process)
          (unless (supervisor--private-reap process
                                           (+ (get-internal-real-time)
                                              (* 5 internal-time-units-per-second))
                                           :ticks-p t)
            (error 'private-process-error :operation ':cleanup
                   :detail                           "Failed launch left a live process group."))
          (sb-ext:process-close process))))))

(declaim (ftype (function (t) (values boolean t)) supervisor--private-status))

(defun supervisor--private-status (process)
  "Return whether the direct child exited, and its exit status or signal."
  (let ((exited-p (not (null (member (sb-ext:process-status process)
                                    '(:exited :signaled))))))
    (values exited-p (and exited-p (sb-ext:process-exit-code process)))))

(declaim (ftype (function (t stream) null) supervisor--private-disconnect))

(defun supervisor--private-disconnect (process stream)
  "Wake protocol readers, then close the owned socket endpoint."
  (declare (ignore process))
  (when (open-stream-p stream)
    (supervisor--socket-shutdown (sb-sys:fd-stream-fd stream) 2)
    (close stream :abort t))
  nil)

(declaim (ftype (function (t) null) supervisor--private-stop))

(defun supervisor--private-stop (process)
  "Stop the child process group, including descendants after direct child exit."
  (sb-ext:process-kill process sb-unix:sigkill ':process-group)
  nil)

(declaim (ftype (function (t real &key (:ticks-p t)) boolean)
                supervisor--private-reap))

(defun supervisor--private-reap (process deadline &key ticks-p)
  "Reap the direct child and confirm that its process group no longer exists."
  (let ((limit (if ticks-p (/ deadline internal-time-units-per-second) deadline)))
    (loop
      (let ((exited-p (supervisor--private-status process)))
        (let ((result (sb-unix:unix-killpg (sb-ext:process-pid process) 0)))
          ;; ESRCH is 3 on the supported POSIX hosts.
          (when (and exited-p (= result -1) (= (sb-unix::get-errno) 3))
            (return t))))
      (when (>= (/ (get-internal-real-time) internal-time-units-per-second) limit)
        (return nil))
      (sleep 0.01))))

(declaim (ftype (function (t) list) supervisor--private-scope))

(defun supervisor--private-scope (process)
  "Identify the native scope proved empty, without claiming escaped groups."
  (list :kind ':posix-process-group :id (sb-ext:process-pid process)))

(declaim (ftype (function (t) null) supervisor--private-release))

(defun supervisor--private-release (process)
  "Release SBCL's already reaped process descriptors."
  (sb-ext:process-close process)
  nil)

(declaim (ftype (function () stream) supervisor--private-child-connection))

(defun supervisor--private-child-connection ()
  "Open a private binary duplicate of the inherited duplex stdin socket."
  (multiple-value-bind (descriptor error-number) (sb-unix:unix-dup 0)
    (unless descriptor
      (error 'private-process-error :operation ':connection :detail error-number))
    (unless (zerop (supervisor--descriptor-flags descriptor 2 1))
      (sb-unix:unix-close descriptor)
      (error 'private-process-error :operation ':connection
             :detail                           "Cannot set close-on-exec."))
    (supervisor--private-byte-stream descriptor :input t :output t)))
