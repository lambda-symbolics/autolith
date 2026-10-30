;;;; -- Private Agent Process Connection --

(in-package #:autolith-supervisor)

(define-condition private-process-error (error)
  ((operation :initarg       :operation
              :reader        private-process-error-operation
              :documentation "The failed launch or cleanup operation.")
   (detail    :initarg       :detail
              :reader        private-process-error-detail
              :documentation "A bounded description of the failure."))
  (:documentation "A private child connection could not start or stop safely.")
  (:report (lambda (condition stream)
             (format stream "Private process ~a: ~a"
                     (private-process-error-operation condition)
                     (private-process-error-detail condition)))))

(defstruct (private-process-diagnostic
             (:constructor private-process--make-diagnostic))
  "A byte-bounded diagnostic tail, drained independently of protocol traffic."
  (bytes (make-array 0 :element-type '(unsigned-byte 8)))
  (count 0)
  (lock (sb-thread:make-mutex :name "private child diagnostics"))
  stream
  thread)

(defstruct (private-process (:constructor private-process--make))
  "An owned native child scope and its private, duplex byte connection."
  native
  connection
  instance-id
  captures
  (cleanup-timeout 5)
  (cleanup-lock (sb-thread:make-mutex :name "private child cleanup"))
  evidence)

(declaim (ftype (function (t t) null) private-process--fail))

(defun private-process--fail (operation detail)
  "Signal a structured private process failure."
  (error 'private-process-error :operation operation :detail detail))

(declaim (ftype (function () real) private-process--now))

(defun private-process--now ()
  "Return monotonic seconds for bounded process operations."
  (/ (get-internal-real-time) internal-time-units-per-second))

(declaim (ftype (function (list string) list) private-process--environment))

(defun private-process--environment (environment instance-id)
  "Validate an explicit environment and add nonsecret startup identity."
  (unless (and (stringp instance-id) (<= 1 (length instance-id) 256)
               (every (lambda (character) (<= 33 (char-code character) 126))
                      instance-id))
    (private-process--fail ':environment "Invalid instance ID."))
  (let ((names (make-hash-table :test 'equal)))
    (dolist (entry environment)
      (let ((separator (and (stringp entry) (position #\= entry))))
        (unless (and separator (plusp separator)
                     (not (find #\Null entry))
                     (every (lambda (character)
                              (or (alphanumericp character) (char= character #\_)))
                            (subseq entry 0 separator)))
          (private-process--fail ':environment "Invalid environment entry."))
        (let ((name (string-upcase (subseq entry 0 separator))))
          (when (or (gethash name names)
                    (equal name "AUTOLITH_PARENT_INSTANCE_ID"))
            (private-process--fail ':environment "Duplicate or reserved environment name."))
          (setf (gethash name names) t)))))
  (append (copy-list environment)
          (list (concatenate 'string "AUTOLITH_PARENT_INSTANCE_ID=" instance-id))))

(declaim (ftype (function (stream integer) private-process-diagnostic)
                private-process--capture))

(defun private-process--capture (stream limit)
  "Drain STREAM into a fixed byte ring without retaining unbounded output."
  (let ((capture (private-process--make-diagnostic
                  :bytes  (make-array limit :element-type '(unsigned-byte 8))
                  :stream stream)))
    (setf (private-process-diagnostic-thread capture)
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (loop for byte = (read-byte stream nil nil)
                        while byte
                        do (sb-thread:with-mutex
                               ((private-process-diagnostic-lock capture))
                             (setf (aref (private-process-diagnostic-bytes capture)
                                         (mod (private-process-diagnostic-count capture)
                                              limit))
                                   byte)
                             (incf (private-process-diagnostic-count capture))))
               (close stream)))
           :name "private child diagnostic reader"))
    capture))

(declaim (ftype (function (private-process) list) private-process-diagnostics))

(defun private-process-diagnostics (process)
  "Return bounded stdout/stderr tails with terminal controls replaced.
Each entry has :TEXT, :TOTAL-BYTES and :DROPPED-BYTES. Only ASCII printing
characters, newline and tab reach the caller; other bytes become question marks."
  (loop for capture in (private-process-captures process)
        collect (sb-thread:with-mutex ((private-process-diagnostic-lock capture))
                  (let* ((bytes (private-process-diagnostic-bytes capture))
                         (total (private-process-diagnostic-count capture))
                         (size  (min total (length bytes)))
                         (start (- total size)))
                    (list :text
                          (with-output-to-string (stream)
                            (dotimes (index size)
                              (let ((byte (aref bytes (mod (+ start index)
                                                          (length bytes)))))
                                (write-char (if (or (<= 32 byte 126)
                                                    (= byte 10) (= byte 9))
                                                (code-char byte)
                                                #\?)
                                            stream))))
                          :total-bytes total :dropped-bytes (- total size))))))

(declaim (ftype (function (list &key (:instance-id string) (:environment list)
                           (:directory t) (:startup t) (:startup-timeout real)
                           (:cleanup-timeout real) (:diagnostic-limit integer))
                         private-process)
                private-process-open))

(defun private-process-open (command &key instance-id
                                      (environment nil environment-p)
                                      directory startup (startup-timeout 10)
                                      (cleanup-timeout 5) (diagnostic-limit 65536))
  "Launch COMMAND with a private inherited connection and explicit ENVIRONMENT.
STARTUP, when supplied, receives the process under STARTUP-TIMEOUT. Use it for
Hello/Welcome/Ready negotiation. A failed startup stops the owned native scope.
Stdout and stderr are diagnostic streams, never protocol streams. This API does
not choose an agent image, supply credentials, or change the default launcher."
  (supervisor--validate-argv command 'command)
  (unless (and environment-p (listp environment)
               (integerp (handler-case (list-length environment) (type-error () nil)))
               (plusp startup-timeout) (plusp cleanup-timeout)
               (<= 1 diagnostic-limit 1048576))
    (private-process--fail ':launch "Supply an explicit environment and positive bounds."))
  (let ((sanitized  (private-process--environment environment instance-id))
        (process    nil)
        (complete-p nil))
    (unwind-protect
         (progn
           ;; Save ownership before an interrupt can escape the native launch.
           (sb-sys:without-interrupts
             (multiple-value-bind (native connection diagnostics)
                 (supervisor--private-launch command sanitized directory)
               (setf process (private-process--make
                              :native native :connection connection
                              :instance-id instance-id :cleanup-timeout cleanup-timeout))
               (dolist (stream diagnostics)
                 (push (private-process--capture stream diagnostic-limit)
                       (private-process-captures process)))
               (setf (private-process-captures process)
                     (nreverse (private-process-captures process)))))
           (when startup
             (sb-ext:with-timeout startup-timeout
               (funcall startup process)))
           (setf complete-p t)
           process)
      (when (and process (not complete-p))
        (private-process-close process)))))

(declaim (ftype (function (private-process) list) private-process-close))

(defun private-process-close (process)
  "Close the connection, stop the native child scope, and return cleanup evidence.
:NATIVE-SCOPE-CLEAN-P proves that the reported :NATIVE-SCOPE is empty and both
diagnostic readers have joined. The POSIX scope is one process group; Windows
uses a job. Callers must track children that deliberately leave that scope.
Failure to finish before the configured deadline signals PRIVATE-PROCESS-ERROR.
Concurrent and repeated calls serialize cleanup and return the same evidence."
  (sb-thread:with-mutex ((private-process-cleanup-lock process))
    (or (private-process-evidence process)
        (let* ((deadline (+ (private-process--now)
                            (private-process-cleanup-timeout process)))
               (native   (private-process-native process)))
          (supervisor--private-disconnect native (private-process-connection process))
          (supervisor--private-stop native)
          (unless (supervisor--private-reap native deadline)
            (private-process--fail ':cleanup "Native child scope did not stop before the deadline."))
          (dolist (capture (private-process-captures process))
            (multiple-value-bind (result reason)
                (sb-thread:join-thread
                 (private-process-diagnostic-thread capture)
                 :timeout (max 0 (- deadline (private-process--now)))
                 :default ':reader-failed)
              (declare (ignore result))
              (when reason
                (private-process--fail ':cleanup "Diagnostic reader did not finish."))))
          (multiple-value-bind (exited-p exit-code)
              (supervisor--private-status native)
            (declare (ignore exited-p))
            (let ((scope (supervisor--private-scope native)))
              (supervisor--private-release native)
              (setf (private-process-evidence process)
                    (list :native-scope-clean-p t :native-scope scope
                          :exit-code   exit-code
                          :diagnostics (private-process-diagnostics process)))))))))

(declaim (ftype (function (private-process) boolean) private-process--finished-p))

(defun private-process--finished-p (process)
  "Check exit state without racing the release of native process handles."
  (sb-thread:with-mutex ((private-process-cleanup-lock process))
    (not (null (or (private-process-evidence process)
                   (supervisor--private-status (private-process-native process)))))))

(declaim (ftype (function (private-process &key (:timeout real)) list)
                private-process-await))

(defun private-process-await (process &key (timeout 10))
  "Wait at most TIMEOUT seconds, then close and reap the owned native child scope.
Return evidence with :TIMED-OUT indicating whether the direct child exceeded the
wait. Stop scope members even after a normal direct exit."
  (unless (plusp timeout)
    (private-process--fail ':await "Timeout must be positive."))
  (let ((deadline (+ (private-process--now) timeout)))
    (loop until (private-process--finished-p process)
          do (when (>= (private-process--now) deadline)
               (return-from private-process-await
                 (append (private-process-close process) (list :timed-out t))))
             (sleep 0.01))
    (append (private-process-close process) (list :timed-out nil))))

(declaim (ftype (function () stream) private-process-child-connection))

(defun private-process-child-connection ()
  "Open the inherited duplex standard-input endpoint as a binary stream.
Stdout and stderr keep their diagnostic role. Read startup identity from
AUTOLITH_PARENT_INSTANCE_ID, never from a provider credential environment."
  (supervisor--private-child-connection))
