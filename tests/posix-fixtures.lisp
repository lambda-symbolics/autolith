(in-package #:autolith)

;;;; -- POSIX Host Fixtures --

(defmethod test-fixture-available-p ((platform posix-platform) fixture)
  "Expose POSIX host facilities. Permission enforcement has a separate probe."
  (declare (ignore platform))
  (check-type fixture test-fixture-kind)
  t)

(defmethod test-fixture-available-p ((platform posix-platform) (fixture (eql :permission-denials)))
  "Check whether this process is denied access to a mode-000 file."
  (let* ((root (test-make-temporary-root))
         (file (merge-pathnames "permission-probe" root)))
    (unwind-protect
         (progn
           (with-open-file (stream file :direction ':output :if-exists ':error))
           (test-fixture-set-file-mode platform file 0)
           (handler-case
               (with-open-file (stream file)
                 nil)
             (file-error () t)))
      (when (probe-file file)
        (test-fixture-set-file-mode platform file #o600))
      (platform-delete-directory-tree platform root :validate t :if-does-not-exist ':ignore))))


;;;; -- Files --

(defmethod test-fixture-make-symbolic-link ((platform posix-platform)
                                            target link)
  "Create LINK with symlink(2)."
  (declare (ignore platform))
  (sb-posix:symlink target link)
  nil)

(defmethod test-fixture-remove-link ((platform posix-platform) link)
  "Unlink LINK itself."
  (declare (ignore platform))
  (sb-posix:unlink link)
  nil)

(defmethod test-fixture-make-fifo ((platform posix-platform) pathname)
  "Create PATHNAME with mkfifo(3) and mode #o600."
  (declare (ignore platform))
  (sb-posix:mkfifo (namestring pathname) #o600)
  nil)

(defmethod test-fixture-device-node ((platform posix-platform))
  "The null device is a character device on every POSIX host."
  (declare (ignore platform))
  #P"/dev/null")

(defmethod test-fixture-file-mode ((platform posix-platform) pathname)
  "Return PATHNAME's access and special permission bits from stat(2)."
  (declare (ignore platform))
  (logand (sb-posix:stat-mode (sb-posix:stat (namestring pathname))) #o7777))

(defmethod test-fixture-set-file-mode ((platform posix-platform) pathname mode)
  "Set PATHNAME's permission bits with chmod(2)."
  (declare (ignore platform))
  (sb-posix:chmod (namestring pathname) mode)
  nil)

(defmethod test-fixture-permissions-p ((platform posix-platform)
                                       pathname permissions)
  "Compare PATHNAME's permission bits with the exact mode PERMISSIONS names."
  (= (test-fixture-file-mode platform pathname)
     (ecase permissions
       (:private-file
        #o600)
       (:private-directory
        #o700)
       (:read-only
        #o444))))


;;;; -- Descriptors and Processes --

(-> posix-fixture--descriptor-stream (integer (member :input :output)) stream)
(defun posix-fixture--descriptor-stream (descriptor direction)
  "Return an unbuffered character stream over DESCRIPTOR in DIRECTION."
  (sb-sys:make-fd-stream descriptor
                         :input           (eq direction ':input)
                         :output          (eq direction ':output)
                         :element-type    'character
                         :external-format ':utf-8
                         :buffering       ':none
                         :auto-close      nil))

(defmethod test-fixture-call-with-descriptor-input ((platform posix-platform)
                                                    content function)
  "Feed CONTENT through a pipe whose read end backs the stream FUNCTION receives."
  (declare (ignore platform))
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input  nil)
          (output nil))
      (unwind-protect
           (progn
             (setf output
                   (posix-fixture--descriptor-stream write-descriptor ':output))
             (write-string content output)
             (finish-output output)
             (close output)
             (setf input
                   (posix-fixture--descriptor-stream read-descriptor ':input))
             (funcall function input))
        (if input
            (close input)
            (sb-posix:close read-descriptor))
        (if output
            (close output)
            (sb-posix:close write-descriptor))))))

(-> posix-fixture--read-byte (integer) integer)
(defun posix-fixture--read-byte (descriptor)
  "Read one synchronization byte from DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type    '(unsigned-byte 8)
           :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:read descriptor (sb-sys:vector-sap buffer) 1))))

(-> posix-fixture--write-byte (integer) integer)
(defun posix-fixture--write-byte (descriptor)
  "Write one synchronization byte to DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type    '(unsigned-byte 8)
           :initial-element 1)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:write descriptor (sb-sys:vector-sap buffer) 1))))

#+linux
(-> posix-fixture--output-poll (integer) integer)
#+linux
(defun posix-fixture--output-poll (descriptor)
  "Return raw output poll flags without SBCL's readiness filtering."
  (sb-alien:with-alien ((poll (sb-alien:struct sb-unix::pollfd)))
    (setf (sb-alien:slot poll 'sb-unix::fd) descriptor
          (sb-alien:slot poll 'sb-unix::events) sb-unix:pollout
          (sb-alien:slot poll 'sb-unix::revents) 0)
    (sb-unix::unix-poll (sb-alien:addr poll) 1 0)
    (sb-alien:slot poll 'sb-unix::revents)))

#+linux
(-> posix-fixture--supervised-request-deadline (real) list)
#+linux
(defun posix-fixture--supervised-request-deadline (delay)
  "Collect causal checks inside a disposable process, including failed cleanup."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((script      (merge-pathnames "prefix-child.lisp" root))
           (ready       (merge-pathnames "prefix.ready" root))
           (record      (supervised-process-create))
           (make-thread (symbol-function 'bt:make-thread))
           (finished    (bt:make-semaphore))
           (budget      1.0)
           (result      ':unset)
           (owner-error nil)
           (owner       nil)
           (reaped      nil)
           (unavailable nil)
           (raw-poll    nil)
           (filtered    ':unset)
           (complete-p  nil)
           (started     nil)
           (returned    nil))
      (with-open-file (stream script :direction ':output :if-exists ':error)
        (write-string
         "(let ((bytes (make-array 4196 :element-type '(unsigned-byte 8))) (total 0))
  (sb-sys:with-pinned-objects (bytes)
    (loop while (< total 4196) do
      (multiple-value-bind (count errno)
          (sb-unix:unix-read 0 (sb-sys:sap+ (sb-sys:vector-sap bytes) total) (- 4196 total))
        (cond ((and count (plusp count)) (incf total count))
              ((eql errno sb-unix:eintr) nil)
              (t (error \"prefix read failed\"))))))
  (with-open-file (stream (merge-pathnames \"prefix.ready\" *load-truename*)
                          :direction :output :if-exists :error)
    (write total :stream stream)))
(sleep 30)
" stream))
      (test-call-with-function-replacements
       (list (list 'bt:make-thread
                   (lambda (function &rest options)
                     (apply make-thread
                            (if (equal (getf options :name) "supervised subprocess I/O")
                                (lambda () (sleep delay) (funcall function))
                                function)
                            options))))
       (lambda ()
         (setf started (get-internal-real-time)
               owner
               (funcall make-thread
                        (lambda ()
                          (unwind-protect
                               (handler-case
                                   (setf result
                                         (supervised-process-request
                                          record (make-string 1048576 :initial-element #\x)
                                          :seconds  budget
                                          :start
                                          (lambda (install)
                                            (let* ((process
                                                     (uiop:launch-program
                                                      (list (namestring sb-ext:*runtime-pathname*)
                                                            "--noinform" "--no-sysinit" "--no-userinit"
                                                            "--script" (namestring script))
                                                      :input ':stream :output ':stream :error-output ':stream))
                                                   (descriptor
                                                     (sb-sys:fd-stream-fd (uiop:process-info-input process)))
                                                   (capacity (sb-posix:fcntl descriptor 1032)))
                                              (format t "~&PIPE capacity-before=~D~%" capacity)
                                              ;; F_GETPIPE_SZ is 1032; F_SETPIPE_SZ is 1031 on Linux.
                                              (unless (= capacity 8192)
                                                (handler-case (sb-posix:fcntl descriptor 1031 8192)
                                                  (sb-posix:syscall-error (condition)
                                                    (setf unavailable
                                                          (format nil "F_SETPIPE_SZ 8192: ~A (errno ~D)"
                                                                  condition (sb-posix:syscall-errno condition)))
                                                    (supervised-process-terminate process)
                                                    (supervised-process--close-streams process)
                                                    (error "~A" unavailable))))
                                              (setf capacity (sb-posix:fcntl descriptor 1032))
                                              (format t "~&PIPE capacity-after=~D~%" capacity)
                                              (unless (= capacity 8192)
                                                (supervised-process-terminate process)
                                                (supervised-process--close-streams process)
                                                (error "Expected 8192-byte pipe, got ~D." capacity))
                                              (funcall install process)))
                                          :active-p (lambda () (not (probe-file ready)))
                                          :stop
                                          (lambda (operation)
                                            (supervised-process-stop operation)
                                            (when (supervised-process-process operation)
                                              (let* ((process (supervised-process-process operation))
                                                     (descriptor
                                                       (sb-sys:fd-stream-fd (uiop:process-info-input process))))
                                                (setf reaped (uiop:wait-process process)
                                                      raw-poll (posix-fixture--output-poll descriptor)
                                                      filtered (sb-unix:unix-simple-poll descriptor :output 0)))))
                                          :complete
                                          (lambda (operation)
                                            (setf complete-p
                                                  (and (supervised-process-thread operation)
                                                       (not (bt:thread-alive-p
                                                             (supervised-process-thread operation)))
                                                       (not (open-stream-p
                                                             (uiop:process-info-input
                                                              (supervised-process-process operation)))))))))
                                 (error (condition) (setf owner-error condition)))
                            (setf returned (get-internal-real-time))
                            (bt:signal-semaphore finished)))
                        :name "test supervised request owner"))
         ;; A fresh budget after delayed scheduling would end near 1.5s, not 1.0s.
         (let ((finished-p (bt:wait-on-semaphore finished :timeout (+ budget 0.25))))
           (when finished-p (bt:join-thread owner))
           (format t "~&PIPE delay=~F reaped=~S raw=~S filtered=~S returned=~S error=~S~%"
                  delay reaped raw-poll filtered
                  (and returned (/ (- returned started) internal-time-units-per-second)) owner-error)
           (when unavailable
             (return-from posix-fixture--supervised-request-deadline
               (list :unavailable unavailable)))
           (when owner-error (error owner-error))
           (list :checks
                  (list (null owner-error) "The 8192-byte pipe setup and request must execute.")
                 (list (not (null (probe-file ready))) "The child consumed exactly 4196 raw bytes.")
                 (list (eql reaped 137) "Stop killed and reaped the fixture child.")
                 (list (eql raw-poll 8) "The readerless pipe reports raw ERR alone.")
                 (list (null filtered) "SBCL filters ERR-only output readiness to NIL.")
                 (list (not (null finished-p)) "The original owner returns within its original request budget.")
                 (list (null result) "The timed out exchange returns NIL.")
                 (list (not (null complete-p)) "Original cleanup joins I/O and abort-closes the input stream.")
                 (list (not (bt:thread-alive-p owner)) "The request owner has exited.")
                 (list (not (bt:thread-alive-p (supervised-process-thread record)))
                       "The actual exchange I/O thread has exited."))))))))

#-linux
(defmethod test-fixture-supervised-request-deadline ((platform posix-platform) delay)
  "Withhold the Linux pipe-sizing fixture on other POSIX hosts."
  (declare (ignore platform delay))
  (test-withheld ':posix-adapter "Linux supervised request pipe sizing"))

#+linux
(defmethod test-fixture-supervised-request-deadline ((platform posix-platform) delay)
  "Replay checks from a bounded process, or explicitly withhold unavailable setup."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((result-file (merge-pathnames "causal-result.sexp" root))
           (pid         (progn (finish-output) (sb-posix:fork)))
           (reaped      nil)
           (status      nil))
      (when (zerop pid)
        (sb-posix:setpgid 0 0)
        (let ((result
                (handler-case (posix-fixture--supervised-request-deadline delay)
                  (error (condition) (list :error (princ-to-string condition))))))
          (with-open-file (stream result-file :direction ':output :if-exists ':error)
            (let ((*print-readably* t)) (write result :stream stream)))
          (finish-output)
          ;; Do not unwind a failed owner's join or keep its spinning I/O thread.
          (sb-posix:_exit 0)))
      (unwind-protect
           (progn
             (loop with deadline = (+ (get-internal-real-time)
                                     (* 4 internal-time-units-per-second))
                   do (multiple-value-bind (waited code)
                          (sb-posix:waitpid pid sb-posix:wnohang)
                        (when (= waited pid)
                          (setf reaped t status code)
                          (return)))
                   when (>= (get-internal-real-time) deadline) do (return)
                   do (sleep 0.01))
             (unless (and reaped (sb-posix:wifexited status)
                          (zerop (sb-posix:wexitstatus status)))
              (error "Supervised request fixture did not exit normally within four seconds."))
             (let* ((*read-eval* nil)
                    (result      (with-open-file (stream result-file) (read stream))))
               (ecase (first result)
                 (:error
                  (error "Supervised request fixture error: ~A" (second result)))
                 (:unavailable
                  (let ((description (second result)))
                    (format t "~&UNAVAILABLE ~A~%" description)
                    (test-withheld ':pipe-resize description)))
                 (:checks
                  (dolist (check (rest result))
                    (test-assert (first check) (second check)))))))
        ;; The prefix child shares this dedicated group, including on setup failure.
        (ignore-errors (sb-posix:kill (- pid) sb-posix:sigkill))
        (unless reaped
          (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
          (sb-posix:waitpid pid 0))))))

(defmethod test-fixture-run-forked ((platform posix-platform) function)
  "Fork, run FUNCTION in the child, and reap the child."
  (declare (ignore platform))
  (let ((child-pid (sb-posix:fork)))
    (if (zerop child-pid)
        (sb-posix:_exit
         (handler-case (funcall function)
           (serious-condition ()
             1)))
        (multiple-value-bind (waited-pid status)
            (sb-posix:waitpid child-pid 0)
          (and (= waited-pid child-pid)
               (sb-posix:wifexited status)
               (sb-posix:wexitstatus status))))))

(defmethod test-fixture-call-with-forked-holder ((platform posix-platform)
                                                 holder-function function)
  "Synchronize the forked holder and the parent through two pipes."
  (declare (ignore platform))
  (multiple-value-bind (ready-read ready-write)
      (sb-posix:pipe)
    (multiple-value-bind (release-read release-write)
        (sb-posix:pipe)
      (let ((child-pid    (sb-posix:fork))
            (reaped-p     nil)
            (clean-exit-p nil))
        (if (zerop child-pid)
            (progn
              (ignore-errors (sb-posix:close ready-read))
              (ignore-errors (sb-posix:close release-write))
              (handler-case
                  (progn
                    (funcall holder-function)
                    (posix-fixture--write-byte ready-write)
                    (posix-fixture--read-byte release-read)
                    (sb-posix:_exit 0))
                (serious-condition ()
                  (sb-posix:_exit 1))))
            (progn
              (sb-posix:close ready-write)
              (sb-posix:close release-read)
              (unwind-protect
                   (funcall function (= (posix-fixture--read-byte ready-read) 1))
                (ignore-errors
                  (posix-fixture--write-byte release-write))
                (ignore-errors
                  (sb-posix:close ready-read))
                (ignore-errors
                  (sb-posix:close release-write))
                (multiple-value-bind (waited-pid status)
                    (sb-posix:waitpid child-pid 0)
                  (setf reaped-p (= waited-pid child-pid)
                        clean-exit-p (and (sb-posix:wifexited status)
                                          (zerop (sb-posix:wexitstatus status))))))
              (values reaped-p clean-exit-p)))))))


;;;; -- Terminals --

(defmethod test-fixture-call-with-pseudo-terminal ((platform posix-platform)
                                                   function)
  "Hold a pseudo-terminal open through a sleeping shell while FUNCTION runs."
  (declare (ignore platform))
  (let ((process (sb-ext:run-program "/bin/sh"
                                     '("-c" "sleep 10")
                                     :pty  t
                                     :wait nil)))
    (unwind-protect
         (let* ((descriptor (sb-sys:fd-stream-fd (sb-ext:process-pty process)))
                (original   (sb-posix:tcgetattr descriptor))
                (echo-mode  (sb-posix:tcgetattr descriptor)))
           (setf (sb-posix:termios-lflag echo-mode)
                 (logior (sb-posix:termios-lflag echo-mode) sb-posix:echo))
           (unwind-protect
                (progn
                  (sb-posix:tcsetattr descriptor sb-posix:tcsanow echo-mode)
                  (funcall function descriptor))
             (sb-posix:tcsetattr descriptor sb-posix:tcsanow original)))
      (ignore-errors (sb-ext:process-kill process 15))
      (ignore-errors (sb-ext:process-wait process)))))

(defmethod test-fixture-terminal-input-mode ((platform posix-platform)
                                             descriptor)
  "Return DESCRIPTOR's local termios flags."
  (declare (ignore platform))
  (sb-posix:termios-lflag (sb-posix:tcgetattr descriptor)))

(defmethod test-fixture-terminal-echo-p ((platform posix-platform) descriptor)
  "Check the ECHO local flag."
  (logtest sb-posix:echo
           (test-fixture-terminal-input-mode platform descriptor)))
