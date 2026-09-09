(in-package #:autolith)

;;;; -- POSIX Host Fixtures --

;;; The POSIX implementation of the host fixture protocol declared in
;;; tests/test-support.lisp. Linux, macOS, and the BSDs provide every
;;; fixture.

(defmethod test-fixture-available-p ((platform posix-platform) fixture)
  "POSIX hosts provide every fixture."
  (declare (ignore platform))
  (check-type fixture test-fixture-kind)
  t)


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
  "Return PATHNAME's permission bits from stat(2)."
  (declare (ignore platform))
  (logand (sb-posix:stat-mode (sb-posix:stat (namestring pathname))) #o777))

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
       (:private-file #o600)
       (:private-directory #o700)
       (:read-only #o444))))


;;;; -- Descriptors and Processes --

(-> posix-fixture--descriptor-stream (integer (member :input :output)) stream)
(defun posix-fixture--descriptor-stream (descriptor direction)
  "Return an unbuffered character stream over DESCRIPTOR in DIRECTION."
  (sb-sys:make-fd-stream descriptor
                         :input (eq direction ':input)
                         :output (eq direction ':output)
                         :element-type 'character
                         :external-format ':utf-8
                         :buffering ':none
                         :auto-close nil))

(defmethod test-fixture-call-with-descriptor-input ((platform posix-platform)
                                                    content function)
  "Feed CONTENT through a pipe whose read end backs the stream FUNCTION receives."
  (declare (ignore platform))
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input nil)
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
           :element-type '(unsigned-byte 8)
           :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:read descriptor (sb-sys:vector-sap buffer) 1))))

(-> posix-fixture--write-byte (integer) integer)
(defun posix-fixture--write-byte (descriptor)
  "Write one synchronization byte to DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type '(unsigned-byte 8)
           :initial-element 1)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:write descriptor (sb-sys:vector-sap buffer) 1))))

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
      (let ((child-pid (sb-posix:fork))
            (reaped-p nil)
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
                    ;; Exit without unwinding so the parent observes kernel
                    ;; cleanup after a dead holder.
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
                                     :pty t
                                     :wait nil)))
    (unwind-protect
         (let* ((descriptor (sb-sys:fd-stream-fd (sb-ext:process-pty process)))
                (original (sb-posix:tcgetattr descriptor))
                (echo-mode (sb-posix:tcgetattr descriptor)))
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
