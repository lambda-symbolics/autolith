;;;; -- Private Process Transport Checks --

(in-package #:autolith-supervisor)

(defparameter *private-process-test-root*
  (asdf:system-source-directory '#:autolith-supervisor)
  "Source root for the standalone subprocess fixture.")

(declaim (ftype (function (string &key (:startup t)) private-process)
                private-process--test-open))

(defun private-process--test-open (mode &key startup)
  "Start a small source-only fixture, not a full agent image."
  (private-process-open
   (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--no-sysinit"
         "--no-userinit" "--script"
         (namestring (merge-pathnames "supervisor/private-process-test-child.lisp"
                                     *private-process-test-root*))
         mode)
   :instance-id "private-test-instance" :diagnostic-limit 128
   :environment (when (uiop:os-windows-p)
                  (list (concatenate 'string "SystemRoot=" (uiop:getenv "SystemRoot"))))
   :directory *private-process-test-root* :startup startup :startup-timeout 5))

(declaim (ftype (function (private-process) null) private-process--test-hello))

(defun private-process--test-hello (process)
  "Check private startup identity and absence of an unrelated parent variable."
  (let ((hello (protocol-read-frame (private-process-connection process))))
    (assert (equal (sb-ext:octets-to-string hello :external-format ':utf-8)
                   "private-test-instance|absent")))
  nil)

(declaim (ftype (function () integer) run-private-process-tests))

(defun run-private-process-tests ()
  "Exercise real inherited byte connections, diagnostic bounds and process exit."
  (let ((process (private-process--test-open "echo" :startup #'private-process--test-hello)))
    (unwind-protect
         (let* ((connection (private-process-connection process))
                (bytes      (make-array 256 :element-type '(unsigned-byte 8))))
           (dotimes (index 256) (setf (aref bytes index) index))
           (sb-ext:with-timeout 5
             (protocol-write-frame connection bytes)
             (assert (equalp bytes (protocol-read-frame connection))))
           (let* ((evidence    (private-process-await process :timeout 5))
                  (diagnostics (getf evidence :diagnostics)))
             (assert (getf evidence :native-scope-clean-p))
             (assert (zerop (getf evidence :exit-code)))
             (assert (not (getf evidence :timed-out)))
             (assert (= (length diagnostics) 2))
             (dolist (diagnostic diagnostics)
               (let ((text (getf diagnostic :text)))
                 (assert (<= (length text) 128))
                 (assert (plusp (getf diagnostic :dropped-bytes)))
                 (assert (not (find #\Escape text)))
                 (assert (not (find #\Bell text)))))
             (assert (search "stdout" (getf (first diagnostics) :text)))
             (assert (search "stderr" (getf (second diagnostics) :text)))
             (assert (not (open-stream-p connection)))))
      (private-process-close process)))
  (let ((process (private-process--test-open "crash" :startup #'private-process--test-hello)))
    (unwind-protect
         (let ((evidence (private-process-await process :timeout 5)))
           (assert (= (getf evidence :exit-code) 23))
           (assert (getf evidence :native-scope-clean-p)))
      (private-process-close process)))
  (let ((process nil) (failed-p nil))
    (handler-case
        (private-process--test-open
         "malformed"
         :startup (lambda (child)
                    (setf process child)
                    (private-process--test-hello child)
                    (protocol-read-frame (private-process-connection child))))
      (protocol-frame-error ()
        (setf failed-p t)))
    (assert failed-p)
    (assert (getf (private-process-evidence process) :native-scope-clean-p))
    (assert (not (open-stream-p (private-process-connection process)))))
  (let ((process (private-process--test-open "sleep" :startup #'private-process--test-hello)))
    (unwind-protect
         (let ((evidence (private-process-await process :timeout 0.05)))
           (assert (getf evidence :timed-out))
           (assert (getf evidence :native-scope-clean-p)))
      (private-process-close process)))
  (dolist (mode '("descendant" "descendant-exit"))
    (let ((process (private-process--test-open mode :startup #'private-process--test-hello)))
      (unwind-protect
           (progn
             (sb-ext:with-timeout 5
               (assert (equal "leaf-ready"
                              (sb-ext:octets-to-string
                               (protocol-read-frame (private-process-connection process))
                               :external-format ':utf-8))))
             (let ((evidence (if (equal mode "descendant-exit")
                                 (private-process-await process :timeout 5)
                                 (private-process-close process))))
               (assert (getf evidence :native-scope-clean-p))
               (when (equal mode "descendant-exit")
                 (assert (zerop (getf evidence :exit-code)))
                 (assert (not (getf evidence :timed-out))))))
        (private-process-close process))))
  (let ((process nil) (timed-out-p nil))
    (handler-case
        (private-process-open
         (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--no-sysinit"
               "--no-userinit" "--script"
               (namestring (merge-pathnames "supervisor/private-process-test-child.lisp"
                                           *private-process-test-root*))
               "sleep")
         :instance-id "private-test-instance" :environment '()
         :startup-timeout 0.05
         :startup         (lambda (child) (setf process child) (sleep 30)))
      (sb-ext:timeout ()
        (setf timed-out-p t)))
    (assert timed-out-p)
    (assert (getf (private-process-evidence process) :native-scope-clean-p)))
  (assert (handler-case
              (progn
                (private-process-open '("unused") :instance-id "private-test-instance")
                nil)
            (private-process-error () t)))
  8)
