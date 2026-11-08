(in-package #:autolith)

;;;; -- Process Usage Observations --

(-> test-platform-usage () null)
(defun test-platform-usage ()
  "Exercise native process observations and unavailable-query contracts."
  (dolist (pid '(0 -1 nil "1"))
    (test-assert (null (platform-process-resident-bytes *platform* pid))
                 "Invalid process IDs are unavailable, never process-group queries"))
  (test-assert (typep (platform-process-resident-bytes *platform* (current-process-id))
                      '(integer 1))
               "The current process has a native resident-byte observation")
  (multiple-value-bind (parents available-p)
      (platform-process-parent-snapshot *platform*)
    (test-assert available-p "Native process-parent enumeration is available")
    (test-assert (assoc (current-process-id) parents)
                 "Native process snapshot includes the current process"))
  (when (fboundp 'posix--process-query)
    (dolist (case (list (list "0" 0) (list (format nil " ~C12~%" #\Tab) 12288)
                       (list "" nil) (list "rss" nil) (list "-1" nil)
                       (list "+1" nil) (list "1 2" nil)
                       (list (format nil "1~%2") nil) (list "1x" nil)))
      (test-call-with-function-replacements
       (list (list 'posix--process-query
                   (lambda (arguments)
                     (test-assert (equal arguments '("-o" "rss=" "-p" "42"))
                                  "RSS query selects exactly one positive PID")
                     (values (first case) t))))
       (lambda ()
         (test-assert (eql (platform-process-resident-bytes *platform* 42)
                           (second case))
                      "RSS parsing is strict and converts KiB to bytes"))))
    (dolist (output (list "" (format nil " 1 0~%42 1~%") "1" "1 -1"
                          "1 0 extra" (format nil "1 0~%1 0~%")))
      (test-call-with-function-replacements
       (list (list 'posix--process-query
                   (lambda (arguments)
                     (test-assert (equal arguments '("-ax" "-o" "pid=" "-o" "ppid="))
                                  "Parent query requests one host-wide snapshot")
                     (values output t))))
       (lambda ()
         (multiple-value-bind (parents available-p)
             (platform-process-parent-snapshot *platform*)
           (cond
             ((string= output "")
              (test-assert (and (null parents) available-p)
                           "Empty successful snapshot is available"))
             ((search "42" output)
              (test-assert (and available-p (equal parents '((1 . 0) (42 . 1))))
                           "PID and parent rows are preserved"))
             (t
              (test-assert (and (null parents) (null available-p))
                           "Malformed and duplicate rows invalidate a snapshot")))))))
    (test-call-with-function-replacements
     (list (list 'posix--process-query (lambda (arguments)
                                        (declare (ignore arguments))
                                        (values nil nil))))
     (lambda ()
       (test-assert (null (platform-process-resident-bytes *platform* 42))
                    "Failed RSS query is unavailable")
       (test-assert (equal (multiple-value-list
                            (platform-process-parent-snapshot *platform*)) '(nil nil))
                    "Failed snapshot is distinct from empty")))
    (dolist (status '(1 127))
      (test-call-with-function-replacements
       (list (list 'uiop:run-program
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (values "12" "failure" status))))
       (lambda ()
         (test-assert (null (platform-process-resident-bytes *platform* 42))
                      "Nonzero ps exit cannot produce a measurement"))))
    (test-call-with-function-replacements
     (list (list 'uiop:run-program
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error 'file-error :pathname #p"missing-ps"))))
     (lambda ()
       (test-assert (null (platform-process-resident-bytes *platform* 42))
                    "Missing ps produces unavailable RSS")
       (test-assert (equal (multiple-value-list
                            (platform-process-parent-snapshot *platform*)) '(nil nil))
                    "Missing ps produces unavailable enumeration"))))
  (when (fboundp 'win32--open-process)
    (test-call-with-function-replacements
     (list (list 'win32--open-process
                 (lambda (&rest arguments) (declare (ignore arguments)) 0)))
     (lambda ()
       (test-assert (null (platform-process-resident-bytes *platform* 42))
                    "A disappeared or inaccessible Windows process is unavailable")))
    (dolist (case '((0 nil) (1 0) (1 4096)))
      (let ((closed nil))
        (test-call-with-function-replacements
         (list (list 'win32--open-process
                     (lambda (&rest arguments) (declare (ignore arguments)) 123))
               (list 'win32--get-process-memory-info
                     (lambda (handle counters size)
                       (test-assert (= handle 123) "RSS queries the opened handle")
                       (test-assert (= size 72) "Native memory-counter structure has x64 size")
                       (test-assert (= (sb-alien:slot (sb-alien:deref counters) 'cb) size)
                                    "Memory-counter cb is initialized")
                       (when (second case)
                         (setf (sb-alien:slot (sb-alien:deref counters) 'working-set-size)
                               (second case)))
                       (first case)))
               (list 'win32--close-handle (lambda (handle) (push handle closed) 1)))
         (lambda ()
           (test-assert (eql (platform-process-resident-bytes *platform* 42) (second case))
                        "Windows preserves measured bytes and zero, but not failed queries")))
        (test-assert (equal closed '(123)) "Windows closes every queried process handle")))
    (dolist (case '((18 (nil t)) (5 (nil nil))))
      (let ((closed nil))
        (test-call-with-function-replacements
         (list (list 'win32--create-process-snapshot
                     (lambda (&rest arguments) (declare (ignore arguments)) 123))
               (list 'win32--process-first
                     (lambda (handle entry)
                       (declare (ignore handle))
                       (test-assert (= (sb-alien:slot (sb-alien:deref entry) 'size) 568)
                                    "Process-entry dwSize uses the native x64 structure")
                       0))
               (list 'win32--get-last-error (lambda () (first case)))
               (list 'win32--close-handle (lambda (handle) (push handle closed) 1)))
         (lambda ()
           (test-assert (equal (multiple-value-list
                                (platform-process-parent-snapshot *platform*)) (second case))
                        "Only Windows end-of-enumeration permits an empty snapshot")))
        (test-assert (equal closed '(123)) "Windows closes unsuccessful snapshot handles")))
    (dolist (error-code '(18 5))
      (let ((closed nil))
        (test-call-with-function-replacements
         (list (list 'win32--create-process-snapshot
                     (lambda (&rest arguments) (declare (ignore arguments)) 123))
               (list 'win32--process-first
                     (lambda (handle entry)
                       (declare (ignore handle))
                       (setf (sb-alien:slot (sb-alien:deref entry) 'process-id) 42
                             (sb-alien:slot (sb-alien:deref entry) 'parent-process-id) 1)
                       1))
               (list 'win32--process-next
                     (lambda (&rest arguments) (declare (ignore arguments)) 0))
               (list 'win32--get-last-error (lambda () error-code))
               (list 'win32--close-handle (lambda (handle) (push handle closed) 1)))
         (lambda ()
           (test-assert (equal (multiple-value-list (platform-process-parent-snapshot *platform*))
                               (if (= error-code 18) '(((42 . 1)) t) '(nil nil)))
                        "Windows discards partial snapshots on enumeration failure")))
        (test-assert (equal closed '(123)) "Windows closes completed and failed snapshots")))
    (let ((closed nil))
      (test-call-with-function-replacements
       (list (list 'win32--create-process-snapshot
                   (lambda (&rest arguments) (declare (ignore arguments)) -1))
             (list 'win32--close-handle (lambda (handle) (push handle closed) 1)))
       (lambda ()
         (test-assert (equal (multiple-value-list (platform-process-parent-snapshot *platform*))
                             '(nil nil))
                      "Snapshot creation failure is unavailable")))
      (test-assert (null closed) "Invalid Windows handles are not closed")))
  nil)
