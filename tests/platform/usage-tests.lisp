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
  nil)
