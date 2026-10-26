(in-package #:autolith)

;;;; -- Standalone Supervisor Boundary --

(-> test-supervisor-subprocess-boundary () null)
(defun test-supervisor-subprocess-boundary ()
  "Exercise the standalone supervisor without loading the agent into its process."
  (let* ((root    (asdf:system-source-directory :autolith-agent))
         (command (list (active-image--sbcl-command)
                        "--noinform" "--end-runtime-options" "--no-sysinit" "--no-userinit" "--script"
                        (namestring (merge-pathnames "supervisor/tests.lisp" root)))))
    (multiple-value-bind (output errors status)
        (uiop:run-program command :directory          root
                                 :output ':string :error-output ':string
                                 :ignore-error-status t)
      (test-assert (zerop status)
                   (format nil "standalone supervisor checks exit ~D: ~A~A"
                           status output errors))))
  nil)

(-> test-supervisor-protocol-boundary () null)
(defun test-supervisor-protocol-boundary ()
  "Exercise the generated wire protocol in a fresh parent process without agent code."
  (let* ((root    (asdf:system-source-directory :autolith-agent))
         (command (list (active-image--sbcl-command)
                        "--noinform" "--end-runtime-options" "--no-sysinit" "--no-userinit" "--script"
                        (namestring (merge-pathnames "supervisor/protocol-test-entry.lisp" root)))))
    (multiple-value-bind (output errors status)
        (uiop:run-program command :directory          root
                                 :output ':string :error-output ':string
                                 :ignore-error-status t)
      (test-assert (zerop status)
                   (format nil "parent protocol checks exit ~D: ~A~A" status output errors))))
  nil)
