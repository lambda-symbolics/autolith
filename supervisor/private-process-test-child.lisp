;;;; -- Private Connection Subprocess Fixture --

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname *load-truename*)))
  (handler-bind ((style-warning #'muffle-warning))
    (dolist (file '("supervisor/package.lisp" "supervisor/framing.lisp"
                    #+win32 "supervisor/platform-windows.lisp"
                    #-win32 "supervisor/platform-posix.lisp"
                    "supervisor/process.lisp" "supervisor/private-process.lisp"
                    #+win32 "supervisor/private-process-windows.lisp"
                    #-win32 "supervisor/private-process-posix.lisp"))
      (load (merge-pathnames file root)))))

(in-package #:autolith-supervisor)

(when (equal (first (uiop:command-line-arguments)) "leaf")
  (protocol-write-frame (private-process-child-connection)
                        (sb-ext:string-to-octets "leaf-ready" :external-format ':utf-8))
  (sleep 30)
  (sb-ext:exit :code 0))

(let ((connection (private-process-child-connection))
      (mode       (first (uiop:command-line-arguments))))
  (protocol-write-frame
   connection
   (sb-ext:string-to-octets
    (format nil "~a|~a" (uiop:getenv "AUTOLITH_PARENT_INSTANCE_ID")
            (or (uiop:getenv "AUTOLITH_PRIVATE_UNRELATED") "absent"))
    :external-format ':utf-8))
  (cond
    ((equal mode "echo")
     (dotimes (index 10000)
       (write-char #\x *standard-output*)
       (write-char #\y *error-output*))
     (format *standard-output* "~C[2Jstdout~%" #\Escape)
     (format *error-output* "~C]0;spoof~Cstderr~%" #\Escape #\Bell)
     (finish-output *standard-output*)
     (finish-output *error-output*)
     (let ((frame (protocol-read-frame connection)))
       (protocol-write-frame connection frame)))
    ((equal mode "malformed")
     (write-sequence (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
                     connection)
     (finish-output connection)
     (sleep 30))
    ((equal mode "crash")
     (sb-ext:exit :code 23 :abort t))
    ((member mode '("descendant" "descendant-exit") :test #'equal)
     ;; Inherited stdin keeps the POSIX descendant in its owner's process group.
     (sb-ext:run-program
      (namestring sb-ext:*runtime-pathname*)
      (list "--noinform" "--no-sysinit" "--no-userinit" "--script"
            (namestring *load-truename*) "leaf")
      :wait nil :input t :output t :error t
      :environment (append
                    '("AUTOLITH_PARENT_INSTANCE_ID=private-test-instance")
                    (when (uiop:os-windows-p)
                      (list (concatenate 'string "SystemRoot=" (uiop:getenv "SystemRoot"))))))
     (when (equal mode "descendant")
       (sleep 30)))
    ((equal mode "sleep")
     (sleep 30)))
  (close connection))
