;;;; -- Supervisor Subprocess Fixture --

(require :asdf)

(let ((arguments (uiop:command-line-arguments)))
  (cond
    ((equal (first arguments) "echo")
     (write (list (rest arguments) (read-line *standard-input*)
                  (namestring (uiop:getcwd)))
            :stream *standard-output*)
     (terpri)
     (write-line "child stderr" *error-output*)
     (uiop:quit 23))
    ((equal (first arguments) "sleep")
     #-win32 (write-line (write-to-string (sb-unix:unix-getpid)))
      (finish-output)
      (sleep 60))
    ((equal (first arguments) "wrapper")
     (write-line "wrapper" *error-output*)
     (uiop:quit
      (uiop:wait-process
       (uiop:launch-program (rest arguments)
                            :input        ':interactive
                            :output       ':interactive
                            :error-output ':interactive))))
    ((equal (first arguments) "api")
     (let* ((root    (uiop:pathname-parent-directory-pathname *load-truename*))
            (runtime (namestring sb-ext:*runtime-pathname*))
            (child   (list runtime "--noinform" "--no-sysinit" "--no-userinit"
                           "--script" (namestring *load-truename*))))
       (asdf:load-asd (merge-pathnames "autolith-supervisor.asd" root))
       (asdf:load-system '#:autolith-supervisor :verbose nil)
       (assert (not (find-package '#:autolith)))
       (assert (not (find-package '#:ql)))
       (let* ((*standard-output* (make-string-output-stream))
              (status            (uiop:symbol-call '#:autolith-supervisor '#:run-agent
                                                   (append child (list "echo" "api argument"))
                                                   :launcher  (append child (list "wrapper"))
                                                   :directory root)))
         (assert (string= "" (get-output-stream-string *standard-output*)))
         (uiop:quit status))))
    ((equal (first arguments) "isolated")
     (assert (not (find-package '#:autolith)))
     (assert (not (find-package '#:ql)))
     (assert (not (find-package '#:autolith-supervisor)))
     (uiop:quit 0))))
