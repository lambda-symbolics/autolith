(in-package #:autolith)

(-> windows-tests--script (pathname list) pathname)
(defun windows-tests--script (pathname forms)
  "Write readable FORMS as a native runtime test script."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                   :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*package* (find-package '#:autolith)))
        (dolist (form forms)
          (write form :stream stream)
          (terpri stream)))))
  pathname)

(-> windows-tests--command (pathname &rest string) list)
(defun windows-tests--command (script &rest arguments)
  "Return the running runtime's script invocation with literal ARGUMENTS."
  (append (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                "--no-userinit" "--no-sysinit" "--script" (namestring script))
          arguments))

(-> test-windows-restart-library () null)
(defun test-windows-restart-library ()
  "Save and boot the library's exact cyclic heap on native Windows.

The library checks whole-process facts, such as being the only live Lisp
thread before a save, so its test operation runs in a fresh runtime instead of
this shared test worker."
  (with-platform-capability (':restartable-image-saver "native Windows exact heap save")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((setup  (merge-pathnames ".qlot/setup.lisp"
                                      (asdf:system-source-directory ':autolith)))
             (script (windows-tests--script
                      (merge-pathnames "library-tests.lisp" root)
                      `((require :asdf)
                        (load ,(namestring setup))
                        (asdf:test-system "sbcl-generations")))))
        (multiple-value-bind (output error-output code)
            (uiop:run-program (windows-tests--command script)
                              :output              ':string
                              :error-output        ':output
                              :ignore-error-status t)
          (declare (ignore error-output))
          (test-assert (zerop code)
                       (format nil "library tests in a fresh runtime exit ~D:~%~A"
                               code output))))))
  nil)
