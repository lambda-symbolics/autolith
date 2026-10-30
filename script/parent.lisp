;;;; -- Trusted Parent Entry --

(require :asdf)

(let* ((root      (uiop:pathname-parent-directory-pathname *load-truename*))
       (arguments (uiop:command-line-arguments)))
  (load (merge-pathnames ".qlot/setup.lisp" root))
  (pushnew root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "autolith-protocol.asd" root))
  (asdf:load-asd (merge-pathnames "autolith-supervisor.asd" root))
  (asdf:load-system "autolith-supervisor/parent-application")
  (handler-case
      (uiop:quit
       (cond
         ((or (null arguments) (equal (first arguments) "--cli"))
          (uiop:symbol-call '#:autolith-supervisor '#:parent-cli-main
                            (if arguments (rest arguments) nil)))
         ((<= 1 (length arguments) 2)
          (uiop:symbol-call
           '#:autolith-supervisor '#:parent-cli-exit-code
           (uiop:symbol-call '#:autolith-supervisor '#:parent-application-main
                             (pathname (first arguments)) :session-id (second arguments))))
         (t
          (write-line "Usage: script/parent [--cli [ARGUMENTS] | CONFIGURATION.sexp [SESSION-ID]]"
                      *error-output*)
          64)))
    (error ()
      (write-line "Parent application stopped. Private diagnostics were not printed." *error-output*)
      (uiop:quit 1))))
