#!/usr/bin/env -S sbcl --script

(require :asdf)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)
(require :sb-posix)

(let* ((arguments       (uiop:command-line-arguments))
       (source-root
         (uiop:ensure-directory-pathname
          (or (first arguments)
              (error "The recovery launcher needs the source root."))))
       (project-setup   (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup      (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname)))
       (quicklisp-setup (if (probe-file project-setup)
                           project-setup
                           user-setup)))
  (unless (probe-file quicklisp-setup)
    (error "Recovery bootstrap needs Quicklisp at ~A." quicklisp-setup))
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload '(:serapeum :sbcl-generations) :silent t)
  (let ((package (or (find-package "AUTOLITH")
                     (make-package "AUTOLITH" :use '("CL")))))
    (export (mapcar (lambda (name) (intern name package))
                    '("RECOVERY-MAIN" "RECOVERY-IMAGE-SAVE"))
            package))
  (load (merge-pathnames "recovery/runtime.lisp" source-root))
  (uiop:symbol-call '#:autolith '#:recovery-main))
