;;;; -- Runtime Probe --

;;; Shared host-runtime probe for launchers that have not loaded Autolith.

(require :asdf)
(load (merge-pathnames "runtime-requirement.lisp"
                       (uiop:pathname-directory-pathname *load-truename*)))

(let ((source-root (uiop:pathname-parent-directory-pathname
                    (uiop:pathname-directory-pathname *load-truename*))))
  (autolith-require-minimum-runtime (merge-pathnames "sbcl-minimum.version" source-root))
  (write-string (lisp-implementation-version)))
