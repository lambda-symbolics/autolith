;;;; -- Standalone Parent Protocol Checks --

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname *load-truename*)))
  (load (merge-pathnames ".qlot/setup.lisp" root))
  (asdf:load-asd (merge-pathnames "autolith-protocol.asd" root))
  (asdf:load-asd (merge-pathnames "autolith-supervisor.asd" root))
  (assert (not (find-package '#:autolith)))
  (asdf:test-system '#:autolith-supervisor/protocol :verbose nil)
  (asdf:test-system '#:autolith-supervisor/session :verbose nil)
  (assert (not (find-package '#:autolith)))
  (write-line "Parent protocol checks passed without loading the agent."))
