;;;; -- Standalone Native Parent Checks --

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname *load-truename*)))
  (load (merge-pathnames ".qlot/setup.lisp" root))
  (asdf:load-asd (merge-pathnames "autolith-supervisor.asd" root))
  (assert (not (find-package '#:autolith)))
  (asdf:load-system '#:autolith-supervisor/parent-application/tests :verbose nil)
  (asdf:load-system '#:autolith-supervisor/access-native/tests :verbose nil)
  (asdf:load-system '#:autolith-supervisor/parent-launch/production-tests :verbose nil))

(in-package #:autolith-supervisor)

(destructuring-bind (mode runtime-name core-name temporary-name result-name)
    (uiop:command-line-arguments)
  (let* ((root      (asdf:system-source-directory '#:autolith-supervisor))
         (runtime   (uiop:parse-native-namestring runtime-name))
         (core      (uiop:parse-native-namestring core-name))
         (temporary (uiop:ensure-directory-pathname
                     (uiop:parse-native-namestring temporary-name)))
         (host      (cond
                      ((uiop:os-macosx-p)
                       ':darwin)
                      ((uiop:os-windows-p)
                       ':windows)
                      ((member :linux *features*)
                       ':linux)
                      (t
                       ':unsupported)))
         (available-p
           (handler-case
               (not (null (access-native--host-class host)))
             (access-policy-error (condition)
               (if (eq (access-policy-error-code condition) ':unsupported-native-host)
                   nil
                   (error condition)))))
         (result
           (if available-p
               (let ((roots (parent-cli-runtime-roots root runtime :agent-core core :worker-core core)))
                 (cond
                   ((string= mode "session")
                    (list :status ':passed :checks
                          (run-parent-provider-extension-native-test
                           :runtime runtime :core core :source-root root
                           :temporary temporary :runtime-roots roots)))
                   ((string= mode "production")
                    (list :status ':passed :checks
                          (run-parent-launch-production-e2e-tests
                           :runtime runtime :core core :source-root root
                           :temporary temporary :runtime-roots roots)))
                   ((string= mode "network")
                    (let ((report (run-parent-network-native-test :runtime-roots roots)))
                      (assert (or (eq (getf report :status) ':passed)
                                  (member (getf report :reason)
                                          '(:unsupported-native-network :compiler-unavailable))))
                      report))
                   ((string= mode "policy")
                    (let ((report (run-native-host-live-access-tests :runtime-roots roots)))
                      (assert (or (eq (getf report :status) ':passed)
                                  (member (getf report :reason) '(:foreign-host :compiler-unavailable))))
                      report))
                   (t
                    (error "Unknown native test mode: ~S" mode))))
               (list :status ':skipped :reason ':unsupported-native-host :host host))))
    (assert (not (find-package '#:autolith)))
    (with-open-file (stream result-name :direction ':output :if-exists ':error)
      (with-standard-io-syntax
        (write result :stream stream)
        (terpri stream)))
    (format t "Parent ~A checks: ~S~%" mode result)))
