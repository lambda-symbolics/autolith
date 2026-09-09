(in-package #:autolith)

;;;; -- Reusable Fixture Tests --

(define-condition test-fixture-error (error)
  ()
  (:documentation "A deliberate failure used to exercise fixture unwinding.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Expected fixture failure." stream))))

(-> test-run-temporary-root-cleanup () null)
(defun test-run-temporary-root-cleanup ()
  "Test scoped run ownership, nested cleanup, thread visibility and every exit."
  (let ((unrelated (test-make-temporary-root))
        (previous (sb-ext:symbol-global-value '*test-temporary-root*)))
    (unwind-protect
         (dolist (exit '(:normal :error :throw))
           (let ((saved-root nil)
                 (configuration-root nil))
             (test-assert
              (equal
               (handler-case
                   (catch 'fixture-exit
                     (multiple-value-list
                      (test-call-with-temporary-root
                       (lambda (root)
                         (setf saved-root root
                               configuration-root
                               (test-configuration-root (test-configuration)))
                         (test-assert (uiop:subpathp configuration-root root)
                                      "configuration roots belong to their run")
                         (let ((thread-root
                                 (sb-thread:join-thread
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (test-configuration-root (test-configuration)))
                                   :name "fixture-root-probe"))))
                           (test-assert (uiop:subpathp thread-root root)
                                        "child thread fixtures share run ownership"))
                         (let ((inner-root nil))
                           (test-call-with-temporary-root
                            (lambda (inner)
                              (setf inner-root inner)
                              (test-assert (uiop:subpathp inner root)
                                           "nested runs belong to the enclosing run")
                              (test-configuration)))
                           (test-assert (not (probe-file inner-root))
                                        "nested cleanup removes the inner run")
                           (test-assert
                            (and (probe-file configuration-root)
                                 (equal root (sb-ext:symbol-global-value
                                              '*test-temporary-root*)))
                            "nested cleanup restores the surviving enclosing run"))
                         (case exit
                           (:error
                            (error 'test-fixture-error))
                           (:throw
                            (throw 'fixture-exit :escaped)))
                         (values :finished 42)))))
                 (test-fixture-error ()
                   :failed))
               (ecase exit
                 (:normal '(:finished 42))
                 (:error :failed)
                 (:throw :escaped)))
              "run scopes preserve multiple values, errors and nonlocal exits")
             (test-assert (and saved-root (not (probe-file saved-root))
                               (not (probe-file configuration-root)))
                          "every exit removes the run and unclaimed fixtures")
             (test-assert (equal previous (sb-ext:symbol-global-value '*test-temporary-root*))
                          "every exit restores the previous process-global fixture parent")
             (test-assert (probe-file unrelated)
                          "cleanup preserves an unrelated run directory")))
      (platform-delete-directory-tree *platform* unrelated :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-configuration-fixture-cleanup () null)
(defun test-configuration-fixture-cleanup ()
  "Test canonical roots, recursive cleanup, and value propagation on every exit."
  (dolist (fail-p '(nil t))
    (let ((saved-root nil))
      (test-assert
       (equal
        (handler-case
            (multiple-value-list
             (with-test-configuration (configuration root)
               (setf saved-root root)
               (test-assert (equal root (truename root))
                            "configuration fixtures expose a canonical root")
               (let ((pathname
                       (merge-pathnames "nested/marker.sexp"
                                        (configuration-data-root configuration))))
                 (snapshot-write pathname '(:fixture :present))
                 (test-assert (probe-file pathname)
                              "fixture bodies can create nested state"))
               (when fail-p
                 (error 'test-fixture-error))
               (values :finished 42)))
          (test-fixture-error ()
            :failed))
        (if fail-p :failed '(:finished 42)))
       "configuration fixtures preserve body values and propagate failures")
      (test-assert (and saved-root (not (probe-file saved-root)))
                   "configuration fixtures remove nested state on every exit")))
  (let ((saved-root nil))
    (test-assert
     (eq :escaped
         (catch 'fixture-exit
           (with-test-configuration (configuration root)
             (declare (ignore configuration))
             (setf saved-root root)
             (throw 'fixture-exit :escaped))))
     "configuration fixtures preserve nonlocal exits")
    (test-assert (not (probe-file saved-root))
                 "configuration fixtures clean up after nonlocal exits"))
  nil)

(-> test-configuration-fixture-isolation () null)
(defun test-configuration-fixture-isolation ()
  "Test nested configurations keep distinct roots and independent durable state."
  (with-test-configuration (outer outer-root)
    (preferences-set-codex-fast-mode outer t)
    (let ((inner-root nil))
      (with-test-configuration (inner root)
        (setf inner-root root)
        (test-assert (not (equal outer-root root))
                     "nested configurations receive distinct temporary roots")
        (dolist (accessor '(configuration-config-root
                            configuration-data-root
                            configuration-state-root
                            configuration-cache-root))
          (test-assert
           (and (uiop:subpathp (funcall accessor outer) outer-root)
                (uiop:subpathp (funcall accessor inner) root)
                (not (equal (funcall accessor outer) (funcall accessor inner))))
           "every mutable configuration root belongs to its own fixture"))
        (test-assert (not (preference-state-codex-fast-mode-p
                          (preferences-load inner)))
                     "an inner configuration cannot read outer preferences")
        (preferences-set-reasoning-traces inner t))
      (test-assert (not (probe-file inner-root))
                   "inner cleanup removes only the inner configuration")
      (test-assert
       (and (probe-file outer-root)
            (preference-state-codex-fast-mode-p (preferences-load outer))
            (not (preference-state-reasoning-traces-p (preferences-load outer))))
       "outer configuration state survives inner mutation and cleanup")))
  nil)

(-> test-environment-fixture-restoration () null)
(defun test-environment-fixture-restoration ()
  "Test nested environment fixtures restore absent, empty, and nonempty values."
  (let* ((name (format nil "AUTOLITH_FIXTURE_~A" (make-identifier)))
         (original (uiop:getenv name)))
    (unless (test-fixture-available-p *platform* ':empty-environment-values)
      (test-withheld ':empty-environment-values
                     "environment fixtures restoring an empty value"))
    (with-test-environment ((name nil))
      (dolist (initial (if (test-fixture-available-p *platform*
                                                     ':empty-environment-values)
                           '(nil "" "outer")
                           '(nil "outer")))
        (with-test-environment ((name initial))
          (dolist (fail-p '(nil t))
            (test-assert
             (equal
              (handler-case
                  (multiple-value-list
                   (with-test-environment ((name "inner"))
                     (test-assert (string= (uiop:getenv name) "inner")
                                  "environment fixtures install their values")
                     (with-test-environment ((name nil))
                       (test-assert (null (uiop:getenv name))
                                    "NIL fixture values remove variables"))
                     (test-assert (string= (uiop:getenv name) "inner")
                                  "nested removal restores the enclosing value")
                     (when fail-p
                       (error 'test-fixture-error))
                     (values :finished 42)))
                (test-fixture-error ()
                  :failed))
              (if fail-p :failed '(:finished 42)))
             "environment fixtures preserve values and propagate failures")
            (test-assert (equal (uiop:getenv name) initial)
                         "environment fixtures restore their precise prior state")))))
    (test-assert (equal (uiop:getenv name) original)
                 "outer environment cleanup restores the process state"))
  nil)

(-> test-environment-fixture-evaluation () null)
(defun test-environment-fixture-evaluation ()
  "Test binding expressions run once and invalid setup cannot leak earlier edits."
  (let ((name (format nil "AUTOLITH_FIXTURE_~A" (make-identifier)))
        (name-count 0)
        (value-count 0))
    (with-test-environment ((name "outer"))
      (with-test-environment (((progn (incf name-count) name)
                               (progn (incf value-count) "inner")))
        (test-assert (and (= name-count 1) (= value-count 1))
                     "fixture names and values are evaluated once"))
      (test-assert
       (handler-case
           (progn
             (with-test-environment ((name "changed") ("IGNORED" 42))
               :unexpected)
             nil)
         (type-error ()
           t))
       "invalid environment values signal a typed setup failure")
      (test-assert (string= (uiop:getenv name) "outer")
                   "environment setup failures do not leak preceding bindings")))
  nil)

(-> test-function-replacement-fixture-restoration () null)
(defun test-function-replacement-fixture-restoration ()
  "Test nested function replacements restore their definitions after errors."
  (let* ((name (gensym "FIXTURE-FUNCTION"))
         (original (lambda () :original))
         (outer (lambda () :outer))
         (inner (lambda () :inner)))
    (unwind-protect
         (progn
           (setf (symbol-function name) original)
           (test-call-with-function-replacements
            (list (list name outer))
            (lambda ()
              (test-assert (eq (funcall name) :outer)
                           "function fixtures install their replacement")
              (test-assert
               (handler-case
                   (test-call-with-function-replacements
                    (list (list name inner))
                    (lambda ()
                      (test-assert (eq (funcall name) :inner)
                                   "nested function fixtures replace the outer definition")
                      (error 'test-fixture-error)))
                 (test-fixture-error ()
                   t))
               "function fixture errors propagate to the caller")
              (test-assert (eq (symbol-function name) outer)
                           "failed nested replacements restore the outer function")))
           (test-assert (eq (symbol-function name) original)
                        "normal fixture exit restores the original function"))
      (fmakunbound name)))
  nil)
