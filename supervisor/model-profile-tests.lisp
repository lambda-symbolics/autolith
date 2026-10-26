;;;; -- Trusted Profile Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (function keyword) null) model-test--error))

(defun model-test--error (function reason)
  "Require a typed semantic failure with a redacted reason."
  (assert (eq reason (handler-case (progn (funcall function) nil)
                       (model-projection-error (condition)
                         (model-projection-error-reason condition)))))
  nil)

(declaim (ftype (function () null) run-model-profile-tests))

(defun run-model-profile-tests ()
  "Check exact trusted profile selection and policy validation."
  (let* ((profile  (make-model-profile :id "installed" :model "trusted-model" :family ':responses))
         (registry (make-model-profile-registry (list profile))))
    (assert (eq profile (model-profile-lookup registry "installed")))
    (dolist (id '("INSTALLED" "https://untrusted.invalid/" "../account" ""))
      (model-test--error (lambda () (model-profile-lookup registry id)) ':unknown-profile))
    (model-test--error (lambda () (make-model-profile-registry (list profile profile))) ':duplicate)
    (model-test--error (lambda () (make-model-profile :id "x" :model "m" :family ':unknown))
                       ':invalid-policy)
    (model-test--error (lambda () (make-model-profile :id "x" :model "m" :family ':anthropic
                                                     :capabilities '(:native-compaction)))
                       ':unsupported-family))
  nil)
