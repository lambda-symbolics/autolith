(in-package #:autolith)

;;;; -- Release Version Tests --

(-> test-version-comparison () null)
(defun test-version-comparison ()
  "Test release version parsing and ordering."
  (dolist (case '(("0.50.3" (0 50 3))
                  ("0.50.3-dev.5" (0 50 3))
                  ("1.0" (1 0))
                  ("2" (2))))
    (destructuring-bind (version components) case
      (test-assert (equal (version-components version) components)
                   (format nil "~A parses into its integer components" version))))
  (dolist (case '(("0.51.0" "0.52.0" t)
                  ("0.52.0" "0.51.0" nil)
                  ("0.52.0" "0.52.0" nil)
                  ("0.52" "0.52.0" nil)
                  ("0.9.9" "0.10.0" t)
                  ("0.53.7-dev.2" "0.54.0" t)
                  ("1.0.0" "0.54.0" nil)))
    (destructuring-bind (left right expected) case
      (test-assert (eq (version< left right) expected)
                   (format nil "~A < ~A is ~A" left right expected))))
  nil)
