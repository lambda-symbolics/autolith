(in-package #:autolith)

;;;; -- Lisp Parenthesis Check Tool --

(defclass lisp-paren-check-tool (workspace-tool)
  ()
  (:documentation
   "Check workspace Lisp-family files for unmatched or mismatched delimiters."))

(defmethod tool-child-safe-p ((tool lisp-paren-check-tool))
  "Permit bounded Lisp-family source checks inside child agents."
  (declare (ignore tool))
  t)

(defmethod tool-storm-guard-exempt-p ((tool lisp-paren-check-tool))
  "Exempt bounded source delimiter checks from the mutating-call storm guard."
  t)

(defmethod tool-compact-result-visible-p ((tool lisp-paren-check-tool))
  "Keep successful explicit source checks visible in compact presentation."
  (declare (ignore tool))
  t)

(defmethod tool-execute ((tool lisp-paren-check-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Check one recognized Lisp-family file or a directory tree.

An approved path outside the workspace is readable for this check; only the
requested tree is checked."
  (declare (ignore tool))
  (let ((requested (tool-argument arguments "path" :required t)))
    (unless (non-empty-string-p requested)
      (error 'tool-error
             :message   "lisp.paren-check requires a non-empty path string."
             :tool-name "lisp.paren-check"))
    (let* ((path  (workspace-tool-path context requested :tool-name "lisp.paren-check"))
           (roots (workspace-tool-readable-roots context)))
      (multiple-value-bind (success-p content)
          (lisp-paren-check-path
           path
           :readable-roots (if (workspace-tool--read-path-allowed-p path roots)
                               roots
                               (cons (lisp-paren-check-tool--root path) roots)))
        (if success-p
            (tool-success content)
            (tool-failure content))))))

(-> lisp-paren-check-tool--root (pathname) pathname)
(defun lisp-paren-check-tool--root (path)
  "Return approved PATH if it is a directory; otherwise, return its containing directory."
  (or (uiop:directory-exists-p path)
      (uiop:pathname-directory-pathname path)))
