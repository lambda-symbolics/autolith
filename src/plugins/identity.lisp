(in-package #:autolith)

;;;; -- Plugin Identity --

(defclass plugin-identity ()
  ((source
    :initarg :source
    :reader plugin-identity-source
    :type non-empty-string
    :documentation "The requested git repository URL or local checkout path.")
   (commit
    :initarg :commit
    :reader plugin-identity-commit
    :type non-empty-string
    :documentation "The exact git commit the requested ref resolved to."))
  (:documentation "The identity of one resolved plugin: git source plus commit.

A ref that resolves to another commit names another identity. The permission
store and the plugin lock both key on this pair, so one identity means one
exact checkout of one repository."))

(-> plugin-identity-p (t) boolean)
(defun plugin-identity-p (value)
  "Return true when VALUE is a resolved plugin identity."
  (typep value 'plugin-identity))

(-> plugin-identity-equal-p (t t) boolean)
(defun plugin-identity-equal-p (left right)
  "Return true when LEFT and RIGHT name the same source and resolved commit."
  (and (plugin-identity-p left)
       (plugin-identity-p right)
       (string= (plugin-identity-source left)
                (plugin-identity-source right))
       (string= (plugin-identity-commit left)
                (plugin-identity-commit right))
       t))

(-> plugin-identity->form (plugin-identity) list)
(defun plugin-identity->form (identity)
  "Return IDENTITY as a portable readable form."
  (list :plugin-identity
        :source (copy-seq (plugin-identity-source identity))
        :commit (copy-seq (plugin-identity-commit identity))))

(defmethod print-object ((identity plugin-identity) stream)
  "Print IDENTITY as its source and resolved commit."
  (print-unreadable-object (identity stream :type t)
    (format stream "~S ~S"
            (plugin-identity-source identity)
            (plugin-identity-commit identity))))
