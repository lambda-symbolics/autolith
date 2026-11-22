(in-package #:autolith)

;;;; -- Property Lists --

(-> plist-key-present-p (list keyword) boolean)
(defun plist-key-present-p (plist key)
  "Return true when PLIST contains KEY in a key position, even for a NIL value."
  (loop for tail on plist by (partial #'nthcdr 2)
        thereis (eq (first tail) key)))
