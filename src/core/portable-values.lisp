(in-package #:autolith)

;;;; -- Bounded Portable Values --

(define-condition portable-value-error (autolith-error)
  ((reason
    :initarg       :reason
    :reader        portable-value-error-reason
    :type          keyword
    :documentation "The structural rejection: cycle, depth, nodes or dotted-list."))
  (:default-initargs :message "Portable data is cyclic or exceeds its structural bounds.")
  (:documentation "A payload-free rejection of an unsafe portable value tree."))

(-> portable-value-map
    (t function &key (:containers-p boolean) (:proper-lists-p boolean) (:list function)
                     (:maximum-depth (integer 0)) (:maximum-nodes (integer 0))
                     (:list-node-policy (member :spine :tree :container))) t)
(defun portable-value-map (value convert &key containers-p proper-lists-p (list #'identity)
                                          (maximum-depth 128) (maximum-nodes 262144)
                                          (list-node-policy ':spine))
  "Copy bounded trees and call CONVERT on other values with a child mapper.

CONVERT receives a value and a one-argument mapper for children. CONTAINERS-P
includes vectors and hash tables in active-path cycle checks and allocation
preflight. CONVERT must map their children before allocating unbounded output.
LIST finishes each copied cons tree. Shared completed trees copy independently.
Root depth is zero; CAR and container children add one level, while cons tails
retain their spine's depth. MAXIMUM-DEPTH and MAXIMUM-NODES bound structure,
not string bytes. LIST-NODE-POLICY determines cons accounting: :SPINE charges
one list entry, every cons cell and the terminal tail; :TREE charges each cons
cell and the terminal tail; :CONTAINER charges the list entry but no spine
cells or terminal NIL. All policies charge each mapped element."
  (check-type maximum-depth (integer 0))
  (check-type maximum-nodes (integer 0))
  (check-type list-node-policy (member :spine :tree :container))
  (let ((active (make-hash-table :test #'eq))
        (nodes  0))
    (labels ((reject (reason)
               "Reject the tree without retaining or printing its payload."
               (error 'portable-value-error :reason reason))

             (charge (depth)
               "Account for one visit before allocating its output."
               (when (> depth maximum-depth)
                 (reject ':depth))
               (when (> (incf nodes) maximum-nodes)
                 (reject ':nodes)))

             (enter (item)
               "Retain ITEM only while visiting its active descendants."
               (when (gethash item active)
                 (reject ':cycle))
               (setf (gethash item active) t))

             (walk (item depth)
               "Map ITEM with bounded recursion and iterative cons spines."
               (charge depth)
               (cond
                 ((consp item)
                  (let ((tails nil))
                    (unwind-protect
                         (loop for tail = item then (rest tail)
                               while (consp tail)
                               do (when (or (eq list-node-policy ':spine)
                                            (and (eq list-node-policy ':tree) (not (eq tail item))))
                                    (charge depth))
                                  (enter tail)
                                  (push tail tails)
                               collect (walk (first tail) (1+ depth)) into items
                               finally
                                  (when (and proper-lists-p tail)
                                    (reject ':dotted-list))
                                  (let ((end (unless (and (eq list-node-policy ':container) (null tail))
                                               (walk tail depth))))
                                    (return (funcall list (nconc items (unless proper-lists-p end))))))
                      (dolist (tail tails)
                        (remhash tail active)))))
                 ((and containers-p
                       (or (hash-table-p item)
                           (and (vectorp item) (not (stringp item)))))
                  (enter item)
                  (unwind-protect
                       (progn
                         ;; Every member requires at least one subsequent visit.
                         (when (> (+ nodes (if (hash-table-p item)
                                              (hash-table-count item)
                                              (length item)))
                                  maximum-nodes)
                           (reject ':nodes))
                         (funcall convert item (lambda (child) (walk child (1+ depth)))))
                    (remhash item active)))
                 (t
                  (funcall convert item (lambda (child) (walk child (1+ depth))))))))
      (walk value 0))))
