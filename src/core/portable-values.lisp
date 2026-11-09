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
    (t function &key (:containers-p boolean) (:proper-lists-p boolean) (:list function)) t)
(defun portable-value-map (value convert &key containers-p proper-lists-p (list #'identity))
  "Copy bounded cons spines and call CONVERT on other values with a child mapper.

CONVERT receives the value and a one-argument mapper for children. With
CONTAINERS-P, vectors and hash tables participate in active-path cycle checks
and allocation preflight. CONVERT must map their children before allocating
unbounded output. LIST finishes each copied cons tree. Completed shared trees
are copied independently. Root depth is zero; CAR and container children add
one level, while the terminal cons tail retains the spine's depth. Charge each
walk entry and spine cell, allowing depth 128 and 262144 charges. These limits
bound structure, not string bytes."
  (let ((active (make-hash-table :test #'eq))
        (nodes  0))
    (labels ((reject (reason)
               "Reject the tree without retaining or printing its payload."
               (error 'portable-value-error :reason reason))

             (charge (depth)
               "Account for one visit before allocating its output."
               (when (> depth 128)
                 (reject ':depth))
               (when (> (incf nodes) 262144)
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
                               do (charge depth)
                                  (enter tail)
                                  (push tail tails)
                               collect (walk (first tail) (1+ depth)) into items
                               finally
                                  (when (and proper-lists-p tail)
                                    (reject ':dotted-list))
                                  (let ((end (walk tail depth)))
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
                                  262144)
                           (reject ':nodes))
                         (funcall convert item (lambda (child) (walk child (1+ depth)))))
                    (remhash item active)))
                 (t
                  (funcall convert item (lambda (child) (walk child (1+ depth))))))))
      (walk value 0))))
