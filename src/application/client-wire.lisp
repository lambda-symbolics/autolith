(in-package #:autolith)

;;;; -- Native Method Frame Values --

(-> application-client-wire--scalar-p (t) boolean)
(defun application-client-wire--scalar-p (value)
  "Recognize native scalars with a reader-evaluation-free printed representation."
  (or (symbolp value) (stringp value) (characterp value) (pathnamep value)
      (and (rationalp value) (<= (integer-length (numerator value)) 1048576)
           (<= (integer-length (denominator value)) 1048576))
      (and (floatp value)
           (handler-case (<= (- most-positive-double-float) value most-positive-double-float)
             (arithmetic-error () nil)))))

(-> application-client-wire--reject () nil)
(defun application-client-wire--reject ()
  "Reject a malformed method frame without printing or retaining its payload."
  (error 'application-client-unavailable :reason "The native method frame is invalid or exceeds its bounds."))

(-> application-client-wire--scalar-size (t) integer)
(defun application-client-wire--scalar-size (value)
  "Validate VALUE and measure its exact ordinary daemon-readable spelling."
  (unless (and (application-client-wire--scalar-p value)
               (typecase value
                 (string
                  (<= (length value) 262144))
                 (symbol
                  (and (<= (length (symbol-name value)) 262144)
                       (or (null (symbol-package value))
                           (<= (length (package-name (symbol-package value))) 262144))))
                 (pathname
                  (<= (length (namestring value)) 262144))
                 (t
                  t)))
    (application-client-wire--reject))
  (with-standard-io-syntax
    (length (write-to-string value :readably t :pretty nil :circle nil))))

(-> application-client-wire--hash-test (hash-table) keyword)
(defun application-client-wire--hash-test (table)
  "Return TABLE's portable standard hash test, rejecting user-defined tests."
  (let* ((test (hash-table-test table))
         (name (find-if (lambda (name) (or (eq test name) (eq test (symbol-function name))))
                        '(eq eql equal equalp))))
    (unless name (application-client-wire--reject))
    (intern (symbol-name name) :keyword)))

(-> application-client-wire-encode (t) list)
(defun application-client-wire-encode (value)
  "Encode a bounded native method value as an unambiguous readable tree.

Every container has an explicit representation, so user lists resembling tags
are data. Only registered method request arguments and reply results use this
codec. The daemon's ordinary frame-size limit also applies."
  (let ((characters 0))
    (handler-case
        (let ((encoded
                (portable-value-map
                 value
                 (lambda (item child)
                   (cond
                     ((hash-table-p item)
                      (list* :hash-table (application-client-wire--hash-test item)
                             (loop for key being the hash-keys of item using (hash-value member)
                                   append (list (funcall child key) (funcall child member)))))
                     ((and (vectorp item) (not (stringp item)))
                      (cons :vector (map 'list child item)))
                    ((application-client-wire--scalar-p item)
                     (when (> (incf characters (application-client-wire--scalar-size item)) 262144)
                       (application-client-wire--reject))
                     (list :atom item))
                    (t
                     (application-client-wire--reject))))
                 :containers-p t :proper-lists-p t
                 :list (lambda (items) (cons :list items))
                 :maximum-depth 32 :maximum-nodes 16384 :list-node-policy ':container)))
          (image-daemon:daemon-packet-string encoded)
          encoded)
      (portable-value-error () (application-client-wire--reject)))))

(-> application-client-wire-decode (t) t)
(defun application-client-wire-decode (encoded)
  "Decode one mandatory method-frame tree, rejecting unfamiliar tags or tests.

The raw tree is bounded to three nodes per native node and one extra depth
level for atom wrappers. Native values retain the 32-depth/16384-node bounds.
No reader evaluation is used."
  (let ((characters 0))
    (handler-case
        (let ((tree
                (portable-value-map
                 encoded
                 (lambda (item child)
                   (declare (ignore child))
                   (unless (application-client-wire--scalar-p item)
                     (application-client-wire--reject))
                  (when (and (stringp item) (> (length item) 262144))
                    (application-client-wire--reject))
                  item)
                 :proper-lists-p t :maximum-depth 33 :maximum-nodes 49152
                 :list-node-policy ':container)))
          (labels ((decode (node)
                     (unless (consp node) (application-client-wire--reject))
                     (case (first node)
                      (:atom
                       (unless (and (= (length node) 2)
                                    (application-client-wire--scalar-p (second node)))
                         (application-client-wire--reject))
                       (when (> (incf characters (application-client-wire--scalar-size (second node))) 262144)
                         (application-client-wire--reject))
                       (second node))
                      (:list
                       (mapcar #'decode (rest node)))
                      (:vector
                       (map 'vector #'decode (rest node)))
                      (:hash-table
                       (let* ((test    (case (second node)
                                         (:eq
                                          'eq)
                                         (:eql
                                          'eql)
                                         (:equal
                                          'equal)
                                         (:equalp
                                          'equalp)
                                         (otherwise
                                          (application-client-wire--reject))))
                              (members (nthcdr 2 node))
                              (table   (make-hash-table :test test)))
                         (unless (evenp (length members)) (application-client-wire--reject))
                         (loop for (key value) on members by (partial #'nthcdr 2)
                               for decoded-key = (decode key)
                               do (when (nth-value 1 (gethash decoded-key table))
                                    (application-client-wire--reject))
                                  (setf (gethash decoded-key table) (decode value)))
                         table))
                      (otherwise
                       (application-client-wire--reject)))))
            (application-client-value (decode tree))))
      (portable-value-error () (application-client-wire--reject)))))
