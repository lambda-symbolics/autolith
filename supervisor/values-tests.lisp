;;;; -- Portable Protocol Value Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (function keyword) null) protocol-test--value-error))

(defun protocol-test--value-error (function reason)
  "Require FUNCTION to reject a value with the redacted REASON."
  (assert (eq reason
              (handler-case (progn (funcall function) nil)
                (protocol-value-error (condition)
                  (protocol-value-error-reason condition)))))
  nil)

(declaim (ftype (function () null) run-protocol-value-tests))

(defun run-protocol-value-tests ()
  "Check portable values, unambiguous null/false/empty containers, and malformed values."
  (dolist (data (list t nil ':protocol-null (- (expt 2 63)) (1- (expt 2 63))
                     0 1.5d0 -0.0d0 "" "λ" #() #(nil t :protocol-null)
                     (protocol-test--bytes nil) (protocol-test--bytes '(0 127 255))
                     (make-hash-table :test #'equal)))
    (let* ((encoded (protocol-data->value data))
           (bytes   (cl-protobufs:serialize-to-bytes encoded))
           (decoded (cl-protobufs:deserialize-from-bytes 'common-proto:value bytes))
           (result  (protocol-value->data decoded)))
      (assert (equalp data result))
      (assert (eq (null data) (null result)))
      (when (and (arrayp data) (not (stringp data)))
        (assert (equal (array-element-type data) (array-element-type result))))))
  (let ((first  (make-hash-table :test #'equal))
        (second (make-hash-table :test #'equal)))
    (setf (gethash "b" first) nil (gethash "a" first) ':protocol-null
          (gethash "a" second) ':protocol-null (gethash "b" second) nil)
    (assert (equalp (cl-protobufs:serialize-to-bytes (protocol-data->value first))
                    (cl-protobufs:serialize-to-bytes (protocol-data->value second))))
    (assert (equalp first (protocol-map->data (protocol-data->map first)))))
  (protocol-test--value-error
   (lambda () (protocol-value->data (common-proto:make-value))) ':missing-kind)
  (protocol-test--value-error
   (lambda () (protocol-value->data
               (common-proto:make-value
                :map-value (common-proto:make-value-map
                            :entries (list (common-proto:make-value-entry :key "x"))))))
   ':missing-value)
  (protocol-test--value-error
   (lambda () (protocol-map->data
               (common-proto:make-value-map
                :entries (loop repeat 2 collect
                          (common-proto:make-value-entry :key   "x"
                                                         :value (protocol-data->value nil))))))
   ':duplicate-key)
  (dolist (high-byte '(240 248))
    (let ((value (cl-protobufs:deserialize-from-bytes
                  'common-proto:value (protocol-test--bytes (list 33 0 0 0 0 0 0 high-byte 127)))))
      (protocol-test--value-error (lambda () (protocol-value->data value)) ':non-finite-number)
      (protocol-test--value-error
       (lambda () (protocol-data->value (common-proto:value.number-value value)))
       ':non-finite-number)))
  (dolist (data (list (expt 2 63) 1/2 1.0f0 'symbol '(1 2)))
    (protocol-test--value-error (lambda () (protocol-data->value data)) ':unsupported-value))
  (let ((map (make-hash-table :test #'equal)))
    (setf (gethash ':interned-key map) "value")
    (protocol-test--value-error (lambda () (protocol-data->value map)) ':invalid-key))
  (let ((map (make-hash-table :test #'eq)))
    (setf (gethash (copy-seq "x") map) nil (gethash (copy-seq "x") map) t)
    (protocol-test--value-error (lambda () (protocol-data->value map)) ':duplicate-key))
  (let ((data "leaf"))
    (dotimes (index 31)
      (declare (ignorable index))
      (setf data (vector data)))
    (assert (equalp data (protocol-value->data (protocol-data->value data))))
    (let ((value (protocol-data->value data)))
      (protocol-test--value-error
       (lambda () (protocol-value->data value :max-depth 31)) ':depth-limit))
    (protocol-test--value-error (lambda () (protocol-data->value (vector data))) ':depth-limit))
  (let ((value (protocol-data->value #(nil t))))
    (assert (equalp #(nil t) (protocol-value->data value :max-entries 2)))
    (protocol-test--value-error
     (lambda () (protocol-value->data value :max-entries 1)) ':entry-limit))
  (protocol-test--value-error
   (lambda () (protocol-data->value #(nil t) :max-entries 1)) ':entry-limit)
  (let ((cycle (vector nil)))
    (setf (aref cycle 0) cycle)
    (protocol-test--value-error (lambda () (protocol-data->value cycle)) ':depth-limit))
  nil)
