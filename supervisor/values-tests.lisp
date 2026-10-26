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

(declaim (ftype (function () null) protocol-test--message-data))

(defun protocol-test--message-data ()
  "Check schema projection, detached values, presence, and shared bounds."
  (let* ((usage-data (make-hash-table :test 'equal))
         (call       (cl-protobufs.autolith.parent.v1:make-tool-call
                      :call-id "call-A" :name "lookup" :arguments-json "{malformed"))
         (first      (cl-protobufs.autolith.parent.v1:make-output-item
                      :family "responses" :item-index 0 :tool-call call))
         (second     (cl-protobufs.autolith.parent.v1:make-output-item
                      :family "responses" :item-index 1 :tool-call
                      (cl-protobufs.autolith.parent.v1:make-tool-call
                       :call-id "call-B" :name "lookup" :arguments-json ""))))
    (setf (gethash "false" usage-data) nil
          (gethash "null" usage-data) ':protocol-null
          (gethash "bytes" usage-data) (protocol-test--bytes '(0 128 255))
          (gethash "signed_zero" usage-data) -0.0d0)
    (let* ((message   (cl-protobufs.autolith.parent.v1:make-model-complete
                       :reason ':tool-calls :turn-completion ':continued
                       :usage        (cl-protobufs.autolith.parent.v1:make-usage
                                      :input-tokens 0 :provider-usage (protocol-data->map usage-data))
                       :output-items (list first second)))
           (data      (protocol-message->data message))
           (usage     (gethash "usage" data))
           (originals (gethash "provider_usage" usage))
           (items     (gethash "output_items" data))
           (decoded   (cl-protobufs:deserialize-from-bytes
                       'cl-protobufs.autolith.parent.v1:model-complete
                       (cl-protobufs:serialize-to-bytes message))))
      (assert (string= "tool_calls" (gethash "reason" data)))
      (assert (string= "continued" (gethash "turn_completion" data)))
      (assert (= 0 (gethash "input_tokens" usage)))
      (assert (not (nth-value 1 (gethash "output_tokens" usage))))
      (assert (not (nth-value 1 (gethash "continuation_handle" data))))
      (assert (nth-value 1 (gethash "false" originals)))
      (assert (null (gethash "false" originals)))
      (assert (eq ':protocol-null (gethash "null" originals)))
      (assert (equalp (protocol-test--bytes '(0 128 255)) (gethash "bytes" originals)))
      (assert (minusp (float-sign (gethash "signed_zero" originals))))
      (assert (= 0 (gethash "item_index" (aref items 0))))
      (assert (equal '("call-A" "call-B")
                     (loop for item across items collect (gethash "call_id" (gethash "tool_call" item)))))
      (assert (equalp data (protocol-message->data decoded)))
      (setf (char (gethash "arguments_json" (gethash "tool_call" (aref items 0))) 0) #\X
            (aref (gethash "bytes" originals) 0) 42)
      (assert (string= "{malformed" (cl-protobufs.autolith.parent.v1:tool-call.arguments-json call)))
      (assert (= 0 (aref (gethash "bytes" (gethash "provider_usage"
                                                 (gethash "usage" (protocol-message->data message)))) 0))))
    (protocol-test--value-error
     (lambda () (protocol-message->data first :max-depth 2)) ':depth-limit)
    (let* ((items   (list first))
           (message (cl-protobufs.autolith.parent.v1:make-model-complete :output-items items)))
      (setf (rest items) items)
      (sb-ext:with-timeout 2
        (protocol-test--value-error
         (lambda () (protocol-message->data message :max-entries 64)) ':entry-limit))))
  (let ((data (protocol-message->data
               (cl-protobufs.autolith.parent.v1:make-turn-start :continuation nil))))
    (assert (nth-value 1 (gethash "continuation" data)))
    (assert (null (gethash "continuation" data))))
  (let ((data (protocol-message->data (cl-protobufs.autolith.parent.v1:make-model-complete))))
    (assert (equalp #() (gethash "output_items" data))))
  (protocol-test--value-error
   (lambda () (protocol-message->data
               (cl-protobufs.autolith.parent.v1:make-model-event :index 1 :attempt 1)
               :max-entries 1)) ':entry-limit)
  (protocol-test--value-error
   (lambda () (protocol-message->data
               (cl-protobufs.autolith.parent.v1:make-usage :input-tokens (expt 2 63))))
   ':unsupported-value)
  (protocol-test--value-error (lambda () (protocol-message->data ':not-a-message)) ':invalid-message)
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
  (protocol-test--message-data)
  nil)
