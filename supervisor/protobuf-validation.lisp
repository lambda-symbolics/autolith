;;;; -- Bounded Protobuf Decode Preflight --

(in-package #:autolith-supervisor)

(define-condition protocol-decode-error (error)
  ((reason :initarg       :reason
           :reader        protocol-decode-error-reason
           :documentation "A fixed, content-free rejection code."))
  (:documentation "An incoming Protobuf frame failed decode preflight.")
  (:report (lambda (condition stream)
             (format stream "Invalid protocol Protobuf: ~A"
                     (protocol-decode-error-reason condition)))))

(declaim (ftype (function (keyword) nil) protocol-protobuf--reject))

(defun protocol-protobuf--reject (reason)
  "Reject input with a fixed code, without including input bytes."
  (error 'protocol-decode-error :reason reason))

(declaim (ftype (function ((array (unsigned-byte 8) (*)) fixnum fixnum) null)
                protocol-protobuf--utf8))

(defun protocol-protobuf--utf8 (bytes start end)
  "Check strict UTF-8 in BYTES between START and END without allocating text."
  (let ((position start))
    (labels ((continuation (low high)
               (unless (< position end)
                 (protocol-protobuf--reject ':invalid-utf8))
               (let ((byte (aref bytes position)))
                 (incf position)
                 (unless (<= low byte high)
                   (protocol-protobuf--reject ':invalid-utf8)))))
      (loop while (< position end)
            for byte = (aref bytes position)
            do (incf position)
               (cond
                 ((<= byte #x7f)
                  nil)
                 ((<= #xc2 byte #xdf)
                  (continuation #x80 #xbf))
                 ((<= #xe0 byte #xef)
                  (continuation (if (= byte #xe0) #xa0 #x80)
                                (if (= byte #xed) #x9f #xbf))
                  (continuation #x80 #xbf))
                 ((<= #xf0 byte #xf4)
                  (continuation (if (= byte #xf0) #x90 #x80)
                                (if (= byte #xf4) #x8f #xbf))
                  (continuation #x80 #xbf)
                  (continuation #x80 #xbf))
                 (t
                  (protocol-protobuf--reject ':invalid-utf8))))))
  nil)

(declaim (ftype (function (cl-protobufs:field-descriptor) (integer 0 5))
                protocol-protobuf--wire-type))

(defun protocol-protobuf--wire-type (field)
  "Return the unpacked wire type for the trusted schema FIELD."
  (case (cl-protobufs:proto-kind field)
    (:message
     2)
    (:enum
     0)
    (:scalar
     (let ((type (cl-protobufs:proto-type field)))
       (cond
         ((member type '(string cl-protobufs:byte-vector))
          2)
         ((member type '(double-float cl-protobufs:fixed64 cl-protobufs:sfixed64))
          1)
         ((member type '(single-float cl-protobufs:fixed32 cl-protobufs:sfixed32))
          5)
         ((member type '(boolean cl-protobufs:int32 cl-protobufs:int64
                         cl-protobufs:uint32 cl-protobufs:uint64
                         cl-protobufs:sint32 cl-protobufs:sint64))
          0)
         (t
          (protocol-protobuf--reject ':unsupported-schema)))))
    (otherwise
     (protocol-protobuf--reject ':unsupported-schema))))

(declaim (ftype (function ((array (unsigned-byte 8) (*)) cl-protobufs:message-descriptor
                           &key (:frame-limit integer) (:depth-limit integer)
                           (:field-limit integer)) boolean)
                protocol-protobuf-preflight))

(defun protocol-protobuf-preflight (bytes descriptor
                                    &key (frame-limit (* 8 1024 1024))
                                        (depth-limit 32) (field-limit 65536))
  "Check BYTES against DESCRIPTOR before calling the Protobuf deserializer.
Bounds may only lower the hard limits. Limit wire message depth to 128, with
root depth one. DEPTH-LIMIT bounds logical Value nesting, at most 32: increment
only on entry to a Value message, not its list, map, entry, or envelope wrappers.
Count every wire field, including unknown and overwritten fields, and every
packed element. Unknown length-delimited data is opaque: check its length, not
its contents. This function checks decode safety, not authority or required
semantic fields. Return T on success. Signal PROTOCOL-DECODE-ERROR on invalid
input. Tables are local to this call and built once per descriptor."
  (unless (and (<= 1 frame-limit (* 8 1024 1024))
               (<= 1 depth-limit 32) (<= 1 field-limit 65536))
    (protocol-protobuf--reject ':invalid-limit))
  (when (zerop (length bytes))
    (protocol-protobuf--reject ':empty-frame))
  (when (> (length bytes) frame-limit)
    (protocol-protobuf--reject ':frame-too-large))
  (let ((position 0)
        (count    0)
        (messages (make-hash-table :test #'eq))
        (enums    (make-hash-table :test #'eq)))
    (labels ((charge ()
               (when (> (incf count) field-limit)
                 (protocol-protobuf--reject ':field-limit)))

             (varint (end)
               (loop with value = 0
                     for index below 10
                     do (when (>= position end)
                          (protocol-protobuf--reject ':truncated-varint))
                        (let ((byte (aref bytes position)))
                          (incf position)
                          (when (and (= index 9) (> byte 1))
                            (protocol-protobuf--reject ':invalid-varint))
                          (setf value (logior value (ash (logand byte #x7f) (* index 7))))
                          (when (< byte #x80)
                            (when (and (plusp index) (zerop byte))
                              (protocol-protobuf--reject ':overlong-varint))
                            (return-from varint value)))
                     finally (protocol-protobuf--reject ':invalid-varint)))

             (advance (size end)
               (when (> size (- end position))
                 (protocol-protobuf--reject ':length-overrun))
               (incf position size))

             (fields (message)
               (or (gethash message messages)
                   (let ((table (make-hash-table :test #'eql)))
                     (dolist (field (cl-protobufs:proto-fields message))
                       (setf (gethash (cl-protobufs:proto-index field) table) field))
                     (dolist (oneof (cl-protobufs:proto-oneofs message))
                       (loop for field across (cl-protobufs:oneof-descriptor-fields oneof)
                             do (setf (gethash (cl-protobufs:proto-index field) table) field)))
                     (setf (gethash message messages) table))))

             (enum-value (field value)
               (let* ((type (cl-protobufs:proto-type field))
                      (table
                        (or (gethash type enums)
                            (let ((table (make-hash-table :test #'eql))
                                  ;; Ordinary fields use an open enum type. Their class
                                  ;; names the registered closed enum; oneofs may not.
                                  (enum  (or (cl-protobufs:find-enum-descriptor type)
                                             (cl-protobufs:find-enum-descriptor
                                              (cl-protobufs:proto-class field)))))
                              (unless enum
                                (protocol-protobuf--reject ':unsupported-schema))
                              (dolist (entry (cl-protobufs:enum-descriptor-values enum))
                                (setf (gethash
                                       (cl-protobufs.implementation::enum-value-descriptor-value entry)
                                       table)
                                      t))
                              (setf (gethash type enums) table)))))
                 ;; Negative enum values use sign-extended uint64 wire values.
                 (when (logbitp 63 value)
                   (decf value (ash 1 64)))
                 (unless (gethash value table)
                   (protocol-protobuf--reject ':unknown-enum))))

             (scalar (field wire end)
               (case wire
                 (0
                  (let ((value (varint end)))
                    (when (and field (eq (cl-protobufs:proto-kind field) ':enum))
                      (enum-value field value))))
                 (1
                  (advance 8 end))
                 (5
                  (advance 4 end))))

             (message (schema end &key depth value-depth)
               (when (> depth 128)
                 (protocol-protobuf--reject ':wire-depth-limit))
               (when (eq (cl-protobufs:proto-class schema)
                         'cl-protobufs.autolith.common.v1:value)
                 (when (> (incf value-depth) depth-limit)
                   (protocol-protobuf--reject ':depth-limit)))
               (let ((table (fields schema)))
                 (loop while (< position end)
                       do (charge)
                          (let* ((tag    (varint end))
                                 (number (ash tag -3))
                                 (wire   (logand tag 7))
                                 (field  (gethash number table)))
                            (unless (<= 1 number #x1fffffff)
                              (protocol-protobuf--reject ':invalid-field-number))
                            (unless (member wire '(0 1 2 5))
                              (protocol-protobuf--reject ':invalid-wire-type))
                            (let* ((expected (and field (protocol-protobuf--wire-type field)))
                                   (packed   (and field (= wire 2) (/= expected 2)
                                                  (eq (cl-protobufs:proto-label field) ':repeated))))
                              (when (and field (not packed) (/= wire expected))
                                (protocol-protobuf--reject ':wrong-wire-type))
                              (if (/= wire 2)
                                  (scalar field wire end)
                                  (let* ((size  (varint end))
                                         (start position)
                                         (limit (+ start size)))
                                    (when (> size (- end start))
                                      (protocol-protobuf--reject ':length-overrun))
                                    (cond
                                      (packed
                                       (loop while (< position limit)
                                             do (charge) (scalar field expected limit)))
                                      ((and field (eq (cl-protobufs:proto-kind field) ':message))
                                       (let ((nested (cl-protobufs:find-message-descriptor
                                                      (cl-protobufs:proto-type field))))
                                         (unless nested
                                           (protocol-protobuf--reject ':unsupported-schema))
                                         (message nested limit :depth       (1+ depth)
                                                               :value-depth value-depth)))
                                      (t
                                       (when (and field (eq (cl-protobufs:proto-type field) 'string))
                                         (protocol-protobuf--utf8 bytes start limit))
                                       (setf position limit)))))))))))
      (message descriptor (length bytes) :depth 1 :value-depth 0))
    t))

(declaim (ftype (function ((array (unsigned-byte 8) (*)) symbol
                           &key (:max-depth integer) (:max-entries integer)
                           (:max-bytes integer)) null)
                protocol-validate-protobuf))

(defun protocol-validate-protobuf (bytes message-type
                                   &key (max-depth 32) (max-entries 65536)
                                        (max-bytes 8388608))
  "Validate BYTES for MESSAGE-TYPE before deserialization. Return NIL on success.
The positive limits can lower, but cannot raise, the hard decode bounds.
Signal PROTOCOL-DECODE-ERROR with a redacted reason on invalid input."
  (let ((descriptor (cl-protobufs:find-message-descriptor message-type)))
    (unless descriptor
      (protocol-protobuf--reject ':unsupported-schema))
    (protocol-protobuf-preflight bytes descriptor :frame-limit max-bytes
                                                 :depth-limit  max-depth
                                                 :field-limit  max-entries))
  nil)
