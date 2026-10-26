;;;; -- Protobuf Decode Safety Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (sequence) (simple-array (unsigned-byte 8) (*)))
                protobuf-test--bytes))

(defun protobuf-test--bytes (bytes)
  "Copy test BYTES to an octet vector."
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(declaim (ftype (function ((unsigned-byte 64)) list) protobuf-test--varint))

(defun protobuf-test--varint (value)
  "Encode VALUE as a canonical varint for independent wire fixtures."
  (loop while (> value 127)
        collect (logior 128 (logand value 127)) into bytes
        do (setf value (ash value -7))
        finally (return (nconc bytes (list value)))))

(declaim (ftype (function (integer list) list) protobuf-test--delimited))

(defun protobuf-test--delimited (number bytes)
  "Wrap BYTES in length-delimited field NUMBER."
  (append (protobuf-test--varint (logior (ash number 3) 2))
          (protobuf-test--varint (length bytes)) bytes))

(declaim (ftype (function (list keyword &key (:type symbol) (:max-depth integer)
                                           (:max-entries integer) (:max-bytes integer)) null)
                protobuf-test--reject))

(defun protobuf-test--reject (bytes reason
                              &key (type 'cl-protobufs.autolith.parent.v1:envelope)
                                  (max-depth 32) (max-entries 65536) (max-bytes 8388608))
  "Require the redacted rejection REASON for BYTES and TYPE under the limits."
  (assert (eq reason
              (handler-case
                  (progn
                    (protocol-validate-protobuf (protobuf-test--bytes bytes) type
                                                :max-depth max-depth
                                                :max-entries max-entries :max-bytes max-bytes)
                    nil)
                (protocol-decode-error (condition)
                  (protocol-decode-error-reason condition)))))
  nil)

(declaim (ftype (function () null) protobuf-test--descriptor-bounds))

(defun protobuf-test--descriptor-bounds ()
  "Check packed fields and wire nesting with isolated test descriptors."
  (let* ((node (gensym "PROTOBUF-TEST-NODE"))
         (descriptor
           (make-instance
            'cl-protobufs:message-descriptor
            :class  node
            :fields (list (make-instance 'cl-protobufs:field-descriptor
                                         :index 1 :type node :kind ':message :label ':optional)
                          (make-instance 'cl-protobufs:field-descriptor
                                         :index 2 :type 'cl-protobufs:uint64
                                         :kind ':scalar :label ':repeated)
                          (make-instance 'cl-protobufs:field-descriptor
                                         :index 3 :type 'cl-protobufs:fixed32
                                         :kind ':scalar :label ':repeated)
                          (make-instance 'cl-protobufs:field-descriptor
                                         :index 4 :type 'cl-protobufs.autolith.common.v1::null
                                         :kind ':enum :label ':repeated))))
         ;; Isolate the runtime registry. No global schema mutation survives this check.
         (cl-protobufs.implementation::*messages*
           (make-hash-table :test #'eq)))
    (setf (gethash node cl-protobufs.implementation::*messages*) descriptor)
    (dolist (bytes '((18 0) (18 3 0 1 2) (16 0 18 2 1 2)
                     (26 4 0 0 0 0) (34 2 0 0)))
      (assert (null (protocol-validate-protobuf (protobuf-test--bytes bytes) node))))
    (dolist (case '(((18 1 128) :truncated-varint)
                    ((18 2 128 0) :overlong-varint)
                    ((26 3 0 0 0) :length-overrun)
                    ((34 1 1) :unknown-enum)))
      (protobuf-test--reject (first case) (second case) :type node))
    (protobuf-test--reject '(18 3 0 1 2) ':field-limit :type node :max-entries 3)
    (assert (null (protocol-validate-protobuf (protobuf-test--bytes '(18 3 0 1 2)) node
                                             :max-entries 4)))
    (let ((bytes nil))
      (dotimes (index 127)
        (setf bytes (protobuf-test--delimited 1 bytes)))
      (assert (null (protocol-validate-protobuf (protobuf-test--bytes bytes) node)))
      (protobuf-test--reject (protobuf-test--delimited 1 bytes) ':wire-depth-limit :type node)))
  nil)

(declaim (ftype (function () (integer 0)) protobuf-test--schema-fields))

(defun protobuf-test--schema-fields ()
  "Check each scalar and enum field reachable from the current Envelope schema.
Return the number of checked fields. Include all ordinary and oneof fields."
  (let ((pending (list 'cl-protobufs.autolith.parent.v1:envelope))
        (seen    (make-hash-table :test #'eq))
        (count   0))
    (loop while pending
          for type = (pop pending)
          unless (gethash type seen)
            do (setf (gethash type seen) t)
               (let* ((descriptor (cl-protobufs:find-message-descriptor type))
                      (fields     (append
                                   (cl-protobufs:proto-fields descriptor)
                                   (loop for oneof in (cl-protobufs:proto-oneofs descriptor)
                                         append (coerce (cl-protobufs:oneof-descriptor-fields oneof)
                                                        'list)))))
                 (dolist (field fields)
                   (if (eq (cl-protobufs:proto-kind field) ':message)
                       (push (cl-protobufs:proto-type field) pending)
                       (let* ((scalar  (cl-protobufs:proto-type field))
                              (wire    (cond
                                         ((eq (cl-protobufs:proto-kind field) ':enum)
                                          0)
                                         ((member scalar '(string cl-protobufs:byte-vector))
                                          2)
                                         ((member scalar '(double-float cl-protobufs:fixed64
                                                           cl-protobufs:sfixed64))
                                          1)
                                         ((member scalar '(single-float cl-protobufs:fixed32
                                                           cl-protobufs:sfixed32))
                                          5)
                                         (t
                                          0)))
                              (payload (make-list (case wire
                                                    (1
                                                     8)
                                                    (5
                                                     4)
                                                    (otherwise
                                                     1))
                                                  :initial-element 0))
                              (tag     (protobuf-test--varint
                                        (logior (ash (cl-protobufs:proto-index field) 3) wire))))
                         (incf count)
                         (assert (null (protocol-validate-protobuf
                                        (protobuf-test--bytes (append tag payload)) type)))
                         (when (eq (cl-protobufs:proto-kind field) ':enum)
                           (protobuf-test--reject
                            (append tag (protobuf-test--varint 65535)) ':unknown-enum :type type)))))))
    count))

(declaim (ftype (function () null) run-protobuf-validation-tests))

(defun run-protobuf-validation-tests ()
  "Check preflight bounds and schema-aware rejection before deserialization."
  (protobuf-test--descriptor-bounds)
  (assert (plusp (protobuf-test--schema-fields)))
  (let ((envelope 'cl-protobufs.autolith.parent.v1:envelope)
        (value    'cl-protobufs.autolith.common.v1:value)
        (hello    'cl-protobufs.autolith.parent.v1:hello))
    (dolist (case '((nil :empty-frame)
                    ((0) :invalid-field-number)
                    ((1 0 0 0 0 0 0 0 0) :invalid-field-number)
                    ((11) :invalid-wire-type)
                    ((12) :invalid-wire-type)
                    ((14) :invalid-wire-type)
                    ((15) :invalid-wire-type)
                    ((10 0) :wrong-wire-type)
                    ((16 0) :wrong-wire-type)
                    ((8) :truncated-varint)
                    ((128) :truncated-varint)
                    ((128 0) :overlong-varint)
                    ((8 128 0) :overlong-varint)
                    ((18 128 0) :overlong-varint)
                    ((18 5 65) :length-overrun)
                    ((18 1 255) :invalid-utf8)
                    ((18 255 255 255 255 255 255 255 255 255 1) :length-overrun)))
      (protobuf-test--reject (first case) (second case)))
    (protobuf-test--reject (protobuf-test--varint (ash #x20000000 3)) ':invalid-field-number)
    (dolist (last '(2 128 255))
      (protobuf-test--reject (append '(8) (make-list 9 :initial-element 255) (list last))
                             ':invalid-varint))
    ;; The largest uint64 remains valid. Unknown fields use the same varint reader.
    (assert (null (protocol-validate-protobuf
                   (protobuf-test--bytes (append '(24) (make-list 9 :initial-element 255) '(1)))
                   envelope)))
    (protobuf-test--reject (append '(40) (make-list 9 :initial-element 128) '(0))
                           ':overlong-varint)
    ;; Known fixed-width types and unknown fixed-width skips cannot cross a boundary.
    (dolist (bytes '((33) (33 0 0 0 0 0 0 0)))
      (protobuf-test--reject bytes ':length-overrun :type value))
    (dolist (bytes '((41) (41 0 0 0 0 0 0 0) (45 0 0 0) (42 3 1 2)))
      (protobuf-test--reject bytes ':length-overrun))
    ;; Unknown strings are opaque bytes. Known strings require strict UTF-8.
    (dolist (bytes '((128) (191) (192 128) (193 191) (194) (194 32)
                     (224 128 128) (237 160 128) (239 191)
                     (240 128 128 128) (244 144 128 128) (245 128 128 128)
                     (255) (226 130)))
      (protobuf-test--reject (protobuf-test--delimited 2 bytes) ':invalid-utf8)
      (assert (null (protocol-validate-protobuf
                     (protobuf-test--bytes (protobuf-test--delimited 5 bytes)) envelope))))
    (dolist (bytes '((0) (127) (194 128) (223 191) (224 160 128) (237 159 191)
                     (238 128 128) (239 191 191) (240 144 128 128) (244 143 191 191)))
      (assert (null (protocol-validate-protobuf
                     (protobuf-test--bytes (protobuf-test--delimited 2 bytes)) envelope))))
    (assert (null (protocol-validate-protobuf
                   (protobuf-test--bytes '(40 1 41 0 0 0 0 0 0 0 0 42 2 255 255 45 0 0 0 0))
                   envelope)))
    ;; A nested invalid enum inside Value.kind must not escape oneof traversal.
    (protobuf-test--reject '(8 1) ':unknown-enum :type value)
    (protobuf-test--reject '(58 4 10 2 8 1) ':unknown-enum :type value)
    (protobuf-test--reject '(58 2 10 1) ':length-overrun :type value)
    (protobuf-test--reject '(56 0) ':wrong-wire-type :type value)
    ;; Duplicate singular fields, oneofs, and synthetic optional oneofs are legal.
    (assert (null (protocol-validate-protobuf (protobuf-test--bytes '(8 1 8 2 32 1 32 2)) envelope)))
    (assert (null (protocol-validate-protobuf (protobuf-test--bytes '(8 0 16 1 42 1 65)) value)))
    (protobuf-test--reject '(8 1 8 2) ':field-limit :max-entries 1)
    (protobuf-test--reject '(40 1 40 2) ':field-limit :max-entries 1)
    (protobuf-test--reject '(8 0 16 1) ':field-limit :max-entries 1 :type value)
    (protobuf-test--reject '(10 0 10 0) ':field-limit :max-entries 1 :type hello)
    ;; Count all entries in all nested messages under one budget.
    (protobuf-test--reject '(58 4 10 2 8 0) ':field-limit :type value :max-entries 2)
    (assert (null (protocol-validate-protobuf (protobuf-test--bytes '(58 4 10 2 8 0)) value
                                             :max-entries 3)))
    (let ((bytes '(8 0)))
      ;; A list wrapper adds wire depth, but only its Value adds logical depth.
      (dotimes (index 31)
        (setf bytes (protobuf-test--delimited 7 (protobuf-test--delimited 1 bytes))))
      (assert (null (protocol-validate-protobuf (protobuf-test--bytes bytes) value)))
      (protobuf-test--reject bytes ':depth-limit :type value :max-depth 31)
      (assert (null (protocol-validate-protobuf
                     (protobuf-test--bytes (protobuf-test--delimited 1 bytes))
                     'cl-protobufs.autolith.common.v1:value-list)))
      (protobuf-test--reject (protobuf-test--delimited 7 (protobuf-test--delimited 1 bytes))
                             ':depth-limit :type value))
    (let ((bytes '(8 0)))
      ;; Map and entry wrappers do not reduce the permitted logical depth.
      (dotimes (index 31)
        (setf bytes (protobuf-test--delimited
                     8 (protobuf-test--delimited 1 (protobuf-test--delimited 2 bytes)))))
      (assert (null (protocol-validate-protobuf (protobuf-test--bytes bytes) value)))
      (protobuf-test--reject bytes ':depth-limit :type value :max-depth 31))
    ;; Inclusive breadth and frame bounds, including hard limits.
    (let ((bytes (make-array (* 2 65536) :element-type '(unsigned-byte 8))))
      (loop for index below (length bytes) by 2
            do (setf (aref bytes index) 40 (aref bytes (1+ index)) 0))
      (assert (null (protocol-validate-protobuf bytes envelope)))
      (protobuf-test--reject (append (coerce bytes 'list) '(40 0)) ':field-limit))
    (protobuf-test--reject '(8 1) ':frame-too-large :max-bytes 1)
    (protobuf-test--reject '(8 1) ':invalid-limit :max-bytes 8388609)
    (protobuf-test--reject '(8 1) ':invalid-limit :max-depth 33)
    (protobuf-test--reject '(8 1) ':invalid-limit :max-entries 65537)
    (dolist (limit '(0 -1))
      (protobuf-test--reject '(8 1) ':invalid-limit :max-bytes limit)
      (protobuf-test--reject '(8 1) ':invalid-limit :max-depth limit)
      (protobuf-test--reject '(8 1) ':invalid-limit :max-entries limit))
    (let* ((prefix (append '(42) (protobuf-test--varint (- 8388608 5))))
           (bytes  (make-array 8388608 :element-type '(unsigned-byte 8) :initial-element 0)))
      (assert (= 5 (length prefix)))
      (replace bytes prefix)
      (assert (null (protocol-validate-protobuf bytes envelope))))
    (let ((bytes (make-array 8388609 :element-type '(unsigned-byte 8) :initial-element 0)))
      (assert (eq ':frame-too-large
                  (handler-case (protocol-validate-protobuf bytes envelope)
                    (protocol-decode-error (condition) (protocol-decode-error-reason condition))))))
    ;; Valid schema-generated serialization must pass and preserve all fields.
    (dolist (message
              (list (cl-protobufs.autolith.parent.v1:make-envelope
                     :protocol-major 1 :instance-id "instance" :message-id 1 :reply-to 2
                     :hello (cl-protobufs.autolith.parent.v1:make-hello
                             :features (list "session.v1" "model.v1")))
                    (cl-protobufs.autolith.parent.v1:make-envelope
                     :protocol-major 1 :instance-id "instance-test" :message-id 1 :reply-to 1
                     :failure (cl-protobufs.autolith.parent.v1:make-failure
                               :code ':invalid-request :message "Invalid startup message."))))
      (let ((bytes (cl-protobufs:serialize-to-bytes message)))
        (assert (null (protocol-validate-protobuf bytes envelope)))
        (assert (cl-protobufs:proto-equal message
                                        (cl-protobufs:deserialize-from-bytes envelope bytes)))))
    ;; Every cut inside one complete outer message must fail.
    (let* ((field
             (loop for oneof in (cl-protobufs:proto-oneofs
                                 (cl-protobufs:find-message-descriptor envelope))
                   thereis (find 'cl-protobufs.autolith.parent.v1:hello
                                 (cl-protobufs:oneof-descriptor-fields oneof)
                                 :key #'cl-protobufs:proto-type)))
           (bytes (protobuf-test--bytes
                   (protobuf-test--delimited (cl-protobufs:proto-index field)
                                             (protobuf-test--delimited 1 '(65 66 67))))))
      (assert (null (protocol-validate-protobuf bytes envelope)))
      (loop for end from 1 below (length bytes)
            do (assert (handler-case
                           (progn (protocol-validate-protobuf (subseq bytes 0 end) envelope) nil)
                         (protocol-decode-error () t))))))
  nil)
