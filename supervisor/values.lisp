;;;; -- Portable Protocol Values --

(in-package #:autolith-supervisor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:common-proto '#:cl-protobufs.autolith.common.v1
                                     '#:autolith-supervisor))

(define-condition protocol-value-error (error)
  ((reason :initarg       :reason
           :reader        protocol-value-error-reason
           :documentation "The invalid value category, without private value contents."))
  (:documentation "A nonportable, ambiguous, or oversized protocol value.")
  (:report (lambda (condition stream)
             (format stream "Invalid protocol value: ~a." (protocol-value-error-reason condition)))))

(declaim (ftype (function (common-proto:value &key (:max-depth (integer 1))
                                                (:max-entries (integer 1))) t)
                protocol-value->data))

(defun protocol-value->data (value &key (max-depth 32) (max-entries 65536))
  "Validate VALUE and return fresh portable data without interning keys.
Use :PROTOCOL-NULL for null, T/NIL for booleans, simple vectors for lists, octet
vectors for bytes, and EQUAL hash tables for string-keyed maps. Bound depth and
total collection entries, including values built directly rather than decoded."
  (unless (and (<= 1 max-depth 32) (<= 1 max-entries 65536))
    (error 'protocol-value-error :reason ':invalid-limit))
  (let ((entries 0))
    (labels ((count-entry ()
               (when (> (incf entries) max-entries)
                 (error 'protocol-value-error :reason ':entry-limit)))

             (walk (item depth)
               (when (> depth max-depth)
                 (error 'protocol-value-error :reason ':depth-limit))
               (unless (typep item 'common-proto:value)
                 (error 'protocol-value-error :reason ':missing-value))
               (case (common-proto:value.kind-case item)
                 (common-proto:null-value
                  (unless (eq ':null-value (common-proto:value.null-value item))
                    (error 'protocol-value-error :reason ':unknown-null))
                  ':protocol-null)
                 (common-proto:boolean-value
                  (if (common-proto:value.boolean-value item) t nil))
                 (common-proto:integer-value
                  (common-proto:value.integer-value item))
                 (common-proto:number-value
                  (let ((number (common-proto:value.number-value item)))
                    (when (or (sb-ext:float-nan-p number) (sb-ext:float-infinity-p number))
                      (error 'protocol-value-error :reason ':non-finite-number))
                    number))
                 (common-proto:string-value
                  (copy-seq (common-proto:value.string-value item)))
                 (common-proto:bytes-value
                  (copy-seq (common-proto:value.bytes-value item)))
                 (common-proto:list-value
                  (let ((items (make-array 0 :adjustable t :fill-pointer 0)))
                    (dolist (child (common-proto:value-list.values
                                    (common-proto:value.list-value item)))
                      (count-entry)
                      (vector-push-extend (walk child (1+ depth)) items))
                    (coerce items 'simple-vector)))
                 (common-proto:map-value
                  (let ((map (make-hash-table :test #'equal)))
                    (dolist (entry (common-proto:value-map.entries (common-proto:value.map-value item)))
                      (count-entry)
                      (let ((key (common-proto:value-entry.key entry)))
                        (when (nth-value 1 (gethash key map))
                          (error 'protocol-value-error :reason ':duplicate-key))
                        (setf (gethash (copy-seq key) map)
                              (walk (common-proto:value-entry.value entry) (1+ depth)))))
                    map))
                 (otherwise
                  (error 'protocol-value-error :reason ':missing-kind)))))
      (walk value 1))))

(declaim (ftype (function (t &key (:max-depth (integer 1)) (:max-entries (integer 1)))
                         common-proto:value)
                protocol-data->value))

(defun protocol-data->value (data &key (max-depth 32) (max-entries 65536))
  "Encode DATA in the representation of PROTOCOL-VALUE->DATA, sorting map keys.
Reject unsupported types, duplicate string keys, nonfinite floats, and bounds."
  (unless (and (<= 1 max-depth 32) (<= 1 max-entries 65536))
    (error 'protocol-value-error :reason ':invalid-limit))
  (let ((entries 0))
    (labels ((count-entry ()
               (when (> (incf entries) max-entries)
                 (error 'protocol-value-error :reason ':entry-limit)))

             (walk (item depth)
               (when (> depth max-depth)
                 (error 'protocol-value-error :reason ':depth-limit))
               (cond
                 ((eq item ':protocol-null)
                  (common-proto:make-value :null-value ':null-value))
                 ((or (eq item t) (null item))
                  (common-proto:make-value :boolean-value item))
                 ((typep item '(signed-byte 64))
                  (common-proto:make-value :integer-value item))
                 ((typep item 'double-float)
                  (when (or (sb-ext:float-nan-p item) (sb-ext:float-infinity-p item))
                    (error 'protocol-value-error :reason ':non-finite-number))
                  (common-proto:make-value :number-value item))
                 ((stringp item)
                  (common-proto:make-value :string-value (copy-seq item)))
                 ((typep item '(simple-array (unsigned-byte 8) (*)))
                  (common-proto:make-value :bytes-value (copy-seq item)))
                 ((simple-vector-p item)
                  (common-proto:make-value
                   :list-value (common-proto:make-value-list
                                :values (loop for child across item
                                              do (count-entry)
                                              collect (walk child (1+ depth))))))
                 ((hash-table-p item)
                  (let ((keys nil)
                        (seen (make-hash-table :test #'equal)))
                    (maphash (lambda (key child)
                               (declare (ignore child))
                               (count-entry)
                               (unless (stringp key)
                                 (error 'protocol-value-error :reason ':invalid-key))
                               (when (gethash key seen)
                                 (error 'protocol-value-error :reason ':duplicate-key))
                               (setf (gethash key seen) t)
                               (push key keys))
                             item)
                    (common-proto:make-value
                     :map-value (common-proto:make-value-map
                                 :entries (loop for key in (sort keys #'string<)
                                                collect (common-proto:make-value-entry
                                                         :key   (copy-seq key)
                                                         :value (walk (gethash key item)
                                                                      (1+ depth))))))))
                 (t
                  (error 'protocol-value-error :reason ':unsupported-value)))))
      (walk data 1))))

(declaim (ftype (function (common-proto:value-map &key (:max-depth (integer 1))
                                                    (:max-entries (integer 1))) hash-table)
                protocol-map->data))

(defun protocol-map->data (map &key (max-depth 32) (max-entries 65536))
  "Validate and convert MAP using the shared portable value limits."
  (protocol-value->data (common-proto:make-value :map-value map)
                       :max-depth max-depth :max-entries max-entries))

(declaim (ftype (function (hash-table &key (:max-depth (integer 1)) (:max-entries (integer 1)))
                         common-proto:value-map)
                protocol-data->map))

(defun protocol-data->map (map &key (max-depth 32) (max-entries 65536))
  "Encode the string-keyed data MAP with deterministic key ordering."
  (common-proto:value.map-value
   (protocol-data->value map :max-depth max-depth :max-entries max-entries)))
