;;;; -- Parent Model Runtime Mailboxes --

(in-package #:autolith-supervisor)

(defclass model-runtime-call ()
  ((function :initarg :function :reader model-runtime-call-function
             :documentation "Operation to execute on the storage owner.")
   (bytes :initarg :bytes :initform 0 :reader model-runtime-call-bytes
          :documentation "Detached input bytes charged to the mailbox.")
   (wait :initarg :wait :reader model-runtime-call-wait
         :documentation "Whether a caller receives operation failures.")
   (delivery :initarg :delivery :initform nil :reader model-runtime-call-delivery
             :documentation "Whether earlier output receipts must be applied before execution.")
   (done :initform nil :accessor model-runtime-call-done
         :documentation "True after owner execution, under the mailbox lock.")
   (values :initform nil :accessor model-runtime-call-values
           :documentation "Returned multiple values.")
   (error :initform nil :accessor model-runtime-call-error
          :documentation "Local condition returned to a waiting caller.")
   (finished :initform      (bordeaux-threads:make-condition-variable)
             :reader        model-runtime-call-finished
             :documentation "Wakes the synchronous caller."))
  (:documentation "One bounded storage-owner operation."))

(defclass model-runtime-write ()
  ((bytes :initarg :bytes :reader model-runtime-write-bytes
          :documentation "Detached encoded envelope, without its frame prefix.")
   (control :initarg :control :reader model-runtime-write-control
            :documentation "Whether this write uses the independent control reserve.")
   (delivered :initform nil :accessor model-runtime-write-delivered
              :documentation "True only after the full frame write returns.")
   (actions :initform nil :accessor model-runtime-write-actions
            :documentation "Owner-only history changes after successful delivery."))
  (:documentation "A queued write and its delivery receipt."))

(defclass model-runtime-mailbox ()
  ((lock :initform      (bordeaux-threads:make-lock "parent model mailbox")
         :reader        model-runtime-mailbox-lock
         :documentation "Protects only bounded queue and latch changes.")
   (ready :initform      (bordeaux-threads:make-condition-variable)
          :reader        model-runtime-mailbox-ready
          :documentation "Notifies the owner about calls, receipts and control latches.")
   (write-ready :initform      (bordeaux-threads:make-condition-variable)
                :reader        model-runtime-mailbox-write-ready
                :documentation "Notifies the sole writer about queued frames.")
   (calls :initform nil :accessor model-runtime-mailbox-calls
          :documentation "FIFO of peer and parent operations.")
   (input-bytes :initform 0 :accessor model-runtime-mailbox-input-bytes
                :documentation "Charged bytes for queued and current operations.")
   (writes :initform nil :accessor model-runtime-mailbox-writes
           :documentation "FIFO including the current write until owner receipt processing.")
   (data-bytes :initform 0 :accessor model-runtime-mailbox-data-bytes
               :documentation "Data bytes including a blocked current write.")
   (control-bytes :initform 0 :accessor model-runtime-mailbox-control-bytes
                  :documentation "Independent control bytes including a current control write.")
   (writing :initform nil :accessor model-runtime-mailbox-writing
            :documentation "Frame currently owned by the writer, or NIL.")
   (last-write :initform nil :accessor model-runtime-mailbox-last-write
               :documentation "Owner's most recent reservation, for delivery actions.")
   (closed :initform nil :accessor model-runtime-mailbox-closed
           :documentation "Reject new admission and stop the owner and writer.")
   (error :initform nil :accessor model-runtime-mailbox-error
          :documentation "First local queue or writer error.")
   (owner :initform nil :accessor model-runtime-mailbox-owner
          :documentation "Tracked storage-owner thread.")
   (writer :initform nil :accessor model-runtime-mailbox-writer
           :documentation "Tracked sole frame-writer thread."))
  (:documentation "Independent bounded input and output queues. No I/O runs under LOCK."))

(declaim (ftype (function (model-runtime-mailbox) null) model-runtime--wake))

(defun model-runtime--wake (mailbox)
  "Wake both workers while holding the mailbox lock."
  (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))
  (bordeaux-threads:condition-notify (model-runtime-mailbox-write-ready mailbox))
  nil)

(declaim (ftype (function (model-runtime-mailbox keyword) null) model-runtime--close-mailbox))

(defun model-runtime--close-mailbox (mailbox code)
  "Latch closure without I/O, callbacks or joins."
  (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
    (unless (model-runtime-mailbox-error mailbox)
      (setf (model-runtime-mailbox-error mailbox) code))
    (setf (model-runtime-mailbox-closed mailbox) t)
    (dolist (call (model-runtime-mailbox-calls mailbox))
      (setf (model-runtime-call-error call)
            (make-condition 'parent-protocol-error :reason code)
            (model-runtime-call-done call) t)
      (bordeaux-threads:condition-notify (model-runtime-call-finished call)))
    (model-runtime--wake mailbox))
  nil)

(declaim (ftype (function (model-runtime-mailbox function &key (:bytes integer) (:wait boolean)
                                                            (:delivery boolean)) t)
                model-runtime--call))

(defun model-runtime--call (mailbox function &key (bytes 0) (wait t) delivery)
  "Submit one operation. Bound input to 64 calls and 16 MiB, including current work."
  (when (eq (bordeaux-threads:current-thread) (model-runtime-mailbox-owner mailbox))
    (return-from model-runtime--call (funcall function)))
  (let ((call (make-instance 'model-runtime-call :function function :bytes bytes :wait wait :delivery delivery)))
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (when (model-runtime-mailbox-closed mailbox)
        (error 'parent-protocol-error :reason ':closed))
      (when (or (>= (length (model-runtime-mailbox-calls mailbox)) 64)
                (> (+ bytes (model-runtime-mailbox-input-bytes mailbox)) (* 16 1024 1024)))
        (error 'parent-protocol-error :reason ':runtime-input-limit))
      (incf (model-runtime-mailbox-input-bytes mailbox) bytes)
      (setf (model-runtime-mailbox-calls mailbox)
            (nconc (model-runtime-mailbox-calls mailbox) (list call)))
      (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox))
      (when wait
        (loop until (model-runtime-call-done call)
              do (bordeaux-threads:condition-wait (model-runtime-call-finished call)
                                                (model-runtime-mailbox-lock mailbox)))))
    (when wait
      (when (model-runtime-call-error call)
        (error (model-runtime-call-error call)))
      (values-list (model-runtime-call-values call)))))

(declaim (ftype (function (model-runtime-mailbox parent-connection parent-proto:envelope) null)
                model-runtime--enqueue-write))

(defun model-runtime--enqueue-write (mailbox connection envelope)
  "Reserve an ID with ordered byte admission. Only the owner calls this operation.
Current writes consume capacity until the owner processes their delivery receipt.
Control and terminal frames have a separate 16 MiB reserve. Data has 16 MiB."
  (unless (eq (bordeaux-threads:current-thread) (model-runtime-mailbox-owner mailbox))
    (error 'parent-protocol-error :reason ':not-runtime-owner))
  (setf (parent-proto:envelope.protocol-major envelope) 1
        (parent-proto:envelope.instance-id envelope) (parent-connection-instance-id connection)
        (parent-proto:envelope.message-id envelope) (1+ (parent-connection-sent connection)))
  (let* ((event   (parent-proto:envelope.model-event envelope))
         (control (or (not (parent-proto:envelope.has-model-event envelope))
                      (parent-proto:model-event.has-complete event)
                      (parent-proto:model-event.has-failed event)))
         (bytes   (cl-protobufs:serialize-to-bytes envelope))
         (write   (make-instance 'model-runtime-write :bytes bytes :control control)))
    (protocol-validate-protobuf bytes 'parent-proto:envelope
                                :max-bytes   (parent-proto:limits.max-frame-bytes
                                              (parent-connection-limits connection))
                                :max-depth   (parent-proto:limits.max-value-depth
                                              (parent-connection-limits connection))
                                :max-entries (parent-proto:limits.max-collection-entries
                                              (parent-connection-limits connection)))
    (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
      (when (model-runtime-mailbox-closed mailbox)
        (error 'parent-protocol-error :reason ':closed))
      (when (> (+ (length bytes) 4
                  (if control (model-runtime-mailbox-control-bytes mailbox)
                      (model-runtime-mailbox-data-bytes mailbox)))
               (* 16 1024 1024))
        (error 'parent-protocol-error :reason ':runtime-output-limit))
      (if control
          (incf (model-runtime-mailbox-control-bytes mailbox) (+ 4 (length bytes)))
          (incf (model-runtime-mailbox-data-bytes mailbox) (+ 4 (length bytes))))
      (incf (parent-connection-sent connection))
      (setf (model-runtime-mailbox-writes mailbox)
            (nconc (model-runtime-mailbox-writes mailbox) (list write))
            (model-runtime-mailbox-last-write mailbox) write)
      (bordeaux-threads:condition-notify (model-runtime-mailbox-write-ready mailbox))))
  nil)

(declaim (ftype (function (model-runtime-mailbox parent-connection function) null)
                model-runtime--writer))

(defun model-runtime--writer (mailbox connection on-error)
  "Write complete frames in order. Publish receipts without running history callbacks."
  (handler-case
      (loop
        (let ((write
                (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
                  (loop
                    (when (model-runtime-mailbox-closed mailbox)
                      (return-from model-runtime--writer nil))
                    (let ((next (find-if-not #'model-runtime-write-delivered
                                            (model-runtime-mailbox-writes mailbox))))
                      (when next
                        (setf (model-runtime-mailbox-writing mailbox) next)
                        (return next)))
                    (bordeaux-threads:condition-wait (model-runtime-mailbox-write-ready mailbox)
                                                    (model-runtime-mailbox-lock mailbox))))))
          (protocol-write-frame (parent-connection-output connection) (model-runtime-write-bytes write)
                                :limit (parent-proto:limits.max-frame-bytes
                                        (parent-connection-limits connection)))
          (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
            (setf (model-runtime-write-delivered write) t
                  (model-runtime-mailbox-writing mailbox) nil)
            (bordeaux-threads:condition-notify (model-runtime-mailbox-ready mailbox)))))
    (error ()
      (model-runtime--close-mailbox mailbox (if (eq ':closed (parent-connection-phase connection))
                                               ':closed ':runtime-write-failed))
      (funcall on-error)))
  nil)

(declaim (ftype (function (model-runtime-mailbox) list) model-runtime--take-receipts))

(defun model-runtime--take-receipts (mailbox)
  "Remove delivered write prefixes under lock. Execute their actions on the owner."
  (bordeaux-threads:with-lock-held ((model-runtime-mailbox-lock mailbox))
    (loop while (and (model-runtime-mailbox-writes mailbox)
                     (model-runtime-write-delivered (first (model-runtime-mailbox-writes mailbox))))
          for write = (pop (model-runtime-mailbox-writes mailbox))
          do (if (model-runtime-write-control write)
                 (decf (model-runtime-mailbox-control-bytes mailbox)
                       (+ 4 (length (model-runtime-write-bytes write))))
                 (decf (model-runtime-mailbox-data-bytes mailbox)
                       (+ 4 (length (model-runtime-write-bytes write)))))
          append (reverse (model-runtime-write-actions write)))))
