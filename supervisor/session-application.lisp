;;;; -- Durable Application Scopes --

(in-package #:autolith-supervisor)

(defclass session-application-scope ()
  ((identifier :initarg :identifier :reader session-application-scope-identifier
               :documentation "The parent-created operation identity.")
   (sequence :initarg :sequence :reader session-application-scope-sequence
             :documentation "The committed input sequence before dispatch.")
   (request :initform nil :accessor session-application-scope-request
            :documentation "The reserved parent steering request identifier.")
   (result :initform nil :accessor session-application-scope-result
           :documentation "The committed typed result, or NIL while work is pending."))
  (:documentation "A bounded application operation within one active provider turn."))

(declaim (ftype (function (session-store string &key (:event-id string) (:fields hash-table)) integer)
                session-store-application-note))

(defun session-store-application-note (store turn-id &key event-id fields)
  "Commit a parent-owned application scope note within the current durable turn."
  (session-store--locked
   store
   (lambda (records active events turns calls)
     (declare (ignore turns calls))
     (unless (and (equal active turn-id) (not (gethash event-id events)))
       (session-store--reject ':inactive-turn))
     (let ((sequence (1+ (length records))))
       (session-store--commit
        store (list sequence event-id turn-id ':note
                    (session-store--value-data (protocol-data->value fields)) ':parent))
       sequence))))

(declaim (ftype (function (session-runtime string parent-proto:application-action) integer)
                session-runtime-application-steer))

(defun session-runtime-application-steer (runtime identifier action)
  "Commit exact active input before sending it to the child FIFO. Never retry on loss."
  (protocol-application-action-validate action)
  (with-session-runtime-owner (runtime)
    (unless (and (eq (session-runtime-phase runtime) ':running)
                 (session-runtime-turn-request runtime)
                 (session-store--identifier-p identifier)
                 (not (gethash identifier (session-runtime-application-scopes runtime)))
                 (< (hash-table-count (session-runtime-application-scopes runtime)) 64)
                 (member "application.v1" (session-runtime-features runtime) :test #'equal))
      (error 'parent-protocol-error :reason ':not-accepting-application-input))
    (let* ((fields  (protocol-application-action->data action))
           (turn-id (session-runtime-turn-id runtime)))
      (setf (gethash "application_scope" fields) identifier)
      (let* ((sequence (session-store-application-note
                        (session-runtime-store runtime) turn-id
                        :event-id (format nil "application/~A/input" identifier) :fields fields))
             (scope    (make-instance 'session-application-scope :identifier identifier :sequence sequence))
             (envelope (parent-proto:make-envelope
                        :application-steering
                        (parent-proto:make-application-steering
                         :turn-id turn-id :operation-id identifier :input-sequence sequence :action action))))
        (setf (gethash identifier (session-runtime-application-scopes runtime)) scope)
        (handler-case
            (progn
              (session-runtime--send (session-runtime-service runtime) runtime envelope)
              (setf (session-application-scope-request scope) (parent-proto:envelope.message-id envelope))
              sequence)
          (error (condition)
            (session-runtime-disconnect runtime)
            (error condition)))))))

(declaim (ftype (function (session-runtime string) boolean) session-application-scope-open-p))

(defun session-application-scope-open-p (runtime identifier)
  "Accept only the current root or a parent-created, unfinished nested operation."
  (let ((scope (gethash identifier (session-runtime-application-scopes runtime))))
    (not (null (or (equal identifier (session-runtime-turn-id runtime))
                   (and scope (null (session-application-scope-result scope))))))))

(declaim (ftype (function (session-runtime) null) session-application--drain-finish))

(defun session-application--drain-finish (runtime)
  "Apply a received TurnFinish only after each admitted active operation has a result."
  (when (and (session-runtime-application-finish runtime)
             (loop for scope being the hash-values of (session-runtime-application-scopes runtime)
                   always (session-application-scope-result scope)))
    (let ((envelope (session-runtime-application-finish runtime)))
      (setf (session-runtime-application-finish runtime) nil)
      (session-runtime--dispatch runtime envelope)))
  nil)

(declaim (ftype (function (session-runtime parent-proto:application-result) integer)
                session-application--commit-result))

(defun session-application--commit-result (runtime result)
  "Commit the typed result before its callback, acknowledgement, or turn completion."
  (protocol-application-result-validate result)
  (let* ((identifier (parent-proto:application-result.turn-id result))
         (scope      (gethash identifier (session-runtime-application-scopes runtime)))
         (fields     (make-hash-table :test #'equal)))
    (unless (and (session-runtime-turn-request runtime)
                 (session-application-scope-open-p runtime identifier)
                 (or scope (null (session-runtime-application-result runtime))))
      (error 'parent-protocol-error :reason ':unexpected-application-result))
    (setf (gethash "application_scope" fields) identifier
          (gethash "application_result" fields) (protocol-message->data result)
          (gethash "text" fields) "")
    (let ((sequence (session-store-application-note
                     (session-runtime-store runtime) (session-runtime-turn-id runtime)
                     :event-id (format nil "application/~A/result" identifier) :fields fields)))
      (if scope
          (setf (session-application-scope-result scope) result)
          (setf (session-runtime-application-result runtime) result))
      (when (session-runtime-application-callback runtime)
        (funcall (session-runtime-application-callback runtime) result))
      sequence)))

(declaim (ftype (function (session-runtime parent-proto:envelope) boolean)
                session-application-handle-incoming))

(defun session-application-handle-incoming (runtime envelope)
  "Handle typed application traffic on the storage owner; return true when consumed."
  (let ((body (parent-proto:envelope.body-case envelope)))
    (case body
      (parent-proto:application-event
       (let* ((event  (parent-proto:envelope.application-event envelope))
              (kind   (parent-proto:application-event.kind event))
              (fields (protocol-map->data (parent-proto:application-event.fields event))))
         (unless (and (not (parent-proto:envelope.has-reply-to envelope))
                      (member kind '(:text :reasoning :status :tool-started :tool-finished :presentation :acp :ui))
                      (<= (length (cl-protobufs:serialize-to-bytes event)) (* 1024 1024)))
           (error 'parent-protocol-error :reason ':invalid-application-event))
         (when (session-runtime-application-event-callback runtime)
           (funcall (session-runtime-application-event-callback runtime) kind fields)))
       t)
      (parent-proto:application-notification
       (let* ((event      (parent-proto:envelope.application-notification envelope))
              (kind       (parent-proto:application-notification.kind event))
              (identifier (parent-proto:application-notification.identifier event)))
         (unless (and (not (parent-proto:envelope.has-reply-to envelope))
                      (member kind '(:acp-completion-ready :ui-completion-ready))
                      (<= (length identifier) 256))
           (error 'parent-protocol-error :reason ':invalid-application-notification))
         (when (session-runtime-application-notification-callback runtime)
           (funcall (session-runtime-application-notification-callback runtime) kind identifier)))
       t)
      (parent-proto:application-result
       (with-session-runtime-owner (runtime)
         (when (or (parent-proto:envelope.has-reply-to envelope)
                   (not (member "application.v1" (session-runtime-features runtime) :test #'equal)))
           (error 'parent-protocol-error :reason ':invalid-application-result))
         (let* ((result   (parent-proto:envelope.application-result envelope))
                (sequence (session-application--commit-result runtime result)))
           (session-runtime--send
            (session-runtime-service runtime) runtime
            (parent-proto:make-envelope
             :reply-to (parent-proto:envelope.message-id envelope)
             :session-ack
             (parent-proto:make-session-ack
              :event-id (format nil "application/~A/result" (parent-proto:application-result.turn-id result))
              :sequence sequence)))
           (session-runtime--after-send (session-runtime-service runtime) runtime
            (lambda ()
              (setf (session-runtime-acknowledged runtime)
                    (max (session-runtime-acknowledged runtime) sequence))
              (setf (gethash (format nil "application/~A/result"
                                      (parent-proto:application-result.turn-id result))
                             (session-runtime-append-acks runtime)) sequence)))
          (session-application--drain-finish runtime)))
       t)
      (parent-proto:turn-finish
       (with-session-runtime-owner (runtime)
         (if (loop for scope being the hash-values of (session-runtime-application-scopes runtime)
                   thereis (null (session-application-scope-result scope)))
             (progn
               (when (session-runtime-application-finish runtime)
                 (error 'parent-protocol-error :reason ':duplicate-turn-finish))
               (setf (session-runtime-application-finish runtime) envelope)
               t)
             nil)))
      (parent-proto:failure
       (with-session-runtime-owner (runtime)
         (let ((scope
                 (and (parent-proto:envelope.has-reply-to envelope)
                      (loop for item being the hash-values of (session-runtime-application-scopes runtime)
                            when (eql (session-application-scope-request item)
                                      (parent-proto:envelope.reply-to envelope))
                              return item))))
           (when scope
             (session-application--commit-result
              runtime (parent-proto:make-application-result
                       :turn-id (session-application-scope-identifier scope) :status ':error
                       :condition "Child refused active input after the turn ended."))
             (session-application--drain-finish runtime)
             t))))
      (otherwise
       nil))))
