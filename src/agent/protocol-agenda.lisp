;;;; -- Optional Parent Agenda Adapter --

(in-package #:autolith)

(defclass agent-protocol-agenda ()
  ((driver :initarg :driver :reader agent-protocol-agenda-driver
           :documentation "The connection's sole-reader driver."))
  (:documentation "A credential-free agenda adapter with no local private pathname."))

(defclass agent-protocol-agenda-item (agenda-item)
  ((revision :initarg :revision :reader agent-protocol-agenda-item-revision
             :type          non-empty-string
             :documentation "The exact parent revision used for a guarded write."))
  (:documentation "An observed parent agenda item with a guarded revision."))

(define-condition agent-protocol-agenda-error (error)
  ((code :initarg :code :reader agent-protocol-agenda-error-code
         :documentation "The parent's fixed public failure code."))
  (:documentation "The parent rejected an agenda request without changing client authority.")
  (:report (lambda (condition stream)
             (format stream "Parent agenda request failed: ~A"
                     (agent-protocol-agenda-error-code condition)))))

(-> make-agent-protocol-agenda (t) agent-protocol-agenda)
(defun make-agent-protocol-agenda (driver)
  "Use DRIVER for parent-owned agenda reads and writes."
  (make-instance 'agent-protocol-agenda :driver driver))

(-> agent-protocol-agenda--invalid (agent-protocol-agenda) nil)
(defun agent-protocol-agenda--invalid (adapter)
  "Close the connection after an invalid typed agenda response."
  (let ((condition (make-condition 'agent-protocol-error :reason ':invalid-agenda-response)))
    (agent-protocol-driver-close (agent-protocol-agenda-driver adapter) :condition condition)
    (error condition)))

(-> agent-protocol-agenda--exchange
    (agent-protocol-agenda agent-wire:record-request) agent-wire:record-response)
(defun agent-protocol-agenda--exchange (adapter request)
  "Wait for one terminal record reply through the sole-reader driver. Never retry."
  (let ((delivery
          (agent-protocol-driver-request
           (agent-protocol-agenda-driver adapter)
           (lambda (client) (agent-protocol-record-request client request)))))
    (multiple-value-bind (envelope terminal-p) (agent-protocol-driver-next delivery)
      (unless (and terminal-p envelope)
        (agent-protocol-agenda--invalid adapter))
      (cond
        ((agent-wire:envelope.has-record-response envelope)
         (agent-wire:envelope.record-response envelope))
        ((agent-wire:envelope.has-failure envelope)
         (error 'agent-protocol-agenda-error
                :code (agent-wire:failure.code (agent-wire:envelope.failure envelope))))
        (t
         (agent-protocol-agenda--invalid adapter))))))

(-> agent-protocol-agenda--item
    (agent-protocol-agenda agent-wire:stored-record) agent-protocol-agenda-item)
(defun agent-protocol-agenda--item (adapter record)
  "Validate and detach one portable agenda view. Never evaluate record data."
  (let* ((identifier (agent-wire:stored-record.record-id record))
         (revision   (agent-wire:stored-record.revision record))
         (fields
           (handler-case
               (autolith-supervisor::protocol-value->data
                (cl-protobufs.autolith.common.v1:make-value
                 :map-value (agent-wire:stored-record.fields record))
                :max-depth 4 :max-entries 64)
             (autolith-supervisor::protocol-value-error ()
               (agent-protocol-agenda--invalid adapter))))
         (text       (gethash "text" fields))
         (status     (rest (assoc (gethash "status" fields)
                                 '(("todo" . :todo) ("doing" . :doing) ("blocked" . :blocked)
                                   ("done" . :done) ("note" . :note)) :test #'equal)))
         (memories   (gethash "memory_ids" fields))
         (created    (gethash "created_at" fields))
         (updated    (gethash "updated_at" fields)))
    (unless (and (non-empty-string-p identifier) (<= (length identifier) 1024)
                 (non-empty-string-p revision) (<= (length revision) 1024)
                 (non-empty-string-p text) (<= (length text) *agenda-item-text-limit*)
                 status (vectorp memories) (not (stringp memories))
                 (agenda--memory-identifiers-p (coerce memories 'list))
                 (typep created 'timestamp) (typep updated 'timestamp))
      (agent-protocol-agenda--invalid adapter))
    (make-instance 'agent-protocol-agenda-item
                   :identifier (copy-seq identifier) :revision (copy-seq revision)
                   :text text :status status :memory-identifiers (coerce memories 'list)
                   :created-at created :updated-at updated)))

(-> agent-protocol-agenda--fields (string agenda-status list)
    cl-protobufs.autolith.common.v1:value-map)
(defun agent-protocol-agenda--fields (text status memory-identifiers)
  "Encode complete writable agenda fields without local persistence."
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "text" fields) text
          (gethash "status" fields) (string-downcase (symbol-name status))
          (gethash "memory_ids" fields) (coerce memory-identifiers 'vector))
    (cl-protobufs.autolith.common.v1:value.map-value
     (autolith-supervisor::protocol-data->value fields))))

(-> agent-protocol-agenda-query
    (agent-protocol-agenda &key (:query string) (:limit integer) (:cursor (option string)))
    (values list (option string)))
(defun agent-protocol-agenda-query (adapter &key (query "") (limit 256) cursor)
  "Return one complete page of parent items and the optional next cursor."
  (let ((response
          (agent-protocol-agenda--exchange
           adapter (apply #'agent-wire:make-record-request :collection ':agenda :operation ':query
                          :query query :limit limit (when cursor (list :cursor cursor))))))
    (values (mapcar (lambda (record) (agent-protocol-agenda--item adapter record))
                    (agent-wire:record-response.records response))
            (when (agent-wire:record-response.has-next-cursor response)
              (agent-wire:record-response.next-cursor response)))))

(-> agent-protocol-agenda--write-result
    (agent-protocol-agenda agent-wire:record-response) agent-protocol-agenda-item)
(defun agent-protocol-agenda--write-result (adapter response)
  "Require exactly one resulting agenda record after a create or replace."
  (unless (and (= 1 (length (agent-wire:record-response.records response)))
               (not (agent-wire:record-response.has-next-cursor response)))
    (agent-protocol-agenda--invalid adapter))
  (agent-protocol-agenda--item adapter (first (agent-wire:record-response.records response))))

(-> agent-protocol-agenda-create
    (agent-protocol-agenda &key (:text string) (:status agenda-status)
     (:memory-identifiers list) (:operation-id string))
    agent-protocol-agenda-item)
(defun agent-protocol-agenda-create
    (adapter &key text (status ':todo) memory-identifiers (operation-id (make-identifier)))
  "Create one parent item. Retain OPERATION-ID if the outcome is unknown."
  (agent-protocol-agenda--write-result
   adapter (agent-protocol-agenda--exchange
            adapter (agent-wire:make-record-request
                     :collection ':agenda :operation ':create :operation-id operation-id
                     :fields (agent-protocol-agenda--fields text status memory-identifiers)))))

(-> agent-protocol-agenda-replace
    (agent-protocol-agenda agent-protocol-agenda-item
     &key (:text string) (:status agenda-status) (:memory-identifiers list) (:operation-id string))
    agent-protocol-agenda-item)
(defun agent-protocol-agenda-replace
    (adapter item &key (text (agenda-item-text item)) (status (agenda-item-status item))
                   (memory-identifiers (agenda-item-memory-identifiers item))
                   (operation-id (make-identifier)))
  "Replace complete writable fields against ITEM's exact parent revision."
  (agent-protocol-agenda--write-result
   adapter (agent-protocol-agenda--exchange
            adapter (agent-wire:make-record-request
                     :collection ':agenda :operation ':replace :operation-id operation-id
                     :record-id         (agenda-item-identifier item)
                     :expected-revision (agent-protocol-agenda-item-revision item)
                     :fields            (agent-protocol-agenda--fields text status memory-identifiers)))))

(-> agent-protocol-agenda-delete
    (agent-protocol-agenda agent-protocol-agenda-item &key (:operation-id string)) null)
(defun agent-protocol-agenda-delete (adapter item &key (operation-id (make-identifier)))
  "Delete ITEM against its exact parent revision, without retrying."
  (let ((response
          (agent-protocol-agenda--exchange
           adapter (agent-wire:make-record-request
                    :collection ':agenda :operation ':delete :operation-id operation-id
                    :record-id         (agenda-item-identifier item)
                    :expected-revision (agent-protocol-agenda-item-revision item)))))
    (when (or (agent-wire:record-response.records response)
              (agent-wire:record-response.has-next-cursor response))
      (agent-protocol-agenda--invalid adapter))
    nil))
