;;;; -- Optional Parent Memory Adapter --

(in-package #:autolith)

(defclass agent-protocol-memory ()
  ((driver :initarg :driver :reader agent-protocol-memory-driver
           :documentation "The connection's sole-reader driver."))
  (:documentation "A parent memory adapter with no local private pathname."))

(defclass agent-protocol-memory-item ()
  ((identifier :initarg :identifier :reader agent-protocol-memory-item-identifier
               :documentation "The parent's opaque record identity.")
   (revision :initarg :revision :reader agent-protocol-memory-item-revision
             :documentation "The exact parent revision for guarded writes.")
   (title :initarg :title :reader agent-protocol-memory-item-title
          :documentation "The memory title.")
   (content :initarg :content :reader agent-protocol-memory-item-content
            :documentation "The memory content.")
   (tags :initarg :tags :reader agent-protocol-memory-item-tags
         :documentation "The detached list of memory tags.")
   (scope :initarg :scope :reader agent-protocol-memory-item-scope
          :documentation "The parent-approved memory scope.")
   (sources :initarg :sources :reader agent-protocol-memory-item-sources
            :documentation "Detached source observations, including parent-checked digests.")
   (premises :initarg :premises :reader agent-protocol-memory-item-premises
             :documentation "Detached premise identities and observed content digests.")
   (fields :initarg :fields :initform (make-hash-table :test #'equal)
           :reader        agent-protocol-memory-item-fields
           :documentation "All validated public fields, including parent-owned evidence metadata."))
  (:documentation "A detached memory view with its exact revision and evidence."))

(define-condition agent-protocol-memory-error (error)
  ((code :initarg :code :reader agent-protocol-memory-error-code
         :documentation "The parent's fixed public failure code."))
  (:documentation "The parent rejected a memory request.")
  (:report (lambda (condition stream)
             (format stream "Parent memory request failed: ~A"
                     (agent-protocol-memory-error-code condition)))))

(-> make-agent-protocol-memory (t) agent-protocol-memory)
(defun make-agent-protocol-memory (driver)
  "Use DRIVER for parent-owned memory reads and writes."
  (make-instance 'agent-protocol-memory :driver driver))

(-> agent-protocol-memory--invalid (agent-protocol-memory) nil)
(defun agent-protocol-memory--invalid (adapter)
  "Close the connection after an invalid typed memory response."
  (let ((condition (make-condition 'agent-protocol-error :reason ':invalid-memory-response)))
    (agent-protocol-driver-close (agent-protocol-memory-driver adapter) :condition condition)
    (error condition)))

(-> agent-protocol-memory--exchange
    (agent-protocol-memory agent-wire:record-request) agent-wire:record-response)
(defun agent-protocol-memory--exchange (adapter request)
  "Wait for one terminal reply through the sole-reader driver. Never retry."
  (let ((delivery
          (agent-protocol-driver-request
           (agent-protocol-memory-driver adapter)
           (lambda (client) (agent-protocol-record-request client request)))))
    (multiple-value-bind (envelope terminal-p) (agent-protocol-driver-next delivery)
      (unless (and terminal-p envelope)
        (agent-protocol-memory--invalid adapter))
      (cond
        ((agent-wire:envelope.has-record-response envelope)
         (agent-wire:envelope.record-response envelope))
        ((agent-wire:envelope.has-failure envelope)
         (error 'agent-protocol-memory-error
                :code (agent-wire:failure.code (agent-wire:envelope.failure envelope))))
        (t
         (agent-protocol-memory--invalid adapter))))))

(-> agent-protocol-memory--item
    (agent-protocol-memory agent-wire:stored-record) agent-protocol-memory-item)
(defun agent-protocol-memory--item (adapter record)
  "Detach validated portable memory fields without evaluating record data."
  (let ((fields
          (handler-case
              (autolith-supervisor::protocol-value->data
               (cl-protobufs.autolith.common.v1:make-value
                :map-value (agent-wire:stored-record.fields record))
               :max-depth 8 :max-entries 1024)
            (autolith-supervisor::protocol-value-error ()
              (agent-protocol-memory--invalid adapter)))))
    (unless (agent-record--response-fields-p fields ':memory)
      (agent-protocol-memory--invalid adapter))
    (make-instance 'agent-protocol-memory-item
                   :identifier (copy-seq (agent-wire:stored-record.record-id record))
                   :revision   (copy-seq (agent-wire:stored-record.revision record))
                   :fields     fields
                   :title      (copy-seq (gethash "title" fields))
                   :content    (copy-seq (gethash "content" fields))
                   :tags       (map 'list #'copy-seq (gethash "tags" fields #()))
                   :scope      (if (equal (gethash "scope" fields) "global") ':global ':workspace)
                   :sources    (coerce (gethash "sources" fields #()) 'list)
                   :premises   (coerce (gethash "premises" fields #()) 'list))))

(-> agent-protocol-memory--fields
    (string string list &key (:scope (option memory-scope)) (:sources list) (:premises list))
    cl-protobufs.autolith.common.v1:value-map)
(defun agent-protocol-memory--fields
    (title content tags &key scope (sources nil sources-p) (premises nil premises-p))
  "Encode writable fields and preserve omitted evidence versus explicit empty lists."
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "title" fields) title
          (gethash "content" fields) content
          (gethash "tags" fields) (coerce tags 'vector))
    (when scope
      (setf (gethash "scope" fields) (string-downcase (symbol-name scope))))
    (when sources-p
      (setf (gethash "sources" fields) (coerce sources 'vector)))
    (when premises-p
      (setf (gethash "premises" fields)
            (map 'vector (lambda (id)
                           (if (uiop:string-prefix-p "memory:id/" id)
                               (resource-item-decode-identifier "memory" "memory" (subseq id 10))
                               id)) premises)))
    (cl-protobufs.autolith.common.v1:value.map-value
     (autolith-supervisor::protocol-data->value fields))))

(-> agent-protocol-memory-query
    (agent-protocol-memory &key (:query string) (:limit integer) (:cursor (option string))
     (:scope (option memory-scope)))
    (values list (option string) (option string)))
(defun agent-protocol-memory-query (adapter &key (query "") (limit 256) cursor scope)
  "Return one page of parent memory views and the optional next cursor."
  (let ((response
          (agent-protocol-memory--exchange
           adapter (apply #'agent-wire:make-record-request :collection ':memory :operation ':query
                          :query query :limit limit
                          (append (when cursor (list :cursor cursor))
                                  (when scope (list :scope scope)))))))
    (values (mapcar (lambda (record) (agent-protocol-memory--item adapter record))
                    (agent-wire:record-response.records response))
            (when (agent-wire:record-response.has-next-cursor response)
              (agent-wire:record-response.next-cursor response))
            (when (agent-wire:record-response.has-collection-revision response)
              (agent-wire:record-response.collection-revision response)))))

(-> agent-protocol-memory--write-result
    (agent-protocol-memory agent-wire:record-response) agent-protocol-memory-item)
(defun agent-protocol-memory--write-result (adapter response)
  "Require exactly one memory result after create or replace."
  (unless (and (= 1 (length (agent-wire:record-response.records response)))
               (not (agent-wire:record-response.has-next-cursor response)))
    (agent-protocol-memory--invalid adapter))
  (agent-protocol-memory--item adapter (first (agent-wire:record-response.records response))))

(-> agent-protocol-memory-create
    (agent-protocol-memory &key (:title string) (:content string) (:tags list)
     (:scope (option memory-scope)) (:sources list) (:premises list) (:operation-id string) (:expected-collection-revision (option string)))
    agent-protocol-memory-item)
(defun agent-protocol-memory-create
       (adapter
        &key title content tags scope (sources nil sources-p) (premises nil premises-p)
        (operation-id (make-identifier)) expected-collection-revision)
  "Create one parent memory. Retain OPERATION-ID if the outcome is unknown."
  (agent-protocol-memory--write-result adapter
   (agent-protocol-memory--exchange adapter
    (apply #'agent-wire:make-record-request :collection ':memory :operation ':create :operation-id
           operation-id :fields
           (apply #'agent-protocol-memory--fields title content tags :scope scope
                  (append (when sources-p (list :sources sources))
                          (when premises-p (list :premises premises))))
           (append (when scope (list :scope scope))
                   (when expected-collection-revision
                     (list :expected-collection-revision expected-collection-revision)))))))

(-> agent-protocol-memory-replace
    (agent-protocol-memory agent-protocol-memory-item
     &key (:title string) (:content string) (:tags list) (:scope (option memory-scope))
     (:sources list) (:premises list) (:operation-id string))
    agent-protocol-memory-item)
(defun agent-protocol-memory-replace
    (adapter item &key (title (agent-protocol-memory-item-title item))
                      (content (agent-protocol-memory-item-content item))
                      (tags (agent-protocol-memory-item-tags item))
                      scope (sources nil sources-p) (premises nil premises-p)
                      (operation-id (make-identifier)))
  "Replace complete writable fields against ITEM's exact parent revision."
  (agent-protocol-memory--write-result
   adapter (agent-protocol-memory--exchange
             adapter (apply #'agent-wire:make-record-request
                      :collection ':memory :operation ':replace :operation-id operation-id
                      :record-id         (agent-protocol-memory-item-identifier item)
                      :expected-revision (agent-protocol-memory-item-revision item)
                      :fields            (apply #'agent-protocol-memory--fields title content tags :scope scope
                                                (append (when sources-p (list :sources sources))
                                                        (when premises-p (list :premises premises))))
                      (when (eq (agent-protocol-memory-item-scope item) ':global)
                        (list :scope ':global))))))

(-> agent-protocol-memory-delete
    (agent-protocol-memory agent-protocol-memory-item
     &key (:operation-id string) (:scope (option memory-scope))) null)
(defun agent-protocol-memory-delete
    (adapter item &key (operation-id (make-identifier))
                       (scope (when (eq (agent-protocol-memory-item-scope item) ':global) ':global)))
  "Delete ITEM against its exact parent revision without retrying."
  (let ((response
          (agent-protocol-memory--exchange
           adapter (apply #'agent-wire:make-record-request
                    :collection ':memory :operation ':delete :operation-id operation-id
                    :record-id         (agent-protocol-memory-item-identifier item)
                    :expected-revision (agent-protocol-memory-item-revision item)
                    (when scope (list :scope scope))))))
    (when (or (agent-wire:record-response.records response)
              (agent-wire:record-response.has-next-cursor response))
      (agent-protocol-memory--invalid adapter))
    nil))
