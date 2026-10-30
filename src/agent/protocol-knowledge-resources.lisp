(in-package #:autolith)

;;;; -- Remote Knowledge Resources --

(defclass agent-knowledge-resource (agent-record-resource)
  ((view :initarg :view :reader agent-knowledge-resource-view
         :documentation "The public knowledge view identifier."))
  (:documentation "A memory knowledge resource served only by the parent."))

(defmethod agent-knowledge-resource-resolve ((scope agent-record-resource-scope) (identifier string))
  "Resolve a public knowledge identifier without opening a local store."
  (unless (agent-record-resource-scope-knowledge scope)
    (error 'agent-record-resource-contract-unsupported :identifier identifier))
  (make-instance 'agent-knowledge-resource :scope scope :collection ':memory
                 :identifier identifier :view identifier :exact-p nil
                 :uri (format nil "memory:~A" identifier)))

(defmethod resource-capabilities ((resource agent-knowledge-resource) (context tool-context))
  "Allow guarded edits only on the facts view in this connection."
  (when (eq (agent-record-resource-scope resource) (agent-record-resource--scope context))
    (if (equal (agent-knowledge-resource-view resource) "facts") '(:read :edit) '(:read))))

(-> agent-knowledge-resource--observe
    (agent-knowledge-resource &key (:query (option string)) (:maximum integer))
    agent-record-resource-observation)
(defun agent-knowledge-resource--observe (resource &key query (maximum 0))
  "Read the parent view and retain its exact guard."
  (let* ((reply    (agent-protocol-knowledge-request
                    (agent-record-resource-scope-knowledge (agent-record-resource-scope resource))
                    ':read :view (agent-knowledge-resource-view resource) :query query :limit maximum))
         (revision (and (agent-wire:knowledge-response.has-revision reply)
                        (agent-wire:knowledge-response.revision reply))))
    (make-instance 'agent-record-resource-observation
                   :uri (resource-uri resource) :identity (resource-item-identity resource)
                   :revision (or revision (make-identifier)) :collection-revision revision
                   :records nil :snapshot nil :metadata nil
                   :content (agent-wire:knowledge-response.content reply))))

(defmethod resource-tool-read
    ((resource agent-knowledge-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read through knowledge.v1 using normal conversation observations."
  (declare (ignore tool))
  (let ((query   (tool-argument arguments "query"))
        (maximum (tool-argument arguments "max-results")))
    (unless (and (or (null query) (and (non-empty-string-p query) (<= (length query) 4096)))
                 (or (null maximum) (and (integerp maximum) (<= 1 maximum 50))))
      (error 'tool-error :tool-name "resource.read" :message "Invalid knowledge query or result limit."))
    (let* ((observation (agent-knowledge-resource--observe resource :query query :maximum (or maximum 0)))
           (state       (resource-observation-state-ensure (tool-context-conversation context) observation)))
      (tool-success (resource-item-read-result state)))))

(defmethod resource-apply-operations
    ((resource agent-knowledge-resource) (context tool-context) &key base-revision operations)
  "Submit one bounded atomic batch with a stable identity for explicit retries."
  (unless (and (equal (agent-knowledge-resource-view resource) "facts")
               (listp operations) (<= 1 (length operations) 16)
               (every #'hash-table-p operations))
    (error 'tool-error :tool-name "resource.edit" :message "Knowledge edits require 1 to 16 fact operations."))
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held ((conversation-resource-observation-lock conversation))
      (let* ((state       (resource-item-find-observation-state conversation resource base-revision))
             (observation (resource-observation-state-observation state))
             (attempts    (agent-record-resource-observation-state-attempts state))
             (key         (agent-record-resource--operation-key (coerce operations 'vector)))
             (attempt     (gethash key attempts)))
        (unless attempt
          (when (>= (hash-table-count attempts) 64)
            (error 'tool-error :tool-name "resource.edit"
                   :message               "Knowledge retry limit reached. Read the resource again."))
          (let ((guard (agent-record-resource-observation-collection-revision observation)))
            (unless (non-empty-string-p guard)
              (agent-record-resource--stale resource base-revision))
            (setf attempt (list (make-identifier) guard
                                (agent-protocol-knowledge--fields
                                 (json-object "operations" (coerce operations 'vector)) :json-p t))
                  (gethash key attempts) attempt)))
        (handler-case
            (let* ((request (agent-wire:make-knowledge-request
                             :operation ':edit :operation-id (first attempt) :scope ':workspace
                             :view "facts" :expected-revision (second attempt) :fields (third attempt)))
                   (reply   (agent-session-driver-knowledge-request
                             (agent-protocol-knowledge-driver
                              (agent-record-resource-scope-knowledge (agent-record-resource-scope resource)))
                             request)))
              (declare (ignore reply))
              (values (agent-knowledge-resource--observe resource) "Updated parent knowledge." nil))
          (agent-protocol-knowledge-error (condition)
            (if (eq (agent-protocol-knowledge-error-code condition) ':revision-conflict)
                (agent-record-resource--stale resource base-revision)
                (error condition))))))))

(defmethod resource-tool-edit
    ((resource agent-knowledge-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply a guarded knowledge batch through the normal resource tool."
  (declare (ignore tool))
  (let ((revision   (tool-argument arguments "base-revision" :required t))
        (operations (tool-argument arguments "operations" :required t)))
    (unless (and (non-empty-string-p revision) (vectorp operations) (not (stringp operations)))
      (error 'tool-error :tool-name "resource.edit" :message "An observed revision and operation array are required."))
    (handler-case
        (multiple-value-bind (observation summary)
            (resource-apply-operations resource context :base-revision revision :operations (coerce operations 'list))
          (let ((state (resource-observation-state-ensure (tool-context-conversation context) observation)))
            (tool-success (format nil "~A~%~A" summary (resource-item-read-result state)))))
      (resource-revision-stale ()
        (tool-failure "Knowledge revision is stale or was not observed in this conversation. Reread memory:facts.")))))
