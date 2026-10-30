;;;; -- Agent Private Record Client --

(in-package #:autolith)

(defclass agent-record-request ()
  ((request :initarg :request :reader agent-record-request-message
            :documentation "Detached request used to check its terminal response."))
  (:documentation "Expected terminal response for one records.v1 request."))

(-> agent-record--string-p (t integer) boolean)
(defun agent-record--string-p (value maximum)
  "Require nonempty bounded text without interpreting it as a pathname."
  (and (stringp value) (<= 1 (length value) maximum)))

(-> agent-record--strings-p (t) boolean)
(defun agent-record--strings-p (value)
  "Require a bounded portable list of nonempty string identities or tags."
  (and (simple-vector-p value) (<= (length value) 256)
       (every (lambda (item) (agent-record--string-p item 1024)) value)))

(-> agent-record--fields (agent-wire::record-request) hash-table)
(defun agent-record--fields (request)
  "Decode writable fields with fixed local bounds and duplicate-key checks."
  (if (agent-wire:record-request.has-fields request)
      (autolith-supervisor::protocol-value->data
       (cl-protobufs.autolith.common.v1:make-value :map-value (agent-wire:record-request.fields request))
       :max-depth 4 :max-entries 1024)
      (make-hash-table :test 'equal)))

(-> agent-record--allowed-fields (keyword keyword) list)
(defun agent-record--allowed-fields (collection operation)
  "Return the exact writable field names for COLLECTION and OPERATION."
  (case collection
    (:memory
     '("title" "content" "tags"))
    (:agenda
     '("text" "status" "memory_ids"))
    (:papercut
     (case operation
       (:create
        '("title" "content" "diagnostic" "issue_kind" "tool"))
       (:assess
        '("verdict" "note" "diagnostic"))
       (:close
        '("resolution" "diagnostic"))))))

(-> agent-record--writable-p (hash-table keyword keyword) boolean)
(defun agent-record--writable-p (fields collection operation)
  "Check writable field names and types, including required fields."
  (and
   (loop for key being the hash-keys of fields
         always (member key (agent-record--allowed-fields collection operation) :test #'equal))
   (loop for key being the hash-keys of fields using (hash-value value)
         always (if (member key '("tags" "memory_ids") :test #'equal)
                    (agent-record--strings-p value)
                    (and (stringp value) (<= (length value) 65536))))
   (case collection
     (:memory
      (and (agent-record--string-p (gethash "title" fields) 65536)
           (agent-record--string-p (gethash "content" fields) 65536)))
     (:agenda
      (and (agent-record--string-p (gethash "text" fields) 500)
           (not (null (member (gethash "status" fields)
                              '("todo" "doing" "blocked" "done" "note") :test #'equal)))))
     (:papercut
      (case operation
        (:create
         (and (agent-record--string-p (gethash "title" fields) 65536)
              (agent-record--string-p (gethash "content" fields) 65536)
              (or (not (nth-value 1 (gethash "issue_kind" fields)))
                  (not (null (member (gethash "issue_kind" fields)
                                     '("broken_tool" "misleading_success" "authorization" "performance" "other")
                                     :test #'equal))))))
        (:assess
         (and (not (null (member (gethash "verdict" fields)
                                 '("improved" "worse" "unchanged" "too-early") :test #'equal)))
              (agent-record--string-p (gethash "note" fields) 65536)))
        (:close
         (agent-record--string-p (gethash "resolution" fields) 65536)))))))

(-> agent-record--validate-request (agent-wire:record-request) null)
(defun agent-record--validate-request (request)
  "Check records.v1 operation shape before registering or sending a request."
  (let* ((collection (agent-wire:record-request.collection request))
         (operation  (agent-wire:record-request.operation request))
         (fields     (agent-record--fields request))
         (query-p    (eq operation ':query))
         (create-p   (eq operation ':create))
         (delete-p   (eq operation ':delete)))
    (unless
        (and (member collection '(:memory :papercut :agenda))
             (member operation (if (eq collection ':papercut)
                                   '(:query :create :assess :close)
                                   '(:query :create :replace :delete)))
             (if query-p
                 (and (zerop (length (agent-wire:record-request.operation-id request)))
                      (not (agent-wire:record-request.has-record-id request))
                      (not (agent-wire:record-request.has-expected-revision request))
                      (not (agent-wire:record-request.has-fields request))
                      (<= 1 (agent-wire:record-request.limit request) 256)
                      (<= (length (agent-wire:record-request.query request)) 1024)
                      (or (not (agent-wire:record-request.has-cursor request))
                          (agent-record--string-p (agent-wire:record-request.cursor request) 1024)))
                 (and (agent-record--string-p (agent-wire:record-request.operation-id request) 1024)
                      (not (agent-wire:record-request.has-query request))
                      (not (agent-wire:record-request.has-cursor request))
                      (zerop (agent-wire:record-request.limit request))
                      (if create-p
                          (and (not (agent-wire:record-request.has-record-id request))
                               (not (agent-wire:record-request.has-expected-revision request)))
                          (and (agent-record--string-p (agent-wire:record-request.record-id request) 1024)
                               (agent-record--string-p (agent-wire:record-request.expected-revision request) 1024)))
                      (if delete-p
                          (not (agent-wire:record-request.has-fields request))
                          (and (agent-wire:record-request.has-fields request)
                               (agent-record--writable-p fields collection operation))))))
      (error 'agent-protocol-error :reason ':invalid-record-request)))
  nil)

(-> agent-protocol-record-request (agent-protocol-client agent-wire:record-request) integer)
(defun agent-protocol-record-request (client request)
  "Send one detached records.v1 request and return its integer request ID.
Receive its terminal RecordResponse or Failure through the existing reader or
sole-reader driver. This function does not retry writes. Scope is selected by
the parent, never by fields supplied by the caller."
  (let ((copy (agent-protocol--message-copy request)))
    (handler-case (agent-record--validate-request copy)
      (autolith-supervisor::protocol-value-error ()
        (error 'agent-protocol-error :reason ':invalid-record-request)))
    (agent-protocol--send
     client
     (lambda ()
       (unless (member "records.v1" (agent-wire:welcome.features (agent-protocol-client-welcome client))
                       :test #'equal)
         (error 'agent-protocol-error :reason ':records-not-negotiated))
       (when (agent-protocol-client-stop client)
         (error 'agent-protocol-error :reason ':stopping))
       (when (>= (hash-table-count (agent-protocol-client-pending client))
                 (agent-wire:limits.max-in-flight-requests
                  (autolith-supervisor::parent-connection-limits client)))
         (error 'agent-protocol-error :reason ':request-limit))
       (values (agent-wire:make-envelope :record-request copy)
               (make-instance 'agent-record-request :request copy))))))

(-> agent-record--stored-fields (agent-protocol-client agent-wire:stored-record) hash-table)
(defun agent-record--stored-fields (client record)
  "Decode every returned portable field under the negotiated value bounds."
  (unless (and (agent-record--string-p (agent-wire:stored-record.record-id record) 1024)
               (agent-record--string-p (agent-wire:stored-record.revision record) 1024)
               (agent-wire:stored-record.has-fields record))
    (agent-protocol--fatal client ':invalid-record-response))
  (let ((limits (autolith-supervisor::parent-connection-limits client)))
    (handler-case
        (autolith-supervisor::protocol-value->data
         (cl-protobufs.autolith.common.v1:make-value :map-value (agent-wire:stored-record.fields record))
         :max-depth   (agent-wire:limits.max-value-depth limits)
         :max-entries (agent-wire:limits.max-collection-entries limits))
      (autolith-supervisor::protocol-value-error ()
        (agent-protocol--fatal client ':invalid-record-response)))))

(-> agent-record--response-fields-p (hash-table keyword) boolean)
(defun agent-record--response-fields-p (fields collection)
  "Check collection-specific record data without accepting writable authority."
  (case collection
    (:memory
     (agent-record--writable-p fields collection ':replace))
    (:agenda
     (let ((writable (make-hash-table :test 'equal)))
       (and (loop for key being the hash-keys of fields using (hash-value value)
                  always (cond
                           ((member key '("created_at" "updated_at") :test #'equal)
                            (typep value '(integer 0)))
                           ((member key '("text" "status" "memory_ids") :test #'equal)
                            (setf (gethash key writable) value)
                            t)))
            (agent-record--writable-p writable collection ':replace))))
    (:papercut
     (and (agent-record--string-p (gethash "title" fields) 65536)
          (agent-record--string-p (gethash "content" fields) 65536)
          (not (null (member (gethash "status" fields) '("active" "closed") :test #'equal)))
          (loop for key being the hash-keys of fields using (hash-value value)
                always (cond
                         ((member key '("reported_at" "assessed_at") :test #'equal)
                          (typep value '(integer 0)))
                         ((equal key "verdict")
                          (member value '("improved" "worse" "unchanged" "too-early") :test #'equal))
                         ((equal key "issue_kind")
                          (member value '("broken_tool" "misleading_success" "authorization"
                                          "performance" "other") :test #'equal))
                         ((member key '("title" "content" "diagnostic" "tool" "source_conversation"
                                        "status" "note") :test #'equal)
                          (and (stringp value) (<= (length value) 65536)))))))))

(-> agent-record--write-result-p (agent-wire:record-request hash-table) boolean)
(defun agent-record--write-result-p (request fields)
  "Require exact submitted text in write results without comparing normalized lists."
  (let ((operation (agent-wire:record-request.operation request)))
    (and
     (or (not (eq operation ':close)) (equal (gethash "status" fields) "closed"))
     (or (not (member operation '(:create :replace :assess)))
         (let ((submitted (agent-record--fields request)))
           (loop for key being the hash-keys of submitted using (hash-value value)
                 always (or (not (member key '("title" "content" "text" "status" "verdict" "note")
                                         :test #'equal))
                            (equal value (gethash key fields)))))))))

(defmethod agent-protocol--response-data (client envelope (expected agent-record-request))
  "Validate one correlated record result under the existing client state lock."
  (case (agent-wire:envelope.body-case envelope)
    (agent-wire:failure
     (agent-protocol--failure client (agent-wire:envelope.failure envelope)))
    (agent-wire:record-response
     (let* ((request   (agent-record-request-message expected))
            (operation (agent-wire:record-request.operation request))
            (response  (agent-wire:envelope.record-response envelope))
            (records   (agent-wire:record-response.records response))
            (cursor-p  (agent-wire:record-response.has-next-cursor response))
            (seen      (make-hash-table :test 'equal)))
       (unless (and (if (eq operation ':query)
                        (<= (length records) (agent-wire:record-request.limit request))
                        (= (length records) (if (eq operation ':delete) 0 1)))
                    (or (not cursor-p)
                        (and (eq operation ':query) records
                             (agent-record--string-p (agent-wire:record-response.next-cursor response) 1024)
                             (not (equal (agent-wire:record-response.next-cursor response)
                                         (agent-wire:record-request.cursor request))))))
         (agent-protocol--fatal client ':invalid-record-response))
       (dolist (record records)
         (let ((id     (agent-wire:stored-record.record-id record))
               (fields (agent-record--stored-fields client record)))
           (unless (and (not (gethash id seen))
                        (or (member operation '(:query :create))
                            (string= id (agent-wire:record-request.record-id request)))
                        (agent-record--response-fields-p fields (agent-wire:record-request.collection request))
                        (agent-record--write-result-p request fields))
             (agent-protocol--fatal client ':invalid-record-response))
           (setf (gethash id seen) t)))))
    (otherwise
     (agent-protocol--fatal client ':unexpected-response)))
  t)
