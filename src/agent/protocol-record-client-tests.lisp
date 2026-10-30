;;;; -- Serialized Private Record Client Checks --

(in-package #:autolith)

(-> agent-record-test--map (list) cl-protobufs.autolith.common.v1:value-map)
(defun agent-record-test--map (pairs)
  "Encode test field pairs through the portable codec."
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
          do (setf (gethash key table) value))
    (cl-protobufs.autolith.common.v1:value.map-value
     (autolith-supervisor::protocol-data->value table))))

(-> agent-record-test--welcome () agent-wire:welcome)
(defun agent-record-test--welcome ()
  "Offer the record feature over the real test byte channel."
  (let ((welcome (agent-protocol-test--welcome)))
    (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1"))
    welcome))

(-> agent-record-test--record () agent-wire:stored-record)
(defun agent-record-test--record ()
  "Build a complete portable memory record."
  (agent-wire:make-stored-record
   :record-id "memory-1" :revision "revision-2"
   :fields (agent-record-test--map '("title" "Title" "content" "Body" "tags" #("tag")))))

(-> agent-record-test--query () agent-wire:record-request)
(defun agent-record-test--query ()
  "Build a bounded first-page query."
  (agent-wire:make-record-request :collection ':memory :operation ':query :limit 1))

(-> agent-record-test--write (keyword) agent-wire:record-request)
(defun agent-record-test--write (operation)
  "Build one complete memory write with exact revision preconditions."
  (let ((request (agent-wire:make-record-request :collection ':memory :operation operation
                                                :operation-id "operation-1")))
    (unless (eq operation ':create)
      (setf (agent-wire:record-request.record-id request) "memory-1"
            (agent-wire:record-request.expected-revision request) "revision-1"))
    (unless (eq operation ':delete)
      (setf (agent-wire:record-request.fields request)
            (agent-record-test--map '("title" "Title" "content" "Body" "tags" #("tag")))))
    request))

(-> agent-record-test--exchange (agent-wire:record-request agent-wire:envelope boolean) null)
(defun agent-record-test--exchange (request response invalid-p)
  "Check one serialized terminal result through the sole-reader driver."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-record-test--welcome))
     (let ((received (autolith-supervisor::parent-connection-receive parent)))
       (assert (eq 'agent-wire:record-request (agent-wire:envelope.body-case received)))
       (assert (equalp (cl-protobufs:serialize-to-bytes request)
                       (cl-protobufs:serialize-to-bytes (agent-wire:envelope.record-request received))))
       (setf (agent-wire:envelope.reply-to response) (agent-wire:envelope.message-id received))
       (autolith-supervisor::parent-connection-send parent response))
     ;; A single output is the test barrier. Any retry is an unexpected body.
     (unless invalid-p
       (assert (eq 'agent-wire:output
                   (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent))))))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "records.v1"))
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
            (let ((delivery
                    (agent-protocol-driver-request
                     driver (lambda (peer) (agent-protocol-record-request peer request)))))
              (if invalid-p
                  (progn
                    (assert (handler-case
                                (progn (agent-protocol-driver-next delivery :timeout 2) nil)
                              (agent-protocol-error () t)))
                    (assert (eq ':closed (autolith-supervisor::parent-connection-phase client))))
                  (multiple-value-bind (reply terminal) (agent-protocol-driver-next delivery :timeout 2)
                    (assert terminal)
                    (assert (eq (agent-wire:envelope.body-case response) (agent-wire:envelope.body-case reply)))
                    (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
                    (assert (handler-case (progn (agent-protocol-receive client) nil)
                              (agent-protocol-error (condition)
                                (eq ':reader-owned (agent-protocol-error-reason condition)))))
                    (agent-protocol-output client ':diagnostic "done"))))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-record-test--success () null)
(defun agent-record-test--success ()
  "Read a page, acknowledge writes, and accept terminal structured failures."
  (dolist (operation '(:query :create :replace :delete))
    (agent-record-test--exchange
     (if (eq operation ':query) (agent-record-test--query) (agent-record-test--write operation))
     (agent-wire:make-envelope
      :record-response (agent-wire:make-record-response
                        :records (unless (eq operation ':delete) (list (agent-record-test--record)))))
     nil))
  (agent-record-test--exchange
   (agent-record-test--query)
   (agent-wire:make-envelope :record-response
                             (agent-wire:make-record-response :records      (list (agent-record-test--record))
                                                               :next-cursor "page-2"))
   nil)
  (agent-record-test--exchange
   (agent-record-test--write ':replace)
   (agent-wire:make-envelope :failure (agent-wire:make-failure :code ':revision-conflict :retryable t)) nil)
  nil)

(-> agent-record-test--invalid-response () null)
(defun agent-record-test--invalid-response ()
  "Close on wrong bodies, invalid portable fields, page shape, or identity."
  (agent-record-test--exchange (agent-record-test--query)
                               (agent-wire:make-envelope :session-page (agent-wire:make-session-page)) t)
  (agent-record-test--exchange (agent-record-test--query)
                               (agent-wire:make-envelope :failure (agent-wire:make-failure)) t)
  (dolist (kind '(:empty-id :empty-revision :wrong-fields :duplicate :page-limit :write-cursor
                  :wrong-id :missing-write :delete-record :empty-cursor :cursor-without-records))
    (let* ((record   (agent-record-test--record))
           (request  (case kind
                       ((:write-cursor :wrong-id :missing-write)
                        (agent-record-test--write ':replace))
                       (:delete-record
                        (agent-record-test--write ':delete))
                       (otherwise
                        (agent-record-test--query))))
           (response (agent-wire:make-record-response :records (list record))))
      (case kind
        (:empty-id
         (setf (agent-wire:stored-record.record-id record) ""))
        (:empty-revision
         (setf (agent-wire:stored-record.revision record) ""))
        (:wrong-fields
         (setf (agent-wire:stored-record.fields record)
               (agent-record-test--map '("title" "Title" "content" "Body" "scope" "global"))))
        ((:duplicate :page-limit)
         (setf (agent-wire:record-response.records response) (list record (agent-record-test--record)))
         (when (eq kind ':duplicate) (setf (agent-wire:record-request.limit request) 2)))
        (:write-cursor
         (setf (agent-wire:record-response.next-cursor response) "next"))
        (:wrong-id
         (setf (agent-wire:stored-record.record-id record) "another-record"))
        (:missing-write
         (setf (agent-wire:record-response.records response) nil))
        (:empty-cursor
         (setf (agent-wire:record-response.next-cursor response) ""))
        (:cursor-without-records
         (setf (agent-wire:record-response.records response) nil
               (agent-wire:record-response.next-cursor response) "next")))
      (agent-record-test--exchange request (agent-wire:make-envelope :record-response response) t)))
  nil)

(-> agent-record-test--request-shapes () null)
(defun agent-record-test--request-shapes ()
  "Reject invalid operations and attempts to supply record authority in fields."
  (dolist (kind '(:query-operation-id :query-fields :query-id :query-revision :query-zero-limit
                  :query-large-limit :query-empty-cursor :write-no-operation-id :write-query
                  :write-limit :create-id :write-empty-revision :delete-fields :scope :unknown
                  :bad-tags :unsupported-operation :unsupported-collection))
    (let ((request (if (member kind '(:query-operation-id :query-fields :query-id :query-revision
                                      :query-zero-limit :query-large-limit :query-empty-cursor))
                       (agent-record-test--query)
                       (agent-record-test--write ':replace))))
      (case kind
        (:query-operation-id
         (setf (agent-wire:record-request.operation-id request) "write"))
        (:query-fields
         (setf (agent-wire:record-request.fields request) (agent-record-test--map nil)))
        (:query-id
         (setf (agent-wire:record-request.record-id request) "memory-1"))
        (:query-revision
         (setf (agent-wire:record-request.expected-revision request) "revision-1"))
        (:query-zero-limit
         (setf (agent-wire:record-request.limit request) 0))
        (:query-large-limit
         (setf (agent-wire:record-request.limit request) 257))
        (:query-empty-cursor
         (setf (agent-wire:record-request.cursor request) ""))
        (:write-no-operation-id
         (setf (agent-wire:record-request.operation-id request) ""))
        (:write-query
         (setf (agent-wire:record-request.query request) ""))
        (:write-limit
         (setf (agent-wire:record-request.limit request) 1))
        (:create-id
         (setf (agent-wire:record-request.operation request) ':create))
        (:write-empty-revision
         (setf (agent-wire:record-request.expected-revision request) ""))
        (:delete-fields
         (setf (agent-wire:record-request.operation request) ':delete))
        ((:scope :unknown :bad-tags)
         (setf (agent-wire:record-request.fields request)
               (agent-record-test--map
                (append '("title" "Title" "content" "Body")
                        (case kind (:scope
                                    '("scope" "global"))
                              (:unknown
                               '("extra" "text")) (:bad-tags
                                                   '("tags" "tag")))))))
        (:unsupported-operation
         (setf (agent-wire:record-request.operation request) ':close))
        (:unsupported-collection
         (setf (agent-wire:record-request.collection request) ':collection-unspecified)))
      (assert (handler-case (progn (agent-record--validate-request request) nil)
                (agent-protocol-error () t)))))
  nil)

(-> agent-record-test--detached () null)
(defun agent-record-test--detached ()
  "Detach caller data before sending and retain the original validation evidence."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-record-test--welcome))
     (let ((envelope (autolith-supervisor::parent-connection-receive parent)))
       (assert (equal "memory-1" (agent-wire:record-request.record-id
                                  (agent-wire:envelope.record-request envelope))))
       (assert (eq 'agent-wire:output
                   (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent))))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to (agent-wire:envelope.message-id envelope)
                                          :record-response
                                          (agent-wire:make-record-response
                                           :records (list (agent-record-test--record))))))
     (autolith-supervisor::parent-connection-receive parent))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "records.v1"))
     (let ((driver  (agent-protocol-driver-start client))
           (request (agent-record-test--write ':replace)))
       (unwind-protect
            (let ((delivery (agent-protocol-driver-request
                             driver (lambda (peer) (agent-protocol-record-request peer request)))))
              (setf (agent-wire:record-request.record-id request) "changed"
                    (agent-wire:record-request.fields request)
                    (agent-record-test--map '("title" "Changed" "content" "Changed")))
              (agent-protocol-output client ':diagnostic "mutated")
              (multiple-value-bind (reply terminal) (agent-protocol-driver-next delivery :timeout 2)
                (assert terminal)
                (assert (agent-wire:envelope.record-response reply)))
              (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-record-test--collections () null)
(defun agent-record-test--collections ()
  "Validate agenda and papercut schemas through serialized write responses."
  (dolist (collection '(:agenda :papercut))
    (dolist (operation (if (eq collection ':agenda) '(:create :replace) '(:create :assess :close)))
      (let* ((pairs   (if (eq collection ':agenda)
                          '("text" "Task" "status" "doing" "memory_ids" #("memory-1"))
                          (case operation
                            (:create
                             '("title" "Report" "content" "Details" "issue_kind" "other"))
                            (:assess
                             '("verdict" "improved" "note" "Checked"))
                            (:close
                             '("resolution" "Repaired")))))
             (request (agent-wire:make-record-request :collection collection :operation operation
                                                      :operation-id "operation-1"
                                                      :fields       (agent-record-test--map pairs)))
             (record  (agent-wire:make-stored-record
                       :record-id "record-1" :revision "revision-2"
                       :fields
                       (agent-record-test--map
                        (if (eq collection ':agenda)
                            (append pairs '("created_at" 1 "updated_at" 2))
                            (append '("title" "Report" "content" "Details")
                                    (if (eq operation ':assess) pairs)
                                    (list "status" (if (eq operation ':close) "closed" "active"))))))))
        (unless (eq operation ':create)
          (setf (agent-wire:record-request.record-id request) "record-1"
                (agent-wire:record-request.expected-revision request) "revision-1"))
        (agent-record-test--exchange
         request (agent-wire:make-envelope
                  :record-response (agent-wire:make-record-response :records (list record))) nil))))
  nil)

(-> run-agent-protocol-record-tests () integer)
(defun run-agent-protocol-record-tests ()
  "Run serialized record-client success, failure, and strict validation checks."
  (agent-record-test--request-shapes)
  (agent-record-test--success)
  (agent-record-test--invalid-response)
  (agent-record-test--detached)
  (agent-record-test--collections)
  5)
