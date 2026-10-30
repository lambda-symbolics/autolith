;;;; -- Parent Papercut Adapter --

(in-package #:autolith)

(defclass agent-protocol-papercut ()
  ((driver :initarg :driver :reader agent-protocol-papercut-driver
           :documentation "The connection's sole-reader driver."))
  (:documentation "A parent papercut client without private store paths."))

(defclass agent-protocol-papercut-record ()
  ((identifier :initarg :identifier :reader agent-protocol-papercut-record-identifier
               :type non-empty-string :documentation "The parent-issued report ID.")
   (revision :initarg :revision :reader agent-protocol-papercut-record-revision
             :type non-empty-string :documentation "The exact parent revision.")
   (fields :initarg :fields :reader agent-protocol-papercut-record-fields
           :type hash-table :documentation "Detached public fields, with no private workspace path."))
  (:documentation "One observed public record, separate from private store authority."))

(define-condition agent-protocol-papercut-error (error)
  ((code :initarg :code :reader agent-protocol-papercut-error-code
         :documentation "The parent's fixed public failure code."))
  (:documentation "A parent papercut request failed without a client retry.")
  (:report (lambda (condition stream)
             (format stream "Parent papercut request failed: ~A"
                     (agent-protocol-papercut-error-code condition)))))

(-> make-agent-protocol-papercut (t) agent-protocol-papercut)
(defun make-agent-protocol-papercut (driver)
  "Use DRIVER to access parent-owned papercut records."
  (make-instance 'agent-protocol-papercut :driver driver))

(-> agent-protocol-papercut--invalid (agent-protocol-papercut) nil)
(defun agent-protocol-papercut--invalid (adapter)
  "Close the connection on an invalid public papercut record."
  (let ((condition (make-condition 'agent-protocol-error :reason ':invalid-record-response)))
    (agent-protocol-driver-close (agent-protocol-papercut-driver adapter) :condition condition)
    (error condition)))

(-> agent-protocol-papercut--exchange
    (agent-protocol-papercut agent-wire:record-request) agent-wire:record-response)
(defun agent-protocol-papercut--exchange (adapter request)
  "Wait for one terminal reply through the sole-reader driver. Never retry."
  (let ((delivery
          (agent-protocol-driver-request
           (agent-protocol-papercut-driver adapter)
           (lambda (client) (agent-protocol-record-request client request)))))
    (multiple-value-bind (envelope terminal-p) (agent-protocol-driver-next delivery)
      (unless (and envelope terminal-p)
        (agent-protocol-papercut--invalid adapter))
      (cond
        ((agent-wire:envelope.has-record-response envelope)
         (agent-wire:envelope.record-response envelope))
        ((agent-wire:envelope.has-failure envelope)
         (error 'agent-protocol-papercut-error
                :code (agent-wire:failure.code (agent-wire:envelope.failure envelope))))
        (t
         (agent-protocol-papercut--invalid adapter))))))

(-> agent-protocol-papercut--record
    (agent-protocol-papercut agent-wire:stored-record) agent-protocol-papercut-record)
(defun agent-protocol-papercut--record (adapter record)
  "Detach a public record and check the papercut domain's field limits."
  (let* ((fields
           (handler-case
               (autolith-supervisor::protocol-value->data
                (cl-protobufs.autolith.common.v1:make-value
                 :map-value (agent-wire:stored-record.fields record))
                :max-depth 2 :max-entries 32)
             (autolith-supervisor::protocol-value-error ()
               (agent-protocol-papercut--invalid adapter))))
         (verdict     (gethash "verdict" fields))
         (note        (gethash "note" fields))
         (assessed-at (gethash "assessed_at" fields)))
    (unless (and (agent-record--response-fields-p fields ':papercut)
                 (agent-record--string-p (gethash "title" fields) 200)
                 (agent-record--string-p (gethash "content" fields) 8000)
                 (typep (gethash "reported_at" fields) 'timestamp)
                 (member (gethash "issue_kind" fields)
                         '("broken_tool" "misleading_success" "authorization" "performance" "other")
                         :test #'equal)
                 (or (not (nth-value 1 (gethash "diagnostic" fields)))
                     (agent-record--string-p (gethash "diagnostic" fields) 8000))
                 (or (not (nth-value 1 (gethash "tool" fields)))
                     (agent-record--string-p (gethash "tool" fields) 1024))
                 (or (not (nth-value 1 (gethash "source_conversation" fields)))
                     (agent-record--string-p (gethash "source_conversation" fields) 1024))
                 (if verdict
                     (and (agent-record--string-p note 1000) (typep assessed-at 'timestamp))
                     (and (not (nth-value 1 (gethash "verdict" fields)))
                          (not (nth-value 1 (gethash "note" fields)))
                          (not (nth-value 1 (gethash "assessed_at" fields))))))
      (agent-protocol-papercut--invalid adapter))
    (make-instance 'agent-protocol-papercut-record
                   :identifier (copy-seq (agent-wire:stored-record.record-id record))
                   :revision   (copy-seq (agent-wire:stored-record.revision record))
                   :fields     fields)))

(-> agent-protocol-papercut--fields (list) cl-protobufs.autolith.common.v1:value-map)
(defun agent-protocol-papercut--fields (pairs)
  "Encode only explicit writable fields. Omit absent optional values."
  (let ((fields (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (when (rest pair)
        (setf (gethash (first pair) fields) (rest pair))))
    (cl-protobufs.autolith.common.v1:value.map-value
     (autolith-supervisor::protocol-data->value fields))))

(-> agent-protocol-papercut-query
    (agent-protocol-papercut &key (:query string) (:limit integer) (:cursor (option string)))
    (values list (option string) (option string)))
(defun agent-protocol-papercut-query (adapter &key (query "") (limit 256) cursor)
  "Read one page of public reports and return its optional parent cursor."
  (let ((response
          (agent-protocol-papercut--exchange
           adapter (apply #'agent-wire:make-record-request
                          :collection ':papercut :operation ':query :query query :limit limit
                          (when cursor (list :cursor cursor))))))
    (values (mapcar (lambda (record) (agent-protocol-papercut--record adapter record))
                    (agent-wire:record-response.records response))
            (when (agent-wire:record-response.has-next-cursor response)
              (agent-wire:record-response.next-cursor response))
            (when (agent-wire:record-response.has-collection-revision response)
              (agent-wire:record-response.collection-revision response)))))

(-> agent-protocol-papercut--write-result
    (agent-protocol-papercut agent-wire:record-response) agent-protocol-papercut-record)
(defun agent-protocol-papercut--write-result (adapter response)
  "Require exactly one resulting record without a query cursor."
  (unless (and (= 1 (length (agent-wire:record-response.records response)))
               (not (agent-wire:record-response.has-next-cursor response)))
    (agent-protocol-papercut--invalid adapter))
  (agent-protocol-papercut--record adapter (first (agent-wire:record-response.records response))))

(-> agent-protocol-papercut-create
    (agent-protocol-papercut &key (:title string) (:content string) (:diagnostic (option string))
     (:issue-kind string) (:tool (option string)) (:operation-id string) (:expected-collection-revision (option string)))
    agent-protocol-papercut-record)
(defun agent-protocol-papercut-create
       (adapter
        &key title content diagnostic (issue-kind "other") tool (operation-id (make-identifier))
        expected-collection-revision)
  "Create a report. Retain OPERATION-ID when the terminal outcome is unknown."
  (agent-protocol-papercut--write-result adapter
   (agent-protocol-papercut--exchange adapter
    (apply #'agent-wire:make-record-request :collection ':papercut :operation ':create
           :operation-id operation-id :fields
           (agent-protocol-papercut--fields
            (list (cons "title" title) (cons "content" content) (cons "diagnostic" diagnostic)
                  (cons "issue_kind" issue-kind) (cons "tool" tool)))
           (when expected-collection-revision
             (list :expected-collection-revision expected-collection-revision))))))

(-> agent-protocol-papercut-assess
    (agent-protocol-papercut agent-protocol-papercut-record
     &key (:verdict (member :improved :worse :unchanged :too-early))
     (:note string) (:diagnostic (option string)) (:operation-id string))
    agent-protocol-papercut-record)
(defun agent-protocol-papercut-assess
    (adapter record &key verdict note diagnostic (operation-id (make-identifier)))
  "Assess RECORD against its exact parent revision, without retrying."
  (agent-protocol-papercut--write-result
   adapter (agent-protocol-papercut--exchange
            adapter (agent-wire:make-record-request
                     :collection ':papercut :operation ':assess :operation-id operation-id
                     :record-id         (agent-protocol-papercut-record-identifier record)
                     :expected-revision (agent-protocol-papercut-record-revision record)
                     :fields            (agent-protocol-papercut--fields
                                         (list (cons "verdict" (string-downcase (symbol-name verdict)))
                                               (cons "note" note) (cons "diagnostic" diagnostic)))))))

(-> agent-protocol-papercut-close
    (agent-protocol-papercut agent-protocol-papercut-record
     &key (:resolution string) (:diagnostic (option string)) (:operation-id string))
    agent-protocol-papercut-record)
(defun agent-protocol-papercut-close
    (adapter record &key resolution diagnostic (operation-id (make-identifier)))
  "Close RECORD against its exact parent revision, without retrying."
  (agent-protocol-papercut--write-result
   adapter (agent-protocol-papercut--exchange
            adapter (agent-wire:make-record-request
                     :collection ':papercut :operation ':close :operation-id operation-id
                     :record-id         (agent-protocol-papercut-record-identifier record)
                     :expected-revision (agent-protocol-papercut-record-revision record)
                     :fields            (agent-protocol-papercut--fields
                                         (list (cons "resolution" resolution) (cons "diagnostic" diagnostic)))))))
