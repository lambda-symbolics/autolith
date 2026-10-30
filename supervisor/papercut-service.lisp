(in-package #:autolith-supervisor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:parent-proto '#:cl-protobufs.autolith.parent.v1
                                     '#:autolith-supervisor)
  (sb-ext:add-package-local-nickname '#:common-proto '#:cl-protobufs.autolith.common.v1
                                     '#:autolith-supervisor))

;;;; -- Papercut Record Service --

(define-condition papercut-service-error (error)
  ((code :initarg :code :reader papercut-service-error-code
         :documentation "A fixed public protocol failure code."))
  (:documentation "A papercut request failed without disclosing private store details.")
  (:report (lambda (condition stream)
             (format stream "Papercut request failed: ~A"
                     (papercut-service-error-code condition)))))

(defclass papercut-service ()
  ((collection-snapshots :initform (make-hash-table :test #'equal)
              :reader              papercut-service-collection-snapshots
              :documentation       "Bounded parent-issued exact scope observations.")
   (context :initarg :context :reader papercut-service-context :type papercut-context
    :documentation "The trusted private store and workspace authority.")
   (session-id :initarg :session-id :reader papercut-service-session-id :type string
               :documentation "The trusted durable session identity.")
   (cursors :initform (make-hash-table :test #'equal) :reader papercut-service-cursors
            :documentation "Bounded parent-issued, single-use query snapshots.")
   (lock :initform (bordeaux-threads:make-lock "papercut service")
         :reader papercut-service-lock :documentation "Serialize service requests.")
   (max-response-bytes :initarg       :max-response-bytes
                       :reader        papercut-service-max-response-bytes
                       :documentation "The trusted response byte budget, excluding envelope."))
  (:documentation "The parent-owned PAPERCUT collection of records.v1."))

(serapeum/types:-> papercut-service--reject (keyword) nil)
(defun papercut-service--reject (code)
  "Signal a fixed public failure CODE."
  (error 'papercut-service-error :code code))

(serapeum/types:-> make-papercut-service
                  (&key (:context papercut-context) (:session-id string)
                   (:max-response-bytes integer)) papercut-service)
(defun make-papercut-service (&key context session-id (max-response-bytes 65536))
  "Attach trusted store authority, session identity, and response budget."
  (unless (and (typep context 'papercut-context)
               (papercut--string-p session-id) (<= (length session-id) 1024)
               (typep max-response-bytes '(integer 256 1048576)))
    (papercut-service--reject ':invalid-request))
  (papercut--workspace context)
  (make-instance 'papercut-service :context context :session-id (copy-seq session-id)
                 :max-response-bytes max-response-bytes))

(serapeum/types:-> papercut-service--fields (parent-proto:record-request) hash-table)
(defun papercut-service--fields (request)
  "Decode bounded writable fields without interning peer strings."
  (handler-case
      (if (parent-proto:record-request.has-fields request)
          (protocol-value->data
           (common-proto:make-value :map-value (parent-proto:record-request.fields request))
           :max-depth 2 :max-entries 32)
          (make-hash-table :test #'equal))
    (protocol-value-error () (papercut-service--reject ':invalid-request))))

(serapeum/types:-> papercut-service--normalize-fields (keyword hash-table) list)
(defun papercut-service--normalize-fields (operation fields)
  "Validate the exact public fields for one lifecycle operation."
  (let ((allowed (case operation
                   (:create
                    '("title" "content" "diagnostic" "issue_kind" "tool"))
                   (:assess
                    '("verdict" "note" "diagnostic"))
                   (:close
                    '("resolution" "diagnostic")))))
    (unless (loop for key being the hash-keys of fields
                  always (member key allowed :test #'equal))
      (papercut-service--reject ':invalid-request)))
  (handler-case
      (let ((diagnostic (papercut--diagnostic (gethash "diagnostic" fields))))
        (case operation
          (:create
           (list :title      (papercut--validate-text (gethash "title" fields) "title" 200)
                 :content    (papercut--validate-text (gethash "content" fields) "content" 8000)
                 :diagnostic diagnostic
                 :issue-kind (papercut--issue-kind (gethash "issue_kind" fields "other"))
                 :tool       (papercut--tool (gethash "tool" fields))))
          (:assess
           (let ((verdict (rest (assoc (gethash "verdict" fields)
                                      '(("improved" . :improved) ("worse" . :worse)
                                        ("unchanged" . :unchanged) ("too-early" . :too-early))
                                      :test #'equal))))
             (unless verdict (papercut-service--reject ':invalid-request))
             (list :verdict    verdict
                   :note       (papercut--validate-text (gethash "note" fields) "note" 1000)
                   :diagnostic diagnostic)))
          (:close
           (list :resolution (papercut--validate-text (gethash "resolution" fields)
                                                     "resolution" 1000)
                 :diagnostic diagnostic))))
    (papercut-error () (papercut-service--reject ':invalid-request))))

(serapeum/types:-> papercut-service--validate-request
                  (parent-proto:record-request hash-table) list)
(defun papercut-service--validate-request (request fields)
  "Reject unsupported collections, shapes, operations, and scope fields."
  (record-collection--validate request #'papercut-service--reject)
  (unless (eq (parent-proto:record-request.collection request) ':papercut)
    (papercut-service--reject ':unsupported))
  (let ((operation    (parent-proto:record-request.operation request))
        (operation-id (parent-proto:record-request.operation-id request))
        (id-p         (parent-proto:record-request.has-record-id request))
        (revision-p   (parent-proto:record-request.has-expected-revision request))
        (query-p      (parent-proto:record-request.has-query request))
        (cursor-p     (parent-proto:record-request.has-cursor request))
        (limit        (parent-proto:record-request.limit request)))
    (case operation
      (:query
       (unless (and (zerop (length operation-id)) (not id-p) (not revision-p)
                    (not (parent-proto:record-request.has-fields request))
                    (<= 1 limit 256)
                    (<= (length (parent-proto:record-request.query request)) 1024)
                    (or (not cursor-p)
                        (and (papercut--string-p (parent-proto:record-request.cursor request))
                             (<= (length (parent-proto:record-request.cursor request)) 1024))))
         (papercut-service--reject ':invalid-request))
       nil)
      ((:create :assess :close)
       (unless (and (papercut--string-p operation-id) (<= (length operation-id) 1024)
                    (not query-p) (not cursor-p) (zerop limit)
                    (parent-proto:record-request.has-fields request)
                    (if (eq operation ':create)
                        (and (not id-p) (not revision-p))
                        (and id-p revision-p
                             (papercut--string-p (parent-proto:record-request.record-id request))
                             (<= (length (parent-proto:record-request.record-id request)) 1024)
                             (papercut--string-p (parent-proto:record-request.expected-revision request))
                             (<= (length (parent-proto:record-request.expected-revision request)) 1024))))
         (papercut-service--reject ':invalid-request))
       (papercut-service--normalize-fields operation fields))
      (otherwise
       (papercut-service--reject ':unsupported)))))

(serapeum/types:-> papercut-service--snapshot (papercut &key (:status string)) list)
(defun papercut-service--snapshot (report &key (status "active"))
  "Return a complete portable result snapshot for a durable receipt."
  (list :report (papercut--record report) :status status
        :verdict (papercut-assessment-verdict report)
        :note (papercut-assessment-note report) :assessed-at (papercut-assessed-at report)))

(serapeum/types:-> papercut-service--revision (list) string)
(defun papercut-service--revision (snapshot)
  "Digest the complete public state for revision guards."
  (let ((*print-readably* t) (*print-pretty* nil) (*package* (find-package :keyword)))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence ':sha256
                               (babel:string-to-octets (write-to-string snapshot)
                                                       :encoding ':utf-8)))))

(serapeum/types:-> papercut-service--stored-record (list string) parent-proto:stored-record)
(defun papercut-service--stored-record (snapshot revision)
  "Project only public report fields, excluding workspace paths and export identity."
  (let* ((report (rest (getf snapshot :report)))
         (fields (make-hash-table :test #'equal)))
    (when (or (> (length (getf report :id)) 1024)
              (and (getf report :source-conversation)
                   (> (length (getf report :source-conversation)) 1024)))
      (papercut-service--reject ':resource-limit))
    (dolist (pair '(("title" . :title) ("content" . :content)
                    ("diagnostic" . :diagnostic) ("issue_kind" . :issue-kind)
                    ("tool" . :tool) ("reported_at" . :reported-at)
                    ("source_conversation" . :source-conversation)))
      (when (getf report (rest pair))
        (setf (gethash (first pair) fields) (getf report (rest pair)))))
    (setf (gethash "status" fields) (getf snapshot :status))
    (when (getf snapshot :verdict)
      (setf (gethash "verdict" fields) (string-downcase (symbol-name (getf snapshot :verdict)))
            (gethash "note" fields) (getf snapshot :note)
            (gethash "assessed_at" fields) (getf snapshot :assessed-at)))
    (parent-proto:make-stored-record
     :record-id (getf report :id) :revision revision
     :fields (common-proto:value.map-value (protocol-data->value fields)))))

(serapeum/types:-> papercut-service--receipt-response (list) parent-proto:record-response)
(defun papercut-service--receipt-response (receipt)
  "Rebuild the original public response from the committed receipt."
  (parent-proto:make-record-response
   :records (list (papercut-service--stored-record
                   (getf (rest receipt) :result) (getf (rest receipt) :revision)))))

(serapeum/types:-> papercut-service--response-fits-p
                  (papercut-service parent-proto:record-response) boolean)
(defun papercut-service--response-fits-p (service response)
  "Check the exact encoded response against the trusted byte budget."
  (<= (length (cl-protobufs:serialize-to-bytes response))
      (papercut-service-max-response-bytes service)))

(serapeum/types:-> papercut-service--bounded-response
                  (papercut-service parent-proto:record-response) parent-proto:record-response)
(defun papercut-service--bounded-response (service response)
  "Return RESPONSE only when it fits the trusted transport budget."
  (unless (papercut-service--response-fits-p service response)
    (papercut-service--reject ':resource-limit))
  response)

(serapeum/types:-> papercut-service--write
                  (papercut-service parent-proto:record-request list) parent-proto:record-response)
(defun papercut-service--write (service request fields)
  "Atomically commit lifecycle events and the exact idempotent operation receipt."
  (let* ((context      (papercut-service-context service))
         (workspace    (papercut--workspace context))
         (session      (papercut-service-session-id service))
         (operation    (parent-proto:record-request.operation request))
         (operation-id (parent-proto:record-request.operation-id request))
         (id           (parent-proto:record-request.record-id request))
         (expected     (parent-proto:record-request.expected-revision request))
         (payload      (append (list operation id expected fields)
                               (when (parent-proto:record-request.has-expected-collection-revision request)
                                 (list (parent-proto:record-request.expected-collection-revision request))))))
    (papercut-call-with-transaction context
      (lambda (state)
        (let ((previous
                (find-if (lambda (entry)
                           (and (equal session (getf (rest entry) :session))
                                (equal operation-id (getf (rest entry) :id))))
                         (second state))))
          (when previous
            (unless (and (equal workspace (getf (rest previous) :workspace))
                         (equal payload (getf (rest previous) :payload)))
              (papercut-service--reject ':invalid-request))
            (return-from papercut-service--write
              (papercut-service--bounded-response service
                (papercut-service--receipt-response previous)))))
        (record-collection--guard request :issued (papercut-service-collection-snapshots service)
         :snapshot
         (list workspace
               (mapcar #'papercut-service--snapshot
                       (remove-if-not
                        (lambda (report) (equal workspace (papercut-workspace report)))
                        (first state))))
         :reject                                  #'papercut-service--reject)        (when (>= (length (second state)) 16384)
                                                                                           (papercut-service--reject ':resource-limit))
        (let* ((active (first state))
               (report (find-if (lambda (item)
                                  (and (equal id (papercut-identifier item))
                                       (equal workspace (papercut-workspace item)))) active)))
          (unless (eq operation ':create)
            (unless (and report (equal expected (papercut-service--revision
                                                 (papercut-service--snapshot report))))
              (papercut-service--reject ':revision-conflict)))
          (multiple-value-bind (events result)
              (case operation
                (:create
                 (multiple-value-bind (records result)
                     (apply #'papercut--report-unlocked context :active active
                            :source-conversation                        session fields)
                   (values records (first result))))
                (:assess
                 (apply #'papercut--assess-unlocked context id :active active fields))
                (:close
                 (papercut--mark-closed-unlocked context id (getf fields :resolution)
                   :active active :diagnostic (getf fields :diagnostic))))
            (let* ((snapshot (papercut-service--snapshot result
                               :status (if (eq operation ':close) "closed" "active")))
                   (receipt  (list :papercut-operation :version 1 :session session
                                   :workspace workspace :id operation-id :payload payload
                                   :result snapshot :revision (papercut-service--revision snapshot)))
                   (response (papercut-service--receipt-response receipt)))
              (papercut-service--bounded-response service response)
              (values (append events (list receipt)) response t))))))))

(serapeum/types:-> papercut-service--query
                  (papercut-service parent-proto:record-request) parent-proto:record-response)
(defun papercut-service--query (service request)
  "Read a stable, byte-bounded page with bounded single-use snapshot cursors."
  (let* ((query       (parent-proto:record-request.query request))
         (cursor      (parent-proto:record-request.cursor request))
         (cursor-p    (parent-proto:record-request.has-cursor request))
         (table       (papercut-service-cursors service))
         (saved       (and cursor-p (gethash cursor table)))
         (workspace   (papercut--workspace (papercut-service-context service)))
         (all-reports (unless saved (papercut-list (papercut-service-context service))))
         (snapshot    (unless saved (list workspace (mapcar #'papercut-service--snapshot all-reports))))
         (text        (unless saved (record-collection--text snapshot)))
         (revision    (if saved (fourth saved) (record-collection--revision text)))
         (issued      (papercut-service-collection-snapshots service)))
    (when (and cursor-p (or (not saved) (not (equal query (first saved)))
                           (not (equal workspace (third saved)))))
      (papercut-service--reject ':invalid-request))
    (unless saved (record-collection--check-capacity issued :revision revision :text text :reject
                   #'papercut-service--reject))
    (let* ((records
             (if saved (second saved)
                 (let ((matches
                         (remove-if-not
                          (lambda (report)
                            (or (zerop (length query))
                                (search query (papercut-title report) :test #'char-equal)
                                (search query (papercut-content report) :test #'char-equal)))
                          all-reports)))
                   (when (> (length matches) 4096)
                     (papercut-service--reject ':resource-limit))
                   (mapcar (lambda (report)
                             (let ((snapshot (papercut-service--snapshot report)))
                               (papercut-service--stored-record snapshot
                                 (papercut-service--revision snapshot)))) matches))))
           (page (subseq records 0 (min (parent-proto:record-request.limit request)
                                       (length records))))
           (next (papercut--make-identifier)))
      (loop
        (let* ((remaining (nthcdr (length page) records))
               (response  (if remaining
                              (parent-proto:make-record-response :records page :next-cursor next :collection-revision revision)
                              (parent-proto:make-record-response :records page :collection-revision revision))))
          (when (papercut-service--response-fits-p service response)
            (when remaining
              (let ((retained (loop for key being the hash-keys of table using (hash-value value)
                                    unless (and cursor-p (equal key cursor))
                                      append (second value))))
                (when (or (>= (- (hash-table-count table) (if cursor-p 1 0)) 64)
                          (> (+ (length retained) (length remaining)) 4096)
                          (> (loop for record in (append retained remaining)
                                   sum (length (cl-protobufs:serialize-to-bytes record)))
                             4194304))
                  (papercut-service--reject ':resource-limit))))
            (when cursor-p (remhash cursor table))
            (when remaining (setf (gethash next table) (list (copy-seq query) remaining workspace revision)))
            (unless saved (setf (gethash revision issued) text))
            (return response))
          (when (<= (length page) 1) (papercut-service--reject ':resource-limit))
          (setf page (butlast page)))))))

(serapeum/types:-> papercut-service-handle
                  (papercut-service parent-proto:record-request) parent-proto:record-response)
(defun papercut-service-handle (service request)
  "Serve a PAPERCUT request after the dispatcher checks records.v1 selection."
  (bordeaux-threads:with-lock-held ((papercut-service-lock service))
    (handler-case
        (let ((fields (papercut-service--validate-request
                       request (papercut-service--fields request))))
          (papercut-service--bounded-response service
            (if (eq (parent-proto:record-request.operation request) ':query)
                (papercut-service--query service request)
                (papercut-service--write service request fields))))
      (papercut-error () (papercut-service--reject ':invalid-request))
      (protocol-value-error () (papercut-service--reject ':internal)))))
