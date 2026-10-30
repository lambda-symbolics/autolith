(in-package #:autolith-supervisor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:parent-proto '#:cl-protobufs.autolith.parent.v1
                                     '#:autolith-supervisor)
  (sb-ext:add-package-local-nickname '#:common-proto '#:cl-protobufs.autolith.common.v1
                                     '#:autolith-supervisor))

(define-condition agenda-service-error
    (error)
    ((code :initarg :code :reader agenda-service-error-code :documentation
      "A stable public protocol failure code."))
  (:documentation "An agenda request failed without private record details.")
  (:report
   (lambda (condition stream)
     (format stream "Agenda request failed: ~A"
             (agenda-service-error-code condition)))))

(defclass agenda-service nil
          ((collection-snapshots :initform (make-hash-table :test #'equal)
              :reader                      agenda-service-collection-snapshots
              :documentation               "Bounded parent-issued exact scope observations.")
           (context :initarg :context :reader agenda-service-context :type
            agenda-context :documentation
            "The trusted parent store and workspace authority.")
           (session-id :initarg :session-id :reader agenda-service-session-id :type
            string :documentation
            "The trusted durable session identity for deduplication.")
           (cursors :initform (make-hash-table :test #'equal) :reader
            agenda-service-cursors :documentation
            "Bounded parent-issued query snapshots.")
           (lock :initform (bordeaux-threads:make-lock "agenda record service") :reader
            agenda-service-lock :documentation
            "Serialize requests and cursor updates.")
           (max-response-bytes :initarg :max-response-bytes :reader
            agenda-service-max-response-bytes :documentation
            "Trusted portable-response byte budget, excluding the envelope."))
          (:documentation
           "The agenda collection of records.v1, with no agent dependency."))

(serapeum/types:-> make-agenda-service
                   (&key (:context agenda-context) (:session-id string)
                    (:max-response-bytes integer))
                   agenda-service)
(defun make-agenda-service (&key context session-id (max-response-bytes 65536))
  "Attach trusted store authority, session identity, and a response byte budget."
  (unless
      (and (typep context 'agenda-context) (agenda--string-p session-id)
           (<= (length session-id) 1024)
           (typep max-response-bytes '(integer 256 1048576)))
    (error 'agenda-service-error :code ':invalid-request))
  (make-instance 'agenda-service :context context :session-id (copy-seq session-id)
                 :max-response-bytes max-response-bytes))

(serapeum/types:-> agenda-service--reject (keyword) nil)
(defun agenda-service--reject (code)
  "Signal a fixed public rejection CODE."
  (error 'agenda-service-error :code code))

(serapeum/types:-> agenda-service--fields (parent-proto:record-request) hash-table)
(defun agenda-service--fields (request)
  "Decode and bound writable fields without interning untrusted names."
  (handler-case
   (if (parent-proto:record-request.has-fields request)
       (protocol-value->data
        (common-proto:make-value :map-value
                                 (parent-proto:record-request.fields request))
        :max-depth 4 :max-entries 64)
       (make-hash-table :test #'equal))
   (protocol-value-error nil (agenda-service--reject ':invalid-request))))

(serapeum/types:-> agenda-service--normalize-fields (hash-table) list)
(defun agenda-service--normalize-fields (fields)
  "Validate complete agenda writable fields and return portable canonical data."
  (unless
      (and
       (every
        (lambda (key) (member key '("text" "status" "memory_ids") :test #'string=))
        (loop for key being the hash-keys of fields
              collect key))
       (agenda--string-p (gethash "text" fields))
       (<= (length (gethash "text" fields)) *agenda-item-text-limit*)
       (member (gethash "status" fields) '("todo" "doing" "blocked" "done" "note")
               :test #'equal))
    (agenda-service--reject ':invalid-request))
  (let* ((memory-data (gethash "memory_ids" fields #()))
         (identifiers
          (and (vectorp memory-data) (not (stringp memory-data))
               (coerce memory-data 'list))))
    (unless
        (and (vectorp memory-data) (not (stringp memory-data))
             (agenda--memory-identifiers-p identifiers))
      (agenda-service--reject ':invalid-request))
    (list :text (copy-seq (gethash "text" fields)) :status
          (rest
           (assoc (gethash "status" fields)
                  '(("todo" . :todo) ("doing" . :doing) ("blocked" . :blocked)
                    ("done" . :done) ("note" . :note))
                  :test #'string=))
          :memory-ids (mapcar #'copy-seq identifiers) :memory-ids-present-p
          (nth-value 1 (gethash "memory_ids" fields)))))

(serapeum/types:-> agenda-service--validate-request
                   (parent-proto:record-request hash-table) list)
(defun agenda-service--validate-request (request fields)
  "Validate request shape before accessing private agenda state."
  (record-collection--validate request #'agenda-service--reject)
  (unless (eq (parent-proto:record-request.collection request) ':agenda)
    (agenda-service--reject ':unsupported))
  (let ((operation    (parent-proto:record-request.operation request))
        (id-p         (parent-proto:record-request.has-record-id request))
        (revision-p   (parent-proto:record-request.has-expected-revision request))
        (query-p      (parent-proto:record-request.has-query request))
        (cursor-p     (parent-proto:record-request.has-cursor request))
        (operation-id (parent-proto:record-request.operation-id request))
        (limit        (parent-proto:record-request.limit request)))
    (case operation
      (:query
       (unless
           (and (zerop (length operation-id)) (not id-p) (not revision-p)
                (not (parent-proto:record-request.has-fields request)) (<= 1 limit 256)
                (<= (length (parent-proto:record-request.query request)) 1024)
                (or (not cursor-p)
                    (agenda--string-p (parent-proto:record-request.cursor request))))
         (agenda-service--reject ':invalid-request))
       nil)
      ((:create :replace :delete)
       (unless
           (and (agenda--string-p operation-id) (<= (length operation-id) 1024)
                (not query-p) (not cursor-p) (zerop limit)
                (if (eq operation ':create)
                    (and (not id-p) (not revision-p))
                    (and id-p revision-p
                         (agenda--string-p
                          (parent-proto:record-request.record-id request))
                         (<= (length (parent-proto:record-request.record-id request))
                             1024)
                         (agenda--string-p
                          (parent-proto:record-request.expected-revision request))
                         (<=
                          (length
                           (parent-proto:record-request.expected-revision request))
                          1024))))
         (agenda-service--reject ':invalid-request))
       (if (eq operation ':delete)
           (progn
            (when (parent-proto:record-request.has-fields request)
              (agenda-service--reject ':invalid-request))
            nil)
           (agenda-service--normalize-fields fields)))
      (otherwise
       (agenda-service--reject ':unsupported)))))

(serapeum/types:-> agenda-service--item-revision (agenda-state string agenda-item)
                   string)
(defun agenda-service--item-revision (state directory item)
  "Return the committed receipt revision or a stable legacy item digest."
  (let* ((form (agenda--item->form item))
         (receipt
          (find-if
           (lambda (entry)
             (let* ((properties (rest entry)) (result (getf properties :result)))
               (and (string= directory (getf properties :directory)) result
                    (string= (agenda-item-identifier item) (getf (rest result) :id)))))
           (agenda-state-operations state))))
    (if (and receipt (equal form (getf (rest receipt) :result)))
        (getf (rest receipt) :revision)
        (let ((*print-readably* t)
              (*print-pretty*   nil)
              (*package*        (find-package :keyword)))
          (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence ':sha256
                                     (babel:string-to-octets (write-to-string form)
                                                             :encoding ':utf-8)))))))

(serapeum/types:-> agenda-service--stored-record (agenda-item string)
                   parent-proto:stored-record)
(defun agenda-service--stored-record (item revision)
  "Return only typed portable agenda record fields."
  (when (> (length (agenda-item-identifier item)) 1024)
    (agenda-service--reject ':resource-limit))
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "text" fields) (agenda-item-text item)
          (gethash "status" fields)
          (string-downcase (symbol-name (agenda-item-status item)))
          (gethash "memory_ids" fields)
          (coerce (agenda-item-memory-identifiers item) 'vector)
          (gethash "created_at" fields) (agenda-item-created-at item)
          (gethash "updated_at" fields) (agenda-item-updated-at item))
    (parent-proto:make-stored-record :record-id (agenda-item-identifier item) :revision
                                     revision :fields
                                     (common-proto:value.map-value
                                      (protocol-data->value fields)))))

(serapeum/types:-> agenda-service--receipt-response (list) parent-proto:record-response)
(defun agenda-service--receipt-response (receipt)
  "Rebuild the exact original write response from a durable receipt."
  (let* ((properties (rest receipt)) (result (getf properties :result)))
    (parent-proto:make-record-response :records
                                       (when result
                                         (list
                                          (agenda-service--stored-record
                                           (agenda--item-form->item result
                                            *agenda-version*)
                                           (getf properties :revision)))))))

(serapeum/types:-> agenda-service--write
                   (agenda-service parent-proto:record-request list)
                   parent-proto:record-response)
(defun agenda-service--write (service request fields)
  "Commit an agenda mutation, payload identity, and response in one transaction."
  (let* ((context      (agenda-service-context service))
         (directory    (agenda-context-directory context))
         (operation    (parent-proto:record-request.operation request))
         (id           (parent-proto:record-request.record-id request))
         (expected     (parent-proto:record-request.expected-revision request))
         (operation-id (parent-proto:record-request.operation-id request))
         (session-id   (agenda-service-session-id service))
         (payload      (append (list operation id expected fields)
                               (when (parent-proto:record-request.has-expected-collection-revision request)
                                 (list (parent-proto:record-request.expected-collection-revision request)))))
         (state        (make-instance 'agenda-state)))
    (agenda-call-with-transaction context state
     (lambda (current)
       (let ((previous
              (find-if
               (lambda (entry)
                 (let ((properties (rest entry)))
                   (and (string= session-id (getf properties :session))
                        (string= operation-id (getf properties :id)))))
               (agenda-state-operations current))))
         (when previous
           (unless
               (and (string= directory (getf (rest previous) :directory))
                    (equal payload (getf (rest previous) :payload)))
             (agenda-service--reject ':invalid-request))
           (return-from agenda-service--write
             (agenda-service--receipt-response previous))))
       (progn
        (when (>= (length (agenda-state-operations current)) 16384)
          (agenda-service--reject ':resource-limit))
        (let* ((record (agenda-find current directory))
               (item
                (and record
                     (find id (workspace-agenda-items record) :key
                           #'agenda-item-identifier :test #'string=))))
          (unless (eq operation ':create)
            (unless
                (and item
                     (string= expected
                              (agenda-service--item-revision current directory item)))
              (agenda-service--reject ':revision-conflict)))
          (record-collection--guard request :issued (agenda-service-collection-snapshots service)
           :snapshot
           (list directory
                 (mapcar #'agenda--item->form (and record (workspace-agenda-items record))))
           :reject                                  #'agenda-service--reject)          (let* ((result
                                                                                                         (case operation
                                                                                                           (:create
                                                                                                            (agenda--add-unlocked :configuration context :state current :text
                                                                                                             (getf fields :text) :status (getf fields :status)
                                                                                                             :memory-identifiers (getf fields :memory-ids)))
                                                                                                           (:replace
                                                                                                            (agenda--update-unlocked context current id :text
                                                                                                             (getf fields :text) :status (getf fields :status)
                                                                                                             :memory-identifiers (getf fields :memory-ids)))
                                                                                                           (:delete
                                                                                                            (agenda--remove-unlocked context current id) nil)))
                                                                                              (receipt
                                                                                               (list ':operation :session session-id :directory directory :id
                                                                                                     operation-id :payload payload :result
                                                                                                     (and result (agenda--item->form result)) :revision
                                                                                                     (agenda--make-identifier))))
                                                                                             (progn
                                                                                              (agenda-service--bounded-response service
                                                                                               (agenda-service--receipt-response receipt))
                                                                                              (push receipt (agenda-state-operations current)))
                                                                                             (agenda-service--receipt-response receipt))))))))

(serapeum/types:-> agenda-service--query (agenda-service parent-proto:record-request)
                   parent-proto:record-response)
(defun agenda-service--query (service request)
  "Return a byte-bounded page from a bounded scope-bound query snapshot."
  (let* ((query     (parent-proto:record-request.query request))
         (cursor    (parent-proto:record-request.cursor request))
         (cursor-p  (parent-proto:record-request.has-cursor request))
         (limit     (parent-proto:record-request.limit request))
         (table     (agenda-service-cursors service))
         (saved     (and cursor-p (gethash cursor table)))
         (context   (agenda-service-context service))
         (state     (unless saved (agenda--read context)))
         (directory (agenda-context-directory context))
         (record    (and state (agenda-find state directory)))
         (snapshot  (unless saved (list directory (mapcar #'agenda--item->form
                                                        (and record (workspace-agenda-items record))))))
         (text      (unless saved (record-collection--text snapshot)))
         (revision  (if saved (third saved) (record-collection--revision text)))
         (issued    (agenda-service-collection-snapshots service)))
    (when (and cursor-p (or (not saved) (not (string= query (first saved)))))
      (agenda-service--reject ':invalid-request))
    (unless saved (record-collection--check-capacity issued :revision revision :text text :reject
                   #'agenda-service--reject))
    (let* ((records
            (if saved
                (second saved)
                (let* ((context   (agenda-service-context service))
                       (state     state)
                       (directory (agenda-context-directory context))
                       (record    (agenda-find state directory))
                       (matches
                        (remove-if-not
                         (lambda (item)
                           (or (zerop (length query))
                               (search query (agenda-item-text item) :test
                                       #'char-equal)))
                         (and record (workspace-agenda-items record)))))
                  (when (> (length matches) 4096)
                    (agenda-service--reject ':resource-limit))
                  (mapcar
                   (lambda (item)
                     (agenda-service--stored-record item
                      (agenda-service--item-revision state directory item)))
                   matches))))
           (page (subseq records 0 (min limit (length records))))
           (next (agenda--make-identifier)))
      (loop
       (let* ((remaining (nthcdr (length page) records))
              (response
               (if remaining
                   (parent-proto:make-record-response :records page :next-cursor next :collection-revision revision)
                   (parent-proto:make-record-response :records page :collection-revision revision))))
         (when (agenda-service--response-fits-p service response)
           (when remaining
             (when
                 (or (>= (hash-table-count table) 256)
                     (>
                      (+ (length remaining)
                         (loop for value being the hash-values of table
                               sum (length (second value))))
                      4096))
               (agenda-service--reject ':resource-limit))
             (setf (gethash next table) (list (copy-seq query) remaining revision)))
           (unless saved (setf (gethash revision issued) text))
           (return response))
         (when (<= (length page) 1) (agenda-service--reject ':resource-limit))
         (setf page (butlast page)))))))

(serapeum/types:-> agenda-service-handle (agenda-service parent-proto:record-request)
                   parent-proto:record-response)
(defun agenda-service-handle (service request)
  "Serve an authorized records.v1 agenda request. The caller checks feature selection."
  (bordeaux-threads:with-lock-held ((agenda-service-lock service))
    (handler-case
     (let ((fields
            (agenda-service--validate-request request
             (agenda-service--fields request))))
       (agenda-service--bounded-response service
        (if (eq (parent-proto:record-request.operation request) ':query)
            (agenda-service--query service request)
            (agenda-service--write service request fields))))
     (agenda-error (condition)
      (agenda-service--reject
       (if (eq (agenda-error-operation condition) ':validate-item)
           ':invalid-request
           ':internal)))
     (protocol-value-error nil (agenda-service--reject ':internal)))))

(serapeum/types:-> agenda-service--response-fits-p
                   (agenda-service parent-proto:record-response) boolean)
(defun agenda-service--response-fits-p (service response)
  "Check the encoded portable response against the trusted byte budget."
  (<= (length (cl-protobufs:serialize-to-bytes response))
      (agenda-service-max-response-bytes service)))

(serapeum/types:-> agenda-service--bounded-response
                   (agenda-service parent-proto:record-response)
                   parent-proto:record-response)
(defun agenda-service--bounded-response (service response)
  "Return RESPONSE only when it fits the trusted transport budget."
  (unless (agenda-service--response-fits-p service response)
    (agenda-service--reject ':resource-limit))
  response)
