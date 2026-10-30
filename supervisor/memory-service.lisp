(in-package #:autolith-supervisor)

(defclass memory-service nil
          ((collection-snapshots :initform (make-hash-table :test #'equal)
              :reader                      memory-service-collection-snapshots
              :documentation               "Bounded parent-issued exact scope observations.")
           (context :initarg :context :reader memory-service-context :documentation
            "The trusted private store and workspace policy.")
           (session-id :initarg :session-id :reader memory-service-session-id :documentation
            "The trusted session identity for durable operations.")
           (scope :initarg :scope :reader memory-service-scope :documentation
            "The parent-selected global or workspace scope.")
           (allow-global-p :initarg :allow-global-p :reader memory-service-allow-global-p
            :documentation "Whether parent policy permits global memory reads and writes.")
           (input-history :initarg :input-history :reader memory-service-input-history
            :documentation "Return parent-owned user input strings for evidence checks.")
           (snapshots :initform (make-hash-table :test #'equal) :reader memory-service-snapshots
            :documentation "Issued record revisions and their structural snapshots.")
           (cursors :initform (make-hash-table :test #'equal) :reader memory-service-cursors
            :documentation "Bounded query snapshots, private to this service and scope.")
           (maximum-bytes :initarg :maximum-bytes :reader memory-service-maximum-bytes
            :documentation "The maximum encoded response size.")
           (lock :initform (bordeaux-threads:make-lock "memory record service") :reader
            memory-service-lock :documentation "Serialize cursor and snapshot updates."))
          (:documentation "The memory collection of records.v1 without an agent dependency."))

(defun make-memory-service
       (&key context session-id (scope ':workspace) allow-global-p
             (input-history (constantly nil)) (maximum-bytes (* 1024 1024)))
  "Create a service from trusted parent policy, never from agent fields."
  (unless
      (and (typep context 'memory-store-context) (non-empty-string-p session-id)
           (<= (length session-id) 1024) (member scope '(:workspace :global))
           (or (eq scope ':workspace) allow-global-p) (functionp input-history)
           (typep maximum-bytes '(integer 128)))
    (memory-record--reject ':invalid-request))
  (make-instance 'memory-service :context context :session-id (copy-seq session-id) :scope scope
                 :maximum-bytes maximum-bytes :allow-global-p (not (null allow-global-p))
                 :input-history input-history))

(defun memory-service--source-request (value)
  "Validate a source request without accepting claimed observations."
  (unless (hash-table-p value) (memory-record--reject ':invalid-request))
  (let* ((kinds (remove-if-not (lambda (name) (nth-value 1 (gethash name value)))
                              '("file" "url" "user")))
         (kind  (first kinds))
         (quote (gethash "quote" value)))
    (unless (and (= (length kinds) 1)
                 (loop for key being the hash-keys of value
                       always (member key (if (equal kind "user") '("user")
                                              (list kind "quote")) :test #'equal))
                 (or (null quote) (memory-source-quote-p quote)))
      (memory-record--reject ':invalid-request))
    (let ((target (gethash kind value)))
      (unless (non-empty-string-p target) (memory-record--reject ':invalid-request))
      (cond
        ((equal kind "file")
         (unless (<= (length target) *memory-source-path-limit*)
           (memory-record--reject ':invalid-request))
         (list ':file :path target :quote quote))
        ((equal kind "url")
         (unless (and (<= (length target) *memory-source-url-limit*)
                      (or (uiop:string-prefix-p "https://" target)
                          (uiop:string-prefix-p "http://" target)))
           (memory-record--reject ':invalid-request))
         (list ':url :url target :quote quote))
        (t
         (unless (memory-source-quote-p target) (memory-record--reject ':invalid-request))
         (list ':user :quote target))))))

(defun memory-service--observe-source (service request)
  "Check a source against parent-owned evidence and record its observation time."
  (let ((context (memory-service-context service))
        (now     (get-universal-time)))
    (ecase (first request)
      (:file
       (let* ((pathname               (merge-pathnames (getf (rest request) :path)
                                                      (memory-context-workspace context)))
              (resolved               (memory-context--check-source context pathname))
              (*memory-store-context* context))
         (memory-source-observe-file resolved :quote (getf (rest request) :quote) :time now)))
      (:url
       (list ':url :url (getf (rest request) :url) :quote (getf (rest request) :quote)
             :observed-at now))
      (:user
       (let ((quote (getf (rest request) :quote)))
         (unless (some (lambda (input) (memory-source-quote-found-p quote input))
                       (funcall (memory-service-input-history service)))
           (memory-record--reject ':invalid-request))
         (list ':user :quote quote :conversation (memory-service-session-id service)
               :observed-at now))))))

(defun memory-service--sources-data (sources)
  "Encode complete source observations with fixed string keys."
  (map 'vector
       (lambda (source)
         (let ((fields (make-hash-table :test #'equal)))
           (setf (gethash "kind" fields) (string-downcase (symbol-name (first source))))
          (loop for (key value) on (rest source) by (lambda (tail) (rest (rest tail)))
                 do (setf (gethash (string-downcase (symbol-name key)) fields) value))
          fields))
       sources))

(defun memory-service--premises-data (premises)
  "Encode observed premise digests without evaluating stored data."
  (map 'vector
       (lambda (premise)
         (let ((fields (make-hash-table :test #'equal)))
          (loop for (key value) on premise by (lambda (tail) (rest (rest tail)))
                 do (setf (gethash (string-downcase (symbol-name key)) fields)
                          (if (keywordp value) (string-downcase (symbol-name value)) value)))
          fields))
       premises))

(defun memory-service--fields (request)
  "Decode bounded writable data and reject unknown fields or field types."
  (let* ((fields
          (handler-case
           (if (parent-proto:record-request.has-fields request)
               (protocol-value->data
                (common-proto:make-value :map-value (parent-proto:record-request.fields request))
                :max-depth 4 :max-entries 256)
               (make-hash-table :test #'equal))
           (protocol-value-error nil (memory-record--reject ':invalid-request))))
         (tags (gethash "tags" fields #())))
    (unless
        (and
         (every (lambda (key) (member key '("title" "content" "tags" "scope" "sources" "premises")
                                    :test #'string=))
                (loop for key being the hash-keys of fields
                      collect key))
         (non-empty-string-p (gethash "title" fields))
         (<= (length (gethash "title" fields)) *memory-title-limit*)
         (non-empty-string-p (gethash "content" fields))
         (<= (length (gethash "content" fields)) *memory-content-limit*) (vectorp tags)
         (not (stringp tags)) (<= (length tags) *memory-tag-count-limit*)
         (every (lambda (tag) (and (non-empty-string-p tag) (<= (length tag) *memory-tag-limit*)))
                tags))
      (memory-record--reject ':invalid-request))
    (let ((normalized
            (list :title   (copy-seq (gethash "title" fields))
                  :content (copy-seq (gethash "content" fields))
                  :tags    (remove-duplicates (map 'list #'copy-seq tags) :test #'string-equal))))
      (when (nth-value 1 (gethash "scope" fields))
        (let ((scope (gethash "scope" fields)))
          (unless (member scope '("workspace" "global") :test #'equal)
            (memory-record--reject ':invalid-request))
          (setf normalized (append normalized (list :scope (if (equal scope "global") ':global ':workspace))))))
      (when (nth-value 1 (gethash "sources" fields))
        (let ((sources (gethash "sources" fields)))
          (unless (and (simple-vector-p sources) (<= (length sources) *memory-source-count-limit*))
            (memory-record--reject ':invalid-request))
          (setf normalized (append normalized (list :sources (map 'list #'memory-service--source-request sources))))))
      (when (nth-value 1 (gethash "premises" fields))
        (let ((premises (gethash "premises" fields)))
          (unless (and (simple-vector-p premises) (<= (length premises) *memory-premise-count-limit*)
                       (every (lambda (id) (and (non-empty-string-p id) (<= (length id) 1024))) premises))
            (memory-record--reject ':invalid-request))
          (setf normalized (append normalized (list :premises (coerce premises 'list))))))
      normalized)))

(defun memory-service--validate (request)
  "Validate the exact request shape before reading a private store."
  (record-collection--validate request #'memory-record--reject)
  (unless (eq (parent-proto:record-request.collection request) ':memory)
    (memory-record--reject ':unsupported))
  (let ((operation  (parent-proto:record-request.operation request))
        (id-p       (parent-proto:record-request.has-record-id request))
        (revision-p (parent-proto:record-request.has-expected-revision request))
        (query-p    (parent-proto:record-request.has-query request))
        (cursor-p   (parent-proto:record-request.has-cursor request))
        (fields-p   (parent-proto:record-request.has-fields request))
        (id         (parent-proto:record-request.operation-id request))
        (limit      (parent-proto:record-request.limit request)))
    (case operation
      (:query
       (unless
           (and (zerop (length id)) (not id-p) (not revision-p) (not fields-p) (<= 1 limit 256)
                (<= (length (parent-proto:record-request.query request)) 1024)
                (or (not cursor-p)
                    (non-empty-string-p (parent-proto:record-request.cursor request))))
         (memory-record--reject ':invalid-request))
       nil)
      ((:create :replace :delete)
       (unless
           (and (non-empty-string-p id) (<= (length id) 1024) (not query-p) (not cursor-p)
                (zerop limit)
                (if (eq operation ':create)
                    (and (not id-p) (not revision-p))
                    (and id-p revision-p
                         (non-empty-string-p (parent-proto:record-request.record-id request))
                         (non-empty-string-p
                          (parent-proto:record-request.expected-revision request))))
                (if (eq operation ':delete)
                    (not fields-p)
                    fields-p))
         (memory-record--reject ':invalid-request))
       (unless (eq operation ':delete) (memory-service--fields request)))
      (otherwise
       (memory-record--reject ':unsupported)))))

(defun memory-service--stored-record (service memory staged)
  "Encode MEMORY and stage its snapshot without changing issued authority."
  (let* ((snapshot (memory-record-snapshot memory))
         (revision (memory-record-revision snapshot))
         (fields   (make-hash-table :test #'equal))
         (key      (list (memory-identifier memory) revision))
         (issued   (memory-service-snapshots service)))
    (unless (or (gethash key issued) (gethash key staged))
      (when (>= (+ (hash-table-count issued) (hash-table-count staged)) 4096)
        (memory-record--reject ':resource-limit))
      (setf (gethash key staged) snapshot))
    (setf (gethash "metadata" fields)
          (memory-service-record-metadata (memory-service-context service) memory)
          (gethash "title" fields) (memory-title memory)
          (gethash "content" fields) (memory-content memory)
          (gethash "tags" fields) (coerce (memory-tags memory) 'vector)
          (gethash "scope" fields) (string-downcase (symbol-name (memory-scope memory)))
          (gethash "sources" fields) (memory-service--sources-data (memory-sources memory))
          (gethash "premises" fields) (memory-service--premises-data (memory-premises memory)))
    (parent-proto:make-stored-record
     :record-id (memory-identifier memory) :revision revision
     :fields (common-proto:value.map-value (protocol-data->value fields)))))

(defun memory-service--publish-snapshots (service staged)
  "Publish snapshots only after the response and store operation succeed."
  (maphash (lambda (key snapshot)
             (setf (gethash key (memory-service-snapshots service)) snapshot))
           staged))

(defun memory-service--check-response (service response)
  "Reject RESPONSE when its exact encoded size exceeds the service limit."
  (unless (<= (length (cl-protobufs:serialize-to-bytes response))
              (memory-service-maximum-bytes service))
    (memory-record--reject ':resource-limit))
  response)

(defun memory-service--query (service request)
  "Return one bounded page and its exact full-scope collection revision."
  (let* ((query    (parent-proto:record-request.query request))
         (cursor-p (parent-proto:record-request.has-cursor request))
         (cursor   (parent-proto:record-request.cursor request))
         (table    (memory-service-cursors service))
         (saved    (and cursor-p (gethash cursor table)))
         (limit    (parent-proto:record-request.limit request)))
    (when (and cursor-p (or (not saved) (not (equal query (first saved)))))
      (memory-record--reject ':invalid-request))
    (let* ((staged    (make-hash-table :test #'equal))
           (reading   (unless saved
                        (multiple-value-list
                         (memory-records-read (memory-service-context service)
                                              :visibility (memory-service-scope service) :query query
                                              :project (lambda (memory)
                                                         (memory-service--stored-record service memory staged))))))
           (text      (unless saved (record-collection--text (second reading))))
           (revision  (if saved (third saved) (record-collection--revision text)))
           (issued    (memory-service-collection-snapshots service))
           (records   (if saved (second saved) (first reading)))
           (next      (memory--make-identifier))
           (page      nil)
           (remaining records))
      (unless saved (record-collection--check-capacity issued :revision revision :text text :reject
                     #'memory-record--reject))
      (loop while (and remaining (< (length page) limit))
            for candidate = (append page (list (first remaining)))
            for response = (apply #'parent-proto:make-record-response
                                  :records candidate :collection-revision revision
                                  (when (rest remaining) (list :next-cursor next)))
            while (<= (length (cl-protobufs:serialize-to-bytes response)) (memory-service-maximum-bytes service))
            do (setf page candidate remaining (rest remaining)))
      (when (and remaining (null page)) (memory-record--reject ':resource-limit))
      (when (and remaining (>= (- (hash-table-count table) (if saved 1 0)) 256))
        (memory-record--reject ':resource-limit))
      (let ((response
              (memory-service--check-response
               service (apply #'parent-proto:make-record-response
                              :records page :collection-revision revision
                              (when remaining (list :next-cursor next))))))
        (memory-service--publish-snapshots service staged)
        (unless saved (setf (gethash revision issued) text))
        (when saved (remhash cursor table))
        (when remaining (setf (gethash next table) (list (copy-seq query) remaining revision)))
        response))))

(defun memory-service-handle (service request)
  "Serve an authorized records.v1 memory request. The caller checks feature selection."
  (bordeaux-threads:with-lock-held ((memory-service-lock service))
    (let ((fields (memory-service--validate request)))
      (when (and (eq (getf fields :scope) ':global) (not (memory-service-allow-global-p service)))
        (memory-record--reject ':invalid-request))
      (handler-case
       (if (eq (parent-proto:record-request.operation request) ':query)
           (memory-service--query service request)
           (let* ((id
                   (and (parent-proto:record-request.has-record-id request)
                        (parent-proto:record-request.record-id request)))
                  (revision
                   (and (parent-proto:record-request.has-expected-revision request)
                        (parent-proto:record-request.expected-revision request)))
                  (snapshot
                   (and id revision
                        (gethash (list id revision) (memory-service-snapshots service))))
                  (staged   (make-hash-table :test #'equal))
                  (response nil))
              (memory-records-mutate (memory-service-context service) :session-id
                                          (memory-service-session-id service) :operation-id
                                          (parent-proto:record-request.operation-id request)
                                          :operation
                                          (parent-proto:record-request.operation request)
                                          :record-id id :expected-revision revision
                                          :expected-snapshot snapshot
                                          :collection-revision
                                          (and (parent-proto:record-request.has-expected-collection-revision request)
                                               (parent-proto:record-request.expected-collection-revision request))
                                          :check-collection
                                          (lambda ()
                                            (record-collection--guard request :issued (memory-service-collection-snapshots service)
                                             :snapshot
                                             (mapcar #'memory-record-snapshot
                                                     (memory-list (memory-service-context service) :visibility
                                                      (memory-service-scope service)))
                                             :reject                                  #'memory-record--reject))
                                          :fields fields :scope
                                          (memory-service-scope service)
                                          :visibility        (if (memory-service-allow-global-p service) ':all
                                                                 (memory-service-scope service))
                                          :prepare-fields
                                          (lambda (fields)
                                            (let ((prepared (copy-list fields)))
                                              (when (member :sources prepared)
                                                (setf (getf prepared :sources)
                                                      (mapcar (lambda (source)
                                                                (memory-service--observe-source service source))
                                                              (getf prepared :sources))))
                                              prepared))
                                          :validate-result
                                          (lambda (result)
                                            (setf response
                                                  (memory-service--check-response
                                                   service
                                                   (parent-proto:make-record-response
                                                    :records
                                                    (mapcar (lambda (memory)
                                                              (memory-service--stored-record
                                                               service memory staged))
                                                            result))))))
              (memory-service--publish-snapshots service staged)
              response))
       (memory-policy-error () (memory-record--reject ':invalid-request))
       (memory-error nil (memory-record--reject ':invalid-request))
       (knowledge-error nil (memory-record--reject ':invalid-request))))))

(declaim (ftype (function (memory-store-context memory) hash-table) memory-service-record-metadata))
