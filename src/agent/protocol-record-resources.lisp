(in-package #:autolith)

;;;; -- Child Record Service Scope --

(defclass agent-record-resource-scope ()
  ((agenda :initarg :agenda :reader agent-record-resource-scope-agenda
           :documentation "The connection's agenda adapter.")
   (memory :initarg :memory :reader agent-record-resource-scope-memory
           :documentation "The connection's memory adapter.")
   (papercut :initarg :papercut :reader agent-record-resource-scope-papercut
             :documentation "The connection's papercut adapter.")
   (identity :initform (make-identifier) :reader agent-record-resource-scope-identity
             :documentation "A fresh scope identity, independent of private paths.")
   (knowledge :initarg :knowledge :initform nil :reader agent-record-resource-scope-knowledge
              :documentation "The optional negotiated parent knowledge adapter.")
   (session :initarg :session :reader agent-record-resource-scope-session
            :documentation "The attached session, retained only as child-local context."))
  (:documentation "Parent record adapters selected for one child configuration."))

(defvar *agent-record-resource-scopes* (make-hash-table :test #'eq)
  "Child configurations with installed parent services. No private paths are stored.")

(defvar *agent-record-resource-scopes-lock* (make-recursive-lock "record resource scopes")
  "Protect installation and removal of child service scopes.")

(-> agent-record-resources-install (t t &key (:session t)) agent-record-resource-scope)
(defun agent-record-resources-install (driver configuration &key session)
  "Select DRIVER for normal private resource tools.
Active or passive knowledge requires knowledge.v1. No local store fallback is allowed."
  (unless (or (equal "off" (config :knowledge-mode configuration))
              (agent-protocol-knowledge-available-p driver))
    (error 'agent-record-resource-contract-unsupported
           :identifier "memory:contracts"
           :message    "Parent-backed knowledge requires negotiated knowledge.v1."))
  (let ((scope (make-instance 'agent-record-resource-scope
                             :agenda    (make-agent-protocol-agenda driver)
                             :memory    (make-agent-protocol-memory driver)
                             :papercut  (make-agent-protocol-papercut driver)
                             :knowledge (when (agent-protocol-knowledge-available-p driver)
                                          (make-agent-protocol-knowledge driver))
                             :session   session)))
    (with-recursive-lock-held (*agent-record-resource-scopes-lock*)
      (setf (gethash configuration *agent-record-resource-scopes*) scope))
    scope))

(-> agent-record-resources-uninstall (t) null)
(defun agent-record-resources-uninstall (configuration)
  "Release CONFIGURATION's connection-local service scope at child shutdown."
  (with-recursive-lock-held (*agent-record-resource-scopes-lock*)
    (remhash configuration *agent-record-resource-scopes*))
  nil)

(-> agent-record-resource--scope (tool-context) (option agent-record-resource-scope))
(defun agent-record-resource--scope (context)
  "Return CONTEXT's installed services without consulting local stores."
  (with-recursive-lock-held (*agent-record-resource-scopes-lock*)
    (gethash (tool-context-configuration context) *agent-record-resource-scopes*)))

(define-condition agent-record-resource-contract-unsupported (tool-error)
  ((identifier :initarg :identifier :reader agent-record-resource-contract-unsupported-identifier
               :documentation "The resource not represented by records.v1."))
  (:default-initargs :tool-name "resource.read" :code ':operation-unsupported
                    :message "records.v1 has no structured knowledge contract; the parent must supply facts, rules and evidence assessment through a negotiated service.")
  (:documentation "A private resource needs a service contract absent from records.v1."))

(defclass agent-record-resource (resource)
  ((scope :initarg :scope :reader agent-record-resource-scope
          :documentation "The installed child-local service scope.")
   (collection :initarg :collection :reader agent-record-resource-collection
               :documentation "The typed parent record collection.")
   (identifier :initarg :identifier :reader agent-record-resource-identifier
               :documentation "The collection name or exact parent record ID.")
   (exact-p :initarg :exact-p :reader agent-record-resource-exact-p
            :documentation "Whether this resource denotes one exact item."))
  (:documentation "An existing resource protocol object backed only by parent records."))

(defclass agent-record-resource-observation (resource-observation)
  ((identity :initarg :identity :reader agent-record-resource-observation-identity
             :documentation "The scope and resource identity used for revision guards.")
   (records :initarg :records :reader agent-record-resource-observation-records
            :documentation "All records in the guarded collection, including filtered-out rows.")
   (collection-revision :initarg :collection-revision :initform nil
                        :reader        agent-record-resource-observation-collection-revision
                        :documentation "The parent-issued atomic collection guard.")
   (snapshot :initarg :snapshot :reader agent-record-resource-observation-snapshot
             :documentation "The detached identity and revision pairs observed at read time."))
  (:documentation "A complete parent-backed resource observation retained by one conversation."))

(defclass agent-record-resource-observation-state (resource-observation-state)
  ((attempts :initform      (make-hash-table :test #'equal)
             :reader        agent-record-resource-observation-state-attempts
             :documentation "Exact operation payloads and stable IDs for explicit retries."))
  (:documentation "A conversation-local parent record observation and its write identities."))

(defmethod resource-observation-state-class ((resource agent-record-resource))
  "Use the parent-backed conversation observation family."
  (declare (ignore resource))
  'agent-record-resource-observation-state)

(defmethod resource-item-identity ((resource agent-record-resource))
  "Bind observations to the installed connection and canonical URI."
  (format nil "~A/~A" (agent-record-resource-scope-identity
                       (agent-record-resource-scope resource)) (resource-uri resource)))

(defmethod resource-observation-identity ((observation agent-record-resource-observation))
  "Return the connection-specific observation identity."
  (agent-record-resource-observation-identity observation))

(defmethod resource-observation-state-family-and-key
    ((observation agent-record-resource-observation))
  "Deduplicate exact snapshots only within the same installed scope."
  (values 'agent-record-resource-observation-state
          (list (resource-observation-identity observation)
                (agent-record-resource-observation-collection-revision observation)
                (agent-record-resource-observation-snapshot observation)
                (resource-observation-content observation))))

(defmethod resource-observation-state-maximum ((state agent-record-resource-observation-state))
  "Bound parent record observations retained by one conversation."
  (declare (ignore state))
  48)

(defmethod resource-capabilities ((resource agent-record-resource) (context tool-context))
  "Expose record operations only through the same installed scope."
  (when (eq (agent-record-resource-scope resource) (agent-record-resource--scope context))
    (if (and (eq (agent-record-resource-collection resource) ':memory)
             (not (agent-record-resource-exact-p resource))
             (member (agent-record-resource-identifier resource) '("relevant" "all") :test #'equal))
        '(:read)
        '(:read :edit))))

(-> agent-record-resource--resolve
    (agent-record-resource-scope keyword string) agent-record-resource)
(defun agent-record-resource--resolve (scope collection identifier)
  "Resolve public record identities without asking for a private pathname."
  (let* ((scheme  (string-downcase (symbol-name collection)))
         (collection-p
           (member identifier (ecase collection
                                (:agenda
                                 '("current"))
                                (:papercut
                                 '("current"))
                                (:memory
                                 '("relevant" "workspace" "global" "all")))
                   :test #'equal))
         (exact-p (not collection-p))
         (id      (if (uiop:string-prefix-p "id/" identifier)
                      (resource-item-decode-identifier scheme scheme (subseq identifier 3))
                      identifier)))
    (when (and (eq collection ':memory) (equal identifier "global"))
      (error 'agent-record-resource-contract-unsupported
             :identifier "memory:global"
             :message    "records.v1 does not select a global memory service scope on this connection."))
    (when (and (eq collection ':memory)
               (or (memory-knowledge-resolve identifier)
                   (string= identifier "recipes")
                   (uiop:string-prefix-p "claim/" identifier)))
      (error 'agent-record-resource-contract-unsupported :identifier identifier))
    (when (and (eq collection ':agenda) exact-p)
      (error 'agenda-resource-identifier-unsupported :identifier identifier))
    (when (and (eq collection ':papercut) exact-p
               (not (uiop:string-prefix-p "id/" identifier)))
      (error 'papercut-resource-identifier-unsupported :identifier identifier))
    (make-instance 'agent-record-resource :scope scope :collection collection
                   :identifier id :exact-p exact-p
                   :uri (if exact-p (resource-item-uri scheme id)
                            (format nil "~A:~A" scheme identifier)))))

(defmethod resource-resolver-resolve :around
    ((resolver agenda-resolver) identifier (context tool-context))
  "Select the installed parent service before any local agenda resolution."
  (let ((scope (agent-record-resource--scope context)))
    (if scope (agent-record-resource--resolve scope ':agenda identifier) (call-next-method))))

(defgeneric agent-knowledge-resource-resolve (scope identifier)
  (:documentation "Resolve a public knowledge view through the negotiated service."))

(defmethod resource-resolver-resolve :around
    ((resolver memory-resolver) identifier (context tool-context))
  "Select the installed parent service before any local memory resolution."
  (let ((scope (agent-record-resource--scope context)))
    (cond
      ((and scope (or (memory-knowledge-resolve identifier)
                      (string= identifier "recipes")
                      (uiop:string-prefix-p "claim/" identifier)))
       (if (string= identifier "recipes")
           (error 'agent-record-resource-contract-unsupported :identifier identifier)
           (agent-knowledge-resource-resolve scope identifier)))
      (scope
       (agent-record-resource--resolve scope ':memory identifier))
      (t
       (call-next-method)))))

(defmethod resource-resolver-resolve :around
    ((resolver papercut-resolver) identifier (context tool-context))
  "Select the installed parent service before any local papercut resolution."
  (let ((scope (agent-record-resource--scope context)))
    (if scope (agent-record-resource--resolve scope ':papercut identifier) (call-next-method))))


;;;; -- Parent Views --

(defgeneric agent-record-resource-record-identity (record)
  (:documentation "Return a detached record's parent ID and exact revision."))

(defmethod agent-record-resource-record-identity ((record agent-protocol-agenda-item))
  "Return the agenda identity and revision."
  (values (agenda-item-identifier record) (agent-protocol-agenda-item-revision record)))

(defmethod agent-record-resource-record-identity ((record agent-protocol-memory-item))
  "Return the memory identity and revision."
  (values (agent-protocol-memory-item-identifier record) (agent-protocol-memory-item-revision record)))

(defmethod agent-record-resource-record-identity ((record agent-protocol-papercut-record))
  "Return the papercut identity and revision."
  (values (agent-protocol-papercut-record-identifier record) (agent-protocol-papercut-record-revision record)))

(defgeneric agent-record-resource-render-record (record)
  (:documentation "Render the complete public record without reading a local store."))

(defmethod agent-record-resource-render-record ((record agent-protocol-agenda-item))
  "Use the shared agenda renderer for a typed parent item."
  (agenda-resource--render-item record))

(defmethod agent-record-resource-render-record ((record agent-protocol-memory-item))
  "Render parent-assessed evidence and all public metadata with the complete body."
  (let* ((metadata   (gethash "metadata" (agent-protocol-memory-item-fields record)))
         (assessment (and metadata (gethash "assessment" metadata))))
    (format nil "id: ~A~%scope: ~(~A~)~%title: ~A~%tags: ~{~A~^, ~}~%status: ~A~%sources: ~A~%premises: ~A~@[~%metadata: ~A~]~%~%~A"
            (agent-protocol-memory-item-identifier record) (agent-protocol-memory-item-scope record)
            (agent-protocol-memory-item-title record) (agent-protocol-memory-item-tags record)
            (if assessment (gethash "status" assessment) "unverified (parent assessment unavailable)")
            (json-encode (coerce (agent-protocol-memory-item-sources record) 'vector))
            (json-encode (coerce (agent-protocol-memory-item-premises record) 'vector))
            (and metadata (json-encode metadata))
            (agent-protocol-memory-item-content record))))

(defmethod agent-record-resource-render-record ((record agent-protocol-papercut-record))
  "Render all public report fields, including assessment and closure data."
  (format nil "id: ~A~%~A" (agent-protocol-papercut-record-identifier record)
          (json-encode (agent-protocol-papercut-record-fields record))))

(-> agent-record-resource--records
    (agent-record-resource &key (:query string)) (values list non-empty-string))
(defun agent-record-resource--records (resource &key (query ""))
  "Read cursor pages and their atomic structural scope revision through the driver."
  (let* ((scope          (agent-record-resource-scope resource))
         (collection     (agent-record-resource-collection resource))
         (adapter        (ecase collection
                           (:agenda
                            (agent-record-resource-scope-agenda scope))
                           (:memory
                            (agent-record-resource-scope-memory scope))
                           (:papercut
                            (agent-record-resource-scope-papercut scope))))
         (query-function (ecase collection
                           (:agenda
                            #'agent-protocol-agenda-query)
                           (:memory
                            #'agent-protocol-memory-query)
                           (:papercut
                            #'agent-protocol-papercut-query)))
         (seen           (make-hash-table :test #'equal))
         (records        nil)
         (revision       nil)
         (cursor         nil))
    (loop
      (multiple-value-bind (page next-cursor collection-revision)
          (funcall query-function adapter :query query :limit 256 :cursor cursor)
        (unless (and (non-empty-string-p collection-revision)
                     (or (null revision) (equal revision collection-revision)))
          (error 'tool-error :tool-name "resource.read" :code ':invalid-response
                 :message "Parent record pages lack one stable collection revision."))
        (setf revision collection-revision)
        (dolist (record page) (push record records))
        (unless next-cursor (return))
        (when (gethash next-cursor seen)
          (error 'tool-error :tool-name "resource.read" :code ':invalid-response
                 :message "Parent record pagination repeated a cursor."))
        (setf (gethash next-cursor seen) t cursor next-cursor)))
    (values (nreverse records) revision)))

(-> agent-record-resource--visible-records (agent-record-resource list) list)
(defun agent-record-resource--visible-records (resource records)
  "Apply resource visibility to the already parent-authorized record set."
  (remove-if-not
   (lambda (record)
     (cond
       ((agent-record-resource-exact-p resource)
        (string= (agent-record-resource-identifier resource)
                 (agent-record-resource-record-identity record)))
       ((and (eq (agent-record-resource-collection resource) ':memory)
             (member (agent-record-resource-identifier resource) '("workspace" "global") :test #'equal))
        (string-equal (agent-record-resource-identifier resource)
                      (symbol-name (agent-protocol-memory-item-scope record))))
       ((eq (agent-record-resource-collection resource) ':papercut)
        (equal "active" (gethash "status" (agent-protocol-papercut-record-fields record))))
       (t
        t)))
   records))

(-> agent-record-resource--snapshot (list) list)
(defun agent-record-resource--snapshot (records)
  "Return a stable exact collection guard independent of page ordering."
  (sort (mapcar (lambda (record)
                  (multiple-value-bind (id revision) (agent-record-resource-record-identity record)
                    (list id revision))) records)
        #'string< :key #'first))

(-> agent-record-resource--observe
    (agent-record-resource &key (:query (option string)) (:maximum (option integer)))
    agent-record-resource-observation)
(defun agent-record-resource--observe (resource &key query maximum)
  "Keep an unfiltered atomic guard and use the parent's shared search semantics."
  (multiple-value-bind (all collection-revision) (agent-record-resource--records resource)
    (let* ((visible  (agent-record-resource--visible-records resource all))
           (guard    (if (agent-record-resource-exact-p resource) visible all))
           (snapshot (agent-record-resource--snapshot guard))
           (filtered
             (if query
                 (multiple-value-bind (matches match-revision)
                     (agent-record-resource--records resource :query query)
                   (unless (equal collection-revision match-revision)
                     (error 'tool-error :tool-name "resource.read" :code ':revision-stale
                            :message "The record collection changed during the read. Read it again."))
                   (agent-record-resource--visible-records resource matches))
                 visible)))
      (when (and (agent-record-resource-exact-p resource) (null visible))
        (error 'tool-error :tool-name "resource.read" :code ':not-found
               :message "The parent record does not exist in the attached scope."))
      (when maximum (setf filtered (subseq filtered 0 (min maximum (length filtered)))))
      (make-instance 'agent-record-resource-observation
                     :uri                 (resource-uri resource)
                     :revision            (resource-readable-snapshot-digest *memory-resource-digest-key* snapshot)
                     :identity (resource-item-identity resource) :records guard :snapshot snapshot
                     :collection-revision collection-revision
                     :content             (if filtered
                                              (format nil "~{~A~^~%~%~}" (mapcar #'agent-record-resource-render-record filtered))
                                              "No active records.")
                     :metadata            (list :query query :maximum-results maximum :count (length filtered))))))

(defmethod resource-observe ((resource agent-record-resource) (context tool-context))
  "Observe the attached record service without local store access."
  (declare (ignore context))
  (agent-record-resource--observe resource))

(defmethod resource-tool-read
    ((resource agent-record-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read through normal resource tools and retain a conversation-local revision."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error :tool-name "resource.read" :code ':line-windows-unsupported
           :message "Parent record resources are always read in full."))
  (let ((query   (tool-argument arguments "query"))
        (maximum (tool-argument arguments "max-results")))
    (when (and (or query maximum)
               (or (not (eq (agent-record-resource-collection resource) ':memory))
                   (agent-record-resource-exact-p resource)))
      (error 'tool-error :tool-name "resource.read"
             :message               "Only memory collections accept query and max-results."))
    (when (and query (not (non-empty-string-p query)))
      (error 'tool-error :tool-name "resource.read" :message "Memory query must be non-empty text."))
    (when (and maximum (not (and (integerp maximum) (plusp maximum))))
      (error 'tool-error :tool-name "resource.read" :message "Memory max-results must be a positive integer."))
    (let* ((observation (agent-record-resource--observe
                         resource :query query :maximum (and maximum (min maximum *memory-resource-maximum-results*))))
           (state       (resource-observation-state-ensure (tool-context-conversation context) observation)))
      (tool-success (resource-item-read-result state)))))


;;;; -- Guarded Record Edits --

(-> agent-record-resource--normalize (agent-record-resource t) list)
(defun agent-record-resource--normalize (resource operation)
  "Reuse the public operation validators and require the correct resource kind."
  (let* ((collection (agent-record-resource-collection resource))
         (normalized (ecase collection
                       (:agenda
                        (agenda-resource--normalize-operation operation))
                       (:memory
                        (memory-resource--normalize-operation operation))
                       (:papercut
                        (papercut-resource--normalize-operation operation))))
         (kind       (getf normalized :kind))
         (exact-p    (agent-record-resource-exact-p resource)))
    (when (and (eq collection ':memory) (eq (getf normalized :scope) ':global))
      (error 'agent-record-resource-contract-unsupported
             :identifier "memory:global" :tool-name "resource.edit"
             :message "records.v1 does not negotiate a global scope for this memory replacement."))
    (unless (ecase collection
              (:agenda
               t)
              (:memory
               (if exact-p (member kind '(:replace :forget))
                   (and (member (agent-record-resource-identifier resource) '("workspace" "global") :test #'equal)
                        (eq kind ':remember))))
              (:papercut
               (if exact-p (member kind '(:assess :close)) (eq kind ':report))))
      (error 'tool-error :tool-name "resource.edit" :message "The operation does not apply to this resource kind."))
    normalized))

(-> agent-record-resource--memory-evidence (hash-table) list)
(defun agent-record-resource--memory-evidence (operation)
  "Forward only supplied evidence fields. Let the parent check their authority."
  (let ((arguments nil))
    (dolist (entry '(("sources" . :sources) ("premises" . :premises)))
      (multiple-value-bind (value supplied-p) (gethash (first entry) operation)
        (when supplied-p
          (setf arguments (append arguments (list (rest entry) (coerce value 'list)))))))
    arguments))

(-> agent-record-resource--write
    (agent-record-resource agent-record-resource-observation list
     &key (:raw hash-table) (:operation-id string))
    (values t (option string)))
(defun agent-record-resource--write (resource observation operation &key raw operation-id)
  "Apply one normalized operation with the original exact parent revision.
Return the resulting record, or NIL for deletion, and its optional exact URI."
  (let* ((scope               (agent-record-resource-scope resource))
         (kind                (getf operation :kind))
         (records             (agent-record-resource-observation-records observation))
         (identifier          (or (getf operation :identifier) (agent-record-resource-identifier resource)))
         (item                (find identifier records :test #'string= :key #'agent-record-resource-record-identity))
         (collection          (agent-record-resource-collection resource))
         (collection-revision (agent-record-resource-observation-collection-revision observation))
         (result
           (ecase collection
             (:agenda
              (let ((adapter (agent-record-resource-scope-agenda scope)))
                (ecase kind
                  (:add
                   (agent-protocol-agenda-create adapter :text                (getf operation :text)
                                                :status                       (getf operation :status)
                                                :memory-identifiers           (getf operation :memory-identifiers)
                                                :operation-id                 operation-id
                                                :expected-collection-revision collection-revision))
                  ((:update :remove)
                   (unless item
                     (error 'tool-error :tool-name "resource.edit" :message "The agenda item was not in the observed collection."))
                   (if (eq kind ':remove)
                       (agent-protocol-agenda-delete adapter item :operation-id    operation-id
                                                     :expected-collection-revision collection-revision)
                       (agent-protocol-agenda-replace
                        adapter item :text  (or (getf operation :text) (agenda-item-text item))
                        :status             (or (getf operation :status) (agenda-item-status item))
                        :memory-identifiers (if (getf operation :memory-identifiers-supplied-p)
                                                (getf operation :memory-identifiers)
                                                (agenda-item-memory-identifiers item))
                        :operation-id operation-id :expected-collection-revision collection-revision))))))
             (:memory
              (let* ((adapter   (agent-record-resource-scope-memory scope))
                     (arguments (append (list :title (getf operation :title) :content (getf operation :content)
                                              :tags (getf operation :tags) :operation-id operation-id)
                                        (agent-record-resource--memory-evidence raw))))
                (ecase kind
                  (:remember
                   (apply #'agent-protocol-memory-create adapter
                          :expected-collection-revision collection-revision arguments))
                  (:replace
                   (apply #'agent-protocol-memory-replace adapter item
                          :scope (getf operation :scope) arguments))
                  (:forget
                   (agent-protocol-memory-delete adapter item :operation-id operation-id)))))
             (:papercut
              (let ((adapter (agent-record-resource-scope-papercut scope)))
                (ecase kind
                  (:report
                   (agent-protocol-papercut-create
                    adapter :title (getf operation :title) :content (getf operation :content)
                    :issue-kind (getf operation :issue-kind)
                    :tool (getf operation :tool) :diagnostic (getf operation :diagnostic)
                    :operation-id operation-id :expected-collection-revision collection-revision))
                  (:assess
                   (agent-protocol-papercut-assess adapter item :verdict (getf operation :verdict)
                                                 :note (getf operation :note) :diagnostic (getf operation :diagnostic)
                                                 :operation-id           operation-id))
                  (:close
                   (agent-protocol-papercut-close adapter item :resolution (getf operation :resolution)
                                                :diagnostic                (getf operation :diagnostic)
                                                :operation-id              operation-id))))))))
    (values result (when (and result (not (eq collection ':agenda)))
                     (resource-item-uri (string-downcase (symbol-name collection))
                                        (agent-record-resource-record-identity result))))))

(-> agent-record-resource--stale (agent-record-resource string) nil)
(defun agent-record-resource--stale (resource alias)
  "Signal a stale model observation without exposing parent private details."
  (error 'resource-revision-stale :uri (resource-uri resource)
         :expected-revision alias :actual-revision nil))

(-> agent-record-resource--operation-key (t) t)
(defun agent-record-resource--operation-key (value)
  "Return an order-independent key for one exact portable operation payload."
  (cond
    ((hash-table-p value)
     (cons ':object
           (mapcar (lambda (key)
                     (cons key (agent-record-resource--operation-key (gethash key value))))
                   (sort (loop for key being the hash-keys of value collect key) #'string<))))
    ((and (vectorp value) (not (stringp value)))
     (cons ':array (map 'list #'agent-record-resource--operation-key value)))
    (t
     value)))

(defmethod resource-apply-operations
    ((resource agent-record-resource) (context tool-context) &key base-revision operations)
  "Apply one parent write using a conversation guard and a stable retry identity."
  (unless (and (listp operations) (= 1 (length operations)))
    (error 'tool-error :tool-name "resource.edit" :message "Record edits require exactly one operation."))
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held ((conversation-resource-observation-lock conversation))
      (let* ((state        (resource-item-find-observation-state conversation resource base-revision))
             (observation  (resource-observation-state-observation state))
             (raw          (first operations))
             (normalized   (agent-record-resource--normalize resource raw))
             (key          (agent-record-resource--operation-key raw))
             (attempts     (agent-record-resource-observation-state-attempts state))
             (operation-id (gethash key attempts)))
        (unless operation-id
          (when (and (not (agent-record-resource-exact-p resource))
                     (not (non-empty-string-p
                           (agent-record-resource-observation-collection-revision observation))))
            (agent-record-resource--stale resource base-revision))
          (setf operation-id (make-identifier) (gethash key attempts) operation-id))
        (handler-case
            (multiple-value-bind (result exact-uri)
                (agent-record-resource--write resource observation normalized :raw raw :operation-id operation-id)
              (let ((after
                      (if (and (agent-record-resource-exact-p resource)
                               (or (null result) (eq (getf normalized :kind) ':close)))
                          (make-instance 'agent-record-resource-observation
                                         :uri (resource-uri resource) :identity (resource-item-identity resource)
                                         :revision operation-id :records nil :snapshot nil
                                         :content "The record is no longer active." :metadata nil)
                          (agent-record-resource--observe resource))))
                (values after "Updated parent record." exact-uri)))
          (agent-protocol-agenda-error (condition)
            (if (eq (agent-protocol-agenda-error-code condition) ':revision-conflict)
                (agent-record-resource--stale resource base-revision) (error condition)))
          (agent-protocol-memory-error (condition)
            (if (eq (agent-protocol-memory-error-code condition) ':revision-conflict)
                (agent-record-resource--stale resource base-revision) (error condition)))
          (agent-protocol-papercut-error (condition)
            (if (eq (agent-protocol-papercut-error-code condition) ':revision-conflict)
                (agent-record-resource--stale resource base-revision) (error condition))))))))

(defmethod resource-tool-edit
    ((resource agent-record-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Invoke guarded parent writes through the existing resource tool protocol."
  (declare (ignore tool))
  (let ((alias      (tool-argument arguments "base-revision" :required t))
        (operations (tool-argument arguments "operations" :required t)))
    (unless (and (non-empty-string-p alias) (vectorp operations) (= 1 (length operations)))
      (error 'tool-error :tool-name "resource.edit"
             :message               "Record edits require an observed base-revision and exactly one operation."))
    (handler-case
        (multiple-value-bind (observation summary exact-uri)
            (resource-apply-operations resource context :base-revision alias :operations (coerce operations 'list))
          (let ((state (resource-observation-state-ensure (tool-context-conversation context) observation)))
            (tool-success (format nil "~A~@[~%Exact URI: ~A~]~%~A" summary exact-uri (resource-item-read-result state)))))
      (resource-revision-stale ()
        (tool-failure (format nil "Resource revision ~A is stale or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                              alias (resource-uri resource)))))))
