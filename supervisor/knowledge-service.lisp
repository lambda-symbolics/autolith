(in-package #:autolith-supervisor)

;;;; -- Parent Structured Knowledge Service --

(define-condition knowledge-service-error (error)
  ((code :initarg :code :reader knowledge-service-error-code
         :documentation "The fixed public failure code."))
  (:documentation "A knowledge request failed without exposing private state.")
  (:report (lambda (condition stream)
             (format stream "Knowledge service failed: ~A" (knowledge-service-error-code condition)))))

(defclass knowledge-service ()
  ((context :initarg :context :reader knowledge-service-context
            :documentation "The trusted store and file evidence policy.")
   (session-id :initarg :session-id :reader knowledge-service-session-id
               :documentation "The parent-owned durable session identity.")
   (allow-global-p :initarg :allow-global-p :reader knowledge-service-allow-global-p
                   :documentation "Whether this service may use global knowledge.")
   (memory :initarg :memory :reader knowledge-service-memory
           :documentation "The shared evidence authority for public source requests.")
   (url-evidence :initarg :url-evidence :reader knowledge-service-url-evidence
                 :documentation "Trusted callback of URL and quote returning checked retrieval evidence.")
   (approve :initarg :approve :reader knowledge-service-approve
            :documentation "Parent-only callback of kind, scope, id, version and exact proposal.")
   (maximum-bytes :initarg :maximum-bytes :reader knowledge-service-maximum-bytes
                  :documentation "The maximum serialized response size.")
   (snapshots :initform (make-hash-table :test #'equal) :reader knowledge-service-snapshots
              :documentation "Bounded issued full-scope graph observations.")
   (handles :initform (make-hash-table :test #'equal) :reader knowledge-service-handles
            :documentation "Bounded parent-issued contract checks, never child-supplied contracts.")
   (context-cache :initform nil :accessor knowledge-service-context-cache
                  :documentation "The last turn's context changes, separate from contract handles.")
   (lock :initform (bordeaux-threads:make-lock "parent knowledge service")
         :reader knowledge-service-lock :documentation "Serialize observations and check handles."))
  (:documentation "The negotiated knowledge.v1 service over the shared knowledge engine."))

(serapeum/types:-> knowledge-service--reject (keyword) nil)
(defun knowledge-service--reject (code)
  "Signal a fixed public service error."
  (error 'knowledge-service-error :code code))

(serapeum/types:-> make-knowledge-service
    (&key (:context memory-store-context) (:session-id string) (:allow-global-p boolean)
          (:input-history function) (:url-evidence function) (:approve function)
          (:maximum-bytes (integer 128))) knowledge-service)
(defun make-knowledge-service (&key context session-id allow-global-p
                                   (input-history (constantly nil))
                                   (url-evidence (constantly nil))
                                   (approve (constantly nil))
                                   (maximum-bytes (* 1024 1024)))
  "Bind parent-owned stores, evidence and admission policy to one session.
The default scope is workspace. Only ALLOW-GLOBAL-P authorizes global requests.
APPROVE receives keyword kind, keyword scope, id, version and exact data."
  (unless (and (typep context 'memory-store-context)
               (session-store--identifier-p session-id) (<= (length session-id) 256)
               (functionp input-history)
               (functionp url-evidence) (functionp approve)
               (typep maximum-bytes '(integer 128 1048576)))
    (knowledge-service--reject ':invalid-request))
  (make-instance 'knowledge-service :context context :session-id (copy-seq session-id)
                 :allow-global-p (not (null allow-global-p))
                 :memory         (make-memory-service :context context :session-id session-id
                                                      :allow-global-p allow-global-p
                                                      :input-history  input-history)
                 :url-evidence url-evidence :approve approve :maximum-bytes maximum-bytes))

(serapeum/types:-> knowledge-service--scope (knowledge-service parent-proto:knowledge-request) keyword)
(defun knowledge-service--scope (service request)
  "Select only a scope authorized by parent configuration."
  (let ((scope (parent-proto:knowledge-request.scope request)))
    (case scope
      ((:scope-unspecified :workspace)
       ':workspace)
      (:global
       (unless (knowledge-service-allow-global-p service)
         (knowledge-service--reject ':permission-denied))
       ':global)
      (otherwise
       (knowledge-service--reject ':invalid-request)))))

(serapeum/types:-> knowledge-service-authorized-base (knowledge-service keyword knowledge-base) knowledge-base)
(defun knowledge-service-authorized-base (service scope base)
  "Project BASE to exact authorized SCOPE before evaluation or public operations.
Copy claims, supports and indexes together. Never expose another workspace or
use its evidence to derive a public fact. Workspace requests include globals
only when the parent explicitly enables global knowledge."
  (let* ((result    (make-instance 'knowledge-base))
         (workspace (knowledge--workspace (knowledge-service-context service)))
         (global-p  (and (eq scope ':workspace) (knowledge-service-allow-global-p service))))
    (labels ((allowed-p (owner directory)
               (or (and (eq owner ':workspace) (eq scope ':workspace) (equal directory workspace))
                   (and (eq owner ':global) (or (eq scope ':global) global-p))))

             (copy-keyed (source target)
               (maphash (lambda (key value)
                          (when (allowed-p (first key) (second key))
                            (setf (gethash key target) (copy-tree value)))) source)))
      (setf (knowledge-base-revision result) (knowledge-base-revision base))
      (copy-keyed (knowledge-base-schemas base) (knowledge-base-schemas result))
      (copy-keyed (knowledge-base-rules base) (knowledge-base-rules result))
      (copy-keyed (knowledge-base-methods-table base) (knowledge-base-methods-table result))
      (copy-keyed (knowledge-base-contracts-table base) (knowledge-base-contracts-table result))
      (copy-keyed (knowledge-base-diagnoses-table base) (knowledge-base-diagnoses-table result))
      (maphash
       (lambda (id claim)
         (when (allowed-p (getf claim :scope) (getf claim :workspace))
           (setf (gethash id (knowledge-base-claims result)) (copy-tree claim))
           (knowledge--index-add (knowledge-base-predicate-index result)
                                 (knowledge-proposition-predicate (getf claim :proposition)) id)
           (dolist (term (knowledge-proposition-arguments (getf claim :proposition)))
             (when (stringp term) (knowledge--index-add (knowledge-base-term-index result) term id)))
           (dolist (support-id (getf claim :supports))
             (let* ((support     (gethash support-id (knowledge-base-supports base)))
                    (evidence-id (getf support :evidence)))
               (setf (gethash support-id (knowledge-base-supports result)) (copy-tree support))
               (when evidence-id
                 (setf (gethash evidence-id (knowledge-base-evidence result))
                       (copy-tree (gethash evidence-id (knowledge-base-evidence base))))
                 (knowledge--index-add (knowledge-base-evidence-index result) evidence-id support-id))))))
       (knowledge-base-claims base))
      (when (eq scope ':workspace)
        (maphash (lambda (key value)
                   (when (equal (first key) workspace)
                     (setf (gethash key (knowledge-base-procedures-table result)) (copy-tree value))))
                 (knowledge-base-procedures-table base))))
    result))

(serapeum/types:-> knowledge-service--snapshot (keyword knowledge-base) string)
(defun knowledge-service--snapshot (scope base)
  "Return canonical full-scope graph data, excluding receipts and unrelated scopes."
  (let ((tables (list (knowledge-base-schemas base) (knowledge-base-claims base)
                      (knowledge-base-supports base) (knowledge-base-evidence base)
                      (knowledge-base-rules base) (knowledge-base-methods-table base)
                      (knowledge-base-contracts-table base) (knowledge-base-procedures-table base))))
    (record-collection--text
     (cons scope (mapcar (lambda (table)
                           (sort (loop for key being the hash-keys of table using (hash-value value)
                                       collect (list key value))
                                 #'string< :key #'record-collection--text)) tables)))))

(serapeum/types:-> knowledge-service--issue (knowledge-service keyword knowledge-base) string)
(defun knowledge-service--issue (service scope base)
  "Issue a bounded exact scope observation."
  (let* ((text     (knowledge-service--snapshot scope base))
         (revision (memory-source-text-digest text)))
    (record-collection--check-capacity (knowledge-service-snapshots service)
                                      :revision revision :text text :reject #'knowledge-service--reject)
    (setf (gethash revision (knowledge-service-snapshots service)) text)
    revision))

(serapeum/types:-> knowledge-service--guard
    (knowledge-service parent-proto:knowledge-request &key (:scope keyword) (:base knowledge-base)) null)
(defun knowledge-service--guard (service request &key scope base)
  "Require an issued observation matching the complete current authorized scope."
  (let* ((revision (parent-proto:knowledge-request.expected-revision request))
         (issued   (gethash revision (knowledge-service-snapshots service))))
    (unless (and (parent-proto:knowledge-request.has-expected-revision request) issued
                 (equal issued (knowledge-service--snapshot scope base)))
      (knowledge-service--reject ':revision-conflict)))
  nil)

(serapeum/types:-> knowledge-service--admit-proposal
    (knowledge-service &key (:scope keyword) (:kind keyword) (:id string) (:version integer)) null)
(defun knowledge-service--admit-proposal (service &key scope kind id version)
  "Admit exact proposed data only through trusted parent policy."
  (let* ((context   (knowledge-service-context service))
         (base      (knowledge-base-read context))
         (workspace (and (eq scope ':workspace) (knowledge--workspace context)))
         (key       (list scope workspace id))
         (entry     (gethash key (if (eq kind ':rule) (knowledge-base-rules base)
                                    (knowledge-base-contracts-table base))))
         (proposal  (if (eq kind ':rule)
                        (getf (knowledge-rule-entry-version entry version) :rule)
                        (getf entry :contract))))
    (when (funcall (knowledge-service-approve service) kind scope id version (copy-tree proposal))
      (ecase kind
        (:rule
         (knowledge-admit-rule context id :scope scope :version version :by "parent-policy"))
        (:contract
         (knowledge-admit-contract context id :scope scope :version version
                                   :contract proposal :by "parent-policy")))))
  nil)

(serapeum/types:-> knowledge-service--response (knowledge-service parent-proto:knowledge-response) parent-proto:knowledge-response)
(defun knowledge-service--response (service response)
  "Bound the complete response before committing a receipt."
  (when (> (length (cl-protobufs:serialize-to-bytes response)) (knowledge-service-maximum-bytes service))
    (knowledge-service--reject ':resource-limit))
  response)

(serapeum/types:-> knowledge-service--fingerprint (parent-proto:knowledge-request) string)
(defun knowledge-service--fingerprint (request)
  "Hash canonical request fields, including operation, scope and revision guard."
  (let* ((copy   (cl-protobufs:deserialize-from-bytes 'parent-proto:knowledge-request
                                                   (cl-protobufs:serialize-to-bytes request)))
         (fields (when (parent-proto:knowledge-request.has-fields copy)
                   (session-store--value-data
                    (common-proto:make-value :map-value (parent-proto:knowledge-request.fields copy))))))
    (when fields
      (setf (parent-proto:knowledge-request.fields copy)
            (common-proto:value.map-value (session-store--data-value fields))))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence ':sha256 (cl-protobufs:serialize-to-bytes copy)))))

(serapeum/types:-> knowledge-service--write
    (knowledge-service parent-proto:knowledge-request &key (:scope keyword) (:function function))
    parent-proto:knowledge-response)
(defun knowledge-service--write (service request &key scope function)
  "Replay a committed result before checking guards, or commit data and result together."
  (let* ((context   (knowledge-service-context service))
         (operation (parent-proto:knowledge-request.operation-id request))
         (key       (list (knowledge-service-session-id service) operation))
         (payload   (knowledge-service--fingerprint request)))
    (unless (and (session-store--identifier-p operation) (<= (length operation) 256))
      (knowledge-service--reject ':invalid-request))
    (knowledge-call-with-batch context
     (lambda (full-base)
       (let ((receipt (gethash key (knowledge-base-operation-receipts full-base))))
         (when receipt
           (unless (equal payload (getf receipt :payload))
             (knowledge-service--reject ':invalid-request))
           (return-from knowledge-service--write
             (cl-protobufs:deserialize-from-bytes 'parent-proto:knowledge-response
                                                  (ironclad:hex-string-to-byte-array
                                                   (getf receipt :result))))))
       (let* ((response (knowledge-service--response service (funcall function)))
              (result
               (ironclad:byte-array-to-hex-string (cl-protobufs:serialize-to-bytes response))))
         (knowledge-transact context
                             (lambda (base)
                               (declare (ignore base))
                               (values
                                (list
                                 (list ':operation-committed :version 1 :session (first key)
                                       :operation operation :payload payload :result result))
                                response t)))))
     :select (lambda (base) (knowledge-service-authorized-base service scope base)))))
