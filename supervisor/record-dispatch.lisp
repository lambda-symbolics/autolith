;;;; -- Parent Record Collection Dispatch --

(in-package #:autolith-supervisor)

(define-condition record-dispatch-error (error)
  ((code :initarg :code :reader record-dispatch-error-code
         :documentation "Fixed public failure code."))
  (:documentation "A record dispatch failed without private details.")
  (:report (lambda (condition stream)
             (format stream "Record dispatch failed: ~A" (record-dispatch-error-code condition)))))

(defclass record-dispatcher ()
  ((session-id :initarg :session-id :reader record-dispatcher-session-id
               :documentation "Trusted session that owns namespace claims.")
   (store :initarg :store :reader record-dispatcher-store
          :documentation "Atomic claim log, without domain results.")
   (bindings :initarg :bindings :reader record-dispatcher-bindings
             :documentation "Exact trusted collection-to-service bindings."))
  (:documentation "Dispatch records.v1 with one durable operation namespace per session."))

(serapeum/types:-> record-dispatch--reject (keyword) nil)
(defun record-dispatch--reject (code)
  "Signal a fixed public failure."
  (error 'record-dispatch-error :code code))

(serapeum/types:-> record-dispatch--claim-p (t) boolean)
(defun record-dispatch--claim-p (form)
  "Validate the exact bounded durable claim schema."
  (and (session-store--shape-p form '(:session :operation :collection :fingerprint))
       (eq (first form) ':claim)
       (every #'session-store--identifier-p
              (list (getf (rest form) :session) (getf (rest form) :operation)))
       (member (getf (rest form) :collection) '(:agenda :memory :papercut))
       (let ((digest (getf (rest form) :fingerprint)))
         (and (stringp digest) (= 64 (length digest))
              (every (lambda (character) (not (null (digit-char-p character 16)))) digest)))
       t))

(serapeum/types:-> record-dispatch--claim-store (pathname) sexp-store:log-store)
(defun record-dispatch--claim-store (pathname)
  "Describe the locked claim log. Fail closed at 16384 claims; never evict."
  (make-instance 'sexp-store:log-store
                 :pathname         pathname
                 :lock-pathname    (pathname (concatenate 'string (namestring pathname) ".lock"))
                 :header           '(:record-claims :version 1)
                 :header-validator (lambda (form) (equal form '(:record-claims :version 1)))
                 :initial-state    (lambda () (make-hash-table :test 'equal))
                 :validator        #'record-dispatch--claim-p
                 :reducer          (lambda (state claim)
                                     (let ((key (list (getf (rest claim) :session)
                                                      (getf (rest claim) :operation))))
                                       (when (or (gethash key state) (>= (hash-table-count state) 16384))
                                         (record-dispatch--reject ':unavailable))
                                       (setf (gethash key state) claim)
                                       state))))

(serapeum/types:-> make-record-dispatcher
                  (&key (:pathname pathname) (:session-id string)
                   (:agenda-service t) (:memory-service t) (:global-memory-service t)
                   (:papercut-service t)) record-dispatcher)
(defun make-record-dispatcher
    (&key pathname session-id agenda-service memory-service global-memory-service papercut-service)
  "Bind trusted services. Global scope requires a separate, explicitly authorized service."
  (unless (and pathname (session-store--identifier-p session-id)
               (or agenda-service memory-service global-memory-service papercut-service)
               (or (null agenda-service)
                   (and (typep agenda-service 'agenda-service)
                        (string= session-id (agenda-service-session-id agenda-service))))
               (or (null memory-service)
                   (and (typep memory-service 'memory-service)
                        (eq (memory-service-scope memory-service) ':workspace)
                        (string= session-id (memory-service-session-id memory-service))))
               (or (null global-memory-service)
                   (and (typep global-memory-service 'memory-service)
                        (eq (memory-service-scope global-memory-service) ':global)
                        (memory-service-allow-global-p global-memory-service)
                      (or (null memory-service)
                          (eq (memory-service-context memory-service)
                              (memory-service-context global-memory-service)))
                      (string= session-id (memory-service-session-id global-memory-service))))
               (or (null papercut-service)
                   (and (typep papercut-service 'papercut-service)
                        (string= session-id (papercut-service-session-id papercut-service)))))
    (record-dispatch--reject ':invalid-request))
  (make-instance 'record-dispatcher :session-id (copy-seq session-id)
                 :store                         (record-dispatch--claim-store pathname)
                 :bindings                      (list (cons ':agenda agenda-service) (cons ':memory memory-service)
                                                      (cons ':global-memory global-memory-service)
                                                      (cons ':papercut papercut-service))))

(serapeum/types:-> record-dispatch--domain (record-dispatcher parent-proto:record-request) t)
(defun record-dispatch--domain (service request)
  "Select only a configured service. Reject scope fields outside the memory collection."
  (let* ((collection (parent-proto:record-request.collection request))
         (scoped-p   (parent-proto:record-request.has-scope request))
         (scope      (and scoped-p (parent-proto:record-request.scope request))))
    (when (and scoped-p
               (or (not (eq collection ':memory)) (not (member scope '(:workspace :global)))))
      (record-dispatch--reject ':invalid-request))
    (when (and (eq collection ':memory) (parent-proto:record-request.has-fields request))
      (let* ((fields (protocol-value->data
                      (common-proto:make-value :map-value (parent-proto:record-request.fields request))))
             (target (gethash "scope" fields)))
        (when (and (member target '("global" "workspace") :test #'equal)
                   (null (rest (assoc (if (equal target "global") ':global-memory ':memory)
                                      (record-dispatcher-bindings service)))))
          (record-dispatch--reject ':unsupported))))
    (let ((domain (rest (assoc (if (eq scope ':global) ':global-memory collection)
                               (record-dispatcher-bindings service)))))
      (unless domain (record-dispatch--reject ':unsupported))
      domain)))

(serapeum/types:-> record-dispatch--scope (parent-proto:record-request) keyword)
(defun record-dispatch--scope (request)
  "Return the effective scope, including the records.v1 workspace default."
  (if (and (parent-proto:record-request.has-scope request)
           (eq (parent-proto:record-request.scope request) ':global))
      ':global
      ':workspace))

(serapeum/types:-> record-dispatch--token (string keyword boolean) string)
(defun record-dispatch--token (token scope encode-p)
  "Bind public tokens to a scope. Accept old unprefixed tokens only for workspace."
  (let ((prefix (if (eq scope ':global) "global/" "workspace/")))
    (cond
      (encode-p
       (concatenate 'string prefix token))
      ((uiop:string-prefix-p prefix token)
       (subseq token (length prefix)))
      ((or (eq scope ':global) (uiop:string-prefix-p "global/" token)
           (uiop:string-prefix-p "workspace/" token))
       (record-dispatch--reject ':revision-conflict))
      (t
       token))))

(serapeum/types:-> record-dispatch--domain-request (parent-proto:record-request) parent-proto:record-request)
(defun record-dispatch--domain-request (request)
  "Detach scope and decode memory tokens. Keep original data in the namespace claim."
  (let ((copy  (cl-protobufs:deserialize-from-bytes
                'parent-proto:record-request (cl-protobufs:serialize-to-bytes request)))
        (scope (record-dispatch--scope request)))
    (when (eq (parent-proto:record-request.collection copy) ':memory)
      (when (parent-proto:record-request.has-cursor copy)
        (setf (parent-proto:record-request.cursor copy)
              (record-dispatch--token (parent-proto:record-request.cursor copy) scope nil)))
      (when (parent-proto:record-request.has-expected-revision copy)
        (setf (parent-proto:record-request.expected-revision copy)
              (record-dispatch--token (parent-proto:record-request.expected-revision copy) scope nil)))
      (when (parent-proto:record-request.has-expected-collection-revision copy)
        (setf (parent-proto:record-request.expected-collection-revision copy)
              (record-dispatch--token (parent-proto:record-request.expected-collection-revision copy) scope nil))))
    (when (parent-proto:record-request.has-scope copy)
      (parent-proto:record-request.clear-scope copy))
    copy))

(serapeum/types:-> record-dispatch--source-observation (memory-service parent-proto:record-request) null)
(defun record-dispatch--source-observation (service request)
  "Reject old scope-move observations when used through the wrong source scope."
  (when (parent-proto:record-request.has-expected-revision request)
    (bordeaux-threads:with-lock-held ((memory-service-lock service))
      (let ((snapshot (gethash (list (parent-proto:record-request.record-id request)
                                    (parent-proto:record-request.expected-revision request))
                               (memory-service-snapshots service))))
        (when (and snapshot (not (eq (getf (rest snapshot) :scope) (memory-service-scope service))))
          (record-dispatch--reject ':revision-conflict)))))
  nil)

(serapeum/types:-> record-dispatch--handoff
                  (record-dispatcher memory-service parent-proto:record-response) null)
(defun record-dispatch--handoff (service source response)
  "Share a returned scope-move observation with its trusted destination service."
  (dolist (record (parent-proto:record-response.records response))
    (let* ((fields   (protocol-value->data
                      (common-proto:make-value :map-value (parent-proto:stored-record.fields record))))
           (target   (rest (assoc (if (equal (gethash "scope" fields) "global") ':global-memory ':memory)
                                 (record-dispatcher-bindings service))))
           (key      (list (parent-proto:stored-record.record-id record) (parent-proto:stored-record.revision record)))
           (snapshot (bordeaux-threads:with-lock-held ((memory-service-lock source))
                       (copy-tree (gethash key (memory-service-snapshots source))))))
      (when (and target snapshot (not (eq source target)))
        (bordeaux-threads:with-lock-held ((memory-service-lock target))
          ;; A full destination requires a fresh session, as for a normal full query.
          (when (< (hash-table-count (memory-service-snapshots target)) 4096)
            (setf (gethash key (memory-service-snapshots target)) snapshot))))))
  nil)

(serapeum/types:-> record-dispatch--response
                  (parent-proto:record-response parent-proto:record-request) parent-proto:record-response)
(defun record-dispatch--response (response request)
  "Namespace public memory tokens without changing the domain's durable receipt."
  (if (not (eq (parent-proto:record-request.collection request) ':memory))
      response
      (let ((copy  (cl-protobufs:deserialize-from-bytes
                    'parent-proto:record-response (cl-protobufs:serialize-to-bytes response)))
            (scope (record-dispatch--scope request)))
        (dolist (record (parent-proto:record-response.records copy))
          (let* ((fields       (protocol-value->data
                                (common-proto:make-value :map-value (parent-proto:stored-record.fields record))))
                 (record-scope (if (equal (gethash "scope" fields) "global") ':global ':workspace)))
            (setf (parent-proto:stored-record.revision record)
                  (record-dispatch--token (parent-proto:stored-record.revision record) record-scope t))))
        (when (parent-proto:record-response.has-next-cursor copy)
          (setf (parent-proto:record-response.next-cursor copy)
                (record-dispatch--token (parent-proto:record-response.next-cursor copy) scope t)))
        (when (parent-proto:record-response.has-collection-revision copy)
          (setf (parent-proto:record-response.collection-revision copy)
                (record-dispatch--token (parent-proto:record-response.collection-revision copy) scope t)))
        copy)))

(serapeum/types:-> record-dispatch--fingerprint (parent-proto:record-request) string)
(defun record-dispatch--fingerprint (request)
  "Hash canonical request data. Map field order has no semantic effect."
  (let* ((copy   (cl-protobufs:deserialize-from-bytes
                  'parent-proto:record-request (cl-protobufs:serialize-to-bytes request)))
         (fields (when (parent-proto:record-request.has-fields copy)
                   (session-store--value-data
                    (common-proto:make-value :map-value (parent-proto:record-request.fields copy))))))
    (when fields
      (setf (parent-proto:record-request.fields copy)
            (common-proto:value.map-value (session-store--data-value fields))))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence ':sha256 (cl-protobufs:serialize-to-bytes copy)))))

(serapeum/types:-> record-dispatch--reserve (record-dispatcher parent-proto:record-request) null)
(defun record-dispatch--reserve (service request)
  "Commit the namespace claim before domain dispatch. An identical claim can retry."
  (let* ((operation   (parent-proto:record-request.operation-id request))
         (collection  (parent-proto:record-request.collection request))
         (fingerprint (record-dispatch--fingerprint request))
         (session     (record-dispatcher-session-id service)))
    (unless (session-store--identifier-p operation)
      (record-dispatch--reject ':invalid-request))
    (let ((*read-eval* nil))
      (handler-case
          (sexp-store:store-transact
           (record-dispatcher-store service)
           (lambda (state)
             (let* ((prior (gethash (list session operation) state))
                    (claim (list ':claim :session session :operation operation
                                 :collection collection :fingerprint fingerprint)))
               (cond (prior
                      (unless (equal prior claim) (record-dispatch--reject ':revision-conflict))
                      (values nil nil nil))
                     ((>= (hash-table-count state) 16384)
                      (record-dispatch--reject ':resource-limit))
                     (t
                      (values (list claim) nil t))))))
        (sexp-store:store-error () (record-dispatch--reject ':unavailable)))))
  nil)

(defmethod session-record-scopes-p ((service record-dispatcher))
  "Offer explicit record scopes for this trusted dispatcher."
  (declare (ignore service))
  t)

(defmethod session-record-attach ((service record-dispatcher) runtime)
  "Reject a trusted binding to the wrong session."
  (unless (string= (record-dispatcher-session-id service)
                   (session-store-id (session-runtime-store runtime)))
    (record-dispatch--reject ':invalid-request))
  nil)

(defmethod session-record-handle ((service record-dispatcher) request)
  "Reserve write identity, then use the exact collection's sole result ledger."
  (when (> (length (cl-protobufs:serialize-to-bytes request)) 1048576)
    (record-dispatch--reject ':resource-limit))
  (handler-case
      (let* ((collection     (parent-proto:record-request.collection request))
             (domain         (record-dispatch--domain service request))
             (domain-request (record-dispatch--domain-request request)))
        (when (eq collection ':memory)
          (record-dispatch--source-observation domain domain-request))
        (unless (eq (parent-proto:record-request.operation request) ':query)
          (record-dispatch--reserve service request))
        (let ((response (case collection
                          (:agenda
                           (agenda-service-handle domain domain-request))
                          (:memory
                           (memory-service-handle domain domain-request))
                          (:papercut
                           (papercut-service-handle domain domain-request)))))
          (when (eq collection ':memory)
            (record-dispatch--handoff service domain response))
          (setf response (record-dispatch--response response request))
          (when (> (length (cl-protobufs:serialize-to-bytes response)) 1048576)
            (record-dispatch--reject ':resource-limit))
          response))
    (agenda-service-error (condition)
      (record-dispatch--reject (agenda-service-error-code condition)))
    (memory-record-error (condition)
      (record-dispatch--reject (memory-record-error-code condition)))
    (papercut-service-error (condition)
      (record-dispatch--reject (papercut-service-error-code condition)))
    (session-store-error () (record-dispatch--reject ':invalid-request))
    (protocol-value-error () (record-dispatch--reject ':invalid-request))))

(defmethod session-record-error-code ((condition record-dispatch-error))
  "Return the fixed record failure code."
  (record-dispatch-error-code condition))
