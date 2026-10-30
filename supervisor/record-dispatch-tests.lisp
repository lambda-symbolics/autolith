;;;; -- Parent Aggregate Record Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> record-dispatch-test--fields (list) common-proto:value-map)
(defun record-dispatch-test--fields (pairs)
  "Encode writable fields in the supplied wire order."
  (common-proto:make-value-map
   :entries (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
                  collect (common-proto:make-value-entry :key key :value (protocol-data->value value)))))

(serapeum/types:-> record-dispatch-test--service (pathname) record-dispatcher)
(defun record-dispatch-test--service (directory)
  "Reopen all trusted services and the shared claim namespace."
  (make-record-dispatcher
   :pathname (merge-pathnames "claims.sexp" directory) :session-id "session"
   :agenda-service
   (make-agenda-service :session-id "session"
                        :context    (make-instance 'agenda-context
                                                   :pathname         (merge-pathnames "agendas.sexp" directory)
                                                   :directory        (namestring directory)
                                                   :memory-validator (constantly t)))
   :memory-service
   (make-memory-service :session-id "session"
                        :context    (make-memory-store-context
                                     :memory-path    (merge-pathnames "memories.sexp" directory)
                                     :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                                     :workspace      directory))
   :papercut-service
   (make-papercut-service :session-id "session"
                          :context    (make-instance 'papercut-context
                                                     :pathname  (merge-pathnames "papercuts.sexp" directory)
                                                     :workspace (namestring directory)))))

(serapeum/types:-> record-dispatch-test--fixture (function &key (:directory pathname) (:model-p boolean)) t)
(defun record-dispatch-test--fixture (function &key directory model-p)
  "Exchange actual framed requests with the sole runtime reader and writer."
  (uiop:with-temporary-file (:stream input :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
      (let* ((connection (make-instance 'parent-connection :input input :output output :instance-id "instance"))
             (service    (record-dispatch-test--service directory))
             (store      (session-store-open (merge-pathnames "session.sexp" directory) "session"))
             (clock      (list 10))
             (model      (when model-p
                           (make-model-service
                            :profiles     (make-hash-table :test 'equal)
                            :replay-store (model-replay-store-open (merge-pathnames "model.sexp" directory) "session")
                            :executor     (lambda (invocation callback)
                                            (declare (ignore invocation callback))
                                            (error "Record tests must not call a provider."))
                            :abort (constantly nil) :clock (lambda () (first clock)))))
             (runtime nil) (next-id 0) (read-position 0))
        (setf (parent-connection-phase connection) ':ready
              runtime (session-runtime-open store connection
                                            :features (append '("session.v1" "records.v1")
                                                              (when model '("model.v1" "model-items-v1")))
                                            :model-service model :record-service service
                                            :clock    (lambda () (first clock))))
        (labels ((exchange (envelope)
                   (incf next-id)
                   (setf (parent-proto:envelope.protocol-major envelope) 1
                         (parent-proto:envelope.instance-id envelope) "instance"
                         (parent-proto:envelope.message-id envelope) next-id)
                   (file-position input :end)
                   (let ((position (file-position input)))
                     (protocol-write-frame input (cl-protobufs:serialize-to-bytes envelope))
                     (file-position input position)
                     (session-runtime-receive runtime))
                   (sb-ext:with-timeout 5
                     (loop until (> (file-length output) read-position) do (sleep 0.001)))
                   (with-open-file (source (pathname output) :element-type '(unsigned-byte 8))
                     (file-position source read-position)
                     (let* ((bytes (protocol-read-frame source))
                            (reply (cl-protobufs:deserialize-from-bytes 'parent-proto:envelope bytes)))
                       (setf read-position (file-position source))
                       (assert (= next-id (parent-proto:envelope.reply-to reply)))
                       reply))))
          (unwind-protect (funcall function runtime #'exchange service clock)
            (session-runtime-disconnect runtime)
            (session-runtime-await-close runtime)))))))

(serapeum/types:-> record-dispatch-test--scopes (pathname) integer)
(defun record-dispatch-test--scopes (directory)
  "Check scope authority, token isolation, durable retries and omitted scope."
  (let* ((root      (merge-pathnames "scopes/" directory))
         (context   (make-memory-store-context :memory-path    (merge-pathnames "memories.sexp" root)
                                               :knowledge-path (merge-pathnames "knowledge.sexp" root)
                                               :workspace      root))
         (workspace (make-memory-service :context context :session-id "scopes"))
         (global    (make-memory-service :context context :session-id "scopes" :scope ':global :allow-global-p t))
         (claims    (merge-pathnames "claims.sexp" root))
         (checks    0))
    (ensure-directories-exist root)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Scoped record check ~D failed." checks)))

             (dispatcher (&optional global-p)
               (make-record-dispatcher :pathname claims :session-id "scopes" :memory-service workspace
                                       :global-memory-service (when global-p global)))

             (query (&optional scope cursor)
               (apply #'parent-proto:make-record-request :collection ':memory :operation ':query :limit 1
                      (append (when scope (list :scope scope)) (when cursor (list :cursor cursor))))))
      (flet ((rejects (service request code)
               (check (handler-case (progn (session-record-handle service request) nil)
                        (record-dispatch-error (condition) (eq code (record-dispatch-error-code condition)))))))
        (let* ((service         (dispatcher t))
               (denied          (dispatcher))
               (workspace-query (session-record-handle service (query)))
               (global-query    (session-record-handle service (query ':global)))
               (workspace-guard (parent-proto:record-response.collection-revision workspace-query))
               (global-guard    (parent-proto:record-response.collection-revision global-query))
               (fields          (record-dispatch-test--fields '("title" "Scoped" "content" "A scoped memory.")))
               (create          (parent-proto:make-record-request :collection ':memory :operation ':create
                                                                  :operation-id "global-create" :scope ':global
                                                                  :fields fields :expected-collection-revision global-guard)))
          (check (session-record-scopes-p service))
          (check (not (equal workspace-guard global-guard)))
          (rejects denied create ':unsupported)
          (check (not (probe-file claims)))
          (check (null (memory-list context :visibility ':all)))
          (let ((forged (cl-protobufs:deserialize-from-bytes
                         'parent-proto:record-request (cl-protobufs:serialize-to-bytes create))))
            (setf (parent-proto:record-request.expected-collection-revision forged) workspace-guard)
            (rejects service forged ':revision-conflict))
          (let* ((result (session-record-handle service create))
                 (fresh  (make-record-dispatcher
                          :pathname claims :session-id "scopes"
                          :memory-service        (make-memory-service :context context :session-id "scopes")
                          :global-memory-service (make-memory-service :context context :session-id "scopes"
                                                                      :scope ':global :allow-global-p t))))
            (check (= 1 (length (memory-list context :visibility ':global))))
            (check (equalp (cl-protobufs:serialize-to-bytes result)
                           (cl-protobufs:serialize-to-bytes (session-record-handle fresh create))))
            (let ((cross (cl-protobufs:deserialize-from-bytes
                          'parent-proto:record-request (cl-protobufs:serialize-to-bytes create))))
              (setf (parent-proto:record-request.scope cross) ':workspace
                    (parent-proto:record-request.expected-collection-revision cross) workspace-guard)
              (rejects service cross ':revision-conflict))
            (let* ((record       (first (parent-proto:record-response.records result)))
                   (wrong-delete (parent-proto:make-record-request
                                  :collection ':memory :operation ':delete :scope ':workspace
                                  :operation-id "wrong-delete" :record-id (parent-proto:stored-record.record-id record)
                                  :expected-revision (parent-proto:stored-record.revision record))))
              (rejects service wrong-delete ':revision-conflict)))
          (let ((legacy (parent-proto:make-record-request :collection ':memory :operation ':create
                                                         :operation-id "legacy" :fields fields)))
            (session-record-handle service legacy)
            (check (= 1 (length (memory-list context :visibility ':workspace))))
            (check (equalp (cl-protobufs:serialize-to-bytes (session-record-handle service legacy))
                           (cl-protobufs:serialize-to-bytes (session-record-handle (dispatcher t) legacy))))
            (setf (parent-proto:record-request.scope legacy) ':workspace)
            (rejects service legacy ':revision-conflict))
          (session-record-handle service (parent-proto:make-record-request
                                          :collection ':memory :operation ':create :scope ':global
                                          :operation-id "second-global" :fields fields))
          (let* ((page   (session-record-handle service (query ':global)))
                 (cursor (parent-proto:record-response.next-cursor page)))
            (check (parent-proto:record-response.has-next-cursor page))
            (rejects service (query ':workspace cursor) ':revision-conflict)
            (check (= 1 (length (parent-proto:record-response.records
                                 (session-record-handle service (query ':global cursor))))))))
        (let ((service (dispatcher)))
          (rejects service (query ':scope-unspecified) ':invalid-request)
          (rejects service (parent-proto:make-record-request
                            :collection ':agenda :operation ':query :limit 1 :scope ':workspace) ':invalid-request)
          (rejects service (parent-proto:make-record-request
                            :collection ':memory :operation ':create :operation-id "field-global"
                            :fields (record-dispatch-test--fields
                                     '("title" "Forbidden" "content" "No authority." "scope" "global"))) ':unsupported))
        (dolist (options (list (list :memory-service global)
                              (list :global-memory-service workspace)
                              (list :global-memory-service
                                    (make-memory-service :context context :session-id "other"
                                                         :scope ':global :allow-global-p t))))
          (check (handler-case
                     (progn (apply #'make-record-dispatcher :pathname claims :session-id "scopes" options) nil)
                   (record-dispatch-error (condition)
                     (eq ':invalid-request (record-dispatch-error-code condition)))))))
      checks)))

(serapeum/types:-> run-record-dispatch-tests () integer)
(defun run-record-dispatch-tests ()
  "Check framed collection routing, claims, crash retry, bounds and cancellation."
  (let* ((checks    0)
         (directory (merge-pathnames (format nil "record-dispatch-~36R/" (random (expt 36 20) (make-random-state t)))
                                     (uiop:temporary-directory)))
         (created   nil))
    (ensure-directories-exist directory)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Record dispatch check ~D failed." checks)))

             (request (collection operation &key operation-id fields)
               (parent-proto:make-envelope
                :record-request
                (apply #'parent-proto:make-record-request
                       :collection collection :operation operation
                       (append (when operation-id (list :operation-id operation-id))
                               (when fields (list :fields (record-dispatch-test--fields fields)))
                               (when (eq operation ':query) (list :limit 256 :query ""))))))

             (failure-p (reply code)
               (and (parent-proto:envelope.has-failure reply)
                    (eq code (parent-proto:failure.code (parent-proto:envelope.failure reply)))))

             (record (reply)
               (first (parent-proto:record-response.records (parent-proto:envelope.record-response reply)))))
      (unwind-protect
           (progn
             (incf checks (record-dispatch-test--collection-guards directory))
            (incf checks (record-dispatch-test--scopes directory))
            (record-dispatch-test--fixture
             (lambda (runtime exchange service clock)
               (declare (ignore runtime clock))
               (dolist (collection '(:agenda :memory :papercut))
                 (check (parent-proto:envelope.has-record-response (funcall exchange (request collection ':query)))))
               (dolist (entry '((:agenda "agenda-create" ("text" "task" "status" "todo"))
                                (:memory "memory-create" ("title" "fact" "content" "body"))
                                (:papercut "papercut-create" ("title" "fault" "content" "evidence"))))
                 (destructuring-bind (collection operation fields) entry
                   (let* ((reply  (funcall exchange (request collection ':create :operation-id operation :fields fields)))
                          (stored (record reply)))
                     (check stored)
                     (push (list collection operation fields stored) created)
                     (let ((retry (funcall exchange (request collection ':create :operation-id operation :fields fields))))
                       (check (equalp (cl-protobufs:serialize-to-bytes stored)
                                      (cl-protobufs:serialize-to-bytes (record retry))))))))
               (check (failure-p (funcall exchange (request ':memory ':create :operation-id "agenda-create"
                                                            :fields                         '("title" "fact" "content" "body")))
                                 ':revision-conflict))
               (check (failure-p (funcall exchange (request ':agenda ':create :operation-id "agenda-create"
                                                            :fields                         '("text" "changed" "status" "todo")))
                                 ':revision-conflict))
               (let ((retry (funcall exchange (request ':agenda ':create :operation-id "agenda-create"
                                                      :fields                          '("status" "todo" "text" "task")))))
                 (check (record retry)))
               (check (failure-p (funcall exchange (request ':agenda ':create :operation-id "invalid"
                                                            :fields                         '("scope" "global" "text" "task")))
                                 ':invalid-request))
               (check (failure-p (funcall exchange (parent-proto:make-envelope
                                                    :cancel-request (parent-proto:make-cancel-request :request-id 999)))
                                 ':invalid-request))
               (check (failure-p (funcall exchange (parent-proto:make-envelope
                                                    :access-request (parent-proto:make-access-request)))
                                 ':unsupported))
               (let* ((pending (request ':agenda ':create :operation-id "crash-before-domain"
                                        :fields                         '("text" "after crash" "status" "todo")))
                      (payload (parent-proto:envelope.record-request pending)))
                 (record-dispatch--reserve service payload)
                 (check (record-dispatch--claim-p
                         (gethash '("session" "crash-before-domain")
                                  (sexp-store:store-read (record-dispatcher-store service)))))))
             :directory directory)
             ;; Reopen every service. No process-local result cache can satisfy these retries.
            (record-dispatch-test--fixture
             (lambda (runtime exchange service clock)
               (declare (ignore runtime service clock))
               (dolist (entry created)
                 (destructuring-bind (collection operation fields stored) entry
                   (let ((reply (funcall exchange (request collection ':create :operation-id operation :fields fields))))
                     (check (equalp (cl-protobufs:serialize-to-bytes stored)
                                    (cl-protobufs:serialize-to-bytes (record reply)))))))
               (check (record (funcall exchange (request ':agenda ':create :operation-id "crash-before-domain"
                                                        :fields                          '("text" "after crash" "status" "todo")))))
               (check (failure-p (funcall exchange (request ':papercut ':create :operation-id "memory-create"
                                                            :fields                           '("title" "fault" "content" "evidence")))
                                 ':revision-conflict)))
             :directory directory :model-p t)
            (record-dispatch-test--fixture
             (lambda (runtime exchange service clock)
               (declare (ignore runtime service))
               (loop repeat 64
                     do (check (parent-proto:envelope.has-record-response
                                (funcall exchange (request ':agenda ':query)))))
               (check (failure-p (funcall exchange (request ':agenda ':query)) ':resource-limit))
               (incf (first clock))
               (check (parent-proto:envelope.has-record-response (funcall exchange (request ':agenda ':query)))))
             :directory directory)
            (let* ((service (record-dispatch-test--service directory))
                   (path    (merge-pathnames "claims.sexp" directory)))
              (check (= 5 (hash-table-count (sexp-store:store-read (record-dispatcher-store service)))))
              (with-open-file (stream path :direction ':output :if-exists ':supersede)
                (write-string "#.(error \"must not evaluate\")" stream))
              (check (handler-case
                         (progn (record-dispatch--reserve service
                                  (parent-proto:envelope.record-request
                                   (request ':agenda ':create :operation-id "new" :fields '("text" "task"))))
                                nil)
                       (record-dispatch-error (condition)
                         (eq ':unavailable (record-dispatch-error-code condition))))))
            checks)
        (dolist (subdirectory (uiop:subdirectories directory))
          (dolist (file (uiop:directory-files subdirectory)) (delete-file file))
          (uiop:delete-empty-directory subdirectory))
        (dolist (file (uiop:directory-files directory)) (delete-file file))
        (uiop:delete-empty-directory directory)))))


;;;; -- Atomic Collection Guards --

(serapeum/types:-> record-dispatch-test--collection-guards (pathname) integer)
(defun record-dispatch-test--collection-guards (directory)
  "Race issued scope guards through separate services, then check receipts and stable pages."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Collection guard check ~D failed." checks)))

             (handle (service request)
               (ecase (parent-proto:record-request.collection request)
                 (:memory
                  (memory-service-handle service request))
                 (:agenda
                  (agenda-service-handle service request))
                 (:papercut
                  (papercut-service-handle service request))))

             (conflict-p (condition)
               (typecase condition
                 (memory-record-error
                  (eq (memory-record-error-code condition) ':revision-conflict))
                 (agenda-service-error
                  (eq (agenda-service-error-code condition) ':revision-conflict))
                 (papercut-service-error
                  (eq (papercut-service-error-code condition) ':revision-conflict)))))
      (dolist (collection '(:memory :agenda :papercut))
        (let* ((root              (merge-pathnames (format nil "~A-guards/" (string-downcase (symbol-name collection))) directory))
               (first-dispatcher  (progn (ensure-directories-exist root) (record-dispatch-test--service root)))
               (second-dispatcher (record-dispatch-test--service root))
               (first-service     (rest (assoc collection (record-dispatcher-bindings first-dispatcher))))
               (second-service    (rest (assoc collection (record-dispatcher-bindings second-dispatcher))))
               (path              (merge-pathnames (ecase collection (:memory
                                                                      "memories.sexp") (:agenda
                                                                                        "agendas.sexp")
                                                                  (:papercut
                                                                   "papercuts.sexp")) root))
               (fields            (record-dispatch-test--fields
                                   (if (eq collection ':agenda) '("text" "Guarded" "status" "todo")
                                       '("title" "Guarded" "content" "Exact collection snapshot."))))
               (query             (parent-proto:make-record-request :collection collection :operation ':query :limit 256))
               (observed          (handle first-service query))
               (revision          (parent-proto:record-response.collection-revision observed))
               (other             (handle second-service query))
               (requests          (loop for id in '("race-a" "race-b")
                                        collect (parent-proto:make-record-request
                                                 :collection collection :operation ':create :operation-id id
                                                 :fields fields :expected-collection-revision revision)))
               (results           (make-array 2 :initial-element nil))
               (gate              (bordeaux-threads:make-semaphore :count 0)))
          (check (parent-proto:record-response.has-collection-revision observed))
          (check (equal revision (parent-proto:record-response.collection-revision other)))
          (let ((workers
                  (loop for service in (list first-service second-service)
                        for request in requests for index from 0
                        collect (let ((selected service) (submitted request) (slot index))
                                  (bordeaux-threads:make-thread
                                   (lambda ()
                                     (bordeaux-threads:wait-on-semaphore gate)
                                     (setf (aref results slot)
                                           (handler-case (handle selected submitted)
                                             (error (condition) condition)))))))))
            (bordeaux-threads:signal-semaphore gate :count 2)
            (mapc #'bordeaux-threads:join-thread workers))
          (check (= 1 (count-if (lambda (result) (typep result 'parent-proto:record-response)) results)))
          (check (= 1 (count-if #'conflict-p results)))
          (let* ((winner          (position-if (lambda (result) (typep result 'parent-proto:record-response)) results))
                 (loser           (- 1 winner))
                 (before          (uiop:read-file-string path))
                 (winning-request (nth winner requests))
                 (winning-result  (aref results winner)))
            (check (handler-case (progn (handle first-service (nth loser requests)) nil)
                     (error (condition) (conflict-p condition))))
            (check (equal before (uiop:read-file-string path)))
            ;; An omitted guard keeps the low-level CREATE contract.
            (handle first-service (parent-proto:make-record-request
                                   :collection collection :operation ':create :operation-id "legacy-create"
                                   :fields (record-dispatch-test--fields
                                            (if (eq collection ':agenda)
                                                '("text" "Legacy" "status" "todo")
                                                '("title" "Legacy" "content" "Distinct unguarded create.")))))
            ;; A new service has no issued authority. Its committed receipt still wins the old guard.
            (let* ((fresh-dispatcher (record-dispatch-test--service root))
                   (fresh            (rest (assoc collection (record-dispatcher-bindings fresh-dispatcher)))))
              (check (equalp (cl-protobufs:serialize-to-bytes winning-result)
                             (cl-protobufs:serialize-to-bytes (handle fresh winning-request)))))
            (let* ((full   (handle first-service query))
                   (page   (handle first-service (parent-proto:make-record-request
                                                  :collection collection :operation ':query :limit 1)))
                   (stable (parent-proto:record-response.collection-revision page))
                   (cursor (parent-proto:record-response.next-cursor page)))
              (check (equal stable (parent-proto:record-response.collection-revision full)))
              (check (= 2 (length (parent-proto:record-response.records full))))
              (handle second-service (parent-proto:make-record-request
                                      :collection collection :operation ':create :operation-id "after-page"
                                      :fields (record-dispatch-test--fields
                                               (if (eq collection ':agenda)
                                                   '("text" "Later" "status" "todo")
                                                   '("title" "Later" "content" "Created after the first page.")))))
              (let ((next (handle first-service (parent-proto:make-record-request
                                                 :collection collection :operation ':query :limit 1 :cursor cursor))))
                (check (equal stable (parent-proto:record-response.collection-revision next)))
                (check (= 1 (length (parent-proto:record-response.records next))))
                (check (not (parent-proto:record-response.has-next-cursor next)))))))))
    checks))
