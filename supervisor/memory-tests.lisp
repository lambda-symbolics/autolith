(in-package #:autolith-supervisor)

;;;; -- Standalone Memory Checks --

(defun memory-tests--fields (title content &optional tags)
  "Return typed writable fields for a memory request."
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "title" fields) title
          (gethash "content" fields) content
          (gethash "tags" fields) (coerce tags 'vector))
    (cl-protobufs.autolith.common.v1:value.map-value (protocol-data->value fields))))

(defun memory-tests--request (operation &rest arguments)
  "Return a typed memory request with the supplied operation arguments."
  (apply #'cl-protobufs.autolith.parent.v1:make-record-request
         :collection ':memory :operation operation arguments))

(defun memory-tests--service-bounds (context)
  "Check atomic response refusal and bounded stable cursor traversal through the service."
  (let ((checks 0)
        (path   (memory-context-memory-path context)))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Memory bounds check ~D failed." checks)))

             (rejects (function &optional (code ':resource-limit))
               (check (handler-case (progn (funcall function) nil)
                        (memory-record-error (condition)
                          (eq code (memory-record-error-code condition))))))

             (contents ()
               (and (probe-file path) (uiop:read-file-string path)))

             (query (service &optional cursor)
               (memory-service-handle
                service (if cursor
                            (memory-tests--request ':query :limit 1 :cursor cursor)
                            (memory-tests--request ':query :limit 1)))))
      (let* ((service (make-memory-service :context context :session-id "bounds" :maximum-bytes 128))
             (request (memory-tests--request
                       ':create :operation-id "bounded-create"
                       :fields                (memory-tests--fields "Bounded" "Atomic result.")))
             (before  (contents)))
        (dotimes (attempt 2)
          (declare (ignorable attempt))
          (rejects (lambda () (memory-service-handle service request)))
          (check (equal before (contents)))
          (check (zerop (hash-table-count (memory-service-snapshots service)))))
        (check (null (memory-records-read context)))
        (memory-state-transact
         context (lambda (state)
                   (check (zerop (hash-table-count (memory-state-receipts state))))))
        (setf (slot-value service 'maximum-bytes) (* 1024 1024))
        (let* ((response  (memory-service-handle service request))
               (encoded   (cl-protobufs:serialize-to-bytes response))
               (record    (first (parent-proto:record-response.records response)))
               (replace   (memory-tests--request
                           ':replace :operation-id "bounded-replace"
                           :record-id              (parent-proto:stored-record.record-id record)
                           :expected-revision      (parent-proto:stored-record.revision record)
                           :fields                 (memory-tests--fields "Changed" "Atomic replacement.")))
               (committed (contents)))
          (setf (slot-value service 'maximum-bytes) (1- (length encoded)))
          (rejects (lambda () (memory-service-handle service request)))
          (check (equal committed (contents)))
          (setf (slot-value service 'maximum-bytes) (length encoded))
          (check (equalp encoded (cl-protobufs:serialize-to-bytes
                                  (memory-service-handle service request))))
          (setf (slot-value service 'maximum-bytes) 128)
          (rejects (lambda () (memory-service-handle service replace)))
          (check (equal committed (contents)))
          (check (= 1 (hash-table-count (memory-service-snapshots service))))
          (setf (slot-value service 'maximum-bytes) (* 1024 1024))
          (let ((replaced (memory-service-handle service replace)))
            (check (not (equalp encoded (cl-protobufs:serialize-to-bytes replaced)))))))
      (let* ((service (make-memory-service :context context :session-id "snapshot-cap"))
             (request (memory-tests--request
                       ':create :operation-id "snapshot-cap"
                       :fields                (memory-tests--fields "Capped" "Retry after capacity is available.")))
             (before  (contents)))
        ;; Fill the authority table without writing thousands of unrelated records.
        (dotimes (index 4096)
          (setf (gethash (list "fixture" index) (memory-service-snapshots service)) '(fixture)))
        (rejects (lambda () (memory-service-handle service request)))
        (check (equal before (contents)))
        (memory-state-transact
         context (lambda (state)
                   (check (null (gethash '("snapshot-cap" "snapshot-cap")
                                        (memory-state-receipts state))))))
        (clrhash (memory-service-snapshots service))
        (let ((response (memory-service-handle service request)))
          (check (= 1 (length (parent-proto:record-response.records response))))))
      ;; One batch keeps setup cheap while the assertions use the service boundary.
      (memory-state-transact
       context (lambda (state)
                 (declare (ignore state))
                 (dotimes (index 256)
                   (memory-remember context :title   (format nil "Page ~D" index)
                                            :content "Stable paged result."))))
      (let ((service (make-memory-service :context context :session-id "pages")))
        (dotimes (traversal 2)
          (declare (ignorable traversal))
          (let ((response    (query service))
                (identifiers nil))
            (loop
              (push (parent-proto:stored-record.record-id
                     (first (parent-proto:record-response.records response))) identifiers)
              (check (<= (hash-table-count (memory-service-cursors service)) 1))
              (unless (parent-proto:record-response.has-next-cursor response) (return))
              (setf response (query service (parent-proto:record-response.next-cursor response))))
            (check (= 258 (length (remove-duplicates identifiers :test #'equal))))
            (check (zerop (hash-table-count (memory-service-cursors service))))))
        (let* ((first  (query service))
               (cursor (parent-proto:record-response.next-cursor first))
               (issued (hash-table-count (memory-service-snapshots service))))
          (setf (slot-value service 'maximum-bytes) 128)
          (dotimes (attempt 2)
            (declare (ignorable attempt))
            (rejects (lambda () (query service cursor)))
            (check (= 1 (hash-table-count (memory-service-cursors service))))
            (check (= issued (hash-table-count (memory-service-snapshots service)))))
          (setf (slot-value service 'maximum-bytes) (* 1024 1024))
          (rejects (lambda ()
                     (memory-service-handle service
                                            (memory-tests--request ':query :limit 1
                                                                   :query "wrong" :cursor cursor)))
                   ':invalid-request)
          (let* ((saved    (second (gethash cursor (memory-service-cursors service))))
                 (expected (cl-protobufs:serialize-to-bytes (first saved)))
                 (id       (parent-proto:stored-record.record-id (first saved))))
            (memory-forget context id)
            (let ((page (query service cursor)))
              (check (equalp expected (cl-protobufs:serialize-to-bytes
                                       (first (parent-proto:record-response.records page))))))
            (rejects (lambda () (query service cursor)) ':invalid-request)
            (check (= 1 (hash-table-count (memory-service-cursors service))))))
        (clrhash (memory-service-cursors service))
        (let ((cursor nil))
          (dotimes (index 256)
            (declare (ignorable index))
            (setf cursor (parent-proto:record-response.next-cursor (query service))))
          (let ((issued (hash-table-count (memory-service-snapshots service))))
            (rejects (lambda () (query service)))
            (check (= issued (hash-table-count (memory-service-snapshots service)))))
          (check (= 256 (hash-table-count (memory-service-cursors service))))
          (check (= 1 (length (parent-proto:record-response.records (query service cursor)))))
          (check (= 256 (hash-table-count (memory-service-cursors service))))))
      checks)))

(defun run-memory-tests (&key (standalone-p t))
  "Check parent memory durability, revision guards, scope, and evidence without agent code."
  (let* ((checks        0)
         (root          (merge-pathnames (format nil "autolith-memory-~A/" (memory--make-identifier))
                                         (uiop:temporary-directory)))
         (workspace     (merge-pathnames "workspace/" root))
         (other         (merge-pathnames "other/" root))
         (private       (merge-pathnames "private/" root))
         (path          (merge-pathnames "memories.sexp" private))
         (knowledge     (merge-pathnames "knowledge.sexp" private))
         (source        (merge-pathnames "source.txt" workspace))
         (context       (make-memory-store-context :memory-path path :knowledge-path knowledge
                                                   :workspace workspace))
         (other-context (make-memory-store-context :memory-path path :knowledge-path knowledge
                                                   :workspace other)))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Memory check ~D failed." checks)))

             (rejects (function &optional (code ':invalid-request))
               (check (handler-case (progn (funcall function) nil)
                        (memory-record-error (condition)
                          (eq code (memory-record-error-code condition))))))

             (record (response)
               (first (cl-protobufs.autolith.parent.v1:record-response.records response)))

             (identifier (record)
               (cl-protobufs.autolith.parent.v1:stored-record.record-id record))

             (revision (record)
               (cl-protobufs.autolith.parent.v1:stored-record.revision record))

             (query (service &rest arguments)
               (memory-service-handle service
                                      (apply #'memory-tests--request ':query :limit 256 arguments)))

             (view (selected)
               (let ((*memory-store-context* selected))
                 (knowledge-evaluate (knowledge-base-read selected)
                                     :workspace (namestring (memory-context-workspace selected))
                                     :check     (evidence-check-create
                                                 :memories (memory-list selected))))))
      (unwind-protect
           (progn
             (when standalone-p (check (null (find-package '#:autolith))))
             (dolist (directory (list workspace other private)) (ensure-directories-exist directory))
             (let* ((service        (make-memory-service :context context :session-id "session-a"))
                    (hidden-service (make-memory-service :context other-context :session-id "session-b"))
                    (fields         (memory-tests--fields "Build policy" "Use a fresh compiler." '("build")))
                    (create         (memory-tests--request ':create :operation-id "create-a" :fields fields))
                    (created        (record (memory-service-handle service create)))
                    (id             (identifier created)))
               (check (non-empty-string-p id))
               (check (= 1 (length (cl-protobufs.autolith.parent.v1:record-response.records
                                    (query service)))))
               (check (null (cl-protobufs.autolith.parent.v1:record-response.records
                             (query hidden-service))))
               (check (equal id (identifier (record (memory-service-handle service create)))))
               (let ((fresh (make-memory-service :context context :session-id "session-a")))
                 (check (equal id (identifier (record (memory-service-handle fresh create))))))
               (rejects (lambda ()
                          (memory-service-handle
                           service (memory-tests--request ':create :operation-id "create-a"
                                    :fields                                      (memory-tests--fields "Changed" "Conflicting reuse.")))))
               (let* ((replace  (memory-tests--request ':replace :operation-id "replace-a"
                                 :record-id id :expected-revision (revision created)
                                 :fields                                       (memory-tests--fields "Build policy" "Use pinned dependencies.")))
                      (replaced (record (memory-service-handle service replace))))
                 (check (not (equal (revision created) (revision replaced))))
                 (check (equal (revision replaced)
                               (revision (record (memory-service-handle service replace)))))
                 (rejects (lambda ()
                            (memory-service-handle
                             service (memory-tests--request ':replace :operation-id "stale-a"
                                      :record-id id :expected-revision (revision created)
                                      :fields                                       fields))) ':revision-conflict)
                 (rejects (lambda ()
                            (memory-service-handle
                             hidden-service (memory-tests--request ':delete :operation-id "hidden"
                                             :record-id id :expected-revision (revision replaced))))
                          ':revision-conflict)
                 (let ((delete (memory-tests--request ':delete :operation-id "delete-a"
                                :record-id id :expected-revision (revision replaced))))
                   (check (null (cl-protobufs.autolith.parent.v1:record-response.records
                                 (memory-service-handle service delete))))
                   (check (null (cl-protobufs.autolith.parent.v1:record-response.records
                                 (memory-service-handle
                                  (make-memory-service :context context :session-id "session-a")
                                  delete))))))
               (rejects (lambda () (make-memory-service :context context :session-id "global"
                                                        :scope ':global)))
               (let ((global (make-memory-service :context context :session-id "global"
                                                  :scope ':global :allow-global-p t)))
                 (memory-service-handle global
                                        (memory-tests--request ':create :operation-id "global"
                                         :fields                                      fields))
                 (check (= 1 (length (cl-protobufs.autolith.parent.v1:record-response.records
                                      (query global)))))
                 (check (null (cl-protobufs.autolith.parent.v1:record-response.records
                               (query service)))))
               (let ((bad (make-hash-table :test #'equal)))
                 (setf (gethash "title" bad) "Injected" (gethash "content" bad) "Path injection"
                       (gethash "memory_path" bad) "/private/not-approved")
                 (rejects (lambda ()
                            (memory-service-handle
                             service (memory-tests--request ':create :operation-id "bad-fields"
                                      :fields                                      (cl-protobufs.autolith.common.v1:value.map-value
                                                                                    (protocol-data->value bad)))))))
               (dolist (text '("One" "Two" "Three"))
                 (memory-service-handle service
                                        (memory-tests--request ':create :operation-id text
                                         :fields                                      (memory-tests--fields text "Paged result."))))
               (let* ((page   (memory-service-handle service (memory-tests--request ':query :limit 1)))
                      (cursor (cl-protobufs.autolith.parent.v1:record-response.next-cursor page)))
                 (check (= 1 (length (cl-protobufs.autolith.parent.v1:record-response.records page))))
                 (check (non-empty-string-p cursor))
                 (rejects (lambda () (query service :query "different" :cursor cursor)))
                 (rejects (lambda () (query hidden-service :cursor cursor)))
                 (check (= 2 (length (cl-protobufs.autolith.parent.v1:record-response.records
                                      (query service :cursor cursor))))))
               (let* ((memory   (first (memory-list context :visibility ':workspace)))
                      (snapshot (memory-record-snapshot memory)))
                 (memory-remember context :identifier (memory-identifier memory)
                                  :title "Concurrent change" :content "Current state differs.")
                 (rejects (lambda ()
                            (memory-records-mutate context :session-id "guard" :operation-id "guard"
                                                   :operation         ':replace
                                                   :record-id         (memory-identifier memory)
                                                   :expected-revision (memory-record-revision snapshot)
                                                   :expected-snapshot snapshot
                                                   :fields            '(:title "Stale" :content "Rejected.")))
                          ':revision-conflict)
                 (check (equal "Concurrent change"
                               (memory-title (memory-find context (memory-identifier memory)))))))
             (sexp-store:snapshot-write-text source "The artifact is ready for release.")
             (knowledge-define-schema
              context (knowledge-make-schema
                       :predicate "artifact.status"
                       :arguments '((:name "artifact" :type :entity) (:name "status" :type :string))
                       :key '("artifact") :template "{artifact} is {status}"))
             (let* ((file-source (memory-source-observe-file source :time (knowledge-now)))
                    (evidence    (knowledge-observe :source    file-source
                                                    :workspace (namestring workspace)
                                                    :digest    (getf (rest file-source) :digest)
                                                    :adapter   "memory-tests"))
                    (claim       (knowledge-assert context '("artifact.status" "release" "ready")
                                                   :evidence evidence :basis ':observation))
                    (memory      (memory-remember context :title    "Release status"
                                                          :content  "The release is ready."
                                                          :sources  (list file-source)
                                                          :premises (list claim))))
               (check (eq ':claim (getf (first (memory-premises memory)) :kind)))
               (check (knowledge-node-supported-p (knowledge-view-node (view context) claim)))
               (check (null (knowledge-view-node (view other-context) claim)))
               (check (handler-case
                          (progn (memory-remember other-context :title "Hidden" :content "Hidden claim."
                                                                :premises (list claim)) nil)
                        (memory-error () t)))
               (sexp-store:snapshot-write-text source "The artifact is not ready.")
               (check (not (knowledge-node-supported-p (knowledge-view-node (view context) claim))))
               (check (eq ':stale (knowledge-node-applicability (knowledge-view-node (view context) claim))))
               (let ((replaced (memory-remember context :identifier (memory-identifier memory)
                                                        :title "Release status" :content "Still recorded.")))
                 (check (equal (memory-sources memory) (memory-sources replaced)))
                 (check (equal (memory-premises memory) (memory-premises replaced))))
               (delete-file source)
               (check (eq ':stale (knowledge-node-applicability (knowledge-view-node (view context) claim))))
               (check (handler-case
                          (progn (memory-remember context :title "Missing" :content "No such premise."
                                                          :premises '("c-missing")) nil)
                        (memory-error () t))))
             (let ((*memory-store-context* context))
               (check (eq ':unreadable (nth-value 1 (memory-source-read-file path)))))
             (+ checks
                (memory-tests--service-bounds
                 (make-memory-store-context
                  :memory-path    (merge-pathnames "bounded-memories.sexp" private)
                  :knowledge-path (merge-pathnames "bounded-knowledge.sexp" private)
                  :workspace      workspace))))
        (dolist (directory (list workspace other private))
          (dolist (file (uiop:directory-files directory)) (delete-file file))
          (when (probe-file directory) (uiop:delete-empty-directory directory)))
        (when (probe-file root) (uiop:delete-empty-directory root))))))
