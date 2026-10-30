(in-package #:autolith-supervisor)

(serapeum/types:-> run-agenda-store-tests nil integer)
(defun run-agenda-store-tests ()
  "Check shared agenda migration, scoped links, transport, and guarded transactions."
  (let* ((checks    0)
         (directory
          (merge-pathnames (format nil "agenda-store-~A/" (agenda--make-identifier))
                           (uiop/stream:temporary-directory)))
         (workspace (merge-pathnames "workspace/" directory))
         (target    (merge-pathnames "target/" directory))
         (pathname  (merge-pathnames "agendas.sexp" directory)))
    (ensure-directories-exist workspace)
    (ensure-directories-exist target)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Agenda store check ~D failed." checks)))

             (context ()
               (make-instance 'agenda-context :pathname pathname :directory
                              (namestring workspace) :memory-validator
                              (lambda (id) (member id '("m1" "m2") :test #'equal))))

             (rejects (function)
               (check
                (handler-case (progn (funcall function) nil) (agenda-error nil t)))))
      (unwind-protect
          (progn
           (dolist (version '(1 2))
             (let* ((form
                     (list ':item :id "legacy" :text "old" :status ':todo :created-at 1
                           :updated-at 1))
                    (item
                     (if (= version 1)
                         form
                         (append form '(:memory-ids ("m1"))))))
               (sexp-store:snapshot-write-text pathname
                                               (write-to-string
                                                (list ':agendas :version version
                                                      :records
                                                      (list
                                                       (list ':agenda :directory
                                                             (namestring workspace)
                                                             :items (list item))))
                                                :readably t :pretty nil)))
             (let* ((context (context))
                    (state   (agenda-load context))
                    (item
                     (first (workspace-agenda-items (agenda-current context state)))))
               (check (equal "legacy" (agenda-item-identifier item)))
               (check
                (equal
                 (if (= version 1)
                     nil
                     '("m1"))
                 (agenda-item-memory-identifiers item)))
               (agenda-update context state "legacy" :text "upgraded")
               (check
                (equal "upgraded"
                       (agenda-item-text
                        (first
                         (workspace-agenda-items
                          (agenda-current context (agenda-load context)))))))
               (check
                (agenda--form-p
                 (let ((*read-eval* nil))
                   (with-open-file (stream pathname) (read stream)))))))
           (let* ((context (context)) (state (agenda-load context)))
             (rejects
              (lambda ()
                (agenda-add :configuration context :state state :text "bad"
                 :memory-identifiers '("outside"))))
             (let ((item
                    (agenda-add :configuration context :state state :text "linked"
                     :memory-identifiers '("m1" "m2"))))
               (check (equal '("m1" "m2") (agenda-item-memory-identifiers item))))
             (let ((copy
                    (agenda-transport :configuration context :state state
                     :source-directory workspace :target-directory target)))
               (check (= 2 (length (workspace-agenda-items copy))))
               (check
                (equal '("m1" "m2")
                       (agenda-item-memory-identifiers
                        (second (workspace-agenda-items copy))))))
             (agenda-transport :configuration context :state state :source-directory
              workspace :target-directory target :move-p t)
             (check (null (agenda-current context state)))
             (check
              (= 2
                 (length
                  (workspace-agenda-items (agenda-find state (namestring target)))))))
           (let* ((context  (context))
                  (state    (agenda-load context))
                  (item
                   (agenda-add :configuration context :state state :text "guarded"))
                  (snapshot (agenda--record->form (agenda-current context state))))
             (agenda-update (context) (make-instance 'agenda-state)
              (agenda-item-identifier item) :text "other writer")
             (rejects
              (lambda ()
                (agenda-call-with-transaction context state
                 (lambda (current)
                   (unless
                       (equal snapshot
                              (agenda--record->form (agenda-current context current)))
                     (error 'agenda-error :message "Stale snapshot." :pathname pathname
                            :operation ':revision-conflict :cause nil))
                   (agenda-update context current (agenda-item-identifier item) :text
                    "stale")))))
             (check
              (string= "other writer"
                       (agenda-item-text
                        (first
                         (workspace-agenda-items
                          (agenda-current context (agenda-load context)))))))
             (agenda-call-with-transaction context state
              (lambda (current)
                (agenda-update context current (agenda-item-identifier item) :text
                 "nested")))
             (check
              (string= "nested"
                       (agenda-item-text
                        (first
                         (workspace-agenda-items (agenda-current context state)))))))
           checks)
        (progn
         (dolist (file (uiop/filesystem:directory-files directory)) (delete-file file))
         (dolist (child (uiop/filesystem:subdirectories directory))
           (uiop/filesystem:delete-empty-directory child))
         (uiop/filesystem:delete-empty-directory directory))))))

(serapeum/types:-> run-agenda-service-tests nil integer)
(defun run-agenda-service-tests ()
  "Check records.v1 shape, durable deduplication, revisions, scopes, pages, and races."
  (let* ((checks          0)
         (directory
          (merge-pathnames (format nil "agenda-records-~A/" (agenda--make-identifier))
                           (uiop/stream:temporary-directory)))
         (workspace       (merge-pathnames "workspace/" directory))
         (other-workspace (merge-pathnames "other/" directory))
         (pathname        (merge-pathnames "agendas.sexp" directory)))
    (ensure-directories-exist workspace)
    (ensure-directories-exist other-workspace)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Agenda service check ~D failed." checks)))

             (service (&optional (key workspace) (session "session-a"))
               (make-agenda-service :context
                (make-instance 'agenda-context :pathname pathname :directory
                               (namestring key) :memory-validator
                               (lambda (id) (equal id "local-memory")))
                :session-id session))

             (fields (&key (text "task") (status "todo") (memories nil memories-p))
               (let ((map (make-hash-table :test #'equal)))
                 (setf (gethash "text" map) text
                       (gethash "status" map) status)
                 (when memories-p
                   (setf (gethash "memory_ids" map) (coerce memories 'vector)))
                 (common-proto:value.map-value (protocol-data->value map))))

             (query (owner &key (limit 256) cursor (text ""))
               (agenda-service-handle owner
                (apply #'parent-proto:make-record-request :collection ':agenda
                       :operation ':query :limit limit :query text
                       (when cursor (list :cursor cursor)))))

             (create (owner operation-id &optional (data (fields)))
               (agenda-service-handle owner
                (parent-proto:make-record-request :collection ':agenda :operation
                                                  ':create :operation-id operation-id
                                                  :fields data)))

             (replace-item
                 (owner operation-id item &optional (data (fields :text "changed")))
               (agenda-service-handle owner
                (parent-proto:make-record-request :collection ':agenda :operation
                                                  ':replace :operation-id operation-id
                                                  :record-id
                                                  (parent-proto:stored-record.record-id
                                                   item)
                                                  :expected-revision
                                                  (parent-proto:stored-record.revision
                                                   item)
                                                  :fields data)))

             (rejects (code function)
               (check
                (handler-case (progn (funcall function) nil)
                              (agenda-service-error (condition)
                               (eq code (agenda-service-error-code condition)))))))
      (unwind-protect
          (let* ((owner             (service))
                 (created
                  (first
                   (parent-proto:record-response.records (create owner "create-1"))))
                 (original-id       (parent-proto:stored-record.record-id created))
                 (original-revision (parent-proto:stored-record.revision created)))
            (check (agenda--string-p original-id))
            (check (agenda--string-p original-revision))
            (check (= 1 (length (parent-proto:record-response.records (query owner)))))
            (let ((retried
                   (first
                    (parent-proto:record-response.records
                     (create (service) "create-1")))))
              (check
               (string= original-id (parent-proto:stored-record.record-id retried)))
              (check
               (string= original-revision
                        (parent-proto:stored-record.revision retried))))
            (rejects ':invalid-request
             (lambda () (create owner "create-1" (fields :text "conflict"))))
            (rejects ':invalid-request
             (lambda () (create owner "create-1" (fields :memories nil))))
            (rejects ':invalid-request
             (lambda () (create (service other-workspace) "create-1")))
            (check
             (null
              (parent-proto:record-response.records
               (query (service other-workspace)))))
            (let* ((changed
                    (first
                     (parent-proto:record-response.records
                      (replace-item owner "replace-1" created))))
                   (retried
                    (first
                     (parent-proto:record-response.records
                      (replace-item (service) "replace-1" created)))))
              (check
               (string= (parent-proto:stored-record.revision changed)
                        (parent-proto:stored-record.revision retried)))
              (check
               (not
                (string= original-revision
                         (parent-proto:stored-record.revision changed))))
              (rejects ':revision-conflict
               (lambda () (replace-item owner "stale" created)))
              (replace-item owner "stale" changed))
            (dolist (status '("todo" "doing" "blocked" "done" "note"))
              (check
               (= 1
                  (length
                   (parent-proto:record-response.records
                    (create owner (concatenate 'string "status-" status)
                     (fields :status status)))))))
            (dolist
                (data
                 (list (fields :text "")
                       (fields :text (make-string 501 :initial-element #\x))
                       (fields :status "invalid") (fields :memories '("outside"))
                       (fields :memories '("local-memory" "local-memory"))))
              (rejects ':invalid-request
               (lambda () (create owner "invalid-fields" data))))
            (check
             (= 1
                (length
                 (parent-proto:record-response.records
                  (create owner "memory" (fields :memories '("local-memory")))))))
            (let ((data (make-hash-table :test #'equal)))
              (setf (gethash "text" data) "x"
                    (gethash "status" data) "todo"
                    (gethash "pathname" data) "/private/path")
              (rejects ':invalid-request
               (lambda ()
                 (create owner "path"
                  (common-proto:value.map-value (protocol-data->value data))))))
            (dolist
                (arguments
                 '((:operation :query :limit 0) (:operation :query :limit 257)
                   (:operation :query :limit 1 :operation-id "not-query")
                   (:operation :query :limit 1 :record-id "id")
                   (:operation :create :operation-id "new" :record-id "id")
                   (:operation :replace :operation-id "new")
                   (:operation :delete :operation-id "new")))
              (rejects ':invalid-request
               (lambda ()
                 (agenda-service-handle owner
                  (apply #'parent-proto:make-record-request :collection ':agenda
                         arguments)))))
            (rejects ':unsupported
             (lambda ()
               (agenda-service-handle owner
                (parent-proto:make-record-request :collection ':memory :operation
                                                  ':query :limit 1))))
            (let* ((page   (query owner :limit 1))
                   (cursor (parent-proto:record-response.next-cursor page))
                   (rest   (query owner :cursor cursor)))
              (check (parent-proto:record-response.has-next-cursor page))
              (check (= 6 (length (parent-proto:record-response.records rest))))
              (create owner "after-page")
              (check
               (= 6
                  (length
                   (parent-proto:record-response.records
                    (query owner :cursor cursor)))))
              (rejects ':invalid-request
               (lambda () (query owner :cursor cursor :text "different")))
              (rejects ':invalid-request
               (lambda () (query (service other-workspace) :cursor cursor))))
            (let* ((item (first (parent-proto:record-response.records (query owner))))
                   (request
                    (parent-proto:make-record-request :collection ':agenda :operation
                                                      ':delete :operation-id "delete"
                                                      :record-id
                                                      (parent-proto:stored-record.record-id
                                                       item)
                                                      :expected-revision
                                                      (parent-proto:stored-record.revision
                                                       item))))
              (check
               (null
                (parent-proto:record-response.records
                 (agenda-service-handle owner request))))
              (check
               (null
                (parent-proto:record-response.records
                 (agenda-service-handle (service) request))))
              (rejects ':revision-conflict
               (lambda () (replace-item owner "deleted" item))))
            (let* ((item
                    (first
                     (parent-proto:record-response.records
                      (create owner "race-source"))))
                   (owners   (list (service) (service)))
                   (outcomes (make-array 2 :initial-element nil))
                   (threads
                    (loop for contender in owners
                          for index from 0
                          collect (let ((index index) (contender contender))
                                    (bordeaux-threads:make-thread
                                     (lambda ()
                                       (setf (aref outcomes index)
                                             (handler-case
                                              (progn
                                               (replace-item contender
                                                (format nil "race-~D" index) item)
                                               ':committed)
                                              (agenda-service-error (condition)
                                               (agenda-service-error-code
                                                condition))))))))))
              (mapc #'bordeaux-threads:join-thread threads)
              (check (= 1 (count ':committed outcomes)))
              (check (= 1 (count ':revision-conflict outcomes))))
            (+ checks (run-agenda-service-bound-tests)))
        (progn
         (dolist (file (uiop/filesystem:directory-files directory)) (delete-file file))
         (dolist (child (uiop/filesystem:subdirectories directory))
           (uiop/filesystem:delete-empty-directory child))
         (uiop/filesystem:delete-empty-directory directory))))))

(serapeum/types:-> run-agenda-service-bound-tests nil integer)
(defun run-agenda-service-bound-tests ()
  "Check byte-limited pages and reject writes before their result would exceed the budget."
  (let* ((checks 0)
         (directory
          (merge-pathnames (format nil "agenda-bounds-~A/" (agenda--make-identifier))
                           (uiop/stream:temporary-directory)))
         (context
          (make-instance 'agenda-context :pathname
                         (merge-pathnames "agendas.sexp" directory) :directory
                         (namestring directory)))
         (small
          (make-agenda-service :context context :session-id "bounds"
           :max-response-bytes 256))
         (normal (make-agenda-service :context context :session-id "bounds"))
         (paged
          (make-agenda-service :context context :session-id "bounds"
           :max-response-bytes 1024)))
    (ensure-directories-exist directory)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Agenda bound check ~D failed." checks)))

             (create (owner id)
               (let ((fields (make-hash-table :test #'equal)))
                 (setf (gethash "text" fields) (make-string 500 :initial-element #\x)
                       (gethash "status" fields) "todo")
                 (agenda-service-handle owner
                  (parent-proto:make-record-request :collection ':agenda :operation
                                                    ':create :operation-id id :fields
                                                    (common-proto:value.map-value
                                                     (protocol-data->value fields))))))

             (query (owner &optional cursor)
               (agenda-service-handle owner
                (apply #'parent-proto:make-record-request :collection ':agenda
                       :operation ':query :limit 256
                       (when cursor (list :cursor cursor)))))

             (limited (function)
               (check
                (handler-case (progn (funcall function) nil)
                              (agenda-service-error (condition)
                               (eq (agenda-service-error-code condition)
                                   ':resource-limit))))))
      (unwind-protect
          (progn
           (limited (lambda () (create small "first")))
           (check (null (parent-proto:record-response.records (query small))))
           (create normal "first")
           (create normal "second")
           (create normal "third")
           (limited (lambda () (query small)))
           (let ((page (query paged)))
             (check (= 1 (length (parent-proto:record-response.records page))))
             (check (<= (length (cl-protobufs:serialize-to-bytes page)) 1024))
             (check (parent-proto:record-response.has-next-cursor page))
             (check
              (= 1
                 (length
                  (parent-proto:record-response.records
                   (query paged (parent-proto:record-response.next-cursor page)))))))
           (agenda-call-with-transaction context (make-instance 'agenda-state)
            (lambda (state)
              (setf (agenda-state-records state)
                    (list
                     (make-instance 'workspace-agenda :directory
                                    (namestring directory) :items
                                    (loop for index below 4097
                                          collect (make-instance 'agenda-item
                                                                 :identifier
                                                                 (format nil
                                                                         "bulk-~D"
                                                                         index)
                                                                 :text "x" :status
                                                                 ':todo :created-at 1
                                                                 :updated-at 1)))))))
           (limited (lambda () (query normal)))
           checks)
        (dolist (file (uiop/filesystem:directory-files directory)) (delete-file file))
        (uiop/filesystem:delete-empty-directory directory)))))
