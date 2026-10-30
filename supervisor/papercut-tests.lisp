
(in-package #:autolith-supervisor)

(serapeum/types:-> papercut-tests--legacy-log nil integer)
(defun papercut-tests--legacy-log ()
  "Read version-one events and append service receipts to the same log."
  (let* ((checks 0)
         (directory
          (merge-pathnames
           (format nil "papercut-legacy-~A/" (papercut--make-identifier))
           (uiop/stream:temporary-directory)))
         (path   (merge-pathnames "papercuts.sexp" directory))
         (context
          (make-instance 'papercut-context :pathname path :workspace
                         "legacy-workspace"))
         (service
          (make-papercut-service :context context :session-id "new-session"))
         (report
          (make-instance 'papercut :identifier "legacy-report" :reported-at 1
                         :workspace "legacy-workspace" :title
                         "Legacy stored report" :content
                         "The previous version recorded this condition."
                         :source-conversation nil)))
    (ensure-directories-exist path)
    (labels ((check (value)
               (incf checks)
               (unless value
                 (error "Legacy papercut check ~D failed." checks)))

             (query ()
               (papercut-service-handle service
                (parent-proto:make-record-request :collection ':papercut
                                                  :operation ':query :limit
                                                  10))))
      (unwind-protect
          (progn
           (sexp-store:snapshot-write-text path
                                           (with-output-to-string (stream)
                                             (dolist
                                                 (record
                                                  (list
                                                   '(:papercuts :version 1)
                                                   (papercut--record report)
                                                   '(:papercut-telemetry
                                                     :version 1 :id
                                                     "legacy-report"
                                                     :telemetry-id
                                                     "11111111111111111111111111111111")
                                                   (papercut--assessed-record
                                                    "legacy-report" :verdict
                                                    ':improved :note
                                                    "The earlier probe passed."
                                                    :assessed-at 2)))
                                               (write record :stream stream
                                                      :readably t :pretty nil)
                                               (terpri stream))))
           (let* ((record
                   (first (parent-proto:record-response.records (query))))
                  (fields
                   (protocol-value->data
                    (common-proto:make-value :map-value
                                             (parent-proto:stored-record.fields
                                              record)))))
             (check
              (equal "legacy-report"
                     (parent-proto:stored-record.record-id record)))
             (check (equal "improved" (gethash "verdict" fields)))
             (check (not (gethash "source_conversation" fields)))
             (check (not (gethash "workspace" fields)))
             (check (not (gethash "telemetry_id" fields))))
           (check
            (equal "11111111111111111111111111111111"
                   (papercut-telemetry-id (first (papercut-list context)))))
           (let ((fields (make-hash-table :test #'equal)))
             (setf (gethash "title" fields) "New independent fault"
                   (gethash "content" fields)
                   "A different worker refused a valid message.")
             (papercut-service-handle service
              (parent-proto:make-record-request :collection   ':papercut
                                                :operation    ':create
                                                :operation-id "new-write"
                                                :fields
                                                (common-proto:value.map-value
                                                 (protocol-data->value
                                                  fields))))
             (check
              (= 1
                 (count "legacy-report" (papercut-list context) :key
                        #'papercut-identifier :test #'equal))))
           (check
            (= 2 (length (parent-proto:record-response.records (query)))))
           (check
            (= 1
               (length
                (second
                 (papercut-call-with-transaction context
                  (lambda (state) (values nil state nil)))))))
           (dolist (field '(:id :source-conversation))
             (let ((record (papercut--record report)))
               (setf (getf (rest record) field) (make-string 1025 :initial-element #\x))
               (sexp-store:snapshot-write-text path
                 (with-output-to-string (stream)
                   (write '(:papercuts :version 1) :stream stream :readably t)
                   (terpri stream)
                   (write record :stream stream :readably t)))
               (check (handler-case (progn (query) nil)
                        (papercut-service-error (condition)
                          (eq ':resource-limit (papercut-service-error-code condition)))))))
           (sexp-store:snapshot-write-text path "(:papercuts :version 1)
(:papercut-operation :version 1 :payload :malformed)
")
           (check
            (handler-case (progn (query) nil) (papercut-service-error nil t)))
           checks)
        (when (probe-file path) (delete-file path))))))

(serapeum/types:-> run-papercut-store-tests nil integer)
(defun run-papercut-store-tests ()
  "Check lifecycle storage, guards, durable receipts, scope, and response bounds."
  (let* ((checks   0)
         (directory
          (merge-pathnames
           (format nil "papercut-store-~A/" (papercut--make-identifier))
           (uiop/stream:temporary-directory)))
         (path     (merge-pathnames "papercuts.sexp" directory))
         (now      1000000)
         (sequence 0))
    (ensure-directories-exist path)
    (labels ((check (value)
               (incf checks)
               (unless value (error "Papercut store check ~D failed." checks)))

             (context (&optional (workspace "workspace-a"))
               (make-instance 'papercut-context :pathname path :workspace
                              workspace :clock (lambda () now)
                              :identifier-source
                              (lambda ()
                                (format nil "report-~D" (incf sequence)))))

             (service (&optional (workspace "workspace-a") (budget 65536))
               (make-papercut-service :context (context workspace) :session-id
                "session-a" :max-response-bytes budget))

             (fields (&rest pairs)
               (let ((data (make-hash-table :test #'equal)))
                (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
                       do (setf (gethash key data) value))
                (common-proto:value.map-value (protocol-data->value data))))

             (request (operation &rest arguments)
               (apply #'parent-proto:make-record-request :collection ':papercut
                      :operation                                     operation arguments))

             (create-request
                 (operation-id
                  &optional (title "Broken tool handler")
                  (content "The named worker rejects a valid request."))
               (request ':create :operation-id operation-id :fields
                (fields "title" title "content" content)))

             (record (response)
               (first (parent-proto:record-response.records response)))

             (data (record)
               (protocol-value->data
                (common-proto:make-value :map-value
                                         (parent-proto:stored-record.fields
                                          record))))

             (same-response-p (left right)
               (equalp (cl-protobufs:serialize-to-bytes left)
                       (cl-protobufs:serialize-to-bytes right)))

             (rejects (code function)
               (check
                (handler-case (progn (funcall function) nil)
                              (papercut-service-error (condition)
                               (eq code
                                   (papercut-service-error-code condition))))))

             (write-request (operation operation-id record &key fields)
               (request operation :operation-id operation-id :record-id
                (parent-proto:stored-record.record-id record)
                :expected-revision (parent-proto:stored-record.revision record)
                :fields            fields))

             (state ()
               (papercut-call-with-transaction (context)
                (lambda (current) (values nil current nil)))))
      (unwind-protect
          (progn
           (let* ((service  (service))
                  (create   (create-request "create-1"))
                  (created  (papercut-service-handle service create))
                  (original (record created)))
             (check
              (equal "report-1"
                     (parent-proto:stored-record.record-id original)))
             (check (equal "active" (gethash "status" (data original))))
             (check
              (equal "session-a"
                     (gethash "source_conversation" (data original))))
             (check (= now (gethash "reported_at" (data original))))
             (check
              (same-response-p created
               (papercut-service-handle (service) create)))
             (check (= 1 (length (second (state)))))
             (rejects ':invalid-request
              (lambda ()
                (papercut-service-handle (service "workspace-b") create)))
             (rejects ':invalid-request
              (lambda ()
                (papercut-service-handle service
                 (create-request "create-1" "Changed payload"))))
             (check (= 1 (length (second (state)))))
             (let ((duplicate
                    (papercut-service-handle service
                     (create-request "create-2"))))
               (check
                (equal (parent-proto:stored-record.record-id original)
                       (parent-proto:stored-record.record-id
                        (record duplicate)))))
             (check (= 1 (length (first (state)))))
             (check (= 2 (length (second (state)))))
             (let* ((assessment-fields
                     (fields "verdict" "unchanged" "note" "Still fails."))
                    (assess
                     (write-request ':assess "assess-1" original :fields
                      assessment-fields))
                    (assessed-response
                     (papercut-service-handle service assess))
                    (assessed (record assessed-response)))
               (check (equal "unchanged" (gethash "verdict" (data assessed))))
               (check (= now (gethash "assessed_at" (data assessed))))
               (check
                (not
                 (equal (parent-proto:stored-record.revision original)
                        (parent-proto:stored-record.revision assessed))))
               (check
                (same-response-p assessed-response
                 (papercut-service-handle (service) assess)))
               (rejects ':revision-conflict
                (lambda ()
                  (papercut-service-handle service
                   (write-request ':assess "stale-1" original :fields
                    assessment-fields))))
               (rejects ':invalid-request
                (lambda ()
                  (papercut-service-handle service
                   (write-request ':close "bad-close" assessed :fields
                    (fields "resolution" "Not resolved.")))))
               (incf now)
               (let* ((improved
                       (record
                        (papercut-service-handle service
                         (write-request ':assess "assess-2" assessed :fields
                          (fields "verdict" "improved" "note"
                           "Probe passed.")))))
                      (close
                       (write-request ':close "close-1" improved :fields
                        (fields "resolution" "Verified corrected behavior."
                         "diagnostic" "The relevant check passed.")))
                      (closed (papercut-service-handle service close)))
                 (check
                  (equal "closed" (gethash "status" (data (record closed)))))
                 (check
                  (same-response-p closed
                   (papercut-service-handle (service) close)))
                 (check (null (papercut-list (context))))
                 (check
                  (null
                   (parent-proto:record-response.records
                    (papercut-service-handle service
                     (request ':query :limit 10)))))
                 (rejects ':revision-conflict
                  (lambda ()
                    (papercut-service-handle service
                     (write-request ':assess "after-close" improved :fields
                      assessment-fields))))
                 (check
                  (same-response-p created
                   (papercut-service-handle (service) create))))))
           (let* ((service (service))
                  (alpha
                   (record
                    (papercut-service-handle service
                     (create-request "alpha" "Alpha failure"
                      "First distinct condition."))))
                  (beta
                   (record
                    (papercut-service-handle service
                     (create-request "beta" "Beta fault"
                      "Second independent problem."))))
                  (gamma
                   (record
                    (papercut-service-handle service
                     (create-request "gamma" "Gamma delay"
                      "Third unrelated performance issue."))))
                  (page
                   (papercut-service-handle service
                    (request ':query :query "" :limit 1)))
                  (cursor  (parent-proto:record-response.next-cursor page)))
             (declare (ignore alpha gamma))
             (check (= 3 (length (papercut-list (context)))))
             (check (= 1 (length (parent-proto:record-response.records page))))
             (check (parent-proto:record-response.has-next-cursor page))
             (check
              (null
               (parent-proto:record-response.records
                (papercut-service-handle (service "workspace-b")
                 (request ':query :limit 10)))))
             (rejects ':revision-conflict
              (lambda ()
                (papercut-service-handle (service "workspace-b")
                 (write-request ':assess "cross-scope" beta :fields
                  (fields "verdict" "improved" "note" "No authority.")))))
             (rejects ':invalid-request
              (lambda ()
                (papercut-service-handle service
                 (request ':query :query "changed" :limit 1 :cursor cursor))))
             (rejects ':invalid-request
              (lambda ()
                (papercut-service-handle (service)
                 (request ':query :query "" :limit 1 :cursor cursor))))
             (papercut-service-handle service
              (write-request ':assess "beta-update" beta :fields
               (fields "verdict" "improved" "note" "Verified independently.")))
             (let* ((next-page
                     (papercut-service-handle service
                      (request ':query :query "" :limit 10 :cursor cursor)))
                    (saved-beta
                     (find (parent-proto:stored-record.record-id beta)
                           (parent-proto:record-response.records next-page)
                           :key #'parent-proto:stored-record.record-id :test
                           #'equal)))
               (check
                (= 2
                   (length (parent-proto:record-response.records next-page))))
               (check saved-beta)
               (check (not (gethash "verdict" (data saved-beta))))
               (check
                (not
                 (parent-proto:record-response.has-next-cursor next-page))))
             (rejects ':invalid-request
              (lambda ()
                (papercut-service-handle service
                 (request ':query :query "" :limit 10 :cursor cursor))))
             (check
              (= 1
                 (length
                  (parent-proto:record-response.records
                   (papercut-service-handle service
                    (request ':query :query "Beta" :limit 10)))))))
           (let* ((small  (service "workspace-a" 256))
                  (before (uiop/stream:read-file-string path)))
             (rejects ':resource-limit
              (lambda ()
                (papercut-service-handle small
                 (create-request "large-write" "Oversized payload"
                  (make-string 2000 :initial-element #\x)))))
             (check (equal before (uiop/stream:read-file-string path)))
             (rejects ':resource-limit
              (lambda ()
                (papercut-service-handle small (request ':query :limit 10)))))
           (dolist
               (bad
                (list
                 (parent-proto:make-record-request :collection ':memory
                                                   :operation ':query :limit 1)
                 (request ':query :limit 0) (request ':query :limit 257)
                 (request ':query :limit 1 :record-id "injected")
                 (request ':query :limit 1 :operation-id "injected")
                 (request ':query :limit 1 :fields (fields "title" "bad"))
                 (request ':create :operation-id "empty")
                 (request ':create :operation-id "scope" :fields
                  (fields "title" "valid" "content" "valid" "workspace"
                   "outside"))
                 (request ':create :operation-id "wrong-type" :fields
                  (fields "title" 42 "content" "valid"))
                 (request ':create :operation-id "bad-kind" :fields
                  (fields "title" "valid" "content" "valid" "issue_kind"
                   "invented"))
                 (request ':create :operation-id "long-title" :fields
                  (fields "title" (make-string 201 :initial-element #\x)
                   "content" "valid"))
                 (request ':create :operation-id "diagnostic" :fields
                  (fields "title" "valid" "content" "valid" "diagnostic" ""))
                 (request ':create :operation-id "guard" :expected-revision
                  "bad" :fields (fields "title" "valid" "content" "valid"))
                 (request ':assess :operation-id "bad-verdict" :record-id
                  "report-2" :expected-revision "bad" :fields
                  (fields "verdict" "invented" "note" "note"))
                 (request ':close :operation-id "no-resolution" :record-id
                  "report-2" :expected-revision "bad" :fields
                  (fields "diagnostic" "only"))
                 (request ':replace :operation-id "replace")
                 (request ':delete :operation-id "delete")))
             (rejects
              (if (or
                   (not
                    (eq (parent-proto:record-request.collection bad)
                        ':papercut))
                   (member (parent-proto:record-request.operation bad)
                           '(:replace :delete)))
                  ':unsupported
                  ':invalid-request)
              (lambda () (papercut-service-handle (service) bad))))
           (let* ((original
                   (record
                    (papercut-service-handle (service)
                     (request ':query :limit 1))))
                  (gate     (bordeaux-threads:make-lock "papercut test start"))
                  (outcomes (make-array 2 :initial-element nil))
                  (threads  nil))
             (bordeaux-threads:with-lock-held (gate)
               (dotimes (index 2)
                 (let ((index index) (writer (service)))
                   (push
                    (bordeaux-threads:make-thread
                     (lambda ()
                       (bordeaux-threads:with-lock-held (gate)
                         nil)
                       (setf (aref outcomes index)
                             (handler-case
                              (progn
                               (papercut-service-handle writer
                                (write-request ':assess
                                 (format nil "race-~D" index) original
                                 :fields
                                 (fields "verdict" "improved" "note"
                                  (format nil "Concurrent result ~D"
                                          index))))
                               ':committed)
                              (papercut-service-error (condition)
                               (papercut-service-error-code condition)))))
                     :name "papercut guarded writer")
                    threads))))
             (dolist (thread threads) (bordeaux-threads:join-thread thread))
             (check (= 1 (count ':committed outcomes)))
             (check (= 1 (count ':revision-conflict outcomes))))
           (let* ((reader (service))
                  (first-page
                   (papercut-service-handle reader (request ':query :limit 1))))
             (dotimes (index 63)
               (papercut-service-handle reader (request ':query :limit 1)))
             (rejects ':resource-limit
              (lambda ()
                (papercut-service-handle reader (request ':query :limit 1))))
             (check
              (= 2
                 (length
                  (parent-proto:record-response.records
                   (papercut-service-handle reader
                    (request ':query :limit 10 :cursor
                     (parent-proto:record-response.next-cursor
                      first-page)))))))
             (check
              (parent-proto:record-response.has-next-cursor
               (papercut-service-handle reader (request ':query :limit 1)))))
           (let ((stored (state)))
             (check (= 3 (length (first stored))))
             (check (= 10 (length (second stored)))))
           (+ checks (papercut-tests--legacy-log)))
        (when (probe-file path) (delete-file path))))))
