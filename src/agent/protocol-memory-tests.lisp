;;;; -- Serialized Parent Memory Adapter Checks --

(in-package #:autolith)

(-> agent-memory-test--object (&rest t) hash-table)
(defun agent-memory-test--object (&rest properties)
  "Build portable string-keyed test data."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on properties by (lambda (tail) (rest (rest tail)))
          do (setf (gethash key object) value))
    object))

(-> agent-memory-test--evidence (agent-protocol-memory t pathname) null)
(defun agent-memory-test--evidence (adapter context directory)
  "Exercise checked evidence, retained observations, explicit clearing, and approved scope changes."
  (let* ((pathname (merge-pathnames "NOTES.md" directory))
         (sources  (list (agent-memory-test--object "file" "NOTES.md" "quote" "make release-prod")
                         (agent-memory-test--object "user" "Use Common Lisp for automation.")
                         (agent-memory-test--object "url" "https://example.test/notes" "quote" "make release-prod"))))
    (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
      (write-line "Deploy with make release-prod." stream))
    (let* ((base           (agent-protocol-memory-create adapter :title "evidence" :content "Deploy."
                                                        :sources sources :operation-id "evidence-base"))
           (derived        (agent-protocol-memory-create
                            adapter :title "derived" :content "Ship after deploying."
                            :premises     (list (format nil "memory:id/~A" (agent-protocol-memory-item-identifier base)))
                            :operation-id "evidence-derived"))
           (digest         (gethash "digest" (first (agent-protocol-memory-item-sources base))))
           (premise-digest (gethash "digest" (first (agent-protocol-memory-item-premises derived)))))
      (assert (= 3 (length (agent-protocol-memory-item-sources base))))
      (assert (memory-source-digest-p digest))
      (assert (equal "client-session"
                     (gethash "conversation" (second (agent-protocol-memory-item-sources base)))))
      (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
        (write-line "Deploy with make ship." stream))
      (setf base (agent-protocol-memory-replace adapter base :content "Changed deployment."
                                                :operation-id         "retain-sources"))
      (assert (equal digest (gethash "digest" (first (agent-protocol-memory-item-sources base)))))
      (setf derived (agent-protocol-memory-replace adapter derived :tags '("retagged")
                                                   :operation-id         "retain-premises"))
      (assert (equal premise-digest
                     (gethash "digest" (first (agent-protocol-memory-item-premises derived)))))
      (setf base (agent-protocol-memory-replace adapter base :sources nil :operation-id "clear-sources")
            derived (agent-protocol-memory-replace adapter derived :premises nil :operation-id "clear-premises"))
      (assert (null (agent-protocol-memory-item-sources base)))
      (assert (null (agent-protocol-memory-item-premises derived)))
      (loop for source in (list (agent-memory-test--object "user" "Invented user quotation.")
                                (agent-memory-test--object "file" "NOTES.md" "quote" "make release-prod")
                                (agent-memory-test--object "file" "/etc/hosts"))
            for index from 0
            do (assert (handler-case
                           (progn (agent-protocol-memory-create
                                   adapter :title "rejected" :content "Unsupported evidence."
                                   :sources (list source) :operation-id (format nil "reject-evidence-~D" index))
                                  nil)
                         (agent-protocol-memory-error (condition)
                           (eq ':invalid-request (agent-protocol-memory-error-code condition))))))
      (setf base (agent-protocol-memory-replace adapter base :scope ':global :operation-id "move-global"))
      (assert (eq ':global (agent-protocol-memory-item-scope base)))
      ;; The moved record can be edited before a query issues another observation.
      (setf base (agent-protocol-memory-replace adapter base :content "Global edit."
                                               :operation-id          "edit-moved-global"))
      (assert (find (agent-protocol-memory-item-identifier base)
                    (agent-protocol-memory-query adapter :scope ':global)
                    :test #'equal :key #'agent-protocol-memory-item-identifier))
      (assert (eq ':global (memory-scope (memory-find context (agent-protocol-memory-item-identifier base)))))
      (setf base (agent-protocol-memory-replace adapter base :scope ':workspace :operation-id "move-workspace"))
      (assert (eq ':workspace (agent-protocol-memory-item-scope base)))
      (setf base (agent-protocol-memory-replace
                  adapter base :sources (list (second sources)) :operation-id "rebind-user"))
      (assert (= 1 (length (agent-protocol-memory-item-sources base))))
      (let ((retry (agent-protocol-memory-create adapter :title "evidence" :content "Deploy."
                                                  :sources sources :operation-id "evidence-base")))
        (assert (equal digest (gethash "digest" (first (agent-protocol-memory-item-sources retry))))))
      (assert (handler-case
                  (progn (agent-record--validate-request
                          (agent-wire:make-record-request
                           :collection ':memory :operation ':create :operation-id "forged"
                           :fields (agent-protocol-memory--fields
                                    "forged" "Unsupported observation." nil
                                    :sources (list (agent-memory-test--object
                                                    "user" "Use Common Lisp for automation."
                                                    "observed-at" 0)))))
                         nil)
                (agent-protocol-error () t)))))
  nil)

(-> agent-memory-test--round-trip () null)
(defun agent-memory-test--round-trip ()
  "Use the shared memory store through a byte channel and the sole-reader driver."
  (let* ((directory   (merge-pathnames (format nil "agent-memory-~A/" (make-identifier))
                                       (uiop:temporary-directory)))
         (context     (autolith-supervisor::make-memory-store-context
                       :memory-path    (merge-pathnames "memories.sexp" directory)
                       :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                       :workspace directory :source-roots (list directory)))
         (service     (autolith-supervisor::make-record-dispatcher
                       :pathname (merge-pathnames "claims.sexp" directory) :session-id "client-session"
                       :memory-service
                       (autolith-supervisor::make-memory-service
                        :context context :session-id "client-session" :maximum-bytes 4096 :allow-global-p t
                        :input-history (constantly '("Use Common Lisp for automation.")))
                       :global-memory-service
                       (autolith-supervisor::make-memory-service
                        :context context :session-id "client-session" :maximum-bytes 4096
                        :scope ':global :allow-global-p t
                        :input-history (constantly '("Use Common Lisp for automation.")))))
         (write-count 0))
    (ensure-directories-exist directory)
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (parent)
            (let ((welcome (agent-protocol-test--welcome)))
               (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1" "record-scopes-v1"))
              (autolith-supervisor::parent-connection-handshake parent welcome))
            (loop for request = (autolith-supervisor::parent-connection-receive parent)
                  do (case (agent-wire:envelope.body-case request)
                       (agent-wire:output
                        (return))
                       (agent-wire:record-request
                        (unless (eq ':query (agent-wire:record-request.operation
                                             (agent-wire:envelope.record-request request)))
                          (incf write-count))
                        (let ((response
                                (handler-case
                                    (agent-wire:make-envelope
                                     :record-response
                                     (autolith-supervisor::session-record-handle
                                      service (agent-wire:envelope.record-request request)))
                                  (autolith-supervisor::record-dispatch-error (condition)
                                    (agent-wire:make-envelope
                                     :failure (agent-wire:make-failure
                                               :code (autolith-supervisor::record-dispatch-error-code
                                                      condition)))))))
                          (setf (agent-wire:envelope.reply-to response)
                                (agent-wire:envelope.message-id request))
                          (autolith-supervisor::parent-connection-send parent response)))
                       (otherwise
                        (assert nil)))))
          (lambda (client)
             (agent-protocol-handshake client :features '("session.v1" "records.v1" "record-scopes-v1"))
            (let* ((driver  (agent-protocol-driver-start client))
                   (adapter (make-agent-protocol-memory driver)))
              (unwind-protect
                   (let* ((original (agent-protocol-memory-create
                                     adapter :title "first" :content "original" :tags '("one")
                                     :operation-id "create-first"))
                          (updated  (agent-protocol-memory-replace
                                     adapter original :content "changed" :operation-id "replace-first"))
                          (retried  (agent-protocol-memory-create
                                     adapter :title "first" :content "original" :tags '("one")
                                     :operation-id "create-first")))
                     (assert (equal '("one") (agent-protocol-memory-item-tags updated)))
                     (assert (string= "changed" (agent-protocol-memory-item-content updated)))
                     (assert (string= (agent-protocol-memory-item-identifier original)
                                      (agent-protocol-memory-item-identifier retried)))
                     (assert (string= (agent-protocol-memory-item-revision original)
                                      (agent-protocol-memory-item-revision retried)))
                     (assert (not (string= (agent-protocol-memory-item-revision original)
                                          (agent-protocol-memory-item-revision updated))))
                     (assert (handler-case
                                 (progn (agent-protocol-memory-replace
                                         adapter original :content "stale" :operation-id "stale")
                                        nil)
                               (agent-protocol-memory-error (condition)
                                 (eq ':revision-conflict (agent-protocol-memory-error-code condition)))))
                     (assert (eq ':ready (autolith-supervisor::parent-connection-phase client)))
                     (agent-protocol-memory-create adapter :title "second" :content "next"
                                                   :operation-id "create-second")
                     (multiple-value-bind (page cursor) (agent-protocol-memory-query adapter :limit 1)
                       (assert (= 1 (length page)))
                       (assert cursor)
                       (multiple-value-bind (next-page next-cursor)
                           (agent-protocol-memory-query adapter :limit 1 :cursor cursor)
                         (assert (= 1 (length next-page)))
                         (assert (not (string= (agent-protocol-memory-item-identifier (first page))
                                               (agent-protocol-memory-item-identifier (first next-page)))))
                         (assert (null next-cursor))))
                     (assert (= 1 (length (agent-protocol-memory-query adapter :query "changed"))))
                     (agent-protocol-memory-delete adapter updated :operation-id "delete-first")
                     (agent-protocol-memory-delete adapter updated :operation-id "delete-first")
                     (assert (= 1 (length (agent-protocol-memory-query adapter))))
                     (agent-memory-test--evidence adapter context directory)
                     (let ((global (agent-protocol-memory-create
                                    adapter :title "global" :content "Global only." :scope ':global
                                    :operation-id "explicit-global")))
                       (assert (eq ':global (agent-protocol-memory-item-scope global)))
                       (assert (= 1 (length (agent-protocol-memory-query adapter :scope ':global))))
                       (assert (not (find (agent-protocol-memory-item-identifier global)
                                          (agent-protocol-memory-query adapter)
                                          :test #'equal :key #'agent-protocol-memory-item-identifier)))
                       (agent-protocol-memory-delete adapter global :scope ':global :operation-id "delete-global"))
                     (agent-protocol-output client ':diagnostic "done"))
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* directory :validate t))
    ;; Count explicit writes, including durable retries and rejected requests.
    (assert (= 23 write-count)))
  nil)

(-> agent-memory-test--authority () null)
(defun agent-memory-test--authority ()
  "Reject raw scope and evidence claims even when callers bypass local validation."
  (let* ((directory (merge-pathnames (format nil "agent-memory-policy-~A/" (make-identifier))
                                     (uiop:temporary-directory)))
         (context   (autolith-supervisor::make-memory-store-context
                     :memory-path    (merge-pathnames "memories.sexp" directory)
                     :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                     :workspace      directory))
         (service   (autolith-supervisor::make-memory-service
                     :context context :session-id "restricted")))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (dolist (extra (list (list "scope" "global")
                               (list "sources" (vector (agent-memory-test--object
                                                        "user" "A quote the user did not say.")))
                               (list "sources" (vector (agent-memory-test--object
                                                        "user" "A quote the user did not say."
                                                        "observed-at" 0)))))
             (let ((fields (apply #'agent-memory-test--object
                                  "title" "rejected" "content" "Not authorized." extra)))
               (assert (handler-case
                           (progn (autolith-supervisor::memory-service-handle
                                   service (agent-wire:make-record-request
                                            :collection ':memory :operation ':create
                                            :operation-id "rejected"
                                            :fields       (cl-protobufs.autolith.common.v1:value.map-value
                                                           (autolith-supervisor::protocol-data->value fields))))
                                  nil)
                         (autolith-supervisor::memory-record-error (condition)
                           (eq ':invalid-request (autolith-supervisor::memory-record-error-code condition)))))))
           (assert (null (memory-list context :visibility ':all))))
      (platform-delete-directory-tree *platform* directory :validate t)))
  nil)

(-> run-agent-protocol-memory-tests () integer)
(defun run-agent-protocol-memory-tests ()
  "Check the optional memory adapter over the real byte channel."
  (agent-memory-test--round-trip)
  (agent-memory-test--authority)
  2)
