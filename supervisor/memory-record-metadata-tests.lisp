(in-package #:autolith-supervisor)

;;;; -- Public Memory Metadata Checks --

(defun run-memory-record-metadata-tests (&key (standalone-p t))
  "Check public metadata, live evidence, premise dependencies, and source-root policy."
  (let* ((checks    0)
         (root      (merge-pathnames (format nil "memory-metadata-~A/" (memory--make-identifier))
                                    (uiop:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (private   (merge-pathnames "private/" root))
         (source    (merge-pathnames "source.txt" workspace))
         (context   (make-memory-store-context
                     :memory-path    (merge-pathnames "memories.sexp" private)
                     :knowledge-path (merge-pathnames "knowledge.sexp" private)
                     :workspace      workspace)))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Memory metadata check ~D failed." checks)))

             (write-source (text)
               (with-open-file (stream source :direction ':output :if-exists ':supersede
                                              :if-does-not-exist ':create)
                 (write-string text stream)))

             (metadata (memory)
               (memory-service-record-metadata context memory))

             (status (metadata)
               (gethash "status" (gethash "assessment" metadata)))

             (portable-p (value)
               (typecase value
                 (boolean
                  t)
                 (hash-table
                  (and (<= (hash-table-count value) 8)
                       (loop for key being the hash-keys of value using (hash-value item)
                             always (and (stringp key) (portable-p item)))))
                 (string
                  (not (search (namestring private) value)))
                 (integer
                  t)
                 (vector
                  (every #'portable-p value))
                 (t
                  nil))))
      (unwind-protect
           (progn
             (when standalone-p (check (null (find-package '#:autolith))))
             (ensure-directories-exist workspace)
             (ensure-directories-exist private)
             (write-source "first evidence")
             (let* ((*memory-store-context* context)
                    (observed               (memory-source-observe-file source :time (get-universal-time)))
                    (memory                 (memory-remember context :title "Observed" :content "Evidence claim."
                                                                     :source-conversation "public-session"
                                                                     :sources             (list observed)))
                    (fresh                  (metadata memory)))
               (check (equal "current" (status fresh)))
               (check (= (memory-created-at memory) (gethash "created_at" fresh)))
               (check (= (memory-updated-at memory) (gethash "updated_at" fresh)))
               (check (equal "public-session" (gethash "source_conversation" fresh)))
               (check (portable-p fresh))
               (let* ((assessment (gethash "assessment" fresh))
                      (outcomes   (gethash "source_outcomes" assessment)))
                 (check (= 1 (length outcomes)))
                 (check (equal "holds" (gethash "outcome" (aref outcomes 0)))))
               (let* ((dependent (memory-remember context :title "Derived" :content "Dependent claim."
                                                          :premises (list (memory-identifier memory))))
                      (derived   (metadata dependent))
                      (outcomes  (gethash "premise_outcomes" (gethash "assessment" derived))))
                 (check (equal "current" (status derived)))
                 (check (equal (memory-identifier memory) (gethash "id" (aref outcomes 0))))
                 (check (equal "Observed" (gethash "title" (aref outcomes 0))))
                 (check (equalp (vector (memory-identifier dependent))
                                (gethash "dependents" (metadata memory))))
                 (check (portable-p derived))
                 (write-source "changed evidence")
                 (let* ((stale      (metadata memory))
                        (assessment (gethash "assessment" stale))
                        (outcome    (aref (gethash "source_outcomes" assessment) 0)))
                   (check (equal "stale" (status stale)))
                   (check (plusp (length (gethash "summary" assessment))))
                   (check (equal "file-changed" (gethash "kind" (gethash "reason" outcome))))
                   (check (equal "stale" (status (metadata dependent))))))
               (let* ((restricted (make-memory-store-context
                                   :memory-path    (memory-context-memory-path context)
                                   :knowledge-path (memory-context-knowledge-path context)
                                   :workspace workspace :source-roots (list private)))
                      (unchecked  (memory-service-record-metadata restricted memory)))
                 (check (equal "unverified" (status unchecked)))
                 (check (portable-p unchecked)))
               (let* ((bare (memory-remember context :title "Bare" :content "No evidence."))
                      (data (metadata bare)))
                 ;; The shared assessor treats an evidence-free memory as current.
                 (check (equal "current" (status data)))
                 (check (not (nth-value 1 (gethash "source_conversation" data))))
                 (check (zerop (length (gethash "source_outcomes" (gethash "assessment" data)))))
                 (check (portable-p data)))
               (check (equalp fresh (protocol-value->data (protocol-data->value fresh))))))
        (dolist (directory (list workspace private))
          (dolist (file (uiop:directory-files directory)) (delete-file file))
          (when (probe-file directory) (uiop:delete-empty-directory directory)))
        (when (probe-file root) (uiop:delete-empty-directory root))))
    checks))
