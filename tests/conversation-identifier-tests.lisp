(in-package #:autolith)

;;;; -- Conversation Identifier Tests --

(-> test-conversation-identifier-format () null)
(defun test-conversation-identifier-format ()
  "Test the stored identifier format Autolith relies on for conversation files.

The pinned vectors guard the on-disk format across idsmall upgrades. A changed
scramble would orphan every stored conversation, so relaxing these cases to
match a new library version is never the correct repair."
  (let ((cases '((0  . "13VNGTr")
                 (10 . "B4JFq84")
                 (57 . "z7435Cs"))))
    (dolist (case cases)
      (test-assert
       (string= (identifier-from-seed 3994000000 (first case))
                (rest case))
       "the stored identifier format is unchanged")))
  (test-assert
   (and (conversation-identifier-migration--legacy-identifier-p
         "cb472f21-969d-48f5-9c1e-e793d19054b9")
        (not (conversation-identifier-migration--legacy-identifier-p
              "arbitrary-legacy-name")))
   "legacy migration recognizes only historical UUID identifiers")
  (test-assert (string= (conversation-identifier-display "K8vQ2mp")
                        "K-8vQ2mp")
               "stored identifiers display with one visual hyphen")
  (test-assert (string= (conversation-identifier-display "arbitrary-legacy-name")
                        "arbitrary-legacy-name")
               "a legacy identifier displays verbatim")
  (dolist (invalid '("K-8vQ2m" "K08vQ2m" 42))
    (test-assert
     (handler-case
         (progn (conversation-identifier-normalize invalid) nil)
       (conversation-identifier-error ()
         t))
     "malformed identifiers signal the Autolith condition"))
  nil)

(-> test-conversation-identifier-allocation () null)
(defun test-conversation-identifier-allocation ()
  "Test seeded allocation, storage collision probing, and structured exhaustion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (storage (configuration-conversation-root configuration))
         (timestamp 3994000000)
         (*random-index-function* (lambda (limit)
                                    (declare (ignore limit))
                                    10)))
    (unwind-protect
         (progn
           (identifier-clear-reservations)
           (let ((first (conversation-identifier-generate
                         storage :timestamp timestamp))
                 (second (conversation-identifier-generate
                          storage :timestamp timestamp)))
             (test-assert (string= first "B4JFq84")
                          "allocation begins at the seeded index")
             (test-assert (string= second (identifier-from-seed timestamp 11))
                          "allocation probes the next seed after a reservation"))
           (let* ((occupied (identifier-from-seed timestamp 12))
                  (pathname (merge-pathnames
                             (make-pathname :name occupied :type "sexp")
                             storage)))
             (ensure-directories-exist pathname)
             (with-open-file (stream pathname
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create)
               (write-string "occupied" stream))
             (test-assert
              (eq (conversation-identifier--reserved-p storage occupied) t)
              "an occupied conversation pathname reports the Boolean true"))
           (let ((reserved
                   (loop for seed below (identifier-base)
                         collect (identifier-from-seed timestamp seed))))
             (test-assert
              (handler-case
                  (progn
                    (conversation-identifier-generate
                     storage
                     :timestamp timestamp
                     :reserved-identifiers reserved)
                    nil)
                (conversation-identifier-space-exhausted (condition)
                  (= (conversation-identifier-space-exhausted-timestamp
                      condition)
                     timestamp)))
              "occupying every seed signals structured exhaustion")))
      (identifier-clear-reservations)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-identifier--legacy-conversation
    (configuration string string)
    conversation)
(defun test-conversation-identifier--legacy-conversation
    (configuration identifier content)
  "Create and persist one legacy test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation content)
    conversation))

(-> test-conversation-identifier-migration () null)
(defun test-conversation-identifier-migration ()
  "Test complete durable-reference migration, aliases, and idempotence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (old "cb472f21-969d-48f5-9c1e-e793d19054b9")
         (other "de916e8a-8227-443d-9e10-11794a62ebd6")
         (*random-index-function* (lambda (limit)
                                    (declare (ignore limit))
                                    10)))
    (unwind-protect
         (let* ((conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration old (format nil "related conversation ~A" other)))
                (other-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration other "second legacy conversation"))
                (old-path (conversation-pathname conversation))
                (old-active (conversation-log-pathname conversation))
                (old-sidecars
                  (conversation-picker-sidecar-pathnames old-path))
                (old-image-root
                  (merge-pathnames (format nil "conversation-images/~A/" old)
                                   (configuration-data-root configuration)))
                (old-task-root
                  (merge-pathnames (format nil "tasks/~A/" old)
                                   (configuration-data-root configuration)))
                (old-task-result (merge-pathnames "run/result.sexp" old-task-root))
                (crash
                  (merge-pathnames "crashes/legacy.sexp"
                                   (configuration-state-root configuration))))
           (declare (ignore other-conversation))
           (test-assert
            (string= (conversation-identifier-migration-resolve
                      configuration "fixture-name")
                     "fixture-name")
            "literal legacy identifiers remain available to internal callers")
           (dolist (invalid '("." ".." "../outside" "bad/name"
                              "bad\\name" "*" "bad?name" "bad[name" "bad]name"))
             (test-assert
              (handler-case
                  (progn
                    (conversation-pathname-for-id configuration invalid)
                    nil)
                (conversation-identifier-error ()
                  t))
              (format nil "unsafe conversation identifier ~S is rejected" invalid)))
           (test-assert
            (handler-case
                (progn
                  (conversation-create configuration :identifier "../outside")
                  nil)
              (conversation-identifier-error ()
                t))
            "explicit conversation creation rejects escaping identifiers")
           (ensure-directories-exist (merge-pathnames "image.png" old-image-root))
           (with-open-file (stream (merge-pathnames "image.png" old-image-root)
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create)
             (write-string "artifact" stream))
           (snapshot-write
            old-task-result
            (list :task-result
                  :conversation-id old
                  :conversation-path
                  (namestring
                   (merge-pathnames (format nil "run/~A.sexp" old)
                                    old-task-root))))
           (snapshot-write
            crash
            (list :crash :version 1 :id "crash" :conversation-id old))
           (memory-remember configuration
                            :title "migration memory"
                            :content "remember the migrated conversation"
                            :scope ':global
                            :tags nil
                            :source-conversation old)
           (conversation-picker-search-close conversation)
           (test-assert (every #'probe-file old-sidecars)
                        "legacy conversation setup publishes picker sidecars")
           (with-open-file (stream old-active
                                   :direction ':output
                                   :if-exists ':append
                                   :external-format ':utf-8)
             (write-string "(:interrupted" stream))
           (let* ((old-write-date (file-write-date old-active))
                  (entries (conversation-identifier-migrate configuration))
                  (entry
                    (conversation-identifier-migration--entry-for-old
                     old entries))
                  (other-entry
                    (conversation-identifier-migration--entry-for-old
                     other entries))
                  (new (getf entry :new))
                  (other-new (getf other-entry :new))
                  (new-path
                    (conversation-identifier-migration--conversation-path
                     configuration new))
                  (new-active (conversation-storage-active-pathname new-path))
                  (new-image-root
                    (merge-pathnames
                     (format nil "conversation-images/~A/" new)
                     (configuration-data-root configuration)))
                  (new-task-root
                    (merge-pathnames (format nil "tasks/~A/" new)
                                     (configuration-data-root configuration)))
                  (new-task-result
                    (merge-pathnames "run/result.sexp" new-task-root))
                  (task-record (snapshot-read new-task-result))
                  (memory (first (memory-list configuration :visibility ':all)))
                  (record
                    (snapshot-read
                     (configuration-conversation-identifier-migration-path
                      configuration))))
             (test-assert (and (identifier-p new)
                               (identifier-p other-new)
                               (not (string= new other-new)))
                          "migration assigns distinct canonical identifiers")
             (test-assert
              (and (not (conversation-storage-occupied-p old-path))
                   new-active
                   (= (file-write-date new-active) old-write-date))
              "conversation replacement preserves chunk identity and activity time")
             (test-assert (notany #'probe-file old-sidecars)
                          "migration removes picker sidecars under the legacy identifier")
             (test-assert
              (string= (conversation-identifier
                        (conversation-load-by-id
                         configuration
                         (conversation-identifier-display new)))
                       new)
              "displayed identifiers resolve to canonical stored conversations")
             (test-assert
              (string= (conversation-identifier
                        (conversation-load-by-id configuration old))
                       new)
              "legacy resume identifiers remain durable aliases")
             (test-assert
              (search other-new
                      (getf (rest (second
                                   (conversation-identifier-migration--read-forms
                                    new-active)))
                            :content))
              "cross-conversation references in durable chunks are rewritten")
             (test-assert
              (and (uiop:directory-exists-p new-image-root)
                   (not (uiop:directory-exists-p old-image-root))
                   (probe-file (merge-pathnames "image.png" new-image-root)))
              "identifier-keyed image artifacts move with the conversation")
             (test-assert
              (and (uiop:directory-exists-p new-task-root)
                   (not (uiop:directory-exists-p old-task-root))
                   (string= (getf (rest task-record) :conversation-id) new)
                   (search new (getf (rest task-record) :conversation-path))
                   (not (search old
                                (getf (rest task-record) :conversation-path))))
              "task artifacts and their internal path references migrate together")
             (test-assert
              (string= (memory-source-conversation memory) new)
              "persistent memory source references use the migrated identifier")
             (test-assert
              (string= (getf (rest (snapshot-read crash)) :conversation-id) new)
              "crash capsule references use the migrated identifier")
             (test-assert
              (and (eq (getf (rest record) :status) ':complete)
                   (= (length (getf (rest record) :entries)) 2))
              "the retained alias record marks the complete migration")
             (test-assert
              (equal entries (conversation-identifier-migrate configuration))
              "running a completed migration again is idempotent")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-identifier-migration-validation () null)
(defun test-conversation-identifier-migration-validation ()
  "Test exact target validation preserves every source before cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-old "36ef35a6-4167-4a91-858b-83a1c9d4dd89")
         (second-old "83b25dd1-9424-4be2-b149-f4457c5c9137")
         (first-new (identifier-from-seed 3994000100 1))
         (second-new (identifier-from-seed 3994000100 2))
         (entries
           (list (list :old first-old
                       :new first-new
                       :created-at 1000)
                 (list :old second-old
                       :new second-new
                       :created-at 1001))))
    (unwind-protect
         (let* ((first-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration first-old "first legacy conversation"))
                (second-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration second-old "second legacy conversation"))
                (first-source (conversation-pathname first-conversation))
                (second-source (conversation-pathname second-conversation))
                (first-target
                  (conversation-identifier-migration--conversation-path
                   configuration first-new))
                (second-target
                  (conversation-identifier-migration--conversation-path
                   configuration second-new)))
           (conversation-append-summary first-conversation "migration checkpoint")
           (conversation-identifier-migration--publish-conversations
            configuration entries)
           (let* ((older-target
                    (first (conversation-storage-pathnames first-target)))
                  (forms
                    (copy-tree
                     (conversation-identifier-migration--read-forms older-target)))
                  (foreign (copy-tree forms)))
             (setf (getf (rest (first foreign)) :id) second-new)
             (unwind-protect
                  (progn
                    (conversation-identifier-migration--write-forms
                     older-target foreign)
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-identifier-migration--validate-target-segment
                            configuration older-target first-new)
                           nil)
                       (conversation-identifier-migration-error ()
                         t))
                     "migration validates the requested older segment exactly")
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-identifier-migration--validate-target-storage
                            configuration first-target first-new)
                           nil)
                       (conversation-identifier-migration-error ()
                         t))
                     "full target validation inspects every ordered segment"))
               (conversation-identifier-migration--write-forms
                older-target forms)))
           (let* ((active-target
                    (conversation-storage-active-pathname second-target))
                  (forms
                    (copy-tree
                     (conversation-identifier-migration--read-forms active-target))))
             (conversation-identifier-migration--write-forms
              active-target (list (first forms)))
             (test-assert
              (handler-case
                  (progn
                    (conversation-identifier-migration--remove-sources
                     configuration entries)
                    nil)
                (conversation-identifier-migration-error ()
                  t))
              "cleanup rejects a header-only version-two target")
             (test-assert
              (and (conversation-storage-occupied-p first-source)
                   (conversation-storage-occupied-p second-source))
              "one invalid target preserves every legacy source before cleanup")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-identifier-migration-resumption () null)
(defun test-conversation-identifier-migration-resumption ()
  "Test restart from a durable phase after new conversations were published."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (old "88e2a524-d03c-48ea-99ad-fcfa17416f10")
         (*random-index-function* (lambda (limit)
                                    (declare (ignore limit))
                                    31)))
    (unwind-protect
         (progn
           (test-conversation-identifier--legacy-conversation
            configuration old "resume an interrupted migration")
           (let* ((legacy
                    (conversation-identifier-migration--legacy-files
                     configuration))
                  (entries
                    (multiple-value-bind (planned work-p)
                        (conversation-identifier-migration--plan
                         configuration nil legacy)
                      (test-assert work-p "legacy data creates migration work")
                      planned)))
             (conversation-identifier-migration--write
              configuration ':prepared entries)
             (conversation-identifier-migration--publish-conversations
              configuration entries)
             (conversation-identifier-migration--write
              configuration ':conversations entries)
             (let* ((completed (conversation-identifier-migrate configuration))
                    (new (getf (first completed) :new)))
                (test-assert
                 (and
                  (not
                   (conversation-storage-occupied-p
                    (conversation-identifier-migration--conversation-path
                     configuration old)))
                  (conversation-storage-active-pathname
                   (conversation-identifier-migration--conversation-path
                    configuration new))
                  (eq (getf (rest
                             (conversation-identifier-migration--read
                              configuration))
                            :status)
                      ':complete))
                 "a repeated migration safely completes every remaining phase"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-identifiers () null)
(defun test-conversation-identifiers ()
  "Test the complete human-friendly conversation identifier subsystem."
  (test-conversation-identifier-format)
  (test-conversation-identifier-allocation)
  (test-conversation-identifier-migration)
  (test-conversation-identifier-migration-validation)
  (test-conversation-identifier-migration-resumption)
  nil)
