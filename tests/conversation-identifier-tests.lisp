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

(-> test-user-identifier-allocation () null)
(defun test-user-identifier-allocation ()
  "Test short user identifiers, reservation probing, and opaque UUID allocation."
  (let ((*random-index-function* (lambda (limit)
                                   (declare (ignore limit))
                                   10))
        (occupied                nil))
    (unwind-protect
         (progn
           (identifier-clear-reservations)
           (let* ((first  (make-user-identifier
                           :namespace  "test-user"
                           :occupied-p (lambda (identifier)
                                         (member identifier occupied :test #'string=))))
                  (second (progn
                            (push first occupied)
                            (make-user-identifier
                             :namespace  "test-user"
                             :occupied-p (lambda (identifier)
                                           (member identifier occupied :test #'string=))))))
             (test-assert (and (identifier-p first)
                               (= (length first) 7)
                               (not (string= first second)))
                          "user-addressable allocation returns short collision-free IDs")
             (test-assert (and (stringp (make-identifier))
                               (= (length (make-identifier)) 36))
                          "opaque protocol allocation retains UUID format")))
      (identifier-clear-reservations)))
  nil)

(-> test-conversation-identifier-allocation () null)
(defun test-conversation-identifier-allocation ()
  "Test seeded allocation, storage collision probing, and structured exhaustion."
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (storage                 (configuration-conversation-root configuration))
         (timestamp               3994000000)
         (*random-index-function* (lambda (limit)
                                    (declare (ignore limit))
                                    10)))
    (unwind-protect
         (progn
           (identifier-clear-reservations)
           (let ((first  (conversation-identifier-generate
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
                                     :direction         ':output
                                     :if-exists         ':supersede
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
                     :timestamp            timestamp
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
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (old                     "cb472f21-969d-48f5-9c1e-e793d19054b9")
         (other                   "de916e8a-8227-443d-9e10-11794a62ebd6")
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
                (old-path        (conversation-pathname conversation))
                (old-active      (conversation-log-pathname conversation))
                (old-sidecars
                  (conversation-picker-sidecar-pathnames old-path))
                (old-image-root
                  (merge-pathnames (format nil "conversation-images/~A/" old)
                                   (config :data-root configuration)))
                (old-task-root
                  (merge-pathnames (format nil "tasks/~A/" old)
                                   (config :data-root configuration)))
                (old-task-result (merge-pathnames "run/result.sexp" old-task-root))
                (crash
                  (merge-pathnames "crashes/legacy.sexp"
                                   (config :state-root configuration))))
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
                                   :direction         ':output
                                   :if-exists         ':supersede
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
                            :title               "migration memory"
                            :content             "remember the migrated conversation"
                            :scope               ':global
                            :tags                nil
                            :source-conversation old)
           (conversation-picker-search-close conversation)
           (test-assert (every #'probe-file old-sidecars)
                        "legacy conversation setup publishes picker sidecars")
           (with-open-file (stream old-active
                                   :direction       ':output
                                   :if-exists       ':append
                                   :external-format ':utf-8)
             (write-string "(:interrupted" stream))
           (let* ((old-write-date (file-write-date old-active))
                  (entries        (conversation-identifier-migrate configuration))
                  (entry
                    (conversation-identifier-migration--entry-for-old
                     old entries))
                  (other-entry
                    (conversation-identifier-migration--entry-for-old
                     other entries))
                  (new            (getf entry :new))
                  (other-new      (getf other-entry :new))
                  (new-path
                    (conversation-identifier-migration--conversation-path
                     configuration new))
                  (new-active     (conversation-storage-active-pathname new-path))
                  (new-image-root
                    (merge-pathnames
                     (format nil "conversation-images/~A/" new)
                     (config :data-root configuration)))
                  (new-task-root
                    (merge-pathnames (format nil "tasks/~A/" new)
                                     (config :data-root configuration)))
                  (new-task-result
                    (merge-pathnames "run/result.sexp" new-task-root))
                  (task-record    (snapshot-read new-task-result))
                  (memory         (first (memory-list configuration :visibility ':all)))
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
             (test-call-with-function-replacements
              (list (list 'conversation--map-storage-records
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (error "Migrated input history must not rebuild."))))
              (lambda ()
                (test-assert
                 (equal (conversation-input-history (conversation-load new-path))
                        (list (format nil "related conversation ~A" other-new)))
                 "migration preserves the input sidecar and rewrites its references")))
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
         (root          (test-configuration-root configuration))
         (first-old     "36ef35a6-4167-4a91-858b-83a1c9d4dd89")
         (second-old    "83b25dd1-9424-4be2-b149-f4457c5c9137")
         (first-new     (identifier-from-seed 3994000100 1))
         (second-new    (identifier-from-seed 3994000100 2))
         (entries
           (list (list :old        first-old
                       :new        first-new
                       :created-at 1000)
                 (list :old        second-old
                       :new        second-new
                       :created-at 1001))))
    (unwind-protect
         (let* ((first-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration first-old "first legacy conversation"))
                (second-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration second-old "second legacy conversation"))
                (first-source  (conversation-pathname first-conversation))
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
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (old                     "88e2a524-d03c-48ea-99ad-fcfa17416f10")
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
                    (new       (getf (first completed) :new)))
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


;;;; -- Conversation Cache Relocation --

(-> test-conversation-cache--with-legacy-paths (function) t)
(defun test-conversation-cache--with-legacy-paths (function)
  "Call FUNCTION with the complete pre-relocation cache pathname protocol."
  (test-call-with-function-replacements
   (list
    (list 'conversation-picker-metadata-pathname
          #'conversation-sidecar-data-pathname)
    (list 'conversation-picker-search-pathname
          (lambda (pathname)
            (second (conversation-cache-legacy-pathnames pathname))))
    (list 'conversation-input-history-pathname
          (lambda (pathname)
            (third (conversation-cache-legacy-pathnames pathname)))))
   function))

(-> test-conversation-cache-relocation () null)
(defun test-conversation-cache-relocation ()
  "Reconstruct relocated indexes from archived records without touching anchors or sources."
  (with-test-configuration (configuration)
    (let ((conversation nil))
      (test-conversation-cache--with-legacy-paths
       (lambda ()
         (setf conversation (conversation-create configuration))
         (conversation-append-user-message conversation "archived input")
         (conversation-append-summary conversation "checkpoint")
         (conversation-append-user-message conversation "active input")
         (conversation-append-user-operation
          conversation :kind ':lisp :source "(+ 1 2)" :status ':ok :result "3")
         (conversation-picker-search-find (conversation-pathname conversation))
         (conversation-input-history conversation)))
      (let* ((identity (conversation-pathname conversation))
             (anchor   (conversation-context-anchors-pathname identity))
             (sources  (conversation-storage-pathnames identity))
             (contents (mapcar #'uiop:read-file-string sources))
             (legacy   (conversation-cache-legacy-pathnames identity))
             (old-data (mapcar #'uiop:read-file-string legacy)))
        (with-open-file (stream anchor :direction ':output :if-exists ':supersede
                                       :if-does-not-exist ':create)
          (write-string "(:authoritative-context-anchor)" stream))
        (test-assert
         (notany #'probe-file (conversation-picker-sidecar-pathnames identity))
         "an old installation has no new cache files")
        (test-assert (member identity (conversation-list configuration) :test #'equal)
                     "conversation discovery uses authoritative data, not cache files")
        (test-assert (conversation-picker-metadata-find identity)
                     "picker metadata reconstructs in the new cache root")
        (test-assert
         (equal (conversation-picker-search-index-messages
                 (conversation-picker-search-find identity))
                '("archived input" "active input"))
         "search reconstructs complete retired and active conversation text")
        (test-assert
         (equal (conversation-input-history (conversation-load identity))
                '("archived input" "active input" "(+ 1 2)"))
         "input recall reconstructs user messages and operations across rotation")
        (test-assert
         (every (lambda (pathname)
                  (uiop:subpathp pathname (config :cache-root configuration)))
                (conversation-picker-sidecar-pathnames identity))
         "all regenerable index payloads use the independent cache root")
        (test-assert
         (equal anchor
                (merge-pathnames
                 (make-pathname :name (format nil "~A.anchors" (pathname-name identity))
                                :type "sexp")
                 (merge-pathnames "conversation-picker/" (config :data-root configuration))))
         "anchors retain their exact historical data location")
        (test-assert
         (equal (conversation-picker-source-lock-pathname identity)
                (make-pathname :type     "lock"
                               :defaults (conversation-sidecar-data-pathname identity)))
         "old and new releases retain the identical source lock")
        (test-assert
         (and (equal (conversation-picker-revision-pathname identity)
                     (make-pathname :type     "revision"
                                    :defaults (conversation-sidecar-data-pathname identity)))
              (equal (conversation-picker-search-revision-pathname identity)
                     (make-pathname :name     (format nil "~A.search" (pathname-name identity))
                                    :type     "revision"
                                    :defaults (conversation-sidecar-data-pathname identity))))
         "both durable source revisions retain their historical DATA paths")
        (test-assert
         (and (equal contents (mapcar #'uiop:read-file-string sources))
              (equal old-data (mapcar #'uiop:read-file-string legacy))
              (string= "(:authoritative-context-anchor)" (uiop:read-file-string anchor)))
         "reconstruction preserves every source, old cache file and authoritative anchor")
        (dolist (pathname (conversation-picker-sidecar-pathnames identity))
          (when (probe-file pathname)
            (delete-file pathname)))
        (test-assert
         (and (conversation-picker-search-find identity)
              (equal (conversation-input-history (conversation-load identity))
                     '("archived input" "active input" "(+ 1 2)")))
         "clearing all new caches permits complete reconstruction again"))))
  nil)

(-> test-conversation-cache-namespaces () null)
(defun test-conversation-cache-namespaces ()
  "Keep identical identifiers in independently configured and custom roots distinct."
  (with-test-configuration (configuration root)
    (let* ((identifier  "K8vQ2mp")
           (first       (conversation-create configuration :identifier identifier))
           (custom      (merge-pathnames "archived/conversations/" root))
           (second      (conversation-create configuration :identifier  identifier
                                                          :storage-root custom))
           (other       (configuration-copy configuration))
           (other-data  (merge-pathnames "other-data/" root))
           (other-cache (merge-pathnames "independent-cache/" root)))
      (setf (config :data-root other) other-data
            (config :cache-root other) other-cache)
      (let ((third (conversation-create other :identifier identifier)))
        (loop for conversation in (list first second third)
              for input in '("first" "second" "third")
              do (conversation-append-user-message conversation input))
        (test-assert
         (= 3 (length (remove-duplicates
                       (mapcar (lambda (conversation)
                                 (conversation-picker-metadata-pathname
                                  (conversation-pathname conversation)))
                               (list first second third))
                       :test #'equal)))
         "complete source-directory namespaces isolate repeated identifiers")
        (test-assert
         (uiop:subpathp (conversation-picker-metadata-pathname (conversation-pathname third))
                       other-cache)
         "data and cache configuration roots are independent")
        (loop for conversation in (list first second third)
              for input in '("first" "second" "third")
              do (test-assert
                  (and (equal (conversation-input-history conversation) (list input))
                       (equal (conversation-picker-search-index-messages
                               (conversation-picker-search-find
                                (conversation-pathname conversation)))
                              (list input)))
                  "equal identifiers never share another source's search or input index")))))
  nil)

(-> test-conversation-cache-old-writer () null)
(defun test-conversation-cache-old-writer ()
  "Honor old-release invalidation under the common source lock and survive publication failure."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity     (conversation-pathname conversation)))
      (conversation-append-user-message conversation "before")
      (conversation-picker-search-find identity)
      (let ((stamp (multiple-value-list (conversation--file-identity identity))))
        (call-with-file-lock
         (conversation-picker-source-lock-pathname identity)
         (lambda ()
           (sexp-store:revision-write
            (conversation-picker-revision-pathname identity)
            (1+ (conversation-picker-revision-read identity))
            :tag ':conversation-picker-revision)
           (sexp-store:revision-write
            (conversation-picker-search-revision-pathname identity)
            (1+ (conversation-picker-search-revision-read identity))
            :tag ':conversation-picker-search-revision)))
        (test-assert
         (and (equal stamp (multiple-value-list (conversation--file-identity identity)))
              (null (conversation-picker-metadata-read identity))
              (null (conversation-picker-search-read identity)))
         "old revision publication invalidates new caches even with identical source size and time"))
      (test-conversation-cache--with-legacy-paths
       (lambda ()
         (let ((old (conversation-load identity)))
           (conversation-append-user-message old "old writer")
           (conversation-picker-search-close old))))
      (test-assert
       (and (equal (conversation-picker-search-index-messages
                    (conversation-picker-search-find identity))
                   '("before" "old writer"))
            (equal (conversation-input-history (conversation-load identity))
                   '("before" "old writer")))
       "new releases observe old-release appends and rebuild complete caches")
      (let ((new (conversation-load identity)))
        (conversation-append-user-message new "new writer"))
      (test-conversation-cache--with-legacy-paths
       (lambda ()
         (test-assert
          (and (equal (conversation-picker-search-index-messages
                       (conversation-picker-search-find identity))
                      '("before" "old writer" "new writer"))
               (equal (conversation-input-history (conversation-load identity))
                      '("before" "old writer" "new writer")))
          "legacy readers observe new-release invalidation and retain all inputs")))
      (let ((source-before
              (mapcar #'uiop:read-file-string (conversation-storage-pathnames identity)))
            (revision-write (symbol-function 'sexp-store:revision-write))
            (new            (conversation-load identity)))
        (test-call-with-function-replacements
         (list (list 'sexp-store:revision-write
                     (lambda (pathname revision &rest options)
                       (when (equal pathname
                                    (conversation-picker-revision-pathname identity))
                         (error "Simulated DATA revision publication failure."))
                       (apply revision-write pathname revision options))))
         (lambda ()
           (test-assert
            (handler-case
                (progn (conversation-append-user-message new "not written") nil)
              (conversation-invariant-error () t))
            "failed DATA invalidation prevents durable source mutation")))
        (test-assert
         (equal source-before
                (mapcar #'uiop:read-file-string (conversation-storage-pathnames identity)))
         "publication failure loses and appends no authoritative records"))))
  nil)

(-> test-conversation-cache-unavailable () null)
(defun test-conversation-cache-unavailable ()
  "Keep upgrade reads, writes and rotation available without generated cache storage."
  (dolist (existing-cache-p '(nil t))
    (with-test-configuration (configuration)
      (let ((conversation nil))
        (test-conversation-cache--with-legacy-paths
         (lambda ()
           (setf conversation (conversation-create configuration))
           (conversation-append-user-message conversation "archived")
           (conversation-append-summary conversation "checkpoint")
           (conversation-append-user-message conversation "active")
           (conversation-picker-search-close conversation)))
        (let* ((identity (conversation-pathname conversation))
               (anchor   (conversation-context-anchors-pathname identity))
               (lock     (conversation-picker-source-lock-pathname identity))
               (cache    (merge-pathnames "conversation-picker/"
                                          (config :cache-root configuration)))
               (saved    (merge-pathnames "conversation-picker.saved/"
                                          (config :cache-root configuration)))
               (blocker  (merge-pathnames "conversation-picker"
                                          (config :cache-root configuration))))
          (with-open-file (stream anchor :direction ':output :if-exists ':supersede
                                         :if-does-not-exist ':create)
            (write-string "(:authoritative-context-anchor)" stream))
          (when existing-cache-p
            (conversation-picker-search-find identity)
            (conversation-input-history (conversation-load identity))
            (rename-file cache saved))
          (ensure-directories-exist blocker)
          (with-open-file (stream blocker :direction ':output :if-does-not-exist ':create)
            (write-string "Cache storage unavailable." stream))
          (unwind-protect
               (progn
                 (let ((sources (mapcar #'uiop:read-file-string
                                        (conversation-storage-pathnames identity))))
                   (test-assert
                    (and (conversation-picker-metadata-find identity)
                         (equal (conversation-picker-search-index-messages
                                 (conversation-picker-search-find identity))
                                '("archived" "active"))
                         (equal (conversation-input-history (conversation-load identity))
                                '("archived" "active")))
                    "picker, search and input recall reconstruct without writable cache")
                   (test-assert
                    (equal sources (mapcar #'uiop:read-file-string
                                           (conversation-storage-pathnames identity)))
                    "cache fallback reads preserve every authoritative source byte"))
                 (let ((new (conversation-load identity)))
                   (conversation-append-user-message new "during outage")
                   (conversation-append-summary new "outage checkpoint")
                   (conversation-append-user-message new "after rotation")
                   (test-assert
                    (equal (conversation-input-history new)
                           '("archived" "active" "during outage" "after rotation"))
                    "rotation retains complete editable history while cache publication fails")
                   (test-assert
                    (block found
                      (conversation-map-input-history
                       new (lambda (input)
                             (when (string= input "active")
                               (return-from found t))))
                      nil)
                    "fallback history streaming permits early exit"))
                 (test-conversation-cache--with-legacy-paths
                  (lambda ()
                    (let ((old (conversation-load identity)))
                      (conversation-append-user-message old "old writer"))))
                 (test-assert
                  (equal (conversation-picker-search-index-messages
                          (conversation-picker-search-find identity))
                         '("archived" "active" "during outage" "after rotation" "old writer"))
                  "new readers include mixed old and new writes during a cache outage"))
            (delete-file blocker)
            (when existing-cache-p
              (rename-file saved cache)))
          (test-assert
           (and (null (conversation-picker-metadata-read identity))
                (null (conversation-picker-search-read identity)))
           "restored pre-outage caches are rejected rather than returned stale")
          (let ((sources (mapcar #'uiop:read-file-string
                                 (conversation-storage-pathnames identity))))
            (test-assert
             (and (equal (conversation-picker-search-index-messages
                          (conversation-picker-search-find identity))
                         '("archived" "active" "during outage" "after rotation" "old writer"))
                  (equal (conversation-input-history (conversation-load identity))
                         '("archived" "active" "during outage" "after rotation" "old writer"))
                  (conversation-picker-metadata-read identity)
                  (conversation-picker-search-read identity))
             "restored cache storage supports complete reconstruction and persistent reuse")
            (test-assert
             (and (equal sources (mapcar #'uiop:read-file-string
                                        (conversation-storage-pathnames identity)))
                  (string= "(:authoritative-context-anchor)" (uiop:read-file-string anchor))
                  (probe-file lock)
                  (equal lock (conversation-picker-source-lock-pathname identity)))
             "reconstruction preserves source bytes, anchor bytes and persistent DATA lock"))))))
  nil)

(-> test-conversation-cache-write-failures () null)
(defun test-conversation-cache-write-failures ()
  "Reject readable stale payloads while read-only or full cache storage refuses writes."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity     (conversation-pathname conversation))
           (cache-root   (config :cache-root configuration))
           (failures     0))
      (conversation-append-user-message conversation "before")
      (conversation-picker-search-find identity)
      (conversation-input-history conversation)
      (let ((payloads (mapcar #'uiop:read-file-string
                              (conversation-picker-sidecar-pathnames identity))))
        (flet ((refuse-cache-write (name)
                 (let ((original (symbol-function name)))
                   (list name
                         (lambda (pathname &rest arguments)
                           (when (uiop:subpathp pathname cache-root)
                             (incf failures)
                             (error "Simulated read-only or full cache storage."))
                           (apply original pathname arguments))))))
          (test-call-with-function-replacements
           (mapcar #'refuse-cache-write '(snapshot-write log-write-streaming log-append))
           (lambda ()
             (conversation-append-user-message conversation "during failure")
             (test-assert
              (and (null (conversation-picker-metadata-read identity))
                   (null (conversation-picker-search-read identity))
                   (equal payloads
                          (mapcar #'uiop:read-file-string
                                  (conversation-picker-sidecar-pathnames identity))))
              "writable DATA invalidates unchanged, still-readable CACHE payloads")
             (test-assert
              (and (conversation-picker-metadata-find identity)
                   (equal (conversation-picker-search-index-messages
                           (conversation-picker-search-find identity))
                          '("before" "during failure"))
                   (equal (conversation-input-history conversation)
                          '("before" "during failure")))
              "failed payload publication returns fresh scans, never readable stale cache")
             (test-assert
              (handler-case
                  (progn
                    (conversation-map-input-history
                     conversation
                     (lambda (input)
                       (declare (ignore input))
                       (error 'simple-error :format-control "Consumer failure.")))
                    nil)
                (simple-error () t))
              "fallback history callbacks propagate their own errors")
             (conversation-append-summary conversation "failure checkpoint")
             (conversation-append-user-message conversation "after rotation")
             (test-assert
              (equal (conversation-input-history (conversation-load identity))
                     '("before" "during failure" "after rotation"))
              "readable stale history does not hide inputs retired during cache failure"))))
        (test-assert (plusp failures) "the cache fault injection exercised publication failures")
        (test-assert
         (and (equal (conversation-picker-search-index-messages
                      (conversation-picker-search-find identity))
                     '("before" "during failure" "after rotation"))
              (equal (conversation-input-history (conversation-load identity))
                     '("before" "during failure" "after rotation")))
         "restored writable cache storage rebuilds complete picker search and input indexes"))))
  nil)
