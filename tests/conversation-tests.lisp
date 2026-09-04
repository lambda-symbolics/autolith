(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-conversation--write-tiny-png (pathname) pathname)
(defun test-conversation--write-tiny-png (pathname)
  "Write the valid one-pixel test PNG to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence
     (base64-string-to-usb8-array *test-conversation-tiny-png*)
     stream))
  pathname)


(-> test-conversation--all-records (conversation) list)
(defun test-conversation--all-records (conversation)
  "Return every durable record across CONVERSATION's ordered storage segments."
  (let ((records nil))
    (conversation-map-records
     conversation
     (lambda (record)
       (push record records)))
    (nreverse records)))

(-> test-conversation-image-input () null)
(defun test-conversation-image-input ()
  "Test image validation, durable artifacts, projection, and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (merge-pathnames "source image.png" root)))
    (unwind-protect
         (progn
           (test-assert
            (and (string= (user-message-input-text "plain input") "plain input")
                 (null (user-message-input-image-pathnames "plain input"))
                 (not (eq (user-message-input-copy "plain input") "plain input")))
            "legacy strings satisfy the user input protocol")
           (test-conversation--write-tiny-png source)
           (test-assert
            (equal (image-input-recognize-pasted-path
                    (format nil "'~A'" (namestring source)))
                   (truename source))
            "quoted pasted image paths are recognized by file content")
           (let* ((conversation
                    (conversation-create configuration :identifier "images"))
                  (input
                    (user-message-input-create
                     :text "Describe [Image #1]."
                     :image-pathnames (list (truename source))))
                  (input-copy (user-message-input-copy input))
                  (item (conversation-append-user-message conversation input))
                  (content (json-get item "content"))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record (second records))
                  (descriptor (first (getf (rest record) :images)))
                  (artifact
                    (merge-pathnames
                     (getf descriptor :artifact)
                     (conversation-image-artifact-root conversation))))
             (test-assert
              (and (not (eq input-copy input))
                   (not (eq (user-message-input-text input-copy) (user-message-input-text input)))
                   (not (eq (user-message-input-image-pathnames input-copy)
                            (user-message-input-image-pathnames input)))
                   (string= (user-message-input-text input-copy)
                            (user-message-input-text input))
                   (equal (user-message-input-image-pathnames input-copy)
                          (user-message-input-image-pathnames input)))
              "rich user input copies preserve text and image attachments")
             (test-assert
              (handler-case (progn (conversation-append-user-message conversation "") nil)
                (configuration-error () t))
              "empty legacy user input remains invalid")
             (test-assert (= (length content) 4)
                          "one image contributes tags, image data, and user text")
             (test-assert
              (and (string= (json-get (aref content 0) "type") "input_text")
                   (search "<image name=[Image #1]"
                           (json-get (aref content 0) "text"))
                   (string= (json-get (aref content 1) "type") "input_image")
                   (uiop:string-prefix-p
                    "data:image/png;base64,"
                    (json-get (aref content 1) "image_url"))
                   (string= (json-get (aref content 1) "detail") "high")
                   (string= (json-get (aref content 2) "text") "</image>")
                   (string= (json-get (aref content 3) "text")
                            "Describe [Image #1]."))
              "image messages use the current Codex Responses wire shape")
             (test-assert (probe-file artifact)
                          "conversation images are copied into private artifacts")
             (test-assert
              (not (search "data:image"
                           (with-output-to-string (stream)
                             (prin1 records stream))))
              "conversation records never inline image bytes")
             (let* ((call
                      (json-object
                       "type" "function_call"
                       "call_id" "view-1"
                       "namespace" "fs"
                       "name" "view-image"
                       "arguments"
                       (json-encode
                        (json-object "path" (namestring source)))))
                    (tool-attachment
                      (image-input-prepare
                       source
                       (conversation-image-artifact-root conversation)))
                    (tool-item nil))
               (conversation-append-provider-item conversation call)
               (setf tool-item
                     (conversation-append-tool-result
                      conversation
                      "view-1"
                      :tool-name "fs.view-image"
                      :output "Viewed the image."
                      :content-blocks
                      (list "Before image."
                            tool-attachment
                            "After image.")
                      :success-p t))
               (let* ((tool-output (json-get tool-item "output"))
                      (durable-records
                        (conversation--read-records
                         (conversation-pathname conversation)))
                      (tool-record
                        (find :tool-result durable-records :key #'first)))
                 (test-assert
                  (and (vectorp tool-output)
                       (= (length tool-output) 3)
                       (string= (json-get (aref tool-output 0) "type")
                                "input_text")
                       (string= (json-get (aref tool-output 0) "text")
                                "Before image.")
                       (string= (json-get (aref tool-output 1) "type")
                                "input_image")
                       (uiop:string-prefix-p
                        "data:image/png;base64,"
                        (json-get (aref tool-output 1) "image_url"))
                       (string= (json-get (aref tool-output 2) "type")
                                "input_text")
                       (string= (json-get (aref tool-output 2) "text")
                                "After image."))
                  "image tools preserve exact multimodal content order")
                 (test-assert
                  (and (getf (rest tool-record) :content-blocks)
                       (null (getf (rest tool-record) :wire-json))
                       (not (search
                             "data:image"
                             (with-output-to-string (stream)
                               (prin1 tool-record stream)))))
                  "image tool results persist descriptors instead of base64"))
             (let* ((loaded
                      (conversation-load-by-id configuration "images"))
                    (loaded-content
                      (json-get (first (conversation-input-items loaded))
                                "content"))
                    (loaded-tool-output
                      (json-get (third (conversation-input-items loaded))
                                "output")))
               (test-assert
                (string= (json-get (aref loaded-content 1) "image_url")
                         (json-get (aref content 1) "image_url"))
                "conversation replay reconstructs the exact user image")
               (test-assert
                (and (vectorp loaded-tool-output)
                     (string=
                      (json-get (aref loaded-tool-output 0) "text")
                      "Before image.")
                     (string=
                      (json-get (aref loaded-tool-output 1) "image_url")
                      (json-get
                       (aref (json-get tool-item "output") 1)
                       "image_url"))
                     (string=
                      (json-get (aref loaded-tool-output 2) "text")
                      "After image."))
                "conversation replay reconstructs ordered image tool output")))
             (delete-file artifact)
             (test-assert
              (handler-case
                  (progn
                    (conversation-load-by-id configuration "images")
                    nil)
                (image-input-error (condition)
                  (eq (image-input-error-stage condition) ':loading)))
              "conversation replay rejects a missing image artifact")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-compaction () null)
(defun test-conversation-compaction ()
  "Test summary records, projection replacement, and usage tracking."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "compact")))
           (conversation-append-user-message conversation "first question")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "one"
                  :usage '(("total_tokens" 4321))))
           (test-assert (= (conversation-last-total-tokens conversation) 4321)
                        "usage totals track the newest provider step")
           (conversation-append-summary conversation
                                        "summary of the earlier work")
           (test-assert (= (length (conversation-input-items conversation)) 1)
                        "compaction replaces the projection with one bridge")
           (test-assert (zerop (conversation-last-total-tokens conversation))
                        "compaction resets the tracked usage")
           (test-assert (null (conversation-turn-state conversation))
                        "compaction drops the provider turn state")
           (conversation-append-user-message conversation "later question")
           (let* ((reloaded (conversation-load-by-id configuration "compact"))
                  (items (conversation-input-items reloaded))
                  (bridge-text (json-get
                                (aref (json-get (first items) "content") 0)
                                "text")))
             (test-assert (= (length items) 2)
                          "replay reproduces the compacted projection")
             (test-assert (search "summary of the earlier work" bridge-text)
                          "the bridge item carries the summary")
             (test-assert (search "compacted" bridge-text)
                          "the bridge item explains its provenance")
             (test-assert (zerop (conversation-last-total-tokens reloaded))
                          "replay resets usage tracked before the summary")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-native-compaction () null)
(defun test-conversation-native-compaction ()
  "Test durable opaque compaction with a portable cross-family handoff."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "native-compact"))
                (item
                  (json-object "type" "context_compaction"
                               "encrypted_content" "codex-checkpoint")))
           (conversation-append-user-message conversation "earlier context")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "before-native-compact"
                  :usage '(("total_tokens" 4321))))
           (conversation-append-native-compaction
            conversation item :family ':codex :summary "Portable handoff.")
           (test-assert
            (and (= (length (conversation-input-items conversation)) 2)
                 (native-compaction-item-p
                  (first (conversation-input-items conversation))))
            "native compaction retains one opaque checkpoint beside its handoff")
           (test-assert
            (string= (json-get (first (conversation-input-items conversation))
                               "type")
                     "context_compaction")
            "native compaction preserves Codex's current checkpoint encoding")
           (test-assert (zerop (conversation-last-total-tokens conversation))
                        "native compaction resets the tracked usage")
            (test-assert
             (= (length (conversation-input-items-for-family conversation ':codex)) 1)
             "the producing family receives only its opaque checkpoint")
           (let ((grok-items
                   (conversation-input-items-for-family conversation ':grok)))
             (test-assert (= (length grok-items) 1)
                          "another family omits the opaque checkpoint")
             (test-assert
              (search "Portable handoff."
                      (json-get
                       (aref (json-get (first grok-items) "content") 0)
                       "text"))
              "another family receives the portable handoff"))
           (test-assert
            (find :native-compaction
                  (rest (conversation--read-records
                         (conversation-pathname conversation)))
                  :key #'first)
            "native compaction persists one durable checkpoint record")
           (let ((reloaded
                   (conversation-load-by-id configuration "native-compact")))
              (test-assert
               (and (= (length (conversation-input-items-for-family reloaded ':codex))
                       1)
                    (= (length (conversation-input-items-for-family reloaded ':grok))
                       1))
               "native checkpoint replay preserves each family projection")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-conversation-chunk-storage () null)
(defun test-conversation-chunk-storage ()
  "Test deterministic compaction chunks and self-contained newest-chunk resume."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "7Hk2mNp"))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier identifier))
                (identity (conversation-pathname conversation))
                (initial-chunk (conversation-log-pathname conversation)))
           (test-assert
            (and (equal initial-chunk (conversation-chunk-pathname identity 1))
                 (string=
                  (pathname-name initial-chunk)
                  (format nil "~V,'0D" *conversation-chunk-sequence-width* 1))
                 (not (probe-file identity)))
            "new conversations target a deterministic sequence-one chunk")
           (conversation-append-user-message
            conversation
            "before compaction"
            :pending-input-identifier "pending-before-compaction")
           (test-assert
            (equal (conversation-storage-pathnames identity) (list initial-chunk))
            "first persistence publishes one deterministic chunk")
           (conversation-set-model-selection conversation "gpt-5.6-luna" "high")
           (conversation-append-user-operation
            conversation
            :kind ':lisp
            :source "(values :before-compaction)"
            :status ':ok
            :result "⇒ :BEFORE-COMPACTION")
           (conversation-append-record
            conversation
            (list :goal
                  :objective "Finish chunk storage."
                  :status ':active
                  :continuations 2
                  :created-at (get-universal-time)))
           (setf (conversation-working-seconds conversation) 37
                 (conversation-last-activity-at conversation) (get-universal-time))
           (let* ((summary
                    (conversation-append-summary conversation "Portable checkpoint."))
                  (summary-sequence (getf (rest summary) :seq))
                  (pathnames (conversation-storage-pathnames identity))
                  (active (conversation-log-pathname conversation))
                  (forms (conversation--read-records active))
                  (expected-working-seconds
                    (conversation-working-seconds conversation))
                  (expected-last-activity
                    (conversation-last-activity-at conversation)))
             (test-assert
              (and (= (length pathnames) 2)
                   (equal (first pathnames) initial-chunk)
                   (equal active (second pathnames))
                   (= (conversation-chunk-start-sequence active) summary-sequence)
                   (string=
                    (pathname-name active)
                    (format nil "~V,'0D"
                            *conversation-chunk-sequence-width*
                            summary-sequence))
                   (= (conversation-log-generation conversation) 1))
              "one compaction publishes exactly one sequence-named active chunk")
             (test-assert
              (and (= (length forms) 2)
                   (eq (first (first forms)) ':conversation)
                   (= (getf (rest (first forms)) :version) 2)
                   (= (getf (rest (first forms)) :chunk-start-sequence)
                      summary-sequence)
                   (eq (first (second forms)) ':summary)
                   (= (getf (rest (second forms)) :seq) summary-sequence))
              "a compacted chunk atomically begins with its header and checkpoint")
             (let ((records (test-conversation--all-records conversation)))
               (test-assert
                (equal (mapcar (lambda (record) (getf (rest record) :seq)) records)
                       (loop for sequence from 1 to summary-sequence collect sequence))
                "cross-chunk enumeration skips headers and preserves durable order"))
             (let ((loaded (conversation-load-by-id configuration identifier)))
               (test-assert
                (and (equal (conversation-log-pathname loaded) active)
                     (= (conversation-next-sequence loaded) (1+ summary-sequence))
                     (= (length (conversation-input-items loaded)) 1)
                     (string= (conversation-model loaded) "gpt-5.6-luna")
                     (string= (conversation-reasoning-effort loaded) "high")
                     (= (conversation-working-seconds loaded)
                        expected-working-seconds)
                     (= (conversation-user-turn-count loaded) 1)
                     (= (conversation-last-activity-at loaded)
                        expected-last-activity)
                     (equal (conversation-pending-input-identifiers loaded)
                            '("pending-before-compaction"))
                     (= (length (conversation-user-operation-snapshot loaded)) 1)
                     (string=
                      (getf (rest (conversation-latest-goal-record loaded))
                            :objective)
                      "Finish chunk storage.")
                     (= (conversation-picker-search-message-count loaded) 1)
                     (string= (conversation-picker-preview loaded)
                              "before compaction"))
                "the newest chunk restores all cumulative resumable state alone")
               (with-open-file (stream initial-chunk
                                       :direction ':output
                                       :if-exists ':append
                                       :external-format ':utf-8)
                 (write-string "(:interrupted" stream))
               (let ((search-pathname
                       (conversation-picker-search-pathname identity)))
                 (when (probe-file search-pathname)
                   (delete-file search-pathname)))
               (conversation-append-user-message loaded "after compaction")
               (test-assert
                (equal
                 (conversation-picker-search-index-messages
                  (conversation-picker-search-find identity))
                 '("before compaction" "after compaction"))
                "a search after a compacted append rebuilds cross-chunk text"))
              (let* ((loaded (conversation-load-by-id configuration identifier))
                     (log-append-function (symbol-function 'log-append))
                     (failure-injected-p nil)
                     (expected-sequence
                       (conversation-next-sequence loaded)))
                (let ((record
                        (test-call-with-function-replacements
                         (list
                          (list
                           'log-append
                           (lambda (&rest arguments)
                             (multiple-value-prog1
                                 (apply log-append-function arguments)
                               (unless failure-injected-p
                                 (setf failure-injected-p t)
                                 (error "simulated failure after chunk publication"))))))
                         (lambda ()
                           (conversation-append-summary
                            loaded "post-publication checkpoint")))))
                  (test-assert
                   (and (= (getf (rest record) :seq) expected-sequence)
                        (= (conversation-next-sequence loaded)
                           (1+ expected-sequence))
                        (= (conversation-chunk-start-sequence
                            (conversation-log-pathname loaded))
                           expected-sequence))
                   "a complete chunk publication survives a post-publication failure")))
             (test-assert
              (and (find identity (conversation-list configuration) :test #'equal)
                   (plusp (conversation-storage-write-date identity)))
              "chunk directories remain visible through stable picker identities")
             (conversation-delete configuration identifier)
             (test-assert
              (and (not (conversation-storage-occupied-p identity))
                   (not (find identity (conversation-list configuration)
                              :test #'equal)))
              "conversation deletion removes deterministic chunks behind the identity")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-conversation-segment-validation () null)
(defun test-conversation-segment-validation ()
  "Test exact chunk paths, headers, checkpoints, and sequences are validated."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "5Vk8sQr"))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier identifier))
                (identity (conversation-pathname conversation)))
           (conversation-append-user-message conversation "before checkpoint")
           (conversation-append-summary conversation "checkpoint")
           (conversation-append-user-message conversation "after checkpoint")
           (let* ((pathnames (conversation-storage-pathnames identity))
                  (initial (first pathnames))
                  (active (second pathnames))
                  (initial-forms
                    (copy-tree (conversation--read-records initial)))
                  (active-forms
                    (copy-tree (conversation--read-records active))))
             (labels ((check-active-rejected (forms message)
                        "Require loading malformed active FORMS to reject with MESSAGE."
                        (unwind-protect
                             (progn
                               (conversation-identifier-migration--write-forms
                                active forms)
                               (test-assert
                                (handler-case
                                    (progn
                                      (conversation-load identity)
                                      nil)
                                  (conversation-invariant-error ()
                                    t))
                                message))
                          (conversation-identifier-migration--write-forms
                           active active-forms))))
               (let ((mismatched (copy-tree active-forms)))
                 (setf (getf (rest (first mismatched)) :id) "wrong-id")
                 (check-active-rejected
                  mismatched
                  "newest-chunk resume rejects a header from another identity"))
               (dolist (case '((4 "active resume rejects a durable sequence gap")
                               (2 "active resume rejects a duplicate sequence")))
                 (let ((malformed (copy-tree active-forms)))
                   (setf (getf (rest (third malformed)) :seq) (first case))
                   (check-active-rejected malformed (second case))))
               (let ((malformed (copy-tree active-forms)))
                 (setf (getf (rest (second malformed)) :through-seq) 0)
                 (check-active-rejected
                  malformed
                  "active resume rejects an incorrect compaction boundary"))
               (let ((malformed (copy-tree active-forms)))
                 (setf (getf (rest (second malformed)) :content) 42)
                 (check-active-rejected
                  malformed
                  "active resume rejects non-string summary content"))
               (let ((malformed (copy-tree active-forms)))
                 (setf (second malformed) (list* :summary 42))
                 (check-active-rejected
                  malformed
                  "malformed first records signal a conversation invariant error"))
               (let ((malformed (copy-tree active-forms)))
                 (setf (getf (rest (first malformed)) :version) 1)
                 (check-active-rejected
                  malformed
                  "legacy headers are rejected inside deterministic chunk files")))
             (let ((noncontiguous (copy-tree initial-forms)))
               (setf (getf (rest (second noncontiguous)) :seq) 2)
               (unwind-protect
                    (progn
                      (conversation-identifier-migration--write-forms
                       initial noncontiguous)
                      (test-assert
                       (handler-case
                           (progn
                             (conversation--map-storage-records identity #'identity)
                             nil)
                         (conversation-invariant-error ()
                           t))
                       "cross-segment scans reject noncontiguous durable sequences")
                      (test-assert
                       (handler-case
                           (progn
                             (conversation--load-all-segments identity pathnames)
                             nil)
                         (conversation-invariant-error ()
                           t))
                       "full replay rejects cross-segment sequence discontinuity"))
                 (conversation-identifier-migration--write-forms
                  initial initial-forms)))
             (let* ((directory (conversation-storage-directory-pathname identity))
                    (alias
                      (merge-pathnames
                       (make-pathname
                        :name (format nil "~V,'0D"
                                      (1+ *conversation-chunk-sequence-width*)
                                      1)
                        :type "sexp")
                       directory))
                    (long-sequence
                      (expt 10 *conversation-chunk-sequence-width*)))
               (test-assert
                (and (null (conversation-chunk-start-sequence alias))
                     (= (conversation-chunk-start-sequence
                         (conversation-chunk-pathname identity long-sequence))
                        long-sequence))
                "chunk parsing rejects zero-padded aliases but permits longer sequences"))
             (let ((records nil))
               (conversation--map-storage-records
                identity
                (lambda (record)
                  (push record records)))
               (test-assert
                (equal (mapcar (lambda (record) (getf (rest record) :seq))
                               (nreverse records))
                       '(1 2 3))
                "restored deterministic chunks remain fully scannable")))
           (let* ((publication
                    (conversation-create configuration :identifier "4Pn7wTx"))
                  (segment (conversation-log-pathname publication))
                  (record
                    (list :message :seq 1 :time 1000 :role ':user :content "one"))
                  (forms
                    (list (conversation--header-record publication)
                          record)))
             (conversation-identifier-migration--write-forms segment forms)
             (test-assert
              (conversation--published-segment-p publication segment record 1)
              "post-publication recovery accepts the complete expected header")
             (incf (getf (rest (first forms)) :working-seconds))
             (conversation-identifier-migration--write-forms segment forms)
             (test-assert
              (not
               (conversation--published-segment-p publication segment record 1))
              "post-publication recovery rejects any header-state disagreement")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-legacy-storage () null)
(defun test-conversation-legacy-storage ()
  "Test legacy single-file replay and mixed legacy-plus-chunk rotation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "9Jt4qRs"))
    (unwind-protect
         (let* ((created (conversation-create configuration :identifier identifier))
                (identity (conversation-pathname created)))
           (conversation-append-user-message created "legacy message")
           (let* ((active (conversation-log-pathname created))
                  (forms (copy-tree (conversation--read-records active))))
             (setf (getf (rest (first forms)) :version) 1)
             (conversation-identifier-migration--write-forms identity forms)
             (uiop:delete-directory-tree
              (conversation-storage-directory-pathname identity)
              :validate t
              :if-does-not-exist ':ignore))
           (let ((legacy (conversation-load identity)))
             (test-assert
              (and (equal (conversation-log-pathname legacy) identity)
                   (= (conversation-next-sequence legacy) 2)
                   (= (length (conversation-input-items legacy)) 1))
              "a legacy version-one single file remains directly resumable")
             (let* ((summary
                      (conversation-append-summary legacy "legacy checkpoint"))
                    (summary-sequence (getf (rest summary) :seq))
                    (pathnames (conversation-storage-pathnames identity)))
               (test-assert
                (and (= summary-sequence 2)
                     (= (length pathnames) 2)
                     (equal (first pathnames) identity)
                     (= (conversation-chunk-start-sequence (second pathnames)) 2))
                "compacting a legacy file retains it as the oldest storage segment")
               (let* ((reloaded (conversation-load identity))
                      (records (test-conversation--all-records reloaded)))
                 (test-assert
                  (and (equal (conversation-log-pathname reloaded)
                              (second pathnames))
                       (= (length (conversation-input-items reloaded)) 1)
                       (equal (mapcar #'first records) '(:message :summary))
                       (equal
                        (mapcar (lambda (record) (getf (rest record) :seq)) records)
                        '(1 2)))
                  "newest-chunk resume and full scans preserve mixed legacy history")))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-ephemeral-tool-projection () null)
(defun test-conversation-ephemeral-tool-projection ()
  "Test request-local tool correlation stays out of durable history and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "ephemeral-tool-projection"))
         (durable-call
           (json-object
            "type" "function_call"
            "call_id" "durable-call"
            "namespace" "test"
            "name" "echo"
            "arguments" "{\"value\":\"durable\"}"))
         (ephemeral-call
           (json-object
            "type" "function_call"
            "call_id" "ephemeral-call"
            "namespace" "skill"
            "name" "load"
            "arguments" "{\"name\":\"alpha-secret\"}")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "Run mixed calls.")
           (conversation-append-provider-item conversation durable-call)
           (conversation-append-provider-item
            conversation
            ephemeral-call
            :persistence ':next-response)
           (conversation-append-tool-result
            conversation
            "durable-call"
            :tool-name "test.echo"
            :output "echo: durable"
            :success-p t)
           (conversation-append-tool-result
            conversation
            "ephemeral-call"
            :tool-name "skill.load"
            :output "Selected alpha-secret."
            :success-p t
            :persistence ':next-response)
           (let* ((live
                    (conversation-input-items-for-request conversation))
                  (durable
                    (conversation-input-items-for-request
                     conversation
                     :include-ephemeral-p nil))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record-source
                    (with-output-to-string (stream)
                      (prin1 records stream))))
             (test-assert
              (and (= (length live) 5)
                   (string= (json-get (second live) "call_id")
                            "durable-call")
                   (string= (json-get (third live) "call_id")
                            "ephemeral-call")
                   (string= (json-get (fourth live) "call_id")
                            "durable-call")
                   (string= (json-get (fifth live) "call_id")
                            "ephemeral-call"))
              "mixed durable and request-local call items retain exact wire order")
             (test-assert
              (and (= (length durable) 3)
                   (string= (json-get (second durable) "call_id")
                            "durable-call")
                   (string= (json-get (third durable) "call_id")
                            "durable-call"))
              "compaction input excludes request-local call correlation")
             (test-assert
              (and (null (search "ephemeral-call" record-source))
                   (null (search "alpha-secret" record-source)))
              "request-local skill names, calls, and results never enter the append-only file")
             (let ((reloaded
                     (conversation-load-by-id
                      configuration
                      "ephemeral-tool-projection")))
               (test-assert
                (and (= (length (conversation-input-items reloaded)) 3)
                     (null
                      (find "ephemeral-call"
                            (conversation-input-items reloaded)
                            :key (lambda (item)
                                   (json-get item "call_id"))
                            :test #'string=)))
                "crash replay omits request-local call correlation without synthesizing repair")))
           (conversation-append-summary conversation "Durable mixed-call work.")
           (test-assert
            (and (= (length
                     (conversation-input-items-for-request conversation))
                    3)
                 (= (length
                     (conversation-input-items-for-request
                      conversation
                      :include-ephemeral-p nil))
                    1))
            "compaction preserves pending correlation only for the next normal request")
           (conversation-clear-ephemeral-input-items conversation)
           (test-assert
            (and (= (length (conversation-input-items conversation)) 1)
                 (null (conversation-ephemeral-input-entries conversation)))
            "a successful provider response consumes all request-local correlation")
           (let ((reloaded
                   (conversation-load-by-id
                    configuration
                    "ephemeral-tool-projection")))
             (test-assert
              (= (length (conversation-input-items reloaded)) 1)
              "replay after compaction contains only the durable summary bridge")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-ephemeral-append-interruption () null)
(defun test-conversation-ephemeral-append-interruption ()
  "Test interrupted request-local insertion remains owned and removable."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration :identifier "ephemeral-append-interruption"))
         (item
           (json-object
            "type" "function_call"
            "call_id" "interrupted-ephemeral-call"
            "namespace" "skill"
            "name" "load"
            "arguments" "{\"name\":\"interrupted-skill\"}"))
         (original-append
           (symbol-function 'conversation--append-input-item)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'conversation--append-input-item
            (lambda (target candidate)
              (funcall original-append target candidate)
              (error "Injected interruption after provider projection append."))))
          (lambda ()
            (test-assert
             (handler-case
                 (progn
                   (conversation--append-ephemeral-input-item conversation item)
                   nil)
               (simple-error ()
                 t))
             "an interruption after projection append reaches the caller")
            (test-assert
             (and
              (eq item (first (conversation-input-items conversation)))
              (eq
               item
               (getf
                (first
                 (conversation-ephemeral-input-entries conversation))
                :item)))
             "an interrupted provider item already has request-local ownership")
            (conversation-clear-ephemeral-input-items conversation)
            (test-assert
             (and
              (null (conversation-input-items conversation))
              (null (conversation-input-items-tail conversation))
              (null (conversation-ephemeral-input-entries conversation)))
             "request-local cleanup removes the interrupted provider item")))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-origin-directory () null)
(defun test-conversation-origin-directory ()
  "Test origin directory persistence, peeking, and legacy header tolerance."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "origin"))
                (expected (namestring
                           (configuration-working-directory configuration))))
           (test-assert (string= (conversation-origin-directory conversation)
                                 expected)
                        "a new conversation records its origin directory")
           (conversation-append-user-message conversation "remember this workspace")
           (test-assert (string= (conversation-origin-directory
                                  (conversation-load-by-id configuration
                                                           "origin"))
                                 expected)
                        "a reloaded conversation preserves its origin directory")
           (test-assert (string= (getf (rest (conversation-peek-header
                                              (conversation-pathname
                                               conversation)))
                                       :directory)
                                 expected)
                        "peeking reads the origin directory cheaply")
           (let ((legacy (conversation-pathname-for-id configuration "legacy")))
             (snapshot-write
              legacy
              (list :conversation :version 1 :id "legacy" :created-at 1))
             (test-assert (null (conversation-origin-directory
                                 (conversation-load-by-id configuration
                                                          "legacy")))
                          "legacy conversations without an origin still load")
             (test-assert
              (not (find legacy (conversation-list configuration) :test #'equal))
              "header-only legacy conversations stay out of saved listings")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-model-selection () null)
(defun test-conversation-model-selection ()
  "Test model selection headers, append-only changes, and legacy loading."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "model-choice")))
           (test-assert
            (string= (conversation-model conversation) "gpt-5.6-sol")
            "new conversations inherit the configured model")
           (test-assert
            (string= (conversation-reasoning-effort conversation) "ultra")
            "new conversations inherit the configured effort")
           (conversation-set-model-selection conversation "gpt-5.6-luna" "high")
           (test-assert
            (not (conversation-storage-occupied-p
                  (conversation-pathname conversation)))
            "selecting a model does not persist an empty conversation")
           (conversation-append-user-message conversation "remember this model")
           (let ((header (first (conversation--read-records
                                 (conversation-pathname conversation)))))
             (test-assert (string= (getf (rest header) :model) "gpt-5.6-luna")
                          "the initial model is stored in the header")
             (test-assert
              (string= (getf (rest header) :reasoning-effort) "high")
              "the initial effort is stored in the header"))
           (conversation-set-model-selection conversation "gpt-5.6-terra" "low")
           (let* ((records (conversation--read-records
                            (conversation-pathname conversation)))
                  (selection (first (last records))))
             (test-assert (eq (first selection) :configuration)
                          "later model changes append configuration records")
             (test-assert (string= (getf (rest selection) :model)
                                   "gpt-5.6-terra")
                          "the appended record carries the changed model"))
           (let ((reloaded (conversation-load-by-id configuration "model-choice")))
             (test-assert
              (string= (conversation-model reloaded) "gpt-5.6-terra")
              "conversation replay restores the latest model")
             (test-assert
              (string= (conversation-reasoning-effort reloaded) "low")
              "conversation replay restores the latest effort"))
           (let ((legacy (conversation-pathname-for-id configuration "legacy-model")))
             (snapshot-write
              legacy
              (list :conversation :version 1 :id "legacy-model" :created-at 1))
             (let ((loaded (conversation-load-by-id configuration "legacy-model")))
               (test-assert (null (conversation-model loaded))
                            "legacy conversations load without a model")
               (test-assert (null (conversation-reasoning-effort loaded))
                            "legacy conversations load without an effort"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-interrupted-tool-call () null)
(defun test-conversation-interrupted-tool-call ()
  "Test append-only repair of a function call whose process exited mid-tool."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "interrupted"))
                 (call
                   ;; Keep the removed name to cover interrupted pre-upgrade history.
                   (json-object
                    "type" "function_call"
                    "status" "completed"
                    "arguments" "{\"patterns\":[\"one\",\"two\"]}"
                    "call_id" "call-interrupted"
                    "name" "multi-content"
                    "namespace" "search")))
           (conversation-append-user-message conversation "continue the task")
           (conversation-append-provider-item conversation call)
           ;; Reproduce a user message persisted after restart but before the
           ;; malformed provider replay was rejected.
           (conversation-append-user-message conversation "carry on")
           (let* ((loaded
                    (conversation-load-by-id configuration "interrupted"))
                  (items (conversation-input-items loaded))
                  (records
                    (conversation--read-records
                     (conversation-pathname loaded))))
             (test-assert (= (length items) 4)
                          "replay adds exactly one interrupted tool output")
             (test-assert
              (and (string= (json-get (second items) "type") "function_call")
                   (string= (json-get (third items) "type")
                            "function_call_output")
                   (string= (json-get (third items) "call_id")
                            "call-interrupted")
                   (search "may have changed external state"
                           (json-get (third items) "output"))
                   (string= (json-get (fourth items) "role") "user"))
              "repair places an honest failure output before later user input")
             (let ((repair (first (last records))))
               (test-assert
                (and (eq (first repair) :tool-result)
                     (eq (getf (rest repair) :status) :error)
                     (string= (getf (rest repair) :call-id)
                              "call-interrupted")
                     (string= (getf (rest repair) :tool)
                              "search.multi-content"))
                "repair persists a correlated append-only failure record"))
             (let ((record-count (length records))
                   (reloaded
                     (conversation-load-by-id configuration "interrupted")))
               (test-assert
                (= (length (conversation--read-records
                            (conversation-pathname reloaded)))
                   record-count)
                "loading repaired history does not append duplicate outputs")
               (test-assert
                (string= (json-get
                          (third (conversation-input-items reloaded))
                          "type")
                         "function_call_output")
                "reloaded history keeps the repaired provider ordering"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-late-duplicate-tool-output () null)
(defun test-conversation-late-duplicate-tool-output ()
  "Test replay keeps the result used before a stale writer appended another."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "duplicate-output"))
         (call
           (json-object
            "type" "function_call"
            "status" "completed"
            "arguments" "{}"
            "call_id" "call-duplicate"
            "name" "run"
            "namespace" "shell")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "run the check")
           (conversation-append-provider-item conversation call)
           (conversation-append-tool-result
            conversation
            "call-duplicate"
            :tool-name "shell.run"
            :output *conversation-interrupted-tool-output*
            :success-p nil)
           (conversation-append-user-message conversation "continue")
           (log-append
            (conversation-log-pathname conversation)
            `(:tool-result
              :seq 5
              :time ,(get-universal-time)
              :call-id "call-duplicate"
              :tool "shell.run"
              :status :error
              :output "The user denied this command."
              :wire-json
              ,(json-encode
                (function-call-output-item
                 "call-duplicate"
                 "The user denied this command."))))
           (let* ((record-count
                    (length
                     (conversation--read-records
                      (conversation-pathname conversation))))
                  (loaded
                    (conversation-load-by-id configuration "duplicate-output"))
                  (items (conversation-input-items loaded))
                  (outputs
                    (remove-if-not
                     (lambda (item)
                       (and
                        (json-object-p item)
                        (conversation--wire-item-type-p
                         item "function_call_output")))
                     items)))
             (test-assert
              (and (= (length outputs) 1)
                   (string=
                    (json-get (first outputs) "output")
                    *conversation-interrupted-tool-output*))
              "replay keeps the first tool output used by subsequent history")
             (test-assert
              (= (length
                  (conversation--read-records
                   (conversation-pathname loaded)))
                 record-count)
              "replay leaves the stale duplicate only in the append-only log")
             (test-assert
              (string= (json-get (fourth items) "role") "user")
              "replay retains history produced after the selected output")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      call
                      (function-call-output-item
                       "call-duplicate"
                       "first ordinary result")
                      (function-call-output-item
                       "call-duplicate"
                       "second ordinary result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "replay rejects arbitrary conflicting tool outputs")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      call
                      (function-call-output-item
                       "call-duplicate"
                       *conversation-interrupted-tool-output*)
                      (function-call-output-item
                       "call-duplicate"
                       "first stale result")
                      (function-call-output-item
                       "call-duplicate"
                       "second stale result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "replay tolerates only one stale result after a repair")
             (test-assert
              (handler-case
                  (progn
                    (conversation--tool-item-tables
                     loaded
                     (list
                      (function-call-output-item
                       "call-duplicate"
                       *conversation-interrupted-tool-output*)
                      call
                      (function-call-output-item
                       "call-duplicate"
                       "late ordinary result")))
                    nil)
                (conversation-invariant-error ()
                  t))
              "duplicate tolerance requires the call before the repair")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-malformed-tool-projections () null)
(defun test-conversation-malformed-tool-projections ()
  "Test replay rejects impossible durable tool-output projections."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "malformed-tools")))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (conversation--apply-record
                   conversation
                   '(:tool-result
                     :seq 1
                     :call-id "failed-image"
                     :status :error
                     :content-blocks ((:text "impossible"))))
                  nil)
              (conversation-invariant-error ()
                t))
            "replay rejects image-form output on a failed tool result")
           (test-assert
            (handler-case
                (progn
                  (conversation--apply-record
                   conversation
                   '(:tool-result
                     :seq 2
                     :call-id "ambiguous"
                     :status :ok
                     :content-blocks ((:text "one"))
                     :wire-json "{}"))
                  nil)
              (conversation-invariant-error ()
                t))
            "replay rejects a tool result with competing wire projections"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-private-storage () null)
(defun test-conversation-private-storage ()
  "Test private transcript persistence outside public conversation discovery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (storage-root  (merge-pathnames "private/task-transcripts/" root)))
    (unwind-protect
         (let ((conversation
                 (conversation-create configuration
                                      :identifier "private-turn"
                                      :storage-root storage-root)))
            (test-assert (not (conversation-storage-occupied-p
                               (conversation-pathname conversation)))
                         "an empty private conversation leaves no transcript")
            (conversation-append-user-message conversation "private assignment")
            (test-assert (conversation-storage-active-pathname
                          (conversation-pathname conversation))
                         "the first private record persists its transcript")
           (let ((loaded (conversation-load
                          (conversation-pathname conversation))))
             (test-assert
              (string= (json-get (first (conversation-input-items loaded))
                                 "role")
                       "user")
              "a private transcript remains directly reloadable"))
           (test-assert
            (not (find (conversation-pathname conversation)
                       (conversation-list configuration)
                       :test #'equal))
            "private transcripts stay out of public conversation listings")
           (test-assert
            (handler-case
                (progn
                  (conversation-load-by-id configuration "private-turn")
                  nil)
              (conversation-error ()
                t))
            "public identifier loading cannot reach a private transcript"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-concurrent-appends () null)
(defun test-conversation-concurrent-appends ()
  "Test concurrent writers retain one contiguous durable sequence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "concurrent-appends"))
         (threads
           (loop for writer below 4
                 collect
                 (make-thread
                  (lambda ()
                    (dotimes (index 25)
                      (conversation-append-record
                       conversation
                       (list :goal
                             :writer writer
                             :index index))))
                  :name (format nil
                                "Autolith conversation writer ~D"
                                writer)))))
    (unwind-protect
         (progn
           (dolist (thread threads)
             (join-thread thread))
           (multiple-value-bind (records incomplete-tail-p)
               (conversation--read-records
                (conversation-pathname conversation))
             (let ((sequences
                     (mapcar (lambda (record)
                               (getf (rest record) :seq))
                             (rest records))))
               (test-assert
                (and (not incomplete-tail-p)
                     (= (length records) 101)
                     (equal sequences
                            (loop for sequence from 1 to 100
                                  collect sequence)))
                "concurrent appends preserve every unique sequence in order"))))
      (dolist (thread threads)
        (when (thread-alive-p thread)
          (join-thread thread)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation--descriptor-read-byte (integer) integer)
(defun test-conversation--descriptor-read-byte (descriptor)
  "Read one synchronization byte from DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type '(unsigned-byte 8)
           :initial-element 0)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:read descriptor (sb-sys:vector-sap buffer) 1))))

(-> test-conversation--descriptor-write-byte (integer) integer)
(defun test-conversation--descriptor-write-byte (descriptor)
  "Write one synchronization byte to DESCRIPTOR and return the byte count."
  (let ((buffer
          (make-array
           1
           :element-type '(unsigned-byte 8)
           :initial-element 1)))
    (sb-sys:with-pinned-objects (buffer)
      (sb-posix:write descriptor (sb-sys:vector-sap buffer) 1))))

(-> test-conversation--single-thread-p () boolean)
(defun test-conversation--single-thread-p ()
  "Return true when this image may safely use SB-POSIX:FORK."
  (= 1 (length (sb-thread:list-all-threads))))

(-> test-conversation--sbcl-command () string)
(defun test-conversation--sbcl-command ()
  "Return the SBCL command used to spawn isolated lease children."
  (or (uiop:getenv "AUTOLITH_SBCL")
      (namestring (truename (uiop:argv0)))
      "sbcl"))

(-> test-conversation--child-configuration-form (configuration) string)
(defun test-conversation--child-configuration-form (configuration)
  "Return a readable form that reconstructs CONFIGURATION in a child process."
  (let ((*package* (find-package '#:cl-user))
        (*print-readably* t)
        (*print-circle* nil))
    (prin1-to-string
     `(make-instance
       'configuration
       :source-root
       (pathname ,(namestring (configuration-source-root configuration)))
       :working-directory
       (pathname ,(namestring (configuration-working-directory configuration)))
       :config-root
       (pathname ,(namestring (configuration-config-root configuration)))
       :data-root
       (pathname ,(namestring (configuration-data-root configuration)))
       :state-root
       (pathname ,(namestring (configuration-state-root configuration)))
       :cache-root
       (pathname ,(namestring (configuration-cache-root configuration)))
       :codex-auth-path
       (pathname ,(namestring (configuration-codex-auth-path configuration)))
       :grok-bootstrap-auth-path
       (pathname
        ,(namestring (configuration-grok-bootstrap-auth-path configuration)))
       :model ,*default-model*
       :reasoning-effort ,*default-reasoning-effort*
       :provider-endpoint ,*codex-responses-endpoint*))))

(-> test-conversation--child-project-setup () pathname)
(defun test-conversation--child-project-setup ()
  "Return the locked project setup loaded by fresh test children."
  (let ((pathname
          (merge-pathnames ".qlot/setup.lisp"
                           (asdf:system-source-directory :autolith))))
    (or (probe-file pathname)
        (error "Autolith child tests need locked dependencies at ~A" pathname))))

(-> test-conversation--child-command (string) list)
(defun test-conversation--child-command (form)
  "Return a fresh-SBCL command loading locked Autolith dependencies and FORM."
  (let ((source-root (asdf:system-source-directory :autolith)))
    (list (test-conversation--sbcl-command)
          "--noinform"
          "--no-sysinit"
          "--no-userinit"
          "--disable-debugger"
          "--non-interactive"
          "--eval" "(require :asdf)"
          ;; Nix-wrapped SBCL binaries force-extend CL_SOURCE_REGISTRY with
          ;; store paths, so ignore the inherited registry in-process before
          ;; the locked project setup loads.
          "--eval"
          "(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))"
          "--eval"
          (format nil "(load ~S)"
                  (namestring (test-conversation--child-project-setup)))
          "--eval"
          (format nil "(asdf:load-asd ~S)"
                  (namestring (merge-pathnames "autolith.asd" source-root)))
          "--eval" "(asdf:load-system :autolith)"
          "--eval" form
          "--quit")))

(-> test-conversation--run-child-form
    (string &key (:environment list))
    (integer 0 255))
(defun test-conversation--run-child-form (form &key environment)
  "Evaluate FORM in a fresh SBCL and return its status and captured output."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (let ((command (test-conversation--child-command form)))
         (if environment
             (append (list "env") environment command)
             command))
       :output ':string
       :error-output ':output
       :ignore-error-status t)
    (declare (ignore error-output))
    (values status output)))

(-> test-conversation-child-project-setup () null)
(defun test-conversation-child-project-setup ()
  "Test fresh children load Autolith through the locked project setup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (home (merge-pathnames "isolated-home/" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "placeholder" home))
           (multiple-value-bind (status output)
               (test-conversation--run-child-form
                "(uiop:quit 0)"
                :environment
                (list (format nil "HOME=~A" (namestring home))
                      (format nil "XDG_CONFIG_HOME=~A"
                              (namestring (merge-pathnames "config/" home)))
                      (format nil "XDG_CACHE_HOME=~A"
                              (namestring (merge-pathnames "cache/" home)))
                      "CL_SOURCE_REGISTRY=(:source-registry :ignore-inherited-configuration)"))
             (test-assert
              (zerop status)
              (format nil
                      "the clean child loads locked Autolith dependencies:~%~A"
                      output))))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation--call-with-child-lease/fork
    (configuration string function)
    null)
(defun test-conversation--call-with-child-lease/fork
    (configuration identifier function)
  "Call FUNCTION while a forked child holds IDENTIFIER until released."
  (multiple-value-bind (ready-read ready-write)
      (sb-posix:pipe)
    (multiple-value-bind (release-read release-write)
        (sb-posix:pipe)
      (let ((child-pid (sb-posix:fork))
            (child-status nil))
        (if (zerop child-pid)
            (progn
              (ignore-errors (sb-posix:close ready-read))
              (ignore-errors (sb-posix:close release-write))
              (handler-case
                  (progn
                    (conversation-lease-acquire configuration identifier)
                    (test-conversation--descriptor-write-byte ready-write)
                    (test-conversation--descriptor-read-byte release-read)
                    ;; Deliberately bypass release to exercise kernel cleanup
                    ;; after a dead conversation owner.
                    (sb-posix:_exit 0))
                (serious-condition ()
                  (sb-posix:_exit 1))))
            (progn
              (sb-posix:close ready-write)
              (sb-posix:close release-read)
              (unwind-protect
                   (progn
                     (test-assert
                      (= (test-conversation--descriptor-read-byte ready-read) 1)
                      "the child process acquired its conversation lease")
                     (funcall function))
                (ignore-errors
                  (test-conversation--descriptor-write-byte release-write))
                (ignore-errors
                  (sb-posix:close ready-read))
                (ignore-errors
                  (sb-posix:close release-write))
                (multiple-value-bind (waited-pid status)
                    (sb-posix:waitpid child-pid 0)
                  (test-assert
                   (= waited-pid child-pid)
                   "the conversation lease holder was reaped")
                  (setf child-status status)))
              (test-assert
               (and (sb-posix:wifexited child-status)
                    (zerop (sb-posix:wexitstatus child-status)))
               "the child conversation owner exited cleanly"))))))
  nil)

(-> test-conversation--call-with-child-lease/process
    (configuration string function)
    null)
(defun test-conversation--call-with-child-lease/process
    (configuration identifier function)
  "Call FUNCTION while an external SBCL holds IDENTIFIER until released.

SBCL cannot fork once any non-main thread exists, so multi-threaded suites use a
fresh process and file-based synchronization instead of SB-POSIX:FORK."
  (let* ((root (test-configuration-root configuration))
         (ready-path (merge-pathnames "lease-child-ready" root))
         (release-path (merge-pathnames "lease-child-release" root))
         (output-path (merge-pathnames "lease-child-output" root))
         (process nil))
    (dolist (pathname (list ready-path release-path output-path))
      (when (probe-file pathname)
        (delete-file pathname)))
    (setf process
          (uiop:launch-program
           (test-conversation--child-command
            (format
             nil
             "(let ((configuration ~A)) (autolith::conversation-lease-acquire configuration ~S) (with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create) (write-line \"ready\" stream)) (loop until (probe-file ~S) do (sleep 0.05)) (uiop:quit 0))"
             (test-conversation--child-configuration-form configuration)
             identifier
             (namestring ready-path)
             (namestring release-path)))
           :output output-path
           :error-output ':output))
    (unwind-protect
         (progn
           (loop with deadline = (+ (get-internal-real-time)
                                    (* 30 internal-time-units-per-second))
                 until (or (probe-file ready-path)
                           (not (uiop:process-alive-p process)))
                 do (when (> (get-internal-real-time) deadline)
                      (error "Timed out waiting for the child lease holder."))
                    (sleep 0.05))
           (test-assert (probe-file ready-path)
                        "the child process acquired its conversation lease")
           (funcall function))
      (ignore-errors
        (with-open-file (stream release-path
                                :direction ':output
                                :if-exists ':supersede
                                :if-does-not-exist ':create)
          (write-line "release" stream)))
      (let ((status (uiop:wait-process process)))
        (test-assert
         (zerop status)
         (format nil "the child conversation owner exited cleanly:~%~A"
                 (if (probe-file output-path)
                     (uiop:read-file-string output-path)
                      "<no child output>"))))))
  nil)

(-> test-conversation--call-with-child-lease
    (configuration string function)
    null)
(defun test-conversation--call-with-child-lease
    (configuration identifier function)
  "Call FUNCTION while a child process holds IDENTIFIER until explicitly released."
  (if (test-conversation--single-thread-p)
      (test-conversation--call-with-child-lease/fork
       configuration identifier function)
      (test-conversation--call-with-child-lease/process
       configuration identifier function)))

(-> test-conversation--child-can-acquire-lease-p
    (configuration string)
    boolean)
(defun test-conversation--child-can-acquire-lease-p
    (configuration identifier)
  "Return true when a separate child process can claim IDENTIFIER."
  (if (test-conversation--single-thread-p)
      (let ((child-pid (sb-posix:fork)))
        (if (zerop child-pid)
            (handler-case
                (progn
                  (ls-flock:reset-after-fork)
                  (conversation-lease-acquire configuration identifier)
                  (sb-posix:_exit 0))
              (conversation-in-use ()
                (sb-posix:_exit 2))
              (serious-condition ()
                (sb-posix:_exit 1)))
            (multiple-value-bind (waited-pid status)
                (sb-posix:waitpid child-pid 0)
              (and (= waited-pid child-pid)
                   (sb-posix:wifexited status)
                   (zerop (sb-posix:wexitstatus status))))))
      (zerop
       (test-conversation--run-child-form
        (format
         nil
         "(handler-case (progn (let ((configuration ~A)) (autolith::conversation-lease-acquire configuration ~S)) (uiop:quit 0)) (autolith::conversation-in-use () (uiop:quit 2)) (serious-condition () (uiop:quit 1)))"
         (test-conversation--child-configuration-form configuration)
         identifier)))))

(-> test-conversation-process-lease () null)
(defun test-conversation-process-lease ()
  "Test live-owner exclusion and automatic lease release after process death."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier "K8vQ2mp")
         (conversation
           (conversation-create configuration :identifier identifier)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "persist this session")
           (test-conversation--call-with-child-lease
            configuration
            identifier
            (lambda ()
              (test-assert
               (handler-case
                   (let ((lease
                           (conversation-lease-acquire
                            configuration identifier)))
                     (conversation-lease-release lease)
                     nil)
                 (conversation-in-use (condition)
                   (and
                    (string=
                     (conversation-in-use-identifier condition)
                     identifier)
                    (equal
                     (conversation-error-pathname condition)
                     (conversation-pathname conversation)))))
               "a second process cannot own the active conversation")
              (test-assert
               (and
                (find
                 (conversation-pathname conversation)
                 (conversation-list configuration)
                 :test #'equal)
                (conversation-peek-header
                 (conversation-pathname conversation)))
               "conversation picker enumeration remains read-only while leased")
               (let ((records-seen 0))
                 (conversation--map-records
                  (conversation-log-pathname conversation)
                  (lambda (record)
                    (declare (ignore record))
                    (incf records-seen)))
                 (test-assert
                  (= records-seen 2)
                  "history enumeration remains read-only while leased"))))
           (let ((lease
                   (conversation-lease-acquire configuration identifier)))
             (unwind-protect
                  (progn
                    (test-assert
                     (conversation-lease-held-p lease)
                     "a dead process releases its conversation lease")
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-lease-acquire
                            configuration identifier)
                           nil)
                       (conversation-in-use ()
                         t))
                     "one process cannot acquire the same lease twice")
                    (test-assert
                     (not
                      (test-conversation--child-can-acquire-lease-p
                       configuration identifier))
                     "the process-local guard retains the kernel lease"))
               (conversation-lease-release lease))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-inherited-reference () null)
(defun test-conversation-inherited-reference ()
  "Test filtered spawn-time parent history, durable replay, and isolation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (parent (conversation-create configuration :identifier "reference-parent"))
         (child (conversation-create configuration :identifier "reference-child"))
         (assistant-part
           (json-object "type" "output_text" "text" "Parent answer."))
         (assistant-item
           (json-object "type" "message"
                        "role" "assistant"
                        "content" (json-array assistant-part))))
    (unwind-protect
         (progn
           (conversation-append-user-message parent "Parent question.")
           (conversation-append-provider-item
            parent
            (json-object
             "type" "message"
             "role" "assistant"
             "phase" "commentary"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "Intermediate note."))))
           (conversation-append-provider-item parent assistant-item)
           (conversation-append-provider-item
            parent
            (json-object "type" "function_call"
                         "call_id" "parent-tool"
                         "name" "read"
                         "namespace" "fs"
                         "arguments" "{}"))
           (conversation-append-provider-item
            parent
            (json-object
             "type" "message"
             "role" "user"
             "content" (json-array
                        (json-object "type" "input_image"
                                     "image_url" "data:image/png;base64,AAAA")
                        (json-object "type" "input_text"
                                     "text" "Portable image caption."))))
           (conversation-append-provider-item
            parent
            (json-object "type" "message"
                         "role" "assistant"
                         "content" (json-array
                                    (json-object "type" "output_text"
                                                 "text" "Ephemeral answer.")))
            :persistence ':next-response)
           (let ((snapshot
                    (conversation-inherited-reference-snapshot parent 1000000)))
             (setf (gethash "text" assistant-part) "Mutated parent answer.")
             (test-assert (= (length snapshot) 3)
                          "reference snapshots retain only portable turn messages")
             (test-assert
              (string= (json-get
                        (aref (json-get (second snapshot) "content") 0)
                        "text")
                       "Parent answer.")
              "reference snapshots do not alias parent provider items")
             (let* ((newest (third snapshot))
                    (limit
                      (conversation--inherited-reference-wire-byte-length
                       (list newest)))
                    (bounded
                      (conversation-inherited-reference-snapshot parent limit)))
               (test-assert
                (and (= (length bounded) 1)
                     (string=
                      (json-get
                       (aref (json-get (first bounded) "content") 0)
                       "text")
                      "Portable image caption.")
                     (<= (conversation--inherited-reference-wire-byte-length
                          bounded)
                         limit))
                "bounded snapshots retain the newest complete fitting suffix"))
             (conversation-append-inherited-reference
              child (conversation-identifier parent) snapshot)
             (conversation-append-user-message child "Child assignment.")
             (let ((items (conversation-input-items child)))
               (test-assert (= (length items) 5)
                            "child projection adds one boundary and assignment")
               (test-assert
                (equal (mapcar (lambda (item) (json-get item "role")) items)
                       '("user" "assistant" "user" "developer" "user"))
                "inherited messages precede a developer boundary and child task")
               (test-assert
                (string= (json-get
                          (aref (json-get (third items) "content") 0)
                          "text")
                         "Portable image caption.")
                "inherited reference history strips inline image data"))
             (conversation-append-user-message parent "Later parent turn.")
             (test-assert (= (length (conversation-input-items child)) 5)
                          "later parent activity cannot mutate the child snapshot")
             (let ((reloaded
                     (conversation-load-by-id configuration "reference-child")))
               (test-assert
                (equal (mapcar #'json-encode
                               (conversation-input-items reloaded))
                       (mapcar #'json-encode
                               (conversation-input-items child)))
                "inherited reference history replays exactly from one durable record")
               (let ((records
                       (conversation--read-records
                        (conversation-pathname reloaded))))
                 (test-assert
                  (= (count :inherited-reference records :key #'first) 1)
                  "child history persists one hidden inherited-reference record"))
               (conversation-append-summary reloaded "Compacted child reference.")
               (test-assert
                (and (= (length (conversation-input-items reloaded)) 1)
                     (search "Compacted child reference."
                             (json-encode
                              (first (conversation-input-items reloaded)))))
                "ordinary compaction replaces inherited reference history")
               (let ((compacted
                       (conversation-load-by-id configuration "reference-child")))
                 (test-assert
                  (= (length (conversation-input-items compacted)) 1)
                  "compacted inherited reference history replays as one bridge"))))
          (let* ((filtered-parent
                   (conversation-create configuration
                                        :identifier "filtered-reference-parent"))
                 (boundary-child
                   (conversation-create configuration
                                        :identifier "boundary-reference-child")))
            (conversation-append-provider-item
             filtered-parent
             (json-object "type" "function_call"
                          "call_id" "filtered-tool"
                          "name" "read"
                          "arguments" "{}"))
            (let ((snapshot
                    (conversation-inherited-reference-snapshot
                     filtered-parent 1000000)))
              (test-assert (null snapshot)
                           "fully filtered parent history yields no messages")
              (conversation-append-inherited-reference
               boundary-child (conversation-identifier filtered-parent) snapshot)
              (test-assert
               (and (= (length (conversation-input-items boundary-child)) 1)
                    (conversation--inherited-reference-boundary-p
                     (first (conversation-input-items boundary-child))))
               "empty inherited history still persists its reference boundary")
              (let ((reloaded
                      (conversation-load-by-id
                       configuration "boundary-reference-child")))
                (test-assert
                 (and (= (length (conversation-input-items reloaded)) 1)
                      (conversation--inherited-reference-boundary-p
                       (first (conversation-input-items reloaded))))
                 "boundary-only inherited history replays durably"))))
          (let ((malformed
                  (conversation-create configuration
                                       :identifier "malformed-reference")))
            (test-assert
             (handler-case
                 (progn
                   (conversation--apply-record
                    malformed
                    (list :inherited-reference
                          :seq 1
                          :source-conversation-id "reference-parent"
                          :wire-json
                          (json-encode
                           (json-array
                            (json-object "type" "function_call"
                                         "call_id" "forged")
                            (conversation--inherited-reference-boundary-item)))))
                   nil)
               (conversation-invariant-error ()
                 t))
             "replay rejects nonportable inherited-reference items")
            (test-assert
             (handler-case
                 (progn
                   (conversation--apply-record
                    malformed
                    (list :inherited-reference
                          :seq 2
                          :source-conversation-id "reference-parent"
                          :wire-json "{not-json"))
                   nil)
               (conversation-invariant-error ()
                 t))
             "replay wraps malformed inherited-reference JSON")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (test-conversation-image-input)
  (test-conversation-inherited-reference)
  (test-conversation-ephemeral-tool-projection)
  (test-conversation-ephemeral-append-interruption)
  (test-conversation-malformed-tool-projections)
  (test-conversation-concurrent-appends)
  (test-conversation-child-project-setup)
  (test-conversation-process-lease)
  (test-conversation-interrupted-tool-call)
  (test-conversation-late-duplicate-tool-output)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration :identifier "test-turn"))
                (projection
                  (conversation-create configuration :identifier "future-record"))
                (assistant-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text" "text" "hello")))))
           (conversation--apply-record projection '(:future-record :seq 7))
           (test-assert (= (conversation-next-sequence projection) 8)
                        "unknown record kinds retain common projection")
           (test-assert (not (conversation-persisted-p conversation))
                        "a new conversation begins only in memory")
           (test-assert
            (not (conversation-storage-occupied-p
                  (conversation-pathname conversation)))
            "an empty conversation has no durable storage")
           (test-assert (null (conversation-list configuration))
                        "empty conversations never appear in saved listings")
           (conversation-append-user-message conversation "hi")
           (test-assert (conversation-persisted-p conversation)
                        "the first durable record publishes the conversation")
           (let ((records (conversation--read-records
                           (conversation-pathname conversation))))
             (test-assert (and (= (length records) 2)
                               (eq (first (first records)) :conversation)
                               (eq (first (second records)) :message))
                          "first persistence atomically publishes header and record"))
           (conversation-append-provider-item conversation assistant-item)
           (conversation-append-tool-result
            conversation
            "call-1"
            :tool-name "lisp.eval"
            :output "42"
            :success-p t)
           (test-assert
            (eq (conversation-input-items-tail conversation)
                (last (conversation-input-items conversation)))
            "provider projection appends retain their constant-time tail")
           (with-open-file (stream (conversation-log-pathname conversation)
                                   :direction ':output
                                   :if-exists ':append
                                   :external-format ':utf-8)
             (write-string "(:incomplete" stream))
           (let ((loaded (conversation-load-by-id configuration "test-turn")))
             (test-assert (= (length (conversation-input-items loaded)) 3)
                          "conversation reload projects complete wire items")
             (test-assert (= (conversation-next-sequence loaded) 4)
                          "conversation reload restores its next sequence")
             (test-assert
              (string= (json-get (first (conversation-input-items loaded)) "role")
                       "user")
              "conversation reload preserves the first user message")
             (test-assert (zerop (conversation-log-generation loaded))
                          "a loaded log begins at generation zero")
             (conversation-append-user-message loaded "after interrupted write")
             (test-assert (= (conversation-log-generation loaded) 1)
                          "tail repair invalidates incremental log positions")
             (multiple-value-bind (records incomplete-tail-p)
                 (conversation--read-records (conversation-pathname loaded))
               (test-assert
                (and (not incomplete-tail-p)
                     (= (length records) 5)
                     (string= (getf (rest (first (last records))) :content)
                              "after interrupted write"))
                "the next conversation append atomically repairs its tail"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-working-seconds () null)
(defun test-conversation-working-seconds ()
  "Test working-time accumulation across durable records and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((pathname (conversation-pathname-for-id configuration "worked"))
               (message-json
                 "{\"type\":\"message\",\"role\":\"user\",\"content\":[]}")
               (reasoning-json "{\"type\":\"reasoning\",\"content\":[]}"))
           (ensure-directories-exist pathname)
           (snapshot-write
            pathname
            (list :conversation :version 1 :id "worked" :created-at 1000))
           (dolist (record
                    (list
                     (list :message :seq 1 :time 1000 :role ':user
                           :content "start" :wire-json message-json)
                     (list :provider-item :seq 2 :time 1030
                           :wire-json reasoning-json)
                     (list :provider-item :seq 3 :time 1090
                           :wire-json reasoning-json)
                     (list :message :seq 4 :time 1500 :role ':user
                           :content "next" :wire-json message-json)
                     (list :provider-item :seq 5 :time 1520
                           :wire-json reasoning-json)))
             (log-append pathname record))
           (multiple-value-bind (working-seconds user-turn-count)
               (conversation-activity-summary pathname)
             (test-assert (= working-seconds 110)
                          "activity summaries reproduce durable working time")
             (test-assert (= user-turn-count 2)
                          "activity summaries count durable user turns")
             (test-assert (string= (application--conversation-tally pathname)
                                   "01:50")
                          "resume tallies prefer measured working time"))
           (let ((metadata-pathname
                   (conversation-picker-metadata-pathname pathname)))
             (test-assert (probe-file metadata-pathname)
                          "activity scans publish compact picker metadata")
             (let ((metadata (conversation-picker-metadata-read pathname)))
               (test-assert
                (and metadata
                     (= (conversation-picker-metadata-working-seconds metadata)
                        110)
                     (= (conversation-picker-metadata-user-turn-count metadata) 2)
                     (string= (conversation-picker-metadata-preview metadata)
                              "next"))
                "picker metadata retains exact tallies and newest messages"))
             (test-call-with-function-replacements
              (list
               (list
                'conversation--map-records
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error "A valid picker cache must avoid a log scan."))))
              (lambda ()
                (multiple-value-bind (working-seconds user-turn-count)
                    (conversation-activity-summary pathname)
                  (test-assert
                   (and (= working-seconds 110) (= user-turn-count 2))
                   "valid picker metadata avoids rescanning conversation logs")))))
           (let ((loaded (conversation-load-by-id configuration "worked")))
             (test-assert
              (= (conversation-working-seconds loaded) 110)
              "working seconds accumulate record gaps within logical turns")
             (test-assert
              (= (conversation-user-turn-count loaded) 2)
              "working-time replay preserves the user turn count")
             (conversation-append-user-message loaded "one more")
             (test-assert
              (= (conversation-working-seconds loaded) 110)
              "the idle gap before a live user message accrues nothing"))
           (let ((reloaded (conversation-load-by-id configuration "worked")))
             (test-assert
              (= (conversation-working-seconds reloaded) 110)
              "replay reproduces the accumulated working seconds exactly")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-picker-metadata-stability () null)
(defun test-conversation-picker-metadata-stability ()
  "Test that an append racing a cache scan cannot publish stale metadata."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (conversation-pathname-for-id configuration "metadata-race")))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (snapshot-write
            pathname
            (list :conversation :version 1 :id "metadata-race" :created-at 1000))
           (log-append pathname
                       (list :message :seq 1 :time 1000 :role ':user
                             :content "first"))
           (let ((map-records-function (symbol-function 'conversation--map-records)))
             (test-assert
              (null
               (test-call-with-function-replacements
                (list
                 (list
                  'conversation--map-records
                  (lambda (mapped-pathname function &key (start-position 0))
                    (multiple-value-prog1
                        (funcall map-records-function
                                 mapped-pathname
                                 function
                                 :start-position start-position)
                      (log-append pathname
                                  (list :message :seq 2 :time 1010 :role ':user
                                        :content "second"))))))
                (lambda ()
                  (conversation-picker-metadata-scan pathname))))
              "a scan racing an append refuses to publish stale picker metadata"))
           (let ((metadata (conversation-picker-metadata-find pathname)))
             (test-assert
              (and metadata
                   (= (conversation-picker-metadata-user-turn-count metadata) 2)
                   (string= (conversation-picker-metadata-preview metadata)
                            "second"))
               "a later picker scan rebuilds metadata from the complete log"))
           (let* ((conversation
                   (conversation-create configuration
                                        :identifier "metadata-rotation"))
                  (identity (conversation-pathname conversation))
                  (map-records-function
                   (symbol-function 'conversation--map-records))
                  (file-identity-function
                   (symbol-function 'conversation--file-identity))
                  (rotated-p nil))
             (conversation-append-user-message conversation "before rotation")
             (test-assert
              (null
               (test-call-with-function-replacements
                (list
                 (list
                  'conversation--file-identity
                  (lambda (mapped-pathname)
                    (multiple-value-bind (segment size write-date)
                        (funcall file-identity-function mapped-pathname)
                      (declare (ignore size write-date))
                      (values segment 100 200))))
                 (list
                  'conversation-picker-revision-read
                  (lambda (mapped-pathname)
                    (declare (ignore mapped-pathname))
                    0))
                 (list
                  'conversation--map-records
                  (lambda (mapped-pathname function &key (start-position 0))
                    (multiple-value-prog1
                        (funcall map-records-function
                                 mapped-pathname
                                 function
                                 :start-position start-position)
                      (unless rotated-p
                        (setf rotated-p t)
                        (conversation-append-summary
                         conversation "rotation checkpoint"))))))
                (lambda ()
                  (conversation-picker-metadata-scan identity))))
              "same-size same-date chunk rotation rejects stale picker metadata")
             (let ((metadata (conversation-picker-metadata-find identity)))
               (test-assert
                (and metadata
                     (string=
                      (conversation-picker-metadata-source-segment metadata)
                      (namestring (conversation-log-pathname conversation))))
                "rebuilt picker metadata records the exact active segment"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-picker-search () null)
(defun test-conversation-picker-search ()
  "Test durable message-only search indexes, rebuilding, and scan races."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "search-index"))
         (pathname (conversation-pathname conversation))
         (search-pathname (conversation-picker-search-pathname pathname))
         (expected (list "user needle" "assistant needle")))
    (labels ((find-with-scan-count ()
               "Find the index and return how many source scans it required."
               (let ((count 0)
                     (map-records-function
                       (symbol-function 'conversation--map-records)))
                 (values
                  (test-call-with-function-replacements
                   (list
                    (list
                     'conversation--map-records
                     (lambda (mapped-pathname function &key (start-position 0))
                       (incf count)
                       (funcall map-records-function
                                mapped-pathname
                                function
                                :start-position start-position))))
                   (lambda ()
                     (conversation-picker-search-find pathname)))
                  count))))
      (unwind-protect
           (progn
             (conversation-append-user-message conversation "user needle")
             (conversation-append-provider-item
              conversation
              (json-object
               "type" "message"
               "role" "assistant"
               "content" (json-array
                          (json-object
                           "type" "output_text"
                           "text" "assistant needle"))))
             (let ((message-revision
                     (conversation-picker-search-revision-read pathname)))
               (conversation-append-provider-item
                conversation
                (json-object
                 "type" "reasoning"
                 "summary" (json-array
                            (json-object
                             "type" "summary_text"
                             "text" "reasoning secret"))))
               (conversation-append-record
                conversation
                (list :tool-result :status ':ok :output "tool secret"))
               (conversation-append-summary conversation "summary secret")
               (test-assert
                (= (conversation-picker-search-revision-read pathname)
                   message-revision)
                "non-message records do not advance the search revision"))
             (test-assert
              (null (conversation-picker-search-read pathname))
              "searchable appends leave the sidecar stale for on-demand rebuilding")
             (conversation-picker-search-close conversation)
             (let ((index (conversation-picker-search-read pathname)))
               (test-assert
                (and index
                     (equal (conversation-picker-search-index-messages index)
                            expected)
                     (string=
                      (conversation-picker-search-index-text index)
                      "user needle
assistant needle"))
                "closing publishes only chronological visible messages"))
             (multiple-value-bind (index scan-count)
                 (find-with-scan-count)
               (test-assert
                (and index (zerop scan-count))
                "a valid search sidecar avoids scanning the conversation log"))
             (let ((loaded (conversation-load pathname)))
               (test-assert
                (= (conversation-picker-search-message-count loaded) 2)
                "newest-chunk replay restores the searchable message count")
               (conversation-append-user-message loaded "later user")
               (setf expected (append expected (list "later user")))
               (test-assert
                (null (conversation-picker-search-read pathname))
                "an append invalidates the published search sidecar")
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (plusp scan-count)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a search after appends rebuilds old and new text on demand"))
               (delete-file search-pathname)
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (= scan-count 2)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a missing search sidecar rebuilds from both chunks once"))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index (zerop scan-count))
                  "the rebuilt search sidecar serves later reads without scanning"))
               (snapshot-write search-pathname '(:malformed))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (= scan-count 2)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a malformed search sidecar rebuilds both chunks once"))
               (with-open-file (stream search-pathname
                                       :direction ':output
                                       :if-exists ':supersede
                                       :external-format ':utf-8)
                 (write-string "(:conversation-picker-search" stream))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (= scan-count 2)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a truncated search sidecar rebuilds both chunks once"))
               (let* ((metadata (conversation-picker-metadata-read pathname))
                      (revision
                       (conversation-picker-search-revision-read pathname))
                      (message-count
                       (conversation-picker-metadata-search-message-count metadata)))
                 (conversation-picker-search-write
                  pathname
                  (make-instance
                   'conversation-picker-search-index
                   :source-revision (1- revision)
                   :message-count message-count
                   :messages (make-list message-count :initial-element "stale"))))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (= scan-count 2)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a stale search revision rebuilds both chunks once"))
               (let ((map-records-function
                      (symbol-function 'conversation--map-records))
                     (appended-p nil))
                 (test-assert
                  (null
                   (test-call-with-function-replacements
                    (list
                     (list
                      'conversation--map-records
                      (lambda (mapped-pathname function
                               &key (start-position 0))
                        (multiple-value-prog1
                            (funcall map-records-function
                                     mapped-pathname
                                     function
                                     :start-position start-position)
                          (unless appended-p
                            (setf appended-p t)
                            (conversation-append-user-message
                             loaded "racing message"))))))
                    (lambda ()
                      (conversation-picker-search-scan pathname))))
                  "a searchable append racing a scan rejects the stale snapshot"))
               (setf expected (append expected (list "racing message")))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (plusp scan-count)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "a search after the racing append rebuilds the current corpus"))
               (let ((log-append-function (symbol-function 'log-append))
                     (pre-append-index nil)
                     (append-failed-p nil))
                 (handler-case
                     (test-call-with-function-replacements
                      (list
                       (list
                        'log-append
                        (lambda (&rest arguments)
                          (setf pre-append-index
                                (conversation-picker-search-find pathname))
                          (apply log-append-function arguments)
                          (error "simulated failure after durable log append"))))
                      (lambda ()
                        (conversation-append-user-message
                         loaded "post-append crash message")))
                   (conversation-invariant-error ()
                     (setf append-failed-p t)))
                 (test-assert
                  (and append-failed-p
                       pre-append-index
                       (equal
                        (conversation-picker-search-index-messages
                         pre-append-index)
                        expected))
                  "a pre-append scan can publish only the old searchable corpus")
                 (let ((stale-index
                        (conversation-picker-search-index-from-record
                         (snapshot-read search-pathname))))
                   (test-assert
                    (and stale-index
                         (equal
                          (conversation-picker-search-index-messages stale-index)
                          expected))
                    "the simulated crash leaves the pre-append sidecar on disk"))
                 (test-assert
                  (null (conversation-picker-search-read pathname))
                  "stale picker metadata rejects the stale post-append sidecar")
                 (test-assert
                  (conversation-picker-metadata-find pathname)
                  "picker metadata can rebuild independently after the crash")
                 (test-assert
                  (null (conversation-picker-search-read pathname))
                  "current metadata still rejects a sidecar from the old file")
                 (setf expected
                       (append expected (list "post-append crash message")))
                 (let ((rebuilt (conversation-picker-search-find pathname)))
                   (test-assert
                    (and rebuilt
                         (equal
                          (conversation-picker-search-index-messages rebuilt)
                          expected))
                    "a later find rebuilds search after the post-append crash")))
               (delete-file search-pathname)
               (with-open-file (stream (conversation-log-pathname loaded)
                                       :direction ':output
                                       :if-exists ':append
                                       :external-format ':utf-8)
                 (write-string "(:provider-item" stream))
               (multiple-value-bind (index scan-count)
                   (find-with-scan-count)
                 (test-assert
                  (and index
                       (= scan-count 4)
                       (equal
                        (conversation-picker-search-index-messages index)
                        expected))
                  "an incomplete active tail rebuilds metadata and search across both chunks"))
               (let ((reloaded (conversation-load pathname)))
                 (test-assert
                  (and (conversation-incomplete-tail-p reloaded)
                       (= (conversation-picker-search-message-count reloaded)
                          (length expected)))
                  "newest-chunk replay retains cumulative search state before tail repair")
                 (conversation-append-user-message reloaded "after repair")
                 (setf expected (append expected (list "after repair")))
                 (test-assert
                  (and (not (conversation-incomplete-tail-p reloaded))
                       (equal
                        (conversation-picker-search-index-messages
                         (conversation-picker-search-find pathname))
                        expected))
                  "tail repair leaves the complete history searchable on demand"))))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))
  nil)


(-> test-conversation-deletion () null)
(defun test-conversation-deletion ()
  "Test deletion ownership checks and private artifact cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "delete-me"))
         (pathname (conversation-pathname conversation))
         (sidecars (conversation-picker-sidecar-pathnames pathname))
         (image-root
           (merge-pathnames "conversation-images/delete-me/"
                            (configuration-data-root configuration)))
         (task-root
           (merge-pathnames "tasks/delete-me/"
                            (configuration-data-root configuration)))
         (lease nil))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "temporary")
           (conversation-picker-search-close conversation)
           (test-assert (every #'probe-file sidecars)
                        "appending and closing publish all picker sidecars")
           (snapshot-write (merge-pathnames "image.sexp" image-root)
                           '(:image))
           (snapshot-write (merge-pathnames "task/result.sexp" task-root)
                           '(:task))
           (setf lease (conversation-lease-acquire configuration "delete-me"))
           (test-assert
            (handler-case
                (progn
                  (conversation-delete configuration "delete-me")
                  nil)
              (conversation-in-use ()
                t))
            "deletion refuses a conversation owned by another live lease")
           (test-assert (conversation-storage-occupied-p pathname)
                        "refused deletion preserves conversation storage")
           (conversation-lease-release lease)
           (setf lease nil)
           (test-assert (equal (conversation-delete configuration "delete-me")
                               pathname)
                        "deletion returns the removed conversation pathname")
           (test-assert (not (conversation-storage-occupied-p pathname))
                        "deletion removes conversation storage")
           (test-assert (notany #'probe-file sidecars)
                        "deletion removes all picker sidecars")
           (test-assert (not (probe-file image-root))
                        "deletion removes private image artifacts")
           (test-assert (not (probe-file task-root))
                        "deletion removes child task artifacts")
           (test-assert
            (handler-case
                (progn
                  (conversation-delete configuration "delete-me")
                  nil)
              (conversation-error ()
                t))
            "deleting a missing conversation signals a conversation error")
           (let* ((failed
                    (conversation-create
                     configuration
                     :identifier "cleanup-failure"))
                  (failed-pathname (conversation-pathname failed))
                  (failed-sidecars
                    (conversation-picker-sidecar-pathnames failed-pathname))
                  (failed-image-root
                    (merge-pathnames
                     "conversation-images/cleanup-failure/"
                     (configuration-data-root configuration)))
                  (reported nil))
             (conversation-append-user-message failed "temporary")
             (snapshot-write
              (merge-pathnames "image.sexp" failed-image-root)
              '(:image))
             (let ((*conversation-delete-directory-tree-function*
                     (lambda (root &key validate if-does-not-exist)
                       (declare (ignore root validate if-does-not-exist))
                       (error "simulated cleanup failure"))))
               (handler-case
                   (conversation-delete configuration "cleanup-failure")
                 (conversation-invariant-error (condition)
                   (setf reported (format nil "~A" condition)))))
             (test-assert
              (and reported
                   (search "was deleted, but private artifacts remain" reported))
              "artifact cleanup failures report the committed deletion")
             (test-assert (not (probe-file failed-pathname))
                          "cleanup failure cannot leave a broken resumable conversation")
              (test-assert
               (notany #'probe-file failed-sidecars)
               "committed deletion removes sidecars before artifact cleanup")
             (test-assert (probe-file failed-image-root)
                          "cleanup failure leaves undeleted artifacts recoverable")))
      (when lease
        (conversation-lease-release lease))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-titles () null)
(defun test-conversation-titles ()
  "Test initial prompt titles, generated replacements, picker caches, and replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation
                 (conversation-create configuration :identifier "titles")))
           (test-assert
            (string= (conversation-title-derive
                      "add titles to autolith sessions. Make them automatic.")
                     "Add titles to autolith sessions")
            "initial titles use the leading prompt sentence")
           (test-assert
            (and (string= (conversation-title-normalize "# Title: automatic session titles")
                          "Automatic session titles")
                 (string= (conversation-title-normalize "**Title:** automatic session titles")
                          "Automatic session titles")
                 (null (conversation-title-normalize "**Title:**")))
            "title normalization removes combined labels and lightweight markup")
           (dolist (case
                    '(("automatic session titles." "Automatic session titles")
                      ("Title" nil)
                      ("one two three four five six seven eight nine" nil)
                      ("{\"title\":\"Automatic session titles\"}" nil)
                      ("# Automatic session titles" nil)
                      ("Title: Automatic session titles" nil)
                      ("Here is your automatic session title" nil)
                      ("Automatic\nsession titles" nil)))
             (destructuring-bind (input expected) case
               (test-assert
                (equal (conversation-title-generated-normalize input) expected)
                (format nil "generated title validation maps ~S to ~S"
                        input expected))))
           (let* ((image-pathname (merge-pathnames "title-image.png" root))
                  (image-conversation
                    (conversation-create configuration :identifier "image-title")))
             (test-conversation--write-tiny-png image-pathname)
             (conversation-append-user-message
              image-conversation
              (user-message-input-create
               :image-pathnames (list image-pathname)))
             (test-assert
              (and (string= (conversation-title image-conversation)
                            "Image attachment")
                   (equal
                    (conversation-picker-search-index-messages
                     (conversation-picker-search-find
                      (conversation-pathname image-conversation)))
                    '("Image attachment")))
              "an image-only first turn gets a local title and provider context"))
           (conversation-append-user-message
            conversation
            "add titles to autolith sessions. Make them automatic.")
           (test-assert
            (and (string= (conversation-title conversation)
                          "Add titles to autolith sessions")
                 (eq (conversation-title-source conversation) ':initial))
            "the first user prompt immediately names a conversation")
           (let* ((pathname (conversation-pathname conversation))
                  (header (conversation-peek-header pathname))
                  (metadata (conversation-picker-metadata-read pathname)))
             (test-assert
              (string= (getf (rest header) :title)
                       "Add titles to autolith sessions")
              "the initial title is published in the first chunk header")
             (test-assert
              (and metadata
                   (string= (conversation-picker-metadata-title metadata)
                            "Add titles to autolith sessions"))
              "picker metadata publishes the initial title"))
           (test-assert
            (not (conversation-title-refresh-due-p conversation))
            "one user turn does not request a generated title")
           (conversation-append-user-message conversation "show it in resume lists")
           (test-assert
            (not (conversation-title-refresh-due-p conversation))
            "two user turns retain the initial title")
           (conversation-append-user-message conversation "persist the better title")
           (test-assert
            (conversation-title-refresh-due-p conversation)
            "three user turns request the generated title")
           (conversation-set-title conversation
                                   "automatic session titles!"
                                   :source ':generated)
           (test-assert
            (and (string= (conversation-title conversation)
                          "Automatic session titles")
                 (eq (conversation-title-source conversation) ':generated)
                 (not (conversation-title-refresh-due-p conversation)))
            "a generated title replaces the initial title once")
           (let ((next-sequence (conversation-next-sequence conversation)))
             (conversation-set-title conversation
                                     "a second generated title"
                                     :source ':generated)
             (test-assert
              (and (= (conversation-next-sequence conversation) next-sequence)
                   (string= (conversation-title conversation)
                            "Automatic session titles"))
              "a generated title cannot be replaced automatically a second time"))
           (test-assert
            (handler-case
                (progn
                  (conversation-set-title conversation
                                          "restored initial title"
                                          :source ':initial)
                  nil)
              (conversation-invariant-error ()
                t))
            "a generated title cannot transition back to an initial title")
           (let ((metadata
                   (conversation-picker-metadata-read
                    (conversation-pathname conversation))))
             (test-assert
              (and metadata
                   (string= (conversation-picker-metadata-title metadata)
                            "Automatic session titles"))
              "picker metadata follows generated title records"))
           (let ((reloaded (conversation-load-by-id configuration "titles")))
             (test-assert
              (and (string= (conversation-title reloaded)
                            "Automatic session titles")
                   (eq (conversation-title-source reloaded) ':generated))
              "conversation replay restores the latest generated title"))
           (let ((duplicate
                   (conversation-create configuration
                                        :identifier "duplicate-generated-title")))
             (conversation-append-user-message duplicate "duplicate generated title")
             (conversation-set-title duplicate
                                     "first generated title"
                                     :source ':generated)
             (conversation-append-record
              duplicate
              (list :title
                    :value "Second generated title"
                    :source ':generated))
             (test-assert
              (handler-case
                  (progn
                    (conversation-load-by-id configuration
                                             "duplicate-generated-title")
                    nil)
                (conversation-invariant-error ()
                  t))
              "replay rejects a second generated title record"))
           (let ((automatic
                   (conversation-create configuration
                                        :identifier "automatic-title-turn")))
             (conversation-append-user-message automatic
                                               "continue automatically"
                                               :automatic-p t)
             (test-assert
              (and (zerop (conversation-user-turn-count automatic))
                   (null (conversation-title automatic)))
              "automatic continuation prompts neither count nor name a session")
             (conversation-append-user-message automatic "real user task")
             (test-assert
              (and (= (conversation-user-turn-count automatic) 1)
                   (string= (conversation-title automatic) "Real user task"))
              "the first actual user turn sets the immediate session title"))
           (let* ((image-pathname (merge-pathnames "automatic-title.png" root))
                  (image-automatic
                    (conversation-create
                     configuration
                     :identifier "image-automatic-title-turn")))
             (test-conversation--write-tiny-png image-pathname)
             (conversation-append-user-message
              image-automatic
              (user-message-input-create
               :text ""
               :image-pathnames (list (truename image-pathname))))
             (conversation-append-user-message
              image-automatic
              "continue automatically"
              :automatic-p t)
             (test-assert
              (and (= (conversation-user-turn-count image-automatic) 1)
                   (string= (conversation-title image-automatic)
                            "Image attachment"))
              "an image-only actual turn keeps its local title through continuation")
             (let ((reloaded
                     (conversation-load-by-id
                      configuration "image-automatic-title-turn")))
               (test-assert
                (and (= (conversation-user-turn-count reloaded) 1)
                     (string= (conversation-title reloaded)
                              "Image attachment"))
                "replay preserves the image title and excludes continuation text")))
           (let* ((published
                    (conversation-create configuration
                                         :identifier "published-title"))
                  (log-append-function (symbol-function 'log-append)))
             (conversation-append-user-message published "publish title safely")
             (let ((title-sequence (conversation-next-sequence published)))
               (test-call-with-function-replacements
                (list
                 (list 'log-append
                       (lambda (&rest arguments)
                         (apply log-append-function arguments)
                         (error "simulated failure after title publication"))))
                (lambda ()
                  (conversation-set-title published
                                          "durable generated title"
                                          :source ':generated)))
               (test-assert
                (= (conversation-next-sequence published) (1+ title-sequence))
                "a published title survives an ambiguous append failure"))
             (conversation-append-user-message published "continue after title")
             (let ((reloaded
                     (conversation-load-by-id configuration "published-title")))
               (test-assert
                (and (string= (conversation-title reloaded)
                              "Durable generated title")
                     (= (conversation-user-turn-count reloaded) 2))
                "later records remain contiguous after an ambiguous title append")))
           (let* ((header-only
                    (conversation-create configuration
                                         :identifier "header-title")))
             (conversation-append-user-message header-only "initial header title")
             (conversation-set-title header-only
                                     "generated header title"
                                     :source ':generated)
             (let ((old-segment (conversation-log-pathname header-only)))
               (conversation-append-summary header-only "compacted title context")
               (delete-file old-segment))
             (let ((metadata
                     (conversation-picker-metadata-scan
                      (conversation-pathname header-only))))
               (test-assert
                (and metadata
                     (string= (conversation-picker-metadata-title metadata)
                              "Generated header title"))
                "picker rebuilds recover titles from self-contained chunk headers"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-conversation-cross-family-reasoning () null)
(defun test-conversation-cross-family-reasoning ()
  "Test that reasoning replay is confined to the family that produced it."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "families")))
           (conversation-append-user-message conversation "start on gpt")
           (conversation-append-provider-item
            conversation
            (json-object "type" "reasoning"
                         "encrypted_content" "codex-private-blob"
                         "summary" (json-array)))
           (conversation-set-model-selection conversation "grok-4.5" "high")
           (conversation-append-user-message conversation "continue on grok")
           (conversation-append-provider-item
            conversation
            (json-object "type" "reasoning"
                         "encrypted_content" "grok-private-blob"
                         "summary" (json-array)))
           (dolist (case '((:codex "codex-private-blob" "grok-private-blob")
                           (:grok "grok-private-blob" "codex-private-blob")))
             (destructuring-bind (family kept dropped) case
               (let ((blobs
                       (loop for item
                               in (conversation-input-items-for-family
                                   conversation family)
                             when (reasoning-item-p item)
                               collect (json-get item "encrypted_content"))))
                 (test-assert
                  (and (member kept blobs :test #'equal)
                       (not (member dropped blobs :test #'equal)))
                  (format nil "~(~A~) requests replay only their own reasoning"
                          family)))))
           (test-assert
            (= (length (conversation-input-items-for-family conversation
                                                            ':grok))
               (1- (length (conversation-input-items-for-request
                            conversation))))
            "only the foreign reasoning item is withheld from a request")
           (let ((reloaded (conversation-load-by-id configuration "families")))
             (test-assert
              (equal (loop for item
                             in (conversation-input-items-for-family reloaded
                                                                     ':grok)
                           when (reasoning-item-p item)
                             collect (json-get item "encrypted_content"))
                     '("grok-private-blob"))
              "replay restores each item's producing family")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
