(in-package #:autolith)

;;;; -- Read-only Conversation Replay Tests --

(-> conversation-replay-tests--session () conversation-replay-session)
(defun conversation-replay-tests--session ()
  "Return deterministic replay navigation state spanning two local dates."
  (let* ((first-time  (encode-universal-time 0 0 9 27 8 2026))
         (second-time (encode-universal-time 0 30 10 28 8 2026))
         (conversation
           (make-instance 'conversation
                          :identifier       "replay-test"
                          :prompt-cache-key "replay-test"
                          :pathname         #p"/tmp/replay-test.sexp"
                          :log-pathname     #p"/tmp/replay-test.sexp"
                          :persisted-p      nil
                          :created-at       first-time
                          :next-sequence    1
                          :input-items      nil))
         (records
           (vector
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 10 :time first-time
                                         :role ':user :content "first")
                           :turn   1)
            (make-instance 'conversation-replay-record
                           :record (list ':assistant :seq 11 :time (1+ first-time)
                                         :content "answer")
                           :turn   1)
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 20 :time second-time
                                         :role ':user :content "second")
                           :turn   2))))
    (make-instance 'conversation-replay-session
                   :conversation conversation
                   :records      records)))

(-> test-conversation-replay-navigation () null)
(defun test-conversation-replay-navigation ()
  "Test record stepping and turn, sequence, date, and time selection."
  (let ((session (conversation-replay-tests--session)))
    (conversation-replay-select-turn session 2)
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        20)
     "turn selection lands on the turn's first replay record")
    (conversation-replay-select-sequence session 11)
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        11)
     "sequence selection lands on the first record at or after the target")
    (conversation-replay-select-date session "2026-08-28")
    (test-assert
     (= (conversation-replay-record-turn
         (conversation-replay--current-record session))
        2)
     "date selection uses local record dates")
    (conversation-replay-select-time session "10:30")
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        20)
     "bare time selection uses the selected record's local date")
    (conversation-replay-move session -2)
    (let ((output (make-string-output-stream)))
      (test-assert
       (conversation-replay-execute-command session "next" output)
       "next continues the replay session")
      (test-assert
       (search "sequence 11" (get-output-stream-string output))
       "next renders the newly selected replay record")
      (test-assert
       (not (conversation-replay-execute-command session "quit" output))
       "quit terminates the replay session")))
  nil)

(-> test-conversation-replay-projection () null)
(defun test-conversation-replay-projection ()
  "Test provider replay projection keeps visible content and drops opaque payloads."
  (let* ((wire-json
           (json-encode
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (vector (json-object "type" "output_text"
                                             "text" "projected answer"))
             "opaque" (make-string 10000 :initial-element #\x))))
         (record
           (list ':provider-item :seq 7 :time 100 :wire-json wire-json))
         (projected (conversation-replay-project-record record)))
    (test-assert
     (and (eq (first projected) ':assistant)
          (string= (getf (rest projected) :content) "projected answer")
          (< (length (prin1-to-string projected)) 200))
     "provider replay stores only bounded visible assistant content"))
  (test-assert
   (null
    (conversation-replay-project-record
     (list ':native-compaction
           :seq       8
           :time      101
           :wire-json (make-string 10000 :initial-element #\x))))
   "opaque native compaction payloads are omitted from replay projection")
  (let ((projected
          (conversation-replay-project-record
           (list ':native-compaction
                 :seq         9
                 :time        101
                 :through-seq 8
                 :family      ':codex
                 :wire-json   (make-string 10000 :initial-element #\x)
                 :summary     "portable handoff"))))
    (test-assert
     (and (equal projected
                 '(:summary :seq 9 :time 101 :through-seq 8
                   :content "portable handoff"))
          (< (length (prin1-to-string projected)) 200))
     "native compaction replays as its portable summary without the opaque item"))
  (let* ((record
           (list ':turn-aborted
                 :seq               12
                 :time              102
                 :turn-start-seq    8
                 :last-complete-seq 11
                 :reason            ':agent-loop
                 :condition-type    "AGENT-LOOP-ERROR"
                 :message           "provider response was malformed"
                 :request-number    2))
         (projected (conversation-replay-project-record record))
         (output    (make-string-output-stream)))
    (conversation-replay-write-record-body projected output)
    (let ((rendered (get-output-stream-string output)))
      (test-assert
       (and (equal projected record)
            (search "durable prefix 8-11 aborted [agent-loop]" rendered)
            (search "provider request 2" rendered)
            (search "provider response was malformed" rendered))
       "replay exposes the recoverable prefix and diagnosis of an aborted turn")))
  nil)

(-> test-conversation-replay-storage () null)
(defun test-conversation-replay-storage ()
  "Test standalone replay reads a saved conversation without mutating its files."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "read-only-replay")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect me")
           (let* ((pathname   (conversation-log-pathname conversation))
                  (write-date (file-write-date pathname))
                  (length     (with-open-file (stream pathname)
                                (file-length stream)))
                  (output     (make-string-output-stream)))
             (conversation-replay-run configuration
                                      "read-only-replay"
                                      nil
                                      :output        output
                                      :interactive-p nil)
             (test-assert
              (and (= write-date (file-write-date pathname))
                   (= length
                      (with-open-file (stream pathname)
                        (file-length stream)))
                   (search "inspect me" (get-output-stream-string output)))
              "replay reads and renders a conversation without writing storage")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-replay-command-line () null)
(defun test-conversation-replay-command-line ()
  "Test the top-level replay command forwards its identifier and selector."
  (let ((observed nil))
    (test-call-with-function-replacements
     (list
      (list 'conversation-replay-run
            (lambda (configuration identifier selection &rest arguments)
              (declare (ignore arguments))
              (setf observed
                    (list (config :immutable-p configuration)
                          identifier
                          selection)))))
     (lambda ()
       (main-dispatch '("replay" "abc123" "turn" "4"))))
    (test-assert
     (equal observed '(t "abc123" ("turn" "4")))
     "the replay command starts an immutable selected inspection"))
  nil)

(-> conversation-replay-tests--raw-records (conversation) list)
(defun conversation-replay-tests--raw-records (conversation)
  "Collect a small fixture's exact records for durable-prefix assertions."
  (let ((records nil))
    (conversation-map-records conversation (lambda (record) (push record records)))
    (nreverse records)))

(-> test-conversation-fork-storage () null)
(defun test-conversation-fork-storage ()
  "Test historical forks preserve state, provenance, artifacts, and source files."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (first-image   (test-conversation--write-tiny-png
                         (merge-pathnames "first.png" root)))
         (second-image  (test-conversation--write-tiny-png
                         (merge-pathnames "second.png" root)))
         (source        (conversation-create configuration
                                             :identifier       "fork-source"
                                             :prompt-cache-key "source-cache")))
    (unwind-protect
         (progn
           (setf (conversation-title source) "Historical title"
                 (conversation-title-source source) ':initial
                 (conversation-model source) "historical-model"
                 (conversation-model-provider source) "historical-provider"
                 (conversation-reasoning-effort source) "high")
           (conversation-append-user-message
            source (user-message-input-create :text "first" :image-pathnames (list first-image)))
           (conversation-append-provider-item
            source
            (json-object "type" "message" "role" "assistant"
                         "content" (vector (json-object "type" "output_text"
                                                        "text" "answer"))))
           (conversation-append-record
            source (list :summary :content "compact" :through-seq 2))
           (conversation-append-provider-item
            source
            (json-object "type" "function_call"
                         "call_id" "call-1"
                         "name" "demo"
                         "arguments" "{}"))
           (conversation-append-tool-result
            source "call-1" :tool-name "demo" :output "done" :success-p t)
           (conversation-append-user-message
            source (user-message-input-create :text "second" :image-pathnames (list second-image)))
           (let* ((source-records (conversation-replay-tests--raw-records source))
                  (source-pathnames
                    (conversation-storage-pathnames
                     (conversation-pathname source)))
                  (source-bytes
                    (mapcar #'uiop:read-file-string source-pathnames))
                  (fork
                    (conversation-fork configuration "fork-source"
                                       :selection  '("sequence" "5")
                                       :identifier "fork-target"))
                  (records        (conversation-replay-tests--raw-records fork))
                  (provenance     (first (last records)))
                  (artifacts
                    (uiop:directory-files
                     (conversation-image-artifact-root fork))))
             (test-assert
              (and (string= (conversation-identifier fork) "fork-target")
                   (not (string= (conversation-prompt-cache-key fork)
                                 "source-cache"))
                   (= (length records) 6)
                   (equal (subseq records 0 5)
                          (subseq source-records 0 5))
                   (eq (first provenance) :fork)
                   (string= (getf (rest provenance) :source-id) "fork-source")
                   (= (getf (rest provenance) :source-sequence) 5)
                   (= (length (conversation-input-items fork)) 3)
                   (= (conversation-user-turn-count fork) 1)
                   (string= (conversation-title fork) "Historical title")
                   (string= (conversation-model fork) "historical-model")
                   (string= (conversation-model-provider fork) "historical-provider")
                   (string= (conversation-reasoning-effort fork) "high")
                   (= (length artifacts) 1)
                   (equal source-bytes
                          (mapcar #'uiop:read-file-string source-pathnames)))
              "forks retain the selected chunked prefix, state, provenance, one referenced artifact, and immutable source"))
           (dolist (arguments '(("sequence" "99") ("sequence" "2" "extra")))
             (test-assert
              (handler-case
                  (progn
                    (conversation-fork configuration "fork-source"
                                       :selection  arguments
                                       :identifier "rejected-target")
                    nil)
                (error ()
                  (not (conversation-storage-occupied-p
                        (conversation-pathname-for-id configuration
                                                      "rejected-target")))))
              "invalid or nonexistent fork heads leave no target")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-fork-rejections () null)
(defun test-conversation-fork-rejections ()
  "Test generated identities and gapped source rejection without partial targets."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((source (conversation-create configuration
                                              :identifier       "generated-source"
                                              :prompt-cache-key "source-cache")))
             (conversation-append-user-message source "one")
             (let ((fork (conversation-fork configuration "generated-source")))
               (test-assert
                (and (not (string= (conversation-identifier fork)
                                   "generated-source"))
                     (not (string= (conversation-prompt-cache-key fork)
                                   "source-cache")))
                "a fork without --id receives fresh conversation and cache identities")))
           (let ((source (conversation-create configuration
                                              :identifier "gapped-source")))
             (conversation-append-user-message source "one")
             (log-append
              (conversation-log-pathname source)
              (list :message :seq 3 :time (get-universal-time)
                    :role :user :content "gap"
                    :wire-json (json-encode (user-message-item "gap"))))
             (test-assert
              (handler-case
                  (progn
                    (conversation-fork configuration "gapped-source"
                                       :identifier "gapped-target")
                    nil)
                (conversation-invariant-error ()
                  (not (conversation-storage-occupied-p
                        (conversation-pathname-for-id configuration
                                                      "gapped-target")))))
              "a gapped source head is rejected without a partial target")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-fork-chunks-and-selectors () null)
(defun test-conversation-fork-chunks-and-selectors ()
  "Test selector parity and resumable state at historical chunk boundaries."
  (with-test-configuration (configuration)
    (let* ((source      (conversation-create configuration :identifier "chunk-source"))
           (first-time  (encode-universal-time 0 0 9 27 8 2026))
           (second-time (encode-universal-time 0 30 10 28 8 2026)))
      (setf (conversation-title source) "Initial title"
            (conversation-title-source source) ':initial)
      (conversation-append-user-message source "one")
      (conversation-append-provider-item
       source (json-object "type" "message" "role" "assistant"
                           "content" (vector (json-object "type" "output_text"
                                                          "text" "answer"))))
      (setf (conversation-title source) "Middle title")
      (conversation-append-summary source "first checkpoint")
      (conversation-append-user-message source "two")
      (setf (conversation-title source) "Latest title")
      (conversation-append-summary source "second checkpoint")
      (conversation-append-user-message source "three")
      (dolist (pathname (conversation-storage-pathnames (conversation-pathname source)))
        (let* ((forms  (conversation--read-records pathname))
               (header (rest (first forms)))
               (start  (getf header :chunk-start-sequence)))
          (when (> start 1)
            (setf (getf header :working-seconds) 37
                  (getf header :last-activity-at)
                  (if (= start 3) first-time second-time)))
          (dolist (record (rest forms))
            (setf (getf (rest record) :time)
                  (if (<= (getf (rest record) :seq) 3) first-time second-time)))
          (log-write pathname forms)))
      (loop for head in '(2 4 6)
            for chunks from 1
            for title in '("Initial title" "Middle title" "Latest title")
            for fork = (conversation-fork configuration "chunk-source"
                                          :selection (list "sequence" (write-to-string head)))
            do (test-assert
                (and (= (length (conversation-storage-pathnames
                                 (conversation-pathname fork))) chunks)
                     (= (conversation-user-turn-count fork) chunks)
                     (= (conversation-working-seconds fork) (if (= chunks 1) 0 37))
                     (string= (conversation-title fork) title)
                     (= (conversation-next-sequence fork) (+ head 2)))
                (format nil "fork head ~D: chunks ~D, turns ~D, working ~D, title ~S, next ~D"
                        head (length (conversation-storage-pathnames (conversation-pathname fork)))
                        (conversation-user-turn-count fork) (conversation-working-seconds fork)
                        (conversation-title fork) (conversation-next-sequence fork))))
      (dolist (selection '(("sequence" "0") ("sequence" "3")
                           ("turn" "1") ("turn" "2") ("turn" "3")
                           ("date" "2026-08-27") ("date" "2026-08-28")
                           ("time" "09:00") ("time" "2026-08-28T10:30:00")
                           ("time" "2026-08-28T00:00:00Z")))
        (let* ((session    (conversation-replay-create source))
               (entry      (conversation-replay-apply-selection session selection))
               (head       (conversation-replay--record-sequence entry))
               (fork       (conversation-fork configuration "chunk-source" :selection selection))
               (provenance (first (last (conversation-replay-tests--raw-records fork)))))
          (test-assert
           (= (getf (rest provenance) :source-sequence) head)
           "streaming forks and replay select the same inclusive head")))))
  nil)

(-> test-conversation-fork-source-tails () null)
(defun test-conversation-fork-source-tails ()
  "Test historical heads ignore later corruption and newest forks tolerate torn tails."
  (with-test-configuration (configuration)
    (dolist (mode '(:torn :body :header))
      (let* ((identifier (format nil "tail-~(~A~)" mode))
             (source     (conversation-create configuration :identifier identifier)))
        (conversation-append-user-message source "one")
        (conversation-append-summary source "checkpoint")
        (conversation-append-user-message source "two")
        (let ((active (conversation-log-pathname source)))
          (with-open-file (stream active :direction       ':output
                                         :if-exists       (if (eq mode ':header) ':supersede ':append)
                                         :external-format ':utf-8)
            (write-string (if (eq mode ':torn) "(:message" ")") stream))
          (let* ((before (uiop:read-file-string active))
                 (early  (conversation-fork configuration identifier
                                            :selection '("sequence" "1"))))
            (test-assert
             (= (length (conversation-replay-tests--raw-records early)) 2)
             "an earlier fork never reads later malformed bodies or headers")
            (if (eq mode ':torn)
                (let ((latest (conversation-fork configuration identifier)))
                  (test-assert
                   (= (length (conversation-replay-tests--raw-records latest)) 4)
                   "the newest fork excludes an incomplete final form"))
                (test-assert
                 (handler-case
                     (progn (conversation-fork configuration identifier) nil)
                   (conversation-invariant-error () t))
                 "a newest fork rejects malformed complete source data"))
            (test-assert (string= before (uiop:read-file-string active))
                         "forking never repairs source bytes"))))))
  nil)

(-> test-conversation-fork-copy-failures () null)
(defun test-conversation-fork-copy-failures ()
  "Test owned-prefix cleanup after write or attachment failures and target collisions."
  (with-test-configuration (configuration root)
    (let* ((image  (test-conversation--write-tiny-png (merge-pathnames "image.png" root)))
           (source (conversation-create configuration :identifier "failure-source")))
      (conversation-append-user-message
       source (user-message-input-create :text "one" :image-pathnames (list image)))
      (conversation-append-summary source "checkpoint")
      (conversation-append-user-message source "two")
      (dolist (failure '(:initial :append :attachment))
        (let* ((identifier (format nil "failed-~(~A~)" failure))
               (identity   (conversation-pathname-for-id configuration identifier))
               (target     (conversation-create configuration :identifier identifier)))
          (test-call-with-function-replacements
           (ecase failure
             (:initial
              (list (list 'sexp-store:log-write
                          (lambda (pathname forms &rest arguments)
                            (declare (ignore forms arguments))
                            (ensure-directories-exist pathname)
                            (error 'conversation-error :message "Injected publication failure."
                                                       :pathname identity :sequence nil)))))
             (:append
              (list (list 'log-append
                          (lambda (pathname record &rest arguments)
                            (declare (ignore pathname record arguments))
                            (error 'conversation-error :message "Injected append failure."
                                                       :pathname identity :sequence nil)))))
             (:attachment
              (list (list 'uiop:copy-file
                          (lambda (from to &rest arguments)
                            (declare (ignore from to arguments))
                            (error 'conversation-error :message "Injected copy failure."
                                                       :pathname identity :sequence nil))))))
           (lambda ()
             (test-assert
              (handler-case
                  (progn (conversation-fork configuration "failure-source"
                                            :identifier identifier)
                         nil)
                (conversation-error () t))
              "copy failures propagate as failures")))
          (test-assert
           (and (not (conversation-storage-occupied-p identity))
                (not (probe-file (conversation-image-artifact-root target))))
           "a failed fork removes its published chunks and partial artifacts")))
      (let* ((identifier "occupied-target")
             (identity   (conversation-pathname-for-id configuration identifier))
             (create     (symbol-function 'conversation-create))
             (occupied   nil))
        (test-call-with-function-replacements
         (list (list 'conversation-create
                     (lambda (configuration &rest arguments &key identifier &allow-other-keys)
                       (let ((target (apply create configuration arguments)))
                         (when (string= identifier "occupied-target")
                           (setf occupied (apply create configuration arguments))
                           (conversation-append-user-message occupied "occupied"))
                         target))))
         (lambda ()
           (test-assert
            (handler-case
                (progn (conversation-fork configuration "failure-source"
                                          :identifier identifier)
                       nil)
              (sexp-store:publication-conflict () t))
            "a target published by another writer wins initial publication")))
        (test-assert
         (and (conversation-storage-occupied-p identity)
              (string= (getf (rest (first (conversation-replay-tests--raw-records occupied)))
                            :content)
                       "occupied"))
         "failure cleanup never deletes another writer's target"))
      (let* ((target   (conversation-create configuration :identifier "occupied-artifacts"))
             (artifact (merge-pathnames "existing.png" (conversation-image-artifact-root target))))
        (ensure-directories-exist artifact)
        (uiop:copy-file image artifact)
        (test-assert
         (and (handler-case
                  (progn (conversation-fork configuration "failure-source"
                                            :identifier "occupied-artifacts")
                         nil)
                (sexp-store:publication-conflict () t))
              (probe-file artifact)
              (not (conversation-storage-occupied-p (conversation-pathname target))))
         "preexisting artifacts prevent publication and survive failure cleanup"))))
  nil)

(-> test-conversation-fork-command-line () null)
(defun test-conversation-fork-command-line ()
  "Test the fork command creates an explicit target and resumes it."
  (let ((fork-call  nil)
        (start-call nil))
    (test-call-with-function-replacements
     (list
      (list 'conversation-fork
            (lambda (configuration source &key selection identifier)
              (declare (ignore configuration))
              (setf fork-call (list source selection identifier))
              (make-instance 'conversation
                             :identifier       identifier
                             :prompt-cache-key "fork-cache"
                             :pathname         #P"/tmp/cli-fork.sexp"
                             :log-pathname     #P"/tmp/cli-fork.sexp"
                             :persisted-p t :created-at 1
                             :next-sequence 1 :input-items nil)))
      (list 'main--start-session
            (lambda (command &key resume-requested-p resume-id &allow-other-keys)
              (declare (ignore command))
              (setf start-call (list resume-requested-p resume-id)))))
     (lambda ()
       (main-dispatch '("fork" "--id" "new-fork" "source" "turn" "2"))))
    (test-assert
     (and (equal fork-call '("source" ("turn" "2") "new-fork"))
          (equal start-call '(t "new-fork")))
     "the fork CLI forwards the selector and starts the new conversation"))
  nil)
