(in-package #:autolith)

;;;; -- Read-only Conversation Replay Tests --

(-> conversation-replay-tests--session () conversation-replay-session)
(defun conversation-replay-tests--session ()
  "Return deterministic replay navigation state spanning two local dates."
  (let* ((first-time (encode-universal-time 0 0 9 27 8 2026))
         (second-time (encode-universal-time 0 30 10 28 8 2026))
         (conversation
           (make-instance 'conversation
                          :identifier "replay-test"
                          :prompt-cache-key "replay-test"
                          :pathname #p"/tmp/replay-test.sexp"
                          :log-pathname #p"/tmp/replay-test.sexp"
                          :persisted-p nil
                          :created-at first-time
                          :next-sequence 1
                          :input-items nil))
         (records
           (vector
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 10 :time first-time
                                         :role ':user :content "first")
                           :turn 1)
            (make-instance 'conversation-replay-record
                           :record (list ':assistant :seq 11 :time (1+ first-time)
                                         :content "answer")
                           :turn 1)
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 20 :time second-time
                                         :role ':user :content "second")
                           :turn 2))))
    (make-instance 'conversation-replay-session
                   :conversation conversation
                   :records records)))

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
         (projected (conversation-replay--project-record record)))
    (test-assert
     (and (eq (first projected) ':assistant)
          (string= (getf (rest projected) :content) "projected answer")
          (< (length (prin1-to-string projected)) 200))
     "provider replay stores only bounded visible assistant content"))
    (test-assert
     (null
      (conversation-replay--project-record
       (list ':native-compaction
             :seq 8
             :time 101
             :wire-json (make-string 10000 :initial-element #\x))))
     "opaque native compaction payloads are omitted from replay projection")
    (let* ((record
             (list ':turn-aborted
                   :seq 12
                   :time 102
                   :turn-start-seq 8
                   :last-complete-seq 11
                   :reason ':agent-loop
                   :condition-type "AGENT-LOOP-ERROR"
                   :message "provider response was malformed"
                   :request-number 2))
           (projected (conversation-replay--project-record record))
           (output (make-string-output-stream)))
      (conversation-replay--write-record-body projected output)
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
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "read-only-replay")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect me")
           (let* ((pathname (conversation-log-pathname conversation))
                  (write-date (file-write-date pathname))
                  (length (with-open-file (stream pathname)
                            (file-length stream)))
                  (output (make-string-output-stream)))
             (conversation-replay-run configuration
                                      "read-only-replay"
                                      nil
                                      :output output
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
                    (list (configuration-immutable-p configuration)
                          identifier
                          selection)))))
     (lambda ()
       (main-dispatch '("replay" "abc123" "turn" "4"))))
    (test-assert
     (equal observed '(t "abc123" ("turn" "4")))
     "the replay command starts an immutable selected inspection"))
  nil)


(-> test-conversation-fork-storage () null)
(defun test-conversation-fork-storage ()
  "Test historical forks preserve state, provenance, artifacts, and source files."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-image (test-conversation--write-tiny-png
                       (merge-pathnames "first.png" root)))
         (second-image (test-conversation--write-tiny-png
                        (merge-pathnames "second.png" root)))
         (source (conversation-create configuration
                                      :identifier "fork-source"
                                      :prompt-cache-key "source-cache")))
    (unwind-protect
         (progn
           (conversation-append-user-message
            source (user-message-input-create :text "first" :image-pathnames (list first-image)))
           (conversation-append-provider-item
            source
            (json-object "type" "message" "role" "assistant"
                         "content" (vector (json-object "type" "output_text"
                                                        "text" "answer"))))
           (conversation-append-record source (list :summary :content "compact"))
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
           (let* ((source-records (conversation-fork--raw-records source))
                  (source-pathnames
                    (conversation-storage-pathnames
                     (conversation-pathname source)))
                  (source-bytes
                    (mapcar #'uiop:read-file-string source-pathnames))
                  (fork
                    (conversation-fork configuration "fork-source"
                                       :selection '("sequence" "5")
                                       :identifier "fork-target"))
                  (records (conversation-fork--raw-records fork))
                  (provenance (first (last records)))
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
                   (= (length artifacts) 1)
                   (equal source-bytes
                          (mapcar #'uiop:read-file-string source-pathnames)))
              "forks retain the selected chunked prefix, state, provenance, one referenced artifact, and immutable source"))
           (dolist (arguments '(("sequence" "99") ("sequence" "2" "extra")))
             (test-assert
              (handler-case
                  (progn
                    (conversation-fork configuration "fork-source"
                                       :selection arguments
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
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((source (conversation-create configuration
                                              :identifier "generated-source"
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

(-> test-conversation-fork-command-line () null)
(defun test-conversation-fork-command-line ()
  "Test the fork command creates an explicit target and resumes it."
  (let ((fork-call nil)
        (start-call nil))
    (test-call-with-function-replacements
     (list
      (list 'conversation-fork
            (lambda (configuration source &key selection identifier)
              (declare (ignore configuration))
              (setf fork-call (list source selection identifier))
              (make-instance 'conversation
                             :identifier identifier
                             :prompt-cache-key "fork-cache"
                             :pathname #P"/tmp/cli-fork.sexp"
                             :log-pathname #P"/tmp/cli-fork.sexp"
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

(-> test-conversation-replay () null)
(defun test-conversation-replay ()
  "Run read-only replay and durable conversation fork tests."
  (test-conversation-replay-navigation)
  (test-conversation-replay-projection)
  (test-conversation-replay-storage)
  (test-conversation-replay-command-line)
  (test-conversation-fork-storage)
  (test-conversation-fork-rejections)
  (test-conversation-fork-command-line)
  nil)
