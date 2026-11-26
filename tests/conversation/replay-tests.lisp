(in-package #:autolith)

;;;; -- Read-only Conversation Replay Tests --

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

(-> test-conversation-image-archive-ownership () null)
(defun test-conversation-image-archive-ownership ()
  "Test that sealed user and tool images are kept when a conversation is reopened."
  (with-test-configuration (configuration root)
    (let* ((source       (test-conversation--write-tiny-png (merge-pathnames "source.png" root)))
           (conversation (conversation-create configuration :identifier "image-archive"))
           (original-map (symbol-function 'sexp-store:segment-map))
           (opens        0)
           (bytes        0))
      (conversation-append-user-message
       conversation (user-message-input-create :text "image" :image-pathnames (list source)))
      (conversation-append-provider-item
       conversation (json-object "type" "function_call" "call_id" "image-call"
                                 "name" "image" "arguments" "{}"))
      (conversation-append-tool-result
       conversation "image-call" :tool-name "image" :output "image" :success-p t
       :image-attachments (list (image-input-prepare source (conversation-image-artifact-root conversation))))
      (let ((artifacts (conversation--image-artifact-pathnames
                        (conversation-image-artifact-root conversation))))
        (dotimes (index 12)
          (conversation-append-record
           conversation (list :summary :content "compact" :through-seq (1- (conversation-next-sequence conversation))))
          (conversation-append-user-message conversation (format nil "tail ~D" index)))
        (let ((active (conversation-log-pathname conversation))
              (lease  nil)
              (loaded nil))
          (unwind-protect
               (progn
                 (test-call-with-function-replacements
                  (list (list 'sexp-store:segment-map
                              (lambda (function pathname &rest options)
                                (test-assert (equal pathname active) "owned reopening reads no sealed segment")
                                (incf opens)
                                (multiple-value-prog1
                                    (apply original-map function pathname options)
                                  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
                                    (incf bytes (file-length stream)))))))
                  (lambda ()
                    (multiple-value-setq (loaded lease)
                      (application--conversation-load-owned nil configuration "image-archive"))))
                 (test-assert (and (= opens 1) (plusp bytes)
                                   (every #'probe-file artifacts)
                                   (conversation-image-artifact-manifest-complete-p loaded))
                              "one active-segment load preserves complete archival image roots")
                 (conversation--prune-unreferenced-image-artifacts loaded)
                 (test-assert (every #'probe-file artifacts) "maintenance retains sealed user and tool images"))
            (when lease (conversation-lease-release lease)))))))
  nil)

(-> test-conversation-image-maintenance-failures () null)
(defun test-conversation-image-maintenance-failures ()
  "Separate optional deletion and manifest repair from required image loading."
  (with-test-configuration (configuration root)
    (let* ((source       (test-conversation--write-tiny-png (merge-pathnames "source.png" root)))
           (conversation (conversation-create configuration :identifier "image-maintenance")))
      (conversation-append-user-message
       conversation (user-message-input-create :text "image" :image-pathnames (list source)))
      (conversation-append-record conversation (list :summary :content "compact" :through-seq 1))
      (let* ((orphan          (merge-pathnames "orphan.png" (conversation-image-artifact-root conversation)))
             (original-delete (symbol-function 'delete-file))
             (lease           nil))
        (uiop:copy-file source orphan)
        (unwind-protect
             (sb-ext:without-package-locks
              (test-call-with-function-replacements
               (list (list 'delete-file
                           (lambda (pathname)
                             (if (equal pathname orphan)
                                 (error 'file-error :pathname pathname)
                                 (funcall original-delete pathname)))))
               (lambda ()
                 (multiple-value-bind (loaded acquired)
                     (application--conversation-load-owned nil configuration "image-maintenance")
                   (setf lease acquired)
                   (test-assert (probe-file orphan) "owned open performs no optional deletion")
                   (conversation--prune-unreferenced-image-artifacts loaded)
                   (test-assert (and (probe-file orphan)
                                     (conversation-image-artifact-maintenance-diagnostics loaded))
                                "failed optional deletion records a maintenance diagnostic")
                   (conversation--restore-image-manifest loaded nil)
                   (conversation--prune-unreferenced-image-artifacts loaded)
                   (test-assert (and (probe-file orphan)
                                     (conversation-image-artifact-repair-pending-p loaded))
                                "unknown archival roots retain artifacts and request repair")
                   (conversation-image-artifacts-repair loaded)
                   (test-assert (conversation-image-artifact-manifest-complete-p
                                 (conversation-load-by-id configuration "image-maintenance"))
                                "explicit repair publishes reconstructible archival roots")))))
          (when lease (conversation-lease-release lease))))))
  nil)

(-> test-conversation-image-publication-roots () null)
(defun test-conversation-image-publication-roots ()
  "Protect prepared images and durable images when post-publication adapters fail."
  (with-test-configuration (configuration root)
    (let* ((source       (test-conversation--write-tiny-png (merge-pathnames "source.png" root)))
           (conversation (conversation-create configuration :identifier "image-publication")))
      (conversation-append-user-message conversation "initial")
      (let* ((prepared (image-input-prepare source (conversation-image-artifact-root conversation)))
             (pathname (image-attachment-pathname prepared)))
        (conversation--prune-unreferenced-image-artifacts conversation)
        (test-assert (probe-file pathname) "reclamation retains an in-flight publication root")
        (conversation--delete-image-attachments (list prepared))
        (test-assert (and (not (probe-file pathname))
                          (not (gethash pathname *image-input-publication-roots*)))
                     "explicit rollback releases and removes an unpublished artifact"))
      (let ((*conversation-append-publication-function*
              (lambda (owner record)
                (declare (ignore record))
                (error 'conversation-invariant-error :pathname (conversation-pathname owner)
                       :sequence nil :message "Injected post-publication failure."))))
        (test-assert
         (handler-case
             (progn
               (conversation-append-user-message
                conversation (user-message-input-create :text "published" :image-pathnames (list source)))
               nil)
           (conversation-invariant-error () t))
         "the post-publication failure is observable"))
      (test-assert (= 2 (length (conversation-input-items
                                 (conversation-load-by-id configuration "image-publication"))))
                   "durably referenced images survive failed post-publication adapters")))
  nil)
