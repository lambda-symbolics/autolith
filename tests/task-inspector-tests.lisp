(in-package #:autolith)

;;;; -- Optional Inspector Boundaries --

(-> test-task-inspector-transcript () null)
(defun test-task-inspector-transcript ()
  "Bound transcript windows, reject missing/foreign paths, and expose durable rows."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore parent))
     (let* ((directory (uiop:pathname-directory-pathname path))
            (transcript (merge-pathnames "conversation.sexp" directory))
            (result-path (merge-pathnames "result.sexp" directory))
            (execution (session-job-execution-identifier job)))
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-inspector-transcript context orchestrator execution)))
        "Missing transcript artifacts produce an explicit refusal")
       (snapshot-write-text transcript "abcdefghij")
       (snapshot-write result-path
                       (list :status ':success :conversation-file (namestring transcript)
                             :output (make-string 20000 :initial-element #\x)
                             :duration-ms 321 :request-count 7
                             :usage '(("input_tokens" 42))))
       (let ((result (getf (first (task-inspector-rows context orchestrator)) :result)))
         (test-assert (and (null (getf result :output))
                           (= 321 (getf result :duration-ms))
                           (= 7 (getf result :request-count))
                           (equal '(("input_tokens" 42)) (getf result :usage))
                           (getf result :output-path))
                      "Inspector projects terminal timing, usage and artifact paths without full output"))
       (let ((window (task-inspector-transcript context orchestrator execution :offset 2 :limit 3)))
         (test-assert (equal "cde" (getf window :text)) "Read exactly the requested character window")
         (test-assert (and (= 5 (getf window :next-offset)) (not (getf window :eof-p)))
                      "Return bounded pagination without reading the rest"))
       (let ((window (task-inspector-transcript context orchestrator execution :offset 9 :limit 3)))
         (test-assert (and (equal "j" (getf window :text)) (getf window :eof-p))
                      "EOF is explicit for a short final window"))
       (dolist (bounds '((-1 10) (1000001 10) (0 0) (0 16001)))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-inspector-transcript context orchestrator execution
                                                 :offset (first bounds) :limit (second bounds))))
          "Reject transcript bounds before filesystem access"))
       (test-assert (= 1 (length (task-inspector-rows context orchestrator)))
                    "Lost durable tasks are visible alongside live jobs")
       (let ((foreign (merge-pathnames "foreign-transcript.sexp"
                                       (config :working-directory
                                               (tool-context-configuration context)))))
         (snapshot-write-text foreign "not this child's transcript")
         (snapshot-write result-path (list :status ':success
                                          :conversation-file (namestring foreign)))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-inspector-transcript context orchestrator execution)))
          "Persisted transcript paths cannot escape the child's artifact directory"))
       (let* ((conversation (conversation-create
                             (tool-context-configuration context)
                             :identifier execution :storage-root directory))
              (identity (conversation-pathname conversation)))
         (conversation-append-user-message conversation "actual persisted conversation")
         (conversation-append-summary conversation "persisted compacted checkpoint")
         (conversation-append-user-message conversation "after compaction")
         (snapshot-write result-path (list :status ':success :conversation-file (namestring identity)))
         (let* ((segments (conversation-storage-pathnames identity))
                (text (with-output-to-string (output)
                        (dolist (segment segments)
                          (write-string (uiop:read-file-string segment) output))))
                (boundary (length (uiop:read-file-string (first segments))))
                (offset (- boundary 2))
                (window (task-inspector-transcript context orchestrator execution
                                                   :offset offset :limit 6)))
           (test-assert (and (not (probe-file identity)) (> (length segments) 1))
                        "Actual conversation persistence uses compacted chunk storage")
           (test-assert (and (equal (subseq text offset (+ offset 6)) (getf window :text))
                            (= (+ offset 6) (getf window :next-offset)))
                        "Logical transcript windows cross actual compacted chunk boundaries")
           (let ((end (task-inspector-transcript context orchestrator execution
                                                :offset (+ 10 (length text)) :limit 6)))
             (test-assert (and (equal "" (getf end :text)) (getf end :eof-p)
                              (= (length text) (getf end :next-offset)))
                          "Beyond-EOF offsets are clamped to actual logical transcript length"))
           (delete-file result-path)
           (test-assert
            (equal (subseq text 0 6)
                   (getf (task-inspector-transcript context orchestrator execution :limit 6) :text))
            "Unfinished children are readable before publishing a terminal result")))
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-inspector-transcript context orchestrator "foreign-execution")))
        "A caller cannot invent a transcript identity"))))
  nil)

(-> test-task-inspector-dispatch () null)
(defun test-task-inspector-dispatch ()
  "Steer and cancel through ordinary owned job tools, preserving authorization."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
      (declare (ignore path))
     (let ((pool (task-orchestrator-pool orchestrator)))
       (with-lock-held ((cl-jobpond::job-pool--lock pool))
         (setf (gethash (job-identifier job) (cl-jobpond::job-pool--jobs pool)) job)
         (incf (cl-jobpond::job-pool--live-count pool)))
       (with-lock-held ((cl-jobpond::job--lock job))
         (setf (job-state job) ':running
               (job-started-at job) (get-internal-real-time)))
       (task-job--set-progress-state job ':running)
       (let* ((text (concatenate 'string (make-string 7000 :initial-element #\x)
                                "LATEST NOTE"))
              (snapshot (task-job-snapshot job)))
         (setf (getf (getf snapshot :progress) :recent-output) text)
         (dolist (limit '(0 256))
           (let* ((record (task--job-native-record snapshot :preview-limit limit
                                                  :include-progress-p t))
                  (preview (getf (getf record :progress) :recent-output)))
             (test-assert
              (and (equal (subseq text (- (length text) limit)) (getf preview :tail))
                   (= (- (length text) limit)
                      (getf (getf preview :omitted) :characters)))
              "Native progress previews retain the newest bounded tail, including zero budget")
             (test-assert (< (length (task--write-readable-sexp record)) 1024)
                          "A long live progress record fits the compact inspector boundary")))
         (test-assert
          (equal "short" (task--artifact-field "short" ':progress-output :preview-limit 10))
          "Short progress is readable inline"))
       (test-assert (= 1 (length (task-inspector-rows context orchestrator)))
                    "A live job and durable record merge into one inspector row")
        (let* ((application (mission-test--application (agent-configuration parent)))
               (message "Use the corrected assignment, preserving these spaces.")
               (invocation (application-command-invocation-parse
                            (format nil "/tasks send ~A ~A"
                                    (session-job-identifier job) message))))
          (unwind-protect
               (progn
                 (setf (application-agent application) parent
                       (application-tool-registry application) (agent-tool-registry parent)
                       (application-conversation application) (agent-conversation parent))
                 (application-command-execute
                  (application-command-invocation-command invocation) application invocation)
                 (test-assert (= 1 (task-job-steering-pending-count job))
                              "The slash inspector enters the ordinary durable child input queue")
                 (test-assert
                  (search message
                          (user-message-input-text
                           (agent-steering-input-content (first (task-job-take-steering job)))))
                  "Multiword slash steering reaches the child without tokenization loss"))
            (terminal-ui-stop (application-ui application))))
       (let ((count (length (task-orchestrator-list-jobs orchestrator))))
         (task-inspector-action context orchestrator "revive"
                                :id (session-job-execution-identifier job) :authorized-p t)
         (test-assert (= count (length (task-orchestrator-list-jobs orchestrator)))
                      "Inspector reattachment cannot duplicate a live child"))
       (task-inspector-action context orchestrator "cancel" :id (session-job-identifier job))
       (test-assert (job-cancellation-requested-p job)
                    "Inspector cancellation uses the ordinary job lifecycle"))
     (let ((called-p nil)
           (registry (tool-context-registry context)))
       (tool-registry-register
        registry (make-instance 'task-test-authorization-tool :namespace "test" :name "authorize"
                                :description "Check forwarded command authority."
                                :parameters (tool-object-schema (json-object) nil)))
       (let ((authorized-context
               (make-instance
                'tool-context :configuration (tool-context-configuration context)
                :agent (tool-context-agent context) :registry registry
                :worker nil :conversation (tool-context-conversation context)
                :command-authorization-function
                (lambda (command directory)
                  (declare (ignore command directory))
                  (setf called-p t)
                  ':deny))))
         (task-continuity-dispatch authorized-context "test" "authorize" (json-object)))
       (test-assert (and called-p (eq :deny *task-test-command-decision*))
                    "Ordinary action dispatch retains the originating authorization callback"))))
  nil)
