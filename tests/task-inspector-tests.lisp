(in-package #:autolith)

;;;; -- Optional Inspector Boundaries --

(-> test-task-inspector-transcript () null)
(defun test-task-inspector-transcript ()
  "Bound transcript windows, reject missing/foreign paths, and expose durable rows."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore parent))
     (let* ((directory   (uiop:pathname-directory-pathname path))
            (transcript  (merge-pathnames "conversation.sexp" directory))
            (result-path (merge-pathnames "result.sexp" directory))
            (execution   (session-job-execution-identifier job)))
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-inspector-transcript context orchestrator execution)))
        "Missing transcript artifacts produce an explicit refusal")
       (snapshot-write-text transcript "abcdefghij")
       (snapshot-write result-path
                       (list :status ':success :conversation-file (namestring transcript)
                             :output (make-string 20000 :initial-element #\x)
                             :duration-ms 321 :request-count 7
                             :usage  '(("input_tokens" 42))))
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
         (snapshot-write result-path (list :status           ':success
                                          :conversation-file (namestring foreign)))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-inspector-transcript context orchestrator execution)))
          "Persisted transcript paths cannot escape the child's artifact directory"))
       (let* ((conversation (conversation-create
                             (tool-context-configuration context)
                             :identifier execution :storage-root directory))
              (identity     (conversation-pathname conversation)))
         (conversation-append-user-message conversation "actual persisted conversation")
         (conversation-append-summary conversation "persisted compacted checkpoint")
         (conversation-append-user-message conversation "after compaction")
         (snapshot-write result-path (list :status ':success :conversation-file (namestring identity)))
         (let* ((segments (conversation-storage-pathnames identity))
                (text     (with-output-to-string (output)
                            (dolist (segment segments)
                              (write-string (uiop:read-file-string segment) output))))
                (boundary (length (uiop:read-file-string (first segments))))
                (offset   (- boundary 2))
                (window   (task-inspector-transcript context orchestrator execution
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
  "Inspect and cancel through ordinary owned job tools, preserving authorization."
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
       (let* ((text     (concatenate 'string (make-string 7000 :initial-element #\x)
                                    "LATEST NOTE"))
              (snapshot (task-job-snapshot job)))
         (setf (getf (getf snapshot :progress) :recent-output) text)
         (dolist (limit '(0 256))
           (let* ((record  (task--job-native-record snapshot :preview-limit limit
                                                   :include-progress-p      t))
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
          "Short progress is readable inline")
         (test-assert
          (string= (task--write-readable-sexp
                    (list :id   (coerce "exec:1" 'base-string)
                          :pair (cons 1 (coerce "x" 'base-string))))
                   "(:ID \"exec:1\" :PAIR (1 . \"x\"))")
          "Native records print base strings as plain strings"))
       (test-assert (= 1 (length (task-inspector-rows context orchestrator)))
                    "A live job and durable record merge into one inspector row")
       (let ((application (mission-test--application (agent-configuration parent))))
         (unwind-protect
              (progn
                (setf (application-agent application) parent
                      (application-tool-registry application) (agent-tool-registry parent)
                      (application-conversation application) (agent-conversation parent))
                (test-assert (eq ':ok (application-lisp-evaluation-status
                                       (application-lisp-evaluate "(tasks)" :application application)))
                             "native tasks list is callable from the Lisp prompt")
                (let ((id (session-job-identifier job)))
                  (dolist (arguments (list (list ':send id :message "Steer through the channel.")
                                          (list ':transcript id :offset "2" :limit 3)
                                          (list ':transcript id :offset 0 :limit 0)
                                          (list ':revive id :authorize "authorize")
                                          (list ':cancel id :extra t)))
                    (test-assert
                     (handler-case
                         (progn (apply #'application-operation-call application "tasks" arguments) nil)
                       (task-continuity-error () t))
                     "invalid native task values receive typed rejections"))
                  (test-assert (not (job-cancellation-requested-p job))
                               "rejected task calls do not cancel the job"))
                (let ((observed nil))
                  (test-call-with-function-replacements
                   (list (list 'task-inspector-action
                               (lambda (owner-context owner-orchestrator action &rest options)
                                 (declare (ignore owner-context owner-orchestrator))
                                 (push (cons action options) observed)
                                 "inspected")))
                   (lambda ()
                     (application-operation-call application "tasks" ':revive
                                                 (session-job-identifier job) :authorize nil)
                     (application-operation-call application "tasks" ':abandon
                                                 (session-job-identifier job) :authorize t)
                     (application-operation-call application "tasks" ':transcript
                                                 (session-job-identifier job) :offset 2 :limit 3)))
                  (test-assert
                   (and (= 2 (getf (rest (first observed)) :offset))
                        (= 3 (getf (rest (first observed)) :limit))
                        (eq t (getf (rest (second observed)) :authorized-p))
                        (null (getf (rest (third observed)) :authorized-p)))
                   "native transcript integers and explicit true/false authority reach ordinary task dispatch")))
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
                                :parameters  (tool-object-schema (json-object) nil)))
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
         (task-continuity-dispatch authorized-context "test" "authorize" :arguments (json-object)))
       (test-assert (and called-p (eq :deny *task-test-command-decision*))
                    "Ordinary action dispatch retains the originating authorization callback"))))
  nil)

(-> test-task-inspector-bounded-usage () null)
(defun test-task-inspector-bounded-usage ()
  "Inspect live and restored children without serializing provider attribution."
  (let ((cyclic (list '("input_tokens" 0) '("output_tokens" -1))))
    (setf (rest (rest cyclic)) cyclic)
    (test-assert
     (equal '(("input_tokens" 0)) (task-progress--usage-summary cyclic))
     "Cyclic usage retains zero counters without scanning indefinitely"))
  (dolist (usage (list nil 42 '("bad" . 3)
                      '(("output_tokens" "invalid") ("attribution" (1 2 3)))))
    (test-assert (null (task-progress--usage-summary usage))
                 "Malformed usage contributes no supervisory counters"))
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context orphan path)
     (declare (ignore path))
     (let* ((job         (task-tests--register-job
                          orchestrator parent (task-job-definition orphan) :name "bounded-usage"))
            (progress    (task-job-progress job))
            (attribution (loop for tree = '("provider") then (list tree tree)
                               repeat 22 finally (return tree)))
            (usage       (list (list "input_tokens" 3)
                               (list "output_tokens" 2)
                               (list "input_tokens_details" '(("cached_tokens" 1)))
                               (list "attribution" attribution)))
            (expected    '(("input_tokens" 6) ("output_tokens" 4)
                           ("input_tokens_details" (("cached_tokens" 2)))))
            (application (make-instance 'application
                                        :configuration (agent-configuration parent)
                                        :conversation  (agent-conversation parent)
                                        :tool-registry (agent-tool-registry parent)
                                        :agent parent :ui nil)))
       (task-continuity-record-job job parent)
       (dotimes (index 2)
         (task-progress-note-status job ':provider-request-completed
                                    (list :request-number (1+ index) :usage usage)))
       (test-assert (equal expected (getf (task-progress-snapshot job) :usage))
                    "Repeated provider events sum numeric and nested counters without attribution")
       (test-assert (eq attribution (second (assoc "attribution" usage :test #'string=)))
                    "Supervisory projection leaves provider audit data untouched")
       (let* ((directory (uiop:pathname-directory-pathname
                          (task-continuity-record-job job parent)))
              (activity  (task-continuity--read (merge-pathnames "activity.sexp" directory))))
         (test-assert (equal expected (getf (getf activity :progress) :usage))
                      "Published activity can be reread within ordinary artifact limits"))
       ;; Simulate progress retained by a pre-fix image before another event arrives.
       (setf (task-progress-usage progress) usage)
       (test-assert
        (and (application-task-inspector application)
             (task-continuity-inventory parent orchestrator)
             (tool-result-success-p
              (task-continuity-dispatch context "job" "get"
                                        :arguments (json-object "id" (job-identifier job)))))
        "Inspector, continuity and job.get accept restored oversized progress")
       (task-tests--publish-terminal job ':failed (task--failed-result job ':failed "Provider failed"))
       ;; Restore the old result shape without asking the new publisher to create it.
       (setf (getf (job-result job) :usage) usage
             (task-progress-usage progress) usage)
       (let* ((snapshot (session-job-snapshot job))
              (rows     (task-inspector-rows context orchestrator))
              (row      (find (session-job-execution-identifier job) rows
                              :key (lambda (value) (getf value :execution-id)) :test #'equal)))
         (test-assert
          (and (eq ':failed (getf snapshot :state))
               (equal (task-progress--usage-summary usage)
                      (getf (getf snapshot :result) :usage))
               (equal (task-progress--usage-summary usage)
                      (getf (getf row :result) :usage))
               (application-task-inspector application)
               (task-continuity-inventory parent orchestrator))
          "Terminal snapshots and the inspector project oversized retained result usage")))))
  nil)

(-> test-task-inspector-presentation () null)
(defun test-task-inspector-presentation ()
  "Present native task state and metadata through bounded, navigable inspector pages."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (let* ((application (mission-test--application (agent-configuration parent)))
            (transcript  (merge-pathnames "conversation.sexp" path))
            (rows        (list (list :job-id "failed-child" :state ':failed :updated-at 3
                                     :execution-id "failed-execution" :owners '("primary" "parent")
                                     :error "lost worker" :decision ':abandon
                                     :progress '(:usage (("input_tokens" 42)))
                                     :result   '(:status :failed :duration-ms 1200))
                               (list :job-id "live-child" :state ':running :updated-at 1
                                     :progress '(:current-tool "shell.run" :request-count 2)))))
       (unwind-protect
            (progn
              (setf (application-agent application) parent
                    (application-tool-registry application) (agent-tool-registry parent)
                    (application-conversation application) (agent-conversation parent))
              (test-call-with-function-replacements
               (list (list 'task-inspector-rows
                           (lambda (owner-context owner-orchestrator)
                             (declare (ignore owner-context owner-orchestrator))
                             (copy-list rows))))
               (lambda ()
                 (dolist (width '(12 90))
                   (terminal-ui-resize (application-ui application) width :rows 20)
                   (let ((entry (application-task-inspector application)))
                     (test-assert (and (terminal-styled-text-p entry)
                                       (find ':failure entry :key #'terminal-span-style)
                                       (find ':notice entry :key #'terminal-span-style))
                                  "Task state and failures retain semantic styles at narrow and wide widths")))
                 (let ((page (task-inspector-render context orchestrator :limit 1)))
                   (test-assert (and (search "live-child" page) (not (search "failed-child" page)))
                                "Live tasks precede newer terminal records in a bounded page"))
                 (let ((page (task-inspector-render context orchestrator :offset 1 :limit 1)))
                   (test-assert (and (search "failed-child" page) (search "input_tokens 42" page))
                                "Later pages preserve task metadata without serializing provider attribution"))
                 (test-assert
                  (eq ':ok (application-lisp-evaluation-status
                            (application-lisp-evaluate "(tasks :list :offset 1 :limit 1)"
                                                       :application application)))
                  "Native list pagination accepts keyword bounds without a dummy ID")))
              (test-assert (equal '(:list nil :offset 32 :limit 10)
                                  (application-task-inspector-slash-arguments "list 32 10"))
                           "Slash pagination reaches the native list options")
              (snapshot-write-text transcript "0123456789abcdefghij")
              (snapshot-write (merge-pathnames "result.sexp" path)
                              (list :status ':success :conversation-file (namestring transcript)))
              (terminal-ui-resize (application-ui application) 90 :rows 20)
              (let ((entry (task-inspector-action context orchestrator "transcript"
                                                   :id (session-job-execution-identifier job)
                                                   :application application :offset 2 :limit 8)))
                (test-assert (and (terminal-styled-text-p entry)
                                  (command-view-tests--role-p entry ':code "23456789")
                                  (find ':hint entry :key #'terminal-span-style))
                             "Transcript windows present text and next-window navigation")))
         (terminal-ui-stop (application-ui application))))))
  nil)
