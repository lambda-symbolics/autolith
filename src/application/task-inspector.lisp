(in-package #:autolith)

;;;; -- Optional Nonmodal Task Inspector --

(defparameter *task-inspector-transcript-maximum-characters* 16000
  "The largest transcript window returned by the optional inspector.")

(-> task-inspector-rows (tool-context task-orchestrator) list)
(defun task-inspector-rows (context orchestrator)
  "Merge coherent owned live snapshots with durable continuity rows."
  (let* ((viewer (tool-context-agent context))
         (durable (task-continuity-inventory viewer orchestrator))
         (jobs (task-orchestrator-list-visible-jobs orchestrator viewer)))
    (append
     (loop for job in jobs
           for continuity = (find (session-job-execution-identifier job) durable
                                  :key (lambda (row) (getf row :execution-id))
                                  :test #'equal)
            for snapshot = (session-job-snapshot job)
            collect (append (list :job-id (job-identifier job)
                                  :execution-id (session-job-execution-identifier job)
                                  :owners (copy-list (job-owner-identifiers job))
                                  :root-conversation (session-job-root-conversation-identifier job)
                                  :classification (getf continuity :classification)
                                  :tool-count (getf continuity :tool-count)
                                  :latest-activity (getf continuity :latest-activity)
                                  :updated-at (getf continuity :updated-at)
                                  :path (getf continuity :path)
                                  :decision (getf continuity :decision)
                                  :recovery-outcome (getf continuity :recovery-outcome)
                                  :recovery-result (getf continuity :recovery-result)
                                  :state (getf snapshot :state)
                                  :progress (task-continuity--progress-summary (getf snapshot :progress))
                                  :result (task-continuity--result-summary
                                           (getf snapshot :result)
                                           (and (getf continuity :path)
                                                (merge-pathnames "result.sexp"
                                                                 (uiop:pathname-directory-pathname
                                                                  (getf continuity :path))))))))
     (remove-if (lambda (record)
                  (find (getf record :execution-id) jobs
                        :key #'session-job-execution-identifier :test #'equal))
                durable))))

(-> task-inspector-render (tool-context task-orchestrator) string)
(defun task-inspector-render (context orchestrator)
  "Render compact lineage, state, timing, known usage, activity and artifact paths."
  (let ((rows (task-inspector-rows context orchestrator)))
    (with-output-to-string (stream)
      (format stream "tasks (~D)~%" (length rows))
      (dolist (row rows)
        (let* ((progress (getf row :progress))
               (result (getf row :result))
               (recent (getf progress :recent-tools)))
          (format stream "~A <- ~A  ~(~A~)  ~A~%"
                  (or (getf row :job-id) (getf row :execution-id) "artifact")
                  (or (first (last (getf row :owners))) "primary")
                  (or (getf row :state) (getf row :classification))
                  (or (getf progress :current-tool) (first (last recent))
                      (getf row :summary) ""))
          (format stream "  ~A  ~A ms  requests ~A  tools ~A~%"
                  (getf row :execution-id)
                  (or (getf progress :duration-ms) (getf result :duration-ms) "unknown")
                  (or (getf progress :request-count) (getf result :request-count) "unknown")
                  (or (getf row :tool-count) "unknown"))
          (format stream "  provider usage ~A~%"
                  (if (or (getf progress :usage) (getf result :usage))
                      (bounded-string
                       (task--write-readable-sexp
                        (or (getf progress :usage) (getf result :usage))) :limit 500)
                      "unknown"))
          (when (getf row :updated-at)
            (format stream "  activity ~(~A~) at ~D~%"
                    (getf row :latest-activity) (getf row :updated-at)))
          (dolist (path (remove nil (list (getf row :path)
                                         (getf result :output-path)
                                         (getf result :worktree-artifact-path))))
            (format stream "  artifact ~A~%" path))))
      (format stream "get ID | transcript ID [OFFSET LIMIT] | send ID MESSAGE | cancel ID | revive ID authorize | abandon ID authorize"))))

(-> task-inspector-transcript
    (tool-context task-orchestrator string &key (:offset integer) (:limit integer)) list)
(defun task-inspector-transcript (context orchestrator identifier
                                 &key (offset 0) (limit 4000))
  "Read a bounded logical character window across the existing conversation chunks.

OFFSET is capped to bound scanning. NEXT-OFFSET is the actual consumed position,
clamped at EOF. Live children use their deterministic conversation identity."
  (unless (and (<= 0 offset 1000000)
               (<= 1 limit *task-inspector-transcript-maximum-characters*))
    (task-continuity--fail "Transcript offset or limit is outside its bounds."))
  (let* ((viewer (tool-context-agent context))
         (entry (task-continuity--find viewer identifier))
         (record (getf entry :record))
         (directory (uiop:pathname-directory-pathname (getf entry :path)))
         (live (find (getf record :execution-id)
                     (task-orchestrator-list-visible-jobs orchestrator viewer)
                     :key #'session-job-execution-identifier :test #'equal))
         (result-path (merge-pathnames "result.sexp" directory))
         (result (or (and live (getf (session-job-snapshot live) :result))
                     (and (probe-file result-path)
                          (task--read-result-artifact result-path))))
         (identity (pathname
                    (or (getf result :conversation-file)
                        (merge-pathnames
                         (make-pathname :name (getf record :execution-id) :type "sexp")
                         directory))))
         (segments (conversation-storage-pathnames identity))
         (position 0)
         (emitted 0)
         (eof-p t))
    (unless (and segments
                 (every (lambda (path)
                          (uiop:subpathp (platform-truename *platform* path)
                                        (platform-truename *platform* directory)))
                        segments))
      (task-continuity--fail "No owned transcript artifact is available."))
    (let ((text
            (with-output-to-string (output)
              (block scan
                (dolist (path segments)
                  (with-open-file (stream path :external-format ':utf-8)
                    (loop while (peek-char nil stream nil nil)
                          do (when (= emitted limit)
                               (setf eof-p nil)
                               (return-from scan nil))
                             (let ((character (read-char stream)))
                               (incf position)
                               (when (> position offset)
                                 (write-char character output)
                                 (incf emitted))))))))))
      (list :path (namestring identity) :offset offset :text text
            :next-offset position :eof-p eof-p))))

(-> task-inspector-action
    (tool-context task-orchestrator string &key (:id (option string))
     (:message (option string)) (:authorized-p boolean)
     (:offset integer) (:limit integer)) string)
(defun task-inspector-action (context orchestrator action
                              &key id message authorized-p (offset 0) (limit 4000))
  "Inspect or control owned jobs through ordinary registered job dispatch."
  (cond
    ((equal action "list")
     (task-inspector-render context orchestrator))
    ((equal action "transcript")
     (task--write-readable-sexp
      (task-inspector-transcript context orchestrator id :offset offset :limit limit)))
    ((member action '("send" "cancel" "get") :test #'equal)
     (let ((arguments (json-object "id" id)))
       (when (equal action "send")
         (setf (gethash "message" arguments) message))
       (tool-result-content (task-continuity-dispatch context "job" action arguments))))
    ((member action '("revive" "abandon") :test #'equal)
     (tool-result-content
      (task-continuity-dispatch
       context "job" "continuity"
       (json-object "action" action "id" id
                    "authorize" (if authorized-p t (json-false))))))
    (t
     (task-continuity--fail "Unknown task inspector action."))))

(-> application-task-inspector (application string) string)
(defun application-task-inspector (application arguments)
  "Implement optional /tasks with a compact nonmodal command interface."
  (let* ((agent (application-agent application))
         (words (remove "" (uiop:split-string
                            (string-trim '(#\Space #\Tab) arguments)
                            :separator '(#\Space #\Tab)) :test #'equal))
         (action (or (first words) "list"))
         (id (second words)))
    (unless agent
      (task-continuity--fail "Connect an agent before inspecting tasks."))
    (let* ((registry (agent-tool-registry agent))
           (tool (tool-registry-find registry "job" "list"))
           (observer (application-agent-observer application))
           (context (make-instance
                     'tool-context :configuration (agent-configuration agent)
                     :conversation (agent-conversation agent)
                     :worker (agent-worker agent) :agent agent :registry registry
                     :observer observer :call-id (make-identifier)
                     :command-authorization-function
                     (lambda (command directory)
                       (agent-observer-authorize-command observer command directory))
                     :tool-authorization-function
                     (lambda (requested-tool tool-arguments)
                       (agent-observer-authorize-tool observer requested-tool tool-arguments)))))
      (unless (typep tool 'task-job-tool)
        (task-continuity--fail "Task inspection is not registered in this session."))
      (task-inspector-action
       context (task-job-tool-orchestrator tool) action :id id
       :message (when (and (equal action "send") (third words))
                  (let ((position (search (third words) arguments
                                          :start2 (+ (or (search id arguments) 0)
                                                     (length id)))))
                    (and position (subseq arguments position))))
       :authorized-p (equal (third words) "authorize")
       :offset (if (and (equal action "transcript") (third words))
                   (or (ignore-errors (parse-integer (third words))) -1) 0)
       :limit (if (and (equal action "transcript") (fourth words))
                  (or (ignore-errors (parse-integer (fourth words))) 0) 4000)))))
