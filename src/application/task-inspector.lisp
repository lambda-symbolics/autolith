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
                                  :agent (getf snapshot :agent)
                                  :created-at (getf continuity :created-at)
                                  :root-conversation (session-job-root-conversation-identifier job)
                                  :classification (getf continuity :classification)
                                  :tool-count (getf continuity :tool-count)
                                  :latest-activity (getf continuity :latest-activity)
                                  :updated-at (getf continuity :updated-at)
                                  :path (getf continuity :path)
                                  :decision (getf continuity :decision)
                                  :recovery-outcome (getf continuity :recovery-outcome)
                                  :recovery-result (getf continuity :recovery-result)
                                  :shell-logs
                                  (getf (shell-log-job-metadata (agent-configuration viewer) job)
                                        :shell-logs)
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

(-> task-inspector--metadata-fields (list) list)
(defun task-inspector--metadata-fields (row)
  "Return bounded lineage, counters, recovery and artifact fields for one inspector ROW."
  (let* ((progress (getf row :progress))
         (result (getf row :result))
         (usage (task-progress--usage-summary (or (getf progress :usage) (getf result :usage))))
         (log-summary (task--shell-log-reference-summary (getf row :shell-logs)))
         (references (or (getf result :shell-log-references)
                         (getf log-summary :shell-log-references)))
         (omitted (+ (getf result :shell-log-omitted-count
                          (getf log-summary :shell-log-omitted-count 0))
                     (max 0 (- (length references) 8)))))
    (append
     (loop for (label value style) in
           (list (list "owners" (format nil "~{~A~^ → ~}" (getf row :owners)) ':dim)
                 (list "execution" (getf row :execution-id) ':code)
                 (list "tools" (getf row :tool-count) ':plain)
                 (list "activity" (when (getf row :updated-at)
                                    (format nil "~(~A~) at ~D" (getf row :latest-activity)
                                            (getf row :updated-at))) ':dim)
                 (list "recovery" (or (getf row :recovery-outcome) (getf row :decision)) ':notice)
                 (list "error" (getf row :error) ':failure)
                 (list "worktree" (getf result :worktree-artifact-path) ':code))
           when (and value (not (equal value "")))
             collect (list :label label :value (format nil "~A" value) :style style))
     (when usage
       (list (list :label "usage" :style ':plain
                   :value (format nil "~{~A~^ · ~}"
                                  (loop for (name value) in usage
                                        if (integerp value)
                                          collect (format nil "~A ~D" name value)
                                        else
                                          append (loop for (nested counter) in value
                                                       collect (format nil "~A.~A ~D" name nested counter)))))))
     (loop for reference in references
           repeat 8
           collect (list :label "shell log" :value reference :style ':code))
     (when (plusp omitted)
       (list (list :label "shell logs" :style ':hint
                   :value (format nil "~D more; inspect with job.get" omitted)))))))

(-> task-inspector-render
    (tool-context task-orchestrator &key (:application (option application))
     (:offset integer) (:limit integer))
    (or string list))
(defun task-inspector-render (context orchestrator
                              &key application (offset 0) (limit *task-job-page-default*))
  "Render a navigable page of owned tasks, with semantic spans when APPLICATION is supplied."
  (unless (and (<= 0 offset 1000000) (<= 1 limit *task-job-page-maximum*))
    (task-continuity--fail "Task page offset or limit is outside its bounds."))
  (let* ((rows (stable-sort
                (task-inspector-rows context orchestrator)
                (lambda (left right)
                  (let ((left-live (member (getf left :state) '(:queued :running :finalizing)))
                        (right-live (member (getf right :state) '(:queued :running :finalizing))))
                    (cond
                      ((and left-live (not right-live))
                       t)
                      ((and right-live (not left-live))
                       nil)
                      (t
                       (> (or (getf left :updated-at) (getf left :created-at) 0)
                          (or (getf right :updated-at) (getf right :created-at) 0))))))))
         (total (length rows))
         (start (min offset total))
         (end (min total (+ start limit)))
         (page (subseq rows start end))
         (heading (format nil "tasks · ~D total · ~D to ~D" total
                          (if page (1+ start) 0) end))
         (navigation (format nil "(tasks :list :offset OFFSET :limit LIMIT)~@[ · next: (tasks :list :offset ~D :limit ~D)~]"
                             (and (< end total) end) limit))
         (actions "(tasks :get \"ID\") | (tasks :transcript \"ID\" :offset OFFSET :limit LIMIT) | (tasks :send \"ID\" :message \"MESSAGE\") | (tasks :cancel \"ID\") | (tasks :revive \"ID\" :authorize t) | (tasks :abandon \"ID\" :authorize t)"))
    (if application
        (application--tool-entry
         application :style ':notice :header heading
         :rows (append
                (loop for row in page
                    append (append (application--native-snapshot-rows application row)
                                   (application--tool-field-rows
                                    application (task-inspector--metadata-fields row))
                                   (list nil)))
                (application--tool-field-rows
                 application (list (list :label "pages" :value navigation :style ':hint)
                                   (list :label "actions" :value actions :style ':hint)))))
        (with-output-to-string (stream)
          (format stream "~A~%" heading)
          (dolist (row page)
            (let* ((progress (getf row :progress))
                   (result (getf row :result)))
              (format stream "~A <- ~A  ~(~A~)  ~A~%  ~A  ~A ms  requests ~A~%"
                      (or (getf row :job-id) (getf row :execution-id) "artifact")
                      (or (first (last (getf row :owners))) "primary")
                      (or (getf row :state) (getf row :classification))
                      (or (getf progress :current-tool) (getf row :summary) "")
                      (getf row :execution-id)
                      (or (getf progress :duration-ms) (getf result :duration-ms) "unknown")
                      (or (getf progress :request-count) (getf result :request-count) "unknown"))
              (dolist (field (task-inspector--metadata-fields row))
                (format stream "  ~A ~A~%" (getf field :label) (getf field :value)))
              (dolist (path (remove nil (list (getf row :path) (getf result :output-path))))
                (format stream "  artifact ~A~%" path))))
          (format stream "~A~%~A" navigation actions)))))

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

(-> task-inspector--result-entry
    (application tool-context &key (:operation string) (:result tool-result)) list)
(defun task-inspector--result-entry (application context &key operation result)
  "Present an ordinary job dispatch result using its retained native details."
  (application-tool-result-entry
   (tool-registry-find (tool-context-registry context) "job" operation)
   application
   (list :tool-result :tool (format nil "job.~A" operation)
         :status (if (tool-result-success-p result) ':ok ':error)
         :output (tool-result-content result) :details (tool-result-details result))))

(-> task-inspector-action
    (tool-context task-orchestrator string &key (:id (option string))
     (:message (option string)) (:authorized-p boolean)
     (:offset integer) (:limit integer) (:application (option application)))
    (or string list))
(defun task-inspector-action (context orchestrator action
                              &key id message authorized-p (offset 0)
                                (limit (if (equal action "list") *task-job-page-default* 4000))
                                application)
  "Inspect or control owned jobs through ordinary registered job dispatch."
  (cond
    ((equal action "list")
     (task-inspector-render context orchestrator :application application :offset offset :limit limit))
    ((equal action "transcript")
     (let ((window (task-inspector-transcript context orchestrator id :offset offset :limit limit)))
       (if application
           (application--tool-entry
            application :style ':notice :header "task transcript"
            :rows (append
                   (application--tool-field-rows
                    application
                    (list (list :label "artifact" :value (getf window :path) :style ':code)
                          (list :label "characters"
                                :value (format nil "~D to ~D · ~A" (getf window :offset)
                                               (getf window :next-offset)
                                               (if (getf window :eof-p) "end" "more")))
                          (list :label "next"
                                :value (if (getf window :eof-p) "end of transcript"
                                           (format nil "(tasks :transcript ~S :offset ~D :limit ~D)"
                                                   id (getf window :next-offset) limit))
                                :style ':hint)))
                   (list nil)
                   (loop for line in (application--display-lines (getf window :text))
                         append (application--tool-field-rows
                                 application (list (list :label "" :value line :style ':code))))))
           (task--write-readable-sexp window))))
    ((member action '("send" "cancel" "get") :test #'equal)
     (let ((arguments (json-object "id" id)))
       (when (equal action "send")
         (setf (gethash "message" arguments) message))
       (let ((result (task-continuity-dispatch context "job" action arguments)))
         (if application
             (task-inspector--result-entry application context :operation action :result result)
             (tool-result-content result)))))
    ((member action '("revive" "abandon") :test #'equal)
     (let ((result (task-continuity-dispatch
                    context "job" "continuity"
                    (json-object "action" action "id" id
                                 "authorize" (if authorized-p t (json-false))))))
       (if application
           (task-inspector--result-entry application context :operation "continuity" :result result)
           (tool-result-content result))))
    (t
     (task-continuity--fail "Unknown task inspector action."))))

(-> application-task-inspector-slash-arguments (string) list)
(defun application-task-inspector-slash-arguments (text)
  "Parse task actions and page bounds, preserving the steering message tail."
  (let ((position 0) (size (length text)))
    (labels ((skip-space ()
               (loop while (and (< position size) (find (char text position) '(#\Space #\Tab)))
                     do (incf position)))

             (word ()
               (skip-space)
               (let ((start position))
                 (loop while (and (< position size) (not (find (char text position) '(#\Space #\Tab))))
                       do (incf position))
                 (unless (= start position) (subseq text start position))))

             (parse-bound (value default)
               (if value
                   (or (ignore-errors (parse-integer value :junk-allowed nil))
                       (task-continuity--fail "Task bounds must be integers."))
                   default)))
      (let* ((name (or (word) "list"))
             (action (find name '(:list :get :transcript :send :cancel :revive :abandon)
                           :key #'symbol-name :test #'string-equal))
             (id (word)))
        (unless action (task-continuity--fail "Unknown task inspector action."))
        (skip-space)
        (case action
          (:send
           (list action id :message (subseq text position)))
          (:list
           (let ((limit (word)))
             (when (word) (task-continuity--fail "Too many task page arguments."))
             (list action nil :offset (parse-bound id 0)
                   :limit (parse-bound limit *task-job-page-default*))))
          (:transcript
           (let* ((offset (word)) (limit (word)) (extra (word)))
             (when extra (task-continuity--fail "Too many transcript arguments."))
             (list action id :offset (parse-bound offset 0) :limit (parse-bound limit 4000))))
          ((:revive :abandon)
           (let ((authorization (word)))
             (when (or (word) (and authorization (not (string-equal authorization "authorize"))))
               (task-continuity--fail "Expected authorize after the task ID."))
             (list action id :authorize (and authorization t))))
          (otherwise
           (when (word) (task-continuity--fail "Unexpected task arguments."))
           (if id (list action id) (list action))))))))

(-> application-task-inspector (application &optional t t &rest t) (or string list))
(defun application-task-inspector (application &optional (action ':list) id &rest options)
  "Inspect tasks with native values. LIST accepts :OFFSET and :LIMIT without an ID."
  (when (and (eq action ':list) (keywordp id))
    (setf options (cons id options)
          id nil))
  (unless (and (member action '(:list :get :transcript :send :cancel :revive :abandon))
               (if (eq action ':list) (null id) (non-empty-string-p id))
               (evenp (length options))
               (let ((keys (loop for (key value) on options by #'cddr collect key)))
                 (and (= (length keys) (length (remove-duplicates keys)))
                      (every (lambda (key)
                               (member key (case action
                                             (:send '(:message))
                                             ((:list :transcript) '(:offset :limit))
                                             ((:revive :abandon) '(:authorize)))))
                             keys)))
               (case action
                 (:send (non-empty-string-p (getf options :message)))
                 (:list
                  (and (integerp (getf options :offset 0))
                       (<= 0 (getf options :offset 0) 1000000)
                       (integerp (getf options :limit *task-job-page-default*))
                       (<= 1 (getf options :limit *task-job-page-default*) *task-job-page-maximum*)))
                 (:transcript
                  (and (integerp (getf options :offset 0))
                       (<= 0 (getf options :offset 0) 1000000)
                       (integerp (getf options :limit 4000))
                       (<= 1 (getf options :limit 4000) *task-inspector-transcript-maximum-characters*)))
                 ((:revive :abandon) (member (getf options :authorize) '(t nil)))
                 (otherwise t)))
    (task-continuity--fail "Invalid task action or arguments."))
  (let ((agent (application-agent application)))
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
       context (task-job-tool-orchestrator tool) (string-downcase (symbol-name action)) :id id
       :message (getf options :message) :authorized-p (getf options :authorize)
       :application (and (application-ui application) application) :offset (getf options :offset 0)
       :limit (getf options :limit (if (eq action ':list) *task-job-page-default* 4000))))))
