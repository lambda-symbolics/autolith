(in-package #:autolith)

;;;; -- Task Runtime --

(-> task--environment-integer
    (string integer &key (:minimum (option integer)) (:maximum (option integer)))
    integer)
(defun task--environment-integer (name fallback &key minimum maximum)
  "Return bounded integer environment NAME or FALLBACK."
  (let ((value (uiop/os:getenv name)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (if (and (integerp parsed)
                       (or (null minimum) (>= parsed minimum)))
                  (if maximum (min parsed maximum) parsed)
                  fallback))
          (error nil fallback))
        fallback)))

(-> task-orchestrator--apply-execution-limits-locked (task-orchestrator) null)
(defun task-orchestrator--apply-execution-limits-locked (orchestrator)
  "Apply asynchronous tool execution bounds while ORCHESTRATOR is locked."
  (let ((pool (task-orchestrator-execution-pool orchestrator)))
    (job-pool-update-limits
     pool
     :maximum-concurrency
     (task--environment-integer
      "AUTOLITH_EXECUTION_MAX_CONCURRENCY"
      *tool-execution-default-maximum-concurrency*
      :minimum 1
      :maximum *tool-execution-maximum-concurrency*)
     :maximum-batch-size 1
     :maximum-live-jobs  *tool-execution-maximum-live-jobs*
     :maximum-runtime-milliseconds
     (job-pool-maximum-runtime-milliseconds pool)))
  nil)

(-> task-orchestrator-add-listener (task-orchestrator function) function)
(defun task-orchestrator-add-listener (orchestrator listener)
  "Register LISTENER for portable task events and return it."
  (check-type listener function)
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (pushnew listener (task-orchestrator-listeners orchestrator) :test #'eq))
  listener)

(-> task-orchestrator-remove-listener (task-orchestrator t) null)
(defun task-orchestrator-remove-listener (orchestrator listener)
  "Remove LISTENER from ORCHESTRATOR."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator)
          (remove listener (task-orchestrator-listeners orchestrator) :test
                  #'eq)))
  nil)

(-> task-orchestrator-emit (task-orchestrator keyword list) null)
(defun task-orchestrator-emit (orchestrator channel payload)
  "Deliver portable CHANNEL and PAYLOAD to a snapshot of listeners."
  (let ((listeners
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (copy-list (task-orchestrator-listeners orchestrator)))))
    (dolist (listener listeners)
      (handler-case
          (funcall listener channel payload)
        (serious-condition ()
          nil))))
  nil)

(-> task--identifier-fragment ((option string)) (option string))
(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((unbounded (string-downcase (task--trim (or value ""))))
         (text      (subseq unbounded
                            0
                            (min (length unbounded)
                                 *task-identifier-maximum-characters*)))
         (mapped
          (map 'string
               (lambda (character)
                 (if (or (alphanumericp character)
                         (member character '(#\HYPHEN-MINUS #\LOW_LINE) :test
                                 #'char=))
                     character
                     #\HYPHEN-MINUS))
               text))
         (trimmed   (string-trim '(#\HYPHEN-MINUS) mapped)))
    (and (non-empty-string-p trimmed) trimmed)))

(-> task-execution--claim-directory (pathname) boolean)
(defun task-execution--claim-directory (directory)
  "Exclusively claim a new execution DIRECTORY, returning NIL when already occupied.

Keep the claim after admission failure or a crash: an execution identity with
possibly published evidence must never be recycled. Existing directories from
older executions need no claim marker to count as occupied."
  (block nil
    (when (probe-file directory)
      (return nil))
    (let ((claim (merge-pathnames "identity.reservation" directory)))
      (platform-ensure-directories-exist *platform* claim)
      (with-open-stream
          (stream (handler-case (platform-open-new-file *platform* claim)
                    (platform-error (condition)
                      (if (eq (platform-error-reason condition) ':exists)
                          (return nil)
                          (error condition)))))
        (write-byte 1 stream)
        t))))

(-> task-execution-identifier-generate (configuration non-empty-string) string)
(defun task-execution-identifier-generate (configuration root-identifier)
  "Reserve a short child or tool execution identity and its durable directory.

Children also use this identity for their conversation and channel. Share the
conversation reservation namespace and reject both stored conversations and
any prior execution directory below ROOT-IDENTIFIER, including partial claims.
Exclusive marker creation arbitrates concurrent allocators across processes."
  (let ((namespace (namestring (uiop:ensure-directory-pathname
                                (configuration-conversation-root configuration))))
        (root      (task--artifact-group-root configuration root-identifier)))
    (identifier-generate
     :timestamp (get-universal-time)
     :namespace namespace
     :occupied-p
     (lambda (candidate)
       (or (conversation-storage-occupied-p
            (conversation-pathname-for-id configuration candidate))
           (not (task-execution--claim-directory
                 (merge-pathnames (format nil "~A/" candidate) root))))))))

(-> task-orchestrator-reserve-session-orders
    (task-orchestrator (integer 1))
    list)
(defun task-orchestrator-reserve-session-orders (orchestrator count)
  "Reserve COUNT consecutive cross-pool job orders."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (loop repeat count
          collect (incf (task-orchestrator-next-session-order orchestrator)))))

(-> session-job-identifier (session-job) non-empty-string)
(defun session-job-identifier (job)
  "Return JOB's stable model-visible identifier."
  (or (session-job-public-identifier job)
      (job-identifier job)))

(-> session-job-order (session-job) (integer 1))
(defun session-job-order (job)
  "Return JOB's cross-pool order, falling back to its pool index."
  (or (session-job-explicit-order job)
      (job-index job)))

(-> session-job-root-conversation-identifier
    (session-job)
    non-empty-string)
(defun session-job-root-conversation-identifier (job)
  "Return the primary conversation identifier owning JOB's session tree."
  (job-root-identifier job))


;;;; -- Tool Execution Pool --

(-> task--pool-event-listener (keyword list) null)
(defun task--pool-event-listener (channel payload)
  "Relay tool execution lifecycle to session listeners."
  (let ((job (getf payload :job)))
    (when (and (eq channel ':job-lifecycle) (typep job 'tool-execution-job))
      (task-orchestrator-emit
       (session-job-orchestrator job) ':tool-execution-lifecycle
       (list :id                  (session-job-identifier job)
             :tool                (tool-execution-job-tool-name job)
             :status              (getf payload :status)
             :parent-tool-call-id (session-job-parent-call-id job)))))
  nil)

(-> task-orchestrator-create () task-orchestrator)
(defun task-orchestrator-create ()
  "Create the channel's lazy tool worker pool."
  (let ((pool (make-job-pool
               :name "Autolith execution" :job-class 'tool-execution-job
               :maximum-concurrency          (task--environment-integer
                                              "AUTOLITH_EXECUTION_MAX_CONCURRENCY"
                                              *tool-execution-default-maximum-concurrency*
                                              :minimum 1 :maximum *tool-execution-maximum-concurrency*)
               :maximum-batch-size 1 :maximum-live-jobs *tool-execution-maximum-live-jobs*
               :maximum-runtime-milliseconds 0
               :terminal-retention-limit     *tool-execution-terminal-retention-limit*
               :start-threads-p              nil)))
    (job-pool-add-listener pool #'task--pool-event-listener)
    (make-instance 'task-orchestrator :execution-pool pool)))

(-> task-orchestrator-refresh (task-orchestrator &key (:reopen-p boolean)) task-orchestrator)
(defun task-orchestrator-refresh (orchestrator &key reopen-p)
  "Refresh the worker pool. Only explicit resume may reopen it."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (when (and (task-orchestrator-closed-p orchestrator) (not reopen-p))
      (error 'task-error :message "The session job runtime is closed." :tool-name "job.list"))
    (let ((pool (task-orchestrator-execution-pool orchestrator)))
      (task-orchestrator--apply-execution-limits-locked orchestrator)
      (job-pool-add-listener pool #'task--pool-event-listener)
      (handler-case (job-pool-refresh pool)
        (job-pool-closed ()
          (error 'task-error :message "The session job runtime is still shutting down." :tool-name "job.list"))))
    (when reopen-p (setf (task-orchestrator-closed-p orchestrator) nil)))
  orchestrator)

(-> task-orchestrator-session-live-count (task-orchestrator) (integer 0))
(defun task-orchestrator-session-live-count (orchestrator)
  "Return the number of live tool jobs."
  (job-pool-live-count (task-orchestrator-execution-pool orchestrator)))

(-> task-orchestrator-lifecycle-state (task-orchestrator) keyword)
(defun task-orchestrator-lifecycle-state (orchestrator)
  "Return the worker pool lifecycle state."
  (job-pool-lifecycle-state (task-orchestrator-execution-pool orchestrator)))

(-> task-orchestrator-close (task-orchestrator) boolean)
(defun task-orchestrator-close (orchestrator)
  "Stop the tool pool, close completion services, and report whether workers stopped."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-closed-p orchestrator) t))
  (let ((cl-jobpond:*shutdown-timeout-seconds* *task-shutdown-timeout-seconds*))
    (prog1 (job-pool-close (task-orchestrator-execution-pool orchestrator))
      (task-completion-close orchestrator))))

(-> task-orchestrator-detach (task-orchestrator) null)
(defun task-orchestrator-detach (orchestrator)
  "Remove the closed worker graph and listeners before saving the image."
  (handler-case (job-pool-detach (task-orchestrator-execution-pool orchestrator))
    (job-pool-detach-refused (condition)
      (error 'task-error :message   (if (eq (job-pool-detach-refused-reason condition) ':not-closed)
                                        "Session job runtimes must close before they can detach."
                                        "Session job runtimes cannot detach while their threads are alive.")
                         :tool-name "job.list")))
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator) nil))
  nil)

(defmethod tool-runtime-identity ((tool task-orchestrator-tool))
  "Return the scheduler shared by task and job tools."
  (task-orchestrator-tool-orchestrator tool))

(defmethod tool-runtime-close ((tool task-orchestrator-tool))
  "Stop TOOL's shared jobs and reusable scheduler threads."
  (unless (task-orchestrator-close
           (task-orchestrator-tool-orchestrator tool))
    (error 'task-error
           :message   "Task workers did not stop before the shutdown deadline."
           :tool-name (tool-canonical-name tool)))
  nil)

(defmethod tool-runtime-close-priority ((tool task-orchestrator-tool))
  "Stop child task workers before runtimes their shared registries may use."
  100)

(defmethod tool-runtime-resume
    ((tool task-orchestrator-tool) (registry tool-registry))
  "Restart TOOL's scheduler after a non-stopping checkpoint fork."
  (declare (ignore registry))
  (task-orchestrator-refresh
   (task-orchestrator-tool-orchestrator tool)
   :reopen-p t))

(defmethod tool-runtime-detach ((tool task-orchestrator-tool))
  "Remove TOOL's closed shared scheduler graph before image saving."
  (task-orchestrator-detach (task-orchestrator-tool-orchestrator tool)))

(-> task--milliseconds-between (real real) integer)
(defun task--milliseconds-between (start end)
  "Return elapsed milliseconds between internal real times START and END."
  (round (* 1000 (- end start)) internal-time-units-per-second))

(-> task-progress--usage-object-p (t) boolean)
(defun task-progress--usage-object-p (value)
  "Return true for a portable string-keyed provider usage object."
  (and (proper-list-p value)
       (not (null value))
       (every (lambda (entry)
                (and (consp entry)
                     (stringp (first entry))
                     (consp (rest entry))
                     (null (nthcdr 2 entry))))
              value)))

(-> task-progress--usage-summary (t) list)
(defun task-progress--usage-summary (usage)
  "Return a detached bounded provider-counter snapshot of USAGE."
  (cl-llm-provider-api:provider-usage-snapshot usage))

(-> task--terminal-state-p (keyword) boolean)
(defun task--terminal-state-p (state)
  "Return true when STATE is a published terminal task state.

Snapshots carry a state rather than a job, so this is still needed alongside the
pool's own JOB-TERMINAL-P."
  (not (null (member state '(:completed :failed :aborted) :test #'eq))))

(defgeneric session-job-snapshot (job)
                                 (:documentation
                                  "Return a coherent portable lifecycle snapshot for session JOB."))

(defmethod session-job-snapshot ((job tool-execution-job))
  "Return JOB's asynchronous tool execution snapshot."
  (let* ((snapshot (job-snapshot job))
         (progress (getf snapshot :progress)))
    (list :job-id              (session-job-identifier job)
          :execution-id        (session-job-execution-identifier job)
          :owner               (session-job-completion-owner-conversation job)
          :completion-policy   (session-job-completion-policy job)
          :type                :tool
          :owners              (copy-list (job-owner-identifiers job))
          :root-conversation   (session-job-root-conversation-identifier job)
          :state               (getf snapshot :state)
          :detached            (session-job-detached-p job)
          :tool                (tool-execution-job-tool-name job)
           :description        (tool-execution-job-description job)
          :summary             (tool-execution-job-summary job)
          :progress
          (list :duration-ms (getf progress :duration-milliseconds))
          :cleanup
          (with-lock-held ((cl-jobpond::job--lock job))
            (when (tool-execution-job-cleanup-condition job)
              (list :status ':native-cleanup :owner (session-job-identifier job))))
          :result              (copy-tree (getf snapshot :result))
          :cancellation-reason (getf snapshot :cancellation-reason)
          :condition-report    (getf snapshot :condition-report))))

(-> task-orchestrator-list-jobs (task-orchestrator) list)
(defun task-orchestrator-list-jobs (orchestrator)
  "Return all retained tool jobs in admission order."
  (stable-sort (job-pool-list-jobs (task-orchestrator-execution-pool orchestrator))
               #'< :key #'session-job-order))

(-> session-job-visible-to-agent-p (session-job agent) boolean)
(defun session-job-visible-to-agent-p (job viewer)
  "Authorize live lookup only in the initiating channel."
  (equal (session-job-completion-owner-conversation job)
         (conversation-identifier (agent-conversation viewer))))

(-> task-orchestrator--find-job
    (task-orchestrator string)
    (option session-job))
(defun task-orchestrator--find-job (orchestrator identifier)
  "Return IDENTIFIER's retained session job, or NIL when it is absent."
  (find identifier
        (task-orchestrator-list-jobs orchestrator)
        :key  #'session-job-identifier
        :test #'string=))

(defgeneric session-job-live-activity (job &key viewer owner)
  (:documentation
   "Return JOB's lightweight live row, optionally scoped to VIEWER or OWNER."))

(defmethod tool-execution-activity-callback ((runtime task-orchestrator))
  "Capture the executing job so nested workers report to the same owner."
  (let ((job *tool-execution-current-job*))
    (when (and job (eq runtime (session-job-orchestrator job)))
      (lambda (activity)
        (check-type activity string)
        (let ((changed-p nil))
          (with-lock-held ((cl-jobpond::job--lock job))
            (when (member (job-state job) '(:queued :running) :test #'eq)
              (setf (tool-execution-job-activity job)
                    (bounded-string (sanitize-text activity :single-line-p t)
                                    :limit 160)
                    (tool-execution-job-activity-at job) (get-internal-real-time)
                    changed-p t)))
          (when changed-p
            (task-orchestrator-emit
             runtime ':tool-execution-progress
             (list :id (session-job-identifier job)))))))))

(defmethod session-job-live-activity ((job tool-execution-job) &key viewer (owner nil owner-supplied-p))
  "Return tool JOB's bounded live phase when it belongs to the specified OWNER."
  (declare (ignore viewer))
  (let ((state (job-state job)))
    (when (and (member state '(:queued :running) :test #'eq)
               (or (not owner-supplied-p)
                   (and owner (equal owner (session-job-completion-owner-conversation job)))))
      (let ((now (get-internal-real-time)))
        (multiple-value-bind (activity activity-at)
            (with-lock-held ((cl-jobpond::job--lock job))
              (values (tool-execution-job-activity job)
                      (tool-execution-job-activity-at job)))
          (list :id       (session-job-identifier job)
                :type     ':tool
                :index    (session-job-order job)
                :tool     (tool-execution-job-tool-name job)
                :description
                (if (tool-recursive-inference-p (tool-execution-job-tool-name job))
                    (or activity "starting")
                    (let ((description (tool-execution-job-description job)))
                      (if (non-empty-string-p description)
                          description
                          (tool-execution-job-summary job))))
                :activity-age-ms
                (and (or activity-at (job-started-at job))
                     (task--milliseconds-between
                      (or activity-at (job-started-at job)) now))
                :state    state
                :duration-ms
                (and (job-started-at job)
                     (task--milliseconds-between (job-started-at job) now))
                :detached (session-job-detached-p job)))))))

(-> task-orchestrator-live-activities
    (task-orchestrator &key (:viewer (option agent)) (:owner (option string))) list)
(defun task-orchestrator-live-activities (orchestrator &key viewer owner)
  "Return live task rows by VIEWER ancestry and command rows for exact OWNER.
With no OWNER, omit commands; with no VIEWER, do not filter task ancestry."
  (loop for job in (task-orchestrator-list-jobs orchestrator)
        for activity = (session-job-live-activity job :viewer viewer :owner owner)
        when activity
          collect activity))

(-> task-orchestrator-list-visible-jobs
    (task-orchestrator agent)
    list)
(defun task-orchestrator-list-visible-jobs (orchestrator viewer)
  "Return session jobs VIEWER may inspect, in shared admission order."
  (remove-if-not
   (lambda (job) (session-job-visible-to-agent-p job viewer))
   (task-orchestrator-list-jobs orchestrator)))

(-> task-orchestrator-visible-job-named (task-orchestrator t string) (option session-job))
(defun task-orchestrator-visible-job-named (orchestrator viewer identifier)
  "Return VIEWER's visible job whose job or execution identifier is IDENTIFIER.

child.spawn reports both identifiers, and models pass either one."
  (find-if (lambda (job)
             (or (equal identifier (session-job-identifier job))
                 (equal identifier (session-job-execution-identifier job))))
           (task-orchestrator-list-visible-jobs orchestrator viewer)))

(-> task-orchestrator-find-visible-job
    (task-orchestrator string &key (:viewer agent) (:tool-name string))
    session-job)
(defun task-orchestrator-find-visible-job
    (orchestrator identifier &key viewer tool-name)
  "Return VIEWER's visible IDENTIFIER or signal a non-disclosing task error."
  (let ((job (task-orchestrator--find-job orchestrator identifier)))
    (if (and job (session-job-visible-to-agent-p job viewer))
        job
        (error 'task-error
               :message   (format nil "No visible job named ~A exists."
                                  identifier)
               :tool-name tool-name
               :task-id   identifier))))

(defgeneric session-job-cancel (job reason)
  (:documentation
   "Cancel session JOB and return whether it was accepted plus descendant IDs."))

(defmethod session-job-cancel ((job session-job) reason)
  "Cancel JOB's linked subtree across pools before terminal publication."
  (multiple-value-bind (accepted-p cascaded)
      (job-cancel job :reason reason :cascade-p t)
    (when accepted-p
      (session-job-note-cancellation job reason))
    (dolist (descendant cascaded)
      (when (typep descendant 'session-job)
        (session-job-note-cancellation descendant reason)))
    (values (not (null accepted-p))
            (sort (loop for descendant in cascaded
                        when (typep descendant 'session-job)
                          collect (session-job-identifier descendant))
                  #'string<))))

(-> session-job-await
    (session-job (option (real 0)))
    (values list boolean))
(defun session-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return JOB's snapshot plus terminal flag."
  (multiple-value-bind (pool-snapshot terminal-p)
      (job-await job :timeout-seconds timeout-seconds)
    (declare (ignore pool-snapshot))
    (values (session-job-snapshot job) terminal-p)))
