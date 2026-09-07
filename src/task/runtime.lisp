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

(-> task-orchestrator--apply-limits-locked
    (task-orchestrator &key (:refresh-runtime-p boolean))
    null)
(defun task-orchestrator--apply-limits-locked
    (orchestrator &key refresh-runtime-p)
  "Apply environment or hurry-up admission bounds while ORCHESTRATOR is locked.

Hurry-up mode replaces every admission bound with one small number, because an
urgent session should not be spending its remaining budget on child agents.
Cl-jobpond serializes the complete policy update with task submission and worker
claims."
  (let* ((pool       (task-orchestrator-pool orchestrator))
         (hurry-up-p (task-orchestrator-hurry-up-p orchestrator))
         (maximum-concurrency
           (if hurry-up-p
               *task-hurry-up-maximum-agents*
               (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                          *task-default-maximum-concurrency*
                                          :minimum 1
                                          :maximum *task-maximum-concurrency*)))
         (maximum-batch-size
           (if hurry-up-p
               *task-hurry-up-maximum-agents*
               *task-maximum-batch-size*))
         (maximum-live-jobs
           (if hurry-up-p
               *task-hurry-up-maximum-agents*
               *task-maximum-live-jobs*))
         (maximum-runtime-milliseconds
           (if refresh-runtime-p
               (task--environment-integer
                "AUTOLITH_TASK_MAX_RUNTIME_MS"
                *task-default-maximum-runtime-milliseconds*
                :minimum 0)
               (job-pool-maximum-runtime-milliseconds pool))))
    (job-pool-update-limits
     pool
     :maximum-concurrency maximum-concurrency
     :maximum-batch-size maximum-batch-size
     :maximum-live-jobs maximum-live-jobs
     :maximum-runtime-milliseconds maximum-runtime-milliseconds))
  nil)

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
     :maximum-live-jobs *tool-execution-maximum-live-jobs*
     :maximum-runtime-milliseconds
     (job-pool-maximum-runtime-milliseconds pool)))
  nil)

(-> task-orchestrator-set-hurry-up (task-orchestrator boolean) task-orchestrator)
(defun task-orchestrator-set-hurry-up (orchestrator enabled-p)
  "Apply ENABLED-P and its hard admission limits to ORCHESTRATOR.

A state change atomically updates the pool policy and starts or wakes the worker
capacity required by the new concurrency bound. Reapplying the current state is
a no-op, preserving lazy pools before their first job."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (unless (eq (task-orchestrator-hurry-up-p orchestrator) enabled-p)
      (setf (task-orchestrator-hurry-up-p orchestrator) enabled-p
            (task-orchestrator-hurry-up-admission-count orchestrator)
            (if enabled-p
                (job-pool-live-count (task-orchestrator-pool orchestrator))
                0))
      (task-orchestrator--apply-limits-locked orchestrator)))
  orchestrator)


;;;; -- Pool Bounds Seen As Orchestrator State --

(-> task-orchestrator-maximum-concurrency (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-concurrency (orchestrator)
  "Return the child jobs ORCHESTRATOR may run at the same time."
  (job-pool-maximum-concurrency (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-batch-size (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-batch-size (orchestrator)
  "Return the children ORCHESTRATOR accepts in one atomic batch."
  (job-pool-maximum-batch-size (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-live-jobs (task-orchestrator) (integer 1))
(defun task-orchestrator-maximum-live-jobs (orchestrator)
  "Return the combined queued and running children ORCHESTRATOR permits."
  (job-pool-maximum-live-jobs (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-maximum-runtime-milliseconds
    (task-orchestrator)
    (integer 0))
(defun task-orchestrator-maximum-runtime-milliseconds (orchestrator)
  "Return ORCHESTRATOR's wall-clock cap for one child, or zero when disabled."
  (job-pool-maximum-runtime-milliseconds (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-live-count (task-orchestrator) (integer 0))
(defun task-orchestrator-live-count (orchestrator)
  "Return ORCHESTRATOR's admitted queued, running, and finalizing children."
  (job-pool-live-count (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-active-count (task-orchestrator) (integer 0))
(defun task-orchestrator-active-count (orchestrator)
  "Return the children ORCHESTRATOR is currently running on workers."
  (job-pool-active-count (task-orchestrator-pool orchestrator)))

(-> task-orchestrator-execution-live-count (task-orchestrator) (integer 0))
(defun task-orchestrator-execution-live-count (orchestrator)
  "Return ORCHESTRATOR's queued, running, and finalizing tool executions."
  (job-pool-live-count (task-orchestrator-execution-pool orchestrator)))

(-> task-orchestrator-session-live-count (task-orchestrator) (integer 0))
(defun task-orchestrator-session-live-count (orchestrator)
  "Return all live child and tool jobs in ORCHESTRATOR."
  (+ (task-orchestrator-live-count orchestrator)
     (task-orchestrator-execution-live-count orchestrator)))

(-> task-orchestrator-lifecycle-state (task-orchestrator) keyword)
(defun task-orchestrator-lifecycle-state (orchestrator)
  "Return ORCHESTRATOR's :OPEN, :CLOSING, or :CLOSED lifecycle state."
  (job-pool-lifecycle-state (task-orchestrator-pool orchestrator)))

;;;; -- Construction and Refresh --

(-> task-orchestrator-create () task-orchestrator)
(defun task-orchestrator-create ()
  "Create an orchestrator and its lazy worker pools from the environment."
  (let* ((task-pool
           (make-job-pool
            :name "Autolith task"
            :job-class 'task-job
            :maximum-concurrency
            (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                       *task-default-maximum-concurrency*
                                       :minimum 1
                                       :maximum *task-maximum-concurrency*)
            :maximum-batch-size *task-maximum-batch-size*
            :maximum-live-jobs *task-maximum-live-jobs*
            :maximum-runtime-milliseconds
            (task--environment-integer
             "AUTOLITH_TASK_MAX_RUNTIME_MS"
             *task-default-maximum-runtime-milliseconds*
             :minimum 0)
            :terminal-retention-limit *task-terminal-retention-limit*
            :start-threads-p nil))
         (execution-pool
           (make-job-pool
            :name "Autolith execution"
            :job-class 'tool-execution-job
            :maximum-concurrency
            (task--environment-integer
             "AUTOLITH_EXECUTION_MAX_CONCURRENCY"
             *tool-execution-default-maximum-concurrency*
             :minimum 1
             :maximum *tool-execution-maximum-concurrency*)
            :maximum-batch-size 1
            :maximum-live-jobs *tool-execution-maximum-live-jobs*
            :maximum-runtime-milliseconds 0
            :terminal-retention-limit
            *tool-execution-terminal-retention-limit*
            ;; Sessions without child or asynchronous tool work stay single
            ;; threaded so they can save their own image.
            :start-threads-p nil))
         (orchestrator
           (make-instance 'task-orchestrator
                          :pool task-pool
                          :execution-pool execution-pool
                          :maximum-depth
                          (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                                     *task-default-maximum-depth*
                                                     :minimum 1))))
    (job-pool-add-listener task-pool #'task--pool-event-listener)
    (job-pool-add-listener execution-pool #'task--pool-event-listener)
    orchestrator))

(-> task-orchestrator-refresh
    (task-orchestrator &key (:reopen-p boolean))
    task-orchestrator)
(defun task-orchestrator-refresh (orchestrator &key reopen-p)
  "Apply current limits and refresh both reusable worker pools.

A completed shutdown can be reopened only by an explicit runtime resume."
  (let ((task-pool (task-orchestrator-pool orchestrator))
        (execution-pool (task-orchestrator-execution-pool orchestrator)))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (when (and (task-orchestrator-closed-p orchestrator)
                 (not reopen-p))
        (error 'task-error
               :message "The session job runtime is closed."
               :tool-name "job.list"))
      (task-orchestrator--apply-limits-locked
       orchestrator
       :refresh-runtime-p t)
      (task-orchestrator--apply-execution-limits-locked orchestrator)
      (setf (task-orchestrator-maximum-depth orchestrator)
            (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                       *task-default-maximum-depth* :minimum 1))
      (dolist (pool (list task-pool execution-pool))
        (job-pool-add-listener pool #'task--pool-event-listener)
        (handler-case
            (job-pool-refresh pool)
          (job-pool-closed ()
            (error 'task-error
                   :message "The session job runtime is still shutting down."
                   :tool-name "job.list"))))
      (when reopen-p
        (setf (task-orchestrator-closed-p orchestrator) nil))))
  orchestrator)


;;;; -- Listeners --

(defun task-orchestrator-add-listener (orchestrator listener)
  "Register LISTENER for portable task events and return it."
  (check-type listener function)
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (pushnew listener (task-orchestrator-listeners orchestrator) :test #'eq))
  listener)

(defun task-orchestrator-remove-listener (orchestrator listener)
  "Remove LISTENER from ORCHESTRATOR."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator)
          (remove listener (task-orchestrator-listeners orchestrator) :test
                  #'eq)))
  nil)

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

(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((unbounded (string-downcase (task--trim (or value ""))))
         (text (subseq unbounded
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
         (trimmed (string-trim '(#\HYPHEN-MINUS) mapped)))
    (and (non-empty-string-p trimmed) trimmed)))

(-> task-orchestrator--child-name (task-orchestrator (option string)) string)
(defun task-orchestrator--child-name (orchestrator requested-name)
  "Return the readable name a new child of ORCHESTRATOR is admitted under.

A supplied name passes through: the pool bounds it and appends the admission index
to make it unique. A child given no name still gets a readable one, since a task
identifier is what an agent uses to refer to its own children."
  (if (non-empty-string-p requested-name)
      requested-name
      (let ((index (with-lock-held ((task-orchestrator-lock orchestrator))
                     (incf (task-orchestrator-next-name-index orchestrator))))
            (adjectives
              #("amber" "brisk" "calm" "clear" "keen" "quiet" "rapid" "steady"
                "vivid" "wise"))
            (nouns
              #("badger" "falcon" "heron" "lynx" "otter" "raven" "sparrow" "tern"
                "wolf" "wren")))
        (format nil "~A-~A"
                (aref adjectives (mod (1- index) (length adjectives)))
                (aref nouns
                      (mod (floor (1- index) (length adjectives))
                           (length nouns)))))))

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


;;;; -- Pool Events Seen As Task Events --

(defun task--pool-event-listener (channel payload)
  "Re-emit one pool lifecycle event in Autolith's session vocabulary."
  (let ((job (getf payload :job)))
    (when (and (typep job 'session-job) (eq channel :job-lifecycle))
      (let ((status (getf payload :status)))
        (etypecase job
          (task-job
           (task-orchestrator-emit
            (session-job-orchestrator job)
            :task-subagent-lifecycle
            (task-job--lifecycle-event
             job status
             (if (eq status :started) nil (job-result job)))))
          (tool-execution-job
           (task-orchestrator-emit
            (session-job-orchestrator job)
            :tool-execution-lifecycle
            (list :id (session-job-identifier job)
                  :tool (tool-execution-job-tool-name job)
                  :status status
                  :parent-tool-call-id (session-job-parent-call-id job)))))))
  nil))


;;;; -- Shutdown --

(-> task-orchestrator-close (task-orchestrator) boolean)
(defun task-orchestrator-close (orchestrator)
  "Cancel all jobs, stop both pools, and report complete shutdown."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-closed-p orchestrator) t))
  (let ((cl-jobpond:*shutdown-timeout-seconds* *task-shutdown-timeout-seconds*))
    (let ((execution-closed-p
            (job-pool-close (task-orchestrator-execution-pool orchestrator)))
          (task-closed-p
            (job-pool-close (task-orchestrator-pool orchestrator))))
      (and execution-closed-p task-closed-p))))

(-> task-orchestrator-detach (task-orchestrator) null)
(defun task-orchestrator-detach (orchestrator)
  "Remove both closed pool graphs before an image save or registry replacement."
  (dolist (pool (list (task-orchestrator-execution-pool orchestrator)
                      (task-orchestrator-pool orchestrator)))
    (handler-case
        (job-pool-detach pool)
      (job-pool-detach-refused (condition)
        (error 'task-error
               :message
               (if (eq (job-pool-detach-refused-reason condition) :not-closed)
                   "Session job runtimes must close before they can detach."
                   "Session job runtimes cannot detach while their threads are alive.")
               :tool-name "job.list"))))
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
           :message "Task workers did not stop before the shutdown deadline."
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

(defun task--milliseconds-between (start end)
  "Return elapsed milliseconds between internal real times START and END."
  (round (* 1000 (- end start)) internal-time-units-per-second))

(defun task-progress-append-output (progress text)
  "Append streamed TEXT while retaining only a bounded tail.

The tail lives in one reusable growable buffer, so readers must copy it
under the progress lock before releasing it to other threads."
  (with-lock-held ((task-progress-lock progress))
    (let ((buffer (task-progress-output-tail progress))
          (limit *task-progress-output-limit*))
      (unless (and (array-has-fill-pointer-p buffer)
                   (adjustable-array-p buffer))
        (setf buffer (text-buffer-append (text-buffer-create) buffer)
              (task-progress-output-tail progress) buffer))
      (text-buffer-append buffer text)
      (let ((excess (- (fill-pointer buffer) limit)))
        (when (plusp excess)
          (replace buffer buffer :start2 excess)
          (setf (fill-pointer buffer) limit))))
    (setf (task-progress-updated-at progress) (get-internal-real-time)))
  nil)

(-> task-progress--usage-object-p (t) boolean)
(defun task-progress--usage-object-p (value)
  "Return true when VALUE is a portable string-keyed provider usage object."
  (and (listp value)
       (not (null value))
       (every (lambda (entry)
                (and (consp entry)
                     (stringp (first entry))
                     (consp (rest entry))
                     (null (cddr entry))))
              value)))

(-> task-progress--merge-usage (t t) t)
(defun task-progress--merge-usage (aggregate usage)
  "Add portable provider USAGE counters to AGGREGATE without losing metadata."
  (cond
    ((null usage)
     (copy-tree aggregate))
    ((null aggregate)
     (copy-tree usage))
    ((and (task-progress--usage-object-p aggregate)
          (task-progress--usage-object-p usage))
     (let ((merged (copy-tree aggregate)))
       (dolist (entry usage merged)
         (let* ((name (first entry))
                (value (second entry))
                (existing (assoc name merged :test #'string=)))
           (if existing
               (let ((old-value (second existing)))
                 (setf (second existing)
                       (cond
                         ((and (typep old-value '(integer 0))
                               (typep value '(integer 0)))
                          (+ old-value value))
                         ((and (task-progress--usage-object-p old-value)
                               (task-progress--usage-object-p value))
                          (task-progress--merge-usage old-value value))
                         (t
                          (copy-tree value)))))
               (setf merged
                     (append merged (list (copy-tree entry)))))))))
    (t
     (copy-tree usage))))

(defun task-progress-note-status (job status details)
  "Update JOB's normalized progress from one child observer STATUS event."
  (let ((observer (task-orchestrator-status-observer (task-job-orchestrator job))))
    (when observer
      (funcall observer job status details)))
  (let ((progress (task-job-progress job))
        (event nil)
        (now (get-internal-real-time)))
    (with-lock-held ((task-progress-lock progress))
      (case status
        (:provider-request-started
         (setf (task-progress-request-count progress)
               (or (getf details :request-number)
                   (1+ (task-progress-request-count progress)))))
        (:provider-request-completed
         (setf (task-progress-usage progress)
               (task-progress--merge-usage
                (task-progress-usage progress)
                (getf details :usage))))
        (:tool-call-started
         (let ((tool (getf details :tool)))
           (setf (task-progress-current-tool progress) tool
                 (task-progress-current-tool-started-at progress)
                 (and tool now))))
        (:tool-call-completed
         (let ((tool (getf details :tool)))
           (when tool
             (deque-push-back (task-progress-recent-tools progress) tool)))
         (setf (task-progress-current-tool progress) nil
               (task-progress-current-tool-started-at progress) nil)))
      (setf (task-progress-updated-at progress) now
            event
            (list :id (job-identifier job)
                  :status (task-progress-status progress)
                  :current-tool (task-progress-current-tool progress)
                  :request-count (task-progress-request-count progress))))
    (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-progress
                            event)
    ;; Every observed child event is also a cancellation point, so a child whose
    ;; controller gave up stops at its next provider or tool boundary even when
    ;; an interrupt could not be delivered to it.
    (job-check-cancellation job))
  nil)

(-> task--terminal-state-p (keyword) boolean)
(defun task--terminal-state-p (state)
  "Return true when STATE is a published terminal task state.

Snapshots carry a state rather than a job, so this is still needed alongside the
pool's own JOB-TERMINAL-P."
  (not (null (member state '(:completed :failed :aborted) :test #'eq))))

(-> task-job-display-name (task-job) non-empty-string)
(defun task-job-display-name (job)
  "Return the name JOB is presented under in results and transcripts.

A child admitted with a name keeps it; one named for it falls back to its
identifier, which is generated to be readable for this reason. A caller-supplied
name is bounded here, because only the identifier derived from it was."
  (let ((name (job-name job)))
    (if name
        (subseq name 0 (min (length name) *task-identifier-maximum-characters*))
        (job-identifier job))))


;;;; -- Child Steering --


(-> task-job-steering-pending-count (task-job) (integer 0))
(defun task-job-steering-pending-count (job)
  "Return JOB's accepted steering messages not yet durably acknowledged."
  (with-lock-held ((task-job-steering-lock job))
    (+ (deque-count (task-job-steering-items job))
       (deque-count (task-job-steering-in-flight-items job)))))

(-> task-job-response-promotion-pending-count (task-job) (integer 0))
(defun task-job-response-promotion-pending-count (job)
  "Return JOB's accepted steering prompts still awaiting a verbal response."
  (with-lock-held ((task-job-steering-lock job))
    (+ (deque-count (task-job-response-promotion-pending-identifiers job))
       (deque-count (task-job-response-promotion-identifiers job)))))

(-> task-job-enqueue-steering
    (task-job (or string user-message-input)
     &key (:promote-response-p boolean))
    (values (option agent-steering-input) keyword))
(defun task-job-enqueue-steering (job content &key (promote-response-p nil))
  "Atomically accept CONTENT for running JOB, returning an entry and reason.

When PROMOTE-RESPONSE-P is true, durable acknowledgment reserves one FIFO token
for JOB's next verbal response."
  (block nil
    (let* ((copy (user-message-input-copy content))
           (characters (length (user-message-input-text copy))))
      (unless (or (plusp characters)
                  (user-message-input-image-pathnames copy))
        (return (values nil ':empty)))
      (when (> characters *task-steering-maximum-characters*)
        (return (values nil ':content-too-large)))
      (with-lock-held ((cl-jobpond::job--lock job))
        (unless (eq (job-state job) ':running)
          (return (values nil ':not-running)))
        (when (or (job-cancellation-reason job)
                  (cl-jobpond::job--publication-claimed-p job))
          (return (values nil ':closing)))
        (with-lock-held ((task-job-steering-lock job))
          (when (task-job-steering-closed-p job)
            (return (values nil ':closed)))
          (let ((count
                  (+ (deque-count (task-job-steering-items job))
                     (deque-count (task-job-steering-in-flight-items job))))
                (retained-characters
                  (+ (deque-total-weight (task-job-steering-items job))
                     (deque-total-weight
                      (task-job-steering-in-flight-items job)))))
            (when (or (>= count *task-steering-maximum-items*)
                      (and promote-response-p
                           (>= (+ (deque-count
                                   (task-job-response-promotion-pending-identifiers job))
                                  (deque-count
                                   (task-job-response-promotion-identifiers job)))
                               *task-response-promotion-maximum-items*))
                      (> (+ retained-characters characters)
                         *task-steering-maximum-total-characters*))
              (return (values nil ':full))))
          (let ((entry
                  (agent-steering-input-create
                   :identifier (make-identifier)
                   :content copy)))
            (deque-push-back (task-job-steering-items job) entry)
            (when promote-response-p
              (deque-push-back
               (task-job-response-promotion-pending-identifiers job)
               (agent-steering-input-identifier entry)))
            (values entry ':accepted)))))))

(-> task-job-take-steering (task-job) list)
(defun task-job-take-steering (job)
  "Move JOB's queued steering into in-flight state and return it in FIFO order."
  (with-lock-held ((task-job-steering-lock job))
    (let ((entries (deque->list (task-job-steering-items job))))
      (deque-move-all
       (task-job-steering-items job)
       (task-job-steering-in-flight-items job))
      entries)))

(-> task-job-acknowledge-steering (task-job non-empty-string) boolean)
(defun task-job-acknowledge-steering (job identifier)
  "Forget one in-flight steering IDENTIFIER and activate its response promotion."
  (with-lock-held ((task-job-steering-lock job))
    (multiple-value-bind (entry present-p)
        (deque-delete
         identifier
         (task-job-steering-in-flight-items job)
         :key #'agent-steering-input-identifier
         :test #'string=)
      (declare (ignore entry))
      (when present-p
        (multiple-value-bind (pending-identifier promoted-p)
            (deque-delete
             identifier
             (task-job-response-promotion-pending-identifiers job)
             :test #'string=)
          (declare (ignore pending-identifier))
          (when promoted-p
            (deque-push-back
             (task-job-response-promotion-identifiers job)
             identifier))))
      present-p)))

(-> task-job-note-verbal-response (task-job string timestamp) boolean)
(defun task-job-note-verbal-response (job text timestamp)
  "Consume one FIFO promotion token and emit JOB's durable verbal response."
  (unless (and (non-empty-string-p text)
               (plusp (length (task--trim text))))
    (return-from task-job-note-verbal-response nil))
  (let ((steering-identifier
          (with-lock-held ((task-job-steering-lock job))
            (let ((identifiers
                    (task-job-response-promotion-identifiers job)))
              (unless (deque-empty-p identifiers)
                (deque-pop-front identifiers))))))
    (when steering-identifier
      (task-orchestrator-emit
       (task-job-orchestrator job)
       ':task-subagent-verbal-response
       (list :id (session-job-identifier job)
             :execution-id (task-job-execution-identifier job)
             :child-name (copy-seq (task-job-display-name job))
             :steering-id (copy-seq steering-identifier)
             :text (copy-seq text)
             :time timestamp)))
    (not (null steering-identifier))))

(-> task-job-close-steering (task-job) (integer 0))
(defun task-job-close-steering (job)
  "Close JOB's mailbox, release its content, and return undelivered entry count."
  (with-lock-held ((task-job-steering-lock job))
    (let ((count
            (+ (deque-count (task-job-steering-items job))
               (deque-count (task-job-steering-in-flight-items job)))))
      (setf (task-job-steering-closed-p job) t)
      (deque-clear (task-job-steering-items job))
      (deque-clear (task-job-steering-in-flight-items job))
       (deque-clear (task-job-response-promotion-pending-identifiers job))
      (deque-clear (task-job-response-promotion-identifiers job))
      count)))

(-> task-job-identity (task-job) list)
(defun task-job-identity (job)
  "Return JOB's stable identity plist.

The pool owns the identifier and index, so this assembles what a child session is
handed rather than storing a second copy."
  (list :id (job-identifier job)
        :display-name (task-job-display-name job)
        :index (job-index job)))

(-> task-job-root-conversation-identifier (task-job) non-empty-string)
(defun task-job-root-conversation-identifier (job)
  "Return the primary conversation identifier owning JOB's task tree."
  (session-job-root-conversation-identifier job))

(-> task-job-agent-name (task-job) non-empty-string)
(defun task-job-agent-name (job)
  "Return JOB's live or retained child role name."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-name definition)
        (getf (task-job-definition-summary job) :name))))

(-> task-job-agent-source (task-job) keyword)
(defun task-job-agent-source (job)
  "Return JOB's live or retained child role source."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-source definition)
        (getf (task-job-definition-summary job) :source))))

(-> task-progress--snapshot
    (task-job &key (:result t) (:ended-at t))
    list)
(defun task-progress--snapshot (job &key result ended-at)
  "Return JOB progress using lifecycle values captured under the job lock."
  (multiple-value-bind (parent definition agent-name)
      (with-lock-held ((cl-jobpond::job--lock job))
        (let ((definition (task-job-definition job)))
          (values
           (task-job-parent-agent job)
           definition
           (if definition
               (task-agent-definition-name definition)
               (getf (task-job-definition-summary job) :name)))))
    (let ((progress (task-job-progress job)))
      (with-lock-held ((task-progress-lock progress))
        (let ((now (or ended-at (get-internal-real-time))))
          (list :id (job-identifier job)
                :agent agent-name
                :status (task-progress-status progress)
                :current-tool (task-progress-current-tool progress)
                :current-tool-duration-ms
                (and (task-progress-current-tool-started-at progress)
                     (task--milliseconds-between
                      (task-progress-current-tool-started-at progress)
                      now))
                :recent-tools
                (coerce (deque->vector (task-progress-recent-tools progress)) 'list)
                :recent-output (copy-seq (task-progress-output-tail progress))
                :request-count (task-progress-request-count progress)
                :usage (copy-tree (task-progress-usage progress))
                :duration-ms
                (and (task-progress-started-at progress)
                     (task--milliseconds-between
                      (task-progress-started-at progress)
                      now))
                :model
                (or (getf result :model)
                    (and parent
                         definition
                         (configuration-model
                          (task-configuration-for-definition
                           (agent-configuration parent)
                           definition))))))))))

(-> task-progress-snapshot (task-job) list)
(defun task-progress-snapshot (job)
  "Return a coherent portable snapshot of JOB's current progress."
  (let ((snapshot (job-snapshot job)))
    (task-progress--snapshot job
                             :result (getf snapshot :result)
                             :ended-at (getf snapshot :ended-at))))

(-> task-job-snapshot (task-job) list)
(defun task-job-snapshot (job)
  "Return JOB's coherent portable lifecycle, progress, and result snapshot.

The lifecycle fields are read once through the pool snapshot so they cannot mix
values from either side of a terminal transition."
  (let* ((snapshot (job-snapshot job))
         (result (copy-tree (getf snapshot :result)))
         (agent-name
           (with-lock-held ((cl-jobpond::job--lock job))
             (task-job-agent-name job))))
    (list :job-id (session-job-identifier job)
          :execution-id (task-job-execution-identifier job)
          :type :task
          :state (getf snapshot :state)
          :pending-prompt-count (task-job-steering-pending-count job)
          :detached (task-job-detached-p job)
          :agent agent-name
          :assignment
          (bounded-string (getf (task-job-item job) :task)
                          :limit *task-retained-assignment-limit*)
          :progress
          (task-progress--snapshot job
                                   :result result
                                   :ended-at (getf snapshot :ended-at))
          :result result
          :cancellation-reason (getf snapshot :cancellation-reason)
          :condition-report (getf snapshot :condition-report))))

(-> session-job-snapshot (session-job) list)
(defgeneric session-job-snapshot (job)
  (:documentation
   "Return a coherent portable lifecycle snapshot for session JOB."))

(defmethod session-job-snapshot ((job task-job))
  "Return task JOB's child-agent snapshot."
  (task-job-snapshot job))

(defmethod session-job-snapshot ((job tool-execution-job))
  "Return JOB's asynchronous tool execution snapshot."
  (let* ((snapshot (job-snapshot job))
         (progress (getf snapshot :progress)))
    (list :job-id (session-job-identifier job)
          :execution-id (session-job-execution-identifier job)
          :type :tool
          :state (getf snapshot :state)
          :detached (session-job-detached-p job)
          :tool (tool-execution-job-tool-name job)
           :description (tool-execution-job-description job)
          :summary (tool-execution-job-summary job)
          :progress
          (list :duration-ms (getf progress :duration-milliseconds))
          :result (copy-tree (getf snapshot :result))
          :cancellation-reason (getf snapshot :cancellation-reason)
          :condition-report (getf snapshot :condition-report))))

(-> task-orchestrator-list-jobs (task-orchestrator) list)
(defun task-orchestrator-list-jobs (orchestrator)
  "Return every retained child and tool job in shared admission order."
  (stable-sort
   (append (job-pool-list-jobs (task-orchestrator-pool orchestrator))
           (job-pool-list-jobs
            (task-orchestrator-execution-pool orchestrator)))
   #'<
   :key #'session-job-order))

(-> task-orchestrator--find-job
    (task-orchestrator string)
    (option session-job))
(defun task-orchestrator--find-job (orchestrator identifier)
  "Return IDENTIFIER's retained session job, or NIL when it is absent."
  (find identifier
        (task-orchestrator-list-jobs orchestrator)
        :key #'session-job-identifier
        :test #'string=))

(-> session-job-live-activity (session-job) (option list))
(defgeneric session-job-live-activity (job)
  (:documentation
   "Return JOB's lightweight live presentation, or NIL when it has none."))

(defmethod session-job-live-activity ((job task-job))
  "Return task JOB's bounded queued or running activity trace."
  (let ((state (job-state job)))
    (when (member state '(:queued :running) :test #'eq)
      (let ((progress (task-job-progress job)))
        (with-lock-held ((task-progress-lock progress))
          (let ((now (get-internal-real-time)))
            (list :id (session-job-identifier job)
                  :type ':task
                  :index (session-job-order job)
                  :agent (task-job-agent-name job)
                  :state state
                  :pending-prompt-count
                  (task-job-steering-pending-count job)
                  :current-tool (task-progress-current-tool progress)
                  :current-tool-duration-ms
                  (and (task-progress-current-tool-started-at progress)
                       (task--milliseconds-between
                        (task-progress-current-tool-started-at progress)
                        now))
                  :recent-tools
                  (coerce (deque->vector (task-progress-recent-tools progress)) 'list)
                  :request-count (task-progress-request-count progress)
                  :duration-ms
                  (and (task-progress-started-at progress)
                       (task--milliseconds-between
                        (task-progress-started-at progress)
                        now))
                  :assignment
                  (bounded-string
                   (getf (task-job-item job) :task)
                   :limit *task-retained-assignment-limit*)
                  :detached (task-job-detached-p job))))))))

(defmethod session-job-live-activity ((job tool-execution-job))
  "Return a primary-owned tool JOB's queued or running command presentation."
  (let ((state (job-state job)))
    (when (and (null (job-owner-identifiers job))
               (member state '(:queued :running) :test #'eq))
      (let ((now (get-internal-real-time)))
        (list :id (session-job-identifier job)
              :type ':tool
              :index (session-job-order job)
              :tool (tool-execution-job-tool-name job)
              :description
              (let ((description (tool-execution-job-description job)))
                (if (non-empty-string-p description)
                    description
                    (tool-execution-job-summary job)))
              :state state
              :duration-ms
              (and (job-started-at job)
                   (task--milliseconds-between (job-started-at job) now))
              :detached (session-job-detached-p job))))))

(-> task-job-live-activity (task-job) (option list))
(defun task-job-live-activity (job)
  "Return task JOB's lightweight live presentation snapshot."
  (session-job-live-activity job))

(-> task-orchestrator-live-activities (task-orchestrator) list)
(defun task-orchestrator-live-activities (orchestrator)
  "Return stable lightweight snapshots for every presented live job."
  (loop for job in (task-orchestrator-list-jobs orchestrator)
        for activity = (session-job-live-activity job)
        when activity
          collect activity))

(-> session-job-visible-to-agent-p (session-job agent) boolean)
(defun session-job-visible-to-agent-p (job viewer)
  "Return true when VIEWER owns JOB through conversation or task ancestry."
  (not
   (null
    (if (typep viewer 'task-child-agent)
        (member (job-identifier (task-child-agent-job viewer))
                (job-owner-identifiers job)
                :test #'string=)
        (string=
         (session-job-root-conversation-identifier job)
         (conversation-identifier (agent-conversation viewer)))))))

(-> task-orchestrator-list-visible-jobs
    (task-orchestrator agent)
    list)
(defun task-orchestrator-list-visible-jobs (orchestrator viewer)
  "Return session jobs VIEWER may inspect, in shared admission order."
  (remove-if-not
   (lambda (job) (session-job-visible-to-agent-p job viewer))
   (task-orchestrator-list-jobs orchestrator)))

(-> task-orchestrator-find-visible-job
    (task-orchestrator string agent string)
    session-job)
(defun task-orchestrator-find-visible-job
    (orchestrator identifier viewer tool-name)
  "Return VIEWER's visible IDENTIFIER or signal a non-disclosing task error."
  (let ((job (task-orchestrator--find-job orchestrator identifier)))
    (if (and job (session-job-visible-to-agent-p job viewer))
        job
        (error 'task-error
               :message (format nil "No visible job named ~A exists."
                                identifier)
               :tool-name tool-name
               :task-id identifier))))

(-> session-job-cancel
    (session-job keyword)
    (values boolean list))
(defgeneric session-job-cancel (job reason)
  (:documentation
   "Cancel session JOB and return whether it was accepted plus descendant IDs."))

(defmethod session-job-cancel ((job session-job) reason)
  "Cancel JOB and every retained descendant in its own pool."
  (multiple-value-bind (accepted-p cascaded)
      (job-cancel job :reason reason :cascade-p t)
    (values accepted-p
            (sort (mapcar #'session-job-identifier cascaded) #'string<))))

(-> task-job--subtree-identifiers (task-job) list)
(defun task-job--subtree-identifiers (job)
  "Return base identifiers for retained task jobs in JOB's subtree."
  (let ((identifier (job-identifier job)))
    (loop for candidate in
            (job-pool-list-jobs
             (task-orchestrator-pool (task-job-orchestrator job)))
          when (or (eq candidate job)
                   (member identifier
                           (job-owner-identifiers candidate)
                           :test #'string=))
            collect (job-identifier candidate))))

(defmethod session-job-cancel ((job task-job) reason)
  "Cancel task JOB and accepted tool executions owned by its task subtree."
  (multiple-value-bind (accepted-p cancelled-descendants)
      (call-next-method)
    (when accepted-p
      (let ((task-identifiers (task-job--subtree-identifiers job)))
        (dolist (candidate
                 (job-pool-list-jobs
                  (task-orchestrator-execution-pool
                   (task-job-orchestrator job))))
          (when (some (lambda (identifier)
                        (member identifier
                                (job-owner-identifiers candidate)
                                :test #'string=))
                      task-identifiers)
            (multiple-value-bind (cancelled-p cascaded)
                (job-cancel candidate :reason reason :cascade-p t)
              (when cancelled-p
                (push (session-job-identifier candidate)
                      cancelled-descendants))
              (dolist (descendant cascaded)
                (push (session-job-identifier descendant)
                      cancelled-descendants)))))))
    (values accepted-p
            (sort (remove-duplicates cancelled-descendants :test #'string=)
                  #'string<))))

(-> task-job-cancel (task-job keyword) (values boolean list))
(defun task-job-cancel (job reason)
  "Cancel task JOB and every retained child or tool descendant."
  (session-job-cancel job reason))

(-> session-job-await
    (session-job (option (real 0)))
    (values list boolean))
(defun session-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return JOB's snapshot plus terminal flag."
  (multiple-value-bind (pool-snapshot terminal-p)
      (job-await job :timeout-seconds timeout-seconds)
    (declare (ignore pool-snapshot))
    (values (session-job-snapshot job) terminal-p)))

(-> task-job-await
    (task-job (option (real 0)))
    (values list boolean))
(defun task-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return task JOB's snapshot plus terminal flag."
  (session-job-await job timeout-seconds))
