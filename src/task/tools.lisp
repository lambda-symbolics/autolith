(in-package #:autolith)

;;;; -- Task Provider Tools --

(-> task--repair-prose (t) t)
(defun task--repair-prose (value)
  "Repair a provider string that was JSON-encoded one extra time."
  (if (not (stringp value))
      value
      (loop with current = value
            for trimmed = (task--trim current)
            repeat 2
            if (and (> (length trimmed) 1)
                    (char= (char trimmed 0) #\QUOTATION_MARK)
                    (char= (char trimmed (1- (length trimmed)))
                           #\QUOTATION_MARK))
            do (handler-case
                   (let ((decoded (json-decode trimmed)))
                     (if (stringp decoded)
                         (setf current decoded)
                         (return current)))
                 (error nil (return current))) else return current
            finally (return current))))

(-> task--combine-context ((option string) (option string)) (option string))
(defun task--combine-context (shared item)
  "Combine optional SHARED and ITEM context without manufacturing instructions."
  (cond
    ((and (non-empty-string-p shared) (non-empty-string-p item))
     (format nil "~A~2%~A" shared item))
    ((non-empty-string-p shared)
     shared)
    ((non-empty-string-p item)
     item)
    (t
     nil)))

(-> task--validate-json-fields (json-object list string) null)
(defun task--validate-json-fields (object allowed-fields location)
  "Reject fields outside ALLOWED-FIELDS in task JSON OBJECT at LOCATION."
  (loop for field being the hash-keys of object
        unless (member field allowed-fields :test #'string=)
          do (error 'task-error
                    :message
                    (format nil "Unknown child.spawn field ~S in ~A."
                            field location)
                    :tool-name "child.spawn"))
  nil)

(-> task--normalize-item (t) list)
(defun task--normalize-item (object)
  "Validate and normalize one child channel request."
  (unless (json-object-p object)
    (error 'task-error :message "child.spawn requires an object." :tool-name "child.spawn"))
  (task--validate-json-fields object '("name" "task" "context" "isolation") "child.spawn")
  (let ((task    (task--repair-prose (json-get object "task")))
        (name    (json-get object "name"))
        (context (task--repair-prose (json-get object "context"))))
    (unless (non-empty-string-p task)
      (error 'task-error :message "Every child requires a non-empty task assignment."
             :tool-name           "child.spawn"))
    (when (and name (not (non-empty-string-p name)))
      (error 'task-error :message "A supplied task name must be a non-empty string."
             :tool-name           "child.spawn"))
    (when (and name (> (length name) *task-identifier-maximum-characters*))
      (error 'task-error :message
             (format nil "A task name may contain at most ~D characters."
                     *task-identifier-maximum-characters*)
             :tool-name "child.spawn"))
    (when (and (nth-value 1 (gethash "context" object)) (not (stringp context)))
      (error 'task-error :message "A supplied task context must be a string." :tool-name
             "child.spawn"))
    (list :name      name
          :task      task
          :context   context
          :isolation (task-worktree-normalize-options (json-get object "isolation")))))

(-> task-normalize-arguments (hash-table) list)
(defun task-normalize-arguments (arguments)
  "Return the validated child description shared by execution and presentation."
  (task--normalize-item arguments))

(-> task-tool-result
    (t t &key (:success-p t) (:code (option keyword)) (:denied-p t))
    task-tool-result)
(defun task-tool-result (content details &key (success-p t) code denied-p)
  "Return readable CONTENT, portable DETAILS and retained failure classification."
  (unless (and (stringp content)
               (<= (length content) *task-tool-content-limit*))
    (error 'task-error
           :message   "A task tool produced an oversized native result."
           :tool-name "child.spawn"))
  (make-instance 'task-tool-result
                 :content    content
                 :success-p  (and success-p t)
                 :category   (if success-p ':success ':failure)
                 :error-code code
                 :denied-p   denied-p
                 :details    details))

(-> task--validate-tool-arguments (t list string) null)
(defun task--validate-tool-arguments (arguments allowed-fields tool-name)
  "Require an object with only ALLOWED-FIELDS for TOOL-NAME."
  (unless (json-object-p arguments)
    (error 'task-error
           :message   (format nil "~A arguments must be a JSON object." tool-name)
           :tool-name tool-name))
  (loop for field being the hash-keys of arguments
        unless (member field allowed-fields :test #'string=)
          do (error 'task-error
                    :message
                    (format nil "Unknown ~A field ~S." tool-name field)
                    :tool-name tool-name))
  nil)

(-> task--validate-job-identifier (t string) non-empty-string)
(defun task--validate-job-identifier (value tool-name)
  "Return bounded non-empty job identifier VALUE for TOOL-NAME."
  (unless (and (non-empty-string-p value)
               (<= (length value) *task-identifier-maximum-characters*))
    (error 'task-error
           :message
           (format nil "~A requires a non-empty job id of at most ~D characters."
                   tool-name *task-identifier-maximum-characters*)
           :tool-name tool-name))
  value)

(-> task-job-identifier-property () hash-table)
(defun task-job-identifier-property ()
  "Return the public schema for a full job ID or unambiguous visible number."
  (json-object
   "description" "The full session job identifier, or its nonnegative number when it identifies exactly one visible job."
   "anyOf" (vector (tool-string-property "The full session job identifier.")
                   (json-object "type" "integer" "minimum" 0))))

(-> task--numbered-job-identifier
    ((integer 0) &key (:orchestrator task-orchestrator) (:viewer agent) (:tool-name string))
    non-empty-string)
(defun task--numbered-job-identifier (number &key orchestrator viewer tool-name)
  "Return the one visible job identifier ending in :NUMBER, such as exec:NUMBER.

A bare number is shorthand only while it is unambiguous: no match or several
matches signal a TASK-ERROR naming the candidates."
  (let* ((suffix  (format nil ":~D" number))
         (matches (loop for job in (task-orchestrator-list-visible-jobs orchestrator viewer)
                        for identifier = (session-job-identifier job)
                        when (uiop:string-suffix-p identifier suffix)
                          collect identifier)))
    (cond
      ((= (length matches) 1)
       (first matches))
      ((null matches)
       (error 'task-error
              :message   (format nil "No visible job is numbered ~D." number)
              :tool-name tool-name))
      (t
       (error 'task-error
              :message   (format nil "Job number ~D is ambiguous among ~{~A~^, ~}; use the full id."
                                 number (sort matches #'string<))
              :tool-name tool-name)))))

(-> task--resolve-numbered-job-argument
    (hash-table &key (:orchestrator task-orchestrator) (:viewer agent) (:tool-name string))
    hash-table)
(defun task--resolve-numbered-job-argument (arguments &key orchestrator viewer tool-name)
  "Return ARGUMENTS with a numeric \"id\" replaced by the job identifier it names."
  (let ((value (gethash "id" arguments)))
    (if (typep value '(integer 0))
        (let ((resolved (make-hash-table :test (hash-table-test arguments))))
          (maphash (lambda (key entry)
                     (setf (gethash key resolved) entry))
                   arguments)
          (setf (gethash "id" resolved)
                (task--numbered-job-identifier value :orchestrator orchestrator
                                                     :viewer       viewer
                                                     :tool-name    tool-name))
          resolved)
        arguments)))

(-> task--artifact-group-root (configuration string) pathname)
(defun task--artifact-group-root (configuration conversation-identifier)
  "Return the common artifact root for one primary conversation."
  (merge-pathnames
   (format nil "tasks/~A/"
           (or (conversation-identifier-path-fragment conversation-identifier)
               (task--identifier-fragment conversation-identifier)
               "conversation"))
   (config :data-root configuration)))

(-> task--safe-owner-path (configuration pathname) pathname)
(defun task--safe-owner-path (configuration path)
  "Reject links and special-file ancestors within configured private storage."
  (let ((root (uiop:ensure-directory-pathname (config :data-root configuration))))
    (unless (uiop:subpathp path root)
      (error 'task-error :message "Task owner path escaped private storage."))
    (loop for current = path then (uiop:pathname-parent-directory-pathname current)
          while (and current (uiop:subpathp current root))
          for status = (platform-path-status *platform* current :follow-links-p nil)
          when (and status
                    (not (eq (platform-file-status-kind status)
                             (if (and (equal current path)
                                      (not (uiop:directory-pathname-p path)))
                                 :file :directory))))
            do (error 'task-error :message "Task owner storage contains a link or special file.")
          when (equal current root) do (return)))
  path)

(-> task--owner-conversation-identity (configuration t t) pathname)
(defun task--owner-conversation-identity (configuration root owner)
  "Resolve exact OWNER storage under ROOT, without falling back from task to primary storage."
  (unless (every #'shell-log--component-p (list root owner))
    (error 'task-error :message "Invalid task owner conversation identifier."))
  (task--safe-owner-path
   configuration
   (if (equal owner root)
       (conversation-pathname-for-id configuration owner)
       (merge-pathnames (make-pathname :name owner :type "sexp")
                        (merge-pathnames (format nil "~A/" owner)
                                         (task--artifact-group-root configuration root))))))

(-> task--receipt-index (list) hash-table)
(defun task--receipt-index (receipts)
  "Index exact durable RECEIPTS for constant-time delivery checks."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (receipt receipts index)
      (setf (gethash receipt index) t))))

(-> task--owner-receipts
    (configuration t t &key (:cache (option hash-table))) (option hash-table))
(defun task--owner-receipts (configuration root owner &key cache)
  "Index detached receipts from one fully validated physical owner segment.
Version-2 checkpoint headers restate all preceding identifiers; v1 uses its
identity file. Publish nothing until the complete selected segment validates.
CACHE, when supplied, belongs to one caller snapshot, including negative reads."
  (handler-case
      (let* ((identity (task--owner-conversation-identity configuration root owner))
             (key      (namestring identity)))
        (multiple-value-bind (receipts present-p) (and cache (gethash key cache))
          (if present-p
              receipts
              (let ((result
                      (handler-case
                          (progn
                            (task--safe-owner-path configuration
                                                  (conversation-storage-directory-pathname identity))
                            (when-let ((active (conversation-storage-active-pathname identity)))
                              (task--safe-owner-path configuration active)
                              (let* ((header       (conversation--peek-segment-header active))
                                     (conversation (conversation--from-header identity active header))
                                     (found        (task--receipt-index (conversation-pending-input-identifiers conversation))))
                                (conversation--map-segment-records
                                 identity active
                                 (lambda (record)
                                   (when (eq (first record) :message)
                                     (let ((receipt (getf (rest record) :pending-input-identifier)))
                                       (when (non-empty-string-p receipt)
                                         (setf (gethash receipt found) t))))))
                                found)))
                        (error () nil))))
                (when cache (setf (gethash key cache) result))
                result))))
    (error () nil)))

(-> task--artifact-field
    (t keyword &key (:preview-limit integer)
                    (:artifact-available-p boolean))
    t)
(defun task--artifact-field
    (value field &key preview-limit artifact-available-p)
  "Return VALUE inline, a bounded newest progress tail, or an artifact descriptor."
  (cond
    ((null value)
     nil)
    ((and (eq field :progress-output) (stringp value)
          (> (length value) preview-limit))
     (let ((start (- (length value) preview-limit)))
       (list :tail    (subseq value start)
             :omitted (list :field field :characters start))))
    ((stringp value)
     (if (<= (length value) preview-limit)
         value
         (if artifact-available-p
             (list :in-artifact :field field :characters (length value))
             (list :omitted :field field :characters (length value)))))
    (t
     (let ((characters (length (task--write-readable-sexp value))))
       (if (<= characters preview-limit)
           value
           (if artifact-available-p
               (list :in-artifact :field field :characters characters)
               (list :omitted :field field :characters characters)))))))

(-> task--retained-result-field
    (list keyword &key (:preview-limit integer)
                       (:artifact-available-p boolean))
    t)
(defun task--retained-result-field
    (result field &key preview-limit artifact-available-p)
  "Return RESULT FIELD, respecting terminal compaction metadata."
  (let* ((storage-field
            (ecase field
              (:output
               ':output-storage)
              (:error
               ':error-storage)
              (:worktree-error
               ':worktree-error-storage)
              (:label
               ':label-storage)
              (:usage
               ':usage-storage)))
         (characters-field
           (ecase field
             (:output
              ':output-characters)
             (:error
              ':error-characters)
             (:worktree-error
              ':worktree-error-characters)
             (:label
              ':label-characters)
             (:usage
              ':usage-characters)))
         (storage    (getf result storage-field))
         (characters (getf result characters-field)))
    (cond
      ((member storage '(:artifact :omitted) :test #'eq)
       (let* ((value (getf result field))
              (artifact-p
                (and (eq storage :artifact) artifact-available-p))
              (descriptor
                (list :field field :characters characters)))
         (if (and (stringp value) (plusp preview-limit))
             (list :preview
                   (task--retained-prefix value preview-limit)
                   (if artifact-p :in-artifact :omitted)
                   descriptor)
             (if artifact-p
                 (list :in-artifact :field field :characters characters)
                 (list :omitted :field field :characters characters)))))
      (t
       (task--artifact-field (getf result field)
                             field
                             :preview-limit        preview-limit
                             :artifact-available-p artifact-available-p)))))

(-> task--job-native-record
    (list &key (:artifact-path (option string))
               (:preview-limit integer)
               (:include-progress-p boolean))
    list)
(defun task--job-native-record
       (snapshot &key artifact-path (preview-limit 0) include-progress-p)
  "Return one bounded native job record from SNAPSHOT."
  (let* ((state                (getf snapshot :state))
         (terminal-p           (task--terminal-state-p state))
         (result               (getf snapshot :result))
         (progress             (getf snapshot :progress))
         (artifact-available-p (and terminal-p result (getf result :output-path) t))
         (artifact
          (list :path artifact-path :format :sexp :available-p
                (and artifact-available-p t)))
         (result-record
          (and result
               (list :status (getf result :status) :error
                     (task--retained-result-field result :error :preview-limit
                                                  preview-limit :artifact-available-p
                                                  artifact-available-p)
                     :label
                     (task--retained-result-field result :label :preview-limit
                                                  preview-limit :artifact-available-p
                                                  artifact-available-p)
                     :output
                     (task--retained-result-field result :output :preview-limit
                                                  preview-limit :artifact-available-p
                                                  artifact-available-p)
                     :usage
                     (task--retained-result-field result :usage :preview-limit
                                                  preview-limit :artifact-available-p
                                                  artifact-available-p)
                     :duration-ms (getf result :duration-ms) :model (getf result :model)
                     :worktree (getf result :worktree) :worktree-artifact-path
                     (getf result :worktree-artifact-path) :worktree-error
                     (task--retained-result-field result :worktree-error :preview-limit
                                                  preview-limit :artifact-available-p
                                                  artifact-available-p)
                     :artifact artifact))))
    (append
     (list :id (or (getf result :id) (getf snapshot :job-id)) :execution-id (getf snapshot :execution-id)
           :type :task :state state :detached (and (getf snapshot :detached) t) :owner
           (getf snapshot :owner) :completion-policy (getf snapshot :completion-policy)
           :result result-record :cleanup-descendants
           (getf snapshot :cleanup-descendants) :cleanup-worktree
           (getf snapshot :cleanup-worktree) :cleanup-tool-waves
           (getf snapshot :cleanup-tool-waves) :cancellation-reason
           (getf snapshot :cancellation-reason))
     (when include-progress-p
       (list :progress
             (list :status (getf progress :status) :current-tool
                   (getf progress :current-tool) :recent-tools
                   (getf progress :recent-tools) :recent-output
                   (task--artifact-field (getf progress :recent-output) :progress-output
                                         :preview-limit preview-limit
                                         :artifact-available-p nil)
                   :request-count (getf progress :request-count) :usage
                   (task--artifact-field (getf progress :usage) :usage       :preview-limit
                                         preview-limit :artifact-available-p nil)
                   :duration-ms (getf progress :duration-ms) :model
                   (getf progress :model)))))))

(defgeneric session-job-native-record
    (job snapshot viewer &key preview-limit include-progress-p)
  (:documentation "Return one bounded native inspection record for session JOB."))

(defmethod session-job-native-record
    ((job tool-execution-job) snapshot viewer
     &key (preview-limit 0) include-progress-p)
  "Return tool execution JOB's bounded native inspection record."
  (declare (ignore preview-limit))
  (let ((progress (getf snapshot :progress))
        (result   (getf snapshot :result)))
    (append
     (list :id                  (getf snapshot :job-id)
           :execution-id        (getf snapshot :execution-id)
           :type                :tool
           :tool                (tool-execution-job-tool-name job)
            :description        (getf snapshot :description)
           :summary             (getf snapshot :summary)
           :state               (getf snapshot :state)
           :detached            (and (getf snapshot :detached) t)
           :owner               (getf snapshot :owner)
           :completion-policy   (getf snapshot :completion-policy)
           :shell-logs
           (getf (shell-log-job-metadata (agent-configuration viewer) job) :shell-logs)
           :result
           (and result
                (list :status      (getf result :status)
                      :content     (getf result :content)
                      :details     (getf result :details)
                      :shell-logs  (getf result :shell-logs)
                      :duration-ms (getf result :duration-ms)))
           :cleanup             (getf snapshot :cleanup)
           :cancellation-reason (getf snapshot :cancellation-reason)
           :condition-report    (getf snapshot :condition-report))
     (when include-progress-p
       (list :progress
             (list :duration-ms (getf progress :duration-ms)))))))

(-> task--durable-tool-native-record (list pathname integer) list)
(defun task--durable-tool-native-record (snapshot artifact-path preview-limit)
  "Project a durable tool SNAPSHOT with bounded content and retained shell logs."
  (let ((result (getf snapshot :result)))
    (list :id (getf snapshot :job-id) :execution-id (getf snapshot :execution-id)
          :type ':tool :tool (getf snapshot :tool) :state (getf snapshot :state)
          :detached (getf snapshot :detached) :shell-logs (getf result :shell-logs)
          :result (list :status      (getf result :status)
                        :content     (task--artifact-field (getf result :content) :content
                                                           :preview-limit preview-limit
                                                           :artifact-available-p t)
                        :details     (task--artifact-field (getf result :details) :details
                                                           :preview-limit preview-limit
                                                           :artifact-available-p t)
                        :shell-logs  (getf result :shell-logs)
                        :duration-ms (getf result :duration-ms)
                        :artifact    (namestring artifact-path)))))

(-> task--job-native-form
    ((option session-job) list agent &key (:preview-limit integer)
     (:artifact-path (option pathname)) (:wrapper (option function)))
    (values list string))
(defun task--job-native-form
    (job snapshot viewer &key (preview-limit 6000) artifact-path wrapper)
  "Fit a canonical live or durable SNAPSHOT into a bounded native tool result."
  (when artifact-path
    (setf snapshot (copy-list snapshot)
          (getf snapshot :result)
          (append (copy-list (getf snapshot :result))
                  (list :output-path (namestring artifact-path)))))
  (loop for limit = preview-limit then (floor limit 2)
        for record = (cond
                       (job
                        (session-job-native-record job snapshot viewer
                                                   :preview-limit limit :include-progress-p t))
                       ((eq (getf snapshot :type) :tool)
                        (task--durable-tool-native-record snapshot artifact-path limit))
                       (t
                        (task--job-native-record snapshot :artifact-path (namestring artifact-path)
                                                          :preview-limit limit)))
        for form = (if wrapper (funcall wrapper record) (list :job record))
        for content = (task--write-readable-sexp form :pretty-p t)
        when (<= (length content) *task-tool-content-limit*)
          return (values form content)
        when (zerop limit)
          do (error 'task-error
                    :message   "The mandatory job snapshot exceeds its native result bound."
                    :tool-name "job.get")))

(-> task--durable-job-result
    (agent string string)
    (values (option list) (option pathname) (option list) (option keyword)))
(defun task--durable-job-result (viewer identifier tool-name)
  "Return an owned durable result and its identity and terminal state.

Legacy ancestor storage is read only after the continuity record authenticates
VIEWER's exact channel. Ambiguous identifiers are refused."
  (let ((match nil))
    (task-continuity-map-records
     viewer
     (lambda (entry)
       (let ((record (getf entry :record)))
         (when (and record (or (equal identifier (getf record :job-id))
                               (equal identifier (getf record :execution-id))))
           (when match
             (error 'task-error :message "Ambiguous durable job; use its execution ID."
                                :tool-name tool-name :task-id identifier))
           (setf match entry)))))
    (unless match
      (return-from task--durable-job-result (values nil nil nil nil)))
    (let* ((directory     (uiop:pathname-directory-pathname (getf match :path)))
           (result-path   (merge-pathnames "result.sexp" directory))
           (terminal-path (merge-pathnames "terminal.sexp" directory)))
      (handler-case
          (cond
            ((probe-file terminal-path)
             (let ((terminal (task-continuity--read terminal-path)))
               (values (getf terminal :result) terminal-path (getf match :record)
                       (getf terminal :state))))
            ((probe-file result-path)
             (values (task--read-result-artifact result-path) result-path
                     (getf match :record) nil))
            (t
             (values nil nil nil nil)))
        (task-error (condition)
          (error condition))
        (error (condition)
          (error 'task-error :message (format nil "Could not read durable job result ~A: ~A"
                                              identifier condition)
                             :tool-name tool-name :task-id identifier))))))

(-> task-query-job
    (task-orchestrator agent &key (:identifier string) (:tool-name string)
     (:execution-alias-p boolean))
    (values (option list) (option session-job) (option pathname)))
(defun task-query-job
       (orchestrator viewer &key identifier tool-name (execution-alias-p t))
  "Return a canonical snapshot, retained job and durable result pathname.

Live jobs take precedence. Durable lookup requires VIEWER's exact ownership.
EXECUTION-ALIAS-P also accepts a live job's execution identifier. Missing
jobs return three NIL values; each frontend owns its error and presentation."
  (if-let ((job
            (if execution-alias-p
                (task-orchestrator-visible-job-named orchestrator viewer identifier)
                (find identifier
                      (task-orchestrator-list-visible-jobs orchestrator viewer) :key
                      #'session-job-identifier :test #'string=))))
    (values (session-job-snapshot job) job nil)
    (multiple-value-bind (result path identity terminal-state)
        (task--durable-job-result viewer identifier tool-name)
      (values
       (when result
         (list :job-id (getf identity :job-id) :execution-id (getf identity :execution-id)
               :type (getf identity :kind) :parent-id (getf result :parent-id)
               :tool (or (getf result :tool) (getf identity :tool)) :state
               (or terminal-state (case (getf result :status)
                                   (:success
                                    ':completed)
                                   (:aborted
                                    ':aborted)
                                   (otherwise
                                    ':failed)))
               :detached (and (getf result :detached) t) :progress
               (list :duration-ms (getf result :duration-ms) :usage (getf result :usage))
               :result result))
       nil path))))

(-> session-job-handoff-form (tool-execution-job agent keyword) (values list string))
(defun session-job-handoff-form (job viewer reason)
  "Return background JOB's handoff form for VIEWER and that form as text.

REASON says why the job runs in the background. Every presentation of a
handoff reads this form, whoever started the job."
  (task--job-native-form
   job (session-job-snapshot job) viewer
   :wrapper
   (lambda (record)
     (list :tool-execution
           :handed-off-p   t
           :handoff-reason reason
           :job            record))))

(-> task--tool-execution-handoff-result
    (tool-execution-job agent keyword)
    task-tool-result)
(defun task--tool-execution-handoff-result (job viewer reason)
  "Detach JOB and return its inspectable identity with handoff REASON.

The content opens with how the result will reach VIEWER, so the model does not
poll a job whose completion will wake it."
  (task-completion-watch job viewer)
  (multiple-value-bind (form content) (session-job-handoff-form job viewer reason)
    (task-tool-result
     (format nil "~A~%~A" (task--handoff-guidance job viewer) content)
     form)))

(-> task--handoff-guidance (session-job agent) string)
(defun task--handoff-guidance (job viewer)
  "Return one sentence telling VIEWER how detached JOB's result will arrive."
  (let ((identifier (session-job-identifier job)))
    (cond
      ((not (task-completion-wakeup-connected-p viewer))
       (format nil "Job ~A runs in the background. This interface has no automatic wakeup; use job.get when you need its result."
               identifier))
      ((eq (session-job-completion-policy job) ':notify)
       (format nil "Job ~A runs in the background. Its completion notice arrives with your next turn; do not poll it."
               identifier))
      (t
       (format nil "Job ~A runs in the background. When it finishes, its result preview wakes you as new input. Continue independent work or end your turn."
               identifier)))))

(defmethod tool-execution-invoke
    ((runtime task-orchestrator) parent
     &key tool-name description summary operation-function async-p parent-call-id
       (completion-policy ':continue))
  "Run one operation exactly once, returning quickly or handing off its same job."
  (unless (typep parent 'agent)
    (error 'tool-error
           :message   "Inspectable execution requires an executing agent context."
           :tool-name tool-name))
  (let* ((overflow-function *tool-result-overflow-function*)
         (job
           (task-orchestrator-start-execution-job
            runtime parent
            :tool-name         tool-name
            :description       description
            :summary           summary
            :operation-function
            (lambda ()
              (let ((*tool-result-overflow-function* overflow-function))
                (funcall operation-function)))
            :detached-p        async-p
            :completion-policy completion-policy
            :parent-call-id    parent-call-id)))
    (if async-p
        (task--tool-execution-handoff-result job parent ':requested)
        (multiple-value-bind (snapshot terminal-p)
            (session-job-await job *tool-execution-blocking-grace-seconds*)
          (declare (ignore snapshot))
          (if terminal-p
              (tool-execution-job-result->tool-result job)
              (task--tool-execution-handoff-result
               job parent ':grace-expired))))))

(-> task--bounded-page (list integer integer
                         &key (:tag keyword) (:entry-key keyword) (:project function)
                         (:tool-name string) (:message string))
    (values list string))
(defun task--bounded-page (entries offset limit &key tag entry-key (project #'identity)
                                                  tool-name message)
  "Return a native page and its text, stopping before the content bound."
  (let* ((total      (length entries))
         (end        (min total (+ offset limit)))
         (candidates (subseq entries (min offset total) end))
         (selected   nil))
    (flet ((page-form (rows)
             "Construct the page envelope for ROWS."
             (let* ((count (length rows))
                    (next  (+ offset count)))
               (list tag :offset offset :count count :total total
                     :next-offset (and (< next total) next) entry-key rows))))
      (dolist (entry candidates)
        (let* ((trial   (append selected (list (funcall project entry))))
               (content (task--write-readable-sexp (page-form trial) :pretty-p t)))
          (if (<= (length content) *task-tool-content-limit*)
              (setf selected trial)
              (return))))
      (when (and candidates (null selected))
        (error 'task-error :message message :tool-name tool-name))
      (let ((form (page-form selected)))
        (values form (task--write-readable-sexp form :pretty-p t))))))

(-> task--job-list-summary (list) list)
(defun task--job-list-summary (snapshot)
  "Return the compact job-list summary of SNAPSHOT."
  (append (list :id (getf snapshot :job-id) :type (getf snapshot :type))
          (when (getf snapshot :tool)
            (list :tool (getf snapshot :tool) :summary (getf snapshot :summary)))
          (list :state (getf snapshot :state) :detached (and (getf snapshot :detached) t)
                :status (getf (getf snapshot :result) :status))))

(-> task--job-list-page (list integer integer) (values list string))
(defun task--job-list-page (snapshots offset limit)
  "Return a content-aware native page of compact job summaries."
  (task--bounded-page snapshots offset limit
                      :tag       ':job-list
                      :entry-key ':jobs
                      :project   #'task--job-list-summary
                      :tool-name "job.list"
                      :message   "One job summary exceeds the native result bound."))

(defmethod tool-execute ((tool task-run-tool) (context tool-context) arguments)
  "Create an owned child channel, then send its first message normally."
  (declare (ignore tool))
  (let ((parent (tool-context-agent context)))
    (unless (typep parent 'agent)
      (error 'task-error :tool-name "child.spawn" :message "child.spawn requires an executing agent."))
    (let* ((item          (task-normalize-arguments arguments))
           (configuration (agent-configuration parent))
           (origin        (agent-conversation parent))
           (mission       (mission-context-find origin))
           (parent-id     (conversation-identifier origin))
           (identifier    (conversation-identifier-generate (configuration-conversation-root configuration)))
           (isolation     (getf item :isolation))
           (authorize     (and isolation (lambda (command directory)
                                           (tool-context-authorize-command context command directory))))
           (job           *tool-execution-current-job*)
           (child         nil)
           (metadata      nil))
      (world-child-check-admission parent)
      (let ((lease (conversation-lease-acquire configuration identifier)))
        (unwind-protect
             (unwind-protect
                  (progn
                    (when isolation
                      (multiple-value-setq (configuration metadata)
                        (task-worktree-configuration
                         parent-id identifier configuration :isolation isolation :authorize authorize
                         :cancel-function (and job (lambda () (job-cancellation-requested-p job))))))
                    (setf child (world-child-create parent :name            (getf item :name)
                                                           :identifier      identifier
                                                           :configuration   configuration
                                                           :permission-mode (task-child--permission-mode context)
                                                           :lease           lease)))
               (when (and isolation (null child))
                 (handler-case
                     (unless (world-channel-find (world-load configuration) identifier)
                       (task-worktree-discard configuration identifier :parent-identifier parent-id
                                                                       :authorize         authorize))
                   (error (cause)
                     (warn 'world-session-warning :pathname (configuration-world-path configuration)
                                                  :cause    cause)))))
          (conversation-lease-release lease)))
      (let* ((text        (format nil "~A~@[~2%~A~]~@[~2%~A~]"
                                  (getf item :task) (getf item :context)
                                  (when metadata
                                    "Use this child's isolated worktree. Stage new files before reporting back so the parent can inspect the complete patch.")))
             (receipt     (if mission
                              (with-recursive-lock-held ((mission-context-request-lock mission))
                                (mission-context-bind-child origin child :context mission)
                                (world-child-send parent child text))
                              (world-child-send parent child text)))
             (world       (world-load configuration))
             (resident    (find ':agent (world-channel-residents world identifier) :key #'participant-kind))
             (handle      (and resident (participant-name resident)))
             (delivered-p (not (null (member identifier (getf receipt :channels) :test #'string=))))
             (form        (list :child-spawn :channel identifier :name handle :message-id (getf receipt :id)
                                             :delivery (if delivered-p ':delivered ':pending))))
        (task-tool-result (task--write-readable-sexp form :pretty-p t) form)))))

(defmethod tool-execute ((tool task-job-tool) (context tool-context) arguments)
  "Execute the job operation named by TOOL."
  (let* ((viewer       (tool-context-agent context))
         (orchestrator (task-job-tool-orchestrator tool))
         (operation    (tool-operation tool)))
    (unless (typep viewer 'agent)
      (error 'task-error
             :message   "Job tools require an executing agent context."
             :tool-name (tool-canonical-name tool)))
    (setf arguments (task--resolve-numbered-job-argument
                     arguments :orchestrator orchestrator
                               :viewer       viewer
                               :tool-name    (tool-canonical-name tool)))
    (case operation
      (:list
       (task--validate-tool-arguments arguments '("offset" "limit")
                                      "job.list")
       (let ((offset (or (tool-argument arguments "offset") 0))
             (limit  (or (tool-argument arguments "limit")
                         *task-job-page-default*)))
         (unless (and (integerp offset) (<= 0 offset 1000000))
           (error 'task-error
                  :message   "job.list offset must be an integer from 0 to 1000000."
                  :tool-name "job.list"))
         (unless (and (integerp limit)
                      (<= 1 limit *task-job-page-maximum*))
           (error 'task-error
                  :message
                  (format nil "job.list limit must be an integer from 1 to ~D."
                          *task-job-page-maximum*)
                  :tool-name "job.list"))
         (let* ((jobs
                  (task-orchestrator-list-visible-jobs orchestrator viewer))
                (snapshots (mapcar #'session-job-snapshot jobs)))
           (multiple-value-bind (form content)
               (task--job-list-page snapshots offset limit)
             (task-tool-result content form)))))
      (:get
       (task--validate-tool-arguments arguments '("id") "job.get")
       (let* ((identifier
                (task--validate-job-identifier
                 (tool-argument arguments "id" :required t)
                 "job.get"))
              (tool-name "job.get"))
          (multiple-value-bind (snapshot job path)
              (task-query-job orchestrator viewer :identifier identifier :tool-name tool-name)
            (unless snapshot
              (error 'task-error
                     :message (format nil "No visible job named ~A exists. Jobs from before a restart, and jobs that ended without a result, are listed by job.continuity."
                                      identifier)
                     :tool-name tool-name :task-id identifier))
            (multiple-value-bind (form content)
                (task--job-native-form
                 job snapshot viewer
                 :artifact-path path
                 :preview-limit (if job 6000 *task-result-preview-limit*)
                 :wrapper       (lambda (record)
                                  (list :job-get :durable-p (null job) :job record)))
              (task-tool-result content form)))))
      (:cancel
       (let ((tool-name (tool-canonical-name tool)))
         (task--validate-tool-arguments arguments '("id") tool-name)
         (let* ((identifier (task--validate-job-identifier
                             (tool-argument arguments "id" :required t) tool-name))
                (job        (task-orchestrator-find-visible-job
                             orchestrator identifier :viewer viewer :tool-name tool-name)))
           (multiple-value-bind (accepted-p cancelled-descendants) (session-job-cancel job :user)
             (multiple-value-bind (form content)
                 (task--job-native-form
                  job (session-job-snapshot job) viewer
                  :wrapper (lambda (record)
                             (list :job-cancel :id identifier :accepted-p (and accepted-p t)
                                               :reason ':user :cancelled-descendants cancelled-descendants
                                               :job record)))
               (task-tool-result content form))))))
      (t
       (error 'task-error :message
              (format nil "Unknown job operation ~(~A~)." operation) :tool-name
              (tool-canonical-name tool))))))

(-> task-run-parameters-schema () hash-table)
(defun task-run-parameters-schema ()
  "Return the flat schema advertised by child.spawn."
  (tool-object-schema
   (json-object "name" (tool-string-property "Optional stable child name.") "task"
                (tool-string-property "Self-contained child assignment.") "context"
                (tool-string-property "Optional background for this child.") "isolation"
                  (task-worktree-options-schema))
   '("task")))

(-> task-augment-tool-registry (tool-registry) tool-registry)
(defun task-augment-tool-registry (registry)
  "Register one session-scoped task orchestrator and its task/job tools."
  (when (tool-registry-find registry "child" "spawn")
    (return-from task-augment-tool-registry registry))
  (tool-registry-describe-namespace registry "child"
                                    "Spawn and manage your child agents.")
  (tool-registry-describe-namespace registry "job"
                                    "Inspect, cancel, and recover jobs owned by this channel.")
  (let* ((orchestrator (task-orchestrator-create))
         (identifier-schema
          (tool-object-schema (json-object "id" (task-job-identifier-property)) '("id")))
         (job-list-schema
          (tool-object-schema
           (json-object "offset"
                        (tool-integer-property "Zero-based job offset; defaults to 0.")
                        "limit"
                        (tool-integer-property
                         (format nil "Page size from 1 to ~D; defaults to ~D."
                                 *task-job-page-maximum* *task-job-page-default*)))
           nil)))
    (tool-registry-bind-runtime registry 'task-orchestrator orchestrator)
    (tool-registry-register registry
                            (make-instance 'task-run-tool :orchestrator orchestrator
                                           :namespace "child" :name "spawn" :description
                                            "Create a persistent child channel and send its first message. You are subscribed to the child."
                                           :parameters                  (task-run-parameters-schema)))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "list" :description
                                           "List this channel's tool execution jobs."
                                           :parameters                  job-list-schema))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "get" :description
                                           "Inspect one live session job or a completed task result persisted for the current primary conversation."
                                           :parameters                  identifier-schema))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "cancel" :description
                                           "Request interruption of one queued or running session job."
                                           :parameters                  identifier-schema))
    (tool-registry-register registry
                            (make-instance 'task-continuity-tool :orchestrator
                                           orchestrator :namespace "job" :name
                                           "continuity" :description
                                           "Classify owned durable jobs and explicitly reattach, reconstruct, safely restart, or abandon unfinished work. Recovery requires primary-owner authorization and a durable replay claim."
                                           :parameters (task-continuity-parameters)))
    (task-worktree-register-tool registry)
    (task-children-register-tools registry orchestrator)
    registry))
