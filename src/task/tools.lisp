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

(-> task--normalize-item
    (t &key (:shared-context (option string)) (:top-completion-policy keyword))
    list)
(defun task--normalize-item
       (object &key shared-context (top-completion-policy ':continue))
  "Validate and normalize one flat task OBJECT."
  (unless (json-object-p object)
    (error 'task-error :message "Every tasks item must be a JSON object." :tool-name
           "child.spawn"))
  (task--validate-json-fields object
                              '("name" "task" "context" "isolation" "completion-policy")
                              "a task item")
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
    (list :name name :task task :context (task--combine-context shared-context context)
          :isolation (task-worktree-normalize-options (json-get object "isolation"))
          :completion-policy
          (tool-completion-policy-argument object :tool-name "child.spawn" :default
                                           top-completion-policy))))

(-> task-normalize-arguments (hash-table) list)
(defun task-normalize-arguments (arguments)
  "Return the validated single child description shared by execution and presentation."
  (task--validate-json-fields arguments
                              '("name" "task" "context" "isolation" "completion-policy")
                              "the top-level call")
  (list (append (task--normalize-item arguments) (list :index 1 :mode ':detached))))

(-> task--admit-items (agent task-orchestrator list) list)
(defun task--admit-items (parent orchestrator items)
  "Admit generic assignments subject to structural nesting depth."
  (unless (< (task-parent-depth parent) (task-orchestrator-maximum-depth orchestrator))
    (error 'task-error :message "The current child depth does not permit nested delegation."
                      :code ':authority :tool-name "child.spawn"))
  (mapcar (lambda (item) (list :item item :detached (eq (getf item :mode) ':detached))) items))

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
     (list :id (getf snapshot :job-id) :execution-id (getf snapshot :execution-id) :type
           :task :state state :detached (and (getf snapshot :detached) t) :owner
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
    ((job task-job) snapshot viewer
     &key (preview-limit 0) include-progress-p)
  "Return task JOB's artifact-aware native inspection record."
  (let* ((root
           (task--artifact-group-root
            (agent-configuration viewer)
            (task-job-root-conversation-identifier job)))
         (artifact-path
           (namestring
            (merge-pathnames
             (format nil "~A/result.sexp"
                     (task-job-execution-identifier job))
             root))))
    (task--job-native-record
     snapshot
     :artifact-path      artifact-path
     :preview-limit      preview-limit
     :include-progress-p include-progress-p)))

(-> task--tool-result-content (t integer (option string)) t)
(defun task--tool-result-content (content preview-limit artifact-path)
  "Return CONTENT inline, or its leading preview and where the rest is kept."
  (if (or (not (stringp content)) (<= (length content) preview-limit))
      content
      (list :preview (task--retained-prefix content preview-limit)
            (if artifact-path :in-artifact :omitted)
            (list* :field :content :characters (length content)
                   (when artifact-path (list :path artifact-path))))))

(-> task--tool-result-artifact-path (tool-execution-job list agent) (option string))
(defun task--tool-result-artifact-path (job result viewer)
  "Return where tool JOB's complete terminal RESULT is retained, or NIL."
  (or (getf result :output-path)
      (let ((path (merge-pathnames
                   (format nil "~A/terminal.sexp" (session-job-execution-identifier job))
                   (task--artifact-group-root
                    (agent-configuration viewer)
                    (session-job-root-conversation-identifier job)))))
        (and (probe-file path) (namestring path)))))

(defmethod session-job-native-record
    ((job tool-execution-job) snapshot viewer
     &key (preview-limit 0) include-progress-p)
  "Return tool execution JOB's native inspection record.

A NIL PREVIEW-LIMIT keeps the result whole. An integer bounds its content and
details, pointing at the retained terminal artifact when one exists."
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
                (if preview-limit
                    (let ((artifact-path (task--tool-result-artifact-path job result viewer)))
                      (list :status      (getf result :status)
                            :content     (task--tool-result-content
                                          (getf result :content) preview-limit artifact-path)
                            :details     (task--artifact-field
                                          (getf result :details) :details
                                          :preview-limit        preview-limit
                                          :artifact-available-p (and artifact-path t))
                            :shell-logs  (getf result :shell-logs)
                            :duration-ms (getf result :duration-ms)))
                    (list :status      (getf result :status)
                          :content     (getf result :content)
                          :details     (getf result :details)
                          :shell-logs  (getf result :shell-logs)
                          :duration-ms (getf result :duration-ms))))
           :cleanup             (getf snapshot :cleanup)
           :cancellation-reason (getf snapshot :cancellation-reason)
           :condition-report    (getf snapshot :condition-report))
     (when include-progress-p
       (list :progress
             (list :duration-ms (getf progress :duration-ms)))))))

(-> task--task-run-native-form
    (list list &key (:duration-milliseconds integer)
                    (:artifact-root string)
                    (:success-p boolean))
    (values list string))
(defun task--task-run-native-form
    (jobs snapshots &key duration-milliseconds artifact-root success-p)
  "Fit every admitted JOB into one fair bounded native child.spawn manifest."
  (loop with preview-limit = *task-result-preview-limit*
        for records =
          (loop for job in jobs
                for snapshot in snapshots
                collect
                (task--job-native-record
                 snapshot
                 :artifact-path
                 (format nil "~A/result.sexp"
                         (task-job-execution-identifier job))
                 :preview-limit preview-limit))
        for form =
          (list :task-run
                :succeeded-p (and success-p t)
                :total-duration-ms duration-milliseconds
                :artifact-root     artifact-root
                :results           records)
        for content = (task--write-readable-sexp form :pretty-p t)
        when (<= (length content) *task-tool-content-limit*)
          return (values form content)
        when (zerop preview-limit)
          do (error 'task-error
                    :message
                    "The mandatory child.spawn manifest exceeds its native result bound."
                    :tool-name "child.spawn")
        do (setf preview-limit (floor preview-limit 2))))

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
  ;; A tool job is first shown whole, as before bounding existed, and is bounded
  ;; only when that does not fit.
  (loop for limit = (if (typep job 'tool-execution-job) nil preview-limit)
          then (if limit (floor limit 2) preview-limit)
        for record = (if job
                         (session-job-native-record job snapshot viewer
                                                    :preview-limit limit :include-progress-p t)
                         (task--job-native-record snapshot :artifact-path (namestring artifact-path)
                                                          :preview-limit  limit))
        for form = (if wrapper (funcall wrapper record) (list :job record))
        for content = (task--write-readable-sexp form :pretty-p t)
        when (<= (length content) *task-tool-content-limit*)
          return (values form content)
        when (eql limit 0)
          do (error 'task-error
                    :message   "The mandatory job snapshot exceeds its native result bound."
                    :tool-name (if artifact-path "job.get" "job.wait"))))

(-> task--durable-job-result
    (agent string string)
    (values (option list) (option pathname)))
(defun task--durable-job-result (viewer identifier tool-name)
  "Return VIEWER's durable task result named by execution IDENTIFIER.

Only the current primary conversation's artifact root is searched."
  (when (typep viewer 'task-child-agent)
    (return-from task--durable-job-result (values nil nil)))
  (let* ((fragment (task--identifier-fragment identifier))
         (root
           (task--artifact-group-root
            (agent-configuration viewer)
            (conversation-identifier (agent-conversation viewer))))
         (path
           (and fragment
                (merge-pathnames
                 (format nil "~A/result.sexp" fragment)
                 root))))
    (unless (and path (probe-file path))
      (return-from task--durable-job-result (values nil nil)))
    (handler-case
        (values (task--read-result-artifact path) path)
      (task-error (condition)
        (error condition))
      (error (condition)
        (error 'task-error
               :message   (format nil
                                  "Could not read durable job result ~A: ~A"
                                  identifier condition)
               :tool-name tool-name
               :task-id   identifier)))))

(-> task-query-job
    (task-orchestrator agent &key (:identifier string) (:tool-name string)
     (:execution-alias-p boolean))
    (values (option list) (option session-job) (option pathname)))
(defun task-query-job
       (orchestrator viewer &key identifier tool-name (execution-alias-p t))
  "Return a canonical snapshot, retained job and durable result pathname.

Live jobs take precedence. Durable lookup uses only VIEWER's primary artifact
root. EXECUTION-ALIAS-P also accepts a live job's execution identifier. Missing
jobs return three NIL values; each frontend owns its error and presentation."
  (if-let ((job
            (if execution-alias-p
                (task-orchestrator-visible-job-named orchestrator viewer identifier)
                (find identifier
                      (task-orchestrator-list-visible-jobs orchestrator viewer) :key
                      #'session-job-identifier :test #'string=))))
    (values (session-job-snapshot job) job nil)
    (multiple-value-bind (result path)
        (task--durable-job-result viewer identifier tool-name)
      (values
       (when result
         (list :job-id (or (getf result :id) identifier) :execution-id identifier
               :parent-id (getf result :parent-id) :tool (getf result :tool) :state
               (case (getf result :status)
                 (:success
                  ':completed)
                 (:aborted
                  ':aborted)
                 (otherwise
                  ':failed))
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
       (format nil "Job ~A runs in the background. Continue independent work, then use job.wait when you need its result."
               identifier))
      ((eq (session-job-completion-policy job) ':notify)
       (format nil "Job ~A runs in the background. Its completion notice arrives with your next turn; do not poll it."
               identifier))
      (t
       (format nil "Job ~A runs in the background. When it finishes, its result preview wakes you as new input; do not poll it with job.get or job.wait. Continue independent work or end your turn."
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

(defgeneric task-context-spawn-items (context arguments)
  (:documentation "Normalize child assignments with the originating execution context."))

(defmethod task-context-spawn-items ((context tool-context) arguments)
  "Normalize the ordinary model-visible child assignment."
  (declare (ignore context))
  (task-normalize-arguments arguments))

(defmethod tool-execute ((tool task-run-tool) (context tool-context) arguments)
  "Validate, admit, and detach one child agent."
  (let ((parent (tool-context-agent context)))
    (unless (typep parent 'agent)
      (error 'task-error :message
             "child.spawn requires an executing parent agent context." :tool-name
             "child.spawn"))
    (let* ((orchestrator (task-orchestrator-refresh (task-run-tool-orchestrator tool)))
           (items        (task-context-spawn-items context arguments))
           (resolved     (task--admit-items parent orchestrator items))
           (jobs
            (task-orchestrator-start-jobs orchestrator parent resolved :parent-call-id
                                          (tool-context-call-id context)
                                          :command-authorization-function
                                          (tool-context-command-authorization-function
                                           context)
                                          :tool-authorization-function
                                          (tool-context-tool-authorization-function
                                           context)))
           (snapshots    (mapcar #'task-job-snapshot jobs))
           (duration
            (if jobs
                (task--milliseconds-between (reduce #'min jobs :key #'job-created-at)
                                            (get-internal-real-time))
                0))
           (artifact-root
            (namestring
             (task--artifact-group-root (agent-configuration parent)
                                        (task-parent-root-conversation-identifier
                                         parent)))))
      (multiple-value-bind (form content)
          (task--task-run-native-form jobs snapshots :duration-milliseconds duration
                                      :artifact-root artifact-root :success-p t)
        (task-tool-result content form)))))

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
      ((:wait :cancel)
       (task--validate-tool-arguments
        arguments
        (if (eq operation ':wait)
            '("id" "timeout-seconds")
            '("id"))
        (tool-canonical-name tool))
       (let* ((identifier
                (task--validate-job-identifier
                 (tool-argument arguments "id" :required t)
                 (tool-canonical-name tool)))
              (job (task-orchestrator-find-visible-job
                    orchestrator
                    identifier
                    :viewer    viewer
                    :tool-name (tool-canonical-name tool))))
         (if (eq operation ':cancel)
             (multiple-value-bind (accepted-p cancelled-descendants)
                 (session-job-cancel job :user)
               (let ((snapshot (session-job-snapshot job)))
                 (multiple-value-bind (form content)
                     (task--job-native-form
                      job snapshot viewer
                      :wrapper
                      (lambda (record)
                        (list :job-cancel
                              :id identifier
                              :accepted-p            (and accepted-p t)
                              :reason                ':user
                              :cancelled-descendants cancelled-descendants
                              :job                   record)))
                   (task-tool-result content form))))
             (let ((timeout (or (tool-argument arguments "timeout-seconds")
                                0)))
               (unless (and (integerp timeout)
                            (<= 0 timeout *task-job-wait-maximum-seconds*))
                 (error 'task-error
                        :message
                        (format nil
                                "job.wait timeout-seconds must be an integer from 0 to ~D."
                                *task-job-wait-maximum-seconds*)
                        :tool-name "job.wait"))
               (when (and (plusp timeout)
                          (typep viewer 'task-child-agent)
                          (typep job 'task-job))
                 (handler-case
                     (make-thread
                      (lambda () (job-run-inline job))
                      :name (format nil "Autolith job.wait ~A" identifier))
                   (error ()
                     nil)))
               (multiple-value-bind (snapshot terminal-p)
                   (session-job-await job timeout)
                 (multiple-value-bind (form content)
                     (task--job-native-form
                      job snapshot viewer
                      :wrapper
                      (lambda (record)
                        (list :job-wait
                              :timeout-seconds timeout
                              :terminal-p terminal-p
                              :job        record)))
                   (task-tool-result content form)))))))
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
                (task-worktree-options-schema) "completion-policy"
                (tool-completion-policy-property))
   nil))

(-> task-augment-tool-registry (tool-registry) tool-registry)
(defun task-augment-tool-registry (registry)
  "Register one session-scoped task orchestrator and its task/job tools."
  (when (tool-registry-find registry "child" "spawn")
    (return-from task-augment-tool-registry registry))
  (tool-registry-describe-namespace registry "child"
                                    "Spawn and manage your child agents.")
  (tool-registry-describe-namespace registry "job"
                                    "Inspection, waiting, cancellation, and durable result lookup for jobs.")
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
                                            "Start a detached child agent with an assignment and optional name and context."
                                           :parameters                  (task-run-parameters-schema)))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "list" :description
                                           "List child and tool execution jobs in this session."
                                           :parameters                  job-list-schema))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "get" :description
                                           "Inspect one live session job or a completed task result persisted for the current primary conversation."
                                           :parameters                  identifier-schema))
    (tool-registry-register registry
                            (make-instance 'task-job-tool :orchestrator orchestrator
                                           :namespace "job" :name "wait" :description
                                           "Wait up to a bounded timeout for one session job and return its current or terminal result."
                                           :parameters
                                           (tool-object-schema
                                            (json-object "id"
                                                         (task-job-identifier-property)
                                                         "timeout-seconds"
                                                         (tool-integer-property
                                                          "Maximum wait in seconds; defaults to 0."))
                                            '("id"))))
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
    (task-worktree-register-tool registry orchestrator)
    (task-children-register-tools registry orchestrator)
    registry))
