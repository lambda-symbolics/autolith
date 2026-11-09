(in-package #:autolith)

(defgeneric application-client--job-structured-window (application job &key offset limit)
  (:documentation "Project a structured transcript through the installed frontend format adapter.")
  (:method (application job &key offset limit)
    "Reject structured transcript formats without an installed adapter."
    (declare (ignore application job offset limit))
    (error 'application-client-unavailable :reason "No structured transcript adapter is installed.")))

;;;; -- Client Job Queries --

(defparameter *application-client-job-transcript-limit* 65536
  "Maximum characters returned by one job transcript request.")

(-> application-client--job-orchestrator (application) task-orchestrator)
(defun application-client--job-orchestrator (application)
  "Return APPLICATION's task orchestrator."
  (or (application--task-orchestrator application)
      (error 'application-client-unavailable :reason "The task runtime is unavailable.")))

(-> application-client--job-viewer (application) agent)
(defun application-client--job-viewer (application)
  "Return the application agent that owns the visible job view."
  (application-agent application))

(-> application-client--live-job (application string string) (or session-job list))
(defun application-client--live-job (application identifier tool-name)
  "Return owned live IDENTIFIER, or NIL when it has completed durably."
  (let ((viewer (application-client--job-viewer application)))
    (or (find identifier
              (task-orchestrator-list-visible-jobs
               (application-client--job-orchestrator application) viewer)
              :key #'session-job-identifier :test #'string=)
        (multiple-value-bind (record path)
            (task--durable-job-result viewer identifier tool-name)
          (declare (ignore path))
          (and record (list :durable record))))))

(-> application-client--find-job (application string string) (or session-job list))
(defun application-client--find-job (application identifier tool-name)
  "Find IDENTIFIER only when it is owned by SESSION's application agent."
  (or (application-client--live-job application identifier tool-name)
      (error 'task-error :message (format nil "No visible job named ~A exists." identifier)
             :tool-name tool-name :task-id identifier)))

(-> application-client--job-state ((option keyword)) string)
(defun application-client--job-state (state)
  "Convert an internal job STATE to its stable wire spelling."
  (case state
    (:queued
     "pending")
    (:finalizing
     "running")
    (:success
     "completed")
    (:aborted
     "cancelled")
    (otherwise
     (string-downcase (symbol-name (or state ':unknown))))))

(-> application-client--job-parent-id (session-job) (option string))
(defun application-client--job-parent-id (job)
  "Return JOB's parent job identity, not its tool-call identifier."
  (when (typep job 'task-job)
    (let ((parent (task-job-parent-agent job)))
      (and (typep parent 'task-child-agent)
           (session-job-identifier (task-child-agent-job parent))))))

(-> application-client--job-usage (t) t)
(defun application-client--job-usage (usage)
  "Project numeric counters, treating omitted native usage as unknown."
  (if (and usage
           (or (hash-table-p usage)
               (and (proper-list-p usage) (every #'consp usage))))
      (agentcomms:json-object
       "input" (or (conversation--usage-field usage "input_tokens") ':null)
       "output" (or (conversation--usage-field usage "output_tokens") ':null)
       "cached" (or (conversation--usage-field usage "cached_input_tokens") ':null))
      ':null))

(-> application-client--job-artifacts (list) hash-table)
(defun application-client--job-artifacts (record)
  "Expose the known transcript location as a named artifact."
  (agentcomms:json-object
   "conversation" (or (getf record :conversation-file) ':null)
   "result" (or (getf record :artifact-path) ':null)
   "shellLogs" (coerce (getf record :shell-log-references) 'vector)))

(-> application-client--job-row (session-job) hash-table)
(defun application-client--job-row (job)
  "Project one owned live JOB into a bounded ACP row."
  (let* ((snapshot (session-job-snapshot job))
         (progress (getf snapshot :progress))
         (result   (getf snapshot :result)))
    (agentcomms:json-object
     "id" (session-job-identifier job)
     "parentId" (or (application-client--job-parent-id job) ':null)
     "role" (or (and (typep job 'task-job) (task-job-agent-name job))
                (and (typep job 'tool-execution-job) (tool-execution-job-tool-name job)) ':null)
     "state" (application-client--job-state (getf snapshot :state))
     "startedAt" (or (getf progress :started-at) ':null)
     "elapsedSeconds" (if (getf progress :duration-ms) (floor (getf progress :duration-ms) 1000) ':null)
     "usage" (application-client--job-usage (getf progress :usage))
     "artifacts" (application-client--job-artifacts result)
     "activity" (or (getf progress :current-tool)
                    (getf (session-job-live-activity job) :description) ':null))))

(-> application-client--durable-row (string list) hash-table)
(defun application-client--durable-row (identifier record)
  "Project a validated durable task RECORD into an ACP row."
  (agentcomms:json-object
   "id" identifier
   "parentId" (or (getf record :parent-id) ':null)
   "role" (or (getf record :agent) (getf record :tool) ':null)
   "state" (application-client--job-state (or (getf record :state) (getf record :status)))
   "startedAt" ':null
   "elapsedSeconds" (if (getf record :duration-ms) (floor (getf record :duration-ms) 1000) ':null)
   "usage" (application-client--job-usage (getf record :usage))
   "artifacts" (application-client--job-artifacts record)
   "activity" ':null))

(-> application-client-jobs (application hash-table) hash-table)
(defun application-client-jobs (application params)
  "Return owned jobs in stable order with bounded offset paging."
  (let* ((cursor (agentcomms:acp-field params "cursor" :type ':string))
         (offset (if cursor
                     (or (handler-case (parse-integer cursor :junk-allowed nil)
                           (parse-error ()
                             nil))
                         (agentcomms:acp-invalid-params "Invalid jobs cursor."))
                     0))
         (limit  (or (agentcomms:acp-field params "limit" :type ':integer) 100)))
    (unless (and (<= 0 offset) (<= 1 limit 200))
      (agentcomms:acp-invalid-params "Invalid jobs page bounds."))
    (let* ((jobs  (sort (copy-list (task-orchestrator-list-visible-jobs
                                    (application-client--job-orchestrator application)
                                    (application-client--job-viewer application))) #'< :key #'session-job-order))
           (start (min offset (length jobs)))
           (end   (min (+ start limit) (length jobs))))
      (agentcomms:json-object
       "items" (map 'vector #'application-client--job-row (subseq jobs start end))
       "nextCursor" (if (< end (length jobs)) (princ-to-string end) ':null)))))

(-> application-client-job (application hash-table) hash-table)
(defun application-client-job (application params)
  "Return one owned job row selected by JOB-ID."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (found      (application-client--find-job application identifier "_autolith/job")))
    (if (and (consp found) (eq (first found) :durable))
        (application-client--durable-row identifier (second found))
        (application-client--job-row found))))

(-> application-client--job-transcript-path (application t) (option pathname))
(defun application-client--job-transcript-path (application job)
  "Return the owned child's durable transcript identity, including during execution."
  (let* ((record (if (consp job) (second job) (getf (session-job-snapshot job) :result)))
         (path   (or (getf record :conversation-file)
                     (when (typep job 'task-job)
                       (merge-pathnames
                        (make-pathname :name (task-job-execution-identifier job) :type "sexp")
                        (task--artifact-root
                         (application-configuration application) job))))))
    (when (and path (conversation-storage-occupied-p (pathname path)))
      (pathname path))))

(defparameter *application-client-job-structured-limit* 200
  "Maximum ACP updates returned by one structured job transcript request.")

(-> application-client--transcript-page
    ((option pathname) &key (:offset integer) (:limit integer) (:mapper function) (:collector function))
    (values integer integer integer))
(defun application-client--transcript-page (path &key offset limit mapper collector)
  "Count semantic extents while projecting only the requested page.

MAPPER receives the header-only conversation and an emitter taking an extent and
lazy producer. COLLECTOR receives that producer and relative overlap bounds;
returning NIL stops projection but not counting. Return total, start and next."
  (let ((total 0) (next offset) (collect-p t))
    (when path
      (funcall mapper (conversation-replay-load-identity (conversation-storage-identity-pathname path))
               (lambda (extent producer)
                 (let* ((end   (+ total extent))
                        (first (max total offset))
                        (last  (min end (+ offset limit))))
                   (when (and collect-p (< first last))
                     (if (funcall collector producer (- first total) (- last total))
                         (setf next last)
                         (setf collect-p nil)))
                   (setf total end)))))
    (values total (min offset total) (min next total))))

(-> application-client--job-window (application t &key (:offset integer) (:limit integer)) hash-table)
(defun application-client--job-window (application job &key offset limit)
  "Render readable records while retaining only the requested character range."
  (let ((path   (application-client--job-transcript-path application job))
        (stream (make-string-output-stream)))
    (labels ((map-text (conversation emit)
               "Emit readable spans with character extents."
               (labels ((text (value)
                          (when (stringp value)
                            (funcall emit (length value) (lambda () value))))

                        (line (label value)
                          "Emit one labeled entry without concatenating its body."
                          (when value
                            (text label) (text value) (text (string #\Newline)))))
                 (conversation-replay--map-records
                  conversation
                  (lambda (record)
                    (let ((fields (rest record)))
                      (case (first record)
                        (:message
                         (line (if (eq (getf fields :role) ':user) "User: " "Assistant: ")
                               (getf fields :content)))
                        (:provider-item
                         (let ((item (json-decode (getf fields :wire-json))))
                           (cond ((equal (json-get item "type") "message")
                                  (line "Assistant: " (response-item-assistant-text item)))
                                 ((equal (json-get item "type") "reasoning")
                                  (line "Thought: " (response-item-reasoning-summary item)))
                                 ((equal (json-get item "type") "function_call")
                                  (line "Tool: " (function-call-canonical-name item))
                                  (line "Arguments: " (json-get item "arguments"))))))
                        (:tool-result
                         (line "Result: " (getf fields :output))))))))))
      (multiple-value-bind (total start next)
          (application-client--transcript-page
           path :offset offset :limit limit :mapper #'map-text
           :collector (lambda (producer first last)
                        (write-string (funcall producer) stream :start first :end last)
                        t))
        (agentcomms:json-object
         "content" (get-output-stream-string stream) "offset" start "nextOffset" next
         "total" total "complete" (if (= next total) t (argo:json-false)))))))

(-> application-client-job-transcript (application hash-table) hash-table)
(defun application-client-job-transcript (application params)
  "Return a bounded text or structured window from an owned child transcript."
  (let* ((identifier   (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (output       (or (agentcomms:acp-field params "output" :type ':string) "text"))
         (job          (application-client--find-job application identifier "_autolith/job-transcript"))
         (structured-p (cond ((string= output "text")
                              nil)
                             ((string= output "structured")
                              t)
                             (t
                              (agentcomms:acp-invalid-params
                               "Transcript output must be text or structured.")))))
    (if structured-p
        (let ((offset (or (agentcomms:acp-field params "offset" :type ':integer) 0))
              (limit  (or (agentcomms:acp-field params "limit" :type ':integer) 100)))
          (unless (and (<= 0 offset) (<= 1 limit *application-client-job-structured-limit*))
            (agentcomms:acp-invalid-params "Invalid structured transcript window."))
          (application-client--job-structured-window application job :offset offset :limit limit))
        (let ((offset (or (agentcomms:acp-field params "offset" :type ':integer) 0))
              (limit  (or (agentcomms:acp-field params "limit" :type ':integer) 4096)))
          (unless (and (<= 0 offset) (<= 1 limit *application-client-job-transcript-limit*))
            (agentcomms:acp-invalid-params "Invalid transcript window."))
          (application-client--job-window application job :offset offset :limit limit)))))

(-> application-client-job-send (application hash-table) hash-table)
(defun application-client-job-send (application params)
  "Queue owned child steering text and return its admission status."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (text       (agentcomms:acp-field params "text" :type ':string :required-p t))
         (job        (application-client--find-job application identifier "_autolith/job-send")))
    (unless (typep job 'task-job)
      (error 'application-client-unavailable :reason "Only child jobs accept steering."))
    (multiple-value-bind (entry reason) (task-job-enqueue-steering job text)
      (agentcomms:json-object "accepted" (if entry t (argo:json-false))
                              "reason" (string-downcase (symbol-name reason))))))

(-> application-client-job-cancel (application hash-table) hash-table)
(defun application-client-job-cancel (application params)
  "Cancel an owned job and its retained descendants."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (job        (application-client--find-job application identifier "_autolith/job-cancel")))
    (unless (typep job 'session-job)
      (error 'application-client-unavailable :reason "Only live jobs can be cancelled."))
    (multiple-value-bind (accepted descendants) (session-job-cancel job ':user)
      (agentcomms:json-object "accepted" (if accepted t (argo:json-false))
                              "descendantIds" (coerce descendants 'vector)))))
