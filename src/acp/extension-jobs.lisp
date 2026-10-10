(in-package #:autolith)

;;;; -- ACP Job Extensions --

(defparameter *acp-extension-job-transcript-limit* 65536
  "Maximum characters returned by one job transcript request.")

(-> acp-extension--job-orchestrator (acp-session) task-orchestrator)
(defun acp-extension--job-orchestrator (session)
  "Return SESSION's task orchestrator."
  (or (application--task-orchestrator (acp-session-application session))
      (error 'acp-extension-unavailable :reason "The task runtime is unavailable.")))

(-> acp-extension--job-viewer (acp-session) agent)
(defun acp-extension--job-viewer (session)
  "Return the application agent that owns SESSION's visible job view."
  (application-agent (acp-session-application session)))

(-> acp-extension--live-job (acp-session string string) (or session-job list))
(defun acp-extension--live-job (session identifier tool-name)
  "Return owned live IDENTIFIER, or NIL when it has completed durably."
  (let ((viewer (acp-extension--job-viewer session)))
    (or (find identifier
              (task-orchestrator-list-visible-jobs
               (acp-extension--job-orchestrator session) viewer)
              :key #'session-job-identifier :test #'string=)
        (multiple-value-bind (record path)
            (task--durable-job-result viewer identifier tool-name)
          (declare (ignore path))
            (and record (list :durable record))))))

(-> acp-extension--find-job (acp-session string string) (or session-job list))
(defun acp-extension--find-job (session identifier tool-name)
  "Find IDENTIFIER only when it is owned by SESSION's application agent."
  (or (acp-extension--live-job session identifier tool-name)
      (error 'task-error :message (format nil "No visible job named ~A exists." identifier)
             :tool-name tool-name :task-id identifier)))

(-> acp-extension--job-state ((option keyword)) string)
(defun acp-extension--job-state (state)
  "Convert an internal job STATE to its stable wire spelling."
  (case state
    (:queued "pending")
    (:finalizing "running")
    (:success "completed")
    (:aborted "cancelled")
    (otherwise (string-downcase (symbol-name (or state ':unknown))))))

(-> acp-extension--job-parent-id (session-job) (option string))
(defun acp-extension--job-parent-id (job)
  "Return JOB's parent job identity, not its tool-call identifier."
  (when (typep job 'task-job)
    (let ((parent (task-job-parent-agent job)))
      (and (typep parent 'task-child-agent)
           (session-job-identifier (task-child-agent-job parent))))))

(-> acp-extension--job-usage (t) t)
(defun acp-extension--job-usage (usage)
  "Project numeric counters, treating omitted native usage as unknown."
  (if (and usage
           (or (hash-table-p usage)
               (and (proper-list-p usage) (every #'consp usage))))
      (agentcomms:json-object
       "input" (or (conversation--usage-field usage "input_tokens") ':null)
       "output" (or (conversation--usage-field usage "output_tokens") ':null)
       "cached" (or (conversation--usage-field usage "cached_input_tokens") ':null))
      ':null))

(-> acp-extension--job-artifacts (list) hash-table)
(defun acp-extension--job-artifacts (record)
  "Expose the known transcript location as a named artifact."
  (agentcomms:json-object
   "conversation" (or (getf record :conversation-file) ':null)
   "result" (or (getf record :artifact-path) ':null)
   "shellLogs" (coerce (getf record :shell-log-references) 'vector)))

(-> acp-extension--job-row (session-job) hash-table)
(defun acp-extension--job-row (job)
  "Project one owned live JOB into a bounded ACP row."
  (let* ((snapshot (session-job-snapshot job))
         (progress (getf snapshot :progress))
         (result (getf snapshot :result)))
    (agentcomms:json-object
     "id" (session-job-identifier job)
     "parentId" (or (acp-extension--job-parent-id job) ':null)
     "role" (or (and (typep job 'task-job) (task-job-agent-name job))
                 (and (typep job 'tool-execution-job) (tool-execution-job-tool-name job)) ':null)
     "state" (acp-extension--job-state (getf snapshot :state))
     "startedAt" (or (getf progress :started-at) ':null)
     "elapsedSeconds" (if (getf progress :duration-ms) (floor (getf progress :duration-ms) 1000) ':null)
     "usage" (acp-extension--job-usage (getf progress :usage))
     "artifacts" (acp-extension--job-artifacts result)
     "activity" (or (getf progress :current-tool)
                    (getf (session-job-live-activity job) :description) ':null))))

(-> acp-extension--durable-row (string list) hash-table)
(defun acp-extension--durable-row (identifier record)
  "Project a validated durable task RECORD into an ACP row."
  (agentcomms:json-object
   "id" identifier
   "parentId" (or (getf record :parent-id) ':null)
   "role" (or (getf record :agent) (getf record :tool) ':null)
   "state" (acp-extension--job-state (or (getf record :state) (getf record :status)))
   "startedAt" ':null
   "elapsedSeconds" (if (getf record :duration-ms) (floor (getf record :duration-ms) 1000) ':null)
   "usage" (acp-extension--job-usage (getf record :usage))
   "artifacts" (acp-extension--job-artifacts record)
   "activity" ':null))

(-> acp-extension-jobs (acp-session hash-table) hash-table)
(defun acp-extension-jobs (session params)
  "Return owned jobs in stable order with bounded offset paging."
  (let* ((cursor (agentcomms:acp-field params "cursor" :type ':string))
         (offset (if cursor
                     (or (ignore-errors (parse-integer cursor :junk-allowed nil))
                         (agentcomms:acp-invalid-params "Invalid jobs cursor."))
                     0))
         (limit (or (agentcomms:acp-field params "limit" :type ':integer) 100)))
    (unless (and (<= 0 offset) (<= 1 limit 200))
      (agentcomms:acp-invalid-params "Invalid jobs page bounds."))
    (let* ((jobs (sort (copy-list (task-orchestrator-list-visible-jobs
                                 (acp-extension--job-orchestrator session)
                                 (acp-extension--job-viewer session))) #'< :key #'session-job-order))
           (start (min offset (length jobs)))
           (end (min (+ start limit) (length jobs))))
      (agentcomms:json-object
       "items" (map 'vector #'acp-extension--job-row (subseq jobs start end))
       "nextCursor" (if (< end (length jobs)) (princ-to-string end) ':null)))))

(-> acp-extension-job (acp-session hash-table) hash-table)
(defun acp-extension-job (session params)
  "Return one owned job row selected by JOB-ID."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (found (acp-extension--find-job session identifier "_autolith/job")))
    (if (and (consp found) (eq (first found) :durable))
        (acp-extension--durable-row identifier (second found))
        (acp-extension--job-row found))))

(-> acp-extension--job-transcript-path (acp-session t) (option pathname))
(defun acp-extension--job-transcript-path (session job)
  "Return the owned child's durable transcript identity, including during execution."
  (let* ((record (if (consp job) (second job) (getf (session-job-snapshot job) :result)))
         (path (or (getf record :conversation-file)
                   (when (typep job 'task-job)
                     (merge-pathnames
                      (make-pathname :name (task-job-execution-identifier job) :type "sexp")
                      (task--artifact-root
                       (application-configuration (acp-session-application session)) job))))))
    (when (and path (conversation-storage-occupied-p (pathname path)))
      (pathname path))))

(defparameter *acp-extension-job-structured-limit* 200
  "Maximum ACP updates returned by one structured job transcript request.")
(defparameter *acp-extension-job-structured-response-limit* 262144
  "Maximum UTF-8 bytes in a structured job transcript's typed response envelope.")

(-> acp-extension--job-structured-window
    (acp-session t &key (:offset integer) (:limit integer)) hash-table)
(defun acp-extension--job-structured-window (session job &key offset limit)
  "Return a bounded page of standard ACP updates from JOB's durable transcript."
  (let ((path (acp-extension--job-transcript-path session job))
        (updates nil))
    (when path
      (acp-replay-map-updates (conversation-load path)
                              (lambda (update) (push update updates))))
    (let* ((ordered (nreverse updates))
           (total (length ordered))
           (start (min offset total))
           (cursor start)
           (selected nil))
      (loop for index from start below (min (+ start limit) total)
            for update = (nth index ordered)
            do (let* ((candidate (append (reverse selected) (list update)))
                      (probe (agentcomms:json-object
                              "format" "acp-session-update"
                              "updates" (coerce candidate 'vector)
                              "offset" start
                              "nextOffset" (if (= (1+ index) total) ':null (1+ index))
                              "total" total
                              "complete" (if (= (1+ index) total) t (argo:json-false))))
                      (size (length (utf8-string-to-octets
                                    (agentcomms:json-encode
                                     (acp-extension-result ':ok :value probe))))))
                 (cond ((<= size *acp-extension-job-structured-response-limit*)
                        (push update selected)
                        (setf cursor (1+ index)))
                       ((null selected)
                        (agentcomms:acp-invalid-params
                         "A structured transcript update exceeds the response limit."))
                       (t (return)))))
      (let* ((page (nreverse selected))
             (complete (= cursor total)))
        (agentcomms:json-object
         "format" "acp-session-update"
         "updates" (coerce page 'vector)
         "offset" start
         "nextOffset" (if complete ':null cursor)
         "total" total
         "complete" (if complete t (argo:json-false)))))))

(-> acp-extension--job-window (acp-session t &key (:offset integer) (:limit integer)) hash-table)
(defun acp-extension--job-window (session job &key offset limit)
  "Render readable records while retaining only the requested character range."
  (let ((path (acp-extension--job-transcript-path session job))
        (total 0)
        (stream (make-string-output-stream)))
    (labels ((emit (text)
               "Count TEXT and retain its overlap with the requested range."
               (when (stringp text)
                 (let* ((end (+ total (length text)))
                        (first (max total offset))
                        (last (min end (+ offset limit))))
                   (when (< first last)
                     (write-string text stream :start (- first total) :end (- last total)))
                   (setf total end))))
             (line (label text)
               "Emit one labeled transcript entry without concatenating its body."
               (when text
                 (emit label) (emit text) (emit (string #\Newline)))))
      (when path
        (conversation-replay--map-records
         (conversation-load path)
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
               (:tool-result (line "Result: " (getf fields :output)))))))))
    (let* ((content (get-output-stream-string stream))
           (start (min offset total))
           (next (+ start (length content))))
      (agentcomms:json-object "content" content "offset" start "nextOffset" next
                              "total" total "complete" (if (= next total) t (argo:json-false))))))

(-> acp-extension-job-transcript (acp-session hash-table) hash-table)
(defun acp-extension-job-transcript (session params)
  "Return a bounded text or structured window from an owned child transcript."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (output (or (agentcomms:acp-field params "output" :type ':string) "text"))
         (job (acp-extension--find-job session identifier "_autolith/job-transcript"))
         (structured-p (cond ((string= output "text") nil)
                             ((string= output "structured") t)
                             (t (agentcomms:acp-invalid-params
                                 "Transcript output must be text or structured.")))))
    (if structured-p
        (let ((offset (or (agentcomms:acp-field params "offset" :type ':integer) 0))
              (limit (or (agentcomms:acp-field params "limit" :type ':integer) 100)))
          (unless (and (<= 0 offset) (<= 1 limit *acp-extension-job-structured-limit*))
            (agentcomms:acp-invalid-params "Invalid structured transcript window."))
          (acp-extension--job-structured-window session job :offset offset :limit limit))
        (let ((offset (or (agentcomms:acp-field params "offset" :type ':integer) 0))
              (limit (or (agentcomms:acp-field params "limit" :type ':integer) 4096)))
          (unless (and (<= 0 offset) (<= 1 limit *acp-extension-job-transcript-limit*))
            (agentcomms:acp-invalid-params "Invalid transcript window."))
          (acp-extension--job-window session job :offset offset :limit limit)))))

(-> acp-extension-job-send (acp-session hash-table) hash-table)
(defun acp-extension-job-send (session params)
  "Queue owned child steering text and return its admission status."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (text (agentcomms:acp-field params "text" :type ':string :required-p t))
         (job (acp-extension--find-job session identifier "_autolith/job-send")))
    (unless (typep job 'task-job)
      (error 'acp-extension-unavailable :reason "Only child jobs accept steering."))
    (multiple-value-bind (entry reason) (task-job-enqueue-steering job text)
      (agentcomms:json-object "accepted" (if entry t (argo:json-false))
                              "reason" (string-downcase (symbol-name reason))))))

(-> acp-extension-job-cancel (acp-session hash-table) hash-table)
(defun acp-extension-job-cancel (session params)
  "Cancel an owned job and its retained descendants."
  (let* ((identifier (agentcomms:acp-field params "jobId" :type ':string :required-p t))
         (job (acp-extension--find-job session identifier "_autolith/job-cancel")))
    (unless (typep job 'session-job)
      (error 'acp-extension-unavailable :reason "Only live jobs can be cancelled."))
    (multiple-value-bind (accepted descendants) (session-job-cancel job ':user)
      (agentcomms:json-object "accepted" (if accepted t (argo:json-false))
                              "descendantIds" (coerce descendants 'vector)))))
