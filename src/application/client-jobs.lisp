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

(-> application-client--find-job (application string string)
    (values (or session-job list) list))
(defun application-client--find-job (application identifier tool-name)
  "Return the owned live job or durable record wrapper, and its canonical snapshot."
  (multiple-value-bind (snapshot job)
      (task-query-job (application-client--job-orchestrator application)
                      (application-agent application)
                      :identifier identifier :tool-name tool-name :execution-alias-p nil)
    (unless snapshot
      (error 'task-error :message (format nil "No visible job named ~A exists." identifier)
             :tool-name tool-name :task-id identifier))
    (values (or job (list :durable (getf snapshot :result))) snapshot)))

(-> application-client--job-usage (t) list)
(defun application-client--job-usage (usage)
  "Project numeric counters as native values."
  (when (and usage
             (or (hash-table-p usage)
                 (and (proper-list-p usage) (every #'consp usage))))
    (list :input  (conversation--usage-field usage "input_tokens")
          :output (conversation--usage-field usage "output_tokens")
          :cached (conversation--usage-field usage "cached_input_tokens"))))

(-> application-client--job-artifacts (list) list)
(defun application-client--job-artifacts (record)
  "Expose the known transcript location as native artifact metadata."
  (list :conversation (getf record :conversation-file)
        :result       (getf record :artifact-path)
        :shell-logs   (getf record :shell-log-references)))

(-> application-client--job-row (list &key (:identifier (option string))
                                      (:job (option session-job))) list)
(defun application-client--job-row (snapshot &key identifier job)
  "Project a canonical SNAPSHOT into native fields."
  (let ((progress (getf snapshot :progress))
        (result   (getf snapshot :result)))
    (list :id              (or identifier (getf snapshot :job-id))
          :parent-id       (getf snapshot :parent-id)
          :role            (or (getf snapshot :agent) (getf snapshot :tool))
          :state           (getf snapshot :state)
          :elapsed-seconds (and (getf progress :duration-ms)
                                (floor (getf progress :duration-ms) 1000))
          :usage           (application-client--job-usage (getf progress :usage))
          :artifacts       (application-client--job-artifacts result)
          :activity        (or (getf progress :current-tool)
                               (and job (getf (session-job-live-activity job) :description))))))

(-> application-client-jobs (application &key (:cursor (option (or integer string)))
                                             (:limit (option integer))) list)
(defun application-client-jobs (application &key cursor (limit 100))
  "Return owned jobs in stable order with bounded offset paging."
  (let* ((limit  (or limit 100))
         (offset (cond ((null cursor)
                        0)
                       ((integerp cursor)
                        cursor)
                       ((stringp cursor)
                        (handler-case (parse-integer cursor :junk-allowed nil)
                          (error ()
                            (error 'application-client-unavailable :reason "Invalid jobs cursor."))))
                       (t
                        (error 'application-client-unavailable :reason "Invalid jobs cursor.")))))
    (unless (and (<= 0 offset) (<= 1 limit 200))
      (error 'application-client-unavailable :reason "Invalid jobs page bounds."))
    (let* ((jobs  (task-orchestrator-list-visible-jobs
                   (application-client--job-orchestrator application)
                   (application-agent application)))
           (start (min offset (length jobs)))
           (end   (min (+ start limit) (length jobs))))
      (list :items       (mapcar (lambda (job)
                                   (application-client--job-row (session-job-snapshot job) :job job))
                                 (subseq jobs start end))
            :next-cursor (and (< end (length jobs)) end)))))

(-> application-client-job (application string) list)
(defun application-client-job (application identifier)
  "Return one owned job row selected by IDENTIFIER."
  (multiple-value-bind (found snapshot)
        (application-client--find-job application identifier "job")
    (application-client--job-row snapshot :identifier identifier
                                 :job                 (and (typep found 'session-job) found))))

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

(-> application-client--job-window (application t &key (:offset integer) (:limit integer)) list)
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
                           (cond ((message-item-p item)
                                  (line "Assistant: " (response-item-assistant-text item)))
                                 ((reasoning-item-p item)
                                  (line "Thought: " (response-item-reasoning-summary item)))
                                 ((function-call-item-p item)
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
          (list :content  (get-output-stream-string stream)
                :offset start :next-offset next :total total
                :complete (= next total))))))

(-> application-client-job-transcript
    (application string &key (:output keyword) (:offset (option integer)) (:limit (option integer))) t)
(defun application-client-job-transcript (application identifier
                                           &key (output ':text) (offset 0) limit)
  "Return a bounded native text or structured window from an owned child transcript."
  (let* ((offset (or offset 0))
         (job    (application-client--find-job application identifier "job-transcript")))
    (ecase output
      (:structured
       (let ((window-limit (or limit 100)))
         (unless (and (<= 0 offset) (<= 1 window-limit *application-client-job-structured-limit*))
           (error 'application-client-unavailable :reason "Invalid structured transcript window."))
         (application-client--job-structured-window application job
                                                      :offset offset :limit window-limit)))
      (:text
       (let ((window-limit (or limit 4096)))
         (unless (and (<= 0 offset) (<= 1 window-limit *application-client-job-transcript-limit*))
           (error 'application-client-unavailable :reason "Invalid transcript window."))
         (application-client--job-window application job
                                          :offset offset :limit window-limit))))))

(-> application-client-job-send (application string string) list)
(defun application-client-job-send (application identifier text)
  "Queue owned child steering text and return its native admission status."
  (let ((job (application-client--find-job application identifier "job-send")))
    (unless (typep job 'task-job)
      (error 'application-client-unavailable :reason "Only child jobs accept steering."))
    (multiple-value-bind (entry reason) (task-job-enqueue-steering job text)
      (list :accepted (not (null entry)) :reason reason))))

(-> application-client-job-cancel (application string) list)
(defun application-client-job-cancel (application identifier)
  "Cancel an owned job and its retained descendants."
  (let ((job (application-client--find-job application identifier "job-cancel")))
    (unless (typep job 'session-job)
      (error 'application-client-unavailable :reason "Only live jobs can be cancelled."))
    (multiple-value-bind (accepted descendants) (session-job-cancel job ':user)
      (list :accepted (not (null accepted)) :descendant-ids descendants))))
