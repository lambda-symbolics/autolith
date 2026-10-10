(in-package #:autolith)

;;;; -- ACP Job Extension Tests --

(-> acp-extension-job-tests--definition () task-agent-definition)
(defun acp-extension-job-tests--definition ()
  "Return a valid child definition for ACP job fixtures."
  (task-agent-definition-create
   :name "acp-job-fixture"
   :description "Exercise ACP job projection."
   :instructions "Return the fixture result."
   :source ':test))

(-> acp-extension-job-tests--result (task-job pathname) list)
(defun acp-extension-job-tests--result (job pathname)
  "Return a valid result containing PATHNAME's active transcript segment."
  (let ((result (task-tests--terminal-result job :output "fixture output"))
        (active (first (last (conversation-storage-pathnames pathname)))))
    (setf (getf result :id) (copy-seq (getf result :id))
          (getf result :name) (copy-seq (getf result :name))
          (getf result :conversation-file) (namestring active))
    result))

(-> test-acp-extension-job-foreign-rejection () null)
(defun test-acp-extension-job-foreign-rejection ()
  "Reject a real foreign job while projecting owned jobs and details."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service session-id))
              (application (acp-session-application session))
              (viewer (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition (acp-extension-job-tests--definition))
              (owned (let ((job (task-tests--register-job orchestrator viewer definition
                                                         :name "owned")))
                       (setf (task-progress-usage (task-job-progress job))
                             '(("input_tokens" 12) ("output_tokens" 3)))
                       job))
              (foreign-agent (task-tests--primary-agent configuration "foreign-owner"))
              (foreign (task-tests--register-job orchestrator foreign-agent definition
                                                 :name "foreign"))
              (items (agentcomms:json-get
                      (agentcomms:json-get
                       (agentcomms:client-agent-request
                        client "_autolith/jobs"
                        (agentcomms:json-object "sessionId" session-id))
                       "value")
                      "items"))
              (detail (agentcomms:json-get
                       (agentcomms:client-agent-request
                        client "_autolith/job"
                        (agentcomms:json-object "sessionId" session-id
                                               "jobId" (job-identifier owned)))
                       "value"))
              (rejected nil))
         (handler-case
             (acp-extension--find-job session (job-identifier foreign) "_autolith/job")
           (task-error () (setf rejected t)))
         (test-assert (find (job-identifier owned) items :key
                            (lambda (item) (agentcomms:json-get item "id"))
                            :test #'string=)
                      "owned live jobs are enumerated")
         (test-assert (string= (job-identifier owned)
                               (agentcomms:json-get detail "id"))
                      "owned job details are returned")
         (test-assert (= 12 (agentcomms:json-get
                            (agentcomms:json-get detail "usage") "input"))
                      "job usage survives the portable wire projection")
         (test-assert (hash-table-p (agentcomms:json-get detail "artifacts"))
                      "job artifacts are a named wire object")
         (setf (task-progress-usage (task-job-progress owned))
               (task--compact-native-value '(("input_tokens" 12) ("output_tokens" 3)) 1))
         (let ((compacted (agentcomms:client-agent-request
                           client "_autolith/job"
                           (agentcomms:json-object "sessionId" session-id
                                                  "jobId" (job-identifier owned)))))
           (test-assert (equal "ok" (agentcomms:json-get compacted "outcome"))
                        "compacted native usage does not fail the job projection")
           (test-assert (null (agentcomms:json-get
                              (agentcomms:json-get compacted "value") "usage"))
                        "omitted usage is unknown rather than a fabricated counter"))
         (test-assert (not (find (job-identifier foreign) items :key
                                 (lambda (item) (agentcomms:json-get item "id"))
                                 :test #'string=))
                      "foreign live jobs are not enumerated")
         (test-assert rejected "a real foreign job is rejected")))))
  nil)

(-> test-acp-extension-job-transcript-window () null)
(defun test-acp-extension-job-transcript-window ()
  "Project a completed child conversation and page its transcript."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service session-id))
              (application (acp-session-application session))
              (viewer (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition (acp-extension-job-tests--definition))
              (job (task-tests--register-job orchestrator viewer definition
                                              :name "transcript"))
              (child (task-tests--child-viewer configuration job))
              (conversation (agent-conversation child))
              (needle "ACP transcript fixture with real content")
              (full nil))
         (conversation-append-user-message conversation needle)
         (task-tests--publish-terminal
          job :completed
          (acp-extension-job-tests--result job (conversation-pathname conversation)))
         (let* ((response (acp-extension-job-transcript
                           session
                           (agentcomms:json-object
                            "jobId" (job-identifier job) "offset" 0 "limit" 4096)))
                (content (agentcomms:json-get response "content"))
                (total (agentcomms:json-get response "total")))
           (setf full content)
           (test-assert (plusp total) "completed child transcript has content")
           (test-assert (search needle content) "transcript contains persisted child content")
           (test-assert (= total (length content)) "full transcript reports its length"))
         (let* ((page-size 7)
                (first-page
                  (acp-extension-job-transcript
                   session
                   (agentcomms:json-object
                    "jobId" (job-identifier job) "offset" 0 "limit" page-size)))
                (first-content (agentcomms:json-get first-page "content"))
                (next (agentcomms:json-get first-page "nextOffset"))
                (second-page
                  (acp-extension-job-transcript
                   session
                   (agentcomms:json-object
                    "jobId" (job-identifier job) "offset" next "limit" page-size))))
           (test-assert (string= first-content (subseq full 0 page-size))
                        "transcript first page is character bounded")
           (test-assert (= next page-size) "transcript advances by returned characters")
           (test-assert (string= (agentcomms:json-get second-page "content")
                                 (subseq full page-size
                                         (min (length full) (* 2 page-size))))
                        "transcript second page starts at nextOffset"))))))
  nil)

(-> test-acp-extension-job-steer-and-cancel-authority () null)
(defun test-acp-extension-job-steer-and-cancel-authority ()
  "Exercise steering and cancellation for a real owned live job."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service session-id))
              (application (acp-session-application session))
              (viewer (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition (acp-extension-job-tests--definition))
              (job (let ((job (task-tests--register-job orchestrator viewer definition
                                                       :name "live-control")))
                     (setf (job-state job) ':running)
                     job))
              (foreign-agent (task-tests--primary-agent configuration "foreign-control"))
              (foreign (task-tests--register-job orchestrator foreign-agent definition
                                                 :name "foreign-control"))
              (sent (acp-extension-job-send
                     session
                     (agentcomms:json-object
                      "jobId" (job-identifier job) "text" "steer this job")))
              (cancelled (acp-extension-job-cancel
                          session
                          (agentcomms:json-object "jobId" (job-identifier job))))
              (foreign-rejected nil))
         (handler-case
             (acp-extension-job-cancel
              session (agentcomms:json-object "jobId" (job-identifier foreign)))
           (task-error () (setf foreign-rejected t)))
         (test-assert (agentcomms:json-get sent "accepted")
                      "owned live job accepts steering")
         (test-assert (agentcomms:json-get cancelled "accepted")
                      "owned live job accepts cancellation")
         (task-tests--publish-terminal
          job ':aborted (task-tests--terminal-result job :status ':aborted))
         (let ((response (agentcomms:client-agent-request
                          client "_autolith/job"
                          (agentcomms:json-object "sessionId" session-id
                                                 "jobId" (job-identifier job)))))
           (test-assert (equal "cancelled"
                               (agentcomms:json-get (agentcomms:json-get response "value") "state"))
                        "native aborted jobs expose the cancelled wire state"))
         (test-assert foreign-rejected "foreign live job cannot be cancelled")))))
  nil)

(-> test-acp-extension-job-durable-fallback () null)
(defun test-acp-extension-job-durable-fallback ()
  "Read a completed child from its durable result artifact."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service session-id))
              (application (acp-session-application session))
              (viewer (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition (acp-extension-job-tests--definition))
              (job (task-tests--register-job orchestrator viewer definition
                                              :name "durable"))
              (child (task-tests--child-viewer configuration job))
              (conversation (agent-conversation child))
              (needle "durable ACP transcript"))
         (conversation-append-user-message conversation needle)
         (let ((result-path (merge-pathnames "result.sexp" (task--artifact-root configuration job)))
               (identifier (task-job-execution-identifier job)))
           (ensure-directories-exist result-path)
           (loop for (status expected) in '((:success "completed") (:failed "failed") (:aborted "cancelled"))
                 for result = (acp-extension-job-tests--result job (conversation-pathname conversation))
                 do (setf (getf result :status) status)
                    (snapshot-write result-path result)
                    (let ((row (acp-extension-job session (agentcomms:json-object "jobId" identifier))))
                      (test-assert (string= identifier (agentcomms:json-get row "id"))
                                   "durable result is projected by job detail")
                      (test-assert (string= expected (agentcomms:json-get row "state"))
                                   "durable success, failure and cancellation retain their outcomes")))
           (let ((transcript (acp-extension-job-transcript
                              session (agentcomms:json-object "jobId" identifier "offset" 0 "limit" 4096))))
             (test-assert (search needle (agentcomms:json-get transcript "content"))
                          "durable result exposes its conversation transcript")))))))
  nil)
