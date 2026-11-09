(in-package #:autolith)

;;;; -- ACP Job Extension Tests --

(-> acp-extension-job-tests--definition () task-agent-definition)
(defun acp-extension-job-tests--definition ()
  "Return a valid child definition for ACP job fixtures."
  (task-agent-definition-create
   :name         "acp-job-fixture"
   :description  "Exercise ACP job projection."
   :instructions "Return the fixture result."
   :source       ':test))

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
       (let* ((session-id    (agentcomms:client-new-session client (namestring root)))
              (session       (acp-service--session service session-id))
              (application   (acp-session-test--application session))
              (viewer        (application-agent application))
              (orchestrator  (application--task-orchestrator application))
              (definition    (acp-extension-job-tests--definition))
              (owned         (let ((job (task-tests--register-job orchestrator viewer definition
                                                                 :name "owned")))
                               (setf (task-progress-usage (task-job-progress job))
                                     '(("input_tokens" 12) ("output_tokens" 3)))
                               job))
              (foreign-agent (task-tests--primary-agent configuration "foreign-owner"))
              (foreign       (task-tests--register-job orchestrator foreign-agent definition
                                                       :name "foreign"))
              (items         (agentcomms:json-get
                              (agentcomms:json-get
                               (agentcomms:client-agent-request
                                client "_autolith/jobs"
                                (agentcomms:json-object "sessionId" session-id))
                               "value")
                              "items"))
              (detail        (agentcomms:json-get
                              (agentcomms:client-agent-request
                               client "_autolith/job"
                               (agentcomms:json-object "sessionId" session-id
                                                      "jobId" (job-identifier owned)))
                              "value"))
              (rejected      nil))
         (handler-case
             (application-client--find-job (acp-session-test--application session) (job-identifier foreign) "_autolith/job")
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
       (let* ((session-id   (agentcomms:client-new-session client (namestring root)))
              (session      (acp-service--session service session-id))
              (application  (acp-session-test--application session))
              (viewer       (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition   (acp-extension-job-tests--definition))
              (job          (task-tests--register-job orchestrator viewer definition
                                                       :name "transcript"))
              (child        (task-tests--child-viewer configuration job))
              (conversation (agent-conversation child))
              (needle       "ACP transcript fixture with real content")
              (full         nil))
           (conversation-append-user-message conversation needle)
          (dotimes (turn 2)
            (when (plusp turn)
              (conversation-append-summary conversation "Continue in a new segment.")
              (conversation-append-user-message conversation "automatic continuation" :automatic-p t))
            (conversation-append-record
             conversation
             (list :provider-item :wire-json
                   (json-encode (agentcomms:json-object
                                 "type" "function_call" "call_id" (format nil "call-~D" turn)
                                 "name" "shell.run" "arguments" "{\"command\":\"pwd\"}"))))
            (conversation-append-record
             conversation (list :tool-result :call-id (format nil "call-~D" turn)
                                :status ':ok :output "done")))
          (conversation-append-user-message
           conversation
           (make-string 1000 :initial-element #\λ))
          (dotimes (index 4)
            (conversation-append-user-message conversation
                                               (format nil "page update ~D" index)))
         (task-tests--publish-terminal
          job :completed
          (acp-extension-job-tests--result job (conversation-pathname conversation)))
         (let* ((response (application-client-job-transcript
                           (acp-session-test--application session)
                           (agentcomms:json-object
                            "jobId" (job-identifier job) "offset" 0 "limit" 4096)))
                (content  (agentcomms:json-get response "content"))
                (total    (agentcomms:json-get response "total")))
           (setf full content)
           (test-assert (plusp total) "completed child transcript has content")
           (test-assert (search needle content) "transcript contains persisted child content")
           (test-assert (= total (length content)) "full transcript reports its length"))
         (let* ((structured (application-client-job-transcript
                              (acp-session-test--application session)
                              (agentcomms:json-object
                               "jobId" (job-identifier job) "output" "structured"
                               "offset" 0 "limit" 20)))
                (updates    (agentcomms:json-get structured "updates")))
           (test-assert (vectorp updates)
                        "structured transcript returns an update vector")
           (test-assert (plusp (length updates))
                        "structured transcript returns persisted updates")
           (test-assert (string= "user_message_chunk"
                                 (agentcomms:json-get (aref updates 0) "sessionUpdate"))
                        "structured transcript uses the canonical ACP update kind"))
         (let* ((page-size     7)
                (first-page
                  (application-client-job-transcript
                   (acp-session-test--application session)
                   (agentcomms:json-object
                    "jobId" (job-identifier job) "offset" 0 "limit" page-size)))
                (first-content (agentcomms:json-get first-page "content"))
                (next          (agentcomms:json-get first-page "nextOffset"))
                (second-page
                  (application-client-job-transcript
                   (acp-session-test--application session)
                   (agentcomms:json-object
                    "jobId" (job-identifier job) "offset" next "limit" page-size))))
           (test-assert (string= first-content (subseq full 0 page-size))
                        "transcript first page is character bounded")
           (test-assert (= next page-size) "transcript advances by returned characters")
           (test-assert (string= (agentcomms:json-get second-page "content")
                                 (subseq full page-size
                                         (min (length full) (* 2 page-size))))
                        "transcript second page starts at nextOffset"))
         (let* ((whole     (application-client-job-transcript
                            (acp-session-test--application session) (agentcomms:json-object "jobId" (job-identifier job)
                                                                     "output" "structured")))
                (updates   (agentcomms:json-get whole "updates"))
                (collected nil)
                (offset    0))
            (let ((replayed  nil)
                  (calls     0)
                  (projector (symbol-function 'acp-replay--record-update)))
              (test-call-with-function-replacements
               (list (list 'acp-replay--record-update
                           (lambda (record turn-sequence)
                             (incf calls)
                             (funcall projector record turn-sequence))))
               (lambda ()
                 (acp-replay-map-updates
                  (conversation-replay-load-identity (conversation-pathname conversation))
                  (lambda (update) (push update replayed)))))
              (test-assert (and (plusp calls)
                                (string= (json-encode updates)
                                         (json-encode (coerce (nreverse replayed) 'vector))))
                           "replay resolves the current named projector and matches the segmented query"))
           (loop
             (let* ((response (agentcomms:client-agent-request
                               client "_autolith/job-transcript"
                               (agentcomms:json-object "sessionId" session-id
                                                      "jobId" (job-identifier job)
                                                      "output" "structured" "offset" offset "limit" 3)))
                    (page     (agentcomms:json-get response "value"))
                    (items    (agentcomms:json-get page "updates")))
               (test-assert (equal "ok" (agentcomms:json-get response "outcome"))
                            "structured pages cross the wire as typed success")
               (when (zerop offset)
                 (test-assert (= 3 (length items)) "one page retains more than two updates"))
               (setf collected (nconc collected (coerce items 'list)))
               (when (eq t (agentcomms:json-get page "complete"))
                 (test-assert (null (agentcomms:json-get page "nextOffset"))
                              "snapshot exhaustion encodes a null cursor")
                 (return))
               (let ((next (agentcomms:json-get page "nextOffset")))
                 (test-assert (= next (+ offset (length items))) "cursor advances by returned updates")
                 (setf offset next))))
           (test-assert (string= (json-encode updates) (json-encode (coerce collected 'vector)))
                        "paged updates preserve all content in chronological order")
           (let ((call (aref updates 1)) (result (aref updates 2))
                 (next-call (aref updates 3)) (next-result (aref updates 4)))
             (test-assert (equal (gethash "toolCallId" call) (gethash "toolCallId" result))
                          "call and result share a qualified identity")
             (test-assert (equal (gethash "toolCallId" next-call) (gethash "toolCallId" next-result))
                          "automatic turns retain call and result pairing")
             (test-assert (not (eql (parse-integer (gethash "toolCallId" call) :junk-allowed t)
                                    (parse-integer (gethash "toolCallId" next-call) :junk-allowed t)))
                          "automatic messages advance the durable tool identity prefix")
             (test-assert (and (equal "done" (gethash "rawOutput" result))
                               (plusp (length (gethash "content" result))))
                          "structured tool results include their visible output"))
           (let* ((prefix (application-client-job-transcript
                           application (agentcomms:json-object "jobId" (job-identifier job)
                                                              "output" "structured" "limit" 5)))
                  (*application-client-job-structured-response-limit*
                    (length (utf8-string-to-octets
                             (json-encode (acp-extension-result ':ok :value prefix))))))
             (let ((page (application-client-job-transcript
                          application (agentcomms:json-object "jobId" (job-identifier job)
                                                             "output" "structured" "limit" 20))))
               (test-assert (and (= 5 (length (gethash "updates" page)))
                                 (= 5 (gethash "nextOffset" page)))
                            "byte admission stops before a giant update rather than skipping to later small updates")))
           (let ((*application-client-job-structured-response-limit* 512))
             (test-assert
              (handler-case
                  (progn (application-client-job-transcript
                          (acp-session-test--application session) (agentcomms:json-object "jobId" (job-identifier job)
                                                                   "output" "structured" "offset" 5))
                         nil)
                (agentcomms:acp-error () t))
              "a giant multibyte update fails explicitly at its own offset")
             (let ((page (application-client-job-transcript
                          (acp-session-test--application session) (agentcomms:json-object "jobId" (job-identifier job)
                                                                   "output" "structured" "limit" 1))))
               (test-assert (<= (length (utf8-string-to-octets
                                         (json-encode (acp-extension-result ':ok :value page)))) 512)
                            "the bound includes the encoded typed response envelope")))
           (with-open-file (stream (conversation-log-pathname conversation)
                                   :direction ':output :if-exists ':append)
             (write-string "(" stream))
           (let ((page (application-client-job-transcript
                        (acp-session-test--application session) (agentcomms:json-object "jobId" (job-identifier job)
                                                                 "output" "structured"))))
             (test-assert (string= (json-encode updates) (json-encode (gethash "updates" page)))
                          "a torn tail preserves the last complete snapshot")))
         (let ((invalid nil))
           (handler-case
               (application-client-job-transcript
                (acp-session-test--application session)
                (agentcomms:json-object
                 "jobId" (job-identifier job) "output" "structured" "offset" -1))
             (agentcomms:acp-error () (setf invalid t)))
           (test-assert invalid "negative structured offsets are rejected"))
        nil)))))

(-> test-acp-extension-job-transcript-streaming () null)
(defun test-acp-extension-job-transcript-streaming ()
  "Bound actual ACP construction independently of transcript size and preserve wire admission."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id   (agentcomms:client-new-session client (namestring root)))
              (application  (acp-session-test--application (acp-service--session service session-id)))
              (orchestrator (application--task-orchestrator application))
              (job          (task-tests--register-job orchestrator (application-agent application)
                                                      (acp-extension-job-tests--definition)
                                                      :name "streaming"))
              (conversation (agent-conversation (task-tests--child-viewer configuration job)))
              (body         (format nil "λ\"\\~%"))
              (constructed  0)
              (constructor  (symbol-function 'agentcomms:acp-text-content)))
         (labels ((query (output offset limit)
                    "Read the owned transcript through its public endpoint."
                    (application-client-job-transcript
                     application (agentcomms:json-object "jobId" (job-identifier job)
                                                        "output" output "offset" offset "limit" limit)))

                  (wire-size (page)
                    "Measure the complete typed success response."
                    (length (utf8-string-to-octets
                             (json-encode (acp-extension-result ':ok :value page))))))
           (test-call-with-function-replacements
            (list (list 'conversation-load
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error "Transcript queries must not reconstruct conversation state.")))
                  (list 'agentcomms:acp-text-content
                        (lambda (&rest arguments)
                          (incf constructed)
                          (apply constructor arguments))))
            (lambda ()
              (dolist (output '("text" "structured"))
                (let ((empty (query output 100 2)))
                  (test-assert (and (= 0 (gethash "total" empty))
                                    (= 0 (gethash "offset" empty))
                                    (eq t (gethash "complete" empty)))
                               "missing transcript clamps an off-end cursor")))
              (conversation-append-record conversation (list :provider :metadata nil))
              (task-tests--publish-terminal
               job :completed (acp-extension-job-tests--result job (conversation-pathname conversation)))
              (dolist (output '("text" "structured"))
                (let ((empty (query output 100 2)))
                  (test-assert (and (= 0 (gethash "total" empty))
                                    (= 0 (gethash "offset" empty))
                                    (eq t (gethash "complete" empty)))
                               "empty durable transcript clamps an off-end cursor")))
              (dolist (count '(20 180))
                (dotimes (index count)
                  (conversation-append-user-message conversation body))
                (let* ((total (if (= count 20) 20 200))
                       (late  (- total 2)))
                  (setf constructed 0)
                  (let ((page (query "structured" late 1)))
                    (test-assert (and (= 1 constructed) (= total (gethash "total" page))
                                      (= (1+ late) (gethash "nextOffset" page))
                                      (= 1 (length (gethash "updates" page))))
                                 "late logical offsets skip projection and count every update"))
                  (setf constructed 0)
                  (let ((page (query "structured" (+ total 10) 2)))
                    (test-assert (and (zerop constructed) (= total (gethash "offset" page))
                                      (eq t (gethash "complete" page))
                                      (eq ':null (gethash "nextOffset" page)))
                                 "off-end structured pages construct nothing"))
                  (let* ((text-total (* total (+ 7 (length body))))
                         (page       (query "text" (- text-total 3) 2)))
                    (test-assert (and (= text-total (gethash "total" page))
                                      (= 2 (length (gethash "content" page)))
                                      (= (1- text-total) (gethash "nextOffset" page)))
                                 "text offsets count characters rather than encoded bytes")
                    (let ((empty (query "text" (+ text-total 10) 2)))
                      (test-assert (and (= text-total (gethash "offset" empty))
                                        (= text-total (gethash "nextOffset" empty))
                                        (zerop (length (gethash "content" empty)))
                                        (eq t (gethash "complete" empty)))
                                   "off-end text pages clamp to the exact character total")))
                  (dolist (offset (list 0 (1- total)))
                    (let* ((one                                                (query "structured" offset 1))
                           (*application-client-job-structured-response-limit* (wire-size one)))
                      (setf constructed 0)
                      (let ((page (query "structured" offset 200)))
                        (test-assert (and (= 1 (length (gethash "updates" page)))
                                          (= total (gethash "total" page))
                                          (eq (gethash "complete" one) (gethash "complete" page))
                                          (eql (gethash "nextOffset" one) (gethash "nextOffset" page))
                                          (= (wire-size page) *application-client-job-structured-response-limit*)
                                          (< constructed 10))
                                     "terminal and nonterminal escaped multibyte pages admit the exact envelope bound"))
                      (decf *application-client-job-structured-response-limit*)
                      (test-assert
                       (handler-case (progn (query "structured" offset 200) nil)
                         (agentcomms:acp-error () t))
                       "one byte below either exact envelope rejects the first update")))))
              (let ((*application-client-job-structured-response-limit* 1))
                (setf constructed 0)
                (test-assert
                 (handler-case (progn (query "structured" 0 200) nil)
                   (agentcomms:acp-error () t))
                 "a failing array lower bound retains the original oversized-first rejection")
                (test-assert (= 1 constructed) "lower-bound failure stops constructors immediately"))
              ;; Replay accepts durable sequence gaps and ignores an incomplete last form.
              (with-open-file (stream (conversation-log-pathname conversation)
                                      :direction ':output :if-exists ':append)
                (let ((*print-readably* t))
                  (print (list :message :seq (+ 10 (conversation-next-sequence conversation))
                               :role ':user :content "gap") stream))
                (write-string "(" stream))
              (let ((page (query "structured" 200 5)))
                (test-assert (and (= 201 (gethash "total" page))
                                  (= 1 (length (gethash "updates" page)))
                                  (eq t (gethash "complete" page)))
                             "gap-tolerant streaming ignores a torn final form")))))))))
  nil)

(-> test-acp-extension-job-steer-and-cancel-authority () null)
(defun test-acp-extension-job-steer-and-cancel-authority ()
  "Exercise steering and cancellation for a real owned live job."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((session-id       (agentcomms:client-new-session client (namestring root)))
              (session          (acp-service--session service session-id))
              (application      (acp-session-test--application session))
              (viewer           (application-agent application))
              (orchestrator     (application--task-orchestrator application))
              (definition       (acp-extension-job-tests--definition))
              (job              (let ((job (task-tests--register-job orchestrator viewer definition
                                                                    :name "live-control")))
                                  (setf (job-state job) ':running)
                                  job))
              (foreign-agent    (task-tests--primary-agent configuration "foreign-control"))
              (foreign          (task-tests--register-job orchestrator foreign-agent definition
                                                          :name "foreign-control"))
              (sent             (application-client-job-send
                                 (acp-session-test--application session)
                                 (agentcomms:json-object
                                  "jobId" (job-identifier job) "text" "steer this job")))
              (cancelled        (application-client-job-cancel
                                 (acp-session-test--application session)
                                 (agentcomms:json-object "jobId" (job-identifier job))))
              (foreign-rejected nil))
         (handler-case
             (application-client-job-cancel
              (acp-session-test--application session) (agentcomms:json-object "jobId" (job-identifier foreign)))
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
       (let* ((session-id   (agentcomms:client-new-session client (namestring root)))
              (session      (acp-service--session service session-id))
              (application  (acp-session-test--application session))
              (viewer       (application-agent application))
              (orchestrator (application--task-orchestrator application))
              (definition   (acp-extension-job-tests--definition))
              (job          (task-tests--register-job orchestrator viewer definition
                                                       :name "durable"))
              (child        (task-tests--child-viewer configuration job))
              (conversation (agent-conversation child))
              (needle       "durable ACP transcript"))
         (conversation-append-user-message conversation needle)
         (let ((result-path (merge-pathnames "result.sexp" (task--artifact-root configuration job)))
               (identifier  (task-job-execution-identifier job)))
           (ensure-directories-exist result-path)
           (loop for (status expected) in '((:success "completed") (:failed "failed") (:aborted "cancelled"))
                 for result = (acp-extension-job-tests--result job (conversation-pathname conversation))
                 do (setf (getf result :status) status)
                    (snapshot-write result-path result)
                    (let ((row (application-client-job (acp-session-test--application session) (agentcomms:json-object "jobId" identifier))))
                      (test-assert (string= identifier (agentcomms:json-get row "id"))
                                   "durable result is projected by job detail")
                      (test-assert (string= expected (agentcomms:json-get row "state"))
                                   "durable success, failure and cancellation retain their outcomes")))
           (let* ((result      (acp-extension-job-tests--result job (conversation-pathname conversation)))
                  (registry    (agent-tool-registry viewer))
                  (identifier  (make-identifier))
                  (result-path (merge-pathnames
                                (format nil "~A/result.sexp" identifier)
                                (task--artifact-group-root
                                 configuration (conversation-identifier (agent-conversation viewer))))))
            (setf (getf result :id) "stored-child"
                  (getf result :state) ':queued
                  (getf result :parent-id) "parent-job"
                  (getf result :artifact-path) "declared-artifact")
            (ensure-directories-exist result-path)
            (snapshot-write result-path result)
            (let* ((row    (application-client-job application (agentcomms:json-object "jobId" identifier)))
                   (native (tool-execute
                            (tool-registry-find registry "job" "get")
                            (make-instance 'tool-context :configuration configuration
                                           :worker nil :conversation (agent-conversation viewer)
                                           :registry registry :agent viewer :call-id "query-projection")
                            (json-object "id" identifier)))
                   (record (getf (rest (tool-result-details native)) :job)))
              (test-assert (and (string= identifier (agentcomms:json-get row "id"))
                                (string= "pending" (agentcomms:json-get row "state"))
                                (string= "parent-job" (agentcomms:json-get row "parentId"))
                                (string= "declared-artifact"
                                         (agentcomms:json-get (agentcomms:json-get row "artifacts") "result")))
                           "ACP retains requested identity, explicit state and declared artifact")
              (test-assert (and (string= "stored-child" (getf record :id))
                                (eq ':completed (getf record :state))
                                (string= (namestring result-path)
                                         (getf (getf (getf record :result) :artifact) :path)))
                           "Native inspection uses stored identity, terminal status and actual artifact pathname")))
           (let ((transcript (application-client-job-transcript
                              (acp-session-test--application session) (agentcomms:json-object "jobId" identifier "offset" 0 "limit" 4096))))
             (test-assert (search needle (agentcomms:json-get transcript "content"))
                          "durable result exposes its conversation transcript")))))))
  nil)
