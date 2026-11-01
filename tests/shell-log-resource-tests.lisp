(in-package #:autolith)

;;;; -- Shell Capture Resource Fixtures --

(-> shell-log-tests--context (configuration string) tool-context)
(defun shell-log-tests--context (configuration identifier)
  "Create a standalone requesting conversation without a runtime."
  (make-instance 'tool-context :configuration configuration :worker nil
                 :conversation (conversation-create configuration :identifier identifier)))

(-> shell-log-tests--capture (shell-log-artifact vector &optional keyword) t)
(defun shell-log-tests--capture (artifact bytes &optional (stream ':output))
  "Write exact fixture octets and return closed generic capture evidence."
  (let* ((path    (merge-pathnames (if (eq stream ':output) "output.bin" "error.bin")
                                   (shell-log-capture-directory artifact)))
         (capture (make-instance 'cl-exec-sandbox:sandbox-capture :path path)))
    (with-open-file (output path :direction ':output :if-exists ':supersede
                                :element-type '(unsigned-byte 8))
      (write-sequence bytes output))
    (setf (cl-exec-sandbox:sandbox-capture-byte-count capture) (length bytes)
          (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) (length bytes)
          (cl-exec-sandbox:sandbox-capture-complete-p capture) t
          (cl-exec-sandbox:sandbox-capture-status capture) ':complete)
    capture))

(-> shell-log-tests--finish (shell-log-artifact vector &key (:error-bytes (option vector))) list)
(defun shell-log-tests--finish (artifact bytes &key error-bytes)
  "Publish merged or separated fixture captures through the generic result boundary."
  (shell-log-record-capture
   artifact
   (make-instance 'cl-exec-sandbox:sandbox-result
                  :output-capture (shell-log-tests--capture artifact bytes)
                  :error-capture  (and error-bytes (shell-log-tests--capture artifact error-bytes ':error))
                  :exit-code 7 :timed-out-p nil :cancelled-p nil :status ':exited)))

(-> shell-log-tests--resolve (shell-log-artifact tool-context &optional keyword) shell-log-resource)
(defun shell-log-tests--resolve (artifact context &optional (stream ':output))
  "Reopen a reference through the authorized resolver rather than retained objects."
  (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                             (subseq (shell-log-reference artifact stream) 10) context))

(-> shell-log-tests--read (shell-log-resource tool-context hash-table) tool-result)
(defun shell-log-tests--read (resource context arguments)
  "Invoke the model-facing bounded resource adapter."
  (resource-tool-read
   resource (make-instance 'resource-read-tool
                           :namespace "resource" :name "read" :description "Read shell capture."
                           :parameters        (tool-object-schema (json-object) '())
                           :resource-registry (make-resource-registry))
   context arguments))

(-> shell-log-tests--public-inventory () null)
(defun shell-log-tests--public-inventory ()
  "Exercise bounded job metadata, public projection and missing-manifest recovery."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-inventory"))
           (orchestrator                   (task-orchestrator-create))
           (job                            (task-tests--make-job orchestrator :identifier "inventory-job"
                                                                 :root-identifier         "shell-inventory"))
           (latest                         nil)
           (base-time                      (get-universal-time)))
      (unwind-protect
           (progn
             (loop for index from 1 to 17
                   for artifact = (shell-log-allocate context :job job)
                   do (shell-log-tests--finish artifact (utf8-string-to-octets "x"))
                      (setf (getf (shell-log-artifact-metadata artifact) :created-at)
                            (+ base-time index)
                            latest artifact)
                      (shell-log--write-manifest artifact))
             (let* ((metadata (shell-log-job-metadata configuration job))
                    (logs     (getf metadata :shell-logs)))
               (test-assert (and (= 16 (length logs)) (= 1 (getf metadata :shell-logs-omitted)))
                            "Job inspection exposes bounded newest logs and explicit omissions")
               (test-assert (equal (getf (first logs) :artifact-id)
                                   (getf (shell-log-artifact-metadata latest) :artifact-id))
                            "Newest public inventory follows durable creation order")
               (test-assert
                (every (lambda (log)
                         (and (not (getf log :owner-execution-ids))
                              (not (getf log :completion-owner-conversation))
                              (every (lambda (capture) (not (getf capture :file)))
                                     (getf log :captures)))) logs)
                "Public inventory omits private filenames and ancestor ownership")
               (test-assert (< (length (utf8-string-to-octets (task--write-readable-sexp metadata)))
                               (* 64 1024))
                            "Public shell metadata fits below the tool metadata bound"))
             (let* ((missing-job (task-tests--make-job orchestrator :identifier "missing-job"
                                                       :root-identifier         "shell-inventory"))
                    (artifact    (shell-log-allocate context :job missing-job)))
               (shell-log-tests--finish artifact (utf8-string-to-octets "x"))
               (platform-delete-file *platform* (shell-log--manifest-path
                                                 (shell-log-capture-directory artifact)))
               (test-assert
                (eq ':unavailable (getf (first (getf (shell-log-job-metadata configuration missing-job)
                                                    :shell-logs)) :state))
                "Missing manifests are bounded diagnostics rather than terminal inspection failures")))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--descendant-authority () null)
(defun shell-log-tests--descendant-authority ()
  "Check current child ownership through the resolver, including sibling refusal."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-descendants"))
           (orchestrator                   (task-orchestrator-create))
           (job                            (task-tests--make-job orchestrator :identifier "current-child"
                                                                 :root-identifier         "shell-descendants"))
           (agent                          (task-tests--child-viewer configuration job))
           (child-context                  (make-instance 'tool-context :configuration configuration :worker nil
                                                          :conversation (agent-conversation agent) :agent agent))
           (artifact                       (shell-log-allocate context)))
      (unwind-protect
           (progn
             (shell-log-tests--finish artifact (utf8-string-to-octets "owned descendant"))
             (setf (getf (shell-log-artifact-metadata artifact) :owner-execution-ids)
                   (list (session-job-execution-identifier job)))
             (shell-log--write-manifest artifact)
             (test-assert
              (tool-result-success-p (shell-log-tests--read
                                      (shell-log-tests--resolve artifact child-context)
                                      child-context (json-object)))
              "A current child may inspect captures owned by its descendants")
             (setf (getf (shell-log-artifact-metadata artifact) :owner-execution-ids)
                   (list "different-sibling-execution"))
             (shell-log--write-manifest artifact)
             (test-assert
              (handler-case (progn (shell-log-tests--resolve artifact child-context) nil)
                (resource-access-denied () t))
              "A sibling execution is refused even within the same root session"))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--delivery-and-accounting () null)
(defun shell-log-tests--delivery-and-accounting ()
  "Protect undelivered sync logs, verify ordinary receipts, and measure actual disk use."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 96)
           (context                          (shell-log-tests--context configuration "shell-sync-proof"))
           (orchestrator                     (task-orchestrator-create))
           (job                              (task-tests--make-job orchestrator :identifier "sync-proof-job"
                                                                   :root-identifier "shell-sync-proof" :detached-p nil)))
      (unwind-protect
           (progn
             (setf (slot-value job 'completion-owner-conversation) "shell-sync-proof"
                   (slot-value job 'parent-call-id) "expected-shell-call")
             (let* ((artifact (shell-log-allocate context :job job))
                    (metadata (shell-log-tests--finish artifact (utf8-string-to-octets "x"))))
              (setf (getf metadata :created-at)
                    (- (get-universal-time) *shell-log-retention-age* 1))
              (shell-log--write-manifest artifact)
              (test-assert
               (= 1 (getf (shell-log--read-manifest
                           configuration (shell-log-capture-directory artifact))
                          :accounted-bytes))
               "Closed captures account for retained bytes, not their launch reservation")
              (test-assert (not (shell-log--prunable-p configuration
                                                      (shell-log-capture-directory artifact) metadata))
                           "Closing a synchronous job does not prove delivery")
               ;; Simulate an oversized regular file independently of the executor.
              (shell-log-tests--capture artifact
                                        (make-array 100 :element-type    '(unsigned-byte 8)
                                                        :initial-element 120))
              (let* ((measured (shell-log--read-manifest configuration
                                                        (shell-log-capture-directory artifact)))
                     (capture  (first (getf measured :captures))))
                (test-assert (and (= 100 (getf measured :accounted-bytes))
                                  (= 100 (getf capture :byte-count))
                                  (getf capture :oversized-p) (not (getf capture :complete-p)))
                             "Accounting includes actual oversized safe regular files")
                (test-assert (handler-case (progn (shell-log-allocate context) nil)
                               (shell-log-error () t))
                             "Actual stored bytes cannot evade aggregate reservation limits"))
              (labels ((append-receipt (call-id)
                         "Persist an ordinary owner result containing this execution identity."
                         (conversation-append-record
                          (tool-context-conversation context)
                          (list :tool-result :call-id call-id :tool "shell.run"
                                :status ':ok :category ':success :output "finished"
                                :details (list :shell-logs (list metadata))))))
                (append-receipt "unrelated-shell-call")
                (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                             "A different provider call cannot acknowledge this execution")
                (append-receipt "expected-shell-call")
                (test-assert (shell-log--sync-delivered-p configuration metadata)
                             "Exact-owner durable terminal tool metadata proves sync delivery")
                (setf (getf metadata :detached-p) t)
                (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                             "Detached jobs require a completion receipt, not an ordinary tool result")
                (setf (getf metadata :detached-p) nil)
                (let ((settled (copy-list metadata)))
                  (setf (getf settled :parent-call-id) "never-recorded-call"
                        (getf settled :created-at)     (- (get-universal-time)
                                                          *shell-log-retention-age* 1))
                  (test-assert (not (shell-log--sync-delivered-p configuration settled))
                               "Age cannot acknowledge a provider call that never recorded its result"))
                (let* ((conversation (tool-context-conversation context))
                       (identity     (conversation-pathname conversation))
                       (active       (conversation-storage-active-pathname identity))
                       (write-record (symbol-function 'task-continuity--write)))
                  (test-call-with-function-replacements
                   (list (list 'task-continuity--write
                               (lambda (path record &key require-absent)
                                 (if (member "shell-delivery" (pathname-directory path) :test #'equal)
                                     (shell-log--fail "Injected receipt publication failure.")
                                     (funcall write-record path record :require-absent require-absent)))))
                   (lambda ()
                     (test-assert
                      (handler-case (progn (conversation-append-summary conversation "Failed checkpoint") nil)
                        (shell-log-error () t))
                      "Receipt publication failure vetoes segment rotation")))
                  (test-assert (equal active (conversation-storage-active-pathname identity))
                               "Failed receipt publication preserves the committed active segment")
                  (test-assert (shell-log--sync-delivered-p configuration metadata)
                               "The committed result is sufficient after failed receipt publication")
                  (let ((done     (sb-thread:make-semaphore))
                        (failure  nil)
                        (thread   nil)
                        (finished nil))
                    (with-shell-log-lock (configuration)
                      (setf thread
                            (make-thread
                             (lambda ()
                               (unwind-protect
                                    (handler-case
                                        (conversation-append-summary (conversation-load identity) "Shell checkpoint")
                                      (error (condition) (setf failure condition)))
                                 (sb-thread:signal-semaphore done)))
                             :name "shell delivery rotation"))
                      (setf finished (sb-thread:wait-on-semaphore done :timeout 5)))
                    (join-thread thread)
                    (test-assert (and finished (null failure))
                                 "Rotation publishes committed receipts without waiting on the shell-log lock"))
                  (test-call-with-function-replacements
                   (list (list 'conversation--map-segment-records
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (error "A preserved receipt must not scan transcript segments."))))
                   (lambda ()
                     (test-assert (shell-log--sync-delivered-p configuration metadata)
                                  "Exact delivery survives rotation and reload without scanning history")))
                  (multiple-value-bind (values worker)
                      (test-conversation--worker-eval configuration
                        (format nil "(let* ((configuration ~A) (directory ~S)
                                            (metadata (shell-log--read-manifest configuration directory :measure-p nil)))
                                       (shell-log--sync-delivered-p configuration metadata))"
                                (test-conversation--child-configuration-form configuration)
                                (shell-log-capture-directory artifact)))
                    (unwind-protect
                         (test-assert (equal values '("T"))
                                      "A fresh process recognizes exact delivery from the rotated owner")
                      (lisp-worker-stop worker)))
                  (dolist (field '(:artifact-id :completion-owner-conversation :parent-call-id))
                    (let ((wrong (copy-list metadata)))
                      (setf (getf wrong field) "different-identity")
                      (test-assert (not (shell-log--sync-delivered-p configuration wrong))
                                   "A preserved receipt refuses a different artifact, owner or provider call")))
                  (let ((stale (copy-list metadata)))
                    (setf (getf stale :state) ':interrupted)
                    (test-assert (shell-log--sync-delivered-p configuration stale)
                                 "A stale interrupted manifest retains exact committed delivery"))))
              (let ((replacement (shell-log-allocate context)))
                (test-assert (eq ':pruned (getf (shell-log--read-manifest
                                                 configuration (shell-log-capture-directory artifact)) :state))
                             "Delivered sync logs are eligible for safe quota pruning")
                (shell-log-release-artifact replacement))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-shell-log-child-delivery () null)
(defun test-shell-log-child-delivery ()
  "Prune child captures only after delivery in the child's private transcript."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (root                              "shell-child-delivery")
           (orchestrator                      (task-orchestrator-create))
           (child-job                         (task-tests--make-job orchestrator :identifier "child-owner"
                                                                     :root-identifier        root))
           (owner                             (task-job-execution-identifier child-job))
           (child                             (conversation-create configuration :identifier owner
                                                                   :storage-root             (task--artifact-root configuration child-job)))
           (unrelated                         (conversation-create configuration :identifier owner))
           (context                           (make-instance 'tool-context :configuration configuration
                                                                          :conversation child :worker nil))
           (job                               (task-tests--make-job orchestrator :identifier "child-command"
                                                                     :root-identifier root :detached-p nil)))
      (unwind-protect
           (progn
             (conversation-append-user-message child "Run one command.")
             (setf (slot-value job 'completion-owner-conversation) owner
                   (slot-value job 'parent-call-id) "child-shell-call")
             (let* ((artifact  (shell-log-allocate context :job job))
                    (metadata  (shell-log-tests--finish artifact (utf8-string-to-octets "child-output")))
                    (directory (shell-log-capture-directory artifact))
                    (capture   (merge-pathnames "output.bin" directory))
                    (receipt   (list :tool-result :call-id "child-shell-call" :tool "shell.run"
                                     :status ':ok :category ':success :output "finished"
                                     :details (list :shell-logs
                                                    (list (shell-log--public-metadata metadata))))))
               (conversation-append-record unrelated receipt)
               (let ((replacement (handler-case (shell-log-allocate context)
                                    (shell-log-error () nil))))
                 (when replacement (shell-log-release-artifact replacement))
                 (test-assert (null replacement)
                              "A same-ID primary transcript cannot acknowledge a child capture"))
               (test-assert (probe-file capture)
                            "Undelivered child output remains available under quota pressure")
               (conversation-append-record child receipt)
               (let ((replacement (shell-log-allocate context)))
                 (unwind-protect
                      (test-assert
                       (and (eq ':pruned (getf (shell-log--read-manifest configuration directory) :state))
                            (not (probe-file capture)))
                       "The exact child transcript permits quota pruning of delivered output")
                   (shell-log-release-artifact replacement)))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--pre-executor-unwind () null)
(defun shell-log-tests--pre-executor-unwind ()
  "Release real pre-executor failures even when a synchronous job has no receipt."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-preflight"))
           (orchestrator                      (task-orchestrator-create))
           (*tool-execution-current-job*
             (task-tests--make-job orchestrator :identifier "preflight-job"
                                  :root-identifier "shell-preflight" :detached-p nil)))
      (unwind-protect
           (let ((original (fdefinition 'shell-log--prune))
                 (enabled  nil))
             (test-call-with-function-replacements
              (list (list 'shell-log--prune
                          (lambda (configuration &rest arguments)
                            (push sb-sys:*interrupts-enabled* enabled)
                            (apply original configuration arguments))))
              (lambda ()
                (shell-log-tests--run-pre-executor-failures configuration context)))
             (test-assert (and enabled (every #'identity enabled))
                          "Pruning runs with interrupts enabled so garbage collection can proceed"))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--run-pre-executor-failures (configuration tool-context) null)
(defun shell-log-tests--run-pre-executor-failures (configuration context)
  "Run two commands that fail before launch and check each releases its capture."
  (loop repeat 2
        do (test-assert
            (handler-case
                (progn
                  (workspace-tool-run-shell-command
                   (test-fixture-shell-command *platform* "exit 0" "exit 0")
                   (config :working-directory configuration)
                   :policy       ':invalid-policy
                   :timeout      1
                   :output-limit 32
                   :context      context)
                  nil)
              (error () t))
            "Policy validation fails before the executor can launch")
           (test-assert (zerop (hash-table-count *shell-log-active*))
                        "A live worker releases the abandoned artifact pin")
           (let* ((metadata
                    (loop for directory in (shell-log--directories configuration)
                          for item = (shell-log--read-manifest configuration directory)
                          when (eq (getf item :state) ':closed) return item)))
             (test-assert (and metadata (eq ':interrupted (getf metadata :status))
                               (zerop (getf metadata :reserved-bytes))
                               (null (getf metadata :captures)))
                          "Pre-executor failures free quota without inventing captures")))
  nil)


;;;; -- Durable Storage and Inspection --

(-> test-shell-log-private-allocation () null)
(defun test-shell-log-private-allocation ()
  "Reserve before launch; generate distinct execution identities without a runtime."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 96)
           (context                          (shell-log-tests--context configuration "shell-owner"))
           (first                            (shell-log-allocate context))
           (second                           (shell-log-allocate context :merge-output-p nil)))
      (test-assert (probe-file (shell-log--manifest-path (shell-log-capture-directory first)))
                   "Ownership exists before any capture launch")
      (test-assert (not (equal (getf (shell-log-artifact-metadata first) :execution-id)
                               (getf (shell-log-artifact-metadata second) :execution-id)))
                   "Standalone invocations have stable independent execution identities")
      (test-assert (= 64 (getf (shell-log-artifact-metadata second) :reserved-bytes))
                   "Separated streams reserve both finite allowances")
      (test-assert (handler-case (progn (shell-log-allocate context) nil)
                     (shell-log-error () t))
                   "Active captures cannot be pruned to evade aggregate reservation")
      (with-test-fixture (':file-modes "private shell artifact permissions")
        (test-assert (= 0 (logand #o077 (platform-file-permissions
                                         *platform* (shell-log-capture-directory first))))
                     "Private capture directories exclude group and other access"))))
  (shell-log-tests--public-inventory)
  (shell-log-tests--delivery-and-accounting)
  (shell-log-tests--pre-executor-unwind)
  nil)

(-> test-shell-log-byte-windows-and-search () null)
(defun test-shell-log-byte-windows-and-search ()
  "Read exact raw ranges and bounded search pages, including the shared fifty-result cap."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*              (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*  256)
           (*shell-log-resource-scan-bytes* 16)
           (context                         (shell-log-tests--context configuration "shell-windows"))
           (artifact                        (shell-log-allocate context))
           (bytes                           (make-array 96 :element-type '(unsigned-byte 8) :initial-element 120)))
      (replace bytes (utf8-string-to-octets "needle") :start1 14)
      (replace bytes (utf8-string-to-octets "needle") :start1 50)
      (setf (aref bytes 30) 255 (aref bytes 31) 195 (aref bytes 32) 169)
      (shell-log-tests--finish artifact bytes)
      (let* ((resource (shell-log-tests--resolve artifact context))
             (path     (merge-pathnames "output.bin" (shell-log-capture-directory artifact))))
        (multiple-value-bind (range size next)
            (shell-log-resource--read-bytes configuration path :offset 29 :count 4)
          (test-assert (equalp range (subseq bytes 29 33))
                       "Binary ranges preserve invalid bytes and UTF-8 boundaries")
          (test-assert (and (= size 96) (= next 33)) "Byte ranges report truthful positions"))
        (let ((result (shell-log-tests--read resource context
                                             (json-object "byte-offset" 30 "byte-count" 3))))
          (test-assert (tool-result-success-p result) "Invalid UTF-8 renders without losing retained raw bytes")
          (test-assert (< (length (tool-result-content result)) 7000) "Resource text is bounded inline"))
        (let ((offset 0) (found nil))
          (loop
            for page = (shell-log-resource--search configuration path :query "needle"
                                                   :offset offset :maximum 1)
            do (setf found (append found (mapcar (lambda (match) (getf match :byte-offset))
                                                 (getf page :matches))))
               (test-assert (<= (getf page :scanned-byte-count) 21) "Search scans finite byte budgets")
            while (getf page :next-byte-offset)
            do (test-assert (> (getf page :next-byte-offset) offset) "Search pagination always progresses")
               (setf offset (getf page :next-byte-offset)))
          (test-assert (equal found '(14 50)) "Search recovers middle and boundary-spanning matches exactly"))
        (let ((*shell-log-resource-scan-bytes* 1024))
          (dolist (maximum '(20 30 50))
            (test-assert
             (tool-result-success-p
              (shell-log-tests--read resource context (json-object "query" "x" "max-results" maximum)))
             "Resource reads accept explicit search limits through fifty"))
          (let* ((page  (shell-log-resource--search configuration path :query "x" :maximum 50))
                 (next  (shell-log-resource--search configuration path :query "x" :maximum 50
                                                   :offset (getf page :next-byte-offset)))
                 (found (mapcar (lambda (match) (getf match :byte-offset))
                                (append (getf page :matches) (getf next :matches)))))
            (test-assert (= 50 (length (getf page :matches))) "Explicit fifty-result pages honor their bound")
            (test-assert (equal found (loop for byte across bytes for index from 0
                                           when (= byte 120) collect index))
                         "Larger pages resume without dropping or repeating matches")
            (test-assert (null (getf next :next-byte-offset)) "The final search page reaches the end")))
        (test-assert
         (handler-case (progn (shell-log-tests--read resource context
                                                     (json-object "query" "x" "max-results" 51)) nil)
           (tool-error () t))
         "Search result limits above fifty are rejected")
        (test-assert
         (handler-case (progn (shell-log-tests--read resource context
                                                     (json-object "byte-count" 4097)) nil)
           (tool-error () t))
         "Oversized resource windows are rejected"))))
  nil)

(-> test-shell-log-resource-authority () null)
(defun test-shell-log-resource-authority ()
  "Reject cross-session references, traversal and symbolic-link capture substitution."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 256)
           (owner                          (shell-log-tests--context configuration "shell-authority"))
           (foreign                        (shell-log-tests--context configuration "shell-foreign"))
           (artifact                       (shell-log-allocate owner)))
      (shell-log-tests--finish artifact (utf8-string-to-octets "private diagnostics"))
      (test-assert
       (handler-case (progn (shell-log-tests--resolve artifact foreign) nil)
         (resource-access-denied () t))
       "A different root session cannot inspect private captures")
      (dolist (identifier '("shell-authority/../x/output" "shell-authority/x/y/../../output"
                            "shell-authority/x/y/%2e%2e"))
        (test-assert
         (handler-case
             (progn (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                                               identifier owner) nil)
           (resource-access-denied () t))
         "URI identifiers cannot introduce traversal"))
      (with-test-fixture (':symbolic-links "shell capture substitution")
        (let* ((path   (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
               (secret (merge-pathnames "secret.bin" root)))
          (with-open-file (stream secret :direction ':output :if-exists ':supersede)
            (write-string "foreign" stream))
          (platform-delete-file *platform* path)
          (test-fixture-make-symbolic-link *platform* (namestring secret) (namestring path))
          (test-assert
           (handler-case
               (progn (shell-log-tests--read (shell-log-tests--resolve artifact owner)
                                             owner (json-object)) nil)
             (shell-log-error () t))
           "Capture symlinks are refused before opening foreign bytes")))))
  (shell-log-tests--descendant-authority)
  nil)

(-> test-shell-log-reopen-and-pruning () null)
(defun test-shell-log-reopen-and-pruning ()
  "Reopen closed streams without replay and preserve missing/pruned diagnostics."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 64)
           (context                          (shell-log-tests--context configuration "shell-restart"))
           (artifact                         (shell-log-allocate context :merge-output-p nil))
           (reference                        (shell-log-reference artifact)))
      (shell-log-tests--finish artifact (utf8-string-to-octets "stdout")
                              :error-bytes (utf8-string-to-octets "stderr failure"))
      (let ((*shell-log-active* (make-hash-table :test #'equal)))
        (let ((reopened (shell-log-tests--resolve artifact context ':error)))
          (test-assert
           (search "stderr failure" (tool-result-content
                                      (shell-log-tests--read reopened context (json-object))))
           "Restart inspection recovers the separated error stream from its manifest")))
      (let* ((*shell-log-retention-byte-limit* 48)
             (replacement                      (shell-log-allocate context)))
        (test-assert (string= reference (shell-log-reference artifact)) "Pruning does not change stable references")
        (test-assert (eq ':pruned (getf (shell-log--read-manifest configuration
                                                               (shell-log-capture-directory artifact)) :state))
                     "Quota pressure prunes eligible closed captures")
        (test-assert (tool-result-success-p
                      (shell-log-tests--read (shell-log-tests--resolve artifact context) context (json-object)))
                     "Tombstones produce a diagnostic, not a failed session")
        (shell-log-release-artifact replacement))))
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-tombstones")))
      (dotimes (index 3)
        (shell-log-tests--finish (shell-log-allocate context)
                                (utf8-string-to-octets (princ-to-string index))))
      (let ((states (loop for directory in (shell-log--directories configuration)
                          collect (getf (shell-log--read-manifest configuration directory) :state))))
        (test-assert (and (= 1 (count ':closed states)) (= 1 (count ':pruned states)))
                     "One allocation enforces both live-artifact and tombstone count bounds"))))
  nil)

(-> test-shell-log-capture-failure-metadata () null)
(defun test-shell-log-capture-failure-metadata ()
  "Persist actual retained/observed bytes and incomplete execution outcomes."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-failures")))
      (dolist (status '(:timeout :cancelled :launch-failed :interrupted))
        (let* ((artifact (shell-log-allocate context))
               (capture  (shell-log-tests--capture artifact (utf8-string-to-octets "partial"))))
          (setf (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) 100
                (cl-exec-sandbox:sandbox-capture-complete-p capture) nil
                (cl-exec-sandbox:sandbox-capture-truncated-p capture) t
                (cl-exec-sandbox:sandbox-capture-status capture) ':limit)
          (let* ((metadata (shell-log-record-capture
                            artifact (make-instance 'cl-exec-sandbox:sandbox-result
                                                     :output-capture capture :exit-code nil
                                                     :status status :timed-out-p (eq status ':timeout)
                                                     :cancelled-p (eq status ':cancelled))))
                 (record   (first (getf metadata :captures))))
            (test-assert (and (eq status (getf metadata :status))
                             (= 7 (getf record :byte-count))
                             (= 100 (getf record :observed-byte-count))
                             (not (getf record :complete-p)) (getf record :truncated-p))
                         "Portable manifests preserve storage loss separately from execution failure"))))))
  nil)

(-> test-shell-log-cross-process-lifecycle () null)
(defun test-shell-log-cross-process-lifecycle ()
  "Keep live foreign captures readable and reserved, then reclaim after owner exit."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-process"))
           (ready                             (merge-pathnames "capture.sexp" root))
           (worker                            nil)
           (replacement                       nil))
      (unwind-protect
           (progn
             (multiple-value-bind (rendered child)
                 (test-conversation--worker-eval
                  configuration
                  (format nil
                          "(let* ((configuration ~A)
         (*shell-log-capture-byte-limit* 32)
         (*shell-log-retention-byte-limit* 32)
         (*shell-log-retention-count-limit* 1)
         (context (make-instance 'tool-context :configuration configuration :worker nil
                    :conversation (conversation-create configuration :identifier \"shell-process\")))
         (artifact (shell-log-allocate context))
         (path (merge-pathnames \"output.bin\" (shell-log-capture-directory artifact)))
         (capture (make-instance 'cl-exec-sandbox:sandbox-capture :path path)))
    (with-open-file (output path :direction ':output :element-type '(unsigned-byte 8))
      (write-sequence (utf8-string-to-octets \"live bytes\") output))
    (shell-log-record-capture artifact
      (make-instance 'cl-exec-sandbox:sandbox-result :output-capture capture
                     :exit-code nil :timed-out-p nil :cancelled-p nil :status ':running)
      :closed-p nil)
    (task-continuity--write ~S
      (list :reference (shell-log-reference artifact) :directory (namestring (shell-log-capture-directory artifact)))))"
                          (test-conversation--child-configuration-form configuration) ready))
               (declare (ignore rendered))
               (setf worker child))
             (let* ((published         (task-continuity--read ready))
                    (reference         (getf published :reference))
                    (capture-directory (getf published :directory)))
               (flet ((resolve ()
                        (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                                                   (subseq reference 10) context)))
                 (let ((resource (resolve)))
                   (test-assert (eq ':active (getf (shell-log-resource-metadata resource) :state))
                                "Another process's live capture is not interrupted")
                   (test-assert
                    (search "live bytes" (tool-result-content
                                              (shell-log-tests--read resource context (json-object))))
                    "A live foreign capture can be read through its authorized reference"))
                 (test-assert (handler-case
                                 (let ((unexpected (shell-log-allocate context)))
                                   (shell-log-release-artifact unexpected)
                                   nil)
                                (shell-log-error () t))
                              "A foreign owner's reservation prevents quota overcommit and pruning")
                 (test-assert (probe-file (merge-pathnames "output.bin" capture-directory))
                              "Quota pressure preserves the live capture file")
                 (lisp-worker-stop worker)
                 (setf worker nil)
                 (test-assert (eq ':interrupted (getf (shell-log-resource-metadata (resolve)) :state))
                              "Owner exit releases the kernel lease and exposes interruption")
                 (setf replacement (shell-log-allocate context))
                 (test-assert (eq ':pruned (getf (shell-log-resource-metadata (resolve)) :state))
                              "Quota pressure reclaims the dead owner's capture with a truthful tombstone")
                 (shell-log-release-artifact replacement)
                 (setf replacement nil)
                 (let ((*shell-log-tombstone-count-limit* 0))
                   (setf replacement (shell-log-allocate context)))
                 (test-assert (not (probe-file capture-directory))
                              "Tombstone expiry removes lease files along with expired metadata")))
             (shell-log-release-artifact replacement)
             (setf replacement nil)
             (multiple-value-bind (rendered child)
                 (test-conversation--worker-eval
                  configuration
                  (format nil
                          "(let* ((configuration ~A)
         (*shell-log-capture-byte-limit* 32)
         (*shell-log-retention-byte-limit* 32)
         (*shell-log-retention-count-limit* 1)
         (context (make-instance 'tool-context :configuration configuration :worker nil
                    :conversation (conversation-create configuration :identifier \"shell-orphan\")))
         (artifact (shell-log-allocate context)))
    (platform-delete-file *platform* (shell-log--manifest-path (shell-log-capture-directory artifact)))
    (task-continuity--write ~S (list :directory (namestring (shell-log-capture-directory artifact)))))"
                          (test-conversation--child-configuration-form configuration) ready))
               (declare (ignore rendered))
               (setf worker child))
             (let ((capture-directory (getf (task-continuity--read ready) :directory)))
               (lisp-worker-stop worker)
               (setf worker nil
                     replacement (shell-log-allocate context))
               (test-assert (not (probe-file capture-directory))
                            "A dead unpublished allocation does not block future shell commands")))
        (when worker (lisp-worker-stop worker))
        (when replacement (shell-log-release-artifact replacement)))))
  (with-test-configuration (configuration)
    (let* ((context   (shell-log-tests--context configuration "shell-staging"))
           (directory (merge-pathnames "tasks/shell-staging/interrupted/shell-log/unpublished/"
                                       (config :data-root configuration))))
      (ensure-directories-exist directory)
      (dolist (name '("owner.lock" ".sexp-store-A1.staging"))
        (with-open-file (stream (merge-pathnames name directory) :direction :output)
          (write-string "interrupted publication" stream)))
      (let ((artifact (shell-log-allocate context)))
        (unwind-protect
             (test-assert (not (probe-file directory))
                          "Interrupted manifest staging is reclaimed before the next allocation")
          (shell-log-release-artifact artifact)))))
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 0)
           (context                           (shell-log-tests--context configuration "shell-update-staging"))
           (original                          (shell-log-allocate context))
           (directory                         (shell-log-capture-directory original)))
      (shell-log-tests--finish original (utf8-string-to-octets "published"))
      (with-open-file (stream (merge-pathnames ".sexp-store-A1.staging" directory) :direction ':output)
        (write-string "interrupted later update" stream))
      (let ((replacement (shell-log-allocate context)))
        (unwind-protect
             (test-assert (not (probe-file directory))
                          "A published manifest's abandoned staging does not block tombstone expiry")
          (shell-log-release-artifact replacement)))))
  (with-test-configuration (configuration root)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 0)
           (context                           (shell-log-tests--context configuration "shell-staging-refusal"))
           (original                          (shell-log-allocate context))
           (directory                         (shell-log-capture-directory original))
           (staging                           (merge-pathnames ".sexp-store-A1.staging" directory)))
      (shell-log-tests--finish original (utf8-string-to-octets "published"))
      (dolist (name '("foreign.bin" ".sexp-store-a1.staging" ".sexp-store-A1.staging.extra"))
        (let ((path (merge-pathnames name directory)))
          (with-open-file (stream path :direction ':output)
            (write-string "unexpected file" stream))
          (test-assert
           (handler-case
               (let ((artifact (shell-log-allocate context)))
                 (shell-log-release-artifact artifact)
                 nil)
             (shell-log-error () t))
           "Tombstone expiry refuses unexpected names instead of deleting them")
          (test-assert (probe-file path) "Refused files are preserved")
          (platform-delete-file *platform* path)))
      (with-test-fixture (':symbolic-links "shell manifest staging substitution")
        (let ((target (merge-pathnames "staging-target.bin" root)))
          (with-open-file (stream target :direction ':output)
            (write-string "foreign contents" stream))
          (test-fixture-make-symbolic-link *platform* (namestring target) (namestring staging))
          (test-assert
           (handler-case
               (let ((artifact (shell-log-allocate context)))
                 (shell-log-release-artifact artifact)
                 nil)
             (shell-log-error () t))
           "A staging-shaped symbolic link cannot authorize tombstone deletion")
          (test-assert (and (probe-file target)
                            (platform-path-status *platform* staging :follow-links-p nil))
                       "Refused staging links and their targets are preserved")
          (platform-delete-file *platform* staging)))))
  nil)

(-> test-shell-log-concurrent-allocation () null)
(defun test-shell-log-concurrent-allocation ()
  "Pause one process after its quota scan and contend from another process."
  (with-test-configuration (configuration)
    (let ((first-worker  nil)
          (second-worker nil))
      (flet ((evaluate (worker form)
               (let ((response (lisp-worker-request worker ':eval (list :forms (list form)))))
                 (test-assert (eq (getf (rest response) :status) ':ok)
                              (format nil "Contention worker completed: ~S" response)))))
        (unwind-protect
             (progn
               (multiple-value-bind (rendered worker)
                   (test-conversation--worker-eval
                    configuration
                    (format nil
                            "(let* ((configuration ~A)
          (scanned (sb-thread:make-semaphore))
          (resume (sb-thread:make-semaphore))
          (done (sb-thread:make-semaphore))
          (hold (sb-thread:make-semaphore))
          (state (list :resume resume :done done :hold hold :result nil))
          (prune (symbol-function 'shell-log--prune)))
     (defparameter *shell-log-contention-fixture* state)
     (setf (symbol-function 'shell-log--prune)
           (lambda (configuration &key (reservation 0))
             (funcall prune configuration :reservation reservation)
             (sb-thread:signal-semaphore scanned)
             (unless (sb-thread:wait-on-semaphore resume :timeout 30)
               (error \"Allocation was never resumed\"))))
     (sb-thread:make-thread
      (lambda ()
        (let ((*shell-log-capture-byte-limit* 32)
              (*shell-log-retention-byte-limit* 32)
              (*shell-log-retention-count-limit* 1))
          (unwind-protect
               (handler-case
                   (setf (getf state :result)
                         (shell-log-allocate
                          (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation (conversation-create configuration :identifier \"shell-race-first\"))))
                 (error (condition) (setf (getf state :result) condition)))
            (setf (symbol-function 'shell-log--prune) prune)
            (sb-thread:signal-semaphore done))
          (sb-thread:wait-on-semaphore hold :timeout 30))))
     (unless (sb-thread:wait-on-semaphore scanned :timeout 30)
       (error \"First allocation never completed its scan\")))"
                            (test-conversation--child-configuration-form configuration)))
                 (declare (ignore rendered))
                 (setf first-worker worker))
               (multiple-value-bind (rendered worker)
                   (test-conversation--worker-eval
                    configuration
                    (format nil
                            "(let* ((configuration ~A)
          (entered (sb-thread:make-semaphore))
          (done (sb-thread:make-semaphore))
          (state (list :done done :result nil)))
     (defparameter *shell-log-contention-fixture* state)
     (handler-case
         (let ((lease (lease-acquire (merge-pathnames \"tasks/shell-log.lock\" (config :data-root configuration)))))
           (lease-release lease)
           (error \"Quota scan did not hold the cross-process allocation lock\"))
       (file-lock-busy () t))
     (sb-thread:make-thread
      (lambda ()
        (let ((*shell-log-capture-byte-limit* 32)
              (*shell-log-retention-byte-limit* 32)
              (*shell-log-retention-count-limit* 1))
          (sb-thread:signal-semaphore entered)
          (unwind-protect
               (setf (getf state :result)
                     (handler-case
                         (shell-log-allocate
                          (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation (conversation-create configuration :identifier \"shell-race-second\")))
                       (shell-log-error () :quota-refused)))
            (sb-thread:signal-semaphore done)))))
     (unless (sb-thread:wait-on-semaphore entered :timeout 30)
       (error \"Second allocator never started\"))
     (when (sb-thread:wait-on-semaphore done :timeout 0.1)
       (error \"Second allocation bypassed the paused quota scan\")))"
                            (test-conversation--child-configuration-form configuration)))
                 (declare (ignore rendered))
                 (setf second-worker worker))
               (evaluate first-worker
                         "(progn
    (sb-thread:signal-semaphore (getf *shell-log-contention-fixture* :resume))
    (unless (sb-thread:wait-on-semaphore (getf *shell-log-contention-fixture* :done) :timeout 30)
      (error \"First allocation did not publish\"))
    (unless (typep (getf *shell-log-contention-fixture* :result) 'shell-log-artifact)
      (error \"First allocation failed: ~S\" (getf *shell-log-contention-fixture* :result))))")
               (evaluate second-worker
                         "(progn
    (unless (sb-thread:wait-on-semaphore (getf *shell-log-contention-fixture* :done) :timeout 30)
      (error \"Second allocation did not finish\"))
    (unless (eq :quota-refused (getf *shell-log-contention-fixture* :result))
      (error \"Concurrent allocators overcommitted the single reservation\")))"))
          (when second-worker (lisp-worker-stop second-worker))
          (when first-worker (lisp-worker-stop first-worker))))))
  nil)


;;;; -- Durable Completion Receipt Regressions --

(-> shell-log-tests--receipt-append (conversation string) t)
(defun shell-log-tests--receipt-append (conversation identifier)
  "Persist a completion identifier through the production message boundary."
  (conversation-append-user-message conversation "Completed."
                                    :pending-input-identifier identifier :automatic-p t))

(-> shell-log-tests--receipt-capture (configuration task-orchestrator conversation &key (:root string)) shell-log-artifact)
(defun shell-log-tests--receipt-capture (configuration orchestrator conversation &key root)
  "Publish one detached closed capture owned by CONVERSATION."
  (let* ((job     (task-tests--make-job orchestrator :identifier "receipt-capture"
                                                  :root-identifier root :detached-p t))
         (context (make-instance 'tool-context :configuration configuration :worker nil
                                             :conversation conversation)))
    (setf (slot-value job 'completion-owner-conversation) (conversation-identifier conversation))
    (let ((artifact (shell-log-allocate context :job job)))
      (shell-log-tests--finish artifact #(65))
      artifact)))

(-> shell-log-tests--receipt-prunable (configuration shell-log-artifact) boolean)
(defun shell-log-tests--receipt-prunable (configuration artifact)
  "Query the production delivery disjunction for one closed fixture capture."
  (shell-log--prunable-p configuration (shell-log-capture-directory artifact)
                        (shell-log-artifact-metadata artifact)))

(-> test-shell-log-receipt-owners () null)
(defun test-shell-log-receipt-owners ()
  "Recognize late root, child and nested-child receipts, never foreign authority."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (root                           (conversation-create configuration :identifier "receipt-root"))
           (root-agent                     (make-instance 'agent :configuration configuration :conversation root))
           (foreign                        (conversation-create configuration :identifier "receipt-foreign"))
           (parent                         root-agent))
      (unwind-protect
           (dotimes (depth 3)
             (let* ((job          (and (plusp depth)
                                       (task-tests--make-job runtime :identifier "receipt-child"
                                         :root-identifier "receipt-root" :parent-agent parent)))
                    (owner        (if job (task-job-execution-identifier job) "receipt-root"))
                    (conversation (if job
                                      (conversation-create configuration :identifier owner
                                        :storage-root                                (task--artifact-root configuration job)) root))
                    (artifact     (shell-log-tests--receipt-capture configuration runtime conversation :root "receipt-root"))
                    (receipt      (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "unknown completion stays protected")
               (shell-log-tests--receipt-append foreign receipt)
               (when job
                 (shell-log-tests--receipt-append (conversation-create configuration :identifier owner) receipt))
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "foreign transcript including same-ID primary is not authority")
               (shell-log-tests--receipt-append conversation "completion:wrong-execution")
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "wrong execution receipt is not delivery")
               (shell-log-tests--receipt-append conversation receipt)
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "exact late receipt permits closed capture pruning at every depth")
               (setf (getf (shell-log-artifact-metadata artifact) :state) :active)
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "delivery never overrides active state")
               (when job
                 (setf parent (make-instance 'task-child-agent :configuration configuration
                                            :conversation conversation :job job :depth depth)))))
        (task-orchestrator-close runtime))))
  nil)

(-> shell-log-tests--newest-receipts (pathname) list)
(defun shell-log-tests--newest-receipts (identity)
  "Independently verify the supported newest-segment persistence invariant."
  (let* ((active   (conversation-storage-active-pathname identity))
         (header   (conversation-peek-header active))
         (receipts (copy-list (getf (rest header) :pending-input-identifiers))))
    (conversation--from-header identity active header)
    (conversation--map-segment-records
     identity active
     (lambda (record)
       (when (eq (first record) :message)
         (let ((receipt (getf (rest record) :pending-input-identifier)))
           (when receipt (pushnew receipt receipts :test #'equal))))))
    receipts))

(-> test-shell-log-receipt-rotation () null)
(defun test-shell-log-receipt-rotation ()
  "Execute two real rotations with reloads for modern and genuine v1 storage."
  (with-test-configuration (configuration)
    (dolist (legacy-p '(nil t))
      (let* ((conversation (conversation-create configuration :identifier (if legacy-p "legacy" "modern")))
             (identity     (conversation-pathname conversation))
             (receipts     '("completion:before" "completion:middle" "completion:after")))
        (shell-log-tests--receipt-append conversation (first receipts))
        (when legacy-p
          (let ((forms (copy-tree (conversation--read-records (conversation-log-pathname conversation)))))
            (setf (getf (rest (first forms)) :version) 1)
            (remf (rest (first forms)) :pending-input-identifiers)
            (conversation-identifier-migration--write-forms identity forms)
            (platform-delete-directory-tree *platform* (conversation-storage-directory-pathname identity)
                                            :validate t :if-does-not-exist :ignore))
          (setf conversation (conversation-load identity))
          (test-assert (member (first receipts) (conversation-pending-input-identifiers conversation) :test #'equal)
                       "v1 reload accumulates receipts from messages")
          (test-assert (member (first receipts) (shell-log-tests--newest-receipts identity) :test #'equal)
                       "v1 identity segment contains accumulated message receipt"))
        (loop for receipt in (rest receipts)
              do (let ((old (conversation-storage-active-pathname identity)))
                   (conversation-append-summary conversation "Real checkpoint")
                   (test-assert (not (equal old (conversation-storage-active-pathname identity)))
                                "production checkpoint rotates the physical segment")
                   (test-assert
                    (every (lambda (id) (member id (getf (rest (conversation-peek-header identity))
                                                        :pending-input-identifiers) :test #'equal))
                           (conversation-pending-input-identifiers conversation))
                    "new checkpoint header restates every accumulated receipt")
                   (setf conversation (conversation-load identity))
                   (shell-log-tests--receipt-append conversation receipt)))
        (test-assert (not (member (third receipts) (getf (rest (conversation-peek-header identity))
                                                       :pending-input-identifiers) :test #'equal))
                     "late receipt is deliberately absent from the header")
        (test-assert (equal (sort (copy-list (conversation-pending-input-identifiers (conversation-load identity))) #'string<)
                            (sort (shell-log-tests--newest-receipts identity) #'string<))
                     "validated newest segment and full replay agree after two rotations"))))
  nil)

(-> test-shell-log-receipt-validation () null)
(defun test-shell-log-receipt-validation ()
  "Validate the whole segment before publishing any receipt, including its tail."
  (with-test-configuration (configuration)
    (let ((*shell-log-capture-byte-limit* 32)
          (runtime                        (task-orchestrator-create)))
      (unwind-protect
           (dolist (kind '(:valid :header :v1 :torn :malformed :gap :checkpoint :identity :version :missing :owner :traversal))
             (let* ((conversation (conversation-create configuration :identifier (string-downcase (symbol-name kind))))
                    (artifact     (shell-log-tests--receipt-capture configuration runtime conversation
                                   :root                                                                (conversation-identifier conversation)))
                    (receipt      (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (shell-log-tests--receipt-append conversation receipt)
               (when (eq kind :header) (conversation-append-summary conversation "Header receipt"))
               (let* ((path  (conversation-storage-active-pathname (conversation-pathname conversation)))
                      (forms (copy-tree (conversation--read-records path))))
                 (case kind
                   (:v1
                    (setf (getf (rest (first forms)) :version) 1)
                    (remf (rest (first forms)) :pending-input-identifiers)
                    (conversation-identifier-migration--write-forms (conversation-pathname conversation) forms)
                    (platform-delete-directory-tree *platform*
                      (conversation-storage-directory-pathname (conversation-pathname conversation))
                      :validate t :if-does-not-exist ':ignore))
                   (:owner
                    (setf (getf (shell-log-artifact-metadata artifact) :completion-owner-conversation) nil))
                   (:traversal
                    (setf (getf (shell-log-artifact-metadata artifact) :completion-owner-conversation) "../foreign"))
                   (:torn
                    (with-open-file (s path :direction :output :if-exists :append)
                      (write-string "(:message :seq " s)))
                   (:malformed
                    (with-open-file (s path :direction :output :if-exists :append)
                      (write '(:message :seq 2 :role) :stream s) (terpri s)))
                   (:gap
                    (incf (getf (rest (second forms)) :seq))
                    (conversation-identifier-migration--write-forms path forms))
                   (:checkpoint
                    (conversation-append-summary conversation "Checkpoint")
                    (let* ((active  (conversation-storage-active-pathname (conversation-pathname conversation)))
                           (rotated (conversation--read-records active)))
                      (conversation-identifier-migration--write-forms active (list (first rotated)))))
                   (:identity
                    (setf (getf (rest (first forms)) :id) "another")
                    (conversation-identifier-migration--write-forms path forms))
                   (:version
                    (setf (getf (rest (first forms)) :version) 999)
                    (conversation-identifier-migration--write-forms path forms))
                   (:missing
                    (platform-delete-file *platform* path)))
                 (test-assert (eq (not (null (shell-log-tests--receipt-prunable configuration artifact)))
                                  (not (null (member kind '(:valid :header :v1 :torn)))))
                              (format nil "whole-segment receipt validation: ~A" kind)))))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-pressure () null)
(defun test-shell-log-receipt-pressure ()
  "Prune verified root/child receipts under pressure, retaining an unknown capture."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-count-limit* 3)
           (runtime                           (task-orchestrator-create))
           (root                              (conversation-create configuration :identifier "pressure-root"))
           (child-job                         (task-tests--make-job runtime :identifier "pressure-child" :root-identifier "pressure-root"))
           (child                             (conversation-create configuration :identifier (task-job-execution-identifier child-job)
                                                                   :storage-root             (task--artifact-root configuration child-job))))
      (unwind-protect
           (let* ((unknown (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root"))
                  (first   (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root"))
                  (second  (shell-log-tests--receipt-capture configuration runtime child :root "pressure-root")))
             (dolist (pair (list (cons first root) (cons second child)))
               (shell-log-tests--receipt-append (rest pair)
                 (task-completion--receipt (getf (shell-log-artifact-metadata (first pair)) :execution-id))))
             (test-assert
              (handler-case (progn (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root") t)
                (shell-log-error () nil))
              "allocation under count pressure reclaims receipt-backed closed captures")
             (test-assert (eq :closed (getf (shell-log--read-manifest configuration
                                             (shell-log-capture-directory unknown)) :state))
                          "pressure preserves genuinely unknown evidence"))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-migration () null)
(defun test-shell-log-receipt-migration ()
  "Resolve a real migrated root alias through its authoritative canonical identity."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (old                            "cb472f21-969d-48f5-9c1e-e793d19054b9")
           (conversation                   (conversation-create configuration :identifier old)))
      (unwind-protect
           (let* ((artifact (shell-log-tests--receipt-capture configuration runtime conversation :root old))
                  (receipt  (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
             (shell-log-tests--receipt-append conversation receipt)
             (let* ((entries   (conversation-identifier-migrate configuration))
                    (entry     (conversation-identifier-migration--entry-for-old old entries))
                    (canonical (getf entry :new)))
               (test-assert (and canonical (not (equal old canonical))) "real migration publishes a new root identifier")
               (test-assert (equal canonical (pathname-name (conversation-pathname-for-id configuration old)))
                            "authoritative root lookup resolves the old alias")
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "old capture metadata recognizes receipt in migrated root")))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-cache () null)
(defun test-shell-log-receipt-cache ()
  "A new prune sees appended receipts after an earlier conservative cache miss."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-count-limit* 1)
           (runtime                           (task-orchestrator-create))
           (conversation                      (conversation-create configuration :identifier "cached-root")))
      (unwind-protect
           (let* ((artifact                  (shell-log-tests--receipt-capture configuration runtime conversation :root "cached-root"))
                  (receipt                   (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id)))
                  (*shell-log-receipt-cache* (make-hash-table :test #'equal)))
             (declare (special *shell-log-receipt-cache*))
             (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                          "first lookup has no verified evidence")
             (shell-log-tests--receipt-append conversation receipt)
             (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                          "one prune-local snapshot may conservatively retain its negative result")
             (let ((*shell-log-receipt-cache* nil))
               (declare (special *shell-log-receipt-cache*))
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "standalone lookup reads fresh delivery evidence"))
             (test-assert (handler-case (progn (shell-log-tests--receipt-capture configuration runtime conversation :root "cached-root") t)
                            (shell-log-error () nil))
                          "new prune overrides a stale caller cache and admits replacement allocation"))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-linked-segment () null)
(defun test-shell-log-receipt-linked-segment ()
  "Reject linked transcript segments even if the target contains exact receipts."
  (with-test-configuration (configuration)
    (with-test-fixture (':symbolic-links "completion receipt segment substitution")
      (let* ((*shell-log-capture-byte-limit* 32)
             (runtime                        (task-orchestrator-create))
             (conversation                   (conversation-create configuration :identifier "linked-root")))
        (unwind-protect
             (let* ((artifact (shell-log-tests--receipt-capture configuration runtime conversation :root "linked-root"))
                    (receipt  (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (shell-log-tests--receipt-append conversation receipt)
               (conversation-append-summary conversation "Header witness")
               (let* ((active (conversation-storage-active-pathname (conversation-pathname conversation)))
                      (target (merge-pathnames "foreign-segment.sexp" (config :data-root configuration))))
                 (uiop:copy-file active target)
                 (platform-delete-file *platform* active)
                 (test-fixture-make-symbolic-link *platform* (namestring target) (namestring active))
                 (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                              "linked receipt segment cannot authorize pruning")))
          (task-orchestrator-close runtime)))))
  nil)

(-> test-shell-log-receipt-large-history () null)
(defun test-shell-log-receipt-large-history ()
  "Resolve header and late receipts amid a large validated owner history."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (conversation                   (conversation-create configuration :identifier "large-receipts")))
      (unwind-protect
           (let* ((early   (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (late    (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (unknown (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (receipt (task-completion--receipt (getf (shell-log-artifact-metadata early) :execution-id))))
             (shell-log-tests--receipt-append conversation receipt)
             (let* ((path   (conversation-log-pathname conversation))
                    (forms  (conversation--read-records path))
                    (header (first forms))
                    (seed   (second forms)))
               (setf (getf (rest header) :pending-input-identifiers)
                     (cons receipt (loop for index below 15000 collect (format nil "completion:header-~D" index))))
               (setf forms
                     (cons header
                           (loop for sequence from 1 to 15000
                                 for record = (copy-tree seed)
                                 do (setf (getf (rest record) :seq) sequence
                                          (getf (rest record) :pending-input-identifier)
                                          (if (= sequence 15000)
                                              (task-completion--receipt (getf (shell-log-artifact-metadata late) :execution-id))
                                              (format nil "completion:message-~D" sequence)))
                                 collect record)))
               (conversation-identifier-migration--write-forms path forms)
               (let ((*shell-log-receipt-cache* (make-hash-table :test #'equal)))
                 (test-assert (shell-log-tests--receipt-prunable configuration early)
                              "A large checkpoint header proves exact earlier delivery")
                 (test-assert (shell-log-tests--receipt-prunable configuration late)
                              "A late receipt after thousands of messages proves delivery")
                 (test-assert (not (shell-log-tests--receipt-prunable configuration unknown))
                              "Large receipt sets do not acknowledge unknown executions"))
               (push receipt (getf (rest header) :pending-input-identifiers))
               (conversation-identifier-migration--write-forms path forms)
               (test-assert (not (shell-log-tests--receipt-prunable configuration early))
                            "Duplicate checkpoint identifiers invalidate all receipt evidence")))
        (task-orchestrator-close runtime))))
  nil)

(-> shell-log-tests--sync-details-case
    (string &key (:path keyword) (:shape keyword) (:mismatch (option keyword))
     (:status keyword) (:child-p boolean)) null)
(defun shell-log-tests--sync-details-case (name &key (path ':tool) (shape ':tagged) mismatch
                                                    (status ':ok) child-p)
  "Persist incompatible details before an exact receipt, checking eligibility only."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (root                           "sync-details-root")
           (owner                          (if child-p "sync-details-child" root))
           (conversation                   (conversation-create configuration :identifier owner
                                             :storage-root                                (and child-p
                                                                                           (merge-pathnames "sync-details-child/"
                                                                                             (task--artifact-group-root configuration root)))))
           (context                        (make-instance 'tool-context :configuration configuration
                                             :conversation conversation :worker nil))
           (runtime                        (task-orchestrator-create))
           (job                            (task-tests--make-job runtime :identifier "details-command"
                                             :root-identifier root :detached-p nil)))
      (unwind-protect
           (progn
             (setf (slot-value job 'completion-owner-conversation) owner
                   (slot-value job 'parent-call-id) (and (eq path ':tool) "details-call"))
             (let* ((artifact (shell-log-allocate context :job job))
                    (metadata (shell-log-tests--finish artifact #(65)))
                    (receipt  (copy-list (shell-log--public-metadata metadata)))
                    (details  (case shape
                                (:none
                                 nil)
                                (:empty
                                 nil)
                                (:unrelated
                                 '(:offset 0))
                                (:tagged
                                 '(:task-agents :offset 0))
                                (:odd
                                 (list :shell-logs (list receipt) :bad-tail))
                                (:atom
                                 17)
                                (:dotted
                                 (list* :shell-logs (list receipt) 17))
                                (:non-keyword
                                 '(17 23))
                                (:duplicate
                                 (list :shell-logs nil :shell-logs (list receipt)))))
                    (target   (if (eq mismatch ':owner)
                                  (conversation-create configuration :identifier "other-owner")
                                  conversation)))
               (labels ((check-state (expected phase)
                          (test-assert (eq expected (shell-log--sync-delivered-p configuration metadata))
                                       (format nil "~A ~A delivery" name phase))
                          (test-assert (eq expected (shell-log--prunable-p configuration
                                                     (shell-log-capture-directory artifact) metadata))
                                       (format nil "~A ~A eligibility" name phase)))

                        (gate (value gate-status)
                          (list :status gate-status :tool-details value))

                        (append-details (value)
                          (conversation-append-record conversation
                            (if (eq path ':tool)
                                (list :tool-result :call-id "details-call" :status ':ok :details value)
                                (list :goal :gates (list (gate value ':passed)))))))
                 (unless (eq shape ':none)
                   (append-details details)
                   (check-state nil "prefix alone"))
                 (case mismatch
                   (:execution
                    (setf (getf receipt :execution-id) "other-execution"))
                   (:artifact
                    (setf (getf receipt :artifact-id) "other-artifact"))
                   (:log-active
                    (setf (getf receipt :state) ':active))
                   (:active
                    (setf (getf metadata :state) ':active))
                   (:detached
                    (setf (getf metadata :detached-p) t))
                   (:goal-call
                    (setf (getf metadata :parent-call-id) "details-call")))
                 (unless (eq mismatch ':no-receipt)
                   (conversation-append-record target
                     (if (eq path ':tool)
                         (list :tool-result :call-id (if (eq mismatch ':call) "other-call" "details-call")
                               :status status :details (list :shell-logs (list receipt)))
                         (list :goal :gates
                               (append (when (eq path ':gates) (list (gate details ':passed)))
                                       (list (gate (list :shell-logs (list receipt)) status)))))))
                 (when (eq mismatch ':header)
                   (let* ((file  (conversation-storage-active-pathname (conversation-pathname conversation)))
                          (forms (copy-tree (conversation--read-records file))))
                     (setf (getf (rest (first forms)) :id) "other-header")
                     (conversation-identifier-migration--write-forms file forms)))
                 (check-state (null mismatch) "after receipt")
                 (unless (eq mismatch ':header)
                   (conversation-append-summary conversation "Details checkpoint")
                   (unless (eq target conversation)
                     (conversation-append-summary target "Other owner checkpoint"))
                   (conversation-load (conversation-pathname conversation))
                   (check-state (null mismatch) "after rotation and reload")))))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-sync-details-shapes () null)
(defun test-shell-log-sync-details-shapes ()
  "Skip incompatible stored details without acknowledging them or losing later receipts."
  (dolist (path '(:tool :goal :gates))
    (dolist (shape '(:empty :unrelated :tagged :odd :atom :dotted :non-keyword :duplicate))
      (shell-log-tests--sync-details-case (format nil "~A/~A" path shape)
                                         :path path :shape shape
                                         :status (if (eq path ':tool) ':ok ':passed))))
  (dolist (row '((:tool :ok) (:tool :error) (:goal :passed) (:goal :failed)))
    (destructuring-bind (path status) row
      (shell-log-tests--sync-details-case (format nil "terminal/~A/~A" path status)
                                         :path path :shape ':none :status status)))
  nil)

(-> test-shell-log-sync-details-identities () null)
(defun test-shell-log-sync-details-identities ()
  "Keep exact identity and lifecycle protection after skipping tagged details."
  (dolist (child-p '(nil t))
    (dolist (path '(:tool :goal))
      (shell-log-tests--sync-details-case (format nil "~A/~A/exact" child-p path)
                                         :path path :child-p child-p
                                         :status (if (eq path ':tool) ':ok ':passed))
      (dolist (mismatch '(:no-receipt :owner :header :execution :artifact :log-active :active :detached))
        (shell-log-tests--sync-details-case (format nil "~A/~A/~A" child-p path mismatch)
                                           :path path :mismatch mismatch :child-p child-p
                                           :status (if (eq path ':tool) ':ok ':passed)))
      (shell-log-tests--sync-details-case (format nil "~A/~A/nonterminal" child-p path)
                                         :path path :mismatch ':status :status ':running :child-p child-p))
    (shell-log-tests--sync-details-case "wrong call" :mismatch ':call :child-p child-p)
    (shell-log-tests--sync-details-case "goal cannot acknowledge call" :path ':goal :status ':passed
                                       :mismatch ':goal-call :child-p child-p))
  nil)


;;;; -- Delivery Authority Across Child Result Retention --

(-> shell-log-tests--disk-prunable (configuration shell-log-artifact) boolean)
(defun shell-log-tests--disk-prunable (configuration artifact)
  "Query fresh persisted metadata rather than the allocation's stale heap copy."
  (let ((directory (shell-log-capture-directory artifact)))
    (shell-log--prunable-p configuration directory
                          (shell-log--read-manifest configuration directory :measure-p nil))))

(-> shell-log-tests--ordered-artifacts (list list) boolean)
(defun shell-log-tests--ordered-artifacts (left right)
  "Give fixture result publications deterministic retention order without sleeps."
  (string< (namestring (first left)) (namestring (first right))))

(-> shell-log-tests--owner-retention
    (function &key (:backfill-p boolean) (:around-retention (option function))) null)
(defun shell-log-tests--owner-retention (check &key backfill-p around-retention)
  "Publish real child results until retention removes delivered and silent owners."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*              (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*  32)
           (*task-artifact-retention-limit* 2)
           (runtime                         (task-orchestrator-create))
           (root                            (conversation-create configuration :identifier "retention-root"))
           (parent                          (make-instance 'agent :configuration configuration :conversation root))
           (jobs                            nil)
           (captures                        nil))
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'task--artifact-record< #'shell-log-tests--ordered-artifacts))
            (lambda ()
              (labels ((child (identifier)
                         (let ((job (task-tests--make-job runtime :identifier identifier
                                      :root-identifier "retention-root" :parent-agent parent)))
                           (setf (slot-value job 'execution-identifier) identifier)
                           job)))
                (dolist (identifier '("000Owner" "001Control"))
                  (let* ((job     (child identifier))
                         (owner   (conversation-create configuration :identifier identifier
                                    :storage-root                                (task--artifact-root configuration job)))
                         (context (make-instance 'tool-context :configuration configuration
                                    :worker nil :conversation owner)))
                    (conversation-append-user-message owner "Capture fixture output.")
                    (push job jobs)
                    (dolist (detached (if (equal identifier "000Owner") '(nil t) '(nil)))
                      (let ((command (task-tests--make-job runtime :identifier "command"
                                       :root-identifier "retention-root" :detached-p detached)))
                        (setf (slot-value command 'completion-owner-conversation) identifier
                              (slot-value command 'parent-call-id) "retention-call")
                        (let ((artifact (shell-log-allocate context :job command)))
                          (shell-log-tests--finish artifact #(65))
                          (unless (equal identifier "000Owner")
                            (setf (getf (shell-log-artifact-metadata artifact) :created-at)
                                  (- (get-universal-time) *shell-log-retention-age* 1))
                            (shell-log--write-manifest artifact))
                          (when (equal identifier "000Owner")
                            (if detached
                                (shell-log-tests--receipt-append owner
                                  (task-completion--receipt (session-job-execution-identifier command)))
                                (conversation-append-record owner
                                  (list :tool-result :call-id "retention-call" :status :ok
                                        :details (list :shell-logs
                                                       (list (shell-log--public-metadata
                                                              (shell-log-artifact-metadata artifact))))))))
                          (push artifact captures))))
                    (conversation-append-summary owner "Owner checkpoint")
                    (conversation-load (conversation-pathname owner))))
                (setf captures (nreverse captures))
                (test-assert (every (lambda (capture)
                                      (shell-log-tests--disk-prunable configuration capture))
                                    (subseq captures 0 2))
                             "Both exact durable delivery paths authorize pruning before retention")
                (test-assert (not (shell-log-tests--disk-prunable configuration (third captures)))
                             "Undelivered control has no authority before retention")
                (when backfill-p
                  (dolist (job jobs)
                    (let* ((owner    (session-job-execution-identifier job))
                           (identity (shell-log--owner-identity configuration "retention-root" owner))
                           (receipts (merge-pathnames "shell-delivery/"
                                      (conversation-storage-directory-pathname identity))))
                      (platform-delete-directory-tree *platform* receipts
                                                      :validate t :if-does-not-exist ':ignore)))
                  (test-assert (not (shell-log-tests--disk-prunable configuration (first captures)))
                               "Pre-index rotated child evidence needs backfill before retention"))
                (flet ((retain ()
                         (dolist (job (reverse jobs))
                           (task--write-result-artifact job (list :status ':success :output "done")))
                         (dotimes (index 3)
                           (task--write-result-artifact (child (format nil "100Later~D" index))
                                                       (list :status ':success :output "later")))))
                  (if around-retention
                      (funcall around-retention configuration root #'retain)
                      (retain)))
                (dolist (job jobs)
                  (test-assert (not (probe-file (task--artifact-root configuration job)))
                               "Actual result retention removed each original owner"))
                (test-assert (= 2 (length (task--completed-artifact-records
                                           (task--artifact-group-root configuration "retention-root"))))
                             "Real publication enforces the fixture retention limit")
                (test-assert (every (lambda (capture)
                                      (probe-file (merge-pathnames "output.bin"
                                                    (shell-log-capture-directory capture)))) captures)
                             "Separate capture directories survive owner artifact retention")
                (funcall check configuration root captures))))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-owner-retention () null)
(defun test-shell-log-owner-retention ()
  "Keep synchronous and detached positive delivery authority after owner deletion."
  (shell-log-tests--owner-retention
   (lambda (configuration root captures)
     (declare (ignore root))
     (dolist (capture (subseq captures 0 2))
       (test-assert (shell-log-tests--disk-prunable configuration capture)
                    "Delivered capture keeps pruning authority after normal owner retention"))
     (test-assert (not (shell-log-tests--disk-prunable configuration (third captures)))
                  "Missing owner alone never grants authority")))
  nil)

(-> test-shell-log-owner-retention-pressure () null)
(defun test-shell-log-owner-retention-pressure ()
  "Exercise age, count and bytes independently after causal owner retention."
  (dolist (pressure '(:age :count :bytes))
    (shell-log-tests--owner-retention
     (lambda (configuration root captures)
       (let ((*shell-log-retention-age*         (if (eq pressure :age) 1 most-positive-fixnum))
             (*shell-log-retention-count-limit* (if (eq pressure :count) 2 256))
             (*shell-log-retention-byte-limit*  (if (eq pressure :bytes) 33 (* 1024 1024))))
         (when (eq pressure :age)
           (dolist (capture captures)
             (let ((metadata (shell-log--read-manifest configuration (shell-log-capture-directory capture))))
               (setf (getf metadata :created-at) (- (get-universal-time) 1000))
               (shell-log--write-manifest
                (make-instance 'shell-log-artifact :configuration configuration
                               :capture-directory (shell-log-capture-directory capture) :metadata metadata)))))
         (let ((replacement
                 (handler-case
                     (shell-log-allocate (make-instance 'tool-context :configuration configuration
                                                       :worker nil :conversation root))
                   (shell-log-error () nil))))
           (unwind-protect
                (progn
                  (test-assert replacement "Delivered logs yield space under independent pressure")
                  (dolist (capture (subseq captures 0 2))
                    (test-assert (not (probe-file (merge-pathnames "output.bin"
                                                   (shell-log-capture-directory capture))))
                                 "Pressure reclaims both delivered files"))
                  (test-assert (probe-file (merge-pathnames "output.bin"
                                            (shell-log-capture-directory (third captures))))
                               "Undelivered control survives every pressure policy"))
             (when replacement (shell-log-release-artifact replacement))))))))
  nil)

(-> shell-log-tests--owner-fixture (function) null)
(defun shell-log-tests--owner-fixture (function)
  "Provide a closed detached capture with exact durable child delivery evidence."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (root                           (conversation-create configuration :identifier "mark-root"))
           (parent                         (make-instance 'agent :configuration configuration :conversation root))
           (job                            (task-tests--make-job runtime :identifier "owner"
                                               :root-identifier "mark-root" :parent-agent parent)))
      (setf (slot-value job 'execution-identifier) "000Owner")
      (unwind-protect
           (let* ((owner    (conversation-create configuration :identifier "000Owner"
                              :storage-root                                (task--artifact-root configuration job)))
                  (artifact (shell-log-tests--receipt-capture configuration runtime owner :root "mark-root")))
             (shell-log-tests--receipt-append owner
               (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id)))
             (funcall function configuration runtime parent job owner artifact))
        (task-orchestrator-close runtime))))
  nil)

(-> shell-log-tests--marked-p (configuration shell-log-artifact) boolean)
(defun shell-log-tests--marked-p (configuration artifact)
  "Read the capture's durable delivery mark without measuring or rewriting it."
  (shell-log--delivery-recorded-p
   (shell-log--read-manifest configuration (shell-log-capture-directory artifact) :measure-p nil)))

(-> test-shell-log-owner-delivery-persistence () null)
(defun test-shell-log-owner-delivery-persistence ()
  "Validate positive marks, exact old-reader data compatibility and idempotent publication."
  (dolist (value '(nil 0 -1 1.5 "1" :delivered))
    (test-assert (not (shell-log--delivery-recorded-p (list :delivered-at value)))
                 "Only a positive integer records delivery authority"))
  (test-assert (shell-log--delivery-recorded-p (list :delivered-at 1))
               "Positive integral time records authority")
  (shell-log-tests--owner-fixture
   (lambda (configuration runtime parent job owner artifact)
     (declare (ignore runtime parent job owner))
     (let ((writes    0)
           (original  (symbol-function 'shell-log--write-manifest))
           (directory (shell-log-capture-directory artifact)))
       (test-call-with-function-replacements
        (list (list 'shell-log--write-manifest
                    (lambda (capture) (incf writes) (funcall original capture))))
        (lambda ()
          (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
          (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")))
       (test-assert (= writes 1) "Retry of an already marked capture performs no publication")
       (test-assert (shell-log-tests--marked-p configuration artifact) "Mark is durable on disk")
       ;; TASK-CONTINUITY--READ is the unchanged bounded reader used at the base.
       (let ((read-back (task-continuity--read (shell-log--manifest-path directory))))
         (test-assert (shell-log--delivery-recorded-p read-back)
                      "The unmodified old bounded plist reader accepts the additive field")
         (test-assert (not (getf (shell-log--public-metadata read-back) :delivered-at))
                      "Delivery authority is private metadata")
         (test-call-with-function-replacements
          (list (list 'shell-log--delivered-p
                      (lambda (&rest arguments) (declare (ignore arguments))
                        (error "Recorded delivery must not reread the owner."))))
          (lambda ()
            (test-assert (shell-log-tests--disk-prunable configuration artifact)
                         "Persisted authority is sufficient without owner lookup")))
         (setf (getf read-back :state) :active)
         (test-assert (not (shell-log--prunable-p configuration directory read-back))
                      "Recorded delivery never overrides active state")))))
  nil)

(-> test-shell-log-owner-delivery-failures () null)
(defun test-shell-log-owner-delivery-failures ()
  "Keep last owner receipts on hook failure, then retry ordinary retention safely."
  (dolist (failure '(:write :read :enumeration :path :size :delete))
    (shell-log-tests--owner-fixture
     (lambda (configuration runtime parent job owner artifact)
       (declare (ignore owner))
       (let* ((*task-artifact-retention-limit* 1)
              (owner-root                      (task--artifact-root configuration job))
              (later                           (task-tests--make-job runtime :identifier "later" :parent-agent parent
                                                 :root-identifier "mark-root"))
              (original-delete                 (symbol-function 'platform-delete-directory-tree))
              (original-write                  (symbol-function 'shell-log--write-manifest))
              (original-path                   (symbol-function 'shell-log--safe-path))
              (target                          (case failure
                                                 (:write
                                                  'shell-log--write-manifest)
                                                 (:read
                                                  'shell-log--read-manifest)
                                                 (:enumeration
                                                  'shell-log--group-directories)
                                                 (:path
                                                  'shell-log--safe-path)
                                                 (:size
                                                  'shell-log--write-manifest)
                                                 (:delete
                                                  'platform-delete-directory-tree))))
         (setf (slot-value later 'execution-identifier) "100Later")
         (task--write-result-artifact job (list :status :success :output "owner done"))
         (test-call-with-function-replacements
          (list (list 'task--artifact-record< #'shell-log-tests--ordered-artifacts)
                (list target
                  (lambda (&rest arguments)
                    (case failure
                      (:delete
                       (if (uiop:pathname-equal (second arguments) owner-root)
                           (error "Injected owner deletion failure.")
                           (apply original-delete arguments)))
                      (:size
                       (let ((*shell-log-manifest-byte-limit* 1))
                         (apply original-write arguments)))
                      (:path
                       (if (equal (second arguments) (shell-log--manifest-path
                                                      (shell-log-capture-directory artifact)))
                           (error "Injected unsafe manifest path.")
                           (apply original-path arguments)))
                      (t
                       (error "Injected pre-delete ~A failure." failure))))))
          (lambda ()
            (task--write-result-artifact later (list :status :success :output "later done"))))
         (test-assert (probe-file owner-root) "Failed handoff must retain the last durable owner receipt")
         (test-assert (shell-log-tests--disk-prunable configuration artifact)
                      "Failure never destroys existing positive delivery authority")
         (test-assert (eq (eq failure :delete) (shell-log-tests--marked-p configuration artifact))
                      "Only post-mark deletion failure leaves a positive mark")
         (let ((retry (task-tests--make-job runtime :identifier "retry" :parent-agent parent
                        :root-identifier "mark-root")))
           (setf (slot-value retry 'execution-identifier) "200Retry")
           (test-call-with-function-replacements
            (list (list 'task--artifact-record< #'shell-log-tests--ordered-artifacts))
            (lambda () (task--write-result-artifact retry (list :status :success :output "retry")))))
         (test-assert (not (probe-file owner-root)) "A later publication retries and completes retention")
         (test-assert (shell-log-tests--marked-p configuration artifact)
                      "Successful retry preserves authority outside the deleted owner")))))
  nil)

(-> test-shell-log-owner-delivery-boundaries () null)
(defun test-shell-log-owner-delivery-boundaries ()
  "Require exact child or nested-child authority, excluding foreign and root storage."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*              (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*  32)
           (*task-artifact-retention-limit* 1)
           (runtime                         (task-orchestrator-create))
           (root                            (conversation-create configuration :identifier "boundary-root"))
           (root-agent                      (make-instance 'agent :configuration configuration :conversation root))
           (parent                          root-agent)
           (foreign                         (conversation-create configuration :identifier "foreign-root")))
      (unwind-protect
           (progn
             (let ((capture (shell-log-tests--receipt-capture configuration runtime root :root "boundary-root")))
               (shell-log-tests--receipt-append root
                 (task-completion--receipt (getf (shell-log-artifact-metadata capture) :execution-id)))
               (test-assert (handler-case
                                (progn (shell-log-persist-owner-delivery configuration
                                         "boundary-root" "boundary-root") nil)
                              (shell-log-error () t))
                            "Primary owner cannot be treated as a deleted child")
               (test-assert (shell-log-tests--disk-prunable configuration capture)
                            "Root capture still uses its ordinary durable delivery reader")
               (test-assert (not (shell-log-tests--marked-p configuration capture))
                            "Unrelated root authority is not eagerly marked"))
             (dotimes (depth 2)
               (let* ((job          (task-tests--make-job runtime :identifier "nested-owner"
                                     :root-identifier "boundary-root" :parent-agent parent))
                      (owner        (task-job-execution-identifier job))
                      (conversation (conversation-create configuration :identifier owner
                                      :storage-root                                (task--artifact-root configuration job)))
                      (capture      (shell-log-tests--receipt-capture configuration runtime conversation
                                      :root "boundary-root"))
                      (metadata     (shell-log-artifact-metadata capture))
                      (receipt      (task-completion--receipt (getf metadata :execution-id))))
                 (shell-log-tests--receipt-append foreign receipt)
                 (shell-log-tests--receipt-append
                  (conversation-create configuration :identifier owner) receipt)
                 (shell-log-tests--receipt-append conversation "completion:wrong")
                 (shell-log-persist-owner-delivery configuration "boundary-root" owner)
                 (test-assert (not (shell-log-tests--marked-p configuration capture))
                              "Foreign root, same-ID primary and wrong receipt cannot grant a mark")
                 (shell-log-tests--receipt-append conversation receipt)
                 (shell-log-persist-owner-delivery configuration "boundary-root" owner)
                 (test-assert (shell-log-tests--marked-p configuration capture)
                              "Exact durable child or nested-child receipt grants authority")
                 (task--write-result-artifact job (list :status :success :output "nested done"))
                 (setf parent (make-instance 'task-child-agent :configuration configuration
                                            :conversation conversation :job job :depth (1+ depth)))))
             ;; A root-parented publication makes both old children eligible, even the ancestor.
             (let ((later (task-tests--make-job runtime :identifier "later" :root-identifier "boundary-root"
                            :parent-agent root-agent)))
               (task--write-result-artifact later (list :status :success :output "later done")))
             (test-assert (= 1 (length (task--completed-artifact-records
                                        (task--artifact-group-root configuration "boundary-root"))))
                          "Real retention can evict nested owners after marking"))
        (task-orchestrator-close runtime))))
  (shell-log-tests--owner-fixture
   (lambda (configuration runtime parent job owner artifact)
     (declare (ignore runtime parent job owner))
     (dolist (key '(:root-conversation :execution-id :artifact-id))
       (let* ((original (copy-tree (shell-log-artifact-metadata artifact)))
              (bad      (copy-tree original)))
         (setf (getf bad key) "different")
         (shell-log--write-manifest
          (make-instance 'shell-log-artifact :configuration configuration
                         :capture-directory (shell-log-capture-directory artifact) :metadata bad))
         (handler-case (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
           (shell-log-error () nil))
         (test-assert (not (shell-log-tests--marked-p configuration artifact))
                      "Wrong group or physical execution/artifact identity cannot be marked")
         (shell-log--write-manifest
          (make-instance 'shell-log-artifact :configuration configuration
                         :capture-directory (shell-log-capture-directory artifact) :metadata original))))))
  nil)

(-> test-shell-log-owner-delivery-tagged-details () null)
(defun test-shell-log-owner-delivery-tagged-details ()
  "Persist exact owner delivery after skipping incompatible tagged details."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (owner                          (conversation-create configuration :identifier "tagged-details"
                                             :storage-root                                (merge-pathnames "tagged-details/"
                                                                                           (task--artifact-group-root configuration "tagged-details-root")))))
      (unwind-protect
           (let* ((artifact (shell-log-tests--receipt-capture configuration runtime owner :root "tagged-details-root"))
                  (metadata (shell-log-artifact-metadata artifact)))
             (setf (getf metadata :detached-p) nil (getf metadata :parent-call-id) nil)
             (shell-log--write-manifest artifact)
             (conversation-append-record owner
               (list :tool-result :status :ok :details '(:task-agents :offset 0)))
             (conversation-append-record owner
               (list :goal :gates (list (list :status  :passed
                                         :tool-details (list :shell-logs (list (shell-log--public-metadata metadata)))))))
             (test-assert (shell-log--sync-delivered-p configuration metadata)
                          "Tagged details do not hide a later exact synchronous receipt")
             (shell-log-persist-owner-delivery configuration "tagged-details-root" "tagged-details")
             (test-assert (shell-log-tests--marked-p configuration artifact)
                          "Exact synchronous delivery persists before owner retention"))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-owner-delivery-leases () null)
(defun test-shell-log-owner-delivery-leases ()
  "Preserve active local and foreign leases despite closed metadata and delivery."
  (shell-log-tests--owner-fixture
   (lambda (configuration runtime parent job owner artifact)
     (declare (ignore runtime parent job owner))
     (let* ((directory (shell-log-capture-directory artifact))
            (path      (merge-pathnames "owner.lock" directory))
            (lease     (lease-acquire path)))
       (unwind-protect
            (progn
              (setf (gethash (namestring directory) *shell-log-active*)
                    (cons (current-thread) lease))
              (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
              (test-assert (not (shell-log-tests--marked-p configuration artifact))
                           "A live local lease excludes even a closed delivered capture")
              (remhash (namestring directory) *shell-log-active*)
              (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
              (test-assert (not (shell-log-tests--marked-p configuration artifact))
                           "Kernel lease exclusion does not require a local heap pin"))
         (remhash (namestring directory) *shell-log-active*)
         (lease-release lease))
       (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
       (test-assert (shell-log-tests--marked-p configuration artifact)
                    "Released lease permits the positive handoff"))))
  nil)

(-> test-shell-log-owner-delivery-restart () null)
(defun test-shell-log-owner-delivery-restart ()
  "Reread the mark in a fresh process and exclude a real foreign capture lease."
  (shell-log-tests--owner-fixture
   (lambda (configuration runtime parent job owner artifact)
     (declare (ignore runtime parent job owner))
     (let ((worker    nil)
           (directory (shell-log-capture-directory artifact)))
       (unwind-protect
            (progn
              (multiple-value-bind (values child)
                  (test-conversation--worker-eval configuration
                    (format nil "(progn (defparameter *owner-retention-lease* (lease-acquire ~S)) t)"
                            (merge-pathnames "owner.lock" directory)))
                (setf worker child)
                (test-assert (equal values '("T")) "Fresh worker acquired the capture's real lease"))
              (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
              (test-assert (not (shell-log-tests--marked-p configuration artifact))
                           "A foreign process lease excludes the positive capture")
              (lisp-worker-stop worker)
              (setf worker nil)
              (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")
              (multiple-value-bind (values child)
                  (test-conversation--worker-eval configuration
                    (format nil "(let* ((configuration ~A) (directory ~S)
                               (metadata (shell-log--read-manifest configuration directory :measure-p nil)))
                         (and (shell-log--delivery-recorded-p metadata)
                              (shell-log--prunable-p configuration directory metadata)))"
                            (test-conversation--child-configuration-form configuration) directory))
                (setf worker child)
                (test-assert (equal values '("T")) "A fresh worker recognizes durable delivery authority")))
         (when worker (lisp-worker-stop worker))))))
  nil)

(-> test-shell-log-owner-delivery-concurrency () null)
(defun test-shell-log-owner-delivery-concurrency ()
  "Serialize ordinary retention handoff, a second marker and quota inspection."
  (shell-log-tests--owner-fixture
   (lambda (configuration runtime parent job owner artifact)
     (declare (ignore owner))
     (let* ((*task-artifact-retention-limit* 1)
            (later                           (task-tests--make-job runtime :identifier "later" :parent-agent parent
                                               :root-identifier "mark-root"))
            (entered                         (sb-thread:make-semaphore))
            (resume                          (sb-thread:make-semaphore))
            (started                         (sb-thread:make-semaphore))
            (done                            (sb-thread:make-semaphore))
            (active                          *shell-log-active*)
            (original                        (symbol-function 'shell-log--write-manifest))
            (writes                          0)
            (threads                         nil)
            (failures                        nil)
            (failure-lock                    (make-lock "retention fixture failures")))
       (setf (slot-value later 'execution-identifier) "100Later")
       (task--write-result-artifact job (list :status :success :output "done"))
       (test-call-with-function-replacements
        (list (list 'task--artifact-record< #'shell-log-tests--ordered-artifacts)
              (list 'shell-log--write-manifest
                (lambda (capture)
                  (incf writes)
                  (when (= writes 1)
                    (sb-thread:signal-semaphore entered)
                    (unless (sb-thread:wait-on-semaphore resume :timeout 10)
                      (error "Timed out waiting to publish the retention mark.")))
                  (funcall original capture))))
        (lambda ()
          (labels ((start (function)
                     (push (make-thread
                            (lambda ()
                              (let ((*shell-log-active*              active)
                                    (*task-artifact-retention-limit* 1))
                                (unwind-protect
                                     (handler-case (funcall function)
                                       (error (condition)
                                         (with-lock-held (failure-lock) (push condition failures))))
                                  (sb-thread:signal-semaphore done))))
                            :name "owner retention fixture") threads)))
            (unwind-protect
                 (progn
                   (start (lambda () (task--write-result-artifact later
                                       (list :status :success :output "later"))))
                   (unless (sb-thread:wait-on-semaphore entered :timeout 10)
                     (error "Retention did not reach mark publication."))
                   (start (lambda ()
                            (sb-thread:signal-semaphore started)
                            (shell-log-persist-owner-delivery configuration "mark-root" "000Owner")))
                   (start (lambda ()
                            (sb-thread:signal-semaphore started)
                            (with-shell-log-lock (configuration)
                              (let ((*shell-log-retention-age* most-positive-fixnum))
                                (shell-log--prune configuration)))))
                   (dotimes (index 2)
                     (unless (sb-thread:wait-on-semaphore started :timeout 10)
                       (error "Concurrent fixture did not start.")))
                   (test-assert (not (sb-thread:wait-on-semaphore done :timeout 0.1))
                                "Concurrent handoff and prune cannot bypass the paused publisher")
                   (sb-thread:signal-semaphore resume)
                   (dotimes (index 3)
                     (unless (sb-thread:wait-on-semaphore done :timeout 10)
                       (error "Retention interleaving failed to complete.")))
                   (test-assert (null failures) "Controlled concurrent operations complete without error")
                   (test-assert (= writes 1) "Concurrent retries publish one monotonic mark")
                   (test-assert (not (probe-file (task--artifact-root configuration job)))
                                "Actual retention removes the owner after successful publication")
                   (test-assert (shell-log-tests--marked-p configuration artifact)
                                "Serialized prune observes fresh durable authority"))
              (sb-thread:signal-semaphore resume)
              (dolist (thread threads)
                (sb-thread:join-thread thread :timeout 12 :default :timed-out)))))))))
  nil)


;;;; -- Resumable Historical Delivery Evidence --

(-> test-shell-log-delivery-backfill () null)
(defun test-shell-log-delivery-backfill ()
  "Recover pre-index rotated evidence in bounded, crash-safe batches."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                      (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*          32)
           (*shell-log-delivery-index-record-limit* 3)
           (context                                 (shell-log-tests--context configuration "backfill"))
           (conversation                            (tool-context-conversation context))
           (identity                                (conversation-pathname conversation))
           (artifact                                (shell-log-allocate context))
           (metadata                                (shell-log-artifact-metadata artifact))
           (receipts                                (merge-pathnames "shell-delivery/"
                                                      (conversation-storage-directory-pathname identity)))
           (cursor-path                             (merge-pathnames "index.sexp" receipts)))
      (shell-log-tests--finish artifact #(65))
      (setf metadata (shell-log-artifact-metadata artifact)
            (getf metadata :completion-owner-conversation) "backfill"
            (getf metadata :job-id) "old-job"
            (getf metadata :parent-call-id) "old-call")
      (shell-log--write-manifest artifact)
      (dotimes (index 12)
        (conversation-append-user-message conversation (format nil "Historical input ~D" index)))
      (conversation-append-record conversation
        (list :tool-result :call-id "old-call" :status ':ok
              :details (list :shell-logs (list (shell-log--public-metadata metadata)))))
      (conversation-append-summary conversation "Checkpoint before the receipt index existed")
      (platform-delete-directory-tree *platform* receipts :validate t)
      (setf conversation (conversation-load identity))
      (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                   "Already-rotated legacy delivery lacks active evidence and receipts")
      (let* ((segment (first (conversation-storage-pathnames identity)))
             (forms   (conversation--read-records segment))
             (wrong   (copy-tree forms)))
        (setf (getf (rest (first wrong)) :id) "foreign-owner")
        (unwind-protect
             (progn
               (conversation-identifier-migration--write-forms segment wrong)
               (test-assert (handler-case (progn (shell-log-prune configuration) nil)
                              (conversation-invariant-error () t))
                            "Backfill refuses a historical segment with a foreign owner header")
               (test-assert (not (probe-file cursor-path))
                            "Rejected historical identity publishes no cursor"))
          (conversation-identifier-migration--write-forms segment forms)))
      (shell-log-prune configuration)
      (let ((progress (task-continuity--read cursor-path)))
        (test-assert (and (plusp (getf progress :position))
                          (not (getf progress :complete-p))
                          (not (shell-log--sync-delivered-p configuration metadata)))
                     "A direct prune persists a bounded incomplete batch, not negative authority")
        (let ((write-record (symbol-function 'task-continuity--write)))
          (test-call-with-function-replacements
           (list (list 'task-continuity--write
                       (lambda (path record &key require-absent)
                         (if (uiop:pathname-equal path cursor-path)
                             (shell-log--fail "Injected cursor publication failure.")
                             (funcall write-record path record :require-absent require-absent)))))
           (lambda ()
             (test-assert (handler-case (progn (shell-log-prune configuration) nil)
                            (shell-log-error () t))
                          "Failed cursor publication aborts this pass")))
          (test-assert (equal progress (task-continuity--read cursor-path))
                       "A failed batch leaves durable progress unchanged")))
      (let ((write-record   (symbol-function 'task-continuity--write))
            (index-segment  (symbol-function 'shell-log--index-segment))
            (failed-p       nil)
            (lock-checked-p nil))
        (test-call-with-function-replacements
         (list (list 'task-continuity--write
                     (lambda (path record &key require-absent)
                       (if (and (member "shell-delivery" (pathname-directory path) :test #'equal)
                                (not (uiop:pathname-equal path cursor-path)))
                           (shell-log--fail "Injected backfill receipt publication failure.")
                           (funcall write-record path record :require-absent require-absent))))
               (list 'shell-log--index-segment
                     (lambda (source segment cursor)
                       (unless lock-checked-p
                         (let* ((done   (sb-thread:make-semaphore))
                                (thread (make-thread
                                         (lambda ()
                                           (with-shell-log-lock (configuration)
                                             (sb-thread:signal-semaphore done)))
                                         :name "backfill lock probe")))
                           (test-assert (sb-thread:wait-on-semaphore done :timeout 5)
                                        "Historical indexing leaves the global shell-log lock available")
                           (sb-thread:join-thread thread :timeout 5 :default nil)
                           (setf lock-checked-p t)))
                       (funcall index-segment source segment cursor))))
         (lambda ()
           (loop for before = (task-continuity--read cursor-path)
                 repeat 20
                 do (handler-case (shell-log-prune configuration)
                      (shell-log-error ()
                        (setf failed-p t)
                        (test-assert (equal before (task-continuity--read cursor-path))
                                     "A failed receipt never publishes progress past its batch")
                        (return))))))
        (test-assert failed-p "The fixture reaches and refuses receipt publication")
        (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                     "Failed receipt publication cannot acknowledge historical delivery"))
      (multiple-value-bind (values worker)
          (test-conversation--worker-eval configuration
            (format nil "(let ((configuration ~A) (*shell-log-delivery-index-record-limit* 3))
                           (loop repeat 20
                                 until (shell-log--backfill-owner-step configuration \"backfill\" \"backfill\"))
                           (shell-log--sync-receipt-p configuration ~S '~S))"
                    (test-conversation--child-configuration-form configuration) identity metadata))
        (unwind-protect
             (test-assert (equal values '("T"))
                          "A fresh process resumes the persisted byte cursor and finds exact delivery")
          (lisp-worker-stop worker)))
      (test-assert (shell-log--sync-delivered-p configuration metadata)
                   "Historical delivery becomes bounded positive receipt authority")
      (dolist (field '(:execution-id :artifact-id :completion-owner-conversation :parent-call-id))
        (let ((wrong (copy-list metadata)))
          (setf (getf wrong field) "wrong-identity")
          (test-assert (not (shell-log--sync-delivered-p configuration wrong))
                       (format nil "Backfilled evidence refuses mismatched ~A" field))))
      ;; A missing receipt keeps requesting backfill, without rereading indexed history.
      (let* ((control          (shell-log-allocate context))
             (control-metadata (shell-log-artifact-metadata control)))
        (shell-log-tests--finish control #(66))
        (setf control-metadata (shell-log-artifact-metadata control)
              (getf control-metadata :completion-owner-conversation) "backfill"
              (getf control-metadata :job-id) "future-job"
              (getf control-metadata :parent-call-id) "future-call")
        (shell-log--write-manifest control)
        (test-call-with-function-replacements
         (list (list 'shell-log--index-segment
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "Indexed historical segments were read again."))))
         (lambda ()
           (dotimes (index 3) (shell-log-prune configuration))
           (test-assert (not (shell-log--sync-delivered-p configuration control-metadata))
                        "Index completion does not acknowledge an unrecorded future call")
           (conversation-append-record conversation
             (list :tool-result :call-id "future-call" :status ':ok
                   :details (list :shell-logs (list (shell-log--public-metadata control-metadata)))))
           (test-assert (shell-log--sync-delivered-p configuration control-metadata)
                        "Future active results remain visible after historical index completion"))))
      (let ((*shell-log-retention-age* -1))
        (shell-log-prune configuration))
      (test-assert (eq ':pruned (getf (shell-log--read-manifest configuration
                                       (shell-log-capture-directory artifact) :measure-p nil) :state))
                   "Aged legacy captures are actually reclaimed after backfill")))
  nil)

(-> test-shell-log-delivery-backfill-retention () null)
(defun test-shell-log-delivery-backfill-retention ()
  "Recover already-rotated child evidence before real owner storage removal."
  (let ((*shell-log-delivery-index-record-limit* 1))
    (shell-log-tests--owner-retention
     (lambda (configuration root captures)
       (declare (ignore root))
       (test-assert (shell-log-tests--marked-p configuration (first captures))
                    "Retention stamps the exact historical synchronous capture before deletion")
       (test-assert (shell-log-tests--disk-prunable configuration (first captures))
                    "The positive manifest authority survives removal of its source owner")
       (test-assert (not (shell-log-tests--disk-prunable configuration (third captures)))
                    "An equally old silent owner never acquires delivery authority"))
     :backfill-p t))
  nil)

(-> shell-log-tests--allocation-retention-race
    (configuration conversation function &key (:during-index-p boolean)) null)
(defun shell-log-tests--allocation-retention-race
    (configuration root retain &key during-index-p)
  "Schedule actual owner removal before the index lock or during a historical batch."
  (let* ((identity         (shell-log--owner-identity configuration "retention-root" "000Owner"))
         (owner-directory  (uiop:pathname-directory-pathname identity))
         (source-lock      (conversation-picker-source-lock-pathname identity))
         (context          (make-instance 'tool-context :configuration configuration
                                         :worker nil :conversation root))
         (lock-function    (symbol-function 'call-with-file-lock))
         (index-function   (symbol-function 'shell-log--index-segment))
         (delete-function  (symbol-function 'platform-delete-directory-tree))
         (active           *shell-log-active*)
         (retention-limit  *task-artifact-retention-limit*)
         (allocation-start (sb-thread:make-semaphore))
         (allocation-ready (sb-thread:make-semaphore))
         (allocation-go    (sb-thread:make-semaphore))
         (retention-start  (sb-thread:make-semaphore))
         (retention-ready  (sb-thread:make-semaphore))
         (deletion-ready   (sb-thread:make-semaphore))
         (deletion-go      (sb-thread:make-semaphore))
         (probe-ready      (sb-thread:make-semaphore))
         (probe-started    (sb-thread:make-semaphore))
         (allocator        nil)
         (retainer         nil)
         (probe            nil)
         (paused-p         nil))
    (labels ((wait-for (semaphore)
               (unless (sb-thread:wait-on-semaphore semaphore :timeout 10)
                 (shell-log--fail
                  (format nil "Timed out synchronizing retention (index ~S, paused ~S, allocation ~A)."
                          during-index-p paused-p
                          (when (and allocator (not (sb-thread:thread-alive-p allocator)))
                            (sb-thread:join-thread allocator))))))

             (pause-allocation ()
               (unless paused-p
                 (setf paused-p t)
                 (sb-thread:signal-semaphore allocation-ready)
                 (wait-for allocation-go))))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'call-with-file-lock
                   (lambda (path function &key (mode #o600))
                     (when (uiop:pathname-equal path source-lock)
                       (cond ((and (eq (current-thread) allocator) (not during-index-p))
                              (pause-allocation))
                             ((eq (current-thread) retainer)
                              (sb-thread:signal-semaphore retention-ready))))
                     (funcall lock-function path function :mode mode)))
             (list 'shell-log--index-segment
                   (lambda (source segment cursor)
                     (when (and during-index-p (eq (current-thread) allocator)
                                (uiop:pathname-equal source identity))
                       (pause-allocation))
                     (funcall index-function source segment cursor)))
             (list 'platform-delete-directory-tree
                   (lambda (platform directory &key validate if-does-not-exist)
                     (when (and during-index-p (uiop:pathname-equal directory owner-directory))
                       (sb-thread:signal-semaphore deletion-ready)
                       (wait-for deletion-go))
                     (funcall delete-function platform directory
                              :validate validate :if-does-not-exist if-does-not-exist))))
            (lambda ()
              (setf allocator
                    (make-thread
                     (lambda ()
                       (wait-for allocation-start)
                       (let ((*shell-log-active* active))
                         (handler-case (shell-log-allocate context)
                           (error (condition) condition))))
                     :name "allocation racing owner retention"))
              (sb-thread:signal-semaphore allocation-start)
              (wait-for allocation-ready)
              (if during-index-p
                  (progn
                    (setf retainer
                          (make-thread
                           (lambda ()
                             (wait-for retention-start)
                            (let ((*shell-log-active*              active)
                                  (*task-artifact-retention-limit* retention-limit))
                               (handler-case (funcall retain)
                                 (error (condition) condition))))
                           :name "retention racing historical batch"))
                    (sb-thread:signal-semaphore retention-start)
                    (wait-for retention-ready)
                    (test-assert (probe-file owner-directory)
                                 "Retention cannot delete storage during the allocation's historical batch")
                    (sb-thread:signal-semaphore allocation-go)
                    (wait-for deletion-ready)
                    (setf probe
                          (make-thread
                           (lambda ()
                             (sb-thread:signal-semaphore probe-started)
                             (funcall lock-function source-lock
                                      (lambda () (sb-thread:signal-semaphore probe-ready))))
                           :name "source lock probe during owner deletion"))
                    (wait-for probe-started)
                    (test-assert (not (sb-thread:wait-on-semaphore probe-ready :timeout 0.1))
                                 "Retention holds the source lock through actual storage deletion")
                    (sb-thread:signal-semaphore deletion-go)
                    (test-assert (not (typep (sb-thread:join-thread retainer) 'condition))
                                 "Concurrent result publication completes without an indexing failure")
                    (wait-for probe-ready)
                    (sb-thread:join-thread probe))
                  (progn
                    (funcall retain)
                    (test-assert (not (probe-file owner-directory))
                                 "Retention removes the owner before allocation obtains its source lock")
                    (sb-thread:signal-semaphore allocation-go)))
              (let ((result (sb-thread:join-thread allocator)))
                (test-assert (typep result 'shell-log-artifact)
                             "Allocation succeeds after concurrent owner retention")
                (when (typep result 'shell-log-artifact)
                  (shell-log-tests--finish result #(65))))))
        (dolist (semaphore (list allocation-start allocation-go retention-start deletion-go))
          (sb-thread:signal-semaphore semaphore))
        (dolist (thread (list allocator retainer probe))
          (when thread
            (sb-thread:join-thread thread :timeout 15 :default nil))))))
  nil)

(-> test-shell-log-backfill-retention-races () null)
(defun test-shell-log-backfill-retention-races ()
  "Allocate successfully across both historical-index and owner-deletion races."
  (dolist (during-index-p '(nil t))
    (shell-log-tests--owner-retention
     (lambda (configuration root captures)
       (declare (ignore root))
       (test-assert (shell-log-tests--marked-p configuration (first captures))
                    "Concurrent retention preserves the historical positive delivery evidence"))
     :backfill-p t
     :around-retention
     (lambda (configuration root retain)
       (shell-log-tests--allocation-retention-race configuration root retain
                                                   :during-index-p during-index-p))))
  nil)
