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
  (let* ((path (merge-pathnames (if (eq stream ':output) "output.bin" "error.bin")
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
                  :error-capture (and error-bytes (shell-log-tests--capture artifact error-bytes ':error))
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
                           :parameters (tool-object-schema (json-object) '())
                           :resource-registry (make-resource-registry))
   context arguments))

(-> shell-log-tests--public-inventory () null)
(defun shell-log-tests--public-inventory ()
  "Exercise bounded job metadata, public projection and missing-manifest recovery."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context (shell-log-tests--context configuration "shell-inventory"))
           (orchestrator (task-orchestrator-create))
           (job (task-tests--make-job orchestrator :identifier "inventory-job"
                                      :root-identifier "shell-inventory"))
           (latest nil)
           (base-time (get-universal-time)))
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
                    (logs (getf metadata :shell-logs)))
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
                                                       :root-identifier "shell-inventory"))
                    (artifact (shell-log-allocate context :job missing-job)))
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
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context (shell-log-tests--context configuration "shell-descendants"))
           (orchestrator (task-orchestrator-create))
           (job (task-tests--make-job orchestrator :identifier "current-child"
                                      :root-identifier "shell-descendants"))
           (agent (task-tests--child-viewer configuration job))
           (child-context (make-instance 'tool-context :configuration configuration :worker nil
                                         :conversation (agent-conversation agent) :agent agent))
           (artifact (shell-log-allocate context)))
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
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 96)
           (context (shell-log-tests--context configuration "shell-sync-proof"))
           (orchestrator (task-orchestrator-create))
           (job (task-tests--make-job orchestrator :identifier "sync-proof-job"
                                      :root-identifier "shell-sync-proof" :detached-p nil)))
      (unwind-protect
           (progn
             (setf (slot-value job 'completion-owner-conversation) "shell-sync-proof"
                   (slot-value job 'parent-call-id) "expected-shell-call")
             (let* ((artifact (shell-log-allocate context :job job))
                    (metadata (shell-log-tests--finish artifact (utf8-string-to-octets "x"))))
               (test-assert (not (shell-log--prunable-p configuration
                                                       (shell-log-capture-directory artifact) metadata))
                            "Closing a synchronous job does not prove delivery")
               ;; Simulate an oversized regular file independently of the executor.
               (shell-log-tests--capture artifact
                                         (make-array 100 :element-type '(unsigned-byte 8)
                                                         :initial-element 120))
               (let* ((measured (shell-log--read-manifest configuration
                                                         (shell-log-capture-directory artifact)))
                      (capture (first (getf measured :captures))))
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
                 (setf (getf metadata :detached-p) nil))
               (let ((replacement (shell-log-allocate context)))
                 (test-assert (eq ':pruned (getf (shell-log--read-manifest
                                                 configuration (shell-log-capture-directory artifact)) :state))
                              "Delivered sync logs are eligible for safe quota pruning")
                 (shell-log-release-artifact replacement))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--pre-executor-unwind () null)
(defun shell-log-tests--pre-executor-unwind ()
  "Release real pre-executor failures even when a synchronous job has no receipt."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 32)
           (*shell-log-retention-count-limit* 1)
           (context (shell-log-tests--context configuration "shell-preflight"))
           (orchestrator (task-orchestrator-create))
           (*tool-execution-current-job*
             (task-tests--make-job orchestrator :identifier "preflight-job"
                                  :root-identifier "shell-preflight" :detached-p nil)))
      (unwind-protect
           (dotimes (attempt 2)
             (declare (ignore attempt))
             (test-assert
              (handler-case
                  (progn
                    (workspace-tool-run-shell-command
                     (test-fixture-shell-command *platform* "exit 0" "exit 0")
                     (config :working-directory configuration) ':invalid-policy 1 32
                     :context context)
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
        (task-orchestrator-close orchestrator))))
  nil)

;;;; -- Durable Storage and Inspection --

(-> test-shell-log-private-allocation () null)
(defun test-shell-log-private-allocation ()
  "Reserve before launch; generate distinct execution identities without a runtime."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 96)
           (context (shell-log-tests--context configuration "shell-owner"))
           (first (shell-log-allocate context))
           (second (shell-log-allocate context :merge-output-p nil)))
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
  "Read exact raw ranges and search bounded pages across gigantic unterminated lines."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 256)
           (*shell-log-resource-scan-bytes* 16)
           (context (shell-log-tests--context configuration "shell-windows"))
           (artifact (shell-log-allocate context))
           (bytes (make-array 96 :element-type '(unsigned-byte 8) :initial-element 120)))
      (replace bytes (utf8-string-to-octets "needle") :start1 14)
      (replace bytes (utf8-string-to-octets "needle") :start1 50)
      (setf (aref bytes 30) 255 (aref bytes 31) 195 (aref bytes 32) 169)
      (shell-log-tests--finish artifact bytes)
      (let* ((resource (shell-log-tests--resolve artifact context))
             (path (merge-pathnames "output.bin" (shell-log-capture-directory artifact))))
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
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 256)
           (owner (shell-log-tests--context configuration "shell-authority"))
           (foreign (shell-log-tests--context configuration "shell-foreign"))
           (artifact (shell-log-allocate owner)))
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
        (let* ((path (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
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
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 64)
           (context (shell-log-tests--context configuration "shell-restart"))
           (artifact (shell-log-allocate context :merge-output-p nil))
           (reference (shell-log-reference artifact)))
      (shell-log-tests--finish artifact (utf8-string-to-octets "stdout")
                              :error-bytes (utf8-string-to-octets "stderr failure"))
      (let ((*shell-log-active* (make-hash-table :test #'equal)))
        (let ((reopened (shell-log-tests--resolve artifact context ':error)))
          (test-assert
           (search "stderr failure" (tool-result-content
                                      (shell-log-tests--read reopened context (json-object))))
           "Restart inspection recovers the separated error stream from its manifest")))
      (let ((replacement (shell-log-allocate context)))
        (test-assert (string= reference (shell-log-reference artifact)) "Pruning does not change stable references")
        (test-assert (eq ':pruned (getf (shell-log--read-manifest configuration
                                                               (shell-log-capture-directory artifact)) :state))
                     "Quota pressure prunes eligible closed captures")
        (test-assert (tool-result-success-p
                      (shell-log-tests--read (shell-log-tests--resolve artifact context) context (json-object)))
                     "Tombstones produce a diagnostic, not a failed session")
        (remhash (namestring (shell-log-capture-directory replacement)) *shell-log-active*)
        (test-assert (eq ':interrupted (getf (shell-log--read-manifest
                                            configuration (shell-log-capture-directory replacement)) :state))
                     "Abandoned prelaunch manifests never claim completion"))))
  (with-test-configuration (configuration)
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (*shell-log-retention-byte-limit* 32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 1)
           (context (shell-log-tests--context configuration "shell-tombstones")))
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
    (let* ((*shell-log-active* (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context (shell-log-tests--context configuration "shell-failures")))
      (dolist (status '(:timeout :cancelled :launch-failed :interrupted))
        (let* ((artifact (shell-log-allocate context))
               (capture (shell-log-tests--capture artifact (utf8-string-to-octets "partial"))))
          (setf (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) 100
                (cl-exec-sandbox:sandbox-capture-complete-p capture) nil
                (cl-exec-sandbox:sandbox-capture-truncated-p capture) t
                (cl-exec-sandbox:sandbox-capture-status capture) ':limit)
          (let* ((metadata (shell-log-record-capture
                            artifact (make-instance 'cl-exec-sandbox:sandbox-result
                                                     :output-capture capture :exit-code nil
                                                     :status status :timed-out-p (eq status ':timeout)
                                                     :cancelled-p (eq status ':cancelled))))
                 (record (first (getf metadata :captures))))
            (test-assert (and (eq status (getf metadata :status))
                             (= 7 (getf record :byte-count))
                             (= 100 (getf record :observed-byte-count))
                             (not (getf record :complete-p)) (getf record :truncated-p))
                         "Portable manifests preserve storage loss separately from execution failure"))))))
  nil)
