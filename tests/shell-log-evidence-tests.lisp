(in-package #:autolith)

;;;; -- Capture Retention at the Knowledge Storage Boundary --

(-> shell-log-evidence-tests--schema (configuration) list)
(defun shell-log-evidence-tests--schema (configuration)
  "Define a global keyed observation schema for disposable retention cases."
  (let ((schema (knowledge-make-schema
                 :predicate "capture.value"
                 :arguments '((:name "item" :type :entity) (:name "value" :type :string))
                 :key       '("item"))))
    (knowledge-define-schema configuration schema :scope ':global)
    schema))

(-> shell-log-evidence-tests--observation
    (shell-log-artifact &key (:stream keyword) (:valid-until (option timestamp))) list)
(defun shell-log-evidence-tests--observation (artifact &key (stream ':output) valid-until)
  "Construct trusted exact stream evidence without copying raw capture bytes."
  (knowledge-make-evidence
   :source (list :tool :name "shell.run" :conversation
                 (getf (shell-log-artifact-metadata artifact) :root-conversation)
                 :shell-logs (list (shell-log-reference artifact stream)))
   :observed-at (knowledge-now) :valid-until valid-until
   :digest (memory-source-text-digest "captured") :adapter "capture-test/1"))

(-> shell-log-evidence-tests--age (shell-log-artifact) null)
(defun shell-log-evidence-tests--age (artifact)
  "Make ARTIFACT eligible for age cleanup without changing its raw evidence."
  (setf (getf (shell-log-artifact-metadata artifact) :created-at)
        (- (get-universal-time) *shell-log-retention-age* 1))
  (shell-log--write-manifest artifact)
  nil)

(-> shell-log-evidence-tests--state (shell-log-artifact) keyword)
(defun shell-log-evidence-tests--state (artifact)
  "Read the durable state independently of the artifact's mutable local metadata."
  (getf (shell-log--read-manifest (shell-log-artifact-configuration artifact)
                                 (shell-log-capture-directory artifact)) :state))

(-> test-shell-log-many-small-artifacts () null)
(defun test-shell-log-many-small-artifacts ()
  "Allocate beyond the former historical count ceiling while respecting byte limits."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 16)
           (context                        (shell-log-tests--context configuration "many-small-captures"))
           (artifacts                      nil))
      (loop repeat 260 do
        (let ((artifact (shell-log-allocate context)))
          (shell-log-tests--finish artifact #(120))
          (push artifact artifacts)))
      (test-assert (and (= 260 (length (shell-log--directories configuration)))
                        (every (lambda (artifact)
                                 (eq ':closed (shell-log-evidence-tests--state artifact))) artifacts))
                   "More than256 tiny completed captures neither refuse allocation nor force count eviction")))
  nil)

(-> test-shell-log-structured-evidence-pins () null)
(defun test-shell-log-structured-evidence-pins ()
  "Retain entire artifacts across workspaces and future validity until the last support ends."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 16)
           (schema                         (shell-log-evidence-tests--schema configuration))
           (context                        (shell-log-tests--context configuration "structured-capture"))
           (artifact                       (shell-log-allocate context :merge-output-p nil))
           (elsewhere                      (configuration-copy configuration
                                                                :working-directory (merge-pathnames "other/" root))))
      (ensure-directories-exist (merge-pathnames "other/" root))
      (shell-log-tests--finish artifact #(111) :error-bytes #(101))
      (let* ((evidence (shell-log-evidence-tests--observation artifact :stream ':error))
             (first    (knowledge-assert
                        elsewhere (knowledge-make-proposition schema '("foreign" "observed"))
                        :evidence evidence :basis ':observation))
             (future   (knowledge-assert
                        configuration (knowledge-make-proposition schema '("future" "observed"))
                        :evidence evidence :basis ':observation :valid-from (+ (knowledge-now) 3600))))
        (shell-log-evidence-tests--age artifact)
        (shell-log-prune configuration)
        (test-assert (and (probe-file (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
                          (probe-file (merge-pathnames "error.bin" (shell-log-capture-directory artifact))))
                     "A foreign-workspace reference to one stream protects both raw streams")
        (knowledge-retract elsewhere first)
        (shell-log-prune configuration)
        (test-assert (eq ':closed (shell-log-evidence-tests--state artifact))
                     "Future-valid support retains evidence before its applicability window")
        (knowledge-retract configuration future)
        (shell-log-prune configuration)
        (test-assert (eq ':pruned (shell-log-evidence-tests--state artifact))
                     "Retraction of the final support releases the containing artifact"))))
  nil)

(-> test-shell-log-evidence-expiry-and-replacement () null)
(defun test-shell-log-evidence-expiry-and-replacement ()
  "Release old observations on replacement and expired raw evidence on the next prune."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 16)
           (schema                         (shell-log-evidence-tests--schema configuration))
           (context                        (shell-log-tests--context configuration "capture-expiry"))
           (old                            (shell-log-allocate context))
           (new                            nil)
           (now                            (knowledge-now)))
      (shell-log-tests--finish old #(111))
      (setf new (shell-log-allocate context))
      (shell-log-tests--finish new #(110))
      (knowledge-replace-observation
       configuration (knowledge-make-proposition schema '("sensor" "old"))
       (shell-log-evidence-tests--observation old))
      (knowledge-replace-observation
       configuration (knowledge-make-proposition schema '("sensor" "new"))
       (shell-log-evidence-tests--observation new :valid-until (+ now 10)))
      (shell-log-evidence-tests--age old)
      (shell-log-evidence-tests--age new)
      (shell-log-prune configuration)
      (test-assert (and (eq ':pruned (shell-log-evidence-tests--state old))
                        (eq ':closed (shell-log-evidence-tests--state new)))
                   "Source replacement releases superseded evidence but retains the replacement")
      (test-call-with-function-replacements
       (list (list 'knowledge-now (lambda () (+ now 11))))
       (lambda () (shell-log-prune configuration)))
      (test-assert (eq ':pruned (shell-log-evidence-tests--state new))
                   "Expired evidence releases its final raw dependency")))
  nil)

(-> test-shell-log-file-evidence-and-capacity () null)
(defun test-shell-log-file-evidence-and-capacity ()
  "Pin exact managed FILE evidence and refuse honest byte pressure when everything is pinned."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   16)
           (*shell-log-retention-byte-limit* 32)
           (schema                           (shell-log-evidence-tests--schema configuration))
           (context                          (shell-log-tests--context configuration "capture-file-pin"))
           (artifact                         (shell-log-allocate context :merge-output-p nil)))
      (shell-log-tests--finish artifact #(1 2 3 4 5 6 7 8) :error-bytes #(8 7 6 5 4 3 2 1))
      (let* ((path  (merge-pathnames "error.bin" (shell-log-capture-directory artifact)))
             (claim (knowledge-assert
                     configuration (knowledge-make-proposition schema '("file" "observed"))
                     :evidence (knowledge-observe-file path) :basis ':observation)))
        (let ((*shell-log-retention-byte-limit* 31))
          (test-assert (handler-case (progn (shell-log-allocate context) nil)
                         (shell-log-error () t))
                       "All pinned raw bytes cause a truthful capacity refusal")
          (test-assert (probe-file path) "Capacity refusal preserves formal managed-file evidence")
          (knowledge-retract configuration claim)
          (let ((replacement (shell-log-allocate context)))
            (unwind-protect
                 (test-assert (eq ':pruned (shell-log-evidence-tests--state artifact))
                              "Releasing the last FILE support permits byte-pressure cleanup")
              (shell-log-release-artifact replacement)))))))
  nil)

(-> test-shell-log-reference-authority () null)
(defun test-shell-log-reference-authority ()
  "Require structured actual-result metadata, not an incidental URI in copied text."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 16)
           (schema                         (shell-log-evidence-tests--schema configuration))
           (context                        (shell-log-tests--context configuration "capture-authority"))
           (artifact                       (shell-log-allocate context)))
      (shell-log-tests--finish artifact #(120))
      (let* ((public (shell-log--public-metadata (shell-log-artifact-metadata artifact)))
             (result (shell-command-result "captured" (list :shell-logs (list public)) t)))
        (let ((key (namestring (shell-log-capture-directory artifact))))
          (with-shell-log-lock (configuration)
            (setf (gethash key *shell-log-active*) ':stale))
          (unwind-protect
               (progn
                 (test-assert (equal (shell-log-result-evidence-references configuration result)
                                     (list (shell-log-reference artifact)))
                              "Actual structured tool details yield the exact retained stream identity")
                 (test-assert (eq ':stale (gethash key *shell-log-active*))
                              "Unlocked evidence identity reads do not sweep local ownership")
                 (with-shell-log-lock (configuration)
                   (shell-log--read-manifest configuration (shell-log-capture-directory artifact))
                   (test-assert (not (gethash key *shell-log-active*))
                                "Locked measuring reads reconcile stale ownership")))
            (with-shell-log-lock (configuration)
              (remhash key *shell-log-active*))))
        (let ((wrong (copy-list public)))
          (setf (getf wrong :execution-id) "another-execution")
          (test-assert
           (handler-case
               (progn (shell-log-result-evidence-references
                       configuration (shell-command-result "captured" (list :shell-logs (list wrong)) t))
                      nil)
             (knowledge-error () t))
           "A copied reference cannot authenticate mismatched actual-result metadata")))
      (knowledge-assert configuration (knowledge-make-proposition schema '("excerpt" "observed"))
                        :evidence (knowledge-observe
                                   :source '(:tool :name "other" :shell-logs nil)
                                   :excerpt (shell-log-reference artifact) :adapter "copy/1")
                        :basis    ':observation)
      (shell-log-evidence-tests--age artifact)
      (shell-log-prune configuration)
      (test-assert (eq ':pruned (shell-log-evidence-tests--state artifact))
                   "A shell-log URI in an excerpt alone does not establish a dependency")))
  nil)

(-> test-shell-log-interrupted-reservations () null)
(defun test-shell-log-interrupted-reservations ()
  "Retain live maximum reservations, then reconcile dead interrupted captures to measured bytes."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 64)
           (context                          (shell-log-tests--context configuration "capture-reservations"))
           (artifact                         (shell-log-allocate context :merge-output-p nil)))
      (shell-log-record-capture
       artifact (make-instance 'cl-exec-sandbox:sandbox-result
                               :output-capture (shell-log-tests--capture artifact #(120))
                               :error-capture  (shell-log-tests--capture artifact #(101))
                               :exit-code nil :timed-out-p nil :cancelled-p nil :status ':running)
       :closed-p nil)
      (shell-log-prune configuration)
      (test-assert (= 64 (getf (shell-log--read-manifest
                                  configuration (shell-log-capture-directory artifact)) :accounted-bytes))
                   "A real live capture lease retains the maximum separated-stream reservation")
      (shell-log--release-owner (shell-log-capture-directory artifact))
      (shell-log-prune configuration)
      (let ((metadata (shell-log--read-manifest configuration (shell-log-capture-directory artifact))))
        (test-assert (and (eq ':interrupted (getf metadata :state))
                          (zerop (getf metadata :reserved-bytes))
                          (= 2 (getf metadata :accounted-bytes))
                          (every (lambda (capture)
                                   (and (not (getf capture :complete-p))
                                        (not (getf capture :observed-byte-count-known-p))))
                                 (getf metadata :captures)))
                     "Dead orphan reservation shrinks to safe measured bytes without fabricating completeness"))
      (let* ((*shell-log-retention-byte-limit* 34)
             (replacement                      (shell-log-allocate context)))
        (unwind-protect
             (test-assert (eq ':interrupted (shell-log-evidence-tests--state artifact))
                          "Reclaimed reservation admits another capture without evicting a tiny recent orphan")
          (shell-log-release-artifact replacement)))))
  nil)

(-> shell-log-evidence-tests--worker-call (t string) list)
(defun shell-log-evidence-tests--worker-call (worker form)
  "Evaluate one control form in a disposable contender and require transport success."
  (let ((response (lisp-worker-request worker ':eval (list :forms (list form)))))
    (test-assert (eq (getf (rest response) :status) ':ok)
                 (format nil "Evidence contender completed: ~S" response))
    response))

(-> shell-log-evidence-tests--start-writer
    (configuration list list &key (:pause-p boolean)) t)
(defun shell-log-evidence-tests--start-writer (configuration proposition evidence &key pause-p)
  "Start a separate-process writer; optionally pause after raw-reference validation."
  (multiple-value-bind (rendered worker)
      (test-conversation--worker-eval
       configuration
       (format nil
               "(let* ((configuration ~A)
                       (ready (sb-thread:make-semaphore))
                       (resume (sb-thread:make-semaphore))
                       (done (sb-thread:make-semaphore))
                       (state (list :ready ready :resume resume :done done :result nil))
                       (validate (symbol-function 'shell-log-validate-evidence-records)))
                  (defparameter *shell-log-evidence-race-fixture* state)
                  (when ~S
                    (setf (symbol-function 'shell-log-validate-evidence-records)
                          (lambda (configuration base records)
                            (funcall validate configuration base records)
                            (sb-thread:signal-semaphore ready)
                            (unless (sb-thread:wait-on-semaphore resume :timeout 30)
                              (error \"The paused evidence commit was not resumed\")))))
                  (sb-thread:make-thread
                   (lambda ()
                     (unwind-protect
                          (handler-case
                              (progn
                                (unless ~S (sb-thread:signal-semaphore ready))
                                (setf (getf state :result)
                                      (knowledge-assert configuration '~S :evidence '~S
                                                        :basis ':observation :scope ':global)))
                            (knowledge-error () (setf (getf state :result) ':refused))
                            (error (condition) (setf (getf state :result) (princ-to-string condition))))
                       (setf (symbol-function 'shell-log-validate-evidence-records) validate)
                       (sb-thread:signal-semaphore done))))
                  (unless (sb-thread:wait-on-semaphore ready :timeout 30)
                    (error \"The evidence writer never reached its admission boundary\")))"
               (test-conversation--child-configuration-form configuration)
               pause-p pause-p proposition evidence))
    (declare (ignore rendered))
    worker))

(-> test-shell-log-evidence-commit-prune-races () null)
(defun test-shell-log-evidence-commit-prune-races ()
  "Serialize cross-process fact commits with final evidence checks and raw deletion in both orders."
  (dolist (commit-first-p '(t nil))
    (with-test-configuration (configuration)
      (let* ((*shell-log-active*             (make-hash-table :test #'equal))
             (*shell-log-capture-byte-limit* 16)
             (schema                         (shell-log-evidence-tests--schema configuration))
             (context                        (shell-log-tests--context configuration "capture-evidence-race"))
             (artifact                       (shell-log-allocate context))
             (worker                         nil)
             (pruner                         nil)
             (prune-result                   nil)
             (ready                          (sb-thread:make-semaphore))
             (resume                         (sb-thread:make-semaphore))
             (done                           (sb-thread:make-semaphore)))
        (shell-log-tests--finish artifact #(120))
        (shell-log-evidence-tests--age artifact)
        (let ((evidence    (shell-log-evidence-tests--observation artifact))
              (proposition (knowledge-make-proposition schema '("contender" "observed"))))
          (labels ((start-prune ()
                     "Start pruning in this process, recording completion separately from admission."
                     (setf pruner
                           (make-thread
                            (lambda ()
                              (unwind-protect
                                   (handler-case (progn (shell-log-prune configuration)
                                                        (setf prune-result t))
                                     (error (condition) (setf prune-result condition)))
                                (sb-thread:signal-semaphore done))))))

                   (finish-prune ()
                     "Join the local contender and preserve its actual condition on failure."
                     (test-assert (sb-thread:wait-on-semaphore done :timeout 30)
                                  "The pruning contender completed")
                     (join-thread pruner)
                     (test-assert (eq prune-result t)
                                  (format nil "Pruning completed without a lock or storage failure: ~A" prune-result))))
            (unwind-protect
                 (if commit-first-p
                     (progn
                       (setf worker (shell-log-evidence-tests--start-writer
                                     configuration proposition evidence :pause-p t))
                       (start-prune)
                       (test-assert (not (sb-thread:wait-on-semaphore done :timeout 0.1))
                                    "Another process's evidence append excludes pruning until commit")
                       (shell-log-evidence-tests--worker-call
                        worker
                        "(sb-thread:signal-semaphore (getf *shell-log-evidence-race-fixture* :resume))")
                       (finish-prune)
                       (shell-log-evidence-tests--worker-call
                        worker
                        "(progn
                           (unless (sb-thread:wait-on-semaphore (getf *shell-log-evidence-race-fixture* :done) :timeout 30)
                             (error \"Evidence commit did not finish\"))
                           (unless (stringp (getf *shell-log-evidence-race-fixture* :result))
                             (error \"Evidence commit failed\")))")
                       (test-assert (eq ':closed (shell-log-evidence-tests--state artifact))
                                    "A committed cross-process fact pins raw storage before the final deletion check"))
                     (let ((prunable (symbol-function 'shell-log--prunable-p)))
                       (test-call-with-function-replacements
                        (list (list 'shell-log--prunable-p
                                    (lambda (configuration directory metadata)
                                      (let ((eligible (funcall prunable configuration directory metadata)))
                                        (when eligible
                                          (sb-thread:signal-semaphore ready)
                                          (unless (sb-thread:wait-on-semaphore resume :timeout 30)
                                            (error "The paused final deletion was not resumed")))
                                        eligible))))
                        (lambda ()
                          (start-prune)
                          (test-assert (sb-thread:wait-on-semaphore ready :timeout 30)
                                       "Pruning reached the locked final eligibility decision")
                          (setf worker (shell-log-evidence-tests--start-writer
                                        configuration proposition evidence))
                          (shell-log-evidence-tests--worker-call
                           worker
                           "(when (sb-thread:wait-on-semaphore (getf *shell-log-evidence-race-fixture* :done) :timeout 0.1)
                              (error \"A writer bypassed the pruning process lock\"))")
                          (sb-thread:signal-semaphore resume)
                          (finish-prune)
                          (shell-log-evidence-tests--worker-call
                           worker
                           "(progn
                              (unless (sb-thread:wait-on-semaphore (getf *shell-log-evidence-race-fixture* :done) :timeout 30)
                                (error \"The excluded evidence writer did not finish\"))
                              (unless (eq ':refused (getf *shell-log-evidence-race-fixture* :result))
                                (error \"A new support adopted raw evidence after deletion\")))")
                          (test-assert (and (eq ':pruned (shell-log-evidence-tests--state artifact))
                                            (zerop (hash-table-count
                                                    (knowledge-base-supports (knowledge-base-read configuration)))))
                                       "Deletion wins atomically, and subsequent support adoption refuses missing raw evidence")))))
              (sb-thread:signal-semaphore resume)
              (when worker (lisp-worker-stop worker))
              (when (and pruner (thread-alive-p pruner)) (join-thread pruner))))))))
  nil)

(-> test-shell-log-closed-live-reservation () null)
(defun test-shell-log-closed-live-reservation ()
  "Charge a closed capture's maximum allowance until its actual cleanup owner exits."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 32)
           (context                          (shell-log-tests--context configuration "closed-live-reservation"))
           (artifact                         (shell-log-allocate context))
           (ready                            (sb-thread:make-semaphore))
           (resume                           (sb-thread:make-semaphore))
           (owner                            (make-thread
                                              (lambda ()
                                                (sb-thread:signal-semaphore ready)
                                                (sb-thread:wait-on-semaphore resume :timeout 30)))))
      (unwind-protect
           (progn
             (test-assert (sb-thread:wait-on-semaphore ready :timeout 30)
                          "The capture cleanup owner is actually live")
             (setf (shell-log-artifact-native-owners artifact) (list owner))
             (shell-log-tests--finish artifact #(120))
             (shell-log-release-artifact artifact)
             (test-assert (= 32 (getf (shell-log--read-manifest
                                       configuration (shell-log-capture-directory artifact))
                                     :accounted-bytes))
                          "Closed capture metadata does not release a live cleanup reservation")
             (test-assert (handler-case (progn (shell-log-allocate context) nil)
                            (shell-log-error () t))
                          "Another allocation cannot overcommit a closed but live writer's reservation")
             (sb-thread:signal-semaphore resume)
             (join-thread owner)
             (shell-log-prune configuration)
             (test-assert (= 1 (getf (shell-log--read-manifest
                                      configuration (shell-log-capture-directory artifact))
                                    :accounted-bytes))
                          "The closed capture costs only measured bytes after its final owner dies"))
        (sb-thread:signal-semaphore resume)
        (when (thread-alive-p owner) (join-thread owner)))))
  nil)

(-> test-shell-log-managed-file-write-validation () null)
(defun test-shell-log-managed-file-write-validation ()
  "Refuse missing or mismatched managed FILE dependencies while accepting ordinary files."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 16)
           (schema                         (shell-log-evidence-tests--schema configuration))
           (context                        (shell-log-tests--context configuration "managed-file-validation"))
           (artifact                       (shell-log-allocate context))
           (directory                      (shell-log-capture-directory artifact)))
      (shell-log-tests--finish artifact #(120))
      (flet ((assert-file (path item)
               "Observe PATH and assert one fresh support through the actual append boundary."
               (knowledge-assert
                configuration (knowledge-make-proposition schema (list item "observed"))
                :evidence (knowledge-observe-file path) :basis ':observation)))
        (let ((wrong (merge-pathnames "not-a-capture.bin" directory)))
          (with-open-file (stream wrong :direction ':output) (write-string "ordinary" stream))
          (test-assert (handler-case (progn (assert-file wrong "mismatch") nil)
                         (knowledge-error () t))
                       "A file inside a managed artifact must match its manifest capture exactly")
          (platform-delete-file *platform* wrong))
        (let* ((path     (merge-pathnames "output.bin" directory))
               (observed (knowledge-observe-file path)))
          (platform-delete-file *platform* (shell-log--manifest-path directory))
          (test-assert
           (handler-case
               (progn (knowledge-assert
                       configuration (knowledge-make-proposition schema '("missing" "observed"))
                       :evidence observed :basis ':observation)
                      nil)
             (knowledge-error () t))
           "A missing managed manifest cannot turn an old FILE dependency into an ordinary file"))
        (test-assert (zerop (hash-table-count
                             (knowledge-base-supports (knowledge-base-read configuration))))
                     "Invalid managed dependencies append no supporting claims")
        (let ((ordinary (merge-pathnames "notes/shell-log/ordinary.txt" root)))
          (ensure-directories-exist ordinary)
          (with-open-file (stream ordinary :direction ':output) (write-string "ordinary" stream))
          (test-assert (stringp (assert-file ordinary "ordinary"))
                       "An unrelated file under a similarly named directory is ordinary evidence")))))
  nil)
