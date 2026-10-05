(in-package #:autolith)

;;;; -- Durable Job Continuity --

(define-condition task-continuity-error (task-error)
  ()
  (:documentation "A continuity decision lacks authority or a safe recovery path."))

(defparameter *task-continuity-maximum-octets* (* 1024 1024)
  "The maximum portable continuity record size.")

(-> task-continuity--fail (string) nil)
(defun task-continuity--fail (message)
  "Signal a typed continuity refusal with MESSAGE."
  (error 'task-continuity-error :message message :tool-name "job.continuity"))

(-> task-continuity--read (pathname) list)
(defun task-continuity--read (path)
  "Read one bounded portable property record without reader evaluation."
  (sexp-store:snapshot-read-record
   path :grammar (task--result-grammar)
   :maximum-octets *task-continuity-maximum-octets*
   :properties-p t :keyword-keys-p t :maximum-length 128))


(-> task-continuity--write (pathname list &key (:require-absent boolean)) pathname)
(defun task-continuity--write (path record &key require-absent)
  "Publish portable metadata without shared-object reader labels."
  (snapshot-write-text path (format nil "~A~%" (task--write-readable-sexp record))
                       :require-absent require-absent))

(-> task-continuity--owner-executions (session-job agent) list)
(defun task-continuity--owner-executions (job parent)
  "Capture durable ancestor identities rather than replay-prone friendly names."
  (let ((parent-job (and (typep parent 'task-child-agent) (task-child-agent-job parent)))
        (jobs (task-orchestrator-list-jobs (session-job-orchestrator job))))
    (loop for identifier in (job-owner-identifiers job)
          for owner = (or (and parent-job (equal identifier (job-identifier parent-job)) parent-job)
                          (find identifier jobs :key #'job-identifier :test #'equal))
          when owner collect (session-job-execution-identifier owner))))

(-> task-continuity-record-job (session-job agent) pathname)
(defun task-continuity-record-job (job parent)
  "Persist JOB's identity and specification before its first side effect.

Tool closures are deliberately not serialized. Task replay needs a separate
explicit safety declaration; admission alone grants no replay authority."
  (let* ((configuration (agent-configuration parent))
         (root (task--artifact-group-root
                configuration (session-job-root-conversation-identifier job)))
         (owner-executions (task-continuity--owner-executions job parent))
         (path (merge-pathnames
                (format nil "~A/continuity.sexp"
                        (session-job-execution-identifier job)) root)))
    (with-lock-held ((task-orchestrator-artifact-lock
                      (session-job-orchestrator job)))
      (unless (probe-file path)
        (ensure-directories-exist path)
        (task-continuity--write
         path
         (list :version 1 :job-id (session-job-identifier job)
               :execution-id (session-job-execution-identifier job)
               :root-conversation (session-job-root-conversation-identifier job)
               :owners (copy-list (job-owner-identifiers job))
               :owner-execution-ids owner-executions
               :parent-call-id (session-job-parent-call-id job)
               :created-at (get-universal-time)
               :kind (if (typep job 'task-job) ':task ':tool)
               :specification (when (typep job 'task-job)
                                (copy-tree (task-job-item job))))
         :require-absent t)))
    path))

(-> task-continuity--visible-p (list agent) boolean)
(defun task-continuity--visible-p (record viewer)
  "Apply ordinary conversation and descendant ownership to durable RECORD."
  (and (equal (getf record :root-conversation)
              (task-parent-root-conversation-identifier viewer))
       (or (not (typep viewer 'task-child-agent))
           (member (session-job-execution-identifier (task-child-agent-job viewer))
                   (getf record :owner-execution-ids) :test #'equal))
       t))

(-> task-continuity-records (agent) list)
(defun task-continuity-records (viewer)
  "Return owned durable records or bounded diagnostics from existing task storage."
  (let* ((root-id (task-parent-root-conversation-identifier viewer))
         (root (task--artifact-group-root (agent-configuration viewer) root-id)))
    (loop for directory in (when (uiop:directory-exists-p root)
                             (uiop:subdirectories root))
          for path = (merge-pathnames "continuity.sexp" directory)
          for result-path = (merge-pathnames "result.sexp" directory)
          when (or (probe-file path)
                   (and (not (typep viewer 'task-child-agent)) (probe-file result-path)))
            append
            (handler-case
                (let* ((old-result (and (not (probe-file path))
                                        (task--read-result-artifact result-path)))
                       (record
                         (if old-result
                             (list :version 1 :kind ':task
                                   :execution-id (first (last (pathname-directory directory)))
                                   :job-id (or (getf old-result :id)
                                               (first (last (pathname-directory directory))))
                                   :root-conversation root-id :owners nil)
                             (task-continuity--read path))))
                  (unless (and (eql 1 (getf record :version))
                               (member (getf record :kind) '(:task :tool))
                               (every #'non-empty-string-p
                                      (list (getf record :execution-id)
                                            (getf record :job-id)
                                            (getf record :root-conversation)))
                               (equal (getf record :execution-id)
                                      (first (last (pathname-directory directory))))
                               (listp (getf record :owners))
                               (every #'non-empty-string-p (getf record :owners)))
                    (task-continuity--fail "Invalid persisted continuity identity."))
                  (let ((specification (getf record :specification))
                        (checkpoint (getf record :checkpoint)))
                    (when specification
                      (unless (and (eq (getf record :kind) :task)
                                   (non-empty-string-p (getf specification :agent))
                                   (non-empty-string-p (getf specification :task)))
                        (task-continuity--fail "Invalid persisted task specification.")))
                    (when checkpoint
                      (unless (and (non-empty-string-p (getf checkpoint :image))
                                   (non-empty-string-p (getf checkpoint :repl)))
                        (task-continuity--fail "Invalid persisted worker checkpoint."))))
                  (when (task-continuity--visible-p record viewer)
                    (list (list :record record :path (namestring path)))))
              (error (condition)
                (unless (typep viewer 'task-child-agent)
                  (list (list :path (namestring path)
                              :error (bounded-string (princ-to-string condition)
                                                     :limit 500)))))))))

(-> task-continuity--find (agent string) list)
(defun task-continuity--find (viewer identifier)
  "Find one owned durable job or execution IDENTIFIER; reject ambiguity."
  (let ((matches
          (remove-if-not
           (lambda (entry)
             (let ((record (getf entry :record)))
               (and record
                    (or (equal identifier (getf record :job-id))
                        (equal identifier (getf record :execution-id))))))
           (task-continuity-records viewer))))
    (unless (= 1 (length matches))
      (task-continuity--fail
       "No unique owned continuity record exists; use an execution ID."))
    (first matches)))

(-> task-continuity--checkpoint-image (list configuration) (option lisp-image))
(defun task-continuity--checkpoint-image (record configuration)
  "Return RECORD's compatible saved worker image with its core still present."
  (let* ((checkpoint (getf record :checkpoint))
         (image (and checkpoint
                     (find (getf checkpoint :image)
                           (lisp-image-scan configuration)
                           :key #'lisp-image-identifier :test #'equal))))
    (when (and image (lisp-image-compatible-p image)
               (probe-file (lisp-image-core-pathname image)))
      image)))


(-> task-continuity--progress-summary (list) list)
(defun task-continuity--progress-summary (progress)
  "Project bounded supervisory progress without copying transcript output."
  (when progress
    (append
     (loop for key in '(:status :current-tool :recent-tools :request-count :duration-ms :model)
           append (list key (task--compact-native-value (getf progress key) 256)))
     (list :usage (task--compact-native-value (getf progress :usage) 1000)))))

(-> task-continuity--result-summary (list (option pathname)) list)
(defun task-continuity--result-summary (result path)
  "Project timing, reported usage and artifact locations without result bodies."
  (when result
    (append
     (loop for key in '(:status :request-count :duration-ms :conversation-file :worktree-artifact-path)
           append (list key (task--compact-native-value (getf result key) 1000)))
     (list :usage (task--compact-native-value (getf result :usage) 1000)
           :output-path (and path (namestring path))))))

(-> task-continuity-classify (list agent task-orchestrator) list)
(defun task-continuity-classify (entry viewer orchestrator)
  "Classify ENTRY from owned live state and readable durable recovery evidence."
  (let* ((record (getf entry :record))
         (path (and (getf entry :path) (pathname (getf entry :path))))
         (directory (and path (uiop:pathname-directory-pathname path)))
         (live (and record
                    (find (getf record :execution-id)
                          (task-orchestrator-list-visible-jobs orchestrator viewer)
                          :key #'session-job-execution-identifier :test #'equal)))
         (claim (and directory (merge-pathnames "revive.sexp" directory)))
         (abandoned (and directory (merge-pathnames "abandon.sexp" directory)))
         (result-path (and directory (merge-pathnames "result.sexp" directory)))
         (terminal-path (and directory (merge-pathnames "terminal.sexp" directory)))
         (activity-path (and directory (merge-pathnames "activity.sexp" directory)))
         (artifact-error (getf entry :error))
         (result (and result-path (probe-file result-path)
                      (handler-case (task--read-result-artifact result-path)
                        (error () (setf artifact-error "The terminal artifact is unreadable.") nil))))
         (terminal (and terminal-path (probe-file terminal-path)
                        (handler-case (task-continuity--read terminal-path)
                          (error () (setf artifact-error "The terminal metadata is unreadable.") nil))))
         (activity (and activity-path (probe-file activity-path)
                        (handler-case (task-continuity--read activity-path)
                          (error () nil))))
         (decision-path (and directory
                             (find-if #'probe-file
                                      (list abandoned claim
                                            (merge-pathnames "reattach.sexp" directory)))))
         (decision (and decision-path
                        (handler-case (task-continuity--read decision-path)
                          (error () (setf artifact-error "The recovery decision is unreadable.") nil))))
         (recovery-path (and directory (merge-pathnames "revive-result.sexp" directory)))
         (recovery (and recovery-path (probe-file recovery-path)
                        (handler-case (task-continuity--read recovery-path)
                          (error () (setf artifact-error "The recovery result is unreadable.") nil)))))
    (append
     (list :classification
           (cond
             ((and live (not (job-terminal-p live))) ':live-owned-detached)
             (artifact-error ':dead-needing-decision)
             ((or (and live (job-terminal-p live)) result terminal) ':completed)
             ((and abandoned (probe-file abandoned)) ':abandoned)
             ((and claim (probe-file claim)) ':dead-needing-decision)
             ((task-continuity--checkpoint-image record (agent-configuration viewer))
              ':reconstructible-checkpoint)
             ((and (eq (getf record :kind) :task)
                   (getf record :specification)
                   (not (getf (getf record :specification) :isolation))
                   (non-empty-string-p (getf record :restart-safety)))
              ':safe-restartable-specification)
             (t ':dead-needing-decision))
           :job-id (getf record :job-id) :execution-id (getf record :execution-id)
           :root-conversation (getf record :root-conversation)
           :owners (getf record :owners) :created-at (getf record :created-at)
           :path (getf entry :path) :tool-count (getf activity :tool-count)
           :latest-activity (getf activity :latest-activity)
           :updated-at (getf activity :updated-at)
           :progress (task-continuity--progress-summary (getf activity :progress))
           :result (task-continuity--result-summary
                    (or result (getf terminal :result)) (if result result-path terminal-path))
           :agent (getf (getf record :specification) :agent)
           :assignment (bounded-string (or (getf (getf record :specification) :task) "")
                                       :limit *task-retained-assignment-limit*)
           :restart-safety (getf record :restart-safety)
           :checkpoint (getf record :checkpoint)
           :owner-execution-ids (getf record :owner-execution-ids)
           :decision (getf decision :decision)
           :decision-record decision :decision-path (and decision-path (namestring decision-path))
           :recovery-outcome (cond
                               (recovery (if (getf recovery :success-p) ':dispatched ':failed))
                               ((and claim (probe-file claim)) ':unknown)
                               (t nil))
           :recovery-result (when recovery
                              (append
                               (loop for key in '(:execution-id :root-conversation :dispatch-call-id
                                                  :new-executions :checkpoint :at :success-p)
                                     append (list key (getf recovery key)))
                               (list :path (namestring recovery-path))))
           :revive-claim-p (and claim (probe-file claim) t))
     (when live
       (let* ((snapshot (session-job-snapshot live))
              (summary (task-continuity--result-summary (getf snapshot :result) nil)))
         (list :live (list :state (getf snapshot :state)
                          :progress (task-continuity--progress-summary (getf snapshot :progress))
                          :result summary))))
     (when artifact-error (list :error artifact-error)))))

(-> task-continuity-inventory (agent task-orchestrator) list)
(defun task-continuity-inventory (viewer orchestrator)
  "Return current durable classifications, including persisted unfinished work."
  (mapcar (lambda (entry) (task-continuity-classify entry viewer orchestrator))
          (task-continuity-records viewer)))

(-> task-continuity-declare
    (agent task-orchestrator string &key (:safety (option string))
     (:image (option string)) (:repl (option string))) list)
(defun task-continuity-declare (viewer orchestrator identifier &key safety image repl)
  "Explicitly declare restart safety or associate a compatible immutable checkpoint.

A primary conversation owner must make this declaration. SAFETY documents why
repeating the task's external effects is safe; it is an assertion, not a test."
  (when (typep viewer 'task-child-agent)
    (task-continuity--fail "Only the primary owner may grant recovery authority."))
  (unless (or (non-empty-string-p safety)
              (and (non-empty-string-p image) (non-empty-string-p repl)))
    (task-continuity--fail "Supply a safety rationale or both image and REPL."))
  (let* ((entry (task-continuity--find viewer identifier))
         (path (pathname (getf entry :path)))
         (record (copy-tree (getf entry :record))))
    (when safety
      (unless (eq (getf record :kind) :task)
        (task-continuity--fail "An opaque tool closure cannot be restarted."))
      (setf (getf record :restart-safety) (bounded-string safety :limit 4000)))
    (when image
      (setf (getf record :checkpoint) (list :image image :repl repl))
      (unless (task-continuity--checkpoint-image record (agent-configuration viewer))
        (task-continuity--fail "The saved worker image is missing or incompatible.")))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (when (probe-file (merge-pathnames "revive.sexp"
                                       (uiop:pathname-directory-pathname path)))
        (task-continuity--fail "A consumed recovery declaration cannot be changed."))
      (task-continuity--write path record))
    (task-continuity-classify (list :record record :path (namestring path))
                              viewer orchestrator)))

(-> task-continuity-dispatch (tool-context string string json-object) (values tool-result string))
(defun task-continuity-dispatch (context namespace name arguments)
  "Dispatch a normal registered operation with correlated durable action records."
  (let* ((registry (tool-context-registry context))
         (tool (tool-registry-find registry namespace name))
         (call-id (make-identifier))
         (conversation (tool-context-conversation context))
         (observer (tool-context-observer context))
         (call (json-object "namespace" namespace "name" name
                            "call_id" call-id "arguments" (json-encode arguments))))
    (unless tool
      (task-continuity--fail "The requested recovery operation is not registered."))
    (let ((policy (and (boundp '*worker-host-tool-policy*)
                       (symbol-value '*worker-host-tool-policy*))))
      (when (and (getf policy :restricted-p)
                 (not (member (tool-canonical-name tool) (getf policy :allowlist)
                              :test #'equal)))
        (task-continuity--fail "The originating capability scope withholds this action.")))
    (conversation-append-record
     conversation (list :task-inspector-call :call-id call-id
                        :parent-call-id (tool-context-call-id context)
                        :tool (tool-canonical-name tool)
                        :arguments (json-encode arguments)))
    (let* ((nested-context
             (make-instance
              'tool-context :configuration (tool-context-configuration context)
              :conversation conversation :worker (tool-context-worker context)
              :registry registry :agent (tool-context-agent context)
              :observer observer :call-id call-id
              :mutation-checker (tool-context-mutation-checker context)
              :command-authorization-function
              (tool-context-command-authorization-function context)
              :tool-authorization-function
              (tool-context-tool-authorization-function context)))
           (result
             (flet ((execute ()
                      (tool-registry-execute-call registry call nested-context)))
               (if observer
                   (agent-observer-call-with-tool-execution observer call-id #'execute)
                   (execute)))))
      (conversation-append-record
       conversation (list :task-inspector-result :call-id call-id
                          :success-p (tool-result-success-p result)
                          :content (bounded-string (tool-result-content result) :limit 8000)))
      (values result call-id))))

(-> task-continuity-revive
    (tool-context task-orchestrator string &key (:authorized-p boolean)) tool-result)
(defun task-continuity-revive (context orchestrator identifier &key authorized-p)
  "Explicitly recover IDENTIFIER once, claiming replay durably before dispatch.

A failed or interrupted dispatch consumes the claim. Its unknown side effects
require a new human decision rather than an automatic second attempt."
  (let* ((viewer (tool-context-agent context))
         (entry (task-continuity--find viewer identifier))
         (record (getf entry :record))
         (directory (uiop:pathname-directory-pathname (getf entry :path)))
         (classification (getf (task-continuity-classify entry viewer orchestrator)
                               :classification)))
    (unless (and authorized-p (not (typep viewer 'task-child-agent)))
      (task-continuity--fail "Recovery requires explicit primary-owner authority."))
    (when (eq classification :live-owned-detached)
      (task-continuity--write
       (merge-pathnames "reattach.sexp" directory)
       (list :decision ':reattach :at (get-universal-time)
             :execution-id (getf record :execution-id)
             :root-conversation (getf record :root-conversation)))
      (return-from task-continuity-revive
        (task-continuity-dispatch context "job" "get"
                                 (json-object "id" (getf record :job-id)))))
    (unless (member classification '(:safe-restartable-specification
                                    :reconstructible-checkpoint))
      (task-continuity--fail "This job has no unconsumed safe recovery pathway."))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (task-continuity--write
       (merge-pathnames "revive.sexp" directory)
       (list :decision (if (eq classification :reconstructible-checkpoint)
                           ':reconstruct ':restart)
             :at (get-universal-time) :execution-id (getf record :execution-id)
             :root-conversation (getf record :root-conversation))
       :require-absent t))
    (let ((checkpoint (getf record :checkpoint))
          (spec (getf record :specification)))
      (multiple-value-bind (result dispatch-call-id)
          (if (eq classification :reconstructible-checkpoint)
              (task-continuity-dispatch
               context "lisp" "start"
               (json-object "image" (getf checkpoint :image) "repl" (getf checkpoint :repl)))
              (task-continuity-dispatch
               context "task" "run"
               (let ((arguments (json-object "agent" (getf spec :agent) "task" (getf spec :task))))
                 (when (getf spec :context)
                   (setf (gethash "context" arguments) (getf spec :context)))
                 arguments)))
        (task-continuity--write
         (merge-pathnames "revive-result.sexp" directory)
         (list :success-p (tool-result-success-p result)
               :execution-id (getf record :execution-id)
               :root-conversation (getf record :root-conversation)
               :dispatch-call-id dispatch-call-id
               :new-executions
               (loop for job in (task-orchestrator-list-visible-jobs orchestrator viewer)
                     when (equal dispatch-call-id (session-job-parent-call-id job))
                       collect (list :job-id (session-job-identifier job)
                                     :execution-id (session-job-execution-identifier job)
                                     :root-conversation (session-job-root-conversation-identifier job)))
               :checkpoint checkpoint
               :content (bounded-string (tool-result-content result) :limit 8000)
               :at (get-universal-time)))
        result))))

(-> task-continuity-abandon (agent task-orchestrator string) list)
(defun task-continuity-abandon (viewer orchestrator identifier)
  "Record an explicit abandon decision for dead owned work."
  (when (typep viewer 'task-child-agent)
    (task-continuity--fail "Only the primary owner may abandon durable work."))
  (let* ((entry (task-continuity--find viewer identifier))
         (classification (task-continuity-classify entry viewer orchestrator)))
    (when (eq (getf classification :classification) :live-owned-detached)
      (task-continuity--fail "Cancel live work through job.cancel before abandonment."))
    (task-continuity--write
     (merge-pathnames "abandon.sexp"
                      (uiop:pathname-directory-pathname (getf entry :path)))
     (list :decision ':abandon :at (get-universal-time)) :require-absent t)
    (task-continuity-classify entry viewer orchestrator)))


(-> task-continuity-note-activity (task-job keyword list) null)
(defun task-continuity-note-activity (job status details)
  "Persist bounded child activity counters after a normal observer event."
  (let* ((parent (task-job-parent-agent job))
         (progress (task-progress-snapshot job))
         (path (merge-pathnames "activity.sexp"
                               (task--artifact-root (agent-configuration parent) job)))
         (orchestrator (session-job-orchestrator job)))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (let* ((old (and (probe-file path) (task-continuity--read path)))
             (count (+ (or (getf old :tool-count) 0)
                       (if (eq status :tool-call-completed) 1 0))))
        (task-continuity--write
         path (list :tool-count count :updated-at (get-universal-time)
                    :latest-activity status :tool (getf details :tool)
                    :progress progress)))))
  nil)

(-> task-continuity-record-terminal (session-job agent list &key (:state keyword)) null)
(defun task-continuity-record-terminal (job parent result &key state)
  "Persist terminal metadata for an opaque asynchronous tool without replaying it."
  (let* ((path (task-continuity-record-job job parent))
         (target (merge-pathnames "terminal.sexp" (uiop:pathname-directory-pathname path))))
    (task-continuity--write target (list :state state :result result
                                :ended-at (get-universal-time)) :require-absent t))
  nil)
