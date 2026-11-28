(in-package #:autolith)

;;;; -- Durable Job Continuity --

(define-condition task-continuity-error (task-error)
  ()
  (:documentation "A continuity decision lacks authority or a safe recovery path."))

(defparameter *task-continuity-maximum-octets* (* 1024 1024)
  "The maximum portable continuity record size.")

(-> task-continuity--fail (string &key (:code (option keyword))) nil)
(defun task-continuity--fail (message &key code)
  "Signal a typed continuity refusal with MESSAGE."
  (error 'task-continuity-error :message message :code code :tool-name "job.continuity"))

(-> task-continuity--read (pathname) list)
(defun task-continuity--read (path)
  "Read one bounded portable property record without reader evaluation."
  (sexp-store:snapshot-read-record
   path :grammar   (task--result-grammar)
   :maximum-octets *task-continuity-maximum-octets*
   :properties-p t :keyword-keys-p t :maximum-length 128))

(-> task-continuity--write (pathname list &key (:require-absent boolean)) pathname)
(defun task-continuity--write (path record &key require-absent)
  "Publish portable metadata only when it fits the bounded continuity reader."
  (let ((text (format nil "~A~%" (task--write-readable-sexp record))))
    (when (> (utf8-string-length text) *task-continuity-maximum-octets*)
      (task-continuity--fail "Continuity metadata exceeds its readable UTF-8 size limit."
                             :code ':storage-limit))
    (snapshot-write-text path text :require-absent require-absent)))

(-> task-continuity-record-job (session-job agent) pathname)
(defun task-continuity-record-job (job parent)
  "Persist JOB's identity before its first side effect.

Tool closures are not serialized and cannot be replayed."
  (let* ((configuration (agent-configuration parent))
         (root          (task--artifact-group-root
                         configuration (session-job-completion-owner-conversation job)))
         (path          (merge-pathnames
                         (format nil "~A/continuity.sexp"
                                 (session-job-execution-identifier job)) root)))
    (with-lock-held ((task-orchestrator-artifact-lock
                      (session-job-orchestrator job)))
      (unless (probe-file path)
        (ensure-directories-exist path)
        (when (session-job-detached-p job)
          (task-completion-index-note job configuration))
        (task-unfinished-work-index-note job configuration)
        (task-continuity--write
         path
         (list :version 1 :job-id (session-job-identifier job)
               :execution-id           (session-job-execution-identifier job)
               :root-conversation      (session-job-root-conversation-identifier job)
                :owners                nil
                :owner-execution-ids   nil
               :parent-call-id         (session-job-parent-call-id job)
                :completion-owner-conversation
                (session-job-completion-owner-conversation job)
                :completion-owner-participant
                (session-job-completion-owner-participant job)
                :completion-policy     (session-job-completion-policy job)
                :completion-mission-id (session-job-completion-mission-id job)
                :detached-p            (session-job-detached-p job)
               :created-at             (get-universal-time)
                :kind                  ':tool
                :tool                  (tool-execution-job-tool-name job))
         :require-absent t)))
    path))

(-> task-continuity--visible-p (list agent) boolean)
(defun task-continuity--visible-p (record viewer)
  "Require RECORD's exact recorded completion owner to match VIEWER's channel."
  (and (equal (getf record :completion-owner-conversation)
              (conversation-identifier (agent-conversation viewer)))
       t))

(-> task-continuity--artifact-owners (agent) list)
(defun task-continuity--artifact-owners (viewer)
  "Return VIEWER's channel and its registered ancestors for legacy artifact lookup."
  (let* ((configuration (agent-configuration viewer))
         (world         (world-load configuration))
         (identifier    (conversation-identifier (agent-conversation viewer)))
         (seen          (make-hash-table :test #'equal))
         (owners        nil))
    (loop while (and identifier (not (gethash identifier seen)))
          do (setf (gethash identifier seen) t)
             (push identifier owners)
             (let ((channel (world-channel-find world identifier)))
               (setf identifier (and channel (channel-parent channel)))))
    (nreverse owners)))

(-> task-continuity-map-records (agent function &key (:execution-ids list)) null)
(defun task-continuity-map-records (viewer function &key (execution-ids nil indexed-p))
  "Call FUNCTION for owned records in this channel and registered legacy ancestors.

If EXECUTION-IDS is supplied, only the listed executions are scanned. Return NIL."
  (dolist (owner (task-continuity--artifact-owners viewer))
    (let* ((root        (task--artifact-group-root (agent-configuration viewer) owner))
           (directories (if indexed-p
                            (mapcar (lambda (identifier)
                                      (merge-pathnames (format nil "~A/" identifier) root))
                                    execution-ids)
                            (when (uiop:directory-exists-p root)
                              (uiop:subdirectories root)))))
      (dolist (directory directories)
        (dolist (entry (task-continuity-directory-entries viewer directory))
          (funcall function entry)))))
  nil)

(-> task-continuity-records (agent) list)
(defun task-continuity-records (viewer)
  "Return owned durable records from existing task storage."
  (let ((records nil))
    (task-continuity-map-records viewer (lambda (entry) (push entry records)))
    (nreverse records)))

(-> task-continuity-directory-entries (agent pathname) list)
(defun task-continuity-directory-entries (viewer directory)
  "Return the owned durable record or bounded diagnostic of one job DIRECTORY.

The result is a list of at most one entry, so callers can append scans."
  (let ((path (merge-pathnames "continuity.sexp" directory)))
    (when (probe-file path)
      (handler-case
          (let ((record (task-continuity--read path)))
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
                  (checkpoint    (getf record :checkpoint)))
              (when specification
                (unless (and (eq (getf record :kind) :task)
                             (non-empty-string-p (getf specification :task)))
                  (task-continuity--fail "Invalid persisted task specification.")))
              (when checkpoint
                (unless (and (non-empty-string-p (getf checkpoint :image))
                             (non-empty-string-p (getf checkpoint :repl)))
                  (task-continuity--fail "Invalid persisted worker checkpoint."))))
            (when (task-continuity--visible-p record viewer)
              (list (list :record record :path (namestring path)))))
        (error ()
          ;; A malformed record has no authenticated owner, even in an ancestor.
          nil)))))

(-> task-continuity--find (agent string) list)
(defun task-continuity--find (viewer identifier)
  "Return the owned entry of IDENTIFIER. Reject ambiguous job or execution IDs."
  (let ((match nil))
    (task-continuity-map-records
     viewer
     (lambda (entry)
       (let ((record (getf entry :record)))
         (when (and record
                    (or (equal identifier (getf record :job-id))
                        (equal identifier (getf record :execution-id))))
           (when match
             (task-continuity--fail
              "No unique owned continuity record exists. Use an execution ID."))
           (setf match entry)))))
    (or match
        (task-continuity--fail
         "No unique owned continuity record exists. Use an execution ID."))))

(-> task-continuity--checkpoint-image (list configuration) (option lisp-image))
(defun task-continuity--checkpoint-image (record configuration)
  "Return RECORD's compatible saved worker image with its core still present."
  (let* ((checkpoint (getf record :checkpoint))
         (image      (and checkpoint
                          (find (getf checkpoint :image)
                                (lisp-image-scan configuration)
                                :key #'sbcl-worker-image-identifier :test #'equal))))
    (when (and image (lisp-image-compatible-p image)
                (probe-file (sbcl-worker-image-core-pathname image)))
      image)))

(-> task-continuity--progress-summary (list) list)
(defun task-continuity--progress-summary (progress)
  "Project bounded supervisory progress without copying transcript output."
  (when progress
    (append
     (loop for key in '(:status :current-tool :recent-tools :request-count :duration-ms :model)
           append (list key (task--compact-native-value (getf progress key) 256)))
     (list :usage (task-progress--usage-summary (getf progress :usage))))))

(-> task--shell-log-reference-summary (list) list)
(defun task--shell-log-reference-summary (logs)
  "Project at most four latest stable log references from portable LOGS metadata."
  (let* ((references
           (remove-duplicates
            (loop for log in (stable-sort (copy-list logs) #'>
                                          :key (lambda (log) (getf log :created-at 0)))
                  append (loop for capture in (getf log :captures)
                               for reference = (getf capture :reference)
                               when (stringp reference) collect reference))
            :test #'equal))
         (total    (length references))
         (retained (remove-if (lambda (reference) (> (length reference) 1024))
                              (subseq references 0 (min total 4)))))
    (list :shell-log-references    retained
          :shell-log-omitted-count (- total (length retained)))))

(-> task-continuity--result-summary (list (option pathname)) list)
(defun task-continuity--result-summary (result path)
  "Project timing, reported usage and artifact locations without result bodies."
  (when result
    (append
     (loop for key in '(:status :label :error :request-count :duration-ms :conversation-file :worktree-artifact-path)
           append (list key (task--compact-native-value (getf result key) 1000)))
     (task--shell-log-reference-summary (getf result :shell-logs))
     (list :usage       (task-progress--usage-summary (getf result :usage))
           :output-path (and path (namestring path))))))

(-> task-continuity-classify (list agent task-orchestrator) list)
(defun task-continuity-classify (entry viewer orchestrator)
  "Classify ENTRY from owned live state and readable durable recovery evidence."
  (let* ((record         (getf entry :record))
         (path           (and (getf entry :path) (pathname (getf entry :path))))
         (directory      (and path (uiop:pathname-directory-pathname path)))
         (live           (and record
                              (find (getf record :execution-id)
                                    (task-orchestrator-list-visible-jobs orchestrator viewer)
                                    :key #'session-job-execution-identifier :test #'equal)))
         (claim          (and directory (merge-pathnames "revive.sexp" directory)))
         (abandoned      (and directory (merge-pathnames "abandon.sexp" directory)))
         (result-path    (and directory (merge-pathnames "result.sexp" directory)))
         (terminal-path  (and directory (merge-pathnames "terminal.sexp" directory)))
         (activity-path  (and directory (merge-pathnames "activity.sexp" directory)))
         (artifact-error (getf entry :error))
         (result         (and result-path (probe-file result-path)
                              (handler-case (task--read-result-artifact result-path)
                                (error () (setf artifact-error "The terminal artifact is unreadable.") nil))))
         (terminal       (and terminal-path (probe-file terminal-path)
                              (handler-case (task-continuity--read terminal-path)
                                (error () (setf artifact-error "The terminal metadata is unreadable.") nil))))
         (activity       (and activity-path (probe-file activity-path)
                              (handler-case (task-continuity--read activity-path)
                                (error () nil))))
         (decision-path  (and directory
                              (find-if #'probe-file
                                       (list abandoned claim
                                             (merge-pathnames "reattach.sexp" directory)))))
         (decision       (and decision-path
                              (handler-case (task-continuity--read decision-path)
                                (error () (setf artifact-error "The recovery decision is unreadable.") nil))))
         (recovery-path  (and directory (merge-pathnames "revive-result.sexp" directory)))
         (recovery       (and recovery-path (probe-file recovery-path)
                              (handler-case (task-continuity--read recovery-path)
                                (error () (setf artifact-error "The recovery result is unreadable.") nil)))))
    (append
     (list :classification
           (cond
             ((and live (not (job-terminal-p live)))
              ':live-owned-detached)
             (artifact-error
              ':dead-needing-decision)
             ((or (and live (job-terminal-p live)) result terminal)
              ':completed)
             ((and abandoned (probe-file abandoned))
              ':abandoned)
             ((and claim (probe-file claim))
              ':dead-needing-decision)
             ((task-continuity--checkpoint-image record (agent-configuration viewer))
              ':reconstructible-checkpoint)
             (t
              ':dead-needing-decision))
           :job-id (getf record :job-id) :execution-id (getf record :execution-id)
           :root-conversation             (getf record :root-conversation)
           :owners (getf record :owners) :created-at (getf record :created-at)
           :completion-owner-conversation (getf record :completion-owner-conversation)
           :completion-policy             (getf record :completion-policy)
           :completion-mission-id         (getf record :completion-mission-id)
           :completion-route
           (when (getf record :detached-p)
             (if (equal (getf record :completion-owner-conversation)
                        (conversation-identifier (agent-conversation viewer)))
                 ':owning-conversation
                 ':inspection-only))
           :path (getf entry :path) :tool-count (getf activity :tool-count)
           :latest-activity               (getf activity :latest-activity)
           :updated-at                    (getf activity :updated-at)
           :progress                      (task-continuity--progress-summary (getf activity :progress))
           :result                        (task-continuity--result-summary
                                           (or result (getf terminal :result)) (if result result-path terminal-path))
           :assignment                    (bounded-string (or (getf (getf record :specification) :task) "")
                                                          :limit 1000)
           :restart-safety                (getf record :restart-safety)
           :checkpoint                    (getf record :checkpoint)
           :owner-execution-ids           (getf record :owner-execution-ids)
           :decision                      (getf decision :decision)
           :decision-record decision :decision-path (and decision-path (namestring decision-path))
           :recovery-outcome              (cond
                                            (recovery
                                             (if (getf recovery :success-p) ':dispatched ':failed))
                                            ((and claim (probe-file claim))
                                             ':unknown)
                                            (t
                                             nil))
           :recovery-result               (when recovery
                                            (append
                                             (loop for key in '(:execution-id :root-conversation :dispatch-call-id
                                                                :new-executions :checkpoint :at :success-p)
                                                   append (list key (getf recovery key)))
                                             (list :path (namestring recovery-path))))
           :revive-claim-p                (and claim (probe-file claim) t))
     (when live
       (let* ((snapshot (session-job-snapshot live))
              (summary  (task-continuity--result-summary (getf snapshot :result) nil)))
         (list :live (list :state   (getf snapshot :state)
                          :progress (task-continuity--progress-summary (getf snapshot :progress))
                          :result   summary))))
     (when artifact-error (list :error artifact-error)))))

(-> task-continuity-inventory (agent task-orchestrator) list)
(defun task-continuity-inventory (viewer orchestrator)
  "Return current durable classifications, including persisted unfinished work."
  (let ((rows nil))
    (task-continuity-map-records
     viewer (lambda (entry) (push (task-continuity-classify entry viewer orchestrator) rows)))
    (nreverse rows)))

(-> task-continuity-declare (agent task-orchestrator string &key (:image string) (:repl string)) list)
(defun task-continuity-declare (viewer orchestrator identifier &key image repl)
  "Associate a compatible worker checkpoint with the owned job."
  (when (conversation-parent-identifier (agent-conversation viewer))
    (task-continuity--fail "Only the primary owner may grant recovery authority." :code ':authority))
  (unless (and (non-empty-string-p image) (non-empty-string-p repl))
    (task-continuity--fail "Supply both image and REPL."))
  (let* ((entry      (task-continuity--find viewer identifier))
         (path       (pathname (getf entry :path)))
         (record     (copy-tree (getf entry :record)))
         (checkpoint (list :image image :repl repl)))
    (setf (getf record :checkpoint) checkpoint)
    (unless (task-continuity--checkpoint-image record (agent-configuration viewer))
      (task-continuity--fail "The saved worker image is missing or incompatible."))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (when (probe-file (merge-pathnames "revive.sexp"
                                        (uiop:pathname-directory-pathname path)))
        (task-continuity--fail "A consumed recovery declaration cannot be changed."))
      (setf record (task-continuity--read path)
            (getf record :checkpoint) checkpoint)
      (task-continuity--write path record))
    (task-continuity-classify (list :record record :path (namestring path))
                              viewer orchestrator)))

(-> task-continuity-dispatch
    (tool-context string string &key (:arguments json-object))
    (values tool-result string))
(defun task-continuity-dispatch (context namespace name &key arguments)
  "Dispatch a normal registered operation with correlated durable action records."
  (let* ((registry     (tool-context-registry context))
         (tool         (tool-registry-find registry namespace name))
         (call-id      (make-identifier))
         (conversation (tool-context-conversation context))
         (observer     (tool-context-observer context))
         (call         (json-object "namespace" namespace "name" name
                                    "call_id" call-id "arguments" arguments)))
    (unless tool
      (task-continuity--fail "The requested recovery operation is not registered."))
    (let ((policy (and (boundp '*worker-host-tool-policy*)
                       (symbol-value '*worker-host-tool-policy*))))
      (when (and (getf policy :restricted-p)
                 (not (member (tool-canonical-name tool) (getf policy :allowlist)
                              :test #'equal)))
        (task-continuity--fail "The originating capability scope withholds this action."
                               :code ':authority)))
    (conversation-append-record
     conversation (list :task-inspector-call :call-id call-id
                        :parent-call-id      (tool-context-call-id context)
                        :tool                (tool-canonical-name tool)
                        :arguments           (json-encode arguments)))
    (let* ((nested-context
             (make-instance
              'tool-context
              :configuration    (tool-context-configuration context)
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
                      (tool-registry-execute-native-call registry call nested-context)))
               (if observer
                   (agent-observer-call-with-tool-execution observer call-id #'execute)
                   (execute)))))
      (conversation-append-record
       conversation (list :task-inspector-result :call-id call-id
                          :success-p             (tool-result-success-p result)
                          :content               (bounded-string (tool-result-content result) :limit 8000)))
      (values result call-id))))

(-> task-continuity-revive
    (tool-context task-orchestrator string &key (:authorized-p boolean)) tool-result)
(defun task-continuity-revive (context orchestrator identifier &key authorized-p)
  "Explicitly recover IDENTIFIER once, claiming replay durably before dispatch.

A failed or interrupted dispatch consumes the claim. Its unknown side effects
require a new human decision rather than an automatic second attempt."
  (let* ((viewer         (tool-context-agent context))
         (entry          (task-continuity--find viewer identifier))
         (record         (getf entry :record))
         (directory      (uiop:pathname-directory-pathname (getf entry :path)))
         (classification (getf (task-continuity-classify entry viewer orchestrator)
                               :classification)))
    (unless (and authorized-p (not (resource-context-child-agent-p context)))
      (task-continuity--fail "Recovery requires explicit primary-owner authority." :code ':authority))
    (when (eq classification :live-owned-detached)
      (task-continuity--write
       (merge-pathnames "reattach.sexp" directory)
       (list :decision ':reattach :at (get-universal-time)
             :execution-id      (getf record :execution-id)
             :root-conversation (getf record :root-conversation)))
      (return-from task-continuity-revive
        (task-continuity-dispatch context "job" "get"
                                  :arguments (json-object "id" (getf record :job-id)))))
    (unless (eq classification :reconstructible-checkpoint)
      (task-continuity--fail "This job has no unconsumed safe recovery pathway."))
    (with-lock-held ((task-orchestrator-artifact-lock orchestrator))
      (task-continuity--write
       (merge-pathnames "revive.sexp" directory)
       (list :decision         ':reconstruct
            :at (get-universal-time) :execution-id (getf record :execution-id)
            :root-conversation (getf record :root-conversation))
       :require-absent t))
    (let ((checkpoint (getf record :checkpoint)))
      (multiple-value-bind (result dispatch-call-id)
          (task-continuity-dispatch
           context "lisp" "start"
           :arguments (json-object "image" (getf checkpoint :image) "repl" (getf checkpoint :repl)))
        (task-continuity--write
         (merge-pathnames "revive-result.sexp" directory)
         (list :success-p         (tool-result-success-p result)
               :execution-id      (getf record :execution-id)
               :root-conversation (getf record :root-conversation)
               :dispatch-call-id  dispatch-call-id
               :new-executions
               (loop for job in (task-orchestrator-list-visible-jobs orchestrator viewer)
                     when (equal dispatch-call-id (session-job-parent-call-id job))
                       collect (list :job-id            (session-job-identifier job)
                                     :execution-id      (session-job-execution-identifier job)
                                     :root-conversation (session-job-root-conversation-identifier job)))
               :checkpoint        checkpoint
               :content           (bounded-string (tool-result-content result) :limit 8000)
               :at                (get-universal-time)))
        result))))

(-> task-continuity-abandon (agent task-orchestrator string) list)
(defun task-continuity-abandon (viewer orchestrator identifier)
  "Record an explicit abandon decision for dead owned work."
  (when (conversation-parent-identifier (agent-conversation viewer))
    (task-continuity--fail "Only the primary owner may abandon durable work." :code ':authority))
  (let* ((entry          (task-continuity--find viewer identifier))
         (classification (task-continuity-classify entry viewer orchestrator)))
    (when (eq (getf classification :classification) :live-owned-detached)
      (task-continuity--fail "Cancel live work through job.cancel before abandonment."))
    (task-continuity--write
     (merge-pathnames "abandon.sexp"
                      (uiop:pathname-directory-pathname (getf entry :path)))
     (list :decision ':abandon :at (get-universal-time)) :require-absent t)
    (task-continuity-classify entry viewer orchestrator)))

(-> task-continuity-record-terminal (session-job agent list &key (:state keyword)) null)
(defun task-continuity-record-terminal (job parent result &key state)
  "Persist terminal metadata for an opaque asynchronous tool without replaying it."
  (let* ((path   (task-continuity-record-job job parent))
         (target (merge-pathnames "terminal.sexp" (uiop:pathname-directory-pathname path))))
    (unless (getf result :shell-logs)
      (let ((metadata (shell-log-job-metadata (agent-configuration parent) job)))
        (when (getf metadata :shell-logs)
          (setf result (copy-list result)
                (getf result :shell-logs) (getf metadata :shell-logs)
                (getf result :shell-logs-omitted) (getf metadata :shell-logs-omitted)))))
    (task-continuity--write target (list :state state :result result
                                    :ended-at (get-universal-time)) :require-absent t))
  nil)

(defmethod resource-tool-edit :before
    ((resource workspace-file-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Prevent child model edits to private continuity records."
  (declare (ignore tool arguments))
  (let ((path (workspace-file-resource-pathname resource)))
    (when (and (resource-context-child-agent-p context)
               (equal (pathname-name path) "continuity")
               (equal (pathname-type path) "sexp")
               (pathname-within-p path (merge-pathnames "tasks/"
                                                       (config :data-root (tool-context-configuration context)))))
      (error 'resource-access-denied :uri (resource-uri resource) :operation ':edit))))
