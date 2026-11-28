(in-package #:autolith)

;;;; -- Continuity Ownership and Recovery Boundaries --

(-> task-continuity-tests--fixture (function) t)
(defun task-continuity-tests--fixture (function)
  "Run FUNCTION with a durable orphan specification and ordinary task registry."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry
            (task-augment-tool-registry
             (make-default-tool-registry :configuration configuration)))
           (parent        (task-tests--primary-agent configuration "continuity-owner" registry))
           (owner         (conversation-identifier (agent-conversation parent)))
           (orchestrator  (tool-registry-runtime-binding registry 'task-orchestrator))
           (job           (make-instance 'tool-execution-job :identifier "orphan" :execution-identifier "orphan"
                                         :name "shell.run" :summary "Orphan fixture" :index 1 :owner-identifiers nil
                                         :orchestrator orchestrator :pool (task-orchestrator-execution-pool orchestrator)
                                         :payload nil :root-identifier owner :completion-owner-conversation owner
                                         :tool-name "shell.run" :detached-p t))
           (context
            (make-instance 'tool-context :configuration configuration :agent parent
                           :registry registry :worker nil :conversation
                           (agent-conversation parent) :call-id "continuity-test"))
           (path          (task-continuity-record-job job parent)))
      (unwind-protect (funcall function parent orchestrator context job path)
        (task-orchestrator-close orchestrator)))))

(-> task-continuity-tests--refused-p (function &key (:code (option keyword))) boolean)
(defun task-continuity-tests--refused-p (function &key code)
  "Return true when FUNCTION signals a typed continuity refusal with optional CODE."
  (handler-case (progn (funcall function) nil)
    (task-continuity-error (condition)
      (or (null code) (eq code (tool-failure-code condition))))))

(-> test-task-continuity-classification () null)
(defun test-task-continuity-classification ()
  "Classify lost work, safe declarations, missing checkpoints and abandonment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (let ((execution (session-job-execution-identifier job)))
       (test-assert
        (eq :dead-needing-decision
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "An opaque unfinished task needs a decision")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-revive context orchestrator execution :authorized-p t)))
        "An opaque tool job cannot be replayed")
       (test-assert
        (equal execution (getf (task-continuity--read path) :execution-id))
        "Durable identity links the original execution")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-declare parent orchestrator execution
                                             :image "missing-image" :repl "recovery")))
        "Missing worker images cannot become recovery evidence")
       (task-continuity-abandon parent orchestrator execution)
       (test-assert
        (eq :abandoned
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "Abandonment persists explicitly")
       (let ((fresh (task-orchestrator-create)))
         (unwind-protect
              (test-assert
               (eq :abandoned
                   (getf (first (task-continuity-inventory parent fresh)) :classification))
               "Classification survives a new runtime")
           (task-orchestrator-close fresh))))))
  (task-continuity-tests--paging)
  nil)

(-> test-task-continuity-ownership () null)
(defun test-task-continuity-ownership ()
  "Reject foreign owners and child replay authority; preserve owned live attachment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore path))
     (let* ((configuration (agent-configuration parent))
            (other         (agent-create :configuration configuration
                                         :provider      (make-instance 'model-provider)
                                         :tool-registry (agent-tool-registry parent) :worker nil))
            (child         (agent-create :configuration configuration :provider (make-instance 'model-provider)
                                         :worker nil :tool-registry (agent-tool-registry parent)
                                         :conversation (conversation-create configuration :identifier "continuity-child"
                                                                             :parent-identifier       "continuity-owner")))
            (execution     (session-job-execution-identifier job)))
       (test-assert (null (task-continuity-records other)) "Another conversation has no durable ownership")
       (test-assert (null (task-continuity-records child)) "A child cannot inspect its parent or siblings")
       (test-assert
        (task-continuity-tests--refused-p
          (lambda () (task-continuity-declare child orchestrator execution :image "image" :repl "recovery"))
          :code ':authority)
        "Children cannot grant checkpoint recovery authority")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-abandon child orchestrator execution)) :code ':authority)
        "Child abandonment is an authority refusal")
       (let ((*worker-host-tool-policy* (list :restricted-p t :allowlist nil)))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-continuity-dispatch context "resource" "read"
                                                :arguments (json-object "uri" "workspace:file"))) :code ':authority)
          "A withheld recovery capability is an authority refusal"))
       (let ((pool (task-orchestrator-execution-pool orchestrator)))
         (unwind-protect
              (progn
                (with-lock-held ((cl-jobpond::job-pool--lock pool))
                  (setf (gethash (job-identifier job) (cl-jobpond::job-pool--jobs pool)) job))
                (test-assert
                 (eq :live-owned-detached
                     (getf (first (task-continuity-inventory parent orchestrator)) :classification))
                 "Actual live ownership dominates orphan classification")
                (test-assert
                 (tool-result-success-p
                  (task-continuity-revive context orchestrator execution :authorized-p t))
                 "Owned attachment returns ordinary job.get rather than replaying")
                (test-assert (= 1 (length (task-orchestrator-list-jobs orchestrator)))
                             "Reattachment does not duplicate execution"))
           (with-lock-held ((cl-jobpond::job-pool--lock pool))
             (remhash (job-identifier job) (cl-jobpond::job-pool--jobs pool))))))))
  nil)

(-> task-continuity-tests--paging () null)
(defun task-continuity-tests--paging ()
  "Exercise whole-form pagination and bounded persisted metadata through the registry."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore job))
     (let* ((record (task-continuity--read path))
            (root   (uiop:pathname-parent-directory-pathname
                     (uiop:pathname-directory-pathname path)))
            (body   (make-string 20000 :initial-element #\x)))
       (loop for index below 12
             for execution = (format nil "page-~2,'0D" index)
             for target = (merge-pathnames (format nil "~A/continuity.sexp" execution) root)
             for copy = (copy-tree record)
             do (setf (getf copy :execution-id) execution
                        (getf copy :job-id) (copy-seq execution)
                        (getf copy :kind) ':task
                        (getf copy :specification) (list :task body))
                (ensure-directories-exist target)
                (task-continuity--write target copy))
       (task-continuity--write (merge-pathnames "result.sexp" (uiop:pathname-directory-pathname path))
                         (list :status ':success :output body
                             :duration-ms 321 :request-count 7
                             :usage                  '(("input_tokens" 42))
                             :worktree-artifact-path "patch.sexp"))
       (let* ((row    (task-continuity-classify (task-continuity--find parent "orphan")
                                                parent orchestrator))
              (result (getf row :result)))
         (test-assert (and (= 321 (getf result :duration-ms))
                           (= 7 (getf result :request-count))
                           (equal '(("input_tokens" 42)) (getf result :usage))
                           (equal "patch.sexp" (getf result :worktree-artifact-path)))
                      "Inventory retains reported timing, usage and artifact metadata")
         (test-assert (and (null (getf result :output))
                           (< (length (task--write-readable-sexp row)) 6000))
                      "Inventory omits full terminal result bodies"))
       (let ((*task-tool-content-limit* 8000)
             (offset                    0)
             (seen                      nil))
         (loop
           (let* ((response    (task-continuity-dispatch
                                context "job" "continuity"
                                :arguments (json-object "action" "list" "offset" offset "limit" 100)))
                  (content     (tool-result-content response))
                  (page        (tool-result-details response))
                  (*read-eval* nil))
             (test-assert (tool-result-success-p response) "Paged inventory succeeds at the registry boundary")
             (with-input-from-string (stream content)
               (test-assert (and (equal page (read stream)) (eq :eof (read stream nil :eof)))
                            "Every native page is exactly one complete readable form"))
             (test-assert (<= (length content) 8000) "Native page respects the character budget")
             (setf seen (append seen (mapcar (lambda (row) (getf row :execution-id))
                                            (getf page :entries))))
             (if (getf page :more-p)
                 (progn
                   (test-assert (> (getf page :next-offset) offset) "Pagination makes progress")
                   (setf offset (getf page :next-offset)))
                 (return))))
         (test-assert (and (= 13 (length seen))
                           (= 13 (length (remove-duplicates seen :test #'equal))))
                      "Character-bounded pages enumerate each durable execution once")
         (let ((page (task-continuity-page parent orchestrator :offset 999)))
           (test-assert (and (null (getf page :entries)) (= 13 (getf page :next-offset))
                             (not (getf page :more-p)))
                        "Beyond-end pages return a complete empty result at EOF"))
         (test-assert (= 1 (length (getf (task-continuity-page parent orchestrator :limit 1) :entries)))
                      "The requested count also bounds each page")
         (dolist (arguments '((:offset -1) (:limit 0) (:limit 101)))
           (test-assert
            (task-continuity-tests--refused-p
             (lambda () (apply #'task-continuity-page parent orchestrator arguments)))
            "Invalid page bounds fail before enumeration"))))))
  nil)

(-> test-task-continuity-channel-owner () null)
(defun test-task-continuity-channel-owner ()
  "Read durable tool results only for their exact recorded channel owner."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry      (task-augment-tool-registry
                           (make-default-tool-registry :configuration configuration)))
           (viewer        (agent-create :configuration configuration
                                        :provider      (make-instance 'task-test-provider)
                                        :tool-registry registry :worker nil))
           (owner         (conversation-identifier (agent-conversation viewer)))
           (directory     (merge-pathnames "owned-execution/"
                                           (task--artifact-group-root configuration owner)))
           (path          (merge-pathnames "continuity.sexp" directory))
           (record        (list :version 1 :kind ':tool :job-id "exec:123"
                                :execution-id "owned-execution" :root-conversation owner
                                :completion-owner-conversation owner :owners nil))
           (logs          (list (list :captures (list (list :reference "shell-log:owner/execution/log/output"))))))
      (let* ((child   (conversation-create configuration
                                           :identifier        "guard-child"
                                           :parent-identifier owner))
             (context (make-instance 'tool-context :configuration configuration
                                                   :conversation  child)))
        (test-assert (resource-context-child-agent-p context)
                     "Hosted child conversations retain private continuity edit restrictions"))
      (ensure-directories-exist path)
      (task-continuity--write path record)
      (task-continuity--write (merge-pathnames "terminal.sexp" directory)
                              (list :state  ':completed
                                    :result (list :status ':success :content "done" :shell-logs logs)))
      (multiple-value-bind (result result-path identity state)
          (task--durable-job-result viewer "owned-execution" "job.get")
        (test-assert (and (equal "done" (getf result :content))
                          (equal logs (getf result :shell-logs))
                          (equal owner (getf identity :completion-owner-conversation))
                          (eq state :completed) result-path)
                     "Owned tool result, state and shell logs survive durable lookup"))
      (setf (getf record :completion-owner-conversation) "foreign-channel")
      (task-continuity--write path record)
      (test-assert (null (task--durable-job-result viewer "owned-execution" "job.get"))
                   "A foreign recorded owner cannot be read from this channel's storage")))
  nil)
