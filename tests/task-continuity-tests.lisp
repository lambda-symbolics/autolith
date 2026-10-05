(in-package #:autolith)

;;;; -- Continuity Ownership and Recovery Boundaries --

(-> task-continuity-tests--fixture (function) t)
(defun task-continuity-tests--fixture (function)
  "Run FUNCTION with a durable orphan specification and ordinary task registry."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (task-augment-tool-registry
                      (make-default-tool-registry :configuration configuration)))
           (provider (make-instance 'task-test-provider :mode ':concurrent))
           (parent (agent-create :configuration configuration :provider provider
                                 :tool-registry registry :worker nil))
           (run-tool (tool-registry-find registry "task" "run"))
           (orchestrator (task-run-tool-orchestrator run-tool))
           (definition (task-agent-definition-create
                        :name "task" :description "continuity fixture"
                        :instructions "Return the result through yield.submit." :tools ':all))
           (job (task-tests--make-job
                 orchestrator :identifier "orphan" :parent-agent parent
                 :definition definition :detached-p t
                 :item (list :agent "task" :task "Return the result." :async t)))
           (context (make-instance 'tool-context :configuration configuration
                                   :agent parent :registry registry :worker nil
                                   :conversation (agent-conversation parent)
                                   :call-id "continuity-test"))
           (path (task-continuity-record-job job parent)))
      (unless (tool-registry-find registry "job" "continuity")
        (tool-registry-register
         registry (make-instance 'task-continuity-tool :namespace "job" :name "continuity"
                                 :description "Test continuity decisions."
                                 :parameters (task-continuity-parameters)
                                 :orchestrator orchestrator)))
      (unwind-protect
           (funcall function parent orchestrator context job path)
        (task-orchestrator-close orchestrator)))))

(-> task-continuity-tests--refused-p (function) boolean)
(defun task-continuity-tests--refused-p (function)
  "Return true when FUNCTION signals a typed continuity refusal."
  (handler-case (progn (funcall function) nil)
    (task-continuity-error () t)))

(-> test-task-continuity-classification () null)
(defun test-task-continuity-classification ()
  "Classify lost work, safe declarations, missing checkpoints and abandonment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore context))
     (let ((execution (session-job-execution-identifier job)))
       (test-assert
        (eq :dead-needing-decision
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "An opaque unfinished task needs a decision")
       (task-continuity-declare parent orchestrator execution
                                :safety "Read-only test fixture; no external effects.")
       (test-assert
        (eq :safe-restartable-specification
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "An explicit safe specification survives a reread")
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

(-> test-task-continuity-revive () null)
(defun test-task-continuity-revive ()
  "Revive a stored task through actual child execution once, with explicit authority."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (let ((execution (session-job-execution-identifier job)))
       (task-continuity-declare parent orchestrator execution
                                :safety "Deterministic read-only provider fixture.")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-revive context orchestrator execution)))
        "Replay requires a fresh explicit authorization")
       (test-assert
        (not (probe-file (merge-pathnames "revive.sexp"
                                          (uiop:pathname-directory-pathname path))))
        "An unauthorized attempt does not claim replay")
       (let* ((result (task-continuity-revive context orchestrator execution :authorized-p t))
              (revived (first (task-orchestrator-list-jobs orchestrator))))
         (test-assert (tool-result-success-p result) "The ordinary task.run pathway admits recovery")
         (test-assert revived "Recovery creates a real supervised child")
         (multiple-value-bind (snapshot terminal-p) (task-job-await revived 10)
           (test-assert (and terminal-p (eq :success (getf (getf snapshot :result) :status)))
                        (format nil "Recovered child yields successfully: ~S" snapshot)))
         (test-assert
          (not (equal execution (session-job-execution-identifier revived)))
          "A restarted execution receives a fresh identity")
         (let* ((row (task-continuity-classify
                      (task-continuity--find parent execution) parent orchestrator))
                (recovery (getf row :recovery-result))
                (link (first (getf recovery :new-executions))))
           (test-assert (and (eq :restart (getf row :decision))
                             (eq :dispatched (getf row :recovery-outcome)))
                        "Inventory exposes the durable recovery decision and dispatch outcome")
           (test-assert (and (equal execution (getf recovery :execution-id))
                             (equal (session-job-execution-identifier revived)
                                    (getf link :execution-id))
                             (equal (session-job-parent-call-id revived)
                                    (getf recovery :dispatch-call-id)))
                        "Recovery links original execution, correlated dispatch and new execution")
           (let ((fresh (task-orchestrator-create)))
             (unwind-protect
                  (test-assert
                   (equal recovery
                          (getf (task-continuity-classify
                                 (task-continuity--find parent execution) parent fresh)
                                :recovery-result))
                   "Recovery linkage survives without the original live job pool")
               (task-orchestrator-close fresh))))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-continuity-revive context orchestrator execution :authorized-p t)))
          "A consumed claim prevents duplicate side effects")
         (test-assert (= 1 (length (task-orchestrator-list-jobs orchestrator)))
                      "Duplicate recovery admits no additional child")))))
  nil)

(-> test-task-continuity-ownership () null)
(defun test-task-continuity-ownership ()
  "Reject foreign owners and child replay authority; preserve owned live attachment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore path))
     (let* ((configuration (agent-configuration parent))
            (other (agent-create :configuration configuration
                                 :provider (make-instance 'model-provider)
                                 :tool-registry (agent-tool-registry parent) :worker nil))
            (child (task-tests--child-viewer configuration job))
            (execution (session-job-execution-identifier job)))
       (test-assert (null (task-continuity-records other)) "Another conversation has no durable ownership")
       (test-assert (null (task-continuity-records child)) "A child cannot inspect its parent or siblings")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-declare child orchestrator execution :safety "read-only")))
        "Children cannot grant replay authority")
       (with-lock-held ((cl-jobpond::job-pool--lock (task-orchestrator-pool orchestrator)))
         (setf (gethash (job-identifier job)
                        (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))) job))
       (test-assert
        (eq :live-owned-detached
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "Actual live ownership dominates orphan classification")
       (test-assert
        (tool-result-success-p
         (task-continuity-revive context orchestrator execution :authorized-p t))
        "Owned attachment returns ordinary job.get rather than replaying")
       (test-assert (= 1 (length (task-orchestrator-list-jobs orchestrator)))
                    "Reattachment does not duplicate execution"))))
  nil)


(-> task-continuity-tests--paging () null)
(defun task-continuity-tests--paging ()
  "Exercise whole-form pagination and bounded persisted metadata through the registry."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore job))
     (let* ((record (task-continuity--read path))
            (root (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname path)))
            (body (make-string 20000 :initial-element #\x)))
       (loop for index below 12
             for execution = (format nil "page-~2,'0D" index)
             for target = (merge-pathnames (format nil "~A/continuity.sexp" execution) root)
             for copy = (copy-tree record)
             do (setf (getf copy :execution-id) execution
                      (getf copy :job-id) execution
                      (getf (getf copy :specification) :task) body)
                (ensure-directories-exist target)
                (task-continuity--write target copy))
       (task-continuity--write (merge-pathnames "result.sexp" (uiop:pathname-directory-pathname path))
                       (list :status ':success :output body :structured-output body
                             :duration-ms 321 :request-count 7
                             :usage '(("input_tokens" 42))
                             :worktree-artifact-path "patch.sexp"))
       (let* ((row (task-continuity-classify (task-continuity--find parent "orphan")
                                              parent orchestrator))
              (result (getf row :result)))
         (test-assert (and (= 321 (getf result :duration-ms))
                           (= 7 (getf result :request-count))
                           (equal '(("input_tokens" 42)) (getf result :usage))
                           (equal "patch.sexp" (getf result :worktree-artifact-path)))
                      "Inventory retains reported timing, usage and artifact metadata")
         (test-assert (and (null (getf result :output))
                           (null (getf result :structured-output))
                           (< (length (task--write-readable-sexp row)) 6000))
                      "Inventory omits full terminal result bodies"))
       (let ((*task-tool-content-limit* 8000)
             (offset 0)
             (seen nil))
         (loop
           (let* ((response (task-continuity-dispatch
                             context "job" "continuity"
                             (json-object "action" "list" "offset" offset "limit" 100)))
                  (content (tool-result-content response))
                  (page (tool-result-details response))
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
