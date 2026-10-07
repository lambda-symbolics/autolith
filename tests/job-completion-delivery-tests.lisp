(in-package #:autolith)

;;;; -- Durable Completion Acceptance --

(-> job-completion-tests--fixture (function) null)
(defun job-completion-tests--fixture (function)
  "Run FUNCTION with an actual asynchronous runtime and isolated durable storage."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((registry (task-augment-tool-registry
                      (make-default-tool-registry :configuration configuration)))
           (parent (agent-create :configuration configuration
                                 :provider (make-instance 'task-test-provider :mode ':concurrent)
                                 :tool-registry registry :worker nil))
           (runtime (task-completion--runtime parent)))
      (conversation-append-user-message (agent-conversation parent) "Start asynchronous work.")
      (unwind-protect (funcall function parent runtime configuration)
        (task-orchestrator-close runtime))))
  nil)

(-> job-completion-tests--start
    (agent task-orchestrator &key (:policy keyword) (:operation function)) tool-execution-job)
(defun job-completion-tests--start
    (parent runtime &key (policy ':continue) (operation (lambda () (tool-success "completed"))))
  "Submit and watch a real tool job, with an optional deterministic outcome."
  (let ((job (task-orchestrator-start-execution-job
              runtime parent :tool-name "completion.fixture" :summary "completion acceptance"
              :operation-function operation :detached-p t :completion-policy policy)))
    (task-completion-watch job parent)
    (session-job-await job 5)
    ;; Await publication's notification callback, not merely its terminal state.
    (cl-jobpond:completion-subscription-refresh
     (task-completion-service-subscription (task-completion--service parent)))
    job))

(-> job-completion-tests--fresh-agent (agent configuration) agent)
(defun job-completion-tests--fresh-agent (parent configuration)
  "Reconstruct the owning conversation with a fresh job runtime."
  (agent-create
   :configuration configuration
   :conversation (conversation-load (conversation-pathname (agent-conversation parent)))
   :provider (make-instance 'task-test-provider :mode ':concurrent)
   :tool-registry (task-augment-tool-registry
                   (make-default-tool-registry :configuration configuration))
   :worker nil))

(-> test-job-completion-durable-delivery () null)
(defun test-job-completion-durable-delivery ()
  "Persist outcomes before delivery and retain exactly-once receipts through compaction."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((job (job-completion-tests--start parent runtime))
            (identifier (session-job-execution-identifier job))
            (fresh (job-completion-tests--fresh-agent parent configuration)))
       (unwind-protect
            (progn
              (task-completion-restore fresh)
              (test-assert (= 1 (length (task-completion-pending fresh)))
                           "restart before delivery retains a completed outcome")
              (test-assert (= 1 (length (task-completion-deliver fresh)))
                           "restored outcome reaches durable conversation history")
              (test-assert (task-completion--delivered-p (agent-conversation fresh) identifier)
                           "delivery stores a stable execution receipt")
              (test-assert (null (task-completion-deliver fresh))
                           "repeated wakeups do not append duplicate messages")
              (conversation-append-summary (agent-conversation fresh) "Completed the asynchronous work.")
              (let ((again (job-completion-tests--fresh-agent fresh configuration)))
                (unwind-protect
                     (progn
                       (task-completion-restore again)
                       (test-assert (task-completion--delivered-p (agent-conversation again) identifier)
                                    "compaction retains completion receipt history")
                       (test-assert (null (task-completion-pending again))
                                    "restart after compaction does not redeliver acknowledged work"))
                  (task-orchestrator-close (task-completion--runtime again)))))
         (task-orchestrator-close (task-completion--runtime fresh))))))
  nil)

(-> test-job-completion-delivery-crash-points () null)
(defun test-job-completion-delivery-crash-points ()
  "Recover both sides of an uncertain delivery claim using durable append receipts."
  (dolist (append-first-p '(nil t))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let* ((job (job-completion-tests--start parent runtime))
              (subscription (task-completion-service-subscription (task-completion--service parent)))
              (message (first (cl-jobpond:completion-subscription-collect subscription :limit 1)))
              (identifier (session-job-execution-identifier job)))
         (when append-first-p
           (conversation-append-user-message
            (agent-conversation parent) "Completed before the crash."
            :automatic-p t :pending-input-identifier (task-completion--receipt identifier)))
         (test-assert (eq ':delivered (getf message :state)) "fixture has a durable unacknowledged claim")
         (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
           (unwind-protect
                (progn
                  (task-completion-restore fresh)
                  (test-assert (= (if append-first-p 0 1) (length (task-completion-deliver fresh)))
                               "receipt distinguishes delivered from undelivered after restart")
                  (test-assert (= 1 (count (task-completion--receipt identifier)
                                          (conversation-pending-input-identifiers (agent-conversation fresh))
                                          :test #'equal))
                               "exactly one durable receipt exists after reconciliation")
                  (test-assert (null (task-completion-pending fresh)) "claim reconciliation finishes delivery"))
             (task-orchestrator-close (task-completion--runtime fresh))))))))
  nil)

(-> test-job-completion-terminal-truth-and-reconstruction () null)
(defun test-job-completion-terminal-truth-and-reconstruction ()
  "Report failure, cancellation and interrupted execution truthfully without retrying work."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((failed (job-completion-tests--start
                     parent runtime :operation (lambda () (tool-failure "expected failure"))))
            (cancelled (job-completion-tests--start
                        parent runtime
                        :operation (lambda ()
                                     (error 'job-aborted :identifier "fixture"
                                            :reason ':cancelled :message "expected cancellation"))))
            (owner (conversation-identifier (agent-conversation parent)))
            (identifier (make-identifier))
            (directory (merge-pathnames
                        (format nil "~A/" identifier)
                        (task--artifact-group-root configuration owner))))
       (declare (ignore failed cancelled))
       (ensure-directories-exist (merge-pathnames "continuity.sexp" directory))
       (task-continuity--write
        (merge-pathnames "continuity.sexp" directory)
        (list :version 1 :job-id "interrupted" :execution-id identifier
              :root-conversation owner :owners nil :kind ':tool
              :created-at (get-universal-time) :detached-p t
              :completion-owner-conversation owner :completion-policy ':continue))
       (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
         (unwind-protect
              (progn
                (task-completion-restore fresh)
                (test-assert (equal '(:failure :cancelled :unknown)
                                    (mapcar (lambda (notice) (getf notice :outcome))
                                            (task-completion-pending fresh)))
                             "terminal outcomes preserve failure, cancellation and execution uncertainty")
                (test-assert (= 3 (length (task-completion-deliver fresh)))
                             "all terminal outcome classes reach the owner")
                (test-assert (null (task-orchestrator-list-jobs (task-completion--runtime fresh)))
                             "notification reconstruction never replays job side effects"))
           (task-orchestrator-close (task-completion--runtime fresh)))))))
  nil)

(-> test-job-completion-event-wakeup () null)
(defun test-job-completion-event-wakeup ()
  "Receive a publication wakeup without invoking job polling or a model callback."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (declare (ignore configuration))
     (let ((lock (make-lock "Completion wakeup test"))
           (condition (make-condition-variable))
           (woken-p nil))
       (task-completion-connect
        parent (lambda ()
                 (with-lock-held (lock)
                   (setf woken-p t)
                   (condition-notify condition))))
       (let ((job (task-orchestrator-start-execution-job
                   runtime parent :tool-name "completion.fixture" :summary "event wakeup"
                   :operation-function (lambda () (tool-success "event delivered"))
                   :detached-p t)))
         (task-completion-watch job parent)
         (with-lock-held (lock)
           (unless woken-p (condition-wait condition lock :timeout 5)))
         (test-assert woken-p "completion publication signals a waiting controller")
         (test-assert (= 1 (length (task-completion-pending parent)))
                      "wakeup follows durable pending event publication")
         (task-completion-deliver parent)
         (task-completion-disconnect parent)))))
  nil)


(-> test-job-completion-history-rollover () null)
(defun test-job-completion-history-rollover ()
  "Deliver more than the mailbox history bound while preserving durable deduplication."
  (let ((*task-completion-capacity* 2)
        (*task-completion-history-limit* 4))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let ((jobs nil))
         (dotimes (index 40)
           (declare (ignore index))
           (push (job-completion-tests--start parent runtime) jobs)
           (task-completion-deliver parent))
         (let ((service (task-completion--service parent)))
           (test-assert (null (task-completion--messages service))
                        "receipt-backed pruning frees the bounded mailbox history"))
         (dolist (job jobs) (task-completion-watch job parent))
         (test-assert (null (task-completion-pending parent))
                      "late duplicate watches exclude already receipted executions")
         (test-assert (= 40 (length (conversation-pending-input-identifiers (agent-conversation parent))))
                      "every execution retains its durable deduplication proof")
         (conversation-append-summary (agent-conversation parent) "Forty jobs completed.")
         (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
           (unwind-protect
                (progn
                  (task-completion-restore fresh)
                  (test-assert (null (task-completion-pending fresh))
                               "pruned notification history reconstructs without redelivery"))
             (task-orchestrator-close (task-completion--runtime fresh))))))))
  nil)

(-> test-job-completion-capacity-recovery () null)
(defun test-job-completion-capacity-recovery ()
  "Drain an overflowing durable backlog in bounded batches without repeating effects."
  (let ((*task-completion-capacity* 2)
        (*task-completion-history-limit* 4))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let ((executions 0))
         (dotimes (index 6)
           (declare (ignore index))
           (let ((job (task-orchestrator-start-execution-job
                       runtime parent :tool-name "completion.fixture" :summary "overflow"
                       :operation-function (lambda () (incf executions) (tool-success "done"))
                       :detached-p t)))
             (task-completion-watch job parent)
             (session-job-await job 5)))
         (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
           (unwind-protect
                (progn
                  (task-completion-restore fresh)
                  (test-assert (= 2 (length (task-completion-pending fresh)))
                               "reconstruction retains only the bounded live notification window")
                  (let ((delivered (loop repeat 4 sum (length (task-completion-deliver fresh)))))
                    (test-assert (= 6 delivered) "capacity pressure cannot strand terminal outcomes"))
                  (test-assert (= 6 executions) "notification recovery never repeats job effects")
                  (test-assert (null (task-completion-pending fresh)) "the complete backlog is receipted"))
             (task-orchestrator-close (task-completion--runtime fresh))))))))
  nil)

(-> test-job-completion-headless-owner () null)
(defun test-job-completion-headless-owner ()
  "Retain a child's detached outcome for authorized inspection without redirecting its owner."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((definition (make-instance 'task-agent-definition :name "fixture" :description "fixture"
                                                            :instructions "fixture" :source ':test))
            (owner-job (task-tests--make-job runtime :identifier "owner" :parent-agent parent
                                            :definition definition :item (list :task "fixture")))
            (child (make-instance 'task-child-agent
                                 :configuration configuration :conversation (conversation-create configuration)
                                 :provider (agent-provider parent) :tool-registry (agent-tool-registry parent)
                                 :worker nil :definition definition :identity nil :depth 1
                                 :completion (make-instance 'task-completion) :orchestrator runtime :job owner-job))
            (job (job-completion-tests--start child runtime)))
       (task-completion-restore parent)
       (test-assert (null (task-completion-pending parent))
                    "a grandchild outcome never enters the primary conversation implicitly")
       (let ((record (find (session-job-execution-identifier job)
                           (task-continuity-inventory parent runtime)
                           :key (lambda (entry) (getf entry :execution-id)) :test #'equal)))
         (test-assert (eq ':inspection-only (getf record :completion-route))
                      "authorized continuity inspection identifies an inactive conversation route")
         (test-assert (equal (conversation-identifier (agent-conversation child))
                             (getf record :completion-owner-conversation))
                      "inspection preserves the exact child owner")
         (test-assert (and (eq ':continue (getf record :completion-policy)) (getf record :result))
                      "headless completion policy and terminal artifacts remain inspectable")))))
  nil)
