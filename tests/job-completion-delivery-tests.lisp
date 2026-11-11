(in-package #:autolith)

;;;; -- Durable Completion Acceptance --

(-> job-completion-tests--fixture (function) null)
(defun job-completion-tests--fixture (function)
  "Run FUNCTION with an actual asynchronous runtime and isolated durable storage."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((registry (task-augment-tool-registry
                      (make-default-tool-registry :configuration configuration)))
           (parent   (agent-create :configuration configuration
                                   :provider      (make-instance 'task-test-provider :mode ':concurrent)
                                   :tool-registry registry :worker nil))
           (runtime  (task-completion--runtime parent)))
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
   :conversation  (conversation-load (conversation-pathname (agent-conversation parent)))
   :provider      (make-instance 'task-test-provider :mode ':concurrent)
   :tool-registry (task-augment-tool-registry
                   (make-default-tool-registry :configuration configuration))
   :worker        nil))

(-> test-job-completion-checkpoint-resume () null)
(defun test-job-completion-checkpoint-resume ()
  "Restore durable delivery and admit new work after checkpoint quiescence."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (declare (ignore configuration))
     (let* ((first-job (job-completion-tests--start parent runtime))
            (first-id  (session-job-execution-identifier first-job)))
       (test-assert (task-orchestrator-close runtime)
                    "checkpoint quiescence closes the runtime")
       (task-orchestrator-refresh runtime :reopen-p t)
       (task-completion-restore parent)
       (test-assert (= 1 (length (task-completion-deliver parent)))
                    "resumption restores the undelivered durable outcome")
       (test-assert (task-completion--delivered-p (agent-conversation parent) first-id)
                    "the restored outcome has a durable delivery receipt")
       (let* ((second-job (job-completion-tests--start parent runtime))
              (second-id  (session-job-execution-identifier second-job)))
         (test-assert (= 1 (length (task-completion-deliver parent)))
                      "new work can register and deliver its completion after resumption")
         (test-assert (task-completion--delivered-p (agent-conversation parent) second-id)
                      "the new outcome has its own durable receipt")
         (test-assert (null (task-completion-deliver parent))
                      "resuming the runtime does not duplicate delivered outcomes"))))))

(-> test-job-completion-durable-delivery () null)
(defun test-job-completion-durable-delivery ()
  "Persist outcomes before delivery and retain exactly-once receipts through compaction."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((job        (job-completion-tests--start
                         parent runtime
                         :operation (lambda ()
                                      (tool-success (make-string 1200 :initial-element #\x)))))
            (identifier (session-job-execution-identifier job))
            (fresh      (job-completion-tests--fresh-agent parent configuration)))
       (unwind-protect
            (progn
              (task-completion-restore fresh)
              (test-assert (= 1 (length (task-completion-pending fresh)))
                           "restart before delivery retains a completed outcome")
              (test-assert (= 1 (length (task-completion-deliver fresh)))
                           "restored outcome reaches durable conversation history")
              (test-assert (task-completion--delivered-p (agent-conversation fresh) identifier)
                           "delivery stores a stable execution receipt")
              (let* ((conversation (agent-conversation fresh))
                     (record       (first (conversation-records-newest conversation 1)))
                     (properties   (rest record))
                     (compact      (application-tests--ui-application :columns 100))
                     (expanded     (application-tests--ui-application
                                    :columns 100 :compact-view-p nil))
                     (entry        (conversation-record-entry compact record))
                     (text         (test-terminal-row-text entry)))
                (test-assert
                 (and (getf properties :automatic-p)
                      (equal identifier
                             (getf (getf properties :job-completion) :execution-id)))
                 "completion delivery persists structured provenance for transcript replay")
                (test-assert
                 (and (eq (terminal-span-style (first entry)) ':notice)
                      (< (length text) 200)
                      (not (find #\Newline text)))
                 "compact completion output is one automated status row without its payload")
                (test-assert
                 (< (length (test-terminal-row-text
                             (conversation-record-entry expanded record))) 650)
                 "expanded completion output bounds its readable summary")
                (test-assert
                 (equal (conversation-input-history conversation)
                        '("Start asynchronous work."))
                 "automatic completion input is not editable user history")
                (let ((older (copy-tree record)))
                  (remf (rest older) :job-completion)
                  (test-assert
                   (eq (terminal-span-style
                        (first (conversation-record-entry compact older))) ':notice)
                   "older completion receipts retain automated attribution without body parsing"))
                (let ((user (copy-tree record)))
                  (remf (rest user) :automatic-p)
                  (remf (rest user) :pending-input-identifier)
                  (remf (rest user) :job-completion)
                  (test-assert
                   (eq (terminal-span-style
                        (first (conversation-record-entry compact user))) ':user)
                   "a real user message with the same text retains user attribution")))
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
       (let* ((job          (job-completion-tests--start parent runtime))
              (subscription (task-completion-service-subscription (task-completion--service parent)))
              (message      (first (cl-jobpond:completion-subscription-collect subscription :limit 1)))
              (identifier   (session-job-execution-identifier job)))
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
     (let* ((failed     (job-completion-tests--start
                         parent runtime :operation (lambda () (tool-failure "expected failure"))))
            (cancelled  (job-completion-tests--start
                         parent runtime
                         :operation (lambda ()
                                      (error 'job-aborted :identifier "fixture"
                                             :reason ':cancelled :message "expected cancellation"))))
            (owner      (conversation-identifier (agent-conversation parent)))
            (identifier (make-identifier))
            (directory  (merge-pathnames
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

(-> test-job-completion-outstanding-index () null)
(defun test-job-completion-outstanding-index ()
  "Restore only indexed undelivered jobs, rebuilding a missing index from job history."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((delivered (session-job-execution-identifier
                        (job-completion-tests--start parent runtime)))
            (owner     (conversation-identifier (agent-conversation parent)))
            (root      (task--artifact-group-root configuration owner))
            (index     (task-completion--index-path root owner))
            (unindexed (make-identifier)))
       (task-completion-deliver parent)
       (flet ((restored-executions (agent)
                (task-completion-restore agent)
                (mapcar (lambda (notice) (getf notice :execution-id))
                        (task-completion-pending agent))))
         (let* ((fresh   (job-completion-tests--fresh-agent parent configuration))
                (pending (session-job-execution-identifier
                          (job-completion-tests--start fresh (task-completion--runtime fresh)))))
           (task-continuity--write
            (merge-pathnames (format nil "~A/continuity.sexp" unindexed) root)
            (list :version 1 :job-id "unindexed" :execution-id unindexed
                  :root-conversation owner :owners nil :kind ':tool
                  :created-at (get-universal-time) :detached-p t
                  :completion-owner-conversation owner :completion-policy ':continue))
           (task-orchestrator-close (task-completion--runtime fresh))
           (let ((again (job-completion-tests--fresh-agent fresh configuration)))
             (unwind-protect
                  (progn
                    (test-assert (equal (list pending) (restored-executions again))
                                 "a complete index restores its jobs without reading other history")
                    (test-assert (equal (list pending)
                                        (getf (task-completion--index-read index) :execution-ids))
                                 "restore drops delivered jobs from the index"))
               (task-orchestrator-close (task-completion--runtime again))))
           (delete-file index)
           (let ((rebuilt (job-completion-tests--fresh-agent fresh configuration)))
             (unwind-protect
                  (progn
                    (test-assert (null (set-exclusive-or (list pending unindexed)
                                                         (restored-executions rebuilt)
                                                         :test #'equal))
                                 "a missing index is rebuilt from every job's continuity")
                    (test-assert (not (member delivered
                                              (getf (task-completion--index-read index) :execution-ids)
                                              :test #'equal))
                                 "the rebuilt index omits delivered jobs"))
               (task-orchestrator-close (task-completion--runtime rebuilt)))))))))
  nil)

(-> test-job-completion-event-wakeup () null)
(defun test-job-completion-event-wakeup ()
  "Receive a publication wakeup without invoking job polling or a model callback."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (declare (ignore configuration))
     (let ((lock      (make-lock "Completion wakeup test"))
           (condition (make-condition-variable))
           (woken-p   nil))
       (task-completion-connect
        parent (lambda ()
                 (with-lock-held (lock)
                   (setf woken-p t)
                   (condition-notify condition))))
       (let ((job (task-orchestrator-start-execution-job
                   runtime parent :tool-name "completion.fixture" :summary "event wakeup"
                   :operation-function (lambda () (tool-success "event delivered"))
                   :detached-p         t)))
         (task-completion-watch job parent)
         (let ((deadline (+ (get-internal-real-time)
                            (* 5 internal-time-units-per-second))))
           (with-lock-held (lock)
             (loop until woken-p
                   for remaining = (- deadline (get-internal-real-time))
                   while (plusp remaining)
                   do (condition-wait condition lock
                                      :timeout (/ remaining internal-time-units-per-second)))))
         (test-assert woken-p "completion publication signals a waiting controller")
         (test-assert (= 1 (length (task-completion-pending parent)))
                      "wakeup follows durable pending event publication")
         (task-completion-deliver parent)
         (task-completion-disconnect parent)))))
  nil)

(-> test-job-completion-history-rollover () null)
(defun test-job-completion-history-rollover ()
  "Deliver more than the mailbox history bound while preserving durable deduplication."
  (let ((*task-completion-capacity*      2)
        (*task-completion-history-limit* 4))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let ((jobs nil))
         (loop repeat 40
               do (push (job-completion-tests--start parent runtime) jobs)
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
  (let ((*task-completion-capacity*      2)
        (*task-completion-history-limit* 4))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let ((executions 0))
         (loop repeat 6
               do (let ((job (task-orchestrator-start-execution-job
                              runtime parent :tool-name "completion.fixture" :summary "overflow"
                              :operation-function (lambda () (incf executions) (tool-success "done"))
                              :detached-p         t)))
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
            (owner-job  (task-tests--make-job runtime :identifier "owner" :parent-agent parent
                                             :definition definition :item (list :task "fixture")))
            (child      (make-instance 'task-child-agent
                                      :configuration configuration :conversation (conversation-create configuration)
                                      :provider (agent-provider parent) :tool-registry (agent-tool-registry parent)
                                      :worker nil :definition definition :identity nil :depth 1
                                      :completion (make-instance 'task-completion) :orchestrator runtime :job owner-job))
            (job        (job-completion-tests--start child runtime)))
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

(-> job-completion-tests--held-cancellation (function) null)
(defun job-completion-tests--held-cancellation (function)
  "Share real held jobs, controller, bounded entry and separate cleanup across cancellation tests."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (let* ((*job-cancellation-warning-delay-seconds* 1)
            (application                              (make-instance 'application :configuration configuration
                                                       :conversation (agent-conversation parent) :provider (agent-provider parent)
                                                       :tool-registry (agent-tool-registry parent) :worker nil :agent parent
                                                       :ui                                       (terminal-ui-create :terminal (make-instance 'recording-terminal))))
            (controller                               (make-instance 'application-input-controller
                                                       :application application :main-thread (current-thread)))
            (entered                                  (sb-thread:make-semaphore :count 0))
            (release (sb-thread:make-semaphore :count 0)) (jobs nil))
       (setf (application-input-controller application) controller
             (application-localgroup-session application) (make-instance 'localgroup-session :application application))
       (labels ((hold (&optional quick-p)
                  (flet ((wait () (sb-thread:signal-semaphore entered)
                                  (sb-thread:wait-on-semaphore release :timeout 20)))
                    (if quick-p (wait) (sb-sys:without-interrupts (wait))))
                  (tool-success "released"))

                (start (&key owners task-p quick-p (policy ':notify))
                  (let ((job nil))
                     (let ((submit (symbol-function 'job-pool-submit-batch)))
                       (test-call-with-function-replacements
                        (list (list 'task-parent-owner-identifiers (lambda (owner) (declare (ignore owner)) owners))
                              (list 'job-pool-submit-batch
                                    (lambda (pool entries)
                                      (funcall submit pool
                                               (mapcar (lambda (entry)
                                                         (list* :parent-job
                                                                (find (first owners) jobs :key #'job-identifier :test #'equal)
                                                                entry))
                                                       entries)))))
                        (lambda ()
                          (setf job
                                (if task-p
                                    (first (task-orchestrator-start-jobs runtime parent
                                            (list (list :definition (make-instance 'task-agent-definition
                                                                     :name "held" :description "held" :instructions "held" :source ':test)
                                                        :item (list :agent "held" :task "held") :detached t))))
                                    (task-orchestrator-start-execution-job runtime parent
                                     :tool-name "cancellation.fixture" :summary "held cancellation"
                                      :operation-function (lambda () (hold quick-p)) :detached-p t :completion-policy policy))))))
                    (push job jobs)
                    (unless task-p (task-completion-watch job parent))
                    (test-assert (sb-thread:wait-on-semaphore entered :timeout 5) "real job body entered")
                    (values job (lambda ()
                                  (unless (job-terminal-p job) (sb-thread:signal-semaphore release))
                                  (session-job-await job 5))))))
         (unwind-protect
              (test-call-with-function-replacements
               (list (list 'task-job--run (lambda (job) (declare (ignore job)) (hold) nil)))
               (lambda () (funcall function parent controller #'start)))
           (dolist (job jobs) (declare (ignore job)) (sb-thread:signal-semaphore release))
           (dolist (job jobs)
             (test-assert (nth-value 1 (session-job-await job 5)) "cleanup joins released worker")))))))
  nil)

(-> test-job-cancellation-warning-once () null)
(defun test-job-cancellation-warning-once ()
  "Own threshold, identity and once-only publication, including quick and native cancellation."
  (job-completion-tests--held-cancellation
   (lambda (parent controller start)
     (declare (ignore controller))
     (dolist (mode '(:quick :accepted :native))
       (multiple-value-bind (job release) (funcall start :quick-p (eq mode ':quick))
         (if (eq mode ':native)
             (test-assert (job-cancel job :reason ':native :cascade-p nil) "native cancellation accepted")
             (test-assert (session-job-cancel job ':fixture) "session cancellation accepted"))
         (if (eq mode ':quick)
             (progn (test-assert (nth-value 1 (session-job-await job 5)) "quick cancellation terminates")
                    (test-assert (null (task-cancellation-maintain parent)) "quick cancellation never warns"))
             (progn
               (when (eq mode ':native) (task-cancellation-maintain parent))
               (let* ((record (session-job-cancellation-observation job))
                      (due    (+ (getf record :at) internal-time-units-per-second)))
                 (test-assert (eq (not (null (getf record :observed-p))) (eq mode ':native)) "native age is an observed lower bound")
                 (test-assert (null (task-cancellation-maintain parent :now (1- due))) "no premature warning")
                 (let ((notices (task-cancellation-maintain parent :now due)))
                   (test-assert (and (= 1 (length notices))
                                     (equal (session-job-identifier job) (getf (first notices) :job-id))
                                     (eq (if (eq mode ':native) ':native ':fixture) (getf (first notices) :reason)))
                                "one due warning retains identity and reason"))
                 (test-assert (not (session-job-cancel job ':again)) "repeated request rejected")
                 (test-assert (null (task-cancellation-maintain parent :now (1+ due))) "publication claimed once")
                 (test-assert (= 1 (length (task-completion-deliver parent))) "ordinary boundary delivers warning")
                 (test-assert (null (task-completion-deliver parent)) "model delivery receipted once"))))
         (funcall release)
         (test-assert (= 1 (length (task-completion-deliver parent))) "real terminal outcome is separate from warning")))
     (let ((job (job-completion-tests--start parent (task-completion--runtime parent))))
       (test-assert (and (not (session-job-cancel job ':late))
                         (null (session-job-cancellation-observation job))) "terminal rejection creates no acceptance record"))))
  nil)

(-> test-job-cancellation-warning-controller () null)
(defun test-job-cancellation-warning-controller ()
  "Inference-only restrictions permit real terminal maintenance without an automatic turn."
  (dolist (slot '(pending-persistence-enabled-p follow-up-edit-index))
    (job-completion-tests--held-cancellation
     (lambda (parent controller start)
       (declare (ignore parent))
       (let ((job (funcall start :policy ':continue)) (presented nil))
         (session-job-cancel job ':controller)
         (setf (getf (session-job-cancellation-observation job) :at) (- (get-internal-real-time) internal-time-units-per-second)
               (slot-value controller slot) (if (eq slot 'follow-up-edit-index) 0 nil))
         (test-call-with-function-replacements
           (list (list 'application-present (lambda (owner entry &key identifier) (declare (ignore owner identifier)) (push entry presented))))
          (lambda ()
            (let ((ticket (second (application-job-completions--take-maintenance controller))))
              (test-assert (getf ticket :cancellation-scan-p) "inference guard permits terminal ticket")
              (application-job-completions-maintain controller ticket)
              (test-assert (= 1 (length presented)) "inference guard permits presentation")
              (test-assert (null (application-job-completions--take-work controller)) "inference guard denies turn"))))))))
  nil)

(-> test-job-cancellation-presentation-boundaries () null)
(defun test-job-cancellation-presentation-boundaries ()
  "Own ticket and actual-output guards, with synchronized closure after handler admission."
  (dolist (boundary '(stopping-p failure localgroup-handoff-p queued-work-paused-p session-pause))
    (job-completion-tests--held-cancellation
     (lambda (parent controller start)
       (let* ((job (funcall start)) (application (application-input-controller-application controller))
              (admitted (sb-thread:make-semaphore :count 0)) (proceed (sb-thread:make-semaphore :count 0))
              (scan (symbol-function 'task-cancellation-maintain))
              (presented nil) (handler nil) (failure nil) (gate-p t))
         (labels ((close-boundary (closed-p)
                    (if (eq boundary 'session-pause)
                        (if closed-p (application-localgroup-pause application) (application-localgroup-resume application))
                        (with-lock-held ((application-input-controller-lock controller))
                          (setf (slot-value controller boundary)
                                (when closed-p (if (eq boundary 'failure) (make-condition 'simple-error) t)))))))
           (session-job-cancel job ':boundary)
           (setf (getf (session-job-cancellation-observation job) :at) (- (get-internal-real-time) internal-time-units-per-second))
           (let ((ticket (second (application-job-completions--take-maintenance controller))))
             (test-call-with-function-replacements
              (list (list 'task-cancellation-maintain
                          (lambda (owner &key (now (get-internal-real-time)))
                            (prog1 (funcall scan owner :now now)
                              (when gate-p (setf gate-p nil) (sb-thread:signal-semaphore admitted)
                                    (sb-thread:wait-on-semaphore proceed :timeout 5)))))
                    (list 'application-present (lambda (owner entry &key identifier) (declare (ignore owner identifier)) (push entry presented))))
              (lambda ()
                (close-boundary t)
                (test-assert (null (application-job-completions--take-maintenance controller)) "closed guard issues no ticket")
                (application-job-completions-maintain controller ticket)
                (test-assert (null (task-completion-pending parent)) "stale ticket cannot claim warning")
                (close-boundary nil)
                (unwind-protect
                     (progn
                       (setf handler (sb-thread:make-thread
                                      (lambda ()
                                        (let ((*job-cancellation-warning-delay-seconds* 1))
                                          (handler-case (application-job-completions-maintain controller ticket)
                                            (error (condition) (setf failure condition)))) ':joined)))
                       (test-assert (sb-thread:wait-on-semaphore admitted :timeout 5) "admission and scan finished")
                       (close-boundary t)
                       (sb-thread:signal-semaphore proceed)
                       (test-assert (eq ':joined (sb-thread:join-thread handler :timeout 5 :default ':timeout)) "closed handler joins"))
                  (sb-thread:signal-semaphore proceed)
                  (when handler (sb-thread:join-thread handler :timeout 5 :default ':timeout)))
                (test-assert (and (null failure) (null presented) (zerop (task-cancellation-scan-wait parent)))
                             "post-admission closure defers output and retains immediate retry")
                (close-boundary nil)
                (application-job-completions-maintain controller ticket)
                (application-job-completions-maintain controller ticket)
                (test-assert (= 1 (length presented)) "reopening presents exactly once")
                (test-assert (null (application-job-completions--take-work controller)) "notify reopening grants no inference")))))))))
  nil)

(-> test-job-cancellation-selected-delivery-presentation () null)
(defun test-job-cancellation-selected-delivery-presentation ()
  "Own selected-delivery publication, interrupted output and independent retained model receipt."
  (job-completion-tests--held-cancellation
   (lambda (parent controller start)
     (let* ((job (funcall start)) (other (job-completion-tests--start parent (session-job-orchestrator job) :policy ':notify))
            (presented nil) (ticket (list :conversation (conversation-identifier (agent-conversation parent)) :cancellation-scan-p t)))
       (session-job-cancel job ':selected)
       (setf (getf (session-job-cancellation-observation job) :at) (- (get-internal-real-time) internal-time-units-per-second))
       (test-assert (= 1 (length (task-completion-deliver parent :identifiers (list (session-job-execution-identifier other)))))
                    "selected unrelated completion delivers alone")
       (test-assert (getf (getf (session-job-cancellation-observation job) :warning) :presentation-pending-p)
                    "selected delivery publishes retained terminal warning")
       (test-call-with-function-replacements
         (list (list 'application-present (lambda (owner entry &key identifier) (declare (ignore owner entry identifier)) (throw 'defer nil))))
        (lambda () (catch 'defer (application-job-completions-maintain controller ticket))))
       (test-assert (zerop (task-cancellation-scan-wait parent)) "interrupted output retains retry")
       (test-call-with-function-replacements
         (list (list 'application-present (lambda (owner entry &key identifier) (declare (ignore owner identifier)) (push entry presented))))
        (lambda ()
          (application-job-completions-maintain controller (second (application-job-completions--take-maintenance controller)))
          (test-assert (= 1 (length presented)) "maintenance presents other-caller publication")
          (test-assert (= 1 (length (task-completion-deliver parent))) "presentation retained separate model receipt")
          (application-job-completions-maintain controller ticket)
          (test-assert (= 1 (length presented)) "model delivery cannot repeat presentation"))))))
  nil)

(-> test-job-cancellation-terminal-replay () null)
(defun test-job-cancellation-terminal-replay ()
  "Render each warning once whether terminal output or durable replay happens first."
  (dolist (model-first-p '(nil t))
    (job-completion-tests--held-cancellation
     (lambda (parent controller start)
       (let* ((job         (funcall start))
              (application (application-input-controller-application controller))
              (ui          (application-ui application))
              (ticket      (list :conversation
                                 (conversation-identifier (agent-conversation parent))
                                 :cancellation-scan-p t)))
         (session-job-cancel job ':replay)
         (setf (getf (session-job-cancellation-observation job) :at)
               (- (get-internal-real-time) internal-time-units-per-second))
         (unless model-first-p
           (application-job-completions-maintain controller ticket))
         (test-assert (= 1 (length (task-completion-deliver parent)))
                      "warning reaches model history independently of presentation")
         (let* ((record (first (conversation-records-newest (agent-conversation parent) 1)))
                (entry  (conversation-record-entry application record))
                (before (hash-table-count (terminal-ui-finalized-identifiers ui))))
           (application--present-conversation-records application (list record))
           (test-assert (= (hash-table-count (terminal-ui-finalized-identifiers ui))
                           (+ before (if model-first-p 1 0)))
                        "durable replay shares the immediate warning identity")
           (application-job-completions-maintain controller ticket)
           (test-assert (not (application--present-conversation-record application record))
                        "single-record replay also deduplicates the warning")
           (test-assert (= 1 (hash-table-count (terminal-ui-finalized-identifiers ui)))
                        "one terminal entry across both delivery paths")
           (test-assert (null (task-completion-service-cancellation-notices
                               (task-completion--service parent)))
                        "already rendered warnings clear their terminal retry flag")
           (let ((fresh (terminal-ui-create :terminal (make-instance 'recording-terminal))))
             (setf (application-ui application) fresh)
             (test-assert (and entry (application--present-conversation-record application record))
                          "a new terminal can replay the warning from durable history")))))))
  nil)

(-> test-job-cancellation-rejected-root-descendants () null)
(defun test-job-cancellation-rejected-root-descendants ()
  "Keep exact subtree selection, observations and once-only publication at both sites."
  (dolist (cross-pool-p '(nil t))
    (let ((publications (make-hash-table :test #'eq))
          (lock         (make-lock "cancellation publication counts"))
          (task-record  (symbol-function 'task-job--terminal-record))
          (tool-record  (symbol-function 'tool-execution-job--terminal-record)))
      (flet ((record-publication (job)
               (with-lock-held (lock)
                 (incf (gethash job publications 0)))))
        (test-call-with-function-replacements
         (list (list 'task-job--terminal-record
                     (lambda (job &rest arguments)
                       (record-publication job)
                       (apply task-record job arguments)))
               (list 'tool-execution-job--terminal-record
                     (lambda (job &rest arguments)
                       (record-publication job)
                       (apply tool-record job arguments))))
         (lambda ()
           (job-completion-tests--held-cancellation
            (lambda (parent controller start)
              (declare (ignore controller))
              (let* ((task-root  (when cross-pool-p (funcall start :task-p t)))
                     (owners     (when task-root (list (job-identifier task-root))))
                     (root       (funcall start :owners owners))
                     (descendant (funcall start :owners (cons (job-identifier root) owners)))
                     (expected   (sort (mapcar #'session-job-identifier
                                               (if task-root (list root descendant) (list descendant)))
                                       #'string<)))
                (job-cancel root :reason ':native-first :cascade-p nil)
                (task-cancellation-maintain parent)
                (let* ((first-record (session-job-cancellation-observation root))
                       (snapshot     (copy-list first-record)))
                  (multiple-value-bind (accepted descendants)
                      (session-job-cancel (or task-root root) ':descendant-accepted)
                    ;; JOB-CANCEL returns selected live descendants, including
                    ;; already cancelling jobs, rather than accepted transitions.
                    (test-assert (and (eq accepted cross-pool-p) (equal descendants expected))
                                 "the exact linked live subtree is returned"))
                  (test-assert (and (getf first-record :observed-p)
                                    (eq first-record (session-job-cancellation-observation root))
                                    (equal snapshot first-record)
                                    (eq ':native-first (job-cancellation-reason root)))
                               "the selected cancelling root preserves its first reason and observation")
                  (let* ((record   (session-job-cancellation-observation descendant))
                         (snapshot (copy-list record)))
                    (test-assert (and record (not (getf record :observed-p))
                                      (eq ':descendant-accepted (getf record :reason)))
                                 "the newly accepted descendant records direct origin and reason")
                    (multiple-value-bind (accepted descendants)
                        (session-job-cancel (or task-root root) ':duplicate)
                      (test-assert (and (null accepted) (equal descendants expected)
                                        (eq record (session-job-cancellation-observation descendant))
                                        (equal snapshot record)
                                        (eq ':descendant-accepted (job-cancellation-reason descendant)))
                                   "reselection changes no accepted transition or observation")))
                  (let ((notices (task-cancellation-maintain
                                  parent :now (+ (get-internal-real-time) internal-time-units-per-second))))
                    (test-assert
                     (and (find (session-job-identifier descendant) notices
                                :key (lambda (notice) (getf notice :job-id)) :test #'equal)
                          (every (lambda (notice)
                                   (equal (conversation-identifier (agent-conversation parent))
                                          (getf notice :owner-conversation)))
                                 notices))
                     "accepted descendants warn their exact owner")))))))))
      ;; The held fixture joins every real worker before restoring these hooks.
      (test-assert (and (= (hash-table-count publications) (if cross-pool-p 3 2))
                        (loop for count being the hash-values of publications always (= count 1)))
                   "each selected job publishes through its terminal hook exactly once")))
  nil)

(-> test-job-completion-continuation-selection () null)
(defun test-job-completion-continuation-selection ()
  "Select the warning prompt only when this turn actually delivers a warning."
  (job-completion-tests--held-cancellation
   (lambda (parent controller start)
     (declare (ignore start))
     (let* ((*application-job-completion-continuation-prompt*   "completion branch")
            (*application-job-cancellation-continuation-prompt* "warning branch")
            (deliver                                            #'task-completion-deliver)
            (raced-warning                                      nil)
            (inputs                                             nil))
       (test-call-with-function-replacements
        (list (list 'application--run-turn
                    (lambda (owner input &rest options)
                      (declare (ignore owner options)) (push input inputs)))
              (list 'task-completion-deliver
                    (lambda (owner &rest options)
                      (when raced-warning
                        ;; An ordinary boundary wins delivery after ticket selection.
                        (funcall deliver owner :identifiers (list raced-warning))
                        (setf raced-warning nil))
                      (apply deliver owner options))))
        (lambda ()
          (dolist (mode '(:completion :warning :raced-warning))
            (job-completion-tests--start parent (task-completion--runtime parent))
            (unless (eq mode ':completion)
              (let ((service    (task-completion--service parent))
                    (identifier (format nil "cancellation/prompt-selection/~A" mode)))
                (with-lock-held ((task-completion-service-delivery-lock service))
                  (push (list :id identifier :kind ':cancellation-warning
                              :owner-conversation (conversation-identifier (agent-conversation parent))
                              :completion-policy ':continue :ready-at (get-universal-time)
                              :job-id "prompt-selection" :summary "fixture cancellation pending")
                        (task-completion-service-cancellation-notices service)))
                (when (eq mode ':raced-warning) (setf raced-warning identifier))))
            (application-job-completions-run
             controller
             (list :conversation (conversation-identifier (agent-conversation parent))
                   :events       (mapcar (lambda (notice) (getf notice :id)) (task-completion-pending parent))))
            (test-assert (equal (first inputs)
                                (if (eq mode ':warning) *application-job-cancellation-continuation-prompt*
                                    *application-job-completion-continuation-prompt*))
                         "actual delivered batch selects the appropriate bound prompt")
            (test-assert (null (task-completion-pending parent)) "batch delivered before continuation"))))
       (test-assert (= 3 (length inputs)) "one continuation per delivered batch"))))
  nil)
