(in-package #:autolith)

;;;; -- Completion Controller Admission --

(-> job-completion-controller-tests--notice
    (application string &key (:policy keyword) (:ready-at integer) (:mission-id (option string))) list)
(defun job-completion-controller-tests--notice
    (application id &key (policy ':continue) (ready-at (1- (get-universal-time))) mission-id)
  "Create one bounded completion owned by the fixture's exact conversation."
  (list :id id :job-id id :execution-id id
        :owner-conversation (conversation-identifier (application-conversation application))
        :completion-policy policy :mission-id mission-id :ready-at ready-at
        :state ':completed :outcome ':success :summary "completed fixture" :artifact-path nil))

(-> job-completion-controller-tests--fixture
    (function &key (:results list)) null)
(defun job-completion-controller-tests--fixture
    (function &key (results (loop repeat 4 collect (mission-test--result "done"))))
  "Exercise real primary turns while isolating the completion service's durable boundary."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration :results results))
           (controller  (make-instance 'application-input-controller
                                       :application application :main-thread (current-thread)))
           (notices     nil))
      (setf (application-input-controller application) controller)
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'task-completion-pending
                   (lambda (agent &key continuation-only-p)
                     (declare (ignore agent))
                     (remove-if-not
                      (lambda (notice)
                        (or (not continuation-only-p)
                            (eq (getf notice :completion-policy) ':continue)))
                      notices)))
             (list 'task-completion-deliver
                    (lambda (agent &key (identifiers nil identifiers-p))
                      (let ((delivered (if identifiers-p
                                           (remove-if-not
                                            (lambda (notice)
                                              (member (getf notice :id) identifiers :test #'equal))
                                            notices)
                                           notices)))
                       (dolist (notice delivered)
                         (conversation-append-user-message
                          (agent-conversation agent) (getf notice :summary)
                          :automatic-p              t
                          :pending-input-identifier (format nil "completion:~A" (getf notice :id))))
                       (setf notices (set-difference notices delivered :test #'eq))
                       delivered)))
             (list 'task-completion-disconnect (lambda (agent) (declare (ignore agent)) nil)))
            (lambda ()
              (funcall function application controller
                       (lambda (&optional (value nil supplied-p))
                         (when supplied-p (setf notices value))
                         notices))))
        (setf (application-input-controller application) nil)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-controller-coalescing () null)
(defun test-job-completion-controller-coalescing ()
  "Dispatch two ready outcomes as one primary continuation without repeating a consumed ticket."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (funcall notices
              (list (job-completion-controller-tests--notice application "first")
                    (job-completion-controller-tests--notice application "second")))
     (let ((work (application-input-controller--next-work controller)))
       (test-assert (eq (first work) ':job-completion) "normal controller admits completion work")
       (test-assert (= 2 (length (getf (second work) :events))) "one ticket contains the complete batch")
       (application-input-controller--run-work controller work)
       (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                    "two completions use one provider turn")
       (test-assert (= 2 (length (conversation-pending-input-identifiers (application-conversation application))))
                    "both completion receipts precede the provider request")
       (application-job-completions-run controller (second work))
       (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                    "repeated consumed ticket cannot spawn another turn")))))

(-> test-job-completion-controller-continuation-notice () null)
(defun test-job-completion-controller-continuation-notice ()
  "Name a completion wakeup as job results, live and replayed, without any goal."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (funcall notices (list (job-completion-controller-tests--notice application "only")))
     (let ((terminal (terminal-ui-terminal (application-ui application))))
       (recording-terminal-reset terminal)
       (application-input-controller--run-work
        controller (application-input-controller--next-work controller))
       (let* ((output   (recording-terminal-output terminal))
              (record   (find-if (lambda (record)
                                   (equal (getf (rest record) :content)
                                          *application-job-completion-continuation-prompt*))
                                 (conversation-records-newest
                                  (application-conversation application) 10)))
              (replayed (and record
                             (terminal--spans-text
                              (conversation-record-entry application record)))))
         (test-assert (null (application-goal application))
                      "the wakeup runs without a session goal")
         (test-assert (and (search "continuing with job results" output)
                           (not (search "goal continues" output)))
                      "a live completion wakeup is not announced as a goal continuation")
         (test-assert (and replayed
                           (search "continuing with job results" replayed)
                           (not (search "Continue the current work" replayed)))
                      "the replayed wakeup shows the same notice instead of its prompt"))))))

(-> test-job-completion-controller-ticket-delivery () null)
(defun test-job-completion-controller-ticket-delivery ()
  "Deliver exact admitted ticket identities despite stale backlog and busy-boundary races."
  (dolist (busy-p '(nil t))
    (job-completion-controller-tests--fixture
     (lambda (application controller notices)
       (application-mission-start application (mission-test--specification))
       (setf (getf (application-goal application) :status) ':verified)
       (let* ((stale  (job-completion-controller-tests--notice application "stale" :mission-id "retired"))
              (owned  (job-completion-controller-tests--notice application "owned"))
              (new    (job-completion-controller-tests--notice application "new"))
              (before (copy-tree (application-goal application))))
         (funcall notices (list stale owned))
         (let ((ticket  (second (application-job-completions--take-work controller)))
               (deliver (symbol-function 'task-completion-deliver)))
           (test-call-with-function-replacements
            (list (list 'task-completion-deliver
                        (lambda (agent &rest arguments)
                          (when busy-p
                            ;; A busy request consumes the ticket's event as new work arrives.
                            (funcall deliver agent :identifiers (list "owned"))
                            (funcall notices (append (funcall notices) (list new))))
                          (apply deliver agent arguments))))
            (lambda () (application-job-completions-run controller ticket)))
           (test-assert (= (if busy-p 0 1)
                           (length (scripted-provider-input-snapshots (application-provider application))))
                        "a consumed ticket cannot borrow a new completion's turn authority")
           (when busy-p
             (test-assert (member stale (funcall notices))
                          "a consumed ticket cannot drain unrelated mission work")
             (test-assert (member new (funcall notices))
                          "new completion waits for its own admitted ticket")
             (application-job-completions-run
              controller (second (application-job-completions--take-work controller)))
             (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                          "the new completion starts exactly one turn under its own ticket"))
           (test-assert (equal before (application-goal application))
                        "ticket delivery preserves terminal mission state and accounting"))))))
  nil)

(-> test-automatic-work-provider-failure () null)
(defun test-automatic-work-provider-failure ()
  "Contain refused automatic turns, retain their outcomes, and accept the next message."
  (dolist (work-kind '(:job-completion :operation-request :goal-continuation))
    (job-completion-controller-tests--fixture
     (lambda (application controller notices)
       (let* ((conversation (application-conversation application))
              (provider     (application-provider application))
              (terminal     (terminal-ui-terminal (application-ui application)))
              (work
                (if (eq work-kind ':operation-request)
                    (list ':operation-request
                          (list :text    "Run the generated fixture request."
                                :request (list :operation "fixture"
                                               :summary   "Run the fixture.")))
                    (progn
                      (funcall notices
                               (list (job-completion-controller-tests--notice
                                      application "refused-completion")))
                      (application-input-controller--next-work controller)))))
         (when (eq work-kind ':goal-continuation)
           (setf (application-goal application)
                 (list :objective "Finish the fixture." :status ':active
                       :continuations 0 :created-at (get-universal-time))))
         (recording-terminal-reset terminal)
         (application-input-controller--run-work controller work)
         (test-assert (= 1 (application-input-controller-prompt-marker-status controller))
                      "the refused automatic turn reports a failed prompt")
         (test-assert (search "cyber_policy" (recording-terminal-output terminal))
                      "the provider refusal is visible in the terminal")
         (test-assert (find-if
                       (lambda (record)
                         (and (eq (first record) ':turn-aborted)
                              (eq (getf (rest record) :reason) ':application-error)))
                       (conversation--read-records (conversation-pathname conversation)))
                      "the failed automatic turn has a durable aborted outcome")
         (unless (eq work-kind ':operation-request)
           (test-assert (member "completion:refused-completion"
                                (conversation-pending-input-identifiers conversation)
                                :test #'equal)
                        "the completion receipt survives its failed provider turn")
           (test-assert (null (funcall notices))
                        "a refused provider turn does not redeliver the completed job"))
         (let ((request-count (length (scripted-provider-input-snapshots provider))))
           (test-assert (= request-count (if (eq work-kind ':goal-continuation) 2 1))
                        "the refusal does not trigger an automatic provider retry")
           (setf (application-goal application) nil)
           (application-input-controller--run-work controller (list ':message "Continue."))
           (test-assert (= (1+ request-count)
                           (length (scripted-provider-input-snapshots provider)))
                        "the same controller accepts a subsequent user message")
           (test-assert (null (application-input-controller-stopping-p controller))
                        "the refused turn does not shut down the controller"))))
     :results (append
               (when (eq work-kind ':goal-continuation)
                 (list (mission-test--result "integrated")))
               (list (make-condition 'provider-error
                                     :message "The provider returned cyber_policy."
                                     :code    "cyber_policy")
                     (mission-test--result "continued")))))
  nil)

(-> test-job-completion-controller-notify-and-busy () null)
(defun test-job-completion-controller-notify-and-busy ()
  "Notify-only waits for a user turn; busy-boundary delivery suppresses a redundant idle turn."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (funcall notices (list (job-completion-controller-tests--notice application "notify" :policy ':notify)))
     (test-assert (null (application-job-completions--take-work controller)) "notify-only never starts work")
     (agent-run-user-turn (application-agent application) "Inspect outcomes")
     (test-assert (null (funcall notices)) "next ordinary user turn delivers notify-only data")
     (funcall notices (list (job-completion-controller-tests--notice application "busy")))
     (let ((ticket (second (application-job-completions--take-work controller))))
       (agent--apply-steering-input (application-agent application) (make-instance 'agent-observer) 1)
       (application-job-completions-run controller ticket)
       (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                    "busy delivery consumes a queued idle wake without another provider turn")))))

(-> test-job-completion-controller-pause-and-switch () null)
(defun test-job-completion-controller-pause-and-switch ()
  "Retain results across cancellation and reject another conversation's completion ticket."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (funcall notices (list (job-completion-controller-tests--notice application "paused")))
     (setf (application-input-controller-queued-work-paused-p controller) t)
     (test-assert (null (application-job-completions--next-time controller)) "cancelled queue has no completion deadline")
     (test-assert (null (application-job-completions--take-work controller)) "completion cannot reopen cancelled work")
     (test-assert (application-input-controller-queued-work-paused-p controller) "admission preserves cancellation authority")
     (test-assert (= 1 (length (funcall notices))) "blocked outcome remains inspectable")
     (setf (application-input-controller-queued-work-paused-p controller) nil)
     (let ((ticket (second (application-job-completions--take-work controller))))
       (setf (getf ticket :conversation) "another-conversation")
       (application-job-completions-run controller ticket)
       (test-assert (= 1 (length (funcall notices))) "stale conversation ticket does not consume an outcome")
       (test-assert (null (scripted-provider-input-snapshots (application-provider application)))
                    "conversation switch cannot redirect an automatic turn")))))

(-> test-job-completion-controller-mission-admission () null)
(defun test-job-completion-controller-mission-admission ()
  "Reject paused, exhausted, uncertain and replaced mission authorities before delivery."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (application-mission-start application (mission-test--specification))
     (let* ((goal     (application-goal application))
            (identity "fixture-mission")
            (notice   (job-completion-controller-tests--notice application "mission" :mission-id identity)))
        (nconc goal (list :completion-id identity))
       (funcall notices (list notice))
       (test-assert (application-job-completions--take-work controller) "current active mission permits completion")
       (dolist (status '(:paused :blocked :cancelled :completed :exhausted :failed))
         (setf (getf goal :status) status)
         (test-assert (null (application-job-completions--take-work controller)) "terminal or paused mission cannot reactivate"))
       (setf (getf goal :status) ':active
             (getf goal :turns-used) (getf goal :turn-limit))
       (test-assert (null (application-job-completions--take-work controller)) "spent inference allowance blocks continuation")
       (setf (getf goal :turns-used) 0 (getf goal :unknown-usage) 1)
       (test-assert (null (application-job-completions--take-work controller)) "unknown usage blocks continuation")
       (setf (getf goal :unknown-usage) 0 (getf goal :completion-id) "replacement-mission")
       (test-assert (null (application-job-completions--take-work controller)) "replaced mission identity blocks continuation")
       (test-assert (= 1 (length (funcall notices))) "inadmissible outcomes remain inspectable")))))

(-> test-job-completion-controller-real-runtime () null)
(defun test-job-completion-controller-real-runtime ()
  "Coalesce real terminal publications through the normal controller and busy request boundary."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((*task-completion-coalescing-seconds* 0)
           (application                          (mission-test--application
                                                  configuration :results (loop repeat 3 collect (mission-test--result "done"))))
           (parent                               (application-agent application))
           (registry                             (task-augment-tool-registry (application-tool-registry application)))
           (runtime                              (task-completion--runtime parent))
           (controller                           (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
      (declare (ignore registry))
      (unwind-protect
           (progn
             (conversation-append-user-message (agent-conversation parent) "Start asynchronous work.")
             (let ((first  (job-completion-tests--start parent runtime))
                   (second (job-completion-tests--start parent runtime)))
               (test-assert (null (scripted-provider-input-snapshots (application-provider application)))
                            "publication callbacks never invoke the provider")
               (let ((work (application-input-controller--next-work controller)))
                 (test-assert (= 2 (length (getf (second work) :events)))
                              "both real outcomes share one controller ticket")
                 (application-input-controller--run-work controller work))
               (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                            "one normal primary turn integrates two real completed jobs")
               (test-assert (and (task-completion--delivered-p (agent-conversation parent)
                                                              (session-job-execution-identifier first))
                                 (task-completion--delivered-p (agent-conversation parent)
                                                              (session-job-execution-identifier second)))
                            "coalesced delivery retains both exact execution receipts"))
             (job-completion-tests--start parent runtime)
             (job-completion-tests--start parent runtime)
             (agent-run-user-turn parent "Continue independent work.")
             (test-assert (null (task-completion-pending parent))
                          "a busy ordinary request consumes completed data at its safe boundary")
             (test-assert (null (application-job-completions--take-work controller))
                          "busy delivery leaves no redundant idle continuation")
             (setf (application-input-controller-queued-work-paused-p controller) t)
             (job-completion-tests--start parent runtime)
             (test-assert (and (null (application-job-completions--take-work controller))
                               (= 1 (length (task-completion-pending parent))))
                          "completion retains data without reopening cancelled work"))
        (application-input-controller-stop controller)
        (setf (application-input-controller application) nil)
        (task-orchestrator-close runtime)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-controller-goal-awaits-jobs () null)
(defun test-job-completion-controller-goal-awaits-jobs ()
  "Hold goal continuations while an owned job runs, then resume them after its wakeup."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((*task-completion-coalescing-seconds* 0)
           (application                          (mission-test--application
                                                  configuration
                                                  :results (list (mission-test--result "started")
                                                                 (mission-test--result "integrated")
                                                                 (mission-test--result "[GOAL-COMPLETE]"))))
           (provider                             (application-provider application))
           (parent                               (application-agent application))
           (registry                             (task-augment-tool-registry (application-tool-registry application)))
           (runtime                              (task-completion--runtime parent))
           (release                              (sb-thread:make-semaphore))
           (controller                           (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
      (declare (ignore registry))
      (unwind-protect
           (let ((job (task-orchestrator-start-execution-job
                       runtime parent :tool-name "completion.fixture" :summary "held job"
                       :operation-function (lambda ()
                                             (sb-thread:wait-on-semaphore release)
                                             (tool-success "completed"))
                       :detached-p t :completion-policy ':continue)))
             (task-completion-watch job parent)
             (setf (application-goal application)
                   (list :objective "integrate the job" :status ':active
                         :continuations 0 :created-at (get-universal-time)))
             (application-run-message application "Start asynchronous work.")
             (test-assert (and (= 1 (length (scripted-provider-input-snapshots provider)))
                               (eq (getf (application-goal application) :status) ':active)
                               (zerop (getf (application-goal application) :continuations)))
                          "a running owned job holds goal continuations without spending them")
             (sb-thread:signal-semaphore release)
             (session-job-await job 5)
             (cl-jobpond:completion-subscription-refresh
              (task-completion-service-subscription (task-completion--service parent)))
             (application-input-controller--run-work
              controller (application-input-controller--next-work controller))
             (test-assert (= 3 (length (scripted-provider-input-snapshots provider)))
                          "the completion wakeup is followed by the goal's continuation")
             (test-assert (eq (getf (application-goal application) :status) ':complete)
                          "the resumed continuation completes the goal"))
        (sb-thread:signal-semaphore release)
        (application-input-controller-stop controller)
        (setf (application-input-controller application) nil)
        (task-orchestrator-close runtime)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-controller-mixed-overflow () null)
(defun test-job-completion-controller-mixed-overflow ()
  "Publish an eligible continuation behind notify-only or stale-mission capacity without user input."
  (dolist (mode '(:notify :stale-mission))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((*task-completion-capacity*           2)
             (*task-completion-history-limit*      4)
             (*task-completion-coalescing-seconds* 0)
             (application                          (mission-test--application configuration :results (list (mission-test--result "done"))))
             (parent                               (application-agent application))
             (registry                             (task-augment-tool-registry (application-tool-registry application)))
             (runtime                              (task-completion--runtime parent))
             (controller                           (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
        (declare (ignore registry))
        (unwind-protect
             (progn
               (conversation-append-user-message (agent-conversation parent) "Start asynchronous work.")
               (when (eq mode ':stale-mission)
                 (application-mission-start application (mission-test--specification)))
               (dotimes (index 2)
                 (declare (ignorable index))
                 (job-completion-tests--start parent runtime :policy (if (eq mode ':notify) ':notify ':continue)))
               (when (eq mode ':stale-mission)
                 (application-mission-start application (mission-test--specification)))
               (let* ((job          (task-orchestrator-start-execution-job
                                     runtime parent :tool-name "completion.fixture" :summary "eligible overflow"
                                     :operation-function (lambda () (tool-success "done")) :detached-p t))
                      (subscription (task-completion-service-subscription (task-completion--service parent))))
                 (task-completion-watch job parent)
                 (session-job-await job 5)
                 (task-completion--try-publish (lambda () (cl-jobpond:completion-subscription-refresh subscription)))
                 (test-assert (null (application-job-completions--take-work controller))
                              "the eligible result initially sits behind bounded notification pressure")
                 (let ((work (application-input-controller--next-work controller)))
                   (test-assert (eq (first work) ':job-completion-maintenance)
                                "normal controller selects bounded metadata maintenance")
                   (application-input-controller--run-work controller work))
                 (test-assert (null (scripted-provider-input-snapshots (application-provider application)))
                              "notify and stale mission drainage never invoke the model")
                 (let ((work (application-input-controller--next-work controller)))
                   (test-assert (and (eq (first work) ':job-completion)
                                     (member (session-job-execution-identifier job)
                                             (getf (second work) :events) :test #'equal))
                                "eligible publication schedules itself without unrelated user input")
                   (when (eq mode ':notify)
                     (application-input-controller--run-work controller work)
                     (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                                  "overflow recovery grants exactly one ordinary continuation")))))
          (application-input-controller-stop controller)
          (setf (application-input-controller application) nil)
          (task-orchestrator-close runtime)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> test-job-completion-controller-after-terminal-mission () null)
(defun test-job-completion-controller-after-terminal-mission ()
  "Resume real missionless completions without reopening a retained terminal mission."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((*task-completion-coalescing-seconds* 0)
           (application                          (mission-test--application configuration :results (list (mission-test--result "integrated"))))
           (parent                               (application-agent application))
           (registry                             (task-augment-tool-registry (application-tool-registry application)))
           (runtime                              (task-completion--runtime parent))
           (controller                           (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
      (declare (ignore registry))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification))
             (job-completion-tests--start parent runtime)
             (setf (getf (application-goal application) :status) ':verified)
             (application--record-goal application)
             (test-assert (null (application-job-completions--take-work controller))
                          "a terminal mission's old completion cannot resume inference")
             (let* ((before (copy-tree (application-goal application)))
                    (job    (job-completion-tests--start parent runtime))
                    (notice (find (session-job-identifier job) (task-completion-pending parent)
                                  :key (lambda (entry) (getf entry :job-id)) :test #'equal))
                    (work   (handler-case
                                (sb-ext:with-timeout 5
                                  (application-input-controller--next-work controller))
                              (sb-ext:timeout () nil))))
               (test-assert (null (session-job-completion-mission-id job))
                            "new work after the terminal mission has no mission authority")
               (test-assert (eq (first work) ':job-completion)
                            "the controller admits the missionless completion")
               (when work
                 (application-input-controller--run-work controller work))
               (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                            "the completion starts exactly one ordinary provider turn")
               (test-assert (member (format nil "completion:~A" (getf notice :id))
                                    (conversation-pending-input-identifiers (application-conversation application))
                                    :test #'equal)
                            "the completion has a durable receipt before inference")
               (test-assert (equal before (application-goal application))
                            "missionless inference does not mutate the retained mission")
               (when work
                 (application-job-completions-run controller (second work)))
               (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                            "a consumed ticket cannot spend another provider turn")))
        (application-input-controller-stop controller)
        (setf (application-input-controller application) nil)
        (task-orchestrator-close runtime)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-terminal-mission-authority () null)
(defun test-job-completion-terminal-mission-authority ()
  "Separate missionless wakeups from stale mission authority across retained states."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (application-mission-start application (mission-test--specification))
     (let ((current-id (task-completion-mission-identifier (application-agent application))))
       (dolist (status '(:active :paused :verified :failed :cancelled :exhausted :blocked))
         (setf (getf (application-goal application) :status) status)
         (dolist (row (list (list nil ':continue nil (not (eq status ':active)))
                            (list current-id ':continue nil (eq status ':active))
                            (list "another-mission" ':continue nil nil)
                            (list nil ':notify nil nil)
                            (list nil ':continue t nil)))
           (destructuring-bind (mission-id policy foreign-p expected) row
             (let ((notice (job-completion-controller-tests--notice
                            application "authority" :mission-id mission-id :policy policy)))
               (when foreign-p
                 (setf (getf notice :owner-conversation) "another-conversation"))
               (test-assert (eq expected (application-job-completions--notice-admissible-p application notice))
                            "only current owned completion authority permits a wakeup")))))
       (funcall notices (list (job-completion-controller-tests--notice application "paused")))
       (setf (application-input-controller-queued-work-paused-p controller) t)
       (test-assert (null (application-job-completions--take-work controller))
                    "completion authority cannot clear a controller pause"))))
  nil)


;;;; -- Empty-Turn Presentation --

(-> test-job-completion-yield-presentation () null)
(defun test-job-completion-yield-presentation ()
  "Finalize empty turns using owned wakeup authority and replay the historical snapshot."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory        root
                                                        :session-title-generation-p nil))
    (let* ((application (mission-test--application
                         configuration
                         :results (list (mission-test--result "")
                                        (mission-test--result "ordinary reply")
                                        (mission-test--result "")
                                        (mission-test--result ""))))
           (parent      (application-agent application))
           (registry    (task-augment-tool-registry (application-tool-registry application)))
           (runtime     (task-completion--runtime parent))
           (controller  (application-input-controller-create
                         application :start-reader-p nil :load-pending-p nil))
           (terminal    (terminal-ui-terminal (application-ui application)))
           (release     (sb-thread:make-semaphore))
           (held-jobs   nil))
      (declare (ignore registry))
      (labels ((hold-job (owner &key (policy ':continue))
                 "Start and watch one real job held until fixture cleanup."
                 (let ((job (task-orchestrator-start-execution-job
                             runtime owner :tool-name "completion.fixture" :summary "held work"
                             :operation-function
                             (lambda ()
                               (sb-thread:wait-on-semaphore release)
                               (tool-success "done"))
                             :detached-p t :completion-policy policy)))
                   (task-completion-watch job owner)
                   (push job held-jobs)
                   job))

               (yield-records ()
                 "Return durable presentation snapshots without provider text."
                 (remove-if-not (lambda (record) (eq (first record) ':assistant-yield))
                                (conversation-records-newest (application-conversation application) 100))))
        (unwind-protect
             (progn
               (hold-job parent)
               (hold-job parent :policy ':notify)
               (hold-job (make-instance 'agent :configuration configuration
                                              :conversation   (conversation-create configuration)
                                              :provider       (application-provider application)
                                              :tool-registry  (application-tool-registry application)))
               ;; Keep a genuine detached child queued, without starting another model.
               (let ((submit (symbol-function 'job-pool-submit-batch)))
                 (test-call-with-function-replacements
                  (list (list 'job-pool-submit-batch
                              (lambda (pool entries)
                                (funcall submit pool
                                         (mapcar (lambda (entry) (list* :inline-only-p t entry)) entries)))))
                  (lambda ()
                    (task-orchestrator-start-jobs
                     runtime parent
                     (list (list :definition
                                 (task-agent-definition-create
                                  :name "yield-fixture" :description "Queued wakeup fixture"
                                  :instructions "Wait for fixture release" :source ':test)
                                 :detached t
                                 :item     (list :name "yield-child" :agent "yield-fixture" :task "Wait")))))))
               (let ((finished (job-completion-tests--start parent runtime)))
                 (test-assert (job-terminal-p finished) "a published outcome is already terminal"))
               (test-assert (= 2 (application-job-completions-awaited-count application))
                            "only the owned continue tool and child are unfinished wakeups")
               (recording-terminal-reset terminal)
               (application-run-message application "Wait for asynchronous work.")
               (let* ((record (first (yield-records)))
                      (entry  (conversation-record-entry application record)))
                 (test-assert (and (= 1 (length (yield-records)))
                                   (= 2 (getf (rest record) :job-count))
                                   (application--record-visible-p application record))
                              "an empty final response persists one exact wakeup snapshot")
                 (test-assert (equal (first entry)
                                     (first (response-item-entry application (agent-test-message "ordinary"))))
                              "the notice uses the normal assistant header style")
                 (test-assert (null (response-item-entry application (agent-test-message "")))
                              "an empty provider message has no blank assistant header")
                 (test-assert
                  (find (json-encode (agent-test-message ""))
                        (conversation-input-items (application-conversation application))
                        :key #'json-encode :test #'string=)
                  "presentation metadata does not replace the raw empty provider message")
                 (test-assert (search (terminal--spans-text entry) (recording-terminal-output terminal))
                              "the finalized presentation includes the snapshot")
                 (recording-terminal-reset terminal)
                 (application-render-records application)
                 (test-assert (zerop (length (recording-terminal-output terminal)))
                              "finalization prevents duplicate presentation")
                 (application-run-message application "Give an ordinary reply.")
                 (test-assert (= 1 (length (yield-records)))
                              "nonempty replies do not create yield metadata")
                 (setf (application-input-controller-queued-work-paused-p controller) t)
                 (test-assert (zerop (application-job-completions-awaited-count application))
                              "paused automatic work has no wakeup authority")
                 (application-run-message application "Finish silently while paused.")
                 (test-assert (= 1 (length (yield-records)))
                              "an empty response with no admitted wakeups stays invisible")
                 (setf (application-input-controller-queued-work-paused-p controller) nil)
                 ;; Finish between the provider's final persistence and turn finalization.
                 (let ((present (symbol-function 'application-job-completions-present-yield)))
                   (test-call-with-function-replacements
                    (list (list 'application-job-completions-present-yield
                                (lambda (application details)
                                  (dotimes (index (length held-jobs))
                                    (declare (ignorable index))
                                    (sb-thread:signal-semaphore release))
                                  (dolist (job held-jobs) (session-job-await job 5))
                                  (dolist (job (task-completion-awaited-jobs parent))
                                    (session-job-cancel job ':test))
                                  (funcall present application details))))
                    (lambda () (application-run-message application "Finish as the jobs complete."))))
                 (test-assert (and (zerop (application-job-completions-awaited-count application))
                                   (= 1 (length (yield-records))))
                              "completion before the snapshot cannot invent a running wakeup")
                 (test-assert (equal entry (conversation-record-entry application record))
                              "historical replay does not consult completed jobs")
                 (test-assert (null (conversation-record-entry application '(:assistant-yield :job-count 0)))
                              "invalid or zero snapshots are invisible")))
          (dotimes (index (length held-jobs))
            (declare (ignorable index))
            (sb-thread:signal-semaphore release))
          (application-input-controller-stop controller)
          (setf (application-input-controller application) nil)
          (task-orchestrator-close runtime)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> job-completion-controller-tests--ready (agent) list)
(defun job-completion-controller-tests--ready (parent)
  "Wait boundedly for genuine persisted completion deadlines, without rewriting them."
  (loop with deadline = (+ (get-internal-real-time) (* 5 internal-time-units-per-second))
        for notices = (task-completion-pending parent)
        when (and notices (every (lambda (notice) (<= (getf notice :ready-at) (get-universal-time))) notices))
          return notices
        when (>= (get-internal-real-time) deadline)
          do (error "The real completion did not become ready: ~S" notices)
        do (sleep 0.01)))

(-> job-completion-controller-tests--terminal-fixture
    (function &key (:operation function) (:coalescing integer) (:unknown-usage integer)
                  (:wait-ready-p boolean) (:response string)) null)
(defun job-completion-controller-tests--terminal-fixture
    (function &key (operation (lambda () (tool-success "terminal fixture result"))) (coalescing 0)
                  (unknown-usage 1) (wait-ready-p t) (response "reviewed"))
  "Publish one real mission-owned outcome, then block its scratch mission."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((*task-completion-coalescing-seconds* coalescing)
           (application                          (mission-test--application configuration :results (list (mission-test--result response))))
           (parent                               (application-agent application))
           (registry                             (task-augment-tool-registry (application-tool-registry application)))
           (runtime                              (task-completion--runtime parent))
           (controller                           (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
      (declare (ignore registry))
      (setf (application-localgroup-session application) (make-instance 'localgroup-session :application application))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification))
             (let ((job (job-completion-tests--start parent runtime :operation operation)))
               (application-mission-stop application ':blocked "Scratch mission needs a decision.")
               (when (plusp unknown-usage)
                 (setf (getf (application-goal application) :unknown-usage) unknown-usage))
               (application--record-goal application)
               (when wait-ready-p (job-completion-controller-tests--ready parent))
               (application-render-records application)
               (recording-terminal-reset (terminal-ui-terminal (application-ui application)))
               (funcall function application controller parent job)))
        (setf (application-input-controller-stopping-p controller) nil
              (application-input-controller-failure controller) nil
              (application-input-controller-localgroup-handoff-p controller) nil
              (application-input-controller-follow-up-edit-index controller) nil
              (application-input-controller-queued-work-paused-p controller) nil)
        (application-localgroup-resume application)
        (application-input-controller-stop controller)
        (setf (application-input-controller application) nil)
        (task-orchestrator-close runtime)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-terminal-visibility () null)
(defun test-job-completion-terminal-visibility ()
  "Show real success, failure, cancellation and timeout outcomes once without inference."
  (dolist (reason '(nil :failed :cancelled :timeout))
    (job-completion-controller-tests--terminal-fixture
     (lambda (application controller parent job)
       (let* ((conversation  (application-conversation application))
              (goal          (copy-tree (application-goal application)))
              (ui            (application-ui application))
              (terminal      (terminal-ui-terminal ui))
              (present       (symbol-function 'application-present))
              (presentations nil)
              (work          (handler-case
                                 (sb-ext:with-timeout 3 (application-input-controller--next-work controller))
                               (sb-ext:timeout () nil))))
         (test-assert (not (task-completion-maintenance-needed-p parent)) "one small result has no queue pressure")
         (test-assert (eq (first work) ':job-completion-maintenance) "idle selection schedules display, not inference")
         (test-call-with-function-replacements
          (list (list 'application-present
                      (lambda (owner entry &key identifier)
                        (push (list identifier entry) presentations)
                        (funcall present owner entry :identifier identifier))))
          (lambda ()
            (when work (application-input-controller--run-work controller work))))
         (let* ((records  (remove-if-not
                           (lambda (record) (getf (rest record) :job-completion))
                           (conversation-records-newest conversation 20)))
                (record   (first records))
                (identity (and record (application--record-presentation-identifier application record))))
           (test-assert (and (= 1 (length records)) (= 1 (length presentations))
                             (equal identity (first (first presentations)))
                             (gethash identity (terminal-ui-finalized-identifiers ui)))
                        "actual terminal finalization and the sole durable outcome share identity")
           (test-assert (and record
                             (equal (getf (getf (rest record) :job-completion) :job-id) (session-job-identifier job))
                             (eq (getf (getf (rest record) :job-completion) :state) (job-state job))
                             (conversation-pending-input-identifier-p conversation
                                                                      (getf (rest record) :pending-input-identifier)))
                        "the genuine terminal state has a durable delivery receipt")
           (test-assert (and (plusp (length (recording-terminal-output terminal)))
                             (search (session-job-identifier job) (terminal--spans-text (second (first presentations)))))
                        "the actual presentation identifies the completed child")
           (test-assert (and (null (scripted-provider-input-snapshots (application-provider application)))
                             (equal goal (application-goal application)))
                        "display consumes no provider request and leaves all mission accounting unchanged")
           (recording-terminal-reset terminal)
           (when work (application-input-controller--run-work controller work))
           (test-assert (and (null (application-job-completions--take-maintenance controller))
                             (null (application-job-completions--next-time controller))
                             (null (task-completion-deliver parent)))
                        "acknowledgement removes repeat work and its deadline without a blocking next-work call")
           (when record
             (test-assert (not (application--present-conversation-record application record))
                          "ordinary transcript rendering cannot finalize the same outcome twice"))
           (conversation-append-user-message conversation "Review the result without resuming the mission.")
           (test-assert (and (null (task-completion-deliver parent))
                             (= 1 (count (getf (rest record) :pending-input-identifier)
                                         (conversation--read-records (conversation-pathname conversation))
                                         :key (lambda (entry) (getf (rest entry) :pending-input-identifier)) :test #'equal)))
                        "ordinary manual input preserves the sole completion receipt")
           (test-assert (zerop (length (recording-terminal-output terminal))) "replayed delivery and rendering produce no repeated output"))))
     :operation (case reason
                  (:failed
                   (lambda () (error "Expected scratch execution failure.")))
                  ((:cancelled :timeout)
                   (lambda () (error 'job-aborted :identifier "terminal fixture" :reason reason :message "expected terminal outcome")))
                  (otherwise
                   (lambda () (tool-success "terminal fixture result"))))))
  nil)

(-> test-job-completion-terminal-boundaries () null)
(defun test-job-completion-terminal-boundaries ()
  "Defer queued and post-selection delivery across edit, pause, stop and ownership changes."
  (dolist (boundary '(follow-up-edit-index queued-work-paused-p stopping-p failure localgroup-handoff-p session-pause owner))
    (job-completion-controller-tests--terminal-fixture
     (lambda (application controller parent job)
       (declare (ignore job))
       (let* ((work            (application-job-completions--take-maintenance controller))
              (ticket          (second work))
              (owner           (getf ticket :conversation))
              (scan            (symbol-function 'task-cancellation-maintain))
              (close-on-scan-p nil))
         (labels ((close-boundary (closed-p)
                    (case boundary
                      (session-pause
                       (if closed-p (application-localgroup-pause application) (application-localgroup-resume application)))
                      (owner
                       (setf (getf ticket :conversation) (if closed-p "foreign-conversation" owner)))
                      (otherwise
                       (setf (slot-value controller boundary)
                             (when closed-p
                               (case boundary (follow-up-edit-index
                                               0)
                                              (failure
                                               (make-condition 'simple-error))
                                              (otherwise
                                               t))))))))
           (test-assert (eq (first work) ':job-completion-maintenance) "a safe controller selects the real outcome")
           (close-boundary t)
           (unless (eq boundary 'owner)
             (test-assert (null (application-job-completions--take-maintenance controller)) "closed selection leaves the outcome pending"))
           (when work (application-input-controller--run-work controller work))
           (test-assert (and (= 1 (length (task-completion-pending parent)))
                             (zerop (length (recording-terminal-output (terminal-ui-terminal (application-ui application))))))
                        "a stale ticket cannot consume or display across a closed boundary")
           (close-boundary nil)
           (setf close-on-scan-p t)
           (test-call-with-function-replacements
            (list (list 'task-cancellation-maintain
                        (lambda (agent &key (now (get-internal-real-time)))
                          (prog1 (funcall scan agent :now now)
                            (when close-on-scan-p
                              (setf close-on-scan-p nil)
                              (close-boundary t))))))
            (lambda () (when work (application-input-controller--run-work controller work))))
           (test-assert (and (= 1 (length (task-completion-pending parent)))
                             (null (scripted-provider-input-snapshots (application-provider application))))
                        "closure during delivery setup is rechecked before the durable append")
           (close-boundary nil)
           (when work (application-input-controller--run-work controller work))
           (test-assert (and (null (task-completion-pending parent))
                             (plusp (length (recording-terminal-output (terminal-ui-terminal (application-ui application))))))
                        "finishing the edit or resuming permits the deferred result exactly once"))))))
  nil)

(-> job-completion-controller-tests--admitted-turn (application application-input-controller) null)
(defun job-completion-controller-tests--admitted-turn (application controller)
  "Run ordinary admitted inference and let real verification block on unproven criteria."
  (let ((automatic (application-job-completions--take-work controller)))
    (test-assert (eq (first automatic) ':job-completion) "ordinary mission admission still selects inference")
    (test-call-with-function-replacements
     (list (list 'application-tool-context (lambda (app) (mission-test--context app))))
     (lambda () (when automatic (application-input-controller--run-work controller automatic))))
    (let ((goal     (application-goal application))
          (requests (length (scripted-provider-input-snapshots (application-provider application)))))
      (format t "~&HIGH177 admitted requests=~D goal=~S~%" requests goal)
      (test-assert (= 1 requests) "the admitted result causes exactly one ordinary automatic provider turn")
      (test-assert (and (eq (getf goal :status) ':blocked) (getf goal :model-complete-p)
                        (= 1 (getf goal :turns-used)) (= 5 (getf goal :tokens-used))
                        (zerop (getf goal :unknown-usage)) (zerop (getf goal :requests-outstanding)))
                   "real verification blocks unproven scratch criteria after one fully accounted turn")))
  nil)

(-> test-job-completion-terminal-admission-control () null)
(defun test-job-completion-terminal-admission-control ()
  "Preserve an ordinary admitted mission continuation on baseline and candidate."
  (job-completion-controller-tests--terminal-fixture
   (lambda (application controller parent job)
     (declare (ignore job))
     (test-assert (= 1 (length (job-completion-controller-tests--ready parent))) "ordinary control has one persisted ready result")
     (application-mission-resume application)
     (job-completion-controller-tests--admitted-turn application controller))
   :unknown-usage 0 :response "[GOAL-COMPLETE]")
  nil)

(-> test-job-completion-terminal-admission-race () null)
(defun test-job-completion-terminal-admission-race ()
  "Keep a newly admissible result for one automatic turn instead of consuming its display ticket."
  (job-completion-controller-tests--terminal-fixture
   (lambda (application controller parent job)
     (declare (ignore job))
     (let ((work (application-job-completions--take-maintenance controller)))
       (test-assert (and (eq (first work) ':job-completion-maintenance)
                         (getf (second work) :terminal-notice-p) (getf (second work) :events))
                    "the race starts with a real ready terminal-notice ticket")
       (application-mission-resume application)
       (when work (application-input-controller--run-work controller work))
       (test-assert (and (= 1 (length (task-completion-pending parent)))
                         (null (scripted-provider-input-snapshots (application-provider application))))
                    "ready-to-admissible change leaves the outcome unconsumed by maintenance")
       (job-completion-controller-tests--admitted-turn application controller)))
   :unknown-usage 0 :response "[GOAL-COMPLETE]")
  nil)

(-> test-job-completion-terminal-mixed-replay () null)
(defun test-job-completion-terminal-mixed-replay ()
  "Reject a partly stale batch without consuming its fresh owned result."
  (job-completion-controller-tests--terminal-fixture
   (lambda (application controller parent job)
     (declare (ignore job))
     (let ((old (application-job-completions--take-maintenance controller)))
       (test-assert (getf (second old) :terminal-notice-p) "mixed replay starts from a real terminal ticket")
       (when old
         (application-input-controller--run-work controller old)
         (application-mission-resume application)
         (job-completion-tests--start parent (task-completion--runtime parent))
         (application-mission-stop application ':blocked "Scratch second result needs a decision.")
         (job-completion-controller-tests--ready parent)
         (let* ((fresh    (application-job-completions--take-maintenance controller))
                (mixed    (copy-tree fresh))
                (terminal (terminal-ui-terminal (application-ui application))))
           (test-assert (getf (second fresh) :terminal-notice-p) "second genuine result has a fresh terminal ticket")
           (setf (getf (second mixed) :events)
                 (append (getf (second old) :events) (getf (second fresh) :events)))
           (recording-terminal-reset terminal)
           (application-input-controller--run-work controller mixed)
           (test-assert (and (= 1 (length (task-completion-pending parent)))
                             (zerop (length (recording-terminal-output terminal))))
                        "rejecting a partly stale ticket leaves the fresh outcome pending and undisplayed")
           (let ((selected (application-job-completions--take-maintenance controller)))
             (test-assert (equal fresh selected) "normal reselection retains precisely the fresh event")
             (when selected (application-input-controller--run-work controller selected)))
           (test-assert (and (null (task-completion-pending parent))
                             (plusp (length (recording-terminal-output terminal)))
                             (= 2 (length (conversation-pending-input-identifiers (application-conversation application)))))
                        "normal selection delivers the fresh result once with its own receipt")
           (recording-terminal-reset terminal)
           (application-input-controller--run-work controller fresh)
           (test-assert (zerop (length (recording-terminal-output terminal))) "fresh-ticket replay cannot repeat presentation")))))
   :unknown-usage 0)
  nil)

(-> test-job-completion-terminal-coalescing () null)
(defun test-job-completion-terminal-coalescing ()
  "Wake the real idle controller when a low-pressure inadmissible batch becomes ready."
  (job-completion-controller-tests--terminal-fixture
   (lambda (application controller parent job)
     (declare (ignore application job))
     (test-assert (integerp (application-job-completions--next-time controller)) "a pending display has a wake deadline")
     (let ((work (handler-case
                     (sb-ext:with-timeout 5 (application-input-controller--next-work controller))
                   (sb-ext:timeout () nil))))
       (test-assert (eq (first work) ':job-completion-maintenance) "the actual idle wait selects display when the deadline matures")
       (when work (application-input-controller--run-work controller work))
       (test-assert (null (task-completion-pending parent)) "the ready outcome is consumed without queue pressure")))
   :coalescing 1 :wait-ready-p nil)
  nil)
