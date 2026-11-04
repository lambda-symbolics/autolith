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

(-> test-job-completion-lightweight-goal-authority () null)
(defun test-job-completion-lightweight-goal-authority ()
  "Admit completed lightweight goals as ordinary session-owned wakeups."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (declare (ignore controller notices))
     (dolist (row '((:active 0 8 :continue nil nil t)
                    (:active 8 8 :continue nil nil nil)
                    (:active 8 :unlimited :continue nil nil t)
                    (:active 500 :unlimited :continue nil nil t)
                    (:complete 0 8 :continue nil nil t)
                    (:complete 8 8 :continue nil nil t)
                    (:paused 0 :unlimited :continue nil nil nil)
                    (:complete 0 8 :notify nil nil nil)
                    (:complete 0 8 :continue t nil nil)
                    (:complete 0 8 :continue nil "foreign-mission" nil)))
       (destructuring-bind (status count limit policy foreign-p mission-id expected) row
         (setf (config :goal-continuation-limit (application-configuration application)) limit)
         (setf (application-goal application)
               (list :objective "integrate results" :status status :continuations count))
         (let ((notice (job-completion-controller-tests--notice
                        application "lightweight-authority" :policy policy :mission-id mission-id)))
           (when foreign-p
             (setf (getf notice :owner-conversation) "foreign-conversation"))
           (test-assert
            (eq expected (application-job-completions--notice-admissible-p application notice))
            (format nil "lightweight wake authority for ~S" row)))))))
  nil)

(-> test-job-completion-completed-lightweight-wake () null)
(defun test-job-completion-completed-lightweight-wake ()
  "Wake once for a held owned job without reviving or charging its completed goal."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((*task-completion-coalescing-seconds* 0)
           (application                          (mission-test--application
                                                  configuration :results (list (mission-test--result "integrated"))))
           (provider                             (application-provider application))
           (parent                               (application-agent application))
           (registry                             (task-augment-tool-registry (application-tool-registry application)))
           (runtime                              (task-completion--runtime parent))
           (release                              (sb-thread:make-semaphore))
           (controller                           (application-input-controller-create
                                                  application :start-reader-p nil :load-pending-p nil)))
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
                   (list :objective "old objective" :status ':complete
                         :continuations 8 :created-at (get-universal-time)))
             (application--record-goal application)
             (test-assert (application-job-completions-awaited-p application)
                          "an unfinished owned job can wake after a lightweight goal completes")
             (test-assert (= 1 (application-job-completions-awaited-count application))
                          "the completed goal permits the owned yield snapshot")
             (sb-thread:signal-semaphore release)
             (session-job-await job 5)
             (cl-jobpond:completion-subscription-refresh
              (task-completion-service-subscription (task-completion--service parent)))
             (let* ((conversation (application-conversation application))
                    (before       (count :goal (conversation-records-newest conversation 100) :key #'first))
                    (work         nil))
               (task-tests--wait-until
                (lambda () (setf work (application-job-completions--take-work controller))) 5)
               (test-assert (eq (first work) ':job-completion)
                            "completed lightweight goals do not withhold controller work")
               (when work
                 (application-input-controller--run-work controller work))
               (test-assert (= 1 (length (scripted-provider-input-snapshots provider)))
                            "one completion causes exactly one ordinary provider request")
               (test-assert (eq (getf (application-goal application) :status) ':complete)
                            "the old goal is not reactivated")
               (test-assert (= 8 (getf (application-goal application) :continuations))
                            "the ordinary wake spends no completed-goal continuation")
               (test-assert (= before (count :goal (conversation-records-newest conversation 100) :key #'first))
                            "the ordinary wake appends no goal record")
               (test-assert
                (not (search "<goal_context>"
                             (json-encode (scripted-provider-input-snapshots provider))))
                "the provider receives no completed lightweight goal context")))
        (sb-thread:signal-semaphore release)
        (application-input-controller-stop controller)
        (setf (application-input-controller application) nil)
        (task-orchestrator-close runtime)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-job-completion-lightweight-limit-accounting () null)
(defun test-job-completion-lightweight-limit-accounting ()
  "Use the same configured cap for admission and keep unlimited wake accounting."
  (job-completion-controller-tests--fixture
   (lambda (application controller notices)
     (setf (config :goal-continuation-limit (application-configuration application)) 1)
     (setf (application-goal application)
           (list :objective "integrate" :status ':active :continuations 1))
     (funcall notices (list (job-completion-controller-tests--notice application "bounded")))
     (test-assert (null (application-job-completions--take-work controller))
                  "a nondefault cap withholds an active goal's next wake")
     (setf (getf (application-goal application) :continuations) 0)
     (let ((work (application-job-completions--take-work controller)))
       (test-assert work "a bounded goal below its cap admits its owned wake")
       (when work
         (application-input-controller--run-work controller work))
       (test-assert (= 1 (getf (application-goal application) :continuations))
                    "one bounded wake increments its active goal once")
       (test-assert (eq (getf (application-goal application) :status) ':complete)
                    "the bounded wake can complete its active goal"))
     (setf (config :goal-continuation-limit (application-configuration application)) ':unlimited)
     (setf (getf (application-goal application) :status) ':active)
     (funcall notices (list (job-completion-controller-tests--notice application "unlimited")))
     (setf (getf (application-goal application) :continuations) 8)
     (let ((work (application-job-completions--take-work controller)))
       (test-assert work "unlimited admits a wake at the old cap")
       (when work
         (application-input-controller--run-work controller work))
       (test-assert (= 9 (getf (application-goal application) :continuations))
                    "one unlimited wake increments its active goal once")
       (test-assert (eq (getf (application-goal application) :status) ':complete)
                    "the admitted wake can complete its active goal")))
   :results (list (mission-test--result "[GOAL-COMPLETE]")
                  (mission-test--result "[GOAL-COMPLETE]")))
  nil)
