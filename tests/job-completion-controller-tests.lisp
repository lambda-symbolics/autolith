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

(-> job-completion-controller-tests--fixture (function) null)
(defun job-completion-controller-tests--fixture (function)
  "Exercise real primary turns while isolating the completion service's durable boundary."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application
                         configuration :results (loop repeat 4 collect (mission-test--result "done"))))
           (controller (make-instance 'application-input-controller
                                      :application application :main-thread (current-thread)))
           (notices nil))
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
                   (lambda (agent)
                     (let ((delivered notices))
                       (dolist (notice delivered)
                         (conversation-append-user-message
                          (agent-conversation agent) (getf notice :summary)
                          :automatic-p t
                          :pending-input-identifier (format nil "completion:~A" (getf notice :id))))
                       (setf notices nil)
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
       (let* ((output (recording-terminal-output terminal))
              (record (find-if (lambda (record)
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
     (let* ((goal (application-goal application))
            (identity "fixture-mission")
            (notice (job-completion-controller-tests--notice application "mission" :mission-id identity)))
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
           (application (mission-test--application
                         configuration :results (loop repeat 3 collect (mission-test--result "done"))))
           (parent (application-agent application))
           (registry (task-augment-tool-registry (application-tool-registry application)))
           (runtime (task-completion--runtime parent))
           (controller (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
      (declare (ignore registry))
      (unwind-protect
           (progn
             (conversation-append-user-message (agent-conversation parent) "Start asynchronous work.")
             (let ((first (job-completion-tests--start parent runtime))
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


(-> test-job-completion-controller-mixed-overflow () null)
(defun test-job-completion-controller-mixed-overflow ()
  "Publish an eligible continuation behind notify-only or stale-mission capacity without user input."
  (dolist (mode '(:notify :stale-mission))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((*task-completion-capacity* 2)
             (*task-completion-history-limit* 4)
             (*task-completion-coalescing-seconds* 0)
             (application (mission-test--application configuration :results (list (mission-test--result "done"))))
             (parent (application-agent application))
             (registry (task-augment-tool-registry (application-tool-registry application)))
             (runtime (task-completion--runtime parent))
             (controller (application-input-controller-create application :start-reader-p nil :load-pending-p nil)))
        (declare (ignore registry))
        (unwind-protect
             (progn
               (conversation-append-user-message (agent-conversation parent) "Start asynchronous work.")
               (when (eq mode ':stale-mission)
                 (application-mission-start application (mission-test--specification)))
               (dotimes (index 2)
                 (declare (ignore index))
                 (job-completion-tests--start parent runtime :policy (if (eq mode ':notify) ':notify ':continue)))
               (when (eq mode ':stale-mission)
                 (application-mission-start application (mission-test--specification)))
               (let* ((job (task-orchestrator-start-execution-job
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
