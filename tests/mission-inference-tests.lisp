(in-package #:autolith)

;;;; -- Concurrent Mission Inference Fixtures --

(defclass mission-inference-test-provider (model-provider)
  ((entered :initarg :entered :reader mission-inference-test-provider-entered
            :type t :documentation "Semaphore signaled at each provider call.")
   (release :initarg :release :reader mission-inference-test-provider-release
            :type t :documentation "Semaphore holding blocked provider calls.")
   (result :initarg :result :reader mission-inference-test-provider-result
           :type provider-result :documentation "Result returned by the provider.")
   (failure :initarg :failure :initform nil
            :reader        mission-inference-test-provider-failure
            :type          (or null function)
            :documentation "Optional function called to raise a provider failure.")
   (blocked-p :initarg :blocked-p :initform t
              :reader mission-inference-test-provider-blocked-p
              :type boolean :documentation "Whether calls wait for RELEASE.")
   (seen-output-cap :initform nil :accessor mission-inference-test-provider-seen-output-cap
                    :documentation "Internal output reservations observed at the provider boundary.")
   (observation-lock :initform      (bordeaux-threads:make-lock "Inference provider observations")
                     :reader        mission-inference-test-provider-observation-lock
                     :documentation "Protects the shared observation list."))
  (:documentation "A provider whose requests can be held at the protocol boundary."))

(defmethod provider-stream-turn
    ((provider mission-inference-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Synchronize a test provider request at the real streaming boundary."
  (declare (ignore conversation tool-namespaces event-callback goal-context compaction-p))
  (bordeaux-threads:with-lock-held
      ((mission-inference-test-provider-observation-lock provider))
    (push *provider-mission-output-allowance*
          (mission-inference-test-provider-seen-output-cap provider)))
  (bordeaux-threads:signal-semaphore
   (mission-inference-test-provider-entered provider))
  (when (mission-inference-test-provider-blocked-p provider)
    (unless (bordeaux-threads:wait-on-semaphore
             (mission-inference-test-provider-release provider) :timeout 5)
      (error "Timed out waiting for the inference test release.")))
  (if (mission-inference-test-provider-failure provider)
      (funcall (mission-inference-test-provider-failure provider))
      (mission-inference-test-provider-result provider)))

(defclass mission-inference-test-thread (standard-object)
  ((thread :initarg :thread :reader mission-inference-test-thread-thread
           :type t :documentation "Worker thread running one inference.")
   (values :initform nil :accessor mission-inference-test-thread-values
           :documentation "Multiple values returned by the worker.")
   (condition :initform nil :accessor mission-inference-test-thread-condition
              :type          (or null condition)
              :documentation "Condition caught at the worker boundary."))
  (:documentation "Result cell for a bounded inference worker."))

(-> mission-inference-test--application (configuration model-provider) application)
(defun mission-inference-test--application (configuration provider)
  "Create a mission application and replace its provider with PROVIDER."
  (let ((application (mission-test--application configuration)))
    (setf (application-provider application) provider)
    application))

(-> mission-inference-test--call (model-provider conversation) provider-result)
(defun mission-inference-test--call (provider conversation)
  "Call PROVIDER through the executable streaming protocol."
  (provider-stream-turn provider conversation :tool-namespaces #()
                        :event-callback                        #'identity))

(-> mission-inference-test--start (function) mission-inference-test-thread)
(defun mission-inference-test--start (thunk)
  "Run THUNK in a worker while retaining returned values and conditions."
  (let ((cell (make-instance 'mission-inference-test-thread :thread nil)))
    (setf (slot-value cell 'thread)
          (bordeaux-threads:make-thread
           (lambda ()
             (handler-case
                 (setf (mission-inference-test-thread-values cell)
                       (multiple-value-list (funcall thunk)))
               (error (caught)
                 (setf (mission-inference-test-thread-condition cell) caught))))))
    cell))

(-> mission-inference-test--join (list bordeaux-threads:semaphore (integer 0)) list)
(defun mission-inference-test--join (cells release count)
  "Release COUNT calls and join CELLS, returning their result cells."
  (dotimes (i count)
    (declare (ignorable i))
    (bordeaux-threads:signal-semaphore release))
  (dolist (cell cells)
    (test-assert
     (sb-thread:join-thread
      (mission-inference-test-thread-thread cell) :timeout 10 :default nil)
     "inference worker terminated")
    nil)
  cells)

(-> test-mission-inference-overlap-and-settlement () null)
(defun test-mission-inference-overlap-and-settlement ()
  "Two inherited conversations overlap and settle reservations exactly once."
  (with-test-configuration (configuration)
    (let* ((entered      (bordeaux-threads:make-semaphore :count 0))
           (release      (bordeaux-threads:make-semaphore :count 0))
           (provider     (make-instance 'mission-inference-test-provider
                                        :entered entered :release release
                                        :result (mission-test--result "done")))
           (application  (mission-inference-test--application configuration provider))
           (context      nil)
           (parent       (application-conversation application))
           (first-child  (conversation-create configuration))
           (second-child (conversation-create configuration))
           (cells        nil))
      (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
      (setf context (mission-context-find parent))
      (mission-context-inherit parent first-child)
      (mission-context-inherit parent second-child)
      (unwind-protect
           (progn
             (setf cells
                   (list (mission-inference-test--start
                          (lambda () (mission-inference-test--call provider first-child)))
                         (mission-inference-test--start
                          (lambda () (mission-inference-test--call provider second-child)))))
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "first provider call entered")
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "second provider call entered")
             (let ((goal (mission-context-goal context)))
               (test-assert (= (getf goal :requests-outstanding) 2) "exactly two provider requests are outstanding")
               (test-assert (= (getf goal :tokens-reserved) 50) "each request reserves its remaining-turn share")
               (test-assert (= (getf goal :turns-used) 2) "each provider call reserves one turn")))
        (mission-inference-test--join cells release 2))
      (test-assert (every (lambda (cell) (null (mission-inference-test-thread-condition cell))) cells)
                   "both provider calls complete normally")
      (let ((goal (mission-context-goal context)))
        (test-assert (zerop (getf goal :requests-outstanding)) "settlement releases request reservations")
        (test-assert (zerop (getf goal :tokens-reserved)) "settlement releases output reservations")
        (test-assert (= 10 (getf goal :tokens-used)) "actual usage is charged once per provider call")
        nil))))

(-> test-mission-inference-turn-contention () null)
(defun test-mission-inference-turn-contention ()
  "A second request cannot consume a turn already reserved by a live request."
  (with-test-configuration (configuration)
    (let* ((entered     (bordeaux-threads:make-semaphore :count 0))
           (release     (bordeaux-threads:make-semaphore :count 0))
           (provider    (make-instance 'mission-inference-test-provider
                                       :entered entered :release release
                                       :result (mission-test--result "done")))
           (application (mission-inference-test--application configuration provider))
           (context nil) (conversation (application-conversation application))
           (cell nil) (caught nil))
      (application-mission-start application (mission-test--specification :turns 1 :tokens 100))
      (setf context (mission-context-find conversation))
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call provider conversation))))
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "first request entered")
             (handler-case
                 (mission-inference-test--call provider conversation)
               (mission-error (condition) (setf caught condition)))
             (test-assert (and caught (eq ':exhausted (mission-error-reason caught)))
                          "turn contention rejects without cancelling the live request")
             (test-assert (eq ':active (getf (mission-context-goal context) :status))
                          "turn contention preserves the admitted request"))
        (mission-inference-test--join (list cell) release 1)))))

(-> test-mission-inference-verification-pending () null)
(defun test-mission-inference-verification-pending ()
  "Verification records pending work while a provider request is outstanding."
  (with-test-configuration (configuration)
    (let* ((entered      (bordeaux-threads:make-semaphore :count 0))
           (release      (bordeaux-threads:make-semaphore :count 0))
           (provider     (make-instance 'mission-inference-test-provider
                                        :entered entered :release release
                                        :result (mission-test--result "done")))
           (application  (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (context nil) (cell nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find conversation))
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call provider conversation))))
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "request entered")
             (application-mission-verify application)
             (test-assert (find ':pending-work (getf (mission-context-goal context) :evidence)
                                :key (lambda (entry) (getf entry :kind)))
                          "verification records outstanding inference"))
        (mission-inference-test--join (list cell) release 1)))))

(-> test-mission-inference-review-accounting () null)
(defun test-mission-inference-review-accounting ()
  "Ordinary and bound reviewer provider calls overlap and charge both policies."
  (with-test-configuration (configuration)
    (let* ((entered      (bordeaux-threads:make-semaphore :count 0))
           (release      (bordeaux-threads:make-semaphore :count 0))
           (provider     (make-instance 'mission-inference-test-provider
                                        :entered entered :release release
                                        :result (mission-test--result "review")))
           (application  (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (reviewer     (conversation-create configuration))
           (context nil) (policy nil) (cells nil))
      (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
      (setf context (mission-context-find conversation)
            policy (list :id "review" :turn-limit 2 :token-limit 50 :turns-used 0
                         :tokens-used 0 :tokens-reserved 0 :requests-outstanding 0))
      (mission-context-inherit conversation reviewer)
      (mission-review-bind-checkpoint (list :policy policy) reviewer)
      (unwind-protect
           (progn
             (setf cells
                   (list (mission-inference-test--start
                          (lambda () (mission-inference-test--call provider conversation)))
                         (mission-inference-test--start
                          (lambda () (mission-inference-test--call provider reviewer)))))
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "ordinary provider call entered")
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "bound reviewer provider call entered")
             (test-assert (= (getf (mission-context-goal context) :requests-outstanding) 2)
                          "ordinary and reviewer calls overlap"))
        (mission-inference-test--join cells release 2))
      (test-assert (every (lambda (cell) (null (mission-inference-test-thread-condition cell))) cells)
                   "ordinary and reviewer calls complete normally")
      (test-assert (= 10 (getf (mission-context-goal context) :tokens-used))
                   "the mission charges both provider calls once")
      (test-assert (= 5 (getf policy :tokens-used)) "review policy charges its usage")
      (test-assert (and (zerop (getf policy :tokens-reserved))
                        (zerop (getf policy :requests-outstanding)))
                   "review policy reservations settle exactly once"))))

(-> test-mission-inference-unknown-and-interruption-cleanup () null)
(defun test-mission-inference-unknown-and-interruption-cleanup ()
  "Unknown usage, cancellation, nonlocal exits and usage errors clean reservations."
  (with-test-configuration (configuration)
    (let* ((provider     (make-instance 'mission-inference-test-provider
                                        :entered (bordeaux-threads:make-semaphore :count 0)
                                        :release (bordeaux-threads:make-semaphore :count 1)
                                        :blocked-p nil :result (mission-test--result "unknown" :usage (json-object))))
           (application  (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (context      nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find conversation))
      (mission-inference-test--call provider conversation)
      (let ((goal (mission-context-goal context)))
        (test-assert (eq ':blocked (getf goal :status)) "unknown usage blocks the mission")
        (test-assert (zerop (getf goal :tokens-reserved)) "unknown usage releases reservation")
        (test-assert (zerop (getf goal :requests-outstanding)) "unknown usage releases request")))
    (let* ((entered      (bordeaux-threads:make-semaphore :count 0))
           (release      (bordeaux-threads:make-semaphore :count 0))
           (provider     (make-instance 'mission-inference-test-provider
                                        :entered entered :release release
                                        :result (mission-test--result "cancelled")))
           (application  (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (context nil) (cell nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find conversation))
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call provider conversation))))
             (test-assert (bordeaux-threads:wait-on-semaphore entered :timeout 5)
                          "cancellable request entered")
             (application-mission-stop application ':cancelled "test cancellation"))
        (mission-inference-test--join (list cell) release 1))
      (test-assert (eq ':cancelled (getf (mission-context-goal context) :status))
                   "cancellation is retained while the call settles")
      (test-assert (zerop (getf (mission-context-goal context) :tokens-reserved))
                   "cancellation releases output reservation"))
    (let* ((application (mission-inference-test--application
                         configuration
                         (make-instance 'mission-inference-test-provider
                                         :entered (bordeaux-threads:make-semaphore :count 0)
                                         :release (bordeaux-threads:make-semaphore :count 1)
                                         :blocked-p nil :result (mission-test--result "error"))))
           (context nil) (caught nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find (application-conversation application)))
      (catch ':test-interrupted
        (mission--account-inference context (lambda () (throw ':test-interrupted t)) #'identity))
      (test-assert (eq ':blocked (getf (mission-context-goal context) :status))
                   "a caught nonlocal provider exit blocks the mission")
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find (application-conversation application)))
      (handler-case
          (mission--account-inference context (lambda () (mission-test--result "usage"))
                                      (lambda (values) (declare (ignore values))
                                        (error "usage failure")))
        (error (condition) (setf caught condition)))
      (test-assert caught "usage-function failure is caught")
      (test-assert (zerop (getf (mission-context-goal context) :tokens-reserved))
                   "usage failure leaves no reservation"))))

(-> test-mission-inference-output-reservation-cap () null)
(defun test-mission-inference-output-reservation-cap ()
  "Output reservation follows remaining turns and binds an explicit provider cap."
  (with-test-configuration (configuration)
    (let* ((provider    (make-instance 'mission-inference-test-provider
                                       :entered (bordeaux-threads:make-semaphore :count 0)
                                       :release (bordeaux-threads:make-semaphore :count 1)
                                       :blocked-p nil :result (mission-test--result "capped")))
           (application (mission-inference-test--application configuration provider))
           (context     nil))
      (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
      (setf context (mission-context-find (application-conversation application)))
      (let ((*provider-maximum-output-tokens* 7))
        (mission-inference-test--call provider (application-conversation application)))
      (let ((goal (mission-context-goal context)))
        (test-assert (member 7 (mission-inference-test-provider-seen-output-cap provider))
                     "provider observes the reserved explicit output cap")
        (test-assert (= 5 (getf goal :tokens-used)) "settlement charges usage once, not the ceiling")
        (test-assert (zerop (getf goal :tokens-reserved)) "explicit ceiling is released")))))

(-> test-mission-inference-reservation-recovery () null)
(defun test-mission-inference-reservation-recovery ()
  "Replay old counters and conservatively recover shared reviewer reservations."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (application-mission-start application (mission-test--specification))
      (application-mission-review-configure
       application '(:id "review" :phase :final-acceptance :inputs ("artifact")
                     :context "Inspect the artifact." :turn-limit 2 :token-limit 50 :run-limit 1))
      (let* ((goal   (application-goal application))
             (policy (first (getf goal :reviews))))
        (setf (getf goal :turns-used) 2
              (getf goal :requests-outstanding) 2
              (getf policy :turns-used) 1
              (getf policy :tokens-reserved) 25
              (getf policy :requests-outstanding) 1)
        (remf goal :tokens-reserved)
        (application--record-goal application))
      (application--load-goal application)
      (let* ((goal   (application-goal application))
             (policy (first (getf goal :reviews))))
        (test-assert (eq ':blocked (getf goal :status)) "interrupted inference restores as blocked")
        (test-assert (= 2 (getf goal :unknown-usage)) "each interrupted request retains unknown usage")
        (test-assert (= 2 (getf goal :turns-used)) "admitted turns are retained during recovery")
        (test-assert (and (zerop (getf goal :tokens-reserved))
                          (zerop (getf goal :requests-outstanding))
                          (zerop (getf policy :tokens-reserved))
                          (zerop (getf policy :requests-outstanding)))
                     "recovery releases goal and reviewer reservations"))
      (application--load-goal application)
      (test-assert (= 2 (getf (application-goal application) :unknown-usage))
                   "a second replay does not charge interrupted requests twice"))))

(defclass mission-inference-test-child-provider (task-test-provider) ()
  (:documentation "Use the task script without forwarding through a second request boundary."))

(defmethod provider-with-configuration
    ((provider mission-inference-test-child-provider) (configuration configuration))
  "Keep one provider invocation per actual child request."
  (declare (ignore configuration))
  provider)

(defclass mission-inference-test-drain-tool (task-test-child-safe-tool)
  ((entered :initform (bt:make-semaphore :count 0) :reader mission-inference-test-drain-entered
            :documentation "Signals entry into the child tool.")
   (release :initform (bt:make-semaphore :count 0) :reader mission-inference-test-drain-release
            :documentation "Permits the child tool to finish.")
   (agent :accessor      mission-inference-test-drain-agent
          :documentation "The executing child for testing further admission."))
  (:documentation "Hold a real child tool across an unrelated unknown settlement."))

(defmethod tool-execute ((tool mission-inference-test-drain-tool)
                         (context tool-context) (arguments hash-table))
  "Release the child only after its sibling request has settled."
  (declare (ignore arguments))
  (setf (mission-inference-test-drain-agent tool) (tool-context-agent context))
  (bt:signal-semaphore (mission-inference-test-drain-entered tool))
  (unless (bt:wait-on-semaphore (mission-inference-test-drain-release tool) :timeout 10)
    (error "Timed out releasing the draining child."))
  (tool-success "released"))

(-> test-mission-inference-unknown-drains-child () null)
(defun test-mission-inference-unknown-drains-child ()
  "Unknown request usage blocks admissions while an admitted child finishes."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((application  (mission-test--application configuration))
           (registry     (application-tool-registry application))
           (provider     (make-instance 'mission-inference-test-child-provider :mode ':blocking-tool))
           (tool         (make-instance 'mission-inference-test-drain-tool
                                        :namespace "test" :name "block"
                                        :description "Hold the admitted child."
                                        :parameters  (tool-object-schema (json-object) nil)))
           (orchestrator (task-orchestrator-create))
           (definition   (task-agent-definition-create
                          :name "mission-child" :description "Finish admitted work"
                          :instructions "Call the blocking tool, then submit success."
                          :tools ':all :source ':test))
           (entry        (list :definition definition :detached t
                               :item (list :agent "mission-child" :task "Finish admitted work.")))
           (job          nil))
      (setf (agent-provider (application-agent application)) provider)
      (tool-registry-register registry tool)
      (task-augment-tool-registry registry)
      (tool-registry-bind-runtime registry 'task-orchestrator orchestrator)
      (application-mission-start application (mission-test--specification))
      (unwind-protect
           (progn
             (setf job (first (task-orchestrator-start-jobs
                               orchestrator (application-agent application) (list entry))))
             (test-assert (bt:wait-on-semaphore (mission-inference-test-drain-entered tool)
                                                :timeout 10)
                          "child reaches a real tool before usage becomes unknown")
             (let ((failure (make-instance 'simple-error :format-control "provider disconnected")))
               (test-assert
                (search "provider disconnected"
                    (handler-case
                        (mission-inference-test--call
                         (make-instance 'mission-inference-test-provider
                                        :entered (bt:make-semaphore :count 0)
                                        :release (bt:make-semaphore :count 0)
                                        :blocked-p nil :result (mission-test--result "unused")
                                        :failure (lambda () (error failure)))
                         (application-conversation application))
                      (error (condition) (princ-to-string condition))))
                "the original provider failure report reaches its caller"))
             (test-assert (eq :blocked (getf (application-goal application) :status))
                          "unknown usage blocks rather than fails the mission")
             (test-assert (not (job-cancellation-requested-p job))
                          "the already-running child is not cancelled")
             (let ((order (task-orchestrator-next-session-order orchestrator)))
               (test-assert
                (handler-case
                    (progn (task-orchestrator-start-jobs
                            orchestrator (mission-inference-test-drain-agent tool) (list entry)) nil)
                  (mission-error (condition) (eq :blocked (mission-error-reason condition))))
                "a bound child cannot admit new mission work")
               (test-assert (= order (task-orchestrator-next-session-order orchestrator))
                            "refusal consumes no session identity"))
             (bt:signal-semaphore (mission-inference-test-drain-release tool))
             (task-job-await job nil)
             (let* ((snapshot (task-job-snapshot job))
                    (result   (getf snapshot :result)))
               (test-assert (and (eq :completed (getf snapshot :state))
                                 (eq :success (getf result :status))
                                 (getf result :yielded-p))
                            "the child continues inference and yields success after the block"))
             (let ((goal (application-goal application)))
               (test-assert (eq :blocked (getf goal :status)) "draining does not reactivate the mission")
               (test-assert (= 1 (getf goal :unknown-usage)) "unknown spend is retained")
               (test-assert
                (search "provider disconnected"
                        (or (getf (find :inference-failure (getf goal :evidence)
                                        :key               (lambda (entry) (getf entry :kind))) :text) ""))
                "failure evidence retains the provider report")
               (test-assert (= 4 (getf goal :tokens-used))
                            (format nil "both child requests settle known usage: ~D" (getf goal :tokens-used)))
               (test-assert (= 3 (getf goal :turns-used))
                            (format nil "requests are charged once: ~D" (getf goal :turns-used)))
               (test-assert (and (zerop (getf goal :tokens-reserved))
                                 (zerop (getf goal :requests-outstanding)))
                            "all reservations and outstanding requests settle")))
        (bt:signal-semaphore (mission-inference-test-drain-release tool))
        (when job (task-job-await job nil))
        (task-orchestrator-close orchestrator)))))

(-> test-mission-inference-cancelling-block-refuses-continuation () null)
(defun test-mission-inference-cancelling-block-refuses-continuation ()
  "Refuse child continuation as soon as a cancelling block publishes its status."
  (dolist (quarantined-p '(nil t))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((application   (mission-test--application configuration))
             (registry      (application-tool-registry application))
             (provider      (make-instance 'mission-inference-test-child-provider :mode ':blocking-tool))
             (tool          (make-instance 'mission-inference-test-drain-tool
                                           :namespace "test" :name "block"
                                           :description "Hold the admitted child."
                                           :parameters  (tool-object-schema (json-object) nil)))
             (orchestrator  (task-orchestrator-create))
             (definition    (task-agent-definition-create
                             :name "mission-child" :description "Finish admitted work"
                             :instructions "Call the blocking tool, then submit success."
                             :tools ':all :source ':test))
             (entry         (list :definition definition :detached t
                                  :item (list :agent "mission-child" :task "Finish admitted work.")))
             (record-status (symbol-function 'mission-knowledge-record-status))
             (published-p   nil)
             (job           nil))
        (setf (agent-provider (application-agent application)) provider)
        (tool-registry-register registry tool)
        (task-augment-tool-registry registry)
        (tool-registry-bind-runtime registry 'task-orchestrator orchestrator)
        (application-mission-start application (mission-test--specification))
        (unwind-protect
             (progn
               (setf job (first (task-orchestrator-start-jobs
                                 orchestrator (application-agent application) (list entry))))
               (test-assert (bt:wait-on-semaphore (mission-inference-test-drain-entered tool)
                                                :timeout 10)
                            "child reaches a real tool before the cancelling block")
               (let ((context (mission-context-find (application-conversation application))))
                 (when quarantined-p
                   (mission--transition context ':blocked "Unknown usage." :cancel-p nil))
                 (test-call-with-function-replacements
                  (list (list 'mission-knowledge-record-status
                              (lambda (published-context status)
                                (when (and (eq published-context context) (eq status ':blocked))
                                  (setf published-p t)
                                  (test-assert (eq :blocked (getf (application-goal application) :status))
                                               "the cancelling status is already published")
                                  (test-assert (not (job-cancellation-requested-p job))
                                               "continuation is tested before job cancellation")
                                  (test-assert (mission-agent-terminal-p
                                                (mission-inference-test-drain-agent tool))
                                               "the child is terminal before cancellation reaches its job")
                                  (let ((*mission-admitted-operation* job))
                                    (dolist (inference-p '(nil t))
                                      (test-assert
                                       (handler-case
                                           (progn (mission--admit context :inference-p inference-p) nil)
                                         (mission-error (condition)
                                           (eq :blocked (mission-error-reason condition))))
                                       "both tool and inference continuation refuse the cancelling block")))
                                  (bt:signal-semaphore (mission-inference-test-drain-release tool))
                                  (task-job-await job nil)
                                  (test-assert (= 1 (getf (application-goal application) :turns-used))
                                               "the released child cannot admit another provider request")
                                  (test-assert (not (getf (getf (task-job-snapshot job) :result) :yielded-p))
                                               "the child cannot continue to yield success"))
                                (funcall record-status published-context status))))
                  (lambda ()
                    (application-mission-stop application ':blocked "Explicit blocker."))))
               (test-assert published-p "the test reached status publication before cancellation"))
          (bt:signal-semaphore (mission-inference-test-drain-release tool))
          (when job (task-job-await job nil))
          (task-orchestrator-close orchestrator))))))

(-> test-mission-inference-cancel-before-dispatch () null)
(defun test-mission-inference-cancel-before-dispatch ()
  "Cancel the real inline inference job before it can invoke its provider."
  (with-test-configuration (configuration)
    (let* ((provider    (make-instance 'mission-inference-test-provider
                                    :entered (bt:make-semaphore :count 0)
                                    :release (bt:make-semaphore :count 0)
                                    :blocked-p nil :result (mission-test--result "unused")))
           (application (mission-inference-test--application configuration provider))
           (run-inline  (symbol-function 'job-run-inline)))
      (application-mission-start application (mission-test--specification))
      (test-call-with-function-replacements
       (list (list 'job-run-inline
                   (lambda (job)
                     (job-cancel job :reason ':test-before-dispatch)
                     (funcall run-inline job))))
       (lambda ()
         (test-assert
          (handler-case
              (progn (mission-inference-test--call provider (application-conversation application)) nil)
            (mission-error () t))
          "the cancelled inline request is refused")))
      (let ((goal (application-goal application)))
        (test-assert (null (mission-inference-test-provider-seen-output-cap provider))
                     "the provider was never dispatched")
        (test-assert (eq :active (getf goal :status)) "pre-dispatch cancellation leaves the mission active")
        (test-assert (and (zerop (getf goal :unknown-usage))
                          (zerop (getf goal :tokens-used)))
                     "an undispatched request has known zero usage")
        (test-assert (and (= 1 (getf goal :turns-used))
                          (zerop (getf goal :tokens-reserved))
                          (zerop (getf goal :requests-outstanding)))
                     "the admitted request settles exactly once")))))


;;;; -- Child UNKNOWN Audit and Reservation Settlement --

(defclass mission-child-unknown-test-provider (mission-inference-test-child-provider)
  ((attempts :initform 0 :accessor mission-child-unknown-test-attempts
             :documentation "External generation attempts made by the real retry loop.")
   (owners :initform nil :accessor mission-child-unknown-test-owners
           :documentation "Synthetic provider entry job and conversation identities.")
   (reservations :initform nil :accessor mission-child-unknown-test-reservations
                 :documentation "Admitted accounting allocations seen at provider entry.")
   (outer-conversation :initform nil :accessor mission-child-unknown-test-outer-conversation
                       :documentation "The exact initial child conversation for nested stimuli."))
  (:documentation "Fail a native child at the deterministic external provider seam."))

(defmethod provider-stream-turn
    ((provider mission-child-unknown-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Exercise the pinned retry classifier inside the ordinary accounting boundary."
  (declare (ignorable conversation tool-namespaces goal-context compaction-p))
  (let ((mode (slot-value provider 'mode)))
    (unless (mission-child-unknown-test-outer-conversation provider)
      (setf (mission-child-unknown-test-outer-conversation provider) conversation))
    (push *provider-mission-output-allowance* (mission-child-unknown-test-reservations provider))
    (push (list (and (typep *mission-admitted-operation* 'task-job)
                     (job-identifier *mission-admitted-operation*))
                (conversation-identifier conversation))
          (mission-child-unknown-test-owners provider))
    (cond
      ((and (eq mode ':nested)
            (eq conversation (mission-child-unknown-test-outer-conversation provider)))
       (call-next-method))
      ((eq mode ':primary-success)
       (incf (mission-child-unknown-test-attempts provider))
       (mission-test--result "Same mission continued."))
      ((eq mode ':provider-error)
       (incf (mission-child-unknown-test-attempts provider))
       (error "Synthetic non-length provider failure."))
      (t
       (cl-llm-provider-api:call-with-streaming-retries
        (lambda (callback)
          (incf (mission-child-unknown-test-attempts provider))
          (when (eq mode ':quota)
            (provider--codex-signal-status-failure
             (make-instance 'codex-subscription-provider) 429
             :headers  '(("x-request-id" . "synthetic-quota") ("retry-after" . "1"))
             :raw-body "{\"error\":{\"type\":\"usage_limit_reached\",\"code\":\"insufficient_quota\",\"message\":\"Synthetic local quota exhausted\",\"plan_type\":\"stub\"}}"))
          (funcall callback (make-instance 'assistant-delta-event :text "partial"))
          (if (or (member mode '(:numeric-success :numeric-failure))
                  (and (eq mode ':retry-success) (= 3 (mission-child-unknown-test-attempts provider))))
              (agent-test-result
               "synthetic-numeric"
               (list (agent-test-call
                      :call-id "synthetic-yield" :namespace "yield" :name "submit"
                      :arguments (json-encode
                                  (if (eq mode ':numeric-failure)
                                      (json-object "status" "failed" "text" "Child failed."
                                                   "error" "Synthetic known-usage failure.")
                                      (json-object "status" "success" "text" "Child finished.")))))
               :usage (json-object "input_tokens" 2 "output_tokens" 3))
              (cl-llm-provider-api::provider--signal-incomplete-response
               (json-object "type" "response.incomplete"
                            "response" (json-object "id" "synthetic-response" "status" "incomplete"
                                                    "incomplete_details" (json-object "reason" "max_output_tokens")
                                                    "output" #())))))
        event-callback :maximum-retries 2 :maximum-streaming-retries 2
        :sleep-function (lambda (delay) (declare (ignore delay)) nil))))))

(-> mission-child-unknown-test--continuation (keyword) null)
(defun mission-child-unknown-test--continuation (mode)
  "Observe the entire native failure and spending-continuation chain before assertions."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore path commands executions))
     (let* ((registry     (task-augment-tool-registry (application-tool-registry application)))
            (provider     (make-instance 'mission-child-unknown-test-provider :mode mode))
            (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
            (definition   (task-agent-definition-create
                           :name "bounded-child" :description "Exercise missing usage"
                           :instructions "Attempt one turn." :tools ':all :source ':test))
            (entry        (list :definition definition :detached t
                                :item (list :agent "bounded-child" :task "Attempt one turn.")))
            (goal         (application-goal application))
            (identity nil) (job nil) (next-child nil)
            (primary-error nil) (child-error nil) (wake-error nil) (duplicate-error nil)
            (ticket nil) (failed-attempts 0) (reservation 0) (result nil)
            (after-failure nil) (after-primary 0) (after-child 0) (after-wake 0))
       (setf (application-provider application) provider
             (agent-provider (application-agent application)) provider
             (slot-value service 'run) #'mission-schedule--run)
       (application-mission-schedule-add application :id "recurring" :content "Inspect state"
                                        :at 110 :interval 10)
       (setf identity (list (getf goal :schedule-identity) (getf goal :schedule-version)))
       ;; Legacy optional metadata must be appended without replacing GOAL.
       (dolist (key '(:unknown-usage-by-child :reservation-charged-tokens :reservation-charges-by-child))
         (remf goal key))
       (unwind-protect
            (progn
              (setf job (first (task-orchestrator-start-jobs
                                orchestrator (application-agent application) (list entry))))
              (task-job-await job nil)
              (setf result (getf (task-job-snapshot job) :result)
                    failed-attempts (mission-child-unknown-test-attempts provider)
                    reservation (first (mission-child-unknown-test-reservations provider))
                    after-failure (copy-tree goal)
                    (slot-value provider 'mode) ':primary-success)
              (handler-case (application--run-turn application "Continue same mission" :continuation-p t)
                (error (condition) (setf primary-error condition)))
              (setf after-primary (mission-child-unknown-test-attempts provider)
                    (slot-value provider 'mode) ':numeric-success)
              (handler-case
                  (progn
                    (setf next-child (first (task-orchestrator-start-jobs
                                             orchestrator (application-agent application) (list entry))))
                    (task-job-await next-child nil))
                (error (condition) (setf child-error condition)))
              (setf after-child (mission-child-unknown-test-attempts provider)
                    (slot-value provider 'mode) ':primary-success)
              (funcall clock 110)
              (let ((wakeups (application-mission-schedules-wake application)))
                (setf ticket (and wakeups (mission-schedule-tests--ticket (first wakeups)))))
              (when ticket
                (handler-case (application-operation-call application "mission-wakeup" ticket)
                  (error (condition) (setf wake-error condition)))
                (setf after-wake (mission-child-unknown-test-attempts provider))
                (handler-case (application-operation-call application "mission-wakeup" ticket)
                  (error (condition) (setf duplicate-error condition))))
              (format t "~&BOUNDED-OBSERVATIONS ~S~%"
                      (list :mode mode :attempts failed-attempts :reservation reservation
                            :shared-unknown  (getf after-failure :unknown-usage)
                            :primary-error   (and primary-error (princ-to-string primary-error))
                            :child-error     (and child-error (princ-to-string child-error))
                            :wake-error      (and wake-error (princ-to-string wake-error))
                            :provider-counts (list failed-attempts after-primary after-child after-wake)))
              ;; First oracle is the rejected policy's visible pre-provider refusal, not new metadata.
              (test-assert (and (null primary-error) (= (1+ failed-attempts) after-primary))
                           "same-mission primary inference reaches the provider without reauthorization")
              (test-assert (and (null child-error) next-child (= (1+ after-primary) after-child)
                                (eq ':success (getf (getf (task-job-snapshot next-child) :result) :status)))
                           "new child admission and its provider work succeed on the same mission")
              (test-assert (and ticket (null wake-error) (= (1+ after-child) after-wake))
                           "native due recurrence obtains measured provider usage through the default runner")
              (test-assert (and duplicate-error (= after-wake (mission-child-unknown-test-attempts provider)))
                           "duplicate ticket dispatches and charges nothing")
              (test-assert (and (eq ':failed (getf result :status)) (getf result :error))
                           "child failure and original report survive settlement")
              (test-assert (= (if (eq mode ':quota) 1 3) failed-attempts)
                           "pinned classifier makes quota terminal and length retry three generations")
              (test-assert (and (typep reservation '(integer 1))
                                (= reservation (getf after-failure :tokens-used))
                                (zerop (getf after-failure :unknown-usage))
                                (zerop (getf after-failure :requests-outstanding))
                                (zerop (getf after-failure :tokens-reserved)))
                           "one full allocation replaces one reservation with no new unresolved UNKNOWN")
              (test-assert (and (= reservation (getf after-failure :reservation-charged-tokens 0))
                                (= 1 (getf result :unknown-usage 0))
                                (= 1 (getf result :reservation-charge-requests 0))
                                (= reservation (getf result :reservation-charged-tokens 0))
                                (null (getf result :usage)))
                           "terminal compact summary distinguishes audit and charged allocation from measured usage")
              (test-assert (= (+ reservation 15) (getf goal :tokens-used))
                           "subsequent primary, child and recurrence each add only measured five tokens")
              (test-assert (and (eq goal (application-goal application))
                                (eq ':active (getf goal :status))
                                (equal identity (list (getf goal :schedule-identity) (getf goal :schedule-version))))
                           "legacy additions and all continuations preserve active mission identity and version")
              (let* ((pathname (getf result :output-path))
                     (before   (uiop:read-file-string pathname))
                     (artifact (task-tests--read-exact-native-value before)))
                (test-assert (and (eq ':failed (getf artifact :status))
                                  (= 1 (getf artifact :unknown-usage 0))
                                  (= reservation (getf artifact :reservation-charged-tokens 0))
                                  (= 1 (getf artifact :reservation-charge-requests 0)))
                             "durable native artifact retains failure UNKNOWN and full allocation charge")
                (task-tests--publish-terminal job ':failed result)
                (test-assert (and (string= before (uiop:read-file-string pathname))
                                  (= reservation (getf goal :reservation-charged-tokens 0)))
                             "duplicate terminal publication neither rewrites nor settles again"))
              (application--load-goal application)
              (let ((restored (application-goal application)))
                (test-assert (and (eq ':active (getf restored :status))
                                  (zerop (getf restored :unknown-usage))
                                  (= reservation (getf restored :reservation-charged-tokens 0))
                                  (getf restored :reservation-charges-by-child)
                                  (getf restored :unknown-usage-by-child))
                             "ordinary durable reload retains charge and audit with no quarantine")
                (application--load-goal application)
                (test-assert (= (getf restored :tokens-used) (getf (application-goal application) :tokens-used))
                             "completed restore does not settle twice")))
         (when job (task-job-await job nil))
         (when next-child (task-job-await next-child nil))
         (task-orchestrator-close orchestrator)))))
  nil)

(-> test-mission-child-bounded-unknown-length-continuation () null)
(defun test-mission-child-bounded-unknown-length-continuation ()
  "Continue actual primary, child and native recurrence after length UNKNOWN."
  (mission-child-unknown-test--continuation ':concurrent))

(-> test-mission-child-bounded-unknown-quota-continuation () null)
(defun test-mission-child-bounded-unknown-quota-continuation ()
  "Continue actual primary, child and native recurrence after terminal quota UNKNOWN."
  (mission-child-unknown-test--continuation ':quota))

(-> test-mission-child-unknown-settlement-controls () null)
(defun test-mission-child-unknown-settlement-controls ()
  "Exercise accounting ownership, numeric results and interrupted settlement independently."
  (dolist (mode '(:numeric :failed-after-numeric :unknown :error :nonlocal :cancel :deadline :root :foreign))
    (with-test-configuration (configuration)
      (let* ((application  (mission-test--application configuration))
             (orchestrator (task-orchestrator-create))
             (child        (task-tests--make-job orchestrator :identifier "accounting-child"))
             (goal         nil)
             (context      nil)
             (reservation  0)
             (deadline-p   nil))
        (application-mission-start application (mission-test--specification :tokens most-positive-fixnum))
        (setf goal (application-goal application)
              context (mission-context-find (application-conversation application))
              (slot-value child 'mission-context) (unless (eq mode ':foreign) context))
        (unwind-protect
             (let ((*mission-admitted-operation* (unless (eq mode ':root) child)))
               (handler-case
                   (progn
                     (catch 'mission-child-test-exit
                       (mission--account-inference
                        context
                        (lambda ()
                         (setf reservation *provider-mission-output-allowance*)
                          ;; Settlement must use entry ownership, not this changed dynamic binding.
                         (let ((*mission-admitted-operation* nil))
                           (case mode
                             (:error
                              (error "Synthetic child provider failure."))
                             (:nonlocal
                              (throw 'mission-child-test-exit ':left))
                             (:cancel
                              (error 'job-aborted :message "Synthetic child cancellation."
                                                 :reason   ':test))
                             (:deadline
                              (error 'sb-sys:deadline-timeout :seconds 2))
                             ((:numeric :failed-after-numeric)
                              (json-object "input_tokens" 2 "output_tokens" 3))
                             (otherwise
                              (json-object)))))
                        #'first))
                     (when (eq mode ':failed-after-numeric)
                       (error "Synthetic child failed after reporting numeric usage.")))
                (sb-sys:deadline-timeout () (setf deadline-p t))
                (simple-error () nil)
                (mission-error (condition)
                  (test-assert (and (member mode '(:error :cancel))
                                    (search (if (eq mode ':error)
                                                "Synthetic child provider failure."
                                                "Synthetic child cancellation.")
                                            (princ-to-string condition)))
                               "supervised provider failure retains its report"))
                (job-aborted () nil))
              (when (eq mode ':deadline)
                (test-assert deadline-p "the native deadline escapes outside accounting")
                (test-assert (and (integerp reservation) (plusp reservation)
                                  (eq goal (application-goal application))
                                  (eq ':active (getf goal :status)))
                             "deadline settlement retains the active goal and finite admitted allocation")
                (let ((charge (rest (assoc (job-identifier child)
                                          (getf goal :reservation-charges-by-child) :test #'equal))))
                  (test-assert (and (= 1 (getf charge :requests 0))
                                    (= reservation (getf charge :tokens 0)))
                               "the deadline charges its exact child reservation once")))
              (let ((numeric-p (member mode '(:numeric :failed-after-numeric)))
                    (root-p    (member mode '(:root :foreign))))
                (test-assert (= (cond (numeric-p
                                       5)
                                      (root-p
                                       0)
                                      (t
                                       reservation))
                                (getf goal :tokens-used))
                             "known usage or exact-child full reservation is charged once")
                (test-assert (= (if root-p 1 0) (getf goal :unknown-usage))
                             "bounded child UNKNOWN audit does not add unresolved uncertainty")
                (test-assert (= (if (or numeric-p root-p) 0 reservation)
                                (getf goal :reservation-charged-tokens 0))
                             "reservation metadata is already included in used tokens")
                (test-assert (eq (if root-p ':blocked ':active) (getf goal :status))
                             "only exact captured child ownership isolates the shared lifecycle")
                (test-assert (= (if (or root-p numeric-p) 0 1)
                               (or (rest (assoc (job-identifier child)
                                                (getf goal :unknown-usage-by-child) :test #'equal)) 0))
                             "only the originating child receives a projection of shared uncertainty")
                (test-assert (and (= 1 (getf goal :turns-used))
                                  (zerop (getf goal :tokens-reserved))
                                  (zerop (getf goal :requests-outstanding)))
                             "normal, failure and nonlocal exits release one reservation exactly once")
                (when (and (not numeric-p) (not root-p))
                  (let ((calls 0))
                    (mission--account-inference
                     context (lambda () (incf calls) (json-object "input_tokens" 0 "output_tokens" 0)) #'first)
                    (test-assert (and (= 1 calls) (= 2 (getf goal :turns-used))
                                      (= reservation (getf goal :tokens-used))
                                      (zerop (getf goal :unknown-usage)))
                                 "finite nil-cap share settlement admits subsequent known-zero provider work"))))
              (when (eq mode ':deadline)
                (mission--account-inference
                 context (lambda () (json-object "input_tokens" 2 "output_tokens" 3)) #'first)
                (test-assert (and (eq goal (application-goal application))
                                  (eq ':active (getf goal :status))
                                  (= (+ reservation 5) (getf goal :tokens-used))
                                  (= reservation (getf goal :reservation-charged-tokens))
                                  (= 3 (getf goal :turns-used))
                                  (zerop (getf goal :unknown-usage))
                                  (zerop (getf goal :tokens-reserved))
                                  (zerop (getf goal :requests-outstanding)))
                             "ordinary numeric work follows the deadline and known-zero work without replacement")))
          (task-orchestrator-close orchestrator)))))
  nil)

(-> test-mission-child-unknown-reservation-drain-and-recovery () null)
(defun test-mission-child-unknown-reservation-drain-and-recovery ()
  "Drain a reserved request and restore conservatively without losing attribution."
  (with-test-configuration (configuration)
    (let* ((entered      (bt:make-semaphore :count 0))
           (release      (bt:make-semaphore :count 0))
           (provider     (make-instance 'mission-inference-test-provider
                                        :entered entered :release release :result (mission-test--result "done")))
           (application  (mission-inference-test--application configuration provider))
           (orchestrator (task-orchestrator-create))
           (child        (task-tests--make-job orchestrator :identifier "uncertain-child"))
           (context      nil)
           (cell         nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find (application-conversation application))
            (slot-value child 'mission-context) context)
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call
                                     provider (application-conversation application)))))
             (test-assert (bt:wait-on-semaphore entered :timeout 5) "the other real request owns a reservation")
             (let ((*mission-admitted-operation* child))
               (mission--account-inference context (lambda () (json-object)) #'first))
             (let ((goal (application-goal application)))
               (test-assert (and (= 1 (getf goal :requests-outstanding))
                                 (plusp (getf goal :tokens-reserved))
                                 (zerop (getf goal :unknown-usage)) (eq ':active (getf goal :status)))
                            "the child's settlement does not release the other request's reservation")
               (mission-inference-test--join (list cell) release 1)
               (test-assert (null (mission-inference-test-thread-condition cell)) "already-reserved work settles")
               (test-assert (and (= (+ 5 (getf goal :reservation-charged-tokens)) (getf goal :tokens-used)) (zerop (getf goal :unknown-usage))
                                 (zerop (getf goal :tokens-reserved))
                                 (zerop (getf goal :requests-outstanding)))
                            "draining charges only known usage and retains uncertainty")
               ;; Crash recovery cannot prove ownership of an outstanding request.
               (setf (getf goal :requests-outstanding) 1)
               (application--record-goal application)
               (application--load-goal application)
               (let ((restored (application-goal application)))
                 (test-assert (and (eq ':blocked (getf restored :status))
                                   (= 1 (getf restored :unknown-usage))
                                   (= 1 (rest (assoc (job-identifier child)
                                                    (getf restored :unknown-usage-by-child) :test #'equal))))
                              "crash recovery blocks unproven outstanding work without erasing child attribution")
                 (test-assert
                  (handler-case (progn (application-mission-resume application) nil)
                    (mission-error (condition) (eq ':unknown-usage (mission-error-reason condition))))
                  "explicit resume cannot erase uncertain spend"))
               (application--load-goal application)
               (test-assert (= 1 (getf (application-goal application) :unknown-usage))
                            "repeated crash restore does not add the same uncertainty twice")))
        (bt:signal-semaphore release)
        (when cell (sb-thread:join-thread (mission-inference-test-thread-thread cell) :timeout 10 :default nil))
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-mission-child-unknown-native-outcome-controls () null)
(defun test-mission-child-unknown-native-outcome-controls ()
  "Compare actual child numeric, failed, retry-success and nested ownership outcomes."
  (dolist (mode '(:numeric-success :numeric-failure :retry-success :provider-error :nested))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((application  (mission-test--application configuration))
             (registry     (task-augment-tool-registry (application-tool-registry application)))
             (provider     (make-instance 'mission-child-unknown-test-provider :mode mode))
             (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
             (definition   (task-agent-definition-create
                            :name "outcome-child" :description "Exercise native outcome"
                            :instructions "Return the requested outcome." :tools ':all :spawns ':all :source ':test))
             (entry        (list :definition definition :detached t
                                 :item (list :agent "outcome-child" :task "Return the requested outcome.")))
             (job          nil))
        (setf (application-provider application) provider
              (agent-provider (application-agent application)) provider)
        (application-mission-start application (mission-test--specification))
        (unwind-protect
             (progn
               (setf job (first (task-orchestrator-start-jobs
                                 orchestrator (application-agent application) (list entry))))
               (task-job-await job nil)
               (let* ((goal    (application-goal application))
                      (result  (getf (task-job-snapshot job) :result))
                      (unknown (member mode '(:provider-error :nested)))
                      (failed  (member mode '(:numeric-failure :provider-error))))
                 (format t "~&NATIVE-OUTCOME ~S~%" (list mode :status (getf result :status) :error (getf result :error) :tokens (getf goal :tokens-used) :charged (getf goal :reservation-charged-tokens) :ledger (getf goal :unknown-usage-by-child)))
                 (test-assert (eq (if failed ':failed ':success) (getf result :status))
                              "the original native child outcome is retained")
                 (test-assert (eq ':active (getf goal :status))
                              "a child outcome does not retire the shared lifecycle")
                 (test-assert (zerop (getf goal :unknown-usage))
                              "only missing final usage creates logical uncertainty")
                 (test-assert (= (if (member mode '(:retry-success :nested)) 3 1)
                                 (mission-child-unknown-test-attempts provider))
                              "the native retry budget counts actual attempts")
                 (test-assert (and (zerop (getf goal :tokens-reserved))
                                   (zerop (getf goal :requests-outstanding)))
                              "native child completion releases all its reservations")
                 (if (eq mode ':nested)
                     (let* ((ledger (getf goal :unknown-usage-by-child))
                            (nested (find (first (first ledger))
                                          (task-orchestrator-list-visible-jobs
                                           orchestrator (application-agent application))
                                          :key #'job-identifier :test #'equal)))
                       (test-assert (and (= 1 (length ledger)) nested (not (eq nested job))
                                         (= 1 (getf (getf (task-job-snapshot nested) :result) :unknown-usage 0))
                                         (zerop (getf result :unknown-usage 0)))
                                    (format nil "nested ledger ~S; outer ~S; owners ~S; visible ~S"
                                            ledger (job-identifier job)
                                            (mission-child-unknown-test-owners provider)
                                            (mapcar #'job-identifier
                                                    (task-orchestrator-list-jobs orchestrator)))))
                     (progn
                       (test-assert (= (if unknown 1 0) (getf result :unknown-usage 0))
                                    "each actual terminal result retains its own uncertainty")
                       (test-assert (= (if unknown (getf goal :reservation-charged-tokens) 5) (getf goal :tokens-used))
                                    "known failure and retry success charge only final reported usage")))))
          (when job (task-job-await job nil))
          (task-orchestrator-close orchestrator)))))
  nil)

(-> test-mission-child-bounded-accounting-controls () null)
(defun test-mission-child-bounded-accounting-controls ()
  "Keep legacy uncertainty, hard limits and reviewer allowances distinct from child audit."
  (dolist (mode '(:unproven :existing-unknown :exact-limit :review :terminal :deadline :multiple))
    (with-test-configuration (configuration)
      (let* ((application  (mission-test--application configuration))
             (orchestrator (task-orchestrator-create))
             (child        (task-tests--make-job orchestrator :identifier "bounded-controls"))
             (allowance    (when (eq mode ':review)
                             (list :turn-limit 2 :token-limit 10 :turns-used 0 :tokens-used 0)))
             (goal nil) (context nil) (calls 0))
        (application-mission-start application
                                   (mission-test--specification :tokens (if (eq mode ':exact-limit) 10 1000)))
        (setf goal (application-goal application)
              context (mission-context-find (application-conversation application))
              (slot-value child 'mission-context) context)
        (unwind-protect
             (let ((*mission-admitted-operation*     child)
                   (*provider-maximum-output-tokens* 10))
               (cond
                 ((eq mode ':unproven)
                  ;; A defensive legacy fixture, not native unlimited admission.
                  (setf (getf goal :requests-outstanding) 1)
                  (uiop:symbol-call
                   :autolith :mission--settle-inference context 0 nil :originating-child child))
                 (t
                  (dotimes (index (if (eq mode ':multiple) 2 1))
                    (declare (ignorable index))
                    (handler-case
                        (mission--account-inference
                         context
                         (lambda ()
                           (case mode
                             (:existing-unknown
                              (incf (getf goal :unknown-usage)))
                             (:terminal
                              (mission--transition context ':cancelled "Test cancellation" :cancel-p nil))
                             (:deadline
                              (setf (getf goal :deadline) (1- (get-universal-time)))))
                           (json-object))
                         #'first :allowance allowance)
                      (mission-error (condition)
                        (test-assert (and (eq mode ':review)
                                          (eq ':review-budget (mission-error-reason condition)))
                                     "review UNKNOWN retains reviewer-local failure after accounting"))))))
               (test-assert (and (eq goal (application-goal application))
                                 (zerop (getf goal :tokens-reserved))
                                 (zerop (getf goal :requests-outstanding)))
                            "all settlement controls preserve goal identity and release bookkeeping")
               (test-assert (= (case mode (:unproven
                                           0)
                                          (:multiple
                                           20)
                                          (otherwise
                                           10))
                               (getf goal :tokens-used))
                            "full reservation charges are once per logical operation")
               (test-assert (= (if (member mode '(:unproven :existing-unknown)) 1 0)
                               (getf goal :unknown-usage))
                            "existing and unproven UNKNOWN are not erased by bounded settlement")
               (test-assert (= (if (eq mode ':multiple) 2 1) (mission-task-job-unknown-usage child))
                            "multiple requests accumulate only on their actual originating child")
               (when allowance
                 (test-assert (and (= 10 (getf allowance :tokens-used))
                                   (= 10 (getf allowance :reservation-charged-tokens))
                                   (zerop (getf allowance :tokens-reserved))
                                   (zerop (getf allowance :requests-outstanding))
                                   (= 10 (getf goal :tokens-used)))
                              "review policy and shared allowance each charge the same R without duplicate global spend"))
               (when (eq mode ':terminal)
                 (test-assert (eq ':cancelled (getf goal :status))
                              "settlement never reactivates an explicit terminal state"))
               (let ((expected (case mode
                                 ((:unproven :existing-unknown)
                                  ':mission-usage-quarantine)
                                 (:exact-limit
                                  ':exhausted)
                                 (:review
                                  ':review-budget)
                                 (:terminal
                                  ':cancelled)
                                 (:deadline
                                  ':exhausted))))
                 (when expected
                   (test-assert
                    (handler-case
                        (progn
                          (mission--account-inference
                           context (lambda () (incf calls) (json-object)) #'first :allowance allowance)
                          nil)
                      (mission-error (condition)
                        (eq expected (mission-error-reason condition))))
                    (format nil "~A keeps its ordinary pre-dispatch refusal ~A" mode expected))
                   (test-assert (zerop calls) "refusal performs no provider dispatch")))
               (when (eq mode ':unproven)
                 (application--record-goal application)
                 (application--load-goal application)
                 (test-assert (and (= 1 (getf (application-goal application) :unknown-usage))
                                   (zerop (getf (application-goal application) :reservation-charged-tokens 0)))
                              "legacy unbounded uncertainty survives durable restore without fabricated charges")))
          (task-orchestrator-close orchestrator)))))
  (with-test-configuration (configuration)
    (let* ((application  (mission-test--application configuration))
           (orchestrator (task-orchestrator-create))
           (children     (loop for id in '("bounded-left" "bounded-right")
                               collect (task-tests--make-job orchestrator :identifier id)))
           (entered      (bt:make-semaphore :count 0))
           (release      (bt:make-semaphore :count 0))
           (cells        nil))
      (application-mission-start application (mission-test--specification))
      (let ((context (mission-context-find (application-conversation application))))
        (unwind-protect
             (progn
               (dolist (child children)
                 (setf (slot-value child 'mission-context) context)
                 (push (mission-inference-test--start
                        (let ((owner child))
                          (lambda ()
                            (let ((*mission-admitted-operation*     owner)
                                  (*provider-maximum-output-tokens* 10))
                              (mission--account-inference
                               context
                               (lambda ()
                                 (bt:signal-semaphore entered)
                                 (unless (bt:wait-on-semaphore release :timeout 5)
                                   (error "Bounded sibling fixture timed out."))
                                 (json-object))
                               #'first)))))
                       cells))
               (dotimes (index 2)
                 (declare (ignorable index))
                 (test-assert (bt:wait-on-semaphore entered :timeout 5)
                              "both exact children admit before either settles"))
               (mission-inference-test--join cells release 2)
               (let ((goal (application-goal application)))
                 (test-assert (and (every (lambda (cell) (null (mission-inference-test-thread-condition cell))) cells)
                                   (= 20 (getf goal :tokens-used))
                                   (= 20 (getf goal :reservation-charged-tokens))
                                   (zerop (getf goal :unknown-usage))
                                   (zerop (getf goal :requests-outstanding))
                                   (zerop (getf goal :tokens-reserved)))
                              "concurrent finite UNKNOWN charges serialize without cancelling the admitted sibling")
                 (test-assert (every (lambda (child)
                                      (equal (mission-task-job-accounting-summary child)
                                             '(:unknown-usage 1 :reservation-charged-tokens 10
                                               :reservation-charge-requests 1))) children)
                              "each concurrent child retains exactly its own audit and charge")))
          (dotimes (index 2) (declare (ignorable index)) (bt:signal-semaphore release))
          (dolist (cell cells)
            (sb-thread:join-thread (mission-inference-test-thread-thread cell) :timeout 10 :default nil))
          (task-orchestrator-close orchestrator)))))
  nil)
