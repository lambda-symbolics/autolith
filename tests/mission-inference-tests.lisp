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
            :reader mission-inference-test-provider-failure
            :type (or null function)
            :documentation "Optional function called to raise a provider failure.")
   (blocked-p :initarg :blocked-p :initform t
              :reader mission-inference-test-provider-blocked-p
              :type boolean :documentation "Whether calls wait for RELEASE.")
   (seen-output-cap :initform nil :accessor mission-inference-test-provider-seen-output-cap
                    :documentation "Output caps observed at the provider boundary.")
   (observation-lock :initform (bordeaux-threads:make-lock "Inference provider observations")
                     :reader mission-inference-test-provider-observation-lock
                     :documentation "Protects the shared observation list."))
  (:documentation "A provider whose requests can be held at the protocol boundary."))

(defmethod provider-stream-turn
    ((provider mission-inference-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Synchronize a test provider request at the real streaming boundary."
  (declare (ignore conversation tool-namespaces event-callback goal-context compaction-p))
  (bordeaux-threads:with-lock-held
      ((mission-inference-test-provider-observation-lock provider))
    (push *provider-maximum-output-tokens*
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
              :type (or null condition)
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
                        :event-callback #'identity))

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
    (declare (ignore i))
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
    (let* ((entered (bordeaux-threads:make-semaphore :count 0))
           (release (bordeaux-threads:make-semaphore :count 0))
           (provider (make-instance 'mission-inference-test-provider
                                    :entered entered :release release
                                    :result (mission-test--result "done")))
           (application (mission-inference-test--application configuration provider))
           (context nil)
           (parent (application-conversation application))
           (first-child (conversation-create configuration))
           (second-child (conversation-create configuration))
           (cells nil))
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
    (let* ((entered (bordeaux-threads:make-semaphore :count 0))
           (release (bordeaux-threads:make-semaphore :count 0))
           (provider (make-instance 'mission-inference-test-provider
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
    (let* ((entered (bordeaux-threads:make-semaphore :count 0))
           (release (bordeaux-threads:make-semaphore :count 0))
           (provider (make-instance 'mission-inference-test-provider
                                    :entered entered :release release
                                    :result (mission-test--result "done")))
           (application (mission-inference-test--application configuration provider))
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
    (let* ((entered (bordeaux-threads:make-semaphore :count 0))
           (release (bordeaux-threads:make-semaphore :count 0))
           (provider (make-instance 'mission-inference-test-provider
                                    :entered entered :release release
                                    :result (mission-test--result "review")))
           (application (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (reviewer (conversation-create configuration))
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
    (let* ((provider (make-instance 'mission-inference-test-provider
                                    :entered (bordeaux-threads:make-semaphore :count 0)
                                    :release (bordeaux-threads:make-semaphore :count 1)
                                    :blocked-p nil :result (mission-test--result "unknown" :usage (json-object))))
           (application (mission-inference-test--application configuration provider))
           (conversation (application-conversation application))
           (context nil))
      (application-mission-start application (mission-test--specification))
      (setf context (mission-context-find conversation))
      (mission-inference-test--call provider conversation)
      (let ((goal (mission-context-goal context)))
        (test-assert (eq ':blocked (getf goal :status)) "unknown usage blocks the mission")
        (test-assert (zerop (getf goal :tokens-reserved)) "unknown usage releases reservation")
        (test-assert (zerop (getf goal :requests-outstanding)) "unknown usage releases request")))
    (let* ((entered (bordeaux-threads:make-semaphore :count 0))
           (release (bordeaux-threads:make-semaphore :count 0))
           (provider (make-instance 'mission-inference-test-provider
                                    :entered entered :release release
                                    :result (mission-test--result "cancelled")))
           (application (mission-inference-test--application configuration provider))
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
      (test-assert (eq ':failed (getf (mission-context-goal context) :status))
                   "a caught nonlocal provider exit fails the mission")
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
    (let* ((provider (make-instance 'mission-inference-test-provider
                                    :entered (bordeaux-threads:make-semaphore :count 0)
                                    :release (bordeaux-threads:make-semaphore :count 1)
                                    :blocked-p nil :result (mission-test--result "capped")))
           (application (mission-inference-test--application configuration provider))
           (context nil))
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
      (let* ((goal (application-goal application))
             (policy (first (getf goal :reviews))))
        (setf (getf goal :turns-used) 2
              (getf goal :requests-outstanding) 2
              (getf policy :turns-used) 1
              (getf policy :tokens-reserved) 25
              (getf policy :requests-outstanding) 1)
        (remf goal :tokens-reserved)
        (application--record-goal application))
      (application--load-goal application)
      (let* ((goal (application-goal application))
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
