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
           (first-child  (conversation-create configuration :parent-identifier (conversation-identifier parent)))
           (second-child (conversation-create configuration :parent-identifier (conversation-identifier parent)))
           (cells        nil))
      (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
      (setf context (mission-context-find parent))
      (mission-context-bind-child parent first-child)
      (mission-context-bind-child parent second-child)
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

(-> test-mission-inference-unknown-drains-child () null)
(defun test-mission-inference-unknown-drains-child ()
  "Test that admitted child inference settles after unknown root usage blocks new work."
  (with-test-configuration (configuration)
    (let* ((entered     (bt:make-semaphore :count 0))
           (release     (bt:make-semaphore :count 0))
           (provider    (make-instance 'mission-inference-test-provider
                                       :entered entered :release release
                                       :result (mission-test--result "done")))
           (application (mission-inference-test--application configuration provider))
           (parent      (application-conversation application))
           (child       (conversation-create configuration :parent-identifier (conversation-identifier parent)))
           (cell        nil))
      (application-mission-start application (mission-test--specification))
      (mission-context-bind-child parent child)
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call provider child))))
             (test-assert (bt:wait-on-semaphore entered :timeout 5) "child inference entered")
             (handler-case
                 (mission--account-inference
                  (mission-context-find parent) (lambda () (error "provider disconnected")) #'first)
               (error () nil))
             (test-assert (eq ':blocked (getf (application-goal application) :status))
                          "unknown root usage blocks the mission")
             (let ((next (conversation-create configuration :parent-identifier (conversation-identifier child))))
               (test-assert (handler-case (progn (mission-context-bind-child child next) nil)
                              (mission-error (condition) (eq ':blocked (mission-error-reason condition))))
                            "blocked child cannot admit new work"))
             (mission-inference-test--join (list cell) release 1)
             (test-assert (null (mission-inference-test-thread-condition cell))
                          "the admitted child request completes")
             (let ((goal (application-goal application)))
               (test-assert (and (= 1 (getf goal :unknown-usage)) (= 5 (getf goal :tokens-used))
                                 (zerop (getf goal :requests-outstanding))
                                 (zerop (getf goal :tokens-reserved)))
                            "the child settles known usage and the root uncertainty is retained")))
        (bt:signal-semaphore release)
        (when cell (sb-thread:join-thread (mission-inference-test-thread-thread cell) :timeout 10 :default nil)))))
  nil)

(-> test-mission-inference-cancelling-block-refuses-continuation () null)
(defun test-mission-inference-cancelling-block-refuses-continuation ()
  "Test that an explicit block refuses child inference and new child binding."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (parent      (application-conversation application))
           (child       (conversation-create configuration :parent-identifier (conversation-identifier parent))))
      (application-mission-start application (mission-test--specification))
      (mission-context-bind-child parent child)
      (application-mission-stop application ':blocked "Explicit blocker.")
      (test-assert (handler-case
                       (progn (mission--admit (mission-context-find child)) nil)
                     (mission-error (condition) (eq ':blocked (mission-error-reason condition))))
                   "child inference is refused")
      (let ((next (conversation-create configuration :parent-identifier (conversation-identifier child))))
        (test-assert (handler-case (progn (mission-context-bind-child child next) nil)
                       (mission-error (condition) (eq ':blocked (mission-error-reason condition))))
                     "new child binding is refused"))))
  nil)

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

(-> test-mission-child-unknown-settlement-controls () null)
(defun test-mission-child-unknown-settlement-controls ()
  "Test child attribution, interruption and subsequent mission spending."
  (dolist (mode '(:numeric :unknown :error :root))
    (with-test-configuration (configuration)
      (let* ((application (mission-test--application configuration))
             (parent      (application-conversation application))
             (child       (conversation-create configuration :parent-identifier (conversation-identifier parent)))
             (reservation nil))
        (application-mission-start application (mission-test--specification))
        (mission-context-bind-child parent child)
        (let* ((context    (mission-context-find parent))
               (goal       (application-goal application))
               (identifier (and (not (eq mode ':root))
                                (mission-context-child-p context child)
                                (conversation-identifier child))))
          (handler-case
              (mission--account-inference
               context
               (lambda ()
                 (setf reservation *provider-mission-output-allowance*)
                 (case mode
                   (:numeric
                    (json-object "input_tokens" 2 "output_tokens" 3))
                   (:error
                    (error "Synthetic provider failure"))
                   (otherwise
                    (json-object))))
               #'first :originating-child identifier)
            (error (condition)
              (unless (eq mode ':error) (error condition))))
          (test-assert (and (= 1 (getf goal :turns-used))
                            (zerop (getf goal :tokens-reserved))
                            (zerop (getf goal :requests-outstanding)))
                       "every request settles its reservation once")
          (case mode
            (:numeric
             (test-assert (and (= 5 (getf goal :tokens-used))
                               (zerop (getf goal :unknown-usage)))
                          "measured child usage is charged"))
            (:root
             (test-assert (and (eq ':blocked (getf goal :status))
                               (= 1 (getf goal :unknown-usage)))
                          "unattributed usage quarantines the mission"))
            (otherwise
             (test-assert (and (integerp reservation) (= reservation (getf goal :tokens-used))
                               (= 1 (rest (assoc identifier (getf goal :unknown-usage-by-child)
                                                 :test #'equal)))
                               (zerop (getf goal :unknown-usage)))
                          "bounded unknown usage is charged to the exact child")
             (mission--account-inference context (lambda () (json-object "input_tokens" 2 "output_tokens" 3)) #'first)
             (test-assert (= (+ reservation 5) (getf goal :tokens-used))
                          "known work continues after bounded child usage")))))))
  nil)

(-> test-mission-child-unknown-reservation-drain-and-recovery () null)
(defun test-mission-child-unknown-reservation-drain-and-recovery ()
  "Drain a reserved request and restore conservatively without losing attribution."
  (with-test-configuration (configuration)
    (let* ((entered     (bt:make-semaphore :count 0))
           (release     (bt:make-semaphore :count 0))
           (provider    (make-instance 'mission-inference-test-provider
                                       :entered entered :release release :result (mission-test--result "done")))
           (application (mission-inference-test--application configuration provider))
           (parent      (application-conversation application))
           (child       (conversation-create configuration :parent-identifier (conversation-identifier parent)))
           (context     nil)
           (cell        nil))
      (application-mission-start application (mission-test--specification))
      (mission-context-bind-child parent child)
      (setf context (mission-context-find parent))
      (unwind-protect
           (progn
             (setf cell (mission-inference-test--start
                         (lambda () (mission-inference-test--call
                                     provider (application-conversation application)))))
             (test-assert (bt:wait-on-semaphore entered :timeout 5) "the other real request owns a reservation")
             (mission--account-inference context (lambda () (json-object)) #'first
                                         :originating-child (and (mission-context-child-p context child)
                                                                 (conversation-identifier child)))
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
                                   (= 1 (rest (assoc (conversation-identifier child)
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
        (when cell (sb-thread:join-thread (mission-inference-test-thread-thread cell) :timeout 10 :default nil)))))
  nil)

(-> test-mission-child-unknown-native-outcome-controls () null)
(defun test-mission-child-unknown-native-outcome-controls ()
  "Test usage attribution through ordinary child and nested conversation calls."
  (dolist (unknown-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((provider    (make-instance 'mission-inference-test-provider
                                         :entered   (bt:make-semaphore :count 0)
                                         :release   (bt:make-semaphore :count 0)
                                         :blocked-p nil
                                         :result    (if unknown-p
                                                        (mission-test--result "done" :usage (json-object))
                                                        (mission-test--result "done"))))
             (application (mission-inference-test--application configuration provider))
             (parent      (application-conversation application))
             (child       (conversation-create configuration :parent-identifier (conversation-identifier parent)))
             (nested      (conversation-create configuration :parent-identifier (conversation-identifier child))))
        (application-mission-start application (mission-test--specification))
        (mission-context-bind-child parent child)
        (mission-context-bind-child child nested)
        (mission-inference-test--call provider nested)
        (let ((goal (application-goal application)))
          (test-assert (and (zerop (getf goal :requests-outstanding))
                            (zerop (getf goal :tokens-reserved)))
                       "the nested request settles its reservation")
          (if unknown-p
              (test-assert (and (= 1 (rest (assoc (conversation-identifier nested)
                                                 (getf goal :unknown-usage-by-child) :test #'equal)))
                                (null (assoc (conversation-identifier child)
                                             (getf goal :unknown-usage-by-child) :test #'equal))
                                (plusp (getf goal :reservation-charged-tokens)))
                           "unknown usage belongs to the nested child")
              (test-assert (and (= 5 (getf goal :tokens-used))
                                (null (getf goal :unknown-usage-by-child)))
                           "measured nested usage is charged once"))))))
  nil)

(-> test-mission-child-bounded-accounting-controls () null)
(defun test-mission-child-bounded-accounting-controls ()
  "Test bounded child usage against mission and reviewer limits."
  (dolist (review-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((application                      (mission-test--application configuration))
             (parent                           (application-conversation application))
             (child                            (conversation-create configuration :parent-identifier (conversation-identifier parent)))
             (allowance                        (when review-p
                                                 (list :turn-limit 2 :token-limit 10 :turns-used 0 :tokens-used 0)))
             (*provider-maximum-output-tokens* 10))
        (application-mission-start application (mission-test--specification :tokens 100))
        (mission-context-bind-child parent child)
        (let* ((context    (mission-context-find parent))
               (goal       (application-goal application))
               (identifier (and (mission-context-child-p context child)
                                (conversation-identifier child))))
          (handler-case
              (mission--account-inference context (lambda () (json-object)) #'first
                                          :allowance allowance :originating-child identifier)
            (mission-error (condition)
              (unless (and review-p (eq ':review-budget (mission-error-reason condition)))
                (error condition))))
          (test-assert (and (= 10 (getf goal :tokens-used))
                            (= 10 (getf goal :reservation-charged-tokens))
                            (zerop (getf goal :unknown-usage))
                            (zerop (getf goal :requests-outstanding))
                            (zerop (getf goal :tokens-reserved)))
                       "the mission charges the admitted child allocation")
          (when review-p
            (test-assert (and (= 10 (getf allowance :tokens-used))
                              (zerop (getf allowance :requests-outstanding))
                              (zerop (getf allowance :tokens-reserved)))
                         "the reviewer allowance settles the same request"))))))
  nil)


;;;; -- Explicit Unknown Usage Policy --

(-> mission-inference-test--policy-specification (t &key (:limit integer)) list)
(defun mission-inference-test--policy-specification (policy &key (limit 100))
  "Build a finite policy fixture, omitting the new key for legacy behavior."
  (append (mission-test--specification :turns limit :tokens limit)
          (unless (eq policy ':missing) (list :unknown-usage-policy policy))))

(-> mission-inference-test--policy-provider (&key (:failure (or null function)) (:unknown-p boolean)) model-provider)
(defun mission-inference-test--policy-provider (&key failure unknown-p)
  "Return a nonblocking provider for actual mission admission and settlement."
  (make-instance 'mission-inference-test-provider
                 :entered (bordeaux-threads:make-semaphore :count 0)
                 :release (bordeaux-threads:make-semaphore :count 0)
                 :blocked-p nil :failure failure
                 :result  (if unknown-p
                              (mission-test--result "unknown" :usage (json-object))
                              (mission-test--result "known"))))

(-> test-mission-unknown-usage-policy-validation () null)
(defun test-mission-unknown-usage-policy-validation ()
  "Only explicit supported policies are accepted; omission selects blocking."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (dolist (policy '(:missing :block :record))
        (application-mission-start application (mission-inference-test--policy-specification policy))
        (test-assert (eq (if (eq policy ':missing) ':block policy)
                         (getf (application-goal application) :unknown-usage-policy))
                     "the selected policy is present in the durable goal"))
      (dolist (policy '(nil :ignore "record" (:record)))
        (let ((before (application-goal application)))
          (test-assert (handler-case
                           (progn
                             (application-mission-start application
                                                        (mission-inference-test--policy-specification policy))
                             nil)
                         (mission-error (condition) (eq ':invalid (mission-error-reason condition))))
                       "invalid policy is rejected")
          (test-assert (eq before (application-goal application))
                       "invalid input does not replace the current mission")))))
  nil)

(-> test-mission-inference-unknown-usage-policy-settlement () null)
(defun test-mission-inference-unknown-usage-policy-settlement ()
  "Count missing usage through successful, failed and interrupted provider calls."
  (dolist (policy '(:missing :block :record))
    (dolist (mode '(:result :error :nonlocal))
      (dolist (limit (list 100 most-positive-fixnum))
        (with-test-configuration (configuration)
          (let* ((provider     (mission-inference-test--policy-provider
                                :unknown-p t
                                :failure   (case mode
                                             (:error
                                              (lambda () (error "Scripted transport failure.")))
                                             (:nonlocal
                                              (lambda () (throw 'policy-interruption ':interrupted))))))
                 (application  (mission-inference-test--application configuration provider))
                 (conversation (application-conversation application))
                 (caught       nil))
            (application-mission-start application
                                       (mission-inference-test--policy-specification policy :limit limit))
            (catch 'policy-interruption
              (handler-case (mission-inference-test--call provider conversation)
                (mission-error () (setf caught t))))
            (test-assert (eq caught (eq mode ':error)) "provider errors still propagate")
            (let ((goal (application-goal application)))
              (test-assert (eq (if (eq policy ':record) ':active ':blocked) (getf goal :status))
                           "only explicit recording avoids the unknown-usage block")
              (test-assert (= 1 (getf goal :unknown-usage)) "every unknown request is counted")
              (test-assert (zerop (getf goal :tokens-used)) "unknown tokens are not guessed")
              (test-assert (and (= 1 (getf goal :turns-used))
                                (zerop (getf goal :tokens-reserved))
                                (zerop (getf goal :requests-outstanding)))
                           "the admitted turn is charged and its reservation released")
              (when (and (eq policy ':record) (eq (getf goal :status) ':active))
                (test-assert (find ':unknown-usage (getf goal :evidence) :key (lambda (entry) (getf entry :kind)))
                             "recording retains bounded evidence")
                (let ((child (conversation-create configuration)))
                  (mission-context-inherit conversation child)
                  (test-assert (eq (mission-context-find child) (mission-context-find conversation))
                               "children share the same policy and accounting")
                  (mission-inference-test--call (mission-inference-test--policy-provider) child))
                (test-assert (and (= 2 (getf goal :turns-used)) (= 5 (getf goal :tokens-used))
                                  (= 1 (getf goal :unknown-usage)))
                             "the next inherited request is admitted and known usage charged")
                (mission--transition (mission-context-find conversation) ':paused "User paused the fixture.")
                (application-mission-resume application)
                (test-assert (eq ':active (getf goal :status)) "explicit resume retains recorded unknown usage")
                (test-assert (= 1 (getf goal :unknown-usage)) "resume never clears unknown usage")
                (mission--transition (mission-context-find conversation) ':blocked "An unrelated blocker.")
                (test-assert (eq ':mission-stopped
                                 (tool-result-error-code
                                  (mission--stopped-tool-result (mission-context-find conversation))))
                             "an unrelated blocker is not reported as unknown-usage quarantine"))))))))
  nil)

(-> test-mission-inference-unknown-usage-policy-limits () null)
(defun test-mission-inference-unknown-usage-policy-limits ()
  "Recording unknowns does not waive finite turns, reported tokens or deadlines."
  (dolist (budget '(:turn :token :deadline))
    (with-test-configuration (configuration)
      (let* ((provider      (mission-inference-test--policy-provider :unknown-p t))
             (application   (mission-inference-test--application configuration provider))
             (conversation  (application-conversation application))
             (specification (mission-inference-test--policy-specification ':record)))
        (setf (getf specification :turn-limit) (if (eq budget ':turn) 1 10)
              (getf specification :token-limit) (if (eq budget ':token) 5 100))
        (application-mission-start application specification)
        (mission-inference-test--call provider conversation)
        (let ((goal (application-goal application)))
          (test-assert (eq ':active (getf goal :status)) "recorded unknown usage alone is not exhaustion")
          (when (eq (getf goal :status) ':active)
            (when (eq budget ':token)
              (mission-inference-test--call (mission-inference-test--policy-provider) conversation))
            (when (eq budget ':deadline)
              (setf (getf goal :deadline) (1- (get-universal-time))))
            (let ((before (copy-tree goal)))
              (test-assert (handler-case
                               (progn (mission-inference-test--call provider conversation) nil)
                             (mission-error (condition) (eq ':exhausted (mission-error-reason condition))))
                           "the exhausted budget refuses the next request")
              (test-assert (= (getf before :turns-used) (getf goal :turns-used))
                           "refusal does not dispatch or charge another turn")
              (test-assert (= 1 (getf goal :unknown-usage)) "refusal does not invent unknown usage")))))))
  nil)

(-> test-mission-inference-unknown-usage-policy-restore () null)
(defun test-mission-inference-unknown-usage-policy-restore ()
  "Reload an actual persisted in-flight admission, then reload again without double charge."
  (dolist (policy '(:missing :block :record))
    (dolist (status '(:active :paused :blocked :failed))
      (with-test-configuration (configuration)
        (let* ((application  (mission-test--application configuration))
               (conversation (application-conversation application))
               (restored     nil)
               (provider
                 (mission-inference-test--policy-provider
                  :failure
                  (lambda ()
                    ;; Admission has already persisted one outstanding request.
                    (unless (eq status ':active)
                      (mission--transition (mission-context-find conversation) status "Unrelated fixture stop."
                                           :cancel-p nil))
                    (setf restored (mission-test--application configuration))
                    (setf (application-conversation restored)
                          (conversation-load-by-id configuration (conversation-identifier conversation)))
                    (application--load-goal restored)
                    ;; Model process death: do not let the superseded request write again.
                    (setf (application-goal application) nil)
                    (throw 'policy-restart ':restarted)))))
          (application-mission-start application (mission-inference-test--policy-specification policy))
          (catch 'policy-restart (mission-inference-test--call provider conversation))
          (let* ((goal     (application-goal restored))
                 (expected (if (and (not (eq policy ':record))
                                    (member status '(:active :paused :blocked)))
                               ':blocked status)))
            (test-assert (eq expected (getf goal :status)) "startup recovery follows policy without reactivating stopped work")
            (test-assert (= 1 (getf goal :unknown-usage)) "the durable outstanding request becomes one unknown")
            (test-assert (and (= 1 (getf goal :turns-used))
                              (zerop (getf goal :tokens-reserved))
                              (zerop (getf goal :requests-outstanding)))
                         "startup preserves the admitted turn and clears its reservation")
            (when (and (eq policy ':record) (eq (getf goal :status) ':active))
              (mission-inference-test--call (mission-inference-test--policy-provider)
                                            (application-conversation restored))
              (test-assert (and (= 2 (getf goal :turns-used)) (= 5 (getf goal :tokens-used)))
                           "a subsequent request uses the restored budget"))
            (setf (application-conversation restored)
                  (conversation-load-by-id configuration (conversation-identifier conversation)))
            (application--load-goal restored)
            (test-assert (= 1 (getf (application-goal restored) :unknown-usage))
                         "a second disk reload does not charge the interruption twice")
            (test-assert (eq expected (getf (application-goal restored) :status))
                         "recovery status is persisted"))))))
  nil)

(-> test-mission-inference-unknown-usage-policy-fail-closed () null)
(defun test-mission-inference-unknown-usage-policy-fail-closed ()
  "Legacy and malformed persisted policies cannot opt in to unbounded unknown spend."
  (dolist (policy '(:missing nil :ignore "record"))
    (dolist (restore-p '(nil t))
      (with-test-configuration (configuration)
        (let* ((application  (mission-test--application configuration))
               (conversation (application-conversation application)))
          (application-mission-start application (mission-test--specification))
          ;; Simulate an old or malformed goal record, never alter usage counters.
          (let ((goal (application-goal application)))
            (if (eq policy ':missing)
                (remf goal :unknown-usage-policy)
                (progn
                  (unless (member :unknown-usage-policy goal)
                    (nconc goal (list :unknown-usage-policy ':block)))
                  (setf (getf goal :unknown-usage-policy) policy))))
          (application--record-goal application)
          (let ((provider (mission-inference-test--policy-provider
                           :unknown-p t
                           :failure   (when restore-p
                                        (lambda ()
                                          (application--load-goal application)
                                          (throw 'legacy-restart ':restarted))))))
            (catch 'legacy-restart (mission-inference-test--call provider conversation)))
          (test-assert (eq ':blocked (getf (application-goal application) :status))
                       "only the exact record keyword can bypass unknown-usage quarantine")
          (test-assert (= 1 (getf (application-goal application) :unknown-usage))
                       "fail-closed recovery retains the unknown count")
          (test-assert (handler-case (progn (application-mission-resume application) nil)
                         (mission-error (condition) (eq ':unknown-usage (mission-error-reason condition))))
                       "legacy and malformed policy cannot resume unknown spend")))))
  nil)

(-> test-mission-inference-unknown-usage-policy-review () null)
(defun test-mission-inference-unknown-usage-policy-review ()
  "Opt-in missions still refuse independent reviews with missing usage."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (allowance   (list :turn-limit 10 :token-limit 100 :turns-used 0 :tokens-used 0
                              :tokens-reserved 0 :requests-outstanding 0)))
      (application-mission-start application (mission-inference-test--policy-specification ':record))
      (test-assert (handler-case
                       (progn
                         (mission--account-inference
                          (mission-context-find (application-conversation application))
                          (lambda () (mission-test--result "unknown" :usage (json-object)))
                          (lambda (results) (provider-result-usage (first results)))
                          :allowance allowance)
                         nil)
                     (mission-error (condition) (eq ':review-budget (mission-error-reason condition))))
                   "review usage remains mandatory")
      (test-assert (eq ':active (getf (application-goal application) :status))
                   "review refusal does not replace the mission unknown-usage policy")
      (test-assert (and (= 1 (getf allowance :turns-used))
                        (zerop (getf allowance :tokens-reserved))
                        (zerop (getf allowance :requests-outstanding))
                        (= 1 (getf (application-goal application) :unknown-usage)))
                   "both allowances release reservations while retaining uncertainty")))
  nil)
