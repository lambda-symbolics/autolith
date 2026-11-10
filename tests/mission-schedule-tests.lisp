(in-package #:autolith)

;;;; -- Mission Schedule Product Boundaries --

(-> mission-schedule-tests--fixture (function) null)
(defun mission-schedule-tests--fixture (function)
  "Call FUNCTION with a real mission, durable scheduler and observable ordinary admission."
  (with-test-configuration (configuration root)
    (let* ((application (mission-test--application configuration))
           (path        (merge-pathnames "mission-wakeups.sexp" root))
           (now 100) (commands nil) (executions nil)
           (service     (mission-schedule-service-create
                         application :path path :clock (lambda () now)
                         :enqueue (lambda (owner command)
                                    (test-assert (eq owner application) "admission retains the owning primary")
                                    (push command commands) t)
                         :run     (lambda (owner content)
                                    (test-assert (eq owner application) "execution retains the owning primary")
                                    (push content executions)))))
      (application-mission-start application (mission-test--specification))
      (setf (gethash application *mission-schedule-services*) service)
      (unwind-protect
           (funcall function application service path
                    (lambda (&optional value) (when value (setf now value)) now)
                    (lambda () (copy-list commands)) (lambda () (copy-list executions)))
        (remhash application *mission-schedule-services*)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> mission-schedule-tests--ticket (list) list)
(defun mission-schedule-tests--ticket (wakeup)
  "Copy only the occurrence identity and current claim token into an ordinary queue ticket."
  (list :id (getf wakeup :id) :token (getf wakeup :token)))

(-> test-mission-schedule-missed-and-dedup () null)
(defun test-mission-schedule-missed-and-dedup ()
  "Exercise missed interval policy and persistent duplicate admission through product authority."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore path))
     (dolist (policy '(:latest :skip :all))
       (application-mission-schedule-add application :id (symbol-name policy) :content "Inspect CI"
                                        :at 100 :interval 10 :missed-policy policy))
     (funcall clock 125)
     (let ((wakeups (application-mission-schedules-wake application)))
       (test-assert (= 4 (length wakeups)) "latest coalesces, skip omits, all admits each overdue occurrence")
       (test-assert (every (lambda (wakeup) (eq (getf wakeup :state) ':claimed)) wakeups)
                    "durable claims precede ordinary enqueue")
       (test-assert (= 4 (length (funcall commands))) "one ordinary command per admitted occurrence")
       (test-assert (null (application-mission-schedules-wake application)) "same wake produces no duplicate work")
       (test-assert (= 130 (application-mission-schedules-next-time application)) "host deadline advances without polling")
       (test-assert (= 4 (length (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups)))
                    "occurrence history retains duplicate proof")
       (let* ((scheduler (mission-schedule-scheduler service))
              (before    (cl-jobpond:scheduler-snapshot scheduler))
              (ticket    (mission-schedule-tests--ticket (first wakeups))))
         (dolist (invalid (list nil "(:id (1) :token 1)" '(:id (1) :token "1")
                               '(:id (1) :token 1 :extra t)))
           (test-assert
            (handler-case (progn (application-operation-call application "mission-wakeup" invalid) nil)
              (mission-error () t))
            "native wakeup rejects serialized or malformed tickets"))
         (test-assert (equal before (cl-jobpond:scheduler-snapshot scheduler))
                      "rejected tickets have no scheduler effects")
         (test-call-with-function-replacements
          (list (list 'application-mission-schedule-execute
                      (lambda (owner supplied)
                        (test-assert (eq owner application) "wakeup retains owning application")
                        (test-assert (eq supplied ticket) "native wakeup retains the actual ticket"))))
          (lambda () (application-operation-call application "mission-wakeup" ticket)))
         (application-operation-call application "mission-wakeup" ticket)
         (test-assert (= 1 (length (funcall executions))) "native ticket executes exactly once")
         (test-assert
          (handler-case (progn (application-operation-call application "mission-wakeup" ticket) nil)
            (mission-error () t))
          "a consumed native ticket cannot replay")
         (application-command application
                              (format nil "/mission-wakeup ~S"
                                      (mission-schedule-tests--ticket (second wakeups))))
         (test-assert (= 2 (length (funcall executions))) "slash ticket keeps ordinary execution semantics")
         (dolist (text '("/mission-wakeup #.(error \"evaluated\")"
                          "/mission-wakeup (:id (1) :token 1) nil"))
           (test-assert (handler-case (progn (application-command application text) nil)
                          (mission-error () t))
                        "slash wakeup rejects read-time evaluation and trailing forms")))))))

(-> test-mission-schedule-version-and-cancel () null)
(defun test-mission-schedule-version-and-cancel ()
  "Recheck mission/version and cancellation when queued tickets are consumed."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore service path clock commands))
     (application-mission-schedule-add application :id "first" :content "First mission" :at 100)
     (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
       (application-mission-start application (mission-test--specification))
       (test-assert (eq ':cancelled (getf (application-mission-schedule-execute application ticket) :state))
                    "a queued wakeup cannot admit a replacement mission")
       (test-assert (null (funcall executions)) "stale mission wakeup has no provider effect"))
     (application-mission-schedule-add application :id "version" :content "Versioned mission" :at 100)
     (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
       (setf (getf (application-goal application) :schedule-version) (make-identifier))
       (test-assert (eq ':cancelled (getf (application-mission-schedule-execute application ticket) :state))
                    "same mission with a newer version rejects the stale ticket"))
     (application-mission-schedule-add application :id "cancel" :content "Cancelled mission" :at 100)
     (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
       (application-mission-schedule-cancel application "cancel")
       (test-assert (eq ':cancelled (getf (application-mission-schedule-execute application ticket) :state))
                    "cancellation remains effective after queue admission")
       (test-assert (null (funcall executions)) "cancelled work is never executed")))))

(-> test-mission-schedule-recovery () null)
(defun test-mission-schedule-recovery ()
  "Restore claims as uncertain, require an explicit decision, then execute exactly once."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore service clock commands executions))
     (let* ((scheduler                              (mission-schedule-scheduler (application-mission-schedules application)))
            (before                                 (cl-jobpond:scheduler-snapshot scheduler))
            (*mission-schedule-snapshot-byte-limit* 256))
       (test-assert
        (handler-case
            (progn
              (application-mission-schedule-add application :id "oversized" :event "ci"
                                                :content (make-string 600 :initial-element #\x))
              nil)
          (mission-error (condition)
            (eq ':schedule-storage (mission-error-reason condition))))
        "unrestorable snapshots are refused before durable admission")
       (test-assert (equal before (cl-jobpond:scheduler-snapshot scheduler))
                    "storage refusal preserves the previously published scheduler"))
     (application-mission-schedule-add application :id "recover" :content "Recover safely" :at 100)
     (let* ((original (first (application-mission-schedules-wake application)))
            (effects  0)
            (restored (mission-schedule-service-create application :path path :clock (lambda () 100)
                                                       :enqueue (lambda (owner command) (declare (ignore owner command)) t)
                                                       :run     (lambda (owner content)
                                                                  (declare (ignore owner content))
                                                                  (incf effects)
                                                                  (make-mission-schedule-outcome :state ':completed :reason ':executed)))))
       (setf (gethash application *mission-schedule-services*) restored)
       (test-assert (eq ':unknown (getf (first (getf (cl-jobpond:scheduler-snapshot
                                                      (mission-schedule-scheduler restored)) :wakeups)) :state))
                    "restart never implies repeated admission")
       (test-assert (null (application-mission-schedules-wake application)) "unknown claims are not automatically replayed")
       (test-assert (handler-case
                        (progn (application-mission-schedule-execute application (mission-schedule-tests--ticket original)) nil)
                      (mission-error () t)) "a pre-restart queued ticket is refused")
       (application-mission-schedule-resolve application (getf original :id) :token (getf original :token) :action ':retry)
       (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
         (test-assert (eq ':completed (getf (application-mission-schedule-execute application ticket) :state))
                      "explicit recovery admits current work")
         (test-assert (= 1 effects) "one execution after explicit recovery")
         (test-assert (handler-case (progn (application-mission-schedule-execute application ticket) nil)
                        (mission-error () t)) "a copied queue ticket cannot repeat effects"))))))

(-> test-mission-schedule-events-and-authority () null)
(defun test-mission-schedule-events-and-authority ()
  "Exercise ordinary tool denial and stable external event identities."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore path clock))
     (let* ((registry (application-tool-registry application))
            (context  (mission-test--context application))
            (other    (agent-create :configuration (application-configuration application)
                                    :conversation  (application-conversation application)
                                    :provider (make-instance 'model-provider) :tool-registry registry :worker nil)))
       (mission-schedule-register-tools registry)
       (setf (slot-value context 'agent) other)
       (test-assert (not (tool-result-success-p
                          (tool-registry-execute-call registry
                                                      (agent-test-call :namespace "mission-schedule" :name "add"
                                                                       :arguments "{\"id\":\"denied\",\"content\":\"work\",\"event\":\"ci\"}") context)))
                    "a non-owning agent cannot schedule the primary mission")
       (setf (slot-value context 'agent) (application-agent application))
       (test-assert (tool-result-success-p
                     (tool-registry-execute-call registry
                                                (agent-test-call :namespace "mission-schedule" :name "add"
                                                                 :arguments "{\"id\":\"ci\",\"content\":\"Inspect CI\",\"event\":\"ci\"}") context))
                    "the owning primary uses the normal tool registry")
       (let* ((wakeup (first (application-mission-schedule-event application "ci" "build-17")))
              (ticket (mission-schedule-tests--ticket wakeup))
              (before (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)))
              (caller (mission-test--application (application-configuration application))))
         (test-assert wakeup "observable event admits work")
         (unwind-protect
              (progn
                (setf (application-agent caller) other
                      (application-conversation caller) (application-conversation application)
                      (application-tool-registry caller) registry)
                (test-assert
                 (handler-case
                     (progn (application-operation-call caller "mission-wakeup" ticket) nil)
                   (mission-error (condition)
                     (eq ':schedule-authority (mission-error-reason condition))))
                 "the typed wakeup handler rejects a non-owning agent")
                (test-assert (equal before (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)))
                             "authority rejection does not consume or settle the claimed ticket")
                (test-assert (null (funcall executions)) "authority rejection has no provider effect")
                (let ((evaluation (application-lisp-evaluate (first (funcall commands)) :application application)))
                  (test-assert (eq ':ok (application-lisp-evaluation-status evaluation))
                               "the generated queue form supplies a quoted native ticket, not serialized ticket text"))
                (test-assert (= 1 (length (funcall executions))) "the owning primary consumes its queued native wakeup once"))
           (terminal-ui-stop (application-ui caller))))
       (test-assert (null (application-mission-schedule-event application "ci" "build-17")) "duplicate event has no second admission")
       (test-assert (null (application-mission-schedules-next-time application)) "event-only work has no heartbeat deadline")))))

(-> test-mission-schedule-live-retry-exclusion () null)
(defun test-mission-schedule-live-retry-exclusion ()
  "Exclude retry decisions during a live operation while permitting normal schedule tools."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore service clock commands executions))
     (let ((wakeup nil) (rejected nil) (recovery-rejected nil))
       (let ((replacement
               (mission-schedule-service-create
                application :path path :clock (lambda () 100)
                :enqueue (lambda (owner command) (declare (ignore owner command)) t)
                :run     (lambda (owner content)
                           (declare (ignore content))
                           (setf rejected (handler-case
                                              (progn (application-mission-schedule-resolve owner (getf wakeup :id)
                                                                                           :token (getf wakeup :token) :action ':retry) nil)
                                            (mission-error () t)))
                           (setf recovery-rejected
                                 (handler-case
                                     (progn (application-mission-schedules-recover owner) nil)
                                   (mission-error (condition)
                                     (eq ':schedule-executing (mission-error-reason condition)))))
                           (application-mission-schedule-add owner :id "followup" :content "Followup" :event "next")))))
         (setf (gethash application *mission-schedule-services*) replacement)
         (application-mission-schedule-add application :id "live" :content "Live work" :at 100)
         (setf wakeup (first (application-mission-schedules-wake application)))
         (application-mission-schedule-execute application (mission-schedule-tests--ticket wakeup))
         (test-assert rejected "a live uncertain reservation cannot be retried concurrently")
         (test-assert recovery-rejected "reconstruction cannot discard a live retry reservation")
         (test-assert (eq replacement (application-mission-schedules application))
                      "rejected reconstruction preserves the exact live scheduler")
         (test-assert (= 2 (length (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler replacement)) :schedules)))
                      "normal schedule changes remain usable during provider work"))))))

(-> test-mission-schedule-hosted-admission () null)
(defun test-mission-schedule-hosted-admission ()
  "Exercise the real daemon timer, input controller command and primary provider budget boundary."
  (with-test-configuration (configuration root)
    (let* ((application (mission-test--application configuration
                                                   :results (list (mission-test--result "Hosted wakeup observed."))))
           (controller  (application-input-controller-create application :load-pending-p nil
                                                                          :pending-persistence-enabled-p t :start-reader-p nil))
           (runtime     (image-daemon:daemon-runtime-create
                         :directory root :identifier (conversation-identifier (application-conversation application))
                         :publish-p nil :request-function
                         (lambda (host request &key socket stream)
                           (declare (ignore host request socket stream)) (list :ok)))))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification :turns 1))
             (image-daemon:daemon-runtime-start runtime)
             (application-mission-schedules-host application runtime)
             (let ((previous (application-mission-schedules application)))
               (application-mission-schedules-recover application)
               (test-assert
                (handler-case
                    (with-recursive-lock-held ((mission-schedule-lock previous))
                      (mission-schedule--admit previous)
                      nil)
                  (mission-error (condition)
                    (eq ':schedule-retired (mission-error-reason condition))))
                "operations holding a pre-recovery scheduler cannot admit work"))
             (application-mission-schedule-add application :id "hosted" :content "Perform the scheduled mission step."
                                              :at (1+ (get-universal-time)))
             (let ((deadline (+ (get-internal-real-time) (* 4 internal-time-units-per-second))))
               (loop until (with-lock-held ((application-input-controller-lock controller))
                             (not (deque-empty-p (application-input-controller-work-items controller))))
                     do (when (> (get-internal-real-time) deadline)
                          (error "The hosted scheduler did not enqueue its deadline."))
                        (sleep 0.01)))
             (let ((work (application-input-controller--next-work controller)))
               (test-assert (eq (first work) ':native-lisp)
                            "timer work uses the noninteractive durable Lisp queue")
               (application-input-controller--run-work controller work))
             (test-assert (= 1 (getf (application-goal application) :turns-used))
                          "the scheduled command enters ordinary mission provider admission")
             (test-assert (test-object-contains-string-p
                           (scripted-provider-input-snapshots (application-provider application))
                           "Perform the scheduled mission step.")
                          "the actual primary provider request contains the scheduled continuation")
             (test-assert (eq ':completed (getf (first (getf (cl-jobpond:scheduler-snapshot
                                                              (mission-schedule-scheduler (application-mission-schedules application))) :wakeups)) :state))
                          "ordinary command completion settles the durable occurrence"))
        (image-daemon:daemon-runtime-stop runtime)
        (remhash application *mission-schedule-services*)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-mission-schedule-hosted-interval-awaits-next-occurrence () null)
(defun test-mission-schedule-hosted-interval-awaits-next-occurrence ()
  "Leave a recurring mission active after its occurrence without re-prompting the provider."
  (with-test-configuration (configuration root)
    (let* ((idle        "Idle until the next native hourly occurrence.")
           (application (mission-test--application configuration
                                                   :results (loop repeat 5 collect (mission-test--result idle))))
           (controller  (application-input-controller-create application :load-pending-p nil
                                                                          :pending-persistence-enabled-p t :start-reader-p nil))
           (runtime     (image-daemon:daemon-runtime-create
                         :directory root :identifier (conversation-identifier (application-conversation application))
                         :publish-p nil :request-function
                         (lambda (host request &key socket stream)
                           (declare (ignore host request socket stream)) (list :ok)))))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification :turns 5))
             (image-daemon:daemon-runtime-start runtime)
             (application-mission-schedules-host application runtime)
             (application-mission-schedule-add application :id "hourly" :content "Port the next package."
                                              :at (1+ (get-universal-time)) :interval 3600)
             (let ((deadline (+ (get-internal-real-time) (* 4 internal-time-units-per-second))))
               (loop until (with-lock-held ((application-input-controller-lock controller))
                             (not (deque-empty-p (application-input-controller-work-items controller))))
                     do (when (> (get-internal-real-time) deadline)
                          (error "The hosted scheduler did not enqueue its deadline."))
                        (sleep 0.01)))
             (mission-schedule-test--run-finished-work controller (application-input-controller--next-work controller))
             (let ((goal     (application-goal application))
                   (schedule (first (getf (cl-jobpond:scheduler-snapshot
                                           (mission-schedule-scheduler (application-mission-schedules application)))
                                          :schedules))))
               (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                            "only the occurrence itself reaches the provider")
               (test-assert (= 1 (getf goal :turns-used))
                            "goal continuations do not spend mission turns between occurrences")
               (test-assert (eq ':active (getf goal :status))
                            "the mission stays active for its next occurrence")
               (test-assert (and (not (getf schedule :cancelled)) (getf schedule :next))
                            "the recurring schedule keeps its next deadline")))
        (image-daemon:daemon-runtime-stop runtime)
        (remhash application *mission-schedule-services*)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-mission-schedule-continuation-wakeup-boundaries () null)
(defun test-mission-schedule-continuation-wakeup-boundaries ()
  "Defer goal continuations only to a hosted time wakeup of the current mission version."
  (dolist (row '((:hosted-interval    :hosted-p t   :continues-p nil)
                 (:unhosted-interval  :hosted-p nil :continues-p t)
                 (:hosted-at          :hosted-p t   :continues-p nil)
                 (:admitted-at        :hosted-p t   :continues-p nil)
                 (:executed-at        :hosted-p t   :continues-p t)
                 (:event              :hosted-p t   :continues-p t)
                 (:cancelled-interval :hosted-p t   :continues-p t)
                 (:stale-version      :hosted-p t   :continues-p t)))
    (destructuring-bind (name &key hosted-p continues-p) row
      (mission-schedule-tests--fixture
       (lambda (application service path clock commands executions)
         (declare (ignore path clock commands executions))
         (let ((calls 0))
           (when hosted-p
             (setf (mission-schedule-notify service) (lambda () nil)))
           (ecase name
             ((:hosted-interval :unhosted-interval)
              (application-mission-schedule-add application :id "hourly" :content "Port" :at 200 :interval 3600))
             (:hosted-at
              (application-mission-schedule-add application :id "later" :content "Port" :at 200))
             (:admitted-at
              (application-mission-schedule-add application :id "now" :content "Port" :at 100)
              (application-mission-schedules-wake application))
             (:executed-at
              (application-mission-schedule-add application :id "now" :content "Port" :at 100)
              (application-mission-schedule-execute
               application (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
             (:event
              (application-mission-schedule-add application :id "ci" :content "Inspect CI" :event "ci"))
             (:cancelled-interval
              (application-mission-schedule-add application :id "hourly" :content "Port" :at 200 :interval 3600)
              (application-mission-schedule-cancel application "hourly"))
             (:stale-version
              (application-mission-schedule-add application :id "hourly" :content "Port" :at 200 :interval 3600)
              (setf (getf (application-goal application) :schedule-version) (make-identifier))))
           (test-call-with-function-replacements
            (list (list 'application--run-turn
                        (lambda (candidate &rest arguments)
                          (declare (ignore arguments))
                          (incf calls)
                          (setf (getf (application-goal candidate) :status) ':paused)
                          nil)))
            (lambda () (application--run-goal-continuations application)))
           (test-assert (= (if continues-p 1 0) calls)
                        (format nil "~(~A~) ~:[defers to the scheduled wakeup~;keeps ordinary continuations~]"
                                name continues-p)))))))
  nil)


;;;; -- Noninteractive Scheduled Wakeups --

(-> mission-schedule-test--run-finished-work (application-input-controller list) t)
(defun mission-schedule-test--run-finished-work (controller work)
  "Dispatch work and reset the prompt even when dispatch unwinds."
  (unwind-protect
       (application-input-controller--run-work controller work)
    (application-input-controller--finish-work controller)))

(-> test-mission-schedule-native-wakeup-stale-claim-process () null)
(defun test-mission-schedule-native-wakeup-stale-claim-process ()
  "Contain a recovered hosted wakeup and accept subsequent input in a real worker."
  ;; Finish cold-cache presentation setup before bounding controller dispatch.
  (application-lisp--source-entry "(+ 1 2)")
  (with-test-configuration (configuration root)
    (let* ((application (mission-test--application configuration))
           (controller  (application-input-controller-create
                         application :load-pending-p nil
                         :pending-persistence-enabled-p t :start-reader-p nil))
           (runtime     (image-daemon:daemon-runtime-create
                         :directory  root
                         :identifier (conversation-identifier
                                      (application-conversation application))
                         :publish-p nil :request-function
                         (lambda (host request &key socket stream)
                           (declare (ignore host request socket stream))
                           (list :ok))))
           (count       0))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification))
             (image-daemon:daemon-runtime-start runtime)
             (application-mission-schedules-host application runtime)
             (application-mission-schedule-add
              application :id "stale-wakeup" :content "Must not reach provider."
              :at (+ (get-universal-time) 1))
             (let ((deadline (+ (get-internal-real-time)
                                (* 4 internal-time-units-per-second))))
               (loop until (with-lock-held ((application-input-controller-lock controller))
                             (not (deque-empty-p
                                   (application-input-controller-work-items controller))))
                     do (when (> (get-internal-real-time) deadline)
                          (error "The hosted scheduler did not enqueue its deadline."))
                        (sleep 0.01)))
             ;; Recovery retires the claim while the controller still owns the ticket.
             (application-mission-schedules-recover application)
             (test-call-with-function-replacements
              (list (list 'application-lisp--select-restart
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (incf count)
                            nil)))
              (lambda ()
                (sb-ext:with-timeout 10
                  (mission-schedule-test--run-finished-work
                   controller (application-input-controller--next-work controller)))))
             (test-assert (zerop count) "stale wakeups never select an interactive restart")
             (let ((record (rest (first (last (conversation-user-operation-snapshot
                                               (application-conversation application)))))))
               (test-assert (member (getf record :status) '(:error :aborted))
                            "the stale wakeup records a durable failure")
               (test-assert (search "stale or requires explicit recovery" (getf record :result))
                            "the stale claim retains its recovery condition"))
             (application-input-controller--enqueue controller ':lisp "(+ 1 2)")
             (sb-ext:with-timeout 10
               (mission-schedule-test--run-finished-work
                controller (application-input-controller--next-work controller)))
             (test-assert (eq (getf (rest (first (last (conversation-user-operation-snapshot
                                                        (application-conversation application)))))
                                    :status)
                              ':ok)
                          "ordinary input progresses after the failed wakeup")
             (test-assert (null (scripted-provider-input-snapshots
                                 (application-provider application)))
                          "the stale claim never reaches the provider"))
        (application-input-controller-stop controller)
        (image-daemon:daemon-runtime-stop runtime)
        (remhash application *mission-schedule-services*)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-mission-schedule-lisp-restart-policy () null)
(defun test-mission-schedule-lisp-restart-policy ()
  "Keep the selector for interactive failures only."
  ;; Finish cold-cache presentation setup before bounding controller dispatch.
  (application-lisp--source-entry "(+ 1 2)")
  (dolist (kind '(:lisp :native-lisp))
    (dolist (entry '((:ok "(+ 1 2)")
                     (:stale "(mission-wakeup '(:id (1) :token 1))")
                     (:malformed "(mission-wakeup '(:bogus 1))")))
      (destructuring-bind (outcome source) entry
        (with-test-configuration (configuration root)
          (declare (ignore root))
          (let* ((application (mission-test--application configuration))
                 (controller  (application-input-controller-create
                               application :load-pending-p nil :start-reader-p nil))
                 (count       0))
            (unwind-protect
                 (progn
                   (application-mission-start application (mission-test--specification))
                   (test-call-with-function-replacements
                    (list (list 'application-lisp--select-restart
                                (lambda (&rest arguments)
                                  (declare (ignore arguments))
                                  (incf count)
                                  nil)))
                    (lambda ()
                      (sb-ext:with-timeout 10
                        (mission-schedule-test--run-finished-work controller (list kind source)))))
                   (test-assert (= count (if (and (eq kind ':lisp) (not (eq outcome ':ok))) 1 0))
                                "only interactive failures select restarts"))
              (remhash application *mission-schedule-services*)
              (terminal-ui-stop (application-ui application))))))))
  nil)

(-> test-vault-edit-accepts-native-lisp () null)
(defun test-vault-edit-accepts-native-lisp ()
  "Accept native work durably while rejecting unknown work kinds before publication."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application          (mission-test--application configuration))
           (controller           (application-input-controller-create
                                  application :load-pending-p nil
                                  :pending-persistence-enabled-p t :start-reader-p nil))
           (*active-application* application)
           (entries              '((:native-lisp "(mission-wakeup '(:bogus 1))")))
           (accepted             nil)
           (rejected             nil))
      (unwind-protect
           (progn
             ;; No dispatch of :NATIVE-LISP: exercise the durable codec boundary.
             (handler-case
                 (progn (setf (vault-contents) entries)
                        (setf accepted (equal (vault-contents) entries)))
               (recovery-input-vault-error () nil))
             (let ((before (vault-contents)))
               (handler-case (setf (vault-contents) '((:other "(+ 1 2)")))
                 (recovery-input-vault-error (condition)
                  (setf rejected (eq (recovery-input-vault-error-operation condition) ':edit))))
               (test-assert accepted "vault editing accepts and reads back native Lisp entries")
               (test-assert rejected "vault editing rejects an unknown kind as typed :edit failure")
               (test-assert (equal (vault-contents) before)
                            "rejected unknown-kind edit does not publish replacement work")))
        (application-input-controller-stop controller)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> mission-schedule-tests--suspend (application keyword) null)
(defun mission-schedule-tests--suspend (application status)
  "Exercise the same pause and block transitions as ordinary mission commands."
  (if (eq status ':blocked)
      (application-mission-stop application ':blocked "Waiting for evidence")
      (setf (getf (application-goal application) :status) status))
  nil)

(-> mission-schedule-tests--inspect (application) list)
(defun mission-schedule-tests--inspect (application)
  "Read the public primary-owned inspection result."
  (task-tool-result-details
   (tool-execute (make-instance 'mission-schedule-tool :namespace "mission-schedule" :name "inspect")
                 (mission-test--context application)
                 (make-hash-table :test #'equal))))

(-> mission-schedule-tests--notices (application) list)
(defun mission-schedule-tests--notices (application)
  "Read persisted schedule outcomes independently of ordinary mission status notices."
  (remove-if-not (lambda (record) (equal (getf (rest record) :source) "mission schedule"))
                 (side-status-tests--records (application-conversation application))))

(-> test-mission-schedule-suspension-resume () null)
(defun test-mission-schedule-suspension-resume ()
  "Keep interval authority through pause/block and resume without re-creating schedules."
  (dolist (status '(:paused :blocked))
    (dolist (policy '(:latest :all :skip))
      (dolist (due '(120 125))
        (mission-schedule-tests--fixture
         (lambda (application service path clock commands executions)
           (declare (ignore path))
           (application-mission-schedule-add application :id "hourly" :content "Continue"
                                            :at 100 :interval 10 :missed-policy policy)
           (let ((first (first (application-mission-schedules-wake application))))
             (application-mission-schedule-execute application (mission-schedule-tests--ticket first)))
           (mission-schedule-tests--suspend application status)
           (funcall clock due)
           (let* ((wakeups  (application-mission-schedules-wake application))
                  (count    (cond ((eq policy ':all)
                                   2)
                                  ((and (eq policy ':skip) (= due 125))
                                   0)
                                  (t
                                   1)))
                  (snapshot (mission-schedule-tests--inspect application)))
             (format t "~&SUSPENSION ~S ~S due=~D results=~S cancelled=~S~%"
                     status policy due (mapcar (lambda (entry) (getf entry :result)) wakeups)
                     (getf (first (getf snapshot :schedules)) :cancelled))
             (test-assert (= count (length wakeups)) "missed policy determines which occurrences are settled")
             (test-assert (every (lambda (entry)
                                   (and (eq (getf entry :state) ':cancelled)
                                        (equal (getf entry :result) (list :skipped status)))) wakeups)
                          "suspension is inspectable as skipped with its mission status")
             (test-assert (not (getf (first (getf snapshot :schedules)) :cancelled))
                          "suspension must not cancel the interval schedule")
             (test-assert (= 130 (getf (first (getf snapshot :schedules)) :next))
                          "the next interval advances even when due work is skipped")
             (test-assert (= count (length (mission-schedule-tests--notices application)))
                          "each skipped occurrence has a persisted user notice")
             (test-assert (= 1 (length (funcall commands))) "suspended work never enters the input queue")
             (test-assert (null (application-mission-schedules-wake application)) "settled skips do not busy-loop"))
           (application-mission-resume application)
           (funcall clock 130)
           (let ((wakeups (application-mission-schedules-wake application)))
             (format t "~&RESUMED ~S ~S due=~D admitted=~D~%" status policy due (length wakeups))
             (test-assert (= 1 (length wakeups)) "the first due occurrence after resume must run")
             (when wakeups
               (application-mission-schedule-execute application (mission-schedule-tests--ticket (first wakeups)))))
           (test-assert (= 2 (length (funcall executions))) "execution continues after resume without schedule recreation")
           (test-assert (= 140 (or (cl-jobpond:scheduler-next-time (mission-schedule-scheduler service)) 0))
                        "resumed interval retains a future deadline"))))))
  nil)

(-> test-mission-schedule-suspension-execute-race () null)
(defun test-mission-schedule-suspension-execute-race ()
  "Settle queued work once when a mission is suspended between admission and execution."
  (dolist (status '(:paused :blocked))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore service path commands))
       (application-mission-schedule-add application :id "race" :content "Continue" :at 100 :interval 10)
       (let* ((wakeup (first (application-mission-schedules-wake application)))
              (ticket (mission-schedule-tests--ticket wakeup)))
         (mission-schedule-tests--suspend application status)
         (let ((result (application-mission-schedule-execute application ticket)))
           (test-assert (equal (getf result :result) (list :skipped status)) "execute-time suspension is skipped"))
         (test-assert (null (funcall executions)) "suspended queued work never runs")
         (test-assert (not (getf (first (getf (mission-schedule-tests--inspect application) :schedules)) :cancelled))
                      "execute-time suspension retains the interval")
         (test-assert (= 1 (length (mission-schedule-tests--notices application)))
                      "execute-time skip is durably presented")
         (test-assert (handler-case (progn (application-mission-schedule-execute application ticket) nil)
                        (mission-error () t)) "skipped tickets cannot replay")
         (application-mission-resume application)
         (funcall clock 110)
         (let ((next (first (application-mission-schedules-wake application))))
           (test-assert next "next occurrence is admitted after race and resume")
           (when next (application-mission-schedule-execute application (mission-schedule-tests--ticket next))))
         (test-assert (= 1 (length (funcall executions))) "only the resumed occurrence executes")))))
  nil)

(-> test-mission-schedule-stale-lifecycle () null)
(defun test-mission-schedule-stale-lifecycle ()
  "Cancel obsolete or terminal authority at both rechecks, including catch-up siblings."
  (dolist (reason '(:verified :exhausted :cancelled :failed :complete :replacement :version :non-mission))
    (dolist (execute-p '(nil t))
      (mission-schedule-tests--fixture
       (lambda (application service path clock commands executions)
         (declare (ignore service path commands))
         (application-mission-schedule-add application :id "stale" :content "Continue"
                                          :at 100 :interval 10 :missed-policy ':all)
         (let ((ticket (when execute-p
                         (mission-schedule-tests--ticket (first (application-mission-schedules-wake application))))))
           (case reason
             (:replacement
              (application-mission-start application (mission-test--specification)))
             (:version
              (setf (getf (application-goal application) :schedule-version) "new-version"))
             (:non-mission
              (setf (application-goal application) nil))
             (otherwise
              (setf (getf (application-goal application) :status) reason)))
           (funcall clock 125)
           (let ((result (if execute-p (application-mission-schedule-execute application ticket)
                             (first (application-mission-schedules-wake application)))))
             (test-assert (eq (getf result :result) ':stale-mission) "obsolete authority is cancelled, not skipped"))
           (let ((snapshot (mission-schedule-tests--inspect application)))
             (test-assert (getf (first (getf snapshot :schedules)) :cancelled) "terminal or mismatched mission cancels schedule")
             (test-assert (notany (lambda (entry) (eq (getf entry :state) ':pending)) (getf snapshot :wakeups))
                          "catch-up siblings do not remain pending after schedule cancellation"))
           (test-assert (= 1 (length (mission-schedule-tests--notices application)))
                        "automatic cancellation has a persisted user notice")
           (test-assert (null (application-mission-schedules-next-time application)) "cancelled schedules have no deadline")
           (test-assert (null (funcall executions)) "terminal or mismatched mission never executes"))))))
  nil)

(-> test-mission-schedule-suspension-events-and-recovery () null)
(defun test-mission-schedule-suspension-events-and-recovery ()
  "Retain skip evidence, event deduplication and unknown-usage admission after restoration."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore commands executions))
     (application-mission-schedule-add application :id "event" :content "Inspect" :event "ci")
     (application-mission-schedule-add application :id "once" :content "Inspect once" :at 100)
     (mission-schedule-tests--suspend application ':blocked)
     (setf (getf (application-goal application) :unknown-usage) 1)
     (application-mission-schedules-wake application)
     (application-mission-schedule-event application "ci" "event-1")
     (test-assert (handler-case (progn (application-mission-resume application) nil) (mission-error () t))
                  "unknown usage still prevents resume")
     (let ((restored (mission-schedule-service-create
                      application :path path :clock clock
                      :enqueue (mission-schedule-enqueue service) :run (mission-schedule-run service))))
       (setf (gethash application *mission-schedule-services*) restored)
       (let ((snapshot (mission-schedule-tests--inspect application)))
         (test-assert (every (lambda (entry) (equal (getf entry :result) '(:skipped :blocked)))
                             (getf snapshot :wakeups)) "skipped evidence survives scheduler restoration")
         (test-assert (notany (lambda (entry) (getf entry :cancelled)) (getf snapshot :schedules))
                      "one-shot and event authority is not falsely cancelled"))
       (test-assert (null (application-mission-schedule-event application "ci" "event-1")) "skipped event IDs remain deduplicated")
       (setf (getf (application-goal application) :unknown-usage) 0)
       (application-mission-resume application)
       (test-assert (null (application-mission-schedules-wake application)) "a skipped one-shot is not replayed after resume")
       (let ((next (application-mission-schedule-event application "ci" "event-2")))
         (test-assert (= 1 (length next)) "a new event is admitted after resume")))))
  nil)

(-> test-mission-schedule-paused-and-blocked-baseline () null)
(defun test-mission-schedule-paused-and-blocked-baseline ()
  "Demonstrate the full lost-schedule lifecycle against unmodified admission APIs."
  (dolist (status '(:paused :blocked))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore path commands))
       (application-mission-schedule-add application :id "hourly" :content "Continue" :at 100 :interval 10)
       (mission-schedule-tests--suspend application status)
       (let ((due (application-mission-schedules-wake application)))
         (format t "~&BASELINE ~S result=~S schedule-cancelled=~S~%"
                 status (getf (first due) :result)
                 (getf (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :schedules)) :cancelled)))
       (application-mission-resume application)
       (funcall clock 110)
       (let ((resumed (application-mission-schedules-wake application)))
         (format t "~&AFTER RESUME ~S admitted=~D~%" status (length resumed))
         (test-assert (= 1 (length resumed)) "a due occurrence must run after pause/block and resume")
         (when resumed
           (application-mission-schedule-execute application (mission-schedule-tests--ticket (first resumed)))))
       (test-assert (= 1 (length (funcall executions))) "resumed interval must execute without schedule recreation"))))
  nil)

(-> test-mission-schedule-unrecognized-status () null)
(defun test-mission-schedule-unrecognized-status ()
  "Withhold unfamiliar non-active states without declaring the mission terminal."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore service path executions))
     (application-mission-schedule-add application :id "future-state" :content "Continue" :at 100 :interval 10)
     (setf (getf (application-goal application) :status) ':waiting-for-review)
     (let ((wakeup (first (application-mission-schedules-wake application))))
       (test-assert (equal (getf wakeup :result) '(:skipped :waiting-for-review))
                    "unrecognized non-active status is skipped, not terminal"))
     (test-assert (null (funcall commands)) "unrecognized status does not run work")
     (test-assert (not (getf (first (getf (mission-schedule-tests--inspect application) :schedules)) :cancelled))
                  "unrecognized status retains mission authority")
     (funcall clock 110)
     (setf (getf (application-goal application) :status) ':active)
     (test-assert (= 1 (length (application-mission-schedules-wake application)))
                  "recognized active state can admit the next interval")))
  nil)

(-> test-mission-schedule-provider-failure-notice () null)
(defun test-mission-schedule-provider-failure-notice ()
  "Drive a real scheduled provider failure and inspect durable occurrence delivery."
  (with-test-configuration (configuration root)
    (let* ((failure     (make-condition 'provider-error :message "fixture_private_schedule_failure"
                                                      :code      "fixture_private_schedule_failure"))
           (application (mission-test--application configuration :results (list failure)))
           (controller  (application-input-controller-create
                         application :load-pending-p nil :pending-persistence-enabled-p t :start-reader-p nil))
           (runtime     (image-daemon:daemon-runtime-create
                         :directory root :identifier (conversation-identifier (application-conversation application))
                         :publish-p nil :request-function
                         (lambda (host request &key socket stream)
                           (declare (ignore host request socket stream)) (list :ok))))
           (escaped     nil)
           (runner      (symbol-function 'mission-schedule--run)))
      (labels ((wakeups ()
                 (getf (cl-jobpond:scheduler-snapshot
                        (mission-schedule-scheduler (application-mission-schedules application))) :wakeups))

               (notices (id)
                 (remove-if-not
                  (lambda (record)
                    (and (eq (first record) ':side-status)
                         (eql 0 (search (format nil "mission-schedule occurrence ~A claim " id)
                                        (getf (rest record) :source)))))
                  (conversation--read-records (conversation-pathname (application-conversation application))))))
        (unwind-protect
             (progn
               (application-mission-start application (mission-test--specification))
               (image-daemon:daemon-runtime-start runtime)
               (application-mission-schedules-host application runtime)
               (application-mission-schedule-add application :id "failure" :content "Observe scheduled failure."
                                                :at (1+ (get-universal-time)))
               (let ((deadline (+ (get-internal-real-time) (* 4 internal-time-units-per-second))))
                 (loop until (with-lock-held ((application-input-controller-lock controller))
                               (not (deque-empty-p (application-input-controller-work-items controller))))
                       do (when (> (get-internal-real-time) deadline) (error "No scheduled failure work."))
                          (sleep 0.01)))
               ;; Instrument the real default runner without changing its behavior.
               (setf (slot-value (application-mission-schedules application) 'run)
                     (lambda (app content)
                       (handler-case (funcall runner app content)
                         (serious-condition (condition)
                           (setf escaped condition)
                           (format t "~&SCHEDULE RUNNER ESCAPED ~S SAME-PROVIDER ~S~%"
                                   (type-of condition) (eq failure condition))
                           (error condition)))))
               (mission-schedule-test--run-finished-work
                controller (application-input-controller--next-work controller))
               (let* ((wakeup (first (wakeups))) (id (getf wakeup :id)))
                 (format t "~&SCHEDULE BASE/RESULT STATE ~S NOTICE-COUNT ~D ESCAPED-TYPE ~S~%"
                         (getf wakeup :state) (length (notices id)) (type-of escaped))
                 (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                              "the real scheduled provider was invoked exactly once")
                 (test-assert (and (typep escaped 'mission-error) (eq ':failed (mission-error-reason escaped)))
                              "the actual provider failure escapes through mission supervision")
                 (test-assert (eq ':failed (getf wakeup :state)) "known provider failure is durably failed")
                 (test-assert (= 1 (length (wakeups))) "failure creates no retry occurrence")
                 (application-mission-schedules-recover application)
                 (test-assert (= 1 (length (notices id))) "recovery publishes the deferred occurrence notice")
                 (test-assert (not (test-object-contains-string-p (notices id) "fixture_private_schedule_failure"))
                              "notices contain no provider payload")
                 (application-mission-schedules-recover application)
                 (test-assert (= 1 (length (notices id))) "reopening the scheduler deduplicates durable notices")))
          (image-daemon:daemon-runtime-stop runtime)
          (remhash application *mission-schedule-services*)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> mission-schedule-test--notices (application) list)
(defun mission-schedule-test--notices (application)
  "Read occurrence notices from authoritative conversation history."
  (remove-if-not
   (lambda (record) (and (eq (first record) ':side-status)
                        (let ((source (getf (rest record) :source)))
                          (and source (search "mission-schedule occurrence " source)))))
   (conversation--read-records (conversation-pathname (application-conversation application)))))

(-> test-mission-schedule-typed-outcomes () null)
(defun test-mission-schedule-typed-outcomes ()
  "Consume normal failed/unknown returns and preserve later recurring execution."
  (dolist (state '(:failed :unknown :completed))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore path commands executions))
       (setf (slot-value service 'run)
             (lambda (owner content)
               (declare (ignore owner content))
               (make-mission-schedule-outcome :state state :reason ':runner-failed)))
       (application-mission-schedule-add application :id "typed" :content "Typed outcome" :at 100 :interval 10)
       (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
         (application-mission-schedule-execute application ticket)
         (test-assert (eq state (getf (first (getf (cl-jobpond:scheduler-snapshot
                                                    (mission-schedule-scheduler service)) :wakeups)) :state))
                      "the explicit runner state controls acknowledgement")
         (test-assert (= (if (eq state ':completed) 0 1) (length (mission-schedule-test--notices application)))
                      "failed and unknown outcomes get a notice, completed work does not")
         (test-assert (null (application-mission-schedules-wake application)) "no automatic retry")
         (setf (slot-value service 'run)
               (lambda (owner content) (declare (ignore owner content))
                 (make-mission-schedule-outcome :state ':completed :reason ':executed)))
         (funcall clock 110)
         (let ((next (first (application-mission-schedules-wake application))))
           (test-assert next "future recurring occurrence is admitted")
           (test-assert (not (equal (getf ticket :id) (getf next :id))) "recurrence has a distinct occurrence identity")
           (test-assert (eq ':completed (getf (application-mission-schedule-execute
                                               application (mission-schedule-tests--ticket next)) :state))
                        "future recurring work completes normally"))))))
  nil)

(-> test-mission-schedule-terminal-classification () null)
(defun test-mission-schedule-terminal-classification ()
  "Classify terminal exits, not recovered provider signals, and preserve control identity."
  (dolist (kind '(:recovered :provider :ambiguous :cancel :fatal))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore path clock commands executions))
       (let* ((condition (case kind
                           ((:recovered :provider)
                            (make-condition 'provider-error :message "private-fixture"))
                           (:ambiguous
                            (make-condition 'mission-error :reason ':failed :message "private-fixture"))
                           (:cancel
                            (make-condition 'application-turn-cancelled))
                           (:fatal
                            (make-condition 'agent-loop-error :message "fatal-fixture"
                                                              :conversation-id "fixture" :request-number nil))))
              (caught    nil))
         (setf (slot-value service 'run)
               (lambda (owner content)
                 (declare (ignore owner content))
                 (mission-schedule--announce service)
                 (test-assert (null (mission-schedule-test--notices application))
                              "actively executing UNKNOWN is excluded")
                 (if (eq kind ':recovered)
                     (progn (handler-case (error condition) (provider-error () nil))
                            (make-mission-schedule-outcome :state ':completed :reason ':executed))
                     (error condition))))
         (application-mission-schedule-add application :id "terminal" :content "Terminal" :at 100)
         (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
           (handler-case (application-mission-schedule-execute application ticket)
             (serious-condition (failure) (setf caught failure)))
           (test-assert (if (eq kind ':recovered) (null caught) (eq caught condition))
                        "terminal condition identity propagates unchanged")
           (test-assert (eq (case kind (:recovered
                                        ':completed)
                                       (:provider
                                        ':failed)
                                       (otherwise
                                        ':unknown))
                            (getf (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service))
                                              :wakeups)) :state))
                        "only a proven terminal provider failure becomes FAILED")
           (mission-schedule--announce service)
           (test-assert (= (if (eq kind ':recovered) 0 1) (length (mission-schedule-test--notices application)))
                        "uncertain exits are recoverably announced once")
           (test-assert (not (test-object-contains-string-p (mission-schedule-test--notices application) "private-fixture"))
                        "failure reports never enter new notices"))))))
  nil)

(-> test-mission-schedule-notice-recovery-windows () null)
(defun test-mission-schedule-notice-recovery-windows ()
  "Recover failure-before-append and presentation-after-append with durable concurrent dedup."
  (dolist (append-first-p '(nil t))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore commands executions))
       (let ((present             (symbol-function 'mission-schedule--append-notice))
             (publication-failure (make-condition 'simple-error :format-control "injected publication failure"))
             (caught              nil))
         (setf (slot-value service 'run)
               (lambda (owner content) (declare (ignore owner content))
                 (make-mission-schedule-outcome :state ':failed :reason ':provider-failed)))
         (application-mission-schedule-add application :id "window" :content "Failure window" :at 100)
         (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
           (test-call-with-function-replacements
            (list (list 'mission-schedule--append-notice
                        (lambda (owner source spans)
                          (when append-first-p (funcall present owner source spans))
                          (error publication-failure))))
            (lambda ()
              (handler-case (application-mission-schedule-execute application ticket)
                (serious-condition (condition) (setf caught condition)))))
           (test-assert (eq caught publication-failure) "publication errors propagate instead of being swallowed")
           (test-assert (= (if append-first-p 1 0) (length (mission-schedule-test--notices application)))
                        "the injected boundary distinguishes absent append from durable append")
           (let* ((restored (mission-schedule-service-create application :path path :clock clock))
                  (record   (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler restored)) :wakeups))))
             (test-assert (and (eq ':failed (getf record :state)) (eq ':provider-failed (getf record :result)))
                          "the pinned dependency retains terminal state and result across reopen")
             (setf (gethash application *mission-schedule-services*) restored)
             (let ((threads (loop repeat 4 collect
                                  (make-thread (lambda () (mission-schedule--announce restored))
                                               :name "Occurrence notice publisher"))))
               (dolist (thread threads) (join-thread thread)))
             (test-assert (= 1 (length (mission-schedule-test--notices application)))
                          "concurrent recovery appends exactly one durable occurrence notice")
             (let ((original (application-conversation application)))
               (unwind-protect
                    (progn
                      (setf (application-conversation application) (conversation-load (conversation-pathname original)))
                      (mission-schedule--announce restored)
                      (test-assert (= 1 (length (mission-schedule-test--notices application)))
                                   "reopened conversation history deduplicates independently of memory"))
                 (setf (application-conversation application) original)))))))))
  nil)

(-> test-mission-schedule-restored-failure-notices () null)
(defun test-mission-schedule-restored-failure-notices ()
  "Include admission refusals and persisted failed/claimed records without result metadata."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore commands executions))
     (setf (slot-value service 'enqueue) (lambda (owner command) (declare (ignore owner command)) nil))
     (application-mission-schedule-add application :id "refused" :content "Refused" :at 100)
     (let ((refused (first (application-mission-schedules-wake application))))
       (test-assert (and (eq ':failed (getf refused :state)) (eq ':admission-refused (getf refused :result)))
                    "admission semantics are preserved")
       (test-assert (= 1 (length (mission-schedule-test--notices application))) "admission refusal has a keyed notice"))
     (setf (slot-value service 'enqueue) (lambda (owner command) (declare (ignore owner command)) t))
     (application-mission-schedule-add application :id "old-failed" :content "Old failure" :at 100)
     (let ((wakeup (first (application-mission-schedules-wake application))))
       (cl-jobpond:scheduler-ack (mission-schedule-scheduler service)
                                :id (getf wakeup :id) :token (getf wakeup :token) :outcome ':failed))
     (application-mission-schedule-add application :id "interrupted" :content "Interrupted" :at 100)
     (application-mission-schedules-wake application)
     (let ((restored (mission-schedule-service-create application :path path :clock clock)))
       (mission-schedule--announce restored)
       (mission-schedule--announce restored)
       (test-assert (= 3 (length (mission-schedule-test--notices application)))
                    "restored FAILED without result and CLAIMED-to-UNKNOWN each receive exactly one notice"))))
  nil)

(-> test-mission-schedule-notice-conversation-switch () null)
(defun test-mission-schedule-notice-conversation-switch ()
  "Switch at both publication boundaries and inspect both durable histories."
  (dolist (boundary '(mission-schedule--publish-notice mission-schedule--append-notice))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore path clock commands executions))
       (let* ((original (application-conversation application))
              (other    (conversation-create (application-configuration application)))
              (publish  (symbol-function boundary)))
         (setf (slot-value service 'run)
               (lambda (owner content)
                 (declare (ignore owner content))
                 (make-mission-schedule-outcome :state ':failed :reason ':provider-failed)))
         (application-mission-schedule-add application :id "switch" :content "Switch" :at 100)
         (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
           (unwind-protect
                (progn
                  (test-call-with-function-replacements
                   (list (list boundary
                               (lambda (&rest arguments)
                                 (setf (application-conversation application) other)
                                 (apply publish arguments))))
                   (lambda () (application-mission-schedule-execute application ticket)))
                  (test-assert (null (mission-schedule-test--notices application))
                               "the replacement conversation receives no old occurrence")
                  (setf (application-conversation application) original)
                  (test-assert (= 1 (length (mission-schedule-test--notices application)))
                               "the captured owner receives its occurrence notice")
                  (mission-schedule--announce service)
                  (test-assert (= 1 (length (mission-schedule-test--notices application)))
                               "reconciliation deduplicates against the captured owner"))
             (setf (application-conversation application) original)))))))
  nil)

(-> test-mission-schedule-non-error-controls () null)
(defun test-mission-schedule-non-error-controls ()
  "Preserve actual non-error rollback/update escapes despite a failing publisher."
  (dolist (control (list (make-condition 'rollback-requested :generation-id "fixture")
                        (make-condition 'update-requested :tag "fixture")))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore path clock commands executions))
       (let ((caught nil) (published-p nil))
         (setf (slot-value service 'run)
               (lambda (owner content)
                 (declare (ignore owner content))
                 (signal control)
                 (error "The outer control handler should transfer.")))
         (application-mission-schedule-add application :id "control" :content "Control" :at 100)
         (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
           (test-call-with-function-replacements
            (list (list 'mission-schedule--publish-notice
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (setf published-p t)
                          (error "Publication must not mask the control transfer."))))
            (lambda ()
              (catch 'control-escape
                (handler-bind ((autolith-control-condition
                                (lambda (condition)
                                  (setf caught condition)
                                  (throw 'control-escape nil))))
                  (application-mission-schedule-execute application ticket)))))
           (test-assert (eq caught control) "the original non-error control object reaches its outer handler")
           (test-assert (not published-p) "cleanup does not attempt publication during control transfer")
           (test-assert (eq ':unknown (getf (first (getf (cl-jobpond:scheduler-snapshot
                                                          (mission-schedule-scheduler service)) :wakeups)) :state))
                        "a control escape remains UNKNOWN")
           (test-assert (zerop (hash-table-count (mission-schedule-executing service)))
                        "control unwind releases the execution reservation")
           (mission-schedule--announce service)
           (test-assert (= 1 (length (mission-schedule-test--notices application)))
                        "later reconciliation publishes the retained interruption"))))))
  nil)

(-> test-mission-schedule-pending-exits () null)
(defun test-mission-schedule-pending-exits ()
  "Preserve every pending exit when occurrence publication would fail."
  (dolist (kind '(:ordinary :provider :cancel :fatal :throw))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore commands executions))
       (let* ((failure     (case kind
                             (:provider
                              (make-condition 'provider-error :message "private-fixture"))
                             (:cancel
                              (make-condition 'application-turn-cancelled))
                             (:fatal
                              (make-condition 'agent-loop-error :message "fatal-fixture"
                                                               :conversation-id "fixture" :request-number nil))
                             (otherwise
                              (make-condition 'simple-error :format-control "runner-fixture"))))
              (caught      nil)
              (published-p nil))
         (setf (slot-value service 'run)
               (lambda (owner content)
                 (declare (ignore owner content))
                 (if (eq kind ':throw) (throw 'runner-exit ':escaped) (error failure))))
         (application-mission-schedule-add application :id "pending" :content "Exit" :at 100)
         (let ((ticket (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
           (test-call-with-function-replacements
            (list (list 'mission-schedule--publish-notice
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (setf published-p t)
                          (error "publication-fixture"))))
            (lambda ()
              (handler-case
                  (setf caught (catch 'runner-exit
                                 (application-mission-schedule-execute application ticket)))
                (serious-condition (condition) (setf caught condition)))))
           (test-assert (if (eq kind ':throw) (eq caught ':escaped) (eq caught failure))
                        "the pending exit retains its original identity or value")
           (test-assert (not published-p) "unwinding does not invoke the notice publisher")
           (test-assert (zerop (hash-table-count (mission-schedule-executing service)))
                        "every exit releases the reservation")
           (let* ((restored (mission-schedule-service-create application :path path :clock clock))
                  (record   (first (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler restored)) :wakeups))))
             (test-assert (eq (if (eq kind ':provider) ':failed ':unknown) (getf record :state))
                          "the durable outcome survives the failed turn")
             (mission-schedule--announce restored)
             (mission-schedule--announce restored)
             (test-assert (= 1 (length (mission-schedule-test--notices application)))
                          "recovery publishes the retained outcome exactly once")))))))
  nil)

(-> test-mission-schedule-retry-failure-notices () null)
(defun test-mission-schedule-retry-failure-notices ()
  "A prior interruption notice cannot suppress a later explicit failure or retry."
  (dolist (action '(:failed :retry))
    (mission-schedule-tests--fixture
     (lambda (application service path clock commands executions)
       (declare (ignore commands executions))
       (setf (slot-value service 'run)
             (lambda (owner content)
               (declare (ignore owner content))
               (make-mission-schedule-outcome)))
       (application-mission-schedule-add application :id "retry-notice" :content "Retry" :at 100)
       (let* ((wakeup (first (application-mission-schedules-wake application)))
              (ticket (mission-schedule-tests--ticket wakeup)))
         (application-mission-schedule-execute application ticket)
         (test-assert (= 1 (length (mission-schedule-test--notices application)))
                      "the interrupted attempt was announced")
         (application-mission-schedule-resolve application (getf ticket :id)
                                               :token (getf ticket :token) :action action)
         (when (eq action ':retry)
           (setf (slot-value service 'run)
                 (lambda (owner content)
                   (declare (ignore owner content))
                   (make-mission-schedule-outcome :state ':failed :reason ':provider-failed)))
           (application-mission-schedule-execute
            application (mission-schedule-tests--ticket (first (application-mission-schedules-wake application)))))
         (mission-schedule--announce service)
         (test-assert (= 2 (length (mission-schedule-test--notices application)))
                      "a later terminal failure has its own durable notice")
         (let ((restored (mission-schedule-service-create application :path path :clock clock)))
           (mission-schedule--announce restored)
           (test-assert (= 2 (length (mission-schedule-test--notices application)))
                        "reopening deduplicates both announcement identities"))))))
  nil)

(-> test-mission-schedule-hosted-notice-resolution () null)
(defun test-mission-schedule-hosted-notice-resolution ()
  "Recheck a daemon timer's snapshot after primary recovery changes its outcome."
  (mission-schedule-tests--fixture
   (lambda (application service path clock commands executions)
     (declare (ignore commands executions))
     (let* ((now          (get-universal-time))
            (entered      (sb-thread:make-semaphore))
            (release      (sb-thread:make-semaphore))
            (finished     (sb-thread:make-semaphore))
            (primary      sb-thread:*current-thread*)
            (timer-thread nil)
            (publisher    (symbol-function 'mission-schedule--publish-notice))
            (runtime      (image-daemon:daemon-runtime-create
                           :directory  (uiop:pathname-directory-pathname path)
                           :identifier (conversation-identifier (application-conversation application))
                           :publish-p nil :request-function
                           (lambda (host request &key socket stream)
                             (declare (ignore host request socket stream)) (list :ok)))))
       (unwind-protect
            (progn
              (image-daemon:daemon-runtime-start runtime)
              (application-mission-schedules-host application runtime)
              (setf now (get-universal-time))
              (funcall clock now)
              (setf (slot-value service 'run)
                    (lambda (owner content)
                      (declare (ignore owner content))
                      (throw 'interrupted nil)))
              (application-mission-schedule-add application :id "timer-resolution" :content "Timer"
                                               :at (- now 3590) :interval 3600)
              (application-mission-schedules-wake application)
              (let ((ticket (mission-schedule-tests--ticket
                             (first (getf (cl-jobpond:scheduler-snapshot
                                           (mission-schedule-scheduler service)) :wakeups)))))
                (catch 'interrupted (application-mission-schedule-execute application ticket))
                (test-call-with-function-replacements
                 (list (list 'mission-schedule--publish-notice
                             (lambda (owner wakeup reason)
                               (when (eq reason ':interrupted)
                                (setf timer-thread sb-thread:*current-thread*)
                                (sb-thread:signal-semaphore entered)
                                (unless (sb-thread:wait-on-semaphore release :timeout 10)
                                  (error "Primary did not release timer publication.")))
                               (unwind-protect (funcall publisher owner wakeup reason)
                                 (sb-thread:signal-semaphore finished)))))
                 (lambda ()
                   (funcall clock (+ now 10))
                   (unless (sb-thread:wait-on-semaphore entered :timeout 15)
                     (error "Daemon timer did not reach occurrence publication."))
                   (test-assert (and timer-thread (not (eq primary timer-thread)))
                                "the real daemon timer runs independently of primary recovery")
                   (let ((notify (symbol-function 'mission-schedule--notify)))
                     (test-call-with-function-replacements
                      (list (list 'mission-schedule--notify
                                  (lambda (owner)
                                    (sb-thread:signal-semaphore release)
                                    (funcall notify owner))))
                      (lambda ()
                        (application-mission-schedule-resolve application (getf ticket :id)
                                                              :token (getf ticket :token) :action ':completed))))
                   (unless (sb-thread:wait-on-semaphore finished :timeout 8)
                     (error "Daemon timer did not finish occurrence publication."))
                   (test-assert (null (mission-schedule-test--notices application))
                                "a resolved outcome cannot publish a stale interruption notice")))))
         (sb-thread:signal-semaphore release)
         (image-daemon:daemon-runtime-stop runtime)))))
  nil)
