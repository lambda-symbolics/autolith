(in-package #:autolith)

;;;; -- Mission Schedule Product Boundaries --

(-> mission-schedule-tests--fixture (function) null)
(defun mission-schedule-tests--fixture (function)
  "Call FUNCTION with a real mission, durable scheduler and observable ordinary admission."
  (with-test-configuration (configuration root)
    (let* ((application (mission-test--application configuration))
           (path (merge-pathnames "mission-wakeups.sexp" root))
           (now 100) (commands nil) (executions nil)
           (service (mission-schedule-service-create
                     application :path path :clock (lambda () now)
                     :enqueue (lambda (owner command)
                                (test-assert (eq owner application) "admission retains the owning primary")
                                (push command commands) t)
                     :run (lambda (owner content)
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
     (declare (ignore path executions))
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
                    "occurrence history retains duplicate proof")))))

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
     (let* ((scheduler (mission-schedule-scheduler (application-mission-schedules application)))
            (before (cl-jobpond:scheduler-snapshot scheduler))
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
            (effects 0)
            (restored (mission-schedule-service-create application :path path :clock (lambda () 100)
                                                       :enqueue (lambda (owner command) (declare (ignore owner command)) t)
                                                       :run (lambda (owner content) (declare (ignore owner content)) (incf effects)))))
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
     (declare (ignore service path clock commands executions))
     (let* ((registry (application-tool-registry application))
            (context (mission-test--context application))
            (other (agent-create :configuration (application-configuration application)
                                 :conversation (application-conversation application)
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
       (test-assert (= 1 (length (application-mission-schedule-event application "ci" "build-17"))) "observable event admits work")
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
                :run (lambda (owner content)
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
           (controller (application-input-controller-create application :load-pending-p nil
                                                                        :pending-persistence-enabled-p nil :start-reader-p nil))
           (runtime (image-daemon:daemon-runtime-create
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
               (test-assert (eq (first work) ':command) "timer work uses the ordinary durable command queue")
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
