(in-package #:autolith)

;;;; -- Idle Compaction Boundaries --

(-> idle-compaction-tests--fixture (function &key (:results list)) null)
(defun idle-compaction-tests--fixture
    (function &key (results (list (agent-test-result
                                   "idle-summary" (list (agent-test-compaction-message))
                                   :turn-completion ':end))))
  "Run FUNCTION with a real recorded application, controller, and scripted provider."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration :results results))
           (provider    (application-provider application))
           (controller  (make-instance 'application-input-controller
                                       :application application :main-thread (current-thread))))
      (setf (slot-value provider 'configuration) configuration
            (application-input-controller application) controller
            (config :prompt-cache-retention-seconds configuration) 300)
      (change-class (terminal-ui-terminal (application-ui application)) 'waiting-recording-terminal)
      (conversation-append-user-message
       (application-conversation application)
       (make-string *idle-compaction-minimum-history-characters* :initial-element #\x))
      (unwind-protect
           (funcall function application controller configuration)
        (setf (application-input-controller application) nil)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> idle-compaction-tests--prime
    (application &key (:started-at real) (:tokens (integer 0))) prompt-cache-episode)
(defun idle-compaction-tests--prime (application &key (started-at 1000) (tokens 40000))
  "Complete one ordinary request without advancing its original cache timestamp."
  (application-idle-compaction-note-started application :now started-at)
  (application-idle-compaction-note-completed
   application (prompt-cache-tests--usage tokens 39000))
  (application-idle-compaction-episode application))

(-> test-idle-compaction-policy () null)
(defun test-idle-compaction-policy ()
  "Exercise estimated retention, completed live episodes, identity, and single-shot consumption."
  (idle-compaction-tests--fixture
   (lambda (application controller configuration)
     (declare (ignore controller))
     (test-assert (config :idle-compaction-p configuration) "idle compaction defaults on")
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "resumed usage without a local cache opportunity cannot schedule compaction")
     (idle-compaction-tests--prime application)
     (dolist (case '((999 nil) (1000 180) (1179 1) (1180 0) (1299 0) (1300 nil)))
       (destructuring-bind (now expected) case
         (test-assert (eql expected (application-idle-compaction-delay application :now now))
                      "only the bounded pre-expiry window is due")))
     (setf (config :idle-compaction-p configuration) nil)
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "disabling the durable setting suppresses the opportunity")
     (setf (config :idle-compaction-p configuration) t
           (config :prompt-cache-retention-seconds configuration) 0)
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "an unknown provider has no implicit retention estimate")
     (test-assert (= 300 (provider-prompt-cache-retention-estimate
                          (make-instance 'codex-subscription-provider)))
                  "ChatGPT supplies an explicit scheduling heuristic")
     (setf (config :prompt-cache-retention-seconds configuration) 1800)
     (test-assert (= 1500 (application-idle-compaction-delay application :now 1180))
                  "an explicit retention estimate overrides provider defaults")
     (setf (config :prompt-cache-retention-seconds configuration) 120)
     (test-assert (null (application-idle-compaction-delay application :now 1000))
                  "retention shorter than the lead cannot form a safe window")
     (setf (config :prompt-cache-retention-seconds configuration) 300)
     (idle-compaction-tests--prime application :tokens 1000)
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "small prompts do not buy an extra compaction request")
     (let* ((conversation (application-conversation application))
            (items        (conversation-input-items conversation)))
       (unwind-protect
            (progn
              (setf (conversation-input-items conversation)
                    (list (json-object "type" "message" "content" "Tiny history")))
              (idle-compaction-tests--prime application)
              (test-assert (null (application-idle-compaction-delay application :now 1180))
                           "large fixed instructions cannot make tiny history worth compacting"))
         (setf (conversation-input-items conversation) items)))
     (test-assert (zerop (application-idle-compaction--history-characters
                          (json-object "image_url" (make-string 140000 :initial-element #\x)
                                       "encrypted_content" "private")))
                  "encoded images and private metadata are not summarizable text")
     (application-idle-compaction-note-started application :now 1000)
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "in-flight requests cannot schedule idle compaction")
     (application-idle-compaction-note-completed application nil)
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "missing usage cannot establish worthwhile compaction")
     (let* ((episode      (idle-compaction-tests--prime application))
            (old-model    (config :model configuration))
            (old-provider (agent-provider (application-agent application))))
       (setf (config :model configuration)
             (find-if (lambda (model) (not (equal model old-model)))
                      (setting-options (configuration-setting configuration :model) configuration)))
       (test-assert (null (application-idle-compaction-delay application :now 1180))
                    "a changed model invalidates the cache opportunity")
       (setf (config :model configuration) old-model
             (agent-provider (application-agent application))
             (make-instance 'scripted-provider :results nil))
       (test-assert (null (application-idle-compaction-delay application :now 1180))
                    "a replaced provider cannot inherit another provider's cache")
       (setf (agent-provider (application-agent application)) old-provider)
       (let ((conversation (application-conversation application)))
         (setf (application-conversation application) (conversation-create configuration))
         (test-assert (null (application-idle-compaction-delay application :now 1180))
                      "a different conversation cannot inherit the opportunity")
         (setf (application-conversation application) conversation))
       (let ((*idle-compaction-clock-function* (lambda () 1180)))
         (test-assert (application-idle-compaction-consume application episode)
                      "a due opportunity is consumed before paid work")
         (test-assert (not (application-idle-compaction-consume application episode))
                      "a consumed opportunity cannot repeat")))
     (idle-compaction-tests--prime application)
     (let ((other-process-id (1+ (current-process-id))))
       (test-call-with-function-replacements
        (list (list 'current-process-id (lambda () other-process-id)))
        (lambda ()
          (test-assert (null (application-idle-compaction-delay application :now 1180))
                       "a restored heap cannot reuse another process's monotonic deadline"))))
     (conversation-append-user-message (application-conversation application) "New history.")
     (test-assert (null (application-idle-compaction-delay application :now 1180))
                  "changed history requires a new completed request")
     (idle-compaction-tests--prime application)
     (test-assert (null (application-idle-compaction-delay application :now 1350))
                  "a long response does not shift expiry to response completion")))
  nil)

(-> test-idle-compaction-controller () null)
(defun test-idle-compaction-controller ()
  "Schedule from real request events and publish one durable idle checkpoint."
  (idle-compaction-tests--fixture
   (lambda (application controller configuration)
     (declare (ignore configuration))
     (let* ((now                              1000)
            (*idle-compaction-clock-function* (lambda () now))
            (provider                         (application-provider application)))
       (push (mission-test--result "Ordinary answer."
                                   :usage (json-object "input_tokens" 40000 "output_tokens" 10))
             (scripted-provider-results provider))
       (application--run-message-input application "Continue" :tools-p nil :goal-continuations-p nil)
       (test-assert (eql 180 (application-idle-compaction-delay application))
                    "ordinary request hooks establish the cache deadline")
       (setf now 1180)
       (let ((work (application-input-controller--next-work controller)))
         (test-assert (eq (first work) ':idle-compaction)
                      "the idle controller admits compaction without new input")
         (application-input-controller--run-work controller work)
         (test-assert (= 2 (length (scripted-provider-input-snapshots provider)))
                      "one ordinary answer and one summary request run")
         (test-assert (equal '(nil t) (reverse (scripted-provider-compaction-flags provider)))
                      "only idle work uses the provider's compaction protocol")
         (test-assert (find :summary (conversation-records-newest (application-conversation application) 20)
                            :key     #'first)
                      "the handoff is durably published")
         (test-assert (null (application-idle-compaction-episode application))
                      "compaction does not rearm itself")
         (application-input-controller--run-work controller work)
         (test-assert (= 2 (length (scripted-provider-input-snapshots provider)))
                      "replaying selected work cannot compact twice")
         (application-input-controller--finish-work controller)))))
  nil)

(-> test-idle-compaction-input-races () null)
(defun test-idle-compaction-input-races ()
  "Give drafts, queued input, shutdown, changed settings, and superseding requests priority."
  (dolist (race '(:draft :queue :stopping :disabled :superseded :expired :buffered-input))
    (idle-compaction-tests--fixture
     (lambda (application controller configuration)
       (let ((now 1180))
         (let ((*idle-compaction-clock-function* (lambda () now)))
           (idle-compaction-tests--prime application)
           (let ((work (application-input-controller--next-work controller)))
             (ecase race
               (:draft
                (terminal-ui-set-input (application-ui application) "User draft"))
               (:queue
                (deque-push-back (application-input-controller-work-items controller)
                                 (list ':message "Pending user input")))
               (:stopping
                (setf (application-input-controller-stopping-p controller) t))
               (:disabled
                (setf (config :idle-compaction-p configuration) nil))
               (:superseded
                (application-idle-compaction-note-started application :now 1180))
               (:expired
                (setf now 1300))
               (:buffered-input
                nil))
             (test-call-with-function-replacements
              (when (eq race ':buffered-input)
                (list (list 'terminal-input-ready-p (lambda (terminal) (declare (ignore terminal)) t))))
              (lambda () (application-input-controller--run-work controller work)))
             (test-assert (null (scripted-provider-input-snapshots (application-provider application)))
                          (format nil "~A prevents a late idle compaction" race))
             (when (eq race ':queue)
               (test-assert (= 1 (deque-count (application-input-controller-work-items controller)))
                            "admission does not consume pending user input"))))))))
  nil)

(-> test-idle-compaction-failure () null)
(defun test-idle-compaction-failure ()
  "Leave history intact after a failed provider response without retrying idle inference."
  (idle-compaction-tests--fixture
   (lambda (application controller configuration)
     (declare (ignore configuration))
     (let ((*idle-compaction-clock-function* (lambda () 1180)))
       (idle-compaction-tests--prime application)
       (application-input-controller--run-work
        controller (application-input-controller--next-work controller))
       (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                    "a failing compaction issues one request")
       (test-assert (not (find :summary
                               (conversation-records-newest (application-conversation application) 20)
                               :key #'first))
                    "a failed summary is not published")
       (test-assert (null (application-idle-compaction-delay application))
                    "failure consumes the opportunity rather than busy-retrying")))
   :results (list (mission-test--result "")))
  nil)

(-> test-idle-compaction-deadline-wakeup () null)
(defun test-idle-compaction-deadline-wakeup ()
  "Wake a genuinely waiting native controller at its cache deadline without user input."
  (idle-compaction-tests--fixture
   (lambda (application controller configuration)
     (let* ((*idle-compaction-lead-seconds* 4/5)
            (started                        (/ (get-internal-real-time) internal-time-units-per-second)))
       (setf (config :prompt-cache-retention-seconds configuration) 1)
       (application-idle-compaction-note-started application)
       (application-idle-compaction-note-completed application (prompt-cache-tests--usage 40000 39000))
       (let ((work (application-input-controller--next-work controller)))
         (test-assert (eq (first work) ':idle-compaction)
                      "the timed idle wait admits work without an input notification")
         (test-assert (>= (- (/ (get-internal-real-time) internal-time-units-per-second) started) 1/10)
                      "the controller waits for the deadline instead of compacting immediately")))))
  nil)
