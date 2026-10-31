(in-package #:autolith)

;;;; -- ACP Idle Compaction Tests --

(-> acp-idle-compaction-tests--fixture (function) null)
(defun acp-idle-compaction-tests--fixture (function)
  "Drive the real ACP cycle manually with an owned scripted session."
  (acp-input-test--session
   (lambda (session client)
     (declare (ignore client))
     (let ((application (acp-session-application session)))
      (setf (slot-value (application-provider application) 'configuration)
            (application-configuration application)
            (scripted-provider-results (application-provider application))
            (list (agent-test-result "acp-idle-summary" (list (agent-test-compaction-message))
                                     :turn-completion ':end)))
      (setf (config :prompt-cache-retention-seconds (application-configuration application)) 300)
      (conversation-append-user-message
       (application-conversation application)
       (make-string *idle-compaction-minimum-history-characters* :initial-element #\x))
      (idle-compaction-tests--prime application)
      (let ((*idle-compaction-clock-function* (lambda () 1180)))
        (funcall function session application)))))
  nil)

(-> test-acp-idle-compaction-publishes-and-consumes () null)
(defun test-acp-idle-compaction-publishes-and-consumes ()
  "Publish one idle checkpoint under ACP ownership without adding a user prompt."
  (acp-idle-compaction-tests--fixture
   (lambda (session application)
     (let* ((provider                         (application-provider application))
            (*idle-compaction-clock-function* (lambda () 1000)))
       (push (mission-test--result "Ordinary answer."
                                   :usage (json-object "input_tokens" 40000 "output_tokens" 10))
             (scripted-provider-results provider))
       (acp-session-run-turn session "Continue")
       (test-assert (eql 180 (application-idle-compaction-delay application))
                    "ACP request hooks establish the cache deadline"))
     (let ((history (conversation-input-history (application-conversation application))))
       (test-assert (zerop (acp-completion--cycle session)) "due idle work runs through ACP")
       (test-assert (= 2 (length (scripted-provider-input-snapshots (application-provider application))))
                    "one ordinary answer and one summary request run")
       (test-assert (find :summary (conversation-records-newest (application-conversation application) 20)
                          :key     #'first)
                    "ACP durably publishes the handoff")
       (test-assert (equal history (conversation-input-history (application-conversation application)))
                    "compaction adds no user prompt")
       (test-assert (null (application-idle-compaction-episode application)) "the episode is consumed")
       (test-assert (null (acp-session-prompt-thread session)) "prompt ownership is released")
       (test-assert (null (acp-completion--cycle session)) "consumed idle work cannot repeat")
       (test-assert (= 2 (length (scripted-provider-input-snapshots (application-provider application))))
                    "no second paid request occurs"))))
  nil)

(-> test-acp-idle-compaction-prioritizes-input-and-close () null)
(defun test-acp-idle-compaction-prioritizes-input-and-close ()
  "Give queued prompts, session closure, and active ownership precedence."
  (dolist (case '(:queued :closing :active))
    (acp-idle-compaction-tests--fixture
     (lambda (session application)
       (let ((controller (acp-input-controller session)))
         (unwind-protect
              (progn
                (ecase case
                  (:queued
                   (acp-input-test--submit session "_autolith/queue" "queued user work"))
                  (:closing
                   (setf (acp-session-closing-p session) t))
                  (:active
                   (setf (acp-session-prompt-thread session) (current-thread)
                         (application-input-controller-active-p controller) t)))
                (acp-completion--cycle session)
                (test-assert (notany #'identity
                                     (scripted-provider-compaction-flags (application-provider application)))
                             "higher-priority state prevents paid compaction")
                (when (eq case ':queued)
                  (test-assert (member "queued user work"
                                       (conversation-input-history (application-conversation application))
                                       :test #'equal)
                               "the queued prompt runs before idle maintenance")))
           (setf (acp-session-closing-p session) nil
                 (acp-session-prompt-thread session) nil
                 (application-input-controller-active-p controller) nil))))))
  nil)

(-> test-acp-idle-compaction-failure-cleans-state () null)
(defun test-acp-idle-compaction-failure-cleans-state ()
  "Clean ownership after a failed summary without retrying the paid opportunity."
  (acp-idle-compaction-tests--fixture
   (lambda (session application)
     (setf (scripted-provider-results (application-provider application))
           (list (agent-test-result "empty-summary" nil)))
     (test-assert (handler-case (progn (acp-completion--cycle session) nil)
                    (provider-protocol-error () t))
                  "an unusable summary signals its typed failure")
     (test-assert (null (acp-session-prompt-thread session)) "failure releases prompt ownership")
     (test-assert (null (acp-session-prompt-interruptible-p session)) "failure finalizes interruption state")
     (test-assert (not (application-input-controller-active-p (acp-input-controller session)))
                  "failure releases native controller activity")
     (test-assert (null (application-idle-compaction-episode application)) "failure consumes the episode")
     (test-assert (null (acp-completion--cycle session)) "failure does not schedule another request")
     (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                  "failure makes exactly one summary request")))
  nil)

(-> test-acp-idle-compaction-schedules-before-future-notice () null)
(defun test-acp-idle-compaction-schedules-before-future-notice ()
  "Use the nearer cache deadline, then compact even while a future notice is waiting."
  (acp-idle-compaction-tests--fixture
   (lambda (session application)
     (let ((now 1000))
       (let ((*idle-compaction-clock-function* (lambda () now)))
         (test-call-with-function-replacements
          (list (list 'acp-completion--admitted-notices
                      (lambda (ignored)
                        (declare (ignore ignored))
                        (list (list :ready-at (+ (get-universal-time) 3600))))))
          (lambda ()
            (test-assert (= 180 (acp-completion--cycle session)) "the nearer cache deadline wins")
            (setf now 1180)
            (test-assert (zerop (acp-completion--cycle session)) "a future notice does not hide due compaction")
            (test-assert (= 1 (length (scripted-provider-input-snapshots (application-provider application))))
                         "the cache opportunity is used before that future notice")))))))
  nil)

(-> test-acp-idle-compaction-cancellation () null)
(defun test-acp-idle-compaction-cancellation ()
  "Cancel a provider-blocked idle checkpoint over ACP and reuse its primary session."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                   :results       (list (acp-session-test-text-result "resumed")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
                (session     (acp-service--session service identifier))
                (application (acp-session-application session)))
          (setf (config :prompt-cache-retention-seconds (application-configuration application)) 300
                (config :idle-compaction-p (application-configuration application)) nil)
          (conversation-append-user-message
           (application-conversation application)
           (make-string *idle-compaction-minimum-history-characters* :initial-element #\x))
          (idle-compaction-tests--prime
           application :started-at (- (funcall *idle-compaction-clock-function*) 181))
          (acp-extension-set-setting
           session (json-object "name" "idle-compaction-p" "value" t))
          (acp-completion-test--wait (lambda () (acp-session-test-gated-entered-p provider)))
          (agentcomms:client-cancel client identifier)
          (acp-completion-test--wait
           (lambda () (and (acp-session-cancelled-p session)
                           (null (acp-session-prompt-thread session)))))
          (test-assert (null (application-idle-compaction-episode application))
                       "cancellation cannot rearm the idle checkpoint")
          (test-assert (not (application-input-controller-active-p (acp-input-controller session)))
                       "cancelled compaction releases the native controller")
          (test-assert (null (scripted-provider-input-snapshots provider))
                       "cancellation interrupts the blocked checkpoint request")
          (agentcomms:client-prompt client identifier (list (agentcomms:acp-text-content "Continue")))
          (test-assert (= 1 (length (scripted-provider-input-snapshots provider)))
                       "a later explicit prompt can use the session")
          (agentcomms:client-close-session client identifier)))
       :provider provider)))
  nil)
