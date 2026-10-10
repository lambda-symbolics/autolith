(in-package #:autolith)

;;;; -- ACP Completion Controller Tests --

(-> acp-completion-test--wait (function) null)
(defun acp-completion-test--wait (predicate)
  "Wait for a fixture's asynchronous state with a bounded failure deadline."
  (let ((deadline (+ (get-internal-real-time) (* 8 internal-time-units-per-second))))
    (loop until (funcall predicate)
          do (when (>= (get-internal-real-time) deadline)
               (error 'simple-error :format-control "ACP completion fixture timed out."))
             (sleep 0.01)))
  nil)

(-> test-acp-completion-continues-and-closes () null)
(defun test-acp-completion-continues-and-closes ()
  "Coalesce real detached jobs into one streamed turn and join the controller on close."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (application (acp-session-application session))
              (agent (application-agent application))
              (runtime (task-completion--runtime agent))
              (provider (application-provider application))
              (controller (acp-session-completion-thread session))
              (jobs nil))
         (agentcomms:client-prompt client identifier (list (agentcomms:acp-text-content "start")))
         (test-assert (task-completion-wakeup-connected-p agent) "ACP connects a completion wakeup")
         (acp-service--call-with-operation
          service session
          (lambda ()
            (push (job-completion-tests--start agent runtime) jobs)
            (push (job-completion-tests--start agent runtime) jobs)))
         (acp-completion-test--wait
          (lambda () (and (= 2 (length (scripted-provider-input-snapshots provider)))
                          (null (acp-session-prompt-thread session)))))
         (test-assert (every (lambda (job)
                              (task-completion--delivered-p
                               (application-conversation application)
                               (session-job-execution-identifier job))) jobs)
                      "both completion receipts precede the automatic request")
         (test-assert (equal '("start") (conversation-input-history (application-conversation application)))
                      "automatic completion records are not user input history")
         (test-assert (null (task-completion-pending agent)) "delivered notices cannot start another turn")
         (acp-completion-test--wait
          (lambda () (>= (count "agent_message_chunk" (acp-session-test-updates client)
                                :test #'equal :key (lambda (update) (gethash "sessionUpdate" update))) 2)))
         (test-assert (= 2 (length (scripted-provider-input-snapshots provider)))
                      "both completed jobs use one streamed continuation")
         (acp-completion-test--wait (lambda () (= 4 (length (acp-session-test-turn-events client)))))
         (acp-session-test-check-turns client '("prompt" "completion") '("end-turn" "end-turn"))
         (agentcomms:client-close-session client identifier)
         (test-assert (not (thread-alive-p controller)) "close joins the controller")
         (test-assert (not (task-completion-wakeup-connected-p agent)) "close disconnects completion wakeups")
         (test-assert (null (application-conversation-lease application)) "close releases conversation ownership")))
     :results (list (acp-session-test-text-result "started")
                    (acp-session-test-text-result "continued"))))
  nil)

(-> test-acp-completion-cancel-and-reuse () null)
(defun test-acp-completion-cancel-and-reuse ()
  "Cancel a provider-blocked automatic turn over ACP and admit a later explicit prompt."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                   :results (list (acp-session-test-text-result "resumed")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session (acp-service--session service identifier))
                (application (acp-session-application session))
                (agent (application-agent application)))
           (conversation-append-user-message (application-conversation application) "start")
           (job-completion-tests--start agent (task-completion--runtime agent))
           (acp-completion-test--wait (lambda () (acp-session-test-gated-entered-p provider)))
           (agentcomms:client-cancel client identifier)
           (acp-completion-test--wait (lambda () (and (acp-session-cancelled-p session)
                                                     (null (acp-session-prompt-thread session)))))
           (test-assert (null (scripted-provider-input-snapshots provider))
                        "cancellation interrupts the blocked automatic request")
           (acp-completion-wake session)
           (test-assert (null (acp-completion--cycle session)) "cancelled work cannot clear its own pause")
           (test-assert (eq ':end-turn (agentcomms:client-prompt
                                        client identifier (list (agentcomms:acp-text-content "resume"))))
                        "explicit reuse completes the turn")
           (test-assert (= 1 (length (scripted-provider-input-snapshots provider)))
                        "an explicit prompt reopens the cancelled session")
           (acp-session-test-check-turns client '("completion" "prompt") '("cancelled" "end-turn"))
           (agentcomms:client-close-session client identifier)))
       :provider provider)))
  nil)

(-> test-acp-completion-start-failure-cleans-session () null)
(defun test-acp-completion-start-failure-cleans-session ()
  "Release a newly published session when its completion controller cannot start."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let ((failed-session nil)
             (rejected-p nil))
         (test-call-with-function-replacements
          (list (list 'acp-completion-start
                      (lambda (session)
                        (setf failed-session session)
                        (error 'simple-error :format-control "Controller startup failed."))))
          (lambda ()
            (handler-case
                (agentcomms:client-new-session client (namestring root))
              (error () (setf rejected-p t)))))
         (test-assert rejected-p "failed startup returns a wire error")
         (test-assert (and failed-session (acp-session-closed-p failed-session))
                      "failed startup closes the session")
         (test-assert (zerop (hash-table-count (acp-service-sessions service)))
                      "failed startup removes the published session")
         (test-assert (null (application-conversation-lease
                             (acp-session-application failed-session)))
                      "failed startup releases its lease")
         (let ((identifier (agentcomms:client-new-session client (namestring root))))
           (agentcomms:client-close-session client identifier))))))
  nil)

(-> test-acp-completion-failure-pauses () null)
(defun test-acp-completion-failure-pauses ()
  "Pause automatic inference after failure even when another completion arrives."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (application (acp-session-application session))
              (agent (application-agent application))
              (attempts 0)
              (original (symbol-function 'agent-run-user-turn)))
         (conversation-append-user-message (application-conversation application) "start")
         (test-call-with-function-replacements
          (list (list 'agent-run-user-turn
                      (lambda (&rest arguments)
                        (if (getf (cddr arguments) :automatic-p)
                            (progn
                              (incf attempts)
                              (error 'simple-error :format-control "Automatic inference failed."))
                            (apply original arguments)))))
          (lambda ()
            (job-completion-tests--start agent (task-completion--runtime agent))
            (acp-completion-test--wait
             (lambda () (acp-session-completion-report session)))
            (job-completion-tests--start agent (task-completion--runtime agent))
            (acp-completion-test--wait (lambda () (task-completion-pending agent)))
            (test-assert (null (acp-completion--cycle session))
                         "a failed automatic turn pauses later notices")
            (test-assert (= 1 attempts) "automatic inference is not retried")
            (test-assert (null (acp-session-prompt-thread session))
                         "failed inference releases the primary turn")
            (agentcomms:client-close-session client identifier)))))))
  nil)
