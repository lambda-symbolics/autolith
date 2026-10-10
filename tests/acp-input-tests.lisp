(in-package #:autolith)

;;;; -- ACP Native Input Tests --

(-> acp-input-test--session (function &key (:manual-p boolean) (:tool-round-p boolean)) null)
(defun acp-input-test--session (function &key (manual-p t) tool-round-p)
  "Run FUNCTION with a leased scripted session, optionally driving completion manually."
  (with-test-configuration (configuration root)
    (flet ((run ()
             "Construct and close the session inside the selected controller fixture."
             (acp-session-test--call-with-client
              configuration
              (lambda (service client)
                (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                       (session (acp-service--session service identifier)))
                  (funcall function session client)
                  (agentcomms:client-close-session client identifier)))
              :results (append
                        (when tool-round-p
                          (list (agent-test-result
                                 "tool-boundary"
                                 (list (agent-test-call :call-id "boundary-read"
                                                        :namespace "resource" :name "read"
                                                        :arguments "{\"uri\":\"workspace:.\"}")))))
                        (loop repeat 8 collect (acp-session-test-text-result "done"))))))
      (if manual-p
          (test-call-with-function-replacements
           (list (list 'acp-completion-start (lambda (session) (declare (ignore session)) nil)))
           #'run)
          (run))))
  nil)

(-> acp-input-test--submit (acp-session string t) hash-table)
(defun acp-input-test--submit (session method text)
  "Submit input through the typed extension boundary."
  (agentcomms:agent-extension-request
   (acp-session-service session) method
   (agentcomms:json-object "sessionId" (acp-session-identifier session) "text" text)))

(-> test-acp-input-extension-rejects-blank () null)
(defun test-acp-input-extension-rejects-blank ()
  "Reject empty and whitespace-only input before native admission."
  (acp-input-test--session
   (lambda (session client)
     (declare (ignore client))
     (dolist (text (list "" (format nil " ~C~%" #\Tab)))
       (test-assert (equal "condition" (gethash "outcome"
                                              (acp-input-test--submit session "_autolith/steer" text)))
                    "blank input returns a typed condition"))
     (test-assert (not (acp-input--pending-p session)) "invalid input is not queued")))
  nil)

(-> test-acp-input-extension-queue-fifo () null)
(defun test-acp-input-extension-queue-fifo ()
  "Run two queued prompts in order and acknowledge them after durable append."
  (acp-input-test--session
   (lambda (session client)
     (dolist (text '("first" "second"))
       (let ((result (acp-input-test--submit session "_autolith/queue" text)))
         (test-assert (equal "ok" (gethash "outcome" result)) "queue admission succeeds")
         (test-assert (equal "queued" (gethash "delivery" (gethash "value" result)))
                      "follow-up delivery is explicit")))
     (test-assert (zerop (acp-completion--cycle session)) "first follow-up runs")
     (test-assert (zerop (acp-completion--cycle session)) "second follow-up runs")
     (test-assert (equal '("first" "second")
                         (conversation-input-history
                          (application-conversation (acp-session-application session))))
                  "queued prompts enter durable user history in FIFO order")
     (test-assert (not (acp-input--pending-p session)) "acknowledged prompts leave pending storage")
     (acp-completion-test--wait
      (lambda () (= 2 (count "user_message_chunk" (acp-session-test-updates client)
                             :key (lambda (update) (gethash "sessionUpdate" update)) :test #'equal))))))
  nil)

(-> test-acp-input-extension-steers-active-turn () null)
(defun test-acp-input-extension-steers-active-turn ()
  "Apply steering and a native command at a real provider-loop safe boundary."
  (acp-input-test--session
   (lambda (session client)
     (let ((original (symbol-function 'agent-run-user-turn))
           (application (acp-session-application session)))
       (test-call-with-function-replacements
        (list
         (list 'agent-run-user-turn
               (lambda (agent text &rest arguments)
                 (when (stringp text)
                 (let ((result (acp-input-test--submit session "_autolith/steer" "steered")))
                   (test-assert (equal "steering" (gethash "delivery" (gethash "value" result)))
                                "running input uses native steering"))
                 (let ((result (agentcomms:agent-extension-request
                                (acp-session-service session) "_autolith/invoke"
                                (agentcomms:json-object
                                 "sessionId" (acp-session-identifier session)
                                 "operation" "hurry-up"
                                 "arguments" (agentcomms:json-object "input" "on")))))
                   (test-assert (equal "ok" (gethash "outcome" result))
                                "boundary-applying commands are admitted during a turn")
                   (test-assert (not (application-hurry-up-p application))
                                "the command does not mutate active work before its safe boundary")))
                 (apply original agent text arguments))))
        (lambda () (acp-session-run-turn session "start")))
       (test-assert (application-hurry-up-p application) "the native safe boundary applies hurry-up")
       (test-assert (equal '("start" "steered")
                           (conversation-input-history (application-conversation application)))
                    "steering is durably recorded as user input")
       (test-assert (not (acp-input--pending-p session)) "acknowledged steering is not repeated")
       (acp-completion-test--wait
        (lambda () (= 1 (count "user_message_chunk" (acp-session-test-updates client)
                               :key (lambda (update) (gethash "sessionUpdate" update)) :test #'equal))))))
   :tool-round-p t)
  nil)

(-> test-acp-input-extension-late-steer-promotes-before-fifo () null)
(defun test-acp-input-extension-late-steer-promotes-before-fifo ()
  "Run late steering before earlier follow-ups without a parallel input queue."
  (acp-input-test--session
   (lambda (session client)
     (declare (ignore client))
     (acp-input-test--submit session "_autolith/queue" "old")
     (acp-input-test--submit session "_autolith/steer" "late")
     (acp-completion--cycle session)
     (acp-completion--cycle session)
     (test-assert (equal '("late" "old")
                         (conversation-input-history
                          (application-conversation (acp-session-application session))))
                  "late steering precedes the FIFO")))
  nil)

(-> test-acp-input-extension-length-and-shape-validation () null)
(defun test-acp-input-extension-length-and-shape-validation ()
  "Reject oversized and non-text prompts through typed extension outcomes."
  (acp-input-test--session
   (lambda (session client)
     (declare (ignore client))
     (dolist (text (list 42 (make-string 262145 :initial-element #\x)))
       (test-assert (equal "condition" (gethash "outcome"
                                              (acp-input-test--submit session "_autolith/queue" text)))
                    "invalid input returns a condition"))
     (test-assert (not (acp-input--pending-p session)) "invalid input consumes no pending slot")))
  nil)

(-> test-acp-input-failure-and-cold-recovery () null)
(defun test-acp-input-failure-and-cold-recovery ()
  "Retain pending input across busy admission, pre-append failure, close and reload."
  (acp-input-test--session
   (lambda (session client)
     (let* ((service (acp-session-service session))
            (identifier (acp-session-identifier session))
            (application (acp-session-application session))
            (directory (namestring (config :working-directory (application-configuration application)))))
       (acp-input-test--submit session "_autolith/queue" "retained")
       (acp-service--call-with-operation
        service (gensym "OTHER-OWNER")
        (lambda ()
          (test-assert (handler-case (progn (acp-completion--cycle session) nil)
                         (acp-operation-busy () t))
                       "busy admission does not consume queued input")))
       (test-assert (acp-input--pending-p session) "busy input is available after release")
       (test-call-with-function-replacements
        (list (list 'agent-run-user-turn
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (error 'configuration-error :message "Scripted pre-append failure."))))
        (lambda ()
          (test-assert (handler-case (progn (acp-completion--cycle session) nil)
                         (configuration-error () t))
                       "a pre-append failure is reported")))
       (test-assert (null (acp-session-prompt-thread session)) "failure releases the prompt owner")
       (test-assert (null (acp-completion--cycle session)) "failure pauses automatic retry")
       (agentcomms:client-close-session client identifier)
       (agentcomms:client-load-session client identifier directory)
       (let ((restored (acp-service--session service identifier)))
         (test-assert (acp-input--pending-p restored) "reload restores the accepted prompt")
         (acp-completion--cycle restored)
         (test-assert (equal '("retained")
                             (conversation-input-history
                              (application-conversation (acp-session-application restored))))
                      "recovery executes pending input exactly once")))))
  nil)

(-> test-acp-input-wire-cancel-and-resume () null)
(defun test-acp-input-wire-cancel-and-resume ()
  "Pause live follow-ups on Stop and resume them through a later wire submission."
  (with-test-configuration (configuration root)
    (let ((provider (make-instance 'acp-session-test-gated-provider
                                   :configuration configuration
                                   :results (loop repeat 4 collect (acp-session-test-text-result "done")))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session (acp-service--session service identifier))
                (outcome nil)
                (thread (make-thread
                         (lambda ()
                           (setf outcome
                                 (handler-case
                                     (agentcomms:client-prompt
                                      client identifier (list (agentcomms:acp-text-content "start")))
                                   (agentcomms:acp-error () ':cancelled)))))))
           (unwind-protect
                (progn
                  (acp-completion-test--wait (lambda () (acp-session-test-gated-entered-p provider)))
                  (dolist (text '("first" "second"))
                    (let ((result (agentcomms:client-agent-request
                                   client "_autolith/queue"
                                   (agentcomms:json-object "sessionId" identifier "text" text))))
                      (test-assert (equal "ok" (gethash "outcome" result))
                                   "wire follow-up is accepted during the blocked prompt")))
                  (agentcomms:client-cancel client identifier)
                  (acp-completion-test--wait
                   (lambda () (and (acp-session-cancelled-p session)
                                   (null (acp-session-prompt-thread session)))))
                  (join-thread thread)
                  (test-assert (eq outcome ':cancelled) "Stop cancels the running wire prompt")
                  (test-assert (null (acp-completion--cycle session)) "Stop holds queued prompts")
                  (agentcomms:client-agent-request
                   client "_autolith/steer"
                   (agentcomms:json-object "sessionId" identifier "text" "resume"))
                  (acp-completion-test--wait
                   (lambda () (and (= 3 (length (scripted-provider-input-snapshots provider)))
                                   (null (acp-session-prompt-thread session)))))
                  (test-assert (equal '("start" "resume" "first" "second")
                                      (conversation-input-history
                                       (application-conversation (acp-session-application session))))
                               "explicit late steering resumes ordered durable follow-ups")
                  (agentcomms:client-close-session client identifier))
             (acp-session-test-gated-release provider)
             (when (thread-alive-p thread)
               (acp-session-cancel session)
               (join-thread thread)))))
       :provider provider)))
  nil)
