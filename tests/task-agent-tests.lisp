(in-package #:autolith)

(-> test-task-tool-default-argument-types () null)
(defun test-task-tool-default-argument-types ()
  "Test that explicit JSON false and null never become omitted task defaults."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (task-augment-tool-registry (make-default-tool-registry)))
         (primary
          (task-tests--primary-agent configuration "task-default-types" registry))
         (conversation  (agent-conversation primary))
         (context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation conversation :registry registry :agent primary))
         (orchestrator
          (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
         (definition    (list))
         (job
          (task-tests--register-job orchestrator primary definition :name
                                    "default-types"))
         (child         (task-tests--child-viewer configuration job :registry registry))
         (waited-job
          (task-tests--register-job orchestrator child definition :name
                                    "wait-descendant"))
         (child-context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation (agent-conversation child) :registry registry
                         :agent child))
         (job-result
          (task-tests--terminal-result job :status ':success :output "already terminal")))
    (unwind-protect
        (progn
         (let ((helper                    nil)
               (helper-name               nil)
               (inline-runs               0)
               (inline-runs-before-helper nil))
           (let ((wait-result
                  (test-call-with-function-replacements
                   (list
                    (list 'make-thread
                          (lambda (function &key name)
                            (setf helper function
                                  helper-name name)
                            nil))
                    (list 'job-run-inline
                          (lambda (awaited-job)
                            (declare (ignore awaited-job))
                            (incf inline-runs)
                            t))
                    (list 'session-job-await
                          (lambda (awaited-job timeout-seconds)
                            (declare (ignore timeout-seconds))
                            (values (session-job-snapshot awaited-job) nil))))
                   (lambda ()
                     (let ((result
                            (tool-execute (tool-registry-find registry "job" "wait")
                                          child-context
                                          (json-object "id" (job-identifier waited-job)
                                                       "timeout-seconds" 1))))
                       (setf inline-runs-before-helper inline-runs)
                       (funcall helper)
                       result)))))
             (test-assert
              (and (tool-result-success-p wait-result) (functionp helper)
                   (stringp helper-name) (zerop inline-runs-before-helper)
                   (= inline-runs 1)
                   (not (getf (rest (tool-result-details wait-result)) :terminal-p)))
              "child job.wait preserves its timeout while help-joining separately")))
         (task-tests--publish-terminal job :completed job-result)
         (labels ((rejected-p (namespace name arguments)
                    "Return true when actual registry dispatch rejects ARGUMENTS."
                    (not
                     (tool-result-success-p
                      (tool-registry-execute-call registry
                                                  (json-object "namespace" namespace
                                                               "name" name "arguments"
                                                               arguments)
                                                  context)))))
           (dolist
               (case
                '(("job" "list" "{\"offset\":false}") ("job" "list" "{\"offset\":null}")
                  ("job" "list" "{\"limit\":false}") ("job" "list" "{\"limit\":null}")))
             (destructuring-bind
                 (namespace name arguments)
                 case
               (test-assert (rejected-p namespace name arguments)
                            (format nil
                                    "~A.~A rejects explicit non-integer default input ~A"
                                    namespace name arguments))))
           (dolist (value '("false" "null"))
             (test-assert
              (rejected-p "job" "wait"
               (format nil "{\"id\":~A,\"timeout-seconds\":~A}"
                       (json-encode (job-identifier job)) value))
              (format nil "job.wait rejects explicit timeout-seconds ~A" value)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-task-child-steering-mailbox () null)
(defun test-task-child-steering-mailbox ()
  "Test bounded child steering, terminal claims, and durable safe-boundary delivery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition    (list)))
    (labels ((fixture (identifier &key (state ':running))
               (task-tests--child-fixture configuration definition identifier :state
                                          state))

             (mark-running (job)
               (with-lock-held ((cl-jobpond::job--lock job))
                 (setf (job-state job) ':running))
               (task-job--set-progress-state job ':running)
               job)

             (steering-texts (entries)
               (mapcar
                (lambda (entry)
                  (user-message-input-text (agent-steering-input-content entry)))
                entries))

             (run-claim-race (index)
               (let* ((fixture
                       (fixture (format nil "steering-completion-race-~D" index)))
                      (job               (mark-running (getf fixture :job)))
                      (normal-result
                       (agent-test-result "normal-race"
                                          (list (agent-test-message "done"))
                                          :turn-completion ':end))
                      (gate-lock         (make-lock "Autolith steering race gate"))
                      (gate-condition    (make-condition-variable))
                      (ready-count       0)
                      (released-p        nil)
                      (enqueue-reason    nil)
                      (enqueue-condition nil)
                      (claim-result      nil)
                      (claim-condition   nil)
                      (enqueue-thread    nil)
                      (claim-thread      nil))
                 (labels ((await-release ()
                            (with-lock-held (gate-lock)
                              (incf ready-count)
                              (task--condition-broadcast gate-condition)
                              (loop until released-p
                                    unless (condition-wait gate-condition gate-lock
                                                           :timeout 2)
                                    do (error
                                        "Timed out waiting for the steering race release.")))))
                   (unwind-protect
                       (progn
                        (setf enqueue-thread
                                (make-thread
                                 (lambda ()
                                   (handler-case
                                    (progn
                                     (await-release)
                                     (multiple-value-bind (entry reason)
                                         (task-job-enqueue-steering job "race context")
                                       (declare (ignore entry))
                                       (setf enqueue-reason reason)))
                                    (condition (condition)
                                     (setf enqueue-condition condition))))
                                 :name "Autolith steering enqueue race")
                              claim-thread
                                (make-thread
                                 (lambda ()
                                   (handler-case
                                    (progn
                                     (await-release)
                                     (setf claim-result
                                             (agent-turn-complete-p (getf fixture :child)
                                                                    normal-result)))
                                    (condition (condition)
                                     (setf claim-condition condition))))
                                 :name "Autolith steering completion race"))
                        (with-lock-held (gate-lock)
                          (loop until (= ready-count 2)
                                unless (condition-wait gate-condition gate-lock :timeout
                                                       2)
                                do (error "Timed out synchronizing the steering race."))
                          (setf released-p t)
                          (task--condition-broadcast gate-condition))
                        (join-thread enqueue-thread)
                        (setf enqueue-thread nil)
                        (join-thread claim-thread)
                        (setf claim-thread nil)
                        (let ((enqueue-won-p (eq enqueue-reason ':accepted)))
                          (test-assert
                           (and (null enqueue-condition) (null claim-condition)
                                (not (eq enqueue-won-p claim-result))
                                (if enqueue-won-p
                                    (= (task-job-steering-pending-count job) 1)
                                    (and (eq enqueue-reason ':closed)
                                         (task-job-steering-closed-p job)
                                         (zerop (task-job-steering-pending-count job)))))
                           "enqueue and normal completion share one atomic winner")))
                     (with-lock-held (gate-lock)
                       (setf released-p t)
                       (task--condition-broadcast gate-condition))
                     (when enqueue-thread (join-thread enqueue-thread))
                     (when claim-thread (join-thread claim-thread))
                     (task-job-close-steering job))))))
      (unwind-protect
          (progn
           (let* ((fixture (fixture "steering-queued" :state ':queued))
                  (job     (getf fixture :job)))
             (multiple-value-bind (entry reason)
                 (task-job-enqueue-steering job "not yet")
               (test-assert
                (and (null entry) (eq reason ':not-running)
                     (zerop (task-job-steering-pending-count job)))
                "queued children reject steering without retaining it")))
           (let* ((fixture (fixture "steering-fifo"))
                  (job     (mark-running (getf fixture :job))))
             (multiple-value-bind (first first-reason)
                 (task-job-enqueue-steering job "first")
               (multiple-value-bind (second second-reason)
                   (task-job-enqueue-steering job "second")
                 (test-assert
                  (and (eq first-reason ':accepted) (eq second-reason ':accepted)
                       (not
                        (string= (agent-steering-input-identifier first)
                                 (agent-steering-input-identifier second)))
                       (= (getf (task-job-snapshot job) :pending-prompt-count) 2)
                       (= (getf (task-job-live-activity job) :pending-prompt-count) 2))
                  "running children expose accepted prompt counts")
                 (let ((entries (task-job-take-steering job)))
                   (test-assert
                    (and (equal (steering-texts entries) '("first" "second"))
                         (= (task-job-steering-pending-count job) 2)
                         (task-job-acknowledge-steering job
                                                        (agent-steering-input-identifier
                                                         first))
                         (= (task-job-steering-pending-count job) 1)
                         (not (task-job-acknowledge-steering job "missing"))
                         (task-job-acknowledge-steering job
                                                        (agent-steering-input-identifier
                                                         second))
                         (zerop (task-job-steering-pending-count job)))
                    "child steering drains FIFO and remains retained until acknowledgment")))
               (test-assert (task-job--claim-normal-completion job)
                            "an empty mailbox permits normal completion")
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "too late")
                 (test-assert (and (null entry) (eq reason ':closed))
                              "normal completion closes later steering admission")))
             (let* ((fixture (fixture "steering-bounds"))
                    (job     (mark-running (getf fixture :job))))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "")
                 (test-assert (and (null entry) (eq reason ':empty))
                              "child steering rejects empty content"))
               (let ((*task-steering-maximum-characters* 3))
                 (multiple-value-bind (entry reason)
                     (task-job-enqueue-steering job "four")
                   (test-assert (and (null entry) (eq reason ':content-too-large))
                                "child steering enforces its per-message bound")))
               (let ((*task-steering-maximum-items*            3)
                     (*task-steering-maximum-characters*       4)
                     (*task-steering-maximum-total-characters* 5))
                 (multiple-value-bind (accepted accepted-reason)
                     (task-job-enqueue-steering job "1234")
                   (task-job-take-steering job)
                   (multiple-value-bind (entry reason)
                       (task-job-enqueue-steering job "12")
                     (test-assert
                      (and accepted (eq accepted-reason ':accepted) (null entry)
                           (eq reason ':full) (= (task-job-steering-pending-count job) 1)
                           (task-job-acknowledge-steering job
                                                          (agent-steering-input-identifier
                                                           accepted))
                           (zerop (task-job-steering-pending-count job)))
                      "character rejection retains accepted in-flight steering")))
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-count-bound"))
                    (job     (mark-running (getf fixture :job))))
               (let ((*task-steering-maximum-items*            2)
                     (*task-steering-maximum-characters*       4)
                     (*task-steering-maximum-total-characters* 20))
                 (multiple-value-bind (first first-reason)
                     (task-job-enqueue-steering job "one")
                   (task-job-take-steering job)
                   (multiple-value-bind (second second-reason)
                       (task-job-enqueue-steering job "two")
                     (multiple-value-bind (entry reason)
                         (task-job-enqueue-steering job "more")
                       (test-assert
                        (and first (eq first-reason ':accepted) second
                             (eq second-reason ':accepted) (null entry)
                             (eq reason ':full)
                             (= (task-job-steering-pending-count job) 2)
                             (equal (steering-texts (task-job-take-steering job))
                                    '("two"))
                             (task-job-acknowledge-steering job
                                                            (agent-steering-input-identifier
                                                             first))
                             (task-job-acknowledge-steering job
                                                            (agent-steering-input-identifier
                                                             second))
                             (zerop (task-job-steering-pending-count job)))
                        "count rejection retains queued and in-flight steering")))))
               (task-job-close-steering job))
             (let* ((fixture (fixture "steering-normal-stop"))
                    (job     (mark-running (getf fixture :job)))
                    (child   (getf fixture :child))
                    (result
                     (agent-test-result "normal-stop" (list (agent-test-message "done"))
                                        :turn-completion ':end)))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "continue instead")
                 (declare (ignore reason))
                 (test-assert (not (agent-turn-complete-p child result))
                              "pending steering prevents a normal provider stop")
                 (task-job-take-steering job)
                 (task-job-acknowledge-steering job
                                                (agent-steering-input-identifier entry))
                 (test-assert
                  (and (agent-turn-complete-p child result)
                       (task-job-steering-closed-p job))
                  "normal completion atomically claims an empty mailbox")))
             (let* ((fixture      (fixture "steering-response-promotion"))
                    (job          (mark-running (getf fixture :job)))
                    (orchestrator (task-job-orchestrator job))
                    (events       nil)
                    (listener
                     (lambda (channel payload)
                       (when (eq channel ':task-subagent-verbal-response)
                         (push (copy-tree payload) events)))))
               (unwind-protect
                   (progn
                    (task-orchestrator-add-listener orchestrator listener)
                    (multiple-value-bind (entry reason)
                        (task-job-enqueue-steering job "background context")
                      (test-assert
                       (and entry (eq reason ':accepted)
                            (zerop (task-job-response-promotion-pending-count job)))
                       "ordinary child steering creates no response promotion token")
                      (task-job-take-steering job)
                      (task-job-acknowledge-steering job
                                                     (agent-steering-input-identifier
                                                      entry)))
                    (multiple-value-bind (first first-reason)
                        (task-job-enqueue-steering job "first promoted prompt"
                                                   :promote-response-p t)
                      (multiple-value-bind (second second-reason)
                          (task-job-enqueue-steering job "second promoted prompt"
                                                     :promote-response-p t)
                        (let ((first-time  (get-universal-time))
                              (second-time (1+ (get-universal-time))))
                          (test-assert
                           (and (eq first-reason ':accepted)
                                (eq second-reason ':accepted)
                                (= (task-job-response-promotion-pending-count job) 2))
                           "promoted steering reserves one FIFO token per accepted prompt")
                          (task-job-note-agent-status job ':assistant-response-persisted
                                                      (list :text "pre-steering answer"
                                                            :time first-time))
                          (test-assert
                           (and (null events)
                                (= (task-job-response-promotion-pending-count job) 2))
                           "a response already in flight does not consume unacknowledged promotions")
                          (task-job-take-steering job)
                          (test-assert
                           (and
                            (task-job-acknowledge-steering job
                                                           (agent-steering-input-identifier
                                                            first))
                            (task-job-acknowledge-steering job
                                                           (agent-steering-input-identifier
                                                            second)))
                           "durable steering acknowledgments activate response promotions")
                          (task-job-note-agent-status job ':provider-request-started
                                                      (list :request-number 1))
                          (task-job-note-agent-status job ':tool-call-completed
                                                      (list :tool "resource.read"))
                          (task-job-note-agent-status job ':assistant-response-persisted
                                                      (list :text "   " :time
                                                            first-time))
                          (test-assert
                           (and (null events)
                                (= (task-job-response-promotion-pending-count job) 2))
                           "tools, status updates, and blank text do not consume promotion tokens")
                          (task-job-note-agent-status job ':assistant-response-persisted
                                                      (list :text "first answer" :time
                                                            first-time))
                          (test-assert
                           (and (= (length events) 1)
                                (= (task-job-response-promotion-pending-count job) 1))
                           "one durable verbal response consumes exactly one token and emits once")
                          (task-job-note-agent-status job ':assistant-response-persisted
                                                      (list :text "second answer" :time
                                                            second-time))
                          (task-job-note-agent-status job ':assistant-response-persisted
                                                      (list :text "unsteered answer"
                                                            :time (1+ second-time)))
                          (let* ((ordered-events (nreverse events))
                                 (first-event    (first ordered-events))
                                 (second-event   (second ordered-events)))
                            (test-assert
                             (and (= (length ordered-events) 2)
                                  (zerop (task-job-response-promotion-pending-count job))
                                  (string= (getf first-event :id)
                                           (session-job-identifier job))
                                  (string= (getf first-event :execution-id)
                                           (task-job-execution-identifier job))
                                  (string= (getf first-event :child-name)
                                           (task-job-display-name job))
                                  (string= (getf first-event :steering-id)
                                           (agent-steering-input-identifier first))
                                  (string= (getf first-event :text) "first answer")
                                  (= (getf first-event :time) first-time)
                                  (string= (getf second-event :steering-id)
                                           (agent-steering-input-identifier second))
                                  (string= (getf second-event :text) "second answer")
                                  (= (getf second-event :time) second-time))
                             "promoted responses preserve FIFO steering and portable child identity"))))))
                 (task-orchestrator-remove-listener orchestrator listener)
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-response-bound"))
                    (job     (mark-running (getf fixture :job))))
               (unwind-protect
                   (let ((*task-response-promotion-maximum-items* 1))
                     (multiple-value-bind (first first-reason)
                         (task-job-enqueue-steering job "reserve one response"
                                                    :promote-response-p t)
                       (test-assert (and first (eq first-reason ':accepted))
                                    "the response promotion bound accepts its first token")
                       (task-job-take-steering job)
                       (task-job-acknowledge-steering job
                                                      (agent-steering-input-identifier
                                                       first))
                       (multiple-value-bind (second second-reason)
                           (task-job-enqueue-steering job "reserve another response"
                                                      :promote-response-p t)
                         (test-assert
                          (and (null second) (eq second-reason ':full)
                               (= (task-job-response-promotion-pending-count job) 1)
                               (zerop (task-job-steering-pending-count job)))
                          "the response token bound rejects promoted steering atomically"))
                       (multiple-value-bind (ordinary ordinary-reason)
                           (task-job-enqueue-steering job "ordinary still fits")
                         (test-assert
                          (and ordinary (eq ordinary-reason ':accepted)
                               (= (task-job-response-promotion-pending-count job) 1)
                               (= (task-job-close-steering job) 1)
                               (zerop (task-job-response-promotion-pending-count job)))
                          "ordinary steering bypasses the token bound and close clears tokens once"))))
                 (task-job-close-steering job)))
             (let* ((fixture        (fixture "steering-response-race"))
                    (job            (mark-running (getf fixture :job)))
                    (orchestrator   (task-job-orchestrator job))
                    (gate-lock      (make-lock "Autolith response promotion race gate"))
                    (gate-condition (make-condition-variable))
                    (event-lock     (make-lock "Autolith response promotion race events"))
                    (ready-count    0)
                    (released-p     nil)
                    (results        (make-array 8 :initial-element nil))
                    (events         nil)
                    (threads        nil)
                    (listener
                     (lambda (channel payload)
                       (when (eq channel ':task-subagent-verbal-response)
                         (with-lock-held (event-lock)
                           (push (copy-tree payload) events))))))
               (labels ((await-release ()
                          (with-lock-held (gate-lock)
                            (incf ready-count)
                            (task--condition-broadcast gate-condition)
                            (loop until released-p
                                  unless (condition-wait gate-condition gate-lock
                                                         :timeout 2)
                                  do (error
                                      "Timed out waiting for the response race release.")))))
                 (unwind-protect
                     (progn
                      (task-orchestrator-add-listener orchestrator listener)
                      (multiple-value-bind (entry reason)
                          (task-job-enqueue-steering job "race response"
                                                     :promote-response-p t)
                        (test-assert (and entry (eq reason ':accepted))
                                     "the response race reserves one promotion token")
                        (task-job-take-steering job)
                        (task-job-acknowledge-steering job
                                                       (agent-steering-input-identifier
                                                        entry))
                        (dotimes (index (length results))
                          (let ((thread-index index))
                            (push
                             (make-thread
                              (lambda ()
                                (await-release)
                                (setf (aref results thread-index)
                                      (task-job-note-verbal-response job
                                                                     (format nil
                                                                             "race answer ~D"
                                                                             thread-index)
                                                                     (get-universal-time))))
                              :name "Autolith response promotion race")
                             threads)))
                        (with-lock-held (gate-lock)
                          (loop until (= ready-count (length results))
                                unless (condition-wait gate-condition gate-lock :timeout
                                                       2)
                                do (error "Timed out synchronizing the response race."))
                          (setf released-p t)
                          (task--condition-broadcast gate-condition))
                        (dolist (thread threads) (join-thread thread))
                        (setf threads nil)
                        (let* ((winner-index (position t results))
                               (observed-events
                                (with-lock-held (event-lock)
                                  (copy-list events)))
                               (event        (first observed-events)))
                          (test-assert
                           (and (= (count t results) 1) winner-index
                                (= (length observed-events) 1)
                                (zerop (task-job-response-promotion-pending-count job))
                                (string= (getf event :steering-id)
                                         (agent-steering-input-identifier entry))
                                (string= (getf event :text)
                                         (format nil "race answer ~D" winner-index)))
                           "concurrent verbal notifications consume one token exactly once"))))
                   (with-lock-held (gate-lock)
                     (setf released-p t)
                     (task--condition-broadcast gate-condition))
                   (dolist (thread threads) (ignore-errors (join-thread thread)))
                   (task-orchestrator-remove-listener orchestrator listener)
                   (task-job-close-steering job))))
             (dotimes (index 12) (run-claim-race index))
             (let* ((fixture      (fixture "steering-tool-free"))
                    (job          (mark-running (getf fixture :job)))
                    (orchestrator (task-job-orchestrator job))
                    (base-child   (getf fixture :child))
                    (conversation (getf fixture :conversation))
                    (reasoning-item
                     (json-object "type" "reasoning" "summary"
                                  (json-array
                                   (json-object "type" "summary_text" "text"
                                                "before context"))))
                    (provider
                     (make-instance 'scripted-provider :results
                                    (list
                                     (agent-test-result "before-steering"
                                                        (list reasoning-item)
                                                        :turn-completion ':end)
                                     (agent-test-result "after-steering"
                                                        (list
                                                         (agent-test-message
                                                          "after context"))
                                                        :turn-completion ':end))))
                    (child
                     (make-instance 'task-child-agent :configuration
                                    (agent-configuration base-child) :provider provider
                                    :conversation conversation :tool-registry
                                    (agent-tool-registry base-child) :worker nil
                                    :identity (task-child-agent-identity base-child)
                                    :depth    (task-child-agent-depth base-child)
                                    :orchestrator
                                    (task-child-agent-orchestrator base-child) :job job))
                    (events       nil)
                    (listener
                     (lambda (channel payload)
                       (when (eq channel ':task-subagent-verbal-response)
                         (push (copy-tree payload) events))))
                    (observer
                     (callback-agent-observer-create :status-callback
                                                     (lambda (status details)
                                                       (task-job-note-agent-status job
                                                                                   status
                                                                                   details))
                                                     :steering-callback
                                                     (lambda ()
                                                       (task-job-take-steering job))
                                                     :steering-persisted-callback
                                                     (lambda (identifier)
                                                       (task-job-acknowledge-steering job
                                                                                      identifier)))))
               (unwind-protect
                   (progn
                    (task-orchestrator-add-listener orchestrator listener)
                    (multiple-value-bind (steering reason)
                        (task-job-enqueue-steering job "tool-free steering"
                                                   :promote-response-p t)
                      (test-assert (and steering (eq reason ':accepted))
                                   "the tool-free child accepts promoted steering")
                      (let ((result
                             (agent-run-user-turn child "initial child input" :observer
                                                  observer)))
                        (test-assert
                         (and
                          (string= (provider-result-response-id result) "after-steering")
                          (equal (nreverse (scripted-provider-input-counts provider))
                                 '(1 3))
                          (zerop (task-job-steering-pending-count job))
                          (zerop (task-job-response-promotion-pending-count job)))
                         "tool-free steering is durable before its follow-up response"))
                      (let ((event (first events)))
                        (test-assert
                         (and (= (length events) 1)
                              (string= (getf event :id) (session-job-identifier job))
                              (string= (getf event :steering-id)
                                       (agent-steering-input-identifier steering))
                              (string= (getf event :text) "after context")
                              (typep (getf event :time) 'timestamp))
                         "the durable agent boundary emits one steered verbal response after reasoning")))
                    (let ((user-records
                           (loop for record in (rest
                                                (conversation--read-records
                                                 (conversation-pathname conversation)))
                                 when (and (eq (first record) ':message)
                                           (eq (getf (rest record) :role) ':user))
                                 collect record)))
                      (test-assert
                       (equal
                        (mapcar (lambda (record) (getf (rest record) :content))
                                user-records)
                        '("initial child input" "tool-free steering"))
                       "tool-free child steering is durable ordinary user input")))
                 (task-orchestrator-remove-listener orchestrator listener)
                 (task-job-close-steering job)))
             (let* ((fixture (fixture "steering-cancellation"))
                    (job     (mark-running (getf fixture :job))))
               (multiple-value-bind (entry reason)
                   (task-job-enqueue-steering job "accepted before cancellation")
                 (test-assert (and entry (eq reason ':accepted))
                              "running child accepts steering before cancellation")
                 (test-assert (job-cancel job :reason ':steering-test)
                              "running child cancellation records its first reason")
                 (multiple-value-bind (late-entry late-reason)
                     (task-job-enqueue-steering job "after cancellation")
                   (test-assert (and (null late-entry) (eq late-reason ':closing))
                                "cancelled child rejects later steering before publication"))
                 (test-assert
                  (task-tests--publish-terminal job ':completed
                                                (list :status ':success :output
                                                      "must be discarded"))
                  "cancelled child publishes one terminal record")
                 (let* ((final-result (job-result job))
                        (artifact
                         (task-tests--read-exact-native-value
                          (uiop/stream:read-file-string
                           (getf final-result :output-path)))))
                   (test-assert
                    (and (eq (job-state job) ':aborted)
                         (= (getf final-result :undelivered-prompt-count) 1)
                         (= (getf artifact :undelivered-prompt-count) 1)
                         (zerop (task-job-steering-pending-count job))
                         (task-job-steering-closed-p job))
                    "cancellation publication records accepted undelivered steering"))))
             (let* ((fixture (fixture "steering-cancelled-claims"))
                    (job     (mark-running (getf fixture :job)))
                    (child   (getf fixture :child))
                    (normal-result
                     (agent-test-result "cancelled-normal-stop"
                                        (list (agent-test-message "done"))
                                        :turn-completion ':end)))
               (test-assert (job-cancel job :reason ':steering-claim-test)
                            "cancellation precedes the terminal claim checks")
               (test-assert
                (and (not (agent-turn-complete-p child normal-result))
                     (not (task-job-steering-closed-p job)))
                "cancelled child rejects normal completion without closing its mailbox")
               (test-assert (task-tests--publish-terminal job ':completed nil)
                            "cancelled terminal-claim fixture publishes its aborted result"))
             (let* ((fixture               (fixture "steering-publication-claim"))
                    (job                   (mark-running (getf fixture :job)))
                    (child                 (getf fixture :child))
                    (normal-result
                     (agent-test-result "publication-normal-stop"
                                        (list (agent-test-message "done"))
                                        :turn-completion ':end))
                    (original-hook         (cl-jobpond:job-terminal-result-function job))
                    (gate-lock             (make-lock "Autolith steering publication gate"))
                    (gate-condition        (make-condition-variable))
                    (entered-p             nil)
                    (released-p            nil)
                    (publication-result    nil)
                    (publication-condition nil)
                    (publication-thread    nil))
               (unwind-protect
                   (progn
                    (setf (slot-value job 'cl-jobpond::terminal-result-function)
                          (lambda (hook-job state result report)
                            (with-lock-held (gate-lock)
                              (setf entered-p t)
                              (task--condition-broadcast gate-condition)
                              (loop until released-p
                                    unless (condition-wait gate-condition gate-lock
                                                           :timeout 2)
                                    do (error
                                        "Timed out holding the publication claim.")))
                            (funcall original-hook hook-job state result report)))
                    (setf publication-thread
                            (make-thread
                             (lambda ()
                               (handler-case
                                (setf publication-result
                                        (task-tests--publish-terminal job ':failed
                                                                      (list :id
                                                                            (job-identifier
                                                                             job)
                                                                            :name
                                                                            (task-job-display-name
                                                                             job)
                                                                            :assignment
                                                                            "Exercise publication admission."
                                                                            :status
                                                                            ':failed
                                                                            :error
                                                                            "stopped")))
                                (condition (condition)
                                 (setf publication-condition condition))))
                             :name "Autolith steering publication claim"))
                    (with-lock-held (gate-lock)
                      (loop until entered-p
                            unless (condition-wait gate-condition gate-lock :timeout 2)
                            do (error "Timed out waiting for the publication claim.")))
                    (multiple-value-bind (entry reason)
                        (task-job-enqueue-steering job "after claim")
                      (test-assert (and (null entry) (eq reason ':closing))
                                   "an active publication claim rejects steering"))
                    (test-assert
                     (and (not (agent-turn-complete-p child normal-result))
                          (not (task-job-steering-closed-p job)))
                     "an active publication claim rejects normal completion")
                    (with-lock-held (gate-lock)
                      (setf released-p t)
                      (task--condition-broadcast gate-condition))
                    (join-thread publication-thread)
                    (setf publication-thread nil)
                    (test-assert
                     (and publication-result (null publication-condition)
                          (eq (job-state job) ':failed)
                          (zerop (task-job-steering-pending-count job)))
                     "publication completes after rejecting post-claim steering"))
                 (with-lock-held (gate-lock)
                   (setf released-p t)
                   (task--condition-broadcast gate-condition))
                 (when publication-thread (join-thread publication-thread))
                 (unless (job-terminal-p job)
                   (setf (slot-value job 'cl-jobpond::terminal-result-function)
                         original-hook))))
             (let* ((fixture (fixture "steering-terminal"))
                    (job     (mark-running (getf fixture :job))))
               (task-job-enqueue-steering job "undelivered one")
               (task-job-enqueue-steering job "undelivered two")
               (task-job-take-steering job)
               (test-assert (= (task-job-steering-pending-count job) 2)
                            "drained steering remains retained until durable acknowledgment")
               (multiple-value-bind (final-result final-report final-state)
                   (task-job--terminal-record job :state ':failed :result
                                              (list :id (job-identifier job) :name
                                                    (task-job-display-name job)
                                                    :assignment
                                                    "Exercise terminal retention."
                                                    :status ':failed :error "stopped")
                                              :report nil)
                 (declare (ignore final-report))
                 (let ((artifact
                        (task-tests--read-exact-native-value
                         (uiop/stream:read-file-string
                          (getf final-result :output-path)))))
                   (test-assert
                    (and (eq final-state ':failed)
                         (= (getf final-result :undelivered-prompt-count) 2)
                         (= (getf artifact :undelivered-prompt-count) 2)
                         (zerop (task-job-steering-pending-count job))
                         (task-job-steering-closed-p job))
                    "terminal publication records queued and in-flight prompts")))))
           (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                           ':ignore)))
      nil)))

(-> test-task-child-messaging () null)
(defun test-task-child-messaging ()
  "Test child notes to the parent."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition    (list))
         (fixture
          (task-tests--child-fixture configuration definition "messaging-child-1"))
         (job           (getf fixture :job))
         (parent        (task-job-parent-agent job)))
    (unwind-protect
        (progn
         (let ((result
                (task-note-post (conversation-identifier (agent-conversation parent))
                                "messaging-child" "halfway done")))
           (test-assert (eq result ':accepted) "task notes queue an interim child note"))
         (let* ((delivery
                 (context-resolve-request configuration (agent-conversation parent) #()))
                (contribution
                 (find "task-notes" (context-delivery-contributions delivery) :test
                       #'string= :key #'context-contribution-identifier)))
           (test-assert
            (and contribution
                 (search "halfway done" (context-contribution-evidence contribution))
                 (search "messaging-child" (context-contribution-evidence contribution)))
            "queued notes deliver behind the parent conversation"))
         (test-assert
          (null
           (find "task-notes"
                 (context-delivery-contributions
                  (context-resolve-request configuration (agent-conversation parent)
                                           #()))
                 :test #'string= :key #'context-contribution-identifier))
          "delivered notes drain and do not repeat")
         (let ((*task-note-maximum-pending* 0))
           (test-assert (eq (task-note-post "any-parent" "child" "text") ':full)
                        "a full note queue refuses further notes")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)
