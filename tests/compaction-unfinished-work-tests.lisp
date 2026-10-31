(in-package #:autolith)

;;;; -- Unfinished Work Across Compaction --

(defclass compaction-tests-provider (native-scripted-provider)
  ((arrival
    :initarg       :arrival
    :initform      nil
    :reader        compaction-tests-provider-arrival
    :documentation "Optional callback appending authoritative work during a request.")
   (stage
    :initarg       :stage
    :initform      ':summary
    :reader        compaction-tests-provider-stage
    :documentation "The request boundary at which ARRIVAL runs."))
  (:documentation "Scripted native or portable compaction with deterministic concurrent arrivals."))

(defmethod provider-native-compact-conversation :around
    ((provider compaction-tests-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Append an arrival after the detached native input has been observed."
  (declare (ignore tool-namespaces event-callback))
  (prog1 (call-next-method)
    (when (and (eq (compaction-tests-provider-stage provider) ':native)
               (compaction-tests-provider-arrival provider))
      (funcall (compaction-tests-provider-arrival provider)))))

(defmethod provider-stream-turn :around
    ((provider compaction-tests-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Append an arrival after recording the summarization request snapshot."
  (declare (ignore tool-namespaces event-callback goal-context))
  (prog1 (call-next-method)
    (when (and compaction-p
               (eq (compaction-tests-provider-stage provider) ':summary)
               (compaction-tests-provider-arrival provider))
      (funcall (compaction-tests-provider-arrival provider)))))

(-> compaction-tests--provider (&key (:configuration configuration) (:native-p boolean) (:stage keyword)
                                    (:arrival (option function))) compaction-tests-provider)
(defun compaction-tests--provider (&key configuration native-p (stage ':summary) arrival)
  "Return one native/portable compaction followed by a usable portable summary."
  (make-instance
   'compaction-tests-provider :configuration configuration :stage stage :arrival arrival
   :native-items (list (when native-p
                         (json-object "type" "compaction" "encrypted_content" "private-checkpoint")))
   :results      (list (agent-test-result
                        "unfinished-summary"
                        (list (agent-test-compaction-message))
                        :turn-completion ':end))))

(-> compaction-tests--call (string) json-object)
(defun compaction-tests--call (identifier)
  "Return a provider function call whose side effect must never be retried."
  (json-object "type" "function_call" "call_id" identifier "namespace" "test"
               "name" "effect" "arguments" "{}"))

(-> compaction-tests--outputs (conversation string) list)
(defun compaction-tests--outputs (conversation identifier)
  "Return correlated tool outputs from CONVERSATION's current projection."
  (remove-if-not
   (lambda (item)
     (and (equal "function_call_output" (json-get item "type"))
          (equal identifier (json-get item "call_id"))))
   (conversation-input-items conversation)))

(-> compaction-tests--contains-p (list string) boolean)
(defun compaction-tests--contains-p (items text)
  "Return whether an item projection contains the fixture's distinctive content."
  (not (null (find-if (lambda (item) (search text (json-encode item))) items))))

(-> compaction-tests--checkpoint (conversation) list)
(defun compaction-tests--checkpoint (conversation)
  "Read the latest summary/native checkpoint from durable storage."
  (find-if (lambda (record) (member (first record) '(:summary :native-compaction)))
           (reverse (rest (conversation--read-records (conversation-pathname conversation))))))

(-> test-compaction-unfinished-cutoff-replay () null)
(defun test-compaction-unfinished-cutoff-replay ()
  "Preserve late correlated outputs, steering and private metadata across restart and family switch."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (dolist (native-p '(nil t))
      (let ((conversation (conversation-create configuration)))
        (conversation-append-user-message conversation "captured question")
        (conversation-append-provider-item conversation (compaction-tests--call "late-call"))
        (let* ((capture  (conversation-compaction-capture conversation))
               (cutoff   (conversation-compaction-cutoff-sequence capture))
               (view     (conversation-compaction-view capture))
               (snapshot (mapcar #'json-encode (conversation-input-items view))))
          (conversation-append-tool-result conversation "late-call" :tool-name "test.effect"
                                           :output "actual late result" :success-p t)
          (conversation-append-user-message conversation "late steering instruction")
          (conversation-append-provider-item
           conversation (json-object "type" "reasoning" "encrypted_content" "private-late-reasoning"))
          (test-assert (equal snapshot (mapcar #'json-encode (conversation-input-items view)))
                       "arrivals do not mutate the provider snapshot")
          (let ((commit-sequence (conversation-next-sequence conversation)))
            (if native-p
                  (conversation-append-native-compaction
                   conversation (list (json-object "type" "compaction" "encrypted_content" "private-checkpoint"))
                   :family ':codex :summary "Portable handoff." :compaction capture)
                (conversation-append-summary conversation "Portable handoff." :compaction capture))
            (let ((record (compaction-tests--checkpoint conversation)))
              (test-assert (= cutoff (getf (rest record) :cutoff-seq)) "checkpoint records the request cutoff")
              (test-assert (= (1- commit-sequence) (getf (rest record) :through-seq))
                           "checkpoint covers arrivals through the publication boundary")))
          (dolist (projection (list conversation (conversation-load (conversation-pathname conversation))))
            (let ((outputs (compaction-tests--outputs projection "late-call")))
              (test-assert (= 1 (length outputs)) "live and restarted history contain one correlated output")
              (test-assert (equal "actual late result" (json-get (first outputs) "output"))
                           "actual output wins over a pure interrupted repair"))
            (test-assert (compaction-tests--contains-p (conversation-input-items projection) "late steering instruction")
                         "steering after the capture survives compaction and replay")
            (test-assert (compaction-tests--contains-p
                          (conversation-input-items-for-family projection ':codex) "private-late-reasoning")
                         "producing family retains its late private context")
            (let ((portable (conversation-input-items-for-family projection ':grok)))
              (test-assert (not (compaction-tests--contains-p portable "private-late-reasoning"))
                           "private late context never leaks across provider families")
              (test-assert (not (compaction-tests--contains-p portable "private-checkpoint"))
                           "opaque native checkpoints never enter portable history")
              (test-assert (compaction-tests--contains-p portable "Portable handoff.")
                           "the other family receives a portable summary")))))))
  nil)

(-> test-compaction-unfinished-durable-repair () null)
(defun test-compaction-unfinished-durable-repair ()
  "Retain a truthful durable repair when late actual output arrives, without repeating the side effect."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation             (conversation-create configuration))
           (*task-test-effect-count* 0))
      (conversation-append-user-message conversation "uncertain work")
      (conversation-append-provider-item conversation (compaction-tests--call "uncertain-call"))
      (conversation-append-summary conversation "The outcome is unknown.")
      (dolist (projection (list conversation (conversation-load (conversation-pathname conversation))))
        (let ((outputs (compaction-tests--outputs projection "uncertain-call")))
          (test-assert (= 1 (length outputs)) "uncertain calls receive exactly one correlated repair")
          (test-assert (equal *conversation-interrupted-tool-output* (json-get (first outputs) "output"))
                       "harness repair truthfully reports interruption rather than success")))
      (let ((capture (conversation-compaction-capture conversation)))
        (conversation-append-tool-result conversation "uncertain-call" :tool-name "test.effect"
                                         :output "authoritative recovered output" :success-p t)
        (conversation-append-summary conversation "The actual outcome is now known."
                                     :compaction capture))
      (dolist (projection (list conversation (conversation-load (conversation-pathname conversation))))
        (let ((outputs (compaction-tests--outputs projection "uncertain-call")))
          (test-assert (= 1 (length outputs)) "a tolerated late result does not duplicate protocol output")
          (test-assert (equal *conversation-interrupted-tool-output* (json-get (first outputs) "output"))
                       "published truthful repair retains its established protocol meaning")))
      (test-assert (zerop *task-test-effect-count*) "transcript repair never dispatches uncertain side effects")))
  nil)

(-> test-agent-compaction-unfinished-arrivals () null)
(defun test-agent-compaction-unfinished-arrivals ()
  "Capture before both provider stages and preserve results and steering received inside each stage."
  (dolist (case '((nil :summary) (t :native) (t :summary)))
    (with-test-configuration (configuration root)
      (declare (ignore root))
      (let* ((conversation (conversation-create configuration))
             (provider     (compaction-tests--provider
                              :configuration configuration
                            :native-p (first case) :stage (second case)
                            :arrival         (lambda ()
                                               (conversation-append-tool-result
                                                conversation "during-request" :tool-name "test.effect"
                                                :output "result during provider request" :success-p t)
                                               (conversation-append-user-message conversation "steering during provider request"))))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (agent-test-registry) :worker nil)))
        (conversation-append-user-message conversation "before provider request")
        (conversation-append-provider-item conversation (compaction-tests--call "during-request"))
        (let ((cutoff (1- (conversation-next-sequence conversation))))
          (agent-compact-conversation agent (make-instance 'agent-observer))
          (test-assert (= cutoff (getf (rest (compaction-tests--checkpoint conversation)) :cutoff-seq))
                       "agent captures its cutoff before issuing either compaction request"))
        (test-assert (not (compaction-tests--contains-p
                           (first (native-scripted-provider-native-input-snapshots provider))
                           "steering during provider request"))
                     "native input remains the pre-request snapshot")
        (test-assert (not (compaction-tests--contains-p
                           (first (scripted-provider-input-snapshots provider))
                           "steering during provider request"))
                     "summary input remains tied to the original cutoff")
        (dolist (projection (list conversation (conversation-load (conversation-pathname conversation))))
          (test-assert (compaction-tests--contains-p (conversation-input-items projection)
                                                   "steering during provider request")
                       "provider-stage steering survives publication and restart")
          (let ((outputs (compaction-tests--outputs projection "during-request")))
            (test-assert (= 1 (length outputs)) "one correlated result survives provider-stage arrivals")
            (test-assert (equal "result during provider request" (json-get (first outputs) "output"))
                         "provider-stage actual output supersedes speculative repair"))))))
  nil)

(-> test-compaction-unfinished-failure-atomicity () null)
(defun test-compaction-unfinished-failure-atomicity ()
  "Leave live and durable projections unchanged when provider or checkpoint publication fails."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration))
           (provider     (make-instance 'scripted-provider
                                        :results (list (make-condition 'simple-error
                                                                       :format-control "compaction failure"))))
           (agent        (agent-create :configuration configuration :conversation conversation
                                       :provider provider :tool-registry (agent-test-registry) :worker nil)))
      (conversation-append-user-message conversation "failure must retain this work")
      (conversation-append-provider-item conversation (compaction-tests--call "failure-call"))
      (let ((items    (copy-list (conversation-input-items conversation)))
            (sequence (conversation-next-sequence conversation)))
        (test-assert (handler-case
                         (progn (agent-compact-conversation agent (make-instance 'agent-observer)) nil)
                       (simple-error () t))
                     "provider failure is propagated")
        (test-assert (equal items (conversation-input-items conversation)) "provider failure preserves the projection")
        (test-assert (= sequence (conversation-next-sequence conversation)) "provider failure publishes no checkpoint")
        (test-call-with-function-replacements
         (list (list 'conversation-append-record
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "injected checkpoint publication failure"))))
         (lambda ()
           (test-assert (handler-case
                            (progn (conversation-append-summary conversation "unpublished summary") nil)
                          (simple-error () t))
                        "checkpoint failure is propagated")))
        (test-assert (equal items (conversation-input-items conversation)) "publication failure preserves the projection")
        (test-assert (= sequence (conversation-next-sequence conversation)) "publication failure consumes no sequence")
        (test-assert (null (compaction-tests--checkpoint conversation)) "failed publication leaves no durable checkpoint")
        (test-assert (compaction-tests--contains-p
                      (conversation-input-items (conversation-load (conversation-pathname conversation)))
                      "failure must retain this work")
                     "restart retains the original durable history"))))
  nil)

(-> test-agent-compaction-live-child-shell () null)
(defun test-agent-compaction-live-child-shell ()
  "Retain real running child and async shell identities, then deliver each outcome once after restart."
  (dolist (native-p '(nil t))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let* ((registry       (agent-tool-registry parent))
              (conversation   (agent-conversation parent))
              (directory      (config :working-directory configuration))
              (release        (merge-pathnames "compaction-release" directory))
              (effects        (merge-pathnames "compaction-effects" directory))
              (barrier        (make-instance 'task-test-blocking-tool
                                             :namespace "test" :name "block"
                                             :description "Wait until compaction has published."
                                             :parameters  (tool-object-schema (json-object) nil)))
              (child-provider (make-instance 'task-test-provider :mode ':blocking-tool))
              (context        (make-instance 'tool-context
                                             :configuration configuration :conversation conversation
                                             :registry registry :agent parent :worker nil
                                             :command-authorization-function
                                             (lambda (command directory)
                                               (declare (ignore command directory))
                                               ':full-access)))
              (child          nil)
              (shell          nil))
         (tool-registry-register registry barrier)
         (setf (agent-provider parent) child-provider)
         (unwind-protect
              (progn
                (tool-execute (tool-registry-find registry "task" "run") context
                              (json-object "name" "compaction-child" "agent" "task"
                                           "task" "Enter the blocking ordinary tool."
                                           "completion-policy" "continue"))
                (setf child (first (task-orchestrator-list-jobs runtime)))
                (test-assert
                 (task-tests--wait-until
                  (lambda ()
                    (with-lock-held ((task-test-blocking-tool-lock barrier))
                      (task-test-blocking-tool-started-p barrier)))
                  5)
                 "the actual child reaches its running tool before compaction")
                (let* ((result
                         (tool-execute
                          (tool-registry-find registry "shell" "run") context
                          (json-object
                           "command"
                           (test-fixture-shell-command
                            *platform*
                            "while [ ! -f compaction-release ]; do sleep 0.02; done; printf 'shell-once\\n' >> compaction-effects; printf shell-finished"
                            "while (!(Test-Path compaction-release)) { Start-Sleep -Milliseconds 20 }; Add-Content compaction-effects shell-once; [Console]::Write('shell-finished')")
                           "async" t "completion-policy" "continue")))
                       (record (getf (rest (tool-result-details result)) :job)))
                  (setf shell (task-orchestrator-find-visible-job runtime (getf record :id)
                                                                  :viewer    parent
                                                                  :tool-name "job.get")))
                (test-assert (and child shell (not (job-terminal-p child)) (not (job-terminal-p shell)))
                             "both real jobs remain outstanding before compaction")
                (task-completion-watch child parent)
                (task-completion-watch shell parent)
                (let* ((identifiers    (mapcar #'session-job-execution-identifier (list child shell)))
                       (child-requests (task-test-provider-request-count child-provider)))
                    (setf (agent-provider parent)
                          (compaction-tests--provider :configuration configuration
                                                      :native-p      native-p))
                  (agent-compact-conversation parent (make-instance 'agent-observer))
                  (let ((snapshot (getf (rest (compaction-tests--checkpoint conversation)) :unfinished-work)))
                    (test-assert (= 2 (getf snapshot :total)) "checkpoint contains authoritative outstanding-job identities")
                    (dolist (identifier identifiers)
                      (test-assert (find identifier (getf snapshot :jobs)
                                         :key (lambda (row) (getf row :execution-id)) :test #'equal)
                                   "both child and shell execution identities survive native/portable compaction")))
                  (test-assert (= child-requests (task-test-provider-request-count child-provider))
                               "compaction issues no new child request")
                  (test-assert (not (probe-file effects)) "compaction does not release or reexecute the shell")
                  (task-tests--release-blocking-tool barrier)
                  (with-open-file (stream release :direction ':output :if-exists ':supersede)
                    (write-line "release" stream))
                  (dolist (job (list child shell))
                    (multiple-value-bind (snapshot terminal-p) (session-job-await job 10)
                      (declare (ignore snapshot))
                      (test-assert terminal-p "the original job completes after test release")))
                  (cl-jobpond:completion-subscription-refresh
                   (task-completion-service-subscription (task-completion--service parent)))
                  (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
                    (unwind-protect
                         (progn
                           (task-completion-restore fresh)
                           (test-assert (= 2 (length (task-completion-deliver fresh)))
                                        "restart delivers each original outcome exactly once")
                           (dolist (identifier identifiers)
                             (test-assert (task-completion--delivered-p (agent-conversation fresh) identifier)
                                          "both stable execution receipts are durable"))
                           (test-assert (null (task-completion-deliver fresh))
                                        "a repeated safe boundary does not redeliver either outcome")
                           (conversation-append-summary (agent-conversation fresh) "Delivered both original jobs.")
                           (let ((again (job-completion-tests--fresh-agent fresh configuration)))
                             (unwind-protect
                                  (progn
                                    (task-completion-restore again)
                                    (test-assert (null (task-completion-deliver again))
                                                 "recompaction and another restart retain exactly-once receipts"))
                               (task-orchestrator-close (task-completion--runtime again)))))
                      (task-orchestrator-close (task-completion--runtime fresh))))
                  (test-assert (= 1 (length (uiop:read-file-lines effects)))
                               "the actual shell side effect ran once across compaction and restarts")))
           (task-tests--release-blocking-tool barrier)
           (with-open-file (stream release :direction ':output :if-exists ':supersede)
             (write-line "cleanup release" stream))
           (dolist (job (remove nil (list child shell)))
             (session-job-await job 10)))))))
  nil)

(-> test-compaction-unfinished-corrupt-replay () null)
(defun test-compaction-unfinished-corrupt-replay ()
  "Reject corrupted cutoff, carry-forward metadata and duplicate correlations at durable replay."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (dolist (corruption '(:cutoff :missing-cutoff :duplicate-output :family :wire-json))
      (let ((conversation (conversation-create configuration)))
        (conversation-append-user-message conversation "checkpoint corruption fixture")
        (conversation-append-provider-item conversation (compaction-tests--call "corruption-call"))
        (conversation-append-summary conversation "Unknown result retained.")
        (let* ((pathname   (conversation-log-pathname conversation))
               (records    (copy-tree (conversation--read-records pathname)))
               (checkpoint (find :summary records :key #'first))
               (properties (rest checkpoint))
               (rows       (getf properties :carry-forward)))
          (ecase corruption
            (:cutoff
             (setf (getf properties :cutoff-seq) (1+ (getf properties :through-seq))))
            (:missing-cutoff
             (remf properties :cutoff-seq))
            (:duplicate-output
             (setf (getf properties :carry-forward)
                   (append rows
                           (loop for text in '("first actual output" "second actual output")
                                 collect (list :wire-json
                                               (json-encode
                                                (function-call-output-item "corruption-call" text)))))))
            (:family
             (setf (getf (first rows) :family) "not-a-family"))
            (:wire-json
             (setf (getf (first rows) :wire-json) "{")))
          (setf (rest checkpoint) properties)
          (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
            (with-standard-io-syntax
              (dolist (record records)
                (write record :stream stream :readably t)
                (terpri stream))))
          (test-assert
           (handler-case (progn (conversation-load (conversation-pathname conversation)) nil)
             (conversation-error () t))
           (format nil "durable replay rejects ~A corruption" corruption))))))
  nil)

(-> test-agent-compaction-completed-during-request () null)
(defun test-agent-compaction-completed-during-request ()
  "Refresh terminal shell state and artifacts at checkpoint publication, then deliver once after restart."
  (dolist (case '((nil :summary) (t :native) (t :summary)))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (let* ((conversation (agent-conversation parent))
              (registry     (agent-tool-registry parent))
              (release      (merge-pathnames "during-request-release" (config :working-directory configuration)))
              (context      (make-instance 'tool-context
                                           :configuration configuration :conversation conversation
                                           :registry registry :agent parent :worker nil
                                           :command-authorization-function
                                           (lambda (command directory)
                                             (declare (ignore command directory))
                                             ':full-access)))
              (result       (tool-execute
                             (tool-registry-find registry "shell" "run") context
                             (json-object
                              "command"
                              (test-fixture-shell-command
                               *platform*
                               "while [ ! -f during-request-release ]; do sleep 0.02; done; printf completed-during-request"
                               "while (!(Test-Path during-request-release)) { Start-Sleep -Milliseconds 20 }; [Console]::Write('completed-during-request')")
                              "async" t "completion-policy" "continue")))
              (record       (getf (rest (tool-result-details result)) :job))
              (job          (task-orchestrator-find-visible-job runtime (getf record :id)
                                                                :viewer    parent
                                                                :tool-name "job.get"))
              (identifier   (session-job-execution-identifier job)))
         (unwind-protect
              (progn
                (task-completion-watch job parent)
                (test-assert (not (job-terminal-p job)) "the actual shell job is outstanding at capture")
                (setf (agent-provider parent)
                      (compaction-tests--provider
                       :configuration configuration
                       :native-p (first case) :stage (second case)
                         :arrival     (lambda ()
                                       (with-open-file (stream release :direction ':output :if-exists ':supersede)
                                         (write-line "complete inside provider request" stream))
                                       (multiple-value-bind (snapshot terminal-p) (session-job-await job 10)
                                         (declare (ignore snapshot))
                                         (test-assert terminal-p "shell finishes before compaction provider returns"))
                                       (cl-jobpond:completion-subscription-refresh
                                        (task-completion-service-subscription (task-completion--service parent))))))
                (agent-compact-conversation parent (make-instance 'agent-observer))
                (let* ((work (getf (rest (compaction-tests--checkpoint conversation)) :unfinished-work))
                       (row  (find identifier (getf work :jobs)
                                   :key (lambda (entry) (getf entry :execution-id)) :test #'equal)))
                  (test-assert (eq ':completed (getf row :status)) "checkpoint refreshes state completed during a provider request")
                  (test-assert (eq ':undelivered (getf row :delivery)) "compaction does not acknowledge terminal outcomes")
                  (test-assert (and (getf row :artifact-path) (probe-file (getf row :artifact-path)))
                               "checkpoint retains an inspectable terminal artifact")
                  (let ((fresh (job-completion-tests--fresh-agent parent configuration)))
                    (unwind-protect
                         (progn
                           (let* ((snapshot (task-unfinished-work-snapshot fresh))
                                  (restored (find identifier (getf snapshot :jobs)
                                                  :key (lambda (entry) (getf entry :execution-id)) :test #'equal)))
                             (test-assert (eq ':completed (getf restored :status))
                                          "post-restart outstanding context reflects authoritative terminal state")
                             (test-assert (equal (getf row :artifact-path) (getf restored :artifact-path))
                                          "post-restart context retains the same artifact reference"))
                           (task-completion-restore fresh)
                           (test-assert (= 1 (length (task-completion-deliver fresh)))
                                        "the completion during summarization is delivered after restart")
                           (test-assert (task-completion--delivered-p (agent-conversation fresh) identifier)
                                        "the original execution receives its durable receipt")
                           (test-assert (null (task-completion-deliver fresh))
                                        "subsequent boundaries do not redeliver the same completion"))
                      (task-orchestrator-close (task-completion--runtime fresh))))))
           (with-open-file (stream release :direction ':output :if-exists ':supersede)
             (write-line "cleanup release" stream))
           (session-job-await job 10))))))
  nil)

(-> test-compaction-announced-before-job-state () null)
(defun test-compaction-announced-before-job-state ()
  "Test compaction is announced before job storage, which may be large, is read."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration))
           (agent        (agent-create :configuration configuration :conversation conversation
                                       :provider (compaction-tests--provider
                                                  :configuration configuration)
                                       :tool-registry (agent-test-registry) :worker nil))
           (events       nil)
           (snapshot     (symbol-function 'task-unfinished-work-snapshot)))
      (conversation-append-user-message conversation "before compaction")
      (test-call-with-function-replacements
       (list (list 'task-unfinished-work-snapshot
                   (lambda (&rest arguments)
                     (push ':job-state events)
                     (apply snapshot arguments))))
       (lambda ()
         (agent-compact-conversation
          agent
          (make-instance 'callback-agent-observer
                         :status-callback (lambda (status details)
                                            (declare (ignore details))
                                            (push status events)
                                            nil)))))
      (let ((order (reverse events)))
        (test-assert (and (member ':compaction-started order)
                          (member ':job-state order)
                          (< (position ':compaction-started order)
                             (position ':job-state order)))
                     "the compaction status precedes reading job state"))))
  nil)

(-> test-compaction-child-owner-receipts () null)
(defun test-compaction-child-owner-receipts ()
  "Read both child depths from task storage and reject a same-ID primary."
  (with-test-configuration (configuration)
    (let* ((root    (conversation-create configuration :identifier "sibling-root"))
           (agent   (make-instance 'agent :configuration configuration :conversation root))
           (runtime (task-orchestrator-create))
           (parent  agent))
      (unwind-protect
           (dotimes (index 2)
             (let* ((job         (task-tests--make-job runtime :identifier "sibling-child" :parent-agent parent
                                                     :root-identifier (task-parent-root-conversation-identifier parent)))
                    (owner       (task-job-execution-identifier job))
                    (child       (conversation-create configuration :identifier owner
                                                      :storage-root             (task--artifact-root configuration job)))
                    (child-agent (make-instance 'task-child-agent :configuration configuration
                                               :conversation child :job job :depth (1+ index)))
                    (receipt     "completion:sibling-work"))
               (conversation-append-user-message child "Completed." :pending-input-identifier receipt)
               (test-assert (equal "sibling-root" (task-parent-root-conversation-identifier child-agent))
                            "nested children retain original root identity")
               (test-assert (gethash receipt (task-unfinished-work--owner-receipts child-agent owner))
                            "current child retains live receipt fast path")
               (test-assert (gethash receipt (task-unfinished-work--owner-receipts agent owner))
                            "root viewer resolves child owner durable receipts at both depths")
               (let ((foreign (conversation-create configuration :identifier owner)))
                 (conversation-append-user-message foreign "Foreign." :pending-input-identifier "completion:foreign")
                 (test-assert (not (gethash "completion:foreign" (task-unfinished-work--owner-receipts agent owner)))
                              "same-ID primary transcript cannot hide unfinished child work"))
               (let* ((entry    (list :record (list :completion-owner-conversation owner :execution-id "sibling-work"
                                                    :detached-p t :id "sibling-job")))
                      (receipts (task-unfinished-work--owner-receipts agent owner)))
                 (test-assert (null (task-unfinished-work--row entry :viewer "sibling-root" :receipts receipts))
                              "delivered detached work disappears from unfinished projection"))
               (with-open-file (s (conversation-log-pathname child) :direction :output :if-exists :append)
                 (write-string "(:message :seq 2 :role)" s))
               (test-assert (null (handler-case (task-unfinished-work--owner-receipts agent owner) (error () nil)))
                            "invalid tail invalidates durable sibling evidence")
               (test-assert (task-unfinished-work--row
                             (list :record (list :completion-owner-conversation owner
                                                 :execution-id "unknown-work" :detached-p t))
                             :viewer "sibling-root" :receipts nil)
                            "unknown detached work stays in unfinished projection")
               (setf parent child-agent)))
        (task-orchestrator-close runtime))))
  nil)
