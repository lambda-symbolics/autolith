(in-package #:autolith)

;;;; -- Continuation Checkpoint Tests --

(-> continuation-tests--checkpoint (&rest t) list)
(defun continuation-tests--checkpoint (&rest replacements)
  "Return a valid checkpoint citing one record, with REPLACEMENTS for its properties."
  (let ((checkpoint
          (make-continuation
           :intent     "Ship the parser."
           :notes      (list (list :kind ':completed :text "Tests pass." :refs '("r1"))
                             (list :kind ':completed :text "Docs are updated." :refs nil)
                             (list :kind ':open :text "Tag the release." :refs nil))
           :next       (list :text "Tag it." :refs '("r1"))
           :references (list (list :handle "r1" :kind ':tool-result :label "A test run."
                                   :conversation "elsewhere" :sequence 3)))))
    (loop for (key value) on replacements by (partial #'nthcdr 2)
          do (setf (getf checkpoint key) value))
    checkpoint))

(-> test-continuation-validity () null)
(defun test-continuation-validity ()
  "Accept only well-formed checkpoints that keep what they cite and stay within their bounds."
  (test-assert (continuation-p (continuation-tests--checkpoint))
               "a checkpoint that keeps the reference it cites is valid")
  (test-assert (continuation-p (make-continuation :unsummarized '(("elsewhere" 1 9))))
               "a checkpoint that summarizes nothing needs no intent")
  (dolist (invalid
            (list (continuation-tests--checkpoint :format 0)
                  (continuation-tests--checkpoint :intent "")
                  (continuation-tests--checkpoint :intent nil)
                  (continuation-tests--checkpoint
                   :notes (list (list :kind ':done :text "Tests pass." :refs nil)))
                  (continuation-tests--checkpoint
                   :notes (list (list :kind ':open :text "" :refs nil)))
                  (continuation-tests--checkpoint :references nil)
                  (continuation-tests--checkpoint :unsummarized '(("elsewhere" 0 9)))
                  (continuation-tests--checkpoint
                   :references (list (list :handle "r1" :kind ':tool-result
                                           :label  (make-string *continuation-maximum-octets*
                                                                :initial-element #\x))))
                  (append (continuation-tests--checkpoint) (list :verified t))
                  "Ship the parser."))
    (test-assert (not (continuation-p invalid))
                 "a malformed, unbounded or dangling checkpoint is rejected"))
  nil)

(-> test-continuation-rendering () null)
(defun test-continuation-rendering ()
  "Render what a checkpoint cites, what it leaves unsubstantiated and what it did not summarize."
  (let* ((checkpoint (continuation-tests--checkpoint
                      :unsummarized '(("elsewhere" 5 9))
                      :references   (list (list :handle "r1" :kind ':plan-step
                                                :label "plan step \"Tag\"" :observed "doing"))))
         (text       (continuation-render checkpoint)))
    (dolist (expected '("Tests pass. [r1]"
                        "Docs are updated. (unsubstantiated)"
                        "r1: plan step \"Tag\" (doing)"
                        "Records 5 to 9 of conversation elsewhere"))
      (test-assert (search expected text)
                   "the text shows citations, unsupported completions, references and gaps"))
    (test-assert (not (search "Tag the release. (unsubstantiated)" text))
                 "only a completed note needs a reference")
    (test-assert (string= text (continuation-render
                                (with-standard-io-syntax
                                  (read-from-string (prin1-to-string checkpoint)))))
                 "a stored checkpoint renders the same text again"))
  nil)

(-> test-continuation-replay () null)
(defun test-continuation-replay ()
  "Restore a typed checkpoint from disk and refuse one that disagrees with its record."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration))
          (checkpoint   (continuation-tests--checkpoint)))
      (conversation-append-user-message conversation "earlier work")
      (conversation-append-summary conversation checkpoint)
      (dolist (current (list conversation (conversation-load (conversation-pathname conversation))))
        (test-assert (equal checkpoint (conversation-continuation current))
                     "the provider context holds the typed checkpoint, live and after reload"))))
  (loop for (content checkpoint)
          in (list (list "Other text." (continuation-tests--checkpoint))
                   (list nil (continuation-tests--checkpoint :format 0))
                   (list nil (continuation-tests--checkpoint
                              :references (list (list :handle "r1" :kind ':tool-result
                                                      :label "A later result."
                                                      :conversation "replayed" :sequence 99)))))
        do (with-test-configuration (configuration)
             (let* ((conversation (conversation-create configuration :identifier "replayed"))
                    (through      (progn
                                    (conversation-append-user-message conversation "earlier work")
                                    (1- (conversation-next-sequence conversation)))))
               (conversation-append-record
                conversation
                (list :summary
                      :content      (or content (continuation-render checkpoint))
                      :continuation checkpoint
                      :cutoff-seq   through
                      :through-seq  through))
               (test-assert (handler-case
                                (progn (conversation-load (conversation-pathname conversation)) nil)
                              (conversation-invariant-error ()
                                t))
                            "a checkpoint that disagrees with its text, format or cutoff does not load"))))
  nil)

(-> test-continuation-adoption () null)
(defun test-continuation-adoption ()
  "Carry a typed checkpoint, with the origin of its references, into the conversation that adopts it."
  (with-test-configuration (configuration)
    (let ((origin      (conversation-create configuration))
          (destination (conversation-create configuration))
          (checkpoint  (continuation-tests--checkpoint)))
      (conversation-append-user-message origin "earlier work")
      (conversation-append-summary origin checkpoint)
      (conversation-append-context-adoption destination (conversation-context-rows origin)
                                            :participant "agent")
      (dolist (current (list destination
                             (conversation-load (conversation-pathname destination))))
        (test-assert (equal checkpoint (conversation-continuation current))
                     "the adopted context keeps the checkpoint and its references unchanged"))))
  nil)

(-> test-continuation-references () null)
(defun test-continuation-references ()
  "Offer records and plan steps to a checkpoint, keep what it cites, and report a cited step that changes."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (agent        (agent-tests--compaction-agent configuration conversation nil))
           (long         (make-string (* 2 *continuation-label-characters*)
                                      :initial-element #\s))
           (steps        (list '(:text "Write the parser" :status :doing :id "parser")
                               '(:text "Tag the release" :status :pending)
                               (list :text (concatenate 'string long " one") :status ':pending)
                               (list :text (concatenate 'string long " two") :status ':done))))
      (plan-update configuration steps)
      (conversation-append-user-message conversation "Fix the parser.")
      (conversation-append-provider-item
       conversation (agent-test-call :call-id "call-1" :namespace "shell" :name "run"))
      (conversation-append-tool-result conversation "call-1" :tool-name "shell.run"
                                                             :output    "12 tests passed"
                                                             :success-p t)
      (flet ((references (&rest arguments)
               "Return the references a checkpoint taken now may cite."
               (apply #'continuation-references
                      agent
                      (conversation-compaction-evidence-records
                       (conversation-compaction-capture conversation))
                      arguments))

             (changes ()
               "Return what an ordinary request is told about changed references."
               (let ((contribution
                       (continuation-changes
                        (make-instance 'request-context :configuration   configuration
                                                        :conversation    conversation
                                                        :tool-namespaces #()))))
                 (and contribution (context-contribution-instruction contribution)))))
        (let* ((offered (references))
               (result  (find ':tool-result offered :key (lambda (reference) (getf reference :kind))))
               (step    (find "parser" offered
                              :key (lambda (reference) (getf reference :step)) :test #'equal)))
          (test-assert (equal '(:plan-step :plan-step :plan-step :plan-step :tool-result :message)
                              (mapcar (lambda (reference) (getf reference :kind)) offered))
                       "plan steps come first, then the covered records, newest first")
          (test-assert (equal '("pending" "done")
                              (mapcar (lambda (reference) (getf reference :observed))
                                      (subseq offered 2 4)))
                       "steps that differ only past the quoted start of their text stay distinct")
          (test-assert (and (equal "call-1" (getf result :call-id))
                            (equal (conversation-identifier conversation) (getf result :conversation))
                            (= (getf result :sequence) (1- (conversation-next-sequence conversation))))
                       "a tool result is named by its conversation, sequence and call")
          (test-assert (search (format nil "conversation:id/~A sequence ~D"
                                       (conversation-identifier conversation)
                                       (getf result :sequence))
                               (getf result :label))
                       "its label gives the address the conversation resource reads it at")
          (test-assert (equal "doing" (getf step :observed))
                       "a plan step holds the state it had at capture")
          (let ((*continuation-maximum-references* 2))
            (test-assert (equal (subseq offered 0 2) (references))
                         "a full catalog keeps its first references"))
          (setf (scripted-provider-results (agent-provider agent))
                (list (agent-test-result
                       "cited"
                       (list (agent-test-checkpoint-call
                              (format nil "{\"intent\":\"Fix the parser.\",\"notes\":[{\"kind\":\"completed\",\"text\":\"The tests pass.\",\"refs\":[~S]}],\"next\":{\"text\":\"Finish the parser.\",\"refs\":[~S]}}"
                                      (getf result :handle) (getf step :handle)))))))
          (agent-compact-conversation agent (make-instance 'agent-observer))
          (let ((checkpoint (conversation-continuation conversation)))
            (test-assert (equal (list step result) (getf checkpoint :references))
                         "the checkpoint keeps exactly the references it cites")
            (test-assert (member result (references :prior checkpoint) :test #'equal)
                         "a later catalog still offers what the previous checkpoint cited"))
          (test-assert (null (changes))
                       "unchanged references add nothing to a request")
          (plan-update configuration (cons '(:text "Write the parser" :status :done :id "parser")
                                           (rest steps)))
          (test-assert (search "was doing, now done" (changes))
                       "a cited plan step that changed is reported with its state now")
          (plan-update configuration (rest steps))
          (test-assert (search "was doing, can no longer be found" (changes))
                       "a cited plan step that no longer exists is reported")))))
  nil)

(-> test-continuation-gate-references () null)
(defun test-continuation-gate-references ()
  "Offer the gates of a conversation's mission, resolve them only there, and notice changed inputs."
  (with-test-configuration (configuration root)
    (setf configuration (configuration-copy configuration :working-directory root))
    (let* ((application  (mission-test--application configuration))
           (conversation (application-conversation application)))
      (mission-test--write-file (merge-pathnames "gate-input" root) "first state")
      (application-mission-start
       application
       (mission-test--specification
        :gates '((:id "unit" :kind :artifact :path "unit" :inputs ("gate-input") :deterministic-p t))))
      (flet ((gate ()
               "Return the reference a checkpoint taken now holds for the gate."
               (find ':gate (continuation-references (application-agent application) nil)
                     :key (lambda (reference) (getf reference :kind))))

             (state (reference)
               "Return the state REFERENCE resolves to now."
               (continuation-reference-state
                ':gate reference (continuation--authorities configuration conversation))))
        (let ((reference (gate)))
          (test-assert (and (search (conversation-identifier conversation) (getf reference :mission))
                            (equal "pending" (getf reference :observed)))
                       "a gate reference names its mission by conversation and holds the gate's status")
          (test-assert (null (state (list* :mission "mission/elsewhere/0" reference)))
                       "a gate of another conversation's mission does not resolve here"))
        (test-call-with-function-replacements
         (list (list 'application-tool-context (lambda (app) (mission-test--context app)))
               (list 'mission-gate-execute
                     (lambda (gate context)
                       (declare (ignore gate context))
                       (values t "bounded test evidence"))))
         (lambda () (application-mission-verify application)))
        (let ((reference (gate)))
          (test-assert (equal "passed" (getf reference :observed))
                       "a gate that passed on its current inputs reads as passed")
          (mission-test--write-file (merge-pathnames "gate-input" root) "changed state")
          (test-assert (equal "passed, inputs changed since it was checked" (state reference))
                       "a passed gate whose declared input changed is reported without running it")
          (test-assert (eq ':passed (getf (first (getf (application-goal application) :gates))
                                          :status))
                       "resolving a reference leaves the mission's gate untouched")))))
  nil)

(-> test-continuation-claim-references () null)
(defun test-continuation-claim-references ()
  "Offer the facts a conversation was given and resolve one that stops holding, derived facts included."
  (with-test-configuration (configuration)
    (setf (config :knowledge-mode configuration) "active")
    (knowledge-tests--define-appointments configuration)
    (knowledge-propose-rule configuration
                            :id         "departure"
                            :premises   '(("event.start" "?event" "?start")
                                          ("event.travel" "?event" "?travel")
                                          ("event.buffer" "?event" "?buffer"))
                            :bindings   '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                            :conclusion '("event.departure" "?event" "?leave"))
    (knowledge-admit-rule configuration "departure")
    (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                           :quote "the review is at one")
    (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                           :quote "be there ten minutes early")
    (let* ((travel       (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                                :quote "it takes half an hour to get there"))
           (conversation (conversation-create configuration))
           (agent        (agent-tests--compaction-agent configuration conversation nil))
           (view         (knowledge-view-current configuration)))
      ;; Recall gave the conversation every fact that holds now.
      (knowledge-viewer-update configuration (memory-context-recall-viewer conversation)
                               (knowledge-view-workspace view)
                               :time     (knowledge-view-now view)
                               :function (lambda (previous time first-p)
                                           (declare (ignore previous time first-p))
                                           (values (knowledge-view-snapshot view) nil)))
      (flet ((claim (text)
               "Return the reference a checkpoint taken now holds for the fact reading TEXT."
               (find-if (lambda (reference)
                          (and (eq ':claim (getf reference :kind))
                               (search text (getf reference :label))))
                        (continuation-references agent nil)))

             (state (reference)
               "Return the state REFERENCE resolves to now."
               (continuation-reference-state
                ':claim reference (continuation--authorities configuration conversation))))
        (let ((asserted (claim "travel to review"))
              (derived  (claim "leave for review")))
          (test-assert (and (equal travel (getf asserted :claim))
                            (search (format nil "memory:claim/~A" travel) (getf asserted :label))
                            (equal "supported, current, on user-assertion" (getf asserted :observed))
                            (equal (getf asserted :observed) (getf derived :observed)))
                       "given facts are offered with their address, status and basis, derived ones included")
          (knowledge-retract configuration travel)
          (test-assert (equal "unknown, retracted" (state asserted))
                       "a retracted fact reports that it no longer applies")
          (test-assert (null (state derived))
                       "a fact derived from it can no longer be found")
          (setf (config :knowledge-mode configuration) "off")
          (test-assert (and (equal (getf derived :observed) (state derived))
                            (null (claim "travel to review")))
                       "with knowledge off no fact is offered and a cited one is taken to stand")))))
  nil)
