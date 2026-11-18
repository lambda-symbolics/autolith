(in-package #:autolith)

;;;; -- Complaint-driven Correction Boundaries --

(-> complaint-tests--application (configuration) application)
(defun complaint-tests--application (configuration)
  "Create a local application with a private conversation and captured terminal."
  (make-instance 'application
                 :configuration configuration
                 :conversation  (conversation-create configuration)
                 :ui            (terminal-ui-create
                                 :terminal (make-instance 'stream-terminal
                                                          :input-stream  (make-string-input-stream "")
                                                          :output-stream (make-string-output-stream)
                                                          :input-file-descriptor -1 :columns 80))))

(-> complaint-tests--message (string) hash-table)
(defun complaint-tests--message (text)
  "Return a canonical completed assistant message."
  (json-object "type" "message" "role" "assistant"
               "content" (vector (json-object "type" "output_text" "text" text))))

(-> complaint-tests--rule (string) list)
(defun complaint-tests--rule (instruction)
  "Return a correction whose literal evidence is deliberately distinctive."
  (list :id "concise" :instruction instruction :when '(:output-contains ("verbose passage"))))

(-> test-complaint-output-selection () null)
(defun test-complaint-output-selection ()
  "Select actual generated output, excluding other conversation channels."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (conversation-append-user-message conversation "user-only evidence")
      (conversation-append-provider-item conversation (complaint-tests--message "verbose passage"))
      (conversation-append-provider-item
       conversation (json-object "type" "reasoning" "summary"
                                 (vector (json-object "type" "summary_text" "text" "private-only evidence"))))
      (conversation-append-provider-item
       conversation (json-object "type" "function_call" "name" "shell.run"
                                 "call_id" "call-complaint" "arguments" "{\"command\":\"tool argument\"}"))
      (conversation-append-tool-result conversation "call-complaint" :tool-name "shell.run"
                                       :output "result-only evidence" :success-p t)
      (conversation-append-provider-item conversation (complaint-tests--message "ephemeral-only evidence")
                                         :persistence ':next-response)
      (let ((outputs (context-rule-recent-outputs conversation)))
        (test-assert (= (length outputs) 2) "only completed assistant text and tool arguments are evidence")
        (test-assert (search "verbose passage" (first outputs)) "evidence is in chronological order")
        (test-assert (context-rule-output-matches-p '("TOOL ARGUMENT") outputs)
                     "literal output matching is case insensitive")
        (dolist (needle '("user-only" "private-only" "result-only" "ephemeral-only"))
          (test-assert (not (context-rule-output-matches-p (list needle) outputs))
                       "unrelated channels cannot validate a trigger")))
      (test-assert (= (length (context-rule-recent-outputs conversation :limit 1)) 1)
                   "recent evidence honors its item bound")
      (conversation-append-provider-item
       conversation (json-object "type" "message" "role" "assistant" "content" "string content evidence"))
      (test-assert (context-rule-output-matches-p '("string content evidence")
                                                  (context-rule-recent-outputs conversation :limit 1))
                   "plain string assistant content is visible evidence")
      (conversation-append-provider-item
       conversation (complaint-tests--message (make-string 20000 :initial-element #\x)))
      (test-assert (= (length (first (context-rule-recent-outputs
                                      conversation :limit 1 :maximum-characters 17))) 17)
                   "evidence is bounded before concatenation")))
  nil)

(-> test-complaint-generation () null)
(defun test-complaint-generation ()
  "Exercise actual structured inference, schema repair and observed-trigger repair."
  (with-test-configuration (configuration)
    (let* ((provider
             (make-instance
              'rlm-inference-test-provider
              :results (mapcar (lambda (text) (rlm-inference-test-result (make-identifier) text 10))
                               '("not JSON"
                                 "{\"id\":\"concise\",\"instruction\":\" \",\"trigger\":\"verbose passage\"}"
                                 "{\"id\":\"concise\",\"instruction\":\"Answer concisely.\",\"trigger\":\"invented evidence\"}"
                                 "{\"id\":\"concise\",\"instruction\":\"Answer concisely.\",\"trigger\":\"verbose passage\"}"))))
           (*active-application* (make-instance 'application :provider provider :configuration configuration)))
      (multiple-value-bind (rule matched-p)
          (complaint-generate-rule configuration "Too wordy" '("A verbose passage"))
        (test-assert matched-p "generation validates the final literal against supplied outputs")
        (test-assert (equal (getf rule :instruction) "Answer concisely.") "generation returns validated rule data")
        (test-assert (null (rlm-inference-test-provider-results provider)) "invalid and unmatched candidates are repaired")
        (test-assert (every #'zerop (rlm-inference-test-provider-tool-schema-counts provider))
                     "complaint inference has no tool capability")
        (test-assert (null (context-rule-saved configuration)) "generation alone never persists a correction")))
    (let* ((provider
             (make-instance
              'rlm-inference-test-provider
              :results (loop repeat *complaint-generation-attempts*
                             collect (rlm-inference-test-result
                                      (make-identifier)
                                      "{\"id\":\"future\",\"instruction\":\"Keep answers short.\",\"trigger\":\"future phrase\"}" 10))))
           (*active-application* (make-instance 'application :provider provider :configuration configuration)))
      (multiple-value-bind (rule matched-p) (complaint-generate-rule configuration "Too wordy" nil)
        (test-assert (and rule (not matched-p)) "unobserved evidence is explicitly marked unconfirmed")))
    (let* ((provider
             (make-instance
              'rlm-inference-test-provider
              :results (loop repeat *complaint-generation-attempts*
                             collect (rlm-inference-test-result
                                      (make-identifier)
                                      "{\"id\":\"invalid\",\"instruction\":\"\",\"trigger\":\"x\"}" 10))))
           (*active-application* (make-instance 'application :provider provider :configuration configuration)))
      (test-assert (handler-case (progn (complaint-generate-rule configuration "Too wordy" nil) nil)
                     (complaint-error (condition) (eq (complaint-error-phase condition) ':generation)))
                   "invalid candidates fail with a typed workflow condition")))
  nil)

(-> test-complaint-review-and-save () null)
(defun test-complaint-review-and-save ()
  "Require scope, unconfirmed-match and replacement approval, with amendment and cancellation."
  (with-test-configuration (configuration root)
    (let* ((configuration     (configuration-copy configuration :working-directory root))
           (application       (complaint-tests--application configuration))
           (choices           nil)
           (matched-p         t)
           (instruction       "Use concise answers.")
           (generation-inputs nil))
      (test-call-with-function-replacements
       (list (list 'terminal-interactive-p (lambda (terminal) (declare (ignore terminal)) t))
             (list 'complaint-generate-rule
                   (lambda (candidate complaint outputs &key feedback previous-rule)
                     (declare (ignore candidate complaint outputs))
                     (push (list feedback previous-rule) generation-inputs)
                     (values (complaint-tests--rule instruction) matched-p)))
             (list 'application--pick-identifier
                   (lambda (candidate &rest arguments)
                     (declare (ignore candidate))
                     (let ((choice (pop choices)))
                       (test-assert (or (null choice)
                                        (find choice (getf arguments :items)
                                              :key (lambda (item) (getf item :value))))
                                    "review choices correspond to offered actions")
                       choice)))
             (list 'application-complaint--read-text
                   (lambda (candidate title) (declare (ignore candidate title)) "Prefer terse explanations.")))
       (lambda ()
         (setf choices '(:cancel))
         (application-complain application "Too wordy")
         (test-assert (null (context-rule-saved configuration)) "cancelled review writes nothing")
         (setf choices '(:amend :workspace))
         (application-complain application "Too wordy")
         (test-assert (and (equal (first (first generation-inputs)) "Prefer terse explanations.")
                           (second (first generation-inputs))) "amendments include feedback and previous candidate")
         (test-assert (equal (getf (first (context-rule-saved configuration :scope ':workspace)) :instruction)
                             instruction) "explicit workspace choice persists the candidate")
         (setf instruction "Answer in one paragraph." choices '(:workspace :cancel))
         (application-complain application "Too wordy")
         (test-assert (equal (getf (first (context-rule-saved configuration :scope ':workspace)) :instruction)
                             "Use concise answers.") "replacement cancellation preserves the existing rule")
         (setf choices '(:workspace :replace))
         (application-complain application "Too wordy")
         (test-assert (equal (getf (first (context-rule-saved configuration :scope ':workspace)) :instruction)
                             instruction) "replacement requires separate approval")
         (setf matched-p nil choices '(:global :cancel))
         (application-complain application "Too wordy")
         (test-assert (null (context-rule-saved configuration :scope ':global))
                      "unmatched trigger cancellation writes nothing")
         (setf choices '(:global :save-unconfirmed))
         (application-complain application "Too wordy")
         (test-assert (= (length (context-rule-saved configuration :scope ':global)) 1)
                      "unmatched rule can be saved only with explicit unconfirmed approval")))
      (test-assert (handler-case (progn (application-complain application "Too wordy") nil)
                     (complaint-error () t)) "noninteractive invocation fails before inference")))
  nil)

(-> test-complaint-editing () null)
(defun test-complaint-editing ()
  "Exercise feedback editing and cancellation through the modal event callback."
  (with-test-configuration (configuration)
    (let ((application (complaint-tests--application configuration)))
      (test-call-with-function-replacements
       (list (list 'application--pick-identifier
                   (lambda (candidate &rest arguments)
                     (declare (ignore candidate))
                     (let ((event (getf arguments :on-event)))
                       (funcall event '(:insert "make it terse!") nil)
                       (funcall event ':backspace nil)
                       (let ((result (funcall event ':submit nil)))
                         (test-assert (eq (first result) ':accept) "enter submits edited feedback")
                         (second result))))))
       (lambda ()
         (test-assert (equal (application-complaint--read-text application "Amend") "make it terse")
                      "modal feedback supports editing before submission")))
      (test-call-with-function-replacements
       (list (list 'application--pick-identifier
                   (lambda (candidate &rest arguments)
                     (declare (ignore candidate))
                     (dolist (event '(:escape :interrupt :end-of-input :stream-end))
                       (test-assert (equal (funcall (getf arguments :on-event) event nil) '(:cancel))
                                    "escape, interruption and closed input cancel feedback"))
                     nil)))
       (lambda () (test-assert (null (application-complaint--read-text application "Amend"))
                               "cancelled editing has no feedback")))))
  nil)

(-> test-complaint-command-aliases () null)
(defun test-complaint-command-aliases ()
  "Invoke canonical and alias commands through local Lisp and slash dispatch."
  (with-test-configuration (configuration)
    (let* ((application (complaint-tests--application configuration))
           (canonical   (application-command-find "/complain"))
           (calls       nil))
      (test-assert (and canonical (eq canonical (application-command-find "/wtf")))
                   "both callable names resolve to the same command")
      (test-assert (find "/complain" (application-command-completion-entries)
                         :key (lambda (entry) (getf entry :name)) :test #'equal)
                   "canonical command is completable")
      (test-assert (find "/wtf" (application-command-alias-completion-entries)
                         :key (lambda (entry) (getf entry :name)) :test #'equal)
                   "alias is completable")
      (let* ((operation  (application-operation-find application "wtf"))
             (completion (find "(wtf" (application-operation-completion-entries application)
                               :key (lambda (entry) (getf entry :name)) :test #'equal)))
        (test-assert (and operation (eq (application-operation-backend operation) canonical))
                     "native alias resolves the same command backend")
        (test-assert (and completion (equal (getf completion :primary) "(complain"))
                     "native alias completion is grouped with its canonical command")
        (test-assert (eq (application-operation-active-turn-action operation nil) ':hold)
                     "native alias honors active-turn command admission"))
      (test-assert (and (eq (application-command-busy-behavior canonical) ':hold)
                        (eq (application-command-terminal-behavior canonical) ':exclusive))
                   "review uses the command protocol's serialized exclusive terminal ownership")
      (test-call-with-function-replacements
       (list (list 'application-complain
                   (lambda (candidate &optional complaint)
                     (declare (ignore candidate))
                     (push complaint calls)
                     nil)))
       (lambda ()
         (dolist (input '("(complain \"Too verbose\")" "(wtf \"Bad choice\")"))
           (test-assert (eq (application-run-lisp-input application input) ':continue)
                        "callable commands invoke the local handler"))
         (test-assert (eq (application-command application "/wtf preserve (all) words") ':continue)
                      "slash alias invokes the same handler without evaluating the complaint")
         (test-assert (equal (reverse calls) '("Too verbose" "Bad choice" "preserve (all) words"))
                      "all invocations preserve complaint text")))))
  nil)
