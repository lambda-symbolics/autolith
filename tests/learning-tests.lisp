(in-package #:autolith)

;;;; -- Procedure Learning Tests --

(-> learning-tests--call
    (conversation &key (:call-id string) (:tool string) (:arguments json-object)
                  (:output json-object) (:success-p boolean))
    null)
(defun learning-tests--call (conversation &key call-id tool arguments output (success-p t))
  "Record that the model called TOOL, a NAMESPACE.NAME string, with ARGUMENTS and received OUTPUT."
  (let ((dot (position #\. tool)))
    (conversation-append-provider-item
     conversation
     (json-object "type" "function_call" "call_id" call-id
                  "namespace" (subseq tool 0 dot) "name" (subseq tool (1+ dot))
                  "arguments" (json-encode arguments))))
  (conversation-append-tool-result conversation call-id
                                   :tool-name tool
                                   :output    (json-encode output)
                                   :success-p success-p)
  nil)

(-> learning-tests--request (conversation string integer) null)
(defun learning-tests--request (conversation response tokens)
  "Record one model request RESPONSE that used TOKENS."
  (conversation-append-provider-metadata
   conversation
   (list :request-number 1 :response-id response
         :usage (list (list "input_tokens" tokens)
                      (list "output_tokens" 0)
                      (list "total_tokens" tokens))))
  nil)

(-> learning-tests--answer (conversation string) null)
(defun learning-tests--answer (conversation text)
  "Record the model's final answer TEXT."
  (conversation-append-provider-item
   conversation
   (json-object "type" "message" "role" "assistant"
                "content" (vector (json-object "type" "output_text" "text" text))))
  nil)

(-> learning-tests--report-turn (conversation string &key (:requests integer) (:success-p boolean))
    null)
(defun learning-tests--report-turn (conversation quarter &key (requests 4) (success-p t))
  "Record a turn producing QUARTER's sales report in three tool calls over REQUESTS requests."
  (let ((file (format nil "data/~A.csv" quarter))
        (table (format nil "t-~A" quarter)))
    (conversation-append-user-message conversation
                                      (format nil "Make the sales report for ~A." quarter))
    (learning-tests--call conversation
                          :call-id   (format nil "~A-1" quarter)
                          :tool      "sales.load"
                          :arguments (json-object "file" file)
                          :output    (json-object "rows" 12 "table" table))
    (learning-tests--call conversation
                          :call-id   (format nil "~A-2" quarter)
                          :tool      "sales.report"
                          :arguments (json-object "table" table
                                                  "title" (format nil "Sales ~A" quarter))
                          :output    (json-object "report" (format nil "reports/~A.md" quarter))
                          :success-p success-p)
    (learning-tests--call conversation
                          :call-id   (format nil "~A-3" quarter)
                          :tool      "notify.send"
                          :arguments (json-object "to" "team"
                                                  "message" "The sales report is ready.")
                          :output    (json-object "sent" t))
    (dotimes (index requests)
      (learning-tests--request conversation (format nil "~A-r~D" quarter index) 1500))
    (learning-tests--answer conversation (format nil "The ~A report is ready." quarter)))
  nil)

(-> test-action-traces () null)
(defun test-action-traces ()
  "Test reading tool-call traces and their model cost from conversation logs."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (learning-tests--report-turn conversation "q1")
      (learning-tests--report-turn conversation "q2" :success-p nil)
      (learning-tests--report-turn conversation "q3" :requests 5)
      (let ((traces (action-traces-in-conversation (conversation-pathname conversation))))
        (test-assert (= 3 (length traces)) "each user turn is one trace")
        (test-assert (equal (action-trace-skeleton (first traces))
                            '("sales.load" "sales.report" "notify.send"))
                     "a trace lists its tool calls in order")
        (let ((call (second (action-trace-calls (third traces)))))
          (test-assert (and (equal (json-get (getf call :arguments) "table") "t-q3")
                            (eq (getf call :status) ':ok)
                            (search "reports/q3.md" (getf call :output)))
                       "each call keeps its arguments, status, and output"))
        (test-assert (and (= 5 (action-trace-requests (third traces)))
                          (= 7500 (action-trace-tokens (third traces))))
                     "a trace counts its model requests and tokens")
        (test-assert (equal (mapcar #'action-trace-accepted-p traces) '(t nil t))
                     "a turn with a failed call is not accepted for learning")
        (learning-tests--report-turn conversation "q4" :requests 2)
        (let ((extended (action-traces-in-conversation (conversation-pathname conversation))))
          (test-assert (and (= 4 (length extended))
                            (eq (first extended) (first traces))
                            (= 2 (action-trace-requests (fourth extended)))
                            (= 5 (action-trace-requests (third traces))))
                       "a later read continues from the cached traces without changing them"))
        (test-assert (null (action-traces-in-conversation (conversation-pathname conversation)
                                                          :workspace "/elsewhere/"))
                     "a conversation from another workspace yields no traces")
        (conversation-append-user-message conversation "Tell the team.")
        (conversation-append-provider-item
         conversation
         (json-object "type" "function_call" "call_id" "t-1" "namespace" "notify" "name" "send"
                      "arguments" "{}"))
        (conversation-append-tool-result conversation "t-1" :tool-name "notify.send"
                                                            :output "Sent to the team."
                                                            :success-p t)
        (flet ((newest ()
                 (first (last (action-traces-in-conversation
                               (conversation-pathname conversation))))))
          (let ((call (first (action-trace-calls (newest)))))
            (test-assert (and (eq (getf call :status) ':ok) (null (getf call :output)))
                         "a result later calls cannot take values from is not kept"))
          (test-assert (not (action-trace-accepted-p (newest)))
                       "a turn the model has not answered is not accepted")
          (learning-tests--answer conversation "Done.")
          (test-assert (action-trace-accepted-p (newest))
                       "a turn the model answered after its last call is accepted")
          (let ((start (conversation-next-sequence conversation)))
            (conversation-append-user-message conversation "Tell the board.")
            (learning-tests--call conversation :call-id "b-1" :tool "notify.send"
                                               :arguments (json-object "to" "board")
                                               :output (json-object "sent" t))
            (learning-tests--answer conversation "Done.")
            (conversation-append-turn-aborted conversation :turn-start-sequence start
                                                           :reason ':cancelled
                                                           :condition-type "interrupt"
                                                           :message "Cancelled.")
            (test-assert (not (action-trace-accepted-p (newest)))
                         "a cancelled turn is not accepted"))))
      (test-assert (equal (mapcar (lambda (trace) (action-trace-input trace))
                                  (action-traces configuration))
                          '("Tell the team." "Make the sales report for q4."
                            "Make the sales report for q3." "Make the sales report for q1."))
                   "recent accepted traces of the workspace come newest first")))
  nil)

(-> learning-tests--procedure () list)
(defun learning-tests--procedure ()
  "Return a two-step report procedure taking a quarter."
  '(:procedure :id "make-sales-report"
    :parameters ((:name "quarter" :type :string))
    :input ("Make" "the" "sales" "report" "for" (:param "quarter" "" "."))
    :steps ((:tool "sales.load"
             :arguments (:object ("file" (:text "data/" "quarter" ".csv"))))
            (:tool "sales.report"
             :arguments (:object ("table" (:from 0 ("table")))
                                 ("title" (:text "Sales " "quarter" ""))
                                 ("format" (:json "\"markdown\"")))))))

(-> test-procedure-templates () null)
(defun test-procedure-templates ()
  "Test filling and matching procedure argument templates and request patterns."
  (let* ((procedure (learning-tests--procedure))
         (steps (getf (rest procedure) :steps))
         (bindings (list (cons "quarter" "q5")))
         (outputs (list (json-decode "{\"rows\":12,\"table\":\"t-q5\"}")))
         (report (procedure-template-instantiate (getf (second steps) :arguments)
                                                 bindings outputs)))
    (test-assert (procedure-p procedure) "a well-formed procedure validates")
    (test-assert (and (equal (json-get report "table") "t-q5")
                      (equal (json-get report "title") "Sales q5")
                      (equal (json-get report "format") "markdown"))
                 "instantiation fills parameters, earlier results, and constants")
    (test-assert (eq ':missing-output
                     (handler-case (procedure-template-instantiate
                                    (getf (second steps) :arguments)
                                    bindings (list (json-object "rows" 3)))
                       (knowledge-error (condition) (knowledge-error-code condition))))
                 "a result without the expected field stops instantiation")
    (multiple-value-bind (found matched-p)
        (procedure-template-match (getf (second steps) :arguments) report nil outputs)
      (test-assert (and matched-p (equal (rest (assoc "quarter" found :test #'string=)) "q5"))
                   "matching recovers the parameters an instantiation used"))
    (test-assert (not (nth-value 1 (procedure-template-match
                                    (getf (second steps) :arguments)
                                    (json-decode "{\"table\":\"t-q5\",\"title\":\"Sales q6\",\"format\":\"markdown\"}")
                                    (list (cons "quarter" "q5")) outputs)))
                 "matching refuses a parameter bound to two values")
    (loop for (text expected) in '(("make the sales report for q7." "q7")
                                   ("Make the sales report for q7" nil)
                                   ("Make the sales report for ." nil)
                                   ("Make the weekly report for q7." nil))
          do (multiple-value-bind (found matched-p) (procedure-match-input procedure text)
               (test-assert (equal (and matched-p (rest (assoc "quarter" found :test #'string=)))
                                   expected)
                            (format nil "request ~S binds ~S" text expected))))
    (dolist (broken (list (list* :procedure :id "x" :parameters nil
                                 :steps '((:tool "a.b" :arguments (:object ("x" (:param "q")))))
                                 nil)
                          (list* :procedure :id "x" :parameters nil
                                 :steps '((:tool "a.b" :arguments (:object ("x" (:from 0 ("y"))))))
                                 nil)))
      (test-assert (not (procedure-p broken))
                   "a procedure naming an unknown parameter or a later step is refused")))
  nil)

(-> test-procedure-lifecycle () null)
(defun test-procedure-lifecycle ()
  "Test recording, admitting, suspending, and retiring a learned procedure."
  (with-test-configuration (configuration)
    (let ((procedure (learning-tests--procedure))
          (workspace (knowledge--workspace configuration)))
      (flet ((entry ()
               (knowledge-base-procedure (knowledge-base-read configuration)
                                         workspace "make-sales-report"))
             (run (status)
               (procedure-record-run configuration "make-sales-report"
                                     :version 1 :status status :steps 1 :caller ':model
                                     :failure (and (eq status ':failed) "sales.report failed"))))
        (test-assert (nth-value 2 (procedure-define configuration procedure
                                                    :examples '((:conversation "c" :sequence 1))
                                                    :baseline '(:requests 4 :tokens 6000)))
                     "a new procedure is recorded")
        (test-assert (and (not (nth-value 2 (procedure-define configuration procedure)))
                          (= 1 (length (getf (entry) :versions))))
                     "recording an equal procedure again adds no version")
        (test-assert (and (eq (procedure-entry-status (entry)) ':candidate)
                          (null (procedure-entry-runnable-version (entry))))
                     "a learned procedure waits for admission")
        (procedure-admit configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "an admitted procedure may run")
        (run ':failed)
        (run ':ok)
        (run ':failed)
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "a success resets the failure streak")
        (run ':failed)
        (test-assert (and (eq (procedure-entry-status (entry)) ':suspended)
                          (= 4 (getf (entry) :runs))
                          (= 3 (getf (entry) :failures))
                          (equal (getf (entry) :last-failure) "sales.report failed"))
                     "consecutive failures suspend the procedure and the ledger counts runs")
        (procedure-admit configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':admitted)
                     "admitting again lifts a suspension")
        (procedure-retire configuration "make-sales-report")
        (test-assert (eq (procedure-entry-status (entry)) ':retired)
                     "a retired procedure no longer runs")
        (test-assert (eq ':unknown-procedure
                         (handler-case (procedure-admit configuration "missing")
                           (knowledge-error (condition) (knowledge-error-code condition))))
                     "admitting an unknown procedure is refused"))))
  nil)

(-> learning-tests--archive-turn (conversation integer &key (:recipient string)) null)
(defun learning-tests--archive-turn (conversation year &key (recipient "team"))
  "Record a turn archiving YEAR's invoices and telling RECIPIENT, in two tool calls."
  (conversation-append-user-message conversation (format nil "Archive invoices of ~D" year))
  (learning-tests--call conversation
                        :call-id   (format nil "a~D-1" year)
                        :tool      "invoices.archive"
                        :arguments (json-object "year" year "compress" t)
                        :output    (json-object "archive" (json-object "path"
                                                                       (format nil "/a/~D.tar" year))))
  (learning-tests--call conversation
                        :call-id   (format nil "a~D-2" year)
                        :tool      "notify.send"
                        :arguments (json-object "to" recipient
                                                "message" (format nil "/a/~D.tar" year))
                        :output    (json-object "sent" t))
  (learning-tests--request conversation (format nil "a~D-r" year) 900)
  (learning-tests--answer conversation "Archived.")
  nil)

(-> test-procedure-mining () null)
(defun test-procedure-mining ()
  "Test generalizing recurring tool sequences into procedures that explain each source turn."
  (with-test-configuration (configuration)
    (let ((reports (conversation-create configuration))
          (archives (conversation-create configuration)))
      (dolist (quarter '("q1" "q3" "q4"))
        (learning-tests--report-turn reports quarter))
      (dolist (year '(2021 2022 2023))
        (learning-tests--archive-turn archives year))
      (let* ((traces (action-traces-in-conversation (conversation-pathname reports)))
             (procedure (procedure-mine traces)))
        (test-assert (equal procedure
                            '(:procedure
                              :id "make-sales-report"
                              :parameters ((:name "file" :type :string))
                              :input ("Make" "the" "sales" "report" "for" (:param "file" "" "."))
                              :steps ((:tool "sales.load"
                                       :arguments (:object ("file" (:text "data/" "file" ".csv"))))
                                      (:tool "sales.report"
                                       :arguments (:object ("table" (:from 0 ("table")))
                                                           ("title" (:text "Sales " "file" ""))))
                                      (:tool "notify.send"
                                       :arguments (:object ("to" (:json "\"team\""))
                                                           ("message"
                                                            (:json "\"The sales report is ready.\"")))))))
                     "varying words, earlier results, and constants become one procedure")
        (test-assert (every (lambda (trace) (procedure-explains-trace-p procedure trace)) traces)
                     "the procedure reproduces every turn it was learned from"))
      (let ((procedure (procedure-mine (action-traces-in-conversation
                                        (conversation-pathname archives)))))
        (test-assert (and (equal (getf (rest procedure) :parameters)
                                 '((:name "year" :type :integer)))
                          (equal (getf (rest procedure) :input)
                                 '("Archive" "invoices" "of" (:param "year" "" "")))
                          (equal (procedure-match-input procedure "archive invoices of 2024")
                                 '(("year" . 2024))))
                     "a number in the request becomes an integer parameter")
        (test-assert (equal (getf (second (getf (rest procedure) :steps)) :arguments)
                            '(:object ("to" (:json "\"team\""))
                              ("message" (:from 0 ("archive" "path")))))
                     "a nested result field feeds a later call"))
      (loop for (description turns expected) in
            '(("two turns are too few" ((2021 "team") (2022 "team")) nil)
              ("three agreeing turns are learned" ((2021 "team") (2022 "team") (2023 "team")) t)
              ("a value seen once is not generalized"
               ((2021 "team") (2022 "team") (2023 "board")) nil)
              ("a value varying throughout becomes a parameter"
               ((2021 "ann") (2022 "bob") (2023 "cy")) t))
            do (let ((conversation (conversation-create configuration)))
                 (loop for (year recipient) in turns
                       do (learning-tests--archive-turn conversation year :recipient recipient))
                 (test-assert (eq expected
                                  (and (procedure-learn-candidates
                                        (reverse (action-traces-in-conversation
                                                  (conversation-pathname conversation))))
                                       t))
                              description)))
      (let ((conversation (conversation-create configuration)))
        (dolist (quarter '("q1" "q2" "q3"))
          (learning-tests--report-turn conversation quarter))
        (conversation-append-user-message conversation "Make the sales report for q9.")
        (learning-tests--call conversation :call-id "x-1" :tool "sales.load"
                                           :arguments (json-object "file" "data/q9.csv")
                                           :output (json-object "rows" 0 "table" "t-q9"))
        (learning-tests--call conversation :call-id "x-2" :tool "archive.store"
                                           :arguments (json-object "table" "t-q9")
                                           :output (json-object "stored" t))
        (destructuring-bind (procedure examples evaluation)
            (first (procedure-learn-candidates
                    (reverse (action-traces-in-conversation (conversation-pathname conversation)))))
          (test-assert (and (= 3 (length examples))
                            (null (getf (rest procedure) :input))
                            (equal evaluation '(:method :leave-one-out :examples 3
                                                :counterexamples 1)))
                       "a request worded the same that did something else keeps it from being offered")))))
  (learning-tests--typed-parameters)
  nil)

(-> test-procedure-learning () null)
(defun test-procedure-learning ()
  "Test recording learned procedures once, keeping their names, and never relearning retired ones."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration))
          (workspace (knowledge--workspace configuration)))
      (learning-tests--report-turn conversation "q1")
      (learning-tests--report-turn conversation "q2")
      (test-assert (null (procedure-learn configuration)) "two turns teach nothing yet")
      (learning-tests--report-turn conversation "q3" :requests 5)
      (test-assert (equal (procedure-learn configuration) '(("make-sales-report" 1 3)))
                   "the third turn teaches a procedure")
      (let ((version (procedure-entry-version
                      (knowledge-base-procedure (knowledge-base-read configuration)
                                                workspace "make-sales-report"))))
        (test-assert (and (equal (getf version :baseline) '(:requests 4 :tokens 6500))
                          (= 3 (length (getf version :examples))))
                     "a procedure records its source turns and their average cost"))
      (learning-tests--report-turn conversation "q4")
      (test-assert (null (procedure-learn configuration))
                   "another agreeing turn does not add a version")
      (test-assert (null (procedure-learn (configuration-copy configuration :knowledge-mode "off")))
                   "nothing is learned when knowledge is off")
      (procedure-retire configuration "make-sales-report")
      (learning-tests--report-turn conversation "q5")
      (test-assert (null (procedure-learn configuration))
                   "a retired procedure is not learned again")))
  nil)

(defclass learning-tests--scripted-tool (tool)
  ((respond
    :initarg :respond
    :reader learning-tests--scripted-tool-respond
    :type function
    :documentation "Called with the arguments; returns a JSON object, or a string to fail with.")
   (calls
    :initform nil
    :accessor learning-tests--scripted-tool-calls
    :documentation "The arguments of each call, newest first."))
  (:documentation "A tool answering from a script, for procedure tests."))

(defmethod tool-execute ((tool learning-tests--scripted-tool) (context tool-context)
                         (arguments hash-table))
  "Record the call and answer as the script says."
  (declare (ignore context))
  (push arguments (learning-tests--scripted-tool-calls tool))
  (let ((answer (funcall (learning-tests--scripted-tool-respond tool) arguments)))
    (if (stringp answer)
        (tool-failure answer)
        (tool-success (json-encode answer)))))

(-> learning-tests--tool (tool-registry string function) learning-tests--scripted-tool)
(defun learning-tests--tool (registry tool respond)
  "Register scripted TOOL, a NAMESPACE.NAME string answering through RESPOND, in REGISTRY."
  (let* ((dot (position #\. tool))
         (instance (make-instance 'learning-tests--scripted-tool
                                  :namespace (subseq tool 0 dot) :name (subseq tool (1+ dot))
                                  :description "A scripted tool."
                                  :parameters (tool-object-schema (json-object) nil)
                                  :respond respond)))
    (tool-registry-register registry instance)
    instance))

(-> learning-tests--offers (configuration string) list)
(defun learning-tests--offers (configuration text)
  "Return the procedure offers context resolution makes for a conversation whose request is TEXT."
  (let ((*context-contributors* nil)
        (*context-resolver* (cl-llm-provider-api:make-context-resolver))
        (*context-last-deliveries* (make-hash-table :test #'equal))
        (*context-last-delivery-order* nil)
        (conversation (conversation-create configuration)))
    (conversation-append-user-message conversation text)
    (register-context-contributor "procedure-offers" 'procedure-offer-context :source ':built-in)
    (remove "procedure-offers"
            (context-delivery-contributions (context-resolve-request configuration conversation #()))
            :key #'context-contribution-identifier :test-not #'string=)))

(-> learning-tests--typed-parameters () null)
(defun learning-tests--typed-parameters ()
  "Test mining, matching, and offering JSON-typed request parameters."
  (loop for (description type words values next-word next-value invalid-words) in
        '(("decimal numbers" :number
           ("1.5" "2.5" "3.5" "4.5") (1.5d0 2.5d0 3.5d0 4.5d0) "5.5" 5.5d0
           ("5.5x" "1/2" "1d3" "+5.5" "05.5" "NaN" "true" "null"))
          ("negative numbers with punctuation" :number
           ("(-1.5)." "(-2.5)." "(-3.5)." "(-4.5).")
           (-1.5d0 -2.5d0 -3.5d0 -4.5d0) "(5.5)." 5.5d0
           ("(--5.5)." "(-5.5x)." "(-1/2)."))
          ("JSON exponents" :number
           ("1e-1" "2e-1" "3e-1" "4e-1") (0.1d0 0.2d0 0.3d0 0.4d0) "5E-1" 0.5d0
           ("5e" "5e-" "5e-1x" "5d-1"))
          ("mixed integers and decimals" :number
           ("1" "2.5" "3" "4.5") (1 2.5d0 3 4.5d0) "5" 5
           ("5x" "#x5" "5/1"))
          ("negative integers" :integer
           ("-1" "-2" "-3" "-4") (-1 -2 -3 -4) "5" 5
           ("5.5" "+5" "05" "5x"))
          ("JSON true" :boolean
           ("true" "false" "true" "false") (t :json-false t :json-false) "true" t
           ("True" "FALSE" "nil" "0" "null" "falsex"))
          ("JSON false with punctuation" :boolean
           ("(true)." "(false)." "(true)." "(false).")
           (t :json-false t :json-false) "(false)." :json-false
           ("(False)." "(nil)." "(0)." "(falsex).")))
        do (with-test-configuration (configuration)
             (let ((conversation (conversation-create configuration)))
               (loop for word in words
                     for value in values
                     for index from 1
                     do (conversation-append-user-message
                         conversation (format nil "Set threshold ~A" word))
                        (learning-tests--call conversation
                                              :call-id (format nil "threshold-~D" index)
                                              :tool "settings.threshold"
                                              :arguments (json-object "value" value)
                                              :output (json-object "saved" t))
                        (learning-tests--call conversation
                                              :call-id (format nil "commit-~D" index)
                                              :tool "settings.commit"
                                              :arguments (json-object)
                                              :output (json-object "saved" t))
                        (learning-tests--answer conversation "Set."))
               (let* ((traces (action-traces-in-conversation (conversation-pathname conversation)))
                      (procedure (procedure-mine traces))
                      (request (format nil "Set threshold ~A" next-word))
                      (identifier (getf (rest procedure) :id)))
                 (test-assert (and procedure
                                   (eq type (getf (first (getf (rest procedure) :parameters)) :type))
                                   (every (lambda (trace) (procedure-explains-trace-p procedure trace))
                                          traces)
                                   (procedure-learn-candidates (reverse traces)))
                              (format nil "~A are mined and explain held-out examples" description))
                 (multiple-value-bind (bindings matched-p) (procedure-match-input procedure request)
                   (let ((binding (assoc "value" bindings :test #'string=)))
                     (test-assert (and matched-p binding
                                       (procedure-json-equal-p (rest binding) next-value))
                                  (format nil "~A bind the next request's JSON value" description))
                     (when binding
                       (test-assert
                        (procedure-json-equal-p
                         (procedure-template-instantiate
                          (getf (first (getf (rest procedure) :steps)) :arguments) bindings nil)
                         (json-object "value" next-value))
                        (format nil "~A instantiate a typed tool argument" description)))))
                 (procedure-define configuration procedure)
                 (procedure-admit configuration identifier)
                 (let ((offer (first (learning-tests--offers configuration request))))
                   (test-assert
                    (and offer
                         (search (json-encode (json-object "id" identifier
                                                          "parameters" (json-object "value" next-value)))
                                 (context-contribution-evidence offer)))
                    (format nil "~A produce an offer with typed JSON parameters" description)))
                 (dolist (word invalid-words)
                   (let ((invalid-request (format nil "Set threshold ~A" word)))
                     (test-assert
                      (and (not (nth-value 1 (procedure-match-input procedure invalid-request)))
                           (null (learning-tests--offers configuration invalid-request)))
                      (format nil "~A reject malformed or mistyped word ~S" description word))))))))
  nil)

(-> test-procedure-running () null)
(defun test-procedure-running ()
  "Test running procedures through the tool path, admission, failures, suspension, and offers."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry (make-default-tool-registry :configuration configuration))
           (decision ':deny)
           (asked nil)
           (report-fails-p nil)
           (load (learning-tests--tool
                  registry "sales.load"
                  (lambda (arguments)
                    (let ((file (json-get arguments "file")))
                      (json-object "rows" 12
                                   "table" (format nil "t-~A" (subseq file 5 (- (length file) 4))))))))
           (report (learning-tests--tool
                    registry "sales.report"
                    (lambda (arguments)
                      (if report-fails-p
                          "The report service is down."
                          (json-object "report" (format nil "~A.md" (json-get arguments "table")))))))
           (context (make-instance 'tool-context
                                   :configuration configuration :worker nil :registry registry
                                   :conversation (conversation-create configuration)
                                   :command-authorization-function
                                   (lambda (command directory)
                                     (declare (ignore directory))
                                     (push command asked)
                                     decision))))
      (procedure-define configuration (learning-tests--procedure))
      (flet ((run (&rest parameters)
               (memory-resource-tests--call registry context "procedure" "run"
                                            "id" "make-sales-report"
                                            "parameters" (apply #'json-object parameters)))

             (entry ()
               (knowledge-base-procedure (knowledge-base-read configuration)
                                         (knowledge--workspace configuration)
                                         "make-sales-report")))
        (let ((declined (run "quarter" "q5")))
          (test-assert (and (not (tool-result-success-p declined))
                            (search "was declined, so it did not run" (tool-result-content declined))
                            (equal asked '("procedure.admit -- make-sales-report v1: sales.load, sales.report"))
                            (null (learning-tests--scripted-tool-calls load))
                            (eq (procedure-entry-status (entry)) ':candidate))
                       "the model's first run asks admission, and a refusal makes no call"))
        (setf decision ':sandboxed)
        (let ((ran (run "quarter" "q5")))
          (test-assert (and (tool-result-success-p ran)
                            (search "made 2 calls" (tool-result-content ran))
                            (search "t-q5.md" (tool-result-content ran))
                            (eq (procedure-entry-status (entry)) ':admitted))
                       "an approved run is admitted and reports each call")
          (let ((arguments (first (learning-tests--scripted-tool-calls report))))
            (test-assert (and (equal (json-get (first (learning-tests--scripted-tool-calls load)) "file")
                                     "data/q5.csv")
                              (equal (json-get arguments "table") "t-q5")
                              (equal (json-get arguments "title") "Sales q5")
                              (equal (json-get arguments "format") "markdown"))
                         "calls receive parameters, an earlier result, and constants")))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate "sales.data-ready"
                                  :arguments '((:name "file" :type :string))))
        (knowledge-define-contract configuration
                                   :id "load-ready-data" :tool "sales.load"
                                   :when (json-object "file" "?file")
                                   :requires #(#("sales.data-ready" "?file")))
        (let ((calls (length (learning-tests--scripted-tool-calls load)))
              (refused (run "quarter" "q8")))
          (test-assert (and (not (tool-result-success-p refused))
                            (search "Procedure make-sales-report does not apply: call 1, sales.load: Contract load-ready-data requires sales.data-ready(data/q8.csv), which does not hold."
                                    (tool-result-content refused))
                            (= calls (length (learning-tests--scripted-tool-calls load))))
                       "a run whose contract requirements do not hold makes no call"))
        (knowledge-tests--said configuration "sales.data-ready" '("data/q5.csv") "q5 data is ready")
        (knowledge-tests--said configuration "sales.data-ready" '("data/q6.csv") "q6 data is ready")
        (knowledge-tests--said configuration "sales.data-ready" '("data/q7.csv") "q7 data is ready")
        (knowledge-tests--said configuration "sales.data-ready" '("data/q9.csv") "q9 data is ready")
        (loop for (parameters expected) in '((() "needs parameter quarter")
                                            (("quarter" "q5" "region" "eu") "region is not one of them")
                                            (("quarter" 5) "must be a non-empty string"))
              do (let ((result (apply #'run parameters)))
                   (test-assert (and (not (tool-result-success-p result))
                                     (search expected (tool-result-content result)))
                                (format nil "parameters ~S are refused: ~A" parameters expected))))
        (setf report-fails-p t)
        (let ((failed (run "quarter" "q6")))
          (test-assert (and (not (tool-result-success-p failed))
                            (search "stopped after 1 of 2 calls: sales.report failed: The report service is down."
                                    (tool-result-content failed))
                            (search "Continue the work with ordinary tool calls" (tool-result-content failed)))
                       "a failing call stops the run and hands the work back"))
        (run "quarter" "q7")
        (test-assert (and (eq (procedure-entry-status (entry)) ':suspended)
                          (= 3 (getf (entry) :runs)))
                     "two failed runs in a row suspend the procedure")
        (test-assert (search "is suspended after 2 failed runs" (tool-result-content (run "quarter" "q8")))
                     "a suspended procedure refuses to run")
        (test-assert (null (learning-tests--offers configuration "Make the sales report for q9."))
                     "a suspended procedure is not offered")
        (procedure-admit configuration "make-sales-report")
        (setf report-fails-p nil)
        (let ((offer (first (learning-tests--offers configuration "make the sales report for q9."))))
          (test-assert (and offer
                            (search "make-sales-report (admitted; calls sales.load, sales.report): procedure.run {\"id\":\"make-sales-report\",\"parameters\":{\"quarter\":\"q9\"}}"
                                    (context-contribution-evidence offer)))
                       "a fitting request is offered the one call that does the work"))
        (test-assert (null (learning-tests--offers configuration "Make the sales forecast for q9."))
                     "a request that does not fit is not offered")
        (test-assert (null (learning-tests--offers configuration "Make the sales report for q8."))
                     "a request whose values its contracts refuse is not offered")
        (let ((user-run (procedure-run context "make-sales-report"
                                       :parameters '(("quarter" . "q9")) :caller ':user)))
          (test-assert (and (eq (getf user-run :status) ':ok)
                            (= 1 (getf (entry) :user-runs)))
                       "the user's own run is recorded as such"))
        (let ((listing (tool-result-content
                        (memory-resource-tests--call registry context "resource" "read"
                                                     "uri" "memory:procedures"))))
          (test-assert (and (search "make-sales-report v1 admitted · 4 runs, 2 failed" listing)
                            (search "parameters: quarter (string)" listing)
                            (search "request: Make the sales report for {quarter}." listing)
                            (search "1. sales.load {\"file\":\"data/{quarter}.csv\"}" listing)
                            (search "2. sales.report {\"table\":<call 1 table>,\"title\":\"Sales {quarter}\",\"format\":\"markdown\"}"
                                    listing))
                       "memory:procedures shows each procedure's record, request, and calls"))))
    (with-test-configuration (base root)
      (let ((registry (make-default-tool-registry
                       :configuration (configuration-copy base :working-directory root
                                                               :knowledge-mode "passive"))))
        (test-assert (null (find "procedure.run" (tool-registry-tools registry)
                                 :key #'tool-canonical-name :test #'string=))
                     "procedure.run is offered only while knowledge is active"))))
  nil)

(-> learning-tests--email-turn (conversation string string &key (:requests integer)) null)
(defun learning-tests--email-turn (conversation request period &key (requests 3))
  "Record a turn answering REQUEST by building PERIOD's report and emailing it."
  (conversation-append-user-message conversation request)
  (learning-tests--call conversation :call-id (format nil "~A-1" period) :tool "report.build"
                                     :arguments (json-object "period" period)
                                     :output (json-object "file" (format nil "~A.pdf" period)))
  (learning-tests--call conversation :call-id (format nil "~A-2" period) :tool "mail.send"
                                     :arguments (json-object "attachment" (format nil "~A.pdf" period))
                                     :output (json-object "sent" t))
  (dotimes (index requests)
    (learning-tests--request conversation (format nil "~A-r~D" period index) 1000))
  (learning-tests--answer conversation "Sent.")
  nil)

(-> test-procedure-automatic-runs () null)
(defun test-procedure-automatic-runs ()
  "Test that only exact, admitted, proven procedures run before the model reads a request."
  (loop for (description requests expected) in
        '(("varying words become wildcards, which never run unread"
           ("Please email report q1" "Kindly email report q2" "Now email report q3")
           nil)
          ("an exact pattern runs once it has saved requests"
           ("Email report q1" "Email report q2" "Email report q3")
           t))
        do (with-test-configuration (base root)
             (let* ((configuration (configuration-copy base :working-directory root))
                    (conversation (conversation-create configuration))
                    (probe (if expected "Email report q4" "Never email report q4")))
               (loop for request in requests
                     for index from 1
                     do (learning-tests--email-turn conversation request (format nil "q~D" index)))
               (destructuring-bind ((name version turns)) (procedure-learn configuration)
                 (declare (ignore version turns))
                 (procedure-admit configuration name)
                 (test-assert (and (procedure-offers configuration probe)
                                   (null (procedure-automatic-offer configuration probe)))
                              (format nil "~A: an admitted procedure without savings is only offered"
                                      description))
                 (learning-tests--demonstrate configuration conversation name
                                              :request (first requests))
                 (test-assert (eq expected
                                  (and (procedure-automatic-offer configuration probe) t))
                              description)))))
  (learning-tests--savings-evidence)
  nil)

(-> learning-tests--demonstrate
    (configuration conversation string
     &key (:request string) (:requests integer) (:tokens (option integer))
          (:answered-p boolean) (:aborted-p boolean) (:failed-extra-p boolean)
          (:provenance-p boolean) (:unmeasured-request-p boolean))
    null)
(defun learning-tests--demonstrate
    (configuration conversation identifier
     &key request (requests 1) (tokens 900) (answered-p t) aborted-p failed-extra-p
          (provenance-p t) unmeasured-request-p)
  "Run IDENTIFIER through its tool and record a turn with the requested outcome and usage."
  (let* ((registry (make-default-tool-registry :configuration configuration))
         (context (make-instance 'tool-context :configuration configuration :worker nil
                                              :registry registry :conversation conversation
                                              :call-id "model-run"))
         (entry (knowledge-base-procedure (knowledge-base-read configuration)
                                          (knowledge--workspace configuration) identifier))
         (procedure (getf (procedure-entry-runnable-version entry) :procedure))
         (parameters (json-object)))
    (dolist (binding (procedure-match-input procedure request))
      (setf (gethash (first binding) parameters) (rest binding)))
    (learning-tests--tool registry "report.build"
                          (lambda (arguments)
                            (json-object "file" (format nil "~A.pdf" (json-get arguments "period")))))
    (learning-tests--tool registry "mail.send"
                          (lambda (arguments) (declare (ignore arguments)) (json-object "sent" t)))
    (conversation-append-user-message conversation request)
    (let ((item (json-object "type" "function_call" "call_id" "model-run"
                             "namespace" "procedure" "name" "run"
                             "arguments" (json-encode (json-object "id" identifier
                                                                   "parameters" parameters)))))
      (conversation-append-provider-item conversation item)
      (let ((result (if provenance-p
                        (tool-registry-execute-call registry item context)
                        (tool-success "A historical run without definition provenance."))))
        (test-assert (tool-result-success-p result) "the measured procedure run succeeds")
        (conversation-append-tool-result conversation "model-run" :tool-name "procedure.run"
                                         :output (tool-result-content result)
                                         :success-p (tool-result-success-p result))))
    (when failed-extra-p
      (learning-tests--call conversation :call-id "extra" :tool "mail.send"
                                         :arguments (json-object) :output (json-object)
                                         :success-p nil))
    (dotimes (index requests)
      (if tokens
          (learning-tests--request conversation (format nil "run-r~D" index) tokens)
          (conversation-append-provider-metadata conversation
                                                 (list :response-id (format nil "run-r~D" index)))))
    (when unmeasured-request-p
      (conversation-append-provider-metadata conversation '(:response-id "unknown-usage")))
    (when answered-p
      (learning-tests--answer conversation "Sent."))
    (when aborted-p
      (conversation-append-record conversation '(:turn-aborted :reason "cancelled"))))
  nil)

(-> learning-tests--savings-evidence () null)
(defun learning-tests--savings-evidence ()
  "Test completion, measured costs, and definition provenance before unread procedure reuse."
  (loop for (description options expected) in
        '(("a completed cheaper turn" () t)
          ("an unfinished turn" (:answered-p nil) nil)
          ("an aborted turn" (:aborted-p t) nil)
          ("a failed companion call" (:failed-extra-p t) nil)
          ("unmeasured tokens" (:tokens nil) nil)
          ("partially measured usage" (:unmeasured-request-p t) nil)
          ("missing definition provenance" (:provenance-p nil) nil)
          ("no model requests" (:requests 0) nil)
          ("equal token use" (:tokens 3000) nil)
          ("higher token use" (:tokens 4000) nil)
          ("equal request count" (:requests 3 :tokens 300) nil))
        do (with-test-configuration (base root)
             (let* ((configuration (configuration-copy base :working-directory root))
                    (conversation (conversation-create configuration)))
               (dolist (period '("q1" "q2" "q3"))
                 (learning-tests--email-turn conversation (format nil "Email report ~A" period) period))
               (let ((name (first (first (procedure-learn configuration)))))
                 (procedure-admit configuration name)
                 (apply #'learning-tests--demonstrate configuration conversation name
                        :request "Email report q4" options)
                 (test-assert (eq expected
                                  (and (procedure-automatic-offer configuration "Email report q5") t))
                              (format nil "~A controls eligibility for unread reuse" description))
                 (when (eq (getf options :answered-p t) nil)
                   (learning-tests--answer conversation "Sent.")
                   (test-assert (procedure-automatic-offer configuration "Email report q5")
                                "only completing the measured turn makes its evidence eligible"))
                 (when expected
                   (let* ((entry (knowledge-base-procedure (knowledge-base-read configuration)
                                                           (knowledge--workspace configuration) name))
                          (procedure (copy-tree (getf (procedure-entry-runnable-version entry) :procedure))))
                     (setf (getf (rest procedure) :description) "A revised procedure.")
                     (procedure-define configuration procedure :baseline '(:requests 3 :tokens 3000))
                     (procedure-admit configuration name)
                     (test-assert (and (null (procedure-savings configuration name))
                                       (null (procedure-automatic-offer configuration "Email report q5")))
                                  "an earlier definition's evidence cannot authorize a revised definition")
                     (procedure-admit configuration name :version 1)
                     (test-assert (procedure-automatic-offer configuration "Email report q5")
                                  "readmitting the demonstrated definition restores only its own evidence")))))))
  nil)

(-> test-procedure-build-then-use () null)
(defun test-procedure-build-then-use ()
  "Test that a procedure using what its earlier call builds runs, checked when each call is made."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry (make-default-tool-registry :configuration configuration))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                                 :registry registry
                                                 :conversation (conversation-create configuration)))
           (builds-p t))
      (learning-tests--tool registry "report.build"
                            (lambda (arguments)
                              (if builds-p
                                  (json-object "file" (format nil "~A.pdf"
                                                              (json-get arguments "period")))
                                  (json-object "error" "nothing built"))))
      (learning-tests--tool registry "mail.send" (lambda (arguments)
                                                   (declare (ignore arguments))
                                                   (json-object "sent" t)))
      (knowledge-define-schema configuration
                               (knowledge-make-schema :predicate "report.built"
                                                      :arguments '((:name "file" :type :string))))
      (knowledge-define-contract configuration
                                 :id "build-observes" :tool "report.build"
                                 :observes (vector (json-object "predicate" "report.built"
                                                                "arguments" #("$result.file"))))
      (knowledge-define-contract configuration
                                 :id "send-built" :tool "mail.send"
                                 :when (json-object "attachment" "?file")
                                 :requires #(#("report.built" "?file")))
      (procedure-define configuration
                        '(:procedure :id "email-report"
                          :parameters ((:name "period" :type :string))
                          :steps ((:tool "report.build"
                                   :arguments (:object ("period" (:param "period"))))
                                  (:tool "mail.send"
                                   :arguments (:object ("attachment" (:text "" "period" ".pdf")))))))
      (procedure-admit configuration "email-report")
      (test-assert (eq ':ok (getf (procedure-run context "email-report"
                                                 :parameters '(("period" . "q1")))
                                  :status))
                   "a call needing what an earlier call builds is not refused in advance")
      (setf builds-p nil)
      (let ((run (procedure-run context "email-report" :parameters '(("period" . "q2")))))
        (test-assert (and (eq ':failed (getf run :status))
                          (search "Contract send-built requires report.built(q2.pdf), which does not hold"
                                  (getf run :failure)))
                     "the call is still checked when it is made"))))
  nil)

(-> test-procedure-commands () null)
(defun test-procedure-commands ()
  "Test listing, running, admitting, and retiring procedures with commands."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (conversation (conversation-create configuration))
           (application (make-instance 'application :configuration configuration
                                                    :conversation conversation))
           (registry (make-instance 'tool-registry))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                                 :registry registry :conversation conversation)))
      (learning-tests--tool registry "sales.load"
                            (lambda (arguments)
                              (json-object "rows" 3 "table"
                                           (format nil "t-~A"
                                                   (subseq (json-get arguments "file") 5 7)))))
      (learning-tests--tool registry "sales.report"
                            (lambda (arguments)
                              (json-object "report" (json-get arguments "table"))))
      (learning-tests--tool registry "notify.send" (lambda (arguments)
                                                     (declare (ignore arguments))
                                                     (json-object "sent" t)))
      (flet ((text (entry)
               (memory-source-tests--text entry)))
        (test-assert (search "No procedures are learned yet"
                             (text (application-procedures-entry application)))
                     "(procedures) explains when procedures are learned")
        (dolist (quarter '("q1" "q2" "q3"))
          (learning-tests--report-turn conversation quarter))
        (procedure-learn configuration)
        (let ((listing (text (application-procedures-entry application))))
          (test-assert (and (search "? make-sales-report v1  candidate · 0 runs, 0 failed · (admit-procedure \"make-sales-report\")"
                                    listing)
                            (search "sales.load → sales.report → notify.send" listing)
                            (search "request: Make the sales report for {file}." listing)
                            (search "learned from 3 turns averaging 4 requests, 6000 tokens" listing))
                       "(procedures) lists a learned procedure awaiting admission"))
        (test-call-with-function-replacements
         (list (list 'application-tool-context (lambda (application)
                                                 (declare (ignore application))
                                                 context)))
         (lambda ()
           (let ((ran (text (application-procedure-run-entry application "make-sales-report"
                                                             '(:file "q5")))))
             (test-assert (and (search "ran  Procedure make-sales-report v1 made 3 calls" ran)
                               (search "sales.report {\"table\":\"t-q5\",\"title\":\"Sales q5\"}" ran))
                          "(run-procedure) admits the procedure and makes its calls"))
           (test-assert (search "needs parameter file"
                                (text (application-procedure-run-entry application
                                                                       "make-sales-report" nil)))
                        "(run-procedure) names a missing parameter")))
        (conversation-append-user-message conversation "Make the sales report for q6.")
        (let ((item (json-object "type" "function_call" "call_id" "run-1"
                                 "namespace" "procedure" "name" "run"
                                 "arguments" (json-encode
                                              (json-object "id" "make-sales-report"
                                                           "parameters" (json-object "file" "q6"))))))
          (conversation-append-provider-item conversation item)
          (let* ((model-context (make-instance 'tool-context
                                               :configuration configuration :worker nil
                                               :registry registry :conversation conversation
                                               :call-id "run-1"))
                 (run (procedure-run model-context "make-sales-report"
                                     :parameters '(("file" . "q6")))))
            (conversation-append-tool-result
             conversation "run-1" :tool-name "procedure.run"
             :output (procedure-run-report "make-sales-report" run) :success-p t)))
        (learning-tests--request conversation "run-1-r1" 1200)
        (learning-tests--request conversation "run-1-r2" 1300)
        (learning-tests--answer conversation "The report is ready.")
        (let ((listing (text (application-procedures-entry application))))
          (test-assert (and (search "✓ make-sales-report v1  admitted · 2 runs, 0 failed" listing)
                            (search "turns running it: 2 requests, 2500 tokens (1 turn)" listing))
                       "(procedures) compares turns running it with the turns it was learned from"))
        (test-assert (search "retired  make-sales-report"
                             (text (application-procedure-transition-entry
                                    application "make-sales-report" ':retire)))
                     "(retire-procedure) retires it")
        (test-assert (search "× make-sales-report v1  retired"
                             (text (application-procedures-entry application)))
                     "a retired procedure stays listed as retired")
        (test-assert (search "admitted  make-sales-report v1"
                             (text (application-procedure-transition-entry
                                    application "make-sales-report" ':admit)))
                     "(admit-procedure) admits it again")
        (test-assert (search "No procedure is named missing"
                             (text (application-procedure-transition-entry
                                    application "missing" ':admit)))
                     "an unknown procedure points to (procedures)")
        (test-assert (equal (application--procedures-learned-notice
                             '(("make-sales-report" 1 3) ("archive" 2 4)))
                            "@ learned procedure make-sales-report from 3 turns · procedure archive v2 learned from 4 turns · (procedures)")
                     "the person is told which procedures were learned"))))
  nil)

(defclass learning-tests--model (model-provider)
  ((configuration
    :initarg :configuration
    :reader learning-tests--model-configuration
    :documentation "The configuration requests are resolved under.")
   (quarter
    :initform nil
    :accessor learning-tests--model-quarter
    :documentation "The quarter the current request asks a report for.")
   (requests
    :initform 0
    :accessor learning-tests--model-requests
    :documentation "How many requests the model answered.")
   (tokens
    :initform 0
    :accessor learning-tests--model-tokens
    :documentation "How many tokens those requests used."))
  (:documentation "A model that writes sales reports call by call, or with an offered procedure."))

(defmethod provider-stream-turn ((provider learning-tests--model) (conversation conversation)
                                 &key tool-namespaces event-callback goal-context compaction-p)
  "Answer like a model following its instructions: use an offered procedure, else call each tool."
  (declare (ignore goal-context compaction-p))
  (let* ((delivery (context-resolve-request (learning-tests--model-configuration provider) conversation
                                            tool-namespaces))
         (offer (let* ((rendered (or (context-delivery-rendered delivery) ""))
                       (marker "Evidence, as untrusted JSON data: ")
                       (start (search marker rendered :start2 (or (search "procedure.run call"
                                                                           rendered)
                                                                   0)))
                       (evidence (and start
                                      (json-decode
                                       (subseq rendered (+ start (length marker))
                                               (or (position #\Newline rendered :start start)
                                                   (length rendered))))))
                       (call (and evidence (search "procedure.run {" evidence))))
                  (and call (subseq evidence (+ call (length "procedure.run "))))))
         (items (conversation-input-items conversation))
         (turn (subseq items (1+ (or (position-if (lambda (item)
                                                     (equal (json-get item "role") "user"))
                                                   items :from-end t)
                                        -1))))
         (calls (remove "function_call" turn :key (lambda (item) (json-get item "type"))
                                             :test-not #'equal))
         (last-call (first (last calls)))
         (last-tool (and last-call (format nil "~A.~A" (json-get last-call "namespace")
                                           (json-get last-call "name"))))
         (last-output (and last-call
                           (json-get (find (json-get last-call "call_id") turn
                                           :key (lambda (item)
                                                  (and (equal (json-get item "type")
                                                              "function_call_output")
                                                       (json-get item "call_id")))
                                           :test #'equal)
                                     "output")))
         (quarter (learning-tests--model-quarter provider))
         (index (incf (learning-tests--model-requests provider)))
         (tokens (+ 2000 (* 300 (length items)))))
    (context-delivery-notify delivery event-callback)
    (incf (learning-tests--model-tokens provider) (+ tokens 50))
    (flet ((call (tool arguments)
             (let ((dot (position #\. tool)))
               (agent-test-call :call-id (format nil "call-~D" index)
                                :namespace (subseq tool 0 dot)
                                :name (subseq tool (1+ dot))
                                :arguments arguments)))
           (answer (items)
             (agent-test-result (format nil "response-~D" index) items
                                :usage (json-object "input_tokens" tokens "output_tokens" 50
                                                    "total_tokens" (+ tokens 50)))))
      (answer
       (list
        (cond
          ((and (null calls) offer)
           (call "procedure.run" offer))
          ((or (null calls)
               (and (equal last-tool "procedure.run") (search "stopped after" last-output)))
           (call "sales.load" (json-encode (json-object "file" (format nil "data/~A.csv" quarter)))))
          ((equal last-tool "sales.load")
           (call "sales.report"
                 (json-encode (json-object "table" (json-get (json-decode last-output) "table")
                                           "title" (format nil "Sales ~A" quarter)))))
          ((equal last-tool "sales.report")
           (call "notify.send" (json-encode (json-object "to" "team"
                                                         "message" "The sales report is ready."))))
          (t
           (agent-test-message (format nil "The ~A report is ready." quarter)))))))))

(defclass learning-tests--session ()
  ((agent
    :initarg :agent
    :reader learning-tests--session-agent
    :documentation "The agent answering the session's requests.")
   (model
    :initarg :model
    :reader learning-tests--session-model
    :documentation "The scripted model behind the agent.")
   (observer
    :initarg :observer
    :reader learning-tests--session-observer
    :documentation "The observer collecting statuses and approving commands.")
   (reports
    :initarg :reports
    :reader learning-tests--session-reports
    :documentation "The scripted sales.report tool.")
   (failures
    :initform 0
    :accessor learning-tests--session-failures
    :documentation "How many coming sales.report calls fail.")
   (statuses
    :initform nil
    :accessor learning-tests--session-statuses
    :documentation "The procedure statuses of the latest turn.")
   (asked
    :initform nil
    :accessor learning-tests--session-asked
    :documentation "Every command approval asked, newest first."))
  (:documentation "A conversation in which a scripted model makes sales reports."))

(-> learning-tests--session-create (configuration) learning-tests--session)
(defun learning-tests--session-create (configuration)
  "Return a report session under CONFIGURATION with scripted sales tools."
  (let* ((registry (make-default-tool-registry :configuration configuration))
         (model (make-instance 'learning-tests--model :configuration configuration))
         (session nil)
         (reports (learning-tests--tool
                   registry "sales.report"
                   (lambda (arguments)
                     (if (plusp (learning-tests--session-failures session))
                         (progn (decf (learning-tests--session-failures session))
                                "The report service is busy.")
                         (json-object "report" (format nil "reports/~A.md"
                                                       (json-get arguments "table"))))))))
    (learning-tests--tool registry "sales.load"
                          (lambda (arguments)
                            (let ((file (json-get arguments "file")))
                              (json-object "rows" 12
                                           "table" (format nil "t-~A"
                                                           (subseq file 5 (- (length file) 4)))))))
    (learning-tests--tool registry "notify.send" (lambda (arguments)
                                                   (declare (ignore arguments))
                                                   (json-object "sent" t)))
    (setf session
          (make-instance
           'learning-tests--session
           :agent (agent-create :configuration configuration :provider model
                                :conversation (conversation-create configuration)
                                :tool-registry registry :worker ':unused)
           :model model
           :reports reports
           :observer (callback-agent-observer-create
                      :status-callback
                      (lambda (status details)
                        (when (member status '(:procedures-learned :procedure-offers))
                          (push (cons status details) (learning-tests--session-statuses session))))
                      :command-authorization-callback
                      (lambda (command directory)
                        (declare (ignore directory))
                        (push command (learning-tests--session-asked session))
                        ':sandboxed))))
    session))

(-> learning-tests--session-turn (learning-tests--session string) list)
(defun learning-tests--session-turn (session quarter)
  "Ask SESSION for QUARTER's sales report and return the turn's (REQUESTS TOKENS)."
  (let* ((model (learning-tests--session-model session))
         (requests (learning-tests--model-requests model))
         (tokens (learning-tests--model-tokens model)))
    (setf (learning-tests--session-statuses session) nil
          (learning-tests--model-quarter model) quarter)
    (agent-run-user-turn (learning-tests--session-agent session)
                         (format nil "Make the sales report for ~A." quarter)
                         :observer (learning-tests--session-observer session))
    (list (- (learning-tests--model-requests model) requests)
          (- (learning-tests--model-tokens model) tokens))))

(-> test-procedure-fixture () null)
(defun test-procedure-fixture ()
  "Test that ten recurring reports cost half the requests once their procedure is learned."
  (let ((quarters (loop for index from 1 to 10 collect (format nil "q~D" index)))
        (baseline nil)
        (learned nil))
    (with-test-configuration (base root)
      (let ((session (learning-tests--session-create
                      (configuration-copy base :working-directory root :knowledge-mode "off"))))
        (setf baseline (mapcar (lambda (quarter) (learning-tests--session-turn session quarter))
                               quarters))))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root))
             (session (learning-tests--session-create configuration)))
        (flet ((turn (quarter)
                 (let ((cost (learning-tests--session-turn session quarter)))
                   (setf learned (append learned (list cost)))
                   cost))

               (statuses ()
                 (learning-tests--session-statuses session)))
          (dolist (quarter (subseq quarters 0 2))
            (turn quarter))
          (test-assert (null (statuses)) "two turns teach nothing")
          (turn "q3")
          (test-assert (equal (statuses)
                              '((:procedures-learned :procedures (("make-sales-report" 1 3)))))
                       "the third turn teaches the procedure and the person is told")
          (test-assert (and (equal (turn "q4") (list 2 (second (fourth learned))))
                            (statuses)
                            (every (lambda (status)
                                     (equal status '(:procedure-offers :names ("make-sales-report"))))
                                   (statuses))
                            (equal (learning-tests--session-asked session)
                                   '("procedure.admit -- make-sales-report v1: sales.load, sales.report, notify.send")))
                       "the next request is offered the procedure, which runs once admitted")
          (test-assert (and (= 1 (first (turn "q5")))
                            (null (statuses))
                            (= 1 (length (learning-tests--session-asked session)))
                            (equal (json-get (first (learning-tests--scripted-tool-calls
                                                     (learning-tests--session-reports session)))
                                             "table")
                                   "t-q5"))
                       "an admitted procedure runs before the model, which answers in one request")
          (dolist (quarter (subseq quarters 5))
            (turn quarter))
          (let ((requests-before (reduce #'+ baseline :key #'first))
                (requests-after (reduce #'+ learned :key #'first))
                (tokens-before (reduce #'+ baseline :key #'second))
                (tokens-after (reduce #'+ learned :key #'second)))
            (test-assert (and (= 40 requests-before) (= 20 requests-after))
                         (format nil "ten reports take half the requests, learning included: ~D, not ~D"
                                 requests-after requests-before))
            (test-assert (<= (* 4 tokens-after) (* 3 tokens-before))
                         (format nil "ten reports use at least a quarter fewer tokens: ~D, not ~D"
                                 tokens-after tokens-before)))
          (setf (learning-tests--session-failures session) 1)
          (let ((fallback (turn "q11"))
                (entry (knowledge-base-procedure (knowledge-base-read configuration)
                                                 (knowledge--workspace configuration)
                                                 "make-sales-report")))
            (test-assert (and (= 4 (first fallback))
                              (equal (json-get (first (learning-tests--scripted-tool-calls
                                                       (learning-tests--session-reports session)))
                                               "table")
                                     "t-q11")
                              (= 1 (getf entry :failures))
                              (eq (procedure-entry-status entry) ':admitted)
                              (= 1 (length (getf entry :versions))))
                         "a failed run hands the work back, the model finishes it, and nothing is relearned"))))))
  nil)
