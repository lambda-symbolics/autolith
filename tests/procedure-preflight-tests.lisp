(in-package #:autolith)

(-> test-procedure-grounded-preflight () null)
(defun test-procedure-grounded-preflight ()
  "Check each grounded requirement before allowing earlier external side effects."
  (dolist (case '(:permanent-blocker :wrong-binding :unmatched-producer :opposite-producer
                 :unaccepted-effect :accepted-effect :observation :derived
                 :derived-blocker :derived-negation :missing-negation :result-and-blocker
                 :shared-result-binding :producer-blocker :producer-joint-blocker
                 :shared-binding-mismatch :shared-binding-match :ambiguous-requirement
                 :search-exhaustion :combined-ambiguity :planned-opposite :planned-key-conflict))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                   :registry registry
                                                   :conversation (conversation-create configuration)))
             (build (learning-tests--tool registry "report.build"
                                          (lambda (arguments)
                                            (json-object "file" (json-get arguments "file") "id" "a"))))
             (send (learning-tests--tool registry "mail.send"
                                         (lambda (arguments)
                                           (declare (ignore arguments)) (json-object "sent" t))))
             (derived-p (member case '(:derived :derived-blocker :derived-negation :missing-negation)))
             (effect-p (member case '(:opposite-producer :unaccepted-effect :accepted-effect :planned-opposite)))
             (blocked-p (not (member case '(:accepted-effect :observation :derived :derived-negation
                                             :shared-binding-match)))))
        (dolist (predicate '("report.built" "report.ready" "report.allowed" "report.blocked"))
          (knowledge-define-schema configuration
                                   (knowledge-make-schema
                                    :predicate predicate
                                    :arguments '((:name "file" :type :string)))))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate "report.pair"
                                  :arguments '((:name "left" :type :string)
                                               (:name "right" :type :string))))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate "report.owner"
                                  :arguments '((:name "file" :type :string) (:name "owner" :type :string))
                                  :key (when (eq case ':planned-key-conflict) '("file"))))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate "report.authorized"
                                  :arguments '((:name "owner" :type :string))))
        (when (member case '(:producer-joint-blocker :ambiguous-requirement))
          (knowledge-tests--said configuration "report.owner" '("q1" "alice") "Alice owns q1"))
        (when (eq case ':ambiguous-requirement)
          (knowledge-tests--said configuration "report.owner" '("q1" "bob") "Bob also owns q1"))
        (when (member case '(:combined-ambiguity :planned-key-conflict))
          (knowledge-tests--said configuration "report.owner" '("q1" "bob") "Bob owns q1"))
        (when (eq case ':planned-opposite)
          (knowledge-tests--said configuration "report.built" '("q1") "q1 is already built"))
        (when (member case '(:shared-binding-match :shared-binding-mismatch :producer-joint-blocker))
          (knowledge-tests--said configuration "report.authorized"
                                 (list (if (eq case ':shared-binding-match) "alice" "bob"))
                                 "The named owner is authorized"))
        (when derived-p
          (knowledge-propose-rule
           configuration :id "built-ready"
           :premises (append '(("report.built" "?file"))
                             (when (member case '(:derived-negation :missing-negation))
                               '((:not ("report.blocked" "?file")))))
           :conclusion '("report.ready" "?file"))
          (knowledge-admit-rule configuration "built-ready"))
        (when (eq case ':derived-negation)
          (knowledge-tests--said configuration "report.blocked" '("q1") "q1 is not blocked" :negated t))
        (knowledge-define-contract
         configuration :id "build-produces" :tool "report.build"
         :when (json-object "file" "?file" "mode" (if (eq case ':unmatched-producer) "other" "build"))
         :requires (case case
                     (:producer-blocker '(("report.allowed" "?file")))
                     (:producer-joint-blocker '(("report.owner" "?file" "?owner")
                                                ("report.authorized" "?owner"))))
         :effects (when effect-p
                    (if (member case '(:opposite-producer :planned-opposite))
                        #(#("not" #("report.built" "?file")))
                        #(#("report.built" "?file"))))
         :observes (unless effect-p
                     (cond
                       ((eq case ':shared-result-binding)
                        (vector (json-object "predicate" "report.pair"
                                             "arguments" #("$result.id" "$result.id"))))
                       ((member case '(:shared-binding-match :shared-binding-mismatch
                                       :combined-ambiguity :planned-key-conflict))
                        (vector (json-object "predicate" "report.owner"
                                             "arguments" #("$arg.file" "$arg.owner"))))
                       (t (vector (json-object "predicate" "report.built"
                                               "arguments" #("$arg.file")))))))
        (knowledge-define-contract
         configuration :id "send-requires" :tool "mail.send"
         :when (json-object "file" "?file")
         :accepts (when (member case '(:accepted-effect :opposite-producer))
                    '(:observation :user-assertion :model-proposal))
         :requires
         (append
          (cond
            ((eq case ':shared-result-binding) '(("report.pair" "a" "b")))
            ((member case '(:ambiguous-requirement :combined-ambiguity))
             '(("report.owner" "?file" "?owner")))
            ((eq case ':planned-key-conflict) '(("report.owner" "?file" "bob")))
            ((member case '(:shared-binding-match :shared-binding-mismatch))
             '(("report.owner" "?file" "?owner") ("report.authorized" "?owner")))
            (t (list (list (if derived-p "report.ready" "report.built") "?file"))))
          (when (member case '(:permanent-blocker :derived-blocker :result-and-blocker))
            '(("report.allowed" "?file")))))
        (procedure-define
         configuration
         (list :procedure :id "preflight-report"
               :steps
               (list
                (list :tool "report.build"
                      :arguments (list :object
                                       (list "file" (list :json (json-encode
                                                                  (if (eq case ':wrong-binding) "q2" "q1"))))
                                       '("mode" (:json "\"build\""))
                                       '("owner" (:json "\"alice\""))))
                (list :tool "mail.send"
                      :arguments (if (eq case ':result-and-blocker)
                                     '(:object ("file" (:json "\"q1\""))
                                               ("token" (:from 0 ("id"))))
                                     '(:object ("file" (:json "\"q1\""))))))))
        (procedure-admit configuration "preflight-report")
        (let ((outcome (handler-case
                           (let ((*knowledge-step-limit* (if (eq case ':search-exhaustion)
                                                             0 *knowledge-step-limit*)))
                             (procedure-run context "preflight-report"))
                         (knowledge-error (condition) condition))))
          (test-assert
           (if blocked-p
               (and (typep outcome 'knowledge-error)
                    (null (learning-tests--scripted-tool-calls build))
                    (null (learning-tests--scripted-tool-calls send)))
               (and (listp outcome) (eq (getf outcome :status) ':ok)
                    (= 1 (length (learning-tests--scripted-tool-calls build)))
                    (= 1 (length (learning-tests--scripted-tool-calls send)))))
           (format nil "~(~A~): grounded preflight ~A before any unwanted effects"
                   case (if blocked-p "refuses" "permits the dependency")))))))
  nil)


(-> test-procedure-preflight-rule-bindings () null)
(defun test-procedure-preflight-rule-bindings ()
  "Check computed bindings and guards, conditional future values, and read-only preflight."
  (dolist (case '(:computed-success :computed-mismatch :guard-false :future-count :future-guard-false))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                   :registry registry
                                                   :conversation (conversation-create configuration)))
             (count (if (member case '(:guard-false :future-guard-false)) 2 4))
             (amount (case case (:computed-mismatch 6) (:future-guard-false 5) (otherwise (1+ count))))
             (future-p (member case '(:future-count :future-guard-false)))
             (refused-p (member case '(:computed-mismatch :guard-false)))
             (build (learning-tests--tool registry "report.build"
                                          (lambda (arguments)
                                            (json-object "count" (json-get arguments "count")))))
             (send (learning-tests--tool registry "mail.send"
                                         (lambda (arguments)
                                           (declare (ignore arguments)) (json-object "sent" t))))
             (procedure
               (list :procedure :id "capacity-report"
                     :steps (list
                             (list :tool "report.build"
                                   :arguments (list :object '("file" (:json "\"q1\""))
                                                    (list "count" (list :json (json-encode count)))))
                             (list :tool "mail.send"
                                   :arguments (list :object '("file" (:json "\"q1\""))
                                                    (list "amount" (list :json (json-encode amount)))))))))
        (dolist (predicate '("report.measure" "report.capacity"))
          (knowledge-define-schema
           configuration (knowledge-make-schema
                          :predicate predicate
                          :arguments '((:name "file" :type :string) (:name "amount" :type :number)))))
        (knowledge-propose-rule configuration :id "measure-capacity"
                                :premises '(("report.measure" "?file" "?count"))
                                :bindings '(("?next" ("+" "?count" 1)))
                                :guards '((">" "?next" 4))
                                :conclusion '("report.capacity" "?file" "?next"))
        (knowledge-admit-rule configuration "measure-capacity")
        (knowledge-define-contract
         configuration :id "build-measure" :tool "report.build"
         :observes (vector (json-object "predicate" "report.measure"
                                        "arguments" (vector "$arg.file"
                                                            (if future-p "$result.count" "$arg.count")))))
        (knowledge-define-contract configuration :id "send-capacity" :tool "mail.send"
                                   :when (json-object "file" "?file" "amount" "?amount")
                                   :requires '(("report.capacity" "?file" "?amount")))
        (procedure-define configuration procedure)
        (procedure-admit configuration "capacity-report")
        (unless refused-p
          (let ((revision (knowledge-base-revision (knowledge-base-read configuration))))
            (procedure--check-contracts configuration procedure nil)
            (test-assert (and (= revision (knowledge-base-revision (knowledge-base-read configuration)))
                              (null (knowledge-view-holds (knowledge-view-current configuration)
                                                          '("report.measure" "q1" "?n"))))
                         "symbolic dependency proofs do not publish speculative facts")))
        (let ((outcome (handler-case (procedure-run context "capacity-report")
                         (knowledge-error (condition) condition))))
          (test-assert
           (cond
             (refused-p
              (and (typep outcome 'knowledge-error)
                   (null (learning-tests--scripted-tool-calls build))
                   (null (learning-tests--scripted-tool-calls send))))
             ((eq case ':future-guard-false)
              (and (eq (getf outcome :status) ':failed)
                   (= 1 (length (learning-tests--scripted-tool-calls build)))
                   (null (learning-tests--scripted-tool-calls send))))
             (t (and (eq (getf outcome :status) ':ok)
                     (= 1 (length (learning-tests--scripted-tool-calls build)))
                     (= 1 (length (learning-tests--scripted-tool-calls send))))))
           (format nil "~(~A~) respects the computed binding and guard at its earliest decidable check" case))))))
  nil)


(-> test-procedure-preflight-conflict-closure () null)
(defun test-procedure-preflight-conflict-closure ()
  "Refuse inevitable derived and partial-term conflicts without guessing future values."
  (dolist (case '(:derived-opposition :computed-opposition :partial-key-conflict
                 :unknown-owner :unknown-key :unknown-guard :false-guard))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                  :registry registry
                                                  :conversation (conversation-create configuration)))
             (owner-p (member case '(:partial-key-conflict :unknown-owner :unknown-key)))
             (blocked-p (member case '(:derived-opposition :computed-opposition :partial-key-conflict)))
             (build (learning-tests--tool
                     registry "report.build"
                     (lambda (arguments)
                       (declare (ignore arguments))
                       (json-object "file" "q2" "owner" "bob" "id" "old" "count" 2))))
             (send (learning-tests--tool
                    registry "mail.send"
                    (lambda (arguments)
                      (declare (ignore arguments)) (json-object "sent" t)))))
        (knowledge-define-schema
         configuration (knowledge-make-schema :predicate "report.built"
                                              :arguments '((:name "file" :type :string))))
        (dolist (predicate '("report.measure" "report.capacity"))
          (knowledge-define-schema
           configuration (knowledge-make-schema
                          :predicate predicate
                          :arguments '((:name "file" :type :string) (:name "amount" :type :number)))))
        (knowledge-define-schema
         configuration (knowledge-make-schema
                        :predicate "report.owner" :key '("file")
                        :arguments '((:name "file" :type :string) (:name "owner" :type :string)
                                     (:name "token" :type :string))))
        (cond
          (owner-p
           (knowledge-tests--said configuration "report.owner" '("q1" "bob" "old") "Bob owns q1"))
          ((eq case ':computed-opposition)
           (knowledge-tests--said configuration "report.capacity" '("q1" 5)
                                  "q1 does not have capacity five" :negated t))
          (t
           (knowledge-tests--said configuration "report.built" '("q1") "q1 is already built")))
        (unless owner-p
          (knowledge-propose-rule
           configuration :id "measure-consequence"
           :premises '(("report.measure" "?file" "?count"))
           :bindings (when (eq case ':computed-opposition) '(("?next" ("+" "?count" 1))))
           :guards (when (member case '(:unknown-guard :false-guard)) '((">" "?count" 4)))
           :conclusion (if (eq case ':computed-opposition)
                           '("report.capacity" "?file" "?next")
                           '(:not ("report.built" "?file"))))
          (knowledge-admit-rule configuration "measure-consequence"))
        (knowledge-define-contract
         configuration :id "build-observation" :tool "report.build"
         :observes
         (vector (if owner-p
                     (json-object "predicate" "report.owner"
                                  "arguments" (vector (if (eq case ':unknown-key) "$result.file" "q1")
                                                      (if (eq case ':unknown-owner) "$result.owner" "alice")
                                                      "$result.id"))
                     (json-object "predicate" "report.measure"
                                  "arguments" (vector "q1" (if (eq case ':unknown-guard)
                                                              "$result.count" 4))))))
        (knowledge-define-contract
         configuration :id "send-requirement" :tool "mail.send"
         :requires (cond
                     (owner-p '(("report.owner" "q1" "bob" "old")))
                     ((eq case ':computed-opposition) '(("report.capacity" "q1" "?amount")))
                     (t '(("report.built" "q1")))))
        (procedure-define
         configuration
         '(:procedure :id "conflict-report"
           :steps ((:tool "report.build" :arguments (:object))
                   (:tool "mail.send" :arguments (:object)))))
        (procedure-admit configuration "conflict-report")
        (let ((outcome (handler-case (procedure-run context "conflict-report")
                         (knowledge-error (condition) condition))))
          (test-assert
           (if blocked-p
               (and (typep outcome 'knowledge-error)
                    (null (learning-tests--scripted-tool-calls build))
                    (null (learning-tests--scripted-tool-calls send)))
               (and (listp outcome) (eq (getf outcome :status) ':ok)
                    (= 1 (length (learning-tests--scripted-tool-calls build)))
                    (= 1 (length (learning-tests--scripted-tool-calls send)))))
           (format nil "~(~A~): distinguish inevitable conflict from unknown future values" case))))))
  nil)


(-> test-procedure-preflight-shared-future-conflicts () null)
(defun test-procedure-preflight-shared-future-conflicts ()
  "Keep shared future identities through selected bindings and nested rule aliases."
  (dolist (case '(:selected-binding :nested-alias :joint-future-constraints :selected-guard))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                  :registry registry
                                                  :conversation (conversation-create configuration)))
             (build (learning-tests--tool
                     registry "report.build"
                     (lambda (arguments)
                       (declare (ignore arguments)) (json-object "id" "a" "other" "a" "count" 5))))
             (send (learning-tests--tool
                    registry "mail.send"
                    (lambda (arguments)
                      (declare (ignore arguments)) (json-object "sent" t)))))
        (dolist (predicate '("report.ready" "report.seed" "report.anchor" "report.copy"))
          (knowledge-define-schema
           configuration (knowledge-make-schema :predicate predicate
                                                :arguments '((:name "file" :type :string)))))
        (knowledge-define-schema
         configuration (knowledge-make-schema
                        :predicate "report.measure"
                        :arguments '((:name "file" :type :string) (:name "amount" :type :number))))
        (when (eq case ':nested-alias)
          (knowledge-tests--said configuration "report.ready" '("a") "a is ready")
          (knowledge-propose-rule configuration :id "seed-copy"
                                 :premises '(("report.seed" "?file"))
                                 :conclusion '("report.copy" "?file"))
          (knowledge-admit-rule configuration "seed-copy"))
        (knowledge-propose-rule
         configuration :id "future-opposition"
         :premises (case case
                     (:nested-alias '(("report.anchor" "?file") ("report.copy" "?file")))
                     (:joint-future-constraints '(("report.seed" "?file") ("report.anchor" "?file")))
                     (:selected-guard '(("report.measure" "?file" "?amount")))
                     (otherwise '(("report.seed" "?file"))))
         :guards (when (eq case ':selected-guard) '((">" "?amount" 4)))
         :conclusion (if (eq case ':nested-alias)
                         '(:not ("report.ready" "a"))
                         '(:not ("report.ready" "?file"))))
        (knowledge-admit-rule configuration "future-opposition")
        (knowledge-define-contract
         configuration :id "shared-observations" :tool "report.build"
         :observes
         (concatenate
          'vector
          (vector (json-object "predicate" (if (eq case ':nested-alias)
                                               "report.anchor" "report.ready")
                               "arguments" #("$result.id")))
          (case case
            (:joint-future-constraints
             (vector (json-object "predicate" "report.seed" "arguments" #("$result.id"))
                     (json-object "predicate" "report.anchor" "arguments" #("$result.other"))))
            (:selected-guard
             (vector (json-object "predicate" "report.measure"
                                  "arguments" #("$result.id" "$result.count"))))
            (otherwise
             (vector (json-object "predicate" "report.seed" "arguments" #("$result.id")))))))
        (knowledge-define-contract
         configuration :id "send-ready" :tool "mail.send"
         :requires (append '(("report.ready" "a"))
                           (case case
                             (:joint-future-constraints '(("report.anchor" "a")))
                             (:selected-guard '(("report.measure" "a" 5))))))
        (procedure-define
         configuration
         '(:procedure :id "shared-conflict-report"
           :steps ((:tool "report.build" :arguments (:object))
                   (:tool "mail.send" :arguments (:object)))))
        (procedure-admit configuration "shared-conflict-report")
        (let ((outcome (handler-case (procedure-run context "shared-conflict-report")
                         (knowledge-error (condition) condition))))
          (test-assert (and (typep outcome 'knowledge-error)
                            (null (learning-tests--scripted-tool-calls build))
                            (null (learning-tests--scripted-tool-calls send)))
                       (format nil "~(~A~): shared symbolic observations cannot prove an undisputed requirement"
                               case))))))
  nil)


(-> test-procedure-preflight-proof-dependencies () null)
(defun test-procedure-preflight-proof-dependencies ()
  "Revalidate every selected proof premise and rule after joining future bindings."
  (dolist (case '(:derived-premise :current-derived-premise :clean-alternative
                 :selected-false-guard :selected-computation-mismatch))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                  :registry registry
                                                  :conversation (conversation-create configuration)))
             (numeric-p (member case '(:selected-false-guard :selected-computation-mismatch)))
             (build (learning-tests--tool
                     registry "report.build"
                     (lambda (arguments)
                       (declare (ignore arguments)) (json-object "id" "a" "other" "a" "count" 2))))
             (send (learning-tests--tool
                    registry "mail.send"
                    (lambda (arguments)
                      (declare (ignore arguments)) (json-object "sent" t)))))
        (dolist (predicate '("report.ready" "report.seed" "report.anchor"))
          (knowledge-define-schema
           configuration (knowledge-make-schema :predicate predicate
                                                :arguments '((:name "file" :type :string)))))
        (dolist (predicate '("report.measure" "report.capacity"))
          (knowledge-define-schema
           configuration (knowledge-make-schema
                          :predicate predicate
                          :arguments '((:name "file" :type :string) (:name "amount" :type :number)))))
        (when (member case '(:current-derived-premise :clean-alternative))
          (knowledge-tests--said configuration "report.seed" '("a") "a is seeded"))
        (when (eq case ':clean-alternative)
          (knowledge-tests--said configuration "report.ready" '("a") "a is independently ready"))
        (knowledge-propose-rule
         configuration :id "derive-requirement"
         :premises (if numeric-p '(("report.measure" "?file" "?count")) '(("report.seed" "?file")))
         :bindings (when (eq case ':selected-computation-mismatch) '(("?next" ("+" "?count" 1))))
         :guards (when (eq case ':selected-false-guard) '((">" "?count" 4)))
         :conclusion (if (eq case ':selected-computation-mismatch)
                         '("report.capacity" "?file" "?next") '("report.ready" "?file")))
        (knowledge-admit-rule configuration "derive-requirement")
        (unless numeric-p
          (knowledge-propose-rule configuration :id "anchor-opposition"
                                 :premises '(("report.anchor" "?file"))
                                 :conclusion '(:not ("report.seed" "?file")))
          (knowledge-admit-rule configuration "anchor-opposition"))
        (knowledge-define-contract
         configuration :id "dependency-observations" :tool "report.build"
         :observes (if numeric-p
                       (vector (json-object "predicate" "report.measure"
                                            "arguments" #("$result.id" "$result.count")))
                       (concatenate
                        'vector
                        (when (eq case ':derived-premise)
                          (vector (json-object "predicate" "report.seed" "arguments" #("$result.id"))))
                        (vector (json-object "predicate" "report.anchor" "arguments" #("$result.other"))))))
        (knowledge-define-contract
         configuration :id "require-derived" :tool "mail.send"
         :requires (append (if (eq case ':selected-computation-mismatch)
                               '(("report.capacity" "a" 5)) '(("report.ready" "a")))
                           (if numeric-p '(("report.measure" "a" 2)) '(("report.anchor" "a")))))
        (procedure-define
         configuration
         '(:procedure :id "dependency-report"
           :steps ((:tool "report.build" :arguments (:object))
                   (:tool "mail.send" :arguments (:object)))))
        (procedure-admit configuration "dependency-report")
        (let ((outcome (handler-case (procedure-run context "dependency-report")
                         (knowledge-error (condition) condition))))
          (test-assert
           (if (eq case ':clean-alternative)
               (and (listp outcome) (eq (getf outcome :status) ':ok)
                    (= 1 (length (learning-tests--scripted-tool-calls build)))
                    (= 1 (length (learning-tests--scripted-tool-calls send))))
               (and (typep outcome 'knowledge-error)
                    (null (learning-tests--scripted-tool-calls build))
                    (null (learning-tests--scripted-tool-calls send))))
           (format nil "~(~A~): final bindings must preserve a complete admissible proof" case))))))
  nil)
