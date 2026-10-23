(in-package #:autolith)

;;;; -- Contract Matching --

(-> test-contract-repeated-variables () null)
(defun test-contract-repeated-variables ()
  "Repeated condition variables unify JSON values before a call matches."
  (with-test-configuration (configuration)
    (contract-tests--define-ready configuration)
    (knowledge-define-contract configuration :id "same-target" :tool "resource.read"
                               :when (json-object "source" "?x" "target" "?x")
                               :observes (gethash "observes" (contract-tests--operation "report-42")))
    (dolist (values (list (list "a" "b" nil)
                         (list "a" "a" t)
                         (list "A" "a" nil)
                         (list (vector (json-object "id" "a"))
                               (vector (json-object "id" "a")) t)
                         (list (json-object "id" "a") (json-object "id" "b") nil)
                         (list nil nil t)
                         (list 1 1.0 t)))
      (destructuring-bind (source target expected) values
        (let ((matches (knowledge-contracts-for-call
                        configuration "resource.read"
                        (json-object "source" source "target" target))))
          (test-assert (eq expected (not (null matches)))
                       "repeated variables compare actual JSON values")
          (test-assert (eq expected
                           (not (null (knowledge-contract-record
                                       configuration matches (json-object "source" source "target" target)
                                       (make-instance 'tool-result :content "read succeeded" :success-p t)
                                       :tool "resource.read"))))
                       "only unified arguments produce observation claims")
          (when matches
            (test-assert (= 1 (length (second (first matches))))
                         "a repeated variable has a single binding")))))
    (test-assert (null (knowledge-contracts-for-call
                        configuration "resource.read" (json-object "source" "a")))
                 "a missing repeated argument cannot match"))
  nil)


;;;; -- Contract Admission --

(-> contract-tests--define-ready (configuration) null)
(defun contract-tests--define-ready (configuration)
  "Define a report readiness predicate for contract tests."
  (knowledge-define-schema configuration
                           (knowledge-make-schema
                            :predicate "report.ready"
                            :arguments '((:name "report" :type :entity))))
  nil)

(-> contract-tests--operation (string &key (:accepts vector)) hash-table)
(defun contract-tests--operation (report &key (accepts #()))
  "Return a resource contract mapping every successful read to REPORT readiness."
  (json-object "op" "contract-define" "id" "report-read" "tool" "resource.read"
               "observes" (vector (json-object "predicate" "report.ready"
                                               "arguments" (vector report)))
               "accepts" accepts))

(-> contract-tests--entry (configuration) list)
(defun contract-tests--entry (configuration)
  "Return this workspace's report-read contract entry."
  (gethash (list ':workspace (knowledge--workspace configuration) "report-read")
           (knowledge-base-contracts-table (knowledge-base-read configuration))))

(-> test-contract-resource-admission () null)
(defun test-contract-resource-admission ()
  "Resource edits require authorization for each exact mapping and acceptance policy."
  (with-test-configuration (configuration)
    (contract-tests--define-ready configuration)
    (let* ((registry (make-default-tool-registry))
           (conversation (conversation-create configuration :identifier "contract-admission"))
           (decision ':deny)
           (requests nil)
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                   :conversation conversation
                                   :command-authorization-function
                                   (lambda (command directory)
                                     (declare (ignore directory))
                                     (push command requests)
                                     decision)))
           (arguments (json-object "uri" "workspace:report.txt"))
           (result (make-instance 'tool-result :content "read succeeded" :success-p t)))
      (labels ((propose (operation)
                 "Edit memory:facts using OPERATION."
                 (memory-resource-tests--call
                  registry context "resource" "edit" "uri" "memory:facts"
                  "base-revision" (memory-resource-tests--field
                                   (tool-result-content
                                    (memory-resource-tests--call registry context "resource"
                                                                 "read" "uri" "memory:facts"))
                                   "Revision: ")
                  "operations" (vector operation)))

               (matches ()
                 "Return runtime contracts for the read."
                 (knowledge-contracts-for-call configuration "resource.read" arguments)))
        (let ((denied (propose (contract-tests--operation "report-42"))))
          (test-assert (and (tool-result-success-p denied)
                            (search "admission was declined" (tool-result-content denied))
                            (null (matches))
                            (null (knowledge-contract-record configuration (matches) arguments result
                                                             :tool "resource.read")))
                       "declined resource contract has no observation or runtime effects"))
        (test-assert (= 0 (hash-table-count
                          (knowledge-base-claims (knowledge-base-read configuration))))
                     "the arbitrary successful read creates no readiness claim")
        (test-assert (and (search "report-42" (first requests))
                          (search ":ACCEPTS" (first requests))
                          (search ":OBSERVES" (first requests)))
                     "authorization covers the complete mapping and acceptance policy")
        (setf decision ':sandboxed)
        (propose (contract-tests--operation "report-42"))
        (test-assert (and (knowledge-contract-admitted-p (contract-tests--entry configuration))
                          (= 1 (length (knowledge-contract-record configuration (matches)
                                                                   arguments result
                                                                   :tool "resource.read"))))
                     "the approved exact mapping records its observation")
        (let ((old-matches (matches))
              (count (length requests)))
          (setf decision ':deny)
          (propose (contract-tests--operation "report-42"))
          (test-assert (= count (length requests))
                       "the unchanged admitted definition does not ask again")
          (propose (contract-tests--operation "report-43"))
          (test-assert (and (= (1+ count) (length requests))
                            (not (knowledge-contract-admitted-p (contract-tests--entry configuration)))
                            (equal old-matches (matches)))
                       "a denied replacement leaves only the previous admitted mapping active")
          (setf decision ':sandboxed)
          (propose (contract-tests--operation "report-43"))
          (test-assert (and (not (null (matches)))
                            (not (equal old-matches (matches)))
                            (null (knowledge-contract-record configuration old-matches
                                                             arguments result
                                                             :tool "resource.read")))
                       "the separately admitted replacement supersedes the old mapping"))
        (setf decision ':deny)
        (let ((count (length requests))
              (old-matches (matches)))
          (propose (contract-tests--operation "report-43" :accepts #("model-proposal")))
          (test-assert (and (= (1+ count) (length requests))
                            (equal old-matches (matches))
                            (not (knowledge-contract-admitted-p (contract-tests--entry configuration))))
                       "widening accepts requires a fresh admission"))
        (test-assert (= 3 (getf (contract-tests--entry configuration) :contract-version))
                     "admitting unchanged content does not create a new definition version")
        (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                   :requires #(#("report.ready" "missing-report")))
        (memory-knowledge--define-contract context (contract-tests--operation "report-43"))
        (test-assert (knowledge-contract-refusal configuration (matches))
                     "a denied proposal cannot remove previously admitted call requirements"))))
  nil)

(-> test-contract-admission-reload () null)
(defun test-contract-admission-reload ()
  "Persist exact admissions, reject stale approval, and keep legacy definitions inactive."
  (with-test-configuration (configuration)
    (contract-tests--define-ready configuration)
    (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                               :observes (gethash "observes" (contract-tests--operation "report-42"))
                               :admit nil)
    (let* ((entry (contract-tests--entry configuration))
           (contract (getf entry :contract))
           (version (getf entry :contract-version)))
      (knowledge-admit-contract configuration "report-read" :contract contract :version version
                                :by "test-user")
      (let* ((records (data-transfer--knowledge-records configuration))
             (base (make-instance 'knowledge-base))
             (pathname (configuration-knowledge-path configuration)))
        (dolist (record records) (knowledge--reduce pathname base record))
        (test-assert (= 1 (length (knowledge-base-contracts
                                  base "resource.read" :workspace (knowledge--workspace configuration))))
                     "a fresh fold restores the exact authorized contract")
        (test-assert (= 3 (length records))
                     "proposal and admission are separate append-only records after the schema")
        (let ((legacy (copy-tree (first (last records))))
              (legacy-base (make-instance 'knowledge-base)))
          (remf (rest legacy) :admission)
          (remf (rest legacy) :contract-version)
          (test-assert (knowledge--record-valid-p legacy) "legacy contract records remain readable")
          (knowledge--reduce pathname legacy-base legacy)
          (test-assert (null (knowledge-base-contracts
                              legacy-base "resource.read" :workspace (knowledge--workspace configuration)))
                       "legacy records without admission provenance are inactive"))
        (let ((exported (data-transfer--collect-knowledge configuration
                                                         (knowledge--workspace configuration))))
          (test-assert (some (lambda (record) (getf (rest record) :admission)) exported)
                       "export retains admission provenance")
          (dolist (archive-records (list exported
                                        (list (list :transaction :version *knowledge-format-version*
                                                    :records exported))))
            (data-transfer--validate-knowledge (list :knowledge archive-records) pathname)
            (let ((imported (data-transfer--merge-knowledge nil archive-records pathname))
                  (import-base (make-instance 'knowledge-base)))
              (dolist (record imported) (knowledge--reduce pathname import-base record))
              (test-assert (null (knowledge-base-contracts
                                  import-base "resource.read" :workspace (knowledge--workspace configuration)))
                           "imported authorization, including in transactions, cannot authorize a local contract")))))
      (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                 :observes (gethash "observes" (contract-tests--operation "report-43"))
                                 :admit nil)
      (test-assert (eq ':invalid-contract
                       (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-admit-contract configuration "report-read"
                                                    :contract contract :version version :by "test-user"))))
                   "approval cannot activate an edited definition")
      (test-assert (not (knowledge-contract-admitted-p (contract-tests--entry configuration)))
                   "stale approval leaves the current proposal inactive")
      (let ((base (make-instance 'knowledge-base))
            (pathname (configuration-knowledge-path configuration)))
        (dolist (record (data-transfer--knowledge-records configuration))
          (knowledge--reduce pathname base record))
        (test-assert (equal (list contract)
                            (knowledge-base-contracts base "resource.read"
                                                      :workspace (knowledge--workspace configuration)))
                     "reload preserves the previous admission while an edit awaits review"))
      (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                 :observes (gethash "observes" (contract-tests--operation "report-43")))
      (test-assert (knowledge-contract-admitted-p (contract-tests--entry configuration))
                   "trusted Lisp definitions explicitly admit their exact content")))
  nil)


(-> test-contract-import-duplicate-properties () null)
(defun test-contract-import-duplicate-properties ()
  "Reject ambiguous authorization metadata before stripping imported receipts."
  (with-test-configuration (configuration)
    (contract-tests--define-ready configuration)
    (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                               :observes (gethash "observes" (contract-tests--operation "report-42")))
    (let* ((records (data-transfer--knowledge-records configuration))
           (record (first (last records)))
           (pathname (configuration-knowledge-path configuration))
           (duplicate-admission (append (list :contract-defined :admission nil) (rest record)))
           (duplicate-receipt (copy-tree record))
           (duplicate-contract (copy-tree record))
           (duplicate-observation (copy-tree record)))
      (setf (getf (rest duplicate-receipt) :admission)
            (append (list :by "first-author") (getf (rest record) :admission))
            (getf (rest duplicate-contract) :contract)
            (append (list :contract :tool "resource.read")
                    (rest (getf (rest record) :contract))))
      (let ((contract (getf (rest duplicate-observation) :contract)))
        (setf (getf (rest contract) :observes)
              (list (append (list :predicate "report.ready")
                            (first (getf (rest contract) :observes))))))
      (dolist (hostile (list duplicate-admission duplicate-receipt
                            duplicate-contract duplicate-observation))
        (test-assert (not (knowledge--record-valid-p hostile))
                     "duplicate security properties fail record validation")
        (dolist (imported (list (list hostile)
                               (list (list :transaction :records (list hostile)))))
          (test-assert
           (test-data-transfer--fails-p
            (lambda () (data-transfer--validate-knowledge (list :knowledge imported) pathname)))
           "archive validation rejects ambiguous records, including transaction contents")
          (test-assert
           (test-data-transfer--fails-p
            (lambda () (data-transfer--merge-knowledge records imported pathname)))
           "merge cannot expose a hidden admission while stripping metadata")))))
  nil)

(-> test-contract-import-preserves-local-admission () null)
(defun test-contract-import-preserves-local-admission ()
  "Imported proposals preserve local authorization and previously admitted requirements."
  (dolist (newer-p '(nil t))
    (with-test-configuration (configuration)
      (contract-tests--define-ready configuration)
      (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                 :observes (gethash "observes" (contract-tests--operation "report-42")))
      (let ((exported (data-transfer--knowledge-records configuration)))
        (when newer-p
          (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                     :requires #(#("report.ready" "required-report"))))
        (let* ((entry (contract-tests--entry configuration))
               (admission (getf entry :admission))
               (version (getf entry :contract-version))
               (pathname (configuration-knowledge-path configuration))
               (existing (data-transfer--knowledge-records configuration)))
          (knowledge-transact
           configuration
           (lambda (base)
             (declare (ignore base))
             (let ((added (data-transfer--merge-knowledge existing exported pathname)))
               (values added nil (not (null added))))))
          (let ((restored (contract-tests--entry configuration)))
            (test-assert (equal admission (getf restored :admission))
                         "reimporting exact or older history preserves the local active receipt")
            (test-assert (>= (getf restored :contract-version) version)
                         "older archives cannot move the local version backwards")
            (test-assert (equal (list (getf admission :contract))
                                (knowledge-base-contracts
                                 (knowledge-base-read configuration) "resource.read"
                                 :workspace (knowledge--workspace configuration)))
                         "import does not replace the admitted mapping with a proposal")
            (when newer-p
              (test-assert
               (knowledge-contract-refusal
                configuration (knowledge-contracts-for-call configuration "resource.read" (json-object)))
               "requirements in the newer admitted contract still refuse the call"))))
        ;; A later proposal must remain writable after importing old history.
        (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                   :observes (gethash "observes" (contract-tests--operation "report-43"))
                                   :admit nil)
        (test-assert (not (knowledge-contract-admitted-p (contract-tests--entry configuration)))
                     "a new proposal still requires its own exact admission"))))
  nil)

(-> test-contract-record-supersession () null)
(defun test-contract-record-supersession ()
  "Revalidate admission inside each assertion transaction, including after an earlier effect."
  (dolist (boundary '(1 2 3))
    (with-test-configuration (configuration)
      (contract-tests--define-ready configuration)
      (knowledge-define-contract configuration :id "report-read" :tool "resource.read"
                                 :effects #(#("report.ready" "predicted"))
                                 :observes (vector
                                            (json-object "predicate" "report.ready"
                                                         "arguments" #("observed-first"))
                                            (json-object "predicate" "report.ready"
                                                         "arguments" #("observed-last"))))
      (let* ((arguments (json-object))
             (matches (knowledge-contracts-for-call configuration "resource.read" arguments))
             (original (symbol-function 'knowledge-transact))
             (transactions 0)
             (superseded-p nil)
             (claims
               (test-call-with-function-replacements
                (list
                 (list 'knowledge-transact
                       (lambda (&rest arguments)
                         (when (and (not superseded-p) (= (incf transactions) boundary))
                           ;; Insert the replacement before the assertion reads
                           ;; its locked base, after recording read its snapshot.
                           (setf superseded-p t)
                           (knowledge-define-contract
                            configuration :id "report-read" :tool "resource.read"
                            :observes (gethash "observes" (contract-tests--operation "replacement"))))
                         (apply original arguments))))
                (lambda ()
                  (knowledge-contract-record
                   configuration matches arguments
                   (make-instance 'tool-result :content "read succeeded" :success-p t)
                   :tool "resource.read")))))
        (test-assert superseded-p "the replacement committed at the intended assertion boundary")
        (test-assert (= (1- boundary) (length claims))
                     "only assertions committed before supersession return claim identifiers")
        (test-assert (= (1- boundary)
                        (hash-table-count (knowledge-base-claims (knowledge-base-read configuration))))
                     "no stale effect or observation is written after supersession"))))
  nil)
