(in-package #:autolith)

;;;; -- Structured Knowledge Resources --

;;; Structured knowledge is read and written through memory: resources, so the
;;; model meets one memory system. memory:facts lists the facts holding in this
;;; workspace and accepts fact, predicate, and rule operations; memory:claim/ID
;;; explains one claim; memory:predicates and memory:rules list the vocabulary
;;; and the rules. Every read evaluates locally and costs no model request.

(defparameter *memory-knowledge-default-results* 16
  "The facts one memory:facts read lists unless max-results asks for more.")

(defparameter *memory-knowledge-maximum-results* 50
  "The most facts one memory:facts read lists.")

(defclass memory-knowledge-resource (memory-resource)
  ((view-kind
    :initarg       :view-kind
    :reader        memory-knowledge-resource-view-kind
    :type          keyword
    :documentation "Which knowledge view: :FACTS, :CLAIM, :PREDICATES, :RULES, :METHODS, :CONTRACTS, :PROCEDURES, or :CHANGES.")
   (claim
    :initarg       :claim
    :initform      nil
    :reader        memory-knowledge-resource-claim
    :type          (option string)
    :documentation "The claim a :CLAIM resource explains."))
  (:documentation "A structured-knowledge view within the memory: scheme."))


;;;; -- Resolution --

(-> memory-knowledge-resolve (string) (option memory-knowledge-resource))
(defun memory-knowledge-resolve (identifier)
  "Return the knowledge resource memory:IDENTIFIER names, or NIL for another kind."
  (flet ((make (kind &optional claim)
           (make-instance 'memory-knowledge-resource
                          :uri        (format nil "memory:~A" identifier)
                          :identifier identifier
                          :view-kind  kind
                          :claim      claim)))
    (cond
      ((string= identifier "facts")
       (make ':facts))
      ((string= identifier "predicates")
       (make ':predicates))
      ((string= identifier "rules")
       (make ':rules))
      ((string= identifier "changes")
       (make ':changes))
      ((string= identifier "methods")
       (make ':methods))
      ((string= identifier "contracts")
       (make ':contracts))
      ((string= identifier "procedures")
       (make ':procedures))
      ((and (uiop:string-prefix-p "claim/" identifier)
            (> (length identifier) (length "claim/")))
       (make ':claim (subseq identifier (length "claim/"))))
      (t
       nil))))

(defmethod resource-capabilities
    ((resource memory-knowledge-resource) (context tool-context))
  "Accept edits only on memory:facts."
  (declare (ignore context))
  (if (eq (memory-knowledge-resource-view-kind resource) ':facts)
      '(:read :edit)
      '(:read)))


;;;; -- Evaluation --

(-> memory-knowledge-view (tool-context &key (:goals list)) knowledge-view)
(defun memory-knowledge-view (context &key goals)
  "Return CONTEXT's structured knowledge evaluated from its workspace now."
  (let ((configuration (tool-context-configuration context)))
    (if goals
        (knowledge-evaluate (knowledge-base-read configuration)
                            :workspace (knowledge--workspace configuration)
                            :check     (evidence-check-create
                                        :memories (memory-list configuration :visibility ':all))
                            :goals     goals)
        (knowledge-view-current configuration))))


;;;; -- Observation --

(-> memory-knowledge-observe
    (memory-knowledge-resource tool-context
     &key (:query (option string)) (:maximum-results (option integer)))
    memory-observation)
(defun memory-knowledge-observe (resource context &key query maximum-results)
  "Return RESOURCE's current rendering as a revisioned memory observation."
  (let* ((configuration (tool-context-configuration context))
         (base          (knowledge-base-read configuration))
         (workspace     (knowledge--workspace configuration))
         (kind          (memory-knowledge-resource-view-kind resource))
         (content
           (ecase kind
             (:facts
              (memory-knowledge--render-facts
               (memory-knowledge-view context)
               :query           query
               :maximum-results (min (or maximum-results *memory-knowledge-default-results*)
                                     *memory-knowledge-maximum-results*)))
             (:claim
              (memory-knowledge--render-claim (memory-knowledge-view context)
                                              (memory-knowledge-resource-claim resource)))
             (:predicates
              (memory-knowledge--render-predicates base workspace))
             (:rules
              (memory-knowledge--render-rules base workspace))
             (:methods
              (memory-knowledge--render-methods base workspace))
             (:contracts
              (memory-knowledge--render-contracts base workspace))
             (:procedures
              (memory-knowledge--render-procedures base workspace))
             (:changes
              (memory-knowledge--render-changes
               configuration
               (format nil "conversation:~A"
                       (conversation-identifier (tool-context-conversation context)))
               (memory-knowledge-view context)))))
         (snapshot      (list :kind       kind
                              :identifier (memory-resource-identifier resource)
                              :workspace  workspace
                              :revision   (knowledge-base-revision base)
                              :content    content)))
    (make-instance 'memory-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *memory-resource-digest-key* snapshot)
                   :content    content
                   :metadata   (list :knowledge kind :query query
                                     :maximum-results maximum-results)
                   :identifier (memory-resource-identifier resource)
                   :kind       kind
                   :snapshot   snapshot)))

(defmethod resource-observe
    ((resource memory-knowledge-resource) (context tool-context))
  "Observe one structured-knowledge view."
  (memory-knowledge-observe resource context))

(defmethod resource-tool-read
    ((resource memory-knowledge-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one structured-knowledge view, with an optional query on memory:facts."
  (declare (ignore tool))
  (let ((query   (tool-argument arguments "query"))
        (maximum (tool-argument arguments "max-results")))
    (when (or (nth-value 1 (gethash "start-line" arguments))
              (nth-value 1 (gethash "line-count" arguments)))
      (memory-resource--fail "memory: knowledge views are read in full and do not accept line windows."))
    (when (and (or query maximum)
               (not (eq (memory-knowledge-resource-view-kind resource) ':facts)))
      (memory-resource--fail "Only memory:facts accepts query and max-results."))
    (when (and query (not (non-empty-string-p query)))
      (memory-resource--fail "The query must be non-empty text."))
    (when (and maximum (not (and (integerp maximum) (plusp maximum))))
      (memory-resource--fail "max-results must be a positive integer."))
    (handler-case
        (tool-success
         (resource-item-read-result
          (resource-observation-state-ensure
           (tool-context-conversation context)
           (memory-knowledge-observe resource context
                                     :query query :maximum-results maximum))))
      (knowledge-error (condition)
        (memory-resource--fail "~A" (autolith-error-message condition))))))


;;;; -- Operations --

(defparameter *memory-knowledge-operation-limit* 16
  "The most operations one memory:facts edit applies.")

(defparameter *memory-knowledge-type-names*
  '("entity" "string" "number" "boolean" "instant" "date" "duration" "quantity")
  "The argument type names a predicate definition accepts.")

(-> memory-knowledge--fields
    (hash-table string &key (:allowed list) (:required list))
    null)
(defun memory-knowledge--fields (operation name &key allowed required)
  "Require OPERATION, a NAME operation, to use only ALLOWED and all REQUIRED fields."
  (loop for field being the hash-keys of operation
        unless (member field allowed :test #'string=)
          do (memory-resource--fail "~A does not accept field ~A; it accepts ~{~A~^, ~}."
                                    name field allowed))
  (dolist (field required)
    (unless (nth-value 1 (gethash field operation))
      (memory-resource--fail "~A requires ~A." name field)))
  nil)

(-> memory-knowledge--scope (hash-table) memory-scope)
(defun memory-knowledge--scope (operation)
  "Return OPERATION's scope, workspace unless it names global."
  (let ((scope (gethash "scope" operation "workspace")))
    (cond
      ((equal scope "workspace")
       ':workspace)
      ((equal scope "global")
       ':global)
      (t
       (memory-resource--fail "scope must be workspace or global.")))))

(-> memory-knowledge--list (t string) list)
(defun memory-knowledge--list (value field)
  "Return JSON array VALUE of FIELD as a list."
  (unless (vectorp value)
    (memory-resource--fail "~A must be an array." field))
  (coerce value 'list))

(-> memory-knowledge--define (configuration hash-table) string)
(defun memory-knowledge--define (configuration operation)
  "Apply one predicate-define OPERATION and describe the result."
  (memory-knowledge--fields operation "predicate-define"
                            :allowed  '("op" "predicate" "arguments" "key" "template" "description" "scope")
                            :required '("predicate" "arguments"))
  (let ((schema
          (knowledge-make-schema
           :predicate   (gethash "predicate" operation)
           :arguments
           (mapcar (lambda (argument)
                     (unless (hash-table-p argument)
                       (memory-resource--fail "Each argument is an object with name and type."))
                     (let ((type (gethash "type" argument)))
                       (unless (member type *memory-knowledge-type-names* :test #'equal)
                         (memory-resource--fail "Argument type ~S must be one of ~{~A~^, ~}."
                                                type *memory-knowledge-type-names*))
                       (append (list :name (gethash "name" argument)
                                     :type (intern (string-upcase type) :keyword))
                               (when (gethash "unit" argument)
                                 (list :unit (gethash "unit" argument))))))
                   (memory-knowledge--list (gethash "arguments" operation) "arguments"))
           :key         (and (gethash "key" operation)
                             (memory-knowledge--list (gethash "key" operation) "key"))
           :template    (gethash "template" operation)
           :description (gethash "description" operation))))
    (multiple-value-bind (schema version)
        (knowledge-define-schema configuration schema
                                 :scope (memory-knowledge--scope operation))
      (format nil "Defined ~A version ~D." (knowledge-schema-predicate schema) version))))

(-> memory-knowledge--evidence (tool-context list) list)
(defun memory-knowledge--evidence (context source)
  "Return knowledge evidence for SOURCE, a checked memory source record."
  (let ((properties (rest source))
        (workspace  (knowledge--workspace (tool-context-configuration context))))
    (ecase (first source)
      (:file
       (let* ((pathname (uiop:parse-native-namestring (getf properties :path)))
              (relative (uiop:subpathp pathname (uiop:ensure-directory-pathname workspace))))
         (knowledge-observe :source    (list :file
                                             :path (if relative
                                                       (uiop:unix-namestring relative)
                                                       (getf properties :path))
                                             :relative (and relative t)
                                             :quote    (getf properties :quote))
                            :workspace (and relative workspace)
                            :digest    (getf properties :digest)
                            :excerpt   (getf properties :quote)
                            :adapter   "file/1"
                            :time      (getf properties :observed-at))))
      (:url
       (let ((receipt (memory-knowledge--url-receipt
                       (tool-context-conversation context)
                       (getf properties :url) (getf properties :quote)
                       :configuration (tool-context-configuration context))))
         (knowledge-observe
          :source  (append (list :url       :url (getf properties :url)
                                     :quote (getf properties :quote))
                           (when receipt (list :retrieval "web_extra.gist")))
          :digest  (getf receipt :result-digest)
          :excerpt (getf properties :quote)
          :adapter (if receipt "web-retrieval/1" "web-proposal/1")
          :time    (if receipt (getf receipt :observed-at) (getf properties :observed-at)))))
      (:user
       (knowledge-observe :source  (list :user         :quote (getf properties :quote)
                                         :conversation (getf properties :conversation))
                          :excerpt (getf properties :quote)
                          :adapter "user/1"
                          :time    (getf properties :observed-at))))))

(-> memory-knowledge--time (hash-table string) (option timestamp))
(defun memory-knowledge--time (operation field)
  "Return OPERATION's optional instant FIELD as a universal time."
  (let ((value (gethash field operation)))
    (and value
         (or (and (stringp value) (knowledge-parse-instant value))
             (memory-resource--fail "~A must be an ISO-8601 instant with a zone." field)))))

(-> memory-knowledge--assert (tool-context hash-table) string)
(defun memory-knowledge--assert (context operation)
  "Apply one fact-assert OPERATION, one support per source, and describe the result."
  (memory-knowledge--fields operation "fact-assert"
                            :allowed  '("op" "predicate" "arguments" "negated" "sources" "basis"
                                        "valid-from" "valid-until" "scope")
                            :required '("predicate" "arguments"))
  (let* ((configuration (tool-context-configuration context))
         (conversation  (tool-context-conversation context))
         (predicate     (gethash "predicate" operation))
         (schema        (or (knowledge-base-schema (knowledge-base-read configuration) predicate
                                                   :workspace (knowledge--workspace configuration))
                            (memory-resource--fail "Define predicate ~A with predicate-define first."
                                                   predicate)))
         (proposition   (knowledge-make-proposition
                         schema
                         (memory-knowledge--list (gethash "arguments" operation) "arguments")
                         :negated (and (gethash "negated" operation)
                                       (not (json-false-p (gethash "negated" operation))))))
         (now           (knowledge-now))
         (requests      (and (gethash "sources" operation)
                             (memory-resource--operation-sources operation)))
         (evidence      (mapcar (lambda (request)
                                  (memory-knowledge--evidence
                                   context (memory-resource--observe-source context request now)))
                                requests))
         (named-basis   (gethash "basis" operation))
         (basis         (and named-basis
                             (or (find named-basis '(:observation :user-assertion :assumption
                                                     :model-proposal)
                                       :key  (lambda (keyword) (string-downcase (symbol-name keyword)))
                                       :test #'string=)
                                 (memory-resource--fail "basis must be observation, user-assertion, assumption, or model-proposal."))))
         (claim         nil)
         (notes         nil))
    (if evidence
        (dolist (item evidence)
          (multiple-value-bind (recorded note) (memory-knowledge--basis context item basis)
            (when note
              (pushnew note notes :test #'string=))
            (setf claim (knowledge-assert
                         configuration proposition
                         :scope               (memory-knowledge--scope operation)
                         :evidence            item
                         :basis               recorded
                         :valid-from          (memory-knowledge--time operation "valid-from")
                         :valid-until         (memory-knowledge--time operation "valid-until")
                         :source-conversation (conversation-identifier conversation)))))
        (setf claim (knowledge-assert
                     configuration proposition
                     :scope               (memory-knowledge--scope operation)
                     :basis               (or basis ':model-proposal)
                     :valid-from          (memory-knowledge--time operation "valid-from")
                     :valid-until         (memory-knowledge--time operation "valid-until")
                     :source-conversation (conversation-identifier conversation))))
    (format nil "Asserted ~A: ~A.~{ ~A~}"
            claim (knowledge-proposition-display proposition schema) (reverse notes))))

(-> memory-knowledge--basis (tool-context list (option keyword)) (values keyword (option string)))
(defun memory-knowledge--basis (context evidence requested)
  "Return the basis EVIDENCE establishes, at most REQUESTED, and a note when it was lowered.

A basis comes from where the evidence was checked, never from the label the
model asks for: a file Autolith read is an observation, the user's own words
are a user assertion, and a page counts as observed only when this
conversation retrieved it with the quoted text. Otherwise the support is the
model's proposal. The model may lower a basis to an assumption or a proposal,
never raise it."
  (declare (ignore context))
  (let* ((source      (getf (rest evidence) :source))
         (established (ecase (first source)
                        (:file
                         ':observation)
                        (:user
                         ':user-assertion)
                        (:url
                         (if (and (equal (getf (rest source) :retrieval) "web_extra.gist")
                                  (getf (rest evidence) :digest))
                             ':observation
                             ':model-proposal)))))
    (cond
      ((member requested '(:assumption :model-proposal))
       (values requested nil))
      ((or (null requested) (eq requested established))
       (values established
               (and (eq established ':model-proposal)
                    (format nil "~A was not retrieved with that text in this conversation, so it supports the fact as your proposal; retrieve it with web_extra.gist to make it an observation."
                            (getf (rest source) :url)))))
      (t
       (values established
               (format nil "The ~(~A~) source supports it as ~(~A~), not ~(~A~)."
                       (first source) established requested))))))

(-> memory-knowledge--receipt-content
    (list string (option configuration)) (option string))
(defun memory-knowledge--receipt-content (receipt output configuration)
  "Verify OUTPUT's presentation and return the retained retrieved text.

Without overflow storage only the original prefix is evidence, never the
omission notice. With storage, verify the complete object's retrieval digest.
Older receipts identify their complete, unbounded output directly."
  (let ((output-digest   (getf receipt :output-digest))
        (content-object  (getf receipt :content-object))
        (retained-length (getf receipt :retained-length)))
    (when (equal (or output-digest (getf receipt :result-digest))
                 (memory-source-text-digest output))
      (cond
        ((null output-digest)
         output)
        (content-object
         (when (and configuration
                    (stringp content-object)
                    (uiop:string-prefix-p "context:" content-object))
           (multiple-value-bind (object content)
               (rlm-context-object-find configuration (subseq content-object 8))
             (when (and object (stringp content)
                        (equal (getf receipt :result-digest)
                               (memory-source-text-digest content)))
               content))))
        ((typep retained-length `(integer 0 ,(length output)))
         (subseq output 0 retained-length))))))

(-> memory-knowledge--url-receipt
    (conversation string (option string) &key (:configuration (option configuration)))
    (option list))
(defun memory-knowledge--url-receipt (conversation url quote &key configuration)
  "Return the latest completed, verified retrieval receipt for URL containing QUOTE.

Match the durable presentation separately from the immutable retrieved content.
Only retained retrieved text supports quotations. Reuse the retrieval time,
never the time a later assertion cites it."
  (let ((receipts (make-hash-table :test #'equal))
        (found    nil))
    (handler-case
        (conversation-map-records
         conversation
         (lambda (record)
           (let ((properties (rest record)))
             (case (first record)
               (:web-retrieval
                (when (and (equal (getf properties :tool) "web_extra.gist")
                           (equal (getf properties :url) url)
                           (non-empty-string-p (getf properties :call-id))
                           (memory-source-digest-p (getf properties :result-digest))
                           (typep (getf properties :observed-at) 'timestamp)
                           (<= (getf properties :observed-at) (knowledge-now)))
                  (setf (gethash (getf properties :call-id) receipts) properties)))
               (:tool-result
                (let* ((receipt (gethash (getf properties :call-id) receipts))
                       (output  (getf properties :output))
                       (content
                         (when (and receipt
                                    (equal (getf properties :tool) "web_extra.gist")
                                    (eq (getf properties :status) ':ok)
                                    (stringp output))
                           (memory-knowledge--receipt-content receipt output configuration))))
                  (when (and content
                             (or (null quote) (memory-source-quote-found-p quote content))
                             (or (null found)
                                 (> (getf receipt :observed-at) (getf found :observed-at))))
                    (setf found receipt))))))))
      (error ()
        (setf found nil)))
    found))

(-> memory-knowledge--retract (tool-context hash-table) string)
(defun memory-knowledge--retract (context operation)
  "Apply one fact-retract OPERATION and describe the result."
  (memory-knowledge--fields operation "fact-retract"
                            :allowed  '("op" "id" "reason")
                            :required '("id"))
  (let ((retracted (knowledge-retract (tool-context-configuration context)
                                      (gethash "id" operation)
                                      :reason (gethash "reason" operation)
                                      :source-conversation
                                      (conversation-identifier
                                       (tool-context-conversation context)))))
    (format nil "Retracted ~D support~:P of ~A." (length retracted) (gethash "id" operation))))

(-> memory-knowledge--propose (tool-context hash-table) string)
(defun memory-knowledge--propose (context operation)
  "Apply one rule-propose OPERATION and describe the result."
  (memory-knowledge--fields operation "rule-propose"
                            :allowed  '("op" "id" "premises" "guards" "bindings" "conclusion"
                                        "description" "scope")
                            :required '("id" "premises" "conclusion"))
  (let ((configuration (tool-context-configuration context))
        (scope         (memory-knowledge--scope operation)))
    (multiple-value-bind (identifier version new-p)
        (knowledge-propose-rule configuration
                                :id                  (gethash "id" operation)
                                :premises            (gethash "premises" operation)
                                :guards              (gethash "guards" operation #())
                                :bindings            (gethash "bindings" operation #())
                                :conclusion          (gethash "conclusion" operation)
                                :description         (gethash "description" operation)
                                :scope               scope
                                :source-conversation (conversation-identifier
                                                      (tool-context-conversation context)))
      (let* ((entry    (knowledge-base-rule (knowledge-base-read configuration) identifier
                                            :workspace (knowledge--workspace configuration)))
             (active-p (eql (getf entry :active) version))
             (decision (if active-p
                           ':full-access
                           (tool-context-authorize-command
                            context
                            (memory-knowledge--admission-command
                             identifier version
                             (getf (knowledge-rule-entry-version entry version) :rule))
                            (config :working-directory configuration)))))
        (cond
          (active-p
           (format nil "Rule ~A version ~D is unchanged and active." identifier version))
          ((eq decision ':deny)
           (format nil "~:[Rule ~A version ~D is unchanged~;Proposed rule ~A version ~D~]; admission was declined, so it derives nothing. The user can admit it with (admit-rule ~S)."
                   new-p identifier version identifier))
          (t
           (knowledge-admit-rule configuration identifier
                                 :scope scope :version version
                                 :by (string-downcase (symbol-name decision)))
           (format nil "Admitted rule ~A version ~D." identifier version)))))))

(-> memory-knowledge--admission-command (string integer list) string)
(defun memory-knowledge--admission-command (identifier version rule)
  "Return the command text a person approves to admit RULE."
  (format nil "rule.admit -- ~A v~D: ~{~A~^ and ~}~@[ where ~{~A~^, ~}~]~@[ with ~{~A~^, ~}~] => ~A"
          identifier version
          (mapcar #'memory-knowledge--pattern-text (getf (rest rule) :premises))
          (memory-knowledge--guard-texts rule)
          (memory-knowledge--binding-texts rule)
          (memory-knowledge--pattern-text (getf (rest rule) :conclusion))))

(-> memory-knowledge--define-method (tool-context hash-table) string)
(defun memory-knowledge--define-method (context operation)
  "Apply one method-define OPERATION and describe the result."
  (memory-knowledge--fields operation "method-define"
                            :allowed  '("op" "id" "task" "when" "steps" "description" "scope")
                            :required '("id" "task" "steps"))
  (multiple-value-bind (identifier changed-p)
      (knowledge-define-method (tool-context-configuration context)
                               :id                  (gethash "id" operation)
                               :task                (gethash "task" operation)
                               :when                (gethash "when" operation #())
                               :steps               (gethash "steps" operation)
                               :description         (gethash "description" operation)
                               :scope               (memory-knowledge--scope operation)
                               :source-conversation (conversation-identifier
                                                     (tool-context-conversation context)))
    (format nil "~:[Method ~A is unchanged~;Defined method ~A~]." changed-p identifier)))

(-> memory-knowledge--define-contract (tool-context hash-table) string)
(defun memory-knowledge--define-contract (context operation)
  "Propose a contract and admit its exact content only after command authorization."
  (memory-knowledge--fields operation "contract-define"
                            :allowed  '("op" "id" "tool" "when" "requires" "effects" "observes"
                                        "accepts" "description" "scope")
                            :required '("id" "tool"))
  (let* ((configuration (tool-context-configuration context))
         (scope         (memory-knowledge--scope operation))
         (workspace     (knowledge--owner configuration scope)))
    (multiple-value-bind (identifier changed-p version)
        (knowledge-define-contract configuration
                                   :id                  (gethash "id" operation)
                                   :tool                (gethash "tool" operation)
                                   :when                (gethash "when" operation)
                                   :requires            (gethash "requires" operation #())
                                   :effects             (gethash "effects" operation #())
                                   :observes            (gethash "observes" operation #())
                                   :accepts             (gethash "accepts" operation #())
                                   :description         (gethash "description" operation)
                                   :scope scope :admit nil
                                   :source-conversation (conversation-identifier
                                                         (tool-context-conversation context)))
      (let* ((entry    (gethash (list scope workspace identifier)
                               (knowledge-base-contracts-table (knowledge-base-read configuration))))
             (contract (getf entry :contract))
             (active-p (knowledge-contract-admitted-p entry))
             (decision
               (unless active-p
                 (tool-context-authorize-command
                  context
                  (with-standard-io-syntax
                    (format nil "contract.admit -- ~(~A~) ~A v~D: ~S"
                            scope identifier version contract))
                  (config :working-directory configuration)))))
        (cond
          (active-p
           (format nil "Contract ~A version ~D is unchanged and active." identifier version))
          ((eq decision ':deny)
           (format nil "~:[Contract ~A version ~D is unchanged~;Proposed contract ~A version ~D~]; admission was declined, so that version has no runtime effects."
                   changed-p identifier version))
          (t
           (knowledge-admit-contract configuration identifier :scope scope
                                     :contract contract :version version
                                     :by                             (string-downcase (symbol-name decision)))
           (format nil "Admitted contract ~A version ~D." identifier version)))))))

(-> memory-knowledge--apply (tool-context t) string)
(defun memory-knowledge--apply (context operation)
  "Apply one memory:facts OPERATION and return its one-line result."
  (unless (hash-table-p operation)
    (memory-resource--fail "Each operation is an object with an op field."))
  (let ((name (gethash "op" operation)))
    (cond
      ((equal name "predicate-define")
       (memory-knowledge--define (tool-context-configuration context) operation))
      ((equal name "fact-assert")
       (memory-knowledge--assert context operation))
      ((equal name "fact-retract")
       (memory-knowledge--retract context operation))
      ((equal name "rule-propose")
       (memory-knowledge--propose context operation))
      ((equal name "method-define")
       (memory-knowledge--define-method context operation))
      ((equal name "contract-define")
       (memory-knowledge--define-contract context operation))
      (t
       (memory-resource--fail "memory:facts accepts predicate-define, fact-assert, fact-retract, rule-propose, method-define, and contract-define.")))))

(defmethod resource-tool-edit
    ((resource memory-knowledge-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply up to the operation limit to memory:facts and report what now holds."
  (declare (ignore tool))
  (unless (eq (memory-knowledge-resource-view-kind resource) ':facts)
    (memory-resource--fail "Only memory:facts accepts edits."))
  (let* ((conversation  (tool-context-conversation context))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operations    (memory-knowledge--list (tool-argument arguments "operations" :required t)
                                                "operations")))
    (unless (<= 1 (length operations) *memory-knowledge-operation-limit*)
      (memory-resource--fail "memory:facts accepts 1 to ~D operations per edit."
                             *memory-knowledge-operation-limit*))
    ;; Additions do not depend on what was read, so any memory:facts revision
    ;; observed in this conversation authorizes an edit.
    (handler-case
        (resource-item-find-observation-state conversation resource base-revision)
      (resource-revision-stale ()
        (memory-resource--fail "Read memory:facts in this conversation and edit against a revision it returned.")))
    (let ((before  (knowledge-view-snapshot (memory-knowledge-view context)))
          (results nil))
      (loop for operation in operations
            for index from 1
            do (handler-case (push (memory-knowledge--apply context operation) results)
                 ((or knowledge-error memory-error tool-error) (condition)
                   (memory-resource--fail "Operation ~D failed: ~A~@[ Operations 1 to ~D were applied.~]"
                                          index (autolith-error-message condition)
                                          (and (> index 1) (1- index))))))
      (let* ((changes     (knowledge-view-changes before (memory-knowledge-view context)))
             (observation (memory-knowledge-observe resource context :maximum-results 1))
             (state       (resource-observation-state-ensure conversation observation)))
        (tool-success
         (format nil "~{~A~%~}~:[~;~:*~{~A~%~}~]Revision: ~A"
                 (reverse results)
                 (knowledge-change-lines changes)
                 (resource-observation-state-alias state)))))))
