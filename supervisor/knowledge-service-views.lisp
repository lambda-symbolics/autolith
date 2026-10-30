(in-package #:autolith-supervisor)

;;;; -- Structured Knowledge Resource Views --


;;;; -- Rendering --

(serapeum/types:-> memory-knowledge--fact-line (knowledge-view knowledge-node) string)
(defun memory-knowledge--fact-line (view node)
  "Return NODE as one compact JSON line for the model."
  (argo:json-encode (knowledge-node-json view node)))

(serapeum/types:-> memory-knowledge--matches-p (knowledge-node list) boolean)
(defun memory-knowledge--matches-p (node terms)
  "Return true when every lowercase query term appears in NODE's text or arguments."
  (= (knowledge-node-relevance node terms) (length terms)))

(serapeum/types:-> memory-knowledge--render-facts
    (knowledge-view &key (:query (option string)) (:maximum-results integer))
    (values string list))
(defun memory-knowledge--render-facts (view &key query maximum-results)
  "Return the fact listing for VIEW and the identifiers it shows.

Conflicted facts come first, then the rest by predicate and text."
  (let* ((terms (and query (memory--search-terms query)))
         (facts (sort (loop for node being the hash-values of (knowledge-view-nodes view)
                            when (and (knowledge-node-supported-p node)
                                      (or (null terms) (memory-knowledge--matches-p node terms)))
                              collect node)
                      (lambda (left right)
                        (let ((left-conflicted  (and (knowledge-node-conflicts left) t))
                              (right-conflicted (and (knowledge-node-conflicts right) t)))
                          (if (eq left-conflicted right-conflicted)
                              (string< (knowledge-node-text left) (knowledge-node-text right))
                              left-conflicted)))))
         (shown (subseq facts 0 (min maximum-results (length facts)))))
    (values
     (with-output-to-string (stream)
       (format stream "collection: facts~@[ matching ~S~]~%count: ~D~:[~; (showing ~D; narrow with query or raise max-results)~]~%"
               query (length facts) (> (length facts) (length shown)) (length shown))
       (when (knowledge-view-incomplete view)
         (format stream "incomplete: rules ~{~A~^, ~} reached a work limit; missing conclusions are unknown, not false~%"
                 (knowledge-view-incomplete view)))
       (dolist (error (knowledge-view-errors view))
         (format stream "rule ~A v~D could not derive: ~A~%"
                 (getf error :rule) (getf error :version) (getf error :message)))
       (if shown
           (format stream "~%~{~A~%~}Read memory:claim/ID for why a fact holds."
                   (mapcar (lambda (node) (memory-knowledge--fact-line view node)) shown))
           (format stream "~%No facts hold~:[~; for this query~]." query)))
     (mapcar #'knowledge-node-identifier shown))))

(serapeum/types:-> memory-knowledge--render-support (list stream integer) null)
(defun memory-knowledge--render-support (support stream indent)
  "Write one explained SUPPORT to STREAM at INDENT."
  (format stream "~vTsupport ~A: ~(~A~), ~A (~A)~%"
          indent
          (getf support :support)
          (getf support :basis)
          (getf support :text)
          (knowledge-applicability-text (getf support :state) (getf support :reason)))
  nil)

(serapeum/types:-> memory-knowledge--render-tree (list stream integer) null)
(defun memory-knowledge--render-tree (tree stream indent)
  "Write one explanation TREE to STREAM, nesting premises at INDENT."
  (cond
    ((getf tree :repeated)
     (format stream "~vT~A (shown above)~%" indent (getf tree :claim)))
    ((getf tree :more)
     (format stream "~vT~A ~A: read memory:claim/~A~%"
             indent (getf tree :claim) (getf tree :text) (getf tree :claim)))
    (t
     (format stream "~vT~A ~A [~(~A~), ~(~A~)]~@[ bases ~{~{~(~A~)~^+~}~^ or ~}~]~%"
             indent (getf tree :claim) (or (getf tree :text) "unknown claim")
             (getf tree :status) (getf tree :applicability) (getf tree :bases))
     (dolist (support (getf tree :supports))
       (memory-knowledge--render-support support stream (+ indent 2)))
     (dolist (justification (getf tree :justifications))
       (format stream "~vTderived by rule ~A v~D~@[ with ~{~A~^, ~}~] from:~%"
               (+ indent 2) (getf justification :rule) (getf justification :version)
               (mapcar (lambda (binding)
                         (format nil "~A = ~A" (first binding)
                                 (knowledge-term-display (rest binding))))
                       (getf justification :bindings)))
       (dolist (premise (getf justification :premises))
         (memory-knowledge--render-tree premise stream (+ indent 4))))
     (when (getf tree :conflicts)
       (format stream "~vTconflicts with ~{~A~^, ~}~%" (+ indent 2) (getf tree :conflicts)))))
  nil)

(serapeum/types:-> memory-knowledge--render-methods (knowledge-base (option string)) string)
(defun memory-knowledge--render-methods (base workspace)
  "Return the task methods visible from WORKSPACE with their conditions and steps."
  (let ((entries (sort (loop for entry being the hash-values of (knowledge-base-methods-table base)
                             when (knowledge-visible-p (getf entry :scope) (getf entry :workspace)
                                                       workspace)
                               collect entry)
                       #'string< :key (lambda (entry) (getf (rest (getf entry :method)) :id)))))
    (with-output-to-string (stream)
      (format stream "collection: methods~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (let ((method (rest (getf entry :method))))
          (format stream "~%~A ~(~A~): ~A~@[ when ~{~A~^ and ~}~]~%~{  ~A~%~}~@[  ~A~%~]"
                  (getf method :id) (getf entry :scope)
                  (knowledge-pattern-text (getf method :task))
                  (mapcar #'knowledge-pattern-text (getf method :when))
                  (mapcar (lambda (step)
                            (if (eq (first step) :task)
                                (format nil "task ~A" (knowledge-pattern-text (second step)))
                                (format nil "step ~S~@[ requires ~{~A~^ and ~}~]"
                                        (second step)
                                        (mapcar #'knowledge-pattern-text
                                                (getf (rest (rest step)) :requires)))))
                          (getf method :steps))
                  (getf method :description))))
      (unless entries
        (format stream "~%No methods are defined.")))))

(serapeum/types:-> memory-knowledge--render-contracts (knowledge-base (option string)) string)
(defun memory-knowledge--render-contracts (base workspace)
  "Return the action contracts visible from WORKSPACE."
  (let ((entries
          (sort (loop for entry being the hash-values of (knowledge-base-contracts-table base)
                      when (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
                        append (let ((admission (getf entry :admission)))
                                 (cons entry
                                       (when (and admission (not (knowledge-contract-admitted-p entry)))
                                         (list (list :contract         (getf admission :contract)
                                                     :contract-version (getf admission :contract-version)
                                                     :admission admission :scope (getf entry :scope)))))))
                #'string< :key (lambda (entry) (getf (rest (getf entry :contract)) :id)))))
    (with-output-to-string (stream)
      (format stream "collection: contracts~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (let ((contract (rest (getf entry :contract))))
          (format stream "~%~A ~(~A~) v~D [~A]: ~A~@[ when ~{~A~^, ~}~]~@[~%  requires ~{~A~^ and ~}~]~@[ on ~{~(~A~)~^, ~}~]~@[~%  predicts ~{~A~^ and ~}~]~@[~%  observes ~{~A~^ and ~}~]~@[~%  ~A~]~%"
                  (getf contract :id) (getf entry :scope) (getf entry :contract-version)
                  (if (knowledge-contract-admitted-p entry) "admitted" "proposed")
                  (getf contract :tool)
                  (mapcar (lambda (condition) (format nil "~A = ~A" (first condition) (rest condition)))
                          (getf contract :when))
                  (mapcar #'knowledge-pattern-text (getf contract :requires))
                  (getf contract :accepts)
                  (mapcar #'knowledge-pattern-text (getf contract :effects))
                  (mapcar (lambda (observation)
                            (format nil "~A(~{~A~^, ~})" (getf observation :predicate)
                                    (getf observation :arguments)))
                          (getf contract :observes))
                  (getf contract :description))))
      (unless entries
        (format stream "~%No contracts are defined.")))))

(serapeum/types:-> memory-knowledge--render-procedures (knowledge-base string) string)
(defun memory-knowledge--render-procedures (base workspace)
  "Return WORKSPACE's learned procedures with their status, record, parameters, and calls."
  (let ((entries (knowledge-base-procedures base workspace)))
    (with-output-to-string (stream)
      (format stream "collection: procedures~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (let* ((version   (procedure-entry-version entry))
               (procedure (getf version :procedure)))
          (format stream "~%~A v~D ~(~A~)~@[ (runs v~D)~] · ~D run~:P, ~D failed~%"
                  (getf entry :id) (getf version :version) (procedure-entry-status entry)
                  (and (getf entry :admitted)
                       (/= (getf entry :admitted) (getf version :version))
                       (getf entry :admitted))
                  (getf entry :runs) (getf entry :failures))
          (when (getf version :examples)
            (format stream "  learned from ~D turns, ~D model requests and ~D tokens each on average; each turn is reproduced by the procedure learned from the others~%"
                    (length (getf version :examples))
                    (getf (getf version :baseline) :requests)
                    (getf (getf version :baseline) :tokens)))
          (let ((counterexamples (getf (getf version :evaluation) :counterexamples)))
            (when (and counterexamples (plusp counterexamples))
              (format stream "  not offered: ~D other request~:P with the same wording did something else~%"
                      counterexamples)))
          (format stream "~@[  parameters: ~{~A~^, ~}~%~]~@[  request: ~A~%~]"
                  (mapcar (lambda (parameter)
                            (format nil "~A (~(~A~)~@[ ending in ~A~])" (getf parameter :name)
                                    (getf parameter :type) (getf parameter :suffix)))
                          (getf (rest procedure) :parameters))
                  (procedure-input-text procedure))
          (loop for step in (getf (rest procedure) :steps)
                for index from 1
                do (format stream "  ~D. ~A ~A~%" index (getf step :tool)
                           (procedure-template-text (getf step :arguments))))))
      (unless entries
        (format stream "~%No procedures are learned yet. Autolith learns one when three turns make the same tool calls.")))))

(serapeum/types:-> memory-knowledge--render-changes (t string knowledge-view) string)
(defun memory-knowledge--render-changes (configuration viewer view)
  "Return what changed for VIEWER since it last read memory:changes, and record this read."
  (multiple-value-bind (changes previous first-p)
      (knowledge-viewer-changes configuration viewer view)
    (cond
      (first-p
       (format nil "First look: ~D fact~:P hold now. Later reads of memory:changes report what changed since this one."
               (length (knowledge-view-snapshot view))))
      (changes
       (format nil "Changes since ~A:~%~{~A~%~}"
               (memory--timestamp-string previous)
               (knowledge-change-lines changes)))
      (t
       (format nil "Nothing changed since ~A." (memory--timestamp-string previous))))))

(serapeum/types:-> memory-knowledge--render-claim (knowledge-view string) string)
(defun memory-knowledge--render-claim (view identifier)
  "Return why claim IDENTIFIER holds or does not, as an indented tree."
  (with-output-to-string (stream)
    (format stream "claim: ~A~%" identifier)
    (memory-knowledge--render-tree (first (knowledge-explain view identifier)) stream 0)))

(serapeum/types:-> memory-knowledge--render-predicates (knowledge-base (option string)) string)
(defun memory-knowledge--render-predicates (base workspace)
  "Return the predicates visible from WORKSPACE with their arguments and templates."
  (let ((entries (sort (loop for key being the hash-keys of (knowledge-base-schemas base)
                               using (hash-value value)
                             when (knowledge-visible-p (first key) (second key) workspace)
                               collect (list key value))
                       #'string< :key (lambda (entry) (third (first entry))))))
    (with-output-to-string (stream)
      (format stream "collection: predicates~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (destructuring-bind ((scope owner predicate) (schema version)) entry
          (declare (ignore owner))
          (format stream "~%~A(~{~A~^, ~}) v~D ~(~A~)~@[ key ~{~A~^, ~}~]~@[ template ~S~]~@[: ~A~]"
                  predicate
                  (mapcar (lambda (argument)
                            (format nil "~A ~(~A~)~@[ in ~A~]"
                                    (getf argument :name) (getf argument :type)
                                    (getf argument :unit)))
                          (knowledge-schema-arguments schema))
                  version scope
                  (mapcar (lambda (position)
                            (getf (nth position (knowledge-schema-arguments schema)) :name))
                          (knowledge-schema-key schema))
                  (getf (rest schema) :template)
                  (getf (rest schema) :description))))
      (unless entries
        (format stream "~%No predicates are defined. Define one with predicate-define on memory:facts.")))))

(serapeum/types:-> memory-knowledge--pattern-text (list) string)
(defun memory-knowledge--pattern-text (pattern)
  "Return a rule PATTERN as compact text."
  (if (knowledge-negation-p pattern)
      (format nil "not ~A" (memory-knowledge--pattern-text (second pattern)))
      (format nil "~A(~{~A~^, ~})" (first pattern)
              (mapcar (lambda (item)
                        (if (knowledge-variable-p item) item (knowledge-term-display item)))
                      (rest pattern)))))

(serapeum/types:-> memory-knowledge--render-rules (knowledge-base (option string)) string)
(defun memory-knowledge--render-rules (base workspace)
  "Return the rules visible from WORKSPACE with each version's state."
  (let ((entries (sort (knowledge-base-rule-entries base :workspace workspace)
                       #'string< :key (lambda (entry) (getf entry :id)))))
    (with-output-to-string (stream)
      (format stream "collection: rules~%count: ~D~%" (length entries))
      (dolist (entry entries)
        (let* ((latest (first (last (getf entry :versions))))
               (rule   (getf latest :rule)))
          (format stream "~%~A v~D ~(~A~): ~:[proposed, awaiting admission~;~:*active version ~D~]~%  ~{~A~^ and ~}~@[ where ~{~A~^, ~}~]~@[ with ~{~A~^, ~}~] => ~A~@[~%  ~A~]~%"
                  (getf entry :id) (getf latest :version) (getf entry :scope)
                  (getf entry :active)
                  (mapcar #'memory-knowledge--pattern-text (getf (rest rule) :premises))
                  (memory-knowledge--guard-texts rule)
                  (memory-knowledge--binding-texts rule)
                  (memory-knowledge--pattern-text (getf (rest rule) :conclusion))
                  (getf (rest rule) :description))))
      (unless entries
        (format stream "~%No rules are defined.")))))

(serapeum/types:-> memory-knowledge--guard-texts (list) list)
(defun memory-knowledge--guard-texts (rule)
  "Return RULE's guards as text."
  (mapcar (lambda (guard)
            (format nil "~A ~A ~A"
                    (memory-knowledge--expression-text (second guard))
                    (first guard)
                    (memory-knowledge--expression-text (third guard))))
          (getf (rest rule) :guards)))

(serapeum/types:-> memory-knowledge--binding-texts (list) list)
(defun memory-knowledge--binding-texts (rule)
  "Return RULE's bindings as text."
  (mapcar (lambda (binding)
            (format nil "~A = ~A" (first binding)
                    (memory-knowledge--expression-text (second binding))))
          (getf (rest rule) :bindings)))

(serapeum/types:-> memory-knowledge--expression-text (t) string)
(defun memory-knowledge--expression-text (expression)
  "Return a rule EXPRESSION in infix-style text people read."
  (cond
    ((knowledge-variable-p expression)
     expression)
    ((and (knowledge-expression-call-p expression)
          (= (length expression) 3)
          (member (first expression) '("+" "-" "*" "/") :test #'string=))
     (format nil "(~A ~A ~A)"
             (memory-knowledge--expression-text (second expression))
             (first expression)
             (memory-knowledge--expression-text (third expression))))
    ((knowledge-expression-call-p expression)
     (format nil "~A(~{~A~^, ~})" (first expression)
             (mapcar #'memory-knowledge--expression-text (rest expression))))
    (t
     (knowledge-term-display expression))))


;;;; -- Parent Service Projection --

(defparameter *knowledge-service-content-limit* 262144
  "The most characters returned by one parent knowledge view.")

(serapeum/types:-> knowledge-service-render
    (memory-store-context keyword
     &key (:base (option knowledge-base)) (:query (option string))
          (:maximum-results (option integer)) (:claim (option string))
          (:viewer (option string)) (:scope keyword) (:allow-global-p boolean))
    string)
(defun knowledge-service-render
    (context view-kind &key base query maximum-results claim viewer
                            (scope ':workspace) allow-global-p)
  "Render VIEW-KIND from the parent's authorized BASE projection.

The caller must select BASE under the session scope policy before this call.
No store is reopened here. CONTEXT supplies trusted evidence access and viewer
storage. VIEWER is the parent session identity, never a child-selected path.
SCOPE distinguishes viewer snapshots. Global reads require ALLOW-GLOBAL-P."
  (unless (and (typep base 'knowledge-base)
               (member scope '(:workspace :global))
               (or (not (eq scope ':global)) allow-global-p))
    (knowledge--fail ':invalid-view-scope scope
                     "The knowledge view has no authorized base or scope."))
  (unless (member view-kind '(:facts :claim :predicates :rules :methods
                              :contracts :procedures :changes))
    (knowledge--fail ':invalid-view-kind view-kind "Unknown knowledge view."))
  (when (or (and query (not (and (non-empty-string-p query) (<= (length query) 4096))))
            (and maximum-results (not (and (integerp maximum-results)
                                           (<= 1 maximum-results 50))))
            (and (or query maximum-results) (not (eq view-kind ':facts)))
            (and (eq view-kind ':claim)
                 (not (and (non-empty-string-p claim) (<= (length claim) 256))))
            (and (eq view-kind ':changes)
                 (not (and (non-empty-string-p viewer) (<= (length viewer) 256)))))
    (knowledge--fail ':invalid-view-arguments view-kind
                     "Invalid knowledge view query, limit, claim, or viewer."))
  (let* ((*memory-store-context* context)
         (workspace              (knowledge--workspace context))
         (view                   (when (member view-kind '(:facts :claim :changes))
                                   (knowledge-evaluate
                                    base :workspace workspace
                                    :check          (evidence-check-create
                                                     :memories (memory-list context :visibility ':all)))))
         (content
           (ecase view-kind
             (:facts
              (memory-knowledge--render-facts
               view :query query :maximum-results (or maximum-results 16)))
             (:claim
              (memory-knowledge--render-claim view claim))
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
               context (format nil "parent:~(~A~):~A" scope viewer) view)))))
    (when (> (length content) *knowledge-service-content-limit*)
      (knowledge--fail ':view-limit view-kind
                       "The knowledge view exceeds the response content limit."))
    content))
