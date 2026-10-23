(in-package #:autolith)

;;;; -- Knowledge Evaluation --

;;; Evaluation turns the stored base into a VIEW for one workspace at one time.
;;; A stored claim is supported when one of its supports applies now. Admitted
;;; rules then derive conclusions from supported claims, in an order where
;;; every predicate a rule reads is complete before the rule runs; rules are
;;; acyclic, so one pass suffices. A derived conclusion carries a
;;; justification naming its rule version, premises, and bindings, and is
;;; supported exactly while its premises are. Nothing derived is stored: the
;;; view is a function of the log, the cited files, and the time, so a restart
;;; reproduces it and recomputation needs no model.

(defparameter *knowledge-match-limit* 4096
  "The most rule conclusions one evaluation derives before it is incomplete.")

(defparameter *knowledge-step-limit* 250000
  "The most candidate claims one evaluation examines before it is incomplete.")

(defparameter *knowledge-error-limit* 32
  "The most rule evaluation errors one view retains.")

(defparameter *knowledge-basis-order*
  '(:observation :user-assertion :model-proposal :assumption)
  "The canonical order of bases within a basis set.")

(defclass knowledge-node ()
  ((identifier
    :initarg :identifier
    :reader knowledge-node-identifier
    :type string
    :documentation "The claim identifier.")
   (proposition
    :initarg :proposition
    :reader knowledge-node-proposition
    :type list
    :documentation "The canonical proposition the claim makes.")
   (scope
    :initarg :scope
    :reader knowledge-node-scope
    :type memory-scope
    :documentation "Whether the claim is global or belongs to a workspace.")
   (workspace
    :initarg :workspace
    :reader knowledge-node-workspace
    :type (option string)
    :documentation "The workspace owning a workspace claim.")
   (schema
    :initarg :schema
    :reader knowledge-node-schema
    :type (option list)
    :documentation "The schema of the claim's predicate visible in the view.")
   (supports
    :initform nil
    :accessor knowledge-node-supports
    :type list
    :documentation "(:ID :STATE :REASON :BASIS :EVIDENCE) for each stored support.")
   (justifications
    :initform nil
    :accessor knowledge-node-justifications
    :type list
    :documentation "(:ID :RULE :VERSION :PREMISES :BINDINGS) for each derivation.")
   (bases
    :initform nil
    :accessor knowledge-node-bases
    :type list
    :documentation "The minimal basis sets of the paths supporting the claim.")
   (conflicts
    :initform nil
    :accessor knowledge-node-conflicts
    :type list
    :documentation "Identifiers of supported claims this one contradicts.")
   (text
    :initform nil
    :accessor knowledge-node--text
    :type (option string)
    :documentation "The claim's sentence, computed once.")
   (haystack
    :initform nil
    :accessor knowledge-node--haystack
    :type (option string)
    :documentation "The claim's lowercase sentence and values for lexical matching."))
  (:documentation "One claim as evaluated in a view."))

(defclass knowledge-view ()
  ((base
    :initarg :base
    :reader knowledge-view-base
    :type knowledge-base
    :documentation "The folded log the view was evaluated from.")
   (workspace
    :initarg :workspace
    :reader knowledge-view-workspace
    :type (option string)
    :documentation "The workspace whose visible knowledge the view holds.")
   (now
    :initarg :now
    :reader knowledge-view-now
    :type timestamp
    :documentation "The time validity and expiry were evaluated at.")
   (nodes
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-nodes
    :type hash-table
    :documentation "Evaluated claims by identifier, stored and derived.")
   (supported
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-supported
    :type hash-table
    :documentation "Supported positive nodes by predicate.")
   (negated
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-negated
    :type hash-table
    :documentation "Supported negated nodes by predicate.")
   (argument-index
    :initform (make-hash-table :test #'equal)
    :reader knowledge-view-argument-index
    :type hash-table
    :documentation "Supported positive nodes by (PREDICATE POSITION STRING-TERM).")
   (incomplete
    :initform nil
    :accessor knowledge-view-incomplete
    :type list
    :documentation "Identifiers of rules whose evaluation hit a work limit.")
   (errors
    :initform nil
    :accessor knowledge-view-errors
    :type list
    :documentation "(:RULE :VERSION :MESSAGE) for derivations a computation refused.")
   (matches
    :initform 0
    :accessor knowledge-view-matches
    :type (integer 0)
    :documentation "Rule conclusions derived so far.")
   (steps
    :initform 0
    :accessor knowledge-view-steps
    :type (integer 0)
    :documentation "Candidate claims examined so far.")
   (boundary
    :initform nil
    :accessor knowledge-view-boundary
    :type (option timestamp)
    :documentation "The earliest later time a support or evidence starts or stops being valid."))
  (:documentation "Knowledge visible from one workspace, evaluated at one time."))


;;;; -- Node Status --

(-> knowledge-node-supported-p (knowledge-node) boolean)
(defun knowledge-node-supported-p (node)
  "Return true when an applicable support or a derivation supports NODE."
  (and (or (knowledge-node-justifications node)
           (find ':current (knowledge-node-supports node)
                 :key (lambda (support) (getf support :state))))
       t))

(-> knowledge-node-derived-p (knowledge-node) boolean)
(defun knowledge-node-derived-p (node)
  "Return true when a rule derives NODE."
  (and (knowledge-node-justifications node) t))

(-> knowledge-node-negated-p (knowledge-node) boolean)
(defun knowledge-node-negated-p (node)
  "Return true when NODE claims an explicit negation."
  (knowledge-negation-p (knowledge-node-proposition node)))

(-> knowledge-node-status (knowledge-view knowledge-node) keyword)
(defun knowledge-node-status (view node)
  "Return NODE's logical status: :SUPPORTED, :CONFLICTED, :REFUTED, or :UNKNOWN.

A supported claim contradicted by another supported claim is conflicted; an
unsupported claim whose explicit negation is supported is refuted."
  (cond
    ((and (knowledge-node-supported-p node) (knowledge-node-conflicts node))
     ':conflicted)
    ((knowledge-node-supported-p node)
     ':supported)
    ((knowledge--opposite-nodes view node)
     ':refuted)
    (t
     ':unknown)))

(-> knowledge-node-applicability (knowledge-node) keyword)
(defun knowledge-node-applicability (node)
  "Return whether NODE applies now, or the strongest reason it does not.

A supported claim is :CURRENT. Otherwise the reason is taken from its supports:
:STALE when evidence changed or disappeared, then :UNVERIFIED, :EXPIRED,
:PENDING, :RETRACTED, and :UNKNOWN when it has no supports at all."
  (if (knowledge-node-supported-p node)
      ':current
      (let ((states (mapcar (lambda (support) (getf support :state))
                            (knowledge-node-supports node))))
        (cond
          ((intersection states '(:changed :missing)) ':stale)
          ((member ':unverified states) ':unverified)
          ((member ':expired states) ':expired)
          ((member ':pending states) ':pending)
          ((member ':retracted states) ':retracted)
          (t ':unknown)))))

(-> knowledge--opposite-nodes (knowledge-view knowledge-node) list)
(defun knowledge--opposite-nodes (view node)
  "Return all supported claims in VIEW that explicitly contradict NODE.

Storage scope identifies ownership, not a separate truth domain within a view."
  (let* ((proposition (knowledge-node-proposition node))
         (opposite (if (knowledge-negation-p proposition)
                       (second proposition)
                       (list :not proposition)))
         (index (if (knowledge-node-negated-p node)
                    (knowledge-view-supported view)
                    (knowledge-view-negated view))))
    (remove-if-not
     (lambda (candidate)
       (knowledge-propositions-equal-p opposite (knowledge-node-proposition candidate)))
     (gethash (knowledge-proposition-predicate proposition) index))))


;;;; -- Bases --

(-> knowledge--basis-set (list) list)
(defun knowledge--basis-set (bases)
  "Return BASES as a canonical, duplicate-free basis set."
  (remove-if-not (lambda (basis) (member basis bases)) *knowledge-basis-order*))

(-> knowledge--add-basis-set (knowledge-node list) null)
(defun knowledge--add-basis-set (node set)
  "Add basis SET to NODE, keeping every minimal set.

Canonical sets draw from the four bases, so there are at most sixteen sets."
  (let ((sets (knowledge-node-bases node)))
    (unless (some (lambda (existing) (subsetp existing set)) sets)
      (setf (knowledge-node-bases node)
            (cons set (remove-if (lambda (existing) (subsetp set existing)) sets)))))
  nil)

(-> knowledge--premise-basis-sets (list) list)
(defun knowledge--premise-basis-sets (premises)
  "Return every distinct union of one basis set from each of PREMISES.

Canonicalizing after each premise bounds the intermediate result by the
sixteen subsets of the four bases, rather than the number of proof paths."
  (let ((sets (list nil)))
    (dolist (premise premises sets)
      (setf sets
            (remove-duplicates
             (loop for set in sets
                   append (loop for alternative in (knowledge-node-bases premise)
                                collect (knowledge--basis-set (append set alternative))))
             :test #'equal)))))

(defparameter *knowledge-established-bases* '(:observation :user-assertion)
  "The bases a fact may rest on to satisfy a gate or an action's requirement by default.")

(-> knowledge-node-admissible-p (knowledge-view knowledge-node &key (:accepted list)) boolean)
(defun knowledge-node-admissible-p
    (view node &key (accepted *knowledge-established-bases*))
  "Return true when NODE may justify an action, a gate, or a diagnosis.

The view must be complete and error-free, and some complete support path must
rest only on ACCEPTED bases with no conflicted premise. Independent paths are
alternatives; a disputed derivation does not disqualify a clean observation."
  (when (or (knowledge-view-incomplete view) (knowledge-view-errors view))
    (return-from knowledge-node-admissible-p nil))
  (let ((states (make-hash-table :test #'eq)))
    (labels ((accepted-support-p (support)
               (and (eq (getf support :state) ':current)
                    (member (getf support :basis) accepted)))

             (accepted-justification-p (justification)
               (every (lambda (identifier)
                        (let ((premise (knowledge-view-node view identifier)))
                          (and premise (admissible-p premise))))
                      (getf justification :premises)))

             (admissible-p (candidate)
               (multiple-value-bind (state known-p) (gethash candidate states)
                 (if known-p
                     (eq state ':accepted)
                     (progn
                       ;; A proof cannot justify itself. Admitted rules are acyclic,
                       ;; but guard the traversal if a malformed view contains a cycle.
                       (setf (gethash candidate states) ':visiting)
                       (let ((admitted-p
                               (and (knowledge-node-supported-p candidate)
                                    (not (knowledge-node-conflicts candidate))
                                    (or (some #'accepted-support-p
                                              (knowledge-node-supports candidate))
                                        (some #'accepted-justification-p
                                              (knowledge-node-justifications candidate))))))
                         (setf (gethash candidate states)
                               (if admitted-p ':accepted ':rejected))
                         (and admitted-p t)))))))
      (admissible-p node))))

(-> knowledge-node-observed-p (knowledge-node &key (:accepted list)) boolean)
(defun knowledge-node-observed-p (node &key (accepted '(:observation :user-assertion)))
  "Return true when some path supporting NODE rests only on ACCEPTED bases.

For decisions, KNOWLEDGE-NODE-ADMISSIBLE-P also validates the view and every
premise of a support path."
  (and (some (lambda (set) (subsetp set accepted)) (knowledge-node-bases node)) t))


;;;; -- Building the View --

(-> knowledge--view-node (knowledge-view string list memory-scope (option string)) knowledge-node)
(defun knowledge--view-node (view identifier proposition scope workspace)
  "Return VIEW's node IDENTIFIER, creating it for PROPOSITION when absent."
  (or (gethash identifier (knowledge-view-nodes view))
      (setf (gethash identifier (knowledge-view-nodes view))
            (make-instance 'knowledge-node
                           :identifier identifier
                           :proposition proposition
                           :scope scope
                           :workspace workspace
                           :schema (knowledge-base-schema
                                    (knowledge-view-base view)
                                    (knowledge-proposition-predicate proposition)
                                    :workspace (knowledge-view-workspace view))))))

(-> knowledge--index-supported (knowledge-view knowledge-node) null)
(defun knowledge--index-supported (view node)
  "Index supported NODE for rule matching, once."
  (let* ((proposition (knowledge-node-proposition node))
         (predicate (knowledge-proposition-predicate proposition)))
    (if (knowledge-node-negated-p node)
        (pushnew node (gethash predicate (knowledge-view-negated view)))
        (unless (member node (gethash predicate (knowledge-view-supported view)))
          (push node (gethash predicate (knowledge-view-supported view)))
          (loop for term in (knowledge-proposition-arguments proposition)
                for position from 0
                when (stringp term)
                  do (push node (gethash (list predicate position term)
                                         (knowledge-view-argument-index view)))))))
  nil)

(-> knowledge--note-boundaries (knowledge-view list (option list)) null)
(defun knowledge--note-boundaries (view support evidence)
  "Lower VIEW's boundary to SUPPORT's or EVIDENCE's next validity change.

A support becomes valid at its valid-from time and stops being valid the
second after its valid-until time, so those are the instants that change it."
  (let ((now (knowledge-view-now view)))
    (dolist (time (list (getf support :valid-from)
                        (let ((until (getf support :valid-until))) (and until (1+ until)))
                        (let ((until (and evidence (getf evidence :valid-until))))
                          (and until (1+ until)))))
      (when (and time (> time now)
                 (or (null (knowledge-view-boundary view))
                     (< time (knowledge-view-boundary view))))
        (setf (knowledge-view-boundary view) time))))
  nil)

(-> knowledge--evaluate-stored (knowledge-view evidence-check) null)
(defun knowledge--evaluate-stored (view check)
  "Add every visible stored claim to VIEW with the state of each support."
  (let ((base (knowledge-view-base view))
        (workspace (knowledge-view-workspace view)))
    (loop for entry being the hash-values of (knowledge-base-claims base)
          when (knowledge-visible-p (getf entry :scope) (getf entry :workspace) workspace)
            do (let ((node (knowledge--view-node view (getf entry :id)
                                                 (getf entry :proposition)
                                                 (getf entry :scope)
                                                 (getf entry :workspace))))
                 (dolist (support-identifier (getf entry :supports))
                   (let ((support (knowledge-base-support base support-identifier)))
                     (knowledge--note-boundaries view support
                                                 (and (getf support :evidence)
                                                      (knowledge-base-evidence-record
                                                       base (getf support :evidence))))
                     (multiple-value-bind (state reason)
                         (evidence-check-support check base support)
                       (push (list :id support-identifier
                                   :state state
                                   :reason reason
                                   :basis (getf support :basis)
                                   :evidence (getf support :evidence))
                             (knowledge-node-supports node))
                       (when (eq state ':current)
                         (knowledge--add-basis-set node (list (getf support :basis)))))))
                 (when (knowledge-node-supported-p node)
                   (knowledge--index-supported view node)))))
  nil)

(-> knowledge--rule-order (list) list)
(defun knowledge--rule-order (rules)
  "Return RULES so that each runs after every rule concluding a predicate it reads."
  (let ((producers (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'eq))
        (ordered nil))
    (dolist (item rules)
      (push item (gethash (knowledge-rule-conclusion-predicate (third item)) producers)))
    (labels ((visit (item)
               (unless (gethash item visited)
                 (setf (gethash item visited) t)
                 (dolist (predicate (knowledge-rule-premise-predicates (third item)))
                   (mapc #'visit (gethash predicate producers)))
                 (push item ordered))))
      (mapc #'visit rules))
    (nreverse ordered)))

(-> knowledge--relevant-rules (list list) list)
(defun knowledge--relevant-rules (rules goals)
  "Return the RULES that can contribute to predicates in GOALS, or all when GOALS is empty."
  (if (null goals)
      rules
      (let ((needed (make-hash-table :test #'equal))
            (pending (copy-list goals)))
        (loop while pending
              do (let ((predicate (pop pending)))
                   (unless (gethash predicate needed)
                     (setf (gethash predicate needed) t)
                     (dolist (item rules)
                       (when (string= (knowledge-rule-conclusion-predicate (third item))
                                      predicate)
                         (setf pending (append (knowledge-rule-premise-predicates (third item))
                                               pending)))))))
        (remove-if-not (lambda (item)
                         (gethash (knowledge-rule-conclusion-predicate (third item)) needed))
                       rules))))

(-> knowledge--candidates (knowledge-view list list) list)
(defun knowledge--candidates (view pattern environment)
  "Return supported nodes that could match positive PATTERN under ENVIRONMENT.

A bound or literal string argument selects candidates through the argument index."
  (let ((predicate (first pattern)))
    (loop for item in (rest pattern)
          for position from 0
          for value = (if (knowledge-variable-p item)
                          (rest (assoc item environment :test #'string=))
                          item)
          when (stringp value)
            do (return-from knowledge--candidates
                 (values (gethash (list predicate position value)
                                  (knowledge-view-argument-index view)))))
    (values (gethash predicate (knowledge-view-supported view)))))

(-> knowledge--unify (list list list) (values list boolean))
(defun knowledge--unify (pattern proposition environment)
  "Extend ENVIRONMENT so PATTERN's arguments match PROPOSITION's, or fail.

Return the extended environment and whether matching succeeded."
  (let ((result environment))
    (loop for item in (rest pattern)
          for term in (knowledge-proposition-arguments proposition)
          do (if (knowledge-variable-p item)
                 (let ((bound (assoc item result :test #'string=)))
                   (cond
                     ((null bound)
                      (push (cons item term) result))
                     ((not (knowledge-term= (rest bound) term))
                      (return-from knowledge--unify (values nil nil)))))
                 (unless (knowledge-term= item term)
                   (return-from knowledge--unify (values nil nil)))))
    (values result t)))

(-> knowledge--instantiate (list list) list)
(defun knowledge--instantiate (pattern environment)
  "Return PATTERN with its variables replaced by their values in ENVIRONMENT."
  (if (knowledge-negation-p pattern)
      (list :not (knowledge--instantiate (second pattern) environment))
      (cons (first pattern)
            (mapcar (lambda (item)
                      (if (knowledge-variable-p item)
                          (rest (assoc item environment :test #'string=))
                          item))
                    (rest pattern)))))

(-> knowledge--negation-nodes (knowledge-view list list &key (:test (option function))) list)
(defun knowledge--negation-nodes (view pattern environment &key test)
  "Return all supported explicit negations of PATTERN under ENVIRONMENT.

When TEST is given, include only negations it accepts. Distinct visible scopes
are independent support paths even when their propositions agree."
  (let ((atom (knowledge--instantiate pattern environment)))
    (remove-if-not (lambda (node)
                     (and (knowledge-propositions-equal-p
                           (second (knowledge-node-proposition node)) atom)
                          (or (null test) (funcall test node))))
                   (gethash (first atom) (knowledge-view-negated view)))))

(-> knowledge--record-error (knowledge-view list integer string) null)
(defun knowledge--record-error (view rule version message)
  "Retain one refused derivation of RULE VERSION with MESSAGE."
  (when (< (length (knowledge-view-errors view)) *knowledge-error-limit*)
    (pushnew (list :rule (getf (rest rule) :id) :version version :message message)
             (knowledge-view-errors view)
             :test #'equal))
  nil)

(-> knowledge--conclude (knowledge-view list list integer list list) null)
(defun knowledge--conclude (view entry rule version premises environment)
  "Derive RULE's conclusion for one complete match of PREMISES under ENVIRONMENT."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (variable . value) in environment
          do (setf (gethash variable table) value))
    (handler-case
        (progn
          (dolist (binding (getf (rest rule) :bindings))
            (setf (gethash (first binding) table)
                  (knowledge-evaluate-expression (second binding) table)))
          (when (every (lambda (guard) (knowledge-guard-holds-p guard table))
                       (getf (rest rule) :guards))
            (let* ((pattern (getf (rest rule) :conclusion))
                   (proposition (knowledge--instantiate
                                 pattern
                                 (loop for variable being the hash-keys of table
                                         using (hash-value value)
                                       collect (cons variable value))))
                   (schema (knowledge-base-schema (knowledge-view-base view)
                                                  (knowledge-proposition-predicate proposition)
                                                  :workspace (knowledge-view-workspace view))))
              (unless (and schema (knowledge-proposition-valid-p proposition schema))
                (knowledge--fail ':evaluation proposition
                                 "The conclusion does not fit the schema of ~A."
                                 (knowledge-proposition-predicate proposition)))
              (let* ((workspace-p (or (eq (getf entry :scope) ':workspace)
                                      (some (lambda (premise)
                                              (eq (knowledge-node-scope premise) ':workspace))
                                            premises)))
                     (scope (if workspace-p ':workspace ':global))
                     (workspace (and workspace-p (knowledge-view-workspace view)))
                     (node (knowledge--view-node
                            view
                            (knowledge-claim-identifier proposition
                                                        :scope scope :workspace workspace)
                            proposition scope workspace))
                     (premise-identifiers (mapcar #'knowledge-node-identifier premises))
                     (justification
                       (knowledge-digest-identifier
                        #\j (list (getf entry :id) version premise-identifiers))))
                (unless (find justification (knowledge-node-justifications node)
                              :key (lambda (item) (getf item :id)) :test #'string=)
                  (incf (knowledge-view-matches view))
                  (push (list :id justification
                              :rule (getf entry :id)
                              :version version
                              :premises premise-identifiers
                              :bindings (loop for binding in (getf (rest rule) :bindings)
                                              collect (cons (first binding)
                                                            (gethash (first binding) table))))
                        (knowledge-node-justifications node))
                  (dolist (set (knowledge--premise-basis-sets premises))
                    (knowledge--add-basis-set node set))
                  (knowledge--index-supported view node))))))
      (knowledge-error (condition)
        (knowledge--record-error view rule version (autolith-error-message condition)))))
  nil)

(-> knowledge--run-rule (knowledge-view list) null)
(defun knowledge--run-rule (view item)
  "Derive every conclusion of one admitted rule ITEM, (ENTRY VERSION RULE), within limits."
  (destructuring-bind (entry version rule) item
    (let ((positive (remove-if #'knowledge-negation-p (getf (rest rule) :premises)))
          (negative (remove-if-not #'knowledge-negation-p (getf (rest rule) :premises))))
      (labels ((exhausted-p ()
                 (or (>= (knowledge-view-matches view) *knowledge-match-limit*)
                     (>= (knowledge-view-steps view) *knowledge-step-limit*)))

               (match-negative (patterns environment premises)
                 (cond
                   ((exhausted-p)
                    (pushnew (getf entry :id) (knowledge-view-incomplete view)
                             :test #'string=))
                   ((null patterns)
                    (knowledge--conclude view entry rule version
                                         (reverse premises) environment))
                   (t
                    (dolist (node (knowledge--negation-nodes
                                   view (second (first patterns)) environment))
                      (when (exhausted-p)
                        (pushnew (getf entry :id) (knowledge-view-incomplete view)
                                 :test #'string=)
                        (return))
                      (incf (knowledge-view-steps view))
                      (match-negative (rest patterns) environment (cons node premises))))))

               (match (patterns environment premises)
                 (cond
                   ((exhausted-p)
                    (pushnew (getf entry :id) (knowledge-view-incomplete view)
                             :test #'string=))
                   ((null patterns)
                     (match-negative negative environment premises))
                   (t
                    (dolist (node (knowledge--candidates view (first patterns) environment))
                      (when (exhausted-p)
                        (pushnew (getf entry :id) (knowledge-view-incomplete view)
                                 :test #'string=)
                        (return))
                      (incf (knowledge-view-steps view))
                      (multiple-value-bind (extended matched-p)
                          (knowledge--unify (first patterns) (knowledge-node-proposition node)
                                            environment)
                        (when matched-p
                          (match (rest patterns) extended (cons node premises)))))))))
        (match positive nil nil))))
  nil)

(-> knowledge--detect-conflicts (knowledge-view) null)
(defun knowledge--detect-conflicts (view)
  "Mark supported claims that disagree on a keyed value or contradict a negation."
  (let ((groups (make-hash-table :test #'equal)))
    (loop for node being the hash-values of (knowledge-view-nodes view)
          when (knowledge-node-supported-p node)
            do (if (knowledge-node-negated-p node)
                   (dolist (positive (knowledge--opposite-nodes view node))
                     (pushnew (knowledge-node-identifier positive)
                              (knowledge-node-conflicts node) :test #'string=)
                     (pushnew (knowledge-node-identifier node)
                              (knowledge-node-conflicts positive) :test #'string=))
                   (let ((key (and (knowledge-node-schema node)
                                   (knowledge-proposition-key (knowledge-node-proposition node)
                                                              (knowledge-node-schema node)))))
                     (when key
                       (push node (gethash (prin1-to-string key) groups))))))
    (loop for nodes being the hash-values of groups
          do (dolist (node nodes)
               (dolist (other nodes)
                 (unless (or (eq node other)
                             (knowledge-propositions-equal-p (knowledge-node-proposition node)
                                                             (knowledge-node-proposition other)))
                   (pushnew (knowledge-node-identifier other) (knowledge-node-conflicts node)
                            :test #'string=))))))
  nil)

(-> knowledge-evaluate
    (knowledge-base &key (:workspace (option string)) (:check evidence-check) (:goals list))
    knowledge-view)
(defun knowledge-evaluate (base &key workspace check goals)
  "Return the view of BASE from WORKSPACE, with evidence decided by CHECK.

GOALS, a list of predicates, limits rule evaluation to rules that can
contribute to them; by default every admitted rule runs. A view whose work
limits were reached lists the unfinished rules as incomplete; their absent
conclusions are unknown, never false."
  (let ((view (make-instance 'knowledge-view
                             :base base
                             :workspace workspace
                             :now (evidence-check-now check))))
    (knowledge--evaluate-stored view check)
    (dolist (item (knowledge--rule-order
                   (knowledge--relevant-rules
                    (knowledge-base-active-rules base :workspace workspace) goals)))
      (knowledge--run-rule view item))
    (knowledge--detect-conflicts view)
    view))

(-> knowledge-view-node (knowledge-view string) (option knowledge-node))
(defun knowledge-view-node (view identifier)
  "Return VIEW's evaluated claim IDENTIFIER, or NIL."
  (values (gethash identifier (knowledge-view-nodes view))))

(-> knowledge-view-holds (knowledge-view list) list)
(defun knowledge-view-holds (view pattern)
  "Return VIEW's supported claims matching PATTERN, a proposition with variables.

A negated PATTERN matches supported explicit negations."
  (let ((negated-p (knowledge-negation-p pattern))
        (atom (knowledge-proposition-atom pattern)))
    (remove-if-not
     (lambda (node)
       (nth-value 1 (knowledge--unify atom
                                      (knowledge-proposition-atom
                                       (knowledge-node-proposition node))
                                      nil)))
     (if negated-p
         (gethash (first atom) (knowledge-view-negated view))
         (gethash (first atom) (knowledge-view-supported view))))))


;;;; -- Queries --

(defparameter *knowledge-solution-limit* 64
  "The most solutions one pattern query returns.")

(-> knowledge-view-solve (knowledge-view list &key (:limit integer) (:test (option function)))
    (values list boolean))
(defun knowledge-view-solve (view patterns &key (limit *knowledge-solution-limit*) test)
  "Return the ways VIEW's supported facts satisfy PATTERNS together.

Each solution is (BINDINGS . NODES): an alist of variables to terms and the
facts used, those matching the positive patterns in order followed by the
explicit negations the negated patterns required, as in rules. When TEST is
given, only facts it accepts are used. The second value is true when the
limit or the view's step limit cut the search short, so absent solutions are
unknown rather than impossible."
  (let ((positive (remove-if #'knowledge-negation-p patterns))
        (negative (remove-if-not #'knowledge-negation-p patterns))
        (solutions nil)
        (count 0)
        (steps 0)
        (cut-p nil))
    (labels ((match-negative (remaining environment nodes)
               (cond
                 ((or (>= count limit) (>= steps *knowledge-step-limit*))
                  (setf cut-p t))
                 ((null remaining)
                  (incf count)
                  (push (cons (reverse environment) (reverse nodes)) solutions))
                 (t
                  (dolist (node (knowledge--negation-nodes
                                 view (second (first remaining)) environment :test test))
                    (when (or (>= count limit) (>= steps *knowledge-step-limit*))
                      (setf cut-p t)
                      (return))
                    (incf steps)
                    (match-negative (rest remaining) environment (cons node nodes))))))

             (match (remaining environment nodes)
               (cond
                 ((or (>= count limit) (>= steps *knowledge-step-limit*))
                  (setf cut-p t))
                 ((null remaining)
                   (match-negative negative environment nodes))
                 (t
                  (dolist (node (remove-if-not (lambda (node) (or (null test) (funcall test node)))
                                               (knowledge--candidates view (first remaining)
                                                                      environment)))
                    (incf steps)
                    (multiple-value-bind (extended matched-p)
                        (knowledge--unify (first remaining) (knowledge-node-proposition node)
                                          environment)
                      (when matched-p
                        (match (rest remaining) extended (cons node nodes)))))))))
      (match positive nil nil))
    (values (nreverse solutions) cut-p)))

(-> knowledge-pattern-text (list) string)
(defun knowledge-pattern-text (pattern)
  "Return PATTERN, possibly negated and with variables, as compact text."
  (if (knowledge-negation-p pattern)
      (format nil "not ~A" (knowledge-pattern-text (second pattern)))
      (format nil "~A(~{~A~^, ~})" (first pattern)
              (mapcar (lambda (item)
                        (if (knowledge-variable-p item) item (knowledge-term-display item)))
                      (rest pattern)))))


;;;; -- Current Views --

(defvar *knowledge-view-cache* (make-hash-table :test #'equal :synchronized t)
  "The latest view per (knowledge log, workspace), with the state it was evaluated from.")

(defparameter *knowledge-unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0)
  "The universal time of the Unix epoch, which file modification times count from.")

(-> knowledge--file-fingerprint (pathname) t)
(defun knowledge--file-fingerprint (pathname)
  "Return the digest of PATHNAME's text, or why it could not be read."
  (multiple-value-bind (text failure) (memory-source-read-file pathname)
    (if text (memory-source-text-digest text) failure)))

(-> knowledge--file-unchanged-p (list timestamp) boolean)
(defun knowledge--file-unchanged-p (file evaluated-at)
  "Return true when cached FILE, (PATHNAME STATUS-KEY FINGERPRINT), still holds what was read.

File times have one-second resolution, so a file modified in the second the
view was evaluated, or later, has its content compared instead of its status."
  (destructuring-bind (pathname status fingerprint) file
    (let ((current (knowledge-file-status-key pathname)))
      (and (if (and (equal current status)
                    (consp current)
                    (< (+ (third current) *knowledge-unix-epoch*) evaluated-at))
               t
               (equal (knowledge--file-fingerprint pathname) fingerprint))
           t))))

(-> knowledge--view-cache-valid-p (list knowledge-base t timestamp) boolean)
(defun knowledge--view-cache-valid-p (entry base memory-key now)
  "Return true when cached view ENTRY still describes BASE, the memories, and the files at NOW."
  (let ((view (getf entry :view)))
    (and view
         (eq (knowledge-view-base view) base)
         (equal (getf entry :memory-key) memory-key)
         (or (eq memory-key ':missing)
             (< (+ (third memory-key) *knowledge-unix-epoch*) (getf entry :evaluated-at)))
         (>= now (knowledge-view-now view))
         (let ((boundary (knowledge-view-boundary view)))
           (or (null boundary) (< now boundary)))
         (every (lambda (file) (knowledge--file-unchanged-p file (getf entry :evaluated-at)))
                (getf entry :files))
         t)))

(-> knowledge-view-current (configuration &key (:workspace (option string))) knowledge-view)
(defun knowledge-view-current (configuration &key workspace)
  "Return CONFIGURATION's knowledge from WORKSPACE now, reusing an evaluation still valid.

A view is reused while the knowledge log, the memory log, and every file it
read are unchanged and no support or evidence has started or stopped being
valid since. Recall and listings in a session therefore evaluate once per
change, not once per request. Views are shared and must not be modified."
  (let* ((workspace (or workspace (knowledge--workspace configuration)))
         (base (knowledge-base-read configuration))
         (now (knowledge-now))
         (memory-key (knowledge-file-status-key (configuration-memory-path configuration)))
         (key (list (namestring (configuration-knowledge-path configuration)) workspace))
         (cached (gethash key *knowledge-view-cache*)))
    (if (and cached (knowledge--view-cache-valid-p cached base memory-key now))
        (getf cached :view)
        (let* ((evaluated-at (get-universal-time))
               (check (evidence-check-create
                       :now now
                       :memories (memory-list configuration :visibility ':all)))
               (view (knowledge-evaluate base :workspace workspace :check check)))
          (setf (gethash key *knowledge-view-cache*)
                (list :view view
                      :memory-key memory-key
                      :evaluated-at evaluated-at
                      :files (loop for pathname being the hash-keys of (evidence-check-files check)
                                     using (hash-value entry)
                                   collect (list (pathname pathname)
                                                 (third entry)
                                                 (if (first entry)
                                                     (memory-source-text-digest (first entry))
                                                     (second entry))))))
          view))))
