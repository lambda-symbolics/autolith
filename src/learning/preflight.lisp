(in-package #:autolith)

;;;; -- Grounded Procedure Preflight --

(-> procedure--preflight-variable () string)
(defun procedure--preflight-variable ()
  "Return a fresh variable representing an as-yet unobserved value."
  (format nil "?future-~A" (remove #\- (make-identifier))))

(-> procedure--preflight-resolve (t list) t)
(defun procedure--preflight-resolve (value environment)
  "Substitute ENVIRONMENT throughout VALUE, following symbolic variable aliases."
  (cond
    ((knowledge-variable-p value)
     (let ((binding (assoc value environment :test #'equal)))
       (if binding
           (procedure--preflight-resolve (rest binding) environment)
           value)))
    ((consp value)
     (cons (procedure--preflight-resolve (first value) environment)
           (procedure--preflight-resolve (rest value) environment)))
    (t value)))

(-> procedure--preflight-unify (t t list) (values list boolean))
(defun procedure--preflight-unify (left right environment)
  "Unify two symbolic terms without replacing future values by concrete guesses."
  (let ((left (procedure--preflight-resolve left environment))
        (right (procedure--preflight-resolve right environment)))
    (cond
      ((equal left right) (values environment t))
      ((knowledge-variable-p left) (values (acons left right environment) t))
      ((knowledge-variable-p right) (values (acons right left environment) t))
      ((and (consp left) (consp right))
       (multiple-value-bind (next matched-p)
           (procedure--preflight-unify (first left) (first right) environment)
         (if matched-p
             (procedure--preflight-unify (rest left) (rest right) next)
             (values nil nil))))
      (t (values nil nil)))))

(-> procedure--preflight-solutions
    (&key (:view knowledge-view) (:patterns list) (:produced list)
          (:accepted list) (:environment list)) list)
(defun procedure--preflight-solutions (&key view patterns produced accepted environment)
  "Return possible environments for PATTERNS using current facts and earlier productions.

Future observations are variables, never stored facts. Resolve explicit negation,
argument equality, accepted bases, and admitted derivations before deferring a
requirement. Guards and computed values still depending on future observations
are conditional until the ordinary per-call check. Bound the entire proof search."
  (let ((steps 0)
        (oppositions (make-hash-table :test #'equal))
        (rules (knowledge-base-active-rules (knowledge-view-base view)
                                           :workspace (knowledge-view-workspace view))))
    (labels ((spend ()
               (when (> (incf steps) *knowledge-step-limit*)
                 (knowledge--fail ':procedure-preflight-unknown patterns
                                  "Procedure preflight exceeded its proof search limit.")))

             (variables (value)
               (cond
                 ((knowledge-variable-p value) (list value))
                 ((consp value) (append (variables (first value)) (variables (rest value))))))

             (fresh-rule (rule)
               (procedure--preflight-resolve
                rule (mapcar (lambda (variable)
                               (cons variable (procedure--preflight-variable)))
                             (remove-duplicates (variables rule) :test #'equal))))

             (incompatible-p (left right)
               (and (equal (knowledge-proposition-predicate left)
                           (knowledge-proposition-predicate right))
                    (if (eq (knowledge-negation-p left) (knowledge-negation-p right))
                        (and (not (knowledge-negation-p left))
                             (let* ((schema (knowledge-base-schema
                                             (knowledge-view-base view)
                                             (knowledge-proposition-predicate left)
                                             :workspace (knowledge-view-workspace view)))
                                    (key (and schema (knowledge-proposition-key left schema))))
                               (and key (equal key (knowledge-proposition-key right schema))
                                    (some (lambda (left-term right-term)
                                            (and (null (variables left-term))
                                                 (null (variables right-term))
                                                 (not (equal left-term right-term))))
                                          (knowledge-proposition-arguments left)
                                          (knowledge-proposition-arguments right)))))
                        (equal (knowledge-proposition-atom left)
                               (knowledge-proposition-atom right)))))

             (finish-rule (rule bindings &key definite-p)
               (let ((table (make-hash-table :test #'equal)))
                 (labels ((refresh ()
                            (dolist (variable (variables rule))
                              (setf (gethash variable table)
                                    (procedure--preflight-resolve variable bindings))))

                          (known-p (expression)
                            (null (variables (procedure--preflight-resolve expression bindings)))))
                   (dolist (binding (getf (rest rule) :bindings))
                     (cond
                       ((known-p (second binding))
                        (refresh)
                        (multiple-value-bind (next matched-p)
                            (procedure--preflight-unify
                             (first binding)
                             (knowledge-evaluate-expression (second binding) table)
                             bindings)
                          (unless matched-p (return-from finish-rule nil))
                          (setf bindings next)))
                       (definite-p
                        (return-from finish-rule nil))))
                   (refresh)
                   (when (every (lambda (guard)
                                  (if (known-p (rest guard))
                                      (knowledge-guard-holds-p guard table)
                                      (not definite-p)))
                                (getf (rest rule) :guards))
                     (list bindings)))))

             (raw-prove (pattern bindings)
               ;; Opposition counts every supported basis, including disputed facts.
               ;; Do not recursively require opponents to be undisputed themselves.
               (spend)
               (let* ((pattern (procedure--preflight-resolve pattern bindings))
                      (predicate (knowledge-proposition-predicate pattern))
                      (answers nil))
                 (dolist (candidate
                          (append (mapcar #'knowledge-node-proposition
                                          (append (gethash predicate (knowledge-view-supported view))
                                                  (gethash predicate (knowledge-view-negated view))))
                                  (mapcar #'first produced)))
                   (multiple-value-bind (next matched-p)
                       (procedure--preflight-unify pattern candidate bindings)
                     (when matched-p (push next answers))))
                 (dolist (item rules)
                   (let* ((rule (fresh-rule (third item)))
                          (conclusion (getf (rest rule) :conclusion)))
                     (when (and (eq (knowledge-negation-p pattern) (knowledge-negation-p conclusion))
                                (equal predicate (knowledge-proposition-predicate conclusion)))
                       (multiple-value-bind (next matched-p)
                           (procedure--preflight-unify pattern conclusion bindings)
                         (when matched-p
                           (dolist (solution (raw-solve (getf (rest rule) :premises) next))
                             (setf answers (nconc (finish-rule rule solution :definite-p t) answers))))))))
                 answers))

             (raw-solve (remaining bindings)
               (spend)
               (if (null remaining)
                   (list bindings)
                   (loop for next in (raw-prove (first remaining) bindings)
                         append (raw-solve (rest remaining) next))))

             (opposition-candidates (predicate bindings)
               (let* ((future-state (mapcar (lambda (variable)
                                             (procedure--preflight-resolve variable bindings))
                                           (remove-duplicates (variables produced) :test #'equal)))
                      (cache-key (list predicate future-state)))
                 (multiple-value-bind (cached present-p) (gethash cache-key oppositions)
                   (if present-p
                       cached
                       (let* ((schema (knowledge-base-schema
                                       (knowledge-view-base view) predicate
                                       :workspace (knowledge-view-workspace view)))
                              (atom (cons predicate
                                          (mapcar (lambda (argument)
                                                    (declare (ignore argument))
                                                    (procedure--preflight-variable))
                                                  (knowledge-schema-arguments schema))))
                              (unbound-futures (remove-duplicates
                                                (remove-if-not #'knowledge-variable-p future-state)
                                                :test #'equal))
                              (candidates nil))
                         (dolist (pattern (list atom (list :not atom)))
                           (dolist (solution (raw-prove pattern bindings))
                             (let ((roots (mapcar (lambda (variable)
                                                   (procedure--preflight-resolve variable solution))
                                                 unbound-futures)))
                               ;; Permit aliases, but no additional guesses or equalities
                               ;; beyond the selected joint requirement bindings.
                               (when (and (every #'knowledge-variable-p roots)
                                          (= (length roots)
                                             (length (remove-duplicates roots :test #'equal))))
                                 (let ((aliases (loop for root in roots
                                                      for variable in unbound-futures
                                                      unless (equal root variable)
                                                        collect (cons root variable))))
                                   (pushnew (procedure--preflight-resolve
                                             (procedure--preflight-resolve pattern solution) aliases)
                                            candidates :test #'equal))))))
                         (setf (gethash cache-key oppositions) candidates))))))

             (opposed-p (pattern bindings)
               (let ((resolved (procedure--preflight-resolve pattern bindings)))
                 (some (lambda (candidate)
                         (incompatible-p resolved (procedure--preflight-resolve candidate bindings)))
                       (opposition-candidates (knowledge-proposition-predicate resolved) bindings))))

             (finish-proof (proof)
               ;; Each proof keeps its bindings, premise patterns, and applied rules.
               ;; Propagate newly grounded computations before checking the entire path.
               (let ((bindings (first proof)))
                 (loop for previous = bindings
                       do (dolist (rule (third proof))
                            (spend)
                            (let ((solutions (finish-rule rule bindings)))
                              (unless solutions (return-from finish-proof nil))
                              (setf bindings (first solutions))))
                       until (eq previous bindings))
                 (unless (some (lambda (premise) (opposed-p premise bindings)) (second proof))
                   (list bindings (second proof) (third proof)))))

             (prove (pattern bindings)
               (spend)
               (let ((answers nil)
                     (pattern (procedure--preflight-resolve pattern bindings)))
                 (dolist (node (knowledge-view-holds view pattern))
                   ;; Reconstruct derived paths through their rules, rather than treating
                   ;; a current conclusion as independent of its future-invalidated premises.
                   (when (and (knowledge-node-admissible-p view node :accepted accepted)
                              (some (lambda (support)
                                      (and (eq (getf support :state) ':current)
                                           (member (getf support :basis) accepted)))
                                    (knowledge-node-supports node)))
                     (multiple-value-bind (next matched-p)
                         (procedure--preflight-unify pattern (knowledge-node-proposition node) bindings)
                       (when (and matched-p (not (opposed-p pattern next)))
                         (push (list next (list pattern) nil) answers)))))
                 (dolist (production produced)
                   (when (member (second production) accepted)
                     (multiple-value-bind (next matched-p)
                         (procedure--preflight-unify pattern (first production) bindings)
                       (when (and matched-p (not (opposed-p pattern next)))
                         (push (list next (list pattern) nil) answers)))))
                 (dolist (item rules)
                   (let* ((rule (fresh-rule (third item)))
                          (conclusion (getf (rest rule) :conclusion)))
                     (when (and (eq (knowledge-negation-p pattern) (knowledge-negation-p conclusion))
                                (equal (knowledge-proposition-predicate pattern)
                                       (knowledge-proposition-predicate conclusion)))
                       (multiple-value-bind (next matched-p)
                           (procedure--preflight-unify pattern conclusion bindings)
                         (when matched-p
                           (dolist (proof (solve (getf (rest rule) :premises) next))
                             (let ((finished (finish-proof
                                              (list (first proof)
                                                    (cons pattern (second proof))
                                                    (append (third proof) (list rule))))))
                               (when finished (push finished answers)))))))))
                 answers))

             (solve (remaining bindings)
               (spend)
               (if (null remaining)
                   (list (list bindings nil nil))
                   (loop for head in (prove (first remaining) bindings)
                         append
                         (loop for tail in (solve (rest remaining) (first head))
                               for finished = (finish-proof
                                               (list (first tail)
                                                     (append (second head) (second tail))
                                                     (append (third head) (third tail))))
                               when finished collect finished)))))
      (when (null (variables (mapcar #'rest environment)))
        (multiple-value-bind (current cut-p)
            (knowledge-view-solve
             view (mapcar (lambda (pattern) (knowledge--instantiate-pattern pattern environment)) patterns)
             :test (lambda (node) (knowledge-node-admissible-p view node :accepted accepted)))
          (declare (ignore current))
          (when cut-p
            (knowledge--fail ':procedure-preflight-unknown patterns
                             "Procedure preflight could not complete the current requirement search."))))
      (let* ((solutions (mapcar #'first (solve patterns environment)))
             (names (remove-duplicates (append (variables patterns) (mapcar #'first environment))
                                       :test #'equal))
             (resolved
               (remove-duplicates
                (mapcar (lambda (solution)
                          (mapcar (lambda (name)
                                    (cons name (procedure--preflight-resolve name solution)))
                                  names))
                        solutions)
                :test #'equal)))
        (when (rest resolved)
          (knowledge--fail ':procedure-inapplicable patterns
                           "Procedure preflight found ambiguous current or earlier-call requirement bindings."))
        solutions))))

(-> procedure--preflight-productions
    (&key (:contract list) (:environment list) (:arguments hash-table)
          (:view knowledge-view) (:result-variable function)) list)
(defun procedure--preflight-productions (&key contract environment arguments view result-variable)
  "Return symbolic predictions and observations of one applicable earlier call."
  (let ((unknowns (make-hash-table :test #'equal))
        (productions nil))
    (labels ((resolve (value)
               (let ((value (procedure--preflight-resolve value environment)))
                 (if (and (knowledge-variable-p value)
                          (not (uiop:string-prefix-p "?future-" value)))
                     (or (gethash value unknowns)
                         (setf (gethash value unknowns) (procedure--preflight-variable)))
                     value))))
      (dolist (effect (getf (rest contract) :effects))
        (push (list (knowledge--instantiate-pattern
                     effect (mapcar (lambda (variable) (cons variable (resolve variable)))
                                    (knowledge--pattern-variables effect)))
                    :model-proposal)
              productions))
      (dolist (observation (getf (rest contract) :observes))
        (let* ((predicate (getf observation :predicate))
               (schema (knowledge-base-schema (knowledge-view-base view) predicate
                                              :workspace (knowledge-view-workspace view)))
               (valid-p t)
               (values
                 (loop for argument in (getf observation :arguments)
                       for specification in (knowledge-schema-arguments schema)
                       collect
                       (cond
                         ((and (knowledge--reference-p argument)
                               (not (uiop:string-prefix-p "$arg." argument)))
                          (funcall result-variable
                                   (if (string= argument "$result")
                                       nil
                                       (uiop:split-string (subseq argument 8) :separator '(#\.)))))
                         ((knowledge--reference-p argument)
                          (multiple-value-bind (value present-p)
                              (knowledge--reference-value argument arguments nil)
                            (unless present-p (setf valid-p nil))
                            (and present-p
                                 (if (knowledge-variable-p value)
                                     value
                                     (knowledge-term-from-input value (getf specification :type))))))
                         ((knowledge-variable-p argument) (resolve argument))
                         (t (knowledge-term-from-input argument (getf specification :type)))))))
          (when valid-p (push (list (cons predicate values) :observation) productions)))))
    productions))


(-> procedure--preflight-arguments (list list function) t)
(defun procedure--preflight-arguments (template bindings result-variable)
  "Instantiate known argument fields, retaining result references as symbolic variables."
  (case (first template)
    (:from (funcall result-variable (second template) (third template)))
    (:object
     (let ((arguments (json-object)))
       (dolist (entry (rest template) arguments)
         (setf (gethash (first entry) arguments)
               (procedure--preflight-arguments (second entry) bindings result-variable)))))
    (:array
     (coerce (mapcar (lambda (item)
                       (procedure--preflight-arguments item bindings result-variable))
                     (rest template)) 'vector))
    (otherwise (procedure-template-instantiate template bindings nil))))

(-> procedure--preflight-matches (knowledge-view string hash-table) list)
(defun procedure--preflight-matches (view tool arguments)
  "Match applicable contracts while retaining unknown scalar arguments symbolically."
  (let ((base (knowledge-view-base view))
        (workspace (knowledge-view-workspace view)))
    (loop for contract in (knowledge-base-contracts base tool :workspace workspace)
          for (environment matched-p) = (multiple-value-list
                                         (knowledge--contract-bindings contract arguments))
          when matched-p
            collect
            (list contract
                  (loop for binding in environment
                        append (if (knowledge-variable-p (rest binding))
                                   (list binding)
                                   (knowledge--typed-environment
                                    (append (getf (rest contract) :requires)
                                            (getf (rest contract) :effects))
                                    (list binding)
                                    (lambda (predicate)
                                      (knowledge-base-schema base predicate :workspace workspace)))))))))
