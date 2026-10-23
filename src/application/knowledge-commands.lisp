(in-package #:autolith)

;;;; -- Knowledge Commands --

;;; Facts appear inside (memories), and (memory "ID") explains a fact as it
;;; explains a memory: (why "ID") is the same command. (what-changed) reports
;;; what changed since the person last asked, and (rules), (admit-rule), and
;;; (retire-rule) manage the rules that derive facts.

(defparameter *knowledge-command-fact-limit* 20
  "The most facts (memories) lists before asking for a query.")

(defparameter *knowledge-command-explanation-depth* 3
  "How many levels of premises (memory \"ID\") expands below a fact.")


;;; Shared presentation

(-> application-knowledge--view (application) knowledge-view)
(defun application-knowledge--view (application)
  "Return APPLICATION's knowledge evaluated from its workspace now."
  (knowledge-view-current (application-configuration application)))

(-> application-knowledge--call (knowledge-node) string)
(defun application-knowledge--call (node)
  "Return the Lisp call that explains NODE."
  (format nil "(memory ~S)" (application-memory--short (knowledge-node-identifier node))))

(-> application-knowledge--text (knowledge-node) string)
(defun application-knowledge--text (node)
  "Return NODE's sentence as one safe terminal line."
  (sanitize-text (knowledge-node-text node) :single-line-p t))

(-> application-knowledge--listed-p (knowledge-node) boolean)
(defun application-knowledge--listed-p (node)
  "Return true when NODE belongs in a listing: it holds, or it stopped holding."
  (and (member (knowledge-node-applicability node)
               '(:current :stale :expired :unverified :pending))
       t))

(-> application-knowledge--rank (knowledge-view knowledge-node) integer)
(defun application-knowledge--rank (view node)
  "Order conflicted facts first, then facts that stopped holding, then current ones."
  (cond
    ((eq (knowledge-node-status view node) ':conflicted) 0)
    ((not (knowledge-node-supported-p node)) 1)
    (t 2)))

(-> application-knowledge--provenance (knowledge-view knowledge-node) (option string))
(defun application-knowledge--provenance (view node)
  "Return where NODE comes from: its deriving rule, or its sources' short labels."
  (let ((justification (first (knowledge-node-justifications node))))
    (if justification
        (format nil "derived by ~A v~D" (getf justification :rule)
                (getf justification :version))
        (let ((labels (remove-duplicates
                       (loop for support in (knowledge-node-supports node)
                             for evidence = (and (getf support :evidence)
                                                 (knowledge-base-evidence-record
                                                  (knowledge-view-base view)
                                                  (getf support :evidence)))
                             collect (if evidence
                                         (let ((source (getf evidence :source)))
                                           (ecase (first source)
                                             (:file (getf (rest source) :path))
                                             (:url (or (ignore-errors
                                                        (quri:uri-host
                                                         (quri:uri (getf (rest source) :url))))
                                                       "web"))
                                             (:user "you said")
                                             ((:tool :memory :mission :skill)
                                              (string-downcase (symbol-name (first source))))))
                                         (string-downcase (symbol-name (getf support :basis)))))
                       :test #'string= :from-end t)))
          (and labels (sanitize-text (format nil "~{~A~^, ~}" labels) :single-line-p t))))))

(-> application-knowledge--reason (knowledge-view knowledge-node) (option string))
(defun application-knowledge--reason (view node)
  "Return why NODE stopped holding, or NIL while it holds."
  (unless (knowledge-node-supported-p node)
    (let ((causes (loop for support in (knowledge-node-supports node)
                        unless (eq (getf support :state) ':current)
                          collect (let ((explained (knowledge--explain-support view support)))
                                    (format nil "~A ~A" (getf explained :text)
                                            (knowledge-applicability-text
                                             (getf support :state) (getf support :reason)))))))
      (sanitize-text (format nil "~{~A~^; ~}" causes) :single-line-p t))))

(-> application-knowledge--fact-row (knowledge-view knowledge-node) list)
(defun application-knowledge--fact-row (view node)
  "Return one compact listing row for fact NODE."
  (let ((status (knowledge-node-status view node))
        (applicability (knowledge-node-applicability node)))
    (append
     (list
      (cond
        ((eq status ':conflicted) (terminal-span ':failure "  ! "))
        ((eq applicability ':current) (terminal-span ':dim "  · "))
        ((eq applicability ':unverified) (terminal-span ':notice "  ? "))
        (t (terminal-span ':failure "  ! ")))
      (terminal-span ':strong (format nil "~A~%" (application-knowledge--text node))))
     (when (eq status ':conflicted)
       (list (terminal-span
              ':failure
              (format nil "    conflicts with ~{\"~A\"~^, ~}~%"
                      (mapcar (lambda (identifier)
                                (let ((other (knowledge-view-node view identifier)))
                                  (if other (application-knowledge--text other) identifier)))
                              (knowledge-node-conflicts node))))))
     (unless (eq applicability ':current)
       (list (terminal-span (if (eq applicability ':unverified) ':notice ':failure)
                            (format nil "    ~(~A~): ~A~%"
                                    applicability (application-knowledge--reason view node)))))
     (list (terminal-span ':dim
                          (format nil "    ~A~@[ · ~A~]~%"
                                  (application-knowledge--call node)
                                  (application-knowledge--provenance view node)))))))

(-> application-knowledge-facts-section
    (knowledge-view &key (:query (option string)) (:memories-p boolean))
    list)
(defun application-knowledge-facts-section (view &key query memories-p)
  "Return the facts part of (memories), or NIL when no fact is listed.

MEMORIES-P separates it from a memory listing above it."
  (let* ((terms (and (non-empty-string-p query) (memory--search-terms query)))
         (nodes (stable-sort
                 (sort (loop for node being the hash-values of (knowledge-view-nodes view)
                             when (and (application-knowledge--listed-p node)
                                       (or (null terms)
                                           (= (knowledge-node-relevance node terms)
                                              (length terms))))
                               collect node)
                       #'string< :key #'knowledge-node-text)
                 #'< :key (lambda (node) (application-knowledge--rank view node))))
         (shown (subseq nodes 0 (min (length nodes) *knowledge-command-fact-limit*))))
    (when nodes
      (append
       (list (terminal-span ':brand (format nil "~:[~;~%~]facts" memories-p))
             (terminal-span
              ':dim
              (format nil "  ~D~[~:; · ~:*~D conflicted~]~[~:; · ~:*~D no longer holding~]~%"
                      (length nodes)
                      (count ':conflicted nodes
                             :key (lambda (node) (knowledge-node-status view node)))
                      (count-if-not #'knowledge-node-supported-p nodes))))
       (loop for node in shown
             append (application-knowledge--fact-row view node))
       (when (> (length nodes) (length shown))
         (list (terminal-span
                ':hint
                (format nil "  ~D more; (memories \"QUERY\") narrows the list~%"
                        (- (length nodes) (length shown))))))))))


;;; Explaining one fact

(-> application-knowledge--support-row (knowledge-view list integer) list)
(defun application-knowledge--support-row (view support indent)
  "Return the evidence line for one explained SUPPORT at INDENT."
  (let* ((evidence (and (getf support :evidence)
                        (knowledge-base-evidence-record (knowledge-view-base view)
                                                        (getf support :evidence))))
         (checked-p (and evidence (eq (first (getf evidence :source)) :file)))
         (state (getf support :state)))
    (list
     (cond
       ((not (eq state ':current))
        (terminal-span ':failure (format nil "~vT✗ " indent)))
       (checked-p
        (terminal-span ':success (format nil "~vT✓ " indent)))
       (t
        (terminal-span ':dim (format nil "~vT· " indent))))
     (terminal-span ':plain
                    (sanitize-text (format nil "~(~A~): ~A" (getf support :basis)
                                           (getf support :text))
                                   :single-line-p t))
     (terminal-span ':failure
                    (format nil "~:[ ~A~;~*~]~%"
                            (eq state ':current)
                            (knowledge-applicability-text state (getf support :reason)))))))

(-> application-knowledge--tree-rows (knowledge-view list &key (:indent integer) (:depth integer))
    list)
(defun application-knowledge--tree-rows (view tree &key (indent 4) (depth 1))
  "Return the lines explaining one premise TREE and, within the depth limit, its own support."
  (let* ((identifier (getf tree :claim))
         (node (knowledge-view-node view identifier))
         (supported-p (and node (knowledge-node-supported-p node))))
    (append
     (list
      (if supported-p
          (terminal-span ':success (format nil "~vT✓ " indent))
          (terminal-span ':failure (format nil "~vT✗ " indent)))
      (terminal-span ':plain (format nil "~A " (if node
                                                   (application-knowledge--text node)
                                                   identifier)))
      (terminal-span ':dim (format nil "~A~%"
                                   (if node (application-knowledge--call node) ""))))
     (when (and node (< depth *knowledge-command-explanation-depth*) (not (getf tree :more)))
       (application-knowledge--why-rows view tree :indent (+ indent 2) :depth (1+ depth))))))

(-> application-knowledge--why-rows (knowledge-view list &key (:indent integer) (:depth integer))
    list)
(defun application-knowledge--why-rows (view tree &key (indent 4) (depth 1))
  "Return TREE's supports and derivations as evidence lines."
  (append
   (loop for support in (getf tree :supports)
         append (application-knowledge--support-row view support indent))
   (loop for justification in (getf tree :justifications)
         append (cons (terminal-span
                       ':dim
                       (format nil "~vTderived by ~A v~D~@[ with ~{~A~^, ~}~] from~%"
                               indent (getf justification :rule) (getf justification :version)
                               (mapcar (lambda (binding)
                                         (format nil "~A = ~A" (first binding)
                                                 (knowledge-term-display (rest binding))))
                                       (getf justification :bindings))))
                      (loop for premise in (getf justification :premises)
                            append (application-knowledge--tree-rows
                                    view premise :indent (+ indent 2) :depth depth))))))

(-> application-knowledge-fact-entry (application knowledge-view knowledge-node) list)
(defun application-knowledge-fact-entry (application view node)
  "Return one fact with why it holds, what it conflicts with, and memories relying on it."
  (let* ((status (knowledge-node-status view node))
         (applicability (knowledge-node-applicability node))
         (tree (first (knowledge-explain view (knowledge-node-identifier node))))
         (dependents (remove-if-not
                      (lambda (memory)
                        (find (knowledge-node-identifier node) (memory-premises memory)
                              :key (lambda (premise) (getf premise :id)) :test #'string=))
                      (memory-list (application-configuration application)))))
    (append
     (list
      (terminal-span ':brand "fact  ")
      (terminal-span (case status
                       (:supported ':success)
                       (:conflicted ':failure)
                       (otherwise (if (eq applicability ':unverified) ':notice ':failure)))
                     (format nil "~(~A~)~:[ · ~(~A~)~;~*~]~%"
                             status (eq applicability ':current) applicability))
      (terminal-span ':dim
                     (format nil "  id ~A~@[ · bases ~{~{~(~A~)~^+~}~^ or ~}~]~2%"
                             (knowledge-node-identifier node) (knowledge-node-bases node)))
      (terminal-span ':strong (format nil "  ~A~2%" (application-knowledge--text node)))
      (terminal-span ':dim (format nil "  why~%")))
     (or (application-knowledge--why-rows view tree)
         (list (terminal-span ':hint (format nil "    nothing supports it now~%"))))
     (when (knowledge-node-conflicts node)
       (cons (terminal-span ':dim (format nil "  conflicts with~%"))
             (loop for identifier in (knowledge-node-conflicts node)
                   for other = (knowledge-view-node view identifier)
                   when other
                     append (list (terminal-span ':plain
                                                 (format nil "    \"~A\" "
                                                         (application-knowledge--text other)))
                                  (terminal-span ':dim
                                                 (format nil "~A~%"
                                                         (application-knowledge--call other)))))))
     (when dependents
       (cons (terminal-span ':dim (format nil "  relied on by~%"))
             (loop for memory in dependents
                   append (application-memory--reference memory)))))))

(-> application-knowledge-resolve (knowledge-view string) (values (option knowledge-node) keyword list))
(defun application-knowledge-resolve (view identifier)
  "Resolve an exact claim identifier or unique prefix among VIEW's listed facts.

The second value is :FOUND, :MISSING, or :AMBIGUOUS; the third lists the
matches of an ambiguous prefix."
  (let* ((text (string-downcase (string-trim '(#\Space #\Tab) identifier)))
         (exact (and (plusp (length text)) (knowledge-view-node view text)))
         (matches (and (plusp (length text))
                       (loop for node being the hash-values of (knowledge-view-nodes view)
                             when (and (application-knowledge--listed-p node)
                                       (uiop:string-prefix-p text
                                                             (knowledge-node-identifier node)))
                               collect node))))
    (cond
      (exact (values exact ':found nil))
      ((null matches) (values nil ':missing nil))
      ((null (rest matches)) (values (first matches) ':found nil))
      (t (values nil ':ambiguous matches)))))


;;; (what-changed)

(-> application-knowledge-changes-entry (application) list)
(defun application-knowledge-changes-entry (application)
  "Return what changed in this workspace's facts since the person last asked."
  (let ((view (application-knowledge--view application)))
    (multiple-value-bind (changes previous first-p)
        (knowledge-viewer-changes (application-configuration application) "user" view)
      (cond
        (first-p
         (list (terminal-span
                ':hint
                (format nil "~D fact~:P hold now. (what-changed) will report changes from here on."
                        (length (knowledge-view-snapshot view))))))
        ((null changes)
         (list (terminal-span ':hint
                              (format nil "Nothing changed since ~A."
                                      (memory--timestamp-string previous)))))
        (t
         (append
          (list (terminal-span ':brand "what changed")
                (terminal-span ':dim (format nil "  since ~A~%"
                                             (memory--timestamp-string previous))))
          (loop for change in changes
                append
                (ecase (getf change :kind)
                  (:changed
                   (list (terminal-span ':notice "  ~ ")
                         (terminal-span ':strong (format nil "~A~%"
                                                         (sanitize-text (getf change :to)
                                                                        :single-line-p t)))
                         (terminal-span ':dim (format nil "    was ~A~%"
                                                      (sanitize-text (getf change :from)
                                                                     :single-line-p t)))))
                  (:new
                   (list (terminal-span ':success "  + ")
                         (terminal-span ':plain (format nil "~A~%"
                                                        (sanitize-text (getf change :text)
                                                                       :single-line-p t)))))
                  (:withdrawn
                   (list (terminal-span ':failure "  - ")
                         (terminal-span ':plain (format nil "~A~%"
                                                        (sanitize-text (getf change :text)
                                                                       :single-line-p t)))))
                  (:conflict
                   (list (terminal-span ':failure "  ! ")
                         (terminal-span ':plain (format nil "~A now conflicts~%"
                                                        (sanitize-text (getf change :text)
                                                                       :single-line-p t))))))
                append (when (getf change :causes)
                         (list (terminal-span
                                ':dim
                                (format nil "    because ~A~%"
                                        (sanitize-text (format nil "~{~A~^; ~}"
                                                               (getf change :causes))
                                                       :single-line-p t))))))))))))


;;; (rules)

(-> application-knowledge-rules-entry (application) list)
(defun application-knowledge-rules-entry (application)
  "Return the rules visible in this workspace and whether each is active."
  (let* ((configuration (application-configuration application))
         (entries (sort (knowledge-base-rule-entries (knowledge-base-read configuration)
                                                     :workspace (knowledge--workspace
                                                                 configuration))
                        #'string< :key (lambda (entry) (getf entry :id)))))
    (if (null entries)
        (list (terminal-span ':hint "No rules are defined. Autolith proposes rules when a derivation recurs."))
        (append
         (list (terminal-span ':brand "rules")
               (terminal-span ':dim (format nil "  ~D~%" (length entries))))
         (loop for entry in entries
               for latest = (first (last (getf entry :versions)))
               for active = (getf entry :active)
               append
               (list (if (eql active (getf latest :version))
                         (terminal-span ':success "  ✓ ")
                         (terminal-span ':notice "  ? "))
                     (terminal-span ':strong (format nil "~A v~D" (getf entry :id)
                                                     (getf latest :version)))
                     (terminal-span ':dim
                                    (format nil " ~(~A~)~:[~; · active v~:*~D~]~:[ · awaiting (admit-rule ~S)~;~*~]~%"
                                            (getf entry :scope) active
                                            (eql active (getf latest :version))
                                            (getf entry :id)))
                     (terminal-span ':plain
                                    (format nil "    ~A~%"
                                            (sanitize-text
                                             (let ((command (memory-knowledge--admission-command
                                                             (getf entry :id) (getf latest :version)
                                                             (getf latest :rule))))
                                               (subseq command (+ 2 (search ": " command))))
                                             :single-line-p t)))))))))

(-> application-knowledge-rule-transition-entry (application string keyword) (or string list))
(defun application-knowledge-rule-transition-entry (application identifier kind)
  "Admit or retire rule IDENTIFIER, as KIND says, and confirm what changed."
  (let* ((configuration (application-configuration application))
         (entry (knowledge-base-rule (knowledge-base-read configuration) identifier
                                     :workspace (knowledge--workspace configuration))))
    (if (null entry)
        (format nil "No rule ~S exists. Run (rules) to list them." identifier)
        (handler-case
            (let ((version (if (eq kind ':admit)
                               (knowledge-admit-rule configuration identifier
                                                     :scope (getf entry :scope) :by "user")
                               (knowledge-retire-rule configuration identifier
                                                      :scope (getf entry :scope)))))
              (list (terminal-span ':success (if (eq kind ':admit) "admitted  " "retired  "))
                    (terminal-span ':strong (format nil "~A v~D~%" identifier version))))
          (knowledge-error (condition)
            (autolith-error-message condition))))))


;;;; -- Command Definitions --

(define-application-command application--builtin-what-changed-command
    (:name "/what-changed"
     :argument nil
     :description "show how facts changed since you last asked"
     :tip "reports facts that changed, appeared, or stopped holding, with the cause."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-knowledge-changes-entry application))
  ':continue)

(define-application-command application--builtin-rules-command
    (:name "/rules"
     :argument nil
     :description "list the rules that derive facts"
     :tip "shows each rule's latest version and whether it is admitted."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-knowledge-rules-entry application))
  ':continue)

(define-application-command application--builtin-admit-rule-command
    (:name "/admit-rule"
     :argument "[ID]"
     :description "admit a proposed rule"
     :tip "lets a proposed rule derive facts; its latest version replaces any earlier one."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t)
    (application &optional identifier)
  (application-present
   application
   (if identifier
       (application-knowledge-rule-transition-entry application identifier ':admit)
       "Usage: (admit-rule \"ID\"). Run (rules) to list rules."))
  ':continue)

(define-application-command application--builtin-retire-rule-command
    (:name "/retire-rule"
     :argument "[ID]"
     :description "stop a rule deriving facts"
     :tip "retires a rule; facts it derived stop holding unless something else supports them."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t)
    (application &optional identifier)
  (application-present
   application
   (if identifier
       (application-knowledge-rule-transition-entry application identifier ':retire)
       "Usage: (retire-rule \"ID\"). Run (rules) to list rules."))
  ':continue)
