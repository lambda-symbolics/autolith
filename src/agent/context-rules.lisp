(in-package #:autolith)

;;;; -- Declarative Context Rules --

(defparameter *context-rule-conversation-limit* 32
  "Maximum conversations retaining request-local trigger metadata.")

(defparameter *context-rule-value-limit* 64
  "Maximum distinct metadata values retained per trigger field.")

(defvar *context-rule-facts* (make-hash-table :test #'equal)
  "Conversation identities mapped to bounded current-turn trigger metadata.")

(defvar *context-rule-order* nil
  "Conversations with trigger metadata, newest activity first.")

(defparameter *context-rule-language-extensions*
  '(("lisp" . "common-lisp") ("cl" . "common-lisp") ("asd" . "common-lisp")
    ("scm" . "scheme") ("ss" . "scheme") ("clj" . "clojure")
    ("c" . "c") ("h" . "c") ("cc" . "c++") ("cpp" . "c++")
    ("rs" . "rust") ("go" . "go") ("py" . "python")
    ("js" . "javascript") ("ts" . "typescript") ("sh" . "shell"))
  "Source extensions whose language identity is unambiguous enough for advice.")

(define-condition context-rule-error (configuration-error)
  ((field :initarg :field :reader context-rule-error-field
          :documentation "The invalid declarative rule field."))
  (:documentation "A malformed declarative context rule or trigger value."))

(-> context-rule--reject (keyword string) nil)
(defun context-rule--reject (field message)
  "Reject invalid FIELD with a bounded configuration diagnostic."
  (error 'context-rule-error :field field :message message))

(-> context-rule--plist-p (t) boolean)
(defun context-rule--plist-p (value)
  "Return true for a finite even-length plist with unique keyword indicators."
  (handler-case
      (and (listp value)
           (let ((length (list-length value)))
             (and length (evenp length)
                  (loop for tail on value by #'cddr
                        always (keywordp (first tail)))
                  (= (/ length 2)
                     (length (remove-duplicates
                              (loop for tail on value by #'cddr
                                    collect (first tail)))))))
           t)
    (type-error ()
      nil)))

(-> context-rule--values (keyword t) list)
(defun context-rule--values (field values)
  "Validate and detach bounded string metadata VALUES for FIELD."
  (unless (handler-case
              (and (listp values) (list-length values)
                   (<= (length values) *context-rule-value-limit*)
                   (every (lambda (value)
                            (and (non-empty-string-p value)
                                 (<= (length value) 1024)))
                          values))
            (type-error ()
              nil))
    (context-rule--reject field "Rule trigger values must be a bounded proper list of nonempty strings."))
  (mapcar #'copy-seq values))

(-> context-rule--validate (t) list)
(defun context-rule--validate (rule)
  "Return a detached validated rule without executing its declarative predicates."
  (unless (and (context-rule--plist-p rule)
               (loop for tail on rule by #'cddr
                     always (member (first tail)
                                    '(:id :when :instruction :priority :lifetime :workspace))))
    (context-rule--reject ':rule "A rule accepts only id, when, instruction, priority, lifetime and workspace."))
  (let ((predicates (getf rule :when))
        (workspace (getf rule :workspace)))
    (context--validate-identifier (getf rule :id) "Context rule ID")
    (unless (and predicates (context-rule--plist-p predicates)
                 (loop for tail on predicates by #'cddr
                       always (member (first tail)
                                      '(:paths :languages :tools :namespaces :roles :diagnostics :events))))
      (context-rule--reject ':when "A rule requires at least one recognized metadata predicate."))
    (loop for (field values) on predicates by #'cddr
          do (unless (context-rule--values field values)
               (context-rule--reject field "A rule predicate requires at least one value.")))
    (when (and workspace
               (not (and (non-empty-string-p workspace)
                         (uiop:absolute-pathname-p (pathname workspace)))))
      (context-rule--reject ':workspace "Rule workspace scope must be an absolute directory namestring."))
    ;; Use the ordinary contribution constructor for all delivery policy validation.
    (make-context-contribution :identifier (getf rule :id)
                               :instruction (getf rule :instruction)
                               :priority (getf rule :priority 0)
                               :lifetime (getf rule :lifetime ':while-relevant))
    (labels ((detach (value)
               (typecase value
                 (string
                  (copy-seq value))
                 (cons
                  (cons (detach (first value)) (detach (rest value))))
                 (t
                  value))))
      (detach rule))))

(-> context-rule--remember (string list) null)
(defun context-rule--remember (identifier facts)
  "Install FACTS and enforce bounded conversation retention under the context lock."
  (setf (gethash identifier *context-rule-facts*) facts
        *context-rule-order*
        (cons identifier (remove identifier *context-rule-order* :test #'equal)))
  (dolist (evicted (nthcdr *context-rule-conversation-limit* *context-rule-order*))
    (remhash evicted *context-rule-facts*))
  (setf *context-rule-order*
        (subseq *context-rule-order* 0
                (min *context-rule-conversation-limit* (length *context-rule-order*))))
  nil)

(-> context-rule-reset () null)
(defun context-rule-reset ()
  "Discard ephemeral trigger metadata without changing rule registrations."
  (with-lock-held (*context-lock*)
    (clrhash *context-rule-facts*)
    (setf *context-rule-order* nil))
  nil)

(-> context-rule--generation (conversation) string)
(defun context-rule--generation (conversation)
  "Capture the current metadata generation, creating an identity when needed."
  (with-lock-held (*context-lock*)
    (let* ((identifier (conversation-identifier conversation))
           (facts (copy-tree (gethash identifier *context-rule-facts*))))
      (unless (getf facts :generation)
        (setf (getf facts :generation) (make-identifier))
        (context-rule--remember identifier facts))
      (getf facts :generation))))

(defmethod initialize-instance :after
    ((context tool-context) &key (context-rule-generation nil generation-p) &allow-other-keys)
  "Capture the originating turn once; explicit cloned generations are preserved."
  (declare (ignore context-rule-generation))
  (when (and (not generation-p)
             (slot-boundp context 'conversation)
             (typep (tool-context-conversation context) 'conversation))
    (setf (slot-value context 'context-rule-generation)
          (context-rule--generation (tool-context-conversation context)))))

(-> context-rule-note
    (conversation &key (:paths list) (:languages list) (:tools list)
                  (:namespaces list) (:roles list) (:diagnostics list) (:events list)
                  (:generation (option string))) null)
(defun context-rule-note (conversation &key paths languages tools namespaces roles diagnostics events
                                           (generation nil generation-p))
  "Add bounded structured trigger metadata for CONVERSATION's current turn.

Values are exact strings, except :paths predicates match workspace-relative
prefixes. Supply a captured GENERATION for asynchronous work; stale facts are
ignored. Metadata never grants tool authority."
  (let ((additions
          (list :paths (context-rule--values ':paths paths)
                :languages (context-rule--values ':languages languages)
                :tools (context-rule--values ':tools tools)
                :namespaces (context-rule--values ':namespaces namespaces)
                :roles (context-rule--values ':roles roles)
                :diagnostics (context-rule--values ':diagnostics diagnostics)
                :events (context-rule--values ':events events)))
        (identifier (conversation-identifier conversation)))
    (with-lock-held (*context-lock*)
      (let ((facts (copy-tree (gethash identifier *context-rule-facts*))))
        (unless (and generation-p (not (equal generation (getf facts :generation))))
          (unless (getf facts :generation)
            (setf (getf facts :generation) (make-identifier)))
          (loop for (field values) on additions by #'cddr
                for merged = (remove-duplicates (append values (getf facts field))
                                                :test #'equal :from-end t)
                do (setf (getf facts field)
                         (subseq merged 0 (min *context-rule-value-limit* (length merged)))))
          (context-rule--remember identifier facts)))))
  nil)

(-> context-rule--matches-p (list request-context) boolean)
(defun context-rule--matches-p (rule request)
  "Match conjunctive fields and disjunctive values against one detached snapshot."
  (and (not (request-context-compaction-p request))
       (or (null (getf rule :workspace))
           (equal (namestring (uiop:ensure-directory-pathname (getf rule :workspace)))
                  (namestring (uiop:ensure-directory-pathname
                               (config :working-directory (request-context-configuration request))))))
       (let ((facts
               (with-lock-held (*context-lock*)
                 (copy-tree (gethash (conversation-identifier (request-context-conversation request))
                                     *context-rule-facts*)))))
         (loop for (field values) on (getf rule :when) by #'cddr
               always (some (lambda (value)
                              (some (lambda (fact)
                                      (if (eq field ':paths)
                                          (uiop:string-prefix-p value fact)
                                          (equal value fact)))
                                    (getf facts field)))
                            values)))
       t))

(-> register-context-rule (list &key (:source keyword)) string)
(defun register-context-rule (rule &key (source *extension-registration-source*))
  "Register one validated project/user RULE as an ordinary bounded contributor.

Fields within :when are conjunctive; values within a field are alternatives.
Registration replaces the same rule ID using existing context registry semantics."
  (let* ((validated (context-rule--validate rule))
         (identifier (concatenate 'string "rule/" (getf validated :id))))
    (register-context-contributor
     identifier
     (lambda (request)
       (when (context-rule--matches-p validated request)
         (make-context-contribution
          :identifier identifier :instruction (getf validated :instruction)
          :priority (getf validated :priority 0)
          :lifetime (getf validated :lifetime ':while-relevant))))
     :source source)))

(-> unregister-context-rule (string) boolean)
(defun unregister-context-rule (identifier)
  "Remove the rule with the original unprefixed IDENTIFIER."
  (unregister-context-contributor (concatenate 'string "rule/" identifier)))

(-> context-rule--agent-role (t) (option string))
(defun context-rule--agent-role (agent)
  "Return the primary or known child role, or NIL before child initialization."
  (typecase agent
    (task-child-agent
     (when (and (slot-boundp agent 'definition)
                (typep (task-child-agent-definition agent) 'task-agent-definition))
       (task-agent-definition-name (task-child-agent-definition agent))))
    (t
     "primary")))

(-> context-rule-start-turn (agent) null)
(defun context-rule-start-turn (agent)
  "Reset trigger metadata inside AGENT's serialized logical turn."
  (with-lock-held (*context-lock*)
    (context-rule--remember (conversation-identifier (agent-conversation agent))
                            (list :generation (make-identifier)
                                  :roles (remove nil (list (context-rule--agent-role agent))))))
  nil)

(-> context-rule--note-tool (tool tool-context json-object) null)
(defun context-rule--note-tool (tool context arguments)
  "Record an authorized operation and bounded source metadata without copying its payload."
  (let* ((paths
           (loop for key in '("path" "uri")
                 for value = (json-get arguments key)
                 when (and (non-empty-string-p value) (<= (length value) 1024))
                   collect (if (uiop:string-prefix-p "workspace:" value)
                               (subseq value 10) value)))
         (languages
           (remove nil
                   (mapcar (lambda (path)
                             (handler-case
                                 (rest (assoc (string-downcase (or (pathname-type (pathname path)) ""))
                                              *context-rule-language-extensions* :test #'equal))
                               (error ()
                                 nil)))
                           paths))))
    (context-rule-note (tool-context-conversation context)
                       :generation (tool-context-rule-generation context)
                       :paths paths :languages languages
                       :tools (list (tool-canonical-name tool))
                       :namespaces (list (tool-namespace tool))
                       :roles (remove nil (list (context-rule--agent-role (tool-context-agent context))))
                       :events '("tool-start")))
  nil)

(defmethod tool-execute :around ((tool tool) (context tool-context) (arguments hash-table))
  "Publish operation metadata only after ordinary registry authorization."
  (when (and (slot-boundp context 'conversation)
             (typep (tool-context-conversation context) 'conversation))
    (context-rule--note-tool tool context arguments))
  (let ((result (call-next-method)))
    (when (and (slot-boundp context 'conversation)
               (typep (tool-context-conversation context) 'conversation))
      (context-rule-note (tool-context-conversation context)
                         :generation (tool-context-rule-generation context)
                         :events (list (if (tool-result-success-p result) "tool-success" "tool-failure"))))
    result))

(-> context-rule-note-diagnostics (conversation t &key (:generation (option string))) null)
(defun context-rule-note-diagnostics (conversation reports &key (generation nil generation-p))
  "Extract bounded LSP severity metadata from structured REPORTS, never diagnostic prose."
  (let ((remaining 256)
        (severities nil))
    (labels ((visit (value depth)
               (when (and (plusp remaining) (< depth 8))
                 (decf remaining)
                 (cond
                   ((json-object-p value)
                    (let ((severity (json-get value "severity")))
                      (when (typep severity '(integer 1 4))
                        (pushnew (nth (1- severity) '("error" "warning" "information" "hint"))
                                 severities :test #'equal)))
                    (maphash (lambda (key child)
                               (declare (ignore key))
                               (visit child (1+ depth)))
                             value))
                   ((and (vectorp value) (not (stringp value)))
                    (loop for child across value while (plusp remaining)
                          do (visit child (1+ depth))))))))
      (visit reports 0))
    (when severities
      (apply #'context-rule-note conversation :diagnostics severities :events '("diagnostics")
             (when generation-p (list :generation generation)))))
  nil)
