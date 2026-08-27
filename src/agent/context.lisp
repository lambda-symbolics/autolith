(in-package #:autolith)

;;;; -- Request-Local Context --

(defparameter *context-contribution-identifier-limit* 128
  "The maximum characters in a context contribution identifier.")

(defparameter *context-contribution-instruction-limit* 4000
  "The maximum characters in one advisory request-local instruction.")

(defparameter *context-mandatory-instruction-limit* (* 128 1024)
  "The maximum characters in one mandatory request-local instruction.")

(defparameter *context-contribution-evidence-limit* 2000
  "The maximum characters in one untrusted evidence value.")

(defparameter *context-contribution-reference-limit* 32
  "The maximum supersession references on one contribution.")

(defparameter *context-delivery-diagnostic-limit* 32
  "The maximum number of conversations retaining last-delivery diagnostics.")

(defparameter *context-advice-token-budget* 1500
  "The approximate request-token budget shared by advisory contributions.")

(defvar *user-init-loading-p* nil
  "True only while Autolith loads executable filesystem configuration.")

(defvar *extension-registration-source* ':runtime
  "The source attributed to extension registrations in the current load context.")

(defvar *context-contributors* nil
  "Portable contributor registrations in deterministic registration order.")

(defvar *context-next-request-delivered* (make-hash-table :test #'equal)
  "Contribution keys delivered while their next-request trigger remains active.")

(defvar *context-last-deliveries* (make-hash-table :test #'equal)
  "Conversation identifiers mapped to their newest payload-free context summary.")

(defvar *context-last-delivery-order* nil
  "Conversation identifiers with diagnostics, newest delivery first.")

(defvar *extension-registry-transaction-lock*
  (make-recursive-lock "Autolith extension registry transaction")
  "The lock publishing MCP, context, and command registry generations.")

(defmacro with-extension-registry-transaction (&body body)
  "Evaluate BODY against one isolated extension registry generation."
  `(with-recursive-lock-held (*extension-registry-transaction-lock*)
     ,@body))

(defvar *context-lock* (make-lock "Autolith request-local context")
  "The lock protecting registrations, delivery state, and diagnostics.")

(defvar *context-contributor-invocation-lock*
  (make-lock "Autolith context contributor invocation")
  "The lock serializing user-extensible contributor function calls.")

(defvar *context-request-contributions* nil
  "Dynamically supplied contributions for exactly one provider request.")


;;;; -- Request and Contribution Values --

(defclass request-context ()
  ((configuration
    :initarg :configuration
    :reader request-context-configuration
    :type configuration
    :documentation "The immutable configuration for this request.")
   (conversation
    :initarg :conversation
    :reader request-context-conversation
    :type conversation
    :documentation "The durable conversation projected by this request.")
   (tool-namespaces
    :initarg :tool-namespaces
    :reader request-context-tool-namespaces
    :type vector
    :documentation "The provider-visible local tool namespaces.")
   (goal-context
    :initarg :goal-context
    :initform nil
    :reader request-context-goal-context
    :type (option string)
    :documentation "The active request-local goal context, when present.")
   (compaction-p
    :initarg :compaction-p
    :initform nil
    :reader request-context-compaction-p
    :type boolean
    :documentation "Whether the request is a side-channel compaction."))
  (:documentation "A read-only snapshot supplied to context contributors."))

(defclass context-contribution ()
  ((identifier
    :initarg :identifier
    :reader context-contribution-identifier
    :type non-empty-string
    :documentation "The stable identity used for inspection and supersession.")
   (instruction
    :initarg :instruction
    :reader context-contribution-instruction
    :type non-empty-string
    :documentation "Trusted advice rendered only into the current request.")
   (evidence
    :initarg :evidence
    :initform nil
    :reader context-contribution-evidence
    :type (option string)
    :documentation "Optional untrusted data supporting the instruction.")
   (priority
    :initarg :priority
    :initform 0
    :reader context-contribution-priority
    :type integer
    :documentation "The budget and rendering priority; larger values matter more.")
   (lifetime
    :initarg :lifetime
    :initform ':while-relevant
    :reader context-contribution-lifetime
    :type context-contribution-lifetime
    :documentation "The declared period for which the advice remains relevant.")
   (class
    :initarg :class
    :initform ':advice
    :reader context-contribution-class
    :type context-contribution-class
    :documentation "Whether the contribution competes for the advice budget.")
   (deduplication-key
    :initarg :deduplication-key
    :initform nil
    :reader context-contribution-deduplication-key
    :type (option string)
    :documentation "The optional semantic identity shared by equivalent advice.")
   (supersedes
    :initarg :supersedes
    :initform nil
    :reader context-contribution-supersedes
    :type list
    :documentation "Contribution identifiers or deduplication keys replaced by this one.")
   (conflict-group
    :initarg :conflict-group
    :initform nil
    :reader context-contribution-conflict-group
    :type (option string)
    :documentation "The optional group in which only the strongest advice applies.")
   (contributor
    :initarg :contributor
    :initform "unknown"
    :reader context-contribution-contributor
    :type non-empty-string
    :documentation "The registration that produced this contribution.")
   (source
    :initarg :source
    :initform ':runtime
    :reader context-contribution-source
    :type keyword
    :documentation "The built-in, user, or runtime origin of the contributor."))
  (:documentation "One structured instruction that never enters conversation history."))

(defclass context-delivery ()
  ((conversation-identifier
    :initarg :conversation-identifier
    :reader context-delivery-conversation-identifier
    :type non-empty-string
    :documentation "The conversation for which this delivery was assembled.")
   (created-at
    :initarg :created-at
    :reader context-delivery-created-at
    :type timestamp
    :documentation "The assembly time as Common Lisp universal time.")
   (contributions
    :initarg :contributions
    :reader context-delivery-contributions
    :type list
    :documentation "The contributions selected for the provider request.")
   (omitted
    :initarg :omitted
    :reader context-delivery-omitted
    :type list
    :documentation "Advisory contributions omitted by the token budget.")
   (failures
    :initarg :failures
    :reader context-delivery-failures
    :type list
    :documentation "Contributor identifiers paired with bounded failure reports.")
   (rendered
    :initarg :rendered
    :reader context-delivery-rendered
    :type (option string)
    :documentation "The complete request-local developer message, when nonempty."))
  (:documentation "Non-conversation diagnostics for one ephemeral context assembly."))

(defclass context-contribution-diagnostic ()
  ((identifier
    :initarg :identifier
    :reader context-contribution-diagnostic-identifier
    :type non-empty-string
    :documentation "The stable identity of the selected or omitted contribution.")
   (contributor
    :initarg :contributor
    :reader context-contribution-diagnostic-contributor
    :type non-empty-string
    :documentation "The registration that produced the contribution.")
   (source
    :initarg :source
    :reader context-contribution-diagnostic-source
    :type keyword
    :documentation "The built-in, user, or runtime origin of the contributor.")
   (priority
    :initarg :priority
    :reader context-contribution-diagnostic-priority
    :type integer
    :documentation "The priority used while resolving the contribution.")
   (lifetime
    :initarg :lifetime
    :reader context-contribution-diagnostic-lifetime
    :type context-contribution-lifetime
    :documentation "The contribution's request-local lifetime.")
   (class
    :initarg :class
    :reader context-contribution-diagnostic-class
    :type context-contribution-class
    :documentation "Whether the contribution was mandatory or advisory.")
   (instruction-character-count
    :initarg :instruction-character-count
    :reader context-contribution-diagnostic-instruction-character-count
    :type (integer 1)
    :documentation "The instruction payload size without retaining its contents.")
   (evidence-character-count
    :initarg :evidence-character-count
    :reader context-contribution-diagnostic-evidence-character-count
    :type (integer 0)
    :documentation "The evidence payload size without retaining its contents.")
   (token-estimate
    :initarg :token-estimate
    :reader context-contribution-diagnostic-token-estimate
    :type (integer 0)
    :documentation "The approximate request-token cost of the contribution."))
  (:documentation
   "Bounded observability metadata detached from request-local payload text."))

(defclass context-delivery-diagnostic ()
  ((conversation-identifier
    :initarg :conversation-identifier
    :reader context-delivery-diagnostic-conversation-identifier
    :type non-empty-string
    :documentation "The conversation for which the delivery was assembled.")
   (created-at
    :initarg :created-at
    :reader context-delivery-diagnostic-created-at
    :type timestamp
    :documentation "The delivery assembly time as Common Lisp universal time.")
   (contributions
    :initarg :contributions
    :reader context-delivery-diagnostic-contributions
    :type list
    :documentation "Metadata for contributions selected for the request.")
   (omitted
    :initarg :omitted
    :reader context-delivery-diagnostic-omitted
    :type list
    :documentation "Metadata for advisory contributions omitted by budgeting.")
   (failures
    :initarg :failures
    :reader context-delivery-diagnostic-failures
    :type list
    :documentation "Contributor identifiers paired with bounded failure reports."))
  (:documentation
   "A retained context delivery summary containing no provider payload text."))


;;;; -- Construction and Registration --

(-> context--validate-identifier (t string) string)
(defun context--validate-identifier (value field)
  "Return non-empty string VALUE after validating bounded FIELD identity."
  (unless (and (non-empty-string-p value)
               (<= (length value) *context-contribution-identifier-limit*))
    (error 'configuration-error
           :message (format nil "~A must contain 1 to ~D characters."
                            field
                            *context-contribution-identifier-limit*)))
  value)

(-> context--validate-references (t) list)
(defun context--validate-references (references)
  "Return a copied, unique list of bounded contribution REFERENCES."
  (unless (handler-case
              (let ((length (list-length references)))
                (and (integerp length)
                     (<= length *context-contribution-reference-limit*)
                     (every (lambda (reference)
                              (and (non-empty-string-p reference)
                                   (<= (length reference)
                                       *context-contribution-identifier-limit*)))
                            references)))
            (type-error ()
              nil))
    (error 'configuration-error
           :message "Context supersession references must be a bounded list of strings."))
  (remove-duplicates (copy-list references) :test #'string= :from-end t))

(-> make-context-contribution
    (&key (:identifier string) (:instruction string)
          (:evidence (option string)) (:priority integer)
          (:lifetime context-contribution-lifetime)
          (:class context-contribution-class)
          (:deduplication-key (option string)) (:supersedes list)
          (:conflict-group (option string)))
    context-contribution)
(defun make-context-contribution
    (&key identifier instruction evidence (priority 0)
      (lifetime ':while-relevant) (class ':advice) deduplication-key
      supersedes conflict-group)
  "Return one validated request-local context contribution."
  (context--validate-identifier identifier "Context contribution identifier")
  (unless (typep class 'context-contribution-class)
    (error 'configuration-error
           :message (format nil "Unsupported context class ~S." class)))
  (let ((instruction-limit
          (if (eq class ':mandatory)
              *context-mandatory-instruction-limit*
              *context-contribution-instruction-limit*)))
    (unless (and (non-empty-string-p instruction)
                 (<= (length instruction) instruction-limit))
      (error 'configuration-error
             :message
             (format nil "Context instruction must contain 1 to ~D characters."
                     instruction-limit))))
  (unless (or (null evidence)
              (and (stringp evidence)
                   (<= (length evidence)
                       *context-contribution-evidence-limit*)))
    (error 'configuration-error
           :message (format nil "Context evidence must contain at most ~D characters."
                            *context-contribution-evidence-limit*)))
  (unless (typep lifetime 'context-contribution-lifetime)
    (error 'configuration-error
           :message (format nil "Unsupported context lifetime ~S." lifetime)))
  (when deduplication-key
    (context--validate-identifier deduplication-key
                                  "Context deduplication key"))
  (when conflict-group
    (context--validate-identifier conflict-group "Context conflict group"))
  (make-instance 'context-contribution
                 :identifier identifier
                 :instruction instruction
                 :evidence evidence
                 :priority priority
                 :lifetime lifetime
                 :class class
                 :deduplication-key deduplication-key
                 :supersedes (context--validate-references supersedes)
                 :conflict-group conflict-group))

(-> context--function-designator-p (t) boolean)
(defun context--function-designator-p (value)
  "Return true when VALUE names or is an invocable contributor function."
  (and (or (functionp value)
           (and (symbolp value) (fboundp value)))
       t))

(-> register-context-contributor
    (string t &key (:source keyword))
    string)
(defun register-context-contributor
    (identifier function-designator
     &key (source *extension-registration-source*))
  "Register FUNCTION-DESIGNATOR under stable IDENTIFIER and return IDENTIFIER.

The function receives one REQUEST-CONTEXT and returns NIL, one contribution,
or a proper list of contributions. Registering the same identifier replaces
its previous definition without changing unrelated contributors. A built-in
identifier can only reload its existing named definition."
  (context--validate-identifier identifier "Context contributor identifier")
  (unless (context--function-designator-p function-designator)
    (error 'configuration-error
           :message (format nil "Context contributor ~A is not callable."
                            identifier)))
  (unless (keywordp source)
    (error 'configuration-error
           :message "A context contributor source must be a keyword."))
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (let* ((definition-name
               (and (symbolp function-designator) function-designator))
             (function
               (if definition-name
                   (symbol-function definition-name)
                   function-designator))
             (registration (list :identifier identifier
                                 :definition-name definition-name
                                 :function function
                                 :source source))
             (existing
               (find identifier *context-contributors*
                     :test #'string=
                     :key (lambda (candidate)
                            (getf candidate :identifier)))))
        (when (and existing
                   (eq (getf existing :source) ':built-in)
                   (not (and (eq source ':built-in)
                             definition-name
                             (eq definition-name
                                 (getf existing :definition-name)))))
          (error 'configuration-error
                 :message
                 (format nil "Built-in context contributor ~A cannot be replaced."
                         identifier)))
        (setf *context-contributors*
              (if existing
                  (substitute registration existing *context-contributors*)
                  (append *context-contributors* (list registration)))))))
  identifier)

(-> unregister-context-contributor (string) boolean)
(defun unregister-context-contributor (identifier)
  "Remove a non-built-in contributor IDENTIFIER and report whether it existed."
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (let ((registration
              (find identifier *context-contributors*
                    :test #'string=
                    :key (lambda (candidate)
                           (getf candidate :identifier)))))
        (when (and registration
                   (not (eq (getf registration :source) ':built-in)))
          (setf *context-contributors*
                (remove registration *context-contributors* :test #'eq))
          t)))))

(-> context--definition-identifier (symbol) string)
(defun context--definition-identifier (name)
  "Return the stable registry identifier for contributor definition NAME."
  (string-downcase (symbol-name name)))

(defmacro define-context-contributor (name lambda-list &body body)
  "Define and register a durable request-context contributor named NAME.

The expansion defines NAME as an ordinary function and registers its exact
function when the containing form is loaded or evaluated. Registration uses
NAME's lowercase symbol name as its stable identifier. BODY and LAMBDA-LIST
have the same evaluation behavior as DEFUN."
  (unless (symbolp name)
    (error "A context contributor definition name must be a symbol."))
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (eval-when (:load-toplevel :execute)
       (register-context-contributor
        ,(context--definition-identifier name)
        ',name))))

(-> context--session-state (request-context) (option context-contribution))
(defun context--session-state (request)
  "Return mutable session guidance unless a compact prompt overrides the persona."
  (unless *system-prompt-override*
    (make-context-contribution
     :identifier "session-state"
     :instruction
     (request-context-session-state (request-context-configuration request))
     :priority -1000
     :class ':mandatory)))

(eval-when (:load-toplevel :execute)
  (register-context-contributor
   "session-state" 'context--session-state :source ':built-in))

(-> context-contributor-registrations () list)
(defun context-contributor-registrations ()
  "Return a detached, ordered description of registered contributors."
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (copy-tree *context-contributors*))))

(-> context--registry-snapshot () list)
(defun context--registry-snapshot ()
  "Return a private snapshot suitable for restoring registration state."
  (context-contributor-registrations))

(-> context--registry-restore (list) null)
(defun context--registry-restore (snapshot)
  "Replace contributor registrations with detached SNAPSHOT."
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (setf *context-contributors* (copy-tree snapshot))))
  nil)

(-> context--registration-find (string) (option list))
(defun context--registration-find (identifier)
  "Return a detached registration for IDENTIFIER, when one exists."
  (find identifier (context-contributor-registrations)
        :test #'string=
        :key (lambda (registration)
               (getf registration :identifier))))

(-> context--registration-snapshot (string) (option list))
(defun context--registration-snapshot (identifier)
  "Return IDENTIFIER's detached registration and exact position, when present."
  (let ((registrations (context-contributor-registrations)))
    (loop for registration in registrations
          for position from 0
          when (string= identifier (getf registration :identifier))
            return (list :position position
                         :registration registration))))

(-> context--registration-restore (string (option list)) null)
(defun context--registration-restore (identifier snapshot)
  "Restore IDENTIFIER to exact SNAPSHOT position or remove it when absent."
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (let ((remaining
              (remove identifier *context-contributors*
                      :test #'string=
                      :key (lambda (candidate)
                             (getf candidate :identifier)))))
        (setf *context-contributors*
              (if snapshot
                  (let* ((position (min (getf snapshot :position)
                                        (length remaining)))
                         (registration
                           (copy-tree (getf snapshot :registration))))
                    (append (subseq remaining 0 position)
                            (list registration)
                            (nthcdr position remaining)))
                  remaining)))))
  nil)

(-> context--remove-registration-source (keyword) null)
(defun context--remove-registration-source (source)
  "Remove every context contributor registered from SOURCE."
  (with-extension-registry-transaction
    (with-lock-held (*context-lock*)
      (setf *context-contributors*
            (remove source *context-contributors*
                    :test #'eq
                    :key (lambda (registration)
                           (getf registration :source))))))
  nil)


;;;; -- Request Inspection --

(-> context--message-text (json-object) (option string))
(defun context--message-text (item)
  "Return concatenated textual content from one provider message ITEM."
  (let ((content (json-get item "content")))
    (when (vectorp content)
      (let ((parts
              (loop for part across content
                    when (and (json-object-p part)
                              (member (json-get part "type")
                                      '("input_text" "output_text")
                                      :test #'string=)
                              (stringp (json-get part "text")))
                      collect (json-get part "text"))))
        (when parts
          (format nil "~{~A~}" parts))))))

(-> request-context-latest-user-text (request-context) (option string))
(defun request-context-latest-user-text (context)
  "Return the newest durable user message text in CONTEXT, when present."
  (loop for item in (reverse
                     (conversation-input-items
                      (request-context-conversation context)))
        when (and (json-object-p item)
                  (string= (or (json-get item "role") "") "user"))
          do (return (context--message-text item))))


;;;; -- Resolution --

(-> context--copy-contribution
    (context-contribution string keyword)
    context-contribution)
(defun context--copy-contribution (contribution contributor source)
  "Return CONTRIBUTION with immutable registration provenance attached."
  (make-instance
   'context-contribution
   :identifier (context-contribution-identifier contribution)
   :instruction (context-contribution-instruction contribution)
   :evidence (context-contribution-evidence contribution)
   :priority (context-contribution-priority contribution)
   :lifetime (context-contribution-lifetime contribution)
   :class (context-contribution-class contribution)
   :deduplication-key (context-contribution-deduplication-key contribution)
   :supersedes (copy-list (context-contribution-supersedes contribution))
   :conflict-group (context-contribution-conflict-group contribution)
   :contributor contributor
   :source source))

(-> context--normalize-result (t string keyword) list)
(defun context--normalize-result (result contributor source)
  "Return RESULT as a validated contribution list with registration provenance."
  (let ((contributions
          (cond
            ((null result) nil)
            ((typep result 'context-contribution) (list result))
            ((handler-case
                 (let ((length (list-length result)))
                   (and (integerp length)
                        (every (lambda (value)
                                 (typep value 'context-contribution))
                               result)))
               (type-error ()
                 nil))
             result)
            (t
             (error "Contributor returned neither context contributions nor NIL.")))))
    (mapcar (lambda (contribution)
              (context--copy-contribution contribution contributor source))
            contributions)))

(-> context--contribution-key (context-contribution) string)
(defun context--contribution-key (contribution)
  "Return CONTRIBUTION's semantic deduplication identity."
  (or (context-contribution-deduplication-key contribution)
      (context-contribution-identifier contribution)))

(-> context--importance-greater-p
    (context-contribution context-contribution)
    boolean)
(defun context--importance-greater-p (left right)
  "Return true when LEFT should survive a conflict before RIGHT."
  (or (and (eq (context-contribution-class left) ':mandatory)
           (not (eq (context-contribution-class right) ':mandatory)))
      (and (eq (context-contribution-class left)
               (context-contribution-class right))
           (> (context-contribution-priority left)
              (context-contribution-priority right)))))

(-> context--deduplicate (list) list)
(defun context--deduplicate (contributions)
  "Return CONTRIBUTIONS with the strongest value retained for each semantic key."
  (let ((selected nil))
    (dolist (contribution contributions)
      (let* ((key (context--contribution-key contribution))
             (existing (find key selected
                             :test #'string=
                             :key #'context--contribution-key)))
        (cond
          ((null existing)
           (setf selected (append selected (list contribution))))
          ((context--importance-greater-p contribution existing)
           (setf selected (substitute contribution existing selected))))))
    selected))

(-> context--apply-supersession (list) list)
(defun context--apply-supersession (contributions)
  "Remove advisory contributions explicitly superseded by another contribution."
  (let ((superseded
          (remove-duplicates
           (mapcan (lambda (contribution)
                     (copy-list
                      (context-contribution-supersedes contribution)))
                   contributions)
           :test #'string=)))
    (remove-if
     (lambda (contribution)
       (and (eq (context-contribution-class contribution) ':advice)
            (or (member (context-contribution-identifier contribution)
                        superseded :test #'string=)
                (member (context--contribution-key contribution)
                        superseded :test #'string=))))
     contributions)))

(-> context--resolve-conflicts (list) list)
(defun context--resolve-conflicts (contributions)
  "Keep the strongest contribution only within explicit conflict groups."
  (let ((selected nil))
    (dolist (contribution contributions)
      (let* ((group (context-contribution-conflict-group contribution))
             (existing (and group
                            (find-if
                             (lambda (candidate)
                               (let ((candidate-group
                                       (context-contribution-conflict-group
                                        candidate)))
                                 (and candidate-group
                                      (string= group candidate-group))))
                             selected))))
        (cond
          ((null existing)
           (setf selected (append selected (list contribution))))
          ((context--importance-greater-p contribution existing)
           (setf selected (substitute contribution existing selected))))))
    selected))

(-> context--token-estimate (context-contribution) integer)
(defun context--token-estimate (contribution)
  "Return a conservative character-based token estimate for CONTRIBUTION."
  (ceiling (+ 24
              (length (context-contribution-instruction contribution))
              (length (or (context-contribution-evidence contribution) "")))
           4))

(-> context--next-request-key (string context-contribution) list)
(defun context--next-request-key (conversation-identifier contribution)
  "Return the conversation-scoped delivery key for CONTRIBUTION."
  (list conversation-identifier (context--contribution-key contribution)))

(-> context--active-next-request-keys (string list) list)
(defun context--active-next-request-keys
    (conversation-identifier contributions)
  "Return semantic keys for active next-request CONTRIBUTIONS."
  (mapcar (lambda (contribution)
            (context--next-request-key conversation-identifier contribution))
          (remove-if-not
           (lambda (contribution)
             (eq (context-contribution-lifetime contribution) ':next-request))
           contributions)))

(-> context--filter-consumed-next-request (string list) list)
(defun context--filter-consumed-next-request
    (conversation-identifier contributions)
  "Suppress next-request advice already delivered for its current activation."
  (let ((active-keys
          (context--active-next-request-keys conversation-identifier
                                             contributions)))
    (with-lock-held (*context-lock*)
      (let ((inactive-keys nil))
        (maphash (lambda (key delivered-p)
                   (declare (ignore delivered-p))
                   (when (and (string= conversation-identifier (first key))
                              (not (member key active-keys :test #'equal)))
                     (push key inactive-keys)))
                 *context-next-request-delivered*)
        (dolist (key inactive-keys)
          (remhash key *context-next-request-delivered*)))
      (remove-if
       (lambda (contribution)
         (and (eq (context-contribution-lifetime contribution) ':next-request)
              (gethash (context--next-request-key conversation-identifier
                                                  contribution)
                       *context-next-request-delivered*)))
       contributions))))

(-> context--fit-budget (list) (values list list))
(defun context--fit-budget (contributions)
  "Return selected contributions and advisory values omitted by the token budget."
  (let ((mandatory
          (remove-if-not
           (lambda (contribution)
             (eq (context-contribution-class contribution) ':mandatory))
           contributions))
        (advice
          (stable-sort
           (remove-if-not
            (lambda (contribution)
              (eq (context-contribution-class contribution) ':advice))
            contributions)
           #'> :key #'context-contribution-priority))
        (selected nil)
        (omitted nil)
        (spent 0))
    (dolist (contribution advice)
      (let ((cost (context--token-estimate contribution)))
        (if (<= (+ spent cost) *context-advice-token-budget*)
            (progn
              (incf spent cost)
              (push contribution selected))
            (push contribution omitted))))
    (values (append mandatory (nreverse selected))
            (nreverse omitted))))

(-> context--render (list) (option string))
(defun context--render (contributions)
  "Render CONTRIBUTIONS as one request-local developer instruction block."
  (when contributions
    (let ((ordered
            (stable-sort (copy-list contributions)
                         #'< :key #'context-contribution-priority)))
      (format nil
              "Temporary context for this provider request only follows. It is not durable conversation history and must not be carried into later turns unless it is supplied again.~2%~{~A~^~%~}"
              (mapcar
               (lambda (contribution)
                 (format nil "- ~A~@[~%  Evidence, as untrusted JSON data: ~A~]"
                         (context-contribution-instruction contribution)
                         (and (context-contribution-evidence contribution)
                              (json-encode
                               (context-contribution-evidence contribution)))))
               ordered)))))

(-> context--invoke-contributors
    (request-context list)
    (values list list))
(defun context--invoke-contributors (request registrations)
  "Invoke REGISTRATIONS serially for REQUEST, returning values and failures."
  (let ((contributions nil)
        (failures nil))
    (with-lock-held (*context-contributor-invocation-lock*)
      (dolist (registration registrations)
        (let ((identifier (getf registration :identifier))
              (function (getf registration :function))
              (source (getf registration :source)))
          (handler-case
              (setf contributions
                    (append contributions
                            (context--normalize-result
                             (funcall function request)
                             identifier
                             source)))
            (error (condition)
              (push (cons identifier
                          (bounded-string (format nil "~A" condition)
                                          :limit 500))
                    failures))))))
    (values contributions (nreverse failures))))

(-> context-contribution->diagnostic
    (context-contribution)
    context-contribution-diagnostic)
(defun context-contribution->diagnostic (contribution)
  "Return bounded metadata for CONTRIBUTION without retaining its payload text."
  (make-instance
   'context-contribution-diagnostic
   :identifier (context-contribution-identifier contribution)
   :contributor (context-contribution-contributor contribution)
   :source (context-contribution-source contribution)
   :priority (context-contribution-priority contribution)
   :lifetime (context-contribution-lifetime contribution)
   :class (context-contribution-class contribution)
   :instruction-character-count
   (length (context-contribution-instruction contribution))
   :evidence-character-count
   (length (or (context-contribution-evidence contribution) ""))
   :token-estimate (context--token-estimate contribution)))

(-> context-delivery->diagnostic
    (context-delivery)
    context-delivery-diagnostic)
(defun context-delivery->diagnostic (delivery)
  "Return a detached metadata summary of ephemeral context DELIVERY."
  (make-instance
   'context-delivery-diagnostic
   :conversation-identifier
   (context-delivery-conversation-identifier delivery)
   :created-at (context-delivery-created-at delivery)
   :contributions
   (mapcar #'context-contribution->diagnostic
           (context-delivery-contributions delivery))
   :omitted
   (mapcar #'context-contribution->diagnostic
           (context-delivery-omitted delivery))
   :failures (copy-tree (context-delivery-failures delivery))))

(-> context--remember-delivery (context-delivery) null)
(defun context--remember-delivery (delivery)
  "Retain DELIVERY's bounded metadata as per-conversation diagnostic state."
  (let* ((diagnostic (context-delivery->diagnostic delivery))
         (identifier
           (context-delivery-diagnostic-conversation-identifier diagnostic)))
    (with-lock-held (*context-lock*)
      (setf (gethash identifier *context-last-deliveries*) diagnostic
            *context-last-delivery-order*
            (cons identifier
                  (remove identifier *context-last-delivery-order*
                          :test #'string=)))
      (let ((evicted
              (nthcdr *context-delivery-diagnostic-limit*
                      *context-last-delivery-order*)))
        (dolist (evicted-identifier evicted)
          (remhash evicted-identifier *context-last-deliveries*))
        (when evicted
          (setf *context-last-delivery-order*
                (subseq *context-last-delivery-order*
                        0
                        *context-delivery-diagnostic-limit*))))))
  nil)

(-> context-resolve-request
    (configuration conversation vector
     &key (:goal-context (option string)) (:compaction-p boolean))
    context-delivery)
(defun context-resolve-request
    (configuration conversation tool-namespaces
     &key goal-context compaction-p)
  "Resolve, stack, budget, and render ephemeral context for one provider request."
  (let* ((request
           (make-instance 'request-context
                          :configuration configuration
                          :conversation conversation
                          :tool-namespaces tool-namespaces
                          :goal-context goal-context
                          :compaction-p compaction-p))
         (registrations (context-contributor-registrations)))
    (multiple-value-bind (contributions failures)
        (context--invoke-contributors request registrations)
      (unless (every
               (lambda (contribution)
                 (typep contribution 'context-contribution))
               *context-request-contributions*)
        (error 'configuration-error
               :message
               "Dynamically supplied request context contains an invalid contribution."))
      (let* ((resolved
               (context--filter-consumed-next-request
                (conversation-identifier conversation)
                (context--resolve-conflicts
                 (context--apply-supersession
                  (context--deduplicate
                   (append
                    *context-request-contributions*
                    contributions)))))))
        (multiple-value-bind (selected omitted)
            (context--fit-budget resolved)
          (let ((delivery
                  (make-instance
                   'context-delivery
                   :conversation-identifier
                   (conversation-identifier conversation)
                   :created-at (get-universal-time)
                   :contributions selected
                   :omitted omitted
                   :failures failures
                   :rendered (context--render selected))))
            (context--remember-delivery delivery)
            delivery))))))

(-> context-delivery-complete ((option context-delivery)) null)
(defun context-delivery-complete (delivery)
  "Consume delivered next-request contributions after a completed response."
  (when delivery
    (with-lock-held (*context-lock*)
      (dolist (contribution (context-delivery-contributions delivery))
        (when (eq (context-contribution-lifetime contribution) ':next-request)
          (setf (gethash
                 (context--next-request-key
                  (context-delivery-conversation-identifier delivery)
                  contribution)
                         *context-next-request-delivered*)
                t)))))
  nil)

(-> context-runtime-reset () null)
(defun context-runtime-reset ()
  "Discard delivery receipts and one-shot state without changing registrations."
  (with-lock-held (*context-lock*)
    (clrhash *context-next-request-delivered*)
    (clrhash *context-last-deliveries*)
    (setf *context-last-delivery-order* nil))
  nil)


;;;; -- Diagnostics --

(-> context--function-label (t) string)
(defun context--function-label (designator)
  "Return a concise printable label for contributor function DESIGNATOR."
  (if (symbolp designator)
      (format nil "~S" designator)
      "<function>"))

(-> context--contribution-status-line
    (context-contribution-diagnostic)
    string)
(defun context--contribution-status-line (contribution)
  "Return one payload-free summary line for contribution DIAGNOSTIC."
  (format
   nil
   "~A  [~(~A~), ~(~A~), ~(~A~), priority ~D, ~D token~:P, ~D instruction character~:P~@[, ~D evidence character~:P~]]  contributor ~A"
   (context-contribution-diagnostic-identifier contribution)
   (context-contribution-diagnostic-source contribution)
   (context-contribution-diagnostic-class contribution)
   (context-contribution-diagnostic-lifetime contribution)
   (context-contribution-diagnostic-priority contribution)
   (context-contribution-diagnostic-token-estimate contribution)
   (context-contribution-diagnostic-instruction-character-count contribution)
   (let ((count
           (context-contribution-diagnostic-evidence-character-count
            contribution)))
     (and (plusp count) count))
   (context-contribution-diagnostic-contributor contribution)))

(-> context--conversation-identifier
    ((or null conversation string))
    (option string))
(defun context--conversation-identifier (conversation-designator)
  "Return the identifier named by CONVERSATION-DESIGNATOR, or NIL."
  (etypecase conversation-designator
    (null
     nil)
    (conversation
     (conversation-identifier conversation-designator))
    (string
     (unless (non-empty-string-p conversation-designator)
       (error 'configuration-error
              :message "A context diagnostic conversation identifier cannot be empty."))
     conversation-designator)))

(-> context--diagnostic-delivery
    ((or null conversation string))
    (values (option string) (option context-delivery-diagnostic)))
(defun context--diagnostic-delivery (conversation-designator)
  "Return the selected conversation identifier and its newest delivery summary."
  (let ((requested-identifier
          (context--conversation-identifier conversation-designator)))
    (with-lock-held (*context-lock*)
      (let ((identifier
              (or requested-identifier
                  (first *context-last-delivery-order*))))
        (values identifier
                (and identifier
                     (gethash identifier *context-last-deliveries*)))))))

(-> context-status (&optional (or null conversation string)) string)
(defun context-status (&optional conversation-designator)
  "Return contributors and diagnostics selected by CONVERSATION-DESIGNATOR."
  (let ((registrations (context-contributor-registrations)))
    (multiple-value-bind (identifier delivery)
        (context--diagnostic-delivery conversation-designator)
      (format nil
              "Registered context contributors:~%~A~2%Last request-local context~@[ for conversation ~A~]:~%~A"
              (if registrations
                  (format nil "~{~A~^~%~}"
                          (mapcar
                           (lambda (registration)
                             (format nil "~A  [~(~A~)]  ~A"
                                     (getf registration :identifier)
                                     (getf registration :source)
                                     (context--function-label
                                      (or
                                       (getf registration :definition-name)
                                       (getf registration :function)))))
                           registrations))
                  "none")
              (and identifier
                   (conversation-identifier-display identifier))
              (cond
                ((null delivery)
                 "none assembled")
                ((and
                  (null
                   (context-delivery-diagnostic-contributions delivery))
                  (null (context-delivery-diagnostic-omitted delivery))
                  (null (context-delivery-diagnostic-failures delivery)))
                 "none active")
                (t
                 (format nil
                         "conversation ~A~%~@[active:~%~{~A~^~%~}~%~]~@[omitted by budget:~%~{~A~^~%~}~%~]~@[contributor failures:~%~{~A~^~%~}~]"
                         (conversation-identifier-display
                          (context-delivery-diagnostic-conversation-identifier
                           delivery))
                         (and
                          (context-delivery-diagnostic-contributions delivery)
                          (mapcar
                           #'context--contribution-status-line
                           (context-delivery-diagnostic-contributions
                            delivery)))
                         (and
                          (context-delivery-diagnostic-omitted delivery)
                          (mapcar
                           #'context--contribution-status-line
                           (context-delivery-diagnostic-omitted delivery)))
                         (and
                          (context-delivery-diagnostic-failures delivery)
                          (mapcar
                           (lambda (failure)
                             (format nil "~A: ~A"
                                     (first failure)
                                     (rest failure)))
                           (context-delivery-diagnostic-failures
                            delivery))))))))))
