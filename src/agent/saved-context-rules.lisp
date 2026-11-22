(in-package #:autolith)

;;;; -- Persistent Saved Context Rules --

(defparameter *context-rule-saved-version* 1
  "The readable version of a saved context-rule collection.")

(define-condition context-rule-storage-error (context-rule-error)
  ((pathname
    :initarg       :pathname
    :reader        context-rule-storage-error-pathname
    :type          pathname
    :documentation "The affected saved-rule pathname.")
   (operation
    :initarg       :operation
    :reader        context-rule-storage-error-operation
    :type          keyword
    :documentation "The storage operation that failed.")
   (cause
    :initarg       :cause
    :reader        context-rule-storage-error-cause
    :type          t
    :documentation "The underlying storage condition."))
  (:documentation "A saved context-rule file could not be read or written."))

(define-condition context-rule-corruption-error (context-rule-storage-error)
  ()
  (:documentation "A saved context-rule file is malformed or unsafe."))

(define-condition context-rule-io-error (context-rule-storage-error)
  ()
  (:documentation "A saved context-rule file encountered an input/output failure."))

(define-condition context-rule-save-conflict (context-rule-error)
  ((rule-id
    :initarg       :rule-id
    :reader        context-rule-save-conflict-rule-id
    :type          string
    :documentation "The existing rule identifier.")
   (pathname
    :initarg       :pathname
    :reader        context-rule-save-conflict-pathname
    :type          pathname
    :documentation "The collection containing the existing rule."))
  (:documentation "A saved rule ID already exists and replacement was not requested."))

(-> context-rule-saved--scope-p (t) boolean)
(defun context-rule-saved--scope-p (scope)
  "Return true when SCOPE names a supported saved-rule collection."
  (if (or (eq scope :workspace)
          (eq scope :global))
      t
      nil))

(-> context-rule-saved--canonical-workspace (configuration) string)
(defun context-rule-saved--canonical-workspace (configuration)
  "Return the canonical workspace identity for CONFIGURATION."
  (context-rule--workspace-name (config :working-directory configuration)))

(-> context-rule-saved--workspace-key (configuration) string)
(defun context-rule-saved--workspace-key (configuration)
  "Return the stable digest key for CONFIGURATION's canonical workspace."
  (memory-source-text-digest (context-rule-saved--canonical-workspace configuration)
                             :babel-utf8-p t))

(-> context-rule-saved-path (configuration keyword) pathname)
(defun context-rule-saved-path (configuration scope)
  "Return the private user-owned pathname for saved rules in SCOPE.

SCOPE is either :GLOBAL or :WORKSPACE. Workspace files are keyed by the
canonical working directory and never reside in the repository."
  (unless (context-rule-saved--scope-p scope)
    (error 'context-rule-error :field ':scope
           :message                   "Saved context-rule scope must be :global or :workspace."))
  (if (eq scope :global)
      (merge-pathnames "context-rules.sexp" (config :data-root configuration))
      (merge-pathnames
       (format nil "context-rules/~A.sexp"
               (context-rule-saved--workspace-key configuration))
       (config :data-root configuration))))

(-> context-rule-saved--lock-path (configuration keyword) pathname)
(defun context-rule-saved--lock-path (configuration scope)
  "Return the stable advisory lock pathname for SCOPE."
  (merge-pathnames
   (if (eq scope :global)
       "context-rules.lock"
       (format nil "context-rules/~A.lock"
               (context-rule-saved--workspace-key configuration)))
   (config :state-root configuration)))

(-> context-rule-saved--rule-p (t) boolean)
(defun context-rule-saved--rule-p (rule)
  "Return true when RULE is a finite, validated context-rule property list."
  (if (and (sexp-store:record-finite-p rule)
           (context-rule--plist-p rule)
           (handler-case
               (progn (context-rule--validate rule) t)
            (error () nil)))
      t
      nil))

(-> context-rule-saved--rules-p (t) boolean)
(defun context-rule-saved--rules-p (rules)
  "Return true for a finite rule list with unique string identifiers."
  (if (and (sexp-store:record-finite-p rules)
           (handler-case
               (and (listp rules)
                    (every #'context-rule-saved--rule-p rules)
                    (let ((ids (mapcar (lambda (rule) (getf rule :id)) rules)))
                      (and (every #'stringp ids)
                           (= (length ids)
                              (length (remove-duplicates ids :test #'string=))))))
             (type-error () nil)))
      t
      nil))

(-> context-rule-saved--form-p (t) boolean)
(defun context-rule-saved--form-p (form)
  "Validate the complete on-disk collection FORM before decoding it."
  (if (and (sexp-store:record-shape-p form :duplicate-keys ':reject)
           (eq (first form) ':context-rules)
           (= 5 (length form))
           (eql (getf (rest form) :version) *context-rule-saved-version*)
           (context-rule-saved--rules-p (getf (rest form) :rules ':missing)))
      t
      nil))

(-> context-rule-saved--decode (list) list)
(defun context-rule-saved--decode (form)
  "Decode a validated collection FORM into detached rule data."
  (mapcar #'context-rule--validate (getf (rest form) :rules)))

(-> context-rule-saved--encode (list) list)
(defun context-rule-saved--encode (rules)
  "Encode RULES as the versioned readable collection record."
  (list :context-rules :version *context-rule-saved-version* :rules rules))

(-> context-rule-saved--store (configuration keyword) sexp-store:snapshot-store)
(defun context-rule-saved--store (configuration scope)
  "Construct the locked atomic snapshot store for SCOPE."
  (make-instance 'sexp-store:snapshot-store
                 :pathname       (context-rule-saved-path configuration scope)
                 :lock-pathname  (context-rule-saved--lock-path configuration scope)
                 :initial-state  #'list
                 :validator      #'context-rule-saved--form-p
                 :duplicate-keys ':reject
                 :decoder        #'context-rule-saved--decode
                 :encoder        #'context-rule-saved--encode))

(-> context-rule-saved--storage-failure (pathname keyword t) nil)
(defun context-rule-saved--storage-failure (pathname operation cause)
  "Signal a typed saved-rule failure for OPERATION and CAUSE."
  (if (or (eq operation ':validate)
          (and (eq operation ':read)
               (typep cause 'sexp-store:store-error)
               (typep (sexp-store:store-error-cause cause) '(or reader-error end-of-file))))
      (error 'context-rule-corruption-error
             :field   ':rules
             :message (format nil "Malformed saved context rules at ~A: ~A"
                              pathname cause)
             :pathname pathname :operation operation :cause cause)
      (error 'context-rule-io-error
             :field   ':rules
             :message (format nil "Could not access saved context rules at ~A: ~A"
                              pathname cause)
             :pathname pathname :operation operation :cause cause)))

(-> context-rule-saved--read (configuration keyword) list)
(defun context-rule-saved--read (configuration scope)
  "Read and validate SCOPE, translating library failures to domain conditions."
  (let ((pathname (context-rule-saved-path configuration scope)))
    (handler-case
        (sexp-store:store-read (context-rule-saved--store configuration scope))
      (sexp-store:store-error (cause)
        (context-rule-saved--storage-failure
         pathname (sexp-store:store-error-operation cause) cause))
      (file-error (cause)
        (context-rule-saved--storage-failure pathname ':read cause)))))

(-> context-rule-saved--validated (configuration keyword list) list)
(defun context-rule-saved--validated (configuration scope rule)
  "Return a detached validated RULE annotated with its owning workspace."
  (unless (and (sexp-store:record-finite-p rule)
               (context-rule--plist-p rule))
    (error 'context-rule-error :field ':rule
           :message                   "Saved context rule must be a finite property list."))
  (let ((candidate (copy-tree rule)))
    (setf (getf candidate :workspace)
          (when (eq scope :workspace)
            (context-rule-saved--canonical-workspace configuration)))
    (context-rule--validate candidate)))

(-> context-rule-saved (configuration &key (:scope (or null keyword))) list)
(defun context-rule-saved (configuration &key scope)
  "Return detached validated saved rules, global followed by workspace.

When SCOPE is supplied, return only that collection. Workspace rules override
a same-ID global rule for the canonical current workspace."
  (unless (or (null scope) (context-rule-saved--scope-p scope))
    (error 'context-rule-error :field ':scope
           :message                   "Saved context-rule scope must be :global, :workspace, or omitted."))
  (let* ((workspace-rules (when (or (null scope) (eq scope :workspace))
                            (context-rule-saved--read configuration :workspace)))
         (global-rules    (when (or (null scope) (eq scope :global))
                            (context-rule-saved--read configuration :global))))
    (cond
      ((eq scope :global)
       (mapcar (lambda (rule)
                 (context-rule-saved--validated configuration :global rule))
               global-rules))
      ((eq scope :workspace)
       (mapcar (lambda (rule)
                 (context-rule-saved--validated configuration :workspace rule))
               workspace-rules))
      (t
       (let ((workspace-ids (mapcar (lambda (rule) (getf rule :id))
                                    workspace-rules)))
         (append
          (mapcar (lambda (rule)
                    (context-rule-saved--validated configuration :global rule))
                  (remove-if (lambda (rule)
                               (member (getf rule :id) workspace-ids :test #'string=))
                             global-rules))
          (mapcar (lambda (rule)
                    (context-rule-saved--validated configuration :workspace rule))
                  workspace-rules)))))))

(-> context-rule-save (configuration list &key (:scope keyword) (:replace-p boolean)) pathname)
(defun context-rule-save (configuration rule &key (scope ':workspace) replace-p)
  "Validate and atomically save RULE, returning its storage pathname.

Duplicate IDs signal CONTEXT-RULE-SAVE-CONFLICT unless REPLACE-P is true."
  (unless (context-rule-saved--scope-p scope)
    (error 'context-rule-error :field ':scope
           :message                   "Saved context-rule scope must be :global or :workspace."))
  (let* ((pathname      (context-rule-saved-path configuration scope))
         (lock-pathname (context-rule-saved--lock-path configuration scope))
         (validated     (context-rule-saved--validated configuration scope rule)))
    (handler-case
        (progn
          (ensure-directories-exist pathname)
          (ensure-directories-exist lock-pathname)
          (sexp-store:store-transact
           (context-rule-saved--store configuration scope)
           (lambda (current)
             (let ((existing (find (getf validated :id) current
                                   :test #'string=
                                   :key  (lambda (saved) (getf saved :id)))))
               (when (and existing (not replace-p))
                 (error 'context-rule-save-conflict
                        :field    ':id
                        :message  "A saved context-rule with this ID already exists."
                        :rule-id  (getf validated :id)
                        :pathname pathname))
               (values (if existing
                           (mapcar (lambda (saved)
                                     (if (eq saved existing) validated saved))
                                   current)
                           (append current (list validated)))
                       nil t))))
          pathname)
      (sexp-store:store-error (cause)
        (context-rule-saved--storage-failure
         pathname (sexp-store:store-error-operation cause) cause))
      (file-error (cause)
        (context-rule-saved--storage-failure pathname ':write cause)))))

(-> context-rule-saved-contributions (request-context) list)
(defun context-rule-saved-contributions (request)
  "Return matching saved-rule contributions for REQUEST's configuration."
  (when (request-context-compaction-p request)
    (return-from context-rule-saved-contributions nil))
  (let* ((configuration (request-context-configuration request))
         (rules         (context-rule-saved configuration)))
    (loop for rule in rules
          when (context-rule--matches-p rule request)
            collect (make-context-contribution
                     :identifier
                     (if (getf rule :workspace)
                         (format nil "saved-context-rule/workspace/~A/~A"
                                 (context-rule-saved--workspace-key configuration)
                                 (getf rule :id))
                         (format nil "saved-context-rule/global/~A" (getf rule :id)))
                     :instruction (getf rule :instruction)
                     :priority    (getf rule :priority 0)
                     :lifetime    (getf rule :lifetime ':while-relevant)))))

(define-context-contributor context-rule-saved-contributor (request)
  (context-rule-saved-contributions request))
