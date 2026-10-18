(in-package #:autolith)

;;;; -- Plugin Lock --

(defparameter *plugin-lock-version* 1
  "The readable plugin lock format version.")

(deftype plugin-requirement-scope ()
  "The precedence scopes a plugin requirement can come from."
  '(member :user :global :site :project :plugin))

(defclass plugin-requirement ()
  ((scope
    :initarg :scope
    :reader plugin-requirement-scope
    :type plugin-requirement-scope
    :documentation "The precedence scope that made this requirement.")
   (requester
    :initarg :requester
    :initform nil
    :reader plugin-requirement-requester
    :type (option non-empty-string)
    :documentation "The plugin name for a plugin-scope requirement.")
   (source
    :initarg :source
    :reader plugin-requirement-source
    :type non-empty-string
    :documentation "The requested git source.")
   (ref
    :initarg :ref
    :initform nil
    :reader plugin-requirement-ref
    :type t
    :documentation "The requested ref form, or NIL for the source as-is.")
   (commit
    :initarg :commit
    :initform nil
    :reader plugin-requirement-commit
    :type (option non-empty-string)
    :documentation "The resolved commit, when resolution reached one."))
  (:documentation "One requirement for a plugin at one precedence scope."))

(defclass plugin-lock-conflict ()
  ((loser
    :initarg :loser
    :reader plugin-lock-conflict-loser
    :type plugin-requirement
    :documentation "The requirement that did not win.")
   (winner
    :initarg :winner
    :reader plugin-lock-conflict-winner
    :type plugin-requirement
    :documentation "The requirement that determined the resolved commit."))
  (:documentation "One recorded dependency conflict beside its winner."))

(defclass plugin-lock-dependency ()
  ((name
    :initarg :name
    :reader plugin-lock-dependency-name
    :type non-empty-string
    :documentation "The dependency plugin name.")
   (identity
    :initarg :identity
    :reader plugin-lock-dependency-identity
    :type plugin-identity
    :documentation "The dependency's resolved source and commit."))
  (:documentation "One edge in the transitive plugin dependency graph."))

(defclass plugin-lock-entry ()
  ((name
    :initarg :name
    :reader plugin-lock-entry-name
    :type non-empty-string
    :documentation "The resolved plugin name.")
   (source
    :initarg :source
    :reader plugin-lock-entry-source
    :type non-empty-string
    :documentation "The requested git source.")
   (ref
    :initarg :ref
    :initform nil
    :reader plugin-lock-entry-ref
    :type t
    :documentation "The requested ref form, or NIL for the source as-is.")
   (identity
    :initarg :identity
    :reader plugin-lock-entry-identity
    :type plugin-identity
    :documentation "The resolved identity of this plugin.")
   (checkout
    :initarg :checkout
    :reader plugin-lock-entry-checkout
    :type non-empty-string
    :documentation "The checkout path in the plugin store.")
   (dependencies
    :initarg :dependencies
    :initform nil
    :reader plugin-lock-entry-dependencies
    :type list
    :documentation "Direct dependency edges, which lead to other entries.")
   (winner
    :initarg :winner
    :initform nil
    :reader plugin-lock-entry-winner
    :type (option plugin-requirement)
    :documentation "The overriding requirement, or NIL when the own pin won.")
   (conflicts
    :initarg :conflicts
    :initform nil
    :reader plugin-lock-entry-conflicts
    :type list
    :documentation "Recorded conflict losers, each beside its winner."))
  (:documentation "One resolved plugin in the plugin lock."))

(defclass plugin-lock ()
  ((version
    :initarg :version
    :initform *plugin-lock-version*
    :reader plugin-lock-version
    :type integer
    :documentation "The lock format version.")
   (entries
    :initarg :entries
    :initform nil
    :reader plugin-lock-entries
    :type list
    :documentation "All resolved plugins, including transitive dependencies."))
  (:documentation "The resolved plugin set stored in plugins.lock."))

(define-condition plugin-lock-error (autolith-error)
  ((pathname
    :initarg :pathname
    :initform nil
    :reader plugin-lock-error-pathname
    :type (option pathname)
    :documentation "The lock pathname related to the failure.")
   (operation
    :initarg :operation
    :reader plugin-lock-error-operation
    :type keyword
    :documentation "The read, write, or validate operation that failed.")
   (cause
    :initarg :cause
    :initform nil
    :reader plugin-lock-error-cause
    :type t
    :documentation "The underlying storage or reader condition, if present."))
  (:documentation "A plugin lock read or write failed.")
  (:report
   (lambda (condition stream)
     (format stream "Plugin lock ~A failed~@[ at ~A~]: ~A"
             (plugin-lock-error-operation condition)
             (plugin-lock-error-pathname condition)
             (autolith-error-message condition)))))

(define-condition plugin-lock-format-error (plugin-lock-error)
  ((form
    :initarg :form
    :reader plugin-lock-format-error-form
    :type t
    :documentation "The malformed form.")
   (reason
    :initarg :reason
    :reader plugin-lock-format-error-reason
    :type string
    :documentation "The exact structural problem."))
  (:documentation "The plugin lock data does not have the supported shape.")
  (:report
   (lambda (condition stream)
     (format stream "Malformed plugin lock~@[ at ~A~]: ~A"
             (plugin-lock-error-pathname condition)
             (plugin-lock-format-error-reason condition)))))

(-> plugin-lock--format-error (t string) null)
(defun plugin-lock--format-error (form reason)
  "Signal a typed malformed-lock condition for FORM and REASON."
  (error 'plugin-lock-format-error
         :message reason
         :operation ':validate
         :form form
         :reason reason))

(-> plugin-lock--ref-p (t) boolean)
(defun plugin-lock--ref-p (value)
  "Return true when VALUE is a readable requested ref or NIL."
  (or (null value)
      (and (plist-schema-p value :keyword-keys-p t)
           t)))

(-> plugin-lock--check (t t list) null)
(defun plugin-lock--check (form tag fields)
  "Signal a format condition unless FORM passes RECORD-CHECK for TAG and FIELDS."
  (multiple-value-bind (valid-p reason)
      (record-check form
                    :tag tag
                    :fields fields
                    :allow-other-keys nil)
    (unless valid-p
      (plugin-lock--format-error form reason)))
  nil)

(-> form->plugin-requirement (t) plugin-requirement)
(defun form->plugin-requirement (form)
  "Return the plugin requirement described by FORM."
  (plugin-lock--check
   form ':requirement
   (list (list :indicator ':scope
               :validate (lambda (value)
                           (typep value 'plugin-requirement-scope))
               :required t)
         (list :indicator ':requester
               :validate (lambda (value)
                           (or (null value) (non-empty-string-p value))))
         (list :indicator ':source
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':ref
               :validate #'plugin-lock--ref-p)
         (list :indicator ':commit
               :validate (lambda (value)
                           (or (null value) (non-empty-string-p value))))))
  (let ((scope (record-property form ':scope))
        (requester (record-property form ':requester)))
    (when (and (eq scope ':plugin) (null requester))
      (plugin-lock--format-error form "a plugin requirement needs a requester"))
    (when (and (not (eq scope ':plugin)) requester)
      (plugin-lock--format-error form "only a plugin requirement names a requester"))
    (make-instance 'plugin-requirement
                   :scope scope
                   :requester requester
                   :source (copy-seq (record-property form ':source))
                   :ref (copy-tree (record-property form ':ref))
                   :commit (record-property form ':commit))))

(-> plugin-requirement->form (plugin-requirement) list)
(defun plugin-requirement->form (requirement)
  "Return REQUIREMENT as a portable readable form."
  (list :requirement
        :scope (plugin-requirement-scope requirement)
        :requester (plugin-requirement-requester requirement)
        :source (copy-seq (plugin-requirement-source requirement))
        :ref (copy-tree (plugin-requirement-ref requirement))
        :commit (plugin-requirement-commit requirement)))

(-> form->plugin-identity (t) plugin-identity)
(defun form->plugin-identity (form)
  "Return the plugin identity described by FORM."
  (plugin-lock--check
   form ':plugin-identity
   (list (list :indicator ':source
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':commit
               :validate #'non-empty-string-p
               :required t)))
  (make-instance 'plugin-identity
                 :source (copy-seq (record-property form ':source))
                 :commit (copy-seq (record-property form ':commit))))

(-> form->plugin-lock-dependency (t) plugin-lock-dependency)
(defun form->plugin-lock-dependency (form)
  "Return the dependency edge described by FORM."
  (plugin-lock--check
   form ':dependency
   (list (list :indicator ':name
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':source
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':commit
               :validate #'non-empty-string-p
               :required t)))
  (make-instance 'plugin-lock-dependency
                 :name (copy-seq (record-property form ':name))
                 :identity
                 (make-instance 'plugin-identity
                                :source (copy-seq (record-property form ':source))
                                :commit (copy-seq (record-property form ':commit)))))

(-> plugin-lock-dependency->form (plugin-lock-dependency) list)
(defun plugin-lock-dependency->form (dependency)
  "Return DEPENDENCY as a portable readable form."
  (let ((identity (plugin-lock-dependency-identity dependency)))
    (list :dependency
          :name (copy-seq (plugin-lock-dependency-name dependency))
          :source (copy-seq (plugin-identity-source identity))
          :commit (copy-seq (plugin-identity-commit identity)))))

(-> form->plugin-lock-conflict (t) plugin-lock-conflict)
(defun form->plugin-lock-conflict (form)
  "Return the conflict record described by FORM."
  (plugin-lock--check
   form ':conflict
   (list (list :indicator ':loser
               :validate (lambda (value)
                           (handler-case
                               (progn (form->plugin-requirement value) t)
                             (plugin-lock-format-error () nil)))
               :required t)
         (list :indicator ':winner
               :validate (lambda (value)
                           (handler-case
                               (progn (form->plugin-requirement value) t)
                             (plugin-lock-format-error () nil)))
               :required t)))
  (make-instance 'plugin-lock-conflict
                 :loser (form->plugin-requirement (record-property form ':loser))
                 :winner (form->plugin-requirement (record-property form ':winner))))

(-> plugin-lock-conflict->form (plugin-lock-conflict) list)
(defun plugin-lock-conflict->form (conflict)
  "Return CONFLICT as a portable readable form."
  (list :conflict
        :loser (plugin-requirement->form (plugin-lock-conflict-loser conflict))
        :winner (plugin-requirement->form (plugin-lock-conflict-winner conflict))))

(-> form->plugin-lock-entry (t) plugin-lock-entry)
(defun form->plugin-lock-entry (form)
  "Return the lock entry described by FORM."
  (plugin-lock--check
   form ':plugin
   (list (list :indicator ':name
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':source
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':ref
               :validate #'plugin-lock--ref-p)
         (list :indicator ':commit
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':checkout
               :validate #'non-empty-string-p
               :required t)
         (list :indicator ':dependencies
               :validate #'listp
               :required t)
         (list :indicator ':winner
               :validate (lambda (value)
                           (or (null value)
                               (handler-case
                                   (progn (form->plugin-requirement value) t)
                                 (plugin-lock-format-error () nil)))))
         (list :indicator ':conflicts
               :validate #'listp
               :required t)))
  (let ((source (copy-seq (record-property form ':source)))
        (commit (copy-seq (record-property form ':commit))))
    (make-instance
     'plugin-lock-entry
     :name (copy-seq (record-property form ':name))
     :source source
     :ref (copy-tree (record-property form ':ref))
     :identity (make-instance 'plugin-identity :source source :commit commit)
     :checkout (copy-seq (record-property form ':checkout))
     :dependencies (mapcar #'form->plugin-lock-dependency
                           (record-property form ':dependencies))
     :winner (let ((winner (record-property form ':winner)))
               (and winner (form->plugin-requirement winner)))
     :conflicts (mapcar #'form->plugin-lock-conflict
                        (record-property form ':conflicts)))))

(-> plugin-lock-entry->form (plugin-lock-entry) list)
(defun plugin-lock-entry->form (entry)
  "Return ENTRY as a portable readable form."
  (let ((identity (plugin-lock-entry-identity entry)))
    (list :plugin
          :name (copy-seq (plugin-lock-entry-name entry))
          :source (copy-seq (plugin-lock-entry-source entry))
          :ref (copy-tree (plugin-lock-entry-ref entry))
          :commit (copy-seq (plugin-identity-commit identity))
          :checkout (copy-seq (plugin-lock-entry-checkout entry))
          :dependencies (mapcar #'plugin-lock-dependency->form
                                (plugin-lock-entry-dependencies entry))
          :winner (let ((winner (plugin-lock-entry-winner entry)))
                    (and winner (plugin-requirement->form winner)))
          :conflicts (mapcar #'plugin-lock-conflict->form
                             (plugin-lock-entry-conflicts entry)))))

(-> plugin-lock->form (plugin-lock) list)
(defun plugin-lock->form (lock)
  "Return LOCK as the whole readable file form."
  (list :plugins-lock
        :version (plugin-lock-version lock)
        :plugins (mapcar #'plugin-lock-entry->form
                         (plugin-lock-entries lock))))

(-> form->plugin-lock (t) plugin-lock)
(defun form->plugin-lock (form)
  "Return the plugin lock described by FORM."
  (multiple-value-bind (valid-p reason)
      (record-check form
                    :tag ':plugins-lock
                    :versions (list *plugin-lock-version*)
                    :fields (list (list :indicator ':plugins
                                        :validate #'listp
                                        :required t))
                    :allow-other-keys nil)
    (unless valid-p
      (plugin-lock--format-error form reason)))
  (let* ((entries (mapcar #'form->plugin-lock-entry
                          (record-property form ':plugins)))
         (names (mapcar #'plugin-lock-entry-name entries)))
    (unless (= (length names)
               (length (remove-duplicates names :test #'string=)))
      (plugin-lock--format-error form "plugin names must be unique"))
    (make-instance 'plugin-lock :entries entries)))

(-> plugin-lock-form-p (t) boolean)
(defun plugin-lock-form-p (form)
  "Return true when FORM is one complete supported plugin lock."
  (handler-case
      (progn (form->plugin-lock form) t)
    (plugin-lock-format-error () nil)))

(-> plugin-lock-p (t) boolean)
(defun plugin-lock-p (value)
  "Return true when VALUE is a plugin lock."
  (typep value 'plugin-lock))

(-> plugin-lock-empty-p (plugin-lock) boolean)
(defun plugin-lock-empty-p (lock)
  "Return true when LOCK holds no plugin entries."
  (null (plugin-lock-entries lock)))

(-> plugin-lock-entry-find (plugin-lock string) (option plugin-lock-entry))
(defun plugin-lock-entry-find (lock name)
  "Return LOCK's entry named NAME, or NIL."
  (find name (plugin-lock-entries lock)
        :test #'string=
        :key #'plugin-lock-entry-name))

(-> plugin-lock-path (configuration) pathname)
(defun plugin-lock-path (configuration)
  "Return CONFIGURATION's plugin lock pathname under the state root."
  (merge-pathnames "plugins.lock" (config :state-root configuration)))

(-> plugin-lock-read (pathname) plugin-lock)
(defun plugin-lock-read (pathname)
  "Read the plugin lock at PATHNAME. A missing file is an empty lock."
  (if (not (probe-file pathname))
      (make-instance 'plugin-lock)
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless sole-form-p
              (plugin-lock--format-error form
                                         "the lock must contain exactly one form"))
            (form->plugin-lock form))
        (plugin-lock-format-error (condition)
          (error 'plugin-lock-format-error
                 :message (autolith-error-message condition)
                 :pathname pathname
                 :operation ':read
                 :form (plugin-lock-format-error-form condition)
                 :reason (plugin-lock-format-error-reason condition)
                 :cause condition))
        (store-error (cause)
          (error 'plugin-lock-error
                 :message (format nil "Could not read the plugin lock: ~A" cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))
        (error (cause)
          (error 'plugin-lock-error
                 :message (format nil "Could not read the plugin lock: ~A" cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause)))))

(-> plugin-lock-write (plugin-lock pathname) pathname)
(defun plugin-lock-write (lock pathname)
  "Atomically write LOCK to PATHNAME and return the pathname."
  (let ((form (plugin-lock->form lock)))
    (unless (plugin-lock-form-p form)
      (plugin-lock--format-error form
                                 "the lock data does not round-trip"))
    (handler-case
        (progn
          (snapshot-write pathname form :mode #o600)
          pathname)
      (store-error (cause)
        (error 'plugin-lock-error
               :message (format nil "Could not write the plugin lock: ~A" cause)
               :pathname pathname
               :operation ':write
               :cause cause)))))

(-> plugin-lock-load (configuration) plugin-lock)
(defun plugin-lock-load (configuration)
  "Read CONFIGURATION's plugin lock."
  (plugin-lock-read (plugin-lock-path configuration)))

(-> plugin-lock-save (configuration plugin-lock) pathname)
(defun plugin-lock-save (configuration lock)
  "Write LOCK to CONFIGURATION's state root."
  (plugin-lock-write lock (plugin-lock-path configuration)))

(defmethod print-object ((lock plugin-lock) stream)
  "Print LOCK with its version and entry count."
  (print-unreadable-object (lock stream :type t)
    (format stream "version ~D, ~D plugin~:P"
            (plugin-lock-version lock)
            (length (plugin-lock-entries lock)))))
