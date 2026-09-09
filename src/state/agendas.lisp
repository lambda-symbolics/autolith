(in-package #:autolith)

;;;; -- Workspace Agendas --

(defparameter *agenda-version* 2
  "The readable workspace-agenda state format version.")

(defparameter *agenda-legacy-version* 1
  "The agenda format version without persistent-memory links.")

(defparameter *agenda-item-text-limit* 500
  "The maximum character count of one agenda item.")

(defparameter *agenda-item-memory-limit* 16
  "The maximum memory identifiers linked to one agenda item.")

(defparameter *agenda-memory-identifier-limit* 128
  "The maximum characters in one linked memory identifier.")

(defvar *agenda-lock* (make-recursive-lock "Autolith workspace agendas")
  "Serialize same-process Autolith agenda reads and read-modify-write transactions.")

(deftype agenda-status ()
  "The lifecycle or informational role of one agenda item."
  '(member :todo :doing :blocked :done :note))

(defclass agenda-item ()
  ((identifier
    :initarg :identifier
    :reader agenda-item-identifier
    :type non-empty-string
    :documentation "The stable identifier of this agenda item.")
   (text
    :initarg :text
    :reader agenda-item-text
    :type non-empty-string
    :documentation "The bounded task, thought, or note text.")
   (status
    :initarg :status
    :reader agenda-item-status
    :type agenda-status
    :documentation "The current lifecycle or informational status.")
   (created-at
    :initarg :created-at
    :reader agenda-item-created-at
    :type timestamp
    :documentation "The universal time at which this item was created.")
   (updated-at
    :initarg :updated-at
    :reader agenda-item-updated-at
    :type timestamp
    :documentation "The universal time at which this item last changed.")
   (memory-identifiers
    :initarg :memory-identifiers
    :initform nil
    :reader agenda-item-memory-identifiers
    :type list
    :documentation "Stable persistent-memory identifiers attached to this item."))
  (:documentation "One stable task, thought, or note in a workspace agenda."))

(defclass workspace-agenda ()
  ((directory
    :initarg :directory
    :reader workspace-agenda-directory
    :type non-empty-string
    :documentation "The canonical or transported workspace directory key.")
   (items
    :initarg :items
    :initform nil
    :reader workspace-agenda-items
    :type list
    :documentation "The ordered agenda items for this workspace."))
  (:documentation "The short persistent agenda associated with one workspace."))

(defclass agenda-state ()
  ((records
    :initarg :records
    :initform nil
    :accessor agenda-state-records
    :type list
    :documentation "Every known workspace agenda, ordered by directory key."))
  (:documentation "Validated user-specific agendas for all known workspaces."))

(-> agenda--memory-identifiers-p (t) boolean)
(defun agenda--memory-identifiers-p (identifiers)
  "Return true when IDENTIFIERS is a bounded unique memory-id list."
  (handler-case
      (let ((length (list-length identifiers)))
        (and (integerp length)
             (<= length *agenda-item-memory-limit*)
             (every (lambda (identifier)
                      (and (non-empty-string-p identifier)
                           (<= (length identifier)
                               *agenda-memory-identifier-limit*)))
                    identifiers)
             (= length
                (length (remove-duplicates identifiers :test #'string=)))))
    (type-error ()
      nil)))

(-> agenda--item-form-p (t integer) boolean)
(defun agenda--item-form-p (form version)
  "Return true when FORM is one complete portable agenda item for VERSION."
  (handler-case
      (and (sexp-store:record-shape-p form)
           (eq (first form) ':item)
           (let ((properties (rest form)))
             (and (= (length properties)
                     (if (= version *agenda-legacy-version*) 10 12))
                  (every (lambda (property)
                           (readable-state-property-present-p properties
                                                              property))
                         '(:id :text :status :created-at :updated-at))
                  (non-empty-string-p (getf properties :id))
                  (let ((text (getf properties :text)))
                    (and (non-empty-string-p text)
                         (<= (length text) *agenda-item-text-limit*)))
                  (typep (getf properties :status) 'agenda-status)
                  (typep (getf properties :created-at) 'timestamp)
                  (typep (getf properties :updated-at) 'timestamp)
                  (or (= version *agenda-legacy-version*)
                      (and (readable-state-property-present-p
                            properties :memory-ids)
                           (agenda--memory-identifiers-p
                            (getf properties :memory-ids)))))))
    (error ()
      nil)))

(-> agenda--record-form-p (t integer) boolean)
(defun agenda--record-form-p (form version)
  "Return true when FORM is one complete portable workspace agenda for VERSION."
  (handler-case
      (and (sexp-store:record-shape-p form)
           (eq (first form) ':agenda)
           (let* ((properties (rest form))
                  (items (getf properties :items)))
             (and (= (length properties) 4)
                  (readable-state-property-present-p properties :directory)
                  (readable-state-property-present-p properties :items)
                  (non-empty-string-p (getf properties :directory))
                  (listp items)
                  (every (lambda (item)
                           (agenda--item-form-p item version))
                         items)
                  (= (length (remove-duplicates
                              (mapcar (lambda (item)
                                        (getf (rest item) :id))
                                      items)
                              :test #'string=))
                     (length items)))))
    (error ()
      nil)))

(-> agenda--form-p (t) boolean)
(defun agenda--form-p (form)
  "Return true when FORM is one supported workspace-agenda state."
  (handler-case
      (let ((version (and (sexp-store:record-shape-p form) (third form))))
        (and version (= (length form) 5)
             (eq (first form) ':agendas)
             (eq (second form) ':version)
             (member version (list *agenda-legacy-version* *agenda-version*))
             (eq (fourth form) ':records)
             (listp (fifth form))
             (every (lambda (record)
                      (agenda--record-form-p record version))
                    (fifth form))
             (= (length (remove-duplicates
                         (mapcar (lambda (record)
                                   (getf (rest record) :directory))
                                 (fifth form))
                         :test #'string=))
                (length (fifth form)))))
    (error ()
      nil)))

(-> agenda--item-form->item (list integer) agenda-item)
(defun agenda--item-form->item (form version)
  "Return the agenda item represented by validated FORM and VERSION."
  (let ((properties (rest form)))
    (make-instance 'agenda-item
                   :identifier (copy-seq (getf properties :id))
                   :text (copy-seq (getf properties :text))
                   :status (getf properties :status)
                   :created-at (getf properties :created-at)
                   :updated-at (getf properties :updated-at)
                   :memory-identifiers
                   (if (= version *agenda-legacy-version*)
                       nil
                       (copy-list (getf properties :memory-ids))))))

(-> agenda--record-form->record (list integer) workspace-agenda)
(defun agenda--record-form->record (form version)
  "Return the workspace agenda represented by validated FORM and VERSION."
  (let ((properties (rest form)))
    (make-instance 'workspace-agenda
                   :directory (copy-seq (getf properties :directory))
                   :items (mapcar (lambda (item)
                                    (agenda--item-form->item item version))
                                  (getf properties :items)))))

(-> agenda--sort-records (list) list)
(defun agenda--sort-records (records)
  "Return a fresh directory-ordered copy of workspace agenda RECORDS."
  (sort (copy-list records) #'string< :key #'workspace-agenda-directory))

(-> agenda--store (configuration) sexp-store:snapshot-store)
(defun agenda--store (configuration)
  "Describe the agenda snapshot schema and product-owned state paths."
  (let ((pathname (configuration-agenda-path configuration)))
    (make-instance
     'sexp-store:snapshot-store
     :pathname pathname
     :lock-pathname (readable-state-lock-pathname pathname "agendas.lock")
     :initial-state (lambda () (make-instance 'agenda-state))
     :validator #'agenda--form-p
     :decoder
     (lambda (form)
       (make-instance
        'agenda-state
        :records (agenda--sort-records
                  (mapcar (lambda (record)
                            (agenda--record-form->record record (third form)))
                          (fifth form)))))
     :encoder #'agenda--state-form)))

(-> agenda--store-error (sexp-store:store-error) nil)
(defun agenda--store-error (cause)
  "Translate storage CAUSE without changing the agenda corruption policy."
  (error 'agenda-error
         :message (sexp-store:store-error-message cause)
         :pathname (sexp-store:store-error-pathname cause)
         :operation (sexp-store:store-error-operation cause)
         :cause cause))

(-> agenda--read (configuration &key (:lock-held-p boolean)) agenda-state)
(defun agenda--read (configuration &key lock-held-p)
  "Read fresh agenda state, optionally under a caller's coordinated store lock."
  (handler-case
      (sexp-store:store-read (agenda--store configuration) :lock-held-p lock-held-p)
    (sexp-store:store-error (cause)
      (agenda--store-error cause))))

(-> agenda-load (configuration) agenda-state)
(defun agenda-load (configuration)
  "Return workspace agendas, warning and using empty state after corruption."
  (handler-case
      (agenda--read configuration)
    (agenda-error (condition)
      (warn 'agenda-load-warning
            :pathname (agenda-error-pathname condition)
            :cause condition)
      (make-instance 'agenda-state))))

(-> agenda--item->form (agenda-item) list)
(defun agenda--item->form (item)
  "Return ITEM as one portable readable form."
  (list :item
        :id (agenda-item-identifier item)
        :text (agenda-item-text item)
        :status (agenda-item-status item)
        :created-at (agenda-item-created-at item)
        :updated-at (agenda-item-updated-at item)
        :memory-ids (agenda-item-memory-identifiers item)))

(-> agenda--record->form (workspace-agenda) list)
(defun agenda--record->form (record)
  "Return workspace agenda RECORD as one portable readable form."
  (list :agenda
        :directory (workspace-agenda-directory record)
        :items (mapcar #'agenda--item->form
                       (workspace-agenda-items record))))

(-> agenda--state-form (agenda-state) list)
(defun agenda--state-form (state)
  "Return STATE as one portable readable form."
  (list :agendas
        :version *agenda-version*
        :records (mapcar #'agenda--record->form
                         (agenda-state-records state))))

(-> agenda-directory-name
    (configuration (or pathname string) &key (:require-existing-p boolean))
    string)
(defun agenda-directory-name
    (configuration location &key require-existing-p)
  "Return LOCATION as an absolute directory key.

Existing locations are canonicalized. A transported source may remain missing
when REQUIRE-EXISTING-P is false, but it must still name an absolute path."
  (handler-case
      (let ((existing (uiop:directory-exists-p location)))
        (cond
          (existing
           (namestring (uiop:ensure-directory-pathname (platform-truename *platform* existing))))
          (require-existing-p
           (error 'agenda-error
                  :message (format nil "Agenda directory ~A does not exist."
                                   location)
                  :pathname (configuration-agenda-path configuration)
                  :operation ':validate-directory
                  :cause nil))
          (t
           (let ((pathname
                   (uiop:ensure-pathname (platform-pathname location)
                                         :ensure-absolute t
                                         :ensure-directory t
                                         :want-non-wild t)))
             (namestring pathname)))))
    (agenda-error (condition)
      (error condition))
    (error (cause)
      (error 'agenda-error
             :message (format nil "Cannot use ~A as an agenda directory."
                              location)
             :pathname (configuration-agenda-path configuration)
             :operation ':validate-directory
             :cause cause))))

(-> agenda-find (agenda-state string) (option workspace-agenda))
(defun agenda-find (state directory)
  "Return STATE's workspace agenda keyed by DIRECTORY, when present."
  (find directory
        (agenda-state-records state)
        :key #'workspace-agenda-directory
        :test #'string=))

(-> agenda-current (configuration agenda-state) (option workspace-agenda))
(defun agenda-current (configuration state)
  "Return STATE's agenda for CONFIGURATION's current workspace."
  (agenda-find state
               (agenda-directory-name
                configuration
                (configuration-working-directory configuration)
                :require-existing-p t)))

(-> agenda--prompt-item-line (agenda-item) string)
(defun agenda--prompt-item-line (item)
  "Return one untrusted agenda item line for the system prompt."
  (format nil "- [~(~A~)] ~A  ~A~@[  memory_ids: ~A~]"
          (agenda-item-status item)
          (agenda-item-identifier item)
          (json-encode (agenda-item-text item))
          (and (agenda-item-memory-identifiers item)
               (json-encode
                (coerce (agenda-item-memory-identifiers item) 'vector)))))

(-> agenda--prompt-visible-item-p (agenda-item) boolean)
(defun agenda--prompt-visible-item-p (item)
  "Return true when ITEM is active state worth injecting into request context."
  (not (null (member (agenda-item-status item)
                     '(:todo :doing :blocked :note)))))

(-> agenda-prompt-item-lines (configuration) (option string))
(defun agenda-prompt-item-lines (configuration)
  "Return active agenda item lines without the lead-in, or NIL when empty."
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((state (agenda-load configuration))
           (record (agenda-current configuration state))
           (items
             (and record
                  (remove-if-not #'agenda--prompt-visible-item-p
                                 (workspace-agenda-items record)))))
      (when items
        (format nil "~{~A~^~%~}" (mapcar #'agenda--prompt-item-line items))))))

(-> agenda--replace-record
    (list workspace-agenda &key (:remove-directory (option string)))
    list)
(defun agenda--replace-record (records replacement &key remove-directory)
  "Return RECORDS with REPLACEMENT installed and REMOVE-DIRECTORY omitted."
  (agenda--sort-records
   (cons replacement
         (remove-if
          (lambda (record)
            (or (string= (workspace-agenda-directory record)
                         (workspace-agenda-directory replacement))
                (and remove-directory
                     (string= (workspace-agenda-directory record)
                              remove-directory))))
          records))))

(-> agenda--validate-text (configuration string) string)
(defun agenda--validate-text (configuration text)
  "Return a copied valid agenda TEXT or signal a typed validation failure."
  (unless (and (non-empty-string-p text)
               (<= (length text) *agenda-item-text-limit*))
    (error 'agenda-error
           :message (format nil "Agenda text must contain 1 to ~D characters."
                            *agenda-item-text-limit*)
           :pathname (configuration-agenda-path configuration)
           :operation ':validate-item
           :cause nil))
  (copy-seq text))

(-> agenda--validate-memory-identifiers (configuration t) list)
(defun agenda--validate-memory-identifiers (configuration identifiers)
  "Return copied active memory IDENTIFIERS or signal an agenda failure."
  (unless (agenda--memory-identifiers-p identifiers)
    (error 'agenda-error
           :message (format nil
                            "Agenda memory-ids must be a unique list of at most ~D bounded strings."
                            *agenda-item-memory-limit*)
           :pathname (configuration-agenda-path configuration)
           :operation ':validate-item
           :cause nil))
  (dolist (identifier identifiers)
    (handler-case
        (unless (memory-find configuration identifier)
          (error 'agenda-error
                 :message (format nil "Memory ~A does not exist." identifier)
                 :pathname (configuration-agenda-path configuration)
                 :operation ':validate-item
                 :cause nil))
      (agenda-error (condition)
        (error condition))
      (memory-error (cause)
        (error 'agenda-error
               :message (format nil "Cannot validate linked memory ~A: ~A"
                                identifier
                                (autolith-error-message cause))
               :pathname (configuration-agenda-path configuration)
               :operation ':validate-item
               :cause cause))))
  (copy-list identifiers))

(-> agenda--add-unlocked
    (&key (:configuration configuration) (:state agenda-state)
          (:text string) (:status agenda-status)
          (:memory-identifiers list) (:now timestamp))
    agenda-item)
(defun agenda--add-unlocked
    (&key configuration state text (status ':todo) memory-identifiers
          (now (get-universal-time)))
  "Add a new agenda item to CONFIGURATION's current workspace."
  (unless (typep status 'agenda-status)
    (error 'agenda-error
           :message (format nil "Unsupported agenda status ~S." status)
           :pathname (configuration-agenda-path configuration)
           :operation ':validate-item
           :cause nil))
  (let* ((directory
           (agenda-directory-name
            configuration
            (configuration-working-directory configuration)
            :require-existing-p t))
         (record (agenda-find state directory))
         (items (and record (workspace-agenda-items record))))
    (let* ((item
             (make-instance 'agenda-item
                            :identifier (make-identifier)
                            :text (agenda--validate-text configuration text)
                            :status status
                            :created-at now
                            :updated-at now
                            :memory-identifiers
                            (agenda--validate-memory-identifiers
                             configuration memory-identifiers)))
           (replacement
             (make-instance 'workspace-agenda
                            :directory directory
                            :items (append items (list item))))
           (records (agenda--replace-record (agenda-state-records state)
                                             replacement)))
      (setf (agenda-state-records state) records)
      item)))

(-> agenda--update-unlocked
    (configuration agenda-state string
     &key (:text (option string)) (:status (option agenda-status))
          (:memory-identifiers list) (:now timestamp))
    agenda-item)
(defun agenda--update-unlocked
    (configuration state identifier
     &key text status (memory-identifiers nil memory-identifiers-supplied-p)
       (now (get-universal-time)))
  "Update IDENTIFIER in the current workspace and return its replacement."
  (when (and status (not (typep status 'agenda-status)))
    (error 'agenda-error
           :message (format nil "Unsupported agenda status ~S." status)
           :pathname (configuration-agenda-path configuration)
           :operation ':validate-item
           :cause nil))
  (unless (or text status memory-identifiers-supplied-p)
    (error 'agenda-error
           :message "agenda.update requires text, status, or memory-ids."
           :pathname (configuration-agenda-path configuration)
           :operation ':update
           :cause nil))
  (let* ((record (agenda-current configuration state))
         (item (and record
                    (find identifier
                          (workspace-agenda-items record)
                          :key #'agenda-item-identifier
                          :test #'string=))))
    (unless item
      (error 'agenda-error
             :message (format nil "Agenda item ~A does not exist here."
                              identifier)
             :pathname (configuration-agenda-path configuration)
             :operation ':update
             :cause nil))
    (let* ((replacement-item
             (make-instance 'agenda-item
                            :identifier (copy-seq identifier)
                            :text (if text
                                      (agenda--validate-text configuration text)
                                      (copy-seq (agenda-item-text item)))
                            :status (or status (agenda-item-status item))
                            :created-at (agenda-item-created-at item)
                            :updated-at now
                            :memory-identifiers
                            (if memory-identifiers-supplied-p
                                (agenda--validate-memory-identifiers
                                 configuration memory-identifiers)
                                (copy-list
                                 (agenda-item-memory-identifiers item)))))
           (replacement-record
             (make-instance
              'workspace-agenda
              :directory (workspace-agenda-directory record)
              :items (substitute replacement-item
                                 identifier
                                 (workspace-agenda-items record)
                                 :key #'agenda-item-identifier
                                 :test #'string=)))
           (records (agenda--replace-record (agenda-state-records state)
                                            replacement-record)))
      (setf (agenda-state-records state) records)
      replacement-item)))

(-> agenda--remove-unlocked (configuration agenda-state string) boolean)
(defun agenda--remove-unlocked (configuration state identifier)
  "Remove current-workspace agenda IDENTIFIER and report whether it existed."
  (let* ((record (agenda-current configuration state))
         (items (and record (workspace-agenda-items record)))
         (remaining (remove identifier items
                            :key #'agenda-item-identifier
                            :test #'string=)))
    (if (= (length remaining) (length items))
        nil
        (let* ((directory (workspace-agenda-directory record))
               (records
                 (if remaining
                     (agenda--replace-record
                      (agenda-state-records state)
                      (make-instance 'workspace-agenda
                                     :directory directory
                                     :items remaining))
                     (remove directory
                             (agenda-state-records state)
                             :key #'workspace-agenda-directory
                             :test #'string=))))
          (setf (agenda-state-records state) records)
          t))))

(-> agenda--copy-item
    (agenda-item &key (:identifier (option string)) (:memory-identifiers list))
    agenda-item)
(defun agenda--copy-item
    (item &key identifier
          (memory-identifiers nil memory-identifiers-supplied-p))
  "Return a detached copy of ITEM with optional identifier and memory links."
  (make-instance 'agenda-item
                 :identifier (or identifier
                                 (copy-seq (agenda-item-identifier item)))
                 :text (copy-seq (agenda-item-text item))
                 :status (agenda-item-status item)
                 :created-at (agenda-item-created-at item)
                 :updated-at (agenda-item-updated-at item)
                 :memory-identifiers
                 (copy-list
                  (if memory-identifiers-supplied-p
                      memory-identifiers
                      (agenda-item-memory-identifiers item)))))

(-> agenda--merge-memory-identifiers (configuration list list) list)
(defun agenda--merge-memory-identifiers (configuration target source)
  "Return stable union of TARGET and SOURCE memory identifiers."
  (let ((merged (remove-duplicates (append target source)
                                   :test #'string=
                                   :from-end t)))
    (when (> (length merged) *agenda-item-memory-limit*)
      (error 'agenda-error
             :message (format nil
                              "Transport would attach more than ~D memories to one agenda item."
                              *agenda-item-memory-limit*)
             :pathname (configuration-agenda-path configuration)
             :operation ':transport
             :cause nil))
    merged))

(-> agenda--merge-items (configuration list list) list)
(defun agenda--merge-items (configuration target-items source-items)
  "Return TARGET-ITEMS followed by non-duplicate copies from SOURCE-ITEMS."
  (let ((result (copy-list target-items)))
    (dolist (source source-items)
      (let ((duplicate
              (find-if
               (lambda (target)
                 (and (string= (agenda-item-text source)
                               (agenda-item-text target))
                      (eq (agenda-item-status source)
                          (agenda-item-status target))))
               result)))
        (if duplicate
            (setf result
                  (substitute
                   (agenda--copy-item
                    duplicate
                    :memory-identifiers
                    (agenda--merge-memory-identifiers
                     configuration
                     (agenda-item-memory-identifiers duplicate)
                     (agenda-item-memory-identifiers source)))
                   duplicate
                   result
                   :test #'eq))
            (let ((identifier (agenda-item-identifier source)))
              (setf result
                    (append
                     result
                     (list
                      (agenda--copy-item
                       source
                       :identifier
                       (if (find identifier result
                                 :key #'agenda-item-identifier
                                 :test #'string=)
                           (make-identifier)
                           (copy-seq identifier))))))))))
    result))

(-> agenda--transport-unlocked
    (&key (:configuration configuration) (:state agenda-state)
          (:source-directory (or pathname string))
          (:target-directory (or pathname string)) (:move-p boolean))
    workspace-agenda)
(defun agenda--transport-unlocked
    (&key configuration state source-directory target-directory move-p)
  "Copy or move SOURCE-DIRECTORY's agenda into existing TARGET-DIRECTORY."
  (let* ((source-name
           (agenda-directory-name configuration source-directory))
         (target-name
           (agenda-directory-name configuration target-directory
                                  :require-existing-p t))
         (source (agenda-find state source-name))
         (target (agenda-find state target-name)))
    (unless source
      (error 'agenda-error
             :message (format nil "No agenda is keyed by ~A." source-name)
             :pathname (configuration-agenda-path configuration)
             :operation ':transport
             :cause nil))
    (when (string= source-name target-name)
      (error 'agenda-error
             :message "Agenda source and target directories are identical."
             :pathname (configuration-agenda-path configuration)
             :operation ':transport
             :cause nil))
    (let* ((items (agenda--merge-items
                   configuration
                   (and target (workspace-agenda-items target))
                   (workspace-agenda-items source)))
           (replacement (make-instance 'workspace-agenda
                                       :directory target-name
                                       :items items))
           (records
             (agenda--replace-record
              (agenda-state-records state)
              replacement
              :remove-directory (and move-p source-name))))
      (setf (agenda-state-records state) records)
      replacement)))


;;;; -- Process-Shared Transactions --

(-> agenda--call-with-transaction (configuration agenda-state function) t)
(defun agenda--call-with-transaction (configuration state function)
  "Apply FUNCTION to private state and install it only after durable publication."
  (with-recursive-lock-held (*agenda-lock*)
    (handler-case
        (sexp-store:store-transact
         (agenda--store configuration)
         (lambda (current)
           (let ((value (funcall function current)))
             (values current value (not (null value)))))
         :publish (lambda (committed)
                    (setf (agenda-state-records state)
                          (agenda-state-records committed))))
      (sexp-store:store-error (cause)
        (agenda--store-error cause)))))

(-> agenda-add
    (&key (:configuration configuration) (:state agenda-state)
          (:text string) (:status agenda-status)
          (:memory-identifiers list) (:now timestamp))
    agenda-item)
(defun agenda-add
    (&key configuration state text (status ':todo) memory-identifiers
          (now (get-universal-time)))
  "Add an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction
   configuration state
   (lambda (current)
     (agenda--add-unlocked :configuration configuration
                           :state current
                           :text text
                           :status status
                           :memory-identifiers memory-identifiers
                           :now now))))

(-> agenda-update
    (configuration agenda-state string
     &key (:text (option string)) (:status (option agenda-status))
          (:memory-identifiers list) (:now timestamp))
    agenda-item)
(defun agenda-update
    (configuration state identifier
     &key text status (memory-identifiers nil memory-identifiers-supplied-p)
       (now (get-universal-time)))
  "Update an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction
   configuration state
   (lambda (current)
     (apply #'agenda--update-unlocked
            configuration current identifier
            (append (and text (list :text text))
                    (and status (list :status status))
                    (and memory-identifiers-supplied-p
                         (list :memory-identifiers memory-identifiers))
                    (list :now now))))))

(-> agenda-remove (configuration agenda-state string) boolean)
(defun agenda-remove (configuration state identifier)
  "Remove an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction
   configuration state
   (lambda (current)
     (agenda--remove-unlocked configuration current identifier))))

(-> agenda-transport
    (&key (:configuration configuration) (:state agenda-state)
          (:source-directory (or pathname string))
          (:target-directory (or pathname string)) (:move-p boolean))
    workspace-agenda)
(defun agenda-transport
    (&key configuration state source-directory target-directory move-p)
  "Transport an agenda in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction
   configuration state
   (lambda (current)
     (agenda--transport-unlocked :configuration configuration
                                 :state current
                                 :source-directory source-directory
                                 :target-directory target-directory
                                 :move-p move-p))))
