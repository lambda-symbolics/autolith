(in-package #:autolith-supervisor)

(defvar *agenda-transaction*
  nil
  "The current private transaction, available only to nested store operations.")

(defgeneric agenda-context-path
    (context)
  (:documentation "Return the trusted private snapshot pathname for CONTEXT."))

(defgeneric agenda-context-directory
    (context)
  (:documentation "Return CONTEXT's trusted workspace directory key."))

(defgeneric agenda-context-memory-valid-p
    (context identifier)
  (:documentation "Validate IDENTIFIER in CONTEXT's permitted memory scope."))

(defgeneric agenda-directory-name
    (context location &key require-existing-p)
  (:documentation "Resolve LOCATION through CONTEXT's trusted directory authority."))

(serapeum/types:-> agenda--string-p (t) boolean)
(defun agenda--string-p (value)
  "Return whether VALUE is a nonempty string."
  (and (stringp value) (plusp (length value))))

(deftype agenda-string ()
  "A nonempty agenda string."
  '(and string (satisfies agenda--string-p)))

(defclass agenda-context nil
          ((pathname :initarg :pathname :reader agenda-context-path :type pathname
                     :documentation "The trusted private agenda snapshot pathname.")
           (directory :initarg :directory :reader agenda-context-directory :type string
                      :documentation "The trusted current workspace directory key.")
           (memory-validator :initarg :memory-validator :initform nil :reader
            agenda-context-memory-validator :documentation
            "An optional scoped memory identity validator.")
           (directory-resolver :initarg :directory-resolver :initform nil :reader
            agenda-context-directory-resolver :documentation
            "An optional trusted directory canonicalizer."))
          (:documentation
           "Explicit parent authority for one agenda store and workspace."))

(defmethod agenda-context-memory-valid-p ((context agenda-context) identifier)
  "Use only the trusted memory validator; absent authority denies memory links."
  (let ((validator (agenda-context-memory-validator context)))
    (and validator (not (null (funcall validator identifier))))))

(defmethod agenda-directory-name
           ((context agenda-context) location &key require-existing-p)
  "Resolve an absolute workspace key without reading agent configuration."
  (let ((resolver (agenda-context-directory-resolver context)))
    (if resolver
        (funcall resolver location require-existing-p)
        (let ((directory
               (uiop/pathname:ensure-pathname location :ensure-absolute t
                                              :ensure-directory t :want-non-wild t)))
          (when
              (and require-existing-p
                   (not (uiop/filesystem:directory-exists-p directory)))
            (error 'agenda-error :message "The agenda directory does not exist."
                   :pathname (agenda-context-path context) :operation
                   ':validate-directory :cause nil))
          (namestring directory)))))

(serapeum/types:-> agenda--lock-pathname (pathname string) pathname)
(defun agenda--lock-pathname (pathname name)
  "Return the common store lock pathname beside PATHNAME."
  (merge-pathnames name (uiop/pathname:pathname-directory-pathname pathname)))

(serapeum/types:-> agenda--make-identifier nil string)
(defun agenda--make-identifier ()
  "Return a fresh identifier from the operating-system random source."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(define-condition agenda-error
    (autolith-error)
    ((pathname :initarg :pathname :reader agenda-error-pathname :type pathname
               :documentation "The agenda snapshot pathname.")
     (operation :initarg :operation :reader agenda-error-operation :type keyword
      :documentation "The failed agenda operation.")
     (cause :initarg :cause :reader agenda-error-cause :documentation
      "The underlying failure when available."))
  (:documentation "Agenda validation or storage failed."))

(define-condition agenda-load-warning
    (warning)
    ((pathname :initarg :pathname :reader agenda-load-warning-pathname :type pathname
               :documentation "The malformed agenda pathname.")
     (cause :initarg :cause :reader agenda-load-warning-cause :type agenda-error
      :documentation "The agenda read failure."))
  (:report
   (lambda (condition stream)
     (format stream "Ignoring workspace agendas at ~A: ~A"
             (agenda-load-warning-pathname condition)
             (agenda-load-warning-cause condition))))
  (:documentation "Malformed agenda state was ignored during startup."))

(defparameter *agenda-version*
  3
  "The agenda snapshot version with atomic record-operation receipts.")

(defparameter *agenda-legacy-version*
  1
  "The agenda format version without persistent-memory links.")

(defparameter *agenda-item-text-limit*
  500
  "The maximum character count of one agenda item.")

(defparameter *agenda-item-memory-limit*
  16
  "The maximum memory identifiers linked to one agenda item.")

(defparameter *agenda-memory-identifier-limit*
  128
  "The maximum characters in one linked memory identifier.")

(defvar *agenda-lock*
  (bordeaux-threads:make-recursive-lock "Autolith workspace agendas")
  "Serialize same-process Autolith agenda reads and read-modify-write transactions.")

(deftype agenda-status ()
  "The lifecycle or informational role of one agenda item."
  '(member :todo :doing :blocked :done :note))

(defclass agenda-item nil
          ((identifier :initarg :identifier :reader agenda-item-identifier :type
            agenda-string :documentation "The stable identifier of this agenda item.")
           (text :initarg :text :reader agenda-item-text :type agenda-string
            :documentation "The bounded task, thought, or note text.")
           (status :initarg :status :reader agenda-item-status :type agenda-status
            :documentation "The current lifecycle or informational status.")
           (created-at :initarg :created-at :reader agenda-item-created-at :type
            (integer 0 *) :documentation
            "The universal time at which this item was created.")
           (updated-at :initarg :updated-at :reader agenda-item-updated-at :type
            (integer 0 *) :documentation
            "The universal time at which this item last changed.")
           (memory-identifiers :initarg :memory-identifiers :initform nil :reader
            agenda-item-memory-identifiers :type list :documentation
            "Stable persistent-memory identifiers attached to this item."))
          (:documentation "One stable task, thought, or note in a workspace agenda."))

(defclass workspace-agenda nil
          ((directory :initarg :directory :reader workspace-agenda-directory :type
                      agenda-string :documentation
                      "The canonical or transported workspace directory key.")
           (items :initarg :items :initform nil :reader workspace-agenda-items :type
            list :documentation "The ordered agenda items for this workspace."))
          (:documentation "The short persistent agenda associated with one workspace."))

(defclass agenda-state nil
          ((records :initarg :records :initform nil :accessor agenda-state-records
            :type list :documentation "Ordered workspace agenda records.")
           (operations :initarg :operations :initform nil :accessor
            agenda-state-operations :type list :documentation
            "Durable session-scoped operation identities and results."))
          (:documentation "Validated agendas and their atomic operation receipts."))

(serapeum/types:-> agenda--memory-identifiers-p (t) boolean)
(defun agenda--memory-identifiers-p (identifiers)
  "Return true when IDENTIFIERS is a bounded unique memory-id list."
  (handler-case
   (let ((length (list-length identifiers)))
     (and (integerp length) (<= length *agenda-item-memory-limit*)
          (every
           (lambda (identifier)
             (and (agenda--string-p identifier)
                  (<= (length identifier) *agenda-memory-identifier-limit*)))
           identifiers)
          (= length (length (remove-duplicates identifiers :test #'string=)))))
   (type-error nil nil)))

(serapeum/types:-> agenda--item-form-p (t integer) boolean)
(defun agenda--item-form-p (form version)
  "Return true when FORM is one complete portable agenda item for VERSION."
  (handler-case
   (and (sexp-store:record-shape-p form) (eq (first form) ':item)
        (let ((properties (rest form)))
          (and
           (= (length properties)
              (if (= version *agenda-legacy-version*)
                  10
                  12))
           (every
            (lambda (property) (sexp-store:record-property-present-p form property))
            '(:id :text :status :created-at :updated-at))
           (agenda--string-p (getf properties :id))
           (let ((text (getf properties :text)))
             (and (agenda--string-p text) (<= (length text) *agenda-item-text-limit*)))
           (typep (getf properties :status) 'agenda-status)
           (typep (getf properties :created-at) '(integer 0 *))
           (typep (getf properties :updated-at) '(integer 0 *))
           (or (= version *agenda-legacy-version*)
               (and (sexp-store:record-property-present-p form :memory-ids)
                    (agenda--memory-identifiers-p (getf properties :memory-ids)))))))
   (error nil nil)))

(serapeum/types:-> agenda--record-form-p (t integer) boolean)
(defun agenda--record-form-p (form version)
  "Return true when FORM is one complete portable workspace agenda for VERSION."
  (handler-case
   (and (sexp-store:record-shape-p form) (eq (first form) ':agenda)
        (let* ((properties (rest form)) (items (getf properties :items)))
          (and (= (length properties) 4)
               (sexp-store:record-property-present-p form :directory)
               (sexp-store:record-property-present-p form :items)
               (agenda--string-p (getf properties :directory)) (listp items)
               (every (lambda (item) (agenda--item-form-p item version)) items)
               (=
                (length
                 (remove-duplicates
                  (mapcar (lambda (item) (getf (rest item) :id)) items) :test
                  #'string=))
                (length items)))))
   (error nil nil)))

(serapeum/types:-> agenda--form-p (t) boolean)
(defun agenda--form-p (form)
  "Validate a supported agenda snapshot before decoding it."
  (handler-case
   (let ((version (and (sexp-store:record-shape-p form) (third form))))
     (and (member version '(1 2 3))
          (= (length form)
             (if (= version 3)
                 7
                 5))
          (eq (first form) ':agendas) (eq (second form) ':version)
          (eq (fourth form) ':records) (listp (fifth form))
          (every (lambda (record) (agenda--record-form-p record version)) (fifth form))
          (=
           (length
            (remove-duplicates
             (mapcar (lambda (record) (getf (rest record) :directory)) (fifth form))
             :test #'string=))
           (length (fifth form)))
          (or (/= version 3)
              (and (eq (sixth form) ':operations)
                   (agenda--operation-ledger-p (seventh form))))))
   (error nil nil)))

(serapeum/types:-> agenda--item-form->item (list integer) agenda-item)
(defun agenda--item-form->item (form version)
  "Return the agenda item represented by validated FORM and VERSION."
  (let ((properties (rest form)))
    (make-instance 'agenda-item :identifier (copy-seq (getf properties :id)) :text
                   (copy-seq (getf properties :text)) :status (getf properties :status)
                   :created-at (getf properties :created-at) :updated-at
                   (getf properties :updated-at) :memory-identifiers
                   (if (= version *agenda-legacy-version*)
                       nil
                       (copy-list (getf properties :memory-ids))))))

(serapeum/types:-> agenda--record-form->record (list integer) workspace-agenda)
(defun agenda--record-form->record (form version)
  "Return the workspace agenda represented by validated FORM and VERSION."
  (let ((properties (rest form)))
    (make-instance 'workspace-agenda :directory (copy-seq (getf properties :directory))
                   :items
                   (mapcar (lambda (item) (agenda--item-form->item item version))
                           (getf properties :items)))))

(serapeum/types:-> agenda--sort-records (list) list)
(defun agenda--sort-records (records)
  "Return a fresh directory-ordered copy of workspace agenda RECORDS."
  (sort (copy-list records) #'string< :key #'workspace-agenda-directory))

(serapeum/types:-> agenda--store (t) sexp-store:snapshot-store)
(defun agenda--store (configuration)
  "Describe the agenda snapshot schema and product-owned state paths."
  (let ((pathname (agenda-context-path configuration)))
    (make-instance 'sexp-store:snapshot-store :pathname pathname :lock-pathname
                   (agenda--lock-pathname pathname "agendas.lock") :initial-state
                   (lambda () (make-instance 'agenda-state)) :validator
                   #'agenda--form-p :decoder
                   (lambda (form)
                     (make-instance 'agenda-state :records
                                    (agenda--sort-records
                                     (mapcar
                                      (lambda (record)
                                        (agenda--record-form->record record
                                         (third form)))
                                      (fifth form)))
                                    :operations
                                    (if (= (third form) 3)
                                        (seventh form)
                                        nil)))
                   :encoder #'agenda--state-form)))

(serapeum/types:-> agenda--store-error (sexp-store:store-error) nil)
(defun agenda--store-error (cause)
  "Translate storage CAUSE without changing the agenda corruption policy."
  (error 'agenda-error :message (sexp-store:store-error-message cause) :pathname
         (sexp-store:store-error-pathname cause) :operation
         (sexp-store:store-error-operation cause) :cause cause))

(serapeum/types:-> agenda--read (t &key (:lock-held-p boolean)) agenda-state)
(defun agenda--read (configuration &key lock-held-p)
  "Read fresh agenda state, optionally under a caller's coordinated store lock."
  (handler-case
   (sexp-store:store-read (agenda--store configuration) :lock-held-p lock-held-p)
   (sexp-store:store-error (cause) (agenda--store-error cause))))

(serapeum/types:-> agenda-load (t) agenda-state)
(defun agenda-load (configuration)
  "Return workspace agendas, warning and using empty state after corruption."
  (handler-case (agenda--read configuration)
                (agenda-error (condition)
                 (warn 'agenda-load-warning :pathname (agenda-error-pathname condition)
                       :cause                         condition)
                 (make-instance 'agenda-state))))

(serapeum/types:-> agenda--item->form (agenda-item) list)
(defun agenda--item->form (item)
  "Return ITEM as one portable readable form."
  (list :item :id (agenda-item-identifier item) :text (agenda-item-text item) :status
        (agenda-item-status item) :created-at (agenda-item-created-at item) :updated-at
        (agenda-item-updated-at item) :memory-ids
        (agenda-item-memory-identifiers item)))

(serapeum/types:-> agenda--record->form (workspace-agenda) list)
(defun agenda--record->form (record)
  "Return workspace agenda RECORD as one portable readable form."
  (list :agenda :directory (workspace-agenda-directory record) :items
        (mapcar #'agenda--item->form (workspace-agenda-items record))))

(serapeum/types:-> agenda--state-form (agenda-state) list)
(defun agenda--state-form (state)
  "Return STATE as a complete portable snapshot."
  (list ':agendas :version *agenda-version* :records
        (mapcar #'agenda--record->form (agenda-state-records state)) :operations
        (agenda-state-operations state)))

(serapeum/types:-> agenda-find (agenda-state string) (or null workspace-agenda))
(defun agenda-find (state directory)
  "Return STATE's workspace agenda keyed by DIRECTORY, when present."
  (find directory (agenda-state-records state) :key #'workspace-agenda-directory :test
        #'string=))

(serapeum/types:-> agenda-current (t agenda-state) (or null workspace-agenda))
(defun agenda-current (configuration state)
  "Return STATE's agenda for CONFIGURATION's current workspace."
  (agenda-find state
   (agenda-directory-name configuration (agenda-context-directory configuration)
    :require-existing-p t)))

(serapeum/types:-> agenda--replace-record
                   (list workspace-agenda &key (:remove-directory (or null string)))
                   list)
(defun agenda--replace-record (records replacement &key remove-directory)
  "Return RECORDS with REPLACEMENT installed and REMOVE-DIRECTORY omitted."
  (agenda--sort-records
   (cons replacement
         (remove-if
          (lambda (record)
            (or
             (string= (workspace-agenda-directory record)
                      (workspace-agenda-directory replacement))
             (and remove-directory
                  (string= (workspace-agenda-directory record) remove-directory))))
          records))))

(serapeum/types:-> agenda--validate-text (t string) string)
(defun agenda--validate-text (configuration text)
  "Return a copied valid agenda TEXT or signal a typed validation failure."
  (unless (and (agenda--string-p text) (<= (length text) *agenda-item-text-limit*))
    (error 'agenda-error :message
           (format nil "Agenda text must contain 1 to ~D characters."
                   *agenda-item-text-limit*)
           :pathname (agenda-context-path configuration) :operation ':validate-item
           :cause nil))
  (copy-seq text))

(serapeum/types:-> agenda--validate-memory-identifiers (t t) list)
(defun agenda--validate-memory-identifiers (context identifiers)
  "Return detached identifiers after checking the trusted memory scope."
  (unless (agenda--memory-identifiers-p identifiers)
    (error 'agenda-error :message
           "Agenda memory identifiers must be unique bounded strings." :pathname
           (agenda-context-path context) :operation ':validate-item :cause nil))
  (dolist (identifier identifiers)
    (handler-case
     (unless (agenda-context-memory-valid-p context identifier)
       (error 'agenda-error :message "A linked memory is not available in this scope."
              :pathname (agenda-context-path context) :operation ':validate-item :cause
              nil))
     (agenda-error (condition) (error condition))
     (error (cause)
            (error 'agenda-error :message "Cannot validate a linked memory." :pathname
                   (agenda-context-path context) :operation ':validate-item :cause
                   cause))))
  (copy-list identifiers))

(serapeum/types:-> agenda--add-unlocked
                   (&key (:configuration t) (:state agenda-state) (:text string)
                    (:status agenda-status) (:memory-identifiers list)
                    (:now (integer 0 *)))
                   agenda-item)
(defun agenda--add-unlocked
       (
        &key configuration state text (status ':todo) memory-identifiers
        (now (get-universal-time)))
  "Add a new agenda item to CONFIGURATION's current workspace."
  (unless (typep status 'agenda-status)
    (error 'agenda-error :message (format nil "Unsupported agenda status ~S." status)
           :pathname (agenda-context-path configuration) :operation ':validate-item
           :cause                 nil))
  (let* ((directory
          (agenda-directory-name configuration (agenda-context-directory configuration)
           :require-existing-p t))
         (record (agenda-find state directory))
         (items  (and record (workspace-agenda-items record))))
    (let* ((item
            (make-instance 'agenda-item :identifier (agenda--make-identifier) :text
                           (agenda--validate-text configuration text) :status status
                           :created-at now :updated-at now :memory-identifiers
                           (agenda--validate-memory-identifiers configuration
                            memory-identifiers)))
           (replacement
            (make-instance 'workspace-agenda :directory directory :items
                           (append items (list item))))
           (records (agenda--replace-record (agenda-state-records state) replacement)))
      (setf (agenda-state-records state) records)
      item)))

(serapeum/types:-> agenda--update-unlocked
                   (t agenda-state string &key (:text (or null string))
                    (:status (or null agenda-status)) (:memory-identifiers list)
                    (:now (integer 0 *)))
                   agenda-item)
(defun agenda--update-unlocked
       (configuration state identifier
        &key text status (memory-identifiers nil memory-identifiers-supplied-p)
        (now (get-universal-time)))
  "Update IDENTIFIER in the current workspace and return its replacement."
  (when (and status (not (typep status 'agenda-status)))
    (error 'agenda-error :message (format nil "Unsupported agenda status ~S." status)
           :pathname (agenda-context-path configuration) :operation ':validate-item
           :cause                 nil))
  (unless (or text status memory-identifiers-supplied-p)
    (error 'agenda-error :message "agenda.update requires text, status, or memory-ids."
           :pathname (agenda-context-path configuration) :operation ':update :cause
           nil))
  (let* ((record (agenda-current configuration state))
         (item
          (and record
               (find identifier (workspace-agenda-items record) :key
                     #'agenda-item-identifier :test #'string=))))
    (unless item
      (error 'agenda-error :message
             (format nil "Agenda item ~A does not exist here." identifier) :pathname
             (agenda-context-path configuration) :operation ':update :cause nil))
    (let* ((replacement-item
            (make-instance 'agenda-item :identifier (copy-seq identifier) :text
                           (if text
                               (agenda--validate-text configuration text)
                               (copy-seq (agenda-item-text item)))
                           :status (or status (agenda-item-status item)) :created-at
                           (agenda-item-created-at item) :updated-at now
                           :memory-identifiers
                           (if memory-identifiers-supplied-p
                               (agenda--validate-memory-identifiers configuration
                                memory-identifiers)
                               (copy-list (agenda-item-memory-identifiers item)))))
           (replacement-record
            (make-instance 'workspace-agenda :directory
                           (workspace-agenda-directory record) :items
                           (substitute replacement-item identifier
                                       (workspace-agenda-items record) :key
                                       #'agenda-item-identifier :test #'string=)))
           (records
            (agenda--replace-record (agenda-state-records state) replacement-record)))
      (setf (agenda-state-records state) records)
      replacement-item)))

(serapeum/types:-> agenda--remove-unlocked (t agenda-state string) boolean)
(defun agenda--remove-unlocked (configuration state identifier)
  "Remove current-workspace agenda IDENTIFIER and report whether it existed."
  (let* ((record (agenda-current configuration state))
         (items  (and record (workspace-agenda-items record)))
         (remaining
          (remove identifier items :key #'agenda-item-identifier :test #'string=)))
    (if (= (length remaining) (length items))
        nil
        (let* ((directory (workspace-agenda-directory record))
               (records
                (if remaining
                    (agenda--replace-record (agenda-state-records state)
                     (make-instance 'workspace-agenda :directory directory :items
                                    remaining))
                    (remove directory (agenda-state-records state) :key
                            #'workspace-agenda-directory :test #'string=))))
          (setf (agenda-state-records state) records)
          t))))

(serapeum/types:-> agenda--copy-item
                   (agenda-item &key (:identifier (or null string))
                    (:memory-identifiers list))
                   agenda-item)
(defun agenda--copy-item
       (item &key identifier (memory-identifiers nil memory-identifiers-supplied-p))
  "Return a detached copy of ITEM with optional identifier and memory links."
  (make-instance 'agenda-item :identifier
                 (or identifier (copy-seq (agenda-item-identifier item))) :text
                 (copy-seq (agenda-item-text item)) :status (agenda-item-status item)
                 :created-at (agenda-item-created-at item) :updated-at
                 (agenda-item-updated-at item) :memory-identifiers
                 (copy-list
                  (if memory-identifiers-supplied-p
                      memory-identifiers
                      (agenda-item-memory-identifiers item)))))

(serapeum/types:-> agenda--merge-memory-identifiers (t list list) list)
(defun agenda--merge-memory-identifiers (configuration target source)
  "Return stable union of TARGET and SOURCE memory identifiers."
  (let ((merged (remove-duplicates (append target source) :test #'string= :from-end t)))
    (when (> (length merged) *agenda-item-memory-limit*)
      (error 'agenda-error :message
             (format nil
                     "Transport would attach more than ~D memories to one agenda item."
                     *agenda-item-memory-limit*)
             :pathname (agenda-context-path configuration) :operation ':transport
             :cause nil))
    merged))

(serapeum/types:-> agenda--merge-items (t list list) list)
(defun agenda--merge-items (configuration target-items source-items)
  "Return TARGET-ITEMS followed by non-duplicate copies from SOURCE-ITEMS."
  (let ((result (copy-list target-items)))
    (dolist (source source-items)
      (let ((duplicate
             (find-if
              (lambda (target)
                (and (string= (agenda-item-text source) (agenda-item-text target))
                     (eq (agenda-item-status source) (agenda-item-status target))))
              result)))
        (if duplicate
            (setf result
                    (substitute
                     (agenda--copy-item duplicate :memory-identifiers
                      (agenda--merge-memory-identifiers configuration
                       (agenda-item-memory-identifiers duplicate)
                       (agenda-item-memory-identifiers source)))
                     duplicate result :test #'eq))
            (let ((identifier (agenda-item-identifier source)))
              (setf result
                      (append result
                              (list
                               (agenda--copy-item source :identifier
                                (if (find identifier result :key
                                          #'agenda-item-identifier :test #'string=)
                                    (agenda--make-identifier)
                                    (copy-seq identifier))))))))))
    result))

(serapeum/types:-> agenda--transport-unlocked
                   (&key (:configuration t) (:state agenda-state)
                    (:source-directory (or pathname string))
                    (:target-directory (or pathname string)) (:move-p boolean))
                   workspace-agenda)
(defun agenda--transport-unlocked
       (&key configuration state source-directory target-directory move-p)
  "Copy or move SOURCE-DIRECTORY's agenda into existing TARGET-DIRECTORY."
  (let* ((source-name (agenda-directory-name configuration source-directory))
         (target-name
          (agenda-directory-name configuration target-directory :require-existing-p t))
         (source      (agenda-find state source-name))
         (target      (agenda-find state target-name)))
    (unless source
      (error 'agenda-error :message
             (format nil "No agenda is keyed by ~A." source-name) :pathname
             (agenda-context-path configuration) :operation ':transport :cause nil))
    (when (string= source-name target-name)
      (error 'agenda-error :message
             "Agenda source and target directories are identical." :pathname
             (agenda-context-path configuration) :operation ':transport :cause nil))
    (let* ((items
            (agenda--merge-items configuration
             (and target (workspace-agenda-items target))
             (workspace-agenda-items source)))
           (replacement
            (make-instance 'workspace-agenda :directory target-name :items items))
           (records
            (agenda--replace-record (agenda-state-records state) replacement
             :remove-directory (and move-p source-name))))
      (setf (agenda-state-records state) records)
      replacement)))

(serapeum/types:-> agenda--call-with-transaction (t agenda-state function) t)
(defun agenda--call-with-transaction (configuration state function)
  "Apply FUNCTION in one locked snapshot transaction, including nested calls."
  (if (and *agenda-transaction* (eq configuration (first *agenda-transaction*)))
      (funcall function (second *agenda-transaction*))
      (bordeaux-threads:with-recursive-lock-held (*agenda-lock*)
        (handler-case
         (sexp-store:store-transact (agenda--store configuration)
                                    (lambda (current)
                                      (let* ((*agenda-transaction*
                                              (list configuration current))
                                             (value (funcall function current)))
                                        (values current value (not (null value)))))
                                    :publish
                                    (lambda (committed)
                                      (setf (agenda-state-records state)
                                            (agenda-state-records committed)
                                            (agenda-state-operations state)
                                            (agenda-state-operations committed))))
         (sexp-store:store-error (cause) (agenda--store-error cause))))))

(serapeum/types:-> agenda-add
                   (&key (:configuration t) (:state agenda-state) (:text string)
                    (:status agenda-status) (:memory-identifiers list)
                    (:now (integer 0 *)))
                   agenda-item)
(defun agenda-add
       (
        &key configuration state text (status ':todo) memory-identifiers
        (now (get-universal-time)))
  "Add an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction configuration state
   (lambda (current)
     (agenda--add-unlocked :configuration configuration :state current :text text
      :status status :memory-identifiers memory-identifiers :now now))))

(serapeum/types:-> agenda-update
                   (t agenda-state string &key (:text (or null string))
                    (:status (or null agenda-status)) (:memory-identifiers list)
                    (:now (integer 0 *)))
                   agenda-item)
(defun agenda-update
       (configuration state identifier
        &key text status (memory-identifiers nil memory-identifiers-supplied-p)
        (now (get-universal-time)))
  "Update an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction configuration state
   (lambda (current)
     (apply #'agenda--update-unlocked configuration current identifier
            (append (and text (list :text text)) (and status (list :status status))
                    (and memory-identifiers-supplied-p
                         (list :memory-identifiers memory-identifiers))
                    (list :now now))))))

(serapeum/types:-> agenda-remove (t agenda-state string) boolean)
(defun agenda-remove (configuration state identifier)
  "Remove an item in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction configuration state
   (lambda (current) (agenda--remove-unlocked configuration current identifier))))

(serapeum/types:-> agenda-transport
                   (&key (:configuration t) (:state agenda-state)
                    (:source-directory (or pathname string))
                    (:target-directory (or pathname string)) (:move-p boolean))
                   workspace-agenda)
(defun agenda-transport
       (&key configuration state source-directory target-directory move-p)
  "Transport an agenda in one process-shared read-modify-write transaction."
  (agenda--call-with-transaction configuration state
   (lambda (current)
     (agenda--transport-unlocked :configuration configuration :state current
      :source-directory source-directory :target-directory target-directory :move-p
      move-p))))

(serapeum/types:-> agenda--operation-ledger-p (t) boolean)
(defun agenda--operation-ledger-p (ledger)
  "Validate receipt shapes, payloads, results, and unique session operation IDs."
  (let ((seen (make-hash-table :test #'equal)))
    (handler-case
     (and (listp ledger) (integerp (list-length ledger)) (<= (length ledger) 16384)
          (every
           (lambda (entry)
             (and (sexp-store:record-shape-p entry) (eq (first entry) ':operation)
                  (= (length entry) 13)
                  (every
                   (lambda (key) (sexp-store:record-property-present-p entry key))
                   '(:session :directory :id :payload :result :revision))
                  (let* ((properties (rest entry))
                         (session    (getf properties :session))
                         (id         (getf properties :id))
                         (payload    (getf properties :payload))
                         (result     (getf properties :result))
                         (revision   (getf properties :revision)))
                    (and (agenda--string-p session) (<= (length session) 1024)
                         (agenda--string-p id) (<= (length id) 1024)
                         (agenda--string-p (getf properties :directory))
                         (agenda--string-p revision) (<= (length revision) 128)
                         (not (gethash (list session id) seen))
                         (progn (setf (gethash (list session id) seen) t) t)
                         (listp payload) (member (list-length payload) '(4 5))
                         (or (= (length payload) 4)
                             (and (member (first payload) '(:create :replace :delete))
                                  (agenda--string-p (fifth payload))
                                  (<= (length (fifth payload)) 1024)))
                         (member (first payload) '(:create :replace :delete))
                         (stringp (second payload)) (stringp (third payload))
                         (if (eq (first payload) ':delete)
                             (and (null (fourth payload)) (null result))
                             (let ((fields (fourth payload)))
                               (and (listp fields) (eql (list-length fields) 8)
                                    (every (lambda (key) (member key fields))
                                           '(:text :status :memory-ids
                                             :memory-ids-present-p))
                                    (agenda--string-p (getf fields :text))
                                    (<= (length (getf fields :text))
                                        *agenda-item-text-limit*)
                                    (typep (getf fields :status) 'agenda-status)
                                    (agenda--memory-identifiers-p
                                     (getf fields :memory-ids))
                                    (member (getf fields :memory-ids-present-p)
                                            '(nil t))
                                    (agenda--item-form-p result 3))))))))
           ledger))
     (error nil nil))))

(serapeum/types:-> agenda-call-with-transaction (t agenda-state function) t)
(defun agenda-call-with-transaction (context state function)
  "Run FUNCTION against fresh private state under the process-shared store lock. Validate guarded snapshots and apply all changes inside FUNCTION. Nested agenda writes use this same transaction. Publish STATE only after commit."
  (agenda--call-with-transaction context state function))
