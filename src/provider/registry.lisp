(in-package #:autolith)

;;;; -- Provider Model Metadata --

(defclass provider-model ()
  ((name
    :initarg :name
    :reader provider-model-name
    :type non-empty-string
    :documentation "The model identifier accepted by a provider.")
   (description
    :initarg :description
    :initform ""
    :reader provider-model-description
    :type string
    :documentation "The optional user-visible model description.")
   (context-window
    :initarg :context-window
    :initform *default-context-window*
    :reader provider-model-context-window
    :type (integer 1)
    :documentation "The model context window in tokens.")
   (reasoning-efforts
    :initarg :reasoning-efforts
    :initform *supported-reasoning-efforts*
    :reader provider-model-reasoning-efforts
    :type list
    :documentation "The reasoning efforts offered for this model."))
  (:documentation "Metadata describing one model exposed by a registered provider."))

(defclass provider-registration ()
  ((name
    :initarg :name
    :reader provider-registration-name
    :type non-empty-string
    :documentation "The stable user-visible provider name.")
   (description
    :initarg :description
    :reader provider-registration-description
    :type string
    :documentation "The user-visible provider description.")
   (family
    :initarg :family
    :reader provider-registration-family
    :type keyword
    :documentation "The conversation family keyword used for private item filtering.")
   (models
    :initarg :models
    :reader provider-registration-models
    :type list
    :documentation "The ordered effective model metadata exposed by this provider.")
   (declared-models
    :initarg :declared-models
    :reader provider-registration-declared-models
    :type list
    :documentation "The static model metadata declared by this provider.")
   (discovered-models
    :initarg :discovered-models
    :initform nil
    :reader provider-registration-discovered-models
    :type list
    :documentation "The last successful dynamic model metadata for this provider.")
   (model-discovery
    :initarg :model-discovery
    :initform nil
    :reader provider-registration-model-discovery
    :type (option function)
    :documentation "The function that discovers current model identifiers.")
   (model-discovery-endpoint
    :initarg :model-discovery-endpoint
    :initform nil
    :reader provider-registration-model-discovery-endpoint
    :type (option string)
    :documentation "The endpoint used for model discovery, when declared.")
   (model-discovery-endpoint-resolver
    :initarg :model-discovery-endpoint-resolver
    :initform nil
    :reader provider-registration-model-discovery-endpoint-resolver
    :type (option function)
    :documentation
    "The optional zero-argument function returning the current discovery endpoint.")
   (model-discovery-lock
    :initform (make-recursive-lock "Provider model discovery")
    :reader provider-registration-model-discovery-lock
    :type t
    :documentation "The lock serializing discovery requests for this provider.")
   (factory
    :initarg :factory
    :reader provider-registration-factory
    :type function
    :documentation "The function creating a provider for one configuration.")
   (authenticator
    :initarg :authenticator
    :initform nil
    :reader provider-registration-authenticator
    :type (option function)
    :documentation "The optional function implementing provider authentication.")
   (protocol
    :initarg :protocol
    :initform ':custom
    :reader provider-registration-protocol
    :type keyword
    :documentation "The wire protocol label shown in provider diagnostics.")
   (endpoint
    :initarg :endpoint
    :initform nil
    :reader provider-registration-endpoint
    :type (option string)
    :documentation "The provider endpoint, when one is declared by metadata.")
   (source
    :initarg :source
    :reader provider-registration-source
    :type keyword
    :documentation "The extension layer that supplied this registration.")
   (sequence
    :initarg :sequence
    :reader provider-registration-sequence
    :type (integer 0)
    :documentation "The monotonic registration order within one image."))
  (:documentation "One provider registration layer and its model metadata."))

(defvar *provider-registrations* nil
  "All provider registrations, including shadowed lower-precedence layers.")

(defvar *provider-registration-sequence* 0
  "The next monotonic provider registration sequence number.")

(defvar *provider-registry-lock*
  (make-recursive-lock "Autolith provider registry")
  "The recursive lock protecting provider registration layers.")

(defparameter *provider-registration-sources*
  '(:builtin :site :user :runtime)
  "The provider registration sources ordered from lowest to highest precedence.")

(defparameter *provider-model-cache-version* 2
  "The portable version of the successful provider model cache.")

(defvar *provider-model-cache-lock*
  (make-lock "Autolith provider model cache")
  "The lock protecting provider model cache read-modify-write operations.")


;;;; -- Provider Registration --

(-> provider--registration-key (string) string)
(defun provider--registration-key (name)
  "Return the case-insensitive registry key for provider NAME."
  (string-downcase name))

(-> provider--source-rank (keyword) integer)
(defun provider--source-rank (source)
  "Return the precedence rank of provider registration SOURCE."
  (or (position source *provider-registration-sources*)
      (error 'configuration-error
             :message (format nil "Unknown provider registration source ~S." source))))

(-> provider--current-registration-source () keyword)
(defun provider--current-registration-source ()
  "Return the registration source appropriate to the current load context."
  (if (boundp '*extension-registration-source*)
      (symbol-value '*extension-registration-source*)
      ':runtime))

(-> provider--family-keyword (string) keyword)
(defun provider--family-keyword (name)
  "Derive a stable family keyword from provider NAME."
  (intern
   (string-upcase
    (with-output-to-string (stream)
      (loop for character across name
            do (write-char (if (alphanumericp character) character #\-) stream))))
   '#:keyword))

(-> provider-model-create (t) provider-model)
(defun provider-model-create (spec)
  "Normalize one model SPEC into provider metadata.

SPEC may be a model string, an existing PROVIDER-MODEL, or a property list with
:NAME, :DESCRIPTION, :CONTEXT-WINDOW, and :REASONING-EFFORTS keys."
  (etypecase spec
    (provider-model
     spec)
    (string
     (unless (non-empty-string-p spec)
       (error 'configuration-error
              :message "Provider model names must not be empty."))
     (make-instance 'provider-model :name spec))
    (cons
     (let ((name (getf spec ':name)))
       (unless (non-empty-string-p name)
         (error 'configuration-error
                :message (format nil "Provider model metadata needs a nonempty :name: ~S."
                                 spec)))
       (let ((context-window (getf spec ':context-window
                                   *default-context-window*))
             (reasoning-efforts (getf spec ':reasoning-efforts
                                      *supported-reasoning-efforts*)))
         (unless (and (integerp context-window) (plusp context-window))
           (error 'configuration-error
                  :message (format nil
                                   "Provider model ~A needs a positive :context-window."
                                   name)))
         (unless (and (listp reasoning-efforts)
                      reasoning-efforts
                      (every #'non-empty-string-p reasoning-efforts))
           (error 'configuration-error
                  :message (format nil
                                   "Provider model ~A has invalid :reasoning-efforts."
                                   name)))
         (make-instance 'provider-model
                        :name name
                        :description (or (getf spec ':description) "")
                        :context-window context-window
                        :reasoning-efforts (copy-list reasoning-efforts)))))))

(-> provider--normalize-models (list &key (:allow-empty-p boolean)) list)
(defun provider--normalize-models (models &key allow-empty-p)
  "Normalize and validate a provider's ordered MODELS."
  (unless (and (listp models) (or models allow-empty-p))
    (error 'configuration-error
           :message "A provider registration needs at least one model."))
  (let ((seen (make-hash-table :test #'equal))
        (normalized nil))
    (dolist (spec models (nreverse normalized))
      (let* ((model (provider-model-create spec))
             (name (provider-model-name model)))
        (when (gethash name seen)
          (error 'configuration-error
                 :message (format nil
                                  "Provider registration repeats model ~A."
                                  name)))
        (setf (gethash name seen) t)
        (push model normalized)))))

(-> provider--model-cache-form (provider-model) list)
(defun provider--model-cache-form (model)
  "Serialize MODEL into the private provider model cache form."
  (list :name (provider-model-name model)
        :description (provider-model-description model)
        :context-window (provider-model-context-window model)
        :reasoning-efforts (copy-list (provider-model-reasoning-efforts model))))

(-> provider--cache-entry-form (list) list)
(defun provider--cache-entry-form (entry)
  "Serialize one normalized provider model cache ENTRY."
  (list :provider-name (getf entry :provider-name)
        :discovery-endpoint (getf entry :discovery-endpoint)
        :models (mapcar #'provider--model-cache-form
                        (getf entry :models))))

(-> provider--read-model-cache (pathname) list)
(defun provider--read-model-cache (pathname)
  "Read valid provider model cache entries from PATHNAME, or return NIL."
  (if (not (probe-file pathname))
      nil
      (handler-case
          (let* ((record (read-portable-form pathname))
                 (version (and (listp record)
                               (getf (rest record) :version)))
                 (entries (and (listp record)
                               (eq (first record) :provider-model-cache)
                               (getf (rest record) :providers))))
            (unless (and (listp record)
                         (eq (first record) :provider-model-cache)
                         (= version *provider-model-cache-version*)
                         (listp entries))
              (error "Invalid provider model cache."))
            (remove nil
                    (mapcar
                     (lambda (entry)
                       (let ((name (and (listp entry)
                                        (getf entry :provider-name)))
                             (endpoint (and (listp entry)
                                            (getf entry :discovery-endpoint)))
                             (models (and (listp entry)
                                          (getf entry :models))))
                         (when (and (non-empty-string-p name)
                                    (or (null endpoint)
                                        (non-empty-string-p endpoint))
                                    (listp models))
                           (list :provider-name name
                                 :discovery-endpoint endpoint
                                 :models
                                 (provider--normalize-models
                                  models :allow-empty-p t)))))
                     entries)))
        (error () nil))))

(-> provider--effective-model-discovery-endpoint
    (provider-registration)
    (option string))
(defun provider--effective-model-discovery-endpoint (registration)
  "Return REGISTRATION's current model-discovery cache identity."
  (let ((resolver
          (provider-registration-model-discovery-endpoint-resolver registration)))
    (if resolver
        (let ((endpoint (funcall resolver)))
          (unless (non-empty-string-p endpoint)
            (error 'configuration-error
                   :message
                   (format nil
                           "Provider ~A resolved an invalid model discovery endpoint."
                           (provider-registration-name registration))))
          endpoint)
        (provider-registration-model-discovery-endpoint registration))))

(-> provider--cache-entry-for (provider-registration list) (option list))
(defun provider--cache-entry-for (registration entries)
  "Find the cache ENTRY matching REGISTRATION's discovery identity."
  (let ((discovery-endpoint
          (provider--effective-model-discovery-endpoint registration)))
    (find-if
     (lambda (entry)
       (and (string= (provider--registration-key
                      (provider-registration-name registration))
                     (provider--registration-key
                      (getf entry :provider-name)))
            (equal discovery-endpoint
                   (getf entry :discovery-endpoint))))
     entries)))

(-> provider--write-model-cache
    (configuration provider-registration list)
    null)
(defun provider--write-model-cache (configuration registration models)
  "Best-effort atomically update REGISTRATION's cached MODELS."
  (handler-case
      (with-lock-held (*provider-model-cache-lock*)
        (let* ((pathname (configuration-provider-model-cache-path configuration))
               (entries (provider--read-model-cache pathname))
               (provider-name (provider-registration-name registration))
               (discovery-endpoint
                 (provider--effective-model-discovery-endpoint registration))
               (updated-entry
                 (list :provider-name provider-name
                       :discovery-endpoint discovery-endpoint
                       :models (copy-list models)))
               (updated-entries
                 (cons updated-entry
                       (remove-if
                        (lambda (entry)
                          (and (string= (provider--registration-key provider-name)
                                        (provider--registration-key
                                         (getf entry :provider-name)))
                               (equal discovery-endpoint
                                      (getf entry :discovery-endpoint))))
                        entries))))
          (ensure-directories-exist pathname)
          (snapshot-write
           pathname
           (list :provider-model-cache
                 :version *provider-model-cache-version*
                 :providers (mapcar #'provider--cache-entry-form updated-entries))
           :mode #o600)))
    (error () nil))
  nil)

(-> register-provider
    (string &key
            (:description (option string))
            (:family (option keyword))
            (:models (option list))
            (:factory function)
            (:authenticator (option function))
            (:protocol keyword)
            (:endpoint (option string))
            (:model-discovery (option function))
            (:model-discovery-endpoint (option string))
            (:model-discovery-endpoint-resolver (option function))
            (:source keyword))
    string)
(defun register-provider
    (name &key description family models factory authenticator
      (protocol ':custom) endpoint model-discovery model-discovery-endpoint
      model-discovery-endpoint-resolver
      (source (provider--current-registration-source)))
  "Register a provider and its model metadata.

FACTORY receives CONFIGURATION and the keyword REASONING-SUMMARIES-P and must
return a MODEL-PROVIDER. MODEL-DISCOVERY, when supplied, receives CONFIGURATION
and returns model strings or metadata property lists. The optional endpoint
resolver returns the current model-discovery cache identity. AUTHENTICATOR, when
supplied, receives the provider and the keyword arguments STREAM and
OPEN-BROWSER-P. Site, user, and live runtime registrations replace only the
same source and shadow lower-precedence registrations with the same name."
  (unless (non-empty-string-p name)
    (error 'configuration-error
           :message "Provider names must not be empty."))
  (unless (functionp factory)
    (error 'configuration-error
           :message (format nil "Provider ~A needs a callable :factory." name)))
  (when (and model-discovery (not (functionp model-discovery)))
    (error 'configuration-error
           :message
           (format nil
                   "Provider ~A has a non-callable :model-discovery."
                   name)))
  (when (and model-discovery-endpoint-resolver
             (not (functionp model-discovery-endpoint-resolver)))
    (error 'configuration-error
           :message
           (format nil
                   "Provider ~A has a non-callable model discovery endpoint resolver."
                   name)))
  (unless (or model-discovery (and (listp models) models))
    (error 'configuration-error
           :message
           (format nil
                   "Provider ~A needs :models or a callable :model-discovery."
                   name)))
  (unless (member source *provider-registration-sources* :test #'eq)
    (error 'configuration-error
           :message (format nil "Unknown provider registration source ~S." source)))
  (when (and endpoint (not (non-empty-string-p endpoint)))
    (error 'configuration-error
           :message (format nil "Provider ~A has an invalid endpoint." name)))
  (when (and model-discovery-endpoint
             (not (non-empty-string-p model-discovery-endpoint)))
    (error 'configuration-error
           :message
           (format nil "Provider ~A has an invalid model discovery endpoint."
                   name)))
  (let* ((declared-models
           (provider--normalize-models
            models
            :allow-empty-p (functionp model-discovery)))
         (previous-registration
           (with-recursive-lock-held (*provider-registry-lock*)
             (find-if
              (lambda (candidate)
                (and (eq (provider-registration-source candidate) source)
                     (string= (provider--registration-key
                               (provider-registration-name candidate))
                              (provider--registration-key name))))
              *provider-registrations*)))
         (retained-discovered-models
           (if (and model-discovery
                    previous-registration
                    (null model-discovery-endpoint-resolver)
                    (null
                     (provider-registration-model-discovery-endpoint-resolver
                      previous-registration))
                    (equal endpoint
                           (provider-registration-endpoint previous-registration))
                    (equal model-discovery-endpoint
                           (provider-registration-model-discovery-endpoint
                            previous-registration)))
               (copy-list
                (provider-registration-discovered-models previous-registration))
               nil))
         (registration
           (make-instance
            'provider-registration
            :name name
            :description (or description name)
            :family (or family (provider--family-keyword name))
            :models (provider--merge-models declared-models
                                            retained-discovered-models)
            :declared-models declared-models
            :discovered-models retained-discovered-models
            :model-discovery model-discovery
            :model-discovery-endpoint model-discovery-endpoint
            :model-discovery-endpoint-resolver
            model-discovery-endpoint-resolver
            :factory factory
            :authenticator authenticator
            :protocol protocol
            :endpoint endpoint
            :source source
            :sequence 0)))
    (with-recursive-lock-held (*provider-registry-lock*)
      (setf (slot-value registration 'sequence)
            (incf *provider-registration-sequence*)
            *provider-registrations*
            (append
             (remove-if
              (lambda (candidate)
                (and (string= (provider--registration-key
                               (provider-registration-name candidate))
                              (provider--registration-key name))
                     (eq (provider-registration-source candidate) source)))
              *provider-registrations*)
             (list registration)))
      (provider--refresh-model-settings))
    name))

(-> unregister-provider (string &key (:source (option keyword))) boolean)
(defun unregister-provider (name &key (source (provider--current-registration-source)))
  "Remove NAME from one provider registration SOURCE layer."
  (unless (member source *provider-registration-sources* :test #'eq)
    (error 'configuration-error
           :message (format nil "Unknown provider registration source ~S." source)))
  (with-recursive-lock-held (*provider-registry-lock*)
    (let ((before (length *provider-registrations*)))
      (setf *provider-registrations*
            (remove-if
             (lambda (candidate)
               (and (eq (provider-registration-source candidate) source)
                    (string= (provider--registration-key
                              (provider-registration-name candidate))
                             (provider--registration-key name))))
             *provider-registrations*))
      (provider--refresh-model-settings)
      (< (length *provider-registrations*) before))))


(-> provider--merge-models (list list) list)
(defun provider--merge-models (declared-models discovered-models)
  "Merge discovered models behind declared metadata overrides."
  (let ((seen (make-hash-table :test #'equal))
        (merged nil))
    (dolist (model declared-models)
      (let ((name (provider-model-name model)))
        (setf (gethash name seen) t)
        (push model merged)))
    (dolist (model (provider--normalize-models
                    discovered-models
                    :allow-empty-p t))
      (let ((name (provider-model-name model)))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (push model merged))))
    (nreverse merged)))

(-> provider-load-model-cache (configuration) null)
(defun provider-load-model-cache (configuration)
  "Load successful dynamic model metadata from CONFIGURATION's private cache."
  (let ((entries
          (with-lock-held (*provider-model-cache-lock*)
            (provider--read-model-cache
             (configuration-provider-model-cache-path configuration)))))
    (when entries
      (with-recursive-lock-held (*provider-registry-lock*)
        (dolist (registration *provider-registrations*)
          (when (provider-registration-model-discovery registration)
            (let ((entry (provider--cache-entry-for registration entries)))
              (when entry
                (let ((discovered-models (copy-list (getf entry :models))))
                  (setf (slot-value registration 'discovered-models)
                        discovered-models
                        (slot-value registration 'models)
                        (provider--merge-models
                         (provider-registration-declared-models registration)
                         discovered-models)))))))
        (provider--refresh-model-settings)))
  nil))

(-> provider--refresh-registration-models
    (provider-registration configuration)
    list)
(defun provider--refresh-registration-models (registration configuration)
  "Refresh REGISTRATION's dynamic models for CONFIGURATION."
  (with-recursive-lock-held
      ((provider-registration-model-discovery-lock registration))
    (let* ((discovery (provider-registration-model-discovery registration))
           (discovered-models
             (provider--normalize-models
              (funcall discovery configuration)
              :allow-empty-p t))
           (models (provider--merge-models
                    (provider-registration-declared-models registration)
                    discovered-models))
           (published-p nil))
      (with-recursive-lock-held (*provider-registry-lock*)
        (when (find registration *provider-registrations* :test #'eq)
          (setf (slot-value registration 'discovered-models) discovered-models
                (slot-value registration 'models) models
                published-p t)
          (provider--refresh-model-settings)))
      (when published-p
        (provider--write-model-cache configuration registration discovered-models))
      models)))

(-> provider-refresh-models
    (configuration &key (:provider-name (option string)))
    list)
(defun provider-refresh-models (configuration &key provider-name)
  "Refresh dynamic provider model lists and return discovery failures.

When PROVIDER-NAME is supplied, refresh only that effective registration. Static
registrations are ignored. Failures retain the last successful model list."
  (let* ((registrations
           (if provider-name
               (list
                (or (provider-registration-find provider-name)
                    (error 'configuration-error
                           :message
                           (format nil "Unknown provider ~A."
                                   provider-name))))
               (provider-registrations)))
         (failures nil))
    (dolist (registration registrations (nreverse failures))
      (let ((discovery (provider-registration-model-discovery registration)))
        (when discovery
          (handler-case
              (provider--refresh-registration-models registration configuration)
            (serious-condition (cause)
              (push
               (make-condition
                'provider-model-discovery-error
                :message
                (format nil
                        "Could not discover models for provider ~A."
                        (provider-registration-name registration))
                :provider-name (provider-registration-name registration)
                :cause cause)
               failures))))))))

(-> provider-bootstrap-configuration
    (configuration &key (:refresh-p boolean))
    configuration)
(defun provider-bootstrap-configuration (configuration &key refresh-p)
  "Load local provider metadata and validate CONFIGURATION.

When REFRESH-P is true, also perform the explicit synchronous discovery operation.
The default startup path never performs remote model discovery."
  (provider-load-model-cache configuration)
  (when refresh-p
    (provider-refresh-models configuration))
  (let ((model (configuration-model configuration)))
    (configuration-with-model
     configuration
     (if (configuration--model-supported-p model)
         model
         *default-model*))))

(defparameter *provider-name-aliases*
  '(("codex" . "chatgpt")
    ("openai" . "chatgpt"))
  "Legacy provider names mapped to registered provider names.")

(-> provider--canonical-name (string) string)
(defun provider--canonical-name (name)
  "Return NAME normalized for provider registry lookup."
  (or (cdr (assoc (string-downcase name) *provider-name-aliases* :test #'string=))
      (string-downcase name)))


;;;; -- Effective Registry Views --

(-> provider--registration-precedence< (provider-registration provider-registration)
    boolean)
(defun provider--registration-precedence< (left right)
  "Return true when LEFT has higher precedence than RIGHT."
  (let ((left-rank (provider--source-rank (provider-registration-source left)))
        (right-rank (provider--source-rank (provider-registration-source right))))
    (or (> left-rank right-rank)
        (and (= left-rank right-rank)
             (> (provider-registration-sequence left)
                (provider-registration-sequence right))))))

(-> provider--precedence-registrations () list)
(defun provider--precedence-registrations ()
  "Return registrations ordered from highest to lowest precedence."
  (sort (copy-list *provider-registrations*)
        #'provider--registration-precedence<))

(-> provider--effective-registration-for-name (string) (option provider-registration))
(defun provider--effective-registration-for-name (name)
  "Return the highest-precedence registration named NAME."
  (find-if
   (lambda (registration)
     (string= (provider--registration-key (provider-registration-name registration))
              (provider--registration-key name)))
   (provider--precedence-registrations)))

(-> provider-registrations () list)
(defun provider-registrations ()
  "Return the effective provider registrations in stable display order."
  (with-recursive-lock-held (*provider-registry-lock*)
    (let ((seen (make-hash-table :test #'equal))
          (effective nil))
      (dolist (registration
               (sort (copy-list *provider-registrations*) #'<
                     :key #'provider-registration-sequence))
        (let ((key (provider--registration-key
                    (provider-registration-name registration))))
          (unless (gethash key seen)
            (let ((winner (provider--effective-registration-for-name key)))
              (setf (gethash key seen) t)
              (when winner
                (push winner effective))))))
      (nreverse effective))))

(-> provider-registration-find (string) (option provider-registration))
(defun provider-registration-find (name)
  "Return the effective provider registration named NAME."
  (unless (non-empty-string-p name)
    (return-from provider-registration-find nil))
  (with-recursive-lock-held (*provider-registry-lock*)
    (provider--effective-registration-for-name name)))

(-> provider-registration-for-model (string) (option provider-registration))
(defun provider-registration-for-model (model)
  "Return the highest-precedence effective provider serving MODEL.

When more than one provider claims MODEL, the registration source precedence and
then newest registration order decide which provider is effective."
  (unless (non-empty-string-p model)
    (return-from provider-registration-for-model nil))
  (with-recursive-lock-held (*provider-registry-lock*)
    (find-if
     (lambda (registration)
       (some (lambda (candidate)
               (string= (provider-model-name candidate) model))
             (provider-registration-models registration)))
     (sort (copy-list (provider-registrations))
           #'provider--registration-precedence<))))

(-> provider-model-for (string) (option provider-model))
(defun provider-model-for (model)
  "Return the effective model metadata for MODEL."
  (let ((registration (provider-registration-for-model model)))
    (and registration
         (find model
               (provider-registration-models registration)
               :key #'provider-model-name
               :test #'string=))))

(-> provider-model-identifiers () list)
(defun provider-model-identifiers ()
  "Return unique model identifiers exposed by effective providers."
  (with-recursive-lock-held (*provider-registry-lock*)
    (let ((seen (make-hash-table :test #'equal))
          (models nil))
      (dolist (registration (provider-registrations))
        (dolist (model (provider-registration-models registration))
          (let ((name (provider-model-name model)))
            (when (and (not (gethash name seen))
                       (eq registration (provider-registration-for-model name)))
              (setf (gethash name seen) t)
              (push name models)))))
      (nreverse models))))

(-> provider-model-family (string) (option keyword))
(defun provider-model-family (model)
  "Return the registered family serving MODEL, or NIL when unknown."
  (let ((registration (provider-registration-for-model model)))
    (and registration (provider-registration-family registration))))

(-> provider-model-endpoint (string) (option string))
(defun provider-model-endpoint (model)
  "Return the registered endpoint serving MODEL, when metadata declares one."
  (let ((registration (provider-registration-for-model model)))
    (and registration (provider-registration-endpoint registration))))

(-> provider-model-context-window-for (string) (option integer))
(defun provider-model-context-window-for (model)
  "Return the registered context window for MODEL, when metadata declares one."
  (let ((metadata (provider-model-for model)))
    (and metadata (provider-model-context-window metadata))))

(-> provider-model-reasoning-efforts-for (string) (option list))
(defun provider-model-reasoning-efforts-for (model)
  "Return the reasoning efforts declared for MODEL, when available."
  (let ((metadata (provider-model-for model)))
    (and metadata (copy-list (provider-model-reasoning-efforts metadata)))))

(-> provider-model-provider-name (string) (option string))
(defun provider-model-provider-name (model)
  "Return the display name of the provider serving MODEL, when known."
  (let ((registration (provider-registration-for-model model)))
    (and registration (provider-registration-name registration))))


;;;; -- Registry State --

(-> provider--refresh-model-settings () null)
(defun provider--refresh-model-settings ()
  "Synchronize legacy model tables with the effective provider registry."
  (setf *supported-models* (provider-model-identifiers)
        *model-context-windows*
        (loop for model in (provider-model-identifiers)
              for window = (provider-model-context-window-for model)
              when window collect (cons model window)))
  nil)

(-> provider--remove-registration-source (keyword) null)
(defun provider--remove-registration-source (source)
  "Remove all provider registrations supplied by SOURCE."
  (with-recursive-lock-held (*provider-registry-lock*)
    (setf *provider-registrations*
          (remove source *provider-registrations*
                  :test #'eq
                  :key #'provider-registration-source))
    (provider--refresh-model-settings))
  nil)

(-> provider--snapshot-discovered-models
    (provider-registration list)
    list)
(defun provider--snapshot-discovered-models (registration model-snapshot)
  "Return discovered models from current or legacy MODEL-SNAPSHOT state."
  (if (rest (rest model-snapshot))
      (copy-list (third model-snapshot))
      (let ((declared-models
              (provider-registration-declared-models registration)))
        (loop for model in (second model-snapshot)
              unless (find (provider-model-name model)
                           declared-models
                           :key #'provider-model-name
                           :test #'string=)
                collect model))))

(-> provider--registry-snapshot () list)
(defun provider--registry-snapshot ()
  "Return an exact snapshot of provider registration layers and model state."
  (with-recursive-lock-held (*provider-registry-lock*)
    (list :registrations (copy-list *provider-registrations*)
          :models
          (loop for registration in *provider-registrations*
                collect
                (list registration
                      (copy-list
                       (provider-registration-models registration))
                      (copy-list
                       (provider-registration-discovered-models registration))))
          :sequence *provider-registration-sequence*)))

(-> provider--registry-restore (list) null)
(defun provider--registry-restore (snapshot)
  "Restore provider registration layers and model state from SNAPSHOT."
  (with-recursive-lock-held (*provider-registry-lock*)
    (dolist (model-snapshot (getf snapshot ':models))
      (let ((registration (first model-snapshot)))
        (setf (slot-value registration 'models)
              (copy-list (second model-snapshot))
              (slot-value registration 'discovered-models)
              (provider--snapshot-discovered-models
               registration model-snapshot))))
    (setf *provider-registrations* (copy-list (getf snapshot ':registrations))
          *provider-registration-sequence* (getf snapshot ':sequence))
    (provider--refresh-model-settings))
  nil)
