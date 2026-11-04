(in-package #:autolith)

;;;; -- Provider Registry --

;;; Provider registrations, their source layers, model discovery and the model
;;; cache live in cl-llm-provider-api/registry. Autolith owns one registry, the
;;; extension source a load binds, the private cache file, the configuration
;;; condition protocol, and the legacy model tables kept in step with it.

(defparameter *provider-registration-sources*
  '(:builtin :site :user :runtime)
  "The provider registration sources ordered from lowest to highest precedence.")

(defvar *provider-registry*
  (provider-registry-create
   :sources                   *provider-registration-sources*
   :default-context-window    *default-context-window*
   :default-reasoning-efforts *supported-reasoning-efforts*
   :cache-read-function       (lambda (configuration)
                                (provider--read-model-cache configuration))
   :cache-write-function      (lambda (configuration form)
                                (provider--write-model-cache configuration form))
   :change-function           (lambda (registry)
                                (declare (ignore registry))
                                (provider--refresh-model-settings)))
  "Every provider registration layer of this image and its model metadata.")

(setf *provider-registry-error-class* 'provider-registry-configuration-error)

(defparameter *provider-name-aliases*
  '(("codex" . "chatgpt")
    ("openai" . "chatgpt"))
  "Legacy provider names mapped to registered provider names.")


;;;; -- Registration --

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
  (provider-registry-register
   *provider-registry* name
   :description                       description
   :family                            family
   :models                            models
   :factory                           factory
   :authenticator                     authenticator
   :protocol                          protocol
   :endpoint                          endpoint
   :model-discovery                   model-discovery
   :model-discovery-endpoint          model-discovery-endpoint
   :model-discovery-endpoint-resolver model-discovery-endpoint-resolver
   :source                            source))

(-> unregister-provider (string &key (:source (option keyword))) boolean)
(defun unregister-provider (name &key (source (provider--current-registration-source)))
  "Remove NAME from one provider registration SOURCE layer."
  (provider-registry-unregister *provider-registry* name :source source))

(-> provider--current-registration-source () keyword)
(defun provider--current-registration-source ()
  "Return the registration source appropriate to the current load context."
  (if (boundp '*extension-registration-source*)
      (symbol-value '*extension-registration-source*)
      ':runtime))

(-> provider--canonical-name (string) string)
(defun provider--canonical-name (name)
  "Return NAME normalized for provider registry lookup."
  (or (rest (assoc (string-downcase name) *provider-name-aliases* :test #'string=))
      (string-downcase name)))


;;;; -- Discovery and Cache --

(-> provider-load-model-cache (configuration) null)
(defun provider-load-model-cache (configuration)
  "Load successful dynamic model metadata from CONFIGURATION's private cache."
  ;; The catalog structure is defined after this file, so its reader is not inlined.
  (declare (notinline model-catalog-providers))
  (provider-registry-load-model-cache *provider-registry* configuration)
  (let ((catalog (provider-catalog-load configuration)))
    (when (model-catalog-providers catalog)
      (dolist (registration (provider-registrations))
        ; mallet:suppress no-ignore-errors -- cached metadata that fails to install keeps the built-in fallback
        (ignore-errors (provider-catalog--install catalog registration)))))
  nil)

(-> provider-refresh-models
    (configuration &key (:provider-name (option string)))
    list)
(defun provider-refresh-models (configuration &key provider-name)
  "Refresh models.dev metadata and account-specific discovery; return failures.

When PROVIDER-NAME is supplied, refresh only that effective registration. Static
registrations use models.dev when mapped. Failures retain the last successful list."
  (let* ((registrations (if provider-name
                            (list (or (provider-registration-find provider-name)
                                      (error 'configuration-error :message "Unknown provider.")))
                            (provider-registrations)))
         (catalog       (provider-catalog-load configuration))
         (failures      nil))
    (when (some #'provider-catalog--source registrations)
      (provider-catalog-refresh catalog))
    (dolist (registration registrations)
      (if (provider-catalog--source registration)
          (handler-case (provider-catalog--install catalog registration)
            (error (cause)
              (push (make-condition 'provider-model-discovery-error
                                    :message       "Could not load provider models from models.dev."
                                    :provider-name (provider-registration-name registration)
                                    :cause         cause)
                    failures)))
          (setf failures
                (append
                 (reverse
                  (mapcar
                   (lambda (failure)
                     (make-condition
                      'provider-model-discovery-error
                      :message (cl-llm-provider-api:provider-api-error-message failure)
                      :provider-name
                      (cl-llm-provider-api:provider-model-discovery-error-provider-name failure)
                      :cause   (cl-llm-provider-api:provider-model-discovery-error-cause failure)))
                   (provider-registry-refresh-models
                    *provider-registry* configuration
                    :provider-name (provider-registration-name registration))))
                 failures))))
    (nreverse failures)))

(-> provider-bootstrap-configuration
    (configuration &key (:refresh-p boolean))
    configuration)
(defun provider-bootstrap-configuration (configuration &key refresh-p)
  "Load local provider metadata and validate CONFIGURATION.

When REFRESH-P is true, also perform the explicit synchronous discovery operation.
The default startup path never performs remote model discovery. Launch overrides
are normalized after executable init and cache loading establish their provider."
  (provider-load-model-cache configuration)
  (when refresh-p
    (provider-refresh-models configuration))
  (when (config :model-selection-override-p configuration)
    (setf configuration
          (configuration-copy configuration
                              :model                 (config :model configuration)
                              :model-provider        (config :model-provider configuration)
                              :provider-validation-p nil)))
  (let ((saved-model     (config :model configuration))
        (saved-provider  (config :model-provider configuration))
        (model-source    (configuration-setting-source configuration :model))
        (provider-source (configuration-setting-source configuration :model-provider)))
    ;; Reconcile the whole saved pair before dropping a model to its default:
    ;; a registered saved provider need not serve that default either.
    (when (and (eq provider-source ':durable)
               (member model-source '(nil :durable))
               (non-empty-string-p saved-provider)
               (configuration--model-problem saved-model configuration))
      (configuration-unset configuration :model-provider))
    (configuration-validate-deferred configuration)
    (when (or (and (eq model-source ':durable)
                   (not (equal saved-model (config :model configuration))))
              (and (eq provider-source ':durable)
                   (not (equal saved-provider (config :model-provider configuration)))))
      (setf (config :saved-model-selection-notice configuration)
            (format nil "Saved model selection ~A~A is no longer available; using ~A."
                    (if (non-empty-string-p saved-provider)
                        (concatenate 'string saved-provider "/") "")
                    saved-model (config :model configuration))))
    configuration))

(-> provider--model-cache-store (configuration) sexp-store:snapshot-store)
(defun provider--model-cache-store (configuration)
  "Return the transactional cache store shared by refresh and upgrade publication."
  (let ((pathname (configuration-provider-model-cache-path configuration)))
    (make-instance 'sexp-store:snapshot-store
                   :pathname           pathname
                   :lock-pathname      (make-pathname :type "lock" :defaults pathname)
                   :initial-state      #'list
                   :validator          #'listp
                   :decoder            #'identity
                   :encoder            #'identity
                   :recover-read-error (constantly nil))))

(-> provider--read-model-cache (configuration) t)
(defun provider--read-model-cache (configuration)
  "Move legacy metadata once, then read only the cache-root copy.

Remove the source under both writer locks only after atomic publication. An
existing target wins, including when retrying interrupted source cleanup.
Failed publication uses legacy metadata offline. Remove the branch at 0.68.0."
  (block nil
    (let ((target (configuration-provider-model-cache-path configuration))
          (legacy (merge-pathnames "provider-models.sexp" (config :state-root configuration))))
      (flet ((target-present-p ()
               (handler-case (probe-file target)
                 (file-error () nil))))
        (when (and (probe-file legacy)
                   (not (or (uiop:pathname-equal target legacy)
                            (handler-case
                                (and (target-present-p)
                                     (uiop:pathname-equal (platform-truename *platform* target)
                                                          (platform-truename *platform* legacy)))
                              (file-error () nil)))))
          (handler-case
              (progn
                (configuration-ensure-root-directory configuration ':cache-root)
                (call-with-file-lock
                 (make-pathname :type "lock" :defaults legacy)
                 (lambda ()
                   (when (probe-file legacy)
                     (sexp-store:store-transact
                      (provider--model-cache-store configuration)
                      (lambda (current)
                        (if (target-present-p)
                            (values current nil nil)
                            (let ((form (read-portable-form legacy)))
                              (values form nil t))))
                      :publish (lambda (committed)
                                 (declare (ignore committed))
                                 (delete-file legacy)))))))
            (error (cause)
              (warn 'provider-model-cache-migration-warning :source legacy :target target :cause cause)
              (unless (target-present-p)
                (return
                  (handler-case (read-portable-form legacy)
                    ((or sexp-store:store-error file-error) (condition)
                      ;; Publication may have consumed the source before it opened.
                      (cond
                        ((target-present-p)
                         (read-portable-form target))
                        ((probe-file legacy)
                         (error condition))
                        (t
                         nil)))))))))
        (when (target-present-p)
          (read-portable-form target))))))

(-> provider--write-model-cache (configuration list) null)
(defun provider--write-model-cache (configuration form)
  "Atomically replace CONFIGURATION's private model cache file with FORM."
  (configuration-ensure-root-directory configuration ':cache-root)
  (sexp-store:store-transact
   (provider--model-cache-store configuration)
   (lambda (current)
     (declare (ignore current))
     (values form nil t)))
  nil)

(define-condition provider-model-cache-migration-warning (warning)
  ((source :initarg       :source
           :reader        provider-model-cache-migration-warning-source
           :documentation "The legacy model cache pathname to remove after publication.")
   (target :initarg       :target
           :reader        provider-model-cache-migration-warning-target
           :documentation "The desired XDG cache pathname.")
   (cause  :initarg       :cause
           :reader        provider-model-cache-migration-warning-cause
           :documentation "The condition that prevented publication or source cleanup."))
  (:documentation "Provider cache migration is incomplete and will be retried.")
  (:report (lambda (condition stream)
             (format stream "Could not finish migrating model cache from ~A to ~A: ~A. Migration will be retried."
                     (provider-model-cache-migration-warning-source condition)
                     (provider-model-cache-migration-warning-target condition)
                     (provider-model-cache-migration-warning-cause condition)))))


;;;; -- Effective Registry Views --

(-> provider-registrations () list)
(defun provider-registrations ()
  "Return the effective provider registrations in stable display order."
  (provider-registry-registrations *provider-registry*))

(-> provider-registration-find (string) (option provider-registration))
(defun provider-registration-find (name)
  "Return the effective provider registration named NAME."
  (provider-registry-find *provider-registry* name))

(-> provider--selection-error
    (string (option string) &key (:reason keyword) (:message string)) nil)
(defun provider--selection-error (model provider &key reason message)
  "Signal a structured selection failure with actionable MESSAGE."
  (error 'model-selection-error :model model :provider provider
         :reason reason :message message))

(-> provider--registration-model (provider-registration string) (option provider-model))
(defun provider--registration-model (registration model)
  "Find the exact MODEL in REGISTRATION, including slash-containing identifiers."
  (find model (provider-registration-models registration)
        :key #'provider-model-name :test #'string=))

(-> provider-resolve-model
    (string &key (:provider (option string)))
    (values string (option provider-registration)))
(defun provider-resolve-model (model &key provider)
  "Return the registry model ID and its provider registration.

Bare IDs retain registry precedence. A registered provider prefix selects that
provider; arbitrary slashes in exact catalog IDs are preserved. PROVIDER selects
an exact model in that provider first, so a slash ID cannot be misrouted."
  (let* ((requested (and provider (provider--canonical-name provider)))
         (selected  (and requested (provider-registration-find requested)))
         (slash     (position #\/ model))
         (prefix    (and slash (subseq model 0 slash)))
         (prefixed  (and prefix (provider-registration-find prefix)))
         (suffix    (and slash (subseq model (1+ slash))))
         (exact     (provider-registry-for-model *provider-registry* model)))
    (when (and requested (null selected))
      (provider--selection-error
       model provider :reason ':unknown-provider
       :message               (format nil "Unknown provider ~S. Registered providers: ~{~A~^, ~}."
                                      provider (mapcar #'provider-registration-name (provider-registrations)))))
    (when selected
      (cond
        ((provider--registration-model selected model)
         (return-from provider-resolve-model (values model selected)))
        ((and prefixed (not (eq selected prefixed)))
         (provider--selection-error
          model provider :reason ':provider-mismatch
          :message               (format nil "Model ~S names provider ~A, not ~A. Select its exact model ID with :provider ~S."
                                         model (provider-registration-name prefixed) requested requested)))
        ((and prefixed (provider--registration-model selected suffix))
         (return-from provider-resolve-model (values suffix selected)))
        (t
         (provider--selection-error
          model provider :reason ':unsupported-model
          :message               (format nil "Provider ~A does not serve model ~S. Use (models :provider ~S) to list its models."
                                         requested model requested)))))
    (let ((qualified (and prefixed (provider--registration-model prefixed suffix))))
      (when (and exact qualified (not (eq exact prefixed)))
        (provider--selection-error
         model nil :reason ':ambiguous-model
         :message          (format nil "Model ~S is both a catalog ID and a provider-qualified ID. Select explicitly with :provider."
                                   model)))
      (cond
        (qualified
         (values suffix prefixed))
        (exact
         (values model exact))
        (prefixed
         (provider--selection-error
          model prefix :reason ':unsupported-model
          :message             (format nil "Provider ~A does not serve model ~S." prefix suffix)))
        (t
         (values model nil))))))

(-> provider-model-selection-id (string provider-registration) string)
(defun provider-model-selection-id (model registration)
  "Return a provider-qualified selection, preserving legacy namespaced catalog IDs."
  (let ((prefix (format nil "~A/" (provider-registration-name registration))))
    (if (and (uiop:string-prefix-p prefix model)
             (null (provider--registration-model registration (subseq model (length prefix)))))
        model
        (concatenate 'string prefix model))))

(-> configuration-model-selection-id (configuration) string)
(defun configuration-model-selection-id (configuration)
  "Return CONFIGURATION's provider-qualified display ID."
  (let ((registration (configuration-model-registration configuration))
        (model        (config :model configuration)))
    (if registration (provider-model-selection-id model registration) model)))

(-> configuration-model-registration (configuration) (option provider-registration))
(defun configuration-model-registration (configuration)
  "Resolve CONFIGURATION's selected provider without discarding its identity."
  (let ((provider (config :model-provider configuration)))
    (nth-value 1 (provider-resolve-model
                  (config :model configuration)
                  :provider (and (non-empty-string-p provider) provider)))))

(-> configuration-model-provider-name (configuration) (option string))
(defun configuration-model-provider-name (configuration)
  "Return the selected provider name, including legacy bare-model resolution."
  (let ((registration (configuration-model-registration configuration)))
    (and registration (provider-registration-name registration))))

(-> provider-model-selections (&key (:provider (option string))) list)
(defun provider-model-selections (&key provider)
  "Return every model as a qualified ID, optionally restricted to PROVIDER."
  (let ((registrations
          (if provider
              (list (or (provider-registration-find (provider--canonical-name provider))
                        (provider--selection-error
                         "" provider :reason ':unknown-provider
                         :message            (format nil "Unknown provider ~S." provider))))
              (provider-registrations))))
    (loop for registration in registrations
          append (loop for metadata in (provider-registration-models registration)
                       collect (provider-model-selection-id
                                (provider-model-name metadata) registration)))))

(-> provider-registration-for-model (string) (option provider-registration))
(defun provider-registration-for-model (model)
  "Return the provider for a bare or provider-qualified MODEL selection."
  (nth-value 1 (provider-resolve-model model)))

(-> provider--resolve-model-metadata
    (string &key (:provider (option string)))
    (values string (option provider-registration)))
(defun provider--resolve-model-metadata (model &key provider)
  "Resolve metadata without requiring a populated model catalog.

Concrete adapters and deferred configurations may name models before discovery.
Keep selection validation in PROVIDER-RESOLVE-MODEL; metadata callers use their
normal defaults when no exact model metadata exists. Ambiguous selections and
contradictory provider prefixes still signal their selection errors."
  (handler-case (provider-resolve-model model :provider provider)
    (model-selection-error (condition)
      (unless (member (model-selection-error-reason condition)
                      '(:unsupported-model :unknown-provider))
        (error condition))
      (let* ((slash        (position #\/ model))
             (name         (or provider (and slash (subseq model 0 slash))))
             (registration (and name
                                (provider-registration-find
                                 (provider--canonical-name name)))))
        (values model registration)))))

(-> provider-model-for (string &key (:provider (option string))) (option provider-model))
(defun provider-model-for (model &key provider)
  "Return available metadata for MODEL without validating catalog membership."
  (multiple-value-bind (identifier registration)
      (provider--resolve-model-metadata model :provider provider)
    (and registration (provider--registration-model registration identifier))))

(-> provider-model-identifiers () list)
(defun provider-model-identifiers ()
  "Return unique model identifiers exposed by effective providers."
  (provider-registry-model-identifiers *provider-registry*))

(-> provider-model-family (string &key (:provider (option string))) (option keyword))
(defun provider-model-family (model &key provider)
  "Return MODEL's selected provider family, even before model discovery."
  (let ((registration (nth-value 1 (provider--resolve-model-metadata model :provider provider))))
    (and registration (provider-registration-family registration))))

(-> provider-model-endpoint (string &key (:provider (option string))) (option string))
(defun provider-model-endpoint (model &key provider)
  "Return MODEL's selected provider endpoint, even before model discovery."
  (let ((registration (nth-value 1 (provider--resolve-model-metadata model :provider provider))))
    (and registration (provider-registration-endpoint registration))))

(-> provider-model-context-window-for (string &key (:provider (option string))) (option integer))
(defun provider-model-context-window-for (model &key provider)
  "Return MODEL's metadata from the selected provider."
  (let ((metadata (provider-model-for model :provider provider)))
    (and metadata (provider-model-context-window metadata))))

(-> provider-model-reasoning-efforts-for (string &key (:provider (option string))) (option list))
(defun provider-model-reasoning-efforts-for (model &key provider)
  "Return MODEL's metadata from the selected provider."
  (let ((metadata (provider-model-for model :provider provider)))
    (and metadata (copy-list (provider-model-reasoning-efforts metadata)))))

(-> provider-model-provider-name (string) (option string))
(defun provider-model-provider-name (model)
  "Return the display name of the provider serving MODEL, when known."
  (let ((registration (provider-registration-for-model model)))
    (and registration (provider-registration-name registration))))


;;;; -- Registry State --

(-> provider--refresh-model-settings () null)
(defun provider--refresh-model-settings ()
  "Synchronize legacy bare-model tables with registry precedence."
  (let ((models (provider-model-identifiers)))
    (setf *supported-models* models
          *model-context-windows*
          (loop for model in models
                for metadata = (provider-registry-model *provider-registry* model)
                for window = (and metadata (provider-model-context-window metadata))
                when window collect (cons model window))))
  nil)

(-> provider--remove-registration-source (keyword) null)
(defun provider--remove-registration-source (source)
  "Remove all provider registrations supplied by SOURCE."
  (provider-registry-remove-source *provider-registry* source))

(-> provider--registry-snapshot () list)
(defun provider--registry-snapshot ()
  "Return an exact snapshot of provider registration layers and model state."
  (provider-registry-snapshot *provider-registry*))

(-> provider--registry-restore (list) null)
(defun provider--registry-restore (snapshot)
  "Restore provider registration layers and model state from SNAPSHOT."
  (provider-registry-restore *provider-registry* snapshot))
