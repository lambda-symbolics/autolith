(in-package #:autolith)

;;;; -- Provider Models from models.dev --

(defparameter *provider-catalog-sources*
  '(("chatgpt" "openai" "")
    ("gemini" "google" "")
    ("grok" "xai" "")
    ("anthropic" "anthropic" "")
    ("fireworks" "fireworks-ai" "")
    ("opencode-go" "opencode-go" "opencode/")
    ("opencode-zen" "opencode" "opencode-zen/")
    ("openrouter" "openrouter" "openrouter/")
    ("mistral" "mistral" ""))
  "Built-in transport names, public catalog keys and user-visible model prefixes.
Account-specific Copilot routing and providers absent from models.dev retain discovery.")

(defclass provider-catalog-context ()
  ()
  (:documentation "An explicit model-refresh request, independent of telemetry consent."))

(defmethod model-catalog-context-active-p ((context provider-catalog-context))
  "Authorize the synchronous user-requested catalog operation."
  (declare (ignore context))
  t)

(defmethod model-catalog-context-fetch ((context provider-catalog-context))
  "Fetch public metadata without provider credentials."
  (declare (ignore context))
  (model-catalog--worker-fetch))

(-> provider-catalog--source (provider-registration) list)
(defun provider-catalog--source (registration)
  "Resolve a built-in public source without overriding extensions or custom endpoints."
  (when (and (eq (provider-registration-source registration) ':builtin)
             (let ((resolver (provider-registration-model-discovery-endpoint-resolver registration)))
               (or (null resolver)
                   (equal (funcall resolver)
                          (provider-registration-model-discovery-endpoint registration)))))
    (assoc (provider-registration-name registration) *provider-catalog-sources* :test #'string=)))

(-> provider-catalog--efforts (hash-table) list)
(defun provider-catalog--efforts (record)
  "Return advertised effort choices, or the protocol default for reasoning models."
  (let ((options (json-get record "reasoning_options")))
    (or (when (vectorp options)
          (loop for option across options
                when (and (hash-table-p option) (equal (json-get option "type") "effort"))
                  return (let ((values (json-get option "values")))
                           (unless (and (vectorp values) (plusp (length values))
                                        (every #'non-empty-string-p values))
                             (model-catalog--reject "invalid reasoning efforts"))
                           (coerce values 'list))))
        (if (json-true-p (json-get record "reasoning"))
            (copy-list *supported-reasoning-efforts*)
            (list "none")))))

(-> provider-catalog--models (model-catalog list &key (:allowed-models (option list))) list)
(defun provider-catalog--models (catalog source &key allowed-models)
  "Project tool-capable text models to registry metadata, keeping exact wire IDs."
  (destructuring-bind (name key prefix) source
    (declare (ignore name))
    (let* ((providers (model-catalog-providers catalog))
           (provider  (and providers (gethash key providers)))
           (models    (and provider (json-get provider "models")))
           (result    nil))
      (unless (hash-table-p models) (model-catalog--reject "provider absent"))
      (maphash
       (lambda (identifier record)
         (let ((output (json-get (json-get record "modalities") "output"))
               (window (json-get (json-get record "limit") "context")))
           (when (and (or (null allowed-models) (member identifier allowed-models :test #'string=))
                      (json-true-p (json-get record "tool_call"))
                      (vectorp output) (find "text" output :test #'equal)
                      (not (equal (json-get record "status") "deprecated")))
             (unless (typep window '(integer 1))
               (model-catalog--reject "invalid context window"))
             (let ((description (or (json-get record "description") (json-get record "name"))))
               (unless (or (null description) (stringp description))
                 (model-catalog--reject "invalid model description"))
               (push (list :name              (concatenate 'string prefix identifier)
                           :description description :context-window window
                           :reasoning-efforts (provider-catalog--efforts record))
                     result)))))
       models)
      (unless result (model-catalog--reject "no usable provider models"))
      (sort result #'string< :key (lambda (spec) (getf spec :name))))))

(-> provider-catalog--install (model-catalog provider-registration) null)
(defun provider-catalog--install (catalog registration)
  "Replace built-in fallback metadata while preserving its transport and authentication."
  (let ((source (provider-catalog--source registration)))
    (when source
      (let ((models (provider-catalog--models
                     catalog source
                     :allowed-models
                     (when (member (provider-registration-name registration)
                                   '("chatgpt" "gemini" "grok") :test #'string=)
                       (mapcar #'provider-model-name (provider-registration-models registration))))))
        (when (member *provider-cache-selection-hint-model*
                      (provider-registration-discovered-models registration) :test #'eq)
          (unregister-provider (provider-registration-name registration) :source ':builtin)
          (setf *provider-cache-selection-hint-model* nil))
        (register-provider
         (provider-registration-name registration)
         :description              (provider-registration-description registration)
         :family                   (provider-registration-family registration)
         :models                   models
         :factory                  (provider-registration-factory registration)
         :authenticator            (provider-registration-authenticator registration)
         :protocol                 (provider-registration-protocol registration)
         :endpoint                 (provider-registration-endpoint registration)
         :model-discovery          (provider-registration-model-discovery registration)
         :model-discovery-endpoint (provider-registration-model-discovery-endpoint registration)
         :model-discovery-endpoint-resolver
         (provider-registration-model-discovery-endpoint-resolver registration)
         :source                   ':builtin))))
  nil)

(-> provider-catalog-load (configuration) model-catalog)
(defun provider-catalog-load (configuration)
  "Load shared last-good public metadata locally, without starting network work or timers."
  (let ((catalog (model-catalog-create :cache-root (config :cache-root configuration))))
    (model-catalog--load catalog)
    catalog))

(-> provider-catalog-refresh (model-catalog) null)
(defun provider-catalog-refresh (catalog)
  "Refresh once on explicit model discovery; failures preserve the last-good snapshot."
  (model-catalog--refresh catalog (make-instance 'provider-catalog-context)
                          *model-catalog-fetch-function*)
  nil)
