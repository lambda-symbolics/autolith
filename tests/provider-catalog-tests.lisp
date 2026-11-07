(in-package #:autolith)

;;;; -- Provider Catalog Integration --

(-> test-provider-catalog--fixture () string)
(defun test-provider-catalog--fixture ()
  "Provide a new public model plus a non-tool model that must not enter the picker."
  (json-encode
   (json-object
    "openrouter"
    (json-object
     "models"
     (json-object
      "lab/new-agent"
      (json-object "id" "lab/new-agent" "name" "New Agent" "tool_call" t
                   "reasoning" t "reasoning_options"
                   (vector (json-object "type" "effort" "values" #("low" "high")))
                   "modalities" (json-object "output" #("text"))
                   "limit" (json-object "context" 123456))
      "lab/image"
      (json-object "id" "lab/image" "tool_call" nil
                   "modalities" (json-object "output" #("image"))
                   "limit" (json-object "context" 4096)))))))

(-> test-provider-catalog-picker () null)
(defun test-provider-catalog-picker ()
  "Refresh models.dev into the real registry, picker, wire route and offline startup."
  (with-test-configuration (configuration)
    (let ((snapshot (provider--registry-snapshot))
          (calls    0)
          (original (provider-registration-find "openrouter")))
      (unwind-protect
           (let ((*model-catalog-fetch-function*
                   (lambda (context)
                     (test-assert (typep context 'provider-catalog-context)
                                  "Picker discovery is independent of telemetry consent.")
                     (incf calls)
                     (test-provider-catalog--fixture))))
             (test-assert (null (provider-refresh-models configuration :provider-name "openrouter"))
                          "Public discovery succeeds without account credentials.")
             (test-assert (= calls 1) "One explicit refresh fetches the public catalog once.")
             (let ((metadata     (provider-model-for "openrouter/lab/new-agent"))
                   (registration (provider-registration-find "openrouter")))
               (test-assert (and metadata (= (provider-model-context-window metadata) 123456)
                                 (equal (provider-model-reasoning-efforts metadata) '("low" "high")))
                            "Public context and reasoning metadata reach configuration validation.")
               (test-assert (equal (openrouter--wire-model-name "openrouter/lab/new-agent") "lab/new-agent")
                            "Picker names preserve exact provider wire identifiers.")
               (test-assert (and (eq (provider-registration-factory registration)
                                     (provider-registration-factory original))
                                 (eq (provider-registration-authenticator registration)
                                     (provider-registration-authenticator original)))
                            "Public metadata preserves the account transport and authentication."))
             (test-call-with-function-replacements
              (list (list 'application--credentialed-provider-names
                          (lambda (application) (declare (ignore application)) '("openrouter"))))
              (lambda ()
                (let ((items (application--model-items
                              (make-instance 'application :configuration configuration))))
                  (test-assert (find "openrouter/lab/new-agent" items
                                     :key (lambda (item) (getf item :name)) :test #'equal)
                               "A newly cataloged model is offered by the real picker.")
                  (test-assert (not (find "openrouter/lab/image" items
                                          :key (lambda (item) (getf item :name)) :test #'equal))
                               "Non-tool image models are excluded."))))
             (provider--registry-restore snapshot)
             (let ((*model-catalog-fetch-function* (lambda (context)
                                                    (declare (ignore context))
                                                    (error "Startup must remain offline."))))
               (provider-load-model-cache configuration)
               (test-assert (provider-model-for "openrouter/lab/new-agent")
                            "A fresh registry loads public metadata from the last-good disk cache."))
             (let ((*model-catalog-fetch-function* (lambda (context) (declare (ignore context)) nil)))
               (test-assert (null (provider-refresh-models configuration :provider-name "openrouter"))
                            "Warm offline refresh retains usable public metadata.")))
        (provider--registry-restore snapshot))))
  nil)

(-> test-provider-catalog-failures () null)
(defun test-provider-catalog-failures ()
  "Cold and malformed sources preserve built-ins; extension providers retain their discovery."
  (with-test-configuration (configuration)
    (let ((snapshot (provider--registry-snapshot))
          (original (provider-registration-find "anthropic")))
      (unwind-protect
           (progn
             (dolist (body '(nil "{}" "{\"anthropic\":{\"models\":{\"bad\":{\"id\":\"bad\"}}}}"))
               (let ((*model-catalog-fetch-function* (lambda (context) (declare (ignore context)) body)))
                 (test-assert (provider-refresh-models configuration :provider-name "anthropic")
                              "Cold unavailable or unusable catalogs report discovery failure.")
                 (test-assert (eq original (provider-registration-find "anthropic"))
                              "Failed refresh does not replace usable fallback metadata.")))
             (register-provider "anthropic" :models '("extension/model")
                                :factory (provider-registration-factory original) :source ':runtime)
             (let ((*model-catalog-fetch-function* (lambda (context)
                                                    (declare (ignore context))
                                                    (error "An extension must not fetch the public catalog."))))
               (test-assert (null (provider-refresh-models configuration :provider-name "anthropic"))
                            "A custom provider keeps its registration and explicit model list.")
               (test-assert (equal (mapcar #'provider-model-name
                                           (provider-registration-models (provider-registration-find "anthropic")))
                                   '("extension/model"))
                            "The public mapping cannot replace a higher-precedence extension.")))
        (provider--registry-restore snapshot))))
  nil)

(-> test-provider-catalog-subscriptions () null)
(defun test-provider-catalog-subscriptions ()
  "Choose subscription budgets per configuration without expanding account access."
  (with-test-configuration (configuration)
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" nil)
                            ("AUTOLITH_PREFER_CODEX_CONTEXT_WINDOW" nil))
      (let ((snapshot (provider--registry-snapshot)))
        (unwind-protect
             (multiple-value-bind (identifiers providers)
                 (model-catalog-parse
                  (json-encode
                   (json-object
                    "openai"
                    (json-object
                     "models"
                     (let ((models (make-hash-table :test #'equal)))
                       (dolist (name (append (mapcar (lambda (spec) (getf spec :name))
                                                    *codex-models*)
                                             (list "api-only-model")))
                         (setf (gethash name models)
                               (json-object "id" name "tool_call" t
                                            "modalities" (json-object "output" #("text"))
                                            "limit" (json-object "context" 1050000))))
                       models)))))
               (provider-catalog--install
                (model-catalog-create :identifiers identifiers :providers providers)
                (provider-registration-find "chatgpt"))
               (test-assert (= (provider-model-context-window-for *default-model*
                                                                 :provider "chatgpt")
                               1050000)
                            "Public metadata is available when a configuration opts in.")
               (dolist (spec *codex-models*)
                 (let* ((model  (getf spec :name))
                        (codex  (configuration-copy configuration :model model
                                                    :model-provider      "chatgpt"))
                        (public (configuration-copy codex :prefer-codex-context-window-p nil)))
                   (test-assert (= (config :context-window codex) 272000)
                                "Codex subscription windows take precedence by default.")
                   (test-assert (= (config :context-window public) 1050000)
                                "A configuration can opt in to the public catalog window.")
                   (test-assert (= (configuration-compaction-token-limit codex) 217600)
                                "Compaction uses the selected subscription budget.")
                   (test-assert (= (configuration-compaction-token-limit public) 840000)
                                "An opted-in child uses its own compaction budget.")))
               (setf (config :prefer-codex-context-window-p configuration) nil)
               (test-assert (= (config :context-window configuration) 1050000)
                            "Changing the setting takes effect without a catalog refresh.")
               (flet ((reload ()
                        (configuration-create
                         :config-root (config :config-root configuration)
                         :data-root   (config :data-root configuration)
                         :cache-root  (config :cache-root configuration)
                         :state-root  (config :state-root configuration)
                         :model *default-model* :model-provider "chatgpt")))
                 (test-assert (not (config :prefer-codex-context-window-p (reload)))
                              "The public-window preference survives configuration reload.")
                 (with-test-environment (("AUTOLITH_PREFER_CODEX_CONTEXT_WINDOW" "on"))
                   (test-assert (= (config :context-window (reload)) 272000)
                                "An environment choice overrides the saved preference.")))
               (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "500000"))
                 (dolist (prefer '(t nil))
                   (test-assert
                    (= (config :context-window
                               (configuration-copy configuration :prefer-codex-context-window-p prefer))
                       500000)
                    "An explicit context window wins over either metadata source.")))
               (test-assert (null (provider-registration-for-model "api-only-model"))
                            "Public API membership cannot grant subscription model access.")
               (register-provider "test-api"
                                  :models  (list (list :name *default-model* :context-window 700000))
                                  :factory (provider-registration-factory
                                            (provider-registration-find "chatgpt"))
                                  :source  ':runtime)
               (test-assert
                (= (config :context-window
                           (configuration-copy configuration :model-provider                "test-api"
                                                             :prefer-codex-context-window-p t))
                   700000)
                "The same wire model on another provider uses that provider's metadata.")
               (register-provider "chatgpt"
                                  :models  (list (list :name *default-model* :context-window 333333))
                                  :factory (provider-registration-factory
                                            (provider-registration-find "chatgpt"))
                                  :source  ':runtime)
               (test-assert
                (= (config :context-window
                           (configuration-copy configuration :prefer-codex-context-window-p t))
                   333333)
                "User registrations override built-in Codex defaults."))
          (provider--registry-restore snapshot)))))
  nil)

(-> test-provider-catalog-opencode-tiers () null)
(defun test-provider-catalog-opencode-tiers ()
  "Keep Go and Zen metadata, picker selections and persisted routes independent."
  (with-test-configuration (configuration)
    (let ((snapshot (provider--registry-snapshot))
          (body     (json-object)))
      (dolist (tier '(("opencode-go" 1000000) ("opencode" 128000)))
        (setf (gethash (first tier) body)
              (json-object
               "models" (json-object
                         "deepseek-v4.1-flash"
                         (json-object "id" "deepseek-v4.1-flash" "tool_call" t "reasoning" t
                                      "modalities" (json-object "output" #("text"))
                                      "limit" (json-object "context" (second tier)))))))

      (setf (gethash "deepseek-v4.1-flash-free" (json-get (gethash "opencode" body) "models"))
            (json-object "id" "deepseek-v4.1-flash-free" "tool_call" t "reasoning" t
                         "modalities" (json-object "output" #("text"))
                         "limit" (json-object "context" 32000)))
      (unwind-protect
           (let ((*model-catalog-fetch-function*
                   (lambda (context) (declare (ignore context)) (json-encode body))))
             (test-assert (eq (provider-registration-find "opencode")
                              (provider-registration-find "opencode-go"))
                          "Legacy provider selection resolves to Go, never Zen.")
             (dolist (name '("opencode" "opencode-zen"))
               (test-assert (null (provider-refresh-models configuration :provider-name name))
                            "Each OpenCode tier refreshes its own public catalog."))
             (flet ((check-selections ()

                      (test-assert
                       (and (provider-model-for "opencode-zen/deepseek-v4.1-flash-free")
                            (null (provider-model-for "opencode/deepseek-v4.1-flash-free")))
                       "Zen's free variant does not enter the Go catalog.")
                      (dolist (tier '(("opencode/deepseek-v4.1-flash" "opencode-go" 1000000)
                                      ("opencode-zen/deepseek-v4.1-flash" "opencode-zen" 128000)))
                        (destructuring-bind (model name window) tier
                          (let ((selected (configuration-copy configuration :model        model
                                                                          :model-provider name)))
                            (test-assert (= (config :context-window selected) window)
                                         "Tier-specific context reaches selected configuration.")
                            (test-assert (equal (provider-model-provider-name model) name)
                                         "The same raw ID belongs to independent registrations."))))))
               (check-selections)
               (test-call-with-function-replacements
                (list (list 'application--credentialed-provider-names
                            (lambda (application)
                              (declare (ignore application)) '("opencode-go" "opencode-zen"))))
                (lambda ()
                  (let ((items (application--model-items
                                (make-instance 'application :configuration configuration))))
                    (dolist (model '("opencode/deepseek-v4.1-flash"
                                     "opencode-zen/deepseek-v4.1-flash"))
                      (let ((item (find model items :key (lambda (item) (getf item :model))
                                                   :test #'equal)))
                        (test-assert item "Both OpenCode selections appear in the picker.")
                        (multiple-value-bind (identifier registration)
                            (provider-resolve-model (getf item :name))
                          (test-assert (and (equal identifier model)
                                            (equal (getf item :provider)
                                                   (provider-registration-name registration)))
                                       "Picker selection preserves its tier.")))))))
               (provider--registry-restore snapshot)
               (provider-load-model-cache configuration)
               (check-selections)
               (test-assert (null (provider-refresh-models configuration :provider-name "opencode"))
                            "Refreshing the legacy Go selection keeps the Go catalog.")
               (check-selections)))
        (provider--registry-restore snapshot))))
  nil)
