;;;; -- Trusted Extension Profiles and Execution --

(in-package #:autolith-supervisor)

(defclass parent-extension-account (provider-account)
  ((registry :initarg :registry :reader parent-extension-account-registry
             :documentation "Parent-only executable provider registry.")
   (provider-name :initarg :provider-name :reader parent-extension-account-provider-name
                  :documentation "Exact trusted provider registration name.")
   (settings :initarg :settings :reader parent-extension-account-settings
             :documentation "Parent-only factory configuration, never a child projection."))
  (:documentation "An executable custom provider binding owned by the trusted parent."))

(serapeum/types:-> parent-provider--builtin-family (string) (or null keyword))
(defun parent-provider--builtin-family (name)
  "Resolve ordinary provider names and legacy aliases without evaluating agent init."
  (rest (assoc (string-downcase name)
               '(("chatgpt" . :codex) ("codex" . :codex) ("openai" . :codex)
                 ("openai-api" . :openai-api) ("grok" . :grok) ("fireworks" . :fireworks)
                 ("anthropic" . :anthropic) ("mistral" . :mistral) ("openrouter" . :openrouter)
                 ("opencode" . :opencode) ("nous" . :nous) ("gemini" . :gemini)
                 ("copilot" . :copilot)) :test #'string=)))

(serapeum/types:-> parent-provider--protocol (cl-llm-provider-api:provider-registration) keyword)
(defun parent-provider--protocol (registration)
  "Choose the logical projection dialect from trusted extension metadata."
  (case (cl-llm-provider-api:provider-registration-protocol registration)
    ((:messages :anthropic)
     ':anthropic)
    ((:gemini :gemini-code-assist)
     ':gemini)
    (:chat-completions
     ':chat-completions)
    (otherwise
     ':responses)))

(serapeum/types:-> parent-provider--extension-profile
    (cl-llm-provider-api:provider-registration cl-llm-provider-api:provider-model) list)
(defun parent-provider--extension-profile (registration model)
  "Build an inert profile specification from a parent registration and one model."
  (let ((name       (cl-llm-provider-api:provider-registration-name registration))
        (identifier (cl-llm-provider-api:provider-model-name model)))
    (list :id (format nil "~A/~A" name identifier) :family ':custom
          :provider-name name :model identifier :protocol (parent-provider--protocol registration)
          :reasoning-efforts   (copy-list (cl-llm-provider-api:provider-model-reasoning-efforts model))
          :reasoning-summaries '("auto" "concise" "detailed" "none")
          :capabilities        '(:media))))

(serapeum/types:-> parent-provider-resolve-model
    (cl-llm-provider-api:provider-registry string &key (:provider-name (or null string)))
    (values string string (or null cl-llm-provider-api:provider-registration) &optional))
(defun parent-provider-resolve-model (registry model &key provider-name)
  "Resolve exact and provider-qualified model IDs without losing slash-containing IDs."
  (unless (plusp (length model)) (parent-provider--fail ':missing-selected-model))
  (when (and provider-name (zerop (length provider-name)))
    (setf provider-name nil))
  (let* ((slash    (position #\/ model))
         (prefix   (and slash (subseq model 0 slash)))
         (suffix   (and slash (subseq model (1+ slash))))
         (prefixed (and prefix (cl-llm-provider-api:provider-registry-find registry prefix)))
         (exact    (cl-llm-provider-api:provider-registry-for-model registry model))
         (selected (and provider-name (cl-llm-provider-api:provider-registry-find registry provider-name))))
    (labels ((serves-p (registration identifier)
               "Check an exact identifier in one effective registration."
               (and registration
                    (find identifier (cl-llm-provider-api:provider-registration-models registration)
                          :key #'cl-llm-provider-api:provider-model-name :test #'equal)))

             (resolve-registration (registration)
               "Preserve exact slash IDs before considering a qualified suffix."
               (let ((name (cl-llm-provider-api:provider-registration-name registration)))
                 (cond
                   ((serves-p registration model)
                    (values model name registration))
                   ((and prefixed (not (eq prefixed registration)))
                    (parent-provider--fail ':provider-model-mismatch))
                   ((and (eq prefixed registration) (serves-p registration suffix))
                    (values suffix name registration))
                   (t
                    (parent-provider--fail ':unknown-selected-model))))))
      (when selected (return-from parent-provider-resolve-model (resolve-registration selected)))
      (unless provider-name
        (when (and exact prefixed (not (eq exact prefixed)) (serves-p prefixed suffix))
          (parent-provider--fail ':ambiguous-model))
        (when (and prefixed (serves-p prefixed suffix))
          (return-from parent-provider-resolve-model (resolve-registration prefixed)))
        (when exact (return-from parent-provider-resolve-model (resolve-registration exact))))
      (let* ((name   (or provider-name
                         (and prefix (parent-provider--builtin-family prefix) prefix)
                         "chatgpt"))
             (family (parent-provider--builtin-family name)))
        (unless family (parent-provider--fail ':unknown-provider))
        (when (and prefix (parent-provider--builtin-family prefix)
                   (not (eq family (parent-provider--builtin-family prefix))))
          (parent-provider--fail ':provider-model-mismatch))
        (values (if (and prefix (eq family (parent-provider--builtin-family prefix))) suffix model)
                (if (eq family ':codex) "chatgpt" (string-downcase name)) nil)))))

(serapeum/types:-> parent-provider-configuration-profiles
    (list &key (:registry (or null cl-llm-provider-api:provider-registry))
          (:extensions list) (:writable-roots list))
    (values list string cl-llm-provider-api:provider-registry &optional))
(defun parent-provider-configuration-profiles (settings &key registry extensions writable-roots)
  "Return profile specifications, selected ID, and private registry without remote discovery.
Load only explicit EXTENSIONS outside WRITABLE-ROOTS before model selection."
  (let* ((registry   (or registry
                         (make-parent-provider-extension-registry
                          :cache-path (when (getf settings :private-root)
                                        (merge-pathnames "provider-model-cache.sexp"
                                                         (uiop:ensure-directory-pathname
                                                          (getf settings :private-root)))))))
         (extensions (or extensions (getf settings :provider-extensions)))
         (requested  (getf settings :model))
         (profiles   nil))
    (unless (and (stringp requested) (plusp (length requested)))
      (parent-provider--fail ':missing-selected-model))
    (when extensions
      (parent-provider-load-extensions registry extensions :writable-roots writable-roots))
    (parent-provider-load-model-cache registry settings)
    (multiple-value-bind (model name registration)
        (parent-provider-resolve-model registry requested :provider-name (getf settings :model-provider))
      (dolist (candidate (cl-llm-provider-api:provider-registry-registrations registry))
        (dolist (metadata (cl-llm-provider-api:provider-registration-models candidate))
          (push (parent-provider--extension-profile candidate metadata) profiles)))
      (unless registration
        (push (list :id                (format nil "~A/~A" name model)
                    :family (parent-provider--builtin-family name) :provider-name name :model model
                    :reasoning-efforts '("none" "minimal" "low" "medium" "high" "xhigh" "max" "ultra")
                    :reasoning-summaries '("auto" "concise" "detailed" "none") :capabilities '(:media)) profiles))
      (values (nreverse profiles) (format nil "~A/~A" name model) registry))))

(serapeum/types:-> parent-provider--configured-endpoint (keyword list) (or null string))
(defun parent-provider--configured-endpoint (family settings)
  "Resolve a parent setting before the ordinary family environment override."
  (labels ((present (value)
             "Reject empty endpoint overrides."
             (and (stringp value) (plusp (length value)) value)))
    (or (present (getf settings :provider-endpoint))
        (present
         (case family
           (:codex
            (uiop:getenv "AUTOLITH_PROVIDER_ENDPOINT"))
           (:grok
            (uiop:getenv "AUTOLITH_GROK_PROVIDER_ENDPOINT"))
           (:fireworks
            (uiop:getenv "AUTOLITH_FIREWORKS_PROVIDER_ENDPOINT"))
           (:opencode
            (uiop:getenv "AUTOLITH_OPENCODE_PROVIDER_ENDPOINT"))
           (:openrouter
            (uiop:getenv "AUTOLITH_OPENROUTER_PROVIDER_ENDPOINT"))
           (:mistral
            (uiop:getenv "AUTOLITH_MISTRAL_PROVIDER_ENDPOINT"))
           (:nous
            (or (uiop:getenv "AUTOLITH_NOUS_PROVIDER_ENDPOINT")
                (let ((base (present (uiop:getenv "AUTOLITH_NOUS_INFERENCE_BASE_URL"))))
                  (when base
                    (concatenate 'string (string-right-trim "/" base) "/chat/completions"))))))))))

(serapeum/types:-> parent-provider-build-bindings
    (parent-configuration list &key (:registry (or null cl-llm-provider-api:provider-registry)))
    (values hash-table hash-table cl-llm-provider-api:provider-registry list &optional))
(defun parent-provider-build-bindings (configuration settings &key registry)
  "Return public profile lookup, private accounts, registry and protected plugin files.
Validate plugin paths against each child writable scope before executing code.
Retain protected files in the application's private-root exclusions for all grants."
  (let* ((supplied-registry-p (not (null registry)))
         (registry            (or registry
                                  (make-parent-provider-extension-registry
                                   :cache-path (merge-pathnames
                                                "provider-model-cache.sexp"
                                                (uiop:ensure-directory-pathname
                                                 (parent-configuration-get configuration ':private-root))))))
         (roots               (remove nil
                                      (append (mapcar (lambda (key) (parent-configuration-get configuration key))
                                                      '(:workspace :temporary-root :private-root))
                                              (list (getf settings :site-config-root))
                                              (getf settings :child-writable-roots))))
         (specifications      (parent-configuration-get configuration ':provider-extensions))
         (protected
           (if supplied-registry-p
               (mapcar (lambda (specification)
                         (parent-provider-validate-extension-path (getf specification :path) roots))
                       specifications)
               (parent-provider-load-extensions registry specifications :writable-roots roots)))
         (accounts-configuration
           (make-provider-account-configuration
            (parent-configuration-get configuration ':account-configuration)))
         (bindings            nil))
    (parent-provider-load-model-cache registry settings)
    (dolist (specification (parent-configuration-get configuration ':profiles))
      (if (eq (getf specification :family) ':custom)
          (let* ((name             (getf specification :provider-name))
                 (registration     (parent-provider--find registry name))
                 (protocol         (parent-provider--protocol registration))
                 (profile
                   (make-model-profile
                    :id (getf specification :id) :model (getf specification :model)
                    :family protocol :capabilities (getf specification :capabilities)
                    :reasoning-efforts   (getf specification :reasoning-efforts)
                    :reasoning-summaries (getf specification :reasoning-summaries)))
                 (factory-settings (copy-tree settings)))
            (setf (getf factory-settings :model) (getf specification :model)
                  (getf factory-settings :model-provider) name)
            (push (list profile
                        (make-instance 'parent-extension-account :registry registry
                                       :provider-name name :settings factory-settings
                                       :family ':custom :protocol protocol :credential-manager nil
                                       :configuration accounts-configuration :endpoint "parent-extension"))
                  bindings))
          (let ((arguments (copy-tree specification)))
            (unless (getf arguments :endpoint)
              (setf (getf arguments :endpoint)
                    (parent-provider--configured-endpoint (getf specification :family) settings)))
            (unless (getf arguments :headers)
              (setf (getf arguments :headers) (copy-tree (getf settings :provider-headers))))
            (unless (getf arguments :options)
              (setf (getf arguments :options) (copy-tree (getf settings :provider-options))))
            (multiple-value-bind (profile account)
                (apply #'make-parent-provider-profile :configuration accounts-configuration arguments)
              (push (list profile account) bindings)))))
    (multiple-value-bind (profiles accounts)
        (make-parent-provider-profile-registry (nreverse bindings))
      (values profiles accounts registry protected))))

(serapeum/types:-> parent-provider-model-executor
    (&key (:accounts hash-table) (:session-id string) (:user-agent string)
          (:maximum-attempts (integer 1)) (:clock function))
    (values function function &optional))
(defun parent-provider-model-executor (&key accounts session-id (user-agent "autolith")
                                        (maximum-attempts 8) (clock #'session-runtime--clock))
  "Return model-service execution and strict abort callbacks for all parent providers.
Custom transports run in the service's tracked thread. Abort interrupts that thread
and waits for its unwind before it confirms completion. Callback conditions never
cross the private boundary. Built-in accounts use the existing owned HTTP executor."
  (multiple-value-bind (builtin-execute builtin-abort)
      (make-provider-family-model-executor :accounts accounts :session-id session-id
                                          :user-agent user-agent :maximum-attempts maximum-attempts
                                          :clock clock)
    (let ((lock   (bordeaux-threads:make-lock "parent provider callbacks"))
          (states (make-hash-table :test #'eq :weakness ':key)))
      (labels ((account (invocation)
                 "Find the exact trusted binding selected by the admitted profile."
                 (gethash (third (model-invocation-inputs invocation)) accounts))

               (state (invocation)
                 "Create a permanent cancellation latch under LOCK."
                 (or (gethash invocation states)
                     (setf (gethash invocation states) (list nil nil nil))))

               (execute (invocation callback)
                 "Execute built-in transport or one trusted extension callback."
                 (let ((account (account invocation)))
                   (unless (typep account 'parent-extension-account)
                     (return-from execute (funcall builtin-execute invocation callback)))
                   (let ((state (bordeaux-threads:with-lock-held (lock) (state invocation))))
                     (bordeaux-threads:with-lock-held (lock)
                       (when (third state) (error 'model-service-cancelled))
                       (setf (first state) (bordeaux-threads:current-thread)))
                     (unwind-protect
                         (destructuring-bind
                             (projection tools profile kind summary-p continuation turn-state)
                             (model-invocation-inputs invocation)
                           (declare (ignore profile continuation turn-state))
                           (unless (eq kind ':generate)
                             (parent-provider--fail ':unsupported-custom-operation))
                           (funcall callback
                                    (make-instance 'cl-llm-provider-api:provider-retry-event
                                                   :attempt 1 :maximum-attempts maximum-attempts :delay 0))
                           (cl-llm-provider-api:provider-call-with-response-deadline
                            (- (model-invocation-deadline invocation) (funcall clock))
                            (lambda ()
                              (parent-provider-stream-turn
                               (parent-extension-account-registry account)
                               (parent-extension-account-provider-name account)
                               :configuration         (parent-extension-account-settings account)
                               :conversation projection :tool-namespaces tools
                               :reasoning-summaries-p summary-p
                               :event-callback
                               (lambda (event)
                                 (bordeaux-threads:with-lock-held (lock)
                                   (when (third state) (error 'model-service-cancelled)))
                                 (funcall callback event))))))
                       (bordeaux-threads:with-lock-held (lock)
                         (setf (second state) t))))))

               (abort-invocation (invocation)
                 "Fence future callbacks, interrupt actual execution and await unwind."
                 (unless (typep (account invocation) 'parent-extension-account)
                  (return-from abort-invocation (funcall builtin-abort invocation)))
                 (let ((thread nil) (state nil))
                   (bordeaux-threads:with-lock-held (lock)
                     (setf state (state invocation)
                           (third state) t
                           thread (first state)))
                   (when (and thread (not (eq thread (bordeaux-threads:current-thread)))
                              (bordeaux-threads:thread-alive-p thread))
                      (bordeaux-threads:interrupt-thread
                       thread (lambda () (unless (second state) (error 'model-service-cancelled)))))
                   (loop with deadline = (+ (funcall clock) 10)
                         until (bordeaux-threads:with-lock-held (lock)
                                 (or (null (first state)) (second state)))
                         do (when (>= (funcall clock) deadline)
                              (parent-provider--fail ':custom-transport-abort-failed))
                            (sleep 0.01))
                   nil)))
        (values #'execute #'abort-invocation)))))
