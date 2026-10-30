;;;; -- Trusted Parent Profile and Account Factory --

(in-package #:autolith-supervisor)

(declaim (ftype (function () string) provider-account--identifier))

(defun provider-account--identifier ()
  "Return a fresh parent-owned request identifier."
  (format nil "~(~{~2,'0X~}~)" (coerce (ironclad:random-data 16) 'list)))

(declaim (ftype (function (pathname) provider-account-configuration) read-provider-account-configuration))

(defun read-provider-account-configuration (pathname)
  "Read one trusted data record. Never load Lisp configuration into the parent."
  (multiple-value-bind (form sole-form-p)
      (sexp-store:snapshot-read pathname)
    (unless
        (and sole-form-p (listp form) (eq (first form) ':provider-accounts)
             (= (getf (rest form) :version 0) 1)
             (loop for key in (rest form) by (lambda (tail) (rest (rest tail)))
                   always (member key '(:version :configuration))))
      (error 'provider-executor-error :reason ':invalid-account-configuration))
    (make-provider-account-configuration (getf (rest form) :configuration))))(declaim (ftype (function (keyword string) keyword) provider-account--protocol))

(defun provider-account--protocol (family model)
  "Select the built-in wire family from trusted service and model data."
  (case family
    ((:codex :grok :fireworks :openai-api)
     ':responses)
    (:anthropic
     ':anthropic)
    (:nous
     (if (uiop:string-prefix-p "anthropic/" (string-downcase model))
         ':anthropic ':chat-completions))
    (:gemini
     ':gemini)
    (:copilot
     (if (equal model "copilot/auto") ':auto ':chat-completions))
    ((:mistral :openrouter :opencode :openai-compatible)
     ':chat-completions)
    (otherwise
     (error 'provider-executor-error :reason ':unknown-family))))

(declaim (ftype (function (keyword keyword) string) provider-account--endpoint))

(defun provider-account--endpoint (family protocol)
  "Return a built-in trusted endpoint. Custom services require an explicit endpoint."
  (ecase family
    (:codex
     "https://chatgpt.com/backend-api/codex/responses")
    (:grok
     "https://cli-chat-proxy.grok.com/v1/responses")
    (:fireworks
     "https://api.fireworks.ai/inference/v1/responses")
    (:openai-api
     "https://api.openai.com/v1/responses")
    (:anthropic
     "https://api.anthropic.com/v1/messages")
    (:mistral
     "https://api.mistral.ai/v1/chat/completions")
    (:openrouter
     "https://openrouter.ai/api/v1/chat/completions")
    (:opencode
     "https://opencode.ai/zen/go/v1/chat/completions")
    (:nous
     (if (eq protocol ':anthropic)
         "https://inference-api.nousresearch.com/v1/messages"
         "https://inference-api.nousresearch.com/v1/chat/completions"))
    (:gemini
     "https://cloudcode-pa.googleapis.com/v1internal")
    (:copilot
     "https://api.individual.githubcopilot.com")))

(declaim (ftype (function (provider-account-configuration keyword
                           &key (:provider-name (or null string)) (:environment-variable (or null string)))
                         cl-rfc8628:credential-manager) make-provider-account-credential-manager))

(defun make-provider-account-credential-manager
       (configuration family &key provider-name environment-variable)
  "Create a parent-only source and refresh manager without loading credentials."
  (let* ((root
          (uiop/pathname:ensure-directory-pathname
           (provider-account-config ':state-root configuration)))
         (name (or provider-name (string-downcase family)))
         (source
          (make-instance 'autolith-credential-source :pathname
                         (merge-pathnames (format nil "~A-auth.sexp" name) root))))
    (case family
      (:codex
       (credential-manager-create :auth-path (merge-pathnames "auth.sexp" root) :codex-auth-path
        (merge-pathnames ".codex/auth.json" (user-homedir-pathname))))
      (:grok
       (let ((bootstrap (provider-account-config ':grok-bootstrap-auth-path configuration)))
         (make-instance 'grok-credential-manager :primary-source source :bootstrap-source
                        (when bootstrap
                          (make-instance 'grok-bootstrap-credential-source :pathname
                                         (pathname bootstrap))))))
      (:nous
       (nous-credential-manager-create configuration))
      (:gemini
       (gemini-credential-manager-create configuration))
      (:copilot
       (let ((domain (or (provider-account-config ':copilot-domain configuration) "github.com")))
         (unless
             (or (equal domain "github.com")
                 (and (uiop/utility:string-suffix-p domain ".ghe.com") (= (count #\. domain) 2)
                      (let ((tenant (subseq domain 0 (- (length domain) 8))))
                        (and (<= 1 (length tenant) 63) (not (find (char tenant 0) "-"))
                             (not (find (char tenant (1- (length tenant))) "-"))
                             (every
                              (lambda (character)
                                (find character "abcdefghijklmnopqrstuvwxyz0123456789-"))
                              tenant)))))
           (error 'provider-executor-error :reason ':invalid-copilot-domain))
         (make-instance 'copilot-credential-manager :primary-source source :domain domain)))
      (otherwise
       (let ((variable
              (or environment-variable
                  (case family
                    (:anthropic
                     "ANTHROPIC_API_KEY")
                    (:mistral
                     "MISTRAL_API_KEY")
                    (:fireworks
                     "FIREWORKS_API_KEY")
                    (:openrouter
                     "OPENROUTER_API_KEY")
                    (:opencode
                     "OPENCODE_API_KEY")))))
         (make-instance 'static-api-key-credential-manager :primary-source
                        (if (member family '(:fireworks :opencode))
                            source
                            (make-instance 'api-key-credential-source :provider-name name :pathname
                                           (pathname
                                            (or
                                             (provider-account-config ':api-keys-path
                                              configuration)
                                             (merge-pathnames "api-keys.sexp" root)))))
                        :bootstrap-source
                        (when variable
                          (make-instance 'environment-api-key-credential-source
                                         :environment-variable variable :account-id name))))))))

(declaim (ftype (function (&key (:id string) (:family keyword) (:model string)
                               (:configuration provider-account-configuration)
                               (:credential-manager (or null cl-rfc8628:credential-manager))
                               (:protocol (or null keyword)) (:endpoint (or null string))
                               (:provider-name (or null string)) (:headers list) (:options list)
                               (:capabilities list) (:maximum-output-tokens (integer 1))
                               (:reasoning-efforts list) (:reasoning-summaries list))
                         (values model-profile provider-account &optional)) make-parent-provider-profile))

(defun make-parent-provider-profile (&key id family model configuration credential-manager protocol endpoint
                                         provider-name headers options capabilities (maximum-output-tokens 32000)
                                         reasoning-efforts reasoning-summaries)
  "Return a trusted public profile and its private account binding.
The parent chooses all endpoints, headers, credentials, wire options, and capabilities.
Keep the account and profile objects in the parent. Send only the profile ID."
  (let* ((protocol     (or protocol (provider-account--protocol family model)))
         (endpoint     (or endpoint (provider-account--endpoint family protocol)))
         (options      (copy-tree options))
         (capabilities (copy-list capabilities)))
    (when (and (member ':native-compaction capabilities) (not (eq family ':codex)))
      (error 'provider-executor-error :reason ':unsupported))
    (when (and (member ':turn-state capabilities) (not (eq family ':codex)))
      (error 'provider-executor-error :reason ':unsupported))
    (when (eq family ':gemini)
      (setf (getf options :project) (or (getf options :project)
                                      (provider-account-config ':gemini-project configuration))))
    (let ((profile
            (make-model-profile :id id :model model
                                :family  (if (eq protocol ':auto) ':chat-completions protocol)
                                :options (copy-tree (getf options :wire-options))
                                :capabilities capabilities :maximum-output-tokens maximum-output-tokens
                                :reasoning-efforts reasoning-efforts :reasoning-summaries reasoning-summaries))
          (account
            (make-provider-account :family family :protocol protocol :endpoint endpoint
                                   :configuration configuration :headers headers :options options
                                   :credential-manager
                                   (or credential-manager
                                       (make-provider-account-credential-manager
                                        configuration family :provider-name provider-name)))))
      (values profile account))))

(declaim (ftype (function (list) (values hash-table hash-table &optional))
                make-parent-provider-profile-registry))

(defun make-parent-provider-profile-registry (bindings)
  "Return public ID lookup and private EQ account lookup for profile/account pairs."
  (let ((profiles nil) (accounts (make-hash-table :test #'eq)))
    (dolist (binding bindings)
      (destructuring-bind (profile account) binding
        (unless (and (typep profile 'model-profile) (typep account 'provider-account))
          (error 'provider-executor-error :reason ':invalid-account-registry))
        (push profile profiles)
        (setf (gethash profile accounts) account)))
    (values (make-model-profile-registry (nreverse profiles)) accounts)))
