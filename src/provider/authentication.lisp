(in-package #:autolith)

;;;; -- Legacy Account Configuration Adapter --

(-> credential-manager-create (configuration) chatgpt-credential-manager)
(defun credential-manager-create (configuration)
  "Create the shared ChatGPT manager for CONFIGURATION's trusted private paths."
  (autolith-supervisor:credential-manager-create
   :auth-path       (configuration-auth-path configuration)
   :codex-auth-path (config :codex-auth-path configuration)))

(-> provider-user-agent () string)
(defun provider-user-agent ()
  "Return the honest Autolith user agent for providers and authentication services."
  (autolith-supervisor:accounts-user-agent))
