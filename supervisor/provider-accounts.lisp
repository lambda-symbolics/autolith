;;;; -- Trusted Provider Account Bindings --

(in-package #:autolith-supervisor)

(defclass provider-account (chatgpt-account)
  ((family :initarg :family :reader provider-account-family
           :documentation "Trusted account family, separate from the wire protocol.")
   (protocol :initarg :protocol :reader provider-account-protocol
             :documentation "Trusted library encoder family.")
   (credential-manager :initarg :credential-manager :reader provider-account-credential-manager
                       :documentation "Parent-only credential source and refresh policy.")
   (endpoint :initarg :endpoint :reader provider-account-endpoint
             :documentation "Trusted inference endpoint.")
   (configuration :initarg :configuration :initform nil :reader provider-account-configuration
                  :documentation "Trusted OAuth configuration, never agent data.")
   (headers :initarg :headers :initform nil :reader provider-account-headers
            :documentation "Parent-selected extra request headers.")
   (options :initarg :options :initform nil :reader provider-account-options
            :documentation "Parent-selected family policy options.")
   (lock :initform (bordeaux-threads:make-lock "provider account setup") :reader provider-account-lock
         :documentation "Serialize subscription setup and account catalog changes.")
   (state :initform nil :accessor provider-account-state
          :documentation "Parent-only setup and route data without credentials."))
  (:documentation "A trusted account binding. Only its profile ID crosses the agent boundary."))

(declaim (ftype (function (&key (:family keyword) (:protocol keyword)
                               (:credential-manager cl-rfc8628:credential-manager)
                               (:endpoint string) (:configuration (or null provider-account-configuration))
                               (:headers list) (:options list)) provider-account)
                make-provider-account))

(defun make-provider-account (&key family protocol credential-manager endpoint configuration headers options)
  "Bind a trusted account and transport policy. Wire data cannot call this factory."
  (unless (and (member family '(:codex :grok :fireworks :openai-api :anthropic :mistral
                                :openrouter :opencode :openai-compatible :nous :gemini :copilot))
               (member protocol '(:responses :chat-completions :anthropic :gemini :auto))
               (typep credential-manager 'cl-rfc8628:credential-manager)
               (family-account-non-empty-string-p endpoint)
               (every (lambda (header)
                        (and (consp header) (stringp (first header)) (stringp (rest header))
                             (not (find-if (lambda (character) (find character '(#\Return #\Newline)))
                                           (concatenate 'string (first header) (rest header))))))
                      headers)
               (evenp (length options)))
    (error 'provider-executor-error :reason ':invalid-account))
  (unless (case family
            ((:codex :grok :fireworks :openai-api)
             (eq protocol ':responses))
            (:anthropic
             (eq protocol ':anthropic))
            ((:mistral :openrouter :opencode :openai-compatible)
             (eq protocol ':chat-completions))
            (:nous
             (member protocol '(:anthropic :chat-completions)))
            (:gemini
             (eq protocol ':gemini))
            (:copilot
             (member protocol '(:auto :responses :anthropic :chat-completions))))
    (error 'provider-executor-error :reason ':account-protocol-mismatch))
  (make-instance 'provider-account :family family :protocol protocol
                 :credential-manager credential-manager :endpoint (copy-seq endpoint)
                 :configuration configuration :headers (copy-tree headers) :options (copy-tree options)))
