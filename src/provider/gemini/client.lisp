(in-package #:autolith)

;;;; -- Gemini Product Request Projection --

(defclass gemini-product-provider-mixin ()
  ()
  (:documentation "Autolith history and context projection for Gemini transports."))

(defmethod provider-request-object
    ((provider gemini-product-provider-mixin) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Project Autolith history and context through the shared GenerateContent encoder.
Return the encoded request and its context delivery."
  (let* ((configuration (provider-configuration provider))
         (effective-tools
           (if compaction-p
               #()
               (provider-request-tool-namespaces configuration tool-namespaces)))
         (delivery
           (unless compaction-p
             (context-resolve-request configuration conversation effective-tools
                                      :goal-context goal-context
                                      :compaction-p compaction-p)))
         (projection
           (make-instance
            'cl-llm-provider-api:wire-request
            :model   (config :model configuration)
            :items   (conversation-input-items-for-family
                      conversation (provider-family provider)
                      :include-ephemeral-p (not compaction-p))
            :prefix  (list (system-prompt configuration)
                           (and compaction-p *compaction-instructions*))
            :suffix  (unless compaction-p
                       (list goal-context
                             (and delivery (context-delivery-rendered delivery))))
            :options (list :tool-choice           *provider-tool-choice*
                           :maximum-output-tokens *provider-maximum-output-tokens*
                           :include-thoughts-p
                           (not (string= (config :reasoning-effort configuration) "none"))))))
    (values (provider-request-object provider projection
                                     (provider-wire-tools provider effective-tools))
            delivery)))
