(in-package #:autolith)

;;;; -- Request Cost --

(defvar *provider-cost-ledger* (make-cost-ledger)
  "The process-wide record of what completed model requests cost.")

(defgeneric provider-model-price (provider configuration &key usage)
  (:documentation
   "Return CONFIGURATION's model price on PROVIDER's route, or NIL when unknown.

USAGE supplies normalized request usage for prompt-length tiers. Without usage,
return NIL for tiered prices. Configured model prices take precedence."))


;;;; -- Methods --

(defmethod provider-model-price
    ((provider model-provider) (configuration configuration) &key usage)
  "Return the configured model price."
  (declare (ignore provider usage))
  (provider--configured-model-price configuration))


;;;; -- Public Functions --

(-> provider-cost-check (t) null)
(defun provider-cost-check (provider)
  "Signal COST-LIMIT-REACHED if spending has reached PROVIDER's cost limit."
  (let* ((configuration (provider--cost-configuration provider))
         (limit         (and configuration (config :cost-limit configuration))))
    (cost-ledger-check-limit *provider-cost-ledger* (and limit (plusp limit) limit))))

(-> provider-cost-record (t provider-result) list)
(defun provider-cost-record (provider result)
  "Record RESULT's cost in the process ledger and return the ledger entry."
  (let ((configuration (provider--cost-configuration provider))
        (usage         (provider-usage-normalize (provider-result-usage result))))
    (cost-ledger-record *provider-cost-ledger* usage
                        :price (and configuration
                                    (provider-model-price provider configuration :usage usage))
                        :model (and configuration (config :model configuration))
                        :route (and (typep provider 'model-provider)
                                    (provider-account-label provider)))))


;;;; -- Private Functions --

(-> provider--cost-configuration (t) (option configuration))
(defun provider--cost-configuration (provider)
  "Return PROVIDER's configuration, or NIL for a provider without one."
  (and (typep provider 'subscription-provider)
       (typep (provider-configuration provider) 'configuration)
       (provider-configuration provider)))

(-> provider--configured-model-price (configuration) (option model-price))
(defun provider--configured-model-price (configuration)
  "Return CONFIGURATION's model price from the model prices setting, or NIL."
  (let ((text (config :model-prices configuration)))
    (unless (zerop (length text))
      (let ((rates (json-get (json-decode text) (config :model configuration))))
        (when rates
          (make-model-price :input       (json-get rates "input")
                            :cache-read  (json-get rates "cache_read")
                            :cache-write (json-get rates "cache_write")
                            :output      (json-get rates "output")
                            :source      "the model-prices setting"))))))
