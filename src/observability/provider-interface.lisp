(in-package #:autolith)

;;;; -- Provider Boundary --

(-> observability--active-p () boolean)
(defun observability--active-p ()
  "Return true when an observability backend is active for this context."
  (nemo-relay--active-p))

(-> observability-provider-name (t) string)
(defgeneric observability-provider-name (provider)
  (:documentation "Return the backend-neutral account name for PROVIDER."))

(defmethod observability-provider-name ((provider t))
  "Return a generic account name for an unknown provider implementation."
  (declare (ignore provider))
  "provider")

(-> observability--call-with-provider
    (model-provider json-object &key (:function function))
    t)
(defun observability--call-with-provider (provider request &key function)
  "Run FUNCTION inside the optional observability boundary for one provider call."
  (if (not (observability--active-p))
      (funcall function)
      (nemo-relay--call-with-llm
       :name (observability-provider-name provider)
       :model (nemo-relay--provider-model provider)
       :request request
       :function function)))

(defmacro with-observed-provider-call ((provider request) &body body)
  "Run BODY inside the optional observability boundary for one provider call."
  `(if (observability--active-p)
       (observability--call-with-provider
        ,provider ,request
        :function (lambda () ,@body))
       (progn ,@body)))
