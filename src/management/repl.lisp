(in-package #:autolith)

;;;; -- Management REPL Conditions --

(define-condition management-repl-error (configuration-error)
  ((operation
    :initarg       :operation
    :reader        management-repl-error-operation
    :type          keyword
    :documentation "The management endpoint operation that failed.")
   (reason
    :initarg       :reason
    :initform      nil
    :reader        management-repl-error-reason
    :type          (option keyword)
    :documentation "The non-secret failure category."))
  (:documentation "An active-image management endpoint failure."))


;;;; -- Endpoint Ownership --

;;; The management REPL is image-daemon's evaluation endpoint, configured from
;;; the :MANAGEMENT-REPL-* settings, evaluating in the AUTOLITH package, and
;;; owned by one application or one world server at a time.

(defgeneric management-repl-owner-configuration (owner)
  (:documentation "Return the configuration OWNER's management endpoint is configured from."))

(defgeneric management-repl-owner-endpoint (owner)
  (:documentation "Return OWNER's running management endpoint, or NIL."))

(defgeneric (setf management-repl-owner-endpoint) (endpoint owner)
  (:documentation "Record ENDPOINT, or NIL, as OWNER's running management endpoint."))

(defmethod management-repl-owner-configuration ((owner application))
  "An application configures the endpoint from its own configuration."
  (application-configuration owner))

(defmethod management-repl-owner-endpoint ((owner application))
  "An application keeps its endpoint in its own slot."
  (application-management-repl-endpoint owner))

(defmethod (setf management-repl-owner-endpoint) (endpoint (owner application))
  "An application keeps its endpoint in its own slot."
  (setf (application-management-repl-endpoint owner) endpoint))

(-> management-repl-start (t) (option eval-endpoint))
(defun management-repl-start (owner)
  "Start OWNER's configured management endpoint when enabled and return it."
  (let ((configuration (management-repl-owner-configuration owner)))
    (cond
      ((not (config :management-repl-enabled-p configuration))
       nil)
      ((management-repl-owner-endpoint owner))
      (t
       (let ((endpoint (management-repl--call
                        (lambda ()
                          (eval-endpoint-start (management-repl--endpoint configuration))))))
         (setf (management-repl-owner-endpoint owner) endpoint))))))

(-> management-repl-stop (t) null)
(defun management-repl-stop (owner)
  "Stop OWNER's management endpoint, signaling when its threads do not quiesce."
  (let ((endpoint (management-repl-owner-endpoint owner)))
    (when endpoint
      (management-repl--call (lambda () (eval-endpoint-stop endpoint)))
      (setf (management-repl-owner-endpoint owner) nil)))
  nil)

(-> management-repl-transfer (t t) null)
(defun management-repl-transfer (old-owner new-owner)
  "Move the running management endpoint from OLD-OWNER to NEW-OWNER."
  (let ((endpoint (management-repl-owner-endpoint old-owner)))
    (when endpoint
      (setf (management-repl-owner-endpoint new-owner) endpoint
            (management-repl-owner-endpoint old-owner) nil)))
  nil)

(-> management-repl-call-quiesced (t function) t)
(defun management-repl-call-quiesced (owner function)
  "Call FUNCTION with OWNER's management endpoint stopped, then restart it."
  (let ((endpoint (management-repl-owner-endpoint owner)))
    (when (and endpoint (eq (current-thread) (eval-endpoint-evaluator-thread endpoint)))
      (error 'management-repl-error
             :message   "A management evaluation cannot checkpoint its own evaluator."
             :operation ':checkpoint
             :reason    ':evaluator))
    (when endpoint
      (management-repl-stop owner))
    (unwind-protect
         (funcall function)
      (when endpoint
        (management-repl-start owner)))))

(-> management-repl--endpoint (configuration) eval-endpoint)
(defun management-repl--endpoint (configuration)
  "Return an unstarted evaluation endpoint configured by CONFIGURATION."
  (eval-endpoint-create
   :transport              (config :management-repl-transport configuration)
   :unix-pathname          (config :management-repl-unix-socket-path configuration)
   :tcp-address            (config :management-repl-tcp-address configuration)
   :tcp-port               (config :management-repl-tcp-port configuration)
   :token-pathname         (config :management-repl-token-file-path configuration)
   :package                '#:autolith
   :evaluation-timeout     (config :management-repl-evaluation-timeout configuration)
   :authentication-timeout (config :management-repl-authentication-timeout configuration)
   :maximum-frame-size     (config :management-repl-maximum-frame-size configuration)
   :maximum-source-size    (config :management-repl-maximum-source-size configuration)
   :maximum-output-size    (config :management-repl-maximum-output-size configuration)
   :queue-capacity         (config :management-repl-queue-capacity configuration)
   :maximum-clients        (config :management-repl-maximum-clients configuration)
   :error-function         #'management-repl--report))

(-> management-repl--call (function) t)
(defun management-repl--call (function)
  "Call FUNCTION, resignaling endpoint failures as MANAGEMENT-REPL-ERROR."
  (handler-case (funcall function)
    (eval-endpoint-error (condition)
      (error 'management-repl-error
             :message   (daemon-error-message condition)
             :operation (daemon-error-operation condition)
             :reason    (eval-endpoint-error-reason condition)))))

(-> management-repl--report (eval-endpoint-error) null)
(defun management-repl--report (condition)
  "Warn about credential CONDITION met while authenticating a management client."
  (warn "Management authentication configuration failure (~A): ~A"
        (eval-endpoint-error-reason condition)
        (daemon-error-message condition))
  nil)
