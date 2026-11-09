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

(-> management-repl-start (application) (option eval-endpoint))
(defun management-repl-start (application)
  "Start APPLICATION's configured management endpoint when enabled and return it."
  (let ((configuration (application-configuration application)))
    (cond
      ((not (config :management-repl-enabled-p configuration))
       nil)
      ((application-management-repl-endpoint application))
      (t
       (let ((endpoint (management-repl--call
                        (lambda ()
                          (eval-endpoint-start (management-repl--endpoint configuration))))))
         (setf (application-management-repl-endpoint application) endpoint))))))

(-> management-repl-stop (application) null)
(defun management-repl-stop (application)
  "Stop APPLICATION's management endpoint, signaling when its threads do not quiesce."
  (let ((endpoint (application-management-repl-endpoint application)))
    (when endpoint
      (management-repl--call (lambda () (eval-endpoint-stop endpoint)))
      (setf (application-management-repl-endpoint application) nil)))
  nil)

(-> management-repl-transfer (application application) null)
(defun management-repl-transfer (old-application new-application)
  "Move the running management endpoint from OLD-APPLICATION to NEW-APPLICATION."
  (let ((endpoint (application-management-repl-endpoint old-application)))
    (when endpoint
      (setf (application-management-repl-endpoint new-application) endpoint
            (application-management-repl-endpoint old-application) nil)))
  nil)

(-> application-call-with-management-repl-quiesced (application function) t)
(defun application-call-with-management-repl-quiesced (application function)
  "Call FUNCTION with APPLICATION's management endpoint stopped, then restart it."
  (let ((endpoint (application-management-repl-endpoint application)))
    (when (and endpoint (eq (current-thread) (eval-endpoint-evaluator-thread endpoint)))
      (error 'management-repl-error
             :message   "A management evaluation cannot checkpoint its own evaluator."
             :operation ':checkpoint
             :reason    ':evaluator))
    (when endpoint
      (management-repl-stop application))
    (unwind-protect
         (funcall function)
      (when endpoint
        (management-repl-start application)))))

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
