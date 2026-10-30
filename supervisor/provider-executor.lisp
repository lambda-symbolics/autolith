;;;; -- Trusted ChatGPT HTTP Execution --

(in-package #:autolith-supervisor)

(defclass chatgpt-account ()
  ((credential-manager :initarg :credential-manager :reader chatgpt-account-credential-manager
                       :documentation "Trusted credential manager, never supplied by the peer.")
   (endpoint :initarg :endpoint :reader chatgpt-account-endpoint
             :documentation "Trusted complete Responses endpoint."))
  (:documentation "Parent-owned ChatGPT account and HTTP routing policy."))

(defclass chatgpt-wire-provider (cl-llm-provider-api:responses-api-provider)
  ()
  (:documentation "ChatGPT wire policy over the shared Responses encoder and parser."))

(defmethod cl-llm-provider-api:provider-responses-instructions-placement ((provider chatgpt-wire-provider))
  "Put ChatGPT's stable instructions in the required top-level field."
  (declare (ignore provider))
  ':top-level)

(defmethod cl-llm-provider-api:provider-account-label ((provider chatgpt-wire-provider))
  "Name the trusted account family in sanitized library failures."
  (declare (ignore provider))
  "ChatGPT")

(define-condition provider-executor-error (error)
  ((reason :initarg :reason :reader provider-executor-error-reason
           :documentation "Fixed failure category without transport or credential text."))
  (:report (lambda (condition stream)
             (format stream "Parent provider execution failed (~A)."
                     (provider-executor-error-reason condition))))
  (:documentation "A trusted executor failed admission or strict transport cleanup."))

(defclass provider-http-state ()
  ((lock :initform (bordeaux-threads:make-lock "provider HTTP state")
         :reader provider-http-state-lock :documentation "Short lifecycle lock.")
   (ready :initform (bordeaux-threads:make-condition-variable)
          :reader provider-http-state-ready :documentation "Lifecycle change notification.")
   (cancelled :initform nil :accessor provider-http-state-cancelled
              :documentation "Permanent cancellation latch.")
   (opening :initform nil :accessor provider-http-state-opening
            :documentation "An open operation has not returned yet.")
   (closing :initform nil :accessor provider-http-state-closing
            :documentation "Exactly one strict close operation is running.")
   (stream :initform nil :accessor provider-http-state-stream
           :documentation "Transport requiring a confirmed close.")
   (failed :initform nil :accessor provider-http-state-failed
           :documentation "Permanent strict cleanup failure latch."))
  (:documentation "One invocation's transport, including gaps before open and between retries."))

(declaim (ftype (function (&key (:credential-manager cl-rfc8628:credential-manager)
                               (:endpoint string)) chatgpt-account) make-chatgpt-account))

(defun make-chatgpt-account (&key credential-manager
                                (endpoint "https://chatgpt.com/backend-api/codex/responses"))
  "Bind a trusted credential manager and complete HTTP endpoint."
  (check-type credential-manager cl-rfc8628:credential-manager)
  (unless (and (stringp endpoint) (plusp (length endpoint)))
    (error 'provider-executor-error :reason ':invalid-endpoint))
  (make-instance 'chatgpt-account :credential-manager credential-manager :endpoint (copy-seq endpoint)))

(declaim (ftype (function (provider-http-state) null) provider-http--check))

(defun provider-http--check (state)
  "Reject cancelled invocations and unconfirmed transport cleanup. Caller holds the lock."
  (when (provider-http-state-failed state)
    (error 'provider-executor-error :reason ':cleanup-failed))
  (when (provider-http-state-cancelled state)
    (error 'model-service-cancelled))
  nil)

(declaim (ftype (function (provider-http-state function) null) provider-http--close))

(defun provider-http--close (state closer)
  "Serialize strict close outside the lifecycle lock and remember all failures."
  (let ((stream nil) (failed nil))
    (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
      (loop while (provider-http-state-closing state)
            do (bordeaux-threads:condition-wait (provider-http-state-ready state)
                                               (provider-http-state-lock state) :timeout 0.05))
      (when (provider-http-state-failed state)
        (error 'provider-executor-error :reason ':cleanup-failed))
      (setf stream (provider-http-state-stream state))
      (when stream (setf (provider-http-state-closing state) t)))
    (when stream
      (unwind-protect
           (handler-case
               (progn
                 (setf failed t)
                 (funcall closer stream)
                 (setf failed (open-stream-p stream)))
             (error () (setf failed t)))
        (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
          (setf (provider-http-state-closing state) nil
                (provider-http-state-failed state) failed)
          (unless failed (setf (provider-http-state-stream state) nil))
          (bordeaux-threads:condition-notify (provider-http-state-ready state)))))
    (when failed (error 'provider-executor-error :reason ':cleanup-failed)))
  nil)

(declaim (ftype (function (stream) t) provider-http--close-stream))

(defun provider-http--close-stream (stream)
  "Close the actual transport without pooling and without suppressing close errors."
  (close stream :abort t))

(declaim (ftype (function (provider-http-state function) null) provider-http--abort))

(defun provider-http--abort (state closer)
  "Latch cancellation before open, close a published stream, and await in-flight open."
  (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
    (setf (provider-http-state-cancelled state) t)
    (bordeaux-threads:condition-notify (provider-http-state-ready state)))
  (loop
    (provider-http--close state closer)
    (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
      (when (and (not (provider-http-state-opening state))
                 (not (provider-http-state-closing state))
                 (not (provider-http-state-stream state)))
        (return))
      (bordeaux-threads:condition-wait (provider-http-state-ready state)
                                     (provider-http-state-lock state) :timeout 0.05)))
  nil)

(declaim (ftype (function (provider-http-state function function) (values t t t &optional))
                provider-http--open))

(defun provider-http--open (state opener closer)
  "Publish each returned transport before observing cancellation, including a late open."
  (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
    (provider-http--check state)
    (setf (provider-http-state-opening state) t))
  (unwind-protect
       (multiple-value-bind (stream status headers) (funcall opener)
         (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
           (setf (provider-http-state-stream state) (and (streamp stream) stream)))
         (handler-case
             (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
               (provider-http--check state))
           (error (condition)
             (provider-http--close state closer)
             (error condition)))
         (values stream status headers))
    (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
      (setf (provider-http-state-opening state) nil)
      (bordeaux-threads:condition-notify (provider-http-state-ready state)))))

(declaim (ftype (function (provider-http-state real function) null) provider-http--sleep))

(defun provider-http--sleep (state seconds clock)
  "Wait through a retry delay with immediate cancellation wakeup."
  (let ((end (+ (funcall clock) seconds)))
    (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
      (loop
        (provider-http--check state)
        (let ((remaining (- end (funcall clock))))
          (unless (plusp remaining) (return))
          (bordeaux-threads:condition-wait (provider-http-state-ready state)
                                         (provider-http-state-lock state) :timeout remaining)))))
  nil)

(defclass chatgpt-executor ()
  ((accounts :initarg :accounts :reader chatgpt-executor-accounts
             :documentation "Private EQ profile-to-account registry.")
   (session-id :initarg :session-id :reader chatgpt-executor-session-id
               :documentation "Trusted session identity for affinity headers.")
   (user-agent :initarg :user-agent :reader chatgpt-executor-user-agent
               :documentation "Trusted application user agent.")
   (maximum-attempts :initarg :maximum-attempts :reader chatgpt-executor-maximum-attempts
                     :documentation "Total HTTP attempt budget, including authentication recovery.")
   (opener :initarg :opener :reader chatgpt-executor-opener
           :documentation "Synchronous HTTP open operation.")
   (closer :initarg :closer :reader chatgpt-executor-closer
           :documentation "Strict actual-transport close operation.")
   (clock :initarg :clock :reader chatgpt-executor-clock
          :documentation "The same monotonic clock used by the model service.")
   (delay-function :initarg :delay-function :reader chatgpt-executor-delay-function
                   :documentation "Optional library retry-delay policy."))
  (:documentation "Trusted execution configuration. Holds no credential values."))

(declaim (ftype (function (provider-http-state model-invocation function) null)
                provider-http--check-invocation))

(defun provider-http--check-invocation (state invocation clock)
  "Check cancellation, cleanup, and the total invocation deadline."
  (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
    (provider-http--check state))
  (unless (< (funcall clock) (model-invocation-deadline invocation))
    (error 'provider-executor-error :reason ':deadline))
  nil)

(declaim (ftype (function (function) (values t t t &optional)) provider-http--http-values))

(defun provider-http--http-values (opener)
  "Return HTTP error bodies through the same owned-stream path as successful opens."
  (handler-case (funcall opener)
    (dexador.error:http-request-failed (condition)
      (values (dexador.error:response-body condition)
              (dexador.error:response-status condition)
              (dexador.error:response-headers condition)))))

(declaim (ftype (function (&key (:executor chatgpt-executor) (:state provider-http-state)
                               (:invocation model-invocation) (:account chatgpt-account)
                               (:request hash-table) (:credentials cl-rfc8628:oauth-credentials)
                               (:turn-state (or null string)) (:callback function)) t)
                chatgpt-executor--request))

(defun chatgpt-executor--request (&key executor state invocation account request credentials
                                      turn-state callback)
  "Run the shared request engine and verify strict cleanup outside its suppressed cleanup hook."
  (let ((clock    (chatgpt-executor-clock executor))
        (closer   (chatgpt-executor-closer executor))
        (provider (make-instance 'chatgpt-wire-provider)))
    (unwind-protect
         (cl-llm-provider-api::provider--call-with-transport-normalization
          (lambda ()
            (cl-llm-provider-api::provider-execute-request
             provider request :secrets (cl-rfc8628:oauth-credentials-secret-values credentials)
             :event-callback           (lambda (event)
                                         (provider-http--check-invocation state invocation clock)
                                         (funcall callback event))
             :transport
             (lambda (encoded)
               (provider-http--open
                state
                (lambda ()
                  (provider-http--http-values
                   (lambda ()
                     (provider-http--check-invocation state invocation clock)
                     (funcall (chatgpt-executor-opener executor)
                              (chatgpt-account-endpoint account) encoded credentials
                              :session-id       (chatgpt-executor-session-id executor)
                              :turn-state turn-state :user-agent (chatgpt-executor-user-agent executor)
                              :deadline-seconds (- (model-invocation-deadline invocation) (funcall clock))))))
                closer))
             :call-with-deadline
             (lambda (function)
               (provider-http--check-invocation state invocation clock)
               (cl-llm-provider-api:provider-call-with-response-deadline
                (- (model-invocation-deadline invocation) (funcall clock)) function))
             :cleanup                  (lambda (stream)
                                         (declare (ignore stream))
                                         (provider-http--close state closer)))))
      ;; PROVIDER-EXECUTE-REQUEST suppresses cleanup errors, including our hook's errors.
      (provider-http--close state closer))))

(declaim (ftype (function (chatgpt-executor provider-http-state model-invocation
                           &key (:callback function)) t) chatgpt-executor--run))

(defun chatgpt-executor--run (executor state invocation &key callback)
  "Encode one admitted projection and share one retry budget across HTTP and refresh recovery."
  (destructuring-bind (projection tools profile kind summary-p continuation turn-state)
      (model-invocation-inputs invocation)
    (declare (ignore summary-p))
    (unless (and (eq kind ':generate) (eq (model-profile-family profile) ':responses))
      (error 'provider-executor-error :reason ':unsupported))
    (let* ((account          (gethash profile (chatgpt-executor-accounts executor)))
           (maximum-attempts (chatgpt-executor-maximum-attempts executor))
           (clock            (chatgpt-executor-clock executor))
           (provider         (make-instance 'chatgpt-wire-provider))
           (attempts 0) (refresh nil) (refreshed nil) (retry-announced nil))
      (unless (typep account 'chatgpt-account)
        (error 'provider-executor-error :reason ':unknown-account))
      (labels ((check ()
                 "Check the permanent lifecycle state and total deadline."
                 (provider-http--check-invocation state invocation clock))

               (observe (event)
                 "Translate the library's paired retry notices to one actual next-attempt announcement."
                 (check)
                 (if (typep event 'cl-llm-provider-api:provider-retry-event)
                     (unless retry-announced
                       (when (>= attempts maximum-attempts)
                         (error 'provider-executor-error :reason ':attempt-limit))
                       (setf retry-announced t)
                       (funcall callback
                                (make-instance 'cl-llm-provider-api:provider-retry-event
                                               :attempt (1+ attempts) :maximum-attempts maximum-attempts
                                               :delay (cl-llm-provider-api:provider-retry-event-delay event))))
                     (funcall callback event)))

               (attempt (event-callback)
                 "Consume one budget unit, including an authentication retry."
                 (check)
                 (when (>= attempts maximum-attempts)
                   (error 'provider-executor-error :reason ':attempt-limit))
                 (incf attempts)
                 (setf retry-announced nil)
                 (cl-rfc8628:call-with-credentials
                  (chatgpt-account-credential-manager account)
                  (lambda (credentials)
                    (setf refresh nil)
                    (check)
                    (let ((request (cl-llm-provider-api:provider-request-object provider projection tools)))
                      (when continuation (setf (gethash "previous_response_id" request) continuation))
                      (unless (gethash "include" request)
                        (setf (gethash "include" request) #("reasoning.encrypted_content")))
                      (setf (gethash "prompt_cache_key" request) (chatgpt-executor-session-id executor))
                      (prog1
                          (chatgpt-executor--request
                           :executor executor :state state :invocation invocation :account account
                           :request request :credentials credentials :turn-state turn-state
                           :callback event-callback)
                        (check))))
                  :force-refresh refresh)))
        (check)
        (funcall callback (make-instance 'cl-llm-provider-api:provider-retry-event
                                        :attempt 1 :maximum-attempts maximum-attempts :delay 0))
        (unwind-protect
             (cl-llm-provider-api:call-with-streaming-retries
              (lambda (event-callback)
                (handler-case (attempt event-callback)
                  (cl-llm-provider-api:provider-unauthorized (condition)
                    (unless (and (not refreshed) (< attempts maximum-attempts)
                                 (cl-rfc8628:credential-manager-refreshable-p
                                  (chatgpt-account-credential-manager account)))
                      (error condition))
                    (setf refresh t refreshed t)
                    ;; Re-enter the same engine, not a nested authentication retry ladder.
                    (error 'cl-llm-provider-api:provider-retryable-error
                           :message "Refresh ChatGPT credentials before retry." :status 401))))
              #'observe :maximum-retries (1- maximum-attempts)
              :maximum-streaming-retries (min 2 (1- maximum-attempts))
              :sleep-function            (lambda (seconds)
                                           (check)
                                           (provider-http--sleep
                                            state (min seconds (- (model-invocation-deadline invocation)
                                                                  (funcall clock))) clock)
                                           (check))
              :delay-function            (chatgpt-executor-delay-function executor))
          (provider-http--close state (chatgpt-executor-closer executor)))))))

(declaim (ftype (function (&key (:accounts hash-table) (:session-id string) (:user-agent string)
                               (:maximum-attempts (integer 1)) (:opener function) (:closer function)
                               (:clock function) (:delay-function (or null function)))
                         (values function function &optional)) make-chatgpt-model-executor))

(defun make-chatgpt-model-executor (&key accounts session-id (user-agent "autolith")
                                       (maximum-attempts 8) (opener #'chatgpt-open-response-stream)
                                       (closer #'provider-http--close-stream)
                                       (clock #'session-runtime--clock) delay-function)
  "Return synchronous executor and strict abort functions for MAKE-MODEL-SERVICE.
ACCOUNTS maps trusted profile objects by EQ to CHATGPT-ACCOUNT objects. Configure
the same MAXIMUM-ATTEMPTS and CLOCK on the service. All attempts, including
credential recovery, share this budget. Abort waits for in-flight opens, closes
the actual stream, and reports cleanup failure instead of confirming it."
  (check-type accounts hash-table)
  (check-type session-id string)
  (check-type maximum-attempts (integer 1))
  (unless (eq (hash-table-test accounts) 'eq)
    (error 'provider-executor-error :reason ':invalid-account-registry))
  (let ((states   (make-hash-table :test #'eq :weakness ':key))
        (lock     (bordeaux-threads:make-lock "provider invocations"))
        (executor (make-instance 'chatgpt-executor :accounts accounts :session-id (copy-seq session-id)
                                :user-agent (copy-seq user-agent) :maximum-attempts maximum-attempts
                                :opener opener :closer closer :clock clock :delay-function delay-function)))
    (labels ((state-for (invocation)
               "Create the cancellation latch even when abort runs before execution."
               (bordeaux-threads:with-lock-held (lock)
                 (or (gethash invocation states)
                     (setf (gethash invocation states) (make-instance 'provider-http-state))))))
      (values (lambda (invocation callback)
                (chatgpt-executor--run executor (state-for invocation) invocation :callback callback))
              (lambda (invocation) (provider-http--abort (state-for invocation) closer))))))
