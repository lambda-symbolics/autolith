;;;; -- Optional Parent Launch Composition --

(in-package #:autolith-supervisor)

(define-condition parent-launch-error (error)
  ((reason :initarg :reason :reader parent-launch-error-reason
           :documentation "A fixed launch failure category without peer data."))
  (:documentation "The optional parent launch could not complete safely.")
  (:report (lambda (condition stream)
             (format stream "Parent launch failed: ~A." (parent-launch-error-reason condition)))))

(defclass parent-launch ()
  ((process :initform nil :accessor parent-launch-process
            :documentation "The owned native child scope.")
   (process-closer :initarg :process-closer :initform #'private-process-close
                   :reader        parent-launch-process-closer
                   :documentation "Trusted cleanup function returning native scope evidence.")
   (close-lock :initform (bordeaux-threads:make-lock "parent launch close")
               :reader parent-launch-close-lock :documentation "Serialize complete cleanup calls.")
   (runtime :initform nil :accessor parent-launch-runtime
            :documentation "The session dispatcher, read only by READER.")
   (store :initarg :store :reader parent-launch-store
          :documentation "The parent-owned durable session journal.")
   (replay-store :initarg :replay-store :reader parent-launch-replay-store
                 :documentation "The parent-owned model replay journal.")
   (terminal :initarg :terminal :reader parent-launch-terminal
             :documentation "The trusted presentation and input adapter.")
   (reader :initform nil :accessor parent-launch-reader
           :documentation "The sole post-handshake connection reader.")
   (presenter :initform nil :accessor parent-launch-presenter
              :documentation "The bounded output consumer, separate from storage ownership.")
   (lock :initform (bordeaux-threads:make-lock "parent launch")
         :reader parent-launch-lock :documentation "Protect short lifecycle and output updates.")
   (ready :initform (bordeaux-threads:make-condition-variable)
          :reader parent-launch-ready :documentation "Notify lifecycle and output waiters.")
   (output :initform nil :accessor parent-launch-output
           :documentation "At most 128 pending presentation events.")
   (output-bytes :initform 0 :accessor parent-launch-output-bytes
                 :documentation "Pending output bytes, bounded by 65536.")
   (presenting :initform nil :accessor parent-launch-presenting
               :documentation "A presentation event is currently outside the queue.")
   (turn :initform nil :accessor parent-launch-turn
         :documentation "The sole submitted turn, until its result is consumed.")
   (outcome :initform nil :accessor parent-launch-outcome
            :documentation "The durable terminal outcome of TURN.")
   (stopped :initform nil :accessor parent-launch-stopped
            :documentation "ReadyToStop was durably accepted.")
   (stopping :initform nil :accessor parent-launch-stopping
             :documentation "Reject new input while accepted output drains.")
   (closing :initform nil :accessor parent-launch-closing
            :documentation "No new input or output may be admitted.")
   (failure :initform nil :accessor parent-launch-failure
            :documentation "The first fixed failure category.")
   (evidence :initform nil :accessor parent-launch-evidence
             :documentation "Final native-scope cleanup evidence."))
  (:documentation "Optional explicit launch. It does not select a default or enforce OS policy."))

(declaim (ftype (function (parent-launch keyword) null) parent-launch--fail))

(defun parent-launch--fail (launch reason)
  "Latch a fixed failure and wake waiters without waiting for transport or storage."
  (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
    (unless (parent-launch-failure launch)
      (setf (parent-launch-failure launch) reason))
    (bordeaux-threads:condition-notify (parent-launch-ready launch)))
  (when (parent-launch-runtime launch)
    (session-runtime-disconnect (parent-launch-runtime launch)))
  nil)

(declaim (ftype (function (parent-launch keyword string) null) parent-launch--output))

(defun parent-launch--output (launch channel text)
  "Admit bounded presentation work without doing I/O on the storage owner."
  (let ((overflow nil))
    (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
      (let ((bytes (session-output--bytes text)))
        (if (or (parent-launch-closing launch)
                (>= (length (parent-launch-output launch)) 128)
                (> (+ bytes (parent-launch-output-bytes launch)) 65536))
            (setf overflow t)
            (progn
              (setf (parent-launch-output launch)
                    (nconc (parent-launch-output launch) (list (list channel text))))
              (incf (parent-launch-output-bytes launch) bytes)
              (bordeaux-threads:condition-notify (parent-launch-ready launch))))))
    (when overflow
      (parent-launch--fail launch ':output-overflow)
      (error 'parent-launch-error :reason ':output-overflow)))
  nil)

(declaim (ftype (function (parent-launch &key (:kind keyword) (:turn-id t) (:outcome t)) null)
                parent-launch--lifecycle))

(defun parent-launch--lifecycle (launch &key kind turn-id outcome)
  "Observe only lifecycle results already committed by the session owner."
  (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
    (case kind
      (:turn-finished
       (when (equal turn-id (parent-launch-turn launch))
         (setf (parent-launch-outcome launch) outcome)))
      (:stopped
       (setf (parent-launch-stopped launch) t)))
    (bordeaux-threads:condition-notify (parent-launch-ready launch)))
  nil)

(declaim (ftype (function (parent-launch) null) parent-launch--present))

(defun parent-launch--present (launch)
  "Render bounded output outside the session owner; fail closed on renderer errors."
  (handler-case
      (let ((render (parent-terminal-output-callback (parent-launch-terminal launch))))
        (loop
          (let ((event
                  (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
                    (loop while (and (null (parent-launch-output launch))
                                     (not (parent-launch-closing launch)))
                          do (bordeaux-threads:condition-wait
                              (parent-launch-ready launch) (parent-launch-lock launch)
                              :timeout 0.05))
                    (when (parent-launch-closing launch)
                      (return-from parent-launch--present nil))
                    (let ((event (pop (parent-launch-output launch))))
                      (decf (parent-launch-output-bytes launch)
                            (session-output--bytes (second event)))
                      (setf (parent-launch-presenting launch) t)
                      event))))
            (unwind-protect
                 (funcall render (first event) (second event))
              (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
                (setf (parent-launch-presenting launch) nil)
                (bordeaux-threads:condition-notify (parent-launch-ready launch)))))))
    (error ()
      (parent-launch--fail launch ':presentation-failed)))
  nil)

(declaim (ftype (function (parent-launch) null) parent-launch--read))

(defun parent-launch--read (launch)
  "Run the sole post-handshake reader. Never read this connection elsewhere."
  (handler-case
      (loop
        (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
          (when (or (parent-launch-closing launch) (parent-launch-stopped launch))
            (return)))
        (session-runtime-receive (parent-launch-runtime launch)))
    (error ()
      (unless (parent-launch-stopped launch)
        (parent-launch--fail launch ':connection-failed))))
  nil)

(declaim (ftype (function (parent-launch function real) boolean) parent-launch--wait))

(defun parent-launch--wait (launch predicate timeout)
  "Wait for a short state predicate, failure, or a monotonic deadline."
  (let ((deadline (+ (session-runtime--clock) timeout)))
    (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
      (loop
        (when (funcall predicate)
          (return t))
        (when (or (parent-launch-failure launch)
                  (>= (session-runtime--clock) deadline))
          (return nil))
        (bordeaux-threads:condition-wait
         (parent-launch-ready launch) (parent-launch-lock launch)
         :timeout (min 0.05 (max 0 (- deadline (session-runtime--clock)))))))))

(declaim (ftype (function (parent-launch string string
                           &key (:timeout real) (:application t)) keyword)
                parent-launch-turn-run))

(defun parent-launch-turn-run (launch turn-id text &key (timeout 120) application)
  "Persist input, send one TurnStart, and await its durable outcome. Never retry on loss."
  (unless (plusp timeout)
    (error 'parent-launch-error :reason ':invalid-timeout))
  (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
    (when (or (parent-launch-turn launch) (parent-launch-closing launch)
              (parent-launch-stopping launch) (parent-launch-failure launch))
      (error 'parent-launch-error :reason ':not-accepting-input))
    (setf (parent-launch-turn launch) turn-id
          (parent-launch-outcome launch) nil))
  (handler-case
      (session-runtime-start-turn (parent-launch-runtime launch) turn-id text :application application)
    (error ()
      (parent-launch--fail launch ':turn-send-uncertain)
      (error 'parent-launch-error :reason ':turn-send-uncertain)))
  (unless (parent-launch--wait launch (lambda () (parent-launch-outcome launch)) timeout)
    (parent-launch--fail launch ':turn-outcome-uncertain)
    (error 'parent-launch-error :reason ':turn-outcome-uncertain))
  (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
    (prog1 (parent-launch-outcome launch)
      (setf (parent-launch-turn launch) nil
            (parent-launch-outcome launch) nil))))

(declaim (ftype (function (parent-launch string string
                           &key (:kind keyword) (:timeout real) (:cursor integer) (:image-paths list) (:job t))
                         (values keyword t))
                parent-launch-application-run))

(defun parent-launch-application-run
    (launch identifier source &key (kind ':command) (timeout 120) (cursor 0) image-paths job)
  "Execute application input in a fresh root or a durable nested scope."
  (let* ((runtime (parent-launch-runtime launch))
         (action  (parent-proto:make-application-action :kind kind :source source :cursor cursor
                     :image-paths image-paths :job (when job (protocol-data->map job)))))
    (protocol-application-action-validate action)
    (if (session-runtime-turn-id runtime)
        (progn
          (session-runtime-application-steer runtime identifier action)
          (unless (parent-launch--wait launch
                   (lambda ()
                     (let ((scope (gethash identifier (session-runtime-application-scopes runtime))))
                       (and scope (session-application-scope-result scope)))) timeout)
            (error 'parent-launch-error :reason ':application-outcome-uncertain))
          (values ':completed
            (session-application-scope-result
              (gethash identifier (session-runtime-application-scopes runtime)))))
        (let ((outcome (parent-launch-turn-run launch identifier source :timeout timeout :application action)))
          (values outcome (session-runtime-application-result runtime))))))

(declaim (ftype (function (parent-launch &key (:reason string)) boolean) parent-launch-cancel))

(defun parent-launch-cancel (launch &key (reason "Cancelled by user."))
  "Cancel the current child scope without claiming that its effects were undone."
  (let ((turn (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
                (parent-launch-turn launch))))
    (if turn
        (session-runtime-cancel-turn (parent-launch-runtime launch) turn :reason reason)
        nil)))

(declaim (ftype (function (parent-launch function &key (:turn-timeout real)) keyword)
                parent-launch-run-input))

(defun parent-launch-run-input (launch turn-id-source &key (turn-timeout 120))
  "Run trusted terminal input serially. Assign IDs in the parent and wait for durable outcomes."
  (unwind-protect
       (parent-terminal-run-input
        (parent-launch-terminal launch)
        (lambda (text)
          (parent-launch-turn-run launch (funcall turn-id-source) text :timeout turn-timeout))
        :cancelled (lambda ()
                     (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
                       (not (null (or (parent-launch-closing launch)
                                      (parent-launch-stopping launch) (parent-launch-failure launch)))))))
    (parent-launch-close launch)))

(declaim (ftype (function (parent-launch &key (:grace-seconds real)) list) parent-launch-close))

(defun parent-launch-close (launch &key (grace-seconds 2))
  "Request quiescence, then stop the owned native scope and return exact evidence."
  (bordeaux-threads:with-lock-held ((parent-launch-close-lock launch))
   (or (parent-launch-evidence launch)
       (let ((graceful nil))
        (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
          (setf (parent-launch-stopping launch) t))
        (when (and (parent-launch-runtime launch) (not (parent-launch-failure launch)))
          (handler-case
              (progn
                (session-runtime-prepare-stop
                 (parent-launch-runtime launch)
                 :grace-milliseconds (ceiling (* 1000 grace-seconds)))
                (setf graceful
                      (parent-launch--wait launch (lambda () (parent-launch-stopped launch))
                                           grace-seconds)))
            (error ()
              (parent-launch--fail launch ':stop-failed))))
        (unless (or (null (parent-launch-presenter launch))
                    (parent-launch--wait launch
                                         (lambda () (and (null (parent-launch-output launch))
                                                         (not (parent-launch-presenting launch))))
                                         grace-seconds))
          (parent-launch--fail launch ':presentation-incomplete))
        (parent-terminal-close (parent-launch-terminal launch))
        (bordeaux-threads:with-lock-held ((parent-launch-lock launch))
          (setf (parent-launch-closing launch) t)
          (bordeaux-threads:condition-notify (parent-launch-ready launch)))
        (let ((evidence (when (parent-launch-process launch)
                          (funcall (parent-launch-process-closer launch) (parent-launch-process launch)))))
          (when (parent-launch-runtime launch)
            (session-runtime-disconnect (parent-launch-runtime launch))
            (session-runtime-await-close (parent-launch-runtime launch)))
          (dolist (thread (list (parent-launch-reader launch) (parent-launch-presenter launch)))
            (when thread
              (multiple-value-bind (result reason)
                  (sb-thread:join-thread thread :timeout 5 :default ':join-failed)
                (declare (ignore result))
                (when reason
                  (error 'parent-launch-error :reason ':thread-cleanup-failed)))))
          (setf (parent-launch-evidence launch)
                (append evidence (list :graceful-stop-p graceful))))))))

(declaim (ftype (function (&key (:command list) (:environment list) (:directory t)
                               (:session-id string) (:instance-id string)
                               (:session-path t) (:replay-path t) (:configuration t)
                               (:profiles hash-table) (:accounts hash-table)
                               (:terminal parent-terminal) (:startup-timeout real)
                               (:model-timeout real) (:maximum-attempts integer)
                               (:policy-revision integer) (:active-grants list)
                               (:process-opener function) (:process-closer function)
                               (:startup-policy (or null function)) (:executor-factory function)
                               (:record-service t) (:access-service t) (:knowledge-service t)
                               (:application-event-callback (or null function)) (:application-notification-callback (or null function))
                                (:application-service t) (:application-callback (or null function))
                               (:lifecycle-callback (or null function)))
                         (values parent-launch &optional))
                parent-launch-open))

(defun parent-launch-open (&key command (environment nil environment-p) directory
                               session-id instance-id session-path replay-path configuration
                               profiles accounts terminal (startup-timeout 30)
                               (model-timeout 120) (maximum-attempts 1)
                               (policy-revision 0) active-grants
                               (process-opener #'private-process-open)
                               (process-closer #'private-process-close) startup-policy record-service access-service
                               (executor-factory #'make-chatgpt-model-executor) lifecycle-callback knowledge-service
                               application-event-callback application-notification-callback application-service application-callback)
  "Launch an explicit child and compose parent-owned provider service, journals, and terminal.
COMMAND must enter a fresh agent image through its child-only bootstrap. Supply an
explicit approved ENVIRONMENT. No credential or parent Lisp crosses the connection.
The caller must apply host access policy before launch. This API does not grant it."
  (unless (and environment-p (typep terminal 'parent-terminal)
               (hash-table-p profiles) (hash-table-p accounts))
    (error 'parent-launch-error :reason ':invalid-configuration))
  (let* ((store    (session-store-open session-path session-id))
         (replay   (model-replay-store-open replay-path session-id))
         (launch   (make-instance 'parent-launch :store store :replay-store replay :terminal terminal
                                                :process-closer process-closer))
         (complete nil))
    (unwind-protect
         (multiple-value-bind (executor abort)
            (funcall executor-factory :accounts accounts :session-id session-id
                                      :maximum-attempts maximum-attempts)
           (let* ((service  (make-model-service :profiles profiles :replay-store replay
                                               :executor executor :abort abort
                                               :timeout model-timeout :maximum-attempts maximum-attempts
                                               :nonblocking-output t))
                  (features (append '("session.v1" "model.v1" "model-items-v1" "application.v1")
                                    (when record-service '("records.v1"))
                                    (when (session-record-scopes-p record-service) '("record-scopes-v1"))
                                    (when knowledge-service '("knowledge.v1"))
                                    (when access-service '("access.v1"))))
                  (resume   (session-store-read store (parent-proto:make-session-read :limit 128)))
                  (welcome  (parent-proto:make-welcome
                             :session-id session-id :policy-revision policy-revision
                             :active-grants active-grants :features features :configuration configuration
                             :resume resume
                             :limits (parent-proto:make-limits
                                      :max-frame-bytes 8388608 :max-value-depth 32
                                      :max-collection-entries 65536 :max-in-flight-requests 64))))
             (setf (parent-launch-process launch)
                   (funcall process-opener
                    command :environment environment :directory directory :instance-id instance-id
                    :startup-timeout startup-timeout
                    :startup
                    (lambda (process)
                      (let* ((stream     (private-process-connection process))
                             (connection (make-instance 'parent-connection :input stream :output stream
                                                       :instance-id instance-id)))
                        (when startup-policy
                          (multiple-value-bind (revision grants) (funcall startup-policy process)
                            (unless (and (typep revision '(unsigned-byte 64)) (listp grants))
                              (error 'parent-launch-error :reason ':invalid-startup-policy))
                            (setf (parent-proto:welcome.policy-revision welcome) revision
                                  (parent-proto:welcome.active-grants welcome) grants)))
                        (parent-connection-handshake connection welcome :timeout startup-timeout)
                        (setf (parent-launch-runtime launch)
                              (session-runtime-open
                               store connection :features features :resume-page resume :model-service service
                               :record-service record-service :access-service access-service
                               :knowledge-service                   knowledge-service
                               :application-service                 application-service
                               :application-event-callback          application-event-callback
                                 :application-notification-callback application-notification-callback
                                 :application-callback              application-callback
                               :output-callback                     (lambda (channel text) (parent-launch--output launch channel text))
                               :lifecycle-callback
                               (lambda (kind turn-id outcome)
                                (parent-launch--lifecycle launch :kind kind :turn-id turn-id :outcome outcome)
                                (when lifecycle-callback
                                  (funcall lifecycle-callback kind turn-id outcome)))))))))
             (setf (parent-launch-presenter launch)
                   (bordeaux-threads:make-thread (lambda () (parent-launch--present launch))
                                                 :name "parent launch presentation")
                   (parent-launch-reader launch)
                   (bordeaux-threads:make-thread (lambda () (parent-launch--read launch))
                                                 :name "parent launch sole reader"))
             (setf complete t)
             (values launch)))
      (unless complete
        (parent-launch-close launch :grace-seconds 0)))))

(declaim (ftype (function (parent-launch string string &key (:kind keyword) (:image-paths list) (:job t) (:cursor integer)) integer) parent-launch-steer))

(defun parent-launch-steer (launch identifier source &key (kind ':message) image-paths job (cursor 0))
  "Persist one active input and send its typed scope to the child."
  (session-runtime-application-steer (parent-launch-runtime launch) identifier
    (parent-proto:make-application-action :kind kind :source source :cursor cursor
      :image-paths image-paths :job (when job (protocol-data->map job)))))
