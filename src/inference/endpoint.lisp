(in-package #:autolith)

;;;; -- Recursive Inference Host Endpoint --

(defparameter *rlm-endpoint-map-maximum-tasks* 64
  "The most tasks one proxied environment rlm-map call may fan out.")

(defclass rlm-endpoint (image-daemon:daemon-runtime)
  ((provider
    :initarg :provider
    :reader rlm-endpoint--provider
    :type model-provider
    :documentation "The provider serving proxied sub-inferences.")
   (configuration
    :initarg :configuration
    :reader rlm-endpoint--configuration
    :type configuration
    :documentation "The configuration serving proxied sub-inferences.")
   (budget
    :initarg :budget
    :reader rlm-endpoint--budget
    :type rlm-budget
    :documentation "The root budget subtree every proxied call descends.")
   (ledger
    :initarg :ledger
    :initform nil
    :reader rlm-endpoint--ledger
    :type (option function)
    :documentation "An optional function recording one plist per served operation.")
   (activity-callback
    :initarg :activity-callback
    :initform nil
    :reader rlm-endpoint--activity-callback
    :type (option function)
    :documentation "An optional function receiving compact proxied activity.")
   (activity-lock
    :initform (make-recursive-lock "Autolith inference endpoint activity")
    :reader rlm-endpoint--activity-lock
    :type t
    :documentation "The lock serializing activity publication with revocation.")
   (activity-enabled-p
    :initform t
    :accessor rlm-endpoint--activity-enabled-p
    :type boolean
    :documentation "True while proxied operations may publish activity.")
   (final-value
    :initform nil
    :accessor rlm-endpoint--final-value
    :type t
    :documentation "The value the environment recorded through finish.")
   (final-p
    :initform nil
    :accessor rlm-endpoint--final-p
    :type boolean
    :documentation "True once the environment recorded a final value.")
   (active-operations
    :initform 0
    :accessor rlm-endpoint--active-operations
    :type (integer 0)
    :documentation "The admitted inference operations still running."))
  (:documentation
   "An unpublished loopback endpoint proxying environment inference calls."))

(-> rlm-endpoint-port (rlm-endpoint) (integer 1))
(defun rlm-endpoint-port (endpoint)
  "Return ENDPOINT's inherited loopback port."
  (image-daemon:daemon-runtime-port endpoint))

(-> rlm-endpoint-token (rlm-endpoint) non-empty-string)
(defun rlm-endpoint-token (endpoint)
  "Return ENDPOINT's inherited capability token."
  (image-daemon:daemon-runtime-token endpoint))

(-> rlm-endpoint--lock (rlm-endpoint) t)
(defun rlm-endpoint--lock (endpoint)
  "Return ENDPOINT's inherited lifecycle lock."
  (image-daemon:daemon-runtime-lock endpoint))

(-> rlm-endpoint-final (rlm-endpoint) (values t boolean))
(defun rlm-endpoint-final (endpoint)
  "Return the environment's recorded final value and whether one exists."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (values (rlm-endpoint--final-value endpoint)
            (rlm-endpoint--final-p endpoint))))

(-> rlm-endpoint--admit (rlm-endpoint) null)
(defun rlm-endpoint--admit (endpoint)
  "Atomically admit one inference operation unless the run finished.

Admission and the finished check share one lock, so no operation can
start after a finish commits; operations admitted earlier complete
normally, bounded by the shared budget."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (when (rlm-endpoint--final-p endpoint)
      (error 'rlm-inference-error
             :message "The run already recorded its final value."))
    (incf (rlm-endpoint--active-operations endpoint)))
  nil)

(-> rlm-endpoint--release (rlm-endpoint) null)
(defun rlm-endpoint--release (endpoint)
  "Release one admitted inference operation."
  (with-lock-held ((rlm-endpoint--lock endpoint))
    (decf (rlm-endpoint--active-operations endpoint)))
  nil)

(-> rlm-endpoint--call-admitted (rlm-endpoint t function) list)
(defun rlm-endpoint--call-admitted (endpoint admit-p function)
  "Call FUNCTION, holding an admission lease while ADMIT-P."
  (if admit-p
      (progn
        (rlm-endpoint--admit endpoint)
        (unwind-protect
             (funcall function)
          (rlm-endpoint--release endpoint)))
      (funcall function)))

(-> rlm-endpoint--record (rlm-endpoint list) null)
(defun rlm-endpoint--record (endpoint record)
  "Append one served-operation RECORD to ENDPOINT's ledger, when any.

The ledger links every child trace to the root run even when the
environment's Lisp discards the returned trace identifiers, so a run
leaves a machine-readable invocation tree instead of orphaned frames."
  (let ((ledger (rlm-endpoint--ledger endpoint)))
    (when ledger
      (handler-case
          (funcall ledger
                   (append record
                           (list :calls-remaining
                                 (rlm-budget-remaining-calls
                                  (rlm-endpoint--budget endpoint))
                                 :tokens-remaining
                                 (rlm-budget-remaining-tokens
                                  (rlm-endpoint--budget endpoint)))))
        (error (condition)
          (format *error-output*
                  "~&The inference run ledger failed: ~A~%" condition)))))
  nil)

(-> rlm-endpoint--operation-activity-callback
    (rlm-endpoint keyword)
    (option function))
(defun rlm-endpoint--operation-activity-callback (endpoint operation)
  "Return ENDPOINT's revocable activity callback for proxied OPERATION."
  (let ((callback (rlm-endpoint--activity-callback endpoint)))
    (when callback
      (lambda (activity)
        (with-lock-held ((rlm-endpoint--activity-lock endpoint))
          (when (rlm-endpoint--activity-enabled-p endpoint)
            (rlm--note-activity
             callback
             (format nil "~(~A~) · ~A" operation activity))))))))

(-> rlm-endpoint--dispatch (rlm-endpoint keyword list) list)
(defun rlm-endpoint--dispatch (endpoint operation arguments)
  "Serve one authenticated environment OPERATION and return its response."
  (let ((provider (rlm-endpoint--provider endpoint))
        (configuration (rlm-endpoint--configuration endpoint))
        (budget (rlm-endpoint--budget endpoint))
        (activity-callback
          (rlm-endpoint--operation-activity-callback endpoint operation)))
    (rlm-endpoint--call-admitted
     endpoint
     (member operation '(:infer :map :run))
     (lambda ()
       (ecase operation
         (:run
          (let ((task (getf arguments ':task))
                (policy (or (getf arguments ':policy) ':direct)))
            (unless (and (stringp task) (non-empty-string-p task))
              (error 'rlm-inference-error
                     :message "An environment run call requires task text."))
            (unless (keywordp policy)
              (error 'rlm-inference-error
                     :message "An environment run policy must be a keyword."))
            (let ((run-budget (rlm-budget-descend budget :task task)))
              (unless (compute-applicable-methods
                       #'rlm-decompose-inference-task
                       (list policy task nil run-budget))
                (error 'rlm-inference-error
                       :message
                       (format nil "No decomposition policy is named ~S."
                               policy)))
              (multiple-value-bind (value trace-identifier tokens-spent)
                  (rlm-run task
                           :policy policy
                           :context (rlm--context-designators
                                     (getf arguments ':context))
                           :contract (or (getf arguments ':contract) ':text)
                           :budget run-budget
                           :provider provider
                           :configuration configuration
                           :activity-callback activity-callback
                           :concurrency
                           (let ((requested (getf arguments ':concurrency)))
                             (if (and (integerp requested) (plusp requested))
                                 requested
                                 *rlm-map-default-concurrency*)))
                (rlm-endpoint--record endpoint
                                      (list :operation :run
                                            :task task
                                            :policy policy
                                            :child-trace trace-identifier
                                            :tokens tokens-spent))
                (list :rlm-response :status :ok
                      :value value :trace trace-identifier
                      :tokens tokens-spent)))))
         (:infer
          (let ((task (getf arguments ':task)))
            (unless (and (stringp task) (non-empty-string-p task))
              (error 'rlm-inference-error
                     :message "An environment infer call requires task text."))
            (multiple-value-bind (value trace-identifier tokens-spent)
                (infer task
                       :context (getf arguments ':context)
                       :contract (or (getf arguments ':contract) ':text)
                       :budget (rlm-budget-descend budget :task task)
                       :provider provider
                       :configuration configuration
                       :activity-callback activity-callback)
              (rlm-endpoint--record endpoint
                                    (list :operation :infer
                                          :task task
                                          :child-trace trace-identifier
                                          :tokens tokens-spent))
              (list :rlm-response :status :ok
                    :value value :trace trace-identifier
                    :tokens tokens-spent))))
         (:map
          (let ((tasks (getf arguments ':tasks)))
            (unless (and (listp tasks)
                         (plusp (length tasks))
                         (<= (length tasks) *rlm-endpoint-map-maximum-tasks*))
              (error 'rlm-inference-error
                     :message
                     (format nil "An environment map call fans out 1 to ~D tasks."
                             *rlm-endpoint-map-maximum-tasks*)))
            (let ((results
                    (rlm-map tasks
                             :contract (or (getf arguments ':contract) ':text)
                             :budget (rlm-budget-descend budget :task "rlm-map")
                             :provider provider
                             :configuration configuration
                             :concurrency
                             (let ((requested (getf arguments ':concurrency)))
                               (if (and (integerp requested) (plusp requested))
                                   requested
                                   *rlm-map-default-concurrency*))
                             :activity-callback activity-callback)))
              (rlm-endpoint--record
               endpoint
               (list :operation :map
                     :children
                     (loop for result in results
                           collect (append
                                    (list :task (getf result ':task))
                                    (if (getf result ':error)
                                        (list :error (getf result ':error))
                                        (list :child-trace
                                              (getf result ':trace)
                                              :tokens
                                              (getf result ':tokens)))))))
              (list :rlm-response :status :ok :value results))))
         (:finish
          (with-lock-held ((rlm-endpoint--lock endpoint))
            ;; The first finish wins, atomically with the admission check, so
            ;; no operation can start after the terminal state commits.
            (when (rlm-endpoint--final-p endpoint)
              (error 'rlm-inference-error
                     :message "The run already recorded its final value."))
            (setf (rlm-endpoint--final-value endpoint) (getf arguments ':value)
                  (rlm-endpoint--final-p endpoint) t))
          (rlm-endpoint--record endpoint (list :operation :finish))
          (list :rlm-response :status :ok :value ':finished)))))))

(defmethod image-daemon:daemon-runtime-request-valid-p
    ((endpoint rlm-endpoint) request)
  "Validate ENDPOINT's rlm request with the shared capability checks."
  (image-daemon:daemon-request-valid-p
   request
   (rlm-endpoint-token endpoint)
   :request-tag ':rlm-request
   :protocol-version nil))

(defmethod image-daemon:daemon-runtime-error-response
    ((endpoint rlm-endpoint) condition)
  "Format a rejected or failed ENDPOINT request as an rlm response."
  (when (typep condition 'rlm-partial-result)
    (let ((observation (rlm-partial-result-observation condition)))
      (rlm-endpoint--record
       endpoint (list :operation ':incomplete :result observation))))
  (list :rlm-response :status ':error
        :message
        (if (typep condition 'rlm-partial-result)
            (let ((observation (rlm-partial-result-observation condition)))
              (format nil "~A~%~S" condition observation))
            (princ-to-string condition))))

(-> rlm-endpoint--request
    (rlm-endpoint list &key (:socket t) (:stream t))
    null)
(defun rlm-endpoint--request (endpoint request &key socket stream)
  "Dispatch one library-authenticated environment request."
  (declare (ignore socket))
  (let ((fields (rest request)))
    (daemon-write-packet
     stream
     (rlm-endpoint--dispatch endpoint
                              (getf fields ':operation)
                              (getf fields ':arguments))))
  nil)


(-> rlm-endpoint-start
    (&key (:provider model-provider)
          (:configuration configuration)
          (:budget rlm-budget)
          (:ledger (option function))
          (:activity-callback (option function)))
    rlm-endpoint)
(defun rlm-endpoint-start
    (&key provider configuration budget ledger activity-callback)
  "Start an unpublished loopback endpoint proxying environment calls."
  (let ((endpoint
          (image-daemon:daemon-runtime-create
           :publish-p nil
           :token (daemon-random-token)
           :request-function #'rlm-endpoint--request
           :class 'rlm-endpoint
           :initargs (list :provider provider
                           :configuration configuration
                           :budget budget
                           :ledger ledger
                           :activity-callback activity-callback))))
    (image-daemon:daemon-runtime-start endpoint)
    endpoint))

(-> rlm-endpoint-stop (rlm-endpoint) null)
(defun rlm-endpoint-stop (endpoint)
  "Revoke activity publication, then stop ENDPOINT's supervised runtime."
  (with-lock-held ((rlm-endpoint--activity-lock endpoint))
    (setf (rlm-endpoint--activity-enabled-p endpoint) nil))
  (image-daemon:daemon-runtime-stop endpoint)
  nil)
