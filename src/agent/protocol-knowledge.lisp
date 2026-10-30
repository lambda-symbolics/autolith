(in-package #:autolith)

;;;; -- Parent Knowledge Client --

(defclass agent-protocol-knowledge ()
  ((driver :initarg :driver :reader agent-protocol-knowledge-driver
           :documentation "The sole-reader protocol driver."))
  (:documentation "A data-only client for negotiated parent knowledge."))

(defclass agent-knowledge-request ()
  ((message :initarg :message :reader agent-knowledge-request-message
            :documentation "The detached request used for response validation."))
  (:documentation "One pending knowledge.v1 exchange."))

(define-condition agent-protocol-knowledge-error (error)
  ((code :initarg :code :reader agent-protocol-knowledge-error-code
         :documentation "The fixed public failure code."))
  (:report (lambda (condition stream)
             (format stream "Parent knowledge request failed: ~A"
                     (agent-protocol-knowledge-error-code condition))))
  (:documentation "A parent knowledge request failed."))

(-> agent-protocol-knowledge-available-p (t) boolean)
(defun agent-protocol-knowledge-available-p (driver)
  "Return whether DRIVER negotiated knowledge.v1."
  (and driver
       (not (null (member "knowledge.v1"
                          (agent-wire:welcome.features
                           (agent-protocol-client-welcome (agent-protocol-driver-client driver)))
                          :test #'equal)))))

(-> make-agent-protocol-knowledge (t) agent-protocol-knowledge)
(defun make-agent-protocol-knowledge (driver)
  "Select DRIVER without opening any local knowledge files."
  (unless (agent-protocol-knowledge-available-p driver)
    (error 'agent-protocol-knowledge-error :code ':operation-unsupported))
  (make-instance 'agent-protocol-knowledge :driver driver))

(-> agent-protocol-knowledge--fields
    (hash-table &key (:json-p boolean)) cl-protobufs.autolith.common.v1:value-map)
(defun agent-protocol-knowledge--fields (fields &key json-p)
  "Encode bounded fields. Normalize agent JSON only when JSON-P is true."
  (cl-protobufs.autolith.common.v1:value.map-value
   (autolith-supervisor::protocol-data->value
    (if json-p (agent-context--data fields) fields) :max-depth 16 :max-entries 4096)))

(-> agent-protocol-knowledge-request
    (agent-protocol-knowledge keyword &key (:view (option string)) (:scope keyword)
     (:operation-id (option string)) (:expected-revision (option string)) (:query (option string))
     (:limit integer) (:fields hash-table)) agent-wire:knowledge-response)
(defun agent-protocol-knowledge-request
    (adapter operation &key view (scope ':workspace) operation-id expected-revision query
                         (limit 0) (fields (make-hash-table :test #'equal)))
  "Send one request. Never retry implicitly or select a private store root."
  (unless (and (member operation '(:read :edit :check :record :context :expand))
               (member scope '(:workspace :global)))
    (error 'agent-protocol-knowledge-error :code ':invalid-request))
  (let ((request (agent-wire:make-knowledge-request
                  :operation operation :scope scope
                  :operation-id (or operation-id (if (member operation '(:edit :record)) (make-identifier) ""))
                  :view (or view (if (member operation '(:read :edit)) "facts" "")) :limit limit
                  :fields       (agent-protocol-knowledge--fields
                                 fields :json-p (not (null (member operation '(:check :edit))))))))
    (when expected-revision
      (setf (agent-wire:knowledge-request.expected-revision request) expected-revision))
    (when query
      (setf (agent-wire:knowledge-request.query request) query))
    (agent-session-driver-knowledge-request (agent-protocol-knowledge-driver adapter) request)))

(-> agent-protocol-knowledge-response-fields
    (agent-wire:knowledge-response &key (:max-depth (integer 1 32))
     (:max-entries (integer 1 65536))) hash-table)
(defun agent-protocol-knowledge-response-fields (response &key (max-depth 32) (max-entries 65536))
  "Decode public fields under bounded portable value limits."
  (if (agent-wire:knowledge-response.has-fields response)
      (autolith-supervisor::protocol-value->data
       (cl-protobufs.autolith.common.v1:make-value
        :map-value (agent-wire:knowledge-response.fields response))
       :max-depth max-depth :max-entries max-entries)
      (make-hash-table :test #'equal)))


;;;; -- Serialized Exchange --

(-> agent-protocol-knowledge-send (agent-protocol-client agent-wire:knowledge-request) integer)
(defun agent-protocol-knowledge-send (client request)
  "Send one detached request and register its terminal response."
  (let ((copy (agent-protocol--message-copy request)))
    (agent-protocol--send
     client
     (lambda ()
       (unless (member "knowledge.v1"
                       (agent-wire:welcome.features (agent-protocol-client-welcome client))
                       :test #'equal)
         (error 'agent-protocol-error :reason ':knowledge-not-negotiated))
       (when (agent-protocol-client-stop client)
         (error 'agent-protocol-error :reason ':stopping))
       (when (>= (hash-table-count (agent-protocol-client-pending client))
                 (agent-wire:limits.max-in-flight-requests
                  (autolith-supervisor::parent-connection-limits client)))
         (error 'agent-protocol-error :reason ':request-limit))
       (values (agent-wire:make-envelope :knowledge-request copy)
               (make-instance 'agent-knowledge-request :message copy))))))

(-> agent-protocol-knowledge--response-fields-p (keyword hash-table) boolean)
(defun agent-protocol-knowledge--response-fields-p (operation fields)
  "Accept only the fixed public fields for each operation."
  (flet ((text-p (value)
           (and (non-empty-string-p value) (<= (length value) 65536)))

         (texts-p (value)
           (and (simple-vector-p value) (<= (length value) 4096)
                (every (lambda (item) (and (stringp item) (<= (length item) 65536))) value))))
    (case operation
      ((:read :edit)
       (zerop (hash-table-count fields)))

      (:plan-read
       (and (= 3 (hash-table-count fields))
            (simple-vector-p (gethash "steps" fields))
            (<= (length (gethash "steps" fields)) 32)
            (every #'hash-table-p (gethash "steps" fields))
            (stringp (gethash "rendered" fields))
            (<= (length (gethash "rendered" fields)) 131072)
            (simple-vector-p (gethash "changes" fields))
            (<= (length (gethash "changes" fields)) 32)
            (every (lambda (line) (and (stringp line) (<= (length line) 131072)))
                   (gethash "changes" fields))
            t))
      (:check
       (or (and (= 1 (hash-table-count fields)) (eq t (gethash "none" fields)))
           (and (<= 1 (hash-table-count fields) 2)
                (loop for key being the hash-keys of fields
                      always (member key '("handle" "refusal") :test #'equal))
                (or (text-p (gethash "handle" fields)) (text-p (gethash "refusal" fields)))
                (loop for value being the hash-values of fields always (text-p value))
                t)))
      (:record
       (and (= 2 (hash-table-count fields))
            (texts-p (gethash "claims" fields))
            (let ((errors (gethash "errors" fields)))
              (and (simple-vector-p errors) (<= (length errors) 4096)
                   (every (lambda (entry)
                            (and (hash-table-p entry) (= 3 (hash-table-count entry))
                                 (every (lambda (key) (text-p (gethash key entry)))
                                        '("contract" "mapping" "message"))))
                          errors)))
            t))
      (:context
       (and (= 6 (hash-table-count fields))
            (texts-p (gethash "memory_lines" fields))
            (texts-p (gethash "fact_lines" fields))
            (<= (length (gethash "memory_lines" fields)) 6)
            (<= (length (gethash "fact_lines" fields)) 6)
            (stringp (gethash "changes" fields))
            (<= (length (gethash "changes" fields)) 262144)
            (every (lambda (key) (typep (gethash key fields) '(integer 0)))
                   '("stale_count" "unverified_count" "change_count"))
            t))
      (:expand
       (and (= 3 (hash-table-count fields))
            (simple-vector-p (gethash "steps" fields))
            (<= (length (gethash "steps" fields)) 1000)
            (text-p (gethash "status" fields))
            (hash-table-p (gethash "details" fields))
            t)))))

(defmethod agent-protocol--response-data (client envelope (expected agent-knowledge-request))
  "Check one correlated terminal response without evaluating returned data."
  (case (agent-wire:envelope.body-case envelope)
    (agent-wire:failure
     (agent-protocol--failure client (agent-wire:envelope.failure envelope)))
    (agent-wire:knowledge-response
     (let* ((response  (agent-wire:envelope.knowledge-response envelope))
            (request   (agent-knowledge-request-message expected))
            (operation (if (and (eq (agent-wire:knowledge-request.operation request) ':read)
                                (equal (agent-wire:knowledge-request.view request) "plan"))
                           ':plan-read
                           (agent-wire:knowledge-request.operation request))))
       (unless (and (<= (length (agent-wire:knowledge-response.content response)) 262144)
                    (or (not (member operation '(:read :edit)))
                        (and (agent-wire:knowledge-response.has-revision response)
                             (non-empty-string-p (agent-wire:knowledge-response.revision response))
                             (<= (length (agent-wire:knowledge-response.revision response)) 1024))))
         (agent-protocol--fatal client ':invalid-knowledge-response))
       (handler-case
           (let ((limits (autolith-supervisor::parent-connection-limits client)))
             (unless (agent-protocol-knowledge--response-fields-p
                      operation
                      (agent-protocol-knowledge-response-fields
                       response :max-depth (agent-wire:limits.max-value-depth limits)
                       :max-entries        (agent-wire:limits.max-collection-entries limits)))
               (agent-protocol--fatal client ':invalid-knowledge-response)))
         (autolith-supervisor::protocol-value-error ()
           (agent-protocol--fatal client ':invalid-knowledge-response)))))
    (otherwise
     (agent-protocol--fatal client ':unexpected-response)))
  t)

(-> agent-session-driver-knowledge-request (t agent-wire:knowledge-request) agent-wire:knowledge-response)
(defun agent-session-driver-knowledge-request (driver request)
  "Wait through the sole-reader driver. Never resend a request."
  (let ((delivery (agent-protocol-driver-request
                   driver (lambda (client) (agent-protocol-knowledge-send client request)))))
    (multiple-value-bind (envelope terminal-p) (agent-protocol-driver-next delivery)
      (unless (and terminal-p envelope)
        (error 'agent-protocol-error :reason ':invalid-knowledge-response))
      (cond
        ((agent-wire:envelope.has-knowledge-response envelope)
         (agent-wire:envelope.knowledge-response envelope))
        ((agent-wire:envelope.has-failure envelope)
         (error 'agent-protocol-knowledge-error
                :code (agent-wire:failure.code (agent-wire:envelope.failure envelope))))
        (t
         (error 'agent-protocol-error :reason ':invalid-knowledge-response))))))
