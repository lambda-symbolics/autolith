(in-package #:autolith-supervisor)

;;;; -- Knowledge Request Dispatch --

(serapeum/types:-> knowledge-service--data (parent-proto:knowledge-request) hash-table)
(defun knowledge-service--data (request)
  "Decode bounded public fields without evaluating Lisp data."
  (if (parent-proto:knowledge-request.has-fields request)
      (let ((data
             (protocol-value->data
              (common-proto:make-value :map-value
                                       (parent-proto:knowledge-request.fields request)))))
        (unless (hash-table-p data) (knowledge-service--reject ':invalid-request))
        data)
      (make-hash-table :test #'equal)))

(serapeum/types:-> knowledge-service--view-name (string) (values keyword (option string)))
(defun knowledge-service--view-name (name)
  "Resolve only existing public knowledge identifiers."
  (cond
    ((member name '("facts" "predicates" "rules" "methods" "contracts" "procedures" "changes")
             :test #'equal)
     (values (find name '(:facts :predicates :rules :methods :contracts :procedures :changes)
                   :test #'equal :key (lambda (item) (string-downcase (symbol-name item)))) nil))
    ((and (uiop:string-prefix-p "claim/" name) (<= 7 (length name) 262))
     (values ':claim (subseq name 6)))
    (t
     (knowledge-service--reject ':unsupported))))

(serapeum/types:-> knowledge-service--render
    (knowledge-service parent-proto:knowledge-request keyword) parent-proto:knowledge-response)
(defun knowledge-service--render (service request scope)
  "Render one public view from the authorized graph and issue its guard."
  (let* ((context (knowledge-service-context service))
         (base    (knowledge-service-authorized-base service scope (knowledge-base-read context))))
    (multiple-value-bind (kind claim) (knowledge-service--view-name (parent-proto:knowledge-request.view request))
      (when (and (not (eq kind ':facts))
                 (or (parent-proto:knowledge-request.has-query request)
                     (plusp (parent-proto:knowledge-request.limit request))))
        (knowledge-service--reject ':invalid-request))
      (parent-proto:make-knowledge-response
       :revision (knowledge-service--issue service scope base)
       :content  (knowledge-service-render
                  context kind :base base :claim claim
                  :query           (and (parent-proto:knowledge-request.has-query request)
                                        (parent-proto:knowledge-request.query request))
                  :maximum-results (and (plusp (parent-proto:knowledge-request.limit request))
                                        (parent-proto:knowledge-request.limit request))
                  :viewer (knowledge-service-session-id service) :scope scope
                  :allow-global-p  (knowledge-service-allow-global-p service))))))

(serapeum/types:-> knowledge-service--check
    (knowledge-service hash-table keyword) parent-proto:knowledge-response)
(defun knowledge-service--check (service fields scope)
  "Check exact admitted contracts and retain bounded bindings when recording is needed."
  (knowledge-service--fields fields '("tool" "arguments") '("tool" "arguments"))
  (unless (and (eq scope ':workspace) (non-empty-string-p (gethash "tool" fields))
               (<= (length (gethash "tool" fields)) 1024) (hash-table-p (gethash "arguments" fields)))
    (knowledge-service--reject ':invalid-request))
  (let* ((context   (knowledge-service-context service))
         (arguments (gethash "arguments" fields))
         (tool      (gethash "tool" fields))
         (matches   (knowledge-contracts-for-call context tool arguments))
         (data      (make-hash-table :test #'equal)))
    (if (null matches)
        (setf (gethash "none" data) t)
        (multiple-value-bind (refusal admitted) (knowledge-contract-refusal context matches)
          (when refusal (setf (gethash "refusal" data) refusal))
          (unless (and refusal (knowledge-active-p context))
            (let* ((handle (memory-context-make-identifier context))
                   (charge (length (cl-protobufs:serialize-to-bytes (protocol-data->map fields)))))
              (when (or (>= (hash-table-count (knowledge-service-handles service)) 64)
                        (> (+ charge (loop for entry being the hash-values of (knowledge-service-handles service)
                                            sum (getf entry :charge))) 4194304))
                (knowledge-service--reject ':resource-limit))
              (setf (gethash handle (knowledge-service-handles service))
                    (list :scope scope :tool tool :arguments arguments
                          :matches (or admitted matches) :charge charge)
                    (gethash "handle" data) handle)))))
    (parent-proto:make-knowledge-response :fields (protocol-data->map data))))

(serapeum/types:-> knowledge-service--record
    (knowledge-service hash-table keyword) parent-proto:knowledge-response)
(defun knowledge-service--record (service fields scope)
  "Record a successful result or durably release an abandoned contract check."
  (knowledge-service--fields fields '("handle" "result" "outcome") '("handle"))
  (let ((outcome (gethash "outcome" fields "succeeded")))
    (unless (and (non-empty-string-p (gethash "handle" fields))
                 (member outcome '("succeeded" "abandoned") :test #'equal)
                 (if (equal outcome "abandoned")
                     (not (nth-value 1 (gethash "result" fields)))
                     (and (stringp (gethash "result" fields))
                          (<= (length (gethash "result" fields)) 262144))))
      (knowledge-service--reject ':invalid-request)))
  (let* ((entry   (gethash (gethash "handle" fields) (knowledge-service-handles service)))
         (context (knowledge-service-context service))
         (content (gethash "result" fields)))
    (unless (and entry (eq scope (getf entry :scope)))
      (knowledge-service--reject ':permission-denied))
    (when (equal (gethash "outcome" fields) "abandoned")
      (return-from knowledge-service--record
        (parent-proto:make-knowledge-response
         :fields (protocol-data->map (argo:json-object "claims" #() "errors" #())))))
    (let* ((current
            (knowledge-contracts-for-call context (getf entry :tool) (getf entry :arguments)))
           (contracts (mapcar #'first current)))
      (unless
          (every (lambda (match) (member (first match) contracts :test #'equal))
                 (getf entry :matches))
        (knowledge-service--reject ':revision-conflict)))
    (let ((decoded (handler-case (argo:json-decode content) (argo:json-error nil nil))))
      (multiple-value-bind (claims errors)
          (knowledge-contract-record-data context (getf entry :matches) :arguments
           (getf entry :arguments) :decoded-result decoded :result-content content :tool
           (getf entry :tool) :conversation (knowledge-service-session-id service))
        (let ((data (make-hash-table :test #'equal)))
          (setf (gethash "claims" data) (coerce claims 'vector)
                (gethash "errors" data) (coerce errors 'vector))
          (parent-proto:make-knowledge-response :fields
                                                (common-proto:value.map-value
                                                 (protocol-data->value data))))))))

(serapeum/types:-> knowledge-service-request (knowledge-service parent-proto:knowledge-request)
    parent-proto:knowledge-response)
(defun knowledge-service-request (service request)
  "Validate, authorize and execute one negotiated data-only knowledge request."
  (bordeaux-threads:with-lock-held ((knowledge-service-lock service))
    (handler-case
     (let* ((scope     (knowledge-service--scope service request))
            (operation (parent-proto:knowledge-request.operation request))
            (fields    (knowledge-service--data request))
            (context   (knowledge-service-context service)))
       (unless (<= (parent-proto:knowledge-request.limit request) 50)
         (knowledge-service--reject ':resource-limit))
       (when
           (and (not (eq operation ':read))
                (or (parent-proto:knowledge-request.has-query request)
                    (plusp (parent-proto:knowledge-request.limit request))))
         (knowledge-service--reject ':invalid-request))
       (case operation
         (:read
          (let ((plan-p (equal (parent-proto:knowledge-request.view request) "plan")))
            (unless (and (zerop (length (parent-proto:knowledge-request.operation-id request)))
                         (not (parent-proto:knowledge-request.has-expected-revision request))
                         (if plan-p
                             (and (not (parent-proto:knowledge-request.has-query request))
                                  (zerop (parent-proto:knowledge-request.limit request)))
                             (zerop (hash-table-count fields))))
              (knowledge-service--reject ':invalid-request))
            (knowledge-call-with-batch context
             (lambda (base)
               (declare (ignore base))
               (knowledge-service--response
                service (if plan-p
                            (knowledge-service-plan-response service fields scope)
                            (knowledge-service--render service request scope))))
             :select (lambda (base) (knowledge-service-authorized-base service scope base)))))
         (:edit
          (unless (equal (parent-proto:knowledge-request.view request) "facts")
            (knowledge-service--reject ':invalid-request))
          (knowledge-service--fields fields '("operations") '("operations"))
          (let ((operations (knowledge-service--array (gethash "operations" fields))))
            (unless (<= 1 (length operations) 16) (knowledge-service--reject ':resource-limit))
            (knowledge-service--write service request :scope scope :function
             (lambda ()
               (knowledge-service--guard service request :scope scope :base
                (knowledge-base-read context))
               (let ((lines
                      (mapcar (lambda (item) (knowledge-service--apply service item scope))
                              operations)))
                 (parent-proto:make-knowledge-response :revision
                                                       (knowledge-service--issue service scope
                                                        (knowledge-base-read context))
                                                       :content (format nil "~{~A~%~}" lines)))))))
         (:check
          (unless
              (and (zerop (length (parent-proto:knowledge-request.operation-id request)))
                   (zerop (length (parent-proto:knowledge-request.view request)))
                   (not (parent-proto:knowledge-request.has-expected-revision request)))
            (knowledge-service--reject ':invalid-request))
          (knowledge-call-with-batch context
           (lambda (base) (declare (ignore base)) (knowledge-service--check service fields scope))
           :select (lambda (base) (knowledge-service-authorized-base service scope base))))
         (:record
          (unless
              (and (zerop (length (parent-proto:knowledge-request.view request)))
                   (not (parent-proto:knowledge-request.has-expected-revision request)))
            (knowledge-service--reject ':invalid-request))
          (prog1
              (knowledge-service--write service request :scope scope :function
                                        (lambda () (knowledge-service--record service fields scope)))
            (remhash (gethash "handle" fields) (knowledge-service-handles service))))
         ((:context :expand)
          (unless (and (eq scope ':workspace)
                       (zerop (length (parent-proto:knowledge-request.operation-id request)))
                       (zerop (length (parent-proto:knowledge-request.view request)))
                       (not (parent-proto:knowledge-request.has-expected-revision request)))
            (knowledge-service--reject ':invalid-request))
          (knowledge-call-with-batch
           context
           (lambda (base)
             (declare (ignore base))
             (knowledge-service--response
              service
              (ecase operation
                (:context
                 (knowledge-service-context-response service fields scope))
                (:expand
                 (knowledge-service-expand-response service fields scope)))))
           :select (lambda (base) (knowledge-service-authorized-base service scope base))))
         (otherwise
          (knowledge-service--reject ':unsupported))))
     (memory-policy-error nil (knowledge-service--reject ':permission-denied))
     (knowledge-error nil (knowledge-service--reject ':invalid-request))
     (memory-record-error nil (knowledge-service--reject ':invalid-request)))))

(defmethod session-knowledge-attach ((service knowledge-service) runtime)
  "Require the parent service and session store to have the same owner."
  (unless
      (equal (knowledge-service-session-id service)
             (session-store-id (session-runtime-store runtime)))
    (error 'parent-protocol-error :reason ':invalid-knowledge-service))
  nil)

(defmethod session-knowledge-handle ((service knowledge-service) request)
  "Dispatch a knowledge request through its trusted service."
  (knowledge-service-request service request))

(defmethod session-knowledge-error-code ((condition knowledge-service-error))
  "Return the fixed code for a public knowledge refusal."
  (knowledge-service-error-code condition))
