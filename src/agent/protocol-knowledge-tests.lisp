(in-package #:autolith)

;;;; -- Knowledge Resource Boundary Tests --

(defclass agent-knowledge-test-tool (tool)
  ()
  (:documentation "A contract fixture with success, failure and nonlocal exit paths."))

(defmethod tool-execute ((tool agent-knowledge-test-tool) (context tool-context) arguments)
  "Return the requested outcome without reading a private store."
  (declare (ignore tool context))
  (cond
    ((equal (gethash "outcome" arguments) "throw")
     (throw 'agent-knowledge-test-abandoned ':abandoned))
    ((equal (gethash "outcome" arguments) "fail")
     (tool-failure "failed"))
    (t
     (tool-success "{}"))))

(-> agent-knowledge-test--reply (agent-wire:knowledge-request hash-table) agent-wire:envelope)
(defun agent-knowledge-test--reply (request receipts)
  "Model deterministic guards and receipts at the serialized client boundary."
  (case (agent-wire:knowledge-request.operation request)
    (:read
     (assert (eq ':workspace (agent-wire:knowledge-request.scope request)))
     (agent-wire:make-envelope
      :knowledge-response (agent-wire:make-knowledge-response
                           :content  (format nil "parent view ~A" (agent-wire:knowledge-request.view request))
                           :revision (if (zerop (hash-table-count receipts)) "before" "after"))))
    (:edit
     (let* ((id      (agent-wire:knowledge-request.operation-id request))
            (payload (agent-record-resource--operation-key
                      (autolith-supervisor::protocol-value->data
                       (cl-protobufs.autolith.common.v1:make-value
                        :map-value (agent-wire:knowledge-request.fields request)))))
            (prior   (gethash id receipts)))
       (assert (equal "before" (agent-wire:knowledge-request.expected-revision request)))
       (cond
         (prior
          (assert (equal prior payload))
          (agent-wire:make-envelope
           :knowledge-response (agent-wire:make-knowledge-response :content "duplicate" :revision "after")))
         ((plusp (hash-table-count receipts))
          (agent-wire:make-envelope :failure (agent-wire:make-failure :code ':revision-conflict)))
         (t
          (setf (gethash id receipts) payload)
          (agent-wire:make-envelope
           :knowledge-response (agent-wire:make-knowledge-response :content "created" :revision "after"))))))
    (:check
     (agent-wire:make-envelope
      :knowledge-response (agent-wire:make-knowledge-response
                           :fields (agent-protocol-knowledge--fields (json-object "handle" "checked")))))
    (:record
     (agent-wire:make-envelope
      :knowledge-response (agent-wire:make-knowledge-response
                           :fields (agent-protocol-knowledge--fields
                                    (json-object "claims" #("claim") "errors" #())))))))

(-> agent-knowledge-test--tools (t pathname) null)
(defun agent-knowledge-test--tools (driver private-root)
  "Exercise real resource tools with an unusable child private store."
  (let* ((configuration (make-configuration :provider-validation-p nil :knowledge-mode "active"
                                           :data-root private-root :state-root private-root :config-root private-root))
         (scope         (agent-record-resources-install driver configuration))
         (conversation  (make-instance 'conversation :identifier "knowledge-tools"))
         (context       (make-instance 'tool-context :configuration configuration :conversation conversation :worker nil))
         (other         (make-instance 'tool-context :configuration configuration :worker nil
                                      :conversation (make-instance 'conversation :identifier "knowledge-other")))
         (registry      (make-instance 'tool-registry)))
    (default-tools--register-workspace registry)
    (unwind-protect
         (labels ((read-view (view)
                    (agent-record-resource-test--call registry context "read" "uri" (format nil "memory:~A" view)))

                  (edit-view (revision operation &optional (target context))
                    (agent-record-resource-test--call registry target "edit" "uri" "memory:facts"
                                                     "base-revision" revision "operations" (vector operation))))
           (dolist (view '("facts" "predicates" "rules" "changes" "methods" "contracts" "procedures" "claim/id"))
             (assert (search (format nil "parent view ~A" view) (tool-result-content (read-view view)))))
           (let* ((read      (read-view "facts"))
                  (revision  (agent-record-resource-test--field read "Revision: "))
                  (operation (json-object "op" "predicate-define" "predicate" "test.ready"
                                          "arguments" #()))
                  (first     (edit-view revision operation))
                  (retry     (edit-view revision operation)))
             (assert (tool-result-success-p first) () "~A" (tool-result-content first))
             (assert (tool-result-success-p retry) () "~A" (tool-result-content retry))
             (assert (string= (agent-record-resource-test--field first "Revision: ")
                              (agent-record-resource-test--field retry "Revision: ")))
             (assert (not (tool-result-success-p
                           (edit-view revision (json-object "op" "fact-retract" "id" "other")))))
             (assert (not (tool-result-success-p (edit-view revision operation other)))))
           (assert (not (tool-result-success-p (read-view "recipes"))))
           (let* ((adapter (agent-record-resource-scope-knowledge scope))
                  (check   (agent-protocol-knowledge-request adapter ':check
                             :fields (json-object "tool" "shell.run" "arguments" (json-object)))))
             (assert (equal "checked" (gethash "handle" (agent-protocol-knowledge-response-fields check))))
             (let ((record (agent-protocol-knowledge-request adapter ':record
                             :operation-id "record-result" :fields (json-object "handle" "checked" "result" "{}"))))
               (assert (equalp #("claim") (gethash "claims" (agent-protocol-knowledge-response-fields record)))))))
      (agent-record-resources-uninstall configuration)))
  nil)

(-> agent-knowledge-test-service-tools (t pathname) null)
(defun agent-knowledge-test-service-tools (driver private-root)
  "Exercise normal tools against a real parent knowledge service."
  (let* ((configuration (make-configuration :provider-validation-p nil :knowledge-mode "active"
                                           :data-root private-root :state-root private-root :config-root private-root))
         (conversation  (make-instance 'conversation :identifier "knowledge-service-tools"))
         (context       (make-instance 'tool-context :configuration configuration :conversation conversation :worker nil))
         (registry      (make-instance 'tool-registry)))
    (agent-record-resources-install driver configuration)
    (default-tools--register-workspace registry)
    (unwind-protect
         (labels ((read-view (view)
                    (agent-record-resource-test--call registry context "read" "uri" (format nil "memory:~A" view)))

                  (edit-view (revision operations)
                    (agent-record-resource-test--call registry context "edit" "uri" "memory:facts"
                                                     "base-revision" revision "operations" operations)))
           (let* ((reply (agent-protocol-knowledge-request
                          (make-agent-protocol-knowledge driver) ':context
                          :fields (json-object "query" "boundary" "turn" 1 "active" t)))
                  (data  (agent-protocol-knowledge-response-fields reply)))
             (assert (equalp #() (gethash "memory_lines" data)))
             (assert (equalp #() (gethash "fact_lines" data)))
             (assert (stringp (gethash "changes" data)))
             (dolist (key '("stale_count" "unverified_count" "change_count"))
               (assert (zerop (gethash key data)))))
           (dolist (view '("facts" "predicates" "rules" "changes" "methods" "contracts" "procedures"))
             (let ((result (read-view view)))
               (assert (tool-result-success-p result) () "~A: ~A" view (tool-result-content result))))
           (let* ((before   (read-view "facts"))
                  (revision (agent-record-resource-test--field before "Revision: "))
                  (operations
                    (vector (json-object "op" "predicate-define" "predicate" "boundary.ready"
                                         "arguments" (vector (json-object "name" "subject" "type" "entity")))
                            (json-object "op" "fact-assert" "predicate" "boundary.ready"
                                         "arguments" #("release") "basis" "assumption")))
                  (edited   (edit-view revision operations))
                  (retry    (edit-view revision operations)))
             (assert (tool-result-success-p edited) () "~A" (tool-result-content edited))
             (assert (tool-result-success-p retry) () "~A" (tool-result-content retry))
             (assert (search "boundary.ready" (tool-result-content (read-view "facts"))))
             (assert (search "boundary.ready" (tool-result-content (read-view "predicates"))))
             (let* ((revision (agent-record-resource-test--field (read-view "facts") "Revision: "))
                    (result   (edit-view
                               revision
                               (vector (json-object "op" "contract-define" "id" "failed-call-release"
                                                    "tool" "boundary.probe"
                                                    "effects" #(#("boundary.ready" "successful-call"))))))
                    (tool     (make-instance 'agent-knowledge-test-tool
                                             :namespace "boundary" :name "probe")))
               (assert (tool-result-success-p result) () "~A" (tool-result-content result))
               (loop repeat 70
                     do (sleep 0.06)
                        (assert (not (tool-result-success-p
                                      (tool-execute-parent-contracts
                                       tool context (json-object "outcome" "fail"))))))
               (assert (eq ':abandoned
                           (catch 'agent-knowledge-test-abandoned
                             (tool-execute-parent-contracts tool context (json-object "outcome" "throw")))))
               (assert (not (search "successful-call" (tool-result-content (read-view "facts")))))
               (assert (tool-result-success-p
                        (tool-execute-parent-contracts tool context (json-object))))
               (assert (search "successful-call" (tool-result-content (read-view "facts")))))
             (assert (not (tool-result-success-p
                           (edit-view revision (vector (json-object "op" "fact-assert"
                                                                    "predicate" "boundary.ready"
                                                                    "arguments" #("stale")))))))))
      (agent-record-resources-uninstall configuration)))
  nil)

(-> run-agent-protocol-knowledge-tests () integer)
(defun run-agent-protocol-knowledge-tests ()
  "Run normal knowledge resource tools over the real framed byte protocol."
  (let* ((directory    (merge-pathnames (format nil "knowledge-client-~A/" (make-identifier))
                                        (uiop:temporary-directory)))
         (private-file (merge-pathnames "private" directory))
         (receipts     (make-hash-table :test #'equal)))
    (ensure-directories-exist directory)
    (with-open-file (stream private-file :direction ':output :if-exists ':error)
      (write-line "not a directory" stream))
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (parent)
            (let ((welcome (agent-protocol-test--welcome)))
              (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1" "knowledge.v1"))
              (autolith-supervisor::parent-connection-handshake parent welcome))
            (loop for envelope = (autolith-supervisor::parent-connection-receive parent)
                  do (case (agent-wire:envelope.body-case envelope)
                       (agent-wire:output
                        (return))
                       (agent-wire:knowledge-request
                        (let ((reply (agent-knowledge-test--reply (agent-wire:envelope.knowledge-request envelope) receipts)))
                          (setf (agent-wire:envelope.reply-to reply) (agent-wire:envelope.message-id envelope))
                          (autolith-supervisor::parent-connection-send parent reply)))
                       (otherwise
                        (assert nil)))))
          (lambda (client)
            (agent-protocol-handshake client :features '("session.v1" "records.v1" "knowledge.v1"))
            (let ((driver (agent-protocol-driver-start client)))
              (unwind-protect
                   (agent-knowledge-test--tools driver (uiop:ensure-directory-pathname private-file))
                (agent-protocol-output client ':diagnostic "done")
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))
  (agent-record-resource-test--contract-boundary)
  (+ 2 (run-agent-protocol-knowledge-service-resource-tests)))

(-> run-agent-protocol-knowledge-service-resource-tests () integer)
(defun run-agent-protocol-knowledge-service-resource-tests ()
  "Exercise normal active resource tools against the shared parent knowledge engine."
  (let* ((directory    (merge-pathnames (format nil "knowledge-service-client-~A/" (make-identifier))
                                        (uiop:temporary-directory)))
         (private-file (merge-pathnames "private" directory))
         (context      (autolith-supervisor::make-memory-store-context
                        :memory-path    (merge-pathnames "memory.sexp" directory)
                        :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                        :workspace      directory))
         (service      (autolith-supervisor::make-knowledge-service
                         :context context :session-id "client-session" :approve (constantly t))))
    (ensure-directories-exist directory)
    (with-open-file (stream private-file :direction ':output :if-exists ':error)
      (write-line "not a directory" stream))
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (parent)
            (let ((welcome (agent-protocol-test--welcome)))
              (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1" "knowledge.v1"))
              (autolith-supervisor::parent-connection-handshake parent welcome))
            (loop for envelope = (autolith-supervisor::parent-connection-receive parent)
                  do (case (agent-wire:envelope.body-case envelope)
                       (agent-wire:output
                        (return))
                       (agent-wire:knowledge-request
                        (let ((reply
                                (handler-case
                                    (agent-wire:make-envelope
                                     :knowledge-response
                                     (autolith-supervisor::knowledge-service-request
                                      service (agent-wire:envelope.knowledge-request envelope)))
                                  (autolith-supervisor::knowledge-service-error (condition)
                                    (agent-wire:make-envelope
                                     :failure (agent-wire:make-failure
                                               :code (autolith-supervisor::knowledge-service-error-code condition)))))))
                          (setf (agent-wire:envelope.reply-to reply) (agent-wire:envelope.message-id envelope))
                          (autolith-supervisor::parent-connection-send parent reply)))
                       (otherwise
                        (assert nil)))))
          (lambda (client)
            (agent-protocol-handshake client :features '("session.v1" "records.v1" "knowledge.v1"))
            (let ((driver (agent-protocol-driver-start client)))
              (unwind-protect
                   (agent-knowledge-test-service-tools driver (uiop:ensure-directory-pathname private-file))
                (agent-protocol-output client ':diagnostic "done")
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))
  1)
