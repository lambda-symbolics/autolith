;;;; -- Serialized Access Client Checks --

(in-package #:autolith)

(-> agent-access-test--grant (&optional string) agent-wire:grant)
(defun agent-access-test--grant (&optional (target "/workspace/file"))
  "Build one exact filesystem read grant."
  (agent-wire:make-grant :resource ':filesystem :target target
                         :operations '("read") :duration ':instance))

(-> agent-access-test--request () agent-wire:access-request)
(defun agent-access-test--request ()
  "Build a two-resource batch to exercise atomic decisions."
  (agent-wire:make-access-request
   :operation-id "access-operation-1" :reason "Read two files"
   :requested-grants (list (agent-access-test--grant) (agent-access-test--grant "/workspace/second"))))

(-> agent-access-test--welcome () agent-wire:welcome)
(defun agent-access-test--welcome ()
  "Offer access policy with a complete known active set."
  (let ((welcome (agent-protocol-test--welcome)))
    (setf (agent-wire:welcome.features welcome) '("session.v1" "access.v1")
          (agent-wire:welcome.policy-revision welcome) 4
          (agent-wire:welcome.active-grants welcome)
          (agent-wire:access-request.requested-grants (agent-access-test--request)))
    welcome))

(-> agent-access-test--decision (keyword &key (:revision integer) (:grants list)) agent-wire:envelope)
(defun agent-access-test--decision (state &key (revision 4) grants)
  "Build one typed disposition with explicit active evidence."
  (agent-wire:make-envelope
   :access-decision (agent-wire:make-access-decision :state state :policy-revision revision
                                                     :effective-grants grants)))

(-> agent-access-test--exchange (list boolean) null)
(defun agent-access-test--exchange (responses invalid-p)
  "Send actual bytes through the sole reader; check pending and terminal ownership."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-access-test--welcome))
     (let* ((request (autolith-supervisor::parent-connection-receive parent))
            (id      (agent-wire:envelope.message-id request)))
       (assert (eq 'agent-wire:access-request (agent-wire:envelope.body-case request)))
       (assert (string= "access-operation-1"
                        (agent-wire:access-request.operation-id (agent-wire:envelope.access-request request))))
       (dolist (response responses)
         (setf (agent-wire:envelope.reply-to response) id)
         (autolith-supervisor::parent-connection-send parent response)))
     (unless invalid-p
       (assert (eq 'agent-wire:output
                   (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent))))))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "access.v1"))
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
            (let ((delivery (agent-protocol-driver-request driver
                             (lambda (peer) (agent-protocol-access-request peer (agent-access-test--request))))))
              (if invalid-p
                  (assert (handler-case
                              (progn
                                (loop (multiple-value-bind (reply terminal)
                                          (agent-protocol-driver-next delivery :timeout 2)
                                        (assert reply)
                                        (when terminal (return))))
                                nil)
                            (agent-protocol-error () t)))
                  (progn
                    (loop for expected in responses
                          do (multiple-value-bind (reply terminal)
                                 (agent-protocol-driver-next delivery :timeout 2)
                               (assert reply)
                               (assert (eq (agent-wire:envelope.body-case reply)
                                           (agent-wire:envelope.body-case expected)))
                               (assert (eq terminal
                                           (not (and (eq 'agent-wire:access-decision
                                                         (agent-wire:envelope.body-case expected))
                                                     (eq ':pending-approval
                                                         (agent-wire:access-decision.state
                                                          (agent-wire:envelope.access-decision expected)))))))))
                    (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
                    (assert (= 4 (agent-wire:welcome.policy-revision (agent-protocol-client-welcome client))))
                    (assert (handler-case (progn (agent-protocol-receive client) nil)
                              (agent-protocol-error (condition)
                                (eq ':reader-owned (agent-protocol-error-reason condition)))))
                    (agent-protocol-output client ':diagnostic "done"))))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-access-test--cancel () null)
(defun agent-access-test--cancel ()
  "A cancellation acknowledgement does not retire the access request."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-access-test--welcome))
     (let* ((request (autolith-supervisor::parent-connection-receive parent))
            (target  (agent-wire:envelope.message-id request))
            (cancel  (autolith-supervisor::parent-connection-receive parent)))
       (assert (= target (agent-wire:cancel-request.request-id (agent-wire:envelope.cancel-request cancel))))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to    (agent-wire:envelope.message-id cancel)
                                          :cancel-ack (agent-wire:make-cancel-ack)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to target
                                          :failure (agent-wire:make-failure :code ':cancelled))))
     (let ((cancel (autolith-supervisor::parent-connection-receive parent)))
       (assert (eq 'agent-wire:cancel-request (agent-wire:envelope.body-case cancel)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to    (agent-wire:envelope.message-id cancel)
                                          :cancel-ack (agent-wire:make-cancel-ack :already-terminal t))))
     (assert (eq 'agent-wire:output
                 (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent)))))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "access.v1"))
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
            (let* ((delivery (agent-protocol-driver-request driver
                              (lambda (peer) (agent-protocol-access-request peer (agent-access-test--request)))))
                   (cancel   (agent-protocol-driver-request driver
                               (lambda (peer)
                                 (agent-protocol-access-cancel peer (agent-protocol-delivery-id delivery))))))
              (multiple-value-bind (reply terminal) (agent-protocol-driver-next cancel :timeout 2)
                (assert terminal)
                (assert (eq 'agent-wire:cancel-ack (agent-wire:envelope.body-case reply))))
              (multiple-value-bind (reply terminal) (agent-protocol-driver-next delivery :timeout 2)
                (assert terminal)
                (assert (eq ':cancelled (agent-wire:failure.code (agent-wire:envelope.failure reply)))))
              (let ((late (agent-protocol-driver-request
                           driver (lambda (peer)
                                    (agent-protocol-access-cancel peer (agent-protocol-delivery-id delivery))))))
                (multiple-value-bind (reply terminal) (agent-protocol-driver-next late :timeout 2)
                  (assert terminal)
                  (assert (agent-wire:cancel-ack.already-terminal (agent-wire:envelope.cancel-ack reply)))))
              (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-access-test--observer (keyword &key (:path-p boolean)) null)
(defun agent-access-test--observer (state &key path-p)
  "Exercise command and path dispatch through both observer wrappers."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-access-test--welcome))
     (let* ((request  (autolith-supervisor::parent-connection-receive parent))
            (response (agent-access-test--decision
                       state :revision (if (eq state ':approved-pending-restart) 5 4)
                       :grants         (when (eq state ':active)
                                         (agent-wire:welcome.active-grants (agent-access-test--welcome))))))
       (assert (eq 'agent-wire:access-request (agent-wire:envelope.body-case request)))
       (when path-p
         (let ((grants (agent-wire:access-request.requested-grants (agent-wire:envelope.access-request request))))
           (assert (= 1 (length grants)))
           (assert (equal '("read") (agent-wire:grant.operations (first grants))))))
       (setf (agent-wire:envelope.reply-to response) (agent-wire:envelope.message-id request))
       (autolith-supervisor::parent-connection-send parent response))
     (assert (eq 'agent-wire:output
                 (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent)))))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "access.v1"))
     (let* ((driver   (agent-protocol-driver-start client))
            (observer (make-agent-access-observer
                       driver :command-grants
                       (lambda (command directory)
                         (assert (string= command "read files"))
                         (assert (equal directory #P"/workspace/"))
                         (agent-wire:access-request.requested-grants (agent-access-test--request)))))
            (guarded  (make-instance 'agent-session-observer
                                     :session  (make-instance 'agent-protocol-session :client client)
                                     :delegate (make-instance 'serialized-agent-observer :delegate observer)))
            (context  (make-instance 'tool-context :observer guarded
                                     :command-authorization-function
                                     (lambda (command directory)
                                       (declare (ignore command directory))
                                      (error 'agent-protocol-error :reason ':unexpected-local-approval)))))
       (unwind-protect
            (flet ((authorize ()
                     (if path-p
                         (workspace-tool-authorize-outside-path context #P"/workspace/file" "resource"
                                                                :operations '("read"))
                         (agent-observer-authorize-command guarded "read files" #P"/workspace/"))))
              (if (eq state ':approved-pending-restart)
                  (assert (handler-case (progn (authorize) nil)
                            (agent-access-restart-required (condition)
                              (= 5 (agent-access-restart-required-revision condition)))))
                  (assert (eq (authorize) (if path-p
                                             (eq state ':active)
                                             (if (eq state ':active) ':sandboxed ':deny)))))
              (assert (= 4 (agent-wire:welcome.policy-revision (agent-protocol-client-welcome client))))
              (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-access-test--legacy-path () null)
(defun agent-access-test--legacy-path ()
  "Use command approval only when the path interface is unhandled."
  (let ((driver (allocate-instance (find-class 'agent-protocol-driver))))
    (dolist (observer (list (make-agent-protocol-observer driver) (make-agent-access-observer driver)))
      (assert (eq ':deny (agent-observer-authorize-command observer "printf test" #P"/workspace/"))))
    (dolist (policy '(:deny :inherited-scope))
      (dolist (observer (list (make-agent-protocol-observer driver :command-policy policy)
                             (make-agent-access-observer driver :command-policy policy)))
        (let* ((wrapped (make-instance 'serialized-agent-observer :delegate observer))
               (context (make-instance 'tool-context :observer wrapped
                                        :command-authorization-function
                                        (lambda (command directory)
                                          (agent-observer-authorize-command wrapped command directory)))))
          (assert (eq policy (tool-context-authorize-command context "printf test" #P"/workspace/")))))))
  (dolist (decision '(:full-access :sandboxed :inherited-scope :deny))
    (let* ((calls   0)
           (context (make-instance
                     'tool-context
                     :configuration (make-configuration :working-directory      #P"/workspace/"
                                                         :provider-validation-p nil)
                     :observer      (make-instance 'serialized-agent-observer
                                                   :delegate (make-instance 'agent-observer))
                     :command-authorization-function
                     (lambda (command directory)
                       (declare (ignore command))
                       (assert (equal directory #P"/workspace/"))
                       (incf calls)
                       decision))))
      (assert (eq (workspace-tool-authorize-outside-path context #P"/outside/file" "resource"
                                                        :operations '("write"))
                  (eq decision ':full-access)))
      (assert (= calls 1))))
  nil)

(-> agent-access-test--invalid-request () null)
(defun agent-access-test--invalid-request ()
  "Reject malformed identities and batches before writing or registering them."
  (dolist (change
            (list (lambda (request) (setf (agent-wire:access-request.operation-id request) ""))
                  (lambda (request)
                    (setf (agent-wire:access-request.operation-id request) (make-string 257 :initial-element #\a)))
                  (lambda (request) (setf (agent-wire:access-request.requested-grants request) nil))
                  (lambda (request)
                    (setf (agent-wire:access-request.requested-grants request)
                          (list (agent-access-test--grant) (agent-access-test--grant))))
                  (lambda (request)
                    (setf (agent-wire:grant.target (first (agent-wire:access-request.requested-grants request)))
                          "relative/file"))
                  (lambda (request)
                    (setf (agent-wire:grant.operations (first (agent-wire:access-request.requested-grants request)))
                          '("read" "read")))
                  (lambda (request)
                    (setf (agent-wire:grant.operations (first (agent-wire:access-request.requested-grants request)))
                          '("connect")))))
    (let* ((pipe    (make-instance 'agent-protocol-test-pipe))
           (client  (agent-protocol-open pipe pipe "request-validation"))
           (request (agent-access-test--request)))
      (unwind-protect
           (progn
             (funcall change request)
             (assert (handler-case (progn (agent-protocol-access-request client request) nil)
                       (agent-protocol-error (condition)
                         (eq ':invalid-access-request (agent-protocol-error-reason condition)))))
             (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
             (assert (zerop (length (agent-protocol-test-pipe-bytes pipe)))))
        (agent-protocol-close client))))
  nil)

(-> run-agent-protocol-access-tests () boolean)
(defun run-agent-protocol-access-tests ()
  "Exercise typed access decisions through serialized byte channels and the driver."
  (dolist (state '(:denied :approved-pending-restart :failed :expired :active))
    (agent-access-test--exchange
     (list (agent-access-test--decision ':pending-approval)
           (agent-access-test--decision
            state :revision (if (eq state ':approved-pending-restart) 5 4)
            :grants         (when (eq state ':active)
                              (agent-wire:welcome.active-grants (agent-access-test--welcome))))) nil))
  (dolist (code '(:permission-denied :cancelled :unavailable :revision-conflict))
    (agent-access-test--exchange (list (agent-wire:make-envelope :failure
                                            (agent-wire:make-failure :code code))) nil))
  (dolist (response
            (list (agent-access-test--decision ':state-unspecified)
                  (agent-access-test--decision ':active :revision 3)
                  (agent-access-test--decision ':active :revision 5
                    :grants                                       (agent-wire:welcome.active-grants (agent-access-test--welcome)))
                  (agent-access-test--decision ':active :grants (list (agent-access-test--grant)))
                  (agent-access-test--decision ':denied :grants (list (agent-access-test--grant)))
                  (agent-access-test--decision ':approved-pending-restart)
                  (agent-wire:make-envelope :failure (agent-wire:make-failure :code ':code-unspecified))
                  (agent-wire:make-envelope :record-response (agent-wire:make-record-response))))
    (agent-access-test--exchange (list response) t))
  (agent-access-test--exchange
   (list (agent-access-test--decision ':pending-approval :revision 5)
         (agent-access-test--decision ':denied :revision 4)) t)
  (agent-access-test--cancel)
  (agent-access-test--invalid-request)
  (agent-access-test--legacy-path)
  (dolist (state '(:active :denied :approved-pending-restart))
    (agent-access-test--observer state)
    (agent-access-test--observer state :path-p t))
  t)
