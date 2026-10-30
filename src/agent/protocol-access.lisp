;;;; -- Agent Access Policy Client --

(in-package #:autolith)

(defclass agent-access-request ()
  ((message :initarg :message :reader agent-access-request-message
            :documentation "Detached requested batch and operation identity.")
   (revision :initarg :revision :accessor agent-access-request-revision
             :documentation "Last policy revision reported for this request."))
  (:documentation "Validation state for one correlated access.v1 response stream."))

(defclass agent-access-observer (agent-protocol-observer)
  ((command-grants :initarg :command-grants :reader agent-access-observer-command-grants
                   :documentation "Trusted function of command and directory returning exact grants.")
   (cancelled-p :initarg :cancelled-p :reader agent-access-observer-cancelled-p
                :documentation "Nonblocking predicate for turn cancellation."))
  (:documentation "Ask the parent for access without a local terminal approval path."))

(define-condition agent-access-restart-required (error)
  ((revision :initarg :revision :reader agent-access-restart-required-revision
             :documentation "Approved policy revision which needs a fresh instance."))
  (:documentation "Approval is recorded but does not authorize this instance.")
  (:report (lambda (condition stream)
             (format stream "Access policy revision ~D requires restart."
                     (agent-access-restart-required-revision condition)))))

(-> agent-access--text-p (t integer) boolean)
(defun agent-access--text-p (value maximum)
  "Require nonempty bounded text without control characters."
  (and (stringp value) (<= 1 (length value) maximum)
       (every (lambda (character) (>= (char-code character) 32)) value)))

(-> agent-access--grant-p (agent-wire:grant) boolean)
(defun agent-access--grant-p (grant)
  "Validate the typed grant shape; the parent resolves and binds each target."
  (let ((resource   (agent-wire:grant.resource grant))
        (target     (agent-wire:grant.target grant))
        (operations (agent-wire:grant.operations grant)))
    (and (member resource '(:filesystem :network :service))
         (member (agent-wire:grant.duration grant) '(:instance :session))
         (agent-access--text-p target 4096)
         (or (not (eq resource ':filesystem))
             (uiop:absolute-pathname-p (pathname target)))
         (<= 1 (length operations) 3)
         (= (length operations) (length (remove-duplicates operations :test #'equal)))
         (every (lambda (operation)
                  (member operation (if (eq resource ':filesystem)
                                        '("read" "write" "execute")
                                        '("connect"))
                          :test #'equal))
                operations)
         t)))

(-> agent-access--key (agent-wire:grant) list)
(defun agent-access--key (grant)
  "Return the resource identity of one grant, excluding its operation set."
  (list (agent-wire:grant.resource grant) (agent-wire:grant.target grant)
        (agent-wire:grant.duration grant)))

(-> agent-access--batch-p (list) boolean)
(defun agent-access--batch-p (grants)
  "Check bounded batches and reject duplicate resource identities."
  (and (<= (length grants) 64)
       (every #'agent-access--grant-p grants)
       (= (length grants)
          (length (remove-duplicates grants :test #'equal :key #'agent-access--key)))))

(-> agent-access--covered-p (list list) boolean)
(defun agent-access--covered-p (requested active)
  "Require every operation on each resource identity, including accumulated grants."
  (every (lambda (grant)
           (every (lambda (operation)
                    (some (lambda (candidate)
                            (and (equal (agent-access--key grant) (agent-access--key candidate))
                                 (member operation (agent-wire:grant.operations candidate) :test #'equal)))
                          active))
                  (agent-wire:grant.operations grant)))
         requested))

(-> agent-protocol-access-request (agent-protocol-client agent-wire:access-request) integer)
(defun agent-protocol-access-request (client request)
  "Send one detached atomic batch. Consume replies through the existing driver.
PENDING-APPROVAL is nonterminal. Never retry automatically. Operation identities
are session scoped; the parent owns their durable payload and result ledger."
  (let ((copy (agent-protocol--message-copy request)))
    (unless (and (agent-access--text-p (agent-wire:access-request.operation-id copy) 256)
                 (<= (length (agent-wire:access-request.reason copy)) 8192)
                 (agent-wire:access-request.requested-grants copy)
                 (agent-access--batch-p (agent-wire:access-request.requested-grants copy)))
      (error 'agent-protocol-error :reason ':invalid-access-request))
    (agent-protocol--send
     client
     (lambda ()
       (let ((welcome (agent-protocol-client-welcome client)))
         (unless (member "access.v1" (agent-wire:welcome.features welcome) :test #'equal)
           (error 'agent-protocol-error :reason ':access-not-negotiated))
         (when (agent-protocol-client-stop client)
           (error 'agent-protocol-error :reason ':stopping))
         (when (>= (hash-table-count (agent-protocol-client-pending client))
                   (agent-wire:limits.max-in-flight-requests
                    (autolith-supervisor::parent-connection-limits client)))
           (error 'agent-protocol-error :reason ':request-limit))
         (values (agent-wire:make-envelope :access-request copy)
                 (make-instance 'agent-access-request :message copy
                                :revision                      (agent-wire:welcome.policy-revision welcome))))))))

(defmethod agent-protocol--response-data (client envelope (expected agent-access-request))
  "Validate disposition, revision, and complete batch before delivery."
  (case (agent-wire:envelope.body-case envelope)
    (agent-wire:failure
     (agent-protocol--failure client (agent-wire:envelope.failure envelope))
     t)
    (agent-wire:access-decision
     (let* ((decision  (agent-wire:envelope.access-decision envelope))
            (state     (agent-wire:access-decision.state decision))
            (revision  (agent-wire:access-decision.policy-revision decision))
            (effective (agent-wire:access-decision.effective-grants decision))
            (welcome   (agent-protocol-client-welcome client)))
       (unless (and (member state '(:pending-approval :denied :approved-pending-restart :active :failed :expired))
                    (<= (agent-access-request-revision expected) revision)
                    (<= (length (agent-wire:access-decision.explanation decision)) 8192)
                    (if (eq state ':active)
                        (and (= revision (agent-wire:welcome.policy-revision welcome))
                             (every #'agent-access--grant-p effective)
                             (agent-access--covered-p effective (agent-wire:welcome.active-grants welcome))
                             (agent-access--covered-p (agent-wire:welcome.active-grants welcome) effective))
                        (and (null effective)
                             (or (not (eq state ':approved-pending-restart))
                                 (> revision (agent-wire:welcome.policy-revision welcome))))))
         (agent-protocol--fatal client ':invalid-access-decision))
       (setf (agent-access-request-revision expected) revision)
       (not (eq state ':pending-approval))))
    (otherwise
     (agent-protocol--fatal client ':unexpected-response))))

(-> agent-protocol-access-cancel (agent-protocol-client (integer 1)) integer)
(defun agent-protocol-access-cancel (client request-id)
  "Cancel a prior access request, including a terminal reply racing cancellation.
The caller retains REQUEST-ID from ACCESS-REQUEST. The parent checks its target.
A cancellation acknowledgement does not retire an outstanding access request."
  (agent-protocol--send
   client
   (lambda ()
     (let ((expected (gethash request-id (agent-protocol-client-pending client))))
       (unless (and (<= request-id (autolith-supervisor::parent-connection-sent client))
                    (or (null expected) (typep expected 'agent-access-request)))
         (error 'agent-protocol-error :reason ':unknown-access-request)))
     (when (>= (hash-table-count (agent-protocol-client-pending client))
               (agent-wire:limits.max-in-flight-requests
                (autolith-supervisor::parent-connection-limits client)))
       (error 'agent-protocol-error :reason ':request-limit))
     (values (agent-wire:make-envelope :cancel-request
                                       (agent-wire:make-cancel-request :request-id request-id))
             (list 'agent-wire:cancel-ack)))))

(-> make-agent-access-observer
    (agent-protocol-driver &key (:turn-id function) (:command-grants function) (:cancelled-p function)
                               (:command-policy (member :deny :inherited-scope)))
    agent-access-observer)
(defun make-agent-access-observer (driver &key (turn-id (constantly nil))
                                              (command-grants (lambda (command directory)
                                                                (declare (ignore command directory))
                                                                nil))
                                              (cancelled-p (constantly nil)) (command-policy ':deny))
  "Make a parent-backed observer. COMMAND-GRANTS returns exact typed grants.
The parent validates every returned grant. Explicit inherited command scope uses
only the native policy already installed by the parent before Welcome."
  (make-instance 'agent-access-observer :driver driver :turn-id turn-id
                 :command-grants command-grants :cancelled-p cancelled-p :command-policy command-policy))

(-> agent-access-observer--request (agent-access-observer list string) agent-wire:envelope)
(defun agent-access-observer--request (observer grants reason)
  "Wait on a driver delivery, cancel on turn cancellation, and consume both results."
  (let* ((driver       (agent-protocol-observer-driver observer))
         (operation-id (format nil "agent-access/~A" (make-identifier)))
         (delivery     (agent-protocol-driver-request
                        driver (lambda (peer)
                                 (agent-protocol-access-request
                                  peer (agent-wire:make-access-request :operation-id operation-id
                                                                      :requested-grants grants :reason reason)))))
         (cancellation nil)
         (result       nil))
    (loop
      (multiple-value-bind (reply terminal) (agent-protocol-driver-next delivery :timeout 0.1)
        (when terminal (setf result reply) (return)))
      (when (and (null cancellation) (funcall (agent-access-observer-cancelled-p observer)))
        (setf cancellation
              (agent-protocol-driver-request
               driver (lambda (peer)
                        (agent-protocol-access-cancel peer (agent-protocol-delivery-id delivery)))))))
    (when cancellation
      (loop (multiple-value-bind (reply terminal) (agent-protocol-driver-next cancellation :timeout 0.1)
              (declare (ignore reply))
              (when terminal (return)))))
    result))

(-> agent-access-observer--allowed-p (agent-access-observer list string) boolean)
(defun agent-access-observer--allowed-p (observer grants reason)
  "Authorize only active grants; expose restart approval as a typed condition."
  (let ((reply (agent-access-observer--request observer grants reason)))
    (when (eq (agent-wire:envelope.body-case reply) 'agent-wire:access-decision)
      (let ((decision (agent-wire:envelope.access-decision reply)))
        (when (eq (agent-wire:access-decision.state decision) ':approved-pending-restart)
          (error 'agent-access-restart-required
                 :revision (agent-wire:access-decision.policy-revision decision)))
        (return-from agent-access-observer--allowed-p
          (and (not (funcall (agent-access-observer-cancelled-p observer)))
               (eq (agent-wire:access-decision.state decision) ':active)))))
    nil))

(-> agent-access-observer-authorize-path
    (agent-access-observer pathname &key (:operations list) (:duration keyword) (:reason string)) boolean)
(defun agent-access-observer-authorize-path (observer path &key (operations '("read"))
                                                               (duration ':instance)
                                                               (reason "Tool path access"))
  "Ask the parent for exact PATH access. Return true only for the complete active batch."
  (agent-access-observer--allowed-p
   observer (list (agent-wire:make-grant :resource ':filesystem :target (uiop:native-namestring path)
                                         :operations operations :duration duration)) reason))

(defmethod agent-observer-authorize-command
    ((observer agent-access-observer) (command string) (directory pathname))
  "Run within explicit inherited scope, otherwise ask for an exact parent-approved grant batch."
  (if (eq (agent-protocol-observer-command-policy observer) ':inherited-scope)
      ':inherited-scope
      (let ((grants (funcall (agent-access-observer-command-grants observer) command directory)))
        (if (and grants (agent-access-observer--allowed-p observer grants command))
            ':sandboxed
            ':deny))))

(defmethod agent-observer-authorize-path
    ((observer agent-access-observer) (path pathname) operations)
  "Request exact filesystem operations through the parent access service."
  (agent-access-observer-authorize-path observer path :operations operations))
