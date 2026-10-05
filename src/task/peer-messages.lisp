(in-package #:autolith)

;;;; -- Explicit Peer Authority --

(define-condition peer-message-error (task-error)
  ((reason :initarg :reason :reader peer-message-error-reason
           :documentation "Non-disclosing peer policy refusal reason."))
  (:documentation "A peer operation lacks an active grant or current endpoint identity."))

(defclass peer-message-service ()
  ((session :initarg :session :reader peer-message-session
            :documentation "Root primary conversation identity.")
   (owner :initarg :owner :accessor peer-message-owner
          :documentation "The actual primary agent permitted to grant authority.")
   (orchestrator :initarg :orchestrator :accessor peer-message-orchestrator
                 :documentation "Ordinary task identity and ownership authority.")
   (path :initarg :path :reader peer-message-path
         :documentation "Atomic portable mailbox/grant snapshot.")
   (grants :initform nil :accessor peer-message-grants
           :documentation "Bounded durable explicit directional peer grants.")
   (mailbox :accessor peer-message-mailbox
            :documentation "Library bounded acknowledged mailbox.")
   (lock :initform (make-recursive-lock "Peer authority") :reader peer-message-lock
         :documentation "Serializes authority checks with durable admission.")
   (context-claims :initform (make-hash-table :test #'equal) :reader peer-message-context-claims
                   :documentation "Ephemeral steering ID to durable message/token mapping.")
   (remote-call :initarg :remote-call :initform nil :accessor peer-message-remote-call
                :documentation "Optional authenticated daemon transport supplied by the product host."))
  (:documentation "Explicit primary-owned peer grants around the generic durable mailbox."))

(defvar *peer-message-services* (make-hash-table :test #'equal)
  "Root conversation IDs associated with current in-process peer services.")
(defvar *peer-message-services-lock* (make-lock "Peer services")
  "Protects registration and lookup by root conversation identity.")

(-> peer-message--reject (keyword) null)
(defun peer-message--reject (reason)
  "Refuse a peer operation without revealing unrelated job or message data."
  (error 'peer-message-error :reason reason :message (format nil "Peer operation refused: ~S." reason)
                             :tool-name "peer"))

(-> peer-message--identifier (t) string)
(defun peer-message--identifier (value)
  "Require a bounded stable endpoint/message/grant identifier."
  (unless (and (stringp value) (<= 1 (length value) 256))
    (peer-message--reject ':identity))
  value)

(-> peer-message--endpoint (t) list)
(defun peer-message--endpoint (endpoint)
  "Validate every exact endpoint field and reject ambiguous or unbounded identities."
  (unless (and (listp endpoint)
               (do ((tail endpoint (rest tail)) (count 0 (1+ count)))
                   ((= count 6) (null tail))
                 (unless (consp tail) (return nil)))
               (equal (loop for (key value) on endpoint by #'cddr
                            collect key)
                      '(:session :job :execution)))
    (peer-message--reject ':identity))
  (peer-message--identifier (getf endpoint :session))
  (let ((job (getf endpoint :job)) (execution (getf endpoint :execution)))
    (unless (or (and (null job) (null execution))
                (and job execution
                     (peer-message--identifier job)
                     (peer-message--identifier execution)))
      (peer-message--reject ':identity)))
  endpoint)

(-> peer-message--copy-endpoint (list) list)
(defun peer-message--copy-endpoint (endpoint)
  "Detach endpoint strings as well as its property list from mutable caller data."
  (peer-message--endpoint endpoint)
  (list :session (copy-seq (getf endpoint :session))
        :job (when (getf endpoint :job) (copy-seq (getf endpoint :job)))
        :execution (when (getf endpoint :execution) (copy-seq (getf endpoint :execution)))))

(-> peer-message--endpoint-id (list) string)
(defun peer-message--endpoint-id (endpoint)
  "Encode kind and length-delimited root/job/execution fields without collisions."
  (peer-message--endpoint endpoint)
  (let ((session (getf endpoint :session))
        (job (getf endpoint :job))
        (execution (getf endpoint :execution)))
    (peer-message--identifier
     (if job
         (format nil "C~D:~A~D:~A~D:~A" (length session) session
                 (length job) job (length execution) execution)
         (format nil "P~D:~A" (length session) session)))))

(defmethod initialize-instance :after ((actor task-child-agent) &key)
  "Capture the child actor's peer identity before its mutable job can be replaced."
  (let ((job (task-child-agent-job actor)))
    (flet ((capture-field (value)
             (if (stringp value) (copy-seq value) value)))
      (setf (task-child-agent-peer-endpoint actor)
            (list :session (capture-field (session-job-root-conversation-identifier job))
                  :job (capture-field (session-job-identifier job))
                  :execution (capture-field (session-job-execution-identifier job)))))))

(-> peer-message--actor-session (agent) string)
(defun peer-message--actor-session (actor)
  "Derive the root session from the captured actor identity rather than caller arguments."
  (if (typep actor 'task-child-agent)
      (getf (task-child-agent-peer-endpoint actor) :session)
      (conversation-identifier (agent-conversation actor))))

(-> peer-message--actor-endpoint (agent) list)
(defun peer-message--actor-endpoint (actor)
  "Return the captured actor identity, refusing retained actors after job replacement."
  (if (typep actor 'task-child-agent)
      (let ((endpoint (task-child-agent-peer-endpoint actor))
            (job (task-child-agent-job actor)))
        (unless (and (equal (getf endpoint :session) (session-job-root-conversation-identifier job))
                     (equal (getf endpoint :job) (session-job-identifier job))
                     (equal (getf endpoint :execution) (session-job-execution-identifier job)))
          (peer-message--reject ':endpoint-lifecycle))
        (peer-message--copy-endpoint endpoint))
      (list :session (conversation-identifier (agent-conversation actor)) :job nil :execution nil)))

(-> peer-message--owner (peer-message-service agent) null)
(defun peer-message--owner (service actor)
  "Require SERVICE's exact primary agent for authority changes and recovery decisions."
  (unless (and (eq actor (peer-message-owner service)) (not (typep actor 'task-child-agent)))
    (peer-message--reject ':primary-authority))
  nil)

(-> peer-message--local-endpoint (peer-message-service agent string) list)
(defun peer-message--local-endpoint (service owner identifier)
  "Resolve a primary-visible job and capture its immutable execution identity."
  (peer-message--owner service owner)
  (if (equal identifier "primary")
      (peer-message--actor-endpoint owner)
      (let ((job (task-orchestrator-find-visible-job (peer-message-orchestrator service)
                                                   identifier owner "peer.grant")))
        (unless (and (typep job 'task-job) (not (job-terminal-p job)))
          (peer-message--reject ':endpoint-lifecycle))
        (list :session (peer-message-session service) :job (session-job-identifier job)
              :execution (session-job-execution-identifier job)))))

(-> peer-message--current-p (peer-message-service list &key (:live-p boolean)) boolean)
(defun peer-message--current-p (service endpoint &key (live-p t))
  "Check exact local job identity. Remote identity is checked again by its owning host."
  (not (null
        (if (not (equal (getf endpoint :session) (peer-message-session service)))
            t
            (if (null (getf endpoint :job))
                (and (null (getf endpoint :execution))
                     (equal (peer-message--actor-session (peer-message-owner service))
                            (peer-message-session service)))
                (let ((job (find (getf endpoint :job)
                                 (task-orchestrator-list-visible-jobs
                                  (peer-message-orchestrator service) (peer-message-owner service))
                                 :key #'session-job-identifier :test #'equal)))
                  (and (typep job 'task-job)
                       (equal (getf endpoint :execution) (session-job-execution-identifier job))
                       (or (not live-p) (not (job-terminal-p job))))))))))

(-> peer-message--grant (peer-message-service string string &key (:live-p boolean)) list)
(defun peer-message--grant (service sender receiver &key (live-p t))
  "Require an active exact directional grant and current endpoints."
  (or (find-if (lambda (grant)
                 (and (getf grant :active)
                      (equal sender (peer-message--endpoint-id (getf grant :sender)))
                      (equal receiver (peer-message--endpoint-id (getf grant :receiver)))
                      (peer-message--current-p service (getf grant :sender) :live-p live-p)
                      (peer-message--current-p service (getf grant :receiver) :live-p live-p)))
               (peer-message-grants service))
      (peer-message--reject ':peer-scope)))

(defparameter *peer-message-snapshot-maximum-octets* (* 64 1024 1024)
  "Maximum UTF-8 snapshot size accepted by both publication and recovery.")

(-> peer-message--save (peer-message-service list list) pathname)
(defun peer-message--save (service grants mailbox)
  "Atomically publish bounded authority and mailbox state before visibility."
  (let ((text (format nil "~A~%" (task--write-readable-sexp
                                  (list :version 1 :session (peer-message-session service)
                                        :grants grants :mailbox mailbox)))))
    (when (> (length (utf8-string-to-octets text)) *peer-message-snapshot-maximum-octets*)
      (peer-message--reject ':storage-limit))
    (ensure-directories-exist (peer-message-path service))
    (snapshot-write-text (peer-message-path service) text)))

(-> peer-message-service-create
    (agent task-orchestrator &key (:path (option pathname)) (:clock function)
                                  (:remote-call (option function))) peer-message-service)
(defun peer-message-service-create
    (owner orchestrator &key path (clock #'get-universal-time) remote-call)
  "Restore one primary-owned mailbox; interrupted deliveries become explicit UNKNOWN."
  (when (typep owner 'task-child-agent) (peer-message--reject ':primary-authority))
  (let* ((session (peer-message--actor-session owner))
         (path (or path (make-pathname :type "peers.sexp" :defaults (conversation-pathname (agent-conversation owner)))))
         (record (when (probe-file path)
                   (snapshot-read-record path :grammar (task--result-grammar)
                                          :maximum-octets *peer-message-snapshot-maximum-octets*
                                          :properties-p t :keyword-keys-p t)))
         (service (make-instance 'peer-message-service :session session :owner owner
                                 :orchestrator orchestrator :path path :remote-call remote-call)))
    (when record
      (unless (and (eql (getf record :version) 1) (equal (getf record :session) session)
                   (listp (getf record :grants)) (<= (length (getf record :grants)) 128))
        (peer-message--reject ':storage))
      (dolist (grant (getf record :grants))
        (peer-message--identifier (getf grant :id))
        (dolist (endpoint (list (getf grant :sender) (getf grant :receiver)))
          (peer-message--identifier (getf endpoint :session))
          (peer-message--endpoint-id endpoint))
        (unless (and (typep (getf grant :active) 'boolean) (typep (getf grant :context-p) 'boolean))
          (peer-message--reject ':storage)))
      (setf (peer-message-grants service) (getf record :grants)))
    (setf (peer-message-mailbox service)
          (cl-jobpond:make-mailbox :capacity 64 :history-limit 1024 :clock clock
                                  :snapshot (getf record :mailbox)
                                  :store (lambda (mailbox)
                                           (peer-message--save service (peer-message-grants service) mailbox))))
    service))

(-> peer-message-service-register (peer-message-service) peer-message-service)
(defun peer-message-service-register (service)
  "Register the current logical session for child tools and safe-boundary context callbacks."
  (with-lock-held (*peer-message-services-lock*)
    (setf (gethash (peer-message-session service) *peer-message-services*) service))
  service)

(-> peer-message-service-find (agent) (option peer-message-service))
(defun peer-message-service-find (actor)
  "Find only the root session associated with ACTOR's real identity."
  (with-lock-held (*peer-message-services-lock*)
    (gethash (peer-message--actor-session actor) *peer-message-services*)))

(-> peer-message-grant
    (peer-message-service agent &key (:id string) (:sender list) (:receiver list) (:context-p boolean)) list)
(defun peer-message-grant (service owner &key id sender receiver (context-p nil))
  "Install a durable directional grant with exact endpoint identities, never wildcard peers."
  (peer-message--owner service owner)
  (peer-message--identifier id)
  (unless (typep context-p 'boolean) (peer-message--reject ':identity))
  (dolist (endpoint (list sender receiver))
    (peer-message--endpoint endpoint)
    (peer-message--endpoint-id endpoint)
    (unless (peer-message--current-p service endpoint) (peer-message--reject ':endpoint-lifecycle)))
  (unless (or (equal (getf sender :session) (peer-message-session service))
              (equal (getf receiver :session) (peer-message-session service)))
    (peer-message--reject ':peer-scope))
  (with-recursive-lock-held ((peer-message-lock service))
    (let* ((record (list :id id :sender (copy-tree sender) :receiver (copy-tree receiver)
                         :context-p context-p :active t))
           (old (find id (peer-message-grants service) :key (lambda (grant) (getf grant :id)) :test #'equal)))
      (when old
        (unless (equal old record) (peer-message--reject ':grant-conflict))
        (return-from peer-message-grant (copy-tree old)))
      (when (>= (length (peer-message-grants service)) 128) (peer-message--reject ':grant-capacity))
      (let ((grants (append (peer-message-grants service) (list record))))
        (peer-message--save service grants (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)))
        (setf (peer-message-grants service) grants))
      (copy-tree record))))

(-> peer-message-revoke (peer-message-service agent string) list)
(defun peer-message-revoke (service owner id)
  "Revoke a grant durably, stopping discovery, send, delivery, acknowledgment and waits."
  (peer-message--owner service owner)
  (with-recursive-lock-held ((peer-message-lock service))
    (let* ((grants (copy-tree (peer-message-grants service)))
           (grant (find id grants :key (lambda (entry) (getf entry :id)) :test #'equal)))
      (unless grant (peer-message--reject ':grant))
      (setf (getf grant :active) nil)
      (peer-message--save service grants (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)))
      (setf (peer-message-grants service) grants)
      grant)))

(-> peer-message-discover (peer-message-service agent) list)
(defun peer-message-discover (service actor)
  "Return only ACTOR's granted current peers, not a global job inventory."
  (let ((identity (peer-message--endpoint-id (peer-message--actor-endpoint actor))))
    (unless (equal (peer-message--actor-session actor) (peer-message-session service))
      (peer-message--reject ':session))
    (with-recursive-lock-held ((peer-message-lock service))
      (loop for grant in (peer-message-grants service)
            when (and (getf grant :active)
                      (peer-message--current-p service (getf grant :sender) :live-p nil)
                      (peer-message--current-p service (getf grant :receiver) :live-p nil)
                      (or (equal identity (peer-message--endpoint-id (getf grant :sender)))
                          (equal identity (peer-message--endpoint-id (getf grant :receiver)))))
              collect (copy-tree grant)))))

(-> peer-message--text (t) string)
(defun peer-message--text (text)
  "Bound peer payloads before mailbox or remote transport admission."
  (unless (and (stringp text) (<= 1 (length text) 8192)) (peer-message--reject ':payload-limit))
  text)

(-> peer-message-send (peer-message-service agent &key (:id string) (:receiver string) (:text string)) list)
(defun peer-message-send (service actor &key id receiver text)
  "Send as the actual actor; uncertain transport claims need an explicit recovery decision."
  (peer-message--identifier id)
  (peer-message--text text)
  (unless (equal (peer-message--actor-session actor) (peer-message-session service))
    (peer-message--reject ':session))
  (let ((sender (peer-message--endpoint-id (peer-message--actor-endpoint actor)))
        (grant nil) (delivery nil))
    (with-recursive-lock-held ((peer-message-lock service))
      (setf grant (peer-message--grant service sender receiver))
      (let ((message (cl-jobpond:mailbox-send (peer-message-mailbox service)
                                            :id id :sender sender :receiver receiver :payload text)))
        (when (or (equal (getf (getf grant :receiver) :session) (peer-message-session service))
                  (not (eq (getf message :state) ':queued)))
          (return-from peer-message-send message))
        (unless (peer-message-remote-call service) (peer-message--reject ':transport-unavailable))
        (setf delivery (cl-jobpond:mailbox-receive (peer-message-mailbox service) :receiver receiver :id id))))
    (handler-case
        (let ((remote (funcall (peer-message-remote-call service) (getf (getf grant :receiver) :session)
                               (list :action ':send :id id :sender sender :receiver receiver :payload text)
                               :cancelled-p (when (typep actor 'task-child-agent)
                                              (lambda () (job-cancellation-reason (task-child-agent-job actor)))))))
          (when (eq (getf remote :state) ':acknowledged)
            (with-recursive-lock-held ((peer-message-lock service))
              (peer-message--visible-message service actor id)
              (cl-jobpond:mailbox-ack (peer-message-mailbox service) :id id :receiver receiver
                                      :token (getf delivery :token) :result ':remote-acknowledged)))
          (cl-jobpond:mailbox-find (peer-message-mailbox service) id))
      (error (condition)
        (with-recursive-lock-held ((peer-message-lock service))
          (cl-jobpond:mailbox-resolve (peer-message-mailbox service) :id id :receiver receiver
                                      :token (getf delivery :token) :action ':unknown))
        (error condition)))))

(-> peer-message--incoming (peer-message-service agent &key (:id (option string)) (:queued-p boolean)) (option list))
(defun peer-message--incoming (service actor &key id (queued-p nil))
  "Select only a granted incoming message owned by the actual receiver."
  (unless (equal (peer-message--actor-session actor) (peer-message-session service))
    (peer-message--reject ':session))
  (let* ((receiver (peer-message--endpoint-id (peer-message--actor-endpoint actor)))
         (messages (getf (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)) :messages)))
    (loop for message in messages
          when (and (or (null id) (equal id (getf message :id)))
                    (equal receiver (getf message :receiver))
                    (or (not queued-p) (eq (getf message :state) ':queued)))
            do (let ((permitted-p
                       (handler-case
                           (let ((grant (peer-message--grant service (getf message :sender) receiver :live-p nil)))
                             (when (and queued-p (not (peer-message--current-p service (getf grant :receiver))))
                               (peer-message--reject ':endpoint-lifecycle))
                             t)
                         (peer-message-error (condition)
                           (when id (error condition))
                           nil))))
                 (when permitted-p (return message))))))

(-> peer-message-receive (peer-message-service agent &key (:id (option string))) (option list))
(defun peer-message-receive (service actor &key id)
  "Claim one authorized message; unknown deliveries require explicit primary recovery."
  (with-recursive-lock-held ((peer-message-lock service))
    (let ((message (peer-message--incoming service actor :id id :queued-p t)))
      (when message
        (cl-jobpond:mailbox-receive (peer-message-mailbox service)
                                  :receiver (getf message :receiver) :id (getf message :id))))))

(-> peer-message-ack (peer-message-service agent &key (:id string) (:token integer)) list)
(defun peer-message-ack (service actor &key id token)
  "Acknowledge the exact actual receiver and its durable delivery token."
  (with-recursive-lock-held ((peer-message-lock service))
    (let ((message (peer-message--incoming service actor :id id)))
      (unless message (peer-message--reject ':message-scope))
      (cl-jobpond:mailbox-ack (peer-message-mailbox service)
                            :id id :receiver (getf message :receiver) :token token :result ':acknowledged))))

(-> peer-message--visible-message (peer-message-service agent string) list)
(defun peer-message--visible-message (service actor id)
  "Require exact sender or receiver identity and its active directional grant."
  (let* ((identity (peer-message--endpoint-id (peer-message--actor-endpoint actor)))
         (message (cl-jobpond:mailbox-find (peer-message-mailbox service) id)))
    (unless (and (equal (peer-message--actor-session actor) (peer-message-session service))
                 (or (equal identity (getf message :sender)) (equal identity (getf message :receiver))))
      (peer-message--reject ':message-scope))
    (peer-message--grant service (getf message :sender) (getf message :receiver) :live-p nil)
    message))


(-> peer-message-inspect
    (peer-message-service agent &key (:id (option string)) (:offset integer) (:limit integer)) list)
(defun peer-message-inspect (service actor &key id (offset 0) (limit 16))
  "Expose bounded scoped metadata, including UNKNOWN tokens for explicit recovery decisions."
  (unless (and (integerp offset) (<= 0 offset 1024) (integerp limit) (<= 1 limit 32))
    (peer-message--reject ':page))
  (unless (equal (peer-message--actor-session actor) (peer-message-session service))
    (peer-message--reject ':session))
  (with-recursive-lock-held ((peer-message-lock service))
    (let* ((primary-p (eq actor (peer-message-owner service)))
           (messages (getf (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)) :messages))
           (visible (if primary-p messages
                        (remove-if-not
                         (lambda (message)
                           (handler-case (progn (peer-message--visible-message service actor (getf message :id)) t)
                             (peer-message-error () nil))) messages))))
      (when id
        (let ((message (find id visible :key (lambda (entry) (getf entry :id)) :test #'equal)))
          (unless message (peer-message--reject ':message-scope))
          (return-from peer-message-inspect (list :message message))))
      (let ((end (min (length visible) (+ offset limit))))
        (list :total (length visible) :offset offset :next (when (< end (length visible)) end)
              :messages (loop for message in (subseq visible (min offset (length visible)) end)
                              collect (list :id (getf message :id) :sender (getf message :sender)
                                            :receiver (getf message :receiver) :state (getf message :state)
                                            :token (getf message :token))))))))

(-> peer-message-wait
    (peer-message-service agent &key (:id (option string)) (:timeout real) (:cancelled-p (option function))) t)
(defun peer-message-wait (service actor &key id (timeout 2) cancelled-p)
  "Bound local/remote waits and recheck exact authority before returning any message."
  (unless (and (realp timeout) (<= 0 timeout 5)) (peer-message--reject ':wait-budget))
  (let ((receiver (peer-message--endpoint-id (peer-message--actor-endpoint actor)))
        (original nil) (grant nil))
    (with-recursive-lock-held ((peer-message-lock service))
      (if id
          (setf original (peer-message--visible-message service actor id)
                grant (peer-message--grant service (getf original :sender) (getf original :receiver) :live-p nil))
          (unless (peer-message-discover service actor) (peer-message--reject ':peer-scope))))
    (when (and original (not (equal (getf (getf grant :receiver) :session) (peer-message-session service))))
      (unless (peer-message-remote-call service) (peer-message--reject ':transport-unavailable))
      (let ((remote (funcall (peer-message-remote-call service) (getf (getf grant :receiver) :session)
                             (list :action (if (zerop timeout) ':status ':wait) :id id
                                   :sender (getf original :sender) :receiver (getf original :receiver)
                                   :payload (min timeout 1)) :cancelled-p cancelled-p)))
        (with-recursive-lock-held ((peer-message-lock service))
          (setf original (peer-message--visible-message service actor id))
          (when (and (member (getf original :state) '(:delivered :unknown))
                     (eq (getf remote :state) ':acknowledged))
            (setf original (cl-jobpond:mailbox-ack (peer-message-mailbox service)
                                                 :id id :receiver (getf original :receiver)
                                                 :token (getf original :token) :result ':remote-acknowledged)))
          (return-from peer-message-wait (list :state (getf remote :state) :message original)))))
    (multiple-value-bind (message state)
        (apply #'cl-jobpond:mailbox-wait (peer-message-mailbox service)
               :timeout timeout
               :cancelled-p (lambda ()
                              (or (and cancelled-p (funcall cancelled-p))
                                  (handler-case
                                      (with-recursive-lock-held ((peer-message-lock service))
                                        (if id (progn (peer-message--visible-message service actor id) nil)
                                            (null (peer-message-discover service actor))))
                                    (peer-message-error () t))))
               (if id (list :id id) (list :receiver receiver)))
      (when message
        (with-recursive-lock-held ((peer-message-lock service))
          (peer-message--visible-message service actor (getf message :id))))
      (list :state state :message message))))

(-> peer-message-resolve
    (peer-message-service agent &key (:id string) (:token integer) (:action keyword)) list)
(defun peer-message-resolve (service owner &key id token action)
  "Explicit primary decision on uncertain delivery, including after endpoint termination."
  (peer-message--owner service owner)
  (with-recursive-lock-held ((peer-message-lock service))
    (let ((message (cl-jobpond:mailbox-find (peer-message-mailbox service) id)))
      (case action
        (:acknowledge (cl-jobpond:mailbox-ack (peer-message-mailbox service)
                                            :id id :receiver (getf message :receiver) :token token :result ':primary-resolution))
        (:retry
         (peer-message--grant service (getf message :sender) (getf message :receiver))
         (cl-jobpond:mailbox-resolve (peer-message-mailbox service)
                                     :id id :receiver (getf message :receiver) :token token :action ':retry))
        (:cancel (cl-jobpond:mailbox-cancel (peer-message-mailbox service) :id id :sender (getf message :sender)))
        (otherwise (peer-message--reject ':resolution))))))

;;;; -- Safe-Boundary Context Delivery --

(-> peer-message-take-context ((option agent)) list)
(defun peer-message-take-context (actor)
  "Claim granted context messages at a steering boundary; NIL means no connected agent."
  (let ((service (and actor (peer-message-service-find actor))) (entries nil))
    (when service
      (with-recursive-lock-held ((peer-message-lock service))
        (let ((receiver (peer-message--endpoint-id (peer-message--actor-endpoint actor))))
          (dolist (message (getf (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)) :messages))
            (when (and (eq (getf message :state) ':queued) (equal receiver (getf message :receiver)))
              (let ((grant (handler-case (peer-message--grant service (getf message :sender) receiver :live-p nil)
                             (peer-message-error () nil))))
                (when (and grant (getf grant :context-p))
                  (let* ((delivery (peer-message-receive service actor :id (getf message :id)))
                         (identifier (make-identifier)))
                    (setf (gethash identifier (peer-message-context-claims service))
                          (list :id (getf delivery :id) :token (getf delivery :token) :receiver receiver))
                    (push (agent-steering-input-create
                           :identifier identifier
                           :content (format nil "Peer coordination data from ~A (message ~A):~%~A"
                                            (getf message :sender) (getf message :id) (getf message :payload))) entries)))))))))
    (nreverse entries)))

(-> peer-message-ack-context ((option agent) string) boolean)
(defun peer-message-ack-context (actor identifier)
  "Acknowledge persisted peer input for ACTOR, or return NIL without a connected agent."
  (let ((service (and actor (peer-message-service-find actor))))
    (when service
      (with-recursive-lock-held ((peer-message-lock service))
        (let ((claim (gethash identifier (peer-message-context-claims service))))
          (when (and claim (equal (getf claim :receiver)
                                  (peer-message--endpoint-id (peer-message--actor-endpoint actor))))
            (peer-message-ack service actor :id (getf claim :id) :token (getf claim :token))
            (remhash identifier (peer-message-context-claims service))
            (return-from peer-message-ack-context t))))))
  nil)

(-> peer-message-service-close (peer-message-service) null)
(defun peer-message-service-close (service)
  "Finish a session without silently discarding uncertain delivery or duplicate history."
  (with-recursive-lock-held ((peer-message-lock service))
    (cl-jobpond:mailbox-close (peer-message-mailbox service)))
  nil)

;;;; -- Authenticated Daemon Composition --

(-> peer-message-dispatch (peer-message-service list &key (:cancelled-p (option function))) t)
(defun peer-message-dispatch (service envelope &key cancelled-p)
  "Dispatch send/status/wait through paired grants; transport receipt is not semantic ACK."
  (when (and cancelled-p (funcall cancelled-p)) (peer-message--reject ':cancelled))
  (peer-message--identifier (getf envelope :id))
  (let ((action (getf envelope :action)) (message nil))
    (with-recursive-lock-held ((peer-message-lock service))
      (let ((grant (peer-message--grant service (getf envelope :sender) (getf envelope :receiver)
                                       :live-p (eq action ':send))))
        (unless (equal (getf (getf grant :receiver) :session) (peer-message-session service))
          (peer-message--reject ':session)))
      (case action
        (:send
         (return-from peer-message-dispatch
           (cl-jobpond:mailbox-send (peer-message-mailbox service) :id (getf envelope :id)
                                   :sender (getf envelope :sender) :receiver (getf envelope :receiver)
                                   :payload (peer-message--text (getf envelope :payload)))))
        ((:status :wait)
         (setf message (cl-jobpond:mailbox-find (peer-message-mailbox service) (getf envelope :id)))
         (unless (and (equal (getf message :sender) (getf envelope :sender))
                      (equal (getf message :receiver) (getf envelope :receiver)))
           (peer-message--reject ':message-scope)))
        (otherwise (peer-message--reject ':remote-action))))
    (when (eq action ':wait)
      (unless (and (realp (getf envelope :payload)) (<= 0 (getf envelope :payload) 1))
        (peer-message--reject ':wait-budget))
      (cl-jobpond:mailbox-wait (peer-message-mailbox service) :id (getf envelope :id)
                             :timeout (getf envelope :payload) :cancelled-p cancelled-p))
    (with-recursive-lock-held ((peer-message-lock service))
      (peer-message--grant service (getf envelope :sender) (getf envelope :receiver) :live-p nil)
      (cl-jobpond:mailbox-find (peer-message-mailbox service) (getf envelope :id)))))

(-> peer-message-daemon-call (configuration string list &key (:cancelled-p (option function))) t)
(defun peer-message-daemon-call (configuration session envelope &key cancelled-p)
  "Use authenticated daemon discovery/transport, preserving the same stable message ID."
  (let ((record (find session (localgroup-endpoint-records configuration)
                      :key (lambda (entry) (getf (rest (rest entry)) :session-id)) :test #'equal)))
    (unless record (peer-message--reject ':transport-unavailable))
    (image-daemon:daemon-message-call (rest record) :action (getf envelope :action) :id (getf envelope :id)
                                                  :sender (getf envelope :sender) :receiver (getf envelope :receiver)
                                                  :payload (getf envelope :payload) :timeout 2
                                                  :cancelled-p cancelled-p :character-limit 32768)))

(-> application-peer-message-dispatch (application list &key (:cancelled-p (option function))) t)
(defun application-peer-message-dispatch (application envelope &key cancelled-p)
  "Compose daemon messaging with primary-owned peer grants and named mission events."
  (if (eq (getf envelope :action) ':event)
      (progn
        (unless (and (equal (getf envelope :receiver)
                            (conversation-identifier (application-conversation application)))
                     (stringp (getf envelope :payload)))
          (peer-message--reject ':event-scope))
        (application-mission-schedule-event application (getf envelope :payload) (getf envelope :id)))
      (let* ((actor (application-peer-messages--actor application))
             (service (and actor (peer-message-service-find actor))))
        (unless service (peer-message--reject ':peer-scope))
        (peer-message-dispatch service envelope :cancelled-p cancelled-p))))

(-> application-peer-messages--actor (application) (option agent))
(defun application-peer-messages--actor (application)
  "Return a connected peer actor, or NIL while APPLICATION has no agent."
  (let ((actor (when (slot-boundp application 'agent)
                 (application-agent application))))
    (when (typep actor 'agent)
      actor)))

(-> application-peer-messages-close (application) null)
(defun application-peer-messages-close (application)
  "Close an existing session mailbox on final session finish, not transient daemon handoff."
  (let* ((actor (application-peer-messages--actor application))
         (service (and actor (peer-message-service-find actor))))
    (when service (peer-message-service-close service)))
  nil)


(-> application-peer-messages-recover (application) null)
(defun application-peer-messages-recover (application)
  "Restore durable grants and mark interrupted deliveries unknown after application reconstruction."
  (let* ((owner (application-peer-messages--actor application))
         (orchestrator
           (and owner (slot-boundp application 'tool-registry)
                (typep (application-tool-registry application) 'tool-registry)
                (tool-registry-runtime-binding (application-tool-registry application) 'task-orchestrator))))
    (when (and (typep owner 'agent) (typep orchestrator 'task-orchestrator))
      (peer-message-service-register
       (peer-message-service-create
        owner orchestrator
        :remote-call (lambda (session envelope &key cancelled-p)
                       (peer-message-daemon-call (application-configuration application) session envelope
                                                 :cancelled-p cancelled-p))))))
  nil)


(-> application-peer-messages-ensure (application) null)
(defun application-peer-messages-ensure (application)
  "Idempotently rebind live authority after registry/agent reconstruction, without replaying deliveries."
  (when (and (slot-boundp application 'agent) (slot-boundp application 'tool-registry)
             (slot-boundp application 'conversation) (slot-boundp application 'configuration))
    (let* ((owner (application-agent application))
           (registry (application-tool-registry application))
           (orchestrator (and (typep registry 'tool-registry)
                              (tool-registry-runtime-binding registry 'task-orchestrator))))
      (when (and (typep owner 'agent) (not (typep owner 'task-child-agent))
                 (typep orchestrator 'task-orchestrator))
        (let* ((session (peer-message--actor-session owner))
               (path (make-pathname :type "peers.sexp" :defaults (conversation-pathname (agent-conversation owner))))
               (remote-call (lambda (target envelope &key cancelled-p)
                              (peer-message-daemon-call (application-configuration application) target envelope
                                                        :cancelled-p cancelled-p))))
          (with-lock-held (*peer-message-services-lock*)
            (let ((service (gethash session *peer-message-services*)))
              (if (and service (equal path (peer-message-path service)))
                  (with-recursive-lock-held ((peer-message-lock service))
                    (setf (peer-message-owner service) owner
                          (peer-message-orchestrator service) orchestrator
                          (peer-message-remote-call service) remote-call))
                  (setf (gethash session *peer-message-services*)
                        (peer-message-service-create owner orchestrator :path path :remote-call remote-call)))))))))
  nil)
