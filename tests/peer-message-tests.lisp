(in-package #:autolith)

;;;; -- Granted Peer Product Boundaries --

(-> peer-message-tests--fixture (function) null)
(defun peer-message-tests--fixture (function)
  "Call FUNCTION with a primary, two siblings, normal tools and a durable peer service."
  (with-test-configuration (configuration root)
    (let* ((registry (make-instance 'tool-registry))
           (orchestrator (task-tests--orchestrator))
           (conversation (conversation-create configuration))
           (owner (agent-create :configuration configuration :conversation conversation
                                :provider (make-instance 'model-provider) :tool-registry registry :worker nil))
           (definition (task-agent-definition-create :name "peer-worker" :description "Peer fixture"
                                                     :instructions "Return one bounded result."))
           (jobs (loop repeat 2
                       collect (task-tests--attach-job
                                orchestrator (lambda (id index)
                                               (task-tests--make-job orchestrator :identifier id :index index
                                                                    :definition definition :parent-agent owner
                                                                    :item (list :task "Peer fixture"))))))
           (first-child (task-tests--child-viewer configuration (first jobs) :registry registry))
           (second-child (task-tests--child-viewer configuration (second jobs) :registry registry))
           (path (merge-pathnames "peer-state.sexp" root))
           (service (peer-message-service-create owner orchestrator :path path)))
      (peer-message-service-register service)
      (peer-message-register-tools registry :orchestrator orchestrator)
      (unwind-protect
           (funcall function :configuration configuration :owner owner :sender first-child :receiver second-child
                             :service service :path path :registry registry)
        (remhash (peer-message-session service) *peer-message-services*)
        (task-tests--close-orchestrators))))
  nil)

(-> peer-message-tests--grant (peer-message-service agent &key (:sender agent) (:receiver agent) (:context-p boolean)) list)
(defun peer-message-tests--grant (service owner &key sender receiver context-p)
  "Grant one exact directional peer relation as the owning primary."
  (peer-message-grant service owner :id "first-to-second"
                      :sender (peer-message--actor-endpoint sender) :receiver (peer-message--actor-endpoint receiver)
                      :context-p context-p))

(-> peer-message-tests--context (agent tool-registry) tool-context)
(defun peer-message-tests--context (actor registry)
  "Construct a real agent tool context rather than supplying a sender identity."
  (make-instance 'tool-context :configuration (agent-configuration actor)
                 :conversation (agent-conversation actor) :agent actor :registry registry :worker nil))

(-> test-peer-message-authority () null)
(defun test-peer-message-authority ()
  "Exercise ordinary tool denial, primary grant ownership, sibling scope and revocation."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration path))
     (let ((context (peer-message-tests--context sender registry)))
       (test-assert (null (peer-message-discover service sender)) "ungranted child has no global peer inventory")
       (test-assert (not (tool-result-success-p
                         (tool-registry-execute-call registry
                                                     (agent-test-call :namespace "peer" :name "grant"
                                                                      :arguments "{\"id\":\"escalate\",\"sender\":\"primary\",\"receiver\":\"primary\"}") context)))
                    "the tool boundary rejects child grant escalation")
       (test-assert (not (tool-result-success-p
                         (tool-registry-execute-call registry
                                                     (agent-test-call :namespace "peer" :name "send"
                                                                      :arguments (json-encode (json-object "id" "denied" "receiver" (peer-message--endpoint-id (peer-message--actor-endpoint receiver)) "text" "Denied"))) context)))
                    "ungranted sibling communication is denied")
       (peer-message-tests--grant service owner :sender sender :receiver receiver)
       (test-assert (= 1 (length (peer-message-discover service sender))) "an explicit grant adds exactly one sibling relation")
       (test-assert (not (session-job-visible-to-agent-p (task-child-agent-job receiver) sender))
                    "peer grants do not grant ordinary job ownership")
       (test-assert (tool-result-success-p
                     (tool-registry-execute-call registry
                                                (agent-test-call :namespace "peer" :name "send"
                                                                 :arguments (json-encode (json-object "id" "message" "receiver" (peer-message--endpoint-id (peer-message--actor-endpoint receiver)) "text" "Hello"))) context))
                    "granted messaging uses the normal tool registry")
       (peer-message-revoke service owner "first-to-second")
       (test-assert (null (peer-message-discover service sender)) "revocation removes discovery")
       (test-assert (null (peer-message-receive service receiver)) "untargeted receive skips revoked queued work")
       (test-assert (handler-case (progn (peer-message-receive service receiver :id "message") nil)
                      (peer-message-error () t)) "targeted receive refuses revoked work")
       (peer-message-grant service owner :id "primary-to-second"
                           :sender (peer-message--actor-endpoint owner)
                           :receiver (peer-message--actor-endpoint receiver))
       (peer-message-send service owner :id "later-permitted"
                          :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver))
                          :text "Permitted after revoked work")
       (test-assert (equal "later-permitted" (getf (peer-message-receive service receiver) :id))
                    "revoked queued candidate cannot starve a later permitted message")
       (peer-message-revoke service owner "primary-to-second")
       (test-assert (handler-case (progn (peer-message-wait service receiver :timeout 0) nil)
                      (peer-message-error () t)) "wait also requires current peer scope")))))

(-> test-peer-message-durable-delivery () null)
(defun test-peer-message-durable-delivery ()
  "Exercise stable IDs, actual receiver acknowledgment and explicit uncertain-delivery recovery."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration))
     (peer-message-tests--grant service owner :sender sender :receiver receiver)
     (let* ((endpoint (peer-message--endpoint-id (peer-message--actor-endpoint receiver)))
            (first (peer-message-send service sender :id "stable" :receiver endpoint :text "Payload")))
       (test-assert (equal first (peer-message-send service sender :id "stable" :receiver endpoint :text "Payload"))
                    "identical retries retain one stable message")
       (test-assert (handler-case (progn (peer-message-send service sender :id "stable" :receiver endpoint :text "Different") nil)
                      (cl-jobpond:durable-state-error () t)) "changed payload cannot reuse a stable identity")
       (let* ((delivery (peer-message-receive service receiver))
              (restored (peer-message-service-create owner (peer-message-orchestrator service) :path path)))
         (test-assert (eq ':delivered (getf delivery :state)) "receiving reserves a durable delivery token")
         (test-assert (eq ':unknown (getf (cl-jobpond:mailbox-find (peer-message-mailbox restored) "stable") :state))
                      "restart conservatively marks delivered messages unknown")
         (test-assert (null (peer-message-receive restored receiver)) "unknown delivery is not implicitly replayed")
         (peer-message-service-register restored)
         (test-assert (eq ':unknown (getf (getf (peer-message-inspect restored receiver :id "stable") :message) :state))
                      "scoped inspection exposes uncertain delivery and its recovery token")
         (test-assert (tool-result-success-p
                       (tool-registry-execute-call registry (agent-test-call :namespace "peer" :name "inspect"
                                                                            :arguments "{\"id\":\"stable\"}")
                                                   (peer-message-tests--context receiver registry)))
                      "the normal tool boundary permits actual receiver recovery inspection")
         (test-assert (handler-case (progn (peer-message-ack restored sender :id "stable" :token (getf delivery :token)) nil)
                        (peer-message-error () t)) "sender cannot impersonate receiver acknowledgment")
         (peer-message-resolve restored owner :id "stable" :token (getf delivery :token) :action ':retry)
         (let ((retry (peer-message-receive restored receiver)))
           (test-assert (/= (getf retry :token) (getf delivery :token)) "explicit retry invalidates the old delivery token")
           (test-assert (eq ':acknowledged (getf (peer-message-ack restored receiver :id "stable" :token (getf retry :token)) :state))
                        "only actual receiver acknowledges the new token")
           (test-assert (eq ':acknowledged (getf (peer-message-wait restored sender :id "stable" :timeout 0) :state))
                        "sender can wait for semantic acknowledgment")))))))

(-> test-peer-message-context-boundary () null)
(defun test-peer-message-context-boundary ()
  "Deliver explicitly granted context at the existing steering persistence boundary."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration path registry))
     (peer-message-tests--grant service owner :sender sender :receiver receiver :context-p t)
     (peer-message-send service sender :id "context" :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver)) :text "Shared result")
     (task-tests--publish-terminal (task-child-agent-job sender) ':completed
                                   (task-tests--terminal-result (task-child-agent-job sender)))
     (test-assert (handler-case
                     (progn (peer-message-send service sender :id "after-finish"
                                               :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver))
                                               :text "Denied") nil)
                   (peer-message-error () t)) "a finished sender cannot admit new messages")
     (let* ((entries (peer-message-take-context receiver))
            (identifier (agent-steering-input-identifier (first entries))))
       (test-assert (= 1 (length entries)) "safe boundary provides one bounded peer steering input")
       (test-assert (null (peer-message-take-context receiver)) "in-flight context is not implicitly redelivered")
       (test-assert (eq ':delivered (getf (cl-jobpond:mailbox-find (peer-message-mailbox service) "context") :state))
                    "claim is not an acknowledgment before persistence")
       (test-assert (not (peer-message-ack-context sender identifier)) "another actor cannot acknowledge receiver context")
       (test-assert (peer-message-ack-context receiver identifier) "persistence callback acknowledges the exact claimed message")
       (test-assert (eq ':acknowledged (getf (cl-jobpond:mailbox-find (peer-message-mailbox service) "context") :state))
                    "context acknowledgment is durable")
       (test-assert (not (peer-message-ack-context receiver identifier)) "a duplicate persistence callback has no second effect")))))

(-> peer-message-tests--identity-boundaries () null)
(defun peer-message-tests--identity-boundaries ()
  "Exercise collision resistance, remote field bounds, and post-transport authority."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration receiver path registry))
     (let* ((primary (list :session "remote" :job nil :execution nil))
            (child (list :session "remote" :job "job-one" :execution "primary"))
            (other (list :session "remote" :job "job-two" :execution "primary"))
            (target (peer-message--actor-endpoint owner)))
       (peer-message-grant service owner :id "remote-primary" :sender primary :receiver target)
       (test-assert (handler-case
                       (progn (peer-message-dispatch service
                                                    (list :action ':send :id "alias" :sender (peer-message--endpoint-id child)
                                                          :receiver (peer-message--endpoint-id target) :payload "Denied")) nil)
                     (peer-message-error () t)) "remote child named primary cannot inherit remote primary authority")
       (peer-message-grant service owner :id "remote-child" :sender child :receiver target)
       (test-assert (handler-case
                       (progn (peer-message-dispatch service
                                                    (list :action ':send :id "job-alias" :sender (peer-message--endpoint-id other)
                                                          :receiver (peer-message--endpoint-id target) :payload "Denied")) nil)
                     (peer-message-error () t)) "different jobs cannot alias an execution identifier")
       (dolist (field '(:session :job :execution))
         (let ((bad (copy-list child)))
           (setf (getf bad field) (make-string 257 :initial-element #\x))
           (test-assert (handler-case
                           (progn (peer-message-grant service owner :id "oversized" :sender bad :receiver target) nil)
                         (peer-message-error () t)) "all endpoint fields are bounded before durable admission")))
       (dolist (bad (list (list :session "remote" :job nil :execution "primary")
                         (list :session "remote" :job "job" :execution nil)
                         (list :session "remote" :job "" :execution "primary")))
         (test-assert (handler-case
                         (progn (peer-message-grant service owner :id "malformed" :sender bad :receiver target) nil)
                       (peer-message-error () t)) "asymmetric and empty child identities are refused"))
       (dolist (mode '(:revoke :replace))
         (let* ((id (string-downcase (symbol-name mode)))
                (grant-id (format nil "transport-~A" id)))
           (peer-message-grant service owner :id grant-id :sender (peer-message--actor-endpoint sender) :receiver primary)
           (setf (peer-message-remote-call service)
                 (lambda (session envelope &key cancelled-p)
                   (declare (ignore session envelope cancelled-p))
                   (ecase mode
                     (:revoke (peer-message-revoke service owner grant-id))
                     (:replace (setf (slot-value (task-child-agent-job sender) 'execution-identifier) (make-identifier))))
                   (list :state ':acknowledged)))
           (test-assert (handler-case
                           (progn (peer-message-send service sender :id id :receiver (peer-message--endpoint-id primary) :text "Remote work") nil)
                         (peer-message-error () t)) "transport completion revalidates revoked and replaced authority")
           (test-assert (eq ':unknown (getf (cl-jobpond:mailbox-find (peer-message-mailbox service) id) :state))
                        "lost authority preserves explicit uncertain delivery instead of semantic acknowledgment"))))))
  nil)

(-> peer-message-tests--snapshot-bounds () null)
(defun peer-message-tests--snapshot-bounds ()
  "Exercise matching publication/recovery limits with escaped multibyte payloads."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration registry))
     (peer-message-tests--grant service owner :sender sender :receiver receiver)
     (let* ((*peer-message-snapshot-maximum-octets* 4096)
            (endpoint (peer-message--endpoint-id (peer-message--actor-endpoint receiver)))
            (payload (format nil "~A ~A ~A" (make-string 100 :initial-element (code-char #x3bb))
                             (make-string 100 :initial-element #\") (make-string 100 :initial-element #\\))))
       (peer-message-send service sender :id "bounded" :receiver endpoint :text payload)
       (test-assert (handler-case
                       (progn (peer-message-send service sender :id "too-large" :receiver endpoint
                                                 :text (make-string 4096 :initial-element (code-char #x3bb))) nil)
                     (peer-message-error () t)) "UTF-8 publication refuses snapshots beyond the recovery bound")
       (test-assert (null (find "too-large"
                                (getf (cl-jobpond:mailbox-snapshot (peer-message-mailbox service)) :messages)
                                :key (lambda (message) (getf message :id)) :test #'equal))
                    "failed persistence does not expose the oversized message")
       (let ((restored (peer-message-service-create owner (peer-message-orchestrator service) :path path)))
         (test-assert (equal payload (getf (cl-jobpond:mailbox-find (peer-message-mailbox restored) "bounded") :payload))
                      "every accepted escaped multibyte snapshot is recoverable under the same bound")))))
  nil)

(-> test-peer-message-identity-and-finish () null)
(defun test-peer-message-identity-and-finish ()
  "Bind grants to exact job executions and terminate bounded waits on session finish."
  (peer-message-tests--identity-boundaries)
  (peer-message-tests--snapshot-bounds)
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration path registry))
     (peer-message-tests--grant service owner :sender sender :receiver receiver)
     (let ((endpoint (peer-message--endpoint-id (peer-message--actor-endpoint receiver))))
       (setf (slot-value (task-child-agent-job receiver) 'execution-identifier) (make-identifier))
       (test-assert (null (peer-message-discover service sender)) "replacement execution does not inherit a peer grant")
       (test-assert (handler-case (progn (peer-message-send service sender :id "stale" :receiver endpoint :text "Denied") nil)
                      (peer-message-error () t)) "stale receiver execution is refused"))
       (let ((replacement (task-tests--child-viewer (agent-configuration owner)
                                                   (task-child-agent-job receiver))))
         (peer-message-grant service owner :id "current" :sender (peer-message--actor-endpoint sender)
                             :receiver (peer-message--actor-endpoint replacement))
         (peer-message-send service sender :id "replacement-only"
                            :receiver (peer-message--endpoint-id (peer-message--actor-endpoint replacement))
                            :text "Only the new child may receive")
         (test-assert (handler-case (progn (peer-message-discover service receiver) nil)
                        (peer-message-error () t)) "retained actor cannot discover the replacement's grant")
         (test-assert (handler-case (progn (peer-message-receive service receiver :id "replacement-only") nil)
                        (peer-message-error () t)) "retained actor cannot consume the replacement's message")
         (test-assert (equal "replacement-only" (getf (peer-message-receive service replacement) :id))
                      "a newly constructed actor can use its own exact execution grant")
         (peer-message-service-close service)
         (test-assert (eq ':closed (getf (peer-message-wait service replacement :timeout 0) :state))
                      "finish closes waits without dropping retained IDs")
         (test-assert (handler-case
                         (progn (peer-message-send service sender :id "closed"
                                                   :receiver (peer-message--endpoint-id (peer-message--actor-endpoint replacement))
                                                   :text "Denied") nil)
                       (cl-jobpond:durable-state-error () t)) "session finish refuses new messages")))))

(-> test-peer-message-daemon-transport () null)
(defun test-peer-message-daemon-transport ()
  "Exercise the real authenticated transport between distinct logical sessions and paired grants."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore sender receiver service path registry))
     (let* ((remote-conversation (conversation-create configuration))
            (remote-owner (agent-create :configuration configuration :conversation remote-conversation
                                       :provider (make-instance 'model-provider) :tool-registry (make-instance 'tool-registry) :worker nil))
            (orchestrator (task-tests--orchestrator))
            (source (peer-message-service-create
                     owner orchestrator :remote-call (lambda (session envelope &key cancelled-p)
                                                       (peer-message-daemon-call configuration session envelope :cancelled-p cancelled-p))))
            (target (peer-message-service-create remote-owner orchestrator))
            (sender (peer-message--actor-endpoint owner))
            (receiver (peer-message--actor-endpoint remote-owner))
            (endpoint (peer-message--endpoint-id receiver))
            (runtime (image-daemon:daemon-message-endpoint-create
                      :directory (localgroup-registry-directory configuration)
                      :identifier (peer-message-session target)
                      :dispatcher (lambda (envelope &key cancelled-p)
                                    (peer-message-dispatch target envelope :cancelled-p cancelled-p))
                      :character-limit 32768 :request-timeout 2)))
       (unwind-protect
            (progn
              (peer-message-grant source owner :id "outgoing" :sender sender :receiver receiver)
              (image-daemon:daemon-runtime-start runtime)
              (test-assert (handler-case (progn (peer-message-send source owner :id "unpaired" :receiver endpoint :text "Denied") nil)
                             (image-daemon:daemon-message-error () t)) "remote receiver also requires an explicit paired grant")
              (test-assert (eq ':unknown (getf (cl-jobpond:mailbox-find (peer-message-mailbox source) "unpaired") :state))
                           "remote rejection or lost reply never silently discards outgoing uncertainty")
              (peer-message-grant target remote-owner :id "incoming" :sender sender :receiver receiver)
              (let ((sent (peer-message-send source owner :id "cross-session" :receiver endpoint :text "Delivered via daemon")))
                (test-assert (eq ':delivered (getf sent :state)) "transport receipt retains an unacknowledged durable outbox claim")
                (test-assert (eq ':queued (getf (cl-jobpond:mailbox-find (peer-message-mailbox target) "cross-session") :state))
                             "target product authority admits the remote message")
                (test-assert (equal sent (peer-message-send source owner :id "cross-session" :receiver endpoint :text "Delivered via daemon"))
                             "same-ID sender retry does not redeliver uncertain or unacknowledged effects")
                (let ((delivery (peer-message-receive target remote-owner)))
                  (peer-message-ack target remote-owner :id "cross-session" :token (getf delivery :token)))
                (test-assert (eq ':acknowledged (getf (peer-message-wait source owner :id "cross-session" :timeout 0) :state))
                             "remote status proves semantic receiver acknowledgment")
                (test-assert (eq ':acknowledged (getf (cl-jobpond:mailbox-find (peer-message-mailbox source) "cross-session") :state))
                             "semantic acknowledgment is durable in the source outbox")))
         (image-daemon:daemon-runtime-stop runtime))))))


(-> test-peer-message-child-provider-boundary () null)
(defun test-peer-message-child-provider-boundary ()
  "Exercise the actual child observer callbacks and provider request, not a copied callback fixture."
  (peer-message-tests--fixture
   (lambda (&key configuration owner sender receiver service path registry)
     (declare (ignore configuration path registry))
     (let* ((provider (make-instance 'task-test-provider :mode ':concurrent))
            (job (task-child-agent-job receiver)))
       (setf (slot-value owner 'provider) provider)
       (with-lock-held ((cl-jobpond::job--lock job)) (setf (job-state job) ':running))
       (task-job--set-progress-state job ':running)
       (peer-message-tests--grant service owner :sender sender :receiver receiver :context-p t)
       (peer-message-send service sender :id "provider-context"
                          :receiver (peer-message--endpoint-id (peer-message--actor-endpoint receiver))
                          :text "The peer boundary marker is ready.")
       (let ((result (task-run-child job)))
         (test-assert (eq ':success (getf result :status)) "the ordinary child runtime completes through terminal yield")
         (test-assert (test-object-contains-string-p (task-test-provider-request-inputs provider)
                                                    "The peer boundary marker is ready.")
                      "the actual provider request includes granted peer context")
         (test-assert (eq ':acknowledged (getf (cl-jobpond:mailbox-find (peer-message-mailbox service) "provider-context") :state))
                      "the actual child persistence callback acknowledges the peer message"))))))
