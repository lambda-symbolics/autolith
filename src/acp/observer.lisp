(in-package #:autolith)

;;;; -- ACP Presentation --

(defvar *acp-current-tool-call-id* nil
  "The provider call identifier bound within one ACP tool execution.")

(defparameter *acp-permission-timeout-seconds* 120
  "Maximum wait for an editor permission response.")

(defclass acp-observer (agent-observer)
  ((session :initarg :session :reader acp-observer-session :type acp-session
            :documentation "The session receiving this turn's updates.")
   (turn-sequence :initarg :turn-sequence :accessor acp-observer-turn-sequence
                  :documentation "The durable user-message sequence qualifying tool identifiers.")
   (streamed-text :initform (text-buffer-create) :accessor acp-observer-streamed-text :type string
                  :documentation "Text already emitted for the current provider response.")
   (update-buffer :reader acp-observer-update-buffer :type agentcomms:acp-update-buffer
                  :documentation "The ordered buffer coalescing visible thought fragments.")
   (thought-state :initform nil :accessor acp-observer-thought-state
                  :type (member nil :streaming :boundary)
                  :documentation "Whether contiguous thoughts continue or cross a provider response boundary.")
   (lock :initform (make-lock "Autolith ACP updates") :reader acp-observer-lock
         :documentation "The lock serializing short presentation updates, never permission waits."))
  (:documentation "An incremental ACP presentation sink with editor authorization."))

(defmethod initialize-instance :after ((observer acp-observer) &key)
  "Bind each observer's update buffer to its session's wire envelope."
  (let ((session (acp-observer-session observer)))
    (setf (slot-value observer 'update-buffer)
          (agentcomms:make-agent-update-buffer
           (acp-session-service session) (acp-session-identifier session)))))

(-> acp-observer-create (acp-session) acp-observer)
(defun acp-observer-create (session)
  "Create a fresh presentation sink for SESSION's next user turn."
  (make-instance 'acp-observer :session session
                 :turn-sequence (conversation-next-sequence
                                 (application-conversation (acp-session-application session)))))

(-> acp-observer--send (acp-observer hash-table) null)
(defun acp-observer--send (observer update)
  "Send one update, including terminal tool outcomes during cancellation."
  (let ((session (acp-observer-session observer)))
    (unless (acp-service-closed-p (acp-session-service session))
      (agentcomms:update-buffer-send (acp-observer-update-buffer observer) update)
      (setf (acp-observer-thought-state observer)
            (when (eq ':agent-thought-chunk (agentcomms:acp-update-kind update)) ':streaming))))
  nil)

(-> acp-observer-flush (acp-observer) null)
(defun acp-observer-flush (observer)
  "Deliver pending thoughts before a permission request or terminal prompt response."
  (unless (acp-service-closed-p (acp-session-service (acp-observer-session observer)))
    (agentcomms:update-buffer-flush (acp-observer-update-buffer observer)))
  nil)

(-> acp-tool-identifier (integer string) string)
(defun acp-tool-identifier (turn-sequence identifier)
  "Qualify IDENTIFIER by its durable turn, including during session replay."
  (format nil "~D:~A" turn-sequence identifier))

(-> acp-tool-kind (string) keyword)
(defun acp-tool-kind (name)
  "Map a native tool's operation to an ACP presentation category."
  (cond
    ((string= name "resource.read") ':read)
    ((string= name "resource.edit") ':edit)
    ((string= name "shell.run") ':execute)
    ((uiop:string-prefix-p "search." name) ':search)
    ((string= name "web.run") ':search)
    ((string= name "web_extra.gist") ':fetch)
    ((string= name "plan.update") ':think)
    (t
     ':other)))

(-> acp-observer--arguments (acp-observer string) hash-table)
(defun acp-observer--arguments (observer identifier)
  "Return the persisted provider arguments of IDENTIFIER as a JSON object."
  (let* ((application (acp-session-application (acp-observer-session observer)))
         (call (find identifier (reverse (conversation-input-items
                                          (application-conversation application)))
                     :key (lambda (item) (json-get item "call_id")) :test #'equal)))
    (handler-case (json-decode (or (and call (json-get call "arguments")) "{}"))
      (error ()
        (json-object)))))

(-> acp-tool--argument-preview (t) string)
(defun acp-tool--argument-preview (value)
  "Render a bounded argument summary before sanitizing or measuring display cells."
  (let ((text (cond
                ((stringp value)
                 (subseq value 0 (min (length value) 192)))
                ((hash-table-p value)
                 (format nil "{~D fields}" (hash-table-count value)))
                ((and (vectorp value) (plusp (length value)) (every #'stringp value))
                 (let ((joined (format nil "~{~A~^ ~}"
                                       (map 'list
                                            (lambda (item)
                                              (subseq item 0 (min (length item) 192)))
                                            (subseq value 0 (min 4 (length value)))))))
                   (subseq joined 0 (min (length joined) 192))))
                ((vectorp value)
                 (format nil "[~D items]" (length value)))
                ((and (integerp value) (> (integer-length value) 64))
                 "<integer>")
                (t
                 (json-encode value)))))
    (text-cell-prefix (sanitize-text text :single-line-p t) 48)))

(-> acp-tool-title (string t) string)
(defun acp-tool-title (name arguments)
  "Return NAME with a bounded, single-line preview of its principal argument."
  (let* ((key (when (json-object-p arguments)
                (or (find-if (lambda (key) (nth-value 1 (gethash key arguments)))
                             '("forms" "form" "command" "uri" "query" "patterns" "path"
                               "url" "system" "name" "task" "text"))
                    (first (sort (loop for key being the hash-keys of arguments
                                      collect key)
                                 #'string<)))))
         (preview (when key
                    (acp-tool--argument-preview (gethash key arguments)))))
    (if (non-empty-string-p preview)
        (format nil "~A · ~A" name preview)
        name)))

(-> acp-observer--tool-report
    (acp-observer &key (:identifier string) (:title string) (:status keyword)
                  (:arguments t) (:output (option string))) hash-table)
(defun acp-observer--tool-report (observer &key identifier title status arguments output)
  "Construct a tool report using the same identity for progress and authorization."
  (agentcomms:acp-tool-call
   (acp-tool-identifier (acp-observer-turn-sequence observer) identifier)
   (acp-tool-title title arguments) :name title :kind (acp-tool-kind title) :status status
   :raw-input arguments :raw-output output
   :content (when output
              (list (agentcomms:acp-tool-call-content (agentcomms:acp-text-content output))))))

(defmethod agent-observer-text ((observer acp-observer) text)
  "Stream assistant text and retain its length for nonstreamed response reconciliation."
  (acp-session-check-cancelled (acp-observer-session observer))
  (with-lock-held ((acp-observer-lock observer))
    (text-buffer-append (acp-observer-streamed-text observer) text)
    (acp-observer--send observer
                        (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text))))
  nil)

(defmethod agent-observer-reasoning ((observer acp-observer) text)
  "Stream visible reasoning, separating contiguous summaries from distinct responses."
  (acp-session-check-cancelled (acp-observer-session observer))
  (when (plusp (length text))
    (with-lock-held ((acp-observer-lock observer))
      (acp-observer--send
       observer
       (agentcomms:acp-update-agent-thought
        (agentcomms:acp-text-content
         (if (eq ':boundary (acp-observer-thought-state observer))
             (format nil "~2%~A" text)
             text))))))
  nil)

(-> acp-observer--plan (acp-observer) null)
(defun acp-observer--plan (observer)
  "Project the durable workspace plan after its native publication."
  (let* ((session (acp-observer-session observer))
         (plan (plan-load (application-configuration (acp-session-application session)))))
    (acp-observer--send
     observer
     (agentcomms:acp-update-plan
      (when plan
        (mapcar (lambda (step)
                  (agentcomms:acp-plan-entry
                   (plan-step-text step)
                   :status (ecase (plan-step-status step)
                             (:pending ':pending)
                             (:doing ':in-progress)
                             (:done ':completed))))
                (workspace-plan-steps plan))))))
  nil)

(defmethod agent-observer-status ((observer acp-observer) status details)
  "Translate provider and durable tool lifecycle events to ACP updates."
  ;; Completion events must be sent before the cancelled prompt response.
  (unless (eq status ':tool-call-completed)
    (acp-session-check-cancelled (acp-observer-session observer)))
  (with-lock-held ((acp-observer-lock observer))
    (case status
      (:user-message-persisted
       (setf (acp-observer-turn-sequence observer) (getf details :sequence)))
      (:provider-request-started
       (text-buffer-clear (acp-observer-streamed-text observer))
       (when (acp-observer-thought-state observer)
         (setf (acp-observer-thought-state observer) ':boundary)))
      (:assistant-response-persisted
       (let ((text (getf details :text)) (streamed (acp-observer-streamed-text observer)))
         (when (and (stringp text) (uiop:string-prefix-p streamed text)
                    (< (length streamed) (length text)))
           (acp-observer--send
            observer (agentcomms:acp-update-agent-message
                      (agentcomms:acp-text-content (subseq text (length streamed))))))))
      (:tool-call-started
       (acp-observer--send
        observer
        (agentcomms:acp-update-tool-call
         (acp-observer--tool-report
          observer :identifier (getf details :call-id) :title (getf details :tool)
          :status ':in-progress
          :arguments (acp-observer--arguments observer (getf details :call-id))))))
      (:tool-call-completed
       (let ((identifier (acp-tool-identifier (acp-observer-turn-sequence observer)
                                              (getf details :call-id)))
             (output (getf details :output)))
         (acp-observer--send
          observer
          (agentcomms:acp-update-tool-call-progress
           (agentcomms:acp-tool-call-update
            identifier :status (if (getf details :success-p) ':completed ':failed)
            :raw-output output
            :content (list (agentcomms:acp-tool-call-content
                            (agentcomms:acp-text-content output))))))
         (when (string= (getf details :tool) "plan.update")
           (acp-observer--plan observer))))))
  nil)

(-> acp-observer--permission (acp-observer hash-table list) (option string))
(defun acp-observer--permission (observer report options)
  "Request a bounded editor decision and accept only an offered option."
  (let ((session (acp-observer-session observer)))
    (acp-session-check-cancelled session)
    (acp-observer-flush observer)
    (let ((choice
           (handler-case
               (multiple-value-bind (outcome identifier)
                   (agentcomms:agent-request-permission
                    (acp-session-service session) (acp-session-identifier session)
                    report options :timeout *acp-permission-timeout-seconds*)
                 (when (and (eq outcome ':selected)
                            (find identifier options :test #'equal
                                  :key (lambda (option) (agentcomms:json-get option "optionId"))))
                   identifier))
             (agentcomms:acp-error ()
               nil))))
      (acp-session-check-cancelled session)
      choice)))

(-> acp-observer--approval (acp-observer string hash-table t) boolean)
(defun acp-observer--approval (observer title arguments key)
  "Ask for exact operation approval, retaining allow-always only for this session."
  (let* ((session (acp-observer-session observer))
         (saved (with-lock-held ((acp-session-lock session))
                  (gethash key (acp-session-permissions session)))))
    (acp-session-check-cancelled session)
    (or saved
        (let ((choice
               (acp-observer--permission
                observer
                (acp-observer--tool-report
                 observer :identifier (or *acp-current-tool-call-id* (make-identifier))
                 :title title :status ':pending :arguments arguments)
                (list (agentcomms:acp-permission-option "once" "Allow once" ':allow-once)
                      (agentcomms:acp-permission-option "session" "Allow exact operation this session" ':allow-always)
                      (agentcomms:acp-permission-option "deny" "Reject" ':reject-once)))))
          (when (equal choice "session")
            (with-lock-held ((acp-session-lock session))
              (setf (gethash key (acp-session-permissions session)) t)))
          (and (member choice '("once" "session") :test #'equal) t)))))

(defmethod agent-observer-authorize-command ((observer acp-observer) command directory)
  "Apply native process policy with editor approval in ask mode."
  (let* ((session (acp-observer-session observer))
         (application (acp-session-application session)))
    (acp-session-check-cancelled session)
    (case (acp-session-mode session)
      (:full-access
       ':full-access)
      (:sandboxed
       (if (application--command-sandbox-available-p) ':sandboxed ':deny))
      (:auto
       (application--automatic-command-decision
        (nth-value 0
                   (permissions-model-classify-command
                    command directory :provider (application-provider application)
                    :configuration (application-configuration application)
                    :sandbox-available-p (application--command-sandbox-available-p)))))
      (otherwise
       (if (acp-observer--approval observer "shell.run"
                                   (json-object "command" command "directory" (namestring directory))
                                   (list ':command command (namestring directory)))
           ':full-access
           ':deny)))))

(defmethod agent-observer-authorize-tool ((observer acp-observer) tool arguments)
  "Authorize an external tool through the editor using its exact identity and input."
  (let* ((session (acp-observer-session observer))
         (name (tool-canonical-name tool)))
    (acp-session-check-cancelled session)
    (if (or (eq (acp-session-mode session) ':full-access)
            (acp-observer--approval observer name arguments
                                    (list ':tool (tool-authorization-identity-fields tool)
                                          (agent--tool-signature-value arguments))))
        ':allow
        ':deny)))

(defmethod agent-observer-call-with-tool-execution ((observer acp-observer) identifier function)
  "Own tool workers and propagate ACP session bindings and permission identity."
  (let ((*acp-current-tool-call-id* identifier))
    (acp-session--call-with-tool (acp-observer-session observer) function)))
