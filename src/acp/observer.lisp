(in-package #:autolith)

;;;; -- ACP Presentation of Channel Events --

;;; The channel's events reach the session on its connection's reader thread.
;;; Each becomes the ACP update the editor expects, in the order it arrived;
;;; an approval becomes the editor's permission request on a thread of its
;;; own, so events keep flowing while the editor decides.

(defparameter *acp-permission-timeout-seconds* 120
  "Maximum wait for an editor permission response.")

(defparameter *acp-turn-work-kinds*
  '(:message :command :lisp :native-lisp :operation-request :job-completion :recovery-diagnosis
    :client-work)
  "The kinds of channel work an editor sees as turns.")


;;;; -- Tool Identity --

(-> acp-tool-identifier (integer string) string)
(defun acp-tool-identifier (turn-sequence identifier)
  "Qualify IDENTIFIER by its durable turn, including during session replay."
  (format nil "~D:~A" turn-sequence identifier))

(-> acp-tool-kind (string) keyword)
(defun acp-tool-kind (name)
  "Map a native tool's operation to an ACP presentation category."
  (cond
    ((string= name "resource.read")
     ':read)
    ((string= name "resource.edit")
     ':edit)
    ((string= name "shell.run")
     ':execute)
    ((uiop:string-prefix-p "search." name)
     ':search)
    ((string= name "web.run")
     ':search)
    ((string= name "web_extra.gist")
     ':fetch)
    ((string= name "plan.update")
     ':think)
    (t
     ':other)))

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
  (let* ((key     (when (json-object-p arguments)
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


;;;; -- Sending --

(-> acp-session--send (acp-session hash-table) null)
(defun acp-session--send (session update)
  "Send one session UPDATE to the editor, coalescing thoughts."
  (unless (acp-service-closed-p (acp-session-service session))
    (agentcomms:update-buffer-send (acp-session-update-buffer session) update)
    (setf (acp-session-thought-state session)
          (when (eq ':agent-thought-chunk (agentcomms:acp-update-kind update)) ':streaming)))
  nil)

(-> acp-session-flush (acp-session) null)
(defun acp-session-flush (session)
  "Deliver pending thoughts before a permission request or a prompt's answer."
  (unless (acp-service-closed-p (acp-session-service session))
    (with-lock-held ((acp-session-lock session))
      (agentcomms:update-buffer-flush (acp-session-update-buffer session))))
  nil)

(-> acp-extension-notify (acp-session string &optional t) null)
(defun acp-extension-notify (session kind &optional data)
  "Send an ordered extension event, ignoring a connection that is gone."
  (with-lock-held ((acp-session-extension-lock session))
    (unless (acp-service-closed-p (acp-session-service session))
      (handler-case
          (agentcomms:agent-client-notify
           (acp-session-service session) "_autolith/event"
           (agentcomms:json-object
            "sessionId" (acp-session-identifier session)
            "sequence" (incf (acp-session-extension-sequence session))
            "kind" kind "data" (or data (agentcomms:json-object))))
        (agentcomms:acp-error ()
          nil))))
  nil)

(-> acp-session--plan (acp-session) null)
(defun acp-session--plan (session)
  "Send the workspace plan as it stands."
  (let ((plan (plan-load (acp-session-configuration session))))
    (acp-session--send
     session
     (agentcomms:acp-update-plan
      (when plan
        (mapcar (lambda (step)
                  (agentcomms:acp-plan-entry
                   (plan-step-text step)
                   :status (ecase (plan-step-status step)
                             (:pending
                              ':pending)
                             (:doing
                              ':in-progress)
                             (:done
                              ':completed))))
                (workspace-plan-steps plan))))))
  nil)


;;;; -- Events --

(-> acp-session-event (acp-session list) null)
(defun acp-session-event (session event)
  "Present one channel EVENT to the editor."
  (let ((fields (rest event)))
    (case (first event)
      (:text
       (acp-session--text session (getf fields :delta)))
      (:reasoning
       (acp-session--reasoning session (getf fields :delta)))
      (:status
       (acp-session--status session (getf fields :status) (getf fields :details)))
      (:present
       (let ((text (getf fields :text)))
         (when (non-empty-string-p text)
           (with-lock-held ((acp-session-lock session))
             (acp-session--send session (agentcomms:acp-update-agent-message
                                         (agentcomms:acp-text-content text)))))))
      (:record
       (acp-session--record session (getf fields :record)))
      (:approval
       (make-thread (lambda () (acp-session--approve session fields))
                    :name "Autolith ACP approval"))
      (:work-started
       (acp-session--work-started session (getf fields :kind)))
      (:work-finished
       (acp-session--work-finished session fields))
      (:state
       (acp-session--state session (getf fields :state)))
      (:closed
       (with-lock-held ((acp-session-lock session))
         (setf (acp-session-closed-p session) t)
         (sb-thread:condition-broadcast (acp-session-condition-variable session))))))
  nil)

(-> acp-session--text (acp-session t) null)
(defun acp-session--text (session text)
  "Stream assistant TEXT, keeping what was sent to complete a nonstreamed response.

Empty text sends nothing: an empty message chunk would split the thought
fragments that providers interleave with empty content deltas."
  (when (and (stringp text) (plusp (length text)))
    (with-lock-held ((acp-session-lock session))
      (text-buffer-append (acp-session-streamed-text session) text)
      (acp-session--send session (agentcomms:acp-update-agent-message (agentcomms:acp-text-content text)))))
  nil)

(-> acp-session--reasoning (acp-session t) null)
(defun acp-session--reasoning (session text)
  "Stream visible reasoning, separating summaries of distinct provider responses."
  (when (and (stringp text) (plusp (length text)))
    (with-lock-held ((acp-session-lock session))
      (acp-session--send
       session
       (agentcomms:acp-update-agent-thought
        (agentcomms:acp-text-content
         (if (eq ':boundary (acp-session-thought-state session))
             (format nil "~2%~A" text)
             text))))))
  nil)

(-> acp-session--status (acp-session t list) null)
(defun acp-session--status (session status details)
  "Translate the channel's provider and tool lifecycle STATUS into ACP updates."
  (with-lock-held ((acp-session-lock session))
    (case status
      (:user-message-persisted
       (let ((sequence (getf details :sequence)))
         (when (integerp sequence)
           (setf (acp-session-turn-sequence session) sequence))))
      (:provider-request-started
       (text-buffer-clear (acp-session-streamed-text session))
       (when (acp-session-thought-state session)
         (setf (acp-session-thought-state session) ':boundary)))
      (:assistant-response-persisted
       (let ((text     (getf details :text))
             (streamed (acp-session-streamed-text session)))
         (when (and (stringp text) (uiop:string-prefix-p streamed text)
                    (< (length streamed) (length text)))
           (acp-session--send session (agentcomms:acp-update-agent-message
                                       (agentcomms:acp-text-content (subseq text (length streamed))))))))
      (:tool-call-started
       (let ((identifier (getf details :call-id))
             (name       (getf details :tool))
             (arguments  (handler-case (json-decode (or (getf details :arguments) "{}"))
                           (error ()
                             (json-object)))))
         (setf (acp-session-tool-call session) identifier)
         (acp-session--send
          session
          (agentcomms:acp-update-tool-call
           (agentcomms:acp-tool-call
            (acp-tool-identifier (acp-session-turn-sequence session) identifier)
            (acp-tool-title name arguments)
            :name name :kind (acp-tool-kind name) :status ':in-progress :raw-input arguments)))))
      (:tool-call-completed
       (let ((output (getf details :output)))
         (acp-session--send
          session
          (agentcomms:acp-update-tool-call-progress
           (agentcomms:acp-tool-call-update
            (acp-tool-identifier (acp-session-turn-sequence session) (getf details :call-id))
            :status     (if (getf details :success-p) ':completed ':failed)
            :raw-output output
            :content    (list (agentcomms:acp-tool-call-content
                               (agentcomms:acp-text-content (or output "")))))))
         (when (equal (getf details :tool) "plan.update")
           (acp-session--plan session))))))
  (case status
    (:provider-request-completed
     (acp-extension-notify session "state"))
    (:compaction-completed
     (acp-extension-notify session "compaction"))
    (:tool-call-completed
     (acp-extension-notify session "jobs")))
  nil)

(-> acp-session--record (acp-session t) null)
(defun acp-session--record (session record)
  "Show a user message someone else gave the channel, such as queued or typed input.

The editor already shows its own prompts, so their records are skipped once."
  (when (and (consp record)
             (eq (first record) ':message)
             (eq (getf (rest record) :role) ':user)
             (not (getf (rest record) :automatic-p)))
    (let ((content (getf (rest record) :content)))
      (with-lock-held ((acp-session-lock session))
        (if (member content (acp-session-own-prompts session) :test #'equal)
            (setf (acp-session-own-prompts session)
                  (remove content (acp-session-own-prompts session) :test #'equal :count 1))
            (when (stringp content)
              (acp-session--send session (agentcomms:acp-update-user-message
                                          (agentcomms:acp-text-content content))))))))
  nil)

(-> acp-session--work-started (acp-session keyword) null)
(defun acp-session--work-started (session kind)
  "Open a turn for the editor when the channel starts work of KIND it sees as one."
  (when (member kind *acp-turn-work-kinds*)
    (let ((turn nil))
      (with-lock-held ((acp-session-lock session))
        (setf turn (cons (incf (acp-session-epoch session))
                         (case kind
                           (:message
                            (or (pop (acp-session-sources session)) "prompt"))
                           (:job-completion
                            "completion")
                           (:client-work
                            "invoke")
                           (t
                            "prompt")))
              (acp-session-turn session) turn))
      (acp-extension-notify session "turn"
                            (agentcomms:json-object "state" "running" "epoch" (first turn)
                                                    "source" (rest turn) "outcome" ':null))
      (acp-extension-notify session "state")))
  nil)

(-> acp-session--work-finished (acp-session list) null)
(defun acp-session--work-finished (session fields)
  "Close the open turn, and wake a prompt waiting for the channel to go idle."
  (let ((outcome (case (getf fields :outcome)
                   (:cancelled
                    ':cancelled)
                   (:failed
                    ':condition)
                   (t
                    ':end-turn)))
        (turn    nil))
    (with-lock-held ((acp-session-lock session))
      (when (member (getf fields :kind) *acp-turn-work-kinds*)
        (setf turn                               (acp-session-turn session)
              (acp-session-turn session)         nil
              (acp-session-last-outcome session) outcome))
      (when (getf fields :idle-p)
        (incf (acp-session-idle-count session)))
      (sb-thread:condition-broadcast (acp-session-condition-variable session)))
    (when turn
      (acp-session-flush session)
      (acp-extension-notify session "turn"
                            (agentcomms:json-object "state" "idle" "epoch" (first turn)
                                                    "source" (rest turn)
                                                    "outcome" (string-downcase (symbol-name outcome))))
      (acp-extension-notify session "state")))
  nil)

(-> acp-session--state (acp-session t) null)
(defun acp-session--state (session state)
  "Follow the channel's permission mode, telling the editor when it changed."
  (let ((mode (and (consp state) (getf (rest state) :permission-mode))))
    (when (and (keywordp mode)
               (with-lock-held ((acp-session-lock session))
                 (unless (eq mode (acp-session-mode session))
                   (setf (acp-session-mode session) mode)
                   t)))
      (handler-case
          (agentcomms:agent-send-update (acp-session-service session) (acp-session-identifier session)
                                        (agentcomms:acp-update-current-mode
                                         (string-downcase (symbol-name mode))))
        (agentcomms:acp-error ()
          nil))))
  (acp-extension-notify session "state")
  nil)


;;;; -- Approvals --

(-> acp-session--permission-option (list) hash-table)
(defun acp-session--permission-option (option)
  "Return the editor's permission option for the channel's approval OPTION."
  (let ((value       (getf option :value))
        (description (or (getf option :description) (getf option :value))))
    (cond
      ((equal value "once")
       (agentcomms:acp-permission-option value "Allow once" ':allow-once))
      ((equal value "deny")
       (agentcomms:acp-permission-option value "Reject" ':reject-once))
      (t
       (agentcomms:acp-permission-option value description ':allow-always)))))

(-> acp-session--approve (acp-session list) null)
(defun acp-session--approve (session approval)
  "Ask the editor to decide the channel's APPROVAL and answer it, denying when the editor does not choose."
  (let* ((options (getf approval :options))
         (values  (mapcar (lambda (option) (getf option :value)) options))
         (kind    (getf approval :kind))
         (title   (or (getf approval :title) "approval"))
         (report  (with-lock-held ((acp-session-lock session))
                    (agentcomms:acp-tool-call
                     (acp-tool-identifier (acp-session-turn-sequence session)
                                          (or (acp-session-tool-call session) (make-identifier)))
                     title
                     :name      title
                     :kind      (if (eq kind ':command) ':execute ':other)
                     :status    ':pending
                     :raw-input (agentcomms:json-object "request" (or (getf approval :entry) title)))))
         (choice  nil))
    (acp-session-flush session)
    (setf choice
          (handler-case
              (multiple-value-bind (outcome identifier)
                  (agentcomms:agent-request-permission
                   (acp-session-service session) (acp-session-identifier session) report
                   :options (mapcar #'acp-session--permission-option options)
                   :timeout *acp-permission-timeout-seconds*)
                (and (eq outcome ':selected)
                     (find identifier values :test #'equal)))
            (agentcomms:acp-error ()
              nil)))
    (handler-case (acp-session-call session ':answer
                                    :approval (getf approval :id)
                                    :value    (or choice (if (member "deny" values :test #'equal)
                                                             "deny"
                                                             (first (last values)))))
      (agentcomms:acp-state-error ()
        nil)))
  nil)
