(in-package #:autolith)

;;;; -- ACP Observation --

;; The ACP observation layer streams one session's agent activity to the
;; client as session/update notifications: assistant and reasoning chunks,
;; tool calls, and their progress and completion. Presentation mapping lives
;; in this file; session lifetime and dispatch live in acp/session.lisp.

;;;; -- Tool Call Mapping --

(defparameter *acp-tool-output-limit* 4000
  "The maximum characters of tool output included in one tool update.")

(-> acp--tool-call-id (list) string)
(defun acp--tool-call-id (details)
  "Return the stable ACP toolCallId for one status event DETAILS.

The provider call id is the id of record; the tool round backs it up when a
call omits one, so all events of one call share an id no matter the shape."
  (let ((call-id (getf details :call-id)))
    (if (non-empty-string-p call-id)
        call-id
          (format nil "round_~D" (or (getf details :tool-round) 0)))))

(-> acp--tool-kind ((option string)) string)
(defun acp--tool-kind (tool-name)
  "Return the advisory ACP tool kind for canonical tool NAME like lisp.eval.

ACP kinds are advisory, so unknown namespaces simply report other."
  (let ((name (or tool-name "")))
    (cond
      ((or (uiop:string-prefix-p "lisp." name)
           (uiop:string-prefix-p "shell." name))
       "execute")
      ((uiop:string-prefix-p "rlm." name) "think")
      ((uiop:string-prefix-p "web." name) "fetch")
      ((uiop:string-prefix-p "search." name) "search")
      ((uiop:string-prefix-p "resource." name)
       (cond
         ((search "edit" name) "edit")
         ((search "delete" name) "delete")
         (t "read")))
      (t "other"))))

(-> acp--bounded-tool-output (string) string)
(defun acp--bounded-tool-output (text)
  "Return TEXT cut to *acp-tool-output-limit* characters.
  Longer output keeps its first characters."
  (if (<= (length text) *acp-tool-output-limit*)
      text
      (concatenate 'string
                   (subseq text 0 *acp-tool-output-limit*)
                   "…")))

(-> acp--tool-call-text ((option string)) (option json-object))
(defun acp--tool-call-text (text)
  "Return one bounded ACP text content object for TEXT, or NIL when empty.

Output longer than *acp-tool-output-limit* keeps its first characters."
  (when (and (stringp text) (non-empty-string-p text))
    (json-object
     "type" "content"
     "content"
     (json-object "type" "text" "text"
                  (acp--bounded-tool-output text)))))

  (-> acp--tool-call-preview (json-object) (option string))
(defun acp--tool-call-preview (arguments)
  "Return a one-line preview of ARGUMENTS for the tool-call title.

The sorted first entry renders: string values inline, anything else as
compact JSON."
  (block nil
    (let ((key (first (sort (loop for entry being the hash-keys of arguments
                                  collect entry)
                            #'string<))))
      (unless key
        (return nil))
      (let ((value (json-get arguments key)))
        (if (stringp value)
            (text-cell-prefix (sanitize-text value :single-line-p t) 48)
            (json-encode value))))))

(-> acp--tool-call-title (list) string)
(defun acp--tool-call-title (details)
  "Return the tool-call TITLE for DETAILS, previewing known arguments.

Calls whose rawInput is known render as the tool name, a middle dot, and
the first argument's preview, so the editor's tool window shows the form
or command instead of the bare tool name."
  (let ((tool (or (getf details :tool) "tool"))
        (preview (and (json-object-p (getf details :input))
                      (acp--tool-call-preview (getf details :input)))))
    (if preview
        (format nil "~A · ~A" tool preview)
        tool)))

(-> acp--tool-call-started (acp-session list) null)
(defun acp--tool-call-started (session details)
  "Report one starting tool call to SESSION's client as in progress.

The call's argument object travels as rawInput when DETAILS carries one,
and the title previews the first argument."
  (let ((input (getf details :input)))
    (acp--session-update
     session "tool_call"
     (if (json-object-p input)
         (json-object "toolCallId" (acp--tool-call-id details)
                      "title" (acp--tool-call-title details)
                      "kind" (acp--tool-kind (getf details :tool))
                      "status" "in_progress"
                      "rawInput" input)
         (json-object "toolCallId" (acp--tool-call-id details)
                      "title" (acp--tool-call-title details)
                      "kind" (acp--tool-kind (getf details :tool))
                      "status" "in_progress"))))
  nil)

(-> acp--tool-call-progress (acp-session list) null)
(defun acp--tool-call-progress (session details)
  "Report one tool-call activity update to SESSION's client."
  (let ((content (acp--tool-call-text (getf details :activity))))
    (when content
      (acp--session-update
       session "tool_call_update"
       (json-object "toolCallId" (acp--tool-call-id details)
                    "status" "in_progress"
                    "content" (json-array content)))))
  nil)

(-> acp--tool-call-completed (acp-session list) null)
(defun acp--tool-call-completed (session details)
  "Report one finished tool call with SESSION's final status and output.

Failed calls report failed; timing details stay in the conversation record.
Calls without recorded output report status only. The output travels in
content for display and in rawOutput for machine reading, bounded alike."
  (let* ((output (getf details :output))
         (content (acp--tool-call-text output))
         (raw-output
          (and (stringp output) (non-empty-string-p output)
               (json-object "output" (acp--bounded-tool-output output)))))
    (acp--session-update
     session "tool_call_update"
     (if raw-output
         (json-object "toolCallId" (acp--tool-call-id details)
                      "status" (if (getf details :success-p)
                                   "completed"
                                   "failed")
                      "content" (if content
                                    (json-array content)
                                    (json-array))
                      "rawOutput" raw-output)
         (json-object "toolCallId" (acp--tool-call-id details)
                      "status" (if (getf details :success-p)
                                   "completed"
                                   "failed")
                      "content" (if content
                                    (json-array content)
                                    (json-array))))))
  nil)

(-> acp--report-tool-status (acp-session keyword list) null)
(defun acp--report-tool-status (session status details)
  "Present one tool-call STATUS event from DETAILS to SESSION's client.

Tool progress and completion inside an ACP turn report as ACP tool calls;
every other status keyword stays invisible here."
  (case status
    (:tool-call-started
     (acp--tool-call-started session details))
    (:tool-call-progress
     (acp--tool-call-progress session details))
    (:tool-call-completed
     (acp--tool-call-completed session details)))
  nil)

;;;; -- Session Observation --

(-> acp--session-observation (acp-session) agent-observer)
(defun acp--session-observation (session)
  "Return one serialized observer streaming SESSION's turn to the client.

Every callback boundary first honors session/cancel; command and external
tool authorization asks the editor through acp/authorization.lisp."
  (make-instance
   'serialized-agent-observer
   :delegate (callback-agent-observer-create
              :text-callback
              (lambda (text)
                (acp--session-check-cancellation session)
                (acp--flush-reasoning session)
                (acp--session-chunk session "agent_message_chunk" text))
              :reasoning-callback
              (lambda (text)
                (acp--session-check-cancellation session)
                (acp--session-reasoning session text))
              :status-callback
              (lambda (status details)
                (acp--session-check-cancellation session)
                (acp--flush-reasoning session)
                (acp--report-tool-status session status details))
              :command-authorization-callback
              (lambda (command directory)
                (acp--session-check-cancellation session)
                (acp--flush-reasoning session)
                (acp--authorize-command session command directory))
              :tool-authorization-callback
              (lambda (tool arguments)
                (acp--session-check-cancellation session)
                (acp--flush-reasoning session)
                (acp--authorize-tool session tool arguments)))))
