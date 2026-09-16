(in-package #:autolith)

;;;; -- ACP Permission Requests --

;; The editor is the user. When the agent would otherwise prompt for command
;; or external tool approval, this file turns one prompt into one
;; session/request_permission round trip to the connected client, maps the
;; outcome to an authorization decision, and fails closed on every failure
;; mode. Session and application state live in acp/session.lisp; the wiring
;; into the observer lives in acp/observer.lisp.

(-> acp--session-request-permission
    (acp-session json-object list)
    (option string))
(defun acp--session-request-permission (session tool-call-object options)
  "Send one session/request_permission for TOOL-CALL-OBJECT and OPTIONS.

Return the selected optionId string, or NIL when the client cancelled,
answered invalidly, or the connection failed. Every failure mode denies."
  (handler-case
      (block nil
        (let* ((connection (acp-server-connection (acp-session-server session)))
               (reply
                (acp-connection-request
                 connection
                 "session/request_permission"
                 (json-object "sessionId" (acp-session-identifier session)
                              "toolCall" tool-call-object
                              "options" (apply #'json-array options))))
               (outcome (and (json-object-p reply) (json-get reply "outcome"))))
          (unless (json-object-p outcome)
            (return nil))
          (unless (string= (json-get outcome "outcome") "selected")
            (return nil))
          (json-get outcome "optionId")))
    (acp-remote-error ()
      (acp--log "The client failed a permission request; denying.")
      nil)
    (acp-connection-closed-error ()
      (acp--log "The ACP connection closed during a permission request.")
      nil)))

;;;; -- External Tool Authorization --

(-> acp--tool-permission-options () list)
(defun acp--tool-permission-options ()
  "Return the ACP request options for one external tool call."
  (list (json-object "optionId" "allow_once"
                     "name" "Allow"
                     "kind" "allow_once"
                     "description" "Run this one tool call")
        (json-object "optionId" "reject_once"
                     "name" "Reject"
                     "kind" "reject_once"
                     "description" "Do not run this tool call")))

(-> acp--authorize-tool (acp-session tool json-object) keyword)
(defun acp--authorize-tool (session tool arguments)
  "Return the client-selected permission for one external TOOL call.

Every call asks the client; there is no external-tool rule persistence."
  (let ((tool-name (tool-canonical-name tool)))
    (if (string=
         (or (acp--session-request-permission
              session
              (json-object "title" tool-name
                           "kind" (acp--tool-kind tool-name)
                           "rawInput" arguments)
              (acp--tool-permission-options))
             "")
         "allow_once")
        ':allow
        ':deny)))

;;;; -- Shell Command Authorization --

(-> acp--command-permission-options () list)
(defun acp--command-permission-options ()
  "Return the ACP request options for one shell command."
  (list (json-object "optionId" "allow_once"
                     "name" "Allow once"
                     "kind" "allow_once"
                     "description" "Run this command once")
        (json-object "optionId" "allow_always"
                     "name" "Always allow"
                     "kind" "allow_always"
                     "description"
                     "Save this exact command and directory as approved")
        (json-object "optionId" "reject_once"
                     "name" "Reject"
                     "kind" "reject_once"
                     "description" "Do not run this command")))

(-> acp--ask-command-permission (acp-session string pathname) keyword)
(defun acp--ask-command-permission (session command directory)
  "Ask SESSION's client how COMMAND may run in DIRECTORY, failing closed."
  (let* ((application (acp-session-application session))
         (option-id
          (acp--session-request-permission
           session
           (json-object
            "title"
            (text-cell-prefix (sanitize-text command :single-line-p t) 76)
            "kind" "execute")
           (acp--command-permission-options))))
    (cond
      ((string= (or option-id "") "allow_once")
       ':full-access)
      ((string= (or option-id "") "allow_always")
       (permissions-allow
        :configuration (application-configuration application)
        :state         (application-permission-state application)
        :command       command
        :directory     directory)
       ':full-access)
      (t
       ':deny))))

(-> acp--authorize-command (acp-session string pathname) keyword)
(defun acp--authorize-command (session command directory)
  "Return the permission decision for COMMAND in DIRECTORY.

Mirrors the terminal session's mode semantics: saved rules approve first,
full access skips asking, the sandbox mode succeeds when it is available,
auto mode classifies with the provider model, and anything else asks the
editor through one request_permission round trip."
  (let ((application (acp-session-application session)))
    (case (application-permission-mode application)
      ((:full-access)
       ':full-access)
      (:sandboxed
       (if (application--command-sandbox-available-p)
           ':sandboxed
           (acp--ask-command-permission session command directory)))
      (:auto
       (if (permissions-allowed-p
            (application-permission-state application) command directory)
           ':full-access
           (application--auto-command-permission application command directory)))
      (t
       (if (permissions-allowed-p
            (application-permission-state application) command directory)
           ':full-access
           (acp--ask-command-permission session command directory))))))
