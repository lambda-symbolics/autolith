(in-package #:autolith)

;;;; -- ACP Conversation Extension Tests --

(defun acp-conversations-test--make (configuration identifier text)
  "Create and persist one conversation with a user message."
  (let ((conversation (conversation-create
                       configuration
                       :identifier identifier
                       :storage-root (configuration-conversation-root configuration))))
    (conversation-append-user-message conversation text)
    conversation))

(-> test-acp-extension-conversations-paging-and-fields () null)
(defun test-acp-extension-conversations-paging-and-fields ()
  "List created conversations in stable pages and honor a matching cwd filter."
  (with-test-configuration (configuration root)
      (acp-conversations-test--make configuration "conversation-one" "first user message")
      (acp-conversations-test--make configuration "conversation-two" "second user message")
      (acp-conversations-test--make configuration "conversation-three" "third user message")
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session (acp-service--session service identifier))
                (first (acp-extension-conversations
                        session (agentcomms:json-object "limit" 2)))
                (first-items (gethash "items" first))
                (cursor (gethash "nextCursor" first))
                (second (acp-extension-conversations
                         session (agentcomms:json-object "cursor" cursor "limit" 2)))
                (filtered (acp-extension-conversations
                           session
                           (agentcomms:json-object
                            "cwd" (namestring (config :working-directory configuration))
                            "limit" 10))))
           (test-assert (= 2 (length first-items)) "first conversation page is bounded")
           (test-assert (stringp cursor) "non-final conversation page has a cursor")
           (test-assert (plusp (length (gethash "items" second)))
                        "second conversation page contains the remainder")
           (test-assert (= 3 (length (gethash "items" filtered)))
                        "cwd selects conversations from the requested directory")
           (let ((row (aref first-items 0)))
             (test-assert (stringp (gethash "id" row)) "conversation row has an id")
             (test-assert (stringp (gethash "displayId" row)) "conversation row has a display id")
             (test-assert (stringp (gethash "preview" row)) "conversation row has a user preview")
             (test-assert (= 1 (gethash "userTurnCount" row)) "conversation row counts user turns")
             (test-assert (argo:json-false-p (gethash "live" row))
                          "unleased conversations are not live"))))))
    nil)

(-> test-acp-extension-conversations-invalid-bounds () null)
(defun test-acp-extension-conversations-invalid-bounds ()
  "Reject malformed cursors and limits outside the bounded page range."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier)))
         (dolist (params (list (agentcomms:json-object "limit" 0)
                               (agentcomms:json-object "limit" 101)
                               (agentcomms:json-object "cursor" "not-an-integer")))
             (let ((rejected nil))
               (handler-case
                   (acp-extension-conversations session params)
                 (agentcomms:acp-method-error () (setf rejected t))
                 (agentcomms:acp-error () (setf rejected t)))
               (test-assert rejected "invalid conversation paging parameters are rejected")))))))
  nil)
