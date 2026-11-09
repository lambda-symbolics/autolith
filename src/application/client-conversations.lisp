(in-package #:autolith)

;;;; -- ACP Conversation Extensions --

(defparameter *application-client-conversation-page-limit* 100
  "Maximum number of conversations returned by one request.")

(-> application-client--conversation-cwd ((option (or pathname string))) (option string))
(defun application-client--conversation-cwd (value)
  "Normalize a conversation directory VALUE for ACP comparison."
  (when value
    (namestring (uiop:ensure-directory-pathname value))))

(-> application-client--conversation-field (t) t)
(defun application-client--conversation-field (value)
  "Return VALUE as JSON null when it is unknown."
  (or value ':null))

(-> application-client--conversation-preview (conversation) (option string))
(defun application-client--conversation-preview (conversation)
  "Return the durable picker preview when available."
  (conversation-picker-preview conversation))

(-> application-client--conversation-live-p (application string) boolean)
(defun application-client--conversation-live-p (application identifier)
  "Probe the authoritative primary lease, including owners in other processes."
  (let ((lease (application-conversation-lease application)))
    (or (and lease (conversation-lease-matches-p lease identifier))
        (conversation-lease-busy-p (application-configuration application) identifier))))

(-> application-client--conversation-row (application pathname) hash-table)
(defun application-client--conversation-row (application pathname)
  "Load PATHNAME and project its durable metadata as an ACP row."
  (let ((conversation (conversation-load pathname)))
    (agentcomms:json-object
     "id" (conversation-identifier conversation)
     "displayId" (conversation-identifier conversation)
     "title" (application-client--conversation-field (conversation-title conversation))
     "directory" (application-client--conversation-field
                   (conversation-origin-directory conversation))
     "model" (application-client--conversation-field (conversation-model conversation))
     "createdAt" (application-client--conversation-field (conversation-created-at conversation))
     "lastActivityAt" (application-client--conversation-field
                        (conversation-last-activity-at conversation))
     "userTurnCount" (conversation-user-turn-count conversation)
     "preview" (application-client--conversation-field
                 (application-client--conversation-preview conversation))
     "live" (if (application-client--conversation-live-p
                 application (conversation-identifier conversation))
                t
                (argo:json-false)))))

(-> application-client-conversations (application hash-table) hash-table)
(defun application-client-conversations (application params)
  "Return durable conversations with bounded stable offset paging."
  (let* ((application   application)
         (configuration (application-configuration application))
         (cwd-value     (gethash "cwd" params))
         (cursor-value  (gethash "cursor" params))
         (limit-value   (gethash "limit" params))
         (cwd           (when (and cwd-value (not (eq cwd-value ':null)))
                          (unless (stringp cwd-value)
                            (agentcomms:acp-invalid-params "The field cwd must be a string."))
                          cwd-value))
         (cursor        (when (and cursor-value (not (eq cursor-value ':null)))
                          (unless (stringp cursor-value)
                            (agentcomms:acp-invalid-params "The field cursor must be a string."))
                          cursor-value))
         (limit         (if (or (null limit-value) (eq limit-value ':null)) 50 limit-value))
         (offset        (if (or (null cursor) (string= cursor ""))
                            0
                            (handler-case (parse-integer cursor :junk-allowed nil)
                              (error ()
                                (agentcomms:acp-invalid-params "cursor must be an integer"))))))
    (unless (<= 0 offset)
      (agentcomms:acp-invalid-params "cursor must be non-negative"))
    (unless (and (integerp limit)
                 (<= 1 limit *application-client-conversation-page-limit*))
      (agentcomms:acp-invalid-params "limit must be between 1 and 100"))
    (let* ((requested-cwd (and cwd (application-client--conversation-cwd cwd)))
           (paths         (remove-if-not
                           (lambda (pathname)
                             (and (not (conversation-inbox-pathname-p pathname))
                                  (or (null requested-cwd)
                                      (let* ((conversation (conversation-load pathname))
                                             (origin       (conversation-origin-directory
                                                            conversation)))
                                        (and origin
                                             (string= requested-cwd
                                                      (application-client--conversation-cwd origin)))))))
                           (conversation-list configuration)))
           (page          (subseq paths (min offset (length paths))
                                  (min (+ offset limit) (length paths))))
           (next          (+ offset (length page))))
      (agentcomms:json-object
       "items" (coerce (mapcar (lambda (pathname)
                                 (application-client--conversation-row application pathname))
                               page)
                       'vector)
       "nextCursor" (if (< next (length paths)) (princ-to-string next) ':null)))))
