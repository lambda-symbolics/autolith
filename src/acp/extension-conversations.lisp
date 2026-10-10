(in-package #:autolith)

;;;; -- ACP Conversation Extensions --

(defparameter *acp-extension-conversation-page-limit* 100
  "Maximum number of conversations returned by one request.")

(-> acp-extension--conversation-cwd ((option (or pathname string))) (option string))
(defun acp-extension--conversation-cwd (value)
  "Normalize a conversation directory VALUE for ACP comparison."
  (when value
    (namestring (uiop:ensure-directory-pathname value))))

(-> acp-extension--conversation-field (t) t)
(defun acp-extension--conversation-field (value)
  "Return VALUE as JSON null when it is unknown."
  (or value ':null))

(-> acp-extension--conversation-preview (conversation) (option string))
(defun acp-extension--conversation-preview (conversation)
  "Return the durable picker preview when available."
  (conversation-picker-preview conversation))

(-> acp-extension--conversation-live-p (application string) boolean)
(defun acp-extension--conversation-live-p (application identifier)
  "Probe the authoritative primary lease, including owners in other processes."
  (let ((lease (application-conversation-lease application)))
    (or (and lease (conversation-lease-matches-p lease identifier))
        (handler-case
            (let ((probe (conversation-lease-acquire (application-configuration application) identifier)))
              (conversation-lease-release probe)
              nil)
          (conversation-in-use () t)))))

(-> acp-extension--conversation-row (application pathname) hash-table)
(defun acp-extension--conversation-row (application pathname)
  "Load PATHNAME and project its durable metadata as an ACP row."
  (let ((conversation (conversation-load pathname)))
    (agentcomms:json-object
     "id" (conversation-identifier conversation)
     "displayId" (conversation-identifier conversation)
     "title" (acp-extension--conversation-field (conversation-title conversation))
     "directory" (acp-extension--conversation-field
                   (conversation-origin-directory conversation))
     "model" (acp-extension--conversation-field (conversation-model conversation))
     "createdAt" (acp-extension--conversation-field (conversation-created-at conversation))
     "lastActivityAt" (acp-extension--conversation-field
                        (conversation-last-activity-at conversation))
     "userTurnCount" (conversation-user-turn-count conversation)
     "preview" (acp-extension--conversation-field
                 (acp-extension--conversation-preview conversation))
     "live" (if (acp-extension--conversation-live-p
                 application (conversation-identifier conversation))
                 t
                 (argo:json-false)))))

(-> acp-extension-conversations (acp-session hash-table) hash-table)
(defun acp-extension-conversations (session params)
  "Return durable conversations with bounded stable offset paging."
  (let* ((application (acp-session-application session))
         (configuration (application-configuration application))
         (cwd-value (gethash "cwd" params))
         (cursor-value (gethash "cursor" params))
         (limit-value (gethash "limit" params))
         (cwd (when (and cwd-value (not (eq cwd-value ':null)))
                (unless (stringp cwd-value)
                  (agentcomms:acp-invalid-params "The field cwd must be a string."))
                cwd-value))
         (cursor (when (and cursor-value (not (eq cursor-value ':null)))
                   (unless (stringp cursor-value)
                     (agentcomms:acp-invalid-params "The field cursor must be a string."))
                   cursor-value))
         (limit (if (or (null limit-value) (eq limit-value ':null)) 50 limit-value))
         (offset (if (or (null cursor) (string= cursor ""))
                     0
                     (handler-case (parse-integer cursor :junk-allowed nil)
                       (error ()
                         (agentcomms:acp-invalid-params "cursor must be an integer"))))))
    (unless (<= 0 offset)
      (agentcomms:acp-invalid-params "cursor must be non-negative"))
    (unless (and (integerp limit)
                 (<= 1 limit *acp-extension-conversation-page-limit*))
      (agentcomms:acp-invalid-params "limit must be between 1 and 100"))
    (let* ((requested-cwd (and cwd (acp-extension--conversation-cwd cwd)))
           (paths (remove-if-not
                   (lambda (pathname)
                     (or (null requested-cwd)
                         (let* ((conversation (conversation-load pathname))
                                (origin (conversation-origin-directory conversation)))
                           (and origin
                                (string= requested-cwd
                                         (acp-extension--conversation-cwd origin))))))
                   (conversation-list configuration)))
           (page (subseq paths (min offset (length paths))
                         (min (+ offset limit) (length paths))))
           (next (+ offset (length page))))
      (agentcomms:json-object
       "items" (coerce (mapcar (lambda (pathname)
                                 (acp-extension--conversation-row application pathname))
                               page)
                        'vector)
       "nextCursor" (if (< next (length paths)) (princ-to-string next) ':null)))))
