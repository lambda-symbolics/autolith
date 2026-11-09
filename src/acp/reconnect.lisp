(in-package #:autolith)

(defmethod channel-subscription-forget ((session acp-session) &key wire-p)
  "Release ACP registration without removing a replacement session for the same channel."
  (let ((service    (acp-session-service session))
        (identifier (channel-subscription-identifier session)))
    (with-lock-held ((acp-service-lock service))
      (when (eq session (gethash identifier (acp-service-sessions service)))
        (remhash identifier (acp-service-sessions service))
        (when wire-p (agentcomms::agent--forget-session service identifier)))))
  nil)
