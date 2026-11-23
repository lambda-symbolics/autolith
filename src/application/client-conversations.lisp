(in-package #:autolith)

;;;; -- Native Conversation Queries --

(defparameter *application-client-conversation-page-limit* 100
  "Maximum number of conversations returned by one request.")

(-> application-client--conversation-cwd ((option (or pathname string))) (option string))
(defun application-client--conversation-cwd (value)
  "Normalize a conversation directory VALUE for comparison."
  (when value
    (namestring (uiop:ensure-directory-pathname value))))

(-> application-client--conversation-live-p (application string) boolean)
(defun application-client--conversation-live-p (application identifier)
  "Probe the authoritative primary lease, including owners in other processes."
  (let ((lease (application-conversation-lease application)))
    (or (and lease (conversation-lease-matches-p lease identifier))
        (conversation-lease-busy-p (application-configuration application) identifier))))

(-> application-client--conversation-row (application pathname) list)
(defun application-client--conversation-row (application pathname)
  "Load PATHNAME and project its durable metadata as native values."
  (let ((conversation (conversation-load-snapshot pathname)))
    (list :id               (conversation-identifier conversation)
          :display-id       (conversation-identifier conversation)
          :title            (conversation-title conversation)
          :directory        (conversation-origin-directory conversation)
          :model            (conversation-model conversation)
          :created-at       (conversation-created-at conversation)
          :last-activity-at (conversation-last-activity-at conversation)
          :user-turn-count  (conversation-user-turn-count conversation)
          :preview          (conversation-picker-preview conversation)
          :live-p           (application-client--conversation-live-p
                             application (conversation-identifier conversation)))))

(-> application-client-conversations
    (application &key (:cwd (option (or pathname string)))
                 (:cursor (option (or integer string))) (:limit (option integer))) list)
(defun application-client-conversations (application &key cwd cursor (limit 50))
  "Return durable conversations with bounded stable offset paging."
  (let* ((limit         (or limit 50))
         (configuration (application-configuration application))
         (requested-cwd (and cwd (application-client--conversation-cwd cwd)))
         (offset        (cond ((null cursor)
                               0)
                              ((integerp cursor)
                               cursor)
                              ((stringp cursor)
                               (handler-case (parse-integer cursor :junk-allowed nil)
                                 (error ()
                                   (error 'application-client-unavailable
                                          :reason "Cursor must be an integer."))))
                              (t
                               (error 'application-client-unavailable
                                      :reason "Cursor is invalid."))))
         (paths         (remove-if-not
                         (lambda (pathname)
                           (and (not (conversation-inbox-pathname-p pathname))
                                (or (null requested-cwd)
                                    (let* ((conversation (conversation-load-snapshot pathname))
                                           (origin       (conversation-origin-directory conversation)))
                                      (and origin
                                           (string= requested-cwd
                                                    (application-client--conversation-cwd origin)))))))
                         (conversation-list configuration))))
    (unless (and (<= 0 offset)
                 (<= 1 limit *application-client-conversation-page-limit*))
      (error 'application-client-unavailable :reason "Conversation page bounds are invalid."))
    (let* ((start (min offset (length paths)))
           (end   (min (+ start limit) (length paths))))
      (list :items       (mapcar (lambda (pathname)
                                   (application-client--conversation-row application pathname))
                                 (subseq paths start end))
            :next-cursor (and (< end (length paths)) end)))))
