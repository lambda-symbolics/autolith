(in-package #:autolith)

;;;; -- CLI Conversation Selection --

(-> main--open-match-description
    (string &key (:pathname (option pathname)) (:status list))
    string)
(defun main--open-match-description (identifier &key pathname status)
  "Return an actionable identifier and available context for one channel matching what autolith open was given."
  (let* ((metadata  (and pathname (conversation-picker-metadata-find pathname)))
         (header    (and pathname (conversation-peek-header pathname)))
         (title     (or (getf status :conversation-title)
                        (and metadata (conversation-picker-metadata-title metadata))))
         (directory (or (getf status :cwd) (getf (rest header) :directory)))
         (preview   (and metadata
                         (application--conversation-preview-from-metadata metadata)))
         (date      (and pathname
                         (format nil "~{~A~^ ~}"
                                 (multiple-value-list
                                  (application--calendar-parts
                                   (conversation-storage-write-date pathname)))))))
    (format nil "~A~@[  ~A~]~@[  ~A~]~@[  ~A~]~@[  ~A~]~@[  running, PID ~D~]"
            (conversation-identifier-display identifier)
            title directory date preview (getf status :pid))))

(-> main-resolve-conversation (configuration string) string)
(defun main-resolve-conversation (configuration fragment)
  "Resolve an agent name, exact ancestry path or ID fragment against durable, saved and live channels.
Exact channel identifiers take priority over bare participant names; @NAME names
an agent's residence. Retain saved and endpoint-only IDs for rescue sessions."
  (let ((world (world-load configuration)))
    (handler-case
        (when (or (uiop:string-prefix-p "@" fragment)
                  (uiop:string-prefix-p "/" fragment)
                  (string-equal fragment *world-inbox-designator*)
                  (world-message-participant world fragment))
          (return-from main-resolve-conversation
            (channel-identifier (world-channel-designated world configuration fragment))))
      (world-error (condition)
        (error 'configuration-error :message (autolith-error-message condition))))
   (let* ((pathnames (conversation-list configuration))
          (statuses  (mapcar #'rest (localgroup-statuses configuration)))
          (identifiers
            (remove-duplicates
             (append (mapcar #'channel-identifier (world-channels world))
                     (mapcar #'pathname-name pathnames)
                     (loop for status in statuses
                           for identifier = (getf status :conversation-id)
                           when (non-empty-string-p identifier)
                             collect identifier))
             :test #'string=))
          (matches   (identifier-match fragment identifiers)))
     (cond
       ((null matches)
        (error 'configuration-error
               :message (format nil "No saved or running conversation matches ~S." fragment)))
       ((rest matches)
        (error 'configuration-error
               :message
               (format nil "Conversation ID ~S is ambiguous. Use a longer fragment or a full ID:~%~{  ~A~%~}"
                       fragment
                       (loop for identifier in (sort (copy-list matches) #'string<)
                             collect
                             (let* ((channel (world-channel-find world identifier))
                                    (description
                                      (main--open-match-description
                                       identifier
                                       :pathname (find identifier pathnames :key #'pathname-name :test #'string=)
                                       :status   (find identifier statuses
                                                       :key  (lambda (status) (getf status :conversation-id))
                                                       :test #'equal))))
                               (if channel
                                   (format nil "~A  ~A" (world-channel-path world channel) description)
                                   description))))))
       (t
        (first matches))))))
