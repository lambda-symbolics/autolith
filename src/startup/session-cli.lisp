(in-package #:autolith)

;;;; -- CLI Conversation Selection --

(-> main--resume-match-description
    (string &key (:pathname (option pathname)) (:status list))
    string)
(defun main--resume-match-description (identifier &key pathname status)
  "Return an actionable identifier and available context for one resume match."
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
(defun main-resolve-conversation (configuration prefix)
  "Resolve PREFIX against saved and live conversations, preferring exact IDs.

Accept canonical and displayed prefixes with localgroup's case conventions.
Report every ambiguous match with its full displayed ID and available metadata."
  (let* ((pathnames  (conversation-list configuration))
         (statuses  (mapcar #'rest (localgroup-statuses configuration)))
         (identifiers
           (remove-duplicates
            (append (mapcar #'pathname-name pathnames)
                    (loop for status in statuses
                          for identifier = (getf status :conversation-id)
                          when (non-empty-string-p identifier)
                            collect identifier))
            :test #'string=)))
    (labels ((normalize (identifier)
               "Return a normalized full ID or an unchanged prefix."
               (handler-case (session-identifier-normalize identifier)
                 (localgroup-error () identifier))))
      (let* ((exact      (find prefix identifiers :test #'string=))
             (normalized (normalize prefix))
             (matches
               (or (and exact (list exact))
                   (remove-if-not
                    (lambda (identifier) (string= normalized (normalize identifier)))
                    identifiers)
                   (remove-if-not
                    (lambda (identifier) (localgroup--session-prefix-p prefix identifier))
                    identifiers))))
        (cond
          ((null matches)
           (error 'configuration-error
                  :message (format nil "No saved or running conversation matches ~S." prefix)))
          ((rest matches)
           (error 'configuration-error
                  :message
                  (format nil "Conversation ID prefix ~S is ambiguous. Use a longer prefix or a full ID:~%~{  ~A~%~}"
                          prefix
                          (loop for identifier in (sort matches #'string<)
                                collect
                                (main--resume-match-description
                                 identifier
                                 :pathname (find identifier pathnames
                                                 :key #'pathname-name :test #'string=)
                                 :status   (find identifier statuses
                                                 :key (lambda (status)
                                                        (getf status :conversation-id))
                                                 :test #'equal))))))
          (t
           (first matches)))))))


;;;; -- Explicit Detached Starts --

(-> main--validate-detached-options
    (clingon:command &key (:resume-requested-p boolean) (:resume-id (option string)))
    null)
(defun main--validate-detached-options (command &key resume-requested-p resume-id)
  "Reject unattended starts which require interactive startup input."
  (when (getopt* command ':detached)
    (when (and resume-requested-p (not (non-empty-string-p resume-id)))
      (error 'configuration-error
             :message "Detached resume requires a conversation ID prefix: autolith resume PREFIX --detached."))
    (dolist (option '(:images :simulate-crash :localgroup-handoff))
      (when (getopt* command option)
        (error 'configuration-error
               :message (format nil "--detached cannot be combined with ~A."
                                (case option
                                  (:images "--image")
                                  (:simulate-crash "--simulate-crash")
                                  (:localgroup-handoff "--localgroup-handoff")))))))
  nil)

(-> main--validate-live-options (clingon:command) null)
(defun main--validate-live-options (command)
  "Reject launch-only overrides rather than silently ignoring them on live reuse."
  (dolist (option '(:model :provider :permissions :immutable :pristine
                   :fullscreen :almighty :images :simulate-crash))
    (when (getopt* command option)
      (error 'configuration-error
             :message (format nil "A running conversation cannot accept the launch option --~A."
                              (if (eq option ':images) "image" (string-downcase option))))))
  nil)

(-> main--live-conversation-record (configuration string) (option cons))
(defun main--live-conversation-record (configuration identifier)
  "Find an exact live conversation endpoint without selecting another ID by prefix."
  (find identifier (localgroup-endpoint-records configuration)
        :key (lambda (entry) (localgroup--record-session-id (rest entry)))
        :test #'string=))

(-> main--print-detached-receipt
    (cons &key (:stream stream) (:styled-p boolean)) null)
(defun main--print-detached-receipt
    (record &key (stream *standard-output*)
                 (styled-p (localgroup--status-output-styled-p stream)))
  "Print the authenticated conversation ID and actual session PID from RECORD."
  (let* ((response (localgroup-query-record record ':status))
         (status   (getf (rest response) :status))
         (fields   (rest status))
         (id       (getf fields :conversation-id))
         (pid      (getf fields :pid)))
    (unless (and (eq (first status) ':localgroup-status)
                 (equal id (localgroup--record-session-id (rest record)))
                 (equal id (getf fields :session-id))
                 (typep pid '(integer 1))
                 (eql pid (getf (rest (rest record)) :pid)))
      (error 'localgroup-error
             :message "The session did not publish a matching conversation ID and PID."
             :operation ':status
             :session-id (localgroup--record-session-id (rest record))))
    (localgroup--write-styled-line
     stream
     (list (terminal-span ':notice "Conversation ")
           (terminal-span ':brand (conversation-identifier-display id))
           (terminal-span ':dim "  PID ")
           (terminal-span ':success (princ-to-string pid)))
     styled-p)
    (force-output stream))
  nil)

(-> main--start-detached
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:pristine-p boolean) (:conversation-id (option string)))
    null)
(defun main--start-detached
    (configuration &key permission-mode immutable-p pristine-p conversation-id)
  "Start an unattended session, with no fallback to an attached or direct start."
  (let* ((identifier (localgroup-handoff-spawn-fresh
                      configuration
                      :permission-mode   permission-mode
                      :immutable-p       immutable-p
                      :pristine-p        pristine-p
                      :conversation-id   conversation-id
                      :attach-expected-p nil))
         (record     (localgroup--find-record configuration identifier)))
    (handler-case (main--print-detached-receipt record)
      (serious-condition (condition)
        ; mallet:suppress no-ignore-errors -- stop only the fresh process after a failed receipt
        (ignore-errors (localgroup-query-record record ':kill))
        (error condition))))
  nil)
