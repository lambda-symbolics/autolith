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
  "Resolve a resident's name or an ID fragment against saved and live channels.

Exact channel identifiers take priority over bare participant names. Use
@NAME for a participant's residence. Ambiguous ID fragments report their
full displayed IDs and available metadata."
  (let ((world (world-load configuration)))
    (when (or (uiop:string-prefix-p "@" fragment)
              (string-equal fragment *world-inbox-designator*)
              (world-message-participant world fragment))
      (return-from main-resolve-conversation
        (handler-case
            (channel-identifier (world-channel-designated world configuration fragment))
          (world-error (condition)
            (error 'configuration-error :message (autolith-error-message condition)))))))
  (let* ((pathnames (conversation-list configuration))
         (statuses  (mapcar #'rest (localgroup-statuses configuration)))
         (identifiers
           (remove-duplicates
            (append (mapcar #'pathname-name pathnames)
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
                            (main--open-match-description
                             identifier
                             :pathname (find identifier pathnames
                                             :key #'pathname-name :test #'string=)
                             :status   (find identifier statuses
                                             :key  (lambda (status)
                                                     (getf status :conversation-id))
                                             :test #'equal))))))
      (t
       (first matches)))))


;;;; -- Explicit Detached Starts --

(-> main--validate-detached-options
    (clingon:command &key (:open-requested-p boolean))
    null)
(defun main--validate-detached-options (command &key open-requested-p)
  "Reject detached opens and unattended starts requiring interactive input."
  (when (getopt* command ':detached)
    (when open-requested-p
      (error 'configuration-error
             :message "--detached starts a new channel; open attaches this terminal."))
    (dolist (option '(:images :simulate-crash :localgroup-handoff))
      (when (getopt* command option)
        (error 'configuration-error
               :message (format nil "--detached cannot be combined with ~A."
                                (case option
                                  (:images
                                   "--image")
                                  (:simulate-crash
                                   "--simulate-crash")
                                  (:localgroup-handoff
                                   "--localgroup-handoff")))))))
  nil)

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
             :message    "The session did not publish a matching conversation ID and PID."
             :operation  ':status
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
                        (:pristine-p boolean) (:conversation-id (option string))
                        (:launch list))
    null)
(defun main--start-detached
    (configuration &key permission-mode immutable-p pristine-p conversation-id launch)
  "Start an unattended session and print its receipt.

The world server opens the channel, CONVERSATION-ID or a new one for the
launch directory, with the launch options LAUNCH states, and keeps it running
with no terminal attached. A pristine start is a rescue session, which runs
apart from the world server in a process of its own and keeps startup
ownership until its receipt is printed."
  (if pristine-p
      (localgroup-handoff-spawn-fresh
       configuration
       :permission-mode   permission-mode
       :immutable-p       immutable-p
       :pristine-p        t
       :conversation-id   conversation-id
       :attach-expected-p nil
       :ready-function
       (lambda (identifier)
         (main--print-detached-receipt (localgroup--find-record configuration identifier))))
      (let ((identifier (main--open-world-channel configuration
                                                  :conversation-id conversation-id
                                                  :new-p           (null conversation-id)
                                                  :permission-mode permission-mode
                                                  :immutable-p     immutable-p
                                                  :launch          launch)))
        (main--print-detached-receipt (localgroup--find-record configuration identifier))))
  nil)
