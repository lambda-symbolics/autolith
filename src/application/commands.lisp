(in-package #:autolith)

;;;; -- Interactive Commands --

(-> application-help () string)
(defun application-help ()
  "Return the concise interactive command reference."
  (let* ((entries (application-command-completion-entries))
         (label-width
           (loop for entry in entries
                 maximize (length (terminal-completion-label entry)))))
    (format nil "~{~A~^~%~}"
            (loop for entry in entries
                  collect (format nil "~vA  ~A"
                                  label-width
                                  (terminal-completion-label entry)
                                  (getf entry :description))))))

(-> application--entry-fields (list) list)
(defun application--entry-fields (fields)
  "Return aligned semantic FIELDS, each containing a label, style, and value.

Measure sanitized labels in terminal cells. Values wrap through the ordinary
transcript renderer, including in fullscreen."
  (let* ((labels (mapcar (lambda (field)
                          (sanitize-text (first field) :single-line-p t))
                        fields))
         (width (loop for label in labels maximize (text-cell-width label) into size
                      finally (return (or size 0)))))
    (loop for field in fields
          for safe-label in labels
          append (list (terminal-span ':dim
                                      (format nil "  ~A~A  " safe-label
                                              (make-string (- width (text-cell-width safe-label))
                                                           :initial-element #\Space)))
                       (terminal-span (second field) (princ-to-string (third field)))
                       (terminal-span ':plain (string #\Newline))))))

(-> application-conversations-entry (application) list)
(defun application-conversations-entry (application)
  "Return saved conversation names and metadata as semantic terminal spans."
  (let ((items (application--conversation-items application)))
    (if items
        (append (list (terminal-span ':brand (format nil "conversations~%")))
                (loop for item in items
                      append (list (terminal-span ':code (getf item :name))
                                   (terminal-span ':dim
                                                  (format nil "~%  ~A~%"
                                                          (getf item :description))))))
        (list (terminal-span ':hint "No saved conversations exist.")))))

(-> application-list-conversations (application) string)
(defun application-list-conversations (application)
  "Return saved conversations newest first with their times and origins."
  (let ((items (application--conversation-items application)))
    (if items
        (format nil "conversations~%~{~A~%~}"
                (loop for item in items
                      collect (format nil "~A  ~A"
                                      (getf item :name)
                                      (getf item :description))))
        "No saved conversations exist.")))

(-> application--agenda-status-style (agenda-status) keyword)
(defun application--agenda-status-style (status)
  "Return the terminal style associated with agenda STATUS."
  (case status
    (:done ':success)
    (:blocked ':failure)
    (:doing ':brand)
    (:note ':dim)
    (otherwise ':hint)))

(-> application-agenda-entry (application) (or string list))
(defun application-agenda-entry (application)
  "Return the current workspace agenda as a readable transcript entry."
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((configuration (application-configuration application))
           (record (agenda-current configuration (agenda-load configuration)))
           (items (and record (workspace-agenda-items record))))
      (if (null items)
          "The current workspace agenda is empty."
          (append
           (list (terminal-span ':brand "agenda")
                 (terminal-span
                  ':dim
                  (format nil "  ~A~%"
                          (namestring
                           (config :working-directory configuration)))))
           (loop for item in items
                 append
                 (list
                  (terminal-span
                   (application--agenda-status-style
                    (agenda-item-status item))
                   (format nil "  [~(~A~)] " (agenda-item-status item)))
                  (terminal-span ':plain (agenda-item-text item))
                  (terminal-span
                   ':dim
                   (format nil "~%           id ~A~@[ · memories ~{~A~^, ~}~]~%"
                           (agenda-item-identifier item)
                           (agenda-item-memory-identifiers item))))))))))

(-> application--papercut-list-row (papercut) list)
(defun application--papercut-list-row (papercut)
  "Return one styled compact row for PAPERCUT."
  (list
   (terminal-span
    ':failure
    (format nil "  ! ~A  " (papercut-short-identifier papercut)))
   (terminal-span
    ':strong
    (format nil "~A~%"
            (sanitize-text (papercut-title papercut)
                           :single-line-p t)))
   (terminal-span
    ':dim
    (format nil "    reported ~A  ·  ~A~%"
            (papercut-timestamp-string (papercut-reported-at papercut))
            (papercut-call-source papercut)))))

(-> application-papercuts-entry (application) (or string list))
(defun application-papercuts-entry (application)
  "Return the current workspace's papercuts as a compact transcript entry."
  (let* ((configuration (application-configuration application))
         (papercuts (papercut-list configuration)))
    (if (null papercuts)
        (list (terminal-span ':hint
                             "No papercuts recorded for this workspace."))
        (append
         (list (terminal-span
                ':failure
                (format nil "PAPERCUTS  ~D~%" (length papercuts)))
               (terminal-span
                ':dim
                (format nil "  newest first · use (papercut \"ID\") for the full report~%")))
         (loop for papercut in papercuts
               append (application--papercut-list-row papercut))))))

(-> application-papercut-entry (application string) (or string list))
(defun application-papercut-entry (application identifier)
  "Return one complete PAPERCUT report or a readable resolution notice."
  (let ((configuration (application-configuration application)))
    (multiple-value-bind (papercut status matches)
        (papercut-resolve configuration identifier)
      (case status
        (:found
         (list
          (terminal-span ':failure (format nil "PAPERCUT~%"))
          (terminal-span
           ':dim
           (format nil "  id ~A~%  reported ~A~%"
                   (papercut-identifier papercut)
                   (papercut-timestamp-string (papercut-reported-at papercut))))
          (terminal-span
           ':strong
           (format nil "  ~A~%~%"
                   (sanitize-text (papercut-title papercut)
                                  :single-line-p t)))
          (terminal-span ':plain (sanitize-text (papercut-content papercut)))))
        (:ambiguous
         (append
          (list (terminal-span
                 ':failure
                 (format nil
                         "Papercut ID ~A is ambiguous. Enter more characters.~%"
                         (sanitize-text identifier :single-line-p t)))
                (terminal-span ':dim "Matching reports:~%"))
          (loop for match in matches
                append (application--papercut-list-row match))))
        (otherwise
         (format nil
                 "No papercut matches ~S. Run (papercuts) to list current reports."
                 identifier))))))

(-> application-papercut-close-entry (application string) (or string list))
(defun application-papercut-close-entry (application identifier)
  "Close one active papercut selected by IDENTIFIER and return feedback."
  (let ((configuration (application-configuration application)))
    (multiple-value-bind (papercut status matches)
        (papercut-resolve configuration identifier)
      (case status
        (:found
         (papercut-mark-closed
          configuration
          (papercut-identifier papercut)
          :resolution
          (format nil "Closed through (papercut-close ~S)."
                  (papercut-identifier papercut)))
         (list
          (terminal-span ':success "PAPERCUT CLOSED  ")
          (terminal-span
           ':strong
           (sanitize-text (papercut-title papercut) :single-line-p t))))
        (:ambiguous
         (append
          (list (terminal-span
                 ':failure
                 (format nil
                         "Papercut ID ~A is ambiguous. Enter more characters.~%"
                         (sanitize-text identifier :single-line-p t)))
                (terminal-span ':dim "Matching reports:~%"))
          (loop for match in matches
                append (application--papercut-list-row match))))
        (otherwise
         (format nil
                 "No active papercut matches ~S. Run (papercuts) to list reports."
                 identifier))))))


;;;; -- Usage Status --

(-> application--conversation-usage (application) list)
(defun application--conversation-usage (application)
  "Return summed token and prompt-cache usage for the active conversation."
  (let ((input 0)
        (output 0)
        (total 0)
        (cached 0)
        (cache-created 0)
        (cache-seen-p nil))
    (labels ((usage-count (usage key)
               "Return the integer usage value stored under KEY and whether it exists."
               (let* ((entry (assoc key usage :test #'string=))
                      (value (and entry (second entry))))
                 (values (if (integerp value) value 0)
                         (and entry (integerp value))))))
      (conversation-map-records
       (application-conversation application)
       (lambda (record)
         (when (eq (first record) :provider)
           (let ((usage (getf (getf (rest record) :metadata) :usage)))
             (when (listp usage)
               (incf input (usage-count usage "input_tokens"))
               (incf output (usage-count usage "output_tokens"))
               (incf total (usage-count usage "total_tokens"))
               (multiple-value-bind (count present-p)
                   (usage-count usage "cached_input_tokens")
                 (incf cached count)
                 (setf cache-seen-p (or cache-seen-p present-p)))
               (multiple-value-bind (count present-p)
                   (usage-count usage "cache_creation_input_tokens")
                 (incf cache-created count)
                 (setf cache-seen-p (or cache-seen-p present-p)))))))))
    (list :input input
          :output output
          :total total
          :cached cached
          :cache-created cache-created
          :cache-seen-p cache-seen-p)))

(-> application--token-count-description (integer) string)
(defun application--token-count-description (count)
  "Return COUNT as a compact human-readable token quantity."
  (cond
    ((< count 1000)
     (format nil "~D" count))
    ((< count 1000000)
     (format nil "~,1FK" (/ count 1000)))
    (t
     (format nil "~,2FM" (/ count 1000000)))))

(-> application--prompt-cache-description (list) string)
(defun application--prompt-cache-description (usage)
  "Return a compact prompt-cache summary for aggregate USAGE."
  (let* ((input (getf usage :input))
         (cached (getf usage :cached))
         (created (getf usage :cache-created))
         (rate (and (plusp input)
                    (min 100 (round (* 100 cached) input)))))
    (format nil "~A read~A, ~A written"
            (application--token-count-description cached)
            (if rate (format nil " (~D% of input)" rate) "")
            (application--token-count-description created))))


;;;; -- Prompt-Cache Miss Notices --

(-> application--prompt-cache-miss-notice (list) list)
(defun application--prompt-cache-miss-notice (miss)
  "Return measured uncached input and any accompanying local observation."
  (list (terminal-span ':failure "! ")
        (terminal-span ':notice "cache miss ")
        (terminal-span ':code
                       (application--token-count-description
                        (getf miss :uncached-tokens)))
        (terminal-span ':dim " / ")
        (terminal-span ':code
                       (application--token-count-description (getf miss :prompt-tokens)))
        (terminal-span ':dim " input tokens uncached")
        (terminal-span ':dim
                       (case (getf miss :cause)
                         (:model-changed " · model changed")
                         (:idle " · idle interval exceeded local heuristic")
                         (:prompt-shrank " · prompt size decreased")
                         (:resumed " · resumed session")
                         (otherwise "")))
        (terminal-span ':dim
                       (if (getf miss :stalled-p)
                           " · cached-token count stagnant"
                           ""))))

(-> application-note-prompt-cache-request-started (application) null)
(defun application-note-prompt-cache-request-started (application)
  "Start a local cache observation window without warning on resumed history.

The first completed request establishes the baseline for this loaded conversation.
Persisted usage describes an earlier session, not a cache expectation for this one."
  (application-idle-compaction-note-started application)
  (let ((conversation (application-conversation application)))
    (unless (eq (application-prompt-cache-conversation application) conversation)
      (setf (application-prompt-cache-conversation application) conversation
            (application-prompt-cache-baseline application) nil))
    (setf (application-context-estimate-cache application) nil
          (application-prompt-cache-request-started-at application)
          (get-universal-time)
          (application-prompt-cache-request-model application)
          (config :model (application-configuration application))))
  nil)

(-> application-note-prompt-cache-request-completed (application list) null)
(defun application-note-prompt-cache-request-completed (application details)
  "Report a prompt-cache miss for the request completing with DETAILS.

The completed request's usage becomes the next baseline whether or not the
notice preference is on, so enabling it mid-session takes effect immediately."
  (application-idle-compaction-note-completed application (getf details :usage))
  (let* ((usage (getf details :usage))
         (model (application-prompt-cache-request-model application))
         (miss (prompt-cache-miss-detect
                (application-prompt-cache-baseline application)
                usage
                :started-at
                (application-prompt-cache-request-started-at application)
                :model model))
         (baseline (prompt-cache-baseline-create
                    usage
                    :completed-at (get-universal-time)
                    :model model)))
    (when baseline
      (setf (application-prompt-cache-baseline application) baseline))
    (when (and miss (application-cache-miss-notices-p application))
      (application-present application (application--prompt-cache-miss-notice miss))))
  nil)

(-> application--window-label ((option integer) string) string)
(defun application--window-label (minutes fallback)
  "Return the human name of a MINUTES-long rate limit window."
  (labels ((approximately-p (expected)
             "Return true when MINUTES is within five percent of EXPECTED."
             (and minutes
                  (<= (* expected 95/100) minutes (* expected 105/100)))))
    (cond
      ((approximately-p 300) "5h")
      ((approximately-p 1440) "daily")
      ((approximately-p 10080) "weekly")
      ((approximately-p 43200) "monthly")
      ((null minutes) fallback)
      ((>= minutes 60) (format nil "~Dh" (round minutes 60)))
      (t (format nil "~Dm" minutes)))))

(-> application--reset-description ((option integer)) (option string))
(defun application--reset-description (resets-at)
  "Return when RESETS-AT universal time occurs, as a compact local time."
  (when resets-at
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time resets-at)
      (declare (ignore second))
      (if (< (- resets-at (get-universal-time)) 86400)
          (format nil "~2,'0D:~2,'0D" hour minute)
          (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
                  year month date hour minute)))))

(-> application--limit-label (string list) string)
(defun application--limit-label (fallback-label window)
  "Return the row label naming rate limit WINDOW, using FALLBACK-LABEL if unsized."
  (format nil "~A limit"
          (application--window-label (getf window :window-minutes)
                                     fallback-label)))

(-> application--limit-spans (string list (integer 0)) list)
(defun application--limit-spans (label window width)
  "Return rate limit WINDOW as a usage bar row with LABEL padded to WIDTH."
  (let* ((used (min 100 (max 0 (getf window :used-percent))))
         (left (- 100 used))
         (cells 20)
         (filled (round (* cells left) 100))
         (bar (concatenate 'string
                           (make-string filled :initial-element #\█)
                           (make-string (- cells filled) :initial-element #\░))))
    (list (terminal-span :dim (format nil "  ~vA " width label))
          (terminal-span :plain (format nil "[~A] ~D% left" bar (round left)))
          (terminal-span :dim
                         (format nil "~@[ (resets ~A)~]~%"
                                 (application--reset-description
                                  (getf window :resets-at)))))))

(-> application--toggle-state (boolean) string)
(defun application--toggle-state (enabled-p)
  "Return the compact user-facing state for ENABLED-P."
  (if enabled-p "on" "off"))

(-> application-info-entry (application) list)
(defun application-info-entry (application)
  "Return APPLICATION's read-only model and runtime settings display."
  (let ((configuration (application-configuration application)))
    (append
     (list (terminal-span ':brand (format nil "settings~%")))
     (application--field-rows
      (list
       (list "model" (config :model configuration))
       (list "effort" (config :reasoning-effort configuration))
       (list "Codex fast"
             (cond
               ((configuration-codex-fast-mode-active-p configuration)
                "on")
               ((config :codex-fast-mode-p configuration)
                "on (saved; inactive)")
               (t
                "off")))
       (list "web search" (config :web-search-mode configuration))
       (list "trace"
             (application--toggle-state
              (application-reasoning-traces-p application)))
       (list "timestamps"
             (application--toggle-state
              (application-turn-timestamps-p application)))
       (list "cache misses"
             (application--toggle-state
              (application-cache-miss-notices-p application)))
       (list "compact view"
             (application--toggle-state
              (application-compact-view-p application)))
       (list "STE"
             (application--toggle-state
              (config :simple-technical-english-p configuration)))
       (list "session titles"
             (application--toggle-state
              (config :session-title-generation-p configuration)))
       (list "hurry-up"
             (application--toggle-state
              (application-hurry-up-p application)))
       (list "permissions"
             (application--permission-mode-name
              (application-permission-mode application))))))))

(-> application-status-entry (application) list)
(defun application-status-entry (application)
  "Return the styled /status summary of APPLICATION's session and rate limits."
  (let* ((configuration (application-configuration application))
         (provider (application-provider application))
         (snapshot (and provider (provider-rate-limits provider)))
         (usage (application--conversation-usage application))
         (primary (getf snapshot :primary))
         (secondary (getf snapshot :secondary))
         (primary-label (and primary
                             (application--limit-label "primary" primary)))
         (secondary-label (and secondary
                               (application--limit-label "weekly" secondary)))
         (fields
           (remove
            nil
            (list
             (list "model"
                   (format nil "~A (effort ~A)"
                           (config :model configuration)
                           (config :reasoning-effort configuration)))
             (list "reasoning trace"
                   (if (application-reasoning-traces-p application)
                       "visible summaries"
                       "hidden"))
             (list "conversation"
                   (conversation-identifier-display
                    (conversation-identifier
                     (application-conversation application))))
             (list "workspace"
                   (or (application--abbreviated-directory
                        (namestring (config :working-directory configuration)))
                       ""))
             (list "path"
                   (if (configuration-codex-fast-mode-active-p configuration)
                       "fast (2x Codex plan usage)"
                       "standard"))
             (list "web search" (config :web-search-mode configuration))
             (list "goal"
                   (let ((goal (application-goal application)))
                     (if goal
                         (format nil "~(~A~): ~A"
                                 (getf goal :status)
                                 (getf goal :objective))
                         "none")))
             (list "token usage"
                   (format nil "~A total (~A input + ~A output)"
                           (application--token-count-description
                            (getf usage :total))
                           (application--token-count-description
                            (getf usage :input))
                           (application--token-count-description
                            (getf usage :output))))
             (and (getf usage :cache-seen-p)
                  (list "prompt cache"
                        (application--prompt-cache-description usage)))
             (list "context"
                   (multiple-value-bind (used estimated-p)
                       (application-context-usage application)
                     (let ((window (config :context-window configuration)))
                       (format nil "~A~A of ~A used (~D%), compacts at ~D%"
                               (if estimated-p "~" "")
                               (application--token-count-description used)
                               (application--token-count-description window)
                               (round (* 100 used) (max 1 window))
                               (config :compaction-threshold-percent
                                       configuration))))))))
         (width
           (application--field-label-width
            (append (mapcar #'first fields)
                    (remove nil (list primary-label secondary-label))))))
    (append
     (list (terminal-span :brand "autolith")
           (terminal-span :dim (format nil " v~A~%" *autolith-version*)))
     (application--field-rows fields :width width)
     (cond
       ((null snapshot)
        (list (terminal-span :dim
                             "  No rate limit data yet; send a message first.")))
       (t
        (append
         (when primary
           (application--limit-spans primary-label primary width))
         (when secondary
           (application--limit-spans secondary-label secondary width))))))))


;;;; -- Interactive Pickers --

(-> application--calendar-parts (integer) (values string string))
(defun application--calendar-parts (universal-time)
  "Return UNIVERSAL-TIME as separate local calendar date and time strings."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second))
    (values (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)
            (format nil "~2,'0D:~2,'0D" hour minute))))

(-> application--calendar-description (integer) string)
(defun application--calendar-description (universal-time)
  "Return UNIVERSAL-TIME as a compact local calendar description."
  (multiple-value-bind (date time)
      (application--calendar-parts universal-time)
    (format nil "~A ~A" date time)))

(defparameter *conversation-preview-width* 48
  "The cell width of the newest-message excerpt in pickers.")

(-> application--conversation-preview-from-metadata
    (conversation-picker-metadata)
    (option string))
(defun application--conversation-preview-from-metadata (metadata)
  "Return a one-line excerpt from compact picker METADATA."
  (let ((preview (conversation-picker-metadata-preview metadata)))
    (when preview
      (text-cell-prefix
       (sanitize-text preview :single-line-p t)
       *conversation-preview-width*))))

(-> application--conversation-current-directory-p
    ((option string) pathname)
    boolean)
(defun application--conversation-current-directory-p (directory current)
  "Return true when recorded DIRECTORY denotes CURRENT."
  (and (non-empty-string-p directory)
       (handler-case
           (string= (namestring
                     (uiop:ensure-directory-pathname (pathname directory)))
                    (namestring (uiop:ensure-directory-pathname current)))
         (error ()
           nil))))

(-> application--conversation-description
    (integer &key (:directory (option string)) (:current-p boolean)
             (:preview (option string)))
    (values string terminal-styled-text))
(defun application--conversation-description
    (universal-time &key directory current-p preview)
  "Return plain and styled resume descriptions for one conversation."
  (multiple-value-bind (date time)
      (application--calendar-parts universal-time)
    (let ((suffix
            (format nil "~@[, ~A~]~:[~;, current~]~@[ · ~A~]"
                    directory current-p preview)))
      (values
       (format nil "~A ~A~A" date time suffix)
       (list (terminal-span ':plain (format nil "~A " date))
             (terminal-span ':timestamp-time time)
             (terminal-span ':plain suffix))))))

(-> application--conversation-metadata-tally
    (conversation-picker-metadata)
    string)
(defun application--conversation-metadata-tally (metadata)
  "Return a compact work-time or turn-count tally for picker METADATA."
  (let ((working-seconds (conversation-picker-metadata-working-seconds metadata))
        (user-turn-count (conversation-picker-metadata-user-turn-count metadata)))
    (cond
      ((plusp working-seconds)
       (terminal-ui--duration-text working-seconds))
      ((plusp user-turn-count)
       (format nil "~D turn~:P" user-turn-count))
      (t
       "0 turns"))))

(-> application--conversation-tally (pathname) string)
(defun application--conversation-tally (pathname)
  "Return a compact work-time or turn-count tally for PATHNAME."
  (let ((metadata (conversation-picker-metadata-find pathname)))
    (if metadata
        (application--conversation-metadata-tally metadata)
        "0 turns")))

(-> application--conversation-items (application) list)
(defun application--conversation-items (application)
  "Return grouped picker items, newest first within each workspace section."
  (let* ((configuration (application-configuration application))
         (current-identifier
           (conversation-identifier (application-conversation application)))
         (current-directory
           (config :working-directory configuration))
         (current-group
           (format nil "current directory · ~A"
                   (application--abbreviated-directory
                    (namestring current-directory))))
         (current-items nil)
         (other-items nil))
    (dolist (pathname (conversation-list configuration))
      (let* ((identifier (pathname-name pathname))
             (identifier-display
               (conversation-identifier-display identifier))
             (header (conversation-peek-header pathname))
             (directory (getf (rest header) :directory))
             (current-directory-p
               (application--conversation-current-directory-p
                directory
                current-directory))
             (metadata (conversation-picker-metadata-find pathname))
             (title (and metadata
                         (conversation-picker-metadata-title metadata)))
             (preview
               (and metadata
                    (application--conversation-preview-from-metadata metadata))))
        (multiple-value-bind (description description-spans)
            (application--conversation-description
             (conversation-storage-write-date pathname)
             :directory (and (not current-directory-p)
                             (application--abbreviated-directory directory))
             :current-p (string= identifier current-identifier)
             :preview preview)
          (let* ((titled-p (and title
                                (not (string= title identifier-display))))
                 (item-description
                   (if titled-p
                       (format nil "~A · ~A" identifier-display description)
                       description))
                 (item-description-spans
                   (if titled-p
                       (append
                        (list (terminal-span ':code
                                             (format nil "~A · " identifier-display)))
                        description-spans)
                       description-spans))
                 (item
                   (list :name (or title identifier-display)
                         :value identifier
                         :argument nil
                         :group (if current-directory-p
                                    current-group
                                    "other sessions")
                         :tally (if metadata
                                    (application--conversation-metadata-tally metadata)
                                    "0 turns")
                         :description item-description
                         :description-spans item-description-spans)))
            (if current-directory-p
                (push item current-items)
                (push item other-items))))))
    (append (nreverse current-items) (nreverse other-items))))

(-> application--effort-items (application) list)
(defun application--effort-items (application)
  "Return picker items for the active model's supported reasoning efforts."
  (let* ((configuration (application-configuration application))
         (current (config :reasoning-effort configuration)))
    (loop for effort in
          (configuration--reasoning-efforts-for
           (config :model configuration) (config :model-provider configuration))
          collect (list :name effort
                        :argument nil
                        :description (if (string= effort current)
                                         "current"
                                         "")))))

(-> application--persist-model-selection (application configuration) null)
(defun application--persist-model-selection (application configuration)
  "Record the selected provider, model and effort on the active conversation."
  (let ((provider (config :model-provider configuration)))
    (conversation-set-model-selection
     (application-conversation application)
     (config :model configuration)
     (config :reasoning-effort configuration)
     :provider (and (non-empty-string-p provider) provider)))
  nil)

(-> application-set-reasoning-effort (application string) null)
(defun application-set-reasoning-effort (application effort)
  "Switch APPLICATION to reasoning EFFORT and save it as the global default.

The model the effort belongs to is saved beside it, so a later start pairs
the effort with the same model even when the model was never chosen."
  (let ((configuration (application-configuration application)))
    (setf (config :reasoning-effort configuration) effort)
    (configuration-persist configuration :model-provider)
    (configuration-persist configuration :model)
    (application--persist-model-selection application configuration)
    (application--install-configuration application configuration)))

(-> application--codex-fast-mode-environment-p () boolean)
(defun application--codex-fast-mode-environment-p ()
  "Return true when the process environment controls Codex Fast mode."
  (not (null (non-empty-string-p
              (uiop:getenv "AUTOLITH_CODEX_FAST_MODE")))))

(-> application-set-codex-fast-mode (application boolean) null)
(defun application-set-codex-fast-mode (application enabled-p)
  "Persist and apply Codex Fast mode for future provider requests."
  (when (application--codex-fast-mode-environment-p)
    (error 'configuration-error
           :message
           (format nil
                   "AUTOLITH_CODEX_FAST_MODE controls Fast mode for this ~
                    process; unset it before using (fast t) or (fast nil).")))
  (let ((configuration (application-configuration application)))
    (setf (config :codex-fast-mode-p configuration) enabled-p)
    (application--install-configuration application configuration)
    (application-publish-recovery-session application))
  nil)

(-> application--control-choice (t list) keyword)
(defun application--control-choice (value choices)
  "Require VALUE to name one of the keyword CHOICES before changing state."
  (unless (member value choices)
    (error 'configuration-error
           :message (format nil "Expected one of ~{~S~^, ~}, not ~S." choices value)))
  value)

(-> application--boolean-control (t) keyword)
(defun application--boolean-control (value)
  "Return the explicit boolean or status action represented by VALUE."
  (cond
    ((eq value t) ':on)
    ((null value) ':off)
    (t
     (application--control-choice value '(:on :off :status)))))

(-> application--parse-control (string list) list)
(defun application--parse-control (text choices)
  "Parse one quoted slash-command action from TEXT into native CHOICES."
  (let ((arguments (application-command--tokens text)))
    (when arguments
      (unless (= (length arguments) 1)
        (error 'configuration-error :message "A control accepts one action."))
      (let ((choice (find (first arguments) choices
                          :test #'string-equal :key #'symbol-name)))
        (unless choice
          (error 'configuration-error
                 :message (format nil "Expected ~{~(~A~)~^, ~}, not ~S." choices (first arguments))))
        (list choice)))))

(-> application--parse-boolean-control (string) list)
(defun application--parse-boolean-control (text)
  "Read a slash boolean mode without applying Lisp string coercion."
  (application--parse-control text '(:on :off :status)))

(-> application--parse-slynk-control (string) list)
(defun application--parse-slynk-control (text)
  "Read a slash Slynk action."
  (application--parse-control text '(:start :stop :status)))

(-> application--parse-mcp-control (string) list)
(defun application--parse-mcp-control (text)
  "Read a slash MCP action."
  (application--parse-control text '(:refresh :reload :status)))

(-> application--parse-permissions-control (string) list)
(defun application--parse-permissions-control (text)
  "Read a slash command-permission action."
  (application--parse-control text '(:ask :auto :sandbox :full :list :clear)))

(-> application-codex-fast-mode-command (application &optional t) null)
(defun application-codex-fast-mode-command (application &optional (argument ':status))
  "Set or report APPLICATION's persisted Codex Fast mode."
  (let* ((configuration (application-configuration application))
         (mode (application--boolean-control argument))
         (enabled-p (config :codex-fast-mode-p configuration))
         (available-p
           (configuration-codex-fast-mode-available-p configuration)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (cond
          ((application--codex-fast-mode-environment-p)
           (format nil
                   "Codex Fast mode is ~A for this process. ~
                    AUTOLITH_CODEX_FAST_MODE controls it.~:[ The current ~
                    model uses the standard path.~;~]"
                   (if enabled-p "on" "off")
                   available-p))
          (available-p
           (format nil
                   "Codex Fast mode is ~A. This setting persists across restarts."
                   (if enabled-p "on" "off")))
          (t
           (format nil
                   "Codex Fast mode preference is ~A. The current model uses the standard path."
                   (if enabled-p "on" "off"))))))
      ((eq mode ':on)
       (application-set-codex-fast-mode application t)
       (application-present
        application
        (if available-p
            "Codex Fast mode is enabled and saved. Fast responses count more heavily against plan usage."
            (format nil
                    "Codex Fast mode preference is enabled and saved. ~
                     The current model uses the standard path."))))
      ((eq mode ':off)
       (application-set-codex-fast-mode application nil)
       (application-present application "Codex Fast mode is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (fast), (fast t), or (fast nil)."))))
  nil)

(-> application-set-reasoning-traces (application boolean) null)
(defun application-set-reasoning-traces (application enabled-p)
  "Persist and apply whether future reasoning summaries are visible."
  (let ((expanding-visibility-p
          (and enabled-p (not (application-reasoning-traces-p application)))))
    (setf (application-reasoning-traces-p application) enabled-p)
    (let ((provider (application-provider application)))
      (when provider
        (provider-set-reasoning-summaries provider enabled-p)))
    (when expanding-visibility-p
      (application-reset-history-pagination application)))
  (unless enabled-p
    (terminal-ui-set-preview-rows (application-ui application) nil))
  (application-publish-recovery-session application)
  nil)

(-> application-trace-command (application &optional t) null)
(defun application-trace-command (application &optional (argument ':status))
  "Show or change APPLICATION's visible reasoning-summary setting."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil
                "Reasoning summaries are ~:[hidden~;shown~]. This setting ~
                 persists across restarts."
                (application-reasoning-traces-p application))))
      ((eq mode ':on)
       (application-set-reasoning-traces application t)
       (application-present
        application
        "Visible reasoning summaries are enabled and saved."))
      ((eq mode ':off)
       (application-set-reasoning-traces application nil)
       (application-present application "Reasoning summaries are hidden and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (trace t) or (trace nil)."))))
  nil)

(-> application-set-turn-timestamps (application boolean) null)
(defun application-set-turn-timestamps (application enabled-p)
  "Persist and apply whether user and assistant headers show timestamps."
  (unless (eq (application-turn-timestamps-p application) enabled-p)
    (setf (application-turn-timestamps-p application) enabled-p)
    (application-reset-history-pagination application))
  (application-publish-recovery-session application)
  nil)

(-> application-turn-timestamps-command (application &optional t) null)
(defun application-turn-timestamps-command (application &optional (argument ':status))
  "Show or change APPLICATION's optional turn-timestamp presentation."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil
                "Turn timestamps are ~:[hidden~;shown~]. This setting persists across restarts."
                (application-turn-timestamps-p application))))
      ((eq mode ':on)
       (application-set-turn-timestamps application t)
       (application-present application "Turn timestamps are enabled and saved."))
      ((eq mode ':off)
       (application-set-turn-timestamps application nil)
       (application-present application "Turn timestamps are hidden and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (timestamps t) or (timestamps nil)."))))
  nil)

(-> application-set-cache-miss-notices (application boolean) null)
(defun application-set-cache-miss-notices (application enabled-p)
  "Persist and apply whether prompt-cache misses are reported after requests."
  (setf (application-cache-miss-notices-p application) enabled-p)
  nil)

(-> application-cache-miss-notices-command (application &optional t) null)
(defun application-cache-miss-notices-command (application &optional (argument ':status))
  "Show or change APPLICATION's optional prompt-cache miss notices."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil
                "Prompt-cache miss notices are ~:[off~;on~]. This setting persists across restarts."
                (application-cache-miss-notices-p application))))
      ((eq mode ':on)
       (application-set-cache-miss-notices application t)
       (application-present
        application
        "Prompt-cache miss notices are on and saved. Requests that re-read uncached context will be reported."))
      ((eq mode ':off)
       (application-set-cache-miss-notices application nil)
       (application-present application "Prompt-cache miss notices are off and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (cache-misses t) or (cache-misses nil)."))))
  nil)

(-> application-simple-technical-english-command
    (application &optional t)
    null)
(defun application-simple-technical-english-command (application &optional (argument ':status))
  "Show or change APPLICATION's Simple Technical English response style."
  (let* ((configuration (application-configuration application))
         (mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil
                "Simple Technical English is ~:[disabled~;enabled~]. This ~
                 setting persists across restarts."
                (config :simple-technical-english-p configuration))))
      ((eq mode ':on)
       (setf (config :simple-technical-english-p configuration) t)
       (application-present
        application
        "Simple Technical English is enabled and saved. Future replies will use it."))
      ((eq mode ':off)
       (setf (config :simple-technical-english-p configuration) nil)
       (application-present
        application
        "Simple Technical English is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (ste t) or (ste nil)."))))
  nil)

(-> application-session-titles-command (application &optional t) null)
(defun application-session-titles-command (application &optional (argument ':status))
  "Show or change automatic provider-generated session-title refreshes."
  (let* ((configuration (application-configuration application))
         (mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil
                "Provider-generated session titles are ~:[disabled~;enabled~]. This setting persists across restarts."
                (config :session-title-generation-p configuration))))
      ((eq mode ':on)
       (setf (config :session-title-generation-p configuration) t)
       (application-present
        application
        "Provider-generated session titles are enabled and saved."))
      ((eq mode ':off)
       (setf (config :session-title-generation-p configuration) nil)
       (application-present
        application
        "Provider-generated session titles are disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (titles t) or (titles nil)."))))
  nil)

(-> application-compact-tool-command (application &optional t) null)
(defun application-compact-tool-command (application &optional (argument ':status))
  "Persist and apply APPLICATION's compact tool presentation mode."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil "Compact tool presentation is ~:[disabled~;enabled~]."
                (application-compact-view-p application))))
      ((eq mode ':on)
       (setf (application-compact-view-p application) t)
       (application-publish-recovery-session application)
       (application-present
        application
        "Compact tool presentation is enabled and saved."))
      ((eq mode ':off)
       (when (application-compact-view-p application)
         (setf (application-compact-view-p application) nil)
         (application-reset-history-pagination application))
       (application-publish-recovery-session application)
       (application-present
        application
        "Compact tool presentation is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: (compact-tool t) or (compact-tool nil)."))))
  nil)

(-> application--credentialed-provider-names (application) list)
(defun application--credentialed-provider-names (application)
  "Return effective provider names whose stored credentials load successfully."
  (let ((configuration (application-configuration application)))
    (loop for registration in (provider-registrations)
          when (handler-case
                   (progn
                     (credential-manager-load
                      (provider-credential-manager
                       (funcall (provider-registration-factory registration)
                                configuration)))
                     t)
                 (serious-condition ()
                   nil))
            collect (provider-registration-name registration))))

(-> application--offered-provider-p (string (option string) list) boolean)
(defun application--offered-provider-p (provider-name current-provider credentialed)
  "Return true when PROVIDER-NAME is credentialed or serves the current model."
  (or (and current-provider (string= provider-name current-provider))
      (and (member provider-name credentialed :test #'string=) t)))

(-> application--model-items (application &key (:provider (option string))) list)
(defun application--model-items (application &key provider)
  "Return distinct model/provider picker items, optionally filtered by PROVIDER."
  (when provider (provider-model-selections :provider provider))
  (let* ((configuration (application-configuration application))
         (current (configuration-model-registration configuration))
         (current-provider (and current (provider-registration-name current)))
         (credentialed (application--credentialed-provider-names application)))
    (loop for registration in (provider-registrations)
          for name = (provider-registration-name registration)
          when (if provider
                   (string= name (provider--canonical-name provider))
                   (application--offered-provider-p name current-provider credentialed))
            append
            (loop for metadata in (provider-registration-models registration)
                  for model = (provider-model-name metadata)
                  collect (list :name (provider-model-selection-id model registration)
                                :model model :provider name
                                :current-p (and (eq current registration)
                                                (string= model (config :model configuration)))
                                :argument nil :group name
                                :description
                                (format nil "~{~A~^; ~}"
                                        (remove nil
                                                (list (when (and (eq current registration)
                                                                 (string= model (config :model configuration)))
                                                        "current")
                                                      (provider-model-description metadata)))))))))

(-> application--models-description (application &key (:provider (option string))) string)
(defun application--models-description (application &key provider)
  "Return a provider-distinct model listing, optionally restricted to PROVIDER."
  (let ((items (application--model-items application :provider provider)))
    (if items
        (format nil "Available models:~%~{- ~A~%~}" (mapcar (lambda (item) (getf item :name)) items))
        "No registered provider models are available.")))

(-> application-models-entry (application list &key (:provider (option string))) list)
(defun application-models-entry (application failures &key provider)
  "Return offered provider/model selections and discovery FAILURES."
  (let ((items (application--model-items application :provider provider)))
    (append
     (list (terminal-span ':brand (format nil "Available models~%")))
     (if items
         (loop for item in items
               append (list (terminal-span ':code (format nil "  ~A" (getf item :name)))
                            (terminal-span (if (getf item :current-p) ':success ':dim)
                                           (format nil " ~A~%" (getf item :description)))))
         (list (terminal-span ':hint (format nil "No registered provider models are available.~%"))))
     (when failures
       (cons (terminal-span ':notice (format nil "Model discovery warnings~%"))
             (loop for failure in failures
                   collect (terminal-span ':notice
                                          (format nil "  ~A~%" (autolith-error-message failure)))))))))

(-> application-models-command (application &key (:provider (option string))) null)
(defun application-models-command (application &key provider)
  "Refresh and show registered models, optionally restricted to PROVIDER."
  (when provider (provider-model-selections :provider provider))
  (let ((failures (provider-refresh-models (application-configuration application)
                                         :provider-name (and provider (provider--canonical-name provider)))))
    (application-present application (application-models-entry application failures :provider provider)))
  nil)

(-> application-set-model (application string &key (:provider (option string))) null)
(defun application-set-model (application model &key provider)
  "Select MODEL from PROVIDER, preserving provider identity in durable state."
  (multiple-value-bind (identifier registration)
      (provider-resolve-model model :provider provider)
    (unless registration
      (provider--selection-error
       model provider :reason ':unsupported-model
       :message (format nil "Unknown model ~S. Use (models) to list available models." model)))
    (let ((configuration
            (configuration-copy
             (application-configuration application)
             :model identifier
             :model-provider (provider-registration-name registration))))
      (application--install-configuration application configuration)
      (configuration-persist configuration :model-provider)
      (configuration-persist configuration :model)
      (application--persist-model-selection application configuration))))

;;;; -- Private Replay Skips --

(defparameter *application-replay-skip-groups*
  '((:tracked-changed
     "tracked definition changed since the commit was published")
    (:tracked-removed
     "tracked definition removed since the commit was published")
    (:tracked-appeared
     "tracked definition appeared since the commit was published")
    (:revision-moved
     "tracked source moved on; these overrode definitions of the older revision")
    (:owner-moved
     "owning package changed")
    (:uninterned-target
     "uninterned target without a replay identity"))
  "Presentation order and titles of private replay skip reasons.")

(-> application--replay-skip-name (image-replay-skip) string)
(defun application--replay-skip-name (skip)
  "Return SKIP's definition name with method qualifiers and specializers."
  (let ((*package* (find-package '#:autolith))
        (signature (definition-signature (image-replay-skip-definition skip))))
    (format nil "~(~A~)~{ ~(~S~)~}~@[ ~(~A~)~]"
            (second signature)
            (third signature)
            (fourth signature))))

(-> application--replay-skip-row (image-replay-skip) list)
(defun application--replay-skip-row (skip)
  "Return one styled row naming SKIP's definition."
  (list
   (terminal-span
    ':dim
    (format nil "    ~(~10A~) "
            (first (image-replay-skip-definition skip))))
   (terminal-span ':code (application--replay-skip-name skip))
   (terminal-span ':plain (string #\Newline))))

(-> application--short-revision ((option string)) string)
(defun application--short-revision (revision)
  "Return REVISION abbreviated for display, or a marker for an unknown one."
  (if (non-empty-string-p revision)
      (subseq revision 0 (min 8 (length revision)))
      "unknown"))

(-> application--replay-skips-entry
    (list &key (:commit-identifier (option string))
               (:lineage-source-commit (option string))
               (:image-source-commit (option string)))
    list)
(defun application--replay-skips-entry
    (skips &key commit-identifier lineage-source-commit image-source-commit)
  "Return the styled startup notice for private replay SKIPS, or NIL for none.

The notice names the selected COMMIT-IDENTIFIER with the revision its lineage
was published against and the revision this image runs, then lists every
skipped definition grouped by reason."
  (when skips
    (append
     (list (terminal-span
            ':failure
            (format nil "✗ PRIVATE REPLAY SKIPPED ~D DEFINITION~:[S~;~]~%"
                    (length skips)
                    (= (length skips) 1)))
           (terminal-span
            ':dim
            (format nil "  commit ~A · published against ~A · this image runs ~A~%"
                    (application--short-revision commit-identifier)
                    (application--short-revision lineage-source-commit)
                    (application--short-revision image-source-commit))))
     (loop for (reason title) in *application-replay-skip-groups*
           for group = (sort (remove-if-not
                              (lambda (skip)
                                (eq (image-replay-skip-reason skip) reason))
                              skips)
                             #'string<
                             :key #'application--replay-skip-name)
           when group
             append (cons (terminal-span ':notice
                                         (format nil "~%  ~A~%" title))
                          (loop for skip in group
                                append (application--replay-skip-row skip))))
     (list (terminal-span ':hint (format nil "~%  "))
           (terminal-span ':code "(fix-skipped-definitions)")
           (terminal-span
            ':hint
            (format nil " rebuilds or drops them in one model turn · the next self.commit drops any still skipped~%"))))))

(-> application--replay-lineage-source-commit (application) (option string))
(defun application--replay-lineage-source-commit (application)
  "Return the revision the selected private commit lineage was published against."
  (let ((commit (handler-case
                    (image-commit-current
                     (application-configuration application))
                  (error ()
                    nil))))
    (and commit (image-commit-source-commit commit))))

(-> application-replay-skips-entry (application) list)
(defun application-replay-skips-entry (application)
  "Return the startup notice for the running image's private replay skips, or NIL."
  (when *image-replay-skips*
    (application--replay-skips-entry
     *image-replay-skips*
     :commit-identifier *active-image-commit-identifier*
     :lineage-source-commit
     (application--replay-lineage-source-commit application)
     :image-source-commit (image-commit--base-source-commit nil))))

(-> application--skipped-definition-section
    (stream image-replay-skip integer (option configuration))
    null)
(defun application--skipped-definition-section (stream skip index configuration)
  "Write the numbered repair section for SKIP to STREAM.

The section carries the persisted override, the tracked source recorded when
it was published, and the tracked definition CONFIGURATION's source holds now."
  (let* ((definition (image-replay-skip-definition skip))
         (current
           (and configuration
                (handler-case
                    (self-tracked-definition configuration definition)
                  (error ()
                    nil))))
         (recorded
           (cond
             ((not (image-replay-skip-tracked-recorded-p skip))
              "not recorded")
             ((null (image-replay-skip-tracked skip))
              "a new name without a tracked definition")
             (t
              (format nil "~%~A" (image-replay-skip-tracked skip))))))
    (format stream
            "~%~D. ~(~A~) ~A~%~
             Reason: ~A~%~
             Persisted override:~%~A~%~
             Tracked definition when published: ~A~%~
             Current tracked definition~@[ in ~A~]:~%~A~%"
            index
            (first definition)
            (application--replay-skip-name skip)
            (image-replay-skip-message skip)
            (image-replay-skip-source skip)
            recorded
            (and current (tracked-definition-relative-pathname current))
            (if current
                (tracked-definition-source current)
                "none: the current source has no definition with this name and signature."))
    nil))

(-> application--skipped-definitions-prompt
    (list &key (:configuration (option configuration))
               (:commit-identifier (option string))
               (:lineage-source-commit (option string))
               (:image-source-commit (option string))
               (:comment string))
    string)
(defun application--skipped-definitions-prompt
    (skips &key configuration commit-identifier lineage-source-commit
                image-source-commit (comment ""))
  "Return the model request that resolves SKIPS according to the user's COMMENT."
  (with-output-to-string (stream)
    (format stream
            "Autolith skipped ~D private definition~:[s~;~] at startup because ~
             the tracked source they override moved on since they were ~
             published (private commit ~A, published against ~A; this image ~
             runs ~A). Resolve every skipped definition against the current ~
             tracked source.~2%"
            (length skips)
            (= (length skips) 1)
            (application--short-revision commit-identifier)
            (application--short-revision lineage-source-commit)
            (application--short-revision image-source-commit))
    (when (plusp (length comment))
      (format stream
              "User direction (takes precedence over the default repair policy):~%~A~2%"
              comment))
    (format stream
            "Default repair policy: for each definition below, compare the ~
             persisted override with the tracked definition it was written ~
             against and with the current tracked definition. Drop overrides ~
             the user asks to drop, including temporary development experiments, ~
             by leaving them uninstalled. Also drop any override whose behavior ~
             the current tracked source already provides. Otherwise rewrite ~
             the override on top of the current tracked definition, keeping ~
             its intent while following the current calling conventions, ~
             install it with self.redefine, and check that it works. When every ~
             definition is handled, publish the result with self.commit, even ~
             if all skipped overrides were dropped, so the next start replays ~
             only the retained overrides. Explain which overrides were dropped. ~
             Do not edit tracked source files.~%")
    (flet ((write-sections ()
             (loop for skip in skips
                   for index from 1
                   do (application--skipped-definition-section
                       stream skip index configuration))))
      (if configuration
          (with-tracked-definition-snapshot (configuration)
            (write-sections))
          (write-sections)))))

(-> application-fix-skipped-definitions (application &optional string) null)
(defun application-fix-skipped-definitions (application &optional (comment ""))
  "Queue one model turn to rebuild or drop skipped definitions, guided by COMMENT."
  (check-type comment string)
  (let ((skips *image-replay-skips*))
    (cond
      ((null skips)
       (application-present
        application
        (list (terminal-span
               ':hint
               "No private definitions were skipped at startup."))))
      (t
       (let ((controller (application-input-controller application)))
         (unless controller
           (error 'configuration-error
                  :message "Only an interactive session can queue the repair turn."))
         (unless (application-input-controller-enqueue-operation-request
                  controller
                  (application--skipped-definitions-prompt
                   skips
                   :configuration (application-configuration application)
                   :commit-identifier *active-image-commit-identifier*
                   :lineage-source-commit
                   (application--replay-lineage-source-commit application)
                   :image-source-commit
                   (image-commit--base-source-commit nil)
                   :comment comment)
                  (list :operation "fix-skipped-definitions"
                        :summary
                        (format nil "Rebuild or drop the ~D private definition~:P skipped at startup."
                                (length skips))))
           (error 'configuration-error
                  :message "The repair turn was not accepted: the session is stopping."))
         (application-present
          application
          (list (terminal-span
                 ':hint
                 (format nil "Queued ~D skipped definition~:[s~;~] for repair or removal."
                         (length skips)
                         (= (length skips) 1)))))))))
  nil)

(-> application--skipped-definitions-slash-arguments (string) list)
(defun application--skipped-definitions-slash-arguments (text)
  "Return the complete slash remainder TEXT as one optional direction string."
  (when (plusp (length text))
    (list text)))


;;;; -- Manual Compaction --

(-> application-compact (application) null)
(defun application-compact (application)
  "Compact the active conversation, or report an empty conversation without a request."
  (let ((agent (application-agent application))
        (conversation (application-conversation application)))
    (cond
      ((null (conversation-input-items conversation))
       (application-present application "Nothing to compact yet."))
      ((null agent)
       (error 'configuration-error
              :message "No connected agent can compact the conversation."))
      (t
       (unwind-protect
            (agent-compact-conversation
             agent
             (application-agent-observer application))
         (terminal-ui-set-compacting (application-ui application) nil)
         (application-set-activity application nil))
       (application-present
        application
        "Compacted history."))))
  nil)


;;;; -- Command Input --

(-> application--command-remainder (string) string)
(defun application--command-remainder (input)
  "Return INPUT's trimmed text after its slash-command word."
  (let ((space (position-if (lambda (character)
                              (find character '(#\Space #\Tab)))
                            input)))
    (if space
        (string-trim '(#\Space #\Tab) (subseq input space))
        "")))


;;;; -- Session Goal Command --

(-> application--goal-description (application) string)
(defun application--goal-description (application)
  "Return a one-line description of APPLICATION's session goal."
  (let ((goal (application-goal application)))
    (if goal
        (format nil "Goal ~(~A~)~@[ since ~A~]: ~A"
                (getf goal :status)
                (let ((created (getf goal :created-at)))
                  (and (integerp created)
                       (application--calendar-description created)))
                (getf goal :objective))
        "No session goal is set. Use (goal \"OBJECTIVE\") to set one.")))

(-> application--goal-update-argument (string) (option string))
(defun application--goal-update-argument (remainder)
  "Return the objective following /goal update, or NIL for other input."
  (let ((word (string-downcase remainder)))
    (and (uiop:string-prefix-p "update" word)
         (or (= (length word) (length "update"))
             (find (char word (length "update")) '(#\Space #\Tab)))
         (application--command-remainder remainder))))

(-> application--goal-update (application string &key (:literal-p boolean)) boolean)
(defun application--goal-update (application objective &key literal-p)
  "Rewrite the current session goal's objective to OBJECTIVE in place.

An active or paused goal keeps its status and creation time while its
continuation budget restarts. A completed goal becomes active again.
LITERAL-P permits command-shaped strings supplied as evaluated Lisp values.
Return true only when the objective was rewritten."
  (let ((goal (application-goal application)))
    (cond
      ((null goal)
       (application-present
        application
        "No session goal to update. Use (goal \"OBJECTIVE\") to set one.")
       nil)
      ((mission-goal-p goal)
       (application-present application "Use (mission :start SPECIFICATION) with an updated objective and acceptance policy.")
       nil)
      ((zerop (length objective))
       (application-present application "Usage: (goal :update \"NEW-OBJECTIVE\")")
       nil)
      ((and (not literal-p) (uiop:string-prefix-p "/" objective))
       (application-present
        application
        (format nil "~S looks like a command, not an objective. ~
                     Usage: (goal :update \"NEW-OBJECTIVE\")"
                objective))
       nil)
      (t
       (when (eq (getf goal :status) ':complete)
         (setf (getf (application-goal application) :status) ':active))
       (setf (getf (application-goal application) :objective)
             (copy-seq objective)
             (getf (application-goal application) :continuations) 0)
       (application--record-goal application)
       (application-present
        application
        (format nil
                "Goal updated: ~A~:[~;~%Use (goal :resume) to resume work.~]"
                objective
                (eq (getf (application-goal application) :status)
                    ':paused)))
       t))))

(-> application--start-goal-work (application) null)
(defun application--start-goal-work (application)
  "Begin working toward APPLICATION's active session goal immediately.

Goal turns run inside the /goal command's work item, so they wire the
input controller's steering hooks themselves: prose typed during goal
work must steer the running turn, not queue as follow-up work."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (setf (getf (application-goal application) :continuations) 0)
      (flet ((work (steering-function steering-persisted-function)
               (application--run-turn application
                                      *application-goal-continuation-prompt*
                                      :continuation-p t
                                      :steering-function steering-function
                                      :steering-persisted-function
                                      steering-persisted-function)
               (application--run-goal-continuations
                application
                :steering-function steering-function
                :steering-persisted-function steering-persisted-function)))
        (let ((controller (application-input-controller application)))
          (if controller
              (application-input-controller-call-with-primary-steering
               controller #'work)
              (work nil nil))))))
  nil)

(define-condition application-goal-error (autolith-error)
  ((reason
    :initarg :reason
    :reader application-goal-error-reason
    :type keyword
    :documentation "The stable reason the goal request was rejected.")
   (request
    :initarg :request
    :reader application-goal-error-request
    :type t
    :documentation "The objective or action supplied by the caller."))
  (:documentation "A public goal request has an invalid action or arguments."))

(-> application-goal-request (application t list) null)
(defun application-goal-request (application request arguments)
  "Set a literal string objective or apply a keyword action to APPLICATION.

NIL and :STATUS show the goal. :UPDATE takes one string objective; :PAUSE,
:RESUME, and :CLEAR take no arguments. Invalid requests signal APPLICATION-GOAL-ERROR."
  (unless (or (null request) (stringp request)
              (member request '(:status :update :pause :resume :clear)))
    (error 'application-goal-error
           :reason ':invalid-action :request request
           :message "Goal actions are :status, :update, :pause, :resume, and :clear."))
  (unless (if (eq request ':update)
              (and (= (length arguments) 1)
                   (non-empty-string-p (first arguments)))
              (null arguments))
    (error 'application-goal-error
           :reason ':invalid-arguments :request request
           :message "Only (goal :update OBJECTIVE) takes an additional argument."))
  (when (and (stringp request) (not (non-empty-string-p request)))
    (error 'application-goal-error
           :reason ':invalid-objective :request request
           :message "A goal objective must be a nonempty string."))
  (let ((goal (application-goal application)))
    (cond
      ((member request '(nil :status))
       (application-present application
                            (application--goal-description application)))
      ((eq request ':clear)
       (mission-detach application)
       (setf (application-goal application) nil)
       (application--record-goal application)
       (application-present application "The session goal was cleared."))
      ((eq request ':pause)
       (if (and goal (eq (getf goal :status) ':active))
           (progn
             (setf (getf (application-goal application) :status) ':paused)
             (application--record-goal application)
             (application-present application "The session goal is paused."))
           (application-present application "No active goal to pause.")))
      ((eq request ':resume)
       (if (and goal (eq (getf goal :status) ':paused))
           (progn
             (if (mission-goal-p goal)
                 (application-mission-resume application)
                 (progn
                   (setf (getf (application-goal application) :status) ':active
                         (getf (application-goal application) :continuations) 0)
                   (application--record-goal application)))
             (application-present application
                                  "The session goal is active again.")
             (application--start-goal-work application))
           (application-present application "No paused goal to resume.")))
      ((eq request ':update)
       (when (and (application--goal-update application (first arguments) :literal-p t)
                  (application-goal application)
                  (eq (getf (application-goal application) :status) ':active))
         (application--start-goal-work application)))
      (t
       (mission-detach application)
       (setf (application-goal application)
             (list :objective (copy-seq request)
                   :status ':active
                   :continuations 0
                   :created-at (get-universal-time)))
       (application--record-goal application)
       (application-present
        application
        (format nil
                "Goal set: ~A~%Autolith is starting work on it now. Use (goal) ~
                 to inspect it and (goal :clear) to stop."
                request))
       (application--start-goal-work application))))
  nil)

(-> application-goal--slash-arguments (string) list)
(defun application-goal--slash-arguments (text)
  "Parse goal controls and objective text only at the slash-command boundary."
  (let* ((remainder (string-trim *application-command-whitespace* text))
         (word (string-downcase remainder))
         (update (application--goal-update-argument remainder))
         (objective (or update remainder)))
    (when (uiop:string-prefix-p "/" objective)
      (error 'application-goal-error
             :reason ':invalid-objective :request objective
             :message "A slash-command goal objective must not begin with /."))
    (cond
      ((zerop (length remainder))
       nil)
      ((member word '("clear" "pause" "resume" "status") :test #'string=)
       (list (intern (string-upcase word) '#:keyword)))
      (update
       (list ':update update))
      (t
       (list remainder)))))

(-> application-goal-command (application string) null)
(defun application-goal-command (application remainder)
  "Apply a legacy slash goal request through the native goal interface."
  (handler-case
      (let ((arguments (application-goal--slash-arguments remainder)))
        (application-goal-request application (first arguments) (rest arguments)))
    (application-goal-error (condition)
      (application-present application (princ-to-string condition))))
  nil)

(-> application--generation-items (application) list)
(defun application--generation-items (application)
  "Return picker items for retained generations, newest first."
  (loop for generation in (generation-list
                           (application-configuration application))
        collect (list :name (generation-identifier generation)
                      :argument nil
                      :description
                      (format nil "~A~:[, incompatible~;~]"
                              (application--calendar-description
                               (generation-created-at generation))
                              (generation-compatible-p generation)))))

(-> application--pick-identifier
    (application &key (:title string) (:items list) (:usage string)
                 (:empty-notice string) (:hint (option string))
                 (:visible-count (integer 1))
                 (:initial-name (option string))
                 (:initial-value (option string))
                 (:search-p boolean) (:search-key function)
                 (:on-event (option function)))
    (option string))
(defun application--pick-identifier
    (application &key (title "select") items (usage "") (empty-notice "")
                      hint
                      (visible-count *terminal-ui-visible-completions*)
                      initial-name initial-value search-p
                      (search-key #'terminal-ui--picker-default-search-text)
                      on-event)
  "Pick one identifier from ITEMS interactively, or explain why none was picked.

Signals a usage error on non-interactive terminals, presents EMPTY-NOTICE
when ITEMS is empty, and returns NIL when the picker is cancelled. HINT,
VISIBLE-COUNT, INITIAL-NAME, INITIAL-VALUE, SEARCH-P, SEARCH-KEY, and ON-EVENT
are forwarded to TERMINAL-UI-SELECT."
  (block nil
    (let ((ui (application-ui application)))
      (unless (terminal-interactive-p (terminal-ui-terminal ui))
        (error 'configuration-error :message usage))
      (unless items
        (application-present application empty-notice)
        (return nil))
      (terminal-ui-select
       ui
       :title title
       :items items
       :hint hint
       :visible-count visible-count
       :initial-name initial-name
       :initial-value initial-value
       :search-p search-p
       :search-key search-key
       :on-event on-event
       :resize-callback #'application-pending-terminal-size))))

(-> application--conversation-delete-confirm-items (list) list)
(defun application--conversation-delete-confirm-items (item)
  "Return the delete/keep confirmation items for ITEM."
  (list
   (list :name "delete"
         :argument nil
         :description (format nil "permanently delete ~A" (getf item :name)))
   (list :name "keep"
         :argument nil
         :description "return to the conversation list")))

(-> application--pick-conversation (application) (option string))
(defun application--pick-conversation (application)
  "Pick a saved conversation, with d-delete and a confirmation step."
  (let* ((browse-title "resume conversation")
         (browse-hint "enter selects, d deletes, esc cancels")
         (confirm-hint "enter confirms, esc returns")
         (mode ':browse)
         (pending nil)
         (browse-items (application--conversation-items application))
         (current-identifier
           (conversation-identifier
            (application-conversation application))))
    (labels ((refresh-items ()
               "Reload browse items from disk."
               (setf browse-items
                     (application--conversation-items application)))

             (selected-item (selector)
               "Return SELECTOR's currently highlighted item, or NIL."
               (let ((selection (selector-selection selector))
                     (items (selector-items selector)))
                 (and selection
                      items
                      (<= 0 selection (1- (length items)))
                      (nth selection items))))

             (insert-character-p (event character)
               "Return true when EVENT inserts CHARACTER case-insensitively."
               (and (listp event)
                    (eq (first event) ':insert)
                    (stringp (second event))
                    (= (length (second event)) 1)
                    (char-equal (char (second event) 0) character)))

             (browse-action (&optional selected-value)
               "Return the picker action that restores the browse list."
               (if browse-items
                   (list ':replace browse-title browse-items browse-hint
                         selected-value)
                   (list ':cancel)))

             (on-event (event selector)
               "Handle delete confirmation inside the resume picker."
               (ecase mode
                 (:browse
                  (cond
                    ((insert-character-p event #\d)
                     (let ((item (selected-item selector)))
                       (cond
                         ((null item)
                          ':continue)
                         ((string= (getf item :value) current-identifier)
                          (application-present
                           application
                           "Cannot delete the active conversation.")
                          ':continue)
                         (t
                          (setf mode ':confirm
                                pending item)
                          (list
                           ':replace
                           (format nil "delete ~A?" (getf item :name))
                           (application--conversation-delete-confirm-items item)
                           confirm-hint)))))
                    (t
                     nil)))
                 (:confirm
                  (cond
                    ((eq event ':escape)
                     (let ((selected-value (and pending (getf pending :value))))
                       (setf mode ':browse
                             pending nil)
                       (browse-action selected-value)))
                    ((member event '(:interrupt :end-of-input :stream-end)
                             :test #'eq)
                     (list ':cancel))
                    ((eq event ':submit)
                     (let ((choice (selected-item selector))
                           (selected-value (and pending (getf pending :value))))
                       (cond
                         ((and choice
                               (string= (getf choice :name) "delete")
                               pending)
                          (handler-case
                              (progn
                                (conversation-delete
                                 (application-configuration application)
                                 (getf pending :value))
                                (application-present
                                 application
                                 (format nil "Deleted conversation ~A."
                                         (getf pending :name)))
                                (refresh-items))
                            (conversation-error (condition)
                              (application-present
                               application
                               (format nil "~A" condition))
                              (refresh-items)))
                          (setf mode ':browse
                                pending nil)
                          (browse-action selected-value))
                         (t
                          (setf mode ':browse
                                pending nil)
                          (browse-action selected-value)))))
                    ((and (listp event) (eq (first event) ':insert))
                     (let ((selected-value (and pending (getf pending :value))))
                       (setf mode ':browse
                             pending nil)
                       (browse-action selected-value)))
                    (t
                     nil))))))
      (application--pick-identifier
       application
       :title browse-title
       :items browse-items
       :hint browse-hint
       :initial-value current-identifier
       :on-event #'on-event
       :usage "Usage: (resume \"ID\")"
       :empty-notice "No saved conversations exist."))))

(-> application--pick-model (application &key (:provider (option string))) (values (option string) (option string)))
(defun application--pick-model (application &key provider)
  "Return the picked exact model and provider as two values, or two NILs."
  (let* ((configuration (application-configuration application))
         (registration (configuration-model-registration configuration))
         (items (application--model-items application :provider provider))
         (selection
           (application--pick-identifier
            application :title "pick the model" :items items :visible-count 15
            :initial-name (and registration
                               (provider-model-selection-id (config :model configuration) registration))
            :search-p t :usage "Usage: (model \"MODEL\" :provider \"PROVIDER\")"
            :empty-notice "No registered provider models exist."))
         (item (find selection items :key (lambda (item) (getf item :name)) :test #'equal)))
    (values (getf item :model) (getf item :provider))))

(-> application--pick-reasoning-effort (application) (option string))
(defun application--pick-reasoning-effort (application)
  "Prompt for one supported reasoning effort and return the selected name."
  (let ((configuration (application-configuration application)))
    (application--pick-identifier
     application
     :title "pick the reasoning effort"
     :items (application--effort-items application)
     :initial-name (config :reasoning-effort configuration)
     :search-p t
     :usage "Usage: (effort \"LEVEL\")"
     :empty-notice "No supported reasoning efforts exist.")))

(-> application--project-adaptation-offer-items () list)
(defun application--project-adaptation-offer-items ()
  "Return the AUTOLITH.org creation choices for an eligible resumed project."
  (list
   (list :name "create"
         :argument nil
         :description "create a documented project adaptation ledger")
   (list :name "not-now"
         :argument nil
         :description "ask again after five days")
   (list :name "never"
         :argument nil
         :description "never ask again for this repository or path")))

(-> application-maybe-offer-project-adaptation (application) null)
(defun application-maybe-offer-project-adaptation (application)
  "Offer voluntary AUTOLITH.org creation after a qualifying command-line resume."
  (let* ((configuration (application-configuration application))
         (ui (application-ui application))
         (project-root
           (workspace-project-root
            (config :working-directory configuration))))
    (when (terminal-interactive-p (terminal-ui-terminal ui))
      (handler-case
          (when (and (project-adaptation-offer-due-p
                      configuration project-root)
                     (project-adaptation-resume-qualifies-p
                      configuration
                      (application-conversation application)))
            ;; Persist the ordinary dismissal before opening the modal selector,
            ;; so Escape, Ctrl-C, or a lost terminal cannot cause resume-time nagging.
            (project-adaptation-offer-defer configuration project-root)
            (application-present
             application
             "This project has enough Autolith history to benefit from AUTOLITH.org, a voluntary ledger for project-specific adaptations.")
            (let ((choice
                    (application--pick-identifier
                     application
                     :title "create AUTOLITH.org?"
                     :items (application--project-adaptation-offer-items)
                     :usage "Choose create, not-now, or never."
                     :empty-notice "")))
              (cond
                ((and choice (string= choice "create"))
                 (handler-case
                     (let ((pathname
                             (project-adaptation-notes-create project-root)))
                       (application-present
                        application
                        (format nil "Created ~A" (namestring pathname))))
                   (project-adaptation-error (condition)
                     (project-adaptation--offer-retry
                      configuration project-root)
                     (error condition))))
                ((and choice (string= choice "never"))
                 (project-adaptation-offer-refuse configuration project-root)
                 (application-present
                  application
                  "AUTOLITH.org offers are disabled permanently for this path."))
                ((and choice (string= choice "not-now"))
                 (application-present
                  application
                  "The AUTOLITH.org offer is deferred for five days.")))))
        (project-adaptation-error (condition)
          (application-handle-expected-error application condition)))))
  nil)


;;;; -- Working Directory Command --

(-> application-working-directory-command (application (or pathname string null)) null)
(defun application-working-directory-command (application location)
  "Show APPLICATION's workspace, or change it to the native pathname LOCATION."
  (when (and (stringp location) (not (non-empty-string-p location)))
    (error 'configuration-error :message "A workspace pathname must not be empty."))
  (if location
      (let ((directory (application-set-working-directory application location)))
        (application-present
         application
         (format nil "Working directory is now ~A" (namestring directory))))
      (application-present
       application
       (format nil "Working directory: ~A"
               (namestring
                (config :working-directory
                        (application-configuration application))))))
  nil)


;;;; -- Authentication and Checkpoint Commands --

(-> application--authentication-provider-items (application) list)
(defun application--authentication-provider-items (application)
  "Return all registered providers, marking the selected provider."
  (let ((current-provider (configuration-model-provider-name
                           (application-configuration application))))
    (loop for registration in (provider-registrations)
          for name = (provider-registration-name registration)
          collect (list :name name
                        :argument nil
                        :description
                        (let ((description
                                (or (provider-registration-description registration)
                                    "registered provider")))
                          (if (and current-provider
                                   (string= name current-provider))
                              (format nil "~A (current provider)" description)
                              description))))))

(-> application--authentication-provider-visible-count (application) (integer 1))
(defun application--authentication-provider-visible-count (application)
  "Show up to twenty providers in fullscreen, otherwise use the inline limit."
  (if (terminal-ui-fullscreen-p (application-ui application))
      20
      *terminal-ui-visible-completions*))

(-> application--pick-authentication-provider (application) (option string))
(defun application--pick-authentication-provider (application)
  "Prompt for one registered provider to authenticate."
  (application--pick-identifier
   application :title "pick the provider to authenticate"
   :items (application--authentication-provider-items application)
   :visible-count (application--authentication-provider-visible-count application)
   :initial-name (configuration-model-provider-name (application-configuration application))
   :search-p t :usage "Usage: (auth \"PROVIDER\")"
   :empty-notice "No registered providers exist."))

(-> application--authentication-provider (application string) model-provider)
(defun application--authentication-provider (application provider-name)
  "Return APPLICATION's named provider for authentication."
  (provider-authentication-provider
   (application-configuration application)
   provider-name
   :reasoning-summaries-p (application-reasoning-traces-p application)))

(defclass application-authentication-output-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((terminal
    :initarg :terminal
    :reader application-authentication-output-stream-terminal
    :type terminal
    :documentation "The terminal receiving authentication output."))
  (:documentation "A character stream routing authentication output through a terminal."))

(defmethod trivial-gray-streams:stream-write-char
    ((stream application-authentication-output-stream) character)
  "Write CHARACTER through STREAM's terminal."
  (terminal--write
   (application-authentication-output-stream-terminal stream)
   (string character))
  character)

(defmethod trivial-gray-streams:stream-write-string
    ((stream application-authentication-output-stream) string
     &optional (start 0) end)
  "Write STRING's selected range through STREAM's terminal."
  (terminal--write
   (application-authentication-output-stream-terminal stream)
   (subseq string start end))
  string)

(defmethod trivial-gray-streams:stream-force-output
    ((stream application-authentication-output-stream))
  "Flush STREAM's terminal output."
  (terminal-flush
   (application-authentication-output-stream-terminal stream)))

(defmethod trivial-gray-streams:stream-finish-output
    ((stream application-authentication-output-stream))
  "Finish STREAM's terminal output."
  (terminal-flush
   (application-authentication-output-stream-terminal stream)))

(defclass application-authentication-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((terminal
    :initarg :terminal
    :reader application-authentication-input-stream-terminal
    :type localgroup-terminal
    :documentation "The attached terminal supplying hidden authentication input.")
   (pending
    :initform ""
    :accessor application-authentication-input-stream-pending
    :type string
    :documentation "The completed input line still being consumed.")
   (position
    :initform 0
    :accessor application-authentication-input-stream-position
    :type (integer 0)
    :documentation "The next character index in PENDING."))
  (:documentation "A hidden line stream consuming one localgroup controller's events."))

(-> application-authentication-input-stream--controller-p
    (application-authentication-input-stream)
    boolean)
(defun application-authentication-input-stream--controller-p (stream)
  "Return true while STREAM's localgroup terminal has a controller."
  (let ((terminal (application-authentication-input-stream-terminal stream)))
    (with-lock-held ((image-daemon:relay-lock terminal))
      (not (null (image-daemon:relay-controller terminal))))))

(-> application-authentication-input-stream--fill
    (application-authentication-input-stream)
    boolean)
(defun application-authentication-input-stream--fill (stream)
  "Read one hidden localgroup line into STREAM and report success."
  (let ((terminal (application-authentication-input-stream-terminal stream))
        (characters (make-array 64
                                :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (loop
      (let ((event (terminal-read-event terminal)))
        (cond
          ((and (consp event)
                (eq (first event) ':line)
                (stringp (second event)))
           (loop for character across (second event)
                 do (vector-push-extend character characters))
           (vector-push-extend #\Newline characters)
           (setf (application-authentication-input-stream-pending stream)
                 (coerce characters 'string)
                 (application-authentication-input-stream-position stream) 0)
           (return t))
           ((characterp event)
            (vector-push-extend event characters))
          ((and (consp event)
                (member (first event) '(:insert :paste) :test #'eq)
                (stringp (second event)))
           (loop for character across (second event)
                 do (vector-push-extend character characters)))
          ((eq event ':backspace)
           (when (plusp (fill-pointer characters))
             (decf (fill-pointer characters))))
          ((eq event ':submit)
           (vector-push-extend #\Newline characters)
           (setf (application-authentication-input-stream-pending stream)
                 (coerce characters 'string)
                 (application-authentication-input-stream-position stream) 0)
           (return t))
          ((member event '(:interrupt :escape :stream-end) :test #'eq)
           (return nil))
          ((eq event ':end-of-input)
           (unless (application-authentication-input-stream--controller-p stream)
             (return nil))
           (sleep 0.01)))))))

(defmethod trivial-gray-streams:stream-read-char
    ((stream application-authentication-input-stream))
  "Read the next hidden authentication character from STREAM."
  (loop
    for pending = (application-authentication-input-stream-pending stream)
    for position = (application-authentication-input-stream-position stream)
    when (< position (length pending))
      do (setf (application-authentication-input-stream-position stream)
               (1+ position))
         (return (char pending position))
    unless (application-authentication-input-stream--fill stream)
      do (return ':eof)))

(defmethod trivial-gray-streams:stream-listen
    ((stream application-authentication-input-stream))
  "Return true when STREAM has buffered or queued localgroup input."
  (or (< (application-authentication-input-stream-position stream)
         (length (application-authentication-input-stream-pending stream)))
      (terminal-input-ready-p
       (application-authentication-input-stream-terminal stream))))

(-> application--authentication-ui-suspend (terminal-ui) null)
(defun application--authentication-ui-suspend (ui)
  "Dismiss UI's live rows and suppress repaint without stopping transport."
  (with-terminal-ui-locked (ui)
    (setf (terminal-ui-live-output-suspended-p ui) t)
    (when (terminal-ui-started-p ui)
      (live-region-dismiss (terminal-ui-live-region ui))))
  nil)

(-> application--authentication-ui-resume (terminal-ui) null)
(defun application--authentication-ui-resume (ui)
  "Resume and repaint UI's live rows after authentication output."
  (with-terminal-ui-locked (ui)
    (setf (terminal-ui-live-output-suspended-p ui) nil)
    (when (terminal-ui-started-p ui)
      (terminal-ui--paint-live ui)))
  nil)

(-> application-call-with-authentication-ui (terminal-ui boolean function) t)
(defun application-call-with-authentication-ui (ui stop-ui-p function)
  "Call FUNCTION with provider-owned I/O, restoring the console on every exit."
  (if (and (terminal-ui-fullscreen-p ui)
           (fullscreen-terminal-ui-active-p ui))
      (let ((terminal (terminal-ui-terminal ui))
            (output-mode nil)
            (transport-stopped-p nil))
        (unwind-protect
             (progn
               (with-terminal-ui-locked (ui)
                 (setf (terminal-ui-live-output-suspended-p ui) t)
                 (terminal--write terminal (mouse-reporting-disable-sequence))
                 (when stop-ui-p
                   (setf transport-stopped-p t)
                   (terminal-stop terminal))
                 (setf output-mode (platform-terminal-enable-fullscreen *platform*))
                 (terminal-ui-fullscreen-invalidate ui)
                 (terminal-ui-boot-screen ui ':authenticating "Provider authentication in progress"))
               (funcall function))
          (with-terminal-ui-locked (ui)
            (unwind-protect
                 (platform-terminal-restore-fullscreen *platform* output-mode)
              (unwind-protect
                   (when transport-stopped-p
                     (terminal-start terminal))
                (terminal-ui-fullscreen-invalidate ui)
                (unwind-protect
                     (terminal--write terminal (mouse-reporting-enable-sequence))
                  (application--authentication-ui-resume ui)))))))
      (unwind-protect
           (progn
             (if stop-ui-p
                 (terminal-ui-stop ui)
                 (application--authentication-ui-suspend ui))
             (funcall function))
        (if stop-ui-p
            (terminal-ui-start ui)
            (application--authentication-ui-resume ui)))))

(-> terminal-authentication-streams
    (terminal)
    (values stream stream boolean boolean (option integer)))
(defgeneric terminal-authentication-streams (terminal)
  (:documentation
   "Return authentication input, output, UI-stop, echo, and descriptor state for TERMINAL."))

(defmethod terminal-authentication-streams ((terminal terminal))
  "Use the process standard streams for an application-provided TERMINAL."
  (declare (ignore terminal))
  (values *standard-input* *standard-output* t nil
          (api-key--input-file-descriptor *standard-input* nil)))

(defmethod terminal-authentication-streams ((terminal stream-terminal))
  "Use STREAM-TERMINAL's direct streams and input descriptor."
  (values (stream-terminal-input-stream terminal)
          (stream-terminal-output-stream terminal)
          t nil
          (stream-terminal-input-file-descriptor terminal)))

(defmethod terminal-authentication-streams ((terminal localgroup-terminal))
  "Adapt LOCALGROUP-TERMINAL events and output to authentication streams."
  (values
   (make-instance 'application-authentication-input-stream
                  :terminal terminal)
   (make-instance 'application-authentication-output-stream
                  :terminal terminal)
   nil t nil))

(-> application--authentication-streams
    (application)
    (values stream stream boolean boolean (option integer)))
(defun application--authentication-streams (application)
  "Return authentication streams and state selected by APPLICATION's terminal."
  (terminal-authentication-streams
   (terminal-ui-terminal (application-ui application))))

(-> application-authenticate
    (application string &optional t)
    null)
(defun application-authenticate (application provider-name &optional method)
  "Authenticate APPLICATION's explicitly named provider using optional METHOD."
  (let* ((ui       (application-ui application))
         (provider (application--authentication-provider application provider-name))
         (message  nil))
    (provider--authentication-method provider method)
    (application-present
     application
     (format nil "Authenticating provider ~A." provider-name))
    (multiple-value-bind
          (input output stop-ui-p input-echo-disabled-p input-file-descriptor)
        (application--authentication-streams application)
      (terminal-ui-call-with-exclusive-input
       ui
       (lambda ()
         (application-call-with-authentication-ui
          ui stop-ui-p
          (lambda ()
            (let ((*standard-input* input)
                  (*standard-output* output)
                  (*api-key-input-echo-disabled-p* input-echo-disabled-p)
                  (*api-key-input-file-descriptor* input-file-descriptor)
                  (*api-key-output-styled-p*
                    (terminal-styled-p (terminal-ui-terminal ui))))
              (setf message
                    (provider-authenticate-with-method
                     provider method :stream output :open-browser-p t))))))))
    (application-present application message)
    (let ((model (provider-authenticated-model
                  (application-configuration application) provider)))
      (when model
        (application-set-model application model
                               :provider (provider-registration-name
                                          (model-provider-registration provider)))
        (application-present
         application
         (format nil "Active provider: ~A; model: ~A."
                 (configuration-model-provider-name
                  (application-configuration application)) model)))))
  nil)

(-> application-checkpoint (application) null)
(defun application-checkpoint (application)
  "Begin a retained generation for APPLICATION."
  (application-set-activity application "checking source before checkpoint")
  (unwind-protect
       (let* ((configuration (application-configuration application))
              (backend
                (checkpoint-backend-create
                 configuration (application-worker application)
                 :tool-registry (application-tool-registry application)))
              (generation (checkpoint-create backend)))
         (if (platform-supports-p *platform* ':restartable-image-saver)
             (progn
               (setf (application-checkpoint-restart-request application)
                     (cons backend generation))
               (application-present
                application
                (format nil "Exact heap checkpoint ~A is scheduled. Autolith will restart after this command and resume this session."
                        (generation-identifier generation))))
             (application-present
              application
              (format nil "Checkpoint ~A is publishing in process ~D."
                      (generation-identifier generation)
                      (generation-coordinator-pid generation)))))
    (application-set-activity application nil))
  nil)


;;;; -- Command Permissions --

(-> application--permission-mode-name (keyword) string)
(defun application--permission-mode-name (mode)
  "Return a user-facing description of command permission MODE."
  (ecase mode
    (:ask "ask before unrecognized commands")
    (:auto "classify automatically without prompting")
    (:sandboxed "allow commands inside the workspace sandbox")
    (:full-access "let commands run with full user privileges")))

(-> application--permission-mode-items (application) list)
(defun application--permission-mode-items (application)
  "Return available session command permission choices for APPLICATION."
  (let ((current             (application-permission-mode application))
        (sandbox-available-p (application--command-sandbox-available-p)))
    (append
     (list
      (list :name "ask"
            :argument nil
            :description (if (eq current ':ask)
                             "current; prompt unless this exact command was saved"
                             "prompt unless this exact command was saved"))
      (list :name "auto"
            :argument nil
            :description (if (eq current ':auto)
                             "current; classify commands automatically without prompting"
                             "classify commands automatically without prompting")))
     (when sandbox-available-p
       (list
        (list :name "sandbox"
              :argument nil
              :description (if (eq current ':sandboxed)
                               "current; allow commands inside the workspace sandbox"
                               "allow commands inside the workspace sandbox"))))
     (list
      (list :name "full"
            :argument nil
            :description (if (eq current ':full-access)
                             "current; let it ride with full user privileges"
                             "let it ride with full user privileges"))))))

(-> application--saved-permissions-text (application) string)
(defun application--saved-permissions-text (application)
  "Return a readable list of APPLICATION's saved exact command approvals."
  (let ((rules (permission-state-rules
                (application-permission-state application))))
    (if rules
        (format nil "Saved exact command approvals:~%~{~A~^~%~}"
                (loop for rule in rules
                      collect
                      (format nil "  ~A~%    in ~A"
                              (command-permission-command rule)
                              (application--abbreviated-directory
                               (command-permission-directory rule)))))
        "No exact command approvals are saved.")))

(-> application-saved-permissions-entry (application) list)
(defun application-saved-permissions-entry (application)
  "Return saved approvals with request, domain, and pathname presentation roles."
  (let ((rules (permission-state-rules (application-permission-state application))))
    (if rules
        (append
         (list (terminal-span ':brand (format nil "Saved exact approvals~%")))
         (loop for rule in rules
               for kind = (command-permission-kind rule)
               append (application--entry-fields
                       (list (list (if (eq kind ':command) "command" "request")
                                   ':code (command-permission-command rule))
                             (list "kind" ':dim (string-downcase (symbol-name kind)))
                             (list "in" ':code
                                   (application--abbreviated-directory
                                    (command-permission-directory rule)))))))
        (list (terminal-span ':hint "No exact approvals are saved.")))))

(-> application--set-durable-permission-mode (application keyword) null)
(defun application--set-durable-permission-mode (application mode)
  "Set APPLICATION's session permission MODE and persist ask or auto."
  (setf (application-permission-mode application) mode)
  (when (member mode '(:ask :auto) :test #'eq)
    (setf (config :permission-mode (application-configuration application)) mode))
  nil)

(-> application--permission-mode-entry (keyword string) list)
(defun application--permission-mode-entry (mode description)
  "Return a permission MODE receipt, emphasizing full user authority as a warning."
  (list (terminal-span (if (eq mode ':full-access) ':notice ':success) "Permissions ")
        (terminal-span ':code (string-downcase (symbol-name mode)))
        (terminal-span ':dim (format nil "~%  ~A" description))))

(-> application-permissions-command (application t) null)
(defun application-permissions-command (application argument)
  "Apply ARGUMENT to APPLICATION's session command permissions."
  (let ((choice (and argument
                     (application--control-choice argument '(:ask :auto :sandbox :full :list :clear)))))
    (cond
      ((null choice)
       nil)
      ((eq choice ':ask)
       (application--set-durable-permission-mode application ':ask)
       (application-present
        application
        (application--permission-mode-entry
         ':ask "Commands will ask before running unless the exact command was saved.")))
      ((eq choice ':auto)
       (application--set-durable-permission-mode application ':auto)
       (application-present
        application
        (application--permission-mode-entry
         ':auto
         (if (application--command-sandbox-available-p)
             "Commands will be classified automatically as sandboxed, full access, or denied."
             "Commands will be classified automatically as full access or denied."))))
      ((eq choice ':sandbox)
       (unless (application--command-sandbox-available-p)
         (error 'configuration-error
                :message (application--command-sandbox-unavailable-message)))
       (setf (application-permission-mode application) ':sandboxed)
       (application-present
        application
        (application--permission-mode-entry
         ':sandboxed "Commands may run for this session inside the workspace sandbox.")))
      ((eq choice ':full)
       (setf (application-permission-mode application) ':full-access)
       (application-present
        application
        (application--permission-mode-entry
         ':full-access "Commands may run for this session with your full user privileges.")))
      ((eq choice ':list)
       (application-present application (application-saved-permissions-entry application)))
      ((eq choice ':clear)
       (permissions-clear
        (application-configuration application)
        (application-permission-state application))
       (application-present application (list (terminal-span ':success "Saved command approvals were cleared."))))
      (t
       (error 'configuration-error
              :message "Usage: (permissions), or (permissions :MODE) with :ask, :auto, :sandbox, :full, :list, or :clear."))))
  nil)

(-> application-set-hurry-up (application boolean) null)
(defun application-set-hurry-up (application enabled-p)
  "Apply ENABLED-P to APPLICATION's prompt and child-agent runtime."
  (setf (application-hurry-up-p application) enabled-p)
  (let ((agent (and (slot-boundp application 'agent)
                    (application-agent application))))
    (when (typep agent 'agent)
      (setf (agent-hurry-up-p agent) enabled-p)))
  (let ((orchestrator (application--task-orchestrator application)))
    (when orchestrator
      (task-orchestrator-set-hurry-up orchestrator enabled-p)))
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (terminal-ui-set-notice
       ui
       (if enabled-p
           "Time is of the essence. Acting directly with at most two child agents."
           "Hurry-up mode is off.")
       :duration-seconds (if enabled-p 8 3))))
  nil)

(-> application-hurry-up-command (application &optional t) null)
(defun application-hurry-up-command (application &optional (argument ':status))
  "Show or change APPLICATION's session-scoped hurry-up mode."
  (let ((mode (application--boolean-control argument)))
    (cond
      ((eq mode ':status)
       (application-present
        application
        (format nil "Hurry-up mode is ~:[disabled~;enabled~]."
                (application-hurry-up-p application))))
      ((eq mode ':on)
       (application-set-hurry-up application t))
      ((eq mode ':off)
       (application-set-hurry-up application nil))
      (t
       (error 'configuration-error
              :message "Usage: (hurry-up t) or (hurry-up nil)."))))
  nil)

;;;; -- Settings Page --

(-> application--setting-name (t) keyword)
(defun application--setting-name (designator)
  "Return the setting keyword named by a string or symbol DESIGNATOR."
  (unless (typep designator '(or string symbol))
    (error 'configuration-error
           :message (format nil "A setting name must be a string or symbol, not ~S." designator)))
  (intern (string-upcase (string designator)) '#:keyword))

(-> application--setting-adjustable-p (setting) boolean)
(defun application--setting-adjustable-p (setting)
  "Return true when SETTING may change while the process runs."
  (and (member (setting-scope setting) '(:durable :session)) t))

(-> application--setting-source-text (configuration setting) string)
(defun application--setting-source-text (configuration setting)
  "Return how SETTING's current value in CONFIGURATION was chosen."
  (case (setting-scope setting)
    (:derived
     "derived")
    (t
     (case (configuration-setting-source configuration (setting-name setting))
       (:environment "environment")
       (:override "explicit")
       (:durable "saved")
       (:session "this session")
       (t "default")))))

(-> application-apply-setting (application keyword t) null)
(defun application-apply-setting (application name value)
  "Set NAME to VALUE for APPLICATION and apply the runtime side effects.

Settings with runtime consequences, such as the provider's reasoning
summaries or the transcript pagination, go through their dedicated setters so
the settings page and the slash commands behave identically."
  (let* ((configuration (if (member name '(:telemetry-enabled-p :telemetry-diagnostics-p))
                            (telemetry-consent-configuration (application-configuration application))
                            (application-configuration application)))
         (setting (configuration-setting configuration name)))
    (setting-validate setting value configuration)
    (unless (application--setting-adjustable-p setting)
      (error 'configuration-error
             :message (format nil "~A is set at startup by the command line, the environment, or a default and cannot change in a running session."
                              (setting-label setting))))
    (case name
      (:model (application-set-model application value))
      (:reasoning-effort (application-set-reasoning-effort application value))
      (:codex-fast-mode-p (application-set-codex-fast-mode application value))
      (:reasoning-traces-p (application-set-reasoning-traces application value))
      (:turn-timestamps-p (application-set-turn-timestamps application value))
      (:cache-miss-notices-p (application-set-cache-miss-notices application value))
      ((:idle-compaction-p :prompt-cache-retention-seconds)
       (setf (config name configuration) value)
       (application-idle-compaction-wake application))
      (:compact-view-p
       (unless (eq (application-compact-view-p application) value)
         (setf (application-compact-view-p application) value)
         (application-reset-history-pagination application))
       (application-publish-recovery-session application))
      ((:knowledge-mode :diagnosis-p)
       (setf (config name configuration) value)
       ;; The model's resource operations and documentation depend on the
       ;; mode, so the session's tools are rebuilt for the next request.
       (application-reload-mcp application))
      (:hurry-up-p (application-set-hurry-up application value))
      (:relax-p
       (application-set-relax application value))
      (:permission-mode
       (if value
           (application--set-durable-permission-mode application value)
           (setf (config :permission-mode configuration) nil)))
      (t
       (setf (config name configuration) value))))
  nil)

(-> application--grouped-settings (configuration) list)
(defun application--grouped-settings (configuration)
  "Return CONFIGURATION's visible settings with each group's settings together.

Groups keep the order of their first definition and settings their definition
order within a group, so every group heading appears once."
  (let ((visible (remove-if-not #'setting-visible-p
                                (configuration-setting-list configuration)))
        (groups nil))
    (dolist (setting visible)
      (pushnew (setting-group setting) groups))
    (loop for group in (reverse groups)
          append (remove-if-not (lambda (setting)
                                  (eq (setting-group setting) group))
                                visible))))

(-> application--setting-items (application) list)
(defun application--setting-items (application)
  "Return picker items for every visible setting under its group heading.

The current value and its source sit in aligned columns of their own."
  (let ((configuration (application-configuration application)))
    (loop for setting in (application--grouped-settings configuration)
          collect (list :name (setting-label setting)
                        :value (string-downcase (symbol-name (setting-name setting)))
                        :argument nil
                        :group (string-downcase (symbol-name (setting-group setting)))
                        :detail (setting-render-value
                                 setting
                                 (configuration-setting-value configuration setting))
                        :description
                        (format nil "~A~:[ · read-only~;~]"
                                (application--setting-source-text configuration setting)
                                (application--setting-adjustable-p setting))))))

(-> application--setting-option-items (application setting) list)
(defun application--setting-option-items (application setting)
  "Return picker items for SETTING's options, marking the current value."
  (let* ((configuration (application-configuration application))
         (current (configuration-setting-value configuration setting)))
    (loop for option in (setting-options setting configuration)
          for text = (setting-render-value setting option)
          collect (list :name text
                        :argument nil
                        :description (if (equal option current) "current" "")))))

(-> application-settings-listing (application) string)
(defun application-settings-listing (application)
  "Return every visible setting with its value, source, and meaning as text."
  (let ((configuration (application-configuration application))
        (group nil))
    (with-output-to-string (out)
      (dolist (setting (application--grouped-settings configuration))
        (unless (eq group (setting-group setting))
          (setf group (setting-group setting))
          (format out "~:[~;~%~]~(~A~)~%" (plusp (file-position out)) group))
        (format out "  ~(~A~) = ~A (~A)~%    ~A~%"
                (setting-name setting)
                (setting-render-value
                 setting (configuration-setting-value configuration setting))
                (application--setting-source-text configuration setting)
                (setting-documentation setting))))))

(-> application--settings-page (application) null)
(defun application--settings-page (application)
  "Let the user browse settings and change adjustable ones until they cancel."
  (loop
    (let ((chosen (application--pick-identifier
                   application
                   :title "settings"
                   :items (application--setting-items application)
                   :visible-count 15
                   :search-p t
                   :usage "Usage: (settings), (settings :NAME), or (settings :NAME VALUE)"
                   :empty-notice "No settings are registered.")))
      (unless chosen
        (return))
      (let* ((name (application--setting-name chosen))
             (setting (configuration-setting (application-configuration application) name)))
        (cond
          ((not (application--setting-adjustable-p setting))
           (application-present
            application
            (format nil "~A is ~A (~A). It is read-only in a running session."
                    (setting-label setting)
                    (setting-render-value
                     setting (config name (application-configuration application)))
                    (application--setting-source-text
                     (application-configuration application) setting))))
          ((setting-options setting (application-configuration application))
           (let ((option (application--pick-identifier
                          application
                          :title (format nil "~A" (setting-label setting))
                          :items (application--setting-option-items application setting)
                          :initial-name (setting-render-value
                                         setting
                                         (config name (application-configuration application)))
                          :search-p t
                          :usage "Usage: (settings :NAME VALUE)"
                          :empty-notice "This setting offers no choices.")))
             (when option
               (application-apply-setting
                application name
                (setting-parse setting option (application-configuration application)))
               (application-present
                application
                (format nil "~A is now ~A." (setting-label setting) option)))))
          (t
           (application-present
            application
            (format nil "~A is ~A. Change it with (settings :~(~A~) VALUE)."
                    (setting-label setting)
                    (setting-render-value
                     setting (config name (application-configuration application)))
                    name)))))))
  nil)

(-> application--parse-settings-control (string) list)
(defun application--parse-settings-control (text)
  "Retain slash settings text for parsing with the executing configuration."
  (unless (zerop (length (string-trim '(#\Space #\Tab) text)))
    (list text)))

(-> application--settings-text-command (application (option string)) null)
(defun application--settings-text-command (application text)
  "Parse slash settings once using APPLICATION's schema and quoted token rules."
  (let ((arguments (and text (application-command--tokens text))))
    (unless (<= (length arguments) 2)
      (error 'configuration-error :message "Usage: (settings), (settings :NAME), or (settings :NAME VALUE)."))
    (destructuring-bind (&optional name (value nil value-supplied-p)) arguments
      (if value-supplied-p
          (let* ((configuration (application-configuration application))
                 (setting (configuration-setting
                           configuration (application--setting-name name))))
            (application-settings-command
             application name (setting-parse setting value configuration)))
          (application-settings-command application name))))
  nil)

(-> application-settings-command (application &optional t t) null)
(defun application-settings-command (application &optional name (value nil value-supplied-p))
  "Browse settings, show one, or set one, depending on the arguments."
  (when (and value-supplied-p (null name))
    (error 'configuration-error :message "Provide a setting name when changing a value."))
  (cond
    ((and (null name) *application-command-interactive-p*)
     (application--settings-page application))
    ((null name)
     (application-present application (application-settings-listing application)))
    ((not value-supplied-p)
     (let* ((configuration (application-configuration application))
            (key (application--setting-name name))
            (setting (configuration-setting configuration key)))
       (application-present
        application
        (format nil "~A is ~A (~A). ~A"
                (setting-label setting)
                (setting-render-value setting (config key configuration))
                (application--setting-source-text configuration setting)
                (setting-documentation setting)))))
    (t
     (let* ((key (application--setting-name name))
            (setting (configuration-setting (application-configuration application) key)))
       (application-apply-setting application key value)
       (application-present
        application
        (format nil "~A is now ~A~:[~; and saved~]."
                (setting-label setting)
                (setting-render-value setting (config key (application-configuration application)))
                (eq (setting-scope setting) ':durable))))))
  nil)

(define-application-command application--builtin-settings-command
    (:name "/settings"
     :argument "[NAME [VALUE]]"
     :description "browse and change settings"
     :tip "Change native values with `(settings :onboarding-p nil)` or `(settings :boot-screen-seconds 2)`; inspect one setting with `(settings :terminal-theme)`."
     :busy-behavior :apply
     :terminal-behavior :exclusive-without-arguments
     :callable t
     :slash-parser application--parse-settings-control)
    (application &optional name (value nil value-supplied-p))
  (let ((invocation *application-command-presentation-invocation*))
    (cond
      ((and invocation
            (eq (application-command-invocation-syntax invocation) ':slash))
       (application--settings-text-command application name))
      (value-supplied-p
       (application-settings-command application name value))
      (t
       (application-settings-command application name))))
  ':continue)

(-> application-slynk-command (application &optional t) null)
(defun application-slynk-command (application &optional (action ':start))
  "Start, stop, or show APPLICATION's Slynk endpoint using a keyword ACTION."
  (application--control-choice action '(:start :stop :status))
  (let ((configuration (application-configuration application)))
    (cond
      ((eq action ':stop)
       (slynk-stop)
       (application-present application "Slynk stopped. sly connections to this image are closed."))
      ((and (eq action ':start) (null (config :slynk-directory configuration)))
       (application-present
        application
        "Slynk is unavailable: set AUTOLITH_SLYNK_DIRECTORY to your sly installation's slynk/ directory, or start Autolith from M-x autolith-ide."))
      (t
       (when (eq action ':start)
         (slynk-start configuration))
       (destructuring-bind (&key state port message &allow-other-keys) (slynk-status)
         (application-present
          application
          (if (eq state ':running)
              (format nil "Slynk serves this image on ~A:~D. In Emacs: M-x sly-connect RET ~A RET ~D."
                      *slynk-interface* port *slynk-interface* port)
              (format nil "Slynk is ~(~A~)~@[: ~A~]." state message)))))))
  nil)

(define-application-command application--builtin-slynk-command
    (:name "/slynk"
     :description "serve this image to sly"
     :tip "Use `(slynk)` to start Slynk and show its port for M-x sly-connect, or `(slynk :stop)` to close it."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-slynk-control
     :static-options (:start :stop :status))
    (application &optional (action ':start))
  (application-slynk-command application action)
  ':continue)

;;;; -- Installed Release Update --

(-> application-update (application) null)
(defun application-update (application)
  "Check for and request an attended packaged release update for APPLICATION."
  (let* ((configuration (application-configuration application))
         (provenance
           (or (application-installation-provenance application)
               (installation-provenance-detect configuration)))
         (method (installation-provenance-method provenance)))
    (ecase method
      (:source
       (application-present
        application
        "Autolith is running from source. Update the checkout and run ./script/bootstrap."))
      (:nix
       (application-present
        application
        "Autolith is installed through Nix. Update the flake or profile that provides it."))
      (:release
       (if (update-state-refresh configuration :force-p t)
           (let* ((state (update-state-load configuration))
                  (latest-tag (update-state-latest-tag state))
                  (current-tag
                    (installation-provenance-current-tag provenance)))
             (if (and current-tag
                      latest-tag
                      (release-tag< current-tag latest-tag))
                 (error 'update-requested
                        :message
                        (format nil "Update to Autolith ~A."
                                (subseq latest-tag 1))
                        :tag latest-tag)
                 (progn
                   (setf (application-update-availability application) nil)
                   (application-present
                    application
                    (format nil
                            "Autolith ~A is already the newest published release."
                            *autolith-version*)))))
           (application-present
            application
            "Autolith could not check the release service. The installed release is unchanged.")))))
  nil)


;;;; -- Built-in Interactive Commands --

(define-application-command application--builtin-help-command
    (:name "/help"
     :argument "[TOPIC]"
     :description "browse help or look up a command"
     :tip "Use `(help :workspace)` for a section or `(help 'resume)` for a command."
     :busy-behavior :execute
     :terminal-behavior :shared
     :callable t)
    (application &optional topic)
  (application-present
   application
   (application--markdown-body application (application-operation-help application topic)))
  ':continue)

(define-application-command application--builtin-new-command
    (:name "/new"
     :argument nil
     :description "start a new conversation"
     :tip "starts fresh without deleting the current conversation."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-install-conversation
   application
   (conversation-create (application-configuration application)))
  (let ((ui (application-ui application)))
    (when ui
      (terminal-ui-reset-transcript ui)))
  (application-present
   application
   (format nil "Started conversation ~A."
           (conversation-identifier-display
            (conversation-identifier
             (application-conversation application)))))
  ':continue)

(define-application-command application--builtin-resume-command
    (:name "/resume"
     :argument "[ID]"
     :description "pick a saved conversation to resume"
     :tip "returns to a saved conversation from this workspace or another one."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments
     :callable t)
    (application &optional (identifier nil identifier-supplied-p))
  (let* ((picker-p
           (and *application-command-interactive-p*
                (not identifier-supplied-p)))
         (startup-offer-p
           (application-project-adaptation-offer-p application))
         (identifier
           (if identifier-supplied-p
               identifier
               (and picker-p (application--pick-conversation application)))))
    (when (or identifier picker-p)
      (setf (application-project-adaptation-offer-p application) nil))
    (when identifier
      (application-resume-conversation application identifier)
      (application-render-records application)
      (when startup-offer-p
        (application-maybe-offer-project-adaptation application))))
  ':continue)

(define-application-command application--builtin-conversations-command
    (:name "/conversations"
     :argument nil
     :description "list saved conversations"
     :tip "lists saved conversations from newest to oldest."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application
                       (application-conversations-entry application))
  ':continue)

(define-application-command application--builtin-history-command
    (:name "/history"
     :argument nil
     :description "load earlier transcript history"
     :tip "loads one configured page of earlier transcript entries."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-render-history application)
  ':continue)

(define-application-command application--builtin-working-directory-command
    (:name "/cwd"
     :argument "[PATH]"
     :description "show or change the active workspace"
     :tip "Use `(cwd pathname)` to change the active workspace, or `(cwd)` to inspect it."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application &optional pathname)
  (application-working-directory-command application pathname)
  ':continue)

(-> application-authentication--slash-arguments (string) list)
(defun application-authentication--slash-arguments (text)
  "Parse a provider identifier and optional textual authentication method."
  (let ((arguments (application-command--tokens text)))
    (when (> (length arguments) 2)
      (error 'authentication-error
             :message "Auth accepts a provider and optional method."))
    (if (rest arguments)
        (list (first arguments)
              (provider--parse-authentication-method (second arguments)))
        arguments)))

(define-application-command application--builtin-authentication-command
    (:name "/auth"
     :argument "[PROVIDER] [METHOD]"
     :description "pick and authenticate a registered provider"
     :tip "Use `(auth \"chatgpt\" :device-code)` for device authentication, or `:browser`."
     :slash-parser application-authentication--slash-arguments
     :busy-behavior :hold
     :terminal-behavior :exclusive
     :callable t)
    (application &optional (provider-name nil provider-name-supplied-p) method)
  (let ((provider-name
          (if provider-name-supplied-p
              provider-name
              (and *application-command-interactive-p*
                   (application--pick-authentication-provider application)))))
    (when provider-name
      (application-authenticate application provider-name method)))
  ':continue)

(define-application-command application--builtin-model-command
    (:name "/model"
     :argument "[MODEL] [:provider PROVIDER]"
     :description "pick a registered provider model and reasoning effort"
     :tip "Select a provider/model ID, or use (model \"ID\" :provider \"NAME\"). The choice is saved for future sessions."
     :busy-behavior :apply
     :terminal-behavior :exclusive
     :callable t)
    (application &optional (model nil model-supplied-p) &key provider)
  (multiple-value-bind (selected selected-provider)
      (cond
        (model
         (values model provider))
        ((and model-supplied-p (null provider))
         (values nil nil))
        (*application-command-interactive-p*
         (application--pick-model application :provider provider)))
    (when selected
      (application-set-model application selected :provider selected-provider)
      (let ((effort (and *application-command-interactive-p*
                         (application--pick-reasoning-effort application))))
        (when effort (application-set-reasoning-effort application effort))
        (application-present
         application
         (format nil "The model is now ~A with reasoning effort ~A."
                 (configuration-model-selection-id (application-configuration application))
                 (config :reasoning-effort (application-configuration application)))))))
  ':continue)

(define-application-command application--builtin-models-command
    (:name "/models"
     :argument "[:provider PROVIDER]"
     :description "list registered providers and models"
     :tip "Use (models :provider \"NAME\") to list one provider's models."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application &key provider)
  (application-models-command application :provider provider)
  ':continue)

(define-application-command application--builtin-effort-command
    (:name "/effort"
     :argument "[LEVEL]"
     :description "pick the reasoning effort"
     :tip "sets the model's reasoning budget without changing models. Higher levels allow more reasoning and can take longer; available levels depend on the model."
     :busy-behavior :apply
     :terminal-behavior :exclusive-without-arguments
     :callable t)
    (application &optional (effort nil effort-supplied-p))
  (let ((effort
          (if effort-supplied-p
              effort
              (and *application-command-interactive-p*
                   (application--pick-reasoning-effort application)))))
    (when effort
      (application-set-reasoning-effort application effort)
      (application-present
       application
       (format nil "Reasoning effort is now ~A."
               (config :reasoning-effort
                (application-configuration application))))))
  ':continue)

(define-application-command application--builtin-fast-command
    (:name "/fast"
     :description "show or change Codex Fast mode"
     :tip "Use `(fast t)` to request priority service, which counts more heavily against plan usage; use `(fast nil)` for normal service."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil :status))
    (application &optional (mode ':status))
  (application-codex-fast-mode-command application mode)
  ':continue)

(define-application-command application--builtin-trace-command
    (:name "/trace"
     :description "show visible reasoning summaries"
     :tip "Use `(trace t)` to show reasoning summaries or `(trace nil)` to hide them. Change the reasoning budget with `(effort)`."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-trace-command application mode)
  ':continue)

(define-application-command application--builtin-turn-timestamps-command
    (:name "/timestamps"
     :description "show local timestamps beside user and assistant turns"
     :tip "Use `(timestamps t)` to show local-time labels beside user and assistant turns, or `(timestamps nil)` to hide them."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-turn-timestamps-command application mode)
  ':continue)

(define-application-command application--builtin-simple-technical-english-command
    (:name "/ste"
     :description "use Simple Technical English for replies"
     :tip "Use `(ste t)` to request short, direct Simple Technical English, or `(ste nil)` to remove that style instruction."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-simple-technical-english-command application mode)
  ':continue)

(define-application-command application--builtin-session-titles-command
    (:name "/titles"
     :description "allow provider-generated session-title refreshes"
     :tip "Use `(titles t)` to allow provider-generated title refreshes, or `(titles nil)` for locally derived titles."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-session-titles-command application mode)
  ':continue)

(define-application-command application--builtin-cache-misses-command
    (:name "/cache-misses"
     :description "notify when a request re-reads uncached context"
     :tip "Use `(cache-misses t)` to report uncached context and likely causes, or `(cache-misses nil)` to hide these notices."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-cache-miss-notices-command application mode)
  ':continue)

(define-application-command application--builtin-hurry-up-command
    (:name "/hurry-up"
     :description "show or change hurry-up mode"
     :tip "Use `(hurry-up t)` to favor direct work with at most two child agents, or `(hurry-up nil)` for normal delegation limits."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-hurry-up-command application mode)
  ':continue)

(define-application-command application--builtin-permissions-command
    (:name "/permissions"
     :description "choose command access for this session"
     :tip "Use `(permissions :ask)` for approval prompts, `:auto` for automatic authorization, `:sandbox` for workspace confinement, or `:full` for session-wide host access."
     :busy-behavior :apply
     :terminal-behavior :exclusive-without-arguments
     :callable t
     :slash-parser application--parse-permissions-control
     :static-options (:ask :auto :sandbox :full :list :clear))
    (application &optional (choice nil choice-supplied-p))
  (when choice-supplied-p
    (application--control-choice choice '(:ask :auto :sandbox :full :list :clear)))
  (let ((choice
          (if choice-supplied-p
              choice
              (and *application-command-interactive-p*
                   (application--pick-identifier
                    application
                    :title "command permissions"
                    :items (application--permission-mode-items application)
                    :usage "Usage: (permissions), or (permissions :MODE) with :ask, :auto, :sandbox, :full, :list, or :clear"
                    :empty-notice "No command permission modes exist.")))))
    (application-permissions-command
     application
     (if (and (not choice-supplied-p) choice)
         (first (application--parse-permissions-control choice))
         choice)))
  ':continue)

(define-application-command application--builtin-tasks-command
    (:name "/tasks"
     :argument "[list|get|transcript|send|cancel|revive|abandon] [ID] [ARGUMENTS]"
     :description "inspect task lineage, artifacts and explicit recovery"
     :tip "shows compact activity and routes steering through ordinary job authority."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :slash-parser application-task-inspector-slash-arguments
     :callable t)
    (application &optional (action ':list) id &rest options)
  (application-present application (apply #'application-task-inspector application action id options))
  ':continue)

(define-application-command application--builtin-goal-command
    (:name "/goal"
     :argument "[OBJECTIVE]"
     :description "set or view the session goal"
     :tip "Set a string objective; use `(goal :pause)`, `(goal :resume)`, or `(goal :clear)` for control."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t
     :slash-parser application-goal--slash-arguments)
    (application &optional request &rest arguments)
  (application-goal-request application request arguments)
  ':continue)

(define-application-command application--builtin-agenda-command
    (:name "/agenda"
     :argument nil
     :description "show workspace agenda entries"
     :tip "shows durable commitments and notes for the current workspace."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-agenda-entry application))
  ':continue)

(define-application-command application--builtin-papercuts-command
    (:name "/papercuts"
     :argument nil
     :description "show workspace papercut reports"
     :tip "shows problems Autolith recorded when something was not working."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-papercuts-entry application))
  ':continue)

(define-application-command application--builtin-papercut-command
    (:name "/papercut"
     :argument "[ID]"
     :description "show one complete papercut report"
     :tip "opens the full report named by `(papercuts)`."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application &optional identifier)
  (if identifier
      (application-present
       application
       (application-papercut-entry application identifier))
      (application-present
       application
       "Usage: (papercut \"ID\"). Run (papercuts) to list reports."))
  ':continue)

(define-application-command application--builtin-papercut-close-command
    (:name "/papercut-close"
     :argument "[ID]"
     :description "close one resolved papercut report"
     :tip "removes a fixed or obsolete report from `(papercuts)`."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t)
    (application &optional identifier)
  (if identifier
      (application-present
       application
       (application-papercut-close-entry application identifier))
      (application-present
       application
       "Usage: (papercut-close \"ID\"). Run (papercuts) to list reports."))
  ':continue)

(define-application-command application--builtin-skills-command
    (:name "/skills"
     :argument nil
     :description "show available skills"
     :tip "shows request-local skills and any discovery problems."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present
   application
   (skill-status (application-configuration application)))
  ':continue)

(-> application--tool-capability-change-entry (list list) (option list))
(defun application--tool-capability-change-entry (added-tools removed-tools)
  "Return a concise transcript entry for a changed runtime tool-name set."
  (when (or added-tools removed-tools)
    (list
     (terminal-span ':hint "∙ tools added: ")
     (terminal-span (if added-tools ':success ':dim)
                    (if added-tools
                        (format nil "~{~A~^, ~}" added-tools)
                        "none"))
     (terminal-span ':hint "; removed: ")
     (terminal-span (if removed-tools ':failure ':dim)
                    (if removed-tools
                        (format nil "~{~A~^, ~}" removed-tools)
                        "none")))))

(-> application-mcp-status-entry (list) list)
(defun application-mcp-status-entry (records)
  "Return detached MCP status RECORDS as aligned semantic server fields."
  (if (null records)
      (list (terminal-span ':hint "No MCP servers are configured."))
      (loop for record in records
            for state = (getf record :state)
            for failure = (getf record :failure)
            append
            (append
             (list (terminal-span ':strong (format nil "~A~%" (getf record :name))))
             (application--entry-fields
              (list (list "state"
                          (cond
                            ((or failure (member state '(:failed :error)))
                             ':failure)
                            ((eq state ':ready)
                             ':success)
                            (t
                             ':notice))
                          (string-downcase (princ-to-string state)))
                    (list "policy" (if (getf record :required-p) ':notice ':dim)
                          (if (getf record :required-p) "required" "optional"))
                    (list "source" ':dim (getf record :source))
                    (list "transport" ':code (getf record :transport))
                    (list "tools" ':plain (getf record :tool-count))))
             (when (plusp (or (getf record :task-required-tool-count) 0))
               (application--entry-fields
                (list (list "unavailable" ':notice
                            (format nil "~D task-required tools: ~A"
                                    (getf record :task-required-tool-count)
                                    (getf record :task-required-tool-reason))))))
             (when failure
               (application--entry-fields (list (list "failure" ':failure failure))))))))

(-> application-generations-entry (configuration) list)
(defun application-generations-entry (configuration)
  "Return retained generation compatibility and reconstruction metadata for the UI."
  (let ((generations (generation-list configuration)))
    (if (null generations)
        (list (terminal-span ':hint "No retained generations exist."))
        (loop for generation in generations
              for compatible-p = (generation-compatible-p generation)
              for history = (generation-mutation-history-commit generation)
              for replay = (generation-reconstruction-pathname generation)
              append
              (append
               (list (terminal-span ':code (generation-identifier generation))
                     (terminal-span (if compatible-p ':success ':failure)
                                    (format nil "  ~A~%"
                                            (if compatible-p "compatible" "incompatible"))))
               (application--entry-fields
                (list (list "source" ':code (generation-git-commit generation))
                      (list "image" ':code (or (generation-image-commit-identifier generation) "base"))
                      (list "history" (if history ':code ':notice) (or history "unavailable"))
                      (list "replay" (if replay ':code ':notice)
                            (if replay (namestring replay) "unavailable for legacy generation")))))))))

(define-application-command application--builtin-mcp-command
    (:name "/mcp"
     :description "show or refresh configured MCP servers"
     :tip "Inspect connections with `(mcp)`, rediscover tools with `(mcp :refresh)`, or reread configuration with `(mcp :reload)`."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-mcp-control
     :static-options (:refresh :reload))
    (application &optional (mode ':status))
  (let ((mode (application--control-choice mode '(:status :refresh :reload)))
        (added-tools nil)
        (removed-tools nil))
    (cond
      ((eq mode ':status)
       nil)
      ((eq mode ':refresh)
       (mcp-tool-registry-refresh
        (application-tool-registry application)))
      ((eq mode ':reload)
       (multiple-value-setq (added-tools removed-tools)
         (application-reload-mcp application)))
      (t
       (error 'configuration-error
              :message "Usage: (mcp), (mcp :refresh), or (mcp :reload).")))
    (let ((change-entry
            (application--tool-capability-change-entry
             added-tools removed-tools)))
      (when change-entry
        (application-present application change-entry)))
    (let ((manager
            (mcp-tool-registry-manager
             (application-tool-registry application))))
      (application-present
       application
       (if manager
            (application-mcp-status-entry (mcp-manager-status-records manager))
            (list (terminal-span ':hint "No MCP servers are configured."))))))
  ':continue)

(define-application-command application--builtin-checkpoint-command
    (:name "/checkpoint"
     :argument nil
     :description "save a retained live generation"
     :tip "saves the current live state as a retained generation."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-checkpoint application)
  ':continue)

(define-application-command application--builtin-generations-command
    (:name "/generations"
     :argument nil
     :description "list retained generations"
     :tip "shows live generations available for recovery."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present
   application
   (application-generations-entry (application-configuration application)))
  ':continue)

(define-application-command application--builtin-rollback-command
    (:name "/rollback"
     :argument "[ID]"
     :description "pick a generation for recovery"
     :tip "selects a retained generation for the next recovery start."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments
     :callable t)
    (application &optional (identifier nil identifier-supplied-p))
  (let ((identifier
          (if identifier-supplied-p
              identifier
              (and *application-command-interactive-p*
                   (application--pick-identifier
                    application
                    :title "select a generation for recovery"
                    :items (application--generation-items application)
                    :usage "Usage: (rollback \"ID\")"
                    :empty-notice "No retained generations exist.")))))
    (when identifier
      (generation-request-rollback
       (application-configuration application) identifier)))
  ':continue)

(define-application-command application--builtin-info-command
    (:name "/info"
     :argument nil
     :description "show current model and runtime settings"
     :tip "prints the current settings without changing them."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-info-entry application))
  ':continue)

(define-application-command application--builtin-status-command
    (:name "/status"
     :aliases ("/usage")
     :argument nil
     :description "show usage and rate limits"
     :tip "shows the model, context usage, and subscription rate limits."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-status-entry application))
  ':continue)

(define-application-command application--builtin-update-command
    (:name "/update"
     :argument nil
     :description "check and install a newer packaged release"
     :tip "updates curl-installed Autolith and explains source or Nix updates."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-update application)
  ':continue)

(define-application-command application--builtin-context-command
    (:name "/context"
     :argument nil
     :description "inspect request-local context"
     :tip "reveals the ephemeral context prepared for provider requests."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present
   application
   (context-status (application-conversation application)))
  ':continue)

(define-application-command application--builtin-compact-command
    (:name "/compact"
     :argument nil
     :description "compact conversation context now"
     :tip "compacts the conversation context immediately, replacing older turns with a summary."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-compact application)
  ':continue)

(define-application-command application--builtin-fix-skipped-definitions-command
    (:name "/fix-skipped-definitions"
     :argument "[comment]"
     :description "ask the model to rebuild or drop skipped private definitions"
     :tip "Use `(fix-skipped-definitions \"Drop temporary development overrides.\")` to direct which skipped definitions to rebuild or drop before committing."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t
     :slash-parser application--skipped-definitions-slash-arguments)
    (application &optional (comment ""))
  (application-fix-skipped-definitions application comment)
  ':continue)

(define-application-command application--builtin-compact-tool-command
    (:name "/compact-tool"
     :description "collapse or expand tool call detail in the transcript"
     :tip "Use `(compact-tool t)` to collapse tool details or `(compact-tool nil)` to expand them."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t
     :slash-parser application--parse-boolean-control
     :static-options (t nil))
    (application &optional (mode ':status))
  (application-compact-tool-command application mode)
  ':continue)

(define-application-command application--builtin-detach-command
    (:name "/detach"
     :argument nil
     :description "detach Autolith from the current terminal"
     :tip "keeps the session running and returns the foreground shell."
     ;; Detaching exists precisely to leave while work is running; the
     ;; handoff path schedules foreground release safely mid-turn.
     :busy-behavior :execute
     :terminal-behavior :shared
     :callable t)
    (application)
  (let ((session (application-localgroup-session application)))
    (unless session
      (error 'localgroup-error
             :message "The localgroup session is not ready to detach."
             :operation ':detach))
    (let ((result (localgroup--detach-terminal session)))
      (when (getf (rest result) :scheduled-p)
        (application-present
         application
         (list (terminal-span
                ':notice
                "Detach scheduled; the terminal releases once current work reaches a safe stop."))))))
  ':continue)

(define-application-command application--builtin-quit-command
    (:name "/quit"
     :aliases ("/exit")
     :argument nil
     :description "leave Autolith"
     :tip "exits cleanly; Ctrl-C also prints the exact resume command."
     :busy-behavior :cancel
     :terminal-behavior :shared
     :callable t)
    (application)
  (declare (ignore application))
  ':quit)


;;;; -- Command Dispatch --

(-> application-command
    (application string &key (:interactive-p boolean))
    keyword)
(defun application-command (application input &key (interactive-p t))
  "Execute slash command INPUT for APPLICATION and return its loop action."
  (let* ((*application-command-interactive-p* interactive-p)
         (invocation (application-command-invocation-parse input))
         (command (application-command-invocation-command invocation)))
    (application-command--call-with-presentation
     invocation
     (lambda ()
       (if command
           (application-command-execute command application invocation)
           (progn
             (application-present
              application
              (format nil "Unknown command ~A. Use (help)."
                      (application-command-invocation-name invocation)))
             ':continue))))))

(-> application-handle-input
    (application string &key (:interactive-p boolean))
    keyword)
(defun application-handle-input (application input &key (interactive-p t))
  "Handle submitted INPUT and return :CONTINUE or :QUIT."
  (cond
    ((not (non-empty-string-p input))
     :continue)
    ((uiop:string-prefix-p "//" input)
     (application-run-message application (subseq input 1))
     :continue)
    ((uiop:string-prefix-p "/" input)
     (application-command application input :interactive-p interactive-p))
    (t
     (application-run-message application input)
     :continue)))
