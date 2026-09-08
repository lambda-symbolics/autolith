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
                           (configuration-working-directory configuration)))))
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

(-> application--limit-spans (string list) list)
(defun application--limit-spans (fallback-label window)
  "Return one rate limit WINDOW as an aligned transcript row with a usage bar."
  (let* ((used (min 100 (max 0 (getf window :used-percent))))
         (left (- 100 used))
         (cells 20)
         (filled (round (* cells left) 100))
         (bar (concatenate 'string
                           (make-string filled :initial-element #\█)
                           (make-string (- cells filled) :initial-element #\░))))
    (list (terminal-span :dim
                         (format nil "  ~13A "
                                 (format nil "~A limit"
                                         (application--window-label
                                          (getf window :window-minutes)
                                          fallback-label))))
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
     (application--field-spans "model"
                               (configuration-model configuration))
     (application--field-spans "effort"
                               (configuration-reasoning-effort configuration))
     (application--field-spans
      "Codex fast"
      (cond
        ((configuration-codex-fast-mode-active-p configuration)
         "on")
        ((configuration-codex-fast-mode-p configuration)
         "on (saved; inactive)")
        (t
         "off")))
     (application--field-spans "web search"
                               (configuration-web-search-mode configuration))
     (application--field-spans
      "trace"
      (application--toggle-state
       (application-reasoning-traces-p application)))
     (application--field-spans
      "timestamps"
      (application--toggle-state
       (application-turn-timestamps-p application)))
     (application--field-spans
      "compact view"
      (application--toggle-state
       (application-compact-view-p application)))
     (application--field-spans
      "STE"
      (application--toggle-state
       (preferences-simple-technical-english-p configuration)))
     (application--field-spans
      "session titles"
      (application--toggle-state
       (preferences-session-title-generation-p configuration)))
     (application--field-spans
      "hurry-up"
      (application--toggle-state
       (application-hurry-up-p application)))
     (application--field-spans
      "permissions"
      (application--permission-mode-name
       (application-permission-mode application))))))

(-> application-status-entry (application) list)
(defun application-status-entry (application)
  "Return the styled /status summary of APPLICATION's session and rate limits."
  (let* ((configuration (application-configuration application))
         (provider (application-provider application))
         (snapshot (and provider (provider-rate-limits provider)))
         (usage (application--conversation-usage application)))
    (append
     (list (terminal-span :brand "autolith")
           (terminal-span :dim (format nil " v~A~%" *autolith-version*)))
     (application--field-spans "model"
                               (format nil "~A (effort ~A)"
                                       (configuration-model configuration)
                                       (configuration-reasoning-effort
                                        configuration)))
     (application--field-spans "reasoning trace"
                               (if (application-reasoning-traces-p application)
                                   "visible summaries"
                                   "hidden"))
     (application--field-spans "conversation"
                               (conversation-identifier-display
                                (conversation-identifier
                                 (application-conversation application))))
     (application--field-spans "workspace"
                               (or (application--abbreviated-directory
                                    (namestring
                                     (configuration-working-directory
                                      configuration)))
                                   ""))
     (application--field-spans
      "path"
      (if (configuration-codex-fast-mode-active-p configuration)
          "fast (2x Codex plan usage)"
          "standard"))
     (application--field-spans "web search"
                               (configuration-web-search-mode configuration))
     (application--field-spans "goal"
                               (let ((goal (application-goal application)))
                                 (if goal
                                     (format nil "~(~A~): ~A"
                                             (getf goal :status)
                                             (getf goal :objective))
                                     "none")))
     (application--field-spans "token usage"
                               (format nil "~A total (~A input + ~A output)"
                                       (application--token-count-description
                                        (getf usage :total))
                                       (application--token-count-description
                                        (getf usage :input))
                                       (application--token-count-description
                                        (getf usage :output))))
     (if (getf usage :cache-seen-p)
         (application--field-spans
          "prompt cache"
          (application--prompt-cache-description usage))
         nil)
     (application--field-spans
      "context"
      (let ((used (conversation-last-total-tokens
                   (application-conversation application)))
            (window (configuration-context-window configuration)))
        (format nil "~A of ~A used (~D%), compacts at ~D%"
                (application--token-count-description used)
                (application--token-count-description window)
                (round (* 100 used) (max 1 window))
                (configuration-compaction-threshold-percent configuration))))
     (cond
       ((null snapshot)
        (list (terminal-span :dim
                             "  No rate limit data yet; send a message first.")))
       (t
        (append
         (let ((primary (getf snapshot :primary)))
           (when primary
             (application--limit-spans "primary" primary)))
         (let ((secondary (getf snapshot :secondary)))
           (when secondary
             (application--limit-spans "weekly" secondary)))))))))


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

(-> application--abbreviated-directory ((option string)) (option string))
(defun application--abbreviated-directory (namestring)
  "Return NAMESTRING with the user home directory abbreviated to a tilde."
  (when (non-empty-string-p namestring)
    (let ((home (namestring (user-homedir-pathname))))
      (if (and (uiop:string-prefix-p home namestring)
               (> (length namestring) (length home)))
          (concatenate 'string "~/" (subseq namestring (length home)))
          namestring))))

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
           (configuration-working-directory configuration))
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
         (current (configuration-reasoning-effort configuration)))
    (loop for effort in
          (configuration--reasoning-efforts-for
           (configuration-model configuration))
          collect (list :name effort
                        :argument nil
                        :description (if (string= effort current)
                                         "current"
                                         "")))))

(-> application--persist-model-selection (application configuration) null)
(defun application--persist-model-selection (application configuration)
  "Persist CONFIGURATION for the active conversation and future processes."
  (let* ((conversation (application-conversation application))
         (previous-configuration (application-configuration application))
         (previous-model (configuration-model previous-configuration))
         (previous-effort
           (configuration-reasoning-effort previous-configuration)))
    (conversation-set-model-selection
     conversation
     (configuration-model configuration)
     (configuration-reasoning-effort configuration))
    (handler-case
        (preferences-set-model-selection configuration)
      (preferences-error (condition)
        (conversation-set-model-selection conversation
                                          previous-model
                                          previous-effort)
        (error condition))))
  nil)

(-> application-set-reasoning-effort (application string) null)
(defun application-set-reasoning-effort (application effort)
  "Switch APPLICATION to reasoning EFFORT and save it as the global default."
  (let ((configuration
          (configuration-with-reasoning-effort
           (application-configuration application)
           effort)))
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
                    process; unset it before using /fast on or /fast off.")))
  (let ((configuration
          (configuration-with-codex-fast-mode
           (application-configuration application)
           enabled-p)))
    (preferences-set-codex-fast-mode configuration enabled-p)
    (application--install-configuration application configuration)
    (application-publish-recovery-session application))
  nil)

(-> application-codex-fast-mode-command
    (application (option string))
    null)
(defun application-codex-fast-mode-command (application argument)
  "Set or report APPLICATION's persisted Codex Fast mode."
  (let* ((configuration (application-configuration application))
         (mode (and argument (string-downcase argument)))
         (enabled-p (configuration-codex-fast-mode-p configuration))
         (available-p
           (configuration-codex-fast-mode-available-p configuration)))
    (cond
      ((or (null mode) (string= mode "status"))
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
      ((string= mode "on")
       (application-set-codex-fast-mode application t)
       (application-present
        application
        (if available-p
            "Codex Fast mode is enabled and saved. It uses 2x plan usage."
            (format nil
                    "Codex Fast mode preference is enabled and saved. ~
                     The current model uses the standard path."))))
      ((string= mode "off")
       (application-set-codex-fast-mode application nil)
       (application-present application "Codex Fast mode is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /fast [on|off|status]."))))
  nil)

(-> application-set-reasoning-traces (application boolean) null)
(defun application-set-reasoning-traces (application enabled-p)
  "Persist and apply whether future reasoning summaries are visible."
  (preferences-set-reasoning-traces
   (application-configuration application)
   enabled-p)
  (let ((provider (application-provider application)))
    (when provider
      (provider-set-reasoning-summaries provider enabled-p)))
  (unless (eq (application-reasoning-traces-p application) enabled-p)
    (let ((expanding-visibility-p
            (and enabled-p
                 (not (application-reasoning-traces-p application)))))
      (setf (application-reasoning-traces-p application) enabled-p)
      (when expanding-visibility-p
        (application-reset-history-pagination application))))
  (unless enabled-p
    (terminal-ui-set-preview-rows (application-ui application) nil))
  (application-publish-recovery-session application)
  nil)

(-> application-trace-command (application (option string)) null)
(defun application-trace-command (application argument)
  "Show or change APPLICATION's visible reasoning-summary setting."
  (let ((mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil
                "Reasoning summaries are ~:[hidden~;shown~]. This setting ~
                 persists across restarts."
                (application-reasoning-traces-p application))))
      ((string= mode "on")
       (application-set-reasoning-traces application t)
       (application-present
        application
        "Visible reasoning summaries are enabled and saved."))
      ((string= mode "off")
       (application-set-reasoning-traces application nil)
       (application-present application "Reasoning summaries are hidden and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /trace on or /trace off."))))
  nil)

(-> application-set-turn-timestamps (application boolean) null)
(defun application-set-turn-timestamps (application enabled-p)
  "Persist and apply whether user and assistant headers show timestamps."
  (preferences-set-turn-timestamps
   (application-configuration application)
   enabled-p)
  (unless (eq (application-turn-timestamps-p application) enabled-p)
    (setf (application-turn-timestamps-p application) enabled-p)
    (application-reset-history-pagination application))
  (application-publish-recovery-session application)
  nil)

(-> application-turn-timestamps-command (application (option string)) null)
(defun application-turn-timestamps-command (application argument)
  "Show or change APPLICATION's optional turn-timestamp presentation."
  (let ((mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil
                "Turn timestamps are ~:[hidden~;shown~]. This setting persists across restarts."
                (application-turn-timestamps-p application))))
      ((string= mode "on")
       (application-set-turn-timestamps application t)
       (application-present application "Turn timestamps are enabled and saved."))
      ((string= mode "off")
       (application-set-turn-timestamps application nil)
       (application-present application "Turn timestamps are hidden and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /timestamps on or /timestamps off."))))
  nil)

(-> application-simple-technical-english-command
    (application (option string))
    null)
(defun application-simple-technical-english-command (application argument)
  "Show or change APPLICATION's Simple Technical English response style."
  (let* ((configuration (application-configuration application))
         (mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil
                "Simple Technical English is ~:[disabled~;enabled~]. This ~
                 setting persists across restarts."
                (preferences-simple-technical-english-p configuration))))
      ((string= mode "on")
       (preferences-set-simple-technical-english configuration t)
       (application-present
        application
        "Simple Technical English is enabled and saved. Future replies will use it."))
      ((string= mode "off")
       (preferences-set-simple-technical-english configuration nil)
       (application-present
        application
        "Simple Technical English is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /ste on or /ste off."))))
  nil)

(-> application-session-titles-command (application (option string)) null)
(defun application-session-titles-command (application argument)
  "Show or change automatic provider-generated session-title refreshes."
  (let* ((configuration (application-configuration application))
         (mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil
                "Provider-generated session titles are ~:[disabled~;enabled~]. This setting persists across restarts."
                (preferences-session-title-generation-p configuration))))
      ((string= mode "on")
       (preferences-set-session-title-generation configuration t)
       (application-present
        application
        "Provider-generated session titles are enabled and saved."))
      ((string= mode "off")
       (preferences-set-session-title-generation configuration nil)
       (application-present
        application
        "Provider-generated session titles are disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /titles on or /titles off."))))
  nil)

(-> application-compact-view-command (application string) null)
(defun application-compact-view-command (application argument)
  "Persist and apply APPLICATION's compact tool presentation mode."
  (let ((mode (string-downcase argument)))
    (cond
      ((string= mode "on")
       (preferences-set-compact-view
        (application-configuration application)
        t)
       (unless (application-compact-view-p application)
         (setf (application-compact-view-p application) t))
       (application-publish-recovery-session application)
       (application-present
        application
        "Compact tool presentation is enabled and saved."))
      ((string= mode "off")
       (preferences-set-compact-view
        (application-configuration application)
        nil)
       (when (application-compact-view-p application)
         (setf (application-compact-view-p application) nil)
         (application-reset-history-pagination application))
       (application-publish-recovery-session application)
       (application-present
        application
        "Compact tool presentation is disabled and saved."))
      (t
       (error 'configuration-error
              :message "Usage: /compact on or /compact off."))))
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

(-> application--model-items (application) list)
(defun application--model-items (application)
  "Return picker items for every credentialed provider's models."
  (let* ((current (configuration-model
                   (application-configuration application)))
         (current-provider (provider-model-provider-name current))
         (credentialed
           (application--credentialed-provider-names application)))
    (loop for model in (provider-model-identifiers)
          for provider-name = (provider-model-provider-name model)
          for metadata = (provider-model-for model)
          when (application--offered-provider-p
                provider-name current-provider credentialed)
            collect (list :name model
                        :argument nil
                        :group provider-name
                        :description
                        (format nil
                                "~A~@[; ~A~]"
                                (if (string= model current)
                                    "current"
                                    "")
                                (and metadata
                                     (non-empty-string-p
                                      (provider-model-description metadata))))))))

(-> application--models-description (application) string)
(defun application--models-description (application)
  "Return a readable listing of every credentialed provider's models."
  (let* ((current (configuration-model
                   (application-configuration application)))
         (current-provider (provider-model-provider-name current))
         (credentialed
           (application--credentialed-provider-names application)))
    (if (null (provider-model-identifiers))
        "No registered provider models exist."
        (format nil
                "Available models:~%~{~A~%~}"
                (loop for model in (provider-model-identifiers)
                      for provider-name = (provider-model-provider-name model)
                      for metadata = (provider-model-for model)
                        when (application--offered-provider-p
                              provider-name current-provider credentialed)
                          collect
                      (format nil "- ~A / ~A~:[~; (current)~]~@[ - ~A~]"
                              provider-name
                              model
                              (string= model current)
                              (and metadata
                                   (non-empty-string-p
                                    (provider-model-description metadata)))))))))

(-> application-models-command (application) null)
(defun application-models-command (application)
  "Refresh and show the effective registered providers and their model identifiers."
  (let* ((failures
           (provider-refresh-models
            (application-configuration application)))
         (description (application--models-description application)))
    (application-present
     application
     (if failures
         (format nil
                 "~A~%~%Model discovery warnings:~%~{~A~%~}"
                 description
                 (mapcar #'autolith-error-message failures))
         description)))
  nil)

(-> application-set-model (application string) null)
(defun application-set-model (application model)
  "Switch APPLICATION to MODEL and save it as the global default."
  (let ((configuration
          (configuration-with-model (application-configuration application)
                                    model)))
    (application--persist-model-selection application configuration)
    (application--install-configuration application configuration)))

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
        "Compacted; a summary now stands in for the earlier history."))))
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
        "No session goal is set. Use /goal OBJECTIVE to set one.")))

(-> application--goal-update-argument (string) (option string))
(defun application--goal-update-argument (remainder)
  "Return the objective following /goal update, or NIL for other input."
  (let ((word (string-downcase remainder)))
    (and (uiop:string-prefix-p "update" word)
         (or (= (length word) (length "update"))
             (find (char word (length "update")) '(#\Space #\Tab)))
         (application--command-remainder remainder))))

(-> application--goal-update (application string) boolean)
(defun application--goal-update (application objective)
  "Rewrite the current session goal's objective to OBJECTIVE in place.

An active or paused goal keeps its status and creation time while its
continuation budget restarts. A completed goal becomes active again.
Return true only when the objective was rewritten."
  (let ((goal (application-goal application)))
    (cond
      ((null goal)
       (application-present
        application
        "No session goal to update. Use /goal OBJECTIVE to set one.")
       nil)
      ((zerop (length objective))
       (application-present application "Usage: /goal update NEW-OBJECTIVE")
       nil)
      ((uiop:string-prefix-p "/" objective)
       (application-present
        application
        (format nil "~S looks like a command, not an objective. ~
                     Usage: /goal update NEW-OBJECTIVE"
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
                "Goal updated: ~A~:[~;~%It stays paused until /goal resume.~]"
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

(-> application-goal-command (application string) null)
(defun application-goal-command (application remainder)
  "Apply the /goal REMAINDER: show, set, update, clear, pause, or resume the goal."
  (let ((goal (application-goal application))
        (word (string-downcase remainder))
        (update-argument (application--goal-update-argument remainder)))
    (cond
      ((zerop (length remainder))
       (application-present application
                            (application--goal-description application)))
      ((string= word "clear")
       (setf (application-goal application) nil)
       (application--record-goal application)
       (application-present application "The session goal was cleared."))
      ((string= word "pause")
       (if (and goal (eq (getf goal :status) ':active))
           (progn
             (setf (getf (application-goal application) :status) ':paused)
             (application--record-goal application)
             (application-present application "The session goal is paused."))
           (application-present application "No active goal to pause.")))
      ((string= word "resume")
       (if (and goal (eq (getf goal :status) ':paused))
           (progn
             (setf (getf (application-goal application) :status) ':active
                   (getf (application-goal application) :continuations) 0)
             (application--record-goal application)
             (application-present application
                                  "The session goal is active again.")
             (application--start-goal-work application))
           (application-present application "No paused goal to resume.")))
      (update-argument
       (when (and (application--goal-update application update-argument)
                  (application-goal application)
                  (eq (getf (application-goal application) :status) ':active))
         (application--start-goal-work application)))
      ((uiop:string-prefix-p "/" remainder)
       (application-present
        application
        (format nil "~S looks like a command, not an objective. ~
                     Usage: /goal [OBJECTIVE|update NEW-OBJECTIVE|clear|pause|resume]"
                remainder)))
      (t
       (setf (application-goal application)
             (list :objective remainder
                   :status ':active
                   :continuations 0
                   :created-at (get-universal-time)))
       (application--record-goal application)
       (application-present
        application
        (format nil
                "Goal set: ~A~%Autolith is starting work on it now. Use /goal ~
                 to inspect it and /goal clear to stop."
                remainder))
       (application--start-goal-work application))))
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
      (labels ((pick ()
                 "Run the modal selector with sole ownership of terminal input."
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
                  :resize-callback #'application-pending-terminal-size)))
        (let ((controller (application-input-controller application)))
          (if controller
              (application-input-controller-call-with-reader-paused
               controller #'pick)
              (pick)))))))


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
       :usage "Usage: /resume ID"
       :empty-notice "No saved conversations exist."))))


(-> application--pick-model (application) (option string))
(defun application--pick-model (application)
  "Prompt for one registered model and return the selected identifier."
  (let ((configuration (application-configuration application)))
    (application--pick-identifier
     application
     :title "pick the model"
     :items (application--model-items application)
     :visible-count 15
     :initial-name (configuration-model configuration)
     :search-p t
     :usage "Usage: /model NAME"
     :empty-notice "No registered provider models exist.")))


(-> application--pick-reasoning-effort (application) (option string))
(defun application--pick-reasoning-effort (application)
  "Prompt for one supported reasoning effort and return the selected name."
  (let ((configuration (application-configuration application)))
    (application--pick-identifier
     application
     :title "pick the reasoning effort"
     :items (application--effort-items application)
     :initial-name (configuration-reasoning-effort configuration)
     :search-p t
     :usage "Usage: /effort LEVEL"
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
            (configuration-working-directory configuration))))
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

(-> application-working-directory-command (application string) null)
(defun application-working-directory-command (application remainder)
  "Show or change APPLICATION's workspace from the /cwd command REMAINDER."
  (if (non-empty-string-p remainder)
      (let ((directory (application-set-working-directory application remainder)))
        (application-present
         application
         (format nil "Working directory is now ~A" (namestring directory))))
      (application-present
       application
       (format nil "Working directory: ~A"
               (namestring
                (configuration-working-directory
                 (application-configuration application))))))
  nil)


;;;; -- Authentication and Checkpoint Commands --

(-> application--authentication-provider-items (application) list)
(defun application--authentication-provider-items (application)
  "Return picker items for every effective registered provider."
  (let ((current-provider
          (provider-model-provider-name
           (configuration-model
            (application-configuration application)))))
    (loop for registration in (provider-registrations)
          for name = (provider-registration-name registration)
          collect (list :name name
                        :argument nil
                        :description
                        (if (and current-provider
                                 (string= name current-provider))
                            "current provider"
                            "registered provider")))))

(-> application--pick-authentication-provider (application) (option string))
(defun application--pick-authentication-provider (application)
  "Prompt for one registered provider to authenticate."
  (application--pick-identifier
   application
   :title "pick the provider to authenticate"
   :items (application--authentication-provider-items application)
   :initial-name
   (provider-model-provider-name
    (configuration-model
     (application-configuration application)))
   :search-p t
   :usage "Usage: /auth PROVIDER"
   :empty-notice "No registered providers exist."))

(-> application--authentication-provider (application string) model-provider)
(defun application--authentication-provider (application provider-name)
  "Return APPLICATION's named provider for authentication."
  (provider-authentication-provider
   (application-configuration application)
   provider-name
   :reasoning-summaries-p (application-reasoning-traces-p application)))

(defclass application-authentication-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((terminal
    :initarg :terminal
    :reader application-authentication-output-stream-terminal
    :type terminal
    :documentation "The terminal receiving authentication output."))
  (:documentation "A character stream routing authentication output through a terminal."))

(defmethod sb-gray:stream-write-char
    ((stream application-authentication-output-stream) character)
  "Write CHARACTER through STREAM's terminal."
  (terminal--write
   (application-authentication-output-stream-terminal stream)
   (string character))
  character)

(defmethod sb-gray:stream-write-string
    ((stream application-authentication-output-stream) string
     &optional (start 0) end)
  "Write STRING's selected range through STREAM's terminal."
  (terminal--write
   (application-authentication-output-stream-terminal stream)
   (subseq string start end))
  string)

(defmethod sb-gray:stream-force-output
    ((stream application-authentication-output-stream))
  "Flush STREAM's terminal output."
  (terminal-flush
   (application-authentication-output-stream-terminal stream)))

(defmethod sb-gray:stream-finish-output
    ((stream application-authentication-output-stream))
  "Finish STREAM's terminal output."
  (terminal-flush
   (application-authentication-output-stream-terminal stream)))

(defclass application-authentication-input-stream
    (sb-gray:fundamental-character-input-stream)
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

(defmethod sb-gray:stream-read-char
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

(defmethod sb-gray:stream-listen
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
    (application string &optional (or null string symbol))
    null)
(defun application-authenticate (application provider-name &optional method)
  "Authenticate APPLICATION's explicitly named provider using optional METHOD."
  (let* ((ui       (application-ui application))
         (provider (application--authentication-provider application provider-name))
         (message  nil))
    (application-present
     application
     (format nil "Authenticating provider ~A." provider-name))
    (multiple-value-bind
          (input output stop-ui-p input-echo-disabled-p input-file-descriptor)
        (application--authentication-streams application)
      (if stop-ui-p
          (terminal-ui-stop ui)
          (application--authentication-ui-suspend ui))
      (unwind-protect
           (let ((*standard-input* input)
                 (*standard-output* output)
                 (*api-key-input-echo-disabled-p* input-echo-disabled-p)
                 (*api-key-input-file-descriptor* input-file-descriptor)
                 (*api-key-output-styled-p*
                   (terminal-styled-p (terminal-ui-terminal ui))))
              (setf message
                    (provider-authenticate-with-method
                     provider method
                     :stream output
                     :open-browser-p t)))
        (if stop-ui-p
            (terminal-ui-start ui)
            (application--authentication-ui-resume ui))))
    (application-present application message))
  nil)

(-> application-checkpoint (application) null)
(defun application-checkpoint (application)
  "Begin a non-stopping retained generation for APPLICATION."
  (application-set-activity application "checking source before checkpoint")
  (unwind-protect
       (let ((generation
               (checkpoint-create
                (checkpoint-backend-create
                 (application-configuration application)
                 (application-worker application)
                 :tool-registry (application-tool-registry application)))))
         (application-present
          application
          (format nil "Checkpoint ~A is publishing in process ~D."
                  (generation-identifier generation)
                  (generation-coordinator-pid generation))))
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

(-> application--set-durable-permission-mode (application keyword) null)
(defun application--set-durable-permission-mode (application mode)
  "Set APPLICATION's session permission MODE and persist ask or auto."
  (setf (application-permission-mode application) mode)
  (when (member mode '(:ask :auto) :test #'eq)
    (preferences-set-permission-mode
     (application-configuration application)
     mode))
  nil)

(-> application-permissions-command (application (option string)) null)
(defun application-permissions-command (application argument)
  "Apply ARGUMENT to APPLICATION's session command permissions."
  (let ((choice (and argument (string-downcase argument))))
    (cond
      ((null choice)
       nil)
      ((string= choice "ask")
       (application--set-durable-permission-mode application ':ask)
       (application-present
        application
        "Commands will ask before running unless the exact command was saved."))
      ((string= choice "auto")
       (application--set-durable-permission-mode application ':auto)
       (application-present
        application
         (if (application--command-sandbox-available-p)
             "Commands will be classified automatically as sandboxed, full access, or denied."
             "Commands will be classified automatically as full access or denied.")))
      ((string= choice "sandbox")
       (unless (application--command-sandbox-available-p)
         (error 'configuration-error
                :message (application--command-sandbox-unavailable-message)))
       (setf (application-permission-mode application) ':sandboxed)
       (application-present
        application
        "Commands may run for this session inside the workspace sandbox."))
      ((string= choice "full")
       (setf (application-permission-mode application) ':full-access)
       (application-present
        application
        "Commands may run for this session with your full user privileges."))
      ((string= choice "list")
       (application-present application
                            (application--saved-permissions-text application)))
      ((string= choice "clear")
       (permissions-clear
        (application-configuration application)
        (application-permission-state application))
       (application-present application "Saved command approvals were cleared."))
      (t
       (error 'configuration-error
              :message "Usage: /permissions [ask|auto|sandbox|full|list|clear]."))))
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

(-> application-hurry-up-command (application (option string)) null)
(defun application-hurry-up-command (application argument)
  "Show or change APPLICATION's session-scoped hurry-up mode."
  (let ((mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       (application-present
        application
        (format nil "Hurry-up mode is ~:[disabled~;enabled~]."
                (application-hurry-up-p application))))
      ((string= mode "on")
       (application-set-hurry-up application t))
      ((string= mode "off")
       (application-set-hurry-up application nil))
      (t
       (error 'configuration-error
              :message "Usage: /hurry-up on or /hurry-up off."))))
  nil)

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
     :argument nil
     :description "show this reference"
     :tip "shows every registered command and tool operation."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-present application (application-operation-help application))
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
                       (application-list-conversations application))
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
     :tip "shows or moves the active workspace without restarting Autolith."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application &optional (pathname ""))
  (application-working-directory-command
   application
   (etypecase pathname
     (string pathname)
     (pathname (namestring pathname))))
  ':continue)

(define-application-command application--builtin-authentication-command
    (:name "/auth"
     :argument "[PROVIDER] [METHOD]"
     :description "pick and authenticate a registered provider"
     :tip "uses browser auth by default; ChatGPT also accepts device."
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
     :argument "[MODEL]"
     :description "pick a registered provider model and reasoning effort"
     :tip "changes both the model and its reasoning effort."
     :busy-behavior :apply
     :terminal-behavior :exclusive
     :callable t)
    (application &optional (model nil model-supplied-p))
  (let ((model
          (if model-supplied-p
              model
              (and *application-command-interactive-p*
                   (application--pick-model application)))))
    (when model
      (application-set-model application model)
      (let ((effort
              (and *application-command-interactive-p*
                   (application--pick-reasoning-effort application))))
        (when effort
          (application-set-reasoning-effort application effort))
        (application-present
         application
         (format nil "The model is now ~A with reasoning effort ~A."
                 (configuration-model
                  (application-configuration application))
                 (configuration-reasoning-effort
                  (application-configuration application)))))))
  ':continue)

(define-application-command application--builtin-models-command
    (:name "/models"
     :argument nil
     :description "list registered providers and models"
     :tip "shows every effective provider registration and its models."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-models-command application)
  ':continue)

(define-application-command application--builtin-effort-command
    (:name "/effort"
     :argument "[LEVEL]"
     :description "pick the reasoning effort"
     :tip "changes reasoning effort without switching models."
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
               (configuration-reasoning-effort
                (application-configuration application))))))
  ':continue)

(define-application-command application--builtin-fast-command
    (:name "/fast"
     :description "show or change Codex Fast mode"
     :tip "uses service_tier=priority at 2x plan usage for future Codex requests."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off" "status"))
    (application &optional mode)
  (application-codex-fast-mode-command application mode)
  ':continue)

(define-application-command application--builtin-trace-command
    (:name "/trace"
     :description "show visible reasoning summaries"
     :tip "toggles visible reasoning summaries with on or off."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (application-trace-command application mode)
  ':continue)

(define-application-command application--builtin-turn-timestamps-command
    (:name "/timestamps"
     :description "show local timestamps beside user and assistant turns"
     :tip "toggles dim local timestamps beside user and assistant turns."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (application-turn-timestamps-command application mode)
  ':continue)

(define-application-command application--builtin-simple-technical-english-command
    (:name "/ste"
     :description "use Simple Technical English for replies"
     :tip "toggles short, direct Simple Technical English replies."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (application-simple-technical-english-command application mode)
  ':continue)

(define-application-command application--builtin-session-titles-command
    (:name "/titles"
     :description "allow provider-generated session-title refreshes"
     :tip "toggles automatic provider-generated session-title refreshes."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (application-session-titles-command application mode)
  ':continue)

(define-application-command application--builtin-hurry-up-command
    (:name "/hurry-up"
     :description "show or change hurry-up mode"
     :tip "enables direct execution and admits at most two child agents."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (application-hurry-up-command application mode)
  ':continue)

(define-application-command application--builtin-permissions-command
    (:name "/permissions"
     :description "choose command access for this session"
     :tip "chooses how shell commands are authorized, including autonomous auto mode."
     :busy-behavior :apply
     :terminal-behavior :exclusive-without-arguments
     :callable t
     :static-options ("ask" "auto" "sandbox" "full" "list" "clear"))
    (application &optional (choice nil choice-supplied-p))
  (let ((choice
          (if choice-supplied-p
              choice
              (and *application-command-interactive-p*
                   (application--pick-identifier
                    application
                    :title "command permissions"
                    :items (application--permission-mode-items application)
                    :usage "Usage: /permissions [ask|auto|sandbox|full|list|clear]"
                    :empty-notice "No command permission modes exist.")))))
    (application-permissions-command application choice))
  ':continue)

(define-application-command application--builtin-goal-command
    (:name "/goal"
     :argument "[OBJECTIVE]"
     :description "set or view the session goal"
     :tip "sets the objective Autolith should pursue across continuations."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application &optional (remainder ""))
  (application-goal-command application remainder)
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
     :tip "opens the full report named by (papercuts)."
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
     :tip "removes a fixed or obsolete report from (papercuts)."
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

(define-application-command application--builtin-mcp-command
    (:name "/mcp"
     :description "show or refresh configured MCP servers"
     :tip "shows MCP connections; refresh rediscovers, reload rereads configuration."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t
     :static-options ("refresh" "reload"))
    (application &optional mode)
  (let ((mode (and mode (string-downcase mode)))
        (added-tools nil)
        (removed-tools nil))
    (cond
      ((null mode)
       nil)
      ((string= mode "refresh")
       (mcp-tool-registry-refresh
        (application-tool-registry application)))
      ((string= mode "reload")
       (multiple-value-setq (added-tools removed-tools)
         (application-reload-mcp application)))
      (t
       (error 'configuration-error
              :message "Usage: /mcp, /mcp refresh, or /mcp reload.")))
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
           (mcp-manager-render-status manager)
           "No MCP servers are configured."))))
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
   (generation-render-list (application-configuration application)))
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
                    :usage "Usage: /rollback ID"
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
     :description "compact context now; on/off selects tool detail presentation"
     :tip "compacts context without an argument, waiting until an active turn finishes."
     :busy-behavior :hold
     :terminal-behavior :shared
     :callable t
     :static-options ("on" "off"))
    (application &optional mode)
  (if mode
      (application-compact-view-command application mode)
      (application-compact application))
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
              (format nil "Unknown command ~A. Use /help."
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
