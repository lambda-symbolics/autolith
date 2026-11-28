(in-package #:autolith)

;;;; -- Interactive Commands --

(-> application--entry-fields (list) list)
(defun application--entry-fields (fields)
  "Return terminal spans for FIELDS, a list of label, style, and value triples.

Align sanitized labels by terminal cell width. Values wrap in the transcript
renderer, including in fullscreen."
  (let* ((labels (mapcar (lambda (field)
                          (sanitize-text (first field) :single-line-p t))

                        fields))
         (width  (loop for label in labels maximize (text-cell-width label) into size
                       finally (return (or size 0)))))
    (loop for field in fields
          for safe-label in labels
          append (list (terminal-span ':dim
                                      (format nil "  ~A~A  " safe-label
                                              (make-string (- width (text-cell-width safe-label))
                                                           :initial-element #\Space)))
                       (terminal-span (second field) (princ-to-string (third field)))
                       (terminal-span ':plain (string #\Newline))))))

(-> application--agenda-status-style (agenda-status) keyword)
(defun application--agenda-status-style (status)
  "Return the terminal style associated with agenda STATUS."
  (case status
    (:done
     ':success)
    (:blocked
     ':failure)
    (:doing
     ':brand)
    (:note
     ':dim)
    (otherwise
     ':hint)))

(-> application-agenda-entry (application) list)
(defun application-agenda-entry (application)
  "Return the current workspace agenda as a readable transcript entry."
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((configuration (application-configuration application))
           (record        (agenda-current configuration (agenda-load configuration)))
           (items         (and record (workspace-agenda-items record))))
      (if (null items)
          (application-hint-line "the current workspace agenda is empty")
          (append
           (list (terminal-span ':brand "# agenda")
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
            (format-utc-timestamp (papercut-reported-at papercut))
            (papercut-call-source papercut)))))

(-> application--papercut-ambiguous-entry (string list) list)
(defun application--papercut-ambiguous-entry (identifier matches)
  "Return the failure saying IDENTIFIER names each of the MATCHES."
  (append
   (application-failure-line
    (format nil "papercut ID ~A is ambiguous; enter more characters"
            (sanitize-text identifier :single-line-p t)))
   (list (terminal-span ':dim (format nil "~%  matching reports:~%")))
   (loop for match in matches
         append (application--papercut-list-row match))))

(-> application-papercuts-entry (application) list)
(defun application-papercuts-entry (application)
  "Return the current workspace's papercuts as a compact transcript entry."
  (let* ((configuration (application-configuration application))
         (papercuts     (papercut-list configuration)))
    (if (null papercuts)
        (application-hint-line "no papercuts recorded for this workspace")
        (append
         (list (terminal-span
                ':failure
                (format nil "# papercuts  ~D~%" (length papercuts)))
               (terminal-span
                ':dim
                (format nil "  newest first · use (papercut \"ID\") for the full report~%")))
         (loop for papercut in papercuts
               append (application--papercut-list-row papercut))))))

(-> application-papercut-entry (application string) list)
(defun application-papercut-entry (application identifier)
  "Return one complete PAPERCUT report or a readable resolution notice."
  (let ((configuration (application-configuration application)))
    (multiple-value-bind (papercut status matches)
        (papercut-resolve configuration identifier)
      (case status
        (:found
         (list
          (terminal-span ':failure (format nil "# papercut~%"))
          (terminal-span
           ':dim
           (format nil "  id ~A~%  reported ~A~%"
                   (papercut-identifier papercut)
                   (format-utc-timestamp (papercut-reported-at papercut))))
          (terminal-span
           ':strong
           (format nil "  ~A~%~%"
                   (sanitize-text (papercut-title papercut)
                                  :single-line-p t)))
          (terminal-span ':plain (sanitize-text (papercut-content papercut)))))
        (:ambiguous
         (application--papercut-ambiguous-entry identifier matches))
        (otherwise
         (application-failure-line
          (format nil "no papercut matches ~S; (papercuts) lists current reports" identifier)))))))

(-> application-papercut-close-entry (application string) list)
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
         (application-success-line
          (format nil "closed papercut ~A"
                  (sanitize-text (papercut-title papercut) :single-line-p t))))
        (:ambiguous
         (application--papercut-ambiguous-entry identifier matches))
        (otherwise
         (application-failure-line
          (format nil "no active papercut matches ~S; (papercuts) lists reports" identifier)))))))


;;;; -- Usage Status --

(-> application--conversation-usage (application) list)
(defun application--conversation-usage (application)
  "Return summed token and prompt-cache usage for the active conversation."
  (destructuring-bind (input output total cached cache-created cache-seen-p)
      (conversation-usage (application-conversation application))
    (list :input         input
          :output        output
          :total         total
          :cached        cached
          :cache-created cache-created
          :cache-seen-p  cache-seen-p)))

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

(-> application--cost-description (configuration) string)
(defun application--cost-description (configuration)
  "Describe this process's recorded request cost, its limit and unpriced requests."
  (let* ((summary  (cost-ledger-summary *provider-cost-ledger*))
         (counts   (getf summary :counts))
         (dollars  (or (second (assoc "USD" (getf summary :totals) :test #'string=)) 0))
         (limit    (config :cost-limit configuration))
         (unpriced (+ (getf counts :unpriced) (getf counts :unknown))))
    (format nil "$~,4F this process~@[ of a $~,2F limit~]~[~:;, ~:*~D request~:P unpriced~]"
            (float dollars 1d0)
            (and (plusp limit) (float limit 1d0))
            unpriced)))

(-> application--prompt-cache-description (list) string)
(defun application--prompt-cache-description (usage)
  "Return a compact prompt-cache summary for aggregate USAGE."
  (let* ((input   (getf usage :input))
         (cached  (getf usage :cached))
         (created (getf usage :cache-created))
         (rate    (and (plusp input)
                       (min 100 (round (* 100 cached) input)))))
    (format nil "~A read~A, ~A written"
            (application--token-count-description cached)
            (if rate (format nil " (~D% of input)" rate) "")
            (application--token-count-description created))))


;;;; -- Prompt-Cache Miss Notices --

(-> application--prompt-cache-miss-notice (list) list)
(defun application--prompt-cache-miss-notice (miss)
  "Return measured lost reads or stalled shared-history growth, with current usage."
  (append
   (list (terminal-span ':failure "! ")
         (terminal-span ':notice
                        (if (eq (getf miss :kind) ':stalled)
                            "cache stalled · "
                            "cache miss ")))
   (when (getf miss :lost-tokens)
     (list (terminal-span ':code
                          (application--token-count-description (getf miss :lost-tokens)))
           (terminal-span ':dim " cached tokens lost · ")))
   (list (terminal-span ':code
                        (application--token-count-description (getf miss :cached-tokens)))
         (terminal-span ':dim " / ")
         (terminal-span ':code
                        (application--token-count-description (getf miss :prompt-tokens)))
         (terminal-span ':dim " input tokens cached"))
   (when (eq (getf miss :kind) ':stalled)
     (list (terminal-span ':dim
                          (format nil " · shared history grew ~A characters"
                                  (application--token-count-description
                                   (getf miss :history-growth-characters))))))
   (list (terminal-span ':dim
                        (case (getf miss :cause)
                          (:model-changed
                           " · model changed")
                          (:idle
                           " · idle interval exceeded local heuristic")
                          (:prompt-shrank
                           " · prompt size decreased")
                          (:resumed
                           " · resumed session")
                          (otherwise
                           ""))))
   (when-let ((description (application--wire-change-description (getf miss :wire-change))))
     (list (terminal-span ':dim (format nil " · request ~A" description))))))

(-> application--wire-change-description (list) (option string))
(defun application--wire-change-description (change)
  "Describe the final-request CHANGE behind a cache notice, or NIL without a comparison.

A request that repeats the previous reusable prefix with the same fields and
markers reads as intact, which points at the provider rather than the prompt."
  (and change
       (if-let ((parts
                  (remove nil
                          (list (and (getf change :fields)
                                     (format nil "~{~A~^, ~} changed" (getf change :fields)))
                                (let ((index (getf change :first-changed-item))
                                      (count (getf change :items)))
                                  (when index
                                    (if (< index count)
                                        (format nil "item ~D of ~D changed" (1+ index) count)
                                        (format nil "item ~D removed" (1+ index)))))
                                (and (getf change :breakpoints-moved-p)
                                     "cache markers moved")))))
           (format nil "~{~A~^, ~}" parts)
           "prefix intact")))

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
  (let* ((usage    (getf details :usage))
         (prefix   (getf details :cache-prefix))
         (model    (application-prompt-cache-request-model application))
         (miss     (prompt-cache-miss-detect
                    (application-prompt-cache-baseline application)
                    usage
                    :started-at
                    (application-prompt-cache-request-started-at application)
                    :model model :prefix prefix))
         (baseline (prompt-cache-baseline-advance
                    (application-prompt-cache-baseline application)
                    usage
                    :completed-at (get-universal-time)
                    :model        model
                    :prefix       prefix
                    :reported-p   (not (null miss)))))
    (when baseline
      (setf (application-prompt-cache-baseline application) baseline))
    (when (and miss (application-cache-miss-notices-p application))
      (with-recursive-lock-held ((application-render-lock application))
        (application-render-records application)
        (application-side-status-present
         application "prompt-cache" (application--prompt-cache-miss-notice miss)))))
  nil)

(-> application--window-label ((option integer) string) string)
(defun application--window-label (minutes fallback)
  "Return the human name of a MINUTES-long rate limit window."
  (labels ((approximately-p (expected)
             "Return true when MINUTES is within five percent of EXPECTED."
             (and minutes
                  (<= (* expected 95/100) minutes (* expected 105/100)))))
    (cond
      ((approximately-p 300)
       "5h")
      ((approximately-p 1440)
       "daily")
      ((approximately-p 10080)
       "weekly")
      ((approximately-p 43200)
       "monthly")
      ((null minutes)
       fallback)
      ((>= minutes 60)
       (format nil "~Dh" (round minutes 60)))
      (t
       (format nil "~Dm" minutes)))))

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
  (let* ((used   (min 100 (max 0 (getf window :used-percent))))
         (left   (- 100 used))
         (cells  20)
         (filled (round (* cells left) 100))
         (bar    (concatenate 'string
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
     (list (terminal-span ':brand (format nil "# settings~%")))
     (application--field-rows
      (list
       (list "model" (config :model configuration))
       (list "effort" (config :reasoning-effort configuration))
       (list "codex fast"
             (cond
               ((configuration-codex-fast-mode-active-p configuration)
                "on")
               ((config :codex-fast-mode-p configuration)
                "on (saved; inactive)")
               (t
                "off")))
       (list "web search" (string-downcase (config :web-search-mode configuration)))
       (list "trace"
             (application--toggle-state
              (application-reasoning-traces-p application)))
       (list "timestamps"
             (application--toggle-state
              (application-turn-timestamps-p application)))
       (list "cache misses"
             (application--toggle-state
              (application-cache-miss-notices-p application)))
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
  "Return the styled (status) summary of APPLICATION's session and rate limits."
  (let* ((configuration   (application-configuration application))
         (provider        (application-provider application))
         (snapshot        (and provider (provider-rate-limits provider)))
         (usage           (application--conversation-usage application))
         (primary         (getf snapshot :primary))
         (secondary       (getf snapshot :secondary))
         (primary-label   (and primary
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
             (list "web search" (string-downcase (config :web-search-mode configuration)))
             (list "goal"
                   (if-let ((goal (application-goal application)))
                       (format nil "~(~A~): ~A"
                               (getf goal :status)
                               (getf goal :objective))
                       "none"))
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
             (list "cost" (application--cost-description configuration))
             (list "memory" (application--retention-description application))
             (list "context"
                   (multiple-value-bind (used estimated-p)
                       (application-context-usage application)
                      (declare (ignore estimated-p))
                      (let ((window (config :context-window configuration)))
                        (format nil "~A of ~A used (~D%), compacts at ~D%"
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
     (list (terminal-span :brand "# autolith")
           (terminal-span :dim (format nil " v~A~%" *autolith-version*)))
     (application--field-rows fields :width width)
     (cond
       ((null snapshot)
        (list (terminal-span :dim
                             "  no rate limit data yet; send a message first")))
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
  (when-let ((preview (conversation-picker-metadata-preview metadata)))
    (text-cell-prefix
     (sanitize-text preview :single-line-p t)
     *conversation-preview-width*)))

(-> application--effort-items (application) list)
(defun application--effort-items (application)
  "Return picker items for the active model's supported reasoning efforts."
  (let* ((configuration (application-configuration application))
         (current       (config :reasoning-effort configuration)))
    (loop for effort in
          (configuration--reasoning-efforts-for
           (config :model configuration) (config :model-provider configuration))
          collect (list :name        effort
                        :argument    nil
                        :description (if (string= effort current)
                                         "current"
                                         "")))))

(-> application--persist-model-selection (application configuration) null)
(defun application--persist-model-selection (application configuration)
  "Record the selected provider, model and effort on the active conversation and its resident agent."
  (let ((provider (config :model-provider configuration)))
    (conversation-set-model-selection
     (application-conversation application)
     (config :model configuration)
     (config :reasoning-effort configuration)
     :provider (and (non-empty-string-p provider) provider))
    (application-world-agent-settings-update
     application
     (append (list :model (config :model configuration))
             (and (non-empty-string-p provider) (list :model-provider provider))
             (let ((effort (config :reasoning-effort configuration)))
               (and effort (list :reasoning-effort effort))))))
  nil)

(-> application-set-reasoning-effort (application string) null)
(defun application-set-reasoning-effort (application effort)
  "Switch APPLICATION to reasoning EFFORT and save it as the global default.

Save the current model and provider with EFFORT so future sessions use the same
combination."
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
           :message "Unset AUTOLITH_CODEX_FAST_MODE before changing the Fast mode setting."))
  (let ((configuration (application-configuration application)))
    (setf (config :codex-fast-mode-p configuration) enabled-p)
    (application--install-configuration application configuration))
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
    ((eq value t)
     ':on)
    ((null value)
     ':off)
    (t
     (application--control-choice value '(:on :off :status)))))

(-> application-set-reasoning-traces (application boolean) null)
(defun application-set-reasoning-traces (application enabled-p)
  "Persist and apply whether future reasoning summaries are visible."
  (let ((expanding-visibility-p
          (and enabled-p (not (application-reasoning-traces-p application)))))
    (setf (application-reasoning-traces-p application) enabled-p)
    (when-let ((provider (application-provider application)))
      (provider-set-reasoning-summaries provider enabled-p))
    (when expanding-visibility-p
      (application-reset-history-pagination application)))
  (unless enabled-p
    (terminal-ui-set-preview-rows (application-ui application) nil))
  nil)

(-> application--boolean-setting-command (application keyword t) null)
(defun application--boolean-setting-command (application name argument)
  "Report NAME or apply ARGUMENT through the setting's runtime policy."
  (let* ((mode    (application--boolean-control argument))
         (setting (configuration-setting (application-configuration application) name)))
    (unless (eq mode ':status)
      (application-apply-setting application name (eq mode ':on)))
    (multiple-value-bind (value source)
        (application--setting-state application setting)
      (application-present
       application
       (format nil "~A: ~A (~A)."
               (setting-label setting)
               (setting-render-value setting value)
               (application--setting-source-text source setting)))))
  nil)

(-> application-set-turn-timestamps (application boolean) null)
(defun application-set-turn-timestamps (application enabled-p)
  "Persist and apply whether user and assistant headers show timestamps."
  (unless (eq (application-turn-timestamps-p application) enabled-p)
    (setf (application-turn-timestamps-p application) enabled-p)
    (application-reset-history-pagination application))
  nil)

(-> application-set-cache-miss-notices (application boolean) null)
(defun application-set-cache-miss-notices (application enabled-p)
  "Persist and apply whether prompt-cache misses are reported after requests."
  (setf (application-cache-miss-notices-p application) enabled-p)
  nil)

(-> application-simple-technical-english-command
    (application &optional t)
    null)
(defun application-simple-technical-english-command (application &optional (argument ':status))
  "Show or change APPLICATION's Simple Technical English response style."
  (application--boolean-setting-command application ':simple-technical-english-p argument))

(-> application--credentialed-provider-names (application) list)
(defun application--credentialed-provider-names (application)
  "Return effective provider names whose local credentials are available."
  (let ((configuration (application-configuration application)))
    (loop for registration in (provider-registrations)
          when (eq (provider-credential-status configuration registration :refresh-p nil) ':available)
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
  (let* ((configuration    (application-configuration application))
         (current          (configuration-model-registration configuration))
         (current-provider (and current (provider-registration-name current)))
         (credentialed     (application--credentialed-provider-names application)))
    (loop for registration in (provider-registrations)
          for name = (provider-registration-name registration)
          when (if provider
                   (string= name (provider--canonical-name provider))
                   (application--offered-provider-p name current-provider credentialed))
            append
            (loop for metadata in (provider-registration-models registration)
                  for model = (provider-model-name metadata)
                  collect (list :name      (provider-model-selection-id model registration)
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
  (if-let ((items (application--model-items application :provider provider)))
      (format nil "Available models:~%~{- ~A~%~}" (mapcar (lambda (item) (getf item :name)) items))
      "No registered provider models are available."))

(-> application-models-entry (application list &key (:provider (option string))) list)
(defun application-models-entry (application failures &key provider)
  "Return offered provider/model selections and discovery FAILURES."
  (let ((items (application--model-items application :provider provider)))
    (append
     (list (terminal-span ':brand (format nil "# models~%")))
     (if items
         (loop for item in items
               append (list (terminal-span ':code (format nil "  ~A" (getf item :name)))
                            (terminal-span (if (getf item :current-p) ':success ':dim)
                                           (format nil " ~A~%" (getf item :description)))))
         (list (terminal-span ':hint (format nil "  no registered provider models are available~%"))))
     (when failures
       (cons (terminal-span ':notice (format nil "! model discovery warnings~%"))
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
       :message               (format nil "Unknown model ~S. Use (models) to list available models." model)))
    (let ((configuration
            (configuration-copy
             (application-configuration application)
             :model          identifier
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
            (format nil "! private replay skipped ~D definition~:P~%" (length skips)))
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
     :commit-identifier   *active-image-commit-identifier*
     :lineage-source-commit
     (application--replay-lineage-source-commit application)
     :image-source-commit (image-commit--base-source-commit nil))))

(-> application--skipped-definition-section
    (stream image-replay-skip &key (:index integer) (:configuration (option configuration)))
    null)
(defun application--skipped-definition-section (stream skip &key index configuration)
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
             install it with self.define, and check that it works. When every ~
             definition is handled, publish the result with self.commit, even ~
             if all skipped overrides were dropped, so the next start replays ~
             only the retained overrides. Explain which overrides were dropped. ~
             Do not edit tracked source files.~%")
    (flet ((write-sections ()
             (loop for skip in skips
                   for index from 1
                   do (application--skipped-definition-section
                       stream skip :index index :configuration configuration))))
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
        (application-hint-line "no private definitions were skipped at startup")))
      (t
       (let ((controller (application-input-controller application)))
         (unless controller
           (error 'configuration-error
                  :message "Only an interactive session can queue the repair turn."))
         (unless (application-input-controller-enqueue-operation-request
                  controller
                  (application--skipped-definitions-prompt
                   skips
                   :configuration     (application-configuration application)
                   :commit-identifier *active-image-commit-identifier*
                   :lineage-source-commit
                   (application--replay-lineage-source-commit application)
                   :image-source-commit
                   (image-commit--base-source-commit nil)
                   :comment           comment)
                  (list :operation "fix-skipped-definitions"
                        :summary
                        (format nil "Rebuild or drop the ~D private definition~:P skipped at startup."
                                (length skips))))
           (error 'configuration-error
                  :message "The repair turn was not accepted: the session is stopping."))
         (application-present
          application
          (application-notice-line
           (format nil "queued ~D skipped definition~:P for repair or removal" (length skips))))))))
  nil)


;;;; -- Manual Compaction --

(-> application-compact (application) null)
(defun application-compact (application)
  "Compact the active conversation, or report an empty conversation without a request."
  (let ((agent        (application-agent application))
        (conversation (application-conversation application)))
    (cond
      ((null (conversation-input-items conversation))
       (application-present application (application-hint-line "nothing to compact yet")))
      ((null agent)
       (error 'configuration-error
              :message "No connected agent can compact the conversation."))
      (t
       (unwind-protect
            (handler-case
                (agent-compact-conversation
                 agent (application-agent-observer application))
              (provider-context-budget-error (condition)
                (application-present-context-refusal application condition)))
         (terminal-ui-set-compacting (application-ui application) nil)
         (application-set-activity application nil)))))
  nil)


;;;; -- Session Goal Command --

(-> application--start-goal-work (application) null)
(defun application--start-goal-work (application)
  "Begin working toward APPLICATION's active session goal immediately.

Goal turns run inside the (goal) command's work item, so they wire the
input controller's steering hooks themselves: prose typed during goal
work must steer the running turn, not queue as follow-up work."
  (let ((goal (application-goal application)))
    (when (and goal (eq (getf goal :status) ':active))
      (setf (getf (application-goal application) :continuations) 0)
      (flet ((work (steering-function steering-persisted-function)
               (application--run-turn application
                                      *application-goal-continuation-prompt*
                                      :continuation-p    t
                                      :steering-function steering-function
                                      :steering-persisted-function
                                      steering-persisted-function)
               (application--run-goal-continuations
                application
                :steering-function           steering-function
                :steering-persisted-function steering-persisted-function)))
        (if-let ((controller (application-input-controller application)))
            (application-input-controller-call-with-primary-steering
             controller #'work)
            (work nil nil)))))
  nil)

(define-condition application-goal-error (autolith-error)
  ((reason
    :initarg       :reason
    :reader        application-goal-error-reason
    :type          keyword
    :documentation "The stable reason the goal request was rejected.")
   (request
    :initarg       :request
    :reader        application-goal-error-request
    :type          t
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
  (let ((mission (application-goal application)))
    (case request
      ((nil :status)
       (application-mission-command application ':status))
      (:clear
       (application-mission-command application ':clear))
      (:pause
       (application-mission-command application ':pause))
      (:resume
       (application-mission-command application ':resume)
       (application-mission-command application ':run))
      (:update
       (unless (eq (getf mission :acceptance-policy) ':final-marker)
         (mission--reject ':invalid "Only a mission started with (goal) supports (goal :update)."))
       (let ((paused-p (eq (getf mission :status) ':paused)))
         (application-mission-start application (list :objective (first arguments)) :final-marker-p t)
         (application-mission-command application (if paused-p ':pause ':run))))
      (otherwise
       (application-mission-start application (list :objective request) :final-marker-p t)
       (application-mission-command application ':run))))
  nil)

(-> application--generation-items (application) list)
(defun application--generation-items (application)
  "Return picker items for retained generations, newest first."
  (loop for generation in (generation-list
                           (application-configuration application))
        collect (list :name     (generation-identifier generation)
                      :argument nil
                      :description
                      (format nil "~A~:[, incompatible~;~]"
                              (application--calendar-description
                               (generation-created-at generation))
                              (generation-compatible-p generation)))))

(-> application--pick-identifier
    (application &key (:title string) (:items list) (:usage string)
                 (:empty-notice (option string)) (:hint (or string list))
                 (:visible-count (integer 1))
                 (:initial-name (option string))
                 (:initial-value (or null string keyword))
                 (:search-p boolean) (:search-key function)
                 (:on-event (option function)))
    (or null string keyword))
(defun application--pick-identifier
    (application &key (title "select") items (usage "") empty-notice hint
                      (visible-count *terminal-ui-visible-completions*)
                      initial-name initial-value search-p
                      (search-key #'terminal-ui--picker-default-search-text)
                      on-event)
  "Pick an identifier from ITEMS, returning NIL on cancellation.

Signal a usage error on non-interactive terminals. If ITEMS is empty, present
EMPTY-NOTICE and return NIL. Forward HINT, VISIBLE-COUNT, INITIAL-NAME,
INITIAL-VALUE, SEARCH-P, SEARCH-KEY, and ON-EVENT to TERMINAL-UI-SELECT."
  (block nil
    (let ((ui (application-ui application)))
      (unless (terminal-interactive-p (terminal-ui-terminal ui))
        (error 'configuration-error :message usage))
      (unless items
        (when empty-notice
          (application-present application (application-hint-line empty-notice)))
        (return nil))
      (terminal-ui-select
       ui
       :title           title
       :items           items
       :hint            hint
       :visible-count   visible-count
       :initial-name    initial-name
       :initial-value   initial-value
       :search-p        search-p
       :search-key      search-key
       :on-event        on-event
       :resize-callback #'application-pending-terminal-size))))

(-> application--channel-remove-confirm-items (list) list)
(defun application--channel-remove-confirm-items (item)
  "Return the remove/keep confirmation items for channel ITEM."
  (list
   (list :name        "remove"
         :value       ':remove
         :argument    nil
         :description (format nil "delete ~A for good: its agent, its history and the channels nested in it"
                              (string-trim " " (getf item :name))))
   (list :name        "keep"
         :value       ':keep
         :argument    nil
         :description "keep this channel")))

(-> application--channel-items (application) list)
(defun application--channel-items (application)
  "Return picker items grouped by originating local repository, children under parents.

An item names the channel's agent, or else its title or identifier, and says
what the channel is about and whether a terminal session serves it. Resolve
presentation groups once before building items, using the sidebar's policy."
  (let* ((world   (handler-case (application-world application) (autolith-error nil nil)))
         (paths   (and world (world-channel-paths world)))
         (current (conversation-identifier (application-conversation application)))
         (served  (world-channel-served-identifiers (application-configuration application))))
    (when world
      (let* ((inbox  (world-inbox-channel world))
             (channels
               (application--channels-newest-first application world
                (remove-if #'channel-inbox-p (world-channels world))))
             (groups (world-repository-groups channels current))
             (remembered
               (handler-case
                   (world-repository-remember (application-configuration application) channels groups)
                 (autolith-error () channels))))
        (unless (eq remembered channels)
          (setf channels remembered
                groups (world-repository-groups channels current)))
        (labels ((item (channel depth group)
                   "Return the picker item of CHANNEL nested DEPTH deep in GROUP."
                   (let* ((identifier (channel-identifier channel))
                          (agent
                           (find ':agent (world-channel-residents world identifier) :key
                                 #'participant-kind))
                          (name
                           (or (and agent (participant-name agent)) (channel-title channel)
                               (conversation-identifier-display identifier))))
                     (list :name
                           (format nil "~A~A" (make-string (* 2 depth) :initial-element #\ ) name)
                           :value identifier :argument nil :group group :description
                           (format nil "~@[~A · ~]~A~:[~; · open~]"
                                   (and agent (channel-title channel))
                                   (world-channel-path world channel :paths paths)
                                   (member identifier served :test #'equal)))))

                 (items (node depth group)
                   "Return the items of NODE and the channels nested in it."
                   (cons (item (first node) depth group)
                         (loop for child in (rest node)
                               append (items child (1+ depth) group)))))
          (append
           (when (and inbox (not (string= (channel-identifier inbox) current)))
             (list
              (list :name *world-inbox-designator* :value (channel-identifier inbox) :argument nil
                    :description "messages for you")))
           (loop for repository in groups
                 for workspace = (world-repository-group-directory repository)
                 for members = (world-repository-group-channels repository)
                 for group = (format nil "~A · ~A" (world-workspace-leaf workspace)
                                     (application--abbreviated-directory workspace))
                 append (loop for node in (world-channel-forest members)
                              append (items node 0 group)))))))))

(-> application--channels-newest-first (application world list) list)
(defun application--channels-newest-first (application world channels)
  "Return CHANNELS with top-level histories newest first and nested channels by creation.

A top-level channel without durable history sorts by its creation time.
Only top-level conversation files are consulted; locating a child's history
searches every task directory."
  (let ((configuration (application-configuration application)))
    (mapcar #'rest
            (stable-sort
             (mapcar (lambda (channel)
                       (cons (if (world-channel-top-level-p world channel)
                                 (let ((date (conversation-storage-write-date
                                              (conversation-pathname-for-id
                                               configuration (channel-identifier channel)))))
                                   (if (plusp date) date (channel-created-at channel)))
                                 (channel-created-at channel))
                             channel))
                     channels)
             #'> :key #'first))))

(-> application--pick-channel (application) (option string))
(defun application--pick-channel (application)
  "Pick a channel to switch to, with removal of a channel by the Delete key and a confirmation step."
  (let* ((browse-title "switch channel")
         (browse-hint  (list (terminal-span ':legend-accent "Enter")
                             (terminal-span ':legend-dim " select ")
                             (terminal-span ':legend-accent "Delete")
                             (terminal-span ':legend-dim " remove ")
                             (terminal-span ':legend-accent "Escape")
                             (terminal-span ':legend-dim " cancel")))
         (confirm-hint (list (terminal-span ':legend-accent "Enter")
                             (terminal-span ':legend-dim " confirm ")
                             (terminal-span ':legend-accent "Escape")
                             (terminal-span ':legend-dim " return")))
         (mode         ':browse)
         (pending      nil)
         (browse-items (application--channel-items application))
         (current-identifier
           (conversation-identifier
            (application-conversation application))))
    (labels ((refresh-items ()
               "Reload browse items from disk."
               (setf browse-items
                     (application--channel-items application)))

             (selected-item (selector)
               "Return SELECTOR's currently highlighted item, or NIL."
               (let ((selection (selector-selection selector))
                     (items     (selector-items selector)))
                 (and selection
                      items
                      (<= 0 selection (1- (length items)))
                      (nth selection items))))

             (browse-action (&optional selected-value)
               "Return the picker action that restores the browse list."
               (if browse-items
                   (list ':replace browse-title browse-items browse-hint
                         selected-value)
                   (list ':cancel)))

             (on-event (event selector)
               "Handle delete confirmation inside the channel picker."
               (ecase mode
                 (:browse
                  (cond
                    ((eq event ':delete)
                     (let ((item (selected-item selector)))
                       (cond
                         ((null item)
                          ':continue)
                         ((string= (getf item :value) current-identifier)
                          (application-present
                           application
                           (application-failure-line "this terminal shows that channel; switch away to remove it"))
                          ':continue)
                         (t
                          (setf mode ':confirm
                                pending item)
                          (list
                           ':replace
                           (format nil "remove ~A?" (string-trim " " (getf item :name)))
                           (application--channel-remove-confirm-items item)
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
                     (let ((choice         (selected-item selector))
                           (selected-value (and pending (getf pending :value))))
                       (cond
                         ((and choice
                               (eq (getf choice :value) ':remove)
                               pending)
                           ;; End the modal before filesystem cleanup. Clinedi calls
                           ;; this callback under the presentation lock.
                          (setf mode ':remove)
                          (list ':accept selected-value))
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
      (loop
        (let ((selected
                (application--pick-identifier
                 application
                 :title         browse-title
                 :items         browse-items
                 :hint          browse-hint
                 :initial-value current-identifier
                 :on-event      #'on-event
                 :usage         "Usage: (open \"ID\")"
                 :search-p      t
                 :empty-notice  "no other channels exist")))
          (unless (eq mode ':remove)
            (return selected))
          (let* ((ui                (application-ui application))
                 (previous-activity (terminal-ui-local-activity ui)))
            (unwind-protect
                 (progn
                   (application-set-local-activity application "removing channel")
                   (handler-case
                       (application--delete-channel application (getf pending :value)
                                                    :tool-name "(open)" :allow-unrecorded-p nil)
                     (autolith-error (condition)
                       (application-present application
                                            (application-failure-line (autolith-error-message condition))))))
              (application-set-local-activity application previous-activity)))
          (setf mode ':browse pending nil)
          (refresh-items))))))

(-> application--agent-items (application) list)
(defun application--agent-items (application)
  "Return picker items for active agents by name and channel."
  (let ((world (application-world application)))
    (loop for participant in (world-participants world)
          for residence = (participant-residence participant)
          when (and residence (eq (participant-kind participant) ':agent))
            collect (let ((channel (world-channel-find world residence)))
                      (list :name        (participant-name participant)
                            :value       (cond
                                           ((handler-case
                                                (eq participant (world-participant-designated
                                                                 world (participant-name participant)))
                                              (world-error () nil))
                                            (participant-name participant))
                                           ((and channel (= 1 (count ':agent
                                                                    (world-channel-residents world residence)
                                                                    :key #'participant-kind)))
                                            (world-channel-path world channel))
                                           (t
                                            (conversation-identifier-display (participant-identifier participant))))
                            :argument    nil
                            :description (format nil "in ~@[~A · ~]~A"
                                                 (channel-title channel)
                                                 (world-channel-path world channel)))))))

(-> application--pick-agent (application &key (:title string) (:usage string)) (option string))
(defun application--pick-agent (application &key (title "pick an agent") (usage ""))
  "Pick an agent by name, or return NIL when cancelled; USAGE explains the call where no picker can show."
  (application--pick-identifier application
                                :title        title
                                :items        (application--agent-items application)
                                :hint         "type to search, enter selects, esc cancels"
                                :search-p     t
                                :usage        usage
                                :empty-notice "no agents exist"))

(-> application--prefill-call (application string string) null)
(defun application--prefill-call (application operator argument)
  "Put (OPERATOR \"ARGUMENT\" \"\") in the input with the cursor in its empty string, for the person to finish."
  (let ((ui (application-ui application)))
    (terminal-ui-set-input ui (format nil "(~A ~S \"\")" operator argument))
    (dotimes (index 2)
      (terminal-ui-edit-input ui ':left)))
  nil)

(-> application--pick-model (application &key (:provider (option string))) (values (option string) (option string)))
(defun application--pick-model (application &key provider)
  "Return the picked exact model and provider as two values, or two NILs."
  (let* ((configuration (application-configuration application))
         (registration  (configuration-model-registration configuration))
         (items         (application--model-items application :provider provider))
         (selection
           (application--pick-identifier
            application :title "pick the model" :items items :visible-count 15
            :initial-name (and registration
                               (provider-model-selection-id (config :model configuration) registration))
            :search-p t :usage "Usage: (model \"MODEL\" :provider \"PROVIDER\")"
            :empty-notice "No registered provider models exist."))
         (item          (find selection items :key (lambda (item) (getf item :name)) :test #'equal)))
    (values (getf item :model) (getf item :provider))))

(-> application--pick-reasoning-effort (application) (option string))
(defun application--pick-reasoning-effort (application)
  "Prompt for one supported reasoning effort and return the selected name."
  (let ((configuration (application-configuration application)))
    (application--pick-identifier
     application
     :title        "pick the reasoning effort"
     :items        (application--effort-items application)
     :initial-name (config :reasoning-effort configuration)
     :search-p     t
     :usage        "Usage: (effort \"LEVEL\")"
     :empty-notice "No supported reasoning efforts exist.")))

(-> application--project-adaptation-offer-items () list)
(defun application--project-adaptation-offer-items ()
  "Return the AUTOLITH.org creation choices for an eligible resumed project."
  (list
   (list :name        "create"
         :value       ':create
         :argument    nil
         :description "create a documented project adaptation ledger")
   (list :name        "not-now"
         :value       ':not-now
         :argument    nil
         :description "ask again after five days")
   (list :name        "never"
         :value       ':never
         :argument    nil
         :description "never ask again for this repository or path")))

(-> application--project-adaptation-offer-idle-locked-p
    (application application-input-controller)
    boolean)
(defun application--project-adaptation-offer-idle-locked-p (application controller)
  "Check for attached idle while CONTROLLER's lock is held."
  (and (deque-empty-p (application-input-controller-initial-work-items controller))
       (deque-empty-p (application-input-controller-work-items controller))
       (deque-empty-p (application-input-controller-steering-items controller))
       (deque-empty-p (application-input-controller-steering-in-flight-items controller))
       (deque-empty-p (application-input-controller-pending-apply-items controller))
       (null (application-input-controller-follow-up-edit-index controller))
       (not (application-input-controller-turn-cancellation-p controller))
       (not (application-input-controller-stopping-p controller))
       (not (application-input-controller-failure controller))
       (not (application-input-controller-queued-work-paused-p controller))
       (not (application-world-terminal-paused-p application))
       (not (application-world-held-p application))
       (not (application-job-completions-awaited-p application))
       (application-clients-attached-p application)))

(-> application--project-adaptation-offer-idle-p
    (application application-input-controller)
    boolean)
(defun application--project-adaptation-offer-idle-p (application controller)
  "Return whether CONTROLLER is attached and has no blocking work."
  (with-lock-held ((application-input-controller-lock controller))
    (application--project-adaptation-offer-idle-locked-p application controller)))

(-> application-maybe-offer-project-adaptation (application) null)
(defun application-maybe-offer-project-adaptation (application)
  "Offer voluntary AUTOLITH.org creation after qualifying attached work."
  (let* ((configuration (application-configuration application))
         (ui            (application-ui application))
         (controller    (application-input-controller application))
         (project-root
           (workspace-project-root
            (config :working-directory configuration))))
    (when controller
      (unless (application-clients-attached-p application)
        (return-from application-maybe-offer-project-adaptation nil))
      (unless (application--project-adaptation-offer-idle-p application controller)
        (with-lock-held ((application-input-controller-lock controller))
          (setf (application-input-controller-project-adaptation-offer-p controller) t))
        (return-from application-maybe-offer-project-adaptation nil)))
    (when (terminal-interactive-p (terminal-ui-terminal ui))
      (handler-case
          (when (and (project-adaptation-offer-due-p
                      configuration project-root)
                     (project-adaptation-resume-qualifies-p
                      configuration
                      (application-conversation application)))
            (if (or (null controller)
                    (application--project-adaptation-offer-idle-p application controller))
                (progn
                  (project-adaptation-offer-defer configuration project-root)
                  (application-present
                   application
                   "AUTOLITH.org is an optional record of project-specific Autolith adaptations.")
                  (let ((choice
                          (application--pick-identifier
                           application
                           :title "create AUTOLITH.org?"
                           :items (application--project-adaptation-offer-items)
                           :usage "Choose create, not-now, or never.")))
                    (case choice
                      (:create
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
                      (:never
                       (project-adaptation-offer-refuse configuration project-root)
                       (application-present
                        application
                        "AUTOLITH.org offers are disabled permanently for this path."))
                      (:not-now
                       (application-present
                        application
                        "The AUTOLITH.org offer is deferred for five days.")))))
                (when (and controller
                           (application-clients-attached-p application))
                  (with-lock-held ((application-input-controller-lock controller))
                    (setf (application-input-controller-project-adaptation-offer-p controller) t)))))
        (project-adaptation-error (condition)
          (application-handle-expected-error application condition))))
    nil))


;;;; -- Working Directory Command --

(-> application-working-directory-command (application (or pathname string null)) null)
(defun application-working-directory-command (application location)
  "Show APPLICATION's workspace, or change it to LOCATION."
  (when (and (stringp location) (not (non-empty-string-p location)))
    (error 'configuration-error :message "A workspace pathname must not be empty."))
  (let ((directory (if location
                       (application-set-working-directory application location)
                       (config :working-directory (application-configuration application)))))
    (application-present
     application
     (format nil "Working directory~A ~A" (if location " is now" ":") (namestring directory))))
  nil)


;;;; -- Authentication and Checkpoint Commands --

(-> application--authentication-provider-items (application) list)
(defun application--authentication-provider-items (application)
  "Return all registered providers, marking the selected provider."
  (let ((current-provider (configuration-model-provider-name
                           (application-configuration application))))
    (loop for registration in (provider-registrations)
          for name = (provider-registration-name registration)
          collect (list :name     name
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
   :items             (application--authentication-provider-items application)
   :visible-count     (application--authentication-provider-visible-count application)
   :initial-name      (configuration-model-provider-name (application-configuration application))
   :search-p t :usage "Usage: (auth \"PROVIDER\")"
   :empty-notice      "No registered providers exist."))

(-> application--authentication-provider (application string) model-provider)
(defun application--authentication-provider (application provider-name)
  "Return APPLICATION's named provider for authentication."
  (provider-authentication-provider
   (application-configuration application)
   provider-name
   :reasoning-summaries-p (application-reasoning-traces-p application)))

(defclass application-authentication-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((terminal
    :initarg       :terminal
    :reader        application-authentication-input-stream-terminal
    :type          terminal
    :documentation "The terminal whose client supplies hidden authentication input.")
   (pending
    :initform      ""
    :accessor      application-authentication-input-stream-pending
    :type          string
    :documentation "The completed input line still being consumed.")
   (position
    :initform      0
    :accessor      application-authentication-input-stream-position
    :type          (integer 0)
    :documentation "The next character index in PENDING."))
  (:documentation "A hidden line stream consuming the events of one terminal's client."))

(-> application-authentication-input-stream--fill
    (application-authentication-input-stream)
    boolean)
(defun application-authentication-input-stream--fill (stream)
  "Read one hidden world-terminal line into STREAM and report success."
  (let ((terminal   (application-authentication-input-stream-terminal stream))
        (characters (make-array 64
                                :element-type 'character
                                :adjustable   t
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
           ;; Nobody is left at the channel's clients to type the rest.
           (unless (terminal-interactive-p terminal)
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
  "Return true when STREAM has buffered or queued world-terminal input."
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
  (terminal-ui-call-with-blocked-status
   ui ':auth
   (lambda ()
     (cond
       ;; A hosted channel has no screen to give the provider: its output
       ;; shows in the transcript, which a page such as the tour gives way
       ;; to meanwhile, and its input comes from the client.
       ((typep ui 'channel-presentation)
        (let ((suspended-p (terminal-ui-live-output-suspended-p ui)))
          (unwind-protect
               (progn
                 (with-terminal-ui-locked (ui)
                   (setf (terminal-ui-live-output-suspended-p ui) nil)
                   (terminal-ui--paint-live ui))
                 (funcall function))
            (with-terminal-ui-locked (ui)
              (setf (terminal-ui-live-output-suspended-p ui) suspended-p)))))
       ((and (terminal-ui-fullscreen-p ui)
             (fullscreen-terminal-ui-active-p ui))
        (let ((terminal            (terminal-ui-terminal ui))
              (output-mode         nil)
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
                    (application--authentication-ui-resume ui))))))))
       (t
        (unwind-protect
             (progn
               (if stop-ui-p
                   (terminal-ui-stop ui)
                   (application--authentication-ui-suspend ui))
               (funcall function))
          (if stop-ui-p
              (terminal-ui-start ui)
              (application--authentication-ui-resume ui))))))))

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

(defmethod terminal-authentication-streams ((terminal presentation-terminal))
  "Read a hosted channel's client events as hidden input and show output in its transcript."
  (values
   (make-instance 'application-authentication-input-stream
                  :terminal terminal)
   (make-instance 'presentation-output-stream
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
            (let ((*standard-input*                input)
                  (*standard-output*               output)
                  (*api-key-input-echo-disabled-p* input-echo-disabled-p)
                  (*api-key-input-file-descriptor* input-file-descriptor)
                  (*api-key-output-styled-p*
                    (terminal-styled-p (terminal-ui-terminal ui))))
              (setf message
                    (provider-authenticate-with-method
                     provider method :stream output :open-browser-p t))))))))
    (application-present application (application-success-line message))
    (when-let ((model (provider-authenticated-model
                       (application-configuration application) provider)))
      (application-set-model application model
                             :provider (provider-registration-name
                                        (model-provider-registration provider)))
      (application-present
       application
       (format nil "Active provider: ~A; model: ~A."
               (configuration-model-provider-name
                (application-configuration application)) model))))
  nil)

(-> application-checkpoint (application &key (:allow-dirty-p boolean)) null)
(defun application-checkpoint (application &key allow-dirty-p)
  "Begin a retained generation for APPLICATION.

ALLOW-DIRTY-P permits uncommitted source changes and marks the generation dirty."
  (application-set-activity application "checking source before checkpoint")
  (unwind-protect
       (let* ((configuration (application-configuration application))
              (backend
                (checkpoint-backend-create
                 configuration (application-worker application)
                 :tool-registry (application-tool-registry application)
                 :allow-dirty-p allow-dirty-p))
              (generation    (if (application-hosted-p application)
                                 nil
                                 (checkpoint-create backend))))
         (cond
           ((application-hosted-p application)
            (application-present application
                                 (application-notice-line
                                  (world-server-checkpoint application backend))))
           ((platform-supports-p *platform* ':restartable-image-saver)
            (setf (application-checkpoint-restart-request application)
                  (cons backend generation))
            (application-present
             application
             (application-notice-line
              (format nil "checkpoint ~A scheduled: Autolith will restart after this command, then resume this session"
                      (generation-identifier generation)))))
           (t
            (application-present
             application
             (application-notice-line
              (format nil "checkpoint ~A is being published by process ~D"
                      (generation-identifier generation)
                      (generation-coordinator-pid generation)))))))
    (application-set-activity application nil))
  nil)


;;;; -- Command Permissions --

(-> application--permission-mode-name (keyword) string)
(defun application--permission-mode-name (mode)
  "Return a user-facing description of command permission MODE."
  (ecase mode
    (:ask
     "ask before unrecognized commands")
    (:auto
     "classify automatically without prompting")
    (:sandboxed
     "allow commands inside the workspace sandbox")
    (:full-access
     "let commands run with full user privileges")))

(-> application--permission-mode-catalogue () list)
(defun application--permission-mode-catalogue ()
  "Return available (MODE COMMAND-CHOICE TERMINAL-DESCRIPTION) rows in picker order."
  (remove-if (lambda (row)
               (and (eq (first row) ':sandboxed)
                    (not (application--command-sandbox-available-p))))
             '((:ask :ask "prompt unless this exact command was saved")
               (:auto :auto "classify commands automatically without prompting")
               (:sandboxed :sandbox "allow commands inside the workspace sandbox")
               (:full-access :full "run commands with full user privileges"))))

(-> application--permission-mode-items (application) list)
(defun application--permission-mode-items (application)
  "Return available session command permission choices for APPLICATION."
  (let ((current (application-permission-mode application)))
    (loop for (mode choice description) in (application--permission-mode-catalogue)
          collect (list :name        (string-downcase (symbol-name choice))
                        :argument    nil
                        :description (if (eq mode current)
                                         (concatenate 'string "current; " description)
                                         description)))))

(-> application-saved-permissions-entry (application) list)
(defun application-saved-permissions-entry (application)
  "Return saved approvals with request, domain, and pathname presentation roles."
  (if-let ((rules (permission-state-rules (application-permission-state application))))
      (append
       (list (terminal-span ':brand (format nil "# saved exact approvals~%")))
       (loop for rule in rules
             for kind = (command-permission-kind rule)
             append (application--entry-fields
                     (list (list (if (eq kind ':command) "command" "request")
                                 ':code (command-permission-command rule))
                           (list "kind" ':dim (string-downcase (symbol-name kind)))
                           (list "in" ':code
                                 (application--abbreviated-directory
                                  (command-permission-directory rule)))))))
      (application-hint-line "no exact approvals are saved")))

(-> application--permission-mode-entry (keyword string) list)
(defun application--permission-mode-entry (mode description)
  "Return a permission MODE receipt, emphasizing full user authority as a warning."
  (list (terminal-span (if (eq mode ':full-access) ':notice ':success) "# permissions ")
        (terminal-span ':code (string-downcase (symbol-name mode)))
        (terminal-span ':dim
                       (format nil "~%  ~A"
                               (string-right-trim "." (string-downcase description :end (min 1 (length description))))))))

(-> application-permissions-command (application t) null)
(defun application-permissions-command (application argument)
  "Apply ARGUMENT to APPLICATION's channel command permissions."
  (let* ((choice    (and argument
                        (application--control-choice argument '(:ask :auto :sandbox :full :list :clear))))
         (catalogue (and choice (application--permission-mode-catalogue)))
         (mode      (first (find choice catalogue :key #'second))))
    (when (and (eq choice ':sandbox) (not mode))
      (error 'configuration-error :message (application--command-sandbox-unavailable-message)))
    (cond
      ((null choice)
       nil)
      (mode
       (application-set-channel-permission-mode application mode)
       (application-present
        application
        (application--permission-mode-entry
         mode
         (ecase mode
           (:ask
            "Commands will ask before running unless the exact command was saved.")
           (:auto
            (concatenate 'string
                         (if (assoc ':sandboxed catalogue)
                             "Commands will be classified automatically as sandboxed, full access, or denied."
                             "Commands will be classified automatically as full access or denied.")
                         " The status bar shows judging during classification; each decision is attached to its operation."))
           (:sandboxed
            "Commands in this channel may run inside the workspace sandbox.")
           (:full-access
            "Commands in this channel may run with your full user privileges, including after reopening.")))))
      ((eq choice ':list)
       (application-present application (application-saved-permissions-entry application)))
      ((eq choice ':clear)
       (permissions-clear
        (application-configuration application)
        (application-permission-state application))
       (application-present application (application-success-line "saved command approvals cleared")))
      (t
       (error 'configuration-error
              :message "Usage: (permissions), or (permissions :MODE) with :ask, :auto, :sandbox, :full, :list, or :clear.")))
    (when mode
      (application-clients-publish-state application)))
  nil)

(-> application-set-hurry-up (application boolean) null)
(defun application-set-hurry-up (application enabled-p)
  "Apply ENABLED-P to APPLICATION's prompt and agent."
  (setf (application-hurry-up-p application) enabled-p)
  (let ((agent (and (slot-boundp application 'agent)
                    (application-agent application))))
    (when (typep agent 'agent)
      (setf (agent-hurry-up-p agent) enabled-p)))
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (when (typep ui 'terminal-ui)
      (terminal-ui-set-notice
       ui
       (if enabled-p
           "time is of the essence; acting directly with at most two child agents"
           "hurry-up mode is off")
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

(-> application--setting-configuration (application keyword) configuration)
(defun application--setting-configuration (application name)
  "Return the configuration that owns the effective value of NAME."
  (let ((configuration (application-configuration application)))
    (if (member name '(:telemetry-enabled-p :telemetry-diagnostics-p))
        (telemetry-consent-configuration configuration)
        configuration)))

(-> application--setting-state (application setting) (values t (option keyword) list boolean))
(defun application--setting-state (application setting)
  "Return SETTING's effective value, source, options and adjustability."
  (let* ((name          (setting-name setting))
         (configuration (application--setting-configuration application name)))
    (values (configuration-setting-value configuration setting)
            (configuration-setting-source configuration name)
            (setting-options setting configuration)
            (application--setting-adjustable-p setting))))

(-> application--setting-source-text ((option keyword) setting) string)
(defun application--setting-source-text (source setting)
  "Render the effective SOURCE of SETTING for the terminal."
  (if (eq (setting-scope setting) ':derived)
      "derived"
      (case source
        (:environment
         "environment")
        (:override
         "explicit")
        (:durable
         "saved")
        (:session
         "this session")
        (t
         "default"))))

(-> application-set-terminal-theme (application keyword) null)
(defun application-set-terminal-theme (application name)
  "Persist NAME before installing its styles and repainting the running terminal."
  (terminal-theme-find name)
  (setf (config :terminal-theme (application-configuration application)) name)
  (if (and (slot-boundp application 'ui) (application-ui application))
      (terminal-ui-set-theme (application-ui application) name)
      (terminal-theme-install name))
  nil)

(-> application-apply-setting (application keyword t) null)
(defun application-apply-setting (application name value)
  "Set NAME to VALUE for APPLICATION and apply the runtime side effects.

Settings with runtime consequences, such as provider reasoning summaries,
use the same setters in the settings page and commands."
  (let* ((configuration (application--setting-configuration application name))
         (setting       (configuration-setting configuration name)))
    (setting-validate setting value configuration)
    (unless (application--setting-adjustable-p setting)
      (error 'configuration-error
             :message (format nil "~A is set at startup by the command line, the environment, or a default and cannot change in a running session."
                              (setting-label setting))))
    (case name
      (:model
       (application-set-model application value))
      (:reasoning-effort
       (application-set-reasoning-effort application value))
      (:codex-fast-mode-p
       (application-set-codex-fast-mode application value))
      (:reasoning-traces-p
       (application-set-reasoning-traces application value))
      (:turn-timestamps-p
       (application-set-turn-timestamps application value))
      (:cache-miss-notices-p
       (application-set-cache-miss-notices application value))
      ((:idle-compaction-p :prompt-cache-retention-seconds)
       (setf (config name configuration) value)
       (application-idle-compaction-wake application))
      ((:knowledge-mode :diagnosis-p)
       (setf (config name configuration) value)
       ;; The model's resource operations and documentation depend on the
       ;; mode, so the session's tools are rebuilt for the next request.
       (application-reload-mcp application))
      (:hurry-up-p
       (application-set-hurry-up application value))
      (:terminal-theme
       (application-set-terminal-theme application value))
      (:prompt-padding
       (setf (config name configuration) value)
       (when (application-ui application)
         (terminal-ui-set-prompt-padding (application-ui application) value)))
      (:permission-mode
       (setf (config :permission-mode configuration) value)
       (unless (and (slot-boundp application 'conversation)
                    (application--saved-permission-mode configuration
                                                        (application-conversation application)))
         (setf (application-permission-mode application) (or value ':ask))))
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
        (groups  nil))
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
          collect (multiple-value-bind (value source options adjustable-p)
                      (application--setting-state application setting)
                    (declare (ignore options))
                    (list :name        (setting-label setting)
                          :value       (string-downcase (symbol-name (setting-name setting)))
                          :argument    nil
                          :group       (string-downcase (symbol-name (setting-group setting)))
                          :detail      (setting-render-value setting value)
                          :description (format nil "~A~:[ · read-only~;~]"
                                               (application--setting-source-text source setting)
                                               adjustable-p))))))

(-> application--setting-option-items (setting t list) list)
(defun application--setting-option-items (setting current options)
  "Render OPTIONS for SETTING, marking the CURRENT effective value."
  (loop for option in options
        collect (list :name        (setting-render-value setting option)
                      :argument    nil
                      :description (if (equal option current) "current" ""))))

(-> application-settings-entry (application) list)
(defun application-settings-entry (application)
  "Return every visible setting by group with its value, source, and meaning."
  (let ((configuration (application-configuration application))
        (group         nil))
    (cons
     (terminal-span ':brand "# settings")
     (loop for setting in (application--grouped-settings configuration)
           unless (eq group (setting-group setting))
             append (progn
                      (setf group (setting-group setting))
                      (list (terminal-span ':notice (format nil "~2%  ~(~A~)" group))))
            append (multiple-value-bind (value source)
                       (application--setting-state application setting)
                     (list
                      (terminal-span ':code (format nil "~%    ~(~A~)" (setting-name setting)))
                      (terminal-span ':dim " = ")
                      (terminal-span ':plain (setting-render-value setting value))
                      (terminal-span ':dim
                                     (format nil " (~A)~%      ~A"
                                             (application--setting-source-text source setting)
                                             (setting-documentation setting)))))))))

(-> application--setting-value-entry (application keyword) list)
(defun application--setting-value-entry (application name)
  "Return the setting getter and its effective value as one highlighted Lisp row."
  (let ((setting (configuration-setting (application-configuration application) name)))
    (append
     (application-lisp--source-entry (format nil "(config :~(~A~))" name))
     (list (terminal-span ':success " => "))
     (application-lisp--value-spans
      (application-lisp-render-value
       (application--setting-state application setting)
       :columns 16000)))))

(-> application--present-setting-value (application keyword) null)
(defun application--present-setting-value (application name)
  "Present a compact setting row with its getter as the command heading."
  (let ((entry (application--setting-value-entry application name)))
    (setf *application-command-presentation-pending-p* nil)
    (application-present application entry :group ':lisp-result))
  nil)

(-> application--settings-page (application) null)
(defun application--settings-page (application)
  "Let the user browse settings and change adjustable ones until they cancel."
  (loop
    (let ((chosen (application--pick-identifier
                   application
                   :title         "settings"
                   :items         (application--setting-items application)
                   :visible-count 15
                   :search-p      t
                   :usage         "Usage: (settings), (settings :NAME), or (settings :NAME VALUE)"
                   :empty-notice  "No settings are registered.")))
      (unless chosen
        (return))
      (let* ((name    (application--setting-name chosen))
             (setting (configuration-setting (application-configuration application) name)))
        (multiple-value-bind (value source options adjustable-p)
            (application--setting-state application setting)
          (let ((rendered (setting-render-value setting value)))
            (cond
              ((not adjustable-p)
               (application-present
                application
                (format nil "~A is ~A (~A). It is read-only in a running session."
                        (setting-label setting) rendered
                        (application--setting-source-text source setting))))
              (options
               (when-let ((option (application--pick-identifier
                                   application
                                   :title        (setting-label setting)
                                   :items        (application--setting-option-items setting value options)
                                   :initial-name rendered
                                   :search-p     t
                                   :usage        "Usage: (settings :NAME VALUE)"
                                   :empty-notice "This setting offers no choices.")))
                 (application-apply-setting
                  application name
                  (setting-parse setting option (application--setting-configuration application name)))
                 (application--present-setting-value application name)))
              (t
               (application-present
                application
                (format nil "~A is ~A. Change it with (settings :~(~A~) VALUE)."
                        (setting-label setting) rendered name)))))))))
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
     (application-present application (application-settings-entry application)))
    ((not value-supplied-p)
     (let* ((key     (application--setting-name name))
            (setting (configuration-setting (application-configuration application) key)))
       (multiple-value-bind (value source)
           (application--setting-state application setting)
         (application-present
          application
          (format nil "~A is ~A (~A). ~A"
                  (setting-label setting)
                  (setting-render-value setting value)
                  (application--setting-source-text source setting)
                  (setting-documentation setting))))))
    (t
     (let ((key (application--setting-name name)))
       (application-apply-setting application key value)
       (application--present-setting-value application key))))
  nil)

(define-application-command application--builtin-settings-command
    (:name              "settings"
     :help-topic        :display
     :help-order        2
     :argument          "[NAME [VALUE]]"
     :description       "browse and change settings"
     :tip               "Set a value with `(settings :onboarding-p nil)` or inspect one with `(settings :terminal-theme)`."
     :busy-behavior     :apply
     :terminal-behavior :exclusive-without-arguments)
    (application &optional name (value nil value-supplied-p))
  (if value-supplied-p
      (application-settings-command application name value)
      (application-settings-command application name))
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
    (:name              "slynk"
     :description       "serve this image to sly"
     :tip               "Use `(slynk)` to start Slynk and show its port for M-x sly-connect, or `(slynk :stop)` to close it."
     :busy-behavior     :apply
     :terminal-behavior :shared
     :static-options    (:start :stop :status))
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
         (method        (installation-provenance-method provenance)))
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
           (let* ((state      (update-state-load configuration))
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
            (application-failure-line
             "could not check the release service; the installed release is unchanged"))))))
  nil)


;;;; -- Built-in Interactive Commands --

(define-application-command application--builtin-help-command
    (:name              "help"
     :argument          "[TOPIC]"
     :description       "browse help or look up a command"
     :tip               "Use `(help :workspace)` for a section or `(help 'open)` for a command."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application &optional topic)
  (application-present
   application
   (application--markdown-body application (application-operation-help application topic)))
  ':continue)

(define-application-command application--builtin-new-command
    (:name              "new"
     :help-topic        :channels
     :help-order        2
     :argument          nil
     :description       "open a new channel in this workspace"
     :tip               "opens a new channel and moves this terminal there; the channel you leave keeps running."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application)
  (let* ((configuration (application-configuration application))
         (ui            (application-ui application))
         (terminal      (terminal-ui-terminal ui)))
    (if (application-presentation application)
        ;; A hosted channel keeps its conversation and its agent; the new
        ;; conversation is a channel of its own, and the terminal goes there.
        (let* ((presentation
                 (with-terminal-ui-locked (ui)
                   (list :rows     (terminal-rows terminal)
                         :columns  (terminal-columns terminal)
                         :styled-p (not (null (terminal-styled-p terminal))))))
               (identifier
                 (getf (rest (world-server-open
                              (application-world-server application)
                              (append
                               (list :directory (namestring (config :working-directory configuration))
                                     :new-p     t
                                     :switch-p  t)
                               presentation)))
                       :session-id))
               (display (conversation-identifier-display identifier)))
          (unless (application-client-tell application ':switch :session-id identifier)
            (application-present
             application
             (application-notice-line (format nil "new channel ~A opened; (open ~S) goes there"
                                              display display)))))
        (progn
          ;; A standalone session replaces its own conversation instead of
          ;; changing the client's attachment. Never replace it during a turn.
          (when (and (application-input-controller application)
                     (application-input-controller-turn-active-p
                      (application-input-controller application)))
            (error 'configuration-error
                   :message "A standalone session can start a new conversation only when its turn has finished."))
          (application-install-conversation application (conversation-create configuration))
          (terminal-ui-reset-transcript (application-ui application))
          (application-present
           application
           (application-notice-line
            (format nil "started conversation ~A"
                    (conversation-identifier-display
                     (conversation-identifier (application-conversation application)))))))))
  ':continue)

(-> application--switch-target (application string) string)
(defun application--switch-target (application designator)
  "Return the identifier of the channel DESIGNATOR names, among the world's channels, the running sessions or the saved conversations.

A channel opened moments ago has no durable record and so no world channel
yet, but its session is published, so the running sessions are searched when
the world knows no such channel. A conversation saved since the world server
last started is not in its world either, so the saved conversations come last."
  (let* ((configuration (application-configuration application))
         (channel       (world-channel-designated (world-load configuration) configuration designator
                                                  :error-p nil)))
    (or (and channel (channel-identifier channel))
        (handler-case
            (world-terminal--record-session-id (rest (world-terminal--find-record configuration designator)))
          (world-terminal-error () nil))
        (handler-case (main-resolve-conversation configuration designator)
          (configuration-error (condition)
            (error 'world-error :message (autolith-error-message condition)
                   :pathname nil :operation ':resolve :identifier designator))))))

(-> application--channel-or-ask (application string &key (:title string) (:usage string)) (option string))
(defun application--channel-or-ask (application designator &key title usage)
  "Return the identifier of the channel DESIGNATOR names, or NIL once the person has been told why there is none.

A DESIGNATOR that matches several channels opens a picker of just those at an
interactive terminal, titled TITLE, and lists them otherwise; one that matches
nothing is said in one line. Neither reaches the debugger: typing part of an
identifier is no program error. USAGE explains the call where no picker can
show."
  (handler-case (application--switch-target application designator)
    (world-channel-ambiguous (condition)
      (application--pick-among application designator (world-channel-ambiguous-channels condition)
                               :title title
                               :usage usage))
    (world-error (condition)
      (application-present application
                           (application-failure-line (autolith-error-message condition)))
      nil)))

(-> application--pick-among (application string list &key (:title string) (:usage string)) (option string))
(defun application--pick-among (application designator channels &key title usage)
  "Let the person pick one of CHANNELS, which DESIGNATOR matches alike, or list them when no picker can show."
  (let ((identifiers (mapcar #'channel-identifier channels)))
    (if (terminal-interactive-p (terminal-ui-terminal (application-ui application)))
        (application--pick-identifier
         application
         :title        (format nil "~A ~S matches ~D channels" title designator (length channels))
         :items        (remove-if-not (lambda (item)
                                        (member (getf item :value) identifiers :test #'equal))
                                      (application--channel-items application))
         :hint         "enter selects, esc cancels"
         :search-p     t
         :usage        usage
         :empty-notice "none of them can be opened")
        (progn
          (application-present
           application
           (append (application-failure-line
                    (format nil "~S matches ~D channels; use an exact channel path:"
                            designator (length channels)))
                   (loop for channel in channels
                         append (list (terminal-span ':plain (format nil "~%  "))
                                      (terminal-span ':code
                                       (world-channel-path (application-world application) channel))
                                      (terminal-span ':dim (format nil "~@[  ~A~]" (channel-title channel)))))))
          nil))))

(define-application-command application--builtin-open-command
    (:name              "open"
     :help-topic        :channels
     :help-order        1
     :argument          "[TARGET]"
     :description       "move this terminal to another channel"
     :tip               "takes an agent name, @name, exact channel path or unique ID fragment; picks a channel without arguments. The channel you leave keeps running."
     :busy-behavior     :execute
     :terminal-behavior :exclusive-without-arguments)
    (application &optional (designator nil designator-p))
  (let* ((designator (if designator-p
                         designator
                         (and *application-command-interactive-p*
                              (application--pick-channel application))))
         (identifier (and designator
                          (application--channel-or-ask application (princ-to-string designator)
                                                       :title "open which channel?"
                                                       :usage "Usage: (open \"NAME-OR-PATH\")")))
         (display    (and identifier (conversation-identifier-display identifier)))
         (current    (and identifier
                          (conversation-identifier (application-conversation application))))
         (controller (and identifier (application-input-controller application))))
    (cond
      ((null identifier)
       nil)
      ((string= identifier current)
       (application-present application
                            (application-notice-line (format nil "already in channel ~A" display))))
      ((application-presentation application)
       (unless (application-client-tell application ':switch :session-id identifier)
         (application-present
          application
          (application-failure-line
           (format nil "no terminal controls this channel to move to ~A" display)))))
      ((and controller (application-input-controller-turn-active-p controller))
       (application-present
        application
        (application-notice-line
         (format nil "wait for the turn to finish, then (open ~S)" display))))
      (t
       (application-resume-conversation application identifier)
       (application-render-records application))))
  ':continue)

(define-application-command application--builtin-rename-command
    (:name              "rename"
     :help-topic        :channels
     :help-order        7
     :argument          "[[WHO] NAME]"
     :description       "rename an agent"
     :tip               "renames this channel's agent, or the participant WHO names, or picks one without arguments; names are letters, digits, hyphens and underscores."
     :busy-behavior     :inspect
     :terminal-behavior :exclusive-without-arguments)
    (application &optional (first nil first-p) (second nil second-p))
  (unless first-p
    (when-let ((agent (application--pick-agent application
                                               :title "rename an agent"
                                               :usage "Usage: (rename \"WHO\" \"NAME\")")))
      (application--prefill-call application "rename" agent))
    (return-from application--builtin-rename-command ':continue))
  (let* ((configuration (application-configuration application))
         (name          (princ-to-string (if second-p second first)))
         (participant   (if second-p
                            (let ((designator (princ-to-string first)))
                              (or (world-participant-designated
                                   (world-load configuration)
                                   designator)
                                  (error 'world-error
                                         :message    (format nil "No participant is named ~A." designator)
                                         :pathname   nil
                                         :operation  ':rename-participant
                                         :identifier designator)))
                            (or (world-register-conversation configuration
                                                             (application-conversation application))
                                (error 'world-error
                                       :message   "This channel has no agent participant yet; it gets one with its first durable message."
                                       :pathname  nil
                                       :operation ':rename-participant))))
         (before        (participant-name participant)))
    (world-participant-rename configuration (participant-identifier participant) name)
    (application-present application (application-notice-line (format nil "@~A is now @~A" before name))))
  ':continue)

(define-application-command application--builtin-channel-id-command
    (:name              "channel-id"
     :help-topic        :channels
     :help-order        8
     :argument          nil
     :description       "show this channel's ID"
     :tip               "shows the ID of the channel this terminal is in; as a Lisp call it returns the ID."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application)
  (let ((identifier (conversation-identifier (application-conversation application))))
    (application-present
     application
     (application-notice-line
      (format nil "channel ~A" (conversation-identifier-display identifier))))
    (values ':continue identifier)))

(define-application-command application--builtin-tell-command
    (:name              "tell"
     :help-topic        :channels
     :help-order        3
     :argument          "[CHANNEL MESSAGE]"
     :description       "send a message to a channel as yourself"
     :tip               "reaches the channel's residents and anyone you mention. Without a message it picks the channel and lets you write one."
     :busy-behavior     :execute
     :terminal-behavior :exclusive-without-arguments)
    (application &optional channel &rest words)
  (unless words
    (let ((target (or channel
                      (application--pick-identifier application
                                                    :title        "tell which channel?"
                                                    :items        (application--channel-items application)
                                                    :hint         "type to search, enter selects, esc cancels"
                                                    :search-p     t
                                                    :usage        "Usage: (tell \"CHANNEL\" \"MESSAGE\")"
                                                    :empty-notice "no channels exist"))))
      (when target
        (application--prefill-call application "tell" (princ-to-string target)))
      (return-from application--builtin-tell-command ':continue)))
  (when-let ((target (application--channel-or-ask application (princ-to-string channel)
                                                  :title "tell which channel?"
                                                  :usage "Usage: (tell \"CHANNEL\" \"MESSAGE\")")))
    (let ((result (application-world-send application
                                          :channel target
                                          :text    (format nil "~{~A~^ ~}" words))))
      (application-present
       application
       (application-notice-line
        (format nil "sent to ~{~A~^, ~}~@[; waiting for a full inbox: ~{~A~^, ~}~]"
                (mapcar #'conversation-identifier-display (getf result :channels))
                (mapcar #'conversation-identifier-display (getf result :pending)))))))
  ':continue)

(define-application-command application--builtin-history-command
    (:name              "history"
     :help-topic        :conversations
     :help-order        1
     :argument          nil
     :description       "load earlier transcript history"
     :tip               "loads one configured page of earlier transcript entries."
     :busy-behavior     :hold
     :terminal-behavior :shared)
    (application)
  (application-render-history application)
  ':continue)

(define-application-command application--builtin-working-directory-command
    (:name              "cwd"
     :help-topic        :workspace
     :help-order        1
     :argument          "[PATH]"
     :description       "show or change the active workspace"
     :tip               "Use `(cwd pathname)` to change the active workspace, or `(cwd)` to inspect it."
     :busy-behavior     :apply
     :terminal-behavior :shared)
    (application &optional pathname)
  (application-working-directory-command application pathname)
  ':continue)

(define-application-command application--builtin-authentication-command
    (:name              "auth"
     :help-topic        :models
     :help-order        1
     :argument          "[PROVIDER] [METHOD]"
     :description       "pick and authenticate a registered provider"
     :tip               "Use `(auth \"chatgpt\" :device-code)` for device authentication, or `:browser`."
     :busy-behavior     :hold
     :terminal-behavior :exclusive)
    (application &optional (provider-name nil provider-name-supplied-p) method)
  (when-let ((provider-name
               (if provider-name-supplied-p
                   provider-name
                   (and *application-command-interactive-p*
                        (application--pick-authentication-provider application)))))
    (application-authenticate application provider-name method))
  ':continue)

(define-application-command application--builtin-model-command
    (:name              "model"
     :help-topic        :models
     :help-order        2
     :argument          "[MODEL] [:provider PROVIDER]"
     :description       "pick a registered provider model and reasoning effort"
     :tip               "Select a provider/model ID, or use (model \"ID\" :provider \"NAME\"). The choice is saved for future sessions."
     :busy-behavior     :apply-interactive
     :terminal-behavior :exclusive)
    (application &rest arguments)
  (let* ((model-supplied-p (oddp (length arguments)))
         (model            (and model-supplied-p (first arguments))))
    (destructuring-bind (&key provider) (if model-supplied-p (rest arguments) arguments)
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
             (application-selection-line
              (config :reasoning-effort (application-configuration application))
              :model (configuration-model-selection-id
                      (application-configuration application)))))))))
  ':continue)

(define-application-command application--builtin-models-command
    (:name              "models"
     :help-topic        :models
     :help-order        3
     :argument          "[:provider PROVIDER]"
     :description       "list registered providers and models"
     :tip               "Use (models :provider \"NAME\") to list one provider's models."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application &key provider)
  (application-models-command application :provider provider)
  ':continue)

(define-application-command application--builtin-effort-command
    (:name              "effort"
     :help-topic        :models
     :help-order        4
     :argument          "[LEVEL]"
     :description       "pick the reasoning effort"
     :tip               "sets the model's reasoning budget without changing models. Higher levels allow more reasoning and can take longer; available levels depend on the model."
     :busy-behavior     :apply-interactive
     :terminal-behavior :exclusive-without-arguments)
    (application &optional (effort nil effort-supplied-p))
  (when-let ((effort
               (if effort-supplied-p
                   effort
                   (and *application-command-interactive-p*
                        (application--pick-reasoning-effort application)))))
    (application-set-reasoning-effort application effort)
    (application-present
     application
     (application-selection-line
      (config :reasoning-effort (application-configuration application)))))
  ':continue)

(define-application-command application--builtin-simple-technical-english-command
    (:name              "ste"
     :help-topic        :display
     :help-order        5
     :description       "use Simple Technical English for replies"
     :tip               "Use `(ste t)` to request short, direct Simple Technical English, or `(ste nil)` to remove that style instruction."
     :busy-behavior     :apply
     :terminal-behavior :shared
     :static-options    (t nil))
    (application &optional (mode ':status))
  (application-simple-technical-english-command application mode)
  ':continue)

(define-application-command application--builtin-hurry-up-command
    (:name              "hurry-up"
     :help-topic        :models
     :help-order        6
     :description       "show or change hurry-up mode"
     :tip               "Use `(hurry-up t)` to favor direct work with at most two child agents, or `(hurry-up nil)` for normal delegation limits."
     :busy-behavior     :apply
     :terminal-behavior :shared
     :static-options    (t nil))
    (application &optional (mode ':status))
  (application-hurry-up-command application mode)
  ':continue)

(define-application-command application--builtin-permissions-command
    (:name              "permissions"
     :help-topic        :workspace
     :help-order        2
     :description       "choose and remember command access for this channel"
     :tip               "Use `(permissions :ask)` for approval prompts, `:auto` for automatic authorization, `:sandbox` for workspace confinement, or `:full` for host access in this channel across restarts."
     :busy-behavior     :apply-interactive
     :terminal-behavior :exclusive-without-arguments
     :static-options    (:ask :auto :sandbox :full :list :clear))
    (application &optional (choice nil choice-supplied-p))
  (when choice-supplied-p
    (application--control-choice choice '(:ask :auto :sandbox :full :list :clear)))
  (let ((choice
          (if choice-supplied-p
              choice
              (and *application-command-interactive-p*
                   (application--pick-identifier
                    application
                    :title        "command permissions"
                    :items        (application--permission-mode-items application)
                    :usage        "Usage: (permissions), or (permissions :MODE) with :ask, :auto, :sandbox, :full, :list, or :clear"
                    :empty-notice "No command permission modes exist.")))))
    (application-permissions-command
     application
     (if (and (not choice-supplied-p) choice)
         (second (find choice (application--permission-mode-catalogue)
                       :key #'second :test #'string-equal))
         choice)))
  ':continue)

(define-application-command application--builtin-goal-command
    (:name              "goal"
     :help-topic        :conversations
     :help-order        2
     :argument          "[OBJECTIVE]"
     :description       "start or inspect a mission without mission-specific budgets"
     :tip               "Set a string objective; use `(goal :pause)`, `(goal :resume)`, or `(goal :clear)` for control."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application &optional request &rest arguments)
  (application-goal-request application request arguments)
  ':continue)

(define-application-command application--builtin-agenda-command
    (:name              "agenda"
     :help-topic        :workspace
     :help-order        3
     :argument          nil
     :description       "show workspace agenda entries"
     :tip               "shows durable commitments and notes for the current workspace."
     :busy-behavior     :apply
     :terminal-behavior :shared)
    (application)
  (application-present application (application-agenda-entry application))
  ':continue)

(define-application-command application--builtin-papercuts-command
    (:name              "papercuts"
     :help-topic        :workspace
     :help-order        6
     :argument          nil
     :description       "show workspace papercut reports"
     :tip               "shows problems Autolith recorded when something was not working."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present application (application-papercuts-entry application))
  ':continue)

(define-application-command application--builtin-papercut-command
    (:name              "papercut"
     :help-topic        :workspace
     :help-order        7
     :argument          "[ID]"
     :description       "show one complete papercut report"
     :tip               "opens the full report named by `(papercuts)`."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application &optional identifier)
  (if identifier
      (application-present
       application
       (application-papercut-entry application identifier))
      (application-present
       application
       (application-failure-line "usage: (papercut \"ID\"). Run (papercuts) to list reports.")))
  ':continue)

(define-application-command application--builtin-papercut-close-command
    (:name              "papercut-close"
     :help-topic        :workspace
     :help-order        8
     :argument          "[ID]"
     :description       "close one resolved papercut report"
     :tip               "removes a fixed or obsolete report from `(papercuts)`."
     :busy-behavior     :apply
     :terminal-behavior :shared)
    (application &optional identifier)
  (if identifier
      (application-present
       application
       (application-papercut-close-entry application identifier))
      (application-present
       application
       (application-failure-line "usage: (papercut-close \"ID\"). Run (papercuts) to list reports.")))
  ':continue)

(define-application-command application--builtin-skills-command
    (:name              "skills"
     :help-topic        :workspace
     :help-order        4
     :argument          nil
     :description       "show available skills"
     :tip               "shows request-local skills and any discovery problems."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present
   application
   (skill-status-entry (application-configuration application)))
  ':continue)

(-> application--tool-capability-change-entry (list list) (option list))
(defun application--tool-capability-change-entry (added-tools removed-tools)
  "Return a concise transcript entry for a changed runtime tool-name set."
  (when (or added-tools removed-tools)
    (list
     (terminal-span ':hint "· tools added: ")
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
      (application-hint-line "no MCP servers are configured")
      (loop for record in records
            for state = (getf record :state)
            for failure = (getf record :failure)
            append
            (append
             (list (terminal-span ':brand (format nil "# ~A~%" (getf record :name))))
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
        (application-hint-line "no retained generations exist")
        (loop for generation in generations
              for compatible-p = (generation-compatible-p generation)
              for history = (generation-mutation-history-commit generation)
              for replay = (generation-reconstruction-pathname generation)
              append
              (append
               (list (terminal-span ':brand "# ")
                     (terminal-span ':code (generation-identifier generation))
                     (terminal-span (if compatible-p ':success ':failure)
                                    (format nil "  ~A~%"
                                            (if compatible-p "compatible" "incompatible"))))
               (application--entry-fields
                (list (list "source" ':code (format nil "~A~:[~; (dirty)~]"
                                                     (generation-git-commit generation)
                                                     (generation-dirty-p generation)))
                      (list "image" ':code (or (generation-image-commit-identifier generation) "base"))
                      (list "history" (if history ':code ':notice) (or history "unavailable"))
                      (list "replay" (if replay ':code ':notice)
                            (if replay (namestring replay) "unavailable for legacy generation")))))))))

(define-application-command application--builtin-mcp-command
    (:name              "mcp"
     :help-topic        :workspace
     :help-order        5
     :description       "show or refresh configured MCP servers"
     :tip               "Inspect connections with `(mcp)`, rediscover tools with `(mcp :refresh)`, or reread configuration with `(mcp :reload)`."
     :busy-behavior     :apply
     :terminal-behavior :shared
     :static-options    (:refresh :reload))
    (application &optional (mode ':status))
  (let ((mode          (application--control-choice mode '(:status :refresh :reload)))
        (added-tools   nil)
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
    (when-let ((change-entry
                 (application--tool-capability-change-entry
                  added-tools removed-tools)))
      (application-present application change-entry))
    (let ((manager
            (mcp-tool-registry-manager
             (application-tool-registry application))))
      (application-present
       application
       (application-mcp-status-entry (and manager (mcp-manager-status-records manager))))))
  ':continue)

(define-application-command application--builtin-checkpoint-command
    (:name              "checkpoint"
     :help-topic        :runtime
     :help-order        3
     :argument          "[:allow-dirty t]"
     :description       "save a retained live generation"
     :tip               "saves the current live state as a retained generation; (checkpoint :allow-dirty t) accepts uncommitted source changes and marks the generation dirty."
     :busy-behavior     :hold
     :terminal-behavior :shared)
    (application &key allow-dirty)
  (application-checkpoint application :allow-dirty-p (not (null allow-dirty)))
  ':continue)

(define-application-command application--builtin-checkpoint-cancel-command
    (:name              "checkpoint-cancel"
     :help-topic        :runtime
     :help-order        4
     :argument          nil
     :description       "cancel a checkpoint that waits for running work"
     :tip               "a checkpoint holds every channel's new turns until running work finishes; this lets them go on without it."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application)
  (let ((server (application-world-server application)))
    (application-present
     application
     (application-notice-line
      (if (and server (world-server-cancel-checkpoint server))
          "checkpoint cancelled; every channel goes on"
          "no checkpoint is waiting"))))
  ':continue)

(define-application-command application--builtin-generations-command
    (:name              "generations"
     :help-topic        :runtime
     :help-order        5
     :argument          nil
     :description       "list retained generations"
     :tip               "shows live generations available for recovery."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present
   application
   (application-generations-entry (application-configuration application)))
  ':continue)

(define-application-command application--builtin-rollback-command
    (:name              "rollback"
     :help-topic        :runtime
     :help-order        6
     :argument          "[ID]"
     :description       "pick a generation for recovery"
     :tip               "selects a retained generation for the next recovery start."
     :busy-behavior     :hold
     :terminal-behavior :exclusive-without-arguments)
    (application &optional (identifier nil identifier-supplied-p))
  (when-let ((identifier
               (if identifier-supplied-p
                   identifier
                   (and *application-command-interactive-p*
                        (application--pick-identifier
                         application
                         :title        "select a generation for recovery"
                         :items        (application--generation-items application)
                         :usage        "Usage: (rollback \"ID\")"
                         :empty-notice "No retained generations exist.")))))
    (generation-request-rollback
     (application-configuration application) identifier))
  ':continue)

(define-application-command application--builtin-info-command
    (:name              "info"
     :help-topic        :runtime
     :help-order        1
     :argument          nil
     :description       "show current model and runtime settings"
     :tip               "prints the current settings without changing them."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present application (application-info-entry application))
  ':continue)

(define-application-command application--builtin-status-command
    (:name              "status"
     :help-topic        :runtime
     :help-order        2
     :aliases           ("usage")
     :argument          nil
     :description       "show usage and rate limits"
     :tip               "shows the model, context usage, and subscription rate limits."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present application (application-status-entry application))
  ':continue)

(define-application-command application--builtin-update-command
    (:name              "update"
     :help-topic        :runtime
     :help-order        7
     :argument          nil
     :description       "check and install a newer packaged release"
     :tip               "updates curl-installed Autolith and explains source or Nix updates."
     :busy-behavior     :hold
     :terminal-behavior :shared)
    (application)
  (application-update application)
  ':continue)

(define-application-command application--builtin-context-command
    (:name              "context"
     :help-topic        :conversations
     :help-order        3
     :argument          nil
     :description       "inspect request-local context"
     :tip               "reveals the ephemeral context prepared for provider requests."
     :busy-behavior     :inspect
     :terminal-behavior :shared)
    (application)
  (application-present application (application-context-entry application))
  ':continue)

(define-application-command application--builtin-compact-command
    (:name              "compact"
     :help-topic        :conversations
     :help-order        4
     :argument          nil
     :description       "compact conversation context now"
     :tip               "compacts the conversation context immediately, replacing older turns with a summary."
     :busy-behavior     :hold
     :terminal-behavior :shared)
    (application)
  (application-compact application)
  ':continue)

(define-application-command application--builtin-fix-skipped-definitions-command
    (:name              "fix-skipped-definitions"
     :help-topic        :runtime
     :help-order        8
     :argument          "[comment]"
     :description       "ask the model to rebuild or drop skipped private definitions"
     :tip               "Use `(fix-skipped-definitions \"Drop temporary development overrides.\")` to direct which skipped definitions to rebuild or drop before committing."
     :busy-behavior     :hold
     :terminal-behavior :shared)
    (application &optional (comment ""))
  (application-fix-skipped-definitions application comment)
  ':continue)

(define-application-command application--builtin-exit-command
    (:name              "exit"
     :help-topic        :runtime
     :help-order        15
     :argument          nil
     :description       "leave the channel running and return to the shell"
     :tip               "disconnects this client; return with `autolith open ID`."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application)
  (let ((session (application-channel-endpoint application)))
    (unless session
      (error 'world-terminal-error
             :message   "The channel's terminal endpoint is not ready."
             :operation ':detach))
    (world-terminal--detach-terminal session))
  ':continue)

(define-application-command application--builtin-stop-server-command
    (:name              "stop-server"
     :argument          nil
     :description       "stop the current world server and its channels"
     :tip               "stops work in every channel and closes the world's clients."
     :busy-behavior     :execute
     :terminal-behavior :shared)
    (application)
  (let ((server (application-world-server application)))
    (unless server
      (error 'world-error :operation ':stop :message "This client has no world server."))
    (world-server--dispatch server ':stop nil))
  ':continue)
