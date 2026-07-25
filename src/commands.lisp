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
                         (agenda-item-memory-identifiers item)))))))))


;;;; -- Usage Status --

(-> application--conversation-usage (application) list)
(defun application--conversation-usage (application)
  "Return summed (:input N :output N :total N) usage for the active conversation."
  (let ((input 0)
        (output 0)
        (total 0))
    (labels ((usage-count (usage key)
               "Return the integer usage value stored under KEY, or zero."
               (let ((value (second (assoc key usage :test #'string=))))
                 (if (integerp value)
                     value
                     0))))
      (conversation--map-records
       (conversation-pathname (application-conversation application))
       (lambda (record)
         (when (eq (first record) :provider)
           (let ((usage (getf (getf (rest record) :metadata) :usage)))
             (when (listp usage)
               (incf input (usage-count usage "input_tokens"))
               (incf output (usage-count usage "output_tokens"))
               (incf total (usage-count usage "total_tokens"))))))))
    (list :input input :output output :total total)))

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
     (application--field-spans "path"
                               "standard (the fast path is never requested)")
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

(-> application--calendar-description (integer) string)
(defun application--calendar-description (universal-time)
  "Return UNIVERSAL-TIME as a compact local calendar description."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year month date hour minute)))

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

(-> application--conversation-preview (pathname) (option string))
(defun application--conversation-preview (pathname)
  "Return a one-line excerpt of PATHNAME's newest user or assistant message."
  (handler-case
      (let ((preview nil))
        (conversation--map-records
         pathname
         (lambda (record)
           (case (first record)
             (:message
              (let ((content (getf (rest record) :content)))
                (when (and (eq (getf (rest record) :role) :user)
                           (stringp content))
                  (setf preview content))))
             (:provider-item
              (let ((wire-json (getf (rest record) :wire-json)))
                (when (stringp wire-json)
                  (let ((item (json-decode wire-json)))
                    (when (json-object-p item)
                      (let ((text (response-item-assistant-text item)))
                        (when text
                          (setf preview text)))))))))))
        (when preview
          (text-cell-prefix
           (sanitize-text preview :single-line-p t)
           *conversation-preview-width*)))
    (error ()
      nil)))

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
             (header (conversation-peek-header pathname))
             (directory (getf (rest header) :directory))
             (current-directory-p
               (application--conversation-current-directory-p
                directory
                current-directory))
             (item
               (list :name (conversation-identifier-display identifier)
                     :argument nil
                     :group (if current-directory-p
                                current-group
                                "other sessions")
                     :description
                     (if current-directory-p
                         (format nil "~A~:[~;, current~]~@[ · ~A~]"
                                 (application--calendar-description
                                  (or (file-write-date pathname) 0))
                                 (string= identifier current-identifier)
                                 (application--conversation-preview pathname))
                         (format nil
                                 "~A~@[, ~A~]~:[~;, current~]~@[ · ~A~]"
                                 (application--calendar-description
                                  (or (file-write-date pathname) 0))
                                 (application--abbreviated-directory directory)
                                 (string= identifier current-identifier)
                                 (application--conversation-preview pathname))))))
        (if current-directory-p
            (push item current-items)
            (push item other-items))))
    (append (nreverse current-items) (nreverse other-items))))

(-> application--effort-items (application) list)
(defun application--effort-items (application)
  "Return picker items for the supported reasoning efforts."
  (let ((current (configuration-reasoning-effort
                  (application-configuration application))))
    (loop for effort in *supported-reasoning-efforts*
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

(-> application-set-model-selection (application string string) null)
(defun application-set-model-selection (application model effort)
  "Switch APPLICATION to MODEL and reasoning EFFORT, and persist both choices."
  (let ((configuration
          (configuration-with-reasoning-effort
           (configuration-with-model (application-configuration application)
                                     model)
           effort)))
    (application--persist-model-selection application configuration)
    (application--install-configuration application configuration)))

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

(-> application--model-items (application) list)
(defun application--model-items (application)
  "Return picker items for the supported 5.6 model family."
  (let ((current (configuration-model
                  (application-configuration application))))
    (loop for model in *supported-models*
          collect (list :name model
                        :argument nil
                        :description (if (string= model current)
                                         "current"
                                         "")))))

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
  "Manually compact the active conversation into a durable summary."
  (let ((agent (application-agent application))
        (conversation (application-conversation application)))
    (unless agent
      (error 'configuration-error
             :message "No connected agent can compact the conversation."))
    (if (null (conversation-input-items conversation))
        (application-present application "Nothing to compact yet.")
        (progn
          (unwind-protect
               (agent-compact-conversation
                agent
                (application-agent-observer application))
            (application-set-activity application nil))
          (application-render-records application)
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

(-> application--goal-update (application string) null)
(defun application--goal-update (application objective)
  "Rewrite the current session goal's objective to OBJECTIVE in place.

An active or paused goal keeps its status and creation time while its
continuation budget restarts. A completed goal becomes active again."
  (let ((goal (application-goal application)))
    (cond
      ((null goal)
       (application-present
        application
        "No session goal to update. Use /goal OBJECTIVE to set one."))
      ((zerop (length objective))
       (application-present application "Usage: /goal update NEW-OBJECTIVE"))
      ((uiop:string-prefix-p "/" objective)
       (application-present
        application
        (format nil "~S looks like a command, not an objective. ~
                     Usage: /goal update NEW-OBJECTIVE"
                objective)))
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
                    ':paused))))))
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
             (application--run-turn application
                                    *application-goal-continuation-prompt*
                                    :continuation-p t)
             (application--run-goal-continuations application))
           (application-present application "No paused goal to resume.")))
      (update-argument
       (application--goal-update application update-argument))
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
                "Goal set: ~A~%Autolith keeps working toward it after every ~
                 message. Use /goal to inspect it and /goal clear to stop."
                remainder)))))
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
                 (:empty-notice string))
    (option string))
(defun application--pick-identifier
    (application &key (title "select") items (usage "") (empty-notice ""))
  "Pick one identifier from ITEMS interactively, or explain why none was picked.

Signals a usage error on non-interactive terminals, presents EMPTY-NOTICE
when ITEMS is empty, and returns NIL when the picker is cancelled."
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
                  :resize-callback #'application-pending-terminal-size)))
        (let ((controller (application-input-controller application)))
          (if controller
              (application-input-controller-call-with-reader-paused
               controller #'pick)
              (pick)))))))

(-> application--pick-reasoning-effort (application) (option string))
(defun application--pick-reasoning-effort (application)
  "Prompt for one supported reasoning effort and return the selected name."
  (application--pick-identifier
   application
   :title "pick the reasoning effort"
   :items (application--effort-items application)
   :usage "Usage: /effort LEVEL"
   :empty-notice "No supported reasoning efforts exist."))

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

(-> application-authenticate (application) null)
(defun application-authenticate (application)
  "Run Autolith-owned device authentication outside raw terminal mode."
  (let* ((ui (application-ui application))
         (provider (application-provider application)))
    (unless (typep provider 'codex-subscription-provider)
      (error 'authentication-error
             :message "The active provider does not support ChatGPT device login."))
    (terminal-ui-stop ui)
    (unwind-protect
         (device-authentication-login
          (device-authentication-client-create)
          (provider-credential-manager provider)
          :stream *standard-output*
          :open-browser-p t)
      (terminal-ui-start ui))
    (application-present application "ChatGPT authentication was saved by Autolith."))
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
    (:sandboxed "allow commands inside the workspace sandbox")
    (:full-access "let commands run with full user privileges")))

(-> application--permission-mode-items (application) list)
(defun application--permission-mode-items (application)
  "Return session command permission choices for APPLICATION."
  (let ((current (application-permission-mode application)))
    (list
     (list :name "ask"
           :argument nil
           :description (if (eq current ':ask)
                            "current; prompt unless this exact command was saved"
                            "prompt unless this exact command was saved"))
     (list :name "sandbox"
           :argument nil
           :description (if (eq current ':sandboxed)
                            "current; allow commands inside the workspace sandbox"
                            "allow commands inside the workspace sandbox"))
     (list :name "full"
           :argument nil
           :description (if (eq current ':full-access)
                            "current; let it ride with full user privileges"
                            "let it ride with full user privileges")))))

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

(-> application-permissions-command (application (option string)) null)
(defun application-permissions-command (application argument)
  "Show or change APPLICATION's session command permissions."
  (let ((choice
          (or (and argument (string-downcase argument))
              (application--pick-identifier
               application
               :title "command permissions"
               :items (application--permission-mode-items application)
               :usage "Usage: /permissions [ask|sandbox|full|list|clear]"
               :empty-notice "No command permission modes exist."))))
    (cond
      ((null choice)
       nil)
      ((string= choice "ask")
       (setf (application-permission-mode application) ':ask)
       (application-present
        application
        "Commands will ask before running unless the exact command was saved."))
      ((string= choice "sandbox")
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
              :message "Usage: /permissions [ask|sandbox|full|list|clear]."))))
  nil)

(-> application--later-list (application) string)
(defun application--later-list (application)
  "Return APPLICATION's durable deferred inputs in execution order."
  (let* ((controller (application-input-controller application))
         (entries
           (and controller
                (later-state-entries
                 (application-input-controller-later-state controller)))))
    (if entries
        (format nil "Deferred inputs:~%~{~A~^~%~}"
                (loop for entry in entries
                      collect
                      (format nil "  ~A  ~A  ~A~%    ~A"
                              (later-entry-identifier entry)
                              (application--calendar-description
                               (later-entry-due-at entry))
                              (later-entry-window entry)
                              (text-cell-prefix
                               (sanitize-text (later-entry-input entry)
                                              :single-line-p t)
                               72))))
        "No deferred inputs are scheduled.")))

(-> application-later-command (application string) null)
(defun application-later-command (application remainder)
  "List, cancel, or schedule a deferred input from /later REMAINDER."
  (let* ((controller (application-input-controller application))
         (trimmed (string-trim '(#\Space #\Tab) remainder)))
    (unless controller
      (error 'configuration-error
             :message "Deferred scheduling needs the interactive application."))
    (cond
      ((zerop (length trimmed))
       (application-present application (application--later-list application)))
      ((or (string= (string-downcase trimmed) "cancel")
           (uiop:string-prefix-p "cancel " (string-downcase trimmed)))
       (let ((identifier
               (if (> (length trimmed) (length "cancel"))
                   (string-trim '(#\Space #\Tab)
                                (subseq trimmed (length "cancel")))
                   "")))
         (unless (non-empty-string-p identifier)
           (error 'configuration-error
                  :message "Usage: /later cancel ID"))
         (if (application-input-controller-cancel-later controller identifier)
             (application-present application
                                  (format nil "Cancelled deferred input ~A."
                                          identifier))
             (error 'configuration-error
                    :message (format nil "Deferred input ~A does not exist."
                                     identifier)))))
      (t
       (let ((provider (application-provider application)))
         (multiple-value-bind (due-at window)
             (later-reset-deadline
              (and provider (provider-rate-limits provider)))
           (unless (and due-at window)
             (error 'configuration-error
                    :message
                    "No usable rate-limit reset is known. Send a message, then inspect /status."))
           (let ((entry
                   (application-input-controller-schedule-later
                    controller
                    trimmed
                    :due-at due-at
                    :window window)))
             (application-present
              application
              (format nil "Scheduled deferred input ~A for ~A after the ~A reset."
                      (later-entry-identifier entry)
                      (application--calendar-description due-at)
                      window))))))))
  nil)

;;;; -- Built-in Interactive Commands --

(define-application-command application--builtin-help-command
    (:name "/help"
     :argument nil
     :description "show this reference"
     :tip "shows every interactive command."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present application (application-help))
  ':continue)

(define-application-command application--builtin-new-command
    (:name "/new"
     :argument nil
     :description "start a new conversation"
     :tip "starts fresh without deleting the current conversation."
     :busy-behavior :hold
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
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
     :argument nil
     :description "pick a saved conversation to resume"
     :tip "returns to a saved conversation from this workspace or another one."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments)
    (application invocation)
  (let* ((configuration (application-configuration application))
         (startup-offer-p
           (application-project-adaptation-offer-p application))
         (identifier
           (or (application-command-invocation-argument invocation)
               (application--pick-identifier
                application
                :title "resume conversation"
                :items (application--conversation-items application)
                :usage "Usage: /resume ID"
                :empty-notice "No saved conversations exist."))))
    (setf (application-project-adaptation-offer-p application) nil)
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
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present application
                       (application-list-conversations application))
  ':continue)

(define-application-command application--builtin-history-command
    (:name "/history"
     :argument nil
     :description "load earlier transcript history"
     :tip "loads the previous 500 transcript entries on demand."
     :busy-behavior :hold
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-render-history application)
  ':continue)

(define-application-command application--builtin-working-directory-command
    (:name "/cwd"
     :argument "PATH"
     :description "change the active workspace"
     :tip "moves the active workspace without restarting Autolith."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (application-working-directory-command
   application
   (application-command-invocation-remainder invocation))
  ':continue)

(define-application-command application--builtin-authentication-command
    (:name "/auth"
     :argument nil
     :description "authenticate Autolith with ChatGPT"
     :tip "starts direct ChatGPT authentication when credentials need attention."
     :busy-behavior :hold
     :terminal-behavior :exclusive)
    (application invocation)
  (declare (ignore invocation))
  (application-authenticate application)
  ':continue)

(define-application-command application--builtin-model-command
    (:name "/model"
     :argument nil
     :description "pick the 5.6 model and reasoning effort"
     :tip "changes both the model and its reasoning effort."
     :busy-behavior :hold
     :terminal-behavior :exclusive)
    (application invocation)
  (let ((model
          (or (application-command-invocation-argument invocation)
              (application--pick-identifier
               application
               :title "pick the model"
               :items (application--model-items application)
               :usage "Usage: /model NAME"
               :empty-notice "No supported models exist."))))
    (when model
      (configuration-with-model (application-configuration application)
                                model)
      (let ((effort (application--pick-reasoning-effort application)))
        (when effort
          (application-set-model-selection application model effort)
          (application-present
           application
           (format nil "The model is now ~A with reasoning effort ~A."
                   (configuration-model
                    (application-configuration application))
                   (configuration-reasoning-effort
                    (application-configuration application))))))))
  ':continue)

(define-application-command application--builtin-effort-command
    (:name "/effort"
     :argument nil
     :description "pick the reasoning effort"
     :tip "changes reasoning effort without switching models."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments)
    (application invocation)
  (let ((effort
          (or (application-command-invocation-argument invocation)
              (application--pick-reasoning-effort application))))
    (when effort
      (application-set-reasoning-effort application effort)
      (application-present
       application
       (format nil "Reasoning effort is now ~A."
               (configuration-reasoning-effort
                (application-configuration application))))))
  ':continue)

(define-application-command application--builtin-trace-command
    (:name "/trace"
     :argument "on|off"
     :description "show visible reasoning summaries"
     :tip "toggles visible reasoning summaries with on or off."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (application-trace-command
   application
   (application-command-invocation-argument invocation))
  ':continue)

(define-application-command application--builtin-permissions-command
    (:name "/permissions"
     :argument nil
     :description "choose command access for this session"
     :tip "chooses how shell commands are authorized for this session."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments)
    (application invocation)
  (application-permissions-command
   application
   (application-command-invocation-argument invocation))
  ':continue)

(define-application-command application--builtin-later-command
    (:name "/later"
     :argument "INPUT"
     :description "run input after rate limits reset"
     :tip "queues a prompt for the next known rate-limit reset."
     :busy-behavior :hold
     :terminal-behavior :shared)
    (application invocation)
  (application-later-command
   application
   (application-command-invocation-remainder invocation))
  ':continue)

(define-application-command application--builtin-goal-command
    (:name "/goal"
     :argument "OBJECTIVE"
     :description "set or view the session goal"
     :tip "sets the objective Autolith should pursue across continuations."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (application-goal-command
   application
   (application-command-invocation-remainder invocation))
  ':continue)

(define-application-command application--builtin-agenda-command
    (:name "/agenda"
     :argument nil
     :description "show workspace agenda entries"
     :tip "shows durable commitments and notes for the current workspace."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present application (application-agenda-entry application))
  ':continue)

(define-application-command application--builtin-skills-command
    (:name "/skills"
     :argument nil
     :description "show available skills"
     :tip "shows request-local skills and any discovery problems."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present
   application
   (skill-status (application-configuration application)))
  ':continue)

(define-application-command application--builtin-mcp-command
    (:name "/mcp"
     :argument "refresh|reload"
     :description "show or refresh configured MCP servers"
     :tip "shows MCP connections; refresh rediscovers, reload rereads configuration."
     :busy-behavior :hold
     :terminal-behavior :shared)
    (application invocation)
  (let* ((argument (application-command-invocation-argument invocation))
         (mode (and argument (string-downcase argument))))
    (cond
      ((null mode)
       nil)
      ((string= mode "refresh")
       (mcp-tool-registry-refresh
        (application-tool-registry application)))
      ((string= mode "reload")
       (application-reload-mcp application))
      (t
       (error 'configuration-error
              :message "Usage: /mcp, /mcp refresh, or /mcp reload.")))
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
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-checkpoint application)
  ':continue)

(define-application-command application--builtin-generations-command
    (:name "/generations"
     :argument nil
     :description "list retained generations"
     :tip "shows live generations available for recovery."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present
   application
   (generation-render-list (application-configuration application)))
  ':continue)

(define-application-command application--builtin-rollback-command
    (:name "/rollback"
     :argument nil
     :description "pick a generation for recovery"
     :tip "selects a retained generation for the next recovery start."
     :busy-behavior :hold
     :terminal-behavior :exclusive-without-arguments)
    (application invocation)
  (let* ((configuration (application-configuration application))
         (identifier
           (or (application-command-invocation-argument invocation)
               (application--pick-identifier
                application
                :title "select a generation for recovery"
                :items (application--generation-items application)
                :usage "Usage: /rollback ID"
                :empty-notice "No retained generations exist."))))
    (when identifier
      (generation-request-rollback configuration identifier)))
  ':continue)

(define-application-command application--builtin-status-command
    (:name "/status"
     :aliases ("/usage")
     :argument nil
     :description "show usage and rate limits"
     :tip "shows the model, context usage, and subscription rate limits."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present application (application-status-entry application))
  ':continue)

(define-application-command application--builtin-context-command
    (:name "/context"
     :argument nil
     :description "inspect request-local context"
     :tip "reveals the ephemeral context prepared for provider requests."
     :busy-behavior :inspect
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore invocation))
  (application-present
   application
   (context-status (application-conversation application)))
  ':continue)

(define-application-command application--builtin-compact-command
    (:name "/compact"
     :argument "on|off"
     :description "condense tool details, or summarize context with no argument"
     :tip "toggles compact tool presentation; with no argument it compacts context."
     :busy-behavior :hold
     :terminal-behavior :shared)
    (application invocation)
  (let ((argument (application-command-invocation-argument invocation)))
    (if argument
        (application-compact-view-command application argument)
        (application-compact application)))
  ':continue)

(define-application-command application--builtin-quit-command
    (:name "/quit"
     :aliases ("/exit")
     :argument nil
     :description "leave Autolith"
     :tip "exits cleanly; Ctrl-C also prints the exact resume command."
     :busy-behavior :cancel
     :terminal-behavior :shared)
    (application invocation)
  (declare (ignore application invocation))
  ':quit)


;;;; -- Command Dispatch --

(-> application-command (application string) keyword)
(defun application-command (application input)
  "Execute slash command INPUT for APPLICATION and return its loop action."
  (let* ((invocation (application-command-invocation-parse input))
         (command (application-command-invocation-command invocation)))
    (if command
        (application-command-execute command application invocation)
        (progn
          (application-present
           application
           (format nil "Unknown command ~A. Use /help."
                   (application-command-invocation-name invocation)))
          ':continue))))

(-> application-handle-input (application string) keyword)
(defun application-handle-input (application input)
  "Handle submitted INPUT and return :CONTINUE or :QUIT."
  (cond
    ((not (non-empty-string-p input))
     :continue)
    ((uiop:string-prefix-p "//" input)
     (application-run-message application (subseq input 1))
     :continue)
    ((uiop:string-prefix-p "/" input)
     (application-command application input))
    (t
     (application-run-message application input)
     :continue)))
