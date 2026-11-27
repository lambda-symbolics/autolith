(in-package #:autolith)

;;;; -- Application Lifecycle --

(defparameter *application-banner-logo-lines*
  '((:brand-gradient-1 . "  :::.      :::")
    (:brand-gradient-2 . "  ;;`;;     ;;;")
    (:brand-gradient-3 . " ,[[ '[[,   [[[")
    (:brand-gradient-4 . "c$$$cc$$$c  $$'")
    (:brand-gradient-5 . " 888   888,o88oo,.__")
    (:brand-gradient-6 . " YMM   \"\"` \"\"\"\"YUMMM"))
  "The AL mark generated with FIGlet's Cosmic font, paired with row styles.")

(defparameter *application-banner-gap* "   "
  "Horizontal space between the startup mark and session data.")

(defparameter *application-banner-minimum-metadata-width* 32
  "The minimum useful width for metadata beside the startup mark.")

(-> application--banner-logo-lines () list)
(defun application--banner-logo-lines ()
  "Return the startup mark's styled rows."
  *application-banner-logo-lines*)

(-> application--banner-logo-width () (integer 1))
(defun application--banner-logo-width ()
  "Return the widest row of the embedded startup mark in terminal cells."
  (loop for entry in (application--banner-logo-lines)
        maximize (text-cell-width (rest entry))))

(-> application--banner-columns (application) (integer 1))
(defun application--banner-columns (application)
  "Return APPLICATION's current terminal width or the restrained default."
  (if-let ((ui (and (slot-boundp application 'ui)
                    (application-ui application))))
      (terminal-columns (terminal-ui-terminal ui))
      *terminal-default-columns*))

(-> application--banner-metadata-field (string string) list)
(defun application--banner-metadata-field (label value)
  "Return one aligned metadata LABEL and VALUE row without a newline."
  (list (terminal-span :dim (format nil "~12A  " label))
        (terminal-span :plain value)))

(-> application--banner-terminate-row (list) list)
(defun application--banner-terminate-row (spans)
  "Return SPANS followed by one plain newline span."
  (append spans (list (terminal-span :plain (string #\Newline)))))

(-> application--banner-side-by-side-spans (list integer) list)
(defun application--banner-side-by-side-spans (metadata-rows columns)
  "Return the startup mark with METADATA-ROWS aligned beside it within COLUMNS."
  (let* ((logo-width     (application--banner-logo-width))
         (metadata-width (- columns
                            logo-width
                            (text-cell-width *application-banner-gap*))))
    (loop for logo-entry in (application--banner-logo-lines)
          for index from 0
          for metadata-row = (nth index metadata-rows)
          append
          (let ((logo-text (rest logo-entry)))
            (application--banner-terminate-row
             (append
              (list (terminal-span
                     (first logo-entry)
                     (if metadata-row
                         (format nil "~VA" logo-width logo-text)
                         logo-text)))
              (when metadata-row
                (append
                 (list (terminal-span :plain *application-banner-gap*))
                 (termdown:fit-spans metadata-row metadata-width)))))))))

(-> application--banner-stacked-spans (list integer) list)
(defun application--banner-stacked-spans (metadata-rows columns)
  "Return the startup mark above clipped METADATA-ROWS within COLUMNS."
  (append
   (loop for logo-entry in (application--banner-logo-lines)
         append (application--banner-terminate-row
                 (list (terminal-span (first logo-entry)
                                      (rest logo-entry)))))
   (list (terminal-span :plain (string #\Newline)))
   (loop for metadata-row in metadata-rows
         append (application--banner-terminate-row
                 (termdown:fit-spans metadata-row columns)))))

(defparameter *application-startup-tips*
  '(((:plain "Telemetry helps us fix recurring bugs and reduce wasted tokens. Turn it off with ")
     (:code "(setf (config :telemetry-enabled-p) nil)")
     (:plain ". Details: ")
     (:code "docs/telemetry.org"))
    ((:plain "The prompt is a Lisp REPL, you can use it to evaluate arbitrary Common Lisp."))
    ((:plain "The tools available to the model are callable functions in the prompt, try for instance ")
     (:code "(search.files :query \"README\")")
     (:plain " to fuzzy-search workspace file paths with fff."))
    ((:code "AUTOLITH_WEB_SEARCH")
     (:plain " selects provider web search: disabled turns it off, cached uses indexed results, and live fetches fresh results where supported."))
    ((:plain "Configuring a language server in ")
     (:code "lsp.sexp")
     (:plain " under your configuration directory adds definition, hover, and diagnostics tools for workspace code."))
    ((:plain "Claude caches a prompt for five minutes. If you often pause longer, ")
     (:code "(setf (config :prompt-cache-ttl) :1h)")
     (:plain " asks Anthropic and OpenRouter Claude models for one hour, at twice the cache-write price."))
    ((:plain "Visit ")
     (:code "https://almightylisp.com/")
     (:plain " for almighty tools for almighty programmers, and try ")
     (:code "(settings :terminal-theme :almighty)")
     (:plain " to wear its colors.")))
  "Startup advice supplementing registered command tips, as styled span specifications.")

(-> application--tip-spans (list) terminal-styled-text)
(defun application--tip-spans (spans)
  "Return SPANS as one marked startup tip, with its ordinary words dimmed."
  (cons (terminal-span ':dim "· tip: ")
        (loop for span in spans
              collect (if (eq (terminal-span-style span) ':plain)
                          (terminal-span ':dim (terminal-span-text span))
                          span))))

(-> application--command-tip-spans
    (application-command)
    terminal-styled-text)
(defun application--command-tip-spans (entry)
  "Return ENTRY's startup advice with its call and inline code styled.

Use the Markdown renderer's inline parser without choosing a width here;
the terminal wraps the semantic spans."
  (application--tip-spans
   (list* (terminal-span ':code (format nil "(~A)" (application-operation--command-name entry)))
          (terminal-span ':plain " ")
          (termdown:markdown-inline-spans (application-command-tip entry)))))

(-> application--startup-tip-spans () terminal-styled-text)
(defun application--startup-tip-spans ()
  "Choose command or general advice without consuming the saved image's random state."
  (let* ((commands (application-command-list))
         (index    (random (+ (length commands) (length *application-startup-tips*))
                           (make-random-state t))))
    (if (< index (length commands))
        (application--command-tip-spans (nth index commands))
        (application--tip-spans
         (loop for (style text) in (nth (- index (length commands)) *application-startup-tips*)
               collect (terminal-span style text))))))

(-> application--update-notice (application) (option list))
(defun application--update-notice (application)
  "Return the cached update notice appropriate to APPLICATION's installation."
  (when-let ((availability (application-update-availability application)))
    (let ((current-version *autolith-version*)
          (latest-version  (subseq (update-availability-tag availability) 1)))
      (list
       (terminal-span
        ':notice
         (format nil "· ~Aupdate available: ~A ~A -> ~A~%"
                 (if (application-world-server application)
                     "world server "
                     "")
                 (product-name) current-version latest-version))
       (terminal-span
        ':dim
        (if (eq (update-availability-method availability) ':nix)
            (format nil "  This installation uses Nix. Update the flake or profile that provides ~A"
                    (product-name))
            "  choose whether to install it before continuing"))))))

(-> application-present-generation-health-warning (application) boolean)
(defun application-present-generation-health-warning (application)
  "Present a retained-generation health warning during ordinary startup.

The checkpoints belong to the data root, not to a channel, so a world server
presents the warning in the first channel it opens only."
  (let* ((configuration (application-configuration application))
         (server        (application-world-server application))
         (warning       (and (or (null server) (world-server-take-generation-warning server))
                             (generation-health-warning
                              (generation-health-summary configuration)))))
    (when warning
      (application-present application (application-notice-line (format nil "checkpoints: ~A" warning)))
      t)))

(-> application--update-choice-items () list)
(defun application--update-choice-items ()
  "Return the explicit user choices for a packaged release update."
  '((:name "Not now" :value :not-now :argument nil
     :description "continue with the installed release")
    (:name "Update now" :value :update :argument nil
     :description "install the verified release and restart")
    (:name "Skip this version" :value :skip :argument nil
     :description "hide this exact release until a newer one appears")))

(-> application--offer-startup-update (application) null)
(defun application--offer-startup-update (application)
  "Offer APPLICATION's cached release only through an interactive terminal."
  ;; Startup offers precede installation of the input controller.
  (let ((availability (application-update-availability application))
        (ui           (application-ui application)))
    (when (and availability
               (eq (update-availability-method availability) ':release)
               ui
               (terminal-interactive-p (terminal-ui-terminal ui)))
      (let* ((tag (update-availability-tag availability))
             (choice
               (terminal-ui-select
                ui
                :title           (format nil "~A ~A is available" (product-name) (subseq tag 1))
                :items           (application--update-choice-items)
                :resize-callback #'application-pending-terminal-size)))
        (case choice
          (:update
           (error 'update-requested
                  :message (format nil "Update to ~A ~A." (product-name) (subseq tag 1))
                  :tag     tag))
          (:skip
           (update-state-dismiss (application-configuration application) tag)
           (setf (application-update-availability application) nil)
           (application-present
            application
            (list (terminal-span ':dim
                                 (format nil "· skipped ~A ~A" (product-name) (subseq tag 1))))))))))
  nil)

(-> application--expected-error-entry
    ((or autolith-error cl-llm-provider-api:provider-api-error))
    list)
(defun application--expected-error-entry (condition)
  "Return the failure line describing expected CONDITION."
  (application-failure-line
   (format nil "~A~:[~;~%(auth) authenticates ~A directly~]"
           condition (typep condition 'credentials-unavailable) (product-name))))

(-> application-handle-expected-error
    (application (or autolith-error cl-llm-provider-api:provider-api-error))
    null)
(defun application-handle-expected-error (application condition)
  "Present expected CONDITION without abandoning APPLICATION's active path."
  (application-set-activity application nil)
  (application-render-records application)
  (application-present application
                       (application--expected-error-entry condition))
  nil)

(-> application-read-terminal-event (terminal-ui) t)
(defun application-read-terminal-event (ui)
  "Read one UI event, applying pending resizes before and after the blocking read."
  (terminal-ui-refresh-size ui #'application-pending-terminal-size)
  (prog1 (terminal-ui-read-event ui)
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)))

(-> application--client-reports-exit-p (application) boolean)
(defun application--client-reports-exit-p (application)
  "Return whether APPLICATION has no terminal of its own and its clients report its exit."
  (not (null (application-presentation application))))

(-> application--present-open-instruction (application) boolean)
(defun application--present-open-instruction (application)
  "Put durable APPLICATION's reopen advice in its direct terminal epilogue.

Attached clients report their own channel after releasing their shared screen."
  (let ((conversation (application-conversation application))
        (ui           (application-ui application)))
    (when (and (conversation-persisted-p conversation)
               (not (application--client-reports-exit-p application)))
      (terminal-ui-set-epilogue
       ui (channel-open-instruction (conversation-identifier conversation)))
      t)))

(-> application--initial-work-items ((option string)) list)
(defun application--initial-work-items (initial-command)
  "Return controller work for an initial command, when supplied."
  (when initial-command
    (when-let ((work (input-work-from-input initial-command)))
      (list work))))

(-> application-call-with-session-quiesced (application function) t)
(defun application-call-with-session-quiesced (application function)
  "Call FUNCTION while APPLICATION's reader, update check and endpoint threads are stopped.

Attached clients reconnect to the endpoint restarted afterwards. Without a
controller, only the update check and the endpoint are quiesced."
  (let ((controller (application-input-controller application)))
    (flet ((quiesced ()
             "Join the update check and quiesce the endpoint around FUNCTION."
             (application--quiesce-update-check application)
             (application-call-with-world-terminal-quiesced application function)))
      (if controller
          (application-input-controller-call-with-reader-paused controller #'quiesced)
          (quiesced)))))

(-> application--fatal-debugger-hook (application) function)
(defun application--fatal-debugger-hook (application)
  "Return a noninteractive fatal debugger hook for APPLICATION."
  (let ((sb-ext:*invoke-debugger-hook* nil)
        (sb-debug::*old-debugger-hook* nil))
    (sb-ext:disable-debugger)
    (let ((fallback sb-ext:*invoke-debugger-hook*))
      (lambda (condition previous-hook)
        (declare (ignore previous-hook))
        (let ((sb-ext:*invoke-debugger-hook* fallback)
              (*debugger-hook*               nil))
          (funcall (fdefinition 'application-raise-fatal)
                   application condition (application-safe-backtrace))
          (funcall fallback condition fallback))))))

(-> application-serve
    (application &key (:initial-command (option string))
                      (:initial-input (option user-message-input))
                      (:adaptation-offer-p boolean)
                      (:quiescer (option function))
                      (:ready-function (option function))
                      (:resume-p boolean)
                      (:world-terminal-identity list)
                      (:notice (or null string list))
                      (:boot-screen-p boolean))
    (option keyword))
(defun application-serve
    (application &key initial-command initial-input
                      adaptation-offer-p quiescer ready-function
                      resume-p world-terminal-identity notice (boot-screen-p t))
  "Serve APPLICATION's world-owned channel on this thread until it exits.

The world server owns process services and final resource cleanup. QUIESCER,
a function of one thunk, runs checkpoint work with every hosted channel
stopped. READY-FUNCTION, when supplied, is called with APPLICATION once its
endpoint is published.

A session that exits for a checkpoint keeps its resources: its endpoint
stops, so its thread can end for the fork, and the same application is
served again with RESUME-P, which skips the startup presentation and
recovery, WORLD-TERMINAL-IDENTITY, the :TOKEN, :CREATED-AT and
:CHECKPOINT-RECONNECT-ID its clients wait for, and NOTICE, presented once
the session shows again. Without BOOT-SCREEN-P the session opens without
the boot screen, as for a terminal that switched to it. Return the
controller's exit reason."
  (let* ((ui               (application-ui application))
         (input-controller nil)
         (exit-reason      nil))
    (unless (terminal-ui-fullscreen-p ui)
      (change-class ui 'fullscreen-terminal-ui))
    (when (terminal-ui-fullscreen-p ui)
      (setf (terminal-ui-fullscreen-welcome-p ui) t))
    (when (and (terminal-ui-fullscreen-p ui) (not resume-p))
      (setf (terminal-ui-live-output-suspended-p ui) t))
    (labels ((pause-resources ()
               "Stop only the session's own threads, keeping APPLICATION ready to serve again."
               (application-conversation-maintenance-stop application)
               ; mallet:suppress no-ignore-errors -- best-effort shutdown must close every runtime
               (ignore-errors (world-terminal-stop application))
               ; mallet:suppress no-ignore-errors -- a failed join must not keep the pause from completing
               (ignore-errors (application--quiesce-update-check application))
               nil)

             (close-runtime-resources ()
               "Pause for a checkpoint or leave final cleanup to the owning world server."
               (when (eq exit-reason ':checkpoint)
                 (return-from close-runtime-resources (pause-resources)))
               (application-assignment-finish application :stop-p t)
               ; mallet:suppress no-ignore-errors -- the owner retries mandatory cleanup
               (ignore-errors (world-terminal-stop application))
               nil)

             (finish-shutdown ()
               "Preserve accepted input and retire the controller while closing runtimes."
               (if input-controller
                   (progn
                     (application-input-controller--prepare-shutdown input-controller ':shutdown)
                     (unwind-protect (close-runtime-resources)
                       (application-input-controller-stop input-controller))
                     (setf input-controller nil))
                   (close-runtime-resources))))
      (unwind-protect
           (with-terminal-ui (active-ui ui)
             (unwind-protect
                  (progn
                    (application-sync-window-title application)
                    (unless resume-p
                      (when-let ((update-notice
                                   (application--update-notice application)))
                        (application-present application update-notice)))
                    (when-let ((provenance
                                 (application-installation-provenance application)))
                      (setf (application-update-check-thread application)
                            (update-check-start
                             (application-configuration application)
                             provenance)))
                    (unless resume-p
                      (dolist (failure
                               (application-mutation-replay-failures application))
                        (application-present
                         application
                         (application--transcript-entry
                          application
                          :style  ':failure
                          :header "! mutation replay skipped"
                          :body   (format nil "~A~%~A"
                                          (namestring (first failure))
                                          (rest failure)))))
                      (when-let ((skips (application-replay-skips-entry application)))
                        (application-present application skips)))
                    (application-render-records application)
                    (when initial-input
                      (terminal-ui-set-input ui initial-input))
                    (setf input-controller
                          (application-input-controller-create
                           application
                           :initial-work-items
                            (application--initial-work-items initial-command)
                           :project-adaptation-offer-p adaptation-offer-p
                           :start-reader-p             nil))
                    (application-assignment-begin input-controller)
                    (unless resume-p
                      (application-mission-schedules-recover application :startup-p t)
                      (application-recovery-input-vault-present-startup-warning
                       application)
                      (application-present-generation-health-warning
                       application))
                    (apply #'world-terminal-start application world-terminal-identity)
                    (when ready-function
                      (funcall ready-function application))
                    (if resume-p
                        (when notice
                          (application-present application notice))
                        (application-startup-presentation
                         application input-controller
                          :onboarding-p t
                         :boot-screen-p boot-screen-p))
                    (when (and (terminal-ui-fullscreen-p ui) (not resume-p))
                      (with-terminal-ui-locked (ui)
                        (setf (terminal-ui-live-output-suspended-p ui) nil)
                        (terminal-ui--paint-live ui)))
                    (when (and (not resume-p) (null initial-command) (null initial-input))
                      (application--offer-startup-update application))
                    (application-input-controller--open-prompt-if-ready
                     input-controller)
                    (application-input-controller--start-reader
                     input-controller)
                    (application-assignment-finish application)
                    (application-conversation-maintenance-start application :resume-p resume-p)
                    ;; A channel served for a message, or opened while
                    ;; messages waited, reads them now.
                    (handler-case (application-world-deliver application)
                      (error (cause)
                        (warn 'world-session-warning
                              :pathname (configuration-world-path
                                         (application-configuration application))
                              :cause    cause)))
                    ;; Entering the interactive debugger would hang the raw
                    ;; terminal, so debugger entry becomes fatal recovery.
                    (let ((*checkpoint-thread-quiescer* quiescer)
                          (sb-ext:*invoke-debugger-hook*
                            (application--fatal-debugger-hook application)))
                      (handler-case
                          (loop
                            for work =
                              (application-input-controller--next-work
                               input-controller)
                            while work
                            do (handler-case
                                   (let ((*application-turn-cancellation-deliverable-p* t))
                                       (unwind-protect
                                            (progn
                                               (application-world-note-child-turn application (first work))
                                              ;; A cancellation requested before WORK reached
                                              ;; this handler stayed pending; deliver it here.
                                              (when
                                                  (application-input-controller--consume-turn-cancellation-delivery-p
                                                   input-controller)
                                                (error
                                                 (make-condition
                                                  'application-turn-cancelled)))
                                              (application-input-controller--run-work
                                               input-controller work)
                                              (when
                                                  (application-input-controller--consume-turn-cancellation-delivery-p
                                                   input-controller)
                                                (error
                                                 (make-condition
                                                  'application-turn-cancelled))))
                                          (unwind-protect
                                               (application-input-controller--finish-work input-controller)
                                            (application-world-note-child-turn application (first work))))
                                       (application--finish-checkpoint-restart
                                        application))
                                 (application-turn-cancelled ()
                                   (conversation--repair-incomplete-tool-calls
                                    (application-conversation application))
                                   nil)))
                        (application-input-failed (condition)
                          (application-raise-fatal
                           application
                           (application-input-failed-original-condition
                            condition)
                           (application-input-failed-backtrace condition)))))
                    (setf exit-reason
                          (application-input-controller-exit-reason input-controller))
                    (when (member exit-reason '(:interrupt :end-of-input))
                      (application--present-open-instruction application))
                    (unless (eq exit-reason ':checkpoint)
                      (world-terminal-finish-attachments application :status 0)))
               (finish-shutdown)))
        (finish-shutdown)))
    exit-reason))


;;;; -- Command-Line Entry --

(defparameter *main-fatal-status* 70
  "The process status for a fatal session failure.")

(defparameter *main-rollback-status* 75
  "The process status asking the stable launcher to start a selected rollback.")

(defparameter *main-update-request-status* 76
  "The process status asking a packaged outer launcher to perform an update.")

(deftype main-launcher-exit-condition ()
  "A condition ending the session with a status the stable launcher acts on."
  '(or update-requested rollback-requested fatal-control-path-error))

(defgeneric main--launcher-exit (condition)
  (:documentation
   "Return CONDITION's exit status, terminal message and style, and optional
exit details for attached clients."))

(defmethod main--launcher-exit ((condition update-requested))
  "Ask the packaged outer launcher to install the requested release."
  (values *main-update-request-status*
          (format nil "~A will update to ~A after restoring the terminal."
                  (product-name) (subseq (update-requested-tag condition) 1))
          ':notice))

(defmethod main--launcher-exit ((condition rollback-requested))
  "Ask the stable launcher to start the selected retained generation."
  (values *main-rollback-status*
          (format nil "~A is rolling back to retained generation ~A."
                  (product-name) (rollback-requested-generation-id condition))
          ':notice))

(defmethod main--launcher-exit ((condition fatal-control-path-error))
  "Report the crash capsule and end the session."
  (let ((capsule (fatal-control-path-error-capsule-pathname condition)))
    (values *main-fatal-status*
            (format nil
                    "~A stopped after a fatal error.~@[~%Crash capsule: ~A~]~%If startup fails or self-modifications cause instability, run ~A --pristine."
                    (product-name) capsule (product-command-name))
            ':failure
            (when capsule (list :crash-capsule (namestring capsule))))))

(-> main--launcher-exit-text (condition) (values integer string list))
(defun main--launcher-exit-text (condition)
  "Return CONDITION's launcher status, its message styled for the session terminal,
and its exit details.

The message is printed after the terminal UI is gone, so it is rendered for the
active application's terminal, plain when that terminal is not styled."
  (multiple-value-bind (status message style details) (main--launcher-exit condition)
    (values status
            (if *active-application*
                (terminal--render-spans
                 (terminal-ui-terminal (application-ui *active-application*))
                 (list (terminal-span style message)))
                message)
            details)))

(-> main--authentication-provider (configuration (option string)) model-provider)
(defun main--authentication-provider (configuration selection)
  "Return a provider for the active or explicitly selected registration."
  (if selection
      (provider-authentication-provider configuration selection)
      (provider-create configuration)))

(-> main--output-styled-p (stream) boolean)
(defun main--output-styled-p (stream)
  "Return true when command-line output to STREAM may be styled."
  (and (interactive-stream-p stream)
       (terminal-environment-styling-p)))

(defparameter *user-source-tree-skipped-directories*
  '("_build" "dist" "node_modules")
  "Directory leaves never searched for system definitions in user trees.
Dot-prefixed directories such as .git and .qlot are always skipped.")

(-> main--user-source-trees () list)
(defun main--user-source-trees ()
  "Return the conventional user source trees present on this host."
  (remove-if-not
   #'uiop:directory-exists-p
   (list (merge-pathnames "common-lisp/" (user-homedir-pathname))
         (merge-pathnames "quicklisp/local-projects/"
                          (user-homedir-pathname)))))

(-> main--find-system-definition-under (string pathname) (option pathname))
(defun main--find-system-definition-under (name root)
  "Return the first NAME.asd at or beneath directory ROOT, or NIL."
  (let ((definition (make-pathname :name name :type "asd")))
    (labels ((walk (directory)
               (let ((candidate (merge-pathnames definition directory)))
                 (when (uiop:file-exists-p candidate)
                   (return-from main--find-system-definition-under
                     candidate)))
               (dolist (subdirectory (uiop:subdirectories directory))
                 (let ((leaf (lastcar (pathname-directory subdirectory))))
                   (unless (or (not (stringp leaf))
                               (zerop (length leaf))
                               (char= (char leaf 0) #\.)
                               (member leaf *user-source-tree-skipped-directories*
                                       :test #'string=))
                     (walk subdirectory))))))
      (walk root)
      nil)))

(-> main--locate-user-tree-system (t &optional list) (option pathname))
(defun main--locate-user-tree-system
    (name &optional (trees (main--user-source-trees)))
  "Locate NAME's system definition in the user source TREES.

The locked dependency environment replaces ASDF's default configuration,
so ~/common-lisp and ~/quicklisp/local-projects would otherwise be
invisible to REPL forms like (ql:quickload ...). This locator runs after
every other ASDF search and never resolves a system that is already
registered, so user trees cannot shadow Autolith or its locked
dependencies."
  (handler-case
      (let ((primary (asdf:primary-system-name
                      (string-downcase (string name)))))
        (unless (asdf:registered-system primary)
          (loop for tree in trees
                  thereis (main--find-system-definition-under primary tree))))
    (error ()
      nil)))

(-> main--register-local-source-trees () null)
(defun main--register-local-source-trees ()
  "Let REPL system lookups fall back to the conventional user source trees."
  (unless (member 'main--locate-user-tree-system
                  asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'main--locate-user-tree-system))))
  nil)

(-> main-authenticate
    (configuration (option string) &optional (option keyword))
    null)
(defun main-authenticate (configuration selection &optional method)
  "Authenticate a registered provider using native METHOD before the UI starts."
  (let ((provider (main--authentication-provider configuration selection)))
    (provider--authentication-method provider method)
    (configuration-ensure-directories configuration)
    (let ((*api-key-input-file-descriptor* (terminal-standard-input-file-descriptor))
          (*api-key-output-styled-p*       (main--output-styled-p *standard-output*)))
      (format t "~&~A~%"
              (provider-authenticate-with-method
               provider method :stream *standard-output* :open-browser-p t))
      (when-let ((model (provider-authenticated-model configuration provider)))
        (setf (config :model configuration) model)
        (format t "~&Active provider: ~A; model: ~A.~%"
                (provider-model-provider-name model) model))))
  nil)

(-> main--site-root-arguments (clingon:command) list)
(defun main--site-root-arguments (command)
  "Return an explicit site-root override only when COMMAND supplies one."
  (let ((value (getopt* command ':site-config-root)))
    (when (non-empty-string-p value)
      (list :site-config-root (pathname value)))))

(-> main--model-arguments (clingon:command) list)
(defun main--model-arguments (command)
  "Return transient model overrides, leaving provider validation to user init."
  (let ((model    (getopt* command ':model))
        (provider (getopt* command ':provider)))
    (when (and provider (not model))
      (error 'configuration-error :message "--provider requires --model MODEL."))
    (dolist (entry (list (cons "--model" model) (cons "--provider" provider)))
      (when (and (rest entry)
                 (or (not (non-empty-string-p (string-trim '(#\Space #\Tab #\Newline)
                                                          (rest entry))))
                     (uiop:string-prefix-p "--" (rest entry))))
        (error 'configuration-error
               :message (format nil "~A requires a nonempty value." (first entry)))))
    (when model
      (list :model model :model-provider (or provider "")
            :model-selection-override-p t))))

(-> main--start-session
    (clingon:command
     &key (:open-requested-p boolean)
          (:open-id (option string))
          (:new-directory (option pathname))
          (:authenticate-p boolean)
          (:authentication-selection (option string))
          (:authentication-method (option keyword)))
    null)
(defun main--start-session
    (command &key open-requested-p open-id new-directory authenticate-p
                  authentication-selection authentication-method)
  "Start an Autolith session from COMMAND and create a new channel for NEW-DIRECTORY when supplied."
  (main--register-local-source-trees)
  (setf *world-image-selection*
        (cond
          ((getopt* command ':pristine)
           "pristine")
          (*checkpoint-boot-generation*
           (format nil "generation-~A" (generation-identifier *checkpoint-boot-generation*)))
          (t
           nil)))
  (let* ((pristine-p               (not (null (getopt* command ':pristine))))
         (read-only-p              (and open-requested-p (getopt command ':read-only)))
         (attachment-mode          (if read-only-p ':read-only ':control))
         (immutable-p              (or pristine-p
                                       (not (null (getopt* command ':immutable)))))
         (explicit-permission-mode (getopt* command ':permissions))
         (configuration
            (let ((base (apply #'configuration-create
                               :immutable-p                 immutable-p
                               :defer-provider-validation-p t
                               (append (main--site-root-arguments command)
                                       (when new-directory
                                         (list :working-directory new-directory))
                                       (main--model-arguments command)))))
              (terminal-theme-install (config :terminal-theme base))
              base))
         (permission-mode          (or (config :permission-mode configuration) ':ask))
         (handoff-record
           (world-startup-selection
            configuration
            (getopt* command ':world-startup)))
         (effective-open-requested-p
           (and (null handoff-record) open-requested-p))
         (effective-open-id
           (when (and (null handoff-record) open-id)
             (main-resolve-conversation configuration open-id)))
         (conversation-id          effective-open-id)
         (latest-p                 (and effective-open-requested-p (null conversation-id)))
         (inbox-p                  (not (or effective-open-requested-p conversation-id handoff-record new-directory)))
         (image-state-loaded-p
           (and authenticate-p
                (or pristine-p handoff-record
                    (not (typep *active-application* 'application))))))
    (when image-state-loaded-p
      (image-state-load configuration :pristine-p pristine-p))
    (when authenticate-p
      (unless pristine-p
        (user-init-load configuration))
      (main-authenticate (provider-bootstrap-configuration configuration)
                         authentication-selection
                         authentication-method))
    (when handoff-record
      (world-startup-begin-startup handoff-record))
    (when handoff-record
      (let* ((*world-startup-record* handoff-record)
             (status                 (world-server-run configuration
                                                       :permission-mode
                                                       (or explicit-permission-mode permission-mode))))
        (when status
          (uiop:quit status)))
      (return-from main--start-session nil))
    (when read-only-p
      (unless (main--interactive-input-p)
        (error 'configuration-error :message "Read-only opening requires an interactive terminal."))
      (when explicit-permission-mode
        (error 'configuration-error :message "--read-only cannot be combined with --permissions.")))
    (when (and (string-equal (software-type) "Linux")
               (not (application--command-sandbox-available-p)))
      (format *error-output* "~&~A: ~A~%"
              (product-name) (application--command-sandbox-unavailable-message))
      (force-output *error-output*))
    (unless (platform-supports-p *platform* ':detached-sessions)
      (error 'platform-capability-unavailable
             :capability ':detached-sessions
             :message    "Starting a world server requires detached-session support."))
    (if (main--interactive-input-p)
        (main--run-world-client
         configuration
         :permission-mode permission-mode
         :immutable-p     immutable-p
         :conversation-id conversation-id
         :attachment-mode attachment-mode
         :new-p           (not (null new-directory))
         :inbox-p         inbox-p
         :latest-p        latest-p
         :welcome-p       inbox-p
         :launch          (main--launch-options
                           command :permission-mode explicit-permission-mode))
        (apply #'world-client-run-script configuration
               :conversation-id conversation-id
               :permission-mode permission-mode
               :immutable-p     immutable-p
               :launch          (main--launch-options
                                 command :permission-mode explicit-permission-mode)
               (when latest-p (list :latest-p t)))))
  nil)

(-> main--interactive-input-p () boolean)
(defun main--interactive-input-p ()
  "Return true when standard input is a terminal someone types at, not piped input."
  (not (null (interactive-stream-p *standard-input*))))

(-> main--open-world-channel
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:conversation-id (option string))
                        (:pick-channel-p boolean)
                        (:inbox-p boolean) (:latest-p boolean) (:attach-expected-p boolean)
                        (:launch list)
                        (:new-p boolean))
    (values string boolean))
(defun main--open-world-channel
    (configuration &key (permission-mode ':ask) immutable-p conversation-id
                        pick-channel-p launch new-p inbox-p latest-p
                        (attach-expected-p t))
  "Open this terminal's channel in the world server and return its session identifier.

CONVERSATION-ID opens that channel; otherwise the launch directory selects
or creates one, and NEW-P creates one. The second value says whether the
server started the channel for
this terminal. When ATTACH-EXPECTED-P is false, prepare the channel without
a client view. A server that refuses the open, or cannot be started or reached,
ends the start with the reason; no session runs in this terminal instead."
  (handler-case
      (world-client-open configuration
                         :directory         (config :working-directory configuration)
                         :channel           conversation-id
                         :permission-mode   permission-mode
                         :immutable-p       immutable-p
                         :pick-channel-p    pick-channel-p
                         :inbox-p           inbox-p
                         :latest-p          latest-p
                         :attach-expected-p attach-expected-p
                         :switch-p          (not attach-expected-p)
                         :launch            launch
                         :new-p             new-p)
    (error (condition)
      (when (main--world-refusal-p condition)
        (error condition))
      (error 'world-terminal-error
             :message   (format nil "The world server is not available: ~A"
                                (bounded-string (princ-to-string condition) :limit 400))
             :operation ':open))))

(-> main--world-refusal-p (condition) boolean)
(defun main--world-refusal-p (condition)
  "Return true when CONDITION is the world server's answer refusing an open, not a failure to reach it."
  (and (typep condition 'world-terminal-error)
       (eq (image-daemon:daemon-error-operation condition) ':open)
       (null (image-daemon:daemon-error-cause condition))
       (not (world-terminal-error-retry-p condition))))

(-> main--launch-options (clingon:command &key (:permission-mode (option keyword))) list)
(defun main--launch-options (command &key permission-mode)
  "Return the launch options COMMAND states explicitly, as a world server applies them to a channel.

Only options given on the command line appear, PERMISSION-MODE among them
when --permissions was, so a channel the server already serves can tell a
request it conflicts with from one that says nothing."
  (append (when-let ((model (getopt* command ':model)))
            (list :model model))
          (when-let ((provider (getopt* command ':provider)))
            (list :provider provider))
          (when (and permission-mode (getopt* command ':permissions))
            (list :permission-mode permission-mode))
          (when (getopt* command ':immutable)
            (list :immutable-p t))))

(-> main--switch-failure (string condition) string)
(defun main--switch-failure (identifier condition)
  "Return what the terminal is told when its switch to channel IDENTIFIER failed with CONDITION."
  (format nil "could not switch to ~A: ~A"
          (conversation-identifier-display identifier) condition))

(-> main--call-with-channel-wait (world-terminal-screen function) t)
(defun main--call-with-channel-wait (screen function)
  "Wait for one interactive switch on its owner while a scoped pump serves the terminal.

Escape and Ctrl-C unwind the request through normal transport cleanup. The
pump is joined before returning; queued interrupts check their retired scope."
  (let* ((terminal    (world-terminal-screen-terminal screen))
         (owner       (current-thread))
         (lock        (make-lock "Autolith channel switch wait"))
         (active-p    t)
         (cancelled-p nil)
         (resized-p   nil)
         (watch       nil)
         (pump        nil))
    (labels ((active-p ()
               (with-lock-held (lock) active-p))

             (cancelled-p ()
               (with-lock-held (lock) cancelled-p))

             (wake-owner (condition)
               (interrupt-thread owner
                                 (lambda ()
                                   (when active-p
                                     (error condition)))))

             (serve-terminal ()
               (handler-case
                   (loop while (active-p)
                         do (when (terminal-input-ready-p terminal)
                              (when (member (terminal-read-event terminal)
                                            '(:escape :interrupt :abort :detach :eof))
                                (with-lock-held (lock) (setf cancelled-p t))
                                (wake-owner (make-condition 'world-terminal-client-cancelled
                                                            :operation ':switch
                                                            :message   "Channel switch cancelled."))
                                (return)))
                            (when (with-lock-held (lock) (shiftf resized-p nil))
                              (multiple-value-bind (rows columns) (terminal-current-size)
                                (terminal-set-dimensions terminal columns :rows rows))
                              (world-terminal-screen-resized screen))
                            (world-terminal-screen-tick screen)
                            (sleep 0.025))
                 (error (condition)
                   (wake-owner condition)))))
      (handler-case
          (unwind-protect
               (let ((*world-terminal-client-cancelled-p*           #'cancelled-p)
                     (image-daemon:*daemon-connect-timeout-seconds* nil))
                 (setf watch (platform-watch-terminal-resize
                              *platform* (lambda ()
                                           (with-lock-held (lock) (setf resized-p t))))
                       pump (make-thread #'serve-terminal :name "Autolith channel switch terminal"))
                 (multiple-value-prog1 (funcall function)
                   (world-terminal-client-check-cancellation)))
            (with-lock-held (lock) (setf active-p nil))
            (when pump (join-thread pump))
            (when watch
              ; mallet:suppress no-ignore-errors -- releasing the watch must not mask the switch result
              (ignore-errors (platform-unwatch-terminal-resize *platform* watch))))
        (error (condition)
          (if (cancelled-p)
              (error 'world-terminal-client-cancelled :operation ':switch :message "Channel switch cancelled.")
              (error condition)))))))

(-> main--call-with-channel-loading
    (world-terminal-screen string function
     &key (:start-p boolean) (:boot-p boolean) (:agent-name (option string)) (:cancellable-p boolean)) t)
(defun main--call-with-channel-loading
    (screen title function &key start-p boot-p agent-name cancellable-p)
  "Call FUNCTION with loading milestones, optionally serving a cancellable switch wait."
  (flet ((description (text)
           (if cancellable-p (format nil "~A (Esc: return)" text) text)))
    (world-terminal-screen-show-loading screen title :start-p start-p :boot-p boot-p
                                                :agent-name  agent-name
                                                :description (description "Connecting to world"))
    (handler-bind
        ((channel-loading-progress
           (lambda (progress)
             (world-terminal-client-check-cancellation)
             (world-terminal-screen-show-loading
              screen title :step        (channel-loading-progress-step progress)
                           :description (description (channel-loading-progress-description progress))
                           :agent-name  agent-name))))
      (if cancellable-p
          (main--call-with-channel-wait screen function)
          (funcall function)))))

(-> main--attach-world-channel
    (configuration string keyword
     &key (:screen world-terminal-screen) (:notice (option string)) (:client-version (option string))
          (:attached-function (option function)) (:wait-p boolean) (:initial-input list))
    list)
(defun main--attach-world-channel
    (configuration identifier mode &key screen notice client-version attached-function wait-p initial-input)
  "Show channel IDENTIFIER on SCREEN until this terminal leaves it, and return why it left.

The terminal follows the channel as a client that renders it: a view applies
what the channel presents and sends it the user's input. MODE :READ-ONLY
watches without sending any. NOTICE is shown in the transcript once attached,
and so is a note when CLIENT-VERSION differs from the server's. A screen that
still shows the boot panel keeps it until the user starts the session."
  (channel-view-run (channel-view-create configuration identifier screen
                                         :read-only-p (eq mode ':read-only))
                    :notice             notice
                    :client-version     client-version
                    :boot-p             (not (null (world-terminal-screen-boot-ui screen)))
                    :wait-function      (when wait-p
                                          (lambda (function)
                                            (main--call-with-channel-wait screen function)))
                     :attached-function attached-function
                     :initial-input     initial-input))

(-> main--welcome-agent-counts (world string) (values integer integer))
(defun main--welcome-agent-counts (world directory)
  "Return durable agent totals and the number residing in DIRECTORY, including idle agents."
  (loop for participant in (world-participants world)
        for agent-p = (eq (participant-kind participant) ':agent)
        for residence = (and agent-p (participant-residence participant))
        for channel = (and residence (world-channel-find world residence))
        count agent-p into total
        count (and channel
                   (equal (uiop:ensure-directory-pathname directory)
                          (uiop:ensure-directory-pathname (channel-workspace channel)))) into local
        finally (return (values total local))))

(-> main--welcome-frame
    (world-terminal-screen string &key (:total (option integer)) (:local (option integer))
     (:immutable-p boolean) (:tip list) (:height-limit (option integer)) (:notice (option string)))
    clinedi:frame-snapshot)
(defun main--welcome-frame
    (screen directory &key total local immutable-p tip notice height-limit)
  "Render safe client startup information centered in the current pane."
  (multiple-value-bind (height width) (world-terminal-screen-pane-size screen)
    (let* ((height        (if height-limit (min height height-limit) height))
           (content-width (clamp width 1 88))
           (terminal      (world-terminal-screen-terminal screen))
           (title
             (list (terminal-span ':brand (format nil "~:@(~A~) v~A" (product-name) *autolith-version*))))
           (details
             (list* (application--banner-metadata-field "workspace"
                     (sanitize-text directory :single-line-p t))
                    (application--banner-metadata-field
                     "agents" (if total (format nil "~D (~D here)" total local)
                                  "catalogue unavailable"))
                    (when immutable-p
                      (list (application--banner-metadata-field "mode" "immutable")))))
           (divider-width
             (min content-width
                  (loop for row in (cons title details) maximize (termdown:spans-width row))))
           (metadata
             (list* title (list (terminal-span ':dim (make-string divider-width :initial-element #\─)))
                    details))
           (header
             (cond
               ((>= content-width (+ (application--banner-logo-width)
                                     (text-cell-width *application-banner-gap*)
                                     *application-banner-minimum-metadata-width*))
                (application--banner-side-by-side-spans metadata content-width))
               (t
                (application--banner-stacked-spans metadata content-width))))
           (spans
             (append header
               (list (terminal-span ':dim "Sidebar: select channel. Enter: new. Ctrl-C/Z: quit.")
                    (terminal-span ':plain (format nil "~%")))
              (when notice
                (list (terminal-span ':notice
                       (format nil "~A~%" (sanitize-text (bounded-string notice :limit 200)
                                                        :single-line-p t))))))))
      (labels ((rows (content)
                 (wrap-styled-text (terminal--spans-text content)
                                   (terminal--render-spans terminal content) content-width)))
        (let* ((lines    (rows spans))
               (warning  (rows (list (terminal-span ':notice
                                      (format nil "! ~A executes model-generated code with your user privileges. Sandboxing is no substitute for human oversight"
                                              (product-command-name))))))
               (tip-rows (when tip (rows tip))))
          (when (> (length lines) height)
            (setf lines
                  (rows (append
                         (loop for row in metadata append (application--banner-terminate-row row))
                         (nthcdr (length header) spans)))))
          (when (<= (+ (length lines) 1 (length warning)) height)
            (setf lines (append lines (list (list "" "")) warning)))
          (when (and tip-rows (<= (+ (length lines) 1 (length tip-rows)) height))
            (setf lines (append lines (list (list "" "")) tip-rows)))
          (let* ((block-width (loop for (text) in lines maximize (text-cell-width text)))
                 (top         (max 0 (floor (- height (length lines)) 2)))
                 (left        (make-string (max 0 (floor (- width block-width) 2))
                                           :initial-element #\Space)))
            (clinedi:frame-snapshot-apply
             nil (loop for line in lines for row from top below height
                       collect (list row (concatenate 'string left (second line))))
             :height height :width width :complete-p t)))))))

(-> main--welcome-world-client
    (world-terminal-screen configuration string
     &key (:immutable-p boolean) (:notice (option string))
     (:launch list) (:input t))
    (option list))
(defun main--welcome-world-client
    (screen configuration directory &key immutable-p notice launch input)
  "Show startup information above the normal composer and retain its first input for a new channel."
  (let* ((overrides     (loop for key in '(:model :provider)
                              for value = (getf launch key)
                              when value append (list key value)))
         (configuration (apply #'configuration-copy configuration
                               :working-directory (pathname directory) overrides))
         (tip           (application--startup-tip-spans))
         (total         nil)
         (local         nil))
    (handler-case
        (multiple-value-setq (total local)
          (main--welcome-agent-counts (world-load configuration) directory))
      (world-error ()
        nil))
    (welcome-view-run
     screen configuration
     (lambda (height)
       (main--welcome-frame screen directory :total total :local local :immutable-p immutable-p
                           :tip tip :notice notice
                            :height-limit height))
     :input input)))

(-> main--run-world-client
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
     (:conversation-id (option string)) (:attachment-mode (member :control :read-only))
     (:pick-channel-p boolean) (:new-p boolean) (:inbox-p boolean) (:latest-p boolean) (:welcome-p boolean)
     (:launch list))
    (values boolean list))
(defun main--run-world-client
       (configuration
        &key (permission-mode ':ask) immutable-p conversation-id (attachment-mode ':control)
        pick-channel-p new-p inbox-p latest-p welcome-p launch)
  "Serve one terminal across channel switches, committing navigation after attachment acknowledgment.
Interactive switches wait for discovery and attachment until completion or cancellation.
A failed switch restores its source, or reports a disconnected terminal if that source disappeared.
Return success and bounded secondary cleanup metadata without replacing a primary condition or exit."
  (let* ((panel            (world-sidebar-create configuration))
         (screen           (world-terminal-client-screen-create :panel panel))
         (launch-directory (namestring (uiop:getcwd)))
         (last-channel     nil)
         (final-exit       nil)
         (cleanup-failures nil))
    (unwind-protect
        (progn
         (unless welcome-p
           (world-sidebar-start panel))
         (multiple-value-bind (session-id started-p)
                (main--call-with-channel-loading
                 screen (if welcome-p "Loading home" "Opening channel")
                 (lambda ()
                   (if welcome-p
                       (progn
                         (main--open-world-channel configuration
                                                   :permission-mode   permission-mode
                                                   :immutable-p       immutable-p
                                                   :inbox-p           t
                                                   :attach-expected-p nil)
                         (values nil nil))
                       (main--open-world-channel configuration
                                                 :permission-mode permission-mode
                                                 :immutable-p     immutable-p
                                                 :conversation-id conversation-id
                                                 :pick-channel-p  pick-channel-p
                                                 :new-p           new-p
                                                 :inbox-p         inbox-p
                                                 :latest-p        latest-p
                                                 :launch          launch)))
                 :start-p (not (null (terminal--terminal-mode-or-nil
                                      (world-terminal-screen-terminal screen))))
                 :boot-p  (config :boot-screen-p configuration))
           (when welcome-p
             (world-sidebar-start panel))
           (loop with origin = nil
                 with switching-p = nil
                 with restoring-p = nil
                 with client-version = *autolith-version*
                 with notice = nil
                 with pending-input = nil
                 do (let ((exit
                           (handler-case
                             (if (null session-id)
                                 (main--welcome-world-client
                                  screen configuration launch-directory
                                  :immutable-p immutable-p :notice (shiftf notice nil)
                                  :launch launch :input (getf pending-input :payload))
                               (prog1
                                   (main--attach-world-channel configuration session-id attachment-mode
                                                           :initial-input pending-input
                                                          :screen screen :notice
                                                          (shiftf notice nil) :client-version
                                                          (shiftf client-version nil)
                                                          :wait-p         switching-p
                                                          :attached-function
                                                          (lambda ()
                                                            (setf (world-sidebar-current panel)
                                                                  session-id
                                                                  last-channel session-id
                                                                  origin nil
                                                                  switching-p nil
                                                                  restoring-p nil
                                                                  started-p nil)))
                                 (setf pending-input nil)))
                            (error (condition)
                                   (when started-p
                                     ; mallet:suppress no-ignore-errors -- preserve attachment failure while closing an unattached new channel
                                     (ignore-errors
                                      (when-let ((world (world-client-entry configuration)))
                                        (world-terminal-query-record world ':close
                                                                 (list :channel           session-id
                                                                       :unattached-only-p t)))))
                                   (cond
                                    (switching-p
                                     (setf notice (main--switch-failure session-id condition))
                                     (list ':return))
                                    (restoring-p
                                     (setf (world-sidebar-current panel) nil
                                           last-channel nil)
                                     (world-terminal-screen-show-loading screen "Disconnected")
                                     (list ':disconnected :status 1 :message
                                           (format nil
                                                   "Disconnected: the source channel is no longer available. ~A"
                                                   condition)))
                                    (t
                                     (error condition)))))))
                       (cond
                        ((and (consp exit) (eq (first exit) ':attachment-lost))
                         (setf final-exit
                               (list :status 1 :message
                                     (format nil "Channel ~A disconnected: ~A"
                                             (conversation-identifier-display session-id)
                                             (bounded-string
                                              (or (getf (rest exit) :message) "The attachment was lost.")
                                              :limit 400))))
                         (return))
                        ((and (consp exit) (eq (first exit) ':return))
                         (setf session-id origin
                               restoring-p (not (null origin))
                               origin nil
                               switching-p nil
                               started-p nil))
                        ((and (consp exit) (eq (first exit) ':disconnected))
                         (setf final-exit (rest exit)) (return))
                        ((and (consp exit) (eq (first exit) ':switch))
                         (let ((left session-id) (target (getf (rest exit) :session-id)))
                           (handler-case
                            (progn
                             (setf pending-input (getf (rest exit) :initial-input))
                             (multiple-value-setq (session-id started-p)
                                (main--call-with-channel-loading
                                 screen (if target "Loading channel" "Opening channel")
                                 (lambda ()
                                   (world-client-open configuration
                                                      :channel         target
                                                      :directory       (unless left launch-directory)
                                                      :new-p           (getf (rest exit) :new-p)
                                                      :launch          (unless left launch)
                                                      :permission-mode permission-mode
                                                      :immutable-p     immutable-p
                                                      :switch-p        (not (null target))))
                                 :cancellable-p t
                                 :agent-name    (when target (world-sidebar-agent-name panel target))))
                             (setf origin left switching-p t))
                            (error (condition)
                                   (setf notice (if target
                                                    (main--switch-failure target condition)
                                                    (format nil "Could not open a channel: ~A" condition))
                                         session-id left
                                         origin nil
                                         switching-p nil
                                         started-p nil
                                         restoring-p (not (null left)))))))
                        (t
                         (setf final-exit exit) (return)))))))
      (flet ((cleanup (stage function)
               (handler-case (funcall function)
                             (error (condition)
                                    (push (world-terminal--client-cleanup-failure stage condition)
                                          cleanup-failures)))))
        (cleanup "client-screen" (lambda () (world-terminal-client-screen-release screen)))
        (cleanup "sidebar" (lambda () (world-sidebar-stop panel))))
      (setf cleanup-failures (nreverse cleanup-failures)))
    (world-terminal-relay-exit final-exit attachment-mode :session-id last-channel)
    (values t (when cleanup-failures (list :cleanup-failures cleanup-failures)))))

(-> main--session-options () list)
(defun main--session-options ()
  "Return the persistent session options shared with every sub-command."
  (list
   (make-option ':string
                :long-name   "model"
                :key         ':model
                :parameter   "MODEL"
                :persistent  t
                :description "select a model for this launch without saving preferences")
   (make-option ':string
                :long-name   "provider"
                :key         ':provider
                :parameter   "PROVIDER"
                :persistent  t
                :description "select the registered provider for --model")
   (make-option ':flag
                :long-name   "immutable"
                :key         ':immutable
                :persistent  t
                :description "disable source and configuration mutation")
   (make-option ':flag
                :long-name   "pristine"
                :key         ':pristine
                :persistent  t
                :description "boot tracked source without private mutations or user init")
   (make-option ':string
                :long-name   "site-config-root"
                :key         ':site-config-root
                :parameter   "DIRECTORY"
                :persistent  t
                :description "load site configuration before user configuration")
   (make-option ':enum
                :long-name   "permissions"
                :key         ':permissions
                :parameter   "MODE"
                :persistent  t
                :items       '(("ask" . :ask)
                               ("auto" . :auto)
                               ("sandbox" . :sandboxed)
                               ("full" . :full-access))
                :description "initial command authorization mode")
   (make-option ':string
                :long-name   "world-startup"
                :key         ':world-startup
                :parameter   "FILE"
                :persistent  t
                :hidden      t
                :description "claim an internal world-terminal handoff record")))

(-> main--single-selection (clingon:command string) (option string))
(defun main--single-selection (command description)
  "Return COMMAND's optional single positional argument named by DESCRIPTION."
  (let ((arguments (command-arguments command)))
    (when (rest arguments)
      (error 'configuration-error
             :message (format nil "~A accepts at most one ~A."
                              (command-name command)
                              description)))
    (let ((candidate (first arguments)))
      (and (non-empty-string-p candidate)
           candidate))))

(-> main--open-command () clingon:command)
(defun main--open-command ()
  "Return the open sub-command definition."
  (make-command
   :name        "open"
   :description "open a channel, defaulting to the latest in this workspace"
   :usage       "[TARGET] [--read-only]"
   :options     (list (make-option ':flag
                                   :long-name   "read-only"
                                   :key         ':read-only
                                   :description "observe without control"))
   :handler
   (lambda (command)
     (main--start-session
      command
      :open-requested-p t
       :open-id         (progn
                            (main--single-selection command "channel or agent designator")
                            (first (command-arguments command)))))))

(-> main--new-command () clingon:command)
(defun main--new-command ()
  "Return the command that creates a channel and attaches unless asked not to."
  (make-command
   :name        "new"
   :description "create and open a new channel"
   :usage       "[DIRECTORY] [-d|--detached]"
   :options     (list (make-option ':flag
                                   :short-name  #\d
                                   :long-name   "detached"
                                   :key         ':detached
                                   :description "create the channel without attaching"))
   :handler
   (lambda (command)
     (let* ((argument  (main--single-selection command "directory"))
            (directory (if argument
                           (merge-pathnames (platform-pathname argument) (uiop:getcwd))
                           (uiop:getcwd))))
       (unless (uiop:directory-exists-p directory)
         (error 'configuration-error :message
                (format nil "The directory ~A does not exist." directory)))
       (if (getopt command ':detached)
           (let* ((configuration (world-terminal--client-configuration))
                  (identifier    (world-client-open
                                  configuration
                                  :directory         directory
                                  :permission-mode   (or (config :permission-mode configuration) ':ask)
                                  :new-p             t
                                  :attach-expected-p nil)))
             (world-terminal--write-styled-line
              *standard-output* (channel-open-instruction identifier)
              (world-terminal--status-output-styled-p *standard-output*)))
           (main--start-session command :new-directory directory))))))

(-> main--auth-command () clingon:command)
(defun main--auth-command ()
  "Return the auth sub-command definition."
  (make-command
   :name        "auth"
   :description "authenticate a provider, then start a session"
   :usage       "[PROVIDER] [METHOD]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (when (> (length arguments) 2)
         (error 'configuration-error
                :message "Auth accepts a provider and optional method."))
       (main--start-session
        command
        :authenticate-p           t
        :authentication-selection (first arguments)
        :authentication-method
        (provider--parse-authentication-method (second arguments)))))))

(-> main--run-job-command () clingon:command)
(defun main--run-job-command ()
  "Return the non-interactive single-job command definition."
  (make-command :name "run-job" :description
                "run one contracted child-agent job without a terminal" :options
                (list
                 (make-option ':string :long-name "input" :key ':input :parameter "FILE"
                              :description "data-only job S-expression")
                 (make-option ':string :long-name "prompt" :key ':prompt :parameter
                              "TEXT" :description
                              "inline job prompt, or - to read standard input")
                 (make-option ':string :long-name "id" :key ':job-id :parameter "ID"
                              :description "inline prompt job identifier")
                 (make-option ':string :long-name "output" :key ':output :parameter
                               "FILE" :description "atomically installed terminal result"))
                :handler
                (lambda (command)
                  (when (command-arguments command)
                    (error 'configuration-error :message
                           "run-job accepts options only; use --input FILE or --prompt TEXT, with --output FILE."))
                  (let* ((input         (getopt* command ':input))
                         (prompt-option (getopt* command ':prompt))
                         (prompt-p      (not (null prompt-option)))
                         (id-option     (getopt* command ':job-id))
                         (output        (getopt* command ':output)))
                    (when (and input prompt-p)
                      (error 'configuration-error :message
                             "run-job accepts --input or --prompt, not both."))
                    (unless (or (non-empty-string-p input) prompt-p)
                      (error 'configuration-error :message
                             "run-job requires --input FILE or --prompt TEXT."))
                    (when (and input id-option)
                      (error 'configuration-error :message
                             "run-job --id requires --prompt."))
                    (unless (non-empty-string-p output)
                      (error 'configuration-error :message
                             "run-job requires --output FILE."))
                    (flet ((status-line (message)
                             (handler-case
                                 (progn
                                   (world-terminal--write-styled-line
                                    *error-output* (list (terminal-span ':dim message))
                                    (world-terminal--status-output-styled-p *error-output*))
                                   (force-output *error-output*))
                               (stream-error ()
                                 nil))))
                      (status-line "Started")
                      (let* ((configuration
                               (apply #'configuration-create :immutable-p
                                      (not (null (getopt* command ':immutable)))
                                      :defer-provider-validation-p t
                                      (append (main--site-root-arguments command)
                                              (main--model-arguments command))))
                             (permission-mode
                               (or (getopt* command ':permissions)
                                   (config :permission-mode configuration) ':auto))
                             (executor
                               (lambda (configuration request mode)
                                 (run-job-execute-with-application
                                  configuration request mode :status-function #'status-line)))
                             (status
                               (if prompt-p
                                   (apply #'run-job-run-prompt
                                          (if (equal prompt-option "-")
                                              *standard-input*
                                              prompt-option)
                                          output permission-mode :configuration configuration
                                          :executor                             executor
                                          (when id-option (list :identifier id-option)))
                                   (run-job-run input output permission-mode
                                                :configuration configuration :executor executor))))
                        (unless (zerop status) (uiop/image:quit status))))))))

(-> main--known-model-identifiers () list)
(defun main--known-model-identifiers ()
  "Return all known model identifiers in deterministic display order."
  (sort (remove-duplicates (provider-model-selections)
                           :test #'string=)
        #'string<))

(-> main--models-command () clingon:command)
(defun main--models-command ()
  "Return the non-interactive model catalog sub-command definition."
  (make-command
   :name        "models"
   :description "list known model identifiers and exit"
   :handler
   (lambda (command)
     (let* ((pristine-p (not (null (getopt* command ':pristine))))
            (configuration
              (apply #'configuration-create
                     :immutable-p                 t
                     :defer-provider-validation-p t
                     (main--site-root-arguments command)))
            (replay-failures
              (image-state-load configuration :pristine-p pristine-p)))
       (dolist (failure replay-failures)
         (format *error-output* "~A: private image replay failed: ~A: ~A~%"
                 (product-name) (first failure) (rest failure)))
       (unless pristine-p
         (user-init-load configuration))
       (provider-bootstrap-configuration configuration)
       (dolist (model (main--known-model-identifiers))
         (format t "~A~%" model))
       (force-output)))))

(-> main--update-command () clingon:command)
(defun main--update-command ()
  "Describe the launcher-owned update operation without starting a session."
  (make-command
   :name        "update"
   :description "install the latest packaged release and exit"
   :long-description
   "Source checkouts update through Git and ./script/bootstrap; Nix installations
update through their flake or Nix profile."
   :handler
   (lambda (command)
     (declare (ignore command))
     (error 'configuration-error
            :message (format nil "Run update through the installed ~A launcher."
                             (product-command-name))))))

(-> main--uninstall-command () clingon:command)
(defun main--uninstall-command ()
  "Describe the launcher-owned uninstall operation without starting a session."
  (make-command
   :name        "uninstall"
   :description "remove the installation and keep user data"
   :long-description
   "Removes the packaged installation, managed runtimes, built images, and
caches. Conversations, memories, agendas, image commits, settings, credentials,
and configuration stay. The stable launcher performs the removal before any
Lisp starts; on Windows it prints the folders to delete."
   :options     (list (make-option ':flag
                                   :long-name   "yes"
                                   :key         ':yes
                                   :description "skip the confirmation prompt"))
   :handler
   (lambda (command)
     (declare (ignore command))
     (error 'configuration-error
            :message (format nil "Run uninstall through the installed ~A launcher."
                             (product-command-name))))))

(-> main--normalize-update-arguments (list) list)
(defun main--normalize-update-arguments (arguments)
  "Validate standalone update syntax."
  (if (equal (first arguments) "update")
      (let ((tail (rest arguments)))
        (when (equal (last tail) '("--"))
          (setf tail (butlast tail)))
        (unless (or (null tail) (equal tail '("--help")) (equal tail '("-h")))
          (error 'configuration-error
                 :message (format nil "Usage: ~A update [--help]" (product-command-name))))
        (cons "update" (when tail '("--help"))))
      arguments))

(-> main--acp-endpoint (clingon:command) (or null acp-endpoint))
(defun main--acp-endpoint (command)
  "Return the remote endpoint the acp COMMAND selects, or NIL for standard I/O."
  (let ((http-p      (getopt* command ':http))
        (websocket-p (getopt* command ':websocket))
        (token       (getopt* command ':token))
        (arguments   (command-arguments command)))
    (cond
      ((and http-p websocket-p)
       (error 'configuration-error :message "Choose --http or --websocket, not both."))
      ((or http-p websocket-p)
       (when (rest arguments)
         (error 'configuration-error :message "ACP takes at most one [HOST:]PORT."))
       (acp-endpoint-parse (if http-p ':http ':websocket) (first arguments) :token token))
      (arguments
       (error 'configuration-error :message "A [HOST:]PORT needs --http or --websocket."))
      (token
       (error 'configuration-error :message "--token needs --http or --websocket."))
      (t
       nil))))

(-> main--acp-command () clingon:command)
(defun main--acp-command ()
  "Return the Agent Client Protocol server sub-command definition."
  (make-command
   :name        "acp"
   :description "serve the Agent Client Protocol on standard I/O, HTTP, or WebSocket"
   :usage       "[--http | --websocket [HOST:]PORT [--token TOKEN]]"
   :long-description
   "Without options, ACP runs on standard input and output for the editor that
started it. --http serves remote clients over Streamable HTTP and WebSocket on
one endpoint, /acp; --websocket serves WebSocket alone. Both listen on
127.0.0.1:8737 unless HOST:PORT or PORT says otherwise. With --token, clients
must send the token as an Authorization: Bearer header or a token query
parameter. Anything but loopback is reachable from other machines and is not
secure: traffic is unencrypted, so put a TLS proxy in front."
   :options
   (list (make-option ':flag
                      :long-name   "http"
                      :key         ':http
                      :description "serve Streamable HTTP and WebSocket clients")
         (make-option ':flag
                      :long-name   "websocket"
                      :key         ':websocket
                      :description "serve WebSocket clients")
         (make-option ':string
                      :long-name   "token"
                      :key         ':token
                      :parameter   "TOKEN"
                      :description "the bearer token remote clients must present"))
   :handler
   (lambda (command)
     (when (getopt* command ':pristine)
       (error 'configuration-error
              :message (format nil "ACP opens channels in the world server. The world server does not run pristine. Start a pristine rescue session with ~A resume --pristine."
                               (product-command-name))))
     (let ((endpoint        (main--acp-endpoint command))
           (configuration   (configuration-create :defer-provider-validation-p t))
           (permission-mode (or (getopt* command ':permissions) ':ask)))
       (acp-run configuration
                :permission-mode permission-mode
                :endpoint        endpoint)))))

(-> main--top-level-command () clingon:command)
(defun main--top-level-command ()
  "Return Autolith's top-level command-line definition."
  (make-command
   :name         (product-command-name)
   :description  "a small, live, self-modifying Common Lisp agent"
   :long-description
   (format nil "The stable launcher also accepts --recovery and --from-source.
These options select how ~A starts before this command line is parsed.
Use -- to end option parsing. ~A update installs the latest packaged
release and exits without starting a session.
~A uninstall removes the installation and keeps user data."
           (product-name) (product-command-name) (product-command-name))
   :version      *autolith-version*
   :options      (main--session-options)
   :sub-commands (list (main--open-command)
                       (main--new-command)
                       (main--acp-command)
                       (main--auth-command)
                       (main--models-command)
                       (main--update-command)
                       (main--uninstall-command)
                       (main--data-command)
                       (main--usage-command)
                       (main--run-job-command)
                       (world-terminal--status-command)
                       (world-client--tell-command)
                       (world-terminal--operation-command
                        "detach" "detach a hosted channel from its terminal" ':detach)
                       (world-terminal--operation-command
                        "pause" "pause a hosted channel" ':pause)
                       (world-client--start-command)
                       (world-client--stop-command))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'configuration-error
              :message (format nil "Unknown command ~S."
                               (first (command-arguments command)))))
     (main--start-session command))))

(-> main-dispatch (list) null)
(defun main-dispatch (arguments)
  "Dispatch validated Autolith ARGUMENTS inside the active process."
  (setf arguments (main--normalize-update-arguments arguments))
  (cond
    ((and (= (length arguments) 3)
          (string= (first arguments)
                   *image-commit-replay-probe-argument*))
     (image-commit-replay-probe-main (second arguments)
                                     (third arguments)))
    ((equal arguments '("--model-catalog-worker"))
     (model-catalog--worker-main))
    ((equal arguments '("--telemetry-worker"))
     (telemetry--worker-main))
    ((equal arguments '("--worker"))
     (worker-main))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--publish-restart"))
     (generation-restart-publish (configuration-create :defer-provider-validation-p t)
                                 (uiop:parse-native-namestring (second arguments))))
    (t
     (let ((command
             (handler-case
                 (parse-command-line (main--top-level-command) arguments)
               (exit-error (condition)
                 ;; Help and version output was already printed.
                 (uiop:quit (exit-error-code condition)))
               (error (condition)
                 (format *error-output* "~&~A~%" condition)
                 (uiop:quit 64)))))
       (funcall (command-handler command) command))))
  nil)

(-> main--conversation-owner-record (conversation-in-use) (option pathname))
(defun main--conversation-owner-record (condition)
  "Locate this image selection's channel record from a structurally valid lease path."
  (let* ((lease      (conversation-in-use-lease-pathname condition))
         (identifier (conversation-in-use-identifier condition))
         (directory  (pathname-directory lease)))
    (when (and (eq (first directory) ':absolute)
               (equal (first (last directory)) "conversation-leases")
               (equal (pathname-name lease) identifier)
               (equal (pathname-type lease) "lock"))
      (image-daemon:daemon-registry-pathname
       (merge-pathnames
        "channels/"
        (world--endpoint-directory
         (make-pathname :directory (butlast directory) :name nil :type nil :defaults lease)))
       identifier))))

(-> main--startup-error-text (condition) string)
(defun main--startup-error-text (condition)
  "Enrich busy errors only at startup; failed diagnostics preserve the original error."
  (let ((original (princ-to-string condition)))
    (handler-case
        (if (typep condition 'conversation-in-use)
            (let ((pid (conversation-in-use-owner-pid condition)))
              (cond
                ((null pid)
                 (format nil "~A The lock owner could not be identified. Recheck before retrying."
                         original))
                ((= pid (current-process-id))
                 (format nil "~A This process holds the conversation lease." original))
                (t
                 (let* ((record       (main--conversation-owner-record condition))
                        (reachability (if record
                                          (world-terminal-owner-reachability
                                           record (conversation-in-use-identifier condition) pid)
                                          ':unknown)))
                   (format nil "~A Observed lock owner PID ~D. ~A" original pid
                           (case reachability
                             (:reachable
                              (format nil "A matching channel endpoint is reachable in this image selection. Inspect it with ~A status or attach with ~:*~A open ID."
                                      (product-command-name)))
                             (:no-reachable-endpoint
                              (format nil "No matching channel endpoint was found in this image selection. Inspect and recheck PID ~D; if you choose to stop it, run kill ~D, then retry. The owner may have changed since this snapshot." pid pid))
                             (otherwise
                              "Channel endpoint reachability is unknown. Inspect and recheck the observed PID before retrying; the owner may have changed since this snapshot.")))))))
            original)
      ((or sb-sys:deadline-timeout error) ()
        original))))

(-> main--restore-caller-asdf-environment () null)
(defun main--restore-caller-asdf-environment ()
  "Restore one captured caller environment after loading our system and dependencies.

The initialized ASDF configuration belongs to this image. Only the process
variables for later user and child consumers are restored here."
  (dolist (name '("CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"))
    (let* ((value-name (concatenate 'string "AUTOLITH_CALLER_" name))
           (state-name (concatenate 'string value-name "_STATE"))
           (state      (uiop:getenv state-name)))
      (when (member state '("set" "absent") :test #'equal)
        (if (equal state "set")
            (platform-setenv name (or (uiop:getenv value-name) ""))
            (platform-unsetenv name))
        (platform-unsetenv value-name)
        (platform-unsetenv state-name))))
  nil)

(-> main--startup-failure-line (string stream) string)
(defun main--startup-failure-line (message stream)
  "Return the line saying Autolith could not start because of MESSAGE.

The line is colored as a failure when STREAM is a terminal that allows color."
  (let ((line     (format nil "! ~A could not start: ~A" (product-name) message))
        (sequence (and (main--output-styled-p stream)
                       (terminal-style-sequence ':failure))))
    (if sequence
        (concatenate 'string sequence line *terminal-style-reset*)
        line)))

(-> main (list) null)
(defun main (arguments)
  "Run the Autolith command described by ARGUMENTS with stable exit classification."
  (main--restore-caller-asdf-environment)
  (shell-log-accounting-reset)
  (handler-case
      (main-dispatch arguments)
    (fatal-control-path-error (condition)
      (multiple-value-bind (status message) (main--launcher-exit condition)
        (format *error-output* "~&~A~%" message)
        (uiop:quit status)))
    ((or autolith-error cl-llm-provider-api:provider-api-error) (condition)
      (format *error-output* "~A~%"
              (main--startup-failure-line (main--startup-error-text condition)
                                          *error-output*))
      (uiop:quit 64))
    ;; Ctrl-C outside the raw-mode terminal interface ends the command, after
    ;; unwinding its cleanup, with the shell's interrupt status.
    (sb-sys:interactive-interrupt ()
      (uiop:quit 130)))
  nil)
