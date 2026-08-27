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

(defparameter *application-recovery-gradient-styles*
  '(:recovery-gradient-1 :recovery-gradient-2 :recovery-gradient-3
    :recovery-gradient-4 :recovery-gradient-5 :recovery-gradient-6)
  "The distinct row styles used after recovery starts Autolith.")

(defparameter *application-banner-gap* "   "
  "Horizontal space between the startup mark and session data.")

(defparameter *application-banner-minimum-metadata-width* 32
  "The minimum useful width for metadata beside the startup mark.")

(-> application--banner-logo-lines () list)
(defun application--banner-logo-lines ()
  "Return startup-mark rows colored for an ordinary or recovered process."
  (if (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))
      (mapcar (lambda (entry style)
                (cons style (rest entry)))
              *application-banner-logo-lines*
              *application-recovery-gradient-styles*)
      *application-banner-logo-lines*))

(-> application--banner-logo-width () (integer 1))
(defun application--banner-logo-width ()
  "Return the widest row of the embedded startup mark in terminal cells."
  (loop for entry in (application--banner-logo-lines)
        maximize (text-cell-width (rest entry))))

(-> application--banner-columns (application) (integer 1))
(defun application--banner-columns (application)
  "Return APPLICATION's current terminal width or the restrained default."
  (let ((ui (and (slot-boundp application 'ui)
                 (application-ui application))))
    (if ui
        (terminal-columns (terminal-ui-terminal ui))
        *terminal-default-columns*)))

(-> application--banner-metadata-field (string string) list)
(defun application--banner-metadata-field (label value)
  "Return one aligned metadata LABEL and VALUE row without a newline."
  (list (terminal-span :dim (format nil "~12A  " label))
        (terminal-span :plain value)))

(-> application--banner-metadata-rows (application (integer 1)) list)
(defun application--banner-metadata-rows (application maximum-width)
  "Return identity and runtime rows no wider than MAXIMUM-WIDTH."
  (let* ((configuration (application-configuration application))
         (title
           (list (terminal-span :strong "AUTOLITH")
                 (terminal-span :dim (format nil " v~A" *autolith-version*))))
         (model
           (application--banner-metadata-field
            "model"
            (format nil "~A (effort ~A)"
                    (configuration-model configuration)
                    (configuration-reasoning-effort configuration))))
         (workspace
           (application--banner-metadata-field
            "workspace"
            (namestring (configuration-working-directory configuration))))
         (mode
           (and (configuration-immutable-p configuration)
                (application--banner-metadata-field "mode" "immutable")))
         (detail-rows (append (list title model workspace)
                              (when mode (list mode))))
         (divider-width
           (min maximum-width
                (loop for row in detail-rows
                      maximize (terminal--spans-width row)))))
    (append
     (list title
           (list (terminal-span
                  :dim
                  (make-string divider-width :initial-element #\─)))
           model
           workspace)
     (when mode (list mode)))))

(-> application--banner-terminate-row (list) list)
(defun application--banner-terminate-row (spans)
  "Return SPANS followed by one plain newline span."
  (append spans (list (terminal-span :plain (string #\Newline)))))

(-> application--banner-side-by-side-spans (list integer) list)
(defun application--banner-side-by-side-spans (metadata-rows columns)
  "Return the startup mark with METADATA-ROWS aligned beside it within COLUMNS."
  (let* ((logo-width (application--banner-logo-width))
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
                 (terminal--clip-spans metadata-row metadata-width)))))))))

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
                 (terminal--clip-spans metadata-row columns)))))

(-> application--startup-command-entry () application-command)
(defun application--startup-command-entry ()
  "Return one command entry selected for the startup banner."
  (let ((commands (application-command-list)))
    (nth (random (length commands) (make-random-state t))
         commands)))

(-> application--command-tip-spans
    (application-command)
    terminal-styled-text)
(defun application--command-tip-spans (entry)
  "Return a startup tip with ENTRY's canonical Lisp call styled as code."
  (list (terminal-span :plain (format nil "~2%"))
        (terminal-span :dim "Tip: ")
        (terminal-span
         :code
         (format nil "(~A)" (application-operation--command-name entry)))
        (terminal-span :plain
                       (format nil " ~A" (application-command-tip entry)))))

(-> application-banner (application) list)
(defun application-banner (application)
  "Return APPLICATION's identity, session metadata, security notice, and tip."
  (let* ((columns (application--banner-columns application))
         (metadata-width
           (- columns
              (application--banner-logo-width)
              (text-cell-width *application-banner-gap*)))
         (side-by-side-minimum
           (+ (application--banner-logo-width)
              (text-cell-width *application-banner-gap*)
              *application-banner-minimum-metadata-width*))
         (header
           (if (>= columns side-by-side-minimum)
               (application--banner-side-by-side-spans
                (application--banner-metadata-rows application metadata-width)
                columns)
               (application--banner-stacked-spans
                (application--banner-metadata-rows application columns)
                columns))))
    (append
     (list (terminal-span :plain (string #\Newline)))
     header
     (list
      (terminal-span
       :notice
       (format nil "~%Autolith executes model-generated code with your user ~
                    privileges.~%Sandboxing is no substitute for human oversight")))
     (application--command-tip-spans
      (application--startup-command-entry)))))

(-> application--update-notice (application) (option list))
(defun application--update-notice (application)
  "Return the cached update notice appropriate to APPLICATION's installation."
  (let ((availability (application-update-availability application)))
    (when availability
      (let ((current-version *autolith-version*)
            (latest-version (subseq (update-availability-tag availability) 1)))
        (list
         (terminal-span
          ':notice
          (format nil "Update available: Autolith ~A -> ~A.~%"
                  current-version latest-version))
         (terminal-span
          ':dim
          (if (eq (update-availability-method availability) ':nix)
              "Installed through Nix. Update the flake or profile that provides Autolith."
              "Choose whether to install it before continuing.")))))))

(-> application--update-choice-items () list)
(defun application--update-choice-items ()
  "Return the explicit user choices for a packaged release update."
  '((:name "Not now" :argument nil
     :description "continue with the installed release")
    (:name "Update now" :argument nil
     :description "install the verified release and restart")
    (:name "Skip this version" :argument nil
     :description "hide this exact release until a newer one appears")))

(-> application--offer-startup-update (application) null)
(defun application--offer-startup-update (application)
  "Offer an attended update for APPLICATION's validated packaged release."
  (let ((availability (application-update-availability application)))
    (when (and availability
               (eq (update-availability-method availability) ':release))
      (let* ((tag (update-availability-tag availability))
             (choice
               (terminal-ui-select
                (application-ui application)
                :title (format nil "Autolith ~A is available" (subseq tag 1))
                :items (application--update-choice-items)
                :resize-callback #'application-pending-terminal-size)))
        (cond
          ((string= (or choice "") "Update now")
           (error 'update-requested
                  :message (format nil "Update to Autolith ~A." (subseq tag 1))
                  :tag tag))
          ((string= (or choice "") "Skip this version")
           (update-state-dismiss (application-configuration application) tag)
           (setf (application-update-availability application) nil)
           (application-present
            application
            (list (terminal-span ':dim
                                 (format nil "Skipped Autolith ~A." (subseq tag 1))))))))))
  nil)

(-> application--expected-error-entry
    (application autolith-error)
    list)
(defun application--expected-error-entry (application condition)
  "Return the transcript entry describing expected CONDITION."
  (application--transcript-entry
   application
   :style ':failure
   :header "✗ error"
   :body (if (typep condition 'credentials-unavailable)
             (format nil "~A~%Use /auth to authenticate Autolith directly."
                     condition)
             (format nil "~A" condition))))

(-> application-handle-expected-error (application autolith-error) null)
(defun application-handle-expected-error (application condition)
  "Present expected CONDITION without abandoning APPLICATION's active path."
  (application-set-activity application nil)
  (application-render-records application)
  (application-present application
                       (application--expected-error-entry application condition))
  nil)

(-> application-read-terminal-event (terminal-ui) t)
(defun application-read-terminal-event (ui)
  "Read one UI event, applying pending resizes before and after the blocking read."
  (terminal-ui-refresh-size ui #'application-pending-terminal-size)
  (prog1 (terminal-ui-read-event ui)
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)))

(-> application--present-resume-instruction (application) boolean)
(defun application--present-resume-instruction (application)
  "Present APPLICATION's exact resume command when its conversation is durable."
  (let ((conversation (application-conversation application)))
    (if (conversation-persisted-p conversation)
        (not
         (null
          (application-present
           application
           (list
            (terminal-span :dim "To resume this conversation, run:")
            (terminal-span :plain (string #\Newline))
            (terminal-span :code
                           (format nil "  ~A"
                                   (application--resume-command application)))))))
        nil)))

(-> application--initial-work-items
    ((option string) (option string) boolean)
    list)
(defun application--initial-work-items
    (initial-command recovery-diagnosis resume-offer-p)
  "Return ordered controller work for command-line and recovery startup behavior."
  (cond
    (initial-command
     (list (list (if (terminal-ui--lisp-draft-p initial-command)
                     ':lisp
                     ':command)
                 initial-command)))
    (recovery-diagnosis
     (list (list ':recovery-diagnosis recovery-diagnosis)))
    (resume-offer-p
     (list (list ':project-adaptation-offer)))
    (t
     nil)))

(-> application-run
    (application &key (:initial-command (option string))
                      (:initial-input (option user-message-input))
                      (:recovery-diagnosis (option string))
                      (:resume-offer-p boolean))
    null)
(defun application-run
    (application &key initial-command initial-input recovery-diagnosis
                      resume-offer-p)
  "Run APPLICATION with responsive input, always restoring terminal and workers."
  (let* ((ui (application-ui application))
         (worker (application-worker application))
         (input-controller nil)
         (tool-runtimes-closed-p nil)
         (worker-stopped-p nil)
         (recovery-startup-p (application-recovery-startup-p application))
         (recovery-input-storage-ready-p
           (or (not recovery-startup-p)
               (application-recovery-input-vault-import application))))
    (labels ((close-runtime-resources ()
               "Close APPLICATION's external runtimes at most once."
               (ignore-errors (localgroup-stop application))
               (ignore-errors (management-repl-stop application))
               (unless tool-runtimes-closed-p
                 (unwind-protect
                      (ignore-errors
                        (tool-registry-close-runtime-state
                         (application-tool-registry application)))
                   (ignore-errors
                     (application-disconnect-task-presentation application))
                   (setf tool-runtimes-closed-p t)))
               (unless worker-stopped-p
                 (unwind-protect
                      (when worker
                        (lisp-worker-manager-stop worker))
                   (setf worker-stopped-p t)))
               (conversation-picker-search-close
                (application-conversation application))
               (application-release-conversation-lease application)
               nil)

             (finish-shutdown ()
               "Close runtimes while preserving CONTROLLER's Ctrl-C escape."
               (if input-controller
                   (progn
                     (application-input-controller-call-with-shutdown-escape
                      input-controller #'close-runtime-resources)
                     (setf input-controller nil))
                   (close-runtime-resources))))
      (sb-sys:enable-interrupt
       sb-unix:sigwinch
       (lambda (signal code context)
         (declare (ignore signal code context))
         (setf *terminal-resize-pending-p* t)))
      (unwind-protect
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (unwind-protect
                  (progn
                    (application-present application
                                         (application-banner application))
                    (let ((update-notice
                            (application--update-notice application)))
                      (when update-notice
                        (application-present application update-notice)))
                    (when (and (null initial-command)
                               (null initial-input)
                               (null recovery-diagnosis))
                      (application--offer-startup-update application))
                    (let ((provenance
                            (application-installation-provenance application)))
                      (when provenance
                        (setf (application-update-check-thread application)
                              (update-check-start
                               (application-configuration application)
                               provenance))))
                    (dolist (failure
                             (application-mutation-replay-failures application))
                      (application-present
                       application
                       (application--transcript-entry
                        application
                        :style ':failure
                        :header "✗ mutation replay skipped"
                        :body (format nil "~A~%~A"
                                      (namestring (first failure))
                                      (rest failure)))))
                    (application-render-records application)
                    (when initial-input
                      (terminal-ui-set-input ui initial-input))
                    (setf (application-project-adaptation-offer-p application)
                          (and resume-offer-p (not (null initial-command))))
                    (setf input-controller
                          (application-input-controller-create
                           application
                           :initial-work-items
                           (application--initial-work-items
                            initial-command
                            recovery-diagnosis
                            resume-offer-p)
                           :load-pending-p (not recovery-startup-p)
                           :pending-persistence-enabled-p
                           recovery-input-storage-ready-p
                           :start-reader-p nil))
                    (application-recovery-input-vault-present-startup-warning
                     application)
                    (localgroup-start application)
                    (management-repl-start application)
                    (application-input-controller--open-prompt-if-ready
                     input-controller)
                    (application-input-controller--start-reader
                     input-controller)
                    ;; Entering the interactive debugger would hang the raw
                    ;; terminal, so debugger entry becomes fatal recovery.
                    (let ((*checkpoint-thread-quiescer*
                            (lambda (function)
                              (application-input-controller-call-with-reader-paused
                               input-controller
                               (lambda ()
                                 (application--quiesce-update-check application)
                                 (application-call-with-localgroup-quiesced
                                  application
                                  (lambda ()
                                    (application-call-with-management-repl-quiesced
                                     application function)))))))
                          (*debugger-hook*
                            (lambda (condition hook)
                              (declare (ignore hook))
                              (application-raise-fatal
                               application
                               condition
                               (application-safe-backtrace)))))
                      (handler-case
                          (loop
                            for work =
                              (application-input-controller--next-work
                               input-controller)
                            while work
                            do (handler-case
                                   (unwind-protect
                                        (progn
                                          (application-input-controller--run-work
                                           input-controller work)
                                          (when
                                              (application-input-controller--consume-turn-cancellation-delivery-p
                                               input-controller)
                                            (error
                                             (make-condition
                                              'application-turn-cancelled))))
                                     (application-input-controller--finish-work
                                      input-controller))
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
                    (when (eq (application-input-controller-exit-reason
                               input-controller)
                              ':interrupt)
                      (application--present-resume-instruction application)))
               (finish-shutdown)))
        (sb-sys:enable-interrupt sb-unix:sigwinch :default)
        (finish-shutdown))))
  nil)

;;;; -- Command-Line Entry --

(defparameter *main-fatal-recovery-status* 70
  "The process status asking the stable launcher to recover after a fatal error.")

(defparameter *main-rollback-recovery-status* 75
  "The process status asking the stable launcher to start a selected rollback.")

(defparameter *main-update-request-status* 76
  "The process status asking a packaged outer launcher to perform an update.")

(-> main--authentication-provider (configuration (option string)) model-provider)
(defun main--authentication-provider (configuration selection)
  "Return a provider for the active or explicitly selected registration."
  (if selection
      (provider-authentication-provider configuration selection)
      (provider-create configuration)))

(-> main--authentication-output-styled-p (stream) boolean)
(defun main--authentication-output-styled-p (stream)
  "Return true when command-line authentication may style output to STREAM."
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
                 (let ((leaf (first (last (pathname-directory subdirectory)))))
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
    (configuration (option string) &optional (option string))
    null)
(defun main-authenticate (configuration selection &optional method)
  "Authenticate a registered provider before the conversation UI starts."
  (configuration-ensure-directories configuration)
  (let ((provider (main--authentication-provider configuration selection))
        (*api-key-input-file-descriptor* 0)
        (*api-key-output-styled-p*
          (main--authentication-output-styled-p *standard-output*)))
    (format t "~&~A~%"
            (provider-authenticate-with-method
             provider method
             :stream *standard-output*
             :open-browser-p t)))
  nil)

(-> main--image-pathnames (list) list)
(defun main--image-pathnames (image-values)
  "Return validated pathnames from repeated comma-separable IMAGE-VALUES."
  (let ((values
          (loop for value in image-values
                append (uiop:split-string value :separator '(#\,)))))
    (when (some (lambda (value)
                  (not (non-empty-string-p value)))
                values)
      (error 'configuration-error
             :message "Every --image value must name a local image."))
    (mapcar #'image-input-validate-pathname values)))

(-> main--initial-image-input (list) (option user-message-input))
(defun main--initial-image-input (image-values)
  "Return a labelled initial composer draft for IMAGE-VALUES' local images."
  (let ((pathnames (main--image-pathnames image-values)))
    (when pathnames
      (user-message-input-create
       :text (format nil "~{~A~^ ~}"
                     (loop for number from 1 to (length pathnames)
                           collect (terminal-ui--image-label number)))
       :image-pathnames pathnames))))

(-> main--connect-application
    (&key (:configuration configuration)
          (:conversation-id (option string))
          (:immutable-p boolean)
            (:permission-mode (member :ask :auto :sandboxed :full-access))
          (:fresh-conversation-p boolean))
    application)
(defun main--connect-application
    (&key configuration conversation-id immutable-p permission-mode
          fresh-conversation-p)
  "Create or reconnect the active application for one normal or handoff startup."
  (if (and (not fresh-conversation-p)
           (typep *active-application* 'application))
      (application-reconnect *active-application*
                             :conversation-id conversation-id
                             :immutable-p immutable-p
                             :permission-mode permission-mode)
      (application-create configuration
                          :conversation-id conversation-id
                          :permission-mode permission-mode)))

(-> main--start-session
    (clingon:command
     &key (:resume-requested-p boolean)
          (:resume-id (option string))
          (:authenticate-p boolean)
          (:authentication-selection (option string))
          (:authentication-method (option string)))
    null)
(defun main--start-session
    (command &key resume-requested-p resume-id authenticate-p
                  authentication-selection authentication-method)
  "Start one interactive Autolith session from COMMAND's parsed options."
  (main--register-local-source-trees)
  (let* ((immutable-p (not (null (getopt* command ':immutable))))
         (site-config-root-value (getopt* command ':site-config-root))
         (explicit-permission-mode (getopt* command ':permissions))
         (image-values (getopt* command ':images))
         (configuration
           (configuration-create
            :immutable-p immutable-p
            :site-config-root
            (and (non-empty-string-p site-config-root-value)
                 (pathname site-config-root-value))
            :defer-provider-validation-p t))
         (permission-mode
           (or explicit-permission-mode
               (preferences-permission-mode configuration)
               ':ask))
         (handoff-record
           (localgroup-handoff-selection
            configuration
            (getopt* command ':localgroup-handoff)))
         (fresh-handoff-p
           (and handoff-record
                (getf (rest handoff-record) :fresh-conversation-p)))
         (effective-resume-requested-p
           (and (null handoff-record) resume-requested-p))
         (effective-resume-id
           (if handoff-record
               (getf (rest handoff-record) :conversation-id)
               resume-id))
          (recovery-state
            (if handoff-record
                (list nil nil)
                (multiple-value-list
                 (application-recovery-state configuration))))
          (recovery-conversation-id (first recovery-state))
          (recovery-diagnosis
            (if handoff-record
                (getf (rest handoff-record) :recovery-diagnosis)
                (and (null effective-resume-id)
                     (application-recovery-diagnosis-prompt configuration))))
         (resume-command-p
           (or (and handoff-record
                    (getf (rest handoff-record) :resume-command-p))
               (and effective-resume-requested-p
                    (or (not (null effective-resume-id))
                        (and (null recovery-conversation-id)
                             (null recovery-diagnosis)))))))
    (when authenticate-p
      (user-init-load configuration)
      (main-authenticate (preferences-apply-model-selection
                          (provider-bootstrap-configuration configuration))
                         authentication-selection
                         authentication-method))
    (when handoff-record
      (localgroup-handoff-begin-startup handoff-record)
      (application--clear-recovery-environment))
    (when (and (string-equal (software-type) "Linux")
               (not (application--command-sandbox-available-p)))
      (format *error-output* "~&Autolith: ~A~%"
              (application--command-sandbox-unavailable-message))
      (force-output *error-output*))
    (when (main--client-session-p
           :handoff-record handoff-record
           :authenticate-p authenticate-p
           :resume-requested-p effective-resume-requested-p
           :resume-id effective-resume-id
           :recovery-conversation-id recovery-conversation-id
           :recovery-diagnosis recovery-diagnosis
           :image-values image-values
           :simulate-crash-p (not (null (getopt* command ':simulate-crash))))
      (let ((session-id (main--spawn-client-session
                         configuration
                         :permission-mode permission-mode
                         :immutable-p immutable-p
                          :conversation-id
                          (or effective-resume-id recovery-conversation-id)
                          :resume-command-p
                          (and effective-resume-requested-p
                               (null effective-resume-id))
                          :recovery-diagnosis recovery-diagnosis)))
        (when session-id
          (let ((record (localgroup--find-record configuration session-id)))
            (handler-case
                (localgroup-attach-record configuration record ':control)
              (serious-condition (condition)
                ;; The freshly spawned session has no other owner yet, so
                ;; a failed first attach would leak it as an idle
                ;; detached process.
                (ignore-errors (localgroup-query-record record ':kill))
                (error condition))))
          (return-from main--start-session nil))))
    (let ((*localgroup-startup-record* handoff-record))
      (setf *active-application*
            (main--connect-application
             :configuration configuration
             :conversation-id effective-resume-id
             :immutable-p immutable-p
             :permission-mode permission-mode
             :fresh-conversation-p (not (null fresh-handoff-p))))
      (application--clear-recovery-environment)
      (when (and (getopt* command ':simulate-crash)
                 (not (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))))
        (let ((capsule
                (application-write-crash-capsule
                 *active-application*
                 (make-condition 'simple-error
                                 :format-control "Intentional recovery test."
                                 :format-arguments nil))))
          (format *error-output* "Intentional crash capsule: ~A~%" capsule)
          (uiop:quit *main-fatal-recovery-status*)))
      (handler-case
          (application-run
           *active-application*
           :initial-command (and resume-command-p
                                 (null effective-resume-id)
                                 "(resume)")
           :initial-input
           (if handoff-record
               (localgroup-handoff-initial-input handoff-record)
               (main--initial-image-input image-values))
           :recovery-diagnosis recovery-diagnosis
           :resume-offer-p resume-command-p)
        (rollback-requested (condition)
          (format *error-output*
                  "Autolith is rolling back to retained generation ~A.~%"
                  (rollback-requested-generation-id condition))
          (uiop:quit *main-rollback-recovery-status*))
        (update-requested (condition)
          (format *error-output*
                  "Autolith will update to ~A after restoring the terminal.~%"
                  (subseq (update-requested-tag condition) 1))
          (uiop:quit *main-update-request-status*))
        (fatal-control-path-error (condition)
          (format *error-output*
                  "Autolith entered recovery after a fatal error. Capsule: ~A~%"
                  (fatal-control-path-error-capsule-pathname condition))
          (uiop:quit *main-fatal-recovery-status*)))))
  nil)

(-> main--client-session-p
    (&key (:handoff-record (option list))
          (:authenticate-p boolean)
          (:resume-requested-p boolean)
          (:resume-id (option string))
          (:recovery-conversation-id (option string))
          (:recovery-diagnosis t)
          (:image-values t)
          (:simulate-crash-p boolean))
    boolean)
(defun main--client-session-p
    (&key handoff-record authenticate-p resume-requested-p resume-id
          recovery-conversation-id recovery-diagnosis image-values
          simulate-crash-p)
  "Return true when this start should spawn a detached session and attach.

A client-first start keeps this terminal a thin relay whose detach is
immediate and never interrupts session work, so resumes and crash recovery
take it too. Replacements, authentication, startup images, crash simulation,
non-interactive terminals, and AUTOLITH_SESSION_STYLE=direct keep the direct
path."
  (declare (ignore resume-requested-p resume-id
                   recovery-conversation-id recovery-diagnosis))
  (and (null handoff-record)
       (not authenticate-p)
       (null image-values)
       (not simulate-crash-p)
       (not (string= (or (uiop:getenv "AUTOLITH_SESSION_STYLE") "")
                     "direct"))
       (not (null (interactive-stream-p *standard-input*)))))

(-> main--spawn-client-session
    (configuration &key (:permission-mode keyword) (:immutable-p boolean)
                        (:conversation-id (option string))
                        (:resume-command-p boolean)
                        (:recovery-diagnosis t))
    (option string))
(defun main--spawn-client-session
    (configuration &key (permission-mode ':ask) immutable-p conversation-id
                        resume-command-p recovery-diagnosis)
  "Spawn the detached session for a client-first start, or NIL to run direct."
  (handler-case
      (localgroup-handoff-spawn-fresh configuration
                                      :permission-mode permission-mode
                                      :immutable-p immutable-p
                                      :conversation-id conversation-id
                                      :resume-command-p resume-command-p
                                      :recovery-diagnosis recovery-diagnosis)
    (error (condition)
      (format *error-output*
              "Autolith is starting directly in this terminal: ~A~%"
              (bounded-string condition :limit 400))
      nil)))

(-> main--session-options () list)
(defun main--session-options ()
  "Return the persistent session options shared with every sub-command."
  (list
   (make-option ':flag
                :long-name "immutable"
                :key ':immutable
                :persistent t
                :description "disable source and configuration mutation")
   (make-option ':string
                :long-name "site-config-root"
                :key ':site-config-root
                :parameter "DIRECTORY"
                :persistent t
                :description "load site configuration before user configuration")
   (make-option ':enum
                :long-name "permissions"
                :key ':permissions
                :parameter "MODE"
                :persistent t
                :items '(("ask" . :ask)
                         ("auto" . :auto)
                         ("sandbox" . :sandboxed)
                         ("full" . :full-access))
                :description "initial command authorization mode")
   (make-option ':list
                :short-name #\i
                :long-name "image"
                :key ':images
                :parameter "FILE"
                :persistent t
                :description "preload a local image into the composer; repeatable and comma-separable")
   (make-option ':string
                :long-name "localgroup-handoff"
                :key ':localgroup-handoff
                :parameter "FILE"
                :persistent t
                :hidden t
                :description "claim an internal localgroup handoff record")
   (make-option ':flag
                :long-name "simulate-crash"
                :key ':simulate-crash
                :persistent t
                :hidden t
                :description "write a crash capsule and exit into recovery")))

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

(-> main--resume-command () clingon:command)
(defun main--resume-command ()
  "Return the resume sub-command definition."
  (make-command
   :name "resume"
   :description "resume a saved conversation, or pick one interactively"
   :usage "[ID]"
   :handler
   (lambda (command)
     (main--start-session
      command
      :resume-requested-p t
      :resume-id (main--single-selection command
                                         "conversation identifier")))))

(-> main--replay-command () clingon:command)
(defun main--replay-command ()
  "Return the read-only conversation replay sub-command definition."
  (make-command
   :name "replay"
   :description "inspect a saved conversation as a read-only event stream"
   :usage "ID [turn N | date YYYY-MM-DD | time TIME | sequence N]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless arguments
         (error 'configuration-error
                :message "Replay requires a conversation identifier."))
       (when (> (length arguments) 3)
         (error 'configuration-error
                :message "Replay accepts an ID and at most one selector."))
       (conversation-replay-run
        (configuration-create :immutable-p t
                              :defer-provider-validation-p t)
        (first arguments)
        (rest arguments))))))

(-> main--auth-command () clingon:command)
(defun main--auth-command ()
  "Return the auth sub-command definition."
  (make-command
   :name "auth"
   :description "authenticate a provider, then start a session"
   :usage "[PROVIDER] [METHOD]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (when (> (length arguments) 2)
         (error 'configuration-error
                :message "Auth accepts a provider and optional method."))
       (main--start-session
        command
        :authenticate-p t
        :authentication-selection (first arguments)
        :authentication-method (second arguments))))))

(-> main--run-job-command () clingon:command)
(defun main--run-job-command ()
  "Return the non-interactive single-job command definition."
  (make-command
   :name "run-job"
   :description "run one contracted child-agent job without a terminal"
   :options
   (list
    (make-option ':string :long-name "input" :key ':input
                 :parameter "FILE" :description "data-only job S-expression")
    (make-option ':string :long-name "output" :key ':output
                 :parameter "FILE" :description "atomically installed terminal result"))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'configuration-error
              :message "run-job accepts only --input and --output options."))
     (let ((input (getopt* command ':input))
           (output (getopt* command ':output)))
       (unless (non-empty-string-p input)
         (error 'configuration-error :message "run-job requires --input FILE."))
       (unless (non-empty-string-p output)
         (error 'configuration-error :message "run-job requires --output FILE."))
       (let* ((configuration
                (configuration-create
                 :immutable-p (not (null (getopt* command ':immutable)))
                 :defer-provider-validation-p t))
              (permission-mode
                (or (getopt* command ':permissions)
                    (preferences-permission-mode configuration)
                    ':auto))
              (status (run-job-run input output permission-mode
                                   :configuration configuration)))
         (unless (zerop status)
           (uiop:quit status)))))))


(-> main--top-level-command () clingon:command)
(defun main--top-level-command ()
  "Return Autolith's top-level command-line definition."
  (make-command
   :name "autolith"
   :description "a small, live, self-modifying Common Lisp agent"
   :long-description
   "The stable launcher also accepts --recovery and --from-source, which
select how Autolith starts before this command line is parsed."
   :version *autolith-version*
   :options (main--session-options)
   :sub-commands (list (main--resume-command)
                       (main--replay-command)
                       (main--auth-command)
                       (main--run-job-command)
                       (main-localgroup-command))
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
  (cond
    ((and (= (length arguments) 3)
          (string= (first arguments)
                   *image-commit-replay-probe-argument*))
     (image-commit-replay-probe-main (second arguments)
                                     (third arguments)))
    ((member "--worker" arguments :test #'string=)
     (worker-main))
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

(-> main (list) null)
(defun main (arguments)
  "Run the Autolith command described by ARGUMENTS with stable exit classification."
  (handler-case
      (main-dispatch arguments)
    (autolith-error (condition)
      (format *error-output* "Autolith could not start: ~A~%" condition)
      (uiop:quit 64)))
  nil)
