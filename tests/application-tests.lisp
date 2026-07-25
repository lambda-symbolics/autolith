(in-package #:autolith)

;;;; -- Presentation Test Support --

(-> application-tests--ui-application
    (&key (:columns integer) (:reasoning-traces-p boolean)
          (:compact-view-p boolean))
    application)
(defun application-tests--ui-application
    (&key (columns 40) reasoning-traces-p (compact-view-p t))
  "Return a minimal application presenting into a recording terminal."
  (make-instance 'application
                 :reasoning-traces-p reasoning-traces-p
                 :compact-view-p compact-view-p
                 :tool-registry (make-default-tool-registry)
                 :ui (terminal-ui-create
                      :terminal (make-instance 'recording-terminal
                                               :columns columns))))

(defclass cursor-observing-provider (scripted-provider)
  ((visibility-function
    :initarg :visibility-function
    :reader cursor-observing-provider-visibility-function
    :type function
    :documentation "Function reporting live-region cursor visibility.")
   (visible-during-request-p
    :initform t
    :accessor cursor-observing-provider-visible-during-request-p
    :type boolean
    :documentation "Cursor visibility observed when a provider request begins."))
  (:documentation "A scripted provider recording cursor state during a request."))

(defmethod provider-stream-turn :before
    ((provider cursor-observing-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record cursor visibility immediately before PROVIDER starts streaming."
  (declare (ignore conversation tool-namespaces event-callback
                   goal-context compaction-p))
  (setf (cursor-observing-provider-visible-during-request-p provider)
        (funcall (cursor-observing-provider-visibility-function provider))))

(defclass gated-provider (scripted-provider)
  ((gate-lock
    :initform (make-lock "Autolith gated provider")
    :reader gated-provider-lock
    :type t
    :documentation "The lock protecting deterministic provider timing.")
   (gate-condition-variable
    :initform (make-condition-variable :name "Autolith gated provider")
    :reader gated-provider-condition-variable
    :type t
    :documentation "The wait point for first-request entry and release.")
   (request-count
    :initform 0
    :accessor gated-provider-request-count
    :type (integer 0)
    :documentation "The number of provider requests that reached the gate.")
   (entered-p
    :initform nil
    :accessor gated-provider-entered-p
    :type boolean
    :documentation "Whether the first provider request reached the gate.")
   (released-p
    :initform nil
    :accessor gated-provider-released-p
    :type boolean
    :documentation "Whether the first provider request may continue."))
  (:documentation "A scripted provider whose first request waits for terminal input."))

(defmethod provider-stream-turn :around
    ((provider gated-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Hold PROVIDER's first request until its deterministic input gate opens."
  (declare (ignore conversation tool-namespaces event-callback
                   goal-context compaction-p))
  (let ((first-request-p nil))
    (with-lock-held ((gated-provider-lock provider))
      (incf (gated-provider-request-count provider))
      (setf first-request-p (= (gated-provider-request-count provider) 1))
      (when first-request-p
        (setf (gated-provider-entered-p provider) t)
        (condition-notify (gated-provider-condition-variable provider))
        (loop until (gated-provider-released-p provider)
              do (condition-wait
                  (gated-provider-condition-variable provider)
                  (gated-provider-lock provider)))))
    (call-next-method)))

(-> gated-provider-state (gated-provider) (values boolean boolean))
(defun gated-provider-state (provider)
  "Return PROVIDER's first-request entered and released state."
  (with-lock-held ((gated-provider-lock provider))
    (values (gated-provider-entered-p provider)
            (gated-provider-released-p provider))))

(-> gated-provider-release (gated-provider) null)
(defun gated-provider-release (provider)
  "Release PROVIDER's first waiting request."
  (with-lock-held ((gated-provider-lock provider))
    (setf (gated-provider-released-p provider) t)
    (condition-notify (gated-provider-condition-variable provider)))
  nil)

(defclass responsive-scripted-terminal (scripted-terminal)
  ((provider
    :initarg :provider
    :reader responsive-scripted-terminal-provider
    :type gated-provider
    :documentation "The provider whose first request paces later input.")
   (conversation
    :initarg :conversation
    :reader responsive-scripted-terminal-conversation
    :type conversation
    :documentation "The conversation whose durable answers permit final EOF.")
   (pre-provider-event-count
    :initarg :pre-provider-event-count
    :initform 2
    :reader responsive-scripted-terminal-pre-provider-event-count
    :type (integer 0)
    :documentation "The number of initial events admitted before the provider gate opens.")
   (events-read
    :initform 0
    :accessor responsive-scripted-terminal-events-read
    :type (integer 0)
    :documentation "The number of scripted events already returned.")
   (final-provider-item-count
    :initarg :final-provider-item-count
    :initform 2
    :reader responsive-scripted-terminal-final-provider-item-count
    :type (integer 0)
    :documentation "The durable provider item count required before physical EOF."))
  (:documentation
   "A terminal that types more input only while its provider request is active."))

(-> responsive-scripted-terminal--answer-count
    (responsive-scripted-terminal)
    (integer 0))
(defun responsive-scripted-terminal--answer-count (terminal)
  "Return the number of durable provider items visible to TERMINAL."
  (let ((conversation
          (responsive-scripted-terminal-conversation terminal)))
    (if (probe-file (conversation-pathname conversation))
        (count ':provider-item
               (rest (conversation--read-records
                      (conversation-pathname conversation)))
               :key #'first)
        0)))

(defmethod terminal-input-ready-p ((terminal responsive-scripted-terminal))
  "Pace TERMINAL input around its first active request and durable answers."
  (let* ((events (scripted-terminal-events terminal))
         (remaining (length events))
         (provider (responsive-scripted-terminal-provider terminal)))
    (multiple-value-bind (entered-p released-p)
        (gated-provider-state provider)
      (cond
        ((< (responsive-scripted-terminal-events-read terminal)
            (responsive-scripted-terminal-pre-provider-event-count terminal))
         t)
        ((not entered-p)
         nil)
        ((plusp remaining)
         t)
        ((not released-p)
         (gated-provider-release provider)
         nil)
        (t
         (>= (responsive-scripted-terminal--answer-count terminal)
             (responsive-scripted-terminal-final-provider-item-count
              terminal)))))))

(defmethod terminal-read-event ((terminal responsive-scripted-terminal))
  "Return TERMINAL's next paced event, then physical EOF after durable answers."
  (if (scripted-terminal-events terminal)
      (prog1 (pop (scripted-terminal-events terminal))
        (incf (responsive-scripted-terminal-events-read terminal)))
      ':stream-end))

(defclass waiting-recording-terminal (recording-terminal)
  ()
  (:documentation "A recording terminal with no input ready until a test stops it."))

(defmethod terminal-input-ready-p ((terminal waiting-recording-terminal))
  "Report no pending input for TERMINAL."
  (declare (ignore terminal))
  nil)

(defclass queued-recording-terminal (recording-terminal)
  ((event-lock
    :initform (make-lock "Autolith queued terminal")
    :reader queued-recording-terminal-event-lock
    :type t
    :documentation "The lock protecting events submitted by test threads.")
   (events
    :initform nil
    :accessor queued-recording-terminal-events
    :type list
    :documentation "FIFO semantic events awaiting the responsive reader."))
  (:documentation
   "A recording terminal accepting deterministic events from other threads."))

(defmethod terminal-input-ready-p ((terminal queued-recording-terminal))
  "Return true when TERMINAL has a thread-safely queued event."
  (with-lock-held ((queued-recording-terminal-event-lock terminal))
    (not (null (queued-recording-terminal-events terminal)))))

(defmethod terminal-read-event ((terminal queued-recording-terminal))
  "Return TERMINAL's next thread-safely queued event."
  (with-lock-held ((queued-recording-terminal-event-lock terminal))
    (or (pop (queued-recording-terminal-events terminal)) ':end-of-input)))

(-> queued-recording-terminal-enqueue (queued-recording-terminal t) null)
(defun queued-recording-terminal-enqueue (terminal event)
  "Append EVENT to TERMINAL's thread-safe input queue."
  (with-lock-held ((queued-recording-terminal-event-lock terminal))
    (setf (queued-recording-terminal-events terminal)
          (nconc (queued-recording-terminal-events terminal) (list event))))
  nil)


;;;; -- Focused Presentation Tests --

(-> test-application-command-tips () null)
(defun test-application-command-tips ()
  "Test command tips are mandatory metadata rendered with a styled command."
  (test-assert
   (every (lambda (command)
            (non-empty-string-p (application-command-tip command)))
          (application-command-list))
   "every canonical application command carries a non-empty tip")
  (dolist (metadata
           '((:name "/missing-tip"
              :argument nil
              :description "omit the tip"
              :busy-behavior :inspect
              :terminal-behavior :shared)
             (:name "/blank-tip"
              :argument nil
              :description "leave the tip blank"
              :tip "   "
              :busy-behavior :inspect
              :terminal-behavior :shared)))
    (test-assert
     (handler-case
         (progn
           (macroexpand-1
            `(define-application-command application-tests--invalid-command
                 ,metadata
                 (application invocation)
               (declare (ignore application invocation))
               :continue))
           nil)
       (error ()
         t))
     "missing and blank command tips fail during macro expansion"))
  (test-assert
   (and (string= (application-command-name
                  (application-command-find "/EXIT"))
                 "/quit")
        (string= (application-command-name
                  (application-command-find "/usage"))
                 "/status"))
   "declared aliases resolve through their canonical command definitions")
  (test-assert
   (eq
    (application-command-busy-action
     (application-command-find "/EXIT")
     (application-command-invocation-parse "/EXIT"))
    ':cancel)
   "quit aliases inherit the command's cancellation policy")
  (let* ((*random-state* (make-random-state nil))
         (captured-state (make-random-state *random-state*))
         (entry (application--startup-command-entry)))
    (test-assert (member entry (application-command-list) :test #'eq)
                 "startup tip selection returns one canonical command")
    (test-assert (equalp *random-state* captured-state)
                 "startup tip selection does not consume saved image RNG state"))
  (let* ((entry (first (application-command-list)))
         (spans (application--command-tip-spans entry))
         (command-span (third spans))
         (tip-span (fourth spans)))
    (test-assert (eq (terminal-span-style command-span) ':code)
                 "the command token uses the colored code style")
    (test-assert (string= (terminal-span-text command-span)
                          (application-command-name entry))
                 "the colored span contains only the canonical command")
    (test-assert (string= (terminal-span-text tip-span)
                          (format nil " ~A" (application-command-tip entry)))
                 "the command's mandatory tip follows its colored token"))
  nil)

(-> test-application-banner-version () null)
(defun test-application-banner-version ()
  "Test the Cosmic mark, adjacent metadata, narrow layout, and configured version."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "banner"))
         (terminal (make-instance 'recording-terminal :columns 80))
         (previous-recovered (uiop:getenv "AUTOLITH_RECOVERED"))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :ui (terminal-ui-create
                                          :terminal terminal))))
    (sb-posix:unsetenv "AUTOLITH_RECOVERED")
    (unwind-protect
         (let* ((spans (application-banner application))
                (text (format nil "~{~A~}"
                              (mapcar #'terminal-span-text spans)))
                (lines (uiop:split-string text :separator '(#\Newline)))
                (tip-command-spans
                  (remove-if-not
                   (lambda (span)
                     (eq (terminal-span-style span) ':code))
                   spans))
                (tip-command-span (first tip-command-spans))
                (tip-entry
                  (and tip-command-span
                       (application-command-find
                        (terminal-span-text tip-command-span))))
                (gradient-styles
                  (loop for span in spans
                        for style = (terminal-span-style span)
                        when (member style
                                     '(:brand-gradient-1 :brand-gradient-2
                                       :brand-gradient-3 :brand-gradient-4
                                       :brand-gradient-5 :brand-gradient-6))
                          collect style)))
           (test-assert
            (equal gradient-styles
                   '(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
                     :brand-gradient-4 :brand-gradient-5 :brand-gradient-6))
           "the Cosmic AL mark assigns one gradient style to each row")
           (let ((previous-recovered (uiop:getenv "AUTOLITH_RECOVERED")))
             (unwind-protect
                  (progn
                    (sb-posix:setenv "AUTOLITH_RECOVERED" "1" 1)
                    (let* ((recovered-spans (application-banner application))
                           (recovered-styles
                             (loop for span in recovered-spans
                                   for style = (terminal-span-style span)
                                   when (member
                                         style
                                         *application-recovery-gradient-styles*)
                                     collect style)))
                      (test-assert
                       (equal recovered-styles
                              *application-recovery-gradient-styles*)
                       "a recovered process renders every AL row in red")))
               (if previous-recovered
                   (sb-posix:setenv "AUTOLITH_RECOVERED"
                                   previous-recovered 1)
                   (sb-posix:unsetenv "AUTOLITH_RECOVERED"))))
           (test-assert (string= (first lines) "")
                        "the banner begins with one empty row")
           (test-assert (and (search "  :::.      :::" (second lines))
                             (search (format nil "AUTOLITH v~A"
                                             *autolith-version*)
                                     (second lines))
                             (search "────" (third lines))
                             (search "model" (fourth lines))
                             (search "workspace" (fifth lines)))
                        "wide banners divide identity from aligned runtime data")
           (let ((header-end
                   (or (search "Autolith executes" text)
                       (length text))))
             (test-assert
              (not (search "conversation" text :end2 header-end))
              "the startup header omits the internal conversation identifier"))
           (test-assert (search (format nil "v~A" *autolith-version*) text)
                        "the startup banner uses the configured version")
           (test-assert (not (search "v6.6.6" text))
                        "the startup banner contains no stale display version")
           (test-assert (= (length tip-command-spans) 1)
                        "the startup banner shows exactly one command tip")
           (test-assert
            (and tip-entry
                 (search (application-command-tip tip-entry) text))
            "the startup banner pairs a registered command with its own tip")
           (let* ((immutable-configuration
                    (configuration--clone configuration :immutable-p t))
                  (immutable-application
                    (make-instance 'application
                                   :configuration immutable-configuration
                                   :conversation conversation
                                   :ui (application-ui application)))
                  (immutable-text
                    (format nil "~{~A~}"
                            (mapcar #'terminal-span-text
                                    (application-banner
                                     immutable-application)))))
             (test-assert (search "mode          immutable" immutable-text)
                          "the startup banner identifies immutable mode"))
           (let ((logo-end (search "YUMMM" text))
                 (notice-start (search "Autolith executes" text))
                 (tip-start (search "Tip: " text)))
             (test-assert (and logo-end
                               notice-start
                               (< logo-end notice-start))
                          "the security notice follows the complete header")
             (test-assert (and notice-start
                               tip-start
                               (< notice-start tip-start))
                          "the command tip appears below the security notice"))
           (setf (terminal-columns terminal) 40)
           (let* ((narrow-spans (application-banner application))
                  (narrow-text (format nil "~{~A~}"
                                       (mapcar #'terminal-span-text
                                               narrow-spans)))
                  (logo-end (search "YUMMM" narrow-text))
                  (metadata-start (search "AUTOLITH" narrow-text)))
             (test-assert (and logo-end
                               metadata-start
                               (< logo-end metadata-start))
                          "narrow banners stack metadata below the AL mark")
             (test-assert (search "Tip: " narrow-text)
                          "narrow startup banners retain their command tip")))
      (if previous-recovered
          (sb-posix:setenv "AUTOLITH_RECOVERED" previous-recovered 1)
          (sb-posix:unsetenv "AUTOLITH_RECOVERED"))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-thinking-label-selection () null)
(defun test-thinking-label-selection ()
  "Test provider activity uses one self-modifiable word from the configured set."
  (loop repeat 20
        for label = (application-thinking-label)
        do (test-assert (member label *application-thinking-words*
                                :test #'string=)
                        "thinking labels come from the documented word set")
           (test-assert (not (find #\Space label))
                        "every thinking label is exactly one word"))
  (let ((*application-thinking-words* '("musing")))
    (test-assert (string= (application-thinking-label) "musing")
                 "changing the active word set immediately changes presentation"))
  (let ((*application-thinking-words* nil))
    (test-assert (string= (application-thinking-label) "pondering")
                 "an empty exploratory word set retains a safe fallback"))
  nil)

(-> test-startup-update-choice () null)
(defun test-startup-update-choice ()
  "Test cached update notices, attended release choices, and Nix advice."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (tag "v99.0.0")
         (release-provenance
           (make-instance 'installation-provenance
                          :method ':release
                          :current-tag (format nil "v~A" *autolith-version*)))
         (release-availability
           (make-instance 'update-availability :tag tag :method ':release)))
    (labels ((make-application (events)
               "Return a release application scripted with EVENTS."
               (make-instance
                'application
                :configuration configuration
                :installation-provenance release-provenance
                :update-availability release-availability
                :ui (terminal-ui-create
                     :terminal (make-instance 'scripted-terminal
                                              :columns 72
                                              :events events)))))
      (unwind-protect
           (progn
             (let* ((application (make-application (list :submit)))
                    (ui (application-ui application))
                    (notice (application--update-notice application))
                    (text (format nil "~{~A~}"
                                  (mapcar #'terminal-span-text notice))))
               (test-assert
                (and (search "Update available: Autolith" text)
                     (search "Choose whether to install" text))
                "a packaged release receives the cached banner warning")
               (with-terminal-ui (active-ui ui)
                 (declare (ignore active-ui))
                 (application--offer-startup-update application))
               (test-assert
                (application-update-availability application)
                "Not now continues without dismissing the cached version"))

             (let* ((application
                      (make-application (list :history-next :submit)))
                    (ui (application-ui application)))
               (test-assert
                (handler-case
                    (with-terminal-ui (active-ui ui)
                      (declare (ignore active-ui))
                      (application--offer-startup-update application)
                      nil)
                  (update-requested (condition)
                    (string= (update-requested-tag condition) tag)))
                "Update now requests launcher handoff only after UI unwind")
               (test-assert (not (terminal-started-p
                                  (terminal-ui-terminal ui)))
                            "update handoff restores the terminal first"))

             (let* ((application
                      (make-application
                       (list :history-next :history-next :submit)))
                    (ui (application-ui application)))
               (with-terminal-ui (active-ui ui)
                 (declare (ignore active-ui))
                 (application--offer-startup-update application))
               (test-assert
                (and (null (application-update-availability application))
                     (string= (update-state-dismissed-tag
                               (update-state-load configuration))
                              tag))
                "Skip this version atomically dismisses only the selected tag"))

             (let* ((availability
                      (make-instance 'update-availability
                                     :tag tag
                                     :method ':nix))
                    (application
                      (make-instance 'application
                                     :configuration configuration
                                     :update-availability availability))
                    (notice (application--update-notice application))
                    (text (format nil "~{~A~}"
                                  (mapcar #'terminal-span-text notice))))
               (test-assert
                (and (search "Installed through Nix" text)
                     (search "flake or profile" text))
                "Nix receives package-manager advice without an update action"))

             (let* ((application (make-instance 'application))
                    (thread (make-thread (lambda () (sleep 0.01))
                                         :name "Autolith update check test")))
               (setf (application-update-check-thread application) thread)
               (application--quiesce-update-check application)
               (test-assert
                (and (null (application-update-check-thread application))
                     (not (thread-alive-p thread)))
                "checkpoint quiescence joins the background update check")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)

(-> test-application-status-details () null)
(defun test-application-status-details ()
  "Test model, effort, and enclosing Git branch activity metadata."
  (let* ((base (test-configuration))
         (root (test-configuration-root base))
         (repository (merge-pathnames "status-repository/" root))
         (nested (merge-pathnames "nested/workspace/" repository)))
    (unwind-protect
         (progn
           (ensure-directories-exist nested)
           (uiop:run-program (list "git" "init" "--quiet"
                                   (namestring repository)))
           (uiop:run-program
            (list "git" "-C" (namestring repository)
                  "symbolic-ref" "HEAD" "refs/heads/chromatic"))
           (let* ((configuration
                    (configuration-with-working-directory base nested))
                  (ui (terminal-ui-create
                       :terminal (make-instance 'recording-terminal
                                                :columns 120)))
                  (application
                    (make-instance 'application
                                   :configuration configuration
                                   :ui ui)))
             (application-set-activity application "working")
             (let* ((details (terminal-ui-status-details ui))
                    (text (format nil "~{~A~}"
                                  (mapcar #'terminal-span-text details))))
               (test-assert
                (string= (application--git-branch nested) "chromatic")
                "Git branch discovery walks up from a nested workspace")
               (test-assert
                (search "gpt-5.6-sol · ultra · git chromatic"
                        text)
                "activity metadata compactly contains model, effort, and branch")
               (test-assert
                (eq (terminal-span-style (first (last details)))
                    ':status-branch)
                "the branch remains last so narrow status rows clip it first"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-reasoning-trace-command () null)
(defun test-reasoning-trace-command ()
  "Test persistent control of provider-visible reasoning summaries."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (application-tests--ui-application :columns 60))
         (ui (application-ui application))
         (terminal (terminal-ui-terminal ui))
         (provider (provider-create configuration)))
    (setf (application-configuration application) configuration
          (application-provider application) provider)
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (application-trace-command application "on")
           (test-assert (application-reasoning-traces-p application)
                        "/trace on enables reasoning-summary presentation")
           (test-assert (provider-reasoning-summaries-p provider)
                        "/trace on opts provider requests into summaries")
           (test-assert (preferences-reasoning-traces-p configuration)
                        "/trace on persists its enabled state")
           (test-assert (search "enabled and saved"
                                (recording-terminal-output terminal))
                        "/trace on confirms persistence")
           (let ((reloaded
                   (make-instance
                    'application
                    :reasoning-traces-p
                    (preferences-reasoning-traces-p configuration))))
             (test-assert (application-reasoning-traces-p reloaded)
                          "a fresh application can restore trace mode"))
           (terminal-ui-set-preview-rows
            ui
            (application--reasoning-preview-rows application "visible preview"))
           (recording-terminal-reset terminal)
           (application-trace-command application "off")
           (test-assert (not (application-reasoning-traces-p application))
                        "/trace off disables reasoning-summary presentation")
           (test-assert (not (provider-reasoning-summaries-p provider))
                        "/trace off stops requesting provider summaries")
           (test-assert (null (terminal-ui-preview-rows ui))
                        "/trace off removes an unfinished reasoning preview")
           (test-assert (not (preferences-reasoning-traces-p configuration))
                        "/trace off persists its disabled state")
           (test-assert (search "hidden and saved"
                                (recording-terminal-output terminal))
                        "/trace off confirms persistence")
           (test-assert
            (handler-case
                (progn
                  (application-trace-command application "raw")
                  nil)
              (configuration-error ()
                t))
            "unsupported trace modes signal a typed usage error"))
      (terminal-ui-stop ui)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-compact-view-command () null)
(defun test-compact-view-command ()
  "Test persistent filtering of successful routine tool results."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (application-tests--ui-application :columns 60))
         (ui (application-ui application)))
    (setf (application-configuration application) configuration)
    (terminal-ui-start ui)
    (unwind-protect
         (let ((successful-read
                 '(:tool-result :seq 1 :time 0 :call-id 1
                   :tool "fs.read" :status :ok :output "read output"))
               (failed-read
                 '(:tool-result :seq 2 :time 0 :call-id 2
                   :tool "fs.read" :status :error :output "read failed")))
           (test-assert (application-compact-view-p application)
                        "compact tool presentation defaults to enabled")
           (test-assert
            (null (conversation-record-entry application successful-read))
            "compact presentation hides successful routine results")
           (test-assert
            (conversation-record-entry application failed-read)
            "compact presentation retains failed routine results")
           (dolist (tool-name '("fs.write" "fs.edit" "shell.run"
                                "lisp.eval" "self.eval"))
             (test-assert
              (conversation-record-entry
               application
               (list :tool-result :seq 3 :time 0 :call-id tool-name
                     :tool tool-name :status :ok :output "ok"))
              (format nil "compact presentation retains successful ~A results"
                      tool-name)))
           (test-assert (eq (application-command application "/compact off")
                            ':continue)
                        "/compact off remains a nonmodal command")
           (test-assert (not (application-compact-view-p application))
                        "/compact off expands routine results")
           (test-assert (not (preferences-compact-view-p configuration))
                        "/compact off persists expanded presentation")
           (test-assert
            (conversation-record-entry application successful-read)
            "expanded presentation shows successful routine results")
           (test-assert (eq (application-command application "/compact on")
                            ':continue)
                        "/compact on remains a nonmodal command")
           (test-assert (preferences-compact-view-p configuration)
                        "/compact on persists compact presentation")
           (test-assert
            (handler-case
                (progn
                  (application-compact-view-command application "sometimes")
                  nil)
              (configuration-error ()
                t))
            "unsupported compact modes signal a typed usage error"))
      (terminal-ui-stop ui)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-command-permission-modes () null)
(defun test-command-permission-modes ()
  "Test session permission commands and fail-closed command authorization."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'recording-terminal :columns 72))
         (ui (terminal-ui-create :terminal terminal))
         (state (permissions-load configuration))
         (application
           (make-instance 'application
                          :configuration configuration
                          :ui ui
                          :permission-state state)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (test-assert
            (eq (application-authorize-command
                 application "printf unknown" root)
                ':deny)
            "ask mode denies an unknown command without an interactive owner")
           (permissions-allow :configuration configuration
                              :state         state
                              :command       "printf saved"
                              :directory     root)
           (test-assert
            (eq (application-authorize-command
                 application "printf saved" root)
                ':sandboxed)
            "ask mode accepts an exact saved command inside the sandbox")
           (application-command application "/permissions sandbox")
           (test-assert
            (eq (application-authorize-command
                 application "printf any" root)
                ':sandboxed)
            "/permissions sandbox allows sandboxed commands for the session")
           (application-command application "/permissions full")
           (test-assert
            (eq (application-authorize-command
                 application "printf any" root)
                ':full-access)
            "/permissions full grants full command access for the session")
           (application-command application "/permissions ask")
           (test-assert (eq (application-permission-mode application) ':ask)
                        "/permissions ask restores prompt mode")
           (application-command application "/permissions clear")
           (test-assert (null (permission-state-rules state))
                        "/permissions clear removes saved exact approvals")
           (test-assert (search "/permissions" (application-help))
                        "the command reference includes /permissions")
           (terminal-ui-stop ui))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-interrupt-resume-instruction () null)
(defun test-interrupt-resume-instruction ()
  "Test that Ctrl-C exits with an exact command only for durable conversations."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (labels ((interrupt-application (conversation)
               "Run one CONVERSATION until a scripted Ctrl-C and return its output."
               (let* ((terminal (make-instance 'scripted-terminal
                                               :columns 80
                                               :events (list :interrupt)))
                      (application
                        (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider nil
                                       :tool-registry (make-instance 'tool-registry)
                                       :worker nil
                                       :agent nil
                                       :ui (terminal-ui-create
                                            :terminal terminal))))
                 (application-run application)
                 (recording-terminal-output terminal))))
      (unwind-protect
           (let ((durable (conversation-create configuration
                                               :identifier "resume-this"))
                 (empty (conversation-create configuration
                                             :identifier "discard-this")))
             (conversation-append-user-message durable "keep this conversation")
             (let ((output (interrupt-application durable)))
               (test-assert (search "To resume this conversation, run:" output)
                            "Ctrl-C explains how to resume a durable conversation")
               (test-assert (search "autolith resume resume-this" output)
                            "the Ctrl-C instruction carries the exact resume command"))
             (let ((output (interrupt-application empty)))
               (test-assert (not (search "autolith resume" output))
                            "Ctrl-C gives no resume command for an empty conversation")
               (test-assert (not (probe-file (conversation-pathname empty)))
                            "Ctrl-C does not persist an empty conversation")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore))))
  nil)

(-> test-repeated-interrupt-forces-exit () null)
(defun test-repeated-interrupt-forces-exit ()
  "Test another Ctrl-C bypasses APPLICATION-RUN's blocked runtime cleanup."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration
                                            :identifier "force-resume"))
         (terminal (make-instance 'queued-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (registry (make-instance 'tool-registry))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider nil
                          :tool-registry registry
                          :worker nil
                          :agent nil
                          :ui ui))
         (barrier-lock (make-lock "Autolith shutdown cleanup test"))
         (barrier (make-condition-variable
                   :name "Autolith shutdown cleanup test"))
         (cleanup-entered-p nil)
         (cleanup-released-p nil)
         (cleanup-timed-out-p nil)
         (forced-status nil)
         (terminal-restored-before-exit-p nil)
         (first-exit-reason nil)
         (reader-alive-during-cleanup-p nil)
         (producer nil))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "preserve this work")
           (tool-registry-register
            registry
            (make-instance
             'tool-test-runtime-tool
             :namespace "test"
             :name "shutdown"
             :description "Wait at the shutdown regression barrier."
             :parameters (json-object "type" "object")
             :runtime-identity (list ':shutdown-barrier)
             :close-function
             (lambda ()
               ;; Holding the presentation lock proves the emergency path
               ;; never waits for repaint or ordinary UI serialization.
               (with-terminal-ui-locked (ui)
                 (with-lock-held (barrier-lock)
                   (let ((controller
                           (application-input-controller application)))
                     (setf first-exit-reason
                           (application-input-controller-exit-reason controller)
                           reader-alive-during-cleanup-p
                           (thread-alive-p
                            (application-input-controller-reader-thread
                             controller))))
                   (setf cleanup-entered-p t)
                   (condition-notify barrier)
                   (unless cleanup-released-p
                     (condition-wait barrier barrier-lock :timeout 2))
                   (setf cleanup-timed-out-p
                         (not cleanup-released-p)))))
             :detach-function (lambda () nil)))
           (queued-recording-terminal-enqueue terminal ':interrupt)
           (setf producer
                 (make-thread
                  (lambda ()
                    (with-lock-held (barrier-lock)
                      (unless cleanup-entered-p
                        (condition-wait barrier barrier-lock :timeout 2)))
                    (queued-recording-terminal-enqueue terminal ':interrupt))
                  :name "Autolith repeated Ctrl-C test"))
           (let ((*application-forced-exit-function*
                   (lambda (status)
                     (with-lock-held (barrier-lock)
                       (setf forced-status status
                             terminal-restored-before-exit-p
                             (and (not (terminal-started-p terminal))
                                  (not (terminal-interactive-p terminal)))
                             cleanup-released-p t)
                       (condition-notify barrier)))))
             (application-run application))
           (join-thread producer)
           (setf producer nil)
           (test-assert (eq first-exit-reason ':interrupt)
                        "the first Ctrl-C remains a graceful interrupt")
           (test-assert reader-alive-during-cleanup-p
                        "the interrupt-only reader survives into runtime cleanup")
           (test-assert (not cleanup-timed-out-p)
                        "repeated Ctrl-C escapes blocked shutdown cleanup")
           (test-assert (= forced-status *application-forced-interrupt-status*)
                        "a repeated Ctrl-C forces process status 130")
           (test-assert terminal-restored-before-exit-p
                        "forced interruption restores the terminal before exit")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (search "Ctrl-C pressed again; forcing Autolith to exit." output)
              "forced interruption explains why Autolith is exiting")
             (test-assert (search "autolith resume force-resume" output)
                          "forced interruption preserves the exact resume command")))
      (when producer
        (ignore-errors (join-thread producer)))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

(-> test-graceful-shutdown-retains-interrupt-escape () null)
(defun test-graceful-shutdown-retains-interrupt-escape ()
  "Test every graceful shutdown reason retains the emergency Ctrl-C reader."
  (dolist (reason '(:quit :end-of-input))
    (let* ((terminal (make-instance 'queued-recording-terminal :columns 80))
           (ui (terminal-ui-create :terminal terminal))
           (application (make-instance 'application :ui ui))
           (status-lock (make-lock "Autolith graceful shutdown test"))
           (forced-status nil)
           (controller nil))
      (unwind-protect
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (let ((*application-forced-exit-function*
                     (lambda (status)
                       (with-lock-held (status-lock)
                         (setf forced-status status)))))
               (setf controller
                     (application-input-controller-create application)))
             (application-input-controller--request-exit controller reason)
             (test-assert
              (thread-alive-p
               (application-input-controller-reader-thread controller))
              "graceful shutdown preserves its interrupt-only reader")
             (queued-recording-terminal-enqueue terminal ':interrupt)
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held (status-lock)
                   (= (or forced-status -1)
                      *application-forced-interrupt-status*)))
               2)
              "Ctrl-C forces exit while non-interrupt shutdown is pending")
             (application-input-controller-stop controller)
             (setf controller nil)
             (test-assert
              (search "Ctrl-C pressed during shutdown; forcing Autolith to exit."
                      (recording-terminal-output terminal))
              "forced exit distinguishes an interrupt during graceful shutdown"))
        (when controller
          (ignore-errors (application-input-controller-stop controller)))
        (ignore-errors (terminal-ui-stop ui)))))
  nil)

(-> test-transcript-entries () null)
(defun test-transcript-entries ()
  "Test styled transcript entry construction, wrapping, and output bounds."
  (let ((application (application-tests--ui-application
                      :columns 40
                      :compact-view-p nil)))
    (let ((entry (conversation-record-entry
                  application
                  '(:message :seq 1 :time 0 :role :user :content "hello there"))))
      (test-assert (equal (first entry) (terminal-span :user "❯ you"))
                   "user records present a styled you header")
      (test-assert (search "  hello there"
                           (terminal-span-text (first (last entry))))
                   "user bodies are indented beneath their header"))
    (let ((entry (conversation-record-entry
                  application
                  (list :message :seq 1 :time 0 :role :user
                        :content (make-string 50 :initial-element #\a)))))
      (test-assert (= (count #\Newline
                             (terminal-span-text (first (last entry))))
                      1)
                   "long bodies wrap at the terminal width"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"message\",\"role\":\"assistant\",
                     \"content\":[{\"type\":\"output_text\",\"text\":\"hi\"}]}"))))
      (test-assert (equal (first entry) (terminal-span :brand "● autolith"))
                   "assistant items present a styled autolith header"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"message\",\"role\":\"assistant\",
                     \"content\":[{\"type\":\"output_text\",
                                   \"text\":\"see **bold** move\"}]}"))))
      (test-assert (find (terminal-span :strong "bold") entry :test #'equal)
                   "assistant bodies render markdown emphasis"))
    (let ((item
            (json-object
             "type" "reasoning"
             "summary" (json-array
                        (json-object
                         "type" "summary_text"
                         "text" (format nil
                                        "**<thought>** Checked the safe path.~%Compared fallback behavior.~C[31m"
                                        *terminal-escape-character*)))
             "content" (json-array
                        (json-object "type" "reasoning_text"
                                     "text" "raw private reasoning")))))
      (test-assert (null (response-item-entry application item))
                   "reasoning summaries stay hidden by default")
      (let* ((visible-application
               (application-tests--ui-application
                :columns 40
                :reasoning-traces-p t))
             (entry (response-item-entry visible-application item))
             (text (markdown-tests--row-text entry)))
        (test-assert
         (equal (first entry) (terminal-span :hint "◇ reasoning summary"))
         "trace mode labels provider-visible reasoning summaries")
        (test-assert
         (and (find (terminal-span :dim "  │ ") entry :test #'equal)
              (find (terminal-span :strong "<thought>") entry :test #'equal)
              (search "Checked the safe path." text)
              (search "Compared fallback behavior." text)
              (not (search "**" text)))
         "trace mode renders inline Markdown beside a subdued rail")
        (test-assert
         (every (lambda (line)
                  (<= (text-cell-width line) 39))
                (uiop:split-string text :separator '(#\Newline)))
         "reasoning summary rails stay within the transcript width")
        (test-assert (not (find *terminal-escape-character* text))
                     "reasoning summaries neutralize terminal controls")
        (test-assert (not (search "raw private reasoning" text))
                     "trace mode never shows raw reasoning content"))
      (let* ((narrow-application
               (application-tests--ui-application
                :columns 20
                :reasoning-traces-p t))
             (entry
               (application--reasoning-summary-entry
                narrow-application
                "A deliberately long reasoning summary for a narrow terminal."))
             (text (markdown-tests--row-text entry)))
        (test-assert
         (and (> (count (terminal-span :dim "  │ ") entry :test #'equal) 1)
              (every (lambda (line)
                       (<= (text-cell-width line) 19))
                     (uiop:split-string text :separator '(#\Newline))))
         "reasoning summaries wrap with room for the rail on narrow terminals")))
    (let* ((narrow-application
             (application-tests--ui-application :columns 32))
           (rows
             (application--tool-field-rows
              narrow-application
              (list (list :label "path"
                          :value "/tmp/a-short-path"
                          :style ':code)
                    (list :label "maximum-results-per-file"
                          :value "a deliberately long value that must wrap"
                          :style ':code))))
           (label-widths
             (loop for row in rows
                   collect (text-cell-width
                            (terminal-span-text (first row))))))
      (test-assert (and (> (length rows) 2)
                        (apply #'= label-widths))
                   "long tool details wrap beneath one aligned value column")
      (test-assert
       (every (lambda (row)
                (<= (terminal--spans-width row) 29))
              rows)
       "tool detail columns stay inside their transcript cell budget"))
    (let* ((source (format nil "~{form-line-~D~^~%~}"
                           (loop for index from 1 to 10 collect index)))
           (entry (response-item-entry
                   application
                   (json-object
                    "type" "function_call"
                    "namespace" "self"
                    "name" "eval"
                    "arguments" (json-encode
                                 (json-object
                                  "form" source
                                  "restart" "CONTINUE")))))
           (text (markdown-tests--row-text entry)))
      (test-assert (equal (first entry) (terminal-span :tool "▸ self.eval"))
                   "tool requests present a styled tool header")
      (test-assert (and (search "form-line-1" text)
                        (search "… +2 more lines" text))
                   "self.eval previews only the first configured source lines")
      (test-assert (and (search "restart" text)
                        (find-if (lambda (span)
                                   (and (eq (terminal-span-style span) ':notice)
                                        (search "CONTINUE"
                                                (terminal-span-text span))))
                                 entry))
                   "self.eval presents restart selection in a separate area")
      (test-assert (not (search "{\"form\"" text))
                   "tool requests never expose raw argument JSON"))
    (let* ((entry (response-item-entry
                   application
                   (json-object
                    "type" "function_call"
                    "namespace" "lisp"
                    "name" "eval"
                    "arguments" (json-encode
                                 (json-object "form" "(+ 1 2)")))))
           (text (markdown-tests--row-text entry)))
      (test-assert (and (equal (first entry)
                               (terminal-span :tool "▸ lisp.eval"))
                        (search "(+ 1 2)" text)
                        (not (search "{\"form\"" text)))
                   "lisp.eval calls show bounded Lisp source instead of JSON"))
    (let* ((entry (response-item-entry
                   application
                   (json-object
                    "type" "function_call"
                    "namespace" "fs"
                    "name" "read"
                    "arguments" (json-encode
                                 (json-object
                                  "path" "src/application.lisp"
                                  "start-line" 5
                                  "line-count" 3)))))
           (text (markdown-tests--row-text entry)))
      (test-assert (search "src/application.lisp  lines 5-7" text)
                   "fs.read calls show the requested path and line window"))
    (let* ((root (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (format nil "autolith-edit-presentation-~A/"
                           (make-identifier))
                   (uiop:temporary-directory))))
           (pathname (merge-pathnames "example.lisp" root))
           (configuration
             (configuration-create
              :source-root (asdf:system-source-directory :autolith)
              :working-directory root))
           (edit-application
             (make-instance 'application
                            :configuration configuration
                            :tool-registry (make-default-tool-registry)
                            :ui (terminal-ui-create
                                 :terminal (make-instance 'recording-terminal
                                                          :columns 80)))))
      (unwind-protect
           (progn
             (uiop:ensure-all-directories-exist (list root))
             (with-open-file (stream pathname
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
               (format stream
                       "line 1~%line 2~%line 3~%line 4~%line 5~%~
                        line 6~%line 7~%line 8~%line 9~%before~%~
                        old value~%after~%"))
             (let* ((entry (response-item-entry
                            edit-application
                            (json-object
                             "type" "function_call"
                             "namespace" "fs"
                             "name" "edit"
                             "arguments" (json-encode
                                          (json-object
                                           "path" "example.lisp"
                                           "old-text" (format nil
                                                              "before~%~
                                                               old value~%after")
                                           "new-text" (format nil
                                                              "before~%~
                                                               new value~%after")
                                           "replace-all" t)))))
                    (text (markdown-tests--row-text entry)))
               (test-assert (and (search "example.lisp" text)
                                 (search "all occurrences" text)
                                 (not (search "changes" text)))
                            "fs.edit identifies its path and scope without a redundant label")
               (test-assert
                (and (find (terminal-span :dim "  10 │ ")
                           entry
                           :test #'equal)
                     (find (terminal-span :failure "- 11 │ ")
                           entry
                           :test #'equal)
                     (find (terminal-span :success "+ 11 │ ")
                           entry
                           :test #'equal)
                     (find (terminal-span :dim "  12 │ ")
                           entry
                           :test #'equal))
                "fs.edit uses one numbered gutter for all diff lines")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore)))
    (let ((entry (response-item-entry
                  application
                  (json-object
                   "type" "function_call"
                   "namespace" "fs"
                   "name" "edit"
                   "arguments" (json-encode
                                (json-object
                                 "path" "src/main.rs"
                                 "old-text" "fn old() { 1 }"
                                 "new-text" "fn new() { 2 }"))))))
      (test-assert
       (and (find (terminal-span :syntax-keyword "fn") entry :test #'equal)
            (find (terminal-span :syntax-function "old") entry :test #'equal)
            (find (terminal-span :syntax-function "new") entry :test #'equal)
            (find (terminal-span :syntax-number "1") entry :test #'equal)
            (find (terminal-span :syntax-number "2") entry :test #'equal))
       "fs.edit syntax-highlights removed and added source with ColorLisp"))
    (let* ((entry (response-item-entry
                   application
                   (json-object
                    "type" "function_call"
                    "namespace" "shell"
                    "name" "run"
                    "arguments" (json-encode
                                 (json-object
                                  "command" "printf hello && printf world"
                                  "directory" "/tmp/work"
                                  "timeout-seconds" 30)))))
           (text (markdown-tests--row-text entry)))
      (test-assert (and (search "$ printf" text)
                        (search "directory" text)
                        (search "/tmp/work" text)
                        (search "30 seconds" text))
                   "shell.run calls show command text and execution metadata")
      (test-assert (not (search "{\"command\"" text))
                   "shell.run calls omit raw argument JSON"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"web_search_call\",
                     \"action\":{\"type\":\"search\",
                                 \"query\":\"live lisp images\"}}"))))
      (test-assert (equal (first entry) (terminal-span :tool "▸ web search"))
                   "web search calls present a styled search header")
      (test-assert (search "live lisp images"
                           (markdown-tests--row-text entry))
                   "web search entries show their query"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 2 :time 0 :call-id 1
                         :tool "fs.read" :status :ok
                         :cpu-microseconds 1234
                         :real-microseconds 567890
                         :output (format nil
                                         "src/application.lisp lines 5-7 of 100~%~
                                          5  hidden source~%~
                                          6  more hidden source~%~
                                          7  final hidden source"))))
           (text (markdown-tests--row-text entry)))
      (test-assert (search "src/application.lisp lines 5-7 of 100" text)
                   "fs.read results show the actual path, window, and total")
      (test-assert (and (search "cpu 0.001s" text)
                        (search "real 0.568s" text))
                   "tool results show CPU and real elapsed time")
      (test-assert (not (search "hidden source" text))
                   "fs.read results omit returned file contents"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 3 :time 0 :call-id 2
                         :tool "shell.run" :status :ok
                         :output (format nil "exit 3~%command output"))))
           (text (markdown-tests--row-text entry)))
      (test-assert (and (search "exit 3" text)
                        (search "command output" text))
                   "shell.run results separate exit status from command output"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 4 :time 0 :call-id 3
                         :tool "self.eval" :status :ok
                         :output (format nil "Output:~%hello~%Values:~%42~%"))))
           (text (markdown-tests--row-text entry)))
      (test-assert (equal (first entry) (terminal-span :success "✓ self.eval"))
                   "successful tool results present a success header")
      (test-assert (and (search "output" text)
                        (search "hello" text)
                        (search "values" text)
                        (find (terminal-span :code "42") entry :test #'equal))
                   "self.eval results separate captured output from values"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 5 :time 0 :call-id 4
                         :tool "self.inspect" :status :ok
                         :output (format nil
                                         "Symbol: FOO~%Package: AUTOLITH~%~
                                          Function binding: yes~%~
                                          Lambda list: (X)~%Describe:~%details"))))
           (text (markdown-tests--row-text entry)))
      (test-assert (and (search "Symbol" text)
                        (search "FOO" text)
                        (find (terminal-span :strong "Describe")
                              entry
                              :test #'equal))
                   "introspection results use aligned fields and section headings"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "lisp.describe" :status :ok
                         :output (format nil
                                         "Output:~%Symbol: CAR~%~
                                          Documentation: list head~%~
                                          Values:~%"))))
           (text (markdown-tests--row-text entry)))
      (test-assert (and (search "description" text)
                        (search "Symbol" text)
                        (search "CAR" text)
                        (search "values" text))
                   "lisp.describe results separate structured description and values"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "self.eval" :status :error
                         :output (format nil
                                         "Needs a value.~2%Available restarts:~%~
                                            CONTINUE  Try again.~%~
                                            USE-VALUE  Supply a value.~%~
                                          Retry the identical call with a restart."))))
           (text (markdown-tests--row-text entry)))
      (test-assert (equal (first entry)
                          (terminal-span :failure "✗ self.eval failed"))
                   "failed tool results present a failure header")
      (test-assert (and (search "condition" text)
                        (search "available restarts" text)
                        (search "retry" text)
                        (find-if (lambda (span)
                                   (and (eq (terminal-span-style span) ':notice)
                                        (search "CONTINUE"
                                                (terminal-span-text span))))
                                 entry))
                   "correctable failures separate condition, restarts, and retry help")))
  (let ((application (application-tests--ui-application :columns 40)))
    (test-assert (string= (application--indented-body application
                                                      (format nil "3~%"))
                          "  3")
                 "trailing output newlines leave no blank body row"))
  (let ((help (application-help)))
    (test-assert (search "/rollback" help)
                 "help lists every interactive command")
    (test-assert (search "pick a generation for recovery" help)
                 "help lists command descriptions"))
  nil)

(-> test-task-run-call-presentation () null)
(defun test-task-run-call-presentation ()
  "Test compact, expanded, malformed, and narrow task.run call rendering."
  (let* ((registry
           (task-augment-tool-registry
            (make-default-tool-registry)))
         (compact
           (make-instance
            'application
            :compact-view-p t
            :tool-registry registry
            :ui (terminal-ui-create
                 :terminal (make-instance 'recording-terminal
                                          :columns 80))))
         (expanded
           (make-instance
            'application
            :compact-view-p nil
            :tool-registry registry
            :ui (terminal-ui-create
                 :terminal (make-instance 'recording-terminal
                                          :columns 80))))
         (narrow
           (make-instance
            'application
            :compact-view-p t
            :tool-registry registry
            :ui (terminal-ui-create
                 :terminal (make-instance 'recording-terminal
                                          :columns 24)))))
    (labels ((call (arguments &key source)
               (response-item-entry
                compact
                (json-object
                 "type" "function_call"
                 "namespace" "task"
                 "name" "run"
                 "arguments" (or source (json-encode arguments)))))

             (call-for (application arguments &key source)
               (response-item-entry
                application
                (json-object
                 "type" "function_call"
                 "namespace" "task"
                 "name" "run"
                 "arguments" (or source (json-encode arguments))))))
      (unwind-protect
           (let* ((long-assignment
                    "Inspect alpha carefully, record concrete evidence, verify every observation, and report the final sentinel EXPANDED-TAIL.")
                  (batch
                    (json-object
                     "context"
                     "Shared read-only background remains visible and wraps as ordinary prose."
                     "async" t
                     "tasks"
                     (json-array
                      (json-object
                       "name" "alpha"
                       "agent" "librarian"
                       "task" long-assignment
                       "context" "Alpha has one item-specific constraint.")
                      (json-object
                       "name" "beta"
                       "agent" "scout"
                       "async" false
                       "task" "Inspect beta and report briefly."))))
                  (compact-entry (call batch))
                  (expanded-entry (call-for expanded batch))
                  (compact-text
                    (markdown-tests--row-text compact-entry))
                  (expanded-text
                    (markdown-tests--row-text expanded-entry)))
             (test-assert
              (and (equal (first compact-entry)
                          (terminal-span :tool "▸ task.run"))
                   (search "Shared read-only background" compact-text)
                   (search "alpha" compact-text)
                   (search "beta" compact-text)
                   (search "librarian" compact-text)
                   (search "scout" compact-text)
                   (search "detached" compact-text)
                   (search "Alpha has one item-specific" compact-text))
              "compact task.run calls retain shared context and child identity")
             (test-assert
              (and (not (search "EXPANDED-TAIL" compact-text))
                   (< (count #\Newline compact-text)
                      (count #\Newline expanded-text)))
              "compact task.run calls use concise ellipsized child summaries")
             (test-assert
              (and (search "Shared read-only background" expanded-text)
                   (search "task 1" expanded-text)
                   (search "task 2" expanded-text)
                   (search "alpha" expanded-text)
                   (search "beta" expanded-text)
                   (search "librarian" expanded-text)
                   (search "scout" expanded-text)
                   (search "detached" expanded-text)
                   (search "synchronous" expanded-text)
                   (search "task context" expanded-text)
                   (search "Alpha has one item-specific" expanded-text)
                   (search "EXPANDED-TAIL" expanded-text))
              "expanded task.run calls show full child sections and modes")
             (test-assert
              (and (not (search "\"tasks\"" compact-text))
                   (not (search "{\"name\"" compact-text))
                   (not (search "\"tasks\"" expanded-text))
                   (not (search "{\"name\"" expanded-text)))
              "task.run calls never expose raw task JSON in either view")
             (let* ((flat
                      (json-object
                       "task" "Handle one unnamed child assignment."
                       "context" "One-child context."
                       "async" false))
                    (flat-entry (call-for expanded flat))
                    (flat-text
                      (markdown-tests--row-text flat-entry)))
               (test-assert
                (and (search "task 1" flat-text)
                     (search "One-child context." flat-text)
                     (search "Handle one unnamed child assignment." flat-text)
                     (search "synchronous" flat-text)
                     (find (terminal-span :agent-role "task")
                           flat-entry
                           :test #'equal))
                "flat task.run calls show default role, context, and false async"))
             (let* ((malformed-json-entry
                      (call nil :source "{not-json"))
                    (malformed-json-text
                      (markdown-tests--row-text malformed-json-entry))
                    (malformed-batch-entry
                      (call
                       (json-object
                        "context" "Still readable."
                        "tasks" "not-an-array")))
                    (malformed-batch-text
                      (markdown-tests--row-text malformed-batch-entry))
                    (malformed-item-entry
                      (call-for
                       expanded
                       (json-object
                        "context" "Batch context."
                        "tasks" (json-array
                                 "not-an-object"
                                 (json-object
                                  "name" "valid"
                                  "task" "Continue rendering.")))))
                    (malformed-item-text
                      (markdown-tests--row-text malformed-item-entry)))
               (test-assert
                (and (search "arguments unavailable" malformed-json-text)
                     (search "invalid task batch" malformed-batch-text)
                     (search "Still readable." malformed-batch-text)
                     (search "invalid task definition" malformed-item-text)
                     (search "valid" malformed-item-text)
                     (not (search "\"tasks\"" malformed-batch-text)))
                "malformed task.run calls remain structured and render safely"))
             (let* ((narrow-entry
                      (call-for narrow batch))
                    (narrow-text
                      (markdown-tests--row-text narrow-entry)))
               (test-assert
                (and
                 (every
                  (lambda (line)
                    (<= (text-cell-width line) 23))
                  (uiop:split-string narrow-text
                                     :separator '(#\Newline)))
                 (not (find *terminal-escape-character* narrow-text)))
                "task.run rows stay safe inside a narrow terminal"))
             (let* ((many-lines
                      (format nil
                              "start-marker~%~{middle-~D~%~}end-marker"
                              (loop for index from 1 to 20 collect index)))
                    (bounded-entry
                      (call-for
                       expanded
                       (json-object "task" many-lines)))
                    (bounded-text
                      (markdown-tests--row-text bounded-entry)))
               (test-assert
                (and (search "start-marker" bounded-text)
                     (search "more row" bounded-text)
                     (not (search "end-marker" bounded-text)))
                "expanded task instructions remain explicitly bounded")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-recovery-cursor-normalization () null)
(defun test-recovery-cursor-normalization ()
  "Test recovered transcript cursors cannot exceed durable conversation state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "cursor-recovery")))
    (unwind-protect
         (progn
           (dotimes (index 3)
             (conversation-append-user-message
              conversation
              (format nil "message-~D" index)))
           (multiple-value-bind (rendered floor)
               (application--normalize-recovery-cursors
                conversation t 999 999)
             (test-assert
              (and (= rendered 3) (null floor))
              "oversized recovered cursors clamp or reset at the durable tail"))
           (multiple-value-bind (rendered floor)
               (application--normalize-recovery-cursors
                conversation t 2 3)
             (test-assert
              (and (= rendered 2) (= floor 3))
              "a valid recovered history boundary remains intact"))
           (multiple-value-bind (rendered floor)
               (application--normalize-recovery-cursors
                conversation nil 2 1)
             (test-assert
              (and (zerop rendered) (null floor))
              "cursor metadata is ignored outside its recovered conversation")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-bounded-transcript-replay () null)
(defun test-bounded-transcript-replay ()
  "Test startup replays exactly the newest five hundred visible entries."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "bounded-replay"))
         (terminal (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui (terminal-ui-create :terminal terminal))))
    (unwind-protect
         (progn
           (loop for index below 501
                 do (conversation-append-user-message
                     conversation
                     (format nil "visible-entry-~3,'0D" index)))
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (terminal-tests--substring-count "❯ you" output) 500)
              "startup emits no more than five hundred visible entries")
             (test-assert
              (and (not (search "visible-entry-000" output))
                   (search "visible-entry-001" output)
                   (search "visible-entry-500" output))
              "startup retains exactly the newest five hundred visible entries")
             (test-assert
              (search
               "showing the newest 500 of 501 transcript entries"
               output)
              "bounded startup replay explains its omitted older entry"))
           (test-assert
            (= (application-rendered-sequence application) 501)
            "bounded startup replay advances the live cursor through omitted entries")
           (recording-terminal-reset terminal)
           (conversation-append-user-message conversation "one-live-append")
           (application-render-records application)
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (terminal-tests--substring-count "one-live-append" output) 1)
              "a newly appended live record renders exactly once")
             (test-assert
              (= (terminal-tests--substring-count "❯ you" output) 1)
              "incremental rendering does not replay the bounded startup page"))
           (let* ((recovery-terminal
                    (make-instance 'recording-terminal :columns 80))
                  (recovery-application
                    (make-instance
                     'application
                     :configuration configuration
                     :conversation conversation
                     :ui
                     (terminal-ui-create :terminal recovery-terminal))))
             (setf (application-rendered-sequence recovery-application) 1
                   (application-history-floor-sequence recovery-application) 1)
             (application-render-records recovery-application)
             (let ((output
                     (recording-terminal-output recovery-terminal)))
               (test-assert
                (= (terminal-tests--substring-count "❯ you" output) 500)
                "recovery also bounds output after a stale durable cursor")
               (test-assert
                (and (not (search "visible-entry-001" output))
                     (search "visible-entry-002" output)
                     (search "one-live-append" output)
                     (search "entries added after recovery" output))
                "recovery retains only the newest unread transcript page"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-hidden-reasoning-does-not-crowd-replay () null)
(defun test-hidden-reasoning-does-not-crowd-replay ()
  "Test hidden reasoning records do not consume visible replay capacity."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "visible-replay"))
         (terminal (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider nil
                          :reasoning-traces-p nil
                          :ui (terminal-ui-create :terminal terminal))))
    (unwind-protect
         (let ((*application-history-page-size* 3))
           (loop for index from 1 to 4
                 do (conversation-append-user-message
                     conversation
                     (format nil "visible-message-~D" index)))
           (loop for index from 1 to 8
                 do (conversation-append-provider-item
                     conversation
                     (json-object
                      "type" "reasoning"
                      "summary"
                      (json-array
                       (json-object
                        "type" "summary_text"
                        "text" (format nil "hidden-reasoning-~D" index)))
                      "encrypted_content" "opaque")))
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (terminal-tests--substring-count "❯ you" output) 3)
              "the replay page is filled with visible entries")
             (test-assert
              (and (not (search "visible-message-1" output))
                   (search "visible-message-2" output)
                   (search "visible-message-3" output)
                   (search "visible-message-4" output))
              "hidden records cannot crowd older visible messages from the page")
             (test-assert
              (not (search "hidden-reasoning-" output))
              "disabled reasoning summaries remain absent from replay"))
           (test-assert
            (= (application-rendered-sequence application) 12)
            "hidden trailing records still advance the live render cursor")
           (recording-terminal-reset terminal)
           (application-set-reasoning-traces application t)
           (application-render-history application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "hidden-reasoning-6" output)
                   (search "hidden-reasoning-7" output)
                   (search "hidden-reasoning-8" output))
              "changing visibility makes the newest newly visible page replayable")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-paged-transcript-history () null)
(defun test-paged-transcript-history ()
  "Test /history appends bounded older pages without replaying live output."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "paged-history"))
         (terminal (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui (terminal-ui-create :terminal terminal))))
    (unwind-protect
         (let ((*application-history-page-size* 2))
           (loop for index from 1 to 5
                 do (conversation-append-user-message
                     conversation
                     (format nil "history-message-~D" index)))
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (not (search "history-message-3" output))
                   (search "history-message-4" output)
                   (search "history-message-5" output))
              "bounded startup replay begins with the newest history page"))
           (recording-terminal-reset terminal)
           (conversation-append-user-message conversation "history-live-message")
           (application-render-records application)
           (application-render-records application)
           (test-assert
            (= (terminal-tests--substring-count
                "history-live-message"
                (recording-terminal-output terminal))
               1)
            "live output remains singular before older history is loaded")
           (recording-terminal-reset terminal)
           (test-assert
            (eq (application-command application "/history") ':continue)
            "/history keeps the interactive application running")
           (let* ((output (recording-terminal-output terminal))
                  (second-position (search "history-message-2" output))
                  (third-position (search "history-message-3" output)))
             (test-assert
              (and (search "2 earlier transcript entries" output)
                   second-position
                   third-position
                   (< second-position third-position))
              "the first older page is labeled and internally chronological")
             (test-assert
              (and (not (search "history-message-4" output))
                   (not (search "history-message-5" output))
                   (not (search "history-live-message" output)))
              "older history does not duplicate the startup or live page")
             (test-assert
              (search "more earlier history remains" output)
              "a full older page advertises the remaining history"))
           (recording-terminal-reset terminal)
           (application-command application "/history")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "1 earlier transcript entry" output)
                   (search "history-message-1" output)
                   (not (search "history-message-2" output)))
              "the final partial page contains only the oldest remaining entry")
             (test-assert
              (not (search "more earlier history remains" output))
              "the final partial page does not claim more history"))
           (recording-terminal-reset terminal)
           (application-command application "/history")
           (test-assert
            (search "No earlier transcript history remains."
                    (recording-terminal-output terminal))
            "history reports exhaustion after the final page"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-streaming-presentation () null)
(defun test-streaming-presentation ()
  "Test safe streaming, exact record reconciliation, and live tool entries."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "stream-test"))
                (terminal (make-instance 'recording-terminal :columns 30))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :reasoning-traces-p t
                                            :tool-registry
                                            (make-default-tool-registry)
                                            :ui (terminal-ui-create
                                                 :terminal terminal)))
                (observer (application-agent-observer application))
                (send-text (callback-agent-observer-text-callback observer))
                (send-reasoning
                  (callback-agent-observer-reasoning-callback observer))
                (send-status (callback-agent-observer-status-callback observer))
                (reasoning-prefix "**<thought>** Checking the safe path.")
                (reasoning-suffix
                  " Comparing fallback behavior and verifying the live preview remains separate from status.")
                (reasoning-first-part
                  (concatenate 'string reasoning-prefix reasoning-suffix))
                (reasoning-second-part
                  "**<thought>** Confirming the durable summary matches.")
                (streamed-text (format nil
                                       "The quick brown fox jumps over~%the lazy dog")))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-reasoning reasoning-prefix)
           (let* ((ui (application-ui application))
                  (preview (terminal-ui-preview-rows ui))
                  (preview-text
                    (format nil "~{~A~^~%~}"
                            (mapcar #'markdown-tests--row-text preview))))
             (test-assert
              (and (search "◇ reasoning summary" preview-text)
                   (search "  │ " preview-text)
                   (find (terminal-span :strong "<thought>")
                         (apply #'append preview)
                         :test #'equal)
                   (not (search "**" preview-text)))
              "reasoning deltas render as a styled live trace block")
             (test-assert
              (and (member (terminal-ui-status ui)
                           *application-thinking-words*
                           :test #'string=)
                   (not (search "thought" (terminal-ui-status ui)
                                :test #'char-equal)))
              "the activity status stays separate from reasoning text"))
           (funcall send-reasoning reasoning-suffix)
           (let ((preview (terminal-ui-preview-rows (application-ui application))))
             (test-assert
              (and (<= (length preview)
                       *application-reasoning-preview-row-limit*)
                   (find (terminal-span :dim "  │ …")
                         (apply #'append preview)
                         :test #'equal))
              "long live reasoning traces retain a bounded recent preview"))
           (funcall send-reasoning
                    (format nil "~2%~A" reasoning-second-part))
           (recording-terminal-reset terminal)
           (funcall send-text (format nil
                                      "The quick brown fox jumps over~%"))
           (test-assert (null (terminal-ui-preview-rows
                              (application-ui application)))
                        "assistant output replaces the live reasoning preview")
           (test-assert
            (string= (terminal-ui-status (application-ui application))
                     "receiving response")
            "assistant streaming keeps a timed activity phase visible")
           (funcall send-reasoning " late event")
           (test-assert (null (terminal-ui-preview-rows
                              (application-ui application)))
                        "late reasoning deltas cannot resurrect a finalized preview")
           (funcall send-text "the lazy dog")
           (let* ((streamed (recording-terminal-output terminal))
                  (reasoning-position (search "◇ reasoning summary" streamed))
                  (assistant-position (search "● autolith" streamed)))
             (test-assert (and reasoning-position
                               assistant-position
                               (< reasoning-position assistant-position))
                          "the reasoning summary finalizes above assistant output")
             (test-assert (search "Confirming" streamed)
                          "multiple reasoning summary parts stay visibly separated")
             (test-assert (search "The quick brown fox" streamed)
                          "newline-terminated logical lines commit while streaming"))
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "reasoning"
             "summary" (json-array
                        (json-object "type" "summary_text"
                                     "text" reasoning-first-part)
                        (json-object "type" "summary_text"
                                     "text" reasoning-second-part))
             "encrypted_content" "opaque-reasoning"))
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" streamed-text))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-completed nil)
           (let ((completion (recording-terminal-output terminal)))
             (test-assert (search "the lazy dog" completion)
                          "completing a request commits the fluid tail")
             (test-assert (not (search "● autolith" completion))
                          "streamed message records do not render again")
             (test-assert (not (search "◇ reasoning summary" completion))
                          "streamed reasoning records do not render below the answer"))
           (setf (application-rendered-sequence application) 0
                 (application-render-position application) 0
                 (application-transcript-synchronized-p application) nil
                 (application-history-floor-sequence application) nil)
           (recording-terminal-reset terminal)
           (application-render-records application)
           (test-assert
            (not (search "The quick brown fox"
                         (recording-terminal-output terminal)))
            "replaying a conversation does not duplicate streamed messages")
           (test-assert
            (not (search "◇ reasoning summary"
                         (recording-terminal-output terminal)))
            "replaying a conversation does not duplicate streamed reasoning")
           (let ((tool-reasoning "**<thought>** Inspect the value with a tool."))
             (funcall send-status :provider-request-started nil)
             (funcall send-reasoning tool-reasoning)
             (conversation-append-provider-item
              conversation
              (json-object
               "type" "reasoning"
               "summary" (json-array
                          (json-object "type" "summary_text"
                                       "text" tool-reasoning))
               "encrypted_content" "opaque-tool-reasoning"))
             (conversation-append-provider-item
              conversation
              (json-object
               "type" "function_call"
               "call_id" "call-live"
               "namespace" "self"
               "name" "eval"
               "arguments" (json-encode (json-object "form" "(+ 1 2)"))))
             (recording-terminal-reset terminal)
             (funcall send-status :provider-request-completed nil)
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (and (= (terminal-tests--substring-count
                         "◇ reasoning summary"
                         output)
                        1)
                     (search "<thought>" output)
                     (search "▸ self.eval" output)
                     (null (terminal-ui-preview-rows
                            (application-ui application))))
                "tool-only provider steps finalize one trace before the tool call")))
           (conversation-append-tool-result
            conversation
            "call-live"
            :tool-name "self.eval"
            :output "42"
            :success-p t)
           (recording-terminal-reset terminal)
           (funcall send-status :tool-call-completed (list :tool "self.eval"))
           (test-assert (search "✓ self.eval"
                                (recording-terminal-output terminal))
                        "tool results render as soon as they complete")
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "plain answer"))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-started nil)
           (funcall send-text "")
           (funcall send-status :provider-request-completed nil)
           (test-assert (search "plain answer"
                                (recording-terminal-output terminal))
                        "empty deltas cannot suppress a durable assistant message")
           (funcall send-status :provider-request-started nil)
           (funcall send-text "provisional answer")
           (conversation-append-provider-item
            conversation
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (json-array
                        (json-object "type" "output_text"
                                     "text" "corrected answer"))))
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-completed nil)
           (test-assert (search "corrected answer"
                                (recording-terminal-output terminal))
                        "mismatched stream text cannot hide the durable answer")
           (recording-terminal-reset terminal)
           (funcall send-status :provider-request-started nil)
           (funcall send-text (format nil "```lisp~%(+ 1 2)~%"))
           (test-assert (search "1 │ (+ 1 2)"
                                (recording-terminal-output terminal))
                        "streamed code blocks render numbered gutters")
           (funcall send-status :provider-request-completed nil)
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-provider-retry-presentation () null)
(defun test-provider-retry-presentation ()
  "Test reconnect presentation closes and labels interrupted streamed output."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "retry-presentation"))
                (terminal (make-instance 'recording-terminal :columns 50))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :reasoning-traces-p t
                                 :ui (terminal-ui-create :terminal terminal)))
                (observer (application-agent-observer application))
                (send-text (callback-agent-observer-text-callback observer))
                (send-reasoning
                  (callback-agent-observer-reasoning-callback observer))
                (send-status
                  (callback-agent-observer-status-callback observer)))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-reasoning "Partial reasoning")
           (funcall send-text "Partial answer")
           (recording-terminal-reset terminal)
           (funcall send-status
                    :provider-retrying
                    (list :attempt 1 :maximum-attempts 5 :delay 1))
           (let ((output (recording-terminal-output terminal))
                 (ui (application-ui application)))
             (test-assert
              (and (search "Partial answer" output)
                   (search "provider stream interrupted; retrying 1/5" output)
                   (string= (terminal-ui-status ui)
                            "reconnecting 1/5 in 1s")
                   (null (terminal-ui-preview-rows ui))
                   (null (terminal-ui-stream-tail ui)))
              "a reconnect closes and labels the partial presentation attempt"))
           (recording-terminal-reset terminal)
           (funcall send-text "Replacement answer")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "● autolith" output)
                   (search "Replacement answer" output))
              "the replacement attempt starts a distinct assistant block"))
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-turn-cursor-visibility () null)
(defun test-turn-cursor-visibility ()
  "Test model turns retain the editable cursor while updates hide motion atomically."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "cursor-turn"))
                (terminal (make-instance 'recording-terminal :columns 50))
                (ui (terminal-ui-create :terminal terminal))
                (provider
                  (make-instance
                   'cursor-observing-provider
                   :visibility-function
                   (lambda ()
                     (live-region-cursor-visible-p
                      (terminal-ui-live-region ui)))
                   :results
                   (list
                    (agent-test-result
                     "cursor-response"
                     (list (agent-test-message "finished"))
                     :turn-completion :end))))
                (registry (make-instance 'tool-registry))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker t))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :provider provider
                                            :tool-registry registry
                                            :worker t
                                            :agent agent
                                            :ui ui)))
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (recording-terminal-reset terminal)
             (application--run-turn application "hello")
             (test-assert
              (cursor-observing-provider-visible-during-request-p provider)
              "the editable cursor remains visible during provider work")
             (test-assert
              (live-region-cursor-visible-p (terminal-ui-live-region ui))
              "the input cursor is restored after the model turn")
             (let* ((output (recording-terminal-output terminal))
                    (hide (format nil "~C[?25l"
                                  *terminal-escape-character*))
                    (show (format nil "~C[?25h"
                                  *terminal-escape-character*))
                    (hide-count
                      (terminal-tests--substring-count hide output))
                    (show-count
                      (terminal-tests--substring-count show output)))
               (test-assert
                (and (plusp hide-count) (plusp show-count))
                "compound model updates hide motion and restore the cursor")
               (test-assert
                (< (or (search "finished" output) most-positive-fixnum)
                   (or (search show output :from-end t) -1))
                "the final cursor reveal follows the completed answer")
               (test-assert
                (= (terminal-tests--substring-count "hello" output) 1)
                "durable user input reaches scrollback exactly once")
               (test-assert
                (= (application-rendered-sequence application)
                   (1- (conversation-next-sequence conversation)))
                "the transcript cursor follows the actual durable tail"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-responsive-model-input () null)
(defun test-responsive-model-input ()
  "Test steering, follow-up queueing, and cursor stability during model turns."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "responsive-input"))
                (provider
                  (make-instance
                   'gated-provider
                   :results
                   (list
                    (agent-test-result
                     "responsive-tool"
                     (list
                      (agent-test-call
                       :call-id "responsive-call"
                       :arguments "{\"value\":\"before steering\"}")))
                    (agent-test-result
                     "responsive-steered"
                     (list (agent-test-message "steered answer"))
                     :turn-completion :end)
                    (agent-test-result
                     "responsive-queued"
                     (list (agent-test-message "queued answer"))
                     :turn-completion :end))))
                (terminal
                  (make-instance
                   'responsive-scripted-terminal
                   :columns 60
                   :provider provider
                   :conversation conversation
                   :final-provider-item-count 3
                   :events
                   (list '(:insert "first message")
                         :submit
                         '(:insert "steer this turn")
                         :submit
                         '(:insert "queued follow-up")
                         :complete
                         '(:insert "draft survives"))))
                (ui (terminal-ui-create :terminal terminal))
                (registry (agent-test-registry))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker nil))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :provider provider
                                            :tool-registry registry
                                            :worker nil
                                            :agent agent
                                            :ui ui)))
           (application-run application)
           (test-assert
            (string= (line-editor-text (terminal-ui-editor ui))
                     "draft survives")
            "draft input survives steering and a queued follow-up turn")
           (let* ((records
                    (rest (conversation--read-records
                           (conversation-pathname conversation))))
                  (user-messages
                    (loop for record in records
                          when (and (eq (first record) ':message)
                                    (eq (getf (rest record) :role) ':user))
                            collect (getf (rest record) :content)))
                  (output (recording-terminal-output terminal)))
             (test-assert (equal user-messages
                                 '("first message"
                                   "steer this turn"
                                   "queued follow-up"))
                          "steering precedes the post-turn follow-up")
             (test-assert
              (equal (nreverse (scripted-provider-input-counts provider))
                     '(1 4 6))
              "Enter reaches the current tool loop and Tab starts a later turn")
             (test-assert (search "steered answer" output)
                          "the steered response reaches scrollback")
             (test-assert (search "queued answer" output)
                          "the Tab-queued response reaches scrollback")
             (test-assert (search "steering 1/1  steer this turn" output)
                          "the live region previews pending steering")
             (test-assert (search "follow-up 1/1  queued follow-up" output)
                          "the live region previews post-turn follow-up input")
             (test-assert
              (live-region-cursor-visible-p (terminal-ui-live-region ui))
              "responsive model turns leave the input cursor visible")
             (test-assert (not (terminal-tests--forbidden-control-p output))
                          "concurrent input and streaming preserve scrollback")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-responsive-goal-inspection () null)
(defun test-responsive-goal-inspection ()
  "Test argument-free /goal remains available during an active model turn."
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller nil))
    (setf (application-goal application)
          (list :objective "finish the migration"
                :status ':active
                :continuations 0
                :created-at (get-universal-time)))
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (setf controller (application-input-controller-create application))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':message "active turn")
             (application-input-controller--next-work controller)
             (application-input-controller--handle-submission
              controller "/goal")
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (eq
                 (let* ((invocation
                          (application-command-invocation-parse "/goal pause"))
                        (command
                          (application-command-invocation-command invocation)))
                   (application-command-busy-action command invocation))
                 ':hold)
                "goal mutations remain on the serialized application thread")
               (test-assert
                (search "finish the migration" output)
                "/goal renders the active goal while a turn is running")
               (test-assert
                (not (search "command held" output))
                "/goal is not held until the active response finishes")
               (test-assert
                (zerop
                 (length
                  (line-editor-text (terminal-ui-editor ui))))
                "/goal does not return to the editor after inspection")))
        (when controller
          (application-input-controller-stop controller)))))
  nil)

(-> test-input-reader-quiescence () null)
(defun test-input-reader-quiescence ()
  "Test modal and checkpoint work can temporarily restore a single Lisp thread."
  (labels ((terminal-owner-p (input)
             "Return the registered terminal policy for command INPUT."
             (let* ((invocation (application-command-invocation-parse input))
                    (command
                      (application-command-invocation-command invocation)))
               (and command
                    (application-command-terminal-owner-p
                     command invocation)))))
    (test-assert (terminal-owner-p "/model")
                 "argument-free picker commands take terminal ownership")
    (test-assert (terminal-owner-p "/permissions")
                 "the permission picker takes terminal ownership")
    (test-assert (terminal-owner-p "/model gpt-5.6-luna")
                 "explicit model changes own the terminal for the effort picker")
    (test-assert (terminal-owner-p "/resume")
                 "resume declares terminal ownership for its modal selector")
    (test-assert (not (terminal-owner-p "/compact"))
                 "nonmodal model commands retain responsive input")
    (test-assert (terminal-owner-p "/auth")
                 "authentication owns terminal mode while it runs"))
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller nil))
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (setf controller
            (application-input-controller-create
             application
             :initial-work-items
             (list (list ':project-adaptation-offer))))
      (unwind-protect
           (progn
             (let ((work
                     (application-input-controller--next-work controller)))
               (test-assert
                (and (equal work (list ':project-adaptation-offer))
                     (application-input-controller-active-p controller))
                "startup adaptation work enters the interruptible active path")
               (application-input-controller--finish-work controller))
             (test-assert
              (thread-alive-p
               (application-input-controller-reader-thread controller))
              "the responsive terminal reader starts independently")
             (test-assert
              (application-input-controller-call-with-reader-paused
               controller
               #'checkpoint--single-threaded-p)
              "pausing input leaves checkpoint work on the only Lisp thread")
             (test-assert
              (thread-alive-p
               (application-input-controller-reader-thread controller))
              "the terminal reader restarts after single-threaded work"))
        (application-input-controller-stop controller))))
  (test-assert
   (equal (application--initial-work-items nil t)
          (list (list ':project-adaptation-offer)))
   "explicit command-line resume queues interruptible adaptation work")
  (test-assert
   (equal (application--initial-work-items "/resume" t)
          (list (list ':command "/resume")))
   "bare command-line resume qualifies only after its picker selection")
  nil)

(-> test-late-steering-promotion () null)
(defun test-late-steering-promotion ()
  "Test steering with no later tool runs before already queued follow-up input."
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller nil))
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (setf controller (application-input-controller-create application))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':message "active turn")
             (test-assert
              (equal (application-input-controller--next-work controller)
                     '(:message "active turn"))
              "the initial submission becomes active work")
             (application-input-controller--enqueue
              controller ':message "tab follow-up")
             (application-input-controller--enqueue-steering
              controller "late enter")
             (application-input-controller--finish-work controller)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     '(:message "late enter"))
              "unconsumed Enter input moves ahead of Tab follow-ups")
             (application-input-controller--finish-work controller)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     '(:message "tab follow-up"))
              "Tab input remains queued after promoted steering"))
        (when controller
          (application-input-controller-stop controller)))))
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller nil))
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (setf controller (application-input-controller-create application))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':message "active turn")
             (application-input-controller--next-work controller)
             (application-input-controller--enqueue
              controller ':message "first queued thought")
             (application-input-controller--enqueue
              controller ':message "newest queued thought")
             (test-assert
              (application-input-controller--recall-follow-up controller)
              "empty Tab can recall the newest queued follow-up")
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "newest queued thought")
              "a recalled follow-up becomes ordinary editable input")
             (test-assert
              (equal (application-input-controller-work-items controller)
                     '((:message "first queued thought")))
              "recalling a follow-up preserves earlier queue order")
             (test-assert
              (search "follow-up 1/2  first queued thought"
                      (recording-terminal-output terminal))
              "the live queue shows message previews before editing"))
        (when controller
          (application-input-controller-stop controller)))))
  nil)

(-> test-conversation-picker () null)
(defun test-conversation-picker ()
  "Test saved-conversation picker items and interactive selection."
  (multiple-value-bind (requested-p identifier)
      (main--resume-selection '("resume"))
    (test-assert (and requested-p (null identifier))
                 "plain resume requests the interactive conversation picker"))
  (multiple-value-bind (requested-p identifier)
      (main--resume-selection '("resume" "saved-conversation"))
    (test-assert (and requested-p
                      (string= identifier "saved-conversation"))
                 "resume accepts an explicit conversation identifier"))
  (multiple-value-bind (requested-p identifier)
      (main--resume-selection '("--resume" "saved-conversation"))
    (test-assert (and (not requested-p) (null identifier))
                 "the removed --resume option is not recognized"))
  (test-assert (search "--immutable" (main-usage))
               "command-line help documents immutable mode")
  (test-assert (search "--from-source" (main-usage))
               "command-line help documents deliberate source startup")
  (test-assert (not (search "--resume" (main-usage)))
               "command-line help omits the removed --resume option")
  (test-assert (search "--image" (main-usage))
               "command-line help documents initial local images")
  (test-assert
   (equal (main--image-values
           '("-i" "one.png,two.png" "--image=three.png"
             "--image" "four.png"))
          '("one.png" "two.png" "three.png" "four.png"))
   "repeatable image options preserve comma-delimited command-line order")
  (test-assert
   (handler-case
       (progn
         (main--image-values '("--image"))
         nil)
     (configuration-error ()
       t))
   "a command-line image option requires its pathname")
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (image (merge-pathnames "initial.png" root)))
    (unwind-protect
         (progn
           (test-conversation--write-tiny-png image)
           (let ((input
                   (main--initial-image-input
                    (list "--image" (namestring image)))))
             (test-assert
              (and (typep input 'user-message-input)
                   (string= (user-message-input-text input) "[Image #1]")
                   (equal (user-message-input-image-pathnames input)
                          (list (truename image))))
              "command-line images preload a labelled composer draft")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (current-workspace (merge-pathnames "current-workspace/" root))
         (other-workspace (merge-pathnames "other-workspace/" root)))
    (ensure-directories-exist current-workspace)
    (ensure-directories-exist other-workspace)
    (unwind-protect
         (let* ((configuration
                  (configuration-with-working-directory
                   base-configuration
                   current-workspace))
                (other-configuration
                  (configuration-with-working-directory
                   base-configuration
                   other-workspace))
                (current-older
                  (conversation-create configuration
                                       :identifier "current-older"))
                (active (conversation-create configuration :identifier "active"))
                (other-older
                  (conversation-create other-configuration
                                       :identifier "other-older"))
                (other-newer
                  (conversation-create other-configuration
                                       :identifier "other-newer"))
                (terminal (make-instance 'scripted-terminal :columns 60))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation active
                                            :ui (terminal-ui-create
                                                 :terminal terminal))))
           (conversation-append-user-message current-older
                                             "older saved conversation")
           (conversation-append-user-message
            active
            "please refresh the transcript colors")
           (conversation-append-user-message other-older
                                             "older other conversation")
           (conversation-append-user-message other-newer
                                             "newer other conversation")
           (let ((now (- (get-universal-time)
                         *unix-epoch-universal-time*)))
             (flet ((set-activity (conversation seconds-ago)
                      (let ((time (- now seconds-ago)))
                        (sb-posix:utime
                         (namestring (conversation-pathname conversation))
                         time
                         time))))
               (set-activity active 10)
               (set-activity other-newer 20)
               (set-activity current-older 30)
               (set-activity other-older 40)))
           (let ((items (application--conversation-items application)))
             (test-assert (= (length items) 4)
                          "every saved conversation is offered")
             (test-assert
              (equal (mapcar (lambda (item) (getf item :name)) items)
                     '("active" "current-older" "other-newer" "other-older"))
              "resume groups current sessions first and sorts each by activity")
             (test-assert
              (and (search "current directory" (getf (first items) :group))
                   (search "current directory" (getf (second items) :group))
                   (string= (getf (third items) :group) "other sessions")
                   (string= (getf (fourth items) :group) "other sessions"))
              "resume items identify their current and other session groups")
             (test-assert (search ", current"
                                  (getf (find "active" items
                                              :key (lambda (item)
                                                     (getf item :name))
                                              :test #'string=)
                                        :description))
                          "the active conversation is marked current")
             (test-assert
              (search (application--abbreviated-directory
                       (namestring current-workspace))
                      (getf (first items) :group))
              "the current session heading identifies its directory")
             (test-assert
              (search (application--abbreviated-directory
                       (namestring other-workspace))
                      (getf (third items) :description))
              "other session rows identify their origin directories")
             (test-assert
              (search "· please refresh the transcript colors"
                      (getf (first items) :description))
              "picker items preview the newest message")
             (terminal-ui-start (application-ui application))
             (setf (scripted-terminal-events terminal) (list :submit))
             (test-assert (string= (application--pick-identifier
                                    application
                                    :title "resume conversation"
                                    :items items
                                    :usage "Usage: /resume ID"
                                    :empty-notice "none")
                                   (getf (first items) :name))
                          "enter picks the highlighted conversation")
             (terminal-ui-stop (application-ui application))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let ((application (application-tests--ui-application :columns 60)))
    (test-assert (handler-case
                     (progn
                       (application--pick-identifier application
                                                     :title "resume"
                                                     :items nil
                                                     :usage "Usage: /resume ID"
                                                     :empty-notice "none")
                       nil)
                   (configuration-error (condition)
                     (not (null (search "Usage: /resume"
                                        (format nil "~A" condition))))))
                 "non-interactive pickers demand an explicit identifier"))
  nil)

(-> test-working-directory-switch () null)
(defun test-working-directory-switch ()
  "Test transactional application, process, and worker workspace changes."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (workspace (merge-pathnames "workspace with spaces/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (pool nil))
    (ensure-directories-exist workspace)
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "working-directory"))
                (provider (provider-create configuration))
                (registry (make-default-tool-registry))
                (worker-pool (lisp-worker-pool-create configuration))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker worker-pool))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :provider provider
                                 :tool-registry registry
                                 :worker worker-pool
                                 :agent agent
                                 :ui nil))
                (worker nil))
           (setf pool worker-pool
                 worker (lisp-worker-pool-start pool "workspace" "pristine"))
           (lisp-worker-request
            worker :eval '(:form "(defparameter *workspace-marker* 73)"))
           (let ((selected (application-set-working-directory application workspace)))
             (test-assert (equal selected (truename workspace))
                          "workspace switching returns the selected directory")
             (test-assert
              (equal (configuration-working-directory
                      (application-configuration application))
                     (truename workspace))
              "workspace switching replaces the application configuration")
             (test-assert (equal (uiop:getcwd) (truename workspace))
                          "workspace switching changes the process directory")
             (test-assert (equal *default-pathname-defaults* (truename workspace))
                          "workspace switching changes pathname defaults")
             (test-assert
              (equal (configuration-working-directory
                      (agent-configuration (application-agent application)))
                     (truename workspace))
              "workspace switching reconnects the agent with the new directory")
             (test-assert
              (equal (configuration-working-directory
                      (provider-configuration
                       (application-provider application)))
                     (truename workspace))
              "workspace switching reconnects the provider with the new directory"))
           (let ((worker-state
                   (lisp-worker-request
                    worker
                    :eval
                    '(:form
                      "(list *workspace-marker* (namestring (uiop:getcwd)))"))))
             (test-assert
              (and (search "73" (first (getf (rest worker-state) :values)))
                   (search (namestring workspace)
                           (first (getf (rest worker-state) :values))))
              "workspace switching moves a live REPL without losing its heap"))
           (let ((active-configuration (application-configuration application)))
             (test-assert
              (handler-case
                  (progn
                    (application-set-working-directory application "missing-directory")
                    nil)
                (working-directory-error (condition)
                  (eq (working-directory-error-stage condition) ':validation)))
              "invalid workspace changes report their validation stage")
             (test-assert
              (eq (application-configuration application) active-configuration)
              "invalid workspace changes retain the active configuration")
             (test-assert (equal (uiop:getcwd) (truename workspace))
                          "invalid workspace changes retain the process directory")))
      (when pool
        (lisp-worker-pool-stop-all pool))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-busy-conversation-resume () null)
(defun test-application-busy-conversation-resume ()
  "Test resume claims ownership before crash repair can mutate a transcript."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (current
           (conversation-create configuration :identifier "N8vQ2mp"))
         (target
           (conversation-create configuration :identifier "M8vQ2mp"))
         (call
           (json-object
            "type" "function_call"
            "status" "completed"
            "arguments" "{}"
            "call_id" "call-busy-resume"
            "name" "read"
            "namespace" "fs"))
         (current-lease nil)
         (application nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message target "inspect the file")
           (conversation-append-provider-item target call)
           (setf current-lease
                 (conversation-lease-acquire
                  configuration
                  (conversation-identifier current))
                 application
                 (make-instance
                  'application
                  :configuration configuration
                  :conversation current
                  :conversation-lease current-lease))
           (let ((before
                   (uiop:read-file-string
                    (conversation-pathname target))))
             (test-conversation--call-with-child-lease
              configuration
              (conversation-identifier target)
              (lambda ()
                (test-assert
                 (handler-case
                     (progn
                       (application-resume-conversation
                        application
                        (conversation-identifier target))
                       nil)
                   (conversation-in-use ()
                     t))
                 "resume refuses a conversation owned by another process")
                (test-assert
                 (string=
                  before
                  (uiop:read-file-string
                   (conversation-pathname target)))
                 "busy resume does not append interrupted-call repair")))))
      (when application
        (application-release-conversation-lease application))
      (when (and current-lease
                 (conversation-lease-held-p current-lease))
        (conversation-lease-release current-lease))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-fresh-conversation-lease-collision () null)
(defun test-application-fresh-conversation-lease-collision ()
  "Test fresh application ownership probes another seed after a lease race."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (timestamp 3992846378)
         (first-identifier
           (conversation-identifier-from-seed timestamp 0))
         (second-identifier
           (conversation-identifier-from-seed timestamp 1)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-conversation--call-with-child-lease
            configuration
            first-identifier
            (lambda ()
              (let ((*conversation-identifier-random-index-function*
                      (lambda (limit)
                        (declare (ignore limit))
                        0)))
                (multiple-value-bind (conversation lease acquired-p)
                    (application--conversation-create-owned
                     nil configuration :timestamp timestamp)
                  (unwind-protect
                       (test-assert
                        (and
                         acquired-p
                         (string=
                          (conversation-identifier conversation)
                          second-identifier)
                         (conversation-lease-matches-p
                          lease second-identifier)
                         (not
                          (probe-file
                           (conversation-pathname conversation))))
                        "fresh ownership skips a leased empty identifier")
                    (conversation-lease-release lease)))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-tool-runtime-lifecycle () null)
(defun test-application-tool-runtime-lifecycle ()
  "Test conversation switching replaces runtimes before checkpoint detachment."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (make-instance 'tool-registry))
         (runtime-identity (list ':application-runtime))
         (close-count 0)
         (detach-count 0))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration
                                       :identifier "runtime-first"))
                (second-conversation
                  (conversation-create configuration
                                       :identifier "runtime-second"))
                (provider (provider-create configuration))
                (tool
                  (make-instance
                   'tool-test-runtime-tool
                   :namespace "test"
                   :name "runtime"
                   :description "Exercise application runtime cleanup."
                   :parameters (tool-object-schema (json-object) nil)
                   :runtime-identity runtime-identity
                   :close-function (lambda () (incf close-count))
                   :detach-function (lambda () (incf detach-count))))
                (application nil))
           (tool-registry-register registry tool)
           (setf registry (task-augment-tool-registry registry))
           (setf application
                 (make-instance
                  'application
                  :configuration configuration
                  :conversation first-conversation
                  :provider provider
                  :tool-registry registry
                  :worker nil
                  :agent (agent-create :configuration configuration
                                       :provider provider
                                       :conversation first-conversation
                                       :tool-registry registry
                                       :worker nil)
                  :ui nil))
           (let ((old-registry registry)
                 (old-orchestrator
                   (application--task-orchestrator application)))
             (application-install-conversation application second-conversation)
             (let ((new-registry
                     (application-tool-registry application))
                   (new-orchestrator
                     (application--task-orchestrator application)))
               (test-assert
                (= close-count 1)
                "switching conversations closes retired background runtimes")
               (test-assert
                (and
                 (eq (application-conversation application)
                     second-conversation)
                 (not (eq new-registry old-registry))
                 (eq (agent-tool-registry
                      (application-agent application))
                     new-registry))
                "conversation switching installs a fresh registry and agent")
               (test-assert
                (and old-orchestrator
                     new-orchestrator
                     (not (eq old-orchestrator new-orchestrator))
                     (eq
                      (task-orchestrator-lifecycle-state old-orchestrator)
                      ':closed)
                     (eq
                      (task-orchestrator-lifecycle-state new-orchestrator)
                      ':open))
                "conversation switching retires and replaces task runtimes")
               (tool-registry-close-runtime-state new-registry)
               (setf (conversation-turn-state second-conversation)
                     "transient-provider-turn-state")
               (test-assert
                (conversation-lease-matches-p
                 (application-conversation-lease application)
                 (conversation-identifier second-conversation))
                "the active application owns its installed conversation")
               (checkpoint-detach-state application)
               (test-assert
                (zerop detach-count)
                "checkpoint detachment never revisits a retired registry")
               (test-assert
                (null (conversation-turn-state second-conversation))
                "checkpoint detachment removes transient provider turn state")
               (test-assert
                (and
                 (null (application-conversation-lease application))
                 (test-conversation--child-can-acquire-lease-p
                  configuration
                  (conversation-identifier second-conversation)))
                "checkpoint detachment excludes the conversation lease"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(define-condition application-test-runtime-retirement-interruption
    (serious-condition)
  ()
  (:documentation
   "A non-error serious condition injected at the runtime retirement boundary."))

(-> application-tests--replacement-registry
    (string function &key (:resume-function function))
    tool-registry)
(defun application-tests--replacement-registry
    (name close-function &key (resume-function (lambda () nil)))
  "Return one task-capable registry exposing marker tool NAME."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'tool-test-runtime-tool
      :namespace "transition"
      :name name
      :description (format nil "Runtime marker ~A." name)
      :parameters (tool-object-schema (json-object) nil)
      :runtime-identity (list ':runtime-transition name)
      :close-function close-function
      :resume-function resume-function
      :detach-function (lambda () nil)))
    (task-augment-tool-registry registry)))

(-> test-application-runtime-replacement-transactions () null)
(defun test-application-runtime-replacement-transactions ()
  "Test workspace and conversation switches prepare fresh runtimes atomically."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (workspace-one (merge-pathnames "workspace-one/" root))
         (workspace-two (merge-pathnames "workspace-two/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (old-close-count 0)
         (workspace-close-count 0)
         (conversation-close-count 0)
         (old-registry
           (application-tests--replacement-registry
            "old"
            (lambda () (incf old-close-count))))
         (first-conversation
           (conversation-create configuration
                                :identifier "runtime-transaction-first"))
         (second-conversation
           (conversation-create configuration
                                :identifier "runtime-transaction-second"))
         (third-conversation
           (conversation-create configuration
                                :identifier "runtime-transaction-third"))
         (provider (provider-create configuration))
         (application
           (make-instance
            'application
            :configuration configuration
            :conversation first-conversation
            :provider provider
            :tool-registry old-registry
            :worker nil
            :agent
            (agent-create :configuration configuration
                          :provider provider
                          :conversation first-conversation
                          :tool-registry old-registry
                          :worker nil)
            :ui nil))
         (owned-registries (list old-registry)))
    (ensure-directories-exist workspace-one)
    (ensure-directories-exist workspace-two)
    (application-connect-task-presentation application)
    (unwind-protect
         (progn
           (let* ((old-orchestrator
                    (application--task-orchestrator application))
                  (workspace-registry
                    (application-tests--replacement-registry
                     "workspace-new"
                     (lambda () (incf workspace-close-count)))))
             (push workspace-registry owned-registries)
             (test-call-with-function-replacements
              (list
               (list
                'application--create-tool-registry
                (lambda (configuration)
                  (declare (ignore configuration))
                  workspace-registry))
               (list
                'lisp-worker-manager-change-working-directory
                (lambda (manager configuration)
                  (declare (ignore manager configuration))
                  nil)))
              (lambda ()
                (application-set-working-directory
                 application workspace-one)))
             (let ((workspace-orchestrator
                     (application--task-orchestrator application)))
               (test-assert
                (and
                 (= old-close-count 1)
                 (eq (application-tool-registry application)
                     workspace-registry)
                 (eq
                  (agent-tool-registry (application-agent application))
                  workspace-registry)
                 (tool-registry-find
                  workspace-registry "transition" "workspace-new")
                 (null
                  (tool-registry-find
                   workspace-registry "transition" "old"))
                 (search
                  "workspace-new"
                  (json-encode
                   (tool-registry-provider-schemas workspace-registry))))
                "a workspace switch advertises only the fresh tool schema")
               (test-assert
                (and old-orchestrator
                     workspace-orchestrator
                     (not (eq old-orchestrator workspace-orchestrator))
                     (eq
                      (task-orchestrator-lifecycle-state old-orchestrator)
                      ':closed)
                     (eq
                      (task-orchestrator-lifecycle-state
                       workspace-orchestrator)
                      ':open))
                "a workspace switch replaces and restarts task runtimes")
               (let ((active-configuration
                       (application-configuration application))
                     (active-conversation
                       (application-conversation application))
                     (active-provider
                       (application-provider application))
                     (active-agent
                       (application-agent application))
                     (active-process-directory (uiop:getcwd)))
                 (test-call-with-function-replacements
                  (list
                   (list
                    'application--create-tool-registry
                    (lambda (configuration)
                      (declare (ignore configuration))
                      (error
                       'mcp-server-startup-error
                       :message "Required MCP server failed."
                       :server-name "required"
                       :required-p t
                       :cause nil))))
                  (lambda ()
                    (test-assert
                     (handler-case
                         (progn
                           (application-set-working-directory
                            application workspace-two)
                           nil)
                       (working-directory-error (condition)
                         (eq
                          (working-directory-error-stage condition)
                          ':tools)))
                     "required MCP failure aborts workspace preparation")))
                 (test-assert
                  (and
                   (eq (application-configuration application)
                       active-configuration)
                   (eq (application-conversation application)
                       active-conversation)
                   (eq (application-provider application)
                       active-provider)
                   (eq (application-tool-registry application)
                       workspace-registry)
                   (eq (application-agent application) active-agent)
                   (equal (uiop:getcwd) active-process-directory)
                   (zerop workspace-close-count)
                   (eq
                    (task-orchestrator-lifecycle-state
                     workspace-orchestrator)
                    ':open)
                   (tool-registry-find
                    workspace-registry "transition" "workspace-new"))
                  "failed workspace preparation leaves the old application live"))
             (let* ((conversation-registry
                      (application-tests--replacement-registry
                       "conversation-new"
                       (lambda () (incf conversation-close-count))))
                    (workspace-orchestrator
                      (application--task-orchestrator application)))
               (push conversation-registry owned-registries)
               (test-call-with-function-replacements
                (list
                 (list
                  'application--create-tool-registry
                  (lambda (configuration)
                    (declare (ignore configuration))
                    conversation-registry)))
                (lambda ()
                  (application-install-conversation
                   application second-conversation)))
               (let ((conversation-orchestrator
                       (application--task-orchestrator application)))
                 (test-assert
                  (and
                   (= workspace-close-count 1)
                   (eq (application-conversation application)
                       second-conversation)
                   (eq (application-tool-registry application)
                       conversation-registry)
                   (eq
                    (agent-tool-registry
                     (application-agent application))
                    conversation-registry)
                   (tool-registry-find
                    conversation-registry
                    "transition"
                    "conversation-new")
                   (null
                    (tool-registry-find
                     conversation-registry
                     "transition"
                     "workspace-new"))
                   (conversation-lease-matches-p
                    (application-conversation-lease application)
                    (conversation-identifier second-conversation))
                   (search
                    "conversation-new"
                    (json-encode
                     (tool-registry-provider-schemas
                      conversation-registry))))
                  "conversation installation advertises only fresh tool schemas")
                 (test-assert
                  (and
                   (eq
                    (task-orchestrator-lifecycle-state
                     workspace-orchestrator)
                    ':closed)
                   (eq
                    (task-orchestrator-lifecycle-state
                     conversation-orchestrator)
                    ':open)
                   (not
                    (eq workspace-orchestrator
                        conversation-orchestrator)))
                  "conversation installation replaces and restarts task runtimes")
                 (let ((active-configuration
                         (application-configuration application))
                       (active-provider
                         (application-provider application))
                       (active-registry
                         (application-tool-registry application))
                       (active-agent
                         (application-agent application))
                       (active-rendered-sequence
                         (application-rendered-sequence application))
                       (attempted-lease nil)
                       (lease-acquire-function
                         (symbol-function 'conversation-lease-acquire)))
                   (test-call-with-function-replacements
                    (list
                     (list
                      'conversation-lease-acquire
                      (lambda (replacement-configuration identifier)
                        (setf attempted-lease
                              (funcall
                               lease-acquire-function
                               replacement-configuration
                               identifier))))
                     (list
                      'application--create-tool-registry
                      (lambda (configuration)
                        (declare (ignore configuration))
                        (error
                         'mcp-server-startup-error
                         :message "Required MCP server failed."
                         :server-name "required"
                         :required-p t
                         :cause nil))))
                    (lambda ()
                      (test-assert
                       (handler-case
                           (progn
                             (application-install-conversation
                              application third-conversation)
                             nil)
                         (mcp-server-startup-error (condition)
                           (mcp-server-startup-error-required-p condition)))
                       "required MCP failure aborts conversation preparation")))
                   (test-assert
                    (and
                     (eq (application-configuration application)
                         active-configuration)
                     (eq (application-conversation application)
                         second-conversation)
                     (eq (application-provider application)
                         active-provider)
                     (eq (application-tool-registry application)
                         active-registry)
                     (eq (application-agent application) active-agent)
                     (= (application-rendered-sequence application)
                        active-rendered-sequence)
                     (conversation-lease-matches-p
                      (application-conversation-lease application)
                      (conversation-identifier second-conversation))
                     attempted-lease
                     (not (conversation-lease-held-p attempted-lease))
                     (zerop conversation-close-count)
                     (eq
                      (task-orchestrator-lifecycle-state
                       conversation-orchestrator)
                      ':open)
                     (tool-registry-find
                      conversation-registry
                      "transition"
                      "conversation-new"))
                    "failed conversation preparation leaves the old application live")))))))
      (ignore-errors
        (application-disconnect-task-presentation application))
      (application-release-conversation-lease application)
      (dolist (registry (remove-duplicates owned-registries :test #'eq))
        (ignore-errors
          (tool-registry-close-runtime-state registry)))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-runtime-retirement-failures () null)
(defun test-application-runtime-retirement-failures ()
  "Test replacement operations roll back a non-error serious retirement failure."
  (dolist (operation '(:mcp-reload :working-directory :conversation))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (workspace (merge-pathnames "retirement-workspace/" root))
           (previous-process-directory (uiop:getcwd))
           (previous-defaults *default-pathname-defaults*)
           (retirement-failure-enabled-p t)
           (old-registry
             (application-tests--replacement-registry
              "retirement-old"
              (lambda ()
                (when retirement-failure-enabled-p
                  (signal
                   'application-test-runtime-retirement-interruption)))))
           (new-close-count 0)
           (new-registry
             (application-tests--replacement-registry
              "retirement-new"
              (lambda ()
                (incf new-close-count))))
           (old-conversation
             (conversation-create
              configuration :identifier
              (format nil "runtime-retirement-old-~(~A~)" operation)))
           (new-conversation
             (conversation-create
              configuration :identifier
              (format nil "runtime-retirement-new-~(~A~)" operation)))
           (provider (provider-create configuration))
           (old-agent
             (agent-create :configuration configuration
                           :provider provider
                           :conversation old-conversation
                           :tool-registry old-registry
                           :worker nil))
           (application
             (make-instance
              'application
              :configuration configuration
              :conversation old-conversation
              :provider provider
              :tool-registry old-registry
              :worker nil
              :agent old-agent
              :ui nil))
           (old-orchestrator
             (application--task-orchestrator application))
           (new-orchestrator
             (task-run-tool-orchestrator
              (tool-registry-find new-registry "task" "run")))
           (mcp-registration-snapshot (mcp--registry-snapshot))
           (context-registration-snapshot (context--registry-snapshot))
           (command-registration-snapshot
             (application-command--registry-snapshot))
           (failure nil))
      (ensure-directories-exist workspace)
      (application-connect-task-presentation application)
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list
              'application--create-tool-registry
              (lambda (replacement-configuration)
                (declare (ignore replacement-configuration))
                new-registry))
             (list
              'mcp-configuration-load
              (lambda (replacement-configuration)
                (declare (ignore replacement-configuration))
                nil))
             (list
              'user-init-load
              (lambda (replacement-configuration)
                (declare (ignore replacement-configuration))
                nil)))
            (lambda ()
              (setf
               failure
               (handler-case
                   (progn
                     (ecase operation
                       (:mcp-reload
                        (application-reload-mcp application))
                       (:working-directory
                        (application-set-working-directory
                         application workspace))
                       (:conversation
                        (application-install-conversation
                         application new-conversation)))
                     nil)
                 (working-directory-error (condition)
                   (and
                    (eq operation ':working-directory)
                    (eq (working-directory-error-stage condition) ':tools)
                    (typep
                     (working-directory-error-cause condition)
                     'application-test-runtime-retirement-interruption)
                    condition))
                 (application-runtime-replacement-error (condition)
                   (and
                    (eq
                     (application-runtime-replacement-error-operation
                      condition)
                     operation)
                    (eq
                     (application-runtime-replacement-error-stage condition)
                     ':retire)
                    (typep
                     (application-runtime-replacement-error-cause condition)
                     'application-test-runtime-retirement-interruption)
                    condition))))
              (test-assert
               failure
               "runtime retirement reports its non-error serious failure")
              (test-assert
               (and
                (eq (application-configuration application) configuration)
                (eq (application-conversation application) old-conversation)
                (eq (application-provider application) provider)
                (eq (application-tool-registry application) old-registry)
                (eq (application-agent application) old-agent))
               "runtime retirement failure preserves application ownership")
              (test-assert
               (and
                (application-task-presentation-listener application)
                (eq
                 (task-orchestrator-lifecycle-state old-orchestrator)
                 ':open)
                (eq
                 (task-orchestrator-lifecycle-state new-orchestrator)
                 ':closed)
                (= new-close-count 1))
               "runtime retirement rollback resumes old tasks and closes new tasks")
              (test-assert
               (and
                (equal (uiop:getcwd) previous-process-directory)
                (equal *default-pathname-defaults* previous-defaults)
                (equal mcp-registration-snapshot
                       (mcp--registry-snapshot))
                (equal context-registration-snapshot
                       (context--registry-snapshot))
                (equal command-registration-snapshot
                       (application-command--registry-snapshot)))
               "runtime retirement rollback preserves process and extension state")))
        (setf retirement-failure-enabled-p nil)
        (ignore-errors
          (application-disconnect-task-presentation application))
        (ignore-errors
          (tool-registry-close-runtime-state old-registry))
        (ignore-errors
          (tool-registry-close-runtime-state new-registry))
        (uiop:chdir previous-process-directory)
        (setf *default-pathname-defaults* previous-defaults)
        (mcp--registry-restore mcp-registration-snapshot)
        (context--registry-restore context-registration-snapshot)
        (application-command--registry-restore
         command-registration-snapshot)
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist :ignore))))
  nil)

(-> test-application-create-unwind-safety () null)
(defun test-application-create-unwind-safety ()
  "Test late construction failure closes every newly owned runtime."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (original-registry-create
           (symbol-function 'application--create-tool-registry))
         (original-worker-create
           (symbol-function 'lisp-worker-pool-create))
         (original-registry-close
           (symbol-function 'tool-registry-close-runtime-state))
         (original-worker-stop
           (symbol-function 'lisp-worker-manager-stop))
         (original-goal-load
           (symbol-function 'application--load-goal))
         (mcp-registration-snapshot (mcp--registry-snapshot))
         (context-registration-snapshot (context--registry-snapshot))
         (command-registration-snapshot
           (application-command--registry-snapshot))
         (constructed-application nil)
         (created-registry nil)
         (created-worker nil)
         (registry-closed-p nil)
         (worker-stopped-p nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'image-state-load
            (lambda (configuration)
              (declare (ignore configuration))
              nil))
           (list
            'application--create-tool-registry
            (lambda (configuration)
              (setf created-registry
                    (funcall original-registry-create configuration))))
           (list
            'lisp-worker-pool-create
            (lambda (configuration)
              (setf created-worker
                    (funcall original-worker-create configuration))))
           (list
            'tool-registry-close-runtime-state
            (lambda (registry)
              (when (eq registry created-registry)
                (setf registry-closed-p t))
              (funcall original-registry-close registry)))
           (list
            'lisp-worker-manager-stop
            (lambda (worker)
              (when (eq worker created-worker)
                (setf worker-stopped-p t))
              (funcall original-worker-stop worker)))
           (list
            'application--load-goal
            (lambda (application)
              (setf constructed-application application)
              (funcall original-goal-load application)
              (error "Injected failure after application presentation."))))
          (lambda ()
            (test-assert
             (handler-case
                 (progn
                   (application-create configuration)
                   nil)
               (simple-error (condition)
                 (search "Injected failure" (format nil "~A" condition))))
             "application construction propagates a late initialization failure")
            (test-assert
             (and constructed-application
                  created-registry
                  created-worker
                  registry-closed-p
                  worker-stopped-p
                  (not
                   (conversation-lease-held-p
                    (application-conversation-lease
                     constructed-application))))
             "late construction failure closes its registry, worker, and lease")
            (test-assert
             (and
              (null
               (application-task-presentation-listener
                constructed-application))
              (eq
               (task-orchestrator-lifecycle-state
                (application--task-orchestrator constructed-application))
               ':closed))
             "late construction failure removes presentation and task runtimes")
            (test-assert
             (and
              (equal mcp-registration-snapshot (mcp--registry-snapshot))
              (equal context-registration-snapshot
                     (context--registry-snapshot))
              (equal command-registration-snapshot
                     (application-command--registry-snapshot)))
             "late construction failure restores declarative registrations")))
      (uiop:delete-directory-tree
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-reconnect-unwind-safety () null)
(defun test-application-reconnect-unwind-safety ()
  "Test reconnect preparation and runtime retirement rollback."
  (labels ((run-case (stage)
             "Exercise reconnect transaction STAGE with isolated resources."
             (let* ((configuration (test-configuration))
                    (root (test-configuration-root configuration))
                    (conversation
                      (conversation-create
                       configuration
                       :identifier
                       (format nil "reconnect-transaction-~(~A~)" stage)))
                    (provider (provider-create configuration))
                    (old-registry
                      (application--create-tool-registry configuration))
                    (old-worker (lisp-worker-pool-create configuration))
                    (old-agent
                      (agent-create :configuration configuration
                                    :provider provider
                                    :conversation conversation
                                    :tool-registry old-registry
                                    :worker old-worker))
                    (old-ui
                      (terminal-ui-create
                       :terminal
                       (make-instance 'recording-terminal :columns 72)))
                    (application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :provider provider
                                     :tool-registry old-registry
                                     :worker old-worker
                                     :agent old-agent
                                     :ui old-ui))
                    (old-orchestrator
                      (application--task-orchestrator application))
                    (original-context-reset
                      (symbol-function 'context-runtime-reset))
                    (original-registry-create
                      (symbol-function
                       'application--create-tool-registry))
                    (original-worker-create
                      (symbol-function 'lisp-worker-pool-create))
                    (original-registry-close
                      (symbol-function
                       'tool-registry-close-runtime-state))
                    (original-registry-quiesce
                      (symbol-function
                       'tool-registry-quiesce-runtime-state))
                    (original-registry-resume
                      (symbol-function
                       'tool-registry-resume-runtime-state))
                    (original-worker-stop
                      (symbol-function 'lisp-worker-manager-stop))
                    (mcp-registration-snapshot
                      (mcp--registry-snapshot))
                    (context-registration-snapshot
                      (context--registry-snapshot))
                    (command-registration-snapshot
                      (application-command--registry-snapshot))
                    (new-registry nil)
                    (new-worker nil)
                    (new-registry-closed-p nil)
                    (new-worker-stopped-p nil)
                    (old-worker-stopped-p nil)
                    (context-transition-p nil)
                    (image-transition-p nil)
                    (old-close-after-transitions-p nil)
                    (old-registry-resumed-p nil)
                    (returned-application nil)
                    (failure-p nil))
               (configuration-ensure-directories configuration)
               (application-connect-task-presentation application)
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list
                       'configuration-create
                       (lambda (&key source-root working-directory model
                                    reasoning-effort immutable-p)
                         (declare (ignore source-root))
                         (configuration--clone
                          configuration
                          :working-directory working-directory
                          :model model
                          :reasoning-effort reasoning-effort
                          :immutable-p immutable-p)))
                      (list
                       'application-terminal-ui-create
                       (lambda ()
                         (terminal-ui-create
                          :terminal
                          (make-instance
                           'recording-terminal
                           :columns 72))))
                      (list
                       'context-runtime-reset
                       (lambda ()
                         (setf context-transition-p t)
                         (funcall original-context-reset)))
                      (list
                       'image-state-reconnect
                       (lambda ()
                         (setf image-transition-p t)
                         (when (eq stage ':pre-commit)
                           (error
                            "Injected reconnect preparation failure."))
                         nil))
                      (list
                       'application--create-tool-registry
                       (lambda (new-configuration)
                         (setf new-registry
                               (funcall
                                original-registry-create
                                new-configuration))))
                      (list
                       'lisp-worker-pool-create
                       (lambda (new-configuration)
                         (setf new-worker
                               (funcall
                                original-worker-create
                                new-configuration))))
                      (list
                       'tool-registry-quiesce-runtime-state
                       (lambda (registry)
                         (if (eq registry old-registry)
                             (progn
                               (setf old-close-after-transitions-p
                                     (and (not context-transition-p)
                                          image-transition-p))
                               (multiple-value-bind
                                   (completed retirement-failure)
                                   (funcall original-registry-quiesce registry)
                                 (values
                                  completed
                                  (if (eq stage ':retirement)
                                      (make-condition
                                       'task-error
                                       :message
                                       "Injected reconnect retirement failure."
                                       :tool-name "task.run")
                                      retirement-failure))))
                             (funcall original-registry-quiesce registry))))
                      (list
                       'tool-registry-resume-runtime-state
                       (lambda (registry &key tools)
                         (when (eq registry old-registry)
                           (setf old-registry-resumed-p t))
                         (funcall original-registry-resume
                                  registry :tools tools)))
                      (list
                       'tool-registry-close-runtime-state
                       (lambda (registry)
                         (when (eq registry new-registry)
                           (setf new-registry-closed-p t))
                         (funcall original-registry-close registry)))
                      (list
                       'lisp-worker-manager-stop
                       (lambda (worker)
                         (when (eq worker new-worker)
                           (setf new-worker-stopped-p t))
                         (when (eq worker old-worker)
                           (setf old-worker-stopped-p t))
                         (funcall original-worker-stop worker))))
                     (lambda ()
                       (setf failure-p
                             (handler-case
                                 (progn
                                   (setf returned-application
                                         (application-reconnect application))
                                   nil)
                               (error (condition)
                                 (case stage
                                   (:pre-commit
                                    (search
                                     "Injected reconnect preparation"
                                     (format nil "~A" condition)))
                                   (:retirement
                                    (and
                                     (typep
                                      condition
                                      'application-runtime-replacement-error)
                                     (eq
                                      (application-runtime-replacement-error-operation
                                       condition)
                                      ':reconnect)
                                     (eq
                                      (application-runtime-replacement-error-stage
                                       condition)
                                      ':retire)
                                     (search
                                      "Injected reconnect retirement"
                                      (format
                                       nil
                                       "~A"
                                       (application-runtime-replacement-error-cause
                                        condition)))))))))))
                 (ecase stage
                   (:pre-commit
                    (test-assert
                     failure-p
                     "reconnect propagates a pre-commit preparation failure")
                    (test-assert
                     (and
                      (eq
                       (application-tool-registry application)
                       old-registry)
                      (eq (application-worker application) old-worker)
                      (eq (application-agent application) old-agent)
                      (eq
                       (task-orchestrator-lifecycle-state old-orchestrator)
                       ':open)
                      (application-task-presentation-listener application)
                      (not old-worker-stopped-p))
                     "pre-commit failure leaves the retained application live")
                    (test-assert
                     (and
                      new-registry
                      new-worker
                      new-registry-closed-p
                      new-worker-stopped-p)
                     "pre-commit failure closes replacement resources")
                    (test-assert
                     (and
                      (not context-transition-p)
                      image-transition-p
                      (not old-close-after-transitions-p)
                      (equal
                       mcp-registration-snapshot
                       (mcp--registry-snapshot))
                      (equal
                       context-registration-snapshot
                       (context--registry-snapshot))
                      (equal
                       command-registration-snapshot
                       (application-command--registry-snapshot)))
                     "pre-commit failure preserves transient context receipts"))
                   (:retirement
                    (let ((new-orchestrator
                            (task-run-tool-orchestrator
                             (tool-registry-find
                              new-registry "task" "run"))))
                      (test-assert
                       (and failure-p (null returned-application))
                       "runtime retirement failure aborts reconnect")
                      (test-assert
                       (and
                        (eq
                         (application-tool-registry application)
                         old-registry)
                        (eq
                         (application-worker application)
                         old-worker)
                        (eq
                         (application-agent application)
                         old-agent)
                        new-registry-closed-p
                        new-worker-stopped-p)
                      "retirement failure keeps old ownership and closes replacements")
                      (test-assert
                       (and
                        old-close-after-transitions-p
                        old-registry-resumed-p
                        (not old-worker-stopped-p)
                        (not context-transition-p)
                        image-transition-p
                        (application-task-presentation-listener application)
                        (eq
                         (task-orchestrator-lifecycle-state
                          old-orchestrator)
                         ':open)
                        (eq
                         (task-orchestrator-lifecycle-state
                          new-orchestrator)
                         ':closed)
                        (equal
                         mcp-registration-snapshot
                         (mcp--registry-snapshot))
                        (equal
                         context-registration-snapshot
                         (context--registry-snapshot))
                        (equal
                         command-registration-snapshot
                         (application-command--registry-snapshot)))
                       "retirement rollback resumes the old runtime and registrations"))))
              (ignore-errors
                (application-disconnect-task-presentation application))
              (when returned-application
                (ignore-errors
                  (application-disconnect-task-presentation
                   returned-application)))
              (dolist (registry
                       (remove-duplicates
                        (remove nil (list old-registry new-registry))
                        :test #'eq))
                (ignore-errors
                  (funcall original-registry-close registry)))
              (dolist (worker
                       (remove-duplicates
                        (remove nil (list old-worker new-worker))
                        :test #'eq))
                (ignore-errors
                  (funcall original-worker-stop worker)))
              (mcp--registry-restore mcp-registration-snapshot)
              (context--registry-restore context-registration-snapshot)
              (application-command--registry-restore
               command-registration-snapshot)
              (uiop:delete-directory-tree
               root :validate t :if-does-not-exist :ignore)))))
    (run-case ':pre-commit)
    (run-case ':retirement))
  nil)

(-> test-application-task-presentation () null)
(defun test-application-task-presentation ()
  "Test task events drive one reconnectable child-agent terminal projection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry
            (make-default-tool-registry)))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (ui
           (terminal-ui-create
            :terminal (make-instance 'recording-terminal :columns 72)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :tool-registry registry
                          :ui ui))
         (primary
           (task-tests--primary-agent
            configuration "task-presentation-primary" registry))
         (definition
           (task-agent-definition-create
            :name "presenter"
            :description "Exercise task presentation events."
            :instructions "Remain visible while the test inspects the UI."
            :source ':test))
         (queued nil)
         (running nil))
    (unwind-protect
         (progn
           (application-connect-task-presentation application)
           (application-connect-task-presentation application)
           (test-assert
            (= (with-lock-held ((task-orchestrator-lock orchestrator))
                 (length (task-orchestrator-listeners orchestrator)))
               1)
            "reconnection retains exactly one task presentation listener")
           (setf queued
                 (task-tests--register-job
                  orchestrator primary definition :name "queued-agent")
                 running
                 (task-tests--register-job
                  orchestrator primary definition :name "running-agent"))
           (with-lock-held ((task-job-lock running))
             (setf (task-job-state running) ':running)
             (task-job--set-progress-state running ':running))
           (task-progress-note-status
            running ':tool-call-started
            (list :tool "search.content"))
           (let ((activities (terminal-ui-agent-activities ui)))
             (test-assert
              (and (= (length activities) 2)
                   (eq (getf (first activities) :state) ':queued)
                   (eq (getf (second activities) :state) ':running)
                   (string= (getf (second activities) :current-tool)
                            "search.content"))
              "one progress event projects every queued and running child"))
           (task-job--publish-terminal
            queued
            ':completed
            (task-tests--terminal-result
             queued :status ':success :output "queued complete"))
           (test-assert
            (equal
             (mapcar (lambda (activity)
                       (getf activity :id))
                     (terminal-ui-agent-activities ui))
             (list (getf (task-job-identity running) :id)))
            "terminal lifecycle events remove only their finished child")
           (task-job--publish-terminal
            running
            ':completed
            (task-tests--terminal-result
             running :status ':success :output "running complete"))
           (task-orchestrator-emit
            orchestrator
            ':task-subagent-progress
            (list :id (getf (task-job-identity running) :id)))
           (test-assert
            (null (terminal-ui-agent-activities ui))
            "late progress cannot resurrect a terminal child")
           (application-disconnect-task-presentation application)
           (test-assert
            (and
             (null (application-task-presentation-orchestrator application))
             (null (application-task-presentation-listener application))
             (zerop
              (with-lock-held ((task-orchestrator-lock orchestrator))
                (length (task-orchestrator-listeners orchestrator)))))
            "disconnect removes the exact observer and retained scheduler link"))
      (application-disconnect-task-presentation application)
      (dolist (job (remove nil (list queued running)))
        (unless (task-job-terminal-p job)
          (task-job--publish-terminal
           job
           ':aborted
           (task-tests--terminal-result
            job :status ':aborted :output "test cleanup"))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-working-directory-command () null)
(defun test-working-directory-command ()
  "Test /cwd completion, full-path parsing, presentation, and no-argument status."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (workspace (merge-pathnames "command workspace/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (registry (make-default-tool-registry)))
    (ensure-directories-exist workspace)
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "cwd-command"))
                (provider (provider-create configuration))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker nil))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :provider provider
                                 :tool-registry registry
                                 :worker nil
                                 :agent agent
                                 :ui ui)))
           (terminal-ui-start ui)
           (let ((entry
                   (application-command-completion-entry
                    (application-command-find "/cwd"))))
             (test-assert
              (and entry
                   (string= (terminal-completion-label entry) "/cwd PATH"))
              "the command table offers /cwd with its path argument")
             (test-assert (search "/cwd PATH" (application-help))
                          "the command reference includes /cwd"))
           (test-assert
            (string= (application--command-remainder
                      "/cwd directory name with spaces")
                     "directory name with spaces")
            "slash-command remainders retain embedded spaces")
           (test-assert
            (eq (application-command
                 application
                 (format nil "/cwd ~A" (namestring workspace)))
                ':continue)
            "/cwd continues the application loop")
           (test-assert
            (equal (configuration-working-directory
                    (application-configuration application))
                   (truename workspace))
            "/cwd passes the complete path to workspace switching")
           (test-assert
            (search (format nil "Working directory is now ~A"
                            (namestring (truename workspace)))
                    (recording-terminal-output terminal))
            "/cwd presents the selected workspace")
           (application-command application "/cwd")
           (test-assert
            (search (format nil "Working directory: ~A"
                            (namestring (truename workspace)))
                    (recording-terminal-output terminal))
            "/cwd without a path presents the current workspace"))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-effort-switch () null)
(defun test-effort-switch ()
  "Test reasoning effort picker items and in-place configuration switching."
  (let* ((base (test-configuration))
         (configuration
           (make-instance
            'configuration
            :source-root (configuration-source-root base)
            :working-directory (configuration-working-directory base)
            :data-root (configuration-data-root base)
            :state-root (configuration-state-root base)
            :cache-root (configuration-cache-root base)
            :config-root (configuration-config-root base)
            :codex-auth-path (configuration-codex-auth-path base)
            :model (configuration-model base)
            :reasoning-effort (configuration-reasoning-effort base)
            :web-search-mode "live"
            :provider-endpoint "https://provider.test/responses"))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "effort"))
                (provider (provider-create configuration))
                (registry (make-default-tool-registry))
                (worker (lisp-worker-create configuration))
                (agent (agent-create :configuration configuration
                                     :provider provider
                                     :conversation conversation
                                     :tool-registry registry
                                     :worker worker))
                (terminal (make-instance 'scripted-terminal :columns 60))
                (ui (terminal-ui-create :terminal terminal))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation conversation
                                 :provider provider
                                 :tool-registry registry
                                 :worker worker
                                 :agent agent
                                 :ui ui)))
           (setf (provider-rate-limits provider) '(:primary (:used-percent 25)))
           (let ((items (application--effort-items application)))
             (test-assert (= (length items)
                             (length *supported-reasoning-efforts*))
                          "every supported effort is offered")
             (test-assert (find "current" items
                                :key (lambda (item)
                                       (getf item :description))
                                :test #'string=)
                          "the active effort is marked current"))
           (application-set-reasoning-effort application "low")
           (test-assert (string= (configuration-reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "switching effort replaces the configuration")
           (test-assert
            (string= (conversation-reasoning-effort conversation) "low")
            "switching effort updates the active conversation")
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (string= (preference-state-reasoning-effort preferences) "low")
              "switching effort saves the global effort default")
             (test-assert
              (string= (preference-state-model preferences) "gpt-5.6-sol")
              "switching effort saves the accompanying model default"))
           (let ((updated (application-configuration application)))
             (test-assert (equal (configuration-source-root updated)
                                 (configuration-source-root configuration))
                          "effort switching preserves the source root")
             (test-assert (equal (configuration-state-root updated)
                                 (configuration-state-root configuration))
                          "effort switching preserves private state paths")
             (test-assert (string= (configuration-provider-endpoint updated)
                                   "https://provider.test/responses")
                          "effort switching preserves the provider endpoint")
             (test-assert (string= (configuration-web-search-mode updated) "live")
                          "effort switching preserves hosted web search mode"))
           (test-assert
            (string= (provider-session-id (application-provider application))
                     (provider-session-id provider))
            "effort switching preserves the provider session identity")
           (test-assert (equal (provider-rate-limits
                                (application-provider application))
                               '(:primary (:used-percent 25)))
                        "effort switching preserves the latest rate snapshot")
           (test-assert (typep (application-agent application) 'agent)
                        "switching effort reconnects the agent")
           (let ((items (application--model-items application)))
             (test-assert (= (length items) (length *supported-models*))
                          "every 5.6 family model is offered")
             (test-assert (string= (getf (find "current" items
                                               :key (lambda (item)
                                                      (getf item :description))
                                               :test #'string=)
                                         :name)
                                   "gpt-5.6-sol")
                          "the active model is marked current"))
           (application-set-model application "gpt-5.6-terra")
           (test-assert (string= (configuration-model
                                  (application-configuration application))
                                 "gpt-5.6-terra")
                        "switching the model replaces the configuration")
           (test-assert (string= (configuration-reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "model switching preserves the reasoning effort")
           (test-assert (string= (conversation-model conversation)
                                "gpt-5.6-terra")
                        "model switching updates the active conversation")
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (string= (preference-state-model preferences) "gpt-5.6-terra")
              "switching models saves the global model default")
             (test-assert
              (string= (preference-state-reasoning-effort preferences) "low")
              "switching models preserves the global effort default"))
           (setf (scripted-terminal-events terminal)
                 (list :history-next :history-next :submit))
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (test-assert
              (eq (application-command application "/model gpt-5.6-luna")
                  ':continue)
              "an explicit model change continues after choosing its effort"))
           (test-assert
            (and (string= (configuration-model
                           (application-configuration application))
                          "gpt-5.6-luna")
                 (string= (configuration-reasoning-effort
                           (application-configuration application))
                          "medium"))
            "model commands apply the prompted reasoning effort atomically")
           (test-assert
            (and (string= (conversation-model conversation) "gpt-5.6-luna")
                 (string= (conversation-reasoning-effort conversation) "medium"))
            "model commands persist both choices in the active conversation")
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (and (string= (preference-state-model preferences)
                            "gpt-5.6-luna")
                   (string= (preference-state-reasoning-effort preferences)
                            "medium"))
              "model commands persist both choices as global defaults"))
           (test-assert
            (search "The model is now gpt-5.6-luna with reasoning effort medium."
                    (recording-terminal-output terminal))
            "model commands report the complete selection")
           (test-assert (handler-case
                            (progn
                              (application-set-model application "gpt-4")
                              nil)
                          (configuration-error ()
                            t))
                        "unsupported models are rejected with the choices")
           (conversation-append-user-message conversation "persist this choice")
           (let* ((resumed-configuration
                    (configuration-with-reasoning-effort
                     (configuration-with-model configuration "gpt-5.6-luna")
                     "xhigh"))
                  (resumed (conversation-create resumed-configuration
                                                :identifier "resumed-model")))
             (conversation-append-user-message resumed "use the saved choice")
             (application-install-conversation
              application
              (conversation-load-by-id configuration "resumed-model"))
             (test-assert
              (string= (configuration-model
                        (application-configuration application))
                       "gpt-5.6-luna")
              "resuming restores the conversation model")
             (test-assert
              (string= (configuration-reasoning-effort
                        (application-configuration application))
                       "xhigh")
              "resuming restores the conversation effort")
             (test-assert
              (string= (configuration-model
                        (provider-configuration
                         (application-provider application)))
                       "gpt-5.6-luna")
              "the reconnected provider uses the conversation model"))
           (let ((preferences (preferences-load configuration)))
             (test-assert
              (string= (preference-state-model preferences) "gpt-5.6-luna")
              "resuming does not replace the global model default")
             (test-assert
              (string= (preference-state-reasoning-effort preferences) "medium")
              "resuming does not replace the global effort default")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-status-entry () null)
(defun test-status-entry ()
  "Test /status token accounting and rate limit presentation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "status"))
                (provider (provider-create configuration))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :provider provider
                                            :ui (terminal-ui-create
                                                 :terminal (make-instance
                                                            'recording-terminal
                                                            :columns 80)))))
           (test-assert (search "No rate limit data yet"
                                (markdown-tests--row-text
                                 (application-status-entry application)))
                        "status explains missing rate limit data")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "one"
                  :usage '(("input_tokens" 1000)
                           ("output_tokens" 500)
                           ("total_tokens" 1500))))
           (conversation-append-provider-metadata
            conversation
            (list :request-number 2
                  :response-id "two"
                  :usage '(("input_tokens" 2000)
                           ("output_tokens" 300)
                           ("total_tokens" 2300))))
           (setf (provider-rate-limits provider)
                 (list :captured-at (get-universal-time)
                       :primary (list :used-percent 28
                                      :window-minutes 300
                                      :resets-at nil)
                       :secondary (list :used-percent 45.5
                                        :window-minutes 10080
                                        :resets-at nil)))
           (let ((text (markdown-tests--row-text
                        (application-status-entry application))))
             (test-assert (search "3.8K total (3.0K input + 800 output)" text)
                          "status sums token usage across requests")
             (test-assert (search "5h limit" text)
                          "the primary window is named by its duration")
             (test-assert (search "weekly limit" text)
                          "the secondary window is named by its duration")
             (test-assert (search "72% left" text)
                          "status reports the remaining primary percentage")
             (test-assert (search "█" text)
                          "status draws usage bars")
             (test-assert (search "standard" text)
                          "status names the standard service path")
             (test-assert (and (search "reasoning trace" text)
                               (search "hidden" text))
                          "status reports the reasoning-summary display mode")
             (test-assert (search "compacts at 80%" text)
                          "status reports the compaction threshold")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-session-goal () null)
(defun test-session-goal ()
  "Test goal persistence, context injection, continuation, and completion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "goal"))
                (terminal (make-instance 'recording-terminal :columns 60))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :tool-registry
                                            (make-default-tool-registry)
                                            :worker nil
                                            :ui (terminal-ui-create
                                                 :terminal terminal))))
           (terminal-ui-start (application-ui application))
           (application-goal-command application "update nothing yet")
           (test-assert (null (application-goal application))
                        "updating without a goal leaves none set")
           (application-goal-command application "polish the terminal")
           (test-assert (eq (getf (application-goal application) :status)
                            ':active)
                        "setting a goal activates it")
           (let ((context (application-goal-context application)))
             (test-assert (search "polish the terminal" context)
                          "the goal context carries the objective")
             (test-assert (search "[GOAL-COMPLETE]" context)
                          "the goal context teaches the completion marker"))
           (application-goal-command application "update polish the prompt")
           (test-assert (string= (getf (application-goal application)
                                       :objective)
                                 "polish the prompt")
                        "updating rewrites the objective in place")
           (test-assert (eq (getf (application-goal application) :status)
                            ':active)
                        "updating keeps an active goal active")
           (application-goal-command application "update")
           (test-assert (string= (getf (application-goal application)
                                       :objective)
                                 "polish the prompt")
                        "an empty update leaves the objective unchanged")
           (application-goal-command application "update /status")
           (test-assert (string= (getf (application-goal application)
                                       :objective)
                                 "polish the prompt")
                        "command-shaped update objectives are rejected")
           (let ((sibling (make-instance
                           'application
                           :configuration configuration
                           :conversation (conversation-load-by-id configuration
                                                                  "goal")
                           :ui (terminal-ui-create
                                :terminal (make-instance 'recording-terminal
                                                         :columns 60)))))
             (application--load-goal sibling)
             (test-assert (string= (getf (application-goal sibling) :objective)
                                   "polish the prompt")
                          "updated goals reload from durable records"))
           (application-goal-command application "pause")
           (test-assert (null (application-goal-context application))
                        "paused goals inject no context")
           (application-goal-command application "update polish it quietly")
           (test-assert (eq (getf (application-goal application) :status)
                            ':paused)
                        "updating keeps a paused goal paused")
           (let* ((completion-item
                    (json-object
                     "type" "message"
                     "role" "assistant"
                     "content" (json-array
                                (json-object
                                 "type" "output_text"
                                 "text" "All polished. [GOAL-COMPLETE]"))))
                  (working-item
                    (json-object
                     "type" "message"
                     "role" "assistant"
                     "content" (json-array
                                (json-object "type" "output_text"
                                             "text" "Still working."))))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results (list (agent-test-result "goal-1"
                                                       (list working-item)
                                                       :turn-completion :end)
                                    (agent-test-result "goal-2"
                                                       (list completion-item)
                                                       :turn-completion :end))))
                  (agent (agent-create :configuration configuration
                                       :provider provider
                                       :conversation conversation
                                       :tool-registry
                                       (application-tool-registry application)
                                       :worker nil)))
             (setf (application-provider application) provider
                   (application-agent application) agent)
             (application-goal-command application "resume")
             (test-assert (eq (getf (application-goal application) :status)
                              ':complete)
                          "the continuation loop stops at the marker")
             (test-assert (every #'non-empty-string-p
                                 (scripted-provider-goal-contexts provider))
                          "active goals ride along every provider request")
             (test-assert (search "✓ goal complete"
                                  (recording-terminal-output terminal))
                          "completing a goal presents a notice")
             (application-goal-command application "update polish once more")
             (test-assert (eq (getf (application-goal application) :status)
                              ':active)
                          "updating a completed goal reactivates it")
             (test-assert (zerop (getf (application-goal application)
                                       :continuations))
                          "updating restarts the continuation budget"))
           (setf (application-goal application)
                 (list :objective "endless"
                       :status ':active
                       :continuations *application-goal-continuation-limit*
                       :created-at (get-universal-time)))
           (recording-terminal-reset terminal)
           (application--run-goal-continuations application)
           (test-assert (eq (getf (application-goal application) :status)
                            ':paused)
                        "the continuation limit pauses the goal")
           (test-assert (search "paused after"
                                (recording-terminal-output terminal))
                        "pausing explains the continuation budget")
           (test-assert (equal (conversation-record-entry
                                application
                                (list :message :seq 99 :time 0 :role :user
                                      :content
                                      *application-goal-continuation-prompt*))
                               (list (terminal-span :hint "∙ goal continues")))
                        "continuation prompts render as dim notices")
           (application-goal-command application "clear")
           (test-assert (null (application-goal application))
                        "clearing removes the goal")
           (application-goal-command application "/status")
           (test-assert (null (application-goal application))
                        "command-shaped objectives are rejected")
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-later-scheduler () null)
(defun test-later-scheduler ()
  "Test /later scheduling, listing, cancellation, and due-work promotion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (provider-create configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :provider provider
                                     :ui ui))
         (controller nil))
    (unwind-protect
         (with-terminal-ui (active-ui ui)
           (declare (ignore active-ui))
           (setf controller (application-input-controller-create application)
                 (provider-rate-limits provider)
                 (list :primary
                       (list :used-percent 100
                             :window-minutes 300
                             :resets-at (+ (get-universal-time) 100))))
           (application-later-command application "prepare the release")
           (let* ((entry
                    (first
                     (later-state-entries
                      (application-input-controller-later-state controller))))
                  (identifier (later-entry-identifier entry)))
             (test-assert (and entry
                               (string= (later-entry-input entry)
                                        "prepare the release"))
                          "/later schedules its complete input durably")
             (test-assert (search "prepare the release"
                                  (application--later-list application))
                          "/later without input lists scheduled previews")
             (application-later-command application
                                        (format nil "cancel ~A" identifier))
             (test-assert
              (null (later-state-entries
                     (application-input-controller-later-state controller)))
              "/later cancel removes the exact scheduled input"))
           (let ((entry
                   (application-input-controller-schedule-later
                    controller
                    "due now"
                    :due-at (1- (get-universal-time))
                    :window "test")))
             (let ((work (application-input-controller--next-work controller)))
               (test-assert (and (eq (first work) ':later)
                                 (eq (second work) entry))
                            "due deferred inputs enter the ordinary work queue"))
             (application-input-controller--complete-later controller entry)
             (application-input-controller--finish-work controller)
             (test-assert
              (null (later-state-entries (later-load configuration)))
              "successful deferred dispatch removes durable state")))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-application-command-tips)
  (test-application-banner-version)
  (test-startup-update-choice)
  (test-thinking-label-selection)
  (test-application-status-details)
  (test-reasoning-trace-command)
  (test-compact-view-command)
  (test-command-permission-modes)
  (test-interrupt-resume-instruction)
  (test-repeated-interrupt-forces-exit)
  (test-graceful-shutdown-retains-interrupt-escape)
  (test-transcript-entries)
  (test-task-run-call-presentation)
  (test-recovery-cursor-normalization)
  (test-bounded-transcript-replay)
  (test-hidden-reasoning-does-not-crowd-replay)
  (test-paged-transcript-history)
  (test-streaming-presentation)
  (test-provider-retry-presentation)
  (test-turn-cursor-visibility)
  (test-responsive-model-input)
  (test-responsive-goal-inspection)
  (test-input-reader-quiescence)
  (test-late-steering-promotion)
  (test-conversation-picker)
  (test-working-directory-switch)
  (test-application-busy-conversation-resume)
  (test-application-fresh-conversation-lease-collision)
  (test-application-tool-runtime-lifecycle)
  (test-application-runtime-replacement-transactions)
  (test-application-runtime-retirement-failures)
  (test-application-create-unwind-safety)
  (test-application-reconnect-unwind-safety)
  (test-application-task-presentation)
  (test-working-directory-command)
  (test-effort-switch)
  (test-status-entry)
  (test-session-goal)
  (test-later-scheduler)
  t)
