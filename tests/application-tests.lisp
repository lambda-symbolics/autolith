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

;;;; -- Active-Turn Cancellation Test Support --

(defclass application-test-gated-tool (tool)
  ((lock
    :initform (make-lock "Autolith gated tool")
    :reader application-test-gated-tool-lock
    :type t
    :documentation "The lock protecting deterministic tool execution timing.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith gated tool")
    :reader application-test-gated-tool-condition-variable
    :type t
    :documentation "The wait point for gated tool execution.")
   (entered-p
    :initform nil
    :accessor application-test-gated-tool-entered-p
    :type boolean
    :documentation "Whether the tool reached its cancellable execution point.")
   (released-p
    :initform nil
    :accessor application-test-gated-tool-released-p
    :type boolean
    :documentation "Whether a fallback cleanup may complete the tool.")
   (completed-p
    :initform nil
    :accessor application-test-gated-tool--completed-p
    :type boolean
    :documentation "Whether the tool reached its normal completion point."))
  (:documentation "A deterministic tool that waits until cancelled or released."))

(-> application-test-gated-tool-wait-until-entered
    (application-test-gated-tool real)
    boolean)
(defun application-test-gated-tool-wait-until-entered (tool timeout)
  "Wait up to TIMEOUT for TOOL execution to reach its cancellation gate."
  (task-tests--wait-until
   (lambda ()
     (with-lock-held ((application-test-gated-tool-lock tool))
       (application-test-gated-tool-entered-p tool)))
   timeout))

(-> application-test-gated-tool-release (application-test-gated-tool) null)
(defun application-test-gated-tool-release (tool)
  "Allow TOOL to complete when test cleanup needs to release its gate."
  (with-lock-held ((application-test-gated-tool-lock tool))
    (setf (application-test-gated-tool-released-p tool) t)
    (condition-notify (application-test-gated-tool-condition-variable tool)))
  nil)

(-> application-test-gated-tool-completed-p (application-test-gated-tool) boolean)
(defun application-test-gated-tool-completed-p (tool)
  "Return whether TOOL reached normal completion."
  (not
   (null
    (with-lock-held ((application-test-gated-tool-lock tool))
      (application-test-gated-tool--completed-p tool)))))

(defmethod tool-execute ((tool application-test-gated-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Wait at TOOL's gate, then report completion only after explicit release."
  (declare (ignore context arguments))
  (with-lock-held ((application-test-gated-tool-lock tool))
    (setf (application-test-gated-tool-entered-p tool) t)
    (condition-notify (application-test-gated-tool-condition-variable tool))
    (loop until (application-test-gated-tool-released-p tool)
          do (condition-wait
              (application-test-gated-tool-condition-variable tool)
              (application-test-gated-tool-lock tool)))
    (setf (application-test-gated-tool--completed-p tool) t))
  (tool-success "gated tool completed"))

(defclass application-test-counting-provider (scripted-provider)
  ((lock
    :initform (make-lock "Autolith counting provider")
    :reader application-test-counting-provider-lock
    :type t
    :documentation "The lock protecting the completed request count.")
   (request-count
    :initform 0
    :accessor application-test-counting-provider-request-count
    :type (integer 0)
    :documentation "The number of provider requests that returned a result."))
  (:documentation "A scripted provider exposing a synchronized request count."))

(defmethod provider-stream-turn :around
    ((provider application-test-counting-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Count PROVIDER requests only after their scripted result is available."
  (declare (ignore conversation tool-namespaces event-callback
                   goal-context compaction-p))
  (let ((result (call-next-method)))
    (with-lock-held ((application-test-counting-provider-lock provider))
      (incf (application-test-counting-provider-request-count provider)))
    result))

(-> application-test-counting-provider-wait-for-requests
    (application-test-counting-provider (integer 0) real)
    boolean)
(defun application-test-counting-provider-wait-for-requests
    (provider count timeout)
  "Wait up to TIMEOUT for PROVIDER to return at least COUNT scripted results."
  (task-tests--wait-until
   (lambda ()
     (with-lock-held ((application-test-counting-provider-lock provider))
       (>= (application-test-counting-provider-request-count provider) count)))
   timeout))

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

(-> application-tests--conversation-records (conversation) list)
(defun application-tests--conversation-records (conversation)
  "Return every durable non-header record in CONVERSATION."
  (let ((records nil))
    (conversation-map-records
     conversation
     (lambda (record)
       (push record records)))
    (nreverse records)))

(-> responsive-scripted-terminal--answer-count
    (responsive-scripted-terminal)
    (integer 0))
(defun responsive-scripted-terminal--answer-count (terminal)
  "Return the number of durable provider items visible to TERMINAL."
  (count ':provider-item
         (application-tests--conversation-records
          (responsive-scripted-terminal-conversation terminal))
         :key #'first))

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
                 "the canonical Lisp call uses the colored code style")
    (test-assert
     (string= (terminal-span-text command-span)
              (format nil
                      "(~A)"
                      (application-operation--command-name entry)))
     "the colored span contains the canonical parenthesized command")
    (test-assert (string= (terminal-span-text tip-span)
                          (format nil " ~A" (application-command-tip entry)))
                 "the command's mandatory tip follows its colored token"))
  nil)

(-> test-application-command-presentations () null)
(defun test-application-command-presentations ()
  "Test command feedback is visibly labeled without affecting later notices."
  (let* ((invocation
           (application-command-invocation-parse "/timestamps on"))
         (message "Turn timestamps are enabled and saved.")
         (entry
           (application--command-presentation-entry invocation message)))
    (test-assert
     (equal entry
            (list (terminal-span ':notice "› ")
                  (terminal-span ':code "/timestamps on")
                  (terminal-span ':plain (string #\Newline))
                  (terminal-span ':notice "└ ")
                  (terminal-span ':plain message)))
     "command feedback carries a colored command heading and marked notice"))
  (let* ((application (application-tests--ui-application :columns 60))
         (ui (application-ui application))
         (terminal (terminal-ui-terminal ui))
         (invocation
           (application-command-invocation-parse "/timestamps on")))
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (application-command--call-with-presentation
            invocation
            (lambda ()
              (application-present application "first confirmation")
              (application-present application "second confirmation")))
           (let* ((output (recording-terminal-output terminal))
                  (command-position (search "/timestamps on" output))
                  (confirmation-position (search "first confirmation" output)))
             (test-assert
              (and command-position
                   confirmation-position
                   (< command-position confirmation-position))
              "the originating command is shown above its first confirmation")
             (test-assert
              (= (terminal-tests--substring-count "/timestamps on" output) 1)
              "one command invocation receives one heading"))
           (recording-terminal-reset terminal)
           (application-present application "unrelated notice")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "unrelated notice" output)
                   (not (search "/timestamps on" output)))
              "later non-command presentations do not inherit command context")))
      (terminal-ui-stop ui)))
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
                       (find
                        (terminal-span-text tip-command-span)
                        (application-command-list)
                        :test #'string=
                        :key
                        (lambda (entry)
                          (format nil
                                  "(~A)"
                                  (application-operation--command-name entry))))))
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
                                  :if-does-not-exist ':ignore)))
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
                                    :if-does-not-exist ':ignore))))
  nil)


(-> test-explicit-update-operation () null)
(defun test-explicit-update-operation ()
  "Test fresh installed-release updates and nonmutating external update advice."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (make-instance 'application :configuration configuration))
         (presented nil)
         (fetches 0))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'application-present
                 (lambda (observed text)
                   (test-assert (eq observed application)
                                "explicit update output belongs to its application")
                   (push text presented)
                   nil)))
          (lambda ()
            (let ((*update-check-fetch-function*
                    (lambda ()
                      (incf fetches)
                      (format nil "v~A" *autolith-version*))))
              (dolist (case '((:source "running from source")
                              (:nix "installed through Nix")))
                (destructuring-bind (method expected) case
                  (setf (application-installation-provenance application)
                        (make-instance 'installation-provenance :method method)
                        presented nil)
                  (application-update application)
                  (test-assert (search expected (first presented))
                               "external installations receive their update advice")
                  (test-assert (zerop fetches)
                               "external installations never query the release service")))
              (setf (application-installation-provenance application)
                    (make-instance
                     'installation-provenance
                     :method ':release
                     :current-tag (format nil "v~A" *autolith-version*))
                    (application-update-availability application)
                    (make-instance 'update-availability
                                   :tag "v99.0.0"
                                   :method ':release)
                    presented nil)
              (application-update application)
              (test-assert (= fetches 1)
                           "an explicit packaged update performs one fresh check")
              (test-assert
               (and (search "already the newest" (first presented))
                    (null (application-update-availability application)))
               "a current packaged release reports success and clears stale notice state"))
            (setf presented nil)
            (let ((*update-check-fetch-function*
                    (lambda () (error "offline"))))
              (application-update application)
              (test-assert
               (search "could not check the release service" (first presented))
               "a failed explicit check is nonfatal and reports no installation change"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-info-command () null)
(defun test-application-info-command ()
  "Test the read-only current-settings display and callable command."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (application-tests--ui-application :columns 80))
         (ui (application-ui application))
         (terminal (terminal-ui-terminal ui)))
    (setf (application-configuration application) configuration
          (application-reasoning-traces-p application) t
          (application-turn-timestamps-p application) t
          (application-compact-view-p application) nil
          (application-hurry-up-p application) t
          (application-permission-mode application) ':sandboxed)
    (unwind-protect
         (progn
           (preferences-set-simple-technical-english configuration t)
           (terminal-ui-start ui)
           (recording-terminal-reset terminal)
           (labels ((settings-state ()
                      "Return every live setting displayed by INFO."
                      (let ((current (application-configuration application)))
                        (list
                         :model (configuration-model current)
                         :effort (configuration-reasoning-effort current)
                         :web-search (configuration-web-search-mode current)
                         :trace (application-reasoning-traces-p application)
                         :timestamps (application-turn-timestamps-p application)
                         :compact-view (application-compact-view-p application)
                         :ste (preferences-simple-technical-english-p current)
                         :hurry-up (application-hurry-up-p application)
                         :permissions (application-permission-mode application)))))
             (let ((before (settings-state)))
               (test-assert
                (null (application-operation-call application "info"))
                "the callable info command completes without a loop action")
               (let* ((text (recording-terminal-output terminal))
                      (lines
                        (uiop:split-string text :separator '(#\Newline))))
                 (test-assert
                  (search "settings" text)
                  "the callable info command presents the settings display")
                 (flet ((field-present-p (label value)
                          "Return true when one output line associates LABEL with VALUE."
                          (some (lambda (line)
                                  (and (search label line)
                                       (search value line)))
                                lines)))
                   (dolist (field
                            (list
                             (list "model" (getf before :model))
                             (list "effort" (getf before :effort))
                             (list "web search" (getf before :web-search))
                             '("trace" "on")
                             '("timestamps" "on")
                             '("compact view" "off")
                             '("STE" "on")
                             '("hurry-up" "on")
                             '("permissions"
                               "allow commands inside the workspace sandbox")))
                     (test-assert
                      (field-present-p (first field) (second field))
                      (format nil "info reports the ~A setting" (first field))))))
               (test-assert
                (equal before (settings-state))
                "info does not change any displayed setting")
               (test-assert
                (handler-case
                    (progn
                      (application-operation-call application "info" "unexpected")
                      nil)
                  (program-error ()
                    t))
                "info rejects callable arguments")
               (let ((command (application-command-find "/info")))
                 (test-assert
                  (and command
                       (application-command-semantic-handler-p command)
                       (null (application-command-call-lambda-list command)))
                  "info exposes no editable command arguments")))))
      (when (terminal-started-p terminal)
        (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                   :tool "resource.read" :status :ok :output "read output"))
               (failed-read
                 '(:tool-result :seq 2 :time 0 :call-id 2
                   :tool "resource.read" :status :error :output "read failed")))
           (test-assert (application-compact-view-p application)
                        "compact tool presentation defaults to enabled")
           (test-assert
            (null (conversation-record-entry application successful-read))
            "compact presentation hides successful routine results")
           (test-assert
            (conversation-record-entry application failed-read)
            "compact presentation retains failed routine results")
           (dolist (tool-name '("shell.run" "lisp.eval" "self.eval" "resource.edit"))
             (test-assert
              (conversation-record-entry
               application
               (list :tool-result :seq 3 :time 0 :call-id tool-name
                     :tool tool-name :status ':ok :output "ok"))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-turn-timestamps-command () null)
(defun test-turn-timestamps-command ()
  "Test persistent optional timestamps for transcript turn headers."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (application-tests--ui-application :columns 60))
         (ui (application-ui application))
         (terminal (terminal-ui-terminal ui)))
    (setf (application-configuration application) configuration
          (application-conversation application)
          (conversation-create configuration :identifier "timestamp-command"))
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (test-assert (not (application-turn-timestamps-p application))
                        "turn timestamps default to hidden")
           (test-assert (eq (application-command application "/timestamps on")
                            ':continue)
                        "/timestamps on remains a nonmodal command")
           (test-assert (application-turn-timestamps-p application)
                        "/timestamps on enables header timestamps")
           (test-assert (preferences-turn-timestamps-p configuration)
                        "/timestamps on persists visible timestamps")
           (test-assert (search "enabled and saved"
                                (recording-terminal-output terminal))
                        "/timestamps on confirms persistence")
           (recording-terminal-reset terminal)
           (test-assert (eq (application-command application "/timestamps off")
                            ':continue)
                        "/timestamps off remains a nonmodal command")
           (test-assert (not (application-turn-timestamps-p application))
                        "/timestamps off hides header timestamps")
           (test-assert (not (preferences-turn-timestamps-p configuration))
                        "/timestamps off persists hidden timestamps")
           (test-assert (search "hidden and saved"
                                (recording-terminal-output terminal))
                        "/timestamps off confirms persistence")
           (test-assert
            (handler-case
                (progn
                  (application-turn-timestamps-command application "sometimes")
                  nil)
              (configuration-error ()
                t))
            "unsupported timestamp modes signal a typed usage error")
            (recording-terminal-reset terminal)
            (test-assert
             (eq (application--run-command-input
                  application "/timestamps sometimes")
                 ':failed)
             "the normal command boundary presents expected usage errors")
            (let* ((output (recording-terminal-output terminal))
                   (command-position (search "/timestamps sometimes" output))
                   (error-position (search "✗ error" output)))
              (test-assert
               (and command-position
                    error-position
                    (< command-position error-position))
               "normal command errors retain their originating command heading")))
      (terminal-ui-stop ui)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-simple-technical-english-command () null)
(defun test-simple-technical-english-command ()
  "Test persistent opt-in Simple Technical English response guidance."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (application-tests--ui-application :columns 72))
         (ui (application-ui application))
         (terminal (terminal-ui-terminal ui)))
    (setf (application-configuration application) configuration)
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (test-assert
            (not (preferences-simple-technical-english-p configuration))
            "Simple Technical English defaults to disabled")
           (application-simple-technical-english-command application nil)
           (test-assert
            (search "disabled" (recording-terminal-output terminal))
            "/ste reports its disabled default")
           (recording-terminal-reset terminal)
           (test-assert
            (eq (application-command application "/ste on") ':continue)
            "/ste on remains a nonmodal command")
           (test-assert
            (preferences-simple-technical-english-p configuration)
            "/ste on persists its enabled state")
           (test-assert
            (search "Future replies will use it"
                    (recording-terminal-output terminal))
            "/ste on confirms when the new response style takes effect")
           (test-assert (search "/ste" (application-help))
                        "the command reference includes /ste")
           (recording-terminal-reset terminal)
           (application-simple-technical-english-command application nil)
           (test-assert
            (search "enabled" (recording-terminal-output terminal))
            "/ste reports its enabled state")
           (recording-terminal-reset terminal)
           (test-assert
            (eq (application-command application "/ste off") ':continue)
            "/ste off remains a nonmodal command")
           (test-assert
            (not (preferences-simple-technical-english-p configuration))
            "/ste off persists its disabled state")
           (test-assert
            (search "disabled and saved"
                    (recording-terminal-output terminal))
            "/ste off confirms persistence")
           (test-assert
            (handler-case
                (progn
                  (application-simple-technical-english-command
                   application "sometimes")
                  nil)
              (configuration-error ()
                t))
            "unsupported response styles signal a typed usage error"))
      (terminal-ui-stop ui)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                          :permission-state state
                          :provider
                          (make-instance
                           'rlm-inference-test-provider
                           :results
                           (list (rlm-inference-test-result
                                  "classify-1"
                                  "{\"decision\": \"sandboxed\", \"reason\": \"read-only inspection\"}"
                                  20)
                                 (rlm-inference-test-result
                                  "classify-2"
                                  "{\"decision\": \"deny\", \"reason\": \"privilege escalation\"}"
                                  20))))))
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
             (application-command application "/permissions auto")
             (test-assert (eq (application-permission-mode application) ':auto)
                          "/permissions auto selects pick-for-me mode")
             (test-assert
              (eq (application-authorize-command
                   application "git status" root)
                  ':sandboxed)
              "auto mode allows safe inspection inside the sandbox")
             (test-assert
              (eq (application-authorize-command
                   application "sudo ls" root)
                  ':deny)
              "auto mode refuses privilege-escalation commands")
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
               (test-assert
                (not (conversation-storage-occupied-p
                      (conversation-pathname empty)))
                "Ctrl-C does not persist an empty conversation")))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-repeated-interrupt-forces-exit () null)
(defun test-repeated-interrupt-forces-exit ()
  "Test a second active-cancellation Ctrl-C forces exit before cleanup finishes."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration
                                            :identifier "force-resume")))
    (unwind-protect
         (let* ((terminal (make-instance 'recording-terminal :columns 80))
                (ui (terminal-ui-create :terminal terminal))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation conversation
                                            :ui ui))
                (forced-status nil)
                (terminal-restored-before-exit-p nil)
                (controller
                  (make-instance
                   'application-input-controller
                   :application application
                   :later-state (make-instance 'later-state)
                   :main-thread (current-thread)
                   :interrupt-clock-function (lambda () 10)
                   :forced-exit-function
                   (lambda (status)
                     (setf forced-status status
                           terminal-restored-before-exit-p
                           (and (not (terminal-started-p terminal))
                                (not (terminal-interactive-p terminal))))))))
           (conversation-append-user-message conversation "keep this conversation")
           (setf (application-input-controller application) controller
                 (application-input-controller-active-p controller) t)
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (application-input-controller--process-event controller ':interrupt)
             (test-assert
              (application-input-controller-turn-cancellation-p controller)
              "the first Ctrl-C leaves cancellation cleanup pending")
             (test-assert (null forced-status)
                          "the first active Ctrl-C never forces exit")
             (application-input-controller--process-event controller ':interrupt)
             (test-assert (= forced-status *application-forced-interrupt-status*)
                          "a timely second active Ctrl-C forces process status 130")
             (test-assert terminal-restored-before-exit-p
                          "forced interruption restores the terminal before exit")
             (test-assert
              (null (application-input-controller-exit-reason controller))
              "active cancellation does not become application shutdown")
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (search
                 "Ctrl-C pressed twice within 2.5 seconds; forcing Autolith to exit."
                 output)
                "forced interruption explains the active cancellation window")
               (test-assert
                (search "autolith resume force-resume" output)
                "forced cancellation carries the exact resume command"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-forced-exit-without-durable-conversation () null)
(defun test-forced-exit-without-durable-conversation ()
  "Test forced exit offers no resume command for an unsaved conversation."
  (let* ((terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (forced-status nil)
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () 10)
            :forced-exit-function
            (lambda (status)
              (setf forced-status status)))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (application-input-controller--process-event controller ':interrupt)
      (application-input-controller--process-event controller ':interrupt)
      (test-assert (= forced-status *application-forced-interrupt-status*)
                   "a timely second active Ctrl-C still forces exit")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (search
          "Ctrl-C pressed twice within 2.5 seconds; forcing Autolith to exit."
          output)
         "forced interruption still explains why Autolith exits")
        (test-assert (not (search "autolith resume" output))
                     "an unsaved conversation produces no resume command"))))
  nil)

(-> test-graceful-shutdown-retains-interrupt-escape () null)
(defun test-graceful-shutdown-retains-interrupt-escape ()
  "Test Ctrl-C forces already-pending shutdown while Escape remains harmless."
  (dolist (reason '(:quit :end-of-input :shutdown))
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
             (test-assert
              (null (application-input-controller-interrupt-deadline controller))
              "ordinary shutdown never owns the active-cancellation timer")
             (test-assert (null (terminal-ui-notice ui))
                          "ordinary shutdown never shows the force-window hint")
             (queued-recording-terminal-enqueue terminal ':escape)
             (sleep 0.05)
             (test-assert
              (null (with-lock-held (status-lock) forced-status))
              "Escape remains harmless during graceful shutdown")
             (queued-recording-terminal-enqueue terminal ':interrupt)
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held (status-lock)
                   (= (or forced-status -1)
                      *application-forced-interrupt-status*)))
               2)
              "Ctrl-C immediately forces already-pending shutdown")
             (application-input-controller-stop controller)
             (setf controller nil)
             (test-assert
              (search
               "Ctrl-C pressed during shutdown; forcing Autolith to exit."
               (recording-terminal-output terminal))
              "forced shutdown explains that cleanup was already pending"))
        (when controller
          (ignore-errors (application-input-controller-stop controller)))
        (ignore-errors (terminal-ui-stop ui)))))
  nil)

(-> test-active-turn-interrupt-events () null)
(defun test-active-turn-interrupt-events ()
  "Test active Ctrl-C holds follow-ups while Escape permits continuation."
  (dolist (event '(:interrupt :escape))
    (let* ((now 10)
           (terminal (make-instance 'recording-terminal :columns 80))
           (ui (terminal-ui-create
                :terminal terminal
                :clock-function (lambda () now)))
           (application (make-instance 'application :ui ui))
           (controller
             (make-instance
              'application-input-controller
              :application application
              :later-state (make-instance 'later-state)
              :main-thread (current-thread)
              :interrupt-clock-function (lambda () now))))
      (setf (application-input-controller application) controller
            (application-input-controller-active-p controller) t)
      (deque-push-back
       (application-input-controller-work-items controller)
       (list ':message "queued"))
      (deque-push-back
       (application-input-controller-steering-items controller)
       "steering")
      (terminal-ui-set-input ui "draft survives")
      (application-input-controller--process-event controller event)
      (test-assert (not (application-input-controller-stopping-p controller))
                   "an active-turn stop keeps the application running")
      (test-assert (null (application-input-controller-exit-reason controller))
                   "an active-turn stop does not set an application exit reason")
      (test-assert
       (string= (line-editor-text (terminal-ui-editor ui)) "draft survives")
       "active-turn stop keys preserve the current draft")
      (test-assert
       (equal (application-input-controller--state controller :work-items)
              (list (list ':message "queued")))
       "active-turn stop keys preserve queued work")
      (test-assert
       (equal (application-input-controller--state controller :steering-items)
              (list "steering"))
       "active-turn stop keys preserve steering work")
      (test-assert
       (application-input-controller-turn-cancellation-p controller)
       "the cancellation lifecycle remains active until work cleanup finishes")
      (test-assert
       (eq (application-input-controller-queued-work-paused-p controller)
           (eq event ':interrupt))
       "only active Ctrl-C pauses queued work after cancellation")
      (test-assert
       (eq (not (null
                 (application-input-controller-interrupt-deadline controller)))
           (eq event ':interrupt))
       "only active Ctrl-C arms the force-exit window")
      (test-assert
       (eq (not (null
                 (application-input-controller-interrupt-hint-time controller)))
           (eq event ':interrupt))
       "only active Ctrl-C schedules the transient force-exit notice")
      (test-assert (null (terminal-ui-notice ui))
                   "no stop key shows the force-exit notice immediately")
      (application-input-controller--refresh-interrupt-hint controller)
      (test-assert (null (terminal-ui-notice ui))
                   "a cancellation inside the hint delay stays silent")
      (setf now 21/2)
      (application-input-controller--refresh-interrupt-hint controller)
      (test-assert
       (eq (not (null (terminal-ui-notice ui)))
           (eq event ':interrupt))
       "only active Ctrl-C announces forced exit once cancellation persists")
      (test-assert
       (or (eq event ':escape)
           (= (terminal-ui-notice-deadline ui)
              (application-input-controller-interrupt-deadline controller)))
       "the visible notice expires exactly with the force-exit window")
      (test-assert
       (application-input-controller--consume-turn-cancellation-delivery-p
        controller)
       "the first active stop records one pending cancellation delivery")
      (test-assert
       (not
        (application-input-controller--request-active-turn-cancellation
         controller))
       "turn-only cancellation has a single request winner")
      (test-assert
       (not
        (application-input-controller--consume-turn-cancellation-delivery-p
         controller))
       "a repeated request does not re-arm cancellation delivery")
      (application-input-controller--finish-work controller)
      (test-assert
       (and (not (application-input-controller-turn-cancellation-p controller))
            (null
             (application-input-controller-interrupt-deadline controller)))
       "work cleanup ends the cancellation lifecycle and force window")
      (test-assert
       (eq (not (null (terminal-ui-notice ui)))
           (eq event ':interrupt))
       "only Ctrl-C explains that queued work remains held")
      (when (eq event ':interrupt)
        (test-assert
         (application-input-controller-queued-work-paused-p controller)
         "Ctrl-C leaves queued work paused after cancellation cleanup")
        (application-input-controller--enqueue
         controller ':message "explicitly resume")
        (test-assert
         (and
          (not (application-input-controller-queued-work-paused-p controller))
          (null (terminal-ui-notice ui)))
         "new explicit input resumes held queued work and clears its notice"))))
  nil)

(-> test-idle-interrupt-exits-without-force-hint () null)
(defun test-idle-interrupt-exits-without-force-hint ()
  "Test idle Ctrl-C clears a draft or exits directly without force state."
  (let* ((terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () 10))))
    (setf (application-input-controller application) controller)
    (terminal-ui-set-input ui "clear me")
    (application-input-controller--process-event controller ':interrupt)
    (test-assert (not (application-input-controller-stopping-p controller))
                 "idle Ctrl-C only clears a nonempty draft")
    (test-assert (string= (line-editor-text (terminal-ui-editor ui)) "")
                 "idle Ctrl-C clears the current draft")
    (test-assert
     (and (null (application-input-controller-interrupt-deadline controller))
          (null (terminal-ui-notice ui)))
     "clearing an idle draft never arms or displays forced exit")
    (application-input-controller--process-event controller ':interrupt)
    (test-assert (application-input-controller-stopping-p controller)
                 "idle Ctrl-C at an empty prompt requests graceful exit")
    (test-assert (eq (application-input-controller-exit-reason controller)
                     ':interrupt)
                 "empty-prompt Ctrl-C preserves the interrupt exit reason")
    (test-assert
     (and (null (application-input-controller-interrupt-deadline controller))
          (null (terminal-ui-notice ui)))
     "empty-prompt Ctrl-C exits without a force window or hint"))
  nil)

(-> test-active-turn-stop-keys () null)
(defun test-active-turn-stop-keys ()
  "Test Ctrl-C and Escape cancel one provider turn and return to the prompt."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (dolist (event '(:interrupt :escape))
           (let* ((conversation
                    (conversation-create
                     configuration
                     :identifier (format nil "active-stop-~(~A~)" event)))
                  (provider
                    (make-instance
                     'gated-provider
                     :results
                     (list
                      (agent-test-result
                       "unexpected completion"
                       (list (agent-test-message "not cancelled"))
                       :turn-completion ':end))))
                  (terminal
                    (make-instance 'queued-recording-terminal :columns 80))
                  (ui (terminal-ui-create :terminal terminal))
                  (registry (agent-test-registry))
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
                                   :ui ui))
                  (forced-status nil)
                  (provider-entered-observed-p nil)
                  (cancellation-finished-observed-p nil)
                  (draft-preserved-observed-p nil)
                  (output-before-exit nil)
                  (producer nil))
             (queued-recording-terminal-enqueue
              terminal '(:insert "cancel this turn"))
             (queued-recording-terminal-enqueue terminal ':submit)
             (setf producer
                   (make-thread
                    (lambda ()
                      (setf provider-entered-observed-p
                            (task-tests--wait-until
                             (lambda ()
                               (gated-provider-entered-p provider))
                             2))
                      (when provider-entered-observed-p
                        (queued-recording-terminal-enqueue
                         terminal '(:insert "draft survives"))
                        (queued-recording-terminal-enqueue terminal event)
                        (setf cancellation-finished-observed-p
                              (task-tests--wait-until
                               (lambda ()
                                 (let ((controller
                                         (application-input-controller
                                          application)))
                                   (and controller
                                        (not
                                         (application-input-controller-turn-active-p
                                          controller))
                                        (not
                                         (application-input-controller--turn-cancellation-active-p
                                          controller))
                                        (null
                                         (application-input-controller-interrupt-deadline
                                          controller))
                                        (null (terminal-ui-notice ui)))))
                               2)))
                      (unless cancellation-finished-observed-p
                        (gated-provider-release provider)
                        (task-tests--wait-until
                         (lambda ()
                           (let ((controller
                                   (application-input-controller application)))
                             (and controller
                                  (not
                                   (application-input-controller-turn-active-p
                                    controller)))))
                         2))
                      (setf draft-preserved-observed-p
                            (string=
                             (line-editor-text (terminal-ui-editor ui))
                             "draft survives")
                            output-before-exit
                            (recording-terminal-output terminal))
                      (queued-recording-terminal-enqueue terminal ':interrupt)
                      (queued-recording-terminal-enqueue
                       terminal '(:insert "/quit"))
                      (queued-recording-terminal-enqueue terminal ':submit))
                    :name "Autolith active-turn stop-key test"))
             (unwind-protect
                  (let ((*application-forced-exit-function*
                          (lambda (status)
                            (setf forced-status status))))
                    (application-run application))
               (gated-provider-release provider)
               (when producer
                 (join-thread producer)
                 (setf producer nil)))
             (test-assert provider-entered-observed-p
                          "the provider turn reaches its cancellation gate")
             (test-assert cancellation-finished-observed-p
                          "the cancelled turn finishes and returns to input")
             (test-assert
              (not (search "not cancelled"
                           (recording-terminal-output terminal)))
              "the stop key cancels the active provider turn")
             (test-assert (null forced-status)
                          "one active-turn stop key never forces exit")
             (test-assert draft-preserved-observed-p
                          "the active reader preserves draft text while cancelling")
             (test-assert
              (not
               (search "To resume this conversation, run:" output-before-exit))
              "active-turn cancellation never prints a resume command")
             (test-assert
              (not
               (search "Press Ctrl-C again within 2.5 seconds"
                       output-before-exit))
              "a promptly cancelled turn never flashes the force-exit hint")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

;;;; -- Active Command Cancellation --

(-> test-active-command-stop-key () null)
(defun test-active-command-stop-key ()
  "Test Escape cancels a shared active command without fatal recovery."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration
                                            :identifier "active-command-stop"))
         (terminal      (make-instance 'queued-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (application   (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :provider nil
                                       :tool-registry (make-instance 'tool-registry)
                                       :worker nil
                                       :agent nil
                                       :ui ui))
         (command-lock  (make-lock "Autolith active command stop-key test"))
         (command-gate  (make-condition-variable
                         :name "Autolith active command stop-key test"))
         (command-entered-p nil)
         (command-released-p nil)
         (registrations (application-command--registry-snapshot))
         (forced-status nil)
         (cancellation-finished-observed-p nil)
         (draft-preserved-observed-p nil)
         (producer nil))
    (unwind-protect
         (unwind-protect
              (progn
                (register-application-command
                 (application-command-create
                  :definition-name 'test-active-command-stop-key-command
                  :name "/test-cancel-command"
                  :aliases nil
                  :argument nil
                  :description "Block until the active-turn cancellation test stops it."
                  :tip "Use this test command only from the application test suite."
                  :busy-behavior ':hold
                  :terminal-behavior ':shared
                  :handler
                  (lambda (ignored-application ignored-invocation)
                    (declare (ignore ignored-application ignored-invocation))
                    (with-lock-held (command-lock)
                      (setf command-entered-p t)
                      (condition-notify command-gate)
                      (loop until command-released-p
                            do (condition-wait command-gate command-lock)))
                    ':continue))
                 :source ':test)
                (queued-recording-terminal-enqueue
                 terminal '(:insert "/test-cancel-command"))
                (queued-recording-terminal-enqueue terminal ':submit)
                (setf producer
                      (make-thread
                       (lambda ()
                         (let ((command-entered-observed-p
                                 (task-tests--wait-until
                                  (lambda ()
                                    (with-lock-held (command-lock)
                                      command-entered-p))
                                  2)))
                           (when command-entered-observed-p
                             (queued-recording-terminal-enqueue
                              terminal '(:insert "draft survives"))
                             (queued-recording-terminal-enqueue terminal ':escape)
                             (setf cancellation-finished-observed-p
                                   (task-tests--wait-until
                                    (lambda ()
                                      (let ((controller
                                              (application-input-controller
                                               application)))
                                        (and controller
                                             (not
                                              (application-input-controller-turn-active-p
                                               controller))
                                             (not
                                              (application-input-controller--turn-cancellation-active-p
                                               controller)))))
                                    2)))
                           (unless cancellation-finished-observed-p
                             (with-lock-held (command-lock)
                               (setf command-released-p t)
                               (condition-notify command-gate)))
                           (setf draft-preserved-observed-p
                                 (string=
                                  (line-editor-text (terminal-ui-editor ui))
                                  "draft survives"))
                           (queued-recording-terminal-enqueue terminal ':interrupt)
                           (queued-recording-terminal-enqueue terminal ':interrupt)))
                       :name "Autolith active command stop-key test"))
                (let ((*application-forced-exit-function*
                        (lambda (status)
                          (setf forced-status status))))
                  (application-run application))
                (test-assert cancellation-finished-observed-p
                             "Escape cancels a shared active command and returns to input")
                (test-assert draft-preserved-observed-p
                             "command cancellation preserves the draft")
                (test-assert (null forced-status)
                             "command cancellation never enters forced exit"))
           (with-lock-held (command-lock)
             (setf command-released-p t)
             (condition-notify command-gate))
           (when producer
             (join-thread producer)
             (setf producer nil))
           (application-command--registry-restore registrations))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

;;;; -- Active Tool Cancellation --

(-> test-active-tool-stop-repairs-unknown-outcome () null)
(defun test-active-tool-stop-repairs-unknown-outcome ()
  "Test cancellation records an unknown tool outcome before the next turn."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "active-tool-stop"))
                (provider
                  (make-instance
                   'application-test-counting-provider
                   :results
                   (list
                    (agent-test-result
                     "tool-call"
                     (list
                      (agent-test-call
                       :call-id "interrupted-tool-call"
                       :name "gate")))
                    (agent-test-result
                     "recovered-turn"
                     (list (agent-test-message "the next turn ran"))
                     :turn-completion ':end))))
                (registry (agent-test-registry))
                (tool
                  (make-instance
                   'application-test-gated-tool
                   :namespace "test"
                   :name "gate"
                   :description "Wait at the active-tool cancellation test gate."
                   :parameters (tool-object-schema (json-object) '())))
                (terminal (make-instance 'queued-recording-terminal :columns 80))
                (ui (terminal-ui-create :terminal terminal))
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
                                 :ui ui))
                (forced-status nil)
                (tool-entered-observed-p nil)
                (cancellation-finished-observed-p nil)
                (next-turn-observed-p nil)
                (next-turn-finished-observed-p nil)
                (producer-condition nil)
                (producer nil))
           (tool-registry-register registry tool)
           (queued-recording-terminal-enqueue
            terminal '(:insert "run the gated tool"))
           (queued-recording-terminal-enqueue terminal ':submit)
           (setf producer
                 (make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (progn
                               (setf tool-entered-observed-p
                                     (application-test-gated-tool-wait-until-entered
                                      tool 2))
                               (when tool-entered-observed-p
                                 (queued-recording-terminal-enqueue terminal ':escape)
                                 (setf cancellation-finished-observed-p
                                       (task-tests--wait-until
                                        (lambda ()
                                          (let ((controller
                                                  (application-input-controller
                                                   application)))
                                            (and controller
                                                 (not
                                                  (application-input-controller-turn-active-p
                                                   controller))
                                                 (not
                                                  (application-input-controller--turn-cancellation-active-p
                                                   controller)))))
                                        2)))
                               (unless cancellation-finished-observed-p
                                 (application-test-gated-tool-release tool))
                               (when cancellation-finished-observed-p
                                 (queued-recording-terminal-enqueue
                                  terminal '(:insert "continue after cancellation"))
                                 (queued-recording-terminal-enqueue terminal ':submit)
                                 (setf next-turn-observed-p
                                       (application-test-counting-provider-wait-for-requests
                                        provider 2 2))
                                 (when next-turn-observed-p
                                   (setf next-turn-finished-observed-p
                                         (task-tests--wait-until
                                          (lambda ()
                                            (let ((controller
                                                    (application-input-controller
                                                     application)))
                                              (and controller
                                                   (not
                                                    (application-input-controller-turn-active-p
                                                     controller))
                                                   (not
                                                    (application-input-controller--turn-cancellation-active-p
                                                     controller)))))
                                          2)))))
                           (error (condition)
                             (setf producer-condition condition)))
                      (queued-recording-terminal-enqueue terminal ':interrupt)
                      (queued-recording-terminal-enqueue
                       terminal '(:insert "/quit"))
                      (queued-recording-terminal-enqueue terminal ':submit)))
                  :name "Autolith active tool stop-key test"))
           (unwind-protect
                (let ((*application-forced-exit-function*
                        (lambda (status)
                          (setf forced-status status))))
                  (application-run application))
             (application-test-gated-tool-release tool)
             (when producer
               (join-thread producer)
               (setf producer nil)))
           (when producer-condition
             (error producer-condition))
           (test-assert tool-entered-observed-p
                        "the tool reaches its cancellable execution point")
           (test-assert cancellation-finished-observed-p
                        "active-tool cancellation finishes before the next turn")
           (test-assert next-turn-observed-p
                        "the next queued message reaches the provider")
           (test-assert next-turn-finished-observed-p
                        "the repaired next turn finishes before test shutdown")
           (test-assert (not (application-test-gated-tool-completed-p tool))
                        "cancellation leaves the tool outcome unknown")
           (test-assert (null forced-status)
                        "one active-tool stop key never forces exit")
           (let* ((snapshots
                    (nreverse
                     (copy-list
                      (scripted-provider-input-snapshots provider))))
                  (next-request (second snapshots))
                  (output
                    (find-if
                     (lambda (item)
                       (and (json-object-p item)
                            (string=
                             (or (json-get item "type") "")
                             "function_call_output")))
                     next-request)))
             (test-assert (= (length snapshots) 2)
                          "cancellation prevents a same-turn tool retry request")
             (test-assert output
                          "the next request includes an unknown-outcome tool result")
             (test-assert
              (string= (json-get output "call_id") "interrupted-tool-call")
              "the repaired result retains its original tool call identifier")
             (test-assert
              (string= (json-get output "output")
                       *conversation-interrupted-tool-output*)
              "the repaired result explains that tool state is unknown")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-interrupt-force-window () null)
(defun test-interrupt-force-window ()
  "Test only a second Ctrl-C within 2.5 seconds requests forced exit."
  (let* ((now 10)
         (application (make-instance 'application))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () now))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (test-assert
     (null
      (application-input-controller--active-turn-interrupt-action controller))
     "an idle Ctrl-C owns no active-cancellation action")
    (test-assert
     (application-input-controller--request-active-turn-cancellation
      controller :force-exit-window-p t)
     "the first Ctrl-C only arms forced exit")
    (setf now 25/2)
    (test-assert
     (eq (application-input-controller--active-turn-interrupt-action controller)
         ':force)
     "a second Ctrl-C at the 2.5-second boundary forces exit")
    (setf now 15)
    (test-assert
     (eq (application-input-controller--active-turn-interrupt-action controller)
         ':hint)
     "a later first Ctrl-C starts a fresh window")
    (setf now 17501/1000)
    (test-assert
     (eq (application-input-controller--active-turn-interrupt-action controller)
         ':hint)
     "a Ctrl-C after 2.5 seconds does not force exit")
    (setf now 20)
    (test-assert
     (eq (application-input-controller--active-turn-interrupt-action controller)
         ':force)
     "the press after an expired window can arm a new timely second press"))
  nil)

(-> test-visible-interrupt-hint-does-not-extend-window () null)
(defun test-visible-interrupt-hint-does-not-extend-window ()
  "Test a delayed visible notice cannot extend an expired force deadline."
  (let* ((now 0)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () now)))
         (application (make-instance 'application :ui ui))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () now))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (test-assert
     (application-input-controller--request-active-turn-cancellation
      controller :force-exit-window-p t)
     "the first Ctrl-C arms the force-exit deadline")
    (terminal-ui-set-notice
     ui
     "Press Ctrl-C again within 2.5 seconds to force exit."
     :duration-seconds 5)
    (setf now 3)
    (test-assert
     (eq (application-input-controller--active-turn-interrupt-action controller)
         ':hint)
     "a visible notice cannot make an expired Ctrl-C force exit")
    (test-assert
     (= (application-input-controller-interrupt-deadline controller) 11/2)
     "the expired press starts a fresh 2.5-second window"))
  nil)

(-> test-dropped-interrupt-hint-reappears () null)
(defun test-dropped-interrupt-hint-reappears ()
  "Test contended presentation only delays the force-exit hint."
  (let* ((now 0)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () now)))
         (application (make-instance 'application :ui ui))
         (gate (make-lock "Autolith hint contention test"))
         (gate-condition (make-condition-variable :name "Autolith hint test"))
         (holding-p nil)
         (released-p nil)
         (holder nil)
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () now))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (application-input-controller--request-active-turn-cancellation
     controller :force-exit-window-p t)
    (setf now 1/2)
    (setf holder
          (make-thread
           (lambda ()
             (with-terminal-ui-locked (ui)
               (with-lock-held (gate)
                 (setf holding-p t)
                 (condition-notify gate-condition)
                 (loop until released-p
                       do (condition-wait gate-condition gate)))))
           :name "Autolith hint contention holder"))
    (unwind-protect
         (progn
           (with-lock-held (gate)
             (loop until holding-p
                   do (condition-wait gate-condition gate :timeout 2)))
           (application-input-controller--refresh-interrupt-hint controller)
           (test-assert (null (terminal-ui-notice ui))
                        "contended presentation drops the due hint")
           (test-assert
            (application-input-controller-interrupt-hint-time controller)
            "a dropped hint stays due for a later reader pass"))
      (with-lock-held (gate)
        (setf released-p t)
        (condition-notify gate-condition))
      (join-thread holder))
    (application-input-controller--refresh-interrupt-hint controller)
    (test-assert (terminal-ui-notice ui)
                 "the delayed hint appears once presentation is free")
    (test-assert
     (null (application-input-controller-interrupt-hint-time controller))
     "a shown hint stops asking for another reader pass"))
  nil)

(-> test-active-cancellation-interrupt-window-expiry () null)
(defun test-active-cancellation-interrupt-window-expiry ()
  "Test an expired active-cancellation window requires a new timely press."
  (let* ((now 0)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (forced-status nil)
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () now)
            :forced-exit-function
            (lambda (status)
              (setf forced-status status)))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (application-input-controller--process-event controller ':interrupt)
    (test-assert
     (= (application-input-controller-interrupt-deadline controller) 5/2)
     "the first active Ctrl-C starts the 2.5-second window")
    (setf now 3)
    (application-input-controller--process-event controller ':interrupt)
    (test-assert (null forced-status)
                 "an expired second active Ctrl-C does not force exit")
    (test-assert
     (= (application-input-controller-interrupt-deadline controller) 11/2)
     "an expired second active Ctrl-C starts a fresh window")
    (setf now 5)
    (application-input-controller--process-event controller ':interrupt)
    (test-assert (= forced-status *application-forced-interrupt-status*)
                 "a new timely second active Ctrl-C forces exit"))
  nil)

(-> test-cancellation-completion-clears-interrupt-state () null)
(defun test-cancellation-completion-clears-interrupt-state ()
  "Test completed cancellation clears force state and restores idle Ctrl-C."
  (let* ((now 0)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (forced-status nil)
         (controller
           (make-instance
            'application-input-controller
            :application application
            :later-state (make-instance 'later-state)
            :main-thread (current-thread)
            :interrupt-clock-function (lambda () now)
            :forced-exit-function
            (lambda (status)
              (setf forced-status status)))))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (application-input-controller--process-event controller ':interrupt)
    (test-assert
     (and (application-input-controller-turn-cancellation-p controller)
          (application-input-controller-interrupt-deadline controller)
          (application-input-controller-interrupt-hint-time controller))
     "active Ctrl-C establishes cancellation lifecycle and force state")
    (setf now 1/2)
    (application-input-controller--refresh-interrupt-hint controller)
    (test-assert (terminal-ui-notice ui)
                 "a cancellation past the hint delay explains forced exit")
    (application-input-controller--finish-work controller)
    (test-assert
     (and (not (application-input-controller-active-p controller))
          (not (application-input-controller-turn-cancellation-p controller))
          (not
           (application-input-controller-turn-cancellation-delivery-pending-p
            controller))
          (null (application-input-controller-interrupt-deadline controller))
          (null (application-input-controller-interrupt-hint-time controller))
          (null (terminal-ui-notice ui)))
     "turn cleanup clears cancellation delivery, deadline, and notice")
    (application-input-controller--process-event controller ':interrupt)
    (test-assert (application-input-controller-stopping-p controller)
                 "the next empty-prompt Ctrl-C follows idle exit semantics")
    (test-assert (eq (application-input-controller-exit-reason controller)
                     ':interrupt)
                 "post-cancellation idle Ctrl-C records ordinary interrupt exit")
    (test-assert (null forced-status)
                 "post-cancellation idle Ctrl-C never inherits forced exit")
    (test-assert
     (null (application-input-controller-interrupt-deadline controller))
     "post-cancellation idle Ctrl-C does not arm a force window"))
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
    (let* ((timestamp (encode-universal-time 0 30 14 29 7 2026))
           (timestamped-application
             (application-tests--ui-application
              :columns 60
              :compact-view-p nil)))
      (setf (application-turn-timestamps-p timestamped-application) t)
      (let ((user-entry
              (conversation-record-entry
               timestamped-application
               (list :message :seq 1 :time timestamp :role ':user
                     :content "dated prompt"))))
        (test-assert
         (find (terminal-span :dim "  2026-07-29 14:30")
               user-entry
               :test #'equal)
         "enabled timestamps appear as dim local text beside user headers"))
      (let ((assistant-entry
              (conversation-record-entry
               timestamped-application
               (list :provider-item
                     :seq 2
                     :time timestamp
                     :wire-json
                     "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"dated answer\"}]}"))))
        (test-assert
         (find (terminal-span :dim "  2026-07-29 14:30")
               assistant-entry
               :test #'equal)
         "enabled timestamps appear as dim local text beside assistant headers")))
    (let ((entry (conversation-record-entry
                  application
                  (list :message :seq 1 :time 0 :role ':user
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
    (let ((item
            (json-object
             "type" "reasoning"
             "summary" (json-array
                        (json-object
                         "type" "summary_text"
                         "text" (format nil
                                        "<thought> Checked the safe path.~%Compared fallback behavior.~C[31m"
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
             (text (test-terminal-row-text entry)))
        (test-assert
         (equal (first entry) (terminal-span :hint "◇ reasoning summary"))
         "trace mode labels provider-visible reasoning summaries")
        (test-assert
         (and (find (terminal-span :dim "  │ ") entry :test #'equal)
              (search "Checked the safe path." text)
              (search "Compared fallback behavior." text))
         "trace mode presents provider summaries beside a subdued rail")
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
             (text (test-terminal-row-text entry)))
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
    (let ((form
            (application--provider-call-equivalent-form
             (json-object
              "type" "function_call"
              "namespace" "odd"
              "name" "tool"
              "arguments"
              "{\"z\":[true,false,null],\"a key)\":{\"b\":true,\"a\":\"x\"}}"))))
      (test-assert
       (string=
        form
        "(odd.tool :|a key)| (json-object \"a\" \"x\" \"b\" t) :z (vector t nil :null))")
       "provider calls render deterministic readable Lisp with distinct JSON values"))
    (let ((form
            (application--provider-call-equivalent-form
             (json-object
              "type" "function_call"
              "namespace" "1"
              "name" "2"
              "arguments" "{\"path\":\"x\"}"))))
      (test-assert (string= form "(|1.2| :path \"x\")")
                   "numeric provider names are escaped into a Lisp symbol token"))
    (let* ((call
             (json-object
              "type" "function_call"
              "namespace" "resource"
              "name" "read"
              "arguments" "{\"uri\":\"workspace:x\"} trailing"))
           (wide-application
             (application-tests--ui-application
              :columns 80
              :compact-view-p nil))
           (entry (response-item-entry wide-application call))
           (text (test-terminal-row-text entry)))
      (test-assert (null (application--provider-call-equivalent-form call))
                   "trailing JSON data cannot be presented as a Lisp form")
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ resource.read"))
            (not (search "equivalent Lisp" text))
            (not (search "(resource.read" text)))
       "malformed provider calls keep their tool name without fabricated Lisp"))
    (let* ((narrow-application
             (application-tests--ui-application
              :columns 20
              :compact-view-p nil))
           (entry
             (response-item-entry
              narrow-application
              (json-object
               "type" "function_call"
               "namespace"
               (format nil
                       "unsafe~%~C[31mnamespace-with-a-long-name"
                       *terminal-escape-character*)
               "name" "tool-with-a-long-name"
               "arguments" "{}")))
           (header (terminal-span-text (first entry))))
      (test-assert
       (and (eq (terminal-span-style (first entry)) ':tool)
            (not (find #\Newline header))
            (not (find #\Return header))
            (not (find *terminal-escape-character* header))
            (<= (text-cell-width header) 19)
            (char= (char header (1- (length header))) #\…))
       "provider-controlled tool names are sanitized and width-bounded"))
    (let* ((*application-provider-form-characters* 10)
           (uri "workspace:provider-form-fallback")
           (entry
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" "resource"
               "name" "read"
               "arguments" (json-encode (json-object "uri" uri)))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ resource.read"))
            (not (search "(resource.read" text))
            (search uri text))
       "simple tool calls retain their detail when Lisp rendering is bounded out"))
    (let ((*application-provider-form-characters* 10))
      (test-assert
       (null
        (application--provider-call-equivalent-form
         (json-object
          "type" "function_call"
          "namespace" "resource"
          "name" "read"
          "arguments" "{\"uri\":\"workspace:a path beyond the bound\"}")))
       "equivalent provider forms stop before exceeding their output bound"))
    (let* ((form
             (application--provider-call-equivalent-form
              (json-object
               "type" "function_call"
               "namespace" "x|y"
               "name" "z\\q"
               "arguments" "{}"))))
      (multiple-value-bind (expression end)
          (read-from-string form)
        (test-assert
         (and (= end (length form))
              (consp expression)
              (symbolp (first expression))
              (string= (symbol-name (first expression)) "x|y.z\\q"))
         "hostile provider names remain one escaped readable function symbol")))
    (labels ((render (source)
               "Render SOURCE as one ordinary resource.read provider call."
               (application--provider-call-equivalent-form
                (json-object
                 "type" "function_call"
                 "namespace" "resource"
                 "name" "read"
                 "arguments" source))))
      (test-assert
       (and (let ((*application-provider-form-source-characters* 1))
              (null (render "{}")))
            (let ((*application-provider-form-items* 1))
              (null (render "{\"a\":1,\"b\":2}")))
            (let ((*application-provider-form-depth* 1))
              (null (render "{\"value\":{\"a\":{\"b\":1}}}")))
            (let ((*application-provider-form-string-characters* 3))
              (null (render "{\"uri\":\"four\"}")))
            (let ((*application-provider-form-key-characters* 2))
              (null (render "{\"uri\":\"x\"}")))
            (null (render "{\"x\\ny\":1}")))
       "provider forms enforce source, item, depth, string, and key boundaries"))
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
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ self.eval"))
            (find (terminal-span :syntax-function "self.eval")
                  entry
                  :test #'equal)
            (search "(self.eval :form" text)
            (not (search "equivalent Lisp" text)))
       "provider tool requests keep their names and syntax-highlighted Lisp forms")
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
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ lisp.eval"))
            (find (terminal-span :syntax-function "lisp.eval")
                  entry
                  :test #'equal)
            (search "(lisp.eval :form \"(+ 1 2)\")" text)
            (not (search "equivalent Lisp" text))
            (not (search "{\"form\"" text)))
       "lisp.eval calls show syntax-highlighted Lisp instead of JSON"))
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
           (text (test-terminal-row-text entry)))
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
                           (test-terminal-row-text entry))
                   "web search entries show their query"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 2 :time 0 :call-id 1
                         :tool "shell.run" :status ':ok
                         :cpu-microseconds 1234
                         :real-microseconds 567890
                         :output "wrote file")))
           (text (test-terminal-row-text entry)))
      (test-assert (and (search "cpu 0.001s" text)
                        (search "real 0.568s" text))
                   "tool results show CPU and real elapsed time"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 3 :time 0 :call-id 2
                         :tool "shell.run" :status ':ok
                         :output (format nil "exit 3~%command output"))))
           (text (test-terminal-row-text entry)))
      (test-assert (and (search "exit 3" text)
                        (search "command output" text))
                   "shell.run results separate exit status from command output"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 4 :time 0 :call-id 3
                         :tool "self.eval" :status ':ok
                         :output (format nil "Output:~%hello~%Values:~%42~%"))))
           (text (test-terminal-row-text entry)))
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
                          :tool "lisp.describe" :status ':ok
                         :output (format nil
                                         "Symbol: FOO~%Package: AUTOLITH~%~
                                          Function binding: yes~%~
                                          Lambda list: (X)~%Describe:~%details"))))
           (text (test-terminal-row-text entry)))
      (test-assert (and (search "Symbol" text)
                        (search "FOO" text)
                        (find (terminal-span :strong "Describe")
                              entry
                              :test #'equal))
                   "introspection results use aligned fields and section headings"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "lisp.describe" :status ':ok
                         :output (format nil
                                         "Output:~%Symbol: CAR~%~
                                          Documentation: list head~%~
                                          Values:~%"))))
           (text (test-terminal-row-text entry)))
      (test-assert (and (search "description" text)
                        (search "Symbol" text)
                        (search "CAR" text)
                        (search "values" text))
                   "lisp.describe results separate structured description and values"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "self.eval" :status ':error
                         :output (format nil
                                         "Needs a value.~2%Available restarts:~%~
                                            CONTINUE  Try again.~%~
                                            USE-VALUE  Supply a value.~%~
                                          Retry the identical call with a restart."))))
           (text (test-terminal-row-text entry)))
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

(-> test-line-change-tool-presentation () null)
(defun test-line-change-tool-presentation ()
  "Test numbered, syntax-highlighted semantic rulers for source-bearing tools."
  (labels ((call-entry (application namespace name &key arguments)
             "Render one function call into APPLICATION's transcript."
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" (json-encode arguments))))
           (workspace-replacement-entry (application uri revision)
             "Render a two-line workspace replacement for URI and REVISION."
             (call-entry
              application "resource" "edit"
              :arguments
              (json-object
               "uri" uri
               "base-revision" revision
               "operations"
               (json-array
                (json-object
                 "op" "replace-lines"
                 "start-line" 4
                 "end-line" 5
                  "content" (format nil "(defun new-source () 2)~%~%"))))))
           (assert-unavailable-workspace-entry (entry assertion)
             "Assert ENTRY never invents an unavailable workspace preimage."
             (let ((text (test-terminal-row-text entry)))
                (test-assert
                 (and (find (terminal-span :failure "- 4 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 4 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 5 │ ")
                            entry :test #'equal)
                      (not (find (terminal-span :success "+ │ ")
                                 entry :test #'equal))
                      (find (terminal-span :syntax-function "new-source")
                            entry :test #'equal)
                      (not (find (terminal-span :syntax-function "old-source")
                                 entry :test #'equal))
                      (search "observed lines 4-5 unavailable" text))
                 assertion))))
    (let* ((application
             (application-tests--ui-application
              :columns 100
              :compact-view-p nil))
           (source (format nil "(defun highlighted-source ()~%  42)")))
      (dolist (specification
               '(("lisp" "eval" "form")
                 ("self" "eval" "form")
                 ("self" "exercise" "form")
                 ("self" "redefine" "definition")
                 ("self" "persist-definition" "definition")))
        (destructuring-bind (namespace name argument-name) specification
          (let ((entry
                  (call-entry
                   application namespace name
                   :arguments (json-object argument-name source))))
            (test-assert
             (and (find (terminal-span :success "│ ") entry :test #'equal)
                  (find (terminal-span :syntax-keyword "defun")
                        entry :test #'equal)
                  (find (terminal-span :syntax-function "highlighted-source")
                        entry :test #'equal)
                  (find (terminal-span :syntax-number "42") entry :test #'equal))
             (format nil "~A.~A uses a green syntax-highlighted source ruler"
                     namespace name)))))
      (let ((entry
              (call-entry
               application "self" "set"
               :arguments
               (json-object "symbol" "*highlighted-value*"
                            "value" "(list 1 2)"))))
        (test-assert
         (and (find (terminal-span :success "│ ") entry :test #'equal)
              (find (terminal-span :syntax-function "list") entry :test #'equal)
              (find (terminal-span :syntax-number "1") entry :test #'equal)
              (find (terminal-span :syntax-number "2") entry :test #'equal))
         "self.set syntax-highlights its value form beside a green ruler"))
      (let* ((entry
               (call-entry
                application "shell" "run"
                :arguments
                (json-object "command" "printf '%s\\n' highlighted")))
             (text (test-terminal-row-text entry)))
        (test-assert
         (and (find (terminal-span :success "│ ") entry :test #'equal)
              (find (terminal-span :syntax-function "printf")
                    entry :test #'equal)
              (search "$ printf" text))
         "shell.run syntax-highlights executable input beside a green ruler")))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (conversation
             (conversation-create configuration :identifier "line-change-resource"))
           (application
             (application-tests--ui-application
              :columns 100
              :compact-view-p nil))
           (uri "workspace:src/highlighted.lisp")
           (revision "Rline-change")
           (partial-revision "Rline-change-partial")
           (empty-uri "workspace:src/empty.lisp")
           (empty-revision "Rline-change-empty")
           (lines
             (vector "header" "context" "before"
                     "(defun old-source ()" "  1)" "after" ""))
           (observation
             (make-instance
              'workspace-file-observation
              :kind ':file
              :uri uri
              :revision "snapshot-digest"
              :content (format nil "~{~A~%~}" (coerce lines 'list))
              :lines lines
              :line-ending (string #\Newline)
              :final-newline-p t))
           (state
             (make-instance
              'workspace-file-observation-state
              :alias revision
              :observation observation
              :visible-ranges '((1 7))))
           (partial-state
             (make-instance
              'workspace-file-observation-state
              :alias partial-revision
              :observation observation
              :visible-ranges '((1 4))))
           (empty-observation
             (make-instance
              'workspace-file-observation
              :kind ':file
              :uri empty-uri
              :revision "empty-snapshot-digest"
              :content ""
              :lines #()
              :line-ending (string #\Newline)
              :final-newline-p nil))
           (empty-state
             (make-instance
              'workspace-file-observation-state
              :alias empty-revision
              :observation empty-observation
              :visible-ranges nil)))
      (unwind-protect
           (progn
             (setf (application-configuration application) configuration
                   (application-conversation application) conversation)
               (with-recursive-lock-held
                   ((conversation-resource-observation-lock conversation))
                 (let ((states
                         (conversation-resource-observations conversation)))
                   (fifo-cache-put states revision state)
                   (fifo-cache-put states partial-revision partial-state)
                   (fifo-cache-put states empty-revision empty-state)))
             (let* ((entry
                      (workspace-replacement-entry application uri revision))
                    (text (test-terminal-row-text entry)))
                (test-assert
                 (and (find (terminal-span :failure "- 4 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 4 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 5 │ ")
                            entry :test #'equal)
                      (find (terminal-span :syntax-function "old-source")
                            entry :test #'equal)
                      (find (terminal-span :syntax-function "new-source")
                            entry :test #'equal)
                      (search "- 4 │" text)
                      (search "+ 4 │" text))
                 "workspace resource edits number both the preimage and replacement"))
              (let* ((scratchpad-uri "scratchpad:highlighted.lisp")
                     (scratchpad-revision "Rscratchpad-line-change")
                     (scratchpad-observation
                       (make-instance
                        'workspace-file-observation
                        :kind ':file
                        :uri scratchpad-uri
                        :revision "scratchpad-snapshot-digest"
                        :content (format nil "~{~A~%~}" (coerce lines 'list))
                        :lines lines
                        :line-ending (string #\Newline)
                        :final-newline-p t))
                     (scratchpad-state
                       (make-instance
                        'workspace-file-observation-state
                        :alias scratchpad-revision
                        :observation scratchpad-observation
                        :visible-ranges '((1 7)))))
                (with-recursive-lock-held
                    ((conversation-resource-observation-lock conversation))
                  (fifo-cache-put
                   (conversation-resource-observations conversation)
                   scratchpad-revision
                   scratchpad-state))
                (let ((entry
                        (workspace-replacement-entry
                         application scratchpad-uri scratchpad-revision)))
                  (test-assert
                   (and (find (terminal-span :failure "- 4 │ ")
                              entry :test #'equal)
                        (find (terminal-span :success "+ 4 │ ")
                              entry :test #'equal)
                        (find (terminal-span :syntax-function "old-source")
                              entry :test #'equal)
                        (find (terminal-span :syntax-function "new-source")
                              entry :test #'equal))
                   "scratchpad resource edits reuse the numbered syntax change view")))
              (let* ((entry
                       (call-entry
                        application "resource" "edit"
                        :arguments
                        (json-object
                         "uri" uri
                         "base-revision" revision
                         "operations"
                         (json-array
                          (json-object
                           "op" "delete-lines"
                           "start-line" 7
                           "end-line" 7)))))
                     (text (test-terminal-row-text entry)))
                (test-assert
                 (and (find (terminal-span :failure "- 7 │ ")
                            entry :test #'equal)
                      (not (search "no textual change" text)))
                 "workspace edits preserve a visible trailing blank preimage row"))
              (let ((entry
                      (call-entry
                       application "resource" "edit"
                       :arguments
                       (json-object
                        "uri" uri
                        "base-revision" revision
                        "operations"
                        (json-array
                         (json-object
                          "op" "insert-after"
                          "line" 5
                          "content" "(defun later-source () 3)")
                         (json-object
                          "op" "insert-before"
                          "line" 2
                          "content" (format nil
                                            "(defun earlier-source ()~%  1)")))))))
                (test-assert
                 (and (find (terminal-span :success "+ 2 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 3 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 8 │ ")
                            entry :test #'equal))
                 "workspace insertions use exact resulting lines after earlier edits"))
              (let ((entry
                      (call-entry
                       application "resource" "edit"
                       :arguments
                       (json-object
                        "uri" uri
                        "base-revision" revision
                        "operations"
                        (json-array
                         (json-object
                          "op" "replace-lines"
                          "start-line" 5
                          "end-line" 5
                          "content" "(defun shifted-source () 4)")
                         (json-object
                          "op" "delete-lines"
                          "start-line" 2
                          "end-line" 3))))))
                (test-assert
                 (and (find (terminal-span :failure "- 5 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 3 │ ")
                            entry :test #'equal)
                      (find (terminal-span :syntax-function "shifted-source")
                            entry :test #'equal))
                 "earlier deletions shift later replacement line numbers"))
              (let ((entry
                      (call-entry
                       application "resource" "edit"
                       :arguments
                       (json-object
                        "uri" uri
                        "base-revision" revision
                        "operations"
                        (json-array
                         (json-object
                          "op" "replace-lines"
                          "start-line" 3
                          "end-line" 4
                          "content" "overlap replacement")
                         (json-object
                          "op" "insert-before"
                          "line" 4
                          "content" "overlap insertion"))))))
                (test-assert
                 (and (find (terminal-span :success "+ 3 │ ")
                            entry :test #'equal)
                      (find (terminal-span :success "+ 4 │ ")
                            entry :test #'equal)
                      (not (find (terminal-span :success "+ │ ")
                                 entry :test #'equal)))
                 "invalid workspace edits keep their declared target coordinates"))
              (let* ((entry
                       (call-entry
                        application "resource" "edit"
                        :arguments
                        (json-object
                         "uri" empty-uri
                         "base-revision" empty-revision
                         "operations"
                         (json-array
                          (json-object
                           "op" "replace-empty"
                           "content" (string #\Newline))))))
                     (text (test-terminal-row-text entry)))
                (test-assert
                 (and (find (terminal-span :success "+ 1 │ ")
                            entry :test #'equal)
                      (not (search "(empty content)" text)))
                 "empty workspace files show their first numbered blank line"))
             (assert-unavailable-workspace-entry
              (workspace-replacement-entry application uri "Rmissing")
              "workspace resource edits never invent a missing preimage")
             (assert-unavailable-workspace-entry
              (workspace-replacement-entry
               application "workspace:src/mismatch.lisp" revision)
              "workspace resource edits never use another URI's preimage")
             (assert-unavailable-workspace-entry
              (workspace-replacement-entry application uri partial-revision)
              "workspace resource edits require the full range to be visible"))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-agenda-change-tool-presentation () null)
(defun test-agenda-change-tool-presentation ()
  "Test revision-gated agenda mutations use the shared change viewer."
  (labels ((call-entry (application namespace name arguments)
             "Render one function call into APPLICATION's transcript."
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" (json-encode arguments)))))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (conversation
             (conversation-create configuration :identifier "agenda-change-view"))
           (application
             (application-tests--ui-application
              :columns 100
              :compact-view-p nil))
           (revision "Ragenda-change")
           (item nil)
           (record nil))
      (unwind-protect
           (progn
             (with-recursive-lock-held (*agenda-lock*)
               (let ((state (agenda-load configuration)))
                 (setf item
                       (agenda-add :configuration configuration
                                   :state state
                                   :text "old agenda text"
                                   :status ':todo
                                   :memory-identifiers nil)
                       record (agenda-current configuration state))))
             (setf (application-configuration application) configuration
                   (application-conversation application) conversation)
             (let* ((directory (workspace-agenda-directory record))
                    (observation
                      (make-instance
                       'agenda-observation
                       :uri "agenda:current"
                       :revision "agenda-change-digest"
                       :content (agenda-resource--render-record record)
                       :directory directory
                       :snapshot (agenda-resource--snapshot directory record)))
                    (observation-state
                      (make-instance
                       'agenda-observation-state
                       :alias revision
                       :observation observation)))
                (with-recursive-lock-held
                    ((conversation-resource-observation-lock conversation))
                  (fifo-cache-put
                   (conversation-resource-observations conversation)
                   revision
                   observation-state))
               (with-recursive-lock-held (*agenda-lock*)
                 (agenda-update configuration
                                (agenda-load configuration)
                                (agenda-item-identifier item)
                                :text "unobserved live agenda text"))
                (let* ((entry
                         (call-entry
                          application
                          "resource"
                          "edit"
                          (json-object
                           "uri" "agenda:current"
                           "base-revision" revision
                           "operations"
                           (json-array
                            (json-object
                             "op" "agenda-update"
                             "id" (agenda-item-identifier item)
                             "text" "resource agenda text")))))
                       (text (test-terminal-row-text entry)))
                  (test-assert
                   (and (find (terminal-span ':dim "  1 │ ")
                              entry :test #'equal)
                        (find (terminal-span ':failure "- 2 │ ")
                              entry :test #'equal)
                        (find (terminal-span ':success "+ 2 │ ")
                              entry :test #'equal)
                        (find (terminal-span ':dim "  3 │ ")
                              entry :test #'equal)
                        (search "text: old agenda text" text)
                        (search "text: resource agenda text" text)
                        (not (search "unobserved live agenda text" text)))
                   "agenda resource edits use their exact observed preimage"))
                (let* ((entry
                         (call-entry
                          application
                          "resource"
                          "edit"
                          (json-object
                           "uri" "agenda:current"
                           "base-revision" revision
                           "operations"
                           (json-array
                            (json-object
                             "op" "agenda-update"
                             "id" (agenda-item-identifier item)
                             "text" "first invalid agenda text")
                            (json-object
                             "op" "agenda-update"
                             "id" (agenda-item-identifier item)
                             "text" "second invalid agenda text")))))
                       (text (test-terminal-row-text entry)))
                  (test-assert
                   (and (not (find (terminal-span ':success "+ 2 │ ")
                                   entry :test #'equal))
                        (search "operation 1 · update agenda item" text)
                        (search "operation 2 · update agenda item" text))
                   "invalid multi-operation agenda edits stay structured"))))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-memory-change-tool-presentation () null)
(defun test-memory-change-tool-presentation ()
  "Test native and revision-gated memory mutations use the shared change viewer."
  (labels ((call-entry (application namespace name arguments)
             "Render one function call into APPLICATION's transcript."
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" (json-encode arguments)))))
    (let* ((configuration (test-configuration))
           (root (test-configuration-root configuration))
           (conversation
             (conversation-create configuration :identifier "memory-change-view"))
           (application
             (application-tests--ui-application
              :columns 100
              :compact-view-p nil))
           (memory
             (memory-remember
              configuration
              :title "old memory title"
              :content "old memory content"
              :scope ':workspace
              :tags '("viewer")
              :source-conversation "memory-change-view"))
           (identifier (memory-identifier memory))
           (uri (memory-resource--item-uri identifier))
           (item-revision "Rmemory-change-item")
           (collection-revision "Rmemory-change-collection")
           (global-revision "Rmemory-change-global"))
      (unwind-protect
           (progn
             (setf (application-configuration application) configuration
                   (application-conversation application) conversation)
             (let* ((item-observation
                       (make-instance
                        'memory-observation
                        :uri uri
                        :revision "memory-change-item-digest"
                        :content (memory-resource--render-item memory)
                        :identifier identifier
                        :kind ':item
                        :snapshot (list :kind ':item
                                        :identifier identifier
                                        :record (memory--record memory))))
                    (item-state
                      (make-instance
                       'memory-observation-state
                       :alias item-revision
                       :observation item-observation))
                    (workspace
                      (namestring
                       (truename
                        (configuration-working-directory configuration))))
                    (collection-observation
                      (make-instance
                       'memory-observation
                       :uri "memory:workspace"
                       :revision "memory-change-collection-digest"
                       :content (memory-resource--render-collection
                                 "workspace" (list memory))
                       :identifier "workspace"
                       :kind ':collection
                       :snapshot (list :kind ':collection
                                       :identifier "workspace"
                                       :workspace workspace
                                       :records (list (memory--record memory)))))
                    (collection-state
                      (make-instance
                       'memory-observation-state
                       :alias collection-revision
                       :observation collection-observation))
                    (global-observation
                      (make-instance
                       'memory-observation
                       :uri "memory:global"
                       :revision "memory-change-global-digest"
                       :content (memory-resource--render-collection "global" nil)
                       :identifier "global"
                       :kind ':collection
                       :snapshot (list :kind ':collection
                                       :identifier "global"
                                       :workspace nil
                                       :records nil)))
                    (global-state
                      (make-instance
                       'memory-observation-state
                       :alias global-revision
                       :observation global-observation)))
                (with-recursive-lock-held
                    ((conversation-resource-observation-lock conversation))
                  (let ((states
                          (conversation-resource-observations conversation)))
                    (fifo-cache-put states item-revision item-state)
                    (fifo-cache-put states collection-revision collection-state)
                    (fifo-cache-put states global-revision global-state)))
               (memory-remember
                configuration
                :identifier identifier
                :title "old memory title"
                :content "unobserved live memory content"
                :scope ':workspace
                :tags '("viewer")
                :source-conversation "memory-change-view")
               (let* ((entry
                        (call-entry
                         application
                         "resource"
                         "edit"
                         (json-object
                          "uri" uri
                          "base-revision" item-revision
                          "operations"
                          (json-array
                           (json-object
                            "op" "memory-replace"
                            "title" "old memory title"
                            "content" "resource memory content"
                            "tags" (json-array "viewer"))))))
                      (text (test-terminal-row-text entry)))
                 (test-assert
                  (and (find (terminal-span ':dim "  4 │ ")
                             entry :test #'equal)
                       (find (terminal-span ':failure "- 5 │ ")
                             entry :test #'equal)
                       (find (terminal-span ':success "+ 5 │ ")
                             entry :test #'equal)
                       (search "old memory content" text)
                       (search "resource memory content" text)
                       (not (search "unobserved live memory content" text)))
                  "memory item resources use their exact observed preimage"))
               (let* ((entry
                        (call-entry
                         application
                         "resource"
                         "edit"
                         (json-object
                          "uri" uri
                          "base-revision" item-revision
                          "operations"
                          (json-array
                           (json-object "op" "memory-forget")))))
                      (text (test-terminal-row-text entry)))
                 (test-assert
                  (and (find (terminal-span ':failure "- 1 │ ")
                             entry :test #'equal)
                       (find (terminal-span ':failure "- 5 │ ")
                             entry :test #'equal)
                       (not (find (terminal-span ':success "+ 1 │ ")
                                  entry :test #'equal))
                       (search "old memory content" text)
                       (not (search "unobserved live memory content" text)))
                  "memory-forget resources use their exact observed preimage"))
               (let* ((entry
                        (call-entry
                         application
                         "resource"
                         "edit"
                         (json-object
                          "uri" "memory:workspace"
                          "base-revision" collection-revision
                          "operations"
                          (json-array
                           (json-object
                            "op" "memory-remember"
                            "title" "resource-created memory"
                            "content" "resource-created content")))))
                      (text (test-terminal-row-text entry)))
                 (test-assert
                  (and (find (terminal-span ':success "+ 1 │ ")
                             entry :test #'equal)
                       (find (terminal-span ':success "+ 5 │ ")
                             entry :test #'equal)
                       (search "scope: workspace" text)
                       (search "resource-created content" text))
                  "memory collection resources use numbered green added rows"))
               (let* ((entry
                        (call-entry
                         application
                         "resource"
                         "edit"
                         (json-object
                          "uri" "memory:global"
                          "base-revision" global-revision
                          "operations"
                          (json-array
                           (json-object
                            "op" "memory-remember"
                            "title" "global resource memory"
                            "content" "global resource content")))))
                      (text (test-terminal-row-text entry)))
                 (test-assert
                  (and (find (terminal-span ':success "+ 1 │ ")
                             entry :test #'equal)
                       (find (terminal-span ':success "+ 5 │ ")
                             entry :test #'equal)
                       (search "scope: global" text)
                       (search "global resource content" text))
                  "global memory collections derive global scope in change rows"))
               (let* ((entry
                        (call-entry
                         application
                         "resource"
                         "edit"
                         (json-object
                          "uri" "memory:workspace"
                          "base-revision" collection-revision
                          "operations"
                          (json-array
                           (json-object
                            "op" "memory-remember"
                            "title" "first invalid memory"
                            "content" "first invalid content")
                           (json-object
                            "op" "memory-remember"
                            "title" "second invalid memory"
                            "content" "second invalid content")))))
                      (text (test-terminal-row-text entry)))
                 (test-assert
                  (and (not (find (terminal-span ':success "+ 1 │ ")
                                  entry :test #'equal))
                       (search "operation 1 · remember memory" text)
                       (search "operation 2 · remember memory" text))
                  "invalid multi-operation memory edits stay structured"))))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-structured-tool-presentation () null)
(defun test-structured-tool-presentation ()
  "Test resource operations and dynamic tool data never render as raw JSON."
  (let ((application
          (application-tests--ui-application :columns 80 :compact-view-p nil)))
    (let* ((entry
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" "resource"
               "name" "edit"
               "arguments"
               (json-encode
                (json-object
                 "uri" "workspace:src/example.lisp"
                 "base-revision" "revision-123"
                 "operations"
                 (json-array
                  (json-object
                   "op" "replace-lines"
                   "start-line" 4
                   "end-line" 6
                   "content" "(defun example ()~%  :updated)")
                  (json-object
                   "op" "agenda-update"
                   "id" "agenda-7"
                   "status" "done"
                   "text" "Finish the release")))))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ resource.edit"))
            (search "(resource.edit" text)
            (search "replace lines 4-6" text)
            (search "update agenda item agenda-7" text)
            (search "(defun example" text)
            (not (search "{\"op\"" text)))
       "resource.edit presents operations as verbs and code instead of JSON"))
    (let* ((entry
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" "mcp__example"
               "name" "inspect"
               "arguments"
               (json-encode
                (json-object
                 "options" (json-object "timeout" 5)
                 "targets" (json-array "first" "second")
                  "content" (format nil "untrusted~C[31m text"
                                    *terminal-escape-character*))))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (search "options.timeout" text)
            (search "targets[1]" text)
            (search "untrusted" text)
            (not (search "{\"options\"" text))
            (not (find *terminal-escape-character* text)))
       "dynamic tool calls recursively format and sanitize nested arguments"))
    (let* ((entry
             (conversation-record-entry
              application
              (list :tool-result :seq 1 :time 0 :call-id 1
                    :tool "mcp__example.inspect" :status ':ok
                    :output
                    (json-encode
                     (json-object
                      "content" "dynamic result"
                      "details" (json-object "count" 2))))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (search "dynamic result" text)
            (search "details.count" text)
            (not (search "{\"content\"" text)))
       "dynamic JSON results use the recursive readable fallback")))
  nil)

(-> test-plan-update-call-presentation () null)
(defun test-plan-update-call-presentation ()
  "Test Codex-style plan.update checklist rendering and malformed fallbacks."
  (labels ((call-entry (application arguments &key source)
             "Render one plan.update call for APPLICATION."
             (response-item-entry
              application
              (json-object
               "type" "function_call"
               "namespace" "plan"
               "name" "update"
               "arguments" (or source (json-encode arguments))))))
    (let* ((application (application-tests--ui-application :columns 60))
           (entry
             (call-entry
              application
              (json-object
               "explanation" "Adapt the implementation."
               "steps"
               (json-array
                (json-object "step" "Inspect existing behavior"
                             "status" "completed")
                (json-object "step" "Render plan steps clearly"
                             "status" "in_progress")
                (json-object "step" "Run the full checks"
                             "status" "pending")))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ plan.update"))
            (search "(plan.update" text)
            (not (search "equivalent Lisp" text))
            (search "Updated Plan" text))
       "plan.update keeps its checklist beneath the highlighted Lisp call")
      (test-assert
       (and (search "└ Adapt the implementation." text)
            (search "✔ Inspect existing behavior" text)
            (search "□ Render plan steps clearly" text)
            (search "□ Run the full checks" text)
            (find (terminal-span :plan-active "□ ") entry :test #'equal)
            (not (search "{\"step\"" text))
            (not (search "\"status\"" text)))
       "plan.update presents explanation and statuses without argument JSON"))
    (let* ((application (application-tests--ui-application :columns 28))
           (entry
             (call-entry
              application
              (json-object
               "steps"
               (json-array
                (json-object
                 "step" "Wrap a deliberately long active plan step cleanly"
                 "status" "doing")))))
           (text (test-terminal-row-text entry)))
      (test-assert
       (and (> (count #\Newline text) 2)
            (every (lambda (line)
                     (<= (text-cell-width line) 27))
                   (uiop:split-string text :separator '(#\Newline))))
       "plan steps wrap beneath their markers within narrow transcripts"))
    (let* ((application (application-tests--ui-application :columns 40))
           (malformed
             (test-terminal-row-text
              (call-entry application nil :source "{")))
           (empty
             (test-terminal-row-text
              (call-entry application
                          (json-object "steps" (json-array)))))
           (string-steps
             (test-terminal-row-text
              (call-entry application
                          (json-object "steps" "not an array"))))
           (oversized-steps
             (make-array
              (1+ *plan-maximum-steps*)
              :initial-element
              (json-object "step" "bounded plan step" "status" "pending")))
           (oversized
             (test-terminal-row-text
              (call-entry application
                          (json-object "steps" oversized-steps)))))
      (test-assert
       (and (search "arguments unavailable" malformed)
            (search "(no steps provided)" empty)
            (search "steps unavailable" string-steps)
            (search "… +1 more step" oversized)
            (not (search "invalid plan step" string-steps))
            (not (search "{" malformed)))
       "plan.update malformed, empty, and oversized calls stay bounded")))
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
                    (test-terminal-row-text compact-entry))
                  (expanded-text
                    (test-terminal-row-text expanded-entry)))
              (test-assert
               (and (equal (first compact-entry)
                           (terminal-span :tool "▸ task.run"))
                    (search "(task.run" compact-text)
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
                      (test-terminal-row-text flat-entry)))
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
                      (test-terminal-row-text malformed-json-entry))
                    (malformed-batch-entry
                      (call
                       (json-object
                        "context" "Still readable."
                        "tasks" "not-an-array")))
                    (malformed-batch-text
                      (test-terminal-row-text malformed-batch-entry))
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
                      (test-terminal-row-text malformed-item-entry)))
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
                      (test-terminal-row-text narrow-entry)))
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
                      (test-terminal-row-text bounded-entry)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-diagnosis-prompt () null)
(defun test-recovery-diagnosis-prompt ()
  "Test recovered crash context becomes one bounded diagnosis-only prompt."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (state-root (configuration-state-root configuration))
         (capsule-pathname
           (merge-pathnames "crashes/diagnosis.sexp" state-root))
         (pointer-pathname
           (merge-pathnames "crash-pointers/diagnosis.path" state-root))
         (session-pathname
           (merge-pathnames
            "recovery-session-pointers/diagnosis.sexp" state-root))
         (recovered-name "AUTOLITH_RECOVERED")
         (pointer-name "AUTOLITH_CRASH_POINTER")
         (session-name "AUTOLITH_RECOVERY_SESSION_POINTER")
         (conversation-name "AUTOLITH_RECOVERY_CONVERSATION_ID")
         (sequence-name "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
         (floor-name "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")
         (environment-names
           (list recovered-name pointer-name session-name conversation-name
                 sequence-name floor-name))
         (previous-environment
           (loop for name in environment-names
                 collect (cons name (uiop:getenv name)))))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (snapshot-write
            session-pathname
            (list :recovery-session
                  :version 1
                  :conversation-id "2345678"
                  :rendered-sequence 4
                  :history-floor-sequence 2))
           (snapshot-write
            capsule-pathname
            (list :crash
                  :version 1
                  :id "diagnosis"
                  :time 1
                  :condition-type "SIMPLE-ERROR"
                  :condition "Injected recovery failure."
                  :backtrace '("FRAME-ONE" "FRAME-TWO")
                  :conversation-id "recover1"
                  :rendered-sequence 2
                  :history-floor-sequence 1
                  :git-commit "0123456789abcdef0123456789abcdef01234567"
                  :journal-position 17))
           (ensure-directories-exist pointer-pathname)
           (with-open-file (stream pointer-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-line (namestring capsule-pathname) stream))
           (sb-posix:setenv recovered-name "1" 1)
           (sb-posix:setenv pointer-name (namestring pointer-pathname) 1)
           (sb-posix:setenv session-name (namestring session-pathname) 1)
           (let ((prompt
                   (application-recovery-diagnosis-prompt configuration)))
             (test-assert
              (and prompt
                   (search "diagnosis-only" prompt)
                    (search "bounded read-only diagnostic tool rounds" prompt)
                    (search "workspace and source" prompt)
                   (search "Injected recovery failure" prompt)
                   (search "FRAME-ONE" prompt)
                   (search "ask" prompt)
                   (search "active image" prompt))
              "a valid recovered capsule requests diagnosis and user-approved repair"))
           (snapshot-write capsule-pathname '(:crash))
           (sb-posix:setenv conversation-name "untrusted-capsule" 1)
           (sb-posix:setenv sequence-name "99" 1)
           (sb-posix:setenv floor-name "98" 1)
           (multiple-value-bind (conversation-id rendered-sequence
                                history-floor-sequence)
               (application-recovery-state configuration)
             (test-assert
              (and (string= conversation-id "2345678")
                   (= rendered-sequence 4)
                   (= history-floor-sequence 2))
              "an invalid crash capsule falls back to validated session state"))
           (test-assert
            (null (application-recovery-diagnosis-prompt configuration))
            "an invalid crash capsule does not queue diagnosis")
           (snapshot-write
            session-pathname
            (list :recovery-session
                  :version 1
                  :version 1
                  :conversation-id "../bad"
                  :rendered-sequence 0))
           (multiple-value-bind (conversation-id rendered-sequence
                                history-floor-sequence)
               (application-recovery-state configuration)
             (test-assert
              (and (null conversation-id)
                   (null rendered-sequence)
                   (null history-floor-sequence))
              "malformed session state cannot replace invalid crash context"))
           (sb-posix:setenv pointer-name
                           (namestring
                            (merge-pathnames "outside.path" root))
                           1)
           (test-assert
            (null (application-recovery-diagnosis-prompt configuration))
            "diagnosis rejects a crash pointer outside private state"))
      (dolist (entry previous-environment)
        (if (rest entry)
            (sb-posix:setenv (first entry) (rest entry) 1)
            (sb-posix:unsetenv (first entry))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-application-construction () null)
(defun test-recovery-application-construction ()
  "Test source and retained recovery restore conversations and cursor state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (recovered
           (conversation-create configuration :identifier "recover-source"))
         (explicit
           (conversation-create configuration :identifier "explicit-resume"))
         (conversation-name "AUTOLITH_RECOVERY_CONVERSATION_ID")
         (sequence-name "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
         (floor-name "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")
         (environment-names
           (list conversation-name sequence-name floor-name))
         (previous-environment
           (loop for name in environment-names
                 collect (cons name (uiop:getenv name))))
         (mcp-registration-snapshot (mcp--registry-snapshot))
         (context-registration-snapshot (context--registry-snapshot))
         (command-registration-snapshot
           (application-command--registry-snapshot))
         (application nil))
    (labels ((publish-recovery-context ()
               "Publish RECOVERED's one-shot cursor handoff."
               (sb-posix:setenv
                conversation-name (conversation-identifier recovered) 1)
               (sb-posix:setenv sequence-name "2" 1)
               (sb-posix:setenv floor-name "1" 1))

             (close-application ()
               "Close APPLICATION's owned runtime and conversation lease."
               (when application
                 (ignore-errors
                   (application--discard-connection-resources
                    application
                    (application-tool-registry application)
                    (application-worker application)))
                 (ignore-errors
                   (application-release-conversation-lease application))
                 (setf application nil))))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (dotimes (index 3)
               (conversation-append-user-message
                recovered (format nil "recovery message ~D" index)))
             (conversation-append-user-message explicit "explicit message")
             (publish-recovery-context)
             (test-call-with-function-replacements
              (list
               (list 'durable-mutations-load
                     (lambda (active-configuration)
                       (declare (ignore active-configuration))
                       nil))
               (list 'image-state-load
                     (lambda (active-configuration)
                       (declare (ignore active-configuration))
                       nil))
               (list 'image-state-reconnect (lambda () nil))
               (list 'mcp-configuration-load
                     (lambda (active-configuration)
                       (declare (ignore active-configuration))
                       nil))
               (list 'user-init-load
                     (lambda (active-configuration)
                       (declare (ignore active-configuration))
                       nil))
               (list 'configuration-create
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       configuration)))
              (lambda ()
                (setf application (application-create configuration))
                (test-assert
                 (and
                  (string=
                   (conversation-identifier
                    (application-conversation application))
                   (conversation-identifier recovered))
                  (= (application-rendered-sequence application) 2)
                  (= (application-history-floor-sequence application) 1))
                 "clean-source recovery restores its conversation and cursors")
                (publish-recovery-context)
                (setf application
                      (application-reconnect
                       application
                       :conversation-id (conversation-identifier explicit)))
                (test-assert
                 (and
                  (string=
                   (conversation-identifier
                    (application-conversation application))
                   (conversation-identifier explicit))
                  (zerop (application-rendered-sequence application))
                  (null (application-history-floor-sequence application)))
                 "explicit resume overrides recovery conversation and cursor state")
                (publish-recovery-context)
                (setf application (application-reconnect application))
                (test-assert
                 (and
                  (string=
                   (conversation-identifier
                    (application-conversation application))
                   (conversation-identifier recovered))
                  (= (application-rendered-sequence application) 2)
                  (= (application-history-floor-sequence application) 1))
                 "retained-generation reconnect restores recovery state"))))
        (close-application)
        (dolist (entry previous-environment)
          (if (rest entry)
              (sb-posix:setenv (first entry) (rest entry) 1)
              (sb-posix:unsetenv (first entry))))
        (mcp--registry-restore mcp-registration-snapshot)
        (context--registry-restore context-registration-snapshot)
        (application-command--registry-restore command-registration-snapshot)
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)

(-> test-bounded-transcript-replay () null)
(defun test-bounded-transcript-replay ()
  "Test startup replays exactly the newest two hundred fifty visible entries."
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
           (test-assert (= *application-history-page-size* 250)
                        "the reloadable transcript display default is 250")
           (loop for index below 251
                 do (conversation-append-user-message
                     conversation
                     (format nil "visible-entry-~3,'0D" index)))
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (terminal-tests--substring-count "❯ you" output) 250)
              "startup emits no more than two hundred fifty visible entries")
             (test-assert
              (and (not (search "visible-entry-000" output))
                   (search "visible-entry-001" output)
                   (search "visible-entry-250" output))
              "startup retains exactly the newest two hundred fifty visible entries")
             (test-assert
              (search
               "showing 250 newest transcript entries; /history loads an earlier page"
               output)
              "bounded startup replay explains how to load omitted history"))
           (test-assert
            (= (application-rendered-sequence application) 251)
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
                (= (terminal-tests--substring-count "❯ you" output) 250)
                "recovery also bounds output after a stale durable cursor")
               (test-assert
                (and (not (search "visible-entry-001" output))
                     (search "visible-entry-002" output)
                     (search "one-live-append" output)
                     (search "showing 250 newest entries added after recovery"
                             output))
                "recovery retains only the newest unread transcript page"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-chunked-transcript-replay () null)
(defun test-chunked-transcript-replay ()
  "Test newest-first replay, one-segment fallback, history, and cursor rotation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "chunk-empty"))
                  (terminal (make-instance 'recording-terminal :columns 80))
                  (application
                    (make-instance
                     'application
                     :configuration configuration
                     :conversation conversation
                     :ui (terminal-ui-create :terminal terminal))))
             (application-render-records application)
             (test-assert
              (and (application-transcript-synchronized-p application)
                   (zerop (application-render-position application))
                   (zerop (application-rendered-sequence application))
                   (= (application-render-generation application)
                      (conversation-log-generation conversation))
                   (not
                    (conversation-storage-occupied-p
                     (conversation-pathname conversation))))
              "an unpersisted empty conversation synchronizes at the zero cursor")
             (conversation-append-user-message conversation "first durable record")
             (application-render-records application)
             (application-render-records application)
             (test-assert
              (and (= (terminal-tests--substring-count
                       "first durable record"
                       (recording-terminal-output terminal))
                      1)
                   (= (application-rendered-sequence application) 1)
                   (plusp (application-render-position application)))
              "the first later durable record renders exactly once"))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "chunk-page-full")))
             (conversation-append-user-message conversation "older-full-chunk")
             (conversation-append-summary conversation "full checkpoint")
             (conversation-append-user-message conversation "active-full-one")
             (conversation-append-user-message conversation "active-full-two")
             (let* ((loaded
                      (conversation-load (conversation-pathname conversation)))
                    (terminal (make-instance 'recording-terminal :columns 80))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation loaded
                       :ui (terminal-ui-create :terminal terminal)))
                    (mapped-pathnames nil)
                    (map-records-function
                      (symbol-function 'conversation--map-records)))
               (let ((*application-history-page-size* 3))
                 (test-call-with-function-replacements
                  (list
                   (list
                    'conversation--map-records
                    (lambda (pathname function &key (start-position 0))
                      (push pathname mapped-pathnames)
                      (funcall map-records-function
                               pathname
                               function
                               :start-position start-position))))
                  (lambda ()
                    (application-render-records application))))
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (equal (nreverse mapped-pathnames)
                              (list (conversation-log-pathname loaded)))
                       (not (search "older-full-chunk" output))
                       (search "active-full-one" output)
                       (search "active-full-two" output)
                       (= (terminal-tests--substring-count
                           "context compacted through sequence"
                           output)
                          1))
                  "a full active chunk is the only segment scanned at startup"))
               (recording-terminal-reset terminal)
               (conversation-append-summary loaded "rotated checkpoint")
               (conversation-append-user-message loaded "after-runtime-rotation")
               (application-render-records application)
               (application-render-records application)
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (= (terminal-tests--substring-count
                           "after-runtime-rotation" output)
                          1)
                       (= (terminal-tests--substring-count
                           "context compacted through sequence" output)
                          1)
                       (= (application-render-generation application)
                          (conversation-log-generation loaded))
                       (plusp (application-render-position application)))
                  "chunk rotation resets the active cursor and renders each new record once"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "chunk-page-short")))
             (conversation-append-user-message conversation "oldest-third-chunk")
             (conversation-append-summary conversation "middle checkpoint")
             (conversation-append-user-message conversation "middle-chunk-message")
             (conversation-append-summary conversation "active checkpoint")
             (let* ((loaded
                      (conversation-load (conversation-pathname conversation)))
                    (pathnames
                      (conversation-storage-pathnames
                       (conversation-pathname conversation)))
                    (active (third pathnames))
                    (previous (second pathnames))
                    (terminal (make-instance 'recording-terminal :columns 80))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation loaded
                       :ui (terminal-ui-create :terminal terminal)))
                    (mapped-pathnames nil)
                    (map-records-function
                      (symbol-function 'conversation--map-records)))
               (let ((*application-history-page-size* 3))
                 (test-call-with-function-replacements
                  (list
                   (list
                    'conversation--map-records
                    (lambda (pathname function &key (start-position 0))
                      (push pathname mapped-pathnames)
                      (funcall map-records-function
                               pathname
                               function
                               :start-position start-position))))
                  (lambda ()
                    (application-render-records application)))
                 (let ((output (recording-terminal-output terminal)))
                   (test-assert
                    (and (equal (nreverse mapped-pathnames)
                                (list active previous))
                         (not (search "oldest-third-chunk" output))
                         (search "middle-chunk-message" output)
                         (= (terminal-tests--substring-count
                             "context compacted through sequence" output)
                            2)
                         (not (search "/history loads an earlier page" output)))
                    "a short active chunk scans exactly one preceding segment"))
                 (let* ((recovery-terminal
                          (make-instance 'recording-terminal :columns 80))
                        (recovery-application
                          (make-instance
                           'application
                           :configuration configuration
                           :conversation loaded
                           :recovery-startup-p t
                           :ui
                           (terminal-ui-create :terminal recovery-terminal)))
                        (recovery-pathnames nil))
                   (let ((*application-history-page-size* 3))
                     (test-call-with-function-replacements
                      (list
                       (list
                        'conversation--map-records
                        (lambda (pathname function &key (start-position 0))
                          (push pathname recovery-pathnames)
                          (funcall map-records-function
                                   pathname
                                   function
                                   :start-position start-position))))
                      (lambda ()
                        (application-render-records recovery-application))))
                   (let ((output
                           (recording-terminal-output recovery-terminal)))
                     (test-assert
                      (and (equal (nreverse recovery-pathnames) (list active))
                           (not (search "middle-chunk-message" output))
                           (= (terminal-tests--substring-count
                               "context compacted through sequence" output)
                              1))
                      "zero-cursor recovery scans only the active chunk")))
                 (recording-terminal-reset terminal)
                 (application-render-history application)
                 (let ((output (recording-terminal-output terminal)))
                   (test-assert
                    (and (search "oldest-third-chunk" output)
                         (not (search "middle-chunk-message" output))
                         (= (terminal-tests--substring-count
                             "oldest-third-chunk" output)
                            1))
                    "explicit history crosses older chunk boundaries without duplicates")))))
           (let* ((conversation
                    (conversation-create
                     configuration
                     :identifier "chunk-invalid-previous"))
                  (terminal (make-instance 'recording-terminal :columns 80)))
             (conversation-append-user-message conversation "malformed older record")
             (conversation-append-summary conversation "active checkpoint")
             (let* ((pathnames
                      (conversation-storage-pathnames
                       (conversation-pathname conversation)))
                    (previous (first pathnames))
                    (forms (copy-tree (conversation--read-records previous)))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation
                       (conversation-load (conversation-pathname conversation))
                       :ui (terminal-ui-create :terminal terminal))))
               (setf (getf (rest (second forms)) :seq) 2)
               (conversation-identifier-migration--write-forms previous forms)
               (let ((*application-history-page-size* 3))
                 (test-assert
                  (handler-case
                      (progn
                        (application-render-records application)
                        nil)
                    (conversation-invariant-error ()
                      t))
                  "startup fallback validates the exact preceding segment")))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-compaction-presentation-lifecycle () null)
(defun test-compaction-presentation-lifecycle ()
  "Test compaction event presentation and every defensive cleanup boundary."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "compaction-ui"))
         (registry (make-instance 'tool-registry))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition 'simple-error
                             :format-control "manual compaction failed"
                             :format-arguments nil)
             (make-condition 'simple-error
                             :format-control "turn failed"
                             :format-arguments nil))))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker ':unused))
         (terminal (make-instance 'recording-terminal :columns 72))
         (ui (terminal-ui-create :terminal terminal
                                 :clock-function (lambda () 0)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :tool-registry registry
                          :agent agent
                          :ui ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (conversation-append-user-message conversation "history to compact")
           (let* ((observer (application-agent-observer application))
                  (send-status
                    (callback-agent-observer-status-callback observer)))
             (funcall send-status :compaction-started nil)
             (test-assert
              (and (terminal-ui-compacting-p ui)
                   (string= (terminal-ui-status ui)
                            "compacting the conversation"))
              "the compaction event enables its indicator and activity phase")
             (multiple-value-bind (text display cursor)
                 (terminal-ui--live-content ui 0)
               (declare (ignore display cursor))
               (test-assert (search "COMPACTING  [====>" text)
                            "the application observer projects compaction live"))
             (funcall send-status :compaction-completed nil)
             (test-assert (not (terminal-ui-compacting-p ui))
                          "completion removes the compaction indicator")
             (funcall send-status :compaction-started nil)
             (funcall send-status :turn-completed nil)
             (test-assert (and (not (terminal-ui-compacting-p ui))
                               (null (terminal-ui-status ui)))
                          "turn completion defensively clears compaction state"))
           (let ((failure nil))
             (handler-case
                 (application-compact application)
               (simple-error (condition)
                 (setf failure condition)))
             (test-assert failure
                          "manual compaction preserves its provider failure")
             (test-assert (and (not (terminal-ui-compacting-p ui))
                               (null (terminal-ui-compaction-started-at ui))
                               (null (terminal-ui-status ui)))
                          "manual compaction failure clears every live indicator"))
           (terminal-ui-set-compacting ui t)
           (let ((failure nil))
             (handler-case
                 (application--run-turn application "failing turn")
               (simple-error (condition)
                 (setf failure condition)))
             (test-assert failure
                          "turn execution preserves its provider failure")
             (test-assert (and (not (terminal-ui-compacting-p ui))
                               (null (terminal-ui-compaction-started-at ui))
                               (null (terminal-ui-status ui)))
                          "turn unwind cleanup removes stale compaction state")))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                                       "The quick brown fox jumps over the lazy dog~%final tail")))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-reasoning reasoning-prefix)
           (let* ((ui (application-ui application))
                  (preview (terminal-ui-preview-rows ui))
                  (preview-text
                    (format nil "~{~A~^~%~}"
                            (mapcar #'test-terminal-row-text preview))))
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
           (funcall send-text "The quick brown fox jumps over the lazy")
           (test-assert (null (terminal-ui-preview-rows
                              (application-ui application)))
                        "assistant output replaces the live reasoning preview")
           (test-assert
            (string= (terminal-ui-status (application-ui application))
                     "receiving response")
            "assistant streaming keeps a timed activity phase visible")
           (let* ((live-output (recording-terminal-output terminal))
                  (first-row (search "The quick brown fox" live-output))
                  (last-row (search "over the lazy" live-output)))
             (test-assert (and first-row last-row (< first-row last-row))
                          "an unfinished response paints every speculative wrapped row"))
           (funcall send-reasoning " late event")
           (test-assert (null (terminal-ui-preview-rows
                              (application-ui application)))
                        "late reasoning deltas cannot resurrect a finalized preview")
           (funcall send-text (format nil " dog~%"))
           (test-assert (null (terminal-ui-stream-tail
                               (application-ui application)))
                        "a newline with no following text leaves no blank live tail")
           (funcall send-text "final tail")
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
             (test-assert (search "final tail" completion)
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
                       (search "(self.eval :form" output)
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                     :turn-completion ':end))))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                     :turn-completion ':end)
                    (agent-test-result
                     "responsive-queued"
                     (list (agent-test-message "queued answer"))
                     :turn-completion ':end))))
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
                     (application-tests--conversation-records conversation))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                 ':apply)
                "goal mutations apply at the next serialized safe boundary")
               (test-assert
                (search "finish the migration" output)
                "/goal renders the active goal while a turn is running")
               (test-assert
                (not (search "command scheduled" output))
                "/goal is not scheduled until the active response finishes")
               (test-assert
                (zerop
                 (length
                  (line-editor-text (terminal-ui-editor ui))))
                "/goal does not return to the editor after inspection"))
              (recording-terminal-reset terminal)
              (let* ((invocation
                       (application-command-invocation-parse
                        "/timestamps sometimes"))
                     (command
                       (application-command-invocation-command invocation)))
                (test-assert
                 (eq (application-input-controller--run-responsive-command
                      controller command invocation)
                     ':failed)
                 "responsive command errors remain recoverable")
                (let* ((output (recording-terminal-output terminal))
                       (command-position
                         (search "/timestamps sometimes" output))
                       (error-position (search "✗ error" output)))
                  (test-assert
                   (and command-position
                        error-position
                        (< command-position error-position))
                   "responsive command errors retain their command heading"))))
        (when controller
          (application-input-controller-stop controller)))))
  nil)

(-> test-responsive-command-scheduling () null)
(defun test-responsive-command-scheduling ()
  "Test busy commands apply at safe boundaries, hold, or report immediately."
  (labels ((busy-action (input)
             "Return the registered busy action for command INPUT."
             (let* ((invocation (application-command-invocation-parse input))
                    (command
                      (application-command-invocation-command invocation)))
               (and command
                    (application-command-busy-action command invocation)))))
    (dolist (case '(("/model gpt-5.6-luna" :apply)
                    ("/effort high" :apply)
                    ("/mcp refresh" :apply)
                    ("/permissions full" :apply)
                    ("/hurry-up on" :apply)
                    ("/goal pause" :apply)
                    ("/mcp" :execute)
                    ("/hurry-up" :execute)
                    ("/model" :hold)
                    ("/effort" :hold)
                    ("/status" :execute)
                    ("/compact" :hold)
                    ("/resume" :hold)
                    ("/quit" :cancel)))
      (destructuring-bind (input expected) case
        (test-assert (eq (busy-action input) expected)
                     (format nil "busy ~A resolves to ~A" input expected)))))
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "responsive-apply"))
         (application (make-instance 'application
                                     :ui ui
                                     :configuration configuration
                                     :conversation conversation))
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
              controller "/goal pause")
             (let* ((output (recording-terminal-output terminal))
                    (command-position (search "/goal pause" output))
                    (apply-position (search "next safe point" output)))
               (test-assert
                (and command-position
                     apply-position
                     (< apply-position command-position))
                "a busy state change labels its boundary application notice")
               (test-assert
                (zerop (length (line-editor-text (terminal-ui-editor ui))))
                "a boundary-applying command does not return to the editor"))
             (application-input-controller--handle-submission
              controller "/compact")
             (test-assert
              (search "command scheduled"
                      (recording-terminal-output terminal))
              "a busy held command still labels its scheduling notice")
             (application-input-controller--handle-submission
              controller "/bogus-command")
             (test-assert
              (search "Unknown command"
                      (recording-terminal-output terminal))
              "an unknown busy command reports its error immediately")
             (test-assert
              (equal (application-input-controller--state controller :work-items)
                     (list (list ':command "/compact")))
              "only genuinely held commands wait behind the active turn")
             (test-assert
              (equal (deque->list
                      (application-input-controller-pending-apply-items
                       controller))
                     (list "/goal pause"))
              "state changes queue for the next safe boundary instead")
             (application-input-controller--apply-pending-commands controller)
             (test-assert
              (and (deque-empty-p
                    (application-input-controller-pending-apply-items
                     controller))
                   (eq (getf (application-goal application) :status) ':paused))
              "applying pending commands executes their effects")
             (application-input-controller--handle-submission
              controller "/goal resume")
             (application-input-controller--finish-work controller)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     (list ':apply-pending))
              "boundary commands missing their turn apply before follow-ups"))
        (when controller
          (application-input-controller-stop controller))
        (uiop:delete-directory-tree
         root :validate t :if-does-not-exist ':ignore))))
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
             (let* ((reader
                      (application-input-controller-reader-thread controller))
                    (reader-paused-p
                      (application-input-controller-call-with-reader-paused
                       controller
                       (lambda ()
                         (and (not (thread-alive-p reader))
                              (null
                               (application-input-controller-reader-thread
                                controller)))))))
               (test-assert
                reader-paused-p
                "pausing input removes and joins the competing terminal reader"))
             (test-assert
              (thread-alive-p
               (application-input-controller-reader-thread controller))
              "the terminal reader restarts after single-threaded work"))
        (application-input-controller-stop controller))))
  (test-assert
   (equal (application--initial-work-items nil nil t)
          (list (list ':project-adaptation-offer)))
   "explicit command-line resume queues interruptible adaptation work")
  (test-assert
   (equal (application--initial-work-items "(resume)" nil t)
          (list (list ':lisp "(resume)")))
   "bare command-line resume runs through canonical local Lisp")
  (test-assert
   (equal (application--initial-work-items nil "diagnose" nil)
          (list (list ':recovery-diagnosis "diagnose")))
   "automatic crash recovery queues exactly one diagnostic model turn")
  nil)

(-> test-recovery-diagnosis-tool-surface () null)
(defun test-recovery-diagnosis-tool-surface ()
  "Test recovered diagnosis work receives only the read-only native allowlist."
  (let* ((application (make-instance 'application))
         (controller
           (make-instance 'application-input-controller
                          :application application))
         (observed nil))
    (test-call-with-function-replacements
     (list
      (list
       'application--run-message-input
       (lambda (active-application input
                &key steering-function tools-p tool-allowlist
                     tool-restriction-p goal-continuations-p
                     fatal-agent-loop-errors-p &allow-other-keys)
         (declare (ignore steering-function))
         (setf observed
               (list active-application input tools-p tool-allowlist
                     tool-restriction-p goal-continuations-p
                     fatal-agent-loop-errors-p))
         ':continue)))
     (lambda ()
       (application-input-controller--run-work
        controller (list ':recovery-diagnosis "diagnose"))))
    (test-assert
     (and (eq (first observed) application)
          (string= (second observed) "diagnose")
          (third observed)
          (equal (fourth observed)
                 '("resource.read"
                   "search.files" "search.glob" "search.content"
                   "lisp.describe" "lisp.source" "self.status" "self.diff"
                   "self.generations"))
          (fifth observed)
          (null (sixth observed))
          (null (seventh observed)))
     "recovery diagnosis enables only its read-only tool surface"))
  nil)

(-> test-primary-prompt-admission () null)
(defun test-primary-prompt-admission ()
  "Test primary prompts share message steering and FIFO queue admission."
  (labels ((call-with-controller (function)
             (let* ((terminal
                      (make-instance 'waiting-recording-terminal :columns 60))
                    (ui (terminal-ui-create :terminal terminal))
                    (application (make-instance 'application :ui ui))
                    (controller nil))
               (with-terminal-ui (active-ui ui)
                 (declare (ignore active-ui))
                 (setf controller
                       (application-input-controller-create
                        application
                        :load-pending-p nil
                        :start-reader-p nil))
                 (unwind-protect
                      (funcall function controller)
                   (application-input-controller-stop controller))))))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue
        controller ':message "active message")
       (test-assert
        (equal (application-input-controller--next-work controller)
               '(:message "active message"))
        "the primary message becomes active")
       (let ((identifier
               (application-input-controller-active-work-identifier controller)))
         (test-assert
          (application-input-controller--acknowledge-active-work
           controller identifier)
          "the durable user message clears its pending active record"))
       (test-assert
        (and (null (application-input-controller-active-work controller))
             (eq (application-input-controller-active-work-kind controller)
                 ':message))
        "message kind survives durable active-work acknowledgment")
        (let* ((image
                 (merge-pathnames
                  (format nil "terminal-prompt-~A.png" (make-identifier))
                  (uiop:temporary-directory)))
               (input nil)
               (observed nil)
               (original-prompt (symbol-function 'prompt)))
          (unwind-protect
               (progn
                 (test-conversation--write-tiny-png image)
                 (setf input
                       (user-message-input-create
                        :text "[Image #1] steer active message"
                        :image-pathnames (list (truename image))))
                 (test-call-with-function-replacements
                  (list
                   (list 'prompt
                         (lambda (&rest arguments)
                           (setf observed
                                 (list arguments
                                       *prompt-primary-prefer-steering-p*))
                           (apply original-prompt arguments))))
                  (lambda ()
                    (application-input-controller--handle-submission
                     controller input :steer-p t)))
                  (let ((observed-input (first (first observed)))
                        (steering-input
                          (first (application-input-controller--state
                                  controller :steering-items))))
                   (test-assert
                    (and (typep observed-input 'user-message-input)
                         (string= (user-message-input-text observed-input)
                                  "[Image #1] steer active message")
                         (equal (user-message-input-image-pathnames observed-input)
                                (list (truename image)))
                         (second observed)
                         (string=
                          (user-message-input-text steering-input)
                          "[Image #1] steer active message")
                         (equal
                          (user-message-input-image-pathnames steering-input)
                          (list (truename image))))
                    "terminal image prose invokes canonical PROMPT with steering")))
            (ignore-errors (delete-file image))))))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue controller ':lisp "(+ 1 2)")
       (test-assert
        (equal (application-input-controller--next-work controller)
               '(:lisp "(+ 1 2)"))
        "local Lisp becomes active work")
       (let* ((ui
                (application-ui
                 (application-input-controller-application controller)))
              (observed nil)
              (original-prompt (symbol-function 'prompt)))
         (test-call-with-function-replacements
          (list
           (list 'prompt
                 (lambda (&rest arguments)
                   (setf observed
                         (list arguments *prompt-primary-prefer-steering-p*))
                   (apply original-prompt arguments))))
          (lambda ()
            (terminal-ui-set-input ui "ordinary during local Lisp")
            (application-input-controller--process-event controller ':complete)))
          (test-assert
           (and (equal (first observed) '("ordinary during local Lisp"))
                (null (second observed))
                (null (application-input-controller--state
                       controller :steering-items))
                (equal (application-input-controller--state controller :work-items)
                       '((:message "ordinary during local Lisp"))))
           "terminal prose invokes canonical PROMPT without message steering"))))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue
        controller ':message "older follow-up")
       (dolist (text '("first late steer" "second late steer"))
         (multiple-value-bind (accepted-p delivery)
             (application-input-controller-submit-primary-prompt controller text)
           (test-assert
            (and accepted-p (eq delivery ':queued))
            "late steering intent is accepted as queued work")))
       (test-assert
        (equal (application-input-controller--state controller :work-items)
               '((:message "first late steer")
                 (:message "second late steer")
                 (:message "older follow-up")))
        "late steering intent precedes older follow-ups without reversing FIFO")
       (test-assert
        (= (application-input-controller-steering-promotion-prefix-count controller)
           2)
        "promoted steering records its durable FIFO prefix")))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue controller ':lisp "(+ 3 4)")
       (application-input-controller--next-work controller)
       (deque-append (application-input-controller-work-items controller)
                     '((:message "older queued") (:message "newer queued")))
       (setf (application-input-controller-follow-up-edit-index controller) 1
             (application-input-controller-follow-up-edit-work controller)
             '(:message "recalled original"))
       (test-assert
        (application-input-controller--handle-recalled-submission
         controller "edited recalled")
        "recalled prose is handled during local Lisp")
       (test-assert
        (and (null (application-input-controller--state
                    controller :steering-items))
             (equal (application-input-controller--state controller :work-items)
                    '((:message "older queued")
                      (:message "edited recalled")
                      (:message "newer queued")))
             (null (application-input-controller-follow-up-edit-index controller))
             (null (application-input-controller-follow-up-edit-work controller)))
        "recalled prose preserves its FIFO slot instead of entering steering")))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue
        controller ':message "active message")
       (application-input-controller--next-work controller)
       (application-input-controller--enqueue
        controller ':message "older follow-up")
       (application-input-controller-submit-primary-prompt
        controller "older accepted steer")
       (application-input-controller--finish-work controller)
       (multiple-value-bind (accepted-p delivery)
           (application-input-controller-submit-primary-prompt
            controller "late accepted steer")
          (test-assert
           (and accepted-p
                (eq delivery ':queued)
                (equal (application-input-controller--state controller :work-items)
                       '((:message "older accepted steer")
                         (:message "late accepted steer")
                         (:message "older follow-up"))))
           "completion race keeps earlier steering ahead of a late prompt"))))
    (call-with-controller
     (lambda (controller)
       (setf (application-input-controller-localgroup-handoff-p controller) t)
       (multiple-value-bind (accepted-p delivery)
           (application-input-controller-submit-primary-prompt
            controller "handoff prompt")
          (test-assert
           (and (null accepted-p)
                (eq delivery ':rejected)
                (null (application-input-controller--state controller :work-items)))
           "localgroup handoff rejects new primary prompts"))
       (setf (application-input-controller-localgroup-handoff-p controller) nil
             (application-input-controller-stopping-p controller) t)
       (multiple-value-bind (accepted-p delivery)
           (application-input-controller-submit-primary-prompt
            controller "stopping prompt")
          (test-assert
           (and (null accepted-p)
                (eq delivery ':rejected)
                (null (application-input-controller--state controller :work-items)))
           "stopping rejects new primary prompts"))))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue
        controller ':message "initial active message")
       (application-input-controller--next-work controller)
       (application-input-controller-submit-primary-prompt
        controller "first promoted steer")
       (application-input-controller-submit-primary-prompt
        controller "second promoted steer")
       (application-input-controller--finish-work controller)
       (test-assert
        (= (application-input-controller-steering-promotion-prefix-count controller)
           2)
        "completion records both promoted steering messages")
       (application-input-controller--next-work controller)
       (test-assert
        (= (application-input-controller-steering-promotion-prefix-count controller)
           1)
        "dispatch consumes one promoted prefix slot")
       (test-assert
        (application-input-controller--recall-follow-up controller)
        "remaining promoted steering may be recalled while work is active")
       (application-input-controller--process-event controller ':interrupt)
       (test-assert
        (= (application-input-controller-steering-promotion-prefix-count controller)
           0)
        "discarding recalled promoted work removes its prefix slot")
       (application-input-controller--enqueue
        controller ':message "older follow-up")
       (application-input-controller--finish-work controller)
       (application-input-controller-submit-primary-prompt
        controller "late prompt after discard")
       (test-assert
        (equal (application-input-controller--state controller :work-items)
               '((:message "late prompt after discard")
                 (:message "older follow-up")))
        "discarded prefix leaves no phantom slot ahead of a late prompt")))
    (call-with-controller
     (lambda (controller)
       (application-input-controller--enqueue controller ':lisp "(+ 5 6)")
       (application-input-controller--next-work controller)
       (setf (application-input-controller-follow-up-edit-index controller) 0
             (application-input-controller-follow-up-edit-work controller)
             '(:message "recalled original")
             (application-input-controller-localgroup-handoff-p controller) t)
       (let ((ui
               (application-ui
                (application-input-controller-application controller))))
         (terminal-ui-set-input ui "recalled during handoff")
         (application-input-controller--process-event controller ':complete)
         (test-assert
          (and (eql (application-input-controller-follow-up-edit-index controller)
                    0)
               (equal (application-input-controller-follow-up-edit-work controller)
                      '(:message "recalled original"))
               (null (application-input-controller--state controller :work-items)))
          "terminal event keeps recalled prose selected during handoff")
         (setf (application-input-controller-localgroup-handoff-p controller) nil
               (application-input-controller-stopping-p controller) t)
         (terminal-ui-set-input ui "recalled while stopping")
         (application-input-controller--process-event controller ':complete)
         (test-assert
          (and (eql (application-input-controller-follow-up-edit-index controller)
                    0)
               (equal (application-input-controller-follow-up-edit-work controller)
                      '(:message "recalled original"))
               (null (application-input-controller--state controller :work-items)))
          "terminal event keeps recalled prose selected while stopping"))))
    (call-with-controller
     (lambda (controller)
       (setf (application-input-controller-pending-persistence-enabled-p controller)
             nil)
       (multiple-value-bind (accepted-p delivery)
           (application-input-controller-submit-primary-prompt
            controller "/vault")
          (test-assert
           (and (null accepted-p)
                (eq delivery ':rejected)
                (null (application-input-controller--state
                       controller :steering-items))
                (null (application-input-controller--state controller :work-items)))
           "callable prompts cannot bypass unavailable recovery-vault storage"))))
    nil))

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
             (application-input-controller-submit-primary-prompt
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
         (image (merge-pathnames "follow-up-cycle.png"
                                 (uiop:temporary-directory)))
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
              controller ':message "middle queued thought")
             (application-input-controller--enqueue
              controller ':message "newest queued thought")
             (application-input-controller--process-event
              controller ':complete-previous)
             (application-input-controller--process-event controller ':complete)
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "newest queued thought")
              "empty Tab recalls the newest queued follow-up")
             (application-input-controller--enqueue
              controller ':message "arrived while editing")
             (terminal-ui-set-input ui "edited newest thought")
             (application-input-controller--process-event
              controller ':complete-previous)
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "middle queued thought")
              "shift-tab moves to the immediately older follow-up")
             (terminal-ui-set-input ui "/status")
             (application-input-controller--process-event
              controller ':complete-previous)
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "first queued thought")
              "repeated shift-tab continues toward older follow-ups")
             (terminal-ui-set-input ui "edited first thought")
             (application-input-controller--process-event
              controller ':complete-previous)
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "arrived while editing")
              "shift-tab wraps from the oldest follow-up to the newest")
             (application-input-controller--process-event
              controller ':complete-previous)
             (test-assert
              (string= (line-editor-text (terminal-ui-editor ui))
                       "edited newest thought")
              "shift-tab continues backward after wrapping")
             (terminal-ui-set-input
              ui
              (user-message-input-create
               :text "[Image #1] edited newest thought"
               :image-pathnames (list image)))
             (application-input-controller--process-event
              controller ':complete-previous)
             (let ((queued
                     (application-input-controller--state controller :work-items)))
               (test-assert
                (and (equal (first queued)
                            '(:message "edited first thought"))
                     (typep (second (second queued)) 'user-message-input)
                     (equal
                      (user-message-input-image-pathnames
                       (second (second queued)))
                      (list image))
                     (equal (third queued)
                            '(:message "arrived while editing")))
                "cycling preserves image follow-ups at their FIFO position"))
             (terminal-ui-set-input ui "edited command")
             (application-input-controller--process-event
              controller ':complete-previous)
             (terminal-ui-set-input ui "edited first again")
             (application-input-controller--process-event controller ':complete)
             (test-assert
              (and
               (equal (first (application-input-controller--state
                              controller :work-items))
                      '(:message "edited first again"))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "Tab returns the edited follow-up to its original FIFO position")
             (application-input-controller--process-event controller ':complete)
             (line-editor-clear (terminal-ui-editor ui))
             (let ((before
                     (application-input-controller--state controller :work-items)))
               (application-input-controller--process-event controller ':complete)
               (test-assert
                (and
                 (equal (application-input-controller--state controller :work-items)
                        before)
                 (equal
                  (application-input-controller-follow-up-edit-work controller)
                  '(:message "arrived while editing")))
                "empty Tab keeps a blank recalled follow-up selected")
               (application-input-controller--process-event
                controller ':complete-previous)
               (test-assert
                (and
                 (equal (application-input-controller--state controller :work-items)
                        before)
                 (equal
                  (application-input-controller-follow-up-edit-work controller)
                  '(:message "arrived while editing")))
                "blank recalled drafts do not remove another follow-up"))
             (application-input-controller--finish-work controller)
             (test-assert
              (application-input-controller--follow-up-editing-p controller)
              "finishing the active turn preserves recalled follow-up selection"))
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
              controller ':message "older follow-up")
             (application-input-controller--enqueue
              controller ':message "newer follow-up")
             (application-input-controller--process-event controller ':complete)
             (terminal-ui-set-input ui "edited newer follow-up")
             (application-input-controller--finish-work controller)
             (application-input-controller--process-event controller ':complete)
             (test-assert
              (and
               (equal (application-input-controller--state controller :work-items)
                      '((:message "older follow-up")
                        (:message "edited newer follow-up")))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "Tab after turn completion restores the recalled FIFO position"))
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
              controller ':message "older follow-up")
             (application-input-controller--enqueue
              controller ':message "selected message")
             (application-input-controller--process-event controller ':complete)
             (terminal-ui-set-input ui "edited selected message")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (and
               (equal (application-input-controller--state controller :steering-items)
                      '("edited selected message"))
               (equal (application-input-controller--state controller :work-items)
                      '((:message "older follow-up")))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "recalled Enter messages use active-turn steering policy")
             (application-input-controller--enqueue
              controller ':command "/goal pause")
             (application-input-controller--process-event controller ':complete)
             (terminal-ui-set-input ui "/goal pause")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (and
               (equal (application-input-controller--state controller :work-items)
                      '((:message "older follow-up")))
               (equal (deque->list
                       (application-input-controller-pending-apply-items
                        controller))
                      '("/goal pause"))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "recalled Enter commands use active-turn busy policy"))
        (when controller
          (application-input-controller-stop controller)))))
  (let* ((terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller nil)
         (waiter nil)
         (waiter-started-p nil)
         (waited-work ':waiting))
    (with-terminal-ui (active-ui ui)
      (declare (ignore active-ui))
      (setf controller (application-input-controller-create application))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':message "active turn")
             (application-input-controller--next-work controller)
             (application-input-controller--enqueue
              controller ':message "older follow-up")
             (application-input-controller--enqueue
              controller ':message "held follow-up")
             (application-input-controller--process-event controller ':complete)
             (line-editor-clear (terminal-ui-editor ui))
             (application-input-controller--process-event controller ':complete)
             (test-assert
              (equal
               (application-input-controller-follow-up-edit-work controller)
               '(:message "held follow-up"))
              "blank Tab does not replace the held follow-up")
             (application-input-controller--finish-work controller)
             (test-assert
              (= (application-input-controller-follow-up-edit-index controller) 1)
              "turn completion preserves the held virtual FIFO index")
             (test-assert
              (equal (application-input-controller--next-work controller)
                     '(:message "older follow-up"))
              "work older than the held follow-up remains runnable")
             (test-assert
              (zerop
               (application-input-controller-follow-up-edit-index controller))
              "consuming older work advances the held follow-up to the FIFO head")
             (application-input-controller--finish-work controller)
             (application-input-controller--enqueue
              controller ':message "later follow-up")
             (setf waiter
                   (make-thread
                    (lambda ()
                      (setf waiter-started-p t)
                      (setf waited-work
                            (application-input-controller--next-work controller)))
                    :name "Autolith held follow-up FIFO test"))
             (test-assert
              (task-tests--wait-until (lambda () waiter-started-p) 2)
              "the FIFO waiter starts before the blocking assertion")
             (test-assert
              (eq waited-work ':waiting)
              "a held FIFO head blocks newer queued work")
             (application-input-controller--process-event controller ':interrupt)
             (test-assert
              (task-tests--wait-until
               (lambda () (not (thread-alive-p waiter)))
               2)
              "Ctrl-C wakes the blocked FIFO consumer")
             (join-thread waiter)
             (setf waiter nil)
             (test-assert
              (and
               (equal waited-work '(:message "later follow-up"))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "Ctrl-C discards the held follow-up and unblocks newer work"))
        (when (and waiter (thread-alive-p waiter))
          (when controller
            (application-input-controller-stop controller)
            (setf controller nil))
          (unless
              (task-tests--wait-until
               (lambda () (not (thread-alive-p waiter)))
               2)
            (destroy-thread waiter)))
        (when waiter
          (join-thread waiter))
        (when controller
          (application-input-controller-stop controller)))))
  nil)

(-> test-conversation-picker () null)
(defun test-conversation-picker ()
  "Test saved-conversation picker items and interactive selection."
  (let ((observed-initial-command nil)
        (*active-application* nil))
    (test-call-with-function-replacements
     (list
      (list
       'application-create
       (lambda (configuration &key conversation-id permission-mode)
         (declare (ignore configuration conversation-id permission-mode))
         (make-instance 'application)))
      (list
       'application-run
       (lambda (application &key initial-command &allow-other-keys)
         (declare (ignore application))
         (setf observed-initial-command initial-command))))
     (lambda ()
       (main-dispatch '("resume"))))
    (test-assert
     (string= observed-initial-command "(resume)")
     "plain command-line resume starts the canonical Lisp operation"))
  (test-assert
   (string= (application--selected-conversation-id
             "explicit" "recovered")
            "explicit")
   "an explicit resume identifier overrides automatic recovery selection")
  (test-assert
   (string= (application--selected-conversation-id nil "recovered")
            "recovered")
   "automatic recovery supplies the conversation without an explicit resume")
  (let ((observed-mode nil)
        (*active-application* nil))
    (test-call-with-function-replacements
     (list
      (list
       'application-create
       (lambda (configuration &key conversation-id permission-mode)
         (declare (ignore configuration conversation-id))
         (make-instance 'application :permission-mode permission-mode)))
      (list
       'application-run
       (lambda (application &rest arguments)
         (declare (ignore arguments))
         (setf observed-mode (application-permission-mode application)))))
     (lambda ()
       (main-dispatch '("--permissions" "sandbox"))))
    (test-assert (eq observed-mode ':sandboxed)
                 "--permissions sets the initial application mode"))
  (let ((observed-mode nil)
        (*active-application* (make-instance 'application)))
    (test-call-with-function-replacements
     (list
      (list
       'application-reconnect
       (lambda (application &key conversation-id immutable-p permission-mode)
         (declare (ignore application conversation-id immutable-p))
         (make-instance 'application :permission-mode permission-mode)))
      (list
       'application-run
       (lambda (application &rest arguments)
         (declare (ignore arguments))
         (setf observed-mode (application-permission-mode application)))))
     (lambda ()
       (main-dispatch '("--permissions" "full"))))
    (test-assert (eq observed-mode ':full-access)
                 "--permissions sets the initial reconnect mode"))
  (let* ((environment-names
           '("AUTOLITH_RECOVERY_CONVERSATION_ID"
             "AUTOLITH_RECOVERY_RENDERED_SEQUENCE"
             "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE"))
         (previous-environment
           (loop for name in environment-names
                 collect (cons name (uiop:getenv name))))
         (observed-conversation-id :unset)
         (observed-initial-command :unset)
         (observed-recovery-diagnosis :unset)
         (observed-resume-offer-p :unset)
         (reconnect-failure-p nil)
         (*active-application* (make-instance 'application)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'application-reconnect
            (lambda (application
                     &key conversation-id immutable-p permission-mode)
              (declare (ignore application immutable-p permission-mode))
              (when reconnect-failure-p
                (error "reconnect failed"))
              (setf observed-conversation-id conversation-id)
              (make-instance 'application)))
           (list
            'application-recovery-diagnosis-prompt
            (lambda (configuration)
              (declare (ignore configuration))
              "diagnose"))
           (list
            'application-run
            (lambda (application
                     &key initial-command initial-input recovery-diagnosis
                          resume-offer-p)
              (declare (ignore application initial-input))
              (setf observed-initial-command initial-command
                    observed-recovery-diagnosis recovery-diagnosis
                    observed-resume-offer-p resume-offer-p))))
          (lambda ()
            (sb-posix:setenv "AUTOLITH_RECOVERY_CONVERSATION_ID"
                            "recovered" 1)
            (main-dispatch '("resume"))
            (test-assert
             (and (null observed-conversation-id)
                  (null observed-initial-command)
                  (string= observed-recovery-diagnosis "diagnose")
                  (null observed-resume-offer-p))
             "automatic recovery ignores a replayed bare resume picker request")
            (test-assert
             (null (uiop:getenv "AUTOLITH_RECOVERY_CONVERSATION_ID"))
             "successful startup consumes one-shot recovery metadata")
            (setf observed-conversation-id :unset
                  observed-initial-command :unset
                  observed-recovery-diagnosis :unset
                  observed-resume-offer-p :unset)
            (sb-posix:setenv "AUTOLITH_RECOVERY_CONVERSATION_ID"
                            "recovered" 1)
            (main-dispatch '("resume" "explicit"))
            (test-assert
             (and (string= observed-conversation-id "explicit")
                  (null observed-initial-command)
                  (null observed-recovery-diagnosis)
                  observed-resume-offer-p)
             "an explicit recovered resume still overrides crash reconnection")
            (setf reconnect-failure-p t)
            (sb-posix:setenv "AUTOLITH_RECOVERY_CONVERSATION_ID"
                            "recovered" 1)
            (test-assert
             (handler-case
                 (progn
                   (main-dispatch '("resume"))
                   nil)
               (simple-error ()
                 t))
             "failed recovery application construction reports its error")
            (test-assert
             (string= (uiop:getenv "AUTOLITH_RECOVERY_CONVERSATION_ID")
                      "recovered")
             "failed recovery application construction preserves recovery metadata")))
      (dolist (entry previous-environment)
        (if (rest entry)
            (sb-posix:setenv (first entry) (rest entry) 1)
            (sb-posix:unsetenv (first entry))))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (image (merge-pathnames "initial.png" root)))
    (unwind-protect
         (progn
           (test-conversation--write-tiny-png image)
           (let ((input
                   (main--initial-image-input
                    (list (namestring image)))))
             (test-assert
              (and (typep input 'user-message-input)
                   (string= (user-message-input-text input) "[Image #1]")
                   (equal (user-message-input-image-pathnames input)
                          (list (truename image))))
              "command-line images preload a labelled composer draft")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                          (namestring (conversation-log-pathname conversation))
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
             (test-assert
              (every (lambda (item)
                       (string= (getf item :tally) "1 turn"))
                     items)
              "resume items tally conversations without measured work by turns")
             (let* ((item (first items))
                    (description (getf item :description))
                    (spans (getf item :description-spans))
                    (time-span
                      (find ':timestamp-time spans
                            :key #'terminal-span-style
                            :test #'eq)))
               (test-assert
                (and (terminal-styled-text-p spans)
                     (string= description
                              (terminal-ui--raw-spans-text spans)))
                "resume styled descriptions preserve their plain text")
               (test-assert
                (and time-span
                     (plusp (length (terminal-span-text time-span))))
                "resume descriptions color the time independently"))
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
             (recording-terminal-reset terminal)
             (setf (scripted-terminal-events terminal)
                   (list '(:insert "d") :escape))
             (test-assert (null (application--pick-conversation application))
                          "escape cancels after refusing active deletion")
             (test-assert
               (and (conversation-storage-occupied-p
                     (conversation-pathname active))
                   (search "Cannot delete the active conversation."
                           (recording-terminal-output terminal)))
              "the resume picker refuses to delete its active conversation")
             (let* ((failed
                      (conversation-create
                       configuration
                       :identifier "cleanup-failure"))
                    (failed-image-root
                      (merge-pathnames
                       "conversation-images/cleanup-failure/"
                       (configuration-data-root configuration)))
                    (unix-time
                      (- (get-universal-time)
                         *unix-epoch-universal-time*)))
               (conversation-append-user-message failed "temporary")
                (sb-posix:utime
                 (namestring (conversation-log-pathname failed))
                 (- unix-time 25)
                 (- unix-time 25))
               (snapshot-write
                (merge-pathnames "image.sexp" failed-image-root)
                '(:image))
               (recording-terminal-reset terminal)
               (setf (scripted-terminal-events terminal)
                     (list :history-next '(:insert "d") :submit :escape))
               (let ((*conversation-delete-directory-tree-function*
                       (lambda (root &key validate if-does-not-exist)
                         (declare (ignore root validate if-does-not-exist))
                         (error "simulated cleanup failure"))))
                 (test-assert
                  (null (application--pick-conversation application))
                  "cleanup failure returns to browsing until escape"))
               (test-assert
                 (and (not (conversation-storage-occupied-p
                            (conversation-pathname failed)))
                     (probe-file failed-image-root)
                     (search "was deleted, but private artifacts remain"
                             (recording-terminal-output terminal)))
                "the resume picker reports cleanup failure after committed deletion"))
             (setf (scripted-terminal-events terminal)
                   (list :history-next
                         '(:insert "d")
                         :history-next
                         :submit
                         :escape))
             (test-assert (null (application--pick-conversation application))
                          "keeping a conversation returns to the browse picker")
              (test-assert
               (conversation-storage-occupied-p
                (conversation-pathname current-older))
               "the keep confirmation preserves the conversation")
             (let ((image-root
                     (merge-pathnames "conversation-images/current-older/"
                                      (configuration-data-root configuration)))
                   (task-root
                     (merge-pathnames "tasks/current-older/"
                                      (configuration-data-root configuration))))
               (snapshot-write (merge-pathnames "image.sexp" image-root)
                               '(:image))
               (snapshot-write (merge-pathnames "task/result.sexp" task-root)
                               '(:task))
               (recording-terminal-reset terminal)
               (setf (scripted-terminal-events terminal)
                     (list :history-next '(:insert "d") :submit :escape))
               (test-assert (null (application--pick-conversation application))
                            "deletion returns to browsing until escape")
               (test-assert
                 (and (not (conversation-storage-occupied-p
                            (conversation-pathname current-older)))
                     (not (probe-file image-root))
                     (not (probe-file task-root)))
                "confirmed picker deletion removes the conversation and artifacts")
               (test-assert
                (search "Deleted conversation current-older."
                        (recording-terminal-output terminal))
                "confirmed picker deletion reports the removed conversation"))
             (terminal-ui-stop (application-ui application))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
         (old-workspace (merge-pathnames "old workspace/" root))
         (observation-path
           (merge-pathnames "directory-init-observation.sexp" workspace))
         (old-observation-path
           (merge-pathnames "directory-init-observation.sexp" old-workspace))
         (broken-workspace (merge-pathnames "broken workspace/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (extension-registry-snapshot
           (application--extension-registry-snapshot))
         (pool nil)
         (closable-application nil))
    (ensure-directories-exist workspace)
    (ensure-directories-exist old-workspace)
    (ensure-directories-exist broken-workspace)
    (ensure-directories-exist (configuration-directory-init-path workspace))
    (with-open-file (stream (configuration-directory-init-path workspace)
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (let ((*print-readably* nil))
        (write
         '(with-open-file
              (stream "directory-init-observation.sexp"
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :external-format :utf-8)
            (write
             (list (namestring (truename (uiop:getcwd)))
                   (namestring (truename *default-pathname-defaults*)))
             :stream stream))
         :stream stream)))
    (test-directory-configuration--write-manifest
     configuration
     (list (namestring workspace)))
    (test-directory-configuration--write-mcp
     workspace
     (list
      (test-mcp-configuration--server-form
       :name "workspace-scoped"
       :approval ':deny)))
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
                 closable-application application
                 worker (lisp-worker-pool-start pool "workspace" "pristine"))
           (lisp-worker-request
            worker :eval '(:form "(defparameter *workspace-marker* 73)"))
            (uiop:chdir old-workspace)
            (setf *default-pathname-defaults* old-workspace)
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
              (probe-file observation-path)
              "directory initialization resolves relative files in the target workspace")
             (test-assert
              (not (probe-file old-observation-path))
              "directory initialization does not resolve relative files in the old workspace")
              (let ((observation
                      (with-open-file (stream observation-path
                                               :direction ':input
                                               :external-format ':utf-8)
                        (let ((*read-eval* nil))
                          (read stream)))))
                (test-assert
                 (equal observation
                        (list (namestring (truename workspace))
                              (namestring (truename workspace))))
                 "directory initialization observes the target process and pathname directories"))
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
              "workspace switching reconnects the provider with the new directory")
              (test-assert
               (eq
                (mcp-server-registration-source
                 (test-directory-configuration--registration
                  "workspace-scoped"))
                :directory)
               "workspace switching activates inherited MCP configuration"))
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
                          "invalid workspace changes retain the process directory")
              (test-directory-configuration--write-manifest
               configuration
               (list (namestring workspace)
                     (namestring broken-workspace)))
              (test-mcp-configuration--write
               (configuration-directory-mcp-path broken-workspace)
               "(:version 99 :servers ())")
              (test-assert
               (handler-case
                   (progn
                     (application-set-working-directory
                      application broken-workspace)
                     nil)
                 (working-directory-error (condition)
                   (eq (working-directory-error-stage condition) ':tools)))
               "malformed inherited MCP prevents a workspace switch")
              (test-assert
               (and
                (eq (application-configuration application)
                    active-configuration)
                (equal (uiop:getcwd) (truename workspace))
                (eq
                 (mcp-server-registration-source
                  (test-directory-configuration--registration
                   "workspace-scoped"))
                 :directory))
               "failed inherited configuration restores the prior runtime")))
      (when pool
        (lisp-worker-pool-stop-all pool))
      ;; A workspace switch installs a replacement registry owning task
      ;; threads. Leaving them alive breaks a later test that forks.
      (when closable-application
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry closable-application))))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (application--extension-registry-restore extension-registry-snapshot)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                    (application-tests--conversation-records target)))
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
                  (equal
                   before
                   (application-tests--conversation-records target))
                  "busy resume does not append interrupted-call repair")))))
      (when application
        (application-release-conversation-lease application))
      (when (and current-lease
                 (conversation-lease-held-p current-lease))
        (conversation-lease-release current-lease))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-fresh-conversation-lease-collision () null)
(defun test-application-fresh-conversation-lease-collision ()
  "Test fresh application ownership probes another seed after a lease race."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (timestamp 3992846378)
         (first-identifier
           (identifier-from-seed timestamp 0))
         (second-identifier
           (identifier-from-seed timestamp 1)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-conversation--call-with-child-lease
            configuration
            first-identifier
            (lambda ()
              (let ((*random-index-function*
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
                           (conversation-storage-occupied-p
                            (conversation-pathname conversation))))
                         "fresh ownership skips a leased empty identifier")
                    (conversation-lease-release lease)))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                      (job-pool-lifecycle-state (task-orchestrator-pool old-orchestrator))
                      ':closed)
                     (eq
                      (job-pool-lifecycle-state (task-orchestrator-pool new-orchestrator))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
         (workspace-resume-count 0)
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
                     (lambda () (incf workspace-close-count))
                     :resume-function
                     (lambda () (incf workspace-resume-count)))))
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
                      (job-pool-lifecycle-state (task-orchestrator-pool old-orchestrator))
                      ':closed)
                     (eq
                      (job-pool-lifecycle-state
                       (task-orchestrator-pool workspace-orchestrator))
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
                   (= workspace-close-count 1)
                   (= workspace-resume-count 1)
                   (eq
                    (job-pool-lifecycle-state
                     (task-orchestrator-pool workspace-orchestrator))
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
                   (= workspace-close-count 2)
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
                    (job-pool-lifecycle-state
                     (task-orchestrator-pool workspace-orchestrator))
                    ':closed)
                   (eq
                    (job-pool-lifecycle-state
                     (task-orchestrator-pool conversation-orchestrator))
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
                      (job-pool-lifecycle-state
                       (task-orchestrator-pool conversation-orchestrator))
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
       root :validate t :if-does-not-exist ':ignore)))
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
                 (job-pool-lifecycle-state (task-orchestrator-pool old-orchestrator))
                 ':open)
                (if (eq operation ':working-directory)
                    ;; Workspace switches quiesce the old runtime before
                    ;; preparing a replacement, so a retirement failure never
                    ;; creates or closes the candidate registry.
                    (and
                     (eq
                      (job-pool-lifecycle-state (task-orchestrator-pool new-orchestrator))
                      ':open)
                     (zerop new-close-count))
                    (and
                     (eq
                      (job-pool-lifecycle-state (task-orchestrator-pool new-orchestrator))
                      ':closed)
                     (= new-close-count 1))))
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
         root :validate t :if-does-not-exist ':ignore))))
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
       root :validate t :if-does-not-exist ':ignore)))
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
                                    reasoning-effort immutable-p
                                    defer-provider-validation-p)
                         (declare (ignore source-root
                                    defer-provider-validation-p))
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
                       (job-pool-lifecycle-state (task-orchestrator-pool old-orchestrator))
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
                         (job-pool-lifecycle-state
                          (task-orchestrator-pool old-orchestrator))
                         ':open)
                        (eq
                         (job-pool-lifecycle-state
                          (task-orchestrator-pool new-orchestrator))
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
               root :validate t :if-does-not-exist ':ignore)))))
    (run-case ':pre-commit)
    (run-case ':retirement))
  nil)

(-> test-application-task-presentation () null)
(defun test-application-task-presentation ()
  "Test task events drive one reconnectable child-agent terminal projection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration :identifier "task-presentation-primary-window"))
         (registry
           (task-augment-tool-registry
            (make-default-tool-registry)))
         (run-tool (tool-registry-find registry "task" "run"))
         (orchestrator (task-run-tool-orchestrator run-tool))
         (clock 65)
         (terminal
           (make-instance 'recording-terminal :columns 72 :styled-p t))
         (ui
           (terminal-ui-create
            :terminal terminal
            :clock-function (lambda () clock)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
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
         (running nil)
         (replacement-registry nil)
         (replacement-orchestrator nil))
    (unwind-protect
         (progn
           (application-connect-task-presentation application)
           (application-connect-task-presentation application)
           (test-assert
            (= (length (task-orchestrator-listeners orchestrator)) 1)
            "reconnection retains exactly one task presentation listener")
           (let* ((original-add-listener
                    (symbol-function 'task-orchestrator-add-listener))
                  (gate-lock
                    (make-lock "Autolith task presentation connect race"))
                  (gate-condition (make-condition-variable))
                  (first-add-entered-p nil)
                  (release-first-add-p nil)
                  (second-started-p nil)
                  (add-call-count 0)
                  (first-condition nil)
                  (second-condition nil)
                  (first-thread nil)
                  (second-thread nil))
             (test-call-with-function-replacements
              (list
               (list
                'task-orchestrator-add-listener
                (lambda (target listener)
                  (let ((block-p nil))
                    (with-lock-held (gate-lock)
                      (incf add-call-count)
                      (unless first-add-entered-p
                        (setf first-add-entered-p t
                              block-p t)
                        (task--condition-broadcast gate-condition)))
                    (when block-p
                      (with-lock-held (gate-lock)
                        (loop until release-first-add-p
                              unless
                              (condition-wait
                               gate-condition gate-lock :timeout 2)
                                do (error
                                    "Timed out holding the first presentation listener add."))))
                     (funcall original-add-listener target listener)))))
              (lambda ()
                (unwind-protect
                     (progn
                       (setf first-thread
                             (make-thread
                              (lambda ()
                                (handler-case
                                    (application-connect-task-presentation
                                     application)
                                  (condition (condition)
                                    (setf first-condition condition))))
                              :name "Autolith first task presentation connect"))
                       (with-lock-held (gate-lock)
                         (loop until first-add-entered-p
                               unless
                               (condition-wait
                                gate-condition gate-lock :timeout 2)
                                 do (error
                                     "Timed out waiting for the first listener add.")))
                       (let ((acquired-p
                               (bordeaux-threads:acquire-lock
                                (application-task-presentation-lock application)
                                nil)))
                         (when acquired-p
                           (bordeaux-threads:release-lock
                            (application-task-presentation-lock application)))
                         (test-assert
                          (not acquired-p)
                          "listener registration remains inside the presentation transaction"))
                       (setf second-thread
                             (make-thread
                              (lambda ()
                                (with-lock-held (gate-lock)
                                  (setf second-started-p t)
                                  (task--condition-broadcast gate-condition))
                                (handler-case
                                    (application-connect-task-presentation
                                     application)
                                  (condition (condition)
                                    (setf second-condition condition))))
                              :name "Autolith second task presentation connect"))
                       (with-lock-held (gate-lock)
                         (loop until second-started-p
                               unless
                               (condition-wait
                                gate-condition gate-lock :timeout 2)
                                 do (error
                                     "Timed out starting the second listener connect."))
                         (setf release-first-add-p t)
                         (task--condition-broadcast gate-condition))
                       (join-thread first-thread)
                       (setf first-thread nil)
                       (join-thread second-thread)
                       (setf second-thread nil)
                       (let ((listeners
                               (task-orchestrator-listeners orchestrator)))
                         (test-assert
                          (and (null first-condition)
                               (null second-condition)
                               (= add-call-count 2)
                               (= (length listeners) 1)
                               (member
                                (application-task-presentation-listener
                                 application)
                                listeners
                                :test #'eq))
                          "overlapping reconnects retain one exact active listener")))
                  (with-lock-held (gate-lock)
                    (setf release-first-add-p t)
                    (task--condition-broadcast gate-condition))
                  (when first-thread
                    (ignore-errors (join-thread first-thread)))
                  (when second-thread
                     (ignore-errors (join-thread second-thread)))))))
           (setf queued
                 (task-tests--register-job
                  orchestrator primary definition :name "queued-agent")
                 running
                 (task-tests--register-job
                  orchestrator primary definition :name "running-agent"))
           (with-lock-held ((cl-jobpond::job--lock running))
             (setf (job-state running) ':running)
             (task-job--set-progress-state running ':running))
           (task-progress-note-status
            running ':tool-call-started
            (list :tool "search.content"))
           (let* ((progress (task-job-progress running))
                  (now (get-internal-real-time)))
             (with-lock-held ((task-progress-lock progress))
               (setf (task-progress-started-at progress)
                     (- now (* 65 internal-time-units-per-second))
                     (task-progress-current-tool-started-at progress)
                     (- now (* 60 internal-time-units-per-second)))))
           (task-orchestrator-emit
            orchestrator
            ':task-subagent-progress
            (list :id (job-identifier running)))
           (let ((activities (terminal-ui-agent-activities ui)))
             (test-assert
              (and (= (length activities) 2)
                   (eq (getf (first activities) :state) ':queued)
                   (eq (getf (second activities) :state) ':running)
                   (string= (getf (second activities) :current-tool)
                            "search.content")
                   (typep (getf (second activities) :current-tool-duration-ms)
                          '(integer 60000))
                   (typep (getf (second activities) :duration-ms)
                          '(integer 65000)))
              "one progress event projects every queued and running child"))
           (multiple-value-bind (text display cursor)
               (terminal-ui--live-content ui clock)
             (declare (ignore display cursor))
             (test-assert
              (search "search.content 01:00" text)
              "runtime durations render correctly through an injected UI clock"))
           (task-tests--publish-terminal
            queued
            ':completed
            (task-tests--terminal-result
             queued :status ':success :output "queued complete"))
           (test-assert
            (equal
             (mapcar (lambda (activity)
                       (getf activity :id))
                     (terminal-ui-agent-activities ui))
             (list (job-identifier running)))
            "terminal lifecycle events remove only their finished child")
           (task-tests--publish-terminal
            running
            ':completed
            (task-tests--terminal-result
             running :status ':success :output "running complete"))
           (task-orchestrator-emit
            orchestrator
            ':task-subagent-progress
            (list :id (job-identifier running)))
           (test-assert
            (null (terminal-ui-agent-activities ui))
            "late progress cannot resurrect a terminal child")
           (let* ((timestamp
                    (encode-universal-time 0 53 22 10 8 2026))
                  (payload
                    (list :id "child-1"
                          :execution-id "execution-1"
                          :child-name "shared-diff-final-review"
                          :steering-id "steering-1"
                          :text "Use careful context."
                          :time timestamp))
                  (entry
                    (application--child-response-entry application payload))
                  (records-before
                     (copy-tree
                      (application-tests--conversation-records conversation)))
                  (stale-listener
                    (application-task-presentation-listener application)))
              (test-assert
               (and (find
                     (terminal-span ':child-name "shared-diff-final-review")
                     entry
                     :test #'equal)
                    (not (application-turn-timestamps-p application)))
               "child response entries retain semantic child names")
             (application-connect-task-presentation application)
             (test-assert
              (and (= (length (task-orchestrator-listeners orchestrator)) 1)
                   (not
                    (eq stale-listener
                        (application-task-presentation-listener application))))
              "same-orchestrator reconnection replaces the exact listener")
             (recording-terminal-reset terminal)
             (funcall stale-listener
                      ':task-subagent-verbal-response
                      payload)
             (test-assert
              (string= (recording-terminal-output terminal) "")
              "a snapshotted stale listener cannot present after reconnection")
             (recording-terminal-reset terminal)
             (let ((cl-colorist:*color-level* ':indexed))
               (task-orchestrator-emit
                orchestrator ':task-subagent-verbal-response payload))
             (let* ((output (recording-terminal-output terminal))
                    (plain-output (cl-colorist:strip-ansi output))
                    (child-style (terminal-style-sequence ':child-name t))
                    (basic-child-style
                      (terminal-style-sequence ':child-name nil)))
               (test-assert
                (and
                 (search
                  "● autolith [shared-diff-final-review] 2026-08-10 22:53"
                  plain-output)
                 (= (terminal-tests--substring-count
                     "● autolith [shared-diff-final-review]" plain-output)
                    1)
                 (search "Use careful context." plain-output)
                 (search child-style output)
                 (search "38;5;78" child-style)
                 (string= basic-child-style
                          (terminal-style-sequence ':success nil)))
                "one child response is presented once with color 78 or green fallback"))
             (test-assert
              (and
                (equal records-before
                       (application-tests--conversation-records conversation))
               (null (conversation-input-items conversation)))
              "promoted child presentation does not mutate primary conversation state")
             (application-disconnect-task-presentation application)
             (recording-terminal-reset terminal)
             (task-orchestrator-emit
              orchestrator ':task-subagent-verbal-response payload)
             (test-assert
              (string= (recording-terminal-output terminal) "")
              "a disconnected orchestrator cannot present child responses")
             (setf replacement-registry
                   (task-augment-tool-registry
                    (make-default-tool-registry))
                   replacement-orchestrator
                   (task-run-tool-orchestrator
                    (tool-registry-find replacement-registry "task" "run"))
                   (application-tool-registry application)
                   replacement-registry)
             (application-connect-task-presentation application)
             (test-assert
              (and (zerop (length (task-orchestrator-listeners orchestrator)))
                   (= (length
                       (task-orchestrator-listeners replacement-orchestrator))
                      1))
              "runtime replacement moves the exact presentation listener")
             (recording-terminal-reset terminal)
             (task-orchestrator-emit
              orchestrator ':task-subagent-verbal-response payload)
             (test-assert
              (string= (recording-terminal-output terminal) "")
              "the retired orchestrator cannot present after replacement")
             (let ((replacement-payload
                     (list :id "child-2"
                           :execution-id "execution-2"
                           :child-name "replacement-review"
                           :steering-id "steering-2"
                           :text "Replacement response."
                           :time timestamp)))
               (let ((cl-colorist:*color-level* ':indexed))
                 (task-orchestrator-emit
                  replacement-orchestrator
                  ':task-subagent-verbal-response
                  replacement-payload))
               (let ((plain-output
                       (cl-colorist:strip-ansi
                        (recording-terminal-output terminal))))
                 (test-assert
                  (and
                   (search
                    "● autolith [replacement-review] 2026-08-10 22:53"
                    plain-output)
                   (= (terminal-tests--substring-count
                       "● autolith [replacement-review]" plain-output)
                      1)
                   (not (search "shared-diff-final-review" plain-output)))
                  "only the active orchestrator presents its current child response"))))
           (application-disconnect-task-presentation application)
           (test-assert
            (and
             (null (application-task-presentation-orchestrator application))
             (null (application-task-presentation-listener application))
             (or (null replacement-orchestrator)
                 (zerop
                  (length
                   (task-orchestrator-listeners replacement-orchestrator)))))
            "disconnect removes the exact observer and retained scheduler link"))
      (application-disconnect-task-presentation application)
      (dolist (job (remove nil (list queued running)))
        (unless (job-terminal-p job)
          (task-tests--publish-terminal
           job
           ':aborted
           (task-tests--terminal-result
            job :status ':aborted :output "test cleanup"))))
      (when replacement-registry
        (ignore-errors
          (tool-registry-close-runtime-state replacement-registry)))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist ':ignore)))
  nil)

(-> test-working-directory-command () null)
(defun test-working-directory-command ()
  "Test /cwd completion, full-path parsing, and required-argument semantics."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (workspace (merge-pathnames "command workspace/" root))
         (lisp-workspace (merge-pathnames "lisp command workspace/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults *default-pathname-defaults*)
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (registry (make-default-tool-registry)))
    (ensure-directories-exist workspace)
    (ensure-directories-exist lisp-workspace)
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
           (test-assert
            (handler-case
                (progn
                  (application-command application "/cwd")
                  nil)
              (program-error ()
                t))
            "/cwd omission signals its ordinary required-argument error")
           (application--builtin-working-directory-command application "")
           (test-assert
            (search (format nil "Working directory: ~A"
                            (namestring (truename workspace)))
                    (recording-terminal-output terminal))
            "an explicit empty path preserves the status operation")
           (let ((evaluation
                   (application-lisp-evaluate
                    (format nil "(cwd #P~S)" (namestring lisp-workspace))
                    :application application)))
             (test-assert
              (eq (application-lisp-evaluation-status evaluation) ':ok)
              "a canonical CWD call accepts a pathname object")
             (test-assert
              (equal (configuration-working-directory
                      (application-configuration application))
                     (truename lisp-workspace))
              "a canonical CWD pathname switches workspaces")))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
            (let* ((current-model
                     (configuration-model
                      (application-configuration application)))
                   (selected-model nil)
                   (visible-count nil))
              (setf (scripted-terminal-events terminal) (list :submit)
                    (slot-value terminal 'read-callback)
                    (lambda ()
                      (let ((selector (terminal-ui-selector ui)))
                        (when selector
                          (setf visible-count
                                (selector-visible-count selector))))))
              (unwind-protect
                   (with-terminal-ui (active-ui ui)
                     (declare (ignore active-ui))
                     (setf selected-model
                           (application--pick-model application)))
                (setf (slot-value terminal 'read-callback) nil))
              (test-assert (string= selected-model current-model)
                           "the model picker opens on the current model")
              (test-assert (= visible-count 15)
                           "the model picker can display fifteen candidates"))
            (let* ((current-effort
                     (configuration-reasoning-effort
                      (application-configuration application)))
                   (selected-effort nil))
              (setf (scripted-terminal-events terminal) (list :submit))
              (with-terminal-ui (active-ui ui)
                (declare (ignore active-ui))
                (setf selected-effort
                      (application--pick-reasoning-effort application)))
              (test-assert (string= selected-effort current-effort)
                           "the effort picker opens on the current effort"))
            (setf (scripted-terminal-events terminal)
                  (list '(:insert "terra") :submit))
            (with-terminal-ui (active-ui ui)
              (declare (ignore active-ui))
              (test-assert
               (string= (application--pick-model application) "gpt-5.6-terra")
               "the model picker searches registered model identifiers"))
            (setf (scripted-terminal-events terminal)
                  (list '(:insert "low") :submit))
            (with-terminal-ui (active-ui ui)
              (declare (ignore active-ui))
              (test-assert
               (string= (application--pick-reasoning-effort application) "low")
               "the effort picker searches supported levels"))
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
             (test-assert (= (length items) 3)
                          "every configured 5.6 family model is offered")
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
           (let ((model-installed-before-effort-p nil))
             (setf (scripted-terminal-events terminal)
                   (list :history-next :submit)
                   (slot-value terminal 'read-callback)
                   (lambda ()
                     (setf model-installed-before-effort-p
                           (or model-installed-before-effort-p
                               (string=
                                (configuration-model
                                 (application-configuration application))
                                "gpt-5.6-luna")))))
             (unwind-protect
                  (with-terminal-ui (active-ui ui)
                    (declare (ignore active-ui))
                    (test-assert
                     (eq (application-command application
                                              "/model gpt-5.6-luna")
                         ':continue)
                     "an explicit model change continues after choosing its effort"))
               (setf (slot-value terminal 'read-callback) nil))
             (test-assert
              model-installed-before-effort-p
              "model commands install the selection before prompting for effort"))
           (test-assert
            (and (string= (configuration-model
                           (application-configuration application))
                          "gpt-5.6-luna")
                 (string= (configuration-reasoning-effort
                           (application-configuration application))
                          "medium"))
            "model commands apply the prompted effort after switching models")
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                                (test-terminal-row-text
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
           (let ((text (test-terminal-row-text
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
                                                 :terminal terminal)))
                (started-goal-work 0))
           (terminal-ui-start (application-ui application))
           (test-call-with-function-replacements
            (list
             (list
              'application--start-goal-work
              (lambda (app)
                (declare (ignore app))
                (incf started-goal-work)
                nil)))
            (lambda ()
              (application-goal-command application "update nothing yet")
              (test-assert (null (application-goal application))
                           "updating without a goal leaves none set")
              (test-assert (zerop started-goal-work)
                           "a failed goal update does not start a turn")
              (application-goal-command application "polish the terminal")
              (test-assert (eq (getf (application-goal application) :status)
                               ':active)
                           "setting a goal activates it")
              (test-assert (= started-goal-work 1)
                           "setting a goal starts work immediately")
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
              (test-assert (= started-goal-work 2)
                           "updating an active goal starts work immediately")
              (application-goal-command application "update")
              (test-assert (string= (getf (application-goal application)
                                          :objective)
                                    "polish the prompt")
                           "an empty update leaves the objective unchanged")
              (test-assert (= started-goal-work 2)
                           "an empty goal update does not start a turn")
              (application-goal-command application "update /status")
              (test-assert (string= (getf (application-goal application)
                                          :objective)
                                    "polish the prompt")
                           "command-shaped update objectives are rejected")
              (test-assert (= started-goal-work 2)
                           "a rejected goal update does not start a turn")
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
              (test-assert (= started-goal-work 2)
                           "updating a paused goal does not start a turn")))
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
                                                       :turn-completion ':end)
                                    (agent-test-result "goal-2"
                                                       (list completion-item)
                                                       :turn-completion ':end))))
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
             (let ((restarted 0))
               (test-call-with-function-replacements
                (list
                 (list
                  'application--start-goal-work
                  (lambda (app)
                    (incf restarted)
                    (setf (getf (application-goal app) :continuations) 0)
                    nil)))
                (lambda ()
                  (application-goal-command application "update polish once more")
                  (test-assert (eq (getf (application-goal application) :status)
                                   ':active)
                               "updating a completed goal reactivates it")
                  (test-assert (= restarted 1)
                               "reactivating a completed goal starts work")
                  (test-assert (zerop (getf (application-goal application)
                                            :continuations))
                               "updating restarts the continuation budget")))))
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
                                (list :message :seq 99 :time 0 :role ':user
                                      :content
                                      *application-goal-continuation-prompt*))
                               (list (terminal-span :hint "∙ goal continues")))
                        "continuation prompts render as dim notices")
           (application-goal-command application "clear")
           (test-assert (null (application-goal application))
                        "clearing removes the session goal")
           (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-pending-input-persistence () null)
(defun test-pending-input-persistence ()
  "Test active, steering, recalled, and legacy pending input survives safely."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "pending-inputs"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller nil)
         (restored nil)
         (restored-after-append nil)
         (restored-after-shutdown nil)
         (legacy-controller nil)
         (snapshot-identifier nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller (application-input-controller-create application))
           (application-input-controller--enqueue
            controller ':message "active turn")
           (test-assert
            (equal (application-input-controller--next-work controller)
                   '(:message "active turn"))
            "dispatch returns the accepted active message")
           (let ((active-identifier
                   (application-input-controller-active-work-identifier
                    controller)))
             (test-assert
              (and (non-empty-string-p active-identifier)
                   (equal (application-input-controller-active-work controller)
                          '(:message "active turn")))
              "dispatch keeps identified active work durable before append")
             (multiple-value-bind (form complete-p)
                 (snapshot-read
                  (configuration-pending-inputs-path
                   configuration (conversation-pathname conversation)))
               (let ((active-form (and complete-p
                                       (getf (rest form) :active-work)))
                     (current-snapshot-identifier
                       (and complete-p
                            (getf (rest form) :snapshot-identifier))))
                 (setf snapshot-identifier current-snapshot-identifier)
                 (test-assert
                  (and complete-p
                       (non-empty-string-p current-snapshot-identifier)
                       (= (getf (rest form) :version) 2)
                       (string= (getf active-form :identifier)
                                active-identifier)
                       (equal (getf active-form :work)
                              '(:message "active turn")))
                  "version-two snapshots represent dispatched active work"))))
            (application-input-controller-submit-primary-prompt
             controller "first steering")
            (application-input-controller-submit-primary-prompt
             controller "second steering")
           (application-input-controller--enqueue
            controller ':message "follow later")
           (test-assert
            (application-input-controller--recall-follow-up controller)
            "pending persistence can hold one recalled follow-up")
           (test-assert
            (not
             (application-input-controller--cycle-follow-up
              controller "edited follow later"))
            "editing a lone recall preserves its virtual FIFO position")
           (let* ((entries
                    (application-input-controller--take-steering controller))
                  (first-entry (first entries)))
              (test-assert
               (and (equal (mapcar #'agent-steering-input-content entries)
                           '("first steering" "second steering"))
                    (= (length (application-input-controller--state
                                controller :steering-in-flight-items))
                       2))
               "taking steering durably moves each message in flight")
             (multiple-value-bind (form complete-p)
                 (snapshot-read
                  (configuration-pending-inputs-path
                   configuration (conversation-pathname conversation)))
               (test-assert
                (and complete-p
                     (string= (getf (rest form) :snapshot-identifier)
                              snapshot-identifier))
                "one pending collection keeps a stable snapshot identifier"))
             ;; Simulate a crash after the first append but before its observer
             ;; acknowledgement updates the pending snapshot.
             (conversation-append-user-message
              conversation
              (agent-steering-input-content first-entry)
              :pending-input-identifier
              (agent-steering-input-identifier first-entry)))
           (application-input-controller-stop controller)
           (setf controller nil
                 restored (application-input-controller-create application))
           (test-assert
            (equal (application-input-controller--state restored :work-items)
                   '((:message "active turn")
                     (:message "edited follow later")))
            "active and recalled work restore in original FIFO order")
           (test-assert
            (equal (application-input-controller--state restored :steering-items)
                   '("second steering"))
            "already appended steering is filtered while later steering survives")
           (test-assert
            (equal (application-input-controller--next-work restored)
                   '(:message "active turn"))
            "restored active work dispatches before queued follow-ups")
           (let ((active-identifier
                   (application-input-controller-active-work-identifier restored)))
             ;; Simulate the corresponding active-message append before its
             ;; acknowledgement callback can publish the cleared snapshot.
             (conversation-append-user-message
              conversation
              "active turn"
              :pending-input-identifier active-identifier))
           (application-input-controller-stop restored)
           (setf restored nil
                 restored-after-append
                 (application-input-controller-create application))
           (test-assert
            (equal (application-input-controller--state
                    restored-after-append :work-items)
                   '((:message "second steering")
                     (:message "edited follow later")))
            "a durably appended active message is not restored twice")
           (test-assert
            (null (application-input-controller--state
                   restored-after-append :steering-items))
            "steering becomes ordinary ordered work when its old turn is durable")
           ;; Reproduce a publisher delayed until ordinary shutdown has persisted
           ;; and cleared the process-local queues.
           (application-input-controller--prepare-shutdown
            restored-after-append ':interrupt)
           (application-input-controller--publish-counts restored-after-append)
           (application-input-controller-stop restored-after-append)
           (setf restored-after-append nil
                 restored-after-shutdown
                 (application-input-controller-create application))
           (test-assert
            (equal (application-input-controller--state
                    restored-after-shutdown :work-items)
                   '((:message "second steering")
                     (:message "edited follow later")))
            "a delayed publisher cannot erase shutdown pending input")
           (let* ((legacy-conversation
                    (conversation-create configuration
                                         :identifier "pending-legacy"))
                  (legacy-application
                    (make-instance 'application
                                   :configuration configuration
                                   :conversation legacy-conversation
                                   :ui ui))
                  (legacy-pathname
                    (configuration-legacy-pending-inputs-path configuration))
                  (canonical-pathname
                    (configuration-pending-inputs-path
                     configuration
                     (conversation-pathname legacy-conversation))))
             (snapshot-write
              legacy-pathname
              (list :pending-inputs
                    :version 1
                    :conversation-id "pending-legacy"
                    :steering '("legacy steering")
                    :work '((:message "legacy follow-up"))))
             (setf legacy-controller
                   (application-input-controller-create legacy-application))
             (test-assert
              (equal (application-input-controller--state
                      legacy-controller :work-items)
                     '((:message "legacy steering")
                       (:message "legacy follow-up")))
              "legacy steering and work migrate into executable FIFO order")
             (test-assert
              (and (probe-file canonical-pathname)
                   (not (probe-file legacy-pathname)))
              "legacy pending input migrates to its conversation-scoped path")
             (multiple-value-bind (form complete-p)
                 (snapshot-read canonical-pathname)
               (test-assert
                (and complete-p (= (getf (rest form) :version) 2))
                "legacy migration publishes the current pending format"))))
      (when legacy-controller
        (application-input-controller-stop legacy-controller))
      (when restored-after-shutdown
        (application-input-controller-stop restored-after-shutdown))
      (when restored-after-append
        (application-input-controller-stop restored-after-append))
      (when restored
        (application-input-controller-stop restored))
      (when controller
        (application-input-controller-stop controller))
      (terminal-ui-stop ui)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-rate-limit-defers-queued-follow-ups () null)
(defun test-rate-limit-defers-queued-follow-ups ()
  "Test that a 429 defers remaining follow-ups instead of submitting them."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (provider (provider-create configuration))
         (conversation
           (conversation-create configuration :identifier "rate-limit-queue"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider provider
                          :ui ui))
         (later-state (make-instance 'later-state))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :later-state later-state
                          :main-thread (bt:current-thread)))
         (ran-inputs nil)
         (presented nil)
         (original-later-schedule (symbol-function 'later-schedule)))
    (unwind-protect
         (progn
           (setf (provider-rate-limits provider)
                 (list :primary
                       (list :used-percent 100
                             :window-minutes 300
                             :resets-at (+ (get-universal-time) 600)))
                 (application-input-controller application) controller)
           (configuration-ensure-directories configuration)
           (deque-append (application-input-controller-work-items controller)
                         '((:message "first")
                           (:message "second")
                           (:message "third")))
           (test-call-with-function-replacements
            (list
             (list
              'application--run-message-input
              (lambda (application input &key steering-function &allow-other-keys)
                (declare (ignore application steering-function))
                (push (if (stringp input)
                          input
                          (user-message-input-text input))
                      ran-inputs)
                ':rate-limited))
             (list
              'application-present
              (lambda (application text)
                (declare (ignore application))
                (push text presented)))
             (list
              'later-schedule
              (lambda (&rest arguments)
                (if (string= (getf arguments :input) "third")
                    (error 'later-error
                           :message "forced deferred write failure"
                           :pathname (configuration-later-path configuration)
                           :operation ':write
                           :cause nil)
                    (apply original-later-schedule arguments)))))
             (lambda ()
               (let ((work (application-input-controller--next-work controller)))
                 (application-input-controller--run-work controller work)
                 (application-input-controller--finish-work controller))))
           (test-assert (equal (nreverse ran-inputs) '("first"))
                        "only the rate-limited turn runs before deferral")
           (test-assert
            (equal (application-input-controller--state controller :work-items)
                   '((:message "third")))
            "a failed deferred write restores its queued follow-up")
           (let ((entries (later-state-entries later-state)))
             (test-assert
              (and (= (length entries) 1)
                   (string= (later-entry-input (first entries)) "second"))
              "successfully persisted follow-ups retain submission order"))
           (test-assert
            (some (lambda (text)
                    (search "Deferred 1 queued follow-up" text))
                  presented)
            "the user is told that follow-ups were deferred"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
             (test-call-with-function-replacements
              (list
               (list
                'application-set-working-directory
                (lambda (app directory)
                  (declare (ignore app directory))
                  nil))
               (list
                'application--run-message-input
                (lambda (app input &key steering-function &allow-other-keys)
                  (declare (ignore app input steering-function))
                  ':rate-limited)))
              (lambda ()
                (application-input-controller--run-later controller entry)))
             (let ((replacement
                     (first
                      (later-state-entries
                       (application-input-controller-later-state controller)))))
               (test-assert
                (and replacement
                     (string= (later-entry-identifier replacement)
                              (later-entry-identifier entry))
                     (> (later-entry-due-at replacement) (get-universal-time)))
                "another rate limit reschedules the same durable input")
                (test-assert
                 (member
                  replacement
                  (later-state-entries
                   (application-input-controller-later-state controller))
                  :test #'eq)
                 "a rate-limited deferred input returns to the pending scheduler")
               (application-input-controller--complete-later controller replacement))
             (application-input-controller--finish-work controller)
             (test-assert
              (null (later-state-entries (later-load configuration)))
              "successful deferred completion removes durable state")))
      (when controller
        (application-input-controller-stop controller))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-hurry-up-mode () null)
(defun test-hurry-up-mode ()
  "Test urgent prompt selection, notices, and hard child-agent limits."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "hurry-up"))
         (registry (task-augment-tool-registry (make-default-tool-registry)))
         (agent (agent-create :configuration configuration
                              :conversation conversation
                              :tool-registry registry
                              :worker ':unused))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :tool-registry registry
                                     :agent agent
                                     :ui ui))
         (orchestrator (application--task-orchestrator application)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (application-command application "/hurry-up on")
           (test-assert (and (application-hurry-up-p application)
                             (agent-hurry-up-p agent)
                             (task-orchestrator-hurry-up-p orchestrator))
                        "/hurry-up on synchronizes application, agent, and task state")
           (test-assert (= (task-orchestrator-maximum-concurrency orchestrator) 2)
                        "hurry-up caps child concurrency at two")
           (test-assert (terminal-ui-notice ui)
                        "hurry-up presents a transient live notice")
           (application-command application "/hurry-up off")
           (test-assert (and (not (application-hurry-up-p application))
                             (not (agent-hurry-up-p agent))
                             (not (task-orchestrator-hurry-up-p orchestrator)))
                        "/hurry-up off restores ordinary session policy"))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-application-command-tips)
  (test-application-command-presentations)
  (test-application-banner-version)
  (test-startup-update-choice)
  (test-explicit-update-operation)
  (test-thinking-label-selection)
  (test-application-status-details)
  (test-application-info-command)
  (test-reasoning-trace-command)
  (test-compact-view-command)
  (test-turn-timestamps-command)
  (test-simple-technical-english-command)
  (test-hurry-up-mode)
  (test-command-permission-modes)
  (test-interrupt-resume-instruction)
  (test-repeated-interrupt-forces-exit)
  (test-forced-exit-without-durable-conversation)
  (test-graceful-shutdown-retains-interrupt-escape)
  (test-active-turn-interrupt-events)
  (test-idle-interrupt-exits-without-force-hint)
  (test-active-turn-stop-keys)
  (test-active-command-stop-key)
  (test-active-tool-stop-repairs-unknown-outcome)
  (test-interrupt-force-window)
  (test-visible-interrupt-hint-does-not-extend-window)
  (test-dropped-interrupt-hint-reappears)
  (test-active-cancellation-interrupt-window-expiry)
  (test-cancellation-completion-clears-interrupt-state)
  (test-transcript-entries)
  (test-line-change-tool-presentation)
  (test-agenda-change-tool-presentation)
  (test-memory-change-tool-presentation)
  (test-structured-tool-presentation)
  (test-plan-update-call-presentation)
  (test-task-run-call-presentation)
  (test-recovery-cursor-normalization)
  (test-recovery-diagnosis-prompt)
  (test-recovery-application-construction)
  (test-bounded-transcript-replay)
  (test-chunked-transcript-replay)
  (test-hidden-reasoning-does-not-crowd-replay)
  (test-paged-transcript-history)
  (test-compaction-presentation-lifecycle)
  (test-streaming-presentation)
  (test-provider-retry-presentation)
  (test-turn-cursor-visibility)
  (test-responsive-model-input)
  (test-responsive-goal-inspection)
  (test-responsive-command-scheduling)
  (test-recovery-diagnosis-tool-surface)
  (test-input-reader-quiescence)
  (test-primary-prompt-admission)
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
  (test-pending-input-persistence)
  (test-rate-limit-defers-queued-follow-ups)
  (test-later-scheduler)
  t)
