(in-package #:autolith)

;;;; -- Presentation Test Support --

(-> application-tests--ui-application
    (&key (:columns integer) (:reasoning-traces-p boolean)) application)
(defun application-tests--ui-application (&key (columns 40) reasoning-traces-p)
  "Return a nonpersistent application presenting into a recording terminal."
  (let ((*configuration-store* nil))
    (make-instance 'application
                   :configuration (make-configuration :reasoning-traces-p reasoning-traces-p)
                   :tool-registry (make-default-tool-registry)
                   :ui            (terminal-ui-create
                                   :terminal (make-instance 'recording-terminal
                                                            :columns columns)))))

(-> application-tests--render-tool-result (application list) list)
(defun application-tests--render-tool-result (application record)
  "Exercise formatting independently of whether RECORD receives a transcript row."
  (application-tool-result-entry
   (application--find-tool application (getf (rest record) :tool)) application record))

(defclass cursor-observing-provider (scripted-provider)
  ((visibility-function
    :initarg       :visibility-function
    :reader        cursor-observing-provider-visibility-function
    :type          function
    :documentation "Function reporting live-region cursor visibility.")
   (visible-during-request-p
    :initform      t
    :accessor      cursor-observing-provider-visible-during-request-p
    :type          boolean
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
    :initform      (make-lock "Autolith gated provider")
    :reader        gated-provider-lock
    :type          t
    :documentation "The lock protecting deterministic provider timing.")
   (gate-condition-variable
    :initform      (make-condition-variable :name "Autolith gated provider")
    :reader        gated-provider-condition-variable
    :type          t
    :documentation "The wait point for first-request entry and release.")
   (request-count
    :initform      0
    :accessor      gated-provider-request-count
    :type          (integer 0)
    :documentation "The number of provider requests that reached the gate.")
   (entered-p
    :initform      nil
    :accessor      gated-provider-entered-p
    :type          boolean
    :documentation "Whether the first provider request reached the gate.")
   (released-p
    :initform      nil
    :accessor      gated-provider-released-p
    :type          boolean
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
    :initform      (make-lock "Autolith gated tool")
    :reader        application-test-gated-tool-lock
    :type          t
    :documentation "The lock protecting deterministic tool execution timing.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith gated tool")
    :reader        application-test-gated-tool-condition-variable
    :type          t
    :documentation "The wait point for gated tool execution.")
   (entered-p
    :initform      nil
    :accessor      application-test-gated-tool-entered-p
    :type          boolean
    :documentation "Whether the tool reached its cancellable execution point.")
   (released-p
    :initform      nil
    :accessor      application-test-gated-tool-released-p
    :type          boolean
    :documentation "Whether a fallback cleanup may complete the tool.")
   (completed-p
    :initform      nil
    :accessor      application-test-gated-tool--completed-p
    :type          boolean
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
    :initform      (make-lock "Autolith counting provider")
    :reader        application-test-counting-provider-lock
    :type          t
    :documentation "The lock protecting the completed request count.")
   (request-count
    :initform      0
    :accessor      application-test-counting-provider-request-count
    :type          (integer 0)
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
    :initarg       :provider
    :reader        responsive-scripted-terminal-provider
    :type          gated-provider
    :documentation "The provider whose first request paces later input.")
   (conversation
    :initarg       :conversation
    :reader        responsive-scripted-terminal-conversation
    :type          conversation
    :documentation "The conversation whose durable answers permit final EOF.")
   (pre-provider-event-count
    :initarg       :pre-provider-event-count
    :initform      2
    :reader        responsive-scripted-terminal-pre-provider-event-count
    :type          (integer 0)
    :documentation "The number of initial events admitted before the provider gate opens.")
   (events-read
    :initform      0
    :accessor      responsive-scripted-terminal-events-read
    :type          (integer 0)
    :documentation "The number of scripted events already returned.")
   (final-provider-item-count
    :initarg       :final-provider-item-count
    :initform      2
    :reader        responsive-scripted-terminal-final-provider-item-count
    :type          (integer 0)
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
  (let* ((events    (scripted-terminal-events terminal))
         (remaining (length events))
         (provider  (responsive-scripted-terminal-provider terminal)))
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
    :initform      (make-lock "Autolith queued terminal")
    :reader        queued-recording-terminal-event-lock
    :type          t
    :documentation "The lock protecting events submitted by test threads.")
   (events
    :initform      nil
    :accessor      queued-recording-terminal-events
    :type          list
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
  (test-assert
   (string= (application-command-name (application-command-find "usage")) "status")
   "declared aliases resolve through their canonical command definitions")
  (let* ((command
           (make-instance
            'application-command
            :name "test-tip"
            :tip  "Try `(let ((label \"(x)\")) (list 'resume label))` (or later), then `'(a (b c))`."))
         (spans (application--command-tip-spans command)))
    (test-assert
     (equal (loop for span in spans
                  when (eq (terminal-span-style span) ':code)
                    collect (terminal-span-text span))
            '("(test-tip)" "(let ((label \"(x)\")) (list 'resume label))"
              "'(a (b c))"))
     "nested forms, quote syntax, and parentheses in strings form whole code spans")
    (test-assert
     (some (lambda (span)
             (and (eq (terminal-span-style span) ':dim)
                  (search "(or later)" (terminal-span-text span))))
           spans)
     "parenthesized prose keeps its quiet style")
    (test-assert
     (string= (terminal--spans-text spans)
              "· tip: (test-tip) Try (let ((label \"(x)\")) (list 'resume label)) (or later), then '(a (b c)).")
     "inline delimiters disappear without changing example or prose content")
    (dolist (width '(18 42))
      (let ((rows (terminal-ui--word-wrap-spans
                   (make-instance 'recording-terminal :columns width)
                   spans width)))
        (test-assert (> (length rows) 1) "tips wrap through the ordinary terminal path")
        (test-assert
         (every (lambda (row)
                  (<= (text-cell-width (terminal-rendered-row-text row)) width))
                rows)
         "code examples and prose fit the available terminal width"))))
  (let* ((*random-state* (make-random-state nil))
         (captured-state (make-random-state *random-state*)))
    (application--startup-tip-spans)
    (test-assert (equalp *random-state* captured-state)
                 "startup tip selection does not consume saved image RNG state"))
  nil)

(-> test-startup-update-choice () null)
(defun test-startup-update-choice ()
  "Test that startup update choices depend on the installation method."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (tag           "v99.0.0")
         (release-provenance
           (make-instance 'installation-provenance
                          :method      ':release
                          :current-tag (format nil "v~A" *autolith-version*)))
         (release-availability
           (make-instance 'update-availability :tag tag :method ':release)))
    (labels ((make-application (events &key (terminal
                                             (make-instance 'scripted-terminal
                                                            :columns 72
                                                            :events  events)))
               "Return a release application scripted with EVENTS or using TERMINAL."
               (make-instance
                'application
                :configuration           configuration
                :installation-provenance release-provenance
                :update-availability     release-availability
                :ui                      (terminal-ui-create :terminal terminal))))
      (unwind-protect
           (progn
             (let* ((application
                      (make-application
                       nil :terminal (stream-terminal-create
                                      :input-stream          (make-string-input-stream "")
                                      :output-stream         (make-string-output-stream)
                                      :input-file-descriptor -1)))
                    (ui         (application-ui application))
                    (selections 0))
               (test-call-with-function-replacements
                (list
                 (list 'terminal-ui-select
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (incf selections)
                         ':skip)))
                (lambda ()
                  (with-terminal-ui (active-ui ui)
                    (test-assert
                     (and (terminal-started-p (terminal-ui-terminal ui))
                          (not (terminal-interactive-p (terminal-ui-terminal ui))))
                     "an unattended startup uses an active noninteractive terminal")
                    (application--offer-startup-update application))))
               (test-assert (zerop selections)
                            "an unattended startup never enters the update picker")
               (test-assert
                (eq (application-update-availability application) release-availability)
                "an unattended startup retains the cached release for explicit updates")
               (test-assert
                (null (update-state-dismissed-tag (update-state-load configuration)))
                "an unattended startup does not dismiss a release without a choice"))

             (let* ((application (make-application (list :submit)))
                    (ui          (application-ui application))
                    (notice      (application--update-notice application))
                    (text        (format nil "~{~A~}"
                                         (mapcar #'terminal-span-text notice))))
               (test-assert
                (and (search "update available: Autolith" text)
                     (search "choose whether to install" text))
                "a packaged release receives the cached banner warning")
               (with-terminal-ui (active-ui ui)
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
                 (application--offer-startup-update application))
               (test-assert
                (and (null (application-update-availability application))
                     (string= (update-state-dismissed-tag
                               (update-state-load configuration))
                              tag))
                "Skip this version atomically dismisses only the selected tag"))

             (let* ((availability
                      (make-instance 'update-availability
                                     :tag    tag
                                     :method ':nix))
                    (application
                       (make-instance 'application
                                      :configuration       configuration
                                      :update-availability availability
                                      :ui                  (terminal-ui-create
                                                            :terminal (make-instance 'scripted-terminal
                                                                                     :columns 72
                                                                                     :events  nil))))
                    (ui         (application-ui application))
                    (notice     (application--update-notice application))
                    (text       (format nil "~{~A~}"
                                        (mapcar #'terminal-span-text notice)))
                    (selections 0))
               (test-assert
                (and (search "Nix" text) (search "flake" text) (search "profile" text))
                "The Nix notice contains manual update advice.")
               (test-call-with-function-replacements
                (list (list 'terminal-ui-select
                            (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (incf selections)
                              ':not-now)))
                (lambda ()
                  (with-terminal-ui (active-ui ui)
                    (application--offer-startup-update application))))
               (test-assert (zerop selections)
                            "Users are not offered an in-app update for Nix installations.")
               (test-assert (eq (application-update-availability application) availability)
                            "The cached Nix update is available for manual installation."))

             (let* ((application (make-instance 'application))
                    (thread      (make-thread (lambda () (sleep 0.01))
                                              :name "Autolith update check test")))
               (setf (application-update-check-thread application) thread)
               (application--quiesce-update-check application)
               (test-assert
                (and (null (application-update-check-thread application))
                     (not (thread-alive-p thread)))
                "checkpoint quiescence joins the background update check")))
        (platform-delete-directory-tree *platform* root
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-explicit-update-operation () null)
(defun test-explicit-update-operation ()
  "Test fresh installed-release updates and nonmutating external update advice."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   (make-instance 'application :configuration configuration))
         (presented     nil)
         (fetches       0))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'application-present
                 (lambda (observed text)
                   (test-assert (eq observed application)
                                "explicit update output belongs to its application")
                   (push (test-entry-text text) presented)
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
                     :method      ':release
                     :current-tag (format nil "v~A" *autolith-version*))
                    (application-update-availability application)
                    (make-instance 'update-availability
                                   :tag    "v99.0.0"
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-command-permission-modes () null)
(defun test-command-permission-modes ()
  "Test session permission commands and fail-closed command authorization."
  (if (application--command-sandbox-available-p)
      (let* ((configuration (test-configuration))
             (root          (test-configuration-root configuration))
             (terminal      (make-instance 'recording-terminal :columns 72))
             (ui            (terminal-ui-create :terminal terminal))
             (state         (permissions-load configuration))
             (conversation
               (conversation-create configuration :identifier "permission-modes"))
             (application
               (make-instance 'application
                              :configuration    configuration
                              :conversation     conversation
                              :ui               ui
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
                                     20)
                                    (rlm-inference-test-result
                                     "classify-3"
                                     "{\"decision\": \"sandboxed\", \"reason\": \"read-only inspection\"}"
                                     20)
                                    (rlm-inference-test-result
                                     "classify-4"
                                     "{\"decision\": \"full\", \"reason\": \"sandbox unavailable\"}"
                                     20)
                                    (rlm-inference-test-result
                                     "classify-5"
                                     "{\"decision\": \"sandboxed\", \"reason\": \"read-only inspection\"}"
                                     20)
                                    (rlm-inference-test-result
                                     "classify-6"
                                     "{\"decision\": \"sandboxed\", \"reason\": \"read-only inspection\"}"
                                     20))))))
        (unwind-protect
             (progn
               (terminal-ui-start ui)
               (conversation-append-user-message
                conversation
                "Fetch dependencies and inspect this workspace.")
               (test-call-with-function-replacements
                (list
                   (list 'permissions-model-classify-command
                         (lambda (command directory &key provider configuration
                                                         sandbox-available-p
                                                         user-instructions)
                           (declare (ignore command directory provider
                                            configuration sandbox-available-p))
                           (test-assert
                            (string= user-instructions
                                     "Fetch dependencies and inspect this workspace.")
                            "application classification receives current user instructions")
                           (values ':sandboxed "read-only inspection"))))
                (lambda ()
                  (test-assert
                   (eq (application-authorize-command
                        application "printf unknown" root)
                       ':sandboxed)
                   "ask mode classifies commands when no approval UI is available")
                  (test-assert
                   (eq (application-permission-mode application) ':ask)
                   "noninteractive classification preserves ask mode")))
               (conversation-append-user-message
                conversation
                "Run the exact pending command with the access it needs.")
               (test-call-with-function-replacements
                (list
                 (list 'permissions-model-classify-command
                       (lambda (command directory &key provider configuration
                                                       sandbox-available-p
                                                       user-instructions)
                         (declare (ignore command directory provider configuration
                                          sandbox-available-p))
                         (test-assert
                          (string= user-instructions
                                   "Run the exact pending command with the access it needs.")
                          "classification cache keys include current user instructions")
                         (values ':full-access "explicitly requested"))))
                (lambda ()
                  (test-assert
                   (eq (application-authorize-command
                        application "printf unknown" root)
                       ':full-access)
                   "changed user instructions trigger fresh classification")))
               (permissions-allow :configuration configuration
                                  :state         state
                                  :command       "printf saved"
                                  :directory     root)
               (test-assert
                (eq (application-authorize-command
                     application "printf saved" root)
                    ':full-access)
                "ask mode runs an exact saved approval with full access")
               (application-run-lisp-input application "(permissions :sandbox)")
               (test-assert
                (eq (application-authorize-command
                     application "printf any" root)
                    ':sandboxed)
                "(permissions :sandbox) allows sandboxed commands")
               (test-call-with-function-replacements
                (list
                 (list 'sandbox-supported-p
                       (lambda (&optional capability)
                         (declare (ignore capability))
                         nil)))
                (lambda ()
                   (test-assert
                    (handler-case
                        (progn
                          (application-authorize-command
                           application "printf any" root)
                          nil)
                      (command-authorization-unavailable ()
                        t))
                    "session sandbox mode explicitly rejects after helper loss")
                  (test-assert
                   (equal (mapcar (lambda (item) (getf item ':name))
                                  (application--permission-mode-items application))
                          '("ask" "auto" "full"))
                   "the permission mode picker omits an unavailable sandbox")
                  (test-assert
                   (handler-case
                       (progn
                         (application-permissions-command application ':sandbox)
                         nil)
                     (configuration-error ()
                       t))
                   "direct sandbox mode selection rejects unavailable containment")))
               (application-run-lisp-input application "(permissions :full)")
               (test-assert
                (eq (application-authorize-command
                     application "printf any" root)
                    ':full-access)
                "(permissions :full) grants full command access")
               (application-run-lisp-input application "(permissions :auto)")
               (test-assert (eq (application-permission-mode application) ':auto)
                            "(permissions :auto) selects autonomous classification")
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
               (test-call-with-function-replacements
                (list
                 (list 'permissions-model-classify-command
                       (lambda (command directory &key provider configuration
                                                       sandbox-available-p
                                                       user-instructions)
                         (declare (ignore command directory provider configuration
                                          sandbox-available-p user-instructions))
                         (values ':pick "destructive change"))))
                (lambda ()
                  (test-assert
                   (eq (application-authorize-command
                        application "git restore file" root)
                       ':deny)
                   "auto mode rejects a classifier pick decision")))
               (let ((provider (application-provider application)))
                 (unwind-protect
                      (progn
                        (setf (application-provider application) nil)
                        (test-assert
                         (eq (application-authorize-command
                              application "uname -a" root)
                             ':deny)
                         "auto mode denies when no classifier is available"))
                   (setf (application-provider application) provider)))
               (let ((capability-checks 0))
                 (test-call-with-function-replacements
                  (list
                   (list 'sandbox-supported-p
                         (lambda (&optional capability)
                           (declare (ignore capability))
                           (= (incf capability-checks) 1)))
                   (list 'application-approval-possible-p
                         (lambda (ignored)
                           (declare (ignore ignored))
                           t))
                   (list 'application--ask-command-permission
                         (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (error "auto mode attempted to open a permission picker"))))
                  (lambda ()
                    (test-assert
                     (eq (application-authorize-command
                          application "git diff" root)
                         ':deny)
                     "auto mode denies when containment disappears with a picker available")
                    (test-assert
                     (= capability-checks 2)
                     "a sandbox grant is checked at the final authorization boundary"))))
               (test-call-with-function-replacements
                (list
                 (list 'sandbox-supported-p
                       (lambda (&optional capability)
                         (eq capability ':network-isolated))))
                (lambda ()
                  (test-assert
                   (application--command-sandbox-available-p)
                   "optional sandbox capabilities do not disable auto containment")))
               (let ((authorization-item-names nil))
                (test-call-with-function-replacements
                 (list
                  (list 'sandbox-supported-p
                        (lambda (&optional capability)
                          (declare (ignore capability))
                          nil))
                  (list 'application-input-controller
                        (lambda (ignored)
                          (declare (ignore ignored))
                          ':test-controller))
                  (list 'terminal-interactive-p
                        (lambda (ignored)
                          (declare (ignore ignored))
                          t))
                  (list 'application-input-controller-call-with-reader-paused
                        (lambda (ignored function)
                          (declare (ignore ignored))
                          (funcall function)))
                  (list 'terminal-ui-select
                        (lambda (ignored &key items &allow-other-keys)
                          (declare (ignore ignored))
                          (setf authorization-item-names
                                (mapcar (lambda (item) (getf item ':name)) items))
                          ':once)))
                 (lambda ()
                   (test-assert
                    (eq (application-authorize-command
                         application "git status" root)
                        ':full-access)
                    "sandbox availability separates cached auto decisions")
                   (test-assert
                    (eq (application-authorize-command
                         application "printf saved" root)
                        ':full-access)
                    "auto mode reclassifies saved commands without a sandbox")
                   (setf (application-permission-mode application) ':ask)
                   (test-assert
                    (eq (application-authorize-command
                         application "git log" root)
                        ':full-access)
                    "manual once approval grants full access")
                   (test-assert
                    (equal authorization-item-names
                           '("auto" "once" "always" "full" "deny"))
                    "the manual command picker retains full-access approvals without a sandbox")
                   (test-assert
                    (eq (application--apply-command-authorization-choice
                         application "git log" root :choice ':once)
                        ':full-access)
                    "once grants full access without changing session mode")
                   (test-assert
                    (eq (application-permission-mode application) ':ask)
                    "once preserves manual permission mode")
                   (test-assert
                    (eq (application--apply-command-authorization-choice
                         application "git log" root :choice ':always)
                        ':full-access)
                    "always grants full access when the sandbox is unavailable")
                   (test-assert
                    (permissions-allowed-p state "git log" root)
                    "always persists the exact full-access approval"))))
               (setf (application-permission-mode application) ':ask)
               (test-call-with-function-replacements
                (list
                 (list 'permissions-model-classify-command
                       (lambda (command directory &key provider configuration
                                                       sandbox-available-p
                                                       user-instructions)
                         (declare (ignore command directory provider
                                          configuration sandbox-available-p
                                          user-instructions))
                         (values ':full-access "prints a literal"))))
                (lambda ()
                  (test-assert
                   (eq (application--apply-command-authorization-choice
                        application "printf picked" root :choice ':auto)
                       ':full-access)
                   "auto classifies the pending command immediately")))
               (test-assert
                (eq (application-permission-mode application) ':auto)
                "auto stays the session permission mode like full and sandbox")
               (application-run-lisp-input application "(permissions :ask)")
               (test-assert (eq (application-permission-mode application) ':ask)
                            "(permissions :ask) restores prompt mode")
               (application-run-lisp-input application "(permissions :clear)")
               (test-assert (null (permission-state-rules state))
                            "(permissions :clear) removes saved exact approvals")
               (test-assert (application-operation-find application "permissions")
                            "local Lisp can invoke the permissions operation")
               (terminal-ui-stop ui))
          (ignore-errors (terminal-ui-stop ui))
          (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
      (test-withheld ':command-sandbox
                     "session permission modes with a workspace sandbox"))
  nil)

(-> test-automatic-permission-presentation () null)
(defun test-automatic-permission-presentation ()
  "Show judging during inference and attach its verdict across unrelated notices."
  (with-test-configuration (configuration)
    (let* ((ui           (fullscreen-test--ui (make-instance 'recording-terminal :columns 100 :rows 40)))
           (conversation (conversation-create configuration))
           (application  (make-instance 'application :configuration configuration
                                         :conversation conversation :ui ui
                                         :tool-registry             (make-default-tool-registry)
                                         :provider                  (make-instance 'rlm-inference-test-provider :results nil)))
           (directory    (config :working-directory configuration))
           (calls        0)
           (context      (make-instance 'tool-context :conversation conversation :call-id "judged"
                                         :command-authorization-function
                                         (lambda (command directory)
                                           (application--auto-command-permission application command directory)))))
      (with-terminal-ui (active ui)
        (dolist (identifier '("judged" "other"))
          (terminal-ui-present-records
           active
           (list (application--record-presentation
                  application
                  (list :provider-item :seq (if (equal identifier "judged") 1 2)
                        :wire-json     (json-encode
                                        (agent-test-call :call-id identifier :namespace "shell" :name "run"
                                                         :arguments (json-encode
                                                                     (json-object "command" "printf judged")))))))))
        (terminal-ui-set-status active "running shell.run")
        (terminal-ui-set-local-activity active "local work")
        (let ((started-at (terminal-ui-status-started-at active)))
          (test-call-with-function-replacements
           (list (list 'permissions-model-classify-command
                       (lambda (command directory &key provider configuration sandbox-available-p user-instructions)
                         (declare (ignore command directory provider configuration sandbox-available-p user-instructions))
                         (incf calls)
                         (test-assert (search "judging" (terminal--spans-text
                                                         (terminal-ui--status-row-at active
                                                                                    (funcall (terminal-ui-clock-function active))
                                                                                    100)))
                                      "The bottom modeline displays judging while inference is pending.")
                         (test-assert (eq context *tool-authorization-context*)
                                      "Permission inference retains the exact invoking context.")
                         (application-present application "cache miss notice")
                         (values ':full-access "allowed"))))
           (lambda ()
             (test-assert (eq ':full-access (tool-context-authorize-command context "printf judged" directory))
                          "Automatic authorization applies its classified decision.")
             (let ((other (make-instance 'tool-context :conversation conversation :call-id "other"
                                                       :command-authorization-function
                                                       (lambda (command directory)
                                                         (application--auto-command-permission application command directory)))))
               (tool-context-authorize-command other "printf judged" directory))))
          (test-assert (and (= calls 1) (null (terminal-ui-judgments active))
                            (= started-at (terminal-ui-status-started-at active))
                            (equal "local work" (terminal-ui-local-activity active)))
                       "Cached decisions skip judging, and inference preserves unrelated activity clocks."))
        (let* ((viewport (fullscreen-terminal-ui-viewport active))
               (first    (clinedi:transcript-viewport-chunk-text viewport 0))
               (second   (clinedi:transcript-viewport-chunk-text viewport 1))
               (notice   (clinedi:transcript-viewport-chunk-text viewport 2)))
          (test-assert (and (= 3 (fullscreen-test--chunk-count active))
                            (search "auto-permission full access" first)
                            (search "auto-permission full access" second)
                            (equal "· cache miss notice" (string-trim '(#\Newline) notice)))
                       "Identical commands retain their own verdicts, separate from the cache notice."))
        (terminal-ui-present-records
         active (list (application--record-presentation
                       application (list :tool-result :seq 3 :call-id "judged" :tool "shell.run"
                                         :status ':ok :output "done"))))
        (dolist (width '(40 100))
          (terminal-ui-resize active width :rows 40)
          (test-assert (search "auto-permission" (clinedi:transcript-viewport-chunk-text
                                                  (fullscreen-terminal-ui-viewport active) 0))
                       "A folded result and resize preserve its attached verdict."))
        (test-assert (null *tool-authorization-context*) "Authorization restores the caller's context.")))))

(-> test-end-of-input-open-instruction () null)
(defun test-end-of-input-open-instruction ()
  "Test EOF presents one reopen command for durable conversations."
  (with-test-configuration (configuration)
    (labels ((exit-output (conversation events &key reason)
               "Run CONVERSATION through EVENTS and verify its actual exit REASON."
               (let* ((terminal    (make-instance 'queued-recording-terminal :columns 80))
                      (application
                        (make-instance 'application :configuration configuration
                                                    :conversation  conversation
                                                    :provider      nil
                                                    :tool-registry (make-instance 'tool-registry)
                                                    :worker        nil
                                                    :agent         nil
                                                    :ui            (terminal-ui-create :terminal terminal))))
                 (setf (queued-recording-terminal-events terminal) events)
                 ;; Boot linger can consume queued EOF before the controller reads it.
                 (let ((actual-reason
                         (handler-case
                             (sb-ext:with-timeout 10
                               (application-serve application :boot-screen-p nil))
                           (sb-ext:timeout ()
                             (error "The direct terminal did not exit through ~S within 10 seconds."
                                    reason)))))
                   (test-assert (eq actual-reason reason)
                                (format nil "The direct terminal exits through ~S" reason)))
                 (recording-terminal-output terminal)))

             (occurrences (text substring)
               "Count nonoverlapping SUBSTRING occurrences in TEXT."
               (loop for start = 0 then (+ position (length substring))
                     for position = (search substring text :start2 start)
                     while position count position)))
      (dolist (case '((:end-of-input (:end-of-input))))
        (destructuring-bind (reason events) case
          (let* ((durable (conversation-create configuration
                                               :identifier (format nil "resume-~(~A~)" reason)))
                 (empty   (conversation-create configuration
                                               :identifier (format nil "discard-~(~A~)" reason)))
                 (command (channel-open-command (conversation-identifier durable))))
            (conversation-append-user-message durable "keep this conversation")
            (let ((output (exit-output durable (copy-tree events) :reason reason)))
              (test-assert (= 1 (occurrences output command))
                           (format nil "Direct ~S prints the exact reopen command once" reason)))
            (let ((output (exit-output empty (copy-tree events) :reason reason)))
              (test-assert (not (search "autolith open" output))
                           (format nil "Direct ~S gives no reopen command for an empty channel" reason))
              (test-assert (not (conversation-storage-occupied-p (conversation-pathname empty)))
                           (format nil "Direct ~S does not persist an empty conversation" reason))))))))
  nil)

(-> test-detach-control-event-dispatch () null)
(defun test-detach-control-event-dispatch ()
  "Dispatch Ctrl-Z's semantic detach event through the normal command path."
  (dolist (active-p '(nil t))
    (let* ((terminal    (make-instance 'recording-terminal :columns 80))
           (ui          (terminal-ui-create :terminal terminal))
           (application (make-instance 'application :ui ui))
           (controller  (make-instance 'application-input-controller
                                       :application application
                                       :main-thread (current-thread)))
           (called      nil))
      (setf (application-input-controller application) controller
            (application-input-controller-active-p controller) active-p)
      (test-call-with-function-replacements
       (list
        (list 'application-input-controller--handle-submission
              (lambda (received input &key steer-p)
                (declare (ignore steer-p))
                (setf called (list received input))
                nil)))
       (lambda ()
         (application-input-controller--process-event controller ':detach)))
      (test-assert (equal called (list controller "(exit)"))
                   "Ctrl-Z submits exit while idle and active")
      (terminal-ui-stop ui)))
  nil)

(-> test-active-turn-interrupt-events () null)
(defun test-active-turn-interrupt-events ()
  "Test Escape preserves FIFO steering and finishes cancellation."
  (let* ((now         10)
         (terminal    (make-instance 'recording-terminal :columns 80))
         (ui          (terminal-ui-create
                       :terminal       terminal
                       :clock-function (lambda () now)))
         (application (make-instance 'application :ui ui))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :main-thread (current-thread))))
      (setf (application-input-controller application) controller
            (application-input-controller-active-p controller) t)
      (deque-push-back
       (application-input-controller-work-items controller)
       (list ':message "queued"))
      (dolist (content '("first in flight" "second in flight"))
        (deque-push-back
         (application-input-controller-steering-items controller) content))
      (test-assert
       (equal (mapcar #'agent-steering-input-content
                      (application-input-controller--take-steering controller))
              '("first in flight" "second in flight"))
       "ordinary steering delivery drains inputs in FIFO order")
      (dolist (steering '("older steering" "newest steering"))
        (deque-push-back
         (application-input-controller-steering-items controller)
         steering))
      (terminal-ui-set-input ui "draft survives")
    (application-input-controller--process-event controller ':escape)
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
     "cancellation defers steering promotion until active work finishes")
    (test-assert
     (equal (application-input-controller--state controller :steering-items)
            (list "older steering" "newest steering"))
     "cancellation preserves pending steering order")
    (test-assert
     (application-input-controller-turn-cancellation-p controller)
     "the cancellation lifecycle remains active until work cleanup finishes")
    (test-assert
     (string= (terminal-ui-notice ui) "· interrupting request")
     "an accepted stop immediately announces request interruption")
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
     (not (application-input-controller-turn-cancellation-p controller))
     "work cleanup ends the cancellation lifecycle")
    (test-assert
     (equal (application-input-controller--state controller :work-items)
            (list (list ':message "first in flight")
                  (list ':message "second in flight")
                  (list ':message "older steering")
                  (list ':message "newest steering")
                  (list ':message "queued")))
     "cancellation promotes in-flight and pending steering in FIFO order"))
  nil)

(-> test-active-turn-cancellation-side-effects () null)
(defun test-active-turn-cancellation-side-effects ()
  "Test cancellation delivery survives notice failure and follows menu dismissal."
  (let* ((terminal        (make-instance 'recording-terminal :columns 80))
         (ui              (terminal-ui-create :terminal terminal))
         (application     (make-instance 'application :ui ui))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :main-thread (current-thread)))
         (interrupt-count 0))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (test-call-with-function-replacements
     (list
      (list 'terminal-ui-set-notice
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error "notice rendering failed")))
      (list 'application-input-controller--interrupt-main-for-turn-cancellation
            (lambda (observed-controller)
              (test-assert (eq observed-controller controller)
                           "cancellation interrupts the requesting controller")
              (incf interrupt-count))))
     (lambda ()
       (test-assert
        (application-input-controller--request-active-turn-cancellation controller)
        "notice failure does not reject an active cancellation request")))
    (test-assert (= interrupt-count 1)
                 "notice failure does not block cancellation interrupt delivery")
    (test-assert
     (application-input-controller--consume-turn-cancellation-delivery-p controller)
     "notice failure preserves pending cancellation delivery"))
  (let* ((terminal        (make-instance 'recording-terminal :columns 80))
         (ui
           (terminal-ui-create
            :terminal terminal
            :completions
            '((:name "(help)" :argument nil :description "show help" :primary "(help"))))
         (application     (make-instance 'application :ui ui))
         (controller
           (make-instance
            'application-input-controller
            :application application
            :main-thread (current-thread)))
         (interrupt-count 0))
    (setf (application-input-controller application) controller
          (application-input-controller-active-p controller) t)
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (terminal-ui-process-event ui '(:insert "(h"))
           (test-assert (terminal-ui-completion-menu-present-p ui)
                        "typed command input presents a passive completion menu")
           (test-call-with-function-replacements
            (list
             (list
              'application-input-controller--interrupt-main-for-turn-cancellation
              (lambda (observed-controller)
                (test-assert (eq observed-controller controller)
                             "Escape interrupts the active controller")
                (incf interrupt-count))))
            (lambda ()
              (application-input-controller--process-event controller ':escape)
              (test-assert
               (and
                (zerop interrupt-count)
                (not
                 (application-input-controller-turn-cancellation-p controller))
                (not (terminal-ui-completion-menu-present-p ui)))
               "the first Escape dismisses the passive completion menu only")
              (application-input-controller--process-event controller ':escape)))
           (test-assert
            (and (= interrupt-count 1)
                 (application-input-controller-turn-cancellation-p controller))
            "the second Escape interrupts the active request"))
      (terminal-ui-stop ui)))
  nil)

(-> application-tests--serve-host (application) null)
(defun application-tests--serve-host (application)
  "Serve a prepared terminal application under its real world owner on the test thread."
  (world-server-tests--call-with-server
   (application-configuration application)
   (lambda (server)
     (let ((host (make-instance 'world-host :application application)))
       (setf (application-world-server application) server
             (world-host-thread host) (current-thread))
       (with-lock-held ((world-server-lock server))
         (push host (world-server-hosts server)))
       (world-server--host server host :boot-screen-p nil)
       (when (world-host-failure host)
         (error (world-host-failure host))))))
  nil)

(-> test-exit-before-work-handler-keeps-cancellation-pending () null)
(defun test-exit-before-work-handler-keeps-cancellation-pending ()
  "Test an exit reaching new work before its handler leaves cancellation pending."
  (let* ((application (make-instance 'application :ui nil))
         (controller
           (make-instance 'application-input-controller
                          :application                   application
                          :initial-work-items            (make-deque :initial-contents (list (list ':probe)))
                          :pending-persistence-enabled-p nil
                          :main-thread                   (current-thread)))
         (published   (sb-thread:make-semaphore))
         (release     (sb-thread:make-semaphore))
         (processed   (sb-thread:make-semaphore))
         (held-p      nil)
         (outcome     nil)
         (escaped     nil)
         (thread      nil))
    (setf (application-input-controller application) controller
          ;; NEXT-WORK publishes counts after marking its work active and
          ;; before the serving loop's handler is in place; hold it there.
          (client-hub-change-function (application-client-hub application))
          (lambda ()
            (when (and (application-input-controller-active-p controller) (not held-p))
              (setf held-p t)
              (sb-thread:signal-semaphore published)
              (sb-thread:wait-on-semaphore release :timeout 10))))
    (setf thread
          (make-thread
           (lambda ()
             (handler-case
                 (let ((work (application-input-controller--next-work controller)))
                   (setf outcome
                         (list work
                               (application-input-controller--consume-turn-cancellation-delivery-p
                                controller))))
               (serious-condition (condition)
                 (setf escaped condition))))
           :name "Autolith test channel before its work handler"))
    (setf (slot-value controller 'main-thread) thread)
    (unwind-protect
         (progn
           (test-assert (sb-thread:wait-on-semaphore published :timeout 10)
                        "new work is active while its counts publish")
           (application-input-controller--request-exit controller ':end-of-input)
           ;; Interrupts run in order, so this one follows any cancellation.
           (when (thread-alive-p thread)
             (interrupt-thread thread (lambda () (sb-thread:signal-semaphore processed)))
             (sb-thread:wait-on-semaphore processed :timeout 10)))
      (sb-thread:signal-semaphore release)
      (sb-thread:join-thread thread :default nil :timeout 10))
    (test-assert (null escaped)
                 (format nil "no cancellation is signalled outside the work handler: ~S" escaped))
    (test-assert (equal outcome '((:probe) t))
                 "the work arrives with its cancellation pending for the handler"))
  (let* ((application (make-instance 'application :ui nil))
         (controller
           (make-instance 'application-input-controller
                          :application                   application
                          :pending-persistence-enabled-p nil
                          :main-thread                   (current-thread)))
         (waiting     (sb-thread:make-semaphore))
         (never       (sb-thread:make-semaphore))
         (thread
           (make-thread
            (lambda ()
              (let ((*application-turn-cancellation-deliverable-p* t))
                (handler-case
                    (progn
                      (sb-thread:signal-semaphore waiting)
                      (sb-thread:wait-on-semaphore never :timeout 10)
                      ':not-cancelled)
                  (application-turn-cancelled ()
                    ':cancelled))))
            :name "Autolith test channel inside its work handler")))
    (setf (application-input-controller application) controller
          (slot-value controller 'main-thread) thread
          (application-input-controller-active-p controller) t)
    (sb-thread:wait-on-semaphore waiting :timeout 10)
    (test-assert (application-input-controller--request-active-turn-cancellation controller)
                 "an active turn accepts cancellation")
    (test-assert (eq (sb-thread:join-thread thread :default nil :timeout 10) ':cancelled)
                 "work inside its handler is cancelled promptly"))
  nil)

(-> test-active-turn-stop-keys () null)
(defun test-active-turn-stop-keys ()
  "Test Escape cancels one provider turn and returns to the prompt."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let ((event ':escape))
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
                  (ui                               (terminal-ui-create :terminal terminal))
                  (registry                         (agent-test-registry))
                  (agent                            (agent-create :configuration configuration
                                                                  :provider      provider
                                                                  :conversation  conversation
                                                                  :tool-registry registry
                                                                  :worker        nil))
                  (application
                    (make-instance 'application
                                   :configuration configuration
                                   :conversation  conversation
                                   :provider      provider
                                   :tool-registry registry
                                   :worker        nil
                                   :agent         agent
                                   :ui            ui))
                  (provider-entered-observed-p      nil)
                  (cancellation-finished-observed-p nil)
                  (draft-preserved-observed-p       nil)
                  (producer                         nil))
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
                                           controller)))))
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
                             "draft survives"))
                      (queued-recording-terminal-enqueue terminal ':stream-end))
                    :name "Autolith active-turn stop-key test"))
             (unwind-protect
                 (application-tests--serve-host application)
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
             (test-assert draft-preserved-observed-p
                         "the active reader preserves draft text while cancelling")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-escape-submits-steering-together () null)
(defun test-escape-submits-steering-together ()
  "Test that Escape submits both steers in one request and leaves a follow-up queued."
  (let ((*conversation-title-refresh-turn-count* most-positive-fixnum))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration :identifier "escape-steering"))
             (provider     (make-instance 'gated-provider
                                          :results (list (agent-test-result "steered" (list (agent-test-message "steered answer")))
                                                         (agent-test-result "follow-up" (list (agent-test-message "follow-up answer")))
                                                         (agent-test-result "extra" (list (agent-test-message "extra answer"))))))
             (terminal     (make-instance 'queued-recording-terminal :columns 80))
             (ui           (terminal-ui-create :terminal terminal))
             (registry     (agent-test-registry))
             (agent        (agent-create :configuration configuration
                                         :provider      provider
                                         :conversation  conversation
                                         :tool-registry registry
                                         :worker        nil))
             (application  (make-instance 'application
                                          :configuration configuration
                                          :conversation  conversation
                                          :provider      provider
                                          :tool-registry registry
                                          :worker        nil
                                          :agent         agent
                                          :ui            ui))
             (producer     nil))
        (queued-recording-terminal-enqueue terminal '(:insert "original request"))
        (queued-recording-terminal-enqueue terminal ':submit)
        (setf producer
              (make-thread
               (lambda ()
                 (unwind-protect
                      (when (task-tests--wait-until (lambda () (gated-provider-entered-p provider)) 5)
                        (dolist (event '((:insert "first steer") :submit
                                         (:insert "second steer") :submit
                                         (:insert "ordinary follow-up") :complete :escape))
                          (queued-recording-terminal-enqueue terminal event))
                        (task-tests--wait-until
                         (lambda ()
                           (and (>= (gated-provider-request-count provider) 3)
                                (not (application-input-controller-turn-active-p
                                      (application-input-controller application))))) 5))
                   (gated-provider-release provider)
                   (queued-recording-terminal-enqueue terminal ':stream-end)))))
        (unwind-protect (application-tests--serve-host application)
          (gated-provider-release provider)
          (join-thread producer))
        (let ((requests (reverse (scripted-provider-input-snapshots provider))))
          (test-assert
           (equal
            (loop for request in requests
                  collect (loop for item in request
                                when (equal (json-get item "role") "user")
                                  collect (context--message-text item)))
            '(("original request" "first steer" "second steer")
              ("original request" "first steer" "second steer" "ordinary follow-up")))
           "both steers reach the first request after Escape, before the separately queued follow-up")))))
  nil)


;;;; -- Active Command Cancellation --

(-> test-active-command-stop-key () null)
(defun test-active-command-stop-key ()
  "Test Escape cancels a shared active command without fatal recovery."
  (let* ((configuration                    (test-configuration))
         (root                             (test-configuration-root configuration))
         (conversation                     (conversation-create configuration
                                                               :identifier "active-command-stop"))
         (terminal                         (make-instance 'queued-recording-terminal :columns 80))
         (ui                               (terminal-ui-create :terminal terminal))
         (application                      (make-instance 'application
                                                          :configuration configuration
                                                          :conversation  conversation
                                                          :provider      nil
                                                          :tool-registry (make-instance 'tool-registry)
                                                          :worker        nil
                                                          :agent         nil
                                                          :ui            ui))
         (command-lock                     (make-lock "Autolith active command stop-key test"))
         (command-gate                     (make-condition-variable
                                            :name "Autolith active command stop-key test"))
         (command-entered-p                nil)
         (command-released-p               nil)
         (registrations                    (application-command--registry-snapshot))
         (cancellation-finished-observed-p nil)
         (draft-preserved-observed-p       nil)
         (producer                         nil))
    (unwind-protect
         (unwind-protect
              (progn
                (register-application-command
                 (application-command-create
                  :definition-name   'test-active-command-stop-key-command
                  :name              "test-cancel-command"
                  :aliases           nil
                  :argument          nil
                  :description       "Block until the active-turn cancellation test stops it."
                  :tip               "Use this test command only from the application test suite."
                  :busy-behavior     ':hold
                  :terminal-behavior ':shared
                  :handler
                  (lambda (application)
                    (declare (ignore application))
                    (with-lock-held (command-lock)
                      (setf command-entered-p t)
                      (condition-notify command-gate)
                      (loop until command-released-p
                            do (condition-wait command-gate command-lock)))
                    ':continue))
                 :source ':test)
                ;; Build the lazy syntax library before starting bounded input waits.
                (application-lisp-code-spans "(test-cancel-command)")
                (queued-recording-terminal-enqueue
                 terminal '(:insert "(test-cancel-command)"))
                (queued-recording-terminal-enqueue terminal ':submit)
                (setf producer
                      (make-thread
                       (lambda ()
                         (unwind-protect
                              (when (task-tests--wait-until
                                     (lambda ()
                                       (with-lock-held (command-lock)
                                         command-entered-p))
                                     2)
                                (queued-recording-terminal-enqueue
                                 terminal '(:insert "draft survives"))
                                (queued-recording-terminal-enqueue terminal ':escape)
                                (setf cancellation-finished-observed-p
                                      (task-tests--wait-until
                                       (lambda ()
                                         (let ((controller
                                                 (application-input-controller application)))
                                           (and controller
                                                (not
                                                 (application-input-controller-turn-active-p
                                                  controller))
                                                (not
                                                 (application-input-controller--turn-cancellation-active-p
                                                  controller)))))
                                       2)
                                      draft-preserved-observed-p
                                      (string= (line-editor-text (terminal-ui-editor ui))
                                               "draft survives")))
                           (unless cancellation-finished-observed-p
                             (with-lock-held (command-lock)
                               (setf command-released-p t)
                               (condition-notify command-gate)))
                           (queued-recording-terminal-enqueue terminal ':stream-end)))
                       :name "Autolith active command stop-key test"))
                (application-tests--serve-host application)
                (test-assert cancellation-finished-observed-p
                             "Escape cancels a shared active command and returns to input")
                (test-assert draft-preserved-observed-p
                             "command cancellation preserves the draft"))
           (with-lock-held (command-lock)
             (setf command-released-p t)
             (condition-notify command-gate))
           (when producer
             (join-thread producer)
             (setf producer nil))
           (application-command--registry-restore registrations))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
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
                       :name    "gate")))
                    (agent-test-result
                     "recovered-turn"
                     (list (agent-test-message "the next turn ran"))
                     :turn-completion ':end))))
                (registry                         (agent-test-registry))
                (tool
                  (make-instance
                   'application-test-gated-tool
                   :namespace   "test"
                   :name        "gate"
                   :description "Wait at the active-tool cancellation test gate."
                   :parameters  (tool-object-schema (json-object) '())))
                (terminal                         (make-instance 'queued-recording-terminal :columns 80))
                (ui                               (terminal-ui-create :terminal terminal))
                (agent                            (agent-create :configuration configuration
                                                                :provider      provider
                                                                :conversation  conversation
                                                                :tool-registry registry
                                                                :worker        nil))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation  conversation
                                 :provider      provider
                                 :tool-registry registry
                                 :worker        nil
                                 :agent         agent
                                 :ui            ui))
                (tool-entered-observed-p          nil)
                (cancellation-finished-observed-p nil)
                (next-turn-observed-p             nil)
                (next-turn-finished-observed-p    nil)
                (producer-condition               nil)
                (producer                         nil))
           (application-tool-call-entry tool application
                                        (agent-test-call :name "gate"))
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
                      (queued-recording-terminal-enqueue terminal ':stream-end)))
                  :name "Autolith active tool stop-key test"))
           (unwind-protect
               (application-tests--serve-host application)
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-native-tool-result-presentation () null)
(defun test-native-tool-result-presentation ()
  "Present native job identities, outcomes and progress from live and durable data."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (application  (application-tests--ui-application :columns 100)))
      (dolist
          (case
           '((:running :notice) (:failed :failure) (:cancelled :notice)
             (:completed :success)))
        (let* ((details
                (list :job-get :durable-p nil :job
                      (list :id "worker-1" :state (first case) :summary "Review complete"
                            :progress '(:duration-ms 42))))
               (record
                (list :tool-result :tool "job.get" :status ':ok :output
                      "#.(error \"never read output\")" :details details))
               (live (application-tests--render-tool-result application record)))
          (test-assert (null (conversation-record-entry application record))
                       "a successful job lookup has no standalone transcript row")
          (test-assert
           (and (search "worker-1" (test-terminal-row-text live))
                (search "42 ms" (test-terminal-row-text live))
                (search "Review complete" (test-terminal-row-text live))
                (find (second case) (rest live) :key #'terminal-span-style))
           "native job state, duration and summary have semantic spans")
          (conversation-append-tool-result conversation (make-identifier) :tool-name
                                           "job.get" :output (getf (rest record) :output)
                                           :success-p t :details details)
          (let* ((loaded (conversation-load (conversation-pathname conversation)))
                 (replayed
                  (application-tests--render-tool-result application
                                                         (first
                                                          (conversation-records-newest
                                                           loaded 1)))))
            (test-assert (equal live replayed)
                         "durable replay uses retained native details with the same spans"))))
      (let* ((output   "{\"shape\":[1,2]}")
             (details
              (list :exit-code 0 :output-streams
                    (list (list :stream ':stdout :head output :complete-p t))))
             (envelope "(:TOOL-EXECUTION :JOB (:RESULT NIL))"))
        (dolist
            (case
             (list (list "test.output" output nil output)
                   (list "shell.run" envelope details output)
                   (list "shell.run" envelope nil "result details unavailable")))
          (destructuring-bind
              (tool text metadata expected)
              case
            (let* ((success-p (equal tool "shell.run"))
                   (record
                    (list :tool-result :tool tool :status
                          (if success-p
                              ':ok
                              ':error)
                          :output text :details metadata)))
              (conversation-append-tool-result conversation (make-identifier) :tool-name
                                               tool :output text :success-p success-p
                                               :details metadata)
              (let* ((loaded (conversation-load (conversation-pathname conversation)))
                     (replay (first (conversation-records-newest loaded 1))))
                (dolist (source (list record replay))
                  (let ((shown
                         (test-terminal-row-text
                          (conversation-record-entry application source))))
                    (test-assert
                     (and (search expected shown) (not (search envelope shown)))
                     "live and replay preserve literal text without displaying a missing native envelope")))))))
        (let* ((entry
                (conversation-record-entry application
                                           (list :message :role ':user :automatic-p t
                                                 :content envelope :job-completion
                                                 (list :job-id "exec:19" :outcome
                                                       ':success :summary envelope
                                                       :transcript-shell
                                                       (list :details details)))))
               (shown (test-terminal-row-text entry)))
          (test-assert
           (and (search "completed" shown) (not (search output shown))
                (not (search envelope shown)))
           "a successful background shell completion omits capture and envelope"))))))

(-> test-native-continuity-presentation nil null)
(defun test-native-continuity-presentation ()
                                           "Show completed jobs with failed retained outcomes."
                                           (let ((application (application-tests--ui-application :columns 100)))
                                             (dolist
                                                 (case
                                                  (list
                                                   (list "job.continuity"
                                                         '(:classification :completed :job-id "completed-worker" :result
                                                           (:status :failure :error "gate rejected"))
                                                         "completed-worker" "gate rejected")))
                                               (let* ((record
                                                       (list :tool-result :tool (first case) :status ':error :output
                                                             "native output" :details (second case)))
                                                      (entry (conversation-record-entry application record))
                                                      (text  (test-terminal-row-text entry)))
                                                 (test-assert
                                                  (and (search (third case) text) (search (fourth case) text)
                                                       (find ':failure (rest entry) :key #'terminal-span-style))
                                                  "native continuity outcomes retain typed failure information")))
                                             (let ((snapshot
                                                    (list :job-id "json-progress" :state ':running :progress
                                                          (list :request-count 2 :usage (json-object "total_tokens" 10)))))
                                               (test-assert
                                                (search "json-progress"
                                                        (test-terminal-row-text
                                                         (apply #'append
                                                                (application--native-snapshot-rows application snapshot))))
                                                "native snapshot projection accepts nested JSON usage objects"))))

(-> test-native-tool-result-presentation-bounds () null)
(defun test-native-tool-result-presentation-bounds ()
  "Bound native details and literal tool text at the transcript boundary."
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (details '(nil :corrupt (:job) (:job-get :job) (:job-get :job (:id))))
      (let ((entry (conversation-record-entry
                    application (list :tool-result :tool "job.get" :status ':error
                                      :output "fallback" :details details))))
        (test-assert (search "fallback" (test-terminal-row-text entry))
                     "malformed or absent native details use bounded text fallback")))
    (let ((snapshot (list :job-id "cycle" :classification ':failed)))
      (setf (getf snapshot :progress) snapshot)
      (test-assert (search "cycle" (test-terminal-row-text
                                    (apply #'append (application--native-snapshot-rows application snapshot))))
                   "cyclic native progress is bounded before presentation")))
  (let* ((summary (make-string 10000 :initial-element #\x))
         (items   (loop repeat 1000 collect (list :id "bounded" :state ':failed :summary summary)))
         (record  (list :tool-result :tool "child.spawn" :status ':error
                        :output "native output" :details (list :task-run :results items))))
    (dolist (width '(1 4 12))
      (let* ((application (application-tests--ui-application :columns width))
             (entry       (conversation-record-entry application record))
             (lines       (application--display-lines (test-terminal-row-text entry))))
        (test-assert (< (length lines) 40) "native result rows are bounded independently of input size")
        (test-assert (every (lambda (line) (<= (text-cell-width line) width)) lines)
                     "native result rows fit narrow terminals"))))
  (let* ((application (application-tests--ui-application :columns 12))
         (output      (concatenate 'string (make-string 10000 :initial-element #\[)
                                   "0" (make-string 10000 :initial-element #\])))
         (entry       (conversation-record-entry
                       application (list :tool-result :tool "unknown.run" :status ':error :output output))))
    (let ((text (test-terminal-row-text entry)))
      (test-assert (and (search "[[[[" text) (< (length text) 100))
                   "JSON-shaped tool text remains literal and bounded"))))

(-> test-lisp-rainbow-parentheses () null)
(defun test-lisp-rainbow-parentheses ()
  "Color only structural Lisp parentheses by depth, without reading the input."
  ;; Brackets mark structural parentheses; literal parentheses must not gain roles.
  (dolist (case (list '("[loop for x in '[1 2] collect x]" "1221")
                     '("[[a] [b]] [c]" "12222111")
                     (list (format nil "[let [[x 1]]~% [list x]]") "12332221")
                     '("[list \"(literal)\" [x]]" "1221")
                     '("[[[[[[[[x]]]]]]]]" "1234561221654321")
                     '("[" "1") '("]" "1")))
    (destructuring-bind (marked depths) case
      (let* ((source   (substitute #\) #\] (substitute #\( #\[ marked)))
             (spans    (syntax--highlight-spans source
                                                :language              *application-common-lisp-language*
                                                :rainbow-parentheses-p t))
             (expected (loop for character across marked
                             for index from 0
                             when (find character "[]") collect index))
             (actual   (loop with offset = 0
                             for span in spans
                             append (when (member (terminal-span-style span)
                                                  *terminal-parenthesis-style-names*)
                                      (loop for index below (length (terminal-span-text span))
                                            collect (+ offset index)))
                             do (incf offset (length (terminal-span-text span))))))
        (test-assert (equal actual expected)
                     (format nil "Only structural parentheses receive depth roles in ~S" source))
        (test-assert (equal (loop for span in spans
                                 when (member (terminal-span-style span)
                                              *terminal-parenthesis-style-names*)
                                   append (make-list (length (terminal-span-text span))
                                                     :initial-element (terminal-span-style span)))
                            (loop for digit across depths
                                  collect (nth (1- (digit-char-p digit))
                                               *terminal-parenthesis-style-names*)))
                     "Matching delimiters share a depth color across forms and lines.")))))

(-> test-lisp-rainbow-display-scope () null)
(defun test-lisp-rainbow-display-scope ()
  "Apply nesting colors to Lisp tool and local input, source and values only."
  (let* ((rows   (application--source-preview-rows
                  (format nil "(list~% (list 1))")
                  :language              *application-common-lisp-language*
                  :prompt                "(p) "
                  :rainbow-parentheses-p t))
         (prompt (second (first rows))))
    (test-assert (and (eq ':dim (terminal-span-style prompt))
                      (string= "(p) " (terminal-span-text prompt))
                      (find ':syntax-parenthesis-2 (second rows) :key #'terminal-span-style))
                 "The Lisp palette applies across source lines, not to the prompt."))
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (namespace '("lisp" "self"))
      (let ((entry (response-item-entry
                    application
                    (json-object "type" "function_call" "namespace" namespace "name" "eval"
                                 "arguments" (json-encode
                                              (json-object "forms" (vector "(list (list 1))")))))))
        (test-assert (find ':syntax-parenthesis-2 entry :key #'terminal-span-style)
                     "Lisp evaluation tool inputs receive nesting colors.")))
    (dolist (name '("lisp.eval" "self.eval" "lisp.describe" "lisp.source" "self.diff"))
      (let* ((source-p (member name '("lisp.source" "self.diff") :test #'equal))
             (entry    (application-tests--render-tool-result
                        application (list :tool-result :tool name :status ':ok
                                          :output  (if source-p "(list (list 1))" "")
                                          :details (unless source-p
                                                     '(:evaluation (:output "(captured output)" :values "((1))")))))))
        (test-assert (eq (not (equal name "self.diff"))
                         (not (null (find ':syntax-parenthesis-2 entry :key #'terminal-span-style))))
                     "Dedicated Lisp source and values receive nesting colors, excluding diffs.")
        (unless source-p
          (test-assert (find-if (lambda (span)
                                  (and (eq ':plain (terminal-span-style span))
                                       (search "(captured output)" (terminal-span-text span)))) entry)
                       "Captured output has no nesting colors."))))
    (dolist (spans (list (application-lisp-code-spans "((1))")
                        (application-lisp--value-spans "((1))")))
      (test-assert (find ':syntax-parenthesis-2 spans :key #'terminal-span-style)
                   "Local Lisp input and rendered values receive nesting colors."))))

(-> test-lisp-source-result-highlighting () null)
(defun test-lisp-source-result-highlighting ()
  "Present returned Lisp source and mutation forms using syntax token roles."
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (name '("lisp.source" "self.diff"))
      (let* ((record (list :tool-result :tool name :status ':ok
                           :output "(defun greet () \"hello\")"))
             (entry  (application-tests--render-tool-result application record)))
        (test-assert (and (find ':syntax-keyword entry :key #'terminal-span-style)
                          (find ':syntax-function entry :key #'terminal-span-style)
                          (find ':syntax-string entry :key #'terminal-span-style))
                     "Lisp result previews retain keyword, function and string token styles")))))

(-> test-evaluation-value-highlighting () null)
(defun test-evaluation-value-highlighting ()
  "Highlight Lisp values separately from arbitrary captured output."
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (name '("lisp.eval" "lisp.scratchpad-run" "self.eval" "lisp.describe"))
      (let* ((record
                (list :tool-result :tool name :status ':ok
                      :details '(:evaluation (:output "a captured message" :values "(:answer 42 :text \"hello\")"))))
             (entry (application-tests--render-tool-result application record)))
        (test-assert (and (find ':syntax-number entry :key #'terminal-span-style)
                          (find ':syntax-string entry :key #'terminal-span-style))
                     "Evaluation values retain Lisp token roles.")
        (test-assert (search "a captured message" (test-terminal-row-text entry))
                     "Captured output is presented independently of Lisp values.")))))

(-> test-tool-output-literal-presentation () null)
(defun test-tool-output-literal-presentation ()
  "Preserve literal output, rails, terminal safety and row bounds."
  (let* ((application (application-tests--ui-application :columns 100))
         (json        "{\"answer\":42,\"message\":\"hello\"}"))
    (dolist (output (list (format nil "```json~%~A~%```" json)
                          "(:state :ready :message \"hello\")"))
      (let ((entry (application-tests--render-tool-result
                    application (list :tool-result :tool "unknown.run" :status ':ok :output output))))
        (test-assert (every (lambda (line) (search line (test-terminal-row-text entry)))
                            (application--display-lines output))
                     "code-like text and fences are presented literally")
        (test-assert (find-if (lambda (span) (search "│" (terminal-span-text span))) entry)
                     "Formatted tool content retains a left rail.")))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :tool "shell.run" :status ':ok
                         :details (list :exit-code 0 :output-streams
                                        (list (list :stream ':combined :head json :complete-p t)))))))
      (test-assert (and (search json (test-terminal-row-text entry))
                        (not (find ':syntax-number entry :key #'terminal-span-style)))
                   "JSON-looking stdout is presented literally"))
    (let* ((output (format nil "```lisp~%#.(error ~S)~%~C[2J~%```" "do not read tool text" #\Esc))
           (entry  (application-tests--render-tool-result
                    application (list :tool-result :tool "unknown.run" :status ':ok :output output))))
      (test-assert (not (find #\Esc (test-terminal-row-text entry)))
                   "Code output is terminal-safe and is never evaluated.")
      (test-assert (and (search "do not read tool text" (test-terminal-row-text entry))
                        (notany #'termdown:widget-p entry))
                   "fenced text is not treated as an executable or copyable code block"))
    (let* ((*application-tool-output-lines* 4)
           (output                          (format nil "```json~%[~%~{~A~%~}]~%```" (make-list 30 :initial-element "42,")))
           (entry                           (application-tests--render-tool-result
                                             application (list :tool-result :tool "unknown.run" :status ':ok :output output))))
      (test-assert (<= (count #\Newline (test-terminal-row-text entry)) 6)
                   "Fenced output obeys the tool row budget."))))

(-> test-raw-diff-output-presentation () null)
(defun test-raw-diff-output-presentation ()
  "Preserve captured diff prefixes and indentation beneath one output rail."
  (let* ((application (application-tests--ui-application :columns 100))
         (lines       '("diff --git a/demo.lisp b/demo.lisp"
                        "index abcdef0..1234567 100644"
                        "--- a/demo.lisp"
                        "+++ b/demo.lisp"
                        "@@ -1,2 +1,2 @@"
                        " (in-package #:demo)"
                        "-(defun greet () \"before\")"
                        "+(defun greet () \"after\")"
                        "\\ No newline at end of file"
                        "diff --git a/notes.txt b/notes.txt"
                        "--- a/notes.txt"
                        "+++ b/notes.txt"
                        "@@ -1,2 +1,2 @@"
                        " "
                        "-  old text"
                        "+  new text"))
         (diff        (format nil "~{~A~^~%~}" lines)))
    (dolist (case '(("unknown.run" nil 24)
                    ("shell.run" "combined output" 24)
                    ("shell.run" "stderr" 24)
                    ("shell.run" "stdout" 6)))
      (destructuring-bind (tool stream limit) case
        (let* ((*application-tool-output-lines* limit)
               (entry                           (application-tests--render-tool-result
                                                 application
                                                 (list :tool-result :tool tool :status ':ok :output diff
                                                       :details (when stream
                                                                  (list :exit-code 0 :output-streams
                                                                        (list (list :stream (cond
                                                                                              ((string= stream "stderr")
                                                                                               ':stderr)
                                                                                              ((string= stream "stdout")
                                                                                               ':stdout)
                                                                                              (t
                                                                                               ':combined))
                                                                                    :head diff :complete-p t)))))))
               (rendered                        (application--display-lines (test-terminal-row-text entry)))
               (railed                          (remove-if-not (lambda (line) (position #\│ line)) rendered))
               (columns                         (mapcar (lambda (line) (position #\│ line)) railed))
               (payload                         (mapcar (lambda (line) (subseq line (+ 2 (position #\│ line))))
                                                        railed)))
          (test-assert (and columns (apply #'= columns))
                       "metadata, context and change lines share one output rail")
          (test-assert (equal payload (subseq lines 0 (min limit (length lines))))
                       "bounded output preserves each diff line including its original prefix"))))))

(-> test-resource-source-highlighting () null)
(defun test-resource-source-highlighting ()
  "Render retained numbered file contents without depending on live file state."
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (case '(("source.lisp" "(defun greet () \"hello\")" :syntax-function)
                    ("data.json" "{\"answer\":42}" :syntax-number)
                    ("task.sh" "echo \"hello\"" :syntax-string)
                    ("notes.txt" "plain text" :plain)))
      (destructuring-bind (path source role) case
        (let* ((details (list :uri   (format nil "workspace:~A" path)
                              :lines (list (list 1 (cl-hashline:line-anchor source) source))))
               (record  (list :tool-result :tool "resource.read" :status ':ok
                              :details (list :resource-read details)))
               (entry   (application-tests--render-tool-result application record))
               (text    (test-terminal-row-text entry)))
          (test-assert (and (search source text)
                            (search (cl-hashline:line-anchor source) text)
                            (find role entry :key #'terminal-span-style))
                       "File contents preserve source, anchors and path-specific syntax.")
          (test-assert (notany (lambda (span)
                                 (member (terminal-span-style span)
                                         *terminal-parenthesis-style-names*)) entry)
                       "Resource source previews do not use Lisp nesting colors.")
          (test-assert (null (conversation-record-entry application record))
                       "successful resource reads omit their standalone result"))))))

(-> test-search-source-highlighting () null)
(defun test-search-source-highlighting ()
  "Highlight retained source snippets while preserving match locations and text."
  (let ((application (application-tests--ui-application :columns 100)))
    (dolist (path '("src/demo.lisp" "C:\\work\\demo.lisp"))
      (let* ((source "(defun greet () \"hello\")")
             (output (format nil "1 match~%~A:12:3 [git:clean]~%  > 12  ~A" path source))
             (entry  (application-tests--render-tool-result
                      application (list :tool-result :tool "search.content" :status ':ok :output output))))
        (test-assert (and (search source (test-terminal-row-text entry))
                          (search (format nil "~A:12:3" path) (test-terminal-row-text entry))
                          (find ':syntax-function entry :key #'terminal-span-style)
                          (find ':syntax-string entry :key #'terminal-span-style))
                     "Search source preserves paths and coordinates while exposing syntax roles.")))))

(-> test-lsp-result-visibility () null)
(defun test-lsp-result-visibility ()
  "Hide successful diagnostics and display failed transport output literally."
  (let ((application (application-tests--ui-application :columns 120)))
    (dolist (severity '(1 2 3 4))
      (let* ((output (lsp-tool--render
                      (vector (json-object "server" "working" "result"
                                           (json-object "diagnostics"
                                                        (vector (json-object "severity" severity
                                                                             "message" "diagnostic")))))))
             (record (list :tool-result :tool "lsp.diagnostics" :status ':ok :output output)))
        (test-assert (null (conversation-record-entry application record))
                     "successful diagnostics do not have a standalone row")
        (setf (getf (rest record) :status) ':error)
        (let ((entry (conversation-record-entry application record)))
          (test-assert (and (eq (terminal-span-style (first entry)) ':failure)
                            (search output (test-terminal-row-text entry)))
                       "failed diagnostics show the literal result beneath the failure header"))))))

(-> test-tool-execution-handoff-presentation () null)
(defun test-tool-execution-handoff-presentation ()
  "Present actual tool handoffs and their durable replay as bounded background state."
  (job-completion-tests--fixture
   (lambda (parent runtime configuration)
     (declare (ignore configuration))
     (let ((lock       (make-lock "Handoff presentation gate"))
           (gate       (make-condition-variable))
           (released-p nil)
           (compact    (application-tests--ui-application :columns 100)))
       (setf (application-conversation compact) (agent-conversation parent))
       (unwind-protect
            (loop for async-p in '(t nil)
                  for reason in '(:requested :grace-expired)
                  do
                  (let* ((*tool-execution-blocking-grace-seconds* 0)
                         (result
                           (tool-execution-invoke
                            runtime parent
                            :tool-name   "shell.run"
                            :description "Run repository checks"
                            :summary     "check"
                            :operation-function
                            (lambda ()
                              (with-lock-held (lock)
                                (loop until released-p
                                      do (condition-wait gate lock)))
                              (tool-success "checks passed"))
                            :async-p     async-p))
                         (details                                 (tool-result-details result))
                         (identifier                              (getf (getf (rest details) :job) :id))
                         (conversation                            (agent-conversation parent)))
                    (test-assert
                     (eq reason (getf (rest details) :handoff-reason))
                     "explicit async and expired blocking waits produce native handoffs")
                    (conversation-append-tool-result
                     conversation identifier :tool-name "shell.run"
                     :output (tool-result-content result) :details details
                     :success-p t :cpu-microseconds 2000 :real-microseconds 1000)
                    (let* ((loaded (conversation-load (conversation-pathname conversation)))
                           (record (first (conversation-records-newest loaded 1)))
                           (entry  (conversation-record-entry compact record))
                           (text   (test-terminal-row-text entry)))
                      (test-assert
                       (and (eq (terminal-span-style (first entry)) ':notice)
                            (search identifier text)
                            (search "background" text)
                            (< (length text) 200)
                            (not (find #\Newline text)))
                       "durable handoffs show background state and job identity in one compact row")
                      (let* ((widgets (remove-if-not #'termdown:widget-p entry))
                             (action  (list ':insert
                                            (format nil "(job.cancel :id ~S)" identifier))))
                        (test-assert
                         (and (search (format nil "(job.cancel :id ~S)" identifier) text)
                              (> (length widgets) 1)
                              (find ':syntax-function widgets :key #'termdown:widget-role)
                              (every (lambda (widget)
                                       (equal (termdown:widget-action widget) action))
                                     widgets)
                              (string= (terminal--spans-text widgets)
                                       (terminal-ui--job-cancel-text identifier))
                              (equal "handoff shell.run"
                                      (getf (third (application--record-presentation compact record))
                                            :group)))
                         "a running handoff highlights every clickable cancellation token"))
                      (test-assert
                       (not (or (search "Run repository checks" text)
                                (search "blocking wait expired" text)))
                       "handoffs omit the submitted description and wait explanation")
                      (dolist (tool-name '("shell.run" "lisp.load-system" "unregistered.run"))
                        (dolist (case '((:queued "queued" :notice)
                                        (:running "running" :notice)
                                        (:completed "result ready" :notice)
                                        (:failed "failed" :failure)
                                        (:cancelled "cancelled" :notice)))
                          (let* ((variant (copy-tree record))
                                 (job     (getf (rest (getf (rest variant) :details)) :job)))
                            (setf (getf (rest variant) :tool) tool-name
                                  (getf job :state) (first case)
                                  (getf job :description) (make-string 4000 :initial-element #\x))
                            (let* ((entry (conversation-record-entry compact variant))
                                   (text  (test-terminal-row-text entry)))
                              (test-assert
                               (and (application--record-visible-p compact variant)
                                    (eq (terminal-span-style (first entry)) (third case))
                                    (search (second case) text)
                                    (search identifier text)
                                    (< (length text) 200)
                                    (not (find #\Newline text)))
                               "background status is visible for specialized, normally hidden and missing tools")))))
                      (let ((text-only (copy-tree record)))
                        (remf (rest text-only) :details)
                        (test-assert
                         (null (application--tool-execution-handoff text-only))
                         "ordinary output that resembles an envelope is not decoded as orchestration metadata"))
                      (dolist (invalid '((:tool-execution :handed-off-p nil :job (:id "exec:1"))
                                         (:tool-execution :handed-off-p t :job 7)))
                        (let ((malformed (copy-tree record)))
                          (setf (getf (rest malformed) :details) invalid)
                          (test-assert
                           (null (application--tool-execution-handoff malformed))
                           "invalid or non-handoff metadata uses the ordinary output renderer"))))))
         (with-lock-held (lock)
           (setf released-p t)
           (task--condition-broadcast gate))))))
  nil)

(-> test-transcript-entries () null)
(defun test-transcript-entries ()
  "Test styled transcript entry construction, wrapping, and output bounds."
  (let ((application (application-tests--ui-application :columns 40)))
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
                :columns            40
                :reasoning-traces-p t))
             (entry (response-item-entry visible-application item))
             (text  (test-terminal-row-text entry)))
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
                :columns            20
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
    (let* ((item
             (json-object
              "type" "tool_search_output"
              "tools"
              (json-array
               (json-object "type" "namespace"
                            "name" "resource"
                            "tools" (json-array))
               (json-object
                "type" "namespace"
                "name"
                (format nil "unsafe~%~C[31mnamespace"
                        *terminal-escape-character*)
                "tools" (json-array)))))
           (entry (response-item-entry application item))
           (text  (test-terminal-row-text entry)))
      (test-assert
       (and (application--response-item-visible-p application item)
            (= (count (terminal-span ':tool "▸ tool schemas: ")
                      entry :test #'equal)
               1)
            (find (terminal-span ':syntax-function "resource")
                  entry
                  :test #'equal)
            (= (count-if (lambda (span)
                           (eq (terminal-span-style span) ':syntax-function))
                         entry)
               2)
            (not (find (terminal-span ':plain (string #\Newline))
                       entry :test #'equal)))
       "tool discovery collapses expanded namespaces into one compact schema event")
      (test-assert
       (and (not (find *terminal-escape-character* text))
            (every (lambda (line)
                     (<= (text-cell-width line) 39))
                   (uiop:split-string text :separator '(#\Newline))))
       "tool discovery sanitizes and bounds provider-controlled namespaces"))
    (let ((empty-item
            (json-object "type" "tool_search_output"
                         "tools" (json-array))))
      (test-assert
       (and (null (response-item-entry application empty-item))
            (not (application--response-item-visible-p application empty-item)))
       "empty tool discovery output has no transcript entry"))
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
                (<= (termdown:spans-width row) 29))
              rows)
       "tool detail columns stay inside their transcript cell budget"))
    (let* ((call
             (json-object
              "type" "function_call"
              "namespace" "resource"
              "name" "read"
              "arguments" "{\"uri\":\"workspace:x\"} trailing"))
           (wide-application
             (application-tests--ui-application :columns 80))
           (entry (response-item-entry wide-application call))
           (text  (test-terminal-row-text entry)))
      (test-assert
       (and (equal (first entry) (terminal-span :tool "▸ (resource.read)"))
            (not (find #\Newline text)))
       "malformed provider arguments retain the tool header without fabricated fields"))
    (let* ((narrow-application
             (application-tests--ui-application :columns 20))
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
    (let ((inline (response-item-entry
                   (application-tests--ui-application :columns 100)
                   (json-object
                    "type" "function_call" "namespace" "odd" "name" "tool"
                    "arguments" "{\"z\":false,\"a key)\":true,\"nested\":{\"value\":null}}"))))
      (test-assert (string= (test-terminal-row-text inline)
                            "▸ (odd.tool :|a key)| true :nested.value null :z false)")
                   "short call arguments fold into one header line"))
    (let* ((entry
             (response-item-entry
              (application-tests--ui-application :columns 40)
              (json-object
               "type" "function_call" "namespace" "odd" "name" "tool"
               "arguments" "{\"z\":false,\"a key)\":true,\"nested\":{\"value\":null}}")))
           (lines         (rest (uiop:split-string (test-terminal-row-text entry)
                                                   :separator '(#\Newline))))
           (value-columns (mapcar #'search '("true" "null" "false") lines)))
      (test-assert
       (and (search ":|a key)|" (first lines))
            (search ":nested.value" (second lines))
            (search ":z" (third lines))
            (search "true" (first lines))
            (search "null" (second lines))
            (search "false" (third lines)))
       "generic argument tables escape keyword labels and preserve nested JSON values")
      (test-assert (apply #'= value-columns)
                   "generic argument values share one column across nested fields"))
    (dolist (columns '(24 40 100))
      (dolist (uri '("memory:workspace" "workspace:example" "extension:example"))
        (let* ((entry
                 (response-item-entry
                  (application-tests--ui-application :columns columns)
                  (json-object
                   "type" "function_call" "namespace" "resource" "name" "edit"
                   "arguments"
                   (json-encode
                    (json-object
                     "uri" uri "base-revision" "Rfixture"
                     "operations"
                     (vector (json-object "op" "memory-remember"
                                          "title" "TITLE" "scope" "global"
                                          "tags" (vector "TAG")
                                          "content" (format nil "first content~%second content"))))))))
               (lines (uiop:split-string (test-terminal-row-text entry)
                                         :separator '(#\Newline)))
               (value-columns
                 (loop for value in '("global" "TAG" "TITLE")
                       for row = (find-if (lambda (line) (search value line)) lines)
                       collect (and row (search value row)))))
          (test-assert
           (and (every #'integerp value-columns) (apply #'= value-columns))
           (format nil "~A operation fields share a value column at ~D columns"
                   uri columns))
          (test-assert
           (and (or (< columns 40)
                    (search ":base-revision" (test-terminal-row-text entry)))
                (position-if (lambda (line) (search "first content" line)) lines)
                (every (lambda (line) (< (text-cell-width line) columns)) lines))
           "resource operations retain argument names, multiline previews and cell bounds"))))
    (let* ((source (format nil "~{form-line-~D~^~%~}"
                           (loop for index from 1 to 10 collect index)))
           (entry  (response-item-entry
                    application
                    (json-object
                     "type" "function_call"
                     "namespace" "self"
                     "name" "eval"
                     "arguments" (json-encode
                                  (json-object
                                   "forms" (vector source)
                                   "restart" "CONTINUE")))))
           (text   (test-terminal-row-text entry)))
      (test-assert
       (and (string= (first (uiop:split-string text :separator '(#\Newline)))
                     "▸ (self.eval :restart \"CONTINUE\")")
            (search "│ form-line-1" text))
       "provider calls fold short arguments into their header above body rows")
      (test-assert (and (search "form-line-1" text)
                        (search "… +2 more lines" text))
                   "self.eval previews only the first configured source lines")
      (test-assert (and (search "restart" text)
                        (find-if (lambda (span)
                                   (and (eq (terminal-span-style span) ':notice)
                                        (search "CONTINUE"
                                                (terminal-span-text span))))
                                 entry))
                   "self.eval presents the selected restart in its argument table")
      (test-assert (not (search "{\"forms\"" text))
                   "tool requests never expose raw argument JSON"))
    (let* ((entry (response-item-entry
                   application
                   (json-object
                    "type" "function_call"
                    "namespace" "lisp"
                    "name" "eval"
                    "arguments" (json-encode
                                 (json-object "forms" (vector "(+ 1 2)"))))))
           (text  (test-terminal-row-text entry)))
      (test-assert
       (and (eq (terminal-span-style (first entry)) ':tool)
            (string= (terminal-span-text (first entry)) "▸ (lisp.eval)")
            (search "(+ 1 2)" text)
            (not (search "{\"forms\"" text)))
       "lisp.eval calls show a parenthesized name above their source rail"))
    (let* ((entry (response-item-entry
                   (application-tests--ui-application :columns 100)
                   (json-object
                    "type" "function_call"
                    "namespace" "shell"
                    "name" "run"
                    "arguments" (json-encode
                                 (json-object
                                  "command" "printf hello && printf world"
                                  "directory" "/tmp/work"
                                  "timeout-seconds" 30)))))
           (text  (test-terminal-row-text entry)))
      (let ((lines (uiop:split-string text :separator '(#\Newline))))
        (test-assert
         (and (string= (first lines)
                       "▸ (shell.run :directory \"/tmp/work\" :timeout-seconds 30)")
              (search "$ printf" (second lines)))
         "shell source begins immediately after the inline arguments"))
      (let ((rail (find-if (lambda (span) (search "│" (terminal-span-text span))) entry)))
        (test-assert (and rail (eq (terminal-span-style rail) ':dim))
                     "the first shell command rail uses dim styling")))
    (dolist (columns '(24 40 100))
      (dolist (case (list (list "shell" "run"
                               (json-object "command" "printf hello" "directory" "work"))
                         (list "lisp" "eval"
                               (json-object "forms" (vector "(+ 1 2)") "repl" "sample"))
                         (list "self" "set"
                               (json-object "symbol" "*sample*" "value" "(list :x 1)"))))
        (destructuring-bind (namespace name arguments) case
          (let* ((entry
                   (response-item-entry
                    (application-tests--ui-application :columns columns)
                    (json-object "type" "function_call" "namespace" namespace "name" name
                                 "arguments" (json-encode arguments))))
                 (lines (uiop:split-string (test-terminal-row-text entry)
                                           :separator '(#\Newline))))
            (test-assert
             (and (if (< columns 40)
                      (and (search "  :" (second lines))
                           (search "│ " (third lines)))
                      (and (search (format nil "▸ (~A.~A :" namespace name) (first lines))
                           (search "│ " (second lines))))
                  (find ':syntax-keyword entry :key #'terminal-span-style)
                  (every (lambda (line) (< (text-cell-width line) columns)) lines))
             (format nil "~A.~A arguments touch the source rail at ~D columns"
                     namespace name columns))))))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"web_search_call\",
                     \"action\":{\"type\":\"search\",
                                 \"query\":\"live lisp images\"}}"))))
      (test-assert (equal (first entry) (terminal-span :tool "▸ (web search)"))
                   "web search calls present a styled search header")
      (test-assert (search "live lisp images"
                           (test-terminal-row-text entry))
                   "web search entries show their query"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 2 :time 0 :call-id 1
                         :tool "shell.run" :status ':ok
                         :cpu-microseconds  1234
                         :real-microseconds 567890
                         :output            "wrote file")))
           (text  (test-terminal-row-text entry)))
      (test-assert (and (search "cpu 0.001s" text)
                        (search "real 0.568s" text))
                   "tool results show CPU and real elapsed time"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 3 :time 0 :call-id 2
                         :tool "shell.run" :status ':ok
                          :details '(:exit-code 3 :output-streams
                                     ((:stream :combined :head "command output" :complete-p t))))))
           (text  (test-terminal-row-text entry)))
      (test-assert (and (search "exit 3" text)
                        (search "command output" text))
                   "shell.run results separate exit status from command output"))
    (dolist (case
              (list
               (list "a clean run" ':ok
                     '(:exit-code 0 :output-streams
                       ((:stream :combined :head "hello world" :complete-p t)))
                     ':success "✓ shell.run exit 0" '("hello world"))
               (list "a failing exit" ':ok
                     '(:exit-code 2 :output-streams
                       ((:stream :combined :head "oops" :complete-p t)))
                     ':failure "! shell.run exit 2" '("oops"))
               (list "a timeout with truncated separate streams" ':error
                     '(:execution-status :timed-out :timed-out-p t
                       :output-streams ((:stream :stdout :head "partial" :complete-p nil)
                                        (:stream :stderr :head "boom" :complete-p t)))
                     ':failure "! shell.run exit unavailable"
                     '("timed out" "output truncated" "stdout" "partial" "stderr" "boom"))))
      (destructuring-bind (label status details style header shown) case
        (let* ((entry (conversation-record-entry
                       application
                       (list :tool-result :seq 7 :time 0 :call-id "shell"
                              :tool "shell.run" :status status :details details)))
               (text  (test-terminal-row-text entry)))
          (test-assert
           (and (equal (first entry) (terminal-span style header))
                (every (lambda (expected) (search expected text)) shown)
                (notany (lambda (metadata) (search metadata text))
                        '("Execution" "bytes retained" "shell-log:" "Log:")))
           (format nil "shell.run shows ~A without capture metadata: ~S" label text)))))
    (let* ((entry (conversation-record-entry
                   (application-tests--ui-application :columns 100)
                   (list :tool-result :seq 8 :time 0 :call-id "edit"
                         :tool "resource.edit" :status ':ok
                         :details (list :resource-edit (list :summary             "Applied replace-lines 3-4."
                                                             :delimiter-warning-p t)))))
           (text  (test-terminal-row-text entry)))
      (test-assert (and (search "Applied replace-lines 3-4" text)
                        (search "Lisp delimiter warning" text)
                        (= (count #\Newline text) 1))
                   "resource.edit shows one compact delimiter notice after the edit outcome"))
    (let* ((entry (conversation-record-entry
                   (application-tests--ui-application :columns 100)
                   (list :tool-result :seq 9 :time 0 :call-id "parens"
                         :tool "lisp.paren-check" :status ':ok
                         :output (format nil "Path: src/~%Checked 1,204 of 1,204 recognized ~
                                              Lisp-family files.~%No unmatched or mismatched ~
                                              delimiters found."))))
           (text  (test-terminal-row-text entry)))
      (test-assert (and (search "1,204 files balanced" text)
                        (not (find #\Newline text)))
                   "a clean paren check is one line"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 4 :time 0 :call-id 3
                         :tool "self.eval" :status ':ok
                          :details '(:evaluation (:output "hello" :values "42")))))
           (text  (test-terminal-row-text entry)))
      (test-assert (equal (first entry) (terminal-span :success "✓ self.eval"))
                   "successful tool results present a success header")
      (test-assert (and (search "output" text)
                        (search "hello" text)
                        (search "values" text)
                        (find (terminal-span :syntax-number "42") entry :test #'equal))
                   "self.eval results separate captured output from values"))
    (let* ((entry (application-tests--render-tool-result
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "lisp.describe" :status ':ok
                          :details (list :evaluation
                                         (list :output (format nil "Symbol: CAR~%Documentation: list head")
                                               :values "")))))
           (text  (test-terminal-row-text entry)))
      (test-assert (and (search "description" text)
                        (search "Symbol" text)
                        (search "CAR" text)
                        (search "values" text))
                   "lisp.describe results separate structured description and values"))
    (dolist (case '(("resource.read"
                     "resource.read failed: byte-count is out of range."
                     "byte-count is out of range.")
                    ("resource.read" "File not found." "File not found.")
                    ("resource.read" "backend.run failed: unavailable"
                     "backend.run failed: unavailable")
                    ("unknown.run" "unknown.run failed: resource.read failed: denied"
                     "resource.read failed: denied")))
      (destructuring-bind (tool output reason) case
        (let ((entry (conversation-record-entry
                      application
                      (list :tool-result :tool tool :status ':error :output output))))
          (test-assert
           (and (equal (first entry)
                       (terminal-span ':failure (format nil "! ~A failed" tool)))
                (find (terminal-span ':plain reason) entry :test #'equal))
           "the failure header identifies the tool and its body shows only the reason"))))
    (let* ((cause    "No file exists at workspace:gone.lisp; resource.edit can create it.")
           (registry (make-instance 'tool-registry))
           (entry    (progn
                       (tool-registry-register registry (conversation-resource-tests--read-tool))
                       (setf (application-tool-registry application) registry)
                       (conversation-record-entry
                        application
                        (list :tool-result :tool "resource.read" :status ':error
                              :details (list :resource-read (list :cause "No file exists at workspace:gone.lisp"))
                              :output  (format nil "resource.read failed: ~A~%URI: workspace:gone.lisp~%Revision: R1~%Kind: missing~%Elided: none~%Content:~%"
                                               cause)))))
           (text     (test-terminal-row-text entry)))
      (test-assert (and (find-if (lambda (span)
                                   (uiop:string-prefix-p "No file exists at workspace:"
                                                         (terminal-span-text span)))
                                 entry)
                        (not (search "Revision" text)))
                   "a missing file read shows its cause, not the observation the model receives"))
    (let* ((entry (conversation-record-entry
                   application
                   (list :tool-result :seq 6 :time 0 :call-id 5
                         :tool "self.eval" :status ':error
                          :details '(:evaluation (:condition "Needs a value."
                                                  :restarts  (("CONTINUE" "Try again.")
                                                              ("USE-VALUE" "Supply a value."))
                                                  :retry     "Retry the identical call with a restart.")))))
           (text  (test-terminal-row-text entry)))
      (test-assert (equal (first entry)
                          (terminal-span :failure "! self.eval failed"))
                   "failed tool results present a failure header")
      (test-assert (and (find (terminal-span ':plain "Needs a value.") entry :test #'equal)
                        (search "condition" text)
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
  (let* ((application (application-tests--ui-application))
         (help        (application-operation-help application "rollback")))
      (test-assert (search "rollback" help) "help resolves a registered recovery operation"))
  nil)

(-> test-tool-call-inline-arguments () null)
(defun test-tool-call-inline-arguments ()
  "Fit complete arguments by terminal cells and retain wrapped argument tables."
  (dolist (value '("sample" "界é" "" "a\"b\\c"))
    (let* ((expected (format nil "▸ (odd.tool :value ~S)" value))
           (width    (text-cell-width expected)))
      (dolist (fits-p '(nil t))
        (let* ((columns (+ width (if fits-p 1 0)))
               (entry   (response-item-entry
                         (application-tests--ui-application :columns columns)
                         (json-object "type" "function_call" "namespace" "odd" "name" "tool"
                                      "arguments" (json-encode (json-object "value" value)))))
               (text    (test-terminal-row-text entry)))
          (test-assert
           (if fits-p
               (string= text expected)
               (and (find #\Newline text)
                    (string= (terminal-span-text (first entry)) "▸ (odd.tool)")))
           (format nil "inline fit uses terminal cells and reserves one column for ~S" value))
          (test-assert
           (and (find ':syntax-keyword entry :key #'terminal-span-style)
                (every (lambda (line) (< (text-cell-width line) columns))
                       (uiop:split-string text :separator '(#\Newline))))
           "inline and table presentations preserve keyword styling and width bounds")))))
  (let* ((entry (response-item-entry
                 (application-tests--ui-application :columns 24)
                 (json-object "type" "function_call" "namespace" "odd" "name" "tool"
                              "arguments" "{\"value\":\"alpha beta gamma delta epsilon zeta\"}")))
         (text  (test-terminal-row-text entry)))
    (test-assert
     (and (> (count #\Newline text) 1)
          (string= (terminal-span-text (first entry)) "▸ (odd.tool)")
          (every (lambda (word) (search word text))
                 '("alpha" "beta" "gamma" "delta" "epsilon" "zeta")))
     "a wrapped argument retains its complete table rather than a partial inline header"))
  nil)

(-> test-fatal-boundaries-keep-backtraces () null)
(defun test-fatal-boundaries-keep-backtraces ()
  "Test each fatal boundary hands recovery the frames of the failure it ends."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (controller  (make-instance 'application-input-controller
                                       :application application
                                       :main-thread (current-thread)))
           (reported    nil)
           (origin      nil)
           (failure     (lambda ()
                          (error "Injected fatal failure."))))
      (setf (application-input-controller application) controller)
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'application-raise-fatal
                        (lambda (application condition backtrace)
                          (setf origin (list application condition))
                          (throw 'test-fatal backtrace))))

            (lambda ()
              (setf reported (catch 'test-fatal
                               (application--call-with-model-error-handling application failure)))
              (test-assert (and (consp reported) (every #'stringp reported)
                                (find "ERROR" reported :test #'search))
                           "a fatal model failure keeps its signal frames")
              (let* ((condition (make-condition 'sb-ext:timeout))
                     (hooks     (list sb-ext:*invoke-debugger-hook*
                                      (sb-ext:symbol-global-value 'sb-ext:*invoke-debugger-hook*)
                                      (sb-ext:symbol-global-value 'sb-debug::*old-debugger-hook*)))
                     (hook      (application--fatal-debugger-hook application)))
                (let ((sb-ext:*invoke-debugger-hook* hook))
                  (setf reported (catch 'test-fatal (invoke-debugger condition))))
                (test-assert (and (eq (first origin) application)
                                  (eq (second origin) condition)
                                  (consp reported) (every #'stringp reported)
                                  (find "INVOKE-DEBUGGER" reported :test #'search))
                             "the owner debugger hook reports its condition and live failure frames")
                (test-assert (equal hooks
                                    (list sb-ext:*invoke-debugger-hook*
                                          (sb-ext:symbol-global-value 'sb-ext:*invoke-debugger-hook*)
                                          (sb-ext:symbol-global-value 'sb-debug::*old-debugger-hook*)))
                             "the owner debugger hook preserves its surrounding hook bindings"))
              (multiple-value-bind (values status report)
                  (application-lisp-call-with-debugger failure :application application)
                (declare (ignore values))
                (test-assert (and (eq status ':aborted) (non-empty-string-p report))
                             "ordinary native Lisp failures return through their local debugger"))
              (application-input-controller--record-failure
               controller
               (make-condition 'simple-error :format-control "Injected reader failure.")
               (list "READER-FRAME"))
              (test-assert
               (equal (handler-case
                          (progn
                            (application-input-controller--next-work controller)
                            nil)
                        (application-input-failed (condition)
                          (application-input-failed-backtrace condition)))
                      '("READER-FRAME"))
               "a terminal reader failure reaches the main thread with its frames")))
        (setf (application-input-controller application) nil)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-reader-failure-preserves-pending-during-stop () null)
(defun test-reader-failure-preserves-pending-during-stop ()
  "Retain accepted input when teardown overtakes fatal pending publication."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (controller  (make-instance 'application-input-controller
                                       :application application :main-thread (current-thread)))
           (persist     (symbol-function 'application-input-controller--persist-pending))
           (publish     (symbol-function 'application-input-controller--publish-pending-publication))
           (stopped-p   nil))
      (setf (application-input-controller application) controller)
      (unwind-protect
           (progn
             (test-assert
              (and (application-input-controller--enqueue controller ':message "accepted before failure")
                   (application-input-controller--enqueue controller ':lisp "help"))
              "the controller accepts both pending inputs before failure")
             (flet ((stop-before-publication ()
                      "Run real teardown once before the fatal publication reaches storage."
                      (unless stopped-p
                        (setf stopped-p t)
                        (application-input-controller-stop controller))))
               (test-call-with-function-replacements
                (list
                 (list 'application-input-controller--persist-pending
                       (lambda (target &key (error-p nil) (sync-vault-p t))
                         (stop-before-publication)
                         (funcall persist target :error-p error-p :sync-vault-p sync-vault-p)))
                 (list 'application-input-controller--publish-pending-publication
                       (lambda (target publication error-p)
                         (stop-before-publication)
                         (funcall publish target publication error-p))))
                (lambda ()
                  (application-input-controller--record-failure
                   controller (make-condition 'simple-error :format-control "Injected reader failure.")
                   (list "READER-FRAME")))))
             (test-assert (and stopped-p
                               (null (application-input-controller--state controller :work-items)))
                          "teardown clears the live queue before fatal publication completes")
             (let ((restored (make-instance 'application-input-controller
                                            :application application :main-thread (current-thread))))
               (application-input-controller--load-pending restored)
               (test-assert
                (equal (application-input-controller--state restored :work-items)
                       '((:message "accepted before failure") (:lisp "help")))
                "fatal publication retains the accepted FIFO after teardown clears memory")))
        (setf (application-input-controller application) nil)
        (terminal-ui-stop (application-ui application)))))
  nil)

(-> test-explicit-application-reconnect () null)
(defun test-explicit-application-reconnect ()
  "Test an explicit reconnect restores the selected conversation and its history."
  (let* ((configuration                 (test-configuration))
         (root                          (test-configuration-root configuration))
         (explicit
           (conversation-create configuration :identifier "explicit-resume"))
         (mcp-registration-snapshot     (mcp--registry-snapshot))
         (context-registration-snapshot (context--registry-snapshot))
         (command-registration-snapshot
           (application-command--registry-snapshot))
         (application                   nil))
    (labels ((check-history (expected)
               "Recall EXPECTED from the installed UI and return to its draft."
               (let* ((ui     (application-ui application))
                      (editor (terminal-ui-editor ui)))
                 (line-editor-set-text editor "draft" :cursor 2)
                 (terminal-ui-process-event ui ':history-previous)
                 (test-assert (string= expected (line-editor-text editor))
                              "the replacement UI recalls its own conversation")
                 (terminal-ui-process-event ui ':history-next)
                 (test-assert (and (string= "draft" (line-editor-text editor))
                                   (= 2 (line-editor-cursor editor)))
                              "returning from recall restores the draft and cursor")))

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
             (conversation-append-user-message explicit "explicit message")
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
                       configuration))
               (list 'application-terminal-ui-create
                     (lambda (&key client-pane configuration inbox-p)
                       (declare (ignore client-pane configuration inbox-p))
                       (terminal-ui-create
                        :terminal
                        (make-instance 'recording-terminal :columns 80)))))
              (lambda ()
                (setf application (application-create configuration))
                (application-set-hurry-up application t)
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
                  (application-hurry-up-p application)
                  (agent-hurry-up-p (application-agent application))
                  (zerop (application-rendered-sequence application))
                  (null (application-history-floor-sequence application)))
                 "explicit resume restores the selected conversation")
                (check-history "explicit message")
                (let ((ui (application-ui application)))
                  (test-assert
                    (and (plusp (terminal-ui-context-used ui))
                         (terminal-ui-context-estimated-p ui)
                         (= (terminal-ui-context-window ui) 272000)
                         (= (terminal-ui-context-compaction-limit ui) 217600))
                   "application reconnect seeds its idle context meter")
                  (test-assert (null (recording-terminal-chunks
                                      (terminal-ui-terminal ui)))
                               "reconnect seeding does not paint an unstarted UI")
                  (terminal-ui-start ui)
                  (test-assert (search "272K"
                                       (recording-terminal-output
                                        (terminal-ui-terminal ui)))
                              "the first reconnect paint includes its context meter")
                  (terminal-ui-stop ui)))))
        (close-application)
        (mcp--registry-restore mcp-registration-snapshot)
        (context--registry-restore context-registration-snapshot)
        (application-command--registry-restore command-registration-snapshot)
        (platform-delete-directory-tree *platform* root :validate t
                                             :if-does-not-exist   ':ignore))))
  nil)

(-> application-tests--history-selection-work () null)
(defun application-tests--history-selection-work ()
  "Bound formatting and decoded source retention by the final selected page."
  (with-test-configuration (configuration)
    (let* ((application   (application-tool-outcome-tests--application configuration))
           (raw-records
             (loop for index from 1 to 100
                   for call-id = (format nil "bounded-call-~D" index)
                   append (list
                           (application-tool-outcome-tests--call (1- (* 2 index)) call-id)
                           (application-tool-outcome-tests--result
                            (* 2 index) call-id :status ':error
                            :output                     (format nil "bounded failure ~D" index)))))
           (decode-counts (make-hash-table :test #'equal))
           (formatted     nil)
           (decode        (symbol-function 'json-decode))
           (format-record (symbol-function 'conversation-record-entry)))
      (test-call-with-function-replacements
       (list
        (list 'json-decode
              (lambda (source)
                (incf (gethash source decode-counts 0))
                (funcall decode source)))
        (list 'conversation-record-entry
              (lambda (application record)
                (push (application--record-sequence record) formatted)
                (funcall format-record application record))))
       (lambda ()
         (multiple-value-bind (records count latest position outcomes sources)
             (application--select-history-records
              application
              (lambda (callback)
                (mapc callback raw-records)
                123)
              :limit 3)
           (test-assert (null formatted)
                        "selection never formats discarded results or invocations")
           (test-assert (and (= count 200) (= latest 200) (= position 123)
                             (equal (mapcar #'application--record-sequence records)
                                    '(198 199 200)))
                        "raw selection preserves chronological page and scan cursors")
           (test-assert (and (= (hash-table-count outcomes) 1)
                             (= (hash-table-count sources) 3))
                        "selected outcomes and source caches follow the bounded page")
           (dolist (record records)
             (application--record-presentation
              application record :outcomes outcomes :source-cache (gethash record sources)))
           (test-assert (and (= (length formatted) 4)
                             (every (lambda (sequence) (member sequence '(198 199 200)))
                                    formatted))
                        "only final selected entries and their resolved outcome are formatted")
           (test-assert
            (every (lambda (record)
                     (= (gethash (getf (rest record) :wire-json) decode-counts 0) 1))
                   (remove-if-not (lambda (record) (eq (first record) ':provider-item))
                                  raw-records))
            "every provider record is decoded once through selection eviction and publication"))))))
  nil)

(-> application-tests--history-outcome-chronology () null)
(defun application-tests--history-outcome-chronology ()
  "Resolve an older selected call from chronological raw outcomes in a newer segment."
  (dolist (terminal-result-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((application (application-tool-outcome-tests--application configuration))
             (call        (application-tool-outcome-tests--call 1 "older"))
             (handoff     (application-tool-outcome-tests--result 2 "older"))
             (replacement (application-tool-outcome-tests--result 4 "older"))
             (active      (list
                           (application-tool-outcome-tests--completion 3 "old-job" :outcome ':failure)
                           replacement
                           (application-tool-outcome-tests--completion 5 "old-job" :outcome ':success)
                           (application-tool-outcome-tests--completion
                            6 "new-job" :outcome ':failure :summary "selected terminal failure"))))
        (setf (getf (rest handoff) :details)
              (list :tool-execution :handed-off-p t :job (list :id "old-job"))
              (getf (rest replacement) :details)
              (list :tool-execution :handed-off-p t :job (list :id "new-job")))
        (when terminal-result-p
          (setf active (append active
                               (list (application-tool-outcome-tests--result 7 "older")
                                     (application-tool-outcome-tests--completion
                                      8 "new-job" :outcome ':failure)))))
        (multiple-value-bind (records count latest position outcomes sources)
            (application--select-history-records
             application (lambda (callback) (mapc callback (list call handoff)) 10)
             :before-sequence 2 :limit 1)
          (declare (ignore count latest position))
          (test-call-with-function-replacements
           (list (list 'conversation-record-entry
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error "Outcome correlation formatted an unselected record."))))
           (lambda ()
             (application--select-history-records
              application (lambda (callback) (mapc callback active) 20)
              :limit 1 :collect-p nil :outcomes outcomes)))
          (let* ((key          (application--tool-outcome-key application ':call "older"))
                 (raw          (gethash key outcomes))
                 (presentation (application--record-presentation
                                application (first records) :outcomes outcomes
                                :source-cache                         (gethash (first records) sources)))
                 (metadata     (third presentation)))
            (test-assert (= (application--record-sequence (getf raw :outcome-record))
                            (if terminal-result-p 7 6))
                         "replaced job aliases cannot overwrite a later selected outcome")
            (test-assert (eq (getf metadata :state)
                             (if terminal-result-p ':success ':failure))
                         "selected older invocations use their newer segment's final state")
            (test-assert (if terminal-result-p
                             (null (getf metadata :job-key))
                             (equal (getf metadata :job-key)
                                    (application--tool-outcome-key application ':job "new-job")))
                         "job identity follows handoff replacement and terminal results"))))))
  nil)

(-> test-bounded-transcript-replay () null)
(defun test-bounded-transcript-replay ()
  "Test bounded startup and recovery replay without losing live appends."
  (application-tests--history-selection-work)
  (application-tests--history-outcome-chronology)
  (let* ((*application-history-page-size* 3)
         (configuration                   (test-configuration))
         (root                            (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "bounded-replay"))
         (terminal                        (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            (terminal-ui-create :terminal terminal))))
    (unwind-protect
         (progn
           (loop for index below 4
                 do (conversation-append-user-message
                     conversation
                     (format nil "visible-entry-~3,'0D" index)))
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (test-substring-count "❯ you" output) 3)
              "startup emits only one page of visible entries")
             (test-assert
              (and (not (search "visible-entry-000" output))
                   (search "visible-entry-001" output)
                   (search "visible-entry-003" output))
              "startup retains exactly the newest page of visible entries")
             (test-assert (search "(history)" output)
                          "bounded startup replay offers the history command"))
           (test-assert
            (= (application-rendered-sequence application) 4)
            "bounded startup replay advances the live cursor through omitted entries")
           (recording-terminal-reset terminal)
           (conversation-append-user-message conversation "one-live-append")
           (application-render-records application)
           (application-render-records application)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (= (test-substring-count "one-live-append" output) 1)
              "a newly appended live record renders exactly once")
             (test-assert
              (= (test-substring-count "❯ you" output) 1)
              "incremental rendering does not replay the bounded startup page"))
           (let* ((recovery-terminal
                    (make-instance 'recording-terminal :columns 80))
                  (recovery-application
                    (make-instance
                     'application
                     :configuration configuration
                     :conversation  conversation
                     :ui
                     (terminal-ui-create :terminal recovery-terminal))))
             (setf (application-rendered-sequence recovery-application) 1
                   (application-history-floor-sequence recovery-application) 1)
             (application-render-records recovery-application)
             (let ((output
                     (recording-terminal-output recovery-terminal)))
               (test-assert
                (= (test-substring-count "❯ you" output) 3)
                "recovery also bounds output after a stale durable cursor")
               (test-assert
                (and (not (search "visible-entry-001" output))
                     (search "visible-entry-002" output)
                     (search "one-live-append" output))
                "recovery retains only the newest unread transcript page"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-chunked-transcript-replay () null)
(defun test-chunked-transcript-replay ()
  "Test newest-first replay, one-segment fallback, history, and cursor rotation."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
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
                     :conversation  conversation
                     :ui            (terminal-ui-create :terminal terminal))))
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
              (and (= (test-substring-count
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
             (conversation-append-summary conversation (make-continuation :intent "full checkpoint"))
             (conversation-append-user-message conversation "active-full-one")
             (conversation-append-user-message conversation "active-full-two")
             (let* ((loaded
                      (conversation-load (conversation-pathname conversation)))
                    (terminal         (make-instance 'recording-terminal :columns 80))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation  loaded
                       :ui            (terminal-ui-create :terminal terminal)))
                    (mapped-pathnames nil)
                    (map-records-function
                      (symbol-function 'sexp-store:log-map)))
                (let ((*application-history-page-size* 2))
                 (test-call-with-function-replacements
                  (list
                   (list
                    'sexp-store:log-map
                    (lambda (function pathname &key (start-position 0))
                      (push pathname mapped-pathnames)
                      (funcall map-records-function
                               function pathname
                               :start-position start-position))))
                  (lambda ()
                    (application-render-records application))))
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (equal (nreverse mapped-pathnames)
                              (list (conversation-log-pathname loaded)))
                       (not (search "older-full-chunk" output))
                       (search "active-full-one" output)
                       (search "active-full-two" output))
                  "a full active chunk is the only segment scanned at startup"))
               (recording-terminal-reset terminal)
               (conversation-append-summary loaded (make-continuation :intent "rotated checkpoint"))
               (conversation-append-user-message loaded "after-runtime-rotation")
               (application-render-records application)
               (application-render-records application)
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (= (test-substring-count
                           "after-runtime-rotation" output)
                          1)
                       (= (application-render-generation application)
                          (conversation-log-generation loaded))
                       (plusp (application-render-position application)))
                  "chunk rotation resets the active cursor and renders each new record once"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "chunk-page-short")))
             (conversation-append-user-message conversation "oldest-third-chunk")
             (conversation-append-summary conversation (make-continuation :intent "middle checkpoint"))
             (conversation-append-user-message conversation "middle-chunk-message")
             (conversation-append-summary conversation (make-continuation :intent "active checkpoint"))
             (conversation-append-user-message conversation "active-chunk-message")
             (let* ((loaded
                      (conversation-load (conversation-pathname conversation)))
                    (pathnames
                      (conversation-storage-pathnames
                       (conversation-pathname conversation)))
                    (active           (third pathnames))
                    (previous         (second pathnames))
                    (terminal         (make-instance 'recording-terminal :columns 80))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation  loaded
                       :ui            (terminal-ui-create :terminal terminal)))
                    (mapped-pathnames nil)
                    (map-records-function
                      (symbol-function 'sexp-store:log-map)))
               (let ((*application-history-page-size* 3))
                 (test-call-with-function-replacements
                  (list
                   (list
                    'sexp-store:log-map
                    (lambda (function pathname &key (start-position 0))
                      (push pathname mapped-pathnames)
                      (funcall map-records-function
                               function pathname
                               :start-position start-position))))
                  (lambda ()
                    (application-render-records application)))
                 (let ((output (recording-terminal-output terminal)))
                   (test-assert
                    (and (equal (nreverse mapped-pathnames)
                                (list active previous))
                         (not (search "oldest-third-chunk" output))
                         (search "middle-chunk-message" output)
                         (search "active-chunk-message" output)
                         (not (search "use (history) to load an earlier page" output)))
                    "a short active chunk scans exactly one preceding segment"))
                 (recording-terminal-reset terminal)
                 (application-render-history application)
                 (let ((output (recording-terminal-output terminal)))
                   (test-assert
                    (and (search "oldest-third-chunk" output)
                         (not (search "middle-chunk-message" output))
                         (= (test-substring-count
                             "oldest-third-chunk" output)
                            1))
                    "explicit history crosses older chunk boundaries without duplicates")))))
           (let* ((conversation
                    (conversation-create
                     configuration
                     :identifier "chunk-invalid-previous"))
                  (terminal (make-instance 'recording-terminal :columns 80)))
             (conversation-append-user-message conversation "malformed older record")
             (conversation-append-summary conversation (make-continuation :intent "active checkpoint"))
             (let* ((pathnames
                      (conversation-storage-pathnames
                       (conversation-pathname conversation)))
                    (previous (first pathnames))
                    (forms    (copy-tree (conversation--read-records previous)))
                    (application
                      (make-instance
                       'application
                       :configuration configuration
                       :conversation
                       (conversation-load (conversation-pathname conversation))
                       :ui            (terminal-ui-create :terminal terminal))))
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-hidden-reasoning-does-not-crowd-replay () null)
(defun test-hidden-reasoning-does-not-crowd-replay ()
  "Test hidden reasoning records do not consume visible replay capacity."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "visible-replay"))
         (terminal      (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :provider      nil
                          :ui            (terminal-ui-create :terminal terminal))))
    (unwind-protect
         (let ((*application-history-page-size* 3))
           (application-set-reasoning-traces application nil)
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
              (= (test-substring-count "❯ you" output) 3)
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-paged-history-after-eviction () null)
(defun test-paged-history-after-eviction ()
  "Test (history) pages in the records a bounded fullscreen transcript evicted."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "evicted-history"))
         (terminal      (make-instance 'recording-terminal :columns 80 :rows 24))
         (application   nil))
    (unwind-protect
         (let ((*application-history-page-size*         2)
               (*fullscreen-transcript-character-limit* 1500))
           (setf application
                 (make-instance 'application
                                :configuration configuration
                                :conversation  conversation
                                :ui            (terminal-ui-create :terminal     terminal
                                                                   :fullscreen-p t)))
           (flet ((numbers ()
                    (let ((viewport (fullscreen-terminal-ui-viewport (application-ui application))))
                      (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
                            for text = (clinedi:transcript-viewport-chunk-text viewport index)
                            append (loop for start = (search "message-" text)
                                           then (search "message-" text :start2 (1+ start))
                                         while start
                                         collect (parse-integer text :start        (+ start 8)
                                                                     :junk-allowed t))))))
             (loop for index from 1 to 40
                   do (conversation-append-user-message
                       conversation (format nil "message-~D ~A" index
                                            (make-string 30 :initial-element #\x)))
                      (application-render-records application))
             (let ((oldest (reduce #'min (numbers))))
               (test-assert (> oldest 2) "the bounded transcript evicted the oldest messages")
               (let ((retention (application-retention application)))
                 (test-assert (and (<= (getf retention :transcript-characters) 1500)
                                   (plusp (getf retention :transcript-evicted))
                                   (plusp (getf retention :heap-bytes))
                                   (search "chunks evicted"
                                           (application--retention-description application)))
                              "the retention report shows the bounded transcript"))
               (application-render-history application)
               (let ((loaded (last (numbers) 2)))
                 (test-assert (equal loaded (list (- oldest 2) (- oldest 1)))
                              "history pages in the newest evicted messages")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-paged-transcript-history () null)
(defun test-paged-transcript-history ()
  "Test /history appends bounded older pages without replaying live output."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "paged-history"))
         (terminal      (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            (terminal-ui-create :terminal terminal))))
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
            (= (test-substring-count
                "history-live-message"
                (recording-terminal-output terminal))
               1)
            "live output remains singular before older history is loaded")
           (recording-terminal-reset terminal)
           (test-assert
            (eq (application-run-lisp-input application "(history)") ':continue)
            "(history) keeps the interactive application running")
           (let* ((output          (recording-terminal-output terminal))
                  (second-position (search "history-message-2" output))
                  (third-position  (search "history-message-3" output)))
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
           (application-run-lisp-input application "(history)")
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
           (application-run-lisp-input application "(history)")
           (test-assert
            (search "no earlier transcript history remains"
                    (recording-terminal-output terminal))
            "history reports exhaustion after the final page"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-compaction-presentation-lifecycle () null)
(defun test-compaction-presentation-lifecycle ()
  "Test compaction event presentation and every defensive cleanup boundary."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "compaction-ui"))
         (registry      (make-instance 'tool-registry))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition 'simple-error
                             :format-control   "manual compaction failed"
                             :format-arguments nil)
             (make-condition 'simple-error
                             :format-control   "turn failed"
                             :format-arguments nil))))
         (agent         (agent-create :configuration configuration
                                      :provider      provider
                                      :conversation  conversation
                                      :tool-registry registry
                                      :worker        ':unused))
         (terminal      (make-instance 'recording-terminal :columns 72))
         (ui            (terminal-ui-create :terminal       terminal
                                            :clock-function (lambda () 0)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry registry
                          :agent         agent
                          :ui            ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (conversation-append-user-message conversation "history to compact")
           (setf (conversation-last-total-tokens conversation) 262500)
           (application-refresh-context-meter application)
           (test-assert
            (and (< (terminal-ui-context-used ui) 262500)
                 (terminal-ui-context-estimated-p ui)
                 (= (terminal-ui-context-window ui) 272000)
                 (= (terminal-ui-context-compaction-limit ui) 217600))
            "application refresh estimates unverified provider usage for the idle meter")
           (setf (conversation-resumed-p conversation) t)
           (application-refresh-context-meter application)
           (test-assert
            (and (< (terminal-ui-context-used ui) 262500)
                 (terminal-ui-context-estimated-p ui))
            "resumed historical usage is not trusted by the current context meter")
           (setf (conversation-resumed-p conversation) nil)
           (test-assert
            (search (config :model configuration)
                    (terminal-ui--raw-spans-text
                     (terminal-ui-idle-status-details ui)))
            "application refresh projects static model details into the idle row")
           (let* ((observer (application-agent-observer application))
                  (send-status
                    (callback-agent-observer-status-callback observer)))
             (setf (conversation-last-total-tokens conversation) 525000)
             (funcall send-status :provider-request-completed nil)
             (test-assert (and (< (terminal-ui-context-used ui) 525000)
                               (terminal-ui-context-estimated-p ui))
                          "provider completion does not trust an unverified usage report")
             (setf (conversation-last-total-tokens conversation) 262500)
             (funcall send-status :compaction-started nil)
             (test-assert
              (and (terminal-ui-compacting-p ui)
                   (string= (terminal-ui-status ui)
                            "compacting the conversation"))
              "the compaction event enables its indicator and activity phase")
             (let* ((rows (terminal-ui--prompt-prefix-rows ui 0))
                    (text (terminal--spans-text (second rows))))
               (test-assert (and (search "REPL" text) (search "COMPACTING  ━━━━━" text)
                                 (not (search "ctx" text :test #'char-equal))
                                 (= 1 (count-if (lambda (row) (search "COMPACTING" (terminal--spans-text row)))
                                                rows)))
                            "The compaction bar replaces the context meter in the modeline."))
             (conversation-append-provider-metadata
              conversation
              (list :compaction ':summary :usage '(("total_tokens" 270000))))
             (conversation-append-summary conversation (make-continuation :intent "retained handoff"))
             (funcall send-status :compaction-completed nil)
             (test-assert (not (terminal-ui-compacting-p ui))
                          "completion removes the compaction indicator")
             (test-assert (and (plusp (terminal-ui-context-used ui))
                               (< (terminal-ui-context-used ui) 262500)
                               (terminal-ui-context-estimated-p ui))
                          "compaction completion estimates retained context, not compaction spend")
             (conversation-append-provider-metadata
              conversation (list :usage '(("total_tokens" 24000))))
             (funcall send-status :provider-request-completed nil)
             (test-assert (and (plusp (terminal-ui-context-used ui))
                               (< (terminal-ui-context-used ui) 24000)
                               (terminal-ui-context-estimated-p ui))
                          "an unverified provider report cannot replace the retained-context estimate")
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-replay-skips-notice () null)
(defun test-replay-skips-notice ()
  "Test the startup notice groups skipped private replays by reason."
  (flet ((skip (source reason)
           "Return a replay skip for SOURCE under REASON."
           (let ((definition (self-read-form source :read-eval nil)))
             (make-instance 'image-replay-skip :definition definition :key
                            (definition-key definition) :reason reason :message
                            "skipped"))))
    (test-assert (null (application--replay-skips-entry nil))
                 "an image without skips presents no notice")
    (let* ((skips
            (list (skip "(defun zeta-wrapper () 0)" ':tracked-changed)
                  (skip
                   "(defmethod tool-execute :around ((tool shell-run-tool) context arguments) nil)"
                   ':revision-moved)
                  (skip "(defun alpha-wrapper () 0)" ':tracked-changed)))
           (entry
            (application--replay-skips-entry skips :commit-identifier
                                             "c3ec5281-3622-4c30-9bc9-a223fecccd47"
                                             :lineage-source-commit
                                             "07f0e1b86da2be055f1e836a4e62e57fbf9e4b7b"
                                             :image-source-commit nil))
           (text (terminal--spans-text entry))
           (changed-title
            (position-if
             (lambda (span) (search "changed since" (terminal-span-text span))) entry))
           (moved-title
            (position-if (lambda (span) (search "moved on" (terminal-span-text span)))
                         entry)))
      (test-assert
       (and (search "skipped 3 definitions" text) (search "c3ec5281" text)
            (search "07f0e1b8" text) (search "alpha-wrapper" text)
            (search "tool-execute" text))
       "the notice names the commit, revisions, and every skipped definition")
      (test-assert
       (let ((command
              (find "(fix-skipped-definitions)" entry :key #'terminal-span-text :test
                    #'string=)))
         (and command (eq (terminal-span-style command) ':code)))
       "the notice offers the repair operation as a Lisp command")
      (test-assert
       (and changed-title moved-title (< changed-title moved-title)
            (< (search "alpha-wrapper" text) (search "zeta-wrapper" text)))
       "groups follow the reason order and sort their definitions by name")
      (test-assert
       (and (eq (terminal-span-style (first entry)) ':failure)
            (eq (terminal-span-style (nth changed-title entry)) ':notice)
            (find ':code entry :key #'terminal-span-style))
       "the notice styles its header, group titles, and definition names")))
  nil)

(-> test-fix-skipped-definitions-prompt () null)
(defun test-fix-skipped-definitions-prompt ()
  "Test the repair request carries each skipped override with its tracked sources."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-skip-prompt-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration   (test-configuration-for-source-root source-root))
         (source-pathname (merge-pathnames "src/definitions.lisp" source-root))
         (override        "(defun test-self-target () \"Return the private override.\" 7)")
         (recorded        "(defun test-self-target () \"Return the old baseline.\" 0)")
         (current         "(defun test-self-target ()~%  \"Return the current baseline.\"~%  1)"))
    (unwind-protect
         (progn
           (ensure-directories-exist source-pathname)
           (with-open-file (stream source-pathname
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (format stream "(in-package #:autolith)~2%")
             (format stream current)
             (terpri stream))
           (flet ((skip (source reason &rest arguments)
                    "Return a replay skip for SOURCE under REASON with ARGUMENTS."
                    (let ((definition (self-read-form source :read-eval nil)))
                      (apply #'make-instance 'image-replay-skip
                             :definition definition
                             :key        (definition-key definition)
                             :source     source
                             :reason     reason
                             :message    "The persisted defun was skipped: it moved."
                             arguments))))
             (let ((prompt
                     (application--skipped-definitions-prompt
                      (list (skip override ':tracked-changed
                                  :tracked recorded :tracked-recorded-p t)
                            (skip "(defun test-skip-prompt-fresh () 2)"
                                  ':revision-moved
                                  :tracked nil :tracked-recorded-p nil))
                      :configuration         configuration
                      :commit-identifier     "c3ec5281-3622-4c30-9bc9-a223fecccd47"
                      :lineage-source-commit "07f0e1b86da2be055f1e836a4e62e57fbf9e4b7b"
                      :image-source-commit   "8631cedec9e7a704ad03e6200b4d09f64f079531")))
               (test-assert
                 (and (search "c3ec5281" prompt)
                      (search "07f0e1b8" prompt)
                      (search "8631cede" prompt))
                 "the request identifies the commit and both revisions")
               (test-assert
                (and (search "1. defun test-self-target" prompt)
                     (search "Reason: The persisted defun was skipped: it moved." prompt)
                     (search override prompt)
                     (search recorded prompt)
                     (search "src/definitions.lisp:" prompt)
                     (search "Return the current baseline." prompt))
                "an override section carries its persisted, recorded, and current sources")
               (test-assert
                (and (search "2. defun test-skip-prompt-fresh" prompt)
                     (search "Tracked definition when published: not recorded" prompt)
                     (search "none: the current source has no definition" prompt))
                "a legacy override without tracked counterparts says so"))))
      (platform-delete-directory-tree *platform* source-root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> application-tests--stream-ownership (configuration) null)
(defun application-tests--stream-ownership (configuration)
  "Exercise replay interleavings, multipart responses and turn unwind without provider calls."
  (labels ((message (parts)
             "Build one completed assistant item with its separate content parts."
             (json-object "type" "message" "role" "assistant"
                          "content" (coerce (mapcar (lambda (text)
                                                     (json-object "type" "output_text" "text" text))
                                                   parts)
                                            'vector)))

           (run (parts text &key fullscreen-p interleave-p)
             "Count completed assistant entries after one controlled request."
             (let* ((conversation (conversation-create configuration))
                    (terminal     (make-instance 'recording-terminal :columns 120 :rows 30))
                    (ui           (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
                    (application  (make-instance 'application :configuration configuration
                                                 :conversation conversation :ui ui))
                    (observer     (application-agent-observer application)))
               (unwind-protect
                    (progn
                      (terminal-ui-start ui)
                      (application-render-records application)
                      (agent-observer-status observer ':provider-request-started nil)
                      (agent-observer-text observer text)
                      (dolist (item parts)
                        (conversation-append-provider-item conversation (message item)))
                      (when interleave-p
                        (application-render-records application))
                      (agent-observer-status observer ':provider-request-completed nil)
                      (application-render-records application)
                      (test-assert (and (null (application-stream-finish-function application))
                                        (= (application-rendered-sequence application)
                                           (1- (conversation-next-sequence conversation))))
                                   "completion releases stream display and consumes durable records")
                      (let ((output
                              (if fullscreen-p
                                  (let ((viewport (fullscreen-terminal-ui-viewport ui)))
                                    (format nil "~{~A~%~}"
                                            (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
                                                  collect (clinedi:transcript-viewport-chunk-text viewport index))))
                                  (recording-terminal-output terminal))))
                        (test-substring-count "● autolith" output)))
                 (terminal-ui-stop ui)))))
    (dolist (fullscreen-p '(nil t))
      (dolist (case (list (list '(("single reply")) "single reply" 1)
                          (list '(("first item") ("second item")) "first itemsecond item" 1)
                          (list '(("first part" "second part") ("third part"))
                                "first partsecond partthird part" 1)
                          (list '(("A") (" ") ("B")) "A B" 1)
                          (list '(("first part" "second part") ("third part"))
                                (format nil "first part~%second part~%third part") 1)
                          (list '(("authoritative correction")) "provisional answer" 2)
                          (list '(("unstreamed answer")) "" 1)))
        (destructuring-bind (parts text expected) case
          (let ((control     (run parts text :fullscreen-p fullscreen-p))
                (interleaved (run parts text :fullscreen-p fullscreen-p :interleave-p t)))
            (test-assert (and (= control expected) (= interleaved control))
                         "replay during a stream preserves exact matches and authoritative changes"))))))
  (let* ((conversation (conversation-create configuration))
         (terminal     (make-instance 'recording-terminal :columns 120))
         (ui           (terminal-ui-create :terminal terminal))
         (application  (make-instance 'application :configuration configuration
                                      :conversation conversation :agent nil :ui ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (application-render-records application)
           (test-call-with-function-replacements
            (list (list 'agent-run-user-turn
                        (lambda (agent content &key observer &allow-other-keys)
                          (declare (ignore agent content))
                          (agent-observer-status observer ':provider-request-started nil)
                          (agent-observer-text observer "interrupted reply")
                          (conversation-append-provider-item
                           conversation (json-object "type" "message" "role" "assistant"
                                                     "content" (json-array (json-object "type" "output_text"
                                                                                       "text" "interrupted reply"))))
                          (throw 'stream-aborted nil))))
            (lambda ()
              (catch 'stream-aborted
                (application--run-turn application "test input"))))
           (test-assert (null (application-stream-finish-function application))
                        "nonlocal turn unwind releases stream ownership")
           (recording-terminal-reset terminal)
           (conversation-append-provider-item
            conversation (json-object "type" "message" "role" "assistant"
                                      "content" (json-array (json-object "type" "output_text"
                                                                        "text" "authoritative after abort"))))
           (application-render-records application)
           (test-assert (search "authoritative after abort" (recording-terminal-output terminal))
                        "durable replay resumes after an interrupted turn")
           (let ((observer (application-agent-observer application)))
             (agent-observer-status observer ':provider-request-started nil)
             (agent-observer-text observer "stale attempt")
             (agent-observer-status observer ':provider-request-started nil)
             (agent-observer-text observer "fresh attempt")
             (conversation-append-provider-item
              conversation (json-object "type" "message" "role" "assistant"
                                        "content" (json-array (json-object "type" "output_text"
                                                                          "text" "fresh attempt"))))
             (recording-terminal-reset terminal)
             (agent-observer-status observer ':provider-request-completed nil)
             (test-assert (not (search "● autolith" (recording-terminal-output terminal)))
                          "request reset discards the old attempt's reconciliation text")
             (agent-observer-status observer ':provider-request-started nil)
             (agent-observer-text observer "partial before callback failure")
             (test-call-with-function-replacements
              (list (list 'application-clients-publish
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (throw 'stream-callback-aborted nil))))
              (lambda ()
                (catch 'stream-callback-aborted
                  (agent-observer-text observer "discarded event"))))
             (test-assert (null (application-stream-finish-function application))
                          "callback unwind releases stream ownership")
             (recording-terminal-reset terminal)
             (agent-observer-status observer ':provider-request-started nil)
             (agent-observer-text observer "after callback failure")
             (test-assert (= (test-substring-count "● autolith"
                                                              (recording-terminal-output terminal))
                             1)
                          "a failed callback closes its old block before the next request")
             (application--finish-stream application)))
      (terminal-ui-stop ui)))
  nil)

(-> test-streaming-presentation () null)
(defun test-streaming-presentation ()
  "Test safe streaming, exact record reconciliation, and live tool entries."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation     (conversation-create configuration
                                                       :identifier "stream-test"))
                (terminal         (make-instance 'recording-terminal :columns 30))
                (application      (progn
                                    (setf (config :reasoning-traces-p configuration) t)
                                    (make-instance 'application
                                                 :configuration configuration
                                                 :conversation  conversation
                                                 :tool-registry
                                                 (make-default-tool-registry)
                                                 :ui            (terminal-ui-create
                                                                 :terminal terminal))))
                (observer         (application-agent-observer application))
                (send-text        (callback-agent-observer-text-callback observer))
                (send-reasoning
                  (callback-agent-observer-reasoning-callback observer))
                (send-status      (callback-agent-observer-status-callback observer))
                (reasoning-prefix "**<thought>** Checking the safe path.")
                (reasoning-suffix
                  " Comparing fallback behavior and verifying the live preview remains separate from status.")
                (reasoning-first-part
                  (concatenate 'string reasoning-prefix reasoning-suffix))
                (reasoning-second-part
                  "**<thought>** Confirming the durable summary matches.")
                (streamed-text    (format nil
                                          "The quick brown fox jumps over the lazy dog~%final tail")))
           (terminal-ui-start (application-ui application))
           (funcall send-status :provider-request-started nil)
           (funcall send-reasoning reasoning-prefix)
           (let* ((ui      (application-ui application))
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
            "assistant streaming times the modeline")
           (let* ((live-output (recording-terminal-output terminal))
                  (first-row   (search "The quick brown fox" live-output))
                  (last-row    (search "over the lazy" live-output)))
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
           (let* ((streamed           (recording-terminal-output terminal))
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
           (application-render-records application)
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
               "arguments" (json-encode (json-object "forms" (vector "(+ 1 2)")))))
             (recording-terminal-reset terminal)
             (application-render-records application)
             (funcall send-status :provider-request-completed nil)
             (let ((output (recording-terminal-output terminal)))
               (test-assert
                (and (= (test-substring-count
                         "◇ reasoning summary"
                         output)
                        1)
                     (search "<thought>" output)
                     (search "▸ (self.eval)" output)
                     (search "(+ 1 2)" output)
                     (null (terminal-ui-preview-rows
                            (application-ui application))))
                "tool-only provider steps finalize one trace before the tool call")))
           (funcall send-status :tool-call-started (list :tool "rlm.map"))
           (funcall send-status
                    :tool-call-progress
                    (list :tool "rlm.map"
                          :activity
                          "rlm.map · frame 2/4 · request 1 · 6 calls left"))
           (test-assert
            (string= (terminal-ui-status (application-ui application))
                     "rlm.map · frame 2/4 · request 1 · 6 calls left")
            "tool progress replaces the generic live activity with a compact hint")
           (conversation-append-tool-result
            conversation
            "call-live"
            :tool-name "self.eval"
            :output    "42"
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
           (application-tests--stream-ownership configuration)
           (terminal-ui-stop (application-ui application)))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-retry-presentation () null)
(defun test-provider-retry-presentation ()
  "Test reconnect presentation closes and labels interrupted streamed output."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "retry-presentation"))
                (terminal  (make-instance 'recording-terminal :columns 50))
                (application
                  (progn
                    (setf (config :reasoning-traces-p configuration) t)
                    (make-instance 'application
                                   :configuration configuration
                                   :conversation  conversation
                                   :ui            (terminal-ui-create :terminal terminal))))
                (observer  (application-agent-observer application))
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
                 (ui     (application-ui application)))
             (test-assert
              (and (search "Partial answer" output)
                   (search "provider stream interrupted; retrying 1/5" output)
                   (string= (terminal-ui-status ui)
                            "reconnecting 1/5 in 1s")
                   (null (terminal-ui-preview-rows ui))
                   (null (terminal-ui-stream-tail ui)))
              "a reconnect closes and labels the partial presentation attempt"))
           (funcall send-status
                    :provider-retrying
                    (list :attempt 1 :maximum-attempts 5 :delay 0))
           (test-assert
            (string= (terminal-ui-status (application-ui application))
                     "reconnecting 1/5")
            "an in-flight reconnect drops the elapsed countdown")
           (recording-terminal-reset terminal)
           (funcall send-text "Replacement answer")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "● autolith" output)
                   (search "Replacement answer" output))
              "the replacement attempt starts a distinct assistant block"))
           (terminal-ui-stop (application-ui application)))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-turn-cursor-visibility () null)
(defun test-turn-cursor-visibility ()
  "Test model turns retain the editable cursor while updates hide motion atomically."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "cursor-turn"))
                (terminal     (make-instance 'recording-terminal :columns 50))
                (ui           (terminal-ui-create :terminal terminal))
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
                (registry     (make-instance 'tool-registry))
                (agent        (agent-create :configuration configuration
                                            :provider      provider
                                            :conversation  conversation
                                            :tool-registry registry
                                            :worker        t))
                (application  (make-instance 'application
                                             :configuration configuration
                                             :conversation  conversation
                                             :provider      provider
                                             :tool-registry registry
                                             :worker        t
                                             :agent         agent
                                             :ui            ui)))
           (with-terminal-ui (active-ui ui)
             (recording-terminal-reset terminal)
             (application--run-turn application "hello")
             (test-assert
              (cursor-observing-provider-visible-during-request-p provider)
              "the editable cursor remains visible during provider work")
             (test-assert
              (live-region-cursor-visible-p (terminal-ui-live-region ui))
              "the input cursor is restored after the model turn")
             (let* ((output (recording-terminal-output terminal))
                    (hide   (format nil "~C[?25l"
                                    *terminal-escape-character*))
                    (show   (format nil "~C[?25h"
                                    *terminal-escape-character*))
                    (hide-count
                      (test-substring-count hide output))
                    (show-count
                      (test-substring-count show output)))
               (test-assert
                (and (plusp hide-count) (plusp show-count))
                "compound model updates hide motion and restore the cursor")
               (test-assert
                (< (or (search "finished" output) most-positive-fixnum)
                   (or (search show output :from-end t) -1))
                "the final cursor reveal follows the completed answer")
               (test-assert
                (= (test-substring-count "hello" output) 1)
                "durable user input reaches scrollback exactly once")
               (test-assert
                (= (application-rendered-sequence application)
                   (1- (conversation-next-sequence conversation)))
                "the transcript cursor follows the actual durable tail"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-responsive-model-input () null)
(defun test-responsive-model-input ()
  "Test steering, follow-up queueing, and cursor stability during model turns."
  (let* ((*conversation-title-refresh-turn-count* most-positive-fixnum)
         (configuration                           (test-configuration))
         (root                                    (test-configuration-root configuration)))
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
                       :call-id   "responsive-call"
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
                   :columns                   60
                   :provider                  provider
                   :conversation              conversation
                   :final-provider-item-count 3
                   :events
                   (list '(:insert "first message")
                         :submit
                         '(:insert "steer this turn")
                         :submit
                         '(:insert "queued follow-up")
                         :complete
                         '(:insert "draft survives"))))
                (ui          (terminal-ui-create :terminal terminal))
                (registry    (agent-test-registry))
                (agent       (agent-create :configuration configuration
                                           :provider      provider
                                           :conversation  conversation
                                           :tool-registry registry
                                           :worker        nil))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation  conversation
                                            :provider      provider
                                            :tool-registry registry
                                            :worker        nil
                                            :agent         agent
                                            :ui            ui)))
           (application-tests--serve-host application)
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
             "responsive model turns leave the input cursor visible")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-responsive-command-scheduling () null)
(defun test-responsive-command-scheduling ()
  "Test busy commands apply at safe boundaries, hold, or report immediately."
  (labels ((busy-action (name arguments)
             "Return the command policy for evaluated native ARGUMENTS."
             (let ((command (application-command-find name)))
               (application-command-busy-action
                command (application-operation--command-invocation command arguments)))))
    (dolist (case '(("model" ("gpt-5.6-luna") :apply)
                    ("effort" (:high) :apply) ("mcp" (:refresh) :apply)
                    ("permissions" (:full) :apply) ("hurry-up" (t) :apply)
                    ("cwd" ("one/") :apply) ("cwd" () :execute)
                    ("goal" (:pause) :hold) ("mcp" () :execute)
                    ("hurry-up" () :execute) ("model" () :apply-interactive)
                    ("effort" () :apply-interactive) ("permissions" () :apply-interactive)
                    ("status" () :execute) ("compact" () :hold)))
      (destructuring-bind (name arguments expected) case
        (test-assert (eq (busy-action name arguments) expected)
                     (format nil "busy ~A ~S resolves to ~A" name arguments expected)))))
  (let* ((terminal      (make-instance 'waiting-recording-terminal :columns 60))
         (ui            (terminal-ui-create :terminal terminal))
         (configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "responsive-apply"))
         (application   (make-instance 'application
                                       :ui            ui
                                       :configuration configuration
                                       :conversation  conversation))
         (controller    nil))
    (setf (application-goal application)
          (list :objective     "finish the migration"
                :status        ':active
                :continuations 0
                :created-at    (get-universal-time)))
    (with-terminal-ui (active-ui ui)
      (setf controller (application-input-controller-create application))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':message "active turn")
             (application-input-controller--next-work controller)
             (application-input-controller--handle-submission
               controller "(settings :turn-timestamps-p t)")
             (test-assert (zerop (length (line-editor-text (terminal-ui-editor ui))))
                          "a boundary application does not return source to the editor")
             (application-input-controller--handle-submission
              controller "(compact)")
             (test-assert
              (equal (application-input-controller--state controller :work-items)
                     (list (list ':lisp "(compact)")))
              "only genuinely held commands wait behind the active turn")
             (test-assert
               (equal (deque->list
                       (application-input-controller-pending-apply-items
                        controller))
                      (list (list ':native-lisp "(settings :turn-timestamps-p t)")))
              "state changes queue for the next safe boundary instead")
             (application-input-controller--apply-pending-commands controller)
             (test-assert
              (and (deque-empty-p
                    (application-input-controller-pending-apply-items
                     controller))
                   (application-turn-timestamps-p application))
              "applying pending commands executes their effects")
             (application-input-controller--handle-submission
              controller "(model \"gpt-5.6-luna\")")
             (let ((picker-opened-p nil))
               (test-call-with-function-replacements
                (list
                 (list
                  'application-set-model
                  (lambda (candidate model &key provider)
                    (declare (ignore candidate model provider))
                    nil))
                 (list
                  'application--pick-reasoning-effort
                  (lambda (candidate)
                    (declare (ignore candidate))
                    (setf picker-opened-p t)
                    nil)))
                (lambda ()
                  (application-input-controller--apply-pending-commands controller)))
               (test-assert
                (not picker-opened-p)
                "an explicit model value applies without opening an effort picker"))
             (let ((events nil))
               (flet ((record (event)
                        "Record the picker or effect and whether it ran during the turn."
                        (push (list event (application-input-controller-turn-active-p controller)) events)))
                 (test-call-with-function-replacements
                  (list (list 'application--pick-model
                              (lambda (candidate &key provider)
                                (declare (ignore candidate provider))
                                (record ':model-picker)
                                "gpt-5.6-luna"))
                        (list 'application-set-model
                              (lambda (candidate model &key provider)
                                (declare (ignore candidate model provider))
                                (record ':model-changed)))
                        (list 'application--pick-reasoning-effort
                              (lambda (candidate)
                                (declare (ignore candidate))
                                (record ':effort-picker)
                                "high"))
                        (list 'application-set-reasoning-effort
                              (lambda (candidate effort)
                                (declare (ignore candidate effort))
                                (record ':effort-changed)))
                        (list 'application--pick-identifier
                              (lambda (candidate &key &allow-other-keys)
                                (declare (ignore candidate))
                                (record ':permissions-picker)
                                "ask"))
                        (list 'application-permissions-command
                              (lambda (candidate choice)
                                (declare (ignore candidate))
                                (record choice))))
                  (lambda ()
                    (dolist (input '("(model)" "(effort)" "(permissions)"))
                      (application-input-controller--handle-submission controller input))
                    (test-assert (null events) "no picker opens on the submission thread")
                    (application-input-controller--apply-pending-commands controller)))
                 (test-assert
                  (equal (nreverse events)
                         '((:model-picker t) (:model-changed t) (:effort-picker t) (:effort-changed t)
                           (:effort-picker t) (:effort-changed t) (:permissions-picker t) (:ask t)))
                  "queued pickers and selected values apply during the active turn in input order")
                 (test-assert
                  (equal (application-input-controller--state controller :work-items)
                         (list (list ':lisp "(compact)")))
                  "settings use the active boundary instead of joining the held follow-up")))
             (application-input-controller--handle-submission
               controller "(settings :turn-timestamps-p nil)")
             (test-assert
              (equal (deque->list
                      (application-input-controller-pending-apply-items controller))
                     (list (list ':native-lisp "(settings :turn-timestamps-p nil)")))
              "canonical Lisp state changes queue for the next safe boundary")
             (application-input-controller--apply-pending-commands controller)
             (test-assert
              (not (application-turn-timestamps-p application))
              "canonical Lisp pending changes execute noninteractively")
             (application-input-controller--handle-submission
               controller "(settings :turn-timestamps-p t)")
             (application-input-controller--finish-work controller)
             (test-assert
              (equal (application-input-controller--next-work controller)
                     (list ':apply-pending))
              "boundary commands missing their turn apply before follow-ups"))
        (when controller
          (application-input-controller-stop controller))
        (platform-delete-directory-tree
         *platform*
         root :validate t :if-does-not-exist ':ignore))))
  (labels ((call-with-busy-workspace (function)
             "Call FUNCTION with an active controller and isolated real workspace runtime."
             (let* ((configuration      (test-configuration))
                    (root               (test-configuration-root configuration))
                    (previous-directory (uiop:getcwd))
                    (previous-defaults  *default-pathname-defaults*)
                    (extensions         (application--extension-registry-snapshot))
                    (application        nil)
                    (controller         nil))
               (ensure-directories-exist (merge-pathnames "one/two/" root))
               (unwind-protect
                    (let* ((configuration (configuration-copy configuration :working-directory root))
                           (conversation  (conversation-create configuration))
                           (provider      (provider-create configuration))
                           (registry      (make-default-tool-registry))
                           (agent         (agent-create :configuration configuration
                                                        :provider      provider
                                                        :conversation  conversation
                                                        :tool-registry registry
                                                        :worker        nil))
                           (ui            (terminal-ui-create
                                           :terminal (make-instance 'waiting-recording-terminal :columns 80))))
                      (setf application
                            (make-instance 'application
                                           :configuration configuration
                                           :conversation  conversation
                                           :provider      provider
                                           :tool-registry registry
                                           :worker        nil
                                           :agent         agent
                                           :ui            ui))
                      (uiop:chdir root)
                      (setf *default-pathname-defaults* root)
                      (with-terminal-ui (active-ui ui)
                        (setf controller
                              (application-input-controller-create application :start-reader-p nil))
                        (application-input-controller--enqueue controller ':message "active turn")
                        (application-input-controller--next-work controller)
                        (funcall function :application application :controller controller
                                          :agent agent :root root)))
                 (when controller
                   (application-input-controller-stop controller))
                 (when application
                   (ignore-errors
                     (tool-registry-close-runtime-state (application-tool-registry application))))
                 (uiop:chdir previous-directory)
                 (setf *default-pathname-defaults* previous-defaults)
                 (application--extension-registry-restore extensions)
                 (platform-delete-directory-tree *platform* root
                                                 :validate t :if-does-not-exist ':ignore)))))
    (call-with-busy-workspace
     (lambda (&key application controller agent root)
       (application-input-controller--handle-submission controller "(cwd)")
       (test-assert
        (search (namestring root)
                (recording-terminal-output (terminal-ui-terminal (application-ui application))))
        "an argument-free cwd reports immediately during a turn")
       (application-input-controller--handle-submission controller "(cwd \"one/\")")
       (application-input-controller--handle-submission controller "(cwd \"two/\")")
       (test-assert
        (and (equal root (config :working-directory (application-configuration application)))
             (deque-empty-p (application-input-controller-steering-items controller)))
        "workspace changes wait outside the steering queue")
       (application-input-controller--apply-pending-commands controller :agent agent)
       (test-assert
        (and (equal (config :working-directory (application-configuration application))
                    (platform-truename *platform* (merge-pathnames "one/two/" root)))
             (eq (agent-configuration agent) (application-configuration application))
             (eq (agent-provider agent) (application-provider application))
             (eq (agent-tool-registry agent) (application-tool-registry application)))
        "the boundary resolves relative paths in apply order and updates the running agent"))))
  nil)

(-> test-input-reader-quiescence () null)
(defun test-input-reader-quiescence ()
  "Test modal and checkpoint work can temporarily restore a single Lisp thread."
  (labels ((terminal-owner-p (name &rest arguments)
             "Return the registered terminal policy with native ARGUMENTS."
             (let ((command (application-command-find name)))
               (application-command-terminal-owner-p
                command (application-operation--command-invocation command arguments)))))
    (test-assert (terminal-owner-p "model") "argument-free pickers take terminal ownership")
    (test-assert (terminal-owner-p "permissions") "the permission picker takes terminal ownership")
    (test-assert (terminal-owner-p "model" "gpt-5.6-luna") "explicit model changes own the effort picker")
    (test-assert (terminal-owner-p "open") "the channel selector owns its terminal")
    (test-assert (not (terminal-owner-p "compact")) "nonmodal commands retain responsive input")
    (test-assert (terminal-owner-p "auth") "authentication owns terminal mode"))
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
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
              (application-input-controller-reader-live-p controller)
              "the responsive terminal reader starts independently")
             (test-assert
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (and (not (application-input-controller-reader-live-p controller))
                      (application-input-controller-reader-paused-p controller))))
              "pausing input joins the competing terminal reader")
             (test-assert
              (application-input-controller-reader-live-p controller)
              "the terminal reader restarts after single-threaded work"))
        (application-input-controller-stop controller))))
  (dolist (start-reader-p '(nil t))
    (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
           (ui          (terminal-ui-create :terminal terminal))
           (application (make-instance 'application :ui ui)))
      (with-terminal-ui (active-ui ui)
        (let ((controller (application-input-controller-create
                           application :start-reader-p start-reader-p :load-pending-p nil)))
          (unwind-protect
               (dolist (exit-kind '(:normal :nonlocal))
                 (test-assert
                  (eq exit-kind
                      (catch 'modal-result
                        (terminal-ui-call-with-exclusive-input
                         ui
                         (lambda ()
                           (test-assert
                            (not (application-input-controller-reader-live-p controller))
                            "modal input has no competing reader")
                           (terminal-ui-call-with-exclusive-input
                            ui
                            (lambda ()
                              (test-assert
                               (not (application-input-controller-reader-live-p controller))
                               "nested modal input has no competing reader")))
                           (if (eq exit-kind ':nonlocal)
                               (throw 'modal-result exit-kind)
                               exit-kind)))))
                  "exclusive input preserves normal and nonlocal return values")
                 (test-assert
                  (eq start-reader-p (application-input-controller-reader-live-p controller))
                  "exclusive input restores the reader's initial lifecycle"))
            (application-input-controller-stop controller))))))
  (test-assert
   (equal (application--initial-work-items nil)
          nil)
   "startup without a command does not queue work")
  (test-assert
   (equal (application--initial-work-items "(resume)")
          (list (list ':lisp "(resume)")))
   "bare command-line resume runs through canonical local Lisp")
  nil)

(-> test-command-turn-steering () null)
(defun test-command-turn-steering ()
  "Test prose steering command-hosted turns such as goal work."
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
      (setf controller
            (application-input-controller-create
             application
             :load-pending-p nil
             :start-reader-p nil))
      (unwind-protect
           (progn
             (application-input-controller--enqueue
              controller ':lisp "(goal \"ship it\")")
             (test-assert
              (equal (application-input-controller--next-work controller)
                     '(:lisp "(goal \"ship it\")"))
              "the goal command becomes the active work item")
             (multiple-value-bind (accepted-p delivery)
                 (application-input-controller-submit-primary-prompt
                  controller "queued during a plain command")
               (test-assert
                (and accepted-p (eq delivery ':queued))
                "plain command work still queues prose as follow-ups"))
             (application-input-controller-call-with-primary-steering
              controller
              (lambda (take acknowledge)
                (multiple-value-bind (accepted-p delivery)
                    (application-input-controller-submit-primary-prompt
                     controller "steer the goal turn")
                  (test-assert
                   (and accepted-p (eq delivery ':steering))
                   "prose steers a command-hosted steering-consuming turn"))
                (let ((entries (funcall take)))
                  (test-assert
                   (and (= (length entries) 1)
                        (string= (user-message-input-text
                                  (agent-steering-input-content
                                   (first entries)))
                                 "steer the goal turn"))
                   "the running turn takes the steered prose")
                  (funcall acknowledge
                           (agent-steering-input-identifier (first entries)))
                  (test-assert
                   (null (application-input-controller--state
                          controller :steering-in-flight-items))
                   "delivered goal steering retires durably"))))
             (multiple-value-bind (accepted-p delivery)
                 (application-input-controller-submit-primary-prompt
                  controller "after the steering consumer ended")
               (test-assert
                (and accepted-p (eq delivery ':queued))
                "prose queues again once the steering consumer ends")))
        (application-input-controller-stop controller))))
  nil)

(-> test-project-adaptation-offer-lifecycle () null)
(defun test-project-adaptation-offer-lifecycle ()
  "An attached channel offers once after successful turn work reaches idle."
  (with-test-configuration (configuration)
    (dolist (picker-p '(nil t))
      (let* ((application (mission-test--application
                           configuration :results (loop repeat 9 collect (mission-test--result "done"))))
             (controller  (application-input-controller-create
                           application
                            :initial-work-items        (application--initial-work-items (and picker-p "(open)"))
                           :project-adaptation-offer-p t
                           :load-pending-p nil :start-reader-p nil))
             (queue       (application-input-controller-work-items controller))
             (attached-p  t)
             (awaited-p   nil)
             (completed-p nil)
             (offers      0)
             (episode     (make-instance 'prompt-cache-episode
                                        :conversation (application-conversation application)
                                        :provider     (application-provider application)
                                        :model "fixture" :started-at 0)))
        (unwind-protect
             (test-call-with-function-replacements
              (list (list 'application-clients-attached-p
                          (lambda (ignored) (declare (ignore ignored)) attached-p))
                    (list 'application-job-completions-awaited-p
                          (lambda (ignored) (declare (ignore ignored)) awaited-p))
                    (list 'application-world-before-turn (constantly nil))
                    (list 'application-input-controller--run-idle-compaction (constantly nil))
                    (list 'application-job-completions-run
                          (lambda (controller ticket)
                            (declare (ignore controller ticket))
                            (when completed-p
                              (application--run-turn application "job results" :continuation-p t))
                            nil))
                    (list 'application-maybe-offer-project-adaptation
                          (lambda (ignored) (declare (ignore ignored)) (incf offers) nil)))
              (lambda ()
                (flet ((run (work &key cancelled-p)
                         "Run one controller item through the production finish boundary."
                         (setf (application-input-controller-active-p controller) t
                               (application-input-controller-active-work-kind controller) (first work))
                         (application-input-controller--run-work controller work)
                         (setf (application-input-controller-turn-cancellation-p controller) cancelled-p)
                         (application-input-controller--finish-work controller)))
                  (when picker-p
                    (test-assert (application-input-controller--next-work controller)
                                 "picker startup supplies its initial work")
                    (application-input-controller--finish-work controller))
                  (run (list ':idle-compaction episode))
                  (run (list ':message "cancelled") :cancelled-p t)
                  (run (list ':job-completion nil))
                  (test-assert (deque-empty-p queue)
                               "startup, maintenance, cancellation and stale tickets do not offer")
                  (setf awaited-p t)
                  (run (list ':message "start work"))
                  (test-assert (deque-empty-p queue) "a wake-authorized job blocks the offer")
                  (setf awaited-p nil completed-p t attached-p nil)
                  (run (list ':job-completion nil))
                  (test-assert (deque-empty-p queue) "a detached completion does not offer")
                  (setf attached-p t)
                  (run (list ':job-completion nil))
                  (test-assert (equal (deque->list queue) '((:project-adaptation-offer)))
                               "the final attached completion schedules the offer")
                  (run (deque-pop-front queue))
                  (run (list ':message "after offer"))
                  (test-assert (and (= offers 1) (deque-empty-p queue))
                               "subsequent completed turns do not repeat the offer"))))
          (application-input-controller-stop controller)
          (terminal-ui-stop (application-ui application))))))
  nil)

(-> test-primary-prompt-admission () null)
(defun test-primary-prompt-admission ()
  "Test primary prompts share message steering and FIFO queue admission."
  (labels ((call-with-controller (function)
             (let* ((terminal
                      (make-instance 'waiting-recording-terminal :columns 60))
                    (ui          (terminal-ui-create :terminal terminal))
                    (application (make-instance 'application :ui ui))
                    (controller  nil))
               (with-terminal-ui (active-ui ui)
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
              (input                  nil)
              (observed               nil)
              (original-submit-prompt (symbol-function 'application-input-controller-submit-primary-prompt)))
         (unwind-protect
              (progn
                (test-conversation--write-tiny-png image)
                (setf input
                      (user-message-input-create
                       :text            "[Image #1] steer active message"
                       :image-pathnames (list (truename image))))
                (test-call-with-function-replacements
                 (list
                   (list 'application-input-controller-submit-primary-prompt
                         (lambda (&rest arguments)
                           (setf observed arguments)
                           (apply original-submit-prompt arguments))))
                 (lambda ()
                   (application-input-controller--handle-submission
                    controller input :steer-p t)))
                (let ((observed-input (second observed))
                      (steering-input
                        (first (application-input-controller--state
                                controller :steering-items))))
                  (test-assert
                   (and (typep observed-input 'user-message-input)
                        (string= (user-message-input-text observed-input)
                                 "[Image #1] steer active message")
                        (equal (user-message-input-image-pathnames observed-input)
                               (list (truename image)))
                        (getf (nthcdr 2 observed) :prefer-steering-p)
                      (string=
                       (user-message-input-text steering-input)
                       "[Image #1] steer active message")
                      (equal
                       (user-message-input-image-pathnames steering-input)
                       (list (truename image))))
                   "terminal image prose invokes the primary composer admission with steering")))
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
              (original-submit-prompt
                (symbol-function 'application-input-controller-submit-primary-prompt)))
         (test-call-with-function-replacements
          (list
            (list 'application-input-controller-submit-primary-prompt
                  (lambda (&rest arguments)
                    (setf observed arguments)
                    (apply original-submit-prompt arguments))))
          (lambda ()
            (terminal-ui-set-input ui "ordinary during local Lisp")
            (application-input-controller--process-event controller ':complete)))
         (test-assert
           (and (equal (second observed) "ordinary during local Lisp")
                (null (getf (nthcdr 2 observed) :prefer-steering-p))
               (null (application-input-controller--state
                      controller :steering-items))
               (equal (application-input-controller--state controller :work-items)
                      '((:message "ordinary during local Lisp"))))
          "terminal prose invokes the primary composer admission without message steering"))))
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
       (setf (application-input-controller-pending-persistence-enabled-p controller)
             nil)
       (multiple-value-bind (accepted-p delivery)
           (application-input-controller-submit-primary-prompt
            controller "(vault)")
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
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
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
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (image       (merge-pathnames "follow-up-cycle.png"
                                       (uiop:temporary-directory)))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
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
             (terminal-ui-set-input ui "(status)")
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
               :text            "[Image #1] edited newest thought"
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
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
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
  (let* ((terminal    (make-instance 'waiting-recording-terminal :columns 60))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (controller  nil))
    (with-terminal-ui (active-ui ui)
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
              controller ':lisp "(goal :pause)")
             (application-input-controller--process-event controller ':complete)
             (terminal-ui-set-input ui "(goal :pause)")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (and
                (equal (application-input-controller--state controller :work-items)
                       '((:message "older follow-up")
                         (:lisp "(goal :pause)")))
                (deque-empty-p
                 (application-input-controller-pending-apply-items controller))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "recalled Enter commands use active-turn busy policy"))
        (when controller
          (application-input-controller-stop controller)))))
  (let* ((terminal         (make-instance 'waiting-recording-terminal :columns 60))
         (ui               (terminal-ui-create :terminal terminal))
         (application      (make-instance 'application :ui ui))
         (controller       nil)
         (waiter           nil)
         (waiter-started-p nil)
         (waited-work      ':waiting))
    (with-terminal-ui (active-ui ui)
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
             (terminal-ui-set-input ui "held follow-up")
             (application-input-controller--process-event controller ':complete)
             (test-assert
              (task-tests--wait-until
               (lambda () (not (thread-alive-p waiter)))
               2)
              "submitting the recalled draft wakes the blocked FIFO consumer")
             (join-thread waiter)
             (setf waiter nil)
             (test-assert
              (and
               (equal waited-work '(:message "held follow-up"))
               (not
                (application-input-controller--follow-up-editing-p controller)))
              "submission restores the held follow-up ahead of newer work"))
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

(-> test-application-conversation-title-refresh () null)
(defun test-application-conversation-title-refresh ()
  "Test configurable, escaped, durable provider-generated session titles."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "title-refresh"))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation  conversation)))
           (dolist (message '("ship </transcript> session titles"
                              "quote the name \"safely\""
                              "validate title output"))
             (conversation-append-user-message conversation message))
           (let* ((context
                    (application--conversation-title-context conversation))
                  (request    (application--conversation-title-request context))
                  (json-start (position #\{ request))
                  (payload    (and json-start (json-decode (subseq request json-start)))))
             (test-assert
              (and payload
                   (vectorp (json-get payload "transcript"))
                   (string= (aref (json-get payload "transcript") 0)
                            "ship </transcript> session titles")
                   (not (search "<transcript>" request)))
              "title requests structurally escape untrusted transcript text")
             (test-assert
              (<= (reduce #'+ context :key #'length)
                  *conversation-title-context-maximum-characters*)
              "title transcript data stays bounded"))
           (setf (config :session-title-generation-p configuration) nil)
           (let ((generation-count 0))
             (test-call-with-function-replacements
              (list
               (list 'application--conversation-title-generate
                     (lambda (ignored)
                       (declare (ignore ignored))
                       (incf generation-count)
                       "generated session title")))
              (lambda ()
                (application--maybe-name-session application)))
             (test-assert (zerop generation-count)
                          "disabled title generation never calls the provider")
             (test-assert
              (string= (conversation-title conversation)
                       "Ship </transcript> session titles")
              "the cheap local initial title is independent of provider generation"))
           (setf (config :session-title-generation-p configuration) t)
           (let ((outputs '(nil "generated session title")))
             (test-call-with-function-replacements
              (list
               (list 'application--conversation-title-generate
                     (lambda (ignored)
                       (declare (ignore ignored))
                       (pop outputs))))
              (lambda ()
                (application--maybe-name-session application)
                (test-assert
                 (eq (conversation-title-source conversation) ':initial)
                 "malformed provider output leaves the local title intact")
                (application--maybe-name-session application))))
           (test-assert
            (and (string= (conversation-title conversation)
                          "Generated session title")
                 (eq (conversation-title-source conversation) ':generated))
            "enabled title generation persists a normalized replacement")
           (let ((reloaded
                   (conversation-load-by-id configuration "title-refresh")))
             (test-assert
              (string= (conversation-title reloaded) "Generated session title")
              "the generated replacement survives conversation reload")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-conversation-agent-name () null)
(defun test-application-conversation-agent-name ()
  "Test an agent is named for its work once, after enough turns or tool calls, in the title's tool-free request."
  (flet ((scripted (&rest texts)
           "Return a provider that answers the naming requests with TEXTS in turn."
           (make-instance 'rlm-inference-test-provider
                          :results (mapcar (lambda (text)
                                             (rlm-inference-test-result "naming" text 10))
                                           texts)))

         (requests (provider)
           "Return how many requests PROVIDER answered."
           (length (rlm-inference-test-provider-input-snapshots provider)))

         (agent (configuration conversation)
           "Return CONVERSATION's agent as the world holds it now."
           (world-register-conversation configuration conversation)))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (scripted "not JSON"
                                     "{\"title\":\"Fix shell activity visibility\",\"agent_name\":\"bad name\"}"
                                     "{\"title\":\"Another session title\",\"agent_name\":\"shell-visibility\"}"))
             (application  (make-instance 'application :configuration configuration
                                                       :conversation  conversation
                                                       :provider      provider)))
        (dolist (message '("show shell activity" "keep notices with calls" "verify the repair"))
          (conversation-append-user-message conversation message))
        (let ((handle (participant-name (agent configuration conversation))))
          (application--maybe-name-session application)
          (test-assert (and (eq (conversation-title-source conversation) ':initial)
                            (eq (participant-name-source (agent configuration conversation)) ':initial))
                       "an invalid response leaves the title and the name eligible for retry")
          (application--maybe-name-session application)
          (test-assert (and (equal (conversation-title conversation) "Fix shell activity visibility")
                            (eq (conversation-title-source conversation) ':generated)
                            (equal (participant-name (agent configuration conversation)) handle))
                       "a valid title is kept while an invalid name leaves the starting handle")
          (application--maybe-name-session application)
          (let ((named (agent configuration conversation)))
            (test-assert (and (equal (participant-name named) "shell-visibility")
                              (eq (participant-name-source named) ':generated)
                              (equal (conversation-title conversation) "Fix shell activity visibility"))
                         "the next valid response names the agent and leaves the generated title alone")
            (test-assert (equal (participant-identifier
                                 (world-participant-designated (world-load configuration)
                                                               "shell-visibility"))
                                (participant-identifier named))
                         "the inferred handle is at once the one mentions use"))
          (application--maybe-name-session application)
          (test-assert (and (= (requests provider) 3)
                            (equal (rlm-inference-test-provider-tool-schema-counts provider) '(0 0 0))
                            (= (conversation-user-turn-count conversation) 3))
                       "each attempt is one tool-free inference, and a named, titled session asks for none"))))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (scripted "{\"title\":\"A title nobody asked for\",\"agent_name\":\"queue-repair\"}"))
             (application  (make-instance 'application :configuration configuration
                                                       :conversation  conversation
                                                       :provider      provider)))
        (conversation-append-user-message conversation "repair the queue")
        (setf (application-action-count application) (1- *application-agent-name-action-count*))
        (application--maybe-name-session application :title-p nil)
        (test-assert (zerop (requests provider))
                     "an agent that has done little keeps its starting handle")
        (setf (application-action-count application) *application-agent-name-action-count*)
        (application--maybe-name-session application :title-p nil)
        (test-assert (and (equal (participant-name (agent configuration conversation)) "queue-repair")
                          (eq (conversation-title-source conversation) ':initial))
                     "enough tool calls name the agent after a turn nobody typed, which earns no title")))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (scripted "{\"title\":\"Fix shell activity visibility\",\"agent_name\":\"inferred-name\"}"))
             (application  (make-instance 'application :configuration configuration
                                                       :conversation  conversation
                                                       :provider      provider)))
        (dolist (message '("show shell activity" "keep notices with calls" "verify the repair"))
          (conversation-append-user-message conversation message))
        (world-participant-rename
         configuration (participant-identifier (agent configuration conversation)) "chosen-name")
        (application--maybe-name-session application)
        (application--maybe-name-session application)
        (test-assert (and (equal (participant-name (agent configuration conversation)) "chosen-name")
                          (eq (conversation-title-source conversation) ':generated)
                          (= (requests provider) 1))
                     "a name someone chose stays, and only the title is asked for")))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (application  (make-instance 'application :configuration configuration :conversation conversation))
             (calls        0))
        (conversation-append-user-message conversation "repair the queue")
        (setf (application-action-count application) *application-agent-name-action-count*)
        (test-call-with-function-replacements
         (list (list 'application--conversation-title-generate
                     (lambda (owner)
                       (declare (ignore owner))
                       (incf calls)
                       (error "Naming provider unavailable."))))
         (lambda ()
           (loop repeat (+ *application-agent-name-attempt-limit* 2)
                 do (application--maybe-name-session application :title-p nil))))
        (test-assert (= calls *application-agent-name-attempt-limit*)
                     "a failing naming request cannot start a fresh inference every turn")))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (answers      (loop repeat (1+ *application-agent-name-attempt-limit*)
                                 collect "{\"title\":\"Fix shell activity visibility\"}"))
             (provider     (apply #'scripted answers))
             (application  (make-instance 'application :configuration configuration
                                                       :conversation  conversation
                                                       :provider      provider)))
        (dolist (message '("show shell activity" "keep notices with calls" "verify the repair"))
          (conversation-append-user-message conversation message))
        (loop repeat (length answers)
              do (application--maybe-name-session application))
        (test-assert (and (= (requests provider) *application-agent-name-attempt-limit*)
                          (eq (participant-name-source (agent configuration conversation)) ':initial))
                     "a process stops asking once enough answers brought no usable name"))))
  (dolist (case '(("not JSON" nil nil)
                  ("[]" nil nil)
                  ("{\"title\":\"Too short\",\"agent_name\":\"valid-name\"}" nil "valid-name")
                  ("{\"title\":\"A valid session title\",\"agent_name\":\"parent\"}"
                   "A valid session title" nil)
                  ("{\"title\":\"A valid session title\",\"agent_name\":42}"
                   "A valid session title" nil)
                  ("{\"title\":\"A valid session title\",\"agent_name\":\"Mixed-Case\"}"
                   "A valid session title" "mixed-case")))
    (destructuring-bind (text title name) case
      (test-assert (equal (multiple-value-list (application--conversation-title-response text))
                          (list title name))
                   "a response yields the valid title and the valid handle, each on its own")))
  nil)

(-> test-conversation-picker () null)
(defun test-conversation-picker ()
  "Test saved-conversation picker items and interactive selection."
  (dolist (case '(("sandbox" :sandboxed) ("full" :full-access)))
    (let ((observed-mode nil))
      (test-call-with-function-replacements
       (list (list 'main--interactive-input-p (constantly t))
             (list 'main--run-world-client
                   (lambda (configuration &key launch &allow-other-keys)
                     (declare (ignore configuration))
                     (setf observed-mode (getf launch :permission-mode)))))
       (lambda () (main-dispatch (list "--permissions" (first case)))))
      (test-assert (eq observed-mode (second case))
                   "an explicit permission selection reaches the world channel launch")))
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (current-workspace  (merge-pathnames "current-workspace/" root))
         (other-workspace    (merge-pathnames "other-workspace/" root)))
    (ensure-directories-exist current-workspace)
    (ensure-directories-exist other-workspace)
    (unwind-protect
         (let* ((configuration
                  (configuration-copy
                   base-configuration :working-directory
                   current-workspace))
                (other-configuration
                  (configuration-copy
                   base-configuration :working-directory
                   other-workspace))
                (current-older
                  (conversation-create configuration
                                       :identifier "current-older"))
                (active      (conversation-create configuration :identifier "active"))
                (other-older
                  (conversation-create other-configuration
                                       :identifier "other-older"))
                (other-newer
                  (conversation-create other-configuration
                                       :identifier "other-newer"))
                (terminal    (make-instance 'scripted-terminal :columns 60))
                (application (make-instance 'application
                                            :configuration configuration
                                            :conversation  active
                                            :ui            (terminal-ui-create
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
           (let ((now (get-universal-time)))
             (flet ((set-activity (conversation seconds-ago)
                      (platform-set-file-times
                       *platform*
                       (conversation-log-pathname conversation)
                       (- now seconds-ago))))
               (set-activity active 10)
               (set-activity other-newer 20)
               (set-activity current-older 30)
               (set-activity other-older 40)))
           (dolist (conversation (list active current-older other-newer other-older))
             (world-register-conversation configuration conversation))
           (terminal-ui-start (application-ui application))
           (recording-terminal-reset terminal)
           (setf (scripted-terminal-events terminal)
                 (list :delete :escape))
           (test-assert (null (application--pick-channel application))
                        "escape cancels after refusing to remove the channel shown")
           (test-assert
             (and (conversation-storage-occupied-p
                   (conversation-pathname active))
                 (search "switch away to remove it"
                         (recording-terminal-output terminal)))
            "the switch picker refuses to remove the channel this terminal shows")
           (recording-terminal-reset terminal)
           (setf (scripted-terminal-events terminal)
                 (list :history-next :delete :submit :escape))
           (test-call-with-function-replacements
            (list (list 'application--delete-channel
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error 'configuration-error :message "Removal refused."))))
            (lambda ()
              (test-assert (and (null (application--pick-channel application))
                                (search "refused" (recording-terminal-output terminal)))
                           "the picker reports a removal failure and returns to browsing")))
           (setf (scripted-terminal-events terminal)
                 (list :history-next
                       :delete
                       :history-next
                       :submit
                       :escape))
           (test-assert (null (application--pick-channel application))
                        "keeping a channel returns to the browse picker")
           (test-assert
            (conversation-storage-occupied-p
             (conversation-pathname current-older))
            "the keep confirmation preserves the channel")
           (recording-terminal-reset terminal)
           (setf (scripted-terminal-events terminal)
                 (list :history-next :delete :submit :escape))
           (let ((remove-function (symbol-function 'channel-remove))
                 (painted-p       nil))
             (test-call-with-function-replacements
              (list
               (list 'channel-remove
                     (lambda (&rest arguments)
                       (test-assert (null (terminal-ui-selector (application-ui application)))
                                    "cleanup starts after the picker releases its modal")
                       (test-assert (terminal-ui-local-activity (application-ui application))
                                    "cleanup uses the local activity indicator")
                       (let ((painter
                               (make-thread
                                (lambda ()
                                  (with-terminal-ui-locked ((application-ui application))
                                    (setf painted-p t)))
                                :name "picker cleanup presentation probe")))
                         (unwind-protect
                              (test-assert (task-tests--wait-until (lambda () painted-p) 2)
                                           "presentation can advance during channel cleanup")
                           (image-daemon:daemon-stop-thread painter)))
                       (apply remove-function arguments))))
              (lambda ()
                (test-assert (null (application--pick-channel application))
                             "removal returns to browsing until escape")))
             (test-assert (null (terminal-ui-local-activity (application-ui application)))
                          "cleanup restores the previous local activity"))
           (test-assert (null (world-channel-find (world-load configuration) "current-older"))
                        "confirming in the picker removes the selected channel")
           (terminal-ui-stop (application-ui application)))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (let ((application (application-tests--ui-application :columns 60)))
    (test-assert (handler-case
                     (progn
                       (application--pick-identifier application
                                                     :title        "resume"
                                                     :items        nil
                                                     :usage        "Usage: (resume \"ID\")"
                                                     :empty-notice "none")
                       nil)
                   (configuration-error (condition)
                     (not (null (search "Usage: (resume"
                                        (format nil "~A" condition))))))
                 "non-interactive pickers demand an explicit identifier"))
  nil)

(-> test-application-window-title () null)
(defun test-application-window-title ()
  "Test that a started UI writes autolith - workspace as OSC 0."
  (let* ((configuration (test-configuration))
         (terminal      (make-instance 'recording-terminal :columns 80))
         (application
           (make-instance 'application
                          :configuration configuration
                          :tool-registry (make-default-tool-registry)
                          :ui            (terminal-ui-create :terminal terminal)))
         (title         (application--window-title application)))
    (test-assert
     (and title
          (uiop:string-prefix-p "autolith - " title)
          (not (application-sync-window-title application)))
     "an unstarted UI writes no window title")
    (with-terminal-ui (ui (application-ui application))
      (recording-terminal-reset terminal)
      (test-assert
       (and (application-sync-window-title application)
            (equal (recording-terminal-chunks terminal)
                   (list (window-title-sequence title))))
       "a started UI writes autolith - workspace as the window title")))
  nil)

(-> test-working-directory-switch () null)
(defun test-working-directory-switch ()
  "Test transactional application, process, and worker workspace changes."
  (let* ((configuration              (test-configuration))
         (root                       (test-configuration-root configuration))
         (workspace                  (merge-pathnames "workspace with spaces/" root))
         (old-workspace              (merge-pathnames "old workspace/" root))
         (observation-path
           (merge-pathnames "directory-init-observation.sexp" workspace))
         (old-observation-path
           (merge-pathnames "directory-init-observation.sexp" old-workspace))
         (broken-workspace           (merge-pathnames "broken workspace/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults          *default-pathname-defaults*)
         (extension-registry-snapshot
           (application--extension-registry-snapshot))
         (pool                       nil)
         (closable-application       nil))
    (ensure-directories-exist workspace)
    (ensure-directories-exist old-workspace)
    (ensure-directories-exist broken-workspace)
    (ensure-directories-exist (configuration-directory-init-path workspace))
    (with-open-file (stream (configuration-directory-init-path workspace)
                            :direction         ':output
                            :if-exists         ':supersede
                            :if-does-not-exist ':create
                            :external-format   ':utf-8)
      (let ((*print-readably* nil))
        (write
         '(with-open-file
              (stream "directory-init-observation.sexp"
                      :direction         :output
                      :if-exists         :supersede
                      :if-does-not-exist :create
                      :external-format   :utf-8)
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
       :name     "workspace-scoped"
       :approval ':deny)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "working-directory"))
                (provider      (provider-create configuration))
                (registry      (make-default-tool-registry))
                (worker-pool   (lisp-worker-pool-create configuration))
                (agent         (agent-create :configuration configuration
                                             :provider      provider
                                             :conversation  conversation
                                             :tool-registry registry
                                             :worker        worker-pool))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation  conversation
                                 :provider      provider
                                 :tool-registry registry
                                 :worker        worker-pool
                                 :agent         agent
                                 :ui
                                 (terminal-ui-create
                                  :terminal
                                  (make-instance 'recording-terminal :columns 80))))
                (worker        nil)
                (publish-count 0))
           (setf pool worker-pool
                 closable-application application
                 worker (lisp-worker-manager-start pool "workspace" "pristine"))
           (lisp-worker-request
            worker :eval '(:forms ("(defparameter *workspace-marker* 73)")))
           (uiop:chdir old-workspace)
           (setf *default-pathname-defaults* old-workspace)
           (let ((selected
                   (test-call-with-function-replacements
                    (list (list 'application-clients-publish-state
                                (lambda (owner)
                                  (unless (eq owner application) (error "Unexpected application."))
                                  (incf publish-count)
                                  (test-assert
                                   (and (not (eq (application-agent owner) agent))
                                        (eq (agent-configuration (application-agent owner))
                                            (application-configuration owner))
                                        (eq (provider-configuration (application-provider owner))
                                            (application-configuration owner)))
                                   "client state sees the new runtime"))))
                    (lambda () (application-set-working-directory application workspace)))))
             (test-assert (= publish-count 1) "workspace state is published once")
            (test-assert (equal selected (truename workspace))
                         "workspace switching returns the selected directory")
            (test-assert
             (equal (config :working-directory
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
                                             :direction       ':input
                                             :external-format ':utf-8)
                      (let ((*read-eval* nil))
                        (read stream)))))
              (test-assert
               (equal observation
                      (list (namestring (truename workspace))
                            (namestring (truename workspace))))
               "directory initialization observes the target process and pathname directories"))
            (test-assert
             (equal (config :working-directory
                     (agent-configuration (application-agent application)))
                    (truename workspace))
             "workspace switching reconnects the agent with the new directory")
            (test-assert
             (equal (config :working-directory
                     (provider-configuration
                      (application-provider application)))
                    (truename workspace))
             "workspace switching reconnects the provider with the new directory")
            (let ((ui (application-ui application))
                  (active-configuration
                    (application-configuration application)))
             (test-assert
              (and (plusp (terminal-ui-context-used ui))
                   (terminal-ui-context-estimated-p ui)
                   (= (terminal-ui-context-window ui)
                      (config :context-window active-configuration))
                   (= (terminal-ui-context-compaction-limit ui)
                      (configuration-compaction-token-limit
                       active-configuration)))
              "workspace switching refreshes the context meter after replacement"))
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
                    '(:forms
                      ("(list *workspace-marker* (namestring (uiop:getcwd)))")))))
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
             (setf publish-count 0)
             (test-call-with-function-replacements
              (list (list 'application-clients-publish-state
                          (lambda (application)
                            (declare (ignore application))
                            (incf publish-count))))
              (lambda ()
                (test-assert
                 (handler-case
                     (progn (application-set-working-directory application broken-workspace) nil)
                   (working-directory-error (condition)
                     (eq (working-directory-error-stage condition) ':tools)))
                 "malformed inherited MCP prevents a workspace switch")))
             (test-assert
              (and
                (zerop publish-count)
               (eq (application-configuration application)
                   active-configuration)
               (equal (uiop:getcwd) (truename workspace))
               (eq
                (mcp-server-registration-source
                 (test-directory-configuration--registration
                  "workspace-scoped"))
                :directory))
              "failed preparation restores the runtime without announcing a commit"))
           (dolist (failure-stage '(:state :title))
             (let* ((target               (if (eq failure-stage ':state)
                                              workspace
                                              old-workspace))
                    (active-configuration (application-configuration application))
                    (active-provider      (application-provider application))
                    (active-registry      (application-tool-registry application))
                    (active-agent         (application-agent application))
                    (failure              (make-condition 'simple-error
                                                          :format-control "Post-commit failure."))
                    (events               nil))
               (test-call-with-function-replacements
                (list
                 (list 'application-clients-publish-state
                       (lambda (application)
                         (declare (ignore application))
                         (push ':state events)
                         (when (eq failure-stage ':state)
                           (error failure))))
                 (list 'application-sync-window-title
                       (lambda (application)
                         (declare (ignore application))
                         (push ':title events)
                         (when (eq failure-stage ':title)
                           (error failure)))))
                (lambda ()
                  (test-assert
                   (handler-case
                       (progn
                         (application-set-working-directory application target)
                         nil)
                     (simple-error (condition)
                       (eq condition failure)))
                   "post-commit presentation failures propagate directly")))
               (test-assert (equal (reverse events) (if (eq failure-stage ':state) '(:state) '(:state :title)))
                            "title publication follows successful client state publication")
               (test-assert
                (and (not (eq (application-configuration application) active-configuration))
                     (not (eq (application-provider application) active-provider))
                     (not (eq (application-tool-registry application) active-registry))
                     (not (eq (application-agent application) active-agent))
                     (equal (config :working-directory (application-configuration application))
                            (platform-truename *platform* target))
                     (equal (uiop:getcwd) (platform-truename *platform* target))
                     (equal *default-pathname-defaults* (platform-truename *platform* target)))
                "post-commit failures retain the new application and process runtime")
               (let ((worker-state
                       (lisp-worker-request
                        worker :eval
                        '(:forms ("(list *workspace-marker* (namestring (uiop:getcwd)))")))))
                 (test-assert
                  (and (search "73" (first (getf (rest worker-state) :values)))
                       (search (namestring target)
                               (first (getf (rest worker-state) :values))))
                  "post-commit failures retain the worker heap and selected directory")))))
      (when pool
        (lisp-worker-pool-stop-all pool))
      (when closable-application
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry closable-application))))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (application--extension-registry-restore extension-registry-snapshot)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-busy-conversation-resume () null)
(defun test-application-busy-conversation-resume ()
  "Test resume claims ownership before crash repair can mutate a transcript."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
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
         (application   nil))
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
                  :configuration      configuration
                  :conversation       current
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-fresh-conversation-lease-collision () null)
(defun test-application-fresh-conversation-lease-collision ()
  "Test fresh application ownership probes another seed after a lease race."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (timestamp     3992846378)
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-tool-runtime-lifecycle () null)
(defun test-application-tool-runtime-lifecycle ()
  "Test conversation switching replaces runtimes before checkpoint detachment."
  (let* ((configuration    (test-configuration))
         (root             (test-configuration-root configuration))
         (registry         (make-instance 'tool-registry))
         (runtime-identity (list ':application-runtime))
         (close-count      0)
         (detach-count     0))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration
                                       :identifier "runtime-first"))
                (second-conversation
                  (conversation-create configuration
                                       :identifier "runtime-second"))
                (provider    (provider-create configuration))
                (tool
                  (make-instance
                   'tool-test-runtime-tool
                   :namespace        "test"
                   :name             "runtime"
                   :description      "Exercise application runtime cleanup."
                   :parameters       (tool-object-schema (json-object) nil)
                   :runtime-identity runtime-identity
                   :close-function   (lambda () (incf close-count))
                   :detach-function  (lambda () (incf detach-count))))
                (application nil))
           (tool-registry-register registry tool)
           (setf registry (task-augment-tool-registry registry))
           (setf application
                 (make-instance
                  'application
                  :configuration configuration
                  :conversation  first-conversation
                  :provider      provider
                  :tool-registry registry
                  :worker        nil
                  :agent         (agent-create :configuration configuration
                                               :provider      provider
                                               :conversation  first-conversation
                                               :tool-registry registry
                                               :worker        nil)
                  :ui            nil))
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
                (and
                 (null (application-conversation-lease application))
                 (test-conversation--child-can-acquire-lease-p
                  configuration
                  (conversation-identifier second-conversation)))
                "checkpoint detachment excludes the conversation lease"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-conversation-switch-mcp-sharing () null)
(defun test-application-conversation-switch-mcp-sharing ()
  "Test conversation switching keeps unchanged MCP connections and replaces changed ones."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registrations (mcp--registry-snapshot))
         (registries    nil))
    (unwind-protect
         (progn
           (mcp--registry-restore nil)
           (register-mcp-server
            '(:name "switch-shared"
              :transport (:type :stdio :command "/autolith-tests/no-such-mcp-server")
              :startup-timeout-seconds 1)
            :source ':user)
           (let* ((registry     (application--create-tool-registry configuration))
                  (manager      (mcp-tool-registry-manager registry))
                  (runtime      (first (mcp-manager-runtimes manager)))
                  (provider     (provider-create configuration))
                  (conversation (conversation-create configuration :identifier "mcp-first"))
                  (application
                    (make-instance
                     'application
                     :configuration configuration
                     :conversation  conversation
                     :provider      provider
                     :tool-registry registry
                     :worker        nil
                     :agent         (agent-create :configuration configuration
                                                  :provider      provider
                                                  :conversation  conversation
                                                  :tool-registry registry
                                                  :worker        nil)
                     :ui            nil)))
             (push registry registries)
             (flet ((install (identifier)
                      (application-install-conversation
                       application (conversation-create configuration :identifier identifier))
                      (push (application-tool-registry application) registries)
                      (mcp-tool-registry-manager (application-tool-registry application))))
               (test-assert (eq ':failed (mcp-server-runtime-state runtime))
                            "the fixture server records its startup failure")
               (test-assert (and (eq manager (install "mcp-second"))
                                 (eq ':failed (mcp-server-runtime-state runtime)))
                            "switching with unchanged MCP servers keeps their manager open")
               (let ((current (application-tool-registry application)))
                 (test-call-with-function-replacements
                  (list (list 'application--load-goal
                              (lambda (application)
                                (declare (ignore application))
                                (error "late conversation install failure"))))
                  (lambda ()
                    (test-assert (handler-case (progn (install "mcp-failed") nil)
                                   (application-runtime-replacement-error () t))
                                 "an injected install failure rolls the switch back")))
                 (test-assert (and (eq current (application-tool-registry application))
                                   (eq manager (mcp-tool-registry-manager current))
                                   (eq ':failed (mcp-server-runtime-state runtime)))
                              "a rolled back switch leaves the shared manager open and owned"))
               (register-mcp-server
                '(:name "switch-added"
                  :transport (:type :stdio :command "/autolith-tests/no-such-mcp-server")
                  :startup-timeout-seconds 1)
                :source ':user)
               (let ((replacement (install "mcp-third")))
                 (test-assert (and replacement
                                   (not (eq replacement manager))
                                   (eq ':disconnected (mcp-server-runtime-state runtime)))
                              "changed MCP servers get a new manager and close the old one")))))
      (dolist (registry registries)
        (handler-case (tool-registry-close-runtime-state registry)
          (serious-condition () nil)))
      (mcp--registry-restore registrations)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
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
      :namespace        "transition"
      :name             name
      :description      (format nil "Runtime marker ~A." name)
      :parameters       (tool-object-schema (json-object) nil)
      :runtime-identity (list ':runtime-transition name)
      :close-function   close-function
      :resume-function  resume-function
      :detach-function  (lambda () nil)))
    (task-augment-tool-registry registry)))

(-> test-application-conversation-input-history () null)
(defun test-application-conversation-input-history ()
  "Test complete editable history and bounded application views without archived replay."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "input-history")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "first user")
           (conversation-append-record
            conversation
            '(:message :role :assistant :content "ignored assistant"))
           (conversation-append-user-operation
            conversation
            :kind   ':command
            :source "help"
            :status ':ok
            :result "shown")
           (conversation-append-provider-metadata
            conversation
            '(:usage (("input_tokens" 3000) ("cached_input_tokens" 1400))))
           (conversation-append-summary conversation (make-continuation :intent "ignored summary"))
           (conversation-append-user-operation
            conversation
            :kind   ':lisp
            :source "(+ 1 2)"
            :status ':ok
            :result "3")
           (conversation-append-user-message conversation "last user")
           (let ((retired (first (conversation-storage-pathnames
                                  (conversation-pathname conversation)))))
             (with-open-file (stream retired :direction ':output
                                     :if-exists         ':supersede)
               (write-string "(" stream)))
           (let* ((loaded   (conversation-load-by-id configuration "input-history"))
                  (baseline (prompt-cache-baseline-from-conversation loaded))
                  (metadata (conversation-picker-metadata-scan
                             (conversation-pathname loaded))))
             (test-assert
              (equal
               (conversation-input-history-page loaded :limit 3)
               '("help" "(+ 1 2)" "last user"))
              "editable history restores a bounded view without reading retired chunks")
             (test-assert
              (null (conversation-input-history-page loaded :limit 0))
              "a disabled editor history returns no inputs")
             (test-assert
              (and baseline
                   (= (prompt-cache-baseline-prompt-tokens baseline) 3000)
                   (= (prompt-cache-baseline-cached-tokens baseline) 1400))
              "prompt-cache startup uses checkpointed usage without retired chunks")
             (test-assert
              (and metadata
                   (= (conversation-picker-metadata-user-turn-count metadata) 2)
                   (equal (conversation-picker-metadata-preview metadata) "last user"))
              "a missing picker sidecar rebuilds from the self-contained active chunk")
             (let* ((editor (line-editor-create :history-limit 1))
                    (ui     (terminal-ui-create
                             :terminal (make-instance 'recording-terminal :columns 40)
                             :editor   editor)))
               (application--load-conversation-input-history ui loaded)
               (terminal-ui-set-input ui "draft")
               (dolist (input '("last user" "(+ 1 2)" "help" "first user"))
                 (terminal-ui-process-event ui ':history-previous)
                 (test-assert (string= input (line-editor-text editor))
                              "application recall pages archived inputs through the sidecar"))
               (dolist (input '("help" "(+ 1 2)" "last user" "draft"))
                 (terminal-ui-process-event ui ':history-next)
                 (test-assert (string= input (line-editor-text editor))
                              "application recall pages forward and restores its draft")))
             (conversation-append-provider-metadata loaded '(:usage nil))
             (conversation-append-summary loaded (make-continuation :intent "second checkpoint"))
             (let* ((reloaded (conversation-load-by-id configuration "input-history"))
                    (identity (conversation-pathname reloaded))
                    (active   (conversation-log-pathname reloaded))
                    (header   (conversation-peek-header active)))
               (test-assert
                (equal (conversation-input-history reloaded)
                       '("first user" "help" "(+ 1 2)" "last user"))
                "complete editable inputs survive repeated rotation without duplication")
               (test-assert
                (= (prompt-cache-baseline-prompt-tokens
                    (prompt-cache-baseline-from-conversation reloaded)) 3000)
                "a provider response without usage preserves the previous checkpoint")
               (let ((older-header (copy-tree header)))
                 (remf (rest older-header) :input-history-last-sequence)
                 (remf (rest older-header) :prompt-cache-usage)
                 (let ((older (conversation--from-header identity active older-header)))
                   (conversation--map-segment-records
                    identity active
                    (lambda (record) (conversation--apply-record older record)))
                   (test-assert
                    (and (equal (conversation-input-history older)
                                '("first user" "help" "(+ 1 2)" "last user"))
                         (null (prompt-cache-baseline-from-conversation older)))
                    "older headers use the complete sidecar without archived replay")))
               (dolist (case '((:input-history-last-sequence -1)
                               (:input-history-last-sequence "invalid")
                               (:prompt-cache-usage (("input_tokens" -1)
                                                     ("cached_input_tokens" 0)))
                               (:prompt-cache-usage (("input_tokens" 1)
                                                     ("cached_input_tokens" "invalid")))))
                 (let ((malformed (copy-tree header)))
                   (setf (getf (rest malformed) (first case)) (second case))
                   (test-assert
                    (handler-case
                        (progn (conversation--from-header identity active malformed) nil)
                      (conversation-invariant-error () t))
                    "malformed startup checkpoint state signals a typed corruption error"))))))
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-application-runtime-replacement-transactions () null)
(defun test-application-runtime-replacement-transactions ()
  "Test workspace and conversation switches prepare fresh runtimes atomically."
  (let* ((configuration              (test-configuration))
         (root                       (test-configuration-root configuration))
         (workspace-one              (merge-pathnames "workspace-one/" root))
         (workspace-two              (merge-pathnames "workspace-two/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults          *default-pathname-defaults*)
         (old-close-count            0)
         (workspace-close-count      0)
         (workspace-resume-count     0)
         (conversation-close-count   0)
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
         (terminal                   (make-instance 'recording-terminal :columns 80))
         (editor                     (line-editor-create :history-limit 4))
         (ui                         (terminal-ui-create :terminal terminal :editor editor))
         (provider                   (provider-create configuration))
         (application
           (make-instance
            'application
            :configuration configuration
            :conversation  first-conversation
            :provider      provider
            :tool-registry old-registry
            :worker        nil
            :agent
            (agent-create :configuration configuration
                          :provider      provider
                          :conversation  first-conversation
                          :tool-registry old-registry
                          :worker        nil)
            :ui            ui))
         (owned-registries           (list old-registry)))
    (ensure-directories-exist workspace-one)
    (ensure-directories-exist workspace-two)
    (application-connect-task-presentation application)
    (unwind-protect
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
              (lambda (configuration &key shared-mcp-manager)
                (declare (ignore configuration shared-mcp-manager))
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
                  (lambda (configuration &key shared-mcp-manager)
                    (declare (ignore configuration shared-mcp-manager))
                    (error
                     'mcp-server-startup-error
                     :message     "Required MCP server failed."
                     :server-name "required"
                     :required-p  t
                     :cause       nil))))
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
              (conversation-append-user-message
               second-conversation "second conversation history")
              (terminal-ui-load-history ui '("workspace history"))
              (terminal-ui-set-input ui "draft before install")
              (test-call-with-function-replacements
               (list
                (list
                 'application--create-tool-registry
                 (lambda (configuration &key shared-mcp-manager)
                   (declare (ignore configuration shared-mcp-manager))
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
                (terminal-ui-load-history ui '("old one" "old two"))
                (terminal-ui-set-input ui "draft before failed install")
                (terminal-ui-process-event ui :left)
                (let* ((expected-history
                         (copy-seq (line-editor-history editor)))
                       (failed-close-count 0)
                       (failed-registry
                         (application-tests--replacement-registry
                          "conversation-failed"
                          (lambda () (incf failed-close-count)))))
                  (push failed-registry owned-registries)
                  (conversation-append-user-message
                   third-conversation "replacement history")
                  (test-call-with-function-replacements
                   (list
                    (list
                     'application--create-tool-registry
                     (lambda (configuration &key shared-mcp-manager)
                       (declare (ignore configuration shared-mcp-manager))
                       failed-registry))
                    (list
                     'context-runtime-reset
                     (lambda ()
                       (terminal-ui-set-input
                        ui "input during failed install")
                       (terminal-ui-process-event ui :left)
                       (error "late conversation install failure"))))
                   (lambda ()
                     (test-assert
                      (handler-case
                          (progn
                            (application-install-conversation
                             application third-conversation)
                            nil)
                        (application-runtime-replacement-error (condition)
                          (eq
                           (application-runtime-replacement-error-stage condition)
                           ':install)))
                      "late conversation failure enters installation rollback")))
                  (test-assert
                   (and
                    (= failed-close-count 1)
                    (equalp (line-editor-history editor) expected-history)
                    (string=
                     (line-editor-text editor)
                     "input during failed install")
                    (= (line-editor-cursor editor)
                       (1- (length "input during failed install")))
                    (not (terminal-ui--editor-history-navigating-p editor)))
                   "conversation rollback leaves history and concurrent input untouched"))
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
                      (active-close-count conversation-close-count)
                      (attempted-lease    nil)
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
                     (lambda (configuration &key shared-mcp-manager)
                       (declare (ignore configuration shared-mcp-manager))
                       (error
                        'mcp-server-startup-error
                        :message     "Required MCP server failed."
                        :server-name "required"
                        :required-p  t
                        :cause       nil))))
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
                    (= conversation-close-count active-close-count)
                    (eq
                     (job-pool-lifecycle-state
                      (task-orchestrator-pool conversation-orchestrator))
                     ':open)
                    (tool-registry-find
                     conversation-registry
                     "transition"
                     "conversation-new"))
                   "failed conversation preparation leaves the old application live"))))))
      (ignore-errors
        (application-disconnect-task-presentation application))
      (application-release-conversation-lease application)
      (dolist (registry (remove-duplicates owned-registries :test #'eq))
        (ignore-errors
          (tool-registry-close-runtime-state registry)))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-runtime-retirement-failures () null)
(defun test-application-runtime-retirement-failures ()
  "Test replacement operations roll back a non-error serious retirement failure."
  (dolist (operation '(:mcp-reload :working-directory :conversation))
    (let* ((configuration                 (test-configuration))
           (root                          (test-configuration-root configuration))
           (workspace                     (merge-pathnames "retirement-workspace/" root))
           (previous-process-directory    (uiop:getcwd))
           (previous-defaults             *default-pathname-defaults*)
           (retirement-failure-enabled-p  t)
           (old-registry
             (application-tests--replacement-registry
              "retirement-old"
              (lambda ()
                (when retirement-failure-enabled-p
                  (signal
                   'application-test-runtime-retirement-interruption)))))
           (new-close-count               0)
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
           (provider                      (provider-create configuration))
           (old-agent
             (agent-create :configuration configuration
                           :provider      provider
                           :conversation  old-conversation
                           :tool-registry old-registry
                           :worker        nil))
           (application
             (make-instance
              'application
              :configuration configuration
              :conversation  old-conversation
              :provider      provider
              :tool-registry old-registry
              :worker        nil
              :agent         old-agent
              :ui            nil))
           (old-orchestrator
             (application--task-orchestrator application))
           (new-orchestrator
             (task-run-tool-orchestrator
              (tool-registry-find new-registry "child" "spawn")))
           (mcp-registration-snapshot     (mcp--registry-snapshot))
           (context-registration-snapshot (context--registry-snapshot))
           (command-registration-snapshot
             (application-command--registry-snapshot))
           (failure                       nil))
      (ensure-directories-exist workspace)
      (application-connect-task-presentation application)
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list
              'application--create-tool-registry
              (lambda (replacement-configuration &key shared-mcp-manager)
                (declare (ignore replacement-configuration shared-mcp-manager))
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
        (platform-delete-directory-tree
         *platform*
         root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> application-tests--connection-preparation-failure
    ((member :create :reconnect) (member :worker :registry :initialize :install)
     &key (:different-conversation-p boolean))
    null)
(defun application-tests--connection-preparation-failure
    (operation stage &key different-conversation-p)
  "Inject one connection preparation failure and verify exact resource ownership."
  (with-test-configuration (configuration)
    (let ((original-registry-create         (symbol-function 'application--create-tool-registry))
          (original-worker-create           (symbol-function 'lisp-worker-pool-create))
          (original-registry-close          (symbol-function 'tool-registry-close-runtime-state))
          (original-worker-stop             (symbol-function 'lisp-worker-manager-stop))
          (original-conversation-create     (symbol-function 'application--conversation-create-owned))
          (original-conversation-load       (symbol-function 'application--conversation-load-owned))
          (original-context-refresh         (symbol-function 'application-refresh-context-meter))
          (original-presentation-connect    (symbol-function 'application-connect-task-presentation))
          (original-presentation-disconnect (symbol-function 'application-disconnect-task-presentation))
          (initial-snapshot                 (application--extension-registry-snapshot))
          (injected                         (make-condition 'task-error
                                                            :message   "Injected connection preparation failure."
                                                            :tool-name "application"))
          (probing-p                        nil)
          (old-application                  nil)
          (old-registry                     nil)
          (old-worker                       nil)
          (old-lease                        nil)
          (new-application                  nil)
          (new-registry                     nil)
          (new-worker                       nil)
          (selected-lease                   nil)
          (selected-identifier              nil)
          (selected-lease-acquired-p        nil)
          (new-registry-closed-p            nil)
          (new-worker-stopped-p             nil)
          (new-presentation-disconnected-p  nil)
          (old-registry-closed-p            nil)
          (old-worker-stopped-p             nil)
          (snapshot                         nil)
          (target-id                        nil)
          (observed                         nil))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'image-state-load
                   (lambda (configuration &key pristine-p)
                     (declare (ignore configuration pristine-p))
                     nil))
             (list 'image-state-reconnect (lambda () nil))
             (list 'application--reconnect-configuration
                   (lambda (previous immutable-p)
                     (configuration-copy previous :immutable-p immutable-p)))
             (list 'application-terminal-ui-create
                   (lambda (&key client-pane configuration inbox-p)
                     (declare (ignore client-pane configuration inbox-p))
                     (terminal-ui-create
                      :terminal (make-instance 'recording-terminal :columns 72))))
             (list 'application--create-tool-registry
                    (lambda (configuration &key shared-mcp-manager)
                     (when (and probing-p (eq stage ':registry))
                       (error injected))
                     (multiple-value-bind (registry retained)
                         (funcall original-registry-create configuration
                                  :shared-mcp-manager shared-mcp-manager)
                       (when probing-p
                         (setf new-registry registry))
                       (values registry retained))))
             (list 'lisp-worker-pool-create
                   (lambda (configuration)
                     (when (and probing-p (eq stage ':worker))
                       (error injected))
                     (let ((worker (funcall original-worker-create configuration)))
                       (when probing-p
                         (setf new-worker worker))
                       worker)))
             (list 'application--conversation-create-owned
                   (lambda (application configuration &key timestamp)
                     (multiple-value-bind (conversation lease acquired-p)
                         (funcall original-conversation-create
                                  application configuration :timestamp timestamp)
                       (when probing-p
                        (setf selected-lease lease
                              selected-identifier (conversation-identifier conversation)
                              selected-lease-acquired-p acquired-p))
                       (values conversation lease acquired-p))))
             (list 'application--conversation-load-owned
                   (lambda (application configuration identifier)
                     (multiple-value-bind (conversation lease acquired-p)
                         (funcall original-conversation-load
                                  application configuration identifier)
                       (when probing-p
                        (setf selected-lease lease
                              selected-identifier (conversation-identifier conversation)
                              selected-lease-acquired-p acquired-p))
                       (values conversation lease acquired-p))))
             (list 'application-refresh-context-meter
                   (lambda (application)
                     (when (and probing-p (not (eq application old-application)))
                       (setf new-application application))
                     (funcall original-context-refresh application)
                     (when (and probing-p (eq stage ':initialize)
                                (not (eq application old-application)))
                       (error injected))))
             (list 'application-disconnect-task-presentation
                   (lambda (application)
                     (when (eq application new-application)
                       (setf new-presentation-disconnected-p t))
                     (funcall original-presentation-disconnect application)))
             (list 'application-connect-task-presentation
                   (lambda (application)
                     (funcall original-presentation-connect application)
                     (when (and probing-p (eq stage ':install)
                                (not (eq application old-application)))
                       (error injected))))
             (list 'tool-registry-close-runtime-state
                   (lambda (registry &key retain)
                     (when (eq registry new-registry)
                       (setf new-registry-closed-p t))
                     (when (eq registry old-registry)
                       (setf old-registry-closed-p t))
                     (funcall original-registry-close registry :retain retain)))
             (list 'lisp-worker-manager-stop
                   (lambda (worker)
                     (when (eq worker new-worker)
                       (setf new-worker-stopped-p t))
                     (when (eq worker old-worker)
                       (setf old-worker-stopped-p t))
                     (funcall original-worker-stop worker))))
            (lambda ()
              (when (eq operation ':reconnect)
                (setf old-application (application-create configuration :pristine-p t)
                      old-registry (application-tool-registry old-application)
                      old-worker (application-worker old-application)
                      old-lease (application-conversation-lease old-application))
                (conversation-append-user-message
                 (application-conversation old-application) "retained session")
                (when different-conversation-p
                  (let ((target (conversation-create configuration
                                                     :identifier "replacement-session")))
                    (conversation-append-user-message target "replacement session")
                    (setf target-id (conversation-identifier target)))))
              (setf snapshot (application--extension-registry-snapshot)
                    probing-p t)
              (handler-case
                  (ecase operation
                    (:create
                     (application-create configuration :pristine-p t))
                    (:reconnect
                     (application-reconnect old-application :conversation-id target-id)))
                (serious-condition (condition)
                  (setf observed condition)))
              (setf probing-p nil)
              (test-assert
               (if (eq stage ':install)
                   (and (typep observed 'application-runtime-replacement-error)
                        (eq (application-runtime-replacement-error-operation observed) ':reconnect)
                        (eq (application-runtime-replacement-error-stage observed) ':install)
                        (eq (application-runtime-replacement-error-cause observed) injected)
                        (null (application-runtime-replacement-error-rollback-causes observed)))
                   (eq observed injected))
               "connection failure preserves its cause and replacement stage")
              (test-assert
               (and (or (null new-registry) new-registry-closed-p)
                    (or (null new-worker) new-worker-stopped-p))
               "failure closes exactly the resources acquired before it")
              (when new-application
                (test-assert new-presentation-disconnected-p
                             "initialization failure disconnects the constructed application"))
              (test-assert
               (and selected-lease
                    (eq selected-lease-acquired-p
                        (not (null (or (eq operation ':create) different-conversation-p))))
                    (or (null target-id) (string= selected-identifier target-id))
                    (if selected-lease-acquired-p
                        (and (not (eq selected-lease old-lease))
                             (not (conversation-lease-held-p selected-lease)))
                        (and (eq selected-lease old-lease)
                             (conversation-lease-held-p selected-lease))))
               "failure releases new conversation leases and retains borrowed leases")
              (test-assert
               (equal snapshot (application--extension-registry-snapshot))
               "failed replacement restores extension registrations")
              (when old-application
                (test-assert
                 (and (eq (application-tool-registry old-application) old-registry)
                      (eq (application-worker old-application) old-worker)
                      (eq (application-conversation-lease old-application) old-lease)
                      (conversation-lease-held-p old-lease)
                      (not old-registry-closed-p)
                      (not old-worker-stopped-p)
                      (application-task-presentation-listener old-application))
                 "failed replacement retains old ownership and presentation")
                (let* ((executed-p nil)
                       (probe
                         (job-pool-submit
                          (task-orchestrator-pool
                           (application--task-orchestrator old-application))
                          (lambda (job)
                            (declare (ignore job))
                            (setf executed-p t))
                          :name "retained-runtime-probe")))
                  (multiple-value-bind (result terminal-p)
                      (job-await probe :timeout-seconds 5)
                    (test-assert
                     (and terminal-p executed-p (eq (getf result :state) ':completed))
                     "the retained runtime executes work after rollback"))))))
        (application--discard-connection-resources new-application new-registry new-worker)
        (when old-application
          (application--discard-connection-resources old-application old-registry old-worker)
          (application-release-conversation-lease old-application))
        (when (and selected-lease selected-lease-acquired-p
                   (conversation-lease-held-p selected-lease))
          (conversation-lease-release selected-lease))
        (application--extension-registry-restore initial-snapshot))))
  nil)

(-> test-application-create-unwind-safety () null)
(defun test-application-create-unwind-safety ()
  "Test late construction failure closes every newly owned runtime."
  (let* ((configuration                 (test-configuration))
         (root                          (test-configuration-root configuration))
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
         (mcp-registration-snapshot     (mcp--registry-snapshot))
         (context-registration-snapshot (context--registry-snapshot))
         (command-registration-snapshot
           (application-command--registry-snapshot))
         (constructed-application       nil)
         (created-registry              nil)
         (created-worker                nil)
         (registry-closed-p             nil)
         (worker-stopped-p              nil))
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
                    (funcall original-registry-create
                             configuration))))
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
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  (dolist (stage '(:worker :initialize))
    (application-tests--connection-preparation-failure ':create stage))
  nil)

(-> test-application-reconnect-unwind-safety () null)
(defun test-application-reconnect-unwind-safety ()
  "Test reconnect preparation and runtime retirement rollback."
  (labels ((run-case (stage)
             "Exercise reconnect transaction STAGE with isolated resources."
             (let* ((configuration                 (test-configuration))
                    (root                          (test-configuration-root configuration))
                    (conversation
                      (conversation-create
                       configuration
                       :identifier
                       (format nil "reconnect-transaction-~(~A~)" stage)))
                    (provider                      (provider-create configuration))
                    (old-registry
                      (application--create-tool-registry configuration))
                    (old-worker                    (lisp-worker-pool-create configuration))
                    (old-agent
                      (agent-create :configuration configuration
                                    :provider      provider
                                    :conversation  conversation
                                    :tool-registry old-registry
                                    :worker        old-worker))
                    (old-ui
                      (terminal-ui-create
                       :terminal
                       (make-instance 'recording-terminal :columns 72)))
                    (application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation  conversation
                                     :provider      provider
                                     :tool-registry old-registry
                                     :worker        old-worker
                                     :agent         old-agent
                                     :ui            old-ui))
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
                    (new-registry                  nil)
                    (new-worker                    nil)
                    (new-registry-closed-p         nil)
                    (new-worker-stopped-p          nil)
                    (old-worker-stopped-p          nil)
                    (context-transition-p          nil)
                    (image-transition-p            nil)
                    (old-close-after-transitions-p nil)
                    (old-registry-resumed-p        nil)
                    (returned-application          nil)
                    (failure-p                     nil))
               (configuration-ensure-directories configuration)
               (application-connect-task-presentation application)
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list
                       'configuration-create
                        (lambda (&key source-root working-directory model
                                     site-config-root reasoning-effort codex-fast-mode-p
                                     immutable-p defer-provider-validation-p
                                     &allow-other-keys)
                         (declare (ignore source-root
                                    site-config-root
                                    defer-provider-validation-p))
                         (configuration-copy
                          configuration
                          :working-directory working-directory
                          :model             model
                          :reasoning-effort  reasoning-effort
                          :codex-fast-mode-p codex-fast-mode-p
                          :immutable-p       immutable-p)))
                      (list
                       'application-terminal-ui-create
                       (lambda (&key client-pane configuration inbox-p)
                         (declare (ignore client-pane configuration inbox-p))
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
                         (multiple-value-bind (registry retained)
                             (funcall original-registry-create
                                     new-configuration)
                           (setf new-registry registry)
                           (values registry retained))))
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
                                       :tool-name "child.spawn")
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
                              new-registry "child" "spawn"))))
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
                (platform-delete-directory-tree
                 *platform*
                 root :validate t :if-does-not-exist ':ignore)))))
    (run-case ':pre-commit)
    (run-case ':retirement))
  (dolist (different-conversation-p '(nil t))
    (dolist (stage '(:registry :initialize :install))
      (application-tests--connection-preparation-failure
       ':reconnect stage :different-conversation-p different-conversation-p)))
  nil)

(-> test-application-task-presentation () null)
(defun test-application-task-presentation ()
  "Test task events drive one reconnectable child-agent terminal projection."
  (let* ((configuration            (test-configuration))
         (root                     (test-configuration-root configuration))
         (conversation
          (conversation-create configuration :identifier
                               "task-presentation-primary-window"))
         (registry                 (task-augment-tool-registry (make-default-tool-registry)))
         (run-tool                 (tool-registry-find registry "child" "spawn"))
         (orchestrator             (task-run-tool-orchestrator run-tool))
         (clock                    65)
         (terminal                 (make-instance 'recording-terminal :columns 72 :styled-p t))
         (ui                       (terminal-ui-create :terminal terminal :clock-function (lambda () clock)))
         (application
          (make-instance 'application :configuration configuration :conversation
                         conversation :tool-registry registry :ui ui))
         (primary
          (task-tests--primary-agent configuration "task-presentation-primary" registry))
         (definition               (list))
         (queued                   nil)
         (running                  nil)
         (replacement-registry     nil)
         (replacement-orchestrator nil))
    (unwind-protect
        (progn
         (dolist
             (arguments
              (list (json-object "task" "Default request")
                    (json-object "task" "Context request" "context"
                                 "Background context")))
           (let* ((item (first (task-normalize-arguments arguments)))
                  (call
                   (json-object "type" "function_call" "namespace"
                                (tool-namespace run-tool) "name" (tool-name run-tool)
                                "arguments" (json-encode arguments))))
             (test-assert (eq ':detached (getf item :mode))
                          "normalized requests describe detached execution")
             (dolist (restored-call (list call (json-decode (json-encode call))))
               (let ((text
                      (test-terminal-row-text
                       (response-item-entry application restored-call))))
                 (test-assert
                  (and (search "detached" text) (search (getf item :task) text)
                       (or (not (getf item :context))
                           (search (getf item :context) text)))
                  "live and restored task rows show normalized assignments and context")))))
         (dolist
             (arguments
              (list (json-object "task" "Invalid context" "context" 7)
                    (json-object "task" "Invalid policy" "completion-policy" "invalid")))
           (let ((call
                  (json-object "type" "function_call" "namespace"
                               (tool-namespace run-tool) "name" (tool-name run-tool)
                               "arguments" (json-encode arguments))))
             (dolist (restored-call (list call (json-decode (json-encode call))))
               (test-assert
                (search "invalid task request"
                        (test-terminal-row-text
                         (response-item-entry application restored-call)))
                "live and restored presentations use execution's request validation"))))
         (application-connect-task-presentation application)
         (application-connect-task-presentation application)
         (test-assert (= (length (task-orchestrator-listeners orchestrator)) 1)
                      "reconnection retains exactly one task presentation listener")
         (let ((stale-command
                (list :id "exec:stale" :type ':tool :index 1 :tool "shell.run"
                      :description "Old session command" :state ':running :duration-ms 0
                      :detached nil)))
           (terminal-ui-set-command-activities ui (list stale-command))
           (terminal-ui-set-command-activities ui nil)
           (test-assert (terminal-ui-command-pending-completions ui)
                        "a command completed before paint is pending presentation")
           (application-disconnect-task-presentation application)
           (test-assert
            (and (null (terminal-ui-command-activities ui))
                 (null (terminal-ui-command-unpainted-identifiers ui))
                 (null (terminal-ui-command-pending-completions ui)))
            "disconnect discards every old-session command presentation")
           (application-connect-task-presentation application)
           (recording-terminal-reset terminal)
           (terminal-ui-refresh-status ui)
           (test-assert (not (search "exec:stale" (recording-terminal-output terminal)))
                        "reconnect cannot paint a command retained by the old session"))
         (let* ((original-add-listener (symbol-function 'task-orchestrator-add-listener))
                (gate-lock             (make-lock "Autolith task presentation connect race"))
                (gate-condition        (make-condition-variable))
                (first-add-entered-p   nil)
                (release-first-add-p   nil)
                (second-started-p      nil)
                (add-call-count        0)
                (first-condition       nil)
                (second-condition      nil)
                (first-thread          nil)
                (second-thread         nil))
           (test-call-with-function-replacements
            (list
             (list 'task-orchestrator-add-listener
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
                                 unless (condition-wait gate-condition gate-lock :timeout
                                                        2)
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
                               (application-connect-task-presentation application)
                               (condition (condition) (setf first-condition condition))))
                            :name "Autolith first task presentation connect"))
                   (with-lock-held (gate-lock)
                     (loop until first-add-entered-p
                           unless (condition-wait gate-condition gate-lock :timeout 2)
                           do (error "Timed out waiting for the first listener add.")))
                   (setf second-thread
                           (make-thread
                            (lambda ()
                              (with-lock-held (gate-lock)
                                (setf second-started-p t)
                                (task--condition-broadcast gate-condition))
                              (handler-case
                               (application-connect-task-presentation application)
                               (condition (condition)
                                (setf second-condition condition))))
                            :name "Autolith second task presentation connect"))
                   (with-lock-held (gate-lock)
                     (loop until second-started-p
                           unless (condition-wait gate-condition gate-lock :timeout 2)
                           do (error "Timed out starting the second listener connect."))
                     (setf release-first-add-p t)
                     (task--condition-broadcast gate-condition))
                   (join-thread first-thread)
                   (setf first-thread nil)
                   (join-thread second-thread)
                   (setf second-thread nil)
                   (let ((listeners (task-orchestrator-listeners orchestrator)))
                     (test-assert
                      (and (null first-condition) (null second-condition)
                           (= add-call-count 2) (= (length listeners) 1)
                           (member (application-task-presentation-listener application)
                                   listeners :test #'eq))
                      "overlapping reconnects retain one exact active listener")))
                (with-lock-held (gate-lock)
                  (setf release-first-add-p t)
                  (task--condition-broadcast gate-condition))
                (when first-thread (ignore-errors (join-thread first-thread)))
                (when second-thread (ignore-errors (join-thread second-thread)))))))
         (setf queued
                 (task-tests--register-job orchestrator primary definition :name
                                           "queued-agent")
               running
                 (task-tests--register-job orchestrator primary definition :name
                                           "running-agent"))
         (with-lock-held ((cl-jobpond::job--lock running))
           (setf (job-state running) ':running)
           (task-job--set-progress-state running ':running))
         (task-progress-note-status running ':tool-call-started
                                    (list :tool "search.content"))
         (let* ((progress (task-job-progress running)) (now (get-internal-real-time)))
           (with-lock-held ((task-progress-lock progress))
             (setf (task-progress-started-at progress)
                   (- now (* 65 internal-time-units-per-second))
                   (task-progress-current-tool-started-at progress)
                   (- now (* 60 internal-time-units-per-second)))))
         (task-orchestrator-emit orchestrator ':task-subagent-progress
                                 (list :id (job-identifier running)))
         (let ((activities (terminal-ui-agent-activities ui)))
           (test-assert
            (and (= (length activities) 2) (eq (getf (first activities) :state) ':queued)
                 (eq (getf (second activities) :state) ':running)
                 (string= (getf (second activities) :current-tool) "search.content")
                 (typep (getf (second activities) :current-tool-duration-ms)
                        '(integer 60000))
                 (typep (getf (second activities) :duration-ms) '(integer 65000)))
            "one progress event projects every queued and running child"))
         (multiple-value-bind (text display cursor)
             (terminal-ui--live-content ui clock)
           (declare (ignore display cursor))
           (test-assert
            (and (search "running-agent" text) (search "queued-agent" text)
                 (search "search.content 01:00" text))
            "named jobs project their names and render runtime durations"))
         (task-tests--publish-terminal queued ':completed
                                       (task-tests--terminal-result queued :status
                                                                    ':success :output
                                                                    "queued complete"))
         (test-assert
          (equal
           (mapcar (lambda (activity) (getf activity :id))
                   (terminal-ui-agent-activities ui))
           (list (job-identifier running)))
          "terminal lifecycle events remove only their finished child")
         (task-tests--publish-terminal running ':completed
                                       (task-tests--terminal-result running :status
                                                                    ':success :output
                                                                    "running complete"))
         (task-orchestrator-emit orchestrator ':task-subagent-progress
                                 (list :id (job-identifier running)))
         (test-assert (null (terminal-ui-agent-activities ui))
                      "late progress cannot resurrect a terminal child")
         (let* ((timestamp      (encode-universal-time 0 53 22 10 8 2026))
                (payload
                 (list :id "child-1" :execution-id "execution-1" :child-name
                       "shared-diff-final-review" :steering-id "steering-1" :text
                       "Use careful context." :time timestamp))
                (entry          (application--child-response-entry application payload))
                (records-before
                 (copy-tree (application-tests--conversation-records conversation)))
                (stale-listener (application-task-presentation-listener application)))
           (test-assert
            (and
             (find (terminal-span ':child-name "shared-diff-final-review") entry :test
                   #'equal)
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
           (funcall stale-listener ':task-subagent-verbal-response payload)
           (test-assert (string= (recording-terminal-output terminal) "")
                        "a snapshotted stale listener cannot present after reconnection")
           (recording-terminal-reset terminal)
           (let ((cl-colorist:*color-level* ':indexed))
             (task-orchestrator-emit orchestrator ':task-subagent-verbal-response
                                     payload))
           (let* ((output            (recording-terminal-output terminal))
                  (plain-output      (cl-colorist:strip-ansi output))
                  (child-style       (terminal-style-sequence ':child-name t))
                  (basic-child-style (terminal-style-sequence ':child-name nil)))
             (test-assert
              (and
               (search "● autolith [shared-diff-final-review] 2026-08-10 22:53"
                       plain-output)
               (=
                (test-substring-count "● autolith [shared-diff-final-review]"
                                      plain-output)
                1)
               (search "Use careful context." plain-output) (search child-style output)
               (string= basic-child-style (terminal-style-sequence ':success nil)))
              "one child response is presented once with its configured style"))
           (test-assert
            (and
             (equal records-before
                    (application-tests--conversation-records conversation))
             (null (conversation-input-items conversation)))
            "promoted child presentation does not mutate primary conversation state")
           (application-disconnect-task-presentation application)
           (recording-terminal-reset terminal)
           (task-orchestrator-emit orchestrator ':task-subagent-verbal-response payload)
           (test-assert (string= (recording-terminal-output terminal) "")
                        "a disconnected orchestrator cannot present child responses")
           (setf replacement-registry
                   (task-augment-tool-registry (make-default-tool-registry))
                 replacement-orchestrator
                   (task-run-tool-orchestrator
                    (tool-registry-find replacement-registry "child" "spawn"))
                 (application-tool-registry application) replacement-registry)
           (application-connect-task-presentation application)
           (test-assert
            (and (zerop (length (task-orchestrator-listeners orchestrator)))
                 (= (length (task-orchestrator-listeners replacement-orchestrator)) 1))
            "runtime replacement moves the exact presentation listener")
           (recording-terminal-reset terminal)
           (task-orchestrator-emit orchestrator ':task-subagent-verbal-response payload)
           (test-assert (string= (recording-terminal-output terminal) "")
                        "the retired orchestrator cannot present after replacement")
           (let ((replacement-payload
                  (list :id "child-2" :execution-id "execution-2" :child-name
                        "replacement-review" :steering-id "steering-2" :text
                        "Replacement response." :time timestamp)))
             (let ((cl-colorist:*color-level* ':indexed))
               (task-orchestrator-emit replacement-orchestrator
                                       ':task-subagent-verbal-response
                                       replacement-payload))
             (let ((plain-output
                    (cl-colorist:strip-ansi (recording-terminal-output terminal))))
               (test-assert
                (and
                 (search "● autolith [replacement-review] 2026-08-10 22:53" plain-output)
                 (= (test-substring-count "● autolith [replacement-review]" plain-output)
                    1)
                 (not (search "shared-diff-final-review" plain-output)))
                "only the active orchestrator presents its current child response"))))
         (application-disconnect-task-presentation application)
         (test-assert
          (and (null (application-task-presentation-orchestrator application))
               (null (application-task-presentation-listener application))
               (or (null replacement-orchestrator)
                   (zerop
                    (length (task-orchestrator-listeners replacement-orchestrator)))))
          "disconnect removes the exact observer and retained scheduler link"))
      (application-disconnect-task-presentation application)
      (dolist (job (remove nil (list queued running)))
        (unless (job-terminal-p job)
          (task-tests--publish-terminal job ':aborted
                                        (task-tests--terminal-result job :status
                                                                     ':aborted :output
                                                                     "test cleanup"))))
      (when replacement-registry
        (ignore-errors (tool-registry-close-runtime-state replacement-registry)))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                      ':ignore)))
  nil)

(-> test-working-directory-command () null)
(defun test-working-directory-command ()
  "Test /cwd completion, status, and full-path parsing."
  (let* ((configuration              (test-configuration))
         (root                       (test-configuration-root configuration))
         (workspace                  (merge-pathnames "command workspace/" root))
         (lisp-workspace             (merge-pathnames "lisp command workspace/" root))
         (previous-process-directory (uiop:getcwd))
         (previous-defaults          *default-pathname-defaults*)
         (terminal                   (make-instance 'recording-terminal :columns 80))
         (ui                         (terminal-ui-create :terminal terminal))
         (registry                   (make-default-tool-registry)))
    (ensure-directories-exist workspace)
    (ensure-directories-exist lisp-workspace)
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "cwd-command"))
                (provider (provider-create configuration))
                (agent    (agent-create :configuration configuration
                                        :provider      provider
                                        :conversation  conversation
                                        :tool-registry registry
                                        :worker        nil))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation  conversation
                                 :provider      provider
                                 :tool-registry registry
                                 :worker        nil
                                 :agent         agent
                                 :ui            ui)))
           (let ((received nil))
             (test-call-with-function-replacements
              (list (list 'application-set-working-directory
                           (lambda (target location)
                            (test-assert (eq target application) "CWD uses the current application")
                            (setf received location
                                  (application-configuration target)
                                  (configuration-copy (application-configuration target)
                                                      :working-directory location))
                            (config :working-directory (application-configuration target)))))
              (lambda ()
                (application-operation-call application "cwd" lisp-workspace)
                (test-assert (and (pathnamep received) (equal received lisp-workspace))
                             "a native workspace pathname reaches workspace switching intact")
                (setf received nil)
                (test-assert
                 (handler-case (progn (application-operation-call application "cwd" "") nil)
                   (configuration-error () t))
                 "an explicit empty workspace pathname is rejected")
                (test-assert (null received) "invalid workspace input cannot switch the workspace"))))
           (terminal-ui-start ui)
           (test-assert (application-operation-find application "cwd")
                        "the workspace operation has a canonical local name")
           (test-assert (equal (application-command-invocation-arguments
                                  (application-operation--command-invocation
                                   (application-command-find "cwd") '("directory name with spaces")))
                               '("directory name with spaces"))
                        "native command arguments preserve embedded spaces")
           (test-assert
            (eq (application-run-lisp-input
                 application
                 (format nil "(cwd ~S)" (namestring workspace)))
                ':continue)
            "(cwd pathname) continues the application loop")
           (test-assert
            (equal (config :working-directory
                    (application-configuration application))
                   (truename workspace))
            "(cwd pathname) passes the complete path to workspace switching")
           (test-assert
            (search (format nil "working directory is now ~A"
                            (namestring (truename workspace)))
                    (recording-terminal-output terminal))
            "(cwd pathname) presents the selected workspace")
           (test-assert
            (eq (application-run-lisp-input application "(cwd)") ':continue)
            "(cwd) reports the workspace")
           (test-assert
            (null (application-operation-call application "cwd"))
            "(cwd) without a path reports status")
           (test-assert
            (search (format nil "working directory: ~A"
                            (namestring (truename workspace)))
                    (recording-terminal-output terminal))
            "omitted native paths preserve the status operation")
           (let ((evaluation
                   (application-lisp-evaluate
                    (format nil "(cwd #P~S)" (namestring lisp-workspace))
                    :application application)))
             (test-assert
              (eq (application-lisp-evaluation-status evaluation) ':ok)
              "a canonical CWD call accepts a pathname object")
             (test-assert
              (equal (config :working-directory
                      (application-configuration application))
                     (truename lisp-workspace))
              "a canonical CWD pathname switches workspaces")))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:chdir previous-process-directory)
      (setf *default-pathname-defaults* previous-defaults)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-effort-switch () null)
(defun test-effort-switch ()
  "Test in-place model and reasoning-effort configuration switching."
  (let* ((base          (test-configuration))
         (configuration (configuration-copy base :web-search-mode ':live))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "effort"))
                (provider     (provider-create configuration))
                (registry     (make-default-tool-registry))
                (worker       (lisp-worker-create configuration))
                (agent        (agent-create :configuration configuration
                                            :provider      provider
                                            :conversation  conversation
                                            :tool-registry registry
                                            :worker        worker))
                (terminal     (make-instance 'scripted-terminal :columns 60))
                (ui           (terminal-ui-create :terminal terminal))
                (application
                  (make-instance 'application
                                 :configuration configuration
                                 :conversation  conversation
                                 :provider      provider
                                 :tool-registry registry
                                 :worker        worker
                                 :agent         agent
                                 :ui            ui)))
           (setf (provider-rate-limits provider) '(:primary (:used-percent 25)))
           (application-set-reasoning-effort application "low")
           (test-assert (string= (config :reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "switching effort replaces the configuration")
           (test-assert
            (string= (conversation-reasoning-effort conversation) "low")
            "switching effort updates the active conversation")
           (let ((preferences (preferences-load-values configuration)))
             (test-assert
              (string= (getf preferences :reasoning-effort) "low")
              "switching effort saves the global effort default")
             (test-assert
              (string= (getf preferences :model) *default-model*)
              "switching effort saves the accompanying model default"))
           (let ((updated (application-configuration application)))
             (test-assert (equal (config :source-root updated)
                                 (config :source-root configuration))
                          "effort switching preserves the source root")
             (test-assert (equal (config :state-root updated)
                                 (config :state-root configuration))
                          "effort switching preserves private state paths")
             (test-assert (string= (config :provider-endpoint updated)
                                   (config :provider-endpoint configuration))
                          "effort switching preserves the provider endpoint")
             (test-assert (eq (config :web-search-mode updated) ':live)
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
           (let* ((items  (application--model-items application))
                  (models (provider-registration-models
                           (provider-registration-find "chatgpt"))))
             (test-assert
              (and (= (length items) (length models))
                   (null (set-exclusive-or
                          (mapcar (lambda (item) (getf item :model)) items)
                          (mapcar #'provider-model-name models)
                          :test #'string=)))
              "every configured ChatGPT model is offered")
             (test-assert (string= (getf (find t items
                                               :key (lambda (item) (getf item :current-p)))
                                         :model)
                                   *default-model*)
                          "the active model is marked current"))
           (application-set-model application "gpt-5.6-terra")
           (test-assert (string= (config :model
                                  (application-configuration application))
                                 "gpt-5.6-terra")
                        "switching the model replaces the configuration")
           (test-assert (string= (config :reasoning-effort
                                  (application-configuration application))
                                 "low")
                        "model switching preserves the reasoning effort")
           (test-assert (string= (conversation-model conversation)
                                "gpt-5.6-terra")
                        "model switching updates the active conversation")
           (let ((preferences (preferences-load-values configuration)))
             (test-assert
              (string= (getf preferences :model) "gpt-5.6-terra")
              "switching models saves the global model default")
             (test-assert
              (string= (getf preferences :reasoning-effort) "low")
              "switching models preserves the global effort default"))
           (let ((model-installed-before-effort-p nil))
             (setf (scripted-terminal-events terminal)
                   (list :history-next :submit)
                   (slot-value terminal 'read-callback)
                   (lambda ()
                     (setf model-installed-before-effort-p
                           (or model-installed-before-effort-p
                               (string=
                                (config :model
                                 (application-configuration application))
                                "gpt-5.6-luna")))))
             (unwind-protect
                  (with-terminal-ui (active-ui ui)
                    (test-assert
                     (eq (application-run-lisp-input application
                                              "(model \"gpt-5.6-luna\")")
                         ':continue)
                     "an explicit model change continues after choosing its effort"))
               (setf (slot-value terminal 'read-callback) nil))
             (test-assert
              model-installed-before-effort-p
              "model commands install the selection before prompting for effort"))
           (test-assert
            (and (string= (config :model
                           (application-configuration application))
                          "gpt-5.6-luna")
                 (string= (config :reasoning-effort
                           (application-configuration application))
                          "medium"))
            "model commands apply the prompted effort after switching models")
           (test-assert
            (and (string= (conversation-model conversation) "gpt-5.6-luna")
                 (string= (conversation-reasoning-effort conversation) "medium"))
            "model commands persist both choices in the active conversation")
           (let ((preferences (preferences-load-values configuration)))
             (test-assert
              (and (string= (getf preferences :model)
                            "gpt-5.6-luna")
                   (string= (getf preferences :reasoning-effort)
                            "medium"))
              "model commands persist both choices as global defaults"))
           (test-assert
            (search "chatgpt/gpt-5.6-luna with reasoning effort medium"
                    (recording-terminal-output terminal))
            "model commands report the complete selection")
           (test-assert (handler-case
                            (progn
                              (application-set-model application "gpt-4")
                              nil)
                          (model-selection-error ()
                            t))
                        "unsupported models are rejected with the choices")
           (conversation-append-user-message conversation "persist this choice")
           (let* ((resumed-configuration
                    (configuration-copy
                     (configuration-copy configuration :model "gpt-5.6-luna") :reasoning-effort
                     "xhigh"))
                  (resumed (conversation-create resumed-configuration
                                                :identifier "resumed-model")))
             (conversation-append-user-message resumed "use the saved choice")
             (application-install-conversation
              application
              (conversation-load-by-id configuration "resumed-model"))
             (test-assert
              (string= (config :model
                        (application-configuration application))
                       "gpt-5.6-luna")
              "resuming restores the conversation model")
             (test-assert
              (string= (config :reasoning-effort
                        (application-configuration application))
                       "xhigh")
              "resuming restores the conversation effort")
             (test-assert
              (string= (config :model
                        (provider-configuration
                         (application-provider application)))
                       "gpt-5.6-luna")
              "the reconnected provider uses the conversation model"))
           (let ((preferences (preferences-load-values configuration)))
             (test-assert
              (string= (getf preferences :model) "gpt-5.6-luna")
              "resuming does not replace the global model default")
             (test-assert
              (string= (getf preferences :reasoning-effort) "medium")
              "resuming does not replace the global effort default")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-pending-input-persistence () null)
(defun test-pending-input-persistence ()
  "Test active, steering, recalled, and legacy pending input survives safely."
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (terminal                (make-instance 'waiting-recording-terminal :columns 60))
         (ui                      (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "pending-inputs"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (controller              nil)
         (restored                nil)
         (restored-after-append   nil)
         (restored-after-shutdown nil)
         (legacy-controller       nil)
         (snapshot-identifier     nil))
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
                                   :conversation  legacy-conversation
                                   :ui            ui))
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
                    :steering        '("legacy steering")
                    :work            '((:message "legacy follow-up"))))
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
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-hurry-up-mode () null)
(defun test-hurry-up-mode ()
  "Test urgent prompt selection, notices, and hard child-agent limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "hurry-up"))
         (registry      (task-augment-tool-registry (make-default-tool-registry)))
         (agent         (agent-create :configuration configuration
                                      :conversation  conversation
                                      :tool-registry registry
                                      :worker        ':unused))
         (terminal      (make-instance 'recording-terminal :columns 100))
         (ui            (terminal-ui-create :terminal terminal))
         (application   (make-instance 'application
                                       :configuration configuration
                                       :conversation  conversation
                                       :provider      (agent-provider agent)
                                       :tool-registry registry
                                       :agent         agent
                                       :worker        ':unused
                                       :ui            ui))
         (orchestrator  (application--task-orchestrator application)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (application-run-lisp-input application "(hurry-up)")
           (test-assert
            (and (not (application-hurry-up-p application))
                 (search "hurry-up mode is disabled"
                         (recording-terminal-output terminal)))
            "(hurry-up) reports status without changing policy")
           (application-run-lisp-input application "(hurry-up t)")
           (test-assert (and (application-hurry-up-p application)
                             (agent-hurry-up-p agent)
                             (task-orchestrator-hurry-up-p orchestrator))
                        "(hurry-up t) synchronizes application, agent, and task state")
           (test-assert (= (task-orchestrator-maximum-concurrency orchestrator) 2)
                        "hurry-up caps child concurrency at two")
           (test-assert (terminal-ui-notice ui)
                        "hurry-up presents a transient live notice")
           (application-set-model application "gpt-5.6-luna")
           (test-assert
            (and (application-hurry-up-p application)
                 (agent-hurry-up-p (application-agent application)))
            "configuration replacement preserves hurry-up mode")
           (application-run-lisp-input application "(hurry-up nil)")
           (test-assert
            (and (not (application-hurry-up-p application))
                 (not (agent-hurry-up-p (application-agent application)))
                 (not (task-orchestrator-hurry-up-p orchestrator)))
            "(hurry-up nil) restores ordinary policy"))
      (ignore-errors (terminal-ui-stop ui))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-protocol-failure-is-recoverable () null)
(defun test-provider-protocol-failure-is-recoverable ()
  "Test provider protocol failures do not enter fatal application recovery."
  (let* ((configuration     (test-configuration))
         (root              (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "recoverable-provider-protocol"))
         (registry          (make-default-tool-registry))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry registry
                          :ui            (terminal-ui-create :terminal (make-instance 'recording-terminal))))
         (handled-condition nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'application-run-message
            (lambda (active-application input &rest arguments)
              (declare (ignore active-application input arguments))
              (error 'provider-protocol-error
                     :message     "Compaction produced no summary text."
                     :status      nil
                     :code        nil
                     :request-id  nil
                     :response-id "compact-empty"
                     :response    nil)))
           (list 'application-compact
                 (lambda (active-application)
                   (application-run-message active-application "compact")))
           (list
            'application-handle-expected-error
            (lambda (active-application condition)
              (declare (ignore active-application))
              (setf handled-condition condition))))
          (lambda ()
            (test-assert
             (eq (application--run-message-input application "new question")
                 ':failed)
             "provider protocol failures return the ordinary failed action")
            (test-assert
             (and (typep handled-condition 'provider-protocol-error)
                  (string= (provider-error-response-id handled-condition)
                           "compact-empty"))
             "provider protocol failures use expected error presentation")
            (test-assert
             (member (application-run-lisp-input application "(compact)" :interactive-p nil)
                     '(:aborted :failed))
             "provider failures from native commands return through local evaluation")))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-failed-turn-publishes-durable-wreckage () null)
(defun test-failed-turn-publishes-durable-wreckage ()
  "Test a failed model turn repairs calls and publishes its recoverable prefix."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "failed-turn-wreckage"))
         (registry      (make-default-tool-registry))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry registry)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'application-run-message
            (lambda (active-application input &rest arguments)
              (declare (ignore arguments))
              (conversation-append-user-message
               (application-conversation active-application) input)
              (conversation-append-provider-item
               (application-conversation active-application)
               (json-object
                "type" "function_call"
                "call_id" "wreckage-call"
                "namespace" "fs"
                "name" "read"
                "arguments" "{}"))
              (error 'agent-loop-error
                     :message        "provider response was malformed"
                     :conversation-id
                     (conversation-identifier
                      (application-conversation active-application))
                     :request-number 2)))
           (list
            'application-handle-expected-error
            (lambda (active-application condition)
              (declare (ignore active-application condition))
              nil)))
          (lambda ()
            (test-assert
             (eq
              (application--run-message-input
               application "partial request" :fatal-agent-loop-errors-p nil)
              ':failed)
             "an expected agent-loop failure returns the ordinary failed action")
            (let* ((records     (test-conversation--all-records conversation))
                   (tool-result (find :tool-result records :key #'first))
                   (marker      (find :turn-aborted records :key #'first)))
              (test-assert
               (and tool-result
                    marker
                    (eq (getf (rest tool-result) :status) ':error)
                    (string= (getf (rest tool-result) :output)
                             *conversation-interrupted-tool-output*)
                    (= (getf (rest marker) :last-complete-seq)
                       (getf (rest tool-result) :seq))
                    (eq (getf (rest marker) :reason) ':agent-loop)
                    (= (getf (rest marker) :request-number) 2))
               "failed turns repair incomplete calls before publishing their boundary"))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> application-tests--pending-publication-durability (application-input-controller) null)
(defun application-tests--pending-publication-durability (controller)
  "Test stale publication after snapshot and vault failures, followed by recovery."
  (labels ((capture (text sync-vault-p)
             (with-lock-held ((application-input-controller-lock controller))
               (deque-clear (application-input-controller-work-items controller))
               (deque-append (application-input-controller-work-items controller)
                             (list (list ':message text)))
               (application-input-controller--capture-pending-publication-locked
                controller
                (application-input-controller--next-publication-generation-locked controller)
                sync-vault-p))))
    (dolist (failure-kind '(:snapshot-write :vault-sync))
      (let* ((durable-before (application-input-controller-durable-pending-generation controller))
             (older          (capture "prepared state" nil))
             (newer          (capture "newer state" (eq failure-kind ':vault-sync)))
             (writer         (symbol-function 'snapshot-write))
             (writes         0)
             (fail-p         t))
        (test-call-with-function-replacements
         (list
          (list 'snapshot-write
                (lambda (&rest arguments)
                  (incf writes)
                  (when (and fail-p (eq failure-kind ':snapshot-write))
                    (error 'configuration-error :message "Injected pending snapshot failure."))
                  (apply writer arguments)))
          (list 'application-recovery-input-vault--sync-pending-locked
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error 'configuration-error :message "Injected pending vault failure."))))
         (lambda ()
           (test-assert
            (null (application-input-controller--publish-pending-publication controller newer nil))
            "a failed newer publication reports failure")
           (test-assert
            (and (= (application-input-controller-published-pending-generation controller)
                    (getf newer :generation))
                 (= (application-input-controller-durable-pending-generation controller)
                    durable-before))
            "a partial publication advances only the attempted frontier")
           (test-assert
            (null (application-input-controller--publish-pending-publication controller older nil))
            "an uncovered stale non-strict publication reports failure")
           (test-assert
            (handler-case
                (progn
                  (application-input-controller--publish-pending-publication controller older t)
                  nil)
              (configuration-error ()
                t))
            "an uncovered stale strict publication signals a typed failure")
           (test-assert (= writes 1) "uncovered stale state is never rewritten")
           (setf fail-p nil)
           (let ((fresh (capture "fresh durable state" nil)))
             (test-assert
              (application-input-controller--publish-pending-publication controller fresh t)
              "a fresh generation can publish after a partial failure")
             (test-assert
              (= (application-input-controller-durable-pending-generation controller)
                 (getf fresh :generation))
              "successful publication advances the durable frontier")
             (test-assert
              (and (application-input-controller--publish-pending-publication controller older t)
                   (application-input-controller--publish-pending-publication controller newer t)
                   (= writes 2))
              "covered stale publications succeed without rewriting state")
             (multiple-value-bind (form complete-p) (snapshot-read (getf fresh :pathname))
               (test-assert
                (and complete-p (equal (getf (rest form) :work)
                                       '((:message "fresh durable state"))))
                "the fresh snapshot survives delayed stale publications")))))))
    (with-lock-held ((application-input-controller-lock controller))
      (deque-clear (application-input-controller-work-items controller))))
  nil)

(-> test-pending-publication-lock-boundaries () null)
(defun test-pending-publication-lock-boundaries ()
  "Test external publication releases the controller lock and rejects stale state."
  (let* ((configuration   (test-configuration))
         (root            (test-configuration-root configuration))
         (terminal        (make-instance 'waiting-recording-terminal :columns 80))
         (ui              (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "pending-publication-locks"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (controller      nil)
         (gate            (make-lock "Pending publication test gate"))
         (condition       (make-condition-variable :name "Pending publication test gate"))
         (entered-p       nil)
         (released-p      nil)
         (publisher       nil)
         (original-setter (symbol-function 'terminal-ui-set-pending-inputs))
         (original-publisher
           (symbol-function 'application-input-controller--publish-pending-publication)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil :start-reader-p nil))
           (application-tests--pending-publication-durability controller)
           (with-lock-held ((application-input-controller-lock controller))
             (deque-append
              (application-input-controller-work-items controller)
              '((:message "blocked publication"))))
           (setf (symbol-function 'terminal-ui-set-pending-inputs)
                 (lambda (target-ui steering queued)
                   (when (eq target-ui ui)
                     (with-lock-held (gate)
                       (setf entered-p t)
                       (condition-notify condition)
                       (loop until released-p
                             do (condition-wait condition gate))))
                   (funcall original-setter target-ui steering queued))
                 publisher
                 (make-thread
                  (lambda ()
                    (application-input-controller--publish-counts controller))
                  :name "Blocked pending publication"))
           (with-lock-held (gate)
             (loop until entered-p
                   unless (condition-wait condition gate :timeout 2)
                     do (error "Timed out waiting for pending publication.")))
           (let ((available-p
                   (sb-thread:grab-mutex
                    (application-input-controller-lock controller) :waitp nil)))
             (unwind-protect
                  (test-assert available-p
                               "blocked external publication releases the controller lock")
               (when available-p
                 (sb-thread:release-mutex
                  (application-input-controller-lock controller)))))
           (with-lock-held (gate)
             (setf released-p t)
             (condition-notify condition))
           (join-thread publisher)
           (setf publisher nil
                 (symbol-function 'terminal-ui-set-pending-inputs) original-setter)
           (let ((old-publication
                   (with-lock-held ((application-input-controller-lock controller))
                     (application-input-controller--capture-pending-publication-locked
                      controller
                      (application-input-controller--next-publication-generation-locked
                       controller)
                      nil))))
             (setf entered-p nil
                   released-p nil
                   (symbol-function
                    'application-input-controller--publish-pending-publication)
                   (lambda (writer-controller publication error-p)
                     (when (eq publication old-publication)
                       (with-lock-held (gate)
                         (setf entered-p t)
                         (condition-notify condition)
                         (loop until released-p
                               do (condition-wait condition gate))))
                     (funcall original-publisher
                              writer-controller publication error-p))
                   publisher
                   (make-thread
                    (lambda ()
                      (application-input-controller--publish-pending-publication
                       controller old-publication t))
                    :name "Delayed stale pending publication"))
             (with-lock-held (gate)
               (loop until entered-p
                     unless (condition-wait condition gate :timeout 2)
                       do (error "Timed out waiting for stale publication.")))
             (with-lock-held ((application-input-controller-lock controller))
               (deque-clear (application-input-controller-work-items controller))
               (deque-append
                (application-input-controller-work-items controller)
                '((:message "final shutdown"))))
             (application-input-controller--prepare-shutdown controller ':interrupt)
             (with-lock-held (gate)
               (setf released-p t)
               (condition-notify condition))
             (join-thread publisher)
             (setf publisher nil)
             (multiple-value-bind (form complete-p)
                 (snapshot-read
                  (configuration-pending-inputs-path
                   configuration (conversation-pathname conversation)))
               (test-assert
                (and complete-p
                     (equal (getf (rest form) :work)
                            '((:message "final shutdown"))))
                "an older delayed publication cannot replace the shutdown snapshot"))))
      (setf (symbol-function 'terminal-ui-set-pending-inputs) original-setter
            (symbol-function 'application-input-controller--publish-pending-publication)
            original-publisher)
      (when publisher
        (ignore-errors
          (with-lock-held (gate)
            (setf released-p t)
            (condition-notify condition)))
        (ignore-errors (join-thread publisher)))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-application-git-branch () null)
(defun test-application-git-branch ()
  "Test branch discovery without spawning Git subprocesses."
  (let* ((root
           (merge-pathnames
            (format nil "autolith-git-branch-~A/" (make-identifier))
            (uiop:temporary-directory)))
         (repository      (merge-pathnames "repository/" root))
         (nested          (merge-pathnames "src/deep/" repository))
         (head            (merge-pathnames ".git/HEAD" repository))
         (worktree        (merge-pathnames "worktree/" root))
         (worktree-nested (merge-pathnames "src/" worktree))
         (worktree-marker (merge-pathnames ".git" worktree))
         (worktree-head   (merge-pathnames "metadata/worktrees/example/HEAD" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist head)
           (ensure-directories-exist (merge-pathnames "placeholder" nested))
           (with-open-file (stream head
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create)
             (write-line "ref: refs/heads/feature/status" stream))
           (test-assert
            (string= (application--git-branch nested) "feature/status")
            "branch discovery reads the enclosing worktree HEAD directly")
           (with-open-file (stream head
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create)
             (write-line "892e223b0b7a2d7143787b7c9e4d7ecee644a83c" stream))
           (test-assert
            (null (application--git-branch nested))
            "detached worktrees omit branch status")
           (ensure-directories-exist worktree-marker)
           (ensure-directories-exist worktree-head)
           (ensure-directories-exist (merge-pathnames "placeholder" worktree-nested))
           (with-open-file (stream worktree-marker
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create)
             (write-line "gitdir: ../metadata/worktrees/example/" stream))
           (with-open-file (stream worktree-head
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create)
             (write-line "ref: refs/heads/worktree-branch" stream))
           (test-assert
            (string= (application--git-branch worktree-nested) "worktree-branch")
            "branch discovery follows linked-worktree gitdir markers"))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)


;;;; -- Transcript Actions --

(-> test-application-transcript-actions () null)
(defun test-application-transcript-actions ()
  "Test clicked copy and link actions reach the clipboard, the terminal, and the browser."
  (let* ((terminal    (make-instance 'recording-terminal :columns 60 :rows 12 :styled-p t))
         (ui          (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (text        (format nil "alpha~%beta")))
    (with-terminal-ui (active ui)
      (test-assert (terminal-ui-action-function active)
                   "creating an application connects transcript actions")
      (let ((sophisticated-clipboard:*clipboard-backend*
              (make-instance 'sophisticated-clipboard:terminal-backend :writer (constantly t))))
        (funcall (terminal-ui-action-function active) (list ':copy text)))
      (test-assert (some (lambda (chunk)
                           (search (sophisticated-clipboard:terminal-clipboard-sequence text) chunk))
                         (recording-terminal-chunks terminal))
                   "copying also sends the OSC 52 request to the terminal")
      (test-assert (equal "· copied 2 lines to the clipboard" (terminal-ui-notice active))
                   "a successful copy is announced")
      (let ((sophisticated-clipboard:*clipboard-backend*
              (make-instance 'sophisticated-clipboard:terminal-backend :writer (constantly nil))))
        (terminal-ui-transcript-action (application-ui application) (list ':copy "solo")))
      (test-assert (uiop:string-prefix-p "· sent 1 line to the terminal's clipboard"
                                         (terminal-ui-notice active))
                   "a host clipboard failure still reports the terminal request")
      (let ((opened nil))
        (test-call-with-function-replacements
         (list
          (list 'platform-open-url
                (lambda (platform url)
                  (declare (ignore platform))
                  (push url opened)
                  t)))
         (lambda ()
           (terminal-ui-transcript-action (application-ui application) '(:open-url "https://example.com/x"))
           (terminal-ui-transcript-action (application-ui application) '(:open-url "javascript:alert(1)"))))
        (test-assert (equal '("https://example.com/x") opened)
                     "only web URLs reach the browser launcher")
        (test-assert (search "only http and https" (terminal-ui-notice active))
                     "refused links explain themselves"))
      (let* ((renderer (terminal-markdown-renderer 60))
             (rows     (termdown:markdown-render-source renderer (format nil "```lisp~%(+ 1 2)~%```")))
             (widget   (some (lambda (row) (find-if #'termdown:widget-p row)) rows)))
        (test-assert (and widget
                          (equal '(:copy "(+ 1 2)") (termdown:widget-action widget))
                          (eq ':code-copy (termdown:widget-role widget)))
                     "a closing fence renders a widget copying its source"))
      (test-assert (find-if #'termdown:widget-p
                            (application--markdown-body
                             application
                             (format nil "intro~%```sh~%ls~%```~%outro")))
                   "finalized markdown bodies carry copy widgets")))
  nil)

(-> test-operation-request-turn () null)
(defun test-operation-request-turn ()
  "Test an operation's generated request reaches the model but shows as its notice."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration))
           (terminal     (make-instance 'recording-terminal :columns 100))
           (ui           (terminal-ui-create :terminal terminal))
           (provider     (make-instance 'scripted-provider
                                        :results (list (agent-test-result
                                                        "repair"
                                                        (list (agent-test-message "Dropped."))
                                                        :turn-completion ':end))))
           (registry     (make-instance 'tool-registry))
           (agent        (agent-create :configuration configuration :provider provider
                                       :conversation conversation :tool-registry registry
                                       :worker t))
           (application  (make-instance 'application
                                        :configuration configuration
                                        :conversation  conversation
                                        :provider      provider
                                        :tool-registry registry
                                        :worker        t
                                        :agent         agent
                                        :ui            ui))
           (text         "Generated repair instructions the user never typed.")
           (request      (list :operation "fix-skipped-definitions"
                               :summary   "Rebuild or drop the 1 private definition skipped at startup.")))
      (conversation-append-user-message conversation "typed by the user")
      (with-terminal-ui (active-ui ui)
        (recording-terminal-reset terminal)
        (application--run-turn application text :operation-request request)
        (let ((output (recording-terminal-output terminal))
              (record (find-if (lambda (record)
                                 (getf (rest record) :operation-request))
                               (conversation--read-records
                                (conversation-pathname conversation)))))
          (test-assert (some (lambda (item) (search text (json-encode item)))
                             (conversation-input-items conversation))
                       "the model receives the generated text")
          (test-assert (and (search "▸ fix-skipped-definitions" output)
                            (search "Rebuild or drop the 1 private definition" output)
                            (not (search text output)))
                       "the live transcript shows the request notice, not its text")
          (test-assert (and record (getf (rest record) :automatic-p))
                       "the request is stored as an automatic message with its metadata")
          (let ((shown (terminal--spans-text (conversation-record-entry application record))))
            (test-assert (and (search "▸ fix-skipped-definitions" shown)
                              (not (search text shown)))
                         "a rendered transcript shows the same notice"))))
      (test-assert (equal (conversation-input-history conversation) '("typed by the user"))
                   "input history keeps only what the user typed")))
  nil)

(-> test-application-job-program-status () null)
(defun test-application-job-program-status ()
  "Test that root program status uses the current conversation title."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "status-owner"))
           (terminal     (make-instance 'scripted-terminal))
           (ui           (terminal-ui-create :terminal terminal))
           (application  (make-instance 'application :configuration configuration
                                        :conversation conversation :ui ui)))
      (application-connect-program-status application)
      (with-terminal-ui (active ui)
        (let ((*active-application* application))
          (dolist (title '("First title" "Changed title"))
            (setf (conversation-title conversation) title)
            (recording-terminal-reset terminal)
            (terminal-ui-report-program-status active ':working)
            (test-assert
             (search (format nil ":title=~A" (clinedi:program-status-text title 192))
                     (recording-terminal-output terminal))
             "each root report reads the current conversation title"))))))
  nil)

(-> test-provider-exhaustion-presentation () null)
(defun test-provider-exhaustion-presentation ()
  "Test exhausted retries present the count and provider reason immediately."
  (with-test-configuration (configuration)
    (let* ((terminal    (make-instance 'recording-terminal :columns 100))
           (application
             (make-instance 'application
                            :configuration configuration
                            :conversation  (conversation-create configuration :identifier "exhaustion")
                            :ui            (terminal-ui-create :terminal terminal)))
           (observer    (application-agent-observer application))
           (send-status (callback-agent-observer-status-callback observer)))
      (unwind-protect
           (progn
             (terminal-ui-start (application-ui application))
             (funcall send-status :provider-request-started nil)
             (recording-terminal-reset terminal)
             (funcall send-status :provider-retries-exhausted
                      (list :attempts 7 :code "service_unavailable_error"))
             (let ((output (recording-terminal-output terminal)))
               (test-assert (and (search "7" output)
                                 (search "service_unavailable_error" output))
                            "retry exhaustion visibly includes attempts and reason"))
             (recording-terminal-reset terminal)
             (funcall send-status :provider-request-started nil)
             (test-assert (not (search "service_unavailable_error"
                                      (recording-terminal-output terminal)))
                          "a later request does not inherit an exhaustion notice"))
        (terminal-ui-stop (application-ui application)))))
  nil)
