(in-package #:autolith)

;;;; -- Application Onboarding Boundaries --

(-> onboarding-tests--call (function &key (:fullscreen-p boolean) (:start-reader-p boolean)) t)
(defun onboarding-tests--call (function &key fullscreen-p start-reader-p)
  "Call FUNCTION with an isolated application, controller, configuration, and terminal."
  (with-test-configuration (configuration root)
    (setf configuration
          (configuration-copy configuration :working-directory root :onboarding-p t
                              :fullscreen-p fullscreen-p :boot-screen-seconds 0
                              :boot-screen-tip-seconds 1))
    (let* ((*terminal-theme*       *terminal-theme*)
           (*terminal-style-table* *terminal-style-table*)
           (terminal               (make-instance 'queued-recording-terminal :columns 100 :rows 32))
           (ui                     (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
           (application
             (make-instance 'application :configuration configuration
                            :conversation               (conversation-create configuration :identifier "onboarding-test")
                            :provider nil :worker nil :agent nil
                            :tool-registry (make-default-tool-registry) :ui ui)))
      (with-terminal-ui (active-ui ui)
        (let ((controller (application-input-controller-create
                           application :start-reader-p start-reader-p :load-pending-p nil)))
          (unwind-protect
               (funcall function application controller configuration terminal)
            (application-input-controller-stop controller)))))))

(-> onboarding-tests--pages () list)
(defun onboarding-tests--pages ()
  "Return a small independently versioned catalog for application boundary tests."
  (list (list :id "first" :version 1 :title "First" :body "First page")
        (list :id "second" :version 2 :title "Second" :body "Second page")
        (list :id "third" :version 1 :title "Third" :body "Third page")))

(-> test-onboarding-operation () null)
(defun test-onboarding-operation ()
  "Resolve and invoke the replay command through the real operation boundary."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore controller configuration terminal))
     (let* ((operation (application-operation-find application 'onboard))
            (command   (application-operation-backend operation))
            (called    nil))
       (test-assert (typep operation 'application-command-operation) "onboard resolves to a command operation")
       (test-assert (eq ':hold (application-command-busy-behavior command)) "replay waits for active work")
       (test-assert (eq ':exclusive (application-command-terminal-behavior command)) "replay owns terminal input exclusively")
       (test-call-with-function-replacements
        (list (list 'application-onboard
                    (lambda (target &key new-only-p)
                      (test-assert (eq target application) "operation invokes the current application")
                      (test-assert (not new-only-p) "manual replay includes acknowledged lessons")
                      (setf called t)
                      ':completed)))
        (lambda () (application-operation-invoke operation application nil)))
       (test-assert called "operation invocation reaches onboarding"))))
  nil)

(-> test-onboarding-first-startup () null)
(defun test-onboarding-first-startup ()
  "Present first-run onboarding in fullscreen even when inline is the saved preference."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore terminal))
     (let ((presented  nil)
           (boot-count 0)
           (ui         (application-ui application)))
       (test-assert (not (config :fullscreen-p configuration)) "fixture begins with an inline preference")
       (test-call-with-function-replacements
        (list (list 'terminal-ui-boot-sequence
                    (lambda (target &rest options)
                      (declare (ignore options))
                      (incf boot-count)
                      (test-assert (terminal-ui-fullscreen-p target) "first-run boot uses fullscreen")
                      ':completed))
              (list 'terminal-ui-onboard
                    (lambda (target pages &key help-function settings-function authentication-function resize-callback
                                              choose-function page-completed-function introduction-title)
                      (declare (ignore introduction-title))
                      (test-assert (functionp choose-function) "tutorial can apply a chosen setting")
                      (test-assert (fullscreen-terminal-ui-active-p target) "lesson presentation owns the alternate buffer")
                      (test-assert (non-empty-string-p (funcall help-function)) "tutorial receives real basic operation help")
                      (dolist (topic '(:workspace "resume" resume "not-a-help-topic"))
                        (test-assert (string= (funcall help-function topic)
                                              (application-operation-help application topic))
                                     "tutorial forwards native topics to the real help implementation"))
                      (test-assert (functionp settings-function) "tutorial can open real settings")
                      (test-assert (functionp authentication-function) "tutorial can open real authentication")
                      (test-assert (eq resize-callback #'application-pending-terminal-size)
                                   "tutorial follows the application's terminal resize source")
                      (setf presented (mapcar (lambda (page) (getf page :id)) pages))
                      (dolist (page pages) (funcall page-completed-function page))
                      (values ':completed presented))))
        (lambda ()
          (test-assert (eq ':completed (application-startup-presentation application controller))
                       "first startup completes onboarding")))
       (test-assert (= 1 boot-count) "onboarding includes one boot presentation")
       (test-assert (equal presented (mapcar (lambda (page) (getf page :id)) (onboarding-pages configuration)))
                    "first startup presents every current lesson in catalog order")
       (test-assert (null (onboarding-state-pending configuration (onboarding-pages configuration)))
                    "completed lesson callbacks acknowledge the tour")
       (test-assert (not (terminal-ui-fullscreen-p ui)) "first-run presentation restores inline mode")
       (test-assert (not (config :fullscreen-p configuration)) "onboarding does not overwrite fullscreen preference"))))
  nil)

(-> test-onboarding-startup-input () null)
(defun test-onboarding-startup-input ()
  "Open the listener on Escape or text, retaining input and pending lessons."
  (dolist (event '(:escape (:insert "(model \"deepseek-chat\")")
                          (:paste "(model \"deepseek-chat\")")
                          (:line "(model \"deepseek-chat\")")))
    (onboarding-tests--call
     (lambda (application controller configuration terminal)
       (let* ((ui       (application-ui application))
              (pages    (onboarding-pages configuration))
              (expected (if (consp event) (second event) ""))
              (next     '(:insert "more input")))
         (queued-recording-terminal-enqueue terminal event)
         (queued-recording-terminal-enqueue terminal next)
         (test-assert (eq ':deferred (application-startup-presentation application controller))
                      "Escape or typing bypasses the tour")
         (test-assert (string= expected (line-editor-text (terminal-ui-editor ui)))
                      "typed or pasted commands reach the listener intact")
         (test-assert (equal next (terminal-read-event terminal))
                      "the remaining input is available to the listener")
         (test-assert (equal pages (onboarding-state-pending configuration pages))
                      "bypassing startup leaves lessons available for replay")
         (test-assert (not (terminal-ui-fullscreen-p ui))
                      "the listener uses the saved inline mode")))))
  nil)

(-> test-onboarding-startup-exclusions () null)
(defun test-onboarding-startup-exclusions ()
  "Exclude noninteractive startup and both configuration and caller opt-outs."
  (dolist (case '(:headless :configuration-optout :caller-optout))
    (onboarding-tests--call
     (lambda (application controller configuration terminal)
       (declare (ignore configuration))
       (when (eq case ':headless)
         (setf (terminal-interactive-p terminal) nil))
       (when (eq case ':configuration-optout)
         (setf (application-configuration application)
               (configuration-copy (application-configuration application) :onboarding-p nil)))
       (let ((boot-count 0))
         (test-call-with-function-replacements
          (list (list 'application-onboard
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (error "Excluded startup entered onboarding.")))
                (list 'terminal-ui-boot-sequence
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (incf boot-count)
                        ':completed)))
          (lambda ()
            (application-startup-presentation application controller
                                              :onboarding-p (not (eq case ':caller-optout)))))
         (when (eq case ':headless)
           (test-assert (eq ':unavailable (application-onboard application))
                        "manual tour also declines noninteractive input"))
         (test-assert (= 1 boot-count) "excluded startup follows ordinary boot")))))
  nil)

(-> test-onboarding-version-selection () null)
(defun test-onboarding-version-selection ()
  "Suppress completed/refused versions and present only genuinely pending changes."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore terminal))
     (let* ((pages      (onboarding-tests--pages))
            (selected   nil)
            (boot-count 0)
            (tour-count 0))
       (onboarding-state-record configuration (list (first pages)) ':completed)
       (onboarding-state-record configuration (rest pages) ':refused)
       (test-call-with-function-replacements
        (list (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages))
              (list 'terminal-ui-boot-sequence
                    (lambda (&rest arguments) (declare (ignore arguments)) (incf boot-count) ':completed))
              (list 'terminal-ui-onboard
                    (lambda (ui offered &key help-function page-completed-function introduction-title &allow-other-keys)
                      (declare (ignore ui help-function introduction-title))
                      (incf tour-count)
                      (setf selected (copy-tree offered))
                      (dolist (page offered) (funcall page-completed-function page))
                      (values ':completed (mapcar (lambda (page) (getf page :id)) offered)))))
        (lambda ()
          (application-startup-presentation application controller)
          (test-assert (zerop tour-count) "acknowledged catalog uses ordinary boot")
          (test-assert (= 1 boot-count) "suppressed tour does not add another boot")
          (setf (getf (first pages) :version) 2
                pages (append pages (list (list :id "new" :version 1 :title "New" :body "New page"))))
          (application-startup-presentation application controller)
          (test-assert (equal '("first" "new") (mapcar (lambda (page) (getf page :id)) selected))
                       "automatic update selects revised and new lessons, skipping refused unchanged versions")
          (test-assert (null (onboarding-state-pending configuration pages)) "updates become durably acknowledged")
          (application-onboard application)
          (test-assert (equal pages selected) "explicit replay offers the entire acknowledged catalog"))))))
  nil)

(-> test-onboarding-incremental-interruption () null)
(defun test-onboarding-incremental-interruption ()
  "Persist completed pages on interruption and acknowledge the remainder on refusal."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore controller terminal))
     (let ((pages   (onboarding-tests--pages))
           (offered nil))
       (test-call-with-function-replacements
        (list (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages))
              (list 'terminal-ui-boot-sequence (lambda (&rest arguments) (declare (ignore arguments)) ':completed))
              (list 'terminal-ui-onboard
                    (lambda (ui selected &key help-function page-completed-function introduction-title &allow-other-keys)
                      (declare (ignore ui help-function introduction-title))
                      (setf offered selected)
                      (funcall page-completed-function (first selected))
                      (values ':interrupted (list (getf (first selected) :id))))))
        (lambda ()
          (test-assert (eq ':interrupted (application-onboard application :new-only-p t))
                       "tour interruption propagates to application")
          (test-assert (equal (rest offered) (onboarding-state-pending configuration pages))
                       "only the completed page is acknowledged after interruption")))
       (test-call-with-function-replacements
        (list (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages))
              (list 'terminal-ui-boot-sequence (lambda (&rest arguments) (declare (ignore arguments)) ':completed))
              (list 'terminal-ui-onboard
                    (lambda (ui selected &key help-function page-completed-function introduction-title &allow-other-keys)
                      (declare (ignore ui help-function page-completed-function introduction-title))
                      (test-assert (equal (rest pages) selected) "resume offers unfinished pages only")
                      (values ':refused nil))))
        (lambda ()
          (test-assert (eq ':refused (application-onboard application :new-only-p t)) "explicit refusal propagates")
          (test-assert (null (onboarding-state-pending configuration pages)) "refusal acknowledges remaining selected versions"))))))
  nil)

(-> test-onboarding-fullscreen-restoration () null)
(defun test-onboarding-fullscreen-restoration ()
  "Restore fullscreen mode and input ownership after completion, refusal, and errors."
  (dolist (fullscreen-p '(nil t))
    (dolist (outcome '(:completed :refused :error))
      (onboarding-tests--call
       (lambda (application controller configuration terminal)
         (declare (ignore configuration terminal))
         (let* ((ui                (application-ui application))
                (owner             (terminal-ui-exclusive-input-function ui))
                (reader-was-live-p (application-input-controller-reader-live-p controller)))
           (test-assert reader-was-live-p "fixture has a live responsive reader")
           (terminal-ui-set-input ui "user draft")
           (test-call-with-function-replacements
            (list (list 'terminal-ui-boot-sequence (lambda (&rest arguments) (declare (ignore arguments)) ':completed))
                  (list 'terminal-ui-onboard
                        (lambda (target pages &key help-function page-completed-function introduction-title &allow-other-keys)
                          (declare (ignore pages help-function page-completed-function introduction-title))
                          (test-assert (terminal-ui-fullscreen-p target) "tour enters fullscreen regardless of original mode")
                          (test-assert (fullscreen-terminal-ui-active-p target) "tour owns an active alternate buffer")
                          (when (eq outcome ':error) (error "Expected onboarding presenter failure."))
                          (values outcome nil))))
            (lambda ()
              (if (eq outcome ':error)
                  (test-assert (handler-case (progn (application-onboard application) nil)
                                 (simple-error () t)) "presenter error is propagated")
                  (test-assert (eq outcome (application-onboard application)) "presenter outcome is propagated"))))
           (test-assert (eq fullscreen-p (terminal-ui-fullscreen-p ui)) "original display mode is restored")
           (when fullscreen-p
             (test-assert (fullscreen-terminal-ui-active-p ui) "original fullscreen ownership survives the tour"))
           (test-assert (string= "user draft" (line-editor-text (terminal-ui-editor ui))) "mode transition preserves the draft")
           (test-assert (eq owner (terminal-ui-exclusive-input-function ui)) "exclusive reader owner is restored")
           (test-assert (application-input-controller-reader-live-p controller) "responsive reader is live after tour cleanup")
           (test-assert (not (application-input-controller-exit-requested-p controller)) "manual replay does not retire the controller")))
       :fullscreen-p fullscreen-p :start-reader-p t)))
  nil)

(-> test-onboarding-startup-interruption () null)
(defun test-onboarding-startup-interruption ()
  "Request controller exit when either the onboarding boot or presenter is interrupted."
  (dolist (stage '(:boot :presenter))
    (onboarding-tests--call
     (lambda (application controller configuration terminal)
       (declare (ignore terminal))
       (test-call-with-function-replacements
        (list (list 'terminal-ui-boot-sequence
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (if (eq stage ':boot) ':interrupt ':completed)))
              (list 'terminal-ui-onboard
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (test-assert (eq stage ':presenter) "interrupted boot does not enter lessons")
                      (values ':interrupted nil))))
        (lambda ()
          (test-assert (eq ':interrupted (application-startup-presentation application controller))
                       "startup returns interrupted outcome")
          (test-assert (application-input-controller-exit-requested-p controller)
                       "startup interruption requests controller exit")
          (test-assert (equal (mapcar (lambda (page) (getf page :id)) (onboarding-pages configuration))
                              (mapcar (lambda (page) (getf page :id))
                                      (onboarding-state-pending configuration (onboarding-pages configuration))))
                       "interrupted startup leaves uncompleted revisions pending"))))))
  nil)

(-> test-onboarding-settings-exercise () null)
(defun test-onboarding-settings-exercise ()
  "Try the real settings picker, persist a change, and return to the tour."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore controller))
     (setf (application-configuration application)
           (configuration-copy configuration :boot-screen-p nil))
     (let ((pages (list (onboarding-ui-test--page "settings" ':settings)
                        (onboarding-ui-test--page "finish")))
           (ui    (application-ui application)))
       (terminal-ui-set-input ui "saved user draft")
       (dolist (event '((:paste "(settings)") :submit
                        (:paste "theme") :submit (:paste "almighty") :submit :escape
                        :submit :submit))
         (queued-recording-terminal-enqueue terminal event))
       (test-call-with-function-replacements
        (list (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages)))
        (lambda ()
          (test-assert (eq ':completed (application-onboard application))
                       "settings exercise returns to the tour and completes")))
       (test-assert (eq ':almighty (config :terminal-theme (application-configuration application)))
                    "the user changed a real durable setting")
       (test-assert (null (onboarding-state-pending configuration pages)) "exercise completion is recorded")
       (test-assert (search "settings" (recording-terminal-output terminal)) "the real picker was rendered")
       (test-assert (null (queued-recording-terminal-events terminal)) "every picker and lesson event was consumed")
       (test-assert (string= "saved user draft" (line-editor-text (terminal-ui-editor ui)))
                    "the user's original draft is restored")
       (test-assert (not (terminal-ui-fullscreen-p ui)) "the original inline mode is restored")
       (test-assert (not (terminal-ui-live-output-suspended-p ui)) "live output is available after the tour"))))
  nil)

(-> test-startup-saved-model-selection-notice () null)
(defun test-startup-saved-model-selection-notice ()
  "Present a recovered saved selection once through the startup UI."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (setf (config :saved-model-selection-notice configuration) "Recovered saved selection.")
     (test-call-with-function-replacements
      (list (list 'terminal-ui-boot-sequence
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    ':completed)))
      (lambda ()
        (application-startup-presentation application controller :onboarding-p nil)
        (test-assert (search "Recovered saved selection." (recording-terminal-output terminal))
                     "the notice reaches the terminal presentation path")
        (test-assert (not (non-empty-string-p
                           (config :saved-model-selection-notice configuration)))
                     "the pending notice is consumed after presentation")))))
  nil)

(-> test-onboarding-choice-exercise () null)
(defun test-onboarding-choice-exercise ()
  "Pick a lesson's setting value by typing its form, persisting it for later launches."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore controller))
     (setf (application-configuration application)
           (configuration-copy configuration :boot-screen-p nil))
     (let ((pages (list (list :id "pick" :version 1 :title "Pick" :body "Choose."
                              :exercise ':choose :setting ':activity-line-p :choices '(t nil))
                        (onboarding-ui-test--page "finish"))))
       (dolist (event '((:paste "(settings :activity-line-p maybe)") :submit :home :kill-to-end
                        (:paste "(settings :activity-line-p nil)") :submit :submit :submit))
         (queued-recording-terminal-enqueue terminal event))
       (test-call-with-function-replacements
        (list (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages)))
        (lambda ()
          (test-assert (eq ':completed (application-onboard application))
                       "the choice lesson completes once a value is picked")))
       (test-assert (null (config :activity-line-p (application-configuration application)))
                    "the picked value applies to the running session")
       (test-assert (null (getf (preferences-load-values configuration) :activity-line-p ':absent))
                    "the picked value is saved for later launches")
       (test-assert (null (queued-recording-terminal-events terminal))
                    "a form naming no choice is only edited, then the lesson continues"))))
  nil)

(-> test-onboarding-authentication () null)
(defun test-onboarding-authentication ()
  "Drive real provider and method pickers through login, retry, back, and skip.

Only replace provider effects and model discovery. Keep application-authenticate,
its native method dispatch, terminal ownership, and the tour presenter real."
  (dolist (scenario
            '((:skip ((:paste "(auth)") :submit :submit) nil nil)
              (:continue ((:paste "continue") :submit) nil nil (:submit))
              (:browser ((:paste "(auth)") :submit (:paste "chatgpt") :submit :submit)
               (:browser) nil)
              (:device-code ((:paste "(auth)") :submit (:paste "chatgpt") :submit :down :submit)
               (:device-code) nil)
              (:provider-cancel ((:paste "(auth)") :submit :escape
                                 (:paste "(auth)") :submit :submit) nil nil)
              (:method-back ((:paste "(auth)") :submit (:paste "chatgpt") :submit
                             :escape :submit) nil nil)
              (:retry ((:paste "(auth)") :submit (:paste "chatgpt") :submit :submit
                       (:paste "(auth)") :submit (:paste "chatgpt") :submit :down :submit)
               (:browser :device-code) :failure)
              (:login-cancel ((:paste "(auth)") :submit (:paste "chatgpt") :submit :submit
                              (:paste "(auth)") :submit :submit)
               (:browser) :cancel)))
    (destructuring-bind (name events expected-methods first-outcome
                         &optional (finish-events '(:submit :submit))) scenario
      (onboarding-tests--call
       (lambda (application controller configuration terminal)
         (declare (ignore controller))
         (setf (application-configuration application)
               (configuration-copy configuration :boot-screen-p nil))
         (let* ((ui      (application-ui application))
                (model   (config :model (application-configuration application)))
                (pages   (list (onboarding-ui-test--page "first-provider" ':authentication)
                               (onboarding-ui-test--page "finish")))
                (methods nil)
                (calls   0))
           (terminal-ui-set-input ui "saved draft")
           (dolist (event (append events finish-events))
             (queued-recording-terminal-enqueue terminal event))
           (test-call-with-function-replacements
            (list
             (list 'onboarding-pages (lambda (config) (declare (ignore config)) pages))
             (list 'provider-authenticated-model
                   (lambda (config provider) (declare (ignore config provider)) nil))
             (list 'provider-authenticate
                   (lambda (provider &key stream open-browser-p)
                     (declare (ignore stream))
                     (test-assert (typep provider 'codex-subscription-provider)
                                  "picker resolves the actual registered provider")
                     (test-assert open-browser-p "login uses the usual browser boundary")
                     (push *chatgpt-authentication-method* methods)
                     (incf calls)
                     (when (= calls 1)
                       (case first-outcome
                         (:failure
                          (error 'authentication-error :message "Fixture login failed"))
                         (:cancel
                          (error 'end-of-file))))
                     "Authenticated fixture account.")))
            (lambda ()
              (test-assert (eq ':completed (application-onboard application))
                           (format nil "~A returns from authentication to the tour" name))))
           (test-assert (equal expected-methods (reverse methods))
                        (format nil "~A dispatches exactly the selected login methods" name))
           (test-assert (eq model (config :model (application-configuration application)))
                        "skip or authentication without a discovered model keeps the existing model")
           (test-assert (null (onboarding-state-pending configuration pages))
                        "completed exercises are acknowledged")
           (test-assert (null (queued-recording-terminal-events terminal))
                        "all picker and tour input was consumed")
           (test-assert (string= "saved draft" (line-editor-text (terminal-ui-editor ui)))
                        "authentication restores the user's draft")
           (test-assert (not (terminal-ui-fullscreen-p ui)) "inline mode is restored")
           (test-assert (not (terminal-ui-live-output-suspended-p ui))
                        "authentication restores output suspension"))))))
  nil)

(-> test-onboarding-openai-api () null)
(defun test-onboarding-openai-api ()
  "Enter an official API key through onboarding and discover its models."
  (let ((snapshot (provider--registry-snapshot)))
    (unwind-protect
         (onboarding-tests--call
          (lambda (application controller configuration terminal)
            (declare (ignore controller))
            (setf (application-configuration application)
                  (configuration-copy configuration :boot-screen-p nil))
            (let* ((pages    (list (onboarding-ui-test--page "first-provider" ':authentication)
                                   (onboarding-ui-test--page "finish")))
                   (output   (make-string-output-stream))
                   (input    (make-string-input-stream (format nil "onboarding-fixture-key~%")))
                   (requests 0))
              (dolist (event '((:paste "(auth)") :submit (:paste "openai-api")
                               :submit :submit :submit))
                (queued-recording-terminal-enqueue terminal event))
              (test-call-with-function-replacements
               (list
                (list 'onboarding-pages
                      (lambda (config) (declare (ignore config)) pages))
                (list 'application--authentication-streams
                      (lambda (target)
                        (test-assert (eq target application) "authentication uses this application")
                        (values input output nil t -1)))
                (list 'dexador:get
                      (lambda (url &key headers &allow-other-keys)
                        (test-assert (string= url "https://api.openai.com/v1/models")
                                     "the official API model endpoint is queried")
                        (test-assert
                         (equal (rest (assoc "Authorization" headers :test #'string-equal))
                                "Bearer onboarding-fixture-key")
                         "model discovery uses the privately saved key")
                        (incf requests)
                        (values "{\"data\":[{\"id\":\"openai-onboarding-fixture\"}]}" 200 nil))))
               (lambda ()
                 (test-assert (eq ':completed (application-onboard application))
                              "API-key onboarding returns to and completes the tour")))
              (test-assert (= requests 1) "authentication discovers the account catalog once")
              (test-assert
               (member "openai-onboarding-fixture" (provider-model-identifiers) :test #'string=)
               "the discovered model is available for selection")
              (let* ((provider   (application--authentication-provider application "openai-api"))
                     (credentials
                       (credential-source-load
                        (credential-manager-primary-source (provider-credential-manager provider))))
                     (transcript (concatenate 'string (get-output-stream-string output)
                                              (recording-terminal-output terminal))))
                (test-assert (and credentials
                                  (string= "onboarding-fixture-key"
                                           (oauth-credentials-access-token credentials)))
                             "onboarding saves the key in the private API-key store")
                (test-assert (not (search "onboarding-fixture-key" transcript))
                             "the key is absent from prompts and the terminal transcript")
                (test-assert (not (secret-use-active-p)) "authentication exits the secret-use scope"))
              (let* ((selected
                       (configuration-copy configuration :model "openai-onboarding-fixture"))
                     (provider     (provider-create selected))
                     (conversation (conversation-create selected :identifier "openai-onboarding-request"))
                     (request      (provider-request-object provider conversation #())))
                (test-assert
                 (and (string= "https://api.openai.com/v1/responses"
                               (config :provider-endpoint (provider-configuration provider)))
                      (string= "openai-api" (provider-account-label provider))
                      (string= "openai-onboarding-fixture" (json-get request "model"))
                      (vectorp (json-get request "input")))
                 "a discovered API model resolves to the public Responses endpoint"))
              (test-assert (null (queued-recording-terminal-events terminal))
                           "API-key entry does not open the subscription method picker")
              (test-assert (null (onboarding-state-pending configuration pages))
                           "successful onboarding acknowledges its pages"))))
      (provider--registry-restore snapshot)))
  nil)

(-> test-onboarding-authentication-pending () null)
(defun test-onboarding-authentication-pending ()
  "Offer the new provider lesson to existing users without forcing a login."
  (onboarding-tests--call
   (lambda (application controller configuration terminal)
     (declare (ignore controller))
     (setf (application-configuration application)
           (configuration-copy configuration :boot-screen-p nil))
     (let* ((pages     (onboarding-pages configuration))
            (old-pages (remove "first-provider" pages :test #'string=
                               :key                         (lambda (page) (getf page :id)))))
       (onboarding-state-record configuration old-pages ':completed)
       (test-assert (equal '("first-provider")
                           (mapcar (lambda (page) (getf page :id))
                                   (onboarding-state-pending configuration pages)))
                    "the new lesson revision is pending for an acknowledged old tour")
       (dolist (event '((:paste "(auth)") :submit :escape :interrupt))
         (queued-recording-terminal-enqueue terminal event))
       (test-assert (eq ':interrupted (application-onboard application :new-only-p t))
                    "cancelling a picker does not finish the lesson")
       (test-assert (= 1 (length (onboarding-state-pending configuration pages)))
                    "interruption after cancellation leaves authentication pending")
       (dolist (event '((:paste "(auth)") :submit :submit :submit))
         (queued-recording-terminal-enqueue terminal event))
       (test-assert (eq ':completed (application-onboard application :new-only-p t))
                    "explicit authentication skip completes the lesson")
       (test-assert (null (onboarding-state-pending configuration pages))
                    "skip is persisted as completion without credential data"))))
  nil)
