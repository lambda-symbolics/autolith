(in-package #:autolith)

;;;; -- Application Integration --

(-> application-onboarding--call-fullscreen (application function) t)
(defun application-onboarding--call-fullscreen (application function)
  "Call FUNCTION with exclusive fullscreen ownership, restoring the user's mode."
  (let* ((ui          (application-ui application))
         (inline-p    (not (terminal-ui-fullscreen-p ui)))
         (active-p    (and (not inline-p) (fullscreen-terminal-ui-active-p ui)))
         (suspended-p (terminal-ui-live-output-suspended-p ui)))
    (terminal-ui-call-with-exclusive-input
     ui
     (lambda ()
       (unwind-protect
            (progn
              (with-terminal-ui-locked (ui)
                (setf (terminal-ui-live-output-suspended-p ui) t)
                (when inline-p
                  (change-class ui 'fullscreen-terminal-ui)))
              (terminal-ui-fullscreen-enter ui)
              (funcall function))
         (unwind-protect
              (unless active-p
                (when (terminal-ui-fullscreen-p ui)
                  (terminal-ui-fullscreen-leave ui)))
           (with-terminal-ui-locked (ui)
             (when inline-p
               (change-class ui 'terminal-ui))
             (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
             (unless suspended-p
               (terminal-ui--paint-live ui)))))))))

(-> application-onboarding--settings (application) null)
(defun application-onboarding--settings (application)
  "Open the real settings picker and return to the suspended tour on every exit."
  (let* ((ui          (application-ui application))
         (suspended-p (terminal-ui-live-output-suspended-p ui)))
    (unwind-protect
         (progn
           (with-terminal-ui-locked (ui)
             (setf (terminal-ui-live-output-suspended-p ui) nil))
           (let ((*application-command-interactive-p* t))
             (application-settings-command application)))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-live-output-suspended-p ui) suspended-p))))
  nil)

(-> application-onboarding--authenticate (application) keyword)
(defun application-onboarding--authenticate (application)
  "Pick a provider and method, then use the normal authentication boundary.

Skipping keeps the existing account and model. Picker cancellation returns to
its caller; cancelling the method picker returns to provider selection. Restore
the tour's output suspension even when provider authentication fails. Never
retain provider output or condition details in onboarding state."
  (let* ((ui          (application-ui application))
         (suspended-p (terminal-ui-live-output-suspended-p ui)))
    (unwind-protect
         (progn
           (with-terminal-ui-locked (ui)
             (setf (terminal-ui-live-output-suspended-p ui) nil))
           (block selection
             (loop
               (let ((name
                       (application--pick-identifier
                        application
                        :title         (onboarding-source-text "auth-provider-title")
                        :items         (cons (list :name "skip" :argument nil
                                                   :description (onboarding-source-text "auth-skip"))
                                             (application--authentication-provider-items application))
                        :visible-count (application--authentication-provider-visible-count application)
                        :initial-name "skip" :search-p t)))
                 (unless name
                   (return-from selection ':cancelled))
                 (when (string= name "skip")
                   (return-from selection ':skipped))
                 (let* ((provider  (application--authentication-provider application name))
                        (chatgpt-p (typep provider 'codex-subscription-provider))
                        (method
                          (when chatgpt-p
                            (application--pick-identifier
                             application
                             :title        (onboarding-source-text "auth-method-title")
                             :items
                             (list (list :name "browser" :argument nil
                                         :description (onboarding-source-text "auth-browser"))
                                   (list :name "device-code" :argument nil
                                         :description (onboarding-source-text "auth-device-code")))
                             :initial-name "browser"))))
                   (when (or (not chatgpt-p) method)
                     (return-from selection
                       (handler-case
                           (progn
                             (application-authenticate
                              application name
                              (when method
                                (if (string= method "browser") ':browser ':device-code)))
                             ':authenticated)
                         (authentication-error () ':failed)
                         (end-of-file () ':cancelled)))))))))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
        (terminal-ui-fullscreen-invalidate ui)))))

(-> application-onboard (application &key (:new-only-p boolean)) keyword)
(defun application-onboard (application &key new-only-p)
  "Replay the tour, or only pending revisions when NEW-ONLY-P is true.

Persist each completed page immediately. Explicit refusal acknowledges the
remaining selected revisions; interruption leaves them pending."
  (let* ((*onboarding-source-snapshot* (onboarding-source--read))
         (configuration                (application-configuration application))
         (ui                           (application-ui application))
         (pages                        (onboarding-pages configuration))
         (selected                     (if new-only-p
                                           (onboarding-state-pending configuration pages)
                                           pages)))
    (cond
      ((not (terminal-interactive-p (terminal-ui-terminal ui)))
       ':unavailable)
      ((null selected)
       ':completed)
      (t
       (application-onboarding--call-fullscreen
        application
        (lambda ()
           (let ((*terminal-ui-boot-linger-prompt* (onboarding-source-text "boot-linger"))
                 (*terminal-ui-boot-start-label*   (onboarding-source-text "boot-start")))
             (case (terminal-ui-boot-sequence
                    ui :screen-p (config :boot-screen-p configuration) :linger-p t
                    :resize-callback #'application-pending-terminal-size
                    :duration        (config :boot-screen-seconds configuration)
                    :tip-seconds     (config :boot-screen-tip-seconds configuration))
               (:interrupt
                ':interrupted)
               (:listener
                ':deferred)
               (otherwise
                (multiple-value-bind (outcome seen)
                    (terminal-ui-onboard
                     ui selected
                     :introduction-title
                     (if (and new-only-p (< (length selected) (length pages)))
                         (onboarding-source-text "introduction-new")
                         (onboarding-source-text "introduction"))
                     :help-function     (lambda (&optional topic)
                                          (application-operation-help application topic))
                     :settings-function (lambda () (application-onboarding--settings application))
                     :authentication-function
                     (lambda () (application-onboarding--authenticate application))
                     :choose-function
                     (lambda (setting value)
                       (application-apply-setting application setting value))
                     :resize-callback   #'application-pending-terminal-size
                     :page-completed-function
                     (lambda (page)
                       (onboarding-state-record configuration (list page) ':completed)))
                  (when (eq outcome ':refused)
                    (onboarding-state-record
                     configuration
                     (remove-if (lambda (page) (member (getf page :id) seen :test #'equal))
                                selected)
                     ':refused))
                  outcome))))))))))

(-> application-startup-presentation
    (application application-input-controller &key (:onboarding-p boolean)) keyword)
(defun application-startup-presentation (application controller &key (onboarding-p t))
  "Present pending onboarding on interactive startup, otherwise the usual boot."
  (let* ((ui            (application-ui application))
         (terminal      (terminal-ui-terminal ui))
         (configuration (application-configuration application))
         (pending       (and onboarding-p (config :onboarding-p configuration)
                             (onboarding-state-pending configuration
                                                       (onboarding-pages configuration)))))
    (when (and pending (typep terminal 'localgroup-terminal))
      ;; Client-first sessions publish their listener before the relay reports a
      ;; TTY. Wait for the same window as the launch's attachment watchdog.
      (catch 'onboarding-client-wait-ended
        (terminal-ui--await-interactive
         terminal
         (lambda (seconds)
           (when (application-input-controller-exit-requested-p controller)
             (throw 'onboarding-client-wait-ended nil))
           (sleep seconds))
         *localgroup-first-attach-timeout-seconds*)))
    (let ((outcome
            (if (and pending (terminal-interactive-p terminal)
                     (not (application-input-controller-exit-requested-p controller)))
                (application-onboard application :new-only-p t)
                (terminal-ui-boot-sequence
                 ui :screen-p     (config :boot-screen-p configuration)
                 :duration        (config :boot-screen-seconds configuration)
                 :linger-p        (config :boot-screen-linger-p configuration)
                 :tip-seconds     (config :boot-screen-tip-seconds configuration)
                 :resize-callback #'application-pending-terminal-size
                 :halted-function
                 (lambda () (application-input-controller-exit-requested-p controller))
                 :direct-input-p-function
                 (lambda () (not (application-input-controller-reader-live-p controller)))))))
      (let ((notice (config :saved-model-selection-notice configuration)))
        (when (non-empty-string-p notice)
          (application-present application notice)
          (setf (config :saved-model-selection-notice configuration) "")))
      (when (member outcome '(:interrupt :interrupted))
        (application-input-controller--request-exit controller ':interrupt))
      outcome)))

(define-application-command application--builtin-onboard-command
    (:name              "/onboard"
     :argument          nil
     :description       "replay the fullscreen onboarding tour"
     :tip               "Use `(onboard)` to practice the prompt and explore Autolith's capabilities."
     :busy-behavior     :hold
     :terminal-behavior :exclusive
     :callable          t)
    (application)
  (application-onboard application)
  ':continue)
