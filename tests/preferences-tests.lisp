(in-package #:autolith)

;;;; -- Global Preference Tests --


(-> preferences-tests--without-model-environment (function) t)
(defun preferences-tests--without-model-environment (function)
  "Call FUNCTION while model, effort, and Fast mode overrides are absent."
  (with-test-environment (("AUTOLITH_MODEL" nil)
                          ("AUTOLITH_REASONING_EFFORT" nil)
                          ("AUTOLITH_CODEX_FAST_MODE" nil))
    (funcall function)))

(-> test-preferences () null)
(defun test-preferences ()
  "Test atomic global preferences, version compatibility, and malformed recovery."
  (with-test-configuration (configuration)
    (let* ((pathname (configuration-preferences-path configuration)))
      (test-assert
       (equal pathname
              (merge-pathnames "preferences.sexp"
                               (configuration-state-root configuration)))
       "global preferences live under the state root")
       (let ((preferences (preferences-load configuration)))
         (test-assert
          (not (preference-state-reasoning-traces-p preferences))
          "missing preferences default reasoning summaries to hidden")
         (test-assert (null (preference-state-model preferences))
                      "missing preferences have no saved model")
         (test-assert
          (null (preference-state-reasoning-effort preferences))
          "missing preferences have no saved reasoning effort")
         (test-assert
          (not (preference-state-codex-fast-mode-p preferences))
          "missing preferences default Codex Fast mode to disabled")
         (test-assert
          (preference-state-compact-view-p preferences)
          "missing preferences default to compact tool presentation")
         (test-assert
          (not (preference-state-turn-timestamps-p preferences))
          "missing preferences default turn timestamps to hidden")
         (test-assert
          (not (preference-state-cache-miss-notices-p preferences))
          "missing preferences default prompt-cache miss notices to off")
         (test-assert
          (not (preference-state-simple-technical-english-p preferences))
          "missing preferences default Simple Technical English to disabled")
         (test-assert
          (preference-state-session-title-generation-p preferences)
          "missing preferences enable generated session titles")
         (test-assert
          (null (preference-state-permission-mode preferences))
          "missing preferences have no saved command-permission mode")
         (test-assert
          (eq preferences (preferences-load configuration))
          "unchanged missing preferences reuse their validated state"))
       (preferences-tests--without-model-environment
        (lambda ()
          (platform-setenv "AUTOLITH_CODEX_FAST_MODE" "on")
          (let ((environment-configuration
                  (configuration-create
                   :source-root (asdf:system-source-directory :autolith)
                   :working-directory (asdf:system-source-directory :autolith))))
            (test-assert
             (configuration-codex-fast-mode-p environment-configuration)
             "the environment can enable Codex Fast mode")
            (test-assert
             (not
              (configuration-codex-fast-mode-p
               (configuration-create
                :source-root (asdf:system-source-directory :autolith)
                :working-directory (asdf:system-source-directory :autolith)
                :codex-fast-mode-p nil)))
             "an explicit Fast mode choice overrides the environment"))
          (platform-setenv "AUTOLITH_CODEX_FAST_MODE" "invalid")
          (test-assert
           (handler-case
               (progn
                 (configuration-create
                  :source-root (asdf:system-source-directory :autolith)
                  :working-directory (asdf:system-source-directory :autolith))
                 nil)
             (configuration-error ()
               t))
           "invalid Codex Fast mode environment values are rejected")))
      (ensure-directories-exist pathname)
      (snapshot-write
       pathname
       '(:preferences
         :version 3
         :model "gpt-5.6-luna"
         :reasoning-effort "high"
         :reasoning-traces-p t
         :compact-view-p nil
         :turn-timestamps-p t))
       (let ((preferences (preferences-load configuration)))
         (test-assert
          (not (preference-state-compact-view-p preferences))
          "version three compact preferences remain readable")
         (test-assert
          (preference-state-turn-timestamps-p preferences)
          "version three turn-timestamp preferences remain readable")
         (test-assert
          (not (preference-state-simple-technical-english-p preferences))
          "version three preferences default Simple Technical English to disabled")
         (test-assert
          (not (preference-state-codex-fast-mode-p preferences))
          "version three preferences default Codex Fast mode to disabled")
         (test-assert
          (eq preferences (preferences-load configuration))
          "normalized preferences reuse their validated state")
         (multiple-value-bind (form sole-form-p)
             (snapshot-read pathname)
           (test-assert sole-form-p
                        "normalizing preferences preserves one form")
           (test-assert (= (getf (rest form) :version) 6)
                        "version three preferences migrate to version six")
           (test-assert
            (not (getf (rest form) :compact-view-p))
            "normalizing preferences preserves compact presentation")
           (test-assert
            (not (getf (rest form) :cache-miss-notices-p))
            "normalizing preferences adds disabled prompt-cache miss notices")
           (test-assert
            (getf (rest form) :turn-timestamps-p)
            "normalizing preferences preserves turn-timestamp presentation")
           (test-assert
            (not (getf (rest form) :simple-technical-english-p))
            "normalizing preferences adds the disabled response style")
           (test-assert
            (not (getf (rest form) :codex-fast-mode-p))
            "normalizing preferences adds disabled Codex Fast mode")
           (test-assert
            (getf (rest form) :session-title-generation-p)
            "migration enables generated session titles")))
       (snapshot-write
        pathname
        '(:preferences
          :version 2
          :model "gpt-5.6-sol"
          :reasoning-effort "ultra"
          :reasoning-traces-p nil
          :compact-view-p t))
       (let ((version-two (preferences-load configuration)))
         (test-assert
          (not (preference-state-codex-fast-mode-p version-two))
          "version two preferences without Fast mode default it to disabled")
         (test-assert
          (preference-state-session-title-generation-p version-two)
          "version two preferences enable generated session titles"))
       (snapshot-write
        pathname
        '(:preferences
          :version 4
          :model nil
          :reasoning-effort nil
          :codex-fast-mode-p t
          :reasoning-traces-p t
          :compact-view-p t
          :turn-timestamps-p nil
          :simple-technical-english-p nil
          :session-title-generation-p nil
          :permission-mode nil))
       (let ((preferences (preferences-load configuration)))
         (test-assert
          (preference-state-codex-fast-mode-p preferences)
          "version four preferences preserve Codex Fast mode")
         (test-assert
          (preference-state-reasoning-traces-p preferences)
          "version four preferences preserve reasoning summaries")
         (test-assert
          (not (preference-state-session-title-generation-p preferences))
          "version four preferences preserve disabled title generation")
         (multiple-value-bind (form sole-form-p)
             (snapshot-read pathname)
           (test-assert sole-form-p
                        "version four preferences remain one form")
           (test-assert
            (= (getf (rest form) :version) 6)
            "version four preferences migrate to version six")))
      (with-open-file (stream pathname
                              :direction ':output
                              :if-exists ':supersede
                              :if-does-not-exist ':create
                              :external-format ':utf-8)
        (prin1 '(:preferences
                 :version 1
                 :reasoning-traces-p t)
               stream)
        (terpri stream))
       (let ((legacy (preferences-load configuration)))
         (test-assert
          (preference-state-reasoning-traces-p legacy)
          "version one reasoning-summary preferences remain readable")
         (test-assert (null (preference-state-model legacy))
                      "version one preferences have no saved model")
         (test-assert
          (preference-state-compact-view-p legacy)
          "version one preferences default to compact tool presentation")
         (test-assert
          (not (preference-state-turn-timestamps-p legacy))
          "version one preferences default turn timestamps to hidden")
         (test-assert
          (not (preference-state-simple-technical-english-p legacy))
          "version one preferences default Simple Technical English to disabled")
         (test-assert
          (not (preference-state-codex-fast-mode-p legacy))
          "version one preferences default Codex Fast mode to disabled")
         (test-assert
          (preference-state-session-title-generation-p legacy)
          "version one preferences enable generated session titles"))
       (let* ((selected
                (configuration-with-reasoning-effort
                 (configuration-with-model configuration "gpt-5.6-luna")
                 "high")))
         (preferences-set-model-selection selected)
         (preferences-set-codex-fast-mode selected t)
         (let ((preferences (preferences-load configuration)))
           (test-assert
            (string= (preference-state-model preferences) "gpt-5.6-luna")
            "the selected model survives a preference reload")
           (test-assert
            (string= (preference-state-reasoning-effort preferences) "high")
            "the selected reasoning effort survives a preference reload")
           (test-assert
            (preference-state-codex-fast-mode-p preferences)
            "Codex Fast mode survives a preference reload")
           (test-assert
            (preference-state-reasoning-traces-p preferences)
            "saving model choices preserves the trace preference")
           (test-assert
            (preference-state-compact-view-p preferences)
            "saving model choices preserves compact presentation")
           (test-assert
            (not (preference-state-turn-timestamps-p preferences))
            "saving model choices preserves hidden turn timestamps"))
         (preferences-tests--without-model-environment
          (lambda ()
            (let ((restored
                    (preferences-apply-model-selection configuration)))
              (test-assert
               (string= (configuration-model restored) "gpt-5.6-luna")
               "saved models become startup defaults")
              (test-assert
               (string= (configuration-reasoning-effort restored) "high")
               "saved efforts become startup defaults")
              (test-assert
               (configuration-codex-fast-mode-p restored)
               "saved Codex Fast mode becomes the startup default")))))
      (preferences-tests--without-model-environment
       (lambda ()
         (platform-setenv "AUTOLITH_CODEX_FAST_MODE" "off")
         (test-assert
          (not
           (configuration-codex-fast-mode-p
            (preferences-apply-model-selection
             (configuration-with-codex-fast-mode configuration nil))))
          "the environment can disable saved Codex Fast mode")
         (preferences-set-codex-fast-mode configuration nil)
         (platform-setenv "AUTOLITH_CODEX_FAST_MODE" "on")
         (test-assert
          (configuration-codex-fast-mode-p
           (preferences-apply-model-selection
            (configuration-with-codex-fast-mode configuration t)))
          "the environment can enable Codex Fast mode over a saved default")
         (preferences-set-codex-fast-mode configuration t)))
      (test-assert (test-fixture-permissions-p *platform* pathname ':private-file)
                   "global preferences remain private to the user")
      (preferences-set-reasoning-traces configuration nil)
      (let ((preferences (preferences-load configuration)))
        (test-assert
         (not (preference-state-reasoning-traces-p preferences))
         "disabled reasoning summaries survive a preference reload")
        (test-assert
         (string= (preference-state-model preferences) "gpt-5.6-luna")
         "changing traces preserves the selected model")
        (test-assert
         (string= (preference-state-reasoning-effort preferences) "high")
         "changing traces preserves the selected effort")
        (test-assert
         (preference-state-compact-view-p preferences)
         "changing traces preserves compact presentation")
        (test-assert
         (not (preference-state-turn-timestamps-p preferences))
         "changing traces preserves hidden turn timestamps"))
      (preferences-set-session-title-generation configuration nil)
      (test-assert
       (not (preferences-session-title-generation-p configuration))
       "disabled generated session titles survive a preference reload")
      (preferences-set-session-title-generation configuration t)
      (test-assert
       (preferences-session-title-generation-p configuration)
       "enabled generated session titles survive a preference reload")
      (preferences-set-compact-view configuration nil)
      (let ((preferences (preferences-load configuration)))
        (test-assert
         (not (preference-state-compact-view-p preferences))
         "expanded tool presentation survives a preference reload")
        (test-assert
         (string= (preference-state-model preferences) "gpt-5.6-luna")
         "changing compact presentation preserves the selected model")
        (test-assert
         (not (preference-state-reasoning-traces-p preferences))
         "changing compact presentation preserves trace mode")
        (test-assert
         (not (preference-state-turn-timestamps-p preferences))
         "changing compact presentation preserves hidden turn timestamps"))
      (preferences-set-turn-timestamps configuration t)
      (let ((preferences (preferences-load configuration)))
        (test-assert
         (preference-state-turn-timestamps-p preferences)
         "visible turn timestamps survive a preference reload")
        (test-assert
         (not (preference-state-compact-view-p preferences))
         "changing turn timestamps preserves compact presentation")
        (test-assert
         (not (preference-state-reasoning-traces-p preferences))
         "changing turn timestamps preserves trace mode"))
      (preferences-set-cache-miss-notices configuration t)
      (let ((preferences (preferences-load configuration)))
        (test-assert
         (preference-state-cache-miss-notices-p preferences)
         "prompt-cache miss notices survive a preference reload")
        (test-assert
         (preference-state-turn-timestamps-p preferences)
         "changing cache-miss notices preserves turn timestamps"))
      (preferences-set-simple-technical-english configuration t)
      (test-assert
       (preferences-simple-technical-english-p configuration)
       "Simple Technical English survives a preference reload")
      (preferences-set-reasoning-traces configuration t)
      (preferences-set-compact-view configuration t)
      (preferences-set-turn-timestamps configuration nil)
      (preferences-set-model-selection
       (configuration-with-reasoning-effort
        (configuration-with-model configuration "gpt-5.6-luna")
        "high"))
      (let ((preferences (preferences-load configuration)))
        (test-assert
         (preference-state-simple-technical-english-p preferences)
         "other preference setters preserve Simple Technical English")
        (test-assert
         (preference-state-reasoning-traces-p preferences)
         "changing the response style preserves trace mode")
        (test-assert
         (preference-state-compact-view-p preferences)
         "changing the response style preserves compact presentation")
        (test-assert
         (not (preference-state-turn-timestamps-p preferences))
         "changing the response style preserves hidden turn timestamps")
        (test-assert
         (preference-state-cache-miss-notices-p preferences)
         "other preference setters preserve prompt-cache miss notices")
        (test-assert
         (string= (preference-state-model preferences) "gpt-5.6-luna")
         "changing the response style preserves the selected model"))
        (preferences-set-permission-mode configuration ':auto)
        (test-assert
         (eq (preferences-permission-mode configuration) ':auto)
         "auto command-permission mode survives a preference reload")
        (preferences-set-simple-technical-english configuration nil)
        (test-assert
         (eq (preferences-permission-mode configuration) ':auto)
         "other preference setters preserve auto command-permission mode")
        (test-assert
         (not (preferences-simple-technical-english-p configuration))
         "Simple Technical English can be disabled durably")
        (preferences-set-permission-mode configuration ':ask)
        (test-assert
         (eq (preferences-permission-mode configuration) ':ask)
         "ask command-permission mode can replace auto durably")
      (with-open-file (stream pathname
                              :direction ':output
                              :if-exists ':supersede
                              :external-format ':utf-8)
        (write-string "#.(error \"preference read evaluation escaped\")"
                      stream))
      (let ((warning nil))
        (handler-bind
            ((preferences-load-warning
               (lambda (condition)
                 (setf warning condition)
                 (muffle-warning condition))))
          (test-assert
           (not (preference-state-reasoning-traces-p
                 (preferences-load configuration)))
           "malformed preferences fall back to safe defaults"))
        (test-assert (typep warning 'preferences-load-warning)
                     "malformed preferences emit a typed warning")
        (test-assert (equal (preferences-load-warning-pathname warning)
                            pathname)
                     "preference warnings identify the malformed file"))))
  nil)
