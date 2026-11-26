(in-package #:autolith)

;;;; -- Durable Settings Tests --

(-> preferences-tests--without-model-environment (function) t)
(defun preferences-tests--without-model-environment (function)
  "Call FUNCTION while model, effort, and Fast mode overrides are absent."
  (with-test-environment (("AUTOLITH_MODEL" nil)
                          ("AUTOLITH_REASONING_EFFORT" nil)
                          ("AUTOLITH_CODEX_FAST_MODE" nil))
    (funcall function)))

(-> preferences-tests--create (configuration &rest t) configuration)
(defun preferences-tests--create (configuration &rest overrides)
  "Create a process configuration on CONFIGURATION's roots, reading its preferences."
  (apply #'configuration-create
         :source-root              (config :source-root configuration)
         :working-directory        (config :working-directory configuration)
         :config-root              (config :config-root configuration)
         :data-root                (config :data-root configuration)
         :state-root               (config :state-root configuration)
         :cache-root               (config :cache-root configuration)
         :codex-auth-path          (config :codex-auth-path configuration)
         :grok-bootstrap-auth-path (config :grok-bootstrap-auth-path configuration)
         overrides))

(-> preferences-tests--file-plist (configuration) (values list boolean))
(defun preferences-tests--file-plist (configuration)
  "Return the durable plist in CONFIGURATION's preferences file and whether it is version 8."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read (configuration-preferences-path configuration))
    (values (preferences--form->plist form)
            (and sole-form-p (eql (getf (rest form) :version) 8) t))))

(-> preferences-tests--warned-p (function) boolean)
(defun preferences-tests--warned-p (function)
  "Return true when FUNCTION signals a preferences load warning."
  (let ((warned-p nil))
    (handler-bind ((preferences-load-warning
                     (lambda (warning)
                       (setf warned-p t)
                       (muffle-warning warning))))
      (funcall function))
    warned-p))

(-> test-preferences () null)
(defun test-preferences ()
  "Test the preferences file persists, merges, rejects unsupported versions, and recovers.

Source precedence, source recording, and value rejection belong to setinka."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-configuration (configuration)
       (let ((pathname (configuration-preferences-path configuration)))
         (test-assert
          (equal pathname
                 (merge-pathnames "preferences.sexp" (config :config-root configuration)))
          "global preferences live under the config root")
         (let ((created (preferences-tests--create configuration)))
           (dolist (case '((:reasoning-traces-p t)
                           (:turn-timestamps-p nil) (:cache-miss-notices-p nil)
                           (:simple-technical-english-p nil)
                           (:session-title-generation-p t)
                           (:codex-fast-mode-p nil) (:permission-mode nil)))
             (destructuring-bind (name expected) case
               (test-assert (eq (config name created) expected)
                            (format nil "a missing file leaves ~(~A~) at its default" name))))
           (test-assert (string= (config :model created) *default-model*)
                        "a missing file leaves the default model")
           (test-assert (not (probe-file pathname))
                        "reading defaults does not create the file")
           (setf (config :reasoning-traces-p created) nil)
           (multiple-value-bind (plist version-8-p)
               (preferences-tests--file-plist configuration)
             (test-assert (and version-8-p (null (getf plist :reasoning-traces-p)))
                          "a durable change writes a version 8 record"))
           (setf (config :model created) "gpt-5.6-luna"
                 (config :reasoning-effort created) "high"
                 (config :permission-mode created) ':ask)
           (let ((reloaded (preferences-tests--create configuration)))
             (test-assert (and (not (config :reasoning-traces-p reloaded))
                               (string= (config :model reloaded) "gpt-5.6-luna")
                               (string= (config :reasoning-effort reloaded) "high")
                               (eq (config :permission-mode reloaded) ':ask))
                          "durable values survive into a new configuration"))
           (setf (config :permission-mode created) nil)
           (test-assert (null (config :permission-mode (preferences-tests--create configuration)))
                        "an optional choice can be unset durably")
           (test-assert
            (handler-case (progn (setf (config :permission-mode created) ':sandboxed) nil)
              (setting-invalid () t))
            "session-only permission modes cannot be saved"))
         (with-test-environment (("AUTOLITH_MODEL" "gpt-5.6-terra")
                                 ("AUTOLITH_CODEX_FAST_MODE" "on"))
           (let ((created (preferences-tests--create configuration)))
             (test-assert (string= (config :model created) "gpt-5.6-terra")
                          "AUTOLITH_MODEL supplies the model")
             (test-assert (config :codex-fast-mode-p created)
                          "AUTOLITH_CODEX_FAST_MODE supplies Codex Fast mode")))
         (snapshot-write pathname
                         '(:preferences :version 8
                           :model "gpt-5.6-typo" :reasoning-effort "bogus"
                           :cache-miss-notices-p t :future-key 7))
         (let ((created (preferences-tests--create configuration)))
           (test-assert (string= (config :model created) *default-model*)
                        "an unsupported durable model is dropped")
           (test-assert (string= (config :reasoning-effort created)
                                 *default-reasoning-effort*)
                        "an unsupported durable effort is dropped")
           (setf (config :turn-timestamps-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (= (getf plist :future-key) 7)
                               (eq (getf plist :cache-miss-notices-p) t)
                               (eq (getf plist :turn-timestamps-p) t))
                          "storing one value keeps unknown and unrelated keys"))
           (snapshot-write pathname '(:preferences :version 8 :cache-miss-notices-p t))
           (setf (config :simple-technical-english-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (eq (getf plist :cache-miss-notices-p) t)
                               (member :simple-technical-english-p plist)
                               (eq (getf plist :simple-technical-english-p) t)
                               (null (member :turn-timestamps-p plist)))
                          "storing merges into the file as another process left it")))
         (snapshot-write pathname
                         '(:preferences :version 8
                           :model "first" :model "second"))
         (let ((plist (preferences--read configuration)))
           (test-assert (string= (getf plist :model) "first")
                        "duplicate preference keys use their first value"))
         (preferences-store configuration :turn-timestamps-p t)
         (test-assert (string= (getf (preferences-tests--file-plist configuration)
                                    :model)
                              "first")
                      "preference writes retain first-value duplicate semantics")
         (dolist (version '(1 2 3 4 5 6 7 9))
           (let ((form    (list ':preferences ':version version
                                ':model "gpt-5.6-luna"
                                ':reasoning-effort "high"))
                 (created nil))
             (snapshot-write pathname form)
             (test-assert
              (preferences-tests--warned-p
               (lambda () (setf created (preferences-tests--create configuration))))
              "an unsupported preference version is reported and ignored")
             (test-assert (string= (config :model created) *default-model*)
                          "an unsupported file leaves the default model in place")
             (test-assert (equal (snapshot-read pathname) form)
                          "loading an unsupported file preserves its stored data")))
         (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                          :external-format ':utf-8)
           (write-string "(:preferences :version 8 :model" stream))
         (let ((created nil))
           (test-assert
            (preferences-tests--warned-p
             (lambda () (setf created (preferences-tests--create configuration))))
            "a truncated file is reported and ignored")
           (setf (config :reasoning-traces-p created) nil)
           (test-assert (equal (preferences-tests--file-plist configuration)
                               '(:reasoning-traces-p nil))
                        "the next store replaces a damaged file"))
        nil)))))

(-> test-preferences-unavailable-provider-selection () null)
(defun test-preferences-unavailable-provider-selection ()
  "Drop durable unavailable model/provider pairs during deferred startup validation."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-environment (("AUTOLITH_MODEL_PROVIDER" nil))
       (provider-selection-tests--with-providers
        (lambda ()
          (dolist (provider '("occ-dead" "selection-a"))
            (with-test-configuration (configuration)
              (snapshot-write (configuration-preferences-path configuration)
                              (list :preferences :version 8
                                    :model "dead-model" :model-provider provider))
              (let ((created (preferences-tests--create
                              configuration :defer-provider-validation-p t)))
                (test-assert
                 (handler-case
                     (progn (provider-bootstrap-configuration created) t)
                   (model-selection-error () nil))
                 "unavailable durable selections do not abort startup")
                (test-assert (string= (config :model created) *default-model*)
                             "the default model replaces an unavailable saved model")
                (test-assert (configuration-model-registration created)
                             "the fallback pair resolves to a registered provider")
                (test-assert (non-empty-string-p (config :saved-model-selection-notice created))
                             "bootstrap records a visible dropped-selection notice")
                (test-assert (string= (config :model-provider created) "")
                             "the stale provider is dropped with its saved model")))))))))
  nil)

(-> test-preferences-provider-selection-restoration () null)
(defun test-preferences-provider-selection-restoration ()
  "Recover per launch without overwriting preferences needed after re-registration."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-environment (("AUTOLITH_MODEL_PROVIDER" nil))
       (provider-selection-tests--with-providers
        (lambda ()
          (with-test-configuration (configuration)
            (let ((saved '(:preferences :version 8 :model "restored-model"
                           :model-provider "restored-provider" :future-key 17)))
              (snapshot-write (configuration-preferences-path configuration) saved)
              (dotimes (launch 2)
                (declare (ignorable launch))
                (let ((created (provider-bootstrap-configuration
                                (preferences-tests--create
                                 configuration :defer-provider-validation-p t))))
                  (test-assert (configuration-model-registration created)
                               "each unavailable launch uses a registered fallback")
                  (test-assert (non-empty-string-p (config :saved-model-selection-notice created))
                               "a fresh startup reports recovery again")
                  (test-assert (equal saved (snapshot-read
                                             (configuration-preferences-path configuration)))
                               "recovery leaves the durable selection and unrelated keys intact")))
              (register-provider "restored-provider" :family ':codex
                                 :models                     '("restored-model")
                                 :factory                    #'provider-selection-tests--factory)
              (let ((created (provider-bootstrap-configuration
                              (preferences-tests--create
                               configuration :defer-provider-validation-p t))))
                (test-assert (and (string= (config :model created) "restored-model")
                                  (string= (provider-registration-name
                                            (configuration-model-registration created))
                                           "restored-provider"))
                             "re-registering the provider restores the exact saved selection")
                (test-assert (not (non-empty-string-p
                                   (config :saved-model-selection-notice created)))
                             "a restored selection needs no recovery notice")))))))))
  nil)

(-> test-preferences-mixed-provider-selection () null)
(defun test-preferences-mixed-provider-selection ()
  "Validate recovered defaults and precedence using only local registrations."
  (preferences-tests--without-model-environment
   (lambda ()
     (provider-selection-tests--with-providers
      (lambda ()
        (dolist (case '(("dead-model" nil nil (:model-provider "selection-a") nil)
                        ("dead-model" nil "selection-a" nil nil)
                        ("dead-model" nil nil (:model-provider "chatgpt") "chatgpt")
                        ("dead-model" nil "chatgpt" nil "chatgpt")
                        ("shared" nil "selection-b" (:model-provider "selection-a") "selection-a")
                        ("shared" nil "selection-a" nil "selection-a")
                        ("dead-model" "shared" "selection-b" (:model-provider "selection-a") "selection-a")
                        ("dead-model" "dead-model" "occ-dead"
                         (:model "shared" :model-provider "selection-a") "selection-a")
                        ("shared" "dead-model" "selection-a" nil nil)
                        ("shared" nil "selection-a" (:model "dead-model") nil)
                        ("shared" nil "occ-dead" nil nil)))
          (destructuring-bind (saved-model environment-model environment-provider overrides expected) case
            (with-test-configuration (configuration)
              (snapshot-write (configuration-preferences-path configuration)
                              (list :preferences :version 8 :model saved-model
                                    :model-provider "occ-dead"))
              (with-test-environment (("AUTOLITH_MODEL" environment-model)
                                      ("AUTOLITH_MODEL_PROVIDER" environment-provider))
                (let* ((created (apply #'preferences-tests--create configuration
                                       :defer-provider-validation-p t overrides))
                       (failure (handler-case
                                    (progn
                                      (setf created (provider-bootstrap-configuration created))
                                      nil)
                                  (setting-invalid (condition) condition))))
                  (if expected
                      (progn
                        (test-assert (null failure) "the winning mixed-source pair is valid")
                        (test-assert (and (string= (config :model created)
                                                  (if (string= expected "chatgpt")
                                                      *default-model* "shared"))
                                          (string= (config :model-provider created) expected)
                                          (string= (provider-registration-name
                                                    (configuration-model-registration created))
                                                   expected))
                                     "CLI overrides environment, which overrides the saved pair"))
                      (test-assert (typep failure 'setting-invalid)
                                   "an unusable explicit selection fails before provider construction")))))))))))
  nil)

(-> test-preferences-provider-selection-controls () null)
(defun test-preferences-provider-selection-controls ()
  "Preserve valid saved pairs and reject explicit invalid selections."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-environment (("AUTOLITH_MODEL_PROVIDER" nil))
       (provider-selection-tests--with-providers
        (lambda ()
          (with-test-configuration (configuration)
            (snapshot-write (configuration-preferences-path configuration)
                            '(:preferences :version 8 :model "shared"
                              :model-provider "selection-a"))
            (let ((created (provider-bootstrap-configuration
                            (preferences-tests--create
                             configuration :defer-provider-validation-p t))))
              (test-assert (and (string= (config :model created) "shared")
                                (string= (config :model-provider created) "selection-a")
                                (configuration-model-registration created))
                           "a valid saved provider retains its exact model")
              (test-assert (not (non-empty-string-p
                                 (config :saved-model-selection-notice created)))
                           "valid saved selections need no recovery notice"))
            (dolist (overrides '((:model "dead-model" :model-provider "occ-dead")
                                 (:model "dead-model" :model-provider "selection-a")))
              (test-assert
               (handler-case
                   (progn (provider-bootstrap-configuration
                           (apply #'preferences-tests--create configuration
                                  :defer-provider-validation-p t overrides))
                          nil)
                 (setting-invalid () t)
                 (configuration-error () t))
               "invalid explicit launch choices fail instead of falling back"))
            (with-test-environment (("AUTOLITH_MODEL" "dead-model")
                                    ("AUTOLITH_MODEL_PROVIDER" "occ-dead"))
              (test-assert
               (handler-case
                   (progn (provider-bootstrap-configuration
                           (preferences-tests--create
                            configuration :defer-provider-validation-p t))
                          nil)
                 (setting-invalid () t))
               "invalid environment choices fail instead of falling back")
              (let ((created (provider-bootstrap-configuration
                              (preferences-tests--create
                               configuration :model "shared" :model-provider "selection-a"
                               :defer-provider-validation-p t))))
                (test-assert (and (string= (config :model created) "shared")
                                  (string= (config :model-provider created) "selection-a"))
                             "explicit launch choices take precedence over environment")))))))))
  nil)

(-> test-preferences-provider-aliases () null)
(defun test-preferences-provider-aliases ()
  "Accept legacy provider aliases from both durable and environment selections."
  (preferences-tests--without-model-environment
   (lambda ()
     (dolist (alias '("codex" "openai"))
       (dolist (source '(:durable :environment))
         (with-test-configuration (configuration)
           (snapshot-write (configuration-preferences-path configuration)
                           (list :preferences :version 8 :model *default-model*
                                 :model-provider alias))
           (with-test-environment (("AUTOLITH_MODEL_PROVIDER"
                                    (and (eq source ':environment) alias)))
             (let ((created (preferences-tests--create
                             configuration :defer-provider-validation-p t)))
               (test-assert
                (handler-case (progn (provider-bootstrap-configuration created) t)
                  (setting-invalid () nil))
                "valid legacy aliases do not fail deferred validation")
               (test-assert
                (and (string= (config :model-provider created) alias)
                     (configuration-model-registration created)
                     (not (non-empty-string-p
                           (config :saved-model-selection-notice created))))
                "valid aliases retain their selection without a recovery notice"))))))))
  nil)
