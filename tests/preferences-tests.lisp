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
           (dolist (case '((:reasoning-traces-p nil) (:compact-view-p t)
                           (:turn-timestamps-p nil) (:cache-miss-notices-p nil)
                           (:simple-technical-english-p nil)
                           (:session-title-generation-p t) (:fullscreen-p nil)
                           (:codex-fast-mode-p nil) (:permission-mode nil)))
             (destructuring-bind (name expected) case
               (test-assert (eq (config name created) expected)
                            (format nil "a missing file leaves ~(~A~) at its default" name))))
           (test-assert (string= (config :model created) *default-model*)
                        "a missing file leaves the default model")
           (test-assert (not (probe-file pathname))
                        "reading defaults does not create the file")
           (setf (config :reasoning-traces-p created) t)
           (multiple-value-bind (plist version-8-p)
               (preferences-tests--file-plist configuration)
             (test-assert (and version-8-p (eq (getf plist :reasoning-traces-p) t))
                          "a durable change writes a version 8 record"))
           (setf (config :model created) "gpt-5.6-luna"
                 (config :reasoning-effort created) "high"
                 (config :permission-mode created) ':ask)
           (let ((reloaded (preferences-tests--create configuration)))
             (test-assert (and (config :reasoning-traces-p reloaded)
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
                           :compact-view-p nil :future-key 7))
         (let ((created (preferences-tests--create configuration)))
           (test-assert (string= (config :model created) *default-model*)
                        "an unsupported durable model is dropped")
           (test-assert (string= (config :reasoning-effort created)
                                 *default-reasoning-effort*)
                        "an unsupported durable effort is dropped")
           (setf (config :turn-timestamps-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (= (getf plist :future-key) 7)
                               (not (getf plist :compact-view-p))
                               (eq (getf plist :turn-timestamps-p) t))
                          "storing one value keeps unknown and unrelated keys"))
           (snapshot-write pathname '(:preferences :version 8 :cache-miss-notices-p t))
           (setf (config :fullscreen-p created) t)
           (let ((plist (preferences-tests--file-plist configuration)))
             (test-assert (and (eq (getf plist :cache-miss-notices-p) t)
                               (eq (getf plist :fullscreen-p) t)
                               (null (member :turn-timestamps-p plist)))
                          "storing merges into the file as another process left it")))
         (snapshot-write pathname
                         '(:preferences :version 8
                           :model "first" :model "second"))
         (let ((plist (preferences--read configuration)))
           (test-assert (string= (getf plist :model) "first")
                        "duplicate preference keys use their first value"))
         (preferences-store configuration :compact-view-p t)
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
           (setf (config :compact-view-p created) nil)
           (test-assert (equal (preferences-tests--file-plist configuration)
                               '(:compact-view-p nil))
                        "the next store replaces a damaged file"))
        nil)))))

(-> test-preferences-xdg-migration () null)
(defun test-preferences-xdg-migration ()
  "Move durable choices once, preserving unknown keys and existing config values."
  (preferences-tests--without-model-environment
   (lambda ()
     (with-test-configuration (configuration)
       (let* ((legacy (merge-pathnames "preferences.sexp" (config :state-root configuration)))
              (target (configuration-preferences-path configuration))
              (old    '(:preferences :version 8 :model "gpt-5.6-luna"
                        :compact-view-p nil :telemetry-enabled-p nil :future-key 7)))
         (snapshot-write legacy old)
         (let ((created (preferences-tests--create configuration)))
           (test-assert (and (string= (config :model created) "gpt-5.6-luna")
                             (not (config :compact-view-p created))
                             (not (config :telemetry-enabled-p created)))
                        "upgrade loads model, presentation and telemetry choices")
           (test-assert (equal (snapshot-read target) old)
                        "migration retains unknown configuration keys")
           (test-assert (not (probe-file legacy))
                        "successful migration consumes the legacy preferences")
           (setf (config :turn-timestamps-p created) t)
           (test-assert (and (getf (preferences-tests--file-plist configuration) :turn-timestamps-p)
                             (not (probe-file legacy)))
                        "later changes write only to config"))
         (snapshot-write legacy '(:preferences :version 8 :model "stale"))
         (test-assert (string= (config :model (preferences-tests--create configuration)) "gpt-5.6-luna")
                      "existing config preferences take precedence over old state")
         (test-assert (= (getf (preferences-tests--file-plist configuration) :future-key) 7)
                      "repeated startup preserves migrated unknown values")
         (test-assert (not (probe-file legacy)) "a superseded source is consumed too")
         (delete-file target)
         (test-assert (null (preferences-load-values configuration))
                      "deleting config cannot resurrect legacy preferences")
         (test-assert (not (probe-file target)) "a completed migration is not repeated")))))
  nil)

(-> test-preferences-xdg-migration-failure () null)
(defun test-preferences-xdg-migration-failure ()
  "Keep loading and saving the original when config creation or publication fails."
  (dolist (stage '(:root-creation :publication))
    (with-test-configuration (configuration)
      (let* ((legacy         (merge-pathnames "preferences.sexp" (config :state-root configuration)))
             (target         (configuration-preferences-path configuration))
             (original-store (symbol-function 'preferences--store))
             (original-root  (symbol-function 'configuration-ensure-root-directory))
             (warnings       0))
        (snapshot-write legacy '(:preferences :version 8 :future-key 7 :compact-view-p nil))
        (test-call-with-function-replacements
         (list (list 'preferences--store
                     (lambda (configuration &rest options &key (pathname target) &allow-other-keys)
                       (if (and (eq stage ':publication) (equal pathname target))
                           (error 'file-error :pathname target)
                           (apply original-store configuration options))))
               (list 'configuration-ensure-root-directory
                     (lambda (configuration root)
                       (if (and (eq stage ':root-creation) (eq root ':config-root))
                           (error 'file-error :pathname (config root configuration))
                           (funcall original-root configuration root)))))
         (lambda ()
           (handler-bind ((preferences-migration-warning
                            (lambda (condition)
                              (incf warnings)
                              (test-assert (and (equal (preferences-migration-warning-source condition) legacy)
                                                (equal (preferences-migration-warning-target condition) target))
                                           "migration warnings identify retained and desired files")
                              (muffle-warning condition))))
             (test-assert (= (getf (preferences-load-values configuration) :future-key) 7)
                          "failed migration loads the retained values")
             (preferences-store configuration :turn-timestamps-p t)
             (test-assert (getf (preferences--form->plist (snapshot-read legacy)) :turn-timestamps-p)
                          "failed migration saves changes to the original file"))))
        (test-assert (and (= warnings 2) (not (probe-file target)))
                     "failed publication warns and does not create an empty replacement")
        (test-assert (getf (preferences-load-values configuration) :turn-timestamps-p)
                     "a later successful migration carries forward fallback writes")
        (test-assert (and (probe-file target) (not (probe-file legacy)))
                     "a later startup completes the move"))))
  nil)

(-> test-preferences-xdg-malformed-legacy () null)
(defun test-preferences-xdg-malformed-legacy ()
  "Preserve malformed legacy bytes and permit an explicit fresh configuration choice."
  (with-test-configuration (configuration)
    (let ((legacy (merge-pathnames "preferences.sexp" (config :state-root configuration)))
          (target (configuration-preferences-path configuration)))
      (ensure-directories-exist legacy)
      (with-open-file (stream legacy :direction ':output :external-format ':utf-8)
        (write-string "(:preferences :version 8 :model" stream))
      (let ((bytes (uiop:read-file-string legacy)))
        (test-assert (preferences-tests--warned-p (lambda () (preferences-load-values configuration)))
                     "malformed legacy preferences are reported")
        (test-assert (and (not (probe-file target)) (string= bytes (uiop:read-file-string legacy)))
                     "a failed legacy read leaves the original untouched")
        (preferences-store configuration :compact-view-p nil)
        (test-assert (equal (preferences-tests--file-plist configuration) '(:compact-view-p nil))
                     "an explicit choice can start a fresh config file")
        (test-assert (not (probe-file legacy))
                     "an explicit fresh choice completes migration without stale fallback"))))
  nil)

(-> test-preferences-xdg-migration-cleanup () null)
(defun test-preferences-xdg-migration-cleanup ()
  "Retry interrupted source removal without reimporting stale preferences."
  (with-test-configuration (configuration)
    (let* ((legacy            (merge-pathnames "preferences.sexp" (config :state-root configuration)))
           (target            (configuration-preferences-path configuration))
           (original-transact (symbol-function 'sexp-store:store-transact))
           (warnings          0))
      (snapshot-write legacy '(:preferences :version 8 :future-key 7))
      (test-call-with-function-replacements
       (list (list 'sexp-store:store-transact
                   (lambda (store update &key publish)
                     (funcall original-transact store update
                              :publish (lambda (committed)
                                         (declare (ignore committed))
                                         (when publish
                                           (error 'file-error :pathname legacy)))))))
       (lambda ()
         (handler-bind ((preferences-migration-warning
                          (lambda (condition)
                            (incf warnings)
                            (muffle-warning condition))))
           (test-assert (= (getf (preferences-load-values configuration) :future-key) 7)
                        "published preferences are usable after interrupted removal")
           (preferences-store configuration :future-key 9))))
      (test-assert (and (= warnings 2) (probe-file legacy) (probe-file target))
                   "cleanup failure preserves the source and reports each retry")
      (test-assert (= (getf (preferences-load-values configuration) :future-key) 9)
                   "cleanup retry keeps changes made after publication")
      (test-assert (not (probe-file legacy)) "retry removes the old file")
      (delete-file target)
      (test-assert (null (preferences-load-values configuration))
                   "cache or config removal cannot repeat the import")))
  (with-test-configuration (configuration)
    (setf configuration (configuration-copy configuration :config-root (config :state-root configuration)))
    (preferences-store configuration :future-key 7)
    (test-assert (= (getf (preferences-load-values configuration) :future-key) 7)
                 "identical roots do not delete preferences or reacquire the same lock"))
  nil)

(-> test-xdg-file-migration-concurrent () null)
(defun test-xdg-file-migration-concurrent ()
  "Serialize competing migration readers and return the published data to all."
  (dolist (kind '(:preferences :provider-cache))
    (with-test-configuration (configuration)
      (let* ((preferences-p (eq kind ':preferences))
             (legacy        (merge-pathnames (if preferences-p "preferences.sexp" "provider-models.sexp")
                                             (config :state-root configuration)))
             (reader        (if preferences-p #'preferences-load-values #'provider--read-model-cache))
             (expected      (if preferences-p '(:future-key 7) '(:models :version 1)))
             (threads       nil))
        (snapshot-write legacy (if preferences-p
                                   (preferences--plist->form expected)
                                   expected))
        (call-with-file-lock
         (make-pathname :type "lock" :defaults legacy)
         (lambda ()
           (setf threads
                 (loop repeat 8
                       collect (bt:make-thread
                                (lambda ()
                                  (handler-case (funcall reader configuration)
                                    (error (condition) condition)))
                                :name "XDG file migration reader")))))
        (test-assert (every (lambda (result) (equal result expected))
                            (mapcar #'bt:join-thread threads))
                     "all competing readers get the same complete data")
        (test-assert (not (probe-file legacy)) "only the new location exists after the move"))))
  nil)

(-> test-preferences-xdg-fallback-handoff () null)
(defun test-preferences-xdg-fallback-handoff ()
  "Follow a competing completed migration before using a selected fallback file."
  (dolist (operation '(:read :write))
    (with-test-configuration (configuration)
      (let* ((legacy (merge-pathnames "preferences.sexp" (config :state-root configuration)))
             (target (configuration-preferences-path configuration))
             (result nil))
        (snapshot-write legacy (preferences--plist->form '(:future-key 7)))
        (test-call-with-function-replacements
         (list (list 'preferences--migrate-legacy
                     (lambda (configuration &key recover-read-error)
                       (declare (ignore configuration recover-read-error))
                       ;; A competing migrator finishes after this caller selects fallback.
                       (snapshot-write target (preferences--plist->form '(:future-key 7)))
                       (delete-file legacy)
                       legacy)))
         (lambda ()
           (setf result (if (eq operation ':read)
                            (preferences-load-values configuration)
                            (preferences-store configuration :new-key 9)))))
        (if (eq operation ':read)
            (test-assert (equal result '(:future-key 7))
                         "a delayed fallback reader follows the published file")
            (test-assert (= (getf (preferences-load-values configuration) :new-key) 9)
                         "a delayed fallback writer merges into the published file"))
        (test-assert (= (getf (preferences-load-values configuration) :future-key) 7)
                     "concurrent publication preserves unknown keys")
        (test-assert (not (probe-file legacy)) "fallback cannot recreate the consumed file"))))
  nil)

(-> test-legacy-xdg-migration-removal () null)
(defun test-legacy-xdg-migration-removal ()
  "Require removal of temporary XDG compatibility code at the 0.68.0 boundary."
  (test-assert (version< *autolith-version* "0.68.0")
               "At 0.68.0 remove legacy XDG root selection and choice caches, preferences/model-cache migration and conversation-cache legacy sidecars, then remove this test.")
  nil)

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
