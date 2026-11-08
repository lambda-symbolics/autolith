(in-package #:autolith)

;;;; -- Test Entry --

(-> test-configuration-source-platform-reading () null)
(defun test-configuration-source-platform-reading ()
  "Test the POSIX adapter source reads under each supported platform feature set."
  (let ((source-path
          (merge-pathnames
           "src/core/platform-posix.lisp"
           (asdf:system-source-directory :autolith)))
        (native-features
          (remove-if
           (lambda (feature)
             (member feature
                     '(:linux :darwin :macos :macosx :bsd
                       :freebsd :netbsd :openbsd)))
           *features*)))
    (with-test-fixture (':posix-adapter "reading the POSIX adapter source")
      (dolist (platform-features
               '((:linux) (:darwin :bsd) (:bsd) nil))
        (test-assert
         (handler-case
             (let ((*features*  (append platform-features native-features))
                   (*read-eval* nil))
               (with-open-file (stream source-path
                                       :direction       ':input
                                       :external-format ':utf-8)
                 (loop until (eq (read stream nil ':eof) ':eof)))
               t)
           (error ()
             nil))
         "POSIX adapter source reads with each supported platform feature set"))))
  nil)

(-> tests--restore-environment (string (or null string)) null)
(defun tests--restore-environment (name value)
  "Restore environment variable NAME to VALUE."
  (if value
      (platform-setenv name value)
      (platform-unsetenv name))
  nil)

(-> test-context-window-environment () null)
(defun test-context-window-environment ()
  "Test that context-window overrides accept only positive integers."
  (let ((variable "AUTOLITH_CONTEXT_WINDOW")
        (saved    (uiop:getenv "AUTOLITH_CONTEXT_WINDOW")))
    (unwind-protect
         (progn
           (platform-setenv variable "200000")
           (test-assert
            (= (configuration--context-window-for "unknown-model") 200000)
            "AUTOLITH_CONTEXT_WINDOW accepts a positive integer")
           (dolist (invalid '("200k" "abc" "0" "-1"))
             (platform-setenv variable invalid)
             (test-assert
              (handler-case
                  (progn
                    (configuration--context-window-for "unknown-model")
                    nil)
                (configuration-error ()
                  t))
              (format nil "AUTOLITH_CONTEXT_WINDOW rejects ~S" invalid))))
      (tests--restore-environment variable saved)))
  nil)

(-> test-model-environment-validation () null)
(defun test-model-environment-validation ()
  "Test that configured models are validated after provider registration."
  (with-test-configuration (fixture)
    (let ((snapshot (provider--registry-snapshot)))
      (unwind-protect
           (with-test-environment (("AUTOLITH_MODEL" "gpt-5.6-typo"))
             (flet ((configuration (&key defer-provider-validation-p)
                      "Create a configuration without reading user preferences or model caches."
                      (configuration-create
                       :source-root                 (config :source-root fixture)
                       :working-directory           (config :working-directory fixture)
                       :cache-root                  (config :cache-root fixture)
                       :durable-p                   nil
                       :defer-provider-validation-p defer-provider-validation-p)))
               (test-assert
                (handler-case
                    (progn (configuration) nil)
                  (setting-error () t))
                "AUTOLITH_MODEL rejects unsupported models")
               (test-assert
                (handler-case
                    (progn
                      (provider-bootstrap-configuration
                       (configuration :defer-provider-validation-p t))
                      nil)
                  (setting-error () t))
                "deferred model validation rejects unsupported models after bootstrap")))
        (provider--registry-restore snapshot))))
  nil)

(-> test-text-line-splitting () null)
(defun test-text-line-splitting ()
  "Test line splitting distinguishes CRLF delimiters from a final bare CR."
  (test-assert
   (equalp (text--split-lines (format nil "first~C~Csecond" #\Return #\Newline))
           #("first" "second"))
   "line splitting removes CRLF delimiters")
  (let ((content (format nil "last~C" #\Return)))
    (test-assert
     (equalp (text--split-lines content) (vector content))
     "line splitting preserves a final bare carriage return"))
  nil)

(-> test-xdg-directory-selection () null)
(defun test-xdg-directory-selection ()
  "Test XDG roots reject invalid values, report state, and use private modes."
  (let* ((source-root     (asdf:system-source-directory :autolith))
         (home            (user-homedir-pathname))
         (direct-variable "AUTOLITH_TEST_XDG_DIRECTORY")
         ;; Each case names the variable, the root it selects, and the
         ;; directory the XDG convention chooses without it; the host's own
         ;; fallback is observed with the variable absent.
         (cases
           (list
            (list "XDG_CONFIG_HOME"
                  (lambda (configuration) (config :config-root configuration))
                  (merge-pathnames ".config/autolith/" home))
            (list "XDG_DATA_HOME"
                  (lambda (configuration) (config :data-root configuration))
                  (merge-pathnames ".local/share/autolith/" home))
            (list "XDG_STATE_HOME"
                  (lambda (configuration) (config :state-root configuration))
                  (merge-pathnames ".local/state/autolith/" home))
            (list "XDG_CACHE_HOME"
                  (lambda (configuration) (config :cache-root configuration))
                  (merge-pathnames ".cache/autolith/" home))))
         (saved
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   (cons direct-variable (mapcar #'first cases)))))
    (unwind-protect
         (progn
           (let* ((absolute (merge-pathnames "xdg-home/" source-root))
                  (fallback (merge-pathnames "xdg-fallback/" source-root)))
             (platform-setenv direct-variable (namestring absolute))
             (test-assert
              (equal (environment-directory direct-variable fallback) absolute)
              "environment-directory accepts an absolute directory")
             (dolist (invalid '("" "relative/xdg-home"))
               (platform-setenv direct-variable invalid)
               (test-assert
                (equal (environment-directory direct-variable fallback) fallback)
                "environment-directory rejects empty and relative directories"))
             (platform-unsetenv direct-variable)
             (test-assert
              (equal (environment-directory direct-variable fallback) fallback)
              "environment-directory uses its fallback when the variable is absent"))
           (dolist (case cases)
             (destructuring-bind (variable accessor conventional) case
               (flet ((root ()
                        "Return the root ACCESSOR selects for a fresh configuration."
                        (funcall accessor
                                 (configuration-create
                                  :source-root                 source-root
                                  :working-directory           source-root
                                  :durable-p                   nil
                                  :defer-provider-validation-p t))))
                 (platform-unsetenv variable)
                 (let ((fallback (root)))
                   (with-test-fixture (':posix-adapter
                                       (format nil "the ~A convention" variable))
                     (test-assert
                      (equal fallback conventional)
                      (format nil "~A falls back to the XDG convention" variable)))
                   (dolist (invalid '("" "relative/xdg-home"))
                     (platform-setenv variable invalid)
                     (test-assert
                      (equal (root) fallback)
                      (format nil "~A ignores empty and relative values" variable)))))))
           (let ((state-home (merge-pathnames "xdg-state/" source-root)))
             (platform-setenv "XDG_STATE_HOME" (namestring state-home))
             (test-assert
              (equal
               (environment-api-key-credential-source--pathname "fixture")
               (merge-pathnames "autolith/fixture-auth.sexp" state-home))
              "environment API-key reporting includes one autolith state component")))
         (let* ((configuration (test-configuration))
                (root          (test-configuration-root configuration)))
           (unwind-protect
                (progn
                  (configuration-ensure-directories configuration)
                  (test-assert
                   (every
                    (lambda (directory)
                      (test-fixture-permissions-p
                       *platform* directory ':private-directory))
                    (list (config :config-root configuration)
                          (config :data-root configuration)
                          (config :state-root configuration)
                          (config :cache-root configuration)))
                   "new XDG application roots are private to the user")
                  ;; A file that plain OPEN creates below a private root
                  ;; must stay usable by its creator: Windows gives it the
                  ;; root's inheritable entries, POSIX the process umask.
                  (let ((created (merge-pathnames "created-inside.txt"
                                                  (config :state-root configuration))))
                    (with-open-file (stream created
                                            :direction         ':output
                                            :if-exists         ':supersede
                                            :if-does-not-exist ':create
                                            :external-format   ':utf-8)
                      (write-line "inside" stream))
                    (test-assert
                     (and (string= (with-open-file (stream created :external-format ':utf-8)
                                     (read-line stream))
                                   "inside")
                          (eq (platform-file-status-kind
                               (platform-path-status *platform* created))
                              ':file))
                     "files created below a private root stay readable by their creator")))
             (platform-delete-directory-tree
              *platform*
              root :validate t :if-does-not-exist ':ignore)))
      (dolist (entry saved)
        (tests--restore-environment (first entry) (rest entry)))))
  nil)

(-> test-core-defaults () null)
(defun test-core-defaults ()
  "Test configuration defaults and basic JSON and presentation behavior."
  (let ((configuration (configuration-create
                        :source-root       (asdf:system-source-directory :autolith)
                        :working-directory (asdf:system-source-directory :autolith)
                        :durable-p         nil)))
    (test-assert (string= (config :model configuration) "gpt-6.1-sol")
                 "the default model is gpt-6.1-sol")
    (let ((*default-model* "gpt-5.6-luna"))
      (test-assert
       (string= (config :model
                 (configuration-create
                  :source-root (asdf:system-source-directory :autolith)
                  :working-directory
                  (asdf:system-source-directory :autolith)
                  :durable-p   nil))
                "gpt-5.6-luna")
       "live default parameters affect newly created configurations"))
    (test-assert (string= (config :model
                           (configuration-copy configuration :model
                                                     "gpt-5.6-luna"))
                          "gpt-5.6-luna")
                 "model copies swap only the model")
    (test-assert (plusp (config :context-window configuration))
                 "the default model carries a catalog context window")
    (test-assert (= (config :context-window
                     (configuration-copy configuration :model "gpt-5.6-terra"))
                    (provider-model-context-window-for "gpt-5.6-terra"))
                 "model copies recompute the context window from the catalog")
    (test-assert (plusp *default-context-window*)
                 "unknown models retain a conservative context window fallback")
    (test-assert (= (configuration-compaction-token-limit configuration)
                    (floor (* (config :context-window configuration)
                              (config :compaction-threshold-percent
                               configuration))
                           100))
                 "compaction triggers at the threshold share of the window")
    (test-assert (handler-case
                     (progn
                       (configuration-copy configuration :model "gpt-4")
                       nil)
                   (setting-error ()
                     t))
                 "model copies reject identifiers outside the 5.6 family")
    (let ((moved (configuration-copy configuration :working-directory "tests")))
      (test-assert
       (equal (config :working-directory moved)
              (truename (merge-pathnames "tests/"
                                         (config :working-directory
                                          configuration))))
       "working-directory copies resolve relative existing directories")
      (test-assert
       (equal (config :source-root moved)
              (config :source-root configuration))
       "working-directory copies preserve unrelated configuration"))
    (test-assert
     (handler-case
         (progn
           (configuration-copy configuration :working-directory "README.org")
           nil)
       (working-directory-error (condition)
         (eq (working-directory-error-stage condition) ':validation)))
     "working-directory copies reject files with a structured condition")
    (test-assert (string= (config :reasoning-effort configuration) "high")
                 "the default reasoning effort is high")
    (test-assert (not (config :immutable-p configuration))
                 "ordinary configuration enables active-image mutation tools")
    (test-assert
     (config :immutable-p
      (configuration-copy
       (configuration-copy configuration :immutable-p t) :model
       "gpt-5.6-luna"))
     "configuration clones preserve immutable mode")
    (test-assert (string= (configuration-wire-effort configuration) "high")
                 "the default effort reaches the provider unchanged")
    (test-assert
     (string= (configuration-wire-effort
               (configuration-copy configuration :reasoning-effort "ultra"))
              "max")
     "ultra maps to the provider max effort")
    (test-assert
     (handler-case
         (progn
           (configuration-copy configuration
                               :model            "gpt-5.6-terra"
                               :reasoning-effort "none")
           nil)
       (setting-invalid ()
         t))
     "Codex model metadata rejects unsupported reasoning levels")
    (let ((*print-readably* t))
      (test-assert
       (search "Condition text."
               (bounded-string
                (make-condition 'simple-error
                                :format-control   "Condition text."
                                :format-arguments nil)))
       "bounded presentation renders unreadable conditions safely")))
  nil)

(-> test-bounded-string-limit () null)
(defun test-bounded-string-limit ()
  "Test a truncated string stays within its limit, its notice included, and says what it left out."
  (loop for (limit uri) in '((100 nil) (100 "conversation:current") (40 nil))
        for text = (bounded-string (make-string 5000 :initial-element #\x)
                                   :limit                 limit
                                   :overflow-uri-function (and uri (constantly uri)))
        do (test-assert (and (<= (length text) limit)
                             (search "characters omitted" text)
                             (or (null uri) (search uri text)))
                        (format nil "a ~D-character limit holds with~:[out~;~] an overflow URI" limit uri)))
  nil)

(-> test-make-identifier () null)
(defun test-make-identifier ()
  "Test that identifiers are distinct random version 4 UUIDs."
  (let ((identifiers (loop repeat 256 collect (make-identifier))))
    (test-assert
     (every (lambda (identifier)
              (and (= (length identifier) 36)
                   (every (lambda (index)
                            (char= (char identifier index) #\-))
                          '(8 13 18 23))
                   (every (lambda (character)
                            (or (char= character #\-)
                                (find character "0123456789abcdef")))
                          identifier)
                   (char= (char identifier 14) #\4)
                   (find (char identifier 19) "89ab")))
            identifiers)
     "identifiers are lowercase version 4 UUIDs with the RFC variant")
    (test-assert (= (length identifiers)
                    (length (remove-duplicates identifiers :test #'string=)))
                 "identifiers do not repeat"))
  nil)
