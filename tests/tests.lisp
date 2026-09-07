(in-package #:autolith)

;;;; -- Test Entry --

(-> test-configuration-source-platform-reading () null)
(defun test-configuration-source-platform-reading ()
  "Test settings source reads under each supported platform feature set."
  (let ((settings-path
          (merge-pathnames
           "src/configuration/settings.lisp"
           (asdf:system-source-directory :autolith)))
        (native-features
          (remove-if
           (lambda (feature)
             (member feature
                     '(:linux :darwin :macos :macosx :bsd
                       :freebsd :netbsd :openbsd)))
           *features*)))
    (dolist (platform-features
             '((:linux) (:darwin :bsd) (:bsd) nil))
      (test-assert
       (handler-case
           (let ((*features* (append platform-features native-features))
                 (*read-eval* nil))
             (with-open-file (stream settings-path
                                     :direction ':input
                                     :external-format ':utf-8)
               (loop until (eq (read stream nil ':eof) ':eof)))
             t)
         (error ()
           nil))
       "configuration source reads with each supported platform feature set")))
  nil)


(-> tests--restore-environment (string (or null string)) null)
(defun tests--restore-environment (name value)
  "Restore environment variable NAME to VALUE."
  (if value
      (sb-posix:setenv name value 1)
      (sb-posix:unsetenv name))
  nil)


(-> test-context-window-environment () null)
(defun test-context-window-environment ()
  "Test that context-window overrides accept only positive integers."
  (let ((variable "AUTOLITH_CONTEXT_WINDOW")
        (saved    (uiop:getenv "AUTOLITH_CONTEXT_WINDOW")))
    (unwind-protect
         (progn
           (sb-posix:setenv variable "200000" 1)
           (test-assert
            (= (configuration--context-window-for "unknown-model") 200000)
            "AUTOLITH_CONTEXT_WINDOW accepts a positive integer")
           (dolist (invalid '("200k" "abc" "0" "-1"))
             (sb-posix:setenv variable invalid 1)
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
  (let ((variable "AUTOLITH_MODEL")
        (saved    (uiop:getenv "AUTOLITH_MODEL"))
        (root     (asdf:system-source-directory :autolith)))
    (unwind-protect
         (progn
           (sb-posix:setenv variable "gpt-5.6-typo" 1)
           (test-assert
            (handler-case
                (progn
                  (configuration-create :source-root root
                                        :working-directory root)
                  nil)
              (configuration-error ()
                t))
            "AUTOLITH_MODEL rejects unsupported models")
           (let ((configuration
                   (configuration-create
                    :source-root root
                    :working-directory root
                    :defer-provider-validation-p t)))
             (test-assert
              (handler-case
                  (progn
                    (provider-bootstrap-configuration configuration)
                    nil)
                (configuration-error ()
                  t))
              "deferred model validation rejects unsupported models after bootstrap")))
      (tests--restore-environment variable saved)))
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
  (let* ((source-root (asdf:system-source-directory :autolith))
         (home (user-homedir-pathname))
         (direct-variable "AUTOLITH_TEST_XDG_DIRECTORY")
         (cases
           (list
            (list "XDG_CONFIG_HOME"
                  #'configuration-config-root
                  (merge-pathnames ".config/autolith/" home))
            (list "XDG_DATA_HOME"
                  #'configuration-data-root
                  (merge-pathnames ".local/share/autolith/" home))
            (list "XDG_STATE_HOME"
                  #'configuration-state-root
                  (merge-pathnames ".local/state/autolith/" home))
            (list "XDG_CACHE_HOME"
                  #'configuration-cache-root
                  (merge-pathnames ".cache/autolith/" home))))
         (saved
           (mapcar (lambda (name) (cons name (uiop:getenv name)))
                   (cons direct-variable (mapcar #'first cases)))))
    (unwind-protect
         (progn
           (let* ((absolute (merge-pathnames "xdg-home/" source-root))
                  (fallback (merge-pathnames "xdg-fallback/" source-root)))
             (sb-posix:setenv direct-variable (namestring absolute) 1)
             (test-assert
              (equal (environment-directory direct-variable fallback) absolute)
              "environment-directory accepts an absolute directory")
             (dolist (invalid '("" "relative/xdg-home"))
               (sb-posix:setenv direct-variable invalid 1)
               (test-assert
                (equal (environment-directory direct-variable fallback) fallback)
                "environment-directory rejects empty and relative directories"))
             (sb-posix:unsetenv direct-variable)
             (test-assert
              (equal (environment-directory direct-variable fallback) fallback)
              "environment-directory uses its fallback when the variable is absent"))
           (dolist (case cases)
             (destructuring-bind (variable accessor fallback) case
               (dolist (invalid '("" "relative/xdg-home"))
                 (sb-posix:setenv variable invalid 1)
                 (let ((configuration
                         (configuration-create
                          :source-root source-root
                          :working-directory source-root
                          :defer-provider-validation-p t)))
                   (test-assert
                    (equal (funcall accessor configuration) fallback)
                    (format nil "~A ignores empty and relative values" variable))))))
           (let ((state-home (merge-pathnames "xdg-state/" source-root)))
             (sb-posix:setenv "XDG_STATE_HOME" (namestring state-home) 1)
             (test-assert
              (equal
               (environment-api-key-credential-source--pathname "fixture")
               (merge-pathnames "autolith/fixture-auth.sexp" state-home))
              "environment API-key reporting includes one autolith state component")))
           (let* ((configuration (test-configuration))
                  (root (test-configuration-root configuration)))
             (unwind-protect
                  (progn
                    (configuration-ensure-directories configuration)
                    (test-assert
                     (every
                      (lambda (directory)
                        (= (logand
                            (sb-posix:stat-mode
                             (sb-posix:stat (namestring directory)))
                            #o777)
                           #o700))
                      (list (configuration-config-root configuration)
                            (configuration-data-root configuration)
                            (configuration-state-root configuration)
                            (configuration-cache-root configuration)))
                     "new XDG application roots have mode 0700"))
               (uiop:delete-directory-tree
                root :validate t :if-does-not-exist ':ignore)))
      (dolist (entry saved)
        (tests--restore-environment (first entry) (rest entry)))))
  nil)


(-> test-core-defaults () null)
(defun test-core-defaults ()
  "Test configuration defaults and basic JSON and presentation behavior."
  (let ((configuration (configuration-create
                        :source-root (asdf:system-source-directory :autolith)
                        :working-directory (asdf:system-source-directory :autolith))))
    (test-assert (string= (configuration-model configuration) "gpt-5.6-sol")
                 "the default model is gpt-5.6-sol")
    (let ((*default-model* "gpt-5.6-luna"))
      (test-assert
       (string= (configuration-model
                 (configuration-create
                  :source-root (asdf:system-source-directory :autolith)
                  :working-directory
                  (asdf:system-source-directory :autolith)))
                "gpt-5.6-luna")
       "live default parameters affect newly created configurations"))
    (test-assert (string= (configuration-model
                           (configuration-with-model configuration
                                                     "gpt-5.6-luna"))
                          "gpt-5.6-luna")
                 "model copies swap only the model")
    (test-assert (plusp (configuration-context-window configuration))
                 "the default model carries a catalog context window")
    (test-assert (= (configuration-context-window
                     (configuration-with-model configuration "gpt-5.6-terra"))
                    (provider-model-context-window-for "gpt-5.6-terra"))
                 "model copies recompute the context window from the catalog")
    (test-assert (plusp *default-context-window*)
                 "unknown models retain a conservative context window fallback")
    (test-assert (= (configuration-compaction-token-limit configuration)
                    (floor (* (configuration-context-window configuration)
                              (configuration-compaction-threshold-percent
                               configuration))
                           100))
                 "compaction triggers at the threshold share of the window")
    (test-assert (handler-case
                     (progn
                       (configuration-with-model configuration "gpt-4")
                       nil)
                   (configuration-error ()
                     t))
                 "model copies reject identifiers outside the 5.6 family")
    (let ((moved (configuration-with-working-directory configuration "tests")))
      (test-assert
       (equal (configuration-working-directory moved)
              (truename (merge-pathnames "tests/"
                                         (configuration-working-directory
                                          configuration))))
       "working-directory copies resolve relative existing directories")
      (test-assert
       (equal (configuration-source-root moved)
              (configuration-source-root configuration))
       "working-directory copies preserve unrelated configuration"))
    (test-assert
     (handler-case
         (progn
           (configuration-with-working-directory configuration "README.org")
           nil)
       (working-directory-error (condition)
         (eq (working-directory-error-stage condition) ':validation)))
     "working-directory copies reject files with a structured condition")
    (test-assert (string= (configuration-reasoning-effort configuration) "ultra")
                 "the default reasoning effort is ultra")
    (test-assert (not (configuration-immutable-p configuration))
                 "ordinary configuration enables active-image mutation tools")
    (test-assert
     (configuration-immutable-p
      (configuration-with-model
       (configuration--clone configuration :immutable-p t)
       "gpt-5.6-luna"))
     "configuration clones preserve immutable mode")
    (test-assert (string= (configuration-wire-effort configuration) "max")
                 "ultra maps to the provider max effort")
    (test-assert
     (string= (configuration-wire-effort
               (configuration-with-reasoning-effort configuration "none"))
              "none")
     "none is passed through as a provider reasoning effort")
    (test-assert (= (json-get (json-object "answer" 42) "answer") 42)
                 "JSON object access preserves values")
    (test-assert (vectorp (json-decode "[1,2,3]"))
                 "JSON arrays have one consistent vector representation")
    (let* ((decoded (json-decode
                     "{\"false\":false,\"true\":true,\"null\":null}"))
           (false-marker (gethash "false" decoded))
           (true-value (json-get decoded "true"))
           (null-value (json-get decoded "null")))
      (test-assert (eq false-marker *json-decoded-false*)
                   "JSON false retains a distinct internal marker")
      (test-assert (null (json-get decoded "false"))
                   "decoded JSON false remains false to ordinary object access")
      (test-assert (eq true-value t)
                   "JSON true retains its ordinary Lisp representation")
      (test-assert (null null-value)
                   "JSON null retains its ordinary Lisp representation")
      (test-assert
       (string= (json-encode (json-array false-marker true-value null-value))
                "[false,true,null]")
       "JSON false and null survive a decode and re-encode round trip"))
    (let* ((value (json-object "text" "příliš žluťoučký"))
           (encoded (json-encode value))
           (octets (json-encode-utf8 value)))
      (test-assert
       (equalp octets
               (sb-ext:string-to-octets encoded :external-format ':utf-8))
       "direct UTF-8 JSON encoding preserves the compact wire representation")
      (test-assert
       (subtypep (array-element-type octets) '(unsigned-byte 8))
       "direct UTF-8 JSON encoding returns octets without a wide string body"))
    (let ((*print-readably* t))
      (test-assert
       (search "Condition text."
               (bounded-string
                (make-condition 'simple-error
                                :format-control "Condition text."
                                :format-arguments nil)))
       "bounded presentation renders unreadable conditions safely")))
  nil)
