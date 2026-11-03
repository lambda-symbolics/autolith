(in-package #:autolith)

;;;; -- Reusable Fixture Tests --

(define-condition test-fixture-error (error)
  ()
  (:documentation "A deliberate failure used to exercise fixture unwinding.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Expected fixture failure." stream))))

(-> test-run-temporary-root-cleanup () null)
(defun test-run-temporary-root-cleanup ()
  "Test scoped run ownership, nested cleanup, thread visibility and every exit."
  (let ((unrelated (test-make-temporary-root))
        (previous  (sb-ext:symbol-global-value '*test-temporary-root*)))
    (unwind-protect
         (dolist (exit '(:normal :error :throw))
           (let ((saved-root         nil)
                 (configuration-root nil))
             (test-assert
              (equal
               (handler-case
                   (catch 'fixture-exit
                     (multiple-value-list
                      (test-call-with-temporary-root
                       (lambda (root)
                         (setf saved-root root
                               configuration-root
                               (test-configuration-root (test-configuration)))
                         (test-assert (uiop:subpathp configuration-root root)
                                      "configuration roots belong to their run")
                         (let ((thread-root
                                 (sb-thread:join-thread
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (test-configuration-root (test-configuration)))
                                   :name "fixture-root-probe"))))
                           (test-assert (uiop:subpathp thread-root root)
                                        "child thread fixtures share run ownership"))
                         (let ((inner-root nil))
                           (test-call-with-temporary-root
                            (lambda (inner)
                              (setf inner-root inner)
                              (test-assert (uiop:subpathp inner root)
                                           "nested runs belong to the enclosing run")
                              (test-configuration)))
                           (test-assert (not (probe-file inner-root))
                                        "nested cleanup removes the inner run")
                           (test-assert
                            (and (probe-file configuration-root)
                                 (equal root (sb-ext:symbol-global-value
                                              '*test-temporary-root*)))
                            "nested cleanup restores the surviving enclosing run"))
                         (case exit
                           (:error
                            (error 'test-fixture-error))
                           (:throw
                            (throw 'fixture-exit :escaped)))
                         (values :finished 42)))))
                 (test-fixture-error ()
                   :failed))
               (ecase exit
                 (:normal
                  '(:finished 42))
                 (:error
                  :failed)
                 (:throw
                  :escaped)))
              "run scopes preserve multiple values, errors and nonlocal exits")
             (test-assert (and saved-root (not (probe-file saved-root))
                               (not (probe-file configuration-root)))
                          "every exit removes the run and unclaimed fixtures")
             (test-assert (equal previous (sb-ext:symbol-global-value '*test-temporary-root*))
                          "every exit restores the previous process-global fixture parent")
             (test-assert (probe-file unrelated)
                          "cleanup preserves an unrelated run directory")))
      (platform-delete-directory-tree *platform* unrelated :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-configuration-fixture-cleanup () null)
(defun test-configuration-fixture-cleanup ()
  "Test canonical roots, recursive cleanup, and value propagation on every exit."
  (dolist (fail-p '(nil t))
    (let ((saved-root nil))
      (test-assert
       (equal
        (handler-case
            (multiple-value-list
             (with-test-configuration (configuration root)
               (setf saved-root root)
               (test-assert (equal root (truename root))
                            "configuration fixtures expose a canonical root")
               (let ((pathname
                       (merge-pathnames "nested/marker.sexp"
                                        (config :data-root configuration))))
                 (snapshot-write pathname '(:fixture :present))
                 (test-assert (probe-file pathname)
                              "fixture bodies can create nested state"))
               (when fail-p
                 (error 'test-fixture-error))
               (values :finished 42)))
          (test-fixture-error ()
            :failed))
        (if fail-p :failed '(:finished 42)))
       "configuration fixtures preserve body values and propagate failures")
      (test-assert (and saved-root (not (probe-file saved-root)))
                   "configuration fixtures remove nested state on every exit")))
  (let ((saved-root nil))
    (test-assert
     (eq :escaped
         (catch 'fixture-exit
           (with-test-configuration (configuration root)
             (declare (ignore configuration))
             (setf saved-root root)
             (throw 'fixture-exit :escaped))))
     "configuration fixtures preserve nonlocal exits")
    (test-assert (not (probe-file saved-root))
                 "configuration fixtures clean up after nonlocal exits"))
  nil)

(-> test-configuration-fixture-isolation () null)
(defun test-configuration-fixture-isolation ()
  "Test nested configurations keep distinct roots and independent durable state."
  (with-test-configuration (outer outer-root)
    (setf (config :codex-fast-mode-p outer) t)
    (let ((inner-root nil))
      (with-test-configuration (inner root)
        (setf inner-root root)
        (test-assert (not (equal outer-root root))
                     "nested configurations receive distinct temporary roots")
        (dolist (name '(:config-root :data-root :state-root :cache-root))
          (test-assert
           (and (uiop:subpathp (config name outer) outer-root)
                (uiop:subpathp (config name inner) root)
                (not (equal (config name outer) (config name inner))))
           "every mutable configuration root belongs to its own fixture"))
        (test-assert (not (config :codex-fast-mode-p inner))
                     "an inner configuration cannot read outer preferences")
        (test-assert
         (not (getf (preferences-load-values inner) :codex-fast-mode-p))
         "an inner configuration's durable store is its own")
        (setf (config :reasoning-traces-p inner) t))
      (test-assert (not (probe-file inner-root))
                   "inner cleanup removes only the inner configuration")
      (test-assert
       (and (probe-file outer-root)
            (getf (preferences-load-values outer) :codex-fast-mode-p)
            (not (getf (preferences-load-values outer) :reasoning-traces-p)))
       "outer configuration state survives inner mutation and cleanup")))
  nil)

(-> test-environment-fixture-restoration () null)
(defun test-environment-fixture-restoration ()
  "Test nested environment fixtures restore absent, empty, and nonempty values."
  (let* ((name     (format nil "AUTOLITH_FIXTURE_~A" (make-identifier)))
         (original (uiop:getenv name)))
    (unless (test-fixture-available-p *platform* ':empty-environment-values)
      (test-withheld ':empty-environment-values
                     "environment fixtures restoring an empty value"))
    (with-test-environment ((name nil))
      (dolist (initial (if (test-fixture-available-p *platform*
                                                     ':empty-environment-values)
                           '(nil "" "outer")
                           '(nil "outer")))
        (with-test-environment ((name initial))
          (dolist (fail-p '(nil t))
            (test-assert
             (equal
              (handler-case
                  (multiple-value-list
                   (with-test-environment ((name "inner"))
                     (test-assert (string= (uiop:getenv name) "inner")
                                  "environment fixtures install their values")
                     (with-test-environment ((name nil))
                       (test-assert (null (uiop:getenv name))
                                    "NIL fixture values remove variables"))
                     (test-assert (string= (uiop:getenv name) "inner")
                                  "nested removal restores the enclosing value")
                     (when fail-p
                       (error 'test-fixture-error))
                     (values :finished 42)))
                (test-fixture-error ()
                  :failed))
              (if fail-p :failed '(:finished 42)))
             "environment fixtures preserve values and propagate failures")
            (test-assert (equal (uiop:getenv name) initial)
                         "environment fixtures restore their precise prior state")))))
    (test-assert (equal (uiop:getenv name) original)
                 "outer environment cleanup restores the process state"))
  nil)

(-> test-environment-fixture-evaluation () null)
(defun test-environment-fixture-evaluation ()
  "Test binding expressions run once and invalid setup cannot leak earlier edits."
  (let ((name        (format nil "AUTOLITH_FIXTURE_~A" (make-identifier)))
        (name-count  0)
        (value-count 0))
    (with-test-environment ((name "outer"))
      (with-test-environment (((progn (incf name-count) name)
                               (progn (incf value-count) "inner")))
        (test-assert (and (= name-count 1) (= value-count 1))
                     "fixture names and values are evaluated once"))
      (test-assert
       (handler-case
           (progn
             (with-test-environment ((name "changed") ("IGNORED" 42))
               :unexpected)
             nil)
         (type-error ()
           t))
       "invalid environment values signal a typed setup failure")
      (test-assert (string= (uiop:getenv name) "outer")
                   "environment setup failures do not leak preceding bindings")))
  nil)

(-> test-function-replacement-fixture-restoration () null)
(defun test-function-replacement-fixture-restoration ()
  "Test nested function replacements restore their definitions after errors."
  (let* ((name     (gensym "FIXTURE-FUNCTION"))
         (original (lambda () :original))
         (outer    (lambda () :outer))
         (inner    (lambda () :inner)))
    (unwind-protect
         (progn
           (setf (symbol-function name) original)
           (test-call-with-function-replacements
            (list (list name outer))
            (lambda ()
              (test-assert (eq (funcall name) :outer)
                           "function fixtures install their replacement")
              (test-assert
               (handler-case
                   (test-call-with-function-replacements
                    (list (list name inner))
                    (lambda ()
                      (test-assert (eq (funcall name) :inner)
                                   "nested function fixtures replace the outer definition")
                      (error 'test-fixture-error)))
                 (test-fixture-error ()
                   t))
               "function fixture errors propagate to the caller")
              (test-assert (eq (symbol-function name) outer)
                           "failed nested replacements restore the outer function")))
           (test-assert (eq (symbol-function name) original)
                        "normal fixture exit restores the original function"))
      (fmakunbound name)))
  nil)

(-> test-long-symbolic-link-fixture () null)
(defun test-long-symbolic-link-fixture ()
  "Create and remove a relative-target symbolic link beyond the Windows path limit."
  (with-test-fixture (':symbolic-links "long symbolic-link paths")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((directory (merge-pathnames
                         (format nil "~{~A/~}"
                                 (make-list 5 :initial-element (make-string 50 :initial-element #\x)))
                         root))
             (target    (merge-pathnames "target.txt" directory))
             (link      (merge-pathnames "link.txt" directory)))
        (ensure-directories-exist target)
        (with-open-file (stream target :direction ':output)
          (write-string "long-link-target" stream))
        (test-fixture-make-symbolic-link *platform* "target.txt" (namestring link))
        (test-assert (string= "long-link-target" (uiop:read-file-string link))
                     "A long link resolves its relative target")
        (test-fixture-remove-link *platform* (namestring link))
        (test-assert (and (not (probe-file link)) (probe-file target))
                     "Removing a long link preserves its target"))))
  nil)

(-> test-platform-copy-file-permissions () null)
(defun test-platform-copy-file-permissions ()
  "Copy ordinary and special POSIX permission bits through the host adapter."
  (with-test-fixture (':file-modes "complete POSIX permission copying")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let ((source           (merge-pathnames "source.txt" root))
            (target           (merge-pathnames "target.txt" root))
            (source-directory (merge-pathnames "source/" root))
            (target-directory (merge-pathnames "target/" root)))
        (dolist (path (list source target))
          (with-open-file (stream path :direction ':output :if-exists ':error)
            (write-string "permissions" stream)))
        (ensure-directories-exist source-directory)
        (ensure-directories-exist target-directory)
        (dolist (case (list (list source target '(#o600 #o2600 #o4600 #o6600))
                           (list source-directory target-directory
                                 '(#o700 #o1700 #o2700))))
          (destructuring-bind (source target modes) case
            (dolist (mode modes)
              (test-fixture-set-file-mode *platform* source mode)
              ;; A host may clear setgid when the inherited group is not ours.
              (let ((captured (test-fixture-file-mode *platform* source)))
                (platform-copy-file-permissions *platform* source target)
                (test-assert (= captured (test-fixture-file-mode *platform* target))
                             "copy every captured POSIX permission bit"))))))))
  nil)

(-> test-xdg-native-directory-roots () null)
(defun test-xdg-native-directory-roots ()
  "Resolve literal POSIX XDG directory names in the runtime and standalone helpers."
  (with-test-fixture (':wildcard-file-names "literal XDG directory names")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let ((*package* (find-package :autolith)))
        (load (asdf:system-relative-pathname :autolith "script/roots.lisp")))
      (let ((script-root (symbol-function
                          (find-symbol "AUTOLITH-APPLICATION-ROOT" :autolith))))
        (loop for (variable kind default) in
              '(("XDG_CONFIG_HOME" :config ".config/")
                ("XDG_DATA_HOME" :data ".local/share/")
                ("XDG_STATE_HOME" :state ".local/state/")
                ("XDG_CACHE_HOME" :cache ".cache/"))
              do (dolist (name '("star*" "square[x]" "open[" "back\\slash"))
                   (dolist (suffix '("" "/"))
                     (let* ((base     (concatenate 'string (uiop:native-namestring root) name))
                            (value    (concatenate 'string base suffix))
                            (expected (concatenate 'string base "/autolith/")))
                       (with-test-environment ((variable value))
                         (dolist (actual (list (platform-application-root *platform* kind)
                                              (funcall script-root kind)))
                           (test-assert (string= expected (uiop:native-namestring actual))
                                        "native XDG characters survive directory conversion")
                           (ensure-directories-exist actual)
                           (test-assert (probe-file actual)
                                        "the literal XDG directory is usable"))))))
                 (dolist (value '(nil "" "relative" "relative["))
                   (with-test-environment ((variable value))
                     (let ((expected (merge-pathnames
                                      "autolith/"
                                      (merge-pathnames default (user-homedir-pathname)))))
                       (test-assert (equal expected (platform-application-root *platform* kind))
                                    "runtime ignores unset, empty and relative XDG roots")
                       (test-assert (equal expected (funcall script-root kind))
                                    "standalone helpers ignore invalid XDG roots"))))))))
  nil)

(-> test-xdg-legacy-backslash-roots () null)
(defun test-xdg-legacy-backslash-roots ()
  "Preserve backslash-misparsed installations without moving or merging their files."
  (with-test-fixture (':wildcard-file-names "legacy XDG backslash roots")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let ((*package* (find-package :autolith)))
        (load (asdf:system-relative-pathname :autolith "script/roots.lisp")))
      (let ((script-root     (symbol-function
                              (find-symbol "AUTOLITH-APPLICATION-ROOT" :autolith)))
            (script-conflict (find-symbol "AUTOLITH-SCRIPT-LEGACY-XDG-ROOT-CONFLICT"
                                          :autolith)))
        (loop for (variable kind) in
              '(("XDG_CONFIG_HOME" :config) ("XDG_DATA_HOME" :data)
                ("XDG_STATE_HOME" :state) ("XDG_CACHE_HOME" :cache))
              do (loop for scenario in '(:fresh :empty :legacy :precreated :native :conflict)
                       do (let* ((value         (concatenate 'string (uiop:native-namestring root)
                                                            (format nil "~(~A-~A~)/back\\slash"
                                                                    kind scenario)))
                                 (native        (uiop:parse-native-namestring
                                                 (concatenate 'string value "/autolith/")))
                                 (legacy        (merge-pathnames
                                                 "autolith/" (uiop:ensure-directory-pathname
                                                              (pathname value))))
                                 (native-record (merge-pathnames "nested/opaque.record" native))
                                 (legacy-record (merge-pathnames "unknown/opaque.record" legacy)))
                            (when (member scenario '(:empty :precreated))
                              (ensure-directories-exist
                               (merge-pathnames "crash-pointers/empty/deeper/" native))
                              (ensure-directories-exist
                               (merge-pathnames "empty/deeper/" legacy)))
                            (when (member scenario '(:legacy :precreated :conflict))
                              (ensure-directories-exist legacy-record)
                              (with-open-file (stream legacy-record :direction ':output)
                                (write-string "legacy opaque fixture" stream)))
                            (when (member scenario '(:native :conflict))
                              (ensure-directories-exist native-record)
                              (with-open-file (stream native-record :direction ':output)
                                (write-string "native opaque fixture" stream)))
                            (with-test-environment ((variable value))
                              (loop for (resolve conflict) in
                                    (list (list (lambda ()
                                                  (platform-application-root *platform* kind))
                                                'posix-legacy-xdg-root-conflict)
                                          (list (lambda () (funcall script-root kind))
                                                script-conflict))
                                    do (if (eq scenario :conflict)
                                           (progn
                                             (test-assert
                                              (handler-case (progn (funcall resolve) nil)
                                                (error (condition)
                                                  (typep condition conflict)))
                                              "two populated roots require an explicit choice")
                                             (dolist (choice '(use-native-root use-legacy-root))
                                               (test-assert
                                                (equal
                                                 (handler-bind
                                                     ((error (lambda (condition)
                                                               (when (typep condition conflict)
                                                                 (invoke-restart choice)))))
                                                   (funcall resolve))
                                                 (if (eq choice 'use-native-root) native legacy))
                                                "each conflict restart selects exactly one root")))
                                           (let ((warnings 0)
                                                 (expected (if (member scenario '(:legacy :precreated))
                                                               legacy
                                                               native)))
                                             (test-assert
                                              (equal expected
                                                     (handler-bind
                                                         ((warning (lambda (condition)
                                                                     (incf warnings)
                                                                     (muffle-warning condition))))
                                                       (funcall resolve)))
                                              "choose existing legacy content or a fresh literal root")
                                             (test-assert
                                              (= warnings (if (member scenario '(:legacy :precreated)) 1 0))
                                              "legacy fallback warns before its removal boundary")))))
                            (when (member scenario '(:legacy :precreated :conflict))
                              (test-assert (string= "legacy opaque fixture"
                                                    (uiop:read-file-string legacy-record))
                                           "legacy records are untouched"))
                            (when (member scenario '(:native :conflict))
                              (test-assert (string= "native opaque fixture"
                                                    (uiop:read-file-string native-record))
                                           "native records are untouched"))))))))
  nil)

(-> test-xdg-explicit-roots-bypass-legacy () null)
(defun test-xdg-explicit-roots-bypass-legacy ()
  "Do not evaluate conflicting XDG defaults when every root is explicitly configured."
  (with-test-fixture (':wildcard-file-names "explicit roots with legacy XDG conflicts")
    (with-test-configuration (configuration root)
      (let* ((value  (concatenate 'string (uiop:native-namestring root) "back\\slash"))
             (native (uiop:parse-native-namestring (concatenate 'string value "/autolith/")))
             (legacy (merge-pathnames "autolith/"
                                      (uiop:ensure-directory-pathname (pathname value))))
             (base   (uiop:native-namestring (merge-pathnames "defaults/" root)))
             (defaults
               (with-test-environment (("XDG_CONFIG_HOME" base)
                                       ("XDG_DATA_HOME" base)
                                       ("XDG_STATE_HOME" base)
                                       ("XDG_CACHE_HOME" base))
                 (configuration-create :source-root       (config :source-root configuration)
                                       :working-directory root
                                       :durable-p nil :defer-provider-validation-p t))))
        (dolist (directory (list native legacy))
          (let ((record (merge-pathnames "opaque.record" directory)))
            (ensure-directories-exist record)
            (with-open-file (stream record :direction ':output)
              (write-string "opaque fixture" stream))))
        (with-test-environment (("XDG_CONFIG_HOME" value)
                                ("XDG_DATA_HOME" value)
                                ("XDG_STATE_HOME" value)
                                ("XDG_CACHE_HOME" value))
          (dolist (explicit
                   (list
                    (configuration-create
                     :source-root       (config :source-root configuration)
                     :working-directory root
                     :config-root       (config :config-root configuration)
                     :data-root         (config :data-root configuration)
                     :state-root        (config :state-root configuration)
                     :cache-root        (config :cache-root configuration)
                     :durable-p nil :defer-provider-validation-p t)
                    (make-configuration
                     :source-root           (config :source-root configuration)
                     :working-directory     root
                     :config-root           (config :config-root configuration)
                     :data-root             (config :data-root configuration)
                     :state-root            (config :state-root configuration)
                     :cache-root            (config :cache-root configuration)
                     :provider-validation-p nil)
                    (configuration-copy
                     defaults
                     :config-root           (config :config-root configuration)
                     :data-root             (config :data-root configuration)
                     :state-root            (config :state-root configuration)
                     :cache-root            (config :cache-root configuration)
                     :provider-validation-p nil)))
            (dolist (key '(:config-root :data-root :state-root :cache-root))
              (test-assert (equal (config key configuration) (config key explicit))
                           "explicit roots bypass conflicting XDG defaults")))))))
  nil)

(-> test-xdg-legacy-root-scan () null)
(defun test-xdg-legacy-root-scan ()
  "Count links as content and propagate directory inspection failures."
  (with-test-fixture (':posix-adapter "legacy XDG directory inspection")
    (with-test-fixture (':symbolic-links "legacy XDG directory cycles")
      (with-test-configuration (configuration root)
        (declare (ignore configuration))
        (let ((*package* (find-package :autolith)))
          (load (asdf:system-relative-pathname :autolith "script/roots.lisp")))
        (let* ((directory (merge-pathnames "scan/" root))
               (link      (merge-pathnames "empty/cycle" directory))
               (scan-functions
                 (list #'posix--xdg-root-content-p
                       (symbol-function
                        (find-symbol "AUTOLITH-SCRIPT-XDG-ROOT-CONTENT-P" :autolith)))))
          (ensure-directories-exist (merge-pathnames "empty/" directory))
          (dolist (scan scan-functions)
            (test-assert (not (funcall scan (uiop:native-namestring directory)))
                         "a directory-only tree has no installation content"))
          (test-fixture-make-symbolic-link *platform* ".." (uiop:native-namestring link))
          (dolist (scan scan-functions)
            (test-assert (funcall scan (uiop:native-namestring directory))
                         "a directory symlink is content, not a recursive traversal"))
          (test-fixture-remove-link *platform* (uiop:native-namestring link))
          (let ((blocker (merge-pathnames "not-a-directory" directory)))
            (with-open-file (stream blocker :direction ':output)
              (write-string "opaque fixture" stream))
            (dolist (scan scan-functions)
              (test-assert
               (handler-case
                   (progn
                     (funcall scan (concatenate 'string (uiop:native-namestring blocker)
                                                "/child/"))
                     nil)
                 (sb-posix:syscall-error ()
                   t))
               "inspection errors propagate instead of reporting an empty root")))))))
  nil)
