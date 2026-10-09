(in-package #:autolith)

;;;; -- Native Search Tests --

(-> search-tests--configuration (pathname) configuration)
(defun search-tests--configuration (workspace)
  "Return an isolated search configuration rooted beside WORKSPACE."
  (configuration-copy (test-configuration) :working-directory workspace))

(-> search-tests--write-file (pathname string) null)
(defun search-tests--write-file (pathname content)
  "Write CONTENT to PATHNAME for one native search fixture."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string content stream))
  nil)

(-> search-tests--bootstrap-source-commit (configuration) string)
(defun search-tests--bootstrap-source-commit (configuration)
  "Return the fff revision selected by CONFIGURATION's bootstrap source."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:read-file-string
    (merge-pathnames "native/fff/commit"
                     (config :source-root configuration)))))

(-> search-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun search-tests--call (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with alternating JSON ARGUMENTS."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> test-search-tools () null)
(defun test-search-tools ()
  "Exercise the clifff adapter and all three indexed workspace operations."
    (test-assert
     (and (string= (search-tool--query-with-constraints "symbol" "")
                   "symbol")
            (string= (search-tool--query-with-constraints "symbol" "   ")
                     "symbol")
          (string= (search-tool--query-with-constraints "symbol" "*.lisp src/")
                   "*.lisp src/ symbol"))
     "query constraints prepend as fff path filters")
  (test-assert
   (and (equal (search-tool--constraint-tokens "  *.lisp	src/ !tests/ ") '("*.lisp" "src/" "!tests/"))
        (equal (mapcar #'search-tool--directory-constraint-p '("src/" "!tests/" "*/" "/"))
               '(t nil nil nil))
        (equal (mapcar #'search-tool--file-path-constraint-p
                       '("src/main.lisp" "main.lisp" "*.lisp" "!main.lisp" "v2.0" "src/"))
               '(t t nil nil nil nil)))
   "search constraints classify directory and file path filters as fff does")
  (test-assert
   (and (string= (search-tool--escape-earmuffed-names "src/  *limit* (setf *x*")
                 "src/  \\*limit* (setf \\*x*")
        (string= (search-tool--escape-earmuffed-names "*.lisp *test.* * ** *a/b* symbol")
                 "*.lisp *test.* * ** *a/b* symbol"))
   "query special variable names are escaped while path globs are kept")
  (test-assert
   (and (string= (search-tool--fff-constraints "*.lisp src/ docs/a,b.org !tests/ /lib/")
                 "*.lisp !tests/ {**/src/**,**/docs/a\\,b.org,**/lib/**}")
        (string= (search-tool--fff-constraints "*.lisp src/ !tests/")
                 "*.lisp src/ !tests/"))
   "several directories and file paths become one alternative glob")
  (let* ((default-configuration
           (configuration-create
            :source-root (asdf:system-source-directory :autolith)
            :working-directory (asdf:system-source-directory :autolith)))
         (configured-library (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (library
           (if (non-empty-string-p configured-library)
               (pathname configured-library)
               (merge-pathnames (format nil "native/fff/~A"
                                        (fff-library-file-name))
                                (config :data-root
                                 default-configuration))))
         (previous-library (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (workspace-root (uiop:ensure-directory-pathname
                          (merge-pathnames
                           (format nil "autolith-search-tests-~A/"
                                   (make-identifier))
                           (uiop:temporary-directory))))
         (configuration nil)
         (registry nil))
    (unwind-protect
         (progn
           (test-assert (probe-file library)
                        "bootstrap installs the private fff library")
            (test-assert
             (string= (search-tests--bootstrap-source-commit
                       default-configuration)
                      *fff-source-commit*)
             "bootstrap and runtime use one pinned fff source revision")
            (unless configured-library
              (test-assert
               (fff-library-current-p library *fff-source-commit*)
               "bootstrap installs a manifest matching the pinned fff source"))
           (platform-setenv "AUTOLITH_FFF_LIBRARY" (namestring library))
           (ensure-directories-exist workspace-root)
           (search-tests--write-file
            (merge-pathnames "src/model-selection.lisp" workspace-root)
            (format nil "first context line~%AUTOLITH_FFF_PRIMARY~%last context line~%"))
           (search-tests--write-file
            (merge-pathnames "docs/search-guide.org" workspace-root)
            (format nil "AUTOLITH_FFF_SECONDARY~%"))
           (search-tests--write-file
            (merge-pathnames "src/specials.lisp" workspace-root)
            (format nil "(defvar *autolith-fff-special* 1)~%"))
           (setf configuration (search-tests--configuration workspace-root)
                 registry (make-default-tool-registry))
            (let* ((conversation
                     (conversation-create configuration :identifier "fff-search"))
                   (context
                     (make-instance 'tool-context
                                    :configuration configuration
                                    :worker nil
                                    :conversation conversation
                                    :registry registry))
                   (files-tool (tool-registry-find registry "search" "files"))
                   (glob-tool (tool-registry-find registry "search" "glob"))
                   (content-tool
                     (tool-registry-find registry "search" "content"))
                   (multi-tool
                     (tool-registry-find registry "search" "multi-content")))
              (test-assert (and files-tool glob-tool content-tool (null multi-tool))
                           "three native search tools are registered")
              (test-assert
               (eq (search-tool-engine files-tool)
                   (search-tool-engine content-tool))
               "one registry shares one isolated index across search operations")
              (let* ((schema (tool-parameters content-tool))
                     (properties (json-get schema "properties"))
                     (patterns-schema (json-get properties "patterns"))
                     (item-schema (json-get patterns-schema "items"))
                     (one-of (json-get schema "oneOf")))
                (test-assert
                 (and (vectorp one-of)
                      (= (length one-of) 2)
                      (equalp
                       (map 'list
                            (lambda (variant)
                              (json-get variant "required"))
                            one-of)
                       '(#("query") #("patterns")))
                      (= (json-get patterns-schema "minItems") 1)
                      (string= (json-get item-schema "type") "string")
                      (= (json-get item-schema "minLength") 1))
                 "search.content schema requires exactly one non-empty query or patterns array"))
              (let ((result (search-tests--call registry context
                                                "search" "files"
                                                "query" "model selection")))
                (test-assert (tool-result-success-p result)
                             (format nil
                                     "search.files completes through clifff: ~A"
                                     (tool-result-content result)))
                (test-assert (search "src/model-selection.lisp"
                                     (tool-result-content result))
                             "search.files returns fuzzy workspace-relative paths"))
              (let ((result (search-tests--call registry context
                                                "search" "glob"
                                                "pattern" "**/*.lisp")))
                (test-assert (tool-result-success-p result)
                             "search.glob completes through the shared index")
                (test-assert (search "src/model-selection.lisp"
                                     (tool-result-content result))
                             "search.glob filters indexed relative paths"))
              (let ((result (search-tests--call registry context
                                                "search" "content"
                                                "query" "AUTOLITH_FFF_PRIMARY"
                                                "context" 1)))
                (test-assert (tool-result-success-p result)
                             "search.content completes through the content index")
                (test-assert
                 (and (search "src/model-selection.lisp:2:"
                              (tool-result-content result))
                      (search "first context line"
                              (tool-result-content result))
                      (search "last context line"
                              (tool-result-content result)))
                 (format nil "search.content renders locations and bounded context: ~S"
                         (tool-result-content result))))
              (let ((result
                      (search-tests--call
                       registry context
                       "search" "content"
                       "patterns" #("AUTOLITH_FFF_PRIMARY"
                                    "AUTOLITH_FFF_SECONDARY")
                       "constraints" "*.lisp")))
                (test-assert (tool-result-success-p result)
                             "search.content searches literal alternatives in one pass")
                (test-assert (and (search "src/model-selection.lisp"
                                          (tool-result-content result))
                                  (not (search "docs/search-guide.org"
                                               (tool-result-content result))))
                               "search.content honors separate pattern constraints"))
                (let ((kept
                        (search-tests--call registry context
                                            "search" "content"
                                            "query" "AUTOLITH_FFF_PRIMARY"
                                            "constraints" "*.lisp"))
                      (dropped
                        (search-tests--call registry context
                                            "search" "content"
                                            "query" "AUTOLITH_FFF_SECONDARY"
                                            "constraints" "*.lisp")))
                  (test-assert (and (tool-result-success-p kept)
                                    (search "src/model-selection.lisp"
                                            (tool-result-content kept))
                                    (not (search "docs/search-guide.org"
                                                 (tool-result-content kept))))
                               "search.content keeps query matches under constraints")
                  (test-assert (and (tool-result-success-p dropped)
                                    (not (search "src/model-selection.lisp"
                                                 (tool-result-content dropped)))
                                    (not (search "docs/search-guide.org"
                                                 (tool-result-content dropped))))
                               "search.content applies query constraints as path filters"))
                (let ((existing (search-tests--call registry context
                                                    "search" "content"
                                                    "query" "AUTOLITH_FFF_PRIMARY"
                                                    "constraints" "src/model-selection.lisp"))
                      (missing (search-tests--call registry context
                                                   "search" "content"
                                                   "query" "AUTOLITH_FFF_PRIMARY"
                                                   "constraints" "src/absent.lisp")))
                  (test-assert (and (tool-result-success-p existing)
                                    (search "src/model-selection.lisp"
                                            (tool-result-content existing)))
                               "a file path constraint naming an indexed file narrows the search")
                  (test-assert (and (not (tool-result-success-p missing))
                                    (search "No indexed file matches the constraint src/absent.lisp"
                                            (tool-result-content missing)))
                               "a file path constraint naming no file fails instead of widening"))
                (dolist (case
                          (list (list "patterns" #("AUTOLITH_FFF_PRIMARY" "AUTOLITH_FFF_SECONDARY")
                                      "src/ docs/" t t)
                                (list "query" "AUTOLITH_FFF_"
                                      "src/model-selection.lisp docs/search-guide.org" t t)
                                (list "patterns" #("AUTOLITH_FFF_PRIMARY" "AUTOLITH_FFF_SECONDARY")
                                      "*.lisp src/ docs/" t nil)
                                (list "query" "AUTOLITH_FFF_"
                                      "src/ docs/ !docs/" t nil)))
                  (destructuring-bind (selector value constraints source-p documentation-p) case
                    (let* ((result (search-tests--call registry context
                                                       "search" "content"
                                                       selector value
                                                       "constraints" constraints))
                           (content (tool-result-content result)))
                      (test-assert
                       (and (tool-result-success-p result)
                            (eq (and (search "src/model-selection.lisp" content) t) source-p)
                            (eq (and (search "docs/search-guide.org" content) t) documentation-p))
                       (format nil "constraints ~S treat locations as alternatives that other filters narrow: ~S"
                               constraints content)))))
                (let ((special (search-tests--call registry context
                                                   "search" "content"
                                                   "query" "*autolith-fff-special* src/")))
                  (test-assert (and (tool-result-success-p special)
                                    (search "src/specials.lisp:1:"
                                            (tool-result-content special)))
                               (format nil "a special variable name with a path filter is searched as text: ~S"
                                       (tool-result-content special))))
                (dolist (case
                          (list
                           (list "missing selector" nil
                                 "exactly one of query or patterns")
                           (list "both selectors"
                                 (list "query" "AUTOLITH_FFF_PRIMARY"
                                       "patterns" #("AUTOLITH_FFF_SECONDARY"))
                                 "exactly one of query or patterns")
                           (list "empty patterns" (list "patterns" #())
                                 "non-empty literal strings")
                           (list "invalid patterns" (list "patterns" #(42))
                                 "non-empty literal strings")
                           (list "mode with patterns"
                                 (list "patterns" #("AUTOLITH_FFF_PRIMARY")
                                       "mode" "plain")
                                 "mode applies only")))
                (destructuring-bind (label arguments expected) case
                  (let ((result
                          (apply #'search-tests--call
                                 registry context "search" "content" arguments)))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (search expected (tool-result-content result)))
                     (format nil "search.content rejects ~A" label)))))
              (let* ((worker (search-tool-engine files-tool))
                    (watched-process (worker-process worker))
                    (watched-pid (uiop:process-info-pid watched-process)))
               (sleep 0.25)
               (search-tests--write-file
                (merge-pathnames "src/model-selection.lisp" workspace-root)
                (format nil
                        "first context line~%AUTOLITH_FFF_WATCHED~%last context line~%"))
               (sleep 0.5)
               (let ((result (search-tests--call registry context
                                                 "search" "content"
                                                 "query" "AUTOLITH_FFF_WATCHED")))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "src/model-selection.lisp"
                               (tool-result-content result))
                       (eq watched-process (worker-process worker))
                       (uiop:process-alive-p watched-process)
                       (= watched-pid
                          (uiop:process-info-pid (worker-process worker))))
                  "a watched file update keeps the same native helper alive")))
             (let* ((worker (search-tool-engine files-tool))
                    (failed-process (worker-process worker))
                    (failed-pid (uiop:process-info-pid failed-process))
                    (frecency-marker
                      (merge-pathnames "fff/frecency/test-marker"
                                       (config :cache-root configuration)))
                    (history-marker
                      (merge-pathnames "fff/history/test-marker"
                                       (config :cache-root configuration))))
               (search-tests--write-file frecency-marker "discard me")
               (search-tests--write-file history-marker "discard me")
               (platform-terminate-process *platform* failed-pid :force t)
               (test-assert
                (task-tests--wait-until
                 (lambda () (not (uiop:process-alive-p failed-process))) 10)
                "the killed search helper exits before its replacement starts")
               (let ((result (search-tests--call registry context
                                                 "search" "files"
                                                 "query" "model selection")))
                 (test-assert
                  (tool-result-success-p result)
                  (format nil "search restarts after helper death: ~A"
                          (tool-result-content result)))
                 (test-assert (not (probe-file frecency-marker))
                              "restart discards the dead helper's ranking database")
                 (test-assert (not (probe-file history-marker))
                              "restart discards the dead helper's history database")
                 (test-assert (probe-file (search-worker--log-path configuration))
                              "the replacement helper has a diagnostic log")
                 (test-assert
                  (and (worker-process worker)
                       (uiop:process-alive-p (worker-process worker))
                       (/= failed-pid
                           (uiop:process-info-pid (worker-process worker))))
                  "search runs in a new live helper after the old one dies")))
             (tool-registry-close-runtime-state registry)
             (test-assert
              (null (worker-process (search-tool-engine files-tool)))
              "closing a registry stops and clears its isolated watcher")))
      (when registry
        (ignore-errors (tool-registry-close-runtime-state registry)))
      (if previous-library
          (platform-setenv "AUTOLITH_FFF_LIBRARY" previous-library)
          (platform-unsetenv "AUTOLITH_FFF_LIBRARY"))
      (platform-delete-directory-tree *platform* workspace-root
                                      :validate t
                                      :if-does-not-exist ':ignore)
      (when configuration
        (platform-delete-directory-tree
         *platform*
         (test-configuration-root configuration)
         :validate t
         :if-does-not-exist ':ignore))))
  nil)

(-> test-search-worker-source-root () null)
(defun test-search-worker-source-root ()
  "Locate the helper script through the configured source root, not the build tree."
  (with-test-configuration (configuration root)
    (let ((source-root (asdf:system-source-directory :autolith))
          (setting (configuration-setting configuration :source-root)))
      (configuration-set configuration setting source-root :source ':override)
      (test-assert (typep (search-worker-create :configuration configuration) 'worker)
                   "the helper script resolves below the configured source root")
      (configuration-set configuration setting root :source ':override)
      (test-assert
       (handler-case
           (progn (search-worker-create :configuration configuration) nil)
         (search-error (condition)
           (equal (search-error-pathname condition)
                  (merge-pathnames "bin/autolith-search-worker" root))))
       "a source root without the helper script signals a search error naming it")))
  nil)
