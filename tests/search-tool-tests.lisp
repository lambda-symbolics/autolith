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
                          :direction         ':output
                          :if-exists         ':supersede
                          :if-does-not-exist ':create
                          :external-format   ':utf-8)
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
    (tool-registry tool-context &key (:namespace string) (:name string) (:arguments list))
    tool-result)
(defun search-tests--call (registry context &key namespace name arguments)
  "Execute NAMESPACE.NAME with the alternating JSON keys and values in ARGUMENTS."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> test-search-tools () null)
(defun test-search-tools ()
  "Exercise the clifff adapter and all three indexed workspace operations."
  (let* ((default-configuration
           (configuration-create
            :source-root       (asdf:system-source-directory :autolith)
            :working-directory (asdf:system-source-directory :autolith)))
         (configured-library (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (library
           (if (non-empty-string-p configured-library)
               (pathname configured-library)
               (merge-pathnames (format nil "native/fff/~A"
                                        (fff-library-file-name))
                                (config :data-root
                                 default-configuration))))
         (previous-library   (uiop:getenv "AUTOLITH_FFF_LIBRARY"))
         (workspace-root     (uiop:ensure-directory-pathname
                              (merge-pathnames
                               (format nil "autolith-search-tests-~A/"
                                       (make-identifier))
                               (uiop:temporary-directory))))
         (configuration      nil)
         (registry           nil))
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
                                   :worker        nil
                                   :conversation  conversation
                                   :registry      registry))
                  (files-tool (tool-registry-find registry "search" "files"))
                  (glob-tool  (tool-registry-find registry "search" "glob"))
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
             (let* ((schema          (tool-parameters content-tool))
                    (properties      (json-get schema "properties"))
                    (patterns-schema (json-get properties "patterns"))
                    (item-schema     (json-get patterns-schema "items"))
                    (one-of          (json-get schema "oneOf")))
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
                                               :namespace "search"
                                               :name      "files"
                                               :arguments (list "query" "model selection"))))
               (test-assert (tool-result-success-p result)
                            (format nil
                                    "search.files completes through clifff: ~A"
                                    (tool-result-content result)))
               (test-assert (search "src/model-selection.lisp"
                                    (tool-result-content result))
                            "search.files returns fuzzy workspace-relative paths"))
             (let ((result (search-tests--call registry context
                                               :namespace "search"
                                               :name      "glob"
                                               :arguments (list "pattern" "**/*.lisp"))))
               (test-assert (tool-result-success-p result)
                            "search.glob completes through the shared index")
               (test-assert (search "src/model-selection.lisp"
                                    (tool-result-content result))
                            "search.glob filters indexed relative paths"))
             (let ((result (search-tests--call registry context
                                               :namespace "search"
                                               :name      "content"
                                               :arguments (list "query" "AUTOLITH_FFF_PRIMARY"
                                                                "context" 1))))
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
                     (search-tests--call registry context
                                         :namespace "search"
                                         :name      "content"
                                         :arguments (list "patterns" #("AUTOLITH_FFF_PRIMARY"
                                                                       "AUTOLITH_FFF_SECONDARY")
                                                          "constraints" "*.lisp"))))
               (test-assert (tool-result-success-p result)
                            "search.content searches literal alternatives in one pass")
               (test-assert (and (search "src/model-selection.lisp"
                                         (tool-result-content result))
                                 (not (search "docs/search-guide.org"
                                              (tool-result-content result))))
                            "search.content honors separate pattern constraints"))
             (dolist (case
                       (list
                         (list "missing selector" nil :missing-required-arguments)
                        (list "both selectors"
                              (list "query" "AUTOLITH_FFF_PRIMARY"
                                    "patterns" #("AUTOLITH_FFF_SECONDARY"))
                              "exactly one of query or patterns")
                        (list "mode with patterns"
                              (list "patterns" #("AUTOLITH_FFF_PRIMARY")
                                    "mode" "plain")
                              "mode applies only")))
              (destructuring-bind (label arguments expected) case
                (let ((result
                        (search-tests--call registry context
                                            :namespace "search"
                                            :name      "content"
                                            :arguments arguments)))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (if (keywordp expected)
                             (eq expected (tool-result-error-code result))
                             (search expected (tool-result-content result))))
                    (format nil "search.content rejects ~A" label)))))
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
                                      :validate          t
                                      :if-does-not-exist ':ignore)
      (when configuration
        (platform-delete-directory-tree
         *platform*
         (test-configuration-root configuration)
         :validate          t
         :if-does-not-exist ':ignore))))
  nil)

(-> test-search-worker-source-root () null)
(defun test-search-worker-source-root ()
  "Locate the helper script through the configured source root, not the build tree."
  (with-test-configuration (configuration root)
    (let ((source-root (asdf:system-source-directory :autolith))
          (setting     (configuration-setting configuration :source-root)))
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
