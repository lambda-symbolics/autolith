(in-package #:autolith)

;;;; -- clifff Worker Adapter --

(-> search-worker--source-root ((option configuration)) pathname)
(defun search-worker--source-root (configuration)
  "Return the tracked source root that holds the helper script.

CONFIGURATION carries the root the launcher supplied. Without one, the ASDF
source directory serves, which inside a saved image names the tree the image
was built from and may no longer exist."
  (if configuration
      (config :source-root configuration)
      (asdf:system-source-directory :autolith)))

(-> search-worker-create (&key (:configuration (option configuration))) worker)
(defun search-worker-create (&key configuration)
  "Create Autolith's lazy supervised clifff helper below CONFIGURATION's source root."
  (let* ((source-root (search-worker--source-root configuration))
         (script (merge-pathnames "bin/autolith-search-worker" source-root))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (unless (probe-file script)
      (error 'search-error
             :message (format nil "The private fff helper is missing at ~A."
                              script)
             :operation ':worker
             :pathname script
             :cause nil))
    (make-worker
     :command (list sbcl-command "--noinform" "--script" (namestring script)))))

(-> search-worker--cache-directory (configuration) pathname)
(defun search-worker--cache-directory (configuration)
  "Return CONFIGURATION's private fff database directory."
  (merge-pathnames "fff/" (config :cache-root configuration)))

(-> search-worker--log-path (configuration) pathname)
(defun search-worker--log-path (configuration)
  "Return CONFIGURATION's private diagnostic log for the fff helper."
  (merge-pathnames "worker.log" (search-worker--cache-directory configuration)))

(-> search-worker-request
    (worker configuration &key (:operation keyword) (:arguments list))
    string)
(defun search-worker-request (worker configuration &key operation arguments)
  "Execute one isolated clifff request for CONFIGURATION."
  (handler-case
      (worker-request
       worker
       :library-path (search--validated-library-path configuration)
       :base-path (config :working-directory configuration)
       :cache-directory (search-worker--cache-directory configuration)
       :log-pathname (search-worker--log-path configuration)
       :operation operation
       :arguments arguments)
    (clifff-error (condition)
      (error 'search-error
             :message (princ-to-string condition)
             :operation (clifff-error-operation condition)
             :pathname (or (clifff-error-pathname condition)
                           (config :working-directory configuration))
             :cause (or (clifff-error-cause condition) condition)))))

(-> search-worker-file-count (worker configuration string) (integer 0))
(defun search-worker-file-count (worker configuration glob)
  "Return how many files CONFIGURATION's workspace index holds that match GLOB."
  (handler-case
      (worker-file-count
       worker glob
       :library-path (search--validated-library-path configuration)
       :base-path (config :working-directory configuration)
       :cache-directory (search-worker--cache-directory configuration)
       :log-pathname (search-worker--log-path configuration))
    (clifff-error (condition)
      (error 'search-error
             :message (princ-to-string condition)
             :operation (clifff-error-operation condition)
             :pathname (or (clifff-error-pathname condition)
                           (config :working-directory configuration))
             :cause (or (clifff-error-cause condition) condition)))))

(defmethod tool-runtime-identity ((tool search-tool))
  "Return the isolated clifff worker shared by this search tool family."
  (search-tool-engine tool))

(defmethod tool-runtime-close ((tool search-tool))
  "Stop TOOL's isolated clifff worker."
  (worker-close (search-tool-engine tool))
  nil)

(defmethod tool-runtime-detach ((tool search-tool))
  "Detach TOOL's inherited clifff streams before saving a forked Lisp image."
  (worker-detach (search-tool-engine tool))
  nil)


;;;; -- Tool Execution --

(defmethod tool-execute ((tool search-files-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Fuzzy-search indexed workspace paths through isolated clifff."
  (let ((query (search-tool--string-argument tool arguments "query"
                                             :required t))
        (page (search-tool--bounded-integer arguments "page"
                                            :maximum #xffffffff))
        (maximum-results
          (search-tool--bounded-integer
           arguments "max-results"
           :fallback *search-default-result-limit*
           :minimum 1
           :maximum *search-maximum-result-limit*)))
    (when (find #\Newline query)
      (error 'tool-error
             :message "search.files query must fit on one line."
             :tool-name "search.files"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':files
      :arguments (list query
                       :glob-p nil
                       :page page
                       :page-size maximum-results)))))

(defmethod tool-execute ((tool search-glob-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Glob indexed workspace paths through isolated clifff."
  (let ((pattern (search-tool--string-argument tool arguments "pattern"
                                               :required t))
        (page (search-tool--bounded-integer arguments "page"
                                            :maximum #xffffffff))
        (maximum-results
          (search-tool--bounded-integer
           arguments "max-results"
           :fallback *search-default-result-limit*
           :minimum 1
           :maximum *search-maximum-result-limit*)))
    (when (find #\Newline pattern)
      (error 'tool-error
             :message "search.glob pattern must fit on one line."
             :tool-name "search.glob"))
    (tool-success
     (search-worker-request
      (search-tool-engine tool)
      (tool-context-configuration context)
      :operation ':files
      :arguments (list pattern
                       :glob-p t
                       :page page
                       :page-size maximum-results)))))

(defmethod tool-execute ((tool search-content-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search indexed contents for one query or several literal patterns."
  (let* ((query-value (tool-argument arguments "query"))
         (patterns-value (tool-argument arguments "patterns"))
         (query-p (not (null query-value)))
         (patterns-p (not (null patterns-value))))
    (unless (not (eq query-p patterns-p))
      (error 'tool-error
             :message "search.content requires exactly one of query or patterns."
             :tool-name "search.content"))
      (if patterns-p
          (let* ((patterns (and (vectorp patterns-value)
                                (coerce patterns-value 'list)))
                 (constraints
                   (search-tool--string-argument tool arguments "constraints")))
            (when (nth-value 1 (gethash "mode" arguments))
              (error 'tool-error
                     :message "search.content mode applies only to a single query."
                     :tool-name "search.content"))
            (unless (and patterns
                         (every (lambda (pattern)
                                  (and (non-empty-string-p pattern)
                                       (not (find #\Newline pattern))))
                                patterns))
              (error 'tool-error
                     :message "search.content patterns must be non-empty literal strings without newlines."
                     :tool-name "search.content"))
            (search-tool--check-constraints tool (tool-context-configuration context)
                                            constraints)
            (tool-success
             (search-worker-request
              (search-tool-engine tool)
              (tool-context-configuration context)
              :operation ':multi-content
              :arguments
              (append (list patterns
                            :constraints (search-tool--fff-constraints constraints))
                      (search-tool--common-content-options arguments)))))
          (let* ((query (search-tool--string-argument tool arguments "query"
                                                      :required t))
                 (constraints
                   (search-tool--string-argument tool arguments "constraints"))
                 (mode-name (search-tool--string-argument tool arguments "mode"
                                                          :fallback "plain"))
                 (mode (cond
                         ((string= mode-name "plain") ':plain)
                         ((string= mode-name "regex") ':regex)
                         ((string= mode-name "fuzzy") ':fuzzy)
                         (t
                          (error 'tool-error
                                 :message
                                 "search.content mode must be plain, regex, or fuzzy."
                                 :tool-name "search.content")))))
            (search-tool--check-constraints tool (tool-context-configuration context)
                                            constraints)
            (tool-success
             (search-worker-request
              (search-tool-engine tool)
              (tool-context-configuration context)
              :operation ':content
              :arguments (append (list (search-tool--query-with-constraints
                                        (search-tool--escape-earmuffed-names query)
                                        (search-tool--fff-constraints constraints))
                                       :mode mode)
                                 (search-tool--common-content-options arguments))))))))
