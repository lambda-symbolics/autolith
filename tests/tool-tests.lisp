(in-package #:autolith)

;;;; -- Runtime Test Boundary --

(defclass tool-test-runtime-tool (tool)
  ((runtime-identity
    :initarg :runtime-identity
    :reader tool-test-runtime-identity
    :type t
    :documentation "The shared test runtime identity.")
   (close-function
    :initarg :close-function
    :reader tool-test-runtime-close-function
    :type function
    :documentation "The callback recording runtime closure.")
   (resume-function
    :initarg :resume-function
    :reader tool-test-runtime-resume-function
    :type function
    :documentation "The callback recording runtime restart.")
   (close-priority
    :initarg :close-priority
    :initform 0
    :reader tool-test-runtime-close-priority
    :type integer
    :documentation "The deterministic test runtime dependency priority.")
   (detach-function
    :initarg :detach-function
    :reader tool-test-runtime-detach-function
    :type function
    :documentation "The callback recording runtime detachment."))
  (:documentation "A tool exposing deterministic ephemeral-runtime callbacks."))

(defmethod tool-runtime-identity ((tool tool-test-runtime-tool))
  "Return TOOL's shared test runtime identity."
  (tool-test-runtime-identity tool))

(defmethod tool-runtime-close ((tool tool-test-runtime-tool))
  "Invoke TOOL's deterministic close callback."
  (funcall (tool-test-runtime-close-function tool))
  nil)

(defmethod tool-runtime-close-priority ((tool tool-test-runtime-tool))
  "Return TOOL's deterministic dependency priority."
  (tool-test-runtime-close-priority tool))

(defmethod tool-runtime-resume
    ((tool tool-test-runtime-tool) (registry tool-registry))
  "Invoke TOOL's deterministic resume callback."
  (declare (ignore registry))
  (funcall (tool-test-runtime-resume-function tool))
  nil)

(defmethod tool-runtime-detach ((tool tool-test-runtime-tool))
  "Invoke TOOL's deterministic detach callback."
  (funcall (tool-test-runtime-detach-function tool))
  nil)


;;;; -- Subsystem Tests --

(-> tool-test--grok-web-run () null)
(defun tool-test--grok-web-run ()
  "Test standalone Grok web search dispatch and authentication without network access."
  (let* ((configuration (configuration-with-model (test-configuration) "grok-4.5"))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "grok-web-run"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation))
                (tool
                  (make-instance 'web-run-tool
                                 :namespace "web"
                                 :name "run"
                                 :description "Test web search."
                                 :parameters (web-run-parameters)))
                (credentials
                  (make-instance 'oauth-credentials
                                 :access-token "grok-web-token"
                                 :refresh-token nil
                                 :id-token nil
                                 :account-id "grok-user"
                                 :expires-at nil
                                 :source-path
                                 (configuration-grok-auth-path configuration)))
                (arguments
                  (json-object
                   "open"
                   (json-array (json-object "ref_id" "https://example.com"))))
                (captured-url nil)
                (captured-headers nil))
           (test-call-with-function-replacements
            (list
             (list
              'call-with-credentials
              (lambda (manager function &key force-refresh)
                (declare (ignore manager force-refresh))
                (funcall function credentials)))
             (list
              'dexador:post
              (lambda (url &key headers content &allow-other-keys)
                (declare (ignore content))
                (setf captured-url url
                      captured-headers headers)
                (values "{\"output\":\"search result\"}" 200 nil))))
            (lambda ()
              (let ((result (tool-execute tool context arguments)))
                (flet ((header (name)
                         (rest (assoc name captured-headers :test #'string-equal))))
                  (test-assert
                   (and (tool-result-success-p result)
                        (string= (tool-result-content result) "search result"))
                   "web.run returns Grok standalone search output")
                  (test-assert
                   (string= captured-url
                            "https://cli-chat-proxy.grok.com/v1/alpha/search")
                   "web.run derives Grok's standalone search endpoint")
                  (test-assert
                   (string= (header "Authorization") "Bearer grok-web-token")
                   "web.run sends Grok's bearer token")
                  (test-assert
                   (string= (header "X-XAI-Token-Auth") "xai-grok-cli")
                   "web.run sends Grok's proxy authentication marker")
                  (test-assert
                   (string= (header "x-grok-model-override") "grok-4.5")
                   "web.run sends Grok's selected model")
                  (test-assert
                   (string= (header "Accept") "application/json")
                   "web.run requests a Grok JSON response")
                  (test-assert
                   (null (header "ChatGPT-Account-ID"))
                   "web.run does not send Codex-only headers to Grok"))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(defclass tool-test-overflow-tool (tool)
  ()
  (:documentation "A tool returning one deliberately oversized result."))

(defmethod tool-execute ((tool tool-test-overflow-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a deterministic result far above the bounded result limit."
  (declare (ignore tool context))
  (tool-success (tool-tests--overflow-text)))

(-> tool-tests--overflow-text () string)
(defun tool-tests--overflow-text ()
  "Return deterministic content larger than the bounded result limit."
  (with-output-to-string (stream)
    (loop for line from 1 to 700
          do (format stream "overflow line ~D~%" line))))

(-> test-tool-result-overflow () null)
(defun test-tool-result-overflow ()
  "Test oversized tool results staying readable through context objects."
  (let* ((registry (make-default-tool-registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "tool-overflow"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (full-text (tool-tests--overflow-text)))
           (tool-registry-register
            registry
            (make-instance 'tool-test-overflow-tool
                           :namespace "test"
                           :name "overflow"
                           :description "Return an oversized test result."
                           :parameters (json-object
                                        "type" "object"
                                        "properties" (json-object)
                                        "additionalProperties" false)))
           (let* ((result (tool-registry-execute-call
                           registry
                           (json-object "namespace" "test"
                                        "name" "overflow"
                                        "arguments" "{}")
                           context))
                  (content (tool-result-content result))
                  (marker "read the complete result at context:")
                  (marker-start (search marker content)))
             (test-assert (tool-result-success-p result)
                          "oversized tool results still succeed")
             (test-assert (< (length content) (length full-text))
                          "oversized tool results stay bounded")
             (test-assert marker-start
                          "truncation notices name a durable context URI")
             (let* ((digest-start (+ marker-start (length marker)))
                    (digest-end (or (position-if-not
                                     (lambda (character)
                                       (find character "0123456789abcdef"))
                                     content
                                     :start digest-start)
                                    (length content)))
                    (digest (subseq content digest-start digest-end)))
               (multiple-value-bind (object stored)
                   (rlm-context-object-find configuration digest)
                 (test-assert (and object (string= stored full-text))
                              "the spilled context object holds the complete result"))
               (let ((window (tool-registry-execute-call
                              registry
                              (json-object
                               "namespace" "resource"
                               "name" "read"
                               "arguments"
                               (json-encode
                                (json-object "uri" (format nil "context:~A" digest)
                                             "start-line" 695
                                             "line-count" 10)))
                              context)))
                 (test-assert (and (tool-result-success-p window)
                                   (search "overflow line 700"
                                           (tool-result-content window)))
                              "the discarded tail stays readable at the context URI"))))
           (let ((small (tool-registry-execute-call
                         registry
                         (json-object "namespace" "missing"
                                      "name" "operation"
                                      "arguments" "{}")
                         context)))
             (test-assert (null (search "context:" (tool-result-content small)))
                          "small results carry no overflow notice")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> tool-tests--web-gist-call (tool-registry tool-context json-object) tool-result)
(defun tool-tests--web-gist-call (registry context arguments)
  "Dispatch one web.gist call with JSON ARGUMENTS."
  (tool-registry-execute-call
   registry
   (json-object "namespace" "web" "name" "gist"
                "arguments" (json-encode arguments))
   context))

(-> test-web-gist-tool () null)
(defun test-web-gist-tool ()
  "Test web.gist registration and URL validation without network access."
  (with-test-configuration (configuration)
    (let* ((registry (make-default-tool-registry))
           (tool (tool-registry-find registry "web" "gist"))
           (context (make-instance
                     'tool-context :configuration configuration :worker nil
                     :conversation (conversation-create configuration)))
           (fetched nil))
      (test-assert tool "the default registry contains web.gist")
      (test-assert
       (gethash "url" (json-get (tool-parameters tool) "properties"))
       "web.gist declares its url argument")
      (test-assert (null (tool-child-safe-p tool))
                   "ordinary child agents cannot use web.gist")
      (test-call-with-function-replacements
       (list (list 'fetch-gist:markdown-from-url
                   (lambda (url)
                     (push url fetched)
                     (format nil "# Example Page~%~%Content."))))
       (lambda ()
         (dolist (url '("https://example.com/docs"
                        "HTTP://example.com/docs"
                        "hTtPs://example.com/docs"
                        "http://[::1]:8080/docs"))
           (let ((result (tool-tests--web-gist-call
                          registry context (json-object "url" url))))
             (test-assert
              (and (tool-result-success-p result)
                   (string= (first fetched) url)
                   (search "Content." (tool-result-content result)))
              "absolute HTTP URLs reach the retriever, regardless of scheme case")))
         (let ((fetch-count (length fetched)))
           (dolist (arguments
                     (list (json-object)
                           (json-object "url" "")
                           (json-object "url" 42)
                           (json-object "url" nil)
                           (json-object "url" "file:///etc/passwd")
                           (json-object "url" "/relative/path")
                           (json-object "url" "https://")
                           (json-object "url" "https:///missing-host")
                           (json-object "url" "http://example.com:bad/")))
             (test-assert
              (not (tool-result-success-p
                    (tool-tests--web-gist-call registry context arguments)))
              "invalid or missing URLs produce a failed tool call"))
           (test-assert (= fetch-count (length fetched))
                        "invalid URLs are rejected before network access"))))))
  nil)

(-> test-web-gist-retrieval () null)
(defun test-web-gist-retrieval ()
  "Exercise the pinned fetch-gist API, native HTTP failures, and result bounds."
  (with-test-configuration (configuration)
    (let* ((registry (make-default-tool-registry))
           (context (make-instance
                     'tool-context :configuration configuration :worker nil
                     :conversation (conversation-create configuration)))
           (url "https://example.com/page")
           (markdown (format nil "# Markdown~%~%Some content."))
           (large-markdown (make-string 12000 :initial-element #\a)))
      (dolist (case (list (list "text/markdown; charset=utf-8" markdown t)
                         (list "text/html" "<h1>Title</h1><p>Some content.</p>" t)
                         (list "application/pdf" "%PDF" nil)
                         (list nil "untyped response" nil)))
        (destructuring-bind (content-type body success-p) case
          (test-call-with-function-replacements
           (list (list 'dex:get
                       (lambda (requested &key headers read-timeout force-string)
                         (test-assert
                          (and (string= requested url) headers
                               (plusp read-timeout) force-string)
                          "page retrieval requests decoded text with a read timeout")
                         (values body 200
                                 (json-object "content-type" content-type)
                                 (quri:uri requested)))))
           (lambda ()
             (let ((result (tool-tests--web-gist-call
                            registry context (json-object "url" url))))
               (test-assert (eq (tool-result-success-p result) success-p)
                            "only HTML and Markdown responses are accepted")
               (when success-p
                 (test-assert
                  (search "Some content." (tool-result-content result))
                  "converted HTML and explicit Markdown contain the page text")))))))
      (test-call-with-function-replacements
       (list (list 'dex:get
                   (lambda (requested &rest options)
                     (declare (ignore options))
                     (error 'dexador.error:http-request-not-found
                            :body "missing page" :status 404 :headers (json-object)
                            :uri (quri:uri requested) :method ':get))))
       (lambda ()
         (let ((result (tool-tests--web-gist-call
                        registry context (json-object "url" url))))
           (test-assert
            (and (not (tool-result-success-p result))
                 (search "web.gist" (tool-result-content result))
                 (search "404" (tool-result-content result)))
            "native Dexador HTTP failures become named tool failures"))))
      (test-call-with-function-replacements
       (list (list 'dex:get
                   (lambda (requested &rest options)
                     (declare (ignore options))
                     (values large-markdown 200
                             (json-object "content-type" "text/markdown")
                             (quri:uri requested)))))
       (lambda ()
         (let* ((result (tool-tests--web-gist-call
                         registry context (json-object "url" url)))
                (content (tool-result-content result)))
           (test-assert
            (and (tool-result-success-p result)
                 (< (length content) (length large-markdown))
                 (search "context:" content))
            "large page results include a bounded excerpt and context URI"))))))
  nil)

(-> tool-tests--web-search-call (tool-registry tool-context json-object) tool-result)
(defun tool-tests--web-search-call (registry context arguments)
  "Dispatch one web.search call with JSON ARGUMENTS."
  (tool-registry-execute-call
   registry
   (json-object "namespace" "web" "name" "search"
                "arguments" (json-encode arguments))
   context))

(-> tool-tests--web-search-response () string)
(defun tool-tests--web-search-response ()
  "Return one fixture Parallel search response body."
  (json-encode
   (json-object
    "search_id" "search_fixture"
    "warnings" (json-array "mode adjusted")
    "results"
    (json-array
     (json-object "url" "https://one.example/page"
                  "title" "One"
                  "publish_date" "2026-01-02"
                  "excerpts" (json-array "one excerpt"))
     (json-object "url" "https://one.example/page"
                  "title" "Duplicate"
                  "publish_date" nil
                  "excerpts" (json-array "duplicate excerpt"))
     (json-object "url" "https://two.example/page"
                  "title" "Two"
                  "publish_date" nil
                  "excerpts" (json-array "two excerpt"))))))

(-> test-web-search-tool () null)
(defun test-web-search-tool ()
  "Test web.search registration, schema, and argument validation."
  (with-test-configuration (configuration)
    (let* ((registry (make-default-tool-registry))
           (tool (tool-registry-find registry "web" "search"))
           (context (make-instance
                     'tool-context :configuration configuration :worker nil
                     :conversation (conversation-create configuration))))
      (test-assert tool "the default registry contains web.search")
      (test-assert
       (gethash "query" (json-get (tool-parameters tool) "properties"))
       "web.search declares its query argument")
      (test-assert (null (tool-child-safe-p tool))
                   "ordinary child agents cannot use web.search")
      (dolist (arguments
                (list (json-object)
                      (json-object "query" "")
                      (json-object "query" 42)
                      (json-object "query" "release notes" "mode" "instant")
                      (json-object "query" "release notes" "max_results" 0)
                      (json-object "query" "release notes" "max_results" "many")
                      (json-object "query" "release notes" "search_queries" "one")
                      (json-object "query" "release notes" "search_queries"
                                   (json-array ""))))
        (test-assert
         (not (tool-result-success-p
               (tool-tests--web-search-call registry context arguments)))
         "invalid web.search arguments fail before network access"))
      (with-test-environment (("PARALLEL_API_KEY" nil))
        (let ((result (tool-tests--web-search-call
                       registry context (json-object "query" "release notes"))))
          (test-assert
           (and (not (tool-result-success-p result))
                (search "PARALLEL_API_KEY" (tool-result-content result)))
           "a missing Parallel API key fails the call by name")))))
  nil)

(-> test-web-search-pipeline () null)
(defun test-web-search-pipeline ()
  "Exercise the web.search fetch, extraction, and synthesis stages."
  (with-test-configuration (configuration)
    (let* ((registry (make-default-tool-registry))
           (context (make-instance
                     'tool-context :configuration configuration :worker nil
                     :conversation (conversation-create configuration)))
           (objective "release notes"))
      (with-test-environment (("PARALLEL_API_KEY" "test-key"))
        (let ((fetched-urls nil)
              (fetch-lock (make-lock "web-search test fetch log"))
              (captured-tasks nil)
              (captured-budget nil))
          (test-call-with-function-replacements
           (list
            (list 'dexador:post
                  (lambda (url &key headers content &allow-other-keys)
                    (declare (ignore headers content))
                    (test-assert (string= url *web-search-endpoint*)
                                 "web.search posts to the Parallel endpoint")
                    (values (tool-tests--web-search-response) 200)))
            (list 'web-gist--retrieve
                  (lambda (url)
                    (with-lock-held (fetch-lock)
                      (push url fetched-urls))
                    (if (string= url "https://one.example/page")
                        (concatenate 'string
                                     (make-string 20000 :initial-element #\a)
                                     "TAIL-MARKER")
                        (error "boom on ~A" url))))
            (list 'rlm-map
                  (lambda (tasks &key budget &allow-other-keys)
                    (setf captured-tasks (copy-list tasks)
                          captured-budget budget)
                    (loop for task in tasks
                          collect
                          (list ':task (getf task ':task)
                                ':value "one fact"
                                ':trace "trace-frame-1"))))
            (list 'rlm-synthesize-inference-results
                  (lambda (policy task results &key &allow-other-keys)
                    (declare (ignore policy))
                    (test-assert
                     (and (search "1. One - https://one.example/page" task)
                          (search "Objective: release notes" task))
                     "the synthesis task embeds the numbered source list")
                    (test-assert
                     (= 1 (length results))
                     "only fetched pages reach extraction")
                    (values "The answer." "trace-42"))))
           (lambda ()
             (let* ((result (tool-tests--web-search-call
                             registry context (json-object "query" objective)))
                    (content (tool-result-content result)))
               (test-assert (tool-result-success-p result)
                            "the happy path succeeds")
               (test-assert
                (and (search "The answer." content)
                     (search "1. One - https://one.example/page - published 2026-01-02"
                             content)
                     (search "Skipped pages:" content)
                     (search "- https://two.example/page: boom on https://two.example/page"
                             content)
                     (search "- mode adjusted" content)
                     (search "Parallel search: search_fixture" content)
                     (search "Trace: trace-42" content)
                     (not (search "TAIL-MARKER" content)))
                "the result keeps the answer, sources, skips, warnings, and trace")
               (test-assert
                (and (= 2 (length fetched-urls))
                     (member "https://one.example/page"
                             fetched-urls :test #'string=)
                     (member "https://two.example/page"
                             fetched-urls :test #'string=))
                "deduplicated results fetch one body per URL")
               (test-assert
                (= 2 (rlm-budget-remaining-calls captured-budget))
                "the shared budget reserves one frame per page plus synthesis")
               (test-assert
                (and (= 1 (length captured-tasks))
                     (search "https://one.example/page"
                             (getf (first captured-tasks) ':task))
                     (let ((view (first (getf (first captured-tasks) ':context))))
                       (and (not (search "TAIL-MARKER" (getf view ':content)))
                            (< (length (getf view ':content))
                               (+ *web-search-page-character-limit* 100)))))
                "extraction tasks carry one truncated view per page")))))
        (let ((captured-tasks nil)
              (captured-results nil))
          (test-call-with-function-replacements
           (list
            (list 'dexador:post
                  (lambda (url &key &allow-other-keys)
                    (declare (ignore url))
                    (values (tool-tests--web-search-response) 200)))
            (list 'web-gist--retrieve
                  (lambda (url)
                    (declare (ignore url))
                    "Page body."))
            (list 'rlm-map
                  (lambda (tasks &key &allow-other-keys)
                    (setf captured-tasks (copy-list tasks))
                    (loop for task in tasks
                          for number from 1
                          collect
                          (if (= number 1)
                              (list ':task (getf task ':task)
                                    ':value "fact one"
                                    ':trace "trace-a")
                              (list ':task (getf task ':task)
                                    ':error "frame boom")))))
            (list 'rlm-synthesize-inference-results
                  (lambda (policy task results &key &allow-other-keys)
                    (declare (ignore policy task))
                    (setf captured-results (copy-list results))
                    (values "Partial answer." "trace-7"))))
           (lambda ()
             (let ((content (tool-result-content
                             (tool-tests--web-search-call
                              registry context (json-object "query" objective)))))
               (test-assert
                (and (search "https://one.example/page"
                             (getf (first captured-tasks) ':task))
                     (search "https://two.example/page"
                             (getf (second captured-tasks) ':task)))
                "extraction tasks keep the Parallel result order")
               (test-assert
                (and (getf (first captured-results) ':value)
                     (getf (second captured-results) ':error))
                "failed frames keep their result slot")
               (test-assert
                (and (search "Partial answer." content)
                     (search "Failed extractions:" content)
                     (search "source 2 (https://two.example/page): frame boom"
                             content)
                     (search "2. Two - https://two.example/page" content)
                     (not (search "Skipped pages:" content)))
                "partial extraction failures are reported by source number")))))
        (let ((captured-tasks nil))
          (test-call-with-function-replacements
           (list
            (list 'dexador:post
                  (lambda (url &key &allow-other-keys)
                    (declare (ignore url))
                    (values (tool-tests--web-search-response) 200)))
            (list 'web-gist--retrieve
                  (lambda (url)
                    (declare (ignore url))
                    (error "blocked")))
            (list 'rlm-map
                  (lambda (tasks &key &allow-other-keys)
                    (setf captured-tasks (copy-list tasks))
                    (loop for task in tasks
                          collect
                          (list ':task (getf task ':task)
                                ':value "excerpt fact"))))
            (list 'rlm-synthesize-inference-results
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (values "Excerpt answer." "trace-8"))))
           (lambda ()
             (let ((content (tool-result-content
                             (tool-tests--web-search-call
                              registry context (json-object "query" objective)))))
               (test-assert
                (and (= 2 (length captured-tasks))
                     (search "one excerpt"
                             (getf (first (getf (first captured-tasks) ':context))
                                   ':content))
                     (search "two excerpt"
                             (getf (first (getf (second captured-tasks) ':context))
                                   ':content)))
                "excerpt bodies supply the views when every fetch fails")
               (test-assert
                (and (search "Excerpt answer." content)
                     (search "Skipped pages:" content)
                     (search "- https://one.example/page: blocked" content))
                "the excerpt fallback still reports the skipped fetches")))))
        (test-call-with-function-replacements
         (list
          (list 'dexador:post
                (lambda (url &key &allow-other-keys)
                  (declare (ignore url))
                  (values (tool-tests--web-search-response) 200)))
          (list 'web-gist--retrieve
                (lambda (url)
                  (declare (ignore url))
                  "Page body."))
          (list 'rlm-map
                (lambda (tasks &key &allow-other-keys)
                  (loop for task in tasks
                        collect
                        (list ':task (getf task ':task)
                              ':error "frame boom"))))
          (list 'rlm-synthesize-inference-results
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values "unused" "trace-x"))))
         (lambda ()
           (let ((result (tool-tests--web-search-call
                          registry context (json-object "query" objective))))
             (test-assert
              (and (not (tool-result-success-p result))
                   (search "extraction failed for every source"
                           (tool-result-content result)))
              "every failed extraction fails the call"))))
        (test-call-with-function-replacements
         (list (list 'dexador:post
                     (lambda (url &key &allow-other-keys)
                       (declare (ignore url))
                       (values "{}" 500))))
         (lambda ()
           (let ((result (tool-tests--web-search-call
                          registry context (json-object "query" objective))))
             (test-assert
              (and (not (tool-result-success-p result))
                   (search "HTTP 500" (tool-result-content result)))
              "non-200 search responses fail by status"))))
        (test-call-with-function-replacements
         (list (list 'dexador:post
                     (lambda (url &key &allow-other-keys)
                       (declare (ignore url))
                       (error "connection refused"))))
         (lambda ()
           (let ((result (tool-tests--web-search-call
                          registry context (json-object "query" objective))))
             (test-assert
              (and (not (tool-result-success-p result))
                   (search "Parallel search failed: connection refused"
                           (tool-result-content result)))
              "transport failures keep the condition text"))))
        (test-call-with-function-replacements
         (list (list 'dexador:post
                     (lambda (url &key &allow-other-keys)
                       (declare (ignore url))
                       (values (json-encode
                                (json-object "results" (json-array)))
                               200))))
         (lambda ()
           (let ((result (tool-tests--web-search-call
                          registry context (json-object "query" objective))))
             (test-assert
              (and (tool-result-success-p result)
                   (search "no results" (tool-result-content result)))
              "an empty result list is a successful no-results answer")))))))
  nil)

(-> test-tool-registry () null)
(defun test-tool-registry ()
  "Test tool schemas, dispatch failure handling, and runtime lifecycle cleanup."
  (let* ((registry (make-default-tool-registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "tool-registry"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (unknown-call (json-object
                               "namespace" "missing"
                               "name" "operation"
                               "arguments" "{}"))
                (result (tool-registry-execute-call
                         registry unknown-call context)))
           (let ((immutable-registry
                   (make-default-tool-registry :immutable-p t)))
              (dolist (name '("status" "diff" "generations"))
                (test-assert (tool-registry-find immutable-registry "self" name)
                             (format nil "immutable mode retains self.~A" name)))
              (test-assert
               (and (tool-registry-find immutable-registry "lisp" "describe")
                    (tool-registry-find immutable-registry "lisp" "source"))
               "immutable mode retains active-image inspection through Lisp targets")
             (dolist (name '("eval" "redefine" "set" "persist-definition"
                             "discard" "exercise" "commit" "checkpoint"
                             "rollback"))
               (test-assert
                (null (tool-registry-find immutable-registry "self" name))
                (format nil "immutable mode omits self.~A" name))))
            (let ((old-registry (make-instance 'tool-registry))
                  (new-registry (make-instance 'tool-registry)))
              (labels ((register-name (candidate canonical-name)
                         (let ((separator (position #\. canonical-name)))
                           (tool-registry-register
                            candidate
                            (make-instance
                             'tool
                             :namespace (subseq canonical-name 0 separator)
                             :name (subseq canonical-name (1+ separator))
                              :description "Tool capability diff test."
                              :parameters (tool-object-schema (json-object) nil))))))
                (dolist (name '("z.last" "a.keep" "m.remove"))
                  (register-name old-registry name))
                (dolist (name '("y.add" "a.keep" "b.add"))
                  (register-name new-registry name)))
              (multiple-value-bind (added removed)
                  (tool-registry-capability-diff old-registry new-registry)
                (test-assert
                 (equal added '("b.add" "y.add"))
                 "tool registry diffs additions in deterministic lexical order")
                (test-assert
                 (equal removed '("m.remove" "z.last"))
                 "tool registry diffs removals in deterministic lexical order")))
           (test-assert
            (and (tool-registry-find registry "resource" "read")
                 (tool-registry-find registry "resource" "edit")
                 (null (tool-registry-find registry "fs" "read"))
                 (null (tool-registry-find registry "fs" "edit")))
            "existing-file access is exposed only through the resource protocol")
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure")
           (let* ((commands
                    (json-object
                     "search_query"
                     (json-array
                      (json-object "q" "current UTC date"))))
                  (request (web--search-request context
                                                (provider-create configuration)
                                                commands)))
             (test-assert
              (string=
               (web--search-endpoint
                (make-instance
                 'configuration
                 :provider-endpoint
                 "https://chatgpt.com/backend-api/codex/responses"))
               "https://chatgpt.com/backend-api/codex/alpha/search")
              "web.run derives the standalone provider search endpoint")
             (test-assert
              (string= (json-get
                        (aref (json-get (json-get request "commands")
                                        "search_query")
                              0)
                        "q")
                       "current UTC date")
              "web.run passes Codex search commands to provider search")
             (test-assert
              (eq (json-get (json-get request "settings") "external_web_access")
                  false)
              "cached web.run requests forbid direct web access")
             (let ((live-request
                     (web--search-request
                      (make-instance 'tool-context
                                     :configuration
                                     (configuration--clone configuration
                                                           :web-search-mode "live")
                                     :worker nil
                                     :conversation conversation)
                      (provider-create configuration)
                      commands)))
               (test-assert
                (eq (json-get (json-get live-request "settings")
                              "external_web_access") t)
                "live web.run requests permit direct web access"))
             (let ((indexed-request
                     (web--search-request
                      (make-instance 'tool-context
                                     :configuration
                                     (configuration--clone configuration
                                                           :web-search-mode "indexed")
                                     :worker nil
                                     :conversation conversation)
                      (provider-create configuration)
                      commands)))
               (test-assert
                (string= (json-get (json-get indexed-request "settings")
                                   "external_web_access")
                         "indexed")
                "indexed web.run requests select Codex's indexed search mode"))
             (let ((parameters
                     (tool-parameters (tool-registry-find registry "web" "run"))))
               (test-assert
                (gethash "search_query" (json-get parameters "properties"))
                "web.run declares Codex's search-query command")
               (test-assert
                (gethash "time" (json-get parameters "properties"))
                "web.run declares Codex's time command")
               (test-assert
                (null (gethash "query" (json-get parameters "properties")))
                "web.run no longer declares its incompatible query shim"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((registry (make-instance 'tool-registry))
         (empty-schema (tool-object-schema (json-object) nil))
         (replaceable
           (make-instance 'tool
                          :namespace "second"
                          :name "replaceable"
                          :description "Original registered tool."
                          :parameters empty-schema))
         (first
           (make-instance 'tool
                          :namespace "first"
                          :name "one"
                          :description "Later namespace tool."
                          :parameters empty-schema))
         (second
           (make-instance 'tool
                          :namespace "second"
                          :name "two"
                          :description "Later tool in the first namespace."
                          :parameters empty-schema))
         (replacement
           (make-instance 'tool
                          :namespace "second"
                          :name "replaceable"
                          :description "Replacement registered tool."
                          :parameters empty-schema)))
    (dolist (tool (list replaceable first second replacement))
      (tool-registry-register registry tool))
    (test-assert
     (and (eq (tool-registry-find registry "second" "replaceable") replacement)
          (equal (mapcar #'tool-canonical-name (tool-registry-tools registry))
                 '("second.replaceable" "first.one" "second.two")))
     "replacement lookup changes the object without moving its presentation position")
    (let ((projection (tool-registry-tools registry)))
      (setf (first projection) first)
      (test-assert
       (equal (mapcar #'tool-canonical-name (tool-registry-tools registry))
              '("second.replaceable" "first.one" "second.two"))
       "the public tool list is detached from registry storage"))
    (let ((schemas (tool-registry-provider-schemas registry)))
      (test-assert
       (and (equalp
             (map 'vector (lambda (schema) (json-get schema "name")) schemas)
             #("second" "first"))
            (equalp
             (map 'vector
                  (lambda (schema) (json-get schema "name"))
                  (json-get (aref schemas 0) "tools"))
             #("replaceable" "two")))
       "provider schemas preserve first-seen namespace and tool order")))
  (let ((registry (make-instance 'tool-registry))
        (runtime-identity (list ':shared-runtime))
        (close-count 0)
        (resume-count 0)
        (detach-count 0)
        (events nil))
    (flet ((make-runtime-tool (name)
             "Return one test tool sharing the lexical runtime counters."
             (make-instance
              'tool-test-runtime-tool
              :namespace "test"
              :name name
              :description "Exercise the runtime lifecycle protocol."
              :parameters (tool-object-schema (json-object) nil)
              :runtime-identity runtime-identity
              :close-function
              (lambda ()
                (incf close-count)
                (push (list name ':close) events))
              :resume-function
              (lambda ()
                (incf resume-count)
                (push (list name ':resume) events))
              :detach-function
              (lambda ()
                (incf detach-count)
                (push (list name ':detach) events)))))
      (tool-registry-register registry (make-runtime-tool "first"))
      (tool-registry-register registry (make-runtime-tool "second"))
      (tool-registry-close-runtime-state registry)
      (tool-registry-resume-runtime-state registry)
      (tool-registry-detach-runtime-state registry)
      (test-assert (= close-count 1)
                   "a shared tool runtime closes exactly once per registry")
      (test-assert (= resume-count 1)
                   "a shared tool runtime resumes exactly once per registry")
      (test-assert (= detach-count 1)
                   "a shared tool runtime detaches exactly once per registry")
      (test-assert
       (equal (nreverse events)
              '(("second" :close) ("first" :resume) ("first" :detach)))
       "runtime operations select the representative for their traversal direction")))
  (let ((registry (make-instance 'tool-registry))
        (close-order nil)
        (resume-order nil)
        (failure nil))
    (flet ((make-runtime-tool
               (&key name identity priority close-function resume-function)
             "Return one independently identified close-test tool."
             (make-instance
              'tool-test-runtime-tool
              :namespace "failure-test"
              :name name
              :description "Exercise complete runtime cleanup after failure."
              :parameters (tool-object-schema (json-object) nil)
              :runtime-identity identity
              :close-priority priority
              :close-function close-function
              :resume-function resume-function
              :detach-function (lambda () nil))))
      (tool-registry-register
       registry
       (make-runtime-tool
        :name "failure"
        :identity (list ':failure)
        :priority 50
        :close-function
        (lambda ()
          (push ':failure close-order)
          (error "expected runtime close failure"))
        :resume-function
        (lambda () (push ':failure resume-order))))
      (tool-registry-register
       registry
       (make-runtime-tool
        :name "later"
        :identity (list ':later)
        :priority 100
        :close-function (lambda () (push ':later close-order))
        :resume-function (lambda () (push ':later resume-order))))
      (setf failure
            (handler-case
                (progn
                  (tool-registry-close-runtime-state registry)
                  nil)
              (error (condition)
                condition)))
      (test-assert failure
                   "runtime closure reports the first cleanup failure")
      (test-assert (equal (nreverse close-order) '(:later :failure))
                   "runtime closure unwinds dependencies and survives a failure")
      (tool-registry-resume-runtime-state registry)
      (test-assert (equal (nreverse resume-order) '(:failure :later))
                   "runtime restart restores dependencies before dependents")))
  nil)


(-> test-workspace-tools () null)
(defun test-workspace-tools ()
  "Test workspace image inspection and bounded shell commands."
  (let* ((registry (make-default-tool-registry))
         (base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "workspace")))
           (labels ((run (namespace name &rest arguments)
                      "Execute NAMESPACE.NAME with ARGUMENTS through the registry."
                      (tool-registry-execute-call
                       registry
                       (json-object "namespace" namespace
                                    "name" name
                                    "arguments" (json-encode
                                                 (apply #'json-object
                                                        arguments)))
                       (make-instance 'tool-context
                                      :configuration configuration
                                      :worker nil
                                      :conversation conversation
                                      :command-authorization-function
                                      (lambda (command directory)
                                        (declare (ignore command directory))
                                        ':full-access)))))

             (let* ((image-path (merge-pathnames "tool-image.png" root))
                    (image (test-conversation--write-tiny-png image-path))
                    (result (run "fs" "view-image"
                                 "path" (namestring image)))
                    (attachments (tool-result-image-attachments result)))
               (test-assert
                (and (tool-result-success-p result)
                     (= (length attachments) 1)
                     (probe-file
                      (image-attachment-pathname (first attachments))))
                "fs.view-image validates and privately preserves a local image")
               (test-assert
                (search "1x1, image/png" (tool-result-content result))
                "fs.view-image reports the prepared image metadata"))
             (let ((result (run "shell" "run"
                                "command" "echo autolith-shell-works && exit 3")))
               (test-assert (tool-result-success-p result)
                            "shell.run reports command completion")
               (test-assert (search "exit 3" (tool-result-content result))
                            "shell.run reports nonzero exit codes")
               (test-assert (search "autolith-shell-works"
                                    (tool-result-content result))
                            "shell.run captures combined output"))
             (test-assert
              (= (workspace-tool-shell-timeout
                  (json-object "timeout-seconds" 900))
                 900)
              "shell.run accepts requested timeouts above ten minutes")
             (let* ((result
                      (run "shell" "run"
                           "command" "printf '\\374\\022\\023\\265\\n'"))
                    (content (tool-result-content result)))
               (test-assert (tool-result-success-p result)
                            "shell.run completes after invalid UTF-8 output")
               (test-assert
                (search (string (code-char #xFFFD)) content)
                "shell.run replaces invalid output bytes without losing status"))
             (let* ((*shell-maximum-output-characters* 5)
                    (result (run "shell" "run"
                                 "command" "printf 123456789"))
                    (content (tool-result-content result)))
               (test-assert (tool-result-success-p result)
                            "shell.run completes when output is truncated")
               (test-assert (search "12345" content)
                            "shell.run retains the bounded output prefix")
               (test-assert (not (search "6789" content))
                            "shell.run omits output beyond the capture limit")
               (test-assert
                (search "combined output truncated after 5 characters" content)
                "shell.run reports output truncation explicitly"))
             (let* ((target (merge-pathnames "denied-command.txt" root))
                    (result
                      (tool-registry-execute-call
                       registry
                       (json-object
                        "namespace" "shell"
                        "name" "run"
                        "arguments"
                        (json-encode
                         (json-object
                          "command"
                          (format nil "printf denied > ~A"
                                  (uiop:escape-shell-token
                                   (namestring target))))))
                       (make-instance 'tool-context
                                      :configuration configuration
                                      :worker nil
                                      :conversation conversation))))
               (test-assert (not (tool-result-success-p result))
                            "shell.run denies execution without authorization")
               (test-assert (not (probe-file target))
                            "a denied shell command has no side effects"))
             (let* ((inside (merge-pathnames "sandboxed-command.txt" root))
                    (outside
                      (merge-pathnames
                       (format nil "autolith-blocked-~A.txt" (make-identifier))
                       (user-homedir-pathname)))
                    (sandbox-configuration
                      (configuration--clone configuration
                                            :working-directory root)))
               (unwind-protect
                    (let ((result
                            (tool-registry-execute-call
                             registry
                             (json-object
                              "namespace" "shell"
                              "name" "run"
                              "arguments"
                              (json-encode
                               (json-object
                                "command"
                                (format nil
                                        "printf ok > ~A; printf blocked > ~A"
                                        (uiop:escape-shell-token
                                         (namestring inside))
                                        (uiop:escape-shell-token
                                         (namestring outside))))))
                             (make-instance
                              'tool-context
                              :configuration sandbox-configuration
                              :worker nil
                              :conversation conversation
                              :command-authorization-function
                              (lambda (command directory)
                                (declare (ignore command directory))
                                ':sandboxed)))))
                      (test-assert
                       (tool-result-success-p result)
                       "an authorized shell command runs inside the sandbox")
                      (test-assert (probe-file inside)
                                   "the command sandbox permits workspace writes")
                      (test-assert
                       (not (probe-file outside))
                       "the command sandbox rejects writes outside the workspace"))
                 (when (probe-file outside)
                   (delete-file outside))))
             (let ((result (run "shell" "run"
                                "command" "sleep 5"
                                "timeout-seconds" 1)))
               (test-assert (not (tool-result-success-p result))
                            "shell.run stops runaway commands")
               (test-assert (search "stopped after 1"
                                    (tool-result-content result))
                             "shell.run explains its timeout"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (tool-test--grok-web-run)
  nil)
