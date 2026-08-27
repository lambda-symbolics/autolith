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

(-> test-web-gist-tool () null)
(defun test-web-gist-tool ()
  "Test standalone web.gist page retrieval without network access."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "web-gist"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation))
                (tool (tool-registry-find (make-default-tool-registry)
                                          "web" "gist")))
           (test-assert tool "the default registry contains web.gist")
           (test-assert
            (gethash "url" (json-get (tool-parameters tool) "properties"))
            "web.gist declares its url argument")
           (test-assert
            (null (tool-child-safe-p tool))
            "web.gist stays unavailable to child agents like web.run")
           (test-call-with-function-replacements
            (list
             (list
              'fetch-gist:markdown-from-url
              (lambda (url)
                (test-assert
                 (string= url "https://example.com/docs")
                 "web.gist passes the requested URL to fetch-gist")
                "# Example Page\n\nContent.")))
            (lambda ()
              (let ((result (tool-execute
                             tool context
                             (json-object "url" "https://example.com/docs"))))
                (test-assert
                 (and (tool-result-success-p result)
                      (string= (tool-result-content result)
                               "# Example Page\n\nContent."))
                 "web.gist returns fetched Markdown as a successful result"))))
           (test-call-with-function-replacements
            (list
             (list
              'fetch-gist:markdown-from-url
              (lambda (url)
                (declare (ignore url))
                (error "Fetching ~A returned HTTP status 404"
                       "https://example.com/missing"))))
            (lambda ()
              (handler-case
                  (progn (tool-execute
                          tool context
                          (json-object "url" "https://example.com/missing"))
                         (test-assert nil "web.gist signals on a failed fetch"))
                (tool-error (condition)
                  (test-assert
                   (and (string= (tool-error-tool-name condition) "web.gist")
                        (search "HTTP status 404"
                                (princ-to-string condition)))
                   "web.gist reports fetch failures as web.gist tool errors")))))
           (handler-case
               (progn (tool-execute
                       tool context (json-object "url" "file:///etc/passwd"))
                      (test-assert nil "web.gist rejects non-HTTP URLs"))
             (tool-error (condition)
               (test-assert
                (search "HTTP and HTTPS" (princ-to-string condition))
                "web.gist rejects non-HTTP URLs with a clear message"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
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
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
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
