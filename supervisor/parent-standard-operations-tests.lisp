;;;; -- Trusted Standard Operation Behavior Checks --

(in-package #:autolith-supervisor)

(defclass parent-standard-test-provider (cl-llm-provider-api:model-provider) ()
  (:documentation "Parent-only provider used for command and auth boundary checks."))

(serapeum/types:-> parent-standard-test--configuration (pathname) parent-configuration)
(defun parent-standard-test--configuration (root)
  "Make a complete owner configuration with fixture-only private data."
  (flet ((path (name) (namestring (merge-pathnames name root))))
    (make-parent-configuration
     (list :version 1 :runtime "/usr/bin/false" :agent-core (path "agent.core")
           :worker-core (path "agent.core") :source-root (path "source/")
           :workspace (path "workspace/") :private-root (path "private/")
           :temporary-root (path "temporary/") :runtime-roots (list (path "source/"))
           :preferences-path      (path "private/preferences.sexp")
           :account-configuration (list :state-root (path "private/accounts/"))
           :profiles              '((:id "fixture/first" :family :openai-api :model "first"))
           :profile-id "fixture/first" :access-p nil
           :settings              '(:model "first" :model-provider "fixture" :permission-mode "ask"
                                    :private-test-value "synthetic-owner-secret")))))

(serapeum/types:-> parent-standard-test--replace (symbol function function) t)
(defun parent-standard-test--replace (name replacement body)
  "Run BODY with one function replaced, and restore its exact binding."
  (let ((original (symbol-function name)))
    (unwind-protect
         (progn (setf (symbol-function name) replacement) (funcall body))
      (setf (symbol-function name) original))))

(serapeum/types:-> parent-standard-test--after-stop (parent-application) null)
(defun parent-standard-test--after-stop (application)
  "Run a pending change at the same post-cleanup boundary as the controller."
  (let ((function (parent-application-after-stop application)))
    (assert function)
    (setf (parent-application-after-stop application) nil
          (parent-application-restart application) nil)
    (funcall function))
  nil)

(serapeum/types:-> run-parent-standard-operations-tests (pathname) integer)
(defun run-parent-standard-operations-tests (root)
  "Check fixed dispatch, owner settings, private auth, and deferred state changes."
  (let* ((configuration                        (parent-standard-test--configuration root))
         (registry                             (make-parent-provider-extension-registry))
         (*parent-provider-extension-registry* registry)
         (profiles                             (make-hash-table :test #'equal))
         (application                          (make-instance 'parent-application
                                                              :configuration configuration :session-id "initial"
                                                              :directory (merge-pathnames "private/sessions/initial/" root)
                                                              :profiles profiles :accounts (make-hash-table :test #'eq)
                                                              :provider-registry registry :input-source (constantly nil)
                                                              :renderer  (constantly nil)))
         (authenticated                        0)
         (checks                               0))
    (labels ((check (value)
               "Count a behavioral assertion."
               (incf checks)
               (assert value))

             (request (command &rest arguments)
               "Build the actual child command request shape."
               (parent-standard--map "command" command "arguments" (coerce arguments 'vector)))

             (keyword-data (name)
               "Build an inert child keyword argument."
               (parent-standard--map "keyword" name))

             (call (kind arguments &optional (cancelled (constantly nil)))
               "Dispatch through the same registered factory handlers as a real session."
               (let ((handlers (append (parent-provider-operation-handlers application)
                                       (parent-cli-operation-handlers application)
                                       (parent-session-operation-handlers application))))
                 (funcall (second (assoc kind handlers)) arguments cancelled)))

             (failure (function)
               "Return a public failure category without exposing condition data."
               (handler-case (progn (funcall function) nil)
                 (parent-standard-operation-error (condition)
                   (parent-standard-operation-error-reason condition))
                 (parent-cli-error (condition) (parent-cli-error-reason condition))
                 (parent-application-operation-error (condition)
                   (parent-application-operation-error-code condition))))

             (public-p (value)
               "Check serializable public output for the synthetic private fixture value."
               (protocol-data->map value)
               (not (search "synthetic-owner-secret" (cl-rfc8628:json-encode value)))))
      (ensure-directories-exist (merge-pathnames "private/preferences.sexp" root))
      (setf (parent-configuration-provider-registry configuration) registry)
      (register-parent-provider
       "fixture" :models '("first" "second")
       :factory          (lambda (settings &key reasoning-summaries-p)
                           (declare (ignore settings reasoning-summaries-p))
                           (make-instance 'parent-standard-test-provider))
       :authenticator    (lambda (provider &key stream open-browser-p)
                           (declare (ignore provider open-browser-p))
                           (incf authenticated)
                           (write-string "synthetic-owner-secret" stream)))
      (multiple-value-bind (new-profiles accounts)
          (parent-provider-build-bindings configuration
                                          (parent-configuration-get configuration ':settings)
                                          :registry registry)
        (setf (slot-value application 'profiles) new-profiles
              (slot-value application 'accounts) accounts))
      ;; Settings have one validated writer, for commands and ACP alike.
      (let ((result (call ':preferences (request "/fast" (keyword-data "ON")))))
        (check (gethash "codex-fast-mode-p" (gethash "settings" result)))
        (check (public-p result)))
      (check (probe-file (merge-pathnames "private/preferences.sexp" root)))
      (call ':preferences (request "/fast" (keyword-data "OFF")))
      (check (null (getf (parent-configuration-get configuration ':settings) :codex-fast-mode-p)))
      (call ':preferences (request "/fast" (keyword-data "TOGGLE")))
      (check (getf (parent-configuration-get configuration ':settings) :codex-fast-mode-p))
      (check (stringp (gethash "output" (call ':preferences (request "/fast")))))
      (let* ((params (parent-standard--map "name" "boot-screen-seconds" "value" 2.5 "persistent" nil))
             (result (call ':preferences (parent-standard--map "acp_method" "set-setting" "params" params))))
        (check (= 2.5 (gethash "value" (gethash "setting" result))))
        (check (public-p result)))
      (check (vectorp (gethash "items" (call ':preferences (parent-standard--map "acp_method" "settings")))))
      (check (eq ':invalid-public-setting-value
                 (failure (lambda ()
                            (call ':preferences
                                  (parent-standard--map
                                   "acp_method" "set-setting"
                                   "params" (parent-standard--map "name" "boot-screen-seconds" "value" -1)))))))
      (check (eq ':cancelled (failure (lambda () (call ':preferences (request "/fast" t) (constantly t))))))
      (check (eq ':invalid-command-argument
                 (failure (lambda () (call ':preferences (request "/fast" (list 'eval "private")))))))
      ;; Auth callback text belongs to trusted output, never an operation result.
      (let* ((*standard-output* (make-string-output-stream))
             (result            (call ':auth (request "/auth" "fixture"))))
        (check (= authenticated 1))
        (check (gethash "authenticated" result))
        (check (public-p result)))
      (check (public-p (call ':auth (request "/providers"))))
      (let ((output (make-string-output-stream)))
        (check (= 0 (parent-provider-auth-main
                     configuration (make-parent-cli-request :command ':auth :arguments '("fixture"))
                     :input (make-string-input-stream "") :output output)))
        (check (= authenticated 2)))
      (check (eq ':unknown-provider
                 (failure (lambda () (call ':auth (request "/auth" "#.(error :private)"))))))
      ;; Models expose identities only. Bindings change after the native stop.
      (let ((result (call ':model (request "/models"))))
        (check (plusp (length (gethash "items" result))))
        (check (public-p result)))
      (let ((before (parent-application-profiles application)))
        (check (gethash "restart" (call ':model (request "/model" "fixture/second"))))
        (check (eq before (parent-application-profiles application)))
        (check (equal "first" (getf (parent-configuration-get configuration ':settings) :model)))
        (parent-standard-test--after-stop application)
        (check (equal "second" (getf (parent-configuration-get configuration ':settings) :model)))
        (check (not (eq before (parent-application-profiles application)))))
      (check (eq ':unsupported-reasoning
                 (failure (lambda () (call ':model (request "/reasoning" "unavailable-effort"))))))
      ;; All four modes use the controller contract and retain native enforcement.
      (dolist (mode '("sandbox" "full" "ask" "auto"))
        (let ((previous (parent-standard--permission-mode application)))
          (check (gethash "restart" (call ':permissions (request "/permissions" mode))))
          (check (equal previous (parent-standard--permission-mode application)))
          (parent-standard-test--after-stop application)
          (check (equal mode (parent-standard--permission-mode application)))))
      (check (eq ':invalid-permission-mode
                 (failure (lambda () (call ':permissions (request "/permissions" "invalid"))))))
      (check (public-p (call ':permissions (request "/permissions" (keyword-data "LIST")))))
      ;; Session identity and policy belong to the old process until stop.
      (let* ((old    (parent-application-session-id application))
             (result (call ':session (request "/new")))
             (new    (gethash "session_id" result)))
        (check (not (equal old new)))
        (check (equal old (parent-application-session-id application)))
        (check (public-p result))
        (check (eq ':replacement-pending
                   (failure (lambda () (call ':permissions (request "/permissions" "sandbox"))))))
        (parent-standard-test--after-stop application)
        (check (equal new (parent-application-session-id application))))
      (check (gethash "restart" (call ':restart (make-hash-table :test #'equal))))
      (check (eq ':unknown-restart-operation
                 (failure (lambda () (call ':restart (parent-standard--map "operation" "#.(eval :private)"))))))
      (+ checks (run-parent-standard-authorization-tests application)
         (parent-standard-test--interface-start application root)
         (parent-standard-test--acp-admission root)))))

(serapeum/types:-> run-parent-standard-authorization-tests (parent-application) integer)
(defun run-parent-standard-authorization-tests (application)
  "Check external admission modes and fail-closed model classification."
  (let ((arguments (parent-standard--map "path" "public"))
        (checks    0))
    (labels ((check (value)
               "Count an admission contract assertion."
               (incf checks)
               (assert value))

             (mode (value)
               "Install each mode through its deferred controller boundary."
               (parent-application-set-permission-mode application value)
               (parent-standard-test--after-stop application)))
      (mode "full")
      (check (eq t (parent-application-authorize-tool application "fixture:read" arguments)))
      (check (null (parent-application-authorize-tool application "fixture:read" arguments
                                                     :cancelled (constantly t))))
      (mode "sandbox")
      (check (null (parent-application-authorize-tool application "fixture:read" arguments)))
      (mode "ask")
      (dolist (answer '(nil t))
        (parent-standard-test--replace
         'parent-terminal-approve
         (lambda (&rest values) (declare (ignore values)) answer)
         (lambda ()
           (check (eq answer (parent-application-authorize-tool application "fixture:read" arguments))))))
      (mode "auto")
      (parent-standard-test--replace
       'parent-standard--user-instructions (lambda (owner) (declare (ignore owner)) "Read public data.")
       (lambda ()
         (dolist (case '(("{\"decision\":\"full\",\"reason\":\"read public data\"}" t t)
                         ("{\"decision\":\"deny\",\"reason\":\"private data\"}" t nil)
                         ("{\"decision\":\"sandboxed\",\"reason\":\"read public data\"}" t nil)
                         ("{\"decision\":\"full\",\"reason\":\"read public data\"}" nil nil)
                         ("{\"decision\":\"full\",\"reason\":\"read data\",\"extra\":true}" t nil)
                         ("invalid" t nil)))
           (parent-standard-test--replace
            'parent-provider-model-executor
            (lambda (&rest options)
              (declare (ignore options))
              (values
               (lambda (invocation callback)
                 (declare (ignore invocation))
                 (funcall callback (make-instance 'cl-llm-provider-api:assistant-delta-event :text (first case)))
                 (when (second case)
                   (funcall callback (make-instance 'cl-llm-provider-api:provider-completed-event
                                                    :turn-completion ':end))))
               (lambda (invocation) (declare (ignore invocation)) nil)))
            (lambda ()
              (check (eq (third case)
                         (parent-application-authorize-tool application "fixture:read" arguments))))))))
      checks)))

(serapeum/types:-> parent-standard-test--bindings (list function) t)
(defun parent-standard-test--bindings (bindings body)
  "Run BODY with temporary function bindings and restore each binding on exit."
  (if bindings
      (parent-standard-test--replace
       (first (first bindings)) (second (first bindings))
       (lambda () (parent-standard-test--bindings (rest bindings) body)))
      (funcall body)))

(serapeum/types:-> parent-standard-test--interface-start (parent-application pathname) integer)
(defun parent-standard-test--interface-start (application root)
  "Check image drafts, one-shot crash injection, and semantic result display."
  (let* ((configuration (parent-application-configuration application))
         (image         (merge-pathnames "workspace/input.png" root))
         (terminal      (make-instance 'parent-ui-test-terminal :rows 30 :columns 90))
         (frontend      (make-parent-terminal-ui :terminal terminal :fullscreen-p nil))
         (checks        0)
         (configured    0)
         (runs          0))
    (labels ((check (value)
               "Count a controller startup assertion."
               (incf checks)
               (assert value)))
      (ensure-directories-exist image)
      (with-open-file (stream image :direction ':output :if-exists ':supersede
                              :element-type '(unsigned-byte 8))
        (write-byte 0 stream))
      (unwind-protect
           (parent-standard-test--bindings
            (list
             (list 'parent-application-prepare-configuration #'identity)
             (list 'make-parent-application
                   (lambda (&rest values) (declare (ignore values)) application))
             (list 'make-parent-terminal-ui
                   (lambda (&rest values) (declare (ignore values)) frontend))
             (list 'parent-application-input-history
                   (lambda (owner) (declare (ignore owner)) nil))
             (list 'parent-application-configure
                   (lambda (owner launch)
                     (declare (ignore owner launch))
                     (incf configured)
                     ':completed))
             (list 'parent-application-run-interface
                   (lambda (owner ui &rest values)
                     (declare (ignore owner values))
                     (incf runs)
                     (check (= configured 2))
                     (let* ((editor-ui (parent-terminal-ui-editor-ui ui))
                            (draft     (terminal-ui--submission-input
                                        editor-ui (clinedi:line-editor-text (terminal-ui-editor editor-ui)))))
                       (check (equal (list (ls-compat.posix:resolve-pathname image))
                                     (user-message-input-image-pathnames draft))))
                     ':eof))
             (list 'parent-application-run
                   (lambda (owner)
                     (let ((runner (parent-application-input-runner owner)))
                       (check (eq ':simulated-crash
                                  (handler-case (progn (funcall runner owner nil) nil)
                                    (parent-application-operation-error (condition)
                                      (parent-application-operation-error-code condition)))))
                       (check (= configured 1))
                       (check (zerop runs))
                       (check (eq ':eof (funcall runner owner nil))))
                     ':eof)))
            (lambda ()
              (let* ((state  (make-instance 'parent-interface-controller
                                            :application application :frontend frontend))
                     (result (parent-proto:make-application-result
                              :turn-id "lisp" :status ':ok :values '("42"))))
                (get-output-stream-string (parent-ui-test-terminal-output terminal))
                (parent-terminal-ui-event frontend ':finalized
                                          (list :id "lisp/value" :content (terminal-ui-content->portable "42")))
                (check (search "42" (get-output-stream-string (parent-ui-test-terminal-output terminal))))
                (parent-interface--accept-result state result)
                (parent-interface--accept-result state result)
                (check (zerop (length (get-output-stream-string
                                       (parent-ui-test-terminal-output terminal))))))
              (check (eq ':eof
                         (parent-application-start
                          configuration
                          :request (make-parent-cli-request
                                    :options (list :simulate-crash t :images (list (namestring image))))
                          :input (make-string-input-stream "") :output (make-string-output-stream))))
              (check (= runs 1))))
        (parent-terminal-ui-close frontend)))
    checks))

(serapeum/types:-> parent-standard-test--acp-admission (pathname) integer)
(defun parent-standard-test--acp-admission (root)
  "Race two real session opens and reject invalid capability session identifiers."
  (let* ((configuration (parent-standard-test--configuration root))
         (service       (make-instance 'parent-acp-service :configuration configuration :request nil))
         (entered       (sb-thread:make-semaphore))
         (release       (sb-thread:make-semaphore))
         (second-ready  (sb-thread:make-semaphore))
         (created       0)
         (checks        0)
         (threads       nil))
    (labels ((check (value)
               "Count an ACP admission assertion."
               (incf checks)
               (assert value))

             (open-session ()
               "Admit the same durable identity from a separate requester."
               (handler-case
                   (progn
                     (parent-acp--open service (namestring (merge-pathnames "workspace/" root))
                                       :identifier "same-session")
                     ':opened)
                 (agentcomms:acp-state-error () ':refused))))
      (parent-standard-test--bindings
       (list
        (list 'parent-session-ensure
              (lambda (&rest values) (declare (ignore values)) nil))
        (list 'parent-application-prepare-configuration #'identity)
        (list 'make-parent-application
              (lambda (settings &rest values)
                (declare (ignore values))
                ;; A setup callback can acquire the publication lock.
                (bordeaux-threads:with-lock-held ((parent-acp-lock service)) (incf created))
                (let ((copy (copy-tree (parent-configuration-values settings))))
                  (setf (getf (getf copy :settings) :permission-mode) ':ask)
                  (make-instance 'parent-application :configuration (make-parent-configuration copy)))))
        (list 'parent-acp-mcp-open
              (lambda (&rest values) (declare (ignore values)) #()))
        (list 'parent-application-open
              (lambda (owner)
                (declare (ignore owner))
                (sb-thread:signal-semaphore entered)
                (assert (sb-thread:wait-on-semaphore release :timeout 5))
                (allocate-instance (find-class 'parent-launch))))
        (list 'parent-acp--configure
              (lambda (session) (declare (ignore session)) nil))
        (list 'parent-acp-start-controllers
              (lambda (session) (declare (ignore session)) nil)))
       (lambda ()
         (unwind-protect
              (progn
                (push (sb-thread:make-thread #'open-session :name "first ACP open") threads)
                (check (sb-thread:wait-on-semaphore entered :timeout 3))
                (push (sb-thread:make-thread
                       (lambda () (sb-thread:signal-semaphore second-ready) (open-session))
                       :name "second ACP open") threads)
                (check (sb-thread:wait-on-semaphore second-ready :timeout 3))
                (check (not (sb-thread:wait-on-semaphore entered :timeout 0.1)))
                (sb-thread:signal-semaphore release 2)
                (let ((results (mapcar (lambda (thread)
                                        (sb-thread:join-thread thread :timeout 5 :default ':timeout)) threads)))
                  (check (= 1 (count ':opened results)))
                  (check (= 1 (count ':refused results))))
                (check (= created 1))
                (check (= 1 (hash-table-count (parent-acp-sessions service)))))
           (sb-thread:signal-semaphore release 2)
           (dolist (thread threads) (sb-thread:join-thread thread :timeout 5 :default nil)))))
      (parent-standard-test--replace
       'parent-acp--open
       (lambda (&rest values) (declare (ignore values)) (error "Unexpected capability session admission."))
       (lambda ()
         (dolist (identifier '("missing-session" 42))
           (check (handler-case
                      (progn
                        (agentcomms:agent-extension-request
                         service "_autolith/capabilities" (agentcomms:json-object "sessionId" identifier))
                        nil)
                    (agentcomms:acp-method-error () t)))))))
    checks))
