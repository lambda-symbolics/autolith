;;;; -- Trusted Current-interface Operations --

(in-package #:autolith-supervisor)

(define-condition parent-standard-operation-error (error)
  ((reason :initarg :reason :reader parent-standard-operation-error-reason
           :documentation "A fixed public failure code."))
  (:documentation "A parent command failed admission or validation.")
  (:report (lambda (condition stream)
             (format stream "Parent operation failed: ~A."
                     (parent-standard-operation-error-reason condition)))))

(serapeum/types:-> parent-standard--reject (keyword) nil)
(defun parent-standard--reject (reason)
  "Reject an invalid operation without reporting private data."
  (error 'parent-standard-operation-error :reason reason))

(serapeum/types:-> parent-standard--map (&rest t) hash-table)
(defun parent-standard--map (&rest pairs)
  "Build a fresh string-key public data map."
  (let ((result (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
          do (setf (gethash key result) value))
    result))

(serapeum/types:-> parent-standard--check-cancelled (function) null)
(defun parent-standard--check-cancelled (cancelled)
  "Check cancellation before a modal prompt or owner-side mutation."
  (when (funcall cancelled) (parent-standard--reject ':cancelled))
  nil)

(serapeum/types:-> parent-standard--argument (t) t)
(defun parent-standard--argument (value)
  "Decode an inert command keyword without interning a peer-supplied symbol."
  (if (hash-table-p value)
      (let ((name (gethash "keyword" value)))
        (unless (and (= (hash-table-count value) 1) (stringp name))
          (parent-standard--reject ':invalid-command-argument))
        (or (find name '(:status :provider :ask :auto :sandbox :full :list :clear
                         :browser :device :device-code :on :off :toggle
                         :turn :sequence :date :time)
                  :key #'symbol-name :test #'string-equal)
            (parent-standard--reject ':unknown-command-keyword)))
      (if (or (null value) (eq value t) (stringp value) (realp value))
          value
          (parent-standard--reject ':invalid-command-argument))))

(serapeum/types:-> parent-standard--arguments (hash-table) list)
(defun parent-standard--arguments (request)
  "Decode a bounded vector of already evaluated child command arguments."
  (let ((arguments (gethash "arguments" request #())))
    (unless (and (vectorp arguments) (not (stringp arguments)) (<= (length arguments) 32))
      (parent-standard--reject ':invalid-command-arguments))
    (map 'list #'parent-standard--argument arguments)))

(serapeum/types:-> parent-standard--choose
    ((or null parent-terminal-ui) string list &key (:cancelled function)) (or null string))
(defun parent-standard--choose (frontend title choices &key (cancelled (constantly nil)))
  "Use the parent's modal chooser, never the child's composer."
  (parent-standard--check-cancelled cancelled)
  (when choices
    (unless frontend (parent-standard--reject ':interactive-terminal-required))
    (prog1 (parent-terminal-ui-choose
            frontend title
            (mapcar (lambda (name) (list :name name :value name :description "")) choices))
      (parent-standard--check-cancelled cancelled))))

(serapeum/types:-> parent-standard--settings-result (parent-configuration &optional string) hash-table)
(defun parent-standard--settings-result (configuration &optional output)
  "Return the approved settings projection and optional display text."
  (let ((result (parent-standard--map "settings" (parent-cli-public-settings configuration))))
    (when output (setf (gethash "output" result) output))
    result))

(serapeum/types:-> parent-standard--store
    (parent-application list function &key (:persistent-p boolean)) hash-table)
(defun parent-standard--store (application changes cancelled &key (persistent-p t))
  "Serialize a validated owner preference write and return public settings."
  (bordeaux-threads:with-lock-held ((parent-application-lock application))
    (parent-standard--check-cancelled cancelled)
    (parent-cli-store-settings (parent-application-configuration application) changes
                               :persistent-p persistent-p)
    (parent-standard--settings-result (parent-application-configuration application))))

(serapeum/types:-> parent-standard--boolean (t) boolean)
(defun parent-standard--boolean (value)
  "Validate a portable boolean, including the ACP JSON false representation."
  (cond
    ((or (null value) (eq value ':off) (eq value ':protocol-null) (eq value yason:false)
         (eq value (argo:json-false)) (equal value "false") (equal value "off"))
     nil)
    ((or (eq value t) (eq value ':on) (equal value "true") (equal value "on"))
     t)
    (t
     (parent-standard--reject ':invalid-boolean))))

(serapeum/types:-> parent-standard--setting-value (keyword t) t)
(defun parent-standard--setting-value (key value)
  "Convert public editor spellings before the shared setting validator."
  (let ((default (getf *parent-cli-setting-defaults* key ':absent)))
    (cond
      ((member default '(nil t))
       (parent-standard--boolean value))
      ((eq key ':terminal-theme)
       (or (find value '(:autolith :almighty) :test #'string-equal :key #'symbol-name)
           (parent-standard--reject ':invalid-theme)))
      (t
       value))))

(serapeum/types:-> parent-standard--setting-items (parent-configuration) vector)
(defun parent-standard--setting-items (configuration)
  "Describe public editable settings without exposing owner paths or callbacks."
  (let ((settings (parent-cli-public-settings configuration)))
    (coerce
     (loop for (key default) on *parent-cli-setting-defaults* by (lambda (tail) (rest (rest tail)))
           for name = (string-downcase key)
           when (nth-value 1 (gethash name settings))
             collect (parent-standard--map
                      "name" name "label" name "documentation" "Public session setting"
                      "group" "session" "scope" "durable" "source" "saved"
                      "type" (cond ((member default '(nil t))
                                    "boolean")
                                   ((integerp default)
                                    "integer") (t
                                                "string"))
                      "value" (gethash name settings) "options" #()
                      "adjustable" (not (getf (parent-configuration-get configuration ':settings) :immutable-p))))
     'vector)))

(serapeum/types:-> parent-standard--preferences
    (parent-application hash-table function &key (:frontend (or null parent-terminal-ui))) hash-table)
(defun parent-standard--preferences (application request cancelled &key frontend)
  "Run settings inspection, a validated edit, or a registered boolean control."
  (let* ((configuration (parent-application-configuration application))
         (method        (gethash "acp_method" request))
         (params        (if method (gethash "params" request) request))
         (command       (gethash "command" request))
         (arguments     (unless method (parent-standard--arguments request)))
         (control       (rest (assoc command '(("/fast" . :codex-fast-mode-p)
                                               ("/trace" . :reasoning-traces-p)
                                               ("/timestamps" . :turn-timestamps-p)
                                               ("/ste" . :simple-technical-english-p)
                                               ("/titles" . :session-title-generation-p)
                                               ("/cache-misses" . :cache-miss-notices-p)
                                               ("/hurry-up" . :hurry-up-p)) :test #'equal))))
    (parent-standard--check-cancelled cancelled)
    (cond
      ((equal method "settings")
       (parent-standard--map "items" (parent-standard--setting-items configuration)))
      ((equal method "set-setting")
       (unless (hash-table-p params) (parent-standard--reject ':invalid-setting-request))
       (let* ((name (gethash "name" params))
              (key  (and (stringp name) (parent-cli-public-setting-key name))))
         (unless (and key (nth-value 1 (gethash "value" params)))
           (parent-standard--reject ':missing-setting-value))
         (let ((result (parent-standard--store
                        application (list key (parent-standard--setting-value key (gethash "value" params)))
                        cancelled :persistent-p (parent-standard--boolean (gethash "persistent" params t)))))
           (setf (gethash "setting" result)
                 (find name (parent-standard--setting-items configuration)
                       :key (lambda (item) (gethash "name" item)) :test #'equal))
           result)))
      (control
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-control-arguments))
       (let* ((mode    (if arguments (first arguments) ':status))
              (current (getf (parent-configuration-get configuration ':settings)
                             control (getf *parent-cli-setting-defaults* control))))
         (if (eq mode ':status)
             (parent-standard--settings-result
              configuration (format nil "~A is ~A." command (if current "on" "off")))
             (parent-standard--store
              application (list control (if (eq mode ':toggle) (not current)
                                            (parent-standard--boolean mode))) cancelled))))
      ((member command '("/settings" "/preferences" "/config") :test #'equal)
       (when arguments (parent-standard--reject ':invalid-settings-arguments))
       (let* ((items (parent-standard--setting-items configuration))
              (name  (parent-standard--choose frontend "settings"
                                               (map 'list (lambda (item) (gethash "name" item)) items) :cancelled cancelled)))
         (if name
             (let* ((key     (parent-cli-public-setting-key name))
                    (default (getf *parent-cli-setting-defaults* key))
                    (value   (parent-standard--choose
                              frontend name
                              (cond ((member default '(nil t))
                                     '("on" "off"))
                                    ((eq key ':terminal-theme)
                                     '("autolith" "almighty"))
                                    ((eq key ':knowledge-mode)
                                     '("active" "passive" "off"))
                                    ((eq key ':web-search-mode)
                                     '("disabled" "cached" "live"))
                                    (t
                                     nil)) :cancelled cancelled)))
              (unless (or value (member default '(nil t))
                          (member key '(:terminal-theme :knowledge-mode :web-search-mode)))
                (multiple-value-bind (text status)
                    (parent-terminal-ui-read frontend name :cancelled cancelled)
                  (when (eq status ':line)
                    (setf value
                          (cond
                            ((integerp default)
                             (handler-case (parse-integer text :junk-allowed nil)
                               (parse-error () (parent-standard--reject ':invalid-integer))))
                            ((realp default)
                             (parent-cli--environment-value key text))
                            (t
                             text))))))
              (if value
                  (parent-standard--store application
                                          (list key (parent-standard--setting-value key value)) cancelled)
                  (parent-standard--settings-result configuration)))
             (parent-standard--settings-result configuration))))
      (t
       (parent-standard--reject ':unknown-preferences-operation)))))

(serapeum/types:-> parent-standard--model-items (parent-application &optional (or null string)) vector)
(defun parent-standard--model-items (application &optional provider)
  "Return public installed model identities, without account or endpoint data."
  (let ((items nil))
    (maphash
     (lambda (identifier profile)
       (let* ((slash (position #\/ identifier))
              (name  (and slash (subseq identifier 0 slash))))
         (when (or (null provider) (equal provider name))
           (push (parent-standard--map
                  "id" identifier "provider" (or name "") "model" (model-profile-model profile)
                  "reasoningEfforts" (coerce (model-profile-reasoning-efforts profile) 'vector)) items))))
     (parent-application-profiles application))
    (coerce (sort items #'string< :key (lambda (item) (gethash "id" item))) 'vector)))

(serapeum/types:-> parent-standard--select-model
    (parent-application string function &key (:provider (or null string))) hash-table)
(defun parent-standard--select-model (application identifier cancelled &key provider)
  "Validate a public model, then change its bindings only after native cleanup."
  (let* ((configuration (parent-application-configuration application))
         (registry      (parent-application-provider-registry application)))
    (multiple-value-bind (model name registration)
        (parent-provider-resolve-model registry identifier :provider-name provider)
      (declare (ignore registration))
      (parent-standard--check-cancelled cancelled)
      (when (getf (parent-configuration-get configuration ':settings) :immutable-p)
        (parent-cli--reject ':immutable-configuration))
      (parent-application-defer
       application
       (lambda ()
         (parent-cli-store-settings configuration (list :model model :model-provider name))
         (let* ((settings (parent-configuration-get configuration ':settings))
                (values   (copy-list (parent-configuration-values configuration))))
           (multiple-value-bind (profiles selected)
               (parent-provider-configuration-profiles settings :registry registry)
             (setf (getf values :profiles) profiles
                   (getf values :profile-id) selected
                   (slot-value configuration 'values) values))
           (multiple-value-bind (profiles accounts)
               (parent-provider-build-bindings configuration settings :registry registry)
             (setf (slot-value application 'profiles) profiles
                   (slot-value application 'accounts) accounts)))))
      (parent-standard--map "model" model "provider" name "restart" t))))

(serapeum/types:-> parent-standard--model
    (parent-application hash-table function &key (:frontend (or null parent-terminal-ui))) hash-table)
(defun parent-standard--model (application request cancelled &key frontend)
  "List models or change model and reasoning through approved public values."
  (let* ((method    (gethash "acp_method" request))
         (command   (gethash "command" request))
         (arguments (unless method (parent-standard--arguments request)))
         (params    (and method (gethash "params" request))))
    (parent-standard--check-cancelled cancelled)
    (cond
      ((or (equal method "models") (equal command "/models"))
       (let ((provider (if method (and (hash-table-p params) (gethash "provider" params))
                           (and (eq (first arguments) ':provider) (second arguments)))))
         (unless (or (null provider) (stringp provider)) (parent-standard--reject ':invalid-provider))
         (let ((items (parent-standard--model-items application provider)))
           (parent-standard--map "items" items
                                 "output" (format nil "~{~A~%~}" (map 'list (lambda (item) (gethash "id" item)) items))))))
      ((equal command "/model")
       (let* ((provided (oddp (length arguments)))
              (model    (and provided (first arguments)))
              (options  (if provided (rest arguments) arguments))
              (provider (when options
                          (unless (and (= (length options) 2) (eq (first options) ':provider)
                                       (stringp (second options)))
                            (parent-standard--reject ':invalid-model-arguments))
                          (second options))))
         (unless (or (null model) (stringp model)) (parent-standard--reject ':invalid-model))
         (unless (or model provided)
           (setf model (parent-standard--choose
                        frontend "model"
                        (map 'list (lambda (item) (gethash "id" item))
                             (parent-standard--model-items application provider)) :cancelled cancelled)))
         (if model (parent-standard--select-model application model cancelled :provider provider)
             (parent-standard--settings-result (parent-application-configuration application)))))
      ((member command '("/effort" "/reasoning" "/summary" "/reasoning-summary") :test #'equal)
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-reasoning-arguments))
       (let* ((configuration (parent-application-configuration application))
              (profile       (gethash (parent-configuration-get configuration ':profile-id)
                                     (parent-application-profiles application)))
              (summary-p     (member command '("/summary" "/reasoning-summary") :test #'equal))
              (choices       (if summary-p (model-profile-reasoning-summaries profile)
                                 (model-profile-reasoning-efforts profile)))
              (value         (if arguments (first arguments)
                                 (parent-standard--choose frontend "reasoning" choices :cancelled cancelled))))
         (when (and value (not (member value choices :test #'equal)))
           (parent-standard--reject ':unsupported-reasoning))
         (if value
             (parent-standard--store application
                                     (list (if summary-p ':reasoning-summary ':reasoning-effort) value) cancelled)
             (parent-standard--settings-result configuration))))
      (t
       (parent-standard--reject ':unknown-model-operation)))))

(serapeum/types:-> parent-provider-operation-handlers
    (parent-application &key (:frontend (or null parent-terminal-ui))) list)
(defun parent-provider-operation-handlers (application &key frontend)
  "Bind fixed provider operations to private parent state and modal input."
  (list
   (list ':model (lambda (arguments cancelled)
                   (parent-standard--model application arguments cancelled :frontend frontend)))
   (list ':auth (lambda (request cancelled)
                  (parent-standard--auth application request cancelled :frontend frontend)))))

(serapeum/types:-> parent-cli-operation-handlers
    (parent-application &key (:frontend (or null parent-terminal-ui))) list)
(defun parent-cli-operation-handlers (application &key frontend)
  "Bind fixed public settings and permissions operations."
  (list
   (list ':preferences (lambda (arguments cancelled)
                         (parent-standard--preferences application arguments cancelled :frontend frontend)))
   (list ':permissions (lambda (arguments cancelled)
                         (parent-standard--permissions application arguments cancelled :frontend frontend)))
   (list ':web (lambda (arguments cancelled)
                 (parent-web-operation application arguments cancelled)))))


;;;; -- Trusted Authentication Commands --

(serapeum/types:-> parent-standard--provider-names (cl-llm-provider-api:provider-registry) list)
(defun parent-standard--provider-names (registry)
  "List built-in and explicit trusted extension names without creating providers."
  (sort (remove-duplicates
         (append '("chatgpt" "openai-api" "grok" "gemini" "nous" "copilot"
                   "anthropic" "mistral" "fireworks" "openrouter" "opencode")
                 (mapcar #'cl-llm-provider-api:provider-registration-name
                         (cl-llm-provider-api:provider-registry-registrations registry)))
         :test #'equal) #'string<))

(serapeum/types:-> parent-standard--authenticate
    (parent-configuration cl-llm-provider-api:provider-registry string
     &key (:method (or null keyword)) (:input stream) (:output stream)
          (:cancelled function) (:frontend (or null parent-terminal-ui))) boolean)
(defun parent-standard--authenticate
    (configuration registry name &key method (input *standard-input*) (output *standard-output*)
                                      (cancelled (constantly nil)) frontend)
  "Authenticate on parent-owned input and never return credential-bearing output."
  (parent-standard--check-cancelled cancelled)
  (let ((registration (cl-llm-provider-api:provider-registry-find registry name)))
    (flet ((authenticate ()
             (parent-standard--check-cancelled cancelled)
             (if registration
                 (let ((*standard-input* input) (*query-io* (make-two-way-stream input output)))
                   (parent-provider-authenticate
                    registry name (parent-configuration-get configuration ':settings)
                    :stream output :method method))
                 (parent-provider-login
                  configuration name :method method :input input :output output :cancelled cancelled
                  :read-secret (when frontend
                                 (lambda (prompt)
                                   (parent-terminal-ui-read frontend prompt
                                                            :secret-p t :cancelled cancelled)))))))
      (if frontend
          (parent-terminal-ui-call-with-exclusive-input frontend #'authenticate)
          (authenticate))))
  (parent-standard--check-cancelled cancelled)
  t)

(serapeum/types:-> parent-standard--auth
    (parent-application hash-table function &key (:frontend (or null parent-terminal-ui))) hash-table)
(defun parent-standard--auth (application request cancelled &key frontend)
  "Authenticate a selected provider, or list public provider identities."
  (let* ((command   (gethash "command" request))
         (arguments (parent-standard--arguments request))
         (registry  (parent-application-provider-registry application))
         (names     (parent-standard--provider-names registry)))
    (parent-standard--check-cancelled cancelled)
    (cond
      ((member command '("/providers" "/accounts") :test #'equal)
       (when arguments (parent-standard--reject ':invalid-provider-list-arguments))
       (parent-standard--map "items" (coerce names 'vector)
                             "output" (format nil "~{~A~%~}" names)))
      ((equal command "/auth")
       (when (> (length arguments) 2) (parent-standard--reject ':invalid-auth-arguments))
       (let ((name   (if arguments (first arguments)
                        (parent-standard--choose frontend "authenticate provider" names :cancelled cancelled)))
             (method (second arguments)))
         (unless (or (null name) (and (stringp name) (member name names :test #'equal)))
           (parent-standard--reject ':unknown-provider))
         (unless (member method '(nil :browser :device :device-code))
           (parent-standard--reject ':invalid-authentication-method))
         (when name
           (parent-standard--authenticate (parent-application-configuration application) registry name
                                          :method method :cancelled cancelled :frontend frontend))
         (parent-standard--map "authenticated" (not (null name)))))
      (t
       (parent-standard--reject ':unknown-auth-operation)))))

(serapeum/types:-> parent-provider-auth-main
    (parent-configuration parent-cli-request &key (:input stream) (:output stream)) (integer 0 255))
(defun parent-provider-auth-main (configuration request &key (input *standard-input*) (output *standard-output*))
  "Run the public auth CLI entirely in the trusted parent, without starting a child."
  (unless (eq (parent-cli-request-command request) ':auth)
    (parent-standard--reject ':invalid-auth-command))
  (let* ((arguments (parent-cli-request-arguments request))
         (registry  (or (parent-configuration-provider-registry configuration)
                        (nth-value 2
                                   (parent-provider-build-bindings
                                    configuration (parent-configuration-get configuration ':settings)))))
         (names     (parent-standard--provider-names registry))
         (name      (first arguments))
         (method    (and (second arguments)
                         (find (second arguments) '(:browser :device :device-code)
                               :key #'symbol-name :test #'string-equal))))
    (when (> (length arguments) 2) (parent-standard--reject ':invalid-auth-arguments))
    (when (and (second arguments) (null method))
      (parent-standard--reject ':invalid-authentication-method))
    (unless name
      (format output "Providers: ~{~A~^, ~}~%Provider: " names)
      (finish-output output)
      (setf name (read-line input nil nil)))
    (unless (and name (member name names :test #'equal))
      (parent-standard--reject ':unknown-provider))
    (parent-standard--authenticate configuration registry name :method method :input input :output output)
    (write-line "Authentication saved." output)
    (finish-output output)
    0))


;;;; -- Session Selection and Native Restart --

(serapeum/types:-> parent-standard--defer-session (parent-application string function) hash-table)
(defun parent-standard--defer-session (application identifier cancelled)
  "Change session identity only after the old native process has stopped."
  (parent-session--component identifier)
  (parent-standard--check-cancelled cancelled)
  (parent-application-switch-session application identifier)
  (parent-standard--map "session_id" identifier "restart" t))

(serapeum/types:-> parent-standard--session
    (parent-application hash-table function &key (:frontend (or null parent-terminal-ui))) hash-table)
(defun parent-standard--session (application request cancelled &key frontend)
  "Inspect, fork, replay or replace a session from parent-owned durable data."
  (let* ((configuration (parent-application-configuration application))
         (method        (gethash "acp_method" request))
         (command       (gethash "command" request))
         (arguments     (unless method (parent-standard--arguments request))))
    (parent-standard--check-cancelled cancelled)
    (cond
      ((or (equal method "conversations") (equal command "/conversations"))
       (parent-standard--map
        "items" (coerce (mapcar (lambda (identifier)
                                  (parent-standard--map "id" identifier
                                                        "current" (equal identifier (parent-application-session-id application))))
                                (parent-session-list configuration)) 'vector)))
      ((equal command "/new")
       (when arguments (parent-standard--reject ':invalid-new-arguments))
       (parent-standard--defer-session application (parent-application--identifier) cancelled))
      ((equal command "/resume")
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-resume-arguments))
       (let ((identifier (if arguments (first arguments)
                             (parent-standard--choose frontend "resume session"
                                                     (parent-session-list configuration) :cancelled cancelled))))
         (if identifier
             (progn
               (unless (stringp identifier) (parent-standard--reject ':invalid-session-id))
               (parent-session-ensure configuration identifier)
               (parent-standard--defer-session application identifier cancelled))
             (parent-standard--map "cancelled" t))))
      ((equal command "/fork")
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-fork-arguments))
       (let* ((source (or (first arguments) (parent-application-session-id application)))
              (fork   (parent-session-fork configuration source)))
         (parent-standard--defer-session application (session-store-id fork) cancelled)))
      ((equal command "/replay")
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-replay-arguments))
       (let* ((identifier (or (first arguments) (parent-application-session-id application)))
              (store      (parent-session-ensure configuration identifier :read-only-p t))
              (replay     (parent-session-replay-open store)))
         (unless frontend (parent-standard--reject ':interactive-terminal-required))
         (parent-terminal-ui-call-with-exclusive-input
          frontend
          (lambda ()
            (loop
              (parent-standard--check-cancelled cancelled)
              (parent-terminal-ui-event frontend ':finalized
                                        (list :role    ':system
                                              :content (with-output-to-string (output)
                                                         (parent-session-replay-write replay output))))
              (multiple-value-bind (line status)
                  (parent-terminal-ui-read frontend "replay>" :cancelled cancelled)
                (unless (eq status ':line) (return))
                (let ((continue nil))
                  (parent-terminal-ui-event
                   frontend ':finalized
                   (list :role    ':system
                         :content (with-output-to-string (output)
                                    (setf continue (parent-session-replay-command replay line output)))))
                  (unless continue (return)))))))
         (parent-standard--map "replayed" identifier)))
      (t
       (parent-standard--reject ':unknown-session-operation)))))

(serapeum/types:-> parent-session-operation-handlers
    (parent-application &key (:frontend (or null parent-terminal-ui))) list)
(defun parent-session-operation-handlers (application &key frontend)
  "Bind fixed session and restart operations to native cleanup and durable stores."
  (list
   (list ':session (lambda (arguments cancelled)
                     (parent-standard--session application arguments cancelled :frontend frontend)))
   (list ':restart (lambda (arguments cancelled)
                     (unless (or (zerop (hash-table-count arguments))
                                 (equal (gethash "operation" arguments) "restart"))
                       (parent-standard--reject ':unknown-restart-operation))
                     (parent-standard--check-cancelled cancelled)
                     (bordeaux-threads:with-lock-held ((parent-application-lock application))
                       (setf (parent-application-restart application) t))
                     (parent-standard--map "restart" t)))))


;;;; -- Parent Tool Authorization --

(serapeum/types:-> parent-standard--permission-mode (parent-application) string)
(defun parent-standard--permission-mode (application)
  "Read the current trusted session command permission mode."
  (let ((mode (getf (parent-configuration-get (parent-application-configuration application) ':settings)
                    :permission-mode "ask")))
    (cond
      ((member mode '("ask" "auto" "sandbox" "full") :test #'equal)
       mode)
      ((member mode '(:ask :auto :sandbox :full))
       (string-downcase mode))
      ((eq mode ':full-access)
       "full")
      (t
       (parent-standard--reject ':invalid-permission-mode)))))

(serapeum/types:-> parent-standard--set-permission-mode (parent-application string function) hash-table)
(defun parent-standard--set-permission-mode (application mode cancelled)
  "Replace native policy after shutdown through the controller's mode contract."
  (unless (member mode '("ask" "auto" "sandbox" "full") :test #'equal)
    (parent-standard--reject ':invalid-permission-mode))
  (parent-standard--check-cancelled cancelled)
  (parent-application-set-permission-mode application mode)
  (parent-standard--map "mode" mode "restart" t))

(serapeum/types:-> parent-standard--permissions
    (parent-application hash-table function &key (:frontend (or null parent-terminal-ui))) hash-table)
(defun parent-standard--permissions (application request cancelled &key frontend)
  "Handle modal session access changes and the fixed external tool admission query."
  (let* ((operation (gethash "operation" request))
         (method    (gethash "acp_method" request))
         (params    (and method (gethash "params" request)))
         (arguments (unless (or operation method) (parent-standard--arguments request))))
    (parent-standard--check-cancelled cancelled)
    (cond
      ((equal operation "authorize-tool")
       (parent-standard--map
        "allowed" (parent-application-authorize-tool
                   application (gethash "tool" request) (gethash "arguments" request)
                   :cancelled cancelled)))
      ((or (equal method "set-permission-mode") (equal (gethash "command" request) "/permissions"))
       (when (> (length arguments) 1) (parent-standard--reject ':invalid-permission-arguments))
       (let ((mode (cond (method
                          (and (hash-table-p params) (or (gethash "mode" params)
                                                        (gethash "modeId" params))))
                         (arguments
                          (first arguments))
                         (t
                          (parent-standard--choose frontend "command permissions"
                                                  '("ask" "auto" "sandbox" "full" "list" "clear") :cancelled cancelled)))))
         (when (keywordp mode) (setf mode (string-downcase mode)))
         (cond
           ((or (null mode) (equal mode "list"))
            (parent-standard--map "mode" (parent-standard--permission-mode application)))
           ((equal mode "clear")
            (let ((policy (parent-application-policy application)))
              (when policy
                (dolist (record (access-service-notes policy))
                  (when (eq (getf record :kind) ':approved)
                    (parent-application-revoke application (getf record :operation-id))))))
            (parent-standard--map "mode" (parent-standard--permission-mode application) "restart" t))
           (t
            (parent-standard--set-permission-mode application mode cancelled)))))
      (t
       (parent-standard--reject ':unknown-permissions-operation)))))

(serapeum/types:-> parent-standard--user-instructions (parent-application) string)
(defun parent-standard--user-instructions (application)
  "Read the latest durable user message for the parent's permission classifier."
  (let* ((store   (session-store-open (parent-application--path application "session.sexp")
                                      (parent-application-session-id application)
                                      :recover-interrupted nil :read-only-p t))
         (records (parent-session-snapshot store)))
    (or (loop for record in (reverse records)
              when (eq (fourth record) ':user-input)
                return (second (session-store--field (fifth record) "text")))
        "unavailable; do not infer user approval")))

(serapeum/types:-> parent-standard--classify-tool
    (parent-application string hash-table &key (:cancelled function)) boolean)
(defun parent-standard--classify-tool (application name arguments &key (cancelled (constantly nil)))
  "Classify an external call through the private executor; malformed output denies."
  (handler-case
      (let* ((configuration (parent-application-configuration application))
             (profile       (gethash (parent-configuration-get configuration ':profile-id)
                                     (parent-application-profiles application)))
             (prompt        (cl-rfc8628:json-encode
                             (parent-standard--map
                              "current user instructions" (parent-standard--user-instructions application)
                              "command" (parent-standard--map "external_tool" name "arguments" arguments)
                              "working directory" (namestring (parent-configuration-get configuration ':workspace))
                              "workspace sandbox" "unavailable; external tools cannot use the local sandbox")))
             (request       (make-instance 'cl-llm-provider-api:wire-request
                                          :model   (model-profile-model profile)
                                           :prefix (list *permissions-model-decision-guidance*
                                                        "Return exactly one JSON object with decision (full, sandboxed, deny) and reason. Do not use tools.")
                                          :suffix  (list prompt)))
             (invocation    (make-instance 'model-invocation
                                          :inputs   (list request #() profile ':generate nil nil nil)
                                          :wire-id 1 :request-id (parent-application--identifier)
                                          :turn-id  (parent-application--identifier)
                                          :deadline (+ (session-runtime--clock) 60)))
             (text          (make-string-output-stream))
             (completed nil) (characters 0)
             (done nil) (lock (bordeaux-threads:make-lock "parent classifier")))
        (parent-standard--check-cancelled cancelled)
        (multiple-value-bind (execute abort-invocation)
            (parent-provider-model-executor :accounts         (parent-application-accounts application)
                                            :session-id       (parent-application-session-id application)
                                            :maximum-attempts 2)
          (let ((watcher (bordeaux-threads:make-thread
                          (lambda ()
                            (loop
                              (when (bordeaux-threads:with-lock-held (lock) done) (return))
                              (when (or (funcall cancelled)
                                        (>= (session-runtime--clock) (model-invocation-deadline invocation)))
                                (funcall abort-invocation invocation)
                                (return))
                              (sleep 0.01)))
                          :name "parent classifier cancellation")))
            (unwind-protect
                 (funcall execute invocation
                          (lambda (event)
                            (parent-standard--check-cancelled cancelled)
                            (typecase event
                              (cl-llm-provider-api:assistant-delta-event
                               (let ((delta (cl-llm-provider-api:assistant-delta-event-text event)))
                                 (incf characters (length delta))
                                 (when (> characters 16384) (parent-standard--reject ':classifier-output-limit))
                                 (write-string delta text)))
                              (cl-llm-provider-api:provider-retry-event
                               (get-output-stream-string text)
                               (setf characters 0 completed nil))
                              (cl-llm-provider-api:provider-completed-event
                               (setf completed t)))))
              (bordeaux-threads:with-lock-held (lock) (setf done t))
              (bordeaux-threads:join-thread watcher))))
        (parent-standard--check-cancelled cancelled)
        (let ((value (cl-rfc8628:json-decode (get-output-stream-string text))))
          (and completed (hash-table-p value) (= (hash-table-count value) 2)
               (equal (gethash "decision" value) "full")
               (stringp (gethash "reason" value)) t)))
    (error ()
      nil)))

(serapeum/types:-> parent-application-authorize-tool
    (parent-application string hash-table &key (:cancelled function)) boolean)
(defun parent-application-authorize-tool (application name arguments &key (cancelled (constantly nil)))
  "Authorize an external tool through the current trusted owner permission mode.
External calls cannot use the local command sandbox. Auto classification uses the
same decision guidance as shell permission classification and fails closed."
  (unless (and (plusp (length name)) (<= (length name) 1024) (hash-table-p arguments))
    (parent-standard--reject ':invalid-tool-authorization))
  (protocol-data->map arguments)
  (when (funcall cancelled) (return-from parent-application-authorize-tool nil))
  (let ((mode (parent-standard--permission-mode application)))
    (cond
      ((equal mode "full")
       t)
      ((equal mode "sandbox")
       nil)
      ((equal mode "auto")
       (parent-standard--classify-tool application name arguments :cancelled cancelled))
      (t
       (let* ((terminal (parent-application--terminal application))
              (answer   (parent-terminal-approve
                         terminal
                         (list (parent-proto:make-grant :resource ':service :target name
                                                         :operations '("call") :duration ':instance))
                         :operation-id "external-tool"
                         :reason (cl-rfc8628:json-encode arguments) :cancelled cancelled)))
         (and (not (funcall cancelled)) (not (null answer))))))))
