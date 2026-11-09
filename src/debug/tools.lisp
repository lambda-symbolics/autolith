(in-package #:autolith)

;;;; -- Optional Semantic Debug Tools --

(defclass debug-tool (tool)
  ((operation :initarg :operation :reader debug-tool-operation
              :documentation "Explicit semantic debugger operation.")
   (manager :initarg :manager :reader debug-tool-manager
            :documentation "Shared bounded session runtime.")
   (program :initarg :program :reader debug-tool-program
            :documentation "User-configured adapter executable or NIL.")
   (arguments :initarg :arguments :reader debug-tool-arguments
              :documentation "User-configured literal adapter arguments.")
   (cancel-p :initarg :cancel-p :reader debug-tool-cancel-p
             :documentation "Optional supervisor cancellation predicate."))
  (:documentation "Opt-in semantic DAP operations with ordinary tool authority."))

(defmethod tool-runtime-identity ((tool debug-tool))
  "Group all debug tools by their shared session manager."
  (debug-tool-manager tool))

(defmethod tool-runtime-close ((tool debug-tool))
  "Reap the registry's adapters before runtime shutdown."
  (debug-session-manager-close (debug-tool-manager tool)))

(defmethod tool-runtime-detach ((tool debug-tool))
  "Drop inherited references without signalling adapter processes owned by the parent."
  (with-lock-held ((debug-session-manager-lock (debug-tool-manager tool)))
    (clrhash (debug-session-manager-entries (debug-tool-manager tool)))
    (setf (debug-session-manager-closed-p (debug-tool-manager tool)) t))
  nil)

(defmethod tool-runtime-resume ((tool debug-tool) (registry tool-registry))
  "Reopen a quiesced runtime without resurrecting old debug sessions."
  (declare (ignore registry))
  (with-lock-held ((debug-session-manager-lock (debug-tool-manager tool)))
    (setf (debug-session-manager-closed-p (debug-tool-manager tool)) nil))
  nil)

(defvar *debug-argument-schema* nil
  "Field contracts bound for the currently executing debug tool.")

(-> debug--argument (hash-table string) t)
(defun debug--argument (arguments name)
  "Read one checked/defaulted field, translating payload-free diagnostics."
  (multiple-value-bind (value supplied-p diagnostic)
      (cl-llm-provider-api:provider-schema-argument-value
       *debug-argument-schema* arguments name)
    (declare (ignore supplied-p))
    (when diagnostic
      (debug--fail ':invalid-arguments
                   (format nil "Invalid ~A argument (~A)." name
                           (cl-llm-provider-api:provider-argument-diagnostic-code diagnostic))))
    value))

(-> debug--validate-arguments (debug-tool hash-table) null)
(defun debug--validate-arguments (tool arguments)
  "Check used fields before session reservation, approval or adapter effects."
  (let* ((operation             (debug-tool-operation tool))
         (startup-p             (member operation '("launch" "attach") :test #'equal))
         (initial-breakpoints-p (and startup-p
                                     (or (gethash "breakpoint-path" arguments)
                                         (gethash "breakpoint-lines" arguments))))
         (properties            (json-get *debug-argument-schema* "properties")))
    (when initial-breakpoints-p
      (tool-argument arguments "breakpoint-path" :required t)
      (tool-argument arguments "breakpoint-lines" :required t))
    (maphash
     (lambda (name schema)
       (declare (ignore schema))
       (unless (or (and (equal operation "cancel") (equal name "timeout"))
                   (and startup-p (not initial-breakpoints-p)
                        (member name '("breakpoint-path" "breakpoint-lines") :test #'equal))
                   (and (equal operation "events")
                      (if (tool-argument arguments "wait-for")
                          (equal name "count")
                          (equal name "wait-for")))
                   (and (equal operation "evaluate") (equal name "frame")
                        (not (tool-argument arguments name))))
         (debug--argument arguments name)))
     properties))
  nil)

(-> debug--breakpoints
    (tool-context hash-table &key (:path-name string) (:lines-name string)) (values json-object vector))
(defun debug--breakpoints (context arguments &key (path-name "path") (lines-name "lines"))
  "Authorize a source path and validate a bounded semantic line breakpoint set."
  (tool-argument arguments lines-name :required t)
  (let ((lines (debug--argument arguments lines-name))
        (path  (workspace-tool-path context (tool-argument arguments path-name :required t) :tool-name "debug")))
    (values (json-object "path" (uiop:native-namestring path))
            (map 'vector (lambda (line) (json-object "line" line)) lines))))

(-> debug--events (debug-session-entry hash-table &key (:timeout integer) (:cancel-p function)) t)
(defun debug--events (entry arguments &key timeout cancel-p)
  "Present finite event pages, retaining unpresented events rather than silently dropping them."
  (let ((session (debug-session-entry-session entry))
        (name    (when (tool-argument arguments "wait-for") (debug--argument arguments "wait-for"))))
    (cond
      (name
       (let ((event (find name (debug-session-entry-events entry)
                          :key (lambda (event) (json-get event "event")) :test #'equal)))
         (when event
           (debug--bounded-json event)
           (setf (debug-session-entry-events entry)
                 (delete event (debug-session-entry-events entry) :count 1)))
         (or event (daphne:session-wait-event session name :timeout timeout :cancel-p cancel-p))))
      (t
       (unless (debug-session-entry-events entry)
         (setf (debug-session-entry-events entry) (daphne:session-events session)))
       (let* ((pending (debug-session-entry-events entry))
              (count   (min (length pending) (debug--argument arguments "count")))
              (page    (json-object "events" (coerce (subseq pending 0 count) 'vector)
                                    "bufferedRemaining" (- (length pending) count))))
         (debug--bounded-json page)
         (setf (debug-session-entry-events entry) (nthcdr count pending))
         page)))))

(-> debug--start (debug-tool tool-context hash-table) json-object)
(defun debug--start (tool context arguments)
  "Authorize exact launch/attach arguments, reserve admission and initialize one adapter."
  (let* ((manager           (debug-tool-manager tool))
         (operation         (debug-tool-operation tool))
         (program           (debug-tool-program tool))
         (configuration     (debug--configuration context (tool-argument arguments "configuration" :required t)))
         (timeout           (debug--argument arguments "timeout"))
         (source            nil)
         (breakpoints       nil)
         (breakpoint-result nil))
    (when (or (gethash "breakpoint-path" arguments) (gethash "breakpoint-lines" arguments))
      (multiple-value-setq (source breakpoints)
        (debug--breakpoints context arguments :path-name "breakpoint-path" :lines-name "breakpoint-lines")))
    (unless program (debug--fail ':unavailable "Debug adapter is disabled; configure an explicit executable."))
    (let* ((path      (workspace-tool-path context program :tool-name "debug"))
           (directory (config :working-directory (tool-context-configuration context))))
      (unless (uiop:file-exists-p path)
        (debug--fail ':unavailable "Configured debug adapter executable is unavailable."))
      (debug--authorize context operation configuration)
      (debug--authorize context "adapter"
                        (json-object "program" (uiop:native-namestring path)
                                     "arguments" (coerce (debug-tool-arguments tool) 'vector)
                                     "directory" (namestring directory)))
      (let ((entry (debug--reserve manager context)) (completed-p nil))
        (unwind-protect
             (progn
               (when (and (debug-tool-cancel-p tool) (funcall (debug-tool-cancel-p tool)))
                 (error 'daphne:dap-cancelled :message "Debug startup was cancelled before adapter execution."))
               (with-lock-held ((debug-session-manager-lock manager))
                 (when (or (debug-session-manager-closed-p manager)
                           (debug-session-entry-cancelled-p entry))
                   (debug--fail ':cancelled "Debug startup was cancelled before adapter execution."))
                 (multiple-value-bind (session transport)
                     (daphne:start-adapter (uiop:native-namestring path) (debug-tool-arguments tool)
                                           :directory directory :max-body *debug-result-byte-limit*
                                           :max-events 128 :max-pending 4 :stderr-limit 4096)
                   (setf (debug-session-entry-session entry) session
                         (debug-session-entry-transport entry) transport)))
               (let ((cancel  (lambda () (debug--cancel-p manager entry (debug-tool-cancel-p tool))))
                     (session (debug-session-entry-session entry)))
                 (when (funcall cancel) (debug--fail ':cancelled "Debug startup was cancelled."))
                 (daphne:session-initialize session :timeout timeout :cancel-p cancel)
                 (daphne:session-start session (if (equal operation "launch") ':launch ':attach)
                                       configuration :timeout timeout :cancel-p cancel
                                       :configure (when source
                                                    (lambda (session)
                                                     (setf breakpoint-result
                                                           (daphne:session-set-breakpoints session source breakpoints
                                                                                           :timeout timeout :cancel-p cancel)))))
                 (let ((result (json-object "session" (debug-session-entry-identifier entry)
                                            "state" (string-downcase (symbol-name (daphne:session-state session)))
                                          "capabilities" (daphne:session-capabilities session)
                                          "breakpoints" breakpoint-result)))
                   (debug--bounded-json result)
                   (setf completed-p t)
                   result)))
          (with-lock-held ((debug-session-manager-lock manager))
            (setf (debug-session-entry-busy-p entry) nil))
          (unless completed-p (debug--remove manager entry)))))))

(-> debug--operation (debug-tool tool-context hash-table &key (:entry debug-session-entry)) t)
(defun debug--operation (tool context arguments &key entry)
  "Run one semantic operation against an exclusively reserved session."
  (let* ((session   (debug-session-entry-session entry))
         (operation (debug-tool-operation tool))
         (timeout   (debug--argument arguments "timeout"))
         (cancel    (lambda () (debug--cancel-p (debug-tool-manager tool) entry (debug-tool-cancel-p tool))))
         (options   (list :timeout timeout :cancel-p cancel)))
    (when (funcall cancel)
      (error 'daphne:dap-cancelled :message "Debug operation was cancelled before execution."))
    (cond
      ((equal operation "events")
       (debug--events entry arguments :timeout timeout :cancel-p cancel))
      ((equal operation "breakpoints")
       (multiple-value-bind (source breakpoints) (debug--breakpoints context arguments)
         (apply #'daphne:session-set-breakpoints session source breakpoints options)))
      ((equal operation "threads")
       (apply #'daphne:session-threads session options))
      ((equal operation "stack")
       (apply #'daphne:session-stack-trace session (debug--argument arguments "thread")
              :start-frame (debug--argument arguments "start")
              :levels      (debug--argument arguments "count") options))
      ((equal operation "scopes")
       (apply #'daphne:session-scopes session (debug--argument arguments "frame") options))
      ((equal operation "variables")
       (apply #'daphne:session-variables session (debug--argument arguments "reference")
              :start (debug--argument arguments "start")
              :count (debug--argument arguments "count") options))
      ((equal operation "evaluate")
       (let ((expression (debug--argument arguments "expression"))
             (frame      (when (tool-argument arguments "frame") (debug--argument arguments "frame"))))
         (debug--authorize context "evaluate" (json-object "session" (debug-session-entry-identifier entry)
                                                          "expression" expression "frame" frame))
         (apply #'daphne:session-evaluate session expression :frame-id frame options)))
      ((equal operation "continue")
       (apply #'daphne:session-continue session (debug--argument arguments "thread") options))
      ((equal operation "pause")
       (apply #'daphne:session-pause session (debug--argument arguments "thread") options))
      ((equal operation "step")
       (let ((kind (debug--argument arguments "kind")))
         (apply #'daphne:session-step session (debug--argument arguments "thread")
                (cond ((equal kind "in")
                       ':in)
                      ((equal kind "over")
                       ':over)
                      (t
                       ':out)) options)))
      ((equal operation "terminate")
       (apply #'daphne:session-terminate session :disconnect t
              :terminate-debuggee                            (json-true-p (debug--argument arguments "terminate-debuggee")) options)
       (json-object "terminated" t))
      (t
       (debug--fail ':unknown-operation "Unknown debug operation.")))))

(-> debug--execute (debug-tool tool-context hash-table) t)
(defun debug--execute (tool context arguments)
  "Dispatch capability inspection, startup, cancellation or bounded session operations."
  (let ((operation (debug-tool-operation tool)) (manager (debug-tool-manager tool)))
    (cond
      ((equal operation "status")
       (json-object "loaded" t "configured" (if (debug-tool-program tool) t (json-false))
                    "sessionLimit" *debug-session-limit* "conversationSessionLimit" *debug-conversation-session-limit*
                    "resultByteLimit" *debug-result-byte-limit* "executionAuthority" "full-access"))
      ((member operation '("launch" "attach") :test #'equal)
       (debug--start tool context arguments))
      (t
       (let* ((cancel-p (equal operation "cancel"))
              (entry    (debug--find manager context (tool-argument arguments "session" :required t) :reserve-p (not cancel-p))))
         (if cancel-p
             (progn
               (with-lock-held ((debug-session-manager-lock manager))
                 (setf (debug-session-entry-cancelled-p entry) t))
               (let ((session (debug-session-entry-session entry)))
                 (when session
                   (daphne:session-close session (make-condition 'daphne:dap-cancelled :message "Debug session cancelled."))))
               (unless (debug-session-entry-busy-p entry) (debug--remove manager entry))
               (json-object "cancelled" t))
             (let ((completed-p nil))
               (unwind-protect
                   (handler-case
                     (let ((body (debug--operation tool context arguments :entry entry)))
                         (debug--bounded-json body)
                         (setf completed-p t)
                         body)
                     (tool-error (condition)
                       (setf completed-p t)
                       (error condition))
                     (daphne:dap-request-error (condition)
                       (setf completed-p t)
                       (error condition))
                     (daphne:dap-state-error (condition)
                       (setf completed-p t)
                       (error condition)))
                 (with-lock-held ((debug-session-manager-lock manager))
                   (setf (debug-session-entry-busy-p entry) nil))
                 (when (or (not completed-p) (equal operation "terminate")
                           (member (daphne:session-state (debug-session-entry-session entry)) '(:closed :failed)))
                   (debug--remove manager entry))))))))))

(defmethod tool-execute ((tool debug-tool) (context tool-context) (arguments hash-table))
  "Invoke semantic debug operations and translate library failures to product errors."
  (let ((*debug-argument-schema* (tool-parameters tool)))
    (handler-case
        (progn
          (debug--validate-arguments tool arguments)
          (tool-success (debug--bounded-json (debug--execute tool context arguments))))
      (daphne:dap-error (condition) (debug--translate condition)))))

(-> debug--properties (string) (values json-object list))
(defun debug--properties (operation)
  "Build precise schemas for semantic debugger tools."
  (let ((properties (json-object)) (required nil))
    (labels ((string-field (name description &optional required-p)
               (setf (gethash name properties) (tool-string-property description))
               (when required-p (push name required)))

             (integer-field (name description &optional required-p)
               (setf (gethash name properties)
                     (json-object "type" "integer" "description" description
                                  "minimum" 0 "maximum" most-positive-fixnum "default" 0))
               (when required-p (push name required))))
      (unless (equal operation "status")
        (integer-field "timeout" "Request deadline in seconds, 1..120; default 10."))
      (cond
        ((member operation '("launch" "attach") :test #'equal)
         (string-field "configuration" "Bounded adapter-specific launch or attach JSON object. Exact content requires execution approval." t)
         (string-field "breakpoint-path" "Optional source path for breakpoints before configurationDone; supply breakpoint-lines too.")
         (setf (gethash "breakpoint-lines" properties)
               (json-object "type" "array" "items" (json-object "type" "integer" "minimum" 1 "maximum" 100000000) "maxItems" 100)))
        ((not (equal operation "status"))
         (string-field "session" "Opaque session ID returned by launch/attach in this conversation." t)))
      (when (member operation '("continue" "pause" "step" "stack") :test #'equal)
        (integer-field "thread" "Thread ID obtained from debug.threads." t))
      (when (equal operation "step") (string-field "kind" "in, over or out." t))
      (when (member operation '("stack" "variables") :test #'equal)
        (integer-field "start" "Zero-based page offset.")
        (integer-field "count" "Page size 1..100."))
      (when (equal operation "scopes") (integer-field "frame" "Frame ID obtained from debug.stack." t))
      (when (equal operation "variables") (integer-field "reference" "Reference obtained from debug.scopes or variables." t))
      (when (equal operation "evaluate")
        (string-field "expression" "Explicit expression execution, potentially mutating; requires independent approval." t)
        (integer-field "frame" "Optional stopped frame ID."))
      (when (equal operation "events")
        (string-field "wait-for" "Optional event name to await; omit for a queued event page.")
        (integer-field "count" "Queued event page size 1..100, default 20; bufferedRemaining counts retained events."))
      (when (equal operation "terminate")
        (setf (gethash "terminate-debuggee" properties) (tool-boolean-property "Request debuggee termination on disconnect; default true.")))
      (when (equal operation "breakpoints")
        (string-field "path" "Authorized source pathname." t)
        (setf (gethash "lines" properties) (json-object "type" "array" "items" (json-object "type" "integer" "minimum" 1 "maximum" 100000000) "maxItems" 100))
        (push "lines" required)))
    (flet ((bounds (name &key minimum maximum default)
             (let ((field (gethash name properties)))
               (when field
                 (setf (gethash "minimum" field) minimum
                       (gethash "maximum" field) maximum
                       (gethash "default" field) default)))))
      (bounds "timeout" :minimum 1 :maximum 120 :default 10)
      (bounds "count" :minimum 1 :maximum 100 :default (if (equal operation "variables") 50 20)))
    (when (equal operation "evaluate")
      (remhash "default" (gethash "frame" properties)))
    (when (equal operation "step")
      (setf (gethash "enum" (gethash "kind" properties)) #("in" "over" "out")))
    (when (equal operation "events")
      (setf (gethash "minLength" (gethash "wait-for" properties)) 1))
    (when (equal operation "terminate")
      (setf (gethash "type" (gethash "terminate-debuggee" properties)) #("boolean" "null")
            (gethash "default" (gethash "terminate-debuggee" properties)) t))
    (values properties (nreverse required))))

(-> debug-register-tools
    (tool-registry &key (:program (option string)) (:arguments list) (:cancel-p (option function))) tool-registry)
(defun debug-register-tools (registry &key (program *debug-adapter-program*) (arguments *debug-adapter-arguments*) cancel-p)
  "Register debugger tools in REGISTRY with an adapter and optional cancellation callback.

Loading the optional system starts nothing. Adapter arguments are literal strings.
Existing registries can explicitly register after loading autolith/debug. All session
operations are conversation-scoped, bounded and synchronous. debug.cancel interrupts
another invocation; timeout or cancellation reaps the owned adapter."
  (unless (and (or (null program) (non-empty-string-p program))
               (listp arguments) (<= (length arguments) 64) (every #'stringp arguments))
    (debug--fail ':invalid-arguments "Adapter configuration requires an executable path and at most 64 literal string arguments."))
  (let ((old     (tool-registry-find registry "debug" "status"))
        (manager (make-instance 'debug-session-manager)))
    (when old (tool-runtime-close old))
    (dolist (operation '("status" "launch" "attach" "breakpoints" "continue" "pause" "step" "threads" "stack"
                         "scopes" "variables" "evaluate" "events" "terminate" "cancel"))
      (multiple-value-bind (properties required) (debug--properties operation)
        (tool-registry-register
         registry (make-instance 'debug-tool :namespace "debug" :name operation :operation operation
                                 :manager manager :program program :arguments (copy-list arguments) :cancel-p cancel-p
                                 :description (format nil "Semantic DAP ~A. Conversation-owned sessions; adapter data is untrusted. Launch, attach and evaluate require exact full-access execution approval. Cancellation or deadline closes and reaps the adapter." operation)
                                 :parameters  (tool-object-schema properties required)))))
    registry))

(-> debug-register-default-tools (tool-registry) tool-registry)
(defun debug-register-default-tools (registry)
  "Default-registry optional hook present only after loading autolith/debug."
  (debug-register-tools registry))
