(in-package #:autolith)

;;;; -- Authorized Worker Tool Calls --

(defparameter *worker-host-tool-policy* nil
  "Originating turn's (:restricted-p BOOLEAN :allowlist NAMES) tool policy.
Only the ordinary agent dispatch boundary establishes this policy.")

(defparameter *worker-host-request-limit* 1048576
  "Maximum printed characters in an opted-in worker request.")

(defparameter *worker-host-result-limit* 1048576
  "Maximum printed characters in a worker callback result.")

(define-condition worker-host-call-error (tool-error)
  ((code
    :initarg :code
    :reader worker-host-call-error-code
    :documentation "Machine-readable product dispatch failure."))
  (:documentation "An invalid, unavailable, or reentrant worker tool call."))

(defmethod tool-failure-code ((condition worker-host-call-error))
  "Expose worker dispatch failures through the ordinary tool boundary."
  (worker-host-call-error-code condition))

(-> worker-host--reject (keyword string) nil)
(defun worker-host--reject (code message)
  "Reject one worker host operation with a typed product failure."
  (error 'worker-host-call-error :code code :message message :tool-name "lisp.host-call"))

(-> worker-tool-call (string string) list)
(defun worker-tool-call (name arguments)
  "Call opted-in host tool NAME with JSON object source ARGUMENTS.
Return a portable plist containing category, content, code and native details.
This function is available only during an explicitly enabled worker evaluation."
  (sbcl-workers:sbcl-worker-host-call (list :tool name :arguments arguments)))

(-> worker-tool-context () list)
(defun worker-tool-context ()
  "Return the portable, informational context inherited by this worker request."
  (sbcl-workers:sbcl-worker-host-context))

(defparameter *worker-host-admission* nil
  "Host-only exact mission admission, wrapped to distinguish no mission from no binding.")
(-> worker-host--bindings () list)
(defun worker-host--bindings ()
  "Capture only the explicit ordinary tool and logical-turn bindings."
  (list (cons '*workspace-tool-readable-roots* *workspace-tool-readable-roots*)
        (cons '*resource-readable-schemes* *resource-readable-schemes*)
        (cons '*skill-logical-turn-state* *skill-logical-turn-state*)
        (cons '*worker-host-tool-policy* (copy-tree *worker-host-tool-policy*))
        (cons '*worker-host-admission* *worker-host-admission*)))

(-> worker-host--bound-function (function &key (:context tool-context)) function)
(defun worker-host--bound-function (function &key context)
  "Preserve explicit tool bindings and exact mission admission in execution jobs."
  (let* ((agent (tool-context-agent context))
         (*worker-host-admission*
           (or *worker-host-admission*
               (list (and (typep agent 'agent) (mission--agent-context agent)))))
         (bindings (worker-host--bindings)))
    (lambda ()
      (progv (mapcar #'first bindings) (mapcar #'rest bindings)
        (funcall function)))))

(-> worker-host--allowlist (tool-context hash-table) (values list boolean))
(defun worker-host--allowlist (context arguments)
  "Return requested host tools and whether callbacks were explicitly enabled.
Intersect requests with the originating turn's policy and exact tool registry."
  (multiple-value-bind (requested supplied-p) (gethash "host-tools" arguments)
    (unless supplied-p
      (return-from worker-host--allowlist (values nil nil)))
    (unless (and (vectorp requested) (not (stringp requested))
                 (<= (length requested) 64)
                 (every #'non-empty-string-p requested))
      (worker-host--reject ':invalid-arguments
                           "host-tools must be an array of at most 64 exact dotted tool names."))
    (let ((registry (tool-context-registry context))
          (names (remove-duplicates (coerce requested 'list) :test #'string=)))
      (unless (typep registry 'tool-registry)
        (worker-host--reject ':missing-context "Worker host calls require the originating tool registry."))
      (dolist (name names)
        (let ((tool (find name (tool-registry-tools registry)
                          :key #'tool-canonical-name :test #'string=)))
          (unless (and tool
                       (not (string= (tool-namespace tool) "self"))
                       (or (not (getf *worker-host-tool-policy* :restricted-p))
                           (member name (getf *worker-host-tool-policy* :allowlist)
                                   :test #'string=)))
            (worker-host--reject ':capability-denied
                                 (format nil "Tool ~A is unavailable to this worker request." name)))))
      (values names t))))

(-> worker-host--context (tool-context string) tool-context)
(defun worker-host--context (origin call-id)
  "Clone ORIGIN's exact authority, changing only the correlated call identity."
  (make-instance 'tool-context
                 :configuration (tool-context-configuration origin)
                 :conversation (tool-context-conversation origin)
                 :context-rule-generation (tool-context-rule-generation origin)
                 :worker (tool-context-worker origin)
                 :registry (tool-context-registry origin)
                 :agent (tool-context-agent origin)
                 :observer (tool-context-observer origin)
                 :mutation-checker (tool-context-mutation-checker origin)
                 :command-authorization-function (tool-context-command-authorization-function origin)
                 :tool-authorization-function (tool-context-tool-authorization-function origin)
                 :call-id call-id))

(-> worker-host--result (tool-result) list)
(defun worker-host--result (result)
  "Return ordinary bounded RESULT as readable data, retaining native details."
  (list :success-p (tool-result-success-p result)
        :category (tool-result-category result)
        :content (tool-result-content result)
        :code (tool-result-error-code result)
        :details (tool-result-details result)
        :content-blocks
        (mapcar #'conversation--tool-content-block-record
                (or (tool-result-content-blocks result)
                    (tool-result-image-attachments result)))))

(-> worker-host--reentrant-p (string hash-table &key (:repl string)) boolean)
(defun worker-host--reentrant-p (name arguments &key repl)
  "Detect manager operations which cannot run during REPL's callback."
  (and (uiop:string-prefix-p "lisp." name)
       (or (string= name "lisp.cwd")
           (and (not (member name '("lisp.images" "lisp.repls") :test #'string=))
                (equal repl (or (gethash "repl" arguments) "default"))))
       t))

(-> worker-host--dispatch (t &key (:context list) (:identity list)
                                (:cancelled-p function)) list)
(defun worker-host--dispatch (payload &key context identity cancelled-p)
  "Invoke the normal authorized registry with correlated worker audit records.
CONTEXT is host-owned data; worker payloads cannot replace its authority."
  (destructuring-bind (origin names bindings repl mission) context
    (progv (mapcar #'first bindings) (mapcar #'rest bindings)
      (let ((*worker-host-tool-policy* (list :restricted-p t :allowlist names)))
      (when (funcall cancelled-p)
        (worker-host--reject ':cancelled "The originating worker request was cancelled."))
      (when mission
        (mission--admit mission :inference-p nil))
      (unless (and (listp payload) (= (length payload) 4)
                   (= (count :tool payload :test #'eq) 1)
                   (= (count :arguments payload :test #'eq) 1)
                   (member (first payload) '(:tool :arguments))
                   (member (third payload) '(:tool :arguments))
                   (non-empty-string-p (getf payload :tool))
                   (json-object-source-p (getf payload :arguments)))
        (worker-host--reject ':invalid-arguments
                             "Use worker-tool-call with an exact tool name and JSON object source."))
      (let* ((name (getf payload :tool))
             (call-id (format nil "worker:~A:~A:~A:~A"
                              (or (tool-context-call-id origin) "local")
                              (getf identity :session) (getf identity :request-id)
                              (getf identity :call-id)))
             (conversation (tool-context-conversation origin))
             (call-context (worker-host--context origin call-id))
             (observer (tool-context-observer origin))
             (result nil)
             (completed-p nil))
        (conversation-append-record
         conversation (list :worker-tool-call :call-id call-id
                            :parent-call-id (tool-context-call-id origin)
                            :worker repl :identity identity :tool name
                            :arguments (getf payload :arguments)))
        (unwind-protect
             (progn
               (setf result
                     (cond
                       ((not (member name names :test #'string=))
                        (tool-failure "Tool is not in this worker request's host-tools allowlist."
                                      :code ':capability-denied))
                      ((and (uiop:string-prefix-p "lisp." name)
                            (equal (json-get (json-decode (getf payload :arguments)) "target") "self"))
                       (tool-failure "Worker host calls do not carry active-image authority."
                                     :code ':capability-denied))
                      ((worker-host--reentrant-p name
                                                 (json-decode (getf payload :arguments)) :repl repl)
                        (tool-failure "The calling REPL is awaiting this callback. Select a different REPL for nested execution."
                                      :code ':worker-reentrancy))
                       (t
                       (let ((tool (find name (tool-registry-tools (tool-context-registry origin))
                                         :key #'tool-canonical-name :test #'string=)))
                         (if (or (null tool) (string= (tool-namespace tool) "self"))
                             (tool-failure "The requested host tool is no longer available."
                                           :code ':capability-denied)
                             (let ((call (json-object "namespace" (tool-namespace tool)
                                                     "name" (tool-name tool)
                                                     "call_id" call-id
                                                     "arguments" (getf payload :arguments))))
                               (flet ((execute ()
                                        (tool-registry-execute-call
                                         (tool-context-registry origin) call call-context)))
                                 (if observer
                                     (agent-observer-call-with-tool-execution observer call-id #'execute)
                                     (execute)))))))))
               (let ((portable (worker-host--result result)))
                 (conversation-append-record
                  conversation (list :worker-tool-result :call-id call-id
                                     :parent-call-id (tool-context-call-id origin)
                                     :identity identity :result portable))
                 (setf completed-p t)
                 portable))
          (unless completed-p
            (conversation-append-record
             conversation (list :worker-tool-result :call-id call-id
                                :parent-call-id (tool-context-call-id origin)
                                :identity identity :result
                                (list :success-p nil :category ':failure
                                      :code ':unknown-outcome
                                    :content *conversation-interrupted-tool-output*))))))))))

(-> worker-host-request (sbcl-worker keyword list
                        &key (:context tool-context) (:tools list)) list)
(defun worker-host-request (worker operation arguments &key context tools)
  "Run an opted-in ordinary worker operation with exactly CONTEXT's authority."
  (let* ((agent (tool-context-agent context))
         (mission (if *worker-host-admission*
                      (first *worker-host-admission*)
                      (and (typep agent 'agent) (mission--agent-context agent))))
         (*worker-host-admission* (or *worker-host-admission* (list mission)))
         (bindings (worker-host--bindings))
         (repl (lisp-worker-name worker)))
    (handler-case
        (lisp-worker--call
         (lambda ()
           (sbcl-workers:sbcl-worker-host-request
            worker operation arguments
            :dispatcher #'worker-host--dispatch
            :context (list context tools bindings repl mission)
            :worker-context (list :conversation-id
                                  (conversation-identifier (tool-context-conversation context))
                                  :parent-call-id (tool-context-call-id context)
                                  :worker repl :host-tools tools)
            :cancel-p (and mission (lambda ()
                                     (not (eq (getf (mission-context-goal mission) :status) ':active))))
            :request-limit *worker-host-request-limit*
            :result-limit *worker-host-result-limit*)))
      (job-aborted (condition)
        (sbcl-worker-cancel-request worker)
        (error condition)))))

(-> worker-host-eval-request (sbcl-worker keyword list
                             &key (:context tool-context) (:tools list)
                                  (:enabled-p boolean)) list)
(defun worker-host-eval-request (worker operation arguments &key context tools enabled-p)
  "Use the plain request by default, installing only explicit worker facades on opt-in."
  (if enabled-p
      (worker-host-request
       worker operation
       (list :forms
             (append
              '("(defun autolith::worker-tool-call (name arguments) (sbcl-workers:sbcl-worker-host-call (list :tool name :arguments arguments)))"
                "(defun autolith::worker-tool-context () (sbcl-workers:sbcl-worker-host-context))"
                "(export '(autolith::worker-tool-call autolith::worker-tool-context) :autolith)")
              (getf arguments :forms)))
       :context context :tools tools)
      (lisp-worker-request worker operation arguments)))
