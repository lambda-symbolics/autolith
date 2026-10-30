;;;; -- Typed Application Protocol Validation --

(in-package #:autolith-supervisor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:parent-proto '#:cl-protobufs.autolith.parent.v1
                                     '#:autolith-supervisor))

(define-condition parent-application-operation-error (error)
  ((code :initarg :code :reader parent-application-operation-error-code
         :documentation "A fixed protocol failure code."))
  (:documentation "A typed application operation failed without exposing private state.")
  (:report (lambda (condition stream)
             (format stream "Parent application operation failed: ~A."
                     (parent-application-operation-error-code condition)))))

(declaim (ftype (function (parent-proto:application-action) boolean)
                protocol-application-action-validate))

(defun protocol-application-action-validate (action)
  "Check bounded inert input before the isolated child reads or evaluates source."
  (unless (and (member (parent-proto:application-action.kind action)
                       '(:command :lisp :async-lisp :catalog :completion :message :run-job
                          :acp-prompt :acp-extension :configure :pump))
               (<= (length (cl-protobufs:serialize-to-bytes action)) 4194304)
               (<= (length (parent-proto:application-action.source action)) 4194304)
               (<= (parent-proto:application-action.cursor action)
                   (length (parent-proto:application-action.source action)))
               (<= (length (parent-proto:application-action.image-paths action)) 32)
               (every (lambda (path)
                        (and (plusp (length path)) (<= (length path) 8192)
                             (not (find #\Null path))))
                      (parent-proto:application-action.image-paths action))
               (or (not (member (parent-proto:application-action.kind action) '(:catalog :pump)))
                   (and (zerop (length (parent-proto:application-action.source action)))
                        (null (parent-proto:application-action.image-paths action)))))
    (error 'parent-application-operation-error :code ':invalid-request))
  (when (parent-proto:application-action.has-job action)
    (unless (eq (parent-proto:application-action.kind action) ':run-job)
      (error 'parent-application-operation-error :code ':invalid-request))
    (protocol-map->data (parent-proto:application-action.job action)
                        :max-depth 16 :max-entries 8192))
  t)

(declaim (ftype (function (parent-proto:application-action) hash-table)
                protocol-application-action->data))

(defun protocol-application-action->data (action)
  "Return exact approved action data for the parent-owned input journal."
  (protocol-application-action-validate action)
  (let ((fields (make-hash-table :test #'equal)))
    (setf (gethash "text" fields) (parent-proto:application-action.source action)
          (gethash "application_kind" fields)
          (string-downcase (parent-proto:application-action.kind action))
          (gethash "application_cursor" fields) (parent-proto:application-action.cursor action)
          (gethash "application_images" fields)
          (coerce (parent-proto:application-action.image-paths action) 'simple-vector))
    (when (parent-proto:application-action.has-job action)
      (setf (gethash "application_job" fields)
            (protocol-map->data (parent-proto:application-action.job action))))
    fields))

(declaim (ftype (function (parent-proto:application-result) boolean)
                protocol-application-result-validate))

(defun protocol-application-result-validate (result)
  "Check result limits and all finite command metadata before parent rendering."
  (unless (and (plusp (length (parent-proto:application-result.turn-id result)))
               (member (parent-proto:application-result.status result) '(:ok :aborted :error))
               (<= (length (parent-proto:application-result.commands result)) 2048)
               (<= (length (parent-proto:application-result.completions result)) 4096)
               (<= (length (parent-proto:application-result.completion-entries result)) 8192)
               (<= (length (parent-proto:application-result.values result)) 1024)
               (<= (length (cl-protobufs:serialize-to-bytes result)) 4194304))
    (error 'parent-application-operation-error :code ':resource-limit))
  (dolist (command (parent-proto:application-result.commands result))
    (unless (and (plusp (length (parent-proto:application-command.name command)))
                 (char= #\/ (char (parent-proto:application-command.name command) 0))
                 (<= (length (parent-proto:application-command.name command)) 256)
                 (<= (length (parent-proto:application-command.aliases command)) 64)
                 (<= (length (parent-proto:application-command.options command)) 4096)
                 (member (parent-proto:application-command.busy-behavior command)
                         '(:hold :inspect :execute :apply :cancel))
                 (member (parent-proto:application-command.terminal-behavior command)
                         '(:shared :exclusive :exclusive-without-arguments)))
      (error 'parent-application-operation-error :code ':invalid-request)))
  (when (parent-proto:application-result.has-fields result)
    (protocol-map->data (parent-proto:application-result.fields result)))
  (when (parent-proto:application-result.has-job result)
    (protocol-map->data (parent-proto:application-result.job result)))
  t)

(defparameter *protocol-application-setting-keys*
  '(:model-provider :reasoning-effort :reasoning-summary :openrouter-provider-routing :codex-fast-mode-p
    :prefer-codex-context-window-p :web-search-mode :compaction-threshold-percent
    :reasoning-traces-p :compact-view-p :turn-timestamps-p :cache-miss-notices-p
    :knowledge-mode :diagnosis-p :idle-compaction-p :prompt-cache-retention-seconds
    :simple-technical-english-p :session-title-generation-p :hurry-up-p :relax-p
    :fullscreen-p :onboarding-p :terminal-theme :boot-screen-p :boot-screen-seconds
    :boot-screen-linger-p :boot-screen-tip-seconds :permission-mode :immutable-p :pristine-p
    :site-config-root)
  "Public scalar settings admitted into a child configuration. Never add owner storage or plugin keys.")
