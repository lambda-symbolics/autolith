(in-package #:autolith)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A file or subprocess tool that does not operate on the active Lisp image."))

(defclass fs-view-image-tool (workspace-tool)
  ()
  (:documentation "Attach one local image to the model for visual inspection."))

(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one external command in an authorized directory."))

(defmethod tool-child-safe-p ((tool fs-view-image-tool))
  "Permit native workspace image inspection inside child agents."
  t)

(defmethod tool-storm-guard-exempt-p ((tool fs-view-image-tool))
  "Exempt local image inspection from the mutating-call storm guard."
  t)

(defmethod tool-child-safe-p ((tool shell-run-tool))
  "Permit authorized workspace commands inside child agents."
  t)

(defmethod tool-execution-policy ((tool shell-run-tool))
  "Serialize shell commands because they may mutate shared workspace state."
  (declare (ignore tool))
  ':exclusive)


;;;; -- Workspace Defaults --

(defparameter *shell-default-timeout-seconds* 60
  "The default timeout for a shell.run command, in seconds.")

(defparameter *shell-maximum-output-characters* 6000
  "The raw-byte head/tail sampling budget for an inline shell.run preview.")

(defparameter *workspace-tool-readable-roots* nil
  "Pathname roots accessible without extra approval for the current call.

Agent turns bind this to the configured workspace and source roots. Specialized
resources may extend or replace these roots dynamically. WORKSPACE-TOOL-PATH asks
for full-access approval outside them; WORKSPACE-TOOL-CONFINED-PATH rejects paths
outside them.")

(defvar *workspace-file-mutation-lock*
  (make-recursive-lock "Autolith workspace file mutations")
  "Serialize native workspace file writes and revision-gated publication.")


;;;; -- Path Resolution --

(-> workspace-tool--call-resolving (pathname function) t)
(defun workspace-tool--call-resolving (path function)
  "Call FUNCTION, reporting a failure to resolve PATH as a resource tool error."
  (handler-case
      (funcall function)
    (file-operation-failed (condition)
      (error 'tool-error
             :message   (format nil "Could not resolve workspace path ~A: ~A"
                                path condition)
             :code      (tool-failure-code condition)
             :tool-name "resource"))))

(-> workspace-tool--canonical-path (pathname) pathname)
(defun workspace-tool--canonical-path (path)
  "Resolve existing symlinks in PATH and its nearest existing ancestor.

Signal when an existing path cannot be resolved instead of treating permission
or filesystem failures as absence."
  (workspace-tool--call-resolving
   path
   (lambda ()
     (canonical-pathname path))))

(-> workspace-tool--relative-identifier (pathname pathname) string)
(defun workspace-tool--relative-identifier (path root)
  "Return PATH below directory ROOT as a slash-separated identifier, or \".\" for ROOT.

Pathname components retain their literal characters. Directory identifiers end
with a slash; all separators are slashes, regardless of the host's native syntax."
  (let* ((relative    (uiop:enough-pathname path root))
         (directories (rest (pathname-directory relative)))
         (file        (uiop:native-namestring
                       (make-pathname :host nil :device nil :directory nil
                                      :defaults relative))))
    (if (and (null directories) (string= file ""))
        "."
        (format nil "~{~A/~}~A" directories file))))

(-> workspace-tool--read-path-allowed-p (pathname list) boolean)
(defun workspace-tool--read-path-allowed-p (path roots)
  "Return true when PATH resolves beneath one of the readable ROOTS."
  (workspace-tool--call-resolving
   path
   (lambda ()
     (and (some (lambda (root)
                  (pathname-within-p path root))
                roots)
          t))))

(-> workspace-tool-resolve-path (tool-context (option string)) pathname)
(defun workspace-tool-resolve-path (context path)
  "Return PATH resolved against CONTEXT's working directory without confinement."
  (let* ((configuration     (tool-context-configuration context))
         (working-directory (config :working-directory configuration))
         (resolved
           (if (non-empty-string-p path)
               (merge-pathnames (uiop:parse-native-namestring path)
                                working-directory)
               working-directory)))
    (workspace-tool--canonical-path resolved)))

(-> workspace-tool-readable-roots (tool-context) list)
(defun workspace-tool-readable-roots (context)
  "Return the roots accessible without extra approval under CONTEXT."
  (or *workspace-tool-readable-roots*
      (list (config :working-directory
             (tool-context-configuration context))
            (config :source-root
             (tool-context-configuration context)))))

(-> workspace-tool-confined-path
    (tool-context (option string) &key (:tool-name non-empty-string))
    pathname)
(defun workspace-tool-confined-path (context path &key (tool-name "resource"))
  "Return PATH resolved against CONTEXT's working directory, strictly inside its roots.

When *WORKSPACE-TOOL-READABLE-ROOTS* is NIL, confine access to CONTEXT's
workspace and source roots. Otherwise use the dynamically supplied roots. Resolve
existing symlinks and the nearest existing parent before checking the boundary.
Only callers that cannot work elsewhere, such as the command sandbox, or that
already authorized the path, use this instead of WORKSPACE-TOOL-PATH."
  (let* ((roots     (workspace-tool-readable-roots context))
         (canonical (workspace-tool-resolve-path context path)))
    (unless (workspace-tool--read-path-allowed-p canonical roots)
      (error 'tool-authority-denied
             :message
             (format nil "Path ~A is outside the allowed workspace roots."
                     canonical)
             :tool-name tool-name))
    canonical))

(-> workspace-tool-authorize-outside-path (tool-context pathname non-empty-string) boolean)
(defun workspace-tool-authorize-outside-path (context path tool-name)
  "Return true when TOOL-NAME has full-access approval for PATH outside the roots.

Authorize the command \"TOOL-NAME -- PATH\". Full-access sessions need no
prompt; saved permissions apply, and ask mode prompts once. Sandboxed approval
is insufficient because the tool runs outside the sandbox."
  (let ((approved-p
          (handler-case
              (eq (tool-context-authorize-command
                   context
                   (format nil "~A -- ~A"
                           tool-name
                           (uiop:escape-shell-token (uiop:native-namestring path)))
                   (config :working-directory (tool-context-configuration context)))
                  ':full-access)
            (command-authorization-unavailable ()
              nil))))
    approved-p))

(-> workspace-tool-path
    (tool-context (option string) &key (:tool-name non-empty-string))
    pathname)
(defun workspace-tool-path (context path &key (tool-name "resource"))
  "Return PATH resolved against CONTEXT's working directory for TOOL-NAME.

Paths inside the workspace or source roots need no additional approval.
Other paths require full access through WORKSPACE-TOOL-AUTHORIZE-OUTSIDE-PATH."
  (let ((canonical (workspace-tool-resolve-path context path)))
    (unless (or (workspace-tool--read-path-allowed-p
                 canonical (workspace-tool-readable-roots context))
                (workspace-tool-authorize-outside-path context canonical tool-name))
      (error 'tool-authority-denied
             :message
             (format nil "~A requires full-access approval for path ~A outside the workspace and source roots."
                     tool-name canonical)
             :tool-name tool-name))
    canonical))

(-> workspace-tool-integer-argument
    (json-object string &key (:fallback (option integer)))
    (option integer))
(defun workspace-tool-integer-argument (arguments name &key fallback)
  "Return integer argument NAME from ARGUMENTS, or FALLBACK when absent."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       fallback)
      ((integerp value)
       value)
      ((and (numberp value) (= value (round value)))
       (round value))
      (t
       (error 'tool-error
              :message   (format nil "Tool argument ~S must be an integer." name)
              :tool-name name)))))

(-> workspace-tool-shell-timeout (json-object) (integer 1))
(defun workspace-tool-shell-timeout (arguments)
  "Return the positive requested shell timeout with no product maximum."
  (max 1
       (or (workspace-tool-integer-argument arguments "timeout-seconds")
           *shell-default-timeout-seconds*)))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool fs-view-image-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a local image as native provider image content."
  (let* ((path         (workspace-tool-path
                        context
                        (tool-argument arguments "path" :required t)))
         (conversation (tool-context-conversation context))
         (attachment
           (image-input-prepare
            path
            (conversation-image-artifact-root conversation))))
    (tool-success
     (format nil "Viewed ~A (~Dx~D, ~A)."
             (image-attachment-source-name attachment)
             (image-attachment-width attachment)
             (image-attachment-height attachment)
             (image-attachment-mime-type attachment))
     :image-attachments (list attachment))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Authorize one command, then run it once directly or as an inspectable job."
  (let* ((command            (tool-argument arguments "command" :required t))
         (description
           (let ((value (tool-argument arguments "description")))
             (and (non-empty-string-p value) value)))
         (directory-argument (tool-argument arguments "directory"))
         (directory          (workspace-tool-resolve-path context directory-argument))
         (timeout            (workspace-tool-shell-timeout arguments))
         (merge-output-p
           (not (tool-boolean-argument
                 arguments "separate-output" :tool-name "shell.run")))
         (async-p
           (tool-boolean-argument
            arguments "async" :tool-name "shell.run"))
         (completion-policy
           (tool-completion-policy-argument arguments :tool-name "shell.run")))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message   "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (let ((authorization
            (handler-case
                (tool-context-authorize-command context command directory)
              (command-authorization-unavailable (condition)
                (return-from tool-execute
                  (tool-failure (princ-to-string condition) :denied-p t))))))
      (if (eq authorization ':deny)
          (tool-failure "The user denied this command." :denied-p t)
          (let ((configuration (tool-context-configuration context))
                (output-limit  *shell-maximum-output-characters*))
            (when (eq authorization ':sandboxed)
              (workspace-tool-confined-path context directory-argument
                                            :tool-name "shell.run"))
            (tool-execution-invoke
             (tool-context-execution-runtime context)
             (tool-context-agent context)
             :tool-name         "shell.run"
             :description       description
             :summary           (format nil "~A in ~A" command directory)
             :operation-function
             (lambda ()
               (flet ((run (policy environment)
                        (workspace-tool-run-shell-command
                         command directory
                         :policy         policy
                         :timeout        timeout
                         :output-limit   output-limit
                         :environment    environment
                         :context        context
                         :merge-output-p merge-output-p)))
                 (ecase authorization
                   (:sandboxed
                    (platform-call-with-command-sandbox
                      *platform* (config :working-directory configuration) #'run
                      :writable-roots (task-worktree-command-writable-roots context)))
                   (:full-access
                    (run (external-sandbox-policy) nil)))))
             :async-p           async-p
             :completion-policy completion-policy
             :parent-call-id    (tool-context-call-id context)))))))
