(in-package #:autolith)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A tool touching only workspace files and subprocesses, never the active image."))

(defclass fs-view-image-tool (workspace-tool)
  ()
  (:documentation "Attach one local image to the model for visual inspection."))


(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one authorized external command in the workspace."))

(defmethod tool-child-safe-p ((tool fs-view-image-tool))
  "Permit native workspace image inspection inside child agents."
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
  "The seconds one shell.run command may take by default.")

(defparameter *shell-maximum-output-characters* 65536
  "The maximum combined output characters returned by shell.run.")

(defparameter *workspace-tool-readable-roots* nil
  "Optional pathname roots confining workspace-tool reads for the current call.")

(defvar *workspace-file-mutation-lock*
  (make-recursive-lock "Autolith workspace file mutations")
  "Serialize native workspace file writes and revision-gated publication.")


;;;; -- Path Resolution --

(-> workspace-tool--canonical-path (pathname) pathname)
(defun workspace-tool--canonical-path (path)
  "Resolve existing symlinks in PATH and its nearest existing ancestor.

Signal when an existing path cannot be resolved instead of treating permission
or filesystem failures as absence."
  (labels ((resolve-existing (candidate)
             "Return CANDIDATE's truename and whether it is absent."
             (handler-case
                 (values (truename candidate) nil)
               (file-error (condition)
                 (handler-case
                     (progn
                       (sb-posix:lstat (namestring candidate))
                       (error 'tool-error
                              :message (format nil "Could not resolve workspace path ~A: ~A"
                                               candidate condition)
                              :tool-name "resource"))
                   (sb-posix:syscall-error (inspection-condition)
                     (if (= (sb-posix:syscall-errno inspection-condition)
                            sb-posix:enoent)
                         (values nil t)
                         (error 'tool-error
                                :message
                                (format nil "Could not inspect workspace path ~A: ~A"
                                        candidate inspection-condition)
                                :tool-name "resource")))))))

           (canonical-directory (directory)
             "Return DIRECTORY with every existing ancestor resolved."
             (multiple-value-bind (canonical missing-p)
                 (resolve-existing directory)
               (if (not missing-p)
                   canonical
                   (let* ((components (pathname-directory directory))
                          (leaf (first (last components))))
                     (unless (stringp leaf)
                       (error 'tool-error
                              :message (format nil "Could not resolve workspace path ~A."
                                               path)
                              :tool-name "resource"))
                     (let ((parent
                             (make-pathname
                              :directory (butlast components)
                              :name nil
                              :type nil
                              :version nil
                              :defaults directory)))
                       (merge-pathnames
                        (make-pathname :directory (list ':relative leaf)
                                       :name nil
                                       :type nil)
                        (canonical-directory parent))))))))
    (multiple-value-bind (canonical missing-p)
        (resolve-existing path)
      (if (not missing-p)
          canonical
          (merge-pathnames
           (make-pathname :name (pathname-name path)
                          :type (pathname-type path)
                          :version (pathname-version path))
           (canonical-directory (uiop:pathname-directory-pathname path)))))))

(-> workspace-tool--read-path-allowed-p (pathname list) boolean)
(defun workspace-tool--read-path-allowed-p (path roots)
  "Return true when PATH resolves beneath one of the readable ROOTS."
  (let ((candidate (workspace-tool--canonical-path path)))
    (not
     (null
      (some (lambda (root)
              (uiop:subpathp candidate
                             (workspace-tool--canonical-path root)))
            roots)))))

(-> workspace-tool-path (tool-context (option string)) pathname)
(defun workspace-tool-path (context path)
  "Return PATH resolved against CONTEXT's working directory.

When *WORKSPACE-TOOL-READABLE-ROOTS* is non-NIL, reject paths outside those
roots after resolving existing symlinks and the nearest existing parent."
  (let* ((working-directory (configuration-working-directory
                             (tool-context-configuration context)))
         (resolved (if (non-empty-string-p path)
                       (merge-pathnames (pathname path) working-directory)
                       working-directory))
         (canonical (workspace-tool--canonical-path resolved)))
    (when (and *workspace-tool-readable-roots*
               (not (workspace-tool--read-path-allowed-p
                     canonical *workspace-tool-readable-roots*)))
      (error 'tool-error
             :message
             (format nil "Path ~A is outside the readable workspace and source roots."
                     canonical)
             :tool-name "resource"))
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
              :message (format nil "Tool argument ~S must be an integer." name)
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
  (let* ((path (workspace-tool-path
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


(-> workspace-tool-run-shell-command
    (string pathname t (integer 1) (integer 0))
    tool-result)
(defun workspace-tool-run-shell-command
    (command directory policy timeout output-limit)
  "Run one already authorized shell COMMAND with fully resolved execution policy."
  (let* ((result
           (handler-bind
               ((sb-int:stream-decoding-error
                  (lambda (condition)
                    (let ((restart (find-restart 'use-value condition)))
                      (when restart
                        (invoke-restart restart (code-char #xFFFD)))))))
             (run-sandboxed
              "/bin/sh"
              (list "-c" command)
              :policy policy
              :working-directory directory
              :timeout timeout
              :merge-output-p t
              :output-limit output-limit
              :error-output-limit output-limit)))
          (output (sandbox-result-output result))
          (exit-code (sandbox-result-exit-code result))
          (presented-output
            (if (sandbox-result-output-truncated-p result)
                (format nil
                        "~A~%[combined output truncated after ~D characters]"
                        output output-limit)
                output)))
      (cond
        ((sandbox-result-timed-out-p result)
         (tool-failure
          (format nil "The command was stopped after ~D seconds.~%~A"
                  timeout presented-output)
          :code ':timeout))
        ((zerop exit-code)
         (tool-success
          (format nil "exit ~D~%~A"
                  exit-code presented-output)))
        (t
         (tool-failure
          (format nil "exit ~D~%~A"
                  exit-code presented-output)
          :code ':process-exit
          :details (json-object "process.exit.code" exit-code))))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Authorize one command, then run it once directly or as an inspectable job."
  (let* ((command (tool-argument arguments "command" :required t))
         (description
           (let ((value (tool-argument arguments "description")))
             (and (non-empty-string-p value) value)))
         (directory (workspace-tool-path
                     context
                     (tool-argument arguments "directory")))
         (timeout (workspace-tool-shell-timeout arguments))
         (async-p
           (tool-boolean-argument
            arguments "async" :tool-name "shell.run")))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (let ((authorization
            (handler-case
                (tool-context-authorize-command context command directory)
              (command-authorization-unavailable (condition)
                (return-from tool-execute
                  (tool-failure (princ-to-string condition)))))))
      (if (eq authorization ':deny)
          (tool-failure "The user denied this command.")
          (let* ((configuration (tool-context-configuration context))
                 (policy
                   (ecase authorization
                     (:sandboxed
                      (workspace-write-sandbox-policy
                       :workspace-roots
                       (list
                        (configuration-working-directory configuration))))
                     (:full-access
                      (external-sandbox-policy))))
                 (output-limit *shell-maximum-output-characters*))
            (tool-execution-invoke
             (tool-context-execution-runtime context)
             (tool-context-agent context)
             :tool-name "shell.run"
             :description description
             :summary (format nil "~A in ~A" command directory)
             :operation-function
             (lambda ()
               (workspace-tool-run-shell-command
                command directory policy timeout output-limit))
             :async-p async-p
             :parent-call-id (tool-context-call-id context)))))))
