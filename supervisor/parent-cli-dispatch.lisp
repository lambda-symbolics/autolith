;;;; -- Parent Command Dispatch --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-cli-help (parent-cli-request &optional stream) null)
(defun parent-cli-help (request &optional (stream *standard-output*))
  "Print public command help without opening private configuration."
  (let ((command (parent-cli-request-command request)))
    (format stream "Usage: autolith ~A~%"
            (case command
              (:resume
               "resume [ID] [OPTIONS]")
              (:replay
               "replay ID [turn N | date YYYY-MM-DD | time TIME | sequence N]")
              (:fork
               "fork SOURCE [turn N | date YYYY-MM-DD | time TIME | sequence N] [--id ID]")
              (:auth
               "auth [PROVIDER] [METHOD] [OPTIONS]")
              (:models
               "models [OPTIONS]")
              (:run-job
               "run-job --input FILE --output FILE [--events sexp] [OPTIONS]")
              (:acp
               "acp [OPTIONS]")
              (:data
               "data export|import FILE [--workspace DIRECTORY] [--all]")
              (:localgroup
               "localgroup [status|tell|attach|detach|pause|kill] [ARGUMENTS]")
              (:update
               "update [--help]")
              (:uninstall
               "uninstall [--yes] [--help]")
              (otherwise
               "[COMMAND] [OPTIONS]")))
    (when (eq command ':session)
      (write-line "Commands: resume replay fork auth models acp data run-job localgroup update uninstall" stream))
    (write-line "Session options:" stream)
    (write-line "  --fullscreen       use the alternate-screen terminal" stream)
    (write-line "  --almighty         use fullscreen with the Almighty theme" stream)
    (write-line "  --immutable        disable source and settings mutation" stream)
    (write-line "  --pristine         omit private mutations and agent init" stream)
    (write-line "  --site-config-root DIRECTORY" stream)
    (write-line "  --permissions ask|auto|sandbox|full" stream)
    (write-line "  -i, --image FILE   preload an image; repeatable and comma-separable" stream)
    (write-line "  -h, --help        show help" stream)
    (write-line "  --version         show version" stream))
  nil)

(serapeum/types:-> parent-cli-models (parent-configuration &optional stream) null)
(defun parent-cli-models (configuration &optional (stream *standard-output*))
  "List the trusted registry's cached model identifiers without starting a child."
  (multiple-value-bind (profiles accounts registry paths)
      (parent-provider-build-bindings configuration (parent-configuration-get configuration ':settings)
                                      :registry (parent-configuration-provider-registry configuration))
    (declare (ignore profiles accounts paths))
    (dolist (identifier (sort (copy-list
                               (cl-llm-provider-api:provider-registry-model-identifiers registry))
                             #'string<))
      (write-line identifier stream))
    (finish-output stream))
  nil)

(serapeum/types:-> parent-cli-apply-request (parent-configuration parent-cli-request) parent-configuration)
(defun parent-cli-apply-request (configuration request)
  "Apply explicit options to a data-only owner configuration."
  (let* ((values   (copy-tree (parent-configuration-values configuration)))
         (settings (parent-cli-settings request :preferences (getf values :settings))))
    (setf (getf values :settings) settings
          (getf values :access-p)
          (not (null (member (getf settings :permission-mode) '("full" :full-access) :test #'equal))))
    (let ((replacement (make-parent-configuration values)))
      (setf (parent-configuration-source-path replacement)
            (parent-configuration-source-path configuration)
            (parent-configuration-provider-registry replacement)
            (parent-configuration-provider-registry configuration)
            (parent-configuration-provider-paths replacement)
            (copy-list (parent-configuration-provider-paths configuration)))
      replacement)))

(serapeum/types:-> parent-cli-dispatch (parent-configuration parent-cli-request) (integer 0 255))
(defun parent-cli-dispatch (configuration request)
  "Run a parsed public command in the parent, never through a legacy session."
  (case (parent-cli-request-command request)
    ((:session :auth)
     (parent-cli-exit-code (parent-application-start configuration :request request)))
    (:models
     (parent-cli-models configuration)
     0)
    ((:resume :replay :fork :acp :localgroup :run-job :data)
     (parent-cli-exit-code (parent-session-cli-dispatch configuration request)))
    ((:update :uninstall)
     (write-line "Run this operation through the installed autolith launcher." *error-output*)
     64)
    (otherwise
     (parent-cli--reject ':unknown-command))))

(serapeum/types:-> parent-cli-main (list) (integer 0 255))
(defun parent-cli-main (arguments)
  "Run public CLI arguments with stable status codes and private error text."
  (handler-case
      (let ((request (parent-cli-parse arguments)))
        (cond
          ((parent-cli-option request ':help)
           (parent-cli-help request)
           0)
          ((parent-cli-option request ':version)
           (write-line (string-trim '(#\Newline #\Return)
                                    (uiop:read-file-string
                                     (merge-pathnames "VERSION"
                                                      (asdf:system-source-directory "autolith-supervisor")))))
           0)
          (t
           (let ((path (uiop:getenv "AUTOLITH_PARENT_CONFIGURATION")))
             (parent-cli-dispatch
              (if (and path (plusp (length path)))
                  (parent-cli-apply-request (read-parent-configuration (pathname path)) request)
                  (parent-cli-configuration request))
              request)))))
    (parent-cli-error (condition)
      (format *error-output* "~A~%" condition)
      64)
    (parent-configuration-error (condition)
      (format *error-output* "~A~%" condition)
      64)
    (error ()
      (write-line "Parent application stopped. Private diagnostics were not printed." *error-output*)
      1)))
