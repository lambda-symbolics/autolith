(in-package #:autolith)

;;;; -- Native LSP Configuration --

(define-condition lsp-configuration-error (configuration-error)
  ((pathname :initarg :pathname :initform nil :reader lsp-configuration-error-pathname
             :documentation "The user configuration file involved.")
   (server-name :initarg :server-name :initform nil :reader lsp-configuration-error-server-name
                :documentation "The server definition involved, when known.")
   (field :initarg :field :initform nil :reader lsp-configuration-error-field
          :documentation "The invalid configuration field.")
   (cause :initarg :cause :initform nil :reader lsp-configuration-error-cause
          :documentation "The underlying parser or filesystem condition."))
  (:documentation "A malformed native LSP configuration."))

(-> lsp-configuration-path (configuration) pathname)
(defun lsp-configuration-path (configuration)
  "Return CONFIGURATION's user-controlled native LSP configuration pathname."
  (merge-pathnames "lsp.sexp" (config :config-root configuration)))

(-> lsp-load-configurations (configuration) list)
(defun lsp-load-configurations (configuration)
  "Read CONFIGURATION's user-owned lsp.sexp, or return NIL when absent."
  (let ((pathname (lsp-configuration-path configuration)))
    (when (probe-file pathname)
      (handler-case
          (lsp-read-configurations pathname)
        (cl-lsp:lsp-configuration-error (condition)
          (error 'lsp-configuration-error
                 :message     (cl-lsp:lsp-error-message condition)
                 :pathname    (cl-lsp:lsp-configuration-error-pathname condition)
                 :server-name (cl-lsp:lsp-configuration-error-server-name condition)
                 :field       (cl-lsp:lsp-configuration-error-field condition)
                 :cause       condition))))))

(-> lsp-configuration-enabled-p (configuration) boolean)
(defun lsp-configuration-enabled-p (configuration)
  "Return true when CONFIGURATION enables at least one language server.

Also return true if lsp.sexp is present but malformed, so the configuration
error is reported through the LSP tools rather than blocking registry creation."
  (unless (probe-file (lsp-configuration-path configuration))
    (return-from lsp-configuration-enabled-p nil))
  (handler-case
      (and (some (lambda (server)
                   (not (lsp-server-configuration-disabled-p server)))
                 (lsp-load-configurations configuration))
           t)
    (lsp-configuration-error () t)))

(-> lsp-manager-configure (lsp-manager configuration) list)
(defun lsp-manager-configure (manager configuration)
  "Load and cache CONFIGURATION's user-owned server definitions in MANAGER
until refresh or runtime retirement."
  (unless (lsp-manager-loaded-p manager)
    (setf (lsp-manager-configurations manager) (lsp-load-configurations configuration)
          (lsp-manager-loaded-p manager) t))
  (lsp-manager-configurations manager))
