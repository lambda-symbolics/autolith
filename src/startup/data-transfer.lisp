(in-package #:autolith)

;;;; -- Portable Data CLI --

(-> main--data-transfer-command (keyword) clingon:command)
(defun main--data-transfer-command (operation)
  "Describe one noninteractive data operation backed by the session API."
  (let ((export-p (eq operation ':export)))
    (make-command
     :name (ecase operation (:export "export") (:import "import"))
     :description (if export-p
                      "export all portable user data or one workspace"
                      "import portable user data, optionally relocating a workspace")
     :usage (if export-p "FILE [--all | --workspace DIRECTORY]"
                         "FILE [--workspace DIRECTORY]")
     :options
     (append
      (list (make-option ':string
                         :long-name "workspace"
                         :key ':workspace
                         :parameter "DIRECTORY"
                         :description (if export-p
                                          "export only this workspace"
                                          "relocate a single-workspace archive to this directory")))
      (when export-p
        (list (make-option ':flag
                           :long-name "all"
                           :key ':all
                           :description "export all workspaces and global data (the default)"))))
     :handler
     (lambda (command)
       (let ((pathname (main--single-selection command "archive pathname"))
             (workspace (getopt* command ':workspace)))
         (unless pathname
           (error 'configuration-error :message "Data transfer requires an archive pathname."))
         (when (and export-p workspace (getopt* command ':all))
           (error 'configuration-error :message "Choose either --all or --workspace, not both."))
         (let ((configuration (configuration-create :defer-provider-validation-p t)))
           (handler-case
               (let ((report (funcall (ecase operation
                                        (:export #'data-export)
                                        (:import #'data-import))
                                      (data-transfer--command-path pathname configuration)
                                      :workspace (and workspace
                                                      (data-transfer--command-path workspace configuration))
                                      :configuration configuration)))
                 (format t "~A~%" (data-transfer-render-report
                                  (if export-p "Exported" "Imported") report)))
             ((or data-transfer-error file-error platform-error) (condition)
               (format *error-output* "~A~%" condition)
               (uiop:quit 1)))))))))

(-> main--data-command () clingon:command)
(defun main--data-command ()
  "Return the portable-data command group without starting an interactive session."
  (make-command
   :name "data"
   :description "export and import portable Autolith user data"
   :usage "export|import FILE [OPTIONS]"
   :sub-commands (list (main--data-transfer-command ':export)
                       (main--data-transfer-command ':import))
   :handler (lambda (command)
              (declare (ignore command))
              (error 'configuration-error
                     :message "Usage: autolith data export|import FILE [OPTIONS]"))))
