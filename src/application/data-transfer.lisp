(in-package #:autolith)

;;;; -- Data Transfer Commands --

(-> data-transfer--command-path (t configuration) pathname)
(defun data-transfer--command-path (value configuration)
  "Resolve a user-supplied file or directory against the session workspace."
  (unless (or (pathnamep value) (non-empty-string-p value))
    (error 'configuration-error :message "Data transfer requires a pathname."))
  (merge-pathnames (if (pathnamep value)
                      value
                      (uiop:parse-native-namestring value))
                   (config :working-directory configuration)))

(-> data-transfer--command-workspace (configuration &key (:workspace t) (:all t))
    (option pathname))
(defun data-transfer--command-workspace (configuration &key workspace all)
  "Validate native scope options and resolve WORKSPACE against CONFIGURATION."
  (unless (typep all 'boolean)
    (error 'configuration-error :message "The :all option requires a Boolean value."))
  (when (and all workspace)
    (error 'configuration-error :message "Choose either :all t or :workspace PATHNAME."))
  (when workspace
    (data-transfer--command-path workspace configuration)))

(-> data-transfer-render-report (string list) string)
(defun data-transfer-render-report (action report)
  "Render a compact transfer receipt for terminal and command-line callers."
  (format nil "~A ~A~%~D conversations, ~D memories, ~D agendas, ~D plans, ~D papercuts, ~D files.~%~A"
          action (uiop:native-namestring (pathname (getf report :pathname)))
          (getf report :conversations) (getf report :memories)
          (getf report :agendas) (getf report :plans)
          (getf report :papercuts) (getf report :files)
          (if (getf report :workspace)
              (format nil "Workspace: ~A"
                      (uiop:native-namestring (pathname (getf report :workspace))))
              (format nil "All data across ~D workspaces."
                      (length (getf report :workspaces))))))

(-> data-transfer-report-entry (string list) list)
(defun data-transfer-report-entry (action report)
  "Return a terminal receipt with styled paths, aligned counts, and scope."
  (append
   (list (terminal-span ':success (format nil "✓ ~(~A~)~%" action)))
   (application--entry-fields
    (append
     (list (list "archive" ':code
                 (uiop:native-namestring (pathname (getf report :pathname)))))
     (loop for key in '(:conversations :memories :agendas :plans :papercuts :files)
           collect (list (string-downcase (symbol-name key)) ':plain (getf report key)))
     (list (if (getf report :workspace)
               (list "workspace" ':code
                     (uiop:native-namestring (pathname (getf report :workspace))))
               (list "scope" ':dim
                     (format nil "all data across ~D workspaces"
                             (length (getf report :workspaces))))))))))

(define-application-command application--builtin-data-export-command
    (:name              "data.export"
     :argument          "FILE [--all|--workspace DIRECTORY]"
     :description       "export all portable user data or one workspace"
     :tip               "Use `(data.export pathname :workspace directory)` to export one workspace."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :callable          t)
    (application pathname &key workspace all)
  (let* ((configuration (application-configuration application))
         (workspace     (data-transfer--command-workspace configuration :workspace workspace :all all))
         (report        (data-export (data-transfer--command-path pathname configuration)
                                     :workspace workspace :configuration configuration)))
    (application-present application (data-transfer-report-entry "Exported" report)))
  ':continue)

(define-application-command application--builtin-data-import-command
    (:name              "data.import"
     :argument          "FILE [--workspace DIRECTORY]"
     :description       "import portable user data, optionally relocating a workspace"
     :tip               "Use `(data.import pathname :workspace directory)` to relocate an imported workspace."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :callable          t)
    (application pathname &key workspace)
  (let* ((configuration (application-configuration application))
         (workspace     (data-transfer--command-workspace configuration :workspace workspace))
         (report        (data-import (data-transfer--command-path pathname configuration)
                                     :workspace workspace :configuration configuration)))
    (application-present application (data-transfer-report-entry "Imported" report)))
  ':continue)
