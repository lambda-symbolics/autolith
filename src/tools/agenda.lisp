(in-package #:autolith)

;;;; -- Agenda Tool Classes --

(defclass agenda-tool (tool)
  ()
  (:documentation "A tool for managing persistent workspace agendas."))

(defclass agenda-transport-tool (agenda-tool)
  ()
  (:documentation "A tool for inspecting, copying, or moving workspace agendas."))

(defmethod tool-execute ((tool agenda-transport-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Enumerate, inspect, copy, or move persistent workspace agendas."
  (declare (ignore tool))
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((configuration (tool-context-configuration context))
           (state         (agenda-load configuration))
           (operation     (agenda-resource--string-argument
                           arguments "operation" "agenda.transport" :required t))
           (source        (agenda-resource--string-argument
                           arguments "source-directory" "agenda.transport"))
           (target        (or (agenda-resource--string-argument
                               arguments "target-directory" "agenda.transport")
                              (namestring
                               (config :working-directory configuration)))))
      (cond
        ((string-equal operation "workspaces")
         (tool-success (agenda-resource--render-workspaces state)))
        ((string-equal operation "view")
         (unless source
           (error 'tool-error
                  :message   "agenda.transport view requires source-directory."
                  :tool-name "agenda.transport"))
         (let* ((directory (agenda-directory-name configuration source))
                (record    (agenda-find state directory)))
           (if record
               (tool-success (agenda-resource--render-record record))
              (tool-failure (format nil "No agenda found for ~A." directory)))))
        ((or (string-equal operation "copy")
             (string-equal operation "move"))
         (unless source
           (error 'tool-error
                  :message   (format nil
                                     "agenda.transport ~A requires source-directory."
                                     operation)
                  :tool-name "agenda.transport"))
         (let ((record
                 (agenda-transport
                  :configuration    configuration
                  :state            state
                  :source-directory source
                  :target-directory target
                  :move-p           (string-equal operation "move"))))
           (tool-success
            (format nil "~A agenda into ~A with ~D item~:P."
                    (if (string-equal operation "move") "Moved" "Copied")
                    (workspace-agenda-directory record)
                    (length (workspace-agenda-items record))))))
        (t
         (error 'tool-error
                :message   "agenda.transport operation must be workspaces, view, copy, or move."
                :tool-name "agenda.transport"))))))
