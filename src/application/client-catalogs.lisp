(in-package #:autolith)

;;;; -- Native Operation Catalog and Invocation --

(-> application-client-catalog--operation-schema (application-operation) t)
(defun application-client-catalog--operation-schema (operation)
  "Return OPERATION's native argument schema."
  (if (typep operation 'application-tool-operation)
      (or (tool-parameters (application-operation-backend operation))
          (list :type :object))
      (list :type  :list
            :items (list :type :any))))

(-> application-client-catalog--operation-row (application-operation boolean boolean) list)
(defun application-client-catalog--operation-row (operation idle-p own-turn-p)
  "Project OPERATION's native metadata and admission state."
  (let* ((command  (and (typep operation 'application-command-operation)
                        (application-operation-backend operation)))
         (active-p (and own-turn-p command
                        (member (application-command-busy-behavior command)
                                '(:inspect :execute :apply)))))
    (list :name              (application-operation-name operation)
          :kind              (application-operation-kind operation)
          :description       (application-operation-description operation)
          :argument-schema   (application-client-catalog--operation-schema operation)
          :admitted-p        (and (or idle-p active-p)
                                  (application-client-operation-admitted-p operation))
          :terminal-owning-p (application-client-operation-terminal-p operation)
          :terminal-without-arguments-p
          (and command (eq (application-command-terminal-behavior command)
                           :exclusive-without-arguments))
          :argument-hint     (and command (application-command-argument command))
          :help              (and command (application-command-tip command))
          :choices           (and command (copy-list (application-command-static-options command)))
          :aliases           (and command (copy-list (application-command-aliases command))))))

(-> application-client-operations (application) list)
(defun application-client-operations (application)
  "Return the registered operation catalog as native values."
  (let* ((controller (application-input-controller application))
         (busy-p     (and controller (application-input-controller-busy-p controller))))
    (list :items (mapcar (lambda (operation)
                           (application-client-catalog--operation-row
                            operation (not busy-p) busy-p))
                         (application-operation-list application)))))

(-> application-client-invoke (application t list) list)
(defun application-client-invoke (application operation-name arguments)
  "Invoke a registered operation with native ARGUMENTS."
  (let* ((name      (etypecase operation-name
                      (string
                       operation-name)
                      (symbol
                       (string-downcase (symbol-name operation-name)))))
         (operation (application-client-operation-find application name))
         (decoded   (if (typep operation 'application-command-operation)
                        (application-operation--command-invocation
                         (application-operation-backend operation) arguments)
                        arguments)))
      (multiple-value-bind (result delivery)
          (application-client-operation-invoke application operation decoded)
        (list :result result :delivery delivery))))
