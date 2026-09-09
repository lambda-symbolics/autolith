(in-package #:autolith)

;;;; -- Lisp Scratchpad Resources and Execution --

(defclass lisp-scratchpad-tool (lisp-tool)
  ()
  (:documentation
   "A tool operating on the current conversation's disposable Lisp files."))

(defclass lisp-scratchpad-run-tool (lisp-scratchpad-tool)
  ()
  (:documentation "Load one conversation scratchpad file into a named Lisp REPL."))

(defmethod tool-compact-result-visible-p ((tool lisp-scratchpad-run-tool))
  "Keep arbitrary scratchpad execution visible in compact presentation."
  t)

(defclass scratchpad-resolver (resource-resolver)
  ()
  (:documentation
   "Resolve scratchpad: URIs beneath the current conversation's disposable root."))

(defclass scratchpad-resource (workspace-file-resource)
  ((root
    :initarg :root
    :reader scratchpad-resource-root
    :type pathname
    :documentation "The canonical conversation scratchpad root."))
  (:documentation
   "A file, directory, or missing path beneath one conversation scratchpad."))


;;;; -- Session Paths --

(-> lisp-scratchpad--identifier-fragment (string) string)
(defun lisp-scratchpad--identifier-fragment (identifier)
  "Return a stable filesystem-safe fragment for conversation IDENTIFIER."
  (or (conversation-identifier-path-fragment identifier)
      (let ((fragment
              (map 'string
                   (lambda (character)
                     (if (or (alphanumericp character)
                             (member character '(#\- #\_)))
                         character
                         #\_))
                   identifier)))
        (if (non-empty-string-p fragment)
            fragment
            "conversation"))))

(-> lisp-scratchpad-root (tool-context) pathname)
(defun lisp-scratchpad-root (context)
  "Return CONTEXT's conversation-scoped disposable scratchpad folder."
  (let* ((configuration (tool-context-configuration context))
         (conversation  (tool-context-conversation context))
         (fragment
           (lisp-scratchpad--identifier-fragment
            (conversation-identifier conversation))))
    (merge-pathnames
     (format nil "scratchpads/~A/" fragment)
     (configuration-cache-root configuration))))

(-> lisp-scratchpad-path (tool-context string) pathname)
(defun lisp-scratchpad-path (context relative-path)
  "Resolve RELATIVE-PATH beneath CONTEXT's scratchpad or signal TOOL-ERROR."
  (unless (non-empty-string-p relative-path)
    (error 'tool-error
           :message "A scratchpad path must be a non-empty relative pathname."
           :tool-name "scratchpad"))
  (let ((root (workspace-tool--canonical-path
               (lisp-scratchpad-root context))))
    (when (string= relative-path ".")
      (return-from lisp-scratchpad-path root))
    (let* ((relative  (uiop:parse-native-namestring relative-path))
           (directory (pathname-directory relative)))
      (when (or (uiop:absolute-pathname-p relative)
                (wild-pathname-p relative)
                (some (lambda (component)
                        (member component '(:up :back :wild :wild-inferiors)))
                      directory))
        (error 'tool-error
               :message "Scratchpad paths must stay inside the conversation folder."
               :tool-name "scratchpad"))
      (let ((resolved
              (workspace-tool--canonical-path
               (merge-pathnames relative root))))
        (unless (or (uiop:pathname-equal resolved root)
                    (uiop:subpathp resolved root))
          (error 'tool-error
                 :message "Scratchpad paths must stay inside the conversation folder."
                 :tool-name "scratchpad"))
        resolved))))

(-> scratchpad-resource--canonical-uri (pathname pathname) string)
(defun scratchpad-resource--canonical-uri (root path)
  "Return PATH's canonical scratchpad URI relative to ROOT."
  (format nil "scratchpad:~A"
          (workspace-file--encode-identifier
           (workspace-tool--relative-identifier path root))))

(-> scratchpad-resource--readable-roots (scratchpad-resource) list)
(defun scratchpad-resource--readable-roots (resource)
  "Return readable roots including RESOURCE's conversation scratchpad."
  (let ((root (scratchpad-resource-root resource)))
    (cons root
          (remove root
                  *workspace-tool-readable-roots*
                  :test #'uiop:pathname-equal))))


(defmethod workspace-file-resource-access-roots
    ((resource scratchpad-resource) (context tool-context))
  "Return RESOURCE's disposable scratchpad root as directly accessible."
  (declare (ignore context))
  (scratchpad-resource--readable-roots resource))

(-> scratchpad-resource--root-p (scratchpad-resource) boolean)
(defun scratchpad-resource--root-p (resource)
  "Return true when RESOURCE names the conversation scratchpad root itself."
  (uiop:pathname-equal (workspace-file-resource-pathname resource)
                       (scratchpad-resource-root resource)))


;;;; -- URI Resolution and Authority --

(defmethod resource-resolver-child-safe-p
    ((resolver scratchpad-resolver) context)
  "Permit child agents to use their inherited conversation scratchpad."
  (declare (ignore resolver context))
  t)

(defmethod resource-resolver-resolve
    ((resolver scratchpad-resolver) identifier (context tool-context))
  "Resolve IDENTIFIER beneath CONTEXT's conversation scratchpad."
  (declare (ignore resolver))
  (let* ((root (workspace-tool--canonical-path
                (lisp-scratchpad-root context)))
          (path (lisp-scratchpad-path
                 context (workspace-file--decode-identifier
                          "scratchpad" identifier))))
    (make-instance 'scratchpad-resource
                   :uri      (scratchpad-resource--canonical-uri root path)
                   :pathname path
                   :root     root)))

(defmethod resource-capabilities
    ((resource scratchpad-resource) (context tool-context))
  "Return scratchpad operations allowed by CONTEXT for RESOURCE."
  (declare (ignore context))
  (if (eq (workspace-file--path-kind
           (workspace-file-resource-pathname resource))
          ':other)
      '(:read)
      '(:read :edit)))

(defmethod resource-observe :around
    ((resource scratchpad-resource) (context tool-context))
  "Observe RESOURCE with its disposable root inside the read boundary."
  (let ((*workspace-tool-readable-roots*
          (scratchpad-resource--readable-roots resource)))
    (call-next-method)))

(defmethod resource-apply-operations :around
    ((resource scratchpad-resource) (context tool-context)
     &key base-revision operations)
  "Edit RESOURCE with its disposable root inside the read boundary."
  (declare (ignore base-revision operations))
  (let ((*workspace-tool-readable-roots*
          (scratchpad-resource--readable-roots resource)))
    (call-next-method)))


;;;; -- Revision-Gated Deletion --

(-> scratchpad-resource--delete-operation-p (t) boolean)
(defun scratchpad-resource--delete-operation-p (operation)
  "Return true when OPERATION requests scratchpad deletion."
  (and (json-object-p operation)
       (let ((name (gethash "op" operation)))
         (and (stringp name)
              (string= name "scratchpad-delete")))))

(-> scratchpad-resource--base-observation
    (scratchpad-resource tool-context non-empty-string)
    workspace-file-observation)
(defun scratchpad-resource--base-observation (resource context base-revision)
  "Return RESOURCE's retained BASE-REVISION observation under CONTEXT."
  (with-recursive-lock-held
      ((conversation-resource-observation-lock
        (tool-context-conversation context)))
    (resource-observation-state-observation
     (workspace-file--find-observation-state
      (tool-context-conversation context)
      (resource-uri resource)
      base-revision))))

(-> scratchpad-resource--delete
    (scratchpad-resource tool-context non-empty-string list)
    workspace-file-observation)
(defun scratchpad-resource--delete (resource context base-revision operations)
  "Delete RESOURCE after exact BASE-REVISION validation and return its new state."
  (unless (= (length operations) 1)
    (error 'tool-error
           :message "Operation scratchpad-delete must be the only resource edit operation."
           :tool-name "resource.edit"))
  (let ((operation (first operations)))
    (unless (json-object-p operation)
      (error 'tool-error
             :message "Every resource edit operation must be a JSON object."
             :tool-name "resource.edit"))
    (unless (scratchpad-resource--delete-operation-p operation)
      (error 'tool-error
             :message "Expected scratchpad-delete operation."
             :tool-name "resource.edit"))
    (when (workspace-file--operation-extra-keys-p operation '("op"))
      (error 'tool-error
             :message "Operation scratchpad-delete contains unsupported fields."
             :tool-name "resource.edit")))
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let* ((state
                 (workspace-file--find-observation-state
                  conversation (resource-uri resource) base-revision))
               (base-observation
                 (resource-observation-state-observation state))
               (current (workspace-file--observe-path resource context))
               (path (workspace-file-resource-pathname resource)))
          (unless (workspace-file--same-observation-p current base-observation)
            (workspace-file--signal-stale resource base-observation current))
          (when (and (eq (workspace-file-observation-kind base-observation)
                         ':directory)
                     (search "[directory listing truncated]"
                             (resource-observation-content base-observation)))
            (error 'tool-error
                   :message "Refusing scratchpad directory deletion because its bounded observation was truncated. Read or delete smaller child resources first."
                   :tool-name "resource.edit"))
          (case (workspace-file-observation-kind base-observation)
            (:file
             (delete-file path))
            (:directory
             (platform-delete-directory-tree *platform* path
                                             :validate t
                                             :if-does-not-exist ':error))
            (:missing
             (error 'tool-error
                    :message "Cannot delete an observed missing scratchpad resource."
                    :tool-name "resource.edit")))
          (let ((published (workspace-file--observe-path resource context)))
            (unless (eq (workspace-file-observation-kind published) ':missing)
              (error 'tool-error
                     :message "Scratchpad deletion did not leave the resource missing."
                     :tool-name "resource.edit"))
            (values
             published
             (list (list :kind ':scratchpad-delete
                         :start 1
                         :end 1
                         :summary
                         (if (scratchpad-resource--root-p resource)
                             "scratchpad-delete root"
                             "scratchpad-delete"))))))))))

(defmethod resource-apply-operations
    ((resource scratchpad-resource) (context tool-context)
     &key base-revision operations)
  "Apply scratchpad deletion or inherited structured file edits."
  (let ((delete-count
          (count-if #'scratchpad-resource--delete-operation-p operations)))
    (cond
      ((plusp delete-count)
       (unless (and (= delete-count 1) (= (length operations) 1))
         (error 'tool-error
                :message "Operation scratchpad-delete must be the only resource edit operation."
                :tool-name "resource.edit"))
       (scratchpad-resource--delete
        resource context base-revision operations))
      (t
       (let ((base-observation
               (scratchpad-resource--base-observation
                resource context base-revision)))
         (when (eq (workspace-file-observation-kind base-observation) ':directory)
           (error 'tool-error
                  :message "Scratchpad directories accept only scratchpad-delete."
                  :tool-name "resource.edit"))
         (when (and (scratchpad-resource--root-p resource)
                    (eq (workspace-file-observation-kind base-observation) ':missing)
                    (some (lambda (operation)
                            (and (json-object-p operation)
                                 (string= (or (gethash "op" operation) "")
                                          "replace-empty")))
                          operations))
           (error 'tool-error
                  :message "The scratchpad root cannot be created as a file. Edit a child scratchpad URI instead."
                  :tool-name "resource.edit"))
         (call-next-method))))))


;;;; -- Lisp Execution --

(defmethod tool-execute ((tool lisp-scratchpad-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Load one scratchpad file once into the selected named Lisp REPL."
  (declare (ignore tool))
  (let* ((path
           (lisp-scratchpad-path
            context
            (tool-argument arguments "path" :required t)))
         (repl (lisp-tool-repl-name arguments))
         (form
           (format nil
                   "(load ~A :verbose nil :print nil :external-format :utf-8)"
                   (prin1-to-string path))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure (format nil "~A is a directory." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (lisp-tool-invoke-execution
        context arguments
        :tool-name "lisp.scratchpad-run"
        :summary (format nil "Load scratchpad ~A" path)
        :operation-function
        (lambda (worker)
          (let ((result
                  (worker-response-tool-result
                   (lisp-worker-request worker :eval (list :form form)))))
            (if (tool-result-success-p result)
                (tool-success
                 (format nil "Loaded scratchpad file ~A into Lisp REPL ~A.~%~A"
                         path
                         repl
                         (tool-result-content result)))
                result))))))))
