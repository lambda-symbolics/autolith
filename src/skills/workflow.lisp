(in-package #:autolith)

;;;; -- Executable Skill Workflows --

(-> skill-workflow--restricted-registry (tool-registry) tool-registry)
(defun skill-workflow--restricted-registry (registry)
  "Return REGISTRY's tool view without active-image self tools."
  (let ((restricted (make-instance 'tool-registry)))
    (dolist (tool (tool-registry-tools registry))
      (unless (typep tool 'self-tool)
        (tool-registry-register restricted tool)))
    restricted))

(-> skill-workflow--context
    (tool-context skill-metadata)
    tool-context)
(defun skill-workflow--context (context metadata)
  "Return CONTEXT with METADATA's workflow self-tool policy applied."
  (if (skill-metadata-workflow-self-tools-p metadata)
      context
      (make-instance
       'tool-context
       :configuration (tool-context-configuration context)
       :worker (tool-context-worker context)
       :conversation (tool-context-conversation context)
       :mutation-checker (tool-context-mutation-checker context)
       :registry
       (skill-workflow--restricted-registry
        (tool-context-registry context))
       :command-authorization-function
       (tool-context-command-authorization-function context)
       :tool-authorization-function
       (tool-context-tool-authorization-function context)
       :agent (tool-context-agent context)
       :observer (tool-context-observer context)
       :call-id (tool-context-call-id context))))

(-> skill-workflow--pathname (skill-metadata non-empty-string) pathname)
(defun skill-workflow--pathname (metadata workflow)
  "Return WORKFLOW's pathname beside METADATA's SKILL.sexp."
  (merge-pathnames
   workflow
   (uiop:pathname-directory-pathname
    (skill-metadata-pathname metadata))))

(-> skill-workflow--read (skill-metadata) t)
(defun skill-workflow--read (metadata)
  "Read exactly one bounded Lisp form from METADATA's confined workflow file."
  (let* ((workflow
           (or (skill-metadata-current-workflow metadata)
               (error 'tool-error
                      :message
                      (format nil "Skill ~A no longer declares a workflow."
                              (skill-metadata-name metadata))
                      :tool-name "skill.run")))
         (pathname (skill-workflow--pathname metadata workflow))
         (directory
           (uiop:pathname-directory-pathname
            (skill-metadata-pathname metadata))))
    (multiple-value-bind (source canonical-pathname device inode)
        (skill--read-file-bounded
         pathname
         *skill-workflow-character-limit*
         :root directory
         :label *skill-workflow-filename*)
      (declare (ignore canonical-pathname device inode))
      (let ((*package* (find-package '#:autolith)))
        (self-read-form source :read-eval nil)))))

(-> skill-workflow--render-result (string list) string)
(defun skill-workflow--render-result (output values)
  "Return human-readable OUTPUT and rendered workflow VALUES."
  (with-output-to-string (stream)
    (when (non-empty-string-p output)
      (write-string output stream)
      (unless (char= (char output (1- (length output))) #\Newline)
        (terpri stream)))
    (if values
        (format stream "~{~A~^~%~}" values)
        (write-string "Workflow completed with no values." stream))))

(defmethod skill-workflow-execute
    ((context tool-context) (metadata skill-metadata))
  "Execute METADATA's workflow with CONTEXT's registered tool bindings."
  (let ((name (skill-metadata-name metadata)))
    (when (member name *skill-workflow-stack* :test #'string=)
      (error 'tool-error
             :message
             (format nil "Skill workflow recursion repeated ~A." name)
             :tool-name "skill.run"))
    (when (>= (length *skill-workflow-stack*) *skill-workflow-depth-limit*)
      (error 'tool-error
             :message
             (format nil
                     "Skill workflow nesting exceeds the depth limit of ~D."
                     *skill-workflow-depth-limit*)
             :tool-name "skill.run"))
    (handler-case
        (let* ((workflow-context (skill-workflow--context context metadata))
               (*skill-workflow-stack*
                 (cons name *skill-workflow-stack*))
               (*application-operation-tool-context* workflow-context)
               (*package* (find-package '#:autolith))
               (output-stream (make-string-output-stream)))
          (application-operation-install-context-bindings workflow-context)
          (let ((*standard-output* output-stream)
                (*error-output* output-stream)
                (*trace-output* output-stream))
            (let ((values
                    (mapcar #'sbcl-worker-render-value
                            (multiple-value-list
                             (eval (skill-workflow--read metadata))))))
              (tool-success
               (skill-workflow--render-result
                (get-output-stream-string output-stream)
                values)))))
      (tool-error (condition)
        (tool-failure (autolith-error-message condition)))
      (serious-condition (condition)
        (tool-failure
         (format nil "Skill workflow ~A failed: ~A" name condition))))))
