(in-package #:autolith)

;;;; -- SBCL Worker Adapters --

(deftype lisp-worker ()
  "A persistent isolated process managed by sbcl-workers."
  'sbcl-worker)

(deftype lisp-worker-pool ()
  "A named collection of isolated processes managed by sbcl-workers."
  'sbcl-worker-pool)

(-> lisp-repl--validate-name (string) string)
(defun lisp-repl--validate-name (name)
  "Return valid REPL NAME or signal WORKER-ERROR."
  (unless (sbcl-worker-name-p name)
    (error 'worker-error
           :message
           (format nil
                   "Invalid Lisp REPL name ~S. Use 1 to 80 letters, digits, hyphens, or underscores."
                   name)
           :tool-name "lisp.repls"))
  name)

(-> lisp-worker--tool-name ((or string keyword null)) string)
(defun lisp-worker--tool-name (operation)
  "Return Autolith's dotted tool name for library OPERATION."
  (case operation
    (:change-working-directory "lisp.cwd")
    (:workers "lisp.repls")
    (:save-image "lisp.save-image")
    ((nil)
     "lisp.worker")
    (otherwise
     (format nil "lisp.~(~A~)" operation))))

(-> lisp-worker--call (function) t)
(defun lisp-worker--call (function)
  "Call FUNCTION with isolated worker stderr and translate library conditions."
  (let ((*error-output* (make-broadcast-stream)))
    (handler-case
        (funcall function)
      (sbcl-worker-image-error (condition)
        (error 'lisp-image-error
               :message (sbcl-worker-error-message condition)
               :tool-name
               (lisp-worker--tool-name (sbcl-worker-error-operation condition))
               :pathname (sbcl-worker-error-pathname condition)
               :stage (or (sbcl-worker-error-stage condition) ':manifest)))
      (sbcl-worker-error (condition)
        (error 'worker-error
               :message (sbcl-worker-error-message condition)
               :tool-name
               (lisp-worker--tool-name
                (sbcl-worker-error-operation condition)))))))

(-> lisp-worker-sbcl-command () string)
(defun lisp-worker-sbcl-command ()
  "Return the configured SBCL executable used by disposable workers."
  (let ((configured-command (uiop:getenv "AUTOLITH_SBCL")))
    (if (non-empty-string-p configured-command)
        configured-command
        "sbcl")))

(-> lisp-worker-create
    (configuration &key (:name string) (:image-identifier string))
    lisp-worker)
(defun lisp-worker-create
    (configuration &key (name "default")
                        (image-identifier (pristine-lisp-image-identifier)))
  "Create a stopped named worker based on IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-create
      (lisp-worker--environment configuration)
      :name name
      :image-identifier image-identifier))))

(-> lisp-worker-name (lisp-worker) string)
(defun lisp-worker-name (worker)
  "Return WORKER's stable REPL name."
  (sbcl-worker-name worker))

(-> lisp-worker-image-identifier (lisp-worker) string)
(defun lisp-worker-image-identifier (worker)
  "Return the pristine or saved image used by WORKER."
  (sbcl-worker-used-image-identifier worker))

(-> lisp-worker-running-p (lisp-worker) boolean)
(defun lisp-worker-running-p (worker)
  "Return true when WORKER has a live subprocess."
  (sbcl-worker-running-p worker))

(-> lisp-worker-start (lisp-worker) lisp-worker)
(defun lisp-worker-start (worker)
  "Start WORKER when necessary and verify its protocol handshake."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-start worker))))

(-> lisp-worker-stop (lisp-worker) null)
(defun lisp-worker-stop (worker)
  "Terminate WORKER and discard its process streams and heap state."
  (sbcl-worker-stop worker))

(-> lisp-worker-reset (lisp-worker) lisp-worker)
(defun lisp-worker-reset (worker)
  "Restart WORKER from the same pristine or saved image."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-reset worker))))

(-> lisp-worker-change-working-directory
    (lisp-worker configuration)
    lisp-worker)
(defun lisp-worker-change-working-directory (worker configuration)
  "Move WORKER to CONFIGURATION's workspace without discarding its heap."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-change-working-directory
      worker
      (lisp-worker--environment configuration)))))

(-> lisp-worker-request (lisp-worker keyword list) list)
(defun lisp-worker-request (worker operation arguments)
  "Send OPERATION to WORKER, cancelling only an interrupted protocol request."
  (handler-case
      (lisp-worker--call
       (lambda ()
         (sbcl-worker-request worker operation arguments)))
    (job-aborted (condition)
      (sbcl-worker-cancel-request worker)
      (error condition))))


;;;; -- Named Worker Pool Adapters --

(-> lisp-worker-pool-create (configuration) lisp-worker-pool)
(defun lisp-worker-pool-create (configuration)
  "Create an empty named Lisp worker pool for CONFIGURATION."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-create
      (lisp-worker--environment configuration)))))

(-> lisp-worker-pool-configuration (lisp-worker-pool) configuration)
(defun lisp-worker-pool-configuration (pool)
  "Return the Autolith configuration currently associated with POOL."
  (sbcl-worker-environment-context
   (sbcl-worker-pool-environment pool)))

(-> lisp-worker-pool-start
    (lisp-worker-pool string (option string))
    lisp-worker)
(defun lisp-worker-pool-start (pool name image-identifier)
  "Start or return NAME, enforcing IMAGE-IDENTIFIER when supplied."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-start pool name image-identifier))))

(-> lisp-worker-pool-worker (lisp-worker-pool string) lisp-worker)
(defun lisp-worker-pool-worker (pool name)
  "Return NAME, starting it from pristine SBCL when absent."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-worker pool name))))

(-> lisp-worker-pool-stop
    (lisp-worker-pool string &key (:if-missing keyword))
    null)
(defun lisp-worker-pool-stop (pool name &key (if-missing :error))
  "Stop and forget named REPL NAME."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-stop pool name :if-missing if-missing))))

(-> lisp-worker-pool-reset (lisp-worker-pool string string) lisp-worker)
(defun lisp-worker-pool-reset (pool name image-identifier)
  "Replace named REPL NAME with a fresh process from IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-reset pool name image-identifier))))

(-> lisp-worker-pool-stop-all (lisp-worker-pool) null)
(defun lisp-worker-pool-stop-all (pool)
  "Stop and forget every REPL managed by POOL."
  (sbcl-worker-pool-stop-all pool))

(-> lisp-worker-pool-change-working-directory
    (lisp-worker-pool configuration)
    lisp-worker-pool)
(defun lisp-worker-pool-change-working-directory (pool configuration)
  "Move every REPL in POOL to CONFIGURATION with rollback on failure."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-change-working-directory
      pool
      (lisp-worker--environment configuration)))))

;;; A manager is one worker, a named pool, or NIL; sbcl-workers' manager
;;; protocol addresses all three, and these wrappers translate its failures.

(-> lisp-worker-manager-change-working-directory (t configuration) t)
(defun lisp-worker-manager-change-working-directory (manager configuration)
  "Move MANAGER's current and future REPLs to CONFIGURATION's workspace."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-manager-change-working-directory
      manager (lisp-worker--environment configuration)))))

(-> lisp-worker-manager-stop (t) null)
(defun lisp-worker-manager-stop (manager)
  "Stop every live worker represented by MANAGER."
  (lisp-worker--call (lambda () (sbcl-worker-manager-stop manager)))
  nil)

(-> lisp-worker-manager-worker (t string) lisp-worker)
(defun lisp-worker-manager-worker (manager name)
  "Return named REPL NAME represented by MANAGER."
  (lisp-worker--call (lambda () (sbcl-worker-manager-worker manager name))))

(-> lisp-worker-manager-reset (t string string) lisp-worker)
(defun lisp-worker-manager-reset (manager name image-identifier)
  "Reset named REPL NAME represented by MANAGER from IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda () (sbcl-worker-manager-reset manager name image-identifier))))

(-> lisp-worker-manager-start (t string string) lisp-worker)
(defun lisp-worker-manager-start (manager name image-identifier)
  "Start named REPL NAME through MANAGER from IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda () (sbcl-worker-manager-start manager name image-identifier))))

(-> lisp-worker-manager-stop-worker (t string) null)
(defun lisp-worker-manager-stop-worker (manager name)
  "Stop and forget named REPL NAME represented by MANAGER."
  (lisp-worker--call (lambda () (sbcl-worker-manager-stop-worker manager name)))
  nil)

(-> lisp-worker-pool-render (lisp-worker-pool) string)
(defun lisp-worker-pool-render (pool)
  "Return a concise model-visible list of named REPLs and their images."
  (sbcl-worker-pool-render pool))

(-> lisp-worker-manager-render (t) string)
(defun lisp-worker-manager-render (manager)
  "Return a concise model-visible inventory for MANAGER."
  (lisp-worker--call (lambda () (sbcl-worker-manager-render manager))))

(-> lisp-worker-save-image
    (configuration lisp-worker &key (:identifier string) (:note string))
    lisp-image)
(defun lisp-worker-save-image (configuration worker &key identifier note)
  "Save WORKER as immutable IDENTIFIER with durable NOTE."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-save-image
      (lisp-worker--environment configuration)
      worker
      :identifier identifier
      :note note))))


;;;; -- Tool Result Adaptation --

(-> worker-response-tool-result (list) tool-result)
(defun worker-response-tool-result (response)
  "Convert a portable worker RESPONSE into a bounded tool result."
  (let* ((properties (rest response))
         (status (getf properties :status))
         (output (getf properties :output))
         (result-values (getf properties :values))
         (message (getf properties :message))
         (backtrace (getf properties :backtrace))
         (form-count (getf properties :form-count)))
    (if (eq status :ok)
        (tool-success
         (with-output-to-string (stream)
           (when (non-empty-string-p output)
             (format stream "Output:~%~A~%" output))
           (format stream "Values:~%~{~A~%~}" result-values)))
        (tool-failure
         (with-output-to-string (stream)
           (when (and form-count (> form-count 1))
             (format stream "Form ~D of ~D failed: " (getf properties :form-index) form-count))
           (format stream "~A" (or message "Worker operation failed."))
           ;; Compiler diagnostics and test reports are printed before the
           ;; error, so they explain a failed load or test run.
           (when (non-empty-string-p output)
             (format stream "~%~%Output:~%~A" output))
           (when (non-empty-string-p backtrace)
             (format stream "~%~%Backtrace:~%~A" backtrace)))))))

(-> lisp-tool-repl-name (hash-table) string)
(defun lisp-tool-repl-name (arguments)
  "Return the validated REPL selected by tool ARGUMENTS."
  (lisp-repl--validate-name
   (or (tool-argument arguments "repl") "default")))


(-> lisp-tool-asd-pathname
    (tool-context hash-table non-empty-string)
    (option pathname))
(defun lisp-tool-asd-pathname (context arguments tool-name)
  "Return the optional canonical ASD file selected by ARGUMENTS."
  (let ((requested (tool-argument arguments "asd")))
    (when requested
      (unless (non-empty-string-p requested)
        (error 'tool-error
               :message (format nil
                                "The ~A asd argument must be a non-empty string."
                                tool-name)
               :tool-name tool-name))
      (let ((path (workspace-tool-path context requested :tool-name tool-name)))
        (unless (uiop:file-exists-p path)
          (error 'tool-error
                 :message (format nil
                                  "The ~A asd argument does not name an existing file: ~A"
                                  tool-name path)
                 :tool-name tool-name))
        path))))

(-> lisp-tool-invoke-managed-execution
    (tool-context hash-table
     &key (:tool-name non-empty-string)
       (:summary string)
       (:operation-function function))
    tool-result)
(defun lisp-tool-invoke-managed-execution
    (context arguments &key tool-name summary operation-function)
  "Run one Lisp-manager operation directly or through an inspectable session job."
  (tool-execution-invoke
   (tool-context-execution-runtime context)
   (tool-context-agent context)
   :tool-name tool-name
   :summary summary
   :operation-function (worker-host--bound-function operation-function :context context)
   :async-p (tool-boolean-argument arguments "async" :tool-name tool-name)
   :parent-call-id (tool-context-call-id context)))

(-> lisp-tool-invoke-execution
    (tool-context hash-table
     &key (:tool-name non-empty-string)
       (:summary string)
       (:operation-function function))
    tool-result)
(defun lisp-tool-invoke-execution
    (context arguments &key tool-name summary operation-function)
  "Run one named-worker operation directly or through an inspectable session job.

Worker resolution runs inside the job. Cancellation during a protocol request detaches
that worker, while cancellation after a completed request leaves the REPL intact."
  (let ((manager (tool-context-worker context))
        (repl-name (lisp-tool-repl-name arguments)))
    (lisp-tool-invoke-managed-execution
     context arguments
     :tool-name tool-name
     :summary (format nil "Lisp REPL ~A: ~A" repl-name summary)
     :operation-function
     (lambda ()
       (let ((worker (lisp-worker-manager-worker manager repl-name)))
         (funcall operation-function worker))))))

(defmethod tool-execute ((tool lisp-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate or compile and execute the required forms in order through CONTEXT's worker."
  (declare (ignore tool))
  (let ((forms (tool-forms-argument arguments "lisp.eval"))
        (compile-p
          (tool-boolean-argument arguments "compile" :tool-name "lisp.eval")))
    (multiple-value-bind (tools enabled-p) (worker-host--allowlist context arguments)
      (lisp-tool-invoke-execution
       context arguments
       :tool-name "lisp.eval"
       :summary (format nil "~{~A~^ ~}" forms)
       :operation-function
       (lambda (worker)
         (worker-response-tool-result
          (worker-host-eval-request worker
                                    (if compile-p ':compile ':eval)
                                    (list :forms forms)
                                    :context context :tools tools :enabled-p enabled-p)))))))

(defmethod tool-execute ((tool lisp-load-system-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Load the required system once through CONTEXT's isolated worker."
  (declare (ignore tool))
  (let* ((system (tool-argument arguments "system" :required t))
         (asd-pathname
           (lisp-tool-asd-pathname context arguments "lisp.load-system")))
    (lisp-tool-invoke-execution
     context arguments
     :tool-name "lisp.load-system"
     :summary (format nil "Load system ~A" system)
     :operation-function
     (lambda (worker)
       (worker-response-tool-result
        (lisp-worker-request
         worker
         :load-system
         (list :system system
               :asd-pathname (and asd-pathname
                                  (namestring asd-pathname)))))))))

(-> lisp-tool--active-target-p (tool-context hash-table string) boolean)
(defun lisp-tool--active-target-p (context arguments tool-name)
  "Return true for an authorized self target, rejecting unknown targets."
  (let ((target (or (tool-argument arguments "target") "worker")))
    (cond
      ((string= target "worker")
       nil)
      ((string= target "self")
       (when (resource-context-child-agent-p context)
         (error 'tool-error
                :message "Task child agents cannot inspect the active image."
                :tool-name tool-name))
       t)
      (t
       (error 'tool-error
              :message (format nil "~A target must be worker or self." tool-name)
              :tool-name tool-name)))))

(-> lisp-describe-active-image (tool-context hash-table) tool-result)
(defgeneric lisp-describe-active-image (context arguments)
  (:documentation "Describe one active-image symbol selected by ARGUMENTS."))

(-> lisp-source-active-image (tool-context hash-table) tool-result)
(defgeneric lisp-source-active-image (context arguments)
  (:documentation "Read active-image source selected by ARGUMENTS."))

(defmethod tool-execute ((tool lisp-describe-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Describe the required designator in a worker or the active image."
  (declare (ignore tool))
  (let ((designator (tool-argument arguments "designator" :required t)))
    (if (lisp-tool--active-target-p context arguments "lisp.describe")
        (lisp-describe-active-image context arguments)
        (lisp-tool-invoke-execution
         context arguments
         :tool-name "lisp.describe"
         :summary (format nil "Describe ~A" designator)
         :operation-function
         (lambda (worker)
           (worker-response-tool-result
            (lisp-worker-request
             worker :describe (list :designator designator))))))))

(defmethod tool-execute ((tool lisp-source-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read matching source from a worker or the active image."
  (declare (ignore tool))
  (let ((name (tool-argument arguments "name" :required t))
        (kind (tool-argument arguments "kind")))
    (if (lisp-tool--active-target-p context arguments "lisp.source")
        (lisp-source-active-image context arguments)
        (lisp-tool-invoke-execution
         context arguments
         :tool-name "lisp.source"
         :summary (format nil "Read source for ~A" name)
         :operation-function
         (lambda (worker)
           (worker-response-tool-result
            (lisp-worker-request
             worker :source (list :name name :kind kind))))))))

(defmethod tool-execute ((tool lisp-run-tests-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run the required system's tests once through CONTEXT's isolated worker."
  (declare (ignore tool))
  (let* ((system (tool-argument arguments "system" :required t))
         (asd-pathname
           (lisp-tool-asd-pathname context arguments "lisp.run-tests")))
    (lisp-tool-invoke-execution
     context arguments
     :tool-name "lisp.run-tests"
     :summary (format nil "Run tests for system ~A" system)
     :operation-function
     (lambda (worker)
       (worker-response-tool-result
        (lisp-worker-request
         worker
         :run-tests
         (list :system system
               :asd-pathname (and asd-pathname
                                  (namestring asd-pathname)))))))))

(defmethod tool-execute ((tool lisp-reset-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Reset the named REPL to pristine or an explicitly selected saved image."
  (declare (ignore tool))
  (let* ((manager (tool-context-worker context))
         (name (lisp-tool-repl-name arguments))
         (image (or (tool-argument arguments "image")
                    (pristine-lisp-image-identifier))))
    (lisp-tool-invoke-managed-execution
     context arguments
     :tool-name "lisp.reset"
     :summary (format nil "Reset Lisp REPL ~A from image ~A" name image)
     :operation-function
     (lambda ()
       (lisp-worker-manager-reset manager name image)
       (tool-success
        (format nil "Lisp REPL ~A was reset from image ~A." name image))))))

(defmethod tool-execute ((tool lisp-start-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Start a named REPL from pristine or one compatible saved image."
  (declare (ignore tool))
  (let* ((manager (tool-context-worker context))
         (name (lisp-tool-repl-name arguments))
         (image (or (tool-argument arguments "image")
                    (pristine-lisp-image-identifier))))
    (lisp-tool-invoke-managed-execution
     context arguments
     :tool-name "lisp.start"
     :summary (format nil "Start Lisp REPL ~A from image ~A" name image)
     :operation-function
     (lambda ()
       (let ((worker (lisp-worker-manager-start manager name image)))
         (tool-success
          (format nil "Lisp REPL ~A is running from image ~A."
                  name
                  (lisp-worker-image-identifier worker))))))))

(defmethod tool-execute ((tool lisp-stop-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Stop and forget one named REPL."
  (declare (ignore tool))
  (let ((manager (tool-context-worker context))
        (name (lisp-tool-repl-name arguments)))
    (lisp-tool-invoke-managed-execution
     context arguments
     :tool-name "lisp.stop"
     :summary (format nil "Stop Lisp REPL ~A" name)
     :operation-function
     (lambda ()
       (lisp-worker-manager-stop-worker manager name)
       (tool-success (format nil "Lisp REPL ~A was stopped." name))))))

(defmethod tool-execute ((tool lisp-repls-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List every named REPL in CONTEXT's worker pool."
  (declare (ignore tool arguments))
  (tool-success
   (lisp-worker-manager-render (tool-context-worker context))))

(defmethod tool-execute ((tool lisp-images-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List pristine and saved Lisp worker images visible to CONTEXT."
  (declare (ignore tool arguments))
  (tool-success
   (lisp-image-render-inventory (tool-context-configuration context))))

(defmethod tool-execute ((tool lisp-save-image-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Save one named REPL as an immutable worker image with a durable note."
  (declare (ignore tool))
  (let ((identifier (tool-argument arguments "image" :required t))
        (note (tool-argument arguments "note" :required t)))
    (lisp-tool-invoke-execution
     context arguments
     :tool-name "lisp.save-image"
     :summary (format nil "Save image ~A" identifier)
     :operation-function
     (lambda (worker)
       (let ((image
               (lisp-worker-save-image
                (tool-context-configuration context)
                worker
                :identifier identifier
                :note note)))
         (tool-success
          (format nil "Saved Lisp REPL ~A as image ~A.~%Parent: ~A~%Note: ~A"
                  (lisp-worker-name worker)
                  (lisp-image-identifier image)
                  (lisp-image-parent-identifier image)
                  (lisp-image-note image))))))))


;;;; -- Worker Runtime Entry Points --

(-> worker-source (string (option string)) (values list string))
(defun worker-source (name kind)
  "Return exact matching SBCL source for NAME and optional KIND."
  (sbcl-worker-runtime-configure
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version *lisp-worker-protocol-version*
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT")
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-source name kind))))

(-> worker-handle-request (list) list)
(defun worker-handle-request (request)
  "Execute one portable worker REQUEST through sbcl-workers."
  (sbcl-worker-runtime-configure
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version *lisp-worker-protocol-version*
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT")
  (sbcl-worker-handle-request request))

(-> worker-main () null)
(defun worker-main ()
  "Run Autolith's isolated worker protocol until standard-input reaches EOF."
  (sbcl-worker-main
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version *lisp-worker-protocol-version*
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT"))
