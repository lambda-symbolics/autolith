(in-package #:autolith)

;;;; -- Optional Structural Workspace Adapter --

(defparameter *structural-program* nil
  "Explicit ast-grep executable pathname, or NIL to leave the optional backend disabled.")

(defparameter *structural-maximum-bytes* (* 1024 1024)
  "Maximum size, in UTF-8 bytes, of source text or a preview for one structural operation.")

(defparameter *structural-maximum-proposals* 16
  "Maximum number of rewrite proposals retained per conversation.")

(define-condition structural-workspace-error (tool-error)
  ((code :initarg :code :reader structural-workspace-error-code
         :documentation "Machine-readable adapter failure category."))
  (:documentation "An optional structural operation violates workspace adapter policy."))

(defclass structural-state (edit-proposal-store) ()
  (:documentation "Registry-owned structural proposal store with an independent retention budget."))

(defclass structural-proposal (edit-proposal)
  ((resource :initarg :resource :reader structural-proposal-resource
             :documentation "Authorized ordinary workspace resource.")
   (plan :initarg :plan :reader structural-proposal-plan
         :documentation "Immutable library plan against an exact resource revision."))
  (:documentation "A previewed replacement published only through ordinary staged resources."))

(defmethod edit-proposal-store-limits ((store structural-state))
  "Preserve the structural entry budget independently of semantic choices."
  (values *structural-maximum-proposals* most-positive-fixnum most-positive-fixnum))

(defmethod edit-proposal-fail ((store structural-state) code message)
  "Signal policy failures as typed structural errors."
  (structural--fail code message))

(-> structural--fail (keyword string) null)
(defun structural--fail (code message)
  "Signal a typed adapter failure without source text or process output."
  (error 'structural-workspace-error :tool-name "structural" :code code :message message))

(-> structural--snapshot (tool-context string string)
    (values workspace-file-resource clasted:snapshot workspace-file-observation))
(defun structural--snapshot (context uri revision)
  "Return the authorized resource, exact library snapshot and owned baseline observation."
  (unless (uiop:string-prefix-p "workspace:" uri)
    (structural--fail ':unsupported-resource "Structural queries require a workspace: file resource."))
  (let* ((resource (resource-registry-resolve
                    (tool-registry-resource-registry (tool-context-registry context)) uri context))
         (observation
           (workspace-file--call-with-authorized-access
            resource context ':read
            :function
            (lambda () (cl-resources:resource-check-revision resource context revision)))))
    (unless (eq (workspace-file-observation-kind observation) ':file)
      (structural--fail ':unsupported-resource "Structural queries require an existing regular text file."))
    (values resource
            (clasted:make-snapshot :file (resource-uri resource) :revision revision
                                   :text          (resource-observation-content observation)
                                   :maximum-bytes *structural-maximum-bytes*)
            observation)))

(-> structural--backend
    (tool-context string &key (:cancelled-p (option function))) clasted:ast-grep-backend)
(defun structural--backend (context program &key cancelled-p)
  "Resolve PROGRAM with path authorization and authorize every immutable-stdin invocation."
  (let ((path (workspace-tool-path context program :tool-name "structural")))
    (unless (uiop:file-exists-p path)
      (structural--fail ':unavailable "Configured ast-grep executable is unavailable."))
    (clasted:make-ast-grep-backend
     :program (uiop:native-namestring path)
     :maximum-matches 1000 :maximum-output-bytes (* 2 *structural-maximum-bytes*)
     :maximum-preview-bytes *structural-maximum-bytes* :timeout 30 :cancelled-p cancelled-p
     :runner
     (lambda (argv stdin &key maximum-output-bytes timeout cancelled-p)
       (unless (eq (tool-context-authorize-command
                    context (format nil "~{~A~^ ~}" (mapcar #'uiop:escape-shell-token argv))
                    (config :working-directory (tool-context-configuration context)))
                   ':full-access)
         (structural--fail ':authorization "Structural backend execution requires full-access command approval."))
       (clasted:run-ast-grep argv stdin :maximum-output-bytes maximum-output-bytes
                            :timeout timeout :cancelled-p cancelled-p)))))

(-> structural--request (structural-proposal) cl-resources:resource-change)
(defun structural--request (proposal)
  "Build a fresh revision-guarded resource request from a retained immutable preview."
  (let ((plan (structural-proposal-plan proposal)))
    (cl-resources:make-resource-change
     (structural-proposal-resource proposal)
     :base-revision (resource-observation-state-alias
                     (second (gethash (resource-uri (structural-proposal-resource proposal))
                                      (edit-proposal-snapshots proposal))))
     :operations    (list (make-instance 'workspace-content-operation :content (clasted:plan-preview plan))))))

(-> structural--present (structural-proposal) json-object)
(defun structural--present (proposal)
  "Return the complete bounded preview and the edit list in original coordinates."
  (let* ((plan     (structural-proposal-plan proposal))
         (snapshot (clasted:plan-snapshot plan)))
    (json-object
     "proposal" (edit-proposal-identifier proposal)
     "uri" (clasted:snapshot-file snapshot) "revision" (clasted:snapshot-revision snapshot)
     "snapshot" (clasted:snapshot-id snapshot) "preview" (clasted:plan-preview plan)
     "edits" (map 'vector (lambda (edit)
                             (json-object "start" (clasted:edit-start edit) "end" (clasted:edit-end edit)
                                          "replacement" (clasted:edit-replacement edit)))
                   (clasted:plan-edits plan)))))

(-> structural--find (structural-state tool-context string) structural-proposal)
(defun structural--find (state context identifier)
  "Find an unconsumed structural proposal in this conversation."
  (edit-proposal-find state context identifier))

(defmethod edit-proposal-requests ((context tool-context) (proposal structural-proposal))
  "Return a list containing the replacement request, or NIL if the plan has no edits."
  (when (plusp (length (clasted:plan-edits (structural-proposal-plan proposal))))
    (list (structural--request proposal))))

(defmethod edit-proposal-access ((proposal structural-proposal))
  "Use read-only access to validate freshness for an authorized rewrite with no edits."
  (if (plusp (length (clasted:plan-edits (structural-proposal-plan proposal)))) ':edit ':read))

(-> structural--prepare
    (structural-state tool-context &key (:resource workspace-file-resource) (:plan clasted:edit-plan)
                                       (:observation workspace-file-observation))
    structural-proposal)
(defun structural--prepare (state context &key resource plan observation)
  "Take ownership of the exact baseline and stage the preview before retaining the proposal."
  (let* ((snapshots (make-hash-table :test #'equal))
         (proposal  (make-instance 'structural-proposal :resource resource :plan plan :snapshots snapshots)))
    (setf (gethash (resource-uri resource) snapshots)
          (list resource (make-instance 'workspace-file-observation-state
                                        :alias (format nil "P~A" (daemon-random-token))
                                        :observation observation :visible-ranges nil)))
    (edit-proposal-call-with-observations
     context proposal
     (lambda ()
       (sb-sys:without-interrupts
         (let ((set (cl-resources:change-set-stage (list (structural--request proposal)) context)))
           (unwind-protect
                (sb-sys:with-local-interrupts (cl-resources:change-set-abort set))
             (when (eq (cl-resources:change-set-state set) ':staged)
               (cl-resources:change-set-abort set)))))))
    (edit-proposal-retain state context proposal)))

(-> structural--apply
    (structural-state tool-context structural-proposal &key (:cancelled-p (option function))) list)
(defun structural--apply (state context proposal &key cancelled-p)
  "Publish once through the shared lifecycle and ordinary resources."
  (edit-proposal-apply state context proposal
                       :before (lambda ()
                                 (when (and cancelled-p (funcall cancelled-p))
                                   (structural--fail ':cancelled "Structural publication was cancelled.")))))
