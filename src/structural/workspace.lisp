(in-package #:autolith)

;;;; -- Optional Structural Workspace Adapter --

(defparameter *structural-program* nil
  "Explicit ast-grep executable pathname, or NIL to leave the optional backend disabled.")

(defparameter *structural-maximum-bytes* (* 1024 1024)
  "Maximum UTF-8 source and preview bytes for one structural operation.")

(defparameter *structural-maximum-proposals* 16
  "Maximum retained rewrite proposals per conversation.")

(define-condition structural-workspace-error (tool-error)
  ((code :initarg :code :reader structural-workspace-error-code
         :documentation "Machine-readable adapter failure category."))
  (:documentation "An optional structural operation violates workspace adapter policy."))

(defclass structural-state ()
  ((entries :initform (make-hash-table :test #'eq :weakness :key)
            :reader structural-state-entries
            :documentation "Conversation-keyed bounded immutable rewrite proposals.")
   (lock :initform (make-lock "structural proposals") :reader structural-state-lock
         :documentation "Serializes proposal retention and consumption."))
  (:documentation "Registry-owned state; queries retain no backend state or source snapshots."))

(defclass structural-proposal ()
  ((identifier :initform (daemon-random-token) :reader structural-proposal-identifier
               :documentation "Opaque conversation-local identifier.")
   (resource :initarg :resource :reader structural-proposal-resource
             :documentation "Authorized ordinary workspace resource.")
   (plan :initarg :plan :reader structural-proposal-plan
         :documentation "Immutable library plan against an exact resource revision.")
   (applying-p :initform nil :accessor structural-proposal-applying-p :type boolean
               :documentation "Whether one invocation reserves this proposal under its state lock."))
  (:documentation "A previewed replacement published only through ordinary staged resources."))

(-> structural--fail (keyword string) null)
(defun structural--fail (code message)
  "Signal a typed adapter failure without source text or process output."
  (error 'structural-workspace-error :tool-name "structural" :code code :message message))

(-> structural--snapshot (tool-context string string) (values workspace-file-resource clasted:snapshot))
(defun structural--snapshot (context uri revision)
  "Authorize URI and validate the caller's retained resource REVISION before querying."
  (unless (uiop:string-prefix-p "workspace:" uri)
    (structural--fail ':unsupported-resource "Structural queries require a workspace: file resource."))
  (let* ((resource (resource-registry-resolve
                    (tool-registry-resource-registry (tool-context-registry context)) uri context))
         (observation
           (workspace-file--call-with-authorized-access
            resource context ':read
            (lambda () (cl-resources:resource-check-revision resource context revision)))))
    (unless (eq (workspace-file-observation-kind observation) ':file)
      (structural--fail ':unsupported-resource "Structural queries require an existing regular text file."))
    (values resource
            (clasted:make-snapshot :file (resource-uri resource) :revision revision
                                   :text (resource-observation-content observation)
                                   :maximum-bytes *structural-maximum-bytes*))))

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
  (let* ((plan (structural-proposal-plan proposal))
         (snapshot (clasted:plan-snapshot plan)))
    (cl-resources:make-resource-change
     (structural-proposal-resource proposal) :base-revision (clasted:snapshot-revision snapshot)
     :operations (list (make-instance 'workspace-content-operation :content (clasted:plan-preview plan))))))

(-> structural--present (structural-proposal) json-object)
(defun structural--present (proposal)
  "Return the complete bounded preview and exact original-coordinate edit list."
  (let* ((plan (structural-proposal-plan proposal))
         (snapshot (clasted:plan-snapshot plan)))
    (json-object
     "proposal" (structural-proposal-identifier proposal)
     "uri" (clasted:snapshot-file snapshot) "revision" (clasted:snapshot-revision snapshot)
     "snapshot" (clasted:snapshot-id snapshot) "preview" (clasted:plan-preview plan)
     "edits" (map 'vector (lambda (edit)
                             (json-object "start" (clasted:edit-start edit) "end" (clasted:edit-end edit)
                                          "replacement" (clasted:edit-replacement edit)))
                   (clasted:plan-edits plan)))))

(-> structural--store (structural-state tool-context structural-proposal) structural-proposal)
(defun structural--store (state context proposal)
  "Retain a bounded conversation-local preview without evicting active applications."
  (with-lock-held ((structural-state-lock state))
    (let* ((conversation (tool-context-conversation context))
           (entries (gethash conversation (structural-state-entries state))))
      (loop while (>= (length entries) *structural-maximum-proposals*)
            for oldest = (find-if-not #'structural-proposal-applying-p entries :from-end t)
            do (unless oldest
                 (structural--fail ':proposal-limit "All retained proposal slots are reserved; retry after application completes."))
               (setf entries (remove oldest entries)))
      (setf (gethash conversation (structural-state-entries state)) (cons proposal entries))))
  proposal)

(-> structural--find (structural-state tool-context string) structural-proposal)
(defun structural--find (state context identifier)
  "Find an unconsumed proposal in this conversation, without extending snapshot lifetime."
  (or (with-lock-held ((structural-state-lock state))
        (find identifier (gethash (tool-context-conversation context) (structural-state-entries state))
              :key #'structural-proposal-identifier :test #'equal))
      (structural--fail ':unknown-proposal "Proposal is unknown, consumed or evicted; prepare a new rewrite.")))


(-> structural--reserve (structural-state tool-context structural-proposal) null)
(defun structural--reserve (state context proposal)
  "Atomically reserve one retained proposal for exactly one application invocation."
  (with-lock-held ((structural-state-lock state))
    (unless (member proposal (gethash (tool-context-conversation context) (structural-state-entries state)))
      (structural--fail ':unknown-proposal "Proposal is consumed or evicted; prepare a new rewrite."))
    (when (structural-proposal-applying-p proposal)
      (structural--fail ':proposal-busy "Proposal is already being applied; inspect it or wait for that invocation."))
    (setf (structural-proposal-applying-p proposal) t))
  nil)

(-> structural--prepare
    (structural-state tool-context &key (:resource workspace-file-resource) (:plan clasted:edit-plan))
    structural-proposal)
(defun structural--prepare (state context &key resource plan)
  "Stage and abort the complete preview to validate ordinary edit authority before retention."
  (let ((proposal (make-instance 'structural-proposal :resource resource :plan plan)))
    (sb-sys:without-interrupts
      (let ((set (cl-resources:change-set-stage (list (structural--request proposal)) context)))
        (unwind-protect
             (sb-sys:with-local-interrupts
               (cl-resources:change-set-abort set))
          (when (eq (cl-resources:change-set-state set) ':staged)
            (cl-resources:change-set-abort set)))))
    (structural--store state context proposal)))

(-> structural--apply
    (structural-state tool-context structural-proposal &key (:cancelled-p (option function))) list)
(defun structural--apply (state context proposal &key cancelled-p)
  "Reserve and publish through ordinary resources; failed applications retain inspection and retry."
  (sb-sys:without-interrupts
    (structural--reserve state context proposal)
    (unwind-protect
         (sb-sys:with-local-interrupts
           (when (and cancelled-p (funcall cancelled-p))
             (structural--fail ':cancelled "Structural publication was cancelled."))
           (let* ((plan (structural-proposal-plan proposal))
                  (results
                    (if (plusp (length (clasted:plan-edits plan)))
                        (workspace-change-set-apply context (list (structural--request proposal)))
                        ;; Even a no-match rewrite must reject a stale original snapshot.
                        (progn
                          (workspace-file--call-with-authorized-access
                           (structural-proposal-resource proposal) context ':read
                           (lambda ()
                             (cl-resources:resource-check-revision
                              (structural-proposal-resource proposal) context
                              (clasted:snapshot-revision (clasted:plan-snapshot plan)))))
                          nil))))
             (with-lock-held ((structural-state-lock state))
               (let ((conversation (tool-context-conversation context)))
                 (setf (gethash conversation (structural-state-entries state))
                       (remove proposal (gethash conversation (structural-state-entries state))))))
             results))
      (with-lock-held ((structural-state-lock state))
        (setf (structural-proposal-applying-p proposal) nil)))))
