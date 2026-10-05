(in-package #:autolith)

;;;; -- Revisioned Workspace Change Sets --

(defclass workspace-content-operation ()
  ((content
    :initarg :content
    :reader workspace-content-operation-content
    :type (option string)
    :documentation "Complete replacement text, or NIL for removal of a regular file.")
   (permission-donor
    :initarg :permission-donor
    :initform nil
    :reader workspace-content-operation-permission-donor
    :type (option workspace-file-observation)
    :documentation "Exact source snapshot whose staging-time permissions a moved file receives."))
  (:documentation "An internal semantic edit prepared from an exact workspace observation."))

(defclass workspace-prepared-change (cl-resources:prepared-change)
  ((base
    :initarg :base
    :reader workspace-prepared-change-base
    :type workspace-file-observation
    :documentation "The exact original snapshot retained for validation and undo.")
   (content
    :initarg :content
    :reader workspace-prepared-change-content
    :type (option string)
    :documentation "Prepared replacement text, or NIL for deletion.")
   (backup
    :initform nil
    :accessor workspace-prepared-change-backup
    :type (option pathname)
    :documentation "Owned undo file preserving the original bytes and permissions.")
   (permissions
    :initform nil
    :accessor workspace-prepared-change-permissions
    :documentation "Restorable source permissions captured before ordered moves remove their source.")
   (original-permissions
    :initform nil
    :accessor workspace-prepared-change-original-permissions
    :documentation "Restorable destination permissions retained independently of undo-file aliases.")
   (restored-p
    :initform nil
    :accessor workspace-prepared-change-restored-p
    :type boolean
    :documentation "Whether compensation installed the original filesystem state.")
   (attempted-p
    :initform nil
    :accessor workspace-prepared-change-attempted-p
    :type boolean
    :documentation "Whether commit entered the mutation boundary.")
   (published
    :initform nil
    :accessor workspace-prepared-change-published
    :type (option workspace-file-observation)
    :documentation "The resulting snapshot when commit completed its observation."))
  (:documentation "A recoverable workspace adapter payload for cl-resources change sets."))

(defmethod cl-resources:resource-call-with-transaction
    ((resource workspace-file-resource) (context tool-context) thunk)
  "Authorize RESOURCE and serialize validation, publication and undo."
  (workspace-file--call-with-authorized-access
   resource context ':edit
   (lambda ()
     (with-recursive-lock-held (*workspace-file-mutation-lock*)
       (with-recursive-lock-held
           ((conversation-resource-observation-lock
             (tool-context-conversation context)))
         (funcall thunk))))))

(defmethod cl-resources:resource-check-revision
    ((resource workspace-file-resource) (context tool-context) expected-revision)
  "Resolve a conversation-local revision handle and validate its exact snapshot."
  (let* ((state (workspace-file--find-observation-state
                 (tool-context-conversation context)
                 (resource-uri resource) expected-revision))
         (base (resource-observation-state-observation state))
         (current (workspace-file--observe-path resource context)))
    (unless (workspace-file--same-observation-p base current)
      (workspace-file--signal-stale resource base current))
    current))

(defmethod cl-resources:resource-stage-operations
    ((resource workspace-file-resource) (context tool-context)
     &key base-revision operations)
  "Prepare original-coordinate edits or one internal semantic replacement."
  (let* ((state (workspace-file--find-observation-state
                 (tool-context-conversation context)
                 (resource-uri resource) base-revision))
         (base (resource-observation-state-observation state))
         (current (workspace-file--observe-path resource context)))
    (unless (workspace-file--same-observation-p base current)
      (workspace-file--signal-stale resource base current))
    (when (eq (workspace-file-observation-kind base) ':directory)
      (error 'resource-operation-unsupported
             :uri (resource-uri resource) :operation ':stage))
    (let ((content
            (if (and (= (length operations) 1)
                     (typep (first operations) 'workspace-content-operation))
                (workspace-content-operation-content (first operations))
                (handler-case
                    (cl-hashline:edit-text
                     (resource-observation-content base)
                     (mapcar #'workspace-file--json-operation operations)
                     :visible-lines
                     (workspace-file-observation-state-visible-ranges state)
                     :split-lines #'text--split-lines
                     :anchor-maximum-offset
                     *workspace-file-resource-anchor-maximum-offset*)
                  (cl-hashline:hashline-error (condition)
                    (error 'tool-error
                           :tool-name "resource.edit"
                           :message (princ-to-string condition)))))))
      (when (and (null content)
                 (not (eq (workspace-file-observation-kind base) ':file)))
        (error 'resource-operation-unsupported
               :uri (resource-uri resource) :operation ':delete))
      (when content
        (workspace-file--replacement-octets content))
      (unless (uiop:directory-exists-p
               (uiop:pathname-directory-pathname
                (workspace-file-resource-pathname resource)))
        (error 'resource-operation-unsupported
               :uri (resource-uri resource) :operation ':create-parent-directory))
      (let ((change (make-instance 'workspace-prepared-change
                                   :resource resource :base-revision base-revision
                                   :base base :content content))
            (complete-p nil))
        (unwind-protect
             (progn
               (when (and (= (length operations) 1)
                          (typep (first operations) 'workspace-content-operation))
                 (workspace-prepared-change--retain-permissions
                  change context (workspace-content-operation-permission-donor
                                  (first operations))))
               (setf complete-p t)
               change)
          (unless complete-p
            (workspace-prepared-change--release change)))))))

(-> workspace-prepared-change--retain-permissions
    (workspace-prepared-change tool-context (option workspace-file-observation)) null)
(defun workspace-prepared-change--retain-permissions (change context donor)
  "Capture authorized DONOR permissions without allocating a staging artifact."
  (when donor
    (unless (and (workspace-prepared-change-content change)
                 (eq (workspace-file-observation-kind donor) ':file))
      (error 'resource-operation-unsupported
             :uri (resource-observation-uri donor) :operation ':copy-permissions))
    (let ((resource (resource-registry-resolve
                     (tool-registry-resource-registry (tool-context-registry context))
                     (resource-observation-uri donor) context)))
      (workspace-file--call-with-authorized-access
       resource context ':read
       (lambda ()
         (let ((current (resource-observe resource context)))
           (unless (workspace-file--same-observation-p donor current)
             (workspace-file--signal-stale resource donor current)))
         (setf (workspace-prepared-change-permissions change)
               (platform-file-permissions *platform* (workspace-file-resource-pathname resource)))
         (let ((after (resource-observe resource context)))
           (unless (workspace-file--same-observation-p donor after)
             (workspace-file--signal-stale resource donor after)))))))
  nil)

(defmethod cl-resources:prepared-change-validate
    ((change workspace-prepared-change) (context tool-context))
  "Reject a stale retained snapshot before any member of the set commits."
  (let* ((resource (cl-resources:prepared-change-resource change))
         (base (workspace-prepared-change-base change))
         (current (workspace-file--observe-path resource context)))
    (unless (workspace-file--same-observation-p base current)
      (workspace-file--signal-stale resource base current))))

(-> workspace-prepared-change--save-undo (workspace-prepared-change) null)
(defun workspace-prepared-change--save-undo (change)
  "Retain original bytes and permissions before entering the mutation boundary."
  (when (and (eq (workspace-file-observation-kind
                 (workspace-prepared-change-base change)) ':file)
             (null (workspace-prepared-change-backup change)))
    (let* ((path (workspace-file-resource-pathname
                  (cl-resources:prepared-change-resource change)))
           (backup (workspace-file--temporary-path path))
           (octets (workspace-file--replacement-octets
                    (resource-observation-content
                     (workspace-prepared-change-base change)))))
      ;; Register exclusive ownership before an asynchronous cancellation can run.
      (sb-sys:without-interrupts
        (with-open-file (stream backup :direction ':output
                                      :if-exists ':error
                                      :if-does-not-exist ':create
                                      :element-type '(unsigned-byte 8))
          (setf (workspace-prepared-change-backup change) backup
                (workspace-prepared-change-original-permissions change)
                (platform-file-permissions *platform* path))
          (sb-sys:with-local-interrupts
            (write-sequence octets stream)
            (finish-output stream))))
      (setf (platform-file-permissions *platform* backup)
            (workspace-prepared-change-original-permissions change))))
  nil)

(defmethod cl-resources:prepared-change-commit
    ((change workspace-prepared-change) (context tool-context))
  "Publish the prepared replacement with exact pre-publication validation."
  (let* ((resource (cl-resources:prepared-change-resource change))
         (base (workspace-prepared-change-base change))
         (content (workspace-prepared-change-content change))
         (permissions (workspace-prepared-change-permissions change))
         (publish *workspace-file-resource-publish-function*)
         (create *workspace-file-resource-create-function*))
    (workspace-prepared-change--save-undo change)
    (cl-resources:prepared-change-validate change context)
    (labels ((install (function temporary target)
               "Retain the actual mutation outcome, including partial failed publication."
               (when permissions
                 (setf (platform-file-permissions *platform* temporary) permissions))
               (unwind-protect
                    (funcall function temporary target)
                 (sb-sys:without-interrupts
                   (setf (workspace-prepared-change-published change)
                         (workspace-file--observe-path resource context))))))
      (let ((*workspace-file-resource-publish-function*
              (lambda (temporary target) (install publish temporary target)))
            (*workspace-file-resource-create-function*
              (lambda (temporary target) (install create temporary target))))
        (setf (workspace-prepared-change-attempted-p change) t
              (workspace-prepared-change-published change)
              (if content
                  (workspace-file--publish resource context base content)
                  (progn
                    (platform-delete-file *platform* (workspace-file-resource-pathname resource))
                    (workspace-file--observe-path resource context))))))))

(-> workspace-prepared-change--installed-p
    (workspace-prepared-change workspace-file-observation) boolean)
(defun workspace-prepared-change--installed-p (change current)
  "Return true when CURRENT is exactly the state this commit could install."
  (let ((published (workspace-prepared-change-published change))
        (content (workspace-prepared-change-content change)))
    (if published
        (workspace-file--same-observation-p published current)
        (and (workspace-prepared-change-attempted-p change)
             (if content
                 (and (eq (workspace-file-observation-kind current) ':file)
                      (string= content (resource-observation-content current)))
                 (eq (workspace-file-observation-kind current) ':missing))
             t))))

(defmethod cl-resources:prepared-change-rollback
    ((change workspace-prepared-change) (context tool-context))
  "Restore original content and permissions without replacing a later external change."
  (let* ((resource (cl-resources:prepared-change-resource change))
         (path (workspace-file-resource-pathname resource))
         (base (workspace-prepared-change-base change))
         (current (workspace-file--observe-path resource context)))
    (unless (workspace-file--same-observation-p base current)
      (unless (workspace-prepared-change--installed-p change current)
        (workspace-file--signal-stale resource base current))
      (if (eq (workspace-file-observation-kind base) ':missing)
          (platform-delete-file *platform* path)
          (let ((backup (workspace-prepared-change-backup change)))
            (unless (and backup (probe-file backup))
              (error 'cl-resources:resource-protocol-error
                     :uri (resource-uri resource)
                     :reason "The original workspace undo file is unavailable."))
            (funcall
             (if (eq (workspace-file-observation-kind current) ':missing)
                 *workspace-file-resource-create-function*
                 *workspace-file-resource-publish-function*)
             backup path))))
    (let ((restored (workspace-file--observe-path resource context)))
      (unless (workspace-file--same-observation-p base restored)
        (workspace-file--signal-stale resource base restored)))
    (setf (workspace-prepared-change-restored-p change) t)
    (when (workspace-prepared-change-original-permissions change)
      (setf (platform-file-permissions *platform* path)
            (workspace-prepared-change-original-permissions change))))
  nil)

(-> workspace-prepared-change--release (workspace-prepared-change) null)
(defun workspace-prepared-change--release (change)
  "Release owned undo storage without modifying permissions through a restored hard link."
  (let ((backup (workspace-prepared-change-backup change)))
    (when backup
      (let* ((path (workspace-file-resource-pathname
                    (cl-resources:prepared-change-resource change)))
             (backup-status (platform-path-status *platform* backup))
             (target-status (and (workspace-prepared-change-restored-p change)
                                 (platform-path-status *platform* path)))
             (aliased-p (and backup-status target-status
                             (equal (platform-file-status-identity backup-status)
                                    (platform-file-status-identity target-status)))))
        (unwind-protect
             (when backup-status
               (platform-delete-file *platform* backup))
          (when aliased-p
            (setf (platform-file-permissions *platform* path)
                  (workspace-prepared-change-original-permissions change))))
        (setf (workspace-prepared-change-backup change) nil))))
  nil)

(defmethod cl-resources:prepared-change-discard
    ((change workspace-prepared-change) context)
  "Release undo storage for an uncommitted or rolled-back change."
  (declare (ignore context))
  (workspace-prepared-change--release change))

(defmethod cl-resources:prepared-change-finalize
    ((change workspace-prepared-change) context)
  "Release undo storage after the complete change set publishes."
  (declare (ignore context))
  (workspace-prepared-change--release change))

(-> workspace-change-set-apply (tool-context list) list)
(defun workspace-change-set-apply (context requests)
  "Apply revisioned REQUESTS through the staged library transaction protocol.

Backend publication is atomic per file. On failure, retain the change-set in
CL-RESOURCES:CHANGE-SET-ERROR for explicit recovery if undo or cleanup failed.
Defer interruption through staging ownership transfer; an interrupted commit
compensates under the library lifecycle protocol."
  (sb-sys:without-interrupts
    (let ((set (cl-resources:change-set-stage requests context)))
      (unwind-protect
           (sb-sys:with-local-interrupts
             (multiple-value-bind (committed observations)
                 (cl-resources:change-set-commit set)
               (declare (ignore committed))
               observations))
        (when (eq (cl-resources:change-set-state set) ':staged)
          (cl-resources:change-set-abort set))))))
