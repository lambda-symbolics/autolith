;;;; -- Workspace Change Set Tests --

(in-package #:autolith)

(-> workspace-change-tests--context (configuration tool-registry) tool-context)

(defun workspace-change-tests--context (configuration registry)
  "Create a conversation with independent retained resource revisions."
  (make-instance 'tool-context :configuration configuration :registry registry
                 :worker nil :conversation (conversation-create configuration)))

(-> workspace-change-tests--request
    (tool-context string (option string)
     &key (:permission-donor (option workspace-file-observation))) cl-resources:resource-change)
(defun workspace-change-tests--request (context uri content &key permission-donor)
  "Observe URI and construct a content request at its retained revision alias."
  (let* ((registry (tool-context-registry context))
         (resource (resource-registry-resolve
                    (tool-registry-resource-registry registry) uri context))
         (observation (resource-observe resource context))
         (state (resource-observation-state-ensure
                 (tool-context-conversation context) observation :visible-ranges nil)))
    (cl-resources:make-resource-change
     resource :base-revision (resource-observation-state-alias state)
     :operations (list (make-instance 'workspace-content-operation
                                     :content content :permission-donor permission-donor)))))

(-> workspace-change-tests--failure (function) (option condition))

(defun workspace-change-tests--failure (thunk)
  "Return the expected typed boundary failure from THUNK."
  (handler-case (progn (funcall thunk) nil)
                (cl-resources:resource-error (condition) condition)
                (tool-error (condition) condition)))

(-> test-workspace-change-publication nil null)

(defun test-workspace-change-publication ()
  "Publish replacement, creation and deletion using conversation-local revisions."
  (with-test-configuration (configuration fixture-root)
   (let* ((configuration
           (configuration-copy configuration :working-directory fixture-root))
          (registry (make-default-tool-registry :configuration configuration))
          (context (workspace-change-tests--context configuration registry))
          (root (config :working-directory configuration))
          (replacement (merge-pathnames "replace.txt" root))
          (deleted (merge-pathnames "delete.txt" root))
          (created (merge-pathnames "create.txt" root)))
     (unwind-protect
         (progn
          (workspace-resource-tests--write-text replacement "before")
          (workspace-resource-tests--write-text deleted "deleted")
          (platform-make-private *platform* replacement)
          (let ((private-p
                 (platform-file-status-private-p
                  (platform-path-status *platform* replacement)))
                (requests
                 (list
                  (workspace-change-tests--request context
                   "workspace:replace.txt" "after😀")
                  (workspace-change-tests--request context
                   "workspace:create.txt" "new")
                  (workspace-change-tests--request context
                   "workspace:delete.txt" nil))))
            (test-assert
             (= 3 (length (workspace-change-set-apply context requests)))
             "publish all three resources")
            (test-assert
             (string= "after😀" (uiop/stream:read-file-string replacement))
             "replace exact Unicode content")
            (test-assert (string= "new" (uiop/stream:read-file-string created))
             "create requested content")
            (test-assert (not (probe-file deleted)) "remove requested file")
            (test-assert
             (eq private-p
                 (platform-file-status-private-p
                  (platform-path-status *platform* replacement)))
             "preserve replacement permissions")
            (test-assert
             (workspace-change-tests--failure
              (lambda () (workspace-change-set-apply context requests)))
             "reject already consumed revisions")))
       (tool-registry-close-runtime-state registry))))
  nil)

(-> test-workspace-change-stale-and-authority nil null)

(defun test-workspace-change-stale-and-authority ()
  "Reject any stale member before publication and reject aliases from another caller."
  (with-test-configuration (configuration fixture-root)
   (let* ((configuration
           (configuration-copy configuration :working-directory fixture-root))
          (registry (make-default-tool-registry :configuration configuration))
          (context (workspace-change-tests--context configuration registry))
          (other (workspace-change-tests--context configuration registry))
          (root (config :working-directory configuration))
          (first (merge-pathnames "first.txt" root))
          (second (merge-pathnames "second.txt" root)))
     (unwind-protect
         (progn
          (workspace-resource-tests--write-text first "first")
          (workspace-resource-tests--write-text second "second")
          (let ((requests
                 (list
                  (workspace-change-tests--request context
                   "workspace:first.txt" "changed")
                  (workspace-change-tests--request context
                   "workspace:second.txt" "changed"))))
            (test-assert
             (workspace-change-tests--failure
              (lambda () (workspace-change-set-apply other requests)))
             "reject another conversation's revisions")
            (workspace-resource-tests--write-text second "external")
            (test-assert
             (typep
              (workspace-change-tests--failure
               (lambda () (workspace-change-set-apply context requests)))
              'resource-revision-stale)
             "reject a stale batch member")
            (test-assert (string= "first" (uiop/stream:read-file-string first))
             "validate the batch before writing")
            (test-assert
             (string= "external" (uiop/stream:read-file-string second))
             "preserve an external update"))
          (let ((request
                 (workspace-change-tests--request context
                  "workspace:absent-parent/new.txt" "new")))
            (test-assert
             (typep
              (workspace-change-tests--failure
               (lambda () (workspace-change-set-apply context (list request))))
              'resource-operation-unsupported)
             "reject absent parent directories")))
       (tool-registry-close-runtime-state registry))))
  nil)

(-> test-workspace-change-compensation nil null)

(defun test-workspace-change-compensation ()
  "Undo already installed changes when later publication fails, including partial failure."
  (dolist (partial-p '(nil t))
    (with-test-configuration (configuration fixture-root)
     (let* ((configuration
             (configuration-copy configuration :working-directory
                                 fixture-root))
            (registry
             (make-default-tool-registry :configuration configuration))
            (context (workspace-change-tests--context configuration registry))
            (root (config :working-directory configuration))
            (first (merge-pathnames "first.txt" root))
            (second (merge-pathnames "second.txt" root)))
       (unwind-protect
           (progn
            (workspace-resource-tests--write-text first "first")
            (workspace-resource-tests--write-text second "second")
            (platform-make-private *platform* first)
            (let* ((requests
                    (list
                     (workspace-change-tests--request context
                      "workspace:first.txt" "changed-first")
                     (workspace-change-tests--request context
                      "workspace:second.txt" "changed-second")))
                   (publish *workspace-file-resource-publish-function*)
                   (failed-p nil)
                   (*workspace-file-resource-publish-function*
                    (lambda (temporary target)
                      (if (and (equal target second) (not failed-p))
                          (progn
                           (setf failed-p t)
                           (when partial-p (funcall publish temporary target))
                           (error 'cl-resources:resource-protocol-error :uri
                                  "workspace:second.txt" :reason
                                  "injected publication failure"))
                          (funcall publish temporary target)))))
              (test-assert
               (workspace-change-tests--failure
                (lambda () (workspace-change-set-apply context requests)))
               "propagate publication failure")
              (test-assert failed-p "exercise injected failure")
              (test-assert
               (string= "first" (uiop/stream:read-file-string first))
               "restore the first installed member")
              (test-assert
               (string= "second" (uiop/stream:read-file-string second))
               "restore a partially installed member")
              (test-assert
               (platform-file-status-private-p
                (platform-path-status *platform* first))
               "preserve original rollback permissions")))
         (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-workspace-change-recovery nil null)

(defun test-workspace-change-recovery ()
  "Retain undo ownership when an external writer intervenes, then recover explicitly."
  (with-test-configuration (configuration fixture-root)
   (let* ((configuration
           (configuration-copy configuration :working-directory fixture-root))
          (registry (make-default-tool-registry :configuration configuration))
          (context (workspace-change-tests--context configuration registry))
          (root (config :working-directory configuration))
          (first (merge-pathnames "first.txt" root))
          (second (merge-pathnames "second.txt" root)))
     (unwind-protect
         (progn
          (workspace-resource-tests--write-text first "first")
          (workspace-resource-tests--write-text second "second")
          (let* ((requests
                  (list
                   (workspace-change-tests--request context
                    "workspace:first.txt" "changed-first")
                   (workspace-change-tests--request context
                    "workspace:second.txt" "changed-second")))
                 (publish *workspace-file-resource-publish-function*)
                 (failure
                  (let ((*workspace-file-resource-publish-function*
                         (lambda (temporary target)
                           (if (equal target second)
                               (progn
                                (workspace-resource-tests--write-text first
                                 "external")
                                (error 'cl-resources:resource-protocol-error
                                       :uri "workspace:second.txt" :reason
                                       "injected later failure"))
                               (funcall publish temporary target)))))
                    (workspace-change-tests--failure
                     (lambda ()
                       (workspace-change-set-apply context requests))))))
            (test-assert (typep failure 'cl-resources:change-set-error)
             "expose recoverable undo state")
            (test-assert
             (string= "external" (uiop/stream:read-file-string first))
             "do not overwrite later external content")
            (test-assert
             (string= "second" (uiop/stream:read-file-string second))
             "leave the failing member untouched")
            (let ((set (cl-resources:change-set-error-set failure)))
              (test-assert
               (eq ':recovery-required (cl-resources:change-set-state set))
               "retain failed compensation")
              (workspace-resource-tests--write-text first "first")
              (cl-resources:change-set-recover set)
              (test-assert (eq ':aborted (cl-resources:change-set-state set))
               "complete explicit recovery")
              (test-assert
               (string= "first" (uiop/stream:read-file-string first))
               "restore the requested baseline")
              (cl-resources:change-set-recover set))))
       (tool-registry-close-runtime-state registry))))
  nil)


(-> test-workspace-change-partial-deletion () null)
(defun test-workspace-change-partial-deletion ()
  "Compensate publication that removes its destination before signalling."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (make-default-tool-registry :configuration configuration))
           (context (workspace-change-tests--context configuration registry))
           (path (merge-pathnames "original.txt" root)))
      (unwind-protect
           (progn
             (workspace-resource-tests--write-text path "original")
             (let ((requests (list (workspace-change-tests--request
                                    context "workspace:original.txt" "replacement")))
                   (*workspace-file-resource-publish-function*
                     (lambda (temporary target)
                       (declare (ignore temporary))
                       (platform-delete-file *platform* target)
                       (error 'cl-resources:resource-protocol-error
                              :uri "workspace:original.txt" :reason "partial deletion"))))
               (test-assert (workspace-change-tests--failure
                             (lambda () (workspace-change-set-apply context requests)))
                            "propagate the partial publisher failure")
               (test-assert (string= "original" (uiop:read-file-string path))
                            "restore a destination removed by a failed publisher")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-workspace-change-same-content-permissions () null)
(defun test-workspace-change-same-content-permissions ()
  "Restore destination permissions even when replacement content equals its baseline."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (make-default-tool-registry :configuration configuration))
           (context (workspace-change-tests--context configuration registry))
           (target (merge-pathnames "target.txt" root))
           (donor (merge-pathnames "donor.txt" root))
           (later (merge-pathnames "later.txt" root)))
      (unwind-protect
           (progn
             (workspace-resource-tests--write-text target "same")
             (workspace-resource-tests--write-text donor "same")
             (workspace-resource-tests--write-text later "later")
             (platform-make-read-only *platform* target)
             (platform-make-private *platform* donor)
             (let* ((original-permissions (platform-file-permissions *platform* target))
                    (donor-resource (resource-registry-resolve
                                     (tool-registry-resource-registry registry)
                                     "workspace:donor.txt" context))
                    (requests (list (workspace-change-tests--request
                                     context "workspace:target.txt" "same"
                                     :permission-donor (resource-observe donor-resource context))
                                    (workspace-change-tests--request
                                     context "workspace:later.txt" "replacement")))
                    (publish *workspace-file-resource-publish-function*)
                    (*workspace-file-resource-publish-function*
                      (lambda (temporary path)
                        (if (equal path later)
                            (error 'cl-resources:resource-protocol-error
                                   :uri "workspace:later.txt" :reason "later failure")
                            (funcall publish temporary path)))))
               (test-assert (workspace-change-tests--failure
                             (lambda () (workspace-change-set-apply context requests)))
                            "exercise failure after a same-content move")
               (test-assert (equal original-permissions (platform-file-permissions *platform* target))
                            "restore the original destination permissions")
               (test-assert (string= "same" (uiop:read-file-string target))
                            "restore the original destination content")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-workspace-change-linked-undo () null)
(defun test-workspace-change-linked-undo ()
  "Retain and release undo source ownership across interrupted new-file restoration."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (make-default-tool-registry :configuration configuration))
           (context (workspace-change-tests--context configuration registry))
           (target (merge-pathnames "target.txt" root))
           (later (merge-pathnames "later.txt" root))
           (restore-source nil))
      (unwind-protect
           (progn
             (workspace-resource-tests--write-text target "original")
             (workspace-resource-tests--write-text later "later")
             (platform-make-read-only *platform* target)
             (let* ((original-permissions (platform-file-permissions *platform* target))
                    (requests (list (workspace-change-tests--request context "workspace:target.txt" nil)
                                    (workspace-change-tests--request context "workspace:later.txt" "replacement")))
                    (failure
                      (let ((*workspace-file-resource-publish-function*
                              (lambda (temporary path)
                                (declare (ignore temporary path))
                                (error 'cl-resources:resource-protocol-error
                                       :uri "workspace:later.txt" :reason "later failure")))
                            (*workspace-file-resource-create-function*
                              (lambda (source path)
                                (setf restore-source source)
                                (platform-publish-new-file *platform* source path)
                                (error 'cl-resources:resource-protocol-error
                                       :uri "workspace:target.txt" :reason "interrupted source release"))))
                        (workspace-change-tests--failure
                         (lambda () (workspace-change-set-apply context requests))))))
               (test-assert (typep failure 'cl-resources:change-set-error)
                            "retain interrupted restoration for explicit recovery")
               (test-assert (string= "original" (uiop:read-file-string target))
                            "observe the partially restored original content")
               (cl-resources:change-set-recover (cl-resources:change-set-error-set failure))
               (test-assert (not (probe-file restore-source)) "release every owned restore alias")
               (test-assert (equal original-permissions (platform-file-permissions *platform* target))
                            "preserve restored permissions when removing an undo alias")))
        (tool-registry-close-runtime-state registry))))
  nil)
