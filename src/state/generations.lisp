(in-package #:autolith)

;;;; -- Generation Store --

(-> generation-root (configuration) pathname)
(defun generation-root (configuration)
  "Return CONFIGURATION's retained-generation directory."
  (merge-pathnames "generations/" (configuration-data-root configuration)))

(-> generation-current-pathname (configuration) pathname)
(defun generation-current-pathname (configuration)
  "Return CONFIGURATION's selected-generation record pathname."
  (merge-pathnames "current-generation.sexp"
                   (configuration-state-root configuration)))

(-> generation--validate-manifest (list pathname) null)
(defun generation--validate-manifest (properties pathname)
  "Require PROPERTIES to carry the Autolith fields its manifest version promises."
  (let* ((version (getf properties :version))
         (directory (uiop:pathname-directory-pathname pathname))
         (reconstruction (getf properties :reconstruction))
         (reconstruction-pathname
           (and (non-empty-string-p reconstruction) (pathname reconstruction)))
         (image-commit (getf properties :image-commit))
         (history-commit (getf properties :mutation-history-commit)))
    (unless (non-empty-string-p (getf properties :git-commit))
      (error 'checkpoint-error
             :message "A generation manifest has no source revision."
             :stage ':manifest
             :pathname pathname))
    (when (member version '(2 3))
      (unless (and reconstruction-pathname
                   (uiop:subpathp reconstruction-pathname directory)
                   (probe-file reconstruction-pathname)
                   (or (null image-commit)
                       (image-commit--identifier-p image-commit)))
        (error 'checkpoint-error
               :message "A generation reconstruction manifest is invalid."
               :stage ':manifest
               :pathname pathname)))
    (when (eql version 3)
      (unless (if image-commit
                  (image-history--commit-p history-commit)
                  (null history-commit))
        (error 'checkpoint-error
               :message "A generation mutation-history identity is invalid."
               :stage ':manifest
               :pathname pathname))))
  nil)

(-> generation--validate-publication (t) null)
(defun generation--validate-publication (generation)
  "Require GENERATION to own the reconstruction script a replay depends on."
  (let ((reconstruction (generation-reconstruction-pathname generation)))
    (unless (and reconstruction
                 (uiop:subpathp reconstruction (generation-directory generation))
                 (probe-file reconstruction))
      (error 'checkpoint-error
             :message "The checkpoint has no valid reconstruction script."
             :stage ':publish
             :pathname reconstruction)))
  nil)

(-> generation-store-for (configuration) generation-store)
(defun generation-store-for (configuration)
  "Return the retained-generation store CONFIGURATION publishes into.

Version 3 manifests are written. Versions 1 and 2 remain readable so that a
generation saved before the reconstruction and mutation-history fields existed
can still be listed and booted."
  (make-generation-store
   :root (generation-root configuration)
   :current-pathname (generation-current-pathname configuration)
   :core-name "autolith.core"
   :temporary-core-name ".autolith.core.tmp"
   :manifest-version 3
   :accepted-manifest-versions '(1 2 3)
   :manifest-validator #'generation--validate-manifest
   :publish-validator #'generation--validate-publication
   ;; Autolith already owns an atomic state layer, so the store uses it rather
   ;; than the library's plain-Lisp default: that keeps the private file mode and
   ;; the crash-tolerant reader that every other Autolith state file relies on.
   :write-function #'snapshot-write
   :read-function #'read-portable-form))


;;;; -- Autolith Generation Metadata --

(-> generation-git-commit (t) t)
(defun generation-git-commit (generation)
  "Return the clean source revision GENERATION represents."
  (getf (generation-metadata generation) :git-commit))

(-> generation-image-commit-identifier (t) (option string))
(defun generation-image-commit-identifier (generation)
  "Return the private image commit GENERATION captured, or NIL for a base image."
  (getf (generation-metadata generation) :image-commit))

(-> generation-mutation-history-commit (t) (option string))
(defun generation-mutation-history-commit (generation)
  "Return the private Git commit retaining GENERATION's captured image state."
  (getf (generation-metadata generation) :mutation-history-commit))

(-> generation-journal-position (t) integer)
(defun generation-journal-position (generation)
  "Return the mutation journal position captured before GENERATION forked."
  (or (getf (generation-metadata generation) :journal-position) 0))

(-> generation-reconstruction-pathname (t) (option pathname))
(defun generation-reconstruction-pathname (generation)
  "Return GENERATION's base-image Lisp reconstruction script, if it has one."
  (let ((value (getf (generation-metadata generation) :reconstruction)))
    (and (non-empty-string-p value) (pathname value))))

(-> generation--metadata (configuration string &key (:git-commit (option string))
                                                    (:mutation-checker
                                                     (option mutation-checker)))
    list)
(defun generation--metadata
    (configuration identifier &key git-commit mutation-checker)
  "Return the Autolith manifest properties for a new generation IDENTIFIER.

Preparing the image commit and writing its replay script happen here, before any
quiescing, because both touch the private history repository."
  (let* ((directory (merge-pathnames (format nil "~A/" identifier)
                                     (generation-root configuration)))
         (reconstruction-pathname (merge-pathnames "reconstruct.lisp" directory))
         (image-commit
           (image-commit-prepare-checkpoint
            configuration
            identifier
            :checker (or mutation-checker
                         (make-instance 'standard-mutation-checker)))))
    (image-commit-write-generation-script
     reconstruction-pathname
     :generation-identifier identifier
     :commit image-commit)
    (list :reconstruction (namestring reconstruction-pathname)
          :image-commit (and image-commit (image-commit-identifier image-commit))
          :mutation-history-commit
          (and image-commit (image-commit-history-commit image-commit))
          :git-commit (or git-commit
                          (string-trim
                           '(#\Space #\Tab #\Newline #\Return)
                           (self-git-command configuration '("rev-parse" "HEAD"))))
          :journal-position
          (let ((journal (configuration-journal-path configuration)))
            (if (probe-file journal)
                (with-open-file (stream journal
                                        :direction ':input
                                        :element-type '(unsigned-byte 8))
                  (file-length stream))
                0)))))


;;;; -- Retained Generations --

(-> generation-create-record
    (configuration &key (:git-commit (option string))
                        (:mutation-checker (option mutation-checker)))
    generation)
(defun generation-create-record (configuration &key git-commit mutation-checker)
  "Create a pending generation record with immutable artifact paths."
  (let* ((identifier (make-identifier))
         (metadata (generation--metadata configuration identifier
                                         :git-commit git-commit
                                         :mutation-checker mutation-checker))
         (directory (merge-pathnames (format nil "~A/" identifier)
                                     (generation-root configuration))))
    (make-instance 'generation
                   :identifier identifier
                   :directory directory
                   :core-pathname (merge-pathnames "autolith.core" directory)
                   :temporary-core-pathname
                   (merge-pathnames ".autolith.core.tmp" directory)
                   :manifest-pathname (merge-pathnames "manifest.sexp" directory)
                   :metadata metadata
                   :created-at (get-universal-time)
                   :status ':pending)))

(-> generation--write-form-atomically (pathname list) pathname)
(defun generation--write-form-atomically (pathname form)
  "Atomically publish portable FORM at PATHNAME."
  (snapshot-write pathname form)
  pathname)

(-> generation--translate (t) t)
(defun generation--translate (condition)
  "Signal CONDITION as the Autolith error reporting it.

A host check that already signaled an Autolith error reaches here wrapped as
the library's cause. Re-signal that original instead of the wrapper, so its
structured type and message survive the round trip."
  (let ((cause (sbcl-generations:checkpoint-error-cause condition)))
    (if (typep cause 'autolith-error)
        (error cause)
        (error 'checkpoint-error
               :message (sbcl-generations::checkpoint-error-message condition)
               :stage (sbcl-generations:checkpoint-error-stage condition)
               :pathname (sbcl-generations:checkpoint-error-pathname condition)
               :cause cause))))

(defmacro with-generation-errors (&body body)
  "Run BODY, reporting a library checkpoint failure as an Autolith one."
  `(handler-case
       (progn ,@body)
     (sbcl-generations:checkpoint-error (condition)
       (generation--translate condition))))

(-> generation-core-probe-runner-create () generation-core-probe-runner)
(defun generation-core-probe-runner-create ()
  "Create the subprocess runner that boots unpublished generation cores."
  (make-sbcl-core-probe-runner
   :command (let ((configured (uiop:getenv "AUTOLITH_SBCL")))
              (if (non-empty-string-p configured)
                  configured
                  "sbcl"))))

(-> generation-load-manifest ((or pathname string) configuration) generation)
(defun generation-load-manifest (pathname configuration)
  "Load and validate one ready generation manifest from PATHNAME."
  (with-generation-errors
    (sbcl-generations:generation-load-manifest
     pathname
     (generation-store-for configuration))))

(-> generation-list (configuration) list)
(defun generation-list (configuration)
  "Return valid retained generations newest first."
  (sbcl-generations:generation-list (generation-store-for configuration)))

(-> generation-find (configuration string) (option generation))
(defun generation-find (configuration identifier)
  "Return retained generation IDENTIFIER, or NIL when it is unknown."
  (sbcl-generations:generation-find (generation-store-for configuration)
                                    identifier))

(-> generation-select (configuration generation) generation)
(defun generation-select (configuration generation)
  "Select compatible GENERATION for the next recovery startup."
  (with-generation-errors
    (sbcl-generations:generation-select (generation-store-for configuration)
                                       generation)))

(-> generation-selected (configuration) (option generation))
(defun generation-selected (configuration)
  "Return CONFIGURATION's selected retained generation, if it remains valid."
  (sbcl-generations:generation-selected (generation-store-for configuration)))

(-> generation-publish
    (configuration generation
     &key (:probe-runner generation-core-probe-runner))
    generation)
(defun generation-publish
    (configuration generation
     &key (probe-runner (generation-core-probe-runner-create)))
  "Validate and publish GENERATION's temporary core, manifest, and selection."
  (with-generation-errors
    (sbcl-generations:generation-publish (generation-store-for configuration)
                                        generation
                                        :probe-runner probe-runner)))

(-> generation-request-rollback (configuration string) null)
(defun generation-request-rollback (configuration identifier)
  "Select retained generation IDENTIFIER and request an immediate rollback."
  (handler-case
      (sbcl-generations:generation-request-rollback
       (generation-store-for configuration)
       identifier)
    (sbcl-generations:rollback-requested (condition)
      (error 'rollback-requested
             :message
             (format nil "Rollback requested for retained generation ~A."
                     (sbcl-generations:rollback-requested-generation-identifier
                      condition))
             :generation-id
             (sbcl-generations:rollback-requested-generation-identifier
              condition)))
    (sbcl-generations:checkpoint-error (condition)
      (generation--translate condition))))

(-> generation-render-list (configuration) string)
(defun generation-render-list (configuration)
  "Return a concise model-visible list of retained generations."
  (let ((generations (generation-list configuration)))
    (if generations
        (with-output-to-string (stream)
          (dolist (generation generations)
            (format stream
                    "~A  ~A  source ~A~%  image ~A~%  history ~A~%  replay ~A~%"
                    (generation-identifier generation)
                    (if (generation-compatible-p generation)
                        "compatible"
                        "incompatible")
                    (generation-git-commit generation)
                    (or (generation-image-commit-identifier generation)
                        "base")
                    (or (generation-mutation-history-commit generation)
                        "unavailable")
                    (if (generation-reconstruction-pathname generation)
                        (namestring
                         (generation-reconstruction-pathname generation))
                        "unavailable for legacy generation"))))
        "No retained generations exist.")))


;;;; -- Checkpoint Backend --

(defvar *checkpoint-thread-quiescer* nil
  "A dynamic callback running checkpoint work without ephemeral application threads.")

(-> checkpoint-detach-state (t) t)
(defgeneric checkpoint-detach-state (state)
  (:documentation "Detach ephemeral resources from globally rooted checkpoint STATE."))

(defmethod checkpoint-detach-state ((state t))
  "Leave unrecognized checkpoint STATE unchanged."
  state)

(-> checkpoint-resume-main (list) null)
(defun checkpoint-resume-main (arguments)
  "Run Autolith's normal entry point inside a booted retained core."
  (main arguments)
  nil)

(-> checkpoint--source-snapshot (configuration) string)
(defun checkpoint--source-snapshot (configuration)
  "Return the clean checked commit from CONFIGURATION's source tree."
  (labels ((clean-commit ()
             "Return HEAD when the source is clean, otherwise signal a checkpoint error."
             (let ((status (self-git-command
                            configuration
                            '("status" "--porcelain"))))
               (when (non-empty-string-p status)
                 (error 'checkpoint-error
                        :message "A checkpoint requires a clean source revision."
                        :stage ':validation
                        :pathname (configuration-source-root configuration))))
             (string-trim
              '(#\Space #\Tab #\Newline #\Return)
              (self-git-command configuration '("rev-parse" "HEAD")))))
    (let ((before (clean-commit)))
      (handler-case
          (uiop:run-program
           (list (namestring
                  (merge-pathnames "script/check"
                                   (configuration-source-root configuration))))
           :directory (configuration-source-root configuration)
           :output ':string
           :error-output ':output)
        (error (condition)
          (error 'checkpoint-error
                 :message (format nil "The repository check failed: ~A" condition)
                 :stage ':validation
                 :pathname (configuration-source-root configuration))))
      (let ((after (clean-commit)))
        (unless (string= before after)
          (error 'checkpoint-error
                 :message "The source revision changed during checkpoint validation."
                 :stage ':validation
                 :pathname (configuration-source-root configuration)))
        after))))

(-> checkpoint--revalidate-source (configuration string) null)
(defun checkpoint--revalidate-source (configuration expected-commit)
  "Require CONFIGURATION to remain clean at EXPECTED-COMMIT immediately before fork."
  (let ((status (self-git-command configuration '("status" "--porcelain")))
        (commit (string-trim
                 '(#\Space #\Tab #\Newline #\Return)
                 (self-git-command configuration '("rev-parse" "HEAD")))))
    (unless (and (not (non-empty-string-p status))
                 (string= commit expected-commit))
      (error 'checkpoint-error
             :message "The source changed after checkpoint validation."
             :stage ':validation
             :pathname (configuration-source-root configuration))))
  nil)

(-> checkpoint--process-identifier () integer)
(defun checkpoint--process-identifier ()
  "Return the current process identifier used to recognize a checkpoint child."
  (sb-posix:getpid))

(-> checkpoint--detach-worker (t) null)
(defun checkpoint--detach-worker (worker)
  "Detach SAVER's inherited worker streams without signaling the live subprocess."
  (sbcl-worker-manager-detach-inherited-processes worker)
  nil)

(-> checkpoint--call-with-fork-guard (t function) t)
(defun checkpoint--call-with-fork-guard (worker thunk)
  "Call THUNK exclusively and detach inherited WORKER state in its forked child."
  (let ((parent-pid (checkpoint--process-identifier)))
    (call-with-secret-use-quiescence
     (lambda ()
       (with-live-mutation
         (multiple-value-prog1 (funcall thunk)
           (unless (= parent-pid (checkpoint--process-identifier))
             (checkpoint--detach-worker worker))))))))

(-> checkpoint--prepare-saver (t t) null)
(defun checkpoint--prepare-saver (worker generation)
  "Clear secrets and detach inherited resources inside the saver child.

Everything this process still holds is written into the core, so credentials are
dropped and inherited worker descriptors detached before the image is saved."
  (declare (ignore generation))
  (setf *credentials-in-request-scope* nil
        *active-secret-use-count* 0
        *secret-use-depth* 0
        *secret-use-quiescence-owner* nil)
  (observability-prepare-checkpoint)
  (checkpoint--detach-worker worker)
  (when (boundp '*active-application*)
    (checkpoint-detach-state (symbol-value '*active-application*)))
  nil)

(-> checkpoint-backend-create
    (configuration t &key (:tool-registry (option tool-registry)))
    checkpoint-backend)
(defun checkpoint-backend-create (configuration worker &key tool-registry)
  "Return the checkpoint backend supported by this runtime."
  (let ((quiesced-runtime-tools nil))
    (with-generation-errors
      (make-checkpoint-backend
       :store (generation-store-for configuration)
       :toplevel-function #'checkpoint-resume-main
       :identifier-function #'make-identifier
       :probe-runner (generation-core-probe-runner-create)
       :around-function
       (lambda (thunk)
         (when *credentials-in-request-scope*
           (error 'checkpoint-error
                  :message "A checkpoint cannot run inside a credential request scope."
                  :stage ':validation
                  :pathname nil))
         (if *checkpoint-thread-quiescer*
             (let ((quiescer *checkpoint-thread-quiescer*))
               (funcall quiescer
                        (lambda ()
                          (let ((*checkpoint-thread-quiescer* nil))
                            (funcall thunk)))))
             (funcall thunk)))
       :precheck-function
       (lambda ()
         (checkpoint--source-snapshot configuration))
       :fork-guard-function
       (lambda (thunk)
         (checkpoint--call-with-fork-guard worker thunk))
       :validate-function
       ;; The quiesced runtimes are recorded in the enclosing scope rather than
       ;; returned, because a close failure leaves this hook through a non-local
       ;; exit and the runtimes that did close still have to be resumed.
       (lambda (source-commit)
         (checkpoint--revalidate-source configuration source-commit)
         (when tool-registry
           (multiple-value-bind (quiesced-tools close-failure)
               (tool-registry-quiesce-runtime-state tool-registry)
             (setf quiesced-runtime-tools quiesced-tools)
             (when close-failure
               (error close-failure))))
         (when (secret-use-active-p)
           (error 'checkpoint-error
                  :message
                  "A provider or tool runtime retained a secret while checkpointing."
                  :stage ':fork
                  :pathname nil))
         quiesced-runtime-tools)
       :metadata-function
       (lambda (identifier source-commit)
         (generation--metadata configuration identifier
                               :git-commit source-commit))
       :prepare-function
       (lambda (generation)
         (checkpoint--prepare-saver worker generation))
       :resume-function
       (lambda (state)
         (declare (ignore state))
         (when (and tool-registry quiesced-runtime-tools)
           (tool-registry-resume-runtime-state
            tool-registry :tools quiesced-runtime-tools)))))))

(-> checkpoint-create (checkpoint-backend) generation)
(defun checkpoint-create (backend)
  "Begin a validated checkpoint through BACKEND and return its pending generation."
  (handler-bind
      ((sbcl-generations:checkpoint-resume-warning
         (lambda (condition)
           (muffle-warning
            (progn
              (warn 'checkpoint-runtime-resume-warning
                    :generation-id
                    (sbcl-generations:checkpoint-resume-warning-generation-identifier
                     condition)
                    :cause
                    (sbcl-generations:checkpoint-resume-warning-cause condition))
              condition)))))
    (with-generation-errors
      (sbcl-generations:checkpoint-create backend))))


;;;; -- Generation Tools --

(defmethod tool-execute ((tool self-checkpoint-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Begin a validated non-stopping checkpoint of CONTEXT's active image."
  (declare (ignore tool arguments))
  (let ((generation
          (checkpoint-create
           (checkpoint-backend-create
            (tool-context-configuration context)
            (tool-context-worker context)
            :tool-registry (tool-context-registry context)))))
    (observability-mark
     :checkpoint-created
     :generation generation)
    (tool-success
     (format nil "Checkpoint ~A is being published by coordinator process ~D."
             (generation-identifier generation)
             (generation-coordinator-pid generation)))))

(defmethod tool-execute ((tool self-generations-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List retained generations visible to CONTEXT."
  (declare (ignore tool arguments))
  (tool-success
   (generation-render-list (tool-context-configuration context))))

(defmethod tool-execute ((tool self-rollback-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Select a compatible retained generation and request an immediate rollback."
  (declare (ignore tool))
  (generation-request-rollback
   (tool-context-configuration context)
   (tool-argument arguments "generation" :required t)))
