(in-package #:autolith)

;;;; -- Probe Test Boundary --

(defclass test-generation-core-probe-runner (generation-core-probe-runner)
  ((output
    :initarg       :output
    :reader        test-generation-core-probe-runner-output
    :type          string
    :documentation "The exact probe output returned without starting an SBCL core."))
  (:documentation "A deterministic generation-core probe runner for publication tests."))

(defmethod generation-core-probe-run
    ((runner test-generation-core-probe-runner) (generation generation))
  "Return RUNNER's configured probe output for GENERATION."
  (declare (ignore generation))
  (test-generation-core-probe-runner-output runner))

(-> test-generation-replay-target () integer)
(defun test-generation-replay-target ()
  "Return the baseline value used by generation reconstruction tests."
  0)

(-> generation-tests--generation (configuration string string) generation)
(defun generation-tests--generation (configuration identifier git-commit)
  "Return a pending test generation named IDENTIFIER at GIT-COMMIT."
  (let ((directory (merge-pathnames (format nil "~A/" identifier)
                                    (generation-root configuration))))
    (make-instance 'generation
                   :identifier    identifier
                   :directory     directory
                   :core-pathname (merge-pathnames "autolith.core" directory)
                   :temporary-core-pathname
                   (merge-pathnames ".autolith.core.tmp" directory)
                   :manifest-pathname
                   (merge-pathnames "manifest.sexp" directory)
                   :metadata
                   (list :reconstruction
                         (namestring (merge-pathnames "reconstruct.lisp" directory))
                         :git-commit       git-commit
                         :journal-position 27)
                   :created-at    4000000000
                   :status        ':pending)))

(-> generation-tests--write-fake-core (generation) pathname)
(defun generation-tests--write-fake-core (generation)
  "Write one deliberately non-bootable byte to GENERATION's temporary core."
  (ensure-directories-exist (generation-temporary-core-pathname generation))
  (with-open-file (stream (generation-temporary-core-pathname generation)
                          :direction         ':output
                          :if-exists         ':supersede
                          :if-does-not-exist ':create
                          :element-type      '(unsigned-byte 8))
    (write-byte 42 stream))
  (generation-temporary-core-pathname generation))

(-> generation-tests--write-reconstruction (generation) pathname)
(defun generation-tests--write-reconstruction (generation)
  "Write GENERATION's deterministic test reconstruction script."
  (image-commit-write-script
   (generation-reconstruction-pathname generation)
   :identifier (generation-identifier generation)
   :title      "Test generation reconstruction"
   :entries    nil))

(-> generation-tests--make-core-plausible (generation) pathname)
(defun generation-tests--make-core-plausible (generation)
  "Expand GENERATION's published core past the static compatibility threshold."
  (with-open-file (stream (generation-core-pathname generation)
                          :direction         ':output
                          :if-exists         ':overwrite
                          :if-does-not-exist ':error
                          :element-type      '(unsigned-byte 8))
    (file-position stream 1048576)
    (write-byte 42 stream))
  (generation-core-pathname generation))

(-> test-checkpoint-source-check-diagnostics () null)
(defun test-checkpoint-source-check-diagnostics ()
  "Verify successful source checks and bounded diagnostics for validation failures."
  (with-test-configuration (configuration)
    (dolist (status '(0 7 :launch-failure))
      (test-call-with-function-replacements
       (list
         (list 'source-clean-p
               (lambda (source &key paths)
                 (declare (ignore source paths))
                 t))
         (list 'source-revision
               (lambda (source)
                 (declare (ignore source))
                 "fixture-commit"))
        (list 'uiop:run-program
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (when (eq status ':launch-failure)
                  (error "Synthetic checker launch failure."))
                (values (concatenate 'string "Early checker output."
                                     (make-string 20000 :initial-element #\.)
                                     "Important checker failure.")
                        nil status))))
       (lambda ()
         (handler-case
             (let ((commit (checkpoint--source-snapshot configuration)))
               (test-assert
                (and (eql status 0) (string= commit "fixture-commit"))
                "a successful checker returns the revalidated source commit"))
           (checkpoint-error (condition)
             (let ((message (princ-to-string condition)))
               (test-assert
                (and (not (eql status 0))
                     (eq (checkpoint-error-stage condition) ':validation)
                     (equal (checkpoint-error-pathname condition)
                            (config :source-root configuration)))
                "checker failures carry the validation stage and source path")
               (if (eq status ':launch-failure)
                   (test-assert (search "Synthetic checker launch failure." message)
                                "checker launch failures retain their cause")
                   (test-assert
                    (and (search "Important checker failure." message)
                         (not (search "Early checker output." message))
                         (< (length message) 17000))
                    "a failed check reports the bounded tail containing its diagnostics")))))))))
  nil)

(-> test-checkpoint-source-precheck-order () null)
(defun test-checkpoint-source-precheck-order ()
  "Test source validation precedes quiescence and fork preparation uses it."
  (with-platform-capability (':forked-image-saver "checkpoint source validation")
    (with-test-configuration (configuration)
      (let ((quiesced-p          nil)
            (prechecked-p        nil)
            (precheck-quiesced-p nil)
            (validate-quiesced-p nil)
            (fork-quiesced-p     nil)
            (quiesce-count       0))
        (let ((*checkpoint-thread-quiescer*
                (lambda (thunk)
                  (incf quiesce-count)
                  (setf quiesced-p t)
                  (unwind-protect (funcall thunk)
                    (setf quiesced-p nil)))))
          (test-call-with-function-replacements
           (list
            (list 'checkpoint--source-snapshot
                  (lambda (active-configuration)
                    (declare (ignore active-configuration))
                    (setf prechecked-p t
                          precheck-quiesced-p quiesced-p)
                    "test-source-commit"))
            (list 'checkpoint--revalidate-source
                  (lambda (active-configuration source-commit)
                    (declare (ignore active-configuration source-commit))
                    (setf validate-quiesced-p quiesced-p)))
            (list 'checkpoint-single-threaded-p (lambda () t))
            (list 'checkpoint--call-with-fork-guard
                  (lambda (worker thunk)
                    (declare (ignore worker))
                    (funcall thunk)))
            (list 'sbcl-generations::checkpoint--fork
                  (lambda (generation)
                    (declare (ignore generation))
                    (setf fork-quiesced-p quiesced-p)
                    (values nil nil)))
            (list 'generation--metadata
                  (lambda (active-configuration identifier &key git-commit mutation-checker)
                    (declare (ignore active-configuration identifier git-commit mutation-checker))
                    nil)))
           (lambda ()
             (test-assert
              (handler-case (checkpoint-create (checkpoint-backend-create configuration nil))
                (checkpoint-error () t))
              "the synthetic missing coordinator reports checkpoint failure"))))
        (test-assert (and prechecked-p (not precheck-quiesced-p))
                     "source validation runs before session quiescence")
        (test-assert (and validate-quiesced-p fork-quiesced-p)
                     "source revalidation and fork run inside session quiescence")
        (test-assert (and (= quiesce-count 1) (not quiesced-p))
                     "the parent restores its context after failed fork preparation"))))
  nil)

(-> generation-tests--test-checkpoint-runtime-resume () null)
(defun generation-tests--test-checkpoint-runtime-resume ()
  "Test failed checkpoint preparation resumes every quiesced tool runtime."
  (let* ((configuration    (test-configuration))
         (root             (test-configuration-root configuration))
         (registry         (make-instance 'tool-registry))
         (runtime-identity (list ':checkpoint-runtime))
         (close-count      0)
         (resume-count     0)
         (secret-lock      (make-lock "Autolith checkpoint secret drain"))
         (secret-condition
           (make-condition-variable
            :name "Autolith checkpoint secret drain"))
         (secret-ready-p   nil)
         (release-secret-p nil)
         (secret-thread    nil)
         (tool
           (make-instance
            'tool-test-runtime-tool
            :namespace        "checkpoint-test"
            :name             "runtime"
            :description      "Exercise checkpoint runtime recovery."
            :parameters       (tool-object-schema (json-object) nil)
            :runtime-identity runtime-identity
            :close-function
            (lambda ()
              (incf close-count)
              (with-lock-held (secret-lock)
                (setf release-secret-p t)
                (condition-notify secret-condition))
              (join-thread secret-thread))
            :resume-function  (lambda () (incf resume-count))
            :detach-function  (lambda () nil)))
         (backend
           (checkpoint-backend-create configuration nil
                                      :tool-registry registry)))
    (unwind-protect
         (progn
           (tool-registry-register registry tool)
           (setf
            secret-thread
            (make-thread
             (lambda ()
               (call-with-secret-use
                (lambda ()
                  (with-lock-held (secret-lock)
                    (setf secret-ready-p t)
                    (condition-notify secret-condition)
                    (loop until release-secret-p
                          do (condition-wait
                              secret-condition secret-lock))))))
             :name "Autolith checkpoint active secret test"))
           (with-lock-held (secret-lock)
             (loop until secret-ready-p
                   do (condition-wait secret-condition secret-lock)))
           (let ((*checkpoint-thread-quiescer* nil))
             (test-assert
              (test-call-with-function-replacements
               (list
                (list
                 'checkpoint--source-snapshot
                 (lambda (active-configuration)
                   (declare (ignore active-configuration))
                   "test-source-commit"))
                (list
                 'checkpoint--revalidate-source
                 (lambda (active-configuration source-commit)
                   (declare
                    (ignore active-configuration source-commit))
                   nil))
                (list
                 'checkpoint-single-threaded-p
                 (lambda ()
                   nil)))
               (lambda ()
                 (handler-case
                     (progn
                       (checkpoint-create backend)
                       nil)
                   (checkpoint-error (condition)
                     (eq (checkpoint-error-stage condition) ':fork)))))
              "post-quiesce checkpoint validation failure remains structured"))
           (test-assert
            (and (= close-count 1)
                 (= resume-count 1))
            "failed checkpoint preparation resumes its quiesced runtime once")
           (test-assert
            (not (secret-use-active-p))
            "checkpoint preparation drains a runtime's existing secret use"))
      (when (and secret-thread
                 (sb-thread:thread-alive-p secret-thread))
        (with-lock-held (secret-lock)
          (setf release-secret-p t)
          (condition-notify secret-condition))
        (join-thread secret-thread))
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--test-partial-runtime-quiescence () null)
(defun generation-tests--test-partial-runtime-quiescence ()
  "Test checkpoint failure resumes only runtimes whose close completed."
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (registry                (make-instance 'tool-registry))
         (successful-close-count  0)
         (successful-resume-count 0)
         (failed-resume-count     0)
         (close-failure
           (make-condition
            'simple-error
            :format-control   "Synthetic checkpoint close failure."
            :format-arguments nil))
         (backend
           (checkpoint-backend-create configuration nil
                                      :tool-registry registry)))
    (labels
        ((runtime-tool
             (&key name identity priority close-function resume-function)
           "Return one checkpoint runtime test tool."
           (make-instance
            'tool-test-runtime-tool
            :namespace        "checkpoint-partial"
            :name             name
            :description      "Exercise partial checkpoint runtime quiescence."
            :parameters       (tool-object-schema (json-object) nil)
            :runtime-identity identity
            :close-priority   priority
            :close-function   close-function
            :resume-function  resume-function
            :detach-function  (lambda () nil))))
      (unwind-protect
           (progn
             (tool-registry-register
              registry
              (runtime-tool
               :name     "closed"
               :identity (list ':closed)
               :priority 100
               :close-function
               (lambda ()
                 (incf successful-close-count))
               :resume-function
               (lambda ()
                 (incf successful-resume-count))))
             (tool-registry-register
              registry
              (runtime-tool
               :name     "still-live"
               :identity (list ':still-live)
               :priority 50
               :close-function
               (lambda ()
                 (error close-failure))
               :resume-function
               (lambda ()
                 (incf failed-resume-count))))
             (let ((*checkpoint-thread-quiescer* nil))
               (test-assert
                (handler-case
                    (test-call-with-function-replacements
                     (list
                      (list
                       'checkpoint--source-snapshot
                       (lambda (active-configuration)
                         (declare (ignore active-configuration))
                         "test-source-commit"))
                      (list
                       'checkpoint--revalidate-source
                       (lambda (active-configuration source-commit)
                         (declare
                          (ignore active-configuration source-commit))
                         nil)))
                     (lambda ()
                       (checkpoint-create backend)))
                  (checkpoint-error (condition)
                    (eq (checkpoint-error-cause condition)
                        close-failure)))
                "a partial runtime close reports its structured underlying cause"))
             (test-assert
              (and (= successful-close-count 1)
                   (= successful-resume-count 1)
                   (zerop failed-resume-count))
              "checkpoint recovery resumes only the runtime that actually closed"))
        (platform-delete-directory-tree
         *platform*
         root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> generation-tests--test-active-provider-secret-refusal () null)
(defun generation-tests--test-active-provider-secret-refusal ()
  "Test checkpoint preparation refuses an undrained provider secret scope."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (barrier-lock  (make-lock "Autolith checkpoint provider secret"))
         (barrier
           (make-condition-variable
            :name "Autolith checkpoint provider secret"))
         (ready-p       nil)
         (release-p     nil)
         (thread
           (make-thread
            (lambda ()
              (call-with-secret-use
               (lambda ()
                 (with-lock-held (barrier-lock)
                   (setf ready-p t)
                   (condition-notify barrier)
                   (loop until release-p
                         do (condition-wait barrier barrier-lock))))))
            :name "Autolith active provider secret test"))
         (backend
           (checkpoint-backend-create configuration nil
                                      :tool-registry nil)))
    (unwind-protect
         (progn
           (with-lock-held (barrier-lock)
             (loop until ready-p
                   do (condition-wait barrier barrier-lock)))
           (let ((*checkpoint-thread-quiescer* nil))
             (test-assert
              (test-call-with-function-replacements
               (list
                (list
                 'checkpoint--source-snapshot
                 (lambda (active-configuration)
                   (declare (ignore active-configuration))
                   "test-source-commit"))
                (list
                 'checkpoint--revalidate-source
                 (lambda (active-configuration source-commit)
                   (declare
                    (ignore active-configuration source-commit))
                   nil)))
               (lambda ()
                 (handler-case
                     (progn
                       (checkpoint-create backend)
                       nil)
                   (checkpoint-error (condition)
                     (and
                      (eq (checkpoint-error-stage condition) ':fork)
                      (search
                       "retained a secret"
                       (autolith-error-message condition)))))))
              "checkpoint preparation refuses an active provider secret scope")))
      (with-lock-held (barrier-lock)
        (setf release-p t)
        (condition-notify barrier))
      (join-thread thread)
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--unpublished-p (configuration generation) boolean)
(defun generation-tests--unpublished-p (configuration generation)
  "Return true when failed GENERATION left no visible publication artifacts."
  (and (probe-file (generation-temporary-core-pathname generation))
       (not (probe-file (generation-core-pathname generation)))
       (not (probe-file (generation-manifest-pathname generation)))
       (not (probe-file (generation-current-pathname configuration)))
       (eq (generation-status generation) ':pending)))

(-> generation-tests--test-rollback-control-path () null)
(defun generation-tests--test-rollback-control-path ()
  "Test rollback selection and propagation through the tool registry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (generation
           (generation-tests--generation configuration
                                         "rollback-generation"
                                         "0123456789abcdef"))
         (runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record generation)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core generation)
           (generation-tests--write-reconstruction generation)
           (generation-publish configuration generation :probe-runner runner)
           (generation-tests--make-core-plausible generation)
           (delete-file (generation-current-pathname configuration))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "rollback-control-path"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker        nil
                                   :conversation  conversation))
                  (call
                    (json-object
                     "namespace" "self"
                     "name" "rollback"
                     "arguments"
                     (json-encode
                      (json-object "generation" "rollback-generation"))))
                  (condition
                    (handler-case
                        (progn
                          (tool-registry-execute-call
                           (make-default-tool-registry)
                           call
                           context)
                          nil)
                      (rollback-requested (condition)
                        condition))))
             (test-assert condition
                          "self.rollback propagates its control condition")
             (test-assert
              (string= (rollback-requested-generation-id condition)
                       "rollback-generation")
              "the rollback condition carries the selected generation ID")
             (let ((selected (generation-selected configuration)))
               (test-assert selected
                            "rollback selects the generation before signaling")
               (test-assert
                (string= (generation-identifier selected)
                         "rollback-generation")
                "the rollback selection names the requested generation"))
             (let* ((application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation  conversation
                                     :provider      nil
                                     :tool-registry (make-instance 'tool-registry)
                                     :worker        nil
                                     :agent         nil
                                     :ui            nil))
                    (command-condition
                      (handler-case
                          (progn
                            (application-command
                             application
                             "/rollback rollback-generation")
                            nil)
                        (rollback-requested (condition)
                          condition))))
               (test-assert command-condition
                            "/rollback requests process recovery immediately")
               (test-assert
                (string= (rollback-requested-generation-id command-condition)
                         "rollback-generation")
                "/rollback carries the selected generation into recovery"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--test-reconstruction-capture () null)
(defun generation-tests--test-reconstruction-capture ()
  "Test automatic mutation commits and per-generation replay scripts."
  (let* ((configuration                (test-configuration))
         (root                         (test-configuration-root configuration))
         (previous-function            (symbol-function 'test-generation-replay-target))
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier   *active-image-commit-identifier*)
         (previous-history-commit      *active-image-history-commit*)
         (previous-lineage-identifier  *active-image-lineage-identifier*)
         (check-count                  0)
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              (incf check-count)
              "active generation checks passed"))))
    (unwind-protect
         (progn
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (test-assert (null (image-state-load configuration))
                        "generation capture initializes an empty image lineage")
           (self-install-definition
            configuration
            "(defun test-generation-replay-target () \"Return captured state.\" 73)")
           (let* ((generation
                    (generation-create-record
                     configuration
                     :git-commit       "0123456789abcdef"
                     :mutation-checker checker))
                  (commit (image-commit-current configuration))
                  (script
                    (uiop:read-file-string
                     (generation-reconstruction-pathname generation))))
             (test-assert (= check-count 1)
                          "checkpoint capture checks staged live mutations once")
             (test-assert commit
                          "checkpoint capture automatically creates a private commit")
             (test-assert
              (string= (or (generation-image-commit-identifier generation) "")
                       (image-commit-identifier commit))
              "a generation records the exact private image commit")
             (test-assert
              (string=
               (or (generation-mutation-history-commit generation) "")
               (or (image-commit-history-commit commit) ""))
              "a generation records the exact private Git commit")
             (test-assert
              (and (search "Return captured state." script)
                   (uiop:subpathp
                    (generation-reconstruction-pathname generation)
                    (generation-directory generation)))
              "a generation receives a contained full replay script")
             (test-assert
              (test-fixture-permissions-p
               *platform*
               (generation-reconstruction-pathname generation)
               ':read-only)
              "generation replay scripts are published read-only")
             (test-assert
              (null (image-commit-pending-records configuration))
              "checkpoint capture consumes staged reconstructible mutations")))
      (setf (symbol-function 'test-generation-replay-target) previous-function
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (remhash (definition-key '(defun test-generation-replay-target () 0))
               *exploratory-definitions*)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--test-checkpoint-worker-detachment () null)
(defun generation-tests--test-checkpoint-worker-detachment ()
  "Test coordinator and saver processes detach inherited worker state."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (worker              (list ':checkpoint-worker))
         (backend             (checkpoint-backend-create configuration worker))
         (guard
           (sbcl-generations::checkpoint-backend-fork-guard-function backend))
         (process-identifiers nil)
         (events              nil))
    (unwind-protect
         (progn
           (labels ((process-identifier ()
                      "Return the next simulated process identifier."
                      (pop process-identifiers))

                    (detach-worker (detached-worker)
                      "Record inherited worker detachment."
                      (push (list ':detach detached-worker) events)
                      nil))
             (setf process-identifiers '(100 101))
             (test-call-with-function-replacements
              (list (list 'checkpoint--process-identifier #'process-identifier)
                    (list 'checkpoint--detach-worker #'detach-worker))
              (lambda ()
                (test-assert
                 (eq (funcall guard
                              (lambda ()
                                (push ':thunk events)
                                ':complete))
                     ':complete)
                 "the checkpoint fork guard preserves its thunk result")))
             (test-assert
              (equal (reverse events)
                     (list ':thunk (list ':detach worker)))
              "the forked coordinator detaches worker state after the guarded fork")
             (setf process-identifiers '(100 100)
                   events nil)
             (test-call-with-function-replacements
              (list (list 'checkpoint--process-identifier #'process-identifier)
                    (list 'checkpoint--detach-worker #'detach-worker))
              (lambda ()
                (funcall guard (lambda () (push ':thunk events)))))
             (test-assert
              (equal events '(:thunk))
              "the live parent keeps ownership of its worker state"))
           (let ((*credentials-in-request-scope* (list "test credential"))
                 (*active-secret-use-count*      2)
                 (*secret-use-depth*             2)
                 (*secret-use-quiescence-owner*  sb-thread:*current-thread*)
                 (*active-application*           ':saved-application)
                 (saved-events                   nil))
             (test-call-with-function-replacements
              (list
               (list 'checkpoint--detach-worker
                     (lambda (detached-worker)
                       (push (list ':detach detached-worker) saved-events)
                       nil))
               (list 'checkpoint-detach-state
                     (lambda (application)
                       (push (list ':state application) saved-events)
                       nil)))
              (lambda ()
                (checkpoint--prepare-saver worker nil)))
             (test-assert
              (and (null *credentials-in-request-scope*)
                   (zerop *active-secret-use-count*)
                   (zerop *secret-use-depth*)
                   (null *secret-use-quiescence-owner*))
              "the saver clears credential state before writing the core")
             (test-assert
              (equal (reverse saved-events)
                     (list (list ':detach worker)
                           '(:state :saved-application)))
              "the saver detaches worker and application state before saving")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--test-active-image-single-thread-check () null)
(defun generation-tests--test-active-image-single-thread-check ()
  "Test only a forked active-image saver requires one live Lisp thread."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (source-root   (asdf:system-source-directory :autolith))
         (core-pathname (merge-pathnames "active/autolith.core" root)))
    (flet ((install-stage (saver)
             "Install with SAVER on a multi-threaded host; return its failure stage and pathname."
             (test-call-with-function-replacements
              (list
               (list 'checkpoint-single-threaded-p (lambda () nil))
               (list 'active-image--fresh-process-command
                     (lambda (saver-source-root temporary)
                       (declare (ignore saver-source-root temporary))
                       (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                             "--non-interactive" "--no-sysinit" "--no-userinit"
                             "--eval" "(sb-ext:exit :code 1)"))))
              (lambda ()
                (handler-case
                    (progn
                      (active-image-install source-root core-pathname :saver saver)
                      nil)
                  (active-image-build-error (condition)
                    (list (active-image-build-error-stage condition)
                          (active-image-build-error-pathname condition))))))))
      (unwind-protect
           (progn
             (with-platform-capability (':forked-image-saver "forked active-image saver")
               (test-assert (equal (install-stage ':automatic) (list ':fork core-pathname))
                            "active-image installation checks for one live thread before forking"))
             (test-assert (eq (first (install-stage ':fresh-process)) ':save)
                          "a fresh-process active-image saver needs no single-threaded host"))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> generation-tests--test-autolith-error-translation () null)
(defun generation-tests--test-autolith-error-translation ()
  "Test generation translation preserves structured Autolith failures."
  (let* ((cause
           (make-condition 'image-commit-error
                           :message   "Private replay failed."
                           :tool-name "self.commit"
                           :pathname  nil
                           :stage     ':replay-probe))
         (wrapper
           (make-condition 'sbcl-generations:checkpoint-error
                           :message  "Checkpoint preparation failed."
                           :stage    ':backend
                           :pathname nil
                           :cause    cause)))
    (test-assert
     (handler-case
         (progn
           (generation--translate wrapper)
           nil)
       (image-commit-error (condition)
         (eq condition cause)))
     "checkpoint translation preserves a structured Autolith cause"))
  nil)


;;;; -- Subsystem Tests --

(-> generation-tests--test-checkpoint-resume-identity () null)
(defun generation-tests--test-checkpoint-resume-identity ()
  "Verify retained sessions resume directly by their persisted conversation ID."
  (let ((main-arguments nil)
        (style-events   nil))
    (let ((*active-application* t))
      (test-call-with-function-replacements
       (list
        (list 'application-conversation
              (lambda (application)
                (declare (ignore application))
                :conversation))
        (list 'conversation-identifier
              (lambda (conversation)
                (declare (ignore conversation))
                "conversation-identity"))
        (list 'main
              (lambda (arguments)
                (setf main-arguments arguments)
                nil))
        (list 'platform-set-environment-variable
              (lambda (platform name value)
                (declare (ignore platform))
                (when (string= name "AUTOLITH_SESSION_STYLE")
                  (push value style-events)))))
       (lambda ()
         (checkpoint-resume-main '("--localgroup-handoff" "consumed"))))
     (test-assert (equal main-arguments '("resume" "conversation-identity"))
                  "saved checkpoints resume their originating conversation")
     (test-assert (member "direct" style-events :test #'string=)
                  "saved checkpoints bypass client-first startup")
     (test-assert (member nil style-events)
                  "saved checkpoint startup restores the prior session style")))
  nil)

(-> test-checkpoint-backend-selection () null)
(defun test-checkpoint-backend-selection ()
  "Test restart-only checkpoint construction and unsupported-host refusal."
  (with-test-configuration (configuration)
    (let ((supports-p (symbol-function 'platform-supports-p)))
      (dolist (restart-p '(t nil))
        (test-call-with-function-replacements
         (list
          (list 'platform-supports-p
                (lambda (platform capability)
                  (case capability
                    (:restartable-image-saver
                     restart-p)
                    (:forked-image-saver
                     nil)
                    (otherwise
                     (funcall supports-p platform capability))))))
         (lambda ()
           (if restart-p
               (test-assert
                (typep (checkpoint-backend-create configuration nil)
                       'sbcl-generations:restart-checkpoint-backend)
                "a restart-only host constructs the exact-heap checkpoint backend")
               (test-assert
                (handler-case
                    (progn
                      (checkpoint-backend-create configuration nil)
                      nil)
                  (platform-capability-unavailable ()
                    t))
                "a host without a checkpoint capability is refused")))))))
  nil)

(-> test-generation-health () null)
(defun test-generation-health ()
  "Exercise runtime drift, fallback, warning presentation and safe selection."
  (with-test-configuration (configuration)
    (let* ((*image-state-initialized-p*       t)
           (*active-image-lineage-identifier* "generation-health-test")
           (*active-image-commit-identifier*  nil)
           (*active-image-history-commit*     nil)
           (stale                             (generation-tests--generation configuration "stale" "0123456789abcdef"))
           (healthy                           (generation-tests--generation configuration "healthy" "0123456789abcdef"))
           (application                       (make-instance 'application :configuration configuration))
           (root                              (test-configuration-root configuration)))
      (labels ((publish (generation)
                 (generation-tests--write-fake-core generation)
                 (generation-tests--write-reconstruction generation)
                 (generation-publish
                  configuration generation
                  :probe-runner (make-instance
                                 'test-generation-core-probe-runner
                                 :output (generation-core-probe-output
                                          (generation-core-probe-record generation)))))

               (check-warning (expected-p)
                 (let* ((summary   (generation-health-summary configuration))
                        (warning   (generation-health-warning summary))
                        (presented nil))
                   (test-assert (eq expected-p (not (null warning)))
                                "warnings reflect retained and selected checkpoint health")
                   (when warning
                     (test-assert (and (search warning (self-status-render configuration))
                                       (search warning (generation-render-list configuration)))
                                  "status and listing present the shared health warning"))
                   (test-call-with-function-replacements
                    (list (list 'application-present
                                (lambda (application spans &rest options)
                                  (declare (ignore application options))
                                  (setf presented (test-terminal-row-text spans)))))
                    (lambda ()
                      (test-assert
                       (eq expected-p (application-present-generation-health-warning application))
                       "ordinary startup presents a warning exactly when needed")))
                   (test-assert (if warning (search warning presented) (null presented))
                                "startup presents the shared diagnostic"))))
        (unwind-protect
             (progn
               (let ((summary (generation-health-summary configuration)))
                 (test-assert (and (zerop (getf summary :retained))
                                   (zerop (getf summary :compatible)))
                              "an empty store has zero retained and compatible checkpoints"))
               (check-warning nil)
               (publish stale)
               (generation-tests--make-core-plausible stale)
               (publish healthy)
               (generation-select configuration stale)
               (let ((manifest (read-portable-form (generation-manifest-pathname stale))))
                 (setf (getf (rest manifest) :sbcl-version) "2.6.4-test")
                 (snapshot-write (generation-manifest-pathname stale) manifest))
               (let* ((summary (generation-health-summary configuration))
                      (health  (getf summary :selected-health)))
                 (test-assert (and (= 2 (getf summary :retained))
                                   (zerop (getf summary :compatible))
                                   (eq ':sbcl-version (getf health :reason-code))
                                   (search "2.6.4-test" (getf health :reason))
                                   (search (lisp-implementation-version) (getf health :reason)))
                              "runtime drift reports saved and current runtimes without offering rollback"))
               (check-warning t)
               (let ((pointer (uiop:read-file-string (generation-current-pathname configuration))))
                 (test-assert
                  (handler-case (progn (generation-select configuration stale) nil)
                    (checkpoint-error (condition)
                      (eq ':selection (checkpoint-error-stage condition))))
                  "an incompatible checkpoint cannot be selected")
                 (test-assert
                  (string= pointer (uiop:read-file-string (generation-current-pathname configuration)))
                  "a rejected selection leaves the pointer unchanged"))
               (generation-tests--make-core-plausible healthy)
               (let ((summary (generation-health-summary configuration)))
                 (test-assert (and (= 1 (getf summary :compatible))
                                   (string= "healthy"
                                            (generation-identifier (getf summary :fallback))))
                              "a stale selection exposes a compatible fallback"))
               (check-warning t)
               (generation-select configuration healthy)
               (check-warning nil)
               (test-assert (= 1 (getf (generation-health-summary configuration) :compatible))
                            "a healthy selection coexists with an incompatible retained checkpoint")
               (delete-file (generation-core-pathname healthy))
               (test-assert (eq ':core-missing (getf (generation-health healthy) :reason-code))
                            "missing core diagnostics preserve the library cause")
               (check-warning t)
               (delete-file (generation-manifest-pathname healthy))
               (test-assert (eq ':manifest-unreadable (getf (generation-health healthy) :reason-code))
                            "manifest read failures are reported without signaling"))
          (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))))
  nil)

(-> test-generation-manifest () null)
(defun test-generation-manifest ()
  "Test generation publication, loading, selection, and compatibility checks."
  (generation-tests--test-checkpoint-resume-identity)
  (generation-tests--test-checkpoint-restart-finish-boundary)
  (with-platform-capability (':forked-image-saver "checkpoint backend behaviour")
    (generation-tests--test-checkpoint-runtime-resume)
    (generation-tests--test-partial-runtime-quiescence)
    (generation-tests--test-active-provider-secret-refusal)
    (generation-tests--test-checkpoint-worker-detachment))
  (generation-tests--test-active-image-single-thread-check)
  (generation-tests--test-autolith-error-translation)
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (generation
           (generation-tests--generation configuration
                                         "generation-under-test"
                                         "0123456789abcdef"))
         (runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record generation)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core generation)
           (generation-tests--write-reconstruction generation)
           (generation-publish configuration generation :probe-runner runner)
           (let ((loaded (generation-find configuration
                                          "generation-under-test")))
             (test-assert loaded
                          "a published generation appears in retained listings")
             (test-assert (not (generation-compatible-p loaded))
                          "a fake one-byte core is never reported as bootable")
             (test-assert
              (let ((*checkpoint-in-progress-p* t))
                (handler-case
                    (progn
                      (generation-select configuration loaded)
                      nil)
                  (checkpoint-error (condition)
                    (search "while a checkpoint publishes"
                            (autolith-error-message condition)))))
              "rollback selection cannot race asynchronous publication")
             (test-assert (= (generation-journal-position loaded) 27)
                          "generation manifests preserve mutation journal position")
             (test-assert
              (null (generation-mutation-history-commit loaded))
              "base generations explicitly carry no private Git identity")
             (test-assert
              (and (generation-reconstruction-pathname loaded)
                   (probe-file (generation-reconstruction-pathname loaded))
                   (search "Autolith image reconstruction script"
                           (uiop:read-file-string
                            (generation-reconstruction-pathname loaded))))
              "generation manifests retain a complete reconstruction script")
             (test-assert
              (string= (generation-identifier
                        (generation-selected configuration))
                       "generation-under-test")
              "publication atomically selects the ready generation")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (directory     (merge-pathnames "legacy-generation/"
                                         (generation-root configuration)))
         (manifest      (merge-pathnames "manifest.sexp" directory))
         (core          (merge-pathnames "autolith.core" directory)))
    (unwind-protect
         (progn
           (snapshot-write
            manifest
            (list :generation
                  :version 1
                  :id                       "legacy-generation"
                  :core                     (namestring core)
                  :git-commit               "0123456789abcdef"
                  :journal-position         11
                  :sbcl-version             (lisp-implementation-version)
                  :operating-system         (software-type)
                  :operating-system-version (software-version)
                  :architecture             (machine-type)
                  :created-at               3999999999))
           (let ((legacy (generation-load-manifest manifest configuration)))
             (test-assert
              (and (string= (generation-identifier legacy)
                            "legacy-generation")
                   (null (generation-reconstruction-pathname legacy)))
              "version-one generation manifests remain readable")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (wrong-commit
           (generation-tests--generation configuration
                                         "wrong-commit"
                                         "0123456789abcdef"))
         (other-identity
           (generation-tests--generation configuration
                                         "wrong-commit"
                                         "fedcba9876543210"))
         (wrong-runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record other-identity))))
         (corrupt
           (generation-tests--generation configuration
                                         "corrupt-core"
                                         "0123456789abcdef"))
         (missing-reconstruction
           (generation-tests--generation configuration
                                         "missing-reconstruction"
                                         "0123456789abcdef"))
         (missing-runner
           (make-instance
            'test-generation-core-probe-runner
            :output (generation-core-probe-output
                     (generation-core-probe-record
                      missing-reconstruction)))))
    (unwind-protect
         (progn
           (generation-tests--write-fake-core wrong-commit)
           (generation-tests--write-reconstruction wrong-commit)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration
                                      wrong-commit
                                      :probe-runner wrong-runner)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':probe)))
            "publication rejects a core whose probe names another Git commit")
           (test-assert
            (generation-tests--unpublished-p configuration wrong-commit)
            "a wrong probe identity leaves every publication path untouched")
           (generation-tests--write-fake-core corrupt)
           (generation-tests--write-reconstruction corrupt)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration corrupt)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':probe)))
            "the production probe rejects a corrupt fake core")
           (test-assert
            (generation-tests--unpublished-p configuration corrupt)
            "a corrupt core leaves every publication path untouched")
           (generation-tests--write-fake-core missing-reconstruction)
           (test-assert
            (handler-case
                (progn
                  (generation-publish configuration
                                      missing-reconstruction
                                      :probe-runner missing-runner)
                  nil)
              (checkpoint-error (condition)
                (eq (checkpoint-error-stage condition) ':publish)))
            "publication rejects a missing reconstruction script")
           (test-assert
            (generation-tests--unpublished-p configuration
                                             missing-reconstruction)
            "missing reconstruction leaves publication paths untouched"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (generation-tests--test-rollback-control-path)
  (generation-tests--test-reconstruction-capture)
  nil)

(-> test-crash-capsule-correlation () null)
(defun test-crash-capsule-correlation ()
  "Test secret-free crash capsules and per-launch pointer publication."
  (let* ((configuration    (test-configuration))
         (root             (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "crash-capsule"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :provider      nil
                          :tool-registry (make-instance 'tool-registry)
                          :worker        nil
                          :agent         nil
                          :ui            nil))
         (pointer          (merge-pathnames "crash-pointers/test-launch.path"
                                            (config :state-root configuration)))
         (session-pointer
           (merge-pathnames
            "recovery-session-pointers/test-launch.sexp"
            (config :state-root configuration)))
         (previous-pointer (uiop:getenv "AUTOLITH_CRASH_POINTER"))
         (previous-session-pointer
           (uiop:getenv "AUTOLITH_RECOVERY_SESSION_POINTER")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "preserve crash context")
           (setf (application-rendered-sequence application) 42
                 (application-history-floor-sequence application) 7)
           (platform-setenv "AUTOLITH_CRASH_POINTER" (namestring pointer))
           (platform-setenv "AUTOLITH_RECOVERY_SESSION_POINTER"
                            (namestring session-pointer))
           (application-publish-recovery-session application)
           (let ((record (read-portable-form session-pointer)))
             (test-assert
              (and (eq (first record) :recovery-session)
                   (= (getf (rest record) :version) 1)
                   (string= (getf (rest record) :conversation-id)
                            "crash-capsule")
                   (= (getf (rest record) :rendered-sequence) 42)
                   (= (getf (rest record) :history-floor-sequence) 7))
              "the per-launch session pointer preserves transcript position"))
           (test-assert (string= (uiop:getenv "AUTOLITH_CRASH_POINTER")
                                 (namestring pointer))
                        "the launch pointer is visible in the active environment")
           (test-assert (uiop:subpathp pointer
                                       (config :state-root configuration))
                        "the launch pointer is contained by private Autolith state")
           (let* ((capsule
                    (application-write-crash-capsule
                     application
                     (make-condition 'simple-error
                                     :format-control   "secret ~A"
                                     :format-arguments '("credential-value"))
                     :backtrace '((secret-frame "credential-value"))))
                  (record (read-portable-form capsule)))
             (test-assert (test-fixture-permissions-p *platform* capsule ':private-file)
                          "crash capsules are private user state")
             (test-assert
              (not (search "credential-value"
                           (uiop:read-file-string capsule)))
              "crash capsules never serialize arbitrary condition arguments")
             (test-assert (= (getf (rest record) :rendered-sequence) 42)
                          "crash capsules retain scrollback presentation progress")
             (test-assert
              (= (getf (rest record) :history-floor-sequence) 7)
              "crash capsules retain the bounded history floor")
             (test-assert
              (string= (getf (rest record) :conversation-id) "crash-capsule")
              "crash capsules correlate persisted conversations")
             (test-assert
              (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (uiop:read-file-string pointer))
                       (namestring capsule))
              "the exact launch pointer names its own crash capsule"))
           (let* ((empty        (conversation-create configuration
                                                     :identifier "empty-crash"))
                  (empty-capsule
                    (progn
                      (setf (application-conversation application) empty)
                      (application-write-crash-capsule
                       application
                       (make-condition 'simple-error
                                       :format-control   "empty crash"
                                       :format-arguments nil))))
                  (empty-record (read-portable-form empty-capsule)))
             (application-publish-recovery-session application)
             (test-assert (null (getf (rest empty-record) :conversation-id))
                          "crashes do not advertise an unpersisted conversation")
             (test-assert
              (not (conversation-storage-occupied-p
                    (conversation-pathname empty)))
              "crash reporting does not materialize an empty conversation")
             (test-assert
              (not (probe-file session-pointer))
              "an empty active conversation clears stale recovery correlation")))
      (if previous-pointer
          (platform-setenv "AUTOLITH_CRASH_POINTER" previous-pointer)
          (platform-unsetenv "AUTOLITH_CRASH_POINTER"))
      (if previous-session-pointer
          (platform-setenv "AUTOLITH_RECOVERY_SESSION_POINTER"
                           previous-session-pointer)
          (platform-unsetenv "AUTOLITH_RECOVERY_SESSION_POINTER"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> generation-tests--test-checkpoint-restart-finish-boundary () null)
(defun generation-tests--test-checkpoint-restart-finish-boundary ()
  "Test that a scheduled restart saves only at the completed-turn boundary."
  (let ((application (make-instance 'application))
        (calls       nil)
        (backend     :checkpoint-backend)
        (generation  :pending-generation))
    (setf (application-checkpoint-restart-request application)
          (cons backend generation))
    (test-assert (null calls)
                 "scheduling a restart does not save before the boundary")
    (test-call-with-function-replacements
     (list
      (list 'checkpoint-restart-save
            (lambda (actual-backend actual-generation)
              (push (list actual-backend actual-generation) calls))))
     (lambda ()
       (application--finish-checkpoint-restart application)))
    (test-assert (null (application-checkpoint-restart-request application))
                 "the pending restart request is consumed at the boundary")
    (test-assert (equal calls (list (list backend generation)))
                 "the completed boundary saves the exact scheduled generation")
    (application--finish-checkpoint-restart application)
    (test-assert (equal calls (list (list backend generation)))
                 "a completed boundary cannot save the same request twice"))
  nil)
