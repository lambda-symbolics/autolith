(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-self-target () integer)
(defun test-self-target ()
  "Return the baseline value used by active-image mutation tests."
  0)

(defvar *test-self-setting* :baseline
  "The mutable binding used by private image-commit replay tests.")

(defvar *test-discard-setting* :baseline
  "The mutable binding used by exploratory discard tests.")

(-> test-application-command-definition-source
    (&key (:definition-name symbol)
          (:name string)
          (:aliases list)
          (:action keyword)
          (:description string)
          (:tip string))
    string)
(defun test-application-command-definition-source
    (&key definition-name name aliases action description tip)
  "Return one complete application command definition for self-tool tests."
  (format nil
          "(define-application-command ~S~%~
             ~2T(:name ~S~%~
             ~3T:aliases ~S~%~
             ~3T:argument nil~%~
             ~3T:description ~S~%~
             ~3T:tip ~S~%~
             ~3T:busy-behavior :hold~%~
             ~3T:terminal-behavior :shared)~%~
             ~2T(application invocation)~%~
             ~2T~S~%~
             ~2T(declare (ignore application invocation))~%~
             ~2T~S)"
          definition-name
          name
          aliases
          description
          tip
          description
          action))

(defvar *test-self-definition-reader-side-effect-p* nil
  "Whether a self.redefine test payload executed during reading.")

(-> test-self-definition-reader-boundary () null)
(defun test-self-definition-reader-boundary ()
  "Test self.redefine rejects reader evaluation before mutation state exists."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (records-before
           (length (mutation-journal-read-records configuration))))
    (unwind-protect
         (progn
           (setf *test-self-definition-reader-side-effect-p* nil)
           (test-assert
            (handler-case
                (progn
                  (self-install-definition
                   configuration
                   "#.(progn (setf *test-self-definition-reader-side-effect-p* t) '(defun test-self-definition-reader-target () 99))")
                  nil)
              (reader-error ()
                t))
            "self.redefine rejects read-time evaluation")
           (test-assert
            (null *test-self-definition-reader-side-effect-p*)
            "rejected reader evaluation has no active-image side effect")
           (test-assert
            (not (fboundp 'test-self-definition-reader-target))
            "rejected reader evaluation installs no returned definition")
           (test-assert
            (= records-before
               (length (mutation-journal-read-records configuration)))
            "rejected reader evaluation creates no partial mutation record"))
      (setf *test-self-definition-reader-side-effect-p* nil)
      (when (fboundp 'test-self-definition-reader-target)
        (fmakunbound 'test-self-definition-reader-target))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-mutation-journal-tail-repair () null)
(defun test-mutation-journal-tail-repair ()
  "Test interrupted mutation journal tails are ignored and repaired before append."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (pathname      (configuration-journal-path configuration)))
    (unwind-protect
         (progn
           (mutation-journal-append
            configuration
            '(:mutation :id "complete-before" :result :pending))
           (with-open-file (stream pathname
                                   :direction ':output
                                   :if-exists ':append
                                   :if-does-not-exist ':error
                                   :external-format ':utf-8)
             (write-string "(:mutation :id \"interrupted" stream)
             (finish-output stream))
           (let ((records (mutation-journal-read-records configuration)))
             (test-assert
              (equal (mapcar (lambda (record) (getf (rest record) :id)) records)
                     '("complete-before"))
              "an interrupted final mutation record is ignored while reading"))
           (mutation-journal-append
            configuration
            '(:mutation :id "complete-after" :result :durable))
           (multiple-value-bind (records incomplete-final-form-p)
               (log-read pathname)
             (test-assert
              (and (not incomplete-final-form-p)
                   (equal (mapcar (lambda (record) (getf (rest record) :id))
                                  records)
                          '("complete-before" "complete-after")))
              "the next mutation append atomically repairs the interrupted tail")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-tools () null)
(defun test-self-tools ()
  "Test active definition installation, inspection, and form-aware persistence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (merge-pathnames "definitions.lisp" root))
         (previous-function (symbol-function 'test-self-target))
         (implementation-package (find-package '#:sb-ext))
         (implementation-name
           (format nil "AUTOLITH-ACTIVE-IMAGE-TEST-~A"
                   (string-upcase (make-identifier))))
         (implementation-source
           nil)
         (implementation-key nil)
         (implementation-record nil))
    (unwind-protect
         (progn
           (setf implementation-source
                 (format nil "(defun ~A () 4242)" implementation-name))
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return the installed test value.\" 42)")
           (test-assert (= (test-self-target) 42)
                        "self definition installation mutates the active image")
           (let ((records
                   (remove-if-not
                    (lambda (record)
                      (and (eq (first record) :mutation)
                           (eq (getf (rest record) :kind) :definition)
                           (string= (getf (rest record) :target)
                                    (definition-key
                                     '(defun test-self-target () 0)))))
                    (mutation-journal-read-records configuration))))
             (test-assert (= (length records) 2)
                          "definition installation journals two state records")
             (test-assert
              (and (non-empty-string-p (getf (rest (first records)) :id))
                   (string= (getf (rest (first records)) :id)
                            (getf (rest (second records)) :id)))
              "definition journal states share one stable mutation identifier"))
           (test-assert
            (search "Return the installed test value."
                    (self-inspect-symbol 'test-self-target))
            "active-image inspection exposes function documentation")
           (test-assert (sb-ext:package-locked-p implementation-package)
                        "the selected SBCL implementation package begins locked")
           (self-install-definition configuration
                                    implementation-source
                                    :package implementation-package)
           (multiple-value-bind (symbol status)
               (find-symbol implementation-name implementation-package)
             (declare (ignore status))
             (setf implementation-key
                   (definition-key (list 'defun symbol nil 4242)))
             (test-assert (and symbol
                               (fboundp symbol)
                               (= (funcall symbol) 4242))
                          "self definition installation can instrument an SBCL package"))
           (test-assert (sb-ext:package-locked-p implementation-package)
                        "active SBCL instrumentation restores the package lock")
           (setf implementation-record
                 (find-if
                  (lambda (candidate)
                    (and (eq (first candidate) :mutation)
                         (eq (getf (rest candidate) :kind) :definition)
                         (string= (or (getf (rest candidate) :package) "")
                                  "SB-EXT")
                         (eq (getf (rest candidate) :result) :installed)))
                  (mutation-journal-read-records configuration)))
           (test-assert implementation-record
                        "active implementation mutations journal their package")
           (let ((script (merge-pathnames "implementation-replay.lisp" root)))
             (image-commit-write-script
              script
              :identifier "implementation-replay"
              :title "Replay implementation instrumentation"
              :entries
              (list (image-commit--record->entry implementation-record)))
             (self-call-with-package-unlocked
              implementation-package
              (lambda ()
                (let ((symbol (find-symbol implementation-name
                                           implementation-package)))
                  (when symbol
                    (when (fboundp symbol)
                      (fmakunbound symbol))
                    (unintern symbol implementation-package)))))
             (load script)
             (let ((symbol (find-symbol implementation-name
                                        implementation-package)))
               (test-assert (and symbol
                                 (fboundp symbol)
                                 (= (funcall symbol) 4242)
                                 (sb-ext:package-locked-p
                                  implementation-package))
                            "private replay reconstructs a locked-package definition")))
           (let* ((source-root (merge-pathnames "source/" root))
                  (source-pathname (merge-pathnames "src/nested/sample.lisp" source-root))
                  (source-configuration
                    (test-configuration-for-source-root source-root)))
             (ensure-directories-exist source-pathname)
             (with-open-file (stream source-pathname
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create
                                     :external-format ':utf-8)
               (format stream
                       "(in-package #:autolith)~%~%(defun test-self-target () ~
                        \"Tracked source documentation.\" 0)~%"))
             (let* ((definitions
                      (self-tracked-definitions source-configuration
                                                'test-self-target))
                    (rendered
                      (self-render-tracked-definitions definitions
                                                       'test-self-target)))
               (test-assert (= (length definitions) 1)
                            "tracked source inspection finds the complete definition")
               (test-assert (search "src/nested/sample.lisp" rendered)
                            "tracked source inspection reports its nested repository path")
               (test-assert (search "Tracked source documentation." rendered)
                            "tracked source inspection returns exact definition text")))
            (let* ((conversation
                     (conversation-create configuration
                                          :identifier "self-describe"))
                   (context
                     (make-instance 'tool-context
                                    :configuration configuration
                                    :worker nil
                                    :conversation conversation))
                   (registry (make-default-tool-registry))
                   (result
                     (tool-execute
                      (tool-registry-find registry "lisp" "describe")
                      context
                      (json-object "designator" "TEST-SELF-TARGET"
                                   "target" "self"))))
              (test-assert
               (and (null (tool-registry-find registry "self" "inspect"))
                    (null (tool-registry-find registry "self" "source"))
                    (tool-result-success-p result)
                    (search "Return the installed test value."
                            (tool-result-content result)))
               "lisp.describe target self replaces self.inspect"))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "self-sbcl-source"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
                   (result
                     (tool-execute
                      (tool-registry-find (make-default-tool-registry)
                                          "lisp"
                                          "source")
                      context
                      (json-object "name" "CL:MAPCAR"
                                   "target" "self"
                                   "kind" "function"))))
              (test-assert
               (and (tool-result-success-p result)
                    (search "src/code/list.lisp"
                            (tool-result-content result)))
               "lisp.source target self falls back to matching active SBCL source"))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "self-dependency-source"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
                   (result
                     (tool-execute
                      (tool-registry-find (make-default-tool-registry)
                                          "lisp"
                                          "source")
                      context
                      (json-object
                       "name" "SBCL-WORKERS:SBCL-WORKER-CREATE"
                       "target" "self"))))
              (test-assert
               (and (tool-result-success-p result)
                    (search "sbcl-workers:source/workers.lisp"
                            (tool-result-content result))
                    (search "(defun sbcl-worker-create"
                            (tool-result-content result)))
               "lisp.source target self reads exact pinned dependency source"))
           (test-assert
            (handler-case
                (progn
                  (self-dependency-definitions
                   'sbcl-workers:sbcl-worker-create
                   (find-package '#:sbcl-workers)
                   :system-name "not-an-autolith-dependency")
                  nil)
              (source-mutation-error ()
                t))
            "dependency source inspection rejects systems outside Autolith")
           (test-assert
            (equal (definition-signature
                    '(defmethod sample-operation ((left string) right) left))
                   (definition-signature
                    '(defmethod sample-operation ((value string) ignored) value)))
            "method identity ignores parameter names while retaining specializers")
           (test-assert
            (definition-form-p '(defun (setf sample-value) (value object)
                                  (declare (ignore object))
                                  value))
            "definition identity accepts SETF function names")
           (test-assert (definition-form-p '(defparameter *sample-value* 42))
                        "durable definitions include mutable global parameters")
           (test-assert
            (not (definition-form-p '(defconstant constant-trial 42)))
            "durable definitions reject Common Lisp constants")
           (test-assert
            (not (definition-form-p '(define-constant constant-trial 42)))
            "durable definitions reject compatibility constant macros")
           (let ((original
                   (make-condition 'simple-error
                                   :format-control "original failure"
                                   :format-arguments nil)))
             (test-assert
              (handler-case
                  (progn
                    (self-restore-definition
                     "(defun test-self-target () 0)"
                     original
                     :installer
                     (lambda (definition source)
                       (declare (ignore definition source))
                       (error "restoration failure")))
                    nil)
                (active-image-corruption (condition)
                  (and (eq (active-image-corruption-original-condition condition)
                           original)
                       (typep
                        (active-image-corruption-restoration-condition condition)
                        'serious-condition))))
              "a restoration failure preserves both conditions and escapes tool handling"))
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream
                     "; preserve this comment~%~%(defun first-definition () 1)~%~%(defun test-self-target () 0)~%"))
           (source-replace-definition
            pathname
            "(defun test-self-target () \"Persisted documentation.\" 84)")
           (let ((updated (uiop:read-file-string pathname)))
             (test-assert (search "; preserve this comment" updated)
                          "form-aware replacement preserves preceding comments")
             (test-assert (search "Persisted documentation." updated)
                          "form-aware replacement writes the complete definition")
             (test-assert (search "(defun first-definition () 1)" updated)
                          "form-aware replacement preserves neighboring forms")))
      (setf (symbol-function 'test-self-target) previous-function)
      (remhash (definition-key '(defun test-self-target () 0))
               *exploratory-definitions*)
      (when implementation-key
        (remhash implementation-key *exploratory-definitions*))
      (self-call-with-package-unlocked
       implementation-package
       (lambda ()
         (let ((symbol (find-symbol implementation-name
                                    implementation-package)))
           (when symbol
             (when (fboundp symbol)
               (fmakunbound symbol))
             (unintern symbol implementation-package)))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-definition-installation-rollback () null)
(defun test-self-definition-installation-rollback ()
  "Test failed definition installation restores exact live and cached state."
  (let* ((*package* (find-package '#:autolith))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (existing-name 'test-self-atomic-existing)
         (new-name 'test-self-atomic-new)
         (existing-identifier
           (context--definition-identifier existing-name))
         (new-identifier (context--definition-identifier new-name))
         (baseline-source
           "(define-context-contributor test-self-atomic-existing (context) (declare (ignore context)) :baseline)")
         (replacement-source
           "(define-context-contributor test-self-atomic-existing (context) (declare (ignore context)) :replacement)")
         (new-source
           "(define-context-contributor test-self-atomic-new (context) (declare (ignore context)) :new)")
         (baseline-definition (self-read-form baseline-source))
         (new-definition (self-read-form new-source))
         (existing-target (definition-key baseline-definition))
         (new-target (definition-key new-definition))
         (original-journal-function (symbol-function 'mutation-journal-append))
         (original-register-function
           (symbol-function 'register-context-contributor)))
    (unwind-protect
         (progn
           (when (fboundp existing-name)
             (fmakunbound existing-name))
           (when (fboundp new-name)
             (fmakunbound new-name))
           (unregister-context-contributor existing-identifier)
           (unregister-context-contributor new-identifier)
           (remhash existing-target *exploratory-definitions*)
           (remhash new-target *exploratory-definitions*)
           (self--install-definition baseline-definition baseline-source)
           (let ((baseline-registration
                   (context--registration-snapshot existing-identifier)))
             (setf (symbol-function 'mutation-journal-append)
                   (lambda (checked-configuration record)
                     (if (and (eq (getf (rest record) :kind) :definition)
                              (eq (getf (rest record) :result) :installed))
                         (error "Injected installed-journal failure.")
                         (funcall original-journal-function
                                  checked-configuration
                                  record))))
             (test-assert
              (handler-case
                  (progn
                    (self-install-definition configuration replacement-source)
                    nil)
                (error ()
                  t))
              "an installed-journal failure rejects the exploratory definition")
             (test-assert
              (eq (funcall (symbol-function existing-name) nil) :baseline)
              "failed installation restores the exact preceding function")
             (test-assert
              (equal (context--registration-snapshot existing-identifier)
                     baseline-registration)
              "failed installation restores the preceding side registration")
             (test-assert
              (string= (gethash existing-target *exploratory-definitions*)
                       baseline-source)
              "failed installation restores the preceding exploratory source")
             (setf (symbol-function 'mutation-journal-append)
                   original-journal-function))
           (setf (symbol-function 'register-context-contributor)
                 (lambda (identifier function-designator &key source)
                   (declare (ignore identifier function-designator source))
                   (error "Injected registration failure.")))
           (test-assert
            (handler-case
                (progn
                  (self-install-definition configuration new-source)
                  nil)
              (error ()
                t))
            "a partial defining-form failure rejects the new definition")
           (test-assert (not (fboundp new-name))
                        "partial installation restores an unbound function")
           (test-assert (null (context--registration-snapshot new-identifier))
                        "partial installation removes a new side registration")
           (test-assert
            (not (nth-value 1
                            (gethash new-target *exploratory-definitions*)))
            "failed installation does not invent an exploratory source")
           (test-assert
            (equal
             (loop for record in (mutation-journal-read-records configuration)
                   when (and (eq (first record) :mutation)
                             (member (getf (rest record) :target)
                                     (list existing-target new-target)
                                     :test #'string=))
                     collect (getf (rest record) :result))
             '(:pending :failed :pending :failed))
            "each rejected definition journals pending and failed states"))
      (setf (symbol-function 'mutation-journal-append)
            original-journal-function
            (symbol-function 'register-context-contributor)
            original-register-function)
      (unregister-context-contributor existing-identifier)
      (unregister-context-contributor new-identifier)
      (when (fboundp existing-name)
        (fmakunbound existing-name))
      (when (fboundp new-name)
        (fmakunbound new-name))
      (remhash existing-target *exploratory-definitions*)
      (remhash new-target *exploratory-definitions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-application-command-definitions () null)
(defun test-self-application-command-definitions ()
  "Test command definition identity, rollback, discard, and private replay."
  (let* ((*package* (find-package '#:autolith))
         (configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry-snapshot (application-command--registry-snapshot))
         (definition-names
           '(test-self-command-existing
             test-self-command-new
             test-self-command-collision-owner
             test-self-command-collision-rejected
             test-self-command-replay))
         (function-snapshots
           (loop for name in definition-names
                 collect
                 (list name
                       (not (null (fboundp name)))
                       (and (fboundp name) (fdefinition name)))))
         (existing-baseline-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-existing
            :name "/self-command-existing"
            :aliases nil
            :action ':continue
            :description "Return the baseline command action."
            :tip "Use /self-command-existing for the baseline action."))
         (existing-replacement-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-existing
            :name "/self-command-existing"
            :aliases '("/self-command-existing-alias")
            :action ':quit
            :description "Return the replacement command action."
            :tip "Use /self-command-existing for the replacement action."))
         (new-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-new
            :name "/self-command-new"
            :aliases nil
            :action ':continue
            :description "Return the new command action."
            :tip "Use /self-command-new for the new action."))
         (collision-owner-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-collision-owner
            :name "/self-command-collision-owner"
            :aliases '("/self-command-collision")
            :action ':continue
            :description "Own the colliding command alias."
            :tip "Use /self-command-collision-owner for the owner."))
         (collision-rejected-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-collision-rejected
            :name "/self-command-collision"
            :aliases nil
            :action ':quit
            :description "Attempt to collide with an existing alias."
            :tip "This colliding command must never become effective."))
         (replay-source
           (test-application-command-definition-source
            :definition-name 'test-self-command-replay
            :name "/self-command-replay"
            :aliases '("/self-command-replay-alias")
            :action ':continue
            :description "Return the replayed command action."
            :tip "Use /self-command-replay after private replay."))
         (existing-baseline-definition
           (self-read-form existing-baseline-source :read-eval nil))
         (existing-replacement-definition
           (self-read-form existing-replacement-source :read-eval nil))
         (new-definition (self-read-form new-source :read-eval nil))
         (collision-rejected-definition
           (self-read-form collision-rejected-source :read-eval nil))
         (replay-definition (self-read-form replay-source :read-eval nil))
         (targets
           (mapcar
            #'definition-key
            (list existing-baseline-definition
                  new-definition
                  collision-rejected-definition
                  replay-definition)))
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*))
    (unwind-protect
         (progn
           (dolist (name definition-names)
             (unregister-application-command name :source ':runtime)
             (when (fboundp name)
               (fmakunbound name)))
           (dolist (target targets)
             (remhash target *exploratory-definitions*))
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (image-state-load configuration)
           (test-assert
            (definition-form-p existing-baseline-definition)
            "application commands are supported top-level definitions")
           (test-assert
            (equal (definition-signature existing-baseline-definition)
                   (definition-signature existing-replacement-definition))
            "command definition identity survives metadata changes")
           (test-assert
            (not
             (equal (definition-signature existing-baseline-definition)
                    '(defun test-self-command-existing)))
            "command definitions remain distinct from same-named functions")
           (let* ((tracked-root (merge-pathnames "tracked/" root))
                  (tracked-pathname
                    (merge-pathnames "src/application/commands.lisp" tracked-root))
                  (tracked-configuration
                    (test-configuration-for-source-root tracked-root)))
             (ensure-directories-exist tracked-pathname)
             (with-open-file (stream tracked-pathname
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create
                                     :external-format ':utf-8)
               (format stream "(in-package #:autolith)~2%~A~%"
                       replay-source))
             (let ((definitions
                     (self-tracked-definitions
                      tracked-configuration
                      'test-self-command-replay)))
               (test-assert
                (and (= (length definitions) 1)
                     (equal
                      (definition-signature
                       (source-form-form
                        (tracked-definition-source-form
                         (first definitions))))
                      (definition-signature replay-definition)))
                "tracked source inspection finds application command definitions")))
           (eval existing-baseline-definition)
           (let ((baseline-binding
                   (fdefinition 'test-self-command-existing))
                 (baseline-registration
                   (application-command--registration-snapshot
                    'test-self-command-existing
                    ':runtime))
                 (baseline-command
                   (application-command-find "/self-command-existing")))
             (self-install-definition
              configuration
              existing-replacement-source)
             (test-assert
              (eq (funcall
                   (fdefinition 'test-self-command-existing)
                   nil
                   nil)
                  ':quit)
              "self.redefine installs replacement command behavior")
             (test-assert
              (eq (application-command-definition-name
                   (application-command-find
                    "/self-command-existing-alias"))
                  'test-self-command-existing)
              "self.redefine publishes replacement command metadata")
             (self-discard-mutation configuration nil)
             (test-assert
              (eq (fdefinition 'test-self-command-existing)
                  baseline-binding)
              "discard restores an existing command's exact function")
             (test-assert
              (equal
               (application-command--registration-snapshot
                'test-self-command-existing
                ':runtime)
               baseline-registration)
              "discard restores an existing command's exact runtime registration")
             (test-assert
              (and (eq (application-command-find "/self-command-existing")
                       baseline-command)
                   (null
                    (application-command-find
                     "/self-command-existing-alias")))
              "discard restores the preceding command projection"))
           (self-install-definition configuration new-source)
           (test-assert
            (and (fboundp 'test-self-command-new)
                 (eq
                  (application-command-definition-name
                   (application-command-find "/self-command-new"))
                  'test-self-command-new))
            "self.redefine installs a new command and registration")
           (self-discard-mutation configuration nil)
           (test-assert
            (and (not (fboundp 'test-self-command-new))
                 (null
                  (application-command--registration-snapshot
                   'test-self-command-new
                   ':runtime))
                 (null (application-command-find "/self-command-new")))
            "discard removes a new command's function and registration")
           (eval (self-read-form collision-owner-source :read-eval nil))
           (let ((owner-command
                   (application-command-find "/self-command-collision")))
             (test-assert
              (handler-case
                  (progn
                    (self-install-definition
                     configuration
                     collision-rejected-source)
                    nil)
                (error ()
                  t))
              "identifier collision rejects a new command definition")
             (test-assert
              (and
               (not (fboundp 'test-self-command-collision-rejected))
               (null
                (application-command--registration-snapshot
                 'test-self-command-collision-rejected
                 ':runtime))
               (eq (application-command-find "/self-command-collision")
                   owner-command)
               (not
                (nth-value
                 1
                 (gethash
                  (definition-key collision-rejected-definition)
                  *exploratory-definitions*))))
              "collision rollback restores the exact function, registry, and cache"))
           (let ((script (merge-pathnames "command-replay.lisp" root)))
             (image-commit-write-script
              script
              :identifier "command-replay"
              :title "Replay one application command"
              :entries
              (list
               (list :kind ':definition
                     :id "command-replay-definition"
                     :target (definition-key replay-definition)
                     :package "AUTOLITH"
                     :source replay-source)))
             (load script)
             (load script))
           (test-assert
            (= (loop for registration
                       in (application-command--registrations)
                     for command = (getf registration :command)
                     count
                     (and
                      (eq (getf registration :source) ':runtime)
                      (eq (application-command-definition-name command)
                          'test-self-command-replay)))
               1)
            "private replay replaces rather than duplicates command registration")
           (test-assert
            (and
             (eq
              (application-command-definition-name
               (application-command-find
                "/self-command-replay-alias"))
              'test-self-command-replay)
             (eq
              (funcall
               (application-command-handler
                (application-command-find "/self-command-replay"))
               nil
               nil)
              ':continue))
            "private replay reconstructs command metadata and behavior"))
      (application-command--registry-restore registry-snapshot)
      (dolist (snapshot function-snapshots)
        (self--restore-function-binding
         (first snapshot)
         (second snapshot)
         (third snapshot)))
      (dolist (target targets)
        (remhash target *exploratory-definitions*))
      (setf *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-restart-selection () null)
(defun test-self-restart-selection ()
  "Test restart discovery and selection through the active-image tools."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "restarts"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (registry (make-default-tool-registry))
                (eval-tool (tool-registry-find registry "self" "eval")))
           (labels ((run (&rest arguments)
                      "Execute self.eval with ARGUMENTS through the registry."
                      (tool-registry-execute-call
                       registry
                       (json-object "namespace" "self"
                                    "name" "eval"
                                    "arguments" (json-encode
                                                 (apply #'json-object
                                                        arguments)))
                       context)))
             (declare (ignorable eval-tool))
             (let ((result (run "form"
                                "(cerror \"Keep going anyway.\" \"Deliberate stop.\")")))
               (test-assert (not (tool-result-success-p result))
                            "correctable conditions fail without a restart")
               (test-assert (search "Available restarts"
                                    (tool-result-content result))
                            "failures enumerate the available restarts")
               (test-assert (search "CONTINUE" (tool-result-content result))
                            "the continue restart is offered")
               (test-assert (not (search "  ABORT"
                                         (tool-result-content result)))
                            "the abort restart is never offered"))
             (test-assert (tool-result-success-p
                           (run "form"
                                "(cerror \"Keep going anyway.\" \"Deliberate stop.\")"
                                "restart" "CONTINUE"))
                          "selecting continue completes the operation")
             (let ((result (run "form"
                                "(restart-case (error \"Needs a value.\") (use-value (value) value))"
                                "restart" "USE-VALUE"
                                "restart-value" "(* 6 7)")))
               (test-assert (and (tool-result-success-p result)
                                 (search "42" (tool-result-content result)))
                            "value restarts receive the evaluated value"))
             (test-assert (not (tool-result-success-p
                                (run "form"
                                     "(cerror \"Keep going.\" \"Stop.\")"
                                     "restart" "NO-SUCH-RESTART")))
                          "unknown restart names still fail with the menu")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-discard () null)
(defun test-self-discard ()
  "Test exact layered restoration and append-only exploratory discard state."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (previous-function (symbol-function 'test-self-target))
         (previous-setting *test-discard-setting*)
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "self-discard"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation))
                (registry (make-default-tool-registry))
                (set-tool (tool-registry-find registry "self" "set"))
                (diff-tool (tool-registry-find registry "self" "diff"))
                (exercise-tool (tool-registry-find registry "self" "exercise"))
                (discard-tool (tool-registry-find registry "self" "discard")))
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (image-state-load configuration)
           (tool-execute set-tool context
                         (json-object "symbol" "*test-discard-setting*"
                                      "value" ":first"))
           (let ((result
                   (tool-execute
                    exercise-tool
                    context
                    (json-object
                     "form"
                     "(assert (eq *test-discard-setting* :first))"))))
             (test-assert
              (and (tool-result-success-p result)
                   (search "passed for mutation" (tool-result-content result)))
              "self.exercise reports a focused passing assertion"))
           (test-assert
            (handler-case
                (progn
                  (tool-execute exercise-tool
                                context
                                (json-object "form" "(assert nil)"))
                  nil)
              (error ()
                t))
            "self.exercise propagates a focused assertion failure")
           (let ((exercise-records
                   (remove-if-not
                    (lambda (record)
                      (and (eq (first record) :mutation)
                           (eq (getf (rest record) :kind) :exercise)))
                    (mutation-journal-read-records configuration))))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :result))
                             exercise-records)
                     '(:pending :passed :pending :failed))
              "focused exercise evidence is append-only and records outcomes"))
           (tool-execute set-tool context
                         (json-object "symbol" "*test-discard-setting*"
                                      "value" ":second"))
           (let ((diff (tool-execute diff-tool context (json-object))))
             (test-assert
              (and (search "Installed mutations: 2"
                           (tool-result-content diff))
                   (search "Effective changes: 1"
                           (tool-result-content diff))
                   (search ":second" (tool-result-content diff))
                   (not (search ":first" (tool-result-content diff))))
              "self.diff collapses layered edits to their effective state"))
           (tool-execute discard-tool context (json-object))
           (test-assert (eq *test-discard-setting* :first)
                        "self.discard restores an exact previous object value")
           (test-assert (= (length (image-commit-pending-records configuration))
                           1)
                        "discarding a layered set exposes its preceding mutation")
           (tool-execute discard-tool context (json-object))
           (test-assert (eq *test-discard-setting* :baseline)
                        "discarding the first set restores its baseline binding")
           (tool-execute set-tool context
                         (json-object "symbol" "*test-discard-setting*"
                                      "value" ":temporary"))
           (tool-execute set-tool context
                         (json-object "symbol" "*test-discard-setting*"
                                      "value" ":baseline"))
           (test-assert
            (search "no effective change"
                    (tool-result-content
                     (tool-execute diff-tool context (json-object))))
            "self.diff recognizes a mutation stack returned to its baseline")
           (tool-execute discard-tool context (json-object))
           (tool-execute discard-tool context (json-object))
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return first discard layer.\" 1)")
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return second discard layer.\" 2)")
           (tool-execute discard-tool context (json-object))
           (test-assert (= (test-self-target) 1)
                        "self.discard peels back a layered definition")
           (test-assert
            (search "Return first discard layer."
                    (gethash (definition-key '(defun test-self-target () 0))
                             *exploratory-definitions*))
            "a layered discard exposes the preceding exploratory source")
           (let* ((effective
                    (image-commit-effective-pending-records configuration))
                  (identifier (getf (rest (first effective)) :id)))
             (tool-execute discard-tool
                           context
                           (json-object "mutation" identifier)))
           (test-assert (= (test-self-target) 0)
                        "an identified definition discard restores its exact function")
           (test-assert
            (null (gethash (definition-key '(defun test-self-target () 0))
                           *exploratory-definitions*))
            "discarding the first definition clears its exploratory source")
           (self-install-definition
            configuration
            "(defun test-new-discard-target () \"Exist only briefly.\" 3)")
           (test-assert (fboundp 'test-new-discard-target)
                        "the new exploratory definition is installed")
           (tool-execute discard-tool context (json-object))
           (test-assert (not (fboundp 'test-new-discard-target))
                        "discarding a new definition restores an unbound function")
           (test-assert (null (image-commit-pending-records configuration))
                        "completed discards leave no commit candidates")
           (let ((result
                   (tool-execute
                    exercise-tool
                    context
                    (json-object
                     "form"
                     "(assert (eq *test-discard-setting* :baseline))"))))
             (test-assert
              (and (tool-result-success-p result)
                   (search "current active state"
                           (tool-result-content result)))
              "self.exercise verifies restored state after discard"))
           (test-assert
            (handler-case
                (progn
                  (tool-execute exercise-tool
                                context
                                (json-object "form" "t"
                                             "mutation" "absent"))
                  nil)
              (source-mutation-error ()
                t))
            "self.exercise rejects an unknown mutation identifier")
           (test-assert
            (handler-case
                (progn
                  (tool-execute discard-tool
                                context
                                (json-object "mutation" "absent"))
                  nil)
              (source-mutation-error ()
                t))
            "self.discard rejects an unknown mutation identifier"))
      (setf (symbol-function 'test-self-target) previous-function
            *test-discard-setting* previous-setting
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (when (fboundp 'test-new-discard-target)
        (fmakunbound 'test-new-discard-target))
      (clrhash *exploratory-undo-actions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-standard-mutation-checker () null)
(defun test-standard-mutation-checker ()
  "Test the default commit gate reads proposed definitions without the suite."
  (flet ((record (identifier proposed)
           (list :mutation :id identifier :kind :durable-definition
                 :target identifier :proposed proposed)))
    (test-assert
     (search "2 proposed forms"
             (image-commit-proposed-forms-check
              (list (record "a" "(defun mutation-checker-test-a () 1)")
                    (record "b" "(defun mutation-checker-test-b () 2)"))
              nil))
     "the structural gate reads every proposed definition form")
    (test-assert
     (search "0 proposed forms"
             (image-commit-proposed-forms-check
              (list (record "removal" nil))
              nil))
     "records without proposed sources pass the structural gate")
    (test-assert
     (handler-case
         (progn (image-commit-proposed-forms-check
                 (list (record "broken" "(defun broken ("))
                 nil)
                nil)
       (image-commit-error (condition)
         (and (eq (image-commit-error-stage condition) ':validation)
              (search "broken" (autolith-error-message condition)))))
     "unreadable proposed definitions fail commit validation")
    (test-assert
     (handler-case
         (progn (image-commit-proposed-forms-check
                 (list (record "hostile" "#.(error \"never evaluated\")"))
                 nil)
                nil)
       (image-commit-error () t))
     "the structural gate never evaluates reader forms"))
  nil)

(-> test-durable-self-mutation () null)
(defun test-durable-self-mutation ()
  "Test private live-mutation commits, replay, and recovery."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-durable-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration (test-configuration-for-source-root source-root))
         (outside-workspace
           (uiop:ensure-directory-pathname
            (merge-pathnames "unrelated-workspace/"
                             (uiop:temporary-directory))))
         (source-pathname (merge-pathnames "src/definitions.lisp" source-root))
         (previous-function (symbol-function 'test-self-target))
         (previous-setting *test-self-setting*)
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*)
         (legacy-identifier nil)
         (*image-commit-replay-probe-function*
           (lambda (checked-configuration script identifier)
             (declare (ignore checked-configuration identifier))
             (test-assert (probe-file script)
                          "private replay is written before its clean probe")
             nil))
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              "active checks passed"))))
    (unwind-protect
         (progn
           (ensure-directories-exist source-pathname)
           (with-open-file (stream source-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream
                     "(in-package #:autolith)~%~%(defun test-self-target () \"Return the durable baseline.\" 0)~%"))
           (self-git-command configuration '("init" "--quiet"))
           (self-git-command configuration
                             '("config" "user.name" "Autolith Test"))
           (self-git-command
            configuration
            '("config" "user.email" "autolith-test@example.invalid"))
           (self-git-command configuration '("add" "src/definitions.lisp"))
           (self-git-command configuration
                             '("commit" "--quiet" "--no-gpg-sign" "-m" "Create baseline"))
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "durable-mutation"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :mutation-checker checker))
                  (outside-configuration
                    (make-instance
                     'configuration
                     :source-root source-root
                     :working-directory outside-workspace
                     :data-root (configuration-data-root configuration)
                     :state-root (configuration-state-root configuration)
                     :cache-root (configuration-cache-root configuration)
                     :config-root (configuration-config-root configuration)
                     :codex-auth-path
                     (configuration-codex-auth-path configuration)
                     :model (configuration-model configuration)
                     :reasoning-effort
                     (configuration-reasoning-effort configuration)
                     :provider-endpoint
                     (configuration-provider-endpoint configuration)))
                  (outside-context
                    (make-instance 'tool-context
                                   :configuration outside-configuration
                                   :worker nil
                                   :conversation conversation
                                   :mutation-checker checker))
                  (failing-active-context
                    (make-instance
                     'tool-context
                     :configuration configuration
                     :worker nil
                     :conversation conversation
                     :mutation-checker
                     (make-instance
                      'callback-mutation-checker
                      :active-callback
                      (lambda (checked-configuration definition-source)
                        (declare (ignore checked-configuration
                                         definition-source))
                        (error "Injected active check failure.")))))
                  (registry (make-default-tool-registry))
                  (persist-tool (tool-registry-find registry
                                                    "self"
                                                    "persist-definition"))
                  (set-tool (tool-registry-find registry "self" "set"))
                  (diff-tool (tool-registry-find registry "self" "diff"))
                  (commit-tool (tool-registry-find registry "self" "commit")))
              (setf *image-state-initialized-p* nil
                    *active-image-commit-identifier* nil
                    *active-image-history-commit* nil
                    *active-image-lineage-identifier* nil)
              (test-assert (null (image-state-load configuration))
                           "startup initializes an empty private mutation lineage")
              (test-assert (null (image-commit-current configuration))
                           "empty startup begins without a private image commit")
              (test-assert
               (eq (durable-mutation-record-p
                    configuration
                    (list :mutation
                          :kind :durable-definition
                          :id "retired-overlay-record"
                          :target (definition-key
                                   '(defun test-self-target () 0))
                          :pathname
                          (namestring
                           (merge-pathnames
                            "retired/overlays/test-self-target.lisp"
                            (uiop:temporary-directory)))
                          :previous ""
                          :proposed
                          "(defun test-self-target () 0)"
                          :base-commit nil
                          :result ':failed))
                   t)
               "v0.31.1 overlay journal paths remain valid after the upgrade")
              (let* ((legacy-source
                       "(defun test-self-target () \"Return the legacy private value.\" 9)")
                     (current-source
                       "(defun test-self-target () \"Return the v0.31.1 committed value.\" 10)")
                     (identifier (make-identifier))
                     (directory (image-commit--directory configuration identifier))
                     (script-pathname (merge-pathnames "reconstruct.lisp" directory))
                     (manifest-pathname (merge-pathnames "manifest.sexp" directory))
                     (entries
                       (list (list :kind ':legacy
                                   :id "legacy-test-self-target"
                                   :target "defun-test-self-target.lisp"
                                   :source legacy-source)
                             (list :kind ':definition
                                   :id "definition-test-self-target"
                                   :target (definition-key
                                            '(defun test-self-target () 0))
                                   :package "AUTOLITH"
                                   :source current-source)))
                     (generation-script
                       (merge-pathnames "legacy-generation.lisp" source-root)))
                (image-commit-write-script
                 script-pathname
                 :identifier identifier
                 :title "Selected v0.31.1 mixed private commit"
                 :entries entries)
                (image-commit--write-form-atomically
                 manifest-pathname
                 (image-commit--manifest-form
                  :identifier identifier
                  :parent-identifier nil
                  :title "Selected v0.31.1 mixed private commit"
                  :source-commit nil
                  :script-pathname script-pathname
                  :entries entries
                  :consumed-mutation-identifiers
                  '("legacy-test-self-target" "definition-test-self-target")
                  :journal-position 0
                  :created-at (get-universal-time)))
                (let ((history-commit
                        (image-history-commit
                         configuration
                         :identifier identifier
                         :title "Selected v0.31.1 mixed private commit"
                         :manifest-pathname manifest-pathname
                         :script-pathname script-pathname)))
                  (image-commit--write-form-atomically
                   (configuration-current-image-commit-path configuration)
                   (list :current-image-commit
                         :version 2
                         :id identifier
                         :manifest (namestring manifest-pathname)
                         :history-commit history-commit)))
                (setf legacy-identifier identifier
                      (symbol-function 'test-self-target) previous-function
                      *image-state-initialized-p* nil
                      *active-image-commit-identifier* nil
                      *active-image-history-commit* nil
                      *active-image-lineage-identifier* nil)
                (test-assert (null (image-state-load configuration))
                             "startup replays a selected v0.31.1 mixed commit")
                (test-assert (= (test-self-target) 10)
                             "the newest historical definition wins replay")
                (test-assert
                 (string= (image-commit-definition-source
                           configuration
                           (definition-key '(defun test-self-target () 0)))
                          current-source)
                 "rollback resolves the newest historical definition source")
                (image-commit-write-generation-script
                 generation-script
                 :generation-identifier "legacy-generation"
                 :commit (image-commit-current configuration))
                (setf (symbol-function 'test-self-target) previous-function)
                (load generation-script)
                (test-assert (= (test-self-target) 10)
                             "retained generations preserve mixed-entry replay order")
                (self-install-definition
                 configuration
                 "(defun test-self-target () \"Return a discarded value.\" 12)")
                (let ((record (first (image-commit-pending-records configuration))))
                  (test-assert record
                               "legacy upgrade stages a reversible mutation")
                  (self-discard-mutation configuration
                                         (getf (rest record) :id)))
                (test-assert (= (test-self-target) 10)
                             "discard restores the newest historical definition"))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     persist-tool
                     failing-active-context
                     (json-object
                      "definition"
                      "(defun test-self-target () \"Return a rejected value.\" 13)"))
                    nil)
                (error ()
                  t))
              "a failing active check rejects private persistence")
              (test-assert (= (test-self-target) 10)
                           "a rejected definition restores the newest historical value")
              (test-assert
               (string= (or (image-commit--pointer-identifier configuration) "")
                        legacy-identifier)
               "a rejected definition preserves private selection")
             (let* ((*package* (find-package '#:autolith))
                    (new-definition-source
                      "(defun test-self-new-rejected-definition () \"Exist only during a rejected durable mutation.\" 17)")
                    (new-definition
                      (self-read-form new-definition-source :read-eval nil))
                    (new-target (definition-key new-definition)))
               (when (fboundp 'test-self-new-rejected-definition)
                 (fmakunbound 'test-self-new-rejected-definition))
               (remhash new-target *exploratory-definitions*)
               (test-assert
                (handler-case
                    (progn
                      (tool-execute
                       persist-tool
                       failing-active-context
                       (json-object "definition" new-definition-source))
                      nil)
                  (error ()
                    t))
                "a failing active check rejects a new durable definition")
               (test-assert
                (not (fboundp 'test-self-new-rejected-definition))
                "rejected new persistence restores an unbound function")
               (test-assert
                (not (nth-value
                      1
                      (gethash new-target *exploratory-definitions*)))
                "rejected new persistence removes its exploratory source")
               (test-assert
                (string= (or (image-commit--pointer-identifier configuration) "")
                         legacy-identifier)
                "rejected new persistence leaves private selection unchanged"))
             (let* ((result
                      (tool-execute
                       persist-tool
                       context
                       (json-object
                        "definition"
                        "(defun test-self-target () \"Return the durable value.\" 84)")))
                    (first-commit (image-commit-current configuration)))
               (test-assert (tool-result-success-p result)
                            "durable definition persistence succeeds")
               (test-assert first-commit
                            "durable persistence selects a private commit")
               (test-assert
                (uiop:subpathp (image-commit-script-pathname first-commit)
                               (configuration-image-commit-root configuration))
                "the reconstruction script stays under private Autolith data")
               (test-assert
                (= (logand #o777
                           (sb-posix:stat-mode
                            (sb-posix:stat
                             (namestring
                              (image-commit-script-pathname first-commit)))))
                   #o444)
                "published private replay scripts are read-only")
                (test-assert
                 (string= (or (image-commit-parent-identifier first-commit) "")
                          legacy-identifier)
                 "durable persistence advances the selected legacy lineage")
                (let ((script
                        (uiop:read-file-string
                         (image-commit-script-pathname first-commit))))
                  (test-assert
                   (and (search "Return the durable value." script)
                        (not (search "Return the legacy private value." script))
                        (not (search "Return the v0.31.1 committed value." script)))
                   "durable persistence normalizes the historical definition entries"))
               (let ((first-identifier (image-commit-identifier first-commit)))
                 (tool-execute
                  persist-tool
                  context
                  (json-object
                   "definition"
                   "(defun test-self-target () \"Return the second value.\" 85)"))
                 (let* ((second-commit (image-commit-current configuration))
                        (script (uiop:read-file-string
                                 (image-commit-script-pathname second-commit))))
                   (test-assert
                    (string= (or (image-commit-parent-identifier second-commit) "")
                             first-identifier)
                    "private definition commits form an immutable lineage")
                   (test-assert
                    (and (search "Return the second value." script)
                         (not (search "Return the durable value." script)))
                    "a full replay snapshot retains only the effective definition"))))
             (test-assert (= (test-self-target) 85)
                          "private persistence installs the latest definition")
             (test-assert
              (search "Return the durable baseline."
                      (uiop:read-file-string source-pathname))
              "private persistence never modifies tracked source")
             (let ((mutation
                     (loop for value being the hash-values
                             of *durable-mutations*
                           when (and
                                 (string=
                                  (durable-mutation-target value)
                                  (definition-key
                                   '(defun test-self-target () 0)))
                                 (eq (durable-mutation-phase value) :durable))
                             return value)))
               (test-assert mutation
                            "private definition persistence becomes durable")
               (let ((identifier (durable-mutation-identifier mutation)))
                 (clrhash *durable-mutations*)
                 (durable-mutations-load configuration)
                 (test-assert
                  (eq (durable-mutation-phase
                       (gethash identifier *durable-mutations*))
                      :durable)
                  "durable private state replays from the journal")))
             (let ((failed-result
                     (handler-case
                         (progn
                           (tool-execute
                            set-tool
                            context
                            (json-object
                             "symbol" "*test-self-setting*"
                             "value" "(error \"Rejected setting.\")"))
                           nil)
                       (error ()
                         t))))
               (test-assert failed-result
                            "a failed self.set operation escapes as a failure"))
             (tool-execute
              set-tool
              context
              (json-object
               "symbol" "*test-self-setting*"
               "value" ":committed-setting"))
             (self-install-definition
              configuration
              "(defun test-self-target () \"Return the staged value.\" 86)")
             (let ((diff
                     (tool-execute diff-tool context (json-object))))
               (test-assert
                (and (tool-result-success-p diff)
                     (search "Return the staged value."
                             (tool-result-content diff))
                     (search ":committed-setting"
                             (tool-result-content diff)))
                "self.diff shows pending reconstructible image mutations"))
             (with-open-file (stream source-pathname
                                     :direction ':output
                                     :if-exists ':append
                                     :external-format ':utf-8)
               (format stream "~%;; A user-made repository change.~%"))
             (let* ((head-before
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD"))))
                    (parent-before
                      (image-commit-identifier
                       (image-commit-current configuration)))
                    (result
                      (tool-execute
                       commit-tool
                       outside-context
                       (json-object
                        "title" "Persist staged live mutations")))
                    (committed (image-commit-current configuration))
                    (script
                      (uiop:read-file-string
                       (image-commit-script-pathname committed)))
                    (head-after
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD")))))
               (test-assert (tool-result-success-p result)
                            "self.commit persists staged live mutations")
               (test-assert
                (string= (or (image-commit-parent-identifier committed) "")
                         parent-before)
                "self.commit advances the active private lineage")
               (test-assert
                (and (search "Return the staged value." script)
                     (search ":committed-setting" script))
                "self.commit writes a complete executable replay script")
               (test-assert
                (uiop:subpathp (image-commit-manifest-pathname committed)
                               (configuration-data-root configuration))
                "self.commit writes only beneath private Autolith data")
               (test-assert
                (uiop:subpathp
                 (configuration-current-image-commit-path configuration)
                 (configuration-state-root configuration))
                "self.commit selects its result beneath private Autolith state")
               (test-assert (string= head-before head-after)
                            "self.commit never changes workspace Git history")
               (test-assert
                (search "A user-made repository change."
                        (uiop:read-file-string source-pathname))
                "self.commit leaves tracked workspace changes untouched"))
             (test-assert
              (null (image-commit-pending-records configuration))
              "self.commit consumes every successful staged mutation")
             (let* ((committed (image-commit-current configuration))
                    (identifier (image-commit-identifier committed))
                    (canonical-directory
                      (image-commit-directory committed))
                    (history-directory
                      (image-history--artifact-directory
                       configuration identifier)))
               (uiop:delete-directory-tree
                history-directory :validate t :if-does-not-exist ':ignore)
               (uiop:delete-directory-tree
                canonical-directory :validate t :if-does-not-exist ':ignore)
               (setf (symbol-function 'test-self-target) previous-function
                     *test-self-setting* :baseline
                     *image-state-initialized-p* nil
                     *active-image-commit-identifier* nil
                     *active-image-history-commit* nil
                     *active-image-lineage-identifier* nil)
               (test-assert (null (image-state-load configuration))
                            "private startup replay loads without failures")
               (test-assert
                (and (probe-file
                      (merge-pathnames "manifest.sexp" canonical-directory))
                     (probe-file
                      (merge-pathnames "reconstruct.lisp"
                                       canonical-directory)))
                "startup restores deleted replay artifacts from Git objects"))
             (test-assert (= (test-self-target) 86)
                          "startup replay reconstructs committed definitions")
             (test-assert (eq *test-self-setting* :committed-setting)
                          "startup replay reconstructs committed global state")
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     commit-tool
                     context
                     (json-object "title" "Commit nothing"))
                    nil)
                (image-commit-error ()
                  t))
              "self.commit refuses an empty private commit")
             (image-commit-publish
              configuration
              :title "Retain a failing v0.31.1 replay entry"
              :mutation-records nil
              :additional-entries
              (list (list :kind ':legacy
                          :id "legacy-broken-replay"
                          :target "broken-replay.lisp"
                          :source
                          "(error \"Injected private replay failure.\")")))
             (setf *image-state-initialized-p* nil
                   *active-image-commit-identifier* nil
                   *active-image-history-commit* nil
                   *active-image-lineage-identifier* nil)
             (let* ((failures (image-state-load configuration))
                    (application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :provider nil
                                     :tool-registry registry
                                     :worker nil
                                     :agent nil
                                     :ui nil)))
               (setf (application-mutation-replay-failures application)
                     failures)
               (test-assert
                (and (= (length failures) 1)
                     (search "Injected private replay failure."
                             (rest (first failures)))
                     (equal (application-mutation-replay-failures application)
                            failures))
                "selected private replay failures remain visible at startup"))))
      (setf (symbol-function 'test-self-target) previous-function
            *test-self-setting* previous-setting
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (when (fboundp 'test-self-new-rejected-definition)
        (fmakunbound 'test-self-new-rejected-definition))
      (remhash
       (definition-key '(defun test-self-new-rejected-definition () nil))
       *exploratory-definitions*)
      (let ((test-identifiers nil))
        (maphash
         (lambda (identifier mutation)
           (when (member
                  (durable-mutation-target mutation)
                  (list
                   (definition-key '(defun test-self-target () 0))
                   (definition-key
                    '(defun test-self-new-rejected-definition () nil)))
                  :test #'string=)
             (push identifier test-identifiers)))
         *durable-mutations*)
        (dolist (identifier test-identifiers)
          (remhash identifier *durable-mutations*)))
      (uiop:delete-directory-tree source-root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-durable-definition-publication-boundary () null)
(defun test-durable-definition-publication-boundary ()
  "Test a post-publication failure does not undo selected live definition state."
  (let* ((*package* (find-package '#:autolith))
         (source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-publication-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration (test-configuration-for-source-root source-root))
         (source-pathname (merge-pathnames "src/baseline.lisp" source-root))
         (definition-source
           "(defun test-self-published-definition () \"Remain live after publication.\" 23)")
         (definition (self-read-form definition-source :read-eval nil))
         (target (definition-key definition))
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*)
         (original-transition-function
           (symbol-function 'durable-mutation-transition))
         (mutation nil)
         (*image-commit-replay-probe-function*
           (lambda (checked-configuration script identifier)
             (declare (ignore checked-configuration script identifier))
             nil)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-pathname)
           (with-open-file (stream source-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream "(in-package #:autolith)~%"))
           (self-git-command configuration '("init" "--quiet"))
           (self-git-command configuration
                             '("config" "user.name" "Autolith Test"))
           (self-git-command
            configuration
            '("config" "user.email" "autolith-test@example.invalid"))
           (self-git-command configuration '("add" "src/baseline.lisp"))
           (self-git-command configuration
                             '("commit" "--quiet" "--no-gpg-sign" "-m" "Create baseline"))
           (when (fboundp 'test-self-published-definition)
             (fmakunbound 'test-self-published-definition))
           (remhash target *exploratory-definitions*)
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (image-state-load configuration)
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "publication-boundary"))
                  (context
                    (make-instance
                     'tool-context
                     :configuration configuration
                     :worker nil
                     :conversation conversation
                     :mutation-checker
                     (make-instance
                      'callback-mutation-checker
                      :active-callback
                      (lambda (checked-configuration checked-source)
                        (declare (ignore checked-configuration checked-source))
                        "active checks passed"))))
                  (tool (make-instance 'self-persist-definition-tool)))
             (setf (symbol-function 'durable-mutation-transition)
                   (lambda (checked-configuration checked-mutation phase
                            &key detail git-commit)
                     (let ((result
                             (funcall original-transition-function
                                      checked-configuration
                                      checked-mutation
                                      phase
                                      :detail detail
                                      :git-commit git-commit)))
                       (when (eq phase :source-written)
                         (error "Injected post-publication journal failure."))
                       result)))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute
                     tool
                     context
                     (json-object "definition" definition-source))
                    nil)
                (error ()
                  t))
              "an error after image publication still escapes the tool")
             (setf (symbol-function 'durable-mutation-transition)
                   original-transition-function))
           (setf mutation
                 (loop for candidate being the hash-values
                         of *durable-mutations*
                       when (string= target
                                     (durable-mutation-target candidate))
                         return candidate))
           (test-assert mutation
                        "post-publication failure retains its mutation state")
           (test-assert
            (and (fboundp 'test-self-published-definition)
                 (= (test-self-published-definition) 23))
            "post-publication failure preserves the selected live definition")
           (test-assert
            (string= (gethash target *exploratory-definitions*)
                     definition-source)
            "post-publication failure preserves selected exploratory source")
           (test-assert
            (image-commit-contains-mutation-p
             configuration
             (durable-mutation-identifier mutation))
            "post-publication failure leaves the selected replay commit intact")
           (test-assert
            (eq (durable-mutation-phase mutation) :source-written)
            "post-publication journal progress remains reconcilable")
           (durable-mutations-reconcile configuration)
           (test-assert
            (eq (durable-mutation-phase mutation) :durable)
            "reconciliation completes the selected post-publication mutation"))
      (setf (symbol-function 'durable-mutation-transition)
            original-transition-function
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (when (fboundp 'test-self-published-definition)
        (fmakunbound 'test-self-published-definition))
      (remhash target *exploratory-definitions*)
      (when mutation
        (remhash (durable-mutation-identifier mutation) *durable-mutations*))
      (uiop:delete-directory-tree source-root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-self-tuning-experiments () null)
(defun test-self-tuning-experiments ()
  "Test explicit append-only tuning experiments and their mutation gates."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (previous-setting *test-discard-setting*)
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "self-tuning-experiment"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation))
                (registry (make-default-tool-registry))
                (set-tool (tool-registry-find registry "self" "set"))
                (start-tool
                  (tool-registry-find registry "self" "experiment-start"))
                (settle-tool
                  (tool-registry-find registry "self" "experiment-settle"))
                (exercise-tool (tool-registry-find registry "self" "exercise"))
                (status-tool (tool-registry-find registry "self" "status")))
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (image-state-load configuration)
           (tool-execute set-tool context
                         (json-object "symbol" "*test-discard-setting*"
                                      "value" ":experiment"))
           (test-assert (null (tuning-experiment-open configuration))
                        "ordinary exploratory mutations are not tuning experiments")
           (let* ((mutation
                    (getf (rest (first
                                 (image-commit-effective-diff-records
                                  configuration)))
                          :id))
                  (start
                    (tool-execute
                     start-tool context
                     (json-object
                      "mutation" mutation
                      "hypothesis" "A focused setting improves behavior."
                      "criterion" "The focused assertion passes.")))
                  (experiment (tuning-experiment-open configuration))
                  (identifier (tuning-experiment-identifier experiment)))
             (test-assert
              (and (tool-result-success-p start)
                   (string= mutation
                            (tuning-experiment-mutation-identifier experiment))
                   (string= "A focused setting improves behavior."
                            (tuning-experiment-hypothesis experiment))
                   (string= "The focused assertion passes."
                            (tuning-experiment-criterion experiment)))
              "tuning experiment fields round-trip through the journal")
             (test-assert
              (search identifier
                      (tool-result-content
                       (tool-execute status-tool context (json-object))))
              "self.status surfaces the open tuning experiment")
             (test-assert
              (handler-case
                  (progn
                    (tool-execute start-tool context
                                  (json-object
                                   "mutation" mutation
                                   "hypothesis" "Second hypothesis"
                                   "criterion" "Second criterion"))
                    nil)
                (source-mutation-error () t))
              "a second open tuning experiment is rejected")
             (test-assert
              (handler-case
                  (progn
                    (tool-execute set-tool context
                                  (json-object
                                   "symbol" "*test-self-setting*"
                                   "value" ":second"))
                    nil)
                (source-mutation-error () t))
              "an open experiment rejects a second exploratory mutation")
             (tool-execute
              exercise-tool context
              (json-object
               "form" "(assert (eq *test-discard-setting* :experiment))"
               "mutation" mutation))
             (test-assert
              (find identifier
                    (mutation-journal-read-records configuration)
                    :test #'string=
                    :key (lambda (record)
                           (and (eq (first record) :mutation)
                                (eq (getf (rest record) :kind) :exercise)
                                (eq (getf (rest record) :result) :passed)
                                (getf (rest record) :experiment))))
              "self.exercise evidence is associated with the open experiment")
             (let ((result
                     (tool-execute
                      settle-tool context
                      (json-object "experiment" identifier
                                   "verdict" "too-early"
                                   "observation" "One sample is insufficient."))))
               (test-assert
                (and (search "keeps this experiment open"
                             (tool-result-content result))
                     (eq (tuning-experiment-verdict
                          (tuning-experiment-open configuration))
                         :too-early))
                "too-early records evidence and leaves the experiment open"))
             (let* ((long-observation (make-string 2500 :initial-element #\x))
                    (result
                      (tool-execute
                       settle-tool context
                       (json-object "verdict" "better"
                                    "observation" long-observation)))
                    (settled (find identifier
                                   (tuning-experiment-list configuration)
                                   :key #'tuning-experiment-identifier
                                   :test #'string=)))
               (test-assert
                (and (null (tuning-experiment-open configuration))
                     (eq (tuning-experiment-verdict settled) :better)
                     (<= (length (tuning-experiment-observation settled))
                         *tuning-experiment-text-limit*)
                     (search "Commit mutation" (tool-result-content result)))
                "better is terminal, bounded, and guides commit"))
             (dolist (case '(("worse" "Discard mutation")
                             ("unchanged" "Discard mutation")))
               (let* ((new
                        (tool-execute
                         start-tool context
                         (json-object "mutation" mutation
                                      "hypothesis" "Retest the same change."
                                      "criterion" "Compare another sample.")))
                      (new-identifier
                        (tuning-experiment-identifier
                         (tuning-experiment-open configuration))))
                 (declare (ignore new))
                 (let ((result
                         (tool-execute
                          settle-tool context
                          (json-object "experiment" new-identifier
                                       "verdict" (first case)
                                       "observation" "No useful gain."))))
                   (test-assert
                    (and (null (tuning-experiment-open configuration))
                         (search (second case) (tool-result-content result)))
                    "worse and unchanged are terminal and guide discard"))))
             (with-open-file (stream (configuration-journal-path configuration)
                                     :direction ':output
                                     :if-exists ':append
                                     :external-format ':utf-8)
               (write-string "(:mutation :kind :tuning-experiment" stream))
             (test-assert
              (= (length (tuning-experiment-list configuration)) 3)
              "experiment replay tolerates an incomplete journal tail")))
      (setf *test-discard-setting* previous-setting
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (clrhash *exploratory-undo-actions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-self-replay-foreign-home () null)
(defun test-self-replay-foreign-home ()
  "Test replay skips stale definitions whose names moved to a library."
  (let ((*image-replay-skipped-definitions* nil)
        (original (symbol-function
                   'provider--call-with-transport-normalization)))
    (test-assert
     (null (self-replay-definition
            "AUTOLITH"
            "(defun provider--call-with-transport-normalization (attempt-function) attempt-function)"))
     "a library-owned definition is not installed")
    (test-assert
     (eq original (symbol-function
                   'provider--call-with-transport-normalization))
     "the library definition survives the stale replay")
    (test-assert
     (let ((message (first *image-replay-skipped-definitions*)))
       (and message
            (search "provider--call-with-transport-normalization" message)
            (search "CL-LLM-PROVIDER-API" message)))
     "the skip names the definition and its owning package"))
  (let ((*image-replay-skipped-definitions* nil))
    (unwind-protect
         (progn
           (self-replay-definition
            "AUTOLITH"
            "(defun self-replay-home-test-function () :installed)")
           (test-assert
            (eq (funcall (symbol-function
                          (find-symbol "SELF-REPLAY-HOME-TEST-FUNCTION"
                                       '#:autolith)))
                ':installed)
            "definitions home in the target package still replay")
           (test-assert (null *image-replay-skipped-definitions*)
                        "home definitions are not reported as skipped"))
      (let ((symbol (find-symbol "SELF-REPLAY-HOME-TEST-FUNCTION"
                                 '#:autolith)))
        (when symbol
          (fmakunbound symbol)
          (unintern symbol '#:autolith)))))
  (let ((*image-replay-skipped-definitions* nil))
    (test-assert
     (null (self-replay-definition
            "AUTOLITH"
            "(defparameter #:uninterned-replay-target (error \"must not install\"))"
            :home-package nil))
     "an uninterned target is skipped even with explicit ownership metadata")
    (test-assert *image-replay-skipped-definitions*
                 "an uninterned target reports its missing replay identity"))
  (test-assert
   (not (definition-foreign-home-p
         (list 'defmethod 'print-object nil)
         (find-package '#:autolith)))
   "methods on foreign generics stay replayable")
  nil)


(-> test-self--foreign-definition (keyword symbol keyword) list)
(defun test-self--foreign-definition (kind symbol value)
  "Build a fixture definition of KIND under foreign SYMBOL returning VALUE."
  (ecase kind
    (:function
     `(defun ,symbol () ',value))
    (:setf
     `(defun (setf ,symbol) (value object)
        (declare (ignore value object)) ',value))
    (:variable
     `(defparameter ,symbol ',value))
    (:method
     `(defmethod ,symbol ((object t)) (declare (ignore object)) ',value))))

(-> test-self-foreign-definition-lifecycle () null)
(defun test-self-foreign-definition-lifecycle ()
  "Test foreign definitions across installation, discard, replay and failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (package (find-package '#:sb-ext))
         (*image-state-initialized-p* nil)
         (*active-image-commit-identifier* nil)
         (*active-image-history-commit* nil)
         (*active-image-lineage-identifier* nil)
         (*exploratory-definitions* (make-hash-table :test #'equal))
         (*exploratory-undo-actions* (make-hash-table :test #'equal))
         (*image-replay-skipped-definitions* nil))
    (unwind-protect
         (progn
           (image-state-load configuration)
           (dolist (kind '(:function :setf :variable :method))
             (let* ((symbol
                      (self-call-with-package-unlocked
                       package
                       (lambda ()
                         (intern (format nil "AUTOLITH-FOREIGN-~A" (make-identifier))
                                 package))))
                    (baseline (test-self--foreign-definition kind symbol ':original))
                    (replacement (test-self--foreign-definition kind symbol ':replacement))
                    (source (write-to-string replacement :readably t))
                    (script (merge-pathnames "foreign-replay.lisp" root)))
               (flet ((observe ()
                        (ecase kind
                          (:function
                           (funcall symbol))
                          (:setf
                           (funcall (fdefinition (list 'setf symbol)) nil nil))
                          (:variable
                           (symbol-value symbol))
                          (:method
                           (funcall symbol nil)))))
                 (unwind-protect
                      (progn
                        (self-call-with-package-unlocked package (lambda () (eval baseline)))
                        (self-install-definition configuration source)
                        (test-assert (eq (observe) ':replacement)
                                     "a foreign target is replaced from the Autolith reader package")
                        (test-assert (sb-ext:package-locked-p package)
                                     "installation restores the foreign package lock")
                        (let* ((record (first (image-commit-effective-pending-records configuration)))
                               (entry (image-commit--record->entry record)))
                          (test-assert (equal (getf entry :home-package) "SB-EXT")
                                       "replay entries retain the mutation's actual owner")
                          (image-commit-write-script script :identifier "foreign-replay"
                                                     :title "Replay foreign definition"
                                                     :entries (list entry)))
                        (self-discard-mutation configuration nil)
                        (test-assert (eq (observe) ':original)
                                     "discard restores a locked foreign target")
                        (load script)
                        (test-assert (eq (observe) ':replacement)
                                     "the rendered script reinstalls the intentional foreign definition")
                        (test-assert (null *image-replay-skipped-definitions*)
                                     "intentional foreign replacements are not skipped")
                        (test-assert (sb-ext:package-locked-p package)
                                     "discard and replay restore the foreign package lock")
                        (unless (eq kind ':method)
                          (let ((*image-replay-skipped-definitions* nil))
                            (self-replay-definition
                             "AUTOLITH"
                             (write-to-string (test-self--foreign-definition kind symbol ':wrong))
                             :home-package "AUTOLITH")
                            (test-assert (and *image-replay-skipped-definitions*
                                              (eq (observe) ':replacement))
                                         "recorded ownership changes skip stale replacements")))
                        (when (eq kind ':method)
                          (let* ((*image-commit-replay-probe-function*
                                   (lambda (checked-configuration pathname identifier)
                                     (declare (ignore checked-configuration identifier))
                                     (load pathname)
                                     nil))
                                 (context (make-instance 'tool-context :configuration configuration))
                                 (tool (tool-registry-find (make-default-tool-registry)
                                                           "self" "persist-definition"))
                                 (result
                                   (tool-execute
                                    tool context
                                    (json-object
                                     "definition"
                                     (write-to-string
                                      (test-self--foreign-definition kind symbol ':persisted))))))
                            (test-assert (and (tool-result-success-p result)
                                              (eq (observe) ':persisted))
                                         "direct persistence installs and replays foreign methods")
                            (let* ((commit (image-commit-current configuration))
                                   (entry (first (image-commit-base-entries configuration))))
                              (test-assert (equal "SB-EXT" (getf entry :home-package))
                                           "direct persistence records the foreign target owner")
                              (self-call-with-package-unlocked package (lambda () (eval baseline)))
                              (load (image-commit-script-pathname commit))
                              (test-assert (and (eq (observe) ':persisted)
                                                (sb-ext:package-locked-p package))
                                           "the selected private script restores the foreign method and lock"))))
                        (when (eq kind ':variable)
                          (test-assert
                           (handler-case
                               (progn
                                 (self-install-definition
                                  configuration
                                  (format nil "(defparameter ~S (error ~S))"
                                          symbol "expected foreign installation failure"))
                                 nil)
                             (error () t))
                           "failed foreign installation propagates its condition")
                          (test-assert (and (eq (observe) ':replacement)
                                            (sb-ext:package-locked-p package))
                                       "failed installation restores value and lock")
                          (let* ((context (make-instance 'tool-context :configuration configuration))
                                 (tool (tool-registry-find (make-default-tool-registry) "self" "set")))
                            (tool-execute tool context
                                          (json-object "symbol" (write-to-string symbol)
                                                       "value" ":set"))
                            (test-assert (eq (observe) ':set) "self.set accepts foreign variables")
                            (self-discard-mutation configuration nil)
                            (test-assert (eq (observe) ':replacement)
                                         "self.set discard restores foreign variables"))))
                   (self-call-with-package-unlocked
                    package
                    (lambda ()
                      (when (fboundp symbol)
                        (fmakunbound symbol))
                      (when (fboundp (list 'setf symbol))
                        (fmakunbound (list 'setf symbol)))
                      (when (boundp symbol)
                        (makunbound symbol))
                      (unintern symbol package))))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
