(in-package #:autolith)

;;;; -- Active Image Inspection --

(defvar *exploratory-definitions* (make-hash-table :test #'equal)
  "Complete source forms installed exploratorily in the active image.")

(defvar *exploratory-undo-actions* (make-hash-table :test #'equal)
  "Exact in-memory undo actions for this process's pending mutations.")

(-> self-resolve-package ((option string)) package)
(defun self-resolve-package (name)
  "Return existing package NAME, defaulting to the AUTOLITH package.

NAME is accepted as written or upcased, so lowercase names resolve too."
  (let ((package (if (non-empty-string-p name)
                     (package-find name)
                     (find-package '#:autolith))))
    (unless package
      (error 'source-mutation-error
             :message (format nil "No active Common Lisp package is named ~S."
                              name)
             :tool-name "self.redefine"
             :pathname nil))
    package))

(-> self-read-form
    (string &key (:read-eval boolean) (:package package))
    t)
(defun self-read-form
    (source &key (read-eval t) (package (find-package '#:autolith)))
  "Read exactly one Common Lisp form from SOURCE relative to PACKAGE.

Accept AUTOLITH: references to internal symbols using the reader's continuation.
A source without a form, or ending inside one, signals END-OF-FILE."
  (handler-bind
      ((package-error
         (lambda (condition)
           (when (and (typep condition 'reader-error)
                      (eq (find-package (package-error-package condition))
                          (find-package '#:autolith)))
             (let ((restart (find-restart 'continue condition)))
               (when restart
                 (invoke-restart restart)))))))
    (read-one-form source :read-eval read-eval :package package)))

(-> self-resolve-symbol (string &key (:package package)) symbol)
(defun self-resolve-symbol (name &key (package (find-package '#:autolith)))
  "Resolve readable symbol NAME relative to PACKAGE.

A quoted or function-quoted symbol, as in 'name or #'name, resolves to the
symbol itself."
  (resolve-symbol name :package package))

(-> self-inspect-symbol (symbol) string)
(defun self-inspect-symbol (symbol)
  "Return structured documentation and description for active SYMBOL."
  (with-output-to-string (stream)
    (format stream "Symbol: ~S~%Package: ~A~%"
            symbol
            (or (and (symbol-package symbol)
                     (package-name (symbol-package symbol)))
                "uninterned"))
    (when (fboundp symbol)
      (format stream "Function binding: yes~%Lambda list: ~S~%Documentation: ~A~%"
              (symbol-lambda-list symbol)
              (or (documentation symbol 'function) "none")))
    (when (boundp symbol)
      (format stream "Value binding: yes~%Value: ~A~%Documentation: ~A~%"
              (bounded-string (symbol-value symbol) :limit 2000)
              (or (documentation symbol 'variable) "none")))
    (let ((class (find-class symbol nil)))
      (when class
        (closer-mop:finalize-inheritance class)
        (format stream "Class binding: yes~%Class documentation: ~A~%Slots:~%"
                (or (documentation symbol 'type) "none"))
        (dolist (slot (closer-mop:class-slots class))
          (format stream "  ~S~@[ - ~A~]~%"
                  (closer-mop:slot-definition-name slot)
                  (documentation slot t)))))
    (format stream "~%Describe:~%")
    (describe symbol stream)))

(defmethod lisp-describe-active-image
    ((context tool-context) (arguments hash-table))
  "Inspect one required symbol in CONTEXT's active image."
  (declare (ignore context))
  (let* ((package (self-resolve-package (tool-argument arguments "package")))
         (symbol (self-resolve-symbol
                  (tool-argument arguments "designator" :required t)
                  :package package)))
    (unless (or (symbol-defined-p symbol)
                (keywordp symbol))
      (error 'tool-error
             :message
             (format nil "~S has no function, variable, class, or type definition in the active image.~A Find the exact name with lisp.apropos or search.content instead of guessing."
                     symbol
                     (self-symbol-suggestion-text (symbol-name symbol)
                                                  :package package))
             :tool-name "lisp.describe"))
    (tool-success (self-inspect-symbol symbol))))


;;;; -- Mutation Journal --

(defvar *live-mutation-lock* (make-recursive-lock "Autolith live mutation")
  "The process-wide lock serializing active-image and durable mutations.")

(defvar *active-image-lineage-identifier* nil
  "The journal lineage receiving mutations from the running image branch.")

(defvar *image-state-initialized-p* nil
  "True after startup selected the image commit represented by this heap.")

(defvar *active-image-commit-identifier* nil
  "The private image commit represented by the running image, or NIL for base.")

(defvar *active-image-history-commit* nil
  "The private Git commit backing the running image commit, or NIL for legacy state.")

(defmacro with-live-mutation (&body body)
  "Evaluate BODY while excluding checkpoints and other live mutations."
  `(with-recursive-lock-held (*live-mutation-lock*)
     ,@body))

(-> mutation-journal-append (configuration list) list)
(defun mutation-journal-append (configuration record)
  "Append portable mutation RECORD, repairing an interrupted journal tail."
  (let ((pathname (configuration-journal-path configuration))
        (entry (list* (first record)
                      :time (get-universal-time)
                      (rest record))))
    (ensure-directories-exist pathname)
    (log-append pathname entry)
    entry))


;;;; -- Exploratory Evaluation --

(-> self-capture-evaluation (function) (values list string))
(defun self-capture-evaluation (function)
  "Call FUNCTION in the active image while capturing output and rendered values."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*package* (find-package '#:autolith)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'sbcl-worker-render-value result-values) output))))

(-> self-evaluation-result (list string) string)
(defun self-evaluation-result (result-values output)
  "Render active-image RESULT-VALUES and captured OUTPUT for a tool result."
  (with-output-to-string (stream)
    (when (non-empty-string-p output)
      (format stream "Output:~%~A~%" output))
    (format stream "Values:~%~{~A~%~}" result-values)))


;;;; -- Restart Selection --

(-> self--correctable-message (condition list) string)
(defun self--correctable-message (condition restarts)
  "Describe CONDITION and its RESTARTS together with retry instructions."
  (format nil
          "~A~2%Available restarts:~%~{~A~%~}~
           Retry the identical call adding \"restart\": \"NAME\" to invoke ~
           one, and add \"restart-value\" with a value form when the ~
           restart consumes a value."
          condition
          (loop for (name . report) in restarts
                collect (format nil "  ~A  ~A" name report))))

(-> self-call-with-restarts
    (function &key (:restart-name (option string))
              (:restart-value-source (option string)))
    t)
(defun self-call-with-restarts (thunk &key restart-name restart-value-source)
  "Call THUNK, invoking the chosen restart or describing the available ones.

With RESTART-NAME, the first matching non-ABORT restart is invoked while the
signaling operation is still live, optionally passing the evaluated
RESTART-VALUE-SOURCE. Without a match, a condition that offers selectable
restarts becomes a SELF-CORRECTABLE-ERROR whose report teaches the retry
protocol."
  (handler-case
      (call-with-restart-choice
       thunk
       :restart-name           restart-name
       :restart-value-function (and (non-empty-string-p restart-value-source)
                                    (lambda ()
                                      (eval (self-read-form restart-value-source)))))
    (restart-choice-available (available)
      (let ((condition (restart-choice-available-condition available))
            (choices   (restart-choice-available-choices available)))
        (error 'self-correctable-error
               :message       (self--correctable-message condition choices)
               :condition     condition
               :choices       choices
               :restart-names (mapcar #'first choices))))))

(-> self-evaluate-forms (list function) t)
(defun self-evaluate-forms (sources position-function)
  "Read and evaluate each of SOURCES in turn, returning the last form's values.

Each form is read only after the previous one ran, so it may name packages an
earlier form created. POSITION-FUNCTION receives each form's one-based index
before that form is read."
  (loop for source in sources
        for index from 1
        for result = (progn
                       (funcall position-function index)
                       (multiple-value-list (eval (self-read-form source))))
        finally (return (values-list result))))

(defmethod tool-execute ((tool self-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate exploratory forms in order in CONTEXT's active image and journal them."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((sources (tool-forms-argument arguments "self.eval"))
           (source (format nil "~{~A~^~%~}" sources))
           (restart-name (tool-argument arguments "restart"))
           (restart-value-source (tool-argument arguments "restart-value"))
           (configuration (tool-context-configuration context))
           (position 0))
      (mutation-journal-append
       configuration
       (list :mutation :kind :eval :proposed source :result ':pending))
      (handler-case
          (multiple-value-bind (result-values output)
              (self-capture-evaluation
               (lambda ()
                 (self-call-with-restarts
                  (lambda ()
                    (self-evaluate-forms sources
                                         (lambda (index) (setf position index))))
                  :restart-name restart-name
                  :restart-value-source restart-value-source)))
            (mutation-journal-append
             configuration
             (list :mutation :kind :eval :proposed source :result ':installed))
            (tool-success (self-evaluation-result result-values output)))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :eval
                 :proposed source
                 :result ':failed
                 :condition (princ-to-string condition)))
          (error (self--positioned-failure condition position (length sources))))))))

(-> self--positioned-failure (error integer integer) error)
(defun self--positioned-failure (condition position count)
  "Return CONDITION naming form POSITION of COUNT when several forms ran.

A correctable failure keeps its class and restart choices, so the restart menu
still applies; any other failure becomes a TOOL-ERROR keeping its failure code."
  (let ((prefix (format nil "Form ~D of ~D failed: " position count)))
    (cond
      ((= count 1)
       condition)
      ((typep condition 'self-correctable-error)
       (make-condition 'self-correctable-error
                       :message       (concatenate 'string prefix
                                                   (autolith-error-message condition))
                       :condition     (restart-choice-available-condition condition)
                       :choices       (restart-choice-available-choices condition)
                       :restart-names (self-correctable-error-restart-names condition)))
      (t
       (make-condition 'tool-error
                       :message   (format nil "~A~A" prefix condition)
                       :tool-name "self.eval"
                       :code      (tool-failure-code condition))))))


;;;; -- Definition Installation --

(defmethod definition-operator-p ((operator (eql 'define-context-contributor)))
  "Accept context contributor definitions."
  t)

(defmethod definition-operator-p ((operator (eql 'define-application-command)))
  "Accept application command definitions."
  t)

(defclass image-replay-skip ()
  ((definition
    :initarg :definition
    :reader image-replay-skip-definition
    :type list
    :documentation "The parsed persisted definition that was not installed.")
   (key
    :initarg :key
    :reader image-replay-skip-key
    :type string
    :documentation "The definition key computed in the replay's reader package.")
   (source
    :initarg :source
    :reader image-replay-skip-source
    :type string
    :documentation "The complete persisted source of the skipped definition.")
   (tracked
    :initarg :tracked
    :reader image-replay-skip-tracked
    :type (option string)
    :documentation "The tracked source recorded at publication, when the entry recorded one.")
   (tracked-recorded-p
    :initarg :tracked-recorded-p
    :reader image-replay-skip-tracked-recorded-p
    :type boolean
    :documentation "Whether the replay entry recorded its tracked base at all.")
   (reason
    :initarg :reason
    :reader image-replay-skip-reason
    :type (member :owner-moved :uninterned-target :tracked-changed
                  :tracked-removed :tracked-appeared :revision-moved)
    :documentation "Why the definition was skipped.")
   (message
    :initarg :message
    :reader image-replay-skip-message
    :type string
    :documentation "The complete sentence explaining the skip."))
  (:documentation "One stale persisted definition a private replay left uninstalled."))

(defvar *image-replay-skips* nil
  "The IMAGE-REPLAY-SKIP records of the running image's private replay.")

(-> definition-foreign-home-p
    (list package &key (:home-package (option string))) boolean)
(defun definition-foreign-home-p
    (definition package &key (home-package (package-name package)))
  "Return true when DEFINITION's recorded owner is missing or has changed.
Legacy records assume the reader PACKAGE was the owner. Methods extend their
generic function and are not skipped merely because its ownership changed."
  (and (not (eq (first definition) 'defmethod))
       (or (null home-package)
           (not (equal (definition-home-package-name definition) home-package)))))

(-> self-previous-definition (configuration list) (option string))
(defun self-previous-definition (configuration definition)
  "Return complete reconstructible source preceding DEFINITION, when known."
  (let ((target (definition-key definition)))
    (or (gethash target *exploratory-definitions*)
        (and (fboundp 'image-commit-definition-source)
             (funcall (symbol-function 'image-commit-definition-source)
                      configuration
                      target))
        (and (fboundp 'durable-mutation--fallback-source)
             (funcall (symbol-function 'durable-mutation--fallback-source)
                      configuration
                      definition)))))

(defmethod definition-undo-capture ((operator (eql 'define-context-contributor))
                                    definition package &key previous-source)
  "Restore the contributor function and its registration."
  (declare (ignore package previous-source))
  (let* ((name         (second definition))
         (identifier   (context--definition-identifier name))
         (registration (context--registration-snapshot identifier))
         (binding      (function-binding-snapshot name)))
    (lambda ()
      (function-binding-restore name binding)
      (context--registration-restore identifier registration))))

(defmethod definition-undo-capture ((operator (eql 'define-application-command))
                                    definition package &key previous-source)
  "Restore the command function and its runtime registration."
  (declare (ignore package previous-source))
  (let* ((name         (second definition))
         (registration (application-command--registration-snapshot name ':runtime))
         (binding      (function-binding-snapshot name)))
    (lambda ()
      (function-binding-restore name binding)
      (application-command--registration-restore name ':runtime registration))))

(-> self--definition-undo-action (list (option string) package) function)
(defun self--definition-undo-action (definition previous-source package)
  "Return an exact undo action for installing DEFINITION in PACKAGE."
  (handler-case
      (definition-undo-capture (first definition) definition package
                               :previous-source previous-source)
    (definition-irreversible (condition)
      (error 'source-mutation-error
             :message   (surgeon-error-message condition)
             :tool-name "self.redefine"
             :pathname  nil))))

(-> self--definition-state-undo-action
    (list (option string) package)
    function)
(defun self--definition-state-undo-action
    (definition previous-source package)
  "Return an undo action for DEFINITION's live binding and source cache."
  (let ((target (let ((*package* package))
                  (definition-key definition)))
        (binding-undo
          (self--definition-undo-action definition previous-source package)))
    (multiple-value-bind (cached-source cached-source-p)
        (gethash target *exploratory-definitions*)
      (lambda ()
        (call-with-definition-unlocked definition package binding-undo)
        (if cached-source-p
            (setf (gethash target *exploratory-definitions*) cached-source)
            (remhash target *exploratory-definitions*))
        nil))))

(-> self--undo-failed-definition-installation
    (function serious-condition)
    null)
(defun self--undo-failed-definition-installation
    (undo-action original-condition)
  "Run UNDO-ACTION or signal corruption retaining both failure conditions."
  (handler-case
      (funcall undo-action)
    (error (restoration-condition)
      (error 'active-image-corruption
             :message
             "A failed definition mutation could not restore the active image."
             :original-condition original-condition
             :restoration-condition restoration-condition)))
  nil)

(-> self--install-definition (list string &key (:package package)) t)
(defun self--install-definition
    (definition source &key (package (find-package '#:autolith)))
  "Compile and install parsed DEFINITION in PACKAGE, retaining complete SOURCE."
  (call-with-definition-unlocked
   definition package
   (lambda ()
     (let* ((*package* package)
            (result (eval definition)))
       (setf (gethash (definition-key definition) *exploratory-definitions*)
             source)
       result))))

(defclass image-replay-context ()
  ((configuration
    :initarg :configuration
    :reader image-replay-context-configuration
    :type configuration
    :documentation "The configuration whose tracked source root the replay checks against.")
   (lineage-source-commit
    :initarg :lineage-source-commit
    :reader image-replay-context-lineage-source-commit
    :type (option string)
    :documentation "The tracked revision the replayed commit lineage was published against.")
   (image-source-commit
    :initarg :image-source-commit
    :reader image-replay-context-image-source-commit
    :type (option string)
    :documentation "The tracked revision the running image was built from."))
  (:documentation
   "The tracked-source facts one private replay checks its definitions against."))

(defvar *image-replay-context* nil
  "The IMAGE-REPLAY-CONTEXT of the private replay in progress, or NIL outside one.")

(-> self--definition-sources-equal-p
    ((option string) (option string) package)
    boolean)
(defun self--definition-sources-equal-p (recorded current package)
  "Return true when RECORDED and CURRENT sources read as the same form in PACKAGE.

Two absent sources are equal; formatting and comments do not matter."
  (cond
    ((and (null recorded) (null current))
     t)
    ((or (null recorded) (null current))
     nil)
    (t
     (handler-case
         (and (equal (self-read-form recorded :read-eval nil :package package)
                     (self-read-form current :read-eval nil :package package))
              t)
       (error ()
         (and (string= recorded current) t))))))

(-> self--replay-stale-reason
    (list package (option string) boolean)
    (values (option keyword) (option string)))
(defun self--replay-stale-reason (definition package tracked tracked-p)
  "Return why replaying DEFINITION would shadow a moved tracked definition.

The values are the reason keyword and its explanation, or NIL twice when the
definition may replay. TRACKED is the tracked source recorded when the
definition was published and TRACKED-P whether its entry recorded one at all.
An entry without a record is judged by revision: it is stale when the lineage
was published against another tracked revision than the running image and a
tracked definition exists now. Outside a replay context, or when the tracked
source cannot be read, nothing can be checked and the definition replays."
  (let ((context *image-replay-context*))
    (when context
      (multiple-value-bind (current known-p)
          (handler-case
              (values (self-tracked-definition-source
                       (image-replay-context-configuration context)
                       definition)
                      t)
            (error ()
              (values nil nil)))
        (when known-p
          (if tracked-p
              (unless (self--definition-sources-equal-p tracked current package)
                (cond
                  ((null current)
                   (values ':tracked-removed
                           "its tracked definition was removed since the private commit was published. Reapply the definition to keep it."))
                  ((null tracked)
                   (values ':tracked-appeared
                           "a tracked definition now exists where the private commit recorded none. Reapply the definition to replace the tracked one."))
                  (t
                   (values ':tracked-changed
                           "its tracked definition changed since the private commit was published. Reapply the definition against the current source."))))
              (let ((lineage (image-replay-context-lineage-source-commit context))
                    (image (image-replay-context-image-source-commit context)))
                (when (and lineage image current (string/= lineage image))
                  (values ':revision-moved
                          (format nil
                                  "its tracked definition may have changed: the private commit lineage was published against source ~A and this image runs ~A. Reapply the definition against the current source."
                                  lineage
                                  image))))))))))

(-> self-replay-definition
    (string string &key (:home-package (option string))
                        (:tracked (option string)))
    t)
(defun self-replay-definition
    (package-name source &key (home-package package-name) (tracked nil tracked-p))
  "Read and install persisted SOURCE in PACKAGE-NAME during reconstruction.
HOME-PACKAGE records the target's owner when the mutation was made. Intentional
foreign definitions replay; stale definitions whose ownership moved are skipped.
Legacy two-argument records assume their reader package owned the target.
TRACKED records the tracked source the definition shadowed when its commit was
published, or NIL when it introduced a new name. A definition whose tracked
base moved since then is skipped and reported, so the tracked system stays
authoritative; entries without the record are judged by source revision."
  (let* ((package (self-resolve-package package-name))
         (definition
           (self-read-form source :read-eval nil :package package)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "A private image commit contains an invalid definition."
             :tool-name "self.commit"
             :pathname nil))
    (multiple-value-bind (reason explanation)
        (if (definition-foreign-home-p definition package
                                       :home-package home-package)
            (let ((home (symbol-package (definition-name-symbol
                                         (second definition)))))
              (if home
                  (values ':owner-moved
                          (format nil "its owner changed from ~A to ~A. Reapply the definition to authorize its new owner."
                                  (or home-package "an uninterned symbol")
                                  (package-name home)))
                  (values ':uninterned-target
                          "an uninterned target has no stable replay identity.")))
            (self--replay-stale-reason definition package tracked tracked-p))
      (if reason
          (progn
            (push (make-instance
                   'image-replay-skip
                   :definition definition
                   :key (let ((*package* package))
                          (definition-key definition))
                   :source source
                   :tracked tracked
                   :tracked-recorded-p tracked-p
                   :reason reason
                   :message
                   (format nil
                           "The persisted ~(~A~) of ~(~A~) was skipped: ~A"
                           (first definition)
                           (definition-name-symbol (second definition))
                           explanation))
                  *image-replay-skips*)
            nil)
          (self--install-definition definition source :package package)))))

(-> self-restore-definition
    (string serious-condition
     &key (:installer function) (:package (option package)))
    t)
(defun self-restore-definition
    (previous-source original-condition
     &key (installer #'self--install-definition) package)
  "Restore PREVIOUS-SOURCE or signal compound active-image corruption."
  (handler-case
      (if package
          (funcall installer
                   (self-read-form previous-source
                                   :read-eval nil
                                   :package package)
                   previous-source
                   :package package)
          (funcall installer
                   (self-read-form previous-source :read-eval nil)
                   previous-source))
    (error (restoration-condition)
      (error 'active-image-corruption
             :message
             "A failed definition mutation could not restore the active image."
             :original-condition original-condition
             :restoration-condition restoration-condition))))

(-> self-install-definition
    (configuration string &key (:package package))
    t)
(defun self-install-definition
    (configuration source &key (package (find-package '#:autolith)))
  "Compile and install one exploratory SOURCE definition in PACKAGE."
  (with-live-mutation
    (let ((definition (self-read-form source
                                      :read-eval nil
                                      :package package))
          (package-name (package-name package)))
      (unless (definition-form-p definition)
        (error 'source-mutation-error
               :message "self.redefine accepts one complete supported definition."
               :tool-name "self.redefine"
               :pathname nil))
      (tuning-experiment-assert-mutation-installable configuration
                                                       "self.redefine")
      (let ((identifier (make-identifier))
            (home-package (definition-home-package-name definition))
            (key (definition-key definition))
            (previous (self-previous-definition configuration definition))
            (undo-action nil))
        (setf undo-action
              (self--definition-state-undo-action
               definition
               previous
               package))
        (mutation-journal-append
         configuration
         (list :mutation
               :kind :definition
               :id identifier
               :lineage *active-image-lineage-identifier*
               :target key
               :package package-name
               :home-package home-package
               :previous previous
               :proposed source
               :result ':pending))
        (handler-case
            (let ((result (self--install-definition definition
                                                    source
                                                    :package package)))
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind :definition
                     :id identifier
                     :lineage *active-image-lineage-identifier*
                     :target key
                     :package package-name
                     :home-package home-package
                     :previous previous
                     :proposed source
                     :result ':installed))
              (setf (gethash identifier *exploratory-undo-actions*)
                    undo-action)
              result)
          (error (condition)
            (let ((reported-condition condition))
              (handler-case
                  (self--undo-failed-definition-installation
                   undo-action
                   condition)
                (active-image-corruption (corruption)
                  (setf reported-condition corruption)))
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind :definition
                     :id identifier
                     :lineage *active-image-lineage-identifier*
                     :target key
                     :package package-name
                     :home-package home-package
                     :previous previous
                     :proposed source
                     :result ':failed
                     :condition (princ-to-string reported-condition)))
              (error reported-condition))))))))

(defmethod tool-execute ((tool self-redefine-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install one exploratory top-level definition in CONTEXT's active image."
  (declare (ignore tool))
  (let ((source (tool-argument arguments "definition" :required t))
        (package
          (self-resolve-package (tool-argument arguments "package"))))
    (self-call-with-restarts
     (lambda ()
       (self-install-definition (tool-context-configuration context)
                                source
                                :package package))
     :restart-name (tool-argument arguments "restart")
     :restart-value-source (tool-argument arguments "restart-value"))
    (tool-success
     (format nil "The definition was compiled and installed in package ~A."
             (package-name package)))))

(defmethod tool-execute ((tool self-set-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Set one active global binding after journaling its previous value."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((identifier (make-identifier))
           (symbol (self-resolve-symbol
                    (tool-argument arguments "symbol" :required t)))
           (value-source (tool-argument arguments "value" :required t))
           (configuration (tool-context-configuration context))
           (previous (and (boundp symbol)
                          (sbcl-worker-render-value (symbol-value symbol))))
           (previous-bound-p (boundp symbol))
           (previous-value (and (boundp symbol) (symbol-value symbol))))
       (tuning-experiment-assert-mutation-installable configuration "self.set")
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :set
             :id identifier
             :lineage *active-image-lineage-identifier*
             :target (write-to-string symbol)
             :previous previous
             :proposed value-source
             :result ':pending))
      (handler-case
          (let ((value (self-call-with-restarts
                        (lambda ()
                          (let ((evaluated (eval (self-read-form value-source))))
                            (setf (symbol-value symbol) evaluated)
                            evaluated))
                        :restart-name (tool-argument arguments "restart")
                        :restart-value-source (tool-argument arguments
                                                             "restart-value"))))
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :set
                   :id identifier
                   :lineage *active-image-lineage-identifier*
                   :target (write-to-string symbol)
                   :previous previous
                   :proposed value-source
                   :result ':installed))
            (setf (gethash identifier *exploratory-undo-actions*)
                  (lambda ()
                    (value-binding-restore symbol
                                           (list previous-bound-p previous-value))))
            (tool-success
             (format nil "~S is now ~A."
                     symbol
                     (sbcl-worker-render-value value))))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :set
                 :id identifier
                 :lineage *active-image-lineage-identifier*
                 :target (write-to-string symbol)
                 :previous previous
                 :proposed value-source
                 :result ':failed
                 :condition (bounded-string condition :limit 2000)))
          (error condition))))))


;;;; -- Form-Aware Source Persistence --

(defclass tracked-definition ()
  ((relative-pathname
    :initarg :relative-pathname
    :reader tracked-definition-relative-pathname
    :type non-empty-string
    :documentation "The definition file relative to Autolith's source root.")
   (source-form
    :initarg :source-form
    :reader tracked-definition-source-form
    :type source-form
    :documentation "The parsed definition form and its exact source span.")
   (source
    :initarg :source
    :reader tracked-definition-source
    :type string
    :documentation "The complete tracked source text of the definition."))
  (:documentation "One tracked top-level definition exposed for safe self inspection."))

(defclass tracked-definition-snapshot ()
  ((source-root
    :initarg :source-root
    :reader tracked-definition-snapshot-source-root
    :type pathname
    :documentation "The source root whose tracked definitions the snapshot holds.")
   (table
    :initform nil
    :accessor tracked-definition-snapshot--table
    :type (option hash-table)
    :documentation "Tracked definitions by name, read on first use.")
   (failure
    :initform nil
    :accessor tracked-definition-snapshot--failure
    :type (option condition)
    :documentation "The error that prevented reading the tracked source, if any."))
  (:documentation
   "Tracked definitions read once for a batch of lookups that leaves source unchanged."))

(defvar *tracked-definition-snapshot* nil
  "The TRACKED-DEFINITION-SNAPSHOT of the batch lookup in progress, or NIL.")

(defmacro with-tracked-definition-snapshot ((configuration) &body body)
  "Evaluate BODY while tracked definition lookups read CONFIGURATION's source once.

CONFIGURATION is evaluated once before BODY. An enclosing snapshot of the same
source root is reused. BODY must not edit tracked source, or later lookups in it
see the source as it was when first read."
  `(call-with-tracked-definition-snapshot ,configuration (lambda () ,@body)))

(-> call-with-tracked-definition-snapshot (configuration function) t)
(defun call-with-tracked-definition-snapshot (configuration function)
  "Call FUNCTION with one tracked definition snapshot of CONFIGURATION's source root."
  (let ((source-root (config :source-root configuration))
        (snapshot *tracked-definition-snapshot*))
    (if (and snapshot
             (uiop:pathname-equal (tracked-definition-snapshot-source-root snapshot)
                                  source-root))
        (funcall function)
        (let ((*tracked-definition-snapshot*
                (make-instance 'tracked-definition-snapshot
                               :source-root source-root)))
          (funcall function)))))

(-> self-source--tracked-definition
    (pathname string source-form &key (:root pathname) (:path-prefix string))
    tracked-definition)
(defun self-source--tracked-definition
    (pathname source source-form &key root (path-prefix ""))
  "Return the tracked definition SOURCE-FORM spans in PATHNAME's SOURCE text."
  (make-instance 'tracked-definition
                 :relative-pathname
                 (format nil "~A~A" path-prefix (enough-namestring pathname root))
                 :source-form source-form
                 :source (subseq source
                                 (source-form-start source-form)
                                 (source-form-end source-form))))

(-> self-source--definitions
    (list &key (:root pathname)
               (:package package)
               (:symbol symbol)
               (:path-prefix string))
    list)
(defun self-source--definitions
    (pathnames &key root package symbol (path-prefix ""))
  "Return SYMBOL definitions read from PATHNAMES relative to ROOT in PACKAGE."
  (loop for pathname in (sort (copy-list pathnames) #'string< :key #'namestring)
        for source = (uiop:read-file-string pathname)
        append
        (loop for source-form in (source-read-forms source :package package)
              for form = (source-form-form source-form)
              when (and (definition-form-p form)
                        (equal (second form) symbol))
                collect
                (self-source--tracked-definition pathname source source-form
                                                 :root        root
                                                 :path-prefix path-prefix))))

(-> self-source--definition-table (list &key (:root pathname) (:package package))
    hash-table)
(defun self-source--definition-table (pathnames &key root package)
  "Return every definition read from PATHNAMES in PACKAGE, grouped by name.

The EQUAL table maps each definition name to its definitions in the order
SELF-SOURCE--DEFINITIONS returns them."
  (let ((table (make-hash-table :test 'equal)))
    (loop for pathname in (sort (copy-list pathnames) #'string< :key #'namestring)
          for source = (uiop:read-file-string pathname)
          do (loop for source-form in (source-read-forms source :package package)
                   for form = (source-form-form source-form)
                   when (definition-form-p form)
                     do (push (self-source--tracked-definition pathname source source-form
                                                               :root root)
                              (gethash (second form) table))))
    (maphash (lambda (name definitions)
               (setf (gethash name table) (nreverse definitions)))
             table)
    table))

(-> tracked-definition-snapshot--definitions
    (tracked-definition-snapshot configuration t)
    list)
(defun tracked-definition-snapshot--definitions (snapshot configuration name)
  "Return NAME's tracked definitions from SNAPSHOT, reading the source on first use.

A failed read is remembered and signaled again for every later lookup."
  (let ((failure (tracked-definition-snapshot--failure snapshot)))
    (when failure
      (error failure)))
  (unless (tracked-definition-snapshot--table snapshot)
    (handler-case
        (setf (tracked-definition-snapshot--table snapshot)
              (self-source--definition-table
               (self--tracked-source-pathnames configuration)
               :root    (config :source-root configuration)
               :package (find-package '#:autolith)))
      (error (condition)
        (setf (tracked-definition-snapshot--failure snapshot) condition)
        (error condition))))
  (values (gethash name (tracked-definition-snapshot--table snapshot))))

(-> self-source--component-pathnames (t) list)
(defun self-source--component-pathnames (component)
  "Return every existing Common Lisp source file beneath ASDF COMPONENT.

A component whose :IF-FEATURE this image lacks is not part of the system here,
and its file may not even read on this host, so it contributes nothing."
  (let ((feature (asdf/component:component-if-feature component)))
    (cond ((and feature (not (uiop:featurep feature)))
           nil)
          ((typep component 'asdf:cl-source-file)
           (let ((pathname (asdf:component-pathname component)))
             (if (probe-file pathname)
                 (list pathname)
                 nil)))
          (t
           (mapcan #'self-source--component-pathnames
                   (asdf:component-children component))))))

(-> self-source--dependency-names (t) list)
(defun self-source--dependency-names (system)
  "Return the names of ASDF SYSTEM's direct dependencies that apply on this host.

A dependency is a plain name, (:VERSION name version), or (:FEATURE expression
dependency), which counts only when its feature expression holds."
  (labels ((dependency-name (specification)
             "Return the system name SPECIFICATION selects here, or NIL."
             (cond
               ((or (stringp specification) (symbolp specification))
                (string-downcase (string specification)))
               ((and (consp specification)
                     (eq (first specification) ':version))
                (dependency-name (second specification)))
               ((and (consp specification)
                     (eq (first specification) ':feature))
                (and (uiop:featurep (second specification))
                     (dependency-name (third specification))))
               (t
                nil))))
    (remove nil (mapcar #'dependency-name (asdf:system-depends-on system)))))

(-> self-source--dependency-system (package (option string)) (option t))
(defun self-source--dependency-system (package requested-name)
  "Return the direct Autolith dependency selected by PACKAGE or REQUESTED-NAME."
  (block nil
    (let* ((name
             (if (non-empty-string-p requested-name)
                 requested-name
                 (string-downcase (package-name package))))
           (dependencies
             (self-source--dependency-names (asdf:find-system '#:autolith))))
      (unless (member name dependencies :test #'string-equal)
        (when (non-empty-string-p requested-name)
          (error 'source-mutation-error
                 :message
                 (format nil "~S is not a direct Autolith ASDF dependency."
                         requested-name)
                 :tool-name "lisp.source"
                 :pathname nil))
        (return nil))
      (or (asdf:find-system name nil)
          (error 'source-mutation-error
                 :message (format nil "ASDF cannot locate dependency ~S." name)
                 :tool-name "lisp.source"
                 :pathname nil)))))

(-> self-dependency-definitions
    (symbol package &key (:system-name (option string)))
    list)
(defun self-dependency-definitions (symbol package &key system-name)
  "Return SYMBOL definitions from one direct, loaded Autolith dependency."
  (let ((system (self-source--dependency-system package system-name)))
    (when system
      (let ((root (asdf:system-source-directory system)))
        (self-source--definitions
         (self-source--component-pathnames system)
         :root root
         :package package
         :symbol symbol
         :path-prefix (format nil "~A:" (asdf:component-name system)))))))

(-> self-source--withheld-files (&key (:root pathname)) list)
(defun self-source--withheld-files
    (&key (root (asdf:system-source-directory (asdf:find-system "autolith"))))
  "Return source paths unavailable in this image through ASDF component boundaries.

Feature-withheld components and unloaded optional Autolith systems may refer to
packages absent from this image. Do not read those files during source lookup.
Files shared with a loaded system are available through that system."
  (let* ((loaded (asdf:already-loaded-systems))
         (systems (loop for name in (asdf:registered-systems)
                        when (or (string= name "autolith")
                                 (uiop:string-prefix-p "autolith/" name))
                          collect (asdf:find-system name)))
         (available (mapcan #'self-source--component-pathnames
                            (remove-if-not
                             (lambda (component)
                               (member (asdf:component-name component) loaded
                                       :test #'string-equal))
                             systems))))
    (labels ((collect (component withheld-p)
               "Return withheld files below COMPONENT, respecting loaded owners."
               (let* ((feature (asdf/component:component-if-feature component))
                      (withheld-p (or withheld-p
                                      (and feature (not (uiop:featurep feature))))))
                 (cond
                   ((not (typep component 'asdf:cl-source-file))
                    (mapcan (lambda (child) (collect child withheld-p))
                            (asdf:component-children component)))
                   ((and withheld-p
                         (not (member (asdf:component-pathname component) available
                                      :test #'uiop:pathname-equal)))
                    (list (enough-namestring (asdf:component-pathname component)
                                             root)))
                   (t
                    nil)))))
      (remove-duplicates
       (mapcan (lambda (component)
                 (collect component
                          (not (member (asdf:component-name component) loaded
                                       :test #'string-equal))))
               systems)
       :test #'string=))))

(-> self-tracked-definitions (configuration symbol) list)
(defun self-tracked-definitions (configuration symbol)
  "Return complete tracked top-level definitions whose name is SYMBOL.

Files belonging only to unloaded optional systems or withheld features are left out.
Inside WITH-TRACKED-DEFINITION-SNAPSHOT the source is read once for all lookups."
  (let ((snapshot *tracked-definition-snapshot*))
    (if (and snapshot
             (uiop:pathname-equal (tracked-definition-snapshot-source-root snapshot)
                                  (config :source-root configuration)))
        (tracked-definition-snapshot--definitions snapshot configuration symbol)
        (self-source--definitions (self--tracked-source-pathnames configuration)
                                  :root    (config :source-root configuration)
                                  :package (find-package '#:autolith)
                                  :symbol  symbol))))

(-> self--tracked-source-pathnames (configuration) list)
(defun self--tracked-source-pathnames (configuration)
  "Return CONFIGURATION's tracked src/ files this image loads.

Files belonging only to unloaded optional systems or withheld features are left out."
  (let* ((source-root (config :source-root configuration))
         (withheld (self-source--withheld-files :root source-root)))
    (remove-if (lambda (pathname)
                 (member (enough-namestring pathname source-root) withheld
                         :test #'string=))
               (source-lisp-pathnames (merge-pathnames "src/" source-root)))))

(-> self-tracked-definition (configuration list) (option tracked-definition))
(defun self-tracked-definition (configuration definition)
  "Return the tracked definition sharing DEFINITION's key, if any."
  (let ((key (definition-key definition)))
    (loop for tracked in (self-tracked-definitions
                          configuration
                          (definition-name-symbol (second definition)))
          for form = (source-form-form (tracked-definition-source-form tracked))
          when (string= (definition-key form) key)
            return tracked)))

(-> self-tracked-definition-source (configuration list) (option string))
(defun self-tracked-definition-source (configuration definition)
  "Return the tracked source of the definition sharing DEFINITION's key, if any."
  (let ((tracked (self-tracked-definition configuration definition)))
    (and tracked (tracked-definition-source tracked))))

(-> self-render-tracked-definitions (list symbol) string)
(defun self-render-tracked-definitions (definitions symbol)
  "Render complete DEFINITIONS for model inspection of SYMBOL."
  (unless definitions
    (error 'source-mutation-error
           :message (format nil "No tracked top-level definition names ~S." symbol)
           :tool-name "lisp.source"
           :pathname nil))
  (with-output-to-string (stream)
    (loop for definition in definitions
          for first-p = t then nil
          unless first-p
            do (format stream "~%~%")
          do (format stream
                     "~A~%~A"
                     (tracked-definition-relative-pathname definition)
                     (tracked-definition-source definition)))))

(defmethod lisp-source-active-image
    ((context tool-context) (arguments hash-table))
  "Return active-image source definitions selected by ARGUMENTS."
  (let* ((package
           (self-resolve-package (tool-argument arguments "package")))
         (symbol (self-resolve-symbol
                  (tool-argument arguments "name" :required t)
                  :package package))
         (tracked-definitions
           (self-tracked-definitions
            (tool-context-configuration context)
            symbol))
         (dependency-definitions
           (unless tracked-definitions
             (self-dependency-definitions
              symbol
              (or (symbol-package symbol) package)
              :system-name (tool-argument arguments "system")))))
    (cond
      (tracked-definitions
       (tool-success
        (self-render-tracked-definitions tracked-definitions symbol)))
      (dependency-definitions
       (tool-success
        (self-render-tracked-definitions dependency-definitions symbol)))
      ((non-empty-string-p (tool-argument arguments "system"))
       (tool-success
        (self-render-tracked-definitions dependency-definitions symbol)))
      ((not (symbol-defined-p symbol))
       (error 'tool-error
              :message
              (format nil "~S has no function, variable, class, or type definition in the active image.~A Find the exact name with lisp.apropos or search.content instead of guessing."
                      symbol
                      (self-symbol-suggestion-text (symbol-name symbol)
                                                   :package package))
              :tool-name "lisp.source"))
      (t
       (multiple-value-bind (values output)
           (handler-case
               (worker-source (write-to-string symbol :readably t)
                              (tool-argument arguments "kind"))
             (worker-error (condition)
               (error 'tool-error
                      :message (format nil "~S: ~A"
                                       symbol
                                       (autolith-error-message condition))
                      :tool-name "lisp.source")))
         (declare (ignore values))
         (tool-success output))))))

(-> source-replace-definition (pathname string) (values string string))
(defun source-replace-definition (pathname definition-source)
  "Replace one complete definition and return updated and preceding source text."
  (handler-case
      (surgeon:source-replace-definition pathname definition-source
                                         :package (find-package '#:autolith))
    ((or definition-unsupported definition-not-found) (condition)
      (error 'source-mutation-error
             :message   (surgeon-error-message condition)
             :tool-name "self.persist-definition"
             :pathname  pathname))))
