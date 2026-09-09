(in-package #:autolith)

;;;; -- Active Image Inspection --

(defvar *exploratory-definitions* (make-hash-table :test #'equal)
  "Complete source forms installed exploratorily in the active image.")

(defvar *exploratory-undo-actions* (make-hash-table :test #'equal)
  "Exact in-memory undo actions for this process's pending mutations.")

(-> self-resolve-package ((option string)) package)
(defun self-resolve-package (name)
  "Return existing package NAME, defaulting to the AUTOLITH package."
  (let ((package (if (non-empty-string-p name)
                     (find-package name)
                     (find-package '#:autolith))))
    (unless package
      (error 'source-mutation-error
             :message (format nil "No active Common Lisp package is named ~S."
                              name)
             :tool-name "self.redefine"
             :pathname nil))
    package))

(-> self-call-with-package-unlocked (package function) t)
(defun self-call-with-package-unlocked (package thunk)
  "Call THUNK with PACKAGE temporarily unlocked, restoring its lock afterward."
  (let ((locked-p (sb-ext:package-locked-p package)))
    (unwind-protect
         (progn
           (when locked-p
             (sb-ext:unlock-package package))
           (funcall thunk))
      (when (and locked-p (find-package (package-name package)))
        (sb-ext:lock-package package)))))

(-> self-read-form
    (string &key (:read-eval boolean) (:package package))
    t)
(defun self-read-form
    (source &key (read-eval t) (package (find-package '#:autolith)))
  "Read exactly one Common Lisp form from SOURCE relative to PACKAGE."
  (self-call-with-package-unlocked
   package
   (lambda ()
     (let ((*read-eval* read-eval)
           (*package* package)
           (end-marker (cons nil nil)))
       (multiple-value-bind (form position)
           (read-from-string source t nil)
         (multiple-value-bind (extra ignored-position)
             (read-from-string source nil end-marker :start position)
           (declare (ignore ignored-position))
           (unless (eq extra end-marker)
             (error "Expected exactly one Common Lisp form.")))
         form)))))

(-> self-resolve-symbol (string &key (:package package)) symbol)
(defun self-resolve-symbol (name &key (package (find-package '#:autolith)))
  "Resolve readable symbol NAME relative to PACKAGE."
  (let ((value (self-read-form name :read-eval nil :package package)))
    (unless (symbolp value)
      (error "~S does not name a symbol." name))
    value))

(-> self-symbol-lambda-list (symbol) t)
(defun self-symbol-lambda-list (symbol)
  "Return SYMBOL's function lambda list when introspection can recover it."
  (when (fboundp symbol)
    (handler-case
        (let ((function (symbol-function symbol)))
          (if (typep function 'generic-function)
              (closer-mop:generic-function-lambda-list function)
              (multiple-value-bind (expression closure-p lexical-name)
                  (function-lambda-expression function)
                (declare (ignore closure-p lexical-name))
                (and (consp expression)
                     (eq (first expression) 'lambda)
                     (second expression)))))
      (error ()
        nil))))

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
              (self-symbol-lambda-list symbol)
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
  (tool-success
   (self-inspect-symbol
    (self-resolve-symbol
     (tool-argument arguments "designator" :required t)))))


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

(-> self--selectable-restarts (condition) list)
(defun self--selectable-restarts (condition)
  "Return (NAME . REPORT) pairs for CONDITION's invokable restarts.

The ABORT restart is excluded because invoking it would unwind Autolith's own
event loop instead of correcting the failed operation."
  (loop for restart in (compute-restarts condition)
        for name = (restart-name restart)
        when (and name (not (eq name 'abort)))
          collect (cons (symbol-name name)
                        (princ-to-string restart))))

(-> self--find-selected-restart (condition string) t)
(defun self--find-selected-restart (condition name)
  "Return CONDITION's first non-ABORT restart named NAME, or NIL."
  (find-if (lambda (restart)
             (let ((restart-name (restart-name restart)))
               (and restart-name
                    (not (eq restart-name 'abort))
                    (string-equal (symbol-name restart-name) name))))
           (compute-restarts condition)))

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
  (handler-bind
      ((error
         (lambda (condition)
           (unless (typep condition 'self-correctable-error)
             (let ((restarts (self--selectable-restarts condition)))
               (when restarts
                 (error 'self-correctable-error
                        :message (self--correctable-message condition
                                                            restarts)
                        :restart-names (mapcar #'first restarts))))))))
    (if (non-empty-string-p restart-name)
        (handler-bind
            ((error
               (lambda (condition)
                 (let ((restart (self--find-selected-restart condition
                                                             restart-name)))
                   (when restart
                     (if (non-empty-string-p restart-value-source)
                         (invoke-restart restart
                                         (eval (self-read-form
                                                restart-value-source)))
                         (invoke-restart restart)))))))
          (funcall thunk))
        (funcall thunk))))

(defmethod tool-execute ((tool self-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate one exploratory form in CONTEXT's active image and journal it."
  (declare (ignore tool))
  (with-live-mutation
    (let* ((source (tool-argument arguments "form" :required t))
           (restart-name (tool-argument arguments "restart"))
           (restart-value-source (tool-argument arguments "restart-value"))
           (configuration (tool-context-configuration context)))
      (mutation-journal-append
       configuration
       (list :mutation :kind :eval :proposed source :result ':pending))
      (handler-case
          (multiple-value-bind (result-values output)
              (self-capture-evaluation
               (lambda ()
                 (self-call-with-restarts
                  (lambda ()
                    (eval (self-read-form source)))
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
          (error condition))))))


;;;; -- Definition Installation --

(defparameter *definition-operators*
  '(defun defgeneric defmethod defmacro defclass defstruct define-condition
    deftype define-compiler-macro defvar defparameter
    define-context-contributor define-application-command)
  "Top-level defining operators accepted by self.redefine and source persistence.")

(-> definition-name-p (t) boolean)
(defun definition-name-p (value)
  "Return true when VALUE is a symbol or a two-part SETF function name."
  (or (symbolp value)
      (and (listp value)
           (= (length value) 2)
           (eq (first value) 'setf)
           (symbolp (second value)))))

(-> definition-form-p (t) boolean)
(defun definition-form-p (form)
  "Return true when FORM is one supported complete top-level definition."
  (and (consp form)
       (symbolp (first form))
       (member (first form) *definition-operators* :test #'eq)
       (definition-name-p (second form))))

(defvar *image-replay-skipped-definitions* nil
  "Messages for stale replayed definitions skipped during the current load.")

(-> definition-name-symbol (t) symbol)
(defun definition-name-symbol (name)
  "Return the symbol identifying definition NAME, unwrapping SETF names."
  (if (consp name)
      (second name)
      name))

(-> definition-home-package-name (list) (option string))
(defun definition-home-package-name (definition)
  "Return the home package name of DEFINITION's target symbol, if interned."
  (let ((home (symbol-package (definition-name-symbol (second definition)))))
    (when home
      (package-name home))))

(-> self-call-with-definition-unlocked (list package function) t)
(defun self-call-with-definition-unlocked (definition package thunk)
  "Call THUNK with the reader and target packages unlocked, restoring both locks."
  (let ((home (symbol-package (definition-name-symbol (second definition)))))
    (self-call-with-package-unlocked
     package
     (lambda ()
       (if (and home (not (eq home package)))
           (self-call-with-package-unlocked home thunk)
           (funcall thunk))))))

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

(-> method-specializers (list) list)
(defun method-specializers (specialized-lambda-list)
  "Return required SPECIALIZED-LAMBDA-LIST specializers without parameter names."
  (loop for parameter in specialized-lambda-list
        until (member parameter lambda-list-keywords :test #'eq)
        collect (if (consp parameter)
                    (second parameter)
                    t)))

(-> definition-signature (list) list)
(defun definition-signature (definition)
  "Return the semantic source identity of one top-level DEFINITION."
  (if (eq (first definition) 'defmethod)
      (let* ((tail (rest (rest definition)))
             (lambda-position (position-if #'listp tail)))
        (unless lambda-position
          (error "DEFMETHOD has no lambda list."))
        (let ((qualifiers (subseq tail 0 lambda-position))
              (specialized-lambda-list (nth lambda-position tail)))
          (list (first definition)
                (second definition)
                qualifiers
                (method-specializers specialized-lambda-list))))
      (list (first definition) (second definition))))

(-> definition-key (list) string)
(defun definition-key (definition)
  "Return a stable readable key for DEFINITION's semantic signature."
  (write-to-string (definition-signature definition)
                   :readably t
                   :case :downcase))

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

(-> self--restore-function-binding (t boolean t) null)
(defun self--restore-function-binding (name bound-p binding)
  "Restore NAME's exact function BINDING or its prior unbound state."
  (if bound-p
      (setf (fdefinition name) binding)
      (when (fboundp name)
        (fmakunbound name)))
  nil)

(-> self--restore-value-binding (symbol boolean t) null)
(defun self--restore-value-binding (symbol bound-p value)
  "Restore SYMBOL's exact VALUE or its prior unbound state."
  (if bound-p
      (setf (symbol-value symbol) value)
      (when (boundp symbol)
        (makunbound symbol)))
  nil)

(-> self--restore-sbcl-info
    (symbol &key (:category keyword) (:kind keyword) (:snapshot list))
    null)
(defun self--restore-sbcl-info (name &key category kind snapshot)
  "Restore one SBCL global database entry for NAME from SNAPSHOT."
  (if (second snapshot)
      (setf (sb-int:info category kind name) (first snapshot))
      (sb-int:clear-info category kind name))
  nil)

(-> self--package-function-snapshot (package) hash-table)
(defun self--package-function-snapshot (package)
  "Capture exact function bindings currently interned in PACKAGE."
  (let ((snapshot (make-hash-table :test #'eq)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (setf (gethash symbol snapshot)
              (list (not (null (fboundp symbol)))
                    (and (fboundp symbol) (fdefinition symbol))))))
    snapshot))

(-> self--restore-package-functions (package hash-table) null)
(defun self--restore-package-functions (package snapshot)
  "Restore PACKAGE's function bindings from SNAPSHOT."
  (do-symbols (symbol package)
    (when (eq (symbol-package symbol) package)
      (multiple-value-bind (state present-p)
          (gethash symbol snapshot)
        (if present-p
            (self--restore-function-binding symbol
                                            (first state)
                                            (second state))
            (when (fboundp symbol)
              (fmakunbound symbol))))))
  nil)

(-> self--method-components (list package) (values t list list))
(defun self--method-components (definition package)
  "Return DEFINITION's generic name, qualifiers, and method specializers."
  (let* ((tail (rest (rest definition)))
         (lambda-position (position-if #'listp tail))
         (qualifiers (subseq tail 0 lambda-position))
         (lambda-list (nth lambda-position tail)))
    (values
     (second definition)
     qualifiers
     (loop for parameter in lambda-list
           until (member parameter lambda-list-keywords :test #'eq)
           for specializer = (and (consp parameter) (second parameter))
           collect
           (cond
             ((null specializer)
              (find-class 't))
             ((and (consp specializer) (eq (first specializer) 'eql))
              (closer-mop:intern-eql-specializer
               (let ((*package* package))
                 (eval (second specializer)))))
             (t
              (find-class
               (if (symbolp specializer)
                   specializer
                   (let ((*package* package))
                     (eval specializer))))))))))

(-> self--find-definition-method (list package) (values t t))
(defun self--find-definition-method (definition package)
  "Return DEFINITION's generic function and current method, when present."
  (multiple-value-bind (name qualifiers specializers)
      (self--method-components definition package)
    (let ((generic-function (and (fboundp name) (fdefinition name))))
      (values generic-function
              (and (typep generic-function 'generic-function)
                   (find-method generic-function
                                qualifiers
                                specializers
                                nil))))))

(-> self--definition-undo-action (list (option string) package) function)
(defun self--definition-undo-action (definition previous-source package)
  "Return an exact undo action for installing DEFINITION in PACKAGE."
  (let ((operator (first definition))
        (name (second definition)))
    (case operator
      ((defun defgeneric defmacro)
       (let ((bound-p (not (null (fboundp name))))
             (binding (and (fboundp name) (fdefinition name))))
         (lambda ()
           (self--restore-function-binding name bound-p binding))))
      (define-context-contributor
       (let* ((identifier (context--definition-identifier name))
              (registration (context--registration-snapshot identifier))
              (bound-p (not (null (fboundp name))))
              (binding (and bound-p (fdefinition name))))
         (lambda ()
           (self--restore-function-binding name bound-p binding)
           (context--registration-restore identifier registration))))
      (define-application-command
       (let ((registration
               (application-command--registration-snapshot name ':runtime))
             (bound-p (not (null (fboundp name))))
             (binding (and (fboundp name) (fdefinition name))))
         (lambda ()
           (self--restore-function-binding name bound-p binding)
           (application-command--registration-restore
            name
            ':runtime
            registration))))
      (define-compiler-macro
       (let ((binding (compiler-macro-function name)))
         (lambda ()
           (setf (compiler-macro-function name) binding))))
      (defmethod
       (multiple-value-bind (generic-function method)
           (self--find-definition-method definition package)
         (let ((generic-function-existed-p
                 (typep generic-function 'generic-function)))
           (lambda ()
             (multiple-value-bind (current-generic-function current-method)
                 (self--find-definition-method definition package)
               (when current-method
                 (remove-method current-generic-function current-method))
               (when method
                 (add-method current-generic-function method))
               (unless generic-function-existed-p
                 (when (fboundp name)
                   (fmakunbound name))))))))
      ((defvar defparameter)
       (let ((bound-p (boundp name))
             (value (and (boundp name) (symbol-value name)))
             (kind (multiple-value-list
                    (sb-int:info :variable :kind name))))
         (lambda ()
           (self--restore-value-binding name bound-p value)
           (self--restore-sbcl-info name
                                    :category ':variable
                                    :kind ':kind
                                    :snapshot kind))))
      (deftype
       (let ((expander (multiple-value-list
                        (sb-int:info :type :expander name)))
             (source-location (multiple-value-list
                               (sb-int:info :type :source-location name))))
         (lambda ()
           (self--restore-sbcl-info name
                                    :category ':type
                                    :kind ':expander
                                    :snapshot expander)
           (self--restore-sbcl-info name
                                    :category ':type
                                    :kind ':source-location
                                    :snapshot source-location))))
      ((defclass defstruct define-condition)
       (let ((existing-class (find-class name nil))
             (function-snapshot
               (self--package-function-snapshot package)))
         (when (and existing-class (null previous-source))
           (error 'source-mutation-error
                  :message
                  "The existing class has no recoverable source, so this exploratory redefinition cannot be made safely reversible."
                  :tool-name "self.redefine"
                  :pathname nil))
         (lambda ()
           (if previous-source
               (self--install-definition
                (self-read-form previous-source
                                :read-eval nil
                                :package package)
                previous-source
                :package package)
               (progn
                 (when (find-class name nil)
                   (setf (find-class name) nil))
                 (self--restore-package-functions package function-snapshot))))))
      (otherwise
       (error 'source-mutation-error
              :message "The definition kind has no reversible installation strategy."
              :tool-name "self.redefine"
              :pathname nil)))))

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
        (self-call-with-definition-unlocked definition package binding-undo)
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
  (self-call-with-definition-unlocked
   definition package
   (lambda ()
     (let* ((*package* package)
            (result (eval definition)))
       (setf (gethash (definition-key definition) *exploratory-definitions*)
             source)
       result))))

(-> self-replay-definition
    (string string &key (:home-package (option string))) t)
(defun self-replay-definition
    (package-name source &key (home-package package-name))
  "Read and install persisted SOURCE in PACKAGE-NAME during reconstruction.
HOME-PACKAGE records the target's owner when the mutation was made. Intentional
foreign definitions replay; stale definitions whose ownership moved are skipped.
Legacy two-argument records assume their reader package owned the target."
  (let* ((package (self-resolve-package package-name))
         (definition
           (self-read-form source :read-eval nil :package package)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "A private image commit contains an invalid definition."
             :tool-name "self.commit"
             :pathname nil))
    (if (definition-foreign-home-p definition package :home-package home-package)
        (let ((home (symbol-package (definition-name-symbol
                                     (second definition)))))
          (push (format nil
                        "The persisted ~(~A~) of ~(~A~) was skipped: ~A"
                        (first definition)
                        (definition-name-symbol (second definition))
                        (if home
                            (format nil "its owner changed from ~A to ~A. Reapply the definition to authorize its new owner."
                                    (or home-package "an uninterned symbol")
                                    (package-name home))
                            "an uninterned target has no stable replay identity."))
                *image-replay-skipped-definitions*)
          nil)
        (self--install-definition definition source :package package))))

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
                    (self--restore-value-binding symbol
                                                 previous-bound-p
                                                 previous-value)))
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

(defclass source-form ()
  ((form
    :initarg :form
    :reader source-form-form
    :type t
    :documentation "The parsed top-level form.")
   (start
    :initarg :start
    :reader source-form-start
    :type integer
    :documentation "The character offset at which the form begins.")
   (end
    :initarg :end
    :reader source-form-end
    :type integer
    :documentation "The character offset immediately after the form."))
  (:documentation "One parsed top-level form and its exact source span."))

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

(-> source--skip-block-comment (string integer) integer)
(defun source--skip-block-comment (source start)
  "Return the first offset after the nested block comment at START."
  (let ((position (+ start 2))
        (depth 1)
        (length (length source)))
    (loop while (and (< position length) (plusp depth))
          do (cond
               ((and (< (1+ position) length)
                     (char= (char source position) #\#)
                     (char= (char source (1+ position)) #\|))
                (incf depth)
                (incf position 2))
               ((and (< (1+ position) length)
                     (char= (char source position) #\|)
                     (char= (char source (1+ position)) #\#))
                (decf depth)
                (incf position 2))
               (t
                (incf position))))
    (when (plusp depth)
      (error "Unterminated block comment in source file."))
    position))

(-> source--next-form-start (string integer) integer)
(defun source--next-form-start (source start)
  "Skip whitespace and comments in SOURCE beginning at START."
  (let ((position start)
        (length (length source)))
    (loop
      (loop while (and (< position length)
                       (find (char source position)
                             '(#\Space #\Tab #\Newline #\Return #\Page)))
            do (incf position))
      (cond
        ((and (< position length) (char= (char source position) #\;))
         (let ((newline (position #\Newline source :start position)))
           (setf position (if newline (1+ newline) length))))
        ((and (< (1+ position) length)
              (char= (char source position) #\#)
              (char= (char source (1+ position)) #\|))
         (setf position (source--skip-block-comment source position)))
        (t
         (return position))))))

(-> source-read-forms (string &key (:package package)) list)
(defun source-read-forms
    (source &key (package (find-package '#:autolith)))
  "Read complete top-level forms and exact spans from SOURCE in PACKAGE."
  (let ((stream (make-string-input-stream source))
        (position 0)
        (forms nil)
        (*read-eval* nil)
        (*package* package))
    (loop
      (setf position (source--next-form-start source position))
      (when (>= position (length source))
        (return (nreverse forms)))
      (file-position stream position)
      (let ((form (read stream t nil))
            (start position)
            (end (file-position stream)))
        (push (make-instance 'source-form :form form :start start :end end)
              forms)
        (setf position end)))))

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
                (make-instance
                 'tracked-definition
                 :relative-pathname
                 (format nil "~A~A"
                         path-prefix
                         (enough-namestring pathname root))
                 :source-form source-form
                 :source (subseq source
                                 (source-form-start source-form)
                                 (source-form-end source-form))))))

(-> self-source--component-pathnames (t) list)
(defun self-source--component-pathnames (component)
  "Return every existing Common Lisp source file beneath ASDF COMPONENT."
  (if (typep component 'asdf:cl-source-file)
      (let ((pathname (asdf:component-pathname component)))
        (if (probe-file pathname)
            (list pathname)
            nil))
      (mapcan #'self-source--component-pathnames
              (asdf:component-children component))))

(-> self-source--dependency-system (package (option string)) (option t))
(defun self-source--dependency-system (package requested-name)
  "Return the direct Autolith dependency selected by PACKAGE or REQUESTED-NAME."
  (block nil
    (let* ((name
             (if (non-empty-string-p requested-name)
                 requested-name
                 (string-downcase (package-name package))))
           (dependencies
             (asdf:system-depends-on (asdf:find-system '#:autolith))))
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

(-> self-tracked-definitions (configuration symbol) list)
(defun self-tracked-definitions (configuration symbol)
  "Return complete tracked top-level definitions whose name is SYMBOL."
  (let* ((source-root (configuration-source-root configuration))
         (editable-root (merge-pathnames "src/" source-root)))
    (self-source--definitions
     (source-lisp-pathnames editable-root)
     :root source-root
     :package (find-package '#:autolith)
     :symbol symbol)))

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
      (t
       (multiple-value-bind (values output)
           (worker-source (write-to-string symbol :readably t)
                          (tool-argument arguments "kind"))
         (declare (ignore values))
         (tool-success output))))))

(-> source-definition-match-p (source-form list) boolean)
(defun source-definition-match-p (source-form definition)
  "Return true when SOURCE-FORM defines the same operator and name as DEFINITION."
  (let ((form (source-form-form source-form)))
    (and (definition-form-p form)
         (equal (definition-signature form)
                (definition-signature definition)))))

(-> source-find-definition (pathname list) (values source-form string))
(defun source-find-definition (pathname definition)
  "Return DEFINITION's parsed source form and complete file text from PATHNAME."
  (let* ((source (uiop:read-file-string pathname))
         (match (find-if
                 (lambda (source-form)
                   (source-definition-match-p source-form definition))
                 (source-read-forms source))))
    (unless match
      (error 'source-mutation-error
             :message (format nil "No matching definition exists in ~A." pathname)
             :tool-name "self.persist-definition"
             :pathname pathname))
    (values match source)))

(-> source--atomic-write (pathname string) pathname)
(defun source--atomic-write (pathname content)
  "Atomically replace PATHNAME with CONTENT through a sibling temporary file."
  (let ((temporary
          (merge-pathnames
           (format nil ".~A.~D.tmp"
                   (pathname-name pathname)
                   (sb-posix:getpid))
           (uiop:pathname-directory-pathname pathname))))
    (with-open-file (stream temporary
                            :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create
                            :external-format ':utf-8)
      (write-string content stream)
      (finish-output stream))
    (uiop:rename-file-overwriting-target temporary pathname)
    pathname))

(-> source-replace-definition (pathname string) (values string string))
(defun source-replace-definition (pathname definition-source)
  "Replace one complete definition and return updated and preceding source text."
  (let ((definition (self-read-form definition-source :read-eval nil)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "The durable source is not a supported complete definition."
             :tool-name "self.persist-definition"
             :pathname pathname))
    (multiple-value-bind (match source)
        (source-find-definition pathname definition)
      (let ((previous-definition
              (subseq source
                      (source-form-start match)
                      (source-form-end match)))
            (updated
              (concatenate 'string
                           (subseq source 0 (source-form-start match))
                           definition-source
                           (subseq source (source-form-end match)))))
        (source--atomic-write pathname updated)
        (values updated previous-definition)))))
