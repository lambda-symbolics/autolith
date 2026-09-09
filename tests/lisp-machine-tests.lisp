(in-package #:autolith)

;;;; -- Lisp Machine Test Support --

(define-condition lisp-machine-tests--warning-condition (warning)
  ()
  (:report "signaled through ERROR")
  (:documentation
   "A warning signaled through ERROR, mirroring ASDF's invalid configurations."))

(defvar *lisp-machine-test-value* nil
  "Active-image value used to verify direct local evaluation side effects.")

(defvar *lisp-machine-test-activity* nil
  "Local activity captured from inside one explicit active-image evaluation.")

(defvar *lisp-machine-test-interactive-p* nil
  "Whether explicit terminal Lisp observed interactive command context.")

(defvar *lisp-machine-test-repair-value* nil
  "Active-image value used to verify synthetic repair source execution.")

(-> lisp-machine-tests--recovery
    (keyword &key (:restart-id (option string))
                  (:preparation-source (option string))
                  (:argument-source (option string))
                  (:return-source (option string)))
    application-debugger-recovery)
(defun lisp-machine-tests--recovery
    (kind &key restart-id preparation-source argument-source return-source)
  "Return one executable debugger recovery proposal of KIND."
  (make-instance 'application-debugger-recovery
                 :kind kind
                 :report (format nil "test ~(~A~) recovery" kind)
                 :restart-id restart-id
                 :preparation-source preparation-source
                 :argument-source argument-source
                 :return-source return-source))

(-> lisp-machine-tests--debugger-session
    (&key (:retry-p boolean) (:return-values-p boolean))
    application-debugger-session)
(defun lisp-machine-tests--debugger-session
    (&key (retry-p t) (return-values-p t))
  "Return a portable debugger session with one diagnostic restart."
  (make-instance
   'application-debugger-session
   :condition-type "simple-error"
   :condition-report "test failure"
   :source "(error \"test failure\")"
   :operation-kind ':lisp
   :owner-thread (current-thread)
   :restarts '((:id "restart-1" :report "Use the supplied value."))
   :capabilities (list :invoke-restart t
                       :repair-and-invoke t
                       :retry-operation retry-p
                       :repair-and-retry retry-p
                       :return-values return-values-p
                       :abort-operation t)
   :backtrace '("frame one" "frame two")))

(-> lisp-machine-tests--recovery-invalid-p
    (application-debugger-session application-debugger-recovery)
    boolean)
(defun lisp-machine-tests--recovery-invalid-p (session recovery)
  "Return true when RECOVERY is rejected for SESSION."
  (handler-case
      (progn
        (application-debugger--validate-recovery session recovery)
        nil)
    (application-debugger-recovery-error ()
      t)))

(-> lisp-machine-tests--application
    (&key (:terminal (option terminal)))
    (values application pathname))
(defun lisp-machine-tests--application (&key terminal)
  "Return a temporary application using TERMINAL and its data root."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier (make-identifier)))
         (ui
           (terminal-ui-create
            :terminal
            (or terminal
                (make-instance 'recording-terminal
                               :columns 100
                               :styled-p t))))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :tool-registry (make-default-tool-registry)
                          :ui ui)))
    (values application root)))

(-> lisp-machine-tests--controller (application) application-input-controller)
(defun lisp-machine-tests--controller (application)
  "Return a local input controller attached to APPLICATION."
  (let ((controller
          (make-instance 'application-input-controller
                         :application application
                         :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    controller))


;;;; -- Direct Evaluation --

(-> test-application-lisp-evaluation () null)
(defun test-application-lisp-evaluation ()
  "Test one-form reading, output, values, side effects, conditions, and restarts."
  (test-assert (application-lisp-input-incomplete-p "(list 1")
               "an unterminated top-level form requests another input line")
  (test-assert (application-lisp-input-incomplete-p "(format nil \"open")
               "an unterminated string requests another input line")
  (test-assert (not (application-lisp-input-incomplete-p "(list 1)"))
               "a complete top-level form is ready for evaluation")
  (let ((evaluation
          (application-lisp-evaluate
           "(progn (format t \"hello~%\") (values 1 2))")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (string= (application-lisp-evaluation-output evaluation)
                   (format nil "hello~%"))
          (equal (application-lisp-evaluation-values evaluation) '("1" "2")))
     "local evaluation captures output and every returned value"))
  (let ((evaluation (application-lisp-evaluate "(values)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (null (application-lisp-evaluation-values evaluation)))
     "local evaluation preserves a zero-value return"))
  (setf *lisp-machine-test-value* nil)
  (application-lisp-evaluate "(setf *lisp-machine-test-value* :changed)")
  (test-assert (eq *lisp-machine-test-value* ':changed)
               "local evaluation mutates the active image directly")
  (let ((evaluation
          (application-lisp-evaluate "(list 1) (list 2)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
          (search "exactly one Common Lisp form"
                  (application-lisp-evaluation-condition evaluation)
                  :test #'char-equal))
     "local evaluation rejects trailing forms without entering the debugger"))
  (let ((evaluation
          (application-lisp-evaluate
           "(restart-case (error \"missing\") (use-value (value) value))"
           :restart-selector
           (lambda (condition restarts)
             (declare (ignore condition))
             (values (find 'use-value restarts :key #'restart-name)
                     "42")))))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (equal (application-lisp-evaluation-values evaluation) '("42"))
          (member "USE-VALUE"
                  (application-lisp-evaluation-restart-names evaluation)
                  :test #'string=)
          (string= (application-lisp-evaluation-selected-restart-name evaluation)
                   "USE-VALUE"))
     "a selected live restart receives and records evaluated Lisp arguments"))
  (let ((evaluation (application-lisp-evaluate "(error \"stop\")")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
          (string= (application-lisp-evaluation-condition evaluation) "stop")
          (member "ABORT-USER-OPERATION"
                  (application-lisp-evaluation-restart-names evaluation)
                  :test #'string=)
          (string=
           (application-lisp-evaluation-selected-restart-name evaluation)
           "ABORT-USER-OPERATION"))
     "declining restart selection records the prompt abort restart"))
  (let ((evaluation
          (application-lisp-evaluate
           "(error 'lisp-machine-tests--warning-condition)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
          (non-empty-string-p
           (application-lisp-evaluation-condition evaluation)))
     "a warning signaled through ERROR aborts one evaluation instead of the image"))
  (let ((evaluation
          (application-lisp-evaluate "(progn (warn \"noticed\") :done)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (equal (application-lisp-evaluation-values evaluation) '(":DONE")))
     "ordinary warnings still complete their evaluation"))
  nil)


;;;; -- Restart Debugger --

(-> test-application-restart-debugger () null)
(defun test-application-restart-debugger ()
  "Test live restart recovery, command routing, styling, and safe aborts."
  (labels ((close-application (application root)
             "Close APPLICATION's transient state and remove ROOT."
             (let ((controller (application-input-controller application)))
               (when controller
                 (ignore-errors
                   (application-input-controller-stop controller))))
             (ignore-errors (terminal-ui-stop (application-ui application)))
             (ignore-errors
               (tool-registry-close-runtime-state
                (application-tool-registry application)))
             (platform-delete-directory-tree
              *platform*
              root :validate t :if-does-not-exist ':ignore)))
    (dolist (condition
             (list (make-condition 'rollback-requested
                                   :message "requested rollback"
                                   :generation-id "generation")
                   (make-condition 'update-requested
                                   :message "requested update"
                                   :tag "v9.9.9")))
      (test-assert
       (and (typep condition 'autolith-control-condition)
            (not (typep condition 'autolith-error))
            (not (typep condition 'serious-condition)))
       "process handoff controls use the non-error control hierarchy")
      (let ((selector-calls 0))
        (test-assert
         (handler-case
             (progn
               (application-lisp-call-with-debugger
                (lambda () (error condition))
                :restart-selector
                (lambda (signaled-condition restarts)
                  (declare (ignore signaled-condition restarts))
                  (incf selector-calls)
                  (values nil nil)))
               nil)
           (autolith-control-condition (signaled-condition)
             (and (eq signaled-condition condition)
                  (zerop selector-calls))))
         "process handoff controls bypass generic debugger boundaries")))
    (let ((selector-calls 0))
      (test-assert
       (handler-case
           (progn
             (application-lisp-call-with-debugger
              (lambda ()
                (error 'application-operation-loop-action :action ':quit))
              :restart-selector
              (lambda (condition restarts)
                (declare (ignore condition restarts))
                (incf selector-calls)
                (values nil nil)))
             nil)
         (application-operation-loop-action ()
           (zerop selector-calls)))
       "Autolith control conditions bypass the user restart selector"))
    (let ((selector-calls 0))
      (test-assert
       (handler-case
           (progn
             (application-lisp-call-with-debugger
              (lambda () (error "outer failure"))
              :restart-selector
              (lambda (condition restarts)
                (declare (ignore condition restarts))
                (incf selector-calls)
                (error "selector failure")))
             nil)
         (simple-error (condition)
           (and (= selector-calls 1)
                (search "selector failure" (princ-to-string condition)))))
       "a debugger failure propagates once without recursive selection"))
    (setf *lisp-machine-test-value* ':restart-package)
    (let ((*package* (find-package '#:cl-user)))
      (multiple-value-bind
            (values status condition restart-names selected-restart-name)
          (application-lisp-call-with-debugger
           (lambda ()
             (restart-case
                 (error "package lookup")
               (use-value (value)
                 value)))
           :restart-selector
           (lambda (condition restarts)
             (declare (ignore condition))
             (values (find 'use-value restarts :key #'restart-name)
                     "*lisp-machine-test-value*")))
        (declare (ignore condition restart-names))
        (test-assert
         (and (eq status ':ok)
              (equal values '(:restart-package))
              (string= selected-restart-name "USE-VALUE"))
         "restart argument forms read unqualified symbols in AUTOLITH")))
    (let ((items nil)
          (preferred-index nil))
      (multiple-value-bind
            (values status condition restart-names selected-restart-name)
          (application-lisp-call-with-debugger
           (lambda ()
             (restart-case
                 (error "styled failure")
               (use-value (value)
                 value)))
           :restart-selector
           (lambda (condition restarts)
             (declare (ignore condition))
             (setf items (application-lisp--restart-items restarts)
                   preferred-index
                   (application-lisp--preferred-restart-index restarts))
             (values nil nil)))
        (declare (ignore values condition restart-names))
        (test-assert
         (and (eq status ':aborted)
              (string= selected-restart-name "ABORT-USER-OPERATION"))
         "declining a restart selects the explicit prompt abort"))
      (let* ((use-value-item
               (find-if
                (lambda (item)
                  (search "use-value" (getf item :description)
                          :test #'char-equal))
                items))
             (abort-item
               (find-if
                (lambda (item)
                  (search "abort-user-operation" (getf item :description)
                          :test #'char-equal))
                items)))
        (test-assert
         (and use-value-item
              abort-item
              (= preferred-index
                 (position use-value-item items :test #'eq))
              (every (lambda (item)
                       (and (string= (getf item :group) "live restarts")
                            (terminal-completion-p item)))
                     items)
              (eq (terminal-span-style
                   (first (getf use-value-item :description-spans)))
                  ':code)
              (eq (terminal-span-style
                   (first (getf abort-item :description-spans)))
                  ':failure))
         "restart rows carry a group and semantic name styling")))
    (let* ((condition
             (make-condition 'simple-error
                             :format-control "styled failure"
                             :format-arguments nil))
           (entry (application-lisp--debugger-condition-entry condition)))
      (test-assert
       (and (eq (terminal-span-style (first entry)) ':failure)
            (eq (terminal-span-style (third entry)) ':failure)
            (search "styled failure" (terminal--spans-text entry)))
       "the debugger condition heading uses the failure style"))
    (let ((snapshot (application-command--registry-snapshot)))
      (unwind-protect
           (let* ((observed-value nil)
                  (command
                    (application-command-create
                     :definition-name
                     'lisp-machine-tests--required-debugger-command
                     :name "/debug-required"
                     :aliases nil
                     :argument "VALUE"
                     :description "exercise required command recovery"
                     :tip "tests required command recovery."
                     :busy-behavior ':execute
                     :terminal-behavior ':shared
                      :lambda-list '(value)
                      :callable-p t
                     :handler
                     (lambda (application value)
                       (declare (ignore application))
                       (setf observed-value value)
                       ':continue))))
             (register-application-command command)
             (multiple-value-bind (application root)
                 (lisp-machine-tests--application)
               (unwind-protect
                    (let* ((ui (application-ui application))
                           (controller
                             (lisp-machine-tests--controller application))
                           (invocation
                             (application-command-invocation-parse
                              "/debug-required")))
                      (terminal-ui-start ui)
                      (let ((restart-names nil))
                        (test-call-with-function-replacements
                         (list
                          (list
                           'application-lisp--select-restart
                           (lambda (observed-application condition restarts)
                             (declare (ignore observed-application condition))
                             (setf restart-names
                                   (mapcar #'restart-name restarts))
                             (values
                              (find 'supply-arguments restarts
                                    :key #'restart-name)
                              "\"normal\""))))
                         (lambda ()
                           (test-assert
                            (eq (application--run-command-input
                                 application "/debug-required")
                                ':continue)
                            "normal slash dispatch recovers missing arguments")))
                        (test-assert
                         (and (string= observed-value "normal")
                              (member 'supply-arguments restart-names)
                              (member 'abort-user-operation restart-names))
                         "normal command recovery exposes live supply and abort restarts"))
                      (setf observed-value nil)
                      (test-call-with-function-replacements
                       (list
                        (list
                         'application-lisp--select-restart
                         (lambda (observed-application condition restarts)
                           (declare (ignore observed-application condition))
                           (values
                            (find 'supply-arguments restarts
                                  :key #'restart-name)
                            "\"responsive\""))))
                       (lambda ()
                         (test-assert
                          (eq (application-input-controller--run-responsive-command
                               controller command invocation)
                              ':continue)
                          "responsive command dispatch recovers missing arguments")))
                      (test-assert
                       (string= observed-value "responsive")
                       "responsive recovery retries the semantic command")
                      (setf observed-value nil)
                      (test-call-with-function-replacements
                       (list
                        (list
                         'application-lisp--select-restart
                         (lambda (observed-application condition restarts)
                           (declare (ignore observed-application condition restarts))
                           (values nil nil))))
                       (lambda ()
                         (test-assert
                          (eq (application--run-command-input
                               application "/debug-required")
                              ':aborted)
                          "cancelling command recovery returns safely to the prompt")))
                      (test-assert
                       (null observed-value)
                       "an aborted command never enters its semantic handler")
                     (test-call-with-function-replacements
                      (list
                       (list
                        'application-lisp--select-restart
                        (lambda (observed-application condition restarts)
                          (declare (ignore observed-application condition restarts))
                          (values nil nil))))
                      (lambda ()
                        (test-assert
                         (eq (application-input-controller--run-responsive-command
                              controller command invocation)
                             ':aborted)
                         "responsive cancellation returns safely to the prompt")))
                     (test-assert
                      (null observed-value)
                      "responsive cancellation never enters the command handler")
                     (let ((selector-calls 0)
                           (expected-condition nil))
                       (test-call-with-function-replacements
                        (list
                         (list
                          'application-lisp--select-restart
                          (lambda (observed-application condition restarts)
                            (declare
                             (ignore observed-application condition restarts))
                            (incf selector-calls)
                            (values nil nil))))
                        (lambda ()
                          (test-assert
                           (eq
                            (application--call-with-command-debugger
                             application
                             (lambda ()
                               (error 'configuration-error
                                      :message "expected failure"))
                             :expected-error-function
                             (lambda (observed-application condition)
                               (declare (ignore observed-application))
                               (setf expected-condition condition)))
                            ':failed)
                           "typed Autolith errors retain expected command handling")))
                       (test-assert
                        (and (zerop selector-calls)
                             (typep expected-condition 'configuration-error))
                        "expected command errors bypass the restart selector"))
                     (let ((selector-calls 0)
                           (fatal-condition nil))
                       (test-call-with-function-replacements
                        (list
                         (list
                          'application-lisp--select-restart
                          (lambda (observed-application condition restarts)
                            (declare
                             (ignore observed-application condition restarts))
                            (incf selector-calls)
                            (values nil nil)))
                         (list
                          'application-raise-fatal
                          (lambda (observed-application condition backtrace)
                            (declare (ignore observed-application backtrace))
                            (setf fatal-condition condition)
                            ':fatal)))
                        (lambda ()
                          (test-assert
                           (eq
                            (application--call-with-command-debugger
                             application
                             (lambda ()
                               (error
                                'active-image-corruption
                                :message "corruption"
                                :original-condition
                                (make-condition
                                 'simple-error
                                 :format-control "mutation failure"
                                 :format-arguments nil)
                                :restoration-condition
                                (make-condition
                                 'simple-error
                                 :format-control "restoration failure"
                                 :format-arguments nil))))
                            ':fatal)
                           "active image corruption reaches fatal command handling")))
                       (test-assert
                        (and (zerop selector-calls)
                             (typep fatal-condition 'active-image-corruption))
                        "fatal corruption bypasses the user restart selector")))
                 (close-application application root))))
        (application-command--registry-restore snapshot)))
    (let ((terminal
            (make-instance 'scripted-terminal
                           :columns 100
                           :styled-p t
                           :events (list :escape))))
      (multiple-value-bind (application root)
          (lisp-machine-tests--application :terminal terminal)
        (unwind-protect
             (let ((ui (application-ui application)))
               (terminal-ui-start ui)
               (multiple-value-bind
                     (values status condition restart-names selected-restart-name)
                   (application-lisp-call-with-ui-debugger
                    application
                    (lambda ()
                      (restart-case
                          (error "visible failure")
                        (use-value (value)
                          value))))
                 (declare (ignore values condition restart-names))
                 (test-assert
                  (and (eq status ':aborted)
                       (string= selected-restart-name
                                "ABORT-USER-OPERATION"))
                  "the visible picker escape selects prompt abort"))
               (let ((output
                       (clinedi:ansi-strip
                        (recording-terminal-output terminal))))
                 (test-assert
                  (and (search "restart debugger" output)
                       (search "condition: visible failure" output)
                       (search "live restarts" output)
                       (search "abort-user-operation" output))
                  "the debugger paints its condition, restart group, and abort row"))
               (setf (scripted-terminal-events terminal)
                     (list '(:insert "42") :submit))
               (terminal-ui-set-input ui "saved draft")
               (recording-terminal-reset terminal)
               (test-assert
                (string= (application-lisp--read-restart-value application) "42")
                "restart argument input returns the submitted Lisp form")
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (string= (line-editor-text (terminal-ui-editor ui))
                                "saved draft")
                       (search "* 42" (clinedi:ansi-strip output))
                       (search (terminal-style-sequence ':syntax-number) output))
                   "restart argument input highlights Lisp and restores the draft"))
               (let* ((saved-image (merge-pathnames "saved.png" root))
                      (temporary-image (merge-pathnames "temporary.png" root))
                      (saved-input
                        (user-message-input-create
                         :text "[Image #1] saved image draft"
                         :image-pathnames (list saved-image)))
                      (temporary-input
                        (user-message-input-create
                         :text "[Image #1] 42"
                         :image-pathnames (list temporary-image)))
                      (events
                        (list (list ':submit temporary-input)
                              (list ':escape nil))))
                 (terminal-ui-set-input ui saved-input)
                 (recording-terminal-reset terminal)
                 (test-call-with-function-replacements
                  (list
                   (list 'terminal-ui-read-event
                         (lambda (observed-ui)
                           (declare (ignore observed-ui))
                           ':test-event))
                   (list 'terminal-ui-process-event
                         (lambda (observed-ui event)
                           (declare (ignore observed-ui event))
                           (let ((next (pop events)))
                             (values (first next) (second next))))))
                  (lambda ()
                    (test-assert
                     (null (application-lisp--read-restart-value application))
                     "attachment rejection can cancel restart argument input")))
                 (let ((restored
                         (terminal-ui--submission-input
                          ui
                          (line-editor-text (terminal-ui-editor ui)))))
                   (test-assert
                    (and (typep restored 'user-message-input)
                         (string= (user-message-input-text restored)
                                  "[Image #1] saved image draft")
                         (equal (user-message-input-image-pathnames restored)
                                (list saved-image))
                         (search
                          "Restart argument input cannot include image attachments."
                          (clinedi:ansi-strip
                           (recording-terminal-output terminal))))
                    "restart input rejects new images and restores saved attachments"))))
          (close-application application root))))
    nil))


(-> test-application-debugger-recoveries () null)
(defun test-application-debugger-recoveries ()
  "Test typed synthetic recovery validation, execution, and metadata."
  (let* ((session (lisp-machine-tests--debugger-session))
         (snapshot (application-debugger-portable-snapshot session))
         (valid-recoveries
           (list
            (lisp-machine-tests--recovery
             ':invoke-restart :restart-id "restart-1" :argument-source "41")
            (lisp-machine-tests--recovery
             ':repair-and-invoke
             :restart-id "restart-1"
             :preparation-source "(setf *lisp-machine-test-repair-value* :ready)"
             :argument-source "*lisp-machine-test-repair-value*")
            (lisp-machine-tests--recovery ':retry-operation)
            (lisp-machine-tests--recovery
             ':repair-and-retry
             :preparation-source "(setf *lisp-machine-test-repair-value* :ready)")
            (lisp-machine-tests--recovery
             ':return-values :return-source "(values :replacement 7)")
            (lisp-machine-tests--recovery ':abort-operation))))
    (test-assert
     (and (application-debugger--portable-p snapshot)
          (string= (getf snapshot :source) "(error \"test failure\")")
          (eq (getf snapshot :operation-kind) ':lisp)
          (equal (getf snapshot :backtrace) '("frame one" "frame two"))
          (equal (getf snapshot :restarts)
                 '((:id "restart-1" :report "Use the supplied value."))))
     "debugger snapshots contain bounded portable condition and operation data")
    (test-assert
     (every (lambda (recovery)
              (eq (application-debugger--validate-recovery session recovery)
                  recovery))
            valid-recoveries)
     "every supported synthetic recovery validates against its capability set")
    (test-assert
     (every
      (lambda (recovery)
        (lisp-machine-tests--recovery-invalid-p session recovery))
      (list
       (lisp-machine-tests--recovery
        ':invoke-restart
        :restart-id "restart-1"
        :preparation-source "(values)")
       (lisp-machine-tests--recovery
        ':repair-and-invoke :restart-id "restart-1")
       (lisp-machine-tests--recovery
        ':retry-operation :argument-source "1")
       (lisp-machine-tests--recovery
        ':repair-and-retry
        :restart-id "restart-1"
        :preparation-source "(values)")
       (lisp-machine-tests--recovery ':return-values)
       (lisp-machine-tests--recovery
        ':abort-operation :return-source "nil")
       (lisp-machine-tests--recovery ':unsupported)))
     "recovery validation rejects forbidden fields and incomplete proposal kinds")
    (let ((restricted
            (lisp-machine-tests--debugger-session
             :retry-p nil :return-values-p nil)))
      (test-assert
       (and
        (lisp-machine-tests--recovery-invalid-p
         restricted (lisp-machine-tests--recovery ':retry-operation))
        (lisp-machine-tests--recovery-invalid-p
         restricted
         (lisp-machine-tests--recovery
          ':return-values :return-source "1")))
       "retry and replacement values require explicit operation capabilities")))
  (multiple-value-bind
        (values status condition restart-names selected-restart-name)
      (application-lisp-call-with-debugger
       (lambda ()
         (restart-case
             (error "invoke")
           (use-value (value)
             value)))
       :restart-selector
       (lambda (condition restarts)
         (declare (ignore condition restarts))
         (values
          (lisp-machine-tests--recovery
           ':invoke-restart :restart-id "restart-1" :argument-source "41")
          nil)))
    (declare (ignore condition restart-names))
    (test-assert
     (and (eq status ':ok)
          (equal values '(41))
          (string= selected-restart-name "USE-VALUE"))
     "an invoke recovery executes its source arguments against a live restart"))
  (setf *lisp-machine-test-repair-value* nil)
  (multiple-value-bind
        (values status condition restart-names selected-restart-name)
      (application-lisp-call-with-debugger
       (lambda ()
         (restart-case
             (error "repair and invoke")
           (use-value (value)
             value)))
       :restart-selector
       (lambda (condition restarts)
         (declare (ignore condition restarts))
         (values
          (lisp-machine-tests--recovery
           ':repair-and-invoke
           :restart-id "restart-1"
           :preparation-source
           "(setf *lisp-machine-test-repair-value* :prepared)"
           :argument-source "*lisp-machine-test-repair-value*")
          nil)))
    (declare (ignore condition restart-names))
    (test-assert
     (and (eq status ':ok)
          (equal values '(:prepared))
          (eq *lisp-machine-test-repair-value* ':prepared)
          (string= selected-restart-name "USE-VALUE"))
     "repair-and-invoke runs preparation before invoking the live restart"))
  (let ((calls 0))
    (multiple-value-bind
          (values status condition restart-names selected-restart-name)
        (application-lisp-call-with-debugger
         (lambda ()
           (if (= (incf calls) 1)
               (error "retry")
               :retried))
         :retry-p t
         :restart-selector
         (lambda (condition restarts)
           (declare (ignore condition restarts))
           (values (lisp-machine-tests--recovery ':retry-operation) nil)))
      (declare (ignore condition restart-names))
      (test-assert
       (and (= calls 2)
            (eq status ':ok)
            (equal values '(:retried))
            (string= selected-restart-name "RETRY-OPERATION"))
       "retry recovery reruns the explicit operation boundary")))
  (setf *lisp-machine-test-repair-value* nil)
  (let ((calls 0))
    (multiple-value-bind
          (values status condition restart-names selected-restart-name)
        (application-lisp-call-with-debugger
         (lambda ()
           (incf calls)
           (if *lisp-machine-test-repair-value*
               *lisp-machine-test-repair-value*
               (error "repair and retry")))
         :retry-p t
         :restart-selector
         (lambda (condition restarts)
           (declare (ignore condition restarts))
           (values
            (lisp-machine-tests--recovery
             ':repair-and-retry
             :preparation-source
             "(setf *lisp-machine-test-repair-value* :repaired)")
            nil)))
      (declare (ignore condition restart-names))
      (test-assert
       (and (= calls 2)
            (eq status ':ok)
            (equal values '(:repaired))
            (string= selected-restart-name "REPAIR-AND-RETRY"))
       "repair-and-retry applies source before rerunning the operation")))
  (multiple-value-bind
        (values status condition restart-names selected-restart-name)
      (application-lisp-call-with-debugger
       (lambda ()
         (error "replace values"))
       :return-values-p t
       :restart-selector
       (lambda (condition restarts)
         (declare (ignore condition restarts))
         (values
          (lisp-machine-tests--recovery
           ':return-values :return-source "(values :replacement 7)")
          nil)))
    (declare (ignore condition restart-names))
    (test-assert
     (and (eq status ':ok)
          (equal values '(:replacement 7))
          (string= selected-restart-name "RETURN-VALUES"))
     "return-values recovery supplies replacement multiple values"))
  (multiple-value-bind
        (values status condition restart-names selected-restart-name)
      (application-lisp-call-with-debugger
       (lambda ()
         (error "abort"))
       :restart-selector
       (lambda (condition restarts)
         (declare (ignore condition restarts))
         (values (lisp-machine-tests--recovery ':abort-operation) nil)))
    (declare (ignore values condition restart-names))
    (test-assert
     (and (eq status ':aborted)
          (string= selected-restart-name "ABORT-USER-OPERATION"))
     "abort recovery selects the explicit user-operation abort boundary"))
  (let ((metadata nil))
    (application-lisp-call-with-debugger
     (lambda ()
       (error "metadata"))
     :source nil
     :operation-kind ':command
     :retry-p nil
     :return-values-p t
     :restart-selector
     (lambda (condition restarts)
       (declare (ignore condition restarts))
       (setf metadata
             (list *application-debugger-source*
                   *application-debugger-operation-kind*
                   *application-debugger-retry-p*
                   *application-debugger-return-values-p*))
       (values nil nil)))
    (test-assert
     (equal metadata '("" :command nil t))
     "debugger metadata normalizes absent source and reaches the live selector"))
  nil)

(-> test-application-debugger-modal-recoveries () null)
(defun test-application-debugger-modal-recoveries ()
  "Test stable modal recovery choices and diagnosis cancellation."
  (let ((terminal (make-instance 'scripted-terminal :columns 100 :styled-p t)))
    (multiple-value-bind (application root)
        (lisp-machine-tests--application :terminal terminal)
      (let ((ui (application-ui application)))
        (unwind-protect
             (progn
               (terminal-ui-start ui)
               (let ((choices (list "ask-autolith" "AUTOLITH-RECOVERY-1"))
                     (cancel-count 0)
                     (all-items-valid-p t)
                     (proposal
                       (lisp-machine-tests--recovery
                        ':return-values
                        :return-source "(values :modal-recovery 9)")))
                 (test-call-with-function-replacements
                  (list
                   (list 'terminal-ui-select
                         (lambda (ignored &key items &allow-other-keys)
                           (declare (ignore ignored))
                           (setf all-items-valid-p
                                 (and all-items-valid-p
                                      (every #'terminal-completion-p items)
                                      (every (lambda (item)
                                               (typep (getf item :value)
                                                      '(option string)))
                                             items)))
                           (pop choices)))
                   (list 'application-debugger-start-diagnosis
                         (lambda (session configuration)
                           (declare (ignore configuration))
                           (setf (application-debugger-diagnosis-state session)
                                 ':complete
                                 (application-debugger-explanation session)
                                 "Use replacement values."
                                 (application-debugger-proposals session)
                                 (list proposal))
                           session))
                   (list 'application-debugger-cancel-diagnosis
                         (lambda (session)
                           (incf cancel-count)
                           (setf (application-debugger-diagnosis-state session)
                                 ':cancelled)
                           session)))
                  (lambda ()
                    (multiple-value-bind
                          (values status condition restart-names
                                  selected-restart-name)
                        (application-lisp-call-with-ui-debugger
                         application
                         (lambda ()
                           (error "modal recovery"))
                         :source nil
                         :operation-kind ':command
                         :retry-p t
                         :return-values-p t)
                      (declare (ignore condition restart-names))
                      (test-assert all-items-valid-p
                                   "diagnosis modal rows remain valid completions")
                      (test-assert (null choices)
                                   "diagnosis consumes the selected recovery choice")
                      (test-assert (zerop cancel-count)
                                   "completed diagnosis does not require cancellation")
                      (test-assert (eq status ':ok)
                                   "selected diagnosis recovery completes successfully")
                      (test-assert (equal values '(:modal-recovery 9))
                                   "selected diagnosis recovery returns replacement values")
                      (test-assert (string= selected-restart-name "RETURN-VALUES")
                                   "diagnosis resolves a stable name to its recovery object")))))
               (let ((choices (list "ask-autolith" "0"))
                     (cancel-count 0))
                 (test-call-with-function-replacements
                  (list
                   (list 'terminal-ui-select
                         (lambda (ignored &key &allow-other-keys)
                           (declare (ignore ignored))
                           (pop choices)))
                   (list 'application-debugger-start-diagnosis
                         (lambda (session configuration)
                           (declare (ignore configuration))
                           (setf (application-debugger-diagnosis-state session)
                                 ':running)
                           session))
                   (list 'application-debugger-cancel-diagnosis
                         (lambda (session)
                           (incf cancel-count)
                           (setf (application-debugger-diagnosis-state session)
                                 ':cancelled)
                           session)))
                  (lambda ()
                    (multiple-value-bind
                          (values status condition restart-names
                                  selected-restart-name)
                        (application-lisp-call-with-ui-debugger
                         application
                         (lambda ()
                           (restart-case
                               (error "live restart")
                             (continue ()
                               :continued))))
                      (declare (ignore condition restart-names))
                      (test-assert
                       (and (null choices)
                            (= cancel-count 1)
                            (eq status ':ok)
                            (equal values '(:continued))
                            (string= selected-restart-name "CONTINUE"))
                      "selecting a live restart cancels an active diagnosis"))))))
                (let ((choices (list "ask-autolith" "diagnosis-info" "0"))
                      (cancel-count 0)
                      (explanation-visible-p nil))
                  (test-call-with-function-replacements
                   (list
                    (list 'terminal-ui-select
                          (lambda (ignored &key items on-event poll-interval
                                             &allow-other-keys)
                            (declare (ignore ignored items))
                            (when poll-interval
                              (let* ((replacement (funcall on-event ':poll nil))
                                     (replacement-items (third replacement)))
                                (setf explanation-visible-p
                                      (some (lambda (item)
                                              (and (string= (getf item :value)
                                                            "diagnosis-info")
                                                   (search "The failure is understood."
                                                           (getf item :description))))
                                            replacement-items))))
                            (pop choices)))
                    (list 'application-debugger-start-diagnosis
                          (lambda (session configuration)
                            (declare (ignore configuration))
                            (setf (application-debugger-diagnosis-state session)
                                  ':complete
                                  (application-debugger-explanation session)
                                  "The failure is understood.")
                            session))
                    (list 'application-debugger-cancel-diagnosis
                          (lambda (session)
                            (incf cancel-count)
                            (setf (application-debugger-diagnosis-state session)
                                  ':cancelled)
                            session)))
                   (lambda ()
                     (multiple-value-bind
                           (values status condition restart-names
                                   selected-restart-name)
                         (application-lisp-call-with-ui-debugger
                          application
                          (lambda ()
                            (restart-case
                                (error "explain and return")
                              (continue ()
                                :continued-after-explanation))))
                       (declare (ignore condition restart-names))
                       (test-assert
                        (and (null choices)
                             explanation-visible-p
                             (zerop cancel-count)
                             (eq status ':ok)
                             (equal values '(:continued-after-explanation))
                             (string= selected-restart-name "CONTINUE"))
                        "diagnosis explanation returns to the same live restart selector")))))
          (ignore-errors (terminal-ui-stop ui))
          (ignore-errors
            (tool-registry-close-runtime-state
             (application-tool-registry application)))
          (platform-delete-directory-tree *platform* root
                                          :validate t
                                          :if-does-not-exist ':ignore)))))
  nil)

(-> test-application-lisp-activity () null)
(defun test-application-lisp-activity ()
  "Test local evaluation remains visible without replacing provider activity."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let ((ui (application-ui application)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (terminal-ui-set-status ui "provider active")
             (setf *lisp-machine-test-activity* nil
                   *lisp-machine-test-interactive-p* nil)
             (test-assert
              (eq (application-run-lisp-input
                   application
                   "(progn (setf *lisp-machine-test-activity* (terminal-ui-local-activity (application-ui *application-operation-application*))) (setf *lisp-machine-test-interactive-p* *application-command-interactive-p*))")
                  ':continue)
              "explicit local Lisp completes beside provider activity")
             (test-assert
              (and (string= *lisp-machine-test-activity*
                            "evaluating local Lisp")
                   *lisp-machine-test-interactive-p*
                   (string= (terminal-ui-status ui) "provider active")
                   (null (terminal-ui-local-activity ui)))
              "local Lisp receives interactive context without clobbering status"))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (platform-delete-directory-tree *platform* root
                                        :validate t
                                        :if-does-not-exist ':ignore))))
  nil)


;;;; -- Responsive Input Integration --

(-> test-application-lisp-input-routing () null)
(defun test-application-lisp-input-routing ()
  "Test exact Lisp classification, multiline continuation, queues, and execution."
  (test-assert
   (and (null (application--message-input "(values 1 2)"))
        (equal (application-input-controller--input-work "(values 1 2)")
               '(:lisp "(values 1 2)"))
        (equal
         (application-input-controller--restore-work-item
          (application-input-controller--pending-work-entry-form
           '(:lisp "(values 1 2)")))
         '(:lisp "(values 1 2)")))
   "explicit Lisp stays outside model routing and survives pending persistence")
  (test-assert
   (string= (application--message-input " (values 1 2)")
            " (values 1 2)")
   "leading whitespace preserves prose routing")
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (terminal (terminal-ui-terminal ui))
           (controller (lisp-machine-tests--controller application))
           (work-items (application-input-controller-work-items controller)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (terminal-ui-set-input ui "(list 1")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (and (string= (line-editor-text (terminal-ui-editor ui))
                            (format nil "(list 1~%"))
                   (null
                    (application-input-controller--state controller :work-items)))
              "Enter continues an incomplete Lisp form without submitting work")
             (terminal-ui-set-input ui "(values 3 4)")
             (application-input-controller--process-event controller ':submit)
             (test-assert
              (equal (application-input-controller--state controller :work-items)
                     '((:lisp "(values 3 4)")))
              "a complete Lisp form enters the durable local work queue")
             (deque-clear work-items)
             (setf (application-input-controller-active-p controller) t)
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(resource.read :uri \"workspace:.\")")
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (null
                      (application-input-controller--state controller :work-items))
                     (search "(resource.read :uri \"workspace:.\")" output)
                     (not (search "scheduled" output :test #'char-equal)))
                "a nonconflicting registered operation runs beside the active turn"))
             (recording-terminal-reset terminal)
             (setf *lisp-machine-test-value* nil)
             (application-input-controller--handle-submission
              controller
              "(eval-now (setf *lisp-machine-test-value* :immediate))")
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (eq *lisp-machine-test-value* ':immediate)
                     (null
                      (application-input-controller--state controller :work-items))
                     (search "⇒ :IMMEDIATE" output))
                "eval-now explicitly forces arbitrary local evaluation immediately"))
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(self.eval :form \"(+ 1 2)\")")
             (test-assert
              (and (equal (application-input-controller--state controller :work-items)
                          '((:lisp "(self.eval :form \"(+ 1 2)\")")))
                   (search "local evaluation scheduled"
                           (recording-terminal-output terminal)
                           :test #'char-equal))
              "active-image tools wait for the serialized application boundary")
             (deque-clear work-items)
             (recording-terminal-reset terminal)
             (let ((authorization-calls 0))
               (test-call-with-function-replacements
                (list
                 (list 'application-authorize-command
                       (lambda (observed command directory)
                         (declare (ignore observed command directory))
                         (incf authorization-calls)
                         ':sandboxed)))
                (lambda ()
                  (application-input-controller--handle-submission
                   controller "(shell.run :command \"true\")")))
               (test-assert
                (and (zerop authorization-calls)
                     (equal (application-input-controller--state controller :work-items)
                            '((:lisp "(shell.run :command \"true\")")))
                     (search "local evaluation scheduled"
                             (recording-terminal-output terminal)
                             :test #'char-equal))
                "approval-requiring tools wait instead of joining the reader"))
             (deque-clear work-items)
             (recording-terminal-reset terminal)
             (let ((previous-reader
                     (application-input-controller-reader-thread controller)))
               (unwind-protect
                    (progn
                      (setf (application-input-controller-reader-thread controller)
                            (current-thread))
                      (application-input-controller--handle-submission
                       controller
                       "(eval-now (shell.run :command \"true\"))"))
                 (setf (application-input-controller-reader-thread controller)
                       previous-reader)))
             (let ((output
                     (clinedi:ansi-strip
                      (recording-terminal-output terminal))))
               (test-assert
                (and (null
                      (application-input-controller--state controller :work-items))
                     (search "Terminal-owning work cannot pause" output)
                     (search "aborted" output :test #'char-equal))
                "eval-now rejects modal authorization instead of self-joining"))
             (deque-clear work-items)
             (recording-terminal-reset terminal)
             (application-input-controller--handle-submission
              controller "(values :later)")
             (test-assert
              (and (equal (application-input-controller--state controller :work-items)
                          '((:lisp "(values :later)")))
                   (null
                    (application-input-controller--state controller :steering-items))
                   (search "local evaluation scheduled"
                           (recording-terminal-output terminal)
                           :test #'char-equal))
              "busy arbitrary Lisp waits for the idle boundary instead of steering")
             (deque-clear work-items)
             (setf (application-input-controller-active-p controller) nil
                   (application-input-controller-stopping-p controller) t)
             (let ((provider-items-before
                     (copy-list
                      (conversation-input-items
                       (application-conversation application)))))
               (application-input-controller--run-work
                controller '(:lisp "(values 7 8)"))
               (let ((output
                       (clinedi:ansi-strip
                        (recording-terminal-output terminal))))
                 (test-assert
                  (and (search "(values 7 8)" output)
                       (search "⇒ 7" output)
                       (search "⇒ 8" output)
                       (equal provider-items-before
                              (conversation-input-items
                               (application-conversation application))))
                  "local work shows exact source and values outside ordinary provider history")))
             (let ((quit-controller
                     (lisp-machine-tests--controller application)))
               (test-call-with-function-replacements
                (list
                 (list 'application-input-controller-call-with-reader-paused
                       (lambda (ignored function)
                         (declare (ignore ignored))
                         (funcall function))))
                (lambda ()
                  (application-input-controller--run-work
                   quit-controller '(:lisp "(quit)"))))
               (test-assert
                (and (application-input-controller-stopping-p quit-controller)
                     (eq (application-input-controller-exit-reason quit-controller)
                         ':quit))
                "a Lisp quit operation exits through the responsive controller")))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (platform-delete-directory-tree *platform* root
                                        :validate t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-application-prompt-marker-reader-order () null)
(defun test-application-prompt-marker-reader-order ()
  "Test that an idle prompt is marked before its terminal reader can accept input."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (terminal (terminal-ui-terminal ui))
           (controller nil)
           (reader-state nil)
           (reader-output nil)
           (prompt-start
             (terminal--prompt-marker-sequence ':prompt-start 0))
           (input-start
             (semantic-prompt-marker-sequence ':input-start)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (recording-terminal-reset terminal)
             (test-call-with-function-replacements
              (list
               (list 'application-input-controller--start-reader
                     (lambda (observed-controller)
                       (declare (ignore observed-controller))
                       (setf reader-state
                             (terminal-ui-prompt-marker-state ui)
                             reader-output
                             (recording-terminal-output terminal))
                       nil)))
               (lambda ()
                 (setf controller
                       (application-input-controller-create
                        application
                        :initial-work-items
                        '((:recovery-diagnosis "internal"))
                        :load-pending-p nil))))
             (test-assert
              (and (eq reader-state ':input)
                   (= (terminal-tests--substring-count
                       prompt-start reader-output)
                      1)
                   (= (terminal-tests--substring-count
                       input-start reader-output)
                      1)
                   (< (search prompt-start reader-output)
                      (search input-start reader-output))
                   (null
                    (application-input-controller-reader-thread controller)))
               "initial work still emits A/B before starting its reader"))
        (when controller
          (ignore-errors (application-input-controller-stop controller)))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (platform-delete-directory-tree *platform* root
                                        :validate t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-application-prompt-marker-lifecycle () null)
(defun test-application-prompt-marker-lifecycle ()
  "Test prompt blocks around foreground, failed, cancelled, and immediate Lisp."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (terminal (terminal-ui-terminal ui))
           (controller (lisp-machine-tests--controller application)))
      (labels ((marker (payload)
                 (if (string= payload "A")
                     (terminal--prompt-marker-sequence ':prompt-start 0)
                     (format nil "~C]133;~A~C~C"
                             *terminal-escape-character*
                             payload
                             *terminal-escape-character*
                             #\\)))

               (markers-in-order-p (output payloads)
                 (block nil
                   (let ((start 0))
                     (dolist (payload payloads t)
                       (let* ((control (marker payload))
                              (position (search control output :start2 start)))
                         (unless position
                           (return nil))
                         (setf start (+ position (length control))))))))

               (run-lisp-work (source &key cancel-p)
                 (setf (application-input-controller-active-p controller) t
                       (application-input-controller-active-work-interactive-p
                        controller)
                       t)
                 (application-input-controller--run-work
                  controller (list ':lisp source))
                 (when cancel-p
                   (setf (application-input-controller-turn-cancellation-p
                          controller)
                         t))
                 (application-input-controller--finish-work controller)))
        (unwind-protect
             (progn
               (terminal-ui-start ui)
               (recording-terminal-reset terminal)
               (terminal-ui-open-prompt-block ui)
               (test-call-with-function-replacements
                (list
                 (list 'application-input-controller-call-with-reader-paused
                       (lambda (ignored function)
                         (declare (ignore ignored))
                         (funcall function))))
                (lambda ()
                  (run-lisp-work "(values :ok)")
                  (run-lisp-work "(error \"marker failure\")")
                  (run-lisp-work "(values :cancelled)" :cancel-p t)))
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and
                   (markers-in-order-p
                    output
                    '("A" "B" "C" "D;0"
                      "A" "B" "C" "D;1"
                      "A" "B" "C" "D;1"
                      "A" "B"))
                   (= (terminal-tests--substring-count (marker "A") output) 4)
                   (= (terminal-tests--substring-count (marker "B") output) 4)
                   (= (terminal-tests--substring-count (marker "C") output) 3)
                   (= (terminal-tests--substring-count (marker "D;0") output) 1)
                   (= (terminal-tests--substring-count (marker "D;1") output) 2))
                  "foreground Lisp emits ordered success, failure, and cancellation blocks"))
               (let ((execution-count
                       (terminal-tests--substring-count
                        (marker "C") (recording-terminal-output terminal)))
                     (completion-count
                       (+
                        (terminal-tests--substring-count
                         (marker "D;0") (recording-terminal-output terminal))
                        (terminal-tests--substring-count
                         (marker "D;1") (recording-terminal-output terminal)))))
                 (setf (application-input-controller-active-p controller) t
                       (application-input-controller-active-work-interactive-p
                        controller)
                       nil)
                 (application-input-controller--begin-prompt-work
                  controller '(:recovery-diagnosis "internal"))
                 (application-input-controller--finish-work controller)
                 (test-assert
                  (and
                   (= execution-count
                      (terminal-tests--substring-count
                       (marker "C") (recording-terminal-output terminal)))
                   (= completion-count
                      (+
                       (terminal-tests--substring-count
                        (marker "D;0") (recording-terminal-output terminal))
                       (terminal-tests--substring-count
                        (marker "D;1") (recording-terminal-output terminal)))))
                  "startup and internal work add no command marker block"))
               (terminal-ui-start-prompt-execution ui)
               (recording-terminal-reset terminal)
               (test-assert
                (eq
                 (application-input-controller--run-responsive-lisp
                  controller "(values :nested)")
                 ':continue)
                "immediate local Lisp still runs inside active prompt work")
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and
                   (zerop
                    (terminal-tests--substring-count (marker "A") output))
                   (zerop
                    (terminal-tests--substring-count (marker "B") output))
                   (zerop
                    (terminal-tests--substring-count (marker "C") output))
                   (zerop
                    (terminal-tests--substring-count (marker "D;0") output)))
                  "immediate active-turn Lisp does not nest another marker block"))
               (terminal-ui-finish-prompt-block ui 0)
               (terminal-ui-open-prompt-block ui)
               (recording-terminal-reset terminal)
               (test-call-with-function-replacements
                (list
                 (list 'application-input-controller-call-with-reader-paused
                       (lambda (ignored function)
                         (declare (ignore ignored))
                         (funcall function))))
                (lambda ()
                  (run-lisp-work "(quit)")))
               (let ((output (recording-terminal-output terminal)))
                 (test-assert
                  (and (application-input-controller-stopping-p controller)
                       (markers-in-order-p output '("C" "D;0"))
                       (zerop
                        (terminal-tests--substring-count (marker "A") output))
                       (zerop
                        (terminal-tests--substring-count (marker "B") output))
                       (eq (terminal-ui-prompt-marker-state ui) ':closed))
                  "quit completes its block without opening another prompt")))
          (ignore-errors (application-input-controller-stop controller))
          (ignore-errors (terminal-ui-stop ui))
          (ignore-errors
            (tool-registry-close-runtime-state
             (application-tool-registry application)))
          (platform-delete-directory-tree *platform* root
                                          :validate t
                                          :if-does-not-exist ':ignore)))))
  nil)

(-> run-lisp-machine-tests () null)
(defun run-lisp-machine-tests ()
  "Run direct local Lisp evaluation and responsive routing tests."
  (test-application-lisp-evaluation)
  (test-application-restart-debugger)
  (test-application-debugger-recoveries)
  (test-application-debugger-modal-recoveries)
  (test-application-lisp-activity)
  (test-application-lisp-input-routing)
  (test-application-prompt-marker-reader-order)
  (test-application-prompt-marker-lifecycle)
  nil)
