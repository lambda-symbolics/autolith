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

(-> lisp-machine-tests--debugger-session () application-debugger-session)
(defun lisp-machine-tests--debugger-session ()
  "Capture a detached diagnosis session through the live debugger adapter."
  (let (session)
    (application-lisp-call-with-debugger
     (lambda () (error "test failure"))
     :source          "(error \"test failure\")"
     :return-values-p t
     :restart-selector
     (lambda (condition restarts)
       (declare (ignore condition restarts))
       (setf session (make-instance 'application-debugger-session
                                    :snapshot *application-debugger-snapshot*))
       (values nil nil)))
    session))

(-> lisp-machine-tests--application
    (&key (:terminal (option terminal)))
    (values application pathname))
(defun lisp-machine-tests--application (&key terminal)
  "Return a temporary application using TERMINAL and its data root."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier (make-identifier)))
         (ui
           (terminal-ui-create
            :terminal
            (or terminal
                (make-instance 'recording-terminal
                               :columns  100
                               :styled-p t))))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry (make-default-tool-registry)
                          :ui            ui)))
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
  (let ((evaluation
          (application-lisp-evaluate
           "(error 'application-operation-loop-action :action :quit)")))
    (test-assert
     (and (eq (application-lisp-evaluation-status evaluation) ':ok)
          (eq (application-lisp-evaluation-loop-action evaluation) ':quit)
          (null (application-lisp-evaluation-values evaluation)))
     "Autolith operation controls complete evaluation with their loop action"))
  (dolist (condition
           (list (make-condition 'rollback-requested
                                 :message       "requested rollback"
                                 :generation-id "generation")
                 (make-condition 'update-requested
                                 :message "requested update"
                                 :tag     "v9.9.9")))
    (let ((selector-calls 0))
      (test-assert
       (handler-case
           (application-lisp-call-with-debugger
            (lambda () (error condition))
            :restart-selector
            (lambda (condition restarts)
              (declare (ignore condition restarts))
              (incf selector-calls)))
         (autolith-control-condition (observed)
           (and (eq observed condition) (zerop selector-calls))))
       "process handoff controls bypass the debugger adapter")))
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
       "restart argument source resolves AUTOLITH symbols from CL-USER")))
  nil)

(-> test-application-debugger-diagnosis () null)
(defun test-application-debugger-diagnosis ()
  "Test a real debugger diagnosis thread with a scripted agent response."
  (with-test-configuration (configuration)
    (let ((session           (lisp-machine-tests--debugger-session))
          (diagnosis-context nil))
      (let* ((datum (format nil "bad~Cvalue" *terminal-escape-character*))
             (condition
               (make-condition 'type-error
                               :datum         datum
                               :expected-type 'integer))
             (entry (application-lisp--debugger-condition-entry condition))
             (text  (terminal--spans-text entry))
             (fallback
               (terminal--spans-text
                (application-lisp--debugger-condition-entry
                 (make-condition 'condition)))))
        (test-assert
         (and (search "integer" text :test #'char-equal)
              (search "bad" text :test #'char-equal)
              (not (find *terminal-escape-character* text))
              (eq (terminal-span-style (first entry)) ':failure)
              (some (lambda (span)
                      (eq (terminal-span-style span) ':code))
                    entry))
         "typed condition metadata renders sanitized expected type and datum safely")
        (test-assert (non-empty-string-p fallback)
                     "unknown conditions retain a printable fallback report"))
      (test-call-with-function-replacements
       (list
        (list 'provider-create
              (lambda (&rest ignored)
                (declare (ignore ignored))
                nil))
        (list 'agent-create
              (lambda (&rest ignored)
                (declare (ignore ignored))
                nil))
        (list 'agent-run-user-turn
              (lambda (&rest arguments)
                  (setf diagnosis-context (second arguments))
                  (let ((observer (getf (member :observer arguments) :observer)))
                    (agent-observer-text observer "Scripted diagnosis.")))))
       (lambda ()
         (application-debugger-start-diagnosis session configuration)
         (join-thread (application-debugger-diagnosis-thread session))))
      (test-assert
       (and (stringp diagnosis-context)
            (search (write-to-string (application-debugger-portable-snapshot session))
                    diagnosis-context))
       "the diagnosis worker receives the detached library snapshot")
      (let ((status (application-debugger-poll session)))
        (test-assert (and (eq (getf status :state) ':complete)
                          (string= (getf status :explanation)
                                   "Scripted diagnosis."))
                     "a scripted diagnosis completes with its explanation")
        (test-assert (null (getf status :failure))
                     "a completed diagnosis has no failure"))))
  nil)

(-> test-application-command-debugger () null)
(defun test-application-command-debugger ()
  "Test command debugger ownership for missing arguments and typed failures."
  (let ((snapshot           (application-command--registry-snapshot))
        (observed-value     nil)
        (selector-calls     0)
        (expected-condition nil)
        (fatal-condition    nil))
    (unwind-protect
         (progn
           (register-application-command
            (application-command-create
             :definition-name   'lisp-machine-tests--required-debugger-command
             :name              "/debug-required"
             :aliases           nil
             :argument          "VALUE"
             :description       "exercise required command recovery"
             :tip               "tests required command recovery."
             :busy-behavior     ':execute
             :terminal-behavior ':shared
             :lambda-list       '(value)
             :callable-p        t
             :handler           (lambda (application value)
                                  (declare (ignore application))
                                  (setf observed-value value)
                                  ':continue)))
           (multiple-value-bind (application root)
               (lisp-machine-tests--application)
             (unwind-protect
                  (let* ((ui         (application-ui application))
                         (controller (lisp-machine-tests--controller application))
                         (command    (application-command-find "/debug-required"))
                         (invocation (application-command-invocation-parse
                                      "/debug-required")))
                    (terminal-ui-start ui)
                    (test-call-with-function-replacements
                     (list
                      (list 'application-lisp--select-restart
                            (lambda (application condition restarts)
                              (declare (ignore application condition))
                              (incf selector-calls)
                              (values (find 'supply-arguments restarts
                                            :key #'restart-name)
                                      "\"recovered\""))))
                     (lambda ()
                       (test-assert
                        (eq (application--run-command-input
                             application "/debug-required") ':continue)
                        "normal command recovery reaches its semantic handler")
                       (setf observed-value nil)
                       (test-assert
                        (eq (application-input-controller--run-responsive-command
                             controller command invocation)
                            ':continue)
                        "responsive command recovery reaches its semantic handler")))
                    (test-assert (and (string= observed-value "recovered")
                                      (= selector-calls 2))
                                 "both command paths supply required arguments")
                    (setf observed-value nil)
                    (test-call-with-function-replacements
                     (list
                      (list 'application-lisp--select-restart
                            (lambda (&rest ignored)
                              (declare (ignore ignored))
                              (values nil nil))))
                     (lambda ()
                       (test-assert
                        (eq (application--run-command-input
                             application "/debug-required") ':aborted)
                        "normal command cancellation returns to the prompt")
                       (test-assert
                        (eq (application-input-controller--run-responsive-command
                             controller command invocation)
                            ':aborted)
                        "responsive command cancellation returns to the prompt")))
                    (test-assert (null observed-value)
                                 "cancelled commands do not call their handler")
                    (setf selector-calls 0)
                    (test-call-with-function-replacements
                     (list
                      (list 'application-lisp--select-restart
                            (lambda (&rest ignored)
                              (declare (ignore ignored))
                              (incf selector-calls)
                              (values nil nil)))
                      (list 'application-raise-fatal
                            (lambda (application condition backtrace)
                              (declare (ignore application backtrace))
                              (setf fatal-condition condition)
                              ':fatal)))
                     (lambda ()
                       (test-assert
                        (eq (application--call-with-command-debugger
                             application
                             (lambda ()
                               (error 'configuration-error :message "expected"))
                             :expected-error-function
                             (lambda (application condition)
                               (declare (ignore application))
                               (setf expected-condition condition)))
                            ':failed)
                        "expected command errors use the semantic error path")
                       (test-assert
                        (eq (application--call-with-command-debugger
                             application
                             (lambda ()
                               (error 'active-image-corruption
                                      :message               "corruption"
                                      :original-condition    (make-condition 'simple-error)
                                      :restoration-condition (make-condition 'simple-error))))
                            ':fatal)
                        "active-image corruption uses fatal command handling")))
                    (test-assert (and (zerop selector-calls)
                                      (typep expected-condition 'configuration-error)
                                      (typep fatal-condition 'active-image-corruption))
                                 "expected and fatal errors bypass restart selection"))
               (ignore-errors (terminal-ui-stop (application-ui application)))
               (ignore-errors
                 (tool-registry-close-runtime-state
                  (application-tool-registry application)))
               (platform-delete-directory-tree *platform* root
                                               :validate          t
                                               :if-does-not-exist ':ignore))))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-application-debugger-modal-recoveries () null)
(defun test-application-debugger-modal-recoveries ()
  "Test modal presentation, argument editing, recovery choices, and cancellation."
  (let ((terminal (make-instance 'scripted-terminal :columns 100 :styled-p t)))
    (multiple-value-bind (application root)
        (lisp-machine-tests--application :terminal terminal)
      (let ((ui (application-ui application)))
        (unwind-protect
             (progn
               (terminal-ui-start ui)
               (let ((select #'terminal-ui-select) items initial-name)
                 (setf (scripted-terminal-events terminal) (list :escape))
                 (test-call-with-function-replacements
                  (list
                   (list 'terminal-ui-select
                         (lambda (ui &rest arguments)
                           (setf items (getf arguments :items)
                                 initial-name (getf arguments :initial-name))
                           (apply select ui arguments))))
                  (lambda ()
                    (multiple-value-bind (values status condition restarts selected)
                        (application-lisp-call-with-ui-debugger
                         application
                         (lambda ()
                           (restart-case (error "visible failure")
                             (use-value (value) value))))
                      (declare (ignore values condition restarts))
                      (test-assert
                       (and (eq status ':aborted)
                            (string= selected "ABORT-USER-OPERATION"))
                       "Escape in the visible restart modal aborts its operation"))))
                 (let* ((live-items (remove-if-not
                                     (lambda (item)
                                       (equal (getf item :group) "live restarts"))
                                     items))
                        (value-item (find-if
                                     (lambda (item)
                                       (search "use-value" (getf item :description)))
                                     live-items))
                        (abort-item (find-if
                                     (lambda (item)
                                       (search "abort-user-operation"
                                               (getf item :description)))
                                     live-items))
                        (output     (clinedi:ansi-strip
                                     (recording-terminal-output terminal))))
                   (test-assert
                    (and value-item abort-item
                         (every #'terminal-completion-p items)
                         (equal initial-name (getf value-item :name))
                         (eq (terminal-span-style
                              (first (getf value-item :description-spans))) ':code)
                         (eq (terminal-span-style
                              (first (getf abort-item :description-spans))) ':failure)
                         (search "visible failure" output)
                         (search "live restarts" output)
                         (search "abort-user-operation" output))
                    "the modal renders the condition and styled, grouped restart choices")))
               (setf (scripted-terminal-events terminal)
                     (list '(:insert "42") :submit))
               (terminal-ui-set-input ui "saved draft")
               (recording-terminal-reset terminal)
               (test-assert
                (string= (application-lisp--read-restart-value application) "42")
                "restart argument input returns its submitted Lisp form")
               (test-assert
                (and (string= (line-editor-text (terminal-ui-editor ui)) "saved draft")
                     (search (terminal-style-sequence ':syntax-number)
                             (recording-terminal-output terminal)))
                "argument input uses Lisp highlighting and restores the draft")
               (let* ((saved-image     (merge-pathnames "saved.png" root))
                      (temporary-image (merge-pathnames "temporary.png" root))
                      (saved-input     (user-message-input-create
                                        :text            "[Image #1] saved image draft"
                                        :image-pathnames (list saved-image)))
                      (temporary-input (user-message-input-create
                                        :text            "[Image #1] 42"
                                        :image-pathnames (list temporary-image)))
                      (events          (list (list ':submit temporary-input)
                                             (list ':escape nil))))
                 (terminal-ui-set-input ui saved-input)
                 (recording-terminal-reset terminal)
                 (test-call-with-function-replacements
                  (list
                   (list 'terminal-ui-read-event
                         (lambda (ui) (declare (ignore ui)) ':test-event))
                   (list 'terminal-ui-process-event
                         (lambda (ui event)
                           (declare (ignore ui event))
                           (let ((next (pop events)))
                             (values (first next) (second next))))))
                  (lambda ()
                    (test-assert
                     (null (application-lisp--read-restart-value application))
                     "restart argument editing can cancel after rejecting an image")))
                 (let ((restored (terminal-ui--submission-input
                                  ui (line-editor-text (terminal-ui-editor ui)))))
                   (test-assert
                    (and (null events)
                         (equal (user-message-input-image-pathnames restored)
                                (list saved-image))
                         (string= (user-message-input-text restored)
                                  (user-message-input-text saved-input))
                         (search "cannot include image attachments"
                                 (clinedi:ansi-strip
                                  (recording-terminal-output terminal))))
                    "argument editing rejects submitted images and restores saved attachments")))
               (let ((choices           (list "ask-autolith" "AUTOLITH-RECOVERY-1"))
                     (cancel-count      0)
                     (all-items-valid-p t)
                     (proposal
                       (lambda-debugger:make-recovery
                        :kind          ':return-values
                        :report        "Use replacement values."
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
                         :source          nil
                         :operation-kind  ':command
                         :retry-p         t
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
               (let ((choices      (list "ask-autolith" "0"))
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
             (let ((choices               (list "ask-autolith" "diagnosis-info" "0"))
                   (cancel-count          0)
                   (explanation-visible-p nil))
               (test-call-with-function-replacements
                (list
                 (list 'terminal-ui-select
                       (lambda (ignored &key items on-event poll-interval
                                          &allow-other-keys)
                         (declare (ignore ignored items))
                         (when poll-interval
                           (let* ((replacement       (funcall on-event ':poll nil))
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
                                          :validate          t
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
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)


;;;; -- Responsive Input Integration --

(-> test-application-lisp-input-routing () null)
(defun test-application-lisp-input-routing ()
  "Test exact Lisp classification, multiline continuation, queues, and execution."
  (test-assert
   (and (null (input-message "(values 1 2)"))
        (equal (input-work-from-input "(values 1 2)")
               '(:lisp "(values 1 2)"))
        (equal
         (input-work-from-form
          (input-work->form
           '(:lisp "(values 1 2)")))
         '(:lisp "(values 1 2)")))
   "explicit Lisp stays outside model routing and survives pending persistence")
  (test-assert
   (string= (input-message " (values 1 2)")
            " (values 1 2)")
   "leading whitespace preserves prose routing")
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui         (application-ui application))
           (terminal   (terminal-ui-terminal ui))
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
              controller "(self.eval :forms (vector \"(+ 1 2)\"))")
             (test-assert
              (and (equal (application-input-controller--state controller :work-items)
                          '((:lisp "(self.eval :forms (vector \"(+ 1 2)\"))")))
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
             ;; Submissions run on the reader thread in a live session.
             (test-call-with-function-replacements
              (list (list 'application-input-controller-reader-thread-p
                          (lambda (controller)
                            (declare (ignore controller))
                            t)))
              (lambda ()
                (application-input-controller--handle-submission
                 controller
                 "(eval-now (shell.run :command \"true\"))")))
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
               (application-input-controller--run-work
                quit-controller '(:lisp "(quit)"))
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
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-application-prompt-marker-reader-order () null)
(defun test-application-prompt-marker-reader-order ()
  "Test that an idle prompt is marked before its terminal reader can accept input."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui            (application-ui application))
           (terminal      (terminal-ui-terminal ui))
           (controller    nil)
           (reader-state  nil)
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
                   (not (application-input-controller-reader-live-p controller)))
              "initial work still emits A/B before starting its reader"))
        (when controller
          (ignore-errors (application-input-controller-stop controller)))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (platform-delete-directory-tree *platform* root
                                        :validate          t
                                        :if-does-not-exist ':ignore))))
  nil)

(-> test-application-prompt-marker-lifecycle () null)
(defun test-application-prompt-marker-lifecycle ()
  "Test prompt blocks around foreground, failed, cancelled, and immediate Lisp."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui         (application-ui application))
           (terminal   (terminal-ui-terminal ui))
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
                       (let* ((control  (marker payload))
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
               (run-lisp-work "(values :ok)")
               (run-lisp-work "(error \"marker failure\")")
               (run-lisp-work "(values :cancelled)" :cancel-p t)
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
               (run-lisp-work "(quit)")
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
                                          :validate          t
                                          :if-does-not-exist ':ignore)))))
  nil)

(-> test-application-program-status-continuation () null)
(defun test-application-program-status-continuation ()
  "Check terminal status for model work resumed without new prompt execution."
  (multiple-value-bind (application root) (lisp-machine-tests--application)
    (let* ((ui         (application-ui application))
           (controller (lisp-machine-tests--controller application)))
      (unwind-protect
           (with-terminal-ui (active ui)
             (terminal-ui-open-prompt-block active)
             (dolist (kind '(:job-completion :operation-request :recovery-diagnosis))
               (setf (application-input-controller-active-work-kind controller) kind)
               (application-input-controller--begin-prompt-work controller (list kind))
               (test-assert (equal (terminal-ui-program-status active) '(:working nil))
                            "a model continuation reports working")
               (application-input-controller--record-prompt-result controller ':failed)
               (application-input-controller--finish-work controller)
               (test-assert (and (equal (terminal-ui-program-status active) '(:error nil))
                                 (eq (terminal-ui-prompt-marker-state active) ':input))
                            "a failed continuation reports error without an OSC 133 execution block")))
        (application-input-controller-stop controller)
        (tool-registry-close-runtime-state (application-tool-registry application))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)
