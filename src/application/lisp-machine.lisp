(in-package #:autolith)

;;;; -- Local Common Lisp Evaluation --

(deftype application-lisp-evaluation-status ()
  "The terminal outcome of one explicit local Common Lisp evaluation."
  '(member :ok :aborted :error))

(defstruct (application-lisp-evaluation
             (:constructor application-lisp-evaluation-create
                 (&key status output values value-kinds condition restart-names
                        selected-restart-name loop-action)))
  "Captured outcome of one explicit active-image Common Lisp evaluation."
  (status ':ok :type application-lisp-evaluation-status)
  (output "" :type string)
  (values nil :type list)
  (value-kinds nil :type list)
  (condition nil :type (option string))
  (restart-names nil :type list)
  (selected-restart-name nil :type (option string))
  (loop-action nil :type (option (member :quit))))

(defparameter *application-lisp-value-restart-names*
  '(supply-arguments use-value store-value)
  "Restart names whose ordinary interactive use expects Lisp arguments.")

(-> application-lisp-input-incomplete-p (string) boolean)
(defun application-lisp-input-incomplete-p (source)
  "Return true when SOURCE begins a Common Lisp form that needs more input.

The probe disables read-time evaluation so submission classification cannot run
SOURCE twice. Reader errors other than end of file are complete submissions and
are presented by the evaluator."
  (handler-case
      (progn
        (self-read-form source :read-eval nil)
        nil)
    (end-of-file ()
      t)
    (reader-error ()
      nil)
    (error ()
      nil)))

(-> application-lisp-input-with-text
    ((or string user-message-input) string)
    (or string user-message-input))
(defun application-lisp-input-with-text (input text)
  "Return INPUT with exact replacement TEXT and preserved image attachments."
  (etypecase input
    (string
     (copy-seq text))
    (user-message-input
     (user-message-input-create
      :text text
      :image-pathnames (user-message-input-image-pathnames input)))))

(-> application-lisp--control-condition-p (condition) boolean)
(defun application-lisp--control-condition-p (condition)
  "Return true when CONDITION belongs to Autolith's own control boundary."
  (typep condition
         '(or application-operation-loop-action
              application-turn-cancelled
              application-input-failed
              autolith-control-condition
              agent-loop-error
              conversation-invariant-error
              active-image-corruption)))

(-> application-lisp--restart-report (restart) string)
(defun application-lisp--restart-report (restart)
  "Return RESTART's printable report without terminal control characters."
  (sanitize-text (princ-to-string restart) :single-line-p t))

(-> application-lisp--restart-arguments (string) list)
(defun application-lisp--restart-arguments (source)
  "Evaluate one Lisp SOURCE form in AUTOLITH and return all restart arguments."
  (let ((*package* (find-package '#:autolith)))
    (multiple-value-list (eval (self-read-form source)))))

(-> application-lisp-call-with-debugger
    (function &key (:application (option application))
                   (:source (option string))
                   (:operation-kind keyword)
                   (:retry-p boolean)
                   (:return-values-p boolean)
                   (:restart-selector (option function))
                   (:debug-condition-p function))
    (values list (member :ok :aborted) (option string) list (option string)))
(defun application-lisp-call-with-debugger
    (function &key application (source "") (operation-kind ':lisp)
      (retry-p t) (return-values-p nil) restart-selector
      (debug-condition-p (constantly t)))
  "Run FUNCTION with Autolith's selection, evaluation, and control policies.

The library owns live restart extent and recovery execution. The selector sees
CONDITION and live RESTARTS; its portable snapshot is bound for the diagnosis UI.
Return values, status, condition report, restart names, and selected restart name."
  (let ((outcome
          (call-with-debugger
           function
           :selector
           (when restart-selector
             (lambda (session)
               (let ((*application-debugger-snapshot*
                       (debug-session-snapshot session)))
                 (funcall restart-selector
                          (debug-session-condition session)
                          (debug-session-restarts session)))))
           :condition-p debug-condition-p
           :passthrough-p #'application-lisp--control-condition-p
           :restart-p (lambda (restart)
                        (and (restart-name restart)
                             (not (eq (restart-name restart) 'abort))))
           :evaluate-source #'application-lisp--restart-arguments
           :observer
           (when application
             (lambda (event payload)
               (when (eq event ':recovery-failed)
                 (application-present
                  application
                  (list (terminal-span ':failure "Autolith recovery failed: ")
                        (terminal-span
                         ':plain
                         (text-cell-prefix
                          (sanitize-text (getf payload :recovery-condition-report)
                                         :single-line-p t)
                          512)))))))
           :source (or source "")
           :operation-kind operation-kind
           :retry-p retry-p
           :return-values-p return-values-p
           :abort-restart-name 'abort-user-operation
           :abort-report "Return to the Autolith prompt.")))
    (values (outcome-values outcome)
            (outcome-status outcome)
            (outcome-condition-report outcome)
            (outcome-restart-names outcome)
            (outcome-selected-restart-name outcome))))

(-> application-lisp-evaluate
    (string &key (:restart-selector (option function))
                 (:application (option application)))
    application-lisp-evaluation)
(defun application-lisp-evaluate (source &key restart-selector application)
  "Evaluate exactly one SOURCE form in the active AUTOLITH package.

RESTART-SELECTOR receives the signaling condition and selectable restart
objects. It returns a selected restart and, optionally, a Lisp source form whose
multiple values become restart arguments. Returning NIL aborts only this local
evaluation. APPLICATION enables its registered command and tool function
bindings. Output and every returned value are captured without mutation
journaling or provider conversation projection."
  (let ((output-stream (make-string-output-stream))
        (raw-values nil)
        (status ':ok)
        (condition-text nil)
        (restart-names nil)
        (selected-restart-name nil)
        (loop-action nil)
        (*application-lisp-tool-results* (make-hash-table :test #'eq)))
    (handler-case
        (let ((*standard-output* output-stream)
              (*error-output* output-stream)
              (*trace-output* output-stream)
              (*package* (find-package '#:autolith))
              (*application-operation-application* application))
          (multiple-value-bind
                (values debugger-status debugger-condition debugger-restart-names
                        debugger-selected-restart-name)
                (application-lisp-call-with-debugger
                 (lambda ()
                   (when application
                     (application-operation-install-bindings application))
                   (eval (self-read-form source)))
                 :application application
                 :source source
                 :operation-kind ':lisp
                 :retry-p t
                 :return-values-p t
                 :restart-selector restart-selector)
            (setf raw-values values
                  status debugger-status
                  condition-text debugger-condition
                  restart-names debugger-restart-names
                  selected-restart-name debugger-selected-restart-name)))
      (application-operation-loop-action (condition)
        (setf status ':ok
              loop-action (application-operation-loop-action-action condition)
              raw-values nil))
      ((or application-turn-cancelled
           application-input-failed
           agent-loop-error
           conversation-invariant-error
           active-image-corruption)
       (condition)
        (error condition))
      (serious-condition (condition)
        (setf status ':error
              condition-text (princ-to-string condition)
              raw-values nil)))
    (let ((*standard-output* output-stream)
          (*error-output* output-stream)
          (*trace-output* output-stream)
          (*package* (find-package '#:autolith)))
      (multiple-value-bind (rendered-values value-kinds)
          (application-lisp--render-values raw-values application)
        (application-lisp-evaluation-create
         :status status
         :output (get-output-stream-string output-stream)
         :values rendered-values :value-kinds value-kinds
         :condition condition-text
         :restart-names restart-names
         :selected-restart-name selected-restart-name
         :loop-action loop-action)))))


;;;; -- Local Evaluation Presentation --

(-> application-lisp--source-entry (string) list)
(defun application-lisp--source-entry (source)
  "Return exact submitted SOURCE as one syntax-highlighted local transcript entry."
  (append
   (list (terminal-span ':lisp-prompt "* "))
   (or (syntax--highlight-spans source
                               :language *application-common-lisp-language*)
       (list (terminal-span ':code source)))))

(-> application-lisp--output-spans (string) list)
(defun application-lisp--output-spans (output)
  "Return captured OUTPUT as a marked local evaluation section."
  (when (non-empty-string-p output)
    (append
     (list (terminal-span ':notice "output")
           (terminal-span ':plain (string #\Newline))
           (terminal-span ':plain output))
     (unless (char= (char output (1- (length output))) #\Newline)
       (list (terminal-span ':plain (string #\Newline)))))))

(-> application-lisp--result-entry (application-lisp-evaluation) list)
(defun application-lisp--result-entry (evaluation)
  "Return EVALUATION's output, values, or condition as terminal spans."
  (append
   (application-lisp--output-spans
    (application-lisp-evaluation-output evaluation))
   (case (application-lisp-evaluation-status evaluation)
     (:ok
      (let ((values (application-lisp-evaluation-values evaluation)))
        (if values
            (loop for value in values
                  for index from 0
                  append
                  (append
                   (unless (zerop index)
                     (list (terminal-span ':plain (string #\Newline))))
                   (list (terminal-span ':success "⇒ "))
                   (if (eq (nth index (application-lisp-evaluation-value-kinds evaluation)) ':text)
                       (list (terminal-span ':plain value))
                       (application-lisp--value-spans value))))
            (list (terminal-span ':success "⇒ ")
                  (terminal-span ':dim "no values")))))
      (:aborted
       (append
        (list (terminal-span ':failure "aborted"))
        (when (application-lisp-evaluation-condition evaluation)
          (list
           (terminal-span
            ':plain
            (format nil ": ~A"
                    (application-lisp-evaluation-condition evaluation)))))))
     (:error
      (list
       (terminal-span ':failure "error: ")
       (terminal-span
        ':plain
        (or (application-lisp-evaluation-condition evaluation)
            "unknown local evaluation failure")))))))

(-> application-lisp--restart-items (list) list)
(defun application-lisp--restart-items (restarts)
  "Return styled modal selector items for ordered RESTARTS."
  (loop for restart in restarts
        for index from 0
        for name = (restart-name restart)
        for name-text = (format nil "~(~A~)" name)
        for report = (application-lisp--restart-report restart)
        for description = (format nil "~A  ~A" name-text report)
        collect
        (list :name (format nil "~D" index)
              :argument nil
              :group "live restarts"
              :description description
              :description-spans
              (list (terminal-span
                     (if (eq name 'abort-user-operation) ':failure ':code)
                     name-text)
                    (terminal-span ':dim (format nil "  ~A" report))))))

(-> application-lisp--read-restart-value (application) (option string))
(defun application-lisp--read-restart-value (application)
  "Read one restart argument form while preserving the user's current draft."
  (let* ((ui (application-ui application))
         (editor (terminal-ui-editor ui))
         (saved-input
           (terminal-ui--submission-input ui (line-editor-text editor))))
    (let ((saved-lisp-input-p (terminal-ui-lisp-input-p ui)))
      (unwind-protect
           (progn
             (terminal-ui-set-lisp-input ui t)
             (terminal-ui-set-input ui "")
             (application-present
              application
              (list
               (terminal-span
                ':hint
                "Enter one Lisp form. Its multiple values become restart arguments.")))
             (terminal-ui-call-with-exclusive-input
              ui
              (lambda ()
                (loop
                  (multiple-value-bind (action payload)
                      (terminal-ui-process-event ui (terminal-ui-read-event ui))
                    (case action
                      (:submit
                       (if (user-message-input-image-pathnames payload)
                           (progn
                             (application-present
                              application
                              (list
                               (terminal-span
                                ':failure
                                "Restart argument input cannot include image attachments.")))
                             (terminal-ui-set-input ui payload))
                           (return (user-message-input-text payload))))
                      ((:interrupt :end-of-input :escape)
                       (return nil))))))))
        (terminal-ui-set-input ui saved-input)
        (terminal-ui-set-lisp-input ui saved-lisp-input-p)))))

(-> application-lisp--debugger-condition-entry (condition) list)
(defun application-lisp--debugger-condition-entry (condition)
  "Render CONDITION's report, typed details, and available source location."
  (let* ((snapshot *application-debugger-snapshot*)
         (metadata (or (getf snapshot :condition-metadata)
                       (condition-metadata condition)))
         (location (getf metadata :location)))
    (flet ((display-text (text)
             "Bound and sanitize one debugger field for terminal presentation."
             (text-cell-prefix (sanitize-text text :single-line-p t) 512)))
      (append
       (list (terminal-span ':failure "restart debugger")
             (terminal-span ':dim "  ")
             (terminal-span ':code
                            (display-text
                             (or (getf snapshot :condition-type)
                                 (string-downcase (string (type-of condition))))))
             (terminal-span ':plain (string #\Newline))
             (terminal-span ':plain
                            (display-text
                             (or (getf snapshot :condition-report)
                                 (princ-to-string condition)))))
       (loop for field in (getf metadata :fields)
             append
             (list (terminal-span ':plain (string #\Newline))
                   (terminal-span ':notice
                                  (format nil "~A: "
                                          (display-text (getf field :label))))
                   (terminal-span ':code (display-text (getf field :value)))))
       (when location
         (list
          (terminal-span ':plain (string #\Newline))
          (terminal-span ':notice "location: ")
          (terminal-span
           ':code
           (display-text
            (format nil "~A~A"
                    (or (getf location :pathname) "input")
                    (cond
                      ((getf location :line)
                       (format nil ":~D~@[:~D~]"
                               (getf location :line) (getf location :column)))
                      ((getf location :position)
                       (format nil " at stream position ~D" (getf location :position)))
                      (t
                       "")))))))))))

(-> application-lisp--preferred-restart-index (list) (integer 0))
(defun application-lisp--preferred-restart-index (restarts)
  "Return the most useful initial selector index for live RESTARTS."
  (or (position-if
       (lambda (restart)
         (member (restart-name restart)
                 *application-lisp-value-restart-names*
                 :test #'eq))
       restarts)
      (position 'abort-user-operation restarts :key #'restart-name :test #'eq)
      0))

(-> application-lisp--select-restart
    (application condition list)
    (values (option (or restart recovery)) (option string)))
(defun application-lisp--select-restart (application condition restarts)
  "Present CONDITION in a stack-preserving live debugger modal loop.

The owner thread remains inside its dynamic restart binding while an independent
Autolith diagnosis may propose validated executable recoveries."
  (let* ((ui (application-ui application))
         (terminal (and ui (terminal-ui-terminal ui))))
    (when ui
      (application-present
       application
       (application-lisp--debugger-condition-entry condition)))
    (unless (and ui terminal (terminal-interactive-p terminal))
      (return-from application-lisp--select-restart (values nil nil)))
    (let* ((live-items (application-lisp--restart-items restarts))
           (preferred-index (application-lisp--preferred-restart-index restarts))
           (session
             (make-instance 'application-debugger-session
                            :snapshot *application-debugger-snapshot*
                            :application application)))
      (labels ((live-choice (choice)
                 "Resolve CHOICE to a live restart and optional argument source."
                 (let* ((index (and (stringp choice)
                                    (parse-integer choice :junk-allowed t)))
                        (restart (and index (nth index restarts))))
                   (if (and restart
                            (member (restart-name restart)
                                    *application-lisp-value-restart-names*
                                    :test #'eq))
                       (let ((source (application-lisp--read-restart-value application)))
                         (if (non-empty-string-p source)
                             (values restart source)
                             (values nil nil)))
                       (values restart nil))))

               (diagnosis-info-item (snapshot)
                 "Build the bounded explanation or failure entry for SNAPSHOT."
                 (let* ((text
                          (or (getf snapshot :explanation)
                              (getf snapshot :failure)
                              "Diagnosis completed without an explanation."))
                        (bounded-text (bounded-string text :limit 512))
                        (description (format nil "diagnosis  ~A" bounded-text)))
                   (list :name "diagnosis"
                         :argument nil
                         :value "diagnosis-info"
                         :group "Autolith diagnosis"
                         :description description
                         :description-spans
                         (list (terminal-span ':notice "diagnosis")
                               (terminal-span ':dim
                                              (format nil "  ~A" bounded-text))))))

               (diagnosis-items (snapshot running-p)
                 "Build the diagnosis modal items while preserving live RESTARTS."
                 (let ((proposals (getf snapshot :proposals)))
                   (append
                    live-items
                    (if running-p
                        (list (list :name "cancel-diagnosis"
                                    :argument nil
                                    :value "cancel-diagnosis"
                                    :group "Autolith diagnosis"
                                    :description "cancel diagnosis"
                                    :description-spans
                                    (list (terminal-span ':failure
                                                         "cancel diagnosis"))))
                        (list (diagnosis-info-item snapshot)))
                    (loop for proposal in (subseq (reverse proposals) 0
                                                   (min 3 (length proposals)))
                          for name in *application-debugger-recovery-names*
                          for report = (bounded-string
                                        (recovery-report proposal)
                                        :limit 512)
                          for description = (format nil "~A  ~A" name report)
                          collect
                          (list :name name
                                :argument nil
                                :value name
                                :group "Autolith recoveries"
                                :description description
                                :description-spans
                                (list (terminal-span ':success name)
                                      (terminal-span ':dim
                                                     (format nil "  ~A" report))))))))

               (normal-items ()
                 "Build the ordinary live-restart selector items."
                 (append live-items
                         (list (list :name "ask-autolith"
                                     :argument nil
                                     :value "ask-autolith"
                                     :group "Autolith diagnosis"
                                     :description "Ask Autolith why this failed"
                                     :description-spans
                                     (list (terminal-span
                                            ':brand
                                            "Ask Autolith why this failed"))))))

               (recovery-choice (choice)
                 "Resolve a fixed recovery CHOICE to the current proposal object."
                 (let ((index
                         (and (stringp choice)
                              (position choice
                                        *application-debugger-recovery-names*
                                        :test #'string=))))
                   (and index
                        (nth index
                             (reverse
                              (getf (application-debugger-poll session)
                                    :proposals))))))

               (interrupt-event ()
                 "Handle an interrupt without losing a live debugger modal."
                 (let* ((controller (application-input-controller application))
                        (active-p (and controller
                                       (application-input-controller-turn-active-p
                                        controller))))
                   (if (not active-p)
                       '(:cancel)
                       (case (application-input-controller--active-turn-interrupt-action
                              controller)
                         (:force
                          (application-input-controller--force-interrupt-exit controller)
                          '(:cancel))
                         (:hint
                          nil)
                         (otherwise
                          (application-input-controller--request-active-turn-cancellation
                           controller
                           :force-exit-window-p t
                           :pause-queued-work-p t)
                          nil)))))

               (normal-event (event selector)
                 "Handle ordinary debugger picker events."
                 (declare (ignore selector))
                 (case event
                   (:interrupt (interrupt-event))
                   ((:escape :stream-end) '(:cancel))
                   (otherwise nil)))

               (diagnose ()
                 "Run the independent diagnosis modal and return its selection."
                 (application-debugger-start-diagnosis
                  session (application-configuration application))
                 (unwind-protect
                      (let* ((running-items
                               (diagnosis-items
                                (application-debugger-poll session) t))
                             (choice
                               (terminal-ui-select
                                ui
                                :title "restart debugger"
                                :items running-items
                                :initial-name (format nil "~D" preferred-index)
                                :resize-callback #'application-pending-terminal-size
                                :poll-interval 0.1
                                :on-event
                                (lambda (event selector)
                                  (declare (ignore selector))
                                  (cond
                                    ((eq event ':escape)
                                     '(:cancel))
                                    ((eq event ':interrupt)
                                     (interrupt-event))
                                    ((eq event ':stream-end)
                                     '(:cancel))
                                    ((eq event ':poll)
                                     (let* ((snapshot
                                              (application-debugger-poll session))
                                            (state (getf snapshot :state)))
                                       (list ':replace "restart debugger"
                                             (diagnosis-items
                                              snapshot
                                              (eq state ':running))))))))))
                        (or (recovery-choice choice) choice))
                   (when (eq (getf (application-debugger-poll session) :state)
                             ':running)
                     (application-debugger-cancel-diagnosis session)))))
      (loop
        (let ((choice
                (terminal-ui-select
                 ui
                 :title "restart debugger"
                 :items (normal-items)
                 :initial-name (format nil "~D" preferred-index)
                 :resize-callback #'application-pending-terminal-size
                 :on-event #'normal-event)))
          (cond
            ((null choice)
             (return (values nil nil)))
            ((string= choice "ask-autolith")
               (let ((diagnosis-choice (diagnose)))
                 (cond
                   ((or (null diagnosis-choice)
                        (and (stringp diagnosis-choice)
                             (member diagnosis-choice
                                     '("cancel-diagnosis" "diagnosis-info")
                                     :test #'string=)))
                    nil)
                   ((typep diagnosis-choice 'recovery)
                    (return (values diagnosis-choice nil)))
                   ((member diagnosis-choice
                            (mapcar (lambda (item) (getf item :name)) live-items)
                            :test #'string=)
                    (multiple-value-bind (restart source)
                        (live-choice diagnosis-choice)
                      (return (values restart source)))))))
            (t
             (multiple-value-bind (restart source) (live-choice choice)
               (return (values restart source)))))))))))

(-> application-lisp-call-with-ui-debugger
    (application function
     &key (:debug-condition-p function)
          (:source (option string))
          (:operation-kind keyword)
          (:retry-p boolean)
          (:return-values-p boolean))
    (values list (member :ok :aborted) (option string) list (option string)))
(defun application-lisp-call-with-ui-debugger
    (application function
     &key (debug-condition-p (constantly t)) source
       (operation-kind ':command) (retry-p t) (return-values-p nil))
  "Call FUNCTION under APPLICATION's styled local restart debugger."
  (let ((effective-source (or source "")))
    (application-lisp-call-with-debugger
     function
     :application application
     :source effective-source
     :operation-kind operation-kind
     :retry-p retry-p
     :return-values-p return-values-p
     :restart-selector
     (lambda (condition restarts)
       (application-lisp--select-restart application condition restarts))
     :debug-condition-p debug-condition-p)))

(-> application-lisp--user-operation-result
    (application-lisp-evaluation list)
    string)
(defun application-lisp--user-operation-result (evaluation result-entry)
  "Return RESULT-ENTRY as durable text with EVALUATION's loop action."
  (let ((rendered (terminal--spans-text result-entry))
        (action (application-lisp-evaluation-loop-action evaluation)))
    (if action
        (format nil "~A~%loop action: ~(~A~)" rendered action)
        rendered)))

(-> application-run-lisp-input
    (application string &key (:interactive-p boolean))
    keyword)
(defun application-run-lisp-input (application source &key (interactive-p t))
  "Evaluate local Lisp SOURCE and retain bounded context for later requests.

INTERACTIVE-P permits command pickers and restart selection. Present completed
evaluation results before durable retention so a persistence failure cannot
conceal local side effects. The persistence failure still propagates to the
caller."
  (application-present application (application-lisp--source-entry source))
  (application-set-local-activity application "evaluating local Lisp")
  (let* ((evaluation
           (unwind-protect
                (let ((*application-local-user-evaluation-p* t)
                      (*application-command-interactive-p* interactive-p)
                      (*application-user-operation-recording-suppressed-p* t))
                  (application-lisp-evaluate
                   source
                   :application application
                   :restart-selector
                   (and interactive-p
                        (lambda (condition restarts)
                          (application-lisp--select-restart
                           application condition restarts)))))
             (application-set-local-activity application nil)))
         (result-entry (application-lisp--result-entry evaluation))
         (status       (application-lisp-evaluation-status evaluation))
         (result
           (application-lisp--user-operation-result evaluation result-entry)))
    (application-present application result-entry)
    (conversation-append-user-operation
     (application-conversation application)
     :kind ':lisp
     :source source
     :status status
     :result result)
    (case status
      (:ok
       (or (application-lisp-evaluation-loop-action evaluation) ':continue))
      (:aborted ':aborted)
      (:error ':failed))))

(-> application-lisp--value-columns ((option application)) (integer 1))
(defun application-lisp--value-columns (application)
  "Return the local result width after reserving its prefix and edge column."
  (if (and application (slot-boundp application 'ui) (application-ui application))
      (max 1 (- (terminal-columns
                 (terminal-ui-terminal (application-ui application))) 3))
      78))

(-> application-lisp--value-spans (string) list)
(defun application-lisp--value-spans (value)
  "Highlight a rendered VALUE, including canonical Common Lisp booleans."
  (if (member value '("NIL" "T") :test #'string-equal)
      (list (terminal-span ':syntax-number value))
      (or (handler-case
              (syntax--highlight-spans value
                                       :language *application-common-lisp-language*)
            (error () nil))
          (list (terminal-span ':code value)))))

(-> application-lisp--render-values
    (list (option application)) (values list list))
(defun application-lisp--render-values (values application)
  "Snapshot VALUES as display text and corresponding Lisp or report kinds.
Use tool presentation only for unambiguous, unchanged content identities.
No raw values or tool objects escape in the returned lists."
  (let ((texts nil)
        (kinds nil))
    (dolist (value values)
      (let ((snapshot (gethash value *application-lisp-tool-results*)))
        (if (and snapshot (string= value (second snapshot)))
            (progn
              (push (first snapshot) texts)
              (push (third snapshot) kinds))
            (progn
              (push (application-lisp--render-value
                     value :columns (application-lisp--value-columns application)
                           :initial-column 2)
                    texts)
              (push ':lisp kinds)))))
    (values (nreverse texts) (nreverse kinds))))
