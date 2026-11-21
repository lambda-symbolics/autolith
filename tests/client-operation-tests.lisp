(in-package #:autolith)

;;;; -- Headless Operation Admission Tests --

(-> client-operation-tests--steering-cwd (application world-terminal-client-connection) null)
(defun client-operation-tests--steering-cwd (application connection)
  "Exercise decoded cwd admission, durable receipt failure and client completion."
  (let* ((configuration     (application-configuration application))
         (conversation      (application-conversation application))
         (controller        (application-input-controller application))
         (base              (config :working-directory configuration))
         (text-target       (merge-pathnames "client-text-cwd/" base))
         (native-target     (merge-pathnames "client native cwd/" base))
         (operation         (application-client-operation-find application "cwd"))
         (command           (application-operation-backend operation))
         (setter            (symbol-function 'application-set-working-directory))
         (project           (symbol-function 'conversation--project-automatic-message))
         (publish           (symbol-function 'application-clients-publish))
         (entered           (sb-thread:make-semaphore))
         (release           (sb-thread:make-semaphore))
         (take              nil)
         (acknowledge       nil)
         (waiter            nil)
         (current-id        nil)
         (setter-calls      0)
         (completions       nil)
         (process-directory (uiop:getcwd))
         (pathname-defaults *default-pathname-defaults*)
         (fail-project-p    nil))
    (labels ((receipts ()
               (remove-if-not
                (lambda (record)
                  (and (eq (first record) ':message)
                       (equal (getf (rest record) :pending-input-identifier) current-id)))
                (application-tests--conversation-records conversation)))

             (projected-p ()
               (let ((record (first (receipts))))
                 (and record
                      (not (null (find (getf (rest record) :wire-json)
                                       (clinker-transcript:projection-items
                                        (conversation-projection conversation))
                                       :test #'equal :key #'json-encode)))))))
      (ensure-directories-exist text-target)
      (ensure-directories-exist native-target)
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'application-set-working-directory
                   (lambda (&rest arguments)
                     (when (eq (first arguments) application) (incf setter-calls))
                     (apply setter arguments)))
             (list 'conversation--project-automatic-message
                   (lambda (&rest arguments)
                     (when (and fail-project-p (eq (first arguments) conversation))
                       (error 'configuration-error :message "Injected client receipt projection failure."))
                     (apply project arguments)))
             (list 'application-clients-publish
                   (lambda (owner event)
                     (when (and (eq owner application) (eq (first event) ':work-finished)
                                (member (getf (rest event) :kind) '(:command :steering)))
                       (push (list :event event :durable-p (= 1 (length (receipts)))
                                   :projected-p (projected-p)) completions))
                     (funcall publish owner event))))
            (lambda ()
              (setf waiter
                    (make-thread
                     (lambda ()
                       (application-clients-call-as-work
                        application
                        (lambda ()
                          (application-input-controller-call-with-primary-steering
                           controller
                           (lambda (take-function acknowledge-function)
                             (setf take take-function acknowledge acknowledge-function)
                             (sb-thread:signal-semaphore entered)
                             (sb-thread:wait-on-semaphore release))))))
                     :name "Client cwd active primary work"))
              (test-assert (sb-thread:wait-on-semaphore entered :timeout 10)
                           "the primary channel work reaches its steering boundary")
              (let* ((result
                         (application-client-invoke application :cwd
                                                     (list (namestring text-target))))
                     (entry (first (funcall take)))
                     (input (agent-steering-input-content entry)))
                (setf current-id (agent-steering-input-identifier entry))
                (test-assert
                   (and (equal "scheduled" (getf result :delivery)) (= setter-calls 0)
                      (eq (command-message-input-phase input) ':pending)
                      (equal base (config :working-directory configuration))
                      (equal (application-command-invocation-arguments
                              (command-message-input-invocation input)) (list (namestring text-target)))
                      (null completions))
                 "client decoding preserves arguments and scheduled admission precedes commit")
                (application-input-controller--apply-command-steering controller nil entry)
                (funcall acknowledge current-id)
                (test-assert (and (= setter-calls 1) (= 1 (length completions)))
                             "one admitted text command commits and completes once"))
              (let* ((invocation
                       (make-instance 'application-command-invocation
                                      :input "(cwd)" :syntax ':lisp :name (application-command-name command)
                                      :remainder "" :argument nil :arguments (list native-target)
                                      :supplied-argument-count 1 :command command))
                     (delivery (nth-value 1 (application-client-operation-invoke application operation invocation)))
                     (entry    (first (funcall take)))
                     (input    (agent-steering-input-content entry))
                     (before   (length (clinker-transcript:projection-items (conversation-projection conversation)))))
                (setf current-id (agent-steering-input-identifier entry) fail-project-p t)
                (test-assert
                 (and (equal "scheduled" delivery) (= setter-calls 1)
                      (eq (command-message-input-phase input) ':pending)
                      (equal (application-command-invocation-arguments
                              (command-message-input-invocation input)) (list native-target)))
                 "native pathname arguments survive an argument-free invocation display label")
                (test-assert
                 (handler-case
                     (progn (application-input-controller--apply-command-steering controller nil entry) nil)
                   (configuration-error () t))
                 "receipt projection failure escapes command application")
                (test-assert
                 (and (= setter-calls 2) (eq (command-message-input-phase input) ':committed)
                      (= 1 (length (receipts))) (not (projected-p))
                      (= before (length (clinker-transcript:projection-items (conversation-projection conversation))))
                      (= 1 (length completions)))
                 "a durable unprojected receipt cannot announce final client completion")
                (setf fail-project-p nil)
                (application-input-controller--apply-command-steering controller nil entry)
                (funcall acknowledge current-id)
                (test-assert
                 (and (= setter-calls 2) (= 1 (length (receipts))) (projected-p)
                      (= (1+ before) (length (clinker-transcript:projection-items
                                              (conversation-projection conversation))))
                      (= 2 (length completions))
                      (every (lambda (completion)
                               (and (getf completion :durable-p) (getf completion :projected-p)
                                    (eq (getf (rest (getf completion :event)) :kind) ':steering)))
                             completions))
                 "retry projects one receipt and completes without repeating the runtime setter")
                (let* ((reply (world-terminal-client-call connection ':client-state))
                       (state (getf reply :result)))
                  (test-assert
                   (and (eq (getf state :outcome) ':ok)
                        (equal (getf (getf state :value) :workspace)
                               (namestring native-target)))
                   "the shared client state reports the postcommit working directory")))))
        (unwind-protect
             (progn
               (sb-thread:signal-semaphore release)
               (when waiter (join-thread waiter))
               (funcall setter application base))
          (uiop:chdir process-directory)
          (setf *default-pathname-defaults* pathname-defaults)))))
  nil)

(-> test-client-operation-admission () null)
(defun test-client-operation-admission ()
  "Exercise shared permission admission, scheduling, publication and ACP parity."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (channel     (getf (rest (world-server-tests--open entry (world-tests--workspace root "admission")))
                               :session-id))
            (application (world-host-application (world-server--find-host server channel)))
            (connection  (world-terminal-client-connect (world-terminal--find-record configuration channel)
                                                    :name "admission test"))
            (entered     (sb-thread:make-semaphore))
            (release     (sb-thread:make-semaphore))
            (publication (symbol-function 'application-clients-publish-state))
            (published   nil)
            (waiter      nil))
        (labels ((invoke (name &optional input)
                   "Invoke NAME through the shared client parameter decoder and operation admission."
                   (application-client-invoke application
                                               (intern (string-upcase name) :keyword)
                                               (if input (list input) nil)))

                 (saved-mode ()
                   "Read the mode from private state, independently of live publication."
                   (application--saved-permission-mode configuration (application-conversation application)))

                 (idle ()
                   "Wait until the channel has finished its preceding command."
                   (test-assert
                    (task-tests--wait-until
                     (lambda ()
                       (not (application-input-controller-busy-p
                             (application-input-controller application))))
                     10)
                    "preceding channel work reaches idle")))
         (unwind-protect
              (test-call-with-function-replacements
               (list (list 'application-clients-publish-state
                           (lambda (owner)
                             (push (application-permission-mode owner) published)
                             (funcall publication owner))))
               (lambda ()
                 (dolist (mode '(:auto :ask :full-access))
                   (world-terminal-client-call connection ':mode :value mode)
                   (idle)
                   (test-assert (eq mode (application-permission-mode application))
                                "structured modes use the registered permissions command")
                   (test-assert (eq mode (saved-mode))
                                "structured mode changes persist for this channel"))
                 (test-assert (equal '(:full-access :ask :auto) published)
                              "executed mode commands publish their resulting state")
                 (setf published nil)
                 (test-call-with-function-replacements
                  (list (list 'application--command-sandbox-available-p (constantly nil)))
                  (lambda ()
                    (test-assert
                     (handler-case (progn (world-terminal-client-call connection ':mode :value ':sandboxed) nil)
                       (world-terminal-error () t))
                     "structured sandbox mode retains the command sandbox guard")
                    (idle)
                    (test-assert
                      (handler-case (progn (invoke "permissions" :sandbox) nil)
                        (configuration-error () t))
                     "ACP invocation preserves the sandbox configuration condition")))
                 (test-assert (and (eq ':full-access (application-permission-mode application))
                                   (null published))
                              "failed admission does not mutate or publish mode state")
                 (dolist (name '("permissions" "no-such-client-operation"))
                   (test-assert
                    (handler-case (progn (invoke name) nil)
                      (application-client-operation-unavailable () t))
                    "missing operations and argument-dependent terminal ownership keep shared admission errors"))
                 (test-assert
                  (handler-case
                      (progn
                        (application-client-operation-find application "no-such-client-operation")
                        nil)
                    (application-client-operation-unavailable () t))
                  "application admission has its own transport-independent condition")
                 (setf waiter
                       (make-thread
                        (lambda ()
                          (application-clients-call-as-work
                           application
                           (lambda ()
                             (sb-thread:signal-semaphore entered)
                             (sb-thread:wait-on-semaphore release))))
                        :name "client admission active work"))
                 (test-assert (sb-thread:wait-on-semaphore entered :timeout 10)
                              "a real channel work item owns execution")
                 (world-terminal-client-call connection ':mode :value ':auto)
                 (let ((result (invoke "permissions" :ask)))
                   (test-assert (and (stringp (getf result :result))
                                     (equal "scheduled" (getf result :delivery)))
                                "native invocation returns safe-boundary acceptance"))
                 (test-assert (and (eq ':full-access (application-permission-mode application))
                                   (null published)
                                   (eq ':full-access (saved-mode)))
                              "busy mode acceptance neither applies nor publishes before the safe boundary")
                 (test-assert
                    (handler-case (progn (invoke "goal" :pause) nil)
                     (application-client-operation-busy () t))
                  "a held busy command retains ACP busy rejection")
                 (sb-thread:signal-semaphore release)
                 (join-thread waiter)
                 (setf waiter nil)
                 (test-assert
                  (task-tests--wait-until (lambda () (= 2 (length published))) 10)
                  "both scheduled commands execute and publish at the channel boundary")
                 (test-assert (and (equal '(:ask :auto) published)
                                   (eq ':ask (application-permission-mode application))
                                   (eq ':ask (saved-mode)))
                              "safe-boundary execution preserves command ordering and persistence")
                 (idle)
                 (let ((result (invoke "permissions" :auto)))
                    (test-assert (and (null (getf result :result))
                                      (null (getf result :delivery))
                                      (eq ':auto (application-permission-mode application)))
                                 "idle native invocation returns the command's NIL result"))
                 (client-operation-tests--steering-cwd application connection)))
           (sb-thread:signal-semaphore release)
           (when waiter (join-thread waiter))
           (world-terminal-client-close connection))))))
  nil)

(-> test-client-operation-catalog-authority () null)
(defun test-client-operation-catalog-authority ()
  "Read semantic methods over the shared transport and derive choices from registration."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore server))
     (let* ((entry      (world-client-entry configuration))
            (channel    (getf (rest (world-server-tests--open
                                     entry (world-tests--workspace root "catalog")))
                             :session-id))
            (connection (world-terminal-client-connect
                         (world-terminal--find-record configuration channel) :name "catalog test"))
            (command    (application-command-find "permissions")))
       (labels ((request (command)
                  "Call one registered native method without an ACP session."
                  (getf (world-terminal-client-call connection command) :result)))
         (unwind-protect
              (progn
                 (let ((state (request ':client-state)))
                  (test-assert (eq ':ok (getf state :outcome))
                               "state is a server-owned semantic endpoint")
                  (test-assert (stringp (getf (getf state :value) :workspace))
                             "state returns structured application data"))
                 (let* ((catalog (request ':operations))
                        (items   (getf (getf catalog :value) :items))
                        (row     (find "permissions" items :test #'equal
                                       :key                      (lambda (item) (getf item :name)))))
                  (test-assert row "the registered command appears in the shared catalogue")
                  (test-assert (equal (getf row :help) (application-command-tip command))
                             "command help comes from registered metadata")
                  (test-assert
                     (equalp (getf row :choices)
                             (application-command-static-options command))
                   "supported textual choices use the structured client's argument encoding"))
                (let ((find-command (symbol-function 'application-command-find))
                      (restricted   (make-instance 'application-command
                                                   :static-options '(:full :list :ask))))
                  (test-call-with-function-replacements
                   (list (list 'application-command-find
                               (lambda (name)
                                 (if (equal name "permissions")
                                     restricted
                                     (funcall find-command name)))))
                   (lambda ()
                     (test-assert (equal '(:full-access :ask)
                                         (application-client-permission-modes))
                                  "mode support and ordering follow current command metadata")
                     (test-assert
                      (handler-case
                          (progn (application-client-permission-input ':auto) nil)
                        (application-client-operation-unavailable () t))
                      "removed metadata choices cannot be encoded as permission inputs")
                     (test-assert
                      (handler-case
                          (progn (world-terminal-client-call connection ':mode :value ':auto) nil)
                        (world-terminal-error () t))
                      "structured mode admission honors the current supported choices")))))
           (world-terminal-client-close connection))))))
  nil)

(-> test-client-state-goal () null)
(defun test-client-state-goal ()
  "Read goal transitions through the native state boundary."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (labels ((reported ()
                 (let ((reply (application-client-run application :client-state)))
                   (test-assert (eq (getf reply :outcome) :ok)
                                "goal state is a successful native request")
                   (getf (getf reply :value) :goal))))
        (test-assert (null (reported)) "a channel without a goal reports nil")
        (dolist (kind '(:goal :mission))
          (dolist (status '(:active :paused :complete :verified :blocked :exhausted :cancelled :failed))
            (setf (application-goal application)
                  (list :objective "Ship the requested change" :status status :kind kind
                        :created-at 3972384000 :continuations 2))
            (let ((goal (reported)))
              (test-assert
               (and (equal (getf goal :objective) "Ship the requested change")
                    (eq (getf goal :status) status)
                    (eq (getf goal :kind) kind)
                    (= (getf goal :created-at) 3972384000)
                    (= (getf goal :continuations) 2))
               "native state reports goal metadata"))))
        (setf (application-goal application) nil)
        (test-assert (null (reported)) "clearing the goal reports nil")
        (terminal-ui-stop (application-ui application))))
   nil))
