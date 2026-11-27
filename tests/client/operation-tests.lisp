(in-package #:autolith)

;;;; -- Headless Operation Admission Tests --

(-> client-operation-tests--apply-cwd (application) null)
(defun client-operation-tests--apply-cwd (application)
  "Run a decoded cwd invocation at the boundary, without evaluating its display label."
  (let* ((operation  (application-client-operation-find application "cwd"))
         (command    (application-operation-backend operation))
         (target     (merge-pathnames "client native cwd/" (uiop:temporary-directory)))
         (invocation (make-instance 'application-command-invocation
                                    :input "(cwd)" :syntax ':lisp :name "cwd" :remainder ""
                                    :argument nil :arguments (list target)
                                    :supplied-argument-count 1 :command command))
         (received   nil))
    (test-call-with-function-replacements
     (list (list 'application-set-working-directory
                 (lambda (owner location)
                   (setf received (list owner location))
                   location)))
     (lambda ()
       (test-assert
        (application-clients-call-as-work
         application
         (lambda ()
           (and (equal "scheduled" (nth-value 1 (application-client-operation-invoke application operation invocation)))
                (null received))))
        "a busy decoded cwd is accepted before it executes")
       (test-assert (task-tests--wait-until (lambda () received) 10)
                    "the channel executes the scheduled command at its next boundary")
       (test-assert (equal (list application target) received)
                    "the boundary passes the native pathname, not the display label"))))
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
                 (dolist (available-p '(nil t))
                   (test-call-with-function-replacements
                    (list (list 'application--command-sandbox-available-p (constantly available-p)))
                    (lambda ()
                      (let* ((picker (application--permission-mode-items application))
                             (modes  (gethash "modes" (acp-service--modes (make-instance 'acp-session))))
                             (rows   (gethash "availableModes" modes))
                             (item   (find "sandbox" picker
                                           :test #'equal
                                           :key  (lambda (row) (getf row :name))))
                             (row    (find "sandboxed" rows
                                           :test #'equal
                                           :key  (lambda (row) (gethash "id" row)))))
                        (test-assert (and (eq available-p (not (null item)))
                                          (eq available-p (not (null row))))
                                     "terminal choices and ACP advertised modes agree with host capability.")))))
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
                 (client-operation-tests--apply-cwd application)))
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
                 (test-call-with-function-replacements
                  (list (list 'application--permission-mode-catalogue
                              (lambda ()
                                '((:full-access :full "Full access")
                                  (:ask :ask "Ask")))))
                  (lambda ()
                    (test-assert (equal '(:full-access :ask)
                                        (application-client-permission-modes))
                                 "mode support and ordering follow the canonical permission catalogue")
                    (test-assert
                     (handler-case
                         (progn (application-client-permission-input ':auto) nil)
                       (application-client-operation-unavailable () t))
                     "choices absent from the catalogue cannot be encoded as permission inputs")
                    (test-assert
                     (handler-case
                         (progn (world-terminal-client-call connection ':mode :value ':auto) nil)
                       (world-terminal-error () t))
                     "structured mode admission honors the current supported choices"))))
           (world-terminal-client-close connection))))))
  nil)
