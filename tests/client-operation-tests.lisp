(in-package #:autolith)

;;;; -- Headless Operation Admission Tests --

(-> test-client-operation-admission () null)
(defun test-client-operation-admission ()
  "Exercise shared permission admission, scheduling, publication and ACP parity."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (channel     (getf (rest (world-server-tests--open entry (world-tests--workspace root "admission")))
                               :session-id))
            (application (world-host-application (world-server--find-host server channel)))
            (connection  (localgroup-client-connect (localgroup--find-record configuration channel)
                                                    :name "admission test"))
            (entered     (sb-thread:make-semaphore))
            (release     (sb-thread:make-semaphore))
            (publication (symbol-function 'application-clients-publish-state))
            (published   nil)
            (waiter      nil))
       (labels ((invoke (name &optional (input ""))
                  "Invoke NAME through the shared client parameter decoder and operation admission."
                  (application-client-invoke
                   application
                   (agentcomms:json-object
                    "operation" name
                    "arguments" (agentcomms:json-object "input" input))))

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
                   (localgroup-client-call connection ':mode :value mode)
                   (idle)
                   (test-assert (eq mode (application-permission-mode application))
                                "structured modes use the registered permissions command")
                   (when (member mode '(:ask :auto))
                     (test-assert (eq mode (config :permission-mode (application-configuration application)))
                                  "ask and auto persist through the command's configuration hook")))
                 (test-assert (equal '(:full-access :ask :auto) published)
                              "executed mode commands publish their resulting state")
                 (setf published nil)
                 (test-call-with-function-replacements
                  (list (list 'application--command-sandbox-available-p (constantly nil)))
                  (lambda ()
                    (test-assert
                     (handler-case (progn (localgroup-client-call connection ':mode :value ':sandboxed) nil)
                       (localgroup-error () t))
                     "structured sandbox mode retains the command sandbox guard")
                    (idle)
                    (test-assert
                     (handler-case (progn (invoke "permissions" "sandbox") nil)
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
                 (localgroup-client-call connection ':mode :value ':auto)
                 (let ((result (invoke "permissions" "ask")))
                   (test-assert (and (stringp (gethash "text" result))
                                     (equal "scheduled" (gethash "delivery" result)))
                                "ACP preserves its scheduled delivery and presentation envelope"))
                 (test-assert (and (eq ':full-access (application-permission-mode application))
                                   (null published)
                                   (eq ':ask (config :permission-mode (application-configuration application))))
                              "busy mode acceptance neither applies nor publishes before the safe boundary")
                 (test-assert
                  (handler-case (progn (invoke "goal" "pause") nil)
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
                                   (eq ':ask (config :permission-mode (application-configuration application))))
                              "safe-boundary execution preserves command ordering and persistence")
                 (idle)
                 (let ((result (invoke "permissions" "auto")))
                   (test-assert (and (stringp (gethash "text" result))
                                     (not (nth-value 1 (gethash "delivery" result)))
                                     (eq ':auto (application-permission-mode application)))
                                "idle ACP invocation returns text without a scheduled delivery"))))
           (sb-thread:signal-semaphore release)
           (when waiter (join-thread waiter))
           (localgroup-client-close connection))))))
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
            (connection (localgroup-client-connect
                         (localgroup--find-record configuration channel) :name "catalog test"))
            (command    (application-command-find "/permissions")))
       (labels ((request (method)
                  "Call one shared semantic method without an ACP session."
                  (getf (localgroup-client-call connection ':operation
                                               :method method
                                               :params (client-portable (json-object)))
                        :result)))
         (unwind-protect
              (progn
                (let ((state (request "_autolith/state")))
                  (test-assert (eq ':ok (getf state :outcome))
                               "state is a server-owned semantic endpoint")
                  (test-assert (stringp (gethash "workspace" (client-json (getf state :value))))
                               "state returns structured application data"))
                (test-assert (eq ':unsupported (getf (request "_autolith/steer") :outcome))
                             "frontend session handlers are not server semantic methods")
                (let* ((catalog (request "_autolith/operations"))
                       (items   (gethash "items" (client-json (getf catalog :value))))
                       (row     (find "permissions" items :test #'equal
                                      :key                      (lambda (item) (gethash "name" item)))))
                  (test-assert row "the registered command appears in the shared catalogue")
                  (test-assert (equal (gethash "help" row) (application-command-tip command))
                               "command help comes from registered metadata")
                  (test-assert
                   (equalp (gethash "choices" row)
                           (map 'vector #'application-command--slash-option-token
                                (application-command-static-options command)))
                   "supported textual choices use the registered slash spelling"))
                (let ((find-command (symbol-function 'application-command-find))
                      (restricted   (make-instance 'application-command
                                                   :static-options '(:full :list :ask))))
                  (test-call-with-function-replacements
                   (list (list 'application-command-find
                               (lambda (name)
                                 (if (equal name "/permissions")
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
                          (progn (localgroup-client-call connection ':mode :value ':auto) nil)
                        (localgroup-error () t))
                      "structured mode admission honors the current supported choices")))))
           (localgroup-client-close connection))))))
  nil)
