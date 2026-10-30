;;;; -- Trusted Current-Interface Controller --

(in-package #:autolith-supervisor)

(defclass parent-interface-controller ()
  ((application :initarg :application :reader parent-interface-application
                :documentation "Trusted session and native launch owner.")
   (frontend :initarg :frontend :reader parent-interface-frontend
             :documentation "Current terminal editor, owned only by the parent.")
   (queue :initform nil :accessor parent-interface-queue
          :documentation "FIFO of unexecuted input entries.")
   (recalled :initform nil :accessor parent-interface-recalled
             :documentation "Queue entry currently in the composer, retaining its position.")
   (steering :initform nil :accessor parent-interface-steering
             :documentation "Separate sequential active-scope worker state.")
   (steering-entry :initform nil :accessor parent-interface-steering-entry
                   :documentation "FIFO entry under active classification or execution.")
   (steering-queue :initform nil :accessor parent-interface-steering-queue
                   :documentation "Ordered pending active child operations.")
   (commands :initform nil :accessor parent-interface-commands
             :documentation "Validated live child command metadata.")
   (completions :initform nil :accessor parent-interface-completions
                :documentation "Inert completion entries derived from the child catalog.")
   (lock :initform (bordeaux-threads:make-lock "parent interface work")
         :reader parent-interface-lock :documentation "Protects worker completion.")
   (thread :initform nil :accessor parent-interface-thread
           :documentation "Current durable child scope worker.")
   (done :initform nil :accessor parent-interface-done
         :documentation "Whether the current worker returned.")
   (result :initform nil :accessor parent-interface-result
           :documentation "Worker return values, never evaluated in the parent.")
   (failure :initform nil :accessor parent-interface-failure
            :documentation "Worker failure to propagate after joining.")
   (quit-p :initform nil :accessor parent-interface-quit-p
           :documentation "Whether the child requested normal application exit.")
   (eof-p :initform nil :accessor parent-interface-eof-p
          :documentation "Whether input ended and current work must finish cancelling."))
  (:documentation "Parent-only current editor scheduling and inert command metadata."))

(defclass parent-interface-input ()
  ((payload :initarg :payload :accessor parent-interface-input-payload
            :documentation "Text and optional image paths awaiting child admission.")
   (recalled-p :initform nil :accessor parent-interface-input-recalled-p
               :documentation "A recalled entry cannot execute before its edited submission.")
   (pending-p :initform nil :accessor parent-interface-input-pending-p
              :documentation "Active child classification must finish before ordinary execution."))
  (:documentation "Stable FIFO identity, including while its draft is recalled."))

(serapeum/types:-> parent-interface--words (string) list)
(defun parent-interface--words (text)
  "Split trusted command syntax without invoking the Lisp reader."
  (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline #\Return)) :test #'string=))

(serapeum/types:-> parent-interface--network-command-p (string) boolean)
(defun parent-interface--network-command-p (text)
  "Reserve network authority independently of all child command metadata."
  (not (null (member (first (parent-interface--words text)) '("/network") :test #'string-equal))))

(serapeum/types:-> parent-interface--network-completions () list)
(defun parent-interface--network-completions ()
  "Return parent-owned command and help completions."
  (loop for name in '("/network" "/network on" "/network off" "/network status" "/help network")
        collect (list :name name :description "Session network policy; changes require child restart.")))

(serapeum/types:-> parent-interface--network-help (parent-application) null)
(defun parent-interface--network-help (application)
  "Present the parent-owned network command help."
  (funcall (parent-application-renderer application) ':trusted ':notice
           (list :text "/network on|off|status: inspect or change whole-session outbound networking, including private and local addresses. Enabling requires trusted terminal YES. Changes restart the child after cleanup. The grant ends when you switch sessions. Windows does not support this grant."))
  nil)

(serapeum/types:-> parent-interface--network-run (parent-application string) keyword)
(defun parent-interface--network-run (application text)
  "Validate and run a trusted terminal command, without child command authority."
  (let* ((words  (parent-interface--words text))
         (option (second words)))
    (unless (and (<= (length words) 2)
                 (or (null option) (member option '("on" "off" "status") :test #'string-equal)))
      (error 'parent-application-operation-error :code ':invalid-network-command))
    (let ((outcome (if (or (null option) (string-equal option "status"))
                       ':unchanged
                       (parent-application-set-network
                        application (string-equal option "on")
                        :terminal (parent-application--terminal application)))))
      (funcall (parent-application-renderer application) ':trusted ':notice
               (list :text (case outcome
                             (:restarting
                              "Network policy change approved; restarting the child.")
                             (:denied
                              "Network access was not approved.")
                             (otherwise
                              (if (parent-application-network-p application)
                                  "Session network access: on."
                                  "Session network access: off.")))))
      outcome)))

(serapeum/types:-> parent-interface--text (t) string)
(defun parent-interface--text (input)
  "Get input text without reading or evaluating it."
  (user-message-input-text input))

(serapeum/types:-> parent-interface--kind (string) keyword)
(defun parent-interface--kind (text)
  "Classify composer syntax without invoking the Lisp reader in the parent."
  (let ((start (position-if-not (lambda (character) (find character '(#\Space #\Tab #\Newline #\Return)))
                                text)))
    (cond
      ((null start)
       ':message)
      ((char= (char text start) #\()
       ':lisp)
      ((and (char= (char text start) #\?)
            (< (1+ start) (length text)) (char= (char text (1+ start)) #\Space))
       ':async-lisp)
      ((char= (char text start) #\/)
       ':command)
      (t
       ':message))))

(serapeum/types:-> parent-interface--command (parent-interface-controller string) t)
(defun parent-interface--command (controller text)
  "Look up a slash command using the live catalog, including its aliases."
  (let* ((start (position #\/ text))
         (end   (and start (position-if (lambda (character) (find character '(#\Space #\Tab #\Newline)))
                                        text :start start)))
         (name  (and start (subseq text start end))))
    (find-if (lambda (command)
               (or (string-equal name (parent-proto:application-command.name command))
                   (member name (parent-proto:application-command.aliases command) :test #'string-equal)))
             (parent-interface-commands controller))))

(serapeum/types:-> parent-interface--catalog-completions (list) list)
(defun parent-interface--catalog-completions (commands)
  "Derive operation completions from validated child registry metadata."
  (loop for command in commands
        for name = (parent-proto:application-command.name command)
        for description = (parent-proto:application-command.description command)
        append
        (append
         (list (list :name name :argument (parent-proto:application-command.argument command)
                     :description description))
         (loop for alias in (parent-proto:application-command.aliases command)
               collect (list :name alias :primary name :description description))
         (loop for option in (parent-proto:application-command.options command)
               collect (list :name (format nil "~A ~A" name option) :description description)))))

(serapeum/types:-> parent-interface--accept-result
    (parent-interface-controller t &key (:catalog-p boolean)) null)
(defun parent-interface--accept-result (controller result &key catalog-p)
  "Validate child results and update inert metadata. Never evaluate result strings."
  (when result
    (protocol-application-result-validate result)
    (when (or catalog-p (parent-proto:application-result.commands result))
      (setf (parent-interface-commands controller) (parent-proto:application-result.commands result)
            (parent-interface-completions controller)
            (append
             (parent-interface--network-completions)
             (parent-interface--catalog-completions (parent-interface-commands controller))
             (loop for entry in (parent-proto:application-result.completion-entries result)
                   collect (list :name        (parent-proto:application-completion.name entry)
                                 :argument    (parent-proto:application-completion.argument entry)
                                 :description (parent-proto:application-completion.description entry)
                                 :primary     (let ((name (parent-proto:application-completion.primary entry)))
                                                (unless (zerop (length name)) name)))))))
    (when (parent-proto:application-result.quit result)
      (setf (parent-interface-quit-p controller) t)))
  nil)

(serapeum/types:-> parent-interface--enqueue (parent-interface-controller t) null)
(defun parent-interface--enqueue (controller payload)
  "Append new input, or commit a recalled draft at its original FIFO position."
  (let ((recalled (parent-interface-recalled controller)))
    (if recalled
        (setf (parent-interface-input-payload recalled) payload
              (parent-interface-input-recalled-p recalled) nil
              (parent-interface-recalled controller) nil)
        (setf (parent-interface-queue controller)
              (nconc (parent-interface-queue controller)
                     (list (make-instance 'parent-interface-input :payload payload))))))
  (setf (parent-terminal-ui-queue-editing-p (parent-interface-frontend controller)) nil)
  nil)

(serapeum/types:-> parent-interface--recall (parent-interface-controller &key (:cycle-p boolean)) null)
(defun parent-interface--recall (controller &key cycle-p)
  "Recall a queued draft without changing its position or losing another recalled draft."
  (let* ((queue (remove-if #'parent-interface-input-pending-p (parent-interface-queue controller)))
         (old   (parent-interface-recalled controller))
         (next  (if (and cycle-p old)
                    (or (second (member old queue)) (first queue))
                    (first queue))))
    (when next
      (when old
        (setf (parent-interface-input-recalled-p old) nil
              (parent-interface-input-payload old)
              (terminal-ui--submission-input
               (parent-terminal-ui-editor-ui (parent-interface-frontend controller))
               (clinedi:line-editor-text
                (terminal-ui-editor (parent-terminal-ui-editor-ui (parent-interface-frontend controller)))))))
      (setf (parent-interface-recalled controller) next
            (parent-interface-input-recalled-p next) t
            (parent-terminal-ui-queue-editing-p (parent-interface-frontend controller)) t)
      (terminal-ui--set-draft-input
       (parent-terminal-ui-editor-ui (parent-interface-frontend controller))
       (parent-interface-input-payload next))))
  nil)

(serapeum/types:-> parent-interface--start-work (parent-interface-controller function) null)
(defun parent-interface--start-work (controller function)
  "Start one child scope worker while retaining responsive parent input."
  (when (parent-interface-thread controller)
    (error 'parent-application-operation-error :code ':busy))
  (setf (parent-interface-done controller) nil
        (parent-interface-result controller) nil
        (parent-interface-failure controller) nil
        (parent-terminal-ui-active-p (parent-interface-frontend controller)) t)
  (setf (parent-interface-thread controller)
        (bordeaux-threads:make-thread
         (lambda ()
           (let ((result nil) (failure nil))
             (handler-case
                 (setf result (multiple-value-list (funcall function)))
               (error (condition) (setf failure condition)))
             (bordeaux-threads:with-lock-held ((parent-interface-lock controller))
               (setf (parent-interface-result controller) result
                     (parent-interface-failure controller) failure
                     (parent-interface-done controller) t))))
         :name "trusted interface child scope"))
  nil)

(serapeum/types:-> parent-interface--finish-work (parent-interface-controller) boolean)
(defun parent-interface--finish-work (controller)
  "Join completed child work and propagate uncertain outcomes without retry."
  (when (and (parent-interface-thread controller)
             (bordeaux-threads:with-lock-held ((parent-interface-lock controller))
               (parent-interface-done controller)))
    (sb-thread:join-thread (parent-interface-thread controller))
    (setf (parent-interface-thread controller) nil
          (parent-terminal-ui-active-p (parent-interface-frontend controller)) nil)
    (when (parent-interface-failure controller)
      (error (parent-interface-failure controller)))
    (parent-interface--accept-result controller (second (parent-interface-result controller)))
    (parent-interface--accept-result controller (third (parent-interface-result controller)) :catalog-p t)
    (return-from parent-interface--finish-work t))
  nil)

(serapeum/types:-> parent-interface--active-route-p (parent-interface-controller t) boolean)
(defun parent-interface--active-route-p (controller payload)
  "Send local Lisp and permitted shared commands to the child's active dispatcher."
  (let* ((text    (parent-interface--text payload))
         (kind    (parent-interface--kind text))
         (command (and (eq kind ':command) (parent-interface--command controller text))))
    (and (not (parent-interface--network-command-p text))
         (not (null
               (or (member kind '(:lisp :async-lisp))
                   (and command
                        (not (eq (parent-proto:application-command.busy-behavior command) ':hold))
                        (eq (parent-proto:application-command.terminal-behavior command) ':shared))))))))

(serapeum/types:-> parent-interface--submit (parent-interface-controller t &key (:active-p boolean)) null)
(defun parent-interface--submit (controller payload &key active-p)
  "Retain FIFO input, then classify eligible active actions inside the child."
  (let ((recalled (parent-interface-recalled controller)))
    (parent-interface--enqueue controller payload)
    (when (and active-p (null recalled) (parent-interface--active-route-p controller payload))
      (let ((entry (first (last (parent-interface-queue controller)))))
        (setf (parent-interface-input-pending-p entry) t
              (parent-interface-steering-queue controller)
              (nconc (parent-interface-steering-queue controller) (list entry))))))
  nil)

(serapeum/types:-> parent-interface--finish-steering (parent-interface-controller) null)
(defun parent-interface--finish-steering (controller)
  "Commit an active result or retain its original input position when held."
  (let ((worker (parent-interface-steering controller)))
    (when (and worker (parent-interface--finish-work worker))
      (let ((entry       (parent-interface-steering-entry controller))
            (disposition (first (parent-interface-result worker))))
        (unless (member disposition '(:handled :hold))
          (error 'parent-application-operation-error :code ':invalid-active-disposition))
        (parent-interface--accept-result controller (second (parent-interface-result worker)))
        (parent-interface--accept-result controller (third (parent-interface-result worker)) :catalog-p t)
        (setf (parent-interface-input-pending-p entry) nil)
        (when (eq disposition ':handled)
          (setf (parent-interface-queue controller)
                (remove entry (parent-interface-queue controller))))
        (setf (parent-interface-steering controller) nil
              (parent-interface-steering-entry controller) nil))))
  nil)

(serapeum/types:-> parent-interface-drive
    (parent-interface-controller &key (:poll-function function) (:execute-function function)
     (:active-function function) (:cancel-function function) (:stop-function function)
     (:idle-function (or null function))) keyword)
(defun parent-interface-drive
    (controller &key poll-function execute-function active-function cancel-function stop-function idle-function)
  "Drive responsive current-interface input with parent FIFO and child-only evaluation.

POLL-FUNCTION takes seconds and returns action and input. EXECUTE-FUNCTION takes
input and returns outcome and optional ApplicationResult. ACTIVE-FUNCTION takes
input and returns :HANDLED or :HOLD plus optional ApplicationResult. It must let
the child classify Lisp. CANCEL-FUNCTION cancels active work. STOP-FUNCTION is a
trusted connection/replacement predicate. Each accepted input executes at most
once; uncertain connection outcomes propagate instead of retrying."
  (unless (every #'functionp (list poll-function execute-function active-function cancel-function stop-function))
    (error 'parent-application-operation-error :code ':invalid-controller))
  (unwind-protect
       (loop
         (when (funcall stop-function)
           (return ':replaced))
         (parent-interface--finish-work controller)
         (parent-interface--finish-steering controller)
         (let ((active-p (not (null (parent-interface-thread controller)))))
           (setf (parent-terminal-ui-active-p (parent-interface-frontend controller)) active-p)
           (when (and (not active-p) (null (parent-interface-steering controller))
                      (or (parent-interface-quit-p controller) (parent-interface-eof-p controller)))
             (return (if (parent-interface-eof-p controller) ':eof ':completed)))
           (when (and active-p (null (parent-interface-steering controller))
                      (parent-interface-steering-queue controller))
             (let* ((entry  (pop (parent-interface-steering-queue controller)))
                    (worker (make-instance 'parent-interface-controller
                                           :application (parent-interface-application controller)
                                           :frontend    (parent-interface-frontend controller))))
               (setf (parent-interface-steering controller) worker
                     (parent-interface-steering-entry controller) entry)
               (parent-interface--start-work
                worker (lambda () (funcall active-function (parent-interface-input-payload entry))))))
           (when (and (not active-p) (null (parent-interface-steering controller))
                      (not (parent-interface-eof-p controller)))
             ;; A finished provider turn makes unstarted active classifications ordinary FIFO work.
             (dolist (entry (parent-interface-steering-queue controller))
               (setf (parent-interface-input-pending-p entry) nil))
             (setf (parent-interface-steering-queue controller) nil)
             (let ((entry     (first (parent-interface-queue controller)))
                   (idle-work (and idle-function (funcall idle-function))))
               (cond
                 (idle-work
                  (parent-interface--start-work controller idle-work))
                 ((and entry (not (parent-interface-input-recalled-p entry))
                       (not (parent-interface-input-pending-p entry)))
                  (pop (parent-interface-queue controller))
                  (parent-interface--start-work
                   controller (lambda () (funcall execute-function (parent-interface-input-payload entry))))))))
           (unless (parent-interface-eof-p controller)
             (multiple-value-bind (action payload) (funcall poll-function 0.02)
               (case action
                ((:submit :queue)
                 (when payload
                   (parent-interface--submit
                    controller payload
                    :active-p (and (eq action ':submit)
                                   (not (null (parent-interface-thread controller)))))))
                (:interrupt
                 (if (parent-interface-thread controller)
                     (funcall cancel-function)
                     (setf (parent-interface-eof-p controller) t)))
                (:eof
                 (setf (parent-interface-eof-p controller) t)
                 (when (or (parent-interface-thread controller) (parent-interface-steering controller))
                   (funcall cancel-function)))
                (:edit-queue
                 (parent-interface--recall controller))
                (:cycle-queue
                 (parent-interface--recall controller :cycle-p t))
                (:kept
                 (when (parent-interface-recalled controller)
                   (parent-interface--enqueue controller payload)))
                (:timeout
                 nil)
                (otherwise
                 (error 'parent-application-operation-error :code ':invalid-interface-action)))))
           (when (parent-interface-eof-p controller) (sleep 0.005))))
    (when (or (parent-interface-thread controller) (parent-interface-steering controller))
      (funcall cancel-function))
    (dolist (worker (list controller (parent-interface-steering controller)))
      (when (and worker (parent-interface-thread worker))
        (multiple-value-bind (value reason)
            (sb-thread:join-thread (parent-interface-thread worker) :timeout 5 :default ':not-joined)
          (declare (ignore value))
          (when reason
            (error 'parent-application-operation-error :code ':controller-worker-did-not-stop)))
        (setf (parent-interface-thread worker) nil)))
    (when (parent-interface-steering-entry controller)
      (setf (parent-interface-queue controller)
            (remove (parent-interface-steering-entry controller) (parent-interface-queue controller))))
    (dolist (entry (parent-interface-steering-queue controller))
      (setf (parent-interface-input-pending-p entry) nil))
    (setf (parent-interface-steering controller) nil
          (parent-interface-steering-entry controller) nil
          (parent-interface-steering-queue controller) nil)
    (setf (parent-terminal-ui-active-p (parent-interface-frontend controller)) nil)))

(serapeum/types:-> parent-interface--source (t) string)
(defun parent-interface--source (payload)
  "Remove only the async composer marker. The parent never reads the source."
  (let ((text (parent-interface--text payload)))
    (if (eq (parent-interface--kind text) ':async-lisp)
        (subseq text (+ 2 (position #\? text)))
        text)))

(serapeum/types:-> parent-interface--child-run
    (parent-interface-controller parent-launch t &key (:active-p boolean)) (values keyword t t))
(defun parent-interface--child-run (controller launch payload &key active-p)
  "Send one inert composer submission and refresh live operation metadata."
  (let* ((text        (parent-interface--text payload))
         (words       (parent-interface--words text))
         (application (parent-interface-application controller))
         (kind        (parent-interface--kind text)))
    (when (parent-interface--network-command-p text)
      (when active-p
        (return-from parent-interface--child-run (values ':hold nil nil)))
      (handler-case (parent-interface--network-run application text)
        (parent-application-operation-error (condition)
          (unless (eq (parent-application-operation-error-code condition) ':invalid-network-command)
            (error condition))
          (parent-interface--network-help application))
        (access-policy-error (condition)
          (unless (eq (access-policy-error-code condition) ':unsupported-native-network)
            (error condition))
          (funcall (parent-application-renderer application) ':trusted ':notice
                   (list :text "Session network access is not supported on this host."))))
      (return-from parent-interface--child-run (values ':completed nil nil)))
    (when (and (string-equal (first words) "/help")
               (= (length words) 2)
               (member (second words) '("network" "/network") :test #'string-equal))
      (parent-interface--network-help application)
      (return-from parent-interface--child-run (values (if active-p ':handled ':completed) nil nil)))
    (multiple-value-bind (outcome result)
        (parent-launch-application-run
         launch (parent-application--identifier) (parent-interface--source payload)
         :kind kind :image-paths (mapcar #'namestring (user-message-input-image-pathnames payload)))
      (when (and (string-equal (first words) "/help") (= (length words) 1))
        (parent-interface--network-help application))
      (let ((catalog
              (unless (or (eq kind ':message)
                          (and result (parent-proto:application-result.quit result))
                          (parent-application--restart-p (parent-interface-application controller)))
                (nth-value 1
                           (parent-launch-application-run
                            launch (parent-application--identifier) "" :kind ':catalog)))))
        (values (if active-p ':handled outcome) result catalog)))))

(serapeum/types:-> parent-application-run-interface
    (parent-application parent-terminal-ui &key (:controller (or null parent-interface-controller))
     (:launch (or null parent-launch)) (:notification-function (or null function))) keyword)
(defun parent-application-run-interface (application frontend &key controller launch notification-function)
  "Run the current editor over the mandatory isolated launch.

Keep CONTROLLER across policy replacements to retain drafts and queued input.
This entry is also available to trusted terminal adapters. It does not create
an unconfined process or load the agent system into the parent."
  (let* ((connection (or launch (parent-application-launch application)))
         (state      (or controller (make-instance 'parent-interface-controller
                                                  :application application :frontend frontend))))
    (unless connection
      (error 'parent-application-operation-error :code ':application-not-running))
    (multiple-value-bind (outcome catalog)
        (parent-launch-application-run connection (parent-application--identifier) "" :kind ':catalog)
      (unless (eq outcome ':completed)
        (error 'parent-application-operation-error :code ':catalog-failed))
      (parent-interface--accept-result state catalog))
    (parent-interface-drive
     state
     :poll-function
     (lambda (seconds)
       (terminal-ui-set-pending-inputs
        (parent-terminal-ui-editor-ui frontend)
        (mapcar (lambda (entry) (parent-interface--text (parent-interface-input-payload entry)))
                (remove-if-not #'parent-interface-input-pending-p (parent-interface-queue state)))
        (mapcar (lambda (entry) (parent-interface--text (parent-interface-input-payload entry)))
                (remove-if #'parent-interface-input-pending-p (parent-interface-queue state))))
       (parent-terminal-ui-poll-event frontend seconds))
     :execute-function (lambda (payload) (parent-interface--child-run state connection payload))
     :active-function  (lambda (payload) (parent-interface--child-run state connection payload :active-p t))
     :cancel-function  (lambda () (parent-launch-cancel connection))
     :idle-function
     (lambda ()
       (when (and notification-function (funcall notification-function))
         (lambda ()
           (parent-launch-application-run connection (parent-application--identifier) "" :kind ':pump))))
     :stop-function
     (lambda ()
       (or (parent-application--restart-p application)
           (bordeaux-threads:with-lock-held ((parent-launch-lock connection))
             (when (parent-launch-failure connection)
               (error 'parent-launch-error :reason (parent-launch-failure connection)))
             (not (null (or (parent-launch-closing connection) (parent-launch-stopping connection))))))))))


;;;; -- Trusted Image Selection and Recovery --

(serapeum/types:-> parent-application-image-descriptors (parent-configuration) list)
(defun parent-application-image-descriptors (configuration)
  "Read inert retained manifests and offer only source/runtime-compatible cores."
  (let* ((data  (parent-configuration-get configuration ':data-root))
         (state (parent-configuration-get configuration ':state-root))
         (descriptors
           (loop for (key label pristine-p) in
                 '((:agent-core "Current image" nil) (:worker-core "Pristine image" t))
                 for core = (parent-configuration-get configuration key)
                 when (and core (probe-file core))
                   collect (list :id (string-downcase key) :label label :core core :pristine-p pristine-p))))
    (when (and data state)
      (let* ((root     (merge-pathnames "generations/" (uiop:ensure-directory-pathname data)))
             (store    (sbcl-generations:make-generation-store
                        :root root :current-pathname (merge-pathnames "current-generation.sexp"
                                                                      (uiop:ensure-directory-pathname state))
                        :core-name "autolith.core" :temporary-core-name ".autolith.core.tmp"
                        :manifest-version 3 :accepted-manifest-versions '(1 2 3)))
             (revision (sbcl-generations:source-revision
                        (sbcl-generations:make-source-provenance
                         (pathname (parent-configuration-get configuration ':source-root))))))
        (when (uiop:directory-exists-p root)
          (dolist (directory (uiop:subdirectories root))
            (let ((manifest (merge-pathnames "manifest.sexp" directory)))
              (when (probe-file manifest)
                (handler-case
                    (let* ((generation         (sbcl-generations:generation-load-manifest manifest store))
                           (core               (ls-compat.posix:resolve-pathname
                                                (sbcl-generations:generation-core-pathname generation)))
                           (resolved-directory (ls-compat.posix:resolve-pathname directory))
                           (metadata           (sbcl-generations:generation-metadata generation)))
                      (when (and (sbcl-generations:generation-compatible-p generation store)
                                 (equal revision (getf metadata :git-commit))
                                 (uiop:subpathp core resolved-directory)
                                 (parent-application--session-id-p
                                  (sbcl-generations:generation-identifier generation)))
                        (push (list :id         (sbcl-generations:generation-identifier generation)
                                    :label      (format nil "Retained ~A"
                                                        (sbcl-generations:generation-identifier generation))
                                    :core (namestring core) :pristine-p nil
                                    :created-at (sbcl-generations:generation-created-at generation))
                              descriptors)))
                  ;; An invalid, incomplete or incompatible retained image is not bootable.
                  (sbcl-generations:checkpoint-error () nil))))))))
    (sort descriptors #'> :key (lambda (descriptor) (getf descriptor :created-at most-positive-fixnum)))))

(serapeum/types:-> parent-application-select-image
    (parent-application parent-terminal-ui) (values t keyword))
(defun parent-application-select-image (application frontend)
  "Select an approved core through the complete trusted current-interface picker."
  (let* ((descriptors (parent-application-image-descriptors (parent-application-configuration application)))
         (identifier  (parent-terminal-ui-choose
                       frontend "Select image"
                       (loop for descriptor in descriptors
                             collect (list :name  (getf descriptor :label)
                                           :value (getf descriptor :id))))))
    (values (find identifier descriptors :key (lambda (item) (getf item :id)) :test #'equal)
            (if identifier ':selected ':cancelled))))

(serapeum/types:-> parent-application--apply-image (parent-application list) null)
(defun parent-application--apply-image (application descriptor)
  "Change the next core only after native cleanup. Keep private state outside child scopes."
  (let* ((configuration (parent-application-configuration application))
         (values        (copy-tree (parent-configuration-values configuration)))
         (settings      (copy-tree (getf values :settings)))
         (core          (getf descriptor :core))
         (launch        (parent-application-launch application)))
    (unless (and (stringp core) (probe-file core)
                 (or (null launch) (parent-launch-evidence launch)))
      (error 'parent-application-operation-error :code ':image-cleanup-required))
    (setf (getf values :agent-core) core
          (getf values :runtime-roots)
          (remove-duplicates
           (cons (namestring (uiop:pathname-directory-pathname (pathname core)))
                 (getf values :runtime-roots)) :test #'equal)
          (getf settings :pristine-p) (getf descriptor :pristine-p)
          (getf values :settings) settings)
    (let ((replacement (make-parent-configuration values)))
      ;; Validate native private-root separation before accepting the selected core.
      (parent-application-private-roots replacement)
      (setf (slot-value configuration 'values) (parent-configuration-values replacement)))
    (parent-application--select-code-files configuration (getf descriptor :pristine-p))
    (setf (parent-application-policy application) nil
          (parent-application-backend application) nil
          (parent-application-access-session application) nil
          (parent-application-native-application application) nil))
  nil)

(serapeum/types:-> parent-application--crash-record (parent-application error) null)
(defun parent-application--crash-record (application condition)
  "Append a bounded parent crash record without condition text or stack arguments."
  (let ((record (list ':crash ':version 1 ':time (get-universal-time)
                      ':session-id (parent-application-session-id application)
                      ':condition-type (symbol-name (type-of condition)))))
    (with-open-file (stream (parent-application--path application "crashes.sexp")
                            :direction ':output :if-exists ':append :if-does-not-exist ':create)
      (with-standard-io-syntax
        (write record :stream stream :readably t)
        (terpri stream)
        (finish-output stream))))
  nil)

(serapeum/types:-> parent-application-recover (parent-application parent-terminal-ui error) boolean)
(defun parent-application-recover (application frontend condition)
  "Keep durable history and recover through a validated core after native cleanup.
  Never retry an uncertain submitted action. The operator must submit new input."
  (parent-application--crash-record application condition)
  (parent-terminal-ui-render
   frontend :region ':trusted :kind ':recovery
   :payload (list :text "The child stopped. Durable history is available. Select an image to restart, or cancel to exit. An uncertain action will not be retried."))
  (multiple-value-bind (descriptor state) (parent-application-select-image application frontend)
    (when (eq state ':selected)
      (parent-application--apply-image application descriptor)
      (return-from parent-application-recover t)))
  nil)

(serapeum/types:-> parent-interface-restart-handler
    (parent-application parent-terminal-ui) function)
(defun parent-interface-restart-handler (application frontend)
  "Choose child Lisp restarts as data, or defer an approved image replacement."
  (lambda (arguments cancelled)
    (let ((result   (make-hash-table :test #'equal))
          (restarts (gethash "restarts" arguments)))
      (unless (funcall cancelled)
        (if restarts
            (progn
              (unless (and (typep restarts 'vector) (<= (length restarts) 256)
                           (every (lambda (entry)
                                    (and (hash-table-p entry)
                                         (stringp (gethash "name" entry))
                                         (stringp (gethash "report" entry)))) restarts))
                (error 'parent-application-operation-error :code ':invalid-request))
              (parent-terminal-ui-render
               frontend :region ':agent :kind ':output
               :payload (list :text (format nil "~{~A~%~}"
                                            (loop for entry across restarts for index from 0
                                                  collect (format nil "~D: ~A: ~A" index
                                                                  (gethash "name" entry)
                                                                  (gethash "report" entry))))))
              (multiple-value-bind (choice state)
                  (parent-terminal-ui-read frontend "Lisp restart index" :cancelled cancelled)
                (when (eq state ':line)
                  (let ((index (handler-case (parse-integer choice) (parse-error () nil))))
                    (when (and index (<= 0 index) (< index (length restarts)))
                      (multiple-value-bind (source source-state)
                          (parent-terminal-ui-read frontend "Restart argument forms (empty for none)"
                                                   :cancelled cancelled)
                        (when (and (eq source-state ':line) (not (funcall cancelled)))
                          (setf (gethash "index" result) index)
                          (when (plusp (length source)) (setf (gethash "source" result) source)))))))))
            (multiple-value-bind (descriptor state) (parent-application-select-image application frontend)
              (when (and (eq state ':selected) (not (funcall cancelled)))
                (bordeaux-threads:with-lock-held ((parent-application-lock application))
                  (setf (parent-application-after-stop application)
                        (lambda () (parent-application--apply-image application descriptor))
                        (parent-application-restart application) t))
                (setf (gethash "restart" result) t)))))
      result)))


;;;; -- Child Code Projections --

(defvar *parent-application-code-files* (make-hash-table :test #'eq)
  "Parent-only exact projected startup files keyed by trusted configuration.")

(serapeum/types:-> parent-application--prepare-session-code
    (parent-configuration parent-configuration) parent-configuration)
(defun parent-application--prepare-session-code (source replacement)
  "Keep the exact approved startup files when switching durable session storage."
  (let ((files (gethash source *parent-application-code-files*)))
    (if files
        (setf (gethash replacement *parent-application-code-files*) (copy-tree files))
        (parent-application-prepare-configuration replacement)))
  replacement)

(serapeum/types:-> parent-application--inside-p (string string) boolean)
(defun parent-application--inside-p (path root)
  "Check canonical literal containment without wildcard interpretation."
  (not (null (uiop:subpathp (pathname path) (uiop:ensure-directory-pathname root)))))

(serapeum/types:-> parent-application--read-inert (pathname) t)
(defun parent-application--read-inert (pathname)
  "Read one bounded data form without reader evaluation."
  (with-open-file (stream pathname :direction ':input)
    (when (> (file-length stream) (* 4 1024 1024))
      (parent-configuration--reject ':startup-manifest-too-large))
    (with-standard-io-syntax
      (let ((*read-eval* nil) (*package* (find-package ':keyword)))
        (let ((form (read stream nil nil)))
          (unless (eq (read stream nil ':end) ':end)
            (parent-configuration--reject ':startup-manifest-trailing-data))
          form)))))

(serapeum/types:-> parent-application--projection-target (pathname pathname) pathname)
(defun parent-application--projection-target (root relative)
  "Reject existing links and nonregular objects in a startup projection."
  (let* ((base    (ls-compat.posix:resolve-pathname (uiop:pathname-parent-directory-pathname root)))
         (target  (merge-pathnames relative root))
         (parts   (remove "" (uiop:split-string (enough-namestring target base) :separator '(#\/))
                          :test #'equal))
         (current base))
    (loop for tail on parts
          for final-p = (null (rest tail))
          do (setf current (merge-pathnames (first tail) current))
             (let ((information
                     (handler-case (ls-compat.posix:file-information current :follow-links-p nil)
                       (ls-compat.posix:file-operation-failed () nil))))
               (when (and information
                          (not (eq (ls-compat.posix:file-information-kind information)
                                   (if final-p ':file ':directory))))
                 (parent-configuration--reject ':invalid-startup-projection)))
             (unless final-p (setf current (uiop:ensure-directory-pathname current))))
    target))

(serapeum/types:-> parent-application--project-code (parent-configuration pathname) string)
(defun parent-application--project-code (configuration source)
  "Copy exact approved bytes to a content-addressed read-only child projection."
  (let* ((data      (or (parent-configuration-get configuration ':data-root)
                        (parent-configuration-get configuration ':private-root)))
         (root      (merge-pathnames "agent-code/" (uiop:ensure-directory-pathname
                                                    (ls-compat.posix:resolve-pathname data))))
         (resolved  (namestring root))
         (forbidden (append (parent-application-private-roots configuration)
                            (remove nil (list (parent-configuration-get configuration ':workspace)
                                              (parent-configuration-get configuration ':temporary-root)
                                              (getf (parent-configuration-get configuration ':settings)
                                                    :site-config-root))))))
    (when (some (lambda (path)
                  (access-policy--overlap-p resolved
                                           (namestring (ls-compat.posix:resolve-pathname path))))
                forbidden)
      (parent-configuration--reject ':startup-projection-overlaps-scope))
    (let* ((bytes  (with-open-stream (stream (ls-compat.posix:open-regular-file source))
                      (let ((length (file-length stream)))
                        (when (> length (* 16 1024 1024))
                          (parent-configuration--reject ':startup-code-too-large))
                        (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
                          (unless (= (read-sequence bytes stream) length)
                            (parent-configuration--reject ':startup-code-changed))
                          bytes))))
           (digest (ironclad:byte-array-to-hex-string (ironclad:digest-sequence ':sha256 bytes)))
           (target (parent-application--projection-target
                     root (merge-pathnames "startup.lisp" (uiop:ensure-directory-pathname digest)))))
      (ensure-directories-exist target)
      (if (probe-file target)
          (unless (equalp bytes (with-open-stream (stream (ls-compat.posix:open-regular-file target))
                                 (let ((existing (make-array (file-length stream)
                                                             :element-type '(unsigned-byte 8))))
                                   (read-sequence existing stream)
                                   existing)))
            (parent-configuration--reject ':startup-projection-corrupt))
          (with-open-file (stream target :direction ':output :element-type '(unsigned-byte 8)
                                        :if-exists ':error :if-does-not-exist ':create)
            (write-sequence bytes stream)
            (finish-output stream)))
      (namestring (ls-compat.posix:resolve-pathname target)))))

(serapeum/types:-> parent-application-prepare-configuration (parent-configuration) parent-configuration)
(defun parent-application-prepare-configuration (configuration)
  "Project approved replay and init files without evaluating any child Lisp."
  (when (gethash configuration *parent-application-code-files*)
    (return-from parent-application-prepare-configuration configuration))
  (let* ((settings    (parent-configuration-get configuration ':settings))
         (config-root (parent-configuration-get configuration ':config-root))
         (state-root  (parent-configuration-get configuration ':state-root))
         (data-root   (parent-configuration-get configuration ':data-root))
         (workspace   (namestring (ls-compat.posix:resolve-pathname
                                   (parent-configuration-get configuration ':workspace))))
         (init nil) (replay nil))
    (unless (getf settings :pristine-p)
      (labels ((init-file (root suffix)
                 (when root
                   (let ((path (merge-pathnames suffix (uiop:ensure-directory-pathname root))))
                     (when (probe-file path) (push path init))))))
        (init-file (getf settings :site-config-root) "init.lisp")
        (when config-root
          (let ((manifest (merge-pathnames "directory-scopes.sexp"
                                           (uiop:ensure-directory-pathname config-root))))
            (when (probe-file manifest)
              (let* ((data        (parent-application--read-inert manifest))
                     (directories (getf data :directories)))
                (unless (and (eql (getf data :version) 1) (listp directories)
                             (<= (length directories) 64)
                             (every (lambda (path) (and (stringp path) (uiop:absolute-pathname-p path))) directories))
                  (parent-configuration--reject ':invalid-directory-trust))
                (dolist (directory (sort (copy-list directories) #'< :key #'length))
                  (let ((canonical (namestring (ls-compat.posix:resolve-pathname
                                                (uiop:ensure-directory-pathname directory)))))
                    (when (parent-application--inside-p workspace canonical)
                      (init-file canonical ".autolith/init.lisp"))))))))
        (init-file config-root "init.lisp"))
      (when state-root
        (let ((pointer (merge-pathnames "current-image-commit.sexp"
                                       (uiop:ensure-directory-pathname state-root))))
          (when (probe-file pointer)
            (let* ((record (parent-application--read-inert pointer))
                   (fields (rest record)) (identifier (getf fields :id)))
              (unless (and (eq (first record) ':current-image-commit)
                           (member (getf fields :version) '(1 2))
                           (parent-application--session-id-p identifier) data-root)
                (parent-configuration--reject ':invalid-selected-replay))
              (let* ((directory (merge-pathnames (format nil "image-commits/~A/" identifier)
                                                 (uiop:ensure-directory-pathname data-root)))
                     (manifest  (merge-pathnames "manifest.sexp" directory)))
                (unless (probe-file manifest)
                  (parent-configuration--reject ':selected-replay-unavailable))
                (let* ((data (parent-application--read-inert manifest))
                       (fields (rest data)) (script (getf fields :script)))
                  (unless (and (eq (first data) ':image-commit) (eql (getf fields :version) 1)
                               (equal (getf fields :id) identifier) (stringp script)
                               (probe-file script)
                               (parent-application--inside-p
                                (namestring (ls-compat.posix:resolve-pathname script))
                                (namestring (ls-compat.posix:resolve-pathname directory))))
                    (parent-configuration--reject ':invalid-selected-replay))
                  (push (pathname script) replay))))))))
    (let ((projected-init   (mapcar (lambda (path) (parent-application--project-code configuration path))
                                   (nreverse init)))
          (projected-replay (mapcar (lambda (path) (parent-application--project-code configuration path))
                                   (nreverse replay))))
      (setf (getf (slot-value configuration 'values) :runtime-roots)
            (remove-duplicates
             (append (parent-configuration-get configuration ':runtime-roots)
                     (mapcar (lambda (path)
                               (namestring (uiop:pathname-directory-pathname (pathname path))))
                             (append projected-init projected-replay))) :test #'equal)
            (gethash configuration *parent-application-code-files*)
            (list :init projected-init :replay projected-replay :all-init projected-init))
      (when (getf settings :site-config-root)
        (setf (getf (getf (slot-value configuration 'values) :settings) :site-config-root)
              (and projected-init
                   (namestring (uiop:pathname-directory-pathname (first projected-init))))))))
  configuration)

(serapeum/types:-> parent-application--select-code-files (parent-configuration boolean) null)
(defun parent-application--select-code-files (configuration pristine-p)
  "Do not replay a different private commit into an already selected heap."
  (let ((files (gethash configuration *parent-application-code-files*)))
    (when files
      (setf (getf files :replay) nil
            (getf files :init) (unless pristine-p (getf files :all-init))
            (gethash configuration *parent-application-code-files*) files)))
  nil)

(serapeum/types:-> parent-application-configure (parent-application parent-launch) keyword)
(defun parent-application-configure (application launch)
  "Load exact projected startup files in an isolated durable child scope."
  (let* ((configuration (parent-application-configuration application))
         (files         (gethash configuration *parent-application-code-files*)))
    (unless files (parent-configuration--reject ':startup-not-prepared))
    (multiple-value-bind (outcome result)
        (parent-launch-application-run
         launch (parent-application--identifier)
         (agentcomms:json-encode
          (agentcomms:json-object "init_files" (coerce (getf files :init) 'vector)
                                 "replay_files" (coerce (getf files :replay) 'vector)))
         :kind ':configure)
      (unless (and (eq outcome ':completed) result
                   (eq (parent-proto:application-result.status result) ':ok))
        (error 'parent-application-operation-error :code ':configuration-failed))
      outcome)))


;;;; -- Ordinary Parent Session --

(serapeum/types:-> parent-interface--wire-keyword (t list) keyword)
(defun parent-interface--wire-keyword (value allowed)
  "Decode one finite public name without creating symbols."
  (or (and (keywordp value) (find value allowed))
      (and (stringp value) (find value allowed :key #'symbol-name :test #'string-equal))
      (terminal-ui--reject-event)))

(serapeum/types:-> parent-interface-decode-ui (hash-table) (values keyword t))
(defun parent-interface-decode-ui (fields)
  "Decode bounded inert UI maps through a fixed name vocabulary."
  (let ((nodes 0) (characters 0)
        (keys   '(:rows :tail :tail-rows :id :content :status :details :worked-seconds :used :window
                  :compaction-limit :estimated-p :text :duration-seconds :activities :steering
                  :queued :active-p :kind :style :role :label :action :index :agent :state
                  :current-tool :current-tool-duration-ms :recent-tools :request-count
                  :duration-ms :assignment :detached :type :tool :description))
        (values (append '(:span :widget :copy :open-url :queued :running :tool)
                        *terminal-style-names*)))
    (labels ((decode (value depth)
               (when (or (> (incf nodes) *terminal-ui-event-node-limit*) (> depth 16))
                 (terminal-ui--reject-event))
               (typecase value
                 (hash-table
                  (cond
                    ((and (= (hash-table-count value) 1) (gethash "keyword" value))
                     (parent-interface--wire-keyword (gethash "keyword" value) (append keys values)))
                    (t
                     (loop for key being the hash-keys of value using (hash-value item)
                           append (list (parent-interface--wire-keyword key keys)
                                        (decode item (1+ depth)))))))
                 (string
                  (when (> (incf characters (length value)) *terminal-ui-event-character-limit*)
                    (terminal-ui--reject-event))
                  value)
                 (vector
                  (map 'list (lambda (item) (decode item (1+ depth))) value))
                 (cons
                  (mapcar (lambda (item) (decode item (1+ depth))) value))
                 ((or null real)
                  value)
                 (symbol
                  (if (eq value t) t (parent-interface--wire-keyword value (append keys values))))
                 (t
                  (terminal-ui--reject-event)))))
      (let ((kind    (parent-interface--wire-keyword
                      (gethash "kind" fields)
                      '(:stream :finalized :epilogue :preview :status :context :notice :agent-activities
                        :command-activities :pending-inputs :compacting :local-activity :idle-status :reset-transcript)))
            (payload (decode (gethash "payload" fields) 0)))
        (unless (terminal-ui-event-valid-p kind payload) (terminal-ui--reject-event))
        (values kind payload)))))

(serapeum/types:-> parent-interface--initial-image-input
    (parent-configuration list) (or null user-message-input))
(defun parent-interface--initial-image-input (configuration images)
  "Build the initial image draft without evaluating or reading attachment contents."
  (when images
    (let ((paths
            (mapcar
             (lambda (value)
               (unless (and (stringp value) (plusp (length value)) (not (find #\Null value)))
                 (parent-cli--reject ':invalid-image))
               (let ((path (ls-compat.posix:resolve-pathname
                            (merge-pathnames value
                              (uiop:ensure-directory-pathname
                               (parent-configuration-get configuration ':workspace))))))
                 (unless (eq (ls-compat.posix:file-information-kind
                              (ls-compat.posix:file-information path)) ':file)
                   (parent-cli--reject ':invalid-image))
                 path))
             images)))
      (user-message-input-create
       :text            (format nil "~{~A~^ ~}" (loop for number from 1 to (length paths)
                                                     collect (terminal-ui--image-label number)))
       :image-pathnames paths))))

(serapeum/types:-> parent-application-start
    (parent-configuration &key (:request (or null parent-cli-request)) (:input stream) (:output stream)) keyword)
(defun parent-application-start (configuration &key request (input *standard-input*) (output *standard-output*))
  "Run the current fullscreen parent editor over a mandatory native-isolated child."
  (parent-application-prepare-configuration configuration)
  (let* ((settings         (parent-configuration-get configuration ':settings))
         (identifier       (and request (parent-cli-option request ':session-id)))
         (simulate-crash-p (and request (parent-cli-option request ':simulate-crash)))
         (initial-input    (parent-interface--initial-image-input
                            configuration (and request (parent-cli-option request ':images))))
         (frontend nil) (controller nil)
         (notifications nil) (notification-lock (bordeaux-threads:make-lock "parent completion notices"))
         (application
           (make-parent-application
            configuration :session-id identifier
            :input-source             (parent-terminal-stream-input-source input)
            :renderer                 (lambda (region kind payload)
                                        (when frontend
                                          (parent-terminal-ui-render frontend :region region :kind kind :payload payload)))
            :event-callback
            (lambda (kind fields)
              (when (eq kind ':ui)
                (multiple-value-bind (event payload) (parent-interface-decode-ui fields)
                  (parent-terminal-ui-event frontend event payload))))
            :notification-callback
            (lambda (kind identifier)
              (declare (ignore identifier))
              (when (eq kind ':ui-completion-ready)
                (bordeaux-threads:with-lock-held (notification-lock)
                  (setf notifications t)))))))
    (terminal-theme-install (if (or (eq (getf settings :terminal-theme) ':almighty)
                                   (equal (getf settings :terminal-theme) "almighty")) ':almighty ':autolith))
    (unwind-protect
         (progn
           (setf frontend
                 (make-parent-terminal-ui
                  :input input :output output :fullscreen-p (getf settings :fullscreen-p t)
                  :history             (parent-application-input-history application)
                  :completion-root     (pathname (parent-configuration-get configuration ':workspace))
                  :completion-function (lambda () (and controller (parent-interface-completions controller)))))
           (when initial-input
             (terminal-ui--set-draft-input (parent-terminal-ui-editor-ui frontend) initial-input))
           (setf controller (make-instance 'parent-interface-controller :application application :frontend frontend)
                 (parent-application-exclusive-input application)
                 (lambda (function) (parent-terminal-ui-call-with-exclusive-input frontend function))
                 (parent-application-modal-reader application)
                 (lambda (&key timeout cancelled)
                   (parent-terminal-ui-read frontend "Approval" :timeout timeout :cancelled cancelled))
                 (parent-application-operation-handlers application)
                 (append (parent-cli-operation-handlers application :frontend frontend)
                         (parent-provider-operation-handlers application :frontend frontend)
                         (parent-session-operation-handlers application :frontend frontend)
                         (list (list ':restart (parent-interface-restart-handler application frontend))))
                 (parent-application-recovery-handler application)
                 (lambda (owner condition) (parent-application-recover owner frontend condition))
                 (parent-application-input-runner application)
                 (lambda (owner launch)
                   (bordeaux-threads:with-lock-held (notification-lock) (setf notifications nil))
                   (parent-application-configure owner launch)
                  (when simulate-crash-p
                    (setf simulate-crash-p nil)
                    (error 'parent-application-operation-error :code ':simulated-crash))
                  (setf (parent-interface-eof-p controller) nil
                        (parent-interface-quit-p controller) nil)
                  (parent-application-run-interface
                   owner frontend :controller controller :launch launch
                   :notification-function
                   (lambda ()
                     (bordeaux-threads:with-lock-held (notification-lock)
                       (prog1 notifications (setf notifications nil)))))))
           (parent-application-run application))
      (when frontend (parent-terminal-ui-close frontend)))))
