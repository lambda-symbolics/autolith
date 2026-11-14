(in-package #:autolith)

;;;; -- Channel Deletion Tests --

(-> channel-deletion-tests--call (function) null)
(defun channel-deletion-tests--call (function)
  "Call FUNCTION with a standalone application holding its real conversation lease."
  (multiple-value-bind (application root terminal tool)
      (application-operation-tests--application)
    (declare (ignore tool))
    (unwind-protect
         (let* ((configuration (application-configuration application))
                (conversation  (application-conversation application)))
           (conversation-append-user-message conversation "delete command fixture")
           (world-reconcile-sessions configuration)
           (setf (application-conversation-lease application)
                 (conversation-lease-acquire configuration (conversation-identifier conversation))
                 (terminal-interactive-p terminal) t)
           (funcall function application configuration))
      (application-release-conversation-lease application)
      (tool-registry-close-runtime-state (application-tool-registry application))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> channel-deletion-tests--choose ((option string) function) t)
(defun channel-deletion-tests--choose (choice function)
  "Run FUNCTION with the real approval boundary and a terminal choosing CHOICE."
  (test-call-with-function-replacements
   (list (list 'terminal-ui-select
               (lambda (ui &key status-kind title items resize-callback external-event-function on-event)
                 (declare (ignore ui status-kind title resize-callback external-event-function on-event))
                 (test-assert (equal (getf (first items) :name) "keep")
                              "the confirmation defaults to keeping the channel")
                 choice)))
   function))

(-> test-delete-channel-targets () null)
(defun test-delete-channel-targets ()
  "Test canonical invocation, unique prefixes, ambiguity, inbox protection and cancellation."
  (channel-deletion-tests--call
   (lambda (application configuration)
     (let* ((current         (application-conversation application))
            (identifier      (conversation-identifier current))
            (lease           (application-conversation-lease application))
            (original-delete (fdefinition 'cl:delete)))
       (world-tests--persisted-conversation configuration "delete-other-one" 1000)
       (world-tests--persisted-conversation configuration "delete-other-two" 1001)
       (world-reconcile-sessions configuration)
       (application-operation-install-bindings application)
       (test-assert (and (not (eq 'delete 'cl:delete))
                         (application-operation-find application 'delete)
                         (find "(delete" (application-operation-completion-entries application)
                               :key (lambda (item) (getf item :name)) :test #'equal))
                    "delete has a canonical project binding and completion")
       (dolist (choice '(nil "keep"))
         (channel-deletion-tests--choose
          choice
          (lambda () (application-operation-call application "delete")))
         (test-assert (and (eq current (application-conversation application))
                           (eq lease (application-conversation-lease application))
                           (conversation-lease-busy-p configuration identifier)
                           (world-channel-find (world-load configuration) identifier))
                      "canceling leaves the attachment, lease and channel untouched"))
       (test-assert
        (handler-case (progn (application-operation-call application "delete" "delete-other") nil)
          (world-channel-ambiguous () t))
        "an ambiguous prefix cannot reach confirmation or removal")
       (test-assert
        (handler-case (progn (application-operation-call application "delete" "not-a-channel") nil)
          (world-error () t))
        "an unknown prefix cannot remove a channel")
       (test-assert
        (handler-case (progn (application-operation-call application "delete" "inbox") nil)
          (configuration-error () t))
        "the inbox is protected before confirmation")
       (channel-deletion-tests--choose
        "remove"
        (lambda ()
          (test-assert (eq (application-lisp-evaluation-status
                            (application-lisp-evaluate "(delete \"delete-other-o\")" :application application))
                           ':ok)
                       "the canonical Lisp command accepts a unique prefix")))
       (let ((world (world-load configuration)))
         (test-assert (and (null (world-channel-find world "delete-other-one"))
                           (world-channel-find world "delete-other-two")
                           (eq current (application-conversation application))
                           (eq original-delete (fdefinition 'cl:delete)))
                      "only the selected other channel is removed and CL:DELETE is untouched")))))
  nil)

(-> test-delete-channel-standalone () null)
(defun test-delete-channel-standalone ()
  "Test actual standalone current-channel handoff, removal and durable cleanup failure."
  (dolist (cleanup-failure-p '(nil t))
    (channel-deletion-tests--call
     (lambda (application configuration)
       (let* ((identifier (conversation-identifier (application-conversation application)))
              (path       (conversation-pathname (application-conversation application)))
              (inbox      (channel-identifier (world-inbox-channel (world-load configuration)))))
         (channel-deletion-tests--choose
          "remove"
          (lambda ()
            (flet ((invoke ()
                     "Invoke deletion through the real local input lifecycle on success."
                     (if cleanup-failure-p
                         (application-operation-call application "delete")
                         (let ((controller (application-input-controller-create application :start-reader-p nil)))
                           (unwind-protect
                                (progn
                                  (application-input-controller-enqueue-work controller (list :lisp "(delete)"))
                                  (let ((work (application-input-controller--next-work controller)))
                                    (unwind-protect
                                         (application-input-controller--run-work controller work)
                                      (application-input-controller--finish-work controller))))
                             (application-input-controller-stop controller))))))
              (if cleanup-failure-p
                  (test-call-with-function-replacements
                   (list (list 'task-worktree-discard
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (error 'configuration-error :message "injected cleanup failure"))))
                   (lambda ()
                     (test-assert (handler-case (progn (invoke) nil)
                                    (channel-removal-pending () t))
                                  "a published deletion retains its real cleanup failure")))
                  (invoke)))))
         (test-assert (and (equal inbox (conversation-identifier (application-conversation application)))
                           (conversation-lease-busy-p configuration inbox)
                           (not (conversation-lease-busy-p configuration identifier))
                           (null (world-channel-find (world-load configuration) identifier)))
                      "the standalone application holds a usable inbox, not the deleted channel's lease")
         (when cleanup-failure-p
           (test-assert (uiop:directory-files (channel--removal-root configuration) "*.sexp")
                        "failed cleanup retains a durable intent")
           (channel-remove-recover configuration))
         (test-assert (and (null (conversation-storage-active-pathname path))
                           (null (uiop:directory-files (channel--removal-root configuration) "*.sexp")))
                      "history cleanup completes and its durable intent is retired")))))
  (dolist (ancestor-p '(nil t))
    (channel-deletion-tests--call
     (lambda (application configuration)
       (let* ((root  (conversation-identifier (application-conversation application)))
              (child (world-tests--child-conversation
                      configuration "delete-role-child" :root root :parent root
                      :name "helper" :role "task" :created-at 1001))
              (inbox (channel-identifier (world-inbox-channel (world-load configuration)))))
         (world-reconcile-sessions configuration)
         (setf (application-configuration application)
               (application--configuration-for-conversation configuration child))
         (application-install-conversation application child)
         (let ((lease (application-conversation-lease application))
               (roled (application-configuration application)))
           (test-assert
            (handler-case (progn (application-resume-conversation application inbox) nil)
              (application-role-change-error () t))
            "ordinary resume still refuses a child-to-inbox role change")
           (test-assert
            (handler-case
                (progn (application--resume-deletion-inbox application root) nil)
              (configuration-error () t))
            "the deletion handoff cannot install an ordinary roleless conversation")
           (test-call-with-function-replacements
            (list (list 'application--prepare-runtime-replacement
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (error 'configuration-error :message "injected inbox runtime failure"))))
            (lambda ()
              (test-assert
               (handler-case (progn (application--resume-deletion-inbox application inbox) nil)
                 (configuration-error () t))
               "a failed inbox runtime preparation aborts the handoff")))
           (test-assert (and (eq child (application-conversation application))
                             (eq lease (application-conversation-lease application))
                             (eq roled (application-configuration application))
                             (conversation-lease-busy-p configuration "delete-role-child")
                             (not (conversation-lease-busy-p configuration inbox)))
                        "failed handoffs retain the child runtime and release the inbox lease")
           (channel-deletion-tests--choose
            "remove"
            (lambda ()
              (test-assert
               (eq (application-lisp-evaluation-status
                    (application-lisp-evaluate
                     (if ancestor-p (format nil "(delete ~S)" root) "(delete)")
                     :application application))
                   ':ok)
               "a standalone child can delete itself or its displayed ancestor")))
           (test-assert
            (and (equal inbox (conversation-identifier (application-conversation application)))
                 (string= "" (config :child-role (application-configuration application)))
                 (string= "" (config :child-role-instructions (application-configuration application)))
                 (eq (agent-configuration (application-agent application))
                     (application-configuration application))
                 (conversation-lease-busy-p configuration inbox)
                 (not (conversation-lease-busy-p configuration "delete-role-child"))
                 (null (world-channel-find (world-load configuration) "delete-role-child"))
                 (if ancestor-p
                     (null (world-channel-find (world-load configuration) root))
                     (world-channel-find (world-load configuration) root)))
            "the roleless inbox has a fresh runtime and sole lease after child subtree removal"))))))
  nil)

(-> channel-deletion-tests--attachment (localgroup-terminal string) image-daemon:attachment)
(defun channel-deletion-tests--attachment (terminal identifier)
  "Attach a real controlling transport to TERMINAL for channel IDENTIFIER."
  (let ((attachment (make-instance 'image-daemon:attachment :socket nil
                                                          :stream   (make-broadcast-stream)
                                                          :mode     ':control)))
    (test-assert (image-daemon:relay-attach terminal attachment :rows 24 :columns 80
                                                             :styled-p nil :session-id identifier)
                 "the invoking client controls its original channel")
    attachment))

(-> test-delete-channel-hosted () null)
(defun test-delete-channel-hosted ()
  "Test hosted cancellation and an ancestor deletion after a real asynchronous terminal handoff."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (world-tests--persisted-conversation configuration "delete-root" 1000)
     (world-tests--child-conversation configuration "delete-child" :root "delete-root" :parent "delete-root"
                                                                :name "delete-helper" :role "general" :created-at 1001)
     (world-reconcile-sessions configuration)
     (world-server-open server (list :channel "delete-child" :switch-p t))
     (let* ((host        (world-server--find-host server "delete-child"))
            (application (world-host-application host))
            (terminal    (terminal-ui-terminal (application-ui application)))
            (attachment  (channel-deletion-tests--attachment terminal "delete-child"))
            (inbox       (channel-identifier (world-inbox-channel (world-load configuration)))))
       (unwind-protect
            (progn
              (channel-deletion-tests--choose
               nil
               (lambda ()
                 (application-clients-call-as-work
                  application
                  (lambda () (application-operation-call application "delete")))))
              (test-assert (and (eq (image-daemon:relay-controller terminal) attachment)
                                (world-channel-find (world-load configuration) "delete-child")
                                (null (world-server--find-host server inbox)))
                           "hosted cancellation neither opens an inbox nor changes the client's attachment")
              (channel-deletion-tests--choose
               "remove"
               (lambda ()
                 (application-clients-call-as-work
                  application
                  (lambda () (application-operation-call application "delete" "delete-root")))))
              (test-assert (and (world-channel-find (world-load configuration) "delete-child")
                                (eq (image-daemon:relay-controller terminal) attachment))
                           "sending a switch request does not delete a channel before the client leaves")
              (let ((packets (coerce (structlisp:deque->vector (image-daemon:attachment-queue attachment)) 'list)))
                (test-assert (some (lambda (packet) (and (search ":SWITCH" (string-upcase packet))
                                                         (search inbox packet))) packets)
                             "only the invoking control transport receives the inbox switch"))
              (image-daemon:relay-detach terminal attachment)
              (let* ((destination    (world-host-application (world-server--find-host server inbox)))
                     (inbox-terminal (terminal-ui-terminal (application-ui destination))))
                (test-assert (image-daemon:relay-attach inbox-terminal attachment :rows 24 :columns 80
                                                                                :styled-p nil :session-id inbox)
                             "the client can attach to its usable destination")
                (test-assert
                 (task-tests--wait-until
                  (lambda () (null (world-channel-find (world-load configuration) "delete-root"))) 30)
                 "destination work deletes the selected ancestor and its formerly displayed child")
                (test-assert (and (null (world-channel-find (world-load configuration) "delete-child"))
                                  (null (world-server--find-host server "delete-child"))
                                  (eq (image-daemon:relay-controller inbox-terminal) attachment)
                                  (not (application-input-controller-stopping-p
                                        (application-input-controller destination))))
                             "the old host releases its lease while the invoking client keeps a live inbox")
                (image-daemon:relay-detach inbox-terminal attachment)))
         (image-daemon:relay-detach terminal attachment)))))
  nil)

(-> test-delete-channel-hosted-refusals () null)
(defun test-delete-channel-hosted-refusals ()
  "Test handoff timeout, attached clients, running work and queued asynchronous execution."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (dolist (mode '(:timeout :observer :running :execution))
       (let ((identifier (format nil "delete-~(~A~)" mode)))
         (world-tests--persisted-conversation configuration identifier 1000)
         (world-reconcile-sessions configuration)
         (world-server-open server (list :channel identifier :switch-p t))
         (let* ((application (world-host-application (world-server--find-host server identifier)))
                (controller  (application-input-controller application))
                (terminal    (terminal-ui-terminal (application-ui application)))
                (attachment  (channel-deletion-tests--attachment terminal identifier))
                (observer    (when (eq mode ':observer)
                               (make-instance 'image-daemon:attachment :socket nil :stream (make-broadcast-stream)
                                                                      :mode ':read-only)))
                (presented   nil)
                (refusals    nil)
                (busy        (fdefinition 'channel--busy))
                (queued-job  nil)
                (external-p  nil))
           (when observer
             (image-daemon:relay-attach terminal observer :rows 24 :columns 80 :styled-p nil :session-id identifier))
           (unwind-protect
                (test-call-with-function-replacements
                 (list (list 'application-present
                             (lambda (candidate entry &key identifier)
                               (declare (ignore candidate identifier))
                               (when (and (consp entry) (terminal-span-p (first entry))
                                          (eq (terminal-span-style (first entry)) ':failure))
                                 (push entry presented))))
                       (list 'channel--busy
                             (lambda (channel tool-name &key (state ':leased))
                               (push (list channel state) refusals)
                               (funcall busy channel tool-name :state state))))
                 (lambda ()
                   (let ((inbox (channel-identifier (world-inbox-channel (world-load configuration)))))
                     (channel-deletion-tests--choose
                      "remove"
                      (lambda ()
                        (application-clients-call-as-work
                         application
                         (lambda () (application-operation-call application "delete")))))
                     (when (eq mode ':running)
                       (test-assert (task-tests--wait-until
                                     (lambda () (not (application-input-controller-busy-p controller))) 5)
                                    "the initiating command finishes before unrelated work starts")
                       (application-input-controller-begin-external-work controller)
                       (setf external-p t))
                     (when (eq mode ':execution)
                       (let ((orchestrator (application--task-orchestrator application)))
                         (setf queued-job
                               (job-pool-submit (task-orchestrator-execution-pool orchestrator)
                                                (lambda (job) (declare (ignore job)) nil)
                                                :inline-only-p t))
                         (test-assert (and (zerop (task-orchestrator-active-count orchestrator))
                                           (= 1 (task-orchestrator-session-live-count orchestrator)))
                                      "a queued execution is live work even without an active child")))
                     (unless (eq mode ':timeout)
                       (image-daemon:relay-detach terminal attachment))
                     (test-assert (task-tests--wait-until (lambda () presented) 15)
                                  "a failed removal is reported as a failure in the destination")
                     (test-assert (and (world-channel-find (world-load configuration) identifier)
                                       (world-host-live-p (world-server--find-host server inbox))
                                       (null (uiop:directory-files (channel--removal-root configuration) "*.sexp"))
                                       (or (null observer)
                                           (member observer (image-daemon:relay-observers terminal))))
                                  "a refusal preserves the channel, other clients, work and usable inbox")
                     (test-assert (if (eq mode ':timeout)
                                      (null refusals)
                                      (equal (first refusals)
                                             (list identifier (if observer ':attached ':running))))
                                  "timeout precedes removal; fresh removal validation refuses the actual busy state"))))
             (when queued-job
               (job-cancel queued-job)
               (job-await queued-job :timeout-seconds 5))
             (when external-p (application-input-controller-finish-external-work controller))
             (image-daemon:relay-detach terminal attachment)
             (when observer (image-daemon:relay-detach terminal observer))))))))
  nil)

(-> test-delete-channel-standalone-refusals () null)
(defun test-delete-channel-standalone-refusals ()
  "Test a standalone handoff does not retire a provider turn or queued input."
  (dolist (mode '(:active :queued))
    (channel-deletion-tests--call
     (lambda (application configuration)
       (let* ((conversation (application-conversation application))
              (lease        (application-conversation-lease application))
              (registry     (application-tool-registry application))
              (controller   (application-input-controller-create application :start-reader-p nil)))
         (unwind-protect
              (progn
                (if (eq mode ':active)
                    (application-input-controller-begin-external-work controller)
                    (application-input-controller-enqueue-work controller (list :message "pending input")))
                (channel-deletion-tests--choose
                 "remove"
                 (lambda ()
                   (test-assert (handler-case (progn (application-operation-call application "delete") nil)
                                  (task-error () t))
                                "unrelated active or pending work refuses replacement")))
                (test-assert (and (eq conversation (application-conversation application))
                                  (eq lease (application-conversation-lease application))
                                  (eq registry (application-tool-registry application))
                                  (world-channel-find (world-load configuration) (conversation-identifier conversation))
                                  (if (eq mode ':active)
                                      (application-input-controller-turn-active-p controller)
                                      (= 1 (deque-count (application-input-controller-work-items controller)))))
                             "a refused deletion leaves its conversation, lease, runtime and work intact"))
           (when (eq mode ':active)
             (application-input-controller-finish-external-work controller))
           (application-input-controller-stop controller))))))
  nil)
