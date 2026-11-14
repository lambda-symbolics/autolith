(in-package #:autolith)

(-> test-localgroup--write-startup-ticket
    (application localgroup-session keyword)
    pathname)
(defun test-localgroup--write-startup-ticket (application session mode)
  "Publish a startup ticket for cancellation boundary tests."
  (let ((conversation (application-conversation application)))
    (unless (conversation-persisted-p conversation)
      (conversation-append-record conversation (list :localgroup-handoff :mode mode))))
  (let* ((configuration (application-configuration application))
         (conversation  (application-conversation application))
         (pathname
          (localgroup-handoff--pathname configuration
                                        (image-daemon:daemon-runtime-identifier
                                         session)))
         (draft         (line-editor-text (terminal-ui-editor (application-ui application))))
         (record
          (list :localgroup-handoff :version *localgroup-handoff-version* :session-id
                (image-daemon:daemon-runtime-identifier session) :token
                (image-daemon:daemon-runtime-token session) :created-at
                (image-daemon:daemon-runtime-created-at session) :mode mode :state
                ':pending :fresh-conversation-p nil :old-pid (current-process-id)
                :replacement-pid nil :conversation-id
                (conversation-identifier conversation) :draft draft)))
    (localgroup-handoff--write-record pathname record)
    pathname))

;;;; -- Localgroup Handoff Boundary Tests --

(-> test-localgroup-handoff-cancellation () null)
(defun test-localgroup-handoff-cancellation ()
  "Test that timeout cancellation defeats delayed startup and reaps its process."
  (with-platform-capability (':detached-sessions
                             "handoff cancellation against a supervised process")
    (test-localgroup--handoff-cancellation))
  nil)

(-> test-localgroup--handoff-cancellation () null)
(defun test-localgroup--handoff-cancellation ()
  "Race a supervised replacement against handoff timeout and claim cancellation."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (application         nil)
         (session             nil)
         (pathname            nil)
         (process             nil)
         (startup-thread      nil)
         (startup-cancelled-p nil)
         (claimed-pathname    nil)
         (claim-thread        nil)
         (claim-lock          (make-lock "Autolith claimed handoff race"))
         (claim-entered-p     nil)
         (release-claim-p     nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application controller relay conversation)
               (test-localgroup--relay-application configuration)
             (declare (ignore controller relay conversation))
             (setf application new-application
                   session (localgroup-start new-application)
                   pathname
                   (test-localgroup--write-startup-ticket
                    new-application session ':detach)))
           (let ((record (localgroup-handoff--read configuration pathname)))
             (setf process
                   (localgroup-handoff--launch-supervised
                     :arguments       (list (namestring sb-ext:*runtime-pathname*)
                                            "--noinform" "--non-interactive"
                                            "--eval" "(sleep 30)")
                    :handoff-pathname pathname
                    :directory        (config :working-directory configuration)
                    :output           (make-broadcast-stream))
                   startup-thread
                   (make-thread
                    (lambda ()
                      (sleep 0.1)
                      (let ((*localgroup-handoff-setsid-function* (lambda () 0)))
                        (setf startup-cancelled-p
                              (handler-case
                                  (progn
                                    (localgroup-handoff-begin-startup record)
                                    nil)
                                (localgroup-error () t)))))
                    :name "Autolith delayed handoff startup"))
             (test-assert
              (localgroup-handoff--stop-replacement process pathname)
              "timeout cancellation positively reaps the launched process")
             (join-thread startup-thread)
             (test-assert
              (and startup-cancelled-p
                   (not (platform-process-object-alive-p *platform* process))
                   (probe-file
                    (image-daemon:handoff-ticket-sibling pathname ':cancelled)))
              "a delayed replacement cannot reclaim an invalidated handoff"))
           (setf claimed-pathname
                 (test-localgroup--write-startup-ticket application session ':detach))
           (let ((record
                   (localgroup-handoff--read
                    configuration claimed-pathname)))
             (setf claim-thread
                   (make-thread
                    (lambda ()
                      (let ((*localgroup-handoff-setsid-function*
                              (lambda ()
                                (with-lock-held (claim-lock)
                                  (setf claim-entered-p t))
                                (loop
                                  (when (with-lock-held (claim-lock)
                                          release-claim-p)
                                    (return))
                                  (sleep 0.01))
                                0)))
                        (localgroup-handoff-begin-startup record)))
                    :name "Autolith claimed handoff race"))
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (with-lock-held (claim-lock) claim-entered-p))
               2)
              "replacement reaches the post-claim startup boundary")
             (localgroup-handoff--cancel claimed-pathname)
             (with-lock-held (claim-lock)
               (setf release-claim-p t))
             (join-thread claim-thread)
             (test-assert
              (and
               (not (probe-file
                     (image-daemon:handoff-ticket-sibling claimed-pathname ':claimed)))
               (let ((*localgroup-startup-record* record))
                 (handler-case
                     (progn
                       (localgroup-handoff-assert-startup-active)
                       nil)
                   (localgroup-error () t))))
              "cancellation after atomic claim cannot be overwritten by PID acknowledgement")))
      (when (and startup-thread (thread-alive-p startup-thread))
        (ignore-errors (join-thread startup-thread)))
      (when (and claim-thread (thread-alive-p claim-thread))
        (with-lock-held (claim-lock)
          (setf release-claim-p t))
        (ignore-errors (join-thread claim-thread)))
      (when (and process (platform-process-object-alive-p *platform* process))
        (platform-terminate-process-object *platform* process :force t)
        (platform-wait-process *platform* process))
      (when pathname
        (image-daemon:handoff-ticket-delete pathname))
      (when claimed-pathname
        (image-daemon:handoff-ticket-delete claimed-pathname))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-fresh-startup-selection () null)
(defun test-localgroup-fresh-startup-selection ()
  "Test that fresh handoff startup never reconnects a retained application."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   nil)
         (created-p     nil)
         (reconnected-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application controller relay conversation)
               (test-localgroup--relay-application
                configuration :persisted-p t)
             (declare (ignore controller relay conversation))
             (setf application new-application))
           (let ((*active-application* application))
             (test-call-with-function-replacements
              (list
               (list 'application-create
                     (lambda (active-configuration &key conversation-id
                                                   permission-mode)
                       (declare (ignore active-configuration conversation-id
                                        permission-mode))
                       (setf created-p t)
                       application))
               (list 'application-reconnect
                     (lambda (active-application &key conversation-id immutable-p
                                                     permission-mode)
                       (declare (ignore active-application conversation-id
                                        immutable-p permission-mode))
                       (setf reconnected-p t)
                       application)))
              (lambda ()
                (main--connect-application
                 :configuration        configuration
                 :conversation-id      nil
                 :immutable-p          nil
                 :permission-mode      ':ask
                 :fresh-conversation-p t)))
             (test-assert
              (and created-p (not reconnected-p))
              "empty handoff startup creates a fresh conversation instead of resuming retained state")
             (setf created-p nil
                   reconnected-p nil)
             (test-call-with-function-replacements
              (list
               (list 'application-create
                     (lambda (active-configuration &key conversation-id
                                                   permission-mode)
                       (declare (ignore active-configuration conversation-id
                                        permission-mode))
                       (setf created-p t)
                       application))
               (list 'application-reconnect
                     (lambda (active-application &key conversation-id immutable-p
                                                     permission-mode)
                       (declare (ignore active-application conversation-id
                                        immutable-p permission-mode))
                       (setf reconnected-p t)
                       application)))
              (lambda ()
                (main--connect-application
                 :configuration        configuration
                 :conversation-id      "0123456789ABCDEF"
                 :immutable-p          nil
                 :permission-mode      ':ask
                 :fresh-conversation-p nil)))
             (test-assert
              (and reconnected-p (not created-p))
              "durable handoff startup reconnects its explicit conversation")))
      (when application
        (application-release-conversation-lease application))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)
