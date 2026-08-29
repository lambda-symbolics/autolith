(in-package #:autolith)

;;;; -- Localgroup Handoff Boundary Tests --

(-> test-localgroup-handoff-cancellation () null)
(defun test-localgroup-handoff-cancellation ()
  "Test that timeout cancellation defeats delayed startup and reaps its process."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (session nil)
         (pathname nil)
         (process nil)
         (startup-thread nil)
         (startup-cancelled-p nil)
         (claimed-pathname nil)
         (claim-thread nil)
         (claim-lock (make-lock "Autolith claimed handoff race"))
         (claim-entered-p nil)
         (release-claim-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application controller relay conversation)
               (test-localgroup--nemo-relay-application configuration)
             (declare (ignore controller relay conversation))
             (setf application new-application
                   session (localgroup-start new-application)
                   pathname
                   (localgroup-handoff--write
                    new-application session ':detach)))
           (let ((record (localgroup-handoff--read configuration pathname)))
             (setf process
                   (localgroup-handoff--launch-supervised
                    :arguments '("sleep" "30")
                    :handoff-pathname pathname
                    :directory (configuration-working-directory configuration)
                    :output (make-broadcast-stream))
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
                   (not (uiop:process-alive-p process))
                   (probe-file
                    (localgroup-handoff--cancelled-pathname pathname)))
              "a delayed replacement cannot reclaim an invalidated handoff"))
           (setf claimed-pathname
                 (localgroup-handoff--write application session ':detach))
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
                     (localgroup-handoff--claimed-pathname claimed-pathname)))
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
      (when (and process (ignore-errors (uiop:process-alive-p process)))
        (ignore-errors (uiop:terminate-process process)))
      (when pathname
        (localgroup-handoff--delete-state-pathnames pathname))
      (when claimed-pathname
        (localgroup-handoff--delete-state-pathnames claimed-pathname))
      (when application
        (localgroup-stop application)
        (application-release-conversation-lease application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-fresh-startup-selection () null)
(defun test-localgroup-fresh-startup-selection ()
  "Test that fresh handoff startup never reconnects a retained application."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (created-p nil)
         (reconnected-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (multiple-value-bind (new-application controller relay conversation)
               (test-localgroup--nemo-relay-application
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
                 :configuration configuration
                 :conversation-id nil
                 :immutable-p nil
                 :permission-mode ':ask
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
                 :configuration configuration
                 :conversation-id "0123456789ABCDEF"
                 :immutable-p nil
                 :permission-mode ':ask
                 :fresh-conversation-p nil)))
             (test-assert
              (and reconnected-p (not created-p))
              "durable handoff startup reconnects its explicit conversation")))
      (when application
        (application-release-conversation-lease application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)
