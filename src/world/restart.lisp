(in-package #:autolith)

;;;; -- World Server Restart Checkpoints --

;;; Where the host cannot fork, as on Windows, a checkpoint saves the world
;;; server's exact heap and exits. The launcher publishes the saved core and
;;; boots it, and the booted heap serves the paused channels again. While the
;;; process is gone the world's lease is free, so a marker naming the launcher
;;; tells clients the server is restarting and keeps them from starting a
;;; second one.

(defparameter *world-server-restart-mark-seconds* 600
  "How long a restart marker keeps clients waiting, even while its launcher lives.")

(defvar *world-server-restart* nil
  "The restart a world server recorded in its heap before saving it, or NIL.

A plist of :SERVER, the requesting :APPLICATION, the :RECONNECT-ID its clients
wait for, the :GENERATION-ID it saved, and :LEASES, the (HOST . IDENTIFIER)
conversation leases it released for the save.")


;;;; -- Restart Marker --

(-> world-server-restart-pathname (configuration) pathname)
(defun world-server-restart-pathname (configuration)
  "Return the marker saying CONFIGURATION's world server restarts from its saved heap."
  (merge-pathnames "restart.sexp" (world-endpoint-directory configuration)))

(-> world-server-restarting-p (configuration) boolean)
(defun world-server-restarting-p (configuration)
  "Return true while CONFIGURATION's world server restarts from its saved heap.

The marker names the launcher booting the saved core. A marker whose launcher
ended, or older than *WORLD-SERVER-RESTART-MARK-SECONDS*, is stale. Only the
world lease owner removes stale markers."
  (let* ((pathname (world-server-restart-pathname configuration))
         (record   (world-server--restart-mark-read pathname)))
    (and record
         (< (- (get-universal-time) (getf record :created-at))
            *world-server-restart-mark-seconds*)
         (not (null (platform-process-alive-p *platform* (getf record :launcher-pid)))))))

(-> world-server--restart-mark-read (pathname) list)
(defun world-server--restart-mark-read (pathname)
  "Return the restart marker identity and creation time at PATHNAME, or NIL when unreadable."
  (handler-case
      (let ((form (snapshot-read pathname)))
        (when (and (consp form) (eq (first form) ':autolith-world-restart))
          (let ((launcher-pid  (getf (rest form) :launcher-pid))
                (generation-id (getf (rest form) :generation-id))
                (created-at    (getf (rest form) :created-at)))
            (when (and (typep launcher-pid '(integer 1)) (integerp created-at)
                       (non-empty-string-p generation-id))
              (list :launcher-pid launcher-pid :generation-id generation-id
                    :created-at created-at)))))
    (error ()
      nil)))

(-> world-server--restart-mark (configuration &key (:launcher-pid (integer 1)) (:generation-id string)) null)
(defun world-server--restart-mark (configuration &key launcher-pid generation-id)
  "Mark CONFIGURATION's world server as restarting into GENERATION-ID under the launcher LAUNCHER-PID."
  (snapshot-write (world-server-restart-pathname configuration)
                  (list :autolith-world-restart
                        :version       1
                        :launcher-pid  launcher-pid
                        :created-at    (get-universal-time)
                        :generation-id generation-id))
  nil)

(-> world-server--restart-unmark (configuration &key (:restart list)) null)
(defun world-server--restart-unmark (configuration &key restart)
  "Remove CONFIGURATION's restart marker while holding its world lease.

When RESTART is supplied, remove only the marker with its generation and launcher."
  (let* ((pathname (world-server-restart-pathname configuration))
         (record   (and restart (world-server--restart-mark-read pathname))))
    (when (and (probe-file pathname)
               (or (null restart)
                   (and (equal (getf record :generation-id) (getf restart :generation-id))
                        (eql (getf record :launcher-pid) (getf restart :launcher-pid)))))
      (delete-file pathname)))
  nil)

(-> world-server--launcher-pid () (integer 1))
(defun world-server--launcher-pid ()
  "Return the process ID of the launcher that boots this process's saved heap, or signal that none does."
  (let ((pid (parse-integer (or (uiop:getenv "AUTOLITH_LAUNCHER_PID") "") :junk-allowed t)))
    (unless (and (typep pid '(integer 1))
                 (non-empty-string-p (uiop:getenv "AUTOLITH_RESTART_POINTER")))
      (error 'checkpoint-error
             :message  "The world server cannot restart from a saved heap: no stable launcher supervises it."
             :stage    ':validation
             :pathname nil))
    pid))


;;;; -- Saving --

(-> world-server--restart-job
    (world-server &key (:application application) (:backend t) (:reconnect-id string))
    null)
(defun world-server--restart-job (server &key application backend reconnect-id)
  "Save SERVER's exact heap for the checkpoint APPLICATION asked for, and exit for the launcher to boot it.

The generation is reserved while the channels still serve. Then every channel
pauses, its tools close and its Lisp workers stop. After final validation the
restart marker is published, leases are released, and the heap is saved.
A propagated failure restores ownership before the channels serve again.
The saver exits on a fatal save failure, leaving recovery to the launcher."
  (let ((configuration (world-server-configuration server))
        (failure       nil))
    (flet ((notice (served)
             "Return what channel SERVED is told about the failure."
             (application-failure-line
              (format nil "~:[world server checkpoint~;checkpoint~] failed: ~A"
                      (eq served application)
                      (bounded-string (princ-to-string failure) :limit 400)))))
      (handler-case
          (let* ((launcher-pid  (world-server--launcher-pid))
                 (generation    (checkpoint-create backend))
                 (generation-id (generation-identifier generation)))
            (world-server--call-with-quiescer
             server
             :application     application
             :reconnect-id    reconnect-id
             :notice-function #'notice
             :cancel-function (lambda ()
                                (setf failure (world-server--cancelled-condition)))
             :function
             (lambda ()
               (handler-case
                   (funcall *checkpoint-thread-quiescer*
                            (lambda ()
                              (world-server--restart-save
                               server
                               :application application
                               :backend     backend
                               :generation  generation
                               :restart     (list :server        server
                                                  :application   application
                                                  :reconnect-id  reconnect-id
                                                  :generation-id generation-id
                                                  :launcher-pid  launcher-pid)
                               :ready-function
                               (lambda (saved)
                                 (world-server--restart-mark
                                  configuration
                                  :launcher-pid  launcher-pid
                                  :generation-id (generation-identifier saved))))))
                 (serious-condition (condition)
                   (setf failure condition))))))
        (serious-condition (condition)
          (setf failure condition)
          (application-present application (notice application))))))
  nil)

(-> world-server--restart-save
    (world-server &key (:application application) (:backend t) (:generation t)
                       (:restart list) (:ready-function function))
    null)
(defun world-server--restart-save (server &key application backend generation restart ready-function)
  "Release SERVER's process resources and save its exact heap with RESTART recorded in it.

Run with every channel paused. APPLICATION's tools close here, since its
checkpoint backend would close them where nothing restarts them after the
boot. Telemetry detaches, and no application is active while the heap is
saved, so the saver strips no channel the booted heap serves again.
READY-FUNCTION receives the final generation while the world lease is held.
Only after it publishes the marker are leases released and the restart recorded.
A propagated pre-save failure reclaims released leases before removing this
restart's marker. A recovery failure stops the server instead of serving it
without ownership."
  (when-let ((requester (find application (world-server--hosts server) :key #'world-host-application)))
    (world-server--quiesce-host-tools requester))
  (dolist (host (world-server--hosts server))
    (lisp-worker-manager-stop (application-worker (world-host-application host))))
  (let ((leases      nil)
        (ready-p     nil)
        (recovered-p nil))
    (flet ((recover (condition)
             "Restore ownership once before a save failure is handled or propagated."
             (declare (ignore condition))
             (unless recovered-p
               (setf recovered-p t
                     *world-server-restart* nil
                     *active-application* application)
               (handler-case
                   (progn
                     (when (world-server--restart-reclaim-leases server leases :restart restart)
                       (error 'world-error
                              :message   "A checkpoint could not reclaim every conversation."
                              :pathname  (world-server-lock-pathname (world-server-configuration server))
                              :operation ':restart))
                     (when ready-p
                       (world-server--restart-unmark (world-server-configuration server)
                                                    :restart restart))
                     (world-server--attach-telemetry server))
                (serious-condition (recovery)
                  (with-lock-held ((world-server-lock server))
                    (setf (world-server-stopping-p server) t))
                  (handler-case
                      (world-server-stop server :reason "The checkpoint could not recover its world ownership.")
                    (serious-condition (stopping)
                      (warn 'world-session-warning
                            :pathname (world-server-lock-pathname (world-server-configuration server))
                            :cause    stopping)))
                  (warn 'world-session-warning
                        :pathname (world-server-lock-pathname (world-server-configuration server))
                        :cause    recovery))))))
      (handler-bind ((serious-condition #'recover))
        ;; The world quiescer already holds the channels; the backend must not
        ;; pause them again or strip the requesting application from the heap.
        (let ((*checkpoint-thread-quiescer* nil))
          (setf *active-application* nil)
          (checkpoint-restart-save
           backend generation
           :ready-function
           (lambda (saved)
             ;; Recover callback failures before the backend's fatal-save handler.
             (handler-bind ((serious-condition #'recover))
               (setf ready-p t)
               (funcall ready-function saved)
               (world-server--restart-release-leases
                server :released-function (lambda (released) (push released leases)))
               (setf *world-server-restart*
                     (append restart (list :leases (reverse leases))))
               (telemetry-detach))))))))
  nil)

(-> world-server--attach-telemetry (world-server) null)
(defun world-server--attach-telemetry (server)
  "Attach telemetry to the configuration of every channel SERVER hosts."
  (dolist (host (world-server--hosts server))
    (let ((configuration (application-configuration (world-host-application host))))
      (telemetry--call-safely (lambda () (telemetry-attach configuration)))))
  nil)

(-> world-server--restart-release-leases
    (world-server &key (:released-function (option function))) list)
(defun world-server--restart-release-leases (server &key released-function)
  "Release SERVER's conversation leases, then its world lease, returning released pairs.

Call RELEASED-FUNCTION with each (HOST . IDENTIFIER) before releasing it so a
partial failure can recover the leases whose application slot was cleared."
  (let ((released nil))
    (dolist (host (world-server--hosts server))
      (let* ((application (world-host-application host))
             (lease       (application-conversation-lease application)))
        (when lease
          (let ((pair (cons host (conversation-lease-identifier lease))))
            (push pair released)
            (when released-function (funcall released-function pair))
            (application-release-conversation-lease application)))))
    (when-let ((lease (world-server-lease server)))
      (lease-release lease)
      (setf (world-server-lease server) nil))
    (nreverse released)))

(-> world-server--restart-reclaim-leases (world-server list &key (:restart list)) list)
(defun world-server--restart-reclaim-leases (server leases &key restart)
  "Reclaim missing world and conversation LEASES for RESTART, returning conflicted hosts.

Already-owned leases are kept, including those not released before a partial
failure. The world lease must be acquired before any conversation is reclaimed."
  (unless (and (world-server-lease server)
               (ls-flock:lease-held-p (world-server-lease server)))
    (setf (world-server-lease server)
          (world-server--lease-acquire (world-server-configuration server) :restart restart)))
  (loop for (host . identifier) in leases
        for application = (world-host-application host)
        unless (or (let ((lease (application-conversation-lease application)))
                     (and lease (conversation-lease-held-p lease)))
                   (handler-case
                       (progn
                         (setf (application-conversation-lease application)
                               (conversation-lease-acquire (application-configuration application)
                                                           identifier))
                         t)
                     (conversation-error ()
                       nil)))
        collect host))


;;;; -- Resuming --

(-> world-server-restart-claim () list)
(defun world-server-restart-claim ()
  "Return the restart this heap recorded when its launcher boots it for that restart, and forget it either way.

The launcher's restart envelope names the generation it booted. Any other
boot of the core, such as a rollback, finds no matching envelope; the
channel that asked for the checkpoint becomes the active application, so
the boot resumes its conversation as it would after a forked checkpoint."
  (let ((restart (shiftf *world-server-restart* nil))
        (pointer (uiop:getenv "AUTOLITH_RESTART_POINTER")))
    (when restart
      (let ((envelope (and (non-empty-string-p pointer)
                           (probe-file pointer)
                           (handler-case (snapshot-read (pathname pointer))
                             (error ()
                               nil)))))
        (if (and (consp envelope)
                 (eq (first envelope) ':autolith-restart)
                 (equal (getf (rest envelope) :identifier)
                        (getf restart :generation-id)))
            restart
            (progn
              (setf *active-application* (getf restart :application))
              nil))))))

(-> world-server-restart-resume (list) integer)
(defun world-server-restart-resume (restart)
  "Serve the world server RESTART recorded again, in the heap booted for it, and return the process exit status.

The server takes back the world's lease and its channels' conversation
leases, publishes its endpoint under the token its clients hold, starts the
process services, removes the restart marker, and serves every paused
channel again under the reconnect ID its clients wait for. A channel whose
conversation another process took meanwhile is dropped."
  (destructuring-bind (&key server application reconnect-id generation-id launcher-pid leases) restart
    (declare (ignore launcher-pid))
    (let ((configuration (world-server-configuration server)))
      (handler-case
          (dolist (host (world-server--restart-reclaim-leases server leases :restart restart))
            (format *error-output* "~&Channel ~A did not resume: its conversation is open elsewhere.~%"
                    (conversation-identifier-display (world-host-channel host)))
            (world-server--release-host server host))
        (error (condition)
          (format *error-output* "~&Autolith could not resume the world server: ~A~%" condition)
          (return-from world-server-restart-resume 64)))
      (setf *active-application* nil)
      (unwind-protect
           (progn
             (world-server--runtime-start server)
             (management-repl-start server)
             (slynk-start configuration :background-p t)
             (world-server--attach-telemetry server)
             (world-server--restart-unmark configuration :restart restart)
             (world-server--resume-channels
              server application reconnect-id
              :notice-function
              (lambda (served)
                (application-notice-line
                 (format nil "~:[world server restarted from checkpoint ~A~;checkpoint ~A published, world server restarted from it~]"
                         (eq served application) generation-id))))
             (or (world-server-serve server) 0))
        (world-server--shutdown server)))))
