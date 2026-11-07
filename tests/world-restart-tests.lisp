(in-package #:autolith)

;;;; -- World Server Restart Tests --

(-> world-restart-tests--ended-process-id () (integer 1))
(defun world-restart-tests--ended-process-id ()
  "Return the process ID of a child that has already exited."
  (let ((process (uiop:launch-program (list (namestring sb-ext:*runtime-pathname*) "--version")
                                      :output nil :error-output nil)))
    (uiop:wait-process process)
    (uiop:process-info-pid process)))

(-> test-world-server-restart-marker () null)
(defun test-world-server-restart-marker ()
  "Test a restart marker keeps the world server running while its launcher lives, and is dropped once stale."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let ((pathname (world-server-restart-pathname configuration))
          (now      (get-universal-time))
          (ended    (world-restart-tests--ended-process-id)))
      (ensure-directories-exist pathname)
      (test-assert (not (world-server-running-p configuration))
                   "no world server runs without a lease or a restart marker")
      (loop for (form running-p description)
              in `(((:autolith-world-restart :version 1 :launcher-pid ,(current-process-id)
                     :created-at ,now :generation-id "g-test")
                    t "a marker whose launcher lives says the server is restarting")
                   ((:autolith-world-restart :version 1 :launcher-pid ,ended
                     :created-at ,now :generation-id "g-test")
                    nil "a marker whose launcher ended is stale")
                   ((:autolith-world-restart :version 1 :launcher-pid ,(current-process-id)
                     :created-at ,(- now 3600) :generation-id "g-test")
                    nil "a marker older than its bound is stale")
                   ((:something-else)
                    nil "an unreadable marker is stale"))
            do (snapshot-write pathname form)
               (test-assert (and (eq (world-server-running-p configuration) running-p)
                                 (eq (not (null (probe-file pathname))) running-p))
                            (format nil "~A, and ~:[is removed~;stays~]" description running-p))
               (world-server--restart-unmark configuration))))
  nil)

(-> world-restart-tests--submit (cons string &key (:until function)) list)
(defun world-restart-tests--submit (entry text &key until)
  "Submit TEXT to ENTRY's channel as a structured client and return its events, oldest first, once UNTIL accepts them or a minute passed."
  (let* ((lock       (make-lock "Autolith restart test events"))
         (events     nil)
         (connection (localgroup-client-connect
                      entry :name           "restart test"
                            :approves-p     nil
                            :event-function (lambda (event)
                                              (with-lock-held (lock)
                                                (push event events))))))
    (flet ((events ()
             "Return the events so far, oldest first."
             (with-lock-held (lock)
               (reverse events))))
      (unwind-protect
           (progn
             (localgroup-client-call connection ':submit :text text)
             (task-tests--wait-until (lambda () (funcall until (events))) 60)
             (events))
        (localgroup-client-close connection)))))

(-> world-restart-tests--checkpoint (configuration cons) list)
(defun world-restart-tests--checkpoint (configuration entry)
  "Ask ENTRY's channel for a checkpoint, and return the world and channel entries of the server that restarts from it, or two NILs, then the events the channel sent."
  (let* ((channel (getf (rest (rest entry)) :session-id))
         (old-pid (getf (rest (rest (world-client-entry configuration))) :pid))
         (events  (world-restart-tests--submit
                   entry "(checkpoint)"
                   :until (lambda (events) (find ':closed events :key #'first))))
         (world   nil)
         (resumed nil))
    (if (task-tests--wait-until
         (lambda ()
           (setf world   (world-client-entry configuration)
                 resumed (localgroup--find-record-or-nil configuration channel))
           (and world resumed
                (/= (getf (rest (rest world)) :pid) old-pid)
                (eql (getf (rest (rest resumed)) :pid) (getf (rest (rest world)) :pid))))
         180)
        (list world resumed events)
        (list nil nil events))))

(-> world-restart-tests--logs (configuration) string)
(defun world-restart-tests--logs (configuration)
  "Return the tails of CONFIGURATION's detached process logs, to explain a failed restart."
  (with-output-to-string (output)
    (dolist (pathname (directory (merge-pathnames "localgroup/logs/*.log"
                                                  (config :state-root configuration))))
      (let ((text (uiop:read-file-string pathname)))
        (format output "~&--- ~A~%~A~%" (file-namestring pathname)
                (subseq text (max 0 (- (length text) 4000))))))))

(-> world-restart-tests--conversation-held-p (configuration string) boolean)
(defun world-restart-tests--conversation-held-p (configuration identifier)
  "Return true when another process holds the lease on conversation IDENTIFIER."
  (handler-case
      (progn
        (conversation-lease-release (conversation-lease-acquire configuration identifier))
        nil)
    (conversation-in-use ()
      t)))

(-> world-restart-tests--start (configuration pathname) null)
(defun world-restart-tests--start (configuration root)
  "Start CONFIGURATION's world server under the Lisp launcher, which supervises restarts as on Windows, with a channel for ROOT.

The executable user init gives the server the Windows checkpoint behavior and
skips the repository check."
  (configuration-ensure-directories configuration)
  (with-open-file (stream (configuration-user-init-path configuration)
                          :direction ':output :if-exists ':supersede
                          :external-format ':utf-8)
    (write-string "(setf (config :onboarding-p) nil
      (config :telemetry-enabled-p) nil
      *localgroup-first-attach-timeout-seconds* 0.2)
(defmethod platform-supports-p :around (platform (capability (eql :restartable-image-saver)))
  (declare (ignore platform))
  t)
(defun checkpoint--source-snapshot (configuration &key allow-dirty-p)
  (declare (ignore allow-dirty-p))
  (source-revision (make-source-provenance (config :source-root configuration))))
(defun checkpoint--revalidate-source (configuration commit &key allow-dirty-p)
  (declare (ignore configuration commit allow-dirty-p))
  nil)" stream))
  (test-call-with-function-replacements
   (list (list 'platform-session-launch-command
               (lambda (platform source-root)
                 (declare (ignore platform))
                 (list (lisp-worker-sbcl-command) "--noinform"
                       "--no-userinit" "--no-sysinit" "--script"
                       (namestring (merge-pathnames "script/launcher.lisp" source-root))))))
   (lambda ()
     (world-client-open configuration :directory (namestring root))))
  nil)

(-> world-restart-tests--stop (configuration) null)
(defun world-restart-tests--stop (configuration)
  "Stop CONFIGURATION's world server, ending its process if it does not stop."
  (let ((world (world-client-entry configuration)))
    (when world
      (let ((pid (getf (rest (rest world)) :pid)))
        ; mallet:suppress no-ignore-errors -- cleanup a server which may have exited already
        (ignore-errors (localgroup-query-record world ':stop))
        (unless (task-tests--wait-until
                 (lambda () (not (platform-process-alive-p *platform* pid))) 10)
          (platform-terminate-process *platform* pid :force t)))))
  nil)

(-> test-world-server-restart-checkpoint () null)
(defun test-world-server-restart-checkpoint ()
  "Test a world server that cannot fork restarts from its saved heap under its launcher, twice, and serves its channel again."
  (with-platform-capability (':detached-sessions "a launcher-supervised world server restart")
    (with-test-configuration (unused root)
      (declare (ignore unused))
      (session-cli-tests--call-with-roots
       root
       (lambda ()
         (let ((configuration (configuration-create :defer-provider-validation-p t)))
           (unwind-protect
                (progn
                  (world-restart-tests--start configuration root)
                  (let* ((entry   (first (localgroup-endpoint-records configuration)))
                         (channel (getf (rest (rest entry)) :session-id))
                         (token   (getf (rest (rest entry)) :token)))
                    (destructuring-bind (world resumed events)
                        (world-restart-tests--checkpoint configuration entry)
                      (test-assert (and world resumed)
                                   (format nil "a new process serves the world and the channel after the checkpoint~%~S~%~A"
                                           events (world-restart-tests--logs configuration)))
                      (when (and world resumed)
                        (test-assert (equal (getf (rest (rest resumed)) :token) token)
                                     "the channel keeps the token its clients hold")
                        (test-assert (directory (merge-pathnames "generations/*/autolith.core"
                                                                 (config :data-root configuration)))
                                     "the launcher published the saved heap as a generation")
                        (test-assert (and (not (probe-file (world-server-restart-pathname configuration)))
                                          (world-server-running-p configuration)
                                          (world-restart-tests--conversation-held-p configuration channel))
                                     "the restarted server holds the world and the conversation, and removed its marker")
                        ;; The heap was saved holding locks of a thread that
                        ;; is gone; a second checkpoint needs them again.
                        (destructuring-bind (again again-resumed again-events)
                            (world-restart-tests--checkpoint configuration resumed)
                          (test-assert (and again again-resumed)
                                       (format nil "the restarted server checkpoints and restarts again~%~S~%~A"
                                               again-events (world-restart-tests--logs configuration)))
                          (when again-resumed
                            (let ((evaluated (world-restart-tests--submit
                                              again-resumed "(+ 40 2)"
                                              :until (lambda (events)
                                                       (find ':work-finished events :key #'first)))))
                              (test-assert (find-if (lambda (event)
                                                      (and (eq (first event) ':present)
                                                           (search "42" (getf (rest event) :text))))
                                                    evaluated)
                                           (format nil "the channel evaluates input after two restarts: ~S"
                                                   evaluated)))))))))
             (world-restart-tests--stop configuration)))))))
  nil)
