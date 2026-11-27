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
  "Test restart liveness, lease-guarded admission, and checkpoint failure recovery."
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
                                 (probe-file pathname))
                            description)
               (unless running-p
                 (let ((lease (world-server--lease-acquire configuration)))
                   (unwind-protect
                        (test-assert (not (probe-file pathname))
                                     "the new world lease owner removes a stale marker")
                     (lease-release lease))))
               (world-server--restart-unmark configuration))))
  (world-restart-tests--handoff)
  (world-restart-tests--reader-replacement)
  (world-restart-tests--failed-recovery)
  nil)

(-> world-restart-tests--failed-recovery () null)
(defun world-restart-tests--failed-recovery ()
  "Run both real quiescer unwinds after ownership recovery fails."
  (dolist (stage '(:reclaim :unmark))
    (with-test-configuration (configuration)
      (configuration-ensure-directories configuration)
      (let* ((server                 (make-instance 'world-server :configuration configuration))
             (application            (make-instance 'application :configuration configuration :worker nil))
             (backend                (allocate-instance (find-class 'checkpoint-backend)))
             (restart                (list :server server :application application
                                           :generation-id "g-recovery" :launcher-pid (current-process-id)))
             (primary                (make-condition 'checkpoint-error :message "Save failed" :stage ':save :pathname nil))
             (secondary              (make-condition 'checkpoint-error :message "Recovery failed" :stage ':save :pathname nil))
             (reclaim                (symbol-function 'world-server--restart-reclaim-leases))
             (unmark                 (symbol-function 'world-server--restart-unmark))
             (*world-server-restart* nil)
             (*active-application*   nil)
             (events                 nil)
             (caught                 nil))
        (setf (world-server-lease server) (world-server--lease-acquire configuration))
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'world-server--runtime-stop (lambda (server) (declare (ignore server)) nil))
               (list 'management-repl-call-quiesced
                     (lambda (server thunk) (declare (ignore server)) (funcall thunk)))
               (list 'slynk-call-quiesced (lambda (thunk) (funcall thunk)))
               (list 'telemetry-detach (lambda () nil))
               (list 'world-server--runtime-start
                     (lambda (server &rest options) (declare (ignore server options)) (push :endpoint events)))
               (list 'world-server--resume-channels
                     (lambda (&rest arguments) (declare (ignore arguments)) (push :channels events)))
               (list 'world-server-stop
                     (lambda (server &rest options) (declare (ignore server options)) (error secondary)))
               (list 'world-server--restart-reclaim-leases
                     (lambda (server leases &key restart)
                       (when (eq stage ':reclaim) (error secondary))
                       (funcall reclaim server leases :restart restart)))
               (list 'world-server--restart-unmark
                     (lambda (configuration &key restart)
                       (when (eq stage ':unmark) (error secondary))
                       (funcall unmark configuration :restart restart)))
               (list 'checkpoint-restart-save
                     (lambda (backend generation &key ready-function)
                       (declare (ignore backend))
                       (funcall ready-function generation)
                       (error primary))))
              (lambda ()
                (handler-bind ((world-session-warning #'muffle-warning))
                  (handler-case
                      (world-server--call-with-quiescer
                       server :application application :reconnect-id "recovery-test"
                       :function
                       (lambda ()
                         (funcall *checkpoint-thread-quiescer*
                                  (lambda ()
                                    (world-server--restart-save
                                     server :application application :backend backend :generation nil
                                     :restart restart
                                     :ready-function
                                     (lambda (generation)
                                       (declare (ignore generation))
                                       (world-server--restart-mark configuration
                                        :launcher-pid (current-process-id) :generation-id "g-recovery")))))))
                    (checkpoint-error (condition) (setf caught condition))))
                (test-assert (and (eq caught primary)
                                  (world-server-stopping-p server)
                                  (null events)
                                  (probe-file (world-server-restart-pathname configuration)))
                             (format nil "~A recovery failure prevents endpoint/channel restart and preserves the save error" stage))))
          (when (world-server-lease server) (lease-release (world-server-lease server)))))))
  nil)

(-> world-restart-tests--reader-replacement () null)
(defun world-restart-tests--reader-replacement ()
  "Replace a stale marker after a poll reads it, before the poll returns."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((pathname (world-server-restart-pathname configuration))
           (read     (symbol-function 'world-server--restart-mark-read))
           (identity (list :launcher-pid (current-process-id) :generation-id "replacement")))
      (world-server--restart-mark configuration
       :launcher-pid (world-restart-tests--ended-process-id) :generation-id "stale")
      (test-call-with-function-replacements
       (list (list 'world-server--restart-mark-read
                   (lambda (pathname)
                     (prog1 (funcall read pathname)
                       (let ((lease (lease-acquire (world-server-lock-pathname configuration))))
                         (unwind-protect
                              (world-server--restart-mark configuration
                               :launcher-pid (current-process-id) :generation-id "replacement")
                           (lease-release lease)))))))
       (lambda ()
         (test-assert (not (world-server-restarting-p configuration))
                      "the in-flight poll reports its stale observation")))
      (test-assert (world-server-restarting-p configuration)
                   "a stale poll cannot remove the owner's replacement marker")
      (let ((lease (world-server--lease-acquire configuration :restart identity)))
        (unwind-protect
             (progn
               (world-server--restart-unmark configuration
                :restart (list :launcher-pid (current-process-id) :generation-id "stale"))
               (test-assert (probe-file pathname) "rollback cannot remove a different restart's marker")
               (world-server--restart-unmark configuration :restart identity)
               (test-assert (not (probe-file pathname)) "the matching lease owner removes its own marker"))
          (lease-release lease)))))
  nil)

(-> world-restart-tests--handoff () null)
(defun world-restart-tests--handoff ()
  "Exercise the ready boundary and partial rollback without saving or launching a process."
  (dolist (stage '(:validation :marker :release :detach :save))
    (with-test-configuration (configuration)
      (configuration-ensure-directories configuration)
      (let* ((application            (make-instance 'application :configuration configuration :worker nil))
             (host                   (make-instance 'world-host :application application))
             (server                 (make-instance 'world-server :configuration configuration))
             (backend                (allocate-instance (find-class 'checkpoint-backend)))
             (restart                (list :server server :application application
                                           :generation-id "g-handoff" :launcher-pid (current-process-id)))
             (failure                (make-condition 'checkpoint-error :message "Injected handoff failure"
                                                     :stage ':save :pathname nil))
             (release                (symbol-function 'application-release-conversation-lease))
             (release-lease          (symbol-function 'conversation-lease-release))
             (*world-server-restart* nil)
             (*active-application*   nil)
             (caught                 nil))
        (setf (world-server-hosts server) (list host)
              (world-server-lease server) (world-server--lease-acquire configuration)
              (application-conversation-lease application)
              (conversation-lease-acquire configuration "handoff-test"))
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'world-server--quiesce-host-tools (lambda (host) (declare (ignore host)) nil))
               (list 'lisp-worker-manager-stop (lambda (worker) (declare (ignore worker)) nil))
               (list 'world-server--attach-telemetry (lambda (server) (declare (ignore server)) nil))
               (list 'telemetry-detach (lambda () (when (eq stage ':detach) (error failure))))
               (list 'conversation-lease-release
                     (lambda (lease)
                       (funcall release-lease lease)
                       (when (eq stage ':release)
                         (test-assert (and (eq (application-conversation-lease application) lease)
                                           (not (conversation-lease-held-p lease)))
                                      "the injected failure occurs before the released lease slot is cleared")
                         (error failure))))
               (list 'checkpoint-restart-save
                     (lambda (backend generation &key ready-function)
                       (declare (ignore backend))
                       (test-assert (and (world-server-lease server)
                                         (application-conversation-lease application)
                                         (not (probe-file (world-server-restart-pathname configuration))))
                                    "validation runs before ownership is released or restart is marked")
                       (when (eq stage ':validation) (error failure))
                       ;; Catch inside the backend, as its fatal-save handler does.
                       (handler-case
                           (funcall ready-function generation)
                         (checkpoint-error (condition)
                           (test-assert (and (world-server-lease server)
                                           (ls-flock:lease-held-p (world-server-lease server))
                                           (application-conversation-lease application)
                                           (conversation-lease-held-p (application-conversation-lease application))
                                           (not (probe-file (world-server-restart-pathname configuration))))
                                        "callback recovery completes before the backend handles its failure")
                           (error condition)))
                       (test-assert (and (world-server-running-p configuration)
                                         (null (world-server-lease server))
                                         (null (application-conversation-lease application))
                                         (equal (getf *world-server-restart* :generation-id) "g-handoff")
                                         (= (length (getf *world-server-restart* :leases)) 1))
                                    "the marked restart records released conversation ownership before saving")
                       (dolist (identity (list nil
                                             (list :generation-id "other" :launcher-pid (current-process-id))
                                             (list :generation-id "g-handoff" :launcher-pid (1+ (current-process-id)))))
                         (let ((refused-p nil))
                           (handler-case
                               (lease-release (world-server--lease-acquire configuration :restart identity))
                             (world-error () (setf refused-p t)))
                           (test-assert refused-p "fresh or mismatched starts cannot acquire a restarting world")))
                       (world-server--restart-reclaim-leases
                        server (getf *world-server-restart* :leases) :restart restart)
                       (test-assert (and (world-server-lease server)
                                         (application-conversation-lease application)
                                         (probe-file (world-server-restart-pathname configuration)))
                                    "the matching restart reclaims ownership without deleting its marker")
                       (error failure))))
              (lambda ()
                (handler-case
                    (world-server--restart-save
                     server :application application :backend backend :generation nil
                     :restart restart
                     :ready-function
                     (lambda (generation)
                       (declare (ignore generation))
                       (test-assert (and (world-server-lease server)
                                         (application-conversation-lease application))
                                    "the final-generation callback still holds both leases")
                       (world-server--restart-mark configuration
                        :launcher-pid (current-process-id) :generation-id "g-handoff")
                       (test-assert (world-server-running-p configuration)
                                    "marker publication overlaps world lease ownership")
                       (when (eq stage ':marker) (error failure))))
                  (checkpoint-error (condition) (setf caught condition)))
                (test-assert (and (eq caught failure)
                                  (world-server-lease server)
                                  (ls-flock:lease-held-p (world-server-lease server))
                                  (application-conversation-lease application)
                                  (conversation-lease-held-p (application-conversation-lease application))
                                  (null *world-server-restart*)
                                  (not (probe-file (world-server-restart-pathname configuration))))
                             (format nil "~A failure preserves the original condition and recovers ownership" stage))))
          (funcall release application)
          (when (world-server-lease server) (lease-release (world-server-lease server)))
          (world-server--restart-unmark configuration)))))
  nil)

(-> world-restart-tests--submit (cons string &key (:until function)) list)
(defun world-restart-tests--submit (entry text &key until)
  "Submit TEXT to ENTRY's channel as a structured client and return its events, oldest first, once UNTIL accepts them or a minute passed."
  (let* ((lock       (make-lock "Autolith restart test events"))
         (events     nil)
         (connection (world-terminal-client-connect
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
             (world-terminal-client-call connection ':submit :text text)
             (task-tests--wait-until (lambda () (funcall until (events))) 60)
             (events))
        (world-terminal-client-close connection)))))

(-> world-restart-tests--record-state (pathname list) list)
(defun world-restart-tests--record-state (pathname keys)
  "Read only small stage records and return their whitelisted scalar fields."
  (if (probe-file pathname)
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (if (> (file-length stream) 16384)
            (list :file (file-namestring pathname) :status ':oversized)
            (let ((record (snapshot-read pathname)))
              (list :file (file-namestring pathname)
                    :fields
                    (loop for key in keys
                          for value = (getf (rest record) key)
                          when (or (integerp value)
                                   (and (stringp value) (<= (length value) 128)))
                            append (list key value))))))
      (list :file (file-namestring pathname) :status ':absent)))

(-> world-restart-tests--file-state (pathname) list)
(defun world-restart-tests--file-state (pathname)
  "Return a stage file's name and byte length without reading its contents."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (list :file (file-namestring pathname) :bytes (file-length stream))))

(-> world-restart-tests--diagnostics (configuration (integer 1) t) list)
(defun world-restart-tests--diagnostics (configuration old-pid process)
  "Collect bounded stage metadata without changing restart or lease state."
  (labels ((probe (function)
             "Keep an unavailable probe from hiding the checkpoint failure."
             (handler-case (funcall function)
               (error (condition)
                 (list :unavailable (type-of condition)))))

           (limited (pathnames)
             "Keep at most eight entries from a fixture-owned directory."
             (subseq pathnames 0 (min 8 (length pathnames)))))
    (list :original-pid       old-pid
          :original-alive-p   (probe (lambda () (platform-process-alive-p *platform* old-pid)))
          :owned-launch-pid   (probe (lambda () (when process
                                                 (platform-process-object-pid *platform* process))))
          :owned-tree-alive-p (probe (lambda () (when process
                                                 (platform-process-object-alive-p *platform* process))))
          :world-lease        (probe (lambda ()
                                       (multiple-value-list
                                          (ls-flock:file-lock-busy-p
                                           (world-server-lock-pathname configuration)))))
          :restart-marker
          (probe (lambda ()
                   (let* ((marker (world-restart-tests--record-state
                                   (world-server-restart-pathname configuration)
                                   '(:version :launcher-pid :created-at :generation-id)))
                          (pid    (getf (getf marker :fields) :launcher-pid)))
                     (append marker (list :launcher-alive-p
                                          (and (typep pid '(integer 1))
                                               (platform-process-alive-p *platform* pid)))))))
          :restart-envelopes
          (probe (lambda ()
                   (mapcar (lambda (pathname)
                             (world-restart-tests--record-state pathname '(:version :identifier :created-at)))
                           (limited (directory (merge-pathnames "restart-pointers/*.sexp"
                                                                (config :state-root configuration)))))))
          :generation-files
          (probe (lambda ()
                   (mapcar (lambda (directory)
                             (list :generation (first (last (pathname-directory directory)))
                                   :files      (mapcar #'world-restart-tests--file-state
                                                       (limited (uiop:directory-files directory)))))
                           (limited (uiop:subdirectories
                                     (merge-pathnames "generations/" (config :data-root configuration))))))))))

(-> world-restart-tests--checkpoint (configuration cons &key (:process t)) list)
(defun world-restart-tests--checkpoint (configuration entry &key process)
  "Return the resumed world and channel entries after marker removal, events, and timeout diagnostics when unavailable."
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
                 resumed (world-terminal--find-record-or-nil configuration channel))
           (and world resumed
                (/= (getf (rest (rest world)) :pid) old-pid)
                (eql (getf (rest (rest resumed)) :pid) (getf (rest (rest world)) :pid))
                ;; Endpoints precede process services and marker removal.
                (not (probe-file (world-server-restart-pathname configuration)))))
         180)
        (list world resumed events)
        (list nil nil events
              (world-restart-tests--diagnostics configuration old-pid process)))))

(-> world-restart-tests--logs (configuration) string)
(defun world-restart-tests--logs (configuration)
  "Return the tails of CONFIGURATION's detached process logs, to explain a failed restart."
  (with-output-to-string (output)
    (dolist (pathname (directory (merge-pathnames "logs/*.log"
                                                (world-endpoint-directory configuration))))
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
        (config :telemetry-enabled-p) nil)
(defmethod platform-supports-p :around (platform (capability (eql :restartable-image-saver)))
  (declare (ignore platform))
  t)
(defun checkpoint--source-snapshot (configuration &key allow-dirty-p)
  (declare (ignore allow-dirty-p))
  (source-revision (make-source-provenance (config :source-root configuration))))
(defun checkpoint--revalidate-source (configuration commit &key allow-dirty-p)
  (declare (ignore configuration commit allow-dirty-p))
  nil)" stream)
    (terpri stream)
    (write '(defparameter *world-restart-test-native-itimer*
              (let ((symbol (find-symbol "ITIMER-EMULATION-DEINIT" "SB-IMPL")))
                (when (and symbol (fboundp symbol)) (symbol-function symbol)))
              "The original timer teardown, retained to check each saved heap.")
           :stream stream :readably t)
    (terpri stream))
  (test-call-with-function-replacements
   (list (list 'platform-session-launch-command
               (lambda (platform source-root)
                 (declare (ignore platform))
                 (list (sbcl-command) "--noinform"
                       "--no-userinit" "--no-sysinit" "--script"
                       (namestring (merge-pathnames "script/launcher.lisp" source-root))))))
   (lambda ()
     (world-client-open configuration :directory (namestring root))))
  nil)

(define-condition world-restart-test-cleanup-error (error)
  ((failures :initarg       :failures
             :reader        world-restart-test-cleanup-failures
             :documentation "Cleanup conditions, in operation order."))
  (:documentation "One or more fixture cleanup operations failed after every launch was attempted.")
  (:report (lambda (condition stream)
             (format stream "Restart fixture cleanup encountered ~D failures:~{~%~A~}"
                     (length (world-restart-test-cleanup-failures condition))
                     (world-restart-test-cleanup-failures condition)))))

(-> world-restart-tests--stop (configuration list) null)
(defun world-restart-tests--stop (configuration processes)
  "Attempt cleanup of every retained launch before reporting accumulated failures."
  (let ((failures nil))
    (labels ((attempt (function)
               "Return FUNCTION's result and success flag, recording any error."
               (handler-case
                   (values (funcall function) t)
                 (error (condition)
                   (push condition failures)
                   (values nil nil)))))
      (attempt (lambda ()
                 (let ((world (world-client-entry configuration)))
                   (when world
                     ; mallet:suppress no-ignore-errors -- stopping an endpoint that may already be closed is advisory
                     (ignore-errors (world-terminal-query-record world ':stop))))))
      (dolist (process processes)
        (multiple-value-bind (alive-p known-p)
            (attempt (lambda () (platform-process-object-alive-p *platform* process)))
          (when (or alive-p (not known-p))
            (attempt (lambda () (platform-terminate-process-object *platform* process :force t)))))
        (multiple-value-bind (ended-p checked-p)
            (attempt (lambda ()
                       (task-tests--wait-until
                        (lambda () (not (platform-process-object-alive-p *platform* process))) 10)))
          (when checked-p
            (if ended-p
                (attempt (lambda () (platform-wait-process *platform* process)))
                (push (make-condition 'platform-error :operation ':terminate
                                      :message                   "The retained launcher tree did not end during cleanup.")
                      failures))))
        (attempt (lambda () (platform-release-process *platform* process)))))
    (when failures
      (error 'world-restart-test-cleanup-error :failures (nreverse failures))))
  nil)

(-> world-restart-tests--call-with-owned-launch (configuration function) t)
(defun world-restart-tests--call-with-owned-launch (configuration function)
  "Call FUNCTION with a launch lookup function, retaining its fresh launches until cleanup."
  (let ((launch     *world-startup-launch-function*)
        (release    (symbol-function 'platform-release-process))
        (processes  nil)
        (finished-p nil))
    (unwind-protect
         (multiple-value-prog1
             (let ((*world-startup-launch-function*
                     (lambda (&rest arguments)
                       (let ((process (apply launch arguments)))
                         (push process processes)
                         process))))
               (test-call-with-function-replacements
                (list (list 'platform-release-process
                            (lambda (platform process)
                              (unless (member process processes :test #'eq)
                                (funcall release platform process)))))
                (lambda () (funcall function (lambda () (first processes))))))
           (setf finished-p t))
      (handler-case
          (world-restart-tests--stop configuration processes)
        (error (condition)
          (if finished-p
              (error condition)
              (format *error-output* "~&Restart fixture cleanup failed: ~A~%" condition)))))))

(-> test-world-server-restart-owned-cleanup () null)
(defun test-world-server-restart-owned-cleanup ()
  "Test owned-launch cleanup without an endpoint, on normal return and nonlocal exit."
  (with-test-configuration (configuration)
    (dolist (exit-p '(nil t))
      (let* ((owned                           (list ':owned))
             (other                           (list ':other))
             (alive-p                         t)
             (events                          nil)
             (*world-startup-launch-function* (lambda (&rest arguments)
                                                  (declare (ignore arguments))
                                                  owned)))
        (test-call-with-function-replacements
         (list (list 'world-client-entry (lambda (configuration)
                                          (declare (ignore configuration))
                                          nil))
               (list 'platform-terminate-process-object
                     (lambda (platform process &key force)
                       (declare (ignore platform force))
                       (push (list ':terminate process) events)
                       (setf alive-p nil)))
               (list 'platform-process-object-alive-p
                     (lambda (platform process)
                       (declare (ignore platform process))
                       alive-p))
               (list 'platform-wait-process
                     (lambda (platform process)
                       (declare (ignore platform))
                       (push (list ':wait process) events)))
               (list 'platform-release-process
                     (lambda (platform process)
                       (declare (ignore platform))
                       (push (list ':release process) events))))
         (lambda ()
           (catch 'fixture-exit
             (world-restart-tests--call-with-owned-launch
              configuration
              (lambda (launch-process)
                (declare (ignore launch-process))
                (let ((process (funcall *world-startup-launch-function*)))
                  (platform-release-process *platform* process)
                  (test-assert (null events) "handoff does not release the retained launch")
                  (platform-release-process *platform* other)
                  (when exit-p
                    (throw 'fixture-exit nil))))))))
        (test-assert (equal (reverse events)
                            (list (list ':release other) (list ':terminate owned)
                                  (list ':wait owned) (list ':release owned)))
                     "cleanup terminates, waits and releases the owned launch without an endpoint"))))
  nil)

(-> test-world-server-restart-cleanup-failures () null)
(defun test-world-server-restart-cleanup-failures ()
  "Test all retained launches are attempted despite cleanup errors, preserving body failures."
  (with-test-configuration (configuration)
    (dolist (stage '(:lookup :probe :terminate :wait :release))
      (dolist (body-fails-p '(nil t))
        (let* ((first-launch   (list ':first))
               (faulty-launch  (list ':faulty))
               (pending        (list first-launch faulty-launch))
               (alive          (copy-list pending))
               (events         nil)
               (cleanup-error  (make-condition 'platform-error :operation stage
                                                :message                  "Injected cleanup failure."))
               (body-error     (make-condition 'platform-error :operation ':fixture-body
                                                :message                  "Injected body failure."))
               (*error-output* (make-string-output-stream))
               (*world-startup-launch-function*
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (pop pending))))
          (labels ((record (operation process)
                     "Record an attempted cleanup operation and inject its selected failure."
                     (push (list operation process) events)
                     (when (and (eq process faulty-launch) (eq operation stage))
                       (error cleanup-error))))
            (test-call-with-function-replacements
             (list (list 'world-client-entry
                         (lambda (configuration)
                           (declare (ignore configuration))
                           (when (eq stage ':lookup)
                             (error cleanup-error))
                           nil))
                   (list 'platform-process-object-alive-p
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':probe process)
                           (not (null (member process alive :test #'eq)))))
                   (list 'platform-terminate-process-object
                         (lambda (platform process &key force)
                           (declare (ignore platform force))
                           (setf alive (remove process alive :test #'eq))
                           (record ':terminate process)))
                   (list 'platform-wait-process
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':wait process)))
                   (list 'platform-release-process
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':release process))))
             (lambda ()
               (let ((observed
                       (handler-case
                           (world-restart-tests--call-with-owned-launch
                            configuration
                            (lambda (launch-process)
                              (declare (ignore launch-process))
                              (funcall *world-startup-launch-function*)
                              (funcall *world-startup-launch-function*)
                              (when body-fails-p
                                (error body-error))))
                         (error (condition) condition))))
                 (test-assert (if body-fails-p
                                  (eq observed body-error)
                                  (and (typep observed 'world-restart-test-cleanup-error)
                                       (member cleanup-error
                                               (world-restart-test-cleanup-failures observed)
                                               :test #'eq)))
                              "cleanup reports its failures only after preserving any original body error")
                 (test-assert (and (find (list ':release faulty-launch) events :test #'equal)
                                   (find (list ':terminate first-launch) events :test #'equal)
                                   (find (list ':wait first-launch) events :test #'equal)
                                   (find (list ':release first-launch) events :test #'equal))
                              "a cleanup error cannot abandon the remaining retained launch")))))))))
  nil)

(-> world-restart-tests--check-resumed-runtime (list) null)
(defun world-restart-tests--check-resumed-runtime (entry)
  "Check native timer teardown identity and timer scheduling in the resumed server."
  (let ((events
          (world-restart-tests--submit
           entry
           "(progn
  (let ((symbol (find-symbol \"ITIMER-EMULATION-DEINIT\" \"SB-IMPL\")))
    (assert (if (and symbol (fboundp symbol))
                (and (functionp *world-restart-test-native-itimer*)
                     (eq (symbol-function symbol) *world-restart-test-native-itimer*))
                (null *world-restart-test-native-itimer*))))
  (let ((timer (sb-ext:make-timer (lambda () nil))))
    (unwind-protect
         (progn (sb-ext:schedule-timer timer 3600)
                (assert (sb-ext:timer-scheduled-p timer)))
      (sb-ext:unschedule-timer timer)))
  (+ 40 2))"
           :until (lambda (events) (find ':work-finished events :key #'first)))))
    (test-assert (find-if (lambda (event)
                           (and (eq (first event) ':present)
                                (search "42" (getf (rest event) :text))))
                         events)
                 (format nil "The resumed core retains native teardown and can schedule timers: ~S" events)))
  nil)

(-> world-restart-tests--call-with-roots (pathname function) t)
(defun world-restart-tests--call-with-roots (root function)
  "Call FUNCTION with isolated CLI roots and the test's saved active core."
  (test-call-with-environment
   (append
    (loop for (variable directory) in '(("XDG_CONFIG_HOME" "config/")
                                        ("XDG_DATA_HOME" "data/")
                                        ("XDG_STATE_HOME" "state/")
                                        ("XDG_CACHE_HOME" "cache/"))
          collect (list variable (namestring (merge-pathnames directory root))))
    (list (list "AUTOLITH_ACTIVE_CORE" (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE"))
          (list "AUTOLITH_SOURCE_ROOT" (namestring (asdf:system-source-directory :autolith)))
          (list "AUTOLITH_SBCL" (sbcl-command))
          (list "AUTOLITH_SITE_CONFIG_ROOT" nil)
          (list "AUTOLITH_RECOVERED" nil)))
   function))

(-> test-world-server-restart-checkpoint () null)
(defun test-world-server-restart-checkpoint ()
  "Test a world server that cannot fork restarts from its saved heap under its launcher, twice, and serves its channel again."
  (with-platform-capability (':detached-sessions "a launcher-supervised world server restart")
    (with-test-configuration (unused root)
      (declare (ignore unused))
      (world-restart-tests--call-with-roots
       root
       (lambda ()
         (let ((configuration (configuration-create :defer-provider-validation-p t)))
           (world-restart-tests--call-with-owned-launch
            configuration
            (lambda (launch-process)
              (world-restart-tests--start configuration root)
              (let* ((entry   (first (world-terminal-endpoint-records configuration)))
                     (channel (getf (rest (rest entry)) :session-id))
                     (token   (getf (rest (rest entry)) :token)))
                (destructuring-bind (world resumed events &optional diagnostics)
                    (world-restart-tests--checkpoint configuration entry
                                                     :process (funcall launch-process))
                  (test-assert (and world resumed)
                               (format nil "a new process serves the world and the channel after the checkpoint~%~S~%~S~%~A"
                                       events diagnostics (world-restart-tests--logs configuration)))
                  (when (and world resumed)
                    (test-assert (equal (getf (rest (rest resumed)) :token) token)
                                 "the channel keeps the token its clients hold")
                    (test-assert (directory (merge-pathnames "generations/*/autolith.core"
                                                             (config :data-root configuration)))
                                 "the launcher published the saved heap as a generation")
                    (let* ((marker-present    (not (null (probe-file (world-server-restart-pathname configuration)))))
                           (world-running     (not (null (world-server-running-p configuration))))
                           (conversation-held (world-restart-tests--conversation-held-p configuration channel))
                           (ready             (and (not marker-present) world-running conversation-held)))
                      (test-assert
                       ready
                       (if ready
                           "the restarted server holds the world and the conversation, and removed its marker"
                           (format nil "the restarted server holds the world and the conversation, and removed its marker~%marker-present=~S world-running=~S conversation-held=~S~%~A"
                                   marker-present world-running conversation-held
                                   (world-restart-tests--logs configuration)))))
                    (world-restart-tests--check-resumed-runtime resumed)
                    ;; The heap was saved holding locks of a thread that
                    ;; is gone; a second checkpoint needs them again.
                    (destructuring-bind (again again-resumed again-events &optional diagnostics)
                        (world-restart-tests--checkpoint configuration resumed
                                                         :process (funcall launch-process))
                      (test-assert (and again again-resumed)
                                   (format nil "the restarted server checkpoints and restarts again~%~S~%~S~%~A"
                                           again-events diagnostics (world-restart-tests--logs configuration)))
                      (when again-resumed
                        (world-restart-tests--check-resumed-runtime again-resumed)))))))))))))
  nil)
