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
                                        (platform-file-lock-owner *platform*
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
                                  :resume     (world-restart-tests--record-state
                                               (merge-pathnames "resume.sexp" directory)
                                               '(:version :phase :pid))
                                  :files      (mapcar #'world-restart-tests--file-state
                                                      (limited (uiop:directory-files directory)))))
                           (limited (uiop:subdirectories
                                     (merge-pathnames "generations/" (config :data-root configuration))))))))))

(-> test-world-server-restart-diagnostics () null)
(defun test-world-server-restart-diagnostics ()
  "Test timeout evidence for missing, saved and unreadable checkpoint stages."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((pid     (current-process-id))
           (pointer (merge-pathnames "restart-pointers/launcher-test.sexp"
                                     (config :state-root configuration)))
           (core    (merge-pathnames "generations/test-generation/autolith.core"
                                     (config :data-root configuration)))
           (missing (world-restart-tests--diagnostics configuration pid nil)))
      (test-assert (and (eql (getf missing :original-pid) pid)
                        (getf missing :original-alive-p)
                        (eq (getf (getf missing :restart-marker) :status) ':absent))
                   "missing stages report absence alongside original process liveness")
      (world-server--restart-mark configuration :launcher-pid pid :generation-id "test-generation")
      (ensure-directories-exist pointer)
      (snapshot-write pointer '(:autolith-restart :version 1 :identifier "test-generation"
                                :created-at 123 :metadata (:private "omitted")))
      (ensure-directories-exist core)
      (with-open-file (stream core :direction ':output :element-type '(unsigned-byte 8))
        (write-sequence #(1 2 3) stream))
      (let* ((saved      (world-restart-tests--diagnostics configuration pid nil))
             (envelope   (first (getf saved :restart-envelopes)))
             (generation (first (getf saved :generation-files))))
        (test-assert (and (equal (getf (getf envelope :fields) :identifier) "test-generation")
                          (not (member :metadata (getf envelope :fields)))
                          (equal (getf generation :generation) "test-generation")
                          (= (getf (first (getf generation :files)) :bytes) 3))
                     "saved-stage evidence contains identity and sizes, not envelope metadata"))
      (let ((*checkpoint-resume-diagnostic-pathname* (merge-pathnames "resume.sexp" core)))
        (checkpoint-resume-diagnostic ':channels-resumed)
        (let ((record (world-restart-tests--record-state
                       *checkpoint-resume-diagnostic-pathname* '(:version :phase :pid))))
          (test-assert (and (equal (getf (getf record :fields) :phase) "CHANNELS-RESUMED")
                            (eql (getf (getf record :fields) :pid) pid))
                       "a fresh phase file supplies scalar lifecycle evidence independently of process streams")))
      (with-open-file (stream pointer :direction ':output :if-exists ':supersede)
        (write-string "(" stream))
      (test-assert (getf (getf (world-restart-tests--diagnostics configuration pid nil)
                               :restart-envelopes)
                         :unavailable)
                   "an unreadable envelope does not abort timeout evidence collection")
      (with-open-file (stream pointer :direction ':output :if-exists ':supersede)
        (write-string (make-string 16385 :initial-element #\Space) stream))
      (test-assert (eq (getf (world-restart-tests--record-state pointer '(:identifier)) :status)
                       ':oversized)
                   "oversized stage records are not parsed")))
  nil)

(-> world-restart-tests--trace (configuration (integer 1) t) null)
(defun world-restart-tests--trace (configuration old-pid process)
  "Retain scalar cycle evidence outside fixture cleanup for opt-in native probes."
  (let ((path (uiop:getenv "AUTOLITH_TEST_WINDOWS57_TRACE")))
    (when (non-empty-string-p path)
      (with-open-file (stream (uiop:parse-native-namestring path)
                              :direction ':output :if-exists ':append
                              :if-does-not-exist ':create)
        (with-standard-io-syntax
          (write (world-restart-tests--diagnostics configuration old-pid process)
                 :stream stream)
          (terpri stream)))))
  nil)

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
    (prog1
     (if (task-tests--wait-until
          (lambda ()
            (setf world   (world-client-entry configuration)
                  resumed (localgroup--find-record-or-nil configuration channel))
            (and world resumed
                 (/= (getf (rest (rest world)) :pid) old-pid)
                 (eql (getf (rest (rest resumed)) :pid) (getf (rest (rest world)) :pid))
                 ;; Endpoints precede process services and marker removal.
                 (not (probe-file (world-server-restart-pathname configuration)))))
          180)
         (list world resumed events)
         (list nil nil events
                 (world-restart-tests--diagnostics configuration old-pid process)))
     (world-restart-tests--trace configuration old-pid process))))

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
                 (list (lisp-worker-sbcl-command) "--noinform"
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
                     (ignore-errors (localgroup-query-record world ':stop))))))
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
  (let ((launch     *localgroup-fresh-launch-function*)
        (release    (symbol-function 'platform-release-process))
        (processes  nil)
        (finished-p nil))
    (unwind-protect
         (multiple-value-prog1
             (let ((*localgroup-fresh-launch-function*
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
      (let* ((owned                              (list ':owned))
             (other                              (list ':other))
             (alive-p                            t)
             (events                             nil)
             (*localgroup-fresh-launch-function* (lambda (&rest arguments)
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
                (let ((process (funcall *localgroup-fresh-launch-function*)))
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
               (*localgroup-fresh-launch-function*
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
                              (funcall *localgroup-fresh-launch-function*)
                              (funcall *localgroup-fresh-launch-function*)
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
           (world-restart-tests--call-with-owned-launch
            configuration
            (lambda (launch-process)
              (world-restart-tests--start configuration root)
              (let* ((entry   (first (localgroup-endpoint-records configuration)))
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
