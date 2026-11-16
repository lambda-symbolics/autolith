(in-package #:autolith)

;;;; -- Reusable Shell Behavior --

(-> shell-session-tests--run (tool-context string &key (:command string)
                              (:timeout (integer 1)) (:separate-p boolean)) tool-result)
(defun shell-session-tests--run (context name &key (command ":") (timeout 10) separate-p)
  "Invoke the public named-shell tool with bounded test deadlines."
  (tool-execute (tool-registry-find (tool-context-registry context) "shell" "run")
                context (json-object "session" name "command" command
                                     "timeout-seconds" timeout "separate-output" separate-p)))

(-> shell-session-tests--output (tool-result tool-context &key (:stream keyword)) string)
(defun shell-session-tests--output (result context &key (stream ':output))
  "Read exact retained output rather than searching incidental preview text."
  (let* ((metadata (first (getf (tool-result-details result) :shell-logs)))
         (capture  (find stream (getf metadata :captures) :key (lambda (entry) (getf entry :stream)))))
    (utf8-octets-to-string
     (shell-output-tests--read-bytes
      (shell-output-tests--capture-path context (getf capture :reference))))))

(-> test-shell-session-platform-discovery () null)
(defun test-shell-session-platform-discovery ()
  "Expose session operations only on hosts with a safe bounded transport."
  (dolist (platform (list *platform* (make-instance 'platform)))
    (let* ((*platform*  platform)
           (supported-p (platform-shell-sessions-p platform))
           (registry    (default-tools--register-shell (make-instance 'tool-registry)))
           (run         (tool-registry-find registry "shell" "run")))
      (test-assert (eq (not (null (json-get (json-get (tool-parameters run) "properties") "session")))
                       supported-p)
                   "Session parameter follows host capability")
      (dolist (operation '("sessions" "reset" "close"))
        (test-assert (eq (not (null (tool-registry-find registry "shell" operation))) supported-p)
                     "Lifecycle lookup follows host capability"))
      (unless supported-p
        (test-assert
         (handler-case
             (progn (shell-session--require-capability) nil)
           (platform-capability-unavailable () t))
         "Internal unsupported calls fail with the platform condition"))))
  nil)

(-> test-shell-session-state-and-logs () null)
(defun test-shell-session-state-and-logs ()
  "Retain variables, functions, cwd and exact per-command stdout/stderr logs."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (let* ((configuration  (configuration-copy configuration :working-directory root))
             (registry       (default-tools--register-shell (make-instance 'tool-registry)))
             (context        (shell-output-tests--context configuration registry))
             (child          (merge-pathnames "child/" root))
             (authorizations nil))
        (ensure-directories-exist child)
        (setf (slot-value context 'command-authorization-function)
              (lambda (command directory)
                (push (list command directory) authorizations)
                ':full-access))
        (unwind-protect
             (progn
               (test-assert
                (tool-result-success-p
                 (shell-session-tests--run
                  context "development"
                  :command (format nil "declare -a VALUES=(lost kept); VALUE=${VALUES[1]}; f() { printf '%s' \"$VALUE\"; }; cd ~A"
                                    (uiop:escape-sh-token (namestring child)))))
                "Initialize state in one shell")
               (let ((result (shell-session-tests--run context "development" :command "f")))
                 (test-assert (string= "kept" (shell-session-tests--output result context))
                              "Unexported variable and shell function survive")
                 (test-assert (equal child (second (first authorizations)))
                              "Authorize subsequent commands at the observed physical cwd"))
               (let ((result (shell-session-tests--run context "development" :separate-p t
                                                       :command                          "printf out; printf err >&2; false")))
                 (test-assert (= 1 (getf (tool-result-details result) :exit-code))
                              "Preserve nonzero command status without terminating the shell")
                 (test-assert (string= "out" (shell-session-tests--output result context)) "Capture stdout")
                 (test-assert (string= "err" (shell-session-tests--output result context :stream ':error))
                              "Capture stderr separately"))
               (test-assert
                (string= "kept" (shell-session-tests--output
                                  (shell-session-tests--run context "development" :command "f") context))
                "A failed command does not discard shell state")
               (let ((other (shell-session-tests--run context "other" :command "printf '%s' \"${VALUE-unset}\"")))
                 (test-assert (string= "unset" (shell-session-tests--output other context))
                              "Different shell names have independent state"))
               (test-assert (search "development"
                                    (tool-result-content
                                     (tool-execute (tool-registry-find registry "shell" "sessions")
                                                   context (json-object))))
                            "List owner-local live shells"))
          (tool-registry-close-runtime-state registry))
        (test-assert (null (shell-session--find context "development"))
                     "Registry shutdown forgets and closes its shell pool"))))
  nil)

(-> test-shell-session-death-reset-and-scopes () null)
(defun test-shell-session-death-reset-and-scopes ()
  "Require explicit reset after exit, and reject permission or owner crossover."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (declare (ignore root))
      (let* ((registry (default-tools--register-shell (make-instance 'tool-registry)))
             (context  (shell-output-tests--context configuration registry)))
        (unwind-protect
             (progn
               (let ((result (shell-session-tests--run context "work" :command "printf final; exit 7")))
                 (test-assert (= 7 (getf (tool-result-details result) :exit-code)) "Preserve explicit shell exit status")
                 (test-assert (string= "final" (shell-session-tests--output result context))
                              "Retain output from a terminating shell"))
               (test-assert (shell-session-dead-p (shell-session--find context "work")) "Record dead shell")
               (test-assert (handler-case (progn (shell-session-tests--run context "work") nil)
                              (tool-error () t)) "Never silently replay or resurrect a dead shell")
               (test-assert (tool-result-success-p
                             (tool-execute (tool-registry-find registry "shell" "reset") context
                                           (json-object "session" "work"))) "Reset creates a fresh shell")
               (setf (slot-value context 'command-authorization-function)
                     (lambda (command directory)
                       (declare (ignore command directory)) ':sandboxed))
               (test-assert (handler-case (progn (shell-session-tests--run context "work") nil)
                              (tool-error () t)) "Never reuse a full-access shell for sandboxed authorization")
               (setf (slot-value context 'command-authorization-function)
                     (lambda (command directory)
                       (declare (ignore command directory)) ':full-access))
               (let ((other (make-instance 'tool-context :configuration configuration :registry registry
                                           :worker nil :conversation (tool-context-conversation context)
                                           :agent (make-instance 'agent)
                                           :command-authorization-function
                                           (tool-context-command-authorization-function context))))
                 (test-assert (null (shell-session--find other "work")) "Agents cannot acquire another agent's shell"))
               (tool-execute (tool-registry-find registry "shell" "close") context (json-object "session" "work"))
               (test-assert (null (shell-session--find context "work")) "Explicit close forgets the shell"))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-shell-session-timeout-and-bounds () null)
(defun test-shell-session-timeout-and-bounds ()
  "Bound capture, retain partial logs and kill descendants when a command times out."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (let* ((configuration (configuration-copy configuration :working-directory root))
             (registry      (default-tools--register-shell (make-instance 'tool-registry)))
             (context       (shell-output-tests--context configuration registry))
             (marker        (merge-pathnames "escaped-child" root)))
        (unwind-protect
             (progn
               (let* ((*shell-log-capture-byte-limit* 64)
                      (result                         (shell-session-tests--run context "bounded"
                                                                               :command "i=0; while [ $i -lt 200 ]; do printf x; i=$((i+1)); done"))
                      (capture                        (first (getf (first (getf (tool-result-details result) :shell-logs)) :captures))))
                 (test-assert (= 64 (getf capture :byte-count)) "Retained output obeys the existing finite disk limit")
                 (test-assert (= 200 (getf capture :observed-byte-count)) "Drain oversized output without blocking the shell"))
               (let ((result (shell-session-tests--run
                              context "timeout" :timeout 1
                              :command                   (format nil "printf partial; (sleep 3; printf leak > ~A) & wait"
                                                                 (uiop:escape-sh-token (namestring marker))))))
                 (test-assert (eq ':timeout (getf (tool-result-details result) :execution-status)) "Report timeout status")
                 (test-assert (string= "partial" (shell-session-tests--output result context)) "Retain partial output")
                 (test-assert (shell-session-dead-p (shell-session--find context "timeout")) "Timeout invalidates the session"))
               (sleep 3)
               (test-assert (not (probe-file marker)) "Timeout kills the entire shell descendant tree"))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-shell-session-serialization () null)
(defun test-shell-session-serialization ()
  "Serialize simultaneous submissions without mixing captures or losing state."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (declare (ignore root))
      (let* ((registry (default-tools--register-shell (make-instance 'tool-registry)))
             (context  (shell-output-tests--context configuration registry))
             (threads  nil))
        (unwind-protect
             (progn
               (shell-session-tests--run context "serial" :command "COUNT=0")
               (dolist (text '("first" "second"))
                 (let ((text text))
                   (push (sb-thread:make-thread
                          (lambda ()
                            (shell-session-tests--run
                             context "serial" :command
                             (format nil "COUNT=$((COUNT+1)); sleep 0.05; printf ~A" text))))
                         threads)))
               (let ((outputs
                       (mapcar (lambda (thread)
                                 (shell-session-tests--output (sb-thread:join-thread thread) context))
                               threads)))
                 (test-assert (equal '("first" "second") (sort outputs #'string<))
                              "Each command retains only its own output"))
               (test-assert
                (string= "2" (shell-session-tests--output
                               (shell-session-tests--run context "serial" :command "printf '%s' \"$COUNT\"") context))
                "State changes are serialized in one shell"))
          (dolist (thread threads) (sb-thread:join-thread thread))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> shell-session-tests--runtime-fixture (function) t)
(defun shell-session-tests--runtime-fixture (function)
  "Run a named-shell test with a minimal real supervised task runtime."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry      (task-augment-tool-registry
                           (default-tools--register-shell (make-instance 'tool-registry))))
           (parent        (agent-create :configuration configuration :provider (make-instance 'model-provider)
                                        :tool-registry registry :worker nil))
           (orchestrator  (task-run-tool-orchestrator (tool-registry-find registry "child" "spawn")))
           (context       (make-instance 'tool-context :configuration configuration :agent parent
                                         :registry registry :worker nil :conversation (agent-conversation parent)
                                         :command-authorization-function
                                         (lambda (command directory)
                                           (declare (ignore command directory)) ':full-access))))
      (unwind-protect
           (funcall function configuration parent orchestrator context registry)
        (task-orchestrator-close orchestrator)
        (tool-registry-close-runtime-state registry)))))

(-> test-shell-session-cancellation () null)
(defun test-shell-session-cancellation ()
  "Cancel a real detached command and require explicit shell reset afterward."
  (when (platform-shell-sessions-p *platform*)
    (shell-session-tests--runtime-fixture
     (lambda (configuration parent orchestrator context registry)
       (declare (ignore configuration))
       (unwind-protect
            (let* ((result (tool-execute (tool-registry-find registry "shell" "run") context
                                         (json-object "session" "cancelled" "command" "printf started; sleep 30"
                                                      "async" t)))
                   (job    (task-orchestrator-find-visible-job
                            orchestrator (getf (getf (rest (tool-result-details result)) :job) :id)
                            :viewer parent :tool-name "job.get")))
              (test-assert (task-tests--wait-until
                            (lambda ()
                              (let ((session (shell-session--find context "cancelled")))
                                (and session (shell-session-thread session)))) 10)
                           "Detached named command starts its persistent process")
              (job-cancel job :reason ':test-cancel)
              (multiple-value-bind (snapshot terminal-p) (session-job-await job 10)
                (test-assert terminal-p "Cancellation waits for supervised cleanup")
                (test-assert (eq (getf snapshot :state) ':aborted) "Preserve the cancelled job outcome"))
              (test-assert (shell-session-dead-p (shell-session--find context "cancelled"))
                           "Cancellation invalidates the persistent shell")
             (test-assert (not (tool-result-success-p
                                (shell-session-tests--run context "cancelled")))
                          "Cancelled commands are never replayed"))
         (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-shell-session-sandbox () null)
(defun test-shell-session-sandbox ()
  "Keep the sandbox's original workspace boundary for the entire shell lifetime."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (let* ((workspace     (merge-pathnames "workspace/" root))
             (configuration (configuration-copy configuration :working-directory workspace))
             (registry      (default-tools--register-shell (make-instance 'tool-registry)))
             (context       (shell-output-tests--context configuration registry))
             (inside        (merge-pathnames "inside" workspace))
             (outside       (merge-pathnames "outside" root)))
        (ensure-directories-exist workspace)
        (setf (slot-value context 'command-authorization-function)
              (lambda (command directory)
                (declare (ignore command directory)) ':sandboxed))
        (unwind-protect
             (let ((result
                     (shell-session-tests--run
                      context "confined" :command
                      (format nil "printf allowed > ~A; printf forbidden > ~A"
                              (uiop:escape-sh-token (namestring inside))
                              (uiop:escape-sh-token (namestring outside))))))
               (test-assert (not (tool-result-success-p result)) "Refuse writes beyond the original workspace")
               (test-assert (probe-file inside) "Allow writes inside the workspace")
               (test-assert (not (probe-file outside)) "Retain the sandbox write boundary"))
          (tool-registry-close-runtime-state registry)))))
  nil)

(-> test-shell-session-protocol-boundaries () null)
(defun test-shell-session-protocol-boundaries ()
  "Protect publication, bounded records and root cleanup against mutable shell paths."
  (when (platform-shell-sessions-p *platform*)
    (with-test-configuration (configuration root)
      (let* ((configuration (configuration-copy configuration :working-directory root))
             (registry      (default-tools--register-shell (make-instance 'tool-registry)))
             (context       (shell-output-tests--context configuration registry))
             (data          (merge-pathnames "owned/" root))
             (target        (merge-pathnames "target/" root))
             (sentinel      (merge-pathnames "sentinel" target))
             (link          (merge-pathnames "record-link" root))
             (fifo          (merge-pathnames "record-fifo" root))
             (large         (merge-pathnames "record-large" root))
             (session       (make-instance 'shell-session :name "publication" :directory root :scope nil)))
        (ensure-directories-exist data)
        (ensure-directories-exist target)
        (with-open-file (stream sentinel :direction ':output) (write-string "preserved" stream))
        (setf (shell-session-temporary session) data
              (shell-session-staging session)
              (platform-make-temporary-directory *platform* (uiop:temporary-directory) "shell-test-publish-"))
        (flet ((run (command)
                 (tool-execute (tool-registry-find registry "shell" "run") context
                               (json-object "command" command)))

               (quote-path (path)
                 (uiop:escape-sh-token (namestring path))))
          (unwind-protect
               (progn
                 (let ((result (shell-session-tests--run
                                context "shadowed" :command
                                "printf() { return 9; }; shopt -s expand_aliases; alias printf=false; PATH=/missing; false")))
                   (test-assert (= 1 (getf (tool-result-details result) :exit-code))
                                "Retained functions, aliases and PATH do not change protocol status writers"))
                 (test-assert
                  (string= "safe" (shell-session-tests--output
                                   (shell-session-tests--run context "shadowed" :command "builtin printf safe") context))
                  "Protocol survives subsequent commands with shadowed printf")
                 (dolist (name '("command" "frame" "go" "status"))
                   (let ((path (merge-pathnames name data)))
                     (run (format nil "ln -s ~A ~A" (quote-path sentinel) (quote-path path)))
                     (shell-session--write session path "approved")
                     (test-assert (string= "approved" (platform-shell-session-read-record *platform* path 32))
                                  "Atomic publication replaces the planted link")
                     (test-assert (string= "preserved" (uiop:read-file-string sentinel))
                                  "Publication never writes through the planted link")))
                 (run (format nil "ln -s /dev/zero ~A; mkfifo ~A; printf 12345 > ~A"
                              (quote-path link) (quote-path fifo) (quote-path large)))
                 (dolist (path (list link fifo large))
                   (test-assert (handler-case
                                    (progn (platform-shell-session-read-record *platform* path 4) nil)
                                  (platform-error () t))
                                "Reject links, special files and oversized records before unbounded reads"))
                 (platform-shell-session-delete-directory *platform* data)
                 (run (format nil "ln -s ~A ~A" (quote-path target)
                              (uiop:escape-sh-token (string-right-trim "/" (namestring data)))))
                 (shell-session--stop session)
                 (test-assert (string= "preserved" (uiop:read-file-string sentinel))
                              "Closing a substituted root never traverses its target")
                 (let ((overlap (make-instance 'tool-context
                                               :configuration (configuration-copy configuration
                                                                                 :working-directory (uiop:temporary-directory))
                                               :registry registry :worker nil
                                               :conversation  (tool-context-conversation context)
                                               :command-authorization-function
                                               (lambda (command directory)
                                                 (declare (ignore command directory)) ':sandboxed))))
                   (test-assert (handler-case (progn (shell-session-tests--run overlap "overlap") nil)
                                  (tool-error () t))
                                "Refuse sandbox scopes covering the trusted staging or data-root parent")))
            (shell-session--stop session)
            (tool-registry-close-runtime-state registry))))))
  nil)
