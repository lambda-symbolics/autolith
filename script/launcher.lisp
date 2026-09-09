;;;; The stable launcher for hosts without Bash.

;;; bin/autolith is Bash. bin/autolith.ps1 runs this script instead, through
;;; bin/autolith-runtime.ps1, which has already selected the runtime and set
;;; AUTOLITH_SBCL and AUTOLITH_SBCL_SOURCE_ROOT. The script does what the Bash
;;; launcher does: it separates launcher options from application arguments,
;;; runs the preloaded active image or the source loader, offers a bootstrap
;;; when the image is missing, restores the console afterwards, and enters
;;; pristine recovery when a session ends with a crash status.
;;;
;;; The console section binds kernel32 directly because this script runs
;;; before the Autolith system, and with it the platform adapter, is loaded.

(require :asdf)
(require :sb-posix)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))


;;;; -- Launcher Options --

(defparameter *launcher-value-options*
  '("--permissions" "--image" "-i" "--localgroup-handoff" "--id" "--input"
    "--output" "--generation" "--status" "--capsule" "--original-argument"
    "--workspace")
  "Options whose next argument is forwarded verbatim, as script/launcher-cli.sh does.")

(defparameter *launcher-terminal-statuses* '(64 76 130 143)
  "Session exit statuses that end the launcher without entering recovery.")

(defstruct launcher-request
  "The launcher options separated from the application arguments."
  recovery-p from-source-p update-p data-p arguments)

(defun launcher-bootstrap-command-name (source-root)
  "Return the bootstrap command to show the user for SOURCE-ROOT."
  (if (uiop:os-windows-p)
      (uiop:native-namestring (merge-pathnames "script/bootstrap.ps1" source-root))
      (uiop:native-namestring (merge-pathnames "script/bootstrap" source-root))))

(defun launcher-update-usage ()
  "Print the update sub-command usage to standard error."
  (format *error-output* "Usage: autolith update [--help]~%       autolith --update [--help]~%"))

(defun launcher-update-help ()
  "Print the update sub-command help to standard output."
  (format t "Usage: autolith update [--help]~%       autolith --update [--help]~%~%Install the latest packaged release and exit without starting a session.~%Source checkouts: update the checkout, then run script/bootstrap.ps1.~%"))

(defun launcher-parse (arguments)
  "Return the LAUNCHER-REQUEST for ARGUMENTS, exiting on a malformed update."
  (let ((request (make-launcher-request))
        (command-seen-p nil)
        (take-value-p nil)
        (options-ended-p nil)
        (remaining nil))
    (dolist (argument arguments)
      (cond
        (take-value-p
         (push argument remaining)
         (setf take-value-p nil))
        (options-ended-p
         (push argument remaining))
        ((string= argument "--")
         (setf options-ended-p t)
         (push argument remaining))
        ((string= argument "--recovery")
         (setf (launcher-request-recovery-p request) t))
        ((string= argument "--from-source")
         (setf (launcher-request-from-source-p request) t))
        ((member argument *launcher-value-options* :test #'string=)
         (setf take-value-p t)
         (push argument remaining))
        ((string= argument "--update")
         (unless command-seen-p
           (setf (launcher-request-update-p request) t))
         (push argument remaining))
        ((and (plusp (length argument)) (char= (char argument 0) #\-))
         (push argument remaining))
        (t
         (unless command-seen-p
           (when (string= argument "update")
             (setf (launcher-request-update-p request) t))
           (when (string= argument "data")
             (setf (launcher-request-data-p request) t)))
         (setf command-seen-p t)
         (push argument remaining))))
    (setf (launcher-request-arguments request) (nreverse remaining))
    (when (launcher-request-update-p request)
      ;; An update is a standalone operation, never a session option.
      (let ((operands (if (and (> (length arguments) 1)
                               (string= (car (last arguments)) "--"))
                          (butlast arguments)
                          arguments)))
        (when (or (not (member (first operands) '("update" "--update") :test #'string=))
                  (> (length operands) 2)
                  (and (= (length operands) 2)
                       (not (member (second operands) '("--help" "-h") :test #'string=))))
          (launcher-update-usage)
          (uiop:quit 64))
        (when (= (length operands) 2)
          (launcher-update-help)
          (uiop:quit 0))
        (format *error-output* "Update the source checkout, then run script/bootstrap.ps1.~%")
        (uiop:quit 64)))
    request))


;;;; -- Console State --

#+win32
(progn
  (sb-alien:define-alien-routine ("GetStdHandle" launcher--get-std-handle)
      (sb-alien:signed 64)
    (which (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("GetConsoleMode" launcher--get-console-mode)
      sb-alien:int
    (handle (sb-alien:signed 64))
    (mode (* (sb-alien:unsigned 32))))
  (sb-alien:define-alien-routine ("SetConsoleMode" launcher--set-console-mode)
      sb-alien:int
    (handle (sb-alien:signed 64))
    (mode (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("GetConsoleCP" launcher--get-console-cp)
      (sb-alien:unsigned 32))
  (sb-alien:define-alien-routine ("GetConsoleOutputCP" launcher--get-console-output-cp)
      (sb-alien:unsigned 32))
  (sb-alien:define-alien-routine ("SetConsoleCP" launcher--set-console-cp)
      sb-alien:int
    (code-page (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("SetConsoleOutputCP" launcher--set-console-output-cp)
      sb-alien:int
    (code-page (sb-alien:unsigned 32))))

(defparameter *launcher-standard-input-handle* #xFFFFFFF6
  "STD_INPUT_HANDLE.")

(defparameter *launcher-standard-output-handle* #xFFFFFFF5
  "STD_OUTPUT_HANDLE.")

(defparameter *launcher-standard-error-handle* #xFFFFFFF4
  "STD_ERROR_HANDLE.")

(defparameter *launcher-virtual-terminal-processing* #x4
  "ENABLE_VIRTUAL_TERMINAL_PROCESSING for console output.")

(defun launcher-console-mode (which)
  "Return the console mode of standard handle WHICH, or NIL off a console."
  #+win32
  (sb-alien:with-alien ((mode (sb-alien:unsigned 32)))
    (let ((handle (launcher--get-std-handle which)))
      (and (not (zerop (launcher--get-console-mode handle (sb-alien:addr mode))))
           mode)))
  #-win32
  (progn which nil))

(defun launcher-set-console-mode (which mode)
  "Set the console mode of standard handle WHICH to MODE."
  #+win32
  (launcher--set-console-mode (launcher--get-std-handle which) mode)
  #-win32
  (progn which mode nil))

(defun launcher-capture-console ()
  "Capture the console state to restore after a session, or NIL off a console."
  #+win32
  (let ((input (launcher-console-mode *launcher-standard-input-handle*))
        (output (launcher-console-mode *launcher-standard-output-handle*)))
    (when (or input output)
      (list :input input
            :output output
            :error (launcher-console-mode *launcher-standard-error-handle*)
            :input-code-page (launcher--get-console-cp)
            :output-code-page (launcher--get-console-output-cp))))
  #-win32
  (let ((state (ignore-errors
                (uiop:run-program '("stty" "-g") :input :interactive :output :string
                                                 :error-output nil))))
    (and state (plusp (length state)) (string-trim '(#\Newline #\Return) state))))

(defun launcher-enable-console-styling (state)
  "Let the launcher's own output use escape sequences while STATE is captured."
  #+win32
  (dolist (which (list *launcher-standard-output-handle* *launcher-standard-error-handle*))
    (let ((mode (launcher-console-mode which)))
      (when mode
        (launcher-set-console-mode which (logior mode *launcher-virtual-terminal-processing*)))))
  #-win32
  nil
  state)

(defun launcher-restore-console (state)
  "Restore the console STATE captured before a session ran."
  (when state
    #+win32
    (progn
      (let ((current (launcher-console-mode *launcher-standard-output-handle*)))
        (when (and current (logtest current *launcher-virtual-terminal-processing*))
          (format *error-output* "~C[?2004l~C[?25h~C[0m" #\Escape #\Escape #\Escape)
          (finish-output *error-output*)))
      (when (getf state :input)
        (launcher-set-console-mode *launcher-standard-input-handle* (getf state :input)))
      (when (getf state :output)
        (launcher-set-console-mode *launcher-standard-output-handle* (getf state :output)))
      (when (getf state :error)
        (launcher-set-console-mode *launcher-standard-error-handle* (getf state :error)))
      (launcher--set-console-cp (getf state :input-code-page))
      (launcher--set-console-output-cp (getf state :output-code-page)))
    #-win32
    (ignore-errors
     (uiop:run-program (list "stty" state) :input :interactive :output nil :error-output nil)
     (format *error-output* "~C[?2004l~C[?25h~C[0m" #\Escape #\Escape #\Escape)
     (finish-output *error-output*)))
  nil)


;;;; -- Styled Messages --

(defparameter *launcher-styled-p*
  (and (null (uiop:getenvp "NO_COLOR"))
       (interactive-stream-p *error-output*)
       t)
  "Whether launcher messages use the Autolith interface palette.")

(defun launcher-style (code text)
  "Return TEXT wrapped in SGR CODE when styling is enabled."
  (if *launcher-styled-p*
      (format nil "~C[~Am~A~C[0m" #\Escape code text #\Escape)
      text))

(defun launcher-note (control &rest arguments)
  "Print a launcher message to standard error."
  (format *error-output* "~&~?~%" control arguments)
  (finish-output *error-output*))


;;;; -- Images and Processes --

(defstruct launcher-context
  "The paths and runtime one launch works with."
  source-root sbcl data-root state-root
  recovery-core recovery-manifest active-core active-manifest
  crash-pointer recovery-session-pointer)

(defun launcher-environment-pathname (variable default)
  "Return the file VARIABLE names, or DEFAULT when it is unset."
  (let ((value (uiop:getenv variable)))
    (if (and value (plusp (length value)))
        (uiop:parse-native-namestring value)
        default)))

(defun launcher-context (source-root)
  "Return the LAUNCHER-CONTEXT for SOURCE-ROOT and the current environment."
  (let* ((data-root (autolith-application-root :data))
         (state-root (autolith-application-root :state))
         (recovery-core
           (launcher-environment-pathname
            "AUTOLITH_RECOVERY_CORE"
            (merge-pathnames "recovery/autolith-recovery.core" data-root)))
         (active-core
           (launcher-environment-pathname
            "AUTOLITH_ACTIVE_CORE"
            (merge-pathnames "active/autolith-active.core" data-root)))
         (process-id (sb-posix:getpid)))
    (make-launcher-context
     :source-root source-root
     :sbcl (or (uiop:getenvp "AUTOLITH_SBCL")
               (uiop:native-namestring sb-ext:*runtime-pathname*))
     :data-root data-root
     :state-root state-root
     :recovery-core recovery-core
     :recovery-manifest (merge-pathnames "manifest.sexp" recovery-core)
     :active-core active-core
     :active-manifest (merge-pathnames "manifest.sexp" active-core)
     :crash-pointer (merge-pathnames (format nil "crash-pointers/launcher-~D.path" process-id)
                                     state-root)
     :recovery-session-pointer
     (merge-pathnames (format nil "recovery-session-pointers/launcher-~D.sexp" process-id)
                      state-root))))

(defun launcher-run (command &key (output t) (error-output t))
  "Run COMMAND sharing this console and return its exit status."
  (let ((process (sb-ext:run-program (first command) (rest command)
                                     :search t
                                     :input t
                                     :output output
                                     :error error-output
                                     :wait t)))
    (or (sb-ext:process-exit-code process) 1)))

(defun launcher-manifest-header-p (manifest prefix)
  "Return true when MANIFEST holds a line that is PREFIX alone or PREFIX and a space."
  (with-open-file (stream manifest :direction :input :if-does-not-exist nil
                                   :external-format :utf-8)
    (and stream
         (loop for line = (read-line stream nil nil)
               while line
               thereis (and (uiop:string-prefix-p prefix line)
                            (or (= (length line) (length prefix))
                                (member (char line (length prefix))
                                        '(#\Space #\Tab #\Return))))))))

(defun launcher-core-command (context core &rest arguments)
  "Return the command running CORE with the source root and ARGUMENTS."
  (append (list (launcher-context-sbcl context)
                "--noinform" "--core" (uiop:native-namestring core)
                "--end-runtime-options"
                (uiop:native-namestring (launcher-context-source-root context)))
          arguments))

(defun launcher-script-command (context script &rest arguments)
  "Return the command running SCRIPT below the source root with ARGUMENTS."
  (append (list (launcher-context-sbcl context)
                "--script"
                (uiop:native-namestring
                 (merge-pathnames script (launcher-context-source-root context))))
          arguments))

(defun launcher-recovery-image-valid-p (context)
  "Return true when the pristine recovery image exists and answers its probe."
  (let ((core (launcher-context-recovery-core context)))
    (and (probe-file core)
         (probe-file (launcher-context-recovery-manifest context))
         (launcher-manifest-header-p (launcher-context-recovery-manifest context)
                                     "(:RECOVERY-IMAGE :VERSION 2")
         (zerop (launcher-run (launcher-core-command context core "--probe")
                              :output nil :error-output nil)))))

(defun launcher-run-recovery (context arguments)
  "Run pristine recovery with ARGUMENTS and return its exit status."
  (if (launcher-recovery-image-valid-p context)
      (launcher-run (apply #'launcher-core-command context
                           (launcher-context-recovery-core context) arguments))
      (progn
        (launcher-note "~A ~A"
                       (launcher-style "33" "Pristine recovery image is missing or invalid; using source bootstrap.")
                       (launcher-style "2" "Run script/build-recovery to replace it."))
        (launcher-run (apply #'launcher-script-command context "recovery/launcher.lisp"
                             (uiop:native-namestring (launcher-context-source-root context))
                             arguments)))))

(defun launcher-active-image-valid-p (context)
  "Return true when the fast startup image exists and matches its source."
  (let ((core (launcher-context-active-core context)))
    (and (probe-file core)
         (probe-file (launcher-context-active-manifest context))
         (launcher-manifest-header-p (launcher-context-active-manifest context)
                                     "(:ACTIVE-IMAGE :VERSION 1")
         (zerop (launcher-run (launcher-core-command
                               context core "--autolith-internal-active-image-probe")
                              :output nil :error-output nil)))))

(defun launcher-refresh-runtime (context)
  "Adopt the runtime the bootstrap recorded, when it recorded one."
  (let* ((command-pathname (merge-pathnames "runtimes/command"
                                            (launcher-context-data-root context)))
         (recorded (with-open-file (stream command-pathname :direction :input
                                                            :if-does-not-exist nil
                                                            :external-format :utf-8)
                     (and stream (read-line stream nil nil)))))
    (when (and recorded
               (plusp (length recorded))
               (uiop:absolute-pathname-p (uiop:parse-native-namestring recorded))
               (probe-file recorded))
      (setf (launcher-context-sbcl context) recorded)
      (autolith-script-setenv "AUTOLITH_SBCL" recorded)))
  nil)

(defun launcher-bootstrap (context)
  "Run the bootstrap and return 0, or 64 when no usable image results."
  (let ((status (launcher-run (launcher-script-command
                               context "script/runtime.lisp" "--install" "--script"
                               (uiop:native-namestring
                                (merge-pathnames "script/bootstrap.lisp"
                                                 (launcher-context-source-root context)))))))
    (cond
      ((not (zerop status))
       (launcher-note "~A" (launcher-style "31;1" (format nil "Autolith bootstrap failed with status ~D." status)))
       64)
      (t
       (launcher-refresh-runtime context)
       (if (launcher-active-image-valid-p context)
           0
           (progn
             (launcher-note "~A" (launcher-style "31;1" "Autolith bootstrap completed without a usable fast startup image."))
             64))))))

(defun launcher-offer-bootstrap (context)
  "Ask whether to bootstrap now; return :ready, :declined, or :failed."
  (loop
    (format *error-output* "~A fast startup image is missing or stale. Run ~A now? [Y/n] "
            (launcher-style "35;1" "Autolith")
            (launcher-style "2" (format nil "\"~A\"" (launcher-bootstrap-command-name
                                                     (launcher-context-source-root context)))))
    (finish-output *error-output*)
    (let ((answer (string-trim '(#\Space #\Tab #\Return) (or (read-line *standard-input* nil nil) "n"))))
      (cond
        ((member answer '("" "y" "Y" "yes" "YES" "Yes") :test #'string=)
         (return (if (zerop (launcher-bootstrap context)) ':ready ':failed)))
        ((member answer '("n" "N" "no" "NO" "No") :test #'string=)
         (return ':declined))
        (t
         (launcher-note "~A" (launcher-style "2" "Please answer yes or no.")))))))

(defun launcher-run-active (context request)
  "Run the session for REQUEST and return its exit status."
  (autolith-script-setenv "AUTOLITH_SOURCE_ROOT"
                          (uiop:native-namestring (launcher-context-source-root context)))
  (let ((arguments (launcher-request-arguments request)))
    (flet ((run-core ()
             (launcher-run (apply #'launcher-core-command context
                                  (launcher-context-active-core context) arguments)))
           (run-source ()
             (launcher-run (apply #'launcher-script-command context "bin/autolith-active"
                                  arguments))))
      (cond
        ((launcher-request-from-source-p request)
         (run-source))
        ((launcher-active-image-valid-p context)
         (run-core))
        ((and (interactive-stream-p *standard-input*)
              (interactive-stream-p *error-output*))
         (ecase (launcher-offer-bootstrap context)
           (:ready (run-core))
           (:failed 64)
           (:declined
            (launcher-note "~A" (launcher-style "2" "Loading Autolith from source."))
            (run-source))))
        (t
         (run-source))))))


;;;; -- Crash Pointers --

(defun launcher-prepare-pointers (context)
  "Create the pointer directories, clear stale pointers, and export their paths."
  (dolist (pointer (list (launcher-context-crash-pointer context)
                         (launcher-context-recovery-session-pointer context)))
    (ensure-directories-exist pointer)
    (when (probe-file pointer)
      (delete-file pointer)))
  (autolith-script-setenv "AUTOLITH_CRASH_POINTER"
                          (uiop:native-namestring (launcher-context-crash-pointer context)))
  (autolith-script-setenv "AUTOLITH_RECOVERY_SESSION_POINTER"
                          (uiop:native-namestring
                           (launcher-context-recovery-session-pointer context)))
  nil)

(defun launcher-delete-pointers (context)
  "Delete the pointer files this launch owned."
  (dolist (pointer (list (launcher-context-crash-pointer context)
                         (launcher-context-recovery-session-pointer context)))
    (when (probe-file pointer)
      (ignore-errors (delete-file pointer))))
  nil)

(defun launcher-capsule (context)
  "Return the crash capsule the session recorded, when it is a real capsule file."
  (let ((pointer (launcher-context-crash-pointer context)))
    (when (probe-file pointer)
      (let* ((capsule (with-open-file (stream pointer :direction :input
                                                      :external-format :utf-8)
                        (read-line stream nil nil)))
             (crashes (namestring (merge-pathnames "crashes/"
                                                   (launcher-context-state-root context)))))
        (when (and capsule
                   (uiop:string-prefix-p crashes (namestring (uiop:parse-native-namestring capsule)))
                   (uiop:string-suffix-p capsule ".sexp")
                   (probe-file capsule))
          capsule)))))

(defun launcher-recovery-arguments (context status arguments)
  "Return the recovery arguments describing a session that ended with STATUS."
  (append (list "--status" (princ-to-string status))
          (let ((capsule (launcher-capsule context)))
            (when capsule
              (list "--capsule" capsule)))
          (loop for argument in arguments
                append (list "--original-argument" argument))
          (list "--")
          arguments))


;;;; -- Entry --

(defun launcher-main (arguments)
  "Run the launcher for ARGUMENTS and return the exit status."
  (let* ((source-root (uiop:pathname-parent-directory-pathname
                       (uiop:pathname-directory-pathname (truename *load-truename*))))
         (request (launcher-parse arguments))
         (context (launcher-context source-root))
         (console (launcher-enable-console-styling (launcher-capture-console))))
    (unwind-protect
         (if (launcher-request-recovery-p request)
             (progn
               (launcher-restore-console console)
               (launcher-run-recovery context (launcher-request-arguments request)))
             (progn
               (launcher-prepare-pointers context)
               (let ((status (launcher-run-active context request)))
                 (launcher-restore-console console)
                 (cond
                   ((zerop status)
                    0)
                   ((launcher-request-data-p request)
                    status)
                   ((member status *launcher-terminal-statuses*)
                    status)
                   (t
                    (launcher-run-recovery
                     context
                     (launcher-recovery-arguments context status
                                                  (launcher-request-arguments request))))))))
      (launcher-restore-console console)
      (launcher-delete-pointers context))))

(uiop:quit
 (handler-bind ((sb-sys:interactive-interrupt
                  (lambda (condition)
                    ;; The session owns the console; Ctrl-C reaches it directly,
                    ;; and the launcher keeps waiting for its status.
                    (declare (ignore condition))
                    (let ((restart (find-restart 'continue)))
                      (when restart
                        (invoke-restart restart))))))
   (launcher-main (uiop:command-line-arguments))))
