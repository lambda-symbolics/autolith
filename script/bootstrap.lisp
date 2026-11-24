(require :asdf)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

;;;; -- Bootstrap Progress --

(defparameter *bootstrap-steps*
  #("Runtime and prerequisites"
    "Locked Lisp dependencies"
    "Bootstrap dependencies"
    "Command sandbox helper"
    "fff search library"
    "ColorLisp syntax library"
    "Pristine recovery image"
    "Fast startup image")
  "The ordered bootstrap steps shown to the user.")

(defparameter *bootstrap-interactive-p* (interactive-stream-p *standard-output*)
  "Whether progress can replace the current terminal line.")

(defparameter *bootstrap-colors-p*
  (and *bootstrap-interactive-p* (null (uiop:getenvp "NO_COLOR")))
  "Whether bootstrap output uses the Autolith interface palette.")

(defun bootstrap-style (code text)
  "Return TEXT wrapped in SGR CODE when colors are enabled."
  (if *bootstrap-colors-p*
      (format nil "~C[~Am~A~C[0m" #\Escape code text #\Escape)
      text))

(defun bootstrap-progress (step &key done-p status)
  "Show STEP and its progress bar, or its final completion or failure STATUS."
  (let* ((total     (length *bootstrap-steps*))
         (completed (if done-p total (max 0 (1- step))))
         (bar       (concatenate 'string "["
                                 (make-string (* 2 completed) :initial-element #\FULL_BLOCK)
                                 (make-string (* 2 (- total completed)) :initial-element #\LIGHT_SHADE)
                                 "]"))
         (title     (cond
                      (done-p
                       "Done.")
                      ((plusp step)
                       (aref *bootstrap-steps* (1- step)))
                      (t
                       "Starting bootstrap"))))
    (when *bootstrap-interactive-p*
      (format t "~C~C[2K" #\Return #\Escape))
    (format t "~A ~D/~D  ~A~@[  FAILED (exit status ~D)~]"
            (bootstrap-style (cond (status
                                    "31")
                                   (done-p
                                    "32")
                                   (t
                                    "35")) bar)
            step total title status)
    (when (or (not *bootstrap-interactive-p*) done-p status)
      (terpri))
    (finish-output)))

(defun bootstrap-section (step)
  "Flush a step boundary for the supervising bootstrap process."
  (format t "~%AUTOLITH-BOOTSTRAP-STEP:~D~%" step)
  (finish-output))

(defun bootstrap-run-script (command &key (attempts 1))
  "Run COMMAND and exit with its status if all ATTEMPTS fail."
  (loop for attempt from 1 to attempts
        for status = (nth-value 2
                       (uiop:run-program command :output ':interactive :error-output ':interactive
                                                 :ignore-error-status t))
        when (zerop status)
          do (return)
        do (format t "~&~A failed with exit status ~D.~%"
                   (file-namestring (pathname (third command))) status)
           (finish-output)
           (if (< attempt attempts)
               (format t "Retrying (~D of ~D).~%" (1+ attempt) attempts)
               (uiop:quit status))))

(defun bootstrap-build (script-path)
  "Build the source checkout at SCRIPT-PATH in one process with its dependency environment."
  (bootstrap-section 1)
  (let* ((script-directory (uiop:pathname-directory-pathname script-path))
         (source-root      (uiop:pathname-parent-directory-pathname script-directory))
         (version-pathname (merge-pathnames "sbcl-minimum.version" source-root))
         (sbcl-command     (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
         (script-command   (lambda (name &rest arguments)
                             (list* sbcl-command "--script"
                                    (uiop:native-namestring (merge-pathnames name script-directory))
                                    arguments)))
         (quicklisp-setup  (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
    (autolith-require-minimum-runtime version-pathname)
    (unless (probe-file quicklisp-setup)
      (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
    (uiop:with-current-directory (source-root)
      (bootstrap-section 2)
      (bootstrap-run-script (funcall script-command "qlot-install.lisp") :attempts 3)
      (bootstrap-section 3)
      (load (merge-pathnames ".qlot/setup.lisp" source-root))
      (autolith-script-configure-cffi)
      (bootstrap-section 4)
      (load (merge-pathnames "script/build-sandbox.lisp" source-root))
      (bootstrap-section 5)
      (bootstrap-run-script
       (funcall script-command "build-fff.lisp" "--artifact"
                (uiop:native-namestring
                 (merge-pathnames "source/artifact.lisp" (asdf:system-source-directory "clifff")))))
      (bootstrap-section 6)
      (asdf:load-system :colorlisp)
      (uiop:symbol-call '#:colorlisp '#:native-library-path)
      (bootstrap-section 7)
      (bootstrap-run-script (funcall script-command "build-recovery.lisp"))
      (bootstrap-section 8)
      (bootstrap-run-script (funcall script-command "build-active.lisp")))))

(defun bootstrap-supervise (script-path)
  "Capture the build at SCRIPT-PATH and show only progress or the failed step and its output."
  (format t "~&~A~%" (bootstrap-style "35;1" "Autolith bootstrap"))
  (finish-output)
  ;; All Lisp and native output is captured at the process boundary. The worker
  ;; keeps its loaded dependencies and Windows helper selection between steps.
  (uiop:with-temporary-file (:stream capture :pathname log :external-format ':utf-8)
    (let* ((process (uiop:launch-program
                     (list (or (uiop:getenv "AUTOLITH_SBCL") "sbcl") "--script"
                           (uiop:native-namestring script-path) "--worker")
                     :input nil :output ':stream :error-output ':output :external-format ':utf-8))
           (prefix  "AUTOLITH-BOOTSTRAP-STEP:")
           (step    0)
           (start   0)
           (status  nil))
      (unwind-protect
           (progn
             (with-open-stream (output (uiop:process-info-output process))
               (loop for line = (read-line output nil)
                     while line
                     do (let ((next (when (uiop:string-prefix-p prefix line)
                                      (handler-case (parse-integer line :start (length prefix))
                                        (parse-error () nil)))))
                          (cond
                            ((and next (= next (1+ step)) (<= next (length *bootstrap-steps*)))
                             (setf step next start (file-position capture))
                             (bootstrap-progress step))
                            (t
                             (write-line line capture))))))
             (setf status (uiop:wait-process process))
             (when (and (zerop status) (/= step (length *bootstrap-steps*)))
               (write-line "Bootstrap ended before all steps finished." capture)
               (setf status 1))
             (cond
               ((zerop status)
                (bootstrap-progress step :done-p t))
               (t
                (bootstrap-progress step :status status)
                (finish-output capture)
                (close capture)
                (with-open-file (input log :external-format ':utf-8)
                  (file-position input start)
                  (uiop:copy-stream-to-stream input *error-output*))
                (finish-output *error-output*))))
        (unless status
          (uiop:terminate-process process :urgent t)))
      status)))

(let ((script-path (truename *load-truename*)))
  (if (member "--worker" (uiop:command-line-arguments) :test #'string=)
      (bootstrap-build script-path)
      (uiop:quit (bootstrap-supervise script-path))))
