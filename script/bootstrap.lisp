(require :asdf)

;;;; -- Styled Bootstrap Output --

(defvar *bootstrap-colors-p*
  (and (null (uiop:getenvp "NO_COLOR"))
       (interactive-stream-p *standard-output*)
       t)
  "Whether bootstrap output uses the Autolith interface palette.")

(defun bootstrap-style (code text)
  "Return TEXT wrapped in SGR CODE when colors are enabled."
  (if *bootstrap-colors-p*
      (format nil "~C[~Am~A~C[0m" #\Escape code text #\Escape)
      text))

(defun bootstrap-section (title)
  "Print one brand-styled bootstrap section TITLE."
  (format t "~&~%~A~%" (bootstrap-style "35;1" title))
  (finish-output))

(defun bootstrap-note (text)
  "Print one railed detail line under the current section."
  (format t "~&~A~A~%" (bootstrap-style "2" "│ ") text)
  (finish-output))

(defun bootstrap-done ()
  "Mark successful completion of the whole bootstrap."
  (format t "~&~%~A~%" (bootstrap-style "32" "Done."))
  (finish-output))

(defclass bootstrap-rail-stream (sb-gray:fundamental-character-output-stream)
  ((target
    :initarg       :target
    :reader        bootstrap-rail-target
    :documentation "The real stream receiving railed lines.")
   (buffer
    :initform      (make-string-output-stream)
    :reader        bootstrap-rail-buffer
    :documentation "The pending characters of the current output line."))
  (:documentation
   "A character stream prefixing each completed line with a section rail.

A carriage return discards the pending line, so library and build
progress redraws collapse into their final text instead of stacking."))

(defun bootstrap-rail--emit (stream)
  "Write STREAM's pending line under the rail and clear the buffer."
  (let ((pending (get-output-stream-string (bootstrap-rail-buffer stream)))
        (target  (bootstrap-rail-target stream)))
    (format target "~&~A~A~%" (bootstrap-style "2" "│ ") pending)
    (finish-output target)))

(defmethod sb-gray:stream-write-char ((stream bootstrap-rail-stream) character)
  "Buffer CHARACTER, emitting the line at a newline and dropping it at a carriage return."
  (case character
    (#\Newline
     (bootstrap-rail--emit stream))
    (#\Return
     (get-output-stream-string (bootstrap-rail-buffer stream)))
    (t
     (write-char character (bootstrap-rail-buffer stream))))
  character)

(defmethod sb-gray:stream-line-column ((stream bootstrap-rail-stream))
  "Report no column, since the rail prefix shifts every line."
  nil)

(defmethod sb-gray:stream-force-output ((stream bootstrap-rail-stream))
  "Force output on the terminal STREAM writes to."
  (force-output (bootstrap-rail-target stream)))

(defmethod sb-gray:stream-finish-output ((stream bootstrap-rail-stream))
  "Finish output on the terminal STREAM writes to."
  (finish-output (bootstrap-rail-target stream)))

(defun bootstrap-rail-flush (stream)
  "Emit STREAM's unfinished final line when one is pending."
  (let ((pending (get-output-stream-string (bootstrap-rail-buffer stream))))
    (when (plusp (length pending))
      (format (bootstrap-rail-target stream)
              "~&~A~A~%"
              (bootstrap-style "2" "│ ")
              pending)
      (finish-output (bootstrap-rail-target stream)))))

(defun bootstrap-call-railed (function)
  "Call FUNCTION with its standard output railed under the current section.

The rebinding is thread-local, so this suits single-threaded in-process
loads; run subprocess-spawning work through BOOTSTRAP-RUN-SCRIPT, whose
pipe captures every writer."
  (let ((rail (make-instance 'bootstrap-rail-stream
                             :target *standard-output*)))
    (unwind-protect
         (let ((*standard-output* rail)
               (*error-output*    rail)
               (*trace-output*    rail))
           (funcall function))
      (bootstrap-rail-flush rail))))

(defun bootstrap-clean-line (line)
  "Return LINE's final carriage-return segment for stable railed display."
  (let ((segments (uiop:split-string line :separator '(#\Return))))
    (or (find-if (lambda (segment)
                   (plusp (length segment)))
                 segments
                 :from-end t)
        "")))

(defun bootstrap-stream-railed (stream)
  "Show merged Lisp output without mistaking cl-jpeg's status for completion."
  (loop with optimizing-p = nil
        for line = (read-line stream nil)
        while line
        do (let ((text (bootstrap-clean-line line)))
             (cond
               ((string= text "Performing compile-time optimization.. please wait.")
                (setf optimizing-p t)
                (bootstrap-note "Optimizing the JPEG dependency"))
               ((and optimizing-p (string= text "Done."))
                (setf optimizing-p nil)
                (bootstrap-note "JPEG optimization finished"))
               (t
                (bootstrap-note text))))))

(defun bootstrap-run-script (command &key (railed-p t) (attempts 1))
  "Stream COMMAND's merged output and exit with its status if all ATTEMPTS fail.

Unrailed output is copied as written so that native tools can redraw progress."
  (loop for attempt from 1 to attempts
        for status =
          (let ((process (uiop:launch-program command
                                              :input        nil
                                              :output       ':stream
                                              :error-output ':output)))
            (with-open-stream (output (uiop:process-info-output process))
              (if railed-p
                  (bootstrap-stream-railed output)
                  (loop for character = (read-char output nil)
                        while character
                        do (write-char character)
                        when (member character '(#\Newline #\Return))
                          do (finish-output))))
            (finish-output)
            (uiop:wait-process process))
        when (zerop status)
          do (return)
        do (let ((message (format nil "~A failed with exit status ~D."
                                   (file-namestring (pathname (third command))) status)))
             (if (< attempt attempts)
                 (bootstrap-note (format nil "~A Retrying (~D of ~D)."
                                         message (1+ attempt) attempts))
                 (progn
                   (format *error-output* "~&~A~%" message)
                   (finish-output *error-output*)
                   (uiop:quit status))))))


;;;; -- Bootstrap Steps --

(let* ((script-path      (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root      (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl-minimum.version" source-root))
       (sbcl-command     (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
       (script-command   (lambda (name &rest arguments)
                           (list* sbcl-command "--script"
                                  (uiop:native-namestring
                                   (merge-pathnames name script-directory))
                                  arguments)))
       (quicklisp-setup  (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file quicklisp-setup)
    (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
  (format t "~&~A~%" (bootstrap-style "35;1" "Autolith bootstrap"))
  (bootstrap-note (format nil "SBCL ~A" (lisp-implementation-version)))
  (bootstrap-note (format nil "Source ~A" (namestring source-root)))
  (uiop:with-current-directory (source-root)
    (bootstrap-section "Materializing locked Lisp dependencies")
    (bootstrap-run-script (funcall script-command "qlot-install.lisp") :attempts 3)
    (bootstrap-section "Loading bootstrap dependencies")
    (bootstrap-call-railed
     (lambda ()
       (load (merge-pathnames ".qlot/setup.lisp" source-root))
       (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
       (let ((profile-library-directory
               (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
             (library-directories
               (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
         (when (probe-file profile-library-directory)
           (pushnew profile-library-directory
                    (symbol-value library-directories)
                    :test #'equal)))))
    (bootstrap-section "Building the private command sandbox helper")
    (bootstrap-call-railed
     (lambda ()
       (load (merge-pathnames "script/build-sandbox.lisp" source-root))))
    (bootstrap-section "Preparing the private fff search library")
    (bootstrap-run-script
     (funcall script-command "build-fff.lisp" "--artifact"
              (uiop:native-namestring
               (merge-pathnames "source/artifact.lisp"
                                (asdf:system-source-directory "clifff"))))
     :railed-p nil)
    (bootstrap-section "Preparing the private ColorLisp syntax library")
    (bootstrap-call-railed
     (lambda ()
       (asdf:load-system :colorlisp)
       (uiop:symbol-call '#:colorlisp '#:native-library-path)))
    (bootstrap-section "Building the pristine recovery image")
    (bootstrap-run-script (funcall script-command "build-recovery.lisp"))
    (bootstrap-section "Building the fast startup image")
    (bootstrap-run-script (funcall script-command "build-active.lisp"))
    (bootstrap-done)))
