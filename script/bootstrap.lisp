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

(defun bootstrap-done (text)
  "Print the green bootstrap completion TEXT."
  (format t "~&~%~A~%" (bootstrap-style "32" text))
  (finish-output))

(defclass bootstrap-rail-stream (sb-gray:fundamental-character-output-stream)
  ((target
    :initarg :target
    :reader bootstrap-rail-target
    :documentation "The real stream receiving railed lines.")
   (buffer
    :initform (make-string-output-stream)
    :reader bootstrap-rail-buffer
    :documentation "The pending characters of the current output line."))
  (:documentation
   "A character stream prefixing each completed line with a section rail.

A carriage return discards the pending line, so library and build
progress redraws collapse into their final text instead of stacking."))

(defun bootstrap-rail--emit (stream)
  "Write STREAM's pending line under the rail and clear the buffer."
  (let ((pending (get-output-stream-string (bootstrap-rail-buffer stream)))
        (target (bootstrap-rail-target stream)))
    (format target "~&~A~A~%" (bootstrap-style "2" "│ ") pending)
    (finish-output target)))

(defmethod sb-gray:stream-write-char ((stream bootstrap-rail-stream) character)
  (case character
    (#\Newline
     (bootstrap-rail--emit stream))
    (#\Return
     (get-output-stream-string (bootstrap-rail-buffer stream)))
    (t
     (write-char character (bootstrap-rail-buffer stream))))
  character)

(defmethod sb-gray:stream-line-column ((stream bootstrap-rail-stream))
  nil)

(defmethod sb-gray:stream-force-output ((stream bootstrap-rail-stream))
  (force-output (bootstrap-rail-target stream)))

(defmethod sb-gray:stream-finish-output ((stream bootstrap-rail-stream))
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
loads; run threaded or subprocess-spawning work through
BOOTSTRAP-RUN-RAILED, whose pipe captures every writer."
  (let ((rail (make-instance 'bootstrap-rail-stream
                             :target *standard-output*)))
    (unwind-protect
         (let ((*standard-output* rail)
               (*error-output* rail)
               (*trace-output* rail))
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

(defun bootstrap-run-railed (command)
  "Run COMMAND with its output railed and fail loudly on a bad exit.

The subprocess writes into a pipe, so threads, captured streams, and
grandchild processes all land under the rail."
  (let* ((process (uiop:launch-program command
                                       :input nil
                                       :output :stream
                                       :error-output :output))
         (output (uiop:process-info-output process)))
    (loop for line = (read-line output nil nil)
          while line
          do (bootstrap-note (bootstrap-clean-line line)))
    (let ((status (uiop:wait-process process)))
      (unless (zerop status)
        (error "Bootstrap subprocess ~{~A~^ ~} failed with status ~D."
               command status)))))


;;;; -- Bootstrap Steps --

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
       (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
  (load (merge-pathnames "script/runtime-requirement.lisp" source-root))
  (autolith-require-minimum-runtime version-pathname)
  (unless (probe-file quicklisp-setup)
    (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
  (format t "~&~A~%" (bootstrap-style "35;1" "Autolith bootstrap"))
  (bootstrap-note (format nil "SBCL ~A" (lisp-implementation-version)))
  (bootstrap-note (format nil "Source ~A" (namestring source-root)))
  (bootstrap-section "Loading bootstrap dependencies")
  (bootstrap-call-railed
   (lambda ()
     (load quicklisp-setup)
     (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
     (let ((profile-library-directory
             (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
           (library-directories
             (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
       (when (probe-file profile-library-directory)
         (pushnew profile-library-directory
                  (symbol-value library-directories)
                  :test #'equal)))))
  (uiop:with-current-directory (source-root)
    (bootstrap-section "Materializing locked Lisp dependencies")
    (bootstrap-run-railed
     (list sbcl-command
           "--script"
           (namestring (merge-pathnames "script/qlot-install.lisp"
                                        source-root))))
    (bootstrap-call-railed
     (lambda ()
       (load (merge-pathnames ".qlot/setup.lisp" source-root))))
    (bootstrap-section "Building the private command sandbox helper")
    (bootstrap-call-railed
     (lambda ()
       (load (merge-pathnames "script/build-sandbox.lisp" source-root))))
    (bootstrap-section "Building the private fff search library")
    (bootstrap-call-railed
     (lambda ()
       (load (merge-pathnames "script/build-fff.lisp" source-root))))
    (bootstrap-section "Building the private ColorLisp syntax library")
    (bootstrap-call-railed
     (lambda ()
       (asdf:load-system :colorlisp)
       (uiop:symbol-call '#:colorlisp '#:native-library-path)))
    (bootstrap-section "Building the pristine recovery image")
    (bootstrap-run-railed
     (list sbcl-command
           "--script"
           (namestring (merge-pathnames "script/build-recovery.lisp"
                                        source-root))))
    (bootstrap-section "Building the fast startup image")
    (bootstrap-run-railed
     (list sbcl-command
           "--script"
           (namestring (merge-pathnames "script/build-active.lisp"
                                        source-root))))
    (bootstrap-done
     "Autolith dependencies, private native libraries, recovery image, and fast startup image are installed.")))
