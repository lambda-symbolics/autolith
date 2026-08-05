(in-package #:autolith)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A tool touching only workspace files and subprocesses, never the active image."))

(defclass fs-read-tool (workspace-tool)
  ()
  (:documentation "Read one workspace file with numbered lines."))

(defclass fs-view-image-tool (workspace-tool)
  ()
  (:documentation "Attach one local image to the model for visual inspection."))

(defclass fs-list-tool (workspace-tool)
  ()
  (:documentation "List one workspace directory with entry kinds and sizes."))

(defclass fs-write-tool (workspace-tool)
  ()
  (:documentation "Create or replace one workspace file with supplied content."))

(defclass fs-edit-tool (workspace-tool)
  ()
  (:documentation "Replace text occurrences inside one workspace file."))

(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one bounded external command in the workspace."))

(defmethod tool-child-safe-p ((tool fs-read-tool))
  "Permit bounded workspace reads inside child agents."
  t)

(defmethod tool-child-safe-p ((tool fs-view-image-tool))
  "Permit native workspace image inspection inside child agents."
  t)

(defmethod tool-child-safe-p ((tool fs-list-tool))
  "Permit bounded workspace directory listings inside child agents."
  t)

(defmethod tool-child-safe-p ((tool fs-write-tool))
  "Permit workspace writes through the ordinary child capability boundary."
  t)

(defmethod tool-child-safe-p ((tool fs-edit-tool))
  "Permit workspace edits through the ordinary child capability boundary."
  t)

(defmethod tool-child-safe-p ((tool shell-run-tool))
  "Permit authorized workspace commands inside child agents."
  t)


;;;; -- Workspace Defaults --

(defparameter *fs-read-default-line-count* 400
  "The file lines returned by fs.read when no window is given.")

(defparameter *fs-read-stream-buffer-characters* 65536
  "The reusable character buffer used while fs.read scans a file.")

(defparameter *fs-read-maximum-result-characters* 8000
  "The maximum characters fs.read constructs for one model-visible result.")

(defparameter *shell-default-timeout-seconds* 60
  "The seconds one shell.run command may take by default.")

(defparameter *shell-maximum-timeout-seconds* 600
  "The largest timeout one shell.run command may request.")

(defparameter *shell-maximum-output-characters* 65536
  "The maximum combined output characters returned by shell.run.")

(defparameter *workspace-tool-readable-roots* nil
  "Optional pathname roots confining workspace-tool reads for the current call.")


;;;; -- Path Resolution --

(-> workspace-tool--read-path-allowed-p (pathname list) boolean)
(defun workspace-tool--read-path-allowed-p (path roots)
  "Return true when PATH resolves beneath one of the readable ROOTS."
  (let ((candidate (or (ignore-errors (truename path)) path)))
    (not
     (null
      (some (lambda (root)
              (uiop:subpathp candidate
                             (or (ignore-errors (truename root)) root)))
            roots)))))

(-> workspace-tool-path (tool-context (option string)) pathname)
(defun workspace-tool-path (context path)
  "Return PATH resolved against CONTEXT's working directory.

When *WORKSPACE-TOOL-READABLE-ROOTS* is non-NIL, reject paths outside those
roots after resolving existing symlinks."
  (let* ((working-directory (configuration-working-directory
                             (tool-context-configuration context)))
         (resolved (if (non-empty-string-p path)
                       (merge-pathnames (pathname path) working-directory)
                       working-directory)))
    (when (and *workspace-tool-readable-roots*
               (not (workspace-tool--read-path-allowed-p
                     resolved *workspace-tool-readable-roots*)))
      (error 'tool-error
             :message
             (format nil "Path ~A is outside the readable workspace and source roots."
                     resolved)
             :tool-name "fs"))
    resolved))

(-> workspace-tool-protected-path-p (tool-context pathname) boolean)
(defun workspace-tool-protected-path-p (context path)
  "Return true when PATH is off limits to workspace writes.

The stable launcher and recovery artifacts are always read-only. The rest
of Autolith's tracked source is writable only when the workspace itself is
inside the source root, meaning the user deliberately runs Autolith as a
development agent on its own repository. From any other workspace Autolith
never reaches into its own source, and live self-modification persists
through private image commits instead."
  (let* ((configuration (tool-context-configuration context))
         (source-root (configuration-source-root configuration)))
    (cond
      ((not (uiop:subpathp path source-root))
       nil)
      ((or (uiop:subpathp path (merge-pathnames "bin/" source-root))
           (uiop:subpathp path (merge-pathnames "recovery/" source-root))
           (string= (enough-namestring path source-root)
                    "script/build-recovery"))
       t)
      ((uiop:subpathp (configuration-working-directory configuration)
                      source-root)
       nil)
      (t
       t))))

(-> workspace-tool-protection-notice (tool-context pathname) string)
(defun workspace-tool-protection-notice (context path)
  "Explain why PATH is refused by the workspace write tools."
  (let ((source-root (configuration-source-root
                      (tool-context-configuration context))))
    (if (or (uiop:subpathp path (merge-pathnames "bin/" source-root))
            (uiop:subpathp path (merge-pathnames "recovery/" source-root))
            (string= (enough-namestring path source-root)
                     "script/build-recovery"))
        (format nil "~A is a stable launcher or recovery artifact and stays ~
                     read-only."
                path)
        (format nil "~A is Autolith's own source repository. Run Autolith with that ~
                     repository as the workspace to develop it, and use ~
                     self.persist-definition for live self changes."
                path))))

(-> workspace-tool-integer-argument
    (json-object string &key (:fallback (option integer)))
    (option integer))
(defun workspace-tool-integer-argument (arguments name &key fallback)
  "Return integer argument NAME from ARGUMENTS, or FALLBACK when absent."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       fallback)
      ((integerp value)
       value)
      ((and (numberp value) (= value (round value)))
       (round value))
      (t
       (error 'tool-error
              :message (format nil "Tool argument ~S must be an integer." name)
              :tool-name name)))))


;;;; -- Tool Executions --

(-> workspace--read-file-window
    (pathname (integer 1) (integer 1))
    (values string (integer 0) boolean))
(defun workspace--read-file-window (path start-line line-count)
  "Stream PATH and return its numbered window, total lines, and truncation flag.

The returned body never exceeds `*fs-read-maximum-result-characters*`.
The complete stream is still scanned so the total remains exact. A file
ending in a newline has no additional empty line, matching fs.read's
historical line-window behavior, and an empty file has zero lines."
  (let ((buffer
          (make-string (max 1 *fs-read-stream-buffer-characters*)))
        (body
          (make-array (max 1 *fs-read-maximum-result-characters*)
                      :element-type 'character
                      :fill-pointer 0))
        (end-line-exclusive (+ start-line line-count))
        (current-line 1)
        (captured-lines 0)
        (line-started-p nil)
        (saw-character-p nil)
        (last-character-newline-p nil)
        (truncated-p nil))
    (labels ((capture-character (character)
               "Append CHARACTER while the bounded result body has room."
               (if (< (fill-pointer body) (array-total-size body))
                   (vector-push character body)
                   (setf truncated-p t)))

             (capture-string (text)
               "Append TEXT through the bounded character writer."
               (loop for character across text
                     until truncated-p
                     do (capture-character character)))

             (begin-selected-line ()
               "Start the numbered representation of the current selected line."
               (unless line-started-p
                 (setf line-started-p t)
                 (unless truncated-p
                   (when (plusp captured-lines)
                     (capture-character #\Newline))
                   (capture-string (format nil "~4D  " current-line))
                   (incf captured-lines))))

             (skip-chunk-rest (start end)
               "Count newlines from START through END without retaining text."
               (incf current-line
                     (count #\Newline buffer :start start :end end))))
      (with-open-file (stream path
                              :direction :input
                              :external-format :utf-8)
        (loop
          for count = (read-character-sequence buffer stream)
          until (zerop count)
          do
             (setf saw-character-p t
                   last-character-newline-p
                   (char= (char buffer (1- count)) #\Newline))
             (let ((cursor 0))
               (loop
                 while (and (< current-line start-line)
                            (< cursor count))
                 for newline = (position #\Newline
                                         buffer
                                         :start cursor
                                         :end count)
                 do
                    (if newline
                        (progn
                          (incf current-line)
                          (setf cursor (1+ newline)))
                        (setf cursor count)))
               (loop while (< cursor count)
                     do
                        (cond
                          ((or truncated-p
                               (>= current-line end-line-exclusive))
                           (skip-chunk-rest cursor count)
                           (setf cursor count))
                          (t
                           (let ((character (char buffer cursor)))
                             (begin-selected-line)
                             (unless (char= character #\Newline)
                               (capture-character character))
                             (incf cursor)
                             (when (char= character #\Newline)
                               (incf current-line)
                               (setf line-started-p nil)))))))))
      (values
       (coerce body 'string)
       (cond
         ((not saw-character-p)
          0)
         (last-character-newline-p
          (1- current-line))
         (t
          current-line))
       truncated-p))))

(-> workspace--bounded-path-label (pathname (integer 0)) string)
(defun workspace--bounded-path-label (path maximum)
  "Return PATH's namestring bounded to MAXIMUM characters."
  (let ((label (namestring path)))
    (cond
      ((<= (length label) maximum)
       label)
      ((<= maximum 3)
       (subseq label 0 maximum))
      (t
       (format nil "~A..."
               (subseq label 0 (- maximum 3)))))))

(-> workspace--fs-read-result-content
    (pathname string
     &key (:first-line (integer 1))
          (:last-line (integer 0))
          (:total-lines (integer 0))
          (:body-truncated-p boolean))
    string)
(defun workspace--fs-read-result-content
    (path body
     &key first-line last-line total-lines body-truncated-p)
  "Return one fully bounded and honestly labeled fs.read result.

A truncated result labels its line interval as the requested window. The
path label yields space before selected text does, and the truncation marker
is retained whenever the configured maximum can contain the fixed metadata."
  (let* ((maximum (max 1 *fs-read-maximum-result-characters*))
         (path-label (namestring path))
         (ordinary-suffix
           (format nil " lines ~D-~D of ~D"
                   first-line
                   last-line
                   total-lines))
         (ordinary-header
           (format nil "~A~A" path-label ordinary-suffix))
         (ordinary-length
           (+ (length ordinary-header) 1 (length body)))
         (truncated-p
           (or body-truncated-p (> ordinary-length maximum))))
    (if (not truncated-p)
        (format nil "~A~%~A" ordinary-header body)
        (let* ((marker
                 "... fs.read output truncated; request a smaller line window.")
               (suffix
                 (format nil " requested lines ~D-~D of ~D"
                         first-line
                         last-line
                         total-lines))
               (fixed-length (+ (length suffix) 1 (length marker))))
          (if (> fixed-length maximum)
              (let ((fallback (format nil "~A~%~A" marker suffix)))
                (subseq fallback 0 (min maximum (length fallback))))
              (let* ((path-maximum (- maximum fixed-length))
                     (visible-path
                       (workspace--bounded-path-label path path-maximum))
                     (header (format nil "~A~A" visible-path suffix))
                     (body-maximum
                       (max 0
                            (- maximum
                               (length header)
                               (length marker)
                               2)))
                     (visible-body
                       (subseq body 0 (min body-maximum (length body)))))
                (if (plusp (length visible-body))
                    (format nil "~A~%~A~%~A"
                            header
                            visible-body
                            marker)
                    (format nil "~A~%~A" header marker))))))))

(defmethod tool-execute ((tool fs-view-image-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a local image as native provider image content."
  (let* ((path (workspace-tool-path
                context
                (tool-argument arguments "path" :required t)))
         (conversation (tool-context-conversation context))
         (attachment
           (image-input-prepare
            path
            (conversation-image-artifact-root conversation))))
    (tool-success
     (format nil "Viewed ~A (~Dx~D, ~A)."
             (image-attachment-source-name attachment)
             (image-attachment-width attachment)
             (image-attachment-height attachment)
             (image-attachment-mime-type attachment))
     :image-attachments (list attachment))))

(defmethod tool-execute ((tool fs-read-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a numbered window of the requested file."
  (let* ((path (workspace-tool-path
                context
                (tool-argument arguments "path" :required t)))
         (start-line (max 1 (or (workspace-tool-integer-argument
                                 arguments "start-line")
                                1)))
         (line-count (max 1 (or (workspace-tool-integer-argument
                                 arguments "line-count")
                                *fs-read-default-line-count*))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure
        (format nil "~A is a directory; use fs.list instead." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (multiple-value-bind (body total body-truncated-p)
           (workspace--read-file-window path start-line line-count)
         (let* ((window-start (min (1- start-line) total))
                (window-end (min (+ window-start line-count) total)))
           (tool-success
            (workspace--fs-read-result-content
             path
             body
             :first-line (1+ window-start)
             :last-line window-end
             :total-lines total
             :body-truncated-p body-truncated-p))))))))

(defmethod tool-execute ((tool fs-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the requested directory's entries with kinds and byte sizes."
  (let ((path (workspace-tool-path context (tool-argument arguments "path"))))
    (if (not (uiop:directory-exists-p path))
        (tool-failure (format nil "~A is not a directory." path))
        (let ((directories (sort (mapcar (lambda (directory)
                                           (first (last (pathname-directory
                                                         directory))))
                                         (uiop:subdirectories path))
                                 #'string<))
              (files (sort (uiop:directory-files path)
                           #'string<
                           :key #'namestring)))
          (tool-success
           (format nil "~A~%~{~A~%~}~{~A~%~}"
                   path
                   (loop for name in directories
                         collect (format nil "d           ~A/" name))
                   (loop for file in files
                         collect (format nil "f ~9D  ~A"
                                         (handler-case
                                             (with-open-file (stream file
                                                              :element-type
                                                              '(unsigned-byte 8))
                                               (file-length stream))
                                           (error ()
                                             0))
                                         (file-namestring file)))))))))

(-> workspace--count-occurrences (string string) (integer 0))
(defun workspace--count-occurrences (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(-> workspace--replace-occurrences
    (string string string &key (:all boolean))
    string)
(defun workspace--replace-occurrences (needle replacement haystack &key all)
  "Return HAYSTACK with NEEDLE replaced by REPLACEMENT, once or everywhere."
  (with-output-to-string (stream)
    (loop with start = 0
          for position = (search needle haystack :start2 start)
          while position
          do (write-string haystack stream :start start :end position)
             (write-string replacement stream)
             (setf start (+ position (length needle)))
          unless all
            do (loop-finish)
          finally (write-string haystack stream :start start))))

(defmethod tool-execute ((tool fs-write-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Create or replace the requested file with the supplied content."
  (let ((path (workspace-tool-path
               context
               (tool-argument arguments "path" :required t)))
        (content (tool-argument arguments "content" :required t)))
    (unless (stringp content)
      (error 'tool-error
             :message "fs.write requires string content."
             :tool-name "fs.write"))
    (cond
      ((workspace-tool-protected-path-p context path)
       (tool-failure (workspace-tool-protection-notice context path)))
      ((uiop:directory-exists-p path)
       (tool-failure (format nil "~A is a directory." path)))
      (t
       (let ((existed-p (and (probe-file path) t)))
         (ensure-directories-exist path)
         (with-open-file (stream path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
           (write-string content stream))
         (tool-success
          (format nil "~:[Created~;Replaced~] ~A with ~:D character~:P."
                  existed-p
                  path
                  (length content))))))))

(defmethod tool-execute ((tool fs-edit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Replace old-text inside the requested file, with conservative line fallback."
  (let ((path (workspace-tool-path
               context
               (tool-argument arguments "path" :required t)))
        (old-text (tool-argument arguments "old-text" :required t))
        (new-text (tool-argument arguments "new-text" :required t))
        (replace-all (tool-argument arguments "replace-all")))
    (unless (and (stringp old-text) (stringp new-text))
      (error 'tool-error
             :message "fs.edit requires string old-text and new-text."
             :tool-name "fs.edit"))
    (cond
      ((zerop (length old-text))
       (tool-failure "fs.edit requires non-empty old-text."))
      ((workspace-tool-protected-path-p context path)
       (tool-failure (workspace-tool-protection-notice context path)))
      ((or (uiop:directory-exists-p path) (not (probe-file path)))
       (tool-failure (format nil "~A is not an existing file." path)))
      (t
       (let* ((text (uiop:read-file-string path))
              (occurrences (workspace--count-occurrences old-text text)))
         (cond
           ((zerop occurrences)
            (multiple-value-bind (status replacement notice)
                (workspace--relaxed-edit path old-text new-text text)
              (ecase status
                (:success
                 (with-open-file (stream path
                                         :direction :output
                                         :if-exists :supersede
                                         :external-format :utf-8)
                   (write-string replacement stream))
                 (tool-success notice))
                (:failure
                 (tool-failure notice))
                (:none
                 (tool-failure
                  (format nil "The old-text was not found in ~A." path))))))
           ((and (> occurrences 1) (not replace-all))
            (tool-failure
             (format nil "The old-text matches ~D times in ~A; include more ~
                          context or set replace-all."
                     occurrences
                     path)))
           (t
            (with-open-file (stream path
                                    :direction :output
                                    :if-exists :supersede
                                    :external-format :utf-8)
              (write-string (workspace--replace-occurrences
                             old-text
                             new-text
                             text
                             :all (and replace-all t))
                            stream))
            (tool-success
             (format nil "Replaced ~D occurrence~:P in ~A."
                     (if replace-all
                         occurrences
                         1)
                     path)))))))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Authorize and run one bounded external command under the selected policy."
  (let* ((command (tool-argument arguments "command" :required t))
         (directory (workspace-tool-path
                     context
                     (tool-argument arguments "directory")))
         (timeout (min *shell-maximum-timeout-seconds*
                       (max 1 (or (workspace-tool-integer-argument
                                   arguments "timeout-seconds")
                                  *shell-default-timeout-seconds*)))))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (let ((authorization
            (tool-context-authorize-command context command directory)))
      (if (eq authorization ':deny)
          (tool-failure "The user denied this command.")
          (let* ((configuration (tool-context-configuration context))
                 (policy
                   (ecase authorization
                     (:sandboxed
                      (workspace-write-sandbox-policy
                       :workspace-roots
                       (list (configuration-working-directory configuration))))
                     (:full-access
                      (external-sandbox-policy))))
                 (result
                   (command-sandbox-run
                    "/bin/sh"
                    (list "-c" command)
                    :policy policy
                    :working-directory directory
                    :timeout timeout
                    :merge-output-p t
                    :output-limit *shell-maximum-output-characters*
                    :error-output-limit *shell-maximum-output-characters*))
                 (output (sandbox-result-output result))
                 (presented-output
                   (if (sandbox-result-output-truncated-p result)
                       (format nil
                               "~A~%[combined output truncated after ~D characters]"
                               output *shell-maximum-output-characters*)
                       output)))
            (if (sandbox-result-timed-out-p result)
                (tool-failure
                 (format nil "The command was stopped after ~D seconds.~%~A"
                         timeout presented-output))
                (tool-success
                 (format nil "exit ~D~%~A"
                         (sandbox-result-exit-code result)
                         presented-output))))))))
