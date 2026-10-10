(in-package #:autolith)

;;;; -- Emacs Tools --

;;; Four tools reach the person's Emacs through its server: what they are
;;; looking at, a buffer's live text including unsaved edits, showing them a
;;; place in a file, and evaluating elisp for them. They are registered only
;;; while EMACS-SERVER-AVAILABLE-P holds, so a session without Emacs never
;;; sees them. Evaluation is as powerful as a shell command and asks the same
;;; command authorization.

(defclass emacs-tool (tool)
  ()
  (:documentation "A tool answered by the configured Emacs server."))

(defclass emacs-context-tool (emacs-tool)
  ()
  (:documentation "Report the visible windows, their buffers and the active region."))

(defclass emacs-read-buffer-tool (emacs-tool)
  ()
  (:documentation "Read a range of lines from a live buffer, unsaved edits included."))

(defclass emacs-visit-tool (emacs-tool)
  ()
  (:documentation "Show a file position in a window of the person's frame."))

(defclass emacs-eval-tool (emacs-tool)
  ()
  (:documentation "Evaluate authorized Emacs Lisp in the person's Emacs."))

(defparameter *emacs-read-buffer-maximum-lines* 2000
  "The most lines emacs.read-buffer returns at once.")

(defparameter *emacs-text-limit* 65536
  "The most characters of buffer text one Emacs tool result carries.")

(defparameter *emacs-eval-value-limit* 16384
  "The most characters of a printed emacs.eval value returned.")


;;;; -- Elisp --

(defparameter *emacs-context-elisp*
  "(let ((selected (selected-window)) (windows nil))
  (dolist (frame (frame-list))
    (when (frame-visible-p frame)
      (dolist (window (window-list frame 'no-minibuf))
        (with-current-buffer (window-buffer window)
          (push (list :buffer (buffer-name)
                      :file (or buffer-file-name :null)
                      :mode (symbol-name major-mode)
                      :line (line-number-at-pos (window-point window))
                      :column (save-excursion (goto-char (window-point window)) (current-column))
                      :modified (if (and buffer-file-name (buffer-modified-p)) t :false)
                      :selected (if (eq window selected) t :false))
                windows)))))
  (list :windows (vconcat (nreverse windows))
        :region (with-current-buffer (window-buffer selected)
                  (if (use-region-p)
                      (let ((start (region-beginning)) (end (region-end)))
                        (list :buffer (buffer-name)
                              :start-line (line-number-at-pos start)
                              :end-line (line-number-at-pos end)
                              :text (buffer-substring-no-properties start (min end (+ start 8000)))))
                    :null))))"
  "Elisp describing the visible windows and the selected window's region.")

(defparameter *emacs-read-buffer-elisp*
  "(let ((buffer (cond ((> (length a0) 0) (get-buffer a0))
                     ((> (length a1) 0) (find-buffer-visiting a1)))))
  (if (null buffer)
      (list :found :false)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((start (string-to-number a2))
               (count (string-to-number a3))
               (limit (string-to-number a4))
               (text (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- start))
                       (let ((from (point)))
                         (forward-line count)
                         (buffer-substring-no-properties from (point))))))
          (list :found t
                :buffer (buffer-name)
                :file (or buffer-file-name :null)
                :modified (if (buffer-modified-p) t :false)
                :total-lines (count-lines (point-min) (point-max))
                :start-line start
                :truncated (if (> (length text) limit) t :false)
                :text (if (> (length text) limit) (substring text 0 limit) text)))))))"
  "Elisp reading lines A2 to A2+A3 of buffer A0 or of the buffer visiting file A1.")

(defparameter *emacs-visit-elisp*
  "(let* ((buffer (find-file-noselect a0))
       (window (or (get-buffer-window buffer 'visible)
                   (seq-find (lambda (candidate)
                               (buffer-file-name (window-buffer candidate)))
                             (window-list nil 'no-minibuf))
                   (display-buffer buffer))))
  (set-window-buffer window buffer)
  (with-selected-window window
    (goto-char (point-min))
    (forward-line (1- (string-to-number a1)))
    (move-to-column (string-to-number a2))
    (recenter))
  (list :buffer (buffer-name buffer)
        :file (or (buffer-file-name buffer) :null)
        :line (string-to-number a1)))"
  "Elisp showing file A0 at line A1 and column A2 in a file window.")

(defparameter *emacs-eval-elisp*
  "(condition-case failure
    (let* ((value (eval (car (read-from-string a0)) t))
           (text (format \"%S\" value))
           (limit (string-to-number a1)))
      (list :value (if (> (length text) limit) (substring text 0 limit) text)
            :truncated (if (> (length text) limit) t :false)))
  (error (list :error (error-message-string failure))))"
  "Elisp evaluating form text A0 and printing at most A1 characters of its value.")


;;;; -- Methods --

(defmethod tool-execute ((tool emacs-tool) (context tool-context) (arguments hash-table))
  "Run TOOL's Emacs request, turning server failures into tool errors."
  (let ((name (format nil "emacs.~A" (tool-name tool))))
    (handler-case
        (tool-success (json-encode (emacs-tool--execute tool context arguments)))
      (emacs-server-error (condition)
        (error 'tool-error :message (autolith-error-message condition) :tool-name name)))))

(defgeneric emacs-tool--execute (tool context arguments)
  (:documentation "Return TOOL's JSON object result for ARGUMENTS."))

(defmethod emacs-tool--execute ((tool emacs-context-tool) context arguments)
  "Describe every window in the visible frames."
  (declare (ignore tool arguments))
  (emacs-server-call (tool-context-configuration context) *emacs-context-elisp*))

(defmethod emacs-tool--execute ((tool emacs-read-buffer-tool) context arguments)
  "Read lines from the named buffer or the buffer visiting the named file."
  (declare (ignore tool))
  (let ((buffer (tool-argument arguments "buffer"))
        (path (tool-argument arguments "path"))
        (start (or (tool-argument arguments "start-line") 1))
        (count (min (or (tool-argument arguments "line-count") 200)
                    *emacs-read-buffer-maximum-lines*)))
    (unless (or (non-empty-string-p buffer) (non-empty-string-p path))
      (error 'tool-error :message "emacs.read-buffer needs a buffer name or a path."
                         :tool-name "emacs.read-buffer"))
    (unless (and (integerp start) (plusp start) (integerp count) (plusp count))
      (error 'tool-error :message "start-line and line-count must be positive integers."
                         :tool-name "emacs.read-buffer"))
    (emacs-server-call (tool-context-configuration context) *emacs-read-buffer-elisp*
                       (or buffer "")
                       (if (non-empty-string-p path)
                           (uiop:native-namestring
                            (workspace-tool-path context path :tool-name "emacs.read-buffer"))
                           "")
                       (princ-to-string start)
                       (princ-to-string count)
                       (princ-to-string *emacs-text-limit*))))

(defmethod emacs-tool--execute ((tool emacs-visit-tool) context arguments)
  "Show PATH at LINE and COLUMN in a file window of the selected frame."
  (declare (ignore tool))
  (let ((path (tool-argument arguments "path" :required t))
        (line (or (tool-argument arguments "line") 1))
        (column (or (tool-argument arguments "column") 0)))
    (unless (and (integerp line) (plusp line) (integerp column) (not (minusp column)))
      (error 'tool-error :message "line must be positive and column non-negative."
                         :tool-name "emacs.visit"))
    (emacs-server-call (tool-context-configuration context) *emacs-visit-elisp*
                       (uiop:native-namestring
                        (workspace-tool-path context path :tool-name "emacs.visit"))
                       (princ-to-string line)
                       (princ-to-string column))))

(defmethod emacs-tool--execute ((tool emacs-eval-tool) context arguments)
  "Evaluate FORM after full-access authorization for emacs.eval -- FORM."
  (declare (ignore tool))
  (let ((form (tool-argument arguments "form" :required t)))
    (unless (non-empty-string-p form)
      (error 'tool-error :message "emacs.eval needs a non-empty form." :tool-name "emacs.eval"))
    (unless (emacs-tool--authorized-p context form)
      (error 'tool-error :message "emacs.eval was not authorized." :tool-name "emacs.eval"))
    (emacs-server-call (tool-context-configuration context) *emacs-eval-elisp*
                       form (princ-to-string *emacs-eval-value-limit*))))


;;;; -- Registration --

(-> emacs-register-tools (tool-registry) tool-registry)
(defun emacs-register-tools (registry)
  "Register the emacs.* tools in REGISTRY."
  (tool-registry-describe-namespace
   registry "emacs"
   "The person's running Emacs: what they see, live buffers, and actions in their editor.")
  (dolist (specification
           (list
            (list 'emacs-context-tool "emacs" "context"
                  "Report what the person is looking at in Emacs: each visible window's buffer, file, major mode, point line and column, unsaved state, which window is selected, and the active region's text. Use it before advising about \"this code\"."
                  (tool-object-schema (json-object) '()))
            (list 'emacs-read-buffer-tool "emacs" "read-buffer"
                  "Read lines from a live Emacs buffer, including unsaved edits, by buffer name or by the path of the file it visits. Files not open in Emacs are read with resource.read instead."
                  (tool-object-schema
                   (json-object
                    "buffer" (tool-string-property "A buffer name, as emacs.context reports it.")
                    "path" (tool-string-property "The file a buffer visits, when no buffer name is given.")
                    "start-line" (tool-integer-property "The first line, from 1; defaults to 1.")
                    "line-count" (tool-integer-property "How many lines; defaults to 200, at most 2000."))
                   '()))
            (list 'emacs-visit-tool "emacs" "visit"
                  "Show the person a place: open PATH in a file window of their selected frame and move to LINE and COLUMN. It changes what they see, nothing else."
                  (tool-object-schema
                   (json-object
                    "path" (tool-string-property "The file to show.")
                    "line" (tool-integer-property "The line, from 1; defaults to 1.")
                    "column" (tool-integer-property "The column, from 0; defaults to 0."))
                   '("path")))
            (list 'emacs-eval-tool "emacs" "eval"
                  "Evaluate one Emacs Lisp form in the person's Emacs and return its printed value or error. As powerful as a shell command, it asks the same command authorization. Prefer emacs.context, emacs.read-buffer and emacs.visit when they suffice."
                  (tool-object-schema
                   (json-object "form" (tool-string-property "One Emacs Lisp form, as text."))
                   '("form")))))
    (destructuring-bind (class namespace name description parameters) specification
      (tool-registry-register registry
                              (make-instance class
                                             :namespace namespace
                                             :name name
                                             :description description
                                             :parameters parameters))))
  registry)

(-> emacs-tool--authorized-p (tool-context string) boolean)
(defun emacs-tool--authorized-p (context form)
  "Return true when command authorization grants full access to emacs.eval -- FORM."
  (handler-case
      (eq (tool-context-authorize-command
           context
           (format nil "emacs.eval -- ~A" form)
           (config :working-directory (tool-context-configuration context)))
          ':full-access)
    (command-authorization-unavailable ()
      nil)))
