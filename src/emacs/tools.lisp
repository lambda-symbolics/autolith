(in-package #:autolith)

;;;; -- Emacs Tools --

;;; Two tools act in the person's Emacs through its server: showing them a
;;; place in a file and evaluating elisp for them. What they see and their
;;; buffers' live text are emacs: resources, read and edited through
;;; resource.read and resource.edit. Tools and scheme are registered only while
;;; EMACS-SERVER-AVAILABLE-P holds, so a session without Emacs never sees them.
;;; Evaluation is as powerful as a shell command and asks the same command
;;; authorization.

(defclass emacs-tool (tool)
  ()
  (:documentation "A tool answered by the configured Emacs server."))

(defclass emacs-visit-tool (emacs-tool)
  ()
  (:documentation "Show a file position in a window of the person's frame."))

(defclass emacs-eval-tool (emacs-tool)
  ()
  (:documentation "Evaluate authorized Emacs Lisp in the person's Emacs."))

(defparameter *emacs-eval-value-limit* 16384
  "The most characters of a printed emacs.eval value returned.")


;;;; -- Elisp --

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
  "Register the emacs.* tools and the emacs: resource scheme in REGISTRY."
  (tool-registry-describe-namespace
   registry "emacs"
   "Actions in the person's running Emacs. Read and edit what they see and their live buffers through resource.read and resource.edit with emacs: URIs.")
  (resource-registry-register (tool-registry-resource-registry registry)
                              (make-instance 'emacs-resolver :scheme "emacs"))
  (dolist (specification
           (list
            (list 'emacs-visit-tool "emacs" "visit"
                  "Show the person a place: open PATH in a file window of their selected frame and move to LINE and COLUMN. It changes what they see, nothing else."
                  (tool-object-schema
                   (json-object
                    "path" (tool-string-property "The file to show.")
                    "line" (tool-integer-property "The line, from 1; defaults to 1.")
                    "column" (tool-integer-property "The column, from 0; defaults to 0."))
                   '("path")))
            (list 'emacs-eval-tool "emacs" "eval"
                  "Evaluate one Emacs Lisp form in the person's Emacs and return its printed value or error. As powerful as a shell command, it asks the same command authorization. Prefer emacs: resources and emacs.visit when they suffice."
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
