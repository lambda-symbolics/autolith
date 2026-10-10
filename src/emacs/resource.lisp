(in-package #:autolith)

;;;; -- Emacs Resources --

;;; The emacs: scheme exposes the person's running Emacs through the resource
;;; protocol. emacs:current is a read-only snapshot of what they see: visible
;;; windows, the active region and the open file buffers. emacs:buffer/<name>
;;; is one live buffer's whole text, unsaved edits included, read in line
;;; windows and edited with the same original-line operations as workspace
;;; files. An edit replaces the buffer text inside Emacs only while it still
;;; equals the observed text, so the person's concurrent typing makes it stale
;;; instead of being overwritten. Edited buffers stay unsaved and the change
;;; joins their undo history.

(defparameter *emacs-buffer-maximum-characters* (* 1024 1024)
  "The largest buffer, in characters, that emacs: reads or writes.")

(defparameter *emacs-current-region-characters* 8000
  "The most characters of the active region that emacs:current shows.")

(defparameter *emacs-current-maximum-buffers* 200
  "The most file buffers that emacs:current lists.")

(defvar *emacs-resource-digest-key* (random-data 16)
  "The process-local key identifying exact Emacs buffer snapshots.")

(defclass emacs-resolver (resource-resolver)
  ()
  (:documentation "Resolve emacs: URIs against the configured Emacs server."))

(defclass emacs-current-resource (resource)
  ()
  (:documentation "What the person currently sees in Emacs."))

(defclass emacs-buffer-resource (resource)
  ((name
    :initarg :name
    :reader emacs-buffer-resource-name
    :type non-empty-string
    :documentation "The exact Emacs buffer name."))
  (:documentation "One live Emacs buffer, unsaved edits included."))

(defclass emacs-buffer-observation (workspace-file-observation)
  ((file
    :initarg :file
    :reader emacs-buffer-observation-file
    :type (option pathname)
    :documentation "The file the buffer visits, or NIL.")
   (mode
    :initarg :mode
    :reader emacs-buffer-observation-mode
    :type string
    :documentation "The buffer's major mode.")
   (modified-p
    :initarg :modified-p
    :reader emacs-buffer-observation-modified-p
    :type boolean
    :documentation "Whether the buffer has unsaved changes.")
   (read-only-p
    :initarg :read-only-p
    :reader emacs-buffer-observation-read-only-p
    :type boolean
    :documentation "Whether the buffer refuses edits."))
  (:documentation
   "A complete buffer text snapshot, retained and edited like a workspace file."))


;;;; -- Elisp --

(defparameter *emacs-current-elisp*
  "(let ((selected (selected-window)) (windows nil) (buffers nil) (count 0))
  (dolist (frame (frame-list))
    (when (frame-visible-p frame)
      (dolist (window (window-list frame 'no-minibuf))
        (with-current-buffer (window-buffer window)
          (push (list :buffer (buffer-name)
                      :file (or buffer-file-name :null)
                      :mode (symbol-name major-mode)
                      :line (line-number-at-pos (window-point window) t)
                      :column (save-excursion (goto-char (window-point window)) (current-column))
                      :modified (if (and buffer-file-name (buffer-modified-p)) t :false)
                      :selected (if (eq window selected) t :false))
                windows)))))
  (dolist (buffer (buffer-list))
    (when (buffer-file-name buffer)
      (setq count (1+ count))
      (when (<= count (string-to-number a1))
        (push (list :buffer (buffer-name buffer)
                    :file (buffer-file-name buffer)
                    :modified (if (buffer-modified-p buffer) t :false))
              buffers))))
  (list :windows (vconcat (nreverse windows))
        :buffers (vconcat (nreverse buffers))
        :buffer-count count
        :region (with-current-buffer (window-buffer selected)
                  (if (use-region-p)
                      (let* ((start (region-beginning))
                             (end (region-end))
                             (limit (string-to-number a0)))
                        (list :buffer (buffer-name)
                              :start-line (line-number-at-pos start t)
                              :end-line (line-number-at-pos end t)
                              :truncated (if (> (- end start) limit) t :false)
                              :text (buffer-substring-no-properties start (min end (+ start limit)))))
                    :null))))"
  "Elisp describing visible windows, the region of at most A0 characters and A1 file buffers.")

(defparameter *emacs-buffer-snapshot-elisp*
  "(if (> (buffer-size) ~D)
     (list :status \"too-large\" :size (buffer-size))
   (list :status \"ok\"
         :file (or buffer-file-name :null)
         :mode (symbol-name major-mode)
         :modified (if (buffer-modified-p) t :false)
         :read-only (if buffer-read-only t :false)
         :text (buffer-substring-no-properties (point-min) (point-max))))"
  "A FORMAT control producing elisp that snapshots the current, widened buffer.")

(defparameter *emacs-buffer-read-elisp*
  "(let ((buffer (get-buffer a0)))
  (if (null buffer)
      (list :status \"missing\")
    (with-current-buffer buffer
      (save-restriction
        (widen)
        ~A))))"
  "A FORMAT control producing elisp that snapshots buffer A0.")

(defparameter *emacs-buffer-replace-elisp*
  "(let ((buffer (get-buffer a0)))
  (cond
   ((null buffer) (list :status \"missing\"))
   ((buffer-local-value 'buffer-read-only buffer) (list :status \"read-only\"))
   (t
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (if (not (string= (buffer-substring-no-properties (point-min) (point-max)) a1))
            (list :status \"stale\")
          (let ((target (current-buffer)))
            (with-temp-buffer
              (insert a2)
              (let ((source (current-buffer)))
                (with-current-buffer target
                  (replace-buffer-contents source 1.0 100)))))
          ~A))))))"
  "A FORMAT control producing elisp that replaces buffer A0's text A1 with A2.

The comparison and the replacement run in one server request, which Emacs
evaluates without interleaving the person's commands.")


;;;; -- Resolution --

(defmethod resource-resolver-read-documentation ((resolver emacs-resolver))
  "Document emacs: reads."
  (declare (ignore resolver))
  "emacs:current shows what the person sees in Emacs: visible windows with point, the active region, and open file buffers, each with its emacs:buffer/ URI; read it before advising about \"this code\". emacs:buffer/<percent-encoded-buffer-name> returns a live buffer's line-windowed text, unsaved edits included, so prefer it over workspace: for files with unsaved changes.")

(defmethod resource-resolver-edit-documentation ((resolver emacs-resolver))
  "Document emacs: edits."
  (declare (ignore resolver))
  "emacs:buffer/ buffers accept the same original-line operations as workspace: files. The edit lands in the person's live buffer, unsaved and undoable there, and is stale when they changed the buffer since your read.")

(defmethod resource-resolver-resolve
    ((resolver emacs-resolver) identifier (context tool-context))
  "Resolve emacs:current or emacs:buffer/<percent-encoded-buffer-name>."
  (declare (ignore context))
  (let ((prefix "buffer/"))
    (cond
      ((string= identifier "current")
       (make-instance 'emacs-current-resource :uri "emacs:current"))
      ((and (> (length identifier) (length prefix))
            (uiop:string-prefix-p prefix identifier))
       (let ((name (resource-uri-decode (format nil "emacs:~A" identifier)
                                        (subseq identifier (length prefix)))))
         (make-instance 'emacs-buffer-resource
                        :uri  (emacs-buffer-uri name)
                        :name name)))
      (t
       (error 'resource-operation-unsupported
              :uri       (format nil "~A:~A" (resource-resolver-scheme resolver) identifier)
              :operation ':resolve)))))

(defmethod resource-capabilities ((resource emacs-current-resource) (context tool-context))
  "Expose emacs:current for reading only."
  (declare (ignore resource context))
  '(:read))

(defmethod resource-capabilities ((resource emacs-buffer-resource) (context tool-context))
  "Expose buffers for reading and revision-gated editing."
  (declare (ignore resource context))
  '(:read :edit))


;;;; -- Observation --

(defmethod resource-observe ((resource emacs-current-resource) (context tool-context))
  "Observe the person's visible windows, region and file buffers as text."
  (let* ((snapshot (emacs-resource--call context "resource.read" *emacs-current-elisp*
                                         (princ-to-string *emacs-current-region-characters*)
                                         (princ-to-string *emacs-current-maximum-buffers*)))
         (content (emacs-current--render snapshot)))
    (make-instance 'resource-observation
                   :uri      (resource-uri resource)
                   :revision (resource-snapshot-digest *emacs-resource-digest-key* content)
                   :content  content)))

(defmethod resource-observe ((resource emacs-buffer-resource) (context tool-context))
  "Observe RESOURCE's whole live text, authorizing a visited file outside the roots."
  (let ((observation
          (emacs-buffer--observation
           resource
           (emacs-resource--call context "resource.read"
                                 (format nil *emacs-buffer-read-elisp*
                                         (emacs-buffer--snapshot-elisp))
                                 (emacs-buffer-resource-name resource))
           "resource.read")))
    (emacs-buffer--authorize context observation "resource.read")
    observation))

(defmethod resource-apply-operations
    ((resource emacs-buffer-resource) (context tool-context) &key base-revision operations)
  "Apply original-line OPERATIONS to the buffer text observed at BASE-REVISION.

Return the new observation and the normalized operations."
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held ((conversation-resource-observation-lock conversation))
      (let* ((state (workspace-file--find-observation-state
                     conversation (resource-uri resource) base-revision))
             (base  (resource-observation-state-observation state)))
        (emacs-buffer--authorize context base "resource.edit")
        (multiple-value-bind (content normalized)
            (handler-case
                (cl-hashline:edit-text
                 (resource-observation-content base)
                 (mapcar #'workspace-file--json-operation operations)
                 :visible-lines (workspace-file-observation-state-visible-ranges state)
                 :split-lines #'text--split-lines
                 :anchor-maximum-offset *workspace-file-resource-anchor-maximum-offset*)
              (cl-hashline:hashline-error (condition)
                (error 'tool-error :message (princ-to-string condition)
                                   :tool-name "resource.edit")))
          (values (emacs-buffer--replace resource context base content)
                  normalized))))))


;;;; -- Resource Tool Methods --

(defmethod resource-tool-read
    ((resource emacs-current-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read emacs:current in full."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error
           :message "emacs:current is always read in full and does not accept line windows."
           :tool-name "resource.read"))
  (let ((observation (resource-observe resource context)))
    (tool-success (format nil "URI: ~A~%Content:~%~A"
                          (resource-uri resource)
                          (resource-observation-content observation)))))

(defmethod resource-tool-read
    ((resource emacs-buffer-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read a line window of a live buffer and retain its observation for editing."
  (declare (ignore tool))
  (let* ((start-line  (max 1 (or (workspace-tool-integer-argument
                                  arguments "start-line" :fallback 1)
                                 1)))
         (line-count  (min *workspace-file-resource-maximum-line-count*
                           (max 1 (or (workspace-tool-integer-argument
                                       arguments "line-count"
                                       :fallback *workspace-file-resource-default-line-count*)
                                      *workspace-file-resource-default-line-count*))))
         (observation (resource-observe resource context))
         (total-lines (length (workspace-file-observation-lines observation))))
    (when (and (plusp total-lines) (> start-line total-lines))
      (error 'tool-error
             :message (format nil "Start line ~D is beyond the buffer's ~D lines. Request a valid window."
                              start-line total-lines)
             :tool-name "resource.read"))
    (tool-success (emacs-buffer--window-result context observation start-line line-count))))

(defmethod resource-tool-edit
    ((resource emacs-buffer-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply revision-gated line operations to a live buffer and show the edited lines."
  (declare (ignore tool))
  (let ((uri             (tool-argument arguments "uri" :required t))
        (base-revision   (tool-argument arguments "base-revision" :required t))
        (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error
             :message "Resource edit base-revision must be a non-empty string."
             :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (plusp (length operation-array)))
      (error 'tool-error
             :message "Resource edit operations must be a non-empty JSON array."
             :tool-name "resource.edit"))
    (handler-case
        (multiple-value-bind (observation normalized)
            (resource-apply-operations resource context
                                       :base-revision base-revision
                                       :operations    (coerce operation-array 'list))
          (multiple-value-bind (start-line line-count)
              (workspace-file--operation-window
               normalized (length (workspace-file-observation-lines observation)))
            (let ((result (format nil "Applied ~{~A~^; ~}. The buffer is unsaved; the person saves it.~%~A"
                                  (mapcar (lambda (operation) (getf operation :summary))
                                          normalized)
                                  (emacs-buffer--window-result
                                   context observation start-line line-count))))
              (tool-success
               (if (emacs-buffer-observation-file observation)
                   (lisp-source-edit-result-content result
                                                    (emacs-buffer-observation-file observation)
                                                    (resource-observation-content observation))
                   result)))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, or was not observed in this conversation; the buffer may have changed in Emacs. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri))))))


;;;; -- Public Functions --

(-> emacs-buffer-uri (non-empty-string) non-empty-string)
(defun emacs-buffer-uri (name)
  "Return the canonical emacs:buffer/ URI of the buffer named NAME."
  (format nil "emacs:buffer/~A"
          (resource-uri-encode name #'emacs-buffer--uri-safe-octet-p)))


;;;; -- Private Functions --

(-> emacs-resource--call (tool-context non-empty-string string &rest string) json-object)
(defun emacs-resource--call (context tool-name body &rest arguments)
  "Evaluate elisp BODY with ARGUMENTS in CONTEXT's Emacs, failing as TOOL-NAME."
  (handler-case
      (apply #'emacs-server-call (tool-context-configuration context) body arguments)
    (emacs-server-error (condition)
      (error 'tool-error :message (autolith-error-message condition) :tool-name tool-name))))

(-> emacs-buffer--uri-safe-octet-p ((unsigned-byte 8)) boolean)
(defun emacs-buffer--uri-safe-octet-p (octet)
  "Return true when OCTET may appear literally in an emacs:buffer/ identifier."
  (and (or (<= (char-code #\a) octet (char-code #\z))
           (<= (char-code #\A) octet (char-code #\Z))
           (<= (char-code #\0) octet (char-code #\9))
           (find octet (map 'list #'char-code "-._~*") :test #'=))
       t))

(-> emacs-buffer--snapshot-elisp () string)
(defun emacs-buffer--snapshot-elisp ()
  "Return elisp snapshotting the current widened buffer within the size limit."
  (format nil *emacs-buffer-snapshot-elisp* *emacs-buffer-maximum-characters*))

(-> emacs-buffer--observation
    (emacs-buffer-resource json-object non-empty-string)
    emacs-buffer-observation)
(defun emacs-buffer--observation (resource snapshot tool-name)
  "Return the observation of RESOURCE described by the Emacs SNAPSHOT reply, failing as TOOL-NAME."
  (let ((status (json-get snapshot "status"))
        (name   (emacs-buffer-resource-name resource)))
    (cond
      ((equal status "missing")
       (error 'tool-error
              :message (format nil "Emacs has no buffer named ~S; read emacs:current for the open buffers."
                               name)
              :tool-name tool-name))
      ((equal status "too-large")
       (error 'tool-error
              :message (format nil "Buffer ~S holds ~:D characters; emacs: reads at most ~:D."
                               name (json-get snapshot "size") *emacs-buffer-maximum-characters*)
              :tool-name tool-name)))
    (let ((text (json-get snapshot "text"))
          (file (json-get snapshot "file")))
      (make-instance 'emacs-buffer-observation
                     :uri             (resource-uri resource)
                     :revision        (resource-snapshot-digest
                                       *emacs-resource-digest-key*
                                       (format nil "~A~C~A" name #\Null text))
                     :content         text
                     :metadata        (list ':buffer name)
                     :kind            ':file
                     :line-ending     (string #\Newline)
                     :final-newline-p (cl-hashline:final-newline-p text)
                     :file            (and (stringp file) (uiop:parse-native-namestring file))
                     :mode            (json-get snapshot "mode")
                     :modified-p      (eq (json-get snapshot "modified") t)
                     :read-only-p     (eq (json-get snapshot "read-only") t)))))

(-> emacs-buffer--authorize (tool-context emacs-buffer-observation non-empty-string) null)
(defun emacs-buffer--authorize (context observation tool-name)
  "Require authorization for TOOL-NAME when OBSERVATION's buffer visits a file outside the roots."
  (let ((file (emacs-buffer-observation-file observation)))
    (when (and file
               (not (workspace-tool--read-path-allowed-p
                     file (workspace-tool-readable-roots context)))
               (not (workspace-tool-authorize-outside-path context file tool-name)))
      (error 'tool-error
             :message (format nil "~A requires full-access approval for buffer ~A, which visits ~A outside the workspace and source roots."
                              tool-name (resource-observation-uri observation) file)
             :tool-name tool-name)))
  nil)

(-> emacs-buffer--replace
    (emacs-buffer-resource tool-context emacs-buffer-observation string)
    emacs-buffer-observation)
(defun emacs-buffer--replace (resource context base content)
  "Replace RESOURCE's text with CONTENT in Emacs while it still equals BASE's text."
  (when (> (length content) *emacs-buffer-maximum-characters*)
    (error 'tool-error
           :message (format nil "The edited buffer would hold ~:D characters; emacs: writes at most ~:D."
                            (length content) *emacs-buffer-maximum-characters*)
           :tool-name "resource.edit"))
  (let* ((reply  (emacs-resource--call context "resource.edit"
                                       (format nil *emacs-buffer-replace-elisp*
                                               (emacs-buffer--snapshot-elisp))
                                       (emacs-buffer-resource-name resource)
                                       (resource-observation-content base)
                                       content))
         (status (json-get reply "status")))
    (cond
      ((equal status "stale")
       (error 'resource-revision-stale
              :uri               (resource-uri resource)
              :expected-revision (resource-observation-revision base)
              :actual-revision   nil))
      ((equal status "read-only")
       (error 'tool-error
              :message (format nil "Buffer ~S is read-only in Emacs."
                               (emacs-buffer-resource-name resource))
              :tool-name "resource.edit"))
      (t
       (emacs-buffer--observation resource reply "resource.edit")))))

(-> emacs-buffer--window-result
    (tool-context emacs-buffer-observation (integer 1) (integer 1))
    string)
(defun emacs-buffer--window-result (context observation start-line line-count)
  "Render OBSERVATION's line window, retaining it for edits in CONTEXT's conversation."
  (let* ((lines       (workspace-file-observation-lines observation))
         (total-lines (length lines)))
    (multiple-value-bind (body visible-ranges last-line truncated-p)
        (text--numbered-line-window lines start-line line-count
                                    *workspace-file-resource-maximum-result-characters*
                                    :line-anchor-function #'cl-hashline:line-anchor)
      (declare (ignore last-line))
      (let* ((state    (resource-observation-state-ensure
                        (tool-context-conversation context) observation
                        :visible-ranges visible-ranges))
             (ranges   (workspace-file-observation-state-visible-ranges state))
             (elisions (workspace-file--elisions total-lines ranges truncated-p))
             (file     (emacs-buffer-observation-file observation)))
        (format nil "URI: ~A~%Revision: ~A~%File: ~A~%Mode: ~A~%Unsaved changes: ~:[no~;yes~]~:[~;~%Read-only: yes~]~%Visible lines: ~A of ~D~%Elided: ~A~%Content:~%~A"
                (resource-observation-uri observation)
                (resource-observation-state-alias state)
                (if file (uiop:native-namestring file) "none")
                (emacs-buffer-observation-mode observation)
                (emacs-buffer-observation-modified-p observation)
                (emacs-buffer-observation-read-only-p observation)
                (workspace-file--format-ranges ranges)
                total-lines
                (if elisions (format nil "~{~A~^; ~}" elisions) "none")
                body)))))

(-> emacs-current--render (json-object) string)
(defun emacs-current--render (snapshot)
  "Render the emacs:current SNAPSHOT as text naming each buffer by its URI."
  (with-output-to-string (stream)
    (format stream "Visible windows:~%")
    (loop for window across (json-get snapshot "windows")
          do (format stream "- ~A~@[, visiting ~A~] (~A), point at line ~D column ~D~:[~;, unsaved~]~:[~;, selected~]~%"
                     (emacs-buffer-uri (json-get window "buffer"))
                     (emacs-current--file window)
                     (json-get window "mode")
                     (json-get window "line")
                     (json-get window "column")
                     (eq (json-get window "modified") t)
                     (eq (json-get window "selected") t)))
    (let ((region (json-get snapshot "region")))
      (if (json-object-p region)
          (format stream "Region in ~A, lines ~D-~D~:[~;, truncated~]:~%~A~%"
                  (emacs-buffer-uri (json-get region "buffer"))
                  (json-get region "start-line")
                  (json-get region "end-line")
                  (eq (json-get region "truncated") t)
                  (json-get region "text"))
          (format stream "Region: none~%")))
    (format stream "File buffers:~%")
    (loop for buffer across (json-get snapshot "buffers")
          do (format stream "- ~A, visiting ~A~:[~;, unsaved~]~%"
                     (emacs-buffer-uri (json-get buffer "buffer"))
                     (emacs-current--file buffer)
                     (eq (json-get buffer "modified") t)))
    (let ((omitted (- (json-get snapshot "buffer-count")
                      (length (json-get snapshot "buffers")))))
      (when (plusp omitted)
        (format stream "- and ~D more~%" omitted)))))

(-> emacs-current--file (json-object) (option string))
(defun emacs-current--file (entry)
  "Return ENTRY's visited file name, or NIL when it visits none."
  (let ((file (json-get entry "file")))
    (and (stringp file) file)))
