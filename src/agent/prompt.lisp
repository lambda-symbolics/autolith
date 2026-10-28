(in-package #:autolith)

;;;; -- System Prompt --

(defvar *request-context-hurry-up-p* nil
  "Whether the current provider request uses hurry-up guidance.")

(defvar *system-prompt-hosted-web-search-p* nil
  "Whether the current provider request offers web search.")

(defvar *system-prompt-session-date* nil
  "The process identifier and local date the system prompt reports, as (PROCESS-ID . DATE).

The date is fixed for the life of the process so the system prompt stays a
stable cached prefix across midnight; the date-change context contribution
reports the current date once it differs. Keying it by process makes a
restored image take the date of its own start.")

(defvar *system-prompt-override* nil
  "A complete replacement system prompt for the current provider request.

Inference frames bind this so their requests carry a compact frame
prompt instead of the full Autolith persona.")

(defparameter *system-prompt-template-relative-path*
  #p"docs/system-prompt.org"
  "The Org template rendered for each provider request.")

(defparameter *request-context-template-relative-path*
  #p"docs/request-context.org"
  "The Org template for mutable state delivered behind conversation history.")

(defparameter *system-prompt-template-cache* nil
  "The system-prompt template text cached for the current source load.")

(defparameter *request-context-template-cache* nil
  "The request-context template text cached for the current source load.")

(defparameter *workspace-instructions-limit* 16000
  "The characters of workspace AGENTS.md included in the prompt.")

(defparameter *system-prompt-context-value-limit* 256
  "The maximum decoded length of one dynamic system-prompt value.")

(defparameter *system-prompt-context-truncation-marker* "... [truncated]"
  "The suffix identifying a bounded dynamic system-prompt value.")

(defparameter *system-prompt-relax-warning*
  "Your own session, context, and token estimates are unreliable and often too small. Do not reduce scope, omit verification, rush, or end early because of perceived length; the harness compacts automatically, so complete and verify the task."
  "Private system-prompt warning appended while the session's relax mode is enabled.")

(defparameter *request-context-lisp-image-limit* 12000
  "The maximum characters of complete saved-image rows in request context.")

(defparameter *request-context-agenda-limit* 64000
  "The maximum characters of complete agenda rows in request context.")

(-> system-prompt--instruction-paths (pathname) list)
(defun system-prompt--instruction-paths (working-directory)
  "Return AGENTS.md paths from the project root down to WORKING-DIRECTORY."
  (let* ((root (workspace-project-root working-directory))
         (directories
           (loop for directory = working-directory
                   then (uiop:pathname-parent-directory-pathname directory)
                 repeat *workspace-project-depth-limit*
                 collect directory
                 until (or (equal directory root)
                           (equal directory
                                  (uiop:pathname-parent-directory-pathname
                                   directory))))))
    (loop for directory in (reverse directories)
          for path = (merge-pathnames "AGENTS.md" directory)
          when (uiop:file-exists-p path)
            collect path)))

(-> system-prompt--workspace-instructions (configuration) (option string))
(defun system-prompt--workspace-instructions (configuration)
  "Return the concatenated AGENTS.md instructions along the workspace path."
  (let ((sections
          (loop for path in (system-prompt--instruction-paths
                             (config :working-directory configuration))
                for contents = (handler-case
                                   (uiop:read-file-string path)
                                 (error ()
                                   nil))
                when (non-empty-string-p contents)
                  collect (format nil "From ~A:~2%~A"
                                  (system-prompt--context-value
                                   (namestring path))
                                  contents))))
    (when sections
      (bounded-string
       (format nil "Workspace instructions from ~A follow, project root ~
                    first; deeper files refine earlier ones. Respect them ~
                    for work in this workspace.~2%~{~A~^~2%~}"
               (if (rest sections)
                   "AGENTS.md files"
                   "AGENTS.md")
               sections)
       :limit *workspace-instructions-limit*))))

(-> system-prompt--current-date () string)
(defun system-prompt--current-date ()
  "Return the current local date as an ISO-8601 calendar day."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))

(-> system-prompt-session-date () string)
(defun system-prompt-session-date ()
  "Return the local date the system prompt reports for this process, fixing it on first use."
  (let ((process-id (current-process-id)))
    (unless (eql (first *system-prompt-session-date*) process-id)
      (setf *system-prompt-session-date*
            (cons process-id (system-prompt--current-date))))
    (rest *system-prompt-session-date*)))

(-> system-prompt-date-update () (option string))
(defun system-prompt-date-update ()
  "Return today's local date when it differs from the system prompt's session date, or NIL."
  (let ((today (system-prompt--current-date)))
    (and (string/= today (system-prompt-session-date))
         today)))

(-> system-prompt--context-value ((option string)) string)
(defun system-prompt--context-value (value)
  "Return VALUE as a bounded JSON string literal for untrusted prompt context."
  (let* ((text         (if (non-empty-string-p value) value "unknown"))
         (marker       *system-prompt-context-truncation-marker*)
         (prefix-limit (- *system-prompt-context-value-limit*
                          (length marker)))
         (bounded      (if (<= (length text) *system-prompt-context-value-limit*)
                           text
                           (concatenate 'string
                                        (subseq text 0 prefix-limit)
                                        marker))))
    (json-encode bounded)))

(-> system-prompt--environment-value (string) string)
(defun system-prompt--environment-value (name)
  "Return environment variable NAME as bounded untrusted prompt data."
  (system-prompt--context-value (uiop:getenv name)))

(-> system-prompt--web-run-p (configuration) boolean)
(defun system-prompt--web-run-p (configuration)
  "Return true when this request should advertise web.run."
  (and (not *system-prompt-hosted-web-search-p*)
       (not (string= (config :web-search-mode configuration) "disabled"))
       t))

(-> system-prompt--template-path () pathname)
(defun system-prompt--template-path ()
  "Return the Org system-prompt template shipped with Autolith."
  (let ((path (asdf:system-relative-pathname
               :autolith *system-prompt-template-relative-path*)))
    (unless (probe-file path)
      (error "Autolith system prompt template is missing: ~A" path))
    path))

(-> request-context--template-path () pathname)
(defun request-context--template-path ()
  "Return the Org mutable request-context template shipped with Autolith."
  (let ((path (asdf:system-relative-pathname
               :autolith *request-context-template-relative-path*)))
    (unless (probe-file path)
      (error "Autolith request-context template is missing: ~A" path))
    path))

(-> system-prompt--template () string)
(defun system-prompt--template ()
  "Return the cached Org system-prompt template text."
  (or *system-prompt-template-cache*
      (setf *system-prompt-template-cache*
            (uiop:read-file-string (system-prompt--template-path)))))

(-> request-context--template () string)
(defun request-context--template ()
  "Return the cached Org mutable request-context template text."
  (or *request-context-template-cache*
      (setf *request-context-template-cache*
            (uiop:read-file-string (request-context--template-path)))))

(-> system-prompt--org-keyword-line-p (string) boolean)
(defun system-prompt--org-keyword-line-p (line)
  "Return true when LINE is an Org keyword such as #+TITLE."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (>= (length trimmed) 2)
         (char= (char trimmed 0) #\#)
         (char= (char trimmed 1) #\+)
         t)))

(-> system-prompt--drop-org-keyword-lines (string) string)
(defun system-prompt--drop-org-keyword-lines (text)
  "Return TEXT without Org keyword lines."
  (let ((emittedp nil))
    (with-output-to-string (out)
      (with-input-from-string (in text)
        (loop for line = (read-line in nil nil)
              while line
              unless (system-prompt--org-keyword-line-p line)
                do (when emittedp
                     (terpri out))
                   (write-string line out)
                   (setf emittedp t)))
      (when (and (plusp (length text))
                 (char= (char text (1- (length text))) #\Newline)
                 emittedp)
        (terpri out)))))

(-> system-prompt--config (configuration) hash-table)
(defun system-prompt--config (configuration)
  "Return the stable org-templater bindings for CONFIGURATION."
  (dict 'eq
        :hosted-web-search-p *system-prompt-hosted-web-search-p*
        :web-run-p           (system-prompt--web-run-p configuration)
        :rlm-available       t
        :knowledge-active-p  (knowledge-active-p configuration)
        :diagnosis-available (and (knowledge-active-p configuration)
                                  (config :diagnosis-p configuration)
                                  t)
        :user                (system-prompt--environment-value "USER")
        :os                  (system-prompt--context-value (software-type))
        :os-version          (system-prompt--context-value (software-version))
        :arch                (system-prompt--context-value (string-downcase (machine-type)))
        :shell               (system-prompt--environment-value "SHELL")
        :term                (system-prompt--environment-value "TERM")
        :lisp                (system-prompt--context-value (lisp-implementation-type))
        :lisp-version        (system-prompt--context-value (lisp-implementation-version))
        :lang                (system-prompt--environment-value "LANG")
        :immutable-p         (config :immutable-p configuration)
        :source-root         (system-prompt--context-value
                              (namestring (config :source-root configuration)))
        :workspace           (system-prompt--context-value
                              (namestring (config :working-directory configuration)))
        :session-date        (system-prompt-session-date)
        :workspace-instructions
        (system-prompt--workspace-instructions configuration)))

(-> request-context--bounded-complete-lines (string integer) string)
(defun request-context--bounded-complete-lines (text limit)
  "Bound TEXT to LIMIT without splitting a line, adding a truncation marker."
  (if (<= (length text) limit)
      text
      (let* ((marker       *system-prompt-context-truncation-marker*)
             (prefix-limit (- limit (length marker) 1))
             (boundary
               (and (plusp prefix-limit)
                    (position #\Newline text
                              :from-end t
                              :end      (1+ prefix-limit)))))
        (if boundary
            (concatenate 'string
                         (subseq text 0 boundary)
                         (string #\Newline)
                         marker)
            marker))))

(-> request-context--config
    (configuration &key (:hurry-up-p boolean))
    hash-table)
(defun request-context--config (configuration &key hurry-up-p)
  "Return mutable request-local org-templater bindings for CONFIGURATION."
  (let* ((agenda (or (agenda-prompt-item-lines configuration) ""))
         (image-entries
           (request-context--bounded-complete-lines
            (lisp-image-prompt-entries configuration)
            *request-context-lisp-image-limit*)))
    (dict 'eq
          :simple-technical-english-p
          (config :simple-technical-english-p configuration)
          :hurry-up-p         hurry-up-p
          :lisp-image-entries image-entries
          :agenda
          (request-context--bounded-complete-lines
           agenda *request-context-agenda-limit*)
          :agenda-p           (plusp (length agenda)))))

(-> request-context-session-state
    (configuration &key (:hurry-up-p boolean))
    string)
(defun request-context-session-state
    (configuration &key (hurry-up-p *request-context-hurry-up-p*))
  "Render mutable session state for delivery behind conversation history."
  (string-left-trim
   '(#\Newline)
   (system-prompt--drop-org-keyword-lines
    (org-templater:render
     :template (request-context--template)
     :config   (request-context--config configuration :hurry-up-p hurry-up-p)))))

(-> system-prompt (configuration) string)
(defun system-prompt (configuration)
  "Return the system prompt specialized for CONFIGURATION and this process's session date.

Rebuild the prompt for each provider request, including after compaction.
Append the enabled relax warning to ordinary and overridden system prompts."
  (let ((prompt
          (or *system-prompt-override*
              (string-left-trim
               '(#\Newline)
               (system-prompt--drop-org-keyword-lines
                (org-templater:render
                 :template (system-prompt--template)
                 :config   (system-prompt--config configuration)))))))
    (if (config :relax-p configuration)
        (format nil "~A~2%WARNING: ~A" prompt *system-prompt-relax-warning*)
        prompt)))
