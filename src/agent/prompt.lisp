(in-package #:autolith)

;;;; -- System Prompt --

(defvar *system-prompt-hurry-up-p* nil
  "Whether the current provider request uses hurry-up guidance.")

(defvar *system-prompt-hosted-web-search-p* nil
  "Whether the current provider request offers web search.")

(defvar *system-prompt-override* nil
  "A complete replacement system prompt for the current provider request.

Inference frames bind this so their requests carry a compact frame
prompt instead of the full Autolith persona.")

(defparameter *system-prompt-template-relative-path*
  #p"docs/system-prompt.org"
  "The Org template rendered for each provider request.")

(defparameter *workspace-instructions-limit* 16000
  "The characters of workspace AGENTS.md included in the prompt.")

(defparameter *system-prompt-context-value-limit* 256
  "The maximum decoded length of one dynamic system-prompt value.")

(defparameter *system-prompt-context-truncation-marker* "... [truncated]"
  "The suffix identifying a bounded dynamic system-prompt value.")

(-> system-prompt--instruction-paths (pathname) list)
(defun system-prompt--instruction-paths (working-directory)
  "Return AGENTS.md paths from the project root down to WORKING-DIRECTORY."
  (let* ((root (workspace-project-root working-directory))
         (directories
           (loop repeat *workspace-project-depth-limit*
                 for directory = working-directory
                   then (uiop:pathname-parent-directory-pathname directory)
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
                             (configuration-working-directory configuration))
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

(-> system-prompt--context-value ((option string)) string)
(defun system-prompt--context-value (value)
  "Return VALUE as a bounded JSON string literal for untrusted prompt context."
  (let* ((text (if (non-empty-string-p value) value "unknown"))
         (marker *system-prompt-context-truncation-marker*)
         (prefix-limit (- *system-prompt-context-value-limit*
                          (length marker)))
         (bounded (if (<= (length text) *system-prompt-context-value-limit*)
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
       (not (string= (configuration-web-search-mode configuration) "disabled"))
       t))

(-> system-prompt--template-path () pathname)
(defun system-prompt--template-path ()
  "Return the Org system-prompt template shipped with Autolith."
  (let ((path (asdf:system-relative-pathname
               :autolith *system-prompt-template-relative-path*)))
    (unless (probe-file path)
      (error "Autolith system prompt template is missing: ~A" path))
    path))

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

(-> system-prompt--lisp-image-entries (configuration) string)
(defun system-prompt--lisp-image-entries (configuration)
  "Return bounded IMAGE=/INVALID= rows for the Org template."
  (bounded-string (lisp-image-prompt-entries configuration) :limit 12000))

(-> system-prompt--config
    (configuration &key (:hurry-up-p boolean))
    hash-table)
(defun system-prompt--config (configuration &key hurry-up-p)
  "Return the org-templater bindings for CONFIGURATION."
  (let ((agenda (or (agenda-prompt-item-lines configuration) "")))
    (dict 'eq
          :simple-technical-english-p
          (preferences-simple-technical-english-p configuration)
          :hosted-web-search-p *system-prompt-hosted-web-search-p*
          :web-run-p (system-prompt--web-run-p configuration)
          :hurry-up-p hurry-up-p
          :rlm-available nil
          :user (system-prompt--environment-value "USER")
          :os (system-prompt--context-value (software-type))
          :os-version (system-prompt--context-value (software-version))
          :arch (system-prompt--context-value (string-downcase (machine-type)))
          :shell (system-prompt--environment-value "SHELL")
          :term (system-prompt--environment-value "TERM")
          :lisp (system-prompt--context-value (lisp-implementation-type))
          :lisp-version (system-prompt--context-value (lisp-implementation-version))
          :lang (system-prompt--environment-value "LANG")
          :lisp-image-entries (system-prompt--lisp-image-entries configuration)
          :agenda agenda
          :agenda-p (plusp (length agenda))
          :immutable-p (configuration-immutable-p configuration)
          :source-root (system-prompt--context-value
                        (namestring (configuration-source-root configuration)))
          :workspace (system-prompt--context-value
                      (namestring (configuration-working-directory configuration)))
          :current-date (system-prompt--current-date)
          :workspace-instructions
          (system-prompt--workspace-instructions configuration))))

(-> system-prompt (configuration &key (:hurry-up-p boolean)) string)
(defun system-prompt (configuration &key (hurry-up-p *system-prompt-hurry-up-p*))
  "Return the Autolith system prompt specialized for CONFIGURATION and today.

The prompt is rebuilt for every provider request, so the embedded date,
environment, and urgent execution profile reflect the moment it is made."
  (when *system-prompt-override*
    (return-from system-prompt *system-prompt-override*))
  (string-left-trim
   '(#\Newline)
   (system-prompt--drop-org-keyword-lines
    (org-templater:render
     :template-path (system-prompt--template-path)
     :config (system-prompt--config configuration :hurry-up-p hurry-up-p)))))
