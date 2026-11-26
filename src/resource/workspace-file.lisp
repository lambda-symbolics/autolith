(in-package #:autolith)

;;;; -- Workspace File Resources --

(defparameter *workspace-file-resource-maximum-bytes* (* 4 1024 1024)
  "The maximum workspace-file snapshot size in UTF-8 bytes for revision-gated editing.")

(defparameter *workspace-file-resource-maximum-directory-entries* 1000
  "The most directory entries inspected for one workspace resource snapshot.")

(defvar *workspace-file-resource-digest-key* (random-data 16)
  "The process-local key used to identify exact workspace-file snapshots.")

(defclass workspace-file-resource (text-resource)
  ((pathname
    :initarg       :pathname
    :reader        workspace-file-resource-pathname
    :type          pathname
    :documentation "The canonical pathname represented by this resource."))
  (:documentation "An existing or potential file addressed by a workspace URI."))

(defclass workspace-file-resolver (resource-resolver)
  ()
  (:documentation "Resolve workspace: URIs against the configured working directory."))

(defgeneric workspace-file-resource-access-roots (resource context)
  (:documentation
   "Return roots RESOURCE may access without command authorization under CONTEXT."))

(defmethod workspace-file-resource-access-roots
    ((resource workspace-file-resource) (context tool-context))
  "Return CONTEXT's active workspace roots for RESOURCE."
  (declare (ignore resource))
  (workspace-tool-readable-roots context))

(defclass workspace-file-observation (text-resource-observation)
  ((kind
    :initarg :kind :reader workspace-file-observation-kind
    :type          (member :file :directory :missing)
    :documentation "Whether this snapshot is a file, directory, or missing path."))
  (:documentation "An exact filesystem snapshot with backend-independent text lines."))

(defgeneric workspace-file-observation-lines (observation)
  (:documentation "Return OBSERVATION's logical lines for the current operation."))

(defmethod workspace-file-observation-lines
    ((observation workspace-file-observation))
  "Return supplied logical lines or split OBSERVATION's exact content lazily."
  (text-resource-observation-lines observation))

(-> workspace-file--observation-retained-bytes
    (workspace-file-observation)
    (integer 0))
(defun workspace-file--observation-retained-bytes (observation)
  "Return the number of UTF-8 bytes retained by OBSERVATION's exact snapshot."
  (utf8-string-length (resource-observation-content observation)))

(defclass workspace-file-observation-state (text-resource-observation-state)
  ()
  (:documentation "A concrete file observation state for existing file consumers."))

(defgeneric workspace-file-observation-state-visible-ranges (state)
  (:documentation "Return the fully visible ranges of a file observation state."))

(defmethod workspace-file-observation-state-visible-ranges ((state workspace-file-observation-state))
  "Adapt the file API to shared text visibility."
  (text-resource-observation-state-visible-ranges state))

(defgeneric (setf workspace-file-observation-state-visible-ranges) (value state)
  (:documentation "Replace a file observation state's fully visible ranges."))

(defmethod (setf workspace-file-observation-state-visible-ranges) (value (state workspace-file-observation-state))
  "Adapt file visibility updates to shared text state."
  (setf (text-resource-observation-state-visible-ranges state) value))

(defmethod resource-observation-state-family-and-key
    ((observation workspace-file-observation))
  "Return the workspace-file state family and exact content snapshot key."
  (values 'workspace-file-observation-state
          (list (resource-observation-uri observation)
                (resource-observation-revision observation)
                (resource-observation-content observation))))


;;;; -- URI Resolution --

(-> workspace-file--uri-safe-octet-p ((unsigned-byte 8)) boolean)
(defun workspace-file--uri-safe-octet-p (octet)
  "Return true when OCTET may appear literally in a workspace URI identifier."
  (or (resource-uri-unreserved-octet-p octet) (= octet #x2F)))

(-> workspace-file--encode-identifier (string) string)
(defun workspace-file--encode-identifier (identifier)
  "Percent-encode IDENTIFIER while retaining readable path separators."
  (resource-uri-encode identifier #'workspace-file--uri-safe-octet-p))

(-> workspace-file--decode-identifier (string string) string)
(defun workspace-file--decode-identifier (scheme identifier)
  "Decode percent escapes in a workspace-like URI IDENTIFIER."
  (resource-uri-decode (format nil "~A:~A" scheme identifier) identifier))

(-> workspace-file--canonical-uri (tool-context pathname) string)
(defun workspace-file--canonical-uri (context path)
  "Return PATH's stable canonical workspace URI under CONTEXT."
  (let* ((working-directory
           (workspace-tool--canonical-path
            (config :working-directory
             (tool-context-configuration context))))
         (canonical-path (workspace-tool--canonical-path path))
         (identifier
           (if (uiop:subpathp canonical-path working-directory)
               (workspace-tool--relative-identifier canonical-path working-directory)
               (uiop:native-namestring canonical-path))))
    (format nil "workspace:~A"
            (workspace-file--encode-identifier identifier))))

(defmethod resource-resolver-read-documentation ((resolver workspace-file-resolver))
  "Document workspace: reads."
  (declare (ignore resolver))
  "workspace: URIs return bounded, line-windowed file or directory observations. Identifiers are workspace-relative or absolute paths, for example workspace:src/main.lisp, workspace:src/ for a directory listing, and workspace:. for the workspace root; spaces may stay literal or be percent-encoded. Absolute native paths are also accepted and use the same full-access authorization outside the workspace and source roots.")

(defmethod resource-resolver-edit-documentation ((resolver workspace-file-resolver))
  "Document workspace: edits."
  (declare (ignore resolver))
  "workspace: files accept structured original-line operations with optional anchors from resource.read; anchors validate exact lines or correct only a small line-number transcription offset. Workspace directories are read-only. Successful source-file edits may append a non-fatal delimiter warning.")

(defmethod resource-resolver-resolve
    ((resolver workspace-file-resolver) identifier context)
  "Resolve IDENTIFIER without granting authority to the resulting workspace path."
  (declare (ignore resolver))
  (let ((path (workspace-tool-resolve-path
               context (workspace-file--decode-identifier
                        "workspace" identifier))))
    (make-instance 'workspace-file-resource
                   :uri      (workspace-file--canonical-uri context path)
                   :pathname path)))

(defmethod resource-capabilities
    ((resource workspace-file-resource) (context tool-context))
  "Return workspace path operations allowed by CONTEXT for RESOURCE."
  (declare (ignore context))
  (let ((path (workspace-file-resource-pathname resource)))
    (if (member (workspace-file--path-kind path) '(:directory :other))
        '(:read)
        '(:read :edit :stage :transaction))))


;;;; -- Snapshot Observation --

(-> workspace-file--path-kind
    (pathname)
    (member :file :directory :missing :other))
(defun workspace-file--path-kind (path)
  "Return the exact filesystem kind currently present at PATH."
  (handler-case
      (let ((status (platform-path-status *platform* path :follow-links-p t)))
        (cond
          ((null status)
           (if (platform-path-status *platform* path)
               ':other
               ':missing))
          ((eq (platform-file-status-kind status) ':file)
           ':file)
          ((eq (platform-file-status-kind status) ':directory)
           ':directory)
          (t
           ':other)))
    (platform-error (condition)
      (error 'tool-error
             :message   (format nil "Could not inspect workspace resource ~A: ~A"
                                path condition)
             :code      (tool-failure-code condition)
             :tool-name "resource.read"))))

(-> workspace-file--read-content
    (pathname
     &key (:context (option tool-context))
          (:maximum-bytes (integer 0))
          (:tool-name non-empty-string)
          (:description non-empty-string))
    string)
(defun workspace-file--read-content
    (path &key context
            (maximum-bytes *workspace-file-resource-maximum-bytes*)
            (tool-name     "resource.read")
            (description   "Workspace resource snapshot"))
  "Read PATH as a bounded, stable UTF-8 workspace resource."
  (file--read-bounded-utf-8
   path
   :maximum-bytes maximum-bytes
   :tool-name     tool-name
   :description   description
   :validation-function
   (and context
        (lambda ()
          (workspace-tool-confined-path context (uiop:native-namestring path)
                                        :tool-name "resource.read")))))

(-> workspace-file--directory-entry-row (pathname string) (option string))
(defun workspace-file--directory-entry-row (directory name)
  "Return one non-opening directory row for NAME beneath DIRECTORY.

Return NIL when NAME disappears during enumeration."
  (handler-case
      (let ((metadata
              (platform-path-status
               *platform*
               (uiop:parse-native-namestring
                (concatenate 'string
                             (uiop:native-namestring directory) name)))))
        (and metadata
             (ecase (platform-file-status-kind metadata)
               (:directory
                (format nil "d           ~A/" name))
               (:file
                (format nil "f ~9D  ~A" (platform-file-status-size metadata) name))
               (:symbolic-link
                (format nil "l ~9D  ~A" (platform-file-status-size metadata) name))
               ((:socket :other)
                (format nil "o           ~A" name)))))
    (platform-error (condition)
      (error 'tool-error
             :message   (format nil "Could not inspect directory entry ~A beneath ~A: ~A"
                                name directory condition)
             :code      (tool-failure-code condition)
             :tool-name "resource.read"))))

(-> workspace-file--directory-content (pathname) string)
(defun workspace-file--directory-content (path)
  "Return a bounded sorted directory listing without opening its entries."
  (let ((entries     nil)
        (truncated-p nil))
    (multiple-value-bind (names more-p)
        (handler-case
            (platform-list-directory
             *platform* path
             :limit *workspace-file-resource-maximum-directory-entries*)
          (platform-error (condition)
            (error 'tool-error
                   :message   (format nil "Could not list workspace directory ~A: ~A"
                                      path condition)
                   :code      (tool-failure-code condition)
                   :tool-name "resource.read")))
      (setf truncated-p more-p)
      (dolist (name names)
        (when-let ((row (workspace-file--directory-entry-row path name)))
          (push (list name row) entries))))
    (setf entries
          (sort entries
                (lambda (left right)
                  (let ((left-directory-p
                          (char= (char (second left) 0) #\d))
                        (right-directory-p
                          (char= (char (second right) 0) #\d)))
                    (if (eq left-directory-p right-directory-p)
                        (string< (first left) (first right))
                        left-directory-p)))))
    (let* ((marker     (format nil "[directory listing truncated]~%"))
           (limit      *workspace-file-resource-maximum-result-characters*)
           (row-budget (max 0 (- limit (length marker))))
           (used       0))
      (with-output-to-string (stream)
        (dolist (entry entries)
          (let ((row (format nil "~A~%" (second entry))))
            (if (> (+ used (length row)) row-budget)
                (progn
                  (setf truncated-p t)
                  (return))
                (progn
                  (write-string row stream)
                  (incf used (length row))))))
        (when truncated-p
          (write-string
           (subseq marker 0 (min limit (length marker)))
           stream))))))

(-> workspace-file--snapshot-revision
    ((member :file :directory :missing) string)
    string)
(defun workspace-file--snapshot-revision (kind content)
  "Return a revision distinguishing snapshot KIND and exact CONTENT."
  (resource-snapshot-digest
   *workspace-file-resource-digest-key*
   (format nil "~(~A~)~C~A" kind #\Null content)))

(defgeneric workspace-file-check-path (resource context)
  (:documentation "Check resource-specific path constraints before observing or staging a file."))

(defmethod workspace-file-check-path :before
    ((resource workspace-file-resource) (context tool-context))
  "Recheck the mandatory folder policy at observation and publication boundaries."
  (declare (ignore context))
  (tool-path-blacklist-check (workspace-file-resource-pathname resource)))

(defmethod workspace-file-check-path ((resource workspace-file-resource) (context tool-context))
  "Ordinary workspace paths have no additional resource-specific constraints."
  (declare (ignore resource context))
  nil)

(-> workspace-file--observe-path
    (workspace-file-resource tool-context)
    workspace-file-observation)
(defun workspace-file--observe-path (resource context)
  "Return a complete observation of RESOURCE's current filesystem state."
  (workspace-file-check-path resource context)
  (let* ((path (workspace-file-resource-pathname resource))
         (kind (workspace-file--path-kind path)))
    (when (eq kind ':other)
      (error 'tool-error
             :message   (format nil "Resource ~A is not a regular file or directory."
                                (resource-uri resource))
             :tool-name "resource.read"))
    (let ((content
            (case kind
              (:file
               (workspace-file--read-content path :context context))
              (:directory
               (workspace-file--directory-content path))
              (:missing
               ""))))
      (make-instance 'workspace-file-observation
                     :uri             (resource-uri resource)
                     :revision        (workspace-file--snapshot-revision
                                       kind content)
                     :content         content
                     :metadata        (list ':pathname path ':kind kind)
                     :kind            kind
                     :line-ending     (cl-hashline:line-ending content)
                     :final-newline-p (cl-hashline:final-newline-p content)))))

(defmethod resource-observe
    ((resource workspace-file-resource) (context tool-context))
  "Observe RESOURCE as a complete exact filesystem snapshot under CONTEXT."
  (workspace-file--observe-path resource context))

(defmethod resource-whole-text
    ((resource workspace-file-resource) (context tool-context)
     &key maximum-bytes tool-name)
  "Read a workspace file whole up to MAXIMUM-BYTES, past the editing snapshot limit.

Directories and missing files keep their ordinary observation."
  (workspace-file-check-path resource context)
  (let ((path (workspace-file-resource-pathname resource)))
    (if (eq (workspace-file--path-kind path) ':file)
        (workspace-file--read-content path :context       context
                                           :maximum-bytes maximum-bytes
                                           :tool-name     tool-name
                                           :description   "Workspace resource")
        (call-next-method))))


;;;; -- Conversation Observation State --


(defparameter *workspace-file-proposal-observations* nil
  "Conversation, URI and alias keyed baselines owned by one guarded proposal operation.")

(-> workspace-file--find-observation-state
    (conversation non-empty-string non-empty-string)
    workspace-file-observation-state)
(defun workspace-file--find-observation-state (conversation uri alias)
  "Return CONVERSATION's exact URI observation ALIAS or signal stale revision."
  (text-resource--find-state conversation uri alias :family   'workspace-file-observation-state
                                                   :overrides *workspace-file-proposal-observations*))


;;;; -- Bounded Observation Rendering --

(-> workspace-file--read-result
    (workspace-file-observation-state string
     &key (:total-lines (integer 0)) (:truncated-p boolean))
    string)
(defun workspace-file--read-result (state body &key total-lines truncated-p)
  "Render a file observation using the shared text result pipeline."
  (text-resource--read-result state body :total-lines total-lines :truncated-p truncated-p))


;;;; -- Atomic Publication --

(-> workspace-file--temporary-path (pathname) pathname)
(defun workspace-file--temporary-path (path)
  "Return a fresh same-directory temporary pathname for PATH.

The file name alone seeds the temporary name: a Windows device left in place
would print as a drive prefix, and NTFS reads NAME:REST as a named stream."
  (let ((native-file
          (uiop:native-namestring
           (make-pathname :host nil :device nil :directory nil :defaults path))))
    (merge-pathnames
     (uiop:parse-native-namestring
      (format nil ".~A.autolith-resource-~A.tmp"
              native-file
              (subseq (daemon-random-token) 0 16)))
     (uiop:pathname-directory-pathname path))))

(-> workspace-file--replacement-octets (string) (simple-array (unsigned-byte 8) (*)))
(defun workspace-file--replacement-octets (content)
  "Return CONTENT as UTF-8 octets after enforcing the exact replacement limit."
  (let ((octets (utf8-string-to-octets content)))
    (when (> (length octets) *workspace-file-resource-maximum-bytes*)
      (error 'tool-error
             :message
             (format nil "Prospective workspace resource replacement is ~:D UTF-8 bytes; resource.edit permits at most ~:D bytes."
                     (length octets)
                     *workspace-file-resource-maximum-bytes*)
             :tool-name "resource.edit"))
    octets))

(-> workspace-file--rename-overwriting-target (pathname pathname) null)
(defun workspace-file--rename-overwriting-target (source target)
  "Atomically replace exact TARGET with SOURCE without pathname defaulting."
  (platform-replace-file *platform* source target)
  nil)

(-> workspace-file--link-new-target (pathname pathname) null)
(defun workspace-file--link-new-target (source target)
  "Atomically publish SOURCE at the absent TARGET without overwriting a concurrently created file."
  (platform-publish-new-file *platform* source target)
  (when (probe-file source)
    (delete-file source))
  nil)

(defparameter *workspace-file-resource-publish-function*
  #'workspace-file--rename-overwriting-target
  "The function atomically replacing an observed workspace file.")

(defparameter *workspace-file-resource-create-function*
  #'workspace-file--link-new-target
  "The function atomically publishing an observed missing workspace file.")

(-> workspace-file--write-temporary
    (pathname pathname (simple-array (unsigned-byte 8) (*)))
    null)
(defun workspace-file--write-temporary (temporary target octets)
  "Write OCTETS, preserving TARGET permissions and cleaning up a failed new file."
  (let ((created-p  nil)
        (complete-p nil))
    (unwind-protect
         (progn
           (with-open-stream (stream (platform-open-new-file *platform* temporary))
             (setf created-p t)
             (write-sequence octets stream)
             (finish-output stream))
           (handler-case
               (platform-copy-file-permissions *platform* target temporary)
             (platform-error ()
               nil))
           (setf complete-p t))
      (when (and created-p (not complete-p) (probe-file temporary))
        (delete-file temporary))))
  nil)

(-> workspace-file--same-observation-p
    (workspace-file-observation workspace-file-observation)
    boolean)
(defun workspace-file--same-observation-p (left right)
  "Return true when LEFT and RIGHT represent the same exact filesystem state."
  (and (eq (workspace-file-observation-kind left)
           (workspace-file-observation-kind right))
       (string= (resource-observation-revision left)
                (resource-observation-revision right))
       (string= (resource-observation-content left)
                (resource-observation-content right))))

(-> workspace-file--signal-stale
    (workspace-file-resource workspace-file-observation
     &optional (option workspace-file-observation))
    nil)
(defun workspace-file--signal-stale (resource expected &optional actual)
  "Signal that RESOURCE no longer matches EXPECTED, optionally reporting ACTUAL."
  (error 'resource-revision-stale
         :uri               (resource-uri resource)
         :expected-revision (resource-observation-revision expected)
         :actual-revision   (and actual
                                 (resource-observation-revision actual))))

(-> workspace-file--publish
    (workspace-file-resource tool-context
     &key (:base-observation workspace-file-observation) (:content string))
    workspace-file-observation)
(defun workspace-file--publish (resource context &key base-observation content)
  "Stage and publish CONTENT after an immediate exact BASE-OBSERVATION check."
  (workspace-file-check-path resource context)
  (let ((path   (workspace-file-resource-pathname resource))
        (octets (workspace-file--replacement-octets content)))
    (platform-ensure-directories-exist *platform* path)
    (handler-case
        (cl-hashline:publish-edit
         :base    base-observation
         :content content
         :stage   (lambda (replacement)
                    (declare (ignore replacement))
                    (let ((temporary (workspace-file--temporary-path path)))
                      (workspace-file--write-temporary temporary path octets)
                      temporary))
         :observe (lambda () (workspace-file--observe-path resource context))
         :same-p  #'workspace-file--same-observation-p
         :publish (lambda (temporary)
                      (workspace-file-check-path resource context)
                    (handler-case
                        (funcall
                         (if (eq (workspace-file-observation-kind base-observation)
                                 ':missing)
                             *workspace-file-resource-create-function*
                             *workspace-file-resource-publish-function*)
                         temporary path)
                      (platform-error (condition)
                        (if (and (eq (workspace-file-observation-kind base-observation)
                                     ':missing)
                                 (eq (platform-error-reason condition) ':exists))
                            (workspace-file--signal-stale resource base-observation)
                            (error condition))))
                    (workspace-file--observe-path resource context))
         :cleanup (lambda (temporary)
                    (when (probe-file temporary)
                      (delete-file temporary)))
         :verify  (lambda (published replacement)
                    (and (eq (workspace-file-observation-kind published) ':file)
                         (string= replacement
                                  (resource-observation-content published)))))
      (cl-hashline:stale-revision (condition)
        (workspace-file--signal-stale
         resource base-observation (cl-hashline:stale-revision-actual condition)))
      (cl-hashline:hashline-error (condition)
        (error 'tool-error :message   (princ-to-string condition)
                           :tool-name "resource.edit")))))

(defmethod resource-apply-operations
    ((resource workspace-file-resource) (context tool-context) &key base-revision operations)
  "Serialize filesystem edits around the shared text edit pipeline."
  (declare (ignore base-revision operations))
  (with-recursive-lock-held (*workspace-file-mutation-lock*)
    (call-next-method)))

(defmethod text-resource-edit-state ((resource workspace-file-resource) conversation revision)
  "Include guarded workspace proposal baselines when locating a file revision."
  (workspace-file--find-observation-state conversation (resource-uri resource) revision))

(defmethod text-resource-check-edit-base
    ((resource workspace-file-resource) (context tool-context) (base workspace-file-observation))
  "Check the exact filesystem state and refuse directory mutation."
  (let ((current (workspace-file--observe-path resource context)))
    (unless (workspace-file--same-observation-p current base)
      (workspace-file--signal-stale resource base current)))
  (when (eq (workspace-file-observation-kind base) ':directory)
    (error 'tool-error :message "Workspace directories are read-only resources." :tool-name "resource.edit")))

(defmethod text-resource-commit
    ((resource workspace-file-resource) (context tool-context) &key base content)
  "Publish through the existing filesystem staging and revision checks."
  (workspace-file--publish resource context :base-observation base :content content))

(defmethod text-resource-read-start ((resource workspace-file-resource) &key start-line line-count total-lines)
  "Show the last file window, with a note, when the requested start is past EOF."
  (declare (ignore resource))
  (if (and (plusp total-lines) (> start-line total-lines))
      (let ((last-start (max 1 (1+ (- total-lines line-count)))))
        (resource-tool-note "Start line ~D is past the end of the ~D-line resource; showing lines ~D-~D."
                            start-line total-lines last-start total-lines)
        last-start)
      start-line))

(defmethod text-resource-observation-annotations ((observation workspace-file-observation))
  "Describe the filesystem snapshot kind."
  (format nil "Kind: ~(~A~)~%" (workspace-file-observation-kind observation)))

(defmethod text-resource-source-pathname ((observation workspace-file-observation))
  "Use the observed file pathname for source delimiter annotations."
  (getf (resource-observation-metadata observation) :pathname))

(defmethod text-resource-read-tool-result ((observation workspace-file-observation) content &key details)
  "Present file windows and retain a missing path's observation for later edits."
  (case (workspace-file-observation-kind observation)
    (:missing
     (let* ((uri   (resource-observation-uri observation))
            (cause (format nil "No file exists at ~A" uri)))
       (tool-failure (format nil "~A; resource.edit can create it at the revision below.~%~A" cause content)
                     :code    ':not-found
                     :details (list :resource-read (list :cause (resource-tool--detail-text cause))))))
    (:file
     (tool-success content :details details))
    (otherwise
     (tool-success content))))


;;;; -- Resource Tool Methods --

(-> workspace-file--call-with-authorized-access
    (workspace-file-resource tool-context (member :read :edit)
     &key (:function function))
    t)
(defun workspace-file--call-with-authorized-access
    (resource context operation &key function)
  "Call FUNCTION after authorizing out-of-root OPERATION on RESOURCE."
  (let* ((path  (workspace-file-resource-pathname resource))
         (roots (workspace-file-resource-access-roots resource context)))
    (tool-path-blacklist-check path :tool-name (ecase operation
                                                (:read
                                                 "resource.read")
                                                (:edit
                                                 "resource.edit")))
    (if (workspace-tool--read-path-allowed-p path roots)
        (funcall function)
        (let ((tool-name
                (ecase operation
                  (:read
                   "resource.read")
                  (:edit
                   "resource.edit"))))
          (unless (workspace-tool-authorize-outside-path context path tool-name)
            (error 'tool-error
                   :message
                   (format nil "~A requires full-access approval for path ~A outside the workspace and source roots."
                           tool-name path)
                   :code      ':permission-denied
                   :tool-name tool-name))
          (let ((*workspace-tool-readable-roots* (cons path roots)))
            (funcall function))))))

(defmethod resource-observe :around
    ((resource workspace-file-resource) (context tool-context))
  "Authorize an out-of-root workspace RESOURCE for every observer, not only resource.read.

Tools such as rlm.map observe resources directly. An enclosing authorization
already admits the path to the readable roots, so it is not asked again."
  (workspace-file--call-with-authorized-access
   resource context ':read :function (lambda () (call-next-method))))

(defmethod resource-whole-text :around
    ((resource workspace-file-resource) (context tool-context) &key maximum-bytes tool-name)
  "Authorize an out-of-root workspace RESOURCE before reading it whole."
  (declare (ignore maximum-bytes tool-name))
  (workspace-file--call-with-authorized-access
   resource context ':read :function (lambda () (call-next-method))))

(defmethod resource-tool-read :around
    ((resource workspace-file-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Authorize out-of-root workspace RESOURCE reads before inspection."
  (declare (ignore tool arguments))
  (workspace-file--call-with-authorized-access
   resource context ':read :function (lambda () (call-next-method))))

(defmethod resource-tool-edit :around
    ((resource workspace-file-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Authorize out-of-root workspace RESOURCE writes before inspection or mutation."
  (declare (ignore tool arguments))
  (workspace-file--call-with-authorized-access
   resource context ':edit :function (lambda () (call-next-method))))

(defmethod resource-tool-read
    ((resource workspace-file-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Serialize filesystem observation and window registration."
  (declare (ignore tool arguments))
  (with-recursive-lock-held (*workspace-file-mutation-lock*)
    (call-next-method)))
