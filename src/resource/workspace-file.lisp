(in-package #:autolith)

;;;; -- Workspace File Resources --

(defparameter *workspace-file-resource-maximum-observations* 16
  "The transient resource observations retained by one conversation.")

(defparameter *workspace-file-resource-maximum-bytes* (* 4 1024 1024)
  "The largest exact workspace-file snapshot retained for revision-gated editing.")

(defparameter *workspace-file-resource-maximum-retained-bytes* (* 16 1024 1024)
  "The maximum UTF-8 bytes retained by workspace observations per conversation.")

(defparameter *workspace-file-resource-default-line-count* 400
  "The lines requested by resource.read when no explicit count is supplied.")

(defparameter *workspace-file-resource-maximum-line-count* 1000
  "The largest line window accepted by one resource.read call.")

(defparameter *workspace-file-resource-maximum-directory-entries* 1000
  "The most directory entries inspected for one workspace resource snapshot.")

(defparameter *workspace-file-resource-maximum-result-characters* 7600
  "The maximum characters constructed for one resource tool result.")

(defparameter *workspace-file-resource-anchor-maximum-offset* 3
  "The largest line-number transcription offset corrected by a hash anchor.")

(defvar *workspace-file-resource-digest-key* (random-data 16)
  "The process-local key used to identify exact workspace-file snapshots.")

(defclass workspace-file-resource (resource)
  ((pathname
    :initarg :pathname
    :reader workspace-file-resource-pathname
    :type pathname
    :documentation "The canonical pathname represented by this resource."))
  (:documentation "An existing or potential file addressed by a workspace URI."))

(defclass workspace-file-resolver (resource-resolver)
  ()
  (:documentation "Resolve workspace: URIs against the configured working directory."))

(-> workspace-file-resource-access-roots
    (workspace-file-resource tool-context)
    list)
(defgeneric workspace-file-resource-access-roots (resource context)
  (:documentation
   "Return roots RESOURCE may access without command authorization under CONTEXT."))

(defmethod workspace-file-resource-access-roots
    ((resource workspace-file-resource) (context tool-context))
  "Return CONTEXT's active workspace roots for RESOURCE."
  (declare (ignore resource))
  (workspace-tool-readable-roots context))

(defclass workspace-file-observation (resource-observation)
  ((kind
    :initarg :kind
    :reader workspace-file-observation-kind
    :type (member :file :directory :missing)
    :documentation "Whether the observation represents a file, directory, or missing path.")
   (stored-lines
    :initarg :lines
    :initform nil
    :reader workspace-file-observation-stored-lines
    :type (option vector)
    :documentation "Optional pre-split logical lines supplied by tests or callers.")
   (line-ending
    :initarg :line-ending
    :reader workspace-file-observation-line-ending
    :type string
    :documentation "The LF or CRLF sequence used when reconstructing edited content.")
   (final-newline-p
    :initarg :final-newline-p
    :reader workspace-file-observation-final-newline-p
    :type boolean
    :documentation "Whether the exact observed snapshot ended in a line ending."))
  (:documentation "A complete transient UTF-8 workspace-file snapshot."))

(-> workspace-file-observation-lines (workspace-file-observation) vector)
(defgeneric workspace-file-observation-lines (observation)
  (:documentation "Return OBSERVATION's logical lines for the current operation."))

(defmethod workspace-file-observation-lines
    ((observation workspace-file-observation))
  "Return supplied logical lines or split OBSERVATION's exact content lazily."
  (or (workspace-file-observation-stored-lines observation)
      (text--split-lines (resource-observation-content observation))))

(-> workspace-file--observation-retained-bytes
    (workspace-file-observation)
    (integer 0))
(defun workspace-file--observation-retained-bytes (observation)
  "Return the UTF-8 bytes retained by OBSERVATION's exact snapshot."
  (length (utf8-string-to-octets
           (resource-observation-content observation))))

(defclass workspace-file-observation-state (resource-observation-state)
  ((visible-ranges
    :initarg :visible-ranges
    :accessor workspace-file-observation-state-visible-ranges
    :type structlisp:integer-interval-set
    :documentation "Half-open one-based ranges fully shown to the model."))
  (:documentation "One conversation-local model observation of a workspace file."))

(defmethod initialize-instance :after
    ((state workspace-file-observation-state) &key)
  "Convert the renderer's inclusive ranges into an interval set."
  (let ((ranges (workspace-file-observation-state-visible-ranges state)))
    (unless (typep ranges 'structlisp:integer-interval-set)
      (setf (workspace-file-observation-state-visible-ranges state)
            (structlisp:make-integer-interval-set
             :intervals (mapcar (lambda (range)
                                  (cons (first range) (1+ (second range))))
                                ranges))))))

(defmethod resource-observation-state-family-and-key
    ((observation workspace-file-observation))
  "Return the workspace-file state family and exact content snapshot key."
  (values 'workspace-file-observation-state
          (list (resource-observation-uri observation)
                (resource-observation-revision observation)
                (resource-observation-content observation))))

(defmethod resource-observation-state-weight
    (alias (state workspace-file-observation-state))
  "Return STATE's retained workspace snapshot bytes."
  (declare (ignore alias))
  (workspace-file--observation-retained-bytes
   (resource-observation-state-observation state)))


(defmethod resource-observation-state-maximum
    ((state workspace-file-observation-state))
  "Return the configured workspace-file observation limit."
  (declare (ignore state))
  *workspace-file-resource-maximum-observations*)


(defmethod resource-observation-state-trim-storage
    ((conversation conversation) (state workspace-file-observation-state))
  "Evict oldest workspace observations until retained UTF-8 strings fit."
  (declare (ignore state))
  (let ((states (conversation-resource-observations conversation)))
    (loop while (> (fifo-cache-total-weight states)
                   *workspace-file-resource-maximum-retained-bytes*)
          do (fifo-cache-delete-first-if
              (lambda (alias candidate)
                (declare (ignore alias))
                (typep candidate 'workspace-file-observation-state))
              states)))
  nil)


;;;; -- URI Resolution --

(defmethod resource-resolver-child-safe-p
    ((resolver workspace-file-resolver) context)
  "Permit child agents to resolve files through the existing workspace boundary."
  (declare (ignore resolver context))
  t)

(-> workspace-file--uri-safe-octet-p ((unsigned-byte 8)) boolean)
(defun workspace-file--uri-safe-octet-p (octet)
  "Return true when OCTET may appear literally in a workspace URI identifier."
  (and (or (and (>= octet (char-code #\a)) (<= octet (char-code #\z)))
           (and (>= octet (char-code #\A)) (<= octet (char-code #\Z)))
           (and (>= octet (char-code #\0)) (<= octet (char-code #\9)))
           (find octet
                 (map 'list #'char-code "-._~/")
                 :test #'=))
       t))

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
             :message (format nil "Could not inspect workspace resource ~A: ~A"
                              path condition)
             :tool-name "resource.read"))))

(-> workspace-file--read-content
    (pathname &optional (option tool-context))
    string)
(defun workspace-file--read-content (path &optional context)
  "Read PATH as a bounded, stable UTF-8 workspace resource."
  (file--read-bounded-utf-8
   path
   :maximum-bytes *workspace-file-resource-maximum-bytes*
   :tool-name "resource.read"
   :description "Workspace resource snapshot"
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
             :message (format nil "Could not inspect directory entry ~A beneath ~A: ~A"
                              name directory condition)
             :tool-name "resource.read"))))

(-> workspace-file--directory-content (pathname) string)
(defun workspace-file--directory-content (path)
  "Return a bounded sorted directory listing without opening its entries."
  (let ((entries nil)
        (truncated-p nil))
    (multiple-value-bind (names more-p)
        (handler-case
            (platform-list-directory
             *platform* path
             :limit *workspace-file-resource-maximum-directory-entries*)
          (platform-error (condition)
            (error 'tool-error
                   :message (format nil "Could not list workspace directory ~A: ~A"
                                    path condition)
                   :tool-name "resource.read")))
      (setf truncated-p more-p)
      (dolist (name names)
        (let ((row (workspace-file--directory-entry-row path name)))
          (when row
            (push (list name row) entries)))))
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
    (let* ((marker (format nil "[directory listing truncated]~%"))
           (limit *workspace-file-resource-maximum-result-characters*)
           (row-budget (max 0 (- limit (length marker))))
           (used 0))
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

(-> workspace-file--observe-path
    (workspace-file-resource tool-context)
    workspace-file-observation)
(defun workspace-file--observe-path (resource context)
  "Return a complete observation of RESOURCE's current filesystem state."
  (let* ((path (workspace-file-resource-pathname resource))
         (kind (workspace-file--path-kind path)))
    (when (eq kind ':other)
      (error 'tool-error
             :message (format nil "Resource ~A is not a regular file or directory."
                              (resource-uri resource))
             :tool-name "resource.read"))
    (let ((content
            (case kind
              (:file
               (workspace-file--read-content path context))
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


;;;; -- Conversation Observation State --

(defmethod resource-observation-state-merge
    ((state workspace-file-observation-state)
     (observation workspace-file-observation) &rest initargs)
  "Merge newly visible inclusive line ranges into STATE."
  (declare (ignore observation))
  (dolist (range (getf initargs ':visible-ranges))
    (structlisp:integer-interval-set-add
     (workspace-file-observation-state-visible-ranges state)
     (first range) (1+ (second range))))
  state)


(-> workspace-file--find-observation-state
    (conversation non-empty-string non-empty-string)
    workspace-file-observation-state)
(defun workspace-file--find-observation-state (conversation uri alias)
  "Return CONVERSATION's exact URI observation ALIAS or signal stale revision."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           'workspace-file-observation-state)))
    (unless (and state
                 (string= uri
                          (resource-observation-uri
                           (resource-observation-state-observation state))))
      (error 'resource-revision-stale
             :uri               uri
             :expected-revision alias
             :actual-revision   nil))
    state))


;;;; -- Bounded Observation Rendering --

(-> workspace-file--format-ranges (structlisp:integer-interval-set) string)
(defun workspace-file--format-ranges (ranges)
  "Return RANGES in concise model-visible inclusive form."
  (if (structlisp:integer-interval-set-empty-p ranges)
      "none"
      (format nil "~{~A~^, ~}"
              (loop for range across (structlisp:integer-interval-set->vector ranges)
                    for start = (structlisp:integer-interval-start range)
                    for end = (1- (structlisp:integer-interval-end range))
                    collect (if (= start end)
                                (format nil "~D" start)
                                (format nil "~D-~D" start end))))))

(-> workspace-file--elisions
    ((integer 0) structlisp:integer-interval-set boolean) list)
(defun workspace-file--elisions (total-lines ranges truncated-p)
  "Return explicit gaps omitted from the cumulative visible RANGES."
  (let* ((complete (structlisp:make-integer-interval-set
                    :intervals (list (cons 1 (1+ total-lines)))))
         (gaps (structlisp:integer-interval-set-difference complete ranges))
         (elisions
            (loop for gap across (structlisp:integer-interval-set->vector gaps)
                 for start = (structlisp:integer-interval-start gap)
                 for end = (1- (structlisp:integer-interval-end gap))
                 collect (format nil "~D-~D ~A" start end
                                 (cond
                                   ((= end total-lines)
                                    "after")
                                   ((= start 1)
                                    "before")
                                   (t
                                    "between visible ranges"))))))
    (when truncated-p
      (setf elisions (append elisions (list "current result truncated"))))
    elisions))

(-> workspace-file--read-result
    (workspace-file-observation-state string (integer 0) boolean)
    string)
(defun workspace-file--read-result (state body total-lines truncated-p)
  "Return one explicit model-facing resource observation result."
  (let* ((observation (resource-observation-state-observation state))
         (ranges (workspace-file-observation-state-visible-ranges state))
         (elisions (workspace-file--elisions total-lines ranges truncated-p)))
    (format nil "URI: ~A~%Revision: ~A~%Kind: ~(~A~)~%Visible lines: ~A of ~D~%Elided: ~A~%Content:~%~A"
            (resource-observation-uri observation)
            (resource-observation-state-alias state)
            (workspace-file-observation-kind observation)
            (workspace-file--format-ranges ranges)
            total-lines
            (if elisions (format nil "~{~A~^; ~}" elisions) "none")
            body)))


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
  "Atomically publish SOURCE as absent TARGET without overwriting a race."
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
  (let ((created-p nil)
        (complete-p nil))
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction ':output
                                   :if-exists ':error
                                   :if-does-not-exist ':create
                                   :element-type '(unsigned-byte 8))
             (setf created-p t)
             (write-sequence octets stream)
             (finish-output stream))
           (ignore-errors
             (platform-copy-file-permissions *platform* target temporary))
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
         :uri (resource-uri resource)
         :expected-revision (resource-observation-revision expected)
         :actual-revision (and actual
                               (resource-observation-revision actual))))


(-> workspace-file--publish
    (workspace-file-resource tool-context workspace-file-observation string)
    workspace-file-observation)
(defun workspace-file--publish (resource context base-observation content)
  "Stage and publish CONTENT after an immediate exact BASE-OBSERVATION check."
  (let ((path (workspace-file-resource-pathname resource))
        (octets (workspace-file--replacement-octets content)))
    (ensure-directories-exist path)
    (handler-case
        (cl-hashline:publish-edit
         :base base-observation
         :content content
         :stage (lambda (replacement)
                  (declare (ignore replacement))
                  (let ((temporary (workspace-file--temporary-path path)))
                    (workspace-file--write-temporary temporary path octets)
                    temporary))
         :observe (lambda () (workspace-file--observe-path resource context))
         :same-p #'workspace-file--same-observation-p
         :publish (lambda (temporary)
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
         :verify (lambda (published replacement)
                   (and (eq (workspace-file-observation-kind published) ':file)
                        (string= replacement
                                 (resource-observation-content published)))))
      (cl-hashline:stale-revision (condition)
        (workspace-file--signal-stale
         resource base-observation (cl-hashline:stale-revision-actual condition)))
      (cl-hashline:hashline-error (condition)
        (error 'tool-error :message (princ-to-string condition)
                           :tool-name "resource.edit")))))

(-> workspace-file--operation-window (list (integer 0))
    (values (integer 1) (integer 1)))
(defun workspace-file--operation-window (operations total-lines)
  "Return a bounded nearby window covering OPERATIONS in the resulting file."
  (let* ((first-line (reduce #'min operations :key (lambda (op) (getf op :start))))
         (last-line (reduce #'max operations :key (lambda (op) (getf op :end))))
         (start (max 1 (- first-line 2)))
         (count (max 1 (min 120 (+ (- last-line start) 6)))))
    (values (if (zerop total-lines) 1 (min start total-lines)) count)))


;;;; -- Structured Operation Adapter --

(-> workspace-file--json-operation (json-object) list)
(defun workspace-file--json-operation (operation)
  "Translate JSON OPERATION fields into library keyword data."
  (unless (hash-table-p operation)
    (error 'tool-error :message "Resource edit operations must be JSON objects."
                       :tool-name "resource.edit"))
  (let ((fields '(("op" . :op) ("line" . :line)
                  ("start-line" . :start-line) ("end-line" . :end-line)
                  ("anchor" . :anchor) ("start-anchor" . :start-anchor)
                  ("end-anchor" . :end-anchor) ("content" . :content)))
        (result nil))
    (maphash
     (lambda (key value)
       (let ((field (assoc key fields :test #'equal)))
         (unless field
           (error 'tool-error
                  :message (format nil "Unsupported resource edit field ~S." key)
                  :tool-name "resource.edit"))
         (when (eq (rest field) ':op)
           (setf value
                 (or (and (stringp value)
                          (rest (assoc value
                                       '(("replace-lines" . :replace-lines)
                                         ("delete-lines" . :delete-lines)
                                         ("insert-before" . :insert-before)
                                         ("insert-after" . :insert-after)
                                         ("replace-empty" . :replace-empty))
                                       :test #'string=)))
                     value)))
         (setf result (list* (rest field) value result))))
     operation)
    result))

(defmethod resource-apply-operations
    ((resource workspace-file-resource) (context tool-context)
     &key base-revision operations)
  "Apply structured original-line OPERATIONS to RESOURCE at BASE-REVISION."
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let* ((state (workspace-file--find-observation-state
                       conversation (resource-uri resource) base-revision))
               (base (resource-observation-state-observation state))
               (current (workspace-file--observe-path resource context)))
          (unless (workspace-file--same-observation-p current base)
            (workspace-file--signal-stale resource base current))
          (when (eq (workspace-file-observation-kind base) ':directory)
            (error 'tool-error :message "Workspace directories are read-only resources."
                               :tool-name "resource.edit"))
          (handler-case
              (multiple-value-bind (content normalized)
                  (cl-hashline:edit-text
                   (resource-observation-content base)
                   (if (listp operations)
                       (mapcar #'workspace-file--json-operation operations)
                       operations)
                   :visible-lines (workspace-file-observation-state-visible-ranges state)
                   :split-lines #'text--split-lines
                   :anchor-maximum-offset *workspace-file-resource-anchor-maximum-offset*)
                (values (workspace-file--publish resource context base content)
                        normalized))
            (cl-hashline:hashline-error (condition)
              (error 'tool-error :message (princ-to-string condition)
                                 :tool-name "resource.edit"))))))))

;;;; -- Resource Tool Methods --

(-> workspace-file--call-with-authorized-access
    (workspace-file-resource tool-context (member :read :edit) function)
    t)
(defun workspace-file--call-with-authorized-access
    (resource context operation function)
  "Call FUNCTION after authorizing out-of-root OPERATION on RESOURCE."
  (let* ((path  (workspace-file-resource-pathname resource))
         (roots (workspace-file-resource-access-roots resource context)))
    (if (workspace-tool--read-path-allowed-p path roots)
        (funcall function)
        (let ((tool-name
                (ecase operation
                  (:read "resource.read")
                  (:edit "resource.edit"))))
          (unless (workspace-tool-authorize-outside-path context path tool-name)
            (error 'tool-error
                   :message
                   (format nil "~A requires full-access approval for path ~A outside the workspace and source roots."
                           tool-name path)
                   :tool-name tool-name))
          (let ((*workspace-tool-readable-roots* (cons path roots)))
            (funcall function))))))

(defmethod resource-tool-read :around
    ((resource workspace-file-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Authorize out-of-root workspace RESOURCE reads before inspection."
  (declare (ignore tool arguments))
  (workspace-file--call-with-authorized-access
   resource context ':read (lambda () (call-next-method))))

(defmethod resource-tool-edit :around
    ((resource workspace-file-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Authorize out-of-root workspace RESOURCE writes before inspection or mutation."
  (declare (ignore tool arguments))
  (workspace-file--call-with-authorized-access
   resource context ':edit (lambda () (call-next-method))))


(defmethod resource-tool-read
    ((resource workspace-file-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read a bounded workspace resource window and establish its edit observation."
  (declare (ignore tool))
  (let ((start-line
          (max 1 (or (workspace-tool-integer-argument
                      arguments "start-line" :fallback 1)
                     1)))
        (line-count
          (min *workspace-file-resource-maximum-line-count*
               (max 1 (or (workspace-tool-integer-argument
                           arguments "line-count"
                           :fallback *workspace-file-resource-default-line-count*)
                          *workspace-file-resource-default-line-count*)))))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (let* ((observation (resource-observe resource context))
             (lines       (workspace-file-observation-lines observation))
             (total-lines (length lines)))
        (when (and (plusp total-lines) (> start-line total-lines))
          (error 'tool-error
                 :message (format nil "Start line ~D is beyond the resource's ~D lines. Request a valid window."
                                  start-line total-lines)
                 :tool-name "resource.read"))
        (multiple-value-bind (body visible-ranges last-line truncated-p)
            (text--numbered-line-window
             lines
             start-line
             line-count
             *workspace-file-resource-maximum-result-characters*
             :line-anchor-function #'cl-hashline:line-anchor)
          (declare (ignore last-line))
          (when (and (plusp total-lines) (null visible-ranges))
            (error 'tool-error
                   :message (format nil "Line ~D exceeds the resource.read result limit and was not observed. Use search.content or shell.run for bounded inspection."
                                    start-line)
                   :tool-name "resource.read"))
          (let ((state
                   (resource-observation-state-ensure
                    (tool-context-conversation context) observation
                    :visible-ranges visible-ranges)))
            (tool-success
             (workspace-file--read-result state body total-lines truncated-p))))))))

(defmethod resource-tool-edit
    ((resource workspace-file-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply structured revision-gated operations and return a refreshed observation."
  (declare (ignore tool))
  (let* ((uri (tool-argument arguments "uri" :required t))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error
             :message "Resource edit base-revision must be a non-empty string."
             :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (plusp (length operation-array)))
      (error 'tool-error
             :message "Resource edit operations must be a non-empty JSON array."
             :tool-name "resource.edit"))
    (let ((operations (coerce operation-array 'list)))
      (handler-case
        (multiple-value-bind (observation normalized)
            (resource-apply-operations resource context
                                       :base-revision base-revision
                                       :operations operations)
          (let ((lines (workspace-file-observation-lines observation)))
            (multiple-value-bind (start-line line-count)
                (workspace-file--operation-window normalized (length lines))
              (multiple-value-bind (body visible-ranges last-line truncated-p)
                   (text--numbered-line-window
                    lines
                    start-line
                    line-count
                    *workspace-file-resource-maximum-result-characters*
                    :line-anchor-function #'cl-hashline:line-anchor)
                (declare (ignore last-line))
                (let* ((state
                          (resource-observation-state-ensure
                           (tool-context-conversation context) observation
                           :visible-ranges visible-ranges))
                       (result-content
                         (format nil "Applied ~{~A~^; ~}.~%~A"
                                 (mapcar
                                  (lambda (operation) (getf operation :summary))
                                  normalized)
                                 (workspace-file--read-result
                                  state body (length lines) truncated-p))))
                  (tool-success
                   (lisp-source-edit-result-content
                    result-content
                    (workspace-file-resource-pathname resource)
                    (resource-observation-content observation))))))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri)))))))
