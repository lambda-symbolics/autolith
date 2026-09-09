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
  (length (sb-ext:string-to-octets
           (resource-observation-content observation)
           :external-format ':utf-8)))

(defclass workspace-file-observation-state (resource-observation-state)
  ((visible-ranges
    :initarg :visible-ranges
    :accessor workspace-file-observation-state-visible-ranges
    :type list
    :documentation "Inclusive original line ranges fully shown to the model."))
  (:documentation "One conversation-local model observation of a workspace file."))

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
            (configuration-working-directory
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
        '(:read :edit))))


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
          (workspace-tool-path context (uiop:native-namestring path))))))

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

(-> workspace-file--line-ending (string) string)
(defun workspace-file--line-ending (content)
  "Return the line-ending style to preserve for CONTENT."
  (if (search (format nil "~C~C" #\Return #\Newline) content)
      (format nil "~C~C" #\Return #\Newline)
      (string #\Newline)))

(-> workspace-file--mixed-line-endings-p (string) boolean)
(defun workspace-file--mixed-line-endings-p (content)
  "Return true when CONTENT contains both LF and CRLF line endings."
  (let ((crlf-p nil)
        (lf-p nil))
    (loop for position = (position #\Newline content)
            then (position #\Newline content :start (1+ position))
          while position
          do (if (and (plusp position)
                      (char= (char content (1- position)) #\Return))
                 (setf crlf-p t)
                 (setf lf-p t)))
    (and crlf-p lf-p)))

(-> workspace-file--final-newline-p (string) boolean)
(defun workspace-file--final-newline-p (content)
  "Return true when CONTENT ends in LF, including CRLF."
  (and (plusp (length content))
       (char= (char content (1- (length content))) #\Newline)))

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
                     :line-ending     (workspace-file--line-ending content)
                     :final-newline-p (workspace-file--final-newline-p content)))))

(defmethod resource-observe
    ((resource workspace-file-resource) (context tool-context))
  "Observe RESOURCE as a complete exact filesystem snapshot under CONTEXT."
  (workspace-file--observe-path resource context))


;;;; -- Conversation Observation State --

(-> workspace-file--merge-visible-ranges (list list) list)
(defun workspace-file--merge-visible-ranges (left right)
  "Return sorted inclusive LEFT and RIGHT ranges with overlaps merged."
  (let ((ranges (sort (append (copy-tree left) (copy-tree right))
                      #'< :key #'first))
        (result nil))
    (dolist (range ranges)
      (let ((previous (first result)))
        (if (and previous (<= (first range) (1+ (second previous))))
            (setf (second previous) (max (second previous) (second range)))
            (push (copy-list range) result))))
    (nreverse result)))

(defmethod resource-observation-state-merge
    ((state workspace-file-observation-state)
     (observation workspace-file-observation) &rest initargs)
  "Merge newly visible line ranges into equivalent workspace-file STATE."
  (declare (ignore observation))
  (setf (workspace-file-observation-state-visible-ranges state)
        (workspace-file--merge-visible-ranges
         (workspace-file-observation-state-visible-ranges state)
         (getf initargs ':visible-ranges)))
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

(-> workspace-file--line-visible-p
    (workspace-file-observation-state (integer 1))
    boolean)
(defun workspace-file--line-visible-p (state line)
  "Return true when LINE was fully visible under STATE's exact observation."
  (and (some (lambda (range)
               (<= (first range) line (second range)))
             (workspace-file-observation-state-visible-ranges state))
       t))

(-> workspace-file--range-visible-p
    (workspace-file-observation-state (integer 1) (integer 1))
    boolean)
(defun workspace-file--range-visible-p (state start-line end-line)
  "Return true when every line from START-LINE through END-LINE was visible."
  (and (some (lambda (range)
               (and (<= (first range) start-line)
                    (<= end-line (second range))))
             (workspace-file-observation-state-visible-ranges state))
       t))


(-> workspace-file--line-anchor (string) string)
(defun workspace-file--line-anchor (line)
  "Return LINE's stable four-hex-character edit anchor."
  (let ((digest
          (ironclad:digest-sequence
           ':sha256
           (sb-ext:string-to-octets line :external-format ':utf-8))))
    (format nil "~2,'0X~2,'0X" (aref digest 0) (aref digest 1))))


;;;; -- Bounded Observation Rendering --

(-> workspace-file--format-ranges (list) string)
(defun workspace-file--format-ranges (ranges)
  "Return RANGES in concise model-visible inclusive form."
  (if ranges
      (format nil "~{~A~^, ~}"
              (mapcar (lambda (range)
                        (if (= (first range) (second range))
                            (format nil "~D" (first range))
                            (format nil "~D-~D" (first range) (second range))))
                      ranges))
      "none"))

(-> workspace-file--elisions ((integer 0) list boolean) list)
(defun workspace-file--elisions (total-lines ranges truncated-p)
  "Return explicit gaps omitted from the cumulative visible RANGES."
  (let ((cursor 1)
        (elisions nil))
    (dolist (range ranges)
      (when (< cursor (first range))
        (push (format nil "~D-~D~:[ between visible ranges~; before~]"
                      cursor (1- (first range)) (= cursor 1))
              elisions))
      (setf cursor (1+ (second range))))
    (when (<= cursor total-lines)
      (push (format nil "~D-~D after" cursor total-lines) elisions))
    (when truncated-p
      (push "current result truncated" elisions))
    (nreverse elisions)))

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


;;;; -- Structured Operations --

(-> workspace-file--operation-name (json-object) string)
(defun workspace-file--operation-name (operation)
  "Return and validate OPERATION's operation name."
  (let ((name (tool-argument operation "op" :required t)))
    (unless (and (stringp name)
                 (member name
                         '("replace-lines" "insert-before" "insert-after"
                           "delete-lines" "replace-empty")
                         :test #'string=))
      (error 'tool-error
             :message (format nil "Unknown resource edit operation ~S." name)
             :tool-name "resource.edit"))
    name))

(-> workspace-file--required-positive-line (json-object string) (integer 1))
(defun workspace-file--required-positive-line (operation name)
  "Return required positive integer line NAME from OPERATION."
  (let ((value (tool-argument operation name :required t)))
    (unless (and (integerp value) (plusp value))
      (error 'tool-error
             :message (format nil "Resource edit field ~S must be a positive integer."
                              name)
             :tool-name "resource.edit"))
    value))

(-> workspace-file--operation-content (json-object string boolean) string)
(defun workspace-file--operation-content (operation name non-empty-p)
  "Return string content NAME from OPERATION, optionally requiring non-empty text."
  (let ((value (tool-argument operation name :required t)))
    (unless (and (stringp value)
                 (or (not non-empty-p) (plusp (length value))))
      (error 'tool-error
             :message (format nil "Resource edit field ~S must be ~:[a string~;a non-empty string~]."
                              name non-empty-p)
             :tool-name "resource.edit"))
    value))


(-> workspace-file--optional-anchor (json-object string) (option string))
(defun workspace-file--optional-anchor (operation name)
  "Return optional normalized four-hex anchor NAME from OPERATION."
  (let ((value (tool-argument operation name)))
    (when value
      (unless (and (stringp value)
                   (= (length value) 4)
                   (every (lambda (character)
                            (digit-char-p character 16))
                          value))
        (error 'tool-error
               :message (format nil "Resource edit field ~S must be a four-character hexadecimal line anchor."
                                name)
               :tool-name "resource.edit"))
      (string-upcase value))))

(-> workspace-file--resolve-anchored-line
    (workspace-file-observation-state (integer 1) (option string) string)
    (integer 1))
(defun workspace-file--resolve-anchored-line (state requested-line anchor field-name)
  "Resolve REQUESTED-LINE through optional visible ANCHOR for FIELD-NAME."
  (unless anchor
    (return-from workspace-file--resolve-anchored-line requested-line))
  (let* ((observation (resource-observation-state-observation state))
         (lines (workspace-file-observation-lines observation))
         (candidates
           (loop for range in (workspace-file-observation-state-visible-ranges state)
                 append
                 (loop for line from (first range) to (second range)
                       when (string= anchor
                                     (workspace-file--line-anchor
                                      (aref lines (1- line))))
                         collect line)))
         (nearby
           (remove-if (lambda (line)
                        (> (abs (- line requested-line))
                           *workspace-file-resource-anchor-maximum-offset*))
                      candidates)))
    (cond
      ((null candidates)
       (error 'tool-error
              :message (format nil "Anchor ~A for ~A does not match any visible line under this revision. Reread the required window."
                               anchor field-name)
              :tool-name "resource.edit"))
      ((null nearby)
       (error 'tool-error
              :message (format nil "Anchor ~A for ~A matches only beyond the maximum correction offset of ~D lines. Reread the required window."
                               anchor field-name
                               *workspace-file-resource-anchor-maximum-offset*)
              :tool-name "resource.edit"))
      ((rest nearby)
       (error 'tool-error
              :message (format nil "Anchor ~A for ~A is ambiguous near line ~D; matching visible lines are ~{~D~^, ~}. Reread a narrower window."
                               anchor field-name requested-line nearby)
              :tool-name "resource.edit"))
      (t
       (first nearby)))))

(-> workspace-file--operation-extra-keys-p (json-object list) boolean)
(defun workspace-file--operation-extra-keys-p (operation allowed)
  "Return true when OPERATION contains a key outside ALLOWED."
  (loop for key being the hash-keys of operation
        thereis (not (member key allowed :test #'string=))))

(-> workspace-file--normalize-operation
    (json-object workspace-file-observation-state)
    list)
(defun workspace-file--normalize-operation (operation state)
  "Validate one JSON OPERATION against STATE and return its normalized plist."
  (unless (json-object-p operation)
    (error 'tool-error
           :message "Every resource edit operation must be a JSON object."
           :tool-name "resource.edit"))
  (let* ((name (workspace-file--operation-name operation))
         (observation (resource-observation-state-observation state)))
    (cond
      ((string= name "replace-empty")
       (when (workspace-file--operation-extra-keys-p
              operation '("op" "content"))
         (error 'tool-error
                :message "Operation replace-empty contains unsupported fields."
                :tool-name "resource.edit"))
       (unless (or (eq (workspace-file-observation-kind observation) ':missing)
                   (and (eq (workspace-file-observation-kind observation) ':file)
                        (zerop (length
                                (workspace-file-observation-lines observation)))))
         (error 'tool-error
                :message "Operation replace-empty is valid only for an observed missing resource or empty file."
                :tool-name "resource.edit"))
       (let ((content (workspace-file--operation-content operation "content" t)))
         (list :kind ':replace-empty
               :start 0
               :end 0
               :lines (coerce (text--split-lines content) 'list)
               :line-ending (workspace-file--line-ending content)
               :final-newline-p (workspace-file--final-newline-p content)
               :summary "replace-empty")))
      ((member name '("replace-lines" "delete-lines") :test #'string=)
       (let* ((requested-start-line
                (workspace-file--required-positive-line operation "start-line"))
              (requested-end-line
                (workspace-file--required-positive-line operation "end-line"))
              (start-line
                (workspace-file--resolve-anchored-line
                 state requested-start-line
                 (workspace-file--optional-anchor operation "start-anchor")
                 "start-line"))
              (end-line
                (workspace-file--resolve-anchored-line
                 state requested-end-line
                 (workspace-file--optional-anchor operation "end-anchor")
                 "end-line"))
              (replace-p (string= name "replace-lines"))
              (allowed (if replace-p
                           '("op" "start-line" "start-anchor"
                             "end-line" "end-anchor" "content")
                           '("op" "start-line" "start-anchor"
                             "end-line" "end-anchor"))))
         (when (workspace-file--operation-extra-keys-p operation allowed)
           (error 'tool-error
                  :message (format nil "Operation ~A contains unsupported fields." name)
                  :tool-name "resource.edit"))
         (unless (<= start-line end-line)
           (error 'tool-error
                  :message (format nil "Operation ~A has start-line after end-line." name)
                  :tool-name "resource.edit"))
         (unless (workspace-file--range-visible-p state start-line end-line)
           (error 'tool-error
                  :message (format nil "Operation ~A addresses lines ~D-~D that were not fully visible under this revision. Reread the required window."
                                   name start-line end-line)
                  :tool-name "resource.edit"))
         (list :kind (if replace-p ':replace ':delete)
               :start start-line
               :end end-line
               :lines (if replace-p
                          (coerce
                           (text--split-lines
                            (workspace-file--operation-content
                             operation "content" nil))
                           'list)
                          nil)
               :summary (format nil "~A ~D-~D" name start-line end-line))))
      (t
       (let* ((requested-line
                (workspace-file--required-positive-line operation "line"))
              (line
                (workspace-file--resolve-anchored-line
                 state requested-line
                 (workspace-file--optional-anchor operation "anchor")
                 "line"))
              (content (workspace-file--operation-content operation "content" t)))
         (when (workspace-file--operation-extra-keys-p
                operation '("op" "line" "anchor" "content"))
           (error 'tool-error
                  :message (format nil "Operation ~A contains unsupported fields." name)
                  :tool-name "resource.edit"))
         (unless (workspace-file--line-visible-p state line)
           (error 'tool-error
                  :message (format nil "Operation ~A anchors line ~D that was not visible under this revision. Reread the required window."
                                   name line)
                  :tool-name "resource.edit"))
         (list :kind (if (string= name "insert-before") ':insert-before ':insert-after)
               :start line
               :end line
               :lines (coerce (text--split-lines content) 'list)
               :summary (format nil "~A ~D" name line)))))))

(-> workspace-file--validate-operation-overlaps (list) null)
(defun workspace-file--validate-operation-overlaps (operations)
  "Reject operations whose original line intervals overlap or share an anchor."
  (loop for tail on operations
        for operation = (first tail)
        do
           (dolist (other (rest tail))
             (when (and (<= (getf operation :start) (getf other :end))
                        (<= (getf other :start) (getf operation :end)))
               (error 'tool-error
                      :message
                      (format nil "Resource edit operations overlap or ambiguously share original lines ~D-~D and ~D-~D."
                              (getf operation :start) (getf operation :end)
                              (getf other :start) (getf other :end))
                      :tool-name "resource.edit"))))
  nil)

(-> workspace-file--normalize-operations
    (list workspace-file-observation-state)
    list)
(defun workspace-file--normalize-operations (value state)
  "Validate operation list VALUE completely against STATE."
  (unless (and (listp value) value)
    (error 'tool-error
           :message "Resource edit operations must be a non-empty list."
           :tool-name "resource.edit"))
  (let ((operations
          (loop for operation in value
                collect (workspace-file--normalize-operation operation state))))
    (when (and (find ':replace-empty operations
                     :key (lambda (operation) (getf operation :kind)))
               (> (length operations) 1))
      (error 'tool-error
             :message "Operation replace-empty must be the only resource edit operation."
             :tool-name "resource.edit"))
    (workspace-file--validate-operation-overlaps operations)
    (sort operations #'< :key (lambda (operation) (getf operation :start)))))

(-> workspace-file--apply-normalized-operations (vector list) list)
(defun workspace-file--apply-normalized-operations (lines operations)
  "Apply normalized OPERATIONS to original LINES in one deterministic pass."
  (when (and (zerop (length lines))
             (eq (getf (first operations) :kind) ':replace-empty))
    (return-from workspace-file--apply-normalized-operations
      (copy-list (getf (first operations) :lines))))
  (let ((result nil)
        (line 1)
        (remaining operations)
        (total (length lines)))
    (loop while (<= line total)
          for operation = (first remaining)
          do
             (cond
               ((and operation (= line (getf operation :start)))
                (case (getf operation :kind)
                  (:insert-before
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result))
                   (push (aref lines (1- line)) result)
                   (incf line))
                  (:insert-after
                   (push (aref lines (1- line)) result)
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result))
                   (incf line))
                  ((:replace :delete)
                   (setf result (nconc (reverse (copy-list (getf operation :lines)))
                                       result)
                         line (1+ (getf operation :end)))))
                (pop remaining))
               (t
                (push (aref lines (1- line)) result)
                (incf line))))
    (nreverse result)))

(-> workspace-file--join-lines (list string boolean) string)
(defun workspace-file--join-lines (lines line-ending final-newline-p)
  "Return LINES joined with LINE-ENDING while preserving FINAL-NEWLINE-P."
  (with-output-to-string (stream)
    (loop for line in lines
          for first-p = t then nil
          unless first-p do (write-string line-ending stream)
          do (write-string line stream))
    (when (and final-newline-p lines)
      (write-string line-ending stream))))

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
  (let ((octets (sb-ext:string-to-octets content :external-format ':utf-8)))
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
  "Write replacement OCTETS to TEMPORARY and preserve TARGET permissions."
  (with-open-file (stream temporary
                          :direction ':output
                          :if-exists ':error
                          :if-does-not-exist ':create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream)
    (finish-output stream))
  (ignore-errors
    (platform-copy-file-permissions *platform* target temporary))
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
  "Atomically publish CONTENT after an immediate exact BASE-OBSERVATION check.

Autolith mutations are serialized. Existing-file replacement uses portable
POSIX rename, which cannot conditionally reject an unrelated external writer in
the final check-to-rename window. Missing-file publication rejects that race."
  (let ((path (workspace-file-resource-pathname resource)))
    (ensure-directories-exist path)
    (let ((octets (workspace-file--replacement-octets content))
          (temporary (workspace-file--temporary-path path)))
      (unwind-protect
           (progn
             (workspace-file--write-temporary temporary path octets)
             (let ((current (workspace-file--observe-path resource context)))
               (unless (workspace-file--same-observation-p
                        current base-observation)
                 (workspace-file--signal-stale
                  resource base-observation current)))
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
             (let ((published (workspace-file--observe-path resource context)))
               (unless (and (eq (workspace-file-observation-kind published) ':file)
                            (string= content
                                     (resource-observation-content published)))
                 (error 'tool-error
                        :message
                        "Atomic workspace resource publication did not produce the exact requested content."
                        :tool-name "resource.edit"))
               (setf temporary nil)
               published))
        (when (and temporary (probe-file temporary))
          (delete-file temporary))))))

(-> workspace-file--operation-window (list (integer 0))
    (values (integer 1) (integer 1)))
(defun workspace-file--operation-window (operations total-lines)
  "Return a bounded nearby window covering OPERATIONS in the resulting file."
  (let* ((first-line (reduce #'min operations :key (lambda (op) (getf op :start))))
         (last-line (reduce #'max operations :key (lambda (op) (getf op :end))))
         (start (max 1 (- first-line 2)))
         (count (max 1 (min 120 (+ (- last-line start) 6)))))
    (values (if (zerop total-lines) 1 (min start total-lines)) count)))

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
               (base-observation
                 (resource-observation-state-observation state)))
          (when (workspace-file--mixed-line-endings-p
                 (resource-observation-content base-observation))
            (error 'tool-error
                   :message "resource.edit does not rewrite files with mixed LF and CRLF line endings because doing so could change untouched lines. Normalize the complete file deliberately before applying structured edits."
                   :tool-name "resource.edit"))
          (let ((current (workspace-file--observe-path resource context)))
            (unless (workspace-file--same-observation-p
                     current base-observation)
              (workspace-file--signal-stale
               resource base-observation current))
            (let* ((normalized
                     (workspace-file--normalize-operations operations state))
                   (new-lines
                     (workspace-file--apply-normalized-operations
                      (workspace-file-observation-lines base-observation)
                      normalized))
                   (replace-empty
                     (find ':replace-empty normalized
                           :key (lambda (operation) (getf operation :kind))))
                   (content
                     (workspace-file--join-lines
                      new-lines
                      (if replace-empty
                          (getf replace-empty :line-ending)
                          (workspace-file-observation-line-ending base-observation))
                      (if replace-empty
                          (getf replace-empty :final-newline-p)
                          (workspace-file-observation-final-newline-p
                           base-observation)))))
              (values
               (workspace-file--publish resource context base-observation content)
               normalized))))))))


;;;; -- Resource Tool Methods --

(-> workspace-file--authorization-command
    ((member :read :edit) pathname)
    string)
(defun workspace-file--authorization-command (operation path)
  "Return the command-shaped permission request for OPERATION on PATH."
  (format nil "~A -- ~A"
          (ecase operation
            (:read "resource.read")
            (:edit "resource.edit"))
          (uiop:escape-shell-token (uiop:native-namestring path))))

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
        (let* ((tool-name
                 (ecase operation
                   (:read "resource.read")
                   (:edit "resource.edit")))
               (command
                 (workspace-file--authorization-command operation path))
               (decision
                 (tool-context-authorize-command
                  context command
                  (configuration-working-directory
                   (tool-context-configuration context)))))
          (unless (eq decision ':full-access)
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
             :line-anchor-function #'workspace-file--line-anchor)
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
                    :line-anchor-function #'workspace-file--line-anchor)
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
