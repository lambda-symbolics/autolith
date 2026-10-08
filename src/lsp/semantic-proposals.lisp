(in-package #:autolith)

;;;; -- Revisioned Semantic Proposals --

(defparameter *lsp-semantic-maximum-entries* 32
  "Maximum retained proposals and action choices per conversation.")

(defparameter *lsp-semantic-maximum-snapshots-per-entry* 128
  "Maximum resource snapshots retained by one semantic choice.")

(defparameter *lsp-semantic-maximum-snapshots* 512
  "Maximum resource snapshots retained by all choices in one conversation.")

(defparameter *lsp-semantic-maximum-snapshot-bytes* (* 16 1024 1024)
  "Maximum UTF-8 snapshot bytes retained by semantic choices per conversation.")

(defparameter *lsp-semantic-maximum-characters* 240000
  "Maximum encoded characters in a semantic proposal or action choice.")

(define-condition lsp-semantic-error (lsp-error) ()
  (:documentation "A semantic proposal violates Autolith application policy."))

(defclass lsp-semantic-state ()
  ((entries :initform (make-hash-table :test #'eq :weakness :key)
            :reader lsp-semantic-state-entries
            :documentation "Conversation-keyed bounded choices, without owning conversations."))
  (:documentation "Registry-owned semantic choices serialized by the LSP manager lock."))

(defclass lsp-semantic-entry ()
  ((identifier :initform (daemon-random-token) :reader lsp-semantic-entry-identifier
               :documentation "Opaque conversation-local choice identifier.")
   (client :initarg :client :reader lsp-semantic-entry-client
           :documentation "Server that returned this choice.")
   (transport :initarg :transport :reader lsp-semantic-entry-transport
              :documentation "Exact request transport, invalidated by restart.")
   (value :initarg :value :reader lsp-semantic-entry-value
          :documentation "Normalized proposal or raw action row.")
   (kind :initarg :kind :reader lsp-semantic-entry-kind
         :documentation "Either :proposal or :action.")
   (snapshots :initarg :snapshots :reader lsp-semantic-entry-snapshots
              :documentation "URI-keyed resource, proposal-owned observation state and exact document version."))
  (:documentation "An immutable proposed transformation against exact retained snapshots."))

(-> lsp-semantic--fail (string) null)
(defun lsp-semantic--fail (message)
  "Signal a policy failure with a bounded actionable MESSAGE."
  (error 'lsp-semantic-error :message message))

(-> lsp-semantic--resource (tool-context string) workspace-file-resource)
(defun lsp-semantic--resource (context uri)
  "Resolve a local file URI through the ordinary workspace resource boundary."
  (unless (and (stringp uri) (uiop:string-prefix-p "file:///" uri)
               (not (find #\? uri)) (not (find #\# uri)))
    (lsp-semantic--fail "Semantic edits require local file URIs without authorities, queries or fragments."))
  (let* ((name (workspace-file--decode-identifier "file" (subseq uri 7)))
         (path (platform-file-uri-pathname *platform* name))
         (resource (resource-registry-resolve
                    (tool-registry-resource-registry (tool-context-registry context))
                    (concatenate 'string "workspace:" (workspace-file--encode-identifier
                                                       (uiop:native-namestring path)))
                    context)))
    (unless (typep resource 'workspace-file-resource)
      (lsp-semantic--fail "Semantic edits support regular workspace files only."))
    resource))

(-> lsp-semantic--observation (tool-context list) workspace-file-observation)
(defun lsp-semantic--observation (context record)
  "Return the immutable observation owned by a proposal snapshot."
  (declare (ignore context))
  (resource-observation-state-observation (second record)))

(-> lsp-semantic--snapshot-bytes (hash-table) (integer 0))
(defun lsp-semantic--snapshot-bytes (snapshots)
  "Return the total UTF-8 bytes owned by SNAPSHOTS."
  (loop for record being the hash-values of snapshots
        sum (workspace-file--observation-retained-bytes
             (resource-observation-state-observation (second record)))))

(-> lsp-semantic--snapshot-function (tool-context lsp-client hash-table) function)
(defun lsp-semantic--snapshot-function (context client snapshots)
  "Capture bounded authorized resources and synchronized versions before normalization."
  (let ((versions (make-hash-table :test #'equal))
        (bytes (lsp-semantic--snapshot-bytes snapshots)))
    (with-lock-held ((cl-lsp:lsp-client-lock client))
      (maphash (lambda (uri document)
                 (setf (gethash uri versions)
                       (cons (lsp-document-text document) (cl-lsp:lsp-document-version document))))
               (cl-lsp:lsp-client-documents client)))
    (lambda (uri)
      (let ((record (gethash uri snapshots)))
        (unless record
          (when (>= (hash-table-count snapshots)
                    (min *lsp-semantic-maximum-snapshots-per-entry* *lsp-semantic-maximum-snapshots*))
            (lsp-semantic--fail "Semantic proposal addresses too many resources; narrow the request."))
          (let* ((resource (lsp-semantic--resource context uri))
                 (observation
                   (workspace-file--call-with-authorized-access
                    resource context ':read (lambda () (resource-observe resource context))))
                 (kind (workspace-file-observation-kind observation))
                 (version (gethash uri versions))
                 (weight (workspace-file--observation-retained-bytes observation)))
            (unless (member kind '(:file :missing))
              (lsp-semantic--fail "Semantic edits do not support directories or service resources."))
            (when (> (+ bytes weight) *lsp-semantic-maximum-snapshot-bytes*)
              (lsp-semantic--fail "Semantic snapshots exceed the retained byte limit; narrow the request."))
            (when (and version
                       (not (and (eq kind ':file)
                                 (equal (first version) (resource-observation-content observation)))))
              (lsp-semantic--fail "Synchronized document differs from the observed file; retry the query."))
            (maphash (lambda (other-uri other)
                       (when (and (not (equal uri other-uri))
                                  (equal (resource-uri resource) (resource-uri (first other))))
                         (lsp-semantic--fail "A proposal addresses one resource through multiple URI aliases.")))
                     snapshots)
            (setf record (list resource
                               (make-instance 'workspace-file-observation-state
                                              :alias (format nil "P~A" (daemon-random-token))
                                              :observation observation :visible-ranges nil)
                               (rest version))
                  (gethash uri snapshots) record)
            (incf bytes weight)))
        (let ((observation (lsp-semantic--observation context record)))
          (values (and (eq (workspace-file-observation-kind observation) ':file)
                       (resource-observation-content observation))
                  (third record) (workspace-file-observation-kind observation)))))))

(-> lsp-semantic--store (lsp-semantic-state tool-context lsp-semantic-entry) string)
(defun lsp-semantic--store (state context entry)
  "Retain the newest choices within entry, snapshot-count and UTF-8 byte budgets."
  (when (> (length (json-encode (lsp-semantic-entry-value entry)))
           *lsp-semantic-maximum-characters*)
    (lsp-semantic--fail "Semantic proposal exceeds the retained choice limit; narrow the request."))
  (let* ((snapshots (lsp-semantic-entry-snapshots entry))
         (conversation (tool-context-conversation context))
         (entries (cons entry (gethash conversation (lsp-semantic-state-entries state)))))
    (when (or (> (hash-table-count snapshots)
                 (min *lsp-semantic-maximum-snapshots-per-entry* *lsp-semantic-maximum-snapshots*))
              (> (lsp-semantic--snapshot-bytes snapshots) *lsp-semantic-maximum-snapshot-bytes*))
      (lsp-semantic--fail "Semantic snapshots exceed the retained resource budget; narrow the request."))
    (setf (gethash conversation (lsp-semantic-state-entries state))
          (loop for candidate in entries
                for index from 0
                for snapshots = (lsp-semantic-entry-snapshots candidate)
                sum (hash-table-count snapshots) into count
                sum (lsp-semantic--snapshot-bytes snapshots) into bytes
                while (and (< index *lsp-semantic-maximum-entries*)
                           (<= count *lsp-semantic-maximum-snapshots*)
                           (<= bytes *lsp-semantic-maximum-snapshot-bytes*))
                  collect candidate)))
  (lsp-semantic-entry-identifier entry))

(-> lsp-semantic--find
    (lsp-semantic-state tool-context &key (:identifier string) (:kind symbol)) lsp-semantic-entry)
(defun lsp-semantic--find (state context &key identifier kind)
  "Find an unexpired choice of KIND in the current conversation only."
  (or (find-if (lambda (entry)
                 (and (equal identifier (lsp-semantic-entry-identifier entry))
                      (eq kind (lsp-semantic-entry-kind entry))))
               (gethash (tool-context-conversation context) (lsp-semantic-state-entries state)))
      (lsp-semantic--fail "Unknown or expired semantic choice; request a fresh proposal.")))

(-> lsp-semantic--validate (tool-context lsp-semantic-entry) null)
(defun lsp-semantic--validate (context entry)
  "Reauthorize every snapshot and reject disk or server-version drift."
  (let ((client (lsp-semantic-entry-client entry)))
    (unless (eq (lsp-semantic-entry-transport entry) (lsp-client-transport client))
      (lsp-semantic--fail "Language server restarted; request a fresh proposal."))
    (maphash
     (lambda (uri record)
       (destructuring-bind (resource state version) record
         (let ((observation (resource-observation-state-observation state)))
           (workspace-file--call-with-authorized-access
            resource context ':edit
            (lambda ()
              (let ((current (resource-observe resource context)))
                (unless (workspace-file--same-observation-p observation current)
                  (workspace-file--signal-stale resource observation current)))))
           (with-lock-held ((cl-lsp:lsp-client-lock client))
             (let ((document (gethash uri (cl-lsp:lsp-client-documents client))))
               (unless (eql version (and document (cl-lsp:lsp-document-version document)))
                 (lsp-semantic--fail "Document version changed; request a fresh proposal.")))))))
     (lsp-semantic-entry-snapshots entry)))
  nil)

(-> lsp-semantic--edit-text (string vector) string)
(defun lsp-semantic--edit-text (text edits)
  "Apply validated simultaneous offsets, retaining insertion order at equal offsets."
  (let ((cursor 0)
        (ordered (stable-sort (coerce edits 'list)
                              (lambda (left right)
                                (or (< (json-get left "startOffset") (json-get right "startOffset"))
                                    (and (= (json-get left "startOffset") (json-get right "startOffset"))
                                         (< (json-get left "endOffset") (json-get right "endOffset"))))))))
    (with-output-to-string (output)
      (dolist (edit ordered)
        (let ((start (json-get edit "startOffset")) (end (json-get edit "endOffset")))
          (unless (and (integerp start) (integerp end) (<= cursor start end (length text))
                       (stringp (json-get edit "newText")))
            (lsp-semantic--fail "Proposal offsets are invalid for the ordered resource state."))
          (write-string text output :start cursor :end start)
          (write-string (json-get edit "newText") output)
          (setf cursor end)))
      (write-string text output :start cursor))))

(-> lsp-semantic--requests (tool-context lsp-semantic-entry) list)
(defun lsp-semantic--requests (context entry)
  "Evaluate ordered file effects in memory and produce one revisioned request per resource."
  (let* ((plan (lsp-semantic-entry-value entry))
         (snapshots (lsp-semantic-entry-snapshots entry))
         (overlay (make-hash-table :test #'equal))
         (permission-donors (make-hash-table :test #'equal))
         (order nil))
    (unless (equal (json-get plan "positionEncoding") "utf-16")
      (lsp-semantic--fail "Semantic proposal encoding must be UTF-16."))
    (maphash (lambda (uri record)
               (let ((base (lsp-semantic--observation context record)))
                 (setf (gethash uri overlay)
                       (and (eq (workspace-file-observation-kind base) ':file)
                            (resource-observation-content base))
                     (gethash uri permission-donors)
                     (and (eq (workspace-file-observation-kind base) ':file) uri)))) snapshots)
    (labels ((content (uri)
               (unless (gethash uri snapshots)
                 (lsp-semantic--fail "Proposal contains an unobserved resource."))
               (gethash uri overlay))

             (replace-content (uri value)
               (content uri)
               (pushnew uri order :test #'equal)
               (setf (gethash uri overlay) value)))
      (loop for operation across (json-get plan "operations")
            for kind = (json-get operation "kind")
            for uri = (json-get operation "uri")
            for options = (json-get operation "options")
            for overwrite = (eq (and options (json-get options "overwrite")) t)
            for ignore = (eq (and options (json-get options "ignoreIfExists")) t)
            do (cond
                 ((equal kind "text")
                  (let ((text (content uri)))
                    (unless text (lsp-semantic--fail "Cannot edit an absent resource."))
                    (replace-content uri (lsp-semantic--edit-text text (json-get operation "edits")))))
                 ((equal kind "create")
                  (cond
                    ((or overwrite (null (content uri)))
                     (replace-content uri "")
                     (setf (gethash uri permission-donors) nil))
                    (ignore
                     nil)
                    (t
                     (lsp-semantic--fail "Create target exists without overwrite or ignoreIfExists."))))
                 ((equal kind "delete")
                  (cond
                    ((content uri)
                     (replace-content uri nil)
                     (setf (gethash uri permission-donors) nil))
                    ((eq (and options (json-get options "ignoreIfNotExists")) t)
                     nil)
                    (t
                     (lsp-semantic--fail "Delete target does not exist."))))
                 ((equal kind "rename")
                  (let* ((old (json-get operation "oldUri"))
                         (new (json-get operation "newUri"))
                         (source (content old))
                         (target (content new)))
                    (unless source (lsp-semantic--fail "Move source does not exist."))
                    (cond
                      ((equal old new)
                       nil)
                      ((or overwrite (null target))
                       (replace-content old nil)
                       (replace-content new source)
                       (setf (gethash new permission-donors) (gethash old permission-donors)
                             (gethash old permission-donors) nil))
                      (ignore
                       nil)
                      (t
                       (lsp-semantic--fail "Move target exists without overwrite or ignoreIfExists.")))))
                 (t
                  (lsp-semantic--fail "Unsupported semantic resource operation."))))
      (loop for uri in (nreverse order)
            for record = (gethash uri snapshots)
            for revision = (resource-observation-state-alias (second record))
            for base = (lsp-semantic--observation context record)
            for before = (and (eq (workspace-file-observation-kind base) ':file)
                              (resource-observation-content base))
            for after = (gethash uri overlay)
            for donor-uri = (and after (gethash uri permission-donors))
            for moved-p = (and donor-uri (not (equal uri donor-uri)))
            unless (and (equal before after) (not moved-p))
              collect (cl-resources:make-resource-change
                         (first record) :base-revision revision
                       :operations
                       (list (make-instance
                              'workspace-content-operation :content after
                              :permission-donor
                              (and moved-p
                                   (lsp-semantic--observation context (gethash donor-uri snapshots))))))))))

(-> lsp-semantic--capture-plan
    (tool-context lsp-client &key (:snapshots hash-table) (:plan json-object)) lsp-semantic-entry)
(defun lsp-semantic--capture-plan (context client &key snapshots plan)
  "Authorize all operation paths, retain snapshots and reject unsupported effects before presenting."
  (let ((snapshot (lsp-semantic--snapshot-function context client snapshots)))
    (loop for operation across (json-get plan "operations")
          do (dolist (field (if (equal (json-get operation "kind") "rename")
                               '("oldUri" "newUri") '("uri")))
               (funcall snapshot (json-get operation field))))
    (let ((entry (make-instance 'lsp-semantic-entry :client client
                                :transport (lsp-client-transport client) :kind ':proposal
                                :value plan :snapshots snapshots)))
      (lsp-semantic--requests context entry)
      entry)))

(-> lsp-semantic--apply (tool-context lsp-semantic-entry vector) list)
(defun lsp-semantic--apply (context entry approved)
  "Apply a retained proposal after exact validation and explicit annotation approval."
  (maphash
   (lambda (identifier annotation)
     (when (eq (json-get annotation "needsConfirmation") t)
       (unless (find identifier approved :test #'equal)
         (lsp-semantic--fail
          (format nil "Annotation ~A requires explicit approval before applying." identifier)))
       (unless (eq (tool-context-authorize-command
                    context
                    (format nil "lsp.semantic-confirmation --proposal ~S --annotation ~S --details ~A"
                            (lsp-semantic-entry-identifier entry) identifier
                            (json-encode annotation))
                    (config :working-directory (tool-context-configuration context)))
                   ':full-access)
         (lsp-semantic--fail
          (format nil "Permission to apply annotation ~A was denied." identifier)))))
   (json-get (lsp-semantic-entry-value entry) "annotations"))
  (let ((*workspace-file-proposal-observations* (make-hash-table :test #'equal)))
    (maphash
     (lambda (uri record)
       (declare (ignore uri))
       (destructuring-bind (resource state version) record
         (declare (ignore version))
         (setf (gethash (list (tool-context-conversation context)
                             (resource-uri resource) (resource-observation-state-alias state))
                       *workspace-file-proposal-observations*)
               state)))
     (lsp-semantic-entry-snapshots entry))
    (with-recursive-lock-held (*workspace-file-mutation-lock*)
      (lsp-semantic--validate context entry)
      (workspace-change-set-apply context (lsp-semantic--requests context entry)))))
