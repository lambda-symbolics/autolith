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

(define-condition lsp-semantic-authority-denied (lsp-semantic-error tool-authority-denied) ()
  (:documentation "A semantic proposal lacks explicit authority for an annotation."))

(defmethod tool-failure-code ((condition lsp-semantic-authority-denied))
  "Preserve the semantic policy failure code while marking the authority refusal."
  ':tool-failure)

(defclass lsp-semantic-state (edit-proposal-store) ()
  (:documentation "Semantic choices with independent entry and snapshot retention budgets."))

(defclass lsp-semantic-entry (edit-proposal)
  ((client :initarg :client :reader lsp-semantic-entry-client
           :documentation "Server that returned this choice.")
   (transport :initarg :transport :reader lsp-semantic-entry-transport
              :documentation "Exact request transport, invalidated by restart.")
   (value :initarg :value :reader lsp-semantic-entry-value
          :documentation "Normalized proposal or raw action row.")
   (kind :initarg :kind :reader lsp-semantic-entry-kind
         :documentation "Either :proposal or :action."))
  (:documentation "An immutable proposed transformation against exact retained snapshots."))

(declaim (inline lsp-semantic--fail))

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
  (let* ((name     (workspace-file--decode-identifier "file" (subseq uri 7)))
         (path     (platform-file-uri-pathname *platform* name))
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
        (bytes    (lsp-semantic--snapshot-bytes snapshots)))
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
                    resource context ':read
                    :function (lambda () (resource-observe resource context))))
                 (kind     (workspace-file-observation-kind observation))
                 (version  (gethash uri versions))
                 (weight   (workspace-file--observation-retained-bytes observation)))
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

(defmethod edit-proposal-store-limits ((store lsp-semantic-state))
  "Return the semantic limits for entry count, snapshot count and retained snapshot bytes."
  (values *lsp-semantic-maximum-entries* *lsp-semantic-maximum-snapshots*
          *lsp-semantic-maximum-snapshot-bytes*))

(defmethod edit-proposal-fail ((store lsp-semantic-state) code message)
  "Signal proposal-store failures as semantic policy errors."
  (declare (ignore code))
  (lsp-semantic--fail message))

(-> lsp-semantic-entry-identifier (lsp-semantic-entry) string)
(defun lsp-semantic-entry-identifier (entry)
  "Return the shared conversation-local proposal identity."
  (edit-proposal-identifier entry))

(-> lsp-semantic-entry-snapshots (lsp-semantic-entry) hash-table)
(defun lsp-semantic-entry-snapshots (entry)
  "Return independently owned semantic baselines."
  (edit-proposal-snapshots entry))

(-> lsp-semantic--store (lsp-semantic-state tool-context lsp-semantic-entry) string)
(defun lsp-semantic--store (state context entry)
  "Enforce semantic payload limits and retain ENTRY in the common bounded store."
  (when (> (length (json-encode (lsp-semantic-entry-value entry)))
           *lsp-semantic-maximum-characters*)
    (lsp-semantic--fail "Semantic proposal exceeds the retained choice limit; narrow the request."))
  (when (> (hash-table-count (edit-proposal-snapshots entry))
           *lsp-semantic-maximum-snapshots-per-entry*)
    (lsp-semantic--fail "Semantic proposal addresses too many resources; narrow the request."))
  (edit-proposal-retain state context entry)
  (edit-proposal-identifier entry))

(-> lsp-semantic--find
    (lsp-semantic-state tool-context &key (:identifier string) (:kind symbol)) lsp-semantic-entry)
(defun lsp-semantic--find (state context &key identifier kind)
  "Find an unexpired semantic choice of KIND in the current conversation."
  (let ((entry (edit-proposal-find state context identifier)))
    (unless (eq kind (lsp-semantic-entry-kind entry))
      (lsp-semantic--fail "Semantic choice has the wrong kind; request a fresh proposal."))
    entry))

(defmethod edit-proposal-validate :after ((context tool-context) (entry lsp-semantic-entry))
  "Reject choices if the server has restarted or a synchronized document's version has changed."
  (let ((client (lsp-semantic-entry-client entry)))
    (unless (eq (lsp-semantic-entry-transport entry) (lsp-client-transport client))
      (lsp-semantic--fail "Language server restarted; request a fresh proposal."))
    (with-lock-held ((cl-lsp:lsp-client-lock client))
      (maphash
       (lambda (uri record)
         (let ((document (gethash uri (cl-lsp:lsp-client-documents client))))
           (unless (eql (third record) (and document (cl-lsp:lsp-document-version document)))
             (lsp-semantic--fail "Document version changed; request a fresh proposal."))))
       (edit-proposal-snapshots entry)))))

(-> lsp-semantic--validate (tool-context lsp-semantic-entry) null)
(defun lsp-semantic--validate (context entry)
  "Validate owned baselines and server identity before resolving a choice."
  (edit-proposal-validate context entry))

(-> lsp-semantic--requests (tool-context lsp-semantic-entry) list)
(defun lsp-semantic--requests (context entry)
  "Materialize library effects and wrap them in revisioned host publication requests."
  (let* ((snapshots (lsp-semantic-entry-snapshots entry))
         (effects
           (cl-lsp:lsp-materialize-workspace-plan
            (lsp-semantic-entry-value entry)
            :snapshot
            (lambda (uri)
              (let ((record (gethash uri snapshots)))
                (unless record
                  (lsp-semantic--fail "Proposal contains an unobserved resource."))
                (let ((base (lsp-semantic--observation context record)))
                  (values (and (eq (workspace-file-observation-kind base) ':file)
                               (resource-observation-content base))
                          (third record) (workspace-file-observation-kind base))))))))
    (loop for effect across effects
          for uri = (json-get effect "uri")
          for record = (gethash uri snapshots)
          for revision = (resource-observation-state-alias (second record))
          for base = (lsp-semantic--observation context record)
          for before = (and (eq (workspace-file-observation-kind base) ':file)
                            (resource-observation-content base))
          for after = (json-get effect "text")
          for donor-uri = (json-get effect "originUri")
          for moved-p = (and donor-uri (not (equal uri donor-uri)))
          unless (and (equal before after) (not moved-p))
            collect (cl-resources:make-resource-change
                     (first record) :base-revision revision
                     :operations
                     (list (make-instance
                            'workspace-content-operation :content after
                            :permission-donor
                            (and moved-p
                                 (lsp-semantic--observation context (gethash donor-uri snapshots)))))))))

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

(defmethod edit-proposal-requests ((context tool-context) (entry lsp-semantic-entry))
  "Materialize semantic changes through the semantic adapter."
  (lsp-semantic--requests context entry))

(-> lsp-semantic--approve (tool-context lsp-semantic-entry vector) null)
(defun lsp-semantic--approve (context entry approved)
  "Require explicit annotation approval and fresh host authority before applying."
  (maphash
   (lambda (identifier annotation)
     (when (eq (json-get annotation "needsConfirmation") t)
       (unless (find identifier approved :test #'equal)
         (error 'lsp-semantic-authority-denied
                :message (format nil "Annotation ~A requires explicit approval before applying." identifier)))
       (unless (eq (tool-context-authorize-command
                    context
                    (format nil "lsp.semantic-confirmation --proposal ~S --annotation ~S --details ~A"
                            (lsp-semantic-entry-identifier entry) identifier
                            (json-encode annotation))
                    (config :working-directory (tool-context-configuration context)))
                   ':full-access)
         (error 'lsp-semantic-authority-denied
                :message (format nil "Permission to apply annotation ~A was denied." identifier)))))
   (json-get (lsp-semantic-entry-value entry) "annotations"))
  nil)

(-> lsp-semantic--apply (tool-context lsp-semantic-entry vector) list)
(defun lsp-semantic--apply (context entry approved)
  "Publish a semantic proposal through shared one-time consumption."
  ;; Directly constructed entries use the same lifecycle as tool-retained entries.
  (let ((store (or (edit-proposal-store entry) (make-instance 'lsp-semantic-state))))
    (unless (edit-proposal-store entry)
      (edit-proposal-retain store context entry))
    (edit-proposal-apply store context entry
                         :before (lambda () (lsp-semantic--approve context entry approved)))))
