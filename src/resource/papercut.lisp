(in-package #:autolith)

;;;; -- Papercut Resource Conditions --

(define-condition papercut-resource-identifier-unsupported (autolith-error)
  ((identifier
    :initarg :identifier
    :reader papercut-resource-identifier-unsupported-identifier
    :type non-empty-string
    :documentation "The unsupported papercut resource identifier."))
  (:default-initargs :message "The papercut resource identifier is not supported.")
  (:documentation "A papercut: URI names neither the current collection nor an exact item.")
  (:report (lambda (condition stream)
             (format stream
                     "Papercut resource identifier ~S is unsupported; use papercut:current or papercut:id/<percent-encoded-stable-id>."
                     (papercut-resource-identifier-unsupported-identifier
                      condition)))))

(define-condition papercut-resource-not-found (autolith-error)
  ((identifier
    :initarg :identifier
    :reader papercut-resource-not-found-identifier
    :type non-empty-string
    :documentation "The stable papercut identifier that has no active value."))
  (:default-initargs :message "The active papercut does not exist.")
  (:documentation "A papercut: exact-item URI names a closed, missing, or other-workspace report.")
  (:report (lambda (condition stream)
             (format stream "Active papercut ~A does not exist in this workspace."
                     (papercut-resource-not-found-identifier condition)))))


;;;; -- Papercut Resources --

(defparameter *papercut-resource-maximum-observations* 16
  "The transient papercut observations retained by one conversation.")

(defparameter *papercut-resource-excerpt-limit* 240
  "The maximum report characters shown for one collection entry.")

(defvar *papercut-resource-digest-key* (random-data 16)
  "The process-local key identifying exact current-workspace papercut snapshots.")

(defclass papercut-resource (resource)
  ((identifier
    :initarg :identifier
    :reader papercut-resource-identifier
    :type non-empty-string
    :documentation "The collection name or stable report identifier in this URI.")
   (workspace
    :initarg :workspace
    :reader papercut-resource-workspace
    :type non-empty-string
    :documentation "The current workspace identity resolved under authority."))
  (:documentation "A revisioned current-workspace papercut collection or active report."))

(defclass papercut-collection-resource (papercut-resource)
  ()
  (:documentation "The active current-workspace collection exposed as papercut:current."))

(defclass papercut-item-resource (papercut-resource)
  ()
  (:documentation "One exact active current-workspace papercut report."))

(defclass papercut-resolver (resource-resolver)
  ()
  (:documentation "Resolve the current papercut collection and canonical exact active reports."))

(defclass papercut-observation (resource-observation)
  ((identifier
    :initarg :identifier
    :reader papercut-observation-identifier
    :type non-empty-string
    :documentation "The collection name or stable report identifier observed.")
   (workspace
    :initarg :workspace
    :reader papercut-observation-workspace
    :type non-empty-string
    :documentation "The current workspace represented by this observation.")
   (kind
    :initarg :kind
    :reader papercut-observation-kind
    :type keyword
    :documentation "Whether the observation represents a collection, item, or closure.")
   (snapshot
    :initarg :snapshot
    :reader papercut-observation-snapshot
    :type list
    :documentation "The exact detached readable snapshot used for revision gating."))
  (:documentation "An exact rendered and structural current-workspace papercut snapshot."))

(defclass papercut-observation-state (resource-observation-state)
  ()
  (:documentation "One conversation-local model observation of a papercut: resource."))

(defmethod resource-observation-state-family-and-key
    ((observation papercut-observation))
  "Return the papercut state family and workspace-specific snapshot key."
  (values 'papercut-observation-state (list (resource-observation-uri observation)
                                           (papercut-observation-workspace observation)
                                           (resource-observation-revision observation)
                                           (papercut-observation-snapshot observation))))

(defmethod resource-observation-state-class ((resource papercut-resource))
  "Record papercut observations under the papercut state family."
  (declare (ignore resource))
  'papercut-observation-state)

(defmethod resource-item-identity ((resource papercut-resource))
  "Compare papercut observations by collection or stable identifier."
  (papercut-resource-identifier resource))

(defmethod resource-observation-identity ((observation papercut-observation))
  "Compare papercut observations by collection or stable identifier."
  (papercut-observation-identifier observation))

(defmethod resource-observation-state-maximum ((state papercut-observation-state))
  "Return the configured papercut observation limit."
  (declare (ignore state))
  *papercut-resource-maximum-observations*)


;;;; -- URI Resolution --

(-> papercut-resource--make-item
    (non-empty-string non-empty-string)
    papercut-item-resource)
(defun papercut-resource--make-item (identifier workspace)
  "Return one exact item resource with canonical URI and WORKSPACE identity."
  (make-instance 'papercut-item-resource
                 :uri        (resource-item-uri "papercut" identifier)
                 :identifier identifier
                 :workspace  workspace))

(defmethod resource-resolver-resolve
    ((resolver papercut-resolver) identifier (context tool-context))
  "Resolve CURRENT or one canonical exact active-report identifier."
  (declare (ignore resolver))
  (let ((workspace
          (papercut--workspace (tool-context-configuration context))))
    (cond
      ((string= identifier "current")
       (make-instance 'papercut-collection-resource
                      :uri        "papercut:current"
                      :identifier "current"
                      :workspace  workspace))
      ((uiop:string-prefix-p "id/" identifier)
       (papercut-resource--make-item
        (resource-item-decode-identifier
         "papercut" "papercut"
         (subseq identifier (length "id/")))
        workspace))
      (t
       (error 'papercut-resource-identifier-unsupported
              :identifier identifier)))))

(defmethod resource-capabilities
    ((resource papercut-resource) (context tool-context))
  "Expose read and edit only while RESOURCE matches CONTEXT's current workspace."
  (if (string= (papercut-resource-workspace resource)
               (papercut--workspace (tool-context-configuration context)))
      '(:read :edit)
      nil))


;;;; -- Presentation and Snapshots --

(-> papercut-resource--render-assessment (papercut integer) non-empty-string)
(defun papercut-resource--render-assessment (papercut note-limit)
  "Return the latest PAPERCUT assessment with NOTE-LIMIT applied to its note."
  (if (papercut-assessment-verdict papercut)
      (format nil
              "assessment: ~(~A~)~%assessed: ~A~%assessment note: ~A"
              (papercut-assessment-verdict papercut)
              (papercut-timestamp-string (papercut-assessed-at papercut))
              (papercut-excerpt (papercut-assessment-note papercut) note-limit))
      "assessment: unassessed"))

(-> papercut-resource--render-summary (papercut) non-empty-string)
(defun papercut-resource--render-summary (papercut)
  "Return one complete collection-summary entry for PAPERCUT."
  (format nil
          "id: ~A~%uri: ~A~%reported: ~A~%~A~%title: ~A~%excerpt: ~A"
          (papercut-identifier papercut)
          (resource-item-uri "papercut" (papercut-identifier papercut))
          (papercut-timestamp-string (papercut-reported-at papercut))
          (papercut-resource--render-assessment
           papercut *papercut-resource-excerpt-limit*)
          (papercut-title papercut)
          (papercut-excerpt
           (papercut-content papercut)
           *papercut-resource-excerpt-limit*)))

(-> papercut-resource--render-collection (non-empty-string list) non-empty-string)
(defun papercut-resource--render-collection (workspace papercuts)
  "Return the active WORKSPACE PAPERCUTS as complete summary entries."
  (if papercuts
      (format nil "workspace: ~A~%active papercuts: ~D~2%~{~A~^~2%~}"
              workspace
              (length papercuts)
              (mapcar #'papercut-resource--render-summary papercuts))
      (format nil "workspace: ~A~%active papercuts: 0~%No active papercuts."
              workspace)))

(-> papercut-resource--render-item (papercut) non-empty-string)
(defun papercut-resource--render-item (papercut)
  "Return complete model-visible data for one active PAPERCUT."
  (format nil
          "id: ~A~%uri: ~A~%reported: ~A~%workspace: ~A~%source conversation: ~A~%~A~%title: ~A~2%~A"
          (papercut-identifier papercut)
          (resource-item-uri "papercut" (papercut-identifier papercut))
          (papercut-timestamp-string (papercut-reported-at papercut))
          (papercut-workspace papercut)
          (or (papercut-source-conversation papercut) "unknown")
          (papercut-resource--render-assessment
           papercut *papercut-assessment-note-limit*)
          (papercut-title papercut)
          (papercut-content papercut)))

(-> papercut-resource--assessment-snapshot (papercut) list)
(defun papercut-resource--assessment-snapshot (papercut)
  "Return PAPERCUT's detached latest-assessment snapshot."
  (list :verdict (papercut-assessment-verdict papercut)
        :note (papercut-assessment-note papercut)
        :assessed-at (papercut-assessed-at papercut)))

(-> papercut-resource--collection-snapshot (non-empty-string list) list)
(defun papercut-resource--collection-snapshot (workspace papercuts)
  "Return an exact snapshot of active reports and assessments in WORKSPACE."
  (list :kind ':collection
        :workspace workspace
        :identifiers (mapcar #'papercut-identifier papercuts)
        :assessments
        (mapcar (lambda (papercut)
                  (list (papercut-identifier papercut)
                        (papercut-resource--assessment-snapshot papercut)))
                papercuts)))

(-> papercut-resource--item-snapshot (papercut) list)
(defun papercut-resource--item-snapshot (papercut)
  "Return an exact detached snapshot of one active PAPERCUT."
  (list :kind ':item
        :record (papercut--record papercut)
        :assessment (papercut-resource--assessment-snapshot papercut)))

(-> papercut-resource--collection-observation-unlocked
    (papercut-collection-resource tool-context list)
    papercut-observation)
(defun papercut-resource--collection-observation-unlocked (resource context active)
  "Observe RESOURCE from private ACTIVE transaction state."
  (let* ((configuration (tool-context-configuration context))
         (workspace (papercut--workspace configuration)))
    (unless (string= workspace (papercut-resource-workspace resource))
      (error 'resource-revision-stale
             :uri               (resource-uri resource)
             :expected-revision "workspace-context"
             :actual-revision   nil))
    (let* ((papercuts (papercut--workspace-reports configuration active))
           (snapshot
             (papercut-resource--collection-snapshot workspace papercuts)))
      (make-instance 'papercut-observation
                     :uri        (resource-uri resource)
                     :revision   (resource-readable-snapshot-digest
                                  *papercut-resource-digest-key*
                                  snapshot)
                     :content    (papercut-resource--render-collection
                                  workspace papercuts)
                     :identifier "current"
                     :workspace  workspace
                     :kind       ':collection
                     :snapshot   snapshot))))

(-> papercut-resource--item-observation-from-report
    (papercut-item-resource papercut)
    papercut-observation)
(defun papercut-resource--item-observation-from-report (resource papercut)
  "Return RESOURCE's complete exact observation from active PAPERCUT."
  (let ((snapshot (papercut-resource--item-snapshot papercut)))
    (make-instance 'papercut-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *papercut-resource-digest-key*
                                snapshot)
                   :content    (papercut-resource--render-item papercut)
                   :identifier (papercut-identifier papercut)
                   :workspace  (papercut-workspace papercut)
                   :kind       ':item
                   :snapshot   snapshot)))

(-> papercut-resource--item-observation-unlocked
    (papercut-item-resource tool-context list)
    papercut-observation)
(defun papercut-resource--item-observation-unlocked (resource context active)
  "Observe one exact RESOURCE from private ACTIVE transaction state."
  (let* ((configuration (tool-context-configuration context))
         (workspace (papercut--workspace configuration)))
    (unless (string= workspace (papercut-resource-workspace resource))
      (error 'resource-revision-stale
             :uri               (resource-uri resource)
             :expected-revision "workspace-context"
             :actual-revision   nil))
    (let ((papercut
            (find (papercut-resource-identifier resource)
                  (papercut--workspace-reports configuration active)
                  :test #'string=
                  :key #'papercut-identifier)))
      (unless papercut
        (error 'papercut-resource-not-found
               :identifier (papercut-resource-identifier resource)))
      (papercut-resource--item-observation-from-report resource papercut))))

(-> papercut-resource--current-observation-unlocked
    (papercut-resource tool-context list)
    papercut-observation)
(defun papercut-resource--current-observation-unlocked (resource context active)
  "Return RESOURCE's current observation from private ACTIVE transaction state."
  (etypecase resource
    (papercut-collection-resource
     (papercut-resource--collection-observation-unlocked resource context active))
    (papercut-item-resource
     (papercut-resource--item-observation-unlocked resource context active))))

(defmethod resource-observe
    ((resource papercut-resource) (context tool-context))
  "Observe one papercut resource under the process-local and store locks."
  (with-lock-held (*papercut-lock*)
    (papercut--transact
     (tool-context-configuration context)
     (lambda (active)
       (values nil
               (papercut-resource--current-observation-unlocked resource context active)
               nil)))))


;;;; -- Conversation Observation State --


;;;; -- Papercut Operations --

(-> papercut-resource--required-text
    (json-object string string integer)
    non-empty-string)
(defun papercut-resource--required-text (arguments name field limit)
  "Return validated required text NAME from ARGUMENTS for papercut FIELD."
  (let ((value (tool-argument arguments name :required t)))
    (handler-case
        (papercut--validate-text value field limit)
      (papercut-error (condition)
        (error 'tool-error
               :message (autolith-error-message condition)
               :tool-name "resource.edit")))))

(-> papercut-resource--required-verdict
    (json-object)
    (member :improved :worse :unchanged :too-early))
(defun papercut-resource--required-verdict (arguments)
  "Return the validated assessment verdict from ARGUMENTS."
  (let ((value (tool-argument arguments "verdict" :required t)))
    (cond
      ((and (stringp value) (string= value "improved"))
       ':improved)
      ((and (stringp value) (string= value "worse"))
       ':worse)
      ((and (stringp value) (string= value "unchanged"))
       ':unchanged)
      ((and (stringp value) (string= value "too-early"))
       ':too-early)
      (t
       (error 'tool-error
              :message "Papercut assessment verdict must be improved, worse, unchanged, or too-early."
              :tool-name "resource.edit")))))

(-> papercut-resource--normalize-operation (t) list)
(defun papercut-resource--normalize-operation (operation)
  "Return one validated normalized papercut resource OPERATION."
  (unless (hash-table-p operation)
    (error 'tool-error
           :message "Papercut resource operations must be JSON objects."
           :tool-name "resource.edit"))
  (let ((name (tool-argument operation "op" :required t)))
    (unless (stringp name)
      (error 'tool-error
             :message "Papercut resource operation op must be a string."
             :tool-name "resource.edit"))
    (cond
      ((string= name "papercut-report")
       (resource-item-validate-operation-fields
        "Papercut"
        operation '("op" "title" "content") '("op" "title" "content"))
       (list :kind ':report
             :title (papercut-resource--required-text
                     operation "title" "title" *papercut-title-limit*)
             :content (papercut-resource--required-text
                       operation "content" "content" *papercut-content-limit*)))
      ((string= name "papercut-assess")
       (resource-item-validate-operation-fields
        "Papercut"
        operation '("op" "verdict" "note") '("op" "verdict" "note"))
       (list :kind ':assess
             :verdict (papercut-resource--required-verdict operation)
             :note (papercut-resource--required-text
                    operation
                    "note"
                    "assessment note"
                    *papercut-assessment-note-limit*)))
      ((string= name "papercut-close")
       (resource-item-validate-operation-fields
        "Papercut"
        operation '("op" "resolution") '("op" "resolution"))
       (list :kind ':close
             :resolution (papercut-resource--required-text
                          operation
                          "resolution"
                          "closure resolution"
                          *papercut-resolution-limit*)))
      (t
       (error 'tool-error
              :message
              "Papercut resource operation op must be papercut-report, papercut-assess, or papercut-close."
              :tool-name "resource.edit")))))

(-> papercut-resource--validate-operation-target
    (papercut-resource list)
    null)
(defun papercut-resource--validate-operation-target (resource operation)
  "Require normalized OPERATION to match RESOURCE's collection or item kind."
  (etypecase resource
    (papercut-collection-resource
     (unless (eq (getf operation :kind) ':report)
       (error 'tool-error
              :message "papercut:current accepts only papercut-report."
              :tool-name "resource.edit")))
    (papercut-item-resource
     (unless (member (getf operation :kind) '(:assess :close))
       (error 'tool-error
              :message "Exact papercut item URIs accept papercut-assess or papercut-close."
              :tool-name "resource.edit"))))
  nil)

(-> papercut-resource--closed-observation
    (papercut-item-resource papercut non-empty-string)
    papercut-observation)
(defun papercut-resource--closed-observation (resource papercut resolution)
  "Return a revisioned terminal observation for closed PAPERCUT."
  (let* ((identifier (papercut-identifier papercut))
         (snapshot (list :kind ':closed
                         :identifier identifier
                         :resolution resolution)))
    (make-instance 'papercut-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *papercut-resource-digest-key*
                                snapshot)
                   :content    (format nil
                                       "Papercut ~A was closed.~%resolution: ~A"
                                       identifier resolution)
                   :metadata   (list :closed t)
                   :identifier identifier
                   :workspace  (papercut-workspace papercut)
                   :kind       ':closed
                   :snapshot   snapshot)))

(defmethod resource-apply-operations
    ((resource papercut-resource) (context tool-context)
     &key base-revision operations)
  "Validate the revision and publish one papercut mutation in a single transaction."
  (unless (and (listp operations) (= (length operations) 1))
    (error 'tool-error
           :message "papercut: resources require exactly one operation per resource.edit call."
           :tool-name "resource.edit"))
  (let ((operation (papercut-resource--normalize-operation (first operations))))
    (papercut-resource--validate-operation-target resource operation)
    (let ((conversation (tool-context-conversation context))
          (configuration (tool-context-configuration context)))
      (with-lock-held (*papercut-lock*)
        (with-recursive-lock-held
            ((conversation-resource-observation-lock conversation))
          (let* ((state (resource-item-find-observation-state
                         conversation resource base-revision))
                 (base-observation (resource-observation-state-observation state))
                 (papercut
                   (papercut--transact
                    configuration
                    (lambda (active)
                      (let ((current
                              (handler-case
                                  (papercut-resource--current-observation-unlocked
                                   resource context active)
                                (papercut-resource-not-found ()
                                  (error 'resource-revision-stale
                                         :uri (resource-uri resource)
                                         :expected-revision base-revision
                                         :actual-revision nil)))))
                        (unless (and
                                 (string= (resource-observation-revision current)
                                          (resource-observation-revision base-observation))
                                 (equal (papercut-observation-snapshot current)
                                        (papercut-observation-snapshot base-observation)))
                          (error 'resource-revision-stale
                                 :uri (resource-uri resource)
                                 :expected-revision base-revision
                                 :actual-revision (resource-observation-revision current))))
                      (ecase (getf operation :kind)
                        (:report
                         (papercut--report-unlocked
                          configuration :title (getf operation :title)
                                        :content (getf operation :content)
                                        :source-conversation
                                        (conversation-identifier conversation)))
                        (:assess
                         (papercut--assess-unlocked
                          configuration (papercut-resource-identifier resource)
                          :active active :verdict (getf operation :verdict)
                                         :note (getf operation :note)))
                        (:close
                         (papercut--mark-closed-unlocked
                          configuration (papercut-resource-identifier resource)
                          (getf operation :resolution) :active active)))))))
            (ecase (getf operation :kind)
              (:report
               (let ((item-resource
                       (papercut-resource--make-item
                        (papercut-identifier papercut) (papercut-workspace papercut))))
                 (values
                  (papercut-resource--item-observation-from-report item-resource papercut)
                  (format nil "Reported papercut ~A." (papercut-identifier papercut))
                  (resource-uri item-resource))))
              (:assess
               (values
                (papercut-resource--item-observation-from-report resource papercut)
                (format nil "Assessed papercut ~A as ~(~A~)."
                        (papercut-resource-identifier resource) (getf operation :verdict))
                nil))
              (:close
               (values
                (papercut-resource--closed-observation
                 resource papercut (getf operation :resolution))
                (format nil "Closed papercut ~A." (papercut-resource-identifier resource))
                nil)))))))))


;;;; -- Resource Tool Methods --

(defmethod resource-tool-read
    ((resource papercut-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one papercut resource completely and retain its transient exact snapshot."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments))
            (nth-value 1 (gethash "query" arguments))
            (nth-value 1 (gethash "max-results" arguments)))
    (error 'tool-error
           :message "papercut: resources are always read in full and accept no read filters."
           :tool-name "resource.read"))
  (let* ((observation (resource-observe resource context))
         (state
            (resource-observation-state-ensure
            (tool-context-conversation context)
            observation)))
    (tool-success (resource-item-read-result state))))

(defmethod resource-tool-edit
    ((resource papercut-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply one revision-gated papercut operation and return a fresh exact observation."
  (declare (ignore tool))
  (let* ((uri (tool-argument arguments "uri" :required t))
         (base-revision (tool-argument arguments "base-revision" :required t))
         (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error
             :message "Resource edit base-revision must be a non-empty string."
             :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (= (length operation-array) 1))
      (error 'tool-error
             :message "papercut: resources require exactly one operation per resource.edit call."
             :tool-name "resource.edit"))
    (handler-case
        (multiple-value-bind (observation summary exact-uri)
            (resource-apply-operations resource context
                                       :base-revision base-revision
                                       :operations (coerce operation-array 'list))
          (let ((state
                   (resource-observation-state-ensure
                   (tool-context-conversation context)
                   observation)))
            (tool-success
             (format nil "~A~@[~%Exact URI: ~A~]~%~A"
                     summary
                     exact-uri
                     (resource-item-read-result state)))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, for another papercut URI or workspace, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri))))))