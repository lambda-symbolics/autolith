(in-package #:autolith)

;;;; -- Memory Resource Conditions --

(define-condition memory-resource-not-found (autolith-error)
  ((identifier
    :initarg :identifier
    :reader memory-resource-not-found-identifier
    :type non-empty-string
    :documentation "The stable memory identifier that has no active value."))
  (:default-initargs :message "The persistent memory does not exist.")
  (:documentation "A memory: URI names a missing or forgotten persistent memory.")
  (:report (lambda (condition stream)
             (format stream "Memory ~A does not exist."
                     (memory-resource-not-found-identifier condition)))))


;;;; -- Memory Resources --

(defparameter *memory-resource-maximum-observations* 16
  "The transient memory observations retained by one conversation.")

(defparameter *memory-resource-excerpt-limit* 240
  "The maximum memory-body characters shown for one collection entry.")

(defparameter *memory-resource-maximum-results* 50
  "The largest explicit result count accepted by one memory resource read.")

(defvar *memory-resource-digest-key* (random-data 16)
  "The process-local key identifying exact persistent-memory snapshots.")

(defclass memory-resource (resource)
  ((identifier
    :initarg :identifier
    :reader memory-resource-identifier
    :type non-empty-string
    :documentation "The collection name or stable memory identifier in this resource URI."))
  (:documentation "A revisioned persistent-memory collection or active item resource."))

(defclass memory-collection-resource (memory-resource)
  ((visibility
    :initarg :visibility
    :reader memory-collection-resource-visibility
    :type memory-visibility
    :documentation "The existing persistent-memory visibility selected by this collection."))
  (:documentation "A scoped collection of active persistent memories."))

(defclass memory-item-resource (memory-resource)
  ()
  (:documentation "One exact active persistent memory selected by stable identifier."))

(defclass memory-resolver (resource-resolver)
  ()
  (:documentation "Resolve scoped memory collections and exact active memories."))

(defclass memory-observation (resource-observation)
  ((identifier
    :initarg :identifier
    :reader memory-observation-identifier
    :type non-empty-string
    :documentation "The collection name or stable memory identifier observed.")
   (kind
    :initarg :kind
    :reader memory-observation-kind
    :type keyword
    :documentation "Whether the observation represents a collection or exact item.")
   (snapshot
    :initarg :snapshot
    :reader memory-observation-snapshot
    :type list
    :documentation "The exact detached readable memory snapshot, including empty state."))
  (:documentation "An exact rendered and structural persistent-memory snapshot."))

(defclass memory-observation-state (resource-observation-state)
  ()
  (:documentation "One conversation-local model observation of a memory: resource."))

(defmethod resource-observation-state-family-and-key
    ((observation memory-observation))
  "Return the memory state family and exact structural snapshot key."
  (values 'memory-observation-state (list (resource-observation-uri observation)
                                         (resource-observation-revision observation)
                                         (memory-observation-snapshot observation))))

(defmethod resource-observation-state-class ((resource memory-resource))
  "Record memory observations under the memory state family."
  (declare (ignore resource))
  'memory-observation-state)

(defmethod resource-item-identity ((resource memory-resource))
  "Compare memory observations by collection or stable identifier."
  (memory-resource-identifier resource))

(defmethod resource-observation-identity ((observation memory-observation))
  "Compare memory observations by collection or stable identifier."
  (memory-observation-identifier observation))

(defmethod resource-observation-state-maximum ((state memory-observation-state))
  "Return the configured memory observation limit."
  (declare (ignore state))
  *memory-resource-maximum-observations*)


;;;; -- URI Resolution --

(-> memory-resource--collection-visibility
    (non-empty-string)
    (option memory-visibility))
(defun memory-resource--collection-visibility (identifier)
  "Return the reserved collection visibility named by IDENTIFIER, or NIL."
  (cond
    ((string= identifier "relevant") ':relevant)
    ((string= identifier "workspace") ':workspace)
    ((string= identifier "global") ':global)
    ((string= identifier "all") ':all)
    (t
     nil)))

(-> memory-resource--make-item (non-empty-string) memory-item-resource)
(defun memory-resource--make-item (identifier)
  "Return one exact item resource with canonical URI identity."
  (make-instance 'memory-item-resource
                 :uri        (resource-item-uri "memory" identifier)
                 :identifier identifier))

(defmethod resource-resolver-resolve
    ((resolver memory-resolver) identifier (context tool-context))
  "Resolve reserved collections or one canonical or compatible exact identifier."
  (declare (ignore resolver context))
  (let ((visibility (memory-resource--collection-visibility identifier)))
    (cond
      (visibility
       (make-instance 'memory-collection-resource
                      :uri        (format nil "memory:~A" identifier)
                      :identifier identifier
                      :visibility visibility))
      ((uiop:string-prefix-p "id/" identifier)
       (memory-resource--make-item
        (resource-item-decode-identifier
         "memory" "memory"
         (subseq identifier (length "id/")))))
      (t
       (memory-resource--make-item identifier)))))

(defmethod resource-capabilities
    ((resource memory-resource) (context tool-context))
  "Expose only capabilities implemented by RESOURCE's concrete memory kind."
  (declare (ignore context))
  (etypecase resource
    (memory-item-resource
     '(:read :edit))
    (memory-collection-resource
     (if (member (memory-collection-resource-visibility resource)
                 '(:relevant :all))
         '(:read)
         '(:read :edit)))))


;;;; -- Exact Memory Observations --

(-> memory-resource--scope-label (memory) string)
(defun memory-resource--scope-label (memory)
  "Return a concise scope label for MEMORY."
  (if (eq (memory-scope memory) ':global)
      "global"
      (format nil "workspace ~A" (memory-workspace memory))))

(-> memory-resource--render-item (memory) string)
(defun memory-resource--render-item (memory)
  "Return complete model-visible MEMORY content and metadata."
  (format nil
          "id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~2%~A"
          (memory-identifier memory)
          (memory-resource--scope-label memory)
          (memory--timestamp-string (memory-created-at memory))
          (memory--timestamp-string (memory-updated-at memory))
          (if (memory-source-conversation memory)
              (conversation-identifier-display
               (memory-source-conversation memory))
              "unknown")
          (memory-title memory)
          (memory-tags memory)
          (memory-content memory)))

(-> memory-resource--collection-entry (memory) string)
(defun memory-resource--collection-entry (memory)
  "Return one complete metadata and bounded excerpt entry for MEMORY."
  (format nil
          "uri: ~A~%id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~%excerpt: ~A"
          (resource-item-uri "memory" (memory-identifier memory))
          (memory-identifier memory)
          (memory-resource--scope-label memory)
          (memory--timestamp-string (memory-created-at memory))
          (memory--timestamp-string (memory-updated-at memory))
          (if (memory-source-conversation memory)
              (conversation-identifier-display
               (memory-source-conversation memory))
              "unknown")
          (memory-title memory)
          (memory-tags memory)
          (memory--excerpt (memory-content memory)
                           *memory-resource-excerpt-limit*)))

(-> memory-resource--render-collection (non-empty-string list) string)
(defun memory-resource--render-collection (identifier memories)
  "Return a clear complete rendering of collection IDENTIFIER and MEMORIES."
  (if memories
      (format nil "collection: ~A~%count: ~D~2%~{~A~^~2%~}"
              identifier
              (length memories)
              (mapcar #'memory-resource--collection-entry memories))
      (format nil "collection: ~A~%count: 0~2%No matching memories."
              identifier)))

(-> memory-resource--workspace-identity (tool-context) non-empty-string)
(defun memory-resource--workspace-identity (context)
  "Return CONTEXT's canonical current working-directory identity."
  (namestring
   (platform-truename
    *platform*
    (configuration-working-directory
     (tool-context-configuration context)))))

(-> memory-resource--collection-workspace-identity
    (memory-collection-resource tool-context)
    (option non-empty-string))
(defun memory-resource--collection-workspace-identity (resource context)
  "Return the workspace identity that contributes to RESOURCE's observations."
  (unless (member (memory-collection-resource-visibility resource)
                  '(:global :all))
    (memory-resource--workspace-identity context)))

(-> memory-resource--collection-observation
    (memory-collection-resource tool-context
     &key (:query (option string)) (:maximum-results (option integer)))
    memory-observation)
(defun memory-resource--collection-observation
    (resource context &key query maximum-results)
  "Return RESOURCE's exact optionally filtered collection observation."
  (let* ((visibility
           (memory-collection-resource-visibility resource))
         (workspace-identity
           (memory-resource--collection-workspace-identity resource context))
         (configuration (tool-context-configuration context))
         (available
           (if query
               (memory-search configuration query :visibility visibility)
               (memory-list configuration :visibility visibility)))
         (memories
           (if maximum-results
               (subseq available 0 (min maximum-results (length available)))
               available))
         (snapshot
           (list :kind ':collection
                 :identifier (memory-resource-identifier resource)
                 :workspace workspace-identity
                 :query query
                 :records (mapcar #'memory--record memories))))
    (make-instance 'memory-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *memory-resource-digest-key*
                                snapshot)
                   :content    (memory-resource--render-collection
                                (memory-resource-identifier resource)
                                memories)
                   :metadata   (list :visibility visibility
                                     :workspace workspace-identity
                                     :query query
                                     :count (length memories))
                   :identifier (memory-resource-identifier resource)
                   :kind       ':collection
                   :snapshot   snapshot)))

(-> memory-resource--item-observation
    (memory-item-resource tool-context)
    memory-observation)
(defun memory-resource--item-observation (resource context)
  "Return RESOURCE's exact active memory observation under CONTEXT."
  (let* ((identifier (memory-resource-identifier resource))
         (memory
           (memory-find (tool-context-configuration context) identifier)))
    (unless memory
      (error 'memory-resource-not-found :identifier identifier))
    (let ((snapshot
            (list :kind ':item
                  :identifier identifier
                  :record (memory--record memory))))
      (make-instance 'memory-observation
                     :uri        (resource-uri resource)
                     :revision   (resource-readable-snapshot-digest
                                  *memory-resource-digest-key*
                                  snapshot)
                     :content    (memory-resource--render-item memory)
                     :metadata   (list :scope (memory-scope memory))
                     :identifier identifier
                     :kind       ':item
                     :snapshot   snapshot))))

(defmethod resource-observe
    ((resource memory-collection-resource) (context tool-context))
  "Observe one complete scoped persistent-memory collection."
  (memory-resource--collection-observation resource context))

(defmethod resource-observe
    ((resource memory-item-resource) (context tool-context))
  "Observe one complete exact active persistent memory."
  (memory-resource--item-observation resource context))


;;;; -- Conversation Observation State --



;;;; -- Memory Operations --

(-> memory-resource--operation-tags (json-object) list)
(defun memory-resource--operation-tags (operation)
  "Return OPERATION's validated optional tag array as a detached list."
  (multiple-value-bind (tags supplied-p)
      (gethash "tags" operation)
    (cond
      ((not supplied-p)
       nil)
      ((and (vectorp tags) (every #'stringp tags))
       (memory--validate-tags (coerce tags 'list)))
      (t
       (error 'tool-error
              :message "Memory resource tags must be an array of strings."
              :tool-name "resource.edit")))))

(-> memory-resource--operation-scope (json-object) (option memory-scope))
(defun memory-resource--operation-scope (operation)
  "Return OPERATION's optional validated replacement scope."
  (multiple-value-bind (scope supplied-p)
      (gethash "scope" operation)
    (cond
      ((not supplied-p)
       nil)
      ((and (stringp scope) (string-equal scope "global"))
       ':global)
      ((and (stringp scope) (string-equal scope "workspace"))
       ':workspace)
      (t
       (error 'tool-error
              :message "Memory resource scope must be global or workspace."
              :tool-name "resource.edit")))))

(-> memory-resource--normalize-operation (t) list)
(defun memory-resource--normalize-operation (operation)
  "Return one completely validated normalized memory resource OPERATION."
  (unless (hash-table-p operation)
    (error 'tool-error
           :message "Memory resource operations must be JSON objects."
           :tool-name "resource.edit"))
  (let ((name (tool-argument operation "op" :required t)))
    (unless (stringp name)
      (error 'tool-error
             :message "Memory resource operation op must be a string."
             :tool-name "resource.edit"))
    (cond
      ((string= name "memory-remember")
       (resource-item-validate-operation-fields
     "Memory"
        operation
        '("op" "title" "content" "tags")
        '("op" "title" "content"))
       (list :kind ':remember
             :title (memory--validate-text
                     (tool-argument operation "title" :required t)
                     "title"
                     *memory-title-limit*)
             :content (memory--validate-text
                       (tool-argument operation "content" :required t)
                       "content"
                       *memory-content-limit*)
             :tags (memory-resource--operation-tags operation)))
      ((string= name "memory-replace")
       (resource-item-validate-operation-fields
     "Memory"
        operation
        '("op" "title" "content" "tags" "scope")
        '("op" "title" "content"))
       (list :kind ':replace
             :title (memory--validate-text
                     (tool-argument operation "title" :required t)
                     "title"
                     *memory-title-limit*)
             :content (memory--validate-text
                       (tool-argument operation "content" :required t)
                       "content"
                       *memory-content-limit*)
             :tags (memory-resource--operation-tags operation)
             :scope (memory-resource--operation-scope operation)))
      ((string= name "memory-forget")
       (resource-item-validate-operation-fields
     "Memory"
        operation '("op") '("op"))
       (list :kind ':forget))
      (t
       (error 'tool-error
              :message
              "Memory resource operation op must be memory-remember, memory-replace, or memory-forget."
              :tool-name "resource.edit")))))

(-> memory-resource--validate-operation-target (memory-resource list) null)
(defun memory-resource--validate-operation-target (resource operation)
  "Require normalized OPERATION to match RESOURCE's collection or item identity."
  (etypecase resource
    (memory-collection-resource
     (unless (and (eq (getf operation :kind) ':remember)
                  (member (memory-collection-resource-visibility resource)
                          '(:workspace :global)))
       (error 'tool-error
              :message "Only memory:workspace and memory:global accept memory-remember."
              :code ':operation-unsupported
              :tool-name "resource.edit")))
    (memory-item-resource
     (unless (member (getf operation :kind) '(:replace :forget))
       (error 'tool-error
              :message "Exact memory item URIs accept only memory-replace or memory-forget."
              :code ':operation-unsupported
              :tool-name "resource.edit"))))
  nil)

(-> memory-resource--current-observation
    (memory-resource tool-context)
    memory-observation)
(defun memory-resource--current-observation (resource context)
  "Return RESOURCE's current exact observation while the caller holds the memory lock."
  (etypecase resource
    (memory-collection-resource
     (memory-resource--collection-observation resource context))
    (memory-item-resource
     (memory-resource--item-observation resource context))))

(-> memory-resource--forgotten-observation
    (memory-item-resource memory)
    memory-observation)
(defun memory-resource--forgotten-observation (resource memory)
  "Return a revisioned terminal observation for forgotten MEMORY."
  (let* ((identifier (memory-identifier memory))
         (snapshot (list :kind ':forgotten :identifier identifier)))
    (make-instance 'memory-observation
                   :uri        (resource-uri resource)
                   :revision   (resource-readable-snapshot-digest
                                *memory-resource-digest-key*
                                snapshot)
                   :content    (format nil "Memory ~A was forgotten." identifier)
                   :metadata   (list :forgotten t)
                   :identifier identifier
                   :kind       ':forgotten
                   :snapshot   snapshot)))

(defmethod resource-apply-operations
    ((resource memory-resource) (context tool-context)
     &key base-revision operations)
  "Apply exactly one revision-gated append-only mutation to RESOURCE."
  (unless (and (listp operations) (= (length operations) 1))
    (error 'tool-error
           :message "memory: resources require exactly one operation per resource.edit call."
           :tool-name "resource.edit"))
  (let ((operation (memory-resource--normalize-operation (first operations))))
    (memory-resource--validate-operation-target resource operation)
    (let ((conversation (tool-context-conversation context))
          (configuration (tool-context-configuration context)))
      (with-recursive-lock-held (*memory-lock*)
        (with-recursive-lock-held
            ((conversation-resource-observation-lock conversation))
          (let* ((state
                   (resource-item-find-observation-state
                    conversation resource base-revision))
                 (base-observation
                   (resource-observation-state-observation state))
                 (current
                   (handler-case
                       (memory-resource--current-observation resource context)
                     (memory-resource-not-found ()
                       (error 'resource-revision-stale
                              :uri               (resource-uri resource)
                              :expected-revision base-revision
                              :actual-revision   nil)))))
            (unless (and
                     (string= (resource-observation-revision current)
                              (resource-observation-revision base-observation))
                     (equal (memory-observation-snapshot current)
                            (memory-observation-snapshot base-observation)))
              (error 'resource-revision-stale
                     :uri               (resource-uri resource)
                     :expected-revision base-revision
                     :actual-revision
                     (resource-observation-revision current)))
            (case (getf operation :kind)
              (:remember
               (let* ((scope
                        (ecase (memory-collection-resource-visibility resource)
                          (:workspace ':workspace)
                          (:global ':global)))
                      (memory
                        (memory-remember
                         configuration
                         :title (getf operation :title)
                         :content (getf operation :content)
                         :scope scope
                         :tags (getf operation :tags)
                         :source-conversation
                         (conversation-identifier conversation)))
                      (item-resource
                        (memory-resource--make-item
                         (memory-identifier memory))))
                 (values
                  (memory-resource--item-observation item-resource context)
                  (format nil "Created memory ~A."
                          (memory-identifier memory))
                  (resource-uri item-resource))))
              (:replace
               (memory-remember
                configuration
                :identifier (memory-resource-identifier resource)
                :title (getf operation :title)
                :content (getf operation :content)
                :scope (getf operation :scope)
                :tags (getf operation :tags)
                :source-conversation
                (conversation-identifier conversation))
               (values
                (memory-resource--item-observation resource context)
                (format nil "Replaced memory ~A."
                        (memory-resource-identifier resource))
                (resource-uri resource)))
              (:forget
               (let ((memory
                       (memory-forget
                        configuration
                        (memory-resource-identifier resource)
                        :source-conversation
                        (conversation-identifier conversation))))
                 (values
                  (memory-resource--forgotten-observation resource memory)
                  (format nil "Forgot memory ~A."
                          (memory-resource-identifier resource))
                  nil))))))))))


;;;; -- Resource Tool Methods --

(defmethod resource-tool-read
    ((resource memory-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one memory resource completely and retain its transient exact snapshot."
  (declare (ignore tool))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error
           :message "memory: resources are always read in full and do not accept line windows."
           :code ':line-windows-unsupported
           :tool-name "resource.read"))
  (let* ((query (tool-argument arguments "query"))
         (requested-maximum (tool-argument arguments "max-results")))
    (when (and query (not (non-empty-string-p query)))
      (error 'tool-error
             :message "Memory resource query must be a non-empty string."
             :tool-name "resource.read"))
    (when (and requested-maximum
               (not (and (integerp requested-maximum)
                         (plusp requested-maximum))))
      (error 'tool-error
             :message "Memory resource max-results must be a positive integer."
             :tool-name "resource.read"))
    (when (and (typep resource 'memory-item-resource)
               (or query requested-maximum))
      (error 'tool-error
             :message "Exact memory item resources do not accept query or max-results."
             :tool-name "resource.read"))
    (let* ((observation
             (etypecase resource
               (memory-collection-resource
                (memory-resource--collection-observation
                 resource context
                 :query query
                 :maximum-results
                 (and requested-maximum
                      (min requested-maximum
                           *memory-resource-maximum-results*))))
               (memory-item-resource
                (resource-observe resource context))))
           (state
              (resource-observation-state-ensure
              (tool-context-conversation context)
              observation)))
      (tool-success (resource-item-read-result state)))))

(defmethod resource-tool-edit
    ((resource memory-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Apply one revision-gated memory operation and return a fresh exact observation."
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
             :message "memory: resources require exactly one operation per resource.edit call."
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
         (format nil "Resource revision ~A is stale, expired, for another memory URI, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri))))))
