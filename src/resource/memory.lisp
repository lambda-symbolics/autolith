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
  ((knowledge-p
    :initarg :knowledge-p
    :initform t
    :reader memory-resolver-knowledge-p
    :type boolean
    :documentation "Whether structured knowledge views resolve and are documented."))
  (:documentation "Resolve scoped memory collections, exact active memories, and knowledge views."))

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
                                         (memory-observation-snapshot observation)
                                         (resource-observation-metadata observation))))

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

(defmethod resource-resolver-read-documentation ((resolver memory-resolver))
  "Document memory: reads, with structured knowledge views when they are offered."
  (format nil "memory: exposes revisioned persistent memories, always read in full: memory:relevant, memory:all, or a canonical memory:id/<percent-encoded-stable-id> URI.~:[~; Structured knowledge: memory:facts lists facts holding now (query and max-results narrow it), memory:claim/<id> explains why one holds, memory:predicates, memory:rules, memory:methods, and memory:contracts list the vocabulary, rules, task methods, and tool contracts, memory:procedures lists learned procedures procedure.run makes, and memory:changes reports what changed since this conversation last read it.~]"
          (memory-resolver-knowledge-p resolver)))

(defmethod resource-resolver-edit-documentation ((resolver memory-resolver))
  "Document memory: edits, with structured knowledge operations when they are offered."
  (format nil "memory:workspace and memory:global create with memory-remember, canonical exact memory:id/<percent-encoded-stable-id> resources accept memory-replace or memory-forget, and memory:relevant is read-only. When a memory restates a file, a web page, or the user's words, cite them as sources; when it is derived from other memories, name them as premises.~:[~; memory:facts accepts up to 16 operations per edit: predicate-define, fact-assert, fact-retract, rule-propose, method-define, and contract-define. Record a value you will need again as a fact with its source, and propose a rule when the same calculation from facts recurs: rules recompute it locally whenever its inputs change.~]"
          (memory-resolver-knowledge-p resolver)))

(defmethod resource-resolver-resolve
    ((resolver memory-resolver) identifier (context tool-context))
  "Resolve reserved collections or one canonical or compatible exact identifier."
  (declare (ignore context))
  (let ((visibility (memory-resource--collection-visibility identifier))
        (knowledge (and (memory-resolver-knowledge-p resolver)
                        (memory-knowledge-resolve identifier))))
    (cond
      (knowledge
       knowledge)
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

(-> memory-resource--status-line (memory-assessment) string)
(defun memory-resource--status-line (assessment)
  "Return a status line for a stale or unverified ASSESSMENT, or an empty string."
  (if (eq (memory-assessment-status assessment) ':current)
      ""
      (format nil "status: ~(~A~) (~A)~%"
              (memory-assessment-status assessment)
              (memory-assessment-summary assessment))))

(-> memory-resource--source-line (list) string)
(defun memory-resource--source-line (outcome)
  "Return one rendered source line for a source OUTCOME plist."
  (let* ((source (getf outcome :source))
         (properties (rest source))
         (quote (getf properties :quote)))
    (format nil "  - ~A~@[ \"~A\"~]~A~%"
            (ecase (first source)
              (:file
               (format nil "file ~A" (memory-source-file-display source)))
              (:url
               (format nil "url ~A" (getf properties :url)))
              (:user
               "user said"))
            quote
            (case (getf (getf outcome :reason) :kind)
              ((nil) "")
              (:file-changed " (changed)")
              (:file-missing " (missing)")
              (otherwise " (could not be checked)")))))

(-> memory-resource--premise-line (list) string)
(defun memory-resource--premise-line (outcome)
  "Return one rendered premise line for a premise OUTCOME plist."
  (let ((memory (getf outcome :memory))
        (claim (getf outcome :claim))
        (reason (getf outcome :reason)))
    (format nil "  - ~A~@[ \"~A\"~]~@[ (~A)~]~%"
            (if (or claim (eq (getf reason :kind) ':premise-claim))
                (format nil "memory:claim/~A" (getf outcome :id))
                (resource-item-uri "memory" (getf outcome :id)))
            (cond
              (memory (memory-title memory))
              (claim (knowledge-node-text claim))
              (t (getf reason :title)))
            (and reason
                 (ecase (getf reason :kind)
                   (:premise-changed "changed since this memory was written")
                   (:premise-forgotten "forgotten")
                   (:premise-stale "stale")
                   (:premise-unverified "unverified")
                   (:premise-cycle "relies on this memory")
                   (:premise-too-deep "too deep to check")
                   (:premise-claim (if (eq (getf reason :applicability) ':unknown)
                                       "no longer holds"
                                       (format nil "~(~A~)" (getf reason :applicability)))))))))

(-> memory-resource--render-item (memory-assessment list) string)
(defun memory-resource--render-item (assessment dependents)
  "Return complete model-visible content and evidence for ASSESSMENT's memory.

DEPENDENTS are the memories derived from it. A current memory without
evidence or dependents renders exactly as memories always have."
  (let ((memory (memory-assessment-memory assessment)))
    (format nil
            "id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~%~A~@[sources:~%~{~A~}~]~@[premises:~%~{~A~}~]~@[relied on by: ~{~A~^, ~}~%~]~%~A"
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
            (memory-resource--status-line assessment)
            (mapcar #'memory-resource--source-line
                    (memory-assessment-source-outcomes assessment))
            (mapcar #'memory-resource--premise-line
                    (memory-assessment-premise-outcomes assessment))
            (mapcar (lambda (dependent)
                      (resource-item-uri "memory" (memory-identifier dependent)))
                    dependents)
            (memory-content memory))))

(-> memory-resource--collection-entry (memory-assessment) string)
(defun memory-resource--collection-entry (assessment)
  "Return one complete metadata and bounded excerpt entry for ASSESSMENT's memory."
  (let ((memory (memory-assessment-memory assessment)))
    (format nil
            "uri: ~A~%id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~%~Aexcerpt: ~A"
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
            (memory-resource--status-line assessment)
            (memory--excerpt (memory-content memory)
                             *memory-resource-excerpt-limit*))))

(-> memory-resource--render-collection (non-empty-string list) string)
(defun memory-resource--render-collection (identifier assessments)
  "Return a clear complete rendering of collection IDENTIFIER and ASSESSMENTS."
  (if assessments
      (format nil "collection: ~A~%count: ~D~2%~{~A~^~2%~}"
              identifier
              (length assessments)
              (mapcar #'memory-resource--collection-entry assessments))
      (format nil "collection: ~A~%count: 0~2%No matching memories."
              identifier)))

(-> memory-resource--status-metadata (list) list)
(defun memory-resource--status-metadata (assessments)
  "Return the identifiers and explanations of stale or unverified ASSESSMENTS.

Observation metadata carries them, so rereading a memory whose evidence changed
returns a fresh observation instead of the earlier rendering."
  (loop for assessment in assessments
        unless (eq (memory-assessment-status assessment) ':current)
          collect (list (memory-identifier (memory-assessment-memory assessment))
                        (memory-assessment-status assessment)
                        (memory-assessment-summary assessment))))

(-> memory-resource--workspace-identity (tool-context) non-empty-string)
(defun memory-resource--workspace-identity (context)
  "Return CONTEXT's canonical current working-directory identity."
  (namestring
   (platform-truename
    *platform*
    (config :working-directory
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
           (snapshot-memories
             (memory-list configuration :visibility visibility))
           (available
             (if query
                 (memory-search configuration query :visibility visibility)
                 snapshot-memories))
           (memories
             (if maximum-results
                 (subseq available 0 (min maximum-results (length available)))
                 available))
         (snapshot
           (list :kind ':collection
                 :identifier (memory-resource-identifier resource)
                 :workspace workspace-identity
                 :records (mapcar #'memory--record snapshot-memories))))
    (let* ((assessor (memory-assessor-create configuration))
           (assessments (mapcar (lambda (memory)
                                  (memory-assess assessor memory))
                                memories)))
      (make-instance 'memory-observation
                     :uri        (resource-uri resource)
                     :revision   (resource-readable-snapshot-digest
                                  *memory-resource-digest-key*
                                  snapshot)
                     :content    (memory-resource--render-collection
                                  (memory-resource-identifier resource)
                                  assessments)
                     :metadata   (list :visibility visibility
                                       :workspace workspace-identity
                                       :query query
                                       :maximum-results maximum-results
                                       :count (length memories)
                                       :status (memory-resource--status-metadata
                                                assessments))
                     :identifier (memory-resource-identifier resource)
                     :kind       ':collection
                     :snapshot   snapshot))))

(-> memory-resource--item-observation
    (memory-item-resource tool-context)
    memory-observation)
(defun memory-resource--item-observation (resource context)
  "Return RESOURCE's exact active memory observation under CONTEXT."
  (let* ((identifier (memory-resource-identifier resource))
         (assessor
           (memory-assessor-create (tool-context-configuration context)))
         (memory (memory-assessor-find assessor identifier)))
    (unless memory
      (error 'memory-resource-not-found :identifier identifier))
    (let ((snapshot
            (list :kind ':item
                  :identifier identifier
                  :record (memory--record memory)))
          (assessment (memory-assess assessor memory))
          (dependents (memory-assessor-dependents assessor identifier)))
      (make-instance 'memory-observation
                     :uri        (resource-uri resource)
                     :revision   (resource-readable-snapshot-digest
                                  *memory-resource-digest-key*
                                  snapshot)
                     :content    (memory-resource--render-item assessment dependents)
                     :metadata   (list :scope (memory-scope memory)
                                       :status (memory-resource--status-metadata
                                                (list assessment))
                                       :dependents (mapcar #'memory-identifier
                                                           dependents))
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

(-> memory-resource--fail (string &rest t) nil)
(defun memory-resource--fail (control &rest arguments)
  "Signal a corrective memory resource edit failure."
  (error 'tool-error
         :message (apply #'format nil control arguments)
         :tool-name "resource.edit"))

(-> memory-resource--source-request (t) list)
(defun memory-resource--source-request (value)
  "Return one validated source request from a model-supplied JSON source object."
  (unless (hash-table-p value)
    (memory-resource--fail "Each memory source must be an object."))
  (let ((kinds (remove-if-not (lambda (name)
                                (nth-value 1 (gethash name value)))
                              '("file" "url" "user")))
        (quote (gethash "quote" value)))
    (unless (= (length kinds) 1)
      (memory-resource--fail "Each memory source names exactly one of file, url, or user."))
    (loop for name being the hash-keys of value
          unless (member name (if (string= (first kinds) "user")
                                  '("user")
                                  (list (first kinds) "quote"))
                         :test #'string=)
            do (memory-resource--fail "A ~A memory source does not accept field ~A."
                                      (first kinds) name))
    (let ((target (gethash (first kinds) value)))
      (unless (non-empty-string-p target)
        (memory-resource--fail "Memory source ~A must be a non-empty string." (first kinds)))
      (when (and quote (not (memory-source-quote-p quote)))
        (memory-resource--fail "A source quote needs ~D to ~D characters of the cited text."
                               *memory-quote-minimum* *memory-quote-limit*))
      (cond
        ((string= (first kinds) "file")
         (list :file :path target :quote quote))
        ((string= (first kinds) "url")
         (unless (and (<= (length target) *memory-source-url-limit*)
                      (or (uiop:string-prefix-p "https://" target)
                          (uiop:string-prefix-p "http://" target)))
           (memory-resource--fail "A url memory source must be an http or https address."))
         (list :url :url target :quote quote))
        (t
         (unless (memory-source-quote-p target)
           (memory-resource--fail "A user memory source quotes ~D to ~D characters of the user's words."
                                  *memory-quote-minimum* *memory-quote-limit*))
         (list :user :quote target))))))

(-> memory-resource--operation-sources (json-object) (values list boolean))
(defun memory-resource--operation-sources (operation)
  "Return OPERATION's validated source requests and whether it supplied the field."
  (multiple-value-bind (sources supplied-p)
      (gethash "sources" operation)
    (cond
      ((not supplied-p)
       (values nil nil))
      ((not (vectorp sources))
       (memory-resource--fail "Memory sources must be an array of source objects."))
      ((> (length sources) *memory-source-count-limit*)
       (memory-resource--fail "A memory may cite at most ~D sources."
                              *memory-source-count-limit*))
      (t
       (values (map 'list #'memory-resource--source-request sources) t)))))

(-> memory-resource--premise-identifier (t) non-empty-string)
(defun memory-resource--premise-identifier (value)
  "Return the memory identifier named by a premise ID or canonical memory URI."
  (unless (non-empty-string-p value)
    (memory-resource--fail "Memory premises must be memory identifiers or memory:id/ URIs."))
  (if (uiop:string-prefix-p "memory:id/" value)
      (resource-item-decode-identifier "memory" "memory"
                                       (subseq value (length "memory:id/")))
      value))

(-> memory-resource--operation-premises (json-object) (values list boolean))
(defun memory-resource--operation-premises (operation)
  "Return OPERATION's premise identifiers and whether it supplied the field."
  (multiple-value-bind (premises supplied-p)
      (gethash "premises" operation)
    (cond
      ((not supplied-p)
       (values nil nil))
      ((not (vectorp premises))
       (memory-resource--fail "Memory premises must be an array of memory identifiers."))
      (t
       (values (map 'list #'memory-resource--premise-identifier premises) t)))))

(-> memory-resource--observe-source (tool-context list timestamp) list)
(defun memory-resource--observe-source (context request time)
  "Return the source record for REQUEST after checking it against its evidence.

A file is resolved and authorized like any tool path. A user quote must occur
in one of the user's own inputs to this conversation."
  (ecase (first request)
    (:file
     (let ((pathname (workspace-tool-path context (getf (rest request) :path)
                                          :tool-name "resource.edit")))
       (handler-case
           (memory-source-observe-file pathname
                                       :quote (getf (rest request) :quote)
                                       :time time)
         (memory-error (condition)
           (memory-resource--fail "~A" (autolith-error-message condition))))))
    (:url
     (list :url
           :url (getf (rest request) :url)
           :quote (getf (rest request) :quote)
           :observed-at time))
    (:user
     (let ((conversation (tool-context-conversation context))
           (quote (getf (rest request) :quote)))
       (unless (some (lambda (input)
                       (memory-source-quote-found-p quote input))
                     (conversation-input-history conversation))
         (memory-resource--fail "The quote does not occur in the user's messages in this conversation. Quote the user's words exactly."))
       (list :user
             :quote quote
             :conversation (conversation-identifier conversation)
             :observed-at time)))))

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
        '("op" "title" "content" "tags" "sources" "premises")
        '("op" "title" "content"))
       (multiple-value-bind (sources sources-p)
           (memory-resource--operation-sources operation)
         (multiple-value-bind (premises premises-p)
             (memory-resource--operation-premises operation)
           (list :kind ':remember
                 :title (memory--validate-text
                         (tool-argument operation "title" :required t)
                         "title"
                         *memory-title-limit*)
                 :content (memory--validate-text
                           (tool-argument operation "content" :required t)
                           "content"
                           *memory-content-limit*)
                 :tags (memory-resource--operation-tags operation)
                 :sources sources
                 :sources-p sources-p
                 :premises premises
                 :premises-p premises-p))))
      ((string= name "memory-replace")
       (resource-item-validate-operation-fields
     "Memory"
        operation
        '("op" "title" "content" "tags" "scope" "sources" "premises")
        '("op" "title" "content"))
       (multiple-value-bind (sources sources-p)
           (memory-resource--operation-sources operation)
         (multiple-value-bind (premises premises-p)
             (memory-resource--operation-premises operation)
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
                 :scope (memory-resource--operation-scope operation)
                 :sources sources
                 :sources-p sources-p
                 :premises premises
                 :premises-p premises-p))))
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

(-> memory-resource--remember
    (configuration &key (:identifier (option string)) (:title string)
                        (:content string) (:scope (option memory-scope))
                        (:tags list) (:source-conversation (option string))
                        (:evidence-arguments list))
    memory)
(defun memory-resource--remember
    (configuration &key identifier title content scope tags source-conversation
                        evidence-arguments)
  "Remember or replace a memory, reporting invalid premises or sources correctively.

EVIDENCE-ARGUMENTS holds :SOURCES and :PREMISES only when the edit supplied
them, so a replacement that omits them keeps the recorded versions."
  (handler-case
      (apply #'memory-remember
             configuration
             :identifier          identifier
             :title               title
             :content             content
             :scope               scope
             :tags                tags
             :source-conversation source-conversation
             evidence-arguments)
    (memory-error (condition)
      (memory-resource--fail "~A" (autolith-error-message condition)))))

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
    (let* ((conversation (tool-context-conversation context))
           (configuration (tool-context-configuration context))
           (now (get-universal-time))
           ;; Evidence is checked, and outside paths authorized, before the
           ;; memory lock is taken.
           (evidence-arguments
             (append
              (and (getf operation :sources-p)
                   (list :sources
                         (mapcar (lambda (request)
                                   (memory-resource--observe-source context request now))
                                 (getf operation :sources))))
              (and (getf operation :premises-p)
                   (list :premises (getf operation :premises))))))
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
                        (memory-resource--remember
                         configuration
                         :title (getf operation :title)
                         :content (getf operation :content)
                         :scope scope
                         :tags (getf operation :tags)
                         :source-conversation
                         (conversation-identifier conversation)
                         :evidence-arguments evidence-arguments))
                      (item-resource
                        (memory-resource--make-item
                         (memory-identifier memory))))
                 (values
                  (memory-resource--item-observation item-resource context)
                  (format nil "Created memory ~A."
                          (memory-identifier memory))
                  (resource-uri item-resource))))
              (:replace
               (memory-resource--remember
                configuration
                :identifier (memory-resource-identifier resource)
                :title (getf operation :title)
                :content (getf operation :content)
                :scope (getf operation :scope)
                :tags (getf operation :tags)
                :source-conversation
                (conversation-identifier conversation)
                :evidence-arguments evidence-arguments)
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
                  (format nil "Forgot memory ~A.~@[ These memories relied on it and are now stale: ~{~A~^, ~}.~]"
                          (memory-resource-identifier resource)
                          (mapcar (lambda (dependent)
                                    (format nil "~A \"~A\""
                                            (resource-item-uri
                                             "memory" (memory-identifier dependent))
                                            (memory-title dependent)))
                                  (memory-assessor-dependents
                                   (memory-assessor-create configuration)
                                   (memory-identifier memory))))
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
        (resource-revision-stale (condition)
          (tool-failure
           (if (resource-revision-stale-actual-revision condition)
               (format nil "Resource revision ~A is stale: ~A changed to revision ~A. Reread ~A with resource.read and retry against the returned revision."
                       base-revision
                       (resource-revision-stale-uri condition)
                       (resource-revision-stale-actual-revision condition)
                       uri)
                (format nil "Resource revision ~A is stale, expired, for another memory URI, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                        base-revision
                        uri)))))))
