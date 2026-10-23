(in-package #:autolith)

;;;; -- Memory Resource Test Support --

(-> memory-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun memory-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> memory-resource-tests--edit
    (tool-registry tool-context string string json-object)
    tool-result)
(defun memory-resource-tests--edit (registry context uri revision operation)
  "Apply one revision-gated memory resource OPERATION."
  (memory-resource-tests--call
   registry context "resource" "edit"
   "uri" uri
   "base-revision" revision
   "operations" (vector operation)))

(-> memory-resource-tests--field (string string) string)
(defun memory-resource-tests--field (content label)
  "Return the single-line value following LABEL in memory resource CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing memory resource field ~S in ~S." label content))
    (subseq content value-start end)))

(-> memory-resource-tests--observation
    (conversation non-empty-string)
    memory-observation)
(defun memory-resource-tests--observation (conversation alias)
  "Return CONVERSATION's typed memory observation for ALIAS."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           'memory-observation-state)))
    (unless state
      (error "Missing memory observation state ~A." alias))
    (resource-observation-state-observation state)))

(-> memory-resource-tests--uri-identifier (non-empty-string) non-empty-string)
(defun memory-resource-tests--uri-identifier (uri)
  "Return the stable memory identifier represented by canonical item URI."
  (let ((prefix "memory:id/"))
    (unless (uiop:string-prefix-p prefix uri)
      (error "Memory item URI ~S is not canonical." uri))
    (resource-item-decode-identifier "memory" "memory" (subseq uri (length prefix)))))

(-> memory-resource-tests--persist-identifier
    (configuration non-empty-string non-empty-string non-empty-string)
    memory)
(defun memory-resource-tests--persist-identifier
    (configuration identifier title content)
  "Persist and return one fixture with exact stable IDENTIFIER."
  (let* ((now (get-universal-time))
         (memory
           (make-instance 'memory
                          :identifier identifier
                          :created-at now
                          :updated-at now
                          :scope ':workspace
                          :workspace
                          (namestring
                           (config :working-directory configuration))
                          :title title
                          :content content
                          :tags '("resource-uri-fixture")
                          :source-conversation "memory-resource-first")))
    (with-recursive-lock-held (*memory-lock*)
      (memory--transact
       configuration
       (lambda (active)
         (declare (ignore active))
         (values (list (memory--record memory)) nil t))))
    memory))


;;;; -- Memory Resource Tests --

(-> test-memory-resources () null)
(defun test-memory-resources ()
  "Test scoped and exact memory resource reads and compatibility."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (first-workspace (merge-pathnames "memory-first/" root))
         (second-workspace (merge-pathnames "memory-second/" root))
         (configuration nil)
         (second-configuration nil)
         (empty-configuration (test-configuration))
         (empty-root (test-configuration-root empty-configuration))
         (registry (make-default-tool-registry)))
    (ensure-directories-exist (merge-pathnames "marker" first-workspace))
    (ensure-directories-exist (merge-pathnames "marker" second-workspace))
    (setf configuration
          (configuration-copy base-configuration
                                :working-directory first-workspace)
          second-configuration
          (configuration-copy base-configuration
                                :working-directory second-workspace))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-first"))
                (second-conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-second"))
                (empty-conversation
                  (conversation-create empty-configuration
                                       :identifier "memory-resource-empty"))
                (first-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation))
                (second-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation second-conversation))
                (other-workspace-context
                  (make-instance 'tool-context
                                 :configuration second-configuration
                                 :worker nil
                                 :conversation second-conversation))
                (empty-context
                  (make-instance 'tool-context
                                 :configuration empty-configuration
                                 :worker nil
                                 :conversation empty-conversation))
                (child-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation
                                 :agent (allocate-instance
                                         (find-class 'task-child-agent))))
                (workspace-memory
                  (memory-remember
                   configuration
                   :title "First workspace guidance"
                   :content "Use the first workspace release procedure in full."
                   :tags '("first" "release")
                   :source-conversation "memory-resource-first"))
                (global-memory
                  (memory-remember
                   configuration
                   :title "Global response preference"
                   :content "Keep persistent answers direct and evidence based."
                   :scope ':global
                   :tags '("global" "style")
                   :source-conversation "memory-resource-first"))
                (other-memory
                  (memory-remember
                   second-configuration
                   :title "Second workspace secret"
                   :content "Only the second workspace collection may list this."
                   :tags '("second")
                   :source-conversation "memory-resource-second"))
                (reserved-memories
                  (loop for identifier in '("relevant" "workspace" "global")
                        collect
                        (memory-resource-tests--persist-identifier
                         configuration
                         identifier
                         (format nil "Reserved identifier ~A" identifier)
                         "This exact memory must not be shadowed by a collection URI.")))
                (encoded-memory
                  (memory-resource-tests--persist-identifier
                   configuration
                   "stable/id % žluť"
                   "Percent-encoded identifier"
                   "Canonical exact URIs support every persisted string identifier.")))
           (labels ((call (context namespace name &rest arguments)
                      "Execute one memory resource fixture call."
                      (apply #'memory-resource-tests--call
                             registry context namespace name arguments))

                    (read-resource (context uri &rest arguments)
                      "Read URI under CONTEXT with optional ARGUMENTS."
                      (apply #'call context "resource" "read"
                             "uri" uri arguments))

                    (edit-resource (context uri alias operation)
                      "Apply one revision-gated memory resource OPERATION."
                      (call context "resource" "edit"
                            "uri" uri
                            "base-revision" alias
                            "operations" (vector operation)))

                    (revision (result)
                      "Return RESULT's model-visible resource revision alias."
                      (memory-resource-tests--field
                       (tool-result-content result) "Revision: ")))
             (let ((resolver
                      (cl-resources:resource-registry-find
                       (tool-registry-resource-registry registry) "memory")))
               (test-assert (typep resolver 'memory-resolver)
                            "default tools register the memory resource resolver"))
             (let* ((relevant (read-resource first-context "memory:relevant"))
                    (workspace (read-resource first-context "memory:workspace"))
                    (global (read-resource first-context "memory:global"))
                    (relevant-content (tool-result-content relevant))
                    (workspace-content (tool-result-content workspace))
                    (global-content (tool-result-content global)))
               (test-assert (and (tool-result-success-p relevant)
                                 (tool-result-success-p workspace)
                                 (tool-result-success-p global))
                            "every reserved memory collection is readable")
               (test-assert
                (and (search (memory-identifier workspace-memory) relevant-content)
                     (search (memory-identifier global-memory) relevant-content)
                     (not (search (memory-identifier other-memory) relevant-content)))
                "memory:relevant contains global and current-workspace memories only")
               (test-assert
                (and (search (memory-identifier workspace-memory) workspace-content)
                     (not (search (memory-identifier global-memory) workspace-content))
                     (not (search (memory-identifier other-memory) workspace-content)))
                "memory:workspace is confined to the current workspace")
               (test-assert
                (and (search (memory-identifier global-memory) global-content)
                     (not (search (memory-identifier workspace-memory) global-content))
                     (not (search (memory-identifier other-memory) global-content)))
                "memory:global contains global memories only")
               (test-assert
                (and (search (format nil "uri: ~A"
                                     (resource-item-uri
                     "memory"
                                      (memory-identifier workspace-memory)))
                             relevant-content)
                     (search "created:" relevant-content)
                     (search "updated:" relevant-content)
                     (search "tags:" relevant-content)
                     (search "excerpt:" relevant-content))
                "collection entries include canonical item URIs, metadata, and excerpts"))
             (let* ((other-workspace
                      (read-resource other-workspace-context "memory:workspace"))
                    (content (tool-result-content other-workspace)))
               (test-assert
                (and (search (memory-identifier other-memory) content)
                     (not (search (memory-identifier workspace-memory) content)))
                "memory:workspace follows the calling configuration"))
             (let* ((uri (format nil "memory:~A"
                                 (memory-identifier other-memory)))
                    (exact (read-resource first-context uri))
                    (content (tool-result-content exact))
                    (alias (revision exact))
                    (observation
                      (memory-resource-tests--observation first-conversation alias)))
               (test-assert (tool-result-success-p exact)
                            "exact memory reads remain scope-independent")
               (test-assert
                (and (search (memory-title other-memory) content)
                     (search (memory-content other-memory) content)
                     (search (memory-workspace other-memory) content)
                     (search "source conversation:" content))
                "exact memory reads return full content and metadata")
               (test-assert (and (typep observation 'memory-observation)
                                 (eq (memory-observation-kind observation) ':item)
                                 (string=
                                  (resource-observation-uri observation)
                                  (resource-item-uri
                     "memory"
                                   (memory-identifier other-memory)))
                                 (search
                                  (format nil "URI: ~A"
                                          (resource-item-uri
                     "memory"
                                           (memory-identifier other-memory)))
                                  content))
                            "compatible direct item reads render the canonical exact URI")
               (let ((edit
                       (edit-resource
                        first-context uri alias
                        (json-object "op" "replace-lines"
                                     "start-line" 1
                                     "end-line" 1
                                     "content" "mutated"))))
                 (test-assert
                  (and (not (tool-result-success-p edit))
                       (string= (memory-content
                                 (memory-find configuration
                                              (memory-identifier other-memory)))
                                (memory-content other-memory)))
                  "exact memory resources reject non-memory operations")))
             (dolist (memory reserved-memories)
               (let* ((identifier (memory-identifier memory))
                      (uri (resource-item-uri "memory" identifier))
                      (exact (read-resource first-context uri))
                      (collection (read-resource
                                   first-context
                                   (format nil "memory:~A" identifier))))
                 (test-assert
                  (and (tool-result-success-p exact)
                       (search (memory-title memory)
                               (tool-result-content exact))
                       (search (format nil "URI: ~A" uri)
                               (tool-result-content exact))
                       (tool-result-success-p collection)
                       (search (format nil "collection: ~A" identifier)
                               (tool-result-content collection)))
                  "canonical item URIs prevent reserved collection names from shadowing exact memories")))
             (let* ((identifier (memory-identifier encoded-memory))
                    (uri (resource-item-uri "memory" identifier))
                    (exact (read-resource first-context uri)))
               (test-assert
                (and (tool-result-success-p exact)
                     (string= (memory-resource-tests--uri-identifier uri)
                              identifier)
                     (search "%2F" uri)
                     (search "%20" uri)
                     (search "%25" uri)
                     (search "%C5%BE" uri)
                     (search (memory-title encoded-memory)
                             (tool-result-content exact)))
                "canonical exact URIs round-trip percent-encoded persisted identifiers"))
             (dolist (uri '("memory:id/"
                            "memory:id/%GG"
                            "memory:id/%FF"
                            "memory:id/raw/slash"))
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context uri)))
                "malformed canonical exact memory URIs fail closed"))
             (let* ((revision-memory
                      (memory-remember
                       configuration
                       :title "Revision fixture"
                       :content "First exact snapshot."
                       :tags '("revision")))
                    (uri (format nil "memory:~A"
                                 (memory-identifier revision-memory)))
                    (before-read (read-resource first-context uri))
                    (before-alias (revision before-read))
                    (before-observation
                      (memory-resource-tests--observation
                       first-conversation before-alias)))
               (memory-remember
                configuration
                :identifier (memory-identifier revision-memory)
                :title "Revision fixture"
                :content "Second exact snapshot."
                :tags '("revision"))
               (let* ((after-read (read-resource first-context uri))
                      (after-alias (revision after-read))
                      (after-observation
                        (memory-resource-tests--observation
                         first-conversation after-alias)))
                 (test-assert
                  (and (not (string= before-alias after-alias))
                       (not (string=
                             (resource-observation-revision before-observation)
                             (resource-observation-revision after-observation))))
                  "exact memory content changes produce a new snapshot identity")))
             (let* ((empty (read-resource empty-context "memory:relevant"))
                    (empty-again (read-resource empty-context "memory:relevant"))
                    (alias (revision empty))
                    (again-alias (revision empty-again))
                    (observation
                      (memory-resource-tests--observation empty-conversation alias)))
               (test-assert (and (tool-result-success-p empty)
                                 (search "count: 0" (tool-result-content empty))
                                 (search "No matching memories."
                                         (tool-result-content empty)))
                            "empty memory collections return a complete readable snapshot")
               (test-assert (string= alias again-alias)
                            "an unchanged empty collection reuses its exact snapshot alias")
               (test-assert
                (equal (memory-observation-snapshot observation)
                       (list :kind ':collection
                             :identifier "relevant"
                             :workspace
                             (namestring
                              (platform-truename *platform*
                               (config :working-directory
                                empty-configuration)))
                             :records nil))
                "empty relevant revision identity includes workspace and exact empty state"))
             (let* ((first-read (read-resource first-context "memory:global"))
                    (second-read (read-resource second-context "memory:global"))
                    (first-alias (revision first-read))
                    (second-alias (revision second-read))
                    (first-observation
                      (memory-resource-tests--observation
                       first-conversation first-alias))
                    (second-observation
                      (memory-resource-tests--observation
                       second-conversation second-alias)))
               (test-assert (not (string= first-alias second-alias))
                            "resource revision aliases are isolated by conversation")
               (test-assert
                (string= (resource-observation-revision first-observation)
                         (resource-observation-revision second-observation))
                "equal collection snapshots have equal content-addressed identity")
                (test-assert
                 (null (fifo-cache-get
                        (conversation-resource-observations second-conversation)
                        first-alias))
                 "one conversation cannot resolve another conversation's alias"))
             (let ((line-window
                     (read-resource first-context "memory:relevant"
                                    "start-line" 1)))
               (test-assert
                (and (not (tool-result-success-p line-window))
                     (eq (tool-result-error-code line-window)
                         ':line-windows-unsupported))
                "memory resources reject workspace line-window arguments"))
             (let* ((fresh (read-resource first-context "memory:relevant"))
                    (read-only-edit
                      (edit-resource first-context "memory:relevant"
                                     (revision fresh)
                                     (json-object
                                      "op" "memory-remember"
                                      "title" "Denied"
                                      "content" "memory:relevant refuses edits."))))
               (test-assert
                (and (not (tool-result-success-p read-only-edit))
                     (eq (tool-result-error-code read-only-edit)
                         ':operation-unsupported))
                "memory:relevant refuses mutation operations"))
             (let* ((forgotten
                      (memory-remember
                       configuration
                       :title "Temporary memory"
                       :content "Forget this exact test fixture."
                       :tags nil))
                    (forgotten-uri
                      (format nil "memory:~A" (memory-identifier forgotten))))
               (memory-forget configuration (memory-identifier forgotten))
               (let ((forgotten-read
                       (read-resource first-context forgotten-uri))
                     (missing-read
                       (read-resource first-context "memory:missing-stable-id")))
                 (test-assert
                  (and (not (tool-result-success-p forgotten-read))
                       (search "does not exist" (tool-result-content forgotten-read)))
                  "forgotten exact memories no longer resolve")
                 (test-assert
                  (and (not (tool-result-success-p missing-read))
                       (search "does not exist" (tool-result-content missing-read)))
                  "missing exact memory identifiers fail clearly")))
             (let ((collection-denied
                     (read-resource child-context "memory:relevant"))
                   (item-denied
                     (read-resource
                      child-context
                      (format nil "memory:~A" (memory-identifier global-memory)))))
               (test-assert
                (and (not (tool-result-success-p collection-denied))
                     (eq (tool-result-error-code collection-denied)
                         ':access-denied))
                "memory collections remain denied to task child agents")
               (test-assert
                (and (not (tool-result-success-p item-denied))
                     (eq (tool-result-error-code item-denied) ':access-denied))
                "exact memory resources remain denied to task child agents"))
             (let ((sample (merge-pathnames "sample.lisp" first-workspace)))
               (with-open-file (stream sample
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create)
                 (write-line "(in-package #:autolith)" stream))
                (read-resource first-context "workspace:sample.lisp")
                (read-resource first-context "agenda:current"))
             (let* ((expiry-conversation
                      (conversation-create configuration
                                           :identifier "memory-resource-expiry"))
                    (expiry-context
                      (make-instance 'tool-context
                                     :configuration configuration
                                     :worker nil
                                     :conversation expiry-conversation))
                    (workspace-read
                      (read-resource expiry-context "workspace:sample.lisp"))
                    (agenda-read
                      (read-resource expiry-context "agenda:current"))
                    (old-read (read-resource expiry-context "memory:global"))
                    (workspace-alias (revision workspace-read))
                    (agenda-alias (revision agenda-read))
                    (old-alias (revision old-read))
                    (states
                      (conversation-resource-observations expiry-conversation)))
               (let ((*memory-resource-maximum-observations* 1))
                 (memory-remember
                  configuration
                  :title "New global snapshot"
                  :content "Change collection identity for retention testing."
                  :scope ':global
                  :tags '("retention"))
                 (read-resource expiry-context "memory:global")
                 (test-assert
                  (null (resource-observation-state-find
                         states old-alias 'memory-observation-state))
                  "memory observation retention expires the oldest memory alias")
                 (test-assert
                  (and (typep (fifo-cache-get states workspace-alias)
                              'workspace-file-observation-state)
                       (typep (fifo-cache-get states agenda-alias)
                              'agenda-observation-state))
                  "memory observation expiry preserves workspace and agenda states")))
              (let* ((all-read (read-resource first-context "memory:all"))
                     (search-read
                       (read-resource first-context "memory:all"
                                      "query" "response preference"))
                     (limited-read
                       (read-resource first-context "memory:all"
                                      "max-results" 1))
                     (exact-read
                       (read-resource
                        first-context
                        (resource-item-uri
                     "memory"
                         (memory-identifier global-memory)))))
                (test-assert
                 (and (tool-result-success-p all-read)
                      (search (memory-identifier workspace-memory)
                              (tool-result-content all-read))
                      (search (memory-identifier other-memory)
                              (tool-result-content all-read)))
                 "memory:all crosses workspace scopes")
                (test-assert
                 (and (tool-result-success-p search-read)
                      (search (memory-identifier global-memory)
                              (tool-result-content search-read))
                      (not (search (memory-identifier workspace-memory)
                                   (tool-result-content search-read))))
                 "memory collection query preserves weighted lexical search")
                (test-assert
                 (and (tool-result-success-p limited-read)
                      (search "count: 1" (tool-result-content limited-read)))
                 "memory collection max-results bounds returned entries")
                (test-assert
                 (and (tool-result-success-p exact-read)
                      (search (memory-resource--render-item
                               (memory-assess (memory-assessor-create configuration)
                                              global-memory)
                               nil)
                              (tool-result-content exact-read)))
                 "canonical memory item resources return complete content"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)
      (platform-delete-directory-tree *platform* empty-root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-memory-resource-mutations () null)
(defun test-memory-resource-mutations ()
  "Test guarded memory resource mutation, validation, staleness, and locking."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (workspace (merge-pathnames "memory-mutations/" root))
         (switched-workspace (merge-pathnames "memory-mutations-switched/" root))
         (configuration nil)
         (switched-configuration nil)
         (registry (make-default-tool-registry)))
    (ensure-directories-exist (merge-pathnames "marker" workspace))
    (ensure-directories-exist (merge-pathnames "marker" switched-workspace))
    (setf configuration
          (configuration-copy base-configuration
                                :working-directory workspace)
          switched-configuration
          (configuration-copy base-configuration
                                :working-directory switched-workspace))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-mutations"))
                (other-conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-other"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation))
                (other-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation other-conversation))
                (switched-context
                  (make-instance 'tool-context
                                 :configuration switched-configuration
                                 :worker nil
                                 :conversation conversation))
                (child-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :agent (allocate-instance
                                         (find-class 'task-child-agent)))))
           (labels ((read-resource (selected-context uri)
                      "Read one complete memory resource."
                      (memory-resource-tests--call
                       registry selected-context "resource" "read" "uri" uri))

                    (revision (result)
                      "Return RESULT's transient revision alias."
                      (memory-resource-tests--field
                       (tool-result-content result) "Revision: "))

                    (exact-uri (result)
                      "Return RESULT's exact created or replaced memory URI."
                      (memory-resource-tests--field
                       (tool-result-content result) "Exact URI: "))

                    (edit-resource (selected-context uri alias operation)
                      "Apply one guarded memory resource OPERATION."
                      (memory-resource-tests--edit
                       registry selected-context uri alias operation))

                    (form-count ()
                      "Return the number of complete forms in the memory log."
                      (length
                       (first
                        (multiple-value-list
                         (sexp-store:log-read
                          (configuration-memory-path configuration)))))))
             (let* ((workspace-before
                      (read-resource context "memory:workspace"))
                    (relevant-before
                      (read-resource context "memory:relevant"))
                    (global-before
                      (read-resource context "memory:global"))
                    (workspace-after
                      (read-resource switched-context "memory:workspace"))
                    (relevant-after
                      (read-resource switched-context "memory:relevant"))
                    (global-after
                      (read-resource switched-context "memory:global"))
                    (before (form-count))
                    (stale-creation
                      (edit-resource
                       switched-context
                       "memory:workspace"
                       (revision workspace-before)
                       (json-object
                        "op" "memory-remember"
                        "title" "Rejected cross-workspace creation"
                        "content" "An alias from another workspace must be stale."))))
               (test-assert
                (and (not (string= (revision workspace-before)
                                   (revision workspace-after)))
                     (not (string= (revision relevant-before)
                                   (revision relevant-after)))
                     (string= (revision global-before)
                              (revision global-after)))
                "same-conversation collection aliases bind workspace and relevant but not global identity")
               (test-assert
                (and (not (tool-result-success-p stale-creation))
                     (search "stale" (tool-result-content stale-creation))
                     (= before (form-count)))
                "a workspace collection alias cannot create after a configuration switch"))
             (let* ((fixture
                      (memory-resource-tests--persist-identifier
                       configuration
                       "relevant"
                       "Reserved mutation fixture"
                       "Canonical exact item operations must reach this memory."))
                    (uri (resource-item-uri
                     "memory"
                          (memory-identifier fixture)))
                    (observed (read-resource context uri))
                    (replaced
                      (edit-resource
                       context uri (revision observed)
                       (json-object
                        "op" "memory-replace"
                        "title" "Reserved mutation replacement"
                        "content" "The reserved identifier remained exact.")))
                    (forgotten
                      (and (tool-result-success-p replaced)
                           (edit-resource
                            context uri (revision replaced)
                            (json-object "op" "memory-forget")))))
               (test-assert
                (and (tool-result-success-p replaced)
                     forgotten
                     (tool-result-success-p forgotten)
                     (null (memory-find configuration "relevant")))
                "canonical exact item URIs replace and forget reserved identifiers"))
             (let* ((workspace-read
                      (read-resource context "memory:workspace"))
                    (created
                      (edit-resource
                       context "memory:workspace" (revision workspace-read)
                       (json-object
                        "op" "memory-remember"
                        "title" "Resource workspace memory"
                        "content" "Created through the guarded resource protocol."
                        "tags" (json-array "resource" "workspace"))))
                    (uri (and (tool-result-success-p created)
                              (exact-uri created)))
                    (identifier
                      (and uri (memory-resource-tests--uri-identifier uri)))
                    (memory (and identifier
                                 (memory-find configuration identifier))))
               (test-assert
                (and (tool-result-success-p created)
                     uri
                     memory
                     (uiop:string-prefix-p "memory:id/" uri)
                     (string= (memory-resource-tests--uri-identifier uri)
                              (memory-identifier memory))
                     (eq (memory-scope memory) ':workspace)
                     (string= (memory-workspace memory)
                              (namestring workspace))
                     (equal (memory-tags memory) '("resource" "workspace"))
                     (string= (memory-source-conversation memory)
                              "memory-resource-mutations"))
                "memory:workspace creates an attributed workspace memory")
               (let* ((replaced
                        (edit-resource
                         context uri (revision created)
                         (json-object
                          "op" "memory-replace"
                          "title" "Resource replacement"
                          "content" "The stable memory moved to global scope."
                          "tags" (json-array "replacement")
                          "scope" "global")))
                      (replacement (memory-find configuration identifier)))
                 (test-assert
                  (and (tool-result-success-p replaced)
                       replacement
                       (string= (memory-identifier replacement) identifier)
                       (eq (memory-scope replacement) ':global)
                       (null (memory-workspace replacement))
                       (string= (memory-source-conversation replacement)
                                "memory-resource-mutations"))
                  "memory-replace preserves stable identity and scope behavior")
                 (let* ((forgotten
                          (edit-resource
                           context uri (revision replaced)
                           (json-object "op" "memory-forget")))
                        (forms
                          (nth-value
                           0
                           (sexp-store:log-read
                            (configuration-memory-path configuration))))
                        (last-record (first (last forms))))
                   (test-assert
                    (and (tool-result-success-p forgotten)
                         (search "was forgotten" (tool-result-content forgotten))
                         (null (memory-find configuration identifier))
                         (eq (first last-record) ':memory-forgotten)
                         (string= (getf (rest last-record) :source-conversation)
                                  "memory-resource-mutations"))
                    "memory-forget appends a tombstone and renders forgotten state"))))
             (let* ((global-read (read-resource context "memory:global"))
                    (created
                      (edit-resource
                       context "memory:global" (revision global-read)
                       (json-object
                        "op" "memory-remember"
                        "title" "Resource global memory"
                        "content" "This guidance applies across workspaces.")))
                    (uri (and (tool-result-success-p created)
                              (exact-uri created)))
                    (memory
                      (and uri
                           (memory-find
                            configuration
                            (memory-resource-tests--uri-identifier uri)))))
               (test-assert
                (and (tool-result-success-p created)
                     memory
                     (eq (memory-scope memory) ':global)
                     (null (memory-workspace memory)))
                "memory:global creates a global memory from collection scope"))
             (let* ((fixture
                      (memory-remember
                       configuration
                       :title "Stale memory fixture"
                       :content "Snapshot A."
                       :tags '("stale")
                       :source-conversation "legacy-a"))
                    (identifier (memory-identifier fixture))
                    (uri (format nil "memory:~A" identifier))
                    (observed (read-resource context uri))
                    (alias (revision observed)))
               (memory-remember
                configuration
                :identifier identifier
                :title "Stale memory fixture"
                :content "Snapshot B."
                :tags '("stale")
                :source-conversation "legacy-b")
               (let ((before (form-count))
                     (stale
                       (edit-resource
                        context uri alias
                        (json-object
                         "op" "memory-replace"
                         "title" "Rejected stale replacement"
                         "content" "This must not append."))))
                 (test-assert
                  (and (not (tool-result-success-p stale))
                       (search "stale" (tool-result-content stale))
                       (= before (form-count)))
                  "stale memory resource edits append nothing"))
               (with-recursive-lock-held (*memory-lock*)
                 (memory--transact
                  configuration
                  (lambda (active)
                    (declare (ignore active))
                    (values (list (memory--record fixture)) nil t))))
               (test-assert
                (tool-result-success-p
                 (edit-resource
                  context uri alias
                  (json-object
                   "op" "memory-replace"
                   "title" "Accepted ABA replacement"
                   "content" "The exact complete snapshot returned.")))
                "an exact ABA snapshot revives its content-addressed observation"))
             (let* ((workspace-read
                      (read-resource context "memory:workspace"))
                    (global-read (read-resource context "memory:global"))
                    (item (first (memory-list configuration :visibility ':all)))
                    (item-uri (format nil "memory:~A"
                                      (memory-identifier item)))
                    (item-read (read-resource context item-uri))
                    (before (form-count))
                    (results
                      (list
                       (edit-resource
                        context "memory:workspace" (revision global-read)
                        (json-object
                         "op" "memory-remember"
                         "title" "Mismatched alias"
                         "content" "This must not append."))
                       (edit-resource
                        context "memory:relevant"
                        (revision (read-resource context "memory:relevant"))
                        (json-object
                         "op" "memory-remember"
                         "title" "Invalid relevant creation"
                         "content" "This must not append."))
                       (edit-resource
                        context "memory:workspace" (revision workspace-read)
                        (json-object "op" "memory-forget"))
                       (edit-resource
                        context item-uri (revision item-read)
                        (json-object
                         "op" "memory-remember"
                         "title" "Invalid item creation"
                         "content" "This must not append."))
                       (edit-resource
                        context "memory:workspace" (revision workspace-read)
                        (json-object
                         "op" "memory-remember"
                         "title" "Missing complete content")))))
               (test-assert
                (and (every (lambda (result)
                              (not (tool-result-success-p result)))
                            results)
                     (= before (form-count)))
                "invalid memory URI-operation combinations and content append nothing"))
             (let* ((expiring
                      (memory-remember
                       configuration
                       :title "Expiring item"
                       :content "Its resource alias will expire."
                       :tags nil))
                    (uri (format nil "memory:~A"
                                 (memory-identifier expiring)))
                    (observed (read-resource other-context uri))
                    (alias (revision observed)))
               (let ((*memory-resource-maximum-observations* 1))
                 (let ((other
                         (memory-remember
                          configuration
                          :title "Replacement observation"
                          :content "Force another memory observation."
                          :tags nil)))
                   (read-resource
                    other-context
                    (format nil "memory:~A" (memory-identifier other))))
                 (test-assert
                  (not
                   (tool-result-success-p
                    (edit-resource
                     other-context uri alias
                     (json-object
                      "op" "memory-replace"
                      "title" "Expired edit"
                      "content" "This must not append."))))
                  "expired memory resource aliases cannot mutate")))
             (let* ((collection (read-resource context "memory:workspace"))
                    (alias (revision collection))
                    (gate (make-lock "Memory resource compatibility gate"))
                    (condition (make-condition-variable))
                    (started-p nil)
                    (legacy-memory nil)
                    (resource-result nil)
                    (thread nil))
               (with-recursive-lock-held (*memory-lock*)
                 (setf thread
                       (make-thread
                        (lambda ()
                          (with-lock-held (gate)
                            (setf started-p t)
                            (condition-notify condition))
                          (setf legacy-memory
                                (memory-remember
                                 configuration
                                 :title "Concurrent legacy memory"
                                 :content "Legacy mutation waits for the guarded edit."
                                 :tags nil)))
                        :name "memory legacy compatibility"))
                 (with-lock-held (gate)
                   (loop until started-p
                         do (condition-wait condition gate)))
                 (setf resource-result
                       (edit-resource
                        context "memory:workspace" alias
                        (json-object
                         "op" "memory-remember"
                         "title" "Guarded concurrent memory"
                         "content" "The stale check and append are one local critical section."))))
               (join-thread thread)
               (test-assert
                (and (tool-result-success-p resource-result)
                     legacy-memory
                     (memory-find configuration
                                  (memory-identifier legacy-memory)))
                "legacy memory mutation waits behind the recursive resource transaction"))
             (let* ((collection (read-resource context "memory:workspace"))
                    (denied
                      (edit-resource
                       child-context "memory:workspace" (revision collection)
                       (json-object
                        "op" "memory-remember"
                        "title" "Denied child memory"
                        "content" "Task children cannot mutate memory resources."))))
               (test-assert
                (and (not (tool-result-success-p denied))
                     (eq (tool-result-error-code denied) ':access-denied))
                "task child agents remain denied memory resource mutation"))
             (let* ((edit-tool (tool-registry-find registry "resource" "edit"))
                    (operations
                      (json-get
                       (json-get
                        (json-get (tool-parameters edit-tool) "properties")
                        "operations")
                       "items"))
                    (variants (json-get operations "oneOf"))
                    (memory-variants
                      (remove-if-not
                       (lambda (variant)
                         (let* ((properties (json-get variant "properties"))
                                (op (json-get properties "op"))
                                (values (and op (json-get op "enum"))))
                           (and values
                                (search "memory-" (aref values 0)))))
                       (coerce variants 'list))))
               (test-assert
                (and (= (length memory-variants) 3)
                     (every (lambda (variant)
                              (json-false-p (gethash "additionalProperties" variant)))
                            memory-variants))
                "resource.edit advertises three closed memory operation variants")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil))


(-> test-memory-resource-filtered-read-write () null)
(defun test-memory-resource-filtered-read-write ()
  "Test that a filtered collection read provides a usable write revision."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (workspace (merge-pathnames "memory-filtered-read-write/" root))
         (configuration
           (progn
             (ensure-directories-exist (merge-pathnames "marker" workspace))
             (configuration-copy base-configuration :working-directory workspace)))
         (conversation
           (conversation-create configuration :identifier "memory-filtered-read-write"))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation))
         (registry (make-default-tool-registry)))
    (unwind-protect
         (let* ((matching-memory
                  (memory-remember
                   configuration
                   :title "Matching filtered revision probe"
                   :content "needle"))
                (unmatched-memory
                  (memory-remember
                   configuration
                   :title "Unmatched filtered revision probe"
                   :content "This memory must still affect the collection revision."))
                (read-result
                  (memory-resource-tests--call
                   registry context "resource" "read"
                   "uri" "memory:workspace"
                   "query" "needle"
                   "max-results" 10))
                (revision
                  (memory-resource-tests--field
                   (tool-result-content read-result) "Revision: "))
                (unfiltered-result
                  (memory-resource-tests--call
                   registry context "resource" "read"
                   "uri" "memory:workspace"))
                (filtered-again-result
                  (memory-resource-tests--call
                   registry context "resource" "read"
                   "uri" "memory:workspace"
                   "query" "needle"
                   "max-results" 10))
                (write-result
                  (memory-resource-tests--edit
                   registry context "memory:workspace" revision
                   (json-object
                    "op" "memory-remember"
                    "title" "Filtered revision probe"
                    "content" "A filtered collection observation permits a guarded write."))))
           (test-assert
            (and (tool-result-success-p read-result)
                 (search (memory-identifier matching-memory)
                         (tool-result-content read-result))
                 (not (search (memory-identifier unmatched-memory)
                              (tool-result-content read-result))))
            "filtered memory collection reads expose only matching memories")
           (test-assert
            (and (tool-result-success-p unfiltered-result)
                 (search (memory-identifier unmatched-memory)
                         (tool-result-content unfiltered-result)))
            "unfiltered memory collection reads expose hidden memories")
           (test-assert
            (and (tool-result-success-p filtered-again-result)
                 (search (memory-identifier matching-memory)
                         (tool-result-content filtered-again-result))
                 (not (search (memory-identifier unmatched-memory)
                              (tool-result-content filtered-again-result))))
            "repeated filtered reads retain their requested presentation")
           (test-assert (tool-result-success-p write-result)
                        "a filtered memory collection revision permits memory-remember"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-memory-resource-sources () null)
(defun test-memory-resource-sources ()
  "Test cited evidence and premises through memory: resource edits and reads."
  (with-test-configuration (base root)
    (let* ((workspace (merge-pathnames "sourced/" root))
           (configuration (progn
                            (ensure-directories-exist workspace)
                            (configuration-copy base :working-directory workspace)))
           (registry (make-default-tool-registry))
           (conversation (conversation-create configuration
                                              :identifier "memory-sources"))
           (context (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
           (notes (memory-source-tests--write
                   (merge-pathnames "NOTES.md" workspace)
                   (format nil "Deploy with make release-prod after tagging.~%"))))
      (conversation-append-user-message
       conversation "Please always use tabs for indentation here.")
      (labels ((read-resource (uri)
                 "Read one complete memory resource."
                 (memory-resource-tests--call
                  registry context "resource" "read" "uri" uri))

               (revision (result)
                 "Return RESULT's transient revision alias."
                 (memory-resource-tests--field
                  (tool-result-content result) "Revision: "))

               (edit (uri operation)
                 "Apply OPERATION to a fresh observation of URI."
                 (memory-resource-tests--edit
                  registry context uri (revision (read-resource uri)) operation))

               (remember (title content &rest fields)
                 "Create a workspace memory with optional evidence FIELDS."
                 (edit "memory:workspace"
                       (apply #'json-object
                              "op" "memory-remember"
                              "title" title
                              "content" content
                              fields)))

               (created-uri (result)
                 "Return the exact URI of a created memory."
                 (memory-resource-tests--field
                  (tool-result-content result) "Exact URI: "))

               (failure-p (result text)
                 "Return true when RESULT failed and mentions TEXT."
                 (and (not (tool-result-success-p result))
                      (search text (tool-result-content result))
                      t)))
        (let* ((deploy (remember "Deploy command"
                                 "Deploy with make release-prod."
                                 "sources"
                                 (vector (json-object "file" "NOTES.md"
                                                      "quote" "with make release-prod")
                                         (json-object "user" "always use tabs"))))
               (deploy-uri (created-uri deploy))
               (deploy-identifier (memory-resource-tests--uri-identifier deploy-uri))
               (derived (remember "Release day"
                                  "Releases need a tag and make release-prod."
                                  "premises" (vector deploy-uri)))
               (derived-uri (created-uri derived)))
          (test-assert (and (tool-result-success-p deploy)
                            (= 2 (length (memory-sources
                                          (memory-find configuration deploy-identifier)))))
                       "memory-remember records a verified file quote and user statement")
          (test-assert (search "sources:" (tool-result-content (read-resource deploy-uri)))
                       "an exact memory read lists its sources")
          (test-assert (and (tool-result-success-p derived)
                            (search "premises:" (tool-result-content
                                                 (read-resource derived-uri))))
                       "a premise named by its memory URI is recorded and shown")
          (test-assert (search "relied on by:" (tool-result-content
                                                (read-resource deploy-uri)))
                       "an exact memory read names the memories derived from it")
          (dolist (case
                   (list
                    (list "a quote absent from the cited file"
                          (remember "Wrong" "Wrong."
                                    "sources" (vector (json-object
                                                       "file" "NOTES.md"
                                                       "quote" "make release-staging")))
                          "does not occur")
                    (list "words the user never said"
                          (remember "Invented" "Invented."
                                    "sources" (vector (json-object
                                                       "user" "always use spaces")))
                          "user's messages")
                    (list "a source naming two kinds"
                          (remember "Ambiguous" "Ambiguous."
                                    "sources" (vector (json-object
                                                       "file" "NOTES.md"
                                                       "url" "https://example.com")))
                          "exactly one of")
                    (list "an unknown premise"
                          (remember "Orphan" "Orphan."
                                    "premises" (vector "no-such-memory"))
                          "does not exist")))
            (destructuring-bind (label result text) case
              (test-assert (failure-p result text)
                           (format nil "~A is rejected correctively" label))))
          (memory-source-tests--write notes (format nil "Deploy with make ship.~%"))
          (test-assert (search "status: stale (NOTES.md changed)"
                               (tool-result-content (read-resource deploy-uri)))
                       "rereading a memory whose cited text changed reports it stale")
          (test-assert (search "status: stale"
                               (tool-result-content (read-resource "memory:workspace")))
                       "collection entries report stale memories")
          (test-assert (search "(stale)" (tool-result-content (read-resource derived-uri)))
                       "a derived memory shows which premise is stale")
          (let ((restated (edit deploy-uri
                                (json-object "op" "memory-replace"
                                             "title" "Deploy command"
                                             "content" "Deploy with make release-prod."
                                             "tags" (vector "deploy")))))
            (test-assert (and (tool-result-success-p restated)
                              (search "status: stale (NOTES.md changed)"
                                      (tool-result-content restated)))
                         "replacing without sources keeps a stale memory stale"))
          (let ((replaced (edit deploy-uri
                                (json-object "op" "memory-replace"
                                             "title" "Deploy command"
                                             "content" "Deploy with make ship."
                                             "sources" (vector (json-object
                                                                "file" "NOTES.md"
                                                                "quote" "with make ship"))))))
            (test-assert (and (tool-result-success-p replaced)
                              (not (search "status:" (tool-result-content replaced))))
                         "replacing with current evidence makes the memory current"))
          (test-assert (search "changed since this memory was written"
                               (tool-result-content (read-resource derived-uri)))
                       "a derived memory notices that its premise was restated")
          (let ((forgotten (edit deploy-uri (json-object "op" "memory-forget"))))
            (test-assert (and (tool-result-success-p forgotten)
                              (search derived-uri (tool-result-content forgotten)))
                         "forgetting a premise names the memories that relied on it"))))))
  nil)

(-> test-memory-knowledge-resources () null)
(defun test-memory-knowledge-resources ()
  "Test defining, asserting, deriving, explaining, and retracting through memory:facts."
  (with-test-configuration (base root)
    (let* ((workspace (merge-pathnames "facts/" root))
           (configuration (progn
                            (ensure-directories-exist workspace)
                            (configuration-copy base :working-directory workspace)))
           (registry (make-default-tool-registry))
           (conversation (conversation-create configuration :identifier "memory-facts"))
           (context (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation))
           (schedule (memory-source-tests--write
                      (merge-pathnames "schedule.txt" workspace)
                      (format nil "Design review at 13:00 UTC on 22 October.~%"))))
      (conversation-append-user-message conversation "It takes half an hour to get there.")
      (labels ((read-resource (uri &rest arguments)
                 "Read URI with optional ARGUMENTS and return the result."
                 (apply #'memory-resource-tests--call registry context "resource" "read"
                        "uri" uri arguments))

               (content (result)
                 "Return RESULT's text."
                 (tool-result-content result))

               (edit (&rest operations)
                 "Apply OPERATIONS to memory:facts against a fresh revision."
                 (memory-resource-tests--call
                  registry context "resource" "edit"
                  "uri" "memory:facts"
                  "base-revision" (memory-resource-tests--field
                                   (content (read-resource "memory:facts")) "Revision: ")
                  "operations" (coerce operations 'vector)))

               (define (predicate arguments &optional key template)
                 "Return a predicate-define operation."
                 (apply #'json-object "op" "predicate-define" "predicate" predicate
                        "arguments"
                        (map 'vector (lambda (argument)
                                       (json-object "name" (first argument)
                                                    "type" (second argument)))
                             arguments)
                        (append (and key (list "key" (coerce key 'vector)))
                                (and template (list "template" template))))))
        (test-assert (search "No facts hold" (content (read-resource "memory:facts")))
                     "an empty knowledge base lists no facts")
        (let ((defined
                (edit (define "event.start" '(("event" "entity") ("time" "instant"))
                              '("event") "{event} starts at {time}")
                      (define "event.travel" '(("event" "entity") ("duration" "duration"))
                              '("event"))
                      (define "event.buffer" '(("event" "entity") ("duration" "duration"))
                              '("event"))
                      (define "event.departure" '(("event" "entity") ("time" "instant"))
                              '("event") "leave for {event} at {time}")
                      (json-object "op" "fact-assert" "predicate" "event.start"
                                   "arguments" (vector "review" "2026-10-22T13:00:00Z")
                                   "sources" (vector (json-object "file" "schedule.txt"
                                                                  "quote" "review at 13:00")))
                      (json-object "op" "fact-assert" "predicate" "event.travel"
                                   "arguments" (vector "review" "PT30M")
                                   "sources" (vector (json-object "user" "half an hour")))
                      (json-object "op" "fact-assert" "predicate" "event.buffer"
                                   "arguments" (vector "review" "PT10M"))
                      (json-object "op" "rule-propose" "id" "departure"
                                   "premises" (vector (vector "event.start" "?event" "?start")
                                                      (vector "event.travel" "?event" "?travel")
                                                      (vector "event.buffer" "?event" "?buffer"))
                                   "bindings" (vector (vector "?leave"
                                                              (vector "-" "?start"
                                                                      (vector "+" "?travel"
                                                                              "?buffer"))))
                                   "conclusion" (vector "event.departure" "?event" "?leave")))))
          (test-assert (and (tool-result-success-p defined)
                            (search "Defined event.start version 1" (content defined))
                            (search "now holds: review starts at 2026-10-22 13:00 UTC"
                                    (content defined))
                            (search "admission was declined, so it derives nothing"
                                    (content defined)))
                       "one edit defines predicates, asserts facts, and proposes a rule"))
        (knowledge-admit-rule configuration "departure")
        (let* ((facts (content (read-resource "memory:facts")))
               (line (find-if (lambda (line) (search "leave for review" line))
                              (uiop:split-string facts :separator '(#\Newline))))
               (departure (and line (json-get (json-decode line) "id"))))
          (test-assert (and line (search "\"derived\":\"departure v1\"" line)
                            (search "\"basis\":[\"observation+user-assertion+model-proposal\"]"
                                    line))
                       "the derived departure is listed with its rule and bases")
          (let ((why (content (read-resource (format nil "memory:claim/~A" departure)))))
            (test-assert (and (search "derived by rule departure v1 with ?leave = 2026-10-22 12:20 UTC"
                                      why)
                              (search "schedule.txt \"review at 13:00\" (holds)" why)
                              (search "you said \"half an hour\" (holds)" why))
                         "memory:claim explains the derivation down to its evidence")))
        (test-assert (search "event.start(event entity, time instant) v1 workspace key event"
                             (content (read-resource "memory:predicates")))
                     "memory:predicates lists the vocabulary")
        (test-assert (search "departure v1 workspace: active version 1"
                             (content (read-resource "memory:rules")))
                     "memory:rules lists the admitted rule")
        (test-assert (search "count: 4 (showing 1;"
                             (content (read-resource "memory:facts" "query" "review"
                                                     "max-results" 1)))
                     "a query and max-results narrow the listing")
        (test-assert (search "First look: 4 facts hold now"
                             (content (read-resource "memory:changes")))
                     "the first memory:changes read records what this conversation saw")
        (memory-source-tests--write schedule
                                    (format nil "Design review at 14:00 UTC on 22 October.~%"))
        (let ((moved (edit (json-object "op" "fact-assert" "predicate" "event.start"
                                        "arguments" (vector "review" "2026-10-22T14:00:00Z")
                                        "sources" (vector (json-object
                                                           "file" "schedule.txt"
                                                           "quote" "review at 14:00"))))))
          (test-assert (search "now holds: leave for review at 2026-10-22 13:20 UTC"
                               (content moved))
                       "asserting the new start reports the recomputed departure"))
        (let ((changes (content (read-resource "memory:changes"))))
          (test-assert (search "changed: leave for review at 2026-10-22 13:20 UTC, was leave for review at 2026-10-22 12:20 UTC"
                               changes)
                       "memory:changes reports the moved departure since the last read"))
        (test-assert (search "Nothing changed since" (content (read-resource "memory:changes")))
                     "each change is reported once")
        (dolist (case (list (list "an undefined predicate"
                                  (edit (json-object "op" "fact-assert" "predicate" "room.size"
                                                     "arguments" (vector "a" 1)))
                                  "Define predicate room.size")
                            (list "an unknown basis"
                                  (edit (json-object "op" "fact-assert" "predicate" "event.buffer"
                                                     "arguments" (vector "review" "PT5M")
                                                     "basis" "rumor"))
                                  "basis must be")
                            (list "a later failing operation"
                                  (edit (json-object "op" "fact-assert" "predicate" "event.buffer"
                                                     "arguments" (vector "standup" "PT5M"))
                                        (json-object "op" "fact-assert" "predicate" "event.buffer"
                                                     "arguments" (vector "standup" "soon")))
                                  "Operation 2 failed: Argument duration of event.buffer")))
          (destructuring-bind (label result text) case
            (test-assert (and (not (tool-result-success-p result))
                              (search text (content result)))
                         (format nil "~A fails correctively" label))))
        (flet ((asserted-bases (result)
                 "Return the bases of the supports of the claim RESULT asserted."
                 (let* ((text (content result))
                        (start (+ (search "Asserted " text) (length "Asserted ")))
                        (claim (subseq text start (position #\: text :start start)))
                        (base (knowledge-base-read configuration)))
                   (sort (remove-duplicates
                          (mapcar (lambda (support)
                                    (getf (knowledge-base-support base support) :basis))
                                  (getf (knowledge-base-claim base claim) :supports)))
                         #'string<))))
          (let ((unretrieved (edit (json-object "op" "fact-assert" "predicate" "event.buffer"
                                                "arguments" (vector "standup" "PT5M")
                                                "sources" (vector (json-object
                                                                   "url" "https://example.com/agenda"
                                                                   "quote" "five minutes early"))))))
            (test-assert (and (search "https://example.com/agenda was not retrieved with that text in this conversation"
                                      (content unretrieved))
                              (equal (asserted-bases unretrieved) '(:model-proposal)))
                         "a page this conversation never retrieved supports only the model's proposal"))
          (test-call-with-function-replacements
           (list (list 'web-gist--retrieve
                       (lambda (url)
                         (declare (ignore url))
                         "Agenda: be five minutes early to standup.")))
           (lambda ()
             (let* ((fetch-context (make-instance 'tool-context
                                                 :configuration configuration :worker nil
                                                 :conversation conversation :call-id "fetch-1"))
                    (result (memory-resource-tests--call
                             registry fetch-context "web_extra" "gist"
                             "url" "https://example.com/agenda")))
               (conversation-append-tool-result conversation "fetch-1"
                                                :tool-name "web_extra.gist"
                                                :output (tool-result-content result)
                                                :success-p (tool-result-success-p result)))))
          (let ((retrieved (edit (json-object "op" "fact-assert" "predicate" "event.buffer"
                                              "arguments" (vector "standup" "PT5M")
                                              "sources" (vector (json-object
                                                                 "url" "https://example.com/agenda"
                                                                 "quote" "five minutes early"))))))
            (test-assert (and (not (search "not retrieved" (content retrieved)))
                              (member :observation (asserted-bases retrieved)))
                         "a page retrieved in this conversation with the quoted text is an observation"))
          (let ((raised (edit (json-object "op" "fact-assert" "predicate" "event.buffer"
                                           "arguments" (vector "retro" "PT5M")
                                           "basis" "observation"
                                           "sources" (vector (json-object "user" "half an hour"))))))
            (test-assert (and (search "The user source supports it as user-assertion, not observation."
                                      (content raised))
                              (equal (asserted-bases raised) '(:user-assertion)))
                         "a requested basis never exceeds what its source establishes")))
        (test-assert (search "Operations 1 to 1 were applied"
                             (content (edit (json-object "op" "fact-assert"
                                                         "predicate" "event.buffer"
                                                         "arguments" (vector "lunch" "PT5M"))
                                            (json-object "op" "nonsense"))))
                     "a failure names the operations already applied")
        (let ((unobserved (memory-resource-tests--call
                           registry context "resource" "edit"
                           "uri" "memory:facts"
                           "base-revision" (memory-resource-tests--field
                                            (content (read-resource "memory:workspace"))
                                            "Revision: ")
                           "operations" (vector (json-object "op" "fact-retract"
                                                             "id" "c0")))))
          (test-assert (and (not (tool-result-success-p unobserved))
                            (search "Read memory:facts" (content unobserved)))
                       "an edit needs a memory:facts revision from this conversation")))))
  (memory-resource-tests--check-url-receipts)
  (memory-resource-tests--check-url-receipts :long-p t)
  (memory-resource-tests--check-url-receipts :long-p t :spill-p nil)
  nil)

(-> memory-resource-tests--check-url-receipts
    (&key (:long-p boolean) (:spill-p boolean)) null)
(defun memory-resource-tests--check-url-receipts (&key long-p (spill-p t))
  "Test completed retrievals, retained quotations, authenticity, and original observation times."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
           (registry (make-default-tool-registry :configuration configuration))
           (conversation (conversation-create configuration))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                                :conversation conversation))
           (url "https://example.com/ready")
           (text (if long-p
                     (let ((prefix "The report is ready for publication.")
                           (suffix "The appendix is verified."))
                       (concatenate 'string prefix
                                    (make-string (- 9000 (length prefix) (length suffix))
                                                 :initial-element #\Space)
                                    suffix))
                     "The report is ready for publication."))
           (now 100000)
           (*knowledge-clock* (lambda () now)))
      (knowledge-define-schema configuration
                               (knowledge-make-schema :predicate "site.ready"
                                                      :arguments '((:name "site" :type :entity))))
      (labels ((assert-fact (&optional quote (page "page"))
                 "Assert from URL through a fresh resource revision and return its evaluated node."
                 (let* ((read (memory-resource-tests--call registry context "resource" "read"
                                                          "uri" "memory:facts"))
                        (source (if quote (json-object "url" url "quote" quote)
                                    (json-object "url" url)))
                        (result (memory-resource-tests--call
                                 registry context "resource" "edit" "uri" "memory:facts"
                                 "base-revision" (memory-resource-tests--field
                                                  (tool-result-content read) "Revision: ")
                                 "operations" (vector (json-object
                                                       "op" "fact-assert" "predicate" "site.ready"
                                                       "arguments" (vector page)
                                                       "sources" (vector source))))))
                   (test-assert (tool-result-success-p result) "the sourced assertion is recorded")
                   (knowledge-view-node
                    (knowledge-view-current configuration)
                     (knowledge-claim-identifier (list "site.ready" page) :scope ':workspace
                                                 :workspace (knowledge--workspace configuration)))))

               (retrieve (call-id &key fail-p corrupt-p)
                 "Fetch a scripted page and persist its actual result, optionally failing or corrupting it."
                 (test-call-with-function-replacements
                  (append
                   (unless spill-p
                     (list (list 'tool--spill-result-text
                                 (lambda (context name output)
                                   (declare (ignore context name output))
                                   nil))))
                   (list (list 'web-gist--retrieve
                               (lambda (requested)
                                 (test-assert (equal requested url) "the retriever receives the exact URL")
                                 (when fail-p
                                   (error 'tool-error :tool-name "web_extra.gist" :message "Unavailable."))
                                 text))))
                  (lambda ()
                    (let* ((fetch-context (make-instance 'tool-context
                                                        :configuration configuration :worker nil
                                                        :conversation conversation :call-id call-id))
                           (result (memory-resource-tests--call registry fetch-context
                                                                "web_extra" "gist" "url" url)))
                      (conversation-append-tool-result
                       conversation call-id :tool-name "web_extra.gist"
                       :output (if corrupt-p "Different result, ready for publication." (tool-result-content result))
                       :success-p (tool-result-success-p result))
                      (when (and long-p (not fail-p))
                        (test-assert (and (< (length (tool-result-content result)) 8300)
                                          (not (search "The appendix is verified."
                                                       (tool-result-content result))))
                                     "the durable presentation contains only a bounded prefix and notice"))
                      result)))))
        ;; A successful write mentioning a source is not a retrieval, quoted or unquoted.
        (conversation-append-provider-item
         conversation (json-object "type" "function_call" "call_id" "write-1"
                                   "namespace" "resource" "name" "edit"
                                   "arguments" (json-encode (json-object "url" url))))
        (conversation-append-tool-result conversation "write-1" :tool-name "resource.edit"
                                         :output text :success-p t)
        (dolist (quote (list nil "ready for publication"))
          (test-assert (not (knowledge-node-observed-p (assert-fact quote)))
                       "a memory write cannot certify its own source"))
        (retrieve "failed" :fail-p t)
        (test-assert (not (knowledge-node-observed-p (assert-fact)))
                     "a failed retrieval supplies no observation")
        (retrieve "corrupt" :corrupt-p t)
        (test-assert (not (knowledge-node-observed-p (assert-fact)))
                     "a result with a different digest does not satisfy the receipt")
        (retrieve "fetched")
        (test-assert (knowledge-node-observed-p (assert-fact "ready for publication"))
                     "a completed matching retrieval supports an observation")
        (test-assert (null (memory-knowledge--url-receipt conversation url "not in the retrieved page"))
                     "the quote must occur in the identified result")
        (when long-p
          (test-assert (= (length text) 9000) "the retrieval exceeds the presentation limit")
          (test-assert (eql spill-p
                            (knowledge-node-observed-p
                             (assert-fact "The appendix is verified." "appendix")))
                       "a tail quotation requires verified retained overflow content")
          (test-assert (not (knowledge-node-observed-p
                            (assert-fact "characters omitted" "notice")))
                       "the presentation notice is not retrieved evidence"))
        (incf now 10)
        (test-assert (= 100000
                        (getf (memory-knowledge--url-receipt
                               conversation url "ready for publication" :configuration configuration)
                              :observed-at))
                     "receipt resolution preserves the original retrieval time")
        (setf now (+ 100001 *knowledge-url-lifetime*))
        (test-assert (eq (knowledge-node-applicability (assert-fact "ready for publication")) ':expired)
                     "citing an old retrieval does not renew its expiry")
        (retrieve "refetched")
        (test-assert (eq (knowledge-node-applicability (assert-fact "ready for publication")) ':current)
                     "fetching the page again renews its evidence")
        (when (and long-p spill-p)
          (let* ((receipt (memory-knowledge--url-receipt
                           conversation url "The appendix is verified." :configuration configuration))
                 (object (rlm-context-object-find configuration
                                                  (subseq (getf receipt :content-object) 8))))
            (test-assert (= now (getf receipt :observed-at))
                         "fresh retrieval evidence has the new retrieval time")
            (with-open-file (stream (rlm-context-object-pathname object)
                                    :direction ':output :if-exists ':supersede)
              (write-string "Corrupted retained content." stream))
            (test-assert (null (memory-knowledge--url-receipt
                                conversation url "The appendix is verified."
                                :configuration configuration))
                         "corrupt retained content cannot certify a quotation"))))))
  nil)

(-> test-memory-knowledge-rule-admission () null)
(defun test-memory-knowledge-rule-admission ()
  "Test that a proposed rule is admitted exactly when command authorization allows it."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (let* ((registry (make-default-tool-registry))
           (conversation (conversation-create configuration :identifier "rule-admission"))
           (requests nil))
      (flet ((propose (decision description)
               "Propose a departure rule version as a context deciding DECISION would."
               (let ((context (make-instance 'tool-context
                                             :configuration configuration
                                             :worker nil
                                             :conversation conversation
                                             :command-authorization-function
                                             (lambda (command directory)
                                               (declare (ignore directory))
                                               (push command requests)
                                               decision))))
                 (memory-resource-tests--call
                  registry context "resource" "edit"
                  "uri" "memory:facts"
                  "base-revision" (memory-resource-tests--field
                                   (tool-result-content
                                    (memory-resource-tests--call registry context "resource"
                                                                 "read" "uri" "memory:facts"))
                                   "Revision: ")
                  "operations" (vector (json-object
                                        "op" "rule-propose" "id" "departure"
                                        "description" description
                                        "premises" (vector (vector "event.start" "?e" "?s")
                                                           (vector "event.travel" "?e" "?t"))
                                        "bindings" (vector (vector "?l" (vector "-" "?s" "?t")))
                                        "conclusion" (vector "event.departure" "?e" "?l")))))))
        (test-assert (and (search "admission was declined" (tool-result-content
                                                            (propose ':deny "first")))
                          (null (knowledge-base-active-rules (knowledge-base-read configuration)
                                                             :workspace (namestring
                                                                         (config :working-directory
                                                                                 configuration)))))
                     "a declined rule stays inactive")
        (test-assert (equal (first requests)
                            "rule.admit -- departure v1: event.start(?e, ?s) and event.travel(?e, ?t) with ?l = (?s - ?t) => event.departure(?e, ?l)")
                     "the person approves a readable statement of the rule")
        (test-assert (search "Admitted rule departure version 2"
                             (tool-result-content (propose ':sandboxed "second")))
                     "an allowed proposal is admitted at once")
        (test-assert (search "unchanged and active"
                             (tool-result-content (propose ':deny "second")))
                     "proposing the active rule again asks nothing"))))
  nil)
