(in-package #:autolith)

;;;; -- Papercut Resource Test Support --

(-> papercut-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun papercut-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> papercut-resource-tests--field (string string) string)
(defun papercut-resource-tests--field (content label)
  "Return the single-line value following LABEL in papercut resource CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing papercut resource field ~S in ~S." label content))
    (subseq content value-start end)))

(-> papercut-resource-tests--operation (string &rest t) json-object)
(defun papercut-resource-tests--operation (name &rest fields)
  "Return one JSON papercut resource operation named NAME with FIELDS."
  (apply #'json-object "op" name fields))

(-> papercut-resource-tests--persist
    (configuration non-empty-string non-empty-string non-empty-string)
    papercut)
(defun papercut-resource-tests--persist (configuration identifier title content)
  "Persist one active report with exact IDENTIFIER for URI encoding tests."
  (let ((papercut
          (make-instance 'papercut
                         :identifier identifier
                         :reported-at (get-universal-time)
                         :workspace (papercut--workspace configuration)
                         :title title
                         :content content
                         :source-conversation "papercut-resource-fixture")))
    (with-lock-held (*papercut-lock*)
      (papercut--transact
       configuration
       (lambda (active)
         (declare (ignore active))
         (values (list (papercut--record papercut)) nil t))))
    papercut))


;;;; -- Papercut Resource Tests --

(-> test-papercut-resources () null)
(defun test-papercut-resources ()
  "Test papercut collection and exact-item reads, edits, confinement, and schemas."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (first-workspace (merge-pathnames "papercut-first/" root))
         (second-workspace (merge-pathnames "papercut-second/" root))
         (configuration nil)
         (second-configuration nil)
         (registry (make-default-tool-registry)))
    (ensure-directories-exist (merge-pathnames "marker" first-workspace))
    (ensure-directories-exist (merge-pathnames "marker" second-workspace))
    (setf configuration
          (configuration--clone base-configuration
                                :working-directory first-workspace)
          second-configuration
          (configuration--clone base-configuration
                                :working-directory second-workspace))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create
                   configuration :identifier "papercut-resource-first"))
                (second-conversation
                  (conversation-create
                   configuration :identifier "papercut-resource-second"))
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
                (moved-context
                  (make-instance 'tool-context
                                 :configuration second-configuration
                                 :worker nil
                                 :conversation first-conversation))
                (child-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation
                                 :agent (allocate-instance
                                         (find-class 'task-child-agent)))))
           (labels ((call (context namespace name &rest arguments)
                      "Execute one papercut resource fixture call."
                      (apply #'papercut-resource-tests--call
                             registry context namespace name arguments))

                    (read-resource (context uri &rest arguments)
                      "Read URI for CONTEXT with optional ARGUMENTS."
                      (apply #'call context "resource" "read"
                             "uri" uri arguments))

                    (revision (result)
                      "Return RESULT's model-visible resource revision alias."
                      (papercut-resource-tests--field
                       (tool-result-content result) "Revision: "))

                    (exact-uri (result)
                      "Return RESULT's canonical exact URI."
                      (papercut-resource-tests--field
                       (tool-result-content result) "Exact URI: "))

                    (edit-resource (context uri base operation &rest more)
                      "Edit URI at BASE with OPERATION and optional extras."
                      (call context "resource" "edit"
                            "uri" uri
                            "base-revision" base
                            "operations"
                            (coerce (cons operation more) 'vector))))
             (let ((resolver
                     (gethash "papercut"
                              (resource-registry-resolvers
                               (tool-registry-resource-registry registry)))))
               (test-assert (typep resolver 'papercut-resolver)
                            "default tools register the papercut resolver"))
             (let* ((empty-read (read-resource first-context "papercut:current"))
                    (empty-revision (revision empty-read))
                    (empty-state
                      (resource-observation-state-find
                       (conversation-resource-observations first-conversation)
                       empty-revision
                       'papercut-observation-state))
                    (empty-observation
                      (resource-observation-state-observation empty-state)))
               (test-assert (tool-result-success-p empty-read)
                            "resource.read observes the empty papercut collection")
               (test-assert
                (search "active papercuts: 0" (tool-result-content empty-read))
                "empty papercut collection rendering reports zero active items")
               (test-assert
                 (equal (papercut-observation-snapshot empty-observation)
                        (list :kind ':collection
                              :workspace (papercut--workspace configuration)
                              :identifiers nil
                              :assessments nil))
                "papercut collections retain a compact exact identifier snapshot")
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context "papercut:current"
                                     "start-line" 1)))
                "papercut resources reject line windows")
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context "papercut:current"
                                     "query" "failure")))
                "papercut resources reject memory collection filters")
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context "papercut:other")))
                "papercut resources reject unsupported collection identifiers")
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context "papercut:id/%")))
                "papercut resources reject malformed percent escapes")
               (test-assert
                (not (tool-result-success-p
                      (read-resource first-context "papercut:id/missing")))
                "exact papercut resources reject missing active reports")
               (test-assert
                (not (tool-result-success-p
                      (read-resource child-context "papercut:current")))
                "task children cannot resolve papercut resources")
               (let* ((report-operation
                        (papercut-resource-tests--operation
                         "papercut-report"
                         "title" "Resource papercut"
                         "content" "Complete resource report content."))
                      (reported
                        (edit-resource first-context
                                       "papercut:current"
                                       empty-revision
                                       report-operation))
                      (uri (exact-uri reported))
                      (papercut (first (papercut-list configuration))))
                 (test-assert (tool-result-success-p reported)
                              "papercut:current creates a report")
                 (test-assert (string= uri
                                      (resource-item-uri
                                       "papercut"
                                       (papercut-identifier papercut)))
                              "papercut report edits return the canonical exact URI")
                 (test-assert
                  (string= (papercut-source-conversation papercut)
                           (conversation-identifier first-conversation))
                  "papercut resource reports preserve source-conversation provenance")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-resource first-context
                                       "papercut:current"
                                       empty-revision
                                       report-operation)))
                  "collection edits reject stale snapshots")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-resource second-context
                                       "papercut:current"
                                       empty-revision
                                       report-operation)))
                  "papercut revisions are confined to their observing conversation")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-resource moved-context
                                       "papercut:current"
                                       empty-revision
                                       report-operation)))
                  "papercut revisions are confined to their observed workspace")
                  (let* ((item-read (read-resource first-context uri))
                         (item-revision (revision item-read)))
                    (test-assert
                     (and (tool-result-success-p item-read)
                          (search "Complete resource report content."
                                  (tool-result-content item-read))
                          (search "assessment: unassessed"
                                  (tool-result-content item-read)))
                     "exact papercut reads expose complete reports and assessment state")
                    (test-assert
                     (not (tool-result-success-p
                           (edit-resource first-context
                                          uri
                                          item-revision
                                          report-operation)))
                     "exact papercut items reject collection report operations")
                    (test-assert
                     (not (tool-result-success-p
                           (edit-resource
                            first-context
                            "papercut:current"
                            (revision
                             (read-resource first-context "papercut:current"))
                            (papercut-resource-tests--operation
                             "papercut-assess"
                             "verdict" "worse"
                             "note" "Wrong target."))))
                     "papercut:current rejects exact-item assessment operations")
                    (let* ((worse
                             (edit-resource
                              first-context
                              uri
                              item-revision
                              (papercut-resource-tests--operation
                               "papercut-assess"
                               "verdict" "worse"
                               "note" "The attempted fix increased failures.")))
                           (worse-revision (revision worse)))
                      (test-assert
                       (and (tool-result-success-p worse)
                            (search "assessment: worse"
                                    (tool-result-content worse))
                            (search "The attempted fix increased failures."
                                    (tool-result-content worse))
                            (eq (papercut-assessment-verdict
                                 (papercut-find
                                  configuration (papercut-identifier papercut)))
                                ':worse))
                       "papercut-assess appends and renders the latest outcome")
                      (test-assert
                       (not (tool-result-success-p
                             (edit-resource
                              first-context
                              uri
                              item-revision
                              (papercut-resource-tests--operation
                               "papercut-assess"
                               "verdict" "unchanged"
                               "note" "Stale assessment."))))
                       "papercut assessments require the latest item revision")
                      (let ((collection
                              (read-resource first-context "papercut:current")))
                        (test-assert
                         (and (search "assessment: worse"
                                      (tool-result-content collection))
                              (search "The attempted fix increased failures."
                                      (tool-result-content collection)))
                         "papercut collections expose unfavorable outcomes"))
                      (test-assert
                       (not (tool-result-success-p
                             (edit-resource
                              first-context
                              uri
                              worse-revision
                              (papercut-resource-tests--operation
                               "papercut-close"
                               "resolution" "Outcome remains unfavorable."))))
                       "resource closure rejects worse latest assessments")
                      (test-assert
                       (not (tool-result-success-p
                             (edit-resource
                              first-context
                              uri
                              worse-revision
                              (papercut-resource-tests--operation
                               "papercut-assess"
                               "verdict" "unknown"
                               "note" "Invalid verdict."))))
                       "resource assessments reject unknown verdicts")
                      (let* ((improved
                               (edit-resource
                                first-context
                                uri
                                worse-revision
                                (papercut-resource-tests--operation
                                 "papercut-assess"
                                 "verdict" "improved"
                                 "note" "Follow-up confirms the fix.")))
                             (improved-revision (revision improved)))
                        (test-assert
                         (and (tool-result-success-p improved)
                              (search "assessment: improved"
                                      (tool-result-content improved)))
                         "a later improved resource assessment supersedes worse")
                        (papercut-report
                         configuration
                         :title "Independent collection change"
                         :content "This must not stale an exact assessed report.")
                        (let ((closed
                                (edit-resource
                                 first-context
                                 uri
                                 improved-revision
                                 (papercut-resource-tests--operation
                                  "papercut-close"
                                  "resolution" "Implemented through resources."))))
                          (test-assert (tool-result-success-p closed)
                                       "improved exact papercuts may be closed")
                          (test-assert
                           (search "Implemented through resources."
                                   (tool-result-content closed))
                           "papercut closure results include the durable resolution")
                          (test-assert
                           (null (papercut-find
                                  configuration (papercut-identifier papercut)))
                           "papercut-close removes the report from active state")
                          (test-assert
                           (not (tool-result-success-p
                                 (read-resource first-context uri)))
                           "closed exact papercut resources become unavailable")
                          (test-assert
                           (not (tool-result-success-p
                                 (edit-resource
                                  first-context
                                  uri
                                  improved-revision
                                  (papercut-resource-tests--operation
                                   "papercut-close"
                                   "resolution" "Close twice."))))
                           "closed exact reports reject reuse of their old revision"))))))
             (let* ((identifier "custom/id å")
                    (fixture
                      (papercut-resource-tests--persist
                       configuration identifier "Encoded identifier" "Encoded body."))
                    (uri (resource-item-uri "papercut" identifier))
                    (read (read-resource first-context uri)))
               (test-assert
                (and (search "%2F" uri)
                     (search "%20" uri)
                     (tool-result-success-p read)
                     (search (papercut-title fixture) (tool-result-content read)))
                "canonical papercut item URIs percent-encode and decode arbitrary IDs"))
             (let* ((collection-read
                      (read-resource first-context "papercut:current"))
                    (collection-revision (revision collection-read))
                    (invalid-extra
                      (edit-resource
                       first-context
                       "papercut:current"
                       collection-revision
                       (papercut-resource-tests--operation
                        "papercut-report"
                        "title" "Invalid"
                        "content" "Invalid extra field."
                        "resolution" "Not accepted.")))
                    (invalid-count
                      (edit-resource
                       first-context
                       "papercut:current"
                       collection-revision
                       (papercut-resource-tests--operation
                        "papercut-report"
                        "title" "One"
                        "content" "One body.")
                       (papercut-resource-tests--operation
                        "papercut-report"
                        "title" "Two"
                        "content" "Two body."))))
               (test-assert
                (and (not (tool-result-success-p invalid-extra))
                     (not (tool-result-success-p invalid-count)))
                "papercut edits enforce closed variants and one operation per call"))
             (let* ((resource-edit
                      (tool-registry-find registry "resource" "edit"))
                    (operations
                      (gethash "items"
                               (gethash "operations"
                                        (gethash "properties"
                                                 (tool-parameters resource-edit)))))
                    (variants (gethash "oneOf" operations))
                    (names
                      (loop for variant across variants
                            for properties = (gethash "properties" variant)
                            for operation = (gethash "op" properties)
                            collect (aref (gethash "enum" operation) 0)))
                    (assessment-variant
                      (find "papercut-assess" variants
                            :test #'string=
                            :key (lambda (variant)
                                   (aref
                                    (gethash
                                     "enum"
                                     (gethash
                                      "op"
                                      (gethash "properties" variant)))
                                    0))))
                    (verdicts
                      (gethash
                       "enum"
                       (gethash
                        "verdict"
                        (gethash "properties" assessment-variant)))))
               (test-assert
                (and (member "papercut-report" names :test #'string=)
                     (member "papercut-assess" names :test #'string=)
                     (member "papercut-close" names :test #'string=)
                     (equalp verdicts
                             #("improved" "worse" "unchanged" "too-early"))
                     (every (lambda (variant)
                              (eq (gethash "additionalProperties" variant) false))
                            variants))
                "resource.edit advertises closed papercut assessment schemas"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil))