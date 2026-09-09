(in-package #:autolith)

;;;; -- Agenda Resource Test Support --

(-> agenda-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun agenda-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> agenda-resource-tests--field (string string) string)
(defun agenda-resource-tests--field (content label)
  "Return the single-line value following LABEL in agenda resource CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing agenda resource field ~S in ~S." label content))
    (subseq content value-start end)))

(-> agenda-resource-tests--operation (string &rest t) json-object)
(defun agenda-resource-tests--operation (name &rest fields)
  "Return one JSON agenda resource operation named NAME with FIELDS."
  (apply #'json-object "op" name fields))

(-> agenda-resource-tests--current-item (configuration) agenda-item)
(defun agenda-resource-tests--current-item (configuration)
  "Return CONFIGURATION's sole current workspace agenda item."
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((record (agenda-current configuration (agenda-load configuration)))
           (items (and record (workspace-agenda-items record))))
      (unless (= (length items) 1)
        (error "Expected one current agenda item, got ~D." (length items)))
      (first items))))


;;;; -- Agenda Resource Tests --

(-> test-agenda-resources () null)
(defun test-agenda-resources ()
  "Test agenda:current revision gating, confinement, compatibility, and schemas."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (first-workspace (merge-pathnames "agenda-first/" root))
         (second-workspace (merge-pathnames "agenda-second/" root))
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
                  (conversation-create configuration :identifier "agenda-resource-first"))
                (second-conversation
                  (conversation-create configuration :identifier "agenda-resource-second"))
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
                      "Execute one agenda resource fixture call."
                      (apply #'agenda-resource-tests--call
                             registry context namespace name arguments))

                    (read-agenda (context)
                      "Read agenda:current for CONTEXT."
                      (call context "resource" "read" "uri" "agenda:current"))

                    (revision (result)
                      "Return RESULT's model-visible resource revision alias."
                      (agenda-resource-tests--field
                       (tool-result-content result) "Revision: "))

                    (edit-agenda (context base operation &rest more-operations)
                      "Edit agenda:current at BASE with OPERATION and optional extras."
                      (call context "resource" "edit"
                            "uri" "agenda:current"
                            "base-revision" base
                            "operations"
                            (coerce (cons operation more-operations) 'vector))))
             (let ((resolver
                     (gethash "agenda"
                              (resource-registry-resolvers
                               (tool-registry-resource-registry registry)))))
               (test-assert (typep resolver 'agenda-resolver)
                            "default tools register the agenda resource resolver"))
             (let* ((empty-read (read-agenda first-context))
                    (empty-revision (revision empty-read))
                    (empty-state
                      (resource-observation-state-find
                       (conversation-resource-observations first-conversation)
                       empty-revision
                       'agenda-observation-state))
                    (empty-observation
                      (resource-observation-state-observation empty-state)))
               (test-assert (tool-result-success-p empty-read)
                            "resource.read observes an empty current agenda")
               (test-assert
                (search "The workspace agenda is empty."
                        (tool-result-content empty-read))
                "empty agenda resource read uses the existing readable rendering")
               (test-assert
                (equal (agenda-observation-snapshot empty-observation)
                       (list :directory
                             (agenda-directory-name
                              configuration first-workspace
                              :require-existing-p t)
                             :record nil))
                "empty agenda observations retain an exact explicit snapshot")
               (let* ((child-read (read-agenda child-context))
                      (child-edit
                        (edit-agenda
                         child-context empty-revision
                         (agenda-resource-tests--operation
                          "agenda-add" "text" "child mutation"))))
                 (test-assert
                  (and (not (tool-result-success-p child-read))
                       (search "unavailable under this authority context"
                               (tool-result-content child-read)))
                  "task child contexts cannot read agenda resources")
                 (test-assert
                  (and (not (tool-result-success-p child-edit))
                       (search "unavailable under this authority context"
                               (tool-result-content child-edit)))
                  "task child contexts cannot edit agenda resources"))
               (let* ((file (merge-pathnames "child-safe.txt" first-workspace))
                      (read-result nil)
                      (edit-result nil))
                 (workspace-resource-tests--write-text
                  file (format nil "before~%"))
                 (setf read-result
                       (call child-context "resource" "read"
                             "uri" "workspace:child-safe.txt")
                       edit-result
                       (call child-context "resource" "edit"
                             "uri" "workspace:child-safe.txt"
                             "base-revision" (revision read-result)
                             "operations"
                             (json-array
                              (agenda-resource-tests--operation
                               "replace-lines"
                               "start-line" 1
                               "end-line" 1
                               "content" "after"))))
                 (test-assert
                  (and (tool-result-success-p read-result)
                       (tool-result-success-p edit-result)
                       (string= (uiop:read-file-string file)
                                (format nil "after~%")))
                  "task child contexts retain revision-gated workspace resources"))
               (test-assert
                (not (tool-result-success-p
                      (call first-context "resource" "read"
                            "uri" "agenda:other")))
                "agenda identifiers other than current fail clearly")
               (test-assert
                (not (tool-result-success-p
                      (call first-context "resource" "read"
                            "uri" "agenda:current" "start-line" 1)))
                "agenda resource reads reject workspace line windows")
               (let* ((memory
                        (memory-remember configuration
                                         :title "Agenda resource attachment"
                                         :content "Validate stable memory links."
                                         :scope ':workspace
                                         :tags '("agenda-resource")))
                      (memory-id (memory-identifier memory))
                      (add-result
                        (edit-agenda
                         first-context empty-revision
                         (agenda-resource-tests--operation
                          "agenda-add"
                          "text" "Implement agenda resources"
                          "status" "doing"
                          "memory-ids" (json-array memory-id))))
                      (add-revision (revision add-result))
                      (item (agenda-resource-tests--current-item configuration)))
                 (test-assert (tool-result-success-p add-result)
                              "agenda-add applies against an observed empty revision")
                 (test-assert
                  (and (string= (agenda-item-text item)
                                "Implement agenda resources")
                       (eq (agenda-item-status item) ':doing)
                       (equal (agenda-item-memory-identifiers item)
                              (list memory-id)))
                  "agenda-add preserves text, status, and memory validation")
                 (test-assert
                  (search (agenda-item-identifier item)
                          (tool-result-content add-result))
                  "agenda-add returns a fresh complete readable observation")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-agenda
                         second-context add-revision
                         (agenda-resource-tests--operation
                          "agenda-update"
                          "id" (agenda-item-identifier item)
                          "status" "done"))))
                  "agenda revision aliases are isolated by conversation")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-agenda
                         moved-context add-revision
                         (agenda-resource-tests--operation
                          "agenda-update"
                          "id" (agenda-item-identifier item)
                          "status" "done"))))
                  "agenda revision aliases cannot cross current workspaces")
                 (let* ((update-result
                          (edit-agenda
                           first-context add-revision
                           (agenda-resource-tests--operation
                            "agenda-update"
                            "id" (agenda-item-identifier item)
                            "text" "Finish agenda resource integration"
                            "status" "blocked"
                            "memory-ids" (json-array))))
                        (update-revision (revision update-result))
                        (updated (agenda-resource-tests--current-item configuration)))
                   (test-assert (tool-result-success-p update-result)
                                "agenda-update applies one validated field set")
                   (test-assert
                    (and (string= (agenda-item-text updated)
                                  "Finish agenda resource integration")
                         (eq (agenda-item-status updated) ':blocked)
                         (null (agenda-item-memory-identifiers updated)))
                    "agenda-update changes fields and detaches memories")
                   (test-assert
                    (not (tool-result-success-p
                          (edit-agenda
                           first-context update-revision
                           (agenda-resource-tests--operation
                            "agenda-update"
                            "id" (agenda-item-identifier updated)))))
                    "agenda-update requires at least one changed field")
                   (test-assert
                    (not (tool-result-success-p
                          (edit-agenda
                           first-context update-revision
                           (agenda-resource-tests--operation
                            "agenda-remove"
                            "id" (agenda-item-identifier updated)
                            "text" "unexpected"))))
                    "agenda operations reject fields outside their closed variant")
                   (test-assert
                    (not (tool-result-success-p
                          (edit-agenda
                           first-context update-revision
                           (agenda-resource-tests--operation
                            "replace-lines"
                            "start-line" 1
                            "end-line" 1
                            "content" "wrong resource"))))
                    "agenda resources reject workspace operation variants")
                   (test-assert
                    (not (tool-result-success-p
                          (edit-agenda
                           first-context update-revision
                           (agenda-resource-tests--operation
                            "agenda-remove"
                            "id" (agenda-item-identifier updated))
                           (agenda-resource-tests--operation
                            "agenda-remove"
                            "id" (agenda-item-identifier updated)))))
                    "agenda resources accept exactly one operation per call")
                   (let ((remove-result
                           (edit-agenda
                            first-context update-revision
                            (agenda-resource-tests--operation
                             "agenda-remove"
                             "id" (agenda-item-identifier updated)))))
                     (test-assert (tool-result-success-p remove-result)
                                  "agenda-remove deletes an observed current item")
                     (test-assert
                      (search "The workspace agenda is empty."
                              (tool-result-content remove-result))
                      "agenda-remove returns a fresh empty observation"))))
                (let* ((empty-alias (revision (read-agenda first-context)))
                       (temporary-item
                         (with-recursive-lock-held (*agenda-lock*)
                           (let ((state (agenda-load configuration)))
                             (agenda-add
                              :configuration configuration
                              :state state
                              :text "temporary ABA mutation"
                              :status ':todo
                              :memory-identifiers nil)))))
                  (test-assert
                   (not (tool-result-success-p
                         (edit-agenda
                          first-context empty-alias
                          (agenda-resource-tests--operation
                           "agenda-add" "text" "stale resource mutation"))))
                   "an agenda alias is stale while the exact snapshot differs")
                  (with-recursive-lock-held (*agenda-lock*)
                    (agenda-remove configuration
                                   (agenda-load configuration)
                                   (agenda-item-identifier temporary-item)))
                  (let ((returned-alias (revision (read-agenda first-context))))
                    (test-assert
                     (string= returned-alias empty-alias)
                     "the exact returning agenda snapshot reuses its content alias"))
                  (let ((aba-edit
                          (edit-agenda
                           first-context empty-alias
                           (agenda-resource-tests--operation
                            "agenda-add" "text" "content-addressed ABA"))))
                    (test-assert
                     (tool-result-success-p aba-edit)
                     "an old alias is valid again when the exact agenda snapshot returns")
                    (let ((aba-item
                            (agenda-resource-tests--current-item configuration)))
                      (test-assert
                       (tool-result-success-p
                        (edit-agenda
                         first-context (revision aba-edit)
                         (agenda-resource-tests--operation
                          "agenda-remove" "id"
                          (agenda-item-identifier aba-item))))
                       "the ABA regression cleans up through its refreshed revision"))))
             (let* ((resource-edit
                      (tool-registry-find registry "resource" "edit"))
                    (operation-schema
                      (json-get
                       (json-get
                        (json-get (tool-parameters resource-edit) "properties")
                        "operations")
                       "items"))
                    (variants (json-get operation-schema "oneOf"))
                    (names
                      (loop for variant across variants
                            collect
                            (aref
                             (json-get
                              (json-get
                               (json-get variant "properties") "op")
                              "enum")
                             0))))
               (test-assert
                (and (member "replace-lines" names :test #'string=)
                     (member "agenda-add" names :test #'string=)
                     (member "agenda-update" names :test #'string=)
                     (member "agenda-remove" names :test #'string=))
                "resource.edit schema retains workspace and agenda variants")
               (test-assert
                (every (lambda (variant)
                         (eq (json-get variant "additionalProperties") false))
                       variants)
                "every resource.edit operation variant remains closed"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil))
