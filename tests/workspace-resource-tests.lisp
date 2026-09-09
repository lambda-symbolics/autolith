(in-package #:autolith)

;;;; -- Workspace Resource Test Support --

(-> workspace-resource-tests--write-text (pathname string) null)
(defun workspace-resource-tests--write-text (path text)
  "Replace PATH with exact UTF-8 TEXT for a workspace resource test."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :element-type '(unsigned-byte 8))
    (write-sequence (sb-ext:string-to-octets text :external-format ':utf-8)
                    stream))
  nil)

(-> workspace-resource-tests--field (string string) string)
(defun workspace-resource-tests--field (content label)
  "Return the single-line value following LABEL in tool result CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing result field ~S in ~S." label content))
    (subseq content value-start end)))


(-> workspace-resource-tests--line-anchor (string (integer 1)) string)
(defun workspace-resource-tests--line-anchor (content line)
  "Return LINE's rendered hash anchor from resource result CONTENT."
  (let* ((prefix (format nil "~6D:" line))
         (start (search prefix content)))
    (unless start
      (error "Missing rendered line ~D in ~S." line content))
    (subseq content (+ start (length prefix))
            (+ start (length prefix) 4))))

(-> workspace-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun workspace-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> workspace-resource-tests--operation (string &rest t) json-object)
(defun workspace-resource-tests--operation (name &rest fields)
  "Return one JSON resource edit operation named NAME with FIELDS."
  (apply #'json-object "op" name fields))

(defclass workspace-resource-tests-coordination ()
  ((lock
    :initform (make-lock "workspace resource read coordination")
    :reader workspace-resource-tests-coordination-lock
    :documentation "The lock protecting coordinated test state.")
   (condition
    :initform (make-condition-variable)
    :reader workspace-resource-tests-coordination-condition
    :documentation "The condition reporting coordinated observation progress.")
   (observed-p
    :initform nil
    :accessor workspace-resource-tests-coordination-observed-p
    :type boolean
    :documentation "Whether the coordinated resource observation completed."))
  (:documentation "Synchronization state for a deterministic resource.read race test."))

(defclass workspace-resource-tests-coordinated-resource
    (workspace-file-resource)
  ((coordination
    :initarg :coordination
    :reader workspace-resource-tests-coordinated-resource-coordination
    :type workspace-resource-tests-coordination
    :documentation "The synchronization state signaled after exact observation."))
  (:documentation "A workspace resource reporting when its exact observation completes."))

(defclass workspace-resource-tests-coordinated-resolver
    (workspace-file-resolver)
  ((coordination
    :initarg :coordination
    :reader workspace-resource-tests-coordinated-resolver-coordination
    :type workspace-resource-tests-coordination
    :documentation "The synchronization state shared with resolved resources."))
  (:documentation "Resolve coordinated workspace resources for serialization testing."))

(defclass workspace-resource-tests-other-observation-state
    (resource-observation-state)
  ()
  (:documentation "A non-workspace observation state sharing conversation storage."))

(defmethod resource-resolver-resolve
    ((resolver workspace-resource-tests-coordinated-resolver) identifier context)
  "Resolve IDENTIFIER as a coordinated workspace resource under CONTEXT."
  (let ((path (workspace-tool-resolve-path context (url-decode identifier))))
    (make-instance 'workspace-resource-tests-coordinated-resource
                   :uri          (workspace-file--canonical-uri context path)
                   :pathname     path
                   :coordination
                   (workspace-resource-tests-coordinated-resolver-coordination
                    resolver))))

(defmethod resource-observe :around
    ((resource workspace-resource-tests-coordinated-resource)
     (context tool-context))
  "Report completion after observing coordinated RESOURCE under CONTEXT."
  (let* ((observation (call-next-method))
         (coordination
           (workspace-resource-tests-coordinated-resource-coordination resource)))
    (with-lock-held ((workspace-resource-tests-coordination-lock coordination))
      (setf (workspace-resource-tests-coordination-observed-p coordination) t)
      (condition-notify
       (workspace-resource-tests-coordination-condition coordination)))
    observation))


;;;; -- Workspace Resource Tests --

(-> test-workspace-file-resources () null)
(defun test-workspace-file-resources ()
  "Test revision-gated workspace files, directories, missing paths, and tools."
  (let* ((base-configuration     (test-configuration))
         (root                   (test-configuration-root base-configuration))
         (workspace              (merge-pathnames "workspace/" root))
         (configuration          nil)
         (registry               (make-default-tool-registry))
         (authorization-requests nil))
    (ensure-directories-exist workspace)
    (setf configuration
          (configuration--clone base-configuration
                                :working-directory workspace))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration :identifier "resource-first"))
                (second-conversation
                  (conversation-create configuration :identifier "resource-second"))
                (heterogeneous-conversation
                  (conversation-create configuration
                                       :identifier "resource-heterogeneous"))
                (first-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation))
                (full-access-context
                  (make-instance
                   'tool-context
                   :configuration configuration
                   :worker nil
                   :conversation first-conversation
                   :command-authorization-function
                   (lambda (command directory)
                     (push (list command directory) authorization-requests)
                     ':full-access)))
                (sandboxed-context
                  (make-instance
                   'tool-context
                   :configuration configuration
                   :worker nil
                   :conversation first-conversation
                   :command-authorization-function
                   (lambda (command directory)
                     (declare (ignore command directory))
                     ':sandboxed)))
                (second-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation second-conversation))
                (heterogeneous-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation heterogeneous-conversation)))
           (labels ((call (context namespace name &rest arguments)
                      "Execute one resource fixture call."
                      (apply #'workspace-resource-tests--call
                             registry context namespace name arguments))

                    (read-resource (context uri &key start-line line-count)
                      "Read URI and return result, canonical URI, and revision."
                      (let ((arguments (list "uri" uri)))
                        (when start-line
                          (setf arguments
                                (append arguments (list "start-line" start-line))))
                        (when line-count
                          (setf arguments
                                (append arguments (list "line-count" line-count))))
                        (let* ((result (apply #'call context "resource" "read"
                                              arguments))
                               (content (tool-result-content result)))
                          (values result
                                  (and (tool-result-success-p result)
                                       (workspace-resource-tests--field
                                        content "URI: "))
                                  (and (tool-result-success-p result)
                                       (workspace-resource-tests--field
                                        content "Revision: "))))))

                    (edit-resource (context uri revision operations)
                      "Edit URI at REVISION with OPERATIONS."
                      (call context "resource" "edit"
                            "uri" uri
                            "base-revision" revision
                            "operations" (coerce operations 'vector))))
               (let ((path (merge-pathnames "src/c++/x.cpp" workspace)))
                 (workspace-resource-tests--write-text path "plus path")
                 (multiple-value-bind (result canonical-uri revision)
                     (read-resource first-context "workspace:src/c++/x.cpp")
                   (declare (ignore revision))
                   (test-assert
                    (and (tool-result-success-p result)
                         (string= canonical-uri "workspace:src/c%2B%2B/x.cpp")
                         (search "plus path" (tool-result-content result)))
                    "workspace URIs preserve literal plus characters")))
              (with-test-fixture (':wildcard-file-names
                                  "workspace paths holding pathname metacharacters")
                (let* ((relative "src/notes*[1]?\\x.md")
                       (path
                         (merge-pathnames
                          (uiop:parse-native-namestring relative) workspace)))
                  (workspace-resource-tests--write-text path "native path")
                  (multiple-value-bind (result canonical-uri revision)
                      (read-resource
                       first-context "workspace:src/notes*[1]?\\x.md")
                    (test-assert
                     (and (tool-result-success-p result)
                          (string=
                           canonical-uri
                           "workspace:src/notes%2A%5B1%5D%3F%5Cx.md")
                          (search "native path" (tool-result-content result)))
                     "workspace resources preserve native pathname metacharacters")
                    (let ((edited
                            (edit-resource
                             first-context canonical-uri revision
                             (list
                              (workspace-resource-tests--operation
                               "replace-lines"
                               "start-line" 1
                               "end-line" 1
                               "content" "edited native path")))))
                      (test-assert
                       (and (tool-result-success-p edited)
                            (search "edited native path"
                                    (tool-result-content edited)))
                       "resource.edit publishes files with native pathname metacharacters")))))
              (let ((path (merge-pathnames "descriptor-growth.txt" workspace)))
                (workspace-resource-tests--write-text path "before")
                (test-assert
                 (handler-case
                     (progn
                       (file--read-bounded-utf-8
                        path
                        :maximum-bytes 64
                        :tool-name "test.read"
                        :description "Test file"
                        :validation-function
                        (lambda ()
                          (with-open-file
                              (stream path
                                      :direction ':output
                                      :if-exists ':append
                                      :element-type '(unsigned-byte 8))
                            (write-sequence
                             (sb-ext:string-to-octets
                              "after"
                              :external-format ':utf-8)
                             stream))))
                       nil)
                   (tool-error ()
                     t))
                 "bounded descriptor reads reject files that grow during validation"))
              (let ((path (merge-pathnames "retained-ranges.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%four~%"))
                (multiple-value-bind (first-result uri alias)
                    (read-resource first-context "workspace:retained-ranges.txt"
                                   :start-line 1 :line-count 1)
                  (multiple-value-bind (second-result second-uri second-alias)
                      (read-resource first-context "workspace:retained-ranges.txt"
                                     :start-line 3 :line-count 1)
                    (declare (ignore second-uri))
                    (let ((state
                            (workspace-file--find-observation-state
                             first-conversation uri alias)))
                      (test-assert
                       (and (tool-result-success-p first-result)
                            (tool-result-success-p second-result)
                            (string= alias second-alias)
                            (equal
                             (workspace-file-observation-state-visible-ranges state)
                             '((1 1) (3 3))))
                       "equivalent workspace reads reuse state and merge visible ranges")))))
             (let* ((path (merge-pathnames "heterogeneous.txt" workspace))
                    (other-observation
                      (make-instance 'resource-observation
                                     :uri "workspace:heterogeneous.txt"
                                     :revision "foreign-revision"
                                     :content 42))
                    (other-state
                      (make-instance
                       'workspace-resource-tests-other-observation-state
                       :alias "Rforeign"
                       :observation other-observation)))
               (workspace-resource-tests--write-text path "workspace")
                (with-recursive-lock-held
                    ((conversation-resource-observation-lock
                      heterogeneous-conversation))
                  (fifo-cache-put
                   (conversation-resource-observations heterogeneous-conversation)
                   "Rforeign"
                   other-state))
               (let ((*workspace-file-resource-maximum-observations* 1))
                 (multiple-value-bind (result uri revision)
                     (read-resource heterogeneous-context
                                    "workspace:heterogeneous.txt")
                   (test-assert (tool-result-success-p result)
                                "workspace observations ignore other state classes")
                   (with-recursive-lock-held
                       ((conversation-resource-observation-lock
                         heterogeneous-conversation))
                     (let ((states
                             (conversation-resource-observations
                              heterogeneous-conversation)))
                       (test-assert
                        (eq (resource-observation-state-find
                             states "Rforeign"
                             'workspace-resource-tests-other-observation-state)
                            other-state)
                        "workspace observation expiry preserves other resource states")
                        (test-assert
                         (and (= (fifo-cache-count states) 2)
                              (typep (fifo-cache-get states revision)
                                     'workspace-file-observation-state))
                         "workspace and other resource observation states coexist")))
                   (test-assert
                    (handler-case
                        (progn
                          (workspace-file--find-observation-state
                           heterogeneous-conversation uri "Rforeign")
                          nil)
                      (resource-revision-stale ()
                        t))
                    "workspace observation lookup rejects another state class"))))
              (let* ((retention-conversation
                       (conversation-create configuration
                                            :identifier "resource-retention"))
                     (retention-context
                       (make-instance 'tool-context
                                      :configuration configuration
                                      :worker nil
                                      :conversation retention-conversation))
                      (path (merge-pathnames "retained.txt" workspace))
                      (other-observation
                        (make-instance 'resource-observation
                                       :uri "memory:retained"
                                       :revision "other-retained-revision"
                                       :content nil))
                      (other-state
                        (make-instance
                         'workspace-resource-tests-other-observation-state
                         :alias "Rretained-other"
                         :observation other-observation)))
                 (with-recursive-lock-held
                     ((conversation-resource-observation-lock
                       retention-conversation))
                   (fifo-cache-put
                    (conversation-resource-observations retention-conversation)
                    "Rretained-other"
                    other-state))
                 (let ((*workspace-file-resource-maximum-retained-bytes* 12))
                   (workspace-resource-tests--write-text path "ááá")
                   (multiple-value-bind (first-result uri first-revision)
                       (read-resource retention-context "workspace:retained.txt")
                     (test-assert (tool-result-success-p first-result)
                                  "workspace observations establish a retained revision")
                     (let* ((state
                              (workspace-file--find-observation-state
                               retention-conversation uri first-revision))
                            (observation
                              (resource-observation-state-observation state)))
                       (test-assert
                        (null (workspace-file-observation-stored-lines observation))
                        "workspace observations do not retain duplicated logical lines")
                       (test-assert
                        (equalp (workspace-file-observation-lines observation)
                                #("ááá"))
                        "workspace observation lines remain available on demand"))
                     (workspace-resource-tests--write-text path "žžžž")
                     (multiple-value-bind (second-result second-uri second-revision)
                         (read-resource retention-context "workspace:retained.txt")
                       (test-assert (tool-result-success-p second-result)
                                    "workspace observations accept a replacement snapshot")
                       (test-assert
                        (handler-case
                            (progn
                              (workspace-file--find-observation-state
                               retention-conversation uri first-revision)
                              nil)
                          (resource-revision-stale ()
                            t))
                        "workspace byte budget evicts the oldest multibyte revision")
                       (let* ((states
                                (conversation-resource-observations
                                 retention-conversation))
                              (second-state
                                (workspace-file--find-observation-state
                                 retention-conversation
                                 second-uri second-revision)))
                         (test-assert
                          (and (= (fifo-cache-count states) 2)
                               second-state
                               (= (fifo-cache-total-weight states) 8)
                               (eq (resource-observation-state-find
                                    states
                                    "Rretained-other"
                                    'workspace-resource-tests-other-observation-state)
                                   other-state))
                          "workspace byte eviction preserves zero-weight resource families"))))))
             (let* ((resolver
                      (gethash "workspace"
                               (resource-registry-resolvers
                                (tool-registry-resource-registry registry))))
                    (spaced-path (merge-pathnames "space name.txt" workspace)))
               (workspace-resource-tests--write-text spaced-path "content")
               (test-assert (typep resolver 'workspace-file-resolver)
                            "default registries install the workspace resolver")
               (let ((resource
                       (resource-registry-resolve
                        (tool-registry-resource-registry registry)
                        "workspace:space%20name.txt"
                        first-context)))
                 (test-assert
                  (string= (resource-uri resource)
                           "workspace:space%20name.txt")
                  "workspace resolution returns a canonical percent-encoded URI")
                 (test-assert
                  (equal (truename (workspace-file-resource-pathname resource))
                         (truename spaced-path))
                  "workspace resolution reuses workspace-relative path semantics")))
              (multiple-value-bind (result uri revision)
                  (read-resource first-context "workspace:.")
                (test-assert
                 (and (tool-result-success-p result)
                      (string= uri "workspace:.")
                      (non-empty-string-p revision)
                      (search "Kind: directory" (tool-result-content result)))
                 "workspace:. reads the workspace root as a canonical directory resource"))
              (let* ((outside-directory (merge-pathnames "outside/" root))
                     (outside-path      (merge-pathnames "secret.txt"
                                                         outside-directory))
                     (outside-uri
                       (format nil "workspace:~A"
                               (workspace-file--encode-identifier
                                (namestring outside-path)))))
                (workspace-resource-tests--write-text outside-path "secret")
                (let ((resource
                        (resource-registry-resolve
                         (tool-registry-resource-registry registry)
                         outside-uri first-context)))
                  (test-assert
                   (uiop:pathname-equal
                    (workspace-file-resource-pathname resource)
                    (truename outside-path))
                   "workspace resource resolution is authority-neutral"))
                (test-assert
                 (not (tool-result-success-p
                       (call first-context "resource" "read" "uri" outside-uri)))
                 "resource.read denies unapproved out-of-root paths")
                (test-assert
                 (not (tool-result-success-p
                       (call sandboxed-context
                             "resource" "read" "uri" outside-uri)))
                 "resource.read requires full access for out-of-root paths")
                (multiple-value-bind (result canonical-uri revision)
                    (read-resource full-access-context outside-uri)
                  (declare (ignore canonical-uri revision))
                  (test-assert
                   (and (tool-result-success-p result)
                        (search "secret" (tool-result-content result)))
                   "resource.read accepts full-access approval for out-of-root paths"))
                (with-test-fixture (':symbolic-links
                                    "out-of-root resources reached through symlinks")
                  (let ((escape (merge-pathnames "escape" workspace)))
                    (unwind-protect
                         (progn
                           (test-fixture-make-symbolic-link
                            *platform*
                            (namestring outside-directory)
                            (namestring escape))
                           (dolist (uri '(("workspace:escape" . "outside/")
                                          ("workspace:escape/new.txt"
                                           . "outside/new.txt")))
                             (let ((resource
                                     (resource-registry-resolve
                                      (tool-registry-resource-registry registry)
                                      (first uri) first-context)))
                               (test-assert
                                (uiop:pathname-equal
                                 (workspace-file-resource-pathname resource)
                                 (merge-pathnames (rest uri) root))
                                (format nil
                                        "workspace resource resolution preserves symlink target ~A"
                                        (first uri)))))
                           (multiple-value-bind (result canonical-uri revision)
                               (read-resource full-access-context
                                              "workspace:escape/new.txt")
                             (test-assert
                              (and (tool-result-success-p result)
                                   (search "Kind: missing"
                                           (tool-result-content result)))
                              "resource.read observes an approved missing out-of-root path")
                             (let ((denied
                                     (edit-resource
                                      first-context canonical-uri revision
                                      (list
                                       (workspace-resource-tests--operation
                                        "replace-empty" "content" "created")))))
                               (test-assert
                                (and (not (tool-result-success-p denied))
                                     (not (probe-file
                                           (merge-pathnames "new.txt"
                                                            outside-directory))))
                                "resource.edit denies an unapproved out-of-root creation"))
                             (let ((edited
                                     (edit-resource
                                      full-access-context canonical-uri revision
                                      (list
                                       (workspace-resource-tests--operation
                                        "replace-empty" "content" "created")))))
                               (test-assert
                                (and (tool-result-success-p edited)
                                     (string=
                                      (workspace-file--read-content
                                       (merge-pathnames "new.txt" outside-directory))
                                      "created"))
                                "resource.edit accepts full-access approval for out-of-root creation"))))
                      (when (probe-file escape)
                        (test-fixture-remove-link *platform* (namestring escape)))))
                  (test-assert
                   (and (some (lambda (request)
                                (uiop:string-prefix-p "resource.read -- "
                                                      (first request)))
                              authorization-requests)
                        (some (lambda (request)
                                (uiop:string-prefix-p "resource.edit -- "
                                                      (first request)))
                              authorization-requests)
                        (every (lambda (request)
                                 (uiop:pathname-equal (second request) workspace))
                               authorization-requests))
                   "out-of-root resource authorization uses the active workspace directory")))
             (let ((oversized (merge-pathnames "oversized.txt" workspace)))
               (workspace-resource-tests--write-text oversized "123456789")
               (let ((*workspace-file-resource-maximum-bytes* 8))
                 (let ((result
                         (read-resource first-context
                                        "workspace:oversized.txt")))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "snapshot" (tool-result-content result)))
                    "resource.read rejects oversized exact snapshots with bounded guidance"))))
             (let ((invalid (merge-pathnames "invalid-utf8.txt" workspace)))
               (with-open-file (stream invalid
                                       :direction ':output
                                       :if-exists ':supersede
                                       :if-does-not-exist ':create
                                       :element-type '(unsigned-byte 8))
                 (write-byte #xFF stream))
               (let ((result
                       (read-resource first-context
                                      "workspace:invalid-utf8.txt")))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (search "valid UTF-8" (tool-result-content result)))
                  "resource.read rejects invalid UTF-8 without retaining a snapshot")))
             (with-test-fixture (':fifos "resource.read of special files")
               (let ((fifo (merge-pathnames "not-regular" workspace)))
                 (unwind-protect
                      (progn
                        (test-fixture-make-fifo *platform* fifo)
                        (let ((result
                                (read-resource first-context
                                               "workspace:not-regular")))
                          (test-assert
                           (and (not (tool-result-success-p result))
                                (search "not a regular file"
                                        (tool-result-content result)))
                           "resource.read rejects special files before opening them")))
                   (when (probe-file fifo)
                     (sb-posix:unlink (namestring fifo))))))
             (let ((path (merge-pathnames "visible.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "one~%two~%three~%four~%five~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:visible.txt"
                                  :start-line 2 :line-count 2)
                 (test-assert (tool-result-success-p read-result)
                              "resource.read observes existing workspace files")
                 (test-assert
                  (and (search "Visible lines: 2-3 of 5"
                               (tool-result-content read-result))
                       (search "Elided: 1-1 before; 4-5 after"
                               (tool-result-content read-result))
                        (search (format nil "     2:~A  two"
                                        (workspace-file--line-anchor "two"))
                                (tool-result-content read-result)))
                  "resource.read reports URI, revision, visible range, elisions, and numbered content")
                 (test-assert
                  (not (tool-result-success-p
                        (edit-resource
                         first-context uri revision
                         (list
                          (workspace-resource-tests--operation
                           "replace-lines"
                           "start-line" 1
                           "end-line" 1
                           "content" "ONE")))))
                  "resource.edit rejects original lines outside the exact visible range")
                 (let ((isolated
                         (edit-resource
                          second-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 2
                            "end-line" 2
                            "content" "TWO")))))
                   (test-assert
                    (and (not (tool-result-success-p isolated))
                         (search "not observed in this conversation"
                                 (tool-result-content isolated)))
                    "resource revision aliases are isolated by conversation"))
                 (conversation-append-user-message
                  first-conversation "persist without resource snapshots")
                 (let* ((reloaded
                          (conversation-load-by-id configuration "resource-first"))
                        (reloaded-context
                          (make-instance 'tool-context
                                         :configuration configuration
                                         :worker nil
                                         :conversation reloaded))
                        (expired
                          (edit-resource
                           reloaded-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-lines"
                             "start-line" 2
                             "end-line" 2
                             "content" "TWO")))))
                    (test-assert
                     (and (not (tool-result-success-p expired))
                          (zerop (fifo-cache-count
                                  (conversation-resource-observations reloaded))))
                     "resource observations stay out of conversation persistence and expire after reload"))))
              (let ((path (merge-pathnames "anchors-exact.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "alpha~%beta~%gamma~%delta~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-exact.txt")
                  (let* ((content (tool-result-content read-result))
                         (start-anchor
                           (workspace-resource-tests--line-anchor content 2))
                         (end-anchor
                           (workspace-resource-tests--line-anchor content 3))
                         (result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "replace-lines"
                              "start-line" 2
                              "start-anchor" start-anchor
                              "end-line" 3
                              "end-anchor" end-anchor
                              "content" "middle")))))
                    (test-assert
                     (and (tool-result-success-p result)
                          (string= (workspace-file--read-content path)
                                   (format nil "alpha~%middle~%delta~%")))
                     "resource.edit accepts exact range-boundary anchors"))))
              (let ((path (merge-pathnames "anchors-offset.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%four~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-offset.txt")
                  (let* ((anchor
                           (workspace-resource-tests--line-anchor
                            (tool-result-content read-result) 3))
                         (result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "insert-after"
                              "line" 2
                              "anchor" anchor
                              "content" "three-and-a-half")))))
                    (test-assert
                     (and (tool-result-success-p result)
                          (search "insert-after 3"
                                  (tool-result-content result))
                          (string= (workspace-file--read-content path)
                                   (format nil
                                           "one~%two~%three~%three-and-a-half~%four~%")))
                     "resource.edit corrects a small anchored line-number offset"))))
              (let ((path (merge-pathnames "anchors-large-offset.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%four~%five~%six~%seven~%eight~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-large-offset.txt")
                  (let* ((anchor
                           (workspace-resource-tests--line-anchor
                            (tool-result-content read-result) 8))
                         (result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "insert-before"
                              "line" 1
                              "anchor" anchor
                              "content" "no")))))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (search "maximum correction offset"
                                  (tool-result-content result)))
                     "resource.edit refuses a large anchored line-number offset"))))
              (let ((path (merge-pathnames "anchors-ambiguous.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%duplicate~%three~%duplicate~%five~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-ambiguous.txt")
                  (let* ((anchor
                           (workspace-resource-tests--line-anchor
                            (tool-result-content read-result) 2))
                         (result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "insert-before"
                              "line" 3
                              "anchor" anchor
                              "content" "no")))))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (search "ambiguous" (tool-result-content result)))
                     "resource.edit refuses an ambiguous nearby anchor"))))
              (let ((path (merge-pathnames "anchors-missing.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-missing.txt")
                  (declare (ignore read-result))
                  (let ((result
                          (edit-resource
                           first-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "insert-after"
                             "line" 2
                             "anchor" (workspace-file--line-anchor "absent")
                             "content" "no")))))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (search "does not match any visible line"
                                  (tool-result-content result)))
                     "resource.edit refuses a missing anchor"))))
              (let ((path (merge-pathnames "anchors-compatible.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-compatible.txt")
                  (declare (ignore read-result))
                  (let ((result
                          (edit-resource
                           first-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-lines"
                             "start-line" 2
                             "end-line" 2
                             "content" "TWO")))))
                    (test-assert
                     (and (tool-result-success-p result)
                          (string= (workspace-file--read-content path)
                                   (format nil "one~%TWO~%three~%")))
                     "resource.edit remains compatible when anchors are omitted"))))
              (let ((path (merge-pathnames "anchors-stale.txt" workspace)))
                (workspace-resource-tests--write-text
                 path (format nil "one~%two~%three~%"))
                (multiple-value-bind (read-result uri revision)
                    (read-resource first-context "workspace:anchors-stale.txt")
                  (let ((anchor
                          (workspace-resource-tests--line-anchor
                           (tool-result-content read-result) 2)))
                    (workspace-resource-tests--write-text
                     path (format nil "external~%two~%three~%"))
                    (let ((result
                            (edit-resource
                             first-context uri revision
                             (list
                              (workspace-resource-tests--operation
                               "replace-lines"
                               "start-line" 2
                               "start-anchor" anchor
                               "end-line" 2
                               "end-anchor" anchor
                               "content" "TWO")))))
                      (test-assert
                       (and (not (tool-result-success-p result))
                            (search "stale" (tool-result-content result))
                            (string= (workspace-file--read-content path)
                                     (format nil "external~%two~%three~%")))
                       "resource.edit rejects stale revisions before anchored correction")))))
             (let ((path (merge-pathnames "empty.txt" workspace)))
               (workspace-resource-tests--write-text path "")
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:empty.txt")
                 (test-assert
                  (and (tool-result-success-p read-result)
                       (search "Visible lines: none of 0"
                               (tool-result-content read-result)))
                  "resource.read establishes an exact revision for an empty file")
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-empty" "content"
                            (format nil "first~%second~%"))))))
                   (test-assert (tool-result-success-p result)
                                "resource.edit can populate an observed empty file")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "first~%second~%"))
                    "replace-empty preserves supplied content and final newline"))))
             (let ((path (merge-pathnames "multi.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "one~%two~%three~%four~%five~%six~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:multi.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 2
                            "end-line" 3
                            "content" (format nil "TWO~%THREE")
                            )
                           (workspace-resource-tests--operation
                            "insert-after"
                            "line" 5
                            "content" (format nil "five-and-a-half"))))))
                   (test-assert (tool-result-success-p result)
                                "resource.edit applies non-overlapping multi-operations")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil
                                     "one~%TWO~%THREE~%four~%five~%five-and-a-half~%six~%"))
                    "resource.edit interprets every operation against original line numbers")
                   (let ((new-revision
                           (workspace-resource-tests--field
                            (tool-result-content result) "Revision: ")))
                     (test-assert (not (string= revision new-revision))
                                  "successful edits return a fresh revision alias")
                     (test-assert
                      (search "Visible lines:"
                              (tool-result-content result))
                      "successful edits return a refreshed nearby observation")
                     (test-assert
                      (tool-result-success-p
                       (edit-resource
                        first-context uri new-revision
                        (list
                         (workspace-resource-tests--operation
                          "replace-lines"
                          "start-line" 4
                          "end-line" 4
                          "content" "FOUR"))))
                      "the refreshed observation supports the next nearby edit")))))
             (let ((path (merge-pathnames "overlap.txt" workspace)))
               (workspace-resource-tests--write-text
                path (format nil "a~%b~%c~%d~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:overlap.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "delete-lines" "start-line" 2 "end-line" 3)
                           (workspace-resource-tests--operation
                            "insert-before" "line" 3 "content" "x")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "overlap or ambiguously share"
                                 (tool-result-content result)))
                    "resource.edit rejects overlapping or ambiguous original-line operations")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "a~%b~%c~%d~%"))
                    "overlap rejection leaves the file unchanged"))))
             (let ((path (merge-pathnames "stale.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "old~%value~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:stale.txt")
                 (declare (ignore read-result))
                 (workspace-resource-tests--write-text
                  path (format nil "external~%value~%"))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 1
                            "end-line" 1
                            "content" "new")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Reread" (tool-result-content result)))
                    "resource.edit rejects stale current content with actionable reread guidance")
                   (test-assert
                    (string= (workspace-file--read-content path)
                             (format nil "external~%value~%"))
                    "stale revision rejection preserves externally changed content")))
               (let ((expired
                       (edit-resource
                        first-context "workspace:stale.txt" "Rexpired"
                        (list
                         (workspace-resource-tests--operation
                          "replace-lines"
                          "start-line" 1
                          "end-line" 1
                          "content" "new")))))
                 (test-assert
                  (and (not (tool-result-success-p expired))
                       (search "expired" (tool-result-content expired)))
                  "unrecorded or expired revision aliases require a reread")))
             (let* ((crlf (format nil "~C~C" #\Return #\Newline))
                    (path (merge-pathnames "crlf.txt" workspace))
                    (original (format nil "one~Atwo~A" crlf crlf)))
               (workspace-resource-tests--write-text path original)
               (when (test-fixture-available-p *platform* ':file-modes)
                 (test-fixture-set-file-mode *platform* path #o640))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:crlf.txt")
                 (declare (ignore read-result))
                 (test-assert
                  (tool-result-success-p
                   (edit-resource
                    first-context uri revision
                    (list
                     (workspace-resource-tests--operation
                      "replace-lines"
                      "start-line" 2
                      "end-line" 2
                      "content" "TWO"))))
                  "resource.edit rewrites CRLF files")
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "one~ATWO~A" crlf crlf))
                  "resource.edit preserves CRLF and the final newline")
                 (with-test-fixture (':file-modes "resource.edit preserving file modes")
                   (test-assert (= (test-fixture-file-mode *platform* path) #o640)
                                "resource.edit preserves file permissions where practical")))
             (let* ((crlf (format nil "~C~C" #\Return #\Newline))
                    (path (merge-pathnames "mixed-line-endings.txt" workspace))
                    (original (format nil "one~%two~Athree~%" crlf)))
               (workspace-resource-tests--write-text path original)
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context
                                  "workspace:mixed-line-endings.txt")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines" "start-line" 2 "end-line" 2
                            "content" "TWO")))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "mixed LF and CRLF"
                                 (tool-result-content result)))
                    "resource.edit refuses to normalize untouched mixed line endings")
                   (test-assert
                    (string= (workspace-file--read-content path) original)
                    "mixed-line-ending refusal preserves exact content"))))
             (let ((path (merge-pathnames "no-final-newline.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "one~%two"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:no-final-newline.txt")
                 (declare (ignore read-result))
                 (test-assert
                  (tool-result-success-p
                   (edit-resource
                    first-context uri revision
                    (list
                     (workspace-resource-tests--operation
                      "insert-after" "line" 1 "content" "middle"))))
                  "resource.edit rewrites files without final newlines")
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "one~%middle~%two"))
                  "resource.edit preserves absence of a final newline")))
             (let ((path (merge-pathnames "oversized-edit.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "a~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:oversized-edit.txt")
                 (declare (ignore read-result))
                 (let* ((publish-called-p nil)
                       (*workspace-file-resource-maximum-bytes* 6)
                       (*workspace-file-resource-publish-function*
                         (lambda (source target)
                           (declare (ignore source target))
                           (setf publish-called-p t))))
                   (let ((result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "replace-lines"
                              "start-line" 1
                              "end-line" 1
                              "content"
                              (make-string 3
                                           :initial-element (code-char #xE9)))))))
                     (test-assert
                      (and (not (tool-result-success-p result))
                           (search "UTF-8 bytes" (tool-result-content result)))
                      "resource.edit preflights the complete UTF-8 replacement size")
                     (test-assert
                      (and (not publish-called-p)
                           (string= (workspace-file--read-content path)
                                    (format nil "a~%"))
                           (null
                            (directory
                             (merge-pathnames
                              ".*.autolith-resource-*.tmp" workspace))))
                      "oversized replacement rejection writes nothing and preserves the original")))))
             (let ((path (merge-pathnames "publish-failure.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:publish-failure.txt")
                 (declare (ignore read-result))
                 (let ((*workspace-file-resource-publish-function*
                         (lambda (source target)
                           (declare (ignore source target))
                           (error "simulated publish failure"))))
                   (test-assert
                    (not (tool-result-success-p
                          (edit-resource
                           first-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-lines"
                             "start-line" 1
                             "end-line" 1
                             "content" "after")))))
                    "resource.edit reports a failed atomic publish"))
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "before~%"))
                  "failed atomic publication leaves original content intact")
                 (test-assert
                  (null (directory (merge-pathnames ".*.autolith-resource-*.tmp"
                                                    workspace)))
                  "failed atomic publication cleans its same-directory temporary file")))
             (let ((path (merge-pathnames "publish-mismatch.txt" workspace)))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:publish-mismatch.txt")
                 (declare (ignore read-result))
                 (let ((*workspace-file-resource-publish-function*
                         (lambda (source target)
                           (declare (ignore source target))
                           nil)))
                   (let ((result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "replace-lines"
                              "start-line" 1
                              "end-line" 1
                              "content" "after")))))
                     (test-assert
                      (and (not (tool-result-success-p result))
                           (search "exact requested content"
                                   (tool-result-content result)))
                      "resource.edit rejects a publish that leaves stale content")))
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "before~%"))
                  "publication mismatch preserves the original content")
                 (test-assert
                  (null (directory (merge-pathnames ".*.autolith-resource-*.tmp"
                                                    workspace)))
                  "publication mismatch cleans its same-directory temporary file")))
             (let ((path (merge-pathnames "extensionless" workspace)))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:extensionless")
                 (declare (ignore read-result))
                 (let ((result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-lines"
                            "start-line" 1
                            "end-line" 1
                            "content" "after")))))
                   (test-assert
                    (tool-result-success-p result)
                    "resource.edit publishes an extensionless target"))
                 (test-assert
                  (string= (workspace-file--read-content path)
                           (format nil "after~%"))
                  "extensionless publication replaces the exact target")
                 (test-assert
                  (null (probe-file (merge-pathnames "extensionless.tmp" workspace)))
                  "extensionless publication does not default the target type")
                 (test-assert
                  (null (directory (merge-pathnames ".*.autolith-resource-*.tmp"
                                                    workspace)))
                  "extensionless publication leaves no temporary file")))
             (let* ((path (merge-pathnames "serialized.txt" workspace))
                    (gate (make-lock "workspace resource serialization test"))
                    (condition (make-condition-variable))
                    (publish-entered-p nil)
                    (release-publish-p nil)
                    (first-result nil)
                    (second-result nil)
                    (first-thread nil)
                    (second-thread nil)
                    (original-publish *workspace-file-resource-publish-function*))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (first-read first-uri first-revision)
                   (read-resource first-context "workspace:serialized.txt")
                 (declare (ignore first-read))
                 (multiple-value-bind (second-read second-uri second-revision)
                     (read-resource second-context "workspace:serialized.txt")
                   (declare (ignore second-read))
                   (unwind-protect
                        (progn
                          (setf *workspace-file-resource-publish-function*
                                (lambda (source target)
                                  (with-lock-held (gate)
                                    (setf publish-entered-p t)
                                    (condition-notify condition)
                                    (loop until release-publish-p
                                          do (condition-wait condition gate)))
                                  (funcall original-publish source target))
                                first-thread
                                (make-thread
                                 (lambda ()
                                   (setf first-result
                                         (edit-resource
                                          first-context first-uri first-revision
                                          (list
                                           (workspace-resource-tests--operation
                                            "replace-lines"
                                            "start-line" 1
                                            "end-line" 1
                                            "content" "resource")))))
                                 :name "first serialized resource edit"))
                          (with-lock-held (gate)
                            (loop until publish-entered-p
                                  unless (condition-wait condition gate :timeout 2)
                                    do (error "Timed out waiting for resource publication.")))
                          (setf second-thread
                                (make-thread
                                 (lambda ()
                                   (setf second-result
                                         (edit-resource
                                          second-context second-uri second-revision
                                          (list
                                           (workspace-resource-tests--operation
                                            "replace-lines"
                                            "start-line" 1
                                            "end-line" 1
                                            "content" "second")))))
                                 :name "second serialized resource edit"))
                          (sleep 0.05)
                          (test-assert
                           (null second-result)
                           "resource edits wait for revision-gated publication")
                          (with-lock-held (gate)
                            (setf release-publish-p t)
                            (condition-notify condition))
                          (join-thread first-thread)
                          (join-thread second-thread)
                          (test-assert
                           (and (tool-result-success-p first-result)
                                (not (tool-result-success-p second-result))
                                (search "stale"
                                        (tool-result-content second-result)
                                        :test #'char-equal)
                                (string= (workspace-file--read-content path)
                                         (format nil "resource~%")))
                           "serialized resource edits reject a stale concurrent replacement"))
                     (setf *workspace-file-resource-publish-function* original-publish)
                     (with-lock-held (gate)
                       (setf release-publish-p t)
                       (condition-notify condition))
                     (when (and first-thread (thread-alive-p first-thread))
                       (join-thread first-thread))
                     (when (and second-thread (thread-alive-p second-thread))
                       (join-thread second-thread))))))
             (let* ((path (merge-pathnames "serialized-read.txt" workspace))
                    (coordination
                      (make-instance 'workspace-resource-tests-coordination))
                    (resource-registry
                      (tool-registry-resource-registry registry))
                    (previous-resolver
                      (gethash "workspace"
                               (resource-registry-resolvers resource-registry)))
                    (read-result nil)
                    (edit-result nil)
                    (edit-started-p nil)
                    (read-thread nil)
                    (edit-thread nil))
               (workspace-resource-tests--write-text path (format nil "before~%"))
               (multiple-value-bind (edit-read edit-uri edit-revision)
                   (read-resource second-context "workspace:serialized-read.txt")
                 (declare (ignore edit-read))
                 (unwind-protect
                      (progn
                        (resource-registry-register
                         resource-registry
                         (make-instance
                          'workspace-resource-tests-coordinated-resolver
                          :scheme       "workspace"
                          :coordination coordination))
                        (with-recursive-lock-held
                            ((conversation-resource-observation-lock
                              first-conversation))
                          (setf read-thread
                                (make-thread
                                 (lambda ()
                                   (setf read-result
                                         (read-resource
                                          first-context
                                          "workspace:serialized-read.txt")))
                                 :name "serialized resource read"))
                          (with-lock-held
                              ((workspace-resource-tests-coordination-lock
                                coordination))
                            (loop until
                                  (workspace-resource-tests-coordination-observed-p
                                   coordination)
                                  unless
                                  (condition-wait
                                   (workspace-resource-tests-coordination-condition
                                    coordination)
                                   (workspace-resource-tests-coordination-lock
                                    coordination)
                                   :timeout 2)
                                    do (error
                                        "Timed out waiting for resource observation.")))
                          (setf edit-thread
                                (make-thread
                                 (lambda ()
                                   (with-lock-held
                                       ((workspace-resource-tests-coordination-lock
                                         coordination))
                                     (setf edit-started-p t)
                                     (condition-notify
                                      (workspace-resource-tests-coordination-condition
                                       coordination)))
                                   (setf edit-result
                                         (edit-resource
                                          second-context edit-uri edit-revision
                                          (list
                                           (workspace-resource-tests--operation
                                            "replace-lines"
                                            "start-line" 1
                                            "end-line" 1
                                            "content" "edited")))))
                                 :name "serialized read resource edit"))
                          (with-lock-held
                              ((workspace-resource-tests-coordination-lock
                                coordination))
                            (loop until edit-started-p
                                  unless
                                  (condition-wait
                                   (workspace-resource-tests-coordination-condition
                                    coordination)
                                   (workspace-resource-tests-coordination-lock
                                    coordination)
                                   :timeout 2)
                                    do (error
                                        "Timed out waiting for concurrent resource.edit.")))
                          (sleep 0.05)
                          (test-assert
                           (null edit-result)
                           "resource.edit waits while resource.read installs observation state"))
                        (join-thread read-thread)
                        (join-thread edit-thread)
                        (let ((revision
                                (workspace-resource-tests--field
                                 (tool-result-content read-result) "Revision: ")))
                          (test-assert
                           (and (tool-result-success-p read-result)
                                (tool-result-success-p edit-result)
                                (with-recursive-lock-held
                                    ((conversation-resource-observation-lock
                                      first-conversation))
                                  (workspace-file--find-observation-state
                                   first-conversation
                                   "workspace:serialized-read.txt"
                                   revision))
                                (string= (workspace-file--read-content path)
                                         (format nil "edited~%")))
                           "resource.read installs its exact observation before resource.edit proceeds")))
                   (resource-registry-register resource-registry previous-resolver)
                   (when (and read-thread (thread-alive-p read-thread))
                     (join-thread read-thread))
                   (when (and edit-thread (thread-alive-p edit-thread))
                    (join-thread edit-thread)))))
             (let* ((schemas (tool-registry-provider-schemas registry))
                    (resource-namespace
                      (find "resource" schemas
                            :key (lambda (schema) (json-get schema "name"))
                            :test #'string=))
                    (tools (and resource-namespace
                                (json-get resource-namespace "tools")))
                    (read-schema
                      (and tools
                           (find "read" tools
                                 :key (lambda (schema) (json-get schema "name"))
                                 :test #'string=)))
                    (edit-schema
                      (and tools
                           (find "edit" tools
                                 :key (lambda (schema) (json-get schema "name"))
                                 :test #'string=))))
               (test-assert (and resource-namespace (= (length tools) 2))
                            "provider schemas expose the resource namespace and both tools")
               (test-assert
                (equalp (json-get (json-get read-schema "parameters") "required")
                        #( "uri"))
                "resource.read schema requires the URI")
               (test-assert
                (equalp (json-get (json-get edit-schema "parameters") "required")
                        #( "uri" "base-revision" "operations"))
                "resource.edit schema requires URI, base revision, and operations")
               (let* ((properties
                        (json-get (json-get edit-schema "parameters")
                                  "properties"))
                      (operation-items
                        (json-get (json-get properties "operations") "items"))
                      (variants (json-get operation-items "oneOf")))
                 (test-assert
                  (let ((operations
                          (map 'list
                               (lambda (variant)
                                 (let ((op (json-get
                                            (json-get variant "properties")
                                            "op")))
                                   (and op (aref (json-get op "enum") 0))))
                               variants)))
                    (every (lambda (operation)
                             (member operation operations :test #'equal))
                           '("replace-lines" "scratchpad-delete" "agenda-add"
                             "memory-remember" "papercut-report")))
                  "resource.edit exposes workspace, scratchpad, agenda, memory, and papercut operations")
                  (test-assert
                   (every (lambda (variant)
                            (and (json-get variant "required")
                                 (eq (json-get variant "additionalProperties") false)))
                          variants)
                   "every resource edit operation schema requires its fields and rejects extras")
                  (let* ((replace-variant
                           (find "replace-lines" variants
                                 :key (lambda (variant)
                                        (let ((op
                                                (json-get
                                                 (json-get variant "properties")
                                                 "op")))
                                          (and op (aref (json-get op "enum") 0))))
                                 :test #'string=))
                         (replace-properties
                           (json-get replace-variant "properties")))
                    (test-assert
                     (and (json-get replace-properties "start-anchor")
                          (json-get replace-properties "end-anchor"))
                     "resource.edit schema exposes optional range-boundary anchors"))))
              (let* ((directory (merge-pathnames "listing/" workspace))
                     (subdirectory (merge-pathnames "nested/" directory))
                     (file (merge-pathnames "alpha.txt" directory))
                     (extra-file (merge-pathnames "beta.txt" directory))
                     (fifo (merge-pathnames "named-pipe" directory)))
                (ensure-directories-exist (merge-pathnames "marker" subdirectory))
                (workspace-resource-tests--write-text file "abc")
                (multiple-value-bind (result uri revision)
                    (read-resource first-context "workspace:listing")
                  (let ((content (tool-result-content result)))
                    (test-assert
                     (and (tool-result-success-p result)
                          (search "Kind: directory" content)
                          (search "d           nested/" content)
                          (search "f         3  alpha.txt" content)
                          (< (search "nested/" content)
                             (search "alpha.txt" content)))
                     "resource.read returns sorted directory kinds and byte sizes"))
                  (let ((edit-result
                          (edit-resource
                           first-context uri revision
                           (list
                            (workspace-resource-tests--operation
                             "replace-empty" "content" "no")))))
                    (test-assert
                     (and (not (tool-result-success-p edit-result))
                          (eq (tool-result-error-code edit-result)
                              ':operation-unsupported))
                     "workspace directory resources are read-only through capabilities")))
                (unwind-protect
                     (progn
                       (with-test-fixture (':fifos "directory reads classifying FIFOs")
                         (test-fixture-make-fifo *platform* fifo)
                         (let ((result nil)
                               (thread nil))
                           (unwind-protect
                                (progn
                                  (setf thread
                                        (make-thread
                                         (lambda ()
                                           (setf result
                                                 (read-resource
                                                  first-context
                                                  "workspace:listing")))
                                         :name "bounded FIFO directory resource read"))
                                  (multiple-value-bind (value join-status)
                                      (sb-thread:join-thread thread
                                                             :timeout 2
                                                             :default ':timed-out)
                                    (declare (ignore value))
                                    (test-assert
                                     (and (not (eq join-status ':timeout))
                                          result
                                          (tool-result-success-p result)
                                          (search "o           named-pipe"
                                                  (tool-result-content result)))
                                     "directory reads classify FIFOs without opening or blocking")))
                             (when (and thread (thread-alive-p thread))
                               (sb-thread:terminate-thread thread))
                             (when thread
                               (ignore-errors
                                 (sb-thread:join-thread
                                  thread :timeout 0.2 :default nil))))))
                       (workspace-resource-tests--write-text extra-file "extra")
                       (let* ((*workspace-file-resource-maximum-directory-entries* 2)
                              (*workspace-file-resource-maximum-result-characters* 96)
                              (resource
                                (resource-registry-resolve
                                 (tool-registry-resource-registry registry)
                                 "workspace:listing"
                                 first-context))
                              (observation (resource-observe resource first-context))
                              (content (resource-observation-content observation)))
                         (test-assert
                          (and (<= (length content) 96)
                               (search "[directory listing truncated]" content))
                          "directory snapshots bound enumeration and retained content")))
                  (when (probe-file fifo)
                    (sb-posix:unlink (namestring fifo)))))
             (let ((path (merge-pathnames "created/nested.txt" workspace)))
               (multiple-value-bind (result uri revision)
                   (read-resource first-context "workspace:created/nested.txt")
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "Kind: missing" (tool-result-content result))
                       (null (probe-file path)))
                  "resource.read establishes a revision for a missing path")
                 (let ((edit-result
                         (edit-resource
                          first-context uri revision
                          (list
                           (workspace-resource-tests--operation
                            "replace-empty" "content" (format nil "created~%"))))))
                   (test-assert
                    (and (tool-result-success-p edit-result)
                         (search "Kind: file" (tool-result-content edit-result))
                         (string= (workspace-file--read-content path)
                                  (format nil "created~%")))
                    "replace-empty creates an observed missing file and its parents"))))
             (dolist (race
                      (append
                       (list
                        (list ':file
                              (lambda (path)
                                (workspace-resource-tests--write-text path "racer"))
                              nil)
                        (list ':directory
                              (lambda (path)
                                (ensure-directories-exist
                                 (merge-pathnames
                                  "marker"
                                  (uiop:ensure-directory-pathname path))))
                              nil))
                       (if (test-fixture-available-p *platform* ':fifos)
                           (list
                            (list ':other
                                  (lambda (path)
                                    (test-fixture-make-fifo *platform* path))
                                  (lambda (path)
                                    (sb-posix:unlink (namestring path)))))
                           (test-withheld ':fifos "a missing resource racing with a FIFO"))))
               (destructuring-bind (kind creator cleanup) race
                 (let* ((name (format nil "missing-race-~(~A~)" kind))
                        (path (merge-pathnames name workspace))
                        (uri (format nil "workspace:~A" name)))
                   (multiple-value-bind (read-result canonical revision)
                       (read-resource first-context uri)
                     (declare (ignore read-result))
                     (unwind-protect
                          (progn
                            (funcall creator path)
                            (let ((result
                                    (edit-resource
                                     first-context canonical revision
                                     (list
                                      (workspace-resource-tests--operation
                                       "replace-empty" "content" "agent")))))
                              (test-assert
                               (not (tool-result-success-p result))
                               (format nil
                                       "resource.edit rejects a missing-to-~(~A~) race"
                                       kind))))
                       (when cleanup
                         (funcall cleanup path)))))))
             (let* ((path (merge-pathnames "create-race.txt" workspace))
                    (original-create *workspace-file-resource-create-function*))
               (multiple-value-bind (read-result uri revision)
                   (read-resource first-context "workspace:create-race.txt")
                 (declare (ignore read-result))
                 (let ((*workspace-file-resource-create-function*
                         (lambda (source target)
                           (workspace-resource-tests--write-text target "racer")
                           (funcall original-create source target))))
                   (let ((result
                           (edit-resource
                            first-context uri revision
                            (list
                             (workspace-resource-tests--operation
                              "replace-empty" "content" "agent")))))
                     (test-assert
                      (and (not (tool-result-success-p result))
                           (string= (workspace-file--read-content path) "racer"))
                      "missing resource publication never overwrites a post-check race")))))
             (test-assert
              (= (length
                  (remove-duplicates
                   (mapcar (lambda (kind)
                             (workspace-file--snapshot-revision kind ""))
                           '(:file :directory :missing))
                   :test #'string=))
                 3)
              "workspace revisions distinguish missing, empty-file, and directory states")
             (test-assert
              (and (null (tool-registry-find registry "fs" "list"))
                   (null (tool-registry-find registry "fs" "write")))
              "redundant fs.list and fs.write tools are absent"))))
      (tool-registry-close-runtime-state registry)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)
