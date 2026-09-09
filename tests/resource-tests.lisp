(in-package #:autolith)

;;;; -- Resource Protocol Fixtures --

(defclass test-resource (resource)
  ((identifier
    :initarg :identifier
    :reader test-resource-identifier
    :type non-empty-string
    :documentation "The identifier resolved by a test resolver.")
   (marker
    :initarg :marker
    :reader test-resource-marker
    :type keyword
    :documentation "The resolver marker retained by this fixture."))
  (:documentation "A resource used to test registry dispatch."))

(defclass test-resource-resolver (resource-resolver)
  ((marker
    :initarg :marker
    :reader test-resource-resolver-marker
    :type keyword
    :documentation "The marker copied into resolved test resources.")
   (last-context
    :initform nil
    :accessor test-resource-resolver-last-context
    :type t
    :documentation "The exact authority context received by the last resolution."))
  (:documentation "A resolver recording explicit context for protocol tests."))

(defmethod resource-resolver-resolve
    ((resolver test-resource-resolver) identifier context)
  "Resolve IDENTIFIER while retaining the exact test authority CONTEXT."
  (setf (test-resource-resolver-last-context resolver) context)
  (make-instance 'test-resource
                 :uri        (format nil "~A:~A"
                                     (resource-resolver-scheme resolver)
                                     identifier)
                 :identifier identifier
                 :marker     (test-resource-resolver-marker resolver)))

(defmethod resource-capabilities ((resource test-resource) context)
  "Permit fixture reads and edits under any test CONTEXT."
  (declare (ignore resource context))
  '(:read :edit))

(defmethod resource-tool-read
    ((resource test-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return fixture content through generic model-facing resource dispatch."
  (declare (ignore tool))
  (tool-success
   (format nil "read ~A at ~A in ~A"
           (test-resource-identifier resource)
           (tool-argument arguments "start-line")
           (conversation-identifier (tool-context-conversation context)))))

(defmethod resource-tool-edit
    ((resource test-resource) (tool resource-edit-tool)
     (context tool-context) (arguments hash-table))
  "Return fixture edit arguments through generic model-facing resource dispatch."
  (declare (ignore tool context))
  (tool-success
   (format nil "edited ~A from ~A with ~D operations"
           (test-resource-identifier resource)
           (tool-argument arguments "base-revision")
           (length (tool-argument arguments "operations")))))


;;;; -- Resource Protocol Tests --

(-> test-resource-protocol () null)
(defun test-resource-protocol ()
  "Exercise resource observations, URI resolution, and default failures."
  (let* ((metadata    (list ':media-type "text/plain"))
         (observation (make-instance 'resource-observation
                                     :uri      "test:item"
                                     :revision "revision-1"
                                     :content  "value"
                                     :metadata metadata)))
    (test-assert (string= (resource-observation-uri observation) "test:item")
                 "resource observations retain URI identity")
    (test-assert (string= (resource-observation-revision observation) "revision-1")
                 "resource observations retain opaque revisions")
    (test-assert (string= (resource-observation-content observation) "value")
                 "resource observations retain resource-specific content")
    (test-assert (equal (resource-observation-metadata observation) metadata)
                 "resource observations retain metadata separately from content"))
  (let* ((registry       (make-resource-registry))
         (first-resolver (make-instance 'test-resource-resolver
                                        :scheme "test"
                                        :marker ':first))
         (next-resolver  (make-instance 'test-resource-resolver
                                        :scheme "test"
                                        :marker ':next))
         (context        (list ':authority ':fixture)))
    (multiple-value-bind (registered previous)
        (resource-registry-register registry first-resolver)
      (test-assert (eq registered first-resolver)
                   "resource registration returns the installed resolver")
      (test-assert (null previous)
                   "initial resource registration has no replaced resolver"))
    (let ((resource (resource-registry-resolve registry "test:alpha" context)))
      (test-assert (typep resource 'test-resource)
                   "registered resource resolvers construct resources")
      (test-assert (string= (resource-uri resource) "test:alpha")
                   "resource resolution preserves the complete URI")
      (test-assert (string= (test-resource-identifier resource) "alpha")
                   "resource resolution passes only the identifier to the resolver")
      (test-assert (eq (test-resource-marker resource) ':first)
                   "resource resolution dispatches to the registered resolver")
      (test-assert (eq (test-resource-resolver-last-context first-resolver) context)
                   "resource resolution passes explicit authority context unchanged")
      (test-assert (equal (resource-capabilities resource context)
                          '(:read :edit))
                   "concrete resources advertise model-facing capabilities")
      (test-assert
       (handler-case
           (progn
             (resource-observe resource context)
             nil)
         (resource-operation-unsupported (condition)
           (and (eq (resource-operation-unsupported-operation condition) ':observe)
                (string= (resource-operation-unsupported-uri condition)
                         "test:alpha"))))
       "abstract resources reject unsupported observation structurally")
      (test-assert
       (handler-case
           (progn
             (resource-apply-operations resource context
                                        :base-revision "revision-1"
                                        :operations    nil)
             nil)
         (resource-operation-unsupported (condition)
           (and (eq (resource-operation-unsupported-operation condition)
                    ':apply-operations)
                (string= (resource-operation-unsupported-uri condition)
                         "test:alpha"))))
       "abstract resources reject unsupported mutation structurally"))
    (multiple-value-bind (registered previous)
        (resource-registry-register registry next-resolver)
      (test-assert (eq registered next-resolver)
                   "replacement registration returns the new resolver")
      (test-assert (eq previous first-resolver)
                   "replacement registration returns the displaced resolver"))
    (let ((resource (resource-registry-resolve registry "test:beta" context)))
      (test-assert (eq (test-resource-marker resource) ':next)
                   "duplicate schemes replace their previous resolver")))
  (dolist (uri (list nil
                     ""
                     "missing-separator"
                     ":identifier"
                     "scheme:"
                     "Upper:identifier"
                     "bad_scheme:identifier"
                     "scheme:has space"))
    (test-assert
     (handler-case
         (progn
           (resource-uri-parse uri)
           nil)
       (resource-uri-malformed (condition)
         (eq (resource-uri-malformed-uri condition) uri)))
     (format nil "resource URI parser rejects malformed value ~S" uri)))
  (dolist (constructor
           (list (lambda ()
                   (make-instance 'resource :uri "missing-separator"))
                 (lambda ()
                   (make-instance 'resource-observation
                                  :uri      "missing-separator"
                                  :revision "revision-1"
                                  :content  nil))))
    (test-assert
     (handler-case
         (progn
           (funcall constructor)
           nil)
       (resource-uri-malformed ()
         t))
     "resource construction enforces strict URI identity"))
  (dolist (scheme '("test:other" "Upper" "bad scheme"))
    (test-assert
     (handler-case
         (progn
           (make-instance 'resource-resolver :scheme scheme)
           nil)
       (resource-uri-malformed ()
         t))
     (format nil "resource resolvers reject malformed scheme ~S" scheme)))
  (multiple-value-bind (scheme identifier)
      (resource-uri-parse "workspace:src/resource/protocol.lisp")
    (test-assert (string= scheme "workspace")
                 "resource URI parsing returns the strict scheme")
    (test-assert (string= identifier "src/resource/protocol.lisp")
                 "resource URI parsing preserves the complete identifier"))
  (let ((registry (make-resource-registry)))
    (test-assert
     (handler-case
         (progn
           (resource-registry-resolve registry "unknown:item" ':context)
           nil)
       (resource-scheme-unknown (condition)
         (and (string= (resource-scheme-unknown-uri condition) "unknown:item")
              (string= (resource-scheme-unknown-scheme condition) "unknown"))))
     "unknown resource schemes signal a structured condition")
    (resource-registry-register
     registry (make-instance 'resource-resolver :scheme "plain"))
    (test-assert
     (handler-case
         (progn
           (resource-registry-resolve registry "plain:item" ':context)
           nil)
       (resource-operation-unsupported (condition)
         (and (eq (resource-operation-unsupported-operation condition) ':resolve)
              (string= (resource-operation-unsupported-uri condition)
                       "plain:item"))))
     "abstract resource resolvers reject resolution structurally"))
  (let* ((first-tools  (make-instance 'tool-registry))
         (second-tools (make-instance 'tool-registry)))
    (test-assert
     (typep (tool-registry-resource-registry first-tools) 'resource-registry)
     "tool registries own a resource registry")
    (test-assert
     (not (eq (tool-registry-resource-registry first-tools)
              (tool-registry-resource-registry second-tools)))
     "resource resolver registries remain isolated per agent tool registry"))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (make-default-tool-registry))
         (resolver (make-instance 'test-resource-resolver
                                  :scheme "test"
                                  :marker ':tool-dispatch))
         (conversation
           (conversation-create configuration :identifier "resource-dispatch"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation)))
    (unwind-protect
         (progn
           (resource-registry-register
            (tool-registry-resource-registry registry) resolver)
           (let ((result
                   (tool-registry-execute-call
                    registry
                    (json-object
                     "namespace" "resource"
                     "name" "read"
                     "arguments"
                     (json-encode
                      (json-object "uri" "test:alpha" "start-line" 7)))
                    context)))
             (test-assert
              (and (tool-result-success-p result)
                   (string= (tool-result-content result)
                            "read alpha at 7 in resource-dispatch"))
              "resource.read dispatches registered schemes through resource methods"))
           (let ((result
                   (tool-registry-execute-call
                    registry
                    (json-object
                     "namespace" "resource"
                     "name" "edit"
                     "arguments"
                     (json-encode
                      (json-object
                       "uri" "test:alpha"
                       "base-revision" "fixture-revision"
                       "operations" (vector (json-object "op" "fixture")))))
                    context)))
             (test-assert
              (and (tool-result-success-p result)
                   (string= (tool-result-content result)
                            "edited alpha from fixture-revision with 1 operations"))
              "resource.edit dispatches registered schemes through resource methods"))
           (test-assert (eq (test-resource-resolver-last-context resolver) context)
                        "resource tool dispatch preserves the exact authority context"))
      (tool-registry-close-runtime-state registry)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  (let ((condition (make-condition 'resource-revision-stale
                                   :uri               "test:item"
                                   :expected-revision "revision-1"
                                   :actual-revision   "revision-2")))
    (test-assert (string= (resource-revision-stale-uri condition) "test:item")
                 "stale revision conditions retain resource identity")
    (test-assert
     (string= (resource-revision-stale-expected-revision condition) "revision-1")
     "stale revision conditions retain the caller revision")
    (test-assert
     (string= (resource-revision-stale-actual-revision condition) "revision-2")
     "stale revision conditions retain the current revision"))
  nil)

(-> test-resource-edit-operation-schema () null)
(defun test-resource-edit-operation-schema ()
  "Pin the resource.edit operation schema against provider validator limits.

Bare {\"required\": [...]} anyOf variants carry no type declaration, and the
Fireworks JSON Schema validator rejects them with \"could not understand the
instance\", failing the entire request before the model runs. Every anyOf
variant must therefore declare its object type explicitly."
  (let* ((schema (default-tools--resource-operation-schema))
         (variants (json-get schema "oneOf"))
         (anyof-count 0))
    (test-assert (and (vectorp variants) (plusp (length variants)))
                 "resource.edit operations offer one closed variant per operation")
    (loop for variant across variants
          for any-of = (and (json-object-p variant) (json-get variant "anyOf"))
          when any-of
          do (loop for entry across any-of
                   do (incf anyof-count)
                      (test-assert
                       (string= (or (json-get entry "type") "") "object")
                       "anyOf variants declare their object type for provider validators")
                      (test-assert
                       (and (vectorp (json-get entry "required"))
                            (plusp (length (json-get entry "required"))))
                       "anyOf variants retain their required-field constraint")))
    (test-assert (= anyof-count 3)
                 "the agenda-update operation requires one of text, status, or memory-ids"))
  nil)
