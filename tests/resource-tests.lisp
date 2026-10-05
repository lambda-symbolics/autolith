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
  "Exercise per-agent resolver ownership and generic library tool dispatch."
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
           (dolist (uri '("missing-separator" "unknown:item"))
             (let ((result
                     (tool-registry-execute-call
                      registry
                      (json-object "namespace" "resource" "name" "read"
                                   "arguments"
                                   (json-encode (json-object "uri" uri)))
                      context)))
               (test-assert (not (tool-result-success-p result))
                            "generic URI failures cross the tool boundary")))
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
