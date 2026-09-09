(in-package #:autolith)

;;;; -- Resource Conditions --

(define-condition resource-uri-malformed (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-uri-malformed-uri
    :type t
    :documentation "The value rejected as a resource URI.")
   (reason
    :initarg :reason
    :reader resource-uri-malformed-reason
    :type non-empty-string
    :documentation "The specific URI grammar violation."))
  (:default-initargs :message "The resource URI is malformed.")
  (:documentation "A resource URI does not satisfy the simple scheme:identifier grammar.")
  (:report (lambda (condition stream)
             (format stream "Malformed resource URI ~S: ~A"
                     (resource-uri-malformed-uri condition)
                     (resource-uri-malformed-reason condition)))))

(define-condition resource-scheme-unknown (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-scheme-unknown-uri
    :type non-empty-string
    :documentation "The complete URI whose scheme has no registered resolver.")
   (scheme
    :initarg :scheme
    :reader resource-scheme-unknown-scheme
    :type non-empty-string
    :documentation "The syntactically valid unregistered scheme."))
  (:default-initargs :message "The resource URI scheme is not registered.")
  (:documentation "No resolver is registered for a syntactically valid resource URI.")
  (:report (lambda (condition stream)
             (format stream "No resource resolver is registered for scheme ~S in ~S."
                     (resource-scheme-unknown-scheme condition)
                     (resource-scheme-unknown-uri condition)))))

(define-condition resource-access-denied (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-access-denied-uri
    :type non-empty-string
    :documentation "The URI rejected under the caller's authority context."))
  (:default-initargs :message "The resource is unavailable under this authority context.")
  (:documentation "A resolver policy denies a resource under the caller's authority context.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S is unavailable under this authority context."
                     (resource-access-denied-uri condition)))))

(defmethod tool-failure-code ((condition resource-access-denied))
  "Discriminate authority denials without pinning their prose."
  ':access-denied)

(define-condition resource-operation-unsupported (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-operation-unsupported-uri
    :type non-empty-string
    :documentation "The URI of the resource that rejected the operation.")
   (operation
    :initarg :operation
    :reader resource-operation-unsupported-operation
    :type keyword
    :documentation "The unsupported resource protocol operation."))
  (:default-initargs :message "The resource does not support this operation.")
  (:documentation "A resource does not implement a requested protocol operation.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S does not support ~S."
                     (resource-operation-unsupported-uri condition)
                     (resource-operation-unsupported-operation condition)))))

(defmethod tool-failure-code ((condition resource-operation-unsupported))
  "Discriminate unsupported resource operations without pinning their prose."
  ':operation-unsupported)

(define-condition resource-revision-stale (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-revision-stale-uri
    :type non-empty-string
    :documentation "The URI whose observed revision is stale.")
   (expected-revision
    :initarg :expected-revision
    :reader resource-revision-stale-expected-revision
    :type non-empty-string
    :documentation "The revision supplied by the operation caller.")
   (actual-revision
    :initarg :actual-revision
    :reader resource-revision-stale-actual-revision
    :type (option non-empty-string)
    :documentation "The current revision, or NIL when the resource no longer exists."))
  (:default-initargs :message "The observed resource revision is stale.")
  (:documentation "A revision-gated resource operation no longer matches current state.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S changed from revision ~S to ~S."
                     (resource-revision-stale-uri condition)
                     (resource-revision-stale-expected-revision condition)
                     (resource-revision-stale-actual-revision condition)))))


;;;; -- Resource Protocol --

(defparameter *resource-readable-schemes* nil
  "Optional exact URI schemes permitted during one restricted tool call.")

(defclass resource ()
  ((uri
    :initarg :uri
    :reader resource-uri
    :type non-empty-string
    :documentation "The stable URI identifying this resource, not authority to access it."))
  (:documentation "An authority-neutral resource resolved for one explicit context."))

(defclass resource-observation ()
  ((uri
    :initarg :uri
    :reader resource-observation-uri
    :type non-empty-string
    :documentation "The stable URI identifying the observed resource.")
   (revision
    :initarg :revision
    :reader resource-observation-revision
    :type non-empty-string
    :documentation "The opaque revision identifying this observed state.")
   (content
    :initarg :content
    :reader resource-observation-content
    :type t
    :documentation "The resource-specific content visible in this observation.")
   (metadata
    :initarg :metadata
    :initform nil
    :reader resource-observation-metadata
    :type list
    :documentation "Resource-specific metadata associated with the observation."))
  (:documentation "A revisioned observation whose identity is separate from its content."))

(defclass resource-observation-state ()
  ((alias
    :initarg :alias
    :reader resource-observation-state-alias
    :type non-empty-string
    :documentation "The short opaque revision alias visible to the model.")
   (observation
    :initarg :observation
    :reader resource-observation-state-observation
    :type resource-observation
    :documentation "The complete internal observation represented by the alias."))
  (:documentation "Conversation-local state for one model-visible resource observation."))

(defmethod resource-observation-state-weight
    (alias (state resource-observation-state))
  "Charge no retained workspace bytes for a general resource observation."
  (declare (ignore alias state))
  0)

(-> resource-observation-state-new-alias (fifo-cache) non-empty-string)
(defun resource-observation-state-new-alias (states)
  "Return a fresh opaque alias not present in resource observation STATES."
  (loop for candidate = (format nil "R~A"
                                (subseq (daemon-random-token) 0 16))
        unless (nth-value 1 (fifo-cache-get states candidate))
          return candidate))

(-> resource-observation-state-find
    (fifo-cache non-empty-string t)
    (option resource-observation-state))
(defun resource-observation-state-find (states alias class)
  "Return ALIAS from STATES only when it is an instance of CLASS."
  (multiple-value-bind (state present-p)
      (fifo-cache-get states alias)
    (and present-p (typep state class) state)))

(-> resource-observation-state-family-and-key
    (resource-observation)
    (values symbol list))
(defgeneric resource-observation-state-family-and-key (observation)
  (:documentation
   "Return OBSERVATION's state class and exact retention-equivalence key."))

(-> resource-observation-state-merge
    (resource-observation-state resource-observation &rest t)
    resource-observation-state)
(defgeneric resource-observation-state-merge (state observation &rest initargs)
  (:documentation "Merge family INITARGS into equivalent retained STATE.")
  (:method ((state resource-observation-state)
            (observation resource-observation) &rest initargs)
    "Return an equivalent general STATE without additional retained metadata."
    (declare (ignore observation initargs))
    state))

(-> resource-observation-state-maximum
    (resource-observation-state)
    (integer 0))
(defgeneric resource-observation-state-maximum (state)
  (:documentation
   "Return the maximum conversation-local observations retained for STATE's family."))

(-> resource-observation-state-trim-storage
    (conversation resource-observation-state)
    null)
(defgeneric resource-observation-state-trim-storage (conversation state)
  (:documentation "Apply family storage limits after retaining STATE in CONVERSATION.")
  (:method ((conversation conversation) (state resource-observation-state))
    "Apply no additional storage limit to a general observation STATE."
    (declare (ignore conversation state))
    nil))

(-> resource-observation-state-ensure
    (conversation resource-observation &rest t)
    resource-observation-state)
(defun resource-observation-state-ensure (conversation observation &rest initargs)
  "Return or retain CONVERSATION's exact OBSERVATION with family INITARGS."
  (with-recursive-lock-held
      ((conversation-resource-observation-lock conversation))
    (multiple-value-bind (family key)
        (resource-observation-state-family-and-key observation)
      (let* ((states
               (conversation-resource-observations conversation))
             (matching
               (nth-value
                1
                (fifo-cache-find-if
                 (lambda (alias state)
                   (declare (ignore alias))
                   (and (typep state family)
                        (equal key
                               (nth-value
                                1
                                (resource-observation-state-family-and-key
                                 (resource-observation-state-observation state))))))
                 states))))
        (when matching
          (return-from resource-observation-state-ensure
            (apply #'resource-observation-state-merge
                   matching observation initargs)))
        (let* ((alias   (resource-observation-state-new-alias states))
               (state   (apply #'make-instance family
                               :alias alias :observation observation initargs))
               (family-state-p
                 (lambda (candidate-alias candidate)
                   (declare (ignore candidate-alias))
                   (typep candidate family)))
               (maximum (resource-observation-state-maximum state)))
          (fifo-cache-put states alias state)
          (loop while (> (fifo-cache-count-if family-state-p states) maximum)
                do (fifo-cache-delete-first-if family-state-p states))
          (resource-observation-state-trim-storage conversation state)
          state)))))

(-> resource-snapshot-digest
    ((simple-array (unsigned-byte 8) (*)) string)
    non-empty-string)
(defun resource-snapshot-digest (key content)
  "Return a keyed full SipHash digest for exact UTF-8 CONTENT."
  (let ((mac (make-mac ':siphash key :digest-length 16)))
    (update-mac mac
                (sb-ext:string-to-octets content :external-format ':utf-8))
    (with-output-to-string (stream)
      (loop for octet across (produce-mac mac)
            do (format stream "~2,'0X" octet)))))

(-> resource-readable-snapshot-digest
    ((simple-array (unsigned-byte 8) (*)) list)
    non-empty-string)
(defun resource-readable-snapshot-digest (key snapshot)
  "Return a keyed digest for the printed exact SNAPSHOT structure.

Strings print the same whatever their element type: SBCL's readable syntax
sets base strings apart, and Windows namestrings and environment values are
often base strings while the same text built elsewhere is not."
  (resource-snapshot-digest
   key
   (with-standard-io-syntax
     (let ((*print-readably* nil))
       (prin1-to-string snapshot)))))

(-> resource-capabilities (resource t) list)
(defgeneric resource-capabilities (resource context)
  (:documentation
   "Return RESOURCE operations available under explicit authority CONTEXT."))

(defmethod resource-capabilities ((resource resource) context)
  "Report no operations for the abstract RESOURCE protocol object."
  (declare (ignore resource context))
  nil)

(-> resource-observe (resource t) resource-observation)
(defgeneric resource-observe (resource context)
  (:documentation
   "Observe RESOURCE under explicit authority CONTEXT and return a revisioned value."))

(defmethod resource-observe ((resource resource) context)
  "Reject observation when RESOURCE has no concrete observation method."
  (declare (ignore context))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':observe))

(-> resource-apply-operations
    (resource t &key (:base-revision non-empty-string) (:operations list))
    resource-observation)
(defgeneric resource-apply-operations
    (resource context &key base-revision operations)
  (:documentation
   "Apply OPERATIONS to RESOURCE at BASE-REVISION under explicit authority CONTEXT."))

(defmethod resource-apply-operations
    ((resource resource) context &key base-revision operations)
  "Reject mutation when RESOURCE has no concrete operation method."
  (declare (ignore context base-revision operations))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':apply-operations))


;;;; -- URI Resolution --

(-> resource-uri--scheme-character-p (character boolean) boolean)
(defun resource-uri--scheme-character-p (character initial-p)
  "Return true when CHARACTER is valid at this position in a resource scheme."
  (and (or (and (char>= character #\a)
                (char<= character #\z))
           (and (not initial-p)
                (or (digit-char-p character)
                    (find character "+.-" :test #'char=))))
       t))

(-> resource-uri--valid-scheme-p (string) boolean)
(defun resource-uri--valid-scheme-p (scheme)
  "Return true when SCHEME satisfies the strict resource scheme grammar."
  (and (plusp (length scheme))
       (loop for character across scheme
             for initial-p = t then nil
             always (resource-uri--scheme-character-p character initial-p))))

(-> resource-uri--identifier-character-p (character) boolean)
(defun resource-uri--identifier-character-p (character)
  "Return true when CHARACTER is allowed in a simple resource identifier."
  (and (graphic-char-p character)
       (not (find character '(#\Space #\Tab #\Newline #\Return #\Page)))))

(-> resource-uri-parse (t) (values non-empty-string non-empty-string))
(defun resource-uri-parse (uri)
  "Parse strict lowercase SCHEME:IDENTIFIER URI and return both components."
  (unless (stringp uri)
    (error 'resource-uri-malformed
           :uri    uri
           :reason "the URI must be a string"))
  (let ((separator (position #\: uri)))
    (unless separator
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the URI must contain a scheme separator"))
    (when (zerop separator)
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the scheme must not be empty"))
    (when (= separator (1- (length uri)))
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the identifier must not be empty"))
    (let ((scheme     (subseq uri 0 separator))
          (identifier (subseq uri (1+ separator))))
      (unless (resource-uri--valid-scheme-p scheme)
        (error 'resource-uri-malformed
               :uri    uri
               :reason "the scheme must use lowercase URI scheme characters"))
      (unless (every #'resource-uri--identifier-character-p identifier)
        (error 'resource-uri-malformed
               :uri    uri
               :reason "the identifier must contain only graphic non-space characters"))
      (values scheme identifier))))

(defmethod initialize-instance :after ((resource resource) &key)
  "Validate RESOURCE URI identity after initialization."
  (resource-uri-parse (resource-uri resource)))

(defmethod initialize-instance :after ((observation resource-observation) &key)
  "Validate OBSERVATION URI identity after initialization."
  (resource-uri-parse (resource-observation-uri observation)))

(defclass resource-resolver ()
  ((scheme
    :initarg :scheme
    :reader resource-resolver-scheme
    :type non-empty-string
    :documentation "The strict lowercase URI scheme handled by this resolver."))
  (:documentation "A CLOS resolver for one resource URI scheme."))

(defmethod initialize-instance :after ((resolver resource-resolver) &key)
  "Validate RESOLVER's scheme after initialization."
  (unless (resource-uri--valid-scheme-p (resource-resolver-scheme resolver))
    (error 'resource-uri-malformed
           :uri    (resource-resolver-scheme resolver)
           :reason "the resolver scheme must use lowercase URI scheme characters")))

(-> resource-context-child-agent-p (t) boolean)
(defgeneric resource-context-child-agent-p (context)
  (:documentation "Return true when CONTEXT belongs to a restricted task child agent."))

(defmethod resource-context-child-agent-p (context)
  "Treat unknown authority contexts as primary contexts."
  (declare (ignore context))
  nil)

(-> resource-resolver-child-safe-p (resource-resolver t) boolean)
(defgeneric resource-resolver-child-safe-p (resolver context)
  (:documentation
   "Return true when RESOLVER may resolve resources for child-agent CONTEXT."))

(defmethod resource-resolver-child-safe-p
    ((resolver resource-resolver) context)
  "Default resource resolvers closed for task child agents."
  (declare (ignore resolver context))
  nil)

(-> resource-resolver-resolve (resource-resolver non-empty-string t) resource)
(defgeneric resource-resolver-resolve (resolver identifier context)
  (:documentation
   "Resolve IDENTIFIER with RESOLVER under explicit authority CONTEXT."))

(defmethod resource-resolver-resolve
    ((resolver resource-resolver) identifier context)
  "Reject resolution when RESOLVER has no concrete method."
  (declare (ignore context))
  (error 'resource-operation-unsupported
         :uri       (format nil "~A:~A"
                            (resource-resolver-scheme resolver)
                            identifier)
         :operation ':resolve))

(defclass resource-registry ()
  ((resolvers
    :initform (make-hash-table :test #'equal)
    :reader resource-registry-resolvers
    :type hash-table
    :documentation "Strict URI schemes mapped to their active resolver objects."))
  (:documentation "A per-agent registry of resource resolver objects."))

(-> make-resource-registry () resource-registry)
(defun make-resource-registry ()
  "Return an empty resource resolver registry."
  (make-instance 'resource-registry))

(-> resource-registry-register
    (resource-registry resource-resolver)
    (values resource-resolver (option resource-resolver)))
(defun resource-registry-register (registry resolver)
  "Register RESOLVER by scheme, replacing and returning any previous resolver."
  (let ((scheme (resource-resolver-scheme resolver)))
    (unless (resource-uri--valid-scheme-p scheme)
      (error 'resource-uri-malformed
             :uri    scheme
             :reason "the resolver scheme must use lowercase URI scheme characters"))
    (multiple-value-bind (previous present-p)
        (gethash scheme (resource-registry-resolvers registry))
      (setf (gethash scheme (resource-registry-resolvers registry)) resolver)
      (values resolver (and present-p previous)))))

(-> resource-registry-resolve (resource-registry t t) resource)
(defun resource-registry-resolve (registry uri context)
  "Resolve URI through REGISTRY while passing explicit authority CONTEXT."
  (multiple-value-bind (scheme identifier)
      (resource-uri-parse uri)
    (when (and *resource-readable-schemes*
               (not (member scheme *resource-readable-schemes* :test #'string=)))
      (error 'resource-access-denied :uri uri))
    (let ((resolver (gethash scheme (resource-registry-resolvers registry))))
      (unless resolver
        (error 'resource-scheme-unknown
               :uri    uri
               :scheme scheme))
      (when (and (resource-context-child-agent-p context)
                 (not (resource-resolver-child-safe-p resolver context)))
        (error 'resource-access-denied :uri uri))
      (resource-resolver-resolve resolver identifier context))))
