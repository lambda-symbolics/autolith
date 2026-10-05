(in-package #:autolith)

;;;; -- Resource Failure Integration --

(-> resource-uri-malformed-uri (resource-uri-malformed) t)
(defun resource-uri-malformed-uri (condition)
  "Return the rejected identity from a generic resource CONDITION."
  (cl-resources:resource-error-uri condition))

(-> resource-scheme-unknown-uri (resource-scheme-unknown) non-empty-string)
(defun resource-scheme-unknown-uri (condition)
  "Return the unresolved identity from a generic resource CONDITION."
  (cl-resources:resource-error-uri condition))

(-> resource-access-denied-uri (resource-access-denied) non-empty-string)
(defun resource-access-denied-uri (condition)
  "Return the denied identity from a generic resource CONDITION."
  (cl-resources:resource-error-uri condition))

(-> resource-operation-unsupported-uri
    (resource-operation-unsupported) non-empty-string)
(defun resource-operation-unsupported-uri (condition)
  "Return the unsupported identity from a generic resource CONDITION."
  (cl-resources:resource-error-uri condition))

(-> resource-revision-stale-uri (resource-revision-stale) non-empty-string)
(defun resource-revision-stale-uri (condition)
  "Return the changed identity from a generic resource CONDITION."
  (cl-resources:resource-error-uri condition))

(defmethod tool-failure-code ((condition resource-access-denied))
  "Expose generic authority denials through the tool transport."
  ':access-denied)

(defmethod tool-failure-code ((condition resource-operation-unsupported))
  "Expose generic unsupported operations through the tool transport."
  ':operation-unsupported)


;;;; -- Resource Protocol --

(defparameter *resource-readable-schemes* nil
  "Optional exact URI schemes permitted during one restricted tool call.")


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
                (utf8-string-to-octets content))
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

(-> resource-registry-resolve (resource-registry t t) resource)
(defun resource-registry-resolve (registry uri context)
  "Resolve URI after enforcing Autolith's scheme and child authority policy."
  (multiple-value-bind (scheme identifier)
      (resource-uri-parse uri)
    (declare (ignore identifier))
    (when (and *resource-readable-schemes*
               (not (member scheme *resource-readable-schemes* :test #'string=)))
      (error 'resource-access-denied :uri uri))
    (let ((resolver (cl-resources:resource-registry-find registry scheme)))
      (when (and resolver
                 (resource-context-child-agent-p context)
                 (not (resource-resolver-child-safe-p resolver context)))
        (error 'resource-access-denied :uri uri)))
    (cl-resources:resource-registry-resolve registry uri context)))
