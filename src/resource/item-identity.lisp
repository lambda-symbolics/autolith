(in-package #:autolith)

;;;; -- Identified Item Resource Machinery --

;;; Memory, papercut, and agenda resources share one shape: a URI scheme
;;; with reserved collection names and revision-gated observations of
;;; exact items. The scheme-independent machinery lives here once; each
;;; resource supplies its identity protocol methods and nouns.

(defclass single-operation-resource (resource)
  ()
  (:documentation "A resource accepting one revision-gated operation per model edit."))

(defgeneric single-operation-resource-scheme (resource)
  (:documentation "Return the URI scheme used in RESOURCE's edit diagnostics."))

(defgeneric single-operation-resource-stale-message
    (resource condition &key uri base-revision)
  (:documentation "Describe CONDITION for RESOURCE's edit at URI and BASE-REVISION."))

(-> resource-item-encode-identifier (non-empty-string) non-empty-string)
(defun resource-item-encode-identifier (identifier)
  "Return IDENTIFIER as one canonical percent-encoded URI path segment."
  (resource-uri-encode identifier #'resource-uri-unreserved-octet-p))

(-> resource-item--malformed-identifier
    (string string non-empty-string)
    null)
(defun resource-item--malformed-identifier (scheme encoded reason)
  "Signal that ENCODED cannot identify one exact SCHEME item because of REASON."
  (error 'resource-uri-malformed
         :uri    (format nil "~A:id/~A" scheme encoded)
         :reason reason))

(-> resource-item-decode-identifier (string string string) non-empty-string)
(defun resource-item-decode-identifier (scheme noun encoded)
  "Decode one percent-encoded exact-item path segment from ENCODED."
  (when (zerop (length encoded))
    (resource-item--malformed-identifier
     scheme encoded
     (format nil "the exact ~A identifier must not be empty" noun)))
  (let ((identifier
          (resource-uri-decode
           (format nil "~A:id/~A" scheme encoded)
           encoded
           :literal-character-p
           (lambda (character)
             (and (< (char-code character) 128)
                  (resource-uri-unreserved-octet-p
                   (char-code character)))))))
    (unless (non-empty-string-p identifier)
      (resource-item--malformed-identifier
       scheme encoded
       (format nil "the exact ~A identifier must not be empty" noun)))
    identifier))

(-> resource-item-uri (string non-empty-string) non-empty-string)
(defun resource-item-uri (scheme identifier)
  "Return the canonical exact-item resource URI for stable IDENTIFIER."
  (format nil "~A:id/~A" scheme (resource-item-encode-identifier identifier)))


;;;; -- Observation Identity Protocol --

(defgeneric resource-observation-state-class (resource)
  (:documentation "Return the observation-state class recorded for RESOURCE."))

(defgeneric resource-item-identity (resource)
  (:documentation "Return RESOURCE's collection or exact item identity."))

(defgeneric resource-observation-identity (observation)
  (:documentation "Return OBSERVATION's collection or exact item identity."))

(-> resource-item-find-observation-state
    (conversation resource non-empty-string)
    resource-observation-state)
(defun resource-item-find-observation-state (conversation resource alias)
  "Return CONVERSATION's exact RESOURCE observation ALIAS or signal staleness."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           (resource-observation-state-class resource))))
    (unless (and state
                 (let ((observation
                         (resource-observation-state-observation state)))
                   (and (string= (resource-uri resource)
                                 (resource-observation-uri observation))
                        (string= (resource-item-identity resource)
                                 (resource-observation-identity observation)))))
      (error 'resource-revision-stale
             :uri               (resource-uri resource)
             :expected-revision alias
             :actual-revision   nil))
    state))


;;;; -- Operations and Results --

(-> resource-item-validate-operation-fields
    (string json-object &key (:allowed list) (:required list))
    null)
(defun resource-item-validate-operation-fields (noun operation &key allowed required)
  "Require OPERATION to contain only ALLOWED fields and every REQUIRED field."
  (loop for name being the hash-keys of operation
        unless (member name allowed :test #'string=)
          do
             (error 'tool-error
                    :message   (format nil
                                       "~A resource operation does not accept field ~A."
                                       noun name)
                    :tool-name "resource.edit"))
  (dolist (name required)
    (unless (nth-value 1 (gethash name operation))
      (error 'tool-error
             :message   (format nil
                                "~A resource operation requires ~A."
                                noun name)
             :tool-name "resource.edit")))
  nil)

(-> resource-item-read-result (resource-observation-state) non-empty-string)
(defun resource-item-read-result (state)
  "Return STATE's observation as model-facing text with its URI, revision alias and full content."
  (let ((observation (resource-observation-state-observation state)))
    (format nil "URI: ~A~%Revision: ~A~%Content:~%~A"
            (resource-observation-uri observation)
            (resource-observation-state-alias state)
            (resource-observation-content observation))))
