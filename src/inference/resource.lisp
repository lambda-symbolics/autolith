(in-package #:autolith)

;;;; -- Recursive Inference Trace Resources --

(defparameter *rlm-trace-read-maximum-characters* 40000
  "The most trace characters one inference resource read returns.")

(defclass inference-trace-resource (resource)
  ((identifier
    :initarg :identifier
    :reader inference-trace-resource-identifier
    :type non-empty-string
    :documentation "The trace conversation identifier selected by this URI."))
  (:documentation "One read-only persisted inference frame trace."))

(defclass inference-trace-resolver (resource-resolver)
  ()
  (:documentation "Resolve inference frame traces by conversation identifier."))

(-> rlm--trace-identifier-p (t) boolean)
(defun rlm--trace-identifier-p (identifier)
  "Return true when IDENTIFIER is a safe trace conversation identifier."
  (and (stringp identifier)
       (non-empty-string-p identifier)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              identifier)))

(defmethod resource-resolver-resolve
    ((resolver inference-trace-resolver) identifier (context tool-context))
  "Resolve one exact inference trace identifier."
  (declare (ignore context))
  (unless (rlm--trace-identifier-p identifier)
    (error 'resource-operation-unsupported
           :uri (format nil "~A:~A"
                        (resource-resolver-scheme resolver) identifier)
           :operation ':resolve))
  (make-instance 'inference-trace-resource
                 :uri (format nil "inference:~A" identifier)
                 :identifier identifier))

(defmethod resource-capabilities
    ((resource inference-trace-resource) (context tool-context))
  "Expose traces as read-only observations."
  (declare (ignore resource context))
  '(:read))

(-> rlm--trace-content (configuration string) (option string))
(defun rlm--trace-content (configuration identifier)
  "Return the bounded persisted trace IDENTIFIER text, or NIL when absent."
  (let* ((identity (merge-pathnames
                    (make-pathname :name identifier :type "sexp")
                    (configuration-inference-root configuration)))
         (segments (conversation-storage-pathnames identity)))
    (when segments
      (let ((content
              (with-output-to-string (stream)
                (dolist (segment segments)
                  (write-string (uiop:read-file-string segment) stream)))))
        (if (<= (length content) *rlm-trace-read-maximum-characters*)
            content
            (format nil "~A~%;; Trace truncated after ~D of ~D characters."
                    (subseq content 0 *rlm-trace-read-maximum-characters*)
                    *rlm-trace-read-maximum-characters*
                    (length content)))))))

(defmethod resource-tool-read
    ((resource inference-trace-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return one complete bounded inference trace log."
  (declare (ignore tool arguments))
  (let ((content (rlm--trace-content
                  (tool-context-configuration context)
                  (inference-trace-resource-identifier resource))))
    (if content
        (tool-success content)
        (tool-failure
         (format nil "No inference trace ~A exists."
                 (inference-trace-resource-identifier resource))))))
