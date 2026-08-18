(in-package #:autolith)

;;;; -- Recursive Inference Trace Resources --

(defparameter *rlm-trace-read-maximum-characters* 40000
  "The most characters one inference or context resource read returns.")

(defparameter *rlm-resource-default-line-count* 400
  "The default line count for one inference or context resource window.")

(defparameter *rlm-resource-maximum-line-count* 1000
  "The largest line count for one inference or context resource window.")

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
  "Return the complete persisted trace IDENTIFIER text, or NIL when absent."
  (let* ((identity (merge-pathnames
                    (make-pathname :name identifier :type "sexp")
                    (configuration-inference-root configuration)))
         (segments (conversation-storage-pathnames identity)))
    (when segments
      (with-output-to-string (stream)
        (dolist (segment segments)
          (write-string (uiop:read-file-string segment) stream))))))

(-> rlm--window-argument
    (hash-table string (integer 1) (integer 1))
    (integer 1))
(defun rlm--window-argument (arguments name fallback maximum)
  "Return the validated window integer NAME from ARGUMENTS."
  (let ((value (gethash name arguments)))
    (cond
      ((null value) fallback)
      ((and (integerp value) (<= 1 value maximum)) value)
      (t (error 'tool-error
                :message (format nil "~A must be an integer between 1 and ~D."
                                 name maximum)
                :tool-name "resource.read")))))

(-> rlm--resource-window (string hash-table) string)
(defun rlm--resource-window (content arguments)
  "Return the requested bounded numbered line window over CONTENT.

Range access keeps late lines reachable without materializing the
whole text into the model context: the header reports the total line
count, so a follow-up read can target any region, including the tail."
  (let* ((lines (text--split-lines content))
         (total (length lines))
         (start (min (rlm--window-argument
                      arguments "start-line" 1 most-positive-fixnum)
                     (max 1 total)))
         (count (rlm--window-argument
                 arguments "line-count"
                 *rlm-resource-default-line-count*
                 *rlm-resource-maximum-line-count*))
         (end (min total (1- (+ start count))))
         (window
           (with-output-to-string (stream)
             (format stream "lines ~D-~D of ~D~%" start end total)
             (loop for line-number from start to end
                   do (format stream "~5D  ~A~%"
                              line-number (aref lines (1- line-number)))))))
    (if (<= (length window) *rlm-trace-read-maximum-characters*)
        window
        (format nil "~A~%[window truncated after ~D characters]"
                (subseq window 0 *rlm-trace-read-maximum-characters*)
                *rlm-trace-read-maximum-characters*))))

(defmethod resource-observe
    ((resource inference-trace-resource) (context tool-context))
  "Observe one complete persisted inference trace."
  (let* ((identifier (inference-trace-resource-identifier resource))
         (content (rlm--trace-content (tool-context-configuration context)
                                      identifier)))
    (unless content
      (error 'rlm-view-error
             :designator identifier
             :message "no inference trace has this identifier"))
    (make-instance 'resource-observation
                   :uri (resource-uri resource)
                   :revision identifier
                   :content content)))

(defmethod resource-tool-read
    ((resource inference-trace-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return one bounded numbered window over an inference trace log."
  (declare (ignore tool))
  (let ((content (rlm--trace-content
                  (tool-context-configuration context)
                  (inference-trace-resource-identifier resource))))
    (if content
        (tool-success (rlm--resource-window content arguments))
        (tool-failure
         (format nil "No inference trace ~A exists."
                 (inference-trace-resource-identifier resource))))))


;;;; -- Context Object Resources --

(defclass context-object-resource (resource)
  ((digest
    :initarg :digest
    :reader context-object-resource-digest
    :type non-empty-string
    :documentation "The content digest selecting one stored context object."))
  (:documentation "One read-only content-addressed context object."))

(defclass context-object-resolver (resource-resolver)
  ()
  (:documentation "Resolve stored context objects by content digest."))

(defmethod resource-resolver-resolve
    ((resolver context-object-resolver) identifier (context tool-context))
  "Resolve one exact context object digest."
  (declare (ignore context))
  (unless (and (stringp identifier)
               (non-empty-string-p identifier)
               (every (lambda (character) (digit-char-p character 16))
                      identifier))
    (error 'resource-operation-unsupported
           :uri (format nil "~A:~A"
                        (resource-resolver-scheme resolver) identifier)
           :operation ':resolve))
  (make-instance 'context-object-resource
                 :uri (format nil "context:~A" identifier)
                 :digest (string-downcase identifier)))

(defmethod resource-capabilities
    ((resource context-object-resource) (context tool-context))
  "Expose context objects as read-only observations."
  (declare (ignore resource context))
  '(:read))

(-> rlm--context-object-text (context-object-resource tool-context) string)
(defun rlm--context-object-text (resource context)
  "Return RESOURCE's verified stored content."
  (let ((object (rlm-context-object-find
                 (tool-context-configuration context)
                 (context-object-resource-digest resource))))
    (unless object
      (error 'rlm-view-error
             :designator (context-object-resource-digest resource)
             :message "no stored context object has this digest"))
    (uiop:read-file-string (rlm-context-object-pathname object))))

(defmethod resource-observe
    ((resource context-object-resource) (context tool-context))
  "Observe one complete stored context object."
  (make-instance 'resource-observation
                 :uri (resource-uri resource)
                 :revision (context-object-resource-digest resource)
                 :content (rlm--context-object-text resource context)))

(defmethod resource-tool-read
    ((resource context-object-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Return one bounded numbered window over a stored context object."
  (declare (ignore tool))
  (handler-case
      (tool-success (rlm--resource-window
                     (rlm--context-object-text resource context)
                     arguments))
    (rlm-view-error (condition)
      (tool-failure (format nil "~A" condition)))))
