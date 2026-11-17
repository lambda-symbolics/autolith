(in-package #:autolith)

;;;; -- Compaction Failure Reports --

(defclass compaction-report-context ()
  ((result
    :initform      nil
    :accessor      compaction-report-result
    :type          (option provider-result)
    :documentation "The latest completed response, if compaction issued a request.")
   (response
    :initform      nil
    :accessor      compaction-report-response
    :type          (option hash-table)
    :documentation "A detached parsed-result snapshot redacted inside credential ownership.")
   (failure-text
    :initform      nil
    :accessor      compaction-report-failure-text
    :type          (option string)
    :documentation "The report serialized at signal time inside credential ownership.")
   (published-p
    :initform      nil
    :accessor      compaction-report-published-p
    :type          boolean
    :documentation "Whether the checkpoint was committed before a later adapter failure."))
  (:documentation "Diagnostic state confined to one compaction operation."))

(defparameter *compaction-report-context* nil
  "The dynamically bound diagnostic context of the current compaction operation.")

(-> compaction-report-capture-result (compaction-report-context provider-result) null)
(defun compaction-report-capture-result (context result)
  "Capture RESULT once, before request credentials unwind when available."
  (unless (eq result (compaction-report-result context))
    (setf (compaction-report-result context) result
          (compaction-report-response context) nil)
    (let ((metadata (provider-result-metadata result)))
      (setf (compaction-report-response context)
            (provider--sanitize-wire-value
             (json-object
              "response_id" (provider-result-response-id result)
              "output_items" (coerce (provider-result-output-items result) 'vector)
              "tool_calls" (coerce (provider-result-tool-calls result) 'vector)
              "usage" (provider-result-usage result)
              "turn_completion" (string-downcase (symbol-name (provider-result-turn-completion result)))
              "metadata" (when metadata
                           (json-object
                            "model" (provider-response-metadata-model metadata)
                            "finish_reason" (provider-response-metadata-finish-reason metadata)
                            "reasoning_context" (provider-response-metadata-reasoning-context metadata))))))))
  nil)

(-> compaction-report-text (compaction-report-context error) string)
(defun compaction-report-text (context condition)
  "Describe CONDITION at signal time and include the detached response snapshot."
  (format nil "Autolith ~A~%Compaction failure~%~A~%~%Diagnostics:~%~A~%"
          *autolith-version*
          (provider--sanitize-wire-string (princ-to-string condition))
          (json-encode
           (provider--sanitize-wire-value
            (json-object
             "condition_type" (princ-to-string (type-of condition))
             "provider_response" (when (typep condition 'provider-error)
                                   (provider-error-response condition))
             "last_completed_response" (compaction-report-response context))))))

(-> compaction-report-save (configuration string) pathname)
(defun compaction-report-save (configuration text)
  "Atomically save private UTF-8 TEXT below CONFIGURATION's XDG state root."
  (let ((pathname
          (merge-pathnames
           (make-pathname :name (make-identifier) :type "txt")
           (merge-pathnames "compaction-reports/"
                            (merge-pathnames (config :state-root configuration) (uiop:getcwd))))))
    (ensure-directories-exist pathname)
    (snapshot-write-text pathname text :require-absent t)))

(-> compaction-report--notify
    (configuration compaction-report-context &key (:notify function)) null)
(defun compaction-report--notify (configuration context &key notify)
  "Save a report and call NOTIFY without replacing the original failure."
  (let ((details
          (handler-case
              (list :path (namestring (compaction-report-save
                                       configuration (compaction-report-failure-text context))))
            (error (failure)
              (list :save-error (provider--sanitize-wire-string (princ-to-string failure)))))))
    ;; A diagnostic adapter failure must not replace the compaction condition.
    (handler-case (funcall notify details)
      (error () nil)))
  nil)

(-> compaction-report-call (configuration function &key (:notify function)) (values &rest t))
(defun compaction-report-call (configuration function &key (notify (constantly nil)))
  "Call FUNCTION with diagnostic context and report only an aborted compaction.

Preserve its values, condition identity and live restarts. A resumed condition,
cancellation, successful fallback or post-publication adapter error is not a
failed checkpoint. NOTIFY receives a saved absolute :PATH or a :SAVE-ERROR."
  (let* ((context                     (make-instance 'compaction-report-context))
         (*compaction-report-context* context)
         (*provider-result-diagnostic-function*
           (lambda (result) (compaction-report-capture-result context result)))
         (failure                     nil)
         (completed-p                 nil))
    (unwind-protect
         (handler-bind
             ((serious-condition
                (lambda (condition)
                  (setf failure (and (typep condition 'error) condition))
                  (when failure
                    ;; Handlers run before request credentials unwind. Never defer
                    ;; condition printing or provider-body redaction to cleanup.
                    (setf (compaction-report-failure-text context)
                          (handler-case (compaction-report-text context condition)
                            (error () "Compaction failed. Diagnostics could not be serialized.")))))))
           (multiple-value-prog1 (funcall function context)
             (setf completed-p t)))
      (when (and failure (not completed-p) (not (compaction-report-published-p context)))
        ;; Reporting is secondary, including serialization and filesystem errors.
        (handler-case (compaction-report--notify configuration context
                                                :notify notify)
          (error () nil))))))
