(in-package #:autolith)

;;;; -- Diagnosis Tool --

(defclass diagnosis-run-tool (tool)
  ()
  (:documentation "A tool testing hypotheses about a failure with probe tool calls."))

(defmethod tool-execution-policy ((tool diagnosis-run-tool))
  "Run probes apart from the rest of one provider batch."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-execute ((tool diagnosis-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run a saved or given diagnosis and report what its probes ruled out."
  (declare (ignore tool))
  (handler-case
      (let* ((configuration (tool-context-configuration context))
             (base (knowledge-base-read configuration))
             (workspace (knowledge--workspace configuration))
             (name (tool-argument arguments "diagnosis"))
             (save-as (tool-argument arguments "save_as"))
             (hypotheses (tool-argument arguments "hypotheses"))
             (diagnosis
               (cond
                 (hypotheses
                  (let ((parameters (map 'list (lambda (parameter)
                                                 (format nil "?~A" (string-left-trim "?" parameter)))
                                         (or (tool-argument arguments "parameters") #())))
                        (probes (or (tool-argument arguments "probes") #())))
                    (when save-as
                      (knowledge-define-diagnosis configuration
                                                  :id save-as :parameters parameters
                                                  :hypotheses hypotheses :probes probes
                                                  :source-conversation
                                                  (and (tool-context-conversation context)
                                                       (conversation-identifier
                                                        (tool-context-conversation context)))))
                    (knowledge-make-diagnosis
                     :id (or save-as "given") :parameters parameters
                     :hypotheses hypotheses :probes probes
                     :schema-function (lambda (predicate)
                                        (knowledge-base-schema base predicate
                                                               :workspace workspace)))))
                 (name
                  (or (knowledge-base-diagnosis base name :workspace workspace)
                      (knowledge--fail ':unknown-diagnosis name
                                       "No diagnosis is saved as ~A~:[; none is saved yet~;; saved: ~:*~{~A~^, ~}~]."
                                       name
                                       (mapcar (lambda (diagnosis) (getf (rest diagnosis) :id))
                                               (knowledge-base-diagnoses base
                                                                         :workspace workspace)))))
                 (t
                  (knowledge--fail ':invalid-diagnosis arguments
                                   "Give hypotheses and probes, or the name of a saved diagnosis."))))
             (bindings (let ((object (tool-argument arguments "bindings")))
                         (and (json-object-p object)
                              (loop for key being the hash-keys of object using (hash-value value)
                                    collect (cons (format nil "?~A" (string-left-trim "?" key))
                                                  value))))))
        (tool-success
         (knowledge-diagnosis-report (knowledge-diagnosis-run context diagnosis
                                                              :bindings bindings))))
    (knowledge-error (condition)
      (error 'tool-error :message (autolith-error-message condition)
                         :tool-name "diagnosis.run"))))
