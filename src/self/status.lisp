(in-package #:autolith)

;;;; -- Active Image Status --

(-> self-status--identifier ((option string)) string)
(defun self-status--identifier (identifier)
  "Render optional IDENTIFIER as a private image state name."
  (or identifier "base"))

(-> self-status--generation ((option generation)) string)
(defun self-status--generation (generation)
  "Render optional GENERATION's identity and checkpoint health, or NONE."
  (if generation
      (let* ((health (generation-health generation))
             (state  (if (getf health :compatible)
                         "compatible"
                         (format nil "incompatible: ~A"
                                 (getf health :reason)))))
        (format nil "~A (~A)" (generation-identifier generation) state))
      "none"))

(-> self-status-render (configuration) string)
(defun self-status-render (configuration)
  "Return a concise summary of live mutation and retained recovery state."
  (multiple-value-bind (selected-identifier selected-history-commit)
      (image-commit--pointer-state configuration)
    (let* ((pending             (image-commit-pending-records configuration))
           (effective           (image-commit-effective-diff-records configuration))
           (health              (generation-health-summary configuration))
           (newest-generation   (first (getf health :generations)))
           (selected-generation (getf health :selected))
           (warning             (generation-health-warning health))
           (selection-current-p
             (and (equal *active-image-commit-identifier* selected-identifier)
                  (equal *active-image-history-commit*
                         selected-history-commit))))
      (with-output-to-string (stream)
        (format stream "private image~%  running   ~A~%  selected  ~A~%  synchronized  ~:[no~;yes~]~%"
                (self-status--identifier *active-image-commit-identifier*)
                (self-status--identifier selected-identifier)
                selection-current-p)
        (format stream "journal~%  lineage    ~A~%  pending     ~D installed, ~D effective~%"
                (or *active-image-lineage-identifier* "uninitialized")
                (length pending)
                (length effective))
        (dolist (record effective)
          (let ((properties (rest record)))
            (format stream "  ~A  ~A~%"
                    (getf properties :kind)
                    (getf properties :target))))
        (let ((experiment (tuning-experiment-render-open configuration)))
          (when experiment
            (format stream "~A~%" experiment)))
        (format stream "checkpoint~%  publishing  ~:[no~;yes~]~%"
                *checkpoint-in-progress-p*)
        (format stream "generations~%  retained   ~D~%  compatible ~D~%  rollback   ~:[unavailable~;available~]~%  newest     ~A~%  selected   ~A"
                (getf health :retained)
                (getf health :compatible)
                (plusp (getf health :compatible))
                (self-status--generation newest-generation)
                (self-status--generation selected-generation))
        (when warning
          (format stream "~%  warning    ~A" warning))))))

(defmethod tool-execute ((tool self-status-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Summarize CONTEXT's active image and retained recovery state."
  (declare (ignore tool arguments))
  (tool-success
   (self-status-render (tool-context-configuration context))))
