(in-package #:autolith)

;;;; -- Focused Mutation Exercises --

(-> self-exercise--mutation-record
    (configuration (option string))
    (option list))
(defun self-exercise--mutation-record (configuration identifier)
  "Return the effective mutation selected by IDENTIFIER for an exercise.

When IDENTIFIER is NIL and no mutation is pending, return NIL so the exercise
can verify the current active state after a discard."
  (let* ((effective (image-commit-effective-diff-records configuration))
         (record
           (if identifier
               (find identifier effective
                     :key  (lambda (candidate)
                             (getf (rest candidate) :id))
                     :test #'string=)
               (first (last effective)))))
    (when (and identifier (null record))
      (error 'source-mutation-error
             :message
             (format nil "No effective pending mutation is named ~A."
                     identifier)
             :tool-name "self.exercise"
             :pathname  (configuration-journal-path configuration)))
    record))

(-> self-exercise-mutation (configuration string (option string)) string)
(defun self-exercise-mutation (configuration source mutation-identifier)
  "Run SOURCE against the active image and append pass or fail evidence."
  (with-live-mutation
    (let* ((record
              (self-exercise--mutation-record configuration mutation-identifier))
           (mutation-identifier
             (and record (getf (rest record) :id)))
           (exercise-identifier (make-identifier)))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :exercise
             :id exercise-identifier
             :lineage  *active-image-lineage-identifier*
             :mutation mutation-identifier
             :proposed source
             :result   ':pending))
      (handler-case
          (multiple-value-bind (result-values output)
              (self-capture-evaluation
               (lambda ()
                 (eval (self-read-form source))))
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :exercise
                   :id exercise-identifier
                   :lineage  *active-image-lineage-identifier*
                   :mutation mutation-identifier
                   :proposed source
                   :result   ':passed
                   :values   result-values
                   :output   (bounded-string output :limit 2000)))
            (format nil
                    "Exercise ~A passed ~:[against the current active state~;for mutation ~:*~A~].~2%~A"
                    exercise-identifier
                    mutation-identifier
                    (self-evaluation-result result-values output)))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :exercise
                 :id exercise-identifier
                 :lineage   *active-image-lineage-identifier*
                 :mutation  mutation-identifier
                 :proposed  source
                 :result    ':failed
                 :condition (bounded-string condition :limit 2000)))
          (error condition))))))

(defmethod tool-execute ((tool self-exercise-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run and journal one focused exercise against the active image."
  (declare (ignore tool))
  (tool-success
   (self-exercise-mutation
    (tool-context-configuration context)
    (tool-argument arguments "form" :required t)
    (tool-argument arguments "mutation"))))
