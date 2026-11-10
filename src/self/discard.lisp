(in-package #:autolith)

;;;; -- Exploratory Mutation Discard --

(-> self-discard--record (configuration (option string)) list)
(defun self-discard--record (configuration identifier)
  "Return the effective pending mutation selected by IDENTIFIER."
  (let* ((effective (image-commit-effective-pending-records configuration))
         (record
           (if identifier
               (identifier-select identifier effective
                                  :key (lambda (candidate)
                                         (getf (rest candidate) :id)))
               (first (last effective)))))
    (unless record
      (error 'source-mutation-error
             :message
             (if identifier
                 (format nil "No effective pending mutation is named ~A."
                         identifier)
                 "The active image has no effective pending mutation to discard.")
             :tool-name "self.discard"
             :pathname  (configuration-journal-path configuration)))
    record))

(-> self-discard--reapply (list) t)
(defun self-discard--reapply (record)
  "Reapply RECORD's proposed state after an incomplete discard."
  (let ((properties (rest record)))
    (case (getf properties :kind)
      (:definition
       (let* ((package
                (self-resolve-package (getf properties :package)))
              (source (getf properties :proposed)))
         (self--install-definition
          (self-read-form source :read-eval nil :package package)
          source
          :package package)))
      (:set
       (let ((symbol (self-resolve-symbol (getf properties :target))))
         (setf (symbol-value symbol)
               (eval (self-read-form (getf properties :proposed)))))))))

(-> self-discard--synchronize-definition-source (configuration list) null)
(defun self-discard--synchronize-definition-source (configuration record)
  "Synchronize the exploratory source cache after discarding RECORD."
  (let* ((properties (rest record))
         (target     (getf properties :target))
         (preceding
           (find-if
            (lambda (candidate)
              (let ((candidate-properties (rest candidate)))
                (and (eq (getf candidate-properties :kind) :definition)
                     (string= (getf candidate-properties :target) target))))
            (reverse (image-commit-pending-records configuration)))))
    (if preceding
        (setf (gethash target *exploratory-definitions*)
              (getf (rest preceding) :proposed))
        (remhash target *exploratory-definitions*)))
  nil)

(-> self-discard-mutation (configuration (option string)) list)
(defun self-discard-mutation (configuration identifier)
  "Restore and journal the effective mutation selected by IDENTIFIER."
  (with-live-mutation
    (let* ((record              (self-discard--record configuration identifier))
           (properties          (rest record))
           (mutation-identifier (getf properties :id))
           (undo-action
             (gethash mutation-identifier *exploratory-undo-actions*)))
      (unless undo-action
        (error 'source-mutation-error
               :message
               "The exact undo state for this mutation is unavailable in the running image."
               :tool-name "self.discard"
               :pathname  (configuration-journal-path configuration)))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :discard
             :id mutation-identifier
             :lineage *active-image-lineage-identifier*
             :target  (getf properties :target)
             :result  ':pending))
      (funcall undo-action)
      (handler-case
          (progn
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :discard
                   :id mutation-identifier
                   :lineage *active-image-lineage-identifier*
                   :target  (getf properties :target)
                   :result  ':discarded))
            (when (eq (getf properties :kind) :definition)
              (self-discard--synchronize-definition-source
               configuration
               record))
            (remhash mutation-identifier *exploratory-undo-actions*)
            record)
        (error (condition)
          (handler-case
              (self-discard--reapply record)
            (error (restoration-condition)
              (error 'active-image-corruption
                     :message
                     "A discard journal failure could not restore the proposed live state."
                     :original-condition    condition
                     :restoration-condition restoration-condition)))
          (error condition))))))

(defmethod tool-execute ((tool self-discard-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Restore and discard one effective exploratory mutation."
  (declare (ignore tool))
  (tuning-experiment-assert-settled
   (tool-context-configuration context) "self.discard")
  (let* ((record
           (self-discard-mutation
            (tool-context-configuration context)
            (tool-argument arguments "mutation")))
         (properties (rest record)))
    (tool-success
     (format nil "Discarded ~A ~A (~A); the preceding live state is restored."
             (getf properties :kind)
             (getf properties :target)
             (getf properties :id)))))
