(in-package #:autolith)

;;;; -- Resident Tool Presentations --

(-> terminal-ui-fullscreen--record-entry (list keyword list) list)
(defun terminal-ui-fullscreen--record-entry (entry state body)
  "Color ENTRY's original call header for STATE, preserving widgets and BODY."
  (let ((header-p t)
        (style    (case state (:success
                               ':success)
                              (:failure
                               ':failure)
                              (t
                               ':tool)))
        (marker   (case state (:success
                               #\✓)
                              (:failure
                               #\!)
                              (t
                               #\▸))))
    (append
     (loop for span in entry
           for text = (terminal-span-text span)
           for role = (terminal-span-style span)
           collect (if (and header-p (eq role ':tool))
                       (let ((label (if (uiop:string-prefix-p "▸ " text)
                                        (concatenate 'string (string marker) (subseq text 1))
                                        text)))
                         (if (termdown:widget-p span)
                             (termdown:make-widget style label (termdown:widget-action span))
                             (terminal-span style label)))
                       span)
           when (find #\Newline text)
             do (setf header-p nil))
     body)))

(-> terminal-ui-fullscreen--index-record (fullscreen-terminal-ui list) null)
(defun terminal-ui-fullscreen--index-record (ui record)
  "Index immutable resident RECORD by its chunk, call and optional job identity."
  (let* ((index    (fullscreen-terminal-ui-presentation-records ui))
         (previous (gethash (getf record :identifier) index))
         (old-job  (getf previous :job-key)))
    (when (and old-job (not (equal old-job (getf record :job-key))))
      (remhash old-job index))
    (dolist (key (list (getf record :identifier) (getf record :call-key) (getf record :job-key)))
      (when key
        (setf (gethash key index) record))))
  nil)

(-> terminal-ui-fullscreen--tag-last (fullscreen-terminal-ui list) null)
(defun terminal-ui-fullscreen--tag-last (ui identifiers)
  "Tag the last chunk and update any resident call's immutable replacement key."
  (let* ((viewport (fullscreen-terminal-ui-viewport ui))
         (old-tag  (clinedi:transcript-viewport-last-tag viewport))
         (tag      (append identifiers old-tag))
         (index    (fullscreen-terminal-ui-presentation-records ui)))
    (setf (clinedi:transcript-viewport-last-tag viewport) tag)
    (dolist (identifier old-tag)
      (let ((record (gethash identifier index)))
        (when record
          (let ((next (copy-list record)))
            (setf (getf next :tag) tag
                  (getf next :identifiers)
                  (union identifiers (getf record :identifiers) :test #'equal))
            (terminal-ui-fullscreen--index-record ui next))))))
  nil)

(-> terminal-ui-fullscreen--append-entry
    (fullscreen-terminal-ui t (or string list) &key (:metadata list)) string)
(defun terminal-ui-fullscreen--append-entry (ui identifier entry &key metadata)
  "Append and tag one resident ENTRY, retaining original call spans when supplied."
  (let* ((group  (terminal-ui--separation-group entry))
         (prefix (if (terminal-ui--separated-p (terminal-ui-compact-tail-group ui) group)
                     (string #\Newline)
                     "")))
    (multiple-value-bind (text display regions)
        (terminal-ui--finalized-content
         ui (if (eq (getf metadata :kind) ':call)
                (terminal-ui-fullscreen--record-entry
                 entry (getf metadata :state)
                 (when (getf metadata :retain-output-p) (getf metadata :body)))
                entry))
      (when (eq (getf metadata :kind) ':call)
        (terminal-ui-fullscreen--index-record
         ui (list :identifier identifier :entry entry :prefix prefix :tag (list identifier)
                  :call-key (getf metadata :call-key) :job-key (getf metadata :job-key)
                  :identifiers (list identifier))))
      (clinedi:transcript-viewport-append
       (fullscreen-terminal-ui-viewport ui)
       (concatenate 'string prefix text) (concatenate 'string prefix display)
       :regions (termdown:shift-regions regions (length prefix)) :tag (list identifier))
      (unless (eq group ':attached)
        (setf (terminal-ui-compact-tail-group ui) group))
      (setf (terminal-ui-fullscreen-welcome-p ui) nil)
      (terminal-ui--remember-finalized ui (list identifier) :note-output-p nil)
      (concatenate 'string prefix display))))

(-> terminal-ui-fullscreen--replace-record
    (fullscreen-terminal-ui list &key (:identifier t) (:metadata list)) boolean)
(defun terminal-ui-fullscreen--replace-record (ui record &key identifier metadata)
  "Replace RECORD's resident invocation with the outcome in METADATA."
  (let* ((prefix (getf record :prefix))
         (entry  (terminal-ui-fullscreen--record-entry
                  (getf record :entry) (getf metadata :state)
                  (when (getf metadata :retain-output-p) (getf metadata :body))))
         (next   (copy-list record)))
    (setf (getf next :identifiers) (cons identifier (getf record :identifiers))
          (getf next :job-key) (or (getf metadata :job-key) (getf record :job-key)))
    ;; Install the new immutable index value before replacement can evict its chunk.
    (terminal-ui-fullscreen--index-record ui next)
    (multiple-value-bind (text display regions)
        (terminal-ui--finalized-content ui entry)
      (if (clinedi:transcript-viewport-replace
           (fullscreen-terminal-ui-viewport ui) (getf record :tag)
           :text    (concatenate 'string prefix text)
           :display (concatenate 'string prefix display)
           :regions (termdown:shift-regions regions (length prefix)))
          (progn
            (when (gethash (getf record :identifier) (fullscreen-terminal-ui-presentation-records ui))
              (terminal-ui--remember-finalized ui (list identifier) :note-output-p nil))
            t)
          (progn
            (terminal-ui-fullscreen--forget-evicted ui (list (getf record :identifier)))
            nil)))))

(-> terminal-ui-fullscreen--apply-presentations (fullscreen-terminal-ui list) string)
(defun terminal-ui-fullscreen--apply-presentations (ui presentations)
  "Apply ordered semantic PRESENTATIONS inside the current publication transaction."
  (with-output-to-string (output)
    (dolist (presentation presentations)
      (destructuring-bind (identifier entry metadata) presentation
        (unless (terminal-ui--finalized-p ui identifier)
          (let* ((index  (fullscreen-terminal-ui-presentation-records ui))
                 (record (or (gethash (getf metadata :call-key) index)
                             (gethash (getf metadata :job-key) index))))
            (unless (and record (member (getf metadata :kind) '(:result :completion))
                         (terminal-ui-fullscreen--replace-record
                          ui record :identifier identifier :metadata metadata))
              (when entry
                (write-string (terminal-ui-fullscreen--append-entry
                               ui identifier entry :metadata metadata)
                              output)))))))))

(defmethod terminal-ui-present-records ((ui fullscreen-terminal-ui) presentations)
  "Publish individually replaceable records with one paint, or queue them during direct I/O."
  (with-terminal-ui-locked (ui)
    (let ((seen    (make-hash-table :test #'equal))
          (pending nil))
      (dolist (operation (fullscreen-terminal-ui-deferred-operations ui))
        (when (eq (first operation) ':records)
          (dolist (presentation (second operation))
            (setf (gethash (first presentation) seen) t))))
      (dolist (presentation presentations)
        (let ((identifier (first presentation)))
          (unless (or (terminal-ui--finalized-p ui identifier) (gethash identifier seen))
            (setf (gethash identifier seen) t)
            (push presentation pending))))
      (setf pending (nreverse pending))
      (when pending
        (terminal-ui-fullscreen--submit ui (list ':records pending)))
      (length pending))))
