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
    (dolist (key (cons (getf record :identifier)
                       (when (eq (getf record :kind) ':call)
                         (list (getf record :call-key) (getf record :job-key)))))
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

(-> terminal-ui-fullscreen--toggle-tool-input (fullscreen-terminal-ui t) boolean)
(defun terminal-ui-fullscreen--toggle-tool-input (ui key)
  "Toggle the resident input at KEY under UI's presentation lock and repaint."
  (let* ((index  (fullscreen-terminal-ui-presentation-records ui))
         (record (gethash key index)))
    (when (and record (getf record :expanded-entry-layout))
      (let ((next (copy-list record)))
        (setf (getf next :input-expanded-p) (not (getf record :input-expanded-p)))
        (multiple-value-bind (text display regions)
            (terminal-ui-fullscreen--record-content
             ui next (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui)))
          (terminal-ui-fullscreen--index-record ui next)
          (if (clinedi:transcript-viewport-replace
               (fullscreen-terminal-ui-viewport ui) (getf record :tag)
               :text text :display display :regions regions
               :layout-function   (terminal-ui-fullscreen--record-layout-function ui next)
               :source-characters (terminal-ui-fullscreen--record-source-characters next))
              (progn
                (terminal-ui--paint-live ui)
                t)
              (progn
                (terminal-ui-fullscreen--forget-evicted ui (list (getf record :identifier)))
                nil)))))))

(-> terminal-ui-fullscreen--record-content
    (fullscreen-terminal-ui list integer &key (:entry (or string list)))
    (values string string list list))
(defun terminal-ui-fullscreen--record-content (ui record width &key (entry nil entry-p))
  "Lay out resident RECORD at WIDTH, optionally reusing its initial ENTRY."
  (when (getf record :markdown)
    (return-from terminal-ui-fullscreen--record-content
      (terminal-ui-fullscreen--markdown-content ui record width)))
  (let* ((layout      (or (and (getf record :input-expanded-p)
                               (getf record :expanded-entry-layout))
                          (getf record :entry-layout)))
         (entry       (if entry-p entry
                          (if layout (funcall layout width) (getf record :entry))))
         (body-layout (getf record :body-layout))
         (transform   (getf record :entry-transform))
         (body        (when (and (eq (getf record :kind) ':call) (getf record :retain-output-p))
                        (if (and body-layout (not entry-p))
                            (funcall body-layout width)
                            (getf record :body))))
         (prefix      (getf record :prefix "")))
    (multiple-value-bind (text display regions)
        (terminal-ui--finalized-content
         ui (if (eq (getf record :kind) ':call)
                (if transform
                     (append (funcall transform entry (getf record :state) body)
                             (getf record :annotations))
                     (terminal-ui-fullscreen--record-entry
                      entry (getf record :state) (append (getf record :annotations) body)))
                entry)
         :group (getf record :group))
      (values (concatenate 'string prefix text) (concatenate 'string prefix display)
              (termdown:shift-regions regions (length prefix)) nil))))

(-> terminal-ui-fullscreen--record-source-characters (list) (integer 0))
(defun terminal-ui-fullscreen--record-source-characters (record)
  "Charge logical sources retained by RECORD to its bounded viewport chunk."
  (flet ((weight (key)
           "Read an integer charge or a live source-cache weight."
           (let ((value (getf record key)))
             (if (functionp value) (funcall value) (or value 0)))))
    (+ (weight :source-characters)
       (weight :body-source-characters)
       (loop for span in (getf record :annotations) sum (length (terminal-span-text span)))
       (let ((markdown (getf record :markdown)))
         (if markdown
             (+ (length (getf markdown :source))
                (loop for span in (getf markdown :header)
                      sum (length (terminal-span-text span))))
             0)))))

(-> terminal-ui-fullscreen--record-layout-function
    (fullscreen-terminal-ui list) (option function))
(defun terminal-ui-fullscreen--record-layout-function (ui record)
  "Keep RECORD's source callback with its chunk until replacement or eviction."
  (when (or (getf record :markdown) (getf record :entry-layout) (getf record :body-layout))
    (lambda (width)
      (multiple-value-bind (text display regions tail)
          (terminal-ui-fullscreen--record-content ui record width)
        (when (equal (getf record :identifier) (fullscreen-terminal-ui-markdown-stream ui))
          (setf (terminal-ui-stream-tail ui) tail))
        (values text display regions)))))

(-> terminal-ui-fullscreen--append-entry
    (fullscreen-terminal-ui t (or string list) &key (:metadata list)) string)
(defun terminal-ui-fullscreen--append-entry (ui identifier entry &key metadata)
  "Append one resident source-backed entry under stable IDENTIFIER."
  (let* ((group  (getf metadata :group))
         (record (append
                  (list :identifier identifier
                        :entry      (unless (or (getf metadata :markdown)
                                                (getf metadata :entry-layout)) entry)
                        :prefix     (if (terminal-ui--separated-p
                                         (terminal-ui-compact-tail-group ui) group)
                                        (string #\Newline) "")
                        :tag (list identifier) :identifiers (list identifier))
                  metadata)))
    (multiple-value-bind (text display regions)
        (terminal-ui-fullscreen--record-content
         ui record (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui)) :entry entry)
      (unless (eq (getf record :kind) ':call)
        (setf (getf record :body-layout) nil
              (getf record :body-source-characters) 0
              (getf record :body) nil))
      (when (getf record :body-layout)
        (setf (getf record :body) nil))
      (when (or (eq (getf record :kind) ':call) (getf record :markdown)
                (getf record :entry-layout))
        (terminal-ui-fullscreen--index-record ui record))
      (clinedi:transcript-viewport-append
       (fullscreen-terminal-ui-viewport ui) text display :regions regions :tag (getf record :tag)
       :layout-function   (terminal-ui-fullscreen--record-layout-function ui record)
       :source-characters (terminal-ui-fullscreen--record-source-characters record))
      (unless (eq group ':attached)
        (setf (terminal-ui-compact-tail-group ui) group))
      (setf (terminal-ui-fullscreen-welcome-p ui) nil)
      (terminal-ui--remember-finalized ui (list identifier) :note-output-p nil)
      display)))

(-> terminal-ui-fullscreen--apply-outcome (list list) list)
(defun terminal-ui-fullscreen--apply-outcome (record metadata)
  "Update copied RECORD with an outcome or an annotation of its exact call."
  (setf (getf record :job-key) (or (getf metadata :job-key) (getf record :job-key)))
  (if (eq (getf metadata :kind) ':annotation)
      (setf (getf record :annotations)
            (append (getf record :annotations) (getf metadata :annotations)))
      (dolist (key '(:state :retain-output-p :body :body-layout :body-source-characters :entry-transform))
        (setf (getf record key) (getf metadata key))))
  record)

(-> terminal-ui-fullscreen--publish-replacement (fullscreen-terminal-ui list t) boolean)
(defun terminal-ui-fullscreen--publish-replacement (ui next identifier)
  "Replace NEXT's resident chunk and finalize its additional IDENTIFIER."
  (multiple-value-bind (text display regions)
      (terminal-ui-fullscreen--record-content
       ui next (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui)))
    (when (getf next :body-layout)
      (setf (getf next :body) nil))
    ;; Index before replacement can evict this chunk and release its sources.
    (terminal-ui-fullscreen--index-record ui next)
    (if (clinedi:transcript-viewport-replace
         (fullscreen-terminal-ui-viewport ui) (getf next :tag)
         :text text :display display :regions regions
         :layout-function   (terminal-ui-fullscreen--record-layout-function ui next)
         :source-characters (terminal-ui-fullscreen--record-source-characters next))
        (progn
          (when (gethash (getf next :identifier) (fullscreen-terminal-ui-presentation-records ui))
            (terminal-ui--remember-finalized ui (list identifier) :note-output-p nil))
          t)
        (progn
          (terminal-ui-fullscreen--forget-evicted ui (list (getf next :identifier)))
          nil))))

(-> terminal-ui-fullscreen--replace-record
    (fullscreen-terminal-ui list &key (:identifier t) (:metadata list)) boolean)
(defun terminal-ui-fullscreen--replace-record (ui record &key identifier metadata)
  "Replace RECORD's invocation with its latest source-backed outcome."
  (let ((next (copy-list record)))
    (setf (getf next :identifiers) (cons identifier (getf record :identifiers)))
    (terminal-ui-fullscreen--publish-replacement
     ui (terminal-ui-fullscreen--apply-outcome next metadata) identifier)))

(-> terminal-ui-fullscreen--merge-schema-record
    (fullscreen-terminal-ui t list) boolean)
(defun terminal-ui-fullscreen--merge-schema-record (ui identifier metadata)
  "Merge schema names only into the actual last visible schema chunk."
  (when (and (eq (getf metadata :entry-kind) ':tool-schemas)
             (or (not (terminal-ui-started-p ui))
                 (terminal-interactive-p (terminal-ui-terminal ui))
                 (not (terminal-plain-output-p (terminal-ui-terminal ui)))))
    (let* ((tag    (clinedi:transcript-viewport-last-tag (fullscreen-terminal-ui-viewport ui)))
           (index  (fullscreen-terminal-ui-presentation-records ui))
           (record (loop for key in tag thereis (gethash key index))))
      (when (and record (equal tag (getf record :tag))
                 (eq (getf record :entry-kind) ':tool-schemas)
                 (not (getf record :schema-interrupted-p)))
        (let* ((next        (copy-list record))
               (names       (remove-duplicates
                             (append (getf record :schema-names) (getf metadata :schema-names))
                             :test #'string= :from-end t))
               (layout      (getf metadata :schema-layout))
               (identifiers (getf record :identifiers)))
          (setf (getf next :identifiers)
                (cons identifier (subseq identifiers 0 (min (length identifiers)
                                                           (max 0 (1- *terminal-ui-finalized-identifier-limit*)))))
                (getf next :schema-names) names
                (getf next :entry-layout) (lambda (columns) (funcall layout names columns))
                ;; Charge merged identities even when no new namespace is exposed.
                (getf next :source-characters)
                (+ (loop for name in names sum (length name))
                   (* 64 (length (getf next :identifiers)))))
          (terminal-ui-fullscreen--publish-replacement ui next identifier))))))

(defmethod terminal-ui--present-live :after
    ((ui fullscreen-terminal-ui) &key status-now appended-text appended-display appended-regions)
  "Fence schema grouping after an unfinished streamed tail has been painted."
  (declare (ignore status-now appended-text appended-display appended-regions))
  (when (and (terminal-ui-started-p ui)
             (terminal-interactive-p (terminal-ui-terminal ui))
             (not (terminal-ui-live-output-suspended-p ui))
             (some (lambda (row)
                     (plusp (length (terminal-ui--row-content (terminal-ui-terminal ui) row))))
                   (terminal-ui--stream-tail-rows
                    (terminal-ui-terminal ui) (terminal-ui-stream-tail ui)
                    (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui)))))
    (let* ((tag    (clinedi:transcript-viewport-last-tag (fullscreen-terminal-ui-viewport ui)))
           (index  (fullscreen-terminal-ui-presentation-records ui))
           (record (loop for key in tag thereis (gethash key index))))
      (when (and record (eq (getf record :entry-kind) ':tool-schemas))
        (let ((next (copy-list record)))
          (setf (getf next :schema-interrupted-p) t)
          (terminal-ui-fullscreen--index-record ui next))))))

(-> terminal-ui-fullscreen--restore-outcome (fullscreen-terminal-ui t list) (option string))
(defun terminal-ui-fullscreen--restore-outcome (ui identifier metadata)
  "Restore an evicted invocation and its outcome as one indexed viewport chunk."
  (let* ((resolve (getf metadata :origin-function))
         (origin  (when resolve (funcall resolve))))
    (when origin
      (destructuring-bind (origin-identifier entry origin-metadata) origin
        (let ((next (terminal-ui-fullscreen--apply-outcome (copy-list origin-metadata) metadata)))
          (let ((display (terminal-ui-fullscreen--append-entry
                          ui origin-identifier entry :metadata next)))
            (when (gethash origin-identifier (fullscreen-terminal-ui-presentation-records ui))
              (terminal-ui-fullscreen--tag-last ui (list identifier))
              (terminal-ui--remember-finalized ui (list identifier) :note-output-p nil))
            display))))))

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
            (unless (or (terminal-ui-fullscreen--merge-schema-record ui identifier metadata)
                        (and record (member (getf metadata :kind) '(:result :completion :annotation))
                             (terminal-ui-fullscreen--replace-record
                              ui record :identifier identifier :metadata metadata)))
              (let ((restored (when (member (getf metadata :kind) '(:result :completion :annotation))
                                (terminal-ui-fullscreen--restore-outcome ui identifier metadata))))
                (cond
                  (restored
                   (write-string restored output))
                  (entry
                   (write-string (terminal-ui-fullscreen--append-entry
                                  ui identifier entry :metadata metadata)
                                 output)))))))))))

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
