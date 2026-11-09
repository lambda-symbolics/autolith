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

(-> terminal-ui-fullscreen--record-content
    (fullscreen-terminal-ui list integer &key (:entry (or string list)))
    (values string string list list))
(defun terminal-ui-fullscreen--record-content (ui record width &key (entry nil entry-p))
  "Lay out resident RECORD at WIDTH, optionally reusing its initial ENTRY."
  (when (getf record :markdown)
    (return-from terminal-ui-fullscreen--record-content
      (terminal-ui-fullscreen--markdown-content ui record width)))
  (let* ((layout      (getf record :entry-layout))
         (entry       (if entry-p entry
                          (if layout (funcall layout width) (getf record :entry))))
         (body-layout (getf record :body-layout))
         (body        (when (and (eq (getf record :kind) ':call) (getf record :retain-output-p))
                        (if (and body-layout (not entry-p))
                            (funcall body-layout width)
                            (getf record :body))))
         (prefix      (getf record :prefix "")))
    (multiple-value-bind (text display regions)
        (terminal-ui--finalized-content
         ui (if (eq (getf record :kind) ':call)
                (terminal-ui-fullscreen--record-entry entry (getf record :state) body)
                entry)
         :group (getf record :group))
      (values (concatenate 'string prefix text) (concatenate 'string prefix display)
              (termdown:shift-regions regions (length prefix)) nil))))

(-> terminal-ui-fullscreen--record-source-characters (list) (integer 0))
(defun terminal-ui-fullscreen--record-source-characters (record)
  "Charge logical sources retained by RECORD to its bounded viewport chunk."
  (+ (or (getf record :source-characters) 0)
     (or (getf record :body-source-characters) 0)
     (let ((markdown (getf record :markdown)))
       (if markdown
           (+ (length (getf markdown :source))
              (loop for span in (getf markdown :header)
                    sum (length (terminal-span-text span))))
           0))))

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

(-> terminal-ui-fullscreen--replace-record
    (fullscreen-terminal-ui list &key (:identifier t) (:metadata list)) boolean)
(defun terminal-ui-fullscreen--replace-record (ui record &key identifier metadata)
  "Replace RECORD's invocation with its latest source-backed outcome."
  (let ((next (copy-list record)))
    (setf (getf next :identifiers) (cons identifier (getf record :identifiers))
          (getf next :job-key) (or (getf metadata :job-key) (getf record :job-key)))
    (dolist (key '(:state :retain-output-p :body :body-layout :body-source-characters))
      (setf (getf next key) (getf metadata key)))
    (multiple-value-bind (text display regions)
        (terminal-ui-fullscreen--record-content
         ui next (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui)))
      (when (getf next :body-layout)
        (setf (getf next :body) nil))
      ;; Index before replacement can evict this chunk and release its sources.
      (terminal-ui-fullscreen--index-record ui next)
      (if (clinedi:transcript-viewport-replace
           (fullscreen-terminal-ui-viewport ui) (getf record :tag)
           :text text :display display :regions regions
           :layout-function   (terminal-ui-fullscreen--record-layout-function ui next)
           :source-characters (terminal-ui-fullscreen--record-source-characters next))
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
