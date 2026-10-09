(in-package #:autolith)

;;;; -- Complete Editable Input History --

(-> conversation-input-history (conversation) list)
(defun conversation-input-history (conversation)
  "Return all editable inputs in chronological order from the maintained sidecar.
A missing sidecar is built once from durable records. Later reads need only the
sidecar and, after an interrupted append, the active segment."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda ()
       (conversation-input-history--ensure-current conversation)
       (mapcar #'copy-seq
               (deque->list (conversation-input-history-entries conversation)))))))

(-> conversation--note-input-history (conversation list) null)
(defun conversation--note-input-history (conversation record)
  "Track the newest editable input sequence independently of sidecar publication."
  (let ((sequence (getf (rest record) :seq)))
    (when (and (conversation-input-history--record-input record)
               (typep sequence '(integer 1)))
      (setf (conversation-input-history-last-sequence conversation)
            (max (conversation-input-history-last-sequence conversation) sequence))))
  nil)

(-> conversation-input-history--record-input (list) (option non-empty-string))
(defun conversation-input-history--record-input (record)
  "Return RECORD's editable text, excluding automatic, summarized and assistant messages."
  (let* ((properties (rest record))
         (input (case (first record)
                  (:message
                   (when (and (eq (getf properties :role) ':user)
                              (not (getf properties :automatic-p))
                              (null (getf properties :summary)))
                     (getf properties :content)))
                  (:user-operation
                   (when (member (getf properties :kind) '(:command :lisp))
                     (getf properties :source))))))
    (and (non-empty-string-p input) input)))

(-> conversation-input-history--header (conversation) list)
(defun conversation-input-history--header (conversation)
  "Identify the source conversation of one complete input-only index."
  (list :conversation-input-history :version 1
        :id (conversation-identifier conversation)
        :created-at (conversation-created-at conversation)))

(-> conversation-input-history--read (conversation) (values t integer boolean))
(defun conversation-input-history--read (conversation)
  "Read a validated input-only index, repairing a torn final form.
Return its input deque, newest indexed sequence and validity flag."
  (handler-case
      (multiple-value-bind (forms incomplete-p)
          (log-read (conversation-input-history-pathname
                     (conversation-pathname conversation)))
        (let ((header (first forms))
              (entries (make-deque))
              (sequence 0))
          (unless (and (eq (first header) ':conversation-input-history)
                       (eql (getf (rest header) :version) 1)
                       (equal (getf (rest header) :id)
                              (conversation-identifier conversation))
                       (eql (getf (rest header) :created-at)
                            (conversation-created-at conversation)))
            (return-from conversation-input-history--read (values nil 0 nil)))
          (dolist (record (rest forms))
            (let ((next (getf (rest record) :seq))
                  (input (getf (rest record) :source)))
              (unless (and (eq (first record) ':input)
                           (typep next '(integer 1))
                           (> next sequence)
                           (< next (conversation-next-sequence conversation))
                           (non-empty-string-p input))
                (return-from conversation-input-history--read (values nil 0 nil)))
              (deque-push-back entries (copy-seq input))
              (setf sequence next)))
          (when incomplete-p
            (sexp-store:log-repair-tail
             (conversation-input-history-pathname
              (conversation-pathname conversation))))
          (values entries sequence t)))
    (error ()
      (values nil 0 nil))))

(-> conversation-input-history--rebuild (conversation) null)
(defun conversation-input-history--rebuild (conversation)
  "Atomically build the complete sidecar once from authoritative durable records."
  (let ((records nil)
        (entries (make-deque))
        (sequence 0))
    (conversation--map-storage-records
     (conversation-pathname conversation)
     (lambda (record)
       (let ((input (conversation-input-history--record-input record)))
         (when input
           (setf sequence (getf (rest record) :seq))
           (deque-push-back entries (copy-seq input))
           (push (list :input :seq sequence :source input) records)))))
    (sexp-store:log-write
     (conversation-input-history-pathname (conversation-pathname conversation))
     (cons (conversation-input-history--header conversation) (nreverse records)))
    (setf (conversation-input-history-entries conversation) entries
          (conversation-input-history-indexed-sequence conversation) sequence
          (conversation-input-history-last-sequence conversation)
          (max sequence (conversation-input-history-last-sequence conversation))
          (conversation-input-history-loaded-p conversation) t))
  nil)

(-> conversation-input-history--append (conversation list) null)
(defun conversation-input-history--append (conversation record)
  "Append an unindexed input in constant work without rewriting earlier inputs."
  (let ((input (conversation-input-history--record-input record))
        (sequence (getf (rest record) :seq)))
    (when (and input
               (> sequence (conversation-input-history-indexed-sequence conversation)))
      (log-append
       (conversation-input-history-pathname (conversation-pathname conversation))
       (list :input :seq sequence :source input)
       :repair-tail-p nil)
      (deque-push-back (conversation-input-history-entries conversation) (copy-seq input))
      (setf (conversation-input-history-indexed-sequence conversation) sequence)))
  nil)

(-> conversation-input-history--load (conversation) null)
(defun conversation-input-history--load (conversation)
  "Load a complete index, rebuilding when it predates the active checkpoint."
  (multiple-value-bind (entries sequence valid-p)
      (conversation-input-history--read conversation)
    (let* ((header (conversation--peek-segment-header
                    (conversation-log-pathname conversation)))
           (checkpoint (getf (rest header) :input-history-last-sequence 0)))
      (if (or (not valid-p) (< sequence checkpoint))
          (conversation-input-history--rebuild conversation)
          (setf (conversation-input-history-entries conversation) entries
                (conversation-input-history-indexed-sequence conversation) sequence
                (conversation-input-history-last-sequence conversation)
                (max sequence (conversation-input-history-last-sequence conversation))
                (conversation-input-history-loaded-p conversation) t))))
  nil)

(-> conversation-input-history--ensure-current (conversation) null)
(defun conversation-input-history--ensure-current (conversation)
  "Load or repair the sidecar under the conversation append and shared source locks.
Flush it before rotating the active segment so crash recovery never needs a
retired segment unless the complete sidecar itself is missing or corrupt."
  (let ((current-p nil))
    (unwind-protect
         (handler-case
             (progn
               (if (conversation-persisted-p conversation)
                   (progn
                     (when (or (not (conversation-input-history-loaded-p conversation))
                               (not (probe-file (conversation-input-history-pathname
                                                 (conversation-pathname conversation)))))
                       (conversation-input-history--load conversation))
                     (when (< (conversation-input-history-indexed-sequence conversation)
                              (conversation-input-history-last-sequence conversation))
                       (conversation--map-segment-records
                        (conversation-pathname conversation)
                        (conversation-log-pathname conversation)
                        (lambda (record)
                          (conversation-input-history--append conversation record)))))
                   (setf (conversation-input-history-loaded-p conversation) t))
               (setf current-p t))
           (error (condition)
             (error 'conversation-invariant-error
                    :message (format nil "Could not maintain editable input history: ~A"
                                     condition)
                    :pathname (conversation-input-history-pathname
                               (conversation-pathname conversation))
                    :sequence (conversation-next-sequence conversation))))
      (unless current-p
        (setf (conversation-input-history-loaded-p conversation) nil))))
  nil)

(-> conversation-input-history--publish (conversation list) null)
(defun conversation-input-history--publish (conversation record)
  "Maintain the input-only sidecar after a durable append under the source lock.
If publication fails, defer repair to the next history read or compaction. The
conversation record is already durable and remains the recovery authority."
  (when (conversation-input-history--record-input record)
    (let ((published-p nil))
      (unwind-protect
           (handler-case
               (progn
                 (unless (and (conversation-input-history-loaded-p conversation)
                              (probe-file (conversation-input-history-pathname
                                           (conversation-pathname conversation))))
                   (conversation-input-history--ensure-current conversation))
                 (conversation-input-history--append conversation record)
                 (setf published-p t))
             (error () nil))
        (unless published-p
          (setf (conversation-input-history-loaded-p conversation) nil)))))
  nil)
