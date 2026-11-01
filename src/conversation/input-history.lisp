(in-package #:autolith)

;;;; -- Complete Editable Input History --

;;; Every editable input stays recallable through an input-only sidecar beside
;;; the conversation. The sidecar is the index: nothing of it stays resident.
;;; Reads stream it, keep only what the caller asked for, and rebuilding
;;; streams durable records into a new sidecar.

(-> conversation-map-input-history (conversation function) null)
(defun conversation-map-input-history (conversation function)
  "Call FUNCTION with each editable input of CONVERSATION, oldest first.

Inputs stream from the maintained sidecar. A missing or stale sidecar is built
once from durable records. FUNCTION runs under the conversation's append and
source locks, so it must not append to CONVERSATION; it may leave early with
a nonlocal exit."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda ()
       (conversation-input-history--ensure-current conversation)
       (when (conversation-persisted-p conversation)
         (log-map (lambda (record)
                    (when (eq (first record) ':input)
                      (funcall function (getf (rest record) :source))))
                  (conversation-input-history-pathname (conversation-pathname conversation)))))))
  nil)

(-> conversation-input-history-page
    (conversation &key (:skip (integer 0)) (:limit (integer 0))
                  (:start (option (integer 0))))
    (values list (integer 0)))
(defun conversation-input-history-page (conversation &key (skip 0) limit start)
  "Return a bounded page, oldest first, and the total consecutive-deduplicated count.

By default return up to LIMIT inputs just older than the newest SKIP. With
START, read from that absolute oldest-based index instead. Absolute indices
remain stable when inputs are appended. The sidecar streams twice, once to
count and once to collect; only the requested page is held."
  (flet ((map-recalled (function)
           (let ((previous nil))
             (conversation-map-input-history
              conversation
              (lambda (input)
                (unless (and previous (string= input previous))
                  (funcall function input))
                (setf previous input))))))
    (let ((total 0)
          (index 0)
          (page  nil))
      (map-recalled (lambda (input)
                      (declare (ignore input))
                      (incf total)))
      (let* ((end   (if start (min total (+ start limit)) (- total skip)))
             (begin (or start (max 0 (- end limit)))))
        (when (> end begin)
          (block collect
            (map-recalled (lambda (input)
                            (when (>= index end)
                              (return-from collect))
                            (when (>= index begin)
                              (push input page))
                            (incf index))))))
      (values (nreverse page) total))))

(-> conversation-input-history (conversation) list)
(defun conversation-input-history (conversation)
  "Return all editable inputs of CONVERSATION in chronological order.

The list is built for this call and not retained. Prefer
CONVERSATION-INPUT-HISTORY-PAGE or CONVERSATION-MAP-INPUT-HISTORY when only
part of a long history is needed."
  (let ((inputs nil))
    (conversation-map-input-history conversation (lambda (input) (push input inputs)))
    (nreverse inputs)))

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
  "Return RECORD's editable text, excluding automatic messages and assistant output."
  (let* ((properties (rest record))
         (input      (case (first record)
                       (:message
                        (when (and (eq (getf properties :role) ':user)
                                   (not (getf properties :automatic-p)))
                          (getf properties :content)))
                       (:user-operation
                        (when (member (getf properties :kind) '(:command :lisp))
                          (getf properties :source))))))
    (and (non-empty-string-p input) input)))

(-> conversation-input-history--header (conversation) list)
(defun conversation-input-history--header (conversation)
  "Identify the source conversation of one complete input-only index."
  (list :conversation-input-history :version 1
        :id                         (conversation-identifier conversation)
        :created-at                 (conversation-created-at conversation)))

(-> conversation-input-history--read (conversation) (values integer boolean))
(defun conversation-input-history--read (conversation)
  "Validate the input-only index while streaming it, repairing a torn final form.

Return its newest indexed sequence and validity flag. No input is retained."
  (handler-case
      (let ((pathname (conversation-input-history-pathname (conversation-pathname conversation)))
            (header-p t)
            (sequence 0))
        (multiple-value-bind (position incomplete-p count)
            (log-map
             (lambda (record)
               (if header-p
                   (progn
                     (unless (and (eq (first record) ':conversation-input-history)
                                  (eql (getf (rest record) :version) 1)
                                  (equal (getf (rest record) :id)
                                         (conversation-identifier conversation))
                                  (eql (getf (rest record) :created-at)
                                       (conversation-created-at conversation)))
                       (return-from conversation-input-history--read (values 0 nil)))
                     (setf header-p nil))
                   (let ((next  (getf (rest record) :seq))
                         (input (getf (rest record) :source)))
                     (unless (and (eq (first record) ':input)
                                  (typep next '(integer 1))
                                  (> next sequence)
                                  (< next (conversation-next-sequence conversation))
                                  (non-empty-string-p input))
                       (return-from conversation-input-history--read (values 0 nil)))
                     (setf sequence next))))
             pathname)
          (declare (ignore position))
          (when (zerop count)
            (return-from conversation-input-history--read (values 0 nil)))
          (when incomplete-p
            (sexp-store:log-repair-tail pathname))
          (values sequence t)))
    (error ()
      (values 0 nil))))

(-> conversation-input-history--rebuild (conversation) null)
(defun conversation-input-history--rebuild (conversation)
  "Atomically build the complete sidecar from authoritative durable records.

Records stream straight into the new sidecar, so rebuilding holds no history."
  (let ((sequence 0))
    (log-write-streaming
     (conversation-input-history-pathname (conversation-pathname conversation))
     (lambda (emit)
       (funcall emit (conversation-input-history--header conversation))
       (conversation--map-storage-records
        (conversation-pathname conversation)
        (lambda (record)
          (let ((input (conversation-input-history--record-input record)))
            (when input
              (setf sequence (getf (rest record) :seq))
              (funcall emit (list :input :seq sequence :source input))))))))
    (setf (conversation-input-history-indexed-sequence conversation) sequence
          (conversation-input-history-last-sequence conversation)
          (max sequence (conversation-input-history-last-sequence conversation))
          (conversation-input-history-loaded-p conversation) t))
  nil)

(-> conversation-input-history--append (conversation list) null)
(defun conversation-input-history--append (conversation record)
  "Append an unindexed input in constant work without rewriting earlier inputs."
  (let ((input    (conversation-input-history--record-input record))
        (sequence (getf (rest record) :seq)))
    (when (and input
               (> sequence (conversation-input-history-indexed-sequence conversation)))
      (log-append
       (conversation-input-history-pathname (conversation-pathname conversation))
       (list :input :seq sequence :source input)
       :repair-tail-p nil)
      (setf (conversation-input-history-indexed-sequence conversation) sequence)))
  nil)

(-> conversation-input-history--load (conversation) null)
(defun conversation-input-history--load (conversation)
  "Validate the index, rebuilding it when it predates the active checkpoint."
  (multiple-value-bind (sequence valid-p)
      (conversation-input-history--read conversation)
    (let* ((header     (conversation--peek-segment-header
                        (conversation-log-pathname conversation)))
           (checkpoint (getf (rest header) :input-history-last-sequence 0)))
      (if (or (not valid-p) (< sequence checkpoint))
          (conversation-input-history--rebuild conversation)
          (setf (conversation-input-history-indexed-sequence conversation) sequence
                (conversation-input-history-last-sequence conversation)
                (max sequence (conversation-input-history-last-sequence conversation))
                (conversation-input-history-loaded-p conversation) t))))
  nil)

(-> conversation-input-history--ensure-current (conversation) null)
(defun conversation-input-history--ensure-current (conversation)
  "Validate or repair the sidecar under the conversation append and shared source locks.
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
                    :message  (format nil "Could not maintain editable input history: ~A"
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
