(in-package #:autolith)

;;;; -- Complete Editable Input History --

(-> conversation-map-input-history (conversation function) null)
(defun conversation-map-input-history (conversation function)
  "Call FUNCTION with each editable input of CONVERSATION, oldest first.

Inputs stream from the maintained sidecar, or from authoritative records when
cache maintenance fails. FUNCTION runs under the conversation's append and
source locks, so it must not append to CONVERSATION; it may leave early with
a nonlocal exit."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda ()
       (when (conversation-persisted-p conversation)
         (let* ((pathname (conversation-input-history-pathname
                           (conversation-pathname conversation)))
                (cached-p
                  (handler-case
                      (progn
                        (conversation-input-history--ensure-current conversation)
                        ;; A loaded index can become unreadable between history calls.
                        (with-open-file (stream pathname :direction ':input)
                          (declare (ignorable stream)))
                        t)
                    (error ()
                      (setf (conversation-input-history-loaded-p conversation) nil)))))
           (if cached-p
               (log-map (lambda (record)
                          (when (eq (first record) ':input)
                            (funcall function (getf (rest record) :source))))
                        pathname)
               (conversation--map-storage-records
                (conversation-pathname conversation)
                (lambda (record)
                  (let ((input (conversation-input-history--record-input record)))
                    (when input
                      (funcall function input)))))))))))
  nil)

(-> conversation-input-history--map-page-inputs (conversation function function) null)
(defun conversation-input-history--map-page-inputs (conversation function reset-function)
  "Stream inputs to FUNCTION under the caller's append and shared source locks.
Reset tentative results before falling back to authoritative records."
  (when (conversation-persisted-p conversation)
    (let ((cached-p
            (handler-case
                (progn
                  (unless (conversation-input-history--ensure-current
                           conversation :input-function function :reset-function reset-function)
                    (log-map (lambda (record)
                               (when (eq (first record) ':input)
                                 (funcall function (getf (rest record) :source))))
                             (conversation-input-history-pathname
                              (conversation-pathname conversation))))
                  t)
              (error ()
                (setf (conversation-input-history-loaded-p conversation) nil)))))
      (unless cached-p
        (funcall reset-function)
        (conversation--map-storage-records
         (conversation-pathname conversation)
         (lambda (record)
           (let ((input (conversation-input-history--record-input record)))
             (when input (funcall function input))))))))
  nil)

(-> conversation-input-history-page
    (conversation &key (:skip (integer 0)) (:limit (integer 0))
                  (:start (option (integer 0))))
    (values list (integer 0)))
(defun conversation-input-history-page (conversation &key (skip 0) limit start)
  "Return an oldest-first page and the total count, merging consecutive duplicates.

Skip the newest SKIP inputs and return up to LIMIT earlier inputs. If START is
supplied, use that absolute index from the oldest input instead. Appending inputs
does not change existing indices. Collect newest or absolute pages while counting
and validating the cold sidecar in one pass. Relative older pages need a second
pass, keeping only the requested page and one preceding input in memory."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda ()
       (let ((total 0)
             (previous nil)
             (page nil)
             (tail nil)
             (size 0))
         (labels ((reset-page ()
                    (setf total 0 previous nil page nil tail nil size 0))

                  (collect-input (input)
                    (unless (and previous (string= input previous))
                      (when (and (plusp limit)
                                 (if start
                                     (<= start total (1- (+ start limit)))
                                     (zerop skip)))
                        (let ((cell (list input)))
                          (if tail (setf (rest tail) cell) (setf page cell))
                          (setf tail cell)
                          (incf size)
                          (when (> size limit)
                            (setf page (rest page))
                            (decf size))))
                      (incf total))
                    (setf previous input)))
           (conversation-input-history--map-page-inputs
            conversation #'collect-input #'reset-page)
           (when (and (not start) (plusp skip) (plusp limit))
             (let* ((end (- total skip))
                    (begin (max 0 (- end limit)))
                    (index 0))
               (setf previous nil)
               (when (> end begin)
                 (block collect
                   (conversation-input-history--map-page-inputs
                    conversation
                    (lambda (input)
                      (unless (and previous (string= input previous))
                        (when (>= index end) (return-from collect))
                        (when (>= index begin) (push input page))
                        (incf index))
                      (setf previous input))
                    (lambda () (setf index 0 previous nil page nil)))))
               (setf page (nreverse page))))
           (values page total)))))))

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

(-> conversation-input-history--read
    (conversation &key (:input-function (option function))) (values integer boolean))
(defun conversation-input-history--read (conversation &key input-function)
  "Validate the input-only index while streaming it, repairing a torn final form.

Return its newest indexed sequence and validity flag. Call INPUT-FUNCTION with
each validated input; its caller must discard tentative results if validation fails."
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
                      (setf sequence next)
                      (when input-function (funcall input-function input)))))
             pathname)
          (declare (ignore position))
          (when (zerop count)
            (return-from conversation-input-history--read (values 0 nil)))
          (when incomplete-p
            (sexp-store:log-repair-tail pathname))
          (values sequence t)))
    (error ()
      (values 0 nil))))

(-> conversation-input-history--rebuild
    (conversation &key (:input-function (option function))) null)
(defun conversation-input-history--rebuild (conversation &key input-function)
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
                (funcall emit (list :input :seq sequence :source input))
                (when input-function (funcall input-function input))))))))
    (setf (conversation-input-history-indexed-sequence conversation) sequence
          (conversation-input-history-last-sequence conversation)
          (max sequence (conversation-input-history-last-sequence conversation))
          (conversation-input-history-loaded-p conversation) t))
  nil)

(-> conversation-input-history--append
    (conversation list &key (:input-function (option function))) null)
(defun conversation-input-history--append (conversation record &key input-function)
  "Append an unindexed input in constant work without rewriting earlier inputs."
  (let ((input    (conversation-input-history--record-input record))
        (sequence (getf (rest record) :seq)))
    (when (and input
               (> sequence (conversation-input-history-indexed-sequence conversation)))
      (log-append
       (conversation-input-history-pathname (conversation-pathname conversation))
       (list :input :seq sequence :source input)
       :repair-tail-p nil)
      (setf (conversation-input-history-indexed-sequence conversation) sequence)
      (when input-function (funcall input-function input))))
  nil)

(-> conversation-input-history--load
    (conversation &key (:input-function (option function)) (:reset-function (option function)))
    null)
(defun conversation-input-history--load (conversation &key input-function reset-function)
  "Validate the index, rebuilding it when it predates the active checkpoint.
Stream inputs to INPUT-FUNCTION, resetting its tentative results before rebuilding."
  (multiple-value-bind (sequence valid-p)
      (conversation-input-history--read conversation :input-function input-function)
    (let* ((header     (conversation--peek-segment-header
                        (conversation-log-pathname conversation)))
           (checkpoint (getf (rest header) :input-history-last-sequence 0)))
      (if (or (not valid-p) (< sequence checkpoint))
          (progn
            (when reset-function (funcall reset-function))
            (conversation-input-history--rebuild conversation :input-function input-function))
          (setf (conversation-input-history-indexed-sequence conversation) sequence
                (conversation-input-history-last-sequence conversation)
                (max sequence (conversation-input-history-last-sequence conversation))
                (conversation-input-history-loaded-p conversation) t))))
  nil)

(-> conversation-input-history--ensure-current
    (conversation &key (:input-function (option function)) (:reset-function (option function)))
    boolean)
(defun conversation-input-history--ensure-current
    (conversation &key input-function reset-function)
  "Validate or repair the sidecar under the conversation append and shared source locks.
Flush it before rotating the active segment so crash recovery never needs a
retired segment unless the complete sidecar itself is missing or corrupt.
Return true when all current inputs were streamed to INPUT-FUNCTION during loading."
  (let ((current-p nil)
        (mapped-p nil))
    (unwind-protect
         (handler-case
             (progn
               (if (conversation-persisted-p conversation)
                   (progn
                     (when (or (not (conversation-input-history-loaded-p conversation))
                               (not (probe-file (conversation-input-history-pathname
                                                 (conversation-pathname conversation)))))
                       (conversation-input-history--load
                        conversation :input-function input-function :reset-function reset-function)
                       (setf mapped-p (not (null input-function))))
                     (when (< (conversation-input-history-indexed-sequence conversation)
                              (conversation-input-history-last-sequence conversation))
                       (conversation--map-segment-records
                        (conversation-pathname conversation)
                        (conversation-log-pathname conversation)
                        (lambda (record)
                          (conversation-input-history--append
                           conversation record :input-function (when mapped-p input-function))))))
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
        (setf (conversation-input-history-loaded-p conversation) nil)))
    mapped-p))

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
