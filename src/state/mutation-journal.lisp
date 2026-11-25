(in-package #:autolith)

;;;; -- Incremental Mutation Journal Appends --

(defclass mutation-journal-cursor ()
  ((pathname
    :initarg       :pathname
    :reader        mutation-journal-cursor-pathname
    :type          pathname
    :documentation "The validated journal is at this pathname.")
   (status
    :initarg       :status
    :reader        mutation-journal-cursor-status
    :type          platform-file-status
    :documentation "The journal had this status after the last append.")
   (position
    :initarg       :position
    :reader        mutation-journal-cursor-position
    :type          (integer 0)
    :documentation "This is the seek position after the last complete form."))
  (:documentation "This cursor contains the validated prefix position of one mutation journal."))

(defvar *mutation-journal-cursor-cache* (make-hash-table :test #'eq :synchronized t)
  "This cache contains one immutable validated journal cursor.")

(-> mutation-journal--start-position (pathname (option platform-file-status)) (integer 0))
(defun mutation-journal--start-position (pathname status)
  "Return the validated journal prefix position for PATHNAME and STATUS."
  (let ((cursor (gethash ':current *mutation-journal-cursor-cache*)))
    (if (and cursor status
             (equal pathname (mutation-journal-cursor-pathname cursor))
             (platform-file-status-same-object-p status (mutation-journal-cursor-status cursor))
             (<= (mutation-journal-cursor-position cursor) (platform-file-status-size status))
             (or (< (mutation-journal-cursor-position cursor) (platform-file-status-size status))
                 (and (= (platform-file-status-modification-time status)
                         (platform-file-status-modification-time (mutation-journal-cursor-status cursor)))
                      (= (platform-file-status-change-time status)
                         (platform-file-status-change-time (mutation-journal-cursor-status cursor))))))
        (mutation-journal-cursor-position cursor)
        0)))

(-> mutation-journal--append (pathname list) pathname)
(defun mutation-journal--append (pathname entry)
  "Append ENTRY after validating the new journal suffix.

Replaced or shortened journals are scanned from the beginning. Torn tails
are repaired by atomic publication of the complete forms and ENTRY."
  (call-with-file-lock
   (readable-state-lock-pathname pathname "mutation-journal.lock")
   (lambda ()
     (let* ((status     (platform-path-status *platform* pathname :follow-links-p t))
            (start      (mutation-journal--start-position pathname status))
            (complete-p nil))
       (unwind-protect
            (progn
              (multiple-value-bind (position incomplete-p)
                  (log-map (constantly nil) pathname :start-position start)
                (declare (ignore position))
                (if incomplete-p
                    (log-write-streaming
                     pathname
                     (lambda (emit)
                       (log-map emit pathname :physical-forms-p t)
                       (funcall emit entry)))
                    (log-append pathname entry :repair-tail-p nil)))
              (let ((current (platform-path-status *platform* pathname :follow-links-p t)))
                (setf (gethash ':current *mutation-journal-cursor-cache*)
                      (when current
                        (make-instance 'mutation-journal-cursor
                                       :pathname pathname :status current
                                       :position (platform-file-status-size current)))))
              (setf complete-p t))
         (unless complete-p
           (remhash ':current *mutation-journal-cursor-cache*))))))
  pathname)
