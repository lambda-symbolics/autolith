(in-package #:autolith)

;;;; -- Seekable Editable Input Pages --

(defparameter *conversation-input-history-index-stride* 64
  "Maximum deduplicated inputs between persisted seek points.")

(-> conversation-input-history-index--read (conversation &optional integer) (option list))
(defun conversation-input-history-index--read
    (conversation &optional (expected (conversation-input-history-last-sequence conversation)))
  "Load a current seek manifest and check conversation identity and input sequence."
  (let ((identity (conversation-pathname conversation)))
    (sexp-store:log-index-read
     (conversation-input-history-pathname identity)
     (conversation-input-history-index-pathname identity)
     :stride *conversation-input-history-index-stride*
     :header-epoch (lambda (header) (getf (rest header) :epoch))
     :validate (lambda (metadata header tail)
                 (and (conversation-input-history--header-p conversation header)
                      (equal (getf metadata :id) (conversation-identifier conversation))
                      (eql (getf metadata :created-at) (conversation-created-at conversation))
                      (eql (getf metadata :sequence) expected)
                      (if (zerop expected)
                          (zerop (getf metadata :tail-position))
                          (and (eq (first tail) ':input)
                               (eql (getf (rest tail) :seq) expected)
                               (equal (getf (rest tail) :source) (getf metadata :previous)))))))))

(-> conversation-input-history-index--build (conversation) null)
(defun conversation-input-history-index--build (conversation)
  "Build seek offsets for nonduplicate editable inputs."
  (let* ((identity (conversation-pathname conversation))
         (metadata (list :id (conversation-identifier conversation)
                         :created-at (conversation-created-at conversation)
                         :sequence 0 :previous nil)))
    (setf (conversation-input-history-index-metadata conversation)
          (sexp-store:log-index-rebuild
           (conversation-input-history-pathname identity)
           (conversation-input-history-index-pathname identity)
           :stride *conversation-input-history-index-stride*
           :header-epoch (lambda (header) (getf (rest header) :epoch))
           :leaf-pathname (lambda (block) (conversation-input-history-index-pathname identity block))
           :metadata metadata
           :select (lambda (record)
                     (conversation-maintenance-check)
                     (when (eq (first record) ':input)
                       (let ((input    (getf (rest record) :source))
                             (previous (getf metadata :previous)))
                         (setf (getf metadata :previous) input
                               (getf metadata :sequence) (getf (rest record) :seq))
                         (not (equal input previous))))))))
  nil)

(-> conversation-input-history-index--note (conversation list integer) null)
(defun conversation-input-history-index--note (conversation record position)
  "Index a durable append unless its input repeats the previous one."
  (let* ((identity (conversation-pathname conversation))
         (metadata (copy-list (conversation-input-history-index-metadata conversation)))
         (input    (getf (rest record) :source)))
    (unless metadata
      (error 'conversation-invariant-error :pathname identity :sequence nil
             :message "The input seek index needs repair."))
    (let ((selected-p (not (equal input (getf metadata :previous)))))
      (setf (getf metadata :previous) input
            (getf metadata :sequence) (getf (rest record) :seq)
            (conversation-input-history-index-metadata conversation)
            (sexp-store:log-index-note
             (conversation-input-history-pathname identity)
             (conversation-input-history-index-pathname identity)
             metadata :position position :selected-p selected-p
             :leaf-pathname (lambda (block) (conversation-input-history-index-pathname identity block))))))
  nil)

(-> conversation-input-history-index--page
    (conversation list &key (:skip integer) (:limit integer) (:start (option integer))) list)
(defun conversation-input-history-index--page (conversation metadata &key skip limit start)
  "Read only requested input records, including when long duplicate runs precede them."
  (let* ((total      (getf metadata :count))
         (end        (if start (min total (+ start limit)) (max 0 (- total skip))))
         (begin      (if start start (max 0 (- end limit))))
         (leaf       nil)
         (leaf-block nil)
         (page       nil))
    (loop for index from begin below end
          for block = (floor index *conversation-input-history-index-stride*)
          do (unless (eql leaf-block block)
                (setf leaf (sexp-store:log-index-leaf
                            (conversation-input-history-index-pathname (conversation-pathname conversation) block)
                            metadata block)
                      leaf-block block))
              (let* ((position (nth (mod index *conversation-input-history-index-stride*) leaf))
                     (record   (sexp-store:log-first
                                (conversation-input-history-pathname (conversation-pathname conversation))
                                :start-position position :physical-forms-p t)))
               (unless (and (eq (first record) ':input) (non-empty-string-p (getf (rest record) :source)))
                 (error 'conversation-invariant-error :pathname (conversation-pathname conversation) :sequence nil
                        :message "A requested input record is unavailable."))
               (push (getf (rest record) :source) page)))
    (nreverse page)))

(-> conversation-input-history-index--recent (conversation integer) list)
(defun conversation-input-history-index--recent (conversation limit)
  "Return a bounded retained window from the active chunk, without archive fallback."
  (let ((entries  nil)
        (previous nil)
        (count    0))
    (conversation--map-segment-records
     (conversation-pathname conversation) (conversation-log-pathname conversation)
     (lambda (record)
       (let ((input (conversation-input-history--record-input record)))
         (when (and input (not (equal input previous)))
           (setf previous input)
           (push input entries)
           (incf count)
           (when (> count limit)
             (setf entries (butlast entries))
             (decf count))))))
    (nreverse entries)))

(-> conversation-input-history-index-page
    (conversation &key (:skip integer) (:limit integer) (:start (option integer)))
    (values list integer boolean))
(defun conversation-input-history-index-page (conversation &key skip limit start)
  "Return complete indexed paging, or a truthful local window without archival repair."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (unless (conversation-persisted-p conversation)
      (return-from conversation-input-history-index-page (values nil 0 nil)))
    (call-with-file-lock
     (conversation-picker-source-lock-pathname (conversation-pathname conversation))
     (lambda ()
       (let ((metadata (unless (conversation-maintenance-running-p conversation)
                         (conversation-input-history-index--read conversation))))
         (when metadata
           (handler-case
               (return-from conversation-input-history-index-page
                 (values (conversation-input-history-index--page conversation metadata :skip skip :limit limit :start start)
                         (getf metadata :count) t))
             (error () nil)))
         (setf (conversation-input-history-repair-pending-p conversation) t)
         (let* ((recent (conversation-input-history-index--recent conversation (max limit 64)))
                (total  (length recent))
                (end    (if start (min total (+ start limit)) (max 0 (- total skip))))
                (begin  (if start (min start end) (max 0 (- end limit)))))
           (values (subseq recent begin end) total nil)))))))

(-> conversation-maintenance-run (conversation &key (:check-function (option function))) list)
(defun conversation-maintenance-run (conversation &key check-function)
  "Repair optional projections optimistically with bounded managed-job cancellation.
Archive traversal takes no append/source lock. Only source-validated publication
holds them; concurrent input invalidates that attempt rather than waiting for a scan."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (when (conversation-maintenance-running-p conversation)
      (return-from conversation-maintenance-run (list :status ':busy)))
    (setf (conversation-maintenance-running-p conversation) t))
  (unwind-protect
       (let ((*conversation-maintenance-check-function* check-function))
         (dotimes (retry 3)
           (conversation-maintenance-check)
           (when (conversation-input-history-repair-pending-p conversation)
             (block attempt
               (let ((expected (with-recursive-lock-held ((conversation-append-lock conversation))
                                 (conversation-input-history-last-sequence conversation))))
                 (conversation-input-history--rebuild
                  conversation
                  :call-with-publication
                  (lambda (publish)
                    (with-recursive-lock-held ((conversation-append-lock conversation))
                      (call-with-file-lock
                       (conversation-picker-source-lock-pathname (conversation-pathname conversation))
                       (lambda ()
                         (conversation-maintenance-check)
                         (unless (= expected (conversation-input-history-last-sequence conversation))
                           (return-from attempt))
                         (funcall publish)))))))))
           (with-recursive-lock-held ((conversation-append-lock conversation))
             (setf (conversation-input-history-repair-pending-p conversation)
                   (null (conversation-input-history-index--read conversation))))
           (unless (conversation-input-history-repair-pending-p conversation) (return)))
         (when (conversation-image-artifact-repair-pending-p conversation)
           (conversation-image-artifacts-repair conversation))
         (list :input-complete-p  (not (conversation-input-history-repair-pending-p conversation))
               :images-complete-p (not (conversation-image-artifact-repair-pending-p conversation))))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (setf (conversation-maintenance-running-p conversation) nil
            (conversation-input-history-repair-pending-p conversation)
            (null (conversation-input-history-index--read conversation))
            (conversation-input-history-loaded-p conversation)
            (not (conversation-input-history-repair-pending-p conversation))))))
