(in-package #:autolith)

;;;; -- Seekable Editable Input Pages --

(defparameter *conversation-input-history-index-stride* 64
  "Maximum deduplicated inputs between persisted seek points.")

(-> conversation-input-history-index--size (pathname) (integer 0))
(defun conversation-input-history-index--size (pathname)
  "Return the byte length of PATHNAME without decoding its contents."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (file-length stream)))

(-> conversation-input-history-index--first (pathname &optional (integer 0)) list)
(defun conversation-input-history-index--first (pathname &optional (position 0))
  "Read just one complete log record at POSITION through the storage seek API."
  (block first-record
    (log-map (lambda (record) (return-from first-record record)) pathname :start-position position)
    nil))

(-> conversation-input-history-index--manifest (conversation list) pathname)
(defun conversation-input-history-index--manifest (conversation metadata)
  "Publish METADATA after the input log and its sparse leaf are durable."
  (snapshot-write (conversation-input-history-index-pathname (conversation-pathname conversation)) metadata))

(-> conversation-input-history-index--point
    (conversation list &key (:index integer) (:position integer)) pathname)
(defun conversation-input-history-index--point (conversation metadata &key index position)
  "Maintain one fixed-size leaf of direct deduplicated input offsets."
  (let* ((block    (floor index *conversation-input-history-index-stride*))
         (base     (* block *conversation-input-history-index-stride*))
         (pathname (conversation-input-history-index-pathname (conversation-pathname conversation) block))
         (point    (if (= index base)
                       (list :epoch (getf metadata :epoch) :index base :positions nil)
                       (snapshot-read pathname))))
    (unless (and (equal (getf point :epoch) (getf metadata :epoch))
                 (eql (getf point :index) base)
                 (= (length (getf point :positions)) (- index base)))
      (error 'conversation-invariant-error :pathname pathname :sequence nil
             :message "The input offset leaf is stale."))
    (setf (getf point :positions) (append (getf point :positions) (list position)))
    (snapshot-write pathname point)))

(-> conversation-input-history-index--read (conversation &optional integer) (option list))
(defun conversation-input-history-index--read
    (conversation &optional (expected (conversation-input-history-last-sequence conversation)))
  "Validate a constant-size manifest against source identity, epoch, length and tail."
  (handler-case
      (let* ((identity (conversation-pathname conversation))
             (pathname (conversation-input-history-pathname identity))
             (metadata (snapshot-read (conversation-input-history-index-pathname identity)))
             (header   (conversation-input-history-index--first pathname))
             (tail     (and (plusp (getf metadata :sequence 0))
                            (conversation-input-history-index--first pathname (getf metadata :tail-position)))))
        (when (and (equal (getf metadata :id) (conversation-identifier conversation))
                   (eql (getf metadata :created-at) (conversation-created-at conversation))
                   (non-empty-string-p (getf metadata :epoch))
                   (equal (getf metadata :epoch) (getf (rest header) :epoch))
                   (eql (getf metadata :stride) *conversation-input-history-index-stride*)
                   (typep (getf metadata :count) '(integer 0))
                   (eql (getf metadata :sequence) expected)
                   (eql (getf metadata :bytes) (conversation-input-history-index--size pathname))
                   (or (zerop expected)
                       (and (eq (first tail) ':input)
                            (eql (getf (rest tail) :seq) expected)
                            (equal (getf (rest tail) :source) (getf metadata :previous)))))
          metadata))
    (error () nil)))

(-> conversation-input-history-index--build (conversation) null)
(defun conversation-input-history-index--build (conversation)
  "Build fixed-size offset leaves during explicit repair, never during page lookup."
  (let* ((identity  (conversation-pathname conversation))
         (pathname  (conversation-input-history-pathname identity))
         (header    (conversation-input-history-index--first pathname))
         (metadata  (list :id         (conversation-identifier conversation)
                          :created-at (conversation-created-at conversation)
                          :epoch      (getf (rest header) :epoch)
                          :stride     *conversation-input-history-index-stride*
                          :count 0 :sequence 0 :previous nil :tail-position 0))
         (base      0)
         (positions nil))
    (unless (non-empty-string-p (getf metadata :epoch))
      (error 'conversation-invariant-error :pathname pathname :sequence nil
             :message "The input cache predates seekable input pages."))
    (labels ((flush-leaf ()
               "Publish at most one fixed-size leaf and release its offset buffer."
               (when positions
                 (snapshot-write
                  (conversation-input-history-index-pathname identity (floor base *conversation-input-history-index-stride*))
                  (list :epoch (getf metadata :epoch) :index base :positions (reverse positions)))
                 (incf base (length positions))
                 (setf positions nil))))
      (multiple-value-bind (position incomplete count)
          (log-map
           (lambda (record start end)
             (declare (ignore end))
             (conversation-maintenance-check)
             (when (eq (first record) ':input)
               (let ((input (getf (rest record) :source)))
                 (unless (equal input (getf metadata :previous))
                   (push start positions)
                   (incf (getf metadata :count))
                   (when (= (length positions) *conversation-input-history-index-stride*) (flush-leaf)))
                 (setf (getf metadata :previous) input
                       (getf metadata :sequence) (getf (rest record) :seq)
                       (getf metadata :tail-position) start))))
           pathname :with-positions-p t)
        (declare (ignore count))
        (when incomplete
          (error 'conversation-invariant-error :pathname pathname :sequence nil
                 :message "The input cache has an incomplete tail."))
        (setf (getf metadata :bytes) position))
      (flush-leaf))
    (conversation-input-history-index--manifest conversation metadata)
    (setf (conversation-input-history-index-metadata conversation) metadata))
  nil)

(-> conversation-input-history-index--note (conversation list integer) null)
(defun conversation-input-history-index--note (conversation record position)
  "Maintain a count and at most one sparse point after one durable input append."
  (let* ((metadata (copy-list (conversation-input-history-index-metadata conversation)))
         (input    (getf (rest record) :source)))
    (unless metadata
      (error 'conversation-invariant-error :pathname (conversation-pathname conversation) :sequence nil
             :message "The input seek index needs repair."))
    (unless (equal input (getf metadata :previous))
      (conversation-input-history-index--point conversation metadata :index (getf metadata :count) :position position)
      (incf (getf metadata :count)))
    (setf (getf metadata :previous) input
          (getf metadata :sequence) (getf (rest record) :seq)
          (getf metadata :tail-position) position
          (getf metadata :bytes)
          (conversation-input-history-index--size
           (conversation-input-history-pathname (conversation-pathname conversation))))
    (conversation-input-history-index--manifest conversation metadata)
    (setf (conversation-input-history-index-metadata conversation) metadata))
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
               (setf leaf (snapshot-read (conversation-input-history-index-pathname (conversation-pathname conversation) block))
                     leaf-block block)
               (unless (and (equal (getf leaf :epoch) (getf metadata :epoch))
                            (eql (getf leaf :index) (* block *conversation-input-history-index-stride*)))
                 (error 'conversation-invariant-error :pathname (conversation-pathname conversation) :sequence nil
                        :message "The requested input seek leaf is stale.")))
             (let* ((position (nth (mod index *conversation-input-history-index-stride*) (getf leaf :positions)))
                    (record   (and (typep position '(integer 0))
                                   (conversation-input-history-index--first
                                    (conversation-input-history-pathname (conversation-pathname conversation)) position))))
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
