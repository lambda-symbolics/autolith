(in-package #:autolith)

;;;; -- Cutoff Capture and Carry-Forward --

(defclass conversation-compaction ()
  ((conversation
    :initarg :conversation
    :reader conversation-compaction--conversation
    :documentation "The conversation whose projection was captured.")
   (view
    :initarg :view
    :reader conversation-compaction-view
    :documentation "Detached request view of the captured durable items.")
   (source-items
    :initarg :source-items
    :reader conversation-compaction--source-items
    :documentation "Original item identities used to detect chronological arrivals.")
   (plan
    :initarg :plan
    :reader conversation-compaction--plan
    :documentation "Pure transcript carry-forward plan for the captured projection.")
   (cutoff-sequence
    :initarg :cutoff-sequence
    :reader conversation-compaction-cutoff-sequence
    :documentation "Last durable record sequence included in the request snapshot."))
  (:documentation "A detached compaction capture, materialized only at publication."))

(-> conversation-compaction--repaired-output-p (json-object) boolean)
(defun conversation-compaction--repaired-output-p (output)
  "Recognize the harness's replaceable interrupted/unknown output."
  (equal (json-get output "output") *conversation-interrupted-tool-output*))

(-> conversation-compaction--repair-output
    (clinker-transcript:missing-output-repair) json-object)
(defun conversation-compaction--repair-output (repair)
  "Return a truthful correlated output without executing or persisting a tool."
  (function-call-output-item
   (clinker-transcript:missing-output-repair-call-id repair)
   *conversation-interrupted-tool-output*))

(-> conversation-compaction--copy-metadata
    (clinker-transcript:projection clinker-transcript:projection list) null)
(defun conversation-compaction--copy-metadata (source target items)
  "Copy family and portable-handoff metadata for ITEMS from SOURCE to TARGET."
  (dolist (key '(:family :handoff-family))
    (let ((from (clinker-transcript:projection-metadata-table source key))
          (to (clinker-transcript:projection-metadata-table target key)))
      (dolist (item items)
        (multiple-value-bind (value present-p) (gethash item from)
          (when present-p
            (setf (gethash item to) value))))))
  nil)

(-> conversation-compaction-capture (conversation) conversation-compaction)
(defun conversation-compaction-capture (conversation)
  "Capture a fixed durable cutoff before any compaction provider request.

The request view contains pure protocol repairs; the authoritative plan keeps
original calls so actual arrivals take precedence when publishing."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((items (conversation-input-items-for-request
                   conversation :include-ephemeral-p nil))
           (view (conversation-compaction-summary-view conversation items))
           (plan (clinker-transcript:make-compaction-plan
                  (conversation-projection view)
                  :repaired-output-p #'conversation-compaction--repaired-output-p)))
      (setf (conversation-input-items view)
            (clinker-transcript:reconciliation-items
             (clinker-transcript:reconcile-items
              items :repaired-output-p #'conversation-compaction--repaired-output-p)
             :repair-output #'conversation-compaction--repair-output))
      (make-instance 'conversation-compaction
                     :conversation conversation
                     :view view
                     :source-items items
                     :plan plan
                     :cutoff-sequence (1- (conversation-next-sequence conversation))))))

(-> conversation-compaction--arrivals (conversation conversation-compaction) list)
(defun conversation-compaction--arrivals (conversation compaction)
  "Return chronological durable arrivals, rejecting a replaced source projection."
  (let ((source (conversation-compaction--source-items compaction))
        (current (conversation-input-items-for-request
                  conversation :include-ephemeral-p nil)))
    (unless (and (eq conversation (conversation-compaction--conversation compaction))
                 (<= (conversation-compaction-cutoff-sequence compaction)
                     (1- (conversation-next-sequence conversation)))
                 (<= (length source) (length current))
                 (every #'eq source (subseq current 0 (length source))))
      (error 'conversation-invariant-error
             :message "The conversation projection changed after compaction capture."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (nthcdr (length source) current)))

(-> conversation-compaction--serialize-item
    (clinker-transcript:projection json-object) list)
(defun conversation-compaction--serialize-item (projection item)
  "Encode ITEM and its family metadata as one portable carry-forward row."
  (let ((row (list :wire-json (json-encode item))))
    (dolist (key '(:family :handoff-family))
      (multiple-value-bind (value present-p)
          (gethash item (clinker-transcript:projection-metadata-table projection key))
        (when present-p
          (setf (getf row key) value))))
    row))

(-> conversation-compaction--publish
    (conversation &key (:compaction conversation-compaction)
                       (:replacement-items list) (:record list)
                       (:unfinished-work list))
    list)
(defun conversation-compaction--publish
    (conversation &key compaction replacement-items record unfinished-work)
  "Validate carry-forward state, publish one checkpoint, then install it.

The cutoff describes the provider snapshot. THROUGH-SEQ covers the commit
boundary, including arrivals encoded in CARRY-FORWARD, as required by chunks."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (when (and unfinished-work
               (not (and (task-unfinished-work-snapshot-p unfinished-work)
                         (equal (getf unfinished-work :owner-conversation)
                                (conversation-identifier conversation)))))
      (error 'conversation-invariant-error
             :message "The compaction job snapshot is invalid or belongs to another conversation."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (let* ((arrivals (conversation-compaction--arrivals conversation compaction))
           (projection
             (clinker-transcript:compaction-plan-projection
              (conversation-compaction--plan compaction)
              :replacement-items replacement-items
              :additional-items arrivals
              :repair-output #'conversation-compaction--repair-output))
           (items (clinker-transcript:projection-items projection))
           (ephemeral-items
             (mapcar (lambda (entry) (getf entry :item))
                     (conversation-ephemeral-input-entries conversation))))
      (conversation-compaction--copy-metadata
       (conversation-projection conversation) projection items)
      (setf (getf (rest record) :through-seq)
            (1- (conversation-next-sequence conversation))
            (getf (rest record) :cutoff-seq)
            (conversation-compaction-cutoff-sequence compaction)
            (getf (rest record) :carry-forward)
            (mapcar (lambda (item)
                      (conversation-compaction--serialize-item projection item))
                    (nthcdr (length replacement-items) items))
            (getf (rest record) :unfinished-work) unfinished-work)
      ;; Prepare all state before durable publication, including transient items.
      (clinker-transcript:projection-replace
       projection (append items ephemeral-items))
      (conversation-compaction--copy-metadata
       (conversation-projection conversation) projection ephemeral-items)
      (when (eq (first record) :native-compaction)
        (setf (gethash (first replacement-items)
                       (clinker-transcript:projection-metadata-table projection ':family))
              (getf (rest record) :family)
              (gethash (second replacement-items)
                       (clinker-transcript:projection-metadata-table projection ':handoff-family))
              (getf (rest record) :family)))
      (sb-sys:without-interrupts
        (let ((published (conversation-append-record conversation record)))
          (setf (slot-value conversation 'projection) projection
                (conversation-turn-state conversation) nil
                (conversation-last-total-tokens conversation) 0)
          published)))))

(-> conversation-append-summary
    (conversation string &key (:compaction (option conversation-compaction))
                              (:unfinished-work list))
    list)
(defun conversation-append-summary (conversation content &key compaction unfinished-work)
  "Persist CONTENT with captured protocol context and arrivals as one checkpoint."
  (conversation-compaction--publish
   conversation
   :compaction (or compaction (conversation-compaction-capture conversation))
   :replacement-items (list (conversation-summary-item content))
   :record (list :summary :content content)
   :unfinished-work unfinished-work))

(-> conversation-append-native-compaction
    (conversation json-object &key (:family keyword) (:summary string)
                                   (:compaction (option conversation-compaction))
                                   (:unfinished-work list))
    list)
(defun conversation-append-native-compaction
    (conversation item &key family summary compaction unfinished-work)
  "Persist native ITEM, portable SUMMARY, and carry-forward state atomically."
  (native-compaction-item-canonicalize item)
  (unless (and (keywordp family) (native-compaction-item-p item)
               (non-empty-string-p summary))
    (error 'conversation-invariant-error
           :message "A native compaction checkpoint is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (conversation-compaction--publish
   conversation
   :compaction (or compaction (conversation-compaction-capture conversation))
   :replacement-items (list item (conversation-summary-item summary))
   :record (list :native-compaction :family family
                 :wire-json (json-encode item) :summary summary)
   :unfinished-work unfinished-work))


;;;; -- Checkpoint Replay --

(-> conversation-compaction--restore
    (conversation list clinker-transcript:projection) null)
(defun conversation-compaction--restore (conversation properties projection)
  "Validate and restore carry-forward items before installing PROJECTION."
  (when (and (member :carry-forward properties)
             (not (member :cutoff-seq properties)))
    (conversation--record-error
     conversation properties "Carry-forward state has no compaction cutoff."))
  (let ((work (getf properties :unfinished-work)))
    (when (and work
               (not (and (task-unfinished-work-snapshot-p work)
                         (equal (getf work :owner-conversation)
                                (conversation-identifier conversation)))))
      (conversation--record-error
       conversation properties "A compaction job snapshot is invalid.")))
  (when (member :cutoff-seq properties)
    (let ((cutoff (getf properties :cutoff-seq))
          (through (getf properties :through-seq)))
      (unless (and (integerp cutoff) (integerp through) (<= 0 cutoff through)
                   (= through (1- (getf properties :seq))))
        (conversation--record-error
         conversation properties "A compaction cutoff is outside its checkpoint."))))
  (handler-case
      (dolist (row (getf properties :carry-forward))
      (unless (and (listp row) (list-length row) (evenp (length row))
                   (stringp (getf row :wire-json))
                   (let ((keys (loop for (key value) on row by #'cddr collect key)))
                     (and (= (length keys) (length (remove-duplicates keys)))
                          (every (lambda (key)
                                   (member key '(:wire-json :family :handoff-family))) keys))))
          (conversation--record-error
           conversation properties "A compaction carry-forward row is invalid."))
        (let ((item (json-decode (getf row :wire-json))))
          (unless (typep item 'json-object)
            (conversation--record-error
             conversation properties "A compaction carry-forward item is not an object."))
          (native-compaction-item-canonicalize item)
          (clinker-transcript:projection-append projection item)
          (dolist (key '(:family :handoff-family))
            (when (member key row)
            (unless (or (null (getf row key)) (keywordp (getf row key)))
                (conversation--record-error
                 conversation properties "A carry-forward family is invalid."))
              (setf (gethash item
                             (clinker-transcript:projection-metadata-table projection key))
                    (getf row key))))))
    (conversation-error (condition)
      (error condition))
    (error ()
      (conversation--record-error
       conversation properties "A compaction carry-forward payload is invalid.")))
  (handler-case
      (let ((reconciliation
              (clinker-transcript:reconcile-items
               (clinker-transcript:projection-items projection)
               :repaired-output-p #'conversation-compaction--repaired-output-p)))
        (when (clinker-transcript:reconciliation-repairs reconciliation)
          (conversation--record-error
           conversation properties "A compaction checkpoint has unresolved calls.")))
    (clinker-transcript:projection-error ()
      (conversation--record-error
       conversation properties "A compaction checkpoint has invalid correlations.")))
  (setf (slot-value conversation 'projection) projection
        (conversation-turn-state conversation) nil
        (conversation-last-total-tokens conversation) 0)
  nil)

(defmethod conversation--project-record
    ((kind (eql :summary)) conversation properties)
  "Restore a portable checkpoint and its validated chronological carry-forward."
  (let ((content (getf properties :content)))
    (unless (stringp content)
      (conversation--record-error
       conversation properties "A persisted summary checkpoint has invalid content."))
    (conversation-compaction--restore
     conversation properties
     (clinker-transcript:make-projection :items (list (conversation-summary-item content))))))

(defmethod conversation--project-record
    ((kind (eql :native-compaction)) conversation properties)
  "Restore native family privacy, portable handoff, and carry-forward context."
  (let ((family (getf properties :family))
        (wire-json (getf properties :wire-json))
        (summary (getf properties :summary)))
    (unless (and (keywordp family) (stringp wire-json) (non-empty-string-p summary))
      (conversation--record-error
       conversation properties "A persisted native compaction checkpoint is invalid."))
    (let ((item (handler-case (json-decode wire-json)
                  (error ()
                    (conversation--record-error
                     conversation properties "A native checkpoint is not JSON.")))))
      (native-compaction-item-canonicalize item)
      (unless (native-compaction-item-p item)
        (conversation--record-error
         conversation properties "A persisted native compaction item is unsupported."))
      (let* ((summary-item (conversation-summary-item summary))
             (projection (clinker-transcript:make-projection :items (list item summary-item))))
        (setf (gethash item
                       (clinker-transcript:projection-metadata-table projection ':family)) family
              (gethash summary-item
                       (clinker-transcript:projection-metadata-table projection ':handoff-family)) family)
        (conversation-compaction--restore conversation properties projection)))))
