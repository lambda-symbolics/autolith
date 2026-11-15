(in-package #:autolith)

;;;; -- Cutoff Capture and Carry-Forward --

(defclass conversation-compaction ()
  ((conversation
    :initarg       :conversation
    :reader        conversation-compaction--conversation
    :documentation "The conversation whose projection was captured.")
   (view
    :initarg       :view
    :reader        conversation-compaction-view
    :documentation "Detached request view of the captured durable items.")
   (source-items
    :initarg       :source-items
    :reader        conversation-compaction--source-items
    :documentation "Original item identities used to detect chronological arrivals.")
   (plan
    :initarg       :plan
    :reader        conversation-compaction--plan
    :documentation "Pure transcript carry-forward plan for the captured projection.")
   (cutoff-sequence
    :initarg       :cutoff-sequence
    :reader        conversation-compaction-cutoff-sequence
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
  "Copy all named metadata for retained EQ ITEMS from SOURCE to TARGET."
  (let ((remap (make-hash-table :test #'eq)))
    (dolist (item items) (setf (gethash item remap) item))
    (clinker-transcript:projection-copy-metadata source target :remap remap))
  nil)

(-> conversation-compaction-capture (conversation) conversation-compaction)
(defun conversation-compaction-capture (conversation)
  "Capture a fixed durable cutoff before any compaction provider request.

The request view contains pure protocol repairs; the authoritative plan keeps
original calls so actual arrivals take precedence when publishing."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((items (conversation-input-items-for-request
                   conversation :include-ephemeral-p nil))
           (view  (conversation-compaction-summary-view conversation items))
           (plan  (clinker-transcript:make-compaction-plan
                   (conversation-projection view)
                   :repaired-output-p #'conversation-compaction--repaired-output-p)))
      (setf (conversation-input-items view)
            (clinker-transcript:reconciliation-items
             (clinker-transcript:reconcile-items
              items :repaired-output-p #'conversation-compaction--repaired-output-p)
             :repair-output #'conversation-compaction--repair-output))
      (make-instance 'conversation-compaction
                     :conversation    conversation
                     :view            view
                     :source-items    items
                     :plan            plan
                     :cutoff-sequence (1- (conversation-next-sequence conversation))))))

(-> conversation-compaction-evidence-records (conversation-compaction) (values list list))
(defun conversation-compaction-evidence-records (compaction)
  "Return the user message and tool result records COMPACTION covers, newest first.

Those are the records the provider context was built from: the ones after the
latest checkpoint or adopted context, up to COMPACTION's cutoff. The second
value names that range as the conversation identifier with its first and last
sequence, or is NIL when the range is empty."
  (let ((conversation (conversation-compaction--conversation compaction))
        (cutoff       (conversation-compaction-cutoff-sequence compaction))
        (records      nil)
        (start        1))
    (when (conversation-persisted-p conversation)
      (conversation--map-picker-records
       (conversation-pathname conversation)
       (lambda (record)
         (destructuring-bind (kind &key seq role &allow-other-keys) record
           (case kind
             ((:summary :native-compaction :context-adoption)
              (setf records nil
                    start   (1+ seq)))
             ((:message :tool-result)
              (when (and (<= seq cutoff)
                         (or (eq kind ':tool-result) (eq role ':user)))
                (push record records))))))))
    (values records
            (and (<= start cutoff)
                 (list (conversation-identifier conversation) start cutoff)))))

(-> conversation-compaction--arrivals (conversation conversation-compaction) list)
(defun conversation-compaction--arrivals (conversation compaction)
  "Return chronological durable arrivals, rejecting a replaced source projection."
  (let ((source  (conversation-compaction--source-items compaction))
        (current (conversation-input-items-for-request
                  conversation :include-ephemeral-p nil)))
    (unless (and (eq conversation (conversation-compaction--conversation compaction))
                 (<= (conversation-compaction-cutoff-sequence compaction)
                     (1- (conversation-next-sequence conversation)))
                 (<= (length source) (length current))
                 (every #'eq source (subseq current 0 (length source))))
      (error 'conversation-invariant-error
             :message  "The conversation projection changed after compaction capture."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (nthcdr (length source) current)))

(defparameter *conversation-compaction-row-metadata*
  '(:family :handoff-family :checkpoint-summary :continuation)
  "The projection metadata a carried context row keeps for its item.")

(-> conversation-compaction--serialize-item
    (clinker-transcript:projection json-object) list)
(defun conversation-compaction--serialize-item (projection item)
  "Encode ITEM and its projection metadata as one portable carry-forward row."
  (let ((row (list :wire-json (json-encode item))))
    (dolist (key *conversation-compaction-row-metadata*)
      (multiple-value-bind (value present-p)
          (gethash item (clinker-transcript:projection-metadata-table projection key))
        (when present-p
          (setf (getf row key) value))))
    row))

(-> conversation-compaction--mark-checkpoint
    (clinker-transcript:projection json-object (option list)) null)
(defun conversation-compaction--mark-checkpoint (projection item continuation)
  "Record in PROJECTION that ITEM is a checkpoint, with its typed CONTINUATION when it has one."
  (flet ((table (key)
           "Return PROJECTION's metadata table for KEY."
           (clinker-transcript:projection-metadata-table projection key)))
    (setf (gethash item (table ':checkpoint-summary)) t)
    (when continuation
      (setf (gethash item (table ':continuation)) continuation)))
  nil)

(-> conversation-continuation (conversation) (option list))
(defun conversation-continuation (conversation)
  "Return the typed checkpoint CONVERSATION's provider context holds, or NIL."
  (let ((continuations (clinker-transcript:projection-metadata-table
                        (conversation-projection conversation) ':continuation)))
    (loop for item in (reverse (conversation-input-items conversation))
          thereis (gethash item continuations))))

(-> conversation-compaction--publish
    (conversation &key (:compaction conversation-compaction)
                       (:replacement-items list) (:record list)
                       (:unfinished-work list) (:validate-projection (option function)))
    list)
(defun conversation-compaction--publish
    (conversation &key compaction replacement-items record unfinished-work validate-projection)
  "Validate carry-forward state, publish one checkpoint, then install it.

The cutoff describes the provider snapshot. THROUGH-SEQ covers the commit
boundary, including arrivals encoded in CARRY-FORWARD, as required by chunks."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (when (and unfinished-work
               (not (and (task-unfinished-work-snapshot-p unfinished-work)
                         (equal (getf unfinished-work :owner-conversation)
                                (conversation-identifier conversation)))))
      (error 'conversation-invariant-error
             :message  "The compaction job snapshot is invalid or belongs to another conversation."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (unless (continuation-p (getf (rest record) :continuation))
      (error 'conversation-invariant-error
             :message  "The compaction checkpoint is invalid."
             :pathname (conversation-pathname conversation)
             :sequence (conversation-next-sequence conversation)))
    (let* ((arrivals (conversation-compaction--arrivals conversation compaction))
           (projection
             (clinker-transcript:compaction-plan-projection
              (conversation-compaction--plan compaction)
              :replacement-items replacement-items
              :additional-items  arrivals
              :repair-output     #'conversation-compaction--repair-output))
           (items    (clinker-transcript:projection-items projection))
           (ephemeral-items
             (mapcar (lambda (entry) (getf entry :item))
                     (conversation-ephemeral-input-entries conversation))))
        (conversation-compaction--copy-metadata
         (conversation-projection conversation) projection items)
        ;; Only the native window belongs to one provider family. Every
        ;; family reads the checkpoint that follows it.
        (when (eq (first record) :native-compaction)
          (dolist (item (butlast replacement-items))
            (setf (gethash item
                           (clinker-transcript:projection-metadata-table projection ':family))
                  (getf (rest record) :family))))
        (conversation-compaction--mark-checkpoint
         projection (first (last replacement-items)) (getf (rest record) :continuation))
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
      (when validate-projection
        (let ((candidate (conversation-compaction-summary-view conversation nil)))
          (setf (slot-value candidate 'projection) projection)
          (funcall validate-projection candidate)))
      (sb-sys:without-interrupts
        (let ((published (conversation-append-record conversation record)))
          (setf (slot-value conversation 'projection) projection
                (conversation-last-total-tokens conversation) 0)
          published)))))

(-> conversation-append-summary
    (conversation list &key (:compaction (option conversation-compaction))
                            (:unfinished-work list) (:validate-projection (option function)) (:recovery list))
    list)
(defun conversation-append-summary
    (conversation continuation &key compaction unfinished-work validate-projection recovery)
  "Persist CONTINUATION with captured protocol context and arrivals as one checkpoint.

The record also holds the checkpoint's rendered text, for search and for
readers that do not know the typed form."
  (let ((content (continuation-render continuation)))
    (conversation-compaction--publish
     conversation
     :compaction          (or compaction (conversation-compaction-capture conversation))
     :replacement-items   (list (conversation-summary-item content))
     :record              (list :summary :content content :continuation continuation
                                :recovery recovery)
     :unfinished-work     unfinished-work
     :validate-projection validate-projection)))

(-> conversation-append-native-compaction
    (conversation (or list json-object) &key (:family keyword) (:continuation list)
                             (:compaction (option conversation-compaction))
                             (:unfinished-work list) (:validate-projection (option function)) (:recovery list))
    list)
(defun conversation-append-native-compaction
    (conversation window &key family continuation compaction unfinished-work validate-projection recovery)
  "Persist native WINDOW, CONTINUATION, and carry-forward state atomically."
  (when (json-object-p window)
    (setf window (list window)))
  (dolist (item window)
    (native-compaction-item-canonicalize item))
  (unless (and (keywordp family) (listp window) (not (null window))
               (every #'json-object-p window)
               (some #'native-compaction-item-p window))
    (error 'conversation-invariant-error
           :message  "A native compaction window is invalid."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let ((content (continuation-render continuation)))
    (conversation-compaction--publish
     conversation
     :compaction          (or compaction (conversation-compaction-capture conversation))
     :replacement-items   (append window (list (conversation-summary-item content)))
     :record              (list :native-compaction :family family
                                :wire-json         (json-encode (coerce window 'vector))
                                :summary           content :continuation continuation
                                :recovery          recovery)
     :unfinished-work     unfinished-work
     :validate-projection validate-projection)))


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
    (let ((cutoff  (getf properties :cutoff-seq))
          (through (getf properties :through-seq)))
      (unless (and (integerp cutoff) (integerp through) (<= 0 cutoff through)
                   (= through (1- (getf properties :seq))))
        (conversation--record-error
         conversation properties "A compaction cutoff is outside its checkpoint."))))
  (conversation-compaction--append-rows conversation properties projection
                                        :rows (getf properties :carry-forward))
  (conversation-compaction--check-correlations conversation properties projection)
  (setf (slot-value conversation 'projection) projection
        (conversation-last-total-tokens conversation) 0)
  nil)

(-> conversation-compaction--append-rows
    (conversation list clinker-transcript:projection &key (:rows list)) null)
(defun conversation-compaction--append-rows (conversation properties projection &key rows)
  "Decode carried ROWS of the record with PROPERTIES onto PROJECTION, with their family metadata.

A row is a plist of its item's :WIRE-JSON and the metadata the item had where
it was carried from, named by *CONVERSATION-COMPACTION-ROW-METADATA*."
  (handler-case
      (dolist (row rows)
        (unless (and (listp row) (list-length row) (evenp (length row))
                     (stringp (getf row :wire-json))
                     (let ((keys (plist-keys row)))
                       (and (= (length keys) (length (remove-duplicates keys)))
                            (every (lambda (key)
                                     (member key (cons :wire-json
                                                       *conversation-compaction-row-metadata*)))
                                   keys))))
          (conversation--record-error
           conversation properties "A carried context row is invalid."))
        (let ((item (json-decode (getf row :wire-json))))
          (unless (typep item 'json-object)
            (conversation--record-error
             conversation properties "A carried context item is not an object."))
          (native-compaction-item-canonicalize item)
          (clinker-transcript:projection-append projection item)
          (dolist (key *conversation-compaction-row-metadata*)
            (when (member key row)
              (let ((value (getf row key)))
                (unless (or (member key '(:checkpoint-summary :continuation))
                            (typep value '(or null keyword)))
                  (conversation--record-error
                   conversation properties "A carried context row has invalid metadata."))
                ;; The item's text is what the model reads, so a typed
                ;; checkpoint of another format is left out.
                (when (or (not (eq key ':continuation)) (continuation-p value))
                  (setf (gethash item
                                 (clinker-transcript:projection-metadata-table projection key))
                        value)))))))
    (conversation-error (condition)
      (error condition))
    (error ()
      (conversation--record-error
       conversation properties "A carried context payload is invalid.")))
  nil)

(-> conversation-compaction--check-correlations
    (conversation list clinker-transcript:projection) null)
(defun conversation-compaction--check-correlations (conversation properties projection)
  "Reject PROJECTION, restored from the record with PROPERTIES, when a call lacks its output."
  (handler-case
      (let ((reconciliation
              (clinker-transcript:reconcile-items
               (clinker-transcript:projection-items projection)
               :repaired-output-p #'conversation-compaction--repaired-output-p)))
        (when (clinker-transcript:reconciliation-repairs reconciliation)
          (conversation--record-error
           conversation properties "A restored context has unresolved calls.")))
    (clinker-transcript:projection-error ()
      (conversation--record-error
       conversation properties "A restored context has invalid correlations.")))
  nil)

(-> conversation-compaction--continuation (conversation list) (option list))
(defun conversation-compaction--continuation (conversation properties)
  "Return the typed checkpoint of the record with PROPERTIES, or NIL when it cannot be used.

The record's text is what the model reads, so the record loads either way. A
typed checkpoint of another format, or one that cites a record of CONVERSATION
after the cutoff, is left out."
  (let ((continuation (getf properties :continuation))
        (cutoff       (getf properties :cutoff-seq)))
    (and (continuation-p continuation)
         (every (lambda (reference)
                  (or (not (equal (getf reference :conversation)
                                  (conversation-identifier conversation)))
                      (and (integerp cutoff)
                           (integerp (getf reference :sequence))
                           (<= (getf reference :sequence) cutoff))))
                (getf continuation :references))
         continuation)))

(defmethod conversation--project-record
    ((kind (eql :summary)) conversation properties)
  "Restore a portable checkpoint and its validated chronological carry-forward."
  (let ((content (getf properties :content)))
    (unless (stringp content)
      (conversation--record-error
       conversation properties "A persisted summary checkpoint has invalid content."))
    (let* ((item       (conversation-summary-item content))
           (projection (clinker-transcript:make-projection :items (list item))))
      (conversation-compaction--mark-checkpoint
       projection item (conversation-compaction--continuation conversation properties))
      (conversation-compaction--restore conversation properties projection))))

(defmethod conversation--project-record
    ((kind (eql :native-compaction)) conversation properties)
  "Restore native family privacy, the checkpoint, and carry-forward context.

A record written before checkpoints were typed holds a text handoff, which
the family that owns the native window does not read."
  (let ((family    (getf properties :family))
        (wire-json (getf properties :wire-json))
        (summary   (getf properties :summary)))
    (unless (and (keywordp family) (stringp wire-json) (non-empty-string-p summary))
      (conversation--record-error
       conversation properties "A persisted native compaction checkpoint is invalid."))
    (let ((decoded (handler-case (json-decode wire-json)
                     (error ()
                       (conversation--record-error
                        conversation properties "A native checkpoint is not JSON.")))))
      (let ((window (if (vectorp decoded)
                        (coerce decoded 'list)
                        (list decoded))))
        (dolist (item window)
          (when (json-object-p item)
            (native-compaction-item-canonicalize item)))
        (unless (and (every #'json-object-p window)
                     (some #'native-compaction-item-p window))
          (conversation--record-error
           conversation properties "A persisted native compaction window is unsupported."))
        (let* ((summary-item (conversation-summary-item summary))
               (projection   (clinker-transcript:make-projection
                              :items (append window (list summary-item))))
               (continuation (conversation-compaction--continuation conversation properties)))
          (dolist (item window)
            (setf (gethash item
                           (clinker-transcript:projection-metadata-table projection ':family))
                  family))
          (unless (member :continuation properties)
            (setf (gethash summary-item
                           (clinker-transcript:projection-metadata-table projection ':handoff-family))
                  family))
          (conversation-compaction--mark-checkpoint projection summary-item continuation)
          (conversation-compaction--restore conversation properties projection))))))
