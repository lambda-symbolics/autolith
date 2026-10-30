;;;; -- Parent-backed Conversation Projection --

(in-package #:autolith)

(defclass agent-session-conversation (conversation)
  ((driver :initarg :driver :accessor agent-session-conversation-driver
           :documentation "The sole-reader driver used for authoritative session operations.")
   (artifact-root :initform nil :accessor agent-session-conversation-artifact-root
    :documentation "An approved read-only legacy artifact projection supplied by the parent.")
   (records :initform nil :accessor agent-session-conversation-records
            :documentation "Acknowledged or parent-read records, in chronological order.")
   (high-water :initform 0 :accessor agent-session-conversation-high-water
               :documentation "The greatest parent sequence observed or acknowledged.")
   (bytes :initform 0 :accessor agent-session-conversation-bytes
          :documentation "Serialized bytes retained in the bounded record cache.")
   (counter :initform 0 :accessor agent-session-conversation-counter
            :documentation "Connection-local append event counter.")
   (turn-id :initform nil :accessor agent-session-conversation-turn-id
            :documentation "The parent turn allowed to accept new agent records.")
   (started-calls :initform      (make-hash-table :test #'equal)
                  :reader        agent-session-conversation-started-calls
                  :documentation "Tool starts acknowledged on this connection.")
   (repairs :initform nil :accessor agent-session-conversation-repairs
            :documentation "Truthful missing-result records to publish before the next model request."))
  (:documentation "An in-memory projection with parent-owned durable storage. No legacy sidecars."))

(defmethod conversation-image-artifact-root ((conversation agent-session-conversation))
  "Resolve imported images only under the parent's public projection."
  (or (agent-session-conversation-artifact-root conversation) (call-next-method)))

(defmethod conversation--restore-provider-item ((conversation agent-session-conversation) item)
  "Preserve new parent records exactly, including malformed argument strings."
  (declare (ignore conversation))
  item)

(defmethod conversation-input-history ((conversation agent-session-conversation))
  "Return editable parent inputs and agent operations without opening legacy paths."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (loop for record in (agent-session-conversation-records conversation)
          for local = (agent-session--local-record record)
          for input = (and local (conversation-input-history--record-input local))
          when input collect (copy-seq input))))

(defmethod conversation-append-record :around
    ((conversation agent-session-conversation) (record list))
  "Publish through the parent, bypassing the legacy file-lock around method."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((driver   (agent-session-conversation-driver conversation))
           (client   (agent-protocol-driver-client driver))
           (turn-id  (agent-session-conversation-turn-id conversation))
           (event-id (format nil "agent-record/~A/~D"
                             (autolith-supervisor::parent-connection-instance-id client)
                             (incf (agent-session-conversation-counter conversation))))
           (payload  (make-hash-table :test #'equal))
           (kind     ':note)
           (call-id  (getf (rest record) :call-id))
           (started  (gethash call-id (agent-session-conversation-started-calls conversation))))
      (unless (and turn-id (conversation--record-form-p record)
                   (not (conversation--property-present-p (rest record) ':seq)))
        (error 'agent-protocol-error :reason ':invalid-agent-record))
      (setf (gethash "version" payload) 1
            (gethash "agent_record" payload) (agent-session-value-encode record))
      (when (and (member (first record) '(:tool-result :worker-tool-result :task-inspector-result))
                 (equal turn-id (first started)))
        (let ((source (case (first record)
                        (:worker-tool-result
                         "worker")
                        (:task-inspector-result
                         "task-inspector"))))
          (unless (and (eq (fourth started) ':started) (equal source (second started)))
            (error 'agent-protocol-error :reason ':mismatched-tool-result))
          (when (and source
                     (not (equal (getf (rest record) :parent-call-id) (third started))))
            (error 'agent-protocol-error :reason ':mismatched-tool-result))
          (setf kind ':tool-finished
                payload (let ((result (make-hash-table :test #'equal)))
                          (setf (gethash "call_id" result) call-id
                                (gethash "result" result) payload
                                (gethash "is_error" result)
                                (case (first record)
                                  (:tool-result
                                   (not (eq (getf (rest record) :status) ':ok)))
                                  (:worker-tool-result
                                   (not (getf (getf (rest record) :result) :success-p)))
                                  (:task-inspector-result
                                   (not (getf (rest record) :success-p)))))
                          (when source
                            (setf (gethash "source" result) source
                                  (gethash "parent_call_id" result) (third started)))
                          result))))
      (let* ((wire      (agent-wire:make-session-record
                         :event-id event-id :turn-id turn-id :kind kind
                         :payload (autolith-supervisor::protocol-data->value payload)))
             (ack       (agent-session--append conversation wire))
             (sequence  (agent-wire:session-ack.sequence ack))
             (sequenced (list* (first record) :seq sequence :time 0 (rest record))))
        (setf (agent-wire:session-record.sequence wire) sequence
              (agent-wire:session-record.origin wire) ':agent)
        (agent-session--retain conversation wire)
        (when (eq kind ':tool-finished)
          (setf (fourth started) ':finished))
        ;; The existing callers install their prepared projection only after append.
        (setf (conversation-next-sequence conversation) (1+ sequence))
        (conversation--note-activity conversation sequenced)
        (conversation--note-pending-input-identifier conversation sequenced)
        (when (eq (first record) ':goal)
          (setf (conversation-latest-goal-record conversation) sequenced))
        sequenced))))

(-> agent-session--append (agent-session-conversation agent-wire:session-record) agent-wire:session-ack)
(defun agent-session--append (conversation record)
  "Wait for the validated matching acknowledgement. Never retry an uncertain append."
  (let* ((delivery
           (agent-protocol-driver-request
            (agent-session-conversation-driver conversation)
            (lambda (client) (agent-protocol-session-append client record))))
         (envelope (agent-protocol-driver-next delivery)))
    (unless (and envelope (agent-wire:envelope.has-session-ack envelope))
      (error 'agent-protocol-error :reason ':session-append-failed))
    (agent-wire:envelope.session-ack envelope)))

(-> agent-session--retain (agent-session-conversation agent-wire:session-record) null)
(defun agent-session--retain (conversation record)
  "Retain one record within explicit cache limits, after authoritative publication."
  (let ((bytes (length (cl-protobufs:serialize-to-bytes record))))
    (when (or (>= (length (agent-session-conversation-records conversation)) 65536)
              (> (+ bytes (agent-session-conversation-bytes conversation)) 67108864))
      (error 'agent-protocol-error :reason ':session-cache-limit))
    (incf (agent-session-conversation-bytes conversation) bytes)
    (setf (agent-session-conversation-records conversation)
          (nconc (agent-session-conversation-records conversation) (list record))
          (agent-session-conversation-high-water conversation)
          (max (agent-session-conversation-high-water conversation)
               (agent-wire:session-record.sequence record))
          (conversation-next-sequence conversation)
          (1+ (agent-session-conversation-high-water conversation))))
  nil)

(-> agent-session--local-record (agent-wire:session-record) (option list))
(defun agent-session--local-record (record)
  "Decode one explicit agent-owned record or a parent input into legacy projection data."
  (let* ((payload    (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload record)))
         (sequence   (agent-wire:session-record.sequence record))
         (agent-data (if (eq (agent-wire:session-record.kind record) ':tool-finished)
                         (gethash "result" payload)
                         payload)))
    (cond
      ((and (eq (agent-wire:session-record.origin record) ':agent)
            (hash-table-p agent-data) (eql (gethash "version" agent-data) 1)
            (gethash "agent_record" agent-data))
       (let ((local (agent-session-value-decode (gethash "agent_record" agent-data))))
         (unless (and (conversation--record-form-p local)
                      (not (conversation--property-present-p (rest local) ':seq)))
           (error 'agent-protocol-error :reason ':invalid-agent-record))
         (list* (first local) :seq sequence :time 0 (rest local))))
      ((and (eq (agent-wire:session-record.origin record) ':parent)
            (eq (agent-wire:session-record.kind record) ':note)
            (hash-table-p (gethash "legacy_header" payload)))
       (let ((header (gethash "legacy_header" payload)))
         (list* ':conversation
           (loop for (name key) in '(("id" :id) ("created-at" :created-at) ("title" :title)
                                     ("title-source" :title-source) ("prompt-cache-key" :prompt-cache-key)
                                     ("model" :model) ("model-provider" :model-provider)
                                     ("reasoning-effort" :reasoning-effort) ("artifact-root" :artifact-root))
                 when (nth-value 1 (gethash name header))
                   append (list key (gethash name header))))))
      ((and (eq (agent-wire:session-record.origin record) ':parent)
            (eq (agent-wire:session-record.kind record) ':user-input)
            (not (gethash "application_kind" payload)))
       (let ((text (gethash "text" payload)))
         (list :message :seq sequence :time 0 :role ':user :content text
               :wire-json (json-encode (user-message-item text nil)))))
      (t
       nil))))

(-> agent-session--replay-handle (list) (option string))
(defun agent-session--replay-handle (record)
  "Return the replay identity from an explicit provider record, if present."
  (when (and (eq (first record) ':provider-item) (stringp (getf (rest record) :wire-json)))
    (let* ((item   (json-decode (getf (rest record) :wire-json)))
           (marker (or (json-get item "parent_replay") item)))
      (json-get marker "replay_handle"))))

(-> agent-session-conversation-sync (agent-session-conversation) null)
(defun agent-session-conversation-sync (conversation)
  "Read every pending page, then restore parent results and agent projection records.
Do not run a tool, repeat a model request, or write a repair during this read."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((incoming nil)
          (seen     (make-hash-table :test #'equal))
          (saved    (make-hash-table :test #'equal)))
      (dolist (item (conversation-input-items conversation))
        (let* ((marker (or (json-get item "parent_replay") item))
               (handle (json-get marker "replay_handle")))
          (when handle (setf (gethash handle seen) t))))
      (loop
        (let* ((delivery (agent-protocol-driver-request
                          (agent-session-conversation-driver conversation)
                          (lambda (client)
                            (agent-protocol-session-read
                             client :after-sequence (agent-session-conversation-high-water conversation)))))
               (envelope (agent-protocol-driver-next delivery)))
          (unless (and envelope (agent-wire:envelope.has-session-page envelope))
            (error 'agent-protocol-error :reason ':session-read-failed))
          (let ((page (agent-wire:envelope.session-page envelope)))
            (dolist (record (agent-wire:session-page.records page))
              (agent-session--retain conversation record)
              (push record incoming))
            (unless (agent-wire:session-page.has-more page)
              (return)))))
      (dolist (record (agent-session-conversation-records conversation))
        (let* ((local  (agent-session--local-record record))
               (handle (and local (agent-session--replay-handle local))))
          (when handle (setf (gethash handle saved) local))))
      (dolist (record (nreverse incoming))
        (let* ((local  (agent-session--local-record record))
               (handle (and local (agent-session--replay-handle local))))
          (cond
            (local
             (unless (and handle (gethash handle seen))
               (when (and (eq (first local) ':conversation) (getf (rest local) :artifact-root))
                (setf (agent-session-conversation-artifact-root conversation)
                      (uiop:ensure-directory-pathname (getf (rest local) :artifact-root))))
              (if (eq (first local) ':conversation)
                  (agent-session--restore-header conversation (rest local))
                  (conversation--apply-record conversation local))
              (when handle (setf (gethash handle seen) t))))
            ((and (eq (agent-wire:session-record.origin record) ':parent)
                  (eq (agent-wire:session-record.kind record) ':note))
             (agent-session--restore-model-result conversation record seen :saved-records saved))
            ((eq (agent-wire:session-record.kind record) ':tool-started)
             (agent-session--restore-tool-start conversation record)))))))
  nil)

(-> agent-session--restore-model-result
    (agent-session-conversation agent-wire:session-record hash-table
     &key (:saved-records hash-table)) null)
(defun agent-session--restore-model-result (conversation record seen &key saved-records)
  "Restore accepted output in provider order, preferring later saved agent views."
  (let* ((payload  (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload record)))
         (complete (gethash "model_result" payload)))
    (when complete
      (let ((kind    (gethash "request_kind" payload))
            (purpose (gethash "request_purpose" payload))
            (outputs (gethash "output_items" complete #())))
        (when (or (null kind) (null purpose))
          ;; Accept-v1 outboxes did not retain request classification. Retire this
          ;; compatibility read when those unpublished optional journals are removed.
          (unless (and (not (gethash "message" complete))
                       (every (lambda (item) (gethash (gethash "replay_handle" item) saved-records)) outputs))
            (error 'agent-protocol-error :reason ':ambiguous-model-result))
          (return-from agent-session--restore-model-result nil))
        (unless (and (equal kind "generate") (equal purpose "ordinary"))
          (return-from agent-session--restore-model-result nil))
        (when (gethash "message" complete)
          (error 'agent-protocol-error :reason ':unsupported-model-result)))
      (loop for data across (gethash "output_items" complete #())
            for handle = (gethash "replay_handle" data)
            unless (gethash handle seen)
              do (let* ((saved (gethash handle saved-records))
                        (local (or saved
                                   (list :provider-item :seq (agent-wire:session-record.sequence record)
                                         :time 0 :family ':parent
                                         :wire-json     (json-encode (agent-protocol-provider-item-from-data data))))))
                   (conversation--apply-record conversation local)
                   (setf (gethash handle seen) t)))))
  nil)

(-> agent-session--restore-tool-start (agent-session-conversation agent-wire:session-record) null)
(defun agent-session--restore-tool-start (conversation record)
  "Restore a started call absent from accepted provider output, without executing it."
  (let* ((payload (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload record)))
         (id      (gethash "call_id" payload)))
    (unless (or (gethash "source" payload)
                (find id (conversation-input-items conversation)
                      :test #'equal :key (lambda (item) (and (function-call-item-p item) (json-get item "call_id")))))
      (conversation--append-input-item
       conversation (json-object "type" "function_call" "call_id" id
                                 "name" (gethash "name" payload)
                                 "arguments" (json-encode (agent-context--json (gethash "arguments" payload)))))))
  nil)

(-> agent-session-conversation-reconcile (agent-session-conversation) null)
(defun agent-session-conversation-reconcile (conversation)
  "Prepare truthful interrupted results. Publish them when a new active turn permits writes."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let ((plan (clinker-transcript:reconcile-items (conversation-input-items conversation)
                                                   :preserve-order-p  t
                                                   :repaired-output-p #'conversation-compaction--repaired-output-p)))
      (setf (conversation-input-items conversation)
            (clinker-transcript:reconciliation-items
             plan :repair-output
             (lambda (intent)
               (let* ((id   (clinker-transcript:missing-output-repair-call-id intent))
                      (call (clinker-transcript:missing-output-repair-call intent))
                      (item (function-call-output-item id *conversation-interrupted-tool-output*)))
                 (push (list :tool-result :call-id id :tool (conversation--tool-call-name call)
                             :status ':error :category ':failure :output *conversation-interrupted-tool-output*
                             :wire-json (json-encode item))
                       (agent-session-conversation-repairs conversation))
                 item))))))
  nil)

(-> agent-session-conversation-publish-repairs (agent-session-conversation) null)
(defun agent-session-conversation-publish-repairs (conversation)
  "Acknowledge all prepared recovery records before model generation or tool effects."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (loop while (agent-session-conversation-repairs conversation)
          for record = (first (agent-session-conversation-repairs conversation))
          do (conversation-append-record conversation record)
             (pop (agent-session-conversation-repairs conversation))))
  nil)

(-> agent-session--restore-header (agent-session-conversation list) null)
(defun agent-session--restore-header (conversation properties)
  "Restore approved imported metadata without changing the parent session identity."
  (dolist (entry '((:title conversation-title) (:prompt-cache-key conversation-prompt-cache-key)
                   (:model conversation-model) (:model-provider conversation-model-provider)
                   (:reasoning-effort conversation-reasoning-effort)))
    (when (conversation--property-present-p properties (first entry))
      (funcall (fdefinition (list 'setf (second entry)))
               (getf properties (first entry)) conversation)))
  (when (getf properties :created-at)
    (setf (slot-value conversation 'created-at) (getf properties :created-at)))
  (when (getf properties :title-source)
    (setf (conversation-title-source conversation)
      (find (getf properties :title-source) '(:initial :generated :manual :user)
        :test #'string-equal :key #'symbol-name)))
  (when (getf properties :artifact-root)
    (setf (agent-session-conversation-artifact-root conversation)
          (uiop:ensure-directory-pathname (getf properties :artifact-root))))
  nil)
