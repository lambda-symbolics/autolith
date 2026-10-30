;;;; -- Parent-owned Durable Sessions --

(in-package #:autolith-supervisor)

(define-condition session-store-error (error)
  ((code :initarg :code :reader session-store-error-code
         :documentation "A fixed rejection code with no private record data."))
  (:documentation "A session operation failed without exposing private content.")
  (:report (lambda (condition stream)
             (format stream "Session operation failed: ~A"
                     (session-store-error-code condition)))))

(defclass session-store ()
  ((pathname :initarg :pathname :reader session-store-pathname
             :documentation "The private append-only log pathname.")
   (id :initarg :id :reader session-store-id
       :documentation "The parent-assigned durable session identity.")
   (lock-pathname :initarg :lock-pathname :reader session-store-lock-pathname
                  :documentation "The common lock used by every reader and writer.")
   (cache :initform nil :accessor session-store-cache
          :documentation "Validated state and offset, used only under the file lock."))
  (:documentation "A parent-owned session log, independent of the agent image.

Each successful mutation publishes one complete log entry through sexp-store.
The acknowledgement means publication completed under that library's commit
contract. This is not a promise of power-loss survival beyond the filesystem
and sexp-store guarantees. Records and event identity are in the same entry.
All operations refresh validated state while holding the same ls-flock lock,
which serializes threads and processes. Do not change the log outside this API."))

(declaim (ftype (function (keyword) nil) session-store--reject))

(defun session-store--reject (code)
  "Signal a content-free session failure."
  (error 'session-store-error :code code))

(declaim (ftype (function (t) boolean) session-store--identifier-p))

(defun session-store--identifier-p (value)
  "Return whether VALUE is a nonempty, bounded identifier."
  (and (stringp value) (< 0 (length value) 1025)))

(declaim (ftype (function (t list) boolean) session-store--shape-p))

(defun session-store--shape-p (form keys)
  "Check exact portable record keys before traversing private log input."
  (and (sexp-store:record-shape-p form)
       (= (length form) (1+ (* 2 (length keys))))
       (every (lambda (key) (member key keys))
              (loop for tail on (rest form) by (lambda (tail) (rest (rest tail)))
                    collect (first tail)))))

(declaim (ftype (function (t) list) session-store--value-data))

(defun session-store--value-data (value)
  "Validate with the shared Value protocol and encode readable log data."
  (labels ((encode (item)
             (typecase item
               (hash-table
                (list ':map
                      (sort (loop for key being the hash-keys of item
                                    using (hash-value child)
                                  collect (list key (encode child)))
                            #'string< :key #'first)))
               (string
                (list ':string item))
               ((array (unsigned-byte 8) (*))
                (list ':bytes (coerce item 'list)))
               (vector
                (list ':list (map 'list #'encode item)))
               (integer
                (list ':integer item))
               (float
                (list ':number item))
               (otherwise
                (cond ((eq item ':protocol-null)
                       (list ':null))
                      ((member item '(t nil))
                       (list ':boolean item))
                      (t
                       (session-store--reject ':invalid-payload)))))))
    (handler-case (encode (protocol-value->data value))
      (protocol-value-error () (session-store--reject ':invalid-payload)))))

(declaim (ftype (function (t) boolean) session-store--proper-list-p))

(defun session-store--proper-list-p (value)
  "Check private data for a finite proper list before traversal."
  (and (listp value) (not (null (ignore-errors (list-length value))))))

(declaim (ftype (function (list) cl-protobufs.autolith.common.v1:value)
                session-store--data-value))

(defun session-store--data-value (data)
  "Decode the readable log encoding and validate with the shared Value protocol."
  (let ((remaining 65536))
    (labels ((decode (item depth)
               (when (or (> depth 32) (minusp (decf remaining)))
                 (session-store--reject ':corrupt-log))
               (unless (and (session-store--proper-list-p item)
                            (= (length item) (if (eq (first item) ':null) 1 2)))
                 (session-store--reject ':corrupt-log))
               (let ((value (second item)))
                 (case (first item)
                   (:null
                    ':protocol-null)
                   (:boolean
                    (unless (member value '(t nil))
                      (session-store--reject ':corrupt-log))
                    value)
                   (:integer
                    (unless (typep value '(signed-byte 64))
                      (session-store--reject ':corrupt-log))
                    value)
                   (:number
                    (unless (typep value 'double-float)
                      (session-store--reject ':corrupt-log))
                    value)
                   (:string
                    (unless (stringp value) (session-store--reject ':corrupt-log))
                    value)
                   ((:bytes :list :map)
                    (unless (session-store--proper-list-p value)
                      (session-store--reject ':corrupt-log))
                    (case (first item)
                      (:bytes
                       (unless (every (lambda (byte) (typep byte '(unsigned-byte 8))) value)
                         (session-store--reject ':corrupt-log))
                       (make-array (length value) :element-type    '(unsigned-byte 8)
                                                 :initial-contents value))
                      (:list
                       (map 'vector (lambda (child) (decode child (1+ depth))) value))
                      (:map
                       (let ((map (make-hash-table :test 'equal)))
                         (dolist (entry value)
                           (unless (and (session-store--proper-list-p entry)
                                        (= (length entry) 2) (stringp (first entry))
                                        (not (nth-value 1 (gethash (first entry) map))))
                             (session-store--reject ':corrupt-log))
                           (setf (gethash (first entry) map)
                                 (decode (second entry) (1+ depth))))
                         map))))
                   (otherwise
                    (session-store--reject ':corrupt-log))))))
      (handler-case (protocol-data->value (decode data 0))
        (protocol-value-error () (session-store--reject ':corrupt-log))))))

(declaim (ftype (function (t) list) session-store--record-data))

(defun session-store--record-data (record)
  "Copy a generated record into portable canonical data."
  (unless (typep record 'cl-protobufs.autolith.parent.v1:session-record)
    (session-store--reject ':invalid-record))
  (list (cl-protobufs.autolith.parent.v1:session-record.sequence record)
        (cl-protobufs.autolith.parent.v1:session-record.event-id record)
        (cl-protobufs.autolith.parent.v1:session-record.turn-id record)
        (cl-protobufs.autolith.parent.v1:session-record.kind record)
        (session-store--value-data
         (cl-protobufs.autolith.parent.v1:session-record.payload record))
        (cl-protobufs.autolith.parent.v1:session-record.origin record)))

(declaim (ftype (function (list) cl-protobufs.autolith.parent.v1:session-record)
                session-store--data-record))

(defun session-store--data-record (data)
  "Return a fresh record from canonical DATA."
  (destructuring-bind (sequence event turn kind payload origin) data
    (cl-protobufs.autolith.parent.v1:make-session-record
     :sequence sequence :event-id (copy-seq event) :turn-id (copy-seq turn) :kind kind
     :payload (session-store--data-value payload) :origin origin)))

(declaim (ftype (function (list string) t) session-store--field))

(defun session-store--field (payload name)
  "Find one validated map field."
  (second (assoc name (second payload) :test #'equal)))

(declaim (ftype (function (t) hash-table) session-store-model-complete-data))

(defun session-store-model-complete-data (complete)
  "Copy a trusted ModelComplete or its bounded portable field-name map.
Preserve optional presence, ordered items, raw strings, and shared Value types."
  (unless (or (hash-table-p complete)
              (typep complete 'cl-protobufs.autolith.parent.v1:model-complete))
    (session-store--reject ':invalid-completion))
  (handler-case
      (let ((data (if (hash-table-p complete) complete (protocol-message->data complete))))
        (unless (loop for key being the hash-keys of data
                      always (member key '("message" "usage" "reason" "continuation_handle"
                                           "output_items" "response_id" "metadata" "turn_completion"
                                           "turn_state_handle" "compaction_outcome") :test #'equal))
          (session-store--reject ':invalid-completion))
        (protocol-value->data
         (session-store--data-value (session-store--value-data (protocol-data->value data)))))
    (protocol-value-error (condition)
      (session-store--reject
       (if (member (protocol-value-error-reason condition) '(:depth-limit :entry-limit))
           ':resource-limit
           ':invalid-completion)))))

(declaim (ftype (function (list) null) session-store--model-payload-check))

(defun session-store--model-payload-check (payload)
  "Validate the distinct trusted parent model-result note shape."
  (unless (and (eq (first payload) ':map)
               (member (mapcar #'first (second payload))
                       '(("attempt" "model_result" "request_id" "turn_id")
                         ("attempt" "model_result" "request_id" "request_kind" "request_purpose" "turn_id"))
                       :test #'equal)
               (eq (first (session-store--field payload "model_result")) ':map)
               (eq (first (session-store--field payload "attempt")) ':integer)
               (<= 1 (second (session-store--field payload "attempt")) #xffffffff)
               (every (lambda (name)
                        (let ((field (session-store--field payload name)))
                          (and (eq (first field) ':string)
                               (session-store--identifier-p (second field)))))
                      '("request_id" "turn_id")))
    (session-store--reject ':invalid-payload))
  (when (session-store--field payload "request_kind")
    (let ((kind    (session-store--field payload "request_kind"))
          (purpose (session-store--field payload "request_purpose")))
      (unless (and (eq (first kind) ':string) (eq (first purpose) ':string)
                   (member (second kind) '("generate" "native_compaction") :test #'equal)
                   (member (second purpose) '("ordinary" "summary") :test #'equal)
                   (or (equal (second kind) "generate") (equal (second purpose) "ordinary")))
        (session-store--reject ':invalid-request-classification))))
  (session-store-model-complete-data
   (protocol-value->data (session-store--data-value
                          (session-store--field payload "model_result"))))
  nil)

(declaim (ftype (function (list) null) session-store--agent-payload-check))

(defun session-store--agent-payload-check (payload)
  "Validate opaque tagged agent records without reading Lisp or resolving symbols."
  (let* ((data      (protocol-value->data (session-store--data-value payload)))
         (record    (gethash "agent_record" data))
         (remaining 8192))
    (unless (and (= (hash-table-count data) 2) (eql (gethash "version" data) 1))
      (session-store--reject ':invalid-fields))
    (labels ((check (item depth)
               (unless (and (vectorp item) (not (stringp item))
                            (<= 1 (length item) 2) (stringp (aref item 0))
                            (<= depth 12) (not (minusp (decf remaining))))
                 (session-store--reject ':invalid-agent-record))
               (let ((tag   (aref item 0))
                     (child (and (= (length item) 2) (aref item 1))))
                 (unless
                     (cond
                       ((member tag '("nil" "true") :test #'equal)
                        (= (length item) 1))
                       ((equal tag "keyword")
                        (and (= (length item) 2) (stringp child) (< 0 (length child) 161)))
                       ((equal tag "string")
                        (and (= (length item) 2) (stringp child) (<= (length child) 1048576)))
                       ((equal tag "integer")
                        (and (= (length item) 2) (typep child '(signed-byte 64))))
                       ((equal tag "number")
                        (and (= (length item) 2) (typep child 'double-float)))
                       ((member tag '("list" "vector") :test #'equal)
                        (and (= (length item) 2) (vectorp child) (not (stringp child))
                             (progn (loop for entry across child do (check entry (1+ depth))) t)))
                       (t
                        nil))
                   (session-store--reject ':invalid-agent-record)))))
      (check record 0))
    (unless (and (equal (aref record 0) "list")
                 (let ((entries (aref record 1)))
                   (and (oddp (length entries)) (plusp (length entries))
                        (equal (aref (aref entries 0) 0) "keyword")
                        (loop for index from 1 below (length entries) by 2
                              always (equal (aref (aref entries index) 0) "keyword")))))
      (session-store--reject ':invalid-agent-record)))
  nil)

(declaim (ftype (function (keyword list) null) session-store--payload-check))

(defun session-store--payload-check (kind payload)
  "Enforce exact field names and types for a session payload."
  (unless (eq (first payload) ':map)
    (session-store--reject ':invalid-payload))
  (when (and (eq kind ':note) (session-store--field payload "agent_record"))
    (session-store--agent-payload-check payload)
    (return-from session-store--payload-check nil))
  (let* ((fields   (second payload))
         (names    (mapcar #'first fields))
         (required (case kind
                     ((:user-input :assistant-message :note)
                      '("text"))
                     (:tool-started
                      '("call_id" "name" "arguments"))
                     (:tool-finished
                      '("call_id" "result" "is_error"))
                     (:turn-outcome
                      '("outcome"))
                     (otherwise
                      (session-store--reject ':invalid-kind))))
         (allowed  (if (eq kind ':turn-outcome) '("outcome" "reason") required)))
    (unless (and (every (lambda (key) (member key names :test #'equal)) required)
                 (every (lambda (key) (member key allowed :test #'equal)) names))
      (session-store--reject ':invalid-fields))
    (dolist (name '("text" "call_id" "name" "outcome" "reason"))
      (let ((field (session-store--field payload name)))
        (when (and field (not (eq (first field) ':string)))
          (session-store--reject ':invalid-field-type))))
    (when (eq kind ':tool-started)
      (unless (and (session-store--identifier-p
                    (second (session-store--field payload "call_id")))
                   (session-store--identifier-p
                    (second (session-store--field payload "name")))
                   (eq (first (session-store--field payload "arguments")) ':map))
        (session-store--reject ':invalid-field-type)))
    (when (eq kind ':tool-finished)
      (unless (and (session-store--identifier-p
                    (second (session-store--field payload "call_id")))
                   (eq (first (session-store--field payload "is_error")) ':boolean))
        (session-store--reject ':invalid-field-type)))
    (when (eq kind ':turn-outcome)
      (unless (member (second (session-store--field payload "outcome"))
                      '("completed" "cancelled" "failed" "interrupted") :test #'equal)
        (session-store--reject ':invalid-outcome))))
  nil)

(defclass session-store-state ()
  ((records :initform      (make-array 0 :adjustable t :fill-pointer 0)
            :reader        session-store-state-records
            :documentation "Canonical records indexed by sequence minus one.")
   (active :initform nil :accessor session-store-state-active
           :documentation "The current unfinished turn, if any.")
   (events :initform (make-hash-table :test 'equal) :reader session-store-state-events
           :documentation "Records indexed by event strings and tagged model turn/request keys.")
   (turns :initform (make-hash-table :test 'equal) :reader session-store-state-turns
          :documentation "Turn identities that have already been used.")
   (calls :initform (make-hash-table :test 'equal) :reader session-store-state-calls
          :documentation "Tool call state indexed by call identity.")
   (position :initform 0 :accessor session-store-state-position
             :documentation "The log position after the last complete form.")
   (information :initform nil :accessor session-store-state-information
                :documentation "Filesystem identity and metadata after validation."))
  (:documentation "A private incremental replay cache for one session handle."))

(declaim (ftype (function (t t) boolean) session-store--exact-p))

(defun session-store--exact-p (left right)
  "Compare canonical trees with case-sensitive strings and exact typed numbers.

Floating zero includes its sign. Value encoding supplies distinct tags for
integers, numbers, byte arrays, lists, maps, booleans, strings, and null."
  (cond
    ((and (consp left) (consp right))
     (and (session-store--exact-p (first left) (first right))
          (session-store--exact-p (rest left) (rest right))))
    ((and (stringp left) (stringp right))
     (string= left right))
    ((and (floatp left) (floatp right))
     (and (eql left right) (eql (float-sign left) (float-sign right))))
    (t
     (eql left right))))

(declaim (ftype (function (list) list) session-store--identity))

(defun session-store--identity (data)
  "Return agent-supplied identity without assigned sequence and origin."
  (subseq data 1 5))

(declaim (ftype (function (session-store-state list) null) session-store--replay-entry))

(defun session-store--replay-entry (state form)
  "Validate and add one complete entry to STATE in constant index time."
  (unless (and (session-store--shape-p form '(:version :record :continuation))
               (eq (first form) ':session-entry)
               (eql (getf (rest form) :version) 1))
    (session-store--reject ':corrupt-log))
  (let ((data         (getf (rest form) :record))
        (continuation (getf (rest form) :continuation))
        (records      (session-store-state-records state))
        (active       (session-store-state-active state))
        (events       (session-store-state-events state))
        (turns        (session-store-state-turns state))
        (calls        (session-store-state-calls state)))
    (unless (and (session-store--proper-list-p data) (= (length data) 6))
      (session-store--reject ':corrupt-log))
    (unless (session-store--exact-p data (session-store--record-data
                                          (session-store--data-record data)))
      (session-store--reject ':corrupt-log))
    (destructuring-bind (sequence event turn kind payload origin) data
      (unless (and (eql sequence (1+ (length records)))
                   (session-store--identifier-p event)
                   (session-store--identifier-p turn)
                   (not (gethash event events)))
        (session-store--reject ':corrupt-log))
      (let ((model-result (and (eq kind ':note)
                               (session-store--field payload "model_result"))))
        (if (and (eq origin ':parent) model-result)
            (session-store--model-payload-check payload)
            (session-store--payload-check kind payload)))
      (cond
        ((and (eq origin ':parent) (eq kind ':note)
              (session-store--field payload "model_result"))
         (let ((key (list ':model-result turn
                          (second (session-store--field payload "request_id")))))
           (unless (and (gethash turn turns) (null continuation)
                        (equal turn (second (session-store--field payload "turn_id")))
                        (not (gethash key events)))
             (session-store--reject ':corrupt-log))
           (setf (gethash key events) data)))
        ((and (eq origin ':parent) (member kind '(:user-input :note)))
         (when (or active (gethash turn turns)
                   (and continuation
                        (not (and (integerp continuation)
                                  (<= 1 continuation (length records)))))
                   (and (eq kind ':user-input) continuation))
           (session-store--reject ':corrupt-log))
         (setf active turn (gethash turn turns) t))
        ((and (eq origin ':parent) (eq kind ':turn-outcome))
         (unless (and (equal active turn) (null continuation))
           (session-store--reject ':corrupt-log))
         (setf active nil))
        ((and (eq origin ':agent)
              (member kind '(:assistant-message :tool-started :tool-finished :note)))
         (unless (and (equal active turn) (null continuation))
           (session-store--reject ':corrupt-log)))
        (t
         (session-store--reject ':corrupt-log)))
      (when (member kind '(:tool-started :tool-finished))
        (let* ((call  (second (session-store--field payload "call_id")))
               (prior (gethash call calls)))
          (if (eq kind ':tool-started)
              (progn
                (when prior (session-store--reject ':corrupt-log))
                (setf (gethash call calls) (list turn ':started sequence)))
              (progn
                (unless (and prior (equal (first prior) turn)
                             (eq (second prior) ':started))
                  (session-store--reject ':corrupt-log))
                (setf (second prior) ':finished)))))
      (setf (gethash event events) data
            (session-store-state-active state) active)
      (vector-push-extend data records)))
  nil)

(declaim (ftype (function (session-store function) t) session-store--locked))

(defun session-store--locked (store function)
  "Refresh validated state under the common lock, then call FUNCTION.

Read only appended forms when the filesystem object is unchanged and grew.
Rebuild on replacement, shrink, or an observed same-size metadata change.
Object identity detects same-size atomic replacement independently of timestamps.
Filesystem times are only a conservative invalidation hint. External in-place
rewrites are outside the append-only API contract. A cold open always replays
the full log. Clear partial state on every failed operation."
  (handler-case
      (ls-flock:with-file-lock ((session-store-lock-pathname store))
        (let* ((path        (session-store-pathname store))
               (information (ls-compat.posix:file-information path))
               (cached      (session-store-cache store))
               (prior       (and cached (session-store-state-information cached)))
               (state       (if (and prior
                                     (ls-compat.posix:file-information-same-object-p
                                      prior information)
                                     (or (ls-compat.posix:file-information-unchanged-p
                                          prior information)
                                         (> (ls-compat.posix:file-information-size information)
                                            (ls-compat.posix:file-information-size prior))))
                                cached
                                (make-instance 'session-store-state)))
               (header      (plusp (session-store-state-position state))))
          (multiple-value-bind (position incomplete count)
              (sexp-store:log-map
               (lambda (form)
                 (if header
                     (session-store--replay-entry state form)
                     (progn
                       (unless (and (session-store--shape-p form '(:version :id))
                                    (eq (first form) ':session-header)
                                    (eql (getf (rest form) :version) 1)
                                    (equal (getf (rest form) :id) (session-store-id store)))
                         (session-store--reject ':corrupt-header))
                       (setf header t))))
               path :start-position (session-store-state-position state))
            (declare (ignore count))
            (unless header (session-store--reject ':corrupt-header))
            (when incomplete
              (sexp-store:log-repair-tail path))
            (setf (session-store-state-position state) position
                  (session-store-state-information state)
                  (ls-compat.posix:file-information path)
                  (session-store-cache store) (unless incomplete state))
            (funcall function (session-store-state-records state)
                     (session-store-state-active state) (session-store-state-events state)
                     (session-store-state-turns state) (session-store-state-calls state)))))
    (session-store-error (condition)
      (setf (session-store-cache store) nil)
      (error condition))
    (error ()
      (setf (session-store-cache store) nil)
      (session-store--reject ':storage-failure))))

(declaim (ftype (function (session-store list &optional t) null)
                session-store--commit))

(defun session-store--commit (store data &optional continuation)
  "Publish one record and its deduplication identity in the same log entry."
  (sexp-store:log-append
   (session-store-pathname store)
   (list ':session-entry :version 1 :record data :continuation continuation)
   :repair-tail-p nil)
  nil)

(declaim (ftype (function (integer hash-table) string) session-store--parent-event))

(defun session-store--parent-event (sequence events)
  "Choose a parent event ID that no prior agent append has used."
  (loop for suffix from 0
        for event = (format nil "parent/~D/~D" sequence suffix)
        unless (gethash event events) return event))

(declaim (ftype (function (session-store string string &key (:continuation-sequence t))
                         cl-protobufs.autolith.parent.v1:turn-start)
                session-store-start-turn))

(defun session-store-start-turn (store turn-id text &key continuation-sequence)
  "Commit input before returning the TurnStart that the parent can dispatch."
  (unless (and (session-store--identifier-p turn-id) (stringp text))
    (session-store--reject ':invalid-turn))
  (session-store--locked
   store
   (lambda (records active events turns calls)
     (declare (ignore calls))
     (when (or active (gethash turn-id turns))
       (session-store--reject ':turn-conflict))
     (when (and continuation-sequence
                (not (and (integerp continuation-sequence)
                          (<= 1 continuation-sequence (length records)))))
       (session-store--reject ':invalid-sequence))
     (let ((sequence (1+ (length records))))
       (session-store--commit
        store (list sequence (session-store--parent-event sequence events) turn-id
                    (if continuation-sequence ':note ':user-input)
                    (list ':map (list (list "text" (list ':string text)))) ':parent)
        continuation-sequence)
       (cl-protobufs.autolith.parent.v1:make-turn-start
        :turn-id turn-id :input-sequence (or continuation-sequence sequence)
        :continuation (not (null continuation-sequence)))))))

(declaim (ftype (function (session-store string keyword &key (:reason t)) integer)
                session-store-finish-turn))

(defun session-store-finish-turn (store turn-id outcome &key reason)
  "Commit a parent-owned outcome for the active turn and return its sequence."
  (unless (and (member outcome '(:completed :cancelled :failed :interrupted))
               (or (null reason) (stringp reason)))
    (session-store--reject ':invalid-outcome))
  (session-store--locked
   store
   (lambda (records active events turns calls)
     (declare (ignore turns calls))
     (unless (equal active turn-id) (session-store--reject ':inactive-turn))
     (let ((sequence (1+ (length records))))
       (session-store--commit
        store (list sequence (session-store--parent-event sequence events) turn-id ':turn-outcome
                    (list ':map
                          (append (list (list "outcome"
                                              (list ':string (string-downcase outcome))))
                                  (when reason (list (list "reason" (list ':string reason))))))
                    ':parent))
       sequence))))

(declaim (ftype (function (session-store &key (:reason string)) t)
                session-store-interrupt))

(defun session-store-interrupt (store &key (reason "Agent terminated; tool effects may be unknown."))
  "Close an active turn without retrying any unfinished external operation."
  (session-store--locked
   store
   (lambda (records active events turns calls)
     (declare (ignore turns calls))
     (when active
       (let ((sequence (1+ (length records))))
         (session-store--commit
          store (list sequence (session-store--parent-event sequence events) active ':turn-outcome
                      (list ':map (list (list "outcome" (list ':string "interrupted"))
                                        (list "reason" (list ':string reason)))) ':parent))
         sequence)))))

(declaim (ftype (function (t string &key (:recover-interrupted boolean)) session-store)
                session-store-open))

(defun session-store-open (pathname session-id &key (recover-interrupted t))
  "Open a private session log or create it with SESSION-ID.

Recovery closes an unfinished turn as interrupted. Unfinished tool starts keep
their unknown-effect status; this API never retries a tool. Use recovery once
when taking ownership after termination, not when opening a concurrent handle."
  (unless (session-store--identifier-p session-id)
    (session-store--reject ':invalid-session-id))
  (let* ((path  (merge-pathnames pathname))
         (store (make-instance 'session-store :pathname path :id session-id
                               :lock-pathname (pathname (concatenate 'string
                                                          (namestring path) ".lock")))))
    (ensure-directories-exist path)
    (handler-case
        (ls-flock:with-file-lock ((session-store-lock-pathname store))
          (unless (probe-file path)
            (sexp-store:log-write path
                                 (list (list ':session-header :version 1 :id session-id))
                                 :require-absent t)))
      (error () (session-store--reject ':storage-failure)))
    (session-store--locked store
                          (lambda (records active events turns calls)
                            (declare (ignore records active events turns calls))
                            nil))
    (when recover-interrupted (session-store-interrupt store))
    store))

(declaim (ftype (function (session-store t) cl-protobufs.autolith.parent.v1:session-ack)
                session-store-append))

(defun session-store-append (store request)
  "Validate an agent append and return its durable acknowledgement.

Check duplicate event content before active-turn validation. Reject forged
metadata even for duplicate events. Tool starts must commit before execution."
  (unless (typep request 'cl-protobufs.autolith.parent.v1:session-append)
    (session-store--reject ':invalid-request))
  (let* ((data (session-store--record-data
                (cl-protobufs.autolith.parent.v1:session-append.record request)))
         (event (second data)) (turn (third data))
         (kind (fourth data)) (payload (fifth data)))
    (unless (and (eql (first data) 0) (eq (sixth data) ':origin-unspecified))
      (session-store--reject ':forged-metadata))
    (unless (and (session-store--identifier-p event)
                 (session-store--identifier-p turn)
                 (member kind '(:assistant-message :tool-started :tool-finished :note)))
      (session-store--reject ':invalid-record))
    (session-store--payload-check kind payload)
    (session-store--locked
     store
     (lambda (records active events turns calls)
       (declare (ignore turns))
       (let ((prior (gethash event events)))
         (when prior
           (unless (and (eq (sixth prior) ':agent)
                        (session-store--exact-p (session-store--identity data)
                                                (session-store--identity prior)))
             (session-store--reject ':event-conflict))
           (return-from session-store-append
             (cl-protobufs.autolith.parent.v1:make-session-ack
              :event-id event :sequence (first prior)))))
       (unless (equal active turn) (session-store--reject ':inactive-turn))
       (when (member kind '(:tool-started :tool-finished))
         (let* ((call  (second (session-store--field payload "call_id")))
                (prior (gethash call calls)))
           (if (eq kind ':tool-started)
               (when prior (session-store--reject ':call-conflict))
               (unless (and prior (equal (first prior) turn)
                            (eq (second prior) ':started))
                 (session-store--reject ':unmatched-tool-finish)))))
       (setf (first data) (1+ (length records)) (sixth data) ':agent)
       (session-store--commit store data)
       (cl-protobufs.autolith.parent.v1:make-session-ack
        :event-id event :sequence (first data))))))

(declaim (ftype (function (session-store &key (:event-id string) (:turn-id string)
                            (:request-id t) (:attempt integer) (:complete t)
                            (:request-kind keyword) (:request-purpose keyword))
                          cl-protobufs.autolith.parent.v1:session-ack)
                session-store-model-result))

(defun session-store-model-result (store &key event-id turn-id request-id (attempt 1) complete
                                            (request-kind ':generate) (request-purpose ':ordinary))
  "Commit a parent ModelComplete note for the active turn.

Exact retries return the first acknowledgement even after turn closure. New
notes require an active turn. This call has no agent RPC."
  (session-store--write-model-result
   store :event-id event-id :turn-id turn-id :request-id request-id
   :attempt attempt :complete complete
   :request-kind request-kind :request-purpose request-purpose
   :turn-check (lambda (active turns)
                 (declare (ignore turns))
                 (equal active turn-id))))

(declaim (ftype (function (session-store &key (:event-id string) (:turn-id string)
                            (:request-id t) (:attempt integer) (:complete t)
                            (:request-kind (or null keyword)) (:request-purpose (or null keyword)))
                          cl-protobufs.autolith.parent.v1:session-ack)
                session-store--recover-model-result))

(defun session-store--recover-model-result
    (store &key event-id turn-id request-id (attempt 1) complete request-kind request-purpose)
  "Publish a verified model outbox result for an existing turn.

The trusted parent caller must obtain this result from
MODEL-REPLAY-STORE-PENDING-PROJECTIONS and check the owning session identity
before calling. Accept closed turns without changing turn lifecycle. Publish
before acknowledging the model journal projection. Do not expose this operation
to the agent. Exact retry, conflict, payload, and size rules match normal writes."
  (session-store--write-model-result
   store :event-id event-id :turn-id turn-id :request-id request-id
   :attempt attempt :complete complete
   :request-kind request-kind :request-purpose request-purpose
   :turn-check (lambda (active turns)
                 (declare (ignore active))
                 (not (null (gethash turn-id turns))))))

(declaim (ftype (function (session-store &key (:event-id string) (:turn-id string)
                            (:request-id t) (:attempt integer) (:complete t)
                            (:request-kind (or null keyword)) (:request-purpose (or null keyword))
                            (:turn-check function))
                          cl-protobufs.autolith.parent.v1:session-ack)
                session-store--write-model-result))

(defun session-store--write-model-result
    (store &key event-id turn-id request-id (attempt 1) complete turn-check request-kind request-purpose)
  "Apply shared model note validation, deduplication, and publication rules."
  (when (typep request-id '(unsigned-byte 64))
    (setf request-id (format nil "~D" request-id)))
  (unless (and (session-store--identifier-p event-id)
               (session-store--identifier-p turn-id)
               (session-store--identifier-p request-id)
               (typep attempt '(integer 1 #xffffffff)))
    (session-store--reject ':invalid-request))
  (unless (or (and (null request-kind) (null request-purpose))
              (and (member request-kind '(:generate :native-compaction))
                   (member request-purpose '(:ordinary :summary))
                   (or (eq request-kind ':generate) (eq request-purpose ':ordinary))))
    (session-store--reject ':invalid-request-classification))
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "turn_id" payload) turn-id
          (gethash "request_id" payload) request-id
          (gethash "attempt" payload) attempt
          (gethash "model_result" payload) (session-store-model-complete-data complete))
    (when request-kind
      (setf (gethash "request_kind" payload)
            (ecase request-kind (:generate
                                 "generate") (:native-compaction
                                              "native_compaction"))
            (gethash "request_purpose" payload)
            (ecase request-purpose (:ordinary
                                    "ordinary") (:summary
                                                 "summary"))))
    (let* ((data (list 0 event-id turn-id ':note
                       (session-store--value-data (protocol-data->value payload)) ':parent))
           (key  (list ':model-result turn-id request-id)))
      (session-store--model-payload-check (fifth data))
      (session-store--locked
       store
       (lambda (records active events turns calls)
          (declare (ignore calls))
         (let ((prior (or (gethash key events) (gethash event-id events))))
           (when prior
             (unless (and (eq (sixth prior) ':parent)
                          (session-store--exact-p (rest prior) (rest data)))
               (session-store--reject ':model-result-conflict))
             (return-from session-store--write-model-result
              (cl-protobufs.autolith.parent.v1:make-session-ack
               :event-id (copy-seq event-id) :sequence (first prior)))))
         (unless (funcall turn-check active turns)
           (session-store--reject ':inactive-turn))
         (setf (first data) (1+ (length records)))
         (let* ((record (session-store--data-record data))
                (page   (cl-protobufs.autolith.parent.v1:make-session-page
                         :records (list record) :high-water-sequence (first data))))
           (when (> (length (cl-protobufs:serialize-to-bytes page)) (* 8 1024 1024))
             (session-store--reject ':resource-limit)))
         (session-store--commit store data)
         (cl-protobufs.autolith.parent.v1:make-session-ack
          :event-id (copy-seq event-id) :sequence (first data)))))))

(declaim (ftype (function (session-store t &key (:high-water-sequence t) (:max-bytes integer))
                         cl-protobufs.autolith.parent.v1:session-page)
                session-store-read))

(defun session-store-read (store request &key high-water-sequence (max-bytes (* 8 1024 1024)))
  "Read an ascending bounded page from one snapshot.

Pass the returned high-water sequence on subsequent pages to keep the same
snapshot. MAX-BYTES limits the encoded SessionPage, not its outer Envelope;
the caller must reserve framing and envelope space before calling this API."
  (unless (typep request 'cl-protobufs.autolith.parent.v1:session-read)
    (session-store--reject ':invalid-request))
  (let ((after (cl-protobufs.autolith.parent.v1:session-read.after-sequence request))
        (limit (cl-protobufs.autolith.parent.v1:session-read.limit request)))
    (unless (and (<= 1 limit 256) (plusp max-bytes))
      (session-store--reject ':resource-limit))
    (session-store--locked
     store
     (lambda (records active events turns calls)
       (declare (ignore active events turns calls))
       (let* ((high     (or high-water-sequence (length records)))
              (selected nil)
              (page     (cl-protobufs.autolith.parent.v1:make-session-page
                         :high-water-sequence high :has-more nil)))
         (unless (and (integerp high) (<= 0 high (length records)) (<= after high))
           (session-store--reject ':invalid-sequence))
         (labels ((fits-p ()
                    (<= (length (cl-protobufs:serialize-to-bytes page)) max-bytes)))
           (unless (fits-p) (session-store--reject ':resource-limit))
           (loop for index from after below high
                 for record = (aref records index)
                 do
            (when (< after (first record) (1+ high))
              (when (= (length selected) limit) (return))
              (let ((candidate (append selected (list (session-store--data-record record)))))
                (setf (cl-protobufs.autolith.parent.v1:session-page.records page) candidate
                      (cl-protobufs.autolith.parent.v1:session-page.has-more page)
                      (< (first record) high))
                (unless (fits-p)
                  (unless selected (session-store--reject ':resource-limit))
                  (setf (cl-protobufs.autolith.parent.v1:session-page.records page) selected
                        (cl-protobufs.autolith.parent.v1:session-page.has-more page) t)
                  (return))
                (setf selected candidate))))
           page))))))

(declaim (ftype (function (session-store) list) session-store-pending-tools))

(defun session-store-pending-tools (store)
  "Return unfinished call IDs, turn IDs, start sequences, and effects-unknown state.

This is diagnostic data, not a queue of operations to retry."
  (session-store--locked
   store
   (lambda (records active events turns calls)
     (declare (ignore records active events turns))
     (sort (loop for call being the hash-keys of calls using (hash-value state)
                 when (eq (second state) ':started)
                  collect (list :call-id (copy-seq call) :turn-id (copy-seq (first state))
                                 :start-sequence (third state) :effects ':unknown))
           #'< :key (lambda (entry) (getf entry :start-sequence))))))
