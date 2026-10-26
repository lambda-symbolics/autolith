;;;; -- Private Model Replay Journal --

(in-package #:autolith-supervisor)

(define-condition model-replay-store-error (error)
  ((code :initarg :code :reader model-replay-store-error-code
         :documentation "A fixed code that contains no private provider data."))
  (:documentation "A rejected replay operation or an unreadable private journal.")
  (:report (lambda (condition stream)
             (format stream "Model replay operation failed: ~A"
                     (model-replay-store-error-code condition)))))

(defclass model-replay-store ()
  ((pathname :initarg :pathname :reader model-replay-store-pathname
             :documentation "The private journal pathname.")
   (session :initarg :session :reader model-replay-store-session
            :documentation "The attached session identity.")
   (lock-pathname :initarg :lock-pathname :reader model-replay-store-lock-pathname
                  :documentation "The common interprocess lock pathname.")
   (cache :initform nil :accessor model-replay-store-cache
          :documentation "Incrementally validated state, used under the file lock."))
  (:documentation "An append-only parent journal with a write-ahead projection outbox.

ACCEPT commits the original items and the exact public completion projection in
this journal. A separate session journal is not part of that transaction. Publish
each pending projection there with its deterministic note identity, then call
MARK-PROJECTED. COMPLETION refuses to return a terminal projection before that
acknowledgment. A crash between publication and acknowledgment repeats the same
note identity. The trusted caller must deduplicate it in the session journal.
Publication follows sexp-store's filesystem contract, not a stronger power-loss
promise. Opening a store recovers unfinished attempts; do not open a second
recovery owner while a live provider request uses the first one. Never modify the
journal outside this API."))

(declaim (ftype (function (keyword) nil) model-replay-store--reject))

(defun model-replay-store--reject (code)
  "Signal a content-free failure."
  (error 'model-replay-store-error :code code))

(declaim (ftype (function (t) string) model-replay-store--id))

(defun model-replay-store--id (value)
  "Validate an identity and return a detached copy."
  (unless (and (stringp value) (< 0 (length value) 1025))
    (model-replay-store--reject ':invalid-identity))
  (copy-seq value))

(declaim (ftype (function (t &key (:decode boolean)) t) model-replay-store--portable))

(defun model-replay-store--portable (value &key decode)
  "Copy bounded portable values through an exact readable tagged representation.

Library NIL is JSON null, :JSON-FALSE is JSON false, and T is JSON true.
Lists and vectors are distinct. Raw strings, including malformed argument JSON,
are never parsed or repaired. Also preserve protocol null and byte vectors."
  (let ((nodes 65536) (characters 0) (active (make-hash-table :test 'eq)))
    (labels ((visit (item depth)
               (when (or (> depth 32) (minusp (decf nodes)))
                 (model-replay-store--reject ':resource-limit))
               (when (stringp item)
                 (incf characters (length item))
                 (when (> characters 2097152) (model-replay-store--reject ':resource-limit)))
               (when (or (consp item) (hash-table-p item)
                         (and (vectorp item) (not (stringp item))))
                 (when (gethash item active) (model-replay-store--reject ':invalid-value))
                 (setf (gethash item active) t))
               (unwind-protect
                    (if decode
                        (decode-item item depth)
                        (encode-item item depth))
                 (remhash item active)))

             (encode-item (item depth)
               (typecase item
                 (string
                  (list ':string (copy-seq item)))
                 (hash-table
                  (unless (eq (hash-table-test item) 'equal)
                    (model-replay-store--reject ':invalid-value))
                  (list ':map
                        (sort (loop for key being the hash-keys of item using (hash-value child)
                                    do (unless (stringp key)
                                         (model-replay-store--reject ':invalid-value))
                                    collect (list (visit key (1+ depth))
                                                  (visit child (1+ depth))))
                              #'string< :key (lambda (pair) (second (first pair))))))
                 ((array (unsigned-byte 8) (*))
                  (incf characters (length item))
                  (when (> characters 2097152) (model-replay-store--reject ':resource-limit))
                  (list ':bytes (coerce item 'list)))
                 (vector
                  (list ':vector (map 'list (lambda (child) (visit child (1+ depth))) item)))
                 (cons
                  (unless (ignore-errors (list-length item))
                    (model-replay-store--reject ':invalid-value))
                  (list ':list (mapcar (lambda (child) (visit child (1+ depth))) item)))
                 ((signed-byte 64)
                  (list ':integer item))
                 (double-float
                  (when (or (sb-ext:float-nan-p item) (sb-ext:float-infinity-p item))
                    (model-replay-store--reject ':invalid-value))
                  (list ':double item))
                 (otherwise
                  (if (member item '(nil t :json-false :protocol-null))
                      (list ':atom item)
                      (model-replay-store--reject ':invalid-value)))))

             (decode-item (item depth)
               (unless (and (consp item) (ignore-errors (= (list-length item) 2)))
                 (model-replay-store--reject ':corrupt-log))
               (let ((body (second item)))
                 (case (first item)
                   (:string
                    (unless (stringp body) (model-replay-store--reject ':corrupt-log))
                    (incf characters (length body))
                    (when (> characters 2097152) (model-replay-store--reject ':resource-limit))
                    (copy-seq body))
                   (:atom
                    (unless (member body '(nil t :json-false :protocol-null))
                      (model-replay-store--reject ':corrupt-log))
                    body)
                   (:integer
                    (unless (typep body '(signed-byte 64))
                      (model-replay-store--reject ':corrupt-log))
                    body)
                   (:double
                    (unless (and (typep body 'double-float)
                                 (not (sb-ext:float-nan-p body))
                                 (not (sb-ext:float-infinity-p body)))
                      (model-replay-store--reject ':corrupt-log))
                    body)
                   ((:map :vector :list :bytes)
                    (unless (and (listp body) (ignore-errors (list-length body)))
                      (model-replay-store--reject ':corrupt-log))
                    (case (first item)
                      (:map
                       (let ((result (make-hash-table :test 'equal)))
                         (dolist (pair body result)
                          (unless (and (consp pair) (eql 2 (ignore-errors (list-length pair))))
                             (model-replay-store--reject ':corrupt-log))
                          (let ((key (visit (first pair) (1+ depth))))
                            (unless (and (stringp key) (not (nth-value 1 (gethash key result))))
                              (model-replay-store--reject ':corrupt-log))
                            (setf (gethash key result) (visit (second pair) (1+ depth)))))))
                      (:bytes
                       (unless (and (<= (length body) 2097152)
                                    (every (lambda (byte) (typep byte '(unsigned-byte 8))) body))
                         (model-replay-store--reject ':corrupt-log))
                       (incf characters (length body))
                       (when (> characters 2097152) (model-replay-store--reject ':resource-limit))
                       (make-array (length body) :element-type '(unsigned-byte 8) :initial-contents body))
                      (otherwise
                       (let ((children (mapcar (lambda (child) (visit child (1+ depth))) body)))
                         (if (eq (first item) ':list) children (coerce children 'vector))))))
                   (otherwise
                    (model-replay-store--reject ':corrupt-log))))))
      (visit value 0))))

(declaim (ftype (function (t) t) model-replay-store--copy))

(defun model-replay-store--copy (value)
  "Return an independently owned exact portable value."
  (model-replay-store--portable (model-replay-store--portable value) :decode t))

(declaim (ftype (function (t) list) model-replay-store--public-data))

(defun model-replay-store--public-data (value)
  "Validate a public projection against shared Value and session-page bounds."
  (let ((data (model-replay-store--portable value)))
    (handler-case
        (let ((bytes (cl-protobufs:serialize-to-bytes (protocol-data->value value))))
          ;; Reserve room for the parent note identity and session record wrappers.
          (when (> (length bytes) 7340032) (model-replay-store--reject ':resource-limit)))
      (protocol-value-error () (model-replay-store--reject ':invalid-public-value)))
    data))

(declaim (ftype (function (t t) boolean) model-replay-store--exact-p))

(defun model-replay-store--exact-p (left right)
  "Compare canonical data, including string case and floating zero signs."
  (cond ((and (consp left) (consp right))
         (and (= (length left) (length right))
              (every #'model-replay-store--exact-p left right)))
        ((and (stringp left) (stringp right))
         (string= left right))
        ((and (floatp left) (floatp right))
         (and (eql left right) (eql (float-sign left) (float-sign right))))
        (t
         (eql left right))))

(declaim (ftype (function () list) model-replay-store--state))

(defun model-replay-store--state ()
  "Create private indexes and an ordered journal cache."
  (list :requests (make-hash-table :test 'equal) :handles (make-hash-table :test 'equal)
        :entries (make-array 0 :adjustable t :fill-pointer 0) :position 0 :information nil))

(declaim (ftype (function (list string integer) list) model-replay-store--attempt))

(defun model-replay-store--attempt (state request attempt)
  "Find the exact active or terminal request attempt."
  (let ((record (gethash request (getf state :requests))))
    (unless (and record (eql attempt (getf record :attempt)))
      (model-replay-store--reject ':unknown-attempt))
    record))

(declaim (ftype (function (list) hash-table) model-replay-store--terminal-event))

(defun model-replay-store--terminal-event (record)
  "Build the exact indexed complete event without publishing it."
  (let ((event (make-hash-table :test 'equal)))
    (setf (gethash "index" event) (1+ (length (getf record :events)))
          (gethash "attempt" event) (getf record :attempt)
          (gethash "complete" event)
          (model-replay-store--portable (getf record :complete) :decode t))
    event))

(declaim (ftype (function (list) null) model-replay-store--entry-shape))

(defun model-replay-store--entry-shape (entry)
  "Reject unknown journal operations and missing or additional fields."
  (let ((keys (append '(:version :request :attempt)
                      (case (first entry)
                        (:start
                         '(:turn :profile :family :maximum :delay))
                        (:item
                         '(:handle :original :output :event))
                        (:event
                         '(:event))
                        (:accept
                         '(:complete :note-id))
                        (:projected
                         '(:note-id))
                        ((:failed :interrupted)
                         nil)
                        (otherwise
                         (model-replay-store--reject ':corrupt-log))))))
    (unless (and (= (length entry) (1+ (* 2 (length keys))))
                 (loop for tail on (rest entry) by (lambda (tail) (rest (rest tail)))
                       always (member (first tail) keys)))
      (model-replay-store--reject ':corrupt-log)))
  nil)

(declaim (ftype (function (list list) null) model-replay-store--apply))

(defun model-replay-store--apply (state entry)
  "Validate and apply one complete journal operation to the indexes."
  (unless (and (listp entry) (sexp-store:record-shape-p entry)
               (eql (getf (rest entry) :version) 1))
    (model-replay-store--reject ':corrupt-log))
  (model-replay-store--entry-shape entry)
  (let* ((fields (rest entry)) (request (model-replay-store--id (getf fields :request)))
         (attempt (getf fields :attempt)) (requests (getf state :requests))
         (record (gethash request requests)))
    (unless (typep attempt '(integer 1 4294967295))
      (model-replay-store--reject ':invalid-attempt))
    (case (first entry)
      (:start
       (unless (and (= attempt (if record (1+ (getf record :attempt)) 1))
                    (or (null record) (member (getf record :status) '(:failed :interrupted))))
         (model-replay-store--reject ':attempt-conflict))
       (let ((turn    (model-replay-store--id (getf fields :turn)))
             (profile (model-replay-store--id (getf fields :profile)))
             (family  (model-replay-store--id (getf fields :family)))
             (maximum (getf fields :maximum)) (delay (getf fields :delay)))
         (unless (and (typep maximum '(integer 1 1024)) (<= attempt maximum)
                      (typep delay '(integer 0 86400000)) (or (> attempt 1) (zerop delay)))
           (model-replay-store--reject ':invalid-attempt))
         (when (and record (not (equal (list turn profile family)
                                      (list (getf record :turn) (getf record :profile)
                                            (getf record :family)))))
           (model-replay-store--reject ':scope-mismatch))
         (let ((events  (if record (getf record :events)
                            (make-array 0 :adjustable t :fill-pointer 0)))
               (event   (make-hash-table :test 'equal))
               (payload (make-hash-table :test 'equal)))
           (when (>= (length events) 65535) (model-replay-store--reject ':resource-limit))
           (setf (gethash "maximum_attempts" payload) maximum
                 (gethash "delay_milliseconds" payload) delay
                 (gethash "index" event) (1+ (length events))
                 (gethash "attempt" event) attempt
                 (gethash "attempt_started" event) payload)
           (vector-push-extend (model-replay-store--public-data event) events)
           (setf (gethash request requests)
                 (list :request request :attempt attempt :turn turn :profile profile :family family
                       :status ':running :items (make-array 0 :adjustable t :fill-pointer 0)
                       :complete nil :note-id nil :projected nil :accept-sequence nil :events events)))))
      (otherwise
       (setf record (model-replay-store--attempt state request attempt))
       (case (first entry)
         ((:item :event)
          (unless (eq (getf record :status) ':running)
            (model-replay-store--reject ':inactive-attempt))
          (when (>= (length (getf record :events)) 65535)
            (model-replay-store--reject ':resource-limit))
          (let ((event (model-replay-store--portable (getf fields :event) :decode t)))
            (unless (and (hash-table-p event)
                         (eql (gethash "index" event) (1+ (length (getf record :events))))
                         (eql (gethash "attempt" event) attempt))
              (model-replay-store--reject ':invalid-event))
            (unless (and (= (hash-table-count event) 3)
                         (loop for key being the hash-keys of event
                               always (member key
                                              (if (eq (first entry) ':item)
                                                  '("index" "attempt" "item_completed")
                                                  '("index" "attempt" "text_delta" "tool_call_delta"
                                                    "metadata" "attempt_failed" "progress"))
                                              :test #'equal)))
              (model-replay-store--reject ':invalid-event))
            (model-replay-store--public-data event)
            (when (eq (first entry) ':item)
              (let* ((handle (model-replay-store--id (getf fields :handle)))
                     (output (model-replay-store--portable (getf fields :output) :decode t))
                     (items  (getf record :items)))
                (when (or (>= (length items) 4096) (gethash handle (getf state :handles)))
                  (model-replay-store--reject ':resource-limit))
                (unless (and (hash-table-p output)
                             (equal (gethash "replay_handle" output) handle)
                             (equal (gethash "family" output) (getf record :family))
                             (eql (gethash "item_index" output) (length items))
                             (model-replay-store--exact-p (getf fields :output)
                                                          (model-replay-store--portable
                                                           (gethash "item_completed" event))))
                  (model-replay-store--reject ':invalid-item))
                (unless (hash-table-p (model-replay-store--portable
                                       (getf fields :original) :decode t))
                  (model-replay-store--reject ':invalid-original))
                (let ((item (list :handle handle :original (getf fields :original)
                                  :output (getf fields :output) :owner record)))
                  (setf (gethash handle (getf state :handles)) item)
                  (vector-push-extend item items))))
            (vector-push-extend (getf fields :event) (getf record :events))))
         ((:failed :interrupted)
          (unless (eq (getf record :status) ':running)
            (model-replay-store--reject ':inactive-attempt))
          (setf (getf record :status) (first entry)))
         (:accept
          (unless (eq (getf record :status) ':running)
            (model-replay-store--reject ':inactive-attempt))
          (let* ((complete (model-replay-store--portable (getf fields :complete) :decode t))
                 (outputs  (and (hash-table-p complete) (gethash "output_items" complete))))
            (unless (and (vectorp outputs) (not (stringp outputs))
                         (= (length outputs) (length (getf record :items)))
                         (loop for output across outputs for item across (getf record :items)
                               always (model-replay-store--exact-p
                                       (model-replay-store--portable output) (getf item :output))))
              (model-replay-store--reject ':completion-mismatch))
            (setf (getf record :status) ':accepted
                  (getf record :accept-sequence) (length (getf state :entries))
                  (getf record :complete) (getf fields :complete)
                  (getf record :note-id) (model-replay-store--id (getf fields :note-id)))
            (model-replay-store--public-data (model-replay-store--projection record))
            (model-replay-store--public-data (model-replay-store--terminal-event record))))
         (:projected
          (unless (and (eq (getf record :status) ':accepted)
                       (equal (getf fields :note-id) (getf record :note-id)))
            (model-replay-store--reject ':projection-mismatch))
          (when (>= (length (getf record :events)) 65536)
            (model-replay-store--reject ':resource-limit))
          (vector-push-extend
           (model-replay-store--public-data (model-replay-store--terminal-event record))
           (getf record :events))
          (setf (getf record :projected) t))
         (otherwise
          (model-replay-store--reject ':corrupt-log))))))
  (when (>= (length (getf state :entries)) 1000000)
    (model-replay-store--reject ':resource-limit))
  (vector-push-extend entry (getf state :entries))
  nil)

(declaim (ftype (function (model-replay-store function) t) model-replay-store--locked))

(defun model-replay-store--locked (store function)
  "Refresh only appended forms under a common file lock and run FUNCTION.

Replacement, shrink, changed metadata at the same size, torn tail, and errors
invalidate the cache. Arbitrary external in-place rewrites are outside this API."
  (handler-case
      (ls-flock:with-file-lock ((model-replay-store-lock-pathname store))
        (let* ((path        (model-replay-store-pathname store))
               (information (ls-compat.posix:file-information path))
               (cache       (model-replay-store-cache store))
               (prior       (and cache (getf cache :information)))
               (state       (if (and prior
                                     (ls-compat.posix:file-information-same-object-p prior information)
                                     (or (ls-compat.posix:file-information-unchanged-p prior information)
                                         (> (ls-compat.posix:file-information-size information)
                                            (ls-compat.posix:file-information-size prior))))
                                cache (model-replay-store--state)))
               (header      (plusp (getf state :position))))
          (multiple-value-bind (position incomplete count)
              (let ((*read-eval* nil))
                (sexp-store:log-map
                 (lambda (entry)
                   (if header
                       (model-replay-store--apply state entry)
                       (progn
                         (unless (equal entry (list ':model-replay :version 1
                                                    :session                (model-replay-store-session store)))
                           (model-replay-store--reject ':scope-mismatch))
                         (setf header t))))
                 path :start-position (getf state :position)))
            (declare (ignore count))
            (unless header (model-replay-store--reject ':corrupt-log))
            (when incomplete (sexp-store:log-repair-tail path))
            (setf (getf state :position) position
                  (getf state :information) (ls-compat.posix:file-information path)
                  (model-replay-store-cache store) (unless incomplete state))
            (funcall function state))))
    (model-replay-store-error (condition)
      (setf (model-replay-store-cache store) nil)
      (error condition))
    (error ()
      (setf (model-replay-store-cache store) nil)
      (model-replay-store--reject ':storage-failure))))

(declaim (ftype (function (t) t) model-replay-store--tree-copy))

(defun model-replay-store--tree-copy (value)
  "Copy canonical journal lists and strings before retaining caller data."
  (typecase value
    (cons
     (mapcar #'model-replay-store--tree-copy value))
    (string
     (copy-seq value))
    (otherwise
     value)))

(declaim (ftype (function (model-replay-store list list) null) model-replay-store--commit))

(defun model-replay-store--commit (store state entry)
  "Validate and publish one transition without replaying the existing journal.

Advance the cached validated offset after publication. The enclosing lock
operation discards all partial state if validation or publication fails."
  (let ((owned (model-replay-store--tree-copy entry)))
    (model-replay-store--apply state owned)
    (sexp-store:log-append (model-replay-store-pathname store) owned :repair-tail-p nil))
  (with-open-file (stream (model-replay-store-pathname store) :element-type '(unsigned-byte 8))
    (setf (getf state :position) (file-length stream)))
  (setf (getf state :information)
        (ls-compat.posix:file-information (model-replay-store-pathname store))
        (model-replay-store-cache store) state)
  nil)

(declaim (ftype (function (model-replay-store string &key (:attempt integer)
                           (:kind keyword) (:fields list)) null) model-replay-store--operation))

(defun model-replay-store--operation (store request &key attempt kind fields)
  "Commit one lifecycle operation under the common lock."
  (model-replay-store--locked
   store (lambda (state)
           (model-replay-store--commit
            store state (append (list kind :version 1 :request (model-replay-store--id request)
                                      :attempt attempt) fields)))))

(declaim (ftype (function (t string) model-replay-store) model-replay-store-open))

(defun model-replay-store-open (pathname session-id)
  "Open the session journal and durably interrupt unfinished provider attempts.

No network operation runs during recovery. Accepted, unprojected results remain
in the outbox. The caller must serialize ownership with the provider service."
  (handler-case
      (let* ((path  (uiop:ensure-absolute-pathname pathname))
             (store (make-instance 'model-replay-store :pathname path
                                   :session                      (model-replay-store--id session-id)
                                   :lock-pathname                (pathname (concatenate 'string (namestring path) ".lock")))))
        (ensure-directories-exist path)
        (ls-flock:with-file-lock ((model-replay-store-lock-pathname store))
          (unless (probe-file path)
            (sexp-store:log-write path (list (list ':model-replay :version 1
                                                  :session                 (model-replay-store-session store))))))
        (model-replay-store--locked
         store (lambda (state)
                 (maphash (lambda (request record)
                            (when (eq (getf record :status) ':running)
                              (model-replay-store--commit
                               store state (list ':interrupted :version 1 :request request
                                                 :attempt (getf record :attempt)))))
                          (getf state :requests))))
        store)
    (model-replay-store-error (condition) (error condition))
    (error () (model-replay-store--reject ':storage-failure))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:turn-id string)
                           (:profile-id string) (:family string) (:attempt integer)
                           (:maximum-attempts integer) (:delay-milliseconds integer)) hash-table)
                model-replay-store-start-attempt))

(defun model-replay-store-start-attempt (store &key request-id turn-id profile-id family (attempt 1)
                                                  (maximum-attempts 1) (delay-milliseconds 0))
  "Commit a new attempt and its public attempt_started event before HTTP.

Return the detached event for forwarding. Event indexes start at one across
all attempts. Item indexes start at zero within each attempt."
  (model-replay-store--locked
   store (lambda (state)
           (model-replay-store--commit
            store state (list ':start :version 1 :request (model-replay-store--id request-id)
                              :attempt attempt :turn (model-replay-store--id turn-id)
                              :profile (model-replay-store--id profile-id)
                              :family  (model-replay-store--id family)
                              :maximum maximum-attempts :delay delay-milliseconds))
           (let* ((record (gethash request-id (getf state :requests)))
                  (events (getf record :events)))
             (model-replay-store--portable (aref events (1- (length events))) :decode t)))))

(declaim (ftype (function () string) model-replay-store--handle))

(defun model-replay-store--handle ()
  "Return a 256-bit cryptographically random private replay identity."
  (concatenate 'string "replay/" (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)
                           (:original t) (:view hash-table)) (values string hash-table &optional))
                model-replay-store-put-item))

(defun model-replay-store-put-item (store &key request-id attempt original view)
  "Commit an exact original and its public item event before returning values.

VIEW contains exactly one typed OutputItem view field, not identity fields.
Return the handle and a detached event. Only a successfully accepted attempt
makes the handle replayable. Request and attempt identity are journaled."
  (unless (hash-table-p original) (model-replay-store--reject ':invalid-original))
  (let ((original-data (model-replay-store--portable original))
        (public        (model-replay-store--copy view)))
    (unless (and (hash-table-p public) (= (hash-table-count public) 1)
                 (loop for key being the hash-keys of public
                       always (member key '("message" "tool_call" "reasoning"
                                            "hosted_tool" "compaction" "tool_discovery_call")
                                      :test #'equal)))
      (model-replay-store--reject ':invalid-view))
    (model-replay-store--locked
     store (lambda (state)
             (let* ((record (model-replay-store--attempt state request-id attempt))
                    (handle (model-replay-store--handle))
                    (event  (make-hash-table :test 'equal)))
               (setf (gethash "replay_handle" public) handle
                     (gethash "family" public) (getf record :family)
                     (gethash "item_index" public) (length (getf record :items))
                     (gethash "index" event) (1+ (length (getf record :events)))
                     (gethash "attempt" event) attempt
                     (gethash "item_completed" event) public)
               (model-replay-store--commit
                store state (list ':item :version 1 :request request-id :attempt attempt
                                  :handle handle :original original-data
                                  :output (model-replay-store--portable public)
                                  :event  (model-replay-store--portable event)))
               (values (copy-seq handle) (model-replay-store--copy event)))))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)
                           (:kind string) (:payload t)) hash-table) model-replay-store-append-event))

(defun model-replay-store-append-event (store &key request-id attempt kind payload)
  "Commit one nonterminal public event and return a detached indexed event.

Item events use PUT-ITEM. Terminal completion uses ACCEPT and COMPLETION."
  (unless (member kind '("text_delta" "tool_call_delta" "metadata" "attempt_failed" "progress")
                  :test #'equal)
    (model-replay-store--reject ':invalid-event))
  (model-replay-store--locked
   store (lambda (state)
           (let* ((record (model-replay-store--attempt state request-id attempt))
                  (event  (make-hash-table :test 'equal)))
             (setf (gethash "index" event) (1+ (length (getf record :events)))
                   (gethash "attempt" event) attempt (gethash kind event) payload)
             (model-replay-store--commit
              store state (list ':event :version 1 :request request-id :attempt attempt
                                :event (model-replay-store--public-data event)))
             (model-replay-store--copy event)))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)) null)
                model-replay-store-fail-attempt))

(defun model-replay-store-fail-attempt (store &key request-id attempt)
  "Durably reject every handle from an unsuccessful provider attempt."
  (model-replay-store--operation store request-id :attempt attempt :kind ':failed))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)) null)
                model-replay-store-interrupt-attempt))

(defun model-replay-store-interrupt-attempt (store &key request-id attempt)
  "Commit interruption without restarting any external request."
  (model-replay-store--operation store request-id :attempt attempt :kind ':interrupted))

(declaim (ftype (function (model-replay-store string) (values keyword integer &optional))
                model-replay-store-status))

(defun model-replay-store-status (store request-id)
  "Return the latest request status and one-based attempt as two values."
  (model-replay-store--locked
   store (lambda (state)
           (let ((record (gethash request-id (getf state :requests))))
             (unless record (model-replay-store--reject ':unknown-request))
             (values (getf record :status) (getf record :attempt))))))

(declaim (ftype (function (model-replay-store list) string) model-replay-store--note-id))

(defun model-replay-store--note-id (store record)
  "Derive a stable parent note identity from the exact scoped request attempt."
  (let ((identity (with-standard-io-syntax
                    (write-to-string (list (model-replay-store-session store)
                                           (getf record :turn) (getf record :request)
                                           (getf record :attempt)) :readably t))))
    (concatenate 'string "model-result/"
                 (ironclad:byte-array-to-hex-string
                  (ironclad:digest-sequence ':sha256 (sb-ext:string-to-octets identity :external-format ':utf-8))))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)
                           (:complete hash-table)) string) model-replay-store-accept))

(defun model-replay-store-accept (store &key request-id attempt complete)
  "Commit the exact ordered ModelComplete field-name projection and outbox note.

Call only after final provider validation and cleanup succeed. The trusted
caller supplies public fields. OUTPUT_ITEMS must exactly match the journaled
ordered views and handles. Retrying the same acceptance returns the same note
identity. A changed projection fails. Do not send terminal complete yet."
  (let ((data (model-replay-store--public-data complete)))
    (unless (and (hash-table-p complete)
                 (loop for key being the hash-keys of complete
                       always (member key '("message" "usage" "reason" "continuation_handle"
                                            "output_items" "response_id" "metadata" "turn_completion"
                                            "turn_state_handle" "compaction_outcome") :test #'equal)))
      (model-replay-store--reject ':invalid-completion))
    (model-replay-store--locked
     store (lambda (state)
             (let ((record (model-replay-store--attempt state request-id attempt)))
               (if (eq (getf record :status) ':accepted)
                   (unless (model-replay-store--exact-p data (getf record :complete))
                     (model-replay-store--reject ':completion-mismatch))
                   (model-replay-store--commit
                    store state (list ':accept :version 1 :request request-id :attempt attempt
                                      :complete data :note-id
                                      (model-replay-store--note-id store record))))
               (copy-seq (getf record :note-id)))))))

(declaim (ftype (function (list) hash-table) model-replay-store--projection))

(defun model-replay-store--projection (record)
  "Return the deterministic parent model_result note delivery data."
  (let ((result (make-hash-table :test 'equal)))
    (setf (gethash "event_id" result) (copy-seq (getf record :note-id))
          (gethash "turn_id" result) (copy-seq (getf record :turn))
          (gethash "request_id" result) (copy-seq (getf record :request))
          (gethash "attempt" result) (getf record :attempt)
          (gethash "model_result" result)
          (model-replay-store--portable (getf record :complete) :decode t))
    result))

(declaim (ftype (function (model-replay-store) vector) model-replay-store-pending-projections))

(defun model-replay-store-pending-projections (store)
  "Return detached results in acceptance order for session-note reconciliation."
  (model-replay-store--locked
   store (lambda (state)
           (map 'vector #'model-replay-store--projection
                (sort (loop for record being the hash-values of (getf state :requests)
                            when (and (eq (getf record :status) ':accepted)
                                      (not (getf record :projected))) collect record)
                      #'< :key (lambda (record) (getf record :accept-sequence)))))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)
                           (:note-id string)) null) model-replay-store-mark-projected))

(defun model-replay-store-mark-projected (store &key request-id attempt note-id)
  "Acknowledge durable idempotent delivery to the separate session journal.

The trusted caller must first publish the matching model_result parent note.
Repeating the acknowledgment does not create another journal entry."
  (model-replay-store--locked
   store (lambda (state)
           (let ((record (model-replay-store--attempt state request-id attempt)))
             (unless (equal note-id (getf record :note-id))
               (model-replay-store--reject ':projection-mismatch))
             (unless (getf record :projected)
               (model-replay-store--commit
                store state (list ':projected :version 1 :request request-id :attempt attempt
                                  :note-id note-id)))))))

(declaim (ftype (function (model-replay-store &key (:request-id string) (:attempt integer)) hash-table)
                model-replay-store-completion))

(defun model-replay-store-completion (store &key request-id attempt)
  "Return a detached terminal projection only after durable projection acknowledgment."
  (model-replay-store--locked
   store (lambda (state)
           (let ((record (model-replay-store--attempt state request-id attempt)))
             (unless (and (eq (getf record :status) ':accepted) (getf record :projected))
               (model-replay-store--reject ':projection-required))
             (model-replay-store--portable (getf record :complete) :decode t)))))

(declaim (ftype (function (model-replay-store string &key (:session-id string)
                           (:profile-id string) (:family string)) t) model-replay-store-resolve))

(defun model-replay-store-resolve (store handle &key session-id profile-id family)
  "Return a detached original only for an accepted, projected, correctly scoped handle."
  (model-replay-store--locked
   store (lambda (state)
           (let* ((item  (gethash handle (getf state :handles)))
                  (owner (getf item :owner)))
             (unless (and item (eq (getf owner :status) ':accepted)
                          (getf owner :projected)
                          (equal session-id (model-replay-store-session store))
                          (equal profile-id (getf owner :profile))
                          (equal family (getf owner :family)))
               (model-replay-store--reject ':invalid-handle))
             (model-replay-store--portable (getf item :original) :decode t)))))

(declaim (ftype (function (model-replay-store string) vector) model-replay-store-events))

(defun model-replay-store-events (store request-id)
  "Return detached immutable public event history across all request attempts."
  (model-replay-store--locked
   store (lambda (state)
           (let ((record (gethash request-id (getf state :requests))))
             (unless record (model-replay-store--reject ':unknown-request))
             (map 'vector (lambda (event) (model-replay-store--portable event :decode t))
                  (getf record :events))))))

(declaim (ftype (function (model-replay-store) null) model-replay-store-compact))

(defun model-replay-store-compact (store)
  "Atomically republish the retained journal with no torn tail.

Retain all originals, public events, and acceptance/projection evidence. This
conservative compaction never invalidates a handle referenced by session history.
It normalizes journal publication without claiming to reclaim retained history."
  (model-replay-store--locked
   store (lambda (state)
           (sexp-store:log-write
            (model-replay-store-pathname store)
            (cons (list ':model-replay :version 1 :session (model-replay-store-session store))
                  (coerce (getf state :entries) 'list)))
           (setf (model-replay-store-cache store) nil)
           nil)))
