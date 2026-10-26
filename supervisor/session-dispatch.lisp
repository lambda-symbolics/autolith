;;;; -- Parent Session Wire Dispatcher --

(in-package #:autolith-supervisor)

(defclass session-runtime ()
  ((service :initarg :service :initform nil :reader session-runtime-service
            :documentation "Optional parent model service; NIL keeps synchronous session-only behavior.")
   (store :initarg :store :reader session-runtime-store
          :documentation "The attached parent-owned durable session.")
   (connection :initarg :connection :reader session-runtime-connection
               :documentation "The initialized private connection.")
   (lock :initform (bordeaux-threads:make-lock "parent session")
         :reader session-runtime-lock :documentation "Serializes service operations, not deadline enforcement.")
   (lifecycle-lock :initform      (bordeaux-threads:make-lock "parent session lifecycle")
                   :reader        session-runtime-lifecycle-lock
                   :documentation "Protects short phase transitions; never held during I/O or callbacks.")
   (active-receives :initform 0 :accessor session-runtime-active-receives
                    :documentation "Receive calls that must unwind before close cleanup completes.")
   (receive-finished :initform      (bordeaux-threads:make-condition-variable)
                     :reader        session-runtime-receive-finished
                     :documentation "Notifies cleanup when receive calls have unwound.")
   (cleanup-thread :initform nil :accessor session-runtime-cleanup-thread
                   :documentation "At most one asynchronous close worker.")
   (cleanup-error :initform nil :accessor session-runtime-cleanup-error
                  :documentation "A fixed cleanup failure code, reported by AWAIT-CLOSE.")
   (phase :initform ':running :accessor session-runtime-phase
          :documentation "RUNNING, STOPPING, STOPPED, or CLOSED.")
   (turn-id :initform nil :accessor session-runtime-turn-id
            :documentation "The active parent-assigned turn identity.")
   (turn-request :initform nil :accessor session-runtime-turn-request
                 :documentation "The outstanding TurnStart message identifier.")
   (stop-request :initform nil :accessor session-runtime-stop-request
                 :documentation "The outstanding PrepareStop message identifier.")
   (deadline :initform nil :accessor session-runtime-deadline
             :documentation "The monotonic graceful-stop deadline.")
   (acknowledged :initarg :initial-sequence :accessor session-runtime-acknowledged
                 :documentation "The greatest session sequence supplied to this agent.")
   (page-after :initform nil :accessor session-runtime-page-after
               :documentation "Expected next sequence for the one sequential read snapshot.")
   (page-high :initform nil :accessor session-runtime-page-high
              :documentation "Snapshot maximum for the sequential read cursor, or NIL.")
   (append-acks :initform (make-hash-table :test 'equal) :reader session-runtime-append-acks
                :documentation "Event IDs acknowledged on this connection, with assigned sequences.")
   (terminal-requests :initform (make-hash-table :test 'eql) :reader session-runtime-terminal-requests
                      :documentation "Terminal model/access request IDs retained for repeated cancellation.")
   (output-callback :initarg :output-callback :reader session-runtime-output-callback
                    :documentation "Trusted callback of channel and sanitized, labeled untrusted text.")
   (clock :initarg :clock :reader session-runtime-clock
          :documentation "Parent monotonic seconds function.")
   (append-window :initform 0 :accessor session-runtime-append-window
                  :documentation "Start of the current storage-write rate window.")
   (append-count :initform 0 :accessor session-runtime-append-count
                 :documentation "Append requests in the current one-second window, limited to 64.")
   (append-bytes :initform 0 :accessor session-runtime-append-bytes
                 :documentation "Encoded append bytes in the current window, limited to 8 MiB.")
   (output-count :initform 0 :accessor session-runtime-output-count
                 :documentation "Output messages in the current one-second window, limited to 128.")
   (output-window :initform 0 :accessor session-runtime-output-window
                  :documentation "Start of the current output-rate window.")
   (output-bytes :initform 0 :accessor session-runtime-output-bytes
                 :documentation "Sanitized byte count in the current window, limited to 65536."))
  (:documentation "Mandatory session.v1 service with no model, record, or access authority.
Call RECEIVE in one reader loop. Lifecycle calls may run on other threads. Output
callbacks receive only transient, untrusted text; render it outside approval UI.
No output queue is retained. Call EXPIRE from the parent's deadline scheduler.
After EXPIRE or DISCONNECT, stop the child tree, then AWAIT-CLOSE before reusing
this session store. Deadline enforcement never waits for peer I/O or callbacks."))

(defgeneric session-runtime--operation (service runtime function)
  (:documentation "Run FUNCTION under the session lock or on the installed service owner."))

(defmethod session-runtime--operation ((service null) runtime function)
  "Keep the synchronous session-only path."
  (bordeaux-threads:with-lock-held ((session-runtime-lock runtime))
    (funcall function)))

(defmacro with-session-runtime-owner ((runtime) &body body)
  "Evaluate BODY on RUNTIME's owner. Evaluate RUNTIME once."
  (let ((value (gensym "RUNTIME")))
    `(let ((,value ,runtime))
       (session-runtime--operation (session-runtime-service ,value) ,value (lambda () ,@body)))))

(defgeneric session-runtime--incoming (service runtime request &key bytes)
  (:documentation "Admit one envelope from the sole reader. BYTES bounds retained input."))

(defmethod session-runtime--incoming ((service null) runtime request &key bytes)
  "Dispatch a session-only envelope synchronously."
  (declare (ignore bytes))
  (with-session-runtime-owner (runtime)
    (session-runtime--dispatch runtime request)))

(defgeneric session-runtime--send (service runtime envelope)
  (:documentation "Send now or reserve an ordered writer-queue entry and its message ID."))

(defmethod session-runtime--send ((service null) runtime envelope)
  "Send synchronously without an installed model service."
  (parent-connection-send (session-runtime-connection runtime) envelope))

(defgeneric session-runtime--after-send (service runtime function)
  (:documentation "Apply delivery-dependent history only after the preceding frame succeeds."))

(defmethod session-runtime--after-send ((service null) runtime function)
  "The synchronous send has already succeeded."
  (declare (ignore runtime))
  (funcall function))

(defgeneric session-runtime--attach (service runtime)
  (:documentation "Attach an optional service to one runtime before accepting work."))

(defmethod session-runtime--attach ((service null) runtime)
  "No optional service is installed."
  (declare (ignore runtime))
  nil)

(defgeneric session-runtime--fence (service runtime reason &key turn-id)
  (:documentation "Latch cancellation outside storage and I/O locks, then notify the owner."))

(defmethod session-runtime--fence ((service null) runtime reason &key turn-id)
  "Session-only runtimes have no provider work."
  (declare (ignore runtime reason turn-id))
  nil)

(defgeneric session-runtime--quiesce (service runtime)
  (:documentation "Join all optional service workers before durable session-store reuse."))

(defmethod session-runtime--quiesce ((service null) runtime)
  "No service workers require joining."
  (declare (ignore runtime))
  nil)

(defgeneric session-runtime--model-dispatch (service runtime request)
  (:documentation "Handle optional model and cancellation operations. Return true if handled."))

(defmethod session-runtime--model-dispatch ((service null) runtime request)
  "Leave optional operations to the session-only rejection path."
  (declare (ignore runtime request))
  nil)

(defgeneric session-runtime--model-expire (service runtime)
  (:documentation "Latch a parent model deadline without waiting for storage or peer I/O."))

(defmethod session-runtime--model-expire ((service null) runtime)
  "No model deadline exists."
  (declare (ignore runtime))
  nil)

(declaim (ftype (function () real) session-runtime--clock))

(defun session-runtime--clock ()
  "Return monotonic seconds in this parent process."
  (/ (get-internal-real-time) internal-time-units-per-second))

(declaim (ftype (function (session-store parent-connection &key (:features list)
                           (:output-callback (or null function)) (:clock function)
                           (:initial-sequence integer) (:resume-page (or null parent-proto:session-page))
                           (:model-service t))
                         session-runtime)
                session-runtime-open))

(defun session-runtime-open (store connection &key (features '("session.v1"))
                                                   output-callback (clock #'session-runtime--clock)
                                                   (initial-sequence 0) resume-page model-service)
  "Attach STORE to a ready CONNECTION. Supply the delivered Welcome RESUME-PAGE.
INITIAL-SEQUENCE can describe earlier delivered resume data. Select only session.v1
in Welcome. A read after the last page record continues that snapshot; another
after_sequence starts a new snapshot. Only one sequential read cursor is retained."
  (let* ((records       (when resume-page (parent-proto:session-page.records resume-page)))
         (last-sequence (if records (parent-proto:session-record.sequence (first (last records))) 0))
         (high          (parent-proto:session-page.high-water-sequence
                         (session-store-read store (parent-proto:make-session-read :limit 1)))))
    (unless (and (eq ':ready (parent-connection-phase connection))
                 (equal features (if model-service '("session.v1" "model.v1" "model-items-v1") '("session.v1")))
                 (<= 0 initial-sequence high)
                 (or (null resume-page)
                     (<= 0 last-sequence (parent-proto:session-page.high-water-sequence resume-page) high)))
      (error 'parent-protocol-error :reason ':invalid-session-runtime))
    (let ((runtime (make-instance 'session-runtime :store store :connection connection
                                 :initial-sequence (max initial-sequence last-sequence)
                                 :output-callback output-callback :clock clock :service model-service)))
      (when (and resume-page (parent-proto:session-page.has-more resume-page) records)
        (setf (session-runtime-page-after runtime) last-sequence
              (session-runtime-page-high runtime) (parent-proto:session-page.high-water-sequence resume-page)))
      (session-runtime--attach model-service runtime)
      runtime)))

(declaim (ftype (function (session-runtime) null) session-runtime--disconnect))

(defun session-runtime--disconnect (runtime)
  "Schedule the same tracked cleanup for operation failures and lifecycle closure."
  (session-runtime--fence (session-runtime-service runtime) runtime ':closed)
  (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
    (session-runtime--begin-close runtime)))

(declaim (ftype (function (session-runtime) null) session-runtime--cleanup))

(defun session-runtime--cleanup (runtime)
  "Close streams, then serialize durable interruption after all service operations."
  (handler-case
      (unwind-protect
           (parent-connection-close (session-runtime-connection runtime))
        (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
          (loop until (zerop (session-runtime-active-receives runtime))
                do (bordeaux-threads:condition-wait (session-runtime-receive-finished runtime)
                                                  (session-runtime-lifecycle-lock runtime))))
        (session-runtime--quiesce (session-runtime-service runtime) runtime)
        (with-session-runtime-owner (runtime)
          (unwind-protect
               (session-store-interrupt (session-runtime-store runtime))
            (setf (session-runtime-turn-id runtime) nil
                  (session-runtime-turn-request runtime) nil
                  (session-runtime-stop-request runtime) nil)
            (clrhash (session-runtime-append-acks runtime))
            (clrhash (session-runtime-terminal-requests runtime)))))
    (session-store-error (condition)
      (setf (session-runtime-cleanup-error runtime) (session-runtime--store-code (session-store-error-code condition))))
    (error ()
      (setf (session-runtime-cleanup-error runtime) ':close-failed)))
  nil)

(declaim (ftype (function (session-runtime) null) session-runtime--begin-close))

(defun session-runtime--begin-close (runtime)
  "Mark closed and schedule bounded cleanup while holding only the lifecycle lock."
  (unless (eq ':closed (session-runtime-phase runtime))
    (setf (session-runtime-phase runtime) ':closed
          (parent-connection-phase (session-runtime-connection runtime)) ':closed
          (session-runtime-cleanup-thread runtime)
          (bordeaux-threads:make-thread (lambda () (session-runtime--cleanup runtime))
                                       :name "parent session close")))
  nil)

(declaim (ftype (function (session-runtime) null) session-runtime-disconnect))

(defun session-runtime-disconnect (runtime)
  "Reject further work immediately and schedule closure. Stop the child, then AWAIT-CLOSE."
  (session-runtime--fence (session-runtime-service runtime) runtime ':closed)
  (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
    (session-runtime--begin-close runtime)))

(declaim (ftype (function (session-runtime &key (:timeout (real (0)))) null) session-runtime-await-close))

(defun session-runtime-await-close (runtime &key (timeout 30))
  "Wait at most TIMEOUT seconds for close cleanup before reusing the durable store."
  (let ((thread (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
                  (session-runtime-cleanup-thread runtime))))
    (when thread
      (handler-case (sb-ext:with-timeout timeout (bordeaux-threads:join-thread thread))
        (sb-ext:timeout () (error 'parent-protocol-error :reason ':close-timeout))))
    (when (session-runtime-cleanup-error runtime)
      (error 'parent-protocol-error :reason (session-runtime-cleanup-error runtime))))
  nil)

(declaim (ftype (function (session-runtime) boolean) session-runtime-expire))

(defun session-runtime-expire (runtime)
  "Mark an expired stop closed and schedule cleanup, without waiting for peer I/O."
  (session-runtime--model-expire (session-runtime-service runtime) runtime)
  (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
    (if (and (eq ':stopping (session-runtime-phase runtime))
             (>= (funcall (session-runtime-clock runtime)) (session-runtime-deadline runtime)))
        (progn (session-runtime--begin-close runtime) t)
        nil)))

(declaim (ftype (function (session-runtime) null) session-runtime--available))

(defun session-runtime--available (runtime)
  "Require room for another outstanding parent request."
  (unless (< (+ (if (session-runtime-turn-request runtime) 1 0)
                (if (session-runtime-stop-request runtime) 1 0))
             (parent-proto:limits.max-in-flight-requests
              (parent-connection-limits (session-runtime-connection runtime))))
    (error 'parent-protocol-error :reason ':in-flight-limit))
  nil)

(declaim (ftype (function (session-runtime string string &key (:continuation-sequence t)) integer)
                session-runtime-start-turn))

(defun session-runtime-start-turn (runtime turn-id text &key continuation-sequence)
  "Reserve a turn, persist input, then send TurnStart. Never replay on send loss."
  (with-session-runtime-owner (runtime)
   (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
     (unless
         (and (eq ':running (session-runtime-phase runtime)) (null (session-runtime-turn-request runtime)))
       (error 'parent-protocol-error :reason ':not-accepting-turns))
     (session-runtime--available runtime)
     (setf (session-runtime-turn-id runtime) turn-id
           (session-runtime-turn-request runtime) 0))
   (handler-case
    (let* ((start
            (session-store-start-turn (session-runtime-store runtime) turn-id text :continuation-sequence
                                      continuation-sequence))
           (envelope (parent-proto:make-envelope :turn-start start)))
      (session-runtime--send (session-runtime-service runtime) runtime envelope)
      (progn
       (setf (session-runtime-turn-request runtime) (parent-proto:envelope.message-id envelope))
       (session-runtime--after-send (session-runtime-service runtime) runtime
        (lambda ()
          (setf (session-runtime-acknowledged runtime)
                (max (session-runtime-acknowledged runtime)
                     (parent-proto:turn-start.input-sequence start))))))
      (parent-proto:envelope.message-id envelope))
    (error (condition) (session-runtime--disconnect runtime) (error condition)))))(declaim (ftype (function (session-runtime string &key (:reason string)) boolean)
                                                                                            session-runtime-cancel-turn))

(defun session-runtime-cancel-turn (runtime turn-id &key (reason "Cancelled by parent."))
  "Send cancellation for the active turn; inactive and repeated requests are harmless."
  (session-runtime--fence (session-runtime-service runtime) runtime ':cancel-turn :turn-id turn-id)
  (with-session-runtime-owner (runtime)
   (if (and (member (session-runtime-phase runtime) '(:running :stopping))
            (equal turn-id (session-runtime-turn-id runtime)))
       (handler-case
        (progn
         (session-runtime--send (session-runtime-service runtime) runtime
          (parent-proto:make-envelope :turn-cancel
                                      (parent-proto:make-turn-cancel :turn-id turn-id :reason reason)))
         t)
        (error (condition) (session-runtime--disconnect runtime) (error condition)))
       nil)))

(declaim (ftype (function (session-runtime &key (:reason string) (:grace-milliseconds integer)
                           (:next-policy-revision integer)) integer) session-runtime-prepare-stop))

(defun session-runtime-prepare-stop
       (runtime &key (reason "Parent restart.") (grace-milliseconds 1000) (next-policy-revision 0))
  "Stop new work before waiting to send PrepareStop; return its outstanding message ID.
The parent grace interval starts at this send attempt, including backpressure."
  (unless (<= 0 grace-milliseconds 4294967295) (error 'parent-protocol-error :reason ':invalid-grace))
  (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
    (unless (eq ':running (session-runtime-phase runtime))
      (error 'parent-protocol-error :reason ':not-running))
    (session-runtime--available runtime)
    (setf (session-runtime-phase runtime) ':stopping
          (session-runtime-stop-request runtime) 0
          (session-runtime-deadline runtime)
          (+ (funcall (session-runtime-clock runtime)) (/ grace-milliseconds 1000))))
  (session-runtime--fence (session-runtime-service runtime) runtime ':stopping)
  (with-session-runtime-owner (runtime)
   (handler-case
    (let ((envelope
           (parent-proto:make-envelope :prepare-stop
                                       (parent-proto:make-prepare-stop :reason reason :grace-milliseconds
                                                                       grace-milliseconds
                                                                       :next-policy-revision
                                                                       next-policy-revision))))
      (unless (eq ':stopping (session-runtime-phase runtime))
        (error 'parent-protocol-error :reason ':stop-expired))
      (session-runtime--send (session-runtime-service runtime) runtime envelope)
      (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
        (unless (eq ':stopping (session-runtime-phase runtime))
          (error 'parent-protocol-error :reason ':stop-expired))
        (setf (session-runtime-stop-request runtime) (parent-proto:envelope.message-id envelope)))
      (parent-proto:envelope.message-id envelope))
    (error (condition) (session-runtime--disconnect runtime) (error condition)))))

(declaim (ftype (function (string) integer) session-output--bytes))

(defun session-output--bytes (text)
  "Count UTF-8 bytes without allocating another string."
  (loop for character across text for code = (char-code character)
        sum (cond ((< code #x80)
                   1)
                  ((< code #x800)
                   2)
                  ((< code #x10000)
                   3)
                  (t
                   4))))

(declaim (ftype (function (string keyword) string) session-output-sanitize))

(defun session-output-sanitize (text channel)
  "Strip controls and label untrusted lines. Limit input to 8192 characters/32768 bytes.
Truncate rendered output to 32768 UTF-8 bytes, including line labels."
  (unless (and (<= (length text) 8192) (<= (session-output--bytes text) 32768)
               (member channel '(:progress :diagnostic)))
    (error 'parent-protocol-error :reason ':invalid-output))
  (let ((state  ':text)
        (prefix (format nil "[agent ~A] " (string-downcase channel)))
        (buffer (make-array 32768 :element-type 'character :fill-pointer 0))
        (bytes  0))
    (block rendering
      (labels ((emit (character)
                 (let* ((code (char-code character))
                        (size (cond ((< code #x80)
                                     1)
                                    ((< code #x800)
                                     2)
                                    ((< code #x10000)
                                     3)
                                    (t
                                     4))))
                   (when (> (+ bytes size) 32768)
                     (return-from rendering nil))
                   (incf bytes size)
                   (vector-push character buffer)))

               (label-line ()
                 (loop for character across prefix do (emit character))))
        (label-line)
        (loop for character across text for code = (char-code character)
              do (case state
                   (:escape
                    (setf state (case code
                                  (91
                                   ':csi)
                                  ((93 80 88 94 95)
                                   ':string)
                                  (t
                                   ':text))))
                   (:csi
                    (when (<= #x40 code #x7e) (setf state ':text)))
                   (:string
                    (cond ((member code '(7 #x9c))
                           (setf state ':text))
                          ((= code 27)
                           (setf state ':string-escape))))
                   (:string-escape
                    (setf state (if (= code 92) ':text ':string)))
                   (:text
                    (cond ((= code 27)
                           (setf state ':escape))
                          ((= code #x9b)
                           (setf state ':csi))
                          ((member code '(#x90 #x98 #x9d #x9e #x9f))
                           (setf state ':string))
                          ((= code 10)
                           (emit #\Newline) (label-line))
                          ((or (< code 32) (<= 127 code 159)
                               (member code '(#x2028 #x2029))
                               (<= #x202a code #x202e) (<= #x2066 code #x2069))
                           nil)
                          (t
                           (emit character))))))))
    (coerce buffer 'simple-string)))

(declaim (ftype (function (session-runtime parent-proto:envelope keyword) null)
                session-runtime--failure))

(defun session-runtime--failure (runtime request code)
  "Return one redacted terminal rejection for REQUEST."
  (session-runtime--send (session-runtime-service runtime) runtime
   (parent-proto:make-envelope :reply-to (parent-proto:envelope.message-id request) :failure
                               (parent-proto:make-failure :code code :message
                                                          "Parent rejected the operation.")))
  nil)

(declaim (ftype (function (session-runtime parent-proto:envelope &key (:table hash-table) (:key t)) boolean)
                session-runtime--history-full-p))

(defun session-runtime--history-full-p (runtime request &key table key)
  "Reject a new history key beyond 4096 entries, then close without losing exact matches."
  (if (and (not (gethash key table)) (>= (hash-table-count table) 4096))
      (progn
        (session-runtime--failure runtime request ':resource-limit)
        (session-runtime-disconnect runtime)
        t)
      nil))

(declaim (ftype (function (keyword) keyword) session-runtime--store-code))

(defun session-runtime--store-code (code)
  "Map fixed storage conditions to public redacted Failure codes."
  (case code
    (:resource-limit
     ':resource-limit)
    ((:storage-failure :corrupt-log :session-mismatch)
     ':unavailable)
    ((:event-conflict :call-conflict :turn-conflict)
     ':revision-conflict)
    (:forged-metadata
     ':permission-denied)
    (otherwise
     ':invalid-request)))

(declaim (ftype (function (session-runtime string) boolean) session-runtime--acknowledged-event-p))

(defun session-runtime--acknowledged-event-p (runtime event-id)
  "Return whether an append acknowledgement for EVENT-ID was sent on this connection."
  (not (null (gethash event-id (session-runtime-append-acks runtime)))))

(declaim (ftype (function (session-runtime string) boolean) session-runtime--stored-event-p))

(defun session-runtime--stored-event-p (runtime event-id)
  "Find a durable event with bounded pages so stop permits only idempotent retries."
  (loop with after = 0
        with high = nil
        for page = (session-store-read (session-runtime-store runtime)
                                      (parent-proto:make-session-read :after-sequence after :limit 256)
                                      :high-water-sequence high)
        for records = (parent-proto:session-page.records page)
        do (setf high (parent-proto:session-page.high-water-sequence page))
           (when (find event-id records :test #'string= :key #'parent-proto:session-record.event-id)
             (return t))
        while (parent-proto:session-page.has-more page)
        do (setf after (parent-proto:session-record.sequence (first (last records))))
        finally (return nil)))

(declaim (ftype (function (session-runtime parent-proto:envelope) null) session-runtime--dispatch))

(defun session-runtime--dispatch (runtime request)
  "Dispatch one validated envelope under the runtime lock."
  (let* ((body       (parent-proto:envelope.body-case request))
         (reply-p    (parent-proto:envelope.has-reply-to request))
         (reply      (parent-proto:envelope.reply-to request))
         (connection (session-runtime-connection runtime)))
    (labels ((reject (&optional (code ':invalid-request))
               (session-runtime--failure runtime request code))

             (send (envelope)
               (setf (parent-proto:envelope.reply-to envelope) (parent-proto:envelope.message-id request))
               (session-runtime--send (session-runtime-service runtime) runtime envelope)))
      (when (eq ':stopped (session-runtime-phase runtime))
        (reject)
        (return-from session-runtime--dispatch nil))
      (when
          (or (eq ':closed (session-runtime-phase runtime))
              (and (eq ':stopping (session-runtime-phase runtime))
                   (>= (funcall (session-runtime-clock runtime)) (session-runtime-deadline runtime))))
        (session-runtime--disconnect runtime)
        (return-from session-runtime--dispatch nil))
      (unless
          (if (member body '(parent-proto:turn-finish parent-proto:ready-to-stop parent-proto:failure))
              reply-p
              (not reply-p))
        (reject)
        (return-from session-runtime--dispatch nil))
      (when (session-runtime--model-dispatch (session-runtime-service runtime) runtime request)
        (return-from session-runtime--dispatch nil))
      (handler-case
       (case body
         (parent-proto:session-read
          (let* ((read    (parent-proto:envelope.session-read request))
                 (page
                  (session-store-read (session-runtime-store runtime) read :high-water-sequence
                                      (when
                                          (eql (parent-proto:session-read.after-sequence read)
                                               (session-runtime-page-after runtime))
                                        (session-runtime-page-high runtime))
                                      :max-bytes
                                      (max 1
                                           (-
                                            (parent-proto:limits.max-frame-bytes
                                             (parent-connection-limits connection))
                                            (+ 64
                                               (session-output--bytes
                                                (parent-connection-instance-id connection)))))))
                 (records (parent-proto:session-page.records page)))
            (send (parent-proto:make-envelope :session-page page))
            (session-runtime--after-send (session-runtime-service runtime) runtime
             (lambda ()
               (setf (session-runtime-page-after runtime)
                     (when (and records (parent-proto:session-page.has-more page))
                       (parent-proto:session-record.sequence (first (last records))))
                     (session-runtime-page-high runtime)
                     (when (session-runtime-page-after runtime)
                       (parent-proto:session-page.high-water-sequence page)))))
            (when records
              (session-runtime--after-send (session-runtime-service runtime) runtime
               (lambda ()
                 (setf (session-runtime-acknowledged runtime)
                       (max (session-runtime-acknowledged runtime)
                            (parent-proto:session-record.sequence (first (last records))))))))))
         (parent-proto:session-append
          (let* ((append (parent-proto:envelope.session-append request))
                 (record (parent-proto:session-append.record append)))
            (let ((now   (funcall (session-runtime-clock runtime)))
                  (bytes (length (cl-protobufs:serialize-to-bytes append))))
              (when (>= (- now (session-runtime-append-window runtime)) 1)
                (setf (session-runtime-append-window runtime) now
                      (session-runtime-append-count runtime) 0
                      (session-runtime-append-bytes runtime) 0))
              (when
                  (or (>= (session-runtime-append-count runtime) 64)
                      (> (+ bytes (session-runtime-append-bytes runtime)) 8388608))
                (reject ':resource-limit)
                (return-from session-runtime--dispatch nil))
              (incf (session-runtime-append-count runtime))
              (incf (session-runtime-append-bytes runtime) bytes))
            (unless
                (and record (parent-proto:session-record.payload record)
                     (< 0 (length (parent-proto:session-record.event-id record)) 1025))
              (reject)
              (return-from session-runtime--dispatch nil))
            (when
                (session-runtime--history-full-p runtime request :table (session-runtime-append-acks runtime)
                                                 :key                   (parent-proto:session-record.event-id record))
              (return-from session-runtime--dispatch nil))
            (when
                (and
                 (not (equal (parent-proto:session-record.turn-id record) (session-runtime-turn-id runtime)))
                 (not
                  (session-runtime--stored-event-p runtime (parent-proto:session-record.event-id record))))
              (reject)
              (return-from session-runtime--dispatch nil))
            (if (and (eq ':stopping (session-runtime-phase runtime))
                     (not (member (parent-proto:session-record.kind record) '(:note :tool-finished)))
                     (not
                      (session-runtime--stored-event-p runtime
                                                       (parent-proto:session-record.event-id record))))
                (reject ':cancelled)
                (let ((ack (session-store-append (session-runtime-store runtime) append)))
                  (send (parent-proto:make-envelope :session-ack ack))
                  (session-runtime--after-send (session-runtime-service runtime) runtime
                   (lambda ()
                     (setf (gethash (parent-proto:session-ack.event-id ack)
                                    (session-runtime-append-acks runtime))
                           (parent-proto:session-ack.sequence ack))))
                  (session-runtime--after-send (session-runtime-service runtime) runtime
                   (lambda ()
                     (setf (session-runtime-acknowledged runtime)
                           (max (session-runtime-acknowledged runtime)
                                (parent-proto:session-ack.sequence ack)))))))))
         (parent-proto:turn-finish
          (let* ((finish  (parent-proto:envelope.turn-finish request))
                 (outcome (parent-proto:turn-finish.outcome finish)))
            (if (and (session-runtime-turn-request runtime) (plusp (session-runtime-turn-request runtime))
                     (= reply (session-runtime-turn-request runtime))
                     (string= (parent-proto:turn-finish.turn-id finish) (session-runtime-turn-id runtime))
                     (member outcome '(:completed :cancelled :failed))
                     (= (parent-proto:turn-finish.last-session-sequence finish)
                        (session-runtime-acknowledged runtime)))
                (progn
                 (session-store-finish-turn (session-runtime-store runtime) (session-runtime-turn-id runtime)
                                            outcome)
                 (setf (session-runtime-turn-id runtime) nil
                       (session-runtime-turn-request runtime) nil))
                (reject))))
         (parent-proto:ready-to-stop
          (let ((ready (parent-proto:envelope.ready-to-stop request)))
            (if (and (session-runtime-stop-request runtime) (plusp (session-runtime-stop-request runtime))
                     (= reply (session-runtime-stop-request runtime))
                     (= (parent-proto:ready-to-stop.last-session-sequence ready)
                        (session-runtime-acknowledged runtime))
                     (or (not (parent-proto:ready-to-stop.has-wrap-up-event-id ready))
                         (session-runtime--acknowledged-event-p runtime
                                                                (parent-proto:ready-to-stop.wrap-up-event-id
                                                                 ready))))
                (progn
                 (session-store-interrupt (session-runtime-store runtime))
                 (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
                   (when (eq ':stopping (session-runtime-phase runtime))
                     (setf (session-runtime-phase runtime) ':stopped
                           (session-runtime-stop-request runtime) nil
                           (session-runtime-turn-request runtime) nil
                           (session-runtime-turn-id runtime) nil))))
                (reject))))
         (parent-proto:failure
          (let ((failure (parent-proto:envelope.failure request)))
            (if (and (session-runtime-turn-request runtime) (= reply (session-runtime-turn-request runtime))
                     (plusp (session-runtime-turn-request runtime))
                     (plusp (length (parent-proto:failure.message failure)))
                     (member (parent-proto:failure.code failure)
                             '(:invalid-request :permission-denied :revision-conflict :unsupported
                               :resource-limit :cancelled :unavailable :internal)))
                (progn
                 (session-store-finish-turn (session-runtime-store runtime) (session-runtime-turn-id runtime)
                                            ':failed :reason "Agent rejected the turn.")
                 (setf (session-runtime-turn-request runtime) nil
                       (session-runtime-turn-id runtime) nil))
                (reject))))
         (parent-proto:output
          (let* ((output  (parent-proto:envelope.output request))
                 (text    (parent-proto:output.text output))
                 (channel (parent-proto:output.channel output))
                 (now     (funcall (session-runtime-clock runtime))))
            (cond ((or (zerop (length text)) (not (member channel '(:progress :diagnostic))))
                   (reject))
                  ((or (> (length text) 8192) (> (session-output--bytes text) 32768))
                   (reject ':resource-limit))
                  (t
                   (when (>= (- now (session-runtime-output-window runtime)) 1)
                     (setf (session-runtime-output-window runtime) now
                           (session-runtime-output-bytes runtime) 0
                           (session-runtime-output-count runtime) 0))
                   (let* ((safe-text (session-output-sanitize text channel))
                          (bytes     (session-output--bytes safe-text)))
                     (if (or (>= (session-runtime-output-count runtime) 128)
                             (> (+ (session-runtime-output-bytes runtime) bytes) 65536))
                         (reject ':resource-limit)
                         (progn
                          (incf (session-runtime-output-count runtime))
                          (incf (session-runtime-output-bytes runtime) bytes)
                          (when (session-runtime-output-callback runtime)
                            (handler-case
                             (funcall (session-runtime-output-callback runtime) channel safe-text)
                             (error nil (reject ':internal)))))))))))
         ((parent-proto:model-start parent-proto:record-request parent-proto:access-request)
          (when
              (and (member body '(parent-proto:model-start parent-proto:access-request))
                   (session-runtime--history-full-p runtime request :table
                                                    (session-runtime-terminal-requests runtime) :key
                                                    (parent-proto:envelope.message-id request)))
            (return-from session-runtime--dispatch nil))
          (reject ':unsupported)
          (when (member body '(parent-proto:model-start parent-proto:access-request))
            (setf (gethash (parent-proto:envelope.message-id request)
                           (session-runtime-terminal-requests runtime))
                  t)))
         (parent-proto:cancel-request
          (let ((target
                 (parent-proto:cancel-request.request-id (parent-proto:envelope.cancel-request request))))
            (if (gethash target (session-runtime-terminal-requests runtime))
                (send
                 (parent-proto:make-envelope :cancel-ack (parent-proto:make-cancel-ack :already-terminal t)))
                (reject))))
         (otherwise
          (reject ':invalid-request)))
       (session-store-error (condition)
        (reject (session-runtime--store-code (session-store-error-code condition)))))))
  nil)

(declaim (ftype (function (session-runtime) null) session-runtime-receive))

(defun session-runtime-receive (runtime)
  "Receive and dispatch one actual framed envelope. Fatal I/O closes and interrupts.
Track the whole receive call, including a blocking connection error close. The
reader does not hold the lifecycle lock while waiting for input or closing."
  (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
    (when (eq ':closed (session-runtime-phase runtime))
      (error 'parent-protocol-error :reason ':closed))
    (when (plusp (session-runtime-active-receives runtime))
      (error 'parent-protocol-error :reason ':concurrent-reader))
    (incf (session-runtime-active-receives runtime)))
  (unwind-protect
       (handler-case
           (multiple-value-bind (request bytes)
               (parent-connection-receive (session-runtime-connection runtime))
              (session-runtime--incoming (session-runtime-service runtime) runtime request :bytes (length bytes)))
         (error (condition)
           (session-runtime-disconnect runtime)
           (error condition)))
    (bordeaux-threads:with-lock-held ((session-runtime-lifecycle-lock runtime))
      (decf (session-runtime-active-receives runtime))
      (bordeaux-threads:condition-notify (session-runtime-receive-finished runtime))))
  nil)
