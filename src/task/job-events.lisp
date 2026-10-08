(in-package #:autolith)

;;;; -- Headless Event Wire Contract --

(defparameter *run-job-event-emitter* nil
  "The optional emitter scoped to the current headless run.")

(defparameter *run-job-event-queue-capacity* 64
  "Maximum queued event payloads, excluding the single in-flight write.")

(defparameter *run-job-event-maximum-bytes* 16384
  "Maximum UTF-8 octets in one serialized event, including its newline.")

(defparameter *run-job-event-shutdown-seconds* 0.5
  "Maximum time spent waiting for event delivery at run completion.")

(defparameter *run-job-event-write-seconds* 0.5
  "Maximum observed in-flight write age before disabling further delivery.")

(defparameter *run-job-event-schemas*
  '((:run-started :output :output-path)
    (:job-started :job-id :execution-id :status)
    (:tool-started :job-id :call-id :execution-id :tool-name :status)
    (:progress :job-id :call-id :execution-id :count :status)
    (:tool-finished :job-id :call-id :execution-id :tool-name :status
                    :result-uri :resource-uris)
    (:job-finished :job-id :execution-id :status :result-uri :category :published-p)
    (:usage :job-id :input-tokens :output-tokens :total-tokens :provider-requests)
    (:warning :code)
    (:progress-omitted :count)
    (:run-finished :job-id :status :output :output-path :published-p :result-uri :category))
  "The version-one per-kind field allowlists. No freeform message field exists.")

(defparameter *run-job-event-statuses*
  '(:queued :running :succeeded :failed :timed-out :cancelled :aborted
    :success :failure :error :provider-request-started
    :provider-request-completed :provider-progress)
  "Approved lifecycle and progress status values.")

(defparameter *run-job-event-categories*
  '(:invalid-input :invalid-envelope :invalid-contract :input-not-found :unknown-role
    :runtime-unavailable :invalid-output :provider-failure :child-failure
    :artifact-failure :process-failure :timeout :cancelled
    :result-publication-failed :stream-failure)
  "Approved machine-readable failure categories.")

(defparameter *run-job-event-warning-codes*
  '(:invalid-event :stream-failed :stream-interrupted :slow-consumer
    :queue-full :writer-unavailable)
  "Recorded delivery warnings, containing no exception or caller text.")

(define-condition run-job-event-error (autolith-error) ()
  (:documentation "A headless event violates the bounded version-one wire contract."))

(-> run-job-event--error () null)
(defun run-job-event--error ()
  "Signal a content-free event validation error."
  (error 'run-job-event-error :message "Invalid headless event record."))

(-> run-job-event--keywords () list)
(defun run-job-event--keywords ()
  "Return the complete approved version-one keyword vocabulary."
  (remove-duplicates
   (append '(:autolith-event :version :run-id :seq :time :kind :data)
           (mapcan #'copy-list *run-job-event-schemas*)
           *run-job-event-statuses* *run-job-event-categories*
           *run-job-event-warning-codes*)))

(-> run-job-event--grammar () source-grammar)
(defun run-job-event--grammar ()
  "Return the restricted bounded sexp-config event dialect."
  (let ((keywords (run-job-event--keywords)))
    (make-source-grammar
     :label "Headless event" :keywords keywords
     :maximum-depth 8 :maximum-nodes 256 :maximum-string-characters 4096
     :allowed-atom-predicate
     (lambda (value)
       (or (null value) (eq value t) (stringp value)
           (and (keywordp value) (member value keywords))
           (typep value '(integer 0 9223372036854775807)))))))

(-> run-job-event--plist-p (list list) boolean)
(defun run-job-event--plist-p (data keys)
  "Return whether DATA is a duplicate-free plist using only KEYS."
  (and (evenp (length data))
       (loop with seen = nil
             for (key value) on data by #'cddr
             always (and (member key keys) (not (member key seen))
                         (progn (push key seen) t))
             finally (return t))))

(-> run-job-event--data-validate (keyword list) list)
(defun run-job-event--data-validate (kind data)
  "Validate the fixed field schema for KIND and return DATA."
  (validate-tree data (run-job-event--grammar))
  (let ((schema (assoc kind *run-job-event-schemas*)))
    (unless (and schema (listp data)
                 (run-job-event--plist-p data (rest schema)))
      (run-job-event--error)))
  (loop for (key value) on data by #'cddr
        unless
        (case key
          ((:job-id :call-id :execution-id :tool-name :result-uri :output :output-path)
           (and (stringp value) (plusp (length value))))
          (:resource-uris
           (and (listp value)
                (every (lambda (uri) (and (stringp uri) (plusp (length uri)))) value)))
          (:status (not (null (member value *run-job-event-statuses*))))
          (:category (or (null value) (not (null (member value *run-job-event-categories*)))))
          (:code (not (null (member value *run-job-event-warning-codes*))))
          (:published-p (or (null value) (eq value t)))
          (otherwise (typep value '(integer 0 9223372036854775807))))
          do (run-job-event--error))
  (when (eq kind :run-finished)
    (unless (and (member (getf data :status) '(:succeeded :failed :timed-out :cancelled))
                 (member :published-p data)
                 (or (not (getf data :published-p))
                     (getf data :output) (getf data :output-path) (getf data :result-uri))
                 (or (not (eq (getf data :status) :succeeded))
                     (getf data :published-p)))
      (run-job-event--error)))
  data)

(-> run-job-event-validate (t) list)
(defun run-job-event-validate (record)
  "Validate one portable version-one event and return it."
  (validate-tree record (run-job-event--grammar))
  (unless (and (consp record) (eq (first record) :autolith-event)
               (run-job-event--plist-p (rest record)
                                      '(:version :run-id :seq :time :kind :data))
               (= (length (rest record)) 12)
               (eql (getf (rest record) :version) 1)
               (stringp (getf (rest record) :run-id))
               (plusp (length (getf (rest record) :run-id)))
               (typep (getf (rest record) :seq) '(integer 1 9223372036854775807))
               (stringp (getf (rest record) :time)))
    (run-job-event--error))
  (run-job-event--data-validate (getf (rest record) :kind) (getf (rest record) :data))
  record)

(-> run-job-event--snapshot (t) t)
(defun run-job-event--snapshot (value)
  "Copy bounded proper data before validation, rejecting cycles and foreign atoms."
  (let ((nodes 0)
        (ancestors (make-hash-table :test #'eq))
        (keywords (run-job-event--keywords)))
    (labels ((copy-value (value depth)
               (when (or (> (incf nodes) 256) (> depth 8))
                 (run-job-event--error))
               (cond
                 ((consp value)
                  (when (gethash value ancestors)
                    (run-job-event--error))
                  (setf (gethash value ancestors) t)
                  (unwind-protect
                       (let ((head (first value))
                             (tail (rest value)))
                         (unless (listp tail) (run-job-event--error))
                         (cons (copy-value head (1+ depth))
                               (copy-value tail depth)))
                    (remhash value ancestors)))
                 ((stringp value)
                  (let ((length (length value)))
                    (when (> length 4096) (run-job-event--error))
                    (subseq value 0 length)))
                 ((or (null value) (eq value t)
                      (and (keywordp value) (member value keywords))
                      (typep value '(integer 0 9223372036854775807)))
                  value)
                 (t
                  (run-job-event--error)))))
      (copy-value value 0))))

(-> run-job-event--serialize (list) string)
(defun run-job-event--serialize (record)
  "Validate RECORD before producing predictable readable Unicode text."
  (run-job-event-validate record)
  (let* ((*print-readably* nil) (*print-escape* t) (*print-pretty* nil)
         (*print-circle* nil) (*print-level* nil) (*print-length* nil)
         (*print-case* ':upcase) (*print-base* 10) (*print-radix* nil)
         (*print-array* t) (*package* (find-package :cl))
         (text (concatenate 'string (write-to-string record) (string #\Newline))))
    (when (> (length (utf8-string-to-octets text)) *run-job-event-maximum-bytes*)
      (run-job-event--error))
    text))

(-> run-job-event-read-string (string) list)
(defun run-job-event-read-string (source)
  "Read exactly one byte-bounded event through sexp-config with read-eval disabled."
  (when (or (> (length source) *run-job-event-maximum-bytes*)
            (> (length (utf8-string-to-octets source)) *run-job-event-maximum-bytes*))
    (run-job-event--error))
  (let ((*read-eval* nil))
    (run-job-event-validate (read-source source (run-job-event--grammar)))))

(-> run-job-event-read-stream (stream) t)
(defun run-job-event-read-stream (stream)
  "Read the next bounded complete event, or NIL at clean EOF between forms.
This framing scanner never invokes the Lisp reader. sexp-config validates the
complete form; incomplete forms, dispatch syntax and comments are rejected."
  (let ((first (loop for character = (read-char stream nil nil)
                     while (and character (find character '(#\Space #\Tab #\Return #\Newline)))
                     finally (return character))))
    (unless first (return-from run-job-event-read-stream nil))
    (unless (char= first #\() (run-job-event--error))
    (let ((buffer (make-array 128 :element-type 'character :adjustable t :fill-pointer 0))
          (depth 1) (in-string nil) (escaped nil))
      (vector-push-extend first buffer)
      (loop for character = (read-char stream nil nil)
            do (unless character (run-job-event--error))
               (when (>= (length buffer) *run-job-event-maximum-bytes*)
                 (run-job-event--error))
               (vector-push-extend character buffer)
               (cond
                 (escaped (setf escaped nil))
                 (in-string
                  (case character
                    (#\\ (setf escaped t))
                    (#\" (setf in-string nil))))
                 ((char= character #\") (setf in-string t))
                 ((find character '(#\# #\; #\' #\` #\,)) (run-job-event--error))
                 ((char= character #\() (incf depth)
                  (when (> depth 8) (run-job-event--error)))
                 ((char= character #\)) (decf depth)))
            until (zerop depth))
      (run-job-event-read-string buffer))))

;;;; -- Bounded Single Writer --

(defclass run-job-event-emitter ()
  ((run-id :initarg :run-id :reader run-job-event-emitter-run-id
           :documentation "Opaque generated run identifier.")
   (stream :initarg :stream :reader run-job-event-emitter-stream
           :documentation "Caller-owned UTF-8 character or binary output stream.")
   (capacity :initarg :capacity :reader run-job-event-emitter-capacity
             :documentation "Immutable maximum number of queued payloads.")
   (lock :initform (make-lock "headless events") :reader run-job-event-emitter-lock
         :documentation "Protects all mutable queue and delivery state.")
   (condition :initform (make-condition-variable) :reader run-job-event-emitter-condition
              :documentation "Wakes the single delivery writer.")
   (queue :initform nil :accessor run-job-event-emitter-queue
          :documentation "Bounded FIFO of validated copied kind/data pairs.")
   (terminal :initform nil :accessor run-job-event-emitter-terminal
             :documentation "Reserved final event, outside the normal queue.")
   (omitted :initform 0 :accessor run-job-event-emitter-omitted
            :documentation "Saturating count of dropped progress records.")
   (warnings :initform nil :accessor run-job-event-emitter-warning-codes
             :documentation "Deduplicated fixed delivery warning codes.")
   (overflow-p :initform nil :accessor run-job-event-emitter-overflow-p
               :documentation "Whether to report non-progress loss when delivery resumes.")
   (disabled-p :initform nil :accessor run-job-event-emitter-disabled-p
               :documentation "Whether delivery has been disabled.")
   (finishing-p :initform nil :accessor run-job-event-emitter-finishing-p
                :documentation "Whether producers are closed.")
   (done-p :initform nil :accessor run-job-event-emitter-done-p
           :documentation "Whether the delivery thread has exited.")
   (writing-since :initform nil :accessor run-job-event-emitter-writing-since
                  :documentation "Monotonic in-flight write start, or NIL.")
   (thread :initform nil :accessor run-job-event-emitter-thread
           :documentation "The sole writer thread."))
  (:documentation "Best-effort bounded observation transport for one headless run."))

(defvar *run-job-event-writer-lock* (make-lock "headless writer lifecycle")
  "Protects the one-shot CLI writer retention guard.")

(defvar *run-job-event-retained-writer* nil
  "At most one unfinished writer. Another emitter cannot start until it exits.
A blocked stdout write is not interrupted or closed by another thread. At most
one in-flight bounded form and the disabled emitter survive until process exit.")

(-> run-job-event-emitter-warnings (run-job-event-emitter) list)
(defun run-job-event-emitter-warnings (emitter)
  "Return a snapshot of content-free delivery warning codes."
  (with-lock-held ((run-job-event-emitter-lock emitter))
    (copy-list (run-job-event-emitter-warning-codes emitter))))

(-> run-job-event--disable (run-job-event-emitter keyword) null)
(defun run-job-event--disable (emitter code)
  "Disable EMITTER under its lock and clear all pending payloads."
  (pushnew code (run-job-event-emitter-warning-codes emitter))
  (setf (run-job-event-emitter-disabled-p emitter) t
        (run-job-event-emitter-queue emitter) nil
        (run-job-event-emitter-terminal emitter) nil
        (run-job-event-emitter-omitted emitter) 0
        (run-job-event-emitter-overflow-p emitter) nil)
  (condition-notify (run-job-event-emitter-condition emitter))
  nil)

(-> run-job-event--clock () real)
(defun run-job-event--clock ()
  "Return monotonic seconds for bounded delivery waits."
  (/ (get-internal-real-time) internal-time-units-per-second))

(-> run-job-event--time () string)
(defun run-job-event--time ()
  "Return a UTC wire timestamp without implementation-specific objects."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(-> run-job-event--record (string integer &key (:kind keyword) (:data list)) list)
(defun run-job-event--record (run-id sequence &key kind data)
  "Construct the fixed envelope for a previously validated payload."
  (list :autolith-event :version 1 :run-id run-id :seq sequence
        :time (run-job-event--time) :kind kind :data data))

(-> run-job-event--next (run-job-event-emitter) list)
(defun run-job-event--next (emitter)
  "Wait for the next payload under the queue lock, or return NIL at shutdown."
  (with-lock-held ((run-job-event-emitter-lock emitter))
    (loop
      (when (run-job-event-emitter-disabled-p emitter) (return nil))
      (when (run-job-event-emitter-queue emitter)
        (return (pop (run-job-event-emitter-queue emitter))))
      (when (run-job-event-emitter-overflow-p emitter)
        (setf (run-job-event-emitter-overflow-p emitter) nil)
        (return (list :warning (list :code ':queue-full))))
      (when (plusp (run-job-event-emitter-omitted emitter))
        (return (list :progress-omitted
                      (list :count (prog1 (run-job-event-emitter-omitted emitter)
                                     (setf (run-job-event-emitter-omitted emitter) 0))))))
      (when (run-job-event-emitter-terminal emitter)
        (return (prog1 (run-job-event-emitter-terminal emitter)
                  (setf (run-job-event-emitter-terminal emitter) nil))))
      (when (run-job-event-emitter-finishing-p emitter) (return nil))
      (condition-wait (run-job-event-emitter-condition emitter)
                      (run-job-event-emitter-lock emitter)))))

(-> run-job-event--writer (run-job-event-emitter) null)
(defun run-job-event--writer (emitter)
  "Serialize complete forms with writer-assigned sequences, isolating failures."
  (unwind-protect
       (handler-case
           (loop for payload = (run-job-event--next emitter)
                 for sequence from 1
                 while payload
                 do (let* ((text (run-job-event--serialize
                                  (run-job-event--record (run-job-event-emitter-run-id emitter)
                                                        sequence :kind (first payload) :data (second payload))))
                           (stream (run-job-event-emitter-stream emitter)))
                      (with-lock-held ((run-job-event-emitter-lock emitter))
                        (when (run-job-event-emitter-disabled-p emitter) (return))
                        (setf (run-job-event-emitter-writing-since emitter) (run-job-event--clock)))
                      (if (subtypep (stream-element-type stream) 'character)
                          (write-string text stream)
                          (write-sequence (utf8-string-to-octets text) stream))
                      (finish-output stream)
                      (with-lock-held ((run-job-event-emitter-lock emitter))
                        (setf (run-job-event-emitter-writing-since emitter) nil))))
         (error ()
           (with-lock-held ((run-job-event-emitter-lock emitter))
             (run-job-event--disable emitter ':stream-failed)))
         (serious-condition ()
           (with-lock-held ((run-job-event-emitter-lock emitter))
             (run-job-event--disable emitter ':stream-interrupted))))
    (with-lock-held ((run-job-event-emitter-lock emitter))
      (unless (or (run-job-event-emitter-disabled-p emitter)
                  (run-job-event-emitter-finishing-p emitter))
        (run-job-event--disable emitter ':stream-interrupted))
      (setf (run-job-event-emitter-done-p emitter) t
            (run-job-event-emitter-writing-since emitter) nil)))
  nil)

(-> run-job-event-emitter-create (string stream) run-job-event-emitter)
(defun run-job-event-emitter-create (run-id stream)
  "Create one best-effort writer for RUN-ID and caller-owned UTF-8 STREAM.
Character streams must already use UTF-8. Binary streams receive UTF-8 octets.
The stream is never closed or asynchronously interrupted by the emitter."
  (let ((run-id (run-job-event--snapshot run-id)))
    (run-job-event--serialize (run-job-event--record run-id 1 :kind ':run-started :data nil))
    (unless (and (integerp *run-job-event-queue-capacity*)
                 (<= 1 *run-job-event-queue-capacity* 1024))
      (run-job-event--error))
    (let ((emitter (make-instance 'run-job-event-emitter
                                 :run-id run-id :stream stream
                                 :capacity *run-job-event-queue-capacity*)))
      (with-lock-held (*run-job-event-writer-lock*)
        (if (and *run-job-event-retained-writer*
                 (not (run-job-event-emitter-done-p *run-job-event-retained-writer*)))
            (with-lock-held ((run-job-event-emitter-lock emitter))
              (setf (run-job-event-emitter-done-p emitter) t)
              (run-job-event--disable emitter ':writer-unavailable))
            (handler-case
                (progn
                  (setf *run-job-event-retained-writer* emitter)
                  (setf (run-job-event-emitter-thread emitter)
                        (make-thread (lambda () (run-job-event--writer emitter))
                                     :name "headless event writer")))
              (error ()
                (setf (run-job-event-emitter-done-p emitter) t)
                (with-lock-held ((run-job-event-emitter-lock emitter))
                  (run-job-event--disable emitter ':writer-unavailable))))))
      emitter)))

(-> run-job-event--omit-progress (run-job-event-emitter) null)
(defun run-job-event--omit-progress (emitter)
  "Count one omitted progress event while holding EMITTER's queue lock."
  (setf (run-job-event-emitter-omitted emitter)
        (min 9223372036854775807 (1+ (run-job-event-emitter-omitted emitter))))
  nil)

(-> run-job-event-emit (run-job-event-emitter keyword list) boolean)
(defun run-job-event-emit (emitter kind data)
  "Validate and copy an allowlisted payload without blocking on its consumer.
Only FINISH can emit RUN-FINISHED. Projection callers supply safe internal IDs,
canonical tool names and resource references, never caller/provider raw data."
  (handler-case
      (let* ((owned-data (run-job-event--snapshot data))
             (payload (list kind owned-data)))
        (when (eq kind :run-finished) (run-job-event--error))
        (run-job-event--serialize
         (run-job-event--record (run-job-event-emitter-run-id emitter)
                               9223372036854775807 :kind kind :data owned-data))
        (with-lock-held ((run-job-event-emitter-lock emitter))
          (let ((since (run-job-event-emitter-writing-since emitter)))
            (when (and since (> (- (run-job-event--clock) since) *run-job-event-write-seconds*))
              (run-job-event--disable emitter ':slow-consumer)))
          (when (or (run-job-event-emitter-disabled-p emitter)
                    (run-job-event-emitter-finishing-p emitter))
            (return-from run-job-event-emit nil))
          (when (>= (length (run-job-event-emitter-queue emitter))
                    (run-job-event-emitter-capacity emitter))
            (when (eq kind :progress)
              (run-job-event--omit-progress emitter)
              (return-from run-job-event-emit nil))
            (let ((progress (find :progress (run-job-event-emitter-queue emitter) :key #'first)))
              (if progress
                  (progn
                    (setf (run-job-event-emitter-queue emitter)
                          (delete progress (run-job-event-emitter-queue emitter) :test #'eq :count 1))
                    (run-job-event--omit-progress emitter))
                  (progn
                    (pushnew ':queue-full (run-job-event-emitter-warning-codes emitter))
                    (setf (run-job-event-emitter-overflow-p emitter) t)
                    (return-from run-job-event-emit nil)))))
          (setf (run-job-event-emitter-queue emitter)
                (nconc (run-job-event-emitter-queue emitter) (list payload)))
          (condition-notify (run-job-event-emitter-condition emitter))
          t))
    (error ()
      (with-lock-held ((run-job-event-emitter-lock emitter))
        (pushnew ':invalid-event (run-job-event-emitter-warning-codes emitter)))
      nil)))

(-> run-job-event-emitter-finish (run-job-event-emitter list) boolean)
(defun run-job-event-emitter-finish (emitter terminal-data)
  "Close producers and attempt final delivery within the shutdown bound.
Call only after attempting authoritative artifact publication. TERMINAL-DATA
reports its actual status, output reference and PUBLISHED-P. No success is inferred.
If stdout blocks, clear queued payloads and retain only the one bounded writer
until it returns or this one-shot CLI exits; never interrupt its native write."
  (handler-case
      (let ((owned-data (run-job-event--snapshot terminal-data)))
        (run-job-event--serialize
         (run-job-event--record (run-job-event-emitter-run-id emitter)
                               9223372036854775807 :kind ':run-finished :data owned-data))
        (with-lock-held ((run-job-event-emitter-lock emitter))
          (unless (run-job-event-emitter-finishing-p emitter)
            (unless (run-job-event-emitter-disabled-p emitter)
              (setf (run-job-event-emitter-terminal emitter)
                    (list :run-finished owned-data)))
            (setf (run-job-event-emitter-finishing-p emitter) t)
            (condition-notify (run-job-event-emitter-condition emitter)))))
    (error ()
      (with-lock-held ((run-job-event-emitter-lock emitter))
        (setf (run-job-event-emitter-finishing-p emitter) t)
        (run-job-event--disable emitter ':invalid-event))))
  (let ((deadline (+ (run-job-event--clock) *run-job-event-shutdown-seconds*)))
    (loop
      (with-lock-held ((run-job-event-emitter-lock emitter))
        (when (run-job-event-emitter-done-p emitter)
          (return-from run-job-event-emitter-finish
            (not (run-job-event-emitter-disabled-p emitter))))
        (when (>= (run-job-event--clock) deadline)
          (run-job-event--disable emitter ':slow-consumer)
          (return-from run-job-event-emitter-finish nil)))
      (sleep 0.005))))
