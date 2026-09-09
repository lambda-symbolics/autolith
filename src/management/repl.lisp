(in-package #:autolith)

;;;; -- Management REPL Conditions --

(define-condition management-repl-error (autolith-error)
  ((operation
    :initarg :operation
    :reader management-repl-error-operation
    :type keyword
    :documentation "The management endpoint operation that failed."))
  (:documentation "The base condition for active-image management endpoint failures."))

(define-condition management-repl-configuration-error
    (management-repl-error configuration-error)
  ((reason
    :initarg :reason
    :initform ':configuration
    :reader management-repl-configuration-error-reason
    :type keyword
    :documentation "The non-secret configuration failure category."))
  (:documentation "A management endpoint configuration or credential failure."))

(define-condition management-repl-protocol-error (management-repl-error)
  ((reason
    :initarg :reason
    :reader management-repl-protocol-error-reason
    :type keyword
    :documentation "The bounded protocol rejection reason."))
  (:documentation "A malformed, incomplete, or oversized management protocol message."))

(define-condition management-repl-authentication-error (management-repl-error)
  ((reason
    :initarg :reason
    :reader management-repl-authentication-error-reason
    :type keyword
    :documentation "The non-secret authentication rejection reason."))
  (:documentation "A management connection failed challenge-response authentication."))

(define-condition management-repl-capacity-error (management-repl-error)
  ()
  (:documentation "The bounded management evaluation queue is full or stopping."))

(define-condition management-repl-quiescence-error (management-repl-error)
  ((threads
    :initarg :threads
    :reader management-repl-quiescence-error-threads
    :type list
    :documentation "The bounded names of management threads that did not stop."))
  (:documentation "Management threads could not quiesce within the lifecycle bound."))


;;;; -- Bounded Wire Protocol --

(defparameter *management-repl-protocol-version* 1
  "The active-image management wire protocol version.")

(defparameter *management-repl-nonce-octets* 32
  "The number of cryptographically random octets in one authentication nonce.")

(defparameter *management-repl-proof-octets* 32
  "The SHA-256 HMAC proof size in octets.")

(defparameter *management-repl-maximum-protocol-list-length* 16
  "The maximum number of cons cells accepted in a protocol form.")

(defparameter *management-repl-stop-timeout* 2
  "The maximum seconds normal shutdown waits for management threads.")

(defparameter *management-repl-start-thread-function* #'make-thread
  "The thread constructor used while starting the management endpoint.")

(defparameter *management-repl-minimum-frame-size* 128
  "The smallest frame bound that can carry a structured protocol failure.")

(-> management-repl--proper-list-length (t) (integer 0))
(defun management-repl--proper-list-length (value)
  "Return VALUE's bounded proper-list length or reject cycles and dotted tails."
  (let ((seen (make-hash-table :test #'eq))
        (length 0)
        (tail value))
    (loop while (consp tail)
          do (when (or (gethash tail seen)
                       (>= length *management-repl-maximum-protocol-list-length*))
               (management-repl--protocol-error
                ':malformed "Management protocol form is cyclic or too long."))
             (setf (gethash tail seen) t
                   tail (rest tail))
             (incf length))
    (unless (null tail)
      (management-repl--protocol-error
       ':malformed "Management protocol form is not a proper list."))
    length))

(-> management-repl--decode-schema (t keyword list) list)
(defun management-repl--decode-schema (form tag required-keys)
  "Decode FORM as exact TAG with one occurrence of every REQUIRED-KEY."
  (let ((length (management-repl--proper-list-length form)))
    (unless (and (plusp length) (eq (first form) tag) (oddp length))
      (management-repl--protocol-error
       ':request "Management protocol request has the wrong shape."))
    (let ((properties (rest form))
          (seen nil))
      (loop for tail on properties by #'cddr
            for key = (first tail)
            do (unless (member key required-keys)
                 (management-repl--protocol-error
                  ':request "Management protocol request contains an unknown key."))
               (when (member key seen)
                 (management-repl--protocol-error
                  ':request "Management protocol request contains a duplicate key."))
               (push key seen))
      (unless (= (length seen) (length required-keys))
        (management-repl--protocol-error
         ':request "Management protocol request is missing a required key."))
      properties)))

(-> management-repl--protocol-error (keyword string) nil)
(defun management-repl--protocol-error (reason message)
  "Signal a bounded management protocol failure for REASON and MESSAGE."
  (error 'management-repl-protocol-error
         :message message
         :operation ':protocol
         :reason reason))

(-> management-repl--integer->header ((integer 0 #xffffffff))
    (simple-array (unsigned-byte 8) (4)))
(defun management-repl--integer->header (value)
  "Encode unsigned VALUE as a four-octet network-order frame header."
  (let ((header (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref header 0) (ldb (byte 8 24) value)
          (aref header 1) (ldb (byte 8 16) value)
          (aref header 2) (ldb (byte 8 8) value)
          (aref header 3) (ldb (byte 8 0) value))
    header))

(-> management-repl--header->integer
    ((simple-array (unsigned-byte 8) (4)))
    (integer 0 #xffffffff))
(defun management-repl--header->integer (header)
  "Decode a four-octet network-order frame HEADER."
  (logior (ash (aref header 0) 24)
          (ash (aref header 1) 16)
          (ash (aref header 2) 8)
          (aref header 3)))

(-> management-repl--read-exactly (stream (integer 0) &key (:eof-ok-p boolean))
    (option (simple-array (unsigned-byte 8) (*))))
(defun management-repl--read-exactly (stream count &key eof-ok-p)
  "Read exactly COUNT octets from STREAM, permitting clean initial EOF when requested."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (loop with position = 0
          while (< position count)
          for next = (read-sequence octets stream :start position)
          do (when (= next position)
               (if (and eof-ok-p (zerop position))
                   (return-from management-repl--read-exactly nil)
                   (management-repl--protocol-error
                    ':truncated "Truncated management protocol frame.")))
             (setf position next))
    octets))

(-> management-repl-read-frame (stream (integer 1)) t)
(defun management-repl-read-frame (stream maximum-size)
  "Read and safely parse one bounded, length-prefixed S-expression frame."
  (let ((header (management-repl--read-exactly stream 4 :eof-ok-p t)))
    (unless header
      (return-from management-repl-read-frame ':end-of-input))
    (let ((length (management-repl--header->integer header)))
      (when (or (zerop length) (> length maximum-size))
        (management-repl--protocol-error
         ':oversized "Management protocol frame size is outside the configured bound."))
      (let* ((octets (management-repl--read-exactly stream length))
             (source
               (handler-case
                   (sb-ext:octets-to-string octets :external-format ':utf-8)
                 (error ()
                   (management-repl--protocol-error
                    ':encoding "Management protocol frame is not valid UTF-8.")))))
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*readtable* (copy-readtable nil))
                (*package* (find-package '#:autolith))
                (position 0))
            (handler-case
                (multiple-value-bind (form next)
                    (read-from-string source nil ':end-of-input :start position)
                  (when (eq form ':end-of-input)
                    (management-repl--protocol-error
                     ':empty "Management protocol frame contains no form."))
                  (setf position next)
                  (multiple-value-bind (trailing trailing-position)
                      (read-from-string source nil ':end-of-input :start position)
                    (declare (ignore trailing-position))
                    (unless (eq trailing ':end-of-input)
                      (management-repl--protocol-error
                       ':trailing "Management protocol frame contains trailing data.")))
                  form)
              (management-repl-protocol-error (condition)
                (error condition))
              (error ()
                (management-repl--protocol-error
                 ':malformed "Management protocol frame is malformed.")))))))))

(-> management-repl-write-frame (stream t (integer 1)) null)
(defun management-repl-write-frame (stream value maximum-size)
  "Print VALUE readably and write one bounded network-order frame to STREAM."
  (let ((capture (make-instance 'management-repl-bounded-output-stream
                                :limit maximum-size)))
    (with-standard-io-syntax
      (let ((*print-readably* t)
            (*print-circle* t)
            (*print-level* 12)
            (*print-length* 256)
            (*package* (find-package '#:autolith)))
        (write value :stream capture)))
    (when (management-repl-output-truncated-p capture)
      (management-repl--protocol-error
       ':oversized "Management protocol response exceeds the configured frame bound."))
    (let* ((text (management-repl-output-string capture))
           (octets (sb-ext:string-to-octets text :external-format ':utf-8)))
      (when (> (length octets) maximum-size)
        (management-repl--protocol-error
         ':oversized "Management protocol response exceeds the configured frame bound."))
      (write-sequence (management-repl--integer->header (length octets)) stream)
      (write-sequence octets stream)
      (force-output stream)))
  nil)

(-> management-repl--write-response (stream t (integer 1)) null)
(defun management-repl--write-response (stream value maximum-size)
  "Write VALUE, degrading oversized encodings to a bounded protocol response."
  (handler-case
      (management-repl-write-frame stream value maximum-size)
    (management-repl-protocol-error (condition)
      (unless (eq (management-repl-protocol-error-reason condition) ':oversized)
        (error condition))
      (management-repl-write-frame
       stream '(:protocol-error :reason :oversized) maximum-size)))
  nil)


;;;; -- Authentication --

(-> management-repl--octets->hex
    ((simple-array (unsigned-byte 8) (*)))
    string)
(defun management-repl--octets->hex (octets)
  "Return a lowercase hexadecimal encoding of OCTETS."
  (with-output-to-string (stream)
    (loop for octet across octets
          do (format stream "~2,'0x" octet))))

(-> management-repl--hex->octets (string (integer 1))
    (option (simple-array (unsigned-byte 8) (*))))
(defun management-repl--hex->octets (text expected-length)
  "Decode hexadecimal TEXT only when it has EXPECTED-LENGTH octets."
  (unless (= (length text) (* expected-length 2))
    (return-from management-repl--hex->octets nil))
  (let ((octets (make-array expected-length :element-type '(unsigned-byte 8))))
    (handler-case
        (loop for index below expected-length
              for start = (* index 2)
              do (setf (aref octets index)
                       (parse-integer text :start start :end (+ start 2)
                                           :radix 16 :junk-allowed nil))
              finally (return octets))
      (error () nil))))

(-> management-repl--constant-time-equal-p
    ((simple-array (unsigned-byte 8) (*))
     (simple-array (unsigned-byte 8) (*)))
    boolean)
(defun management-repl--constant-time-equal-p (left right)
  "Compare LEFT and RIGHT without data-dependent early return."
  (let ((difference (logxor (length left) (length right))))
    (loop for index below (max (length left) (length right))
          for left-octet = (if (< index (length left)) (aref left index) 0)
          for right-octet = (if (< index (length right)) (aref right index) 0)
          do (setf difference
                   (logior difference (logxor left-octet right-octet))))
    (zerop difference)))

(-> management-repl--token-file-octets (pathname)
    (simple-array (unsigned-byte 8) (*)))
(defun management-repl--token-file-octets (pathname)
  "Read a nonempty token from a private regular file without following links."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (multiple-value-bind (opened status)
                 (platform-open-regular-file *platform* pathname)
               (setf stream opened)
               (let ((length (platform-file-status-size status)))
                 (unless (and (platform-file-status-private-p status)
                              (<= 1 length 4096))
                   (error 'management-repl-configuration-error
                          :message "The management token file must be a nonempty regular file private to the current user and no larger than 4096 octets."
                          :operation ':credentials
                          :reason ':unsafe-token-file))
                 (let ((token (make-array length
                                          :element-type '(unsigned-byte 8))))
                   (unless (= (read-sequence token stream) length)
                     (fill token 0)
                     (error 'management-repl-configuration-error
                            :message "The management token file could not be read completely."
                            :operation ':credentials
                            :reason ':token-read))
                   token)))
           (management-repl-error (condition)
             (error condition))
           (error ()
             (error 'management-repl-configuration-error
                    :message "The management token file is unavailable or unsafe."
                    :operation ':credentials
                    :reason ':token-open)))
      (when stream
        (ignore-errors (close stream))))))

(-> management-repl--hmac
    ((simple-array (unsigned-byte 8) (*))
     (simple-array (unsigned-byte 8) (*)))
    (simple-array (unsigned-byte 8) (*)))
(defun management-repl--hmac (token nonce)
  "Return HMAC-SHA-256 for TOKEN and NONCE."
  (let ((hmac (ironclad:make-hmac token ':sha256)))
    (ironclad:update-hmac hmac nonce)
    (ironclad:hmac-digest hmac)))

(-> management-repl--authenticate
    (stream pathname (integer 1))
    null)
(defun management-repl--authenticate (stream token-pathname maximum-frame-size)
  "Authenticate STREAM using a fresh nonce and token read only for this exchange."
  (let ((nonce    (ironclad:random-data *management-repl-nonce-octets*))
        (token    nil)
        (expected nil)
        (supplied nil))
    (unwind-protect
         (progn
           (management-repl-write-frame
            stream
            (list ':challenge :version *management-repl-protocol-version*
                  :algorithm ':hmac-sha-256
                  :nonce (management-repl--octets->hex nonce))
            maximum-frame-size)
           (let* ((request (management-repl-read-frame stream maximum-frame-size))
                  (properties
                    (handler-case
                        (management-repl--decode-schema request ':authenticate '(:proof))
                      (management-repl-protocol-error ()
                        (error 'management-repl-authentication-error
                               :message "Management authentication request is malformed."
                               :operation ':authenticate
                               :reason ':malformed))))
                  (proof (getf properties :proof)))
             (unless (stringp proof)
               (error 'management-repl-authentication-error
                      :message "Management authentication request is malformed."
                      :operation ':authenticate
                      :reason ':malformed))
             (setf supplied (management-repl--hex->octets
                             proof *management-repl-proof-octets*)
                   token (management-repl--token-file-octets token-pathname)
                   expected (management-repl--hmac token nonce))
             (unless (and supplied
                          (management-repl--constant-time-equal-p supplied expected))
               (error 'management-repl-authentication-error
                      :message "Management authentication failed."
                      :operation ':authenticate
                      :reason ':proof))))
      (fill nonce 0)
      (when token
        (fill token 0))
      (when expected
        (fill expected 0))
      (when supplied
        (fill supplied 0))))
  (management-repl--write-response
   stream (list ':authenticated :version *management-repl-protocol-version*)
   maximum-frame-size)
  nil)


;;;; -- Bounded Evaluation Output --

(defclass management-repl-bounded-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((buffer
    :initform (make-array 0
                          :element-type 'character
                          :adjustable t
                          :fill-pointer 0)
    :reader management-repl-output-buffer
    :documentation "The captured characters up to the configured limit.")
   (limit
    :initarg :limit
    :reader management-repl-output-limit
    :type (integer 0)
    :documentation "The maximum captured character count.")
   (truncated-p
    :initform nil
    :accessor management-repl-output-truncated-p
    :type boolean
    :documentation "Whether further output was discarded."))
  (:documentation "A character output stream that never grows beyond a fixed limit."))

(defmethod sb-gray:stream-write-char
    ((stream management-repl-bounded-output-stream) character)
  "Capture CHARACTER when STREAM still has capacity."
  (if (< (fill-pointer (management-repl-output-buffer stream))
         (management-repl-output-limit stream))
      (vector-push-extend character (management-repl-output-buffer stream))
      (setf (management-repl-output-truncated-p stream) t))
  character)

(defmethod sb-gray:stream-write-string
    ((stream management-repl-bounded-output-stream) string
     &optional (start 0) end)
  "Capture the bounded slice of STRING accepted by STREAM."
  (let* ((end (or end (length string)))
         (available (- (management-repl-output-limit stream)
                       (fill-pointer (management-repl-output-buffer stream))))
         (count (min available (- end start))))
    (loop for index from start below (+ start count)
          do (vector-push-extend (char string index)
                                 (management-repl-output-buffer stream)))
    (when (< count (- end start))
      (setf (management-repl-output-truncated-p stream) t)))
  string)

(-> management-repl-output-string
    (management-repl-bounded-output-stream)
    string)
(defun management-repl-output-string (stream)
  "Return a fresh string containing STREAM's bounded captured output."
  (coerce (management-repl-output-buffer stream) 'string))

(-> management-repl--print-bounded (t (integer 0)) (values string boolean))
(defun management-repl--print-bounded (value limit)
  "Return a readable bounded rendering of VALUE and whether it was truncated."
  (let ((stream (make-instance 'management-repl-bounded-output-stream
                               :limit limit)))
    (let ((*print-readably* nil)
          (*print-escape* t)
          (*print-circle* t)
          (*print-level* 8)
          (*print-length* 64))
      (write value :stream stream))
    (values (management-repl-output-string stream)
            (management-repl-output-truncated-p stream))))


;;;; -- Runtime State and Queue --

(defclass management-repl-request ()
  ((source
    :initarg :source
    :reader management-repl-request-source
    :type string
    :documentation "The already bounded source containing exactly one form.")
   (deadline
    :initarg :deadline
    :reader management-repl-request-deadline
    :type real
    :documentation "The internal real-time deadline for evaluation completion.")
   (lock
    :initform (make-lock "Autolith management request")
    :reader management-repl-request-lock
    :documentation "The lock guarding completion and response.")
   (condition-variable
    :initform (sb-thread:make-waitqueue :name "Autolith management request")
    :reader management-repl-request-condition-variable
    :documentation "The completion notification waitqueue.")
   (completed-p
    :initform nil
    :accessor management-repl-request-completed-p
    :type boolean
    :documentation "Whether the evaluator installed a response.")
   (response
    :initform nil
    :accessor management-repl-request-response
    :type t
    :documentation "The bounded protocol response."))
  (:documentation "One authenticated management evaluation and its completion state."))

(defclass management-repl-runtime ()
  ((configuration
    :initarg :configuration
    :reader management-repl-runtime-configuration
    :type configuration
    :documentation "The endpoint configuration, which contains no raw token bytes.")
   (application
    :initarg :application
    :accessor management-repl-runtime-application
    :type application
    :documentation "The application that currently owns this runtime.")
   (listener
    :initarg :listener
    :accessor management-repl-runtime-listener
    :type t
    :documentation "The listening socket, or NIL after shutdown.")
   (owned-unix-pathname
    :initarg :owned-unix-pathname
    :initform nil
    :reader management-repl-runtime-owned-unix-pathname
    :type (option pathname)
    :documentation "The Unix socket pathname created by this runtime.")
   (owned-unix-identity
    :initarg :owned-unix-identity
    :initform nil
    :reader management-repl-runtime-owned-unix-identity
    :type t
    :documentation "The platform file identity of the Unix socket created by this runtime.")
   (lock
    :initform (make-lock "Autolith management runtime")
    :reader management-repl-runtime-lock
    :documentation "The lock guarding lifecycle, clients, and the request queue.")
   (condition-variable
    :initform (sb-thread:make-waitqueue :name "Autolith management queue")
    :reader management-repl-runtime-condition-variable
    :documentation "The queue work and shutdown notification waitqueue.")
   (stopping-p
    :initform nil
    :accessor management-repl-runtime-stopping-p
    :type boolean
    :documentation "Whether deterministic shutdown has begun.")
   (requests
    :initform nil
    :accessor management-repl-runtime-requests
    :type list
    :documentation "The bounded FIFO of pending evaluation requests.")
   (listener-thread
    :initform nil
    :accessor management-repl-runtime-listener-thread
    :type t
    :documentation "The connection accept thread.")
   (evaluator-thread
    :initform nil
    :accessor management-repl-runtime-evaluator-thread
    :type t
    :documentation "The sole active-image evaluator thread.")
   (client-threads
    :initform nil
    :accessor management-repl-runtime-client-threads
    :type list
    :documentation "The authenticated connection handler threads.")
   (client-sockets
    :initform nil
    :accessor management-repl-runtime-client-sockets
    :type list
    :documentation "The accepted sockets closed during shutdown."))
  (:documentation "The isolated authenticated active-image management subsystem."))

(-> management-repl--remaining-seconds (management-repl-request) real)
(defun management-repl--remaining-seconds (request)
  "Return REQUEST's nonnegative remaining deadline in seconds."
  (max 0
       (/ (- (management-repl-request-deadline request)
             (get-internal-real-time))
          internal-time-units-per-second)))

(-> management-repl--complete-request (management-repl-request t) null)
(defun management-repl--complete-request (request response)
  "Install RESPONSE once and wake REQUEST's waiting client."
  (with-lock-held ((management-repl-request-lock request))
    (unless (management-repl-request-completed-p request)
      (setf (management-repl-request-response request) response
            (management-repl-request-completed-p request) t)
      (sb-thread:condition-broadcast
       (management-repl-request-condition-variable request))))
  nil)

(-> management-repl--read-source-form (string) t)
(defun management-repl--read-source-form (source)
  "Safely read exactly one Lisp form from bounded SOURCE."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*readtable* (copy-readtable nil))
          (*package* (find-package '#:autolith)))
      (multiple-value-bind (form position)
          (read-from-string source nil ':end-of-input)
        (when (eq form ':end-of-input)
          (management-repl--protocol-error
           ':empty "Management evaluation source contains no form."))
        (multiple-value-bind (trailing trailing-position)
            (read-from-string source nil ':end-of-input :start position)
          (declare (ignore trailing-position))
          (unless (eq trailing ':end-of-input)
            (management-repl--protocol-error
             ':trailing "Management evaluation source contains trailing data.")))
        form))))

(-> management-repl--evaluate-request
    (management-repl-runtime management-repl-request)
    list)
(defun management-repl--evaluate-request (runtime request)
  "Evaluate REQUEST once with bounded output and a hard per-request timeout."
  (let* ((configuration (management-repl-runtime-configuration runtime))
         (output-limit
           (min (configuration-management-repl-maximum-output-size configuration)
                (max 16 (floor (configuration-management-repl-maximum-frame-size
                                configuration)
                               4))))
         (output (make-instance 'management-repl-bounded-output-stream
                                :limit output-limit)))
    (labels ((condition-response (condition timed-out-p)
               "Return a structured bounded failure for CONDITION."
               (multiple-value-bind (report report-truncated-p)
                   (management-repl--print-bounded condition output-limit)
                 (list ':evaluation-result
                       :status (if timed-out-p ':timeout ':condition)
                       :condition-type (string (type-of condition))
                       :report report
                       :report-truncated-p report-truncated-p
                       :output (management-repl-output-string output)
                       :output-truncated-p
                       (management-repl-output-truncated-p output)))))
      (handler-case
          (let ((remaining (management-repl--remaining-seconds request))
                (debugger-hook
                  (lambda (condition hook)
                    (declare (ignore hook))
                    (return-from management-repl--evaluate-request
                      (condition-response condition nil)))))
            (when (<= remaining 0)
              (error 'management-repl-capacity-error
                     :message "Management evaluation expired before execution."
                     :operation ':evaluate))
            (sb-ext:with-timeout remaining
              (let ((*package* (find-package '#:autolith))
                    (*standard-output* output)
                    (*error-output* output)
                    (*trace-output* output)
                    (*debug-io* (make-two-way-stream
                                 (make-string-input-stream "") output))
                    (*query-io* (make-two-way-stream
                                 (make-string-input-stream "") output))
                    (*terminal-io* (make-two-way-stream
                                    (make-string-input-stream "") output))
                    (sb-ext:*invoke-debugger-hook* debugger-hook)
                    (*debugger-hook* debugger-hook))
                (let ((values
                        (multiple-value-list
                         (eval
                          (management-repl--read-source-form
                           (management-repl-request-source request))))))
                  (let ((remaining-output output-limit)
                        (rendered nil)
                        (truncated-p nil))
                    (dolist (value values)
                      (multiple-value-bind (text value-truncated-p)
                          (management-repl--print-bounded value remaining-output)
                        (push text rendered)
                        (decf remaining-output (length text))
                        (when value-truncated-p
                          (setf truncated-p t))))
                    (list ':evaluation-result
                          :status ':ok
                          :values (nreverse rendered)
                          :values-truncated-p truncated-p
                          :output (management-repl-output-string output)
                          :output-truncated-p
                          (management-repl-output-truncated-p output)))))))
        (sb-ext:timeout (condition)
          (condition-response condition t))
        (serious-condition (condition)
          (condition-response condition nil))))))

(-> management-repl--run-evaluator (management-repl-runtime) null)
(defun management-repl--run-evaluator (runtime)
  "Run RUNTIME's serial bounded evaluation queue until shutdown."
  (unwind-protect
       (loop for request =
               (with-lock-held ((management-repl-runtime-lock runtime))
                 (loop while (and
                              (null (management-repl-runtime-requests runtime))
                              (not (management-repl-runtime-stopping-p runtime)))
                       do (sb-thread:condition-wait
                           (management-repl-runtime-condition-variable runtime)
                           (management-repl-runtime-lock runtime)))
                 (when (and (management-repl-runtime-stopping-p runtime)
                            (null (management-repl-runtime-requests runtime)))
                   (return-from management-repl--run-evaluator nil))
                 (pop (management-repl-runtime-requests runtime)))
             do (management-repl--complete-request
                 request
                 (management-repl--evaluate-request runtime request)))
    (let ((application (management-repl-runtime-application runtime)))
      (when (and (management-repl-runtime-stopping-p runtime)
                 (eq (application-management-repl-runtime application) runtime))
        (setf (application-management-repl-runtime application) nil)))))

(-> management-repl--submit
    (management-repl-runtime string)
    list)
(defun management-repl--submit (runtime source)
  "Submit bounded SOURCE to RUNTIME and wait no longer than its request deadline."
  (let* ((configuration (management-repl-runtime-configuration runtime))
         (timeout (configuration-management-repl-evaluation-timeout configuration))
         (request
           (make-instance
            'management-repl-request
            :source source
            :deadline (+ (get-internal-real-time)
                         (round (* timeout internal-time-units-per-second))))))
    (with-lock-held ((management-repl-runtime-lock runtime))
      (when (or (management-repl-runtime-stopping-p runtime)
                (>= (length (management-repl-runtime-requests runtime))
                    (configuration-management-repl-queue-capacity configuration)))
        (error 'management-repl-capacity-error
               :message "The management evaluation queue is full or stopping."
               :operation ':queue))
      (setf (management-repl-runtime-requests runtime)
            (nconc (management-repl-runtime-requests runtime) (list request)))
      (sb-thread:condition-notify
       (management-repl-runtime-condition-variable runtime)))
    (with-lock-held ((management-repl-request-lock request))
      (loop until (management-repl-request-completed-p request)
            for remaining = (management-repl--remaining-seconds request)
            while (plusp remaining)
            do (sb-thread:condition-wait
                (management-repl-request-condition-variable request)
                (management-repl-request-lock request)
                :timeout remaining))
      (if (management-repl-request-completed-p request)
          (management-repl-request-response request)
          (list ':evaluation-result
                :status ':timeout
                :condition-type "TIMEOUT"
                :report "Management evaluation deadline expired."
                :report-truncated-p nil
                :output ""
                :output-truncated-p nil)))))


;;;; -- Socket Endpoint --

(-> management-repl--loopback-address-p (string) boolean)
(defun management-repl--loopback-address-p (address)
  "Return true when ADDRESS is an IPv4 loopback literal."
  (handler-case
      (= (aref (sb-bsd-sockets:make-inet-address address) 0) 127)
    (error () nil)))

(-> management-repl--safe-unix-directory (pathname) null)
(defun management-repl--safe-unix-directory (pathname)
  "Create and validate PATHNAME as a private current-user directory."
  (ensure-directories-exist (merge-pathnames ".keep" pathname))
  (let ((status (platform-path-status *platform* pathname)))
    (unless (and status
                 (eq (platform-file-status-kind status) ':directory)
                 (platform-file-status-owned-p status))
      (error 'management-repl-configuration-error
             :message "The management Unix socket directory is not owned by the current user."
             :operation ':listen))
    (platform-make-private *platform* pathname))
  nil)

(-> management-repl--prepare-unix-path (pathname) null)
(defun management-repl--prepare-unix-path (pathname)
  "Quarantine and remove only the exact stale current-user socket at PATHNAME."
  (let* ((directory (uiop:pathname-directory-pathname pathname))
         (quarantine (merge-pathnames
                      (format nil ".repl-stale-~A" (make-identifier)) directory)))
    (management-repl--safe-unix-directory directory)
    (handler-case
        (let ((status (platform-path-status *platform* pathname)))
          (when status
            (unless (and (eq (platform-file-status-kind status) ':socket)
                         (platform-file-status-owned-p status))
              (error 'management-repl-configuration-error
                     :message "The management Unix socket path is occupied by an alien or non-socket object."
                     :operation ':listen
                     :reason ':unsafe-socket-path))
            (management-repl--require-stale-unix-endpoint pathname)
            (platform-replace-file *platform* pathname quarantine)
            (let ((moved (platform-path-status *platform* quarantine)))
              (unless (and moved
                           (eq (platform-file-status-kind moved) ':socket)
                           (platform-file-status-same-object-p status moved))
                (error 'management-repl-configuration-error
                       :message "The management Unix socket changed during stale-path quarantine."
                       :operation ':listen
                       :reason ':socket-replaced))
              (delete-file quarantine))))
      (platform-error ()
        (error 'management-repl-configuration-error
               :message "The management Unix socket path could not be inspected safely."
               :operation ':listen
               :reason ':socket-inspection)))))

(-> management-repl--require-stale-unix-endpoint (pathname) null)
(defun management-repl--require-stale-unix-endpoint (pathname)
  "Signal unless the socket at PATHNAME provably refuses connections."
  (let ((probe nil))
    (unwind-protect
         (handler-case
             (progn
               (setf probe (platform-connect-local *platform* pathname))
               (error 'management-repl-configuration-error
                      :message "The configured management Unix endpoint is already active."
                      :operation ':listen
                      :reason ':active-endpoint))
           (platform-error (condition)
             (unless (eq (platform-error-reason condition) ':refused)
               (error 'management-repl-configuration-error
                      :message "The configured management Unix socket could not be proven stale."
                      :operation ':listen
                      :reason ':unproven-stale))))
      (when probe
        (ignore-errors (sb-bsd-sockets:socket-close probe)))))
  nil)

(-> management-repl--make-listener (configuration)
    (values sb-bsd-sockets:socket (option pathname)))
(defun management-repl--make-listener (configuration)
  "Create CONFIGURATION's validated Unix or loopback TCP listener."
  (when (< (configuration-management-repl-maximum-frame-size configuration)
           *management-repl-minimum-frame-size*)
    (error 'management-repl-configuration-error
           :message "Management maximum frame size must be at least 128 octets."
           :operation ':listen
           :reason ':frame-size))
  (ecase (configuration-management-repl-transport configuration)
    (:unix
     (let ((pathname
             (configuration-management-repl-unix-socket-path configuration)))
       (management-repl--prepare-unix-path pathname)
       (values (platform-local-listener
                *platform* pathname
                :backlog (configuration-management-repl-maximum-clients
                          configuration))
               pathname)))
    (:tcp
     (let ((address
             (configuration-management-repl-tcp-address configuration))
           (listener
             (make-instance 'sb-bsd-sockets:inet-socket
                            :type ':stream
                            :protocol ':tcp)))
       (unless (management-repl--loopback-address-p address)
         (error 'management-repl-configuration-error
                :message "Management TCP requires an IPv4 loopback address."
                :operation ':listen
                :reason ':non-loopback))
       (handler-case
           (progn
             (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
             (sb-bsd-sockets:socket-bind
              listener
              (sb-bsd-sockets:make-inet-address address)
              (configuration-management-repl-tcp-port configuration))
             (sb-bsd-sockets:socket-listen
              listener
              (configuration-management-repl-maximum-clients configuration))
             (values listener nil))
         (error (condition)
           (ignore-errors (sb-bsd-sockets:socket-close listener))
           (error condition)))))))

(-> management-repl--request-source (t (integer 1)) string)
(defun management-repl--request-source (request maximum-source-size)
  "Validate REQUEST exactly and return its bounded source string."
  (let* ((properties (management-repl--decode-schema request ':evaluate '(:source)))
         (source (getf properties :source)))
    (unless (stringp source)
      (management-repl--protocol-error ':request "Evaluation source must be a string."))
    (when (> (length (sb-ext:string-to-octets source :external-format ':utf-8))
             maximum-source-size)
      (management-repl--protocol-error ':oversized "Evaluation source is oversized."))
    source))

(-> management-repl--handle-client
    (management-repl-runtime sb-bsd-sockets:socket)
    null)
(defun management-repl--handle-client (runtime socket)
  "Authenticate and serve bounded serial requests on SOCKET."
  (let* ((configuration (management-repl-runtime-configuration runtime))
         (maximum
           (configuration-management-repl-maximum-frame-size configuration))
         (stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf stream
                     (sb-bsd-sockets:socket-make-stream
                      socket
                      :input t
                      :output t
                      :element-type '(unsigned-byte 8)
                      :buffering ':none
                      :timeout
                      (configuration-management-repl-evaluation-timeout
                       configuration)))
               (sb-ext:with-timeout
                   (configuration-management-repl-authentication-timeout
                    configuration)
                 (management-repl--authenticate
                  stream
                  (configuration-management-repl-token-file-path configuration)
                  maximum))
               (loop for request = (management-repl-read-frame stream maximum)
                     until (eq request ':end-of-input)
                     do (management-repl--write-response
                         stream
                         (management-repl--submit
                          runtime
                          (management-repl--request-source
                           request
                           (configuration-management-repl-maximum-source-size
                            configuration)))
                         maximum)))
           (management-repl-configuration-error (condition)
             (warn "Management authentication configuration failure (~A): ~A"
                   (management-repl-configuration-error-reason condition)
                   (autolith-error-message condition)))
           (management-repl-error ()
             nil)
           (sb-ext:timeout ()
             nil)
           (error ()
             nil))
      (if stream
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> management-repl--run-client (management-repl-runtime sb-bsd-sockets:socket) null)
(defun management-repl--run-client (runtime socket)
  "Serve SOCKET and remove its resources from RUNTIME."
  (unwind-protect
       (management-repl--handle-client runtime socket)
    (with-lock-held ((management-repl-runtime-lock runtime))
      (setf (management-repl-runtime-client-threads runtime)
            (delete (current-thread) (management-repl-runtime-client-threads runtime))
            (management-repl-runtime-client-sockets runtime)
            (delete socket (management-repl-runtime-client-sockets runtime)))))
  nil)

(-> management-repl--serve (management-repl-runtime) null)
(defun management-repl--serve (runtime)
  "Accept clients up to the configured independent client bound."
  (loop
    (handler-case
        (let ((socket
                (sb-bsd-sockets:socket-accept
                 (management-repl-runtime-listener runtime))))
          (with-lock-held ((management-repl-runtime-lock runtime))
            (when (management-repl-runtime-stopping-p runtime)
              (ignore-errors (sb-bsd-sockets:socket-close socket))
              (return))
            (if (>= (length (management-repl-runtime-client-sockets runtime))
                    (configuration-management-repl-maximum-clients
                     (management-repl-runtime-configuration runtime)))
                (ignore-errors (sb-bsd-sockets:socket-close socket))
                (handler-case
                    (let ((thread
                            (make-thread
                             (lambda ()
                               (management-repl--run-client runtime socket))
                             :name "Autolith management client")))
                      (push socket
                            (management-repl-runtime-client-sockets runtime))
                      (push thread
                            (management-repl-runtime-client-threads runtime)))
                  (error ()
                    (ignore-errors
                      (sb-bsd-sockets:socket-close socket)))))))
      (error ()
        (return))))
  nil)

(-> management-repl-start (application) (option management-repl-runtime))
(defun management-repl-start (application)
  "Start APPLICATION's configured management endpoint when enabled."
  (unless
      (configuration-management-repl-enabled-p
       (application-configuration application))
    (return-from management-repl-start nil))
  (when (application-management-repl-runtime application)
    (return-from management-repl-start
      (application-management-repl-runtime application)))
  (multiple-value-bind (listener pathname)
      (management-repl--make-listener (application-configuration application))
    (let* ((status
             (and pathname (platform-path-status *platform* pathname)))
           (runtime
             (make-instance 'management-repl-runtime
                            :configuration
                            (application-configuration application)
                            :application application
                            :listener listener
                            :owned-unix-pathname pathname
                            :owned-unix-identity
                            (and status
                                 (platform-file-status-identity status)))))
      (setf (application-management-repl-runtime application) runtime)
      (handler-case
          (progn
            (setf (management-repl-runtime-evaluator-thread runtime)
                  (funcall *management-repl-start-thread-function*
                           (lambda ()
                             (management-repl--run-evaluator runtime))
                           :name "Autolith management evaluator")
                  (management-repl-runtime-listener-thread runtime)
                  (funcall *management-repl-start-thread-function*
                           (lambda ()
                             (management-repl--serve runtime))
                           :name "Autolith management endpoint"))
            runtime)
        (error (condition)
          (ignore-errors (management-repl-stop application))
          (setf (application-management-repl-runtime application) nil)
          (error condition))))))

(-> management-repl--owned-unix-path-p (management-repl-runtime) boolean)
(defun management-repl--owned-unix-path-p (runtime)
  "Return true when RUNTIME's Unix path still names the socket it created."
  (let ((pathname (management-repl-runtime-owned-unix-pathname runtime)))
    (and pathname
         (handler-case
             (let ((status (platform-path-status *platform* pathname)))
               (and status
                    (eq (platform-file-status-kind status) ':socket)
                    (equal (platform-file-status-identity status)
                           (management-repl-runtime-owned-unix-identity runtime))
                    t))
           (error () nil)))))

(-> management-repl--wait-for-threads (list real) list)
(defun management-repl--wait-for-threads (threads timeout)
  "Return threads still alive after waiting no longer than TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop for alive = (remove-if-not
                       (lambda (thread)
                         (and thread (not (eq thread (current-thread)))
                              (thread-alive-p thread)))
                       threads)
          while (and alive (< (get-internal-real-time) deadline))
          do (sleep 0.01)
          finally (return alive))))

(-> management-repl--wake-listener (management-repl-runtime) null)
(defun management-repl--wake-listener (runtime)
  "Wake a blocking accept during bounded shutdown."
  (let ((configuration (management-repl-runtime-configuration runtime))
        (socket nil))
    (unwind-protect
         (ignore-errors
           (ecase (configuration-management-repl-transport configuration)
             (:unix
              (setf socket
                    (platform-connect-local
                     *platform*
                     (configuration-management-repl-unix-socket-path
                      configuration))))
             (:tcp
              (setf socket
                    (make-instance 'sb-bsd-sockets:inet-socket
                                   :type ':stream
                                   :protocol ':tcp))
              (sb-bsd-sockets:socket-connect
               socket
               (sb-bsd-sockets:make-inet-address
                (configuration-management-repl-tcp-address configuration))
               (configuration-management-repl-tcp-port configuration)))))
      (when socket
        (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> management-repl-transfer (application application) null)
(defun management-repl-transfer (old-application new-application)
  "Transfer endpoint ownership atomically between equivalent reconnect applications."
  (let ((runtime (application-management-repl-runtime old-application)))
    (when runtime
      (with-lock-held ((management-repl-runtime-lock runtime))
        (setf (management-repl-runtime-application runtime) new-application
              (application-management-repl-runtime new-application) runtime
              (application-management-repl-runtime old-application) nil))))
  nil)

(-> management-repl-stop (application) null)
(defun management-repl-stop (application)
  "Stop within *MANAGEMENT-REPL-STOP-TIMEOUT* or signal a quiescence error."
  (let ((runtime (application-management-repl-runtime application)))
    (when runtime
      (let (listener sockets threads pending)
        (with-lock-held ((management-repl-runtime-lock runtime))
          (setf (management-repl-runtime-stopping-p runtime) t
                listener (management-repl-runtime-listener runtime)
                sockets
                (copy-list (management-repl-runtime-client-sockets runtime))
                threads
                (append
                 (list (management-repl-runtime-listener-thread runtime)
                       (management-repl-runtime-evaluator-thread runtime))
                 (management-repl-runtime-client-threads runtime))
                pending (management-repl-runtime-requests runtime)
                (management-repl-runtime-requests runtime) nil)
          (sb-thread:condition-broadcast
           (management-repl-runtime-condition-variable runtime)))
        (dolist (request pending)
          (management-repl--complete-request
           request '(:evaluation-result :status :condition)))
        (when listener
          (management-repl--wake-listener runtime))
        (when listener
          (ignore-errors (sb-bsd-sockets:socket-close listener)))
        (dolist (socket sockets)
          (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (let ((alive
                (management-repl--wait-for-threads
                 threads *management-repl-stop-timeout*)))
          (when alive
            (error 'management-repl-quiescence-error
                   :message
                   "Management threads did not quiesce before the shutdown bound."
                   :operation ':stop
                   :threads
                   (mapcar (lambda (thread)
                             (or (sb-thread:thread-name thread) "unnamed"))
                           alive))))
        (when (management-repl--owned-unix-path-p runtime)
          (ignore-errors
            (delete-file
             (management-repl-runtime-owned-unix-pathname runtime))))
        (setf (management-repl-runtime-listener runtime) nil)
        (unless (eq (current-thread)
                    (management-repl-runtime-evaluator-thread runtime))
          (setf (application-management-repl-runtime application) nil)))))
  nil)

(-> application-call-with-management-repl-quiesced (application function) t)
(defun application-call-with-management-repl-quiesced (application function)
  "Call FUNCTION only after all management threads have boundedly quiesced."
  (let* ((runtime (application-management-repl-runtime application))
         (running-p (not (null runtime))))
    (when (and runtime
               (eq (current-thread)
                   (management-repl-runtime-evaluator-thread runtime)))
      (error 'management-repl-quiescence-error
             :message "A management evaluation cannot checkpoint its own evaluator."
             :operation ':checkpoint
             :threads '("Autolith management evaluator")))
    (when running-p
      (management-repl-stop application))
    (unwind-protect
         (funcall function)
      (when running-p
        (management-repl-start application)))))
