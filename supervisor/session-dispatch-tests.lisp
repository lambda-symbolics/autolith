;;;; -- Session Wire Dispatch Checks --

(in-package #:autolith-supervisor)

(defclass session-dispatch-test-output (trivial-gray-streams:fundamental-binary-output-stream)
  ((lock :initform (bordeaux-threads:make-lock "blocked session output")
         :reader session-dispatch-test-output-lock :documentation "Protects the test gate.")
   (condition :initform (bordeaux-threads:make-condition-variable)
              :reader session-dispatch-test-output-condition :documentation "Notifies gate changes.")
   (blocked :initform nil :accessor session-dispatch-test-output-blocked
            :documentation "Whether output writes and close wait at the test gate.")
   (entered :initform nil :accessor session-dispatch-test-output-entered
            :documentation "Whether a writer reached the gate.")
   (fail-write :initform nil :accessor session-dispatch-test-output-fail-write
               :documentation "Whether writes fail before entering the gate.")
   (close-count :initform 0 :accessor session-dispatch-test-output-close-count
                :documentation "Number of close calls that reached the gate.")
   (first-close-only :initform nil :accessor session-dispatch-test-output-first-close-only
                     :documentation "Whether only the first close call waits at the gate.")
   (released :initform nil :accessor session-dispatch-test-output-released
             :documentation "Whether the test released blocked writes and close."))
  (:documentation "Controlled peer backpressure, including a blocking close operation."))

(defmethod stream-element-type ((stream session-dispatch-test-output))
  "Return the private channel's octet element type."
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-write-byte ((stream session-dispatch-test-output) byte)
  "Discard BYTE after passing the controlled peer gate."
  (when (session-dispatch-test-output-fail-write stream)
    (error 'parent-protocol-error :reason ':test-write-failure))
  (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock stream))
    (when (session-dispatch-test-output-blocked stream)
      (setf (session-dispatch-test-output-entered stream) t)
      (bordeaux-threads:condition-notify (session-dispatch-test-output-condition stream))
      (loop until (session-dispatch-test-output-released stream)
            do (bordeaux-threads:condition-wait (session-dispatch-test-output-condition stream)
                                              (session-dispatch-test-output-lock stream)))))
  byte)

(defmethod close ((stream session-dispatch-test-output) &key abort)
  "Block close as well, so deadline enforcement cannot rely on prompt stream closure."
  (declare (ignore abort))
  (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock stream))
    (let ((close-number (incf (session-dispatch-test-output-close-count stream))))
      (bordeaux-threads:condition-notify (session-dispatch-test-output-condition stream))
      (loop while (and (session-dispatch-test-output-blocked stream)
                       (or (not (session-dispatch-test-output-first-close-only stream)) (= 1 close-number))
                       (not (session-dispatch-test-output-released stream)))
            do (bordeaux-threads:condition-wait (session-dispatch-test-output-condition stream)
                                              (session-dispatch-test-output-lock stream)))))
  (call-next-method))

(declaim (ftype (function () integer) run-session-dispatch-tests))

(defun run-session-dispatch-tests ()
  "Check actual framed dispatch, correlation, durability, stop, and hostile output."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Session dispatch check ~D failed." checks)))

             (fixture (function &key (in-flight 64))
               (let ((path (merge-pathnames (format nil "dispatch-~36R.sexp" (random (expt 36 20)))
                                            (uiop:temporary-directory))))
                 (unwind-protect
                      (uiop:with-temporary-file (:stream input :element-type '(unsigned-byte 8))
                        (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
                          (let* ((store       (session-store-open path "session"))
                                 (connection  (make-instance 'parent-connection :input input :output output
                                                                               :instance-id "instance"))
                                 (now 10) (display nil) (next-id 0) (read-position 0)
                                 (output-path (pathname output)))
                            (setf (parent-connection-phase connection) ':ready
                                  (parent-proto:limits.max-in-flight-requests
                                   (parent-connection-limits connection)) in-flight)
                            (let ((runtime (session-runtime-open
                                            store connection :clock (lambda () now)
                                            :output-callback        (lambda (channel text) (push (list channel text) display)))))
                              (labels ((receive (envelope)
                                         (incf next-id)
                                         (setf (parent-proto:envelope.protocol-major envelope) 1
                                               (parent-proto:envelope.instance-id envelope) "instance"
                                               (parent-proto:envelope.message-id envelope) next-id)
                                         (file-position input :end)
                                         (let ((position (file-position input)))
                                           (protocol-write-frame input (cl-protobufs:serialize-to-bytes envelope))
                                           (file-position input position)
                                           (session-runtime-receive runtime)))

                                       (response ()
                                         (with-open-file (source output-path :element-type '(unsigned-byte 8))
                                           (file-position source read-position)
                                           (let ((bytes (protocol-read-frame source)))
                                             (setf read-position (file-position source))
                                             (when bytes
                                               (cl-protobufs:deserialize-from-bytes 'parent-proto:envelope bytes)))))

                                       (rejection (envelope code)
                                         (receive envelope)
                                         (let ((reply (response)))
                                           (check (eq 'parent-proto:failure (parent-proto:envelope.body-case reply)))
                                           (check (= next-id (parent-proto:envelope.reply-to reply)))
                                           (check (eq code (parent-proto:failure.code (parent-proto:envelope.failure reply))))
                                           (check (plusp (length
                                                          (parent-proto:failure.message (parent-proto:envelope.failure reply)))))))

                                       (append-note (event turn text)
                                         (let ((map (make-hash-table :test 'equal)))
                                           (setf (gethash "text" map) text)
                                           (parent-proto:make-envelope
                                            :session-append (parent-proto:make-session-append
                                                             :record (parent-proto:make-session-record
                                                                      :event-id event :turn-id turn :kind ':note
                                                                      :payload (protocol-data->value map)))))))
                                (unwind-protect
                                     (funcall function runtime #'receive #'response #'rejection #'append-note
                                              (lambda () display) (lambda (time) (setf now time)))
                                  (session-runtime-disconnect runtime)
                                  (session-runtime-await-close runtime)))))))
                   (when (probe-file path) (delete-file path))
                   (let ((lock (pathname (concatenate 'string (namestring path) ".lock"))))
                     (when (probe-file lock) (delete-file lock)))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore time))
         (check (= 1 (session-runtime-start-turn runtime "turn" "parent user input")))
         (let* ((start-envelope (funcall response))
                (start          (parent-proto:envelope.turn-start start-envelope)))
           (check (not (parent-proto:envelope.has-reply-to start-envelope)))
           (check (= 1 (parent-proto:turn-start.input-sequence start)))
           (check (= 1 (parent-proto:session-page.high-water-sequence
                        (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1))))))
         (funcall receive (funcall note "note" "turn" "first"))
         (let ((ack-envelope (funcall response)))
           (check (= 1 (parent-proto:envelope.reply-to ack-envelope)))
           (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack ack-envelope)))))
         (funcall receive (funcall note "note" "turn" "first"))
         (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
         (funcall rejection (funcall note "note" "turn" "conflict") ':revision-conflict)
         (funcall rejection (parent-proto:make-envelope :session-append (parent-proto:make-session-append)) ':invalid-request)
         (funcall rejection (parent-proto:make-envelope :session-read (parent-proto:make-session-read)) ':resource-limit)
         (funcall receive (parent-proto:make-envelope :session-read (parent-proto:make-session-read :limit 256)))
         (check (= 2 (length (parent-proto:session-page.records (parent-proto:envelope.session-page (funcall response))))))
         (dolist (envelope (list (parent-proto:make-envelope :model-start (parent-proto:make-model-start))
                                 (parent-proto:make-envelope :record-request (parent-proto:make-record-request))
                                 (parent-proto:make-envelope :access-request (parent-proto:make-access-request))))
           (funcall rejection envelope ':unsupported))
         (funcall rejection (parent-proto:make-envelope :cancel-request (parent-proto:make-cancel-request :request-id 900)) ':invalid-request)
         (dolist (envelope (list (parent-proto:make-envelope :turn-start (parent-proto:make-turn-start))
                                 (parent-proto:make-envelope :session-page (parent-proto:make-session-page))
                                 (parent-proto:make-envelope :ready (parent-proto:make-ready))
                                 (parent-proto:make-envelope)))
           (funcall rejection envelope ':invalid-request))
         (funcall rejection (parent-proto:make-envelope :reply-to 999 :session-read (parent-proto:make-session-read :limit 1)) ':invalid-request)
         (dolist (fields '((999 "turn" :completed 2) (1 "other" :completed 2)
                           (1 "turn" :outcome-unspecified 2) (1 "turn" :completed 999)))
           (destructuring-bind (reply turn outcome sequence) fields
             (funcall rejection (parent-proto:make-envelope
                                 :reply-to reply :turn-finish (parent-proto:make-turn-finish
                                                               :turn-id turn :outcome outcome :last-session-sequence sequence))
                      ':invalid-request)))
         (check (not (session-runtime-cancel-turn runtime "inactive")))
         (check (session-runtime-cancel-turn runtime "turn"))
         (check (eq 'parent-proto:turn-cancel (parent-proto:envelope.body-case (funcall response))))
         (check (session-runtime-cancel-turn runtime "turn"))
         (funcall response)
         (funcall receive (parent-proto:make-envelope :output (parent-proto:make-output
                                                               :channel ':progress :text (format nil "~C]52;c;secret~C~Cok~C[31mred~C[0m~%done"
                                                                                                 #\Escape #\Escape #\\ #\Escape #\Escape))))
         (check (= 1 (length (funcall display))))
         (check (string= (second (first (funcall display))) (format nil "[agent progress] okred~%[agent progress] done")))
         (check (null (funcall response)))
         (funcall rejection (parent-proto:make-envelope :output (parent-proto:make-output :channel ':channel-unspecified :text "bad")) ':invalid-request)
         (funcall rejection (parent-proto:make-envelope :output (parent-proto:make-output :channel ':diagnostic :text (make-string 8193))) ':resource-limit)
         (funcall receive (parent-proto:make-envelope :reply-to 1 :turn-finish (parent-proto:make-turn-finish
                                                                                :turn-id "turn" :outcome ':completed :last-session-sequence 2)))
         (check (null (session-runtime-turn-id runtime)))
         (check (= 3 (parent-proto:session-page.high-water-sequence
                      (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1)))))
         (funcall rejection (parent-proto:make-envelope :reply-to 1 :turn-finish (parent-proto:make-turn-finish
                                                                                  :turn-id "turn" :outcome ':completed :last-session-sequence 2)) ':invalid-request)
         (funcall receive (funcall note "note" "turn" "first"))
         (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore display time))
         (session-runtime-start-turn runtime "stop-turn" "input")
         (funcall response)
         (let ((stop-id (session-runtime-prepare-stop runtime :grace-milliseconds 5000)))
           (check (eq 'parent-proto:prepare-stop (parent-proto:envelope.body-case (funcall response))))
           (check (eq ':stopping (session-runtime-phase runtime)))
           (check (handler-case (progn (session-runtime-start-turn runtime "no" "no") nil)
                    (parent-protocol-error () t)))
           (let ((request (funcall note "blocked" "stop-turn" "text")))
             (setf (parent-proto:session-record.kind (parent-proto:session-append.record
                                                      (parent-proto:envelope.session-append request))) ':tool-started)
             (funcall rejection request ':cancelled))
           (funcall receive (funcall note "wrap" "stop-turn" "wrap up"))
           (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
           (funcall rejection (parent-proto:make-envelope :reply-to stop-id :ready-to-stop
                                                         (parent-proto:make-ready-to-stop :last-session-sequence 1)) ':invalid-request)
           (funcall rejection (parent-proto:make-envelope :reply-to stop-id :ready-to-stop
                                                         (parent-proto:make-ready-to-stop :last-session-sequence 2 :wrap-up-event-id "unknown")) ':invalid-request)
           (funcall receive (parent-proto:make-envelope :reply-to stop-id :ready-to-stop
                                                       (parent-proto:make-ready-to-stop :last-session-sequence 2 :wrap-up-event-id "wrap")))
           (check (eq ':stopped (session-runtime-phase runtime)))
           (check (null (funcall response)))
           (check (null (session-runtime-turn-request runtime))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore receive rejection note display))
         (session-runtime-start-turn runtime "deadline" "input")
         (funcall response)
         (session-runtime-prepare-stop runtime :grace-milliseconds 100)
         (funcall response)
         (check (not (session-runtime-expire runtime)))
         (funcall time 11)
         (check (session-runtime-expire runtime))
         (check (eq ':closed (parent-connection-phase (session-runtime-connection runtime))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore receive rejection note display time))
         (session-runtime-start-turn runtime "bound" "input")
         (funcall response)
         (check (handler-case (progn (session-runtime-prepare-stop runtime) nil)
                  (parent-protocol-error (condition) (eq ':in-flight-limit (parent-protocol-error-reason condition)))))
         (session-runtime-disconnect runtime))
       :in-flight 1)
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore note display time))
         (session-runtime-start-turn runtime "effects" "input")
         (funcall response)
         (labels ((tool (event kind &rest fields)
                    (let ((map (make-hash-table :test 'equal)))
                      (loop for (key value) on fields by (lambda (tail) (rest (rest tail)))
                            do (setf (gethash key map) value))
                      (parent-proto:make-envelope
                       :session-append (parent-proto:make-session-append
                                        :record (parent-proto:make-session-record
                                                 :event-id event :turn-id "effects" :kind kind
                                                 :payload (protocol-data->value map)))))))
           (let ((start (tool "tool-start" ':tool-started "call_id" "call" "name" "effect" "arguments" (make-hash-table :test 'equal))))
             (funcall receive start)
             (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
             (check (= 1 (length (session-store-pending-tools (session-runtime-store runtime)))))
             (let ((stop (session-runtime-prepare-stop runtime :grace-milliseconds 1000)))
               (funcall response)
               (funcall receive start)
               (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
               (funcall rejection (tool "wrong-finish" ':tool-finished "call_id" "unknown" "result" "ok" "is_error" nil) ':invalid-request)
               (funcall receive (tool "tool-finish" ':tool-finished "call_id" "call" "result" "ok" "is_error" nil))
               (check (= 3 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
               (check (null (session-store-pending-tools (session-runtime-store runtime))))
               (funcall receive (parent-proto:make-envelope :reply-to stop :ready-to-stop
                                                           (parent-proto:make-ready-to-stop :last-session-sequence 3 :wrap-up-event-id "tool-finish")))
               (check (eq ':stopped (session-runtime-phase runtime)))
               (funcall rejection (parent-proto:make-envelope :reply-to stop :ready-to-stop
                                                             (parent-proto:make-ready-to-stop :last-session-sequence 3)) ':invalid-request))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore rejection note display time))
         (session-runtime-start-turn runtime "unknown-effects" "input")
         (funcall response)
         (let ((map (make-hash-table :test 'equal)))
           (setf (gethash "call_id" map) "pending" (gethash "name" map) "effect"
                 (gethash "arguments" map) (make-hash-table :test 'equal))
           (funcall receive (parent-proto:make-envelope
                             :session-append (parent-proto:make-session-append
                                              :record (parent-proto:make-session-record
                                                       :event-id "pending-start" :turn-id "unknown-effects"
                                                       :kind ':tool-started :payload (protocol-data->value map))))))
         (funcall response)
         ;; Real EOF is fatal and records interrupted work, without retrying it.
         (check (handler-case (progn (session-runtime-receive runtime) nil)
                  (parent-protocol-error () t)))
         (check (eq ':closed (session-runtime-phase runtime)))
         (check (equal ':unknown (getf (first (session-store-pending-tools (session-runtime-store runtime))) :effects)))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore note display))
         (funcall rejection (parent-proto:make-envelope :model-start (parent-proto:make-model-start)) ':unsupported)
         (loop repeat 2 do
           (funcall receive (parent-proto:make-envelope :cancel-request (parent-proto:make-cancel-request :request-id 1)))
           (let ((ack (parent-proto:envelope.cancel-ack (funcall response))))
             (check (parent-proto:cancel-ack.already-terminal ack))))
         (loop repeat 7 do
           (funcall receive (parent-proto:make-envelope :output (parent-proto:make-output :channel ':progress :text (make-string 8192 :initial-element #\a)))))
         (funcall rejection (parent-proto:make-envelope :output (parent-proto:make-output :channel ':progress :text (make-string 8192 :initial-element #\a))) ':resource-limit)
         (funcall time 12)
         (funcall receive (parent-proto:make-envelope :output (parent-proto:make-output :channel ':progress :text "new window")))
         (check (null (funcall response)))
         (check (handler-case (progn (session-runtime-open (session-runtime-store runtime)
                                                          (session-runtime-connection runtime)
                                                          :features '("session.v1" "model.v1")) nil)
                  (parent-protocol-error () t)))
         (session-runtime-disconnect runtime)))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore rejection display time))
         (session-runtime-start-turn runtime "pages" "input")
         (funcall response)
         (funcall receive (funcall note "page-note" "pages" "second"))
         (funcall response)
         (funcall receive (parent-proto:make-envelope :session-read (parent-proto:make-session-read :limit 1)))
         (let* ((page    (parent-proto:envelope.session-page (funcall response)))
                (resumed (session-runtime-open (session-runtime-store runtime) (session-runtime-connection runtime) :resume-page page)))
           (check (= 2 (parent-proto:session-page.high-water-sequence page)))
           (check (parent-proto:session-page.has-more page))
           (check (= 1 (session-runtime-acknowledged resumed)))
           (check (= 1 (session-runtime-page-after resumed)))
           (check (= 2 (session-runtime-page-high resumed))))
         (funcall receive (funcall note "page-new" "pages" "third"))
         (funcall response)
         (funcall receive (parent-proto:make-envelope :session-read (parent-proto:make-session-read :after-sequence 1 :limit 1)))
         (let ((page (parent-proto:envelope.session-page (funcall response))))
           (check (= 2 (parent-proto:session-page.high-water-sequence page)))
           (check (not (parent-proto:session-page.has-more page)))
           (check (= 2 (parent-proto:session-record.sequence (first (parent-proto:session-page.records page))))))
         (funcall receive (parent-proto:make-envelope :session-read (parent-proto:make-session-read :after-sequence 2 :limit 1)))
         (check (= 3 (parent-proto:session-page.high-water-sequence (parent-proto:envelope.session-page (funcall response)))))
         (session-runtime-disconnect runtime)))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore display))
         (session-runtime-start-turn runtime "rate" "input")
         (funcall response)
         (loop for index below 64 do
           (funcall receive (funcall note (format nil "rate-~D" index) "rate" "note"))
           (funcall response))
         (funcall rejection (funcall note "rate-over" "rate" "note") ':resource-limit)
         (funcall time 12)
         (funcall receive (funcall note "rate-over" "rate" "note"))
         (check (= 66 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
         (funcall rejection (parent-proto:make-envelope :reply-to 999 :failure
                                                       (parent-proto:make-failure :code ':internal :message "not yours")) ':invalid-request)
         (funcall rejection (parent-proto:make-envelope :reply-to 1 :failure
                                                       (parent-proto:make-failure :code ':internal)) ':invalid-request)
         (funcall receive (parent-proto:make-envelope :reply-to 1 :failure
                                                     (parent-proto:make-failure :code ':internal :message "untrusted failure details")))
         (check (null (session-runtime-turn-request runtime)))
         (check (eq ':running (session-runtime-phase runtime)))
         (session-runtime-disconnect runtime)))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore receive response display time))
         (funcall rejection (funcall note "early" "unstarted" "not authorized") ':invalid-request)
         (close (parent-connection-output (session-runtime-connection runtime)))
         (check (handler-case (progn (session-runtime-start-turn runtime "lost-send" "persist first") nil)
                  (error () t)))
         (check (eq ':closed (session-runtime-phase runtime)))
         (session-runtime-await-close runtime)
         (let ((page (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 256))))
           (check (= 2 (parent-proto:session-page.high-water-sequence page)))
           (check (equal '(:user-input :turn-outcome)
                         (mapcar #'parent-proto:session-record.kind (parent-proto:session-page.records page)))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore note display time))
         (loop for index from 1 to 4096 do
           (funcall rejection
                    (if (oddp index)
                        (parent-proto:make-envelope :model-start (parent-proto:make-model-start))
                        (parent-proto:make-envelope :access-request (parent-proto:make-access-request)))
                    ':unsupported))
         (check (= 4096 (hash-table-count (session-runtime-terminal-requests runtime))))
         (dolist (target '(1 4096))
           (funcall receive (parent-proto:make-envelope :cancel-request (parent-proto:make-cancel-request :request-id target)))
           (check (parent-proto:cancel-ack.already-terminal (parent-proto:envelope.cancel-ack (funcall response)))))
         (funcall rejection (parent-proto:make-envelope :model-start (parent-proto:make-model-start)) ':resource-limit)
         (check (eq ':closed (session-runtime-phase runtime)))
         (session-runtime-await-close runtime)
         (check (zerop (hash-table-count (session-runtime-terminal-requests runtime))))
         (check (zerop (parent-proto:session-page.high-water-sequence
                        (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1)))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore display time))
         (session-runtime-start-turn runtime "history" "input")
         (funcall response)
         (funcall receive (funcall note "known" "history" "one durable note"))
         (funcall response)
         (loop for index below 4095 do
           (setf (gethash (format nil "seed-~D" index) (session-runtime-append-acks runtime)) 2))
         (check (= 4096 (hash-table-count (session-runtime-append-acks runtime))))
         (funcall receive (funcall note "known" "history" "one durable note"))
         (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
         (funcall rejection (funcall note "over-cap" "history" "must not commit") ':resource-limit)
         (check (eq ':closed (session-runtime-phase runtime)))
         (session-runtime-await-close runtime)
         (check (zerop (hash-table-count (session-runtime-append-acks runtime))))
         (check (equal '(:user-input :note :turn-outcome)
                       (mapcar #'parent-proto:session-record.kind
                               (parent-proto:session-page.records
                                (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 256))))))))
      (dolist (mode '(:internal-expiry :send-error :receive-error))
        (fixture
         (lambda (runtime receive response rejection note display time)
           (declare (ignore rejection note display))
           (session-runtime-start-turn runtime "close-gate" "input")
           (funcall response)
           (when (eq mode ':internal-expiry)
             (session-runtime-prepare-stop runtime :grace-milliseconds 1000)
             (funcall response)
             (funcall time 12))
           (let ((gate      (make-instance 'session-dispatch-test-output))
                 (operation nil))
             (setf (slot-value (session-runtime-connection runtime) 'output) gate
                   (session-dispatch-test-output-blocked gate) t
                   (session-dispatch-test-output-fail-write gate) (eq mode ':send-error)
                   (session-dispatch-test-output-first-close-only gate) (not (eq mode ':internal-expiry)))
             (when (eq mode ':receive-error)
               (incf (parent-connection-received (session-runtime-connection runtime))))
             (unwind-protect
                  (progn
                    (setf operation
                          (bordeaux-threads:make-thread
                           (lambda ()
                             (handler-case
                                 (if (eq mode ':send-error)
                                     (session-runtime-cancel-turn runtime "close-gate")
                                     (funcall receive (parent-proto:make-envelope
                                                       :session-read (parent-proto:make-session-read :limit 1))))
                               (error () ':closed)))
                           :name "internal session close check"))
                    (sb-ext:with-timeout 2
                      (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock gate))
                        (loop until (plusp (session-dispatch-test-output-close-count gate))
                              do (bordeaux-threads:condition-wait (session-dispatch-test-output-condition gate)
                                                                (session-dispatch-test-output-lock gate)))))
                    (when (eq mode ':internal-expiry)
                      (check (null (sb-ext:with-timeout 1 (bordeaux-threads:join-thread operation)))))
                    (check (null (sb-ext:with-timeout 1 (session-runtime-disconnect runtime))))
                    (unless (eq mode ':internal-expiry)
                      (sb-ext:with-timeout 2
                        (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock gate))
                          (loop until (= 2 (session-dispatch-test-output-close-count gate))
                                do (bordeaux-threads:condition-wait (session-dispatch-test-output-condition gate)
                                                                  (session-dispatch-test-output-lock gate) :timeout 0.01)))))
                    (let ((cleanup (session-runtime-cleanup-thread runtime)))
                      (check cleanup)
                      (check (eq ':closed (session-runtime-phase runtime)))
                      (let ((closer (bordeaux-threads:make-thread
                                     (lambda () (session-runtime-disconnect runtime))
                                     :name "concurrent session disconnect check")))
                        (check (null (sb-ext:with-timeout 1 (bordeaux-threads:join-thread closer)))))
                      (check (eq cleanup (session-runtime-cleanup-thread runtime)))
                      (check (handler-case (progn (session-runtime-await-close runtime :timeout 0.01) nil)
                               (parent-protocol-error (condition)
                                 (eq ':close-timeout (parent-protocol-error-reason condition)))))))
               (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock gate))
                 (setf (session-dispatch-test-output-released gate) t)
                 (dotimes (index 3)
                   (bordeaux-threads:condition-notify (session-dispatch-test-output-condition gate))))
               (when operation (sb-ext:with-timeout 2 (bordeaux-threads:join-thread operation)))
               (session-runtime-await-close runtime :timeout 2))
             (check (= (if (eq mode ':internal-expiry) 1 2)
                       (session-dispatch-test-output-close-count gate)))
             (check (= 2 (parent-proto:session-page.high-water-sequence
                          (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1)))))))))
      (dolist (mode '(:write :callback :prepare :queued))
        (fixture
         (lambda (runtime receive response rejection note display time)
           (declare (ignore rejection note display))
           (session-runtime-start-turn runtime "blocked" "input")
           (funcall response)
           (let ((gate (make-instance 'session-dispatch-test-output)) (writer nil) (stopper nil))
             (unless (eq mode ':callback)
               (setf (slot-value (session-runtime-connection runtime) 'output) gate))
             (unless (member mode '(:prepare :queued))
               (session-runtime-prepare-stop runtime :grace-milliseconds 1000))
             (when (eq mode ':callback)
               (setf (slot-value runtime 'output-callback)
                     (lambda (channel text) (declare (ignore channel text)) (write-byte 0 gate))))
             (setf (session-dispatch-test-output-blocked gate) t)
             (unwind-protect
                  (progn
                    (setf writer
                          (bordeaux-threads:make-thread
                           (lambda ()
                             (handler-case
                                 (case mode
                                   (:prepare
                                    (session-runtime-prepare-stop runtime :grace-milliseconds 1000))
                                   (:callback
                                    (funcall receive (parent-proto:make-envelope
                                                      :output (parent-proto:make-output :channel ':progress :text "blocked callback"))))
                                   (otherwise
                                    (funcall receive (parent-proto:make-envelope :session-read (parent-proto:make-session-read :limit 1)))))
                               (error () ':closed)))
                           :name "blocked session check"))
                    (sb-ext:with-timeout 2
                      (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock gate))
                        (loop until (session-dispatch-test-output-entered gate)
                              do (bordeaux-threads:condition-wait (session-dispatch-test-output-condition gate)
                                                                (session-dispatch-test-output-lock gate)))))
                    (when (eq mode ':queued)
                      (setf stopper (bordeaux-threads:make-thread
                                     (lambda ()
                                       (handler-case (session-runtime-prepare-stop runtime :grace-milliseconds 1000)
                                         (error () ':closed)))
                                     :name "queued session stop"))
                      (sb-ext:with-timeout 2
                        (loop until (eq ':stopping (session-runtime-phase runtime)) do (sleep 0.001))))
                    (funcall time 12)
                    (check (sb-ext:with-timeout 1 (session-runtime-expire runtime)))
                    (check (eq ':closed (session-runtime-phase runtime)))
                    (check (eq ':closed (parent-connection-phase (session-runtime-connection runtime))))
                    (check (handler-case (progn (session-runtime-await-close runtime :timeout 0.01) nil)
                             (parent-protocol-error (condition)
                               (eq ':close-timeout (parent-protocol-error-reason condition))))))
               (bordeaux-threads:with-lock-held ((session-dispatch-test-output-lock gate))
                 (setf (session-dispatch-test-output-released gate) t)
                 (bordeaux-threads:condition-notify (session-dispatch-test-output-condition gate))
                 (bordeaux-threads:condition-notify (session-dispatch-test-output-condition gate)))
               (when writer (sb-ext:with-timeout 2 (bordeaux-threads:join-thread writer)))
               (when stopper (sb-ext:with-timeout 2 (bordeaux-threads:join-thread stopper)))
               (session-runtime-await-close runtime :timeout 2))
             (check (= 2 (parent-proto:session-page.high-water-sequence
                          (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1)))))))))
      (fixture
       (lambda (runtime receive response rejection note display time)
         (declare (ignore display time))
         (session-runtime-start-turn runtime "byte-quota" "input")
         (funcall response)
         (let ((text (make-string (* 4 1024 1024) :initial-element #\a)))
           (funcall receive (funcall note "large-first" "byte-quota" text))
           (check (= 2 (parent-proto:session-ack.sequence (parent-proto:envelope.session-ack (funcall response)))))
           (funcall rejection (funcall note "large-second" "byte-quota" text) ':resource-limit)
           (check (= 2 (parent-proto:session-page.high-water-sequence
                        (session-store-read (session-runtime-store runtime) (parent-proto:make-session-read :limit 1))))))))
      (check (= 32768 (session-output--bytes (session-output-sanitize (make-string 8192 :initial-element #\Newline) ':diagnostic))))
      (check (<= (session-output--bytes (session-output-sanitize (make-string 8192 :initial-element (code-char #x1f600)) ':diagnostic)) 32768))
      (dolist (case (list (list (format nil "~C]52;secret~Cok" #\Escape (code-char 7)) "ok")
                         (list (format nil "~C52;secret~Cok" (code-char #x9d) (code-char #x9c)) "ok")
                         (list (format nil "~CPsecret~C~Cok" #\Escape #\Escape #\\) "ok")
                         (list (format nil "~C[2J~C[31mok~C[0m" #\Escape #\Escape #\Escape) "ok")
                         (list (format nil "~C]unterminated" #\Escape) "")))
        (check (string= (concatenate 'string "[agent diagnostic] " (second case))
                        (session-output-sanitize (first case) ':diagnostic))))
      (check (string= "[agent diagnostic] safe"
                      (session-output-sanitize (format nil "~C[2Jsafe~C" #\Escape #\Backspace) ':diagnostic)))
      checks)))
