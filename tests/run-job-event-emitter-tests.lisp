(in-package #:autolith)

;;;; -- Headless Event Emitter Tests --

(define-condition run-job-event-test-interruption (serious-condition) ()
  (:documentation "A synthetic interrupted output operation, without native thread interruption."))

(defclass run-job-event-test-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output :initform (make-string-output-stream) :reader run-job-event-test-stream-output
           :documentation "Captured complete or interrupted character writes.")
   (mode :initarg :mode :initform ':normal :reader run-job-event-test-stream-mode
         :documentation "Normal, gated, broken or interrupted test output.")
   (lock :initform (make-lock "event test stream") :reader run-job-event-test-stream-lock
         :documentation "Protects the cooperative test gate.")
   (condition :initform (make-condition-variable) :reader run-job-event-test-stream-condition
              :documentation "Wakes the deliberately gated writer.")
   (entered-p :initform nil :accessor run-job-event-test-stream-entered-p
              :documentation "Whether the writer reached the gate.")
   (released-p :initform nil :accessor run-job-event-test-stream-released-p
               :documentation "Whether the test released output."))
  (:documentation "Portable Gray stream exercising isolated delivery failures."))

(defmethod trivial-gray-streams:stream-write-string
    ((stream run-job-event-test-stream) string &optional (start 0) end)
  "Write STRING after the selected cooperative failure or delay."
  (case (run-job-event-test-stream-mode stream)
    (:gated
     (with-lock-held ((run-job-event-test-stream-lock stream))
       (setf (run-job-event-test-stream-entered-p stream) t)
       (loop until (run-job-event-test-stream-released-p stream)
             do (condition-wait (run-job-event-test-stream-condition stream)
                                (run-job-event-test-stream-lock stream)))))
    (:broken (error 'stream-error :stream stream))
    (:interrupted
     (write-string string (run-job-event-test-stream-output stream)
                   :start start :end (min (or end (length string)) (+ start 5)))
     (error 'run-job-event-test-interruption)))
  (write-string string (run-job-event-test-stream-output stream) :start start :end end)
  string)

(defmethod trivial-gray-streams:stream-write-char ((stream run-job-event-test-stream) character)
  "Delegate a character to the same output boundary as complete strings."
  (trivial-gray-streams:stream-write-string stream (string character))
  character)

(defmethod trivial-gray-streams:stream-finish-output ((stream run-job-event-test-stream))
  "The test output has no additional buffering."
  (declare (ignore stream))
  nil)

(-> run-job-event-tests--wait (function) boolean)
(defun run-job-event-tests--wait (predicate)
  "Wait a bounded two seconds for a cooperative test observation."
  (let ((deadline (+ (run-job-event--clock) 2)))
    (loop until (funcall predicate)
          do (when (> (run-job-event--clock) deadline) (return-from run-job-event-tests--wait nil))
             (sleep 0.005))
    t))

(-> run-job-event-tests--release (run-job-event-test-stream) null)
(defun run-job-event-tests--release (stream)
  "Release a gated test stream without interrupting its writer thread."
  (with-lock-held ((run-job-event-test-stream-lock stream))
    (setf (run-job-event-test-stream-released-p stream) t)
    (condition-notify (run-job-event-test-stream-condition stream)))
  nil)

(-> run-job-event-tests--records (string) list)
(defun run-job-event-tests--records (text)
  "Read all concatenated wire forms, including embedded physical newlines."
  (with-input-from-string (stream text)
    (loop for record = (run-job-event-read-stream stream) while record collect record)))

(-> run-job-event-tests--terminal () list)
(defun run-job-event-tests--terminal ()
  "Return a terminal payload whose authoritative artifact was published."
  '(:status :succeeded :output "result.sexp" :published-p t))

(-> run-job-event-tests--rejected-p (function) boolean)
(defun run-job-event-tests--rejected-p (function)
  "Return whether a data-only reader or validator rejected the test input."
  (handler-case (progn (funcall function) nil)
    ((or run-job-event-error sexp-config-error type-error) () t)))

(-> test-run-job-event-wire-roundtrip () null)
(defun test-run-job-event-wire-roundtrip ()
  "Round-trip Unicode and Lisp string escaping through complete-form readers."
  (let* ((path (format nil "Unicode-λ-日本語~%quote-\"-backslash-\\.sexp"))
         (stream (make-string-output-stream))
         (emitter (run-job-event-emitter-create "run-wire" stream)))
    (test-assert (run-job-event-emit emitter ':run-started (list :output path))
                 "Unicode output reference is accepted")
    (test-assert (run-job-event-emitter-finish
                  emitter (list :status ':succeeded :output path :published-p t))
                 "connected terminal output is delivered")
    (let* ((text (get-output-stream-string stream))
           (records (run-job-event-tests--records text)))
      (test-assert (= (length records) 2) "strings containing newlines do not split wire forms")
      (test-assert (equal (getf (getf (rest (first records)) :data) :output) path)
                   "Unicode, quote and backslash characters round-trip")
      (test-assert (equal (getf (getf (rest (second records)) :data) :output) path)
                   "terminal artifact reference round-trips")
      (test-assert (equal (mapcar (lambda (record) (getf (rest record) :seq)) records) '(1 2))
                   "serialization assigns contiguous sequences")))
  nil)

(-> test-run-job-event-reader-safety () null)
(defun test-run-job-event-reader-safety ()
  "Reject executable syntax, unapproved atoms, structures and field vocabularies."
  (dolist (source '("#.(error \"must not execute\")" "#1=(:autolith-event . #1#)"
                    "'(:autolith-event)" "(cl:eval nil)" "(:autolith-event . nil)"
                    "(:autolith-event :unknown-key t)" "(:autolith-event :seq 1.5)"
                    "(:autolith-event :seq -1)" "(:autolith-event :data #())"
                    "(:autolith-event :data #\\A)" "(:autolith-event"))
    (test-assert (run-job-event-tests--rejected-p
                  (lambda () (run-job-event-read-string source)))
                 "unsafe or malformed syntax is rejected by sexp-config")
    (test-assert (run-job-event-tests--rejected-p
                  (lambda () (with-input-from-string (stream source)
                               (run-job-event-read-stream stream))))
                 "incremental framing rejects unsafe or incomplete syntax"))
  (let ((cycle (list :count 1)))
    (setf (cddr cycle) cycle)
    (test-assert (run-job-event-tests--rejected-p
                  (lambda () (run-job-event--data-validate ':progress cycle)))
                 "cycles are rejected before printing"))
  (dolist (data (list '(:count 1 :count 2) '(:count 1 . 2) '(:count :running)
                      '(:status :not-approved) '(:prompt "secret")
                      (list :job-id (make-string 4097 :initial-element #\a))))
    (test-assert (run-job-event-tests--rejected-p
                  (lambda () (run-job-event--data-validate ':progress data)))
                 "duplicate, improper, mistyped or unrestricted fields are rejected"))
  (test-assert (run-job-event-tests--rejected-p
                (lambda () (run-job-event--data-validate
                            ':run-finished '(:status :succeeded :published-p nil))))
               "unpublished artifacts cannot be announced as success")
  (test-assert (run-job-event-tests--rejected-p
                (lambda () (run-job-event-read-string (make-string 16385 :initial-element #\Space))))
               "wire input is bounded before parsing")
  nil)

(-> test-run-job-event-owned-payload () null)
(defun test-run-job-event-owned-payload ()
  "Enqueued data and queue limits belong to the emitter, not producer bindings."
  (let* ((stream (make-instance 'run-job-event-test-stream :mode ':gated))
         (emitter (let ((*run-job-event-queue-capacity* 2))
                    (run-job-event-emitter-create "run-owned" stream)))
         (identifier (copy-seq "job-owned"))
         (uri (copy-seq "job:owned"))
         (data (list :job-id identifier :resource-uris (list uri))))
    (unwind-protect
         (progn
           (run-job-event-emit emitter ':run-started nil)
           (test-assert (run-job-event-tests--wait
                         (lambda () (run-job-event-test-stream-entered-p stream)))
                        "writer holds the gate before mutable data is queued")
           (test-assert (run-job-event-emit emitter ':tool-finished data)
                        "bounded mutable payload is accepted")
           (fill identifier #\x)
           (fill uri #\x)
           (setf (cddr data) data)
           (test-assert (not (run-job-event-emit emitter ':tool-finished data))
                        "a cyclic producer value is rejected through public admission")
           (run-job-event-emit emitter ':progress '(:count 1))
           (test-assert (not (run-job-event-emit emitter ':progress '(:count 2)))
                        "capacity captured at creation applies outside its dynamic binding")
           (run-job-event-tests--release stream)
           (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                        "mutating producer data cannot trap the delivery writer")
           (let* ((records (run-job-event-tests--records
                            (get-output-stream-string (run-job-event-test-stream-output stream))))
                  (owned (getf (rest (second records)) :data)))
             (test-assert (and (equal (getf owned :job-id) "job-owned")
                               (equal (getf owned :resource-uris) '("job:owned")))
                          "strings and nested lists are snapshotted before enqueueing")))
      (run-job-event-tests--release stream)
      (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))))
  nil)

(-> test-run-job-event-concurrent-writers () null)
(defun test-run-job-event-concurrent-writers ()
  "Concurrent producers cannot interleave complete forms or duplicate sequences."
  (let* ((*run-job-event-queue-capacity* 1024)
         (stream (make-string-output-stream))
         (emitter (run-job-event-emitter-create "run-concurrent" stream))
         (threads (loop for producer below 4
                        collect (let ((identifier (format nil "job-~D" producer)))
                                  (make-thread
                                   (lambda ()
                                     (dotimes (count 40)
                                       (run-job-event-emit emitter ':progress
                                                           (list :job-id identifier :count count)))))))))
    (mapc #'join-thread threads)
    (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                 "bounded concurrent burst completes")
    (let ((records (run-job-event-tests--records (get-output-stream-string stream))))
      (test-assert (= (length records) 161) "all accepted concurrent forms arrive")
      (test-assert (equal (mapcar (lambda (record) (getf (rest record) :seq)) records)
                          (loop for sequence from 1 to 161 collect sequence))
                   "one writer assigns strictly consecutive serialization sequences")
      (test-assert (eq (getf (rest (first (last records))) :kind) :run-finished)
                   "terminal event follows every accepted producer record")
      (test-assert (not (run-job-event-emit emitter ':progress '(:count 99)))
                   "producer acceptance closes before terminal delivery")))
  nil)

(-> test-run-job-event-progress-backpressure () null)
(defun test-run-job-event-progress-backpressure ()
  "Bound progress memory and report exact omissions after the consumer resumes."
  (let* ((*run-job-event-queue-capacity* 2)
         (stream (make-instance 'run-job-event-test-stream :mode ':gated))
         (emitter (run-job-event-emitter-create "run-omitted" stream)))
    (unwind-protect
         (progn
           (run-job-event-emit emitter ':run-started nil)
           (test-assert (run-job-event-tests--wait
                         (lambda () (run-job-event-test-stream-entered-p stream)))
                        "writer reached cooperative gate")
           (dotimes (count 20)
             (run-job-event-emit emitter ':progress (list :count count)))
           (test-assert (= (length (run-job-event-emitter-queue emitter)) 2)
                        "queue remains at its configured bound")
           (run-job-event-tests--release stream)
           (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                        "connected consumer receives final event after omissions")
           (let* ((records (run-job-event-tests--records
                            (get-output-stream-string (run-job-event-test-stream-output stream))))
                  (omitted (find :progress-omitted records
                                 :key (lambda (record) (getf (rest record) :kind)))))
             (test-assert (= (getf (getf (rest omitted) :data) :count) 18)
                          "omission summary accounts for every dropped progress event")
             (test-assert (eq (getf (rest (first (last records))) :kind) :run-finished)
                          "omission summary precedes final terminal event")))
      (run-job-event-tests--release stream)
      (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))))
  nil)

(-> test-run-job-event-critical-backpressure () null)
(defun test-run-job-event-critical-backpressure ()
  "Reserve final delivery during lifecycle bursts and evict progress first."
  (dolist (capacity '(2 64))
    (let* ((*run-job-event-queue-capacity* capacity)
           (stream (make-instance 'run-job-event-test-stream :mode ':gated))
           (emitter (run-job-event-emitter-create "run-critical" stream)))
      (unwind-protect
           (progn
             (run-job-event-emit emitter ':run-started nil)
             (test-assert (run-job-event-tests--wait
                           (lambda () (run-job-event-test-stream-entered-p stream)))
                          "lifecycle burst begins behind a gated writer")
             (dotimes (count capacity)
               (run-job-event-emit emitter ':progress (list :count count)))
             (test-assert (run-job-event-emit emitter ':tool-started '(:tool-name "test.effect"))
                          "lifecycle admission evicts queued progress before losing a transition")
             (dotimes (count (* 2 capacity))
               (run-job-event-emit emitter ':tool-started (list :tool-name "test.effect")))
             (test-assert (<= (length (run-job-event-emitter-queue emitter)) capacity)
                          "a lifecycle burst cannot exceed the queue bound")
             (run-job-event-tests--release stream)
             (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                          "overflow cannot disable the connected consumer's final event")
             (let* ((records (run-job-event-tests--records
                              (get-output-stream-string (run-job-event-test-stream-output stream))))
                    (warning (find ':warning records :key (lambda (record) (getf (rest record) :kind))))
                    (omitted (find ':progress-omitted records
                                   :key (lambda (record) (getf (rest record) :kind)))))
               (test-assert (eq (getf (rest (first (last records))) :kind) ':run-finished)
                            "run-finished is last despite lifecycle overflow")
               (test-assert (eq (getf (getf (rest warning) :data) :code) ':queue-full)
                            "non-progress loss is reported with a fixed warning")
               (test-assert (= (getf (getf (rest omitted) :data) :count) capacity)
                            "every evicted progress record is counted")))
        (run-job-event-tests--release stream)
        (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter))))))
  nil)

(-> test-run-job-event-slow-consumer () null)
(defun test-run-job-event-slow-consumer ()
  "Bound shutdown without native interruption and retain at most one blocked writer."
  (let* ((*run-job-event-shutdown-seconds* 0.05)
         (stream (make-instance 'run-job-event-test-stream :mode ':gated))
         (emitter (run-job-event-emitter-create "run-slow" stream)))
    (unwind-protect
         (progn
           (run-job-event-emit emitter ':run-started nil)
           (test-assert (run-job-event-tests--wait
                         (lambda () (run-job-event-test-stream-entered-p stream)))
                        "slow writer has an in-flight record")
           (let ((start (run-job-event--clock)))
             (test-assert (not (run-job-event-emitter-finish emitter (run-job-event-tests--terminal)))
                          "slow consumer disables delivery")
             (test-assert (< (- (run-job-event--clock) start) 0.5)
                          "slow stdout cannot hold up artifact completion"))
           (test-assert (member :slow-consumer (run-job-event-emitter-warnings emitter))
                        "slow consumer leaves a fixed warning code")
           (test-assert (null (run-job-event-emitter-queue emitter))
                        "disabled queue releases all pending payloads")
           (let ((other (run-job-event-emitter-create "run-other" (make-string-output-stream))))
             (test-assert (member :writer-unavailable (run-job-event-emitter-warnings other))
                          "a second blocked writer cannot accumulate")))
      (run-job-event-tests--release stream)
      (test-assert (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))
                   "released writer cooperatively exits")))
  nil)

(-> test-run-job-event-output-failures () null)
(defun test-run-job-event-output-failures ()
  "Isolate broken and partially interrupted output without changing run execution."
  (dolist (mode '(:broken :interrupted))
    (let* ((stream (make-instance 'run-job-event-test-stream :mode mode))
           (emitter (run-job-event-emitter-create "run-failure" stream)))
      (run-job-event-emit emitter ':run-started nil)
      (test-assert (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))
                   "failed writer terminates without affecting its producer")
      (test-assert (not (run-job-event-emitter-finish emitter (run-job-event-tests--terminal)))
                   "failed output does not claim successful terminal delivery")
      (test-assert (member (if (eq mode :broken) :stream-failed :stream-interrupted)
                           (run-job-event-emitter-warnings emitter))
                   "output failure records only a fixed safe warning")))
  (let* ((stream (make-string-output-stream))
         (emitter (run-job-event-emitter-create "run-invalid" stream)))
    (test-assert (not (run-job-event-emit emitter ':tool-started '(:arguments "secret")))
                 "unrestricted tool arguments never reach the writer")
    (test-assert (run-job-event-emitter-finish emitter (run-job-event-tests--terminal))
                 "invalid observer payload does not disable valid terminal delivery")
    (let ((records (run-job-event-tests--records (get-output-stream-string stream))))
      (test-assert (= (length records) 1) "invalid payload was not printed")))
  nil)

(-> run-job-event-tests--pipe-cleanup (t stream run-job-event-emitter) null)
(defun run-job-event-tests--pipe-cleanup (process stream emitter)
  "Reap a fixture with bounded polling and close only after its writer has exited."
  (unless (run-job-event-emitter-done-p emitter)
    (uiop:terminate-process process :urgent t))
  (when (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))
    (ignore-errors (close stream)))
  (unless (run-job-event-tests--wait (lambda () (not (uiop:process-alive-p process))))
    (uiop:terminate-process process :urgent t))
  (test-assert (run-job-event-tests--wait (lambda () (not (uiop:process-alive-p process))))
               "native pipe fixture exits within its deadline")
  (unless (uiop:process-alive-p process)
    (uiop:wait-process process))
  nil)

(-> test-run-job-event-real-pipes () null)
(defun test-run-job-event-real-pipes ()
  "Exercise actual slow and broken POSIX stdin pipes with bounded cooperative shutdown."
  (with-test-fixture (':posix-shell "headless event delivery through real pipes")
    (let* ((*run-job-event-queue-capacity* 1024)
           (*run-job-event-shutdown-seconds* 0.05)
           (process (uiop:launch-program '("/bin/sh" "-c" "sleep 1; cat >/dev/null")
                                         :input ':stream :output nil :error-output nil))
           (stream (uiop:process-info-input process))
           (emitter (run-job-event-emitter-create "run-real-slow" stream)))
      (unwind-protect
           (progn
             ;; More than a pipe's capacity, but each event is bounded independently.
             (dotimes (count 160)
               (run-job-event-emit emitter ':run-started
                                   (list :output (make-string 4000 :initial-element #\a))))
             (let ((start (run-job-event--clock)))
               (test-assert (not (run-job-event-emitter-finish emitter (run-job-event-tests--terminal)))
                            "an actual non-reading pipe disables event delivery")
               (test-assert (< (- (run-job-event--clock) start) 0.5)
                            "actual pipe backpressure has bounded completion latency"))
             (test-assert (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))
                          "writer exits after the real pipe reader resumes"))
        (run-job-event-tests--pipe-cleanup process stream emitter)))
    (let* ((process (uiop:launch-program '("/bin/sh" "-c" "exit 0")
                                         :input ':stream :output nil :error-output nil))
           (stream (uiop:process-info-input process))
           (emitter (run-job-event-emitter-create "run-real-broken" stream)))
      (unwind-protect
           (progn
             (test-assert (run-job-event-tests--wait
                           (lambda () (not (uiop:process-alive-p process))))
                          "pipe reader exits before the broken-pipe write")
             (run-job-event-emit emitter ':run-started nil)
             (test-assert (run-job-event-tests--wait (lambda () (run-job-event-emitter-done-p emitter)))
                          "closed pipe is isolated in the writer")
             (test-assert (not (run-job-event-emitter-finish emitter (run-job-event-tests--terminal)))
                          "a broken pipe cannot prevent authoritative run completion")
             (test-assert (member :stream-failed (run-job-event-emitter-warnings emitter))
                          "real EPIPE is reported as a content-free stream warning"))
        (run-job-event-tests--pipe-cleanup process stream emitter))))
  nil)
