;;;; -- Agent Protocol Wire Checks --

(in-package #:autolith)

(defclass agent-protocol-test-pipe (trivial-gray-streams:fundamental-binary-input-stream
                                    trivial-gray-streams:fundamental-binary-output-stream)
  ((lock :initform (bordeaux-threads:make-lock "test byte pipe") :reader agent-protocol-test-pipe-lock
         :documentation "Protect the byte queue.")
   (ready :initform (bordeaux-threads:make-condition-variable) :reader agent-protocol-test-pipe-ready
          :documentation "Wake a reader after a byte write.")
   (bytes :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
          :reader agent-protocol-test-pipe-bytes :documentation "Small test-only byte queue.")
   (position :initform 0 :accessor agent-protocol-test-pipe-position
             :documentation "Next unread byte index."))
  (:documentation "A blocking byte pipe for real concurrent serialized handshakes."))

(defmethod stream-element-type ((pipe agent-protocol-test-pipe))
  "Declare the test channel's byte element type."
  (declare (ignore pipe))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((pipe agent-protocol-test-pipe))
  "Read the next queued byte, waiting for the test peer."
  (bordeaux-threads:with-lock-held ((agent-protocol-test-pipe-lock pipe))
    (loop while (= (agent-protocol-test-pipe-position pipe)
                   (length (agent-protocol-test-pipe-bytes pipe)))
          do (unless (open-stream-p pipe) (return-from trivial-gray-streams:stream-read-byte ':eof))
             (bordeaux-threads:condition-wait (agent-protocol-test-pipe-ready pipe)
                                             (agent-protocol-test-pipe-lock pipe)))
    (prog1 (aref (agent-protocol-test-pipe-bytes pipe) (agent-protocol-test-pipe-position pipe))
      (incf (agent-protocol-test-pipe-position pipe)))))

(defmethod trivial-gray-streams:stream-write-byte ((pipe agent-protocol-test-pipe) byte)
  "Append one test byte and wake the peer."
  (bordeaux-threads:with-lock-held ((agent-protocol-test-pipe-lock pipe))
    (vector-push-extend byte (agent-protocol-test-pipe-bytes pipe))
    (bordeaux-threads:condition-notify (agent-protocol-test-pipe-ready pipe)))
  byte)

(defmethod close :after ((pipe agent-protocol-test-pipe) &key abort)
  "Wake the test reader when a test closes its channel."
  (declare (ignore abort))
  (bordeaux-threads:with-lock-held ((agent-protocol-test-pipe-lock pipe))
    (bordeaux-threads:condition-notify (agent-protocol-test-pipe-ready pipe))))

(-> agent-protocol-test--welcome () agent-wire:welcome)
(defun agent-protocol-test--welcome ()
  "Build one valid parent session Welcome."
  (agent-wire:make-welcome
   :session-id "client-session" :features '("session.v1")
   :limits (agent-wire:make-limits :max-frame-bytes 8192 :max-value-depth 32
                                  :max-collection-entries 512 :max-in-flight-requests 4)
   :resume (agent-wire:make-session-page)))

(-> agent-protocol-test--peers (function function) null)
(defun agent-protocol-test--peers (server-function client-function)
  "Run real byte-channel parent and client peers under bounded deadlines."
  (let* ((to-parent    (make-instance 'agent-protocol-test-pipe))
         (to-agent     (make-instance 'agent-protocol-test-pipe))
         (parent       (make-instance 'autolith-supervisor::parent-connection
                                     :input to-parent :output to-agent :instance-id "client-instance"))
         (client       (agent-protocol-open to-agent to-parent "client-instance"))
         (server-error nil)
         (thread       (bordeaux-threads:make-thread
                        (lambda ()
                          (handler-case
                              (sb-ext:with-timeout 10 (funcall server-function parent))
                            (error (condition) (setf server-error condition)
                              (autolith-supervisor::parent-connection-close parent))))
                        :name "test parent protocol peer")))
    (unwind-protect
         (progn
           (sb-ext:with-timeout 10 (funcall client-function client))
           (bordeaux-threads:join-thread thread)
           (when server-error (error server-error)))
      (agent-protocol-close client)
      (sb-ext:with-timeout 12
        (bordeaux-threads:join-thread thread))))
  nil)

(-> agent-protocol-test--session () null)
(defun agent-protocol-test--session ()
  "Exercise handshake, correlated session replies, cancellation, output, and stop."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :turn-start (agent-wire:make-turn-start
                                                    :turn-id "turn-1" :input-sequence 1)))
     (let ((request (autolith-supervisor::parent-connection-receive parent)))
       (assert (eq 'agent-wire:session-read (agent-wire:envelope.body-case request)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                         :session-page (agent-wire:make-session-page :high-water-sequence 1))))
     (let ((request (autolith-supervisor::parent-connection-receive parent)))
       (assert (eq 'agent-wire:session-append (agent-wire:envelope.body-case request)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to    (agent-wire:envelope.message-id request)
                                         :session-ack (agent-wire:make-session-ack :event-id "note-1" :sequence 2))))
     (assert (eq 'agent-wire:output (agent-wire:envelope.body-case
                                     (autolith-supervisor::parent-connection-receive parent))))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :turn-cancel (agent-wire:make-turn-cancel :turn-id "turn-1")))
     (let ((finish (autolith-supervisor::parent-connection-receive parent)))
       (assert (= 2 (agent-wire:envelope.reply-to finish)))
       (assert (eq ':cancelled (agent-wire:turn-finish.outcome (agent-wire:envelope.turn-finish finish))))
       (assert (= 2 (agent-wire:turn-finish.last-session-sequence (agent-wire:envelope.turn-finish finish)))))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :prepare-stop (agent-wire:make-prepare-stop :grace-milliseconds 1000)))
     (let ((ready (autolith-supervisor::parent-connection-receive parent)))
       (assert (= 6 (agent-wire:envelope.reply-to ready)))
       (assert (= 2 (agent-wire:ready-to-stop.last-session-sequence (agent-wire:envelope.ready-to-stop ready))))))
   (lambda (client)
     (let ((initialized nil))
       (agent-protocol-handshake client :initialize (lambda (welcome)
                                                     (assert (string= "client-session" (agent-wire:welcome.session-id welcome)))
                                                     (setf initialized t)))
       (assert initialized))
     (assert (eq 'agent-wire:turn-start (agent-wire:envelope.body-case (agent-protocol-receive client))))
     (let ((id (agent-protocol-session-read client :limit 2)))
       (assert (= id (agent-wire:envelope.reply-to (agent-protocol-receive client)))))
     (let* ((map    (make-hash-table :test 'equal))
            (record (progn (setf (gethash "text" map) "note")
                           (agent-wire:make-session-record
                            :event-id "note-1" :turn-id "turn-1" :kind ':note
                            :payload (autolith-supervisor::protocol-data->value map)))))
       (agent-protocol-session-append client record)
       (assert (eq 'agent-wire:session-ack (agent-wire:envelope.body-case (agent-protocol-receive client)))))
     (agent-protocol-output client ':progress "working")
     (assert (eq 'agent-wire:turn-cancel (agent-wire:envelope.body-case (agent-protocol-receive client))))
     (agent-protocol-finish-turn client ':cancelled)
     (assert (eq 'agent-wire:prepare-stop (agent-wire:envelope.body-case (agent-protocol-receive client))))
     (agent-protocol-ready-to-stop client)
     (assert (eq ':stopped (autolith-supervisor::parent-connection-phase client)))))
  nil)

(-> agent-protocol-test--expect (keyword function) null)
(defun agent-protocol-test--expect (reason function)
  "Require a typed local or shared transport error with REASON."
  (assert (eq reason
              (handler-case (progn (funcall function) nil)
                (agent-protocol-error (condition) (agent-protocol-error-reason condition))
                (autolith-supervisor::parent-protocol-error (condition)
                  (autolith-supervisor::parent-protocol-error-reason condition)))))
  nil)

(-> agent-protocol-test--fixture (list function) null)
(defun agent-protocol-test--fixture (envelopes function)
  "Feed serialized parent envelopes from bounded files and inspect the client."
  (uiop:with-temporary-file (:stream input :element-type '(unsigned-byte 8))
    (uiop:with-temporary-file (:stream output :element-type '(unsigned-byte 8))
      (dolist (envelope envelopes)
        (autolith-supervisor::protocol-write-frame
         input (if (typep envelope '(array (unsigned-byte 8) (*)))
                   envelope
                   (cl-protobufs:serialize-to-bytes envelope))))
      (file-position input 0)
      (let ((client (agent-protocol-open input output "client-instance")))
        (unwind-protect (funcall function client)
          (agent-protocol-close client)))))
  nil)

(-> agent-protocol-test--envelope (integer &rest t) agent-wire:envelope)
(defun agent-protocol-test--envelope (id &rest arguments)
  "Construct valid parent metadata around test body arguments."
  (apply #'agent-wire:make-envelope :protocol-major 1 :instance-id "client-instance"
                                   :message-id id arguments))

(-> agent-protocol-test--welcome-errors () null)
(defun agent-protocol-test--welcome-errors ()
  "Reject invalid negotiation, wrong handshake correlation, and initialization errors."
  (dolist (mutation (list (lambda (welcome) (setf (agent-wire:welcome.features welcome) '("model.v1")))
                          (lambda (welcome) (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1")))
                          (lambda (welcome) (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) 0))
                          (lambda (welcome) (setf (agent-wire:limits.max-in-flight-requests (agent-wire:welcome.limits welcome)) 65))))
    (let ((welcome (agent-protocol-test--welcome)))
      (funcall mutation welcome)
      (agent-protocol-test--fixture
       (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome welcome))
       (lambda (client)
         (agent-protocol-test--expect ':invalid-welcome (lambda () (agent-protocol-handshake client)))
         (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))))))
  (agent-protocol-test--fixture
   (list (agent-protocol-test--envelope 1 :reply-to 2 :welcome (agent-protocol-test--welcome)))
   (lambda (client)
     (agent-protocol-test--expect ':invalid-welcome (lambda () (agent-protocol-handshake client)))))
  (agent-protocol-test--fixture
   (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome)))
   (lambda (client)
     (agent-protocol-test--expect ':initialization-test
                                 (lambda () (agent-protocol-handshake
                                             client :initialize (lambda (welcome)
                                                                  (declare (ignore welcome))
                                                                  (error 'agent-protocol-error :reason ':initialization-test)))))
     (assert (= 1 (autolith-supervisor::parent-connection-sent client)))))
  nil)

(-> agent-protocol-test--response-errors () null)
(defun agent-protocol-test--response-errors ()
  "Check replayed/unknown replies, reply type, ack identity, and wire metadata."
  (dolist (case '(:unknown-request :wrong-type :wrong-event :wrong-major :wrong-instance :wrong-id))
    (let ((reply (agent-protocol-test--envelope 2 :reply-to 3
                                               :session-ack (agent-wire:make-session-ack :event-id "event" :sequence 1))))
      (case case
        (:unknown-request
         (setf (agent-wire:envelope.reply-to reply) 9))
        (:wrong-type
         (setf (agent-wire:envelope.session-page reply) (agent-wire:make-session-page)))
        (:wrong-event
         (setf (agent-wire:session-ack.event-id (agent-wire:envelope.session-ack reply)) "other"))
        (:wrong-major
         (setf (agent-wire:envelope.protocol-major reply) 2))
        (:wrong-instance
         (setf (agent-wire:envelope.instance-id reply) "old-instance"))
        (:wrong-id
         (setf (agent-wire:envelope.message-id reply) 3)))
      (agent-protocol-test--fixture
       (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome)) reply)
       (lambda (client)
         (agent-protocol-handshake client)
         (agent-protocol--request client (agent-wire:make-envelope :session-append (agent-wire:make-session-append))
                                  (list 'agent-wire:session-ack "event"))
         (agent-protocol-test--expect
          (case case
            ((:unknown-request :wrong-type)
             ':unexpected-response)
            (:wrong-event
             ':invalid-session-ack)
            (:wrong-major
             ':unsupported-version)
            (:wrong-instance
             ':instance-mismatch)
            (:wrong-id
             ':message-id-discontinuity))
          (lambda () (agent-protocol-receive client)))
         (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))))))
  nil)

(-> agent-protocol-test--limits () null)
(defun agent-protocol-test--limits ()
  "Require bounded pending requests, one reader, and lifecycle completion barriers."
  (agent-protocol-test--fixture
   (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome)))
   (lambda (client)
     (agent-protocol-handshake client)
     (dotimes (index 4) (agent-protocol-session-read client :after-sequence index))
     (agent-protocol-test--expect ':request-limit (lambda () (agent-protocol-session-read client)))
     (assert (= 4 (hash-table-count (agent-protocol-client-pending client))))
     (agent-protocol-test--expect ':invalid-turn-finish (lambda () (agent-protocol-finish-turn client ':completed)))
     (agent-protocol-test--expect ':invalid-ready-to-stop (lambda () (agent-protocol-ready-to-stop client)))
     (agent-protocol--claim-reader client)
     (unwind-protect
          (agent-protocol-test--expect ':concurrent-reader (lambda () (agent-protocol-receive client)))
       (agent-protocol--release-reader client))))
  nil)

(-> agent-protocol-test--concurrent-output () null)
(defun agent-protocol-test--concurrent-output ()
  "Check that concurrent output writes preserve complete frames and consecutive IDs."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (dotimes (index 80)
       (let ((envelope (autolith-supervisor::parent-connection-receive parent)))
         (assert (= (+ index 3) (agent-wire:envelope.message-id envelope)))
         (assert (eq 'agent-wire:output (agent-wire:envelope.body-case envelope))))))
   (lambda (client)
     (agent-protocol-handshake client)
     (let ((threads (loop repeat 4 collect
                         (bordeaux-threads:make-thread
                          (lambda () (dotimes (index 20)
                                       (agent-protocol-output client ':diagnostic (format nil "output ~d" index))))))))
       (mapc #'bordeaux-threads:join-thread threads))))
  nil)

(-> agent-protocol-test--lower-welcome-limits () null)
(defun agent-protocol-test--lower-welcome-limits ()
  "Charge original unknown and overwritten Welcome fields against negotiated limits."
  (dolist (field '((#xf8 #x07 #x01) (#x08 #x01)))
    (let* ((envelope (agent-protocol-test--envelope
                      1 :reply-to 1 :welcome (agent-protocol-test--welcome)))
           (bytes    (concatenate '(simple-array (unsigned-byte 8) (*))
                                  (cl-protobufs:serialize-to-bytes envelope)
                                  (loop repeat 600 append field))))
      (agent-protocol-test--fixture
       (list bytes)
       (lambda (client)
         (assert (eq ':field-limit
                     (handler-case (progn (agent-protocol-handshake client) nil)
                       (autolith-supervisor::protocol-decode-error (condition)
                         (autolith-supervisor::protocol-decode-error-reason condition)))))
         (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))
         (assert (= 1 (autolith-supervisor::parent-connection-sent client)))))))
  nil)

(-> agent-protocol-test--terminal-replies () null)
(defun agent-protocol-test--terminal-replies ()
  "Retire Failure once, preserve observed sequence, and reject duplicate replies."
  (let ((failure (agent-protocol-test--envelope
                  3 :reply-to 4 :failure (agent-wire:make-failure :code ':resource-limit))))
    (agent-protocol-test--fixture
     (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome))
           (agent-protocol-test--envelope 2 :reply-to 3 :session-page (agent-wire:make-session-page))
           failure
           (agent-protocol-test--envelope 4 :reply-to 4 :failure (agent-wire:make-failure :code ':resource-limit)))
     (lambda (client)
       (agent-protocol-handshake client)
       (agent-protocol-session-read client :after-sequence 100)
       (agent-protocol-receive client)
       (assert (zerop (agent-protocol-client-last-sequence client)))
       (agent-protocol-session-read client)
       (assert (eq 'agent-wire:failure (agent-wire:envelope.body-case (agent-protocol-receive client))))
       (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
       (agent-protocol-test--expect ':unexpected-response (lambda () (agent-protocol-receive client))))))
  nil)

(-> agent-protocol-test--invalid-pages () null)
(defun agent-protocol-test--invalid-pages ()
  "Reject impossible pagination, unordered records, and forged record metadata."
  (dolist (page (list (agent-wire:make-session-page :has-more t :high-water-sequence 2)
                     (agent-wire:make-session-page
                      :high-water-sequence 1
                      :records             (list (agent-wire:make-session-record
                                                  :sequence 1 :event-id "input" :turn-id "turn"
                                                  :kind ':user-input :origin ':agent
                                                  :payload (autolith-supervisor::protocol-data->value "input"))))))
    (agent-protocol-test--fixture
     (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome))
           (agent-protocol-test--envelope 2 :reply-to 3 :session-page page))
     (lambda (client)
       (agent-protocol-handshake client)
       (agent-protocol-session-read client)
       (agent-protocol-test--expect ':invalid-session-page (lambda () (agent-protocol-receive client))))))
  nil)

(-> agent-protocol-test--turnless-notes () null)
(defun agent-protocol-test--turnless-notes ()
  "Accept parent policy notes without a turn and reject turnless agent records."
  (dolist (case '((:parent :note t) (:agent :note nil)
                  (:parent :user-input nil) (:parent :turn-outcome nil)))
    (destructuring-bind (origin kind accepted-p) case
      (let ((welcome (agent-protocol-test--welcome)))
        (setf (agent-wire:welcome.resume welcome)
              (agent-wire:make-session-page
               :high-water-sequence 1
               :records             (list (agent-wire:make-session-record
                                           :sequence 1 :event-id "policy-event" :origin origin :kind kind
                                           :payload (autolith-supervisor::protocol-data->value "policy changed")))))
        (agent-protocol-test--fixture
         (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome welcome))
         (lambda (client)
           (if accepted-p
               (progn
                 (agent-protocol-handshake client)
                 (assert (= 1 (agent-protocol-client-last-sequence client))))
               (agent-protocol-test--expect ':invalid-session-page
                                            (lambda () (agent-protocol-handshake client)))))))))
  nil)

(-> agent-protocol-test--lifecycle-errors () null)
(defun agent-protocol-test--lifecycle-errors ()
  "Reject unselected and wrong-direction operations; ignore inactive cancellation."
  (agent-protocol-test--fixture
   (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome))
         (agent-protocol-test--envelope 2 :model-start (agent-wire:make-model-start))
         (agent-protocol-test--envelope 3 :turn-cancel (agent-wire:make-turn-cancel :turn-id "inactive"))
         (agent-protocol-test--envelope 4 :turn-start (agent-wire:make-turn-start))
         (agent-protocol-test--envelope 5 :turn-start (agent-wire:make-turn-start :turn-id "turn" :input-sequence 1))
         (agent-protocol-test--envelope 6 :turn-start (agent-wire:make-turn-start :turn-id "second" :input-sequence 2)))
   (lambda (client)
     (agent-protocol-handshake client)
     (assert (null (agent-protocol-receive client)))
     (assert (null (agent-protocol-receive client)))
     (assert (null (agent-protocol-receive client)))
     (assert (null (agent-protocol-client-turn client)))
     (assert (agent-protocol-receive client))
     (assert (null (agent-protocol-receive client)))
     (assert (string= "turn" (agent-wire:turn-start.turn-id
                              (agent-wire:envelope.turn-start (agent-protocol-client-turn client)))))
     (assert (eq ':ready (autolith-supervisor::parent-connection-phase client)))
     (agent-protocol-finish-turn client ':failed)))
  nil)

(-> agent-protocol-test--deadline () null)
(defun agent-protocol-test--deadline ()
  "Apply the handshake deadline to initialization, before Ready can be sent."
  (agent-protocol-test--fixture
   (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome)))
   (lambda (client)
     (agent-protocol-test--expect
      ':handshake-timeout
      (lambda () (agent-protocol-handshake client :timeout 0.01
                                           :initialize     (lambda (welcome)
                                                             (declare (ignore welcome))
                                                             (sleep 1)))))
     (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))
     (assert (= 1 (autolith-supervisor::parent-connection-sent client)))))
  nil)

(-> agent-protocol-test--session-runtime () null)
(defun agent-protocol-test--session-runtime ()
  "Exercise the real parent session store and dispatcher over the client channel."
  (uiop:with-temporary-file (:pathname path :stream file)
    (close file)
    (delete-file path)
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (parent)
            (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
            (let* ((store   (autolith-supervisor::session-store-open path "client-session"))
                   (display nil)
                   (runtime (autolith-supervisor::session-runtime-open
                             store parent :output-callback (lambda (channel text)
                                                             (push (list channel text) display)))))
              (autolith-supervisor::session-runtime-start-turn runtime "real-turn" "parent input")
              (autolith-supervisor::session-store-model-result
               store :event-id "model-result/client" :turn-id "real-turn" :request-id 42
               :complete (agent-wire:make-model-complete :reason ':stop :response-id "response"))
              ;; Read the user input, append a note, then repeat the identical event.
              (dotimes (index 3) (autolith-supervisor::session-runtime-receive runtime))
              (autolith-supervisor::session-runtime-receive runtime)
              (assert (eq ':progress (first (first display))))
              (autolith-supervisor::session-runtime-cancel-turn runtime "real-turn")
              (autolith-supervisor::session-runtime-receive runtime)
              (assert (null (autolith-supervisor::session-runtime-turn-id runtime)))
              (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 5000)
              (autolith-supervisor::session-runtime-receive runtime)
              (assert (eq ':stopped (autolith-supervisor::session-runtime-phase runtime)))
              (let* ((reopened (autolith-supervisor::session-store-open path "client-session"))
                     (page     (autolith-supervisor::session-store-read
                                reopened (agent-wire:make-session-read :limit 256)))
                     (records  (agent-wire:session-page.records page)))
                 (assert (= 4 (length records)))
                 (assert (equal '(:user-input :note :note :turn-outcome)
                                (mapcar #'agent-wire:session-record.kind records)))
                 (assert (equal '(:parent :parent :agent :parent)
                                (mapcar #'agent-wire:session-record.origin records))))))
          (lambda (client)
            (agent-protocol-handshake client)
            (assert (eq 'agent-wire:turn-start (agent-wire:envelope.body-case (agent-protocol-receive client))))
            (agent-protocol-session-read client)
            (let* ((page   (agent-wire:envelope.session-page (agent-protocol-receive client)))
                   (record (first (agent-wire:session-page.records page))))
              (assert (eq ':parent (agent-wire:session-record.origin record)))
              (assert (string= "parent input"
                               (gethash "text" (autolith-supervisor::protocol-value->data
                                                 (agent-wire:session-record.payload record)))))
              (let* ((note    (second (agent-wire:session-page.records page)))
                     (payload (autolith-supervisor::protocol-value->data
                               (agent-wire:session-record.payload note))))
                (assert (eq ':parent (agent-wire:session-record.origin note)))
                (assert (string= "response" (gethash "response_id" (gethash "model_result" payload))))))
            (let* ((payload (make-hash-table :test 'equal))
                   (record  (progn
                              (setf (gethash "text" payload) "durable note")
                              (agent-wire:make-session-record
                               :event-id "real-note" :turn-id "real-turn" :kind ':note
                               :payload (autolith-supervisor::protocol-data->value payload)))))
              (dotimes (index 2)
                (agent-protocol-session-append client record)
                (let ((ack (agent-wire:envelope.session-ack (agent-protocol-receive client))))
                  (assert (string= "real-note" (agent-wire:session-ack.event-id ack)))
                  (assert (= 3 (agent-wire:session-ack.sequence ack))))))
            (agent-protocol-output client ':progress "client progress")
            (assert (eq 'agent-wire:turn-cancel (agent-wire:envelope.body-case (agent-protocol-receive client))))
            (agent-protocol-finish-turn client ':cancelled)
            (assert (eq 'agent-wire:prepare-stop (agent-wire:envelope.body-case (agent-protocol-receive client))))
            (agent-protocol-ready-to-stop client :wrap-up-event-id "real-note")))
      (when (probe-file path) (delete-file path))
      (let ((lock (pathname (concatenate 'string (namestring path) ".lock"))))
        (when (probe-file lock) (delete-file lock)))))
  nil)

(defclass agent-protocol-test-blocked-pipe (agent-protocol-test-pipe)
  ((blocked :initform t :accessor agent-protocol-test-pipe-blocked
            :documentation "Hold writes until the test closes or releases the stream.")
   (entered :initform nil :accessor agent-protocol-test-pipe-entered
            :documentation "Record that a writer reached controlled backpressure."))
  (:documentation "A deterministic output backpressure fixture."))

(defmethod trivial-gray-streams:stream-write-byte ((pipe agent-protocol-test-blocked-pipe) byte)
  "Stop the writer without holding the test mutex while waiting."
  (bordeaux-threads:with-lock-held ((agent-protocol-test-pipe-lock pipe))
    (setf (agent-protocol-test-pipe-entered pipe) t)
    (bordeaux-threads:condition-notify (agent-protocol-test-pipe-ready pipe))
    (loop while (and (agent-protocol-test-pipe-blocked pipe) (open-stream-p pipe))
          do (bordeaux-threads:condition-wait (agent-protocol-test-pipe-ready pipe)
                                            (agent-protocol-test-pipe-lock pipe)))
    (unless (open-stream-p pipe) (error 'agent-protocol-error :reason ':test-stream-closed)))
  (call-next-method))

(-> agent-protocol-test--wait-writer (agent-protocol-test-blocked-pipe) null)
(defun agent-protocol-test--wait-writer (pipe)
  "Wait for controlled backpressure, with a deadline for a failed test."
  (sb-ext:with-timeout 1
    (bordeaux-threads:with-lock-held ((agent-protocol-test-pipe-lock pipe))
      (loop until (agent-protocol-test-pipe-entered pipe)
            do (bordeaux-threads:condition-wait (agent-protocol-test-pipe-ready pipe)
                                              (agent-protocol-test-pipe-lock pipe)))))
  nil)

(-> agent-protocol-test--backpressure () null)
(defun agent-protocol-test--backpressure ()
  "Process lifecycle messages during blocked writes; bound exclusion, close, and expiry."
  (dolist (case '(:request :rejection :timeout))
    (let* ((input   (make-instance 'agent-protocol-test-pipe))
           (output  (make-instance 'agent-protocol-test-blocked-pipe))
           (client  (agent-protocol-open input output "client-instance"))
           (parent  (make-instance 'autolith-supervisor::parent-connection
                                   :input output :output input :instance-id "client-instance"))
           (failure nil)
           (thread  nil))
      (setf (autolith-supervisor::parent-connection-phase client) ':ready
            (agent-protocol-client-turn client)
            (agent-protocol-test--envelope 1 :turn-start
                                           (agent-wire:make-turn-start :turn-id "turn" :input-sequence 1)))
      (when (eq case ':rejection)
        (autolith-supervisor::parent-connection-send
         parent (agent-wire:make-envelope :turn-start (agent-wire:make-turn-start))))
      (unwind-protect
           (progn
             (setf thread
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((*agent-protocol-write-timeout* (if (eq case ':timeout) 0.05 2)))
                        (handler-case
                            (if (eq case ':rejection)
                                (agent-protocol-receive client)
                                (agent-protocol-session-read client))
                          (error (condition) (setf failure condition)))))
                    :name "blocked client writer"))
             (agent-protocol-test--wait-writer output)
             (unless (eq case ':timeout)
               (sb-ext:with-timeout 0.5
                 (if (eq case ':rejection)
                     (agent-protocol-test--expect ':concurrent-reader
                                                  (lambda () (agent-protocol-receive client)))
                     (progn
                       (autolith-supervisor::parent-connection-send
                        parent (agent-wire:make-envelope :turn-cancel
                                                         (agent-wire:make-turn-cancel :turn-id "turn")))
                       (assert (agent-wire:envelope.turn-cancel (agent-protocol-receive client)))
                       (autolith-supervisor::parent-connection-send
                        parent (agent-wire:make-envelope :prepare-stop (agent-wire:make-prepare-stop)))
                       (assert (agent-wire:envelope.prepare-stop (agent-protocol-receive client)))
                       (agent-protocol--claim-reader client)
                       (unwind-protect
                            (agent-protocol-test--expect ':concurrent-reader
                                                         (lambda () (agent-protocol-receive client)))
                         (agent-protocol--release-reader client))))
                 (agent-protocol-close client)))
             (sb-ext:with-timeout 1 (bordeaux-threads:join-thread thread))
             (assert failure)
             (when (eq case ':timeout)
               (assert (typep failure 'agent-protocol-error))
               (assert (eq ':write-timeout (agent-protocol-error-reason failure))))
             (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))
             (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
             (assert (null (agent-protocol-client-reading client))))
        (agent-protocol-close client)
        (when thread (sb-ext:with-timeout 3 (bordeaux-threads:join-thread thread))))))
  nil)

(-> agent-protocol-test--record (string integer) agent-wire:session-record)
(defun agent-protocol-test--record (event sequence)
  "Build a valid durable note for an assignment test."
  (agent-wire:make-session-record :event-id event :sequence sequence :turn-id "turn"
                                   :origin ':agent :kind ':note
                                   :payload (autolith-supervisor::protocol-data->value "note")))

(-> agent-protocol-test--assignments () null)
(defun agent-protocol-test--assignments ()
  "Reject correlated identity conflicts from acks, pages, and resumed history."
  (dolist (case '(:ack-ack :same-event :page-ack :ack-page :welcome-ack :page-page))
    (let* ((welcome       (agent-protocol-test--welcome))
           (page          (agent-wire:make-session-page
                           :high-water-sequence 100 :records (list (agent-protocol-test--record "A" 1))))
           (first-page-p  (member case '(:page-ack :page-page)))
           (last-page-p   (member case '(:ack-page :page-page)))
           (resumed-p     (eq case ':welcome-ack))
           (next-event    (if (eq case ':same-event) "A" "B"))
           (next-sequence (if (eq case ':same-event) 2 1)))
      (when resumed-p (setf (agent-wire:welcome.resume welcome) page))
      (agent-protocol-test--fixture
       (append
        (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome welcome))
        (unless resumed-p
          (list (if first-page-p
                    (agent-protocol-test--envelope 2 :reply-to 3 :session-page page)
                    (agent-protocol-test--envelope 2 :reply-to 3 :session-ack
                                                   (agent-wire:make-session-ack :event-id "A" :sequence 1)))))
        (list (if last-page-p
                  (agent-protocol-test--envelope 3 :reply-to 4 :session-page
                                                 (agent-wire:make-session-page
                                                  :high-water-sequence 100
                                                  :records             (list (agent-protocol-test--record "B" 1))))
                  (agent-protocol-test--envelope
                   (if resumed-p 2 3) :reply-to (if resumed-p 3 4) :session-ack
                   (agent-wire:make-session-ack :event-id next-event :sequence next-sequence)))))
       (lambda (client)
         (agent-protocol-handshake client)
         (unless resumed-p
           (if first-page-p
               (agent-protocol-session-read client)
               (agent-protocol--request client (agent-wire:make-envelope :session-append (agent-wire:make-session-append))
                                        (list 'agent-wire:session-ack "A")))
           (assert (agent-protocol-receive client))
           (assert (zerop (hash-table-count (agent-protocol-client-pending client)))))
         (if last-page-p
             (agent-protocol-session-read client)
             (agent-protocol--request client (agent-wire:make-envelope :session-append (agent-wire:make-session-append))
                                      (list 'agent-wire:session-ack next-event)))
         (agent-protocol-test--expect ':conflicting-session-assignment
                                      (lambda () (agent-protocol-receive client)))
         (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))
         (assert (zerop (hash-table-count (agent-protocol-client-pending client))))))))
  nil)

(-> agent-protocol-test--assignment-bound () null)
(defun agent-protocol-test--assignment-bound ()
  "Keep old retries valid at the bound and fail closed instead of evicting evidence."
  (dolist (bound '(:entries :characters))
    (let ((*agent-protocol-assignment-limit*           (if (eq bound ':entries) 2 65536))
          (*agent-protocol-assignment-character-limit* (if (eq bound ':characters) 2 1048576)))
     (agent-protocol-test--fixture
      (cons (agent-protocol-test--envelope 1 :reply-to 1 :welcome (agent-protocol-test--welcome))
            (loop for event in '("A" "B" "A" "C")
                  for sequence in '(1 100 1 101)
                  for id from 2
                  collect (agent-protocol-test--envelope
                           id :reply-to (1+ id) :session-ack
                           (agent-wire:make-session-ack :event-id event :sequence sequence))))
      (lambda (client)
        (agent-protocol-handshake client)
        (dolist (event '("A" "B" "A" "C"))
          (agent-protocol--request client (agent-wire:make-envelope :session-append (agent-wire:make-session-append))
                                   (list 'agent-wire:session-ack event))
          (if (string= event "C")
              (agent-protocol-test--expect ':assignment-limit (lambda () (agent-protocol-receive client)))
              (progn (assert (agent-protocol-receive client))
                     (assert (zerop (hash-table-count (agent-protocol-client-pending client)))))))
        (assert (= 100 (agent-protocol-client-last-sequence client)))
        (assert (= 2 (hash-table-count (agent-protocol-client-assignments client))))
        (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))
        (assert (zerop (hash-table-count (agent-protocol-client-pending client))))))))
  nil)

(-> agent-protocol-test--resume-retry () null)
(defun agent-protocol-test--resume-retry ()
  "Accept old retries after resumed and overlapping pages, with copied identity evidence."
  (let* ((welcome (agent-protocol-test--welcome))
         (a       (agent-protocol-test--record "A" 1))
         (b       (agent-protocol-test--record "B" 100)))
    (setf (agent-wire:welcome.resume welcome)
          (agent-wire:make-session-page :high-water-sequence 100 :has-more t :records (list a)))
    (agent-protocol-test--fixture
     (list (agent-protocol-test--envelope 1 :reply-to 1 :welcome welcome)
           (agent-protocol-test--envelope 2 :reply-to 3 :session-page
                                          (agent-wire:make-session-page :high-water-sequence 100 :records (list b)))
           (agent-protocol-test--envelope 3 :reply-to 4 :session-page
                                          (agent-wire:make-session-page :high-water-sequence 100 :records (list a b)))
           (agent-protocol-test--envelope 4 :reply-to 5 :session-ack
                                          (agent-wire:make-session-ack :event-id "A" :sequence 1)))
     (lambda (client)
       (let ((accepted (agent-protocol-handshake client)))
         (setf (char (agent-wire:session-record.event-id
                      (first (agent-wire:session-page.records (agent-wire:welcome.resume accepted)))) 0) #\X))
       (agent-protocol-session-read client :after-sequence 1)
       (assert (agent-protocol-receive client))
       (agent-protocol-session-read client)
       (assert (agent-protocol-receive client))
       (agent-protocol--request client (agent-wire:make-envelope :session-append (agent-wire:make-session-append))
                                (list 'agent-wire:session-ack "A"))
       (assert (agent-protocol-receive client))
       (assert (= 2 (hash-table-count (agent-protocol-client-assignments client))))
       (assert (= 100 (agent-protocol-client-last-sequence client)))
       (assert (zerop (hash-table-count (agent-protocol-client-pending client)))))))
  nil)

(-> agent-protocol-test--concurrent-requests () null)
(defun agent-protocol-test--concurrent-requests ()
  "Match replies while independent writers register and send session requests."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (dotimes (index 4)
       (let ((request (autolith-supervisor::parent-connection-receive parent)))
         (assert (= (+ index 3) (agent-wire:envelope.message-id request)))
         (autolith-supervisor::parent-connection-send
          parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                           :session-page (agent-wire:make-session-page))))))
   (lambda (client)
     (agent-protocol-handshake client)
     (let* ((results (make-array 4))
            (threads (loop for index below 4
                           collect (let ((slot index))
                                     (bordeaux-threads:make-thread
                                      (lambda ()
                                        (setf (aref results slot)
                                              (handler-case (agent-protocol-session-read client)
                                                (error (condition) condition))))
                                      :name "session request writer")))))
       (dotimes (index 4) (assert (agent-protocol-receive client)))
       (dolist (thread threads) (bordeaux-threads:join-thread thread))
       (assert (equal '(3 4 5 6) (sort (coerce results 'list) #'<)))
       (assert (zerop (hash-table-count (agent-protocol-client-pending client)))))))
  nil)

(defclass agent-protocol-test-ready-close-pipe (agent-protocol-test-pipe)
  ((on-ready :initarg :on-ready :reader agent-protocol-test-pipe-on-ready
             :documentation "Close the client when its Ready frame reaches the output.")
   (frames :initform 0 :accessor agent-protocol-test-pipe-frames
           :documentation "Count completed frame writes."))
  (:documentation "Close at the boundary between Ready I/O and readiness publication."))

(defmethod trivial-gray-streams:stream-finish-output :after ((pipe agent-protocol-test-ready-close-pipe))
  "Run the close operation after the second handshake frame."
  (when (= 2 (incf (agent-protocol-test-pipe-frames pipe)))
    (funcall (agent-protocol-test-pipe-on-ready pipe))))

(-> agent-protocol-test--handshake-close () null)
(defun agent-protocol-test--handshake-close ()
  "Do not publish readiness after concurrent close during the Ready write."
  (let* ((input  (make-instance 'agent-protocol-test-pipe))
         (client nil)
         (output (make-instance 'agent-protocol-test-ready-close-pipe
                                :on-ready (lambda ()
                                            (bordeaux-threads:join-thread
                                             (bordeaux-threads:make-thread
                                              (lambda () (agent-protocol-close client))
                                              :name "close during Ready"))))))
    (setf client (agent-protocol-open input output "client-instance"))
    (unwind-protect
         (progn
           (autolith-supervisor::protocol-write-frame
            input (cl-protobufs:serialize-to-bytes
                   (agent-protocol-test--envelope 1 :reply-to 1
                                                  :welcome    (agent-protocol-test--welcome))))
           (sb-ext:with-timeout 2
             (agent-protocol-test--expect ':closed (lambda () (agent-protocol-handshake client))))
           (assert (eq ':closed (autolith-supervisor::parent-connection-phase client))))
      (agent-protocol-close client)))
  nil)

(-> agent-protocol-test--peer-cleanup () null)
(defun agent-protocol-test--peer-cleanup ()
  "Join the blocked peer before returning a client-side fixture failure."
  (let ((finished nil))
    (agent-protocol-test--expect
     ':test-client-failure
     (lambda ()
       (agent-protocol-test--peers
        (lambda (parent)
          (unwind-protect
               (autolith-supervisor::parent-connection-receive parent)
            (setf finished t)))
        (lambda (client)
          (declare (ignore client))
          (error 'agent-protocol-error :reason ':test-client-failure)))))
    (assert finished))
  nil)

(-> run-agent-protocol-tests () null)
(defun run-agent-protocol-tests ()
  "Run optional protocol client checks over actual serialized parent channels."
  (agent-protocol-test--session)
  (agent-protocol-test--welcome-errors)
  (agent-protocol-test--response-errors)
  (agent-protocol-test--lower-welcome-limits)
  (agent-protocol-test--limits)
  (agent-protocol-test--terminal-replies)
  (agent-protocol-test--invalid-pages)
  (agent-protocol-test--turnless-notes)
  (agent-protocol-test--lifecycle-errors)
  (agent-protocol-test--deadline)
  (agent-protocol-test--session-runtime)
  (agent-protocol-test--concurrent-output)
  (agent-protocol-test--backpressure)
  (agent-protocol-test--assignments)
  (agent-protocol-test--assignment-bound)
  (agent-protocol-test--resume-retry)
  (agent-protocol-test--concurrent-requests)
  (agent-protocol-test--handshake-close)
  (agent-protocol-test--peer-cleanup)
  nil)
