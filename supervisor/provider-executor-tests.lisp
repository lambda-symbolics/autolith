;;;; -- Standalone Parent HTTP Executor Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () cl-rfc8628:oauth-credentials) provider-executor-test--credentials))

(defun provider-executor-test--credentials ()
  "Create synthetic credential material without reading a private account."
  (make-instance 'cl-rfc8628:oauth-credentials
                 :access-token  (format nil "fixture-~36R" (random (expt 36 30) (make-random-state t)))
                 :refresh-token (format nil "fixture-~36R" (random (expt 36 30) (make-random-state t)))
                 :account-id "fixture-account" :expires-at (+ (get-universal-time) 3600)
                 :source-path   #P"fixture"))

(defclass provider-executor-test-source (cl-rfc8628:credential-source)
  ((credentials :initform      (provider-executor-test--credentials)
                :accessor      provider-executor-test-source-credentials
                :documentation "Synthetic in-memory credential rotation."))
  (:documentation "No-file OAuth storage for the real library manager engine."))

(defmethod cl-rfc8628:credential-source-load ((source provider-executor-test-source))
  "Return the synthetic primary rotation."
  (provider-executor-test-source-credentials source))

(defmethod cl-rfc8628:credential-source-save
    ((source provider-executor-test-source) (credentials cl-rfc8628:oauth-credentials))
  "Publish a synthetic refresh in memory."
  (setf (provider-executor-test-source-credentials source) credentials))

(defclass provider-executor-test-manager (cl-rfc8628:managed-credential-manager)
  ((refreshes :initform nil :accessor provider-executor-test-manager-refreshes
              :documentation "Observed refresh exchanges.")
   (refreshable :initarg :refreshable :initform t :reader provider-executor-test-manager-refreshable
                :documentation "Whether the fixture supports token refresh."))
  (:default-initargs :primary-source (make-instance 'provider-executor-test-source))
  (:documentation "Synthetic credentials with the real account continuity and refresh engine."))

(defmethod cl-rfc8628:credential-manager-refresh-exchange
    ((manager provider-executor-test-manager) (credentials cl-rfc8628:oauth-credentials) refresh-token)
  "Observe one refresh and return a fresh synthetic rotation."
  (declare (ignore credentials refresh-token))
  (push t (provider-executor-test-manager-refreshes manager))
  (values (provider-executor-test--credentials) t))

(defmethod cl-rfc8628:credential-manager-refreshable-p ((manager provider-executor-test-manager))
  "Return the fixture's configured refresh capability."
  (provider-executor-test-manager-refreshable manager))

(defclass provider-executor-test-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((read-started :initform (sb-thread:make-semaphore) :reader provider-executor-test-stream-read-started
                 :documentation "Signals that the real stream reader is blocked.")
   (closed :initform (sb-thread:make-semaphore) :reader provider-executor-test-stream-closed
           :documentation "Close unblocks the reader."))
  (:documentation "A blocking character transport whose close releases its reader."))

(defmethod trivial-gray-streams:stream-read-char ((stream provider-executor-test-stream))
  "Wait until actual close instead of returning a fabricated completion."
  (sb-thread:signal-semaphore (provider-executor-test-stream-read-started stream))
  (unless (sb-thread:wait-on-semaphore (provider-executor-test-stream-closed stream) :timeout 5)
    (error 'provider-executor-error :reason ':test-reader-timeout))
  ':eof)

(defmethod close ((stream provider-executor-test-stream) &key abort)
  "Close the stream and unblock its actual read operation."
  (declare (ignore abort))
  (prog1 (call-next-method)
    (sb-thread:signal-semaphore (provider-executor-test-stream-closed stream))))

(declaim (ftype (function (sb-thread:semaphore) null) provider-executor-test--wait))

(defun provider-executor-test--wait (semaphore)
  "Wait for deterministic test ordering with a bounded failure."
  (unless (sb-thread:wait-on-semaphore semaphore :timeout 5)
    (error 'provider-executor-error :reason ':test-timeout))
  nil)

(declaim (ftype (function (bordeaux-threads:thread) t) provider-executor-test--join))

(defun provider-executor-test--join (thread)
  "Join a fixture worker under a hard test timeout."
  (let* ((timeout (list ':timeout))
         (result  (sb-thread:join-thread thread :timeout 6 :default timeout)))
    (when (eq result timeout) (error 'provider-executor-error :reason ':test-join-timeout))
    result))

(declaim (ftype (function () string) provider-executor-test--sse))

(defun provider-executor-test--sse ()
  "Return a real minimal Responses completion for the library SSE parser."
  (format nil "data: {\"type\":\"response.created\",\"response\":{\"id\":\"fixture-response\"}}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"fixture-response\",\"usage\":{\"input_tokens\":1}}}~%~%"))

(declaim (ftype (function (&key (:summary-p boolean) (:kind keyword))
                         (values model-profile model-invocation &optional)) provider-executor-test--invocation))

(defun provider-executor-test--invocation (&key summary-p (kind ':generate))
  "Build the same seven-value projection supplied by model-service."
  (let* ((profile (make-model-profile :id "fixture" :model "fixture-model" :family ':responses))
         (request (make-instance 'cl-llm-provider-api:wire-request :model "fixture-model"
                                :prefix '("stable instructions") :suffix '("request instructions"))))
    (values profile
            (make-instance 'model-invocation :inputs (list request #() profile kind summary-p nil "trusted-turn-state")
                           :wire-id 1 :request-id "fixture-request" :turn-id "fixture-turn"
                           :deadline                 (+ (session-runtime--clock) 30)))))

(declaim (ftype (function (model-profile provider-executor-test-manager) hash-table)
                provider-executor-test--accounts))

(defun provider-executor-test--accounts (profile manager)
  "Bind only the admitted private profile object to the trusted fixture account."
  (let ((accounts (make-hash-table :test #'eq)))
    (setf (gethash profile accounts)
          (make-chatgpt-account :credential-manager manager :endpoint "https://trusted.invalid/responses"))
    accounts))

(declaim (ftype (function (function) boolean) provider-executor-test--fails-p))

(defun provider-executor-test--fails-p (function)
  "Observe a failure without retaining or printing untrusted condition text."
  (handler-case (progn (funcall function) nil)
    (error () t)))

(declaim (ftype (function () null) provider-executor-test--ordinary))

(defun provider-executor-test--ordinary ()
  "Run ordinary and summary requests through the actual codec, parser, and strict cleanup."
  (dolist (summary-p '(nil t))
    (multiple-value-bind (profile invocation) (provider-executor-test--invocation :summary-p summary-p)
      (let* ((manager (make-instance 'provider-executor-test-manager))
             (stream nil) (opened 0) (closed 0) (events nil))
        (multiple-value-bind (execute abort)
            (make-chatgpt-model-executor
             :accounts (provider-executor-test--accounts profile manager) :session-id "trusted-session"
             :maximum-attempts 1
             :opener           (lambda (endpoint request credentials &key session-id turn-state user-agent deadline-seconds)
                                 (declare (ignore user-agent))
                                 (incf opened)
                                 (assert (string= endpoint "https://trusted.invalid/responses"))
                                 (assert (string= (gethash "model" request) "fixture-model"))
                                 (assert (string= (gethash "instructions" request) "stable instructions"))
                                 (assert (find "reasoning.encrypted_content" (gethash "include" request) :test #'equal))
                                 (assert (string= (gethash "prompt_cache_key" request) "trusted-session"))
                                 (assert (string= session-id "trusted-session"))
                                 (assert (string= turn-state "trusted-turn-state"))
                                 (assert (plusp deadline-seconds))
                                 (let ((headers (chatgpt-request-headers credentials :session-id session-id
                                                                        :turn-state turn-state :user-agent "fixture")))
                                   (assert (string= (rest (assoc "session-id" headers :test #'equal)) session-id))
                                   (assert (string= (rest (assoc "ChatGPT-Account-ID" headers :test #'equal)) "fixture-account")))
                                 (setf stream (make-string-input-stream (provider-executor-test--sse)))
                                 (values stream 200 nil))
             :closer           (lambda (stream) (incf closed) (close stream :abort t)))
          (assert (typep (funcall execute invocation (lambda (event) (push event events)))
                         'cl-llm-provider-api:provider-result))
          (assert (= opened 1 closed))
          (assert (not (open-stream-p stream)))
          (assert (find-if (lambda (event) (typep event 'cl-llm-provider-api:provider-completed-event)) events))
          (funcall abort invocation)
          (funcall abort invocation)
          (assert (= closed 1))))))
  nil)

(declaim (ftype (function () null) provider-executor-test--retry))

(defun provider-executor-test--retry ()
  "Count all HTTP opens, including a single authentication recovery, in one retry budget."
  (dolist (statuses '((503 401 200) (401 401) (503 503)))
    (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
      (let* ((manager (make-instance 'provider-executor-test-manager))
             (remaining (copy-list statuses)) (opened 0) (closed 0) (attempts nil))
        (multiple-value-bind (execute abort)
            (make-chatgpt-model-executor
             :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
             :maximum-attempts (length statuses)
             :delay-function   (lambda (number condition) (declare (ignore number condition)) 0)
             :opener           (lambda (endpoint request credentials &rest metadata)
                                 (declare (ignore endpoint request credentials metadata))
                                 (incf opened)
                                 (let ((status (pop remaining)))
                                   (values (make-string-input-stream (if (= status 200) (provider-executor-test--sse) "{}"))
                                           status nil)))
             :closer           (lambda (stream) (incf closed) (close stream :abort t)))
          (let ((failed
                  (provider-executor-test--fails-p
                   (lambda ()
                     (funcall execute invocation
                              (lambda (event)
                                (when (typep event 'cl-llm-provider-api:provider-retry-event)
                                  (assert (= (cl-llm-provider-api:provider-retry-event-maximum-attempts event)
                                             (length statuses)))
                                  (push (cl-llm-provider-api:provider-retry-event-attempt event) attempts))))))))
            (assert (eq failed (not (member 200 statuses)))))
          (assert (= opened closed (length statuses)))
          (assert (equal (reverse attempts) (loop for i from 1 to (length statuses) collect i)))
          (assert (equal (provider-executor-test-manager-refreshes manager)
                         (when (member 401 statuses) '(t))))
          (funcall abort invocation)))))
  nil)

(declaim (ftype (function () null) provider-executor-test--cleanup-failure))

(defun provider-executor-test--cleanup-failure ()
  "Reject swallowed close failures and false success from a closer that leaves the stream open."
  (dolist (signals-p '(nil t))
    (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
      (let* ((manager (make-instance 'provider-executor-test-manager))
             (stream (make-string-input-stream (provider-executor-test--sse))) (closed 0))
        (unwind-protect
             (multiple-value-bind (execute abort)
                 (make-chatgpt-model-executor
                  :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
                  :maximum-attempts 1
                  :opener           (lambda (&rest arguments) (declare (ignore arguments)) (values stream 200 nil))
                  :closer           (lambda (stream)
                                      (declare (ignore stream))
                                      (incf closed)
                                      (when signals-p (error 'provider-executor-error :reason ':test-close-failed))))
               (assert (provider-executor-test--fails-p (lambda () (funcall execute invocation (constantly nil)))))
               (assert (= closed 1))
               (assert (provider-executor-test--fails-p (lambda () (funcall abort invocation)))))
          (close stream :abort t)))))
  nil)

(declaim (ftype (function () null) provider-executor-test--early-abort))

(defun provider-executor-test--early-abort ()
  "Fence execution when abort precedes credential acquisition or HTTP open."
  (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
    (let* ((manager (make-instance 'provider-executor-test-manager)) (opened 0))
      (multiple-value-bind (execute abort)
          (make-chatgpt-model-executor
           :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
           :opener (lambda (&rest arguments) (declare (ignore arguments)) (incf opened)))
        (funcall abort invocation)
        (assert (provider-executor-test--fails-p (lambda () (funcall execute invocation (constantly nil)))))
        (assert (zerop opened))
        (assert (null (provider-executor-test-manager-refreshes manager))))))
  nil)

(declaim (ftype (function () null) provider-executor-test--opening-abort))

(defun provider-executor-test--opening-abort ()
  "Require abort to await a late open, then close that exact stream before confirming."
  (let ((state (make-instance 'provider-http-state))
        (entered (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
        (stream (make-string-input-stream "")) (closed 0) (aborted nil) (failed nil)
        (opening nil) (aborting nil))
    (unwind-protect
         (progn
           (setf opening
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (setf failed
                          (provider-executor-test--fails-p
                           (lambda ()
                             (provider-http--open
                              state (lambda ()
                                      (sb-thread:signal-semaphore entered)
                                      (provider-executor-test--wait release)
                                      (values stream 200 nil))
                              (lambda (stream) (incf closed) (close stream :abort t)))))))))
           (provider-executor-test--wait entered)
           (setf aborting
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (provider-http--abort state (lambda (stream) (incf closed) (close stream :abort t)))
                    (setf aborted t))))
           (bordeaux-threads:with-lock-held ((provider-http-state-lock state))
             (loop until (provider-http-state-cancelled state)
                   do (bordeaux-threads:condition-wait (provider-http-state-ready state)
                                                      (provider-http-state-lock state) :timeout 0.01))
             (assert (not aborted)))
           (sb-thread:signal-semaphore release)
           (provider-executor-test--join opening)
           (provider-executor-test--join aborting)
           (assert (and failed aborted (= closed 1) (not (open-stream-p stream)))))
      (sb-thread:signal-semaphore release)
      (when opening (provider-executor-test--join opening))
      (when aborting (provider-executor-test--join aborting))
      (close stream :abort t)))
  nil)

(declaim (ftype (function () null) provider-executor-test--reading-abort))

(defun provider-executor-test--reading-abort ()
  "Close the real transport during a blocked parser read and join the callback producer."
  (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
    (let* ((manager (make-instance 'provider-executor-test-manager))
           (stream (make-instance 'provider-executor-test-stream)) (thread nil) (failed nil) (closed 0))
      (multiple-value-bind (execute abort)
          (make-chatgpt-model-executor
           :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
           :opener (lambda (&rest arguments) (declare (ignore arguments)) (values stream 200 nil))
           :closer (lambda (stream) (incf closed) (close stream :abort t)))
        (unwind-protect
             (progn
               (setf thread (bordeaux-threads:make-thread
                             (lambda ()
                               (setf failed (provider-executor-test--fails-p
                                             (lambda () (funcall execute invocation (constantly nil))))))))
               (provider-executor-test--wait (provider-executor-test-stream-read-started stream))
               (funcall abort invocation)
               (provider-executor-test--join thread)
               (assert (and failed (= closed 1) (not (open-stream-p stream)))))
          (close stream :abort t)
          (when thread (provider-executor-test--join thread))))))
  nil)

(declaim (ftype (function () null) provider-executor-test--retry-abort))

(defun provider-executor-test--retry-abort ()
  "Cancel in the retry gap without waiting for the full provider backoff."
  (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
    (let* ((manager (make-instance 'provider-executor-test-manager))
           (retrying (sb-thread:make-semaphore)) (opened 0) (thread nil) (failed nil))
      (multiple-value-bind (execute abort)
          (make-chatgpt-model-executor
           :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
           :delay-function (lambda (number condition) (declare (ignore number condition)) 60)
           :opener         (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (incf opened) (values (make-string-input-stream "{}") 503 nil)))
        (unwind-protect
             (progn
               (setf thread
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (setf failed
                              (provider-executor-test--fails-p
                               (lambda ()
                                 (funcall execute invocation
                                          (lambda (event)
                                            (when (and (typep event 'cl-llm-provider-api:provider-retry-event)
                                                       (= 2 (cl-llm-provider-api:provider-retry-event-attempt event)))
                                              (sb-thread:signal-semaphore retrying))))))))))
               (provider-executor-test--wait retrying)
               (funcall abort invocation)
               (provider-executor-test--join thread)
               (assert (and failed (= opened 1))))
          (funcall abort invocation)
          (when thread (provider-executor-test--join thread))))))
  nil)

(declaim (ftype (function () null) provider-executor-test--admission))

(defun provider-executor-test--admission ()
  "Reject native compaction, an unbound profile, and an expired deadline before HTTP."
  (dolist (case '(:native :unbound :deadline))
    (multiple-value-bind (profile invocation)
        (provider-executor-test--invocation :kind (if (eq case ':native) ':native-compaction ':generate))
      (let* ((manager (make-instance 'provider-executor-test-manager))
             (accounts (provider-executor-test--accounts profile manager)) (opened 0))
        (when (eq case ':unbound) (clrhash accounts))
        (multiple-value-bind (execute abort)
            (make-chatgpt-model-executor
             :accounts accounts :session-id "fixture-session"
             :clock  (if (eq case ':deadline) (constantly most-positive-fixnum) #'session-runtime--clock)
             :opener (lambda (&rest arguments) (declare (ignore arguments)) (incf opened)))
          (assert (provider-executor-test--fails-p (lambda () (funcall execute invocation (constantly nil)))))
          (assert (zerop opened))
          (funcall abort invocation)))))
  nil)

(declaim (ftype (function () null) provider-executor-test--local-http))

(defun provider-executor-test--local-http ()
  "Use a real loopback HTTP connection and observe its close from the server."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                        :element-type                  '(unsigned-byte 8)))
         (server nil) (server-failed nil) (peer-closed nil))
    (unwind-protect
         (progn
           (setf server
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (setf server-failed
                          (provider-executor-test--fails-p
                           (lambda ()
                             (let ((socket (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
                               (unwind-protect
                                    (cl-llm-provider-api:provider-call-with-response-deadline
                                     5
                                     (lambda ()
                                       (let* ((raw    (usocket:socket-stream socket))
                                              (stream (flexi-streams:make-flexi-stream raw :external-format ':utf-8))
                                              (length 0) (authorized nil))
                                         (unless (search "POST /responses HTTP/" (read-line stream))
                                           (error 'provider-executor-error :reason ':test-request-target))
                                         (loop for line = (string-right-trim '(#\Return) (read-line stream))
                                               until (zerop (length line))
                                               do (when (uiop:string-prefix-p "Content-Length:" line)
                                                    (setf length (parse-integer line :start 15)))
                                                  (when (uiop:string-prefix-p "Authorization: Bearer " line)
                                                    (setf authorized t)))
                                         (assert authorized)
                                         (dotimes (i length) (read-char stream))
                                         (let* ((body  (provider-executor-test--sse))
                                                (reply (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C~X~C~C~A~C~C0~C~C~C~C"
                                                               #\Return #\Newline #\Return #\Newline
                                                               #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                                               (length body) #\Return #\Newline body
                                                               #\Return #\Newline #\Return #\Newline #\Return #\Newline)))
                                           (write-sequence (sb-ext:string-to-octets reply :external-format ':utf-8) raw)
                                           (finish-output raw))
                                         (setf peer-closed (null (read-byte raw nil nil))))))
                                 (usocket:socket-close socket)))))))))
           (multiple-value-bind (profile invocation) (provider-executor-test--invocation)
             (let* ((manager  (make-instance 'provider-executor-test-manager))
                    (accounts (make-hash-table :test #'eq)))
               (setf (gethash profile accounts)
                     (make-chatgpt-account
                      :credential-manager manager
                      :endpoint           (format nil "http://127.0.0.1:~D/responses" (usocket:get-local-port listener))))
               (multiple-value-bind (execute abort)
                   (make-chatgpt-model-executor :accounts accounts :session-id "fixture-session" :maximum-attempts 1)
                 (assert (typep (funcall execute invocation (constantly nil)) 'cl-llm-provider-api:provider-result))
                 (funcall abort invocation))))
           (provider-executor-test--join server)
           (assert (and (not server-failed) peer-closed)))
      (usocket:socket-close listener)
      (when server (provider-executor-test--join server))))
  nil)

(declaim (ftype (function () null) provider-executor-test--service))

(defun provider-executor-test--service ()
  "Accept a retried HTTP result through the real model service, collector, and journals."
  (let ((execute nil) (abort nil) (opened 0) (closed 0))
    (model-service-test--fixture
     (lambda (&key runtime service receive responses clock)
       (declare (ignore runtime))
       (let* ((profile (gethash "profile" (model-service-profiles service)))
              (manager (make-instance 'provider-executor-test-manager)))
         (multiple-value-setq (execute abort)
           (make-chatgpt-model-executor
            :accounts (provider-executor-test--accounts profile manager) :session-id "fixture-session"
            :maximum-attempts 2 :clock (lambda () (first clock))
            :delay-function (lambda (number condition) (declare (ignore number condition)) 0)
            :opener         (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (incf opened)
                              (values (make-string-input-stream (if (= opened 1) "{}" (provider-executor-test--sse)))
                                      (if (= opened 1) 503 200) nil))
            :closer         (lambda (stream) (incf closed) (close stream :abort t))))
         (funcall receive (model-service-test--start))
         (model-runtime--call (model-service-mailbox service) (constantly nil))
         (model-service-test--settle service)
         (let* ((frames   (funcall responses))
                (complete (find-if (lambda (frame)
                                     (let ((event (parent-proto:envelope.model-event frame)))
                                       (and event (parent-proto:model-event.has-complete event)))) frames)))
           (assert complete)
           (assert (= (parent-proto:envelope.reply-to complete) 1))
           (assert (= opened closed 2)))))
     :maximum-attempts 2
     :executor         (lambda (invocation callback) (funcall execute invocation callback))
     :abort            (lambda (invocation) (when abort (funcall abort invocation)))))
  nil)

(declaim (ftype (function () integer) run-provider-executor-tests))

(defun run-provider-executor-tests ()
  "Run HTTP ownership, real wire execution, retry budget, and cancellation checks without agent code."
  (mapc #'funcall (list #'provider-executor-test--ordinary #'provider-executor-test--retry
                       #'provider-executor-test--cleanup-failure #'provider-executor-test--early-abort
                       #'provider-executor-test--opening-abort #'provider-executor-test--reading-abort
                       #'provider-executor-test--retry-abort #'provider-executor-test--admission
                       #'provider-executor-test--local-http #'provider-executor-test--service))
  10)
