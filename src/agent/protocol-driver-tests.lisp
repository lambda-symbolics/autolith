;;;; -- Agent Protocol Driver Checks --

(in-package #:autolith)

(-> agent-driver-test--lifecycle () null)
(defun agent-driver-test--lifecycle ()
  "Wait for a request inside the lifecycle callback, then finish and stop."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :turn-start (agent-wire:make-turn-start :turn-id "turn" :input-sequence 1)))
     (let ((request (autolith-supervisor::parent-connection-receive parent)))
       (assert (eq 'agent-wire:session-read (agent-wire:envelope.body-case request)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                         :session-page (agent-wire:make-session-page))))
     (assert (eq 'agent-wire:turn-finish
                 (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent))))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :prepare-stop (agent-wire:make-prepare-stop)))
     (assert (eq 'agent-wire:ready-to-stop
                 (agent-wire:envelope.body-case (autolith-supervisor::parent-connection-receive parent)))))
   (lambda (client)
     (agent-protocol-handshake client)
     (let ((calls nil) (disconnects 0)
           (driver nil))
       (setf driver
             (agent-protocol-driver-start
              client :lifecycle
              (lambda (owner envelope)
                (push (agent-wire:envelope.body-case envelope) calls)
                (case (agent-wire:envelope.body-case envelope)
                  (agent-wire:turn-start
                   (let ((delivery (agent-protocol-driver-request owner #'agent-protocol-session-read)))
                     (multiple-value-bind (reply terminal) (agent-protocol-driver-next delivery :timeout 2)
                       (assert terminal)
                       (assert (eq 'agent-wire:session-page (agent-wire:envelope.body-case reply)))))
                   (agent-protocol-finish-turn client ':completed))
                  (agent-wire:prepare-stop
                   (agent-protocol-ready-to-stop client)
                   (agent-protocol-driver-close owner))))
              :disconnect (lambda (owner condition)
                            (declare (ignore owner))
                            (assert (typep condition 'condition))
                            (incf disconnects))))
       (unwind-protect
           (progn
             (agent-protocol-driver-join driver :timeout 4)
             (assert (= 1 disconnects))
             (assert (equal (reverse calls) '(agent-wire:turn-start agent-wire:prepare-stop))))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--cancel () null)
(defun agent-driver-test--cancel ()
  "Keep cancellation acknowledgement separate from the target event stream."
  (agent-protocol-test--peers
   (lambda (parent)
     (agent-model-test--turn parent)
     (let* ((start  (autolith-supervisor::parent-connection-receive parent))
            (cancel (autolith-supervisor::parent-connection-receive parent))
            (id     (agent-wire:envelope.message-id start)))
       (assert (= id (agent-wire:cancel-request.request-id (agent-wire:envelope.cancel-request cancel))))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to id :model-event (agent-model-test--begin)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to   (agent-wire:envelope.message-id cancel)
                                         :cancel-ack (agent-wire:make-cancel-ack)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to id :model-event
                                         (agent-wire:make-model-event :index 2 :attempt 1 :failed
                                                                      (agent-wire:make-failure :code ':cancelled)))))
     ;; Hold the peer open until the consumer has observed every reply.
     (autolith-supervisor::parent-connection-receive parent))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
     (agent-protocol-receive client)
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
           (let* ((request (agent-protocol-driver-request
                            driver (lambda (peer)
                                     (agent-protocol-model-start
                                      peer (agent-wire:make-model-start :profile-id "profile"
                                                                       :turn-id "turn" :kind ':generate)))))
                  (cancel  (agent-protocol-driver-cancel driver request)))
             (multiple-value-bind (reply terminal) (agent-protocol-driver-next cancel :timeout 2)
               (assert terminal)
               (assert (agent-wire:envelope.cancel-ack reply)))
             (multiple-value-bind (reply terminal) (agent-protocol-driver-next request :timeout 2)
               (assert (not terminal))
               (assert (= 1 (agent-wire:model-event.index (agent-wire:envelope.model-event reply)))))
             (multiple-value-bind (reply terminal) (agent-protocol-driver-next request :timeout 2)
               (assert terminal)
               (assert (agent-wire:model-event.failed (agent-wire:envelope.model-event reply))))
             (agent-model-test--rejects (lambda () (agent-protocol-driver-next request)) ':driver-invalid-consumer)
             (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--disconnect () null)
(defun agent-driver-test--disconnect ()
  "Wake a request waiter on peer loss without sending a second request."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (assert (agent-wire:envelope.session-read (autolith-supervisor::parent-connection-receive parent)))
     (autolith-supervisor::parent-connection-close parent))
   (lambda (client)
     (agent-protocol-handshake client)
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
           (let ((request (agent-protocol-driver-request driver #'agent-protocol-session-read)))
             (assert (handler-case (progn (agent-protocol-driver-next request :timeout 2) nil)
                       (error () t)))
             (agent-protocol-driver-join driver))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--limits () null)
(defun agent-driver-test--limits ()
  "Bound retained replies and bytes, including an envelope held by a callback."
  (dolist (byte-limit '(1 16777216))
    (agent-protocol-test--peers
     (lambda (parent)
       (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
       (let ((first  (autolith-supervisor::parent-connection-receive parent))
             (second (autolith-supervisor::parent-connection-receive parent)))
         (dolist (request (list first second))
           (autolith-supervisor::parent-connection-send
            parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                             :session-page (agent-wire:make-session-page)))))
       (handler-case (autolith-supervisor::parent-connection-receive parent) (error () nil)))
     (lambda (client)
       (agent-protocol-handshake client)
       (let ((driver (agent-protocol-driver-start client :max-events 1 :max-bytes byte-limit)))
         (unwind-protect
             (progn
               (agent-protocol-driver-request driver #'agent-protocol-session-read)
               (agent-protocol-driver-request driver #'agent-protocol-session-read)
               (agent-protocol-driver-join driver :timeout 3)
               (assert (eq ':driver-delivery-limit
                           (agent-protocol-error-reason (agent-protocol-driver-failure driver)))))
           (agent-protocol-driver-close driver)
           (agent-protocol-driver-join driver))))))
  nil)

(-> agent-driver-test--callback-failure () null)
(defun agent-driver-test--callback-failure ()
  "Close on lifecycle failure; report join timeout for a blocked callback."
  (agent-protocol-test--peers
   (lambda (parent)
     (agent-model-test--turn parent)
     (handler-case (autolith-supervisor::parent-connection-receive parent) (error () nil)))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
     (let* ((entered (sb-thread:make-semaphore))
            (release (sb-thread:make-semaphore))
            (driver  (agent-protocol-driver-start
                      client :lifecycle (lambda (owner envelope)
                                          (declare (ignore owner envelope))
                                          (sb-thread:signal-semaphore entered)
                                          (sb-thread:wait-on-semaphore release)
                                          (error 'agent-protocol-error :reason ':test-callback-failed)))))
       (unwind-protect
           (progn
             (assert (sb-thread:wait-on-semaphore entered :timeout 2))
             (agent-model-test--rejects (lambda () (agent-protocol-driver-join driver :timeout 0.01))
                                       ':driver-join-timeout)
             (sb-thread:signal-semaphore release)
             (agent-protocol-driver-join driver :timeout 2)
             (assert (eq ':test-callback-failed
                         (agent-protocol-error-reason (agent-protocol-driver-failure driver)))))
         (sb-thread:signal-semaphore release)
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--ownership () null)
(defun agent-driver-test--ownership ()
  "Reject duplicate starts and direct readers without closing the driver."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (let ((request (autolith-supervisor::parent-connection-receive parent)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                         :session-page (agent-wire:make-session-page))))
     (autolith-supervisor::parent-connection-receive parent))
   (lambda (client)
     (agent-protocol-handshake client)
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
           (progn
             (agent-model-test--rejects (lambda () (agent-protocol-driver-start client)) ':reader-owned)
             (agent-model-test--rejects (lambda () (agent-protocol-receive client)) ':reader-owned)
             (let ((request (agent-protocol-driver-request driver #'agent-protocol-session-read)))
               (multiple-value-bind (reply terminal) (agent-protocol-driver-next request :timeout 2)
                 (assert terminal)
                 (assert (agent-wire:envelope.session-page reply))))
             (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--timeout () null)
(defun agent-driver-test--timeout ()
  "Keep a timed-out request live and register a reply received during SEND."
  (let ((release (sb-thread:make-semaphore))
        (replied (sb-thread:make-semaphore)))
    (agent-protocol-test--peers
     (lambda (parent)
       (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
       (dotimes (index 2)
         (let ((request (autolith-supervisor::parent-connection-receive parent)))
           (when (zerop index) (assert (sb-thread:wait-on-semaphore release :timeout 2)))
           (autolith-supervisor::parent-connection-send
            parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id request)
                                             :session-page (agent-wire:make-session-page)))
           (unless (zerop index) (sb-thread:signal-semaphore replied))))
       (autolith-supervisor::parent-connection-receive parent))
     (lambda (client)
       (agent-protocol-handshake client)
       (let ((driver (agent-protocol-driver-start client)))
         (unwind-protect
             (progn
               (let ((request (agent-protocol-driver-request driver #'agent-protocol-session-read)))
                 (multiple-value-bind (reply terminal) (agent-protocol-driver-next request :timeout 0)
                   (assert (null reply))
                   (assert (null terminal)))
                 (sb-thread:signal-semaphore release)
                 (assert (second (multiple-value-list (agent-protocol-driver-next request :timeout 2)))))
               (let ((request (agent-protocol-driver-request
                               driver (lambda (peer)
                                        (prog1 (agent-protocol-session-read peer)
                                          (assert (sb-thread:wait-on-semaphore replied :timeout 2)))))))
                 (assert (second (multiple-value-list (agent-protocol-driver-next request :timeout 2)))))
               (agent-protocol-output client ':diagnostic "done"))
           (sb-thread:signal-semaphore release)
           (agent-protocol-driver-close driver)
           (agent-protocol-driver-join driver))))))
  nil)

(-> agent-driver-test--pending-loss () null)
(defun agent-driver-test--pending-loss ()
  "Do not mark a validated progress event terminal when a send closes the client."
  (agent-protocol-test--peers
   (lambda (parent)
     (agent-model-test--turn parent)
     (let ((request (autolith-supervisor::parent-connection-receive parent)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to    (agent-wire:envelope.message-id request)
                                         :model-event (agent-model-test--begin))))
     (handler-case (autolith-supervisor::parent-connection-receive parent) (error () nil)))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
     (agent-protocol-receive client)
     (let ((driver (agent-protocol-driver-start client)))
       (unwind-protect
           (let ((request
                   (agent-protocol-driver-request
                    driver
                    (lambda (peer)
                      (let ((id (agent-protocol-model-start
                                 peer (agent-wire:make-model-start :profile-id "profile"
                                                                  :turn-id "turn" :kind ':generate))))
                        ;; The registration lock holds routing until this sender returns.
                        ;; Wait until the sole reader has validated the first event.
                        (sb-ext:with-timeout 2
                          (loop until (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock peer))
                                        (= 1 (agent-model-request-index
                                              (gethash id (agent-protocol-client-pending peer)))))
                                do (sleep 0.001)))
                        (agent-protocol-close peer)
                        id)))))
             (multiple-value-bind (reply terminal) (agent-protocol-driver-next request :timeout 2)
               (assert (agent-wire:envelope.model-event reply))
               (assert (null terminal))))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-driver-test--close-order () null)
(defun agent-driver-test--close-order ()
  "Delay client cleanup and require disconnect notification after stream closure."
  (let* ((input    (make-instance 'agent-protocol-test-pipe))
         (output   (make-instance 'agent-protocol-test-pipe))
         (client   (agent-protocol-open input output "close-order"))
         (notified (sb-thread:make-semaphore))
         (driver   nil)
         (closer   nil))
    (setf (autolith-supervisor::parent-connection-phase client) ':ready)
    (setf driver
          (agent-protocol-driver-start
           client :disconnect (lambda (owner condition)
                                (declare (ignore owner condition))
                                (assert (not (open-stream-p input)))
                                (assert (not (open-stream-p output)))
                                (sb-thread:signal-semaphore notified))))
    (unwind-protect
        (progn
          (bordeaux-threads:with-lock-held ((agent-protocol-client-state-lock client))
            ;; Keep the first closer blocked before it can mark the client closed.
            (setf closer (bordeaux-threads:make-thread (lambda () (agent-protocol-driver-close driver))))
            (sb-ext:with-timeout 2
              (loop until (bordeaux-threads:with-lock-held ((agent-protocol-driver-lock driver))
                            (agent-protocol-driver-failure driver))
                    do (sleep 0.001)))
            (assert (null (sb-thread:wait-on-semaphore notified :timeout 0.02))))
          (bordeaux-threads:join-thread closer)
          (agent-protocol-driver-join driver)
          (assert (sb-thread:wait-on-semaphore notified :timeout 2)))
      (when closer (bordeaux-threads:join-thread closer))
      (agent-protocol-driver-close driver)
      (agent-protocol-driver-join driver)))
  nil)

(-> run-agent-protocol-driver-tests () (integer 0))
(defun run-agent-protocol-driver-tests ()
  "Run standalone driver lifecycle, delivery, disconnect and cleanup checks."
  (mapc #'funcall (list #'agent-driver-test--lifecycle #'agent-driver-test--cancel
                       #'agent-driver-test--disconnect #'agent-driver-test--limits
                       #'agent-driver-test--callback-failure #'agent-driver-test--ownership
                       #'agent-driver-test--timeout #'agent-driver-test--pending-loss
                       #'agent-driver-test--close-order))
  9)
