;;;; -- Agent Model Stream Checks --

(in-package #:autolith)

(-> agent-model-test--welcome () agent-wire:welcome)
(defun agent-model-test--welcome ()
  "Select ordered model streams on the existing serialized peer fixture."
  (let ((welcome (agent-protocol-test--welcome)))
    (setf (agent-wire:welcome.features welcome) '("session.v1" "model.v1" "model-items-v1"))
    welcome))

(-> agent-model-test--turn (autolith-supervisor::parent-connection) null)
(defun agent-model-test--turn (parent)
  "Start the test turn after a model-capable handshake."
  (autolith-supervisor::parent-connection-handshake parent (agent-model-test--welcome))
  (autolith-supervisor::parent-connection-send
   parent (agent-wire:make-envelope :turn-start (agent-wire:make-turn-start :turn-id "turn" :input-sequence 1)))
  nil)

(-> agent-model-test--start (agent-protocol-client) integer)
(defun agent-model-test--start (client)
  "Negotiate model support, receive the turn, and start ordinary generation."
  (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
  (agent-protocol-receive client)
  (agent-protocol-model-start client (agent-wire:make-model-start :profile-id "profile" :turn-id "turn"
                                                               :kind ':generate)))

(-> agent-model-test--item (string &optional (integer 0)) agent-wire:output-item)
(defun agent-model-test--item (handle &optional (index 0))
  "Build a completed tool call with malformed arguments preserved verbatim."
  (agent-wire:make-output-item :item-index index :replay-handle handle :family "responses"
                               :tool-call (agent-wire:make-tool-call :call-id "call" :name "run"
                                                                    :arguments-json "{malformed")))

(-> agent-model-test--begin (&key (:index integer) (:attempt integer) (:maximum integer)
                                (:delay integer)) agent-wire:model-event)
(defun agent-model-test--begin (&key (index 1) (attempt 1) (maximum 2) (delay 0))
  "Build an indexed attempt start."
  (agent-wire:make-model-event :index index :attempt attempt :attempt-started
                               (agent-wire:make-attempt-started :maximum-attempts maximum :delay-milliseconds delay)))

(-> agent-model-test--failed (&key (:index integer) (:retry boolean)) agent-wire:model-event)
(defun agent-model-test--failed (&key (index 2) (retry t))
  "Build a retry disposition for the first attempt."
  (agent-wire:make-model-event :index index :attempt 1 :attempt-failed
                               (agent-wire:make-attempt-failed :retryable retry :failure
                                                               (agent-wire:make-failure :code ':unavailable))))

(-> agent-model-test--complete (&key (:index integer) (:attempt integer) (:items list)) agent-wire:model-event)
(defun agent-model-test--complete (&key (index 2) (attempt 1) items)
  "Build one ordinary accepted completion."
  (agent-wire:make-model-event :index index :attempt attempt :complete
                               (agent-wire:make-model-complete :reason ':tool-calls :output-items items)))

(-> agent-model-test--rejects (function keyword) null)
(defun agent-model-test--rejects (function reason)
  "Require a fixed local client error."
  (assert (handler-case (progn (funcall function) nil)
            (agent-protocol-error (condition) (eq reason (agent-protocol-error-reason condition)))))
  nil)

(-> agent-model-test--stream () null)
(defun agent-model-test--stream ()
  "Interleave a retried stream, session reply and cancellation acknowledgements."
  (agent-protocol-test--peers
   (lambda (parent)
     (agent-model-test--turn parent)
     (let* ((start  (autolith-supervisor::parent-connection-receive parent))
            (read   (autolith-supervisor::parent-connection-receive parent))
            (cancel (autolith-supervisor::parent-connection-receive parent))
            (id     (agent-wire:envelope.message-id start))
            (item   (agent-model-test--item "new-handle")))
       (assert (= id (agent-wire:cancel-request.request-id (agent-wire:envelope.cancel-request cancel))))
       (dolist (event (list (agent-model-test--begin)
                           (agent-wire:make-model-event :index 2 :attempt 1
                                                        :item-completed (agent-model-test--item "old-handle"))
                           (agent-model-test--failed :index 3)
                           (agent-model-test--begin :index 4 :attempt 2 :delay 10)))
         (autolith-supervisor::parent-connection-send parent (agent-wire:make-envelope :reply-to id :model-event event)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to     (agent-wire:envelope.message-id read)
                                         :session-page (agent-wire:make-session-page :high-water-sequence 1)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to   (agent-wire:envelope.message-id cancel)
                                         :cancel-ack (agent-wire:make-cancel-ack :already-terminal t)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to id :model-event
                                         (agent-wire:make-model-event :index 5 :attempt 2 :item-completed item)))
       (autolith-supervisor::parent-connection-send
        parent (agent-wire:make-envelope :reply-to id :model-event
                                         (agent-model-test--complete :index 6 :attempt 2 :items (list item))))
       (let ((repeat (autolith-supervisor::parent-connection-receive parent)))
         (assert (= id (agent-wire:cancel-request.request-id (agent-wire:envelope.cancel-request repeat))))
         (autolith-supervisor::parent-connection-send
          parent (agent-wire:make-envelope :reply-to   (agent-wire:envelope.message-id repeat)
                                           :cancel-ack (agent-wire:make-cancel-ack :already-terminal t))))))
   (lambda (client)
     (let* ((*agent-protocol-model-history-limit* 1)
            (id                                   (agent-model-test--start client)))
       (agent-protocol-session-read client)
       (agent-protocol-cancel-request client id)
       (dotimes (index 4)
         (let ((envelope (agent-protocol-receive client)))
           (assert (= id (agent-wire:envelope.reply-to envelope)))
           (assert (= (1+ index) (agent-wire:model-event.index (agent-wire:envelope.model-event envelope))))))
       (assert (eq 'agent-wire:session-page (agent-wire:envelope.body-case (agent-protocol-receive client))))
       (assert (eq 'agent-wire:cancel-ack (agent-wire:envelope.body-case (agent-protocol-receive client))))
       (assert (= 1 (hash-table-count (agent-protocol-client-pending client))))
       (let ((item (agent-wire:model-event.item-completed
                    (agent-wire:envelope.model-event (agent-protocol-receive client)))))
         (assert (string= "{malformed" (agent-wire:tool-call.arguments-json (agent-wire:output-item.tool-call item))))
         (setf (agent-wire:output-item.replay-handle item) "mutated"))
       (assert (agent-wire:model-event.has-complete
                (agent-wire:envelope.model-event (agent-protocol-receive client))))
       (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
       (agent-protocol-cancel-request client id)
       (assert (agent-wire:cancel-ack.already-terminal
                (agent-wire:envelope.cancel-ack (agent-protocol-receive client))))
       (agent-model-test--rejects
        (lambda () (agent-protocol-cancel-request client 999)) ':unknown-model-request)
       (agent-model-test--rejects
        (lambda () (agent-protocol-model-start client (agent-wire:make-model-start :turn-id "turn" :kind ':generate)))
        ':request-limit))))
  nil)

(-> agent-model-test--bad-stream (list keyword) null)
(defun agent-model-test--bad-stream (events reason)
  "Reject an invalid peer sequence through the actual framed reader."
  (agent-protocol-test--peers
   (lambda (parent)
     (agent-model-test--turn parent)
     (let* ((start (autolith-supervisor::parent-connection-receive parent))
            (id    (agent-wire:envelope.message-id start)))
       (dolist (event events)
         (autolith-supervisor::parent-connection-send
          parent (agent-wire:make-envelope :reply-to id :model-event event)))))
   (lambda (client)
     (agent-model-test--start client)
     (agent-model-test--rejects
      (lambda () (dotimes (index (length events)) (agent-protocol-receive client))) reason)
     (assert (eq ':closed (autolith-supervisor::parent-connection-phase client)))))
  nil)

(-> agent-model-test--failures () null)
(defun agent-model-test--failures ()
  "Retire rejected starts and cancelled streams without treating CancelAck as a result."
  (dolist (stream-p '(nil t))
    (agent-protocol-test--peers
     (lambda (parent)
       (agent-model-test--turn parent)
       (let* ((start (autolith-supervisor::parent-connection-receive parent))
              (id    (agent-wire:envelope.message-id start)))
         (when stream-p
           (let ((cancel (autolith-supervisor::parent-connection-receive parent)))
             (autolith-supervisor::parent-connection-send
              parent (agent-wire:make-envelope :reply-to id :model-event (agent-model-test--begin)))
             (autolith-supervisor::parent-connection-send
              parent (agent-wire:make-envelope :reply-to   (agent-wire:envelope.message-id cancel)
                                               :cancel-ack (agent-wire:make-cancel-ack)))))
         (autolith-supervisor::parent-connection-send
          parent (if stream-p
                     (agent-wire:make-envelope :reply-to id :model-event
                                                (agent-wire:make-model-event :index 2 :attempt 1 :failed
                                                                             (agent-wire:make-failure :code ':cancelled)))
                     (agent-wire:make-envelope :reply-to id :failure
                                                (agent-wire:make-failure :code ':resource-limit))))))
     (lambda (client)
       (let ((id (agent-model-test--start client)))
         (when stream-p
           (agent-protocol-cancel-request client id)
           (agent-protocol-receive client)
           (assert (not (agent-wire:cancel-ack.already-terminal
                          (agent-wire:envelope.cancel-ack (agent-protocol-receive client)))))
           (assert (= 1 (hash-table-count (agent-protocol-client-pending client)))))
         (agent-protocol-receive client)
         (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
         (assert (gethash id (agent-protocol-client-model-terminals client)))))))
  nil)

(-> agent-model-test--detached-state () null)
(defun agent-model-test--detached-state ()
  "Keep negotiated capabilities, limits and lifecycle identities separate from returned data."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :turn-start (agent-wire:make-turn-start :turn-id "turn" :input-sequence 1)))
     (let ((finish (autolith-supervisor::parent-connection-receive parent)))
       (assert (= 2 (agent-wire:envelope.reply-to finish)))
       (assert (string= "turn" (agent-wire:turn-finish.turn-id (agent-wire:envelope.turn-finish finish)))))
     (autolith-supervisor::parent-connection-send
      parent (agent-wire:make-envelope :prepare-stop (agent-wire:make-prepare-stop)))
     (assert (= 3 (agent-wire:envelope.reply-to (autolith-supervisor::parent-connection-receive parent)))))
   (lambda (client)
     (let ((welcome (agent-protocol-handshake
                     client :initialize
                     (lambda (value)
                       (setf (agent-wire:welcome.features value) '("session.v1" "model.v1" "model-items-v1")
                             (agent-wire:limits.max-in-flight-requests (agent-wire:welcome.limits value)) 0)))))
       (setf (agent-wire:welcome.features welcome) '("session.v1" "model.v1" "model-items-v1")))
     (assert (= 4 (agent-wire:limits.max-in-flight-requests
                   (autolith-supervisor::parent-connection-limits client))))
     (let ((turn (agent-protocol-receive client)))
       (setf (agent-wire:envelope.message-id turn) 999
             (agent-wire:turn-start.turn-id (agent-wire:envelope.turn-start turn)) "changed"))
     (agent-model-test--rejects
      (lambda () (agent-protocol-model-start client (agent-wire:make-model-start :turn-id "changed" :kind ':generate)))
      ':model-not-negotiated)
     (agent-protocol-finish-turn client ':completed)
     (let ((stop (agent-protocol-receive client)))
       (setf (agent-wire:envelope.message-id stop) 999))
     (agent-protocol-ready-to-stop client)))
  nil)

(-> run-agent-protocol-model-tests () null)
(defun run-agent-protocol-model-tests ()
  "Check ordered model streams, retry evidence, cancellation and malformed peers."
  (agent-model-test--stream)
  (agent-model-test--failures)
  (agent-model-test--detached-state)
  (let ((item (agent-model-test--item "handle")))
    (dolist (case (list
                   (list (list (agent-model-test--begin :index 2)) ':invalid-model-event)
                   (list (list (agent-model-test--begin :attempt 2)) ':invalid-model-attempt)
                   (list (list (agent-model-test--begin :delay 1)) ':invalid-model-attempt)
                   (list (list (agent-model-test--begin) (agent-model-test--begin :index 2 :attempt 2))
                         ':invalid-model-attempt)
                   (list (list (agent-model-test--begin) (agent-model-test--failed)
                               (agent-model-test--begin :index 3 :attempt 2 :maximum 3)) ':invalid-model-attempt)
                   (list (list (agent-model-test--begin) (agent-model-test--failed :retry nil)
                               (agent-model-test--begin :index 3 :attempt 2)) ':invalid-model-attempt)
                   (list (list (agent-model-test--begin)
                               (agent-wire:make-model-event :index 2 :attempt 1
                                                            :item-completed (agent-model-test--item "handle" 1)))
                         ':invalid-model-item)
                   (list (list (agent-model-test--begin)
                               (agent-wire:make-model-event :index 2 :attempt 1 :item-completed item)
                               (agent-model-test--complete :index 3)) ':invalid-model-complete)
                   (list (list (agent-model-test--begin) (agent-model-test--complete)
                               (agent-wire:make-model-event :index 3 :attempt 1
                                                            :text-delta (agent-wire:make-text-delta :text "late")))
                         ':unexpected-response)
                   (list (list (agent-model-test--begin)
                               (agent-wire:make-model-event :index 2 :attempt 1
                                                            :failed (agent-wire:make-failure))) ':invalid-failure)))
      (agent-model-test--bad-stream (first case) (second case))))
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome)))
   (lambda (client)
     (agent-protocol-handshake client)
     (agent-model-test--rejects
      (lambda () (agent-protocol-model-start client (agent-wire:make-model-start :turn-id "turn" :kind ':generate)))
      ':model-not-negotiated)))
  nil)
