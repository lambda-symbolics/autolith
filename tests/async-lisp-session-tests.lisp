(in-package #:autolith)

;;;; -- Async Lisp Session Persistence --

(defun test-conversation-async-lisp-session-durable-append-reload ()
  "Persist queued events and once-only deliveries, including a failed append retry."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration :identifier (make-identifier)))
           (identifier (conversation-append-async-lisp-event conversation "? *x*" nil)))
      (conversation-append-async-lisp-event conversation "? *x*" "42"
                                           :submission-identifier identifier)
      (let ((reloaded (conversation-load (conversation-pathname conversation))))
        (test-assert (= 2 (deque-count (conversation-pending-async-lisp-events reloaded)))
                     "source and completion survive reload before provider delivery"))
      (let ((append-record (symbol-function 'conversation-append-record)))
        (test-call-with-function-replacements
         (list (list 'conversation-append-record
                     (lambda (observed record)
                       (if (eq (first record) ':async-lisp-delivered)
                           (error 'file-error :pathname (conversation-pathname observed))
                           (funcall append-record observed record)))))
         (lambda ()
           (test-assert
            (handler-case (progn (conversation-flush-async-lisp-events conversation) nil)
              (file-error () t))
            "a failed delivery append is reported"))))
      (test-assert (and (null (conversation-input-items conversation))
                       (= 2 (deque-count (conversation-pending-async-lisp-events conversation))))
                   "failed persistence leaves provider history unchanged and events pending")
      (test-assert (= 2 (conversation-flush-async-lisp-events conversation))
                   "retry projects source and completion")
      (test-assert (zerop (conversation-flush-async-lisp-events conversation))
                   "delivered events do not repeat")
      (let ((reloaded (conversation-load (conversation-pathname conversation))))
        (test-assert (and (= 2 (length (conversation-input-items reloaded)))
                         (deque-empty-p (conversation-pending-async-lisp-events reloaded)))
                     "delivery markers reconstruct provider history and clear pending state")
        (test-assert
         (equal (mapcar #'json-encode (conversation-input-items conversation))
                (mapcar #'json-encode (conversation-input-items reloaded)))
         "replay preserves the exact delivered model messages")))))

(defun test-conversation-async-lisp-session-delayed-projection ()
  "Wait for every call in a parallel batch, then deliver at the agent boundary."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration :identifier (make-identifier)))
           (agent (make-instance 'agent :conversation conversation)))
      (dolist (identifier '("first" "second"))
        (conversation-append-provider-item
         conversation
         (json-object "type" "function_call" "call_id" identifier
                      "name" "test_echo" "arguments" "{}")))
      (conversation-append-async-lisp-event conversation "? one" nil)
      (conversation-append-provider-item
       conversation (function-call-output-item "second" "done"))
      (test-assert (zerop (conversation-flush-async-lisp-events conversation))
                   "a result at the projection tail does not hide another pending call")
      (test-assert (= 3 (length (conversation-input-items conversation)))
                   "no user observation interrupts the unfinished call batch")
      (conversation-append-provider-item
       conversation (function-call-output-item "first" "done"))
      (agent--apply-steering-input agent (make-instance 'agent-observer) 1)
      (test-assert (= 5 (length (conversation-input-items conversation)))
                   "the safe agent boundary delivers the pending submission")
      (test-assert (zerop (conversation-flush-async-lisp-events conversation))
                   "the boundary delivers each event once"))))

(defun test-conversation-async-lisp-session-repeated-source-identifiers ()
  "Keep explicit identities when identical forms finish in reverse order."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration :identifier (make-identifier)))
           (first (conversation-append-async-lisp-event conversation "? same" nil))
           (second (conversation-append-async-lisp-event conversation "? same" nil)))
      (test-assert (not (string= first second)) "each submission has an independent identity")
      (conversation-append-async-lisp-event conversation "? same" "second result"
                                           :submission-identifier second)
      (conversation-append-async-lisp-event conversation "? same" "first result"
                                           :submission-identifier first)
      (test-assert (= 4 (conversation-flush-async-lisp-events conversation))
                   "both source and completion pairs are delivered")
      (let ((items (mapcar #'json-encode (conversation-input-items conversation))))
        (test-assert (and (search second (third items))
                         (search "second result" (third items))
                         (search first (fourth items))
                         (search "first result" (fourth items)))
                     "completion messages identify the actual originating submission"))
      (test-assert
       (handler-case
           (progn (conversation-append-async-lisp-event conversation "? same" "ambiguous") nil)
         (conversation-invariant-error () t))
       "a result without an identifier is rejected")
      (test-assert
       (handler-case
           (progn (conversation--project-record ':async-lisp-event conversation
                                                 '(:source "? x" :kind :result)) nil)
         (conversation-invariant-error () t))
       "malformed persisted async events are rejected"))))
