;;;; -- Real Model Replay Hook Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () null) run-model-replay-adapter-tests))

(defun run-model-replay-adapter-tests ()
  "Join the real collector, replay store, session journal, and scoped resolver."
  (let ((paths nil))
    (labels ((path ()
               (let ((path (merge-pathnames
                            (format nil "autolith-model-hooks-~36R.sexp" (random (expt 36 20)))
                            (uiop:temporary-directory))))
                 (push path paths)
                 path))

             (fixture (function &key (start t) capabilities)
               (let ((replay  (model-replay-store-open (path) "session"))
                     (session (session-store-open (path) "session"))
                     (profile (make-model-profile :id "profile" :model "model" :family ':responses
                                                  :capabilities capabilities)))
                 (when start (session-store-start-turn session "turn" "input"))
                 (funcall function replay session profile)))

             (make-collector (replay session profile &key emit (purpose ':ordinary))
               (make-model-replay-collector :replay-store replay :session-store session
                                           :request-id "request" :turn-id "turn" :profile profile :purpose purpose
                                           :emit-event (or emit #'identity)))

             (resolve (replay handle profile &key (kind ':item) (turn "turn"))
               (funcall (make-model-replay-resolver replay :turn-id "turn") handle
                        :kind kind :profile profile :turn-id turn))

             (denied (function)
               (assert (handler-case (progn (funcall function) nil)
                         (model-replay-store-error () t))))

             (records (session)
               (parent-proto:session-page.records
                (session-store-read session (parent-proto:make-session-read :limit 256)))))
      (unwind-protect
           (progn
             ;; Every callback follows its exact durable event, including terminal
             ;; acceptance and the session note. Mutating the sent copy is harmless.
             (fixture
              (lambda (replay session profile)
                (let* ((sent nil)
                       (collector
                         (make-collector
                          replay session profile
                          :emit (lambda (event)
                                  (let ((stored (model-replay-store-events replay "request")))
                                    (model-replay--verify-event
                                     (aref stored (1- (length stored))) (protocol-message->data event)))
                                  (when (parent-proto:model-event.has-complete event)
                                    (assert (= 2 (length (records session))))
                                    (assert (zerop (length (model-replay-store-pending-projections replay)))))
                                  (push (model-events--message-copy event) sent)
                                  (setf (parent-proto:model-event.index event) 0))))
                       (item (model-event-test--object "type" "function_call" "call_id" "call"
                               "name" "run" "arguments" "{malformed")))
                  (model-event-collector-begin collector)
                  (model-event-test--item collector item)
                  (let ((handle (parent-proto:output-item.replay-handle
                                 (parent-proto:model-event.item-completed (first sent)))))
                    (denied (lambda () (resolve replay handle profile)))
                    (let* ((result   (make-instance 'model-wire:provider-result :output-items (list item)))
                           (complete (model-event-collector-finish collector result)))
                      (assert (= 0 (parent-proto:output-item.item-index
                                    (first (parent-proto:model-complete.output-items complete)))))
                      (assert (string= "{malformed" (gethash "arguments" (resolve replay handle profile))))
                      (assert (= 3 (length sent) (length (model-replay-store-events replay "request"))))
                      (model-event-collector-finish collector result)
                      (assert (= 3 (length sent)))
                      (denied (lambda () (resolve replay handle profile :kind ':continuation)))
                      (denied (lambda () (resolve replay handle profile :turn "another-turn"))))))))
             ;; Classification belongs to the accepted outbox, including reply-loss recovery.
             (fixture
              (lambda (replay session profile)
                (let* ((collector (make-collector replay session profile :purpose ':summary))
                       (item      (model-event-test--text))
                       (result    (make-instance 'model-wire:provider-result :output-items (list item))))
                  (model-event-collector-begin collector)
                  (model-event-test--item collector item)
                  (assert (handler-case (progn (model-event-collector-finish collector result) nil)
                             (model-event-error () t)))
                  (let* ((fresh      (model-replay-store-open (model-replay-store-pathname replay) "session"))
                         (pending    (model-replay-store-pending-projections fresh))
                         (projection (aref pending 0)))
                    (assert (= 1 (length pending)))
                    (assert (equal "generate" (gethash "request_kind" projection)))
                    (assert (equal "summary" (gethash "request_purpose" projection)))
                    (denied (lambda () (model-replay-store-accept
                                        fresh :request-id "request" :attempt 1
                                        :complete        (gethash "model_result" projection)
                                        :request-purpose ':ordinary)))
                    (session-store-start-turn session "turn" "input")
                    (assert (= 1 (model-replay-store-project-session fresh session)))
                    (let* ((fresh-session (session-store-open (session-store-pathname session) "session"))
                           (note          (second (records fresh-session)))
                           (payload       (protocol-value->data (parent-proto:session-record.payload note))))
                      (assert (equal "generate" (gethash "request_kind" payload)))
                      (assert (equal "summary" (gethash "request_purpose" payload)))))))
              :start nil)
             ;; Attempt failures remain retryable; final failure is terminal.
             (fixture
              (lambda (replay session profile)
                (let ((collector (make-collector replay session profile))
                      (item      (model-event-test--text)))
                  (model-event-collector-begin collector :maximum-attempts 2)
                  (model-event-test--item collector item)
                  (let ((old (gethash "replay_handle"
                                     (gethash "item_completed"
                                              (aref (model-replay-store-events replay "request") 1)))))
                    (model-event-test--attempt-failed collector)
                    (assert (eq ':failed (model-replay-store-status replay "request")))
                    (model-event-collector-begin collector :maximum-attempts 2 :delay-milliseconds 10)
                    (model-event-test--item collector item)
                    (model-event-collector-finish collector
                                                 (make-instance 'model-wire:provider-result :output-items (list item)))
                    (assert (= 6 (length (model-replay-store-events replay "request"))))
                    (denied (lambda () (resolve replay old profile)))))))
             (fixture
              (lambda (replay session profile)
                (let ((collector (make-collector replay session profile)))
                  (model-event-collector-begin collector)
                  (model-event-collector-fail collector :code ':unavailable)
                  (assert (eq ':terminal-failed (model-replay-store-status replay "request")))
                  (assert (= 2 (length (model-replay-store-events replay "request"))))
                  (assert (gethash "failed" (aref (model-replay-store-events replay "request") 1)))
                  (assert (= 1 (length (records session)))))))
              ;; Reject an item before its durable hook when the public byte
              ;; budget is exhausted. The terminal failure must use the next
              ;; journal index, with no provisional item left behind.
             (fixture
              (lambda (replay session profile)
                (let ((collector (make-collector replay session profile)))
                  (model-event-collector-begin collector)
                  (setf (model-event-collector-public-size collector) (- (* 32 1024 1024) 256))
                  (model-event-test--rejects
                   (lambda ()
                     (model-event-test--item collector
                                             (model-event-test--text (make-string 2048 :initial-element #\x))))
                   ':resource-limit)
                  (assert (= 1 (model-event-collector-index collector)))
                  (assert (= 1 (length (model-replay-store-events replay "request"))))
                  (assert (null (model-event-collector-items collector)))
                  (model-event-collector-fail collector :code ':resource-limit)
                  (let ((events (model-replay-store-events replay "request")))
                    (assert (= 2 (length events)))
                    (assert (= 2 (gethash "index" (aref events 1))))
                    (assert (gethash "failed" (aref events 1))))
                  (assert (eq ':terminal-failed (model-replay-store-status replay "request"))))))
              ;; An item hook can commit its event before the caller sees an
              ;; error. Do not append a terminal event at the stale local index.
             (fixture
              (lambda (replay session profile)
                (let* ((collector (make-collector replay session profile))
                       (writer    (model-event-collector-record-item collector)))
                  (model-event-collector-begin collector)
                  (setf (slot-value collector 'record-item)
                        (lambda (&rest arguments)
                          (apply writer arguments)
                          (error "controlled post-write failure")))
                  (model-event-test--rejects
                   (lambda () (model-event-test--item collector (model-event-test--text)))
                   ':projection-failed)
                  (assert (eq ':uncertain (model-event-collector-phase collector)))
                  (assert (= 2 (length (model-replay-store-events replay "request"))))
                  (assert (= 1 (model-event-collector-index collector)))
                  (assert (handler-case
                              (progn (model-event-collector-fail collector :code ':unavailable) nil)
                            (model-event-error () t)))
                  (assert (= 2 (length (model-replay-store-events replay "request"))))
                  (let ((reopened (model-replay-store-open (model-replay-store-pathname replay) "session")))
                    (assert (eq ':interrupted (model-replay-store-status reopened "request")))))))
             ;; State tokens are private, typed handles. Resolve only after the
             ;; accepted result and its session note have both been published.
             (fixture
              (lambda (replay session profile)
                (let* ((collector (make-collector replay session profile))
                       (result    (make-instance 'model-wire:provider-result :response-id "private-response"
                                                 :turn-completion ':continue :turn-state "private-state")))
                  (model-event-collector-begin collector)
                  (let* ((complete     (model-event-collector-finish collector result))
                         (continuation (parent-proto:model-complete.continuation-handle complete))
                         (state        (parent-proto:model-complete.turn-state-handle complete)))
                    (assert (string= "private-response" (resolve replay continuation profile :kind ':continuation)))
                    (assert (string= "private-state" (resolve replay state profile :kind ':turn-state)))
                    (denied (lambda () (resolve replay state profile :kind ':item)))
                    (denied (lambda () (resolve replay continuation profile :kind ':turn-state)))
                    ;; A new trusted resolver for another turn must not authorize
                    ;; the old turn-state, even when its caller turn matches.
                    (let ((next (make-model-replay-resolver replay :turn-id "next-turn")))
                      (denied (lambda ()
                                (funcall next state :kind ':turn-state :profile profile :turn-id "next-turn")))
                      (assert (string= "private-response"
                                       (funcall next continuation :kind ':continuation
                                                                  :profile profile :turn-id "next-turn"))))
                    (model-replay-store-compact replay)
                    (let ((reopened (model-replay-store-open (model-replay-store-pathname replay) "session")))
                      (assert (string= "private-state" (resolve reopened state profile :kind ':turn-state)))))))
              :capabilities '(:continuation :turn-state))
             ;; A terminal send failure cannot revoke the accepted result or
             ;; trigger a second terminal send when FINISH is repeated.
             (fixture
              (lambda (replay session profile)
                (let* ((sends     0)
                       (collector (make-collector
                                   replay session profile
                                   :emit (lambda (event)
                                           (when (parent-proto:model-event.has-complete event)
                                             (incf sends)
                                             (error "controlled terminal send failure")))))
                       (result    (make-instance 'model-wire:provider-result)))
                  (model-event-collector-begin collector)
                  (assert (handler-case (progn (model-event-collector-finish collector result) nil)
                            (model-event-error () t)))
                  (assert (eq ':accepted (model-event-collector-phase collector)))
                  (assert (= 2 (length (records session))))
                  (assert (hash-table-p (model-replay-store-completion replay :request-id "request" :attempt 1)))
                  (model-event-collector-finish collector result)
                  (assert (= 1 sends)))))
             ;; Acceptance can commit before session publication fails. Leave
             ;; that result pending and reconcile it instead of emitting FAILED.
             (fixture
              (lambda (replay session profile)
                (let* ((sent      nil)
                       (collector (make-collector replay session profile :emit (lambda (event) (push event sent))))
                       (result    (make-instance 'model-wire:provider-result)))
                  (model-event-collector-begin collector)
                  (assert (handler-case (progn (model-event-collector-finish collector result) nil)
                            (model-event-error () t)))
                  (assert (eq ':uncertain (model-event-collector-phase collector)))
                  (assert (eq ':accepted (model-replay-store-status replay "request")))
                  (assert (= 1 (length (model-replay-store-pending-projections replay))))
                  (assert (notany #'parent-proto:model-event.has-complete sent))
                  (assert (notany #'parent-proto:model-event.has-failed sent))
                  (session-store-start-turn session "turn" "input")
                  (assert (= 1 (model-replay-store-project-session replay session)))
                  (assert (hash-table-p (model-replay-store-completion replay :request-id "request" :attempt 1)))))
              :start nil))
        (dolist (path paths)
          (dolist (file (list path (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file file) (delete-file file)))))))
  nil)
