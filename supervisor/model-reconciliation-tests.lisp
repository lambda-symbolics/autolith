;;;; -- Cross-Journal Model Publication Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () integer) run-model-reconciliation-tests))

(defun run-model-reconciliation-tests ()
  "Check publication retry across the two real journals and return check count."
  (let ((checks 0) (paths nil))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Model reconciliation check ~D failed." checks)))

             (path ()
               (let ((path (merge-pathnames
                            (format nil "autolith-model-projection-~36R.sexp"
                                    (random (expt 36 20)))
                            (uiop:temporary-directory))))
                 (push path paths)
                 path))

             (object (&rest fields)
               (let ((result (make-hash-table :test 'equal)))
                 (loop for (key value) on fields by (lambda (tail) (rest (rest tail)))
                       do (setf (gethash key result) value))
                 result))

             (accepted (replay)
               (model-replay-store-start-attempt replay :request-id "42" :turn-id "turn"
                                                       :profile-id "profile" :family "responses")
               (multiple-value-bind (handle event)
                   (model-replay-store-put-item
                    replay :request-id "42" :attempt 1
                    :original (object "arguments" "{malformed")
                    :view     (object "reasoning" (object "text" "summary")))
                 (model-replay-store-accept
                  replay :request-id "42" :attempt 1
                  :complete (object "reason" "stop" "turn_completion" "ended"
                                    "output_items" (vector (gethash "item_completed" event))))
                 handle))

             (pending (replay)
               (length (model-replay-store-pending-projections replay)))

             (records (session)
               (cl-protobufs.autolith.parent.v1:session-page.records
                (session-store-read session
                                    (cl-protobufs.autolith.parent.v1:make-session-read :limit 256))))

             (completion-blocked (replay)
               (check (handler-case
                          (progn (model-replay-store-completion replay :request-id "42" :attempt 1)
                                 nil)
                        (model-replay-store-error (condition)
                          (eq ':projection-required (model-replay-store-error-code condition)))))))
      (unwind-protect
           (progn
             ;; Before publication, a recovered acceptance has a pending outbox entry.
             (let* ((replay-path  (path))
                    (session-path (path))
                    (replay       (model-replay-store-open replay-path "session"))
                    (session      (session-store-open session-path "session")))
               (session-store-start-turn session "turn" "input")
               (accepted replay)
               (setf replay (model-replay-store-open replay-path "session")
                     session (session-store-open session-path "session"))
               (check (= 1 (pending replay)))
               (completion-blocked replay)
               (check (= 1 (model-replay-store-project-session replay session)))
               (check (zerop (pending replay)))
               (check (= 3 (length (records session))))
               (check (eq ':turn-outcome
                          (cl-protobufs.autolith.parent.v1:session-record.kind
                           (second (records session)))))
               (setf replay (model-replay-store-open replay-path "session")
                     session (session-store-open session-path "session"))
               (check (= 3 (length (records session))))
               (check (string= "stop" (gethash "reason" (model-replay-store-completion
                                                         replay :request-id "42" :attempt 1))))
               (check (zerop (model-replay-store-project-session replay session))))
             ;; Crash after the session commit but before the outbox acknowledgment.
             ;; Retry after the turn ends must reuse the original note, not append it.
             (let* ((replay-path  (path))
                    (session-path (path))
                    (replay       (model-replay-store-open replay-path "session"))
                    (session      (session-store-open session-path "session")))
               (session-store-start-turn session "turn" "input")
               (let* ((handle     (accepted replay))
                      (projection (aref (model-replay-store-pending-projections replay) 0)))
                 (setf session (session-store-open session-path "session"))
                 (session-store--recover-model-result
                  session :event-id (gethash "event_id" projection)
                  :turn-id          (gethash "turn_id" projection)
                  :request-id       (gethash "request_id" projection)
                  :attempt          (gethash "attempt" projection)
                   :complete        (gethash "model_result" projection)
                   :request-kind     ':generate :request-purpose ':ordinary)
                 (setf replay (model-replay-store-open replay-path "session")
                       session (session-store-open session-path "session"))
                 (completion-blocked replay)
                 (let ((before (length (records session))))
                   (check (= 1 (model-replay-store-project-session replay session)))
                   (check (= before (length (records session)))))
                 (check (string= "{malformed"
                                 (gethash "arguments"
                                          (model-replay-store-resolve
                                           replay handle :session-id "session"
                                                         :profile-id "profile" :family "responses"))))))
             ;; A session rejection must leave the outbox unacknowledged.
             (let* ((replay  (model-replay-store-open (path) "session"))
                    (session (session-store-open (path) "session"))
                    (foreign (session-store-open (path) "foreign")))
               (accepted replay)
               (check (handler-case (progn (model-replay-store-project-session replay foreign) nil)
                        (model-replay-store-error (condition)
                          (eq ':session-mismatch (model-replay-store-error-code condition)))))
               (check (handler-case (progn (model-replay-store-project-session replay session) nil)
                        (session-store-error () t)))
               (check (= 1 (pending replay)))
               (completion-blocked replay)
               (check (null (records session)))
               (session-store-start-turn session "turn" "input")
               (check (= 1 (model-replay-store-project-session replay session))))
             checks)
        (dolist (path paths)
          (dolist (file (list path (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file file) (delete-file file))))))))
