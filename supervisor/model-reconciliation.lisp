;;;; -- Model Result Session Publication --

(in-package #:autolith-supervisor)

(declaim (ftype (function (model-replay-store session-store) (integer 0 *))
                model-replay-store-project-session))

(defun model-replay-store-project-session (replay-store session-store)
  "Publish pending accepted results to SESSION-STORE and return their count.

The journals are separate. Commit the idempotent parent note before acknowledging
its replay outbox entry. A failed or interrupted call can repeat the same note
identity on retry. Never acknowledge a projection whose session commit failed.
Run this before authorizing replay handles or sending terminal completion.
Obtain recovery evidence only from the validated replay journal below."
  (unless (string= (model-replay-store-session replay-store)
                   (session-store-id session-store))
    (error 'model-replay-store-error :code ':session-mismatch))
  (let ((count 0))
    (loop for projection across (model-replay-store-pending-projections replay-store)
          for request-id = (gethash "request_id" projection)
          for attempt = (gethash "attempt" projection)
          for event-id = (gethash "event_id" projection)
          do (session-store--recover-model-result
              session-store
              :event-id         event-id
              :turn-id          (gethash "turn_id" projection)
              :request-id       request-id
              :attempt          attempt
               :request-kind    (second (assoc (gethash "request_kind" projection)
                                               '(("generate" :generate) ("native_compaction" :native-compaction))
                                               :test #'equal))
               :request-purpose (second (assoc (gethash "request_purpose" projection)
                                               '(("ordinary" :ordinary) ("summary" :summary)) :test #'equal))
              :complete         (gethash "model_result" projection))
             (model-replay-store-mark-projected replay-store
                                                :request-id request-id
                                                :attempt    attempt
                                                :note-id    event-id)
             (incf count))
    count))
