;;;; -- Standalone Private Replay Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () integer) run-model-replay-store-tests))

(defun run-model-replay-store-tests ()
  "Check durable replay, interrupted publication, scopes, and portable originals."
  (let ((checks 0) (paths nil))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Model replay check ~D failed." checks)))

             (rejects (function &optional code)
               (check (handler-case (progn (funcall function) nil)
                        (model-replay-store-error (condition)
                          (or (null code) (eq code (model-replay-store-error-code condition)))))))

             (path ()
               (let ((path (merge-pathnames
                            (format nil "autolith-model-replay-~36R.sexp" (random (expt 36 20)))
                            (uiop:temporary-directory))))
                 (push path paths)
                 path))

             (object (&rest fields)
               (let ((result (make-hash-table :test 'equal)))
                 (loop for (key value) on fields by (lambda (tail) (rest (rest tail)))
                       do (setf (gethash key result) value))
                 result))

             (start (store &key (request "request") (attempt 1))
               (model-replay-store-start-attempt store :request-id request :turn-id "turn"
                                                :profile-id "profile" :family "responses"
                                                :attempt attempt :maximum-attempts 10))

             (put (store &key (request "request") (attempt 1) (original (object "id" "original")))
               (model-replay-store-put-item
                store :request-id request :attempt attempt :original original
                :view (object "reasoning" (object "text" ""))))

             (resolve (store handle)
               (model-replay-store-resolve store handle :session-id "session"
                                           :profile-id "profile" :family "responses"))

             (retain (store kind value &key (request "request") (attempt 1))
               (model-replay-store-retain-state store :request-id request :attempt attempt
                                               :kind kind :value value))

             (resolve-state (store handle kind)
               (model-replay-store-resolve-state store handle :kind kind :session-id "session"
                                                 :profile-id "profile" :family "responses" :turn-id "turn"))

             (fail (store failed &key (request "request") (attempt 1))
               (model-replay-store-fail-request store :request-id request :attempt attempt :failed failed))

             (complete (store &key (request "request") (attempt 1))
               (object "output_items"
                       (coerce (loop for event across (model-replay-store-events store request)
                                     when (and (= attempt (gethash "attempt" event))
                                               (gethash "item_completed" event))
                                       collect (gethash "item_completed" event)) 'vector)
                      "reason" "stop" "turn_completion" "ended"))

             (same (left right)
               (model-replay-store--exact-p (model-replay-store--portable left)
                                            (model-replay-store--portable right))))
      (unwind-protect
           (progn
             ;; A completed item is provisional until final provider validation.
             (let* ((path (path)) (store (model-replay-store-open path "session"))
                    (original (object "id" "Original-ID" "arguments" "{broken arguments"
                                      "null" nil "false" ':json-false "true" t
                                      "signed" (object "text" "" "signature" "private-signature")
                                      "encrypted" "opaque-private-data"
                                      "list" (list 1 2) "vector" #(1 2)
                                      "bytes" (make-array 3 :element-type   '(unsigned-byte 8)
                                                          :initial-contents '(0 128 255))
                                      "number" -0.0d0)))
               (start store)
               (multiple-value-bind (handle event) (put store :original original)
                 (check (= 71 (length handle)))
                 (check (= 2 (gethash "index" event)))
                 (check (= 0 (gethash "item_index" (gethash "item_completed" event))))
                 (rejects (lambda () (resolve store handle)) ':invalid-handle)
                 ;; Input mutations cannot change the committed item or event.
                 (setf (gethash "arguments" original) "{}"
                       (gethash "family" (gethash "item_completed" event)) "forged")
                 (let* ((projection (complete store))
                        (note       (model-replay-store-accept store :request-id "request" :attempt 1
                                                                   :complete projection)))
                   (check (equal note (model-replay-store-accept
                                       store :request-id "request" :attempt 1 :complete projection)))
                   (rejects (lambda () (resolve store handle)) ':invalid-handle)
                   (rejects (lambda () (model-replay-store-resolve
                                        store "replay/not-known" :session-id "session"
                                        :profile-id "profile" :family "responses")) ':invalid-handle)
                   (rejects (lambda () (model-replay-store-completion
                                        store :request-id "request" :attempt 1)) ':projection-required)
                   (check (= 2 (length (model-replay-store-events store "request"))))
                   ;; Crash after acceptance, before publishing the parent note.
                   (setf store (model-replay-store-open path "session"))
                   (rejects (lambda () (resolve store handle)) ':invalid-handle)
                   (let* ((pending  (model-replay-store-pending-projections store))
                          (delivery (aref pending 0)))
                     (check (= 1 (length pending)))
                     (check (equal note (gethash "event_id" delivery)))
                     (check (equal "turn" (gethash "turn_id" delivery)))
                     (check (equal "request" (gethash "request_id" delivery)))
                     (check (same projection (gethash "model_result" delivery)))
                     (setf (gethash "reason" (gethash "model_result" delivery)) "forged")
                     (check (same projection
                                  (gethash "model_result"
                                           (aref (model-replay-store-pending-projections store) 0)))))
                   ;; Simulate the separate journal: publish, crash before acknowledgment,
                   ;; then reconcile again. Both publications use one durable identity.
                   (let ((session-notes (make-hash-table :test 'equal)))
                     (loop repeat 2 do
                       (let ((delivery (aref (model-replay-store-pending-projections store) 0)))
                         (setf (gethash (gethash "event_id" delivery) session-notes) delivery))
                       (setf store (model-replay-store-open path "session")))
                     (check (= 1 (hash-table-count session-notes))))
                   (rejects (lambda () (model-replay-store-mark-projected
                                        store :request-id "request" :attempt 1 :note-id "wrong"))
                            ':projection-mismatch)
                   (model-replay-store-mark-projected store :request-id "request" :attempt 1 :note-id note)
                   (model-replay-store-mark-projected store :request-id "request" :attempt 1 :note-id note)
                   (check (string= "{broken arguments" (gethash "arguments" (resolve store handle))))
                   (check (null (gethash "null" (resolve store handle))))
                   (check (eq ':json-false (gethash "false" (resolve store handle))))
                   (check (minusp (float-sign (gethash "number" (resolve store handle)))))
                   (check (listp (gethash "list" (resolve store handle))))
                   (check (vectorp (gethash "vector" (resolve store handle))))
                   (check (equalp #(0 128 255) (gethash "bytes" (resolve store handle))))
                   (let ((copy (resolve store handle)))
                     (setf (char (gethash "arguments" copy) 0) #\X)
                     (check (char= #\{ (char (gethash "arguments" (resolve store handle)) 0))))
                   (dolist (scope '(("other" "profile" "responses")
                                    ("session" "other" "responses")
                                    ("session" "profile" "other")))
                     (rejects (lambda () (model-replay-store-resolve
                                          store handle :session-id (first scope)
                                          :profile-id (second scope) :family (third scope)))
                              ':invalid-handle))
                   (check (zerop (length (model-replay-store-pending-projections store))))
                   (check (same projection (model-replay-store-completion
                                            store :request-id "request" :attempt 1)))
                   (check (= 3 (length (model-replay-store-events store "request"))))
                   (check (same projection (gethash "complete"
                                                   (aref (model-replay-store-events store "request") 2))))
                   ;; Crash after acknowledgment and before sending terminal complete.
                   (model-replay-store-compact store)
                   (setf store (model-replay-store-open path "session"))
                   (check (zerop (length (model-replay-store-pending-projections store))))
                   (check (same projection (model-replay-store-completion
                                            store :request-id "request" :attempt 1)))
                   (check (string= "private-signature"
                                   (gethash "signature" (gethash "signed" (resolve store handle)))))
                   (setf (gethash "reason" projection) "changed")
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 1 :complete projection))
                            ':completion-mismatch))))
             ;; Failed and unfinished attempts never authorize replay, even after retries.
             (let* ((path (path)) (store (model-replay-store-open path "session")))
               (start store)
               (let ((failed (put store)))
                 (model-replay-store-fail-attempt store :request-id "request" :attempt 1)
                 (rejects (lambda () (resolve store failed)) ':invalid-handle)
                 (start store :attempt 2)
                 (let ((unfinished (put store :attempt 2)))
                   (setf store (model-replay-store-open path "session"))
                   (rejects (lambda () (resolve store unfinished)) ':invalid-handle)
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 2
                                        :complete (complete store :attempt 2))) ':inactive-attempt)
                   (start store :attempt 3)
                   (multiple-value-bind (accepted event) (put store :attempt 3)
                     (check (= 6 (gethash "index" event)))
                     (let ((note (model-replay-store-accept store :request-id "request" :attempt 3
                                                            :complete (complete store :attempt 3))))
                       (model-replay-store-mark-projected store :request-id "request" :attempt 3 :note-id note))
                     (check (equal "original" (gethash "id" (resolve store accepted))))
                     (rejects (lambda () (resolve store failed)) ':invalid-handle)
                     (rejects (lambda () (resolve store unfinished)) ':invalid-handle)
                     (rejects (lambda () (start store :attempt 4)) ':attempt-conflict)))))
             ;; Acceptance requires the exact final ordered views, not edited display data.
             (let ((store (model-replay-store-open (path) "session")))
               (start store)
               (let ((one (put store)) (two (put store)))
                 (check (not (equal one two)))
                 (let* ((projection (complete store)) (items (gethash "output_items" projection)))
                   (rotatef (aref items 0) (aref items 1))
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 1 :complete projection))
                            ':completion-mismatch)
                   (rotatef (aref items 0) (aref items 1))
                   (model-replay-store-accept store :request-id "request" :attempt 1 :complete projection))))
             ;; Resource bounds and type rejection do not expose private data.
             (let ((store (model-replay-store-open (path) "session")))
               (start store)
               (dolist (bad (list (object "secret" #'identity)
                                 (object "secret" (expt 2 80))
                                (object "secret" sb-ext:double-float-positive-infinity)
                                (object "secret" (make-string 2097153 :initial-element #\x))
                                (object "secret" (make-array 2097153 :element-type '(unsigned-byte 8)))
                                (object "secret" (make-array 65537 :initial-element 0))))
                 (rejects (lambda () (put store :original bad))))
               (let ((cycle (object)))
                 (setf (gethash "cycle" cycle) cycle)
                 (rejects (lambda () (put store :original cycle)) ':invalid-value))
               (let ((deep (object "leaf" t)))
                 (dotimes (index 34) (setf deep (object "child" deep)))
                 (rejects (lambda () (put store :original deep)) ':resource-limit))
               (rejects (lambda () (model-replay-store-append-event
                                    store :request-id "request" :attempt 1 :kind "complete" :payload nil))
                        ':invalid-event)
               (rejects (lambda () (model-replay-store-append-event
                                    store :request-id "request" :attempt 1 :kind "progress"
                                    :payload (object "text" ':json-false))) ':invalid-public-value)
               (check (= 1 (length (model-replay-store-events store "request")))))
             ;; Incremental appends reuse the validated state instead of full replay.
             (let* ((path (path)) (store (model-replay-store-open path "session")))
               (start store)
               (let ((state (model-replay-store-cache store)))
                 (dotimes (index 3000)
                   (model-replay-store-append-event
                    store :request-id "request" :attempt 1 :kind "text_delta"
                    :payload (object "text" (format nil "delta ~D" index))))
                 (check (eq state (model-replay-store-cache store))))
               (check (= 3001 (length (model-replay-store-events store "request"))))
               (with-open-file (stream path :direction ':output :if-exists ':append)
                 (write-string "(:event :version" stream))
               (setf store (model-replay-store-open path "session"))
               (check (= 3001 (length (model-replay-store-events store "request"))))
               (start store :attempt 2)
               (check (= 3003 (gethash "index"
                                          (model-replay-store-append-event
                                           store :request-id "request" :attempt 2 :kind "progress"
                                           :payload (object "phase" "retry"))))))
             ;; Independent handles and threads share one append order.
             (let* ((path (path)) (store (model-replay-store-open path "session"))
                    (other (model-replay-store-open path "session"))
                    (failures nil) (failure-lock (bordeaux-threads:make-lock "replay test failures")))
               (start store)
               (let ((threads
                       (loop for index below 4
                             collect
                             (let ((handle (if (evenp index) store other)))
                               (bordeaux-threads:make-thread
                                (lambda ()
                                  (handler-case
                                      (dotimes (number 50)
                                        (model-replay-store-append-event
                                         handle :request-id "request" :attempt 1 :kind "progress"
                                         :payload (object "phase" "test")))
                                    (error (condition)
                                      (bordeaux-threads:with-lock-held (failure-lock)
                                        (push condition failures))))))))))
                 (mapc #'bordeaux-threads:join-thread threads))
               (check (null failures))
               (let ((events (model-replay-store-events store "request")))
                 (check (= 201 (length events)))
                 (loop for event across events for index from 1
                       do (check (= index (gethash "index" event)))))
               (let* ((handle     (put other))
                      (projection (complete store))
                      (note       (model-replay-store-accept store :request-id "request" :attempt 1
                                                                  :complete projection)))
                 (check (equal '(:accepted 1)
                               (multiple-value-list (model-replay-store-status other "request"))))
                 (model-replay-store-compact other)
                 (rejects (lambda () (resolve store handle)) ':invalid-handle)
                 (check (equal note (gethash "event_id"
                                            (aref (model-replay-store-pending-projections other) 0))))
                 (model-replay-store-mark-projected other :request-id "request" :attempt 1 :note-id note)
                 (check (equal "original" (gethash "id" (resolve store handle)))))
               (rejects (lambda () (model-replay-store-open path "other-session")) ':scope-mismatch))
             ;; Explicit interruption rejects originals, and restart retains that outcome.
             (let* ((path (path)) (store (model-replay-store-open path "session")))
               (start store)
               (let ((handle (put store)))
                 (model-replay-store-interrupt-attempt store :request-id "request" :attempt 1)
                 (setf store (model-replay-store-open path "session"))
                 (check (equal '(:interrupted 1)
                               (multiple-value-list (model-replay-store-status store "request"))))
                 (rejects (lambda () (resolve store handle)) ':invalid-handle)))
             ;; A torn acceptance never authorizes the provisional original.
             (let* ((path (path)) (store (model-replay-store-open path "session")))
               (start store)
               (let ((handle (put store)))
                 (with-open-file (stream path :direction ':output :if-exists ':append)
                   (write-string "(:accept :version 1 :request \"request\" :attempt 1 :complete" stream))
                 (setf store (model-replay-store-open path "session"))
                 (check (zerop (length (model-replay-store-pending-projections store))))
                 (rejects (lambda () (resolve store handle)) ':invalid-handle)
                 (rejects (lambda () (model-replay-store-completion
                                      store :request-id "request" :attempt 1)) ':projection-required)))
             ;; Reader evaluation is disabled, even in a valid private journal.
             (let* ((path (path)) (store (model-replay-store-open path "session")))
               (with-open-file (stream path :direction ':output :if-exists ':append)
                 (write-string "#.(error \"private-reader-eval\")" stream))
               (rejects (lambda () (model-replay-store-events store "request")) ':storage-failure))
             ;; Private states consume no public index and require exact completion ownership.
             (let* ((path (path)) (store (model-replay-store-open path "session"))
                    (value (object "raw" "{broken" "number" -0.0d0 "null" nil
                                   "true" t "false" ':json-false "protocol-null" ':protocol-null
                                   "integer" -9223372036854775808 "list" '(1 2) "vector" #(1 2)
                                   "bytes" (make-array 3 :element-type   '(unsigned-byte 8)
                                                       :initial-contents '(0 128 255)))))
               (start store)
               (let* ((continuation (retain store ':continuation "private-response-id"))
                      (turn-state   (retain store ':turn-state value))
                      (item         (put store))
                      (projection   (complete store)))
                 (check (equal continuation (retain store ':continuation "private-response-id")))
                 (check (equal turn-state (retain store ':turn-state value)))
                 (check (not (equal continuation turn-state)))
                 (check (= 2 (length (model-replay-store-events store "request"))))
                 (check (= 0 (gethash "item_index" (aref (gethash "output_items" projection) 0))))
                 (rejects (lambda () (retain store ':continuation "changed")) ':state-conflict)
                 (rejects (lambda () (retain store ':item value)) ':invalid-state-kind)
                 (rejects (lambda () (resolve-state store continuation ':continuation)) ':invalid-handle)
                 (rejects (lambda () (model-replay-store-accept
                                      store :request-id "request" :attempt 1 :complete projection))
                          ':completion-mismatch)
                 (setf (gethash "continuation_handle" projection) continuation
                       (gethash "turn_state_handle" projection) continuation)
                 (rejects (lambda () (model-replay-store-accept
                                      store :request-id "request" :attempt 1 :complete projection))
                          ':completion-mismatch)
                 (setf (gethash "turn_state_handle" projection) turn-state)
                 (dolist (bad (list "raw-private-id" "" item nil))
                   (setf (gethash "continuation_handle" projection) bad)
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 1 :complete projection))
                            ':completion-mismatch))
                 (start store :request "other")
                 (let ((foreign (retain store ':continuation "other" :request "other")))
                   (setf (gethash "continuation_handle" projection) foreign)
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 1 :complete projection))
                            ':completion-mismatch))
                 (setf (gethash "continuation_handle" projection) continuation)
                 (let ((note (model-replay-store-accept
                              store :request-id "request" :attempt 1 :complete projection)))
                   (check (equal note (model-replay-store-accept
                                       store :request-id "request" :attempt 1 :complete projection)))
                   (rejects (lambda () (fail store (object "code" "internal"))) ':inactive-attempt)
                   (rejects (lambda () (resolve-state store turn-state ':turn-state)) ':invalid-handle)
                   (setf store (model-replay-store-open path "session"))
                   (model-replay-store-compact store)
                   (rejects (lambda () (resolve-state store turn-state ':turn-state)) ':invalid-handle)
                   (model-replay-store-mark-projected store :request-id "request" :attempt 1 :note-id note)
                   (check (same value (resolve-state store turn-state ':turn-state)))
                   (check (equal "private-response-id" (resolve-state store continuation ':continuation)))
                   (rejects (lambda () (resolve store continuation)) ':invalid-handle)
                   (rejects (lambda () (resolve-state store item ':continuation)) ':invalid-handle)
                   (rejects (lambda () (resolve-state store continuation ':turn-state)) ':invalid-handle)
                   (rejects (lambda () (resolve-state store turn-state ':continuation)) ':invalid-handle)
                   (loop for (session profile family) in '(("other" "profile" "responses")
                                                           ("session" "other" "responses")
                                                           ("session" "profile" "other"))
                         do (rejects (lambda () (model-replay-store-resolve-state
                                                 store turn-state :kind ':turn-state :session-id session
                                                 :profile-id profile :family family)) ':invalid-handle))
                   (setf (gethash "raw" value) "changed")
                   (check (equal "{broken" (gethash "raw" (resolve-state store turn-state ':turn-state))))
                   (let ((copy (resolve-state store turn-state ':turn-state)))
                     (setf (gethash "raw" copy) "changed"))
                   (model-replay-store-compact store)
                   (setf store (model-replay-store-open path "session"))
                   (check (equal "{broken" (gethash "raw" (resolve-state store turn-state ':turn-state))))
                   (check (equal "private-response-id" (resolve-state store continuation ':continuation)))
                   (check (same projection (model-replay-store-completion
                                            store :request-id "request" :attempt 1)))
                   (rejects (lambda () (fail store (object "code" "internal"))) ':inactive-attempt))))
             ;; Failed and interrupted state handles remain denied after retry acceptance.
             (dolist (status '(:failed :interrupted :reopened))
               (let* ((path (path)) (store (model-replay-store-open path "session")))
                 (start store)
                 (let ((handle (retain store ':continuation "private")))
                   (case status
                     (:failed
                      (model-replay-store-fail-attempt store :request-id "request" :attempt 1))
                     (:interrupted
                      (model-replay-store-interrupt-attempt store :request-id "request" :attempt 1))
                     (:reopened
                      (setf store (model-replay-store-open path "session"))))
                   (rejects (lambda () (resolve-state store handle ':continuation)) ':invalid-handle)
                   (start store :attempt 2)
                   (let ((projection (complete store :attempt 2)))
                     (setf (gethash "continuation_handle" projection) handle)
                     (rejects (lambda () (model-replay-store-accept
                                          store :request-id "request" :attempt 2 :complete projection))
                              ':completion-mismatch)
                     (remhash "continuation_handle" projection)
                     (let ((note (model-replay-store-accept
                                  store :request-id "request" :attempt 2 :complete projection)))
                       (model-replay-store-mark-projected store :request-id "request" :attempt 2 :note-id note)
                       (check (not (nth-value 1 (gethash "continuation_handle"
                                                        (model-replay-store-completion
                                                         store :request-id "request" :attempt 2)))))))
                   (model-replay-store-compact store)
                   (setf store (model-replay-store-open path "session"))
                   (rejects (lambda () (resolve-state store handle ':continuation)) ':invalid-handle))))
             ;; Invalid private values fail before any handle or event can be retained.
             (let ((store (model-replay-store-open (path) "session")))
               (start store)
               (dolist (bad (list 1/2 1.0f0 (expt 2 63) :not-portable
                                 (make-string 2097153 :initial-element #\x)))
                 (rejects (lambda () (retain store ':continuation bad))))
               (check (= 1 (length (model-replay-store-events store "request")))))
             ;; Terminal failure is one durable event from running or retry-waiting states.
             (dolist (status '(:running :failed :interrupted :reopened))
               (let* ((path (path)) (store (model-replay-store-open path "session"))
                      (failed (protocol-message->data
                               (parent-proto:make-failure :code    ':cancelled
                                                          :message "Request cancelled."))))
                 (start store)
                 (let ((item (put store)) (handle (retain store ':turn-state (object "private" t))))
                   (unless (eq status ':running)
                     (model-replay-store-append-event store :request-id "request" :attempt 1
                                                     :kind    "attempt_failed"
                                                     :payload (object "retry_allowed" t))
                     (case status
                       (:failed
                        (model-replay-store-fail-attempt store :request-id "request" :attempt 1))
                       (:interrupted
                        (model-replay-store-interrupt-attempt store :request-id "request" :attempt 1))
                       (:reopened
                        (setf store (model-replay-store-open path "session")))))
                   (let* ((count (length (model-replay-store-events store "request")))
                          (event (fail store failed)))
                     (check (= (1+ count) (gethash "index" event)))
                     (check (= 1 (gethash "attempt" event)))
                     (check (same failed (gethash "failed" event)))
                     (check (same event (protocol-message->data
                                         (parent-proto:make-model-event
                                          :index (1+ count) :attempt 1
                                          :failed (parent-proto:make-failure
                                                   :code ':cancelled :message "Request cancelled.")))))
                     (check (equal '(:terminal-failed 1)
                                   (multiple-value-list (model-replay-store-status store "request"))))
                     (check (same event (fail store failed)))
                     (check (= (1+ count) (length (model-replay-store-events store "request"))))
                     (setf (gethash "message" (gethash "failed" event)) "changed")
                     (check (same failed (gethash "failed" (fail store failed)))))
                   (rejects (lambda () (fail store (object "code" "internal"))) ':failure-conflict)
                   (rejects (lambda () (start store :attempt 2)) ':attempt-conflict)
                   (rejects (lambda () (put store)) ':inactive-attempt)
                   (rejects (lambda () (retain store ':continuation "private")) ':inactive-attempt)
                   (rejects (lambda () (model-replay-store-append-event
                                        store :request-id "request" :attempt 1 :kind "progress"
                                        :payload (object "phase" "late"))) ':inactive-attempt)
                   (rejects (lambda () (model-replay-store-accept
                                        store :request-id "request" :attempt 1 :complete (complete store)))
                            ':inactive-attempt)
                   (rejects (lambda () (resolve store item)) ':invalid-handle)
                   (rejects (lambda () (resolve-state store handle ':turn-state)) ':invalid-handle)
                   (model-replay-store-compact store)
                   (setf store (model-replay-store-open path "session"))
                   (check (same failed (gethash "failed" (fail store failed))))
                   (check (zerop (length (model-replay-store-pending-projections store))))
                   (rejects (lambda () (start store :attempt 2)) ':attempt-conflict)
                   (rejects (lambda () (resolve-state store handle ':turn-state)) ':invalid-handle))))
             ;; Independent caches share state handles, projection, and terminal failure.
             (let* ((path (path)) (store (model-replay-store-open path "session"))
                    (other (model-replay-store-open path "session")))
               (start store)
               (let* ((handle     (retain other ':turn-state "private-state"))
                      (projection (complete store)))
                 (check (equal handle (retain store ':turn-state "private-state")))
                 (setf (gethash "turn_state_handle" projection) handle)
                 (let ((note (model-replay-store-accept
                              other :request-id "request" :attempt 1 :complete projection)))
                   (model-replay-store-mark-projected store :request-id "request" :attempt 1 :note-id note)
                   (check (equal "private-state" (resolve-state other handle ':turn-state)))
                   (model-replay-store-compact other)
                   (check (equal "private-state" (resolve-state store handle ':turn-state)))))
               (start store :request "retry")
               (model-replay-store-append-event other :request-id "retry" :attempt 1
                                               :kind "attempt_failed" :payload (object "retryable" t))
               (model-replay-store-fail-attempt store :request-id "retry" :attempt 1)
               (start other :request "retry" :attempt 2)
               (let* ((failure (protocol-message->data
                                (parent-proto:make-failure :code      ':unavailable
                                                           :message   "Provider unavailable."
                                                           :retryable nil)))
                      (event   (fail store failure :request "retry" :attempt 2)))
                 (check (= 4 (gethash "index" event)))
                 (check (= 2 (gethash "attempt" event)))
                 (check (same event (fail other failure :request "retry" :attempt 2)))
                 (check (eq ':terminal-failed (model-replay-store-status other "retry")))
                 (model-replay-store-compact store)
                 (check (same event (fail other failure :request "retry" :attempt 2)))))
             ;; Invalid Failure fields cannot terminate a running request.
             (let ((store (model-replay-store-open (path) "session")))
               (start store)
               (dolist (invalid (list (object "code" "not-a-code") (object "code" 8)
                                      (object "message" nil) (object "retryable" "false")
                                      (object "retryable" 0) (object "private" "secret")))
                 (rejects (lambda () (fail store invalid)) ':invalid-event)
                 (check (eq ':running (model-replay-store-status store "request"))))
               (check (= 1 (length (model-replay-store-events store "request")))))
             checks)
        (dolist (path paths)
          (dolist (file (list path (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file file) (delete-file file))))))))
