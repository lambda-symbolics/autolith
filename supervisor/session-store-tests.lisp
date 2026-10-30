;;;; -- Standalone Session Storage Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () integer) run-session-store-tests))

(defun run-session-store-tests ()
  "Exercise durable sessions without loading the agent system; return check count."
  (let ((checks 0) (paths nil))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Session store check ~D failed." checks)))

             (rejects (function &optional code)
               (check (handler-case (progn (funcall function) nil)
                        (session-store-error (condition)
                          (or (null code) (eq code (session-store-error-code condition)))))))

             (path ()
               (let ((path (merge-pathnames
                            (format nil "autolith-session-check-~36R.sexp"
                                    (random (expt 36 20)))
                            (uiop:temporary-directory))))
                 (push path paths)
                 path))

             (map-value (&rest entries)
               (let ((map (make-hash-table :test 'equal)))
                 (loop for (key value) on entries by (lambda (tail) (rest (rest tail)))
                       do (setf (gethash key map) value))
                 (protocol-data->value map)))

             (append-request (event turn kind payload &key (sequence 0)
                                                          (origin ':origin-unspecified))
               (cl-protobufs.autolith.parent.v1:make-session-append
                :record (cl-protobufs.autolith.parent.v1:make-session-record
                         :sequence sequence :event-id event :turn-id turn :kind kind
                         :payload payload :origin origin)))

             (note-request (event turn text)
               (append-request event turn ':note (map-value "text" text)))

             (read-page (store &key (after 0) (limit 256) high (max-bytes 8388608))
               (session-store-read
                store (cl-protobufs.autolith.parent.v1:make-session-read
                       :after-sequence after :limit limit)
                :high-water-sequence high :max-bytes max-bytes))

             (records (page)
               (cl-protobufs.autolith.parent.v1:session-page.records page))

             (sequence-of (ack)
               (cl-protobufs.autolith.parent.v1:session-ack.sequence ack)))
      (unwind-protect
           (progn
             ;; Nested calls retain their own identities and an active parent chain.
             (let* ((path  (path))
                    (store (session-store-open path "nested-session")))
               (session-store-start-turn store "nested-turn" "input")
               (labels ((start (event call &key parent (source "worker"))
                          (append-request
                           event "nested-turn" ':tool-started
                           (if parent
                               (map-value "call_id" call "name" "fixture.effect"
                                          "arguments" (make-hash-table :test 'equal)
                                          "parent_call_id" parent "source" source)
                               (map-value "call_id" call "name" "fixture.effect"
                                          "arguments" (make-hash-table :test 'equal)))))

                        (finish (event call &key parent (source "worker"))
                          (append-request
                           event "nested-turn" ':tool-finished
                           (if parent
                               (map-value "call_id" call "result" "done" "is_error" nil
                                          "parent_call_id" parent "source" source)
                               (map-value "call_id" call "result" "done" "is_error" nil)))))
                 (session-store-append store (start "root-start" "root"))
                 (dolist (parent '("missing" "child"))
                   (rejects (lambda () (session-store-append store (start "bad-parent" "child" :parent parent)))
                            ':invalid-nested-parent))
                 (rejects (lambda () (session-store-append store (start "bad-source" "child" :parent "root" :source "model")))
                          ':invalid-nested-call)
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "missing-source" "nested-turn" ':tool-started
                                                   (map-value "call_id" "child" "name" "fixture.effect"
                                                              "arguments" (make-hash-table :test 'equal)
                                                              "parent_call_id" "root"))))
                          ':invalid-nested-call)
                 (let* ((request (start "child-start" "child" :parent "root"))
                        (ack     (session-store-append store request)))
                   (check (= (sequence-of ack) (sequence-of (session-store-append store request))))
                   (rejects (lambda () (session-store-append store (start "duplicate-start" "child" :parent "root")))
                            ':call-conflict)
                   (rejects (lambda () (session-store-append store (start "child-start" "other-child" :parent "root")))
                            ':event-conflict))
                 (dolist (request (list (finish "missing-identity" "child")
                                       (finish "wrong-parent" "child" :parent "other")
                                       (finish "wrong-source" "child" :parent "root" :source "task-inspector")
                                       (finish "missing-start" "other" :parent "root")))
                   (rejects (lambda () (session-store-append store request)) ':unmatched-tool-finish))
                 (let* ((request (finish "child-finish" "child" :parent "root"))
                        (ack     (session-store-append store request)))
                   (check (= (sequence-of ack) (sequence-of (session-store-append store request)))))
                 (rejects (lambda () (session-store-append store (finish "duplicate-finish" "child" :parent "root")))
                          ':unmatched-tool-finish)
                 (rejects (lambda () (session-store-append store (start "closed-parent" "grandchild" :parent "child")))
                          ':invalid-nested-parent)
                 (loop for depth from 1 below 32
                       for parent = "root" then call
                       for call = (format nil "depth-~D" depth)
                       do (session-store-append store (start (format nil "start-~D" depth) call :parent parent)))
                 (rejects (lambda () (session-store-append store (start "depth-overflow" "too-deep" :parent "depth-31")))
                          ':invalid-nested-parent)
                 ;; Cold replay retains the same parent links and completed state.
                 (setf store (session-store-open path "nested-session" :recover-interrupted nil))
                 (check (= 35 (length (records (read-page store)))))
                 (rejects (lambda () (session-store-append store (start "cold-duplicate" "child" :parent "root")))
                          ':call-conflict)
                 (session-store-finish-turn store "nested-turn" ':completed)
                 (session-store-start-turn store "later-turn" "next")
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "cross-turn" "later-turn" ':tool-started
                                                   (map-value "call_id" "later-child" "name" "fixture.effect"
                                                              "arguments" (make-hash-table :test 'equal)
                                                              "parent_call_id" "root" "source" "worker"))))
                          ':invalid-nested-parent)))
             ;; Check the inclusive per-turn call bound without writing 65536 records.
             (let ((calls   (make-hash-table :test 'equal))
                   (payload (session-store--value-data
                             (map-value "call_id" "next" "name" "fixture.effect"
                                        "arguments" (make-hash-table :test 'equal)))))
               (dotimes (index 65535)
                 (setf (gethash (format nil "call-~D" index) calls) (list "bounded" ':started index)))
               (check (null (session-store--call-check calls "bounded" ':tool-started :payload payload)))
               (setf (gethash "last" calls) (list "bounded" ':started 65536))
               (rejects (lambda () (session-store--call-check calls "bounded" ':tool-started :payload payload))
                        ':call-limit))
             ;; Parent model results have a separate trusted shape and retry identity.
             (let* ((path      (path))
                    (store     (session-store-open path "model-session"))
                    (complete  (cl-protobufs.autolith.parent.v1:make-model-complete
                                :reason ':stop :response-id "response-A"
                                :usage (cl-protobufs.autolith.parent.v1:make-usage
                                        :input-tokens 0 :output-tokens 12)
                                :output-items
                                (list (cl-protobufs.autolith.parent.v1:make-output-item
                                       :replay-handle "opaque-A" :family "responses" :item-index 0)
                                      (cl-protobufs.autolith.parent.v1:make-output-item
                                       :replay-handle "opaque-B" :family "responses" :item-index 1))))
                    (projected (session-store-model-complete-data complete)))
               (session-store-start-turn store "model-turn" "input")
               (check (equal "stop" (gethash "reason" projected)))
               (check (= 0 (gethash "input_tokens" (gethash "usage" projected))))
               (check (equal "opaque-B"
                             (gethash "replay_handle" (aref (gethash "output_items" projected) 1))))
               (check (not (nth-value 1 (gethash "continuation_handle" projected))))
               (check (= 2 (sequence-of (session-store-model-result
                                         store :event-id "model-result/stable"
                                         :turn-id "model-turn" :request-id 42 :attempt 1
                                         :complete       complete))))
               (let* ((record  (second (records (read-page store))))
                      (payload (protocol-value->data
                                (cl-protobufs.autolith.parent.v1:session-record.payload record))))
                 (check (eq ':parent (cl-protobufs.autolith.parent.v1:session-record.origin record)))
                 (check (eq ':note (cl-protobufs.autolith.parent.v1:session-record.kind record)))
                 (check (equal "42" (gethash "request_id" payload)))
                 (check (equal "model-turn" (gethash "turn_id" payload)))
                 (check (session-store--exact-p
                         (session-store--value-data (protocol-data->value projected))
                         (session-store--value-data (protocol-data->value (gethash "model_result" payload)))))
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "spoof-model" "model-turn" ':note
                                                   (protocol-data->value payload))))
                          ':invalid-fields)
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "spoof-parent" "model-turn" ':note
                                                   (protocol-data->value payload) :origin ':parent)))
                          ':forged-metadata)
                 (setf (gethash "response_id" (gethash "model_result" payload)) "changed")
                 (check (equal "response-A"
                               (gethash "response_id"
                                        (gethash "model_result"
                                                 (protocol-value->data
                                                  (cl-protobufs.autolith.parent.v1:session-record.payload
                                                   (second (records (read-page store))))))))))
               (session-store-finish-turn store "model-turn" ':completed)
               (setf store (session-store-open path "model-session"))
               (check (= 2 (sequence-of (session-store-model-result
                                         store :event-id "model-result/stable"
                                         :turn-id "model-turn" :request-id "42"
                                         :complete       projected))))
               (dolist (change '(:event :attempt :content))
                 (rejects (lambda ()
                            (session-store-model-result
                             store :event-id (if (eq change ':event) "different" "model-result/stable")
                             :turn-id "model-turn" :request-id "42"
                             :attempt        (if (eq change ':attempt) 2 1)
                             :complete       (if (eq change ':content)
                                                 (cl-protobufs.autolith.parent.v1:make-model-complete :reason ':filtered)
                                                 projected)))
                          ':model-result-conflict))
               (rejects (lambda () (session-store-model-result
                                    store :event-id "new-model" :turn-id "model-turn"
                                    :request-id "43" :complete projected))
                        ':inactive-turn)
               (check (= 3 (length (records (read-page store))))))
             ;; Verified outbox recovery can publish after turn closure.
             (dolist (closure '(:interrupted :completed))
               (let* ((path     (path))
                      (store    (session-store-open path "late-model-session"))
                      (complete (cl-protobufs.autolith.parent.v1:make-model-complete
                                 :reason ':stop)))
                 (session-store-start-turn store "old-turn" "input")
                 (if (eq closure ':interrupted)
                     (setf store (session-store-open path "late-model-session"))
                     (session-store-finish-turn store "old-turn" ':completed))
                 (rejects (lambda ()
                            (session-store-model-result
                             store :event-id "late-result" :turn-id "old-turn"
                             :request-id "late-request" :complete complete))
                          ':inactive-turn)
                 (rejects (lambda ()
                            (session-store--recover-model-result
                             store :event-id "missing-result" :turn-id "missing-turn"
                             :request-id "missing-request" :complete complete))
                          ':inactive-turn)
                 (check (= 3 (sequence-of
                              (session-store--recover-model-result
                               store :event-id "late-result" :turn-id "old-turn"
                                :request-id "late-request" :complete complete
                                :request-kind ':generate :request-purpose ':ordinary))))
                 ;; A late note must not reopen the old turn.
                 (session-store-start-turn store "new-turn" "next input")
                 (rejects (lambda ()
                            (session-store-model-result
                             store :event-id "other-result" :turn-id "old-turn"
                             :request-id "other-request" :complete complete))
                          ':inactive-turn)
                 (check (= 5 (sequence-of
                              (session-store--recover-model-result
                               store :event-id "other-result" :turn-id "old-turn"
                               :request-id "other-request" :complete complete))))
                 (setf store (session-store-open path "late-model-session"
                                                 :recover-interrupted nil))
                 (check (= 5 (length (records (read-page store)))))
                 (check (= 3 (sequence-of
                              (session-store--recover-model-result
                               store :event-id "late-result" :turn-id "old-turn"
                                :request-id "late-request" :complete complete
                                :request-kind ':generate :request-purpose ':ordinary))))
                 (check (= 3 (sequence-of
                               (session-store-model-result
                                store :event-id "late-result" :turn-id "old-turn"
                                :request-id "late-request" :complete complete))))
                 (dolist (change '(:event :attempt :content))
                   (rejects (lambda ()
                              (session-store--recover-model-result
                               store :event-id (if (eq change ':event)
                                                   "changed" "late-result")
                               :turn-id "old-turn" :request-id "late-request"
                               :attempt        (if (eq change ':attempt) 2 1)
                               :complete       (if (eq change ':content)
                                                   (cl-protobufs.autolith.parent.v1:make-model-complete
                                                    :reason ':filtered)
                                                   complete)))
                            ':model-result-conflict))
                 ;; Cold replay and recovery must preserve the different active turn.
                 (check (= 6 (sequence-of
                              (session-store-append
                               store (note-request "new-note" "new-turn" "active")))))
                 (check (= 7 (session-store-finish-turn store "new-turn" ':completed)))
                 (setf store (session-store-open path "late-model-session"))
                 (check (= 7 (length (records (read-page store)))))))
             ;; Input, metadata, strict payload contracts, and event retry after restart.
             (let* ((path (path)) (store (session-store-open path "session-a"))
                    (start   (session-store-start-turn store "turn-a" "user input"))
                    (request (note-request "event-a" "turn-a" "agent note")))
               (check (equal (session-store-id store) "session-a"))
               (check (= 1 (cl-protobufs.autolith.parent.v1:turn-start.input-sequence start)))
               (check (eq ':parent (cl-protobufs.autolith.parent.v1:session-record.origin
                                    (first (records (read-page store))))))
               (check (= 2 (sequence-of (session-store-append store request))))
               (check (= 2 (sequence-of (session-store-append store request))))
               (rejects (lambda () (session-store-append
                                    store (note-request "event-a" "turn-a" "changed")))
                        ':event-conflict)
               (rejects (lambda () (session-store-start-turn store "turn-b" "input"))
                        ':turn-conflict)
               (dolist (kind '(:user-input :turn-outcome :kind-unspecified))
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "forged" "turn-a" kind
                                                   (map-value "text" "x"))))
                          ':invalid-record))
               (dolist (origin '(:parent :agent))
                 (rejects (lambda ()
                            (session-store-append
                             store (append-request "forged" "turn-a" ':note
                                                   (map-value "text" "x") :origin origin)))
                          ':forged-metadata))
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "forged" "turn-a" ':note
                                                 (map-value "text" "x") :sequence 5)))
                        ':forged-metadata)
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "bad-fields" "turn-a" ':note
                                                 (map-value "text" "x" "approval" t))))
                        ':invalid-fields)
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "bad-type" "turn-a" ':note
                                                 (map-value "text" 9))))
                        ':invalid-field-type)
               (rejects (lambda ()
                          (session-store-append
                           store (note-request
                                  (cl-protobufs.autolith.parent.v1:session-record.event-id
                                   (first (records (read-page store))))
                                  "turn-a" "x")))
                        ':event-conflict)
               (rejects (lambda () (session-store-append
                                    store (note-request "inactive" "other-turn" "x"))))
               (check (= 3 (session-store-finish-turn store "turn-a" ':completed)))
               (setf store (session-store-open path "session-a"))
               (check (= 2 (sequence-of (session-store-append store request))))
               (rejects (lambda () (session-store-append
                                    store (note-request "event-a" "turn-a" "changed")))
                        ':event-conflict)
               (rejects (lambda () (session-store-start-turn store "turn-a" "reuse")))
               (rejects (lambda () (session-store-open path "wrong-session")) ':corrupt-header)
               (let ((start (session-store-start-turn store "turn-b" "continue"
                                                     :continuation-sequence 2)))
                 (check (cl-protobufs.autolith.parent.v1:turn-start.continuation start))
                 (check (= 2 (cl-protobufs.autolith.parent.v1:turn-start.input-sequence start))))
               (session-store-finish-turn store "turn-b" ':cancelled :reason "user stopped")
               (check (= 5 (length (records (read-page store))))))
              ;; Exact retry identity includes string case, Value type, and signed zero.
             (let ((bytes (make-array 2 :element-type    '(unsigned-byte 8)
                                       :initial-contents '(1 2))))
                (dolist (entry
                         (append
                          (list
                           (list ':note (map-value "text" "Text") (map-value "text" "text"))
                           (list ':tool-started
                                 (map-value "call_id" "Call" "name" "write"
                                            "arguments" (make-hash-table :test 'equal))
                                 (map-value "call_id" "call" "name" "write"
                                            "arguments" (make-hash-table :test 'equal)))
                           (list ':tool-started
                                 (map-value "call_id" "call" "name" "Write"
                                            "arguments" (make-hash-table :test 'equal))
                                 (map-value "call_id" "call" "name" "write"
                                            "arguments" (make-hash-table :test 'equal))))
                          (loop for before in (list "Value" 1 bytes 0.0d0)
                                for after in (list "value" 1.0d0 #(1 2) -0.0d0)
                                collect
                                (list ':tool-started
                                      (map-value "call_id" "call" "name" "write"
                                                 "arguments" (protocol-value->data
                                                              (map-value "nested" (vector before))))
                                      (map-value "call_id" "call" "name" "write"
                                                 "arguments" (protocol-value->data
                                                              (map-value "nested" (vector after))))))))
                  (destructuring-bind (kind before after) entry
                    (let* ((path (path)) (store (session-store-open path "exact"))
                           (request (append-request "event" "turn" kind before))
                           (changed (append-request "event" "turn" kind after)))
                      (session-store-start-turn store "turn" "input")
                      (check (= 2 (sequence-of (session-store-append store request))))
                      (rejects (lambda () (session-store-append store changed)) ':event-conflict)
                      (session-store-finish-turn store "turn" ':completed)
                      (check (= 2 (sequence-of (session-store-append store request))))
                      (rejects (lambda () (session-store-append store changed)) ':event-conflict)
                      (setf store (session-store-open path "exact"))
                      (check (= 2 (sequence-of (session-store-append store request))))
                      (rejects (lambda () (session-store-append store changed)) ':event-conflict)))))
             (let* ((path (path)) (store (session-store-open path "result-case"))
                    (request (append-request "finish" "turn" ':tool-finished
                                             (map-value "call_id" "call" "result" "Result"
                                                        "is_error" nil))))
               (session-store-start-turn store "turn" "input")
               (session-store-append store
                                     (append-request "start" "turn" ':tool-started
                                                     (map-value "call_id" "call" "name" "write"
                                                                "arguments" (make-hash-table :test 'equal))))
               (session-store-append store request)
               (session-store-finish-turn store "turn" ':completed)
               (setf store (session-store-open path "result-case"))
               (check (= 3 (sequence-of (session-store-append store request))))
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "finish" "turn" ':tool-finished
                                                 (map-value "call_id" "call" "result" "result"
                                                            "is_error" nil))))
                        ':event-conflict))
             ;; Matched tool calls, durable unknown effects, no automatic retry.
             (let* ((path (path)) (store (session-store-open path "tools"))
                    (start  (append-request "start" "turn" ':tool-started
                                            (map-value "call_id" "call" "name" "write"
                                                       "arguments" (make-hash-table :test 'equal))))
                    (finish (append-request "finish" "turn" ':tool-finished
                                            (map-value "call_id" "call" "result" "done"
                                                       "is_error" nil))))
               (session-store-start-turn store "turn" "input")
               (rejects (lambda () (session-store-append store finish)) ':unmatched-tool-finish)
               (session-store-append store start)
               (check (= 1 (length (session-store-pending-tools store))))
               (session-store-append store finish)
               (check (null (session-store-pending-tools store)))
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "start-again" "turn" ':tool-started
                                                 (map-value "call_id" "call" "name" "write"
                                                            "arguments" (make-hash-table :test 'equal)))))
                        ':call-conflict)
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "finish-again" "turn" ':tool-finished
                                                 (map-value "call_id" "call" "result" "x"
                                                            "is_error" t))))
                        ':unmatched-tool-finish)
               (session-store-append
                store (append-request "unknown-start" "turn" ':tool-started
                                      (map-value "call_id" "unknown" "name" "send"
                                                 "arguments" (make-hash-table :test 'equal))))
               (setf store (session-store-open path "tools"))
               (let* ((pending (first (session-store-pending-tools store)))
                      (page (read-page store)) (last (first (last (records page)))))
                 (check (equal "unknown" (getf pending :call-id)))
                 (check (eq ':unknown (getf pending :effects)))
                 (check (eq ':turn-outcome (cl-protobufs.autolith.parent.v1:session-record.kind last)))
                 (check (eq ':parent (cl-protobufs.autolith.parent.v1:session-record.origin last)))
                 (check (equal "interrupted"
                               (gethash "outcome" (protocol-value->data
                                                   (cl-protobufs.autolith.parent.v1:session-record.payload last)))))
                 (check (= 5 (length (records page)))))
               (check (= 2 (sequence-of (session-store-append store start))))
               (check (= 3 (sequence-of (session-store-append store finish))))
               (check (null (session-store-interrupt store)))
               (session-store-start-turn store "next" "next input")
               (rejects (lambda ()
                          (session-store-append
                           store (append-request "late-finish" "next" ':tool-finished
                                                 (map-value "call_id" "unknown" "result" "x"
                                                            "is_error" nil))))
                        ':unmatched-tool-finish))
             ;; Snapshot pages exclude later appends and shrink to the byte budget.
             (let ((store (session-store-open (path) "pages")))
               (session-store-start-turn store "turn" "input")
               (dotimes (index 5)
                 (session-store-append store
                                       (note-request (format nil "event-~D" index) "turn"
                                                     (make-string 100 :initial-element #\x))))
               (let* ((first (read-page store :limit 2))
                      (high  (cl-protobufs.autolith.parent.v1:session-page.high-water-sequence first)))
                 (check (= 6 high))
                 (check (cl-protobufs.autolith.parent.v1:session-page.has-more first))
                 (session-store-append store (note-request "later" "turn" "later input"))
                 (let ((second (read-page store :after 2 :high high)))
                   (check (= 4 (length (records second))))
                   (check (not (cl-protobufs.autolith.parent.v1:session-page.has-more second)))
                   (check (= 6 (cl-protobufs.autolith.parent.v1:session-record.sequence
                                (first (last (records second)))))))
                 (let* ((one   (read-page store :after 1 :limit 1 :high high))
                        (size  (length (cl-protobufs:serialize-to-bytes one)))
                        (short (read-page store :after 1 :high high :max-bytes size)))
                   (check (= 1 (length (records short))))
                   (check (cl-protobufs.autolith.parent.v1:session-page.has-more short))
                   (rejects (lambda () (read-page store :after 1 :high high
                                                       :max-bytes (1- size)))
                            ':resource-limit)))
               (rejects (lambda () (read-page store :limit 0)) ':resource-limit)
               (rejects (lambda () (read-page store :limit 257)) ':resource-limit))
             ;; Recover only the incomplete final form. Reject prior corruption.
             (let* ((path (path)) (store (session-store-open path "tail")))
               (session-store-start-turn store "turn" "input")
               (session-store-append store (note-request "note" "turn" "before crash"))
               (with-open-file (stream path :direction :output :if-exists :append)
                 (write-string "(:session-entry :version 1 :record (" stream))
               (setf store (session-store-open path "tail"))
               (check (= 3 (length (records (read-page store)))))
               (check (not (nth-value 1 (sexp-store:log-read path))))
               (session-store-start-turn store "new" "after crash")
               (check (= 4 (length (records (read-page store))))))
             (dolist (damage '("(:bad :version 1)" ")" "#.(error \"unsafe\")"))
               (let ((path (path)))
                 (session-store-open path "corrupt")
                 (with-open-file (stream path :direction :output :if-exists :append)
                   (write-string damage stream)
                   (terpri stream))
                 (rejects (lambda () (session-store-open path "corrupt")))))
             (let* ((path (path)) (store (session-store-open path "sequence-corruption")))
               (session-store-start-turn store "turn" "input")
               (let* ((forms (sexp-store:log-read path))
                      (entry (second forms)))
                 (setf (first (getf (rest entry) :record)) 9)
                 (sexp-store:log-write path forms))
               (with-open-file (stream path :direction :output :if-exists :append)
                 (write-string "(:incomplete" stream))
               (rejects (lambda () (session-store-open path "sequence-corruption"))
                        ':corrupt-log)
               (check (nth-value 1 (sexp-store:log-read path))))
             ;; Parent IDs cannot collide with agent-selected future IDs.
             (let ((store (session-store-open (path) "event-collision")))
               (session-store-start-turn store "turn" "input")
               (session-store-append store (note-request "parent/3/0" "turn" "agent note"))
               (session-store-finish-turn store "turn" ':completed)
               (let* ((records (records (read-page store)))
                      (events  (mapcar #'cl-protobufs.autolith.parent.v1:session-record.event-id
                                       records)))
                 (check (= (length events) (length (remove-duplicates events :test #'equal))))))
              ;; Warm handles observe replacement, shrink, and repaired tails.
             (let* ((path (path)) (store (session-store-open path "cache")))
               (session-store-start-turn store "turn" "input")
               (session-store-append store (note-request "old" "turn" "old"))
               (read-page store)
               (let ((record (second (records (read-page store)))))
                 (setf (char (cl-protobufs.autolith.parent.v1:session-record.event-id record) 0) #\X
                       (char (cl-protobufs.autolith.parent.v1:session-record.turn-id record) 0) #\X)
                 (check (= 2 (sequence-of (session-store-append
                                           store (note-request "old" "turn" "old"))))))
               (let ((forms (sexp-store:log-read path))
                     (size  (ls-compat.posix:file-information-size
                             (ls-compat.posix:file-information path))))
                (setf (fifth (getf (rest (third forms)) :record))
                      (session-store--value-data (map-value "text" "new")))
                (sexp-store:log-write path forms)
                (check (= size (ls-compat.posix:file-information-size
                                (ls-compat.posix:file-information path))))
                (check (equal "new" (gethash "text" (protocol-value->data
                                                     (cl-protobufs.autolith.parent.v1:session-record.payload
                                                      (second (records (read-page store))))))))
                (with-open-file (stream path :direction :output :if-exists :supersede)
                  (dolist (form (subseq forms 0 2)) (prin1 form stream) (terpri stream)))
                (check (= 1 (length (records (read-page store)))))
                (with-open-file (stream path :direction :output :if-exists :append)
                  (write-string "(:incomplete" stream))
                (check (= 1 (length (records (read-page store)))))
                (check (not (nth-value 1 (sexp-store:log-read path))))
                (check (= 2 (sequence-of (session-store-append
                                          store (note-request "new" "turn" "after repair")))))))
              ;; Thousands of appends use incremental replay; cold open validates all entries.
             (let* ((path (path)) (store (session-store-open path "large")))
               (session-store-start-turn store "turn" "input")
               (dotimes (index 3000)
                 (session-store-append store (note-request (format nil "event-~D" index)
                                                          "turn" "large log")))
               (check (= 3001 (cl-protobufs.autolith.parent.v1:session-page.high-water-sequence
                               (read-page store :after 3000))))
               (setf store (session-store-open path "large" :recover-interrupted nil))
               (check (= 3001 (cl-protobufs.autolith.parent.v1:session-page.high-water-sequence
                               (read-page store :after 3000))))
               (check (= 3001 (sequence-of (session-store-append
                                            store (note-request "event-2999" "turn" "large log"))))))
             ;; The shared file lock serializes concurrent handles and threads.
             (let* ((path (path)) (store (session-store-open path "writers"))
                    (other (session-store-open path "writers" :recover-interrupted nil)))
               (session-store-start-turn store "turn" "input")
               (let ((threads
                       (loop for index below 4
                             collect (let ((worker-index index))
                                       (bordeaux-threads:make-thread
                                        (lambda ()
                                          (handler-case
                                              (dotimes (index 50 t)
                                                (session-store-append
                                                 (if (evenp worker-index) store other)
                                                 (note-request
                                                  (format nil "writer-~D-~D" worker-index index)
                                                  "turn" "concurrent note")))
                                            (error () nil))))))))
                  (push (bordeaux-threads:make-thread
                         (lambda ()
                           (handler-case
                               (loop repeat 100
                                     for page = (read-page other)
                                     always (loop for record in (records page)
                                                  for sequence from 1
                                                  always (= sequence
                                                            (cl-protobufs.autolith.parent.v1:session-record.sequence record))))
                             (error () nil))))
                        threads)
                 (dolist (thread threads) (check (bordeaux-threads:join-thread thread))))
               (let ((records (records (read-page store))))
                  (check (= 201 (length records)))
                 (check (loop for record in records for sequence from 1
                              always (= sequence
                                        (cl-protobufs.autolith.parent.v1:session-record.sequence record))))))
             checks)
        (dolist (path paths)
          (dolist (file (list path (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file file) (delete-file file))))))))
