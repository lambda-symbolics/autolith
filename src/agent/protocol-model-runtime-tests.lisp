;;;; -- Agent and Parent Model Runtime Checks --

(in-package #:autolith)

(-> agent-model-runtime-test--complete (agent-protocol-client integer) agent-wire:model-complete)
(defun agent-model-runtime-test--complete (client request-id)
  "Read one real model stream and require its original request correlation."
  (loop for envelope = (agent-protocol-receive client)
        do (assert (= request-id (agent-wire:envelope.reply-to envelope)))
           (assert (eq 'agent-wire:model-event (agent-wire:envelope.body-case envelope)))
           (let ((event (agent-wire:envelope.model-event envelope)))
             (assert (not (agent-wire:model-event.has-failed event)))
             (when (agent-wire:model-event.has-complete event)
               (return (agent-wire:model-event.complete event))))))

(-> run-agent-model-runtime-tests () null)
(defun run-agent-model-runtime-tests ()
  "Join the real client, dispatcher, collector and journals over serialized bytes."
  (uiop:with-temporary-file (:pathname session-path :stream session-file)
    (uiop:with-temporary-file (:pathname replay-path :stream replay-file)
      (close session-file)
      (close replay-file)
      (delete-file session-path)
      (delete-file replay-path)
      (unwind-protect
           (agent-protocol-test--peers
            (lambda (parent)
              (let ((welcome (agent-model-test--welcome)))
                (setf (agent-wire:limits.max-frame-bytes (agent-wire:welcome.limits welcome)) (* 1024 1024))
                (autolith-supervisor::parent-connection-handshake parent welcome))
              (let* ((store    (autolith-supervisor::session-store-open session-path "client-session"))
                     (replay   (autolith-supervisor::model-replay-store-open replay-path "client-session"))
                     (profile  (autolith-supervisor::make-model-profile
                                :id "profile" :model "parent-model" :family ':responses))
                     (calls    0)
                     (original (let ((item (make-hash-table :test 'equal)))
                                 (setf (gethash "type" item) "function_call"
                                       (gethash "call_id" item) "call"
                                       (gethash "name" item) "run"
                                       (gethash "arguments" item) "{malformed")
                                 item))
                     (service
                       (autolith-supervisor::make-model-service
                        :profiles     (autolith-supervisor::make-model-profile-registry (list profile))
                        :replay-store replay
                        :abort        (lambda (invocation) (declare (ignore invocation)))
                        :executor
                        (lambda (invocation callback)
                          (incf calls)
                          (when (= calls 2)
                            (let* ((request (first (autolith-supervisor::model-invocation-inputs invocation)))
                                   (items   (cl-llm-provider-api:wire-request-items request)))
                              (assert (equalp original (first items)))
                              (assert (string= "{malformed" (gethash "arguments" (first items))))))
                          (funcall callback (make-instance 'cl-llm-provider-api:provider-item-event :item original))
                          (make-instance 'cl-llm-provider-api:provider-result :output-items (list original)))))
                     (runtime  (autolith-supervisor::session-runtime-open
                                store parent :features '("session.v1" "model.v1" "model-items-v1")
                                :model-service         service)))
                (unwind-protect
                     (progn
                       (autolith-supervisor::session-runtime-start-turn runtime "turn" "input")
                       ;; Two model calls, then a history read and the correlated turn finish.
                       (dotimes (index 4)
                         (autolith-supervisor::session-runtime-receive runtime))
                       (autolith-supervisor::session-runtime--operation
                        service runtime
                        (lambda ()
                          (assert (= calls 2))
                          (assert (null (autolith-supervisor::session-runtime-turn-id runtime)))))
                       (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 5000)
                       (autolith-supervisor::session-runtime-receive runtime)
                       (autolith-supervisor::session-runtime--operation
                        service runtime
                        (lambda ()
                          (assert (eq ':stopped (autolith-supervisor::session-runtime-phase runtime))))))
                  (autolith-supervisor::session-runtime-disconnect runtime)
                  (autolith-supervisor::session-runtime-await-close runtime :timeout 5))))
            (lambda (client)
              (agent-protocol-handshake client :features '("session.v1" "model.v1" "model-items-v1"))
              (agent-protocol-receive client)
              (let* ((*context-contributors*          nil)
                     (*context-request-contributions* nil)
                     (configuration                   (make-configuration))
                     (conversation                    (make-instance 'conversation :identifier "client-session"
                                                                     :input-items              (list (json-object "role" "user" "content" "input"))))
                     (projection                      (agent-context-project configuration conversation #()
                                                                             :profile-id "profile" :turn-id "turn"))
                     (first-id                        (agent-protocol-model-start client (agent-context-projection-start projection)))
                     (first-result                    (agent-model-runtime-test--complete client first-id))
                     (item                            (first (agent-wire:model-complete.output-items first-result)))
                     (handle                          (agent-wire:output-item.replay-handle item))
                     (outputs                         (agent-context-accept projection first-result)))
                (assert (plusp (length handle)))
                (assert (string= "{malformed" (agent-wire:tool-call.arguments-json
                                               (agent-wire:output-item.tool-call item))))
                (let* ((next-conversation (make-instance 'conversation :identifier "client-session" :input-items outputs))
                       (next-projection   (agent-context-project configuration next-conversation #()
                                                               :profile-id "profile" :turn-id "turn"))
                       (second-id         (agent-protocol-model-start client (agent-context-projection-start next-projection)))
                       (second-result     (agent-model-runtime-test--complete client second-id)))
                  (assert (not (string= handle (agent-wire:output-item.replay-handle
                                                (first (agent-wire:model-complete.output-items second-result))))))
                  (assert (= 1 (length (agent-context-accept next-projection second-result))))))
              (let ((read-id (agent-protocol-session-read client)))
                (let* ((envelope (agent-protocol-receive client))
                       (records  (agent-wire:session-page.records (agent-wire:envelope.session-page envelope))))
                  (assert (= read-id (agent-wire:envelope.reply-to envelope)))
                  (assert (= 3 (length records)))
                  (assert (every (lambda (record) (eq ':parent (agent-wire:session-record.origin record))) records))))
              (agent-protocol-finish-turn client ':completed)
              (assert (eq 'agent-wire:prepare-stop (agent-wire:envelope.body-case (agent-protocol-receive client))))
              (agent-protocol-ready-to-stop client)))
        (dolist (path (list session-path replay-path))
          (when (probe-file path) (delete-file path))
          (let ((lock (pathname (concatenate 'string (namestring path) ".lock"))))
            (when (probe-file lock) (delete-file lock)))))))
  nil)
