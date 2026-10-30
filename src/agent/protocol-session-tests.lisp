;;;; -- Parent-backed Session Checks --

(in-package #:autolith)

(-> agent-session-test--welcome () agent-wire:welcome)
(defun agent-session-test--welcome ()
  "Build a session Welcome with enough space for structured history."
  (agent-wire:make-welcome
   :session-id "client-session" :features '("session.v1")
   :limits (agent-wire:make-limits :max-frame-bytes 8388608 :max-value-depth 32
                                  :max-collection-entries 65536 :max-in-flight-requests 64)
   :resume (agent-wire:make-session-page)))

(-> agent-session-test--exchange (pathname function function) null)
(defun agent-session-test--exchange (path server client)
  "Run the actual serialized session runtime and bridge with bounded cleanup."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-session-test--welcome))
     (let* ((store   (autolith-supervisor::session-store-open path "client-session"))
            (runtime (autolith-supervisor::session-runtime-open store parent)))
       (unwind-protect
            (funcall server runtime)
         (autolith-supervisor::session-runtime-disconnect runtime)
         (autolith-supervisor::session-runtime-await-close runtime :timeout 3))))
   (lambda (peer)
     (agent-protocol-handshake peer)
     (funcall client peer)))
  nil)

(-> agent-session-test--start (agent-protocol-client function) agent-protocol-session)
(defun agent-session-test--start (client runner)
  "Build a bridge with an explicit turn runner and bind its observer after driver creation."
  (agent-protocol-session-start
   client (make-configuration) "profile"
   :worker ':fixture :tool-registry (make-instance 'tool-registry)
   :observer-factory
   (lambda (driver session)
     (assert (eq driver (agent-protocol-session-driver session)))
     (assert (null (agent-protocol-session-worker session)))
     (assert (null (agent-protocol-session-agent session)))
     (make-instance 'agent-observer))
   :run-turn runner))

(-> agent-session-test--until-finish (t) null)
(defun agent-session-test--until-finish (runtime)
  "Dispatch serialized requests until the peer finishes its active turn."
  (loop while (autolith-supervisor::session-runtime-turn-id runtime)
        do (autolith-supervisor::session-runtime-receive runtime))
  nil)

(-> agent-session-test--stop (t) null)
(defun agent-session-test--stop (runtime)
  "Request orderly stop and dispatch its correlated ready response."
  (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 3000)
  (loop until (member (autolith-supervisor::session-runtime-phase runtime) '(:stopped :closed))
        do (autolith-supervisor::session-runtime-receive runtime))
  nil)

(-> agent-session-test--cleanup (pathname) null)
(defun agent-session-test--cleanup (path)
  "Delete test-owned journal files only after peers and runtime workers joined."
  (dolist (file (list path (autolith-supervisor::session-store-lock-pathname
                            (autolith-supervisor::session-store-open path "client-session"))))
    (when (probe-file file) (delete-file file)))
  nil)

(-> agent-session-test--codec () null)
(defun agent-session-test--codec ()
  "Round-trip portable values and reject unknown symbols without interning them."
  (let ((value (list :provider :metadata (list :usage '(("tokens" 3)) :ready nil :done t)
                     :items    (vector "raw {" 4 1.25d0))))
    (assert (equalp value (agent-session-value-decode (agent-session-value-encode value)))))
  (let ((name "SESSION-UNTRUSTED-KEYWORD-NEVER-INTERN"))
    (assert (not (find-symbol name '#:keyword)))
    (assert (handler-case (progn (agent-session-value-decode (vector "keyword" name)) nil)
              (agent-protocol-error () t)))
    (assert (not (find-symbol name '#:keyword))))
  nil)

(-> agent-session-test--persist-and-resume () null)
(defun agent-session-test--persist-and-resume ()
  "Resume parent history in a new agent object with compaction and full editable history."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (unwind-protect
         (progn
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "first" "first input")
              (agent-session-test--until-finish runtime)
              (agent-session-test--stop runtime))
            (lambda (client)
              (let ((session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (declare (ignore observer))
                                (let ((conversation (agent-protocol-session-conversation session)))
                                  (assert (equal '("first input") (conversation-input-history conversation)))
                                  (conversation-append-provider-item
                                   conversation (json-object "type" "message" "role" "assistant"
                                                             "content" (vector (json-object "type" "output_text" "text" "answer"))))
                                  (conversation-append-summary conversation "retained summary"))))))
                (unwind-protect
                     (progn
                       (agent-protocol-session-join session :timeout 5)
                       (assert (agent-protocol-session-stopped session))
                       (assert (null (agent-protocol-session-failure session))))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3)))))
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "second" "second input")
              (agent-session-test--until-finish runtime)
              (agent-session-test--stop runtime))
            (lambda (client)
              (let ((ran     nil)
                    (session nil))
                (setf session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (declare (ignore observer))
                                (let ((conversation (agent-protocol-session-conversation session)))
                                  (assert (equal '("first input" "second input") (conversation-input-history conversation)))
                                  (assert (search "retained summary" (json-encode (conversation-input-items conversation))))
                                  (assert (search "second input" (json-encode (conversation-input-items conversation))))
                                  (setf ran t)))))
                (unwind-protect
                     (progn (agent-protocol-session-join session :timeout 5) (assert ran))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3))))))
      (agent-session-test--cleanup path)))
  nil)

(-> agent-session-test--interrupted () null)
(defun agent-session-test--interrupted ()
  "Read an unmatched ToolStarted, synthesize an unknown result, and acknowledge its repair."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (unwind-protect
         (progn
           (let* ((store   (autolith-supervisor::session-store-open path "client-session"))
                  (payload (make-hash-table :test #'equal)))
             (autolith-supervisor::session-store-start-turn store "dead-turn" "old input")
             (setf (gethash "call_id" payload) "uncertain-call"
                   (gethash "name" payload) "shell.run"
                   (gethash "arguments" payload) (make-hash-table :test #'equal))
             (autolith-supervisor::session-store-append
              store (agent-wire:make-session-append
                     :record (agent-wire:make-session-record
                              :event-id "started-before-crash" :turn-id "dead-turn" :kind ':tool-started
                              :payload (autolith-supervisor::protocol-data->value payload)))))
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "new-turn" "inspect state")
              (agent-session-test--until-finish runtime)
              (let* ((page    (autolith-supervisor::session-store-read
                               (autolith-supervisor::session-runtime-store runtime) (agent-wire:make-session-read :limit 256)))
                     (records (agent-wire:session-page.records page)))
                (assert (find-if (lambda (record)
                                   (let ((local (agent-session--local-record record)))
                                     (and (eq (first local) ':tool-result)
                                          (equal "uncertain-call" (getf (rest local) :call-id)))))
                                 records)))
              (agent-session-test--stop runtime))
            (lambda (client)
              (let ((ran     nil)
                    (session nil))
                (setf session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (declare (ignore observer))
                                (let ((conversation (agent-protocol-session-conversation session)))
                                  (assert (null (agent-session-conversation-repairs conversation)))
                                  (assert (search *conversation-interrupted-tool-output*
                                                  (json-encode (conversation-input-items conversation))))
                                  (setf ran t)))))
                (unwind-protect
                     (progn (agent-protocol-session-join session :timeout 5) (assert ran))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3))))))
      (agent-session-test--cleanup path)))
  nil)

(-> agent-session-test--cancel-stop () null)
(defun agent-session-test--cancel-stop ()
  "Deliver cancel and stop while the separate turn worker is active."
  (dolist (cancel-first '(nil t))
    (uiop:with-temporary-file (:pathname path :stream stream)
      (close stream) (delete-file path)
      (let ((entered (bordeaux-threads:make-lock "session test turn entered"))
            (ready   (bordeaux-threads:make-condition-variable))
            (running nil))
        (unwind-protect
             (agent-session-test--exchange
              path
              (lambda (runtime)
                (autolith-supervisor::session-runtime-start-turn runtime "cancelled" "slow turn")
                ;; The bridge reads history before invoking the turn runner.
                (loop until (bordeaux-threads:with-lock-held (entered) running)
                      do (autolith-supervisor::session-runtime-receive runtime))
                (when cancel-first
                  (autolith-supervisor::session-runtime-cancel-turn runtime "cancelled"))
                (autolith-supervisor::session-runtime-prepare-stop runtime :grace-milliseconds 3000)
                (agent-session-test--until-finish runtime)
                (loop until (eq (autolith-supervisor::session-runtime-phase runtime) ':stopped)
                      do (autolith-supervisor::session-runtime-receive runtime)))
              (lambda (client)
                (let ((session
                        (agent-session-test--start
                         client (lambda (session observer)
                                  (declare (ignore observer))
                                  (bordeaux-threads:with-lock-held (entered)
                                    (setf running t)
                                    (bordeaux-threads:condition-notify ready))
                                  (agent-protocol-output (agent-protocol-session-client session) ':progress "entered")
                                  (loop until (agent-protocol-session-cancelled-p session) do (sleep 0.001))
                                  (agent-session--check-running session)))))
                  (unwind-protect
                       (progn (agent-protocol-session-join session :timeout 5)
                              (assert (agent-protocol-session-stopped session))
                              (assert (null (agent-protocol-session-failure session))))
                    (agent-protocol-session-close session)
                    (agent-protocol-session-join session :timeout 3)))))
          (agent-session-test--cleanup path)))))
  nil)

(-> agent-session-test--accepted-result () null)
(defun agent-session-test--accepted-result ()
  "Resume mixed saved/missing accepted outputs once, in the original provider order."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (unwind-protect
         (progn
           (let* ((store   (autolith-supervisor::session-store-open path "client-session"))
                  (text    (agent-wire:make-output-item
                            :item-index 0 :family "responses" :replay-handle "saved-text"
                            :message (agent-wire:make-conversation-message
                                      :role ':assistant :content (list (agent-wire:make-content-block :text "accepted answer")))))
                  (call    (agent-wire:make-output-item
                            :item-index 1 :family "responses" :replay-handle "missing-call"
                            :tool-call (agent-wire:make-tool-call :call-id "accepted-call" :name "run"
                                                                  :namespace "shell" :arguments-json "{malformed")))
                  (payload (make-hash-table :test #'equal)))
             (autolith-supervisor::session-store-start-turn store "crashed" "old question")
             (autolith-supervisor::session-store-model-result
              store :event-id "accepted/model" :turn-id "crashed" :request-id "1"
              :complete (agent-wire:make-model-complete :reason ':tool-calls :output-items (list text call)))
             (setf (gethash "version" payload) 1
                   (gethash "agent_record" payload)
                   (agent-session-value-encode
                    (list :provider-item :wire-json
                          (json-encode (agent-protocol-provider-item-from-data
                                        (autolith-supervisor::protocol-message->data text))))))
             (autolith-supervisor::session-store-append
              store (agent-wire:make-session-append
                     :record (agent-wire:make-session-record
                              :event-id "saved/agent-item" :turn-id "crashed" :kind ':note
                              :payload (autolith-supervisor::protocol-data->value payload)))))
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "resumed" "inspect effects")
              (agent-session-test--until-finish runtime)
              (agent-session-test--stop runtime))
            (lambda (client)
              (let ((ran     nil)
                    (session nil))
                (setf session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (declare (ignore observer))
                                (let* ((items   (conversation-input-items (agent-protocol-session-conversation session)))
                                       (outputs (remove-if-not (lambda (item) (json-get item "parent_replay")) items)))
                                  (assert (equal '("saved-text" "missing-call")
                                                 (mapcar (lambda (item) (json-get (json-get item "parent_replay") "replay_handle")) outputs)))
                                  (assert (equal "{malformed" (json-get (second outputs) "arguments")))
                                  (assert (find *conversation-interrupted-tool-output* items
                                                :test #'equal :key (lambda (item) (json-get item "output"))))
                                  (setf ran t)))))
                (unwind-protect
                     (progn (agent-protocol-session-join session :timeout 5) (assert ran))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3))))))
      (agent-session-test--cleanup path)))
  nil)

(-> agent-session-test--paging () null)
(defun agent-session-test--paging ()
  "Read more than one maximum-size SessionPage before the first resumed turn."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (unwind-protect
         (progn
           (let ((store (autolith-supervisor::session-store-open path "client-session")))
             (dotimes (index 130)
               (let ((turn (format nil "old-~D" index)))
                 (autolith-supervisor::session-store-start-turn store turn (format nil "input ~D" index))
                 (autolith-supervisor::session-store-finish-turn store turn ':completed))))
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "paged" "latest")
              (agent-session-test--until-finish runtime)
              (agent-session-test--stop runtime))
            (lambda (client)
              (let ((ran     nil)
                    (session nil))
                (setf session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (declare (ignore observer))
                                (let* ((conversation (agent-protocol-session-conversation session))
                                       (history      (conversation-input-history conversation)))
                                  (assert (= 131 (length history)))
                                  (assert (equal "input 0" (first history)))
                                  (assert (equal "latest" (first (last history))))
                                  (setf ran t)))))
                (unwind-protect
                     (progn (agent-protocol-session-join session :timeout 5) (assert ran))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3))))))
      (agent-session-test--cleanup path)))
  nil)

(-> agent-session-test--lost-ack () null)
(defun agent-session-test--lost-ack ()
  "Lose a committed ToolStarted acknowledgement and prove its effect did not run."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (let ((effects   0)
          (committed nil))
      (unwind-protect
           (agent-session-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "lost-ack" "effect request")
              (loop
                (multiple-value-bind (request bytes)
                    (autolith-supervisor::parent-connection-receive
                     (autolith-supervisor::session-runtime-connection runtime))
                  (if (agent-wire:envelope.has-session-append request)
                      (progn
                        (autolith-supervisor::session-store-append
                         (autolith-supervisor::session-runtime-store runtime)
                         (agent-wire:envelope.session-append request))
                        (setf committed t)
                        (return))
                      (autolith-supervisor::session-runtime--incoming nil runtime request :bytes (length bytes)))))
              (autolith-supervisor::session-runtime-disconnect runtime))
            (lambda (client)
              (let ((session
                      (agent-session-test--start
                       client (lambda (session observer)
                                (let ((conversation (agent-protocol-session-conversation session)))
                                  (conversation--append-input-item
                                   conversation (json-object "type" "function_call" "call_id" "uncertain"
                                                             "name" "run" "namespace" "shell" "arguments" "{}"))
                                  (agent-observer-call-with-tool-execution observer "uncertain" (lambda () (incf effects))))))))
                (unwind-protect
                     (progn
                       (agent-protocol-session-join session :timeout 5)
                       (assert (agent-protocol-session-failure session))
                       (assert committed)
                       (assert (zerop effects)))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3)))))
        (agent-session-test--cleanup path))))
  nil)

(-> agent-session-test--result-classification () null)
(defun agent-session-test--result-classification ()
  "Skip compaction-only outputs and reject ambiguous legacy results without saved views."
  (dolist (case '((:generate :summary nil nil) (:native-compaction :ordinary nil nil)
                  (nil nil nil t) (nil nil t nil)))
    (destructuring-bind (kind purpose saved-p fails-p) case
      (uiop:with-temporary-file (:pathname path :stream stream)
        (close stream) (delete-file path)
        (unwind-protect
             (progn
               (let* ((store (autolith-supervisor::session-store-open path "client-session"))
                      (item  (agent-wire:make-output-item
                              :item-index 0 :family "responses" :replay-handle "classified-output"
                              :message (agent-wire:make-conversation-message
                                        :role ':assistant :content (list (agent-wire:make-content-block :text "summary-only-text"))))))
                 (autolith-supervisor::session-store-start-turn store "old" "old input")
                 (autolith-supervisor::session-store--recover-model-result
                  store :event-id "classified/result" :turn-id "old" :request-id "request"
                  :request-kind kind :request-purpose purpose
                  :complete (agent-wire:make-model-complete :reason ':stop :output-items (list item)))
                 (when saved-p
                   (let ((payload (make-hash-table :test #'equal)))
                     (setf (gethash "version" payload) 1
                           (gethash "agent_record" payload)
                           (agent-session-value-encode
                            (list :provider-item :wire-json
                                  (json-encode (agent-protocol-provider-item-from-data
                                                (autolith-supervisor::protocol-message->data item))))))
                     (autolith-supervisor::session-store-append
                      store (agent-wire:make-session-append
                             :record (agent-wire:make-session-record
                                      :event-id "saved-view" :turn-id "old" :kind ':note
                                      :payload (autolith-supervisor::protocol-data->value payload)))))))
               (agent-session-test--exchange
                path
                (lambda (runtime)
                  (autolith-supervisor::session-runtime-start-turn runtime "new" "new input")
                  (handler-case
                      (progn (agent-session-test--until-finish runtime)
                             (assert (not fails-p))
                             (agent-session-test--stop runtime))
                    (autolith-supervisor::parent-protocol-error (condition)
                      (unless fails-p (error condition)))))
                (lambda (client)
                  (let ((ran     nil)
                        (session nil))
                    (setf session
                          (agent-session-test--start
                           client (lambda (session observer)
                                    (declare (ignore observer))
                                    (let* ((items   (conversation-input-items (agent-protocol-session-conversation session)))
                                           (outputs (remove-if-not (lambda (item) (json-get item "parent_replay")) items)))
                                      (assert (= (if saved-p 1 0) (length outputs)))
                                      (setf ran t)))))
                    (unwind-protect
                         (progn
                           (agent-protocol-session-join session :timeout 5)
                           (if fails-p
                               (let ((failure (agent-protocol-session-failure session)))
                                 (assert (typep failure 'agent-protocol-error))
                                 (assert (eq (agent-protocol-error-reason failure) ':ambiguous-model-result))
                                 (assert (not ran)))
                               (progn (assert ran) (assert (null (agent-protocol-session-failure session))))))
                      (agent-protocol-session-close session)
                      (agent-protocol-session-join session :timeout 3))))))
          (agent-session-test--cleanup path)))))
  nil)

(defclass agent-session-test-effect-tool (tool)
  ((function :initarg :function :reader agent-session-test-effect-tool-function
             :documentation "The observable effect body."))
  (:documentation "A registered tool used by real nested dispatch checks."))

(defmethod tool-execute ((tool agent-session-test-effect-tool) (context tool-context) (arguments hash-table))
  "Run the nested fixture through the ordinary registry boundary."
  (funcall (agent-session-test-effect-tool-function tool) context arguments))

(defclass agent-session-test-effect-observer (agent-observer)
  ((interrupt-p :initarg :interrupt-p :reader agent-session-test-effect-observer-interrupt-p
                :documentation "Whether to interrupt after a nested effect returns."))
  (:documentation "Inject a lost completion after an effect, outside the registry handler."))

(define-condition agent-session-test-completion-interrupted (error) ()
  (:documentation "A fixture effect returned before its completion path failed."))

(defmethod agent-observer-call-with-tool-execution
    ((observer agent-session-test-effect-observer) identifier function)
  "Interrupt only nested completion after the real effect has run."
  (let ((result (funcall function)))
    (when (and (agent-session-test-effect-observer-interrupt-p observer)
               (not (equal identifier "outer")))
      (error 'agent-session-test-completion-interrupted))
    result))

(-> agent-session-test--nested
    (&key (:worker-p boolean) (:interrupted-p boolean) (:lost-finish-ack-p boolean)) null)
(defun agent-session-test--nested (&key worker-p interrupted-p lost-finish-ack-p)
  "Run real worker or task-inspector effects over the sole-reader parent connection."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream) (delete-file path)
    (let* ((root             (uiop:ensure-directory-pathname (make-pathname :type "nested" :defaults path)))
           (configuration
             (apply #'make-configuration
                    :source-root       (asdf:system-source-directory :autolith-agent)
                    :working-directory (asdf:system-source-directory :autolith-agent)
                    :config-root       (merge-pathnames "config/" root)
                    :data-root         (merge-pathnames "data/" root)
                    :state-root        (merge-pathnames "state/" root)
                    :cache-root        (merge-pathnames "cache/" root)
                    :telemetry-enabled-p nil :onboarding-p nil
                    (let ((core (uiop:getenv "AUTOLITH_TEST_ACTIVE_CORE")))
                      (when (non-empty-string-p core) (list :active-image-core (pathname core))))))
           (registry         (make-instance 'tool-registry))
           (pool             (and worker-p (lisp-worker-pool-create configuration)))
           (effects          0)
           (outer-id         (make-string 1024 :initial-element #\p))
           (observed-records nil)
           (turn-failure     nil)
           (nested-id        nil))
      (tool-registry-register
       registry (make-instance 'agent-session-test-effect-tool
                               :namespace "session-test" :name "effect"
                               :description "Count an actual nested effect."
                               :parameters  (tool-object-schema (json-object) nil)
                               :function
                               (lambda (context arguments)
                                 (declare (ignore arguments))
                                 (setf nested-id (tool-context-call-id context))
                                 (incf effects)
                                 (tool-success "effect complete"))))
      (unwind-protect
           (progn
             (agent-session-test--exchange
              path
              (lambda (runtime)
                (autolith-supervisor::session-runtime-start-turn runtime "nested" "input")
                (if lost-finish-ack-p
                    (loop
                      (multiple-value-bind (request bytes)
                          (autolith-supervisor::parent-connection-receive
                           (autolith-supervisor::session-runtime-connection runtime))
                        (let ((record (and (agent-wire:envelope.has-session-append request)
                                           (agent-wire:session-append.record (agent-wire:envelope.session-append request)))))
                          (if (and record (eq (agent-wire:session-record.kind record) ':tool-finished)
                                   (gethash "source" (autolith-supervisor::protocol-value->data
                                                      (agent-wire:session-record.payload record))))
                              (progn
                                (autolith-supervisor::session-store-append
                                 (autolith-supervisor::session-runtime-store runtime)
                                 (agent-wire:envelope.session-append request))
                                (autolith-supervisor::session-runtime-disconnect runtime)
                                (return))
                              (autolith-supervisor::session-runtime--incoming
                               nil runtime request :bytes (length bytes))))))
                    (agent-session-test--until-finish runtime))
                (setf observed-records
                      (agent-wire:session-page.records
                       (autolith-supervisor::session-store-read
                        (autolith-supervisor::session-runtime-store runtime)
                        (agent-wire:make-session-read :limit 256))))
                (unless lost-finish-ack-p (agent-session-test--stop runtime)))
              (lambda (client)
                (let ((session
                        (agent-protocol-session-start
                         client configuration "profile" :tool-registry registry :worker (or pool ':fixture)
                         :observer (make-instance 'agent-session-test-effect-observer :interrupt-p interrupted-p)
                         :run-turn
                         (lambda (session observer)
                           (let* ((conversation (agent-protocol-session-conversation session))
                                  (context      (make-instance 'tool-context :configuration configuration
                                                               :conversation conversation :registry registry
                                                               :agent                       (agent-protocol-session-agent session)
                                                               :worker pool :observer observer :call-id outer-id)))
                             (conversation-append-provider-item
                              conversation (json-object "type" "function_call" "call_id" outer-id
                                                        "name" "fixture.outer" "arguments" "{}"))
                             (handler-case
                                 (agent-observer-call-with-tool-execution
                                  observer outer-id
                                  (lambda ()
                                    (if worker-p
                                        (worker-host-eval-request
                                         (lisp-worker-pool-worker pool "nested-session") ':eval
                                         (list :forms '("(autolith:worker-tool-call \"session-test.effect\" \"{}\")"))
                                         :context context :tools '("session-test.effect") :enabled-p t)
                                        (task-continuity-dispatch context "session-test" "effect" :arguments (json-object)))))
                               (agent-session-test-completion-interrupted () (assert interrupted-p))
                               (error (condition) (setf turn-failure condition)))
                             (assert (= 1 effects))
                             (assert (not (find nested-id (conversation-input-items conversation)
                                                :test #'equal :key (lambda (item) (json-get item "call_id")))))
                             (unless (or interrupted-p lost-finish-ack-p)
                               (conversation-append-record conversation (list :tool-result :call-id outer-id
                                                                              :status ':ok :output "done"))))))))
                  (unwind-protect
                       (progn
                         (agent-protocol-session-join session :timeout 90)
                         (if lost-finish-ack-p
                             (assert (agent-protocol-session-failure session))
                             (progn
                               (assert (agent-protocol-session-stopped session))
                               (assert (null (agent-protocol-session-failure session))))))
                    (agent-protocol-session-close session)
                    (agent-protocol-session-join session :timeout 3)))))
             (let* ((starts   (remove-if-not (lambda (record) (eq (agent-wire:session-record.kind record) ':tool-started)) observed-records))
                    (finishes (remove-if-not (lambda (record) (eq (agent-wire:session-record.kind record) ':tool-finished)) observed-records))
                    (nested   (find nested-id starts :test #'equal
                                    :key                   (lambda (record)
                                                             (gethash "call_id" (autolith-supervisor::protocol-value->data
                                                                                 (agent-wire:session-record.payload record)))))))
               (unless lost-finish-ack-p
                 (assert (null turn-failure) () "Nested dispatch failed: ~A" turn-failure))
               (assert (= 1 effects))
               (assert (= 2 (length starts)))
               (assert (= (cond (lost-finish-ack-p
                                 1) (interrupted-p
                                     0) (t
                                         2)) (length finishes)))
               (assert nested)
               (let ((payload (autolith-supervisor::protocol-value->data (agent-wire:session-record.payload nested))))
                 (assert (equal outer-id (gethash "parent_call_id" payload)))
                 (assert (equal (if worker-p "worker" "task-inspector") (gethash "source" payload))))
               (assert (notany (lambda (record)
                                 (let ((local (agent-session--local-record record)))
                                   (and (member (first local) '(:worker-tool-result :task-inspector-result)) interrupted-p)))
                               observed-records)))
             ;; A fresh session must not turn nested audit calls into model calls.
             (agent-session-test--exchange
              path
              (lambda (runtime)
                (autolith-supervisor::session-runtime-start-turn runtime "resumed" "next")
                (agent-session-test--until-finish runtime)
                (agent-session-test--stop runtime))
              (lambda (client)
                (let ((session
                        (agent-session-test--start
                         client (lambda (session observer)
                                  (declare (ignore observer))
                                  (let ((items (conversation-input-items (agent-protocol-session-conversation session))))
                                    (assert (= 1 effects))
                                    (assert (not (find nested-id items :test #'equal
                                                      :key                   (lambda (item) (json-get item "call_id")))))
                                    (when (or interrupted-p lost-finish-ack-p)
                                      (assert (search *conversation-interrupted-tool-output* (json-encode items)))))))))
                  (unwind-protect
                       (progn
                         (agent-protocol-session-join session :timeout 5)
                         (assert (null (agent-protocol-session-failure session))))
                    (agent-protocol-session-close session)
                    (agent-protocol-session-join session :timeout 3))))))
        (when pool (lisp-worker-pool-stop-all pool))
        (agent-session-test--cleanup path)
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> run-agent-protocol-session-tests () integer)
(defun run-agent-protocol-session-tests ()
  "Run focused parent-backed conversation and turn lifecycle checks."
  (agent-session-test--codec)
  (agent-session-test--persist-and-resume)
  (agent-session-test--interrupted)
  (agent-session-test--cancel-stop)
  (agent-session-test--accepted-result)
  (agent-session-test--paging)
  (agent-session-test--lost-ack)
  (agent-session-test--result-classification)
  (dolist (worker-p '(nil t))
    (dolist (mode '(:completed :interrupted :lost-finish-ack))
      (agent-session-test--nested :worker-p         worker-p
                                 :interrupted-p     (eq mode ':interrupted)
                                 :lost-finish-ack-p (eq mode ':lost-finish-ack))))
  14)
