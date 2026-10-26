;;;; -- Parent Model Projection Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (parent-proto:model-start) parent-proto:model-start) model-test--roundtrip))

(defun model-test--roundtrip (start)
  "Exercise the actual generated Protobuf serializer and decoder."
  (cl-protobufs:deserialize-from-bytes 'parent-proto:model-start
                                     (cl-protobufs:serialize-to-bytes start)))

(declaim (ftype (function (string) parent-proto:transcript-item) model-test--text))

(defun model-test--text (text)
  "Build one ordinary logical user item."
  (parent-proto:make-transcript-item
   :message (parent-proto:make-conversation-message
             :role ':user :content (list (parent-proto:make-content-block :text text)))))

(declaim (ftype (function (&rest t) parent-proto:model-start) model-test--start))

(defun model-test--start (&rest options)
  "Build an ordinary request with overrideable test fields."
  (apply #'parent-proto:make-model-start
         (append options (list :profile-id "installed" :turn-id "turn" :kind ':generate))))

(declaim (ftype (function (&key (:deferred boolean) (:strict t) (:strict-p boolean))
                         parent-proto:tool-definition) model-test--tool))

(defun model-test--tool (&key deferred strict strict-p)
  "Build a namespaced tool with a bounded portable schema."
  (apply #'parent-proto:make-tool-definition
         :name "lookup" :namespace "local" :namespace-description "Local tools"
         :description "Look up data" :deferred deferred
         :input-schema (protocol-data->map (model-wire::json-object "type" "object"))
         (when strict-p (list :strict strict))))

(declaim (ftype (function () null) run-model-projection-parity-tests))

(defun run-model-projection-parity-tests ()
  "Compare actual request encodings with independently assembled library projections."
  (dolist (entry '((:responses model-wire:responses-api-provider)
                   (:chat-completions model-wire:chat-completions-provider)
                   (:anthropic model-wire:anthropic-messages-provider)
                   (:gemini model-wire:gemini-generate-content-provider)))
    (destructuring-bind (family class) entry
      (let* ((profile   (make-model-profile :id "installed" :model "parent-model" :family family))
             (registry  (make-model-profile-registry (list profile)))
             (provider  (make-instance class))
             (start     (model-test--roundtrip
                          (model-test--start :items            (list (model-test--text "durable")
                                                                    (parent-proto:make-transcript-item
                                                                     :tool-call (parent-proto:make-tool-call
                                                                                 :call-id "call" :name "lookup"
                                                                                 :namespace "local" :arguments-json " {\"q\":true} "))
                                                                    (parent-proto:make-transcript-item
                                                                     :tool-result (parent-proto:make-tool-result
                                                                                   :call-id "call"
                                                                                   :content (protocol-data->value "result"))))
                                            :ephemeral-items   (list (model-test--text "local"))
                                            :stable-prefix '("stable") :request-suffix '("suffix")
                                            :max-output-tokens 1024
                                            :tools             (list (model-test--tool)))))
             (durable   (list (model-wire::json-object
                               "type" "message" "role" "user" "content"
                               (vector (model-wire::json-object "type" "input_text" "text" "durable")))
                              (model-wire::json-object "type" "function_call" "call_id" "call"
                                                       "name" "lookup" "namespace" "local"
                                                       "arguments" " {\"q\":true} ")
                              (model-wire::json-object "type" "function_call_output"
                                                       "call_id" "call" "output" "result")))
             (ephemeral (model-wire::json-object
                         "type" "message" "role" "user" "content"
                         (vector (model-wire::json-object "type" "input_text" "text" "local"))))
             (tool      (model-wire::json-object
                         "type" "namespace" "name" "local" "description" "Local tools"
                         "tools" (vector (model-wire::json-object
                                          "type" "function" "name" "lookup" "description" "Look up data"
                                          "parameters" (model-wire::json-object "type" "object")))))
             (expected  (make-instance 'model-wire:wire-request
                                       :model   "parent-model"
                                       :items   (if (eq family ':anthropic)
                                                    durable (append durable (list ephemeral)))
                                       :prefix '("stable") :suffix '("suffix")
                                       :options (append (list :maximum-output-tokens 1024)
                                                        (when (eq family ':anthropic)
                                                          (list :ephemeral-items (list ephemeral)))))))
        (multiple-value-bind (projection tools selected kind summary-p)
            (model-start->wire-request start registry)
          (assert (eq profile selected))
          (assert (eq kind ':generate))
          (assert (not summary-p))
          (assert (equalp
                   (model-wire:provider-request-object
                    provider projection (model-wire:provider-wire-tools provider tools))
                   (model-wire:provider-request-object
                    provider expected (model-wire:provider-wire-tools provider (vector tool)))))))))
  nil)

(declaim (ftype (function () null) run-model-projection-boundary-tests))

(defun run-model-projection-boundary-tests ()
  "Check defaults, discovery output, error flags, and library limits."
  (let* ((profile  (make-model-profile :id "installed" :model "m" :family ':responses
                                      :capabilities '(:deferred-tools)
                                      :options      (list :tool-discovery t :reasoning-effort "low"
                                                          :reasoning-summary "auto")))
         (registry (make-model-profile-registry (list profile))))
    (assert (= 2 (length (nth-value 1 (model-start->wire-request
                                       (model-test--roundtrip
                                        (model-test--start :tools (list (model-test--tool :deferred t)))) registry)))))
    (model-test--error
     (lambda () (model-start->wire-request
                 (model-test--start :tools (list (model-test--tool :deferred t)) :tool-discovery nil) registry))
     ':missing-discovery)
    (multiple-value-bind (projection tools)
        (model-start->wire-request (model-test--roundtrip (model-test--start :purpose ':ordinary-summary)) registry)
      (assert (zerop (length tools)))
      (assert (not (getf (model-wire:wire-request-options projection) :reasoning-summary))))
    (let* ((discovery  (parent-proto:make-tool-discovery-result
                        :call-id "search" :tools (list (model-test--tool :deferred t))))
           (projection (model-start->wire-request
                        (model-test--roundtrip
                         (model-test--start :items (list (parent-proto:make-transcript-item
                                                          :tool-discovery-result discovery)))) registry))
           (item       (first (model-wire:wire-request-items projection))))
      (assert (equal "tool_search_output" (gethash "type" item)))
      (assert (equal "completed" (gethash "status" item)))
      (assert (equal "client" (gethash "execution" item)))
      (assert (= 1 (length (gethash "tools" item))))))
  (let ((registry (make-model-profile-registry
                   (list (make-model-profile :id "installed" :model "m" :family ':responses)))))
    (assert (not (member :tool-discovery (model-wire:wire-request-options
                                          (model-start->wire-request (model-test--start) registry)))))
    (dolist (entry (list (list (parent-proto:make-tool-result :call-id "call") ':missing)
                        (list (parent-proto:make-tool-result :call-id "call" :is-error t
                                                            :content (protocol-data->value "error")) ':unsupported)))
      (model-test--error
       (lambda () (model-start->wire-request
                   (model-test--roundtrip
                    (model-test--start :items (list (parent-proto:make-transcript-item :tool-result (first entry)))))
                   registry)) (second entry))))
  (dolist (family '(:chat-completions :anthropic :gemini))
    (let ((registry (make-model-profile-registry
                     (list (make-model-profile :id "installed" :model "m" :family family)))))
      (model-test--error
       (lambda () (model-start->wire-request
                   (model-test--roundtrip (model-test--start :tools (list (model-test--tool :strict-p t :strict t))))
                   registry)) ':unsupported)))
  (model-test--error
   (lambda () (model-start->wire-request
               (model-test--start :items (list (model-test--text "beyond budget")))
               (make-model-profile-registry
                (list (make-model-profile :id "installed" :model "m" :family ':responses
                                          :maximum-request-bytes 1))))) ':limit)
  (model-test--error
   (lambda () (model-start->wire-request
               (model-test--start :max-output-tokens 32001)
               (make-model-profile-registry
                (list (make-model-profile :id "installed" :model "m" :family ':anthropic
                                          :maximum-output-tokens 40000))))) ':unsupported-by-wire-library)
  (dolist (default '(1000 0 "100"))
    (let ((registry (make-model-profile-registry
                     (list (make-model-profile :id "installed" :model "m" :family ':responses
                                               :maximum-output-tokens 100
                                               :options               (list :maximum-output-tokens default))))))
      (model-test--error
       (lambda () (model-start->wire-request (model-test--start) registry)) ':limit)
      (let ((request (model-start->wire-request (model-test--start :max-output-tokens 50) registry)))
        (assert (= 50 (getf (model-wire:wire-request-options request) :maximum-output-tokens))))))
  (let* ((registry (make-model-profile-registry
                    (list (make-model-profile :id "installed" :model "m" :family ':responses
                                              :maximum-output-tokens 100
                                              :options               '(:maximum-output-tokens 75)))))
         (request  (model-start->wire-request (model-test--start) registry)))
    (assert (= 75 (getf (model-wire:wire-request-options request) :maximum-output-tokens))))
  nil)

(declaim (ftype (function () null) run-model-projection-tests))

(defun run-model-projection-tests ()
  "Check ordered typed input, authority, presence, media, summary, and native kind."
  (run-model-projection-boundary-tests)
  (run-model-projection-parity-tests)
  (let* ((hosted            (make-hash-table :test #'equal))
         (hosted-definition (model-wire::json-object "type" "web_search"))
         (profile           (make-model-profile
                             :id "installed" :model "parent-model" :family ':responses
                             :capabilities '(:media :native-compaction :deferred-tools :continuation :turn-state)
                             :reasoning-efforts '("low" "high") :reasoning-summaries '("auto")
                             :hosted-tools hosted))
         (registry          (make-model-profile-registry (list profile))))
    (setf (gethash "web" hosted) hosted-definition)
    (dolist (raw '("" "{" "42" "[]" " {\"x\": false } \n"))
      (let* ((call       (parent-proto:make-tool-call :call-id "id" :name "lookup" :namespace "local"
                                                    :arguments-json raw))
             (start      (model-test--roundtrip
                          (model-test--start :items (list (parent-proto:make-transcript-item :tool-call call)))))
             (projection (model-start->wire-request start registry)))
        (assert (string= raw (gethash "arguments" (first (model-wire:wire-request-items projection)))))))
    (let* ((map        (model-wire::json-object "false" nil "null" ':protocol-null "array" #()))
           (call       (parent-proto:make-tool-call :call-id "id" :name "lookup"
                                                  :arguments (protocol-data->map map)))
           (projection (model-start->wire-request
                        (model-test--roundtrip
                         (model-test--start :items (list (parent-proto:make-transcript-item :tool-call call))))
                        registry))
           (json       (gethash "arguments" (first (model-wire:wire-request-items projection)))))
      (assert (search "\"false\":false" (remove #\Space json)))
      (assert (search "\"null\":null" (remove #\Space json)))
      (assert (search "\"array\":[]" (remove #\Space json))))
    (dolist (call (list (parent-proto:make-tool-call :call-id "id" :name "lookup")
                       (parent-proto:make-tool-call :call-id "id" :name "lookup" :arguments-json "{}"
                                                    :arguments (common-proto:make-value-map))))
      (model-test--error
       (lambda () (model-start->wire-request
                   (model-test--start :items (list (parent-proto:make-transcript-item :tool-call call))) registry))
       ':ambiguous-presence))
    (let* ((original (model-wire::json-object "type" "reasoning" "encrypted_content" "parent-original"))
           (calls    nil)
           (resolver (lambda (handle &key kind profile turn-id)
                       (push (list handle kind (model-profile-model profile) turn-id) calls)
                       (values (if (eq kind ':item) original "private-routing") (string= handle "valid"))))
           (start    (model-test--roundtrip
                      (model-test--start :items (list (model-test--text "before")
                                                     (parent-proto:make-transcript-item :replay-handle "valid")
                                                     (model-test--text "after"))
                                         :continuation-handle "valid" :turn-state-handle "valid"))))
      (multiple-value-bind (projection tools selected kind summary-p continuation turn-state)
          (model-start->wire-request start registry :replay-resolver resolver)
        (declare (ignore tools selected kind summary-p))
        (assert (eq original (second (model-wire:wire-request-items projection))))
        (assert (= 3 (length (model-wire:wire-request-items projection))))
        (assert (= 3 (length calls)))
        (assert (equal continuation "private-routing"))
        (assert (equal turn-state "private-routing")))
      (model-test--error (lambda () (model-start->wire-request start registry)) ':unresolved)
      (model-test--error
       (lambda () (model-start->wire-request
                   (model-test--start :items (list (parent-proto:make-transcript-item :replay-handle "wrong")))
                   registry :replay-resolver resolver)) ':unresolved))
    (dolist (strict-p '(nil t))
      (let* ((start      (model-test--roundtrip
                          (model-test--start :tools          (list (model-test--tool :strict-p strict-p :strict nil))
                                             :tool-discovery nil)))
             (values     (multiple-value-list (model-start->wire-request start registry)))
             (projection (first values))
             (tool       (aref (gethash "tools" (aref (second values) 0)) 0)))
        (assert (eq strict-p (nth-value 1 (gethash "strict" tool))))
        (when strict-p (assert (eq yason:false (gethash "strict" tool))))
        (assert (member :tool-discovery (model-wire:wire-request-options projection)))
        (assert (null (getf (model-wire:wire-request-options projection) :tool-discovery)))))
    (let* ((start (model-test--roundtrip
                   (model-test--start :tools (list (model-test--tool :deferred t)) :tool-discovery t
                                      :hosted-tools (list (parent-proto:make-hosted-tool :capability-id "web")))))
           (tools (nth-value 1 (model-start->wire-request start registry))))
      (assert (= 3 (length tools)))
      (assert (eq t (gethash "defer_loading" (aref (gethash "tools" (aref tools 0)) 0)))))
    (let* ((data       (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 255)))
           (message    (parent-proto:make-conversation-message
                        :role ':user :content (list (parent-proto:make-content-block
                                                     :media (parent-proto:make-media
                                                             :media-type "image/png" :data data)))))
           (projection (model-start->wire-request
                        (model-test--roundtrip (model-test--start :messages (list message))) registry)))
      (assert (string= "data:image/png;base64,AAEC/w=="
                       (gethash "image_url" (aref (gethash "content"
                                                         (first (model-wire:wire-request-items projection))) 0)))))
    (dolist (case (list
                   (list (model-test--start :profile-id "https://untrusted.invalid") ':unknown-profile)
                   (list (model-test--start :kind ':kind-unspecified) ':unsupported)
                   (list (model-test--start :temperature 0.0d0) ':unsupported-by-wire-library)
                   (list (model-test--start :max-output-tokens 0) ':limit)
                   (list (model-test--start :max-output-tokens 999999) ':limit)
                   (list (model-test--start :reasoning-effort "endpoint") ':unsupported)
                   (list (model-test--start :reasoning-summary "auto") ':missing-effort)
                   (list (model-test--start :items (list (parent-proto:make-transcript-item))) ':missing-oneof)
                   (list (model-test--start :items    (list (model-test--text "x"))
                                            :messages (list (parent-proto:make-conversation-message))) ':mixed-history)
                   (list (model-test--start :tools (list (model-test--tool :deferred t))) ':missing-discovery)
                   (list (model-test--start :purpose ':ordinary-summary :tools (list (model-test--tool))) ':unexpected-tools)
                   (list (model-test--start :purpose ':ordinary-summary :ephemeral-items (list (model-test--text "x"))) ':unexpected-tools)
                   (list (model-test--start :purpose ':ordinary-summary :reasoning-summary "auto") ':unexpected-for-summary)
                   (list (model-test--start :kind ':native-compaction :purpose ':ordinary-summary) ':unsupported)
                   (list (model-test--start :hosted-tools (list (parent-proto:make-hosted-tool :capability-id "endpoint"))) ':unsupported)))
      (destructuring-bind (start reason) case
        (model-test--error (lambda () (model-start->wire-request (model-test--roundtrip start) registry)) reason)))
    (assert (eq ':native-compaction
                (nth-value 3 (model-start->wire-request (model-test--start :kind ':native-compaction) registry))))
    (dolist (message (list
                      (parent-proto:make-conversation-message :role   ':role-unspecified
                                                             :content (list (parent-proto:make-content-block :text "x")))
                      (parent-proto:make-conversation-message :role ':user :content (list (parent-proto:make-content-block)))
                      (parent-proto:make-conversation-message :role ':user :content (list (parent-proto:make-content-block :reasoning-text "forged")))
                      (parent-proto:make-conversation-message :role ':tool :content (list (parent-proto:make-content-block :text "x")))
                      (parent-proto:make-conversation-message :role ':user :content (list (parent-proto:make-content-block
                                                                                           :media (parent-proto:make-media :media-type "https://example.invalid"))))))
      (assert (handler-case (progn (model-start->wire-request
                                    (model-test--roundtrip (model-test--start :messages (list message))) registry) nil)
                (model-projection-error () t)))))
  (let* ((profile  (make-model-profile :id "installed" :model "claude" :family ':anthropic))
         (registry (make-model-profile-registry (list profile)))
         (start    (model-test--roundtrip (model-test--start :purpose      ':ordinary-summary
                                                           :tool-discovery nil
                                                           :items          (list (model-test--text "history"))
                                                           :stable-prefix '("system") :request-suffix '("summarize")))))
    (multiple-value-bind (projection tools selected kind summary-p)
        (model-start->wire-request start registry)
      (declare (ignore selected))
      (assert (eq kind ':generate))
      (assert summary-p)
      (assert (null (getf (model-wire:wire-request-options projection) :cache-p t)))
      (let* ((provider (make-instance 'model-wire:anthropic-messages-provider))
             (request  (model-wire:provider-request-object provider projection tools :compaction-p summary-p)))
        (assert (not (search "cache_control" (model-wire::json-encode request))))))
    (model-test--error (lambda () (model-start->wire-request (model-test--start :kind ':native-compaction) registry))
                       ':unsupported))
  nil)
