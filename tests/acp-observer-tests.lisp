(in-package #:autolith)

;;;; -- ACP Observer Behaviour --

(defclass acp-observer-test-client (agentcomms:acp-client)
  ((updates
    :initform nil :accessor acp-observer-test-client-updates
    :documentation "Updates received by the deterministic ACP client.")
   (permission-updates
    :initform nil :accessor acp-observer-test-client-permission-updates
    :documentation "Updates handled before the latest permission request.")
   (permission-choice
    :initarg :permission-choice :initform "once"
    :accessor      acp-observer-test-client-permission-choice
    :documentation "The permission option selected by the client."))
  (:documentation "A minimal ACP client used by observer protocol tests."))

(defmethod agentcomms:client-session-update
    ((client acp-observer-test-client) session-id update &key params)
  "Record UPDATE so tests can inspect the notifications the client received."
  (declare (ignore session-id params))
  (push update (acp-observer-test-client-updates client))
  nil)

(defmethod agentcomms:client-request-permission
    ((client acp-observer-test-client) session-id tool-call &key options params)
  "Snapshot the updates seen so far and select the client's configured permission choice."
  (declare (ignore session-id tool-call options params))
  (setf (acp-observer-test-client-permission-updates client)
        (reverse (acp-observer-test-client-updates client)))
  (values ':selected (acp-observer-test-client-permission-choice client)))

(defmethod agentcomms:client-extension-request ((client acp-observer-test-client) method params)
  "Acknowledge a barrier after the client has handled preceding notifications."
  (declare (ignore params))
  (if (equal method "_sync")
      (agentcomms:json-object)
      (call-next-method)))

(-> acp-observer-test-service (configuration) acp-service)
(defun acp-observer-test-service (configuration)
  "Create a service suitable for direct observer protocol tests."
  (make-instance 'acp-service :configuration configuration))

(-> test-acp-observer-streams-one-response () null)
(defun test-acp-observer-streams-one-response ()
  "Exercise test-acp-observer-streams-one-response."
  (with-test-configuration (configuration)
    (let* ((service  (acp-observer-test-service configuration))
           (session
            (make-instance 'acp-session :service service :identifier "observer" :application
                           (make-instance 'application :configuration configuration)))
           (observer (make-instance 'acp-observer :session session :turn-sequence 7))
           (client   (make-instance 'acp-observer-test-client))
           (updates  nil))
      (multiple-value-bind (server client-channel)
          (agentcomms:make-acp-channel-pair)
        (unwind-protect
             (progn
               (agentcomms:acp-agent-connect service server)
               (agentcomms:acp-client-connect client client-channel)
               (agentcomms:client-initialize client)
               (setf (acp-observer-test-client-updates client) nil)
               (agent-observer-text observer "hello ")
               (agent-observer-text observer "world")
               (agent-observer-status observer ':assistant-response-persisted
                                      (list :text "hello world"))
               (agentcomms:agent-client-request service "_sync" (agentcomms:json-object))
               (setf updates (reverse (acp-observer-test-client-updates client)))
               (test-assert
                (= 2 (count ':agent-message-chunk updates :key #'agentcomms:acp-update-kind))
                "streamed response is not repeated on persistence")
               (test-assert
                (equal "hello world"
                       (apply #'concatenate 'string
                              (mapcar
                               (lambda (update)
                                 (agentcomms:acp-content-text
                                  (agentcomms:json-get update "content")))
                               (remove-if-not
                                (lambda (update)
                                  (eq ':agent-message-chunk (agentcomms:acp-update-kind update)))
                                updates))))
                "streamed response chunks preserve order"))
          (agentcomms:connection-close (agentcomms:acp-client-connection client))))))
  nil)

(-> test-acp-observer-permission-validates-offered-choice () null)
(defun test-acp-observer-permission-validates-offered-choice ()
  "Exercise test-acp-observer-permission-validates-offered-choice."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((service        (acp-observer-test-service configuration))
           (session
            (make-instance 'acp-session :service service :identifier "permissions" :application
                           (make-instance 'application :configuration configuration)))
           (observer       (make-instance 'acp-observer :session session :turn-sequence 1))
           (client         (make-instance 'acp-observer-test-client :permission-choice "bogus"))
           (server         nil)
           (client-channel nil))
      (multiple-value-setq (server client-channel) (agentcomms:make-acp-channel-pair))
      (unwind-protect
           (progn
             (agentcomms:acp-agent-connect service server)
             (agentcomms:acp-client-connect client client-channel)
             (agentcomms:client-initialize client)
             (test-assert
              (null
               (acp-observer--approval observer "shell.run"
                                       :arguments (agentcomms:json-object)
                                       :key       '("command")))
              "an unoffered permission choice is denied")
             (setf (acp-observer-test-client-permission-choice client) "session")
             (test-assert
              (acp-observer--approval observer "shell.run"
                                      :arguments (agentcomms:json-object)
                                      :key       '("command"))
              "an offered session choice is accepted"))
        (agentcomms:connection-close (agentcomms:acp-client-connection client)))))
  nil)

(-> test-acp-observer-forwards-serialized-tool-execution () null)
(defun test-acp-observer-forwards-serialized-tool-execution ()
  "Exercise test-acp-observer-forwards-serialized-tool-execution."
  (with-test-configuration (configuration)
    (let* ((service     (acp-observer-test-service configuration))
           (application (make-instance 'application :configuration configuration))
           (session
            (make-instance 'acp-session :service service :identifier "forward" :application
                           application))
           (observer    (make-instance 'acp-observer :session session :turn-sequence 1))
           (serialized  (make-instance 'serialized-agent-observer :delegate observer))
           (seen        nil))
      (agent-observer-call-with-tool-execution serialized "call-1"
                                               (lambda ()
                                                 (setf seen
                                                       (and (eq *active-application* application)
                                                            (string= *acp-current-tool-call-id*
                                                                     "call-1")))))
      (test-assert seen "serialized observer forwards ACP tool ownership and bindings")))
  nil)

(-> test-acp-observer-tool-title-previews () null)
(defun test-acp-observer-tool-title-previews ()
  "Preview principal arguments without control characters or unbounded titles."
  (dolist (case (list (list "lisp.eval" (json-object "compile" t "forms" #("(load-it)" "(+ 1 2)")) "(load-it) (+ 1 2)")
                     (list "shell.run" (json-object "async" t "command" "git status") "git status")
                     (list "resource.read" (json-object "uri" "conversation:current") "conversation:current")
                     (list "custom" (json-object "z" 9 "a" #(1 2)) "2 items")
                     (list "custom" (json-object "flag" (argo:json-false)) "false")))
    (destructuring-bind (name arguments preview) case
      (test-assert (search preview (acp-tool-title name arguments))
                   "the title previews the principal argument")))
  (dolist (arguments (list nil (json-object) (json-object "form" "")))
    (test-assert (string= "lisp.eval" (acp-tool-title "lisp.eval" arguments))
                 "empty arguments use the canonical tool name"))
  (let ((title (acp-tool-title "lisp.eval"
                               (json-object "form" (format nil "~C[31m~A~%tail"
                                                            #\Esc (make-string 100 :initial-element #\界))))))
    (test-assert (<= (text-cell-width title) (+ (text-cell-width "lisp.eval") 3 48))
                 "argument previews are bounded in display cells")
    (test-assert (not (find-if (lambda (character)
                                (or (< (char-code character) 32)
                                    (= (char-code character) 127)))
                              title))
                 "titles contain no control characters"))
  (test-call-with-function-replacements
   (list (list 'json-encode (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error "Compound title previews must not serialize their payload.")))
         (list 'sanitize-text (lambda (text &key single-line-p)
                               (declare (ignore single-line-p))
                               (test-assert (<= (length text) 192)
                                            "title sanitization consumes a bounded prefix")
                               text)))
   (lambda ()
     (acp-tool-title "lisp.eval" (json-object "form" (make-string 1000000 :initial-element #\x)))
     (acp-tool-title "custom" (json-object "items" (make-array 100000 :initial-element "large")))
     (acp-tool-title "lisp.eval" (json-object "forms" (make-array 4 :initial-element (make-string 1000000 :initial-element #\x))))))
  nil)

(-> test-acp-observer-batches-thoughts-before-permission () null)
(defun test-acp-observer-batches-thoughts-before-permission ()
  "Coalesce fragments across response boundaries while preserving approval ordering."
  (with-test-configuration (configuration)
    (let* ((service  (acp-observer-test-service configuration))
           (session  (make-instance 'acp-session :service service :identifier "thoughts"
                                    :application (make-instance 'application :configuration configuration)))
           (observer (make-instance 'acp-observer :session session :turn-sequence 1))
           (client   (make-instance 'acp-observer-test-client)))
      (multiple-value-bind (server client-channel) (agentcomms:make-acp-channel-pair)
        (unwind-protect
             (progn
               (agentcomms:acp-agent-connect service server)
               (agentcomms:acp-client-connect client client-channel)
               (agentcomms:client-initialize client)
               (agent-observer-reasoning observer "first ")
               (agent-observer-reasoning observer "thought")
               (agent-observer-status observer ':provider-request-started nil)
               (agent-observer-reasoning observer "")
               (agent-observer-reasoning observer "next ")
               (agent-observer-reasoning observer "thought")
               (agent-observer-text observer "answer")
               (agent-observer-status observer ':provider-request-started nil)
               (agent-observer-reasoning observer "before ")
               (agent-observer-reasoning observer "approval")
               (agent-observer-reasoning observer "continued ")
               (agent-observer-text observer "")
               (agent-observer-reasoning observer "thought")
               (test-assert
                (acp-observer--approval observer "shell.run"
                                        :arguments (json-object "command" "pwd")
                                        :key       '(:command))
                "the editor can approve after receiving pending thoughts")
               (let ((updates (acp-observer-test-client-permission-updates client)))
                 (test-assert
                  (equal '(:agent-thought-chunk :agent-message-chunk :agent-thought-chunk)
                         (mapcar #'agentcomms:acp-update-kind updates))
                  "thought batches precede the next message and permission request")
                 (test-assert
                  (equal (list (format nil "first thought~2%next thought") "answer"
                               (format nil "before approvalcontinued thought"))
                         (mapcar (lambda (update)
                                   (agentcomms:acp-content-text (agentcomms:json-get update "content")))
                                 updates))
                  "coalescing preserves fragment text and order")))
          (agentcomms:connection-close (agentcomms:acp-client-connection client))))))
  nil)

(-> test-acp-observer-skips-empty-message-text () null)
(defun test-acp-observer-skips-empty-message-text ()
  "Skip empty deltas while preserving thought batching and nonstreamed final text."
  (with-test-configuration (configuration)
    (let* ((service  (acp-observer-test-service configuration))
           (session  (make-instance 'acp-session :service service :identifier "empty-text"
                                    :application (make-instance 'application :configuration configuration)))
           (observer (make-instance 'acp-observer :session session :turn-sequence 1))
           (client   (make-instance 'acp-observer-test-client)))
      (multiple-value-bind (server client-channel) (agentcomms:make-acp-channel-pair)
        (unwind-protect
             (progn
               (agentcomms:acp-agent-connect service server)
               (agentcomms:acp-client-connect client client-channel)
               (agentcomms:client-initialize client)
               (agent-observer-text observer "")
               (agentcomms:agent-client-request service "_sync" (agentcomms:json-object))
               (test-assert (null (acp-observer-test-client-updates client))
                            "empty text alone sends no update")
               (agent-observer-reasoning observer "first ")
               (agent-observer-text observer "")
               (agent-observer-reasoning observer "thought")
               (agent-observer-text observer "")
               (agent-observer-status observer ':assistant-response-persisted
                                      (list :text "answer"))
               (agentcomms:agent-client-request service "_sync" (agentcomms:json-object))
               (let ((updates (reverse (acp-observer-test-client-updates client))))
                 (test-assert
                  (equal '(:agent-thought-chunk :agent-message-chunk)
                         (mapcar #'agentcomms:acp-update-kind updates))
                  "the final response follows one thought batch")
                 (test-assert
                  (equal (list "first thought" "answer")
                         (mapcar (lambda (update)
                                   (agentcomms:acp-content-text (agentcomms:json-get update "content")))
                                 updates))
                  "empty streamed deltas preserve the nonstreamed final response")))
          (agentcomms:connection-close (agentcomms:acp-client-connection client))))))
  nil)

(-> test-acp-external-tool-permission-modes () null)
(defun test-acp-external-tool-permission-modes ()
  "Keep editor policy independent and classify external calls without a sandbox."
  (with-test-configuration (configuration root)
    (let* ((application
             (make-instance 'application :configuration configuration
                            :permission-mode            ':full-access
                            :provider                   (make-instance 'rlm-inference-test-provider :results nil)))
           (session        (make-instance 'acp-session
                                          :service     (acp-observer-test-service configuration)
                                          :identifier  "external-permissions"
                                          :application application))
           (observer       (make-instance 'acp-observer :session session :turn-sequence 1))
           (tool           (make-instance 'tool :namespace "service" :name "lookup"
                                          :description "Read one record."))
           (arguments      (json-object "record" "one"))
           (approved-p     nil)
           (approval-count 0)
           (decision       ':full-access))
      (permissions-allow :configuration configuration
                         :state         (application-permission-state application)
                         :command       (application--tool-permission-key tool arguments)
                         :directory root :kind ':external-tool)
      (test-call-with-function-replacements
       (list
        (list 'acp-observer--approval
              (lambda (observer title &key arguments key)
                (declare (ignore observer arguments key))
                (incf approval-count)
                (test-assert (string= title (tool-display-name tool))
                             "editor permission titles use the readable tool identity")
                approved-p))
        (list 'permissions-model-classify-command
              (lambda (request directory &key provider configuration
                                             sandbox-available-p user-instructions)
                (declare (ignore directory provider configuration user-instructions))
                (test-assert (and (not sandbox-available-p)
                                  (search "External tool call" request))
                             "editor auto mode classifies external execution without a local sandbox")
                (values decision "test policy"))))
       (lambda ()
         (test-assert (eq (agent-observer-authorize-tool observer tool arguments) ':deny)
                      "terminal full access and persistent grants cannot bypass an editor session")
         (setf approved-p t)
         (test-assert (eq (agent-observer-authorize-tool observer tool arguments) ':allow)
                      "the editor can approve an exact external request")
         (setf (acp-session-mode session) ':sandboxed approved-p nil)
         (test-assert (eq (agent-observer-authorize-tool observer tool arguments) ':deny)
                      "editor sandbox mode still requires external approval")
         (let ((before approval-count))
           (setf (acp-session-mode session) ':full-access)
           (test-assert (and (eq (agent-observer-authorize-tool observer tool arguments) ':allow)
                             (= before approval-count))
                        "editor full access permits external calls without another approval")
           (setf (acp-session-mode session) ':auto)
           (dolist (case '((:full-access :allow) (:sandboxed :deny) (:deny :deny)))
             (setf decision (first case))
             (clrhash (application-command-classifications application))
             (test-assert (eq (agent-observer-authorize-tool observer tool arguments)
                              (second case))
                          "editor auto applies only available external grants"))
           (test-assert (= before approval-count)
                        "editor auto never falls back to an approval prompt"))))))
  nil)
