(in-package #:autolith)

;;;; -- Operation Extension Boundary Tests --

(-> test-acp-extension-operations-and-invoke () null)
(defun test-acp-extension-operations-and-invoke ()
  "Invoke native command arguments and tool objects through the ACP boundary."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier  (agentcomms:client-new-session client (namestring root)))
              (session     (acp-service--session service identifier))
              (application (acp-session-test--application session))
              (parameters  (tool-object-schema
                            (json-object "text"   (tool-string-property "Text to echo.")
                                         "nested" (json-object "type" "object"))
                            '("text" "nested")))
              (tool        (make-instance 'application-operation-test-tool
                                          :namespace "acp-test" :name "echo"
                                          :description "Exercise headless tool authority."
                                          :parameters  parameters))
              (arguments   (agentcomms:json-object
                            "text" "approved" "nested" (agentcomms:json-object
                                                        "CamelCase" ':null "false" (argo:json-false)))))
         (tool-registry-register (application-tool-registry application) tool)
         (let* ((catalog   (agentcomms:client-agent-request
                            client "_autolith/operations" (agentcomms:json-object "sessionId" identifier)))
                (items     (agentcomms:json-get (agentcomms:json-get catalog "value") "items"))
                (supported (find (tool-canonical-name tool) items :test #'equal
                                 :key                                   (lambda (row) (agentcomms:json-get row "name"))))
                (settings  (find "settings" items :test #'equal
                                 :key                   (lambda (row) (agentcomms:json-get row "name")))))
           (test-assert (equal "ok" (agentcomms:json-get catalog "outcome")) "the catalog is readable")
           (test-assert (eq t (agentcomms:json-get supported "admitted")) "idle tools are admitted")
           (test-assert (argo:json-false-p (gethash "terminalOwning" supported)) "tools do not own the terminal")
           (test-assert (eq t (agentcomms:json-get settings "terminalWithoutArguments"))
                        "the settings picker needs arguments for headless use")
           (test-assert (equal "array" (agentcomms:json-get
                                           (agentcomms:json-get settings "argumentSchema") "type"))
                        "command arguments are arrays"))
         (acp-session-cancel session)
         (let* ((response (agentcomms:client-agent-request
                           client "_autolith/invoke"
                           (agentcomms:json-object "sessionId" identifier
                                                  "operation" (tool-canonical-name tool) "arguments" arguments)))
                (outcome  (agentcomms:json-get response "outcome"))
                (decoded  (application-operation-test-tool-arguments tool)))
           (test-assert (equal "ok" outcome)
                        (format nil "invocation after cancellation succeeds (outcome ~S, condition type ~S)"
                                outcome (agentcomms:json-get response "conditionType")))
           (test-assert (equal "full-access allow: approved"
                               (agentcomms:json-get (agentcomms:json-get response "value") "result"))
                        "tool invocation uses the ACP client's authority")
           (test-assert (eq (application-agent application)
                            (tool-context-agent (application-operation-test-tool-context tool)))
                        "invocation retains the session agent")
           (test-assert (nth-value 1 (gethash "CamelCase" (gethash "nested" decoded)))
                        "object member names retain their case")
           (test-assert (null (gethash "CamelCase" (gethash "nested" decoded))) "tool null is not false")
           (test-assert (json-false-p (gethash "false" (gethash "nested" decoded))) "tool false is distinct"))
         (dolist (request (list
                           (agentcomms:json-object "operation" "settings"
                                                  "arguments" (vector (agentcomms:json-object "keyword" "turn-timestamps-p") t))
                           (agentcomms:json-object "operation" "permissions"
                                                  "arguments" (vector (agentcomms:json-object "keyword" "auto")))))
           (setf (gethash "sessionId" request) identifier)
           (let* ((response (agentcomms:client-agent-request client "_autolith/invoke" request))
                  (value    (agentcomms:json-get response "value")))
             (test-assert (equal "ok" (agentcomms:json-get response "outcome"))
                          "native command arguments invoke through ACP")
             (test-assert (and (nth-value 1 (gethash "result" value)) (null (gethash "result" value)))
                          "an actual NIL command result is present")))
         (test-assert (config :turn-timestamps-p (application-configuration application))
                      "keyword setting names and boolean values cross the ACP boundary")
         (acp-session-test-check-turns client '("invoke" "invoke" "invoke")
                                      '("end-turn" "end-turn" "end-turn"))
         (dolist (request (list
                           (agentcomms:json-object "operation" "settings" "arguments" (agentcomms:json-object))
                           (agentcomms:json-object "operation" (tool-canonical-name tool) "arguments" #())
                           (agentcomms:json-object "operation" "permissions" "arguments" #("auto"))
                           (agentcomms:json-object "operation" "permissions" "arguments"
                                                  (vector (agentcomms:json-object "keyword" 7)))
                           (agentcomms:json-object "operation" "permissions" "arguments"
                                                  (vector (agentcomms:json-object "keyword" "full" "extra" t)))))
           (setf (gethash "sessionId" request) identifier)
           (test-assert (equal "condition" (agentcomms:json-get
                                              (agentcomms:client-agent-request client "_autolith/invoke" request) "outcome"))
                        "invalid containers and keyword tags fail before execution"))
         (dolist (operation '("settings" "missing-operation"))
           (test-assert (equal "unsupported" (agentcomms:json-get
                                                (agentcomms:client-agent-request
                                                 client "_autolith/invoke"
                                                 (agentcomms:json-object "sessionId" identifier "operation" operation))
                                                "outcome"))
                        "unavailable operations have an explicit outcome"))
         (test-assert (= 1 (application-operation-test-tool-calls tool)) "the tool executes exactly once")))))
  nil)
