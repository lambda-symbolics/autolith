(in-package #:autolith)

;;;; -- Operation Extension Boundary Tests --

(-> test-acp-extension-operations-and-invoke () null)
(defun test-acp-extension-operations-and-invoke ()
  "Invoke an actual registered tool with ACP authority and lossless JSON arguments."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (tool (make-instance 'application-operation-test-tool
                                   :namespace "acp-test" :name "echo"
                                   :description "Exercise headless tool authority."
                                   :parameters (tool-object-schema (json-object) nil)))
              (arguments (agentcomms:json-object
                          "text" "approved" "nested" (agentcomms:json-object
                                                    "CamelCase" ':null "false" (argo:json-false)))))
         (tool-registry-register (application-tool-registry (acp-session-application session)) tool)
         (let* ((catalog (agentcomms:client-agent-request
                          client "_autolith/operations"
                          (agentcomms:json-object "sessionId" identifier)))
                (items (agentcomms:json-get (agentcomms:json-get catalog "value") "items"))
                (supported (find (tool-canonical-name tool) items :test #'equal
                                 :key (lambda (row) (agentcomms:json-get row "name"))))
                (terminal (find "settings" items :test #'equal
                                :key (lambda (row) (agentcomms:json-get row "name")))))
           (test-assert (equal "ok" (agentcomms:json-get catalog "outcome")) "the catalog is readable")
           (test-assert (and supported (eq t (agentcomms:json-get supported "admitted")))
                        "idle tools are admitted even when held during an active turn")
            (test-assert (and terminal (eq t (agentcomms:json-get terminal "terminalWithoutArguments")))
                         "the settings picker requires arguments for headless use"))
         (acp-service--call-with-operation
          service session
          (lambda ()
            (let* ((catalog (agentcomms:client-agent-request
                             client "_autolith/operations"
                             (agentcomms:json-object "sessionId" identifier)))
                   (row (find (tool-canonical-name tool)
                              (agentcomms:json-get (agentcomms:json-get catalog "value") "items")
                              :test #'equal :key (lambda (row) (agentcomms:json-get row "name")))))
              (test-assert (argo:json-false-p (gethash "admitted" row))
                           "the readable catalog reflects active primary admission"))))
         (acp-session-cancel session)
         (let* ((result (agentcomms:client-agent-request
                         client "_autolith/invoke"
                         (agentcomms:json-object "sessionId" identifier
                                                "operation" (tool-canonical-name tool) "arguments" arguments)))
                (decoded (application-operation-test-tool-arguments tool)))
           (test-assert (equal "ok" (agentcomms:json-get result "outcome"))
                        "explicit invocation after cancellation succeeds")
           (test-assert (equal "full-access allow: approved"
                               (agentcomms:json-get (agentcomms:json-get result "value") "text"))
                        "command and tool permissions use the ACP client")
           (test-assert (= 1 (application-operation-test-tool-calls tool)) "the tool executes exactly once")
           (test-assert (eq (application-agent (acp-session-application session))
                            (tool-context-agent (application-operation-test-tool-context tool)))
                        "invocation retains the session agent")
           (test-assert (nth-value 1 (gethash "CamelCase" (gethash "nested" decoded)))
                        "JSON member names retain their case")
           (test-assert (null (gethash "CamelCase" (gethash "nested" decoded))) "JSON null is not false")
           (test-assert (json-false-p (gethash "false" (gethash "nested" decoded))) "JSON false is distinct"))
         (dolist (request (list
                             (agentcomms:json-object "operation" "skills")
                             (agentcomms:json-object "operation" "skills")
                             (agentcomms:json-object "operation" "hurry-up"
                                                    "arguments"
                                                    (agentcomms:json-object "input" "on"))
                             (agentcomms:json-object "operation" "hurry-up"
                                                    "arguments"
                                                    (agentcomms:json-object "text" "off"))))
           (setf (gethash "sessionId" request) identifier)
           (let* ((result (agentcomms:client-agent-request client "_autolith/invoke" request))
                  (value (agentcomms:json-get result "value"))
                  (text (and value (agentcomms:json-get value "text"))))
             (test-assert (equal "ok" (agentcomms:json-get result "outcome"))
                          "ordinary application commands invoke through ACP")
             (test-assert
              (or (not (string= "skills" (agentcomms:json-get request "operation")))
                  (and (stringp text) (plusp (length text))))
                "ordinary command invocation returns presented text when it presents")))
         (let ((request (agentcomms:json-object
                         "sessionId" identifier
                         "operation" "permissions"
                         "arguments" (agentcomms:json-object "input" "auto"))))
           (let ((result (agentcomms:client-agent-request client "_autolith/invoke" request)))
             (test-assert (equal "ok" (agentcomms:json-get result "outcome"))
                          "exclusive-without-arguments command accepts supplied arguments")
             (test-assert (plusp (length (agentcomms:json-get
                                          (agentcomms:json-get result "value") "text")))
                          "argument-bearing command returns presented text")))
         (acp-session-test-check-turns
          client
          '("invoke" "invoke" "invoke" "invoke" "invoke" "invoke")
          '("end-turn" "end-turn" "end-turn" "end-turn" "end-turn" "end-turn"))
         (dolist (request (list (agentcomms:json-object "operation" "settings")
                                (agentcomms:json-object "operation" "missing-operation")))
           (setf (gethash "sessionId" request) identifier)
           (test-assert (equal "unsupported"
                               (agentcomms:json-get
                                (agentcomms:client-agent-request client "_autolith/invoke" request) "outcome"))
                        "unavailable adapters return an explicit unsupported outcome"))
         (test-assert (equal "condition"
                             (agentcomms:json-get
                              (agentcomms:client-agent-request
                               client "_autolith/invoke"
                               (agentcomms:json-object "sessionId" identifier "operation" (tool-canonical-name tool)
                                                      "arguments" "not an object")) "outcome"))
                      "invalid arguments fail before tool execution")))))
  nil)
