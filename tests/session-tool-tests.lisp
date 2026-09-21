(in-package #:autolith)

;;;; -- Durable Conversation Search Tool Tests --

(-> session-tool-tests--call (tool-registry tool-context string json-object) tool-result)
(defun session-tool-tests--call (registry context name arguments)
  "Execute one session tool call through REGISTRY."
  (tool-registry-execute-call
   registry
   (json-object "namespace" "session"
                "name" name
                "arguments" (json-encode arguments))
   context))

(-> test-session-search-and-read-tools () null)
(defun test-session-search-and-read-tools ()
  "Search durable message text then page the matching conversation window."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration :identifier "session-search-test"))
         (registry (session-augment-tool-registry (make-instance 'tool-registry)))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry registry)))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "Needle phrase for durable recall.")
           (conversation-append-user-message conversation "A neighboring durable message.")
           (let ((search-result
                   (session-tool-tests--call registry context "search"
                                             (json-object "query" "needle phrase"))))
              (test-assert (search "session-search-test:0"
                                   (tool-result-content search-result))
                           "session.search returns the matching message offset")
              (let ((read-result
                      (session-tool-tests--call registry context "read"
                                                (json-object "conversation-id" "session-search-test"
                                                             "offset" 0
                                                             "context" 1))))
                (test-assert (search "Needle phrase" (tool-result-content read-result))
                             "session.read includes the target message")
                (test-assert (search "neighboring durable" (tool-result-content read-result))
                             "session.read includes the requested context window"))))
      (ignore-errors (conversation-delete configuration "session-search-test")))))