(in-package #:autolith)

;;;; -- Parent Tool Contracts --

(defmethod tool-context-parent-contracts-p ((context tool-context))
  "Use the parent contract path whenever parent record services are installed."
  (not (null (agent-record-resource--scope context))))

(defmethod tool-execute-parent-contracts ((tool tool) (context tool-context) arguments)
  "Check and record through parent knowledge, without opening a child private store."
  (let* ((configuration (tool-context-configuration context))
         (scope         (agent-record-resource--scope context))
         (adapter       (and scope (agent-record-resource-scope-knowledge scope))))
    (unless (knowledge-capture-p configuration)
      (return-from tool-execute-parent-contracts (tool-execute tool context arguments)))
    (unless adapter
      (return-from tool-execute-parent-contracts
        (tool-failure "The parent knowledge service is not available. The call was not made."
                      :code ':precondition-unknown)))
    (let* ((fields
             (handler-case
                 (agent-protocol-knowledge-response-fields
                  (agent-protocol-knowledge-request
                   adapter ':check
                   :fields (json-object "tool" (tool-canonical-name tool) "arguments" arguments)))
               ((or agent-protocol-error agent-protocol-knowledge-error) ()
                 (return-from tool-execute-parent-contracts
                   (tool-failure "The parent could not check tool contracts. The call was not made."
                                 :code ':precondition-unknown)))))
           (refusal (gethash "refusal" fields))
           (handle  (gethash "handle" fields)))
      (when (and refusal (knowledge-active-p configuration))
        (return-from tool-execute-parent-contracts
          (tool-failure refusal :code ':precondition-unmet)))
      (let ((succeeded-p nil))
        (unwind-protect
             (let ((result (tool-execute tool context arguments)))
               (setf succeeded-p (tool-result-success-p result))
               (if (or (not handle) (not succeeded-p))
                   result
                   (let ((errors
                           (handler-case
                               (coerce
                                (gethash "errors"
                                         (agent-protocol-knowledge-response-fields
                                          (agent-protocol-knowledge-request
                                           adapter ':record :operation-id (make-identifier)
                                           :fields                        (json-object "handle" handle
                                                                                       "result" (tool-result-content result)))))
                                'list)
                             ((or agent-protocol-error agent-protocol-knowledge-error) ()
                               (list (json-object "contract" "parent" "mapping" "record"
                                                  "message" "The tool succeeded, but parent knowledge recording did not complete."))))))
                     (if errors
                         (tool-contract--recording-result result errors)
                         result))))
          (when (and handle (not succeeded-p))
            (handler-case
                (agent-protocol-knowledge-request
                 adapter ':record :operation-id (make-identifier)
                 :fields                        (json-object "handle" handle "outcome" "abandoned"))
              ((or agent-protocol-error agent-protocol-knowledge-error) () nil))))))))
