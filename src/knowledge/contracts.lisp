(in-package #:autolith)

(-> knowledge-contract-record
    (configuration list &key (:arguments hash-table) (:result t) (:tool string)
                   (:conversation (option string)))
    (values list list))
(defun knowledge-contract-record (configuration matches &key arguments result tool conversation)
  "Return asserted claim identifiers and structured mapping errors for a successful call.

Requirement claim identities are retained in the evidence source. Mapping errors
never change the external action's outcome and never disappear silently."
  (let* ((content (tool-result-content result))
         (decoded (handler-case (json-decode content)
                    (json-error ()
                      nil))))
    (knowledge-contract-record-data configuration matches
                                    :arguments      arguments
                                    :decoded-result decoded
                                    :result-content content
                                    :tool           tool
                                    :conversation   conversation)))
