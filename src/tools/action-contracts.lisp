(in-package #:autolith)

;;;; -- Contracted Tool Calls --

(defclass contract-recording-result (tool-result)
  ((original
    :initarg :original :reader contract-recording-result-original
    :documentation "The successful external result, retaining its result protocol.")
   (prefix-length
    :initarg :prefix-length :reader contract-recording-result-prefix-length
    :type          (integer 0)
    :documentation "Number of characters retained from external content before the diagnostic.")
   (diagnostic
    :initarg :diagnostic :reader contract-recording-result-diagnostic
    :type          string
    :documentation "Bounded JSON distinguishing external success from mapping failure."))
  (:documentation "A successful external result with failed structured knowledge recording."))

(defgeneric tool-contract--prefix-details (result length)
  (:documentation "Retain RESULT's metadata for the first LENGTH characters of external content.")
  (:method ((result tool-result) length)
    (declare (ignore length))
    (tool-result-details result)))

(defmethod tool-result-details ((result contract-recording-result))
  "Return subtype metadata and the recording diagnostic, with original metadata as provenance."
  (let* ((original   (tool-result-details (contract-recording-result-original result)))
         (retained   (tool-contract--prefix-details
                      (contract-recording-result-original result)
                      (contract-recording-result-prefix-length result)))
         (diagnostic (contract-recording-result-diagnostic result)))
    (if retained
        (append retained (unless (equal original retained) (list :external-details original))
                (list :contract-recording diagnostic))
        (list :contract-recording :diagnostic diagnostic))))

(-> tool-contract--recording-diagnostic
    (tool-result list &key (:include-output-p boolean) (:limit integer)) string)
(defun tool-contract--recording-diagnostic (result errors &key (include-output-p t) (limit 8000))
  "Return valid bounded JSON, accounting for escaping before clipping the external excerpt."
  (let* ((content (if include-output-p (tool-result-content result) ""))
         (summaries
           (loop for error in (take 8 errors)
                 collect (json-object "contract" (bounded-string (json-get error "contract") :limit 80)
                                      "mapping" (json-get error "mapping")
                                      "message" (bounded-string (json-get error "message") :limit 200))))
         (lower   0)
         (upper   (length content)))
    (labels ((encode (count)
               (let ((object (json-object "externalSuccess" t "contractRecording" "failed"
                                          "contractErrorCount" (length errors)
                                          "contractErrors" (coerce summaries 'vector))))
                 (when include-output-p
                   (setf (gethash "externalResult" object) (subseq content 0 count)
                         (gethash "externalResultTruncated" object)
                         (if (< count (length content)) t :json-false)))
                 (json-encode object))))
      (loop while (> (length (encode 0)) limit)
            do (setf summaries (butlast summaries)))
      (loop while (< lower upper)
            for middle = (ceiling (+ lower upper) 2)
            do (if (<= (length (encode middle)) limit)
                   (setf lower middle)
                   (setf upper (1- middle))))
      (encode lower))))

(-> tool-contract--annotated-blocks (list string) list)
(defun tool-contract--annotated-blocks (blocks annotation)
  "Reserve text budget for ANNOTATION after the original ordered image and text blocks."
  (let ((remaining (- 8000 (length annotation)))
        (retained  nil))
    (dolist (block blocks)
      (etypecase block
        (string
         (let ((text (subseq block 0 (min remaining (length block)))))
           (push text retained)
           (decf remaining (length text))))
        (image-attachment
         (push block retained))))
    (append (nreverse retained) (list annotation))))

(-> tool-contract--recording-result (tool-result list) tool-result)
(defun tool-contract--recording-result (result errors)
  "Add a bounded provider-visible recording diagnostic to a successful external result.

Retain image order and subtype metadata. If the text is shortened, retain the
original metadata as provenance and adjust section ranges to the retained prefix."
  (let* ((rich-p     (or (tool-result-content-blocks result) (tool-result-details result)))
         (diagnostic (tool-contract--recording-diagnostic
                      result errors :include-output-p (not rich-p) :limit (if rich-p 2000 8000)))
         (annotation (concatenate 'string (string #\Newline) diagnostic))
         (content    (tool-result-content result))
         (images     (tool-result-image-attachments result))
         (prefix     (if images (length content) (min (length content) (- 8000 (length annotation))))))
    (make-instance 'contract-recording-result
                   :original result :diagnostic diagnostic :prefix-length (max 0 prefix)
                   :content           (cond
                                        ((not rich-p)
                                         diagnostic)
                                        (images
                                         content)
                                        (t
                                         (concatenate 'string (subseq content 0 prefix) annotation)))
                   :content-blocks    (when rich-p
                                        (tool-contract--annotated-blocks
                                         (or (tool-result-content-blocks result) (list content)) annotation))
                   :image-attachments images
                   :success-p         (tool-result-success-p result)
                   :category          (tool-result-category result)
                   :error-code        (tool-result-error-code result)
                   :denied-p          (tool-result-denied-p result))))

(-> tool-contracts-for-call
    (configuration tool json-object &key (:registry (option tool-registry))) list)
(defun tool-contracts-for-call (configuration tool arguments &key registry)
  "Return contract matches for TOOL's canonical name and unambiguous display alias.

When REGISTRY is supplied, include display-name contracts only if the alias
resolves to TOOL. Without a registry, match only the canonical name."
  (let ((display   (tool-display-name tool))
        (canonical (tool-canonical-name tool)))
    (append (knowledge-contracts-for-call configuration canonical arguments)
            (and registry
                 (string/= display canonical)
                 (eq tool (tool-registry-resolve registry display))
                 (knowledge-contracts-for-call configuration display arguments)))))

(-> tool-execute-with-contracts (tool tool-context json-object) tool-result)
(defun tool-execute-with-contracts (tool context arguments)
  "Execute TOOL with ARGUMENTS under CONTEXT, applying matching contracts.

Match canonical names and unique display aliases through CONTEXT's registry."
  (let* ((configuration (tool-context-configuration context))
         (name          (tool-canonical-name tool))
         (matches       (and configuration
                             (knowledge-capture-p configuration)
                             (handler-case (tool-contracts-for-call
                                            configuration tool arguments
                                            :registry (tool-context-registry context))
                               (knowledge-error (condition)
                                 (return-from tool-execute-with-contracts
                                   (tool-failure
                                    (format nil "Contract matching failed for ~A: ~A The call was not made."
                                             name (autolith-error-message condition))
                                    :code ':precondition-unknown)))))))
    (if (null matches)
        (tool-execute tool context arguments)
        (multiple-value-bind (refusal admitted)
            (knowledge-contract-refusal configuration matches)
          (if (and refusal (knowledge-active-p configuration))
              (tool-failure refusal :code ':precondition-unmet)
              (let ((result (tool-execute tool context arguments)))
                (if (or (not (tool-result-success-p result))
                        (tool-result-handed-off-p result))
                    result
                    (multiple-value-bind (claims errors)
                        (knowledge-contract-record
                         configuration (or admitted matches)
                         :arguments    arguments
                         :result       result
                         :tool         name
                         :conversation (and (tool-context-conversation context)
                                            (conversation-identifier
                                             (tool-context-conversation context))))
                      (declare (ignore claims))
                      (if errors
                          (tool-contract--recording-result result errors)
                          result)))))))))
