(in-package #:autolith)

;;;; -- Contracted Tool Calls --

;;; Every tool call passes through its contracts, when any describe it. In
;;; active mode a call whose requirements do not hold is refused before it
;;; runs. In active and passive mode a successful call records its predicted
;;; effects and its observations. Calls no contract describes run unchanged.

(defclass contract-recording-result (tool-result)
  ((original
    :initarg :original :reader contract-recording-result-original
    :documentation "The successful external result, retaining its result protocol.")
   (prefix-length
    :initarg :prefix-length :reader contract-recording-result-prefix-length
    :type (integer 0)
    :documentation "Characters of external content retained before the diagnostic.")
   (diagnostic
    :initarg :diagnostic :reader contract-recording-result-diagnostic
    :type string
    :documentation "Bounded JSON distinguishing external success from mapping failure."))
  (:documentation "A successful external result with failed structured knowledge recording."))

(defgeneric tool-contract--prefix-details (result length)
  (:documentation "Retain RESULT's metadata for LENGTH leading external characters.")
  (:method ((result tool-result) length)
    (declare (ignore length))
    (tool-result-details result)))

(defmethod tool-result-details ((result contract-recording-result))
  "Retain subtype metadata, original clipped metadata, and the recording diagnostic."
  (let* ((original (tool-result-details (contract-recording-result-original result)))
         (retained (tool-contract--prefix-details
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
           (loop for error in errors for index below 8
                 collect (json-object "contract" (bounded-string (json-get error "contract") :limit 80)
                                      "mapping" (json-get error "mapping")
                                      "message" (bounded-string (json-get error "message") :limit 200))))
         (lower 0)
         (upper (length content)))
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
        (retained nil))
    (dolist (block blocks)
      (etypecase block
        (string
         (let ((text (subseq block 0 (min remaining (length block)))))
           (push text retained)
           (decf remaining (length text))))
        (image-attachment (push block retained))))
    (append (nreverse retained) (list annotation))))

(-> tool-contract--recording-result (tool-result list) tool-result)
(defun tool-contract--recording-result (result errors)
  "Decorate external success with a bounded provider-visible recording diagnostic.

Retain image order and subtype metadata. When text must be shortened, keep its
original metadata as provenance and adapt visible section ranges to its prefix."
  (let* ((rich-p (or (tool-result-content-blocks result) (tool-result-details result)))
         (diagnostic (tool-contract--recording-diagnostic
                      result errors :include-output-p (not rich-p) :limit (if rich-p 2000 8000)))
         (annotation (concatenate 'string (string #\Newline) diagnostic))
         (content (tool-result-content result))
         (images (tool-result-image-attachments result))
         (prefix (if images (length content) (min (length content) (- 8000 (length annotation))))))
    (make-instance 'contract-recording-result
                   :original result :diagnostic diagnostic :prefix-length (max 0 prefix)
                   :content (cond
                              ((not rich-p) diagnostic)
                              (images content)
                              (t (concatenate 'string (subseq content 0 prefix) annotation)))
                   :content-blocks (when rich-p
                                     (tool-contract--annotated-blocks
                                      (or (tool-result-content-blocks result) (list content)) annotation))
                   :image-attachments images
                   :success-p (tool-result-success-p result)
                   :category (tool-result-category result)
                   :error-code (tool-result-error-code result)
                   :denied-p (tool-result-denied-p result))))

(-> tool-execute-with-contracts (tool tool-context json-object) tool-result)
(defun tool-execute-with-contracts (tool context arguments)
  "Execute TOOL with ARGUMENTS under CONTEXT, honoring the contracts describing the call."
  (let* ((configuration (tool-context-configuration context))
         (name (tool-canonical-name tool))
         (matches (and configuration
                       (knowledge-capture-p configuration)
                       (handler-case (knowledge-contracts-for-call configuration name arguments)
                         (knowledge-error (condition)
                           (return-from tool-execute-with-contracts
                             (tool-failure
                              (format nil "A contract for ~A could not read the call: ~A The call was not made."
                                      name (autolith-error-message condition))
                              :code ':precondition-unknown)))))))
    (if (null matches)
        (tool-execute tool context arguments)
        (multiple-value-bind (refusal admitted)
            (knowledge-contract-refusal configuration matches)
          (if (and refusal (knowledge-active-p configuration))
              (tool-failure refusal :code ':precondition-unmet)
              (let ((result (tool-execute tool context arguments)))
                (if (not (tool-result-success-p result))
                    result
                    (multiple-value-bind (claims errors)
                        (knowledge-contract-record
                         configuration (or admitted matches) arguments result
                         :tool name
                         :conversation (and (tool-context-conversation context)
                                            (conversation-identifier
                                             (tool-context-conversation context))))
                      (declare (ignore claims))
                      (if errors
                          (tool-contract--recording-result result errors)
                          result)))))))))
