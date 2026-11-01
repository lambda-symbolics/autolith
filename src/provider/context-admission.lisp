(in-package #:autolith)

;;;; -- Request Context Admission --

(defparameter *provider-image-token-estimate* 16384
  "Portable admission allowance per image when a provider supplies no token count.

This deliberately avoids counting base64 bytes or URL length as image tokens;
provider-specific token accounting may refine the estimate.")

(defgeneric provider-request-input-token-estimate (provider request)
  (:documentation
   "Return REQUEST's estimated input tokens and whether non-text estimates were needed.

REQUEST is the final encoded request, including selected tools and temporary
context. This local admission estimate is independent of paid usage. Methods
may use provider token counts when available; the default is not a tokenizer."))

(defmethod provider-request-input-token-estimate ((provider model-provider) (request hash-table))
  "Estimate the actual encoded prompt at four characters per text token."
  (declare (ignore provider))
  (multiple-value-bind (characters non-text-p)
      (provider-context--request-characters request)
    (values (ceiling characters 4) non-text-p)))

(-> provider-context--request-characters (t) (values (integer 0) boolean))
(defun provider-context--request-characters (value)
  "Count encoded VALUE with bounded image allowances and flag opaque/media estimates."
  (typecase value
    (hash-table
     (if (provider-context--image-p value)
         (values (* 4 *provider-image-token-estimate*) t)
         (let ((characters 2)
               (non-text-p nil))
           (maphash
            (lambda (key item)
              (multiple-value-bind (count uncertain-p)
                  (provider-context--request-characters item)
                (incf characters (+ 4 (length (princ-to-string key)) count))
                (when (or uncertain-p (equal key "encrypted_content"))
                  (setf non-text-p t))))
            value)
           (values characters non-text-p))))
    (string
     (values (+ 2 (length value)) nil))
    (vector
     (let ((characters 2)
           (non-text-p nil))
       (loop for item across value
             do (multiple-value-bind (count uncertain-p)
                    (provider-context--request-characters item)
                  (incf characters (1+ count))
                  (when uncertain-p
                    (setf non-text-p t))))
       (values characters non-text-p)))
    (t
     (values (length (princ-to-string value)) nil))))

(-> provider-context--image-p (hash-table) boolean)
(defun provider-context--image-p (value)
  "Recognize image blocks in Responses, Chat, Messages and GenerateContent requests."
  (let ((type (json-get value "type"))
        (mime (or (json-get value "mimeType") (json-get value "mime_type"))))
    (not (null (or (member type '("image" "input_image" "image_url") :test #'equal)
                   (and (stringp mime) (uiop:string-prefix-p "image/" mime)))))))

(-> provider-request-output-ceilings (hash-table &key (:enclosed-p boolean)) list)
(defun provider-request-output-ceilings (request &key enclosed-p)
  "Return positive caller and wire ceilings, optionally inside routing envelopes."
  (let ((generation (json-get request "generationConfig"))
        (inner      (json-get request "request")))
    (append
     (remove-if-not
      (lambda (value) (and (integerp value) (plusp value)))
      (list *provider-maximum-output-tokens*
            (json-get request "max_output_tokens")
            (json-get request "max_completion_tokens")
            (json-get request "max_tokens")
            (and (json-object-p generation)
                 (json-get generation "maxOutputTokens"))))
     ;; Code Assist carries the GenerateContent policy inside its routing envelope.
     (when (and enclosed-p (json-object-p inner))
       (provider-request-output-ceilings inner :enclosed-p t)))))

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (reduce #'max (provider-request-output-ceilings request)
          :initial-value (config :context-output-reserve (provider-configuration provider))))

(-> provider-check-request-context (model-provider hash-table) null)
(defun provider-check-request-context (provider request)
  "Reject an oversized final projection before network I/O, without consuming context."
  (multiple-value-bind (input non-text-p)
      (provider-request-input-token-estimate provider request)
    (let ((reserve (provider-request-output-reserve provider request))
          (window  (config :context-window (provider-configuration provider))))
      (when (> (+ input reserve) window)
        (error 'provider-context-budget-error
               :message                (format nil
                                               "The next request needs approximately ~D input tokens plus ~D output tokens; the model context window is ~D. Compact the conversation or reduce request context before retrying."
                                               input reserve window)
               :estimated-input-tokens input
               :output-reserve         reserve
               :context-window         window
               :non-text-estimate-p    non-text-p))))
  nil)

(defmethod provider-open-response-stream :before
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Admit the fully encoded streaming request after provider-specific modifications."
  (declare (ignore credentials conversation))
  (provider-check-request-context provider request))

(defmethod provider-open-native-compaction :before
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Apply context admission to native compaction requests as well as ordinary turns."
  (declare (ignore credentials conversation))
  (provider-check-request-context provider request))

(define-condition provider-context-budget-error (autolith-error)
  ((estimated-input-tokens
    :initarg       :estimated-input-tokens
    :reader        provider-context-budget-error-estimated-input-tokens
    :type          (integer 0)
    :documentation "Estimated input size of the actual encoded request.")
   (output-reserve
    :initarg       :output-reserve
    :reader        provider-context-budget-error-output-reserve
    :type          (integer 1)
    :documentation "Output allowance reserved beside the input.")
   (context-window
    :initarg       :context-window
    :reader        provider-context-budget-error-context-window
    :type          (integer 1)
    :documentation "The configured shared input/output context window.")
   (non-text-estimate-p
    :initarg       :non-text-estimate-p
    :reader        provider-context-budget-error-non-text-estimate-p
    :type          boolean
    :documentation "Whether image or opaque payload estimates affected admission."))
  (:documentation "Local context admission refused a request before transport."))

(-> provider-call-with-mission-output-allowance (model-provider hash-table function) t)
(defun provider-call-with-mission-output-allowance (provider request continuation)
  "Bound internal mission headroom at the final encoded request boundary.

Validate the original caller policy first. Only the mission-derived allowance
may shrink to the context envelope. Re-estimate after adding the wire field,
then recheck the actual final request. A nonmonotonic custom estimator may
refuse the reduced candidate, but cannot bypass ordinary admission."
  (if (null *provider-mission-output-allowance*)
      (funcall continuation request)
      (let ((ceilings (provider-request-output-ceilings request :enclosed-p t)))
        (let ((*provider-maximum-output-tokens*
                (when ceilings (reduce #'max ceilings))))
          (provider-check-request-context provider request))
        (let* ((window    (config :context-window (provider-configuration provider)))
               (ceiling
                 (reduce #'min ceilings
                         :initial-value
                         (min *provider-mission-output-allowance*
                              (- window (provider-request-input-token-estimate provider request)))))
               (candidate (provider-request-with-output-ceiling provider request ceiling))
               (remaining (- window (provider-request-input-token-estimate provider candidate))))
          (if (plusp remaining)
              (let* ((effective                        (min ceiling remaining))
                     (final-request
                       (if (= effective ceiling)
                           candidate
                           (provider-request-with-output-ceiling provider request effective)))
                     (*provider-maximum-output-tokens* effective))
                (provider-check-request-context provider final-request)
                (funcall continuation final-request))
              ;; No positive output envelope exists for this encoded candidate.
              (let ((*provider-maximum-output-tokens* ceiling))
                (provider-check-request-context provider candidate)))))))

(defmethod provider-open-native-compaction :around
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Finalize mission output headroom before native compaction admission."
  (provider-call-with-mission-output-allowance
   provider request
   (lambda (final-request)
     (call-next-method provider final-request :credentials credentials
                                            :conversation  conversation))))
