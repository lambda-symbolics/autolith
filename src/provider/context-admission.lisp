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

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (let ((generation (json-get request "generationConfig")))
    (reduce #'max
            (remove-if-not
             (lambda (value) (and (integerp value) (plusp value)))
             (list (config :context-output-reserve (provider-configuration provider))
                   *provider-maximum-output-tokens*
                   (json-get request "max_output_tokens")
                   (json-get request "max_completion_tokens")
                   (json-get request "max_tokens")
                   (and (json-object-p generation)
                        (json-get generation "maxOutputTokens")))))))

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
