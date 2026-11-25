(in-package #:autolith)

;;;; -- Request Context Admission --


(-> provider-effective-context-window (model-provider) (integer 1))
(defun provider-effective-context-window (provider)
  "Return the window PROVIDER's requests are checked against.

That is the configured window. A compaction request is checked against the
window the model takes when that one is larger: compacting is how a
conversation gets back under the configured window."
  (let ((configured (config :context-window (provider-configuration provider)))
        (model      (and (eq provider *compaction-provider*)
                         (provider-compaction-recovery-window provider))))
    (if (integerp model)
        (max configured model)
        configured)))

(defparameter *provider-context-admission-state* nil
  "Request-local admission state shared with mission accounting.

FIRST is :PENDING until a provider admits the request, then :ENTERED.
REST holds the condition from a local refusal. Function entry and condition
class do not establish zero usage.")

(defparameter *provider-image-token-estimate* nil
  "Token allowance per image when provider token counts are unavailable.

NIL charges each image at the local preparation cap, *IMAGE-INPUT-MAXIMUM-PATCHES*,
since prepared images never exceed it. Bind an integer for a different policy.")

(-> provider-image-token-allowance () (integer 0))
(defun provider-image-token-allowance ()
  "Return the estimated input tokens charged for one image."
  (or *provider-image-token-estimate* *image-input-maximum-patches*))

(defgeneric provider-request-input-token-estimate (provider request)
  (:documentation
   "Return REQUEST's estimated input tokens and whether non-text estimates were needed.

REQUEST is the final encoded request, including selected tools and temporary
context. This local admission estimate is independent of paid usage. Methods
may use provider token counts when available; the default is not a tokenizer."))

(defmethod provider-request-input-token-estimate ((provider model-provider) (request hash-table))
  "Use matched ordinary usage, otherwise estimate the final encoded prompt."
  (multiple-value-bind (tokens matched-p uncertain-p)
      (provider-context--matched-input provider request)
    (if matched-p
        (values tokens uncertain-p)
        (provider-context--input-tokens provider nil (provider-request-body provider request)))))

(-> provider-context--soft-header (model-provider vector) (option list))
(defun provider-context--soft-header (provider tools)
  "Fingerprint prompt policy and tools when the provider owns a configuration."
  (when-let ((configuration (and (compute-applicable-methods #'provider-configuration (list provider))
                                 (provider-configuration provider))))
    (provider-cache-prefix--entry
     (vector (symbol-name (class-name (class-of provider)))
             (config :model configuration) (config :model-provider configuration)
              (config :reasoning-effort configuration) (config :provider-endpoint configuration)
             (system-prompt configuration) tools))))

(-> provider-context--header-components (model-provider vector) list)
(defun provider-context--header-components (provider tools)
  "Separate stable accounting policy from replaceable prompt and tool text."
  (when-let ((configuration (and (compute-applicable-methods #'provider-configuration (list provider))
                                 (provider-configuration provider))))
    (let ((prompt (system-prompt configuration)))
      (list :policy      (provider-cache-prefix--entry
                          (vector (symbol-name (class-name (class-of provider)))
                                  (config :model configuration) (config :model-provider configuration)
                                  (config :reasoning-effort configuration) (config :provider-endpoint configuration)))
            :prompt      (provider-cache-prefix--entry prompt)
            :tool-header (provider-cache-prefix--entry tools)
            :header-text (list prompt (json-encode tools))))))

(-> provider-context--header-charge (list list) (integer 0))
(defun provider-context--header-charge (sample components)
  "Charge one token per inserted or replacement UTF-8 byte of changed headers."
  (loop for key in '(:prompt :tool-header)
        for previous in (or (getf sample :header-text) '("" ""))
        for current in (getf components :header-text)
        unless (equal (getf sample key) (getf components key))
          sum (cl-llm-provider-api:context-inserted-utf8-bytes previous current)))

(-> provider-context--history (model-provider conversation) list)
(defun provider-context--history (provider conversation)
  "Fingerprint compatible retained history without storing prompt bodies."
  (mapcar #'provider-cache-prefix--entry
          (provider-history-items provider conversation :include-ephemeral-p nil)))

(-> provider-context--extends-p (list list) boolean)
(defun provider-context--extends-p (previous current)
  "Return whether CURRENT preserves every entry of PREVIOUS, including empty items."
  (and (<= (length previous) (length current))
       (equal previous (subseq current 0 (length previous))) t))

(-> provider-context-request-create (model-provider conversation vector) list)
(defun provider-context-request-create (provider conversation tools)
  "Start ordinary request-local evidence; ephemera require the unmeasured fallback."
  (let ((header (provider-context--soft-header provider tools)))
    ;; Preallocate mutable fields to preserve the shared evidence plist head.
    (append (and header (provider-context--header-components provider tools))
     (list :provider provider :conversation conversation
          :history         (and header (provider-context--history provider conversation))
          :soft-header     header
          :eligible-p      (and header (null (conversation-ephemeral-input-entries conversation)) t)
          :encoder nil :projection nil :tools nil :wire-provider nil :wire nil
          :wire-characters 0))))

(-> provider-context--wire-plan (model-provider hash-table) list)
(defun provider-context--wire-plan (provider request)
  "Fingerprint the encoder's measured-input projection."
  (provider-cache-wire-plan provider (provider-request-context-body provider request)))

(-> provider-context--prefix-matches-p (list (integer 0) list) boolean)
(defun provider-context--prefix-matches-p (fingerprint count entries)
  "Verify COUNT retained ENTRIES against a portable prefix FINGERPRINT."
  (and (<= count (length entries))
       (equal fingerprint
              (provider-cache-prefix--entry (coerce (subseq entries 0 count) 'vector)))
       t))

(-> provider-context--sample-matches-p (list (option list) list &key (:components list)) boolean)
(defun provider-context--sample-matches-p (sample header history &key components)
  "Verify measured history under exact headers or a format-two stable policy."
  (and (conversation-context-usage-sample-p sample)
       header
       (or (equal (getf sample :soft-header) header)
           (and (= 2 (getf sample :format)) components
                (equal (getf sample :policy) (getf components :policy))))
       (provider-context--prefix-matches-p
        (getf sample :history-digest) (getf sample :history-count) history)
       t))

(-> provider-context--switch-source-model (codex-subscription-provider list) (option string))
(defun provider-context--switch-source-model (provider sample)
  "Identify the measured model under the same Codex endpoint and provider."
  (let* ((configuration (provider-configuration provider))
         (registration  (model-provider-registration provider))
         (name          (config :model-provider configuration))
         (model-digest  (rest (assoc "model" (getf sample :encoder-fields) :test #'equal))))
    (when (and registration model-digest)
      (loop for candidate in (provider-registration-models registration)
            for model = (provider-model-name candidate)
            when (and (equal model-digest (provider-cache-prefix--entry model))
                      (loop for effort in (configuration--reasoning-efforts-for model name)
                            thereis (equal (getf sample :policy)
                                           (provider-cache-prefix--entry
                                            (vector (symbol-name (class-name (class-of provider)))
                                                    model name effort
                                                    (config :provider-endpoint configuration))))))
              return model))))

(-> provider-context--switch-history (model-provider conversation &key (:sample list) (:current list))
    (values list boolean))
(defun provider-context--switch-history (provider conversation &key sample current)
  "Check which CURRENT items are covered by a compatible Codex measurement.

Verify the original measured prefix. Only sealed reasoning may disappear from
that prefix. Count every new item and take no credit for removed reasoning."
  (block nil
    (unless (and (typep provider 'codex-subscription-provider)
                 (conversation-context-usage-sample-p sample)
                 (= 2 (getf sample :format)))
      (return (values nil nil)))
    (let* ((model (provider-context--switch-source-model provider sample))
           (count (getf sample :history-count))
           (old   (and model
                       (conversation-input-items-for-family
                        conversation (provider-family provider) :include-ephemeral-p nil :model model))))
      (unless (and model (plusp count) (<= count (getf sample :wire-count))
                   (provider-context--prefix-matches-p
                    (getf sample :history-digest) count
                    (mapcar #'provider-cache-prefix--entry old)))
        (return (values nil nil)))
      (let* ((model-changed-p (not (equal model (config :model (provider-configuration provider)))))
             (prefix          (subseq old 0 count))
             (covered         (if model-changed-p
                                  (remove-if #'encrypted-reasoning-item-p prefix)
                                  prefix))
             (added           nil))
        (dolist (item current)
          (cond
            ((and covered (eq item (first covered)))
             (pop covered))
            ((or (null covered) (and model-changed-p (encrypted-reasoning-item-p item)))
             (push item added))
            (t
             (return (values nil nil)))))
        (if covered
            (values nil nil)
            (values (nreverse added) t))))))

(-> provider-context--request-items (model-provider hash-table) (option vector))
(defun provider-context--request-items (provider request)
  "Return the encoder's declared conversation sequence."
  (values (provider-request-history provider request)))

(-> provider-context--input-tokens ((option model-provider) list t &key (:replaced-characters (integer 0))) (values (integer 0) boolean))
(defun provider-context--input-tokens (provider sample additions &key (replaced-characters 0))
  "Retain measured occupancy, charge additions and replace measured temporary context."
  (multiple-value-bind (characters uncertain-p)
      (if additions (provider-context--request-characters provider additions) (values 0 nil))
    (values (max 0 (+ (getf sample :tokens 0)
                      (ceiling (- characters
                                  (if (and sample (vectorp additions)
                                           (not (stringp additions))) 2 0)
                                  replaced-characters)
                               4)))
            uncertain-p)))

(-> provider-context--current-wire-p (list hash-table list) boolean)
(defun provider-context--current-wire-p (context request sample)
  "Reproduce current encoding while retaining measured transport-only fields."
  (let ((adapter    (getf context :encoder))
        (projection (getf context :projection))
        (tools      (getf context :tools)))
    (when (and adapter projection tools)
      (let* ((*provider-context-request*       nil)
             (*provider-cache-prefix-function* nil)
             (encoded                          (provider-context--wire-plan
                                                adapter (provider-request-object adapter projection tools)))
             (current                          (provider-context--wire-plan (getf context :provider) request))
             (transport-fields
               (remove-if (lambda (field)
                            (equal field (assoc (first field) (getf sample :encoder-fields) :test #'equal)))
                          (getf sample :wire-fields)))
             (expected-fields
               (append transport-fields
                       (remove-if (lambda (field)
                                    (assoc (first field) transport-fields :test #'equal))
                                  (getf encoded :fields)))))
        (and (equal (getf encoded :items) (getf current :items))
             (= (length expected-fields) (length (getf current :fields)))
             (every (lambda (field)
                      (equal field (assoc (first field) (getf current :fields) :test #'equal)))
                    expected-fields)
             t)))))

(-> provider-context--switched-field-charge (model-provider hash-table list) (integer 0))
(defun provider-context--switched-field-charge (provider request sample)
  "Charge changed encoded fields that can contribute new context."
  (let ((body (provider-request-context-body provider request)))
    (multiple-value-bind (history history-field) (provider-request-history provider body)
      (declare (ignore history))
      (loop for name being the hash-keys of (provider-request-body provider body) using (hash-value value)
            unless (or (equal name history-field)
                       (member name '("model" "reasoning") :test #'equal)
                       (equal (provider-cache-prefix--entry value)
                              (rest (assoc name (getf sample :wire-fields) :test #'equal))))
              sum (let ((old-prompt (first (getf sample :header-text))))
                    (if (and (equal name "instructions") (stringp value) old-prompt
                             (equal (provider-cache-prefix--entry old-prompt)
                                    (rest (assoc name (getf sample :wire-fields) :test #'equal))))
                        (cl-llm-provider-api:context-inserted-utf8-bytes old-prompt value)
                        (utf8-string-length (json-encode value))))))))

(-> provider-context--switched-input (model-provider hash-table &key (:context list) (:sample list))
    (values (integer 0) boolean boolean))
(defun provider-context--switched-input (provider request &key context sample)
  "Use measured occupancy only if the final request contains the verified history."
  (block nil
    (unless (and (typep provider 'codex-subscription-provider)
                 (conversation-context-usage-sample-p sample)
                 (= 2 (getf sample :format))
                 (not (equal (getf sample :policy) (getf context :policy)))
                 (or (getf sample :wire-history-p)
                     (and (= (getf sample :wire-count) (getf sample :history-count))
                          (equal (getf sample :wire-digest) (getf sample :history-digest)))))
      (return (values 0 nil nil)))
    (let* ((conversation (getf context :conversation))
           (history      (provider-history-items provider conversation :include-ephemeral-p nil))
           (items        (provider-context--request-items provider request)))
      (multiple-value-bind (added matched-p)
          (provider-context--switch-history provider conversation :sample sample :current history)
        (unless (and matched-p items
                     (provider-context--extends-p (mapcar #'provider-cache-prefix--entry history)
                                                  (map 'list #'provider-cache-prefix--entry items))
                     (provider-context--current-wire-p context request sample))
          (return (values 0 nil nil)))
        (values (+ (max (provider-context--header-charge sample context)
                        (provider-context--switched-field-charge provider request sample))
                   (provider-context--input-tokens
                    provider sample
                    (concatenate 'vector added (subseq items (length history)))))
                t t)))))

(-> provider-context--matched-input (model-provider hash-table)
    (values (integer 0) boolean boolean))
(defun provider-context--matched-input (provider request)
  "Revalidate history, encoding and policy before retaining measured occupancy."
  (let* ((context          *provider-context-request*)
         (conversation     (getf context :conversation))
         (sample           (and conversation (conversation-context-usage-sample conversation)))
         (header-changed-p (not (equal (getf sample :soft-header) (getf context :soft-header)))))
    (when (and (eq provider (getf context :provider)) (getf context :eligible-p)
               (provider-context--sample-matches-p
                sample (getf context :soft-header) (provider-context--history provider conversation)
                :components context))
      (let* ((current    (provider-context--wire-plan provider request))
             (items      (provider-context--request-items provider request))
             (fields     (getf current :fields))
             (old-fields (getf sample :wire-fields)))
        (when (and items
                   (if header-changed-p
                       (provider-context--current-wire-p context request sample)
                       (equal old-fields fields))
                   (provider-context--prefix-matches-p
                    (getf sample :wire-digest) (getf sample :wire-count) (getf current :items)))
          (let ((count (getf sample :wire-count)))
            (multiple-value-bind (tokens uncertain-p)
                (provider-context--input-tokens provider sample (unless (= count (length items)) (subseq items count)) :replaced-characters (getf sample :temporary-characters 0))              (return-from provider-context--matched-input
                                                                                                                                                                                                 (values (+ tokens (if header-changed-p
                                                                                                                                                                                                                       (provider-context--header-charge sample context) 0))
                                                                                                                                                                                                         t (or header-changed-p uncertain-p))))))))
    (if (and (eq provider (getf context :provider)) (getf context :eligible-p))
        (provider-context--switched-input provider request :context context :sample sample)
        (values 0 nil nil))))

(-> provider-context--response-entries-p (list list) boolean)
(defun provider-context--response-entries-p (entries output)
  "Require every added entry to be a distinct ordered response item, excluding concurrent input."
  (loop with remaining = output
        for entry in entries
        always (when-let ((tail (member entry remaining :test #'equal)))
                 (setf remaining (rest tail))
                 t)))

(-> provider-context--wire-fields-compatible-p (list list) boolean)
(defun provider-context--wire-fields-compatible-p (encoded observed)
  "Check that ENCODED introduces no field the transport did not observe.

Providers may add transport-only fields after encoding.  The usage sample
must describe the transported request, so those fields come from OBSERVED;
the encoder's own fields must still be present there."
  (and observed
       (loop for field in encoded
             always (assoc (first field) observed :test #'equal))))

(-> provider-context-note-response (list provider-result) null)
(defun provider-context-note-response (context result)
  "Anchor ordinary usage after persistence, before locally generated tool evidence.

Re-encode the measured projection through its original encoder, with its
persisted response and no temporary suffix. Retain header text once per epoch;
messages that later change fail closed."
  (let* ((conversation (getf context :conversation))
         (provider     (getf context :provider))
         (adapter      (getf context :encoder))
         (projection   (getf context :projection))
         (usage        (provider-usage-normalize (provider-result-usage result)))
         (input        (and (json-object-p usage) (json-get usage "input_tokens")))
         (output       (and (json-object-p usage) (json-get usage "output_tokens")))
         (total        (and (json-object-p usage) (json-get usage "total_tokens"))))
    (when conversation
      (when (and (getf context :eligible-p) adapter projection
                 (getf context :wire)
                 (typep input '(integer 1)) (typep output '(integer 0))
                 (or (plusp output) (null (provider-result-output-items result)))
                 (let ((cached (json-get usage "cached_input_tokens")))
                   (or (null cached) (and (typep cached '(integer 0)) (<= cached input))))
                 (or (null total) (and (typep total '(integer 0)) (>= total (+ input output)))))
        (let* ((history (provider-history-items provider conversation :include-ephemeral-p nil))
               (entries (mapcar #'provider-cache-prefix--entry history))
               (before  (getf context :history)))
          (when (and (provider-context--extends-p before entries)
                     (provider-context--response-entries-p
                      (nthcdr (length before) entries)
                      (mapcar #'provider-cache-prefix--entry (provider-result-output-items result))))
            (let* ((options                          (copy-list (cl-llm-provider-api:wire-request-options projection)))
                   (*provider-context-request*       nil)
                   (*provider-cache-prefix-function* nil)
                   (base
                     (make-instance 'cl-llm-provider-api:wire-request
                                    :model   (cl-llm-provider-api:wire-request-model projection)
                                    :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                    :items   (cl-llm-provider-api:wire-request-items projection)
                                    :options options))
                   (base-characters
                     (provider-context--request-characters adapter (provider-context--request-items adapter (provider-request-object adapter base (getf context :tools)))))
                   (covered
                      (make-instance 'cl-llm-provider-api:wire-request
                                     :model   (cl-llm-provider-api:wire-request-model projection)
                                     :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                     :items   (append (cl-llm-provider-api:wire-request-items projection)
                                                      (nthcdr (length before) history))
                                     :options options))
                   (wire                             (provider-context--wire-plan adapter (provider-request-object adapter covered (getf context :tools)))))
              (when (provider-context--wire-fields-compatible-p (getf wire :fields) (getf (getf context :wire) :fields))
                (let ((sample
                        (list :format 2 :soft-header (getf context :soft-header)
                              :policy         (getf context :policy)
                              :prompt         (getf context :prompt)
                              :tool-header    (getf context :tool-header)
                              :history-count  (length entries)
                              :history-digest (provider-cache-prefix--entry (coerce entries 'vector))
                              :wire-fields    (getf (getf context :wire) :fields)
                              :encoder-fields (getf wire :fields)
                              :wire-count     (length (getf wire :items))
                              :wire-digest    (provider-cache-prefix--entry
                                               (coerce (getf wire :items) 'vector))
                              :tokens         (max (+ input output) (or total 0))
                              :temporary-characters
                              (max 0 (- (getf context :wire-characters) base-characters)))))
                  (when (and (typep provider 'codex-subscription-provider)
                             (provider-context--extends-p before (getf (getf context :wire) :items))
                             (provider-context--extends-p entries (getf wire :items)))
                    (setf (getf sample :wire-history-p) t))
                 (let ((previous (conversation-context-usage-sample conversation)))
                   (unless (and (conversation-context-usage-sample-p previous)
                                (getf previous :header-text)
                                (every (lambda (key)
                                         (equal (getf previous key) (getf sample key)))
                                       '(:prompt :tool-header)))
                     (setf (getf sample :header-text) (getf context :header-text)))
                   (conversation-append-record conversation (list :context-usage :sample sample))
                   (setf (conversation-context-usage-sample conversation)
                         (conversation-context-usage-resolve sample previous)))))))))))
  nil)

(-> provider-context--request-characters ((option model-provider) t) (values (integer 0) boolean))
(defun provider-context--request-characters (provider value)
  "Apply Autolith's image allowance to encoder-owned character analysis.

The library charges the allowance only for images in Responses content arrays.
Images placed directly in a function_call_output's output are charged the same
way, and the estimate is marked uncertain."
  (let ((provider  (or provider (make-instance 'model-provider)))
        (allowance (* 4 (provider-image-token-allowance))))
    (multiple-value-bind (value images)
        (provider-context--without-tool-output-images provider value)
      (multiple-value-bind (characters uncertain-p)
          (provider-request-characters provider value :image-characters allowance)
        (values (+ characters
                   (* images (- allowance (provider-request-characters provider (json-object)))))
                (or uncertain-p (plusp images)))))))

(-> provider-context--without-tool-output-images (model-provider t) (values t (integer 0)))
(defun provider-context--without-tool-output-images (provider value)
  "Return VALUE with direct tool-output images replaced by empty objects, and their count.

VALUE is an encoded Responses request or a vector of its history items. Every
Responses provider receives Autolith's image tool results, so this applies to
all of them. Only changed items, and the request or vector holding them, are
copied; other VALUEs are returned unchanged with a count of zero."
  (multiple-value-bind (items field)
      (when (typep provider 'responses-api-provider)
        (typecase value
          (hash-table
           (provider-request-history provider value))
          ((and vector (not string))
           (values value nil))))
    (let ((copy  nil)
          (total 0))
      (loop for item across (or items #())
            for index from 0
            do (multiple-value-bind (replacement images)
                   (provider-context--tool-output-item-without-images provider item)
                 (when (plusp images)
                   (unless copy
                     (setf copy (copy-seq items)))
                   (setf (aref copy index) replacement)
                   (incf total images))))
      (cond
        ((null copy)
         (values value 0))
        (field
         (let ((request (json-object-copy value)))
           (setf (gethash field request) copy)
           (values request total)))
        (t
         (values copy total))))))

(-> provider-context--tool-output-item-without-images (responses-api-provider t) (values t (integer 0)))
(defun provider-context--tool-output-item-without-images (provider item)
  "Return ITEM with the image parts directly in its output replaced, and their count.

Only a function_call_output whose output is a vector qualifies. Image-shaped
data nested deeper is ordinary JSON. ITEM is returned itself when nothing changes."
  (flet ((image-p (part)
           (and (json-object-p part) (provider-request-image-p provider part) t)))
    (let ((output (and (json-object-p item)
                       (equal (json-get item "type") "function_call_output")
                       (json-get item "output"))))
      (if (and (vectorp output) (not (stringp output)) (some #'image-p output))
          (let ((copy (json-object-copy item)))
            (setf (gethash "output" copy)
                  (map 'vector (lambda (part) (if (image-p part) (json-object) part)) output))
            (values copy (count-if #'image-p output)))
          (values item 0)))))

(-> provider-request-output-ceilings (model-provider hash-table) list)
(defun provider-request-output-ceilings (provider request)
  "Combine positive wire limits with the caller's Autolith output allowance."
  (append (when (and (integerp *provider-maximum-output-tokens*)
                     (plusp *provider-maximum-output-tokens*))
            (list *provider-maximum-output-tokens*))
          (provider-request-output-limits provider request)))

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (reduce #'max (provider-request-output-ceilings provider request)          :initial-value (config :context-output-reserve (provider-configuration provider))))

(-> provider-check-request-context (model-provider hash-table) null)
(defun provider-check-request-context (provider request)
  "Reject an oversized encoded request before network I/O."
  (multiple-value-bind (input non-text-p)
      (provider-request-input-token-estimate provider request)
    (let ((reserve (provider-request-output-reserve provider request))
          (window  (provider-effective-context-window provider)))
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
  (declare (ignore credentials))
  (provider-context--admit-request provider request)
  (when (and conversation *provider-context-request*
             (eq conversation (getf *provider-context-request* :conversation)))
    (setf (getf *provider-context-request* :wire-provider) provider
          (getf *provider-context-request* :wire) (provider-context--wire-plan provider request)          (getf *provider-context-request* :wire-characters)
          (provider-context--request-characters provider (provider-context--request-items provider request))))
  (when *provider-context-admission-state*
    (setf (first *provider-context-admission-state*) :entered
          (rest *provider-context-admission-state*) nil)))

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
    :documentation "Tokens reserved for output.")
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

(-> provider-context--admit-request (model-provider hash-table) null)
(defun provider-context--admit-request (provider request)
  "Check the request budget and retain any context-budget error."
  (handler-bind
      ((provider-context-budget-error
         (lambda (condition)
           (when (and *provider-context-admission-state*
                      (eq (first *provider-context-admission-state*) :pending))
             (setf (rest *provider-context-admission-state*) condition)))))
    (provider-check-request-context provider request)))

(-> provider-call-with-mission-output-allowance (model-provider hash-table function) t)
(defun provider-call-with-mission-output-allowance (provider request continuation)
  "Limit mission output tokens to the available context before calling CONTINUATION.

Check caller limits before reducing the mission allowance, then recheck the
encoded request."
  (if (null *provider-mission-output-allowance*)
      (funcall continuation request)
      (let ((ceilings (provider-request-output-ceilings provider request)))
        (let ((*provider-maximum-output-tokens*
                (when ceilings (reduce #'max ceilings))))
          (provider-context--admit-request provider request))
        (let* ((window    (provider-effective-context-window provider))
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
                (provider-context--admit-request provider final-request)
                (funcall continuation final-request))
              ;; No positive output envelope exists for this encoded candidate.
              (let ((*provider-maximum-output-tokens* ceiling))
                (provider-context--admit-request provider candidate)))))))
