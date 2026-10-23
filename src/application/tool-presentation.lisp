(in-package #:autolith)

;;;; -- Tool Transcript Presentation --


(defparameter *application-tool-output-lines* 12
  "The maximum tool output lines shown in the terminal transcript.")

(defparameter *application-tool-inspection-lines* 24
  "The maximum introspection lines shown in the terminal transcript.")

(defparameter *application-common-lisp-language*
  (language-find "common-lisp" :errorp nil)
  "The ColorLisp language used for pathless Common Lisp source previews.")

(defparameter *application-shell-language*
  (language-find "bash" :errorp nil)
  "The ColorLisp language used for shell command previews.")

(-> application--fenced-output-rows (application string integer) (option list))
(defun application--fenced-output-rows (application text limit)
  "Render explicit Markdown code fences within a bounded tool-output rail."
  (when (or (search "```" text) (search "~~~" text))
    (let* ((renderer (application--markdown-renderer application))
           (safe (sanitize-text (bounded-string text :limit 16384)))
           (rows (append
                  (loop for line in (uiop:split-string safe :separator '(#\Newline))
                        append (application--markdown-rows renderer line))
                  (termdown:markdown-render-finish renderer))))
      (append
       (loop for row in (subseq rows 0 (min limit (length rows)))
             collect (cons (terminal-span ':dim "│ ") row))
       (when (> (length rows) limit)
         (list (list (terminal-span ':dim "… more content"))))))))

(-> application--code-output-rows
    (application string &key (:limit integer) (:json-p boolean))
    (option list))
(defun application--code-output-rows
    (application text &key (limit *application-tool-output-lines*) json-p)
  "Format self-identifying code output without reading Lisp or accessing files.

JSON-P also recognizes complete JSON captures, which otherwise retain the
recursive field layout of the generic result presenter. Shell output is not
assumed to be shell source."
  (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (or (application--fenced-output-rows application text limit)
        (when (uiop:string-prefix-p "(:" trimmed)
          (application--source-preview-rows
           text :language *application-common-lisp-language* :limit limit))
        (when (and json-p (nth-value 1 (application--structured-output-value text)))
          (application--source-preview-rows
           text :language (language-find "json" :errorp nil) :limit limit)))))


(-> application--preview-rows
    (string terminal-style integer &key (:gutter (option string)))
    list)
(defun application--preview-rows (text style limit &key gutter)
  "Return up to LIMIT styled rows from TEXT, followed by an omission row."
  (let ((lines (application--display-lines text)))
    (when lines
      (let* ((visible-count (min limit (length lines)))
             (omitted (- (length lines) visible-count)))
        (append
         (loop for line in (subseq lines 0 visible-count)
               collect (append (when gutter
                                 (list (terminal-span ':dim gutter)))
                               (list (terminal-span style line))))
         (when (plusp omitted)
           (list (list (terminal-span
                        ':dim
                        (format nil "… +~D more line~:P" omitted))))))))))

(defvar *application-provider-call-presentation* nil
  "The provider function call currently being rendered as one transcript entry.")

(-> application--function-call-arguments (json-object) (option json-object))
(defun application--function-call-arguments (call)
  "Decode exactly one CALL argument object, returning NIL when it is malformed."
  (let ((source (json-get call "arguments")))
    (when (non-empty-string-p source)
      (handler-case
          (let ((arguments (json-decode source)))
            (and (json-object-p arguments) arguments))
        (json-error ()
          nil)))))

(-> application--lisp-simple-symbol-name-p (string) boolean)
(defun application--lisp-simple-symbol-name-p (name)
  "Return whether NAME is a conservative unescaped Lisp symbol token."
  (and (non-empty-string-p name)
       (alpha-char-p (char name 0))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-+*/<>=!?$%_&~^." :test #'char=)))
              name)))

(-> application--lisp-escaped-symbol-name (string) string)
(defun application--lisp-escaped-symbol-name (name)
  "Return NAME as a Common Lisp multiple-escape symbol token."
  (with-output-to-string (stream)
    (write-char #\| stream)
    (loop for character across name
          do
             (when (find character "\\|" :test #'char=)
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\| stream)))

(-> application--lisp-symbol-token (string) string)
(defun application--lisp-symbol-token (name)
  "Return NAME as one reader-safe unqualified Lisp symbol token."
  (if (application--lisp-simple-symbol-name-p name)
      name
      (application--lisp-escaped-symbol-name name)))

(-> application--lisp-key-token (string) string)
(defun application--lisp-key-token (name)
  "Return JSON property NAME as one reader-safe Lisp keyword token."
  (format nil ":~A" (application--lisp-symbol-token name)))

(-> application--tool-header (application string) string)
(defun application--tool-header (application header)
  "Parenthesize call names, then sanitize and clip HEADER to transcript width."
  (let* ((call-header
           (if (and *application-provider-call-presentation*
                    (uiop:string-prefix-p "▸ " header))
               (format nil "▸ (~A)" (subseq header 2))
               header))
         (safe-header (sanitize-text call-header :single-line-p t))
         (maximum-width
           (max 1
                (1- (terminal-columns
                     (terminal-ui-terminal (application-ui application)))))))
    (cond
      ((<= (text-cell-width safe-header) maximum-width)
       safe-header)
      ((= maximum-width 1)
       "…")
      (t
       (concatenate 'string
                    (text-cell-prefix safe-header (1- maximum-width))
                    "…")))))

(-> application--tool-row-spans (application list) list)
(defun application--tool-row-spans (application row)
  "Return one indented, sanitized, width-bounded tool transcript ROW."
  (let* ((columns (terminal-columns
                   (terminal-ui-terminal (application-ui application))))
         (maximum-width (max 1 (1- columns)))
         (indented (append (list (terminal-span ':plain "  ")) row)))
    (if (<= (terminal--spans-width indented) maximum-width)
        indented
        (append (terminal--clip-spans indented (max 0 (1- maximum-width)))
                (list (terminal-span ':dim "…"))))))

(-> application--tool-entry
    (application &key (:style terminal-style) (:header string)
                 (:detail (option string)) (:rows list))
    list)
(defun application--tool-entry
    (application &key (style ':plain) (header "") detail rows)
  "Return a transcript header followed by concise styled tool ROWS."
  (append
   (application--transcript-entry
    application
    :style style
    :header (application--tool-header application header)
    :detail detail)
   (loop for row in rows
         append (list (terminal-span ':plain (string #\Newline)))
         when row
           append (application--tool-row-spans application row))))

(-> application--tool-field-rows (application list) list)
(defun application--tool-field-rows (application fields)
  "Return aligned, wrapped rows for tool detail FIELDS.

Each field is a plist containing :LABEL, :VALUE, and an optional :STYLE."
  (let* ((safe-fields
           (loop for field in fields
                 collect (list :label
                               (let ((label (sanitize-text
                                             (or (getf field :label) "")
                                             :single-line-p t)))
                                 (if *application-provider-call-presentation*
                                     (application--lisp-key-token label)
                                     label))
                               :value
                               (sanitize-text (or (getf field :value) "")
                                              :single-line-p t)
                               :style (or (getf field :style) ':plain))))
         (available-width
           (max 0
                (- (terminal-columns
                    (terminal-ui-terminal (application-ui application)))
                   3)))
         (column-widths
           (layout-column-widths
            (loop for field in safe-fields
                  collect (list (getf field :label) (getf field :value)))
            available-width
            :gap-width 2
            :minimum-widths '(1 4)))
         (label-width (or (first column-widths) 0))
         (value-width (or (second column-widths) 0)))
    (loop for field in safe-fields
          append
          (let ((value-rows
                  (if (plusp value-width)
                      (or (wrap-text (getf field :value) value-width)
                          (list ""))
                      (list ""))))
            (loop for value-row in value-rows
                  for first-p = t then nil
                  collect
                  (append
                   (list (terminal-span
                          (if *application-provider-call-presentation* ':syntax-keyword ':dim)
                          (layout-fit-text
                           (if first-p (getf field :label) "")
                           label-width)))
                   (when (plusp value-width)
                     (list (terminal-span ':plain "  ")
                           (terminal-span (getf field :style)
                                          value-row)))))))))

(-> application--tool-section-row (string) list)
(defun application--tool-section-row (label)
  "Return one dim tool transcript section heading row."
  (list (terminal-span ':dim label)))


(defparameter *application-tool-structure-items* 12
  "The maximum nested fields or items shown for one generic tool value.")

(defparameter *application-tool-structure-depth* 4
  "The maximum nesting depth expanded for one untrusted structured tool value.")

(defparameter *application-tool-structure-budget* nil
  "Dynamically bound remaining value count for one structured result preview.")

(-> application--presentation-value (t) string)
(defun application--presentation-value (value)
  "Return a concise readable presentation of one JSON VALUE without JSON syntax."
  (cond
    ((stringp value)
     value)
    ((json-object-p value)
     (format nil "~:D field~:P" (hash-table-count value)))
    ((and (vectorp value) (not (stringp value)))
     (format nil "~:D item~:P" (length value)))
    ((eq value t)
     "true")
    ((json-false-p value)
     "false")
    ((null value)
     "null")
    (t
     (bounded-string value :limit 500))))

(-> application--structured-value-style (string t) keyword)
(defun application--structured-value-style (label value)
  "Return semantic state and diagnostic severity styling for a structured field."
  (let* ((separator (position #\. label :from-end t))
         (field     (subseq label (if separator (1+ separator) 0))))
    (cond
      ((member field '("error" "condition-report") :test #'string-equal)
       (if value ':failure ':dim))
      ((and (string-equal field "severity") (integerp value))
       (case value
         (1
          ':failure)
         (2
          ':notice)
         (3
          ':hint)
         (4
          ':dim)
         (otherwise
          ':code)))
      ((and (member field '("status" "state" "outcome" "classification") :test #'string-equal)
            (or (stringp value) (keywordp value)))
       (let ((state (string-downcase (string value))))
         (cond
           ((member state '("failed" "failure" "error" "exhausted") :test #'string=)
            ':failure)
           ((member state '("cancelled" "canceled" "aborted" "blocked" "paused" "running"
                            "queued" "pending" "active" "abandoned" "live-owned-detached"
                            "dead-needing-decision" "reconstructible-checkpoint"
                            "safe-restartable-specification")
                    :test #'string=)
            ':notice)
           ((member state '("completed" "success" "succeeded" "verified" "passed" "accepted")
                    :test #'string=)
            ':success)
           (t
            ':code))))
      (t
       ':code))))

(-> application--structured-value-rows
    (application string t &key (:depth integer))
    list)
(defun application--structured-value-rows
    (application label value &key (depth *application-tool-structure-depth*))
  "Return bounded readable rows for LABEL and nested JSON VALUE.

Objects and arrays are expanded recursively so unregistered and dynamic tools
never fall back to raw JSON argument syntax.  DEPTH bounds untrusted nesting."
  (when *application-tool-structure-budget*
    (when (minusp (decf *application-tool-structure-budget*))
      (return-from application--structured-value-rows nil)))
  (cond
    ((json-object-p value)
     (if (plusp depth)
         (application--structured-object-rows application value
                                              :prefix label
                                              :depth (1- depth))
         (application--tool-field-rows
          application
          (list (list :label label :value "(nested object)" :style ':dim)))))
    ((and (vectorp value) (not (stringp value)))
     (if (not (plusp depth))
         (application--tool-field-rows
          application
          (list (list :label label
                      :value (format nil "~:D nested item~:P" (length value))
                      :style ':dim)))
         (let* ((visible-count (min *application-tool-structure-items*
                                    (length value)))
                (omitted (- (length value) visible-count)))
           (if (zerop (length value))
               (application--tool-field-rows
                application
                (list (list :label label :value "(no items)" :style ':dim)))
               (append
                (loop for index below visible-count
                      append (application--structured-value-rows
                              application
                              (format nil "~A[~D]" label (1+ index))
                              (aref value index)
                              :depth (1- depth)))
                (when (plusp omitted)
                  (list
                   (list (terminal-span
                          ':dim
                          (format nil "… +~D more item~:P" omitted))))))))))
    ((and (stringp value) (find #\Newline value))
     (append
      (list (application--tool-section-row label))
      (application--preview-rows (bounded-string value :limit 2000)
                                 (application--structured-value-style label value)
                                 *application-tool-call-lines* :gutter "│ ")))
    (t
     (application--tool-field-rows
      application
      (list (list :label label
                  :value (if (stringp value)
                             (bounded-string value :limit 500)
                             (application--presentation-value value))
                  :style (application--structured-value-style label value)))))))

(-> application--structured-key-less-p (string string) boolean)
(defun application--structured-key-less-p (left right)
  "Order identities and semantic states before potentially lengthy payloads."
  (let* ((priority '("truncated" "server" "id" "name" "state" "status" "severity" "error"
                     "objective" "message" "summary" "evidence" "gates" "criteria"))
         (left-rank (or (position left priority :test #'string=) 100))
         (right-rank (or (position right priority :test #'string=) 100)))
    (if (= left-rank right-rank)
        (not (null (string< left right)))
        (< left-rank right-rank))))

(-> application--structured-object-rows
    (application json-object &key (:prefix (option string))
                                  (:excluded-keys list) (:depth integer))
    list)
(defun application--structured-object-rows
    (application object &key prefix excluded-keys
                              (depth *application-tool-structure-depth*))
  "Return bounded recursively formatted OBJECT fields under optional PREFIX.

EXCLUDED-KEYS are omitted by exact name, allowing callers to present a
structural discriminator as a readable heading instead."
  (let* ((keys
           (sort
            (loop for key being the hash-keys of object
                  unless (member key excluded-keys :test #'string=)
                    collect key)
           #'application--structured-key-less-p))
         (visible-count (min *application-tool-structure-items* (length keys)))
         (omitted (- (length keys) visible-count)))
    (append
     (loop for key in (subseq keys 0 visible-count)
           append (application--structured-value-rows
                   application
                   (if prefix
                       (format nil "~A.~A" prefix key)
                       key)
                   (json-get object key)
                   :depth depth))
     (when (plusp omitted)
       (list
        (list (terminal-span
               ':dim
               (format nil "… +~D more field~:P" omitted))))))))

(-> application--argument-fields
    (json-object &key (:excluded-keys list))
    list)
(defun application--argument-fields (arguments &key excluded-keys)
  "Flatten bounded ARGUMENTS into fields sharing one aligned value column."
  (labels ((field (label value)
             "Return one bounded scalar or summary field."
             (list (list :label label
                         :value (bounded-string
                                 (application--presentation-value value)
                                 :limit 2000)
                         :style (if (string= label "restart")
                                    ':notice
                                    (application--structured-value-style label value)))))

           (expand-value (label value depth)
             "Expand VALUE beneath LABEL within the nesting and item limits."
             (cond
               ((json-object-p value)
                (if (and (plusp depth) (plusp (hash-table-count value)))
                    (expand-object value label (1- depth))
                    (field label value)))
               ((and (vectorp value) (not (stringp value)))
                (if (and (plusp depth) (plusp (length value)))
                    (append
                     (loop for index below (min (length value)
                                                *application-tool-structure-items*)
                           append (expand-value
                                   (format nil "~A[~D]" label (1+ index))
                                   (aref value index) (1- depth)))
                     (when (> (length value) *application-tool-structure-items*)
                       (field label (format nil "… +~D more items"
                                            (- (length value)
                                               *application-tool-structure-items*)))))
                    (field label value)))
               (t
                (field label value))))

           (expand-object (object prefix depth)
             "Expand OBJECT's fields in stable presentation order."
             (let* ((keys (sort (loop for key being the hash-keys of object
                                     unless (and (null prefix)
                                                 (member key excluded-keys
                                                         :test #'string=))
                                       collect key)
                               #'application--structured-key-less-p))
                    (count (min (length keys) *application-tool-structure-items*)))
               (append
                (loop for key in (subseq keys 0 count)
                      append (expand-value (if prefix
                                               (format nil "~A.~A" prefix key)
                                               key)
                                           (gethash key object) depth))
                (when (> (length keys) count)
                  (field (or prefix "arguments")
                         (format nil "… +~D more fields" (- (length keys) count))))))))
    (expand-object arguments nil *application-tool-structure-depth*)))

(-> application--generic-argument-rows
    (application (option json-object) &key (:excluded-keys list))
    list)
(defun application--generic-argument-rows (application arguments &key excluded-keys)
  "Return aligned argument fields followed immediately by multiline previews."
  (when arguments
    (let* ((fields (application--argument-fields arguments :excluded-keys excluded-keys))
           (multiline (remove-if-not
                       (lambda (field) (find #\Newline (getf field :value))) fields)))
      (append
       (application--tool-field-rows
        application (remove-if (lambda (field) (member field multiline)) fields))
       (loop for field in multiline
             append (cons
                     (application--tool-section-row
                      (application--lisp-key-token
                       (sanitize-text (getf field :label) :single-line-p t)))
                     (application--preview-rows
                      (getf field :value) (getf field :style)
                      *application-tool-call-lines* :gutter "│ ")))))))

(-> application--restart-value-rows ((option json-object)) list)
(defun application--restart-value-rows (arguments)
  "Return a source rail for an explicitly supplied restart value."
  (let ((value (and arguments (json-get arguments "restart-value"))))
    (when (non-empty-string-p value)
      (cons (application--tool-section-row ":restart-value")
            (application--source-preview-rows
             value :language *application-common-lisp-language*
             :limit *application-tool-call-lines*)))))

(-> application--generic-tool-call-entry (application json-object) list)
(defun application--generic-tool-call-entry (application call)
  "Return a readable fallback entry for a function CALL."
  (application--tool-entry
   application
   :style ':tool
   :header (format nil "▸ ~A" (or *application-tool-presentation-label*
                                  (function-call-canonical-name call)))
   :rows (application--generic-argument-rows
          application
          (application--function-call-arguments call))))

(defmethod application-tool-call-entry :around
    ((tool t) (application application) (call hash-table))
  "Use Lisp keyword labels for CALL's argument fields, without changing results."
  (declare (ignore tool application))
  (let ((*application-provider-call-presentation* call))
    (call-next-method)))

(defmethod application-tool-call-entry :around
    ((tool mcp-provider-tool) (application application) (call hash-table))
  "Render MCP identity labels without changing the provider call or dispatch names."
  (let ((display-call (make-hash-table :test (hash-table-test call))))
    (maphash (lambda (key value) (setf (gethash key display-call) value)) call)
    (setf (gethash "namespace" display-call) (tool-display-namespace tool)
          (gethash "name" display-call)
          (mcp-tool-name (mcp-provider-tool-raw-tool tool)))
    (call-next-method tool application display-call)))

(defmethod application-tool-call-entry
    ((tool tool) (application application) (call hash-table))
  "Present CALL using the generic readable argument layout."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))

(defmethod application-tool-call-entry
    ((tool null) (application application) (call hash-table))
  "Present an unregistered CALL using the generic readable argument layout."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))


(defmethod application-tool-call-entry
    ((tool papercut-report-tool) (application application) (call hash-table))
  "Present papercut.report's title without duplicating its complete body."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (title (and arguments (json-get arguments "title"))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ papercut.report"
     :rows (when title
             (application--tool-field-rows
              application (list (list :label "title" :value title :style ':strong)))))))


;;; Tool call specializations

(-> application--lisp-call-entry (application json-object string) list)
(defun application--lisp-call-entry (application call argument-name)
  "Return a syntax-highlighted Lisp source preview for CALL's ARGUMENT-NAME."
  (let* ((arguments (application--function-call-arguments call))
         (value (and arguments (json-get arguments argument-name)))
         (source
           (if (and (vectorp value)
                    (not (stringp value))
                    (every #'stringp value))
               (format nil "~{~A~^~%~}" (coerce value 'list))
               (application--presentation-value (or value "")))))
    (application--tool-entry
     application
     :style ':tool
     :header (format nil "▸ ~A" (function-call-canonical-name call))
     :rows (append
            (application--generic-argument-rows
             application arguments :excluded-keys (list argument-name "restart-value"))
            (application--source-preview-rows
             source :language *application-common-lisp-language*)
            (application--restart-value-rows arguments)))))

;; Codex PlanUpdateCell reference: 5c19155cbd93bfa099016e7487259f61669823ff.
(-> application--plan-update-text (t) (option string))
(defun application--plan-update-text (value)
  "Return VALUE as sanitized non-empty single-line plan text."
  (when (stringp value)
    (let ((text
            (sanitize-text
             (string-trim '(#\Space #\Tab #\Newline #\Return) value)
             :single-line-p t)))
      (and (plusp (length text)) text))))

(-> application--plan-update-wrapped-rows
    (application string
                 &key (:style terminal-style)
                      (:initial-prefix list)
                      (:continuation-prefix list))
    list)
(defun application--plan-update-wrapped-rows
    (application text &key style initial-prefix continuation-prefix)
  "Return styled wrapped plan TEXT rows after the supplied prefix spans."
  (let* ((columns
           (terminal-columns
            (terminal-ui-terminal (application-ui application))))
         (prefix-width
           (max (terminal--spans-width initial-prefix)
                (terminal--spans-width continuation-prefix)))
         (text-width (max 1 (- columns 3 prefix-width)))
         (lines (or (wrap-text text text-width) (list ""))))
    (loop for line in lines
          for first-p = t then nil
          collect
          (append (if first-p initial-prefix continuation-prefix)
                  (list (terminal-span style line))))))

(-> application--plan-update-step-rows
    (application json-object boolean)
    list)
(defun application--plan-update-step-rows (application step first-content-p)
  "Return wrapped rows for one plan STEP with its visible status marker."
  (let* ((text
           (application--plan-update-text
            (or (json-get step "step") (json-get step "text"))))
         (status (plan--status (json-get step "status")))
         (valid-p (and text status))
         (marker
           (if valid-p
               (if (eq status ':done) "✔ " "□ ")
               "? "))
         (style
           (if valid-p
               (if (eq status ':doing) ':plan-active ':dim)
               ':failure))
         (initial-prefix
           (list (terminal-span ':dim (if first-content-p "└ " "  "))
                 (terminal-span style marker)))
         (continuation-prefix (list (terminal-span ':plain "    "))))
    (application--plan-update-wrapped-rows
     application
     (or text "invalid plan step")
     :style style
     :initial-prefix initial-prefix
     :continuation-prefix continuation-prefix)))

(-> application--plan-update-rows
    (application (option json-object))
    list)
(defun application--plan-update-rows (application arguments)
  "Return Codex-style explanation and checklist rows from plan ARGUMENTS."
  (block nil
    (unless arguments
      (return
        (list (list (terminal-span ':dim "└ ")
                    (terminal-span ':hint "arguments unavailable")))))
    (let* ((explanation
             (application--plan-update-text
              (json-get arguments "explanation")))
           (steps (json-get arguments "steps"))
           (rows nil)
           (content-p nil))
      (when explanation
        (setf rows
              (application--plan-update-wrapped-rows
               application explanation
               :style ':hint
               :initial-prefix (list (terminal-span ':dim "└ "))
               :continuation-prefix (list (terminal-span ':plain "  ")))
              content-p t))
      (unless (and (vectorp steps) (not (stringp steps)))
        (return
          (nconc rows
                 (list
                  (list (terminal-span ':dim (if content-p "  " "└ "))
                        (terminal-span ':hint "steps unavailable"))))))
      (if (zerop (length steps))
          (nconc rows
                 (list
                  (list (terminal-span ':dim (if content-p "  " "└ "))
                        (terminal-span ':hint "(no steps provided)"))))
          (let ((visible-count (min (length steps) *plan-maximum-steps*)))
            (loop for index below visible-count
                  for step = (aref steps index)
                  do (setf rows
                           (nconc
                            rows
                            (if (json-object-p step)
                                (application--plan-update-step-rows
                                 application step (not content-p))
                                (list
                                 (list
                                  (terminal-span
                                   ':dim (if content-p "  " "└ "))
                                  (terminal-span ':failure "? invalid plan step")))))
                           content-p t))
            (let ((omitted (- (length steps) visible-count)))
              (when (plusp omitted)
                (setf rows
                      (nconc
                       rows
                       (list
                        (list
                         (terminal-span ':dim "  ")
                         (terminal-span
                          ':dim
                          (format nil "… +~D more step~:P" omitted))))))))
            rows)))))

(defmethod application-tool-call-entry
    ((tool plan-update-tool) (application application) (call hash-table))
  "Present plan.update as a wrapped checklist."
  (declare (ignore tool))
  (let ((rows
          (application--plan-update-rows
           application (application--function-call-arguments call))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ plan.update"
     :rows (append
            (list (list (terminal-span ':dim "• ")
                        (terminal-span ':strong "Updated Plan")))
            rows))))

(-> application--task-run-arguments
    (task-run-tool json-object)
    (option json-object))
(defun application--task-run-arguments (tool call)
  "Decode CALL through TOOL's exact task JSON policy, or return NIL."
  (let ((source (json-get call "arguments")))
    (when (non-empty-string-p source)
      (handler-case
          (let ((arguments (tool-decode-arguments tool source)))
            (and (json-object-p arguments) arguments))
        (error ()
          nil)))))

(-> application--task-run-explicit-mode
    (json-object)
    (values keyword boolean))
(defun application--task-run-explicit-mode (object)
  "Return OBJECT's explicit task mode and whether it was supplied."
  (multiple-value-bind (value present-p)
      (gethash "async" object)
    (values
     (cond
       ((not present-p) ':synchronous)
       ((eq value t) ':detached)
       ((json-false-p value) ':synchronous)
       (t ':invalid))
     present-p)))

(-> application--task-run-mode
    (json-object keyword boolean)
    keyword)
(defun application--task-run-mode
    (object inherited-mode inherited-mode-p)
  "Return OBJECT's explicit mode or its inherited batch mode."
  (multiple-value-bind (mode present-p)
      (application--task-run-explicit-mode object)
    (if present-p
        mode
        (if inherited-mode-p inherited-mode ':synchronous))))

(-> application--task-run-prose
    (json-object string &key (:required boolean))
    (option string))
(defun application--task-run-prose (object field &key required)
  "Return repaired prose FIELD from OBJECT with readable invalid placeholders."
  (multiple-value-bind (value present-p)
      (gethash field object)
    (cond
      ((and present-p (stringp value))
       (let ((repaired (task--repair-prose value)))
         (if (non-empty-string-p repaired)
             repaired
             (and required
                  (format nil "(missing ~A)" field)))))
      (present-p
       (format nil "(invalid ~A)" field))
      (required
       (format nil "(missing ~A)" field))
      (t
       nil))))

(-> application--task-run-items
    (json-object)
    (values list boolean boolean))
(defun application--task-run-items (arguments)
  "Return raw task items, batch mode, and malformed-batch status."
  (multiple-value-bind (tasks present-p)
      (gethash "tasks" arguments)
    (cond
      ((not present-p)
       (values (list arguments) nil nil))
      ((and (vectorp tasks)
            (not (stringp tasks))
            (plusp (length tasks)))
       (values (coerce tasks 'list) t nil))
      (t
       (values nil t t)))))

(-> application--task-run-item
    (t &key (:index integer)
            (:inherited-mode keyword)
            (:inherited-mode-p boolean)
            (:include-context-p boolean))
    list)
(defun application--task-run-item
    (item &key (index 1) (inherited-mode ':synchronous)
          inherited-mode-p include-context-p)
  "Return one best-effort presentation record for raw task ITEM."
  (if (not (json-object-p item))
      (list :index index
            :name nil
            :role "(invalid role)"
            :mode ':invalid
            :assignment "(invalid task definition)"
            :context nil
            :invalid-p t)
      (let* ((name-value (json-get item "name"))
             (role-value (json-get item "agent"))
             (name
               (cond
                 ((null name-value) nil)
                 ((non-empty-string-p name-value) name-value)
                 (t "(invalid name)")))
             (role
               (cond
                 ((null role-value) "task")
                 ((non-empty-string-p role-value)
                  (string-downcase role-value))
                 (t "(invalid role)"))))
        (list :index index
              :name name
              :role role
              :mode (application--task-run-mode
                     item inherited-mode inherited-mode-p)
              :assignment (application--task-run-prose
                           item "task" :required t)
              :context (and include-context-p
                            (application--task-run-prose item "context"))
              :invalid-p nil))))

(-> application--task-run-shared-context
    (json-object boolean)
    (option string))
(defun application--task-run-shared-context (arguments batch-p)
  "Return top-level context, requiring it only for a task batch."
  (application--task-run-prose arguments "context" :required batch-p))

(-> application--task-run-summary-text (string) string)
(defun application--task-run-summary-text (text)
  "Return TEXT as one sanitized whitespace-normalized summary line."
  (format nil
          "~{~A~^ ~}"
          (remove-if
           (lambda (part) (zerop (length part)))
           (uiop:split-string
            (sanitize-text text :single-line-p t)
            :separator '(#\Space #\Tab #\Newline #\Return #\Page)))))

(-> application--fit-ellipsized (string integer) string)
(defun application--fit-ellipsized (text width)
  "Clip TEXT to WIDTH terminal cells with an ellipsis, then pad it."
  (let* ((width (max 0 width))
         (safe (sanitize-text text :single-line-p t))
         (visible
           (cond
             ((<= (text-cell-width safe) width)
              safe)
             ((zerop width)
              "")
             ((= width 1)
              "…")
             (t
              (concatenate
               'string
               (text-cell-prefix safe (1- width))
               "…")))))
    (layout-fit-text visible width)))

(-> application--task-run-prose-rows
    (application string
     &key (:indent string) (:style terminal-style) (:limit integer))
    list)
(defun application--task-run-prose-rows
    (application text &key (indent "") (style ':plain)
                            (limit *application-tool-call-lines*))
  "Return bounded wrapped prose rows with a subdued rail."
  (let* ((prefix (format nil "~A│ " indent))
         (columns
           (terminal-columns
            (terminal-ui-terminal (application-ui application))))
         (width (max 1
                     (- columns
                        3
                        (text-cell-width prefix))))
         (bounded (bounded-string text :limit 8000))
         (wrapped
           (loop for line in (or (application--display-lines bounded)
                                 (list ""))
                 append (or (wrap-text line width) (list ""))))
         (visible-count (min limit (length wrapped)))
         (omitted (- (length wrapped) visible-count)))
    (append
     (loop for line in (subseq wrapped 0 visible-count)
           collect (list (terminal-span ':dim prefix)
                         (terminal-span style line)))
     (when (plusp omitted)
       (list
        (list
         (terminal-span
          ':dim
          (format nil "~A… +~D more row~:P" indent omitted))))))))

(-> application--task-run-context-rows
    (application (option string) &key (:label string))
    list)
(defun application--task-run-context-rows
    (application context &key (label "context"))
  "Return a labeled readable task CONTEXT section when one exists."
  (when context
    (append
     (list (application--tool-section-row label))
     (application--task-run-prose-rows application context))))

(-> application--task-run-mode-label (keyword) string)
(defun application--task-run-mode-label (mode)
  "Return one readable presentation label for task MODE."
  (ecase mode
    (:detached "detached")
    (:synchronous "synchronous")
    (:invalid "(invalid mode)")))

(-> application--task-run-compact-context-row
    (application string)
    list)
(defun application--task-run-compact-context-row (application context)
  "Return one indented compact preview row for item-specific CONTEXT."
  (let* ((prefix "    context  ")
         (columns
           (terminal-columns
            (terminal-ui-terminal (application-ui application))))
         (width (max 0 (- columns 3 (text-cell-width prefix)))))
    (list
     (terminal-span ':dim prefix)
     (terminal-span
      ':plain
      (application--fit-ellipsized
       (application--task-run-summary-text context)
       width)))))

(-> application--task-run-compact-item-rows
    (application list)
    list)
(defun application--task-run-compact-item-rows (application items)
  "Return concise aligned rows for normalized presentation ITEMS."
  (let* ((values
           (loop for item in items
                 collect
                 (list
                  (princ-to-string (getf item :index))
                  (or (getf item :name) "(unnamed)")
                  (getf item :role)
                  (case (getf item :mode)
                    (:detached "detached")
                    (:invalid "invalid")
                    (otherwise ""))
                  (application--task-run-summary-text
                   (getf item :assignment)))))
         (columns
           (terminal-columns
            (terminal-ui-terminal (application-ui application))))
         (widths
           (layout-column-widths
            values
            (max 0 (- columns 3))
            :gap-width 2
            :minimum-widths '(1 4 4 0 4)))
         (styles
           '(:dim :agent-name :agent-role :dim :plain)))
    (loop for item in items
          for row-values in values
          append
          (append
           (list
            (loop for value in row-values
                  for width in widths
                  for style in styles
                  for first-p = t then nil
                  append
                  (append
                   (unless first-p
                     (list (terminal-span ':plain "  ")))
                   (list
                    (terminal-span
                     style
                     (application--fit-ellipsized value width))))))
           (when (getf item :context)
             (list
              (application--task-run-compact-context-row
               application
               (getf item :context))))))))

(-> application--task-run-expanded-item-rows
    (application list)
    list)
(defun application--task-run-expanded-item-rows (application item)
  "Return a full readable section for one task presentation ITEM."
  (let ((name (getf item :name)))
    (append
     (list
      (list
       (terminal-span
        ':dim
        (format nil "task ~D" (getf item :index)))
       (terminal-span
        ':agent-name
        (if name (format nil " · ~A" name) ""))))
     (application--tool-field-rows
      application
      (list
       (list :label "role"
             :value (getf item :role)
             :style ':agent-role)
       (list :label "mode"
             :value (application--task-run-mode-label
                     (getf item :mode)))))
     (when (getf item :context)
       (append
        (list (application--tool-section-row "task context"))
        (application--task-run-prose-rows
         application
         (getf item :context))))
     (list (application--tool-section-row "assignment"))
     (application--task-run-prose-rows
      application
      (getf item :assignment)))))

(-> application--task-run-rows
    (application json-object)
    list)
(defun application--task-run-rows (application arguments)
  "Return compact or expanded structured rows for task.run ARGUMENTS."
  (multiple-value-bind (raw-items batch-p malformed-batch-p)
      (application--task-run-items arguments)
    (let* ((shared-context
             (application--task-run-shared-context arguments batch-p))
           (item-limit (max 1 *task-maximum-batch-size*))
           (visible-raw-items
             (subseq raw-items 0 (min item-limit (length raw-items))))
           (omitted-items
             (- (length raw-items) (length visible-raw-items))))
      (multiple-value-bind (inherited-mode inherited-mode-p)
          (application--task-run-explicit-mode arguments)
        (let ((items
                (loop for raw-item in visible-raw-items
                      for index from 1
                      collect
                      (application--task-run-item
                       raw-item
                       :index index
                       :inherited-mode inherited-mode
                       :inherited-mode-p (and batch-p inherited-mode-p)
                       :include-context-p batch-p))))
          (append
           (application--task-run-context-rows
            application shared-context)
           (when (and shared-context
                      (or malformed-batch-p items))
             (list nil))
           (cond
             (malformed-batch-p
              (list (application--tool-section-row
                     "invalid task batch")))
             ((application-compact-view-p application)
              (append
               (when batch-p
                 (list
                  (application--tool-section-row
                   (format nil "tasks · ~D" (length raw-items)))))
               (application--task-run-compact-item-rows
                application items)))
             (t
              (loop for item in items
                    for first-p = t then nil
                    append
                    (append
                     (unless first-p (list nil))
                     (application--task-run-expanded-item-rows
                      application item)))))
           (when (plusp omitted-items)
             (list
              (list
               (terminal-span
                ':dim
                (format nil "… +~D more task~:P" omitted-items)))))))))))

(defmethod application-tool-call-entry
    ((tool task-run-tool) (application application) (call hash-table))
  "Present task.run children as structured compact or expanded prose."
  (let ((arguments (application--task-run-arguments tool call)))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ task.run"
     :rows (if arguments
               (application--task-run-rows application arguments)
               (list
                (application--tool-section-row
                 "arguments unavailable"))))))

(-> application--resource-operation-heading (json-object integer) string)
(defun application--resource-operation-heading (operation index)
  "Return a readable verb phrase for resource edit OPERATION at INDEX."
  (let ((name (let ((value (json-get operation "op")))
                (if (stringp value) value "")))
        (start (json-get operation "start-line"))
        (end (json-get operation "end-line"))
        (anchor (json-get operation "line"))
        (identifier (json-get operation "id")))
    (format nil "operation ~D · ~A"
            index
            (cond
              ((string= name "replace-lines")
               (if (and (integerp start) (integerp end))
                   (format nil "replace lines ~D-~D" start end)
                   "replace lines"))
              ((string= name "delete-lines")
               (if (and (integerp start) (integerp end))
                   (format nil "delete lines ~D-~D" start end)
                   "delete lines"))
              ((string= name "insert-before")
               (if (integerp anchor)
                   (format nil "insert before line ~D" anchor)
                   "insert before line"))
              ((string= name "insert-after")
               (if (integerp anchor)
                   (format nil "insert after line ~D" anchor)
                   "insert after line"))
              ((string= name "replace-empty")
               "replace empty file")
              ((string= name "agenda-add")
               "add agenda item")
              ((string= name "agenda-update")
               (if (non-empty-string-p identifier)
                   (format nil "update agenda item ~A" identifier)
                   "update agenda item"))
              ((string= name "agenda-remove")
               (if (non-empty-string-p identifier)
                   (format nil "remove agenda item ~A" identifier)
                   "remove agenda item"))
              ((string= name "memory-remember")
               "remember memory")
              ((string= name "memory-replace")
               "replace memory")
              ((string= name "memory-forget")
               "forget memory")
              ((string= name "papercut-report")
               "report papercut")
              ((string= name "papercut-close")
               "close papercut")
              ((non-empty-string-p name)
               name)
              (t
               "invalid operation")))))



(-> application--file-resource-path (string) (option string))
(defun application--file-resource-path (uri)
  "Return the decoded path from a workspace or scratchpad resource URI, or NIL."
  (dolist (prefix '("workspace:" "scratchpad:"))
    (when (and (uiop:string-prefix-p prefix uri)
               (> (length uri) (length prefix)))
      (return-from application--file-resource-path
        (handler-case
            (url-decode (subseq uri (length prefix)))
          (error ()
            nil)))))
  nil)

(-> application--workspace-resource-observed-text
    (application string string &key (:start-line integer) (:end-line integer))
    (option string))
(defun application--workspace-resource-observed-text
    (application uri revision &key start-line end-line)
  "Return visible observed workspace lines for URI and REVISION, or NIL.

The transcript never reads the current file as a revision-gated preimage. It
uses only the exact transient observation and only ranges already shown to the
model."
  (block nil
    (unless (and (slot-boundp application 'conversation)
                 (non-empty-string-p revision)
                 (plusp start-line)
                 (<= start-line end-line))
      (return nil))
    (let ((conversation (application-conversation application)))
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let ((state
                (resource-observation-state-find
                 (conversation-resource-observations conversation)
                 revision
                 'workspace-file-observation-state)))
          (unless state
            (return nil))
          (let* ((observation
                   (resource-observation-state-observation state))
                 (lines (workspace-file-observation-lines observation)))
            (unless (and (string= uri (resource-observation-uri observation))
                         (structlisp:integer-interval-set-covers-p
                          (workspace-file-observation-state-visible-ranges state)
                          start-line (1+ end-line))
                         (<= end-line (length lines)))
              (return nil))
            (let ((selected-lines
                    (loop for line from start-line to end-line
                          collect (aref lines (1- line)))))
              (format nil "~{~A~^~%~}~:[~;~%~]"
                      selected-lines
                      (and selected-lines
                           (zerop
                            (length (first (last selected-lines)))))))))))))

(-> application--workspace-resource-operation-set-valid-p
    (application vector &key (:uri string) (:revision string))
    boolean)
(defun application--workspace-resource-operation-set-valid-p
    (application operations &key uri revision)
  "Return whether OPERATIONS are valid against the exact observed workspace file."
  (block nil
    (unless (and (slot-boundp application 'conversation)
                 (non-empty-string-p uri)
                 (non-empty-string-p revision)
                 (plusp (length operations)))
      (return nil))
    (let ((conversation (application-conversation application)))
      (unless (typep conversation 'conversation)
        (return nil))
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let ((state
                (resource-observation-state-find
                 (conversation-resource-observations conversation)
                 revision
                 'workspace-file-observation-state)))
          (unless state
            (return nil))
          (let ((observation
                  (resource-observation-state-observation state)))
            (unless (string= uri (resource-observation-uri observation))
              (return nil))
            (handler-case
                (progn
                  (cl-hashline:edit-text
                   (resource-observation-content observation)
                   (map 'list #'workspace-file--json-operation operations)
                   :visible-lines (workspace-file-observation-state-visible-ranges state)
                   :split-lines #'text--split-lines
                   :anchor-maximum-offset *workspace-file-resource-anchor-maximum-offset*)
                  t)
              (error ()
                nil))))))))

(-> application--workspace-resource-added-start-lines
    (application vector &key (:uri string) (:revision string))
    vector)
(defun application--workspace-resource-added-start-lines
    (application operations &key uri revision)
  "Return exact resulting first-line coordinates for valid added content."
  (let ((starts (make-array (length operations) :initial-element nil))
        (coordinates nil))
    (block nil
      (unless (application--workspace-resource-operation-set-valid-p
               application operations :uri uri :revision revision)
        (return starts))
      (loop for operation across operations
            for index from 0
            do
               (let* ((name (json-get operation "op"))
                      (start (json-get operation "start-line"))
                      (end (json-get operation "end-line"))
                      (line (json-get operation "line"))
                      (content (json-get operation "content"))
                      (added-count
                        (and (stringp content)
                             (length (text--split-lines content)))))
                 (cond
                   ((string= name "replace-lines")
                    (push (list index
                                start
                                start
                                (- added-count (1+ (- end start))))
                          coordinates))
                   ((string= name "delete-lines")
                    (push (list index
                                start
                                nil
                                (- (1+ (- end start))))
                          coordinates))
                   ((member name '("insert-before" "insert-after") :test #'string=)
                    (push (list index
                                line
                                (+ line
                                   (if (string= name "insert-after") 1 0))
                                added-count)
                          coordinates))
                   ((string= name "replace-empty")
                    (push (list index 0 1 added-count) coordinates)))))
      (loop with delta = 0
            for coordinate in (stable-sort coordinates #'< :key #'second)
            for index = (first coordinate)
            for new-base = (third coordinate)
            do (when new-base
                 (setf (aref starts index) (+ new-base delta)))
               (incf delta (fourth coordinate)))
      starts)))

(-> application--workspace-resource-operation-change-rows
    (application json-object
                 &key (:uri string) (:revision string) (:path string)
                      (:new-start-line (option integer)))
    (option list))
(defun application--workspace-resource-operation-change-rows
    (application operation &key uri revision path new-start-line)
  "Return source-change rows for one workspace resource edit OPERATION.

When exact resulting coordinates are unavailable, added rows fall back to
OPERATION's own declared target line so they stay numbered."
  (let* ((name (json-get operation "op"))
         (start (json-get operation "start-line"))
         (end (json-get operation "end-line"))
         (line (json-get operation "line"))
         (content (json-get operation "content"))
         (new-start-line
           (or new-start-line
               (cond
                 ((and (stringp name)
                       (string= name "insert-after")
                       (integerp line))
                  (1+ line))
                 ((integerp line)
                  line)
                 ((integerp start)
                  start)
                 (t
                  nil)))))
    (cond
      ((and (stringp name)
            (member name '("replace-lines" "delete-lines") :test #'string=)
            (integerp start)
            (integerp end)
            (plusp start)
            (<= start end)
            (or (string= name "delete-lines") (stringp content)))
       (let* ((old-text
                (application--workspace-resource-observed-text
                 application uri revision :start-line start :end-line end))
              (new-text (if (string= name "replace-lines") content "")))
         (if old-text
             (change-viewer-render
              :removed-content old-text
              :added-content new-text
              :removed-start-line start
              :added-start-line new-start-line
              :source-path path)
             (let* ((width (length (princ-to-string end)))
                    (message
                      (format nil "observed line~P ~D~:[~;-~D~] unavailable"
                              (1+ (- end start)) start (< start end) end)))
               (append
                (list
                 (render-line-row
                  ':removed
                  message
                  start
                  :width         width
                  :content-spans (list (terminal-span ':dim message))
                  :span-function #'syntax--terminal-span))
                (when (string= name "replace-lines")
                  (change-viewer-render
                   :added-content new-text
                   :added-start-line new-start-line
                   :source-path path)))))))
      ((and (stringp name)
            (member name '("insert-before" "insert-after" "replace-empty")
                    :test #'string=)
            (stringp content))
       (change-viewer-render
        :added-content content
        :added-start-line new-start-line
        :source-path path))
      (t
       nil))))

(-> application--workspace-resource-operation-rows
    (application t &key (:uri string) (:revision string) (:path string))
    list)
(defun application--workspace-resource-operation-rows
    (application operations &key uri revision path)
  "Return syntax-highlighted line changes for workspace resource OPERATIONS."
  (cond
    ((not (and (vectorp operations) (not (stringp operations))))
     (list (application--tool-section-row "operations unavailable")))
    ((zerop (length operations))
     (list (application--tool-section-row "no operations")))
    (t
     (let* ((new-start-lines
              (application--workspace-resource-added-start-lines
               application operations :uri uri :revision revision))
            (visible-count (min *application-tool-structure-items*
                                (length operations)))
            (omitted (- (length operations) visible-count)))
       (append
        (loop for index below visible-count
              for operation = (aref operations index)
              for first-p = t then nil
              append
              (append
               (unless first-p (list nil))
               (if (json-object-p operation)
                   (let ((change-rows
                           (application--workspace-resource-operation-change-rows
                            application operation
                            :uri uri
                            :revision revision
                            :path path
                            :new-start-line (aref new-start-lines index))))
                     (append
                      (list (application--tool-section-row
                             (application--resource-operation-heading
                              operation
                              (1+ index))))
                      (or change-rows
                          (application--generic-argument-rows
                           application operation :excluded-keys '("op")))))
                   (application--structured-value-rows
                    application
                    (format nil "operation ~D" (1+ index))
                    operation))))
        (when (plusp omitted)
          (list nil
                (list (terminal-span
                       ':dim
                       (format nil "… +~D more operation~:P" omitted))))))))))

(-> application--resource-operation-rows
    (application t &key (:change-rows-function (option function)))
    list)
(defun application--resource-operation-rows
    (application operations &key change-rows-function)
  "Return readable operation headings and optional shared change views."
  (cond
    ((not (and (vectorp operations) (not (stringp operations))))
     (list (application--tool-section-row "operations unavailable")))
    ((zerop (length operations))
     (list (application--tool-section-row "no operations")))
    (t
     (let* ((visible-count (min *application-tool-structure-items*
                                (length operations)))
            (omitted (- (length operations) visible-count)))
       (append
        (loop for index below visible-count
              for operation = (aref operations index)
              for first-p = t then nil
              append
              (append
               (unless first-p (list nil))
               (if (json-object-p operation)
                   (let ((change-rows
                           (and change-rows-function
                                (funcall change-rows-function operation))))
                     (append
                      (list (application--tool-section-row
                             (application--resource-operation-heading
                              operation
                              (1+ index))))
                      (or change-rows
                          (application--generic-argument-rows
                           application operation :excluded-keys '("op")))))
                   (application--structured-value-rows
                    application
                    (format nil "operation ~D" (1+ index))
                    operation))))
        (when (plusp omitted)
          (list nil
                (list (terminal-span
                       ':dim
                       (format nil "… +~D more operation~:P" omitted))))))))))

(defmethod application-tool-call-entry
    ((tool resource-read-tool) (application application) (call hash-table))
  "Present resource.read as a URI and optional bounded line window."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))


(-> application--shell-command-rows (string) list)
(defun application--shell-command-rows (command)
  "Return bounded syntax-highlighted shell source with a green ruler."
  (application--source-preview-rows command
                                    :language *application-shell-language*
                                    :prompt   "$ "))

(defmethod application-tool-call-entry
    ((tool shell-run-tool) (application application) (call hash-table))
  "Present shell.run arguments immediately above the shell source rail."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (command (or (and arguments (json-get arguments "command")) "")))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ shell.run"
     :rows (append
            (application--generic-argument-rows
             application arguments :excluded-keys '("command"))
            (application--shell-command-rows command)))))

(defmethod application-tool-call-entry
    ((tool lisp-eval-tool) (application application) (call hash-table))
  "Present lisp.eval forms as bounded Lisp source, one form per line."
  (declare (ignore tool))
  (application--lisp-call-entry application call "forms"))


(defmethod application-tool-call-entry
    ((tool self-eval-tool) (application application) (call hash-table))
  "Present self.eval forms and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "forms"))

(defmethod application-tool-call-entry
    ((tool self-exercise-tool) (application application) (call hash-table))
  "Present a self.exercise form as bounded Lisp source."
  (declare (ignore tool))
  (application--lisp-call-entry application call "form"))

(defmethod application-tool-call-entry
    ((tool self-redefine-tool) (application application) (call hash-table))
  "Present a self.redefine definition and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "definition"))

(defmethod application-tool-call-entry
    ((tool self-persist-definition-tool)
     (application application)
     (call hash-table))
  "Present a durable definition and any selected restart separately."
  (declare (ignore tool))
  (application--lisp-call-entry application call "definition"))

(defmethod application-tool-call-entry
    ((tool self-set-tool) (application application) (call hash-table))
  "Present a self.set binding and syntax-highlighted value form."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (value
           (application--presentation-value
            (or (and arguments (json-get arguments "value")) ""))))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ self.set"
     :rows (append
           (application--generic-argument-rows
            application arguments :excluded-keys '("value" "restart-value"))
           (application--source-preview-rows
            value :language *application-common-lisp-language*)
           (application--restart-value-rows arguments)))))


;;; Tool result layout

(-> application--tool-result-success-p (list) boolean)
(defun application--tool-result-success-p (record)
  "Return true when tool result RECORD has successful status."
  (eq (getf (rest record) :status) ':ok))

(-> application--tool-result-timing (list) (option string))
(defun application--tool-result-timing (record)
  "Return RECORD's CPU and real duration as a concise detail string."
  (let ((cpu (getf (rest record) :cpu-microseconds))
        (real (getf (rest record) :real-microseconds)))
    (when (and (typep cpu '(integer 0))
               (typep real '(integer 0)))
      (format nil "cpu ~,3Fs · real ~,3Fs"
              (/ cpu 1000000.0d0)
              (/ real 1000000.0d0)))))

(-> application--tool-result-entry
    (application list &key (:detail (option string)) (:rows list))
    list)
(defun application--tool-result-entry (application record &key detail rows)
  "Return RECORD's status header with optional DETAIL and styled ROWS."
  (let* ((success-p (application--tool-result-success-p record))
         (tool-name (application--tool-display-name
                     application (getf (rest record) :tool)))
         (timing (application--tool-result-timing record))
         (complete-detail
           (cond
             ((and detail timing)
              (format nil "~A · ~A" detail timing))
             (detail detail)
             (timing timing))))
    (application--tool-entry
     application
     :style (if success-p ':success ':failure)
     :header (format nil "~:[✗ ~A failed~;✓ ~A~]" success-p tool-name)
     :detail complete-detail
     :rows rows)))

(-> application--section-preview-rows
    (string string terminal-style &key (:limit integer)
                                      (:language (option language)))
    list)
(defun application--section-preview-rows
    (label text style &key (limit *application-tool-output-lines*) language)
  "Return a labeled, bounded transcript section, highlighting declared LANGUAGE."
  (append
   (list (application--tool-section-row label))
   (cond
     ((not (non-empty-string-p text))
      (list (list (terminal-span ':dim "│ (none)"))))
     (language
      (application--source-preview-rows text :language language :limit limit))
     (t
      (application--preview-rows text style limit :gutter "│ ")))))

(-> application--evaluation-parts
    (string)
    (values (option string) (option string)))
(defun application--evaluation-parts (output)
  "Return captured output and rendered values from an evaluation OUTPUT."
  (let* ((marker (format nil "Values:~%"))
         (values-position (search marker output :from-end t)))
    (if values-position
        (let* ((prefix (string-right-trim
                        '(#\Space #\Tab #\Newline #\Return)
                        (subseq output 0 values-position)))
               (captured (if (uiop:string-prefix-p "Output:" prefix)
                             (string-left-trim
                              '(#\Newline #\Return)
                              (subseq prefix (length "Output:")))
                             prefix))
               (values-text (string-trim
                             '(#\Space #\Tab #\Newline #\Return)
                             (subseq output
                                     (+ values-position (length marker))))))
          (values (and (plusp (length captured)) captured)
                  values-text))
        (values nil nil))))

(-> application--evaluation-result-rows (string) list)
(defun application--evaluation-result-rows (output)
  "Return styled output and values sections from evaluation OUTPUT."
  (multiple-value-bind (captured values-text)
      (application--evaluation-parts output)
    (if (or captured values-text)
        (append
         (when captured
           (application--section-preview-rows "output" captured ':plain))
         (when (and captured values-text) (list nil))
         (application--section-preview-rows
          "values" (or values-text "") ':code
          :language *application-common-lisp-language*))
        (application--preview-rows output
                                   ':dim
                                   *application-tool-output-lines*
                                   :gutter "│ "))))

(-> application--labeled-output-rows (string &key (:limit integer)) list)
(defun application--labeled-output-rows
    (output &key (limit *application-tool-inspection-lines*))
  "Return OUTPUT as aligned fields, headings, and readable continuation rows."
  (let* ((lines (or (application--display-lines output) (list "")))
         (visible-count (min limit (length lines)))
         (omitted (- (length lines) visible-count)))
    (append
     (loop for line in (subseq lines 0 visible-count)
           for colon = (position #\: line)
           collect
           (cond
             ((zerop (length line))
              nil)
             ((and colon
                   (= colon (1- (length line))))
              (list (terminal-span ':strong (subseq line 0 colon))))
             ((and colon
                   (< colon (1- (length line)))
                   (char= (char line (1+ colon)) #\Space))
              (list (terminal-span ':dim
                                   (format nil "~18A " (subseq line 0 colon)))
                    (terminal-span ':plain
                                   (string-left-trim
                                    '(#\Space)
                                    (subseq line (1+ colon))))))
             ((member (char line 0) '(#\Space #\Tab))
              (list (terminal-span ':code line)))
             (t
              (list (terminal-span ':plain line)))))
     (when (plusp omitted)
       (list (list (terminal-span
                    ':dim
                    (format nil "… +~D more line~:P" omitted))))))))

(-> application--restart-row (string) list)
(defun application--restart-row (line)
  "Return one aligned restart NAME and report row parsed from LINE."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (separator (position-if (lambda (character)
                                   (member character '(#\Space #\Tab)))
                                 trimmed)))
    (if separator
        (list (terminal-span ':notice
                             (format nil "~18A " (subseq trimmed 0 separator)))
              (terminal-span ':plain
                             (string-left-trim
                              '(#\Space #\Tab)
                              (subseq trimmed separator))))
        (list (terminal-span ':notice trimmed)))))

(-> application--debugger-rows (string) list)
(defun application--debugger-rows (output)
  "Return separate condition, restart, and retry areas from debugger OUTPUT."
  (let* ((heading (format nil "Available restarts:~%"))
         (heading-position (search heading output))
         (restart-start (and heading-position
                             (+ heading-position (length heading))))
         (retry-position (and restart-start
                              (search "Retry the identical call"
                                      output
                                      :start2 restart-start)))
         (condition-text
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (subseq output 0 (or heading-position
                                             (length output)))))
         (restart-text
           (and restart-start
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (subseq output restart-start
                                     (or retry-position (length output))))))
         (retry-text
           (and retry-position
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (subseq output retry-position)))))
    (append
     (application--section-preview-rows "condition" condition-text ':plain)
     (when restart-text
       (append
        (list nil (application--tool-section-row "available restarts"))
        (loop for line in (subseq
                           (application--display-lines restart-text)
                           0
                           (min *application-tool-output-lines*
                                (length (application--display-lines
                                         restart-text))))
              collect (application--restart-row line))))
     (when retry-text
       (append (list nil (application--tool-section-row "retry"))
               (application--preview-rows
                retry-text ':hint *application-tool-output-lines*
                :gutter "│ "))))))

(-> application--failure-result-rows
    (string &key (:tool-name (option string)))
    list)
(defun application--failure-result-rows (output &key tool-name)
  "Return condition details, omitting TOOL-NAME's redundant failure prefix."
  (let* ((prefix (and tool-name (format nil "~A failed: " tool-name)))
         (output (if (and prefix (uiop:string-prefix-p prefix output))
                     (subseq output (length prefix))
                     output))
         (restart-marker (format nil "Available restarts:~%"))
         (backtrace-marker (format nil "~%~%Backtrace:~%")))
    (cond
      ((search restart-marker output)
       (application--debugger-rows output))
      ((search backtrace-marker output)
       (let* ((position (search backtrace-marker output))
              (message (subseq output 0 position))
              (backtrace (subseq output
                                 (+ position (length backtrace-marker)))))
         (append
          (application--section-preview-rows "condition" message ':plain)
          (list nil)
          (application--section-preview-rows "backtrace"
                                             backtrace
                                             ':dim))))
      (t
       (application--preview-rows output
                                  ':plain
                                  *application-tool-output-lines*
                                  :gutter "│ ")))))

(-> application--structured-json-safe-p (string) boolean)
(defun application--structured-json-safe-p (text)
  "Bound JSON input size and nesting before invoking the recursive decoder."
  (and (<= (length text) 131072)
       (loop with depth = 0
             with quoted-p = nil
             with escaped-p = nil
             for character across text
             do (cond
                  (escaped-p
                   (setf escaped-p nil))
                  ((and quoted-p (char= character #\\))
                   (setf escaped-p t))
                  ((char= character #\")
                   (setf quoted-p (not quoted-p)))
                  ((not quoted-p)
                   (case character
                     ((#\{ #\[) (incf depth))
                     ((#\} #\]) (decf depth)))))
             never (> depth 32))))

(-> application--structured-output-value (string) (values t boolean))
(defun application--structured-output-value (output)
  "Decode a JSON object or array OUTPUT for transcript presentation.

Only complete object and array documents opt into this path.  Other output
remains ordinary bounded tool text, including malformed untrusted JSON."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) output)))
    (if (and (plusp (length trimmed))
             (member (char trimmed 0) '(#\{ #\[))
             (application--structured-json-safe-p trimmed))
        (handler-case
            (let ((value (json-decode trimmed)))
              (if (or (json-object-p value)
                      (and (vectorp value) (not (stringp value))))
                  (values value t)
                  (values nil nil)))
          (error ()
            (values nil nil)))
        (values nil nil))))

(-> application--structured-output-rows (application string) list)
(defun application--structured-output-rows (application output)
  "Return bounded recursive fields for JSON OUTPUT or a safe text preview."
  (multiple-value-bind (value structured-p)
      (application--structured-output-value output)
    (if structured-p
        (let* ((*application-tool-structure-budget* 64)
               (rows (if (json-object-p value)
                         (application--structured-object-rows application value :depth 8)
                         (application--structured-value-rows application "result" value :depth 8))))
          (if (or (> (length rows) *application-tool-output-lines*)
                  (minusp *application-tool-structure-budget*))
              (append (subseq rows 0 (min (length rows) *application-tool-output-lines*))
                      (list (list (terminal-span ':dim "… more fields"))))
              rows))
        (or (application--code-output-rows application output)
            (application--preview-rows output ':dim *application-tool-output-lines*
                                       :gutter "│ ")))))

(-> application--mcp-content-sections (t string) list)
(defun application--mcp-content-sections (details output)
  "Return valid MCP section metadata, or NIL for a malformed or clipped record."
  (when (and (proper-list-p details)
             (eq (first details) ':mcp-content)
             (evenp (length (rest details))))
    (let ((sections (getf (rest details) :sections)))
      (when (and (proper-list-p sections)
                 (<= (length sections) (1+ *mcp-maximum-content-blocks*))
                 (every (lambda (section)
                          (and (proper-list-p section)
                               (= (length section) 3)
                               (member (first section) '(:text :structured))
                               (typep (second section) '(integer 0))
                               (typep (third section) '(integer 0))
                               (<= (second section) (third section) (length output))))
                        sections))
        sections))))

(-> application--mcp-content-rows (application list) list)
(defun application--mcp-content-rows (application record)
  "Format validated MCP content sections independently within one row budget."
  (let* ((output (or (getf (rest record) :output) ""))
         (sections (application--mcp-content-sections
                    (getf (rest record) :details) output))
         (rows nil)
         (processed 0))
    (dolist (section sections)
      (when (>= (length rows) *application-tool-output-lines*)
        (return))
      (incf processed)
      (destructuring-bind (kind start end) section
        (declare (ignore kind))
        (setf rows
              (append rows
                      (application--structured-output-rows
                       application (subseq output start end))))))
    (if (or (> (length rows) *application-tool-output-lines*)
            (< processed (length sections)))
        (append (subseq rows 0 (min (length rows) *application-tool-output-lines*))
                (list (list (terminal-span ':dim "… more content"))))
        rows)))

(-> application--native-details-value (t) (values t boolean))
(defun application--native-details-value (details)
  "Project retained native task or mission DETAILS into bounded display data.

Never read serialized Lisp. Limit traversal, including malformed or circular
lists, independently of the producer's transport limits."
  (block invalid
    (let ((remaining 256))
      (labels ((project (value depth)
                 (when (or (minusp (decf remaining)) (zerop depth))
                   (return-from project "… more details"))
                 (cond
                   ((stringp value)
                    (bounded-string value :limit 500))
                   ((keywordp value)
                    (string-downcase (symbol-name value)))
                   ((or (null value) (numberp value) (eq value t) (json-false-p value))
                    value)
                   ((json-object-p value)
                    (let ((object (make-hash-table :test #'equal)) (count 0))
                      (block fields
                        (maphash (lambda (key item)
                                   (unless (stringp key)
                                     (return-from invalid (values nil nil)))
                                   (when (>= count 64)
                                     (setf (gethash "…" object) "more fields")
                                     (return-from fields nil))
                                   (incf count)
                                   (setf (gethash (bounded-string key :limit 80) object)
                                         (project item (1- depth))))
                                 value))
                      object))
                   ((consp value)
                    (if (keywordp (first value))
                        (let ((object (make-hash-table :test #'equal))
                              (tail value))
                          (loop repeat 64
                                while tail
                                do (unless (and (consp tail) (keywordp (first tail))
                                                (consp (rest tail)))
                                     (return-from invalid (values nil nil)))
                                   (setf (gethash (string-downcase (symbol-name (first tail))) object)
                                         (project (second tail) (1- depth))
                                         tail (cddr tail)))
                          (when tail (setf (gethash "…" object) "more fields"))
                          object)
                        (let ((items nil) (tail value))
                          (loop repeat *application-tool-structure-items*
                                while (consp tail)
                                do (push (project (first tail) (1- depth)) items)
                                   (setf tail (rest tail)))
                          (unless (or (null tail) (consp tail))
                            (return-from invalid (values nil nil)))
                          (when tail (push "… more items" items))
                          (coerce (nreverse items) 'vector))))
                   ((and (vectorp value) (not (stringp value)))
                    (let* ((count (min (length value) *application-tool-structure-items*))
                           (items (loop for index below count
                                        collect (project (aref value index) (1- depth)))))
                      (when (< count (length value))
                        (setf items (append items (list "… more items"))))
                      (coerce items 'vector)))
                   (t
                    (return-from invalid (values nil nil))))))
        (when (consp details)
          (let ((tagged-p (member (first details)
                                  '(:task-run :task-agents :job-list :job-get :job-wait
                                    :job-cancel :job-send :mission))))
            (when (or tagged-p
                      (member (first details)
                              '(:job :continuity-list :classification :job-id :execution-id)))
              (let ((value (project (if tagged-p (rest details) details) 8)))
                (if (json-object-p value)
                    (values value t)
                    (values nil nil))))))))))

(-> application--native-job-rows (application json-object) list)
(defun application--native-job-rows (application object)
  "Show one bounded native task, agent, or job identity and its progress."
  (let* ((result       (json-get object "result"))
         (progress     (json-get object "progress"))
         (diagnostic-p (equal (json-get object "kind") "diagnostic"))
         (state        (or (json-get object "state") (json-get object "classification")
                           (json-get object "status") (and diagnostic-p "failed")))
         (status       (and (json-object-p result) (json-get result "status")))
         (identity     (or (json-get object "id") (json-get object "name")
                           (json-get object "job-id") (json-get object "execution-id") "task"))
         (role         (or (json-get object "agent") (json-get object "role")
                           (json-get object "tool")))
         (summary      (or (and (json-object-p result)
                                (or (json-get result "summary") (json-get result "label")
                                    (json-get result "output") (json-get result "content")))
                           (json-get object "summary") (json-get object "description")))
         (failure      (or (and (json-object-p result) (json-get result "error"))
                           (json-get object "cause") (json-get object "condition-report")
                           (json-get object "cancellation-reason")))
         (duration     (or (and (json-object-p progress) (json-get progress "duration-ms"))
                           (and (json-object-p result) (json-get result "duration-ms"))
                           (json-get object "duration-ms")))
         (requests     (and (json-object-p progress) (json-get progress "request-count")))
         (current-tool (and (json-object-p progress) (json-get progress "current-tool")))
         (path         (or (json-get object "path") (json-get object "pathname")
                           (and (json-object-p result) (json-get result "output-path")))))
    (append
     (list (append
            (list (terminal-span (if (or (equal status "failure") (equal state "failed"))
                                     ':failure ':strong)
                                 (sanitize-text (application--presentation-value identity) :single-line-p t)))
            (loop for value in (list state (unless (equal state status) status))
                  when value
                    append (list (terminal-span ':dim " · ")
                                 (terminal-span (application--structured-value-style "state" value)
                                                (sanitize-text (application--presentation-value value)
                                                               :single-line-p t))))))
     (application--tool-field-rows
      application
      (loop for (label value style) in
            (list (list "role" role ':code)
                  (list "duration" (and duration (format nil "~A ms" duration)) ':plain)
                  (list "requests" (and requests (princ-to-string requests)) ':plain)
                  (list "current tool" current-tool ':code)
                  (list "summary" summary ':plain)
                  (list "condition" failure (if (equal state "cancelled") ':notice ':failure))
                  (list "path" path ':code))
            when value
              collect (list :label label
                            :value (bounded-string (application--presentation-value value) :limit 200)
                            :style style))))))

(-> application--native-details-rows (application list) list)
(defun application--native-details-rows (application record)
  "Render retained portable native details with bounded semantic fields."
  (multiple-value-bind (value valid-p)
      (application--native-details-value (getf (rest record) :details))
    (when valid-p
      (let* ((job (json-get value "job"))
             (items (or (json-get value "results") (json-get value "jobs")
                        (json-get value "entries")))
             (rows (cond
                     ((json-object-p job)
                      (application--native-job-rows application job))
                     ((or (json-get value "classification") (json-get value "job-id"))
                      (application--native-job-rows application value))
                     ((and (vectorp items) (not (stringp items)))
                      (if (zerop (length items))
                          (list (list (terminal-span ':dim "no entries")))
                          (loop for item across items
                                if (json-object-p item)
                                  append (application--native-job-rows application item)
                                else
                                  collect (list (terminal-span ':dim "… more entries")))))
                     (t
                      (application--structured-object-rows application value :depth 8))))
             (paging (when (json-get value "next-offset")
                       (application--tool-field-rows
                        application
                        (list (list :label "next offset"
                                    :value (princ-to-string (json-get value "next-offset")))))))
             (all-rows (append rows paging)))
        (if (> (length all-rows) *application-tool-output-lines*)
            (append (subseq all-rows 0 *application-tool-output-lines*)
                    (list (list (terminal-span ':dim "… more details"))))
            all-rows)))))

(-> application--native-snapshot-rows (application list) list)
(defun application--native-snapshot-rows (application snapshot)
  "Return bounded semantic span rows for one native job or continuity SNAPSHOT.

Accept :ID or :JOB-ID, :STATE or :CLASSIFICATION, :PROGRESS, :RESULT and :PATH.
The same projection is used for retained tool details and inspector snapshots."
  (application--native-details-rows
   application (list :tool-result :details (list :job snapshot))))

(-> application--generic-tool-result-entry (application list) list)
(defun application--generic-tool-result-entry (application record)
  "Return a readable fallback entry for tool result RECORD.

This also formats JSON returned by dynamic tools as bounded fields instead of
re-emitting untrusted serialized JSON."
  (let ((output (or (getf (rest record) :output) "")))
    (application--tool-result-entry
     application
     record
     :rows (or (application--native-details-rows application record)
               (application--mcp-content-rows application record)
               (if (application--tool-result-success-p record)
                   (application--structured-output-rows application output)
                   (application--failure-result-rows
                    output :tool-name (getf (rest record) :tool)))))))

(-> application--tool-execution-handoff-entry (application list list) list)
(defun application--tool-execution-handoff-entry (application record handoff)
  "Present a background handoff as job state and identity rather than serialized output."
  (let* ((job (getf (rest handoff) :job))
         (state (getf job :state))
         (reason (getf (rest handoff) :handoff-reason))
         (description (getf job :description))
         (status (case state
                   (:queued "queued in background")
                   (:running
                    (if (eq reason ':grace-expired)
                        "still running in background"
                        "running in background"))
                   (:completed "result ready")
                   (:failed "failed")
                   ((:cancelled :aborted) "cancelled")
                   (otherwise "background handoff"))))
    (application--tool-entry
     application
     :style (if (eq state ':failed) ':failure ':notice)
     :header (format nil "↪ ~A"
                     (application--tool-display-name
                      application (getf (rest record) :tool)))
     :detail (format nil "~A · ~A" status (getf job :id))
     :rows (unless (application-compact-view-p application)
             (application--tool-field-rows
              application
              (append
               (when (non-empty-string-p description)
                 (list (list :label "task"
                             :value (bounded-string description :limit 200))))
               (when (eq reason ':grace-expired)
                 (list (list :label "handoff"
                             :value "blocking wait expired")))))))))

(defmethod application-tool-result-entry :around
    (tool (application application) record)
  "Present native background handoffs before any tool-specific output renderer."
  (declare (ignore tool))
  (let ((handoff (application--tool-execution-handoff record)))
    (if handoff
        (application--tool-execution-handoff-entry application record handoff)
        (call-next-method))))

(defmethod application-tool-result-entry
    ((tool tool) (application application) record)
  "Present RECORD using the generic readable result layout."
  (declare (ignore tool))
  (application--generic-tool-result-entry application record))

(defmethod application-tool-result-entry
    ((tool null) (application application) record)
  "Present an unregistered tool result using the generic readable layout."
  (declare (ignore tool))
  (application--generic-tool-result-entry application record))

(-> application--papercut-result-identifier (string) (option string))
(defun application--papercut-result-identifier (output)
  "Return the papercut identifier encoded in successful tool OUTPUT."
  (let ((line (first (application--display-lines output))))
    (when (and line (uiop:string-prefix-p "papercut-id: " line))
      (let ((identifier (string-trim
                         '(#\Space #\Tab)
                         (subseq line (length "papercut-id: ")))))
        (and (non-empty-string-p identifier) identifier)))))

(-> application--papercut-result-duplicate-p (string) boolean)
(defun application--papercut-result-duplicate-p (output)
  "Return true when successful tool OUTPUT says the report repeated an active papercut."
  (some (lambda (line)
          (uiop:string-prefix-p "already-reported:" line))
        (application--display-lines output)))

(defmethod application-tool-result-entry
    ((tool papercut-report-tool) (application application) record)
  "Present a successful papercut report as a prominent persistent alert.

A report that only repeated an active papercut is presented as such, so the
user sees the existing report rather than a new one."
  (if (application--tool-result-success-p record)
      (let* ((output (or (getf (rest record) :output) ""))
             (identifier (application--papercut-result-identifier output))
             (papercut
               (and identifier
                    (papercut-find
                     (application-configuration application)
                     identifier)))
             (rows
               (if papercut
                   (append
                    (list (list
                           (terminal-span
                            ':strong
                            (sanitize-text (papercut-title papercut)
                                           :single-line-p t))))
                    (application--preview-rows
                     (papercut-content papercut)
                     ':plain
                     *application-tool-output-lines*
                     :gutter "│ ")
                    (list (list
                            (terminal-span
                             ':hint
                             (format nil "Read full: ~A"
                                     (papercut-call-source papercut))))))
                   (application--preview-rows
                    output
                    ':plain
                    *application-tool-output-lines*
                    :gutter "│ "))))
        (application--tool-entry
         application
         :style (if (application--papercut-result-duplicate-p output)
                    ':notice
                    ':failure)
         :header (if (application--papercut-result-duplicate-p output)
                     "! PAPERCUT ALREADY REPORTED"
                     "! PAPERCUT RECORDED")
         :detail (and papercut
                      (format nil "id ~A"
                              (papercut-short-identifier papercut)))
         :rows rows))
      (call-next-method)))

(-> application--shell-capture-header (string) (option list))
(defun application--shell-capture-header (line)
  "Return LINE's capture label, retained and observed bytes, and completeness.

LINE is a shell.run capture header such as \"stdout: 12 bytes retained, 12 bytes
observed; capture-complete yes; status complete\"; any other line returns NIL."
  (let ((colon (search ": " line))
        (retained-marker (search " bytes retained, " line))
        (observed-marker (search " bytes observed; capture-complete " line)))
    (when (and colon retained-marker observed-marker (< colon retained-marker observed-marker))
      (let ((retained (parse-integer line :start (+ colon 2) :end retained-marker
                                          :junk-allowed t))
            (observed (parse-integer line :start (+ retained-marker
                                                    (length " bytes retained, "))
                                          :end observed-marker :junk-allowed t)))
        (when (and retained observed)
          (list :label (subseq line 0 colon)
                :retained retained
                :observed observed
                :complete-p (and (search "capture-complete yes" line) t)))))))

(-> application--shell-result-parts (string) list)
(defun application--shell-result-parts (output)
  "Split shell.run OUTPUT into its execution line, notes, and capture sections.

Return a plist with :EXIT, :EXECUTION, :NOTES and :SECTIONS. Each section is a
capture header plist extended with the :LINES of its preview. Log references
and byte counts are kept for the model and are not part of any preview."
  (let ((lines (application--display-lines output))
        (exit nil)
        (execution nil)
        (notes nil)
        (sections nil))
    (when (and lines (uiop:string-prefix-p "exit " (first lines)))
      (setf exit (pop lines)))
    (when (and lines (uiop:string-prefix-p "Execution: " (first lines)))
      (setf execution (pop lines)))
    (loop while lines
          do (let* ((line (pop lines))
                    (header (application--shell-capture-header line)))
               (cond
                 (header
                  (when (and lines (uiop:string-prefix-p "Log: " (first lines)))
                    (pop lines))
                  (push (append header (list :lines nil)) sections))
                 ((or (uiop:string-prefix-p "Output capture unavailable." line)
                      (search " preview unavailable: " line)
                      (uiop:string-prefix-p "Log metadata diagnostic: " line))
                  (push line notes)
                  (when (and lines (uiop:string-prefix-p "Log: " (first lines)))
                    (pop lines)))
                 (t
                  ;; Output before any capture header belongs to one plain section.
                  (unless sections
                    (push (list :label "output" :retained 0 :observed 0
                                :complete-p t :lines nil)
                          sections))
                  (push line (getf (first sections) :lines))))))
    (list :exit exit
          :execution execution
          :notes (nreverse notes)
          :sections (loop for section in (nreverse sections)
                          collect (let ((section-lines (getf section :lines)))
                                    (loop while (and section-lines
                                                     (string= (first section-lines) ""))
                                          do (pop section-lines))
                                    (setf (getf section :lines) (nreverse section-lines))
                                    section)))))

(-> application--shell-execution-rows ((option string)) list)
(defun application--shell-execution-rows (execution)
  "Return a highlighted row when EXECUTION reports a timeout or cancellation."
  (when execution
    (append
     (when (search "timeout yes" execution)
       (list (list (terminal-span ':failure "timed out"))))
     (when (search "cancelled yes" execution)
       (list (list (terminal-span ':notice "cancelled")))))))

(-> application--shell-section-rows (application list boolean) list)
(defun application--shell-section-rows (application section labeled-p)
  "Return SECTION's preview rows, headed by its stream name when LABELED-P."
  (let* ((lines (getf section :lines))
         (stderr-p (string= (getf section :label) "stderr"))
         (text (format nil "~{~A~^~%~}"
                       (mapcar (lambda (line)
                                 (if (and (uiop:string-prefix-p "[" line)
                                          (search " retained bytes omitted from preview]" line))
                                     "…"
                                     line))
                               lines))))
    (append
     (when labeled-p
       (list (application--tool-section-row (getf section :label))))
     (unless (getf section :complete-p)
       (list (list (terminal-span
                    ':notice
                    (format nil "output truncated: ~:D of ~:D bytes kept"
                            (getf section :retained)
                            (getf section :observed))))))
     (when lines
       (or (unless stderr-p
             (application--code-output-rows application text :json-p t))
           (application--preview-rows text
                                      (if stderr-p ':failure ':dim)
                                      *application-tool-output-lines*
                                      :gutter "│ "))))))

(defmethod application-tool-result-entry
    ((tool shell-run-tool) (application application) record)
  "Present a command's exit status and a short output preview.

The exit status is highlighted in the header. Execution and capture metadata
appear only when something went wrong, such as a timeout or truncated output;
log references and byte counts are for the model."
  (let ((output (or (getf (rest record) :output) "")))
    (if (uiop:string-prefix-p "exit " output)
        (let* ((parts (application--shell-result-parts output))
               (exit (getf parts :exit))
               (sections (remove-if (lambda (section)
                                      (and (null (getf section :lines))
                                           (getf section :complete-p)))
                                    (getf parts :sections)))
               (clean-p (and (string= exit "exit 0")
                             (application--tool-result-success-p record))))
          (application--tool-entry
           application
           :style (if clean-p ':success ':failure)
           :header (format nil "~:[✗~;✓~] ~A ~A"
                           clean-p (getf (rest record) :tool) exit)
           :detail (application--tool-result-timing record)
           :rows (append
                  (application--shell-execution-rows (getf parts :execution))
                  (loop for note in (getf parts :notes)
                        collect (list (terminal-span ':notice note)))
                  (loop for section in sections
                        append (application--shell-section-rows
                                application section (and (rest (getf parts :sections)) t))))))
        (call-next-method))))

(-> application--first-line-summary (string) string)
(defun application--first-line-summary (output)
  "Return OUTPUT's first line without its final period, for a result header detail."
  (string-right-trim "." (or (first (application--display-lines output)) "")))

(defmethod application-tool-result-entry
    ((tool resource-edit-tool) (application application) record)
  "Present a successful edit as its one-line outcome and any delimiter warning.

The call entry already shows the change. The revision, anchors and line window
that follow are the model's next observation, not something the user reads."
  (if (application--tool-result-success-p record)
      (let* ((output (or (getf (rest record) :output) ""))
             (warning (search "WARNING: " output)))
        (application--tool-result-entry
         application
         record
         :detail (application--first-line-summary output)
         :rows (when warning
                 (application--preview-rows (subseq output warning)
                                            ':failure
                                            *application-tool-output-lines*))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool lisp-paren-check-tool) (application application) record)
  "Present a clean delimiter check as one line; problems keep the full report."
  (if (application--tool-result-success-p record)
      (let* ((output (or (getf (rest record) :output) ""))
             (checked (find-if (lambda (line) (uiop:string-prefix-p "Checked " line))
                               (application--display-lines output)))
             (count (and checked
                         (parse-integer (remove #\, checked)
                                        :start (length "Checked ") :junk-allowed t))))
        (application--tool-result-entry
         application
         record
         :detail (if count
                     (format nil "~:D file~:P balanced" count)
                     "balanced")))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool lisp-tool) (application application) record)
  "Present successful worker evaluations as separate output and values areas."
  (if (application--tool-result-success-p record)
      (let ((output (or (getf (rest record) :output) "")))
        (multiple-value-bind (captured values-text)
            (application--evaluation-parts output)
          (if (or captured values-text)
              (application--tool-result-entry
               application
               record
               :rows (application--evaluation-result-rows output))
              (call-next-method))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-eval-tool) (application application) record)
  "Present self.eval output and values in separate bounded areas."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--evaluation-result-rows
              (or (getf (rest record) :output) "")))
      (call-next-method)))


(defmethod application-tool-result-entry
    ((tool lisp-describe-tool) (application application) record)
  "Present lisp.describe output as structured description and values sections."
  (if (application--tool-result-success-p record)
      (let ((output (or (getf (rest record) :output) "")))
        (multiple-value-bind (captured values-text)
            (application--evaluation-parts output)
          (application--tool-result-entry
           application
           record
           :rows (if (or captured values-text)
                     (append
                      (when captured
                        (append
                         (list (application--tool-section-row "description"))
                         (application--labeled-output-rows captured)))
                      (when (and captured values-text) (list nil))
                      (application--section-preview-rows
                       "values" (or values-text "") ':code
                       :language *application-common-lisp-language*))
                     (application--labeled-output-rows output)))))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool lisp-source-tool) (application application) record)
  "Present matching source as a bounded Lisp code area."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--source-preview-rows
              (or (getf (rest record) :output) "")
              :language *application-common-lisp-language*
              :limit *application-tool-output-lines*))
      (call-next-method)))

(defmethod application-tool-result-entry
    ((tool self-diff-tool) (application application) record)
  "Present pending active-image mutations as bounded Lisp source."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--source-preview-rows
              (or (getf (rest record) :output) "")
              :language *application-common-lisp-language*
              :limit *application-tool-output-lines*))
      (call-next-method)))


;;;; -- Numbered Resource Source --

(-> application--numbered-source-line (string) (option cons))
(defun application--numbered-source-line (line)
  "Split a numbered resource LINE into its exact annotation and source text."
  (let* ((start (position-if-not (lambda (character) (char= character #\Space)) line))
         (end (and start (position-if-not #'digit-char-p line :start start))))
    (when (and start end (< start end))
      (let ((separator
              (cond
                ((and (char= (char line end) #\:)
                      (<= (+ end 7) (length line))
                      (every (lambda (character) (digit-char-p character 16))
                             (subseq line (1+ end) (+ end 5)))
                      (string= "  " line :start2 (+ end 5) :end2 (+ end 7)))
                 (+ end 7))
                ((and (<= (+ end 2) (length line))
                      (string= "  " line :start2 end :end2 (+ end 2)))
                 (+ end 2)))))
        (when separator
          (cons (subseq line 0 separator) (subseq line separator)))))))

(-> application--resource-source-rows (string) (option list))
(defun application--resource-source-rows (output)
  "Highlight a retained workspace file window without resolving or rereading it."
  (let* ((lines (application--display-lines output))
         (identity (first lines))
         (content (member "Content:" lines :test #'string=))
         (uri (and identity (uiop:string-prefix-p "URI: " identity) (subseq identity 5)))
         (path (and uri (application--file-resource-path uri))))
    (when (and path content (member "Kind: file" lines :test #'string=))
      (let* ((parts (mapcar #'application--numbered-source-line (rest content)))
             (visible-count (min *application-tool-output-lines* (length parts)))
             (elision (find-if (lambda (line)
                                (and (uiop:string-prefix-p "Elided: " line)
                                     (not (string= line "Elided: none"))))
                              (ldiff lines content))))
        (when (and parts (every #'consp parts))
          (let* ((source (format nil "~{~A~^~%~}" (mapcar #'rest parts)))
                 (highlighted (syntax--highlight-lines source :pathname path)))
            (append
             (list (list (terminal-span ':dim identity)))
             (loop for part in parts
                   for index below visible-count
                   collect (append
                            (list (terminal-span ':success "│ ")
                                  (terminal-span ':dim (first part)))
                            (if (and highlighted (< index (length highlighted)))
                                (aref highlighted index)
                                (list (terminal-span ':plain (rest part))))))
             (when (> (length parts) visible-count)
               (list (list (terminal-span ':dim
                                          (format nil "… +~D more source lines"
                                                  (- (length parts) visible-count))))))
             (when elision
               (list (list (terminal-span ':dim elision)))))))))))

(defmethod application-tool-result-entry
    ((tool resource-read-tool) (application application) record)
  "Present numbered file contents with their path's source language and anchors."
  (let ((rows (and (application--tool-result-success-p record)
                   (application--resource-source-rows
                    (or (getf (rest record) :output) "")))))
    (if rows
        (application--tool-result-entry application record :rows rows)
        (call-next-method))))


;;;; -- Search Source Matches --

(-> application--search-result-path (string) (option string))
(defun application--search-result-path (line)
  "Extract the path from a retained PATH:LINE:COLUMN search heading."
  (let* ((end (or (search " [git:" line :from-end t) (length line)))
         (column (position #\: line :end end :from-end t))
         (number (and column (position #\: line :end column :from-end t))))
    (when (and number column (plusp number)
               (< (1+ number) column) (< (1+ column) end)
               (every #'digit-char-p (subseq line (1+ number) column))
               (every #'digit-char-p (subseq line (1+ column) end)))
      (subseq line 0 number))))

(-> application--search-source-rows (string) (option list))
(defun application--search-source-rows (output)
  "Highlight bounded search snippets using retained headings, not current files."
  (let* ((lines (application--display-lines output))
         (visible-count (min *application-tool-output-lines* (length lines)))
         (path nil)
         (highlighted-p nil)
         (rows
           (loop for line in (subseq lines 0 visible-count)
                 for heading = (application--search-result-path line)
                 for marker = (position-if-not (lambda (character) (char= character #\Space)) line)
                 for source = (and path marker (char= (char line marker) #\>)
                                   (application--numbered-source-line (subseq line (1+ marker))))
                 collect
                 (cond
                   (heading
                    (setf path heading)
                    (list (terminal-span ':dim "│ ") (terminal-span ':strong line)))
                   (source
                    (let ((spans (syntax--highlight-spans (rest source) :pathname path)))
                      (when spans (setf highlighted-p t))
                      (append (list (terminal-span ':dim "│ ")
                                    (terminal-span ':dim
                                                   (concatenate 'string
                                                                (subseq line 0 (1+ marker))
                                                                (first source))))
                              (or spans (list (terminal-span ':plain (rest source)))))))
                   (t
                    (setf path nil)
                    (list (terminal-span ':dim "│ ") (terminal-span ':dim line)))))))
    (when highlighted-p
      (append rows
              (when (> (length lines) visible-count)
                (list (list (terminal-span ':dim "… more search results"))))))))

(defmethod application-tool-result-entry
    ((tool search-content-tool) (application application) record)
  "Present source matches with path-derived token styles and original annotations."
  (let ((rows (and (application--tool-result-success-p record)
                   (application--search-source-rows
                    (or (getf (rest record) :output) "")))))
    (if rows
        (application--tool-result-entry application record :rows rows)
        (call-next-method))))
