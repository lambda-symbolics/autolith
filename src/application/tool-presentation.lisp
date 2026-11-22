(in-package #:autolith)

;;;; -- Tool Transcript Presentation --


(-> application--transcript-columns (application) (integer 1))
(defun application--transcript-columns (application)
  "Return the effective columns for one source-backed transcript layout."
  (or *application-transcript-layout-columns*
      (max 1 (terminal-columns (terminal-ui-terminal (application-ui application))))))

(defparameter *application-tool-output-lines* 12
  "The maximum tool output lines shown in the terminal transcript.")

(defparameter *application-tool-inspection-lines* 24
  "The maximum introspection lines shown in the terminal transcript.")

(-> application--preview-rows
    (string terminal-style integer &key (:gutter (option string)))
    list)
(defun application--preview-rows (text style limit &key gutter)
  "Return up to LIMIT styled rows from TEXT, followed by an omission row."
  (when-let ((lines (application--display-lines text)))
    (let* ((visible-count (min limit (length lines)))
           (omitted       (- (length lines) visible-count)))
      (append
       (loop for line in (subseq lines 0 visible-count)
             collect (append (when gutter
                               (list (terminal-span ':dim gutter)))
                             (list (terminal-span style line))))
       (when (plusp omitted)
         (list (list (terminal-span
                      ':dim
                      (format nil "… +~D more line~:P" omitted)))))))))

(defvar *application-provider-call-presentation* nil
  "The provider function call currently being rendered as one transcript entry.")

(-> application--function-call-arguments (json-object) (option json-object))
(defun application--function-call-arguments (call)
  "Decode exactly one CALL argument object, returning NIL when it is malformed."
  (let ((source (json-get call "arguments")))
    (when (non-empty-string-p source)
      (handler-case
          (let ((arguments (application--transcript-json source)))
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
  "Return a reader-safe local call key naming JSON property NAME exactly.

Plain keywords are upcased by the reader and lowercased at the local call
boundary. Escape mixed-case names; use strings for names with uppercase
letters but no lowercase letters."
  (cond
    ((notany #'upper-case-p name)
     (format nil ":~A" (application--lisp-symbol-token name)))
    ((some #'lower-case-p name)
     (format nil ":~A" (application--lisp-escaped-symbol-name name)))
    (t
     (prin1-to-string name))))

(-> application--tool-header (application string) string)
(defun application--tool-header (application header)
  "Parenthesize and fit HEADER to APPLICATION's transcript width."
  (application--tool-header-at-columns (application--transcript-columns application) header))

(-> application--tool-header-at-columns ((integer 1) string) string)
(defun application--tool-header-at-columns (columns header)
  "Parenthesize call names, then sanitize and clip HEADER to COLUMNS."
  (let* ((call-header
           (if (and *application-provider-call-presentation*
                    (uiop:string-prefix-p "▸ " header))
               (format nil "▸ (~A)" (subseq header 2))
               header))
         (safe-header   (sanitize-text call-header :single-line-p t))
         (maximum-width (max 1 (1- columns))))
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
  "Indent tool ROW at APPLICATION's transcript width."
  (application--tool-row-spans-at-columns (application--transcript-columns application) row))

(-> application--tool-row-spans-at-columns ((integer 1) list) list)
(defun application--tool-row-spans-at-columns (columns row)
  "Indent tool ROW, retaining source actions through clipping and wrapping."
  (let* ((maximum-width (max 1 (1- columns)))
         (indented      (append (list (terminal-span ':plain "  ")) row))
         (action        (when (and row (every #'termdown:widget-p row))
                          (termdown:widget-action (first row))))
         (fitted        (if (<= (termdown:spans-width indented) maximum-width)
                            indented
                            (append (termdown:fit-spans indented (max 0 (1- maximum-width)))
                                    (list (terminal-span ':dim "…"))))))
    (if (and action
             (every (lambda (span) (equal action (termdown:widget-action span))) row))
        (mapcar (lambda (span)
                  (termdown:make-widget (terminal-span-style span) (terminal-span-text span) action))
                fitted)
        fitted)))

(-> application--tool-argument-row-p (list) boolean)
(defun application--tool-argument-row-p (row)
  "Return whether ROW begins a provider call's keyword/value argument table."
  (and row
       (eq (terminal-span-style (first row)) ':syntax-keyword)))

(-> application--inline-tool-call-header
    (application string list &key (:style terminal-style))
    (values (option list) list))
(defun application--inline-tool-call-header (application header rows &key (style ':tool))
  "Fold call arguments into HEADER at APPLICATION's transcript width."
  (application--inline-tool-call-header-at-columns
   (application--transcript-columns application) header rows :style style))

(-> application--inline-tool-call-header-at-columns
    ((integer 1) string list &key (:style terminal-style))
    (values (option list) list))
(defun application--inline-tool-call-header-at-columns (columns header rows &key (style ':tool))
  "Fold complete leading argument rows into HEADER when they fit one line.

Return the inline header spans and remaining body rows, or NIL and the original
ROWS. Wrapped arguments retain their table. Specialized body rows follow the
header without participating in compaction."
  (let* ((arguments (loop for row in rows
                          while (application--tool-argument-row-p row)
                          collect row))
         (complete-p
           (and arguments
                (every (lambda (row)
                         (and (= (length row) 3)
                              (uiop:string-prefix-p
                               ":" (terminal-span-text (first row)))
                              (string= (terminal-span-text (second row)) "  ")))
                       arguments))))
    (when (and complete-p (uiop:string-prefix-p "▸ " header))
      (let* ((spans
               (append
                (list (terminal-span
                       style
                       (format nil "▸ (~A"
                               (sanitize-text (subseq header 2) :single-line-p t))))
                (loop for row in arguments
                      append (list
                              (terminal-span ':plain " ")
                              (terminal-span
                               ':syntax-keyword
                               (string-right-trim
                                '(#\Space) (terminal-span-text (first row))))
                              (terminal-span ':plain " ")
                              (third row)))
                (list (terminal-span style ")"))))
             (available-width
              (max 1 (1- columns))))
        (when (<= (termdown:spans-width spans) available-width)
          (return-from application--inline-tool-call-header-at-columns
            (values spans (nthcdr (length arguments) rows))))))
    (values nil rows)))

(-> application--tool-entry-wire-p
    (string list &key (:detail (option string)) (:invocation (option string))) boolean)
(defun application--tool-entry-wire-p (header rows &key detail invocation)
  "Check the 8KiB text, 512 span and 64 row caps before retaining extra layout data."
  (let ((characters 8192)
        (spans      512)
        (row-count  64)
        (cells      4096))
    (labels ((charge (value)
               "Charge action data and text without allocating a serialized copy."
               (typecase value
                 (string
                  (decf characters (length value)))
                 (cons
                  (when (minusp (decf cells))
                    (return-from application--tool-entry-wire-p nil))
                  (charge (first value))
                  (charge (rest value)))
                 (symbol
                  (charge (symbol-name value)))
                 ((signed-byte 64)
                  (decf characters 20))
                 (t
                  (return-from application--tool-entry-wire-p nil)))
               (when (minusp characters)
                 (return-from application--tool-entry-wire-p nil))))
      (charge header)
      (charge detail)
      (charge invocation)
      (dolist (row rows)
        (when (minusp (decf row-count))
          (return-from application--tool-entry-wire-p nil))
        (dolist (span row)
          (when (minusp (decf spans))
            (return-from application--tool-entry-wire-p nil))
          (charge (terminal-span-text span))
          (when (termdown:widget-p span)
            (charge (termdown:widget-action span)))))
      t)))

(-> application--tool-entry-from-description (list (integer 1)) list)
(defun application--tool-entry-from-description (description columns)
  "Fit bounded pre-clipping tool data to the client's current COLUMNS."
  (let ((*application-provider-call-presentation* (getf description :provider-call-p))
        (*application-tool-call-invocation*       (getf description :invocation))
        (*application-tool-input-expanded-p*      nil))
    (multiple-value-bind (inline-header body-rows)
        (if (getf description :inline-p)
            (application--inline-tool-call-header-at-columns
             columns (getf description :header) (getf description :rows) :style (getf description :style))
            (values nil (getf description :rows)))
      (append
       (application--tool-call-header-actions
        (or inline-header
            (application--transcript-header
             (application--tool-header-at-columns columns (getf description :header))
             (getf description :detail) columns :style (getf description :style))))
       (loop for row in body-rows
             append (list (terminal-span ':plain (string #\Newline)))
             when row
               append (application--tool-row-spans-at-columns columns row))))))

(-> application--tool-entry
    (application &key (:style terminal-style) (:header string)
                 (:detail (option string)) (:rows list))
    list)
(defun application--tool-entry
    (application &key (style ':plain) (header "") detail rows)
  "Return a tool entry, inlining fitting call arguments above any body rows."
  (unless (eq *application-tool-entry-description* ':inactive)
    (let ((display-detail (or detail (application--turn-timestamp-text application nil))))
      (setf *application-tool-entry-description*
            (when (application--tool-entry-wire-p
                   header rows :detail display-detail :invocation *application-tool-call-invocation*)
              (list :version 1 :style style :header header :detail display-detail :rows rows
                    :provider-call-p (not (null *application-provider-call-presentation*))
                    :inline-p        (and (not (null *application-provider-call-presentation*)) (null detail))
                    :invocation      *application-tool-call-invocation*)))))
  (multiple-value-bind (inline-header body-rows)
      (if (and *application-provider-call-presentation* (null detail))
          (application--inline-tool-call-header application header rows :style style)
          (values nil rows))
    (append
     (application--tool-call-header-actions
      (or inline-header
          (application--transcript-entry
           application
           :style  style
           :header (application--tool-header application header)
           :detail detail)))
     (loop for row in body-rows
           append (list (terminal-span ':plain (string #\Newline)))
           when row
             append (application--tool-row-spans application row)))))

(-> application--tool-call-header-actions (list) list)
(defun application--tool-call-header-actions (spans)
  "Make header SPANS insert the rendered call's local Lisp invocation when clicked.

Without a bound *APPLICATION-TOOL-CALL-INVOCATION* the spans are unchanged."
  (if *application-tool-call-invocation*
      (let ((action (list ':insert *application-tool-call-invocation*)))
        (mapcar (lambda (span)
                  (termdown:make-widget (terminal-span-style span)
                                        (terminal-span-text span)
                                        action))
                spans))
      spans))

(-> application--tool-field-rows (application list) list)
(defun application--tool-field-rows (application fields)
  "Return aligned FIELDS at APPLICATION's transcript width."
  (application--tool-field-rows-at-columns (application--transcript-columns application) fields))

(-> application--tool-field-rows-at-columns ((integer 1) list) list)
(defun application--tool-field-rows-at-columns (columns fields)
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
                 (- columns 3)))
         (column-widths
           (termdown:column-widths
            (loop for field in safe-fields
                  collect (list (getf field :label) (getf field :value)))
            available-width
            :gap-width      2
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
                          (termdown:fit-text
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
                                              :depth  (1- depth))
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
                (omitted       (- (length value) visible-count)))
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
  (let* ((priority   '("truncated" "server" "id" "name" "state" "status" "severity" "error"
                       "objective" "message" "summary" "evidence" "gates" "criteria"))
         (left-rank  (or (position left priority :test #'string=) 100))
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
         (omitted       (- (length keys) visible-count)))
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
  "Flatten bounded ARGUMENTS into aligned fields, quoting string literals."
  (labels ((field (label value &key (literal-p t))
             "Return one bounded scalar or summary field."
             (let ((text (bounded-string (application--presentation-value value) :limit 2000)))
               (list (list :label label
                           :value (if (and literal-p (stringp value))
                                      (prin1-to-string text)
                                      text)
                           :style (if (string= label "restart")
                                      ':notice
                                      (application--structured-value-style label value))))))

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
                                               *application-tool-structure-items*))
                               :literal-p nil)))
                    (field label value)))
               (t
                (field label value))))

           (expand-object (object prefix depth)
             "Expand OBJECT's fields in stable presentation order."
             (let* ((keys  (sort (loop for key being the hash-keys of object
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
                         (format nil "… +~D more fields" (- (length keys) count))
                         :literal-p nil))))))
    (expand-object arguments nil *application-tool-structure-depth*)))

(-> application--generic-argument-rows
    (application (option json-object) &key (:excluded-keys list))
    list)
(defun application--generic-argument-rows (application arguments &key excluded-keys)
  "Return aligned argument fields followed immediately by multiline previews."
  (application--argument-field-rows
   (application--transcript-columns application)
   (when arguments (application--argument-fields arguments :excluded-keys excluded-keys))))

(-> application--argument-field-rows ((integer 1) list) list)
(defun application--argument-field-rows (columns fields)
  "Lay out decoded argument FIELDS at COLUMNS, preserving multiline previews."
  (let ((multiline (remove-if-not
                    (lambda (field) (find #\Newline (getf field :value))) fields)))
    (append
     (application--tool-field-rows-at-columns
      columns (remove-if (lambda (field) (member field multiline)) fields))
     (loop for field in multiline
           append (cons
                   (application--tool-section-row
                    (application--lisp-key-token
                     (sanitize-text (getf field :label) :single-line-p t)))
                   (application--preview-rows
                    (getf field :value) (getf field :style)
                    *application-tool-call-lines* :gutter "│ "))))))

(-> application--restart-value-rows ((option json-object)) list)
(defun application--restart-value-rows (arguments)
  "Return a source rail for an explicitly supplied restart value."
  (let ((value (and arguments (json-get arguments "restart-value"))))
    (when (non-empty-string-p value)
      (cons (application--tool-section-row ":restart-value")
            (application--source-preview-rows
             value
             :language              *application-common-lisp-language*
             :rainbow-parentheses-p t
             :limit                 *application-tool-call-lines*)))))

(-> application--generic-tool-call-entry (application json-object) list)
(defun application--generic-tool-call-entry (application call)
  "Return a readable fallback entry for a function CALL."
  (application--tool-entry
   application
   :style  ':tool
   :header (format nil "▸ ~A" (or *application-tool-presentation-label*
                                  (function-call-canonical-name call)))
   :rows   (application--generic-argument-rows
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
    ((tool t) (application application) (call hash-table))
  "Present CALL using the generic readable argument layout."
  (declare (ignore tool))
  (application--generic-tool-call-entry application call))

(defmethod application-tool-call-entry
    ((tool papercut-report-tool) (application application) (call hash-table))
  "Present papercut.report's title without duplicating its complete body."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (title     (and arguments (json-get arguments "title"))))
    (application--tool-entry
     application
     :style  ':tool
     :header "▸ papercut.report"
     :rows   (when title
               (application--tool-field-rows
                application (list (list :label "title" :value (prin1-to-string title) :style ':strong)))))))


;;; Tool call specializations

(-> application--lisp-input-description (application json-object string) list)
(defun application--lisp-input-description (application call argument-name)
  "Retain CALL's decoded display data without rendering its expanded source."
  (application--transcript-source-value
   (list ':source-input call argument-name *application-tool-call-invocation*)
   (lambda ()
     (let* ((arguments     (application--function-call-arguments call))
            (value         (and arguments (json-get arguments argument-name)))
            (restart-value (and arguments (json-get arguments "restart-value"))))
       (list :version 1 :header (format nil "▸ ~A" (function-call-canonical-name call))
             :sources       (if (and (vectorp value) (not (stringp value)) (every #'stringp value))
                                (coerce value 'list)
                                (list (application--presentation-value (or value ""))))
             :fields        (when arguments
                              (application--argument-fields arguments :excluded-keys (list argument-name "restart-value")))
             :restart-value (and (stringp restart-value) restart-value)
             :input-key     (when (terminal-ui-fullscreen-p (application-ui application))
                              (application--tool-outcome-key application ':call (json-get call "call_id")))
             :invocation    *application-tool-call-invocation*
             :detail        (application--turn-timestamp-text application nil)
             :line-limit    *application-tool-call-lines*
             :lisp-columns  *application-lisp-input-columns*)))))

(-> application--wrap-source-row ((integer 1) list) list)
(defun application--wrap-source-row (columns row)
  "Fold a highlighted source ROW behind its rail, at grapheme boundaries."
  (let* ((rail  (first row))
         (spans (rest row))
         (width (- columns 1 2 (termdown:spans-width (list rail)))))
    (unless (and (plusp width) (> (termdown:spans-width spans) width))
      (return-from application--wrap-source-row (list row)))
    (let ((text        (format nil "~{~A~}" (mapcar #'terminal-span-text spans)))
          (span-offset 0)
          (consumed    0)
          (used        0)
          (rows        nil))
      (flet ((take-through (end)
               "Consume the next styled range up to the character offset END."
               (loop while (< consumed end)
                     for span = (first spans)
                     for label = (terminal-span-text span)
                     for count = (min (- end consumed) (- (length label) span-offset))
                     when (plusp count)
                       collect (terminal-span (terminal-span-style span)
                                              (subseq label span-offset (+ span-offset count)))
                     do (incf consumed count)
                        (incf span-offset count)
                        (when (= span-offset (length label))
                          (pop spans)
                          (setf span-offset 0)))))
        (loop for start = 0 then end
              while (< start (length text))
              for end = (clinedi:grapheme-next-boundary text start)
              for cells = (clinedi:grapheme-cell-width text start end)
              do (when (> (+ used cells) width)
                   (when (< consumed start)
                     (push (cons rail (take-through start)) rows))
                   (setf used 0))
                 (cond
                   ((> cells width)
                    (take-through end)
                    (push (list rail (terminal-span ':dim "…")) rows))
                   (t
                    (incf used cells))))
        (when (< consumed (length text))
          (push (cons rail (take-through (length text))) rows)))
      (nreverse rows))))

(-> application--source-input-entry (list (integer 1) &key (:expanded-p boolean)) list)
(defun application--source-input-entry (description columns &key expanded-p)
  "Lay out source display DESCRIPTION locally; its strings are never read or evaluated."
  (let* ((*application-provider-call-presentation* t)
         (*application-tool-call-invocation*       (getf description :invocation))
         (*application-tool-call-lines*            (getf description :line-limit))
         (header                                   (getf description :header))
         (input-key                                (getf description :input-key))
         (source
           (format nil "~{~A~^~%~}"
                   (loop for text in (getf description :sources)
                         collect
                         (if (and expanded-p
                                  (not (find-if (lambda (character) (find character '(#\Newline #\Return))) text)))
                             (let ((width (min (getf description :lisp-columns) (max 1 (- columns 5)))))
                               (application--transcript-source-value
                                (list ':formatted-lisp text width)
                                (lambda () (clinedi:lisp-source-layout text width))))
                             text))))
         (source-rows
           (application--source-preview-rows
            source :language *application-common-lisp-language* :rainbow-parentheses-p t
            :limit (if expanded-p (1+ (count #\Newline source)) *application-tool-call-lines*)))
         (display-rows                             (if expanded-p
                                                       (loop for row in source-rows append (application--wrap-source-row columns row))
                                                       source-rows))
         (rows
           (append
            (application--argument-field-rows columns (getf description :fields))
            (if input-key
                 (loop for row in display-rows
                      collect (loop for span in row
                                    collect (termdown:make-widget
                                             (terminal-span-style span) (terminal-span-text span)
                                             (list ':toggle-tool-input input-key))))
                 display-rows)
            (let ((restart-rows (application--restart-value-rows (json-object "restart-value" (getf description :restart-value)))))
              (if (and expanded-p restart-rows)
                  (cons (first restart-rows)
                        (loop for row in (rest restart-rows) append (application--wrap-source-row columns row)))
                  restart-rows)))))
    (multiple-value-bind (inline-header body-rows)
        (application--inline-tool-call-header-at-columns columns header rows)
      (append
       (application--tool-call-header-actions
        (or inline-header
            (application--transcript-header
             (application--tool-header-at-columns columns header) (getf description :detail) columns :style ':tool)))
       (loop for row in body-rows
             append (list (terminal-span ':plain (string #\Newline)))
             when row
               append (application--tool-row-spans-at-columns columns row))))))

(-> application--lisp-call-entry (application json-object string) list)
(defun application--lisp-call-entry (application call argument-name)
  "Return syntax-highlighted Lisp input that a click expands in the fullscreen transcript."
  (application--source-input-entry
   (application--lisp-input-description application call argument-name)
   (application--transcript-columns application) :expanded-p *application-tool-input-expanded-p*))

;; Codex PlanUpdateCell reference: 5c19155cbd93bfa099016e7487259f61669823ff.
(-> application--plan-update-text (t) (option string))
(defun application--plan-update-text (value)
  "Return VALUE as sanitized non-empty single-line plan text."
  (when (stringp value)
    (let ((text
            (sanitize-text
             (string-trim *line-whitespace* value)
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
           (application--transcript-columns application))
         (prefix-width
           (max (termdown:spans-width initial-prefix)
                (termdown:spans-width continuation-prefix)))
         (text-width (max 1 (- columns 3 prefix-width)))
         (lines      (or (wrap-text text text-width) (list ""))))
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
         (status              (plan--status (json-get step "status")))
         (valid-p             (and text status))
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
     :style               style
     :initial-prefix      initial-prefix
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
           (steps     (json-get arguments "steps"))
           (rows      nil)
           (content-p nil))
      (when explanation
        (setf rows
              (application--plan-update-wrapped-rows
               application explanation
               :style               ':hint
               :initial-prefix      (list (terminal-span ':dim "└ "))
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
     :style  ':tool
     :header "▸ plan.update"
     :rows   (append
              (list (list (terminal-span ':dim "• ")
                          (terminal-span ':strong "updated plan")))
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

(-> application--task-run-summary-text (string) string)
(defun application--task-run-summary-text (text)
  "Return TEXT as one sanitized whitespace-normalized summary line."
  (serapeum:trim-whitespace
   (serapeum:collapse-whitespace (sanitize-text text :single-line-p t))))

(-> application--fit-ellipsized (string integer) string)
(defun application--fit-ellipsized (text width)
  "Clip TEXT to WIDTH terminal cells with an ellipsis, then pad it."
  (let* ((width (max 0 width))
         (safe  (sanitize-text text :single-line-p t))
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
    (termdown:fit-text visible width)))

(-> application--task-run-compact-context-row
    (application string)
    list)
(defun application--task-run-compact-context-row (application context)
  "Return one indented compact preview row for item-specific CONTEXT."
  (let* ((prefix "    context  ")
         (columns
           (application--transcript-columns application))
         (width  (max 0 (- columns 3 (text-cell-width prefix)))))
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
                  (getf item :agent)
                  (case (getf item :mode)
                    (:detached
                     "detached")
                    (otherwise
                     ""))
                  (application--task-run-summary-text
                   (getf item :task)))))
         (columns
           (application--transcript-columns application))
         (widths
           (termdown:column-widths
            values
            (max 0 (- columns 3))
            :gap-width      2
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

(-> application--task-run-rows
    (application json-object)
    list)
(defun application--task-run-rows (application arguments)
  "Lay out the same normalized descriptions used to execute child.spawn."
  (handler-case
      (application--task-run-compact-item-rows application (task-normalize-arguments arguments))
    (tool-error (condition)
      (list (application--tool-section-row
             (format nil "invalid task request: ~A" condition))))))

(defmethod application-tool-call-entry
    ((tool task-run-tool) (application application) (call hash-table))
  "Present child.spawn children as structured assignment previews."
  (let ((arguments (application--task-run-arguments tool call)))
    (application--tool-entry
     application
     :style  ':tool
     :header "▸ child.spawn"
     :rows   (if arguments
                 (application--task-run-rows application arguments)
                 (list
                  (application--tool-section-row
                   "arguments unavailable"))))))

(-> application--resource-failure-spans (string) list)
(defun application--resource-failure-spans (text)
  "Color failure TEXT red, preserving neutral numbers and revision identifiers."
  (let ((spans nil)
        (start 0)
        (index 0)
        (size  (length text)))
    (loop while (< index size)
          for end = (cond
                      ((digit-char-p (char text index))
                       (or (position-if-not #'digit-char-p text :start index) size))
                      ((and (char= (char text index) #\R)
                            (<= (+ index 17) size)
                            (every (lambda (character) (digit-char-p character 16))
                                   (subseq text (1+ index) (+ index 17))))
                       (+ index 17)))
          do (if end
                 (progn
                   (when (< start index)
                     (push (terminal-span ':failure (subseq text start index)) spans))
                   (push (terminal-span ':plain (subseq text index end)) spans)
                   (setf start end index end))
                 (incf index)))
    (when (< start size)
      (push (terminal-span ':failure (subseq text start)) spans))
    (nreverse spans)))

(-> application--resource-edit-failure-entry (list keyword list) list)
(defun application--resource-edit-failure-entry (entry state body)
  "Insert the failed outcome beneath each operation heading, ahead of its diff."
  (let ((found-p nil))
    (let ((updated
            (loop for span in entry
                  for text = (terminal-span-text span)
                  append (if (and (eq (terminal-span-style span) ':dim)
                                  (uiop:string-prefix-p "operation " text))
                             (progn
                               (setf found-p t)
                               (append (application--resource-failure-spans text) body))
                             (list span)))))
      (terminal-ui-fullscreen--record-entry updated state (unless found-p body)))))

(-> application--resource-operation-heading (json-object integer) string)
(defun application--resource-operation-heading (operation index)
  "Return a readable verb phrase for resource edit OPERATION at INDEX."
  (let ((name       (json-get operation "op"))
        (start      (json-get operation "start-line"))
        (end        (json-get operation "end-line"))
        (anchor     (json-get operation "line"))
        (identifier (json-get operation "id")))
    (flet ((lines (verb)
             "Return VERB with the operation's line range when it names one."
             (if (and (integerp start) (integerp end))
                 (format nil "~A lines ~D-~D" verb start end)
                 (format nil "~A lines" verb)))

           (anchored (phrase)
             "Return PHRASE with the operation's anchor line when it names one."
             (format nil "~A line~@[ ~D~]" phrase (and (integerp anchor) anchor)))

           (item (phrase)
             "Return PHRASE with the operation's item identifier when it names one."
             (format nil "~A~@[ ~A~]" phrase (and (non-empty-string-p identifier) identifier))))
      (format nil "operation ~D · ~A"
              index
              (case (resource-edit-kind name)
                (:replace-lines
                 (lines "replace"))
                (:delete-lines
                 (lines "delete"))
                (:insert-before
                 (anchored "insert before"))
                (:insert-after
                 (anchored "insert after"))
                (:replace-empty
                 "replace empty file")
                (:agenda-add
                 "add agenda item")
                (:agenda-update
                 (item "update agenda item"))
                (:agenda-remove
                 (item "remove agenda item"))
                (:memory-remember
                 "remember memory")
                (:memory-replace
                 "replace memory")
                (:memory-forget
                 "forget memory")
                (:papercut-close
                 "close papercut")
                (otherwise
                 (if (non-empty-string-p name)
                     name
                     "invalid operation")))))))

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
  "Return observed workspace lines for URI and REVISION, or NIL.

Use only the exact transient observation and ranges already shown to the model.
Never read the current file as a revision-gated preimage."
  (when (and (plusp start-line) (<= start-line end-line))
    (nth-value 0
      (application--call-with-observation-state
       application uri revision
       :state-class       'text-resource-observation-state
       :observation-class 'text-resource-observation
       :function          (lambda (state observation)
                            (let ((lines (text-resource-observation-lines observation)))
                              (when (and (<= end-line (length lines))
                                         (structlisp:integer-interval-set-covers-p
                                          (text-resource-observation-state-visible-ranges state)
                                          start-line (1+ end-line)))
                                (let ((selected-lines
                                        (loop for line from start-line to end-line
                                              collect (aref lines (1- line)))))
                                  (format nil "~{~A~^~%~}~:[~;~%~]"
                                          selected-lines
                                          (zerop (length (lastcar selected-lines))))))))))))

(-> application--workspace-resource-operation-set-valid-p
    (application vector &key (:uri string) (:revision string))
    boolean)
(defun application--workspace-resource-operation-set-valid-p
    (application operations &key uri revision)
  "Return whether OPERATIONS are valid against the exact observed workspace file."
  (when (plusp (length operations))
    (nth-value 0
      (application--call-with-observation-state
       application uri revision
       :state-class       'text-resource-observation-state
       :observation-class 'text-resource-observation
       :function          (lambda (state observation)
                            (handler-case
                                (progn
                                  (cl-hashline:edit-text
                                   (resource-observation-content observation)
                                   (map 'list #'text-resource--json-operation operations)
                                   :visible-lines         (text-resource-observation-state-visible-ranges state)
                                   :split-lines           #'text--split-lines
                                   :anchor-maximum-offset *workspace-file-resource-anchor-maximum-offset*)
                                  t)
                              (error ()
                                nil)))))))

(-> application--workspace-resource-added-start-lines
    (application vector &key (:uri string) (:revision string))
    vector)
(defun application--workspace-resource-added-start-lines
    (application operations &key uri revision)
  "Return exact resulting first-line coordinates for valid added content."
  (let ((starts      (make-array (length operations) :initial-element nil))
        (coordinates nil))
    (block nil
      (unless (application--workspace-resource-operation-set-valid-p
               application operations :uri uri :revision revision)
        (return starts))
      (loop for operation across operations
            for index from 0
            do
               (let* ((kind    (resource-edit-kind (json-get operation "op")))
                      (start   (json-get operation "start-line"))
                      (end     (json-get operation "end-line"))
                      (line    (json-get operation "line"))
                      (content (json-get operation "content"))
                      (added-count
                        (and (stringp content)
                             (length (text--split-lines content)))))
                 (case kind
                   (:replace-lines
                    (push (list index
                                start
                                start
                                (- added-count (1+ (- end start))))
                          coordinates))
                   (:delete-lines
                    (push (list index
                                start
                                nil
                                (- (1+ (- end start))))
                          coordinates))
                   ((:insert-before :insert-after)
                    (push (list index
                                line
                                (+ line
                                   (if (eq kind ':insert-after) 1 0))
                                added-count)
                          coordinates))
                   (:replace-empty
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
  (let* ((kind    (resource-edit-kind (json-get operation "op")))
         (start   (json-get operation "start-line"))
         (end     (json-get operation "end-line"))
         (line    (json-get operation "line"))
         (content (json-get operation "content"))
         (new-start-line
           (or new-start-line
               (cond
                 ((and (eq kind ':insert-after)
                       (integerp line))
                  (1+ line))
                 ((integerp line)
                  line)
                 ((integerp start)
                  start)
                 (t
                  nil)))))
    (cond
      ((and (member kind '(:replace-lines :delete-lines))
            (integerp start)
            (integerp end)
            (plusp start)
            (<= start end)
            (or (eq kind ':delete-lines) (stringp content)))
       (let* ((old-text
                (application--workspace-resource-observed-text
                 application uri revision :start-line start :end-line end))
              (new-text (if (eq kind ':replace-lines) content "")))
         (if old-text
             (change-viewer-render
              :removed-content    old-text
              :added-content      new-text
              :removed-start-line start
              :added-start-line   new-start-line
              :source-path        path)
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
                (when (eq kind ':replace-lines)
                  (change-viewer-render
                   :added-content    new-text
                   :added-start-line new-start-line
                   :source-path      path)))))))
      ((and (member kind '(:insert-before :insert-after :replace-empty))
            (stringp content))
       (change-viewer-render
        :added-content    content
        :added-start-line new-start-line
        :source-path      path))
      (t
       nil))))

(-> application--workspace-resource-operation-rows
    (application t &key (:uri string) (:revision string) (:path string))
    list)
(defun application--workspace-resource-operation-rows
    (application operations &key uri revision path)
  "Return syntax-highlighted line changes for workspace resource OPERATIONS."
  (let ((new-start-lines
          (and (vectorp operations)
               (not (stringp operations))
               (plusp (length operations))
               (application--workspace-resource-added-start-lines
                application operations :uri uri :revision revision))))
    (application--resource-operation-rows
     application operations
     :change-rows-function
     (lambda (operation index)
       (application--workspace-resource-operation-change-rows
        application operation
        :uri            uri
        :revision       revision
        :path           path
        :new-start-line (aref new-start-lines index))))))

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
            (omitted       (- (length operations) visible-count)))
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
                                (funcall change-rows-function operation index))))
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
    ((tool shell-run-tool) (application application) (call hash-table))
  "Present shell.run arguments immediately above the shell source rail."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (command   (or (and arguments (json-get arguments "command")) "")))
    (application--tool-entry
     application
     :style  ':tool
     :header "▸ shell.run"
     :rows   (append
              (application--generic-argument-rows
               application arguments :excluded-keys '("command"))
              (application--source-preview-rows command
                                                :language *application-shell-language*
                                                :prompt   "$ ")))))

(defmethod application-tool-call-entry
    ((tool lisp-source-input) (application application) (call hash-table))
  "Present the call's Lisp source as bounded, highlighted source and its other arguments above it."
  (application--lisp-call-entry application call (tool-source-argument tool)))

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
     :style  ':tool
     :header "▸ self.set"
     :rows   (append
              (application--generic-argument-rows
               application arguments :excluded-keys '("value" "restart-value"))
              (application--source-preview-rows
               value
               :language              *application-common-lisp-language*
               :rainbow-parentheses-p t)
              (application--restart-value-rows arguments)))))


;;; Tool result layout

(-> application--tool-result-success-p (list) boolean)
(defun application--tool-result-success-p (record)
  "Return true when tool result RECORD has successful status."
  (eq (getf (rest record) :status) ':ok))

(-> application--tool-result-timing (list) (option string))
(defun application--tool-result-timing (record)
  "Return RECORD's CPU and real duration as a concise detail string."
  (let ((cpu  (getf (rest record) :cpu-microseconds))
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
         (timing    (application--tool-result-timing record))
         (complete-detail
           (cond
             ((and detail timing)
              (format nil "~A · ~A" detail timing))
             (detail
              detail)
             (timing
              timing))))
    (application--tool-entry
     application
     :style  (if success-p ':success ':failure)
     :header (format nil "~:[! ~A failed~;✓ ~A~]" success-p tool-name)
     :detail complete-detail
     :rows   rows)))

(-> application--section-preview-rows
    (string string terminal-style &key (:limit integer)
                                      (:language (option language))
                                      (:rainbow-parentheses-p boolean))
    list)
(defun application--section-preview-rows
    (label text style &key (limit *application-tool-output-lines*) language
                           rainbow-parentheses-p)
  "Return a labeled, bounded transcript section, highlighting declared LANGUAGE."
  (append
   (list (application--tool-section-row label))
   (cond
     ((not (non-empty-string-p text))
      (list (list (terminal-span ':dim "│ (none)"))))
     (language
       (application--source-preview-rows
        text :language              language
             :limit                 limit
             :rainbow-parentheses-p rainbow-parentheses-p))
     (t
      (application--preview-rows text style limit :gutter "│ ")))))

(-> application--evaluation-result-rows (list) list)
(defun application--evaluation-result-rows (record)
  "Render the bounded evaluation sections, without interpreting the model text."
  (let* ((details        (getf (rest record) :details))
         (sections       (and (proper-list-p details) (evenp (length details))
                              (getf details :evaluation)))
         (condition-text (getf sections :condition))
         (output         (getf sections :output))
         (backtrace      (getf sections :backtrace))
         (restarts       (getf sections :restarts))
         (retry          (getf sections :retry)))
    (when sections
      (append
       (when condition-text
         (application--section-preview-rows "condition" condition-text ':plain))
       (when (non-empty-string-p output)
         (append (when condition-text (list nil))
                 (application--section-preview-rows
                  (if (equal (getf (rest record) :tool) "lisp.describe") "description" "output")
                  output ':plain)))
       (when restarts
         (append (list nil (application--tool-section-row "available restarts"))
                 (loop for (name report) in restarts
                       repeat *application-tool-output-lines*
                       collect (list (terminal-span ':notice (format nil "~18A " name))
                                     (terminal-span ':plain report)))))
       (when retry
         (append (list nil)
                 (application--section-preview-rows "retry" retry ':hint)))
       (if condition-text
           (when (non-empty-string-p backtrace)
             (append (list nil)
                     (application--section-preview-rows "backtrace" backtrace ':dim)))
           (append (when (non-empty-string-p output) (list nil))
                   (application--section-preview-rows
                    "values" (getf sections :values) ':code
                    :language              *application-common-lisp-language*
                    :rainbow-parentheses-p t)))))))

(-> application--failure-result-rows
    (string &key (:tool-name (option string)))
    list)
(defun application--failure-result-rows (output &key tool-name)
  "Render plain failure text, omitting TOOL-NAME's redundant failure prefix."
  (let ((prefix (and tool-name (format nil "~A failed: " tool-name))))
    (application--preview-rows
     (if (and prefix (uiop:string-prefix-p prefix output))
         (subseq output (length prefix))
         output)
     ':plain *application-tool-output-lines* :gutter "│ ")))

(-> application--tool-text-rows (string) list)
(defun application--tool-text-rows (output)
  "Render bounded OUTPUT as literal text, without inferring a data format."
  (application--preview-rows output ':dim *application-tool-output-lines* :gutter "│ "))

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
  (let* ((output    (or (getf (rest record) :output) ""))
         (details   (getf (rest record) :details))
         (sections  (application--mcp-content-sections details output))
         (rows      nil)
         (processed 0))
    (dolist (section sections)
      (when (>= (length rows) *application-tool-output-lines*)
        (return))
      (incf processed)
      (destructuring-bind (kind start end) section
        (setf rows
              (append rows
                      (ecase kind
                        (:text
                         (application--tool-text-rows (subseq output start end)))
                        (:structured
                         (application--mcp-structured-rows application details)))))))
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
                   ((eq value ':mcp-false)
                    (json-false))
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
                   ((and (consp value) (eq (first value) ':mcp-object))
                    (let ((object (make-hash-table :test #'equal)))
                      (loop for tail on (rest value) for field = (first tail) repeat 13
                            do (unless (and (consp field) (stringp (first field))
                                            (consp (rest field)) (null (rest (rest field))))
                                 (return-from invalid (values nil nil)))
                               (setf (gethash (first field) object) (project (second field) (1- depth))))
                      object))
                   ((and (consp value) (eq (first value) ':mcp-array))
                    (coerce (loop for tail on (rest value) repeat 13
                                  collect (project (first tail) (1- depth)))
                            'vector))
                   ((consp value)
                    (if (keywordp (first value))
                        (let ((object (make-hash-table :test #'equal))
                              (tail   value))
                          (loop repeat 64
                                while tail
                                do (unless (and (consp tail) (keywordp (first tail))
                                                (consp (rest tail)))
                                     (return-from invalid (values nil nil)))
                                   (setf (gethash (string-downcase (symbol-name (first tail))) object)
                                         (project (second tail) (1- depth))
                                         tail (nthcdr 2 tail)))
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
                                     :job-cancel :job-send :mission :mcp-content))))
            (when (or tagged-p
                      (member (first details)
                              '(:job :continuity-list :classification :job-id :execution-id)))
              (let ((value (project (if tagged-p (rest details) details) 8)))
                (if (json-object-p value)
                    (values value t)
                    (values nil nil))))))))))

(-> application--mcp-structured-rows (application list) list)
(defun application--mcp-structured-rows (application details)
  "Render an MCP structured section from the native portable value only."
  (multiple-value-bind (value valid-p)
      (application--native-details-value
       (list :mcp-content :structured-content (getf (rest details) :structured-content)))
    (let ((content                             (and valid-p (json-get value "structured-content")))
          (*application-tool-structure-budget* 64))
      (cond
        ((json-object-p content)
         (or (application--structured-object-rows application content :depth 8)
             (list (list (terminal-span ':dim "empty structured content")))))
        (content
         (application--structured-value-rows application "result" content :depth 8))
        (t
         (list (list (terminal-span ':dim "structured content unavailable"))))))))

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
         (shell-p      (equal role "shell.run"))
         (summary      (unless shell-p
                         (or (and (json-object-p result)
                                  (or (json-get result "summary") (json-get result "label")
                                      (json-get result "output") (json-get result "content")))
                             (json-get object "summary") (json-get object "description"))))
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
                          :style style)))
     (when shell-p
       (application--native-shell-result-rows application result)))))

(-> application--native-details-rows (application list) list)
(defun application--native-details-rows (application record)
  "Render retained portable native details with bounded semantic fields."
  (multiple-value-bind (value valid-p)
      (let ((details (getf (rest record) :details)))
        (unless (and (consp details) (eq (first details) ':mcp-content))
          (application--native-details-value details)))
    (when valid-p
      (let* ((job      (json-get value "job"))
             (items    (or (json-get value "results") (json-get value "jobs")
                           (json-get value "entries")))
             (rows     (cond
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
             (paging   (when (json-get value "next-offset")
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

(-> application--structured-result-output-p (list) boolean)
(defun application--structured-result-output-p (record)
  "Identify producer-owned envelopes without inspecting model-facing output."
  (let* ((properties (rest record))
         (details    (getf properties :details)))
    (or (and (consp details) (eq (first details) ':tool-execution))
        (and (application--tool-result-success-p record)
             (not (null (member (getf properties :tool)
                                '("shell.run" "task.run" "task.agents" "job.list"
                                  "job.get" "job.wait" "job.cancel" "job.send" "job.continuity")
                                :test #'equal)))))))

(-> application--generic-tool-result-entry (application list) list)
(defun application--generic-tool-result-entry (application record)
  "Return a readable fallback entry using native details or bounded literal text."
  (let ((output (or (getf (rest record) :output) "")))
    (application--tool-result-entry
     application
     record
     :rows (or (application--mcp-content-rows application record)
               (application--native-details-rows application record)
               (application--evaluation-result-rows record)
               (cond
                 ((application--structured-result-output-p record)
                  (list (list (terminal-span ':dim "result details unavailable"))))
                 ((application--tool-result-success-p record)
                  (application--tool-text-rows output))
                 (t
                  (application--failure-result-rows
                   output :tool-name (getf (rest record) :tool))))))))

(-> application--tool-execution-handoff-status (list) string)
(defun application--tool-execution-handoff-status (handoff)
  "Return HANDOFF's job state in words, with its job.cancel call while the job can be cancelled."
  (let* ((job   (getf (rest handoff) :job))
         (state (getf job :state)))
    (format nil "~A · ~A"
            (case state
              (:queued
               "queued in background")
              (:running
               (if (eq (getf (rest handoff) :handoff-reason) ':grace-expired)
                   "still running in background"
                   "running in background"))
              (:completed
               "result ready")
              (:failed
               "failed")
              ((:cancelled :aborted)
               "cancelled")
              (otherwise
               "background handoff"))
            (if (member state '(:queued :running))
                (terminal-ui--job-cancel-text (getf job :id))
                (getf job :id)))))

(-> application--tool-execution-handoff-annotation (list) list)
(defun application--tool-execution-handoff-annotation (handoff)
  "Return HANDOFF's job state as spans that follow its call's closing parenthesis."
  (terminal-ui--job-cancel-widget
   (list (terminal-span ':annotation
                        (format nil "  ~A" (application--tool-execution-handoff-status handoff))))
   (getf (getf (rest handoff) :job) :id)))

(-> application--tool-execution-handoff-entry (application string list) list)
(defun application--tool-execution-handoff-entry (application tool-name handoff)
  "Present TOOL-NAME's background HANDOFF as job state and identity, not as serialized output.

A job that is still queued or running offers its job.cancel call. The agent's
tool result, the user's own tool call and the user's background evaluation all
show this entry outside the fullscreen transcript, which notes the state on
the call itself."
  (let ((job (getf (rest handoff) :job)))
    (terminal-ui--job-cancel-widget
     (application--tool-entry
      application
      :style  (if (eq (getf job :state) ':failed) ':failure ':notice)
      :header (format nil "↪ ~A" (application--tool-display-name application tool-name))
      :detail (application--tool-execution-handoff-status handoff))
     (getf job :id))))

(defmethod application-tool-result-entry :around
    (tool (application application) record)
  "Present native background handoffs before any tool-specific output renderer."
  (declare (ignore tool))
  (let ((handoff (application--tool-execution-handoff record))
        (name    (getf (rest record) :tool)))
    (cond
      (handoff
       (application--tool-execution-handoff-entry application name handoff))
      ((equal name "shell.run")
       (or (application--shell-result-entry application record) (call-next-method)))
      (t
       (call-next-method)))))

(defmethod application-tool-result-entry
    ((tool t) (application application) record)
  "Present RECORD using the generic readable result layout."
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
  "Show successful papercut reports as prominent persistent alerts.

Mark reports that repeat an active papercut as already reported."
  (if (application--tool-result-success-p record)
      (let* ((output     (or (getf (rest record) :output) ""))
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
                             (format nil "read full: ~A"
                                     (papercut-call-source papercut))))))
                   (application--preview-rows
                    output
                    ':plain
                    *application-tool-output-lines*
                    :gutter "│ "))))
        (application--tool-entry
         application
         :style  (if (application--papercut-result-duplicate-p output)
                     ':notice
                     ':failure)
         :header (if (application--papercut-result-duplicate-p output)
                     "! PAPERCUT ALREADY REPORTED"
                     "! PAPERCUT RECORDED")
         :detail (and papercut
                      (format nil "id ~A"
                              (papercut-short-identifier papercut)))
         :rows   rows))
      (call-next-method)))

(-> application--shell-section-rows (application list boolean) list)
(defun application--shell-section-rows (application section labeled-p)
  "Return SECTION's bounded stream samples without interpreting output prose."
  (declare (ignore application))
  (let* ((stream   (getf section :stream))
         (stderr-p (eq stream ':stderr))
         (text     (concatenate 'string (or (getf section :head) "")
                                (if (getf section :omitted-p) (format nil "~%…~%") "")
                                (or (getf section :tail) ""))))
    (append
     (when labeled-p
       (list (application--tool-section-row
              (ecase stream
                (:stdout
                 "stdout")
                (:stderr
                 "stderr")
                (:combined
                 "output")))))
     (cond
       ((getf section :unavailable-p)
        (list (list (terminal-span ':notice "output preview unavailable"))))
       ((not (getf section :complete-p))
        (list (list (terminal-span ':notice "output truncated")))))
     (unless (string= text "")
       (application--preview-rows text (if stderr-p ':failure ':dim)
                                  *application-tool-output-lines* :gutter "│ ")))))

(-> application--shell-result-rows (application list) list)
(defun application--shell-result-rows (application details)
  "Return useful execution outcomes and bounded streams from typed DETAILS."
  (let* ((log      (first (getf details :shell-logs)))
         (sections (getf details :output-streams)))
    (append
     (when (or (getf details :timed-out-p) (getf log :timed-out-p))
       (list (list (terminal-span ':failure "timed out"))))
     (when (or (getf details :cancelled-p) (getf log :cancelled-p))
       (list (list (terminal-span ':notice "cancelled"))))
     (when (getf details :capture-diagnostic)
       (list (list (terminal-span ':notice "output capture problem"))))
     (loop for section in sections
           append (application--shell-section-rows application section (and (rest sections) t))))))

(-> application--native-shell-result-rows (application t) list)
(defun application--native-shell-result-rows (application result)
  "Render a projected deferred shell RESULT through the ordinary semantic rows."
  (when (json-object-p result)
    (let* ((details (json-get result "details"))
           (logs    (and (json-object-p details) (json-get details "shell-logs")))
           (log     (and (vectorp logs) (plusp (length logs)) (aref logs 0)))
           (exit    (or (and (json-object-p details) (json-get details "exit-code"))
                        (and (json-object-p log) (json-get log "exit-code"))))
           (streams (and (json-object-p details) (json-get details "output-streams"))))
      (append
       (when (integerp exit)
         (list (list (terminal-span (if (zerop exit) ':success ':failure)
                                    (format nil "exit ~D" exit)))))
       (application--shell-result-rows
        application
        (list :timed-out-p (or (and (json-object-p details) (eq t (json-get details "timed-out-p")))
                              (and (json-object-p log) (eq t (json-get log "timed-out-p"))))
              :cancelled-p (or (and (json-object-p details) (eq t (json-get details "cancelled-p")))
                               (and (json-object-p log) (eq t (json-get log "cancelled-p"))))
              :output-streams
              (when (and (vectorp streams) (not (stringp streams)))
                (loop for section across streams
                      when (json-object-p section)
                        collect (list :stream        (cond
                                                       ((equal (json-get section "stream") "stderr")
                                                        ':stderr)
                                                       ((equal (json-get section "stream") "stdout")
                                                        ':stdout)
                                                       (t
                                                        ':combined))
                                      :head          (json-get section "head")
                                      :tail          (json-get section "tail")
                                      :omitted-p     (eq t (json-get section "omitted-p"))
                                      :complete-p    (eq t (json-get section "complete-p"))
                                      :unavailable-p (eq t (json-get section "unavailable-p")))))))))))

(-> application--shell-result-entry (application list) (option list))
(defun application--shell-result-entry (application record)
  "Present typed shell outcomes and stream previews, never model-facing metadata prose."
  (let* ((raw       (getf (rest record) :details))
         (details   (and (proper-list-p raw) (evenp (length raw))
                         (every #'keywordp (plist-keys raw)) raw))
         (log       (first (getf details :shell-logs)))
         (exit-code (or (getf details :exit-code) (getf log :exit-code)))
         (success-p (and (application--tool-result-success-p record)
                         (or (not (integerp exit-code)) (zerop exit-code)))))
    (if (and details (not (getf details :shell-command-denial)))
        (application--tool-entry
         application
         :style  (if success-p ':success ':failure)
         :header (format nil "~:[!~;✓~] ~A ~A"
                         success-p (getf (rest record) :tool)
                         (if (integerp exit-code)
                             (format nil "exit ~D" exit-code)
                             "exit unavailable"))
         :detail (application--tool-result-timing record)
         :rows   (application--shell-result-rows application details))
        nil)))

(-> application--shell-completion-rows (application t) list)
(defun application--shell-completion-rows (application details)
  "Show a completion's retained shell metadata and bounded stream samples."
  (let* ((valid-p (and (proper-list-p details) (evenp (length details))
                       (every #'keywordp (plist-keys details))))
         (log     (and valid-p (first (getf details :shell-logs))))
         (exit    (and valid-p (or (getf details :exit-code) (getf log :exit-code)))))
    (append (when (integerp exit)
              (list (list (terminal-span (if (zerop exit) ':success ':failure)
                                         (format nil "exit ~D" exit)))))
            (when valid-p (application--shell-result-rows application details)))))

(-> application--first-line-summary (string) string)
(defun application--first-line-summary (output)
  "Return OUTPUT's first line without its final period, for a result header detail."
  (string-right-trim "." (or (first (application--display-lines output)) "")))

(defmethod application-tool-result-entry
    ((tool resource-edit-tool) (application application) record)
  "Present the edit outcome without repeating the model's successor observation."
  (let* ((details  (getf (getf (rest record) :details) :resource-edit))
         (summary  (getf details :summary))
         (revision (getf details :stale-revision)))
    (cond
      ((and (application--tool-result-success-p record) summary)
       (application--tool-result-entry
        application record :detail summary
        :rows                      (when (getf details :delimiter-warning-p)
                                     (list (list (terminal-span ':notice "Lisp delimiter warning"))))))
      ((and (not (application--tool-result-success-p record)) revision)
       (application--tool-result-entry
        application record
        :rows (list (cons (terminal-span ':failure "│ ")
                          (application--resource-failure-spans
                           (format nil "resource revision ~A is stale" revision))))))
      (t
       (call-next-method)))))

(defmethod application-tool-result-entry
    ((tool lisp-paren-check-tool) (application application) record)
  "Present a clean delimiter check as one line; problems keep the full report."
  (if (application--tool-result-success-p record)
      (let* ((output  (or (getf (rest record) :output) ""))
             (checked (find-if (lambda (line) (uiop:string-prefix-p "Checked " line))
                               (application--display-lines output)))
             (count   (and checked
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
    ((tool lisp-source-tool) (application application) record)
  "Present matching source as a bounded Lisp code area."
  (if (application--tool-result-success-p record)
      (application--tool-result-entry
       application
       record
       :rows (application--source-preview-rows
              (or (getf (rest record) :output) "")
              :language              *application-common-lisp-language*
              :rainbow-parentheses-p t
              :limit                 *application-tool-output-lines*))
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
              :limit    *application-tool-output-lines*))
      (call-next-method)))


;;;; -- Numbered Resource Source --

(-> application--numbered-source-line (string) (option cons))
(defun application--numbered-source-line (line)
  "Split a numbered resource LINE into its exact annotation and source text."
  (let* ((start (position-if-not (lambda (character) (char= character #\Space)) line))
         (end   (and start (position-if-not #'digit-char-p line :start start))))
    (when (and start end (< start end))
      (when-let ((separator
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
        (cons (subseq line 0 separator) (subseq line separator))))))

(-> application--resource-source-rows (list) (option list))
(defun application--resource-source-rows (details)
  "Highlight retained source lines without resolving or rereading their file."
  (let* ((uri   (getf details :uri))
         (path  (and uri (application--file-resource-path uri)))
         (lines (getf details :lines)))
    (when (and path lines)
      (let* ((visible-count (min *application-tool-output-lines* (length lines)))
             (elisions      (getf details :elisions))
             (source        (format nil "~{~A~^~%~}" (mapcar #'third lines)))
             (highlighted   (highlight-lines source :pathname path :span-function #'syntax--terminal-span)))
        (append
         (list (list (terminal-span ':dim (format nil "URI: ~A" uri))))
         (loop for (number anchor text) in lines
               for index below visible-count
               collect (append
                        (list (terminal-span ':dim "│ ")
                              (terminal-span ':dim (format nil "~6D:~A  " number anchor)))
                        (if (and highlighted (< index (length highlighted)))
                            (aref highlighted index)
                            (list (terminal-span ':plain text)))))
         (when (> (length lines) visible-count)
           (list (list (terminal-span ':dim
                                      (format nil "… +~D more source lines" (- (length lines) visible-count))))))
         (when elisions
           (list (list (terminal-span ':dim (format nil "Elided: ~{~A~^; ~}" elisions))))))))))

(defmethod application-tool-result-entry
    ((tool resource-read-tool) (application application) record)
  "Present the retained source window or a structured failure cause."
  (let* ((details (getf (getf (rest record) :details) :resource-read))
         (rows    (and (application--tool-result-success-p record)
                       (application--resource-source-rows details)))
         (cause   (getf details :cause)))
    (cond
      (rows
       (application--tool-result-entry application record :rows rows))
      ((and (not (application--tool-result-success-p record)) cause)
       (application--tool-result-entry application record :rows (application--failure-result-rows cause)))
      (t
       (call-next-method)))))


;;;; -- Search Source Matches --

(-> application--search-result-path (string) (option string))
(defun application--search-result-path (line)
  "Extract the path from a retained PATH:LINE:COLUMN search heading."
  (let* ((end    (or (search " [git:" line :from-end t) (length line)))
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
  (let* ((lines         (application--display-lines output))
         (visible-count (min *application-tool-output-lines* (length lines)))
         (path          nil)
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
  (if-let ((rows (and (application--tool-result-success-p record)
                      (application--search-source-rows
                       (or (getf (rest record) :output) "")))))
      (application--tool-result-entry application record :rows rows)
      (call-next-method)))
