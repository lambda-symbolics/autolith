(in-package #:autolith)

(defparameter *provider-compaction-recovery-permit* nil
  "Request-local, single-dispatch metadata window allowance for emergency compaction.")

;;;; -- Detached Emergency Compaction --

(defstruct (compaction-recovery (:constructor make-compaction-recovery (&optional audit-function)))
  "One compaction operation's bounded recovery state, shared by all request paths."
  (audit-function nil :type (option function))
  (tier 0 :type (integer 0 3))
  (permit nil :type list)
  (initialized-p nil :type boolean)
  (captured-p nil :type boolean)
  (protected (make-hash-table :test #'eq) :type hash-table)
  (notes nil :type list)
  (replacements (make-hash-table :test #'eq) :type hash-table))

(defgeneric provider-compaction-recovery-window (provider)
  (:documentation "Return existing selected-route metadata without discovery or preparation."))

(defmethod provider-compaction-recovery-window ((provider model-provider))
  "Read only the selected model/provider's existing registry entry."
  (let ((configuration (provider-configuration provider)))
    (provider-model-context-window-for (config :model configuration)
                                       :provider (config :model-provider configuration))))

(-> compaction-recovery-note (compaction-recovery keyword &rest t) null)
(defun compaction-recovery-note (state tier &rest details)
  "Retain chronological, portable recovery audit details."
  (push (list* :tier tier details) (compaction-recovery-notes state))
  (when (compaction-recovery-audit-function state)
    (funcall (compaction-recovery-audit-function state)
             (list :stage tier :details (copy-tree details))))
  nil)

(-> compaction-recovery-audit (compaction-recovery keyword) (option list))
(defun compaction-recovery-audit (state outcome)
  "Return a detached durable audit, or NIL when no recovery was necessary."
  (when (compaction-recovery-initialized-p state)
    (list :outcome outcome :stages (reverse (copy-tree (compaction-recovery-notes state))))))

(-> compaction-recovery-copy (t) t)
(defun compaction-recovery-copy (value)
  "Deep-copy JSON values before any recovery transformation."
  (typecase value
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key item) (setf (gethash key copy) (compaction-recovery-copy item))) value)
       copy))
    (string
     (copy-seq value))
    (vector
     (map 'vector #'compaction-recovery-copy value))
    (cons
     (mapcar #'compaction-recovery-copy value))
    (t
     value)))

(-> compaction-recovery-view (compaction-recovery conversation) conversation)
(defun compaction-recovery-view (state source)
  "Detach SOURCE while carrying previous removals and replacements across paths."
  (let ((items     nil)
        (origins   nil)
        (table     (compaction-recovery-replacements state))
        (protected (compaction-recovery-protected state)))
    (unless (compaction-recovery-captured-p state)
      (setf (compaction-recovery-captured-p state) t)
      (dolist (item (nthcdr (compaction-recovery-last-user (conversation-input-items source))
                           (conversation-input-items source)))
        (setf (gethash item protected) t)))
    (dolist (item (conversation-input-items source))
      (let ((replacement (gethash item table item)))
        (unless (eq replacement :removed)
          (let ((copy (compaction-recovery-copy replacement)))
            (when (or (gethash item protected) (gethash replacement protected))
              (setf (gethash copy protected) t))
            (setf (gethash item table) copy
                  (gethash copy table) copy)
            (push (cons item copy) origins)
            (push copy items)))))
    (let ((target (conversation-compaction-summary-view source (nreverse items))))
      (dolist (origin origins)
        (dolist (key '(:family :handoff-family :checkpoint-summary))
          (multiple-value-bind (value present-p)
              (gethash (first origin)
                       (clinker-transcript:projection-metadata-table (conversation-projection source) key))
            (when present-p
              (setf (gethash (rest origin)
                             (clinker-transcript:projection-metadata-table (conversation-projection target) key))
                    value)))))
      target)))

(-> compaction-recovery-replace (compaction-recovery json-object t) null)
(defun compaction-recovery-replace (state item replacement)
  "Carry ITEM's replacement through older source identities as well as new views."
  (let ((table (compaction-recovery-replacements state)))
    (maphash (lambda (key value)
               (when (eq value item) (setf (gethash key table) replacement))) table)
    (setf (gethash item table) replacement))
  nil)

(-> compaction-recovery-last-user (list) (integer 0))
(defun compaction-recovery-last-user (items)
  "Return the latest user boundary, or zero when no such boundary exists."
  (or (position-if (lambda (item) (json-string= (json-get item "role") "user")) items :from-end t) 0))

(-> compaction-recovery-linked-p (json-object) boolean)
(defun compaction-recovery-linked-p (item)
  "Conservatively recognize protocol-correlated calls, outputs and opaque anchors."
  (not (null (or (native-compaction-item-p item)
                 (json-get item "call_id")
                 (member (json-get item "type") '("function_call" "function_call_output")
                         :test #'equal)))))

(-> compaction-recovery-strip-encrypted (compaction-recovery conversation) conversation)
(defun compaction-recovery-strip-encrypted (state view)
  "Remove standalone old encrypted reasoning, preserving current-loop protocol state."
  (let* ((items   (conversation-input-items view))
         (removed 0)
         (result
           (loop for item in items
                 for index from 0
                 unless (and (not (gethash item (compaction-recovery-protected state)))
                             (json-string= (json-get item "type") "reasoning")
                             (non-empty-string-p (json-get item "encrypted_content"))
                             (not (compaction-recovery-linked-p item))
                             (not (and (plusp index)
                                       (compaction-recovery-linked-p (nth (1- index) items))))
                             (not (and (< (1+ index) (length items))
                                       (compaction-recovery-linked-p (nth (1+ index) items)))))
                   collect item
                 else do (incf removed)
                         (compaction-recovery-replace state item :removed))))
    (compaction-recovery-note state :encrypted :removed-items removed)
    (conversation-compaction-summary-view view result)))

(-> compaction-recovery-protected-start (compaction-recovery conversation) (integer 0))
(defun compaction-recovery-protected-start (state view)
  "Protect about 40K newest tokens, the latest user, and entire correlated groups."
  (let* ((items      (conversation-input-items view))
         (start      (length items))
         (characters 0))
    (loop for item in (reverse items)
          for index downfrom (1- (length items))
          while (< characters (* 40960 4))
          do (incf characters (length (json-encode item)))
             (setf start index))
    (let ((protected (position-if (lambda (item) (gethash item (compaction-recovery-protected state))) items)))
      (when protected (setf start (min start protected))))
    (let* ((plan (clinker-transcript:make-compaction-plan
                  (conversation-projection view) :cutoff start
                  :repaired-output-p                     #'conversation-compaction--repaired-output-p))
           (tail (clinker-transcript:compaction-plan-items plan)))
      (min start (- (length items) (length tail))))))

(-> compaction-recovery-stub-candidates (compaction-recovery conversation) list)
(defun compaction-recovery-stub-candidates (state view)
  "Return eligible old tool outputs newest-first, outside all protected groups."
  (let* ((items (conversation-input-items view))
         (end   (compaction-recovery-protected-start state view)))
    (loop for index downfrom (1- end) to 0
          for item = (nth index items)
          when (and (json-string= (json-get item "type") "function_call_output")
                    (stringp (json-get item "output"))
                    (not (equal (json-get item "output") *conversation-interrupted-tool-output*))
                    (not (and (plusp index) (native-compaction-item-p (nth (1- index) items))))
                    (not (and (< (1+ index) (length items))
                              (native-compaction-item-p (nth (1+ index) items)))))
            collect item)))

(-> compaction-recovery-stub (compaction-recovery conversation json-object) conversation)
(defun compaction-recovery-stub (state view item)
  "Replace one old output without changing its identifier, batch or position."
  (let* ((copy (compaction-recovery-copy item))
         (size (length (json-get item "output"))))
    (setf (gethash "output" copy)
          (format nil "[Emergency compaction Tier2: old tool output elided; original ~D characters.]" size))
    (compaction-recovery-replace state item copy)
    (compaction-recovery-note state :tool-output   :call-id (json-get item "call_id")
                              :original-characters size)
    (conversation-compaction-summary-view
     view (substitute copy item (conversation-input-items view) :test #'eq))))

(-> compaction-recovery-text-view (compaction-recovery conversation) conversation)
(defun compaction-recovery-text-view (state view)
  "Render whole exchanges as plain text, without clipping the newest exchange."
  (let ((groups  nil)
        (current nil)
        (anchors 0))
    (dolist (item (conversation-input-items view))
      (when (and current (json-string= (json-get item "role") "user"))
        (push (nreverse current) groups)
        (setf current nil))
      (if (or (native-compaction-item-p item)
              (json-string= (json-get item "type") "reasoning"))
          (incf anchors)
          (push item current)))
    (when current (push (nreverse current) groups))
    (compaction-recovery-note state :plain-text :opaque-items-removed anchors
                              :notice           "Native opaque anchors and reasoning are absent from the plain transcript.")
    (conversation-compaction-summary-view
     view (loop for group in (nreverse groups)
                collect (json-object "type" "message" "role" "user"
                                     "content" (vector (json-object "type" "input_text" "text"
                                                                    (json-encode (coerce group 'vector)))))))))

(-> compaction-recovery-drop-exchange (conversation) (option conversation))
(defun compaction-recovery-drop-exchange (view)
  "Drop the oldest whole exchange; never discard the newest exchange."
  (let* ((items (conversation-input-items view))
         (next  (position-if (lambda (item) (json-string= (json-get item "role") "user"))
                            items :start (min 1 (length items)))))
    (when next
      (conversation-compaction-summary-view view (nthcdr next items)))))

(-> compaction-recovery-initialize (compaction-recovery model-provider) null)
(defun compaction-recovery-initialize (state provider)
  "Initialize the metadata permit once, only after a compaction admission refusal."
  (unless (compaction-recovery-initialized-p state)
    (setf (compaction-recovery-initialized-p state) t)
    (let ((window     (provider-compaction-recovery-window provider))
          (configured (config :context-window (provider-configuration provider))))
      (if (and (integerp window) (> window configured))
          (progn
            (setf (compaction-recovery-permit state) (list provider window :ready))
            (compaction-recovery-note state :metadata :configured configured :metadata window))
          (compaction-recovery-note state :metadata-skipped :configured configured :metadata window))))
  nil)

(-> compaction-recovery-call
    (compaction-recovery model-provider conversation
     &key (:dispatch function) (:preflight function) (:native-p boolean))
    (values t conversation))
(defun compaction-recovery-call (state provider source &key dispatch preflight native-p)
  "Recover only a local compaction admission refusal, sharing all tier state.

DISPATCH and PREFLIGHT receive the exact detached candidate. Unrelated errors
abort immediately. Native exhaustion permits portable fallback; portable
exhaustion preserves the original conversation and signals the final refusal."
  (let ((view    (compaction-recovery-view state source))
        (failure nil))
    (labels ((attempt ()
               "Dispatch through the caller's ordinary mission accounting wrapper."
               (handler-case (values (funcall dispatch view) t)
                 (provider-context-budget-error (condition)
                   (setf failure condition)
                   (values nil nil))
                 (error (condition)
                   (when (compaction-recovery-initialized-p state)
                     (compaction-recovery-note state :aborted :condition-type (type-of condition)))
                   (error condition))))

             (fits-p ()
               "Recheck admission without dispatching or consuming a permit."
               (handler-case (progn (funcall preflight view) t)
                 (provider-context-budget-error (condition)
                   (setf failure condition)
                   nil)))

             (try-fitting-candidate ()
               "Try the ordinary envelope, then any still-unspent metadata permit."
               (when (fits-p)
                 (return-from try-fitting-candidate (attempt)))
               (let ((permit (compaction-recovery-permit state)))
                 (when (and permit (eq :ready (third permit)))
                   (let ((*provider-compaction-recovery-permit* permit))
                     (when (fits-p)
                       (return-from try-fitting-candidate (attempt))))))
               (values nil nil)))
      (multiple-value-bind (result success-p) (attempt)
        (when success-p
          (return-from compaction-recovery-call (values result view))))
      (compaction-recovery-initialize state provider)
      (multiple-value-bind (result success-p) (try-fitting-candidate)
        (when success-p
          (return-from compaction-recovery-call (values result view))))
      (when (< (compaction-recovery-tier state) 1)
        (setf (compaction-recovery-tier state) 1
              view                          (compaction-recovery-strip-encrypted state view))
        (multiple-value-bind (result success-p) (try-fitting-candidate)
          (when success-p
            (return-from compaction-recovery-call (values result view)))))
      (when (< (compaction-recovery-tier state) 2)
        (setf (compaction-recovery-tier state) 2)
        (compaction-recovery-note state :tool-output-stage)
        (dolist (item (compaction-recovery-stub-candidates state view))
          (setf view (compaction-recovery-stub state view item))
          (multiple-value-bind (result success-p) (try-fitting-candidate)
            (when success-p
              (return-from compaction-recovery-call (values result view))))))
      (when native-p
        (compaction-recovery-note state :portable-fallback)
        (return-from compaction-recovery-call (values nil view)))
      (when (< (compaction-recovery-tier state) 3)
        (setf (compaction-recovery-tier state) 3
              view                          (compaction-recovery-text-view state view))
        (loop
          (multiple-value-bind (result success-p) (try-fitting-candidate)
            (when success-p
              (return-from compaction-recovery-call (values result view))))
          (let ((next (compaction-recovery-drop-exchange view)))
            (unless next
              (return))
            (compaction-recovery-note state :oldest-exchange-dropped)
            (setf view next))))
      (compaction-recovery-note state :exhausted :message (princ-to-string failure))
      (error failure))))

(-> compaction-recovery-publication (compaction-recovery conversation-compaction) conversation-compaction)
(defun compaction-recovery-publication (state capture)
  "Apply approved losses to detached carry-forward state, preserving the original arrival guard."
  (if (zerop (compaction-recovery-tier state))
      capture
      (let* ((source    (conversation-compaction-view capture))
             (plan      (conversation-compaction--plan capture))
             (tail      (clinker-transcript:compaction-plan-items plan))
             (tail-view (conversation-compaction-summary-view source tail)))
        ;; The portable summary view excludes tail metadata; use the captured plan.
        (conversation-compaction--copy-metadata
         (clinker-transcript:compaction-plan-projection
          plan :repair-output #'conversation-compaction--repair-output)
         (conversation-projection tail-view) tail)
        (let ((view (compaction-recovery-view state tail-view)))
          (make-instance
           'conversation-compaction
           :conversation    (conversation-compaction--conversation capture)
           :view source :source-items (conversation-compaction--source-items capture)
           :cutoff-sequence (conversation-compaction-cutoff-sequence capture)
           :plan            (clinker-transcript:make-compaction-plan
                             (conversation-projection view) :cutoff 0
                             :repaired-output-p                     #'conversation-compaction--repaired-output-p))))))

(-> compaction-recovery-notice (list) (option string))
(defun compaction-recovery-notice (audit)
  "Describe durable recovery stages and losses in transcript and replay notices."
  (when audit
    (format nil "Emergency compaction recovery (~(~A~)): ~{~(~A~)~^, ~}."
            (getf audit :outcome)
            (mapcar (lambda (stage) (getf stage :tier)) (getf audit :stages)))))
