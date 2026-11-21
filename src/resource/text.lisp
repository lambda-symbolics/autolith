(in-package #:autolith)

;;;; -- Revision-Gated Text Resources --

(defparameter *workspace-file-resource-maximum-observations* 16
  "The maximum retained text observations in one conversation.")

(defparameter *workspace-file-resource-maximum-retained-bytes* (* 16 1024 1024)
  "The maximum UTF-8 bytes retained by text observations per conversation.")

(defparameter *workspace-file-resource-default-line-count* 400
  "The default text-resource line window.")

(defparameter *workspace-file-resource-maximum-line-count* 1000
  "The largest text-resource line window.")

(defparameter *workspace-file-resource-maximum-result-characters* 7600
  "The maximum characters in a numbered text window.")

(defparameter *workspace-file-resource-anchor-maximum-offset* 3
  "The largest line-number offset corrected by a hash anchor.")

(defclass text-resource (resource)
  ()
  (:documentation "A resource read and edited through observed logical lines."))

(defclass text-resource-observation (resource-observation)
  ((stored-lines
    :initarg :lines :initform nil
    :reader        text-resource-observation-stored-lines
    :type          (option vector)
    :documentation "Optional pre-split logical lines.")
   (line-ending
    :initarg :line-ending :reader text-resource-observation-line-ending
    :type          string
    :documentation "The line ending used when reconstructing edited content.")
   (final-newline-p
    :initarg :final-newline-p :reader text-resource-observation-final-newline-p
    :type          boolean
    :documentation "Whether the exact snapshot ends in a line ending."))
  (:documentation "An exact text snapshot independent of its storage backend."))

(defclass text-resource-observation-state (resource-observation-state)
  ((visible-ranges
    :initarg       :visible-ranges
    :accessor      text-resource-observation-state-visible-ranges
    :type          structlisp:integer-interval-set
    :documentation "Half-open one-based ranges fully shown in this conversation."))
  (:documentation "An observed text revision and its cumulative visible lines."))

(defgeneric text-resource-read-start (resource &key start-line line-count total-lines)
  (:documentation "Resolve a requested start line using RESOURCE's past-EOF policy."))

(defgeneric text-resource-edit-state (resource conversation revision)
  (:documentation "Find the exact URI-bound observed revision for an edit."))

(defgeneric text-resource-check-edit-base (resource context base)
  (:documentation "Authorize BASE and check backend-specific edit preconditions."))

(defgeneric text-resource-commit (resource context &key base content)
  (:documentation "Commit CONTENT only while RESOURCE still matches BASE; return its observation."))

(defgeneric text-resource-observation-annotations (observation)
  (:documentation "Return backend-specific metadata lines for an observation header."))

(defgeneric text-resource-source-pathname (observation)
  (:documentation "Return the optional source pathname used for delimiter annotations."))

(defgeneric text-resource-edit-notice (resource)
  (:documentation "Return backend-specific prose following a successful edit summary."))

(defgeneric text-resource-read-tool-result (observation content &key details)
  (:documentation "Wrap the bounded read content and details with the backend's read outcome."))

(defmethod initialize-instance :after ((state text-resource-observation-state) &key)
  "Normalize inclusive renderer ranges into the shared interval representation."
  (let ((ranges (text-resource-observation-state-visible-ranges state)))
    (unless (typep ranges 'structlisp:integer-interval-set)
      (setf (text-resource-observation-state-visible-ranges state)
            (structlisp:make-integer-interval-set
             :intervals (mapcar (lambda (range) (cons (first range) (1+ (second range)))) ranges))))))

(defmethod resource-observation-state-family-and-key ((observation text-resource-observation))
  "Identify one exact text revision without depending on backend metadata."
  (values 'text-resource-observation-state
          (list (resource-observation-uri observation)
                (resource-observation-revision observation)
                (resource-observation-content observation))))

(defmethod resource-observation-state-weight (alias (state text-resource-observation-state))
  "Count retained snapshot bytes."
  (declare (ignore alias))
  (length (utf8-string-to-octets
           (resource-observation-content (resource-observation-state-observation state)))))

(defmethod resource-observation-state-maximum ((state text-resource-observation-state))
  "Bound retained text observations."
  (declare (ignore state))
  *workspace-file-resource-maximum-observations*)

(defmethod resource-observation-state-trim-storage
    ((conversation conversation) (state text-resource-observation-state))
  "Evict oldest text observations until retained snapshots fit."
  (declare (ignore state))
  (let ((states (conversation-resource-observations conversation)))
    (loop while (> (fifo-cache-count-if
                    (lambda (alias candidate)
                      (declare (ignore alias))
                      (typep candidate 'text-resource-observation-state))
                    states)
                   *workspace-file-resource-maximum-observations*)
          do (fifo-cache-delete-first-if
              (lambda (alias candidate)
                (declare (ignore alias))
                (typep candidate 'text-resource-observation-state))
              states))
    (loop while (> (fifo-cache-total-weight states) *workspace-file-resource-maximum-retained-bytes*)
          do (fifo-cache-delete-first-if
              (lambda (alias candidate)
                (declare (ignore alias))
                (typep candidate 'text-resource-observation-state))
              states)))
  nil)

(defmethod resource-observation-state-merge
    ((state text-resource-observation-state) (observation text-resource-observation) &rest initargs)
  "Accumulate only fully rendered line ranges."
  (declare (ignore observation))
  (dolist (range (getf initargs :visible-ranges))
    (structlisp:integer-interval-set-add
     (text-resource-observation-state-visible-ranges state) (first range) (1+ (second range))))
  state)

(defmethod text-resource-read-start ((resource text-resource) &key start-line line-count total-lines)
  "Refuse past-EOF windows unless the backend explicitly permits a fallback."
  (declare (ignore resource line-count))
  (when (and (plusp total-lines) (> start-line total-lines))
    (error 'tool-error :message  (format nil "Start line ~D is beyond the resource's ~D lines. Request a valid window."
                                         start-line total-lines)
                      :tool-name "resource.read"))
  start-line)

(defmethod text-resource-edit-state ((resource text-resource) conversation revision)
  "Require a conversation-local observation of this exact URI."
  (text-resource--find-state conversation (resource-uri resource) revision))

(defmethod text-resource-source-pathname ((observation text-resource-observation))
  "Omit source annotations when the backend has no pathname."
  (declare (ignore observation))
  nil)

(defmethod text-resource-edit-notice ((resource text-resource))
  "Use no backend-specific edit notice by default."
  (declare (ignore resource))
  "")

(defmethod text-resource-read-tool-result ((observation text-resource-observation) content &key details)
  "Return a plain text result for backends without a source preview."
  (declare (ignore observation details))
  (tool-success content))

(-> text-resource-observation-lines (text-resource-observation) vector)
(defun text-resource-observation-lines (observation)
  "Return supplied logical lines or split the exact snapshot lazily."
  (or (text-resource-observation-stored-lines observation)
      (text--split-lines (resource-observation-content observation))))

(-> text-resource--find-state
    (conversation non-empty-string non-empty-string &key (:family symbol) (:overrides (option hash-table)))
    text-resource-observation-state)
(defun text-resource--find-state (conversation uri revision &key (family 'text-resource-observation-state) overrides)
  "Find a revision of URI, optionally using one guarded proposal's baselines."
  (let ((state (or (and overrides (gethash (list conversation uri revision) overrides))
                   (resource-observation-state-find (conversation-resource-observations conversation) revision family))))
    (unless (and state (string= uri (resource-observation-uri (resource-observation-state-observation state))))
      (error 'resource-revision-stale :uri uri :expected-revision revision :actual-revision nil))
    state))

(-> text-resource--read-result
    (text-resource-observation-state string &key (:total-lines (integer 0)) (:truncated-p boolean)) string)
(defun text-resource--read-result (state body &key total-lines truncated-p)
  "Render shared identity, visibility and backend-specific snapshot metadata."
  (let* ((observation (resource-observation-state-observation state))
         (ranges      (text-resource-observation-state-visible-ranges state))
         (elisions    (text-resource--elisions total-lines ranges truncated-p)))
    (format nil "URI: ~A~%Revision: ~A~%~AVisible lines: ~A of ~D~%Elided: ~A~%Content:~%~A"
            (resource-observation-uri observation) (resource-observation-state-alias state)
            (text-resource-observation-annotations observation)
            (text-resource--format-ranges ranges) total-lines
            (if elisions (format nil "~{~A~^; ~}" elisions) "none") body)))

(-> text-resource--window-result
    (tool-context text-resource-observation
     &key (:start-line (integer 1)) (:line-count (integer 1)) (:carried list) (:reading-p boolean))
    (values string list &optional))
(defun text-resource--window-result (context observation &key start-line line-count carried reading-p)
  "Return the rendered window and, for reads, its visible source metadata.

A nonempty read with no complete visible line fails before retaining its snapshot.
An edit may produce an overlong line; its successful publication is still reported."
  (let* ((lines       (text-resource-observation-lines observation))
         (total-lines (length lines)))
    (multiple-value-bind (body visible-ranges last-line truncated-p)
        (text--numbered-line-window lines :start-line start-line :line-count line-count
                                   :maximum-characters   *workspace-file-resource-maximum-result-characters*
                                   :line-anchor-function #'cl-hashline:line-anchor)
      (when (and reading-p (plusp total-lines) (null visible-ranges))
        (error 'tool-error
               :message   (format nil "Line ~D exceeds the resource.read result limit and was not observed. Use bounded search to inspect it."
                                  start-line)
               :tool-name "resource.read"))
      (let ((state (resource-observation-state-ensure
                    (tool-context-conversation context) observation
                    :visible-ranges (append carried visible-ranges))))
        (values (text-resource--read-result state body :total-lines total-lines :truncated-p truncated-p)
                (when (and reading-p visible-ranges)
                  (list :resource-read
                        (list :uri      (resource-observation-uri observation)
                              :lines    (loop for number from start-line to last-line
                                              for text = (aref lines (1- number))
                                              collect (list number (cl-hashline:line-anchor text) text))
                              :elisions (remove nil
                                                (list (when (> start-line 1)
                                                        (format nil "1-~D before" (1- start-line)))
                                                      (when (< last-line total-lines)
                                                        (format nil "~D-~D after" (1+ last-line) total-lines))
                                                      (when truncated-p "current result truncated")))))))))))

(defmethod resource-apply-operations
    ((resource text-resource) (context tool-context) &key base-revision operations)
  "Edit one exact observation; return its successor, normalized edits and unchanged visible ranges."
  (let ((conversation (tool-context-conversation context)))
    (with-recursive-lock-held ((conversation-resource-observation-lock conversation))
      (let* ((state (text-resource-edit-state resource conversation base-revision))
             (base  (resource-observation-state-observation state)))
        (text-resource-check-edit-base resource context base)
        (handler-case
            (multiple-value-bind (content normalized)
                (cl-hashline:edit-text
                 (resource-observation-content base)
                 (if (listp operations) (mapcar #'text-resource--json-operation operations) operations)
                 :visible-lines         (text-resource-observation-state-visible-ranges state)
                 :split-lines           #'text--split-lines
                 :anchor-maximum-offset *workspace-file-resource-anchor-maximum-offset*)
              (values (text-resource-commit resource context :base base :content content) normalized
                      (text-resource--carried-visible-ranges
                       (text-resource-observation-state-visible-ranges state)
                       (resource-observation-content base) content)))
          (cl-hashline:hashline-error (condition)
            (error 'tool-error :message (princ-to-string condition) :tool-name "resource.edit")))))))

(defmethod resource-tool-read
    ((resource text-resource) (tool resource-read-tool) (context tool-context) (arguments hash-table))
  "Read one bounded text window and establish its fully visible edit ranges."
  (declare (ignore tool))
  (let* ((start-line  (max 1 (or (workspace-tool-integer-argument arguments "start-line" :fallback 1) 1)))
         (line-count  (min *workspace-file-resource-maximum-line-count*
                           (max 1 (or (workspace-tool-integer-argument
                                       arguments "line-count" :fallback *workspace-file-resource-default-line-count*)
                                      *workspace-file-resource-default-line-count*))))
         (observation (resource-observe resource context))
         (start       (text-resource-read-start resource :start-line start-line :line-count line-count
                                                        :total-lines (length (text-resource-observation-lines observation)))))
    (multiple-value-bind (content details)
        (text-resource--window-result context observation :start-line start :line-count line-count :reading-p t)
      (text-resource-read-tool-result observation content :details details))))

(defmethod resource-tool-edit
    ((resource text-resource) (tool resource-edit-tool) (context tool-context) (arguments hash-table))
  "Apply original-line edits and render their successor observation."
  (declare (ignore tool))
  (let ((uri             (tool-argument arguments "uri" :required t))
        (base-revision   (tool-argument arguments "base-revision" :required t))
        (operation-array (tool-argument arguments "operations" :required t)))
    (unless (non-empty-string-p base-revision)
      (error 'tool-error :message "Resource edit base-revision must be a non-empty string." :tool-name "resource.edit"))
    (unless (and (vectorp operation-array) (plusp (length operation-array)))
      (error 'tool-error :message "Resource edit operations must be a non-empty JSON array." :tool-name "resource.edit"))
    (handler-case
        (multiple-value-bind (observation normalized carried)
            (resource-apply-operations resource context :base-revision base-revision :operations (coerce operation-array 'list))
          (multiple-value-bind (start-line line-count)
              (text-resource--operation-window normalized (length (text-resource-observation-lines observation)))
            (let* ((summary (format nil "Applied ~{~A~^; ~}."
                                    (mapcar (lambda (operation) (getf operation :summary)) normalized)))
                   (result  (format nil "~A~A~%~A"
                                    summary (text-resource-edit-notice resource)
                                    (text-resource--window-result context observation :start-line start-line
                                                                                    :line-count line-count :carried carried)))
                   (path    (text-resource-source-pathname observation))
                   (warning (and path (lisp-source-edit-result-content
                                       "" path (resource-observation-content observation)))))
              (tool-success (if warning (concatenate 'string result warning) result)
                            :details (list :resource-edit
                                           (list :summary             (resource-tool--detail-text summary)
                                                 :delimiter-warning-p (and warning (plusp (length warning)))))))))
      (resource-revision-stale ()
        (tool-failure
         (format nil "Resource revision ~A is stale, expired, or was not observed in this conversation. Reread ~A with resource.read and retry against the returned revision."
                 base-revision uri)
         :details (resource-tool--stale-edit-details base-revision))))))


;;;; -- Line Operation Mechanics --

(-> text-resource--format-ranges (structlisp:integer-interval-set) string)
(defun text-resource--format-ranges (ranges)
  "Return inclusive visible ranges in concise form."
  (if (structlisp:integer-interval-set-empty-p ranges)
      "none"
      (format nil "~{~A~^, ~}"
              (loop for range across (structlisp:integer-interval-set->vector ranges)
                    for start = (structlisp:integer-interval-start range)
                    for end = (1- (structlisp:integer-interval-end range))
                    collect (if (= start end) (format nil "~D" start) (format nil "~D-~D" start end))))))

(-> text-resource--elisions ((integer 0) structlisp:integer-interval-set boolean) list)
(defun text-resource--elisions (total-lines ranges truncated-p)
  "Return gaps outside the accumulated visible ranges."
  (let* ((complete (structlisp:make-integer-interval-set :intervals (list (cons 1 (1+ total-lines)))))
         (gaps     (structlisp:integer-interval-set-difference complete ranges))
         (elisions (loop for gap across (structlisp:integer-interval-set->vector gaps)
                         for start = (structlisp:integer-interval-start gap)
                         for end = (1- (structlisp:integer-interval-end gap))
                         collect (format nil "~D-~D ~A" start end
                                         (cond
                                           ((= end total-lines)
                                            "after")
                                           ((= start 1)
                                            "before")
                                           (t
                                            "between visible ranges"))))))
    (when truncated-p
      (setf elisions (append elisions (list "current result truncated"))))
    elisions))

(-> text-resource--operation-window (list (integer 0)) (values (integer 1) (integer 1)))
(defun text-resource--operation-window (operations total-lines)
  "Return a bounded nearby window covering the edited lines."
  (let* ((first-line (reduce #'min operations :key (lambda (operation) (getf operation :start))))
         (last-line  (reduce #'max operations :key (lambda (operation) (getf operation :end))))
         (start      (max 1 (- first-line 2)))
         (count      (max 1 (min 120 (+ (- last-line start) 6)))))
    (values (if (zerop total-lines) 1 (min start total-lines)) count)))

(-> text-resource--json-operation (json-object) list)
(defun text-resource--json-operation (operation)
  "Translate JSON operation fields into cl-hashline keyword data."
  (unless (hash-table-p operation)
    (error 'tool-error :message "Resource edit operations must be JSON objects." :tool-name "resource.edit"))
  (let ((fields '(("op" . :op) ("line" . :line)
                  ("start-line" . :start-line) ("end-line" . :end-line)
                  ("anchor" . :anchor) ("start-anchor" . :start-anchor)
                  ("end-anchor" . :end-anchor) ("content" . :content)))
        (result nil))
    (maphash
     (lambda (key value)
       (let ((field (assoc key fields :test #'equal)))
         (unless field
           (error 'tool-error :message (format nil "Unsupported resource edit field ~S." key) :tool-name "resource.edit"))
         (when (eq (rest field) ':op)
           ;; Another resource's operation stays as written, for the refusal to quote.
           (setf value
                 (or (find (resource-edit-kind value)
                           '(:replace-lines :delete-lines :insert-before :insert-after
                             :replace-empty))
                     value)))
         (setf result (list* (rest field) value result))))
     operation)
    result))

(-> text-resource--carried-visible-ranges (structlisp:integer-interval-set string string) list)
(defun text-resource--carried-visible-ranges (ranges old-content new-content)
  "Carry visible unchanged prefix and suffix lines into the edited revision."
  (let* ((old       (coerce (text--split-lines old-content) 'vector))
         (new       (coerce (text--split-lines new-content) 'vector))
         (limit     (min (length old) (length new)))
         (prefix    (or (mismatch old new :test #'string= :end1 limit :end2 limit) limit))
         (suffix    (loop for count from 0 below (- limit prefix)
                          while (string= (aref old (- (length old) count 1)) (aref new (- (length new) count 1)))
                          finally (return count)))
         (kept-from (1+ (- (length old) suffix)))
         (shift     (- (length new) (length old)))
         (carried   nil))
    (loop for range across (structlisp:integer-interval-set->vector ranges)
          for start = (structlisp:integer-interval-start range)
          for end = (1- (structlisp:integer-interval-end range))
          do (when (<= start prefix)
               (push (list start (min end prefix)) carried))
             (when (>= end kept-from)
               (push (list (+ (max start kept-from) shift) (+ end shift)) carried)))
    (nreverse carried)))
