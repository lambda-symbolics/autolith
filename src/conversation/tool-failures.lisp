(in-package #:autolith)

;;;; -- Failed Tool Call Survey --

;;; Pair each failed tool result with the call that produced it, so tool
;;; descriptions and ergonomics can be judged from real sessions. Segments are
;;; read one form at a time and only when written since the cutoff, so a long
;;; history costs only its recent part. Nothing is written or repaired.

(defparameter *tool-failure-text-limit* 600
  "Maximum characters kept from one failure's arguments or error text.")

(defparameter *tool-failure-example-limit* 3
  "Maximum examples printed for one tool and error shape.")

(-> tool-failure--trim ((option string) &key (:limit (integer 1))) (option string))
(defun tool-failure--trim (text &key (limit *tool-failure-text-limit*))
  "Return TEXT on one line, cut to LIMIT characters with a marker."
  (when text
    (let ((flat (substitute #\Space #\Newline text)))
      (if (> (length flat) limit)
          (format nil "~A [... ~D more]" (subseq flat 0 limit) (- (length flat) limit))
          flat))))

(-> tool-failure--shape (string) string)
(defun tool-failure--shape (error-text)
  "Return ERROR-TEXT's first line with quoted values and words holding digits blanked.

Identifiers, revisions, paths with numbers and counts differ between otherwise
identical failures, so blanking them groups the failures by their cause."
  (let* ((end    (min 160 (or (position #\Newline error-text) (length error-text))))
         (line   (subseq error-text 0 end))
         (output (make-string-output-stream))
         (index  0))
    (flet ((word-end (start)
             (or (position-if (lambda (character)
                                (member character '(#\Space #\" #\, #\; #\( #\) #\.)))
                              line :start start)
                 end)))
      (loop while (< index end)
            do (let ((character (char line index)))
                 (cond
                   ((char= character #\")
                    (let ((close (position #\" line :start (1+ index))))
                      (write-string "\"...\"" output)
                      (setf index (if close (1+ close) end))))
                   ((member character '(#\Space #\, #\; #\( #\) #\.))
                    (write-char character output)
                    (incf index))
                   (t
                    (let* ((stop (max (1+ index) (word-end (1+ index))))
                           (word (subseq line index stop)))
                      (write-string (if (some #'digit-char-p word) "#" word) output)
                      (setf index stop)))))))
    (get-output-stream-string output)))

(-> tool-failure--function-call (list) (option list))
(defun tool-failure--function-call (record)
  "Return (CALL-ID NAME ARGUMENTS) when provider item RECORD is a function call."
  (let ((wire-json (getf (rest record) :wire-json)))
    (when (and (stringp wire-json) (search "\"function_call\"" wire-json))
      (handler-case
          (let ((item (json-decode wire-json)))
            (when (and (json-object-p item)
                       (equal (json-get item "type") "function_call"))
              (list (json-get item "call_id")
                    (let ((namespace (json-get item "namespace"))
                          (name      (json-get item "name")))
                      (if (stringp namespace) (format nil "~A.~A" namespace name) name))
                    (json-get item "arguments"))))
        (error ()
          nil)))))

(-> tool-failure--conversation-identities (configuration &key (:children-p boolean)) list)
(defun tool-failure--conversation-identities (configuration &key children-p)
  "Return the identities of every primary conversation, and child task conversations when CHILDREN-P."
  (append
   (conversation-list configuration)
   (when children-p
     (let ((tasks (merge-pathnames "tasks/" (config :data-root configuration))))
       (when (uiop:directory-exists-p tasks)
         (loop for group in (uiop:subdirectories tasks)
               append (loop for execution in (uiop:subdirectories group)
                            for name = (first (last (pathname-directory execution)))
                            for identity = (merge-pathnames (make-pathname :name name :type "sexp")
                                                            execution)
                            when (uiop:directory-exists-p
                                  (conversation-storage-directory-pathname identity))
                              collect identity)))))))

(-> tool-failure-scan (pathname &key (:since integer)) list)
(defun tool-failure-scan (identity &key (since 0))
  "Return failed tool calls of conversation IDENTITY from segments written at or after SINCE.

Each failure is a plist of :conversation, :sequence, :time, :tool, :category,
:arguments and :error. Calls whose result lies in a later segment keep their
arguments, and a result whose call is older than the cutoff has none."
  (let ((calls    (make-hash-table :test #'equal))
        (failures nil)
        (name     (pathname-name identity)))
    (dolist (segment (conversation-storage-pathnames identity))
      (when (>= (or (file-write-date segment) 0) since)
        (handler-case
            (conversation--map-records
             segment
             (lambda (record)
               (case (first record)
                 (:provider-item
                  (let ((call (tool-failure--function-call record)))
                    (when (stringp (first call))
                      (setf (gethash (first call) calls) (rest call)))))
                 (:tool-result
                  (let* ((properties (rest record))
                         (call-id    (getf properties :call-id))
                         (call       (gethash call-id calls)))
                    (remhash call-id calls)
                    (when (member (getf properties :status) '(:error :mechanics))
                      (push (list :conversation name
                                  :sequence     (getf properties :seq)
                                  :time         (getf properties :time)
                                  :tool         (or (getf properties :tool) (first call) "unknown")
                                  :category     (getf properties :category)
                                  :arguments    (tool-failure--trim (second call))
                                  :error        (tool-failure--trim
                                                 (let ((output (getf properties :output)))
                                                   (if (stringp output) output ""))))
                            failures)))))))
          (error (condition)
            (format *error-output* "~&Skipped ~A: ~A~%" (namestring segment) condition)))))
    (nreverse failures)))

(-> tool-failures (configuration &key (:days (integer 1)) (:identifiers list) (:children-p boolean)) list)
(defun tool-failures (configuration &key (days 7) identifiers (children-p t))
  "Return failed tool calls from the last DAYS days, oldest conversation first.

IDENTIFIERS limits the survey to those conversations; otherwise every primary
conversation is read, and child task conversations too when CHILDREN-P."
  (let ((since (- (get-universal-time) (* days 24 60 60))))
    (loop for identity in (if identifiers
                              (mapcar (lambda (identifier)
                                        (conversation-pathname-for-id configuration identifier))
                                      identifiers)
                              (tool-failure--conversation-identities configuration
                                                                     :children-p children-p))
          append (tool-failure-scan identity :since since))))

(-> tool-failures-report (list &key (:stream t) (:examples (integer 0))) null)
(defun tool-failures-report (failures &key (stream *standard-output*) (examples *tool-failure-example-limit*))
  "Write FAILURES grouped by tool and error shape, most frequent first, with EXAMPLES each."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (failure failures)
      (push failure (gethash (list (getf failure :tool)
                                   (tool-failure--shape (or (getf failure :error) "")))
                             groups)))
    (let ((sorted (sort (loop for key being the hash-keys of groups using (hash-value members)
                              collect (cons key (reverse members)))
                        #'> :key (lambda (group) (length (rest group))))))
      (format stream "~&~D failed tool calls in ~D groups.~%" (length failures) (length sorted))
      (dolist (group sorted)
        (destructuring-bind ((tool shape) &rest members) group
          (format stream "~%~D x ~A: ~A~%" (length members) tool shape)
          (loop for failure in members
                repeat examples
                do (format stream "  ~A #~A~%    arguments: ~A~%    error: ~A~%"
                           (getf failure :conversation) (getf failure :sequence)
                           (or (getf failure :arguments) "(call not in the surveyed segments)")
                           (getf failure :error)))))))
  nil)
