(in-package #:autolith)

;;;; -- Optional Provider Usage Export --

(defparameter *llm-usage-events-file* nil
  "Override the JSONL destination; NIL uses ANALYTICS_EVENTS_FILE at request time.")

(-> llm-usage--destination () (or null pathname string))
(defun llm-usage--destination ()
  "Return the configured destination, or NIL when export is disabled."
  (let ((target (or *llm-usage-events-file* (uiop:getenv "ANALYTICS_EVENTS_FILE"))))
    (when (or (pathnamep target) (non-empty-string-p target))
      target)))

(-> llm-usage--iso-utc (&optional integer) string)
(defun llm-usage--iso-utc (&optional (timestamp (get-universal-time)))
  "Format TIMESTAMP in UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(-> llm-usage--event (conversation provider-result real) json-object)
(defun llm-usage--event (conversation result wall-seconds)
  "Build one schema-v1 event, omitting unavailable or invalid token counts."
  (let ((event (json-object
                "ts" (llm-usage--iso-utc) "schema_v" 1
                "source" "autolith" "kind" "llm-usage"
                "actor" (or (uiop:getenv "ANALYTICS_ACTOR")
                            (uiop:getenv "USER") (uiop:getenv "USERNAME") "unknown")
                "model" (or (conversation-model conversation) "unknown")
                "requests" 1 "wall_s" wall-seconds))
        (usage (provider-result-usage result)))
    (when (hash-table-p usage)
      (dolist (field '(("tokens_in" . "input_tokens")
                       ("tokens_out" . "output_tokens")
                       ("cached_tokens" . "cached_input_tokens")))
        (let ((value (gethash (rest field) usage)))
          (when (typep value '(integer 0 *))
            (setf (gethash (first field) event) value)))))
    event))

(-> llm-usage--export
    (conversation provider-result &key (:wall-seconds real) (:destination (or pathname string)))
    null)
(defun llm-usage--export (conversation result &key wall-seconds destination)
  "Append a UTF-8 event under a shared file lock, ignoring export errors."
  (handler-case
      (let* ((path      (pathname destination))
             (lock-path (pathname (concatenate 'string (namestring path) ".lock")))
             (line      (json-encode (llm-usage--event conversation result wall-seconds))))
        (ensure-directories-exist path)
        (call-with-file-lock
         lock-path
         (lambda ()
           (with-open-file (stream path :direction ':output :if-exists ':append
                                        :if-does-not-exist ':create :external-format ':utf-8)
             (write-line line stream)))))
    (error ()
      nil))
  nil)

(defmethod provider-stream-turn :around
    ((provider t) (conversation conversation) &key &allow-other-keys)
  "Refuse a request past the cost limit, then record and optionally export a successful call."
  (provider-cost-check provider)
  (let* ((destination (llm-usage--destination))
         (started     (get-internal-real-time))
         (results     (multiple-value-list (call-next-method))))
    (provider-cost-record provider (first results))
    (when destination
      (llm-usage--export
       conversation (first results) :destination destination
       :wall-seconds                             (max 0 (/ (- (get-internal-real-time) started)
                                                          (float internal-time-units-per-second 1d0)))))
    (values-list results)))
