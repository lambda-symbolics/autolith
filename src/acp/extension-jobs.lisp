
(in-package #:autolith)

(defparameter *application-client-job-structured-response-limit*
  262144
  "Maximum UTF-8 bytes in a structured job transcript's typed response envelope.")

(defmethod application-client--job-structured-window
           ((application application) job &key offset limit)
  "Return a bounded page of standard ACP updates from JOB's durable transcript."
  (let ((path (application-client--job-transcript-path application job)) (updates nil))
    (when path
      (acp-replay-map-updates (conversation-load path) (lambda (update) (push update updates))))
    (let* ((ordered  (nreverse updates))
           (total    (length ordered))
           (start    (min offset total))
           (cursor   start)
           (selected nil))
      (loop for index from start below (min (+ start limit) total)
            for update = (nth index ordered)
            do (let* ((candidate (append (reverse selected) (list update)))
                      (probe
                       (agentcomms:json-object "format" "acp-session-update" "updates"
                                               (coerce candidate 'vector) "offset" start
                                               "nextOffset"
                                               (if (= (1+ index) total)
                                                   ':null
                                                   (1+ index))
                                               "total" total "complete"
                                               (if (= (1+ index) total)
                                                   t
                                                   (json-false))))
                      (size
                       (length
                        (utf8-string-to-octets
                         (agentcomms:json-encode (acp-extension-result ':ok :value probe))))))
                 (cond
                  ((<= size *application-client-job-structured-response-limit*)
                   (push update selected) (setf cursor (1+ index)))
                  ((null selected)
                   (agentcomms:acp-invalid-params
                    "A structured transcript update exceeds the response limit."))
                  (t
                   (return)))))
      (let* ((page (nreverse selected)) (complete (= cursor total)))
        (agentcomms:json-object "format" "acp-session-update" "updates" (coerce page 'vector)
                                "offset" start "nextOffset"
                                (if complete
                                    ':null
                                    cursor)
                                "total" total "complete"
                                (if complete
                                    t
                                    (json-false)))))))
