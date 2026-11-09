
(in-package #:autolith)

(defparameter *application-client-job-structured-response-limit*
  262144
  "Maximum UTF-8 bytes in a structured job transcript's typed response envelope.")

(defmethod application-client--job-structured-window
           ((application application) job &key offset limit)
  "Count the transcript once, retaining only byte-bounded requested ACP candidates."
  (let ((candidates (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((page (updates &key start next total)
               "Build the exact response value for an admitted prefix."
               (agentcomms:json-object
                "format" "acp-session-update" "updates" updates "offset" start
                "nextOffset" (if (= next total) ':null next) "total" total
                "complete" (if (= next total) t (json-false))))

             (size (value)
               "Measure the UTF-8 wire encoding, including JSON escaping."
               (length (utf8-string-to-octets (agentcomms:json-encode value)))))
      (multiple-value-bind (total start next)
          (application-client--transcript-page
           (application-client--job-transcript-path application job)
           :offset offset :limit limit
           :mapper    (lambda (conversation emit)
                        (acp-replay--map-records
                         conversation
                         (lambda (record turn-sequence)
                           (let ((producer (acp-replay--record-producer record turn-sequence)))
                             (when producer (funcall emit 1 producer))))))
           :collector (lambda (producer first last)
                        (declare (ignore first last))
                        ;; The array alone is a lower bound for every final envelope.
                        ;; Retain one failing candidate so exact admission owns rejection.
                        (vector-push-extend (funcall producer) candidates)
                        (<= (size candidates) *application-client-job-structured-response-limit*)))
        (declare (ignore next))
        (let ((cursor start) (count 0))
          (loop repeat (length candidates)
                do (let* ((end   (1+ cursor))
                          (probe (page (subseq candidates 0 (1+ count))
                                       :start start :next end :total total)))
                     (cond
                       ((<= (size (acp-extension-result ':ok :value probe))
                            *application-client-job-structured-response-limit*)
                        (incf count) (setf cursor end))
                       ((zerop count)
                        (agentcomms:acp-invalid-params
                         "A structured transcript update exceeds the response limit."))
                       (t
                        (return)))))
          (page (subseq candidates 0 count) :start start :next cursor :total total))))))
