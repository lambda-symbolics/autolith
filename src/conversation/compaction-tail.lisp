(in-package #:autolith)

;;;; -- Verbatim Portable Compaction Tail --

(defparameter *conversation-compaction-tail-characters* 32768
  "Maximum ordinary recent-tail characters retained beside a portable summary.

The current user request and protocol-required call/reasoning groups are retained
even when larger. Request admission checks the resulting projection before I/O.")

(defparameter *conversation-compaction-tail-user-turns* 2
  "Maximum recent user turns considered for the ordinary verbatim tail.")

(-> conversation-compaction--checkpoint-summary-p
    (clinker-transcript:projection json-object) boolean)
(defun conversation-compaction--checkpoint-summary-p (projection item)
  "Identify checkpoint handoffs by projection metadata rather than their prose."
  (not (null (or (gethash item (clinker-transcript:projection-metadata-table
                                projection ':checkpoint-summary))
                 (nth-value 1 (gethash item (clinker-transcript:projection-metadata-table
                                             projection ':handoff-family)))))))

(-> conversation-compaction--tail-start (clinker-transcript:projection list) (integer 0))
(defun conversation-compaction--tail-start (projection items)
  "Choose a bounded suffix without including an earlier checkpoint handoff."
  (let ((start      (length items))
        (characters 0)
        (turns      0))
    (loop for item in (reverse items)
          for index downfrom (1- (length items))
          for size = (length (json-encode item))
          do (when (or (conversation-compaction--checkpoint-summary-p projection item)
                       (native-compaction-item-p item)
                       (> (+ characters size) *conversation-compaction-tail-characters*)
                       (and (json-string= (json-get item "role") "user")
                            (>= turns *conversation-compaction-tail-user-turns*)))
               (return))
             (incf characters size)
             (when (json-string= (json-get item "role") "user")
               (incf turns))
             (setf start index))
    start))

(-> conversation-compaction-portable-capture (conversation-compaction) conversation-compaction)
(defun conversation-compaction-portable-capture (capture)
  "Summarize older history while retaining a verbatim, protocol-valid recent tail.

Keep the latest actual user message even when the ordinary tail starts later in
its turn. The transcript plan expands the cut for correlated calls and private
reasoning. Original item identities, fixed durable cutoff and arrival precedence
are unchanged; this operation neither publishes nor executes anything."
  (let* ((source       (conversation-compaction--source-items capture))
         (view         (conversation-compaction-view capture))
         (raw-view     (conversation-compaction-summary-view view source))
         (projection   (conversation-projection raw-view))
         (suffix-plan  (clinker-transcript:make-compaction-plan
                        projection
                        :cutoff            (conversation-compaction--tail-start projection source)
                        :repaired-output-p #'conversation-compaction--repaired-output-p))
         (suffix       (clinker-transcript:compaction-plan-items suffix-plan))
         (latest-user
           (find-if (lambda (item)
                      (and (json-string= (json-get item "role") "user")
                           (not (conversation-compaction--checkpoint-summary-p projection item))))
                    source :from-end t))
         (tail         (if (and latest-user (not (member latest-user suffix :test #'eq)))
                           (cons latest-user suffix)
                           suffix))
         (tail-view    (conversation-compaction-summary-view view tail))
         (summary-view (conversation-compaction-summary-view
                        view (remove-if (lambda (item) (member item tail :test #'eq)) source))))
    (conversation-compaction--copy-metadata projection (conversation-projection tail-view) tail)
    (make-instance
     'conversation-compaction
     :conversation    (conversation-compaction--conversation capture)
     :view            summary-view
     :source-items    source
     :plan            (clinker-transcript:make-compaction-plan
                       (conversation-projection tail-view) :cutoff 0
                       :repaired-output-p                          #'conversation-compaction--repaired-output-p)
     :cutoff-sequence (conversation-compaction-cutoff-sequence capture))))
