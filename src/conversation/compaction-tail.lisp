(in-package #:autolith)

;;;; -- Verbatim Portable Compaction Tail --

(defparameter *conversation-compaction-tail-characters* 32768
  "Maximum ordinary recent-tail characters retained beside a portable checkpoint.

The current user request and protocol-required call/reasoning groups are retained
even when larger. Request admission checks the resulting projection before I/O.")

(defparameter *conversation-compaction-tail-user-turns* 2
  "Maximum recent user turns considered for the ordinary verbatim tail.")

(-> conversation-compaction--checkpoint-summary-p
    (clinker-transcript:projection json-object) boolean)
(defun conversation-compaction--checkpoint-summary-p (projection item)
  "Identify checkpoints by projection metadata rather than their prose."
  (not (null (gethash item (clinker-transcript:projection-metadata-table
                            projection ':checkpoint-summary)))))

(-> conversation-compaction-portable-capture (conversation-compaction) conversation-compaction)
(defun conversation-compaction-portable-capture (capture)
  "Checkpoint older history while retaining a verbatim, protocol-valid recent tail.

Keep the latest actual user message even when the ordinary tail starts later in
its turn. The transcript plan expands the cut for correlated calls and private
reasoning. Original item identities, fixed durable cutoff and arrival precedence
are unchanged; this operation neither publishes nor executes anything."
  (let* ((source     (conversation-compaction--source-items capture))
         (view       (conversation-compaction-view capture))
         (raw-view   (conversation-compaction-summary-view view source))
         (projection (conversation-projection raw-view)))
    (multiple-value-bind (summary-items tail-projection plan)
        (clinker-transcript:partition-compaction
         projection
         :maximum-size       *conversation-compaction-tail-characters*
         :maximum-user-turns *conversation-compaction-tail-user-turns*
         :measure            (lambda (item) (length (json-encode item)))
         :barrier-p          (lambda (item)
                               (or (conversation-compaction--checkpoint-summary-p projection item)
                                   (native-compaction-item-p item)))
         :pinned-p           (lambda (item)
                               (and (eq (item-role item) ':user)
                                    (not (conversation-compaction--checkpoint-summary-p projection item))))
         :repaired-output-p  #'conversation-compaction--repaired-output-p)
      (declare (ignore tail-projection))
      (make-instance
       'conversation-compaction
       :conversation    (conversation-compaction--conversation capture)
       :view            (conversation-compaction-summary-view view summary-items)
       :source-items    source
       :plan            plan
       :cutoff-sequence (conversation-compaction-cutoff-sequence capture)))))
