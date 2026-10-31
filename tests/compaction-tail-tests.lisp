(in-package #:autolith)

;;;; -- Portable Compaction Tail --

(defun compaction-tail-tests--item (role content &key call-id)
  (let ((item (json-object "role" role "content" content)))
    (when call-id
      (setf (gethash "tool_call_id" item) call-id))
    item))

(defun compaction-tail-tests--capture (items)
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration :identifier "tail-test"))
         (view (conversation-compaction-summary-view conversation items))
         (projection (conversation-projection view))
         (plan (clinker-transcript:make-compaction-plan projection :cutoff 0)))
    (make-instance 'conversation-compaction
                   :conversation conversation :view view :source-items items
                   :plan plan :cutoff-sequence 0)))

(defun test-compaction-tail-preserves-latest-user-and-groups ()
  (let* ((user (compaction-tail-tests--item "user" "latest"))
         (call (json-object "type" "function_call" "id" "call-1" "name" "tool"))
         (output (json-object "type" "function_call_output" "call_id" "call-1" "output" "result"))
         (reasoning (json-object "type" "reasoning" "summary" "private"))
         (capture (compaction-tail-tests--capture
                   (list (compaction-tail-tests--item "user" (make-string 40000 :initial-element #\x))
                         user call output reasoning)))
         (tail (conversation-compaction-portable-capture capture))
         (items (clinker-transcript:compaction-plan-items (conversation-compaction--plan tail))))
    (test-assert (member user items :test #'eq) "latest user is retained verbatim")
    (test-assert (and (member call items :test #'eq) (member output items :test #'eq))
                 "call and output remain correlated")
    (test-assert (member reasoning items :test #'eq) "reasoning remains with the tail")))

(defun test-compaction-tail-preserves-arrival-identities ()
  (let* ((items (list (compaction-tail-tests--item "user" "old")
                      (compaction-tail-tests--item "user" "latest")))
         (capture (compaction-tail-tests--capture items))
         (tail (conversation-compaction-portable-capture capture)))
    (test-assert (eq items (conversation-compaction--source-items tail))
                 "portable capture keeps authoritative source identities")
    (test-assert (= (conversation-compaction-cutoff-sequence tail)
                    (conversation-compaction-cutoff-sequence capture))
                 "portable capture keeps the durable cutoff")))

(defun test-compaction-tail-does-not-promote-checkpoint-summary ()
  (let* ((summary (conversation-summary-item "old handoff"))
         (latest (compaction-tail-tests--item "user" "actual latest"))
         (items (list summary latest))
         (projection (conversation-projection
                      (clinker-transcript:make-projection :items items))))
    (setf (gethash summary
                   (clinker-transcript:projection-metadata-table projection ':checkpoint-summary))
          t)
    (test-assert (conversation-compaction--checkpoint-summary-p projection summary)
                 "checkpoint marker is metadata")
    (test-assert (not (conversation-compaction--checkpoint-summary-p projection latest))
                 "actual user is not marked as a checkpoint")))

(define-test-suite compaction-tail
  test-compaction-tail-preserves-latest-user-and-groups
  test-compaction-tail-preserves-arrival-identities
  test-compaction-tail-does-not-promote-checkpoint-summary)
