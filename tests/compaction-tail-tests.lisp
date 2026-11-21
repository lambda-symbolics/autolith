(in-package #:autolith)

;;;; -- Portable Compaction Tail --

(-> compaction-tail-tests--call (string) json-object)
(defun compaction-tail-tests--call (call-id)
  "Return a valid function call for the tail protocol fixtures."
  (json-object "type" "function_call" "call_id" call-id
               "name" "inspect" "namespace" "test" "arguments" "{}"))

(-> compaction-tail-tests--reasoning (string) json-object)
(defun compaction-tail-tests--reasoning (text)
  "Return private reasoning that belongs to the current provider turn."
  (json-object "type" "reasoning" "summary" (json-array text)))

(-> compaction-tail-tests--items (conversation) list)
(defun compaction-tail-tests--items (conversation)
  "Return the current durable input projection as a list."
  (coerce (conversation-input-items conversation) 'list))

(-> test-compaction-tail-carries-arrivals-through-reload () null)
(defun test-compaction-tail-carries-arrivals-through-reload ()
  "Publish a captured tail with arrivals and verify the durable replay."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let ((conversation (conversation-create configuration :identifier "tail-arrivals")))
      (conversation-append-user-message conversation "captured request")
      (let ((capture (conversation-compaction-portable-capture
                      (conversation-compaction-capture conversation))))
        (conversation-append-provider-item conversation (compaction-tail-tests--call "late-call"))
        (conversation-append-tool-result conversation "late-call"
                                         :tool-name "test.inspect"
                                         :output "late output" :success-p t)
        (conversation-append-user-message conversation "late steering")
        (conversation-append-summary conversation (make-continuation :intent "portable checkpoint") :compaction capture)
        (dolist (projection (list conversation
                                  (conversation-load (conversation-pathname conversation))))
          (let ((items (compaction-tail-tests--items projection)))
            (test-assert (test-object-contains-string-p items "late output")
                         "late correlated output is carried into publication and replay")
            (test-assert (test-object-contains-string-p items "late steering")
                         "late user arrivals are carried into publication and replay")
            (test-assert (test-object-contains-string-p items "portable checkpoint")
                         "the portable checkpoint is present after replay")))))))

(-> test-compaction-tail-repeated-capture-does-not-accumulate-summaries () null)
(defun test-compaction-tail-repeated-capture-does-not-accumulate-summaries ()
  "Repeated checkpoints replace older summary state rather than nesting it."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let ((conversation (conversation-create configuration :identifier "tail-repeat")))
      (conversation-append-user-message conversation "first request")
      (conversation-append-summary conversation (make-continuation :intent "first checkpoint"))
      (conversation-append-user-message conversation "second request")
      (conversation-append-summary conversation (make-continuation :intent "second checkpoint"))
      (let ((items (compaction-tail-tests--items conversation)))
        (test-assert (= 1 (length items))
                     "repeated compactions retain one projected handoff")
        (test-assert (test-object-contains-string-p items "second checkpoint")
                     "the latest portable summary is authoritative")
        (test-assert (not (test-object-contains-string-p items "first checkpoint"))
                     "a prior portable summary is not accumulated"))
      (let ((loaded (conversation-load (conversation-pathname conversation))))
        (conversation-append-user-message loaded "third request")
        (conversation-append-summary
         loaded (make-continuation :intent "third checkpoint")
         :compaction (conversation-compaction-portable-capture
                      (conversation-compaction-capture loaded)))
        (test-assert (not (test-object-contains-string-p
                           (compaction-tail-tests--items loaded) "second checkpoint"))
                     "a checkpoint restored from disk is not carried into the next tail")))))

(-> test-compaction-tail-summarizes-displaced-evidence () null)
(defun test-compaction-tail-summarizes-displaced-evidence ()
  "Send current-turn evidence outside the bounded tail to the summarizer."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (conversation-append-user-message conversation "latest exact request")
      (conversation-append-provider-item conversation (compaction-tail-tests--call "large-call"))
      (conversation-append-tool-result
       conversation "large-call" :tool-name "test.inspect" :success-p nil
       :output (concatenate 'string "failed-effect-evidence" (make-string 40000 :initial-element #\x)))
      (let* ((capture (conversation-compaction-portable-capture
                       (conversation-compaction-capture conversation)))
             (summary (conversation-input-items (conversation-compaction-view capture)))
             (tail    (clinker-transcript:compaction-plan-items (conversation-compaction--plan capture))))
        (test-assert (test-object-contains-string-p tail "latest exact request")
                     "the latest user request is carried verbatim")
        (test-assert (test-object-contains-string-p (append summary tail) "failed-effect-evidence")
                     "displaced tool evidence is summarized or retained, never omitted"))))
  nil)
