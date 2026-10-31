(in-package #:autolith)

;;;; -- Portable Compaction Tail --

(-> compaction-tail-tests--call (string) json-object)
(defun compaction-tail-tests--call (call-id)
  "Return a valid function call for the tail protocol fixtures."
  (json-object "type" "function_call" "call_id" call-id
               "name" "inspect" "namespace" "test" "arguments" "{}"))

(-> compaction-tail-tests--output (string string) json-object)
(defun compaction-tail-tests--output (call-id output)
  "Return the completed output correlated with CALL-ID."
  (json-object "type" "function_call_output" "call_id" call-id
               "output" output))

(-> compaction-tail-tests--reasoning (string) json-object)
(defun compaction-tail-tests--reasoning (text)
  "Return private reasoning that belongs to the current provider turn."
  (json-object "type" "reasoning" "summary" (json-array text)))

(-> compaction-tail-tests--items (conversation) list)
(defun compaction-tail-tests--items (conversation)
  "Return the current durable input projection as a list."
  (coerce (conversation-input-items conversation) 'list))

(-> test-compaction-tail-bounds-recent-user-and-expands-groups () null)
(defun test-compaction-tail-bounds-recent-user-and-expands-groups ()
  "Retain the latest user and expand a cut to complete protocol groups."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let ((conversation (conversation-create configuration :identifier "tail-bounds")))
      (conversation-append-user-message conversation (make-string 40000 :initial-element #\x))
      (conversation-append-user-message conversation "latest request")
      (conversation-append-provider-item conversation (compaction-tail-tests--call "tail-call"))
      (conversation-append-tool-result conversation "tail-call"
                                       :tool-name "test.inspect"
                                       :output "tail-output" :success-p t)
      (conversation-append-provider-item conversation
                                         (compaction-tail-tests--reasoning "private tail"))
      (let* ((capture (conversation-compaction-capture conversation))
             (tail (conversation-compaction-portable-capture capture))
             (items (clinker-transcript:compaction-plan-items
                     (conversation-compaction--plan tail))))
        (test-assert (test-object-contains-string-p items "latest request")
                     "the latest actual user message is retained")
        (test-assert (test-object-contains-string-p items "tail-call")
                     "the correlated function call is retained")
        (test-assert (test-object-contains-string-p items "tail-output")
                     "the correlated function output is retained")
        (test-assert (test-object-contains-string-p items "private tail")
                     "private reasoning is retained with the tail")
        (test-assert (not (test-object-contains-string-p items "xxxxxxxx"))
                     "the ordinary tail omits the oversized old request")))))

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
        (conversation-append-summary conversation "portable checkpoint" :compaction capture)
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
      (conversation-append-summary conversation "first checkpoint")
      (conversation-append-user-message conversation "second request")
      (conversation-append-summary conversation "second checkpoint")
      (let ((items (compaction-tail-tests--items conversation)))
        (test-assert (= 1 (length items))
                     "repeated compactions retain one projected handoff")
        (test-assert (test-object-contains-string-p items "second checkpoint")
                     "the latest portable summary is authoritative")
        (test-assert (not (test-object-contains-string-p items "first checkpoint"))
                     "a prior portable summary is not accumulated")))))

(define-test-suite compaction-tail
  test-compaction-tail-bounds-recent-user-and-expands-groups
  test-compaction-tail-carries-arrivals-through-reload
  test-compaction-tail-repeated-capture-does-not-accumulate-summaries)
