(in-package #:autolith)

;;;; -- Fullscreen Outcome Transactions --

(-> tool-outcome-test--call (t string) list)
(defun tool-outcome-test--call (identifier call-id)
  "Return a clickable pending invocation presentation."
  (list identifier
        (list (termdown:make-widget ':tool "▸ (probe " '(:copy "original call"))
              (terminal-span ':tool (format nil "~S)" call-id)))
        (list :kind ':call :call-key (list :call "owner" call-id) :state ':pending)))

(-> tool-outcome-test--result
    (t string &key (:state keyword) (:body list) (:job-id (option string))) list)
(defun tool-outcome-test--result (identifier call-id &key (state ':success) body job-id)
  "Return an outcome with independently controllable retained BODY and job identity."
  (list identifier nil
        (list :kind ':result :call-key (list :call "owner" call-id)
              :state state :body body :retain-output-p (not (null body))
              :job-key (and job-id (list :job "owner" job-id)))))

(-> tool-outcome-test--text (fullscreen-terminal-ui) string)
(defun tool-outcome-test--text (ui)
  "Return all resident committed text in append order."
  (let ((viewport (fullscreen-terminal-ui-viewport ui)))
    (with-output-to-string (output)
      (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
            do (write-string (clinedi:transcript-viewport-chunk-text viewport index) output)))))

(-> test-fullscreen-outcome-batch () null)
(defun test-fullscreen-outcome-batch ()
  "Replace interleaved invocations in one paint without losing their clickable arguments."
  (with-terminal-ui (ui (fullscreen-test--ui
                         (make-instance 'recording-terminal :columns 80 :rows 20)))
    (let ((paints 0)
          (paint  (fdefinition 'terminal-ui-fullscreen-paint)))
      (test-call-with-function-replacements
       (list (list 'terminal-ui-fullscreen-paint
                   (lambda (&rest arguments)
                     (incf paints)
                     (apply paint arguments))))
       (lambda ()
         (test-assert
          (= 4 (terminal-ui-present-records
                ui (list (tool-outcome-test--call 1 "first")
                         (tool-outcome-test--call 2 "second")
                         (tool-outcome-test--result 3 "second" :state ':failure
                                                    :body             (list (terminal-span ':plain
                                                                                           (format nil "~%  │ error"))))
                         (tool-outcome-test--result 4 "first"))))
          "a batch uses each call and its hidden outcome")
         (test-assert (= paints 1) "one publication paints once")))
      (test-assert (and (= (fullscreen-test--chunk-count ui) 2)
                        (search "✓ (probe \"first\")" (tool-outcome-test--text ui))
                        (search "! (probe \"second\")" (tool-outcome-test--text ui))
                        (search "  │ error" (tool-outcome-test--text ui)))
                   "outcomes replace their own original invocations")
      (test-assert (equal '(:copy "original call")
                          (terminal-ui-fullscreen--click-action ui 4 1))
                   "replacing a header preserves its widget action")
      (test-assert (zerop (terminal-ui-present-records
                           ui (list (tool-outcome-test--result 4 "first"))))
                   "completed record identities deduplicate")))
  nil)

(-> test-fullscreen-outcome-deferred-order () null)
(defun test-fullscreen-outcome-deferred-order ()
  "Publish mixed suspended output in order and associate tags with the queued chunks."
  (with-terminal-ui (ui (fullscreen-test--ui
                         (make-instance 'recording-terminal :columns 80 :rows 20)))
    (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "resident")))
    (setf (terminal-ui-live-output-suspended-p ui) t)
    (terminal-ui-append-finalized ui 2 "before")
    (terminal-ui-present-records ui (list (tool-outcome-test--call 3 "queued")))
    (terminal-ui-stream-update ui :rows (list (list (terminal-span ':plain "stream"))))
    (terminal-ui-append-finalized ui 4 "after")
    (terminal-ui-present-records ui (list (tool-outcome-test--result 5 "resident")
                                         (tool-outcome-test--result 6 "queued")))
    (test-assert (and (= (fullscreen-test--chunk-count ui) 1)
                      (search "▸ (probe" (tool-outcome-test--text ui)))
                 "suspended operations do not mutate resident chunks")
    (test-assert (zerop (terminal-ui-present-records
                         ui (list (tool-outcome-test--result 6 "queued"))))
                 "queued outcomes deduplicate before publication")
    (setf (terminal-ui-live-output-suspended-p ui) nil)
    (terminal-ui--present-live ui)
    (let* ((text   (tool-outcome-test--text ui))
           (before (search "before" text))
           (call   (search "✓ (probe \"queued\")" text))
           (stream (search "stream" text))
           (after  (search "after" text)))
      (test-assert (and before call stream after (< before call stream after)
                        (search "✓ (probe \"resident\")" text))
                   "mixed output retains chronology and both invocations complete"))
    (test-assert (null (fullscreen-terminal-ui-deferred-operations ui))
                 "successful publication drains suspended operations"))
  nil)

(-> test-fullscreen-outcome-retag () null)
(defun test-fullscreen-outcome-retag ()
  "Keep replacement identity valid through chunk retags and job reassignment."
  (let ((ui (fullscreen-test--ui (make-instance 'recording-terminal :columns 80))))
    (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "tagged")))
    (terminal-ui-mark-finalized ui 2)
    (terminal-ui-present-records ui (list (tool-outcome-test--result 3 "tagged")))
    (test-assert (and (= (fullscreen-test--chunk-count ui) 1)
                      (search "✓ (probe" (tool-outcome-test--text ui)))
                 "adding another finalized identity does not stale the replacement tag")
    (terminal-ui-present-records
     ui (list (tool-outcome-test--result 4 "tagged" :state ':pending :job-id "old")
              (tool-outcome-test--result 5 "tagged" :state ':pending :job-id "new")))
    (terminal-ui-present-records
     ui (list (list 6 nil (list :kind ':completion :job-key '(:job "owner" "old")
                                :state ':failure))))
    (test-assert (and (= 3 (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (search "▸ (probe" (tool-outcome-test--text ui)))
                 "job reassignment drops obsolete aliases and ignores their later completion")
    (terminal-ui-present-records
     ui (list (list 7 nil (list :kind ':completion :job-key '(:job "owner" "new")
                                :state ':success))))
    (test-assert (search "✓ (probe" (tool-outcome-test--text ui))
                 "the current job completes its invocation"))
  nil)

(-> test-fullscreen-outcome-publication-retry () null)
(defun test-fullscreen-outcome-publication-retry ()
  "Restore viewport and semantic bookkeeping after a failed paint, then retry once."
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 80 :rows 20))
         (ui       (fullscreen-test--ui terminal)))
    (with-terminal-ui (ui ui)
      (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "retry")))
      (let ((text   (tool-outcome-test--text ui))
            (record (gethash 1 (fullscreen-terminal-ui-presentation-records ui)))
            (result (tool-outcome-test--result
                     2 "retry" :state ':failure
                     :body            (list (terminal-span ':plain (format nil "~%  │ failed"))))))
        (setf (failing-recording-terminal-fail-next-write-p terminal) t)
        (test-assert
         (handler-case (progn (terminal-ui-present-records ui (list result)) nil)
           (terminal-error () t))
         "failed outcome publication propagates the terminal condition")
        (test-assert (and (string= text (tool-outcome-test--text ui))
                          (eq record (gethash 1 (fullscreen-terminal-ui-presentation-records ui)))
                          (not (terminal-ui--finalized-p ui 2))
                          (null (fullscreen-terminal-ui-deferred-operations ui)))
                     "failure restores original spans, aliases, deduplication and pending work")
        (terminal-ui-present-records ui (list result))
        (terminal-ui-append-finalized ui 3 "later")
        (test-assert (and (= (fullscreen-test--chunk-count ui) 2)
                          (search "! (probe" (tool-outcome-test--text ui))
                          (search "later" (tool-outcome-test--text ui)))
                     "replacement and append succeed after rollback"))))
  nil)

(-> test-fullscreen-outcome-eviction-and-reset () null)
(defun test-fullscreen-outcome-eviction-and-reset ()
  "Forget semantic aliases and associated result identities at the residency boundary."
  (let* ((*fullscreen-transcript-character-limit* 1000)
         (ui                                      (fullscreen-test--ui (make-instance 'recording-terminal :columns 80))))
    (terminal-ui-present-records
     ui (list (tool-outcome-test--call '(:conversation "owner" 1) "old")
              (tool-outcome-test--result '(:conversation "owner" 2) "old" :job-id "job")))
    (loop for sequence from 3 to 40
          do (terminal-ui-append-finalized ui (list :conversation "owner" sequence)
                                           (make-string 80 :initial-element #\x)))
    (test-assert (and (zerop (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (not (terminal-ui--finalized-p ui '(:conversation "owner" 1)))
                      (not (terminal-ui--finalized-p ui '(:conversation "owner" 2)))
                      (>= (terminal-ui-take-evicted-sequence ui "owner") 2))
                 "eviction removes call and job aliases together with their outcomes")
    (terminal-ui-present-records ui (list (tool-outcome-test--call 100 "new")))
    (setf (terminal-ui-live-output-suspended-p ui) t)
    (terminal-ui-present-records ui (list (tool-outcome-test--result 101 "new")))
    (terminal-ui-reset-transcript ui)
    (test-assert (and (zerop (fullscreen-test--chunk-count ui))
                      (zerop (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (null (fullscreen-terminal-ui-deferred-operations ui)))
                 "reset clears resident aliases and queued outcomes"))
  nil)
