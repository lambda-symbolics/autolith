(in-package #:autolith)

(-> fullscreen-test--click (terminal-ui integer integer) (values t t))
(defun fullscreen-test--click (ui column row)
  "Press and release the left button at COLUMN and ROW of UI, and return what the release did."
  (terminal-ui-process-event ui (list ':click column row))
  (terminal-ui-process-event ui (list ':release column row)))

(-> fullscreen-test--top (fullscreen-terminal-ui) (option (integer 0)))
(defun fullscreen-test--top (ui)
  "Return UI's first visible transcript row, or NIL while it follows the tail."
  (clinedi:transcript-viewport-top (fullscreen-terminal-ui-viewport ui)))

(-> fullscreen-test--chunk-count (fullscreen-terminal-ui) (integer 0))
(defun fullscreen-test--chunk-count (ui)
  "Return how many output chunks UI's transcript holds."
  (clinedi:transcript-viewport-chunk-count (fullscreen-terminal-ui-viewport ui)))

(-> fullscreen-test--frame (fullscreen-terminal-ui) (option vector))
(defun fullscreen-test--frame (ui)
  "Return the display rows UI last painted, or NIL when the next paint is complete."
  (clinedi:frame-painter-frame (fullscreen-terminal-ui-painter ui)))

(-> fullscreen-test--row-text (fullscreen-terminal-ui integer) string)
(defun fullscreen-test--row-text (ui index)
  "Return the plain text of UI's committed transcript row INDEX."
  (clinedi:transcript-viewport-row-text (fullscreen-terminal-ui-viewport ui) index))

(-> test-terminal-compact-success-groups () null)
(defun test-terminal-compact-success-groups ()
  "Group mixed single-line tool results across append and batch boundaries."
  (let* ((edit     (list (terminal-span ':success "✓ resource.edit")
                         (terminal-span ':dim "  applied")))
         (check    (list (terminal-span ':success "✓ lisp.paren-check")))
         (custom   (list (terminal-span ':success "✓ extension.probe")))
         (body     (list (terminal-span ':success "✓ resource.read")
                         (terminal-span ':plain (format nil "~%  │ body"))))
         (failure  (list (terminal-span ':failure "✗ resource.read failed")))
         (call     (list (terminal-span ':plain "▸ (shell.run)")))
         (message  (list (terminal-span ':success "✓ goal complete")))
         (command  (list (terminal-span ':tool "▸ (shell.run)")
                         (terminal-span ':plain (format nil "~%  :command ls"))))
         (decision (list (terminal-span ':annotation "  auto-permission sandbox")))
         (entries  (list (list 0 edit '(:group :success)) (list 0 "duplicate")
                         (list 1 edit '(:group :success)) (list 2 check '(:group :success))
                         (list 3 custom '(:group :success)) (list 4 body)
                         (list 5 edit '(:group :success)) (list 6 failure)
                         (list 7 edit '(:group :success)) (list 8 call)
                         (list 9 edit '(:group :success)) (list 10 message)
                         (list 11 command '(:group :call))
                         (list 12 decision '(:group :attached)) (list 13 edit '(:group :success))))
         (expected (format nil "✓ resource.edit  applied~%✓ resource.edit  applied~%✓ lisp.paren-check~%✓ extension.probe~%~%✓ resource.read~%  │ body~%~%✓ resource.edit  applied~%~%✗ resource.read failed~%~%✓ resource.edit  applied~%~%▸ (shell.run)~%~%✓ resource.edit  applied~%~%✓ goal complete~%~%▸ (shell.run)~%  :command ls~%  auto-permission sandbox~%~%✓ resource.edit  applied~%")))
    (dolist (fullscreen-p '(nil t))
      (dolist (batch-p '(nil t))
        (let* ((terminal (make-instance 'recording-terminal :columns 100 :rows 24))
               (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p)))
          ;; Exercise durable noninteractive output and interactive deferred output.
          (dolist (deferred-p '(nil t))
            (setf (terminal-ui-live-output-suspended-p ui) deferred-p
                  (terminal-interactive-p terminal) deferred-p)
            (if batch-p
                (progn
                  (test-assert (= 4 (terminal-ui-append-finalized-batch ui (subseq entries 0 5)))
                               "batch counts only unique entries")
                  (terminal-ui-append-finalized-batch ui (subseq entries 5)))
                (dolist (pair entries)
                  (terminal-ui-append-finalized ui (first pair) (second pair)
                                                 :group (getf (third pair) :group))))
            (let ((actual
                    (if deferred-p
                        (terminal-ui-deferred-live-appended-text ui)
                        (if fullscreen-p
                            (format nil "~{~A~%~}"
                                    (loop for row below (clinedi:transcript-viewport-row-count
                                                         (fullscreen-terminal-ui-viewport ui))
                                          collect (fullscreen-test--row-text ui row)))
                            (clinedi:ansi-strip (recording-terminal-output terminal))))))
              (test-assert (string= (string-right-trim '(#\Newline) expected)
                                    (string-right-trim '(#\Newline) actual))
                           "only adjacent one-line tool successes share a block, and a call keeps its decision"))
            ;; A second run uses fresh identifiers and output, but the same UI.
            (clrhash (terminal-ui-finalized-identifiers ui))
            (setf (terminal-ui-deferred-live-appended-text ui) ""
                  (terminal-ui-deferred-live-appended-display ui) ""
                  (terminal-ui-deferred-live-appended-regions ui) nil))))))
  nil)

(-> test-fullscreen-transcript-residency () null)
(defun test-fullscreen-transcript-residency ()
  "Bound transcript and finalized identifiers while evicted records stay presentable again."
  (let ((*fullscreen-transcript-character-limit*  4000)
        (*terminal-ui-finalized-identifier-limit* 64))
    (dolist (fullscreen-p '(t nil))
      (let* ((terminal (make-instance 'recording-terminal :columns 60 :rows 12))
             (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
             (peak     0))
        (loop for sequence from 1 to 300
              do (terminal-ui-append-finalized
                  ui (list :conversation "c" sequence)
                  (format nil "message ~D ~A" sequence (make-string 40 :initial-element #\x)))
                 (when fullscreen-p
                   (setf peak (max peak (clinedi:transcript-viewport-resident-characters
                                         (fullscreen-terminal-ui-viewport ui))))))
        (test-assert (and (<= (hash-table-count (terminal-ui-finalized-identifiers ui)) 64)
                          (= 64 (deque-count (terminal-ui-finalized-order ui)))
                          (terminal-ui--finalized-p ui (list :conversation "c" 300))
                          (not (terminal-ui--finalized-p ui (list :conversation "c" 1))))
                     "only recent finalized identifiers stay remembered")
        (if fullscreen-p
            (let ((evicted (terminal-ui-take-evicted-sequence ui "c")))
              (test-assert (and (<= peak 4000)
                                (plusp (clinedi:transcript-viewport-evicted-count
                                        (fullscreen-terminal-ui-viewport ui))))
                           "resident transcript characters plateau at the limit")
              (test-assert (and (integerp evicted)
                                (< evicted 300)
                                (not (terminal-ui--finalized-p
                                      ui (list :conversation "c" evicted))))
                           "eviction forgets its records and reports the newest one")
              (test-assert (null (terminal-ui-take-evicted-sequence ui "c"))
                           "taking the evicted sequence clears it")
              (test-assert (terminal-ui-append-finalized ui (list :conversation "c" evicted)
                                                         "presented again")
                           "an evicted record can be presented again"))
            (test-assert (null (terminal-ui-take-evicted-sequence ui "c"))
                         "native scrollback evicts nothing")))))
  nil)

(-> test-terminal-compact-success-stream-and-retry () null)
(defun test-terminal-compact-success-stream-and-retry ()
  "Separate live/streamed output and restore compact grouping after failed appends."
  (let ((entry (list (terminal-span ':success "✓ extension.probe"))))
    (dolist (fullscreen-p '(nil t))
      (let* ((terminal (make-instance 'failing-recording-terminal :columns 100 :rows 24))
             (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p)))
        (with-terminal-ui (ui ui)
          (terminal-ui-append-finalized ui 0 entry :group ':success)
          (let ((count  (hash-table-count (terminal-ui-finalized-identifiers ui)))
                (before (terminal-ui-compact-tail-group ui)))
            (terminal-ui--finalized-content ui "capture only")
            (test-assert (and (eql before (terminal-ui-compact-tail-group ui))
                              (= count (hash-table-count (terminal-ui-finalized-identifiers ui))))
                         "capture formatting has no transcript side effects"))
          (test-assert (not (terminal-ui-append-finalized ui 0 "ignored"))
                       "duplicate result does not break a group")
          (terminal-ui-stream-update ui :tail "unfinished")
          (test-assert (null (first (terminal-ui--live-prefix-rows ui)))
                       "live output has a gap after compact results")
          (setf (failing-recording-terminal-fail-next-write-p terminal) t)
          (test-assert
           (handler-case (progn (terminal-ui-append-finalized-batch
                                 ui (list (list 1 entry '(:group :success)) (list 2 "boundary"))) nil)
             (terminal-error () t))
           "append error is propagated")
          (test-assert (and (eq (terminal-ui-compact-tail-group ui) ':success)
                            (not (gethash 1 (terminal-ui-finalized-identifiers ui)))
                            (not (gethash 2 (terminal-ui-finalized-identifiers ui))))
                       "failed batch restores grouping state and identifiers")
          (setf (terminal-ui-live-output-suspended-p ui) t)
          (terminal-ui-append-finalized ui 1 entry :group ':success)
          (terminal-ui-stream-update ui :rows (list (list (terminal-span ':plain "stream")))
                                     :tail    "tail")
          (terminal-ui-stream-update ui :rows (list (list (terminal-span ':plain "next"))))
          (test-assert (string= (terminal-ui-deferred-live-appended-text ui)
                                (format nil "✓ extension.probe~%~%stream~%next~%"))
                       "stream starts after a gap and subsequent chunks are contiguous")
          (setf (terminal-ui-live-output-suspended-p ui) nil)
          (when fullscreen-p
            (terminal-ui-append-finalized ui 3 entry :group ':success)
            (terminal-ui-reset-transcript ui)
            (terminal-ui-append-finalized ui 3 "fresh")
            (test-assert (string= "fresh" (fullscreen-test--row-text ui 0))
                         "clear resets compact separation and result identities")
            (terminal-ui-reset-transcript ui)
            (terminal-ui-append-finalized ui 0 entry :group ':success)
            (let ((actions nil))
              (setf (terminal-ui-action-function ui)
                    (lambda (action) (push action actions)))
              (terminal-ui-append-finalized-batch
               ui (list (list 1 entry '(:group :success))
                        (list 2 (list (termdown:make-widget
                                       ':code-copy "copy" '(:copy "boundary"))))))
              (fullscreen-test--click ui 1 4)
              (test-assert (equal '(:copy "boundary") (first actions))
                           "batch widgets retain their regions across the inserted gap")
              (terminal-ui-reset-transcript ui)
              (terminal-ui-append-finalized ui 0 entry :group ':success)
              (terminal-ui-append-finalized-batch
               ui (list (list 2 (list (termdown:make-widget
                                       ':code-copy "copy" '(:copy "external"))))))
              (fullscreen-test--click ui 1 3)
              (test-assert (equal '(:copy "external") (first actions))
                           "leading batch gap shifts widget regions with the text")))))))
  nil)

(-> test-terminal-compact-call-groups () null)
(defun test-terminal-compact-call-groups ()
  "Group exact call names across append boundaries in both transcript modes."
  (let* ((call            (list (terminal-span ':tool "▸ (resource.read)")))
         (inline          (list (terminal-span ':tool "▸ (resource.read")
                                (terminal-span ':plain " ")
                                (terminal-span ':syntax-keyword ":uri")
                                (terminal-span ':plain " workspace:x")
                                (terminal-span ':tool ")")))
         (other           (list (terminal-span ':tool "▸ (resource.edit)")))
         (success         (list (terminal-span ':success "✓ resource.read")))
         (body            (append call (list (terminal-span ':plain (format nil "~%  │ body")))))
         (clipped         (list (terminal-span ':tool "▸ (resource.re…")))
         (mcp             (list (terminal-span ':tool "▸ (MCP server / read)")))
         (mcp-other       (list (terminal-span ':tool "▸ (MCP server / write)")))
         (handoff         (list (terminal-span ':notice "↪ shell.run")
                                (terminal-span ':dim "  running in background · exec:16")))
         (handoff-next    (list (terminal-span ':notice "↪ shell.run")
                                (terminal-span ':dim "  running in background · ")
                                (termdown:make-widget ':code "(job.cancel :id exec:17)"
                                                      '(:insert "(job.cancel :id \"exec:17\")"))))
         (handoff-other   (list (terminal-span ':notice "↪ lisp.eval")))
         (handoff-body    (append handoff
                                  (list (terminal-span ':plain (format nil "~%  │ task")))))
         (handoff-clipped (list (terminal-span ':notice "↪ shell.r…")))
         (handoff-failed  (list (terminal-span ':failure "↪ shell.run")))
         (entries         (list call inline call other other success success call
                                clipped call body call mcp mcp mcp-other
                                handoff handoff-next handoff-other handoff-body
                                handoff handoff-failed handoff-clipped handoff-clipped))
         (groups          '("resource.read" "resource.read" "resource.read"
                            "resource.edit" "resource.edit" :success :success
                            "resource.read" nil "resource.read" :call "resource.read"
                            "MCP server / read" "MCP server / read" "MCP server / write"
                            "handoff shell.run" "handoff shell.run" "handoff lisp.eval"
                            nil "handoff shell.run" nil nil nil))
         (pairs           (loop for entry in entries for group in groups for index from 0
                                collect (list index entry (list :group group))))
         (expected        (format nil "▸ (resource.read)~%▸ (resource.read :uri workspace:x)~%▸ (resource.read)~%~%▸ (resource.edit)~%▸ (resource.edit)~%~%✓ resource.read~%✓ resource.read~%~%▸ (resource.read)~%~%▸ (resource.re…~%~%▸ (resource.read)~%~%▸ (resource.read)~%  │ body~%~%▸ (resource.read)~%~%▸ (MCP server / read)~%▸ (MCP server / read)~%~%▸ (MCP server / write)~%~%↪ shell.run  running in background · exec:16~%↪ shell.run  running in background · (job.cancel :id exec:17)~%~%↪ lisp.eval~%~%↪ shell.run  running in background · exec:16~%  │ task~%~%↪ shell.run  running in background · exec:16~%~%↪ shell.run~%~%↪ shell.r…~%~%↪ shell.r…")))
    (dolist (fullscreen-p '(nil t))
      (dolist (deferred-p '(nil t))
        (dolist (batch-p '(nil t))
          (let* ((terminal (make-instance 'recording-terminal :columns 100 :rows 24))
                 (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p)))
            (setf (terminal-ui-live-output-suspended-p ui) deferred-p
                  (terminal-interactive-p terminal) deferred-p)
            (if batch-p
                (progn
                  (terminal-ui-append-finalized ui 0 call :group "resource.read")
                  (terminal-ui-append-finalized-batch ui (subseq pairs 0 4))
                  (terminal-ui-append-finalized-batch ui (subseq pairs 4 6))
                  (terminal-ui-append-finalized-batch ui (subseq pairs 6)))
                (dolist (pair pairs)
                  (terminal-ui-append-finalized ui (first pair) (second pair)
                                                 :group (getf (third pair) :group))))
            (let ((actual
                    (if deferred-p
                        (terminal-ui-deferred-live-appended-text ui)
                        (if fullscreen-p
                            (format nil "~{~A~%~}"
                                    (loop for row below (clinedi:transcript-viewport-row-count
                                                         (fullscreen-terminal-ui-viewport ui))
                                          collect (fullscreen-test--row-text ui row)))
                            (clinedi:ansi-strip (recording-terminal-output terminal))))))
              (test-assert (string= expected (string-right-trim '(#\Newline) actual))
                           "same-tool calls share rows, while changes and bodies separate groups")))))))
  (dolist (fullscreen-p '(nil t))
    (let* ((terminal (make-instance 'failing-recording-terminal :columns 100 :rows 24))
           (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
           (call     (list (terminal-span ':tool "▸ (resource.read)"))))
      (with-terminal-ui (active ui)
        (terminal-ui-append-finalized active 0 call :group "resource.read")
        (setf (failing-recording-terminal-fail-next-write-p terminal) t)
        (test-assert
         (handler-case
             (progn (terminal-ui-append-finalized-batch active (list (list 1 call '(:group "resource.read")) (list 2 "body")))
                    nil)
           (terminal-error () t))
         "a failed call batch reports its terminal error")
        (test-assert
         (and (equal "resource.read" (terminal-ui-compact-tail-group active))
              (not (gethash 1 (terminal-ui-finalized-identifiers active)))
              (not (gethash 2 (terminal-ui-finalized-identifiers active))))
         "failed appends restore the exact call group and identifiers")
        (setf (terminal-ui-live-output-suspended-p active) t)
        (terminal-ui-append-finalized active 1 call :group "resource.read")
        (terminal-ui-stream-update active :rows (list (list (terminal-span ':plain "stream"))))
        (test-assert
         (string= (terminal-ui-deferred-live-appended-text active)
                  (format nil "▸ (resource.read)~%~%stream~%"))
         "streamed output separates from compact calls")
        (when fullscreen-p
          (setf (terminal-ui-live-output-suspended-p active) nil)
          (terminal-ui-reset-transcript active)
          (terminal-ui-append-finalized active 0 call :group "resource.read")
          (test-assert (string= "▸ (resource.read)"
                                (fullscreen-test--row-text active 0))
                       "clearing a compact call group starts a fresh transcript")))))
  nil)

;;;; -- Fullscreen Behavior --

(-> fullscreen-test--ui (terminal) terminal-ui)
(defun fullscreen-test--ui (terminal)
  "Construct a fullscreen UI around a deterministic terminal."
  (terminal-ui-create :terminal terminal :fullscreen-p t :prompt "> "))

(-> test-terminal-fullscreen-viewport () null)
(defun test-terminal-fullscreen-viewport ()
  "Exercise pinned geometry, scrolling, streaming, deduplication and width reflow."
  (let ((terminal (make-instance 'recording-terminal :columns 32 :rows 12)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (terminal-ui-set-input ui "draft")
      (terminal-ui-append-finalized-batch
       ui (loop for index below 30 collect (list index (format nil "row ~D: alpha beta gamma delta" index))))
      (multiple-value-bind (frame row column) (terminal-ui-fullscreen--frame ui nil)
        (test-assert (= (length frame) 12) "frame fills the complete height")
        (test-assert (= row 10) "composer is pinned above its bottom spacer")
        (test-assert (= column 7) "composer cursor uses the prompt and draft")
        (test-assert (search "draft" (clinedi:ansi-strip (nth row frame))) "draft is visible"))
      (terminal-ui-process-event ui ':page-up)
      (let ((top (fullscreen-test--top ui)))
        (test-assert (integerp top) "page-up leaves tail-follow mode")
        (terminal-ui-append-finalized ui ':later "new output")
        (terminal-ui-stream-update ui :tail "still streaming")
        (test-assert (= top (fullscreen-test--top ui)) "streaming preserves the scrolled position")
        (let* ((text       (fullscreen-test--row-text ui top))
               (first-word (subseq text 0 (min 3 (length text)))))
          (terminal-ui-resize ui 17 :rows 12)
          (test-assert (and (integerp (fullscreen-test--top ui))
                            (search first-word
                                    (fullscreen-test--row-text ui (fullscreen-test--top ui))))
                       "resize keeps the scrolled source line on top")))
      (let* ((viewport (fullscreen-terminal-ui-viewport ui))
             (rows     (clinedi:transcript-viewport-row-count viewport))
             (count    (clinedi:transcript-viewport-chunk-count viewport)))
        (terminal-ui--paint-live ui)
        (test-assert (= rows (clinedi:transcript-viewport-row-count viewport))
                     "redraw adds no transcript rows")
        (test-assert (not (terminal-ui-append-finalized ui ':later "duplicate")) "finalized ids deduplicate")
        (test-assert (= count (clinedi:transcript-viewport-chunk-count viewport))
                     "deduplication does not extend history"))
      (terminal-ui-process-event ui ':scroll-bottom)
      (test-assert (null (fullscreen-test--top ui)) "Ctrl-End follows the tail")
      (terminal-ui-process-event ui '(:scroll -1))
      (test-assert (integerp (fullscreen-test--top ui)) "wheel-up scrolls history")
      (test-assert (string= "draft" (line-editor-text (terminal-ui-editor ui))) "scrolling preserves the draft")))
  nil)

(-> test-terminal-fullscreen-pinned-activities () null)
(defun test-terminal-fullscreen-pinned-activities ()
  "Keep live chrome beside the draft while unfinished output scrolls with history."
  (let* ((terminal (make-instance 'recording-terminal :columns 100 :rows 36))
         (markers  '("REPL" "exec:1" "exec:2" "review-3" "receiving response"
                     "preview-live" "notice-live" "steer-live" "queued-live")))
    (with-terminal-ui (ui (terminal-ui-create :terminal terminal :fullscreen-p t
                                             :prompt "> " :clock-function (constantly 10)))
      (terminal-ui-set-input ui "draft")
      (terminal-ui-append-finalized-batch
       ui (loop for index below 60 collect (list index (format nil "history ~D" index))))
      (terminal-ui-set-status ui "receiving response")
      (terminal-ui-set-command-activities
       ui (list (list :id "exec:1" :type ':tool :index 1 :tool "shell.run"
                      :description "Build the release" :state ':running
                      :duration-ms 1000 :detached t)
                (list :id "exec:2" :type ':tool :index 2 :tool "rlm.infer"
                      :description "Review source" :state ':running
                      :duration-ms 1000 :detached t)))
      (terminal-ui-set-agent-activities
       ui (list (list :id "review-3" :index 3 :agent "reviewer" :state ':running
                      :current-tool "resource.read" :current-tool-duration-ms 1000
                      :recent-tools nil :request-count 1 :duration-ms 1000
                      :assignment "Review the patch." :detached t)))
      (terminal-ui-set-preview-rows ui (list (list (terminal-span ':plain "preview-live"))))
      (terminal-ui-set-notice ui "notice-live" :duration-seconds 100)
      (terminal-ui-set-pending-inputs ui '("steer-live") '("queued-live"))
      (terminal-ui-stream-update ui :tail "unfinished-output")
      (labels ((frame ()
                 (mapcar #'clinedi:ansi-strip (terminal-ui-fullscreen--frame ui 10)))

               (positions (rows)
                 (loop for marker in markers
                       collect (position-if (lambda (row) (search marker row)) rows))))
        (let* ((initial   (frame))
               (pinned    (positions initial))
               (input-row (position-if (lambda (row) (search "draft" row)) initial)))
          (test-assert (every #'integerp pinned) "all active chrome fits above the draft")
          (test-assert (some (lambda (row) (search "unfinished-output" row)) initial)
                       "tail-follow mode shows unfinished transcript output")
          (dolist (event '(:page-up (:scroll -1) :scroll-top :page-down :scroll-bottom))
            (terminal-ui-process-event ui event)
            (let ((rows (frame)))
              (test-assert (equal pinned (positions rows))
                           "history navigation leaves each live chrome row beside the draft")
              (test-assert (search "draft" (nth input-row rows))
                           "navigation keeps the input cursor row")))
          (terminal-ui-process-event ui ':scroll-top)
          (test-assert (notany (lambda (row) (search "unfinished-output" row)) (frame))
                       "unfinished transcript output scrolls away rather than joining the pinned chrome")
          (let ((top (fullscreen-test--top ui)))
            (terminal-ui-stream-update ui :tail "new unfinished output")
            (terminal-ui-append-finalized ui ':later "new committed output")
            (test-assert (and (= top (fullscreen-test--top ui))
                              (equal pinned (positions (frame))))
                         "new output preserves both the history anchor and pinned chrome"))
          (dolist (row pinned)
            (test-assert (null (terminal-ui-fullscreen--click-action ui 1 (1+ row)))
                         "pinned chrome has no transcript click target")))
        (terminal-ui-set-input ui (format nil "猫 draft~%second line"))
        (dolist (size '((20 8) (1 1) (5 3) (80 18)))
          (destructuring-bind (width height) size
            (terminal-ui-resize ui width :rows height)
            (multiple-value-bind (rows row column) (terminal-ui-fullscreen--frame ui 10)
              (test-assert (and (= (length rows) height)
                                (<= 0 row) (< row height)
                                (<= 0 column) (< column width)
                                (every (lambda (display)
                                         (<= (clinedi:ansi-display-width display) width))
                                       rows))
                           "busy chrome and a multiline draft fit a resized frame")))))))
  nil)

(-> test-terminal-fullscreen-small-windows () null)
(defun test-terminal-fullscreen-small-windows ()
  "Keep Unicode drafts and the cursor inside resized, even one-cell, viewports."
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (terminal-ui-set-input ui (format nil "猫 e~C abcdefghijklmnopqrstuvwxyz~%last" (code-char #x301)))
      (dolist (width '(1 2 7 12 40))
        (dolist (height '(1 2 3 8 24))
          (terminal-ui-resize ui width :rows height)
          (multiple-value-bind (frame row column) (terminal-ui-fullscreen--frame ui nil)
            (test-assert (= (length frame) height) "every frame covers its viewport")
            (test-assert (and (<= 0 row) (< row height)) "cursor row fits")
            (test-assert (and (<= 0 column) (< column width)) "cursor column fits")
            (test-assert (every (lambda (display) (<= (clinedi:ansi-display-width display) width)) frame)
                         "wide and combining glyphs do not overrun a row"))))))
  nil)

(-> test-terminal-fullscreen-flush-wrap-cursor () null)
(defun test-terminal-fullscreen-flush-wrap-cursor ()
  "Hide the space at a flush wrap and keep the composer cursor on its character."
  (let* ((draft    "aaaa bbbb of the string")
         (hanging  (position #\Space draft :start 10))
         (terminal (make-instance 'recording-terminal :columns 14 :rows 12)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (terminal-ui-set-input ui draft)
      (loop for cursor downfrom (length draft) to 0
            do (multiple-value-bind (frame row column) (terminal-ui-fullscreen--frame ui nil)
                 (let* ((line     (clinedi:ansi-strip (nth row frame)))
                        (shown    (if (< column (length line)) (char line column) #\Space))
                        (expected (cond ((= cursor (length draft))
                                         #\Space)
                                        ((= cursor hanging)
                                         (char draft (1+ cursor)))
                                        (t
                                         (char draft cursor)))))
                   (when (= cursor (length draft))
                     (test-assert (search "aaaa bbbb of" (clinedi:ansi-strip (nth (1- row) frame)))
                                  "the flush row keeps its last word")
                     (test-assert (eql 0 (search "the string" line))
                                  "the continuation row hides the hanging space"))
                   (test-assert (char= expected shown)
                                (format nil "cursor ~D sits on its character" cursor))))
               (terminal-ui-process-event ui ':left))))
  nil)

(-> test-terminal-live-theme () null)
(defun test-terminal-live-theme ()
  "Apply theme settings to live normal and fullscreen terminals in both directions."
  (let ((*terminal-theme*       *terminal-theme*)
        (*terminal-style-table* *terminal-style-table*))
    (dolist (fullscreen-p '(nil t))
      (with-test-configuration (configuration root)
        (declare (ignore root))
        (terminal-theme-install ':autolith)
        (let* ((terminal    (make-instance 'recording-terminal :columns 60 :rows 12 :styled-p t))
               (ui          (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
               (application (make-instance 'application :configuration configuration :ui ui)))
          (with-terminal-ui (active ui)
            (terminal-ui-set-input active "(settings :terminal-theme)")
            (terminal-ui-append-finalized active ':history "committed history")
            (let ((viewport (when fullscreen-p (fullscreen-terminal-ui-viewport ui))))
              (dolist (name '(:almighty :autolith))
                (let ((old-prompt (terminal-ui-prompt-render-cache ui))
                      (old-theme  *terminal-theme*)
                      (theme      (terminal-theme-find name)))
                  (setf (recording-terminal-chunks terminal) nil)
                  (application-apply-setting application ':terminal-theme name)
                  (test-assert (and (eq *terminal-theme* theme)
                                    (eq *terminal-style-table* (terminal-theme-style-table theme))
                                    (eq (config :terminal-theme configuration) name))
                               "a setting installs and persists the selected styles")
                  (test-assert (and (terminal-ui-prompt-render-cache ui)
                                    (not (eq old-prompt (terminal-ui-prompt-render-cache ui))))
                               "the unchanged editor draft is rendered under the new theme")
                  (let ((output (recording-terminal-output terminal)))
                    (test-assert (search "settings" (clinedi:ansi-strip output))
                                 "changing the setting immediately repaints the live editor")
                    (test-assert (search (terminal-style-sequence ':brand) output)
                                 "the repaint contains the new prompt styling")
                    (test-assert (search (terminal-theme-enter-sequence theme) output)
                                 "custom defaults are applied on theme entry")
                    (test-assert (search (terminal-theme-leave-sequence old-theme) output)
                                 "the previous theme's custom defaults are restored")
                    (when fullscreen-p
                      (test-assert (= (terminal-rows terminal)
                                      (terminal-tests--substring-count
                                       (format nil "~C[2K" #\Escape) output))
                                   "theme changes repaint every physical row")))
                  (when fullscreen-p
                    (test-assert (and (eq viewport (fullscreen-terminal-ui-viewport ui))
                                      (= 1 (fullscreen-test--chunk-count ui)))
                                 "theme changes preserve the committed viewport"))))
              (application-apply-setting application ':terminal-theme ':almighty)
              (setf (recording-terminal-chunks terminal) nil)))
          (test-assert (search (terminal-theme-leave-sequence (terminal-theme-find ':almighty))
                               (recording-terminal-output terminal))
                       "stopping either presentation restores imposed default colors")))))
  nil)

(-> test-terminal-theme-defaults () null)
(defun test-terminal-theme-defaults ()
  "Acquire and restore theme defaults across startup, attachment and styled-output boundaries."
  (let ((*terminal-theme*       *terminal-theme*)
        (*terminal-style-table* *terminal-style-table*))
    (terminal-theme-install ':almighty)
    (dolist (fullscreen-p '(nil t))
      (dolist (styled-p '(nil t))
        (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12 :styled-p styled-p))
               (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
               (enter    (terminal-theme-enter-sequence *terminal-theme*))
               (leave    (terminal-theme-leave-sequence *terminal-theme*)))
          (with-terminal-ui (active ui)
            (test-assert (eql styled-p (not (null (search enter (recording-terminal-output terminal)))))
                         "only styled terminals impose the startup theme's default colors")
            (setf (recording-terminal-chunks terminal) nil
                  (terminal-interactive-p terminal) nil)
            (terminal-ui-detach active)
            (test-assert (eql styled-p (not (null (search leave (recording-terminal-output terminal)))))
                         "detachment restores only the defaults the UI owns")
            (setf (recording-terminal-chunks terminal) nil
                  (terminal-interactive-p terminal) t)
            (terminal-ui-resize active 50 :rows 14)
            (test-assert (eql styled-p (not (null (search enter (recording-terminal-output terminal)))))
                         "attachment repaint reacquires the selected defaults")
            (setf (recording-terminal-chunks terminal) nil))
          (test-assert (eql styled-p (not (null (search leave (recording-terminal-output terminal)))))
                       "stopping restores owned defaults after attachment")))))
  nil)

(-> test-terminal-theme-write-failure () null)
(defun test-terminal-theme-write-failure ()
  "Retain a persisted theme and cleanup ownership when its first terminal write fails."
  (let ((*terminal-theme*       *terminal-theme*)
        (*terminal-style-table* *terminal-style-table*))
    (with-test-configuration (configuration root)
      (declare (ignore root))
      (terminal-theme-install ':autolith)
      (let* ((terminal    (make-instance 'failing-recording-terminal :columns 40 :rows 12 :styled-p t))
             (ui          (fullscreen-test--ui terminal))
             (application (make-instance 'application :configuration configuration :ui ui)))
        (with-terminal-ui (active ui)
          (setf (failing-recording-terminal-fail-next-write-p terminal) t)
          (test-assert
           (handler-case
               (progn (application-apply-setting application ':terminal-theme ':almighty) nil)
             (terminal-error () t))
           "theme output failures propagate")
          (test-assert (and (eq (config :terminal-theme configuration) ':almighty)
                            (eq *terminal-theme* (terminal-theme-find ':almighty))
                          (null (fullscreen-test--frame active))
                          (not (terminal-ui-theme-defaults-current-p active)))
                     "a failed paint retains the selected logical theme and invalidates the frame")
         (terminal-ui-resize active 42 :rows 12)
         (test-assert (and (fullscreen-test--frame active)
                           (terminal-ui-theme-defaults-current-p active))
                      "the next repaint retries default colors and the frame"))))
    (terminal-theme-install ':almighty)
    (let* ((terminal (make-instance 'failing-recording-terminal :columns 40 :rows 12 :styled-p t))
           (ui       (fullscreen-test--ui terminal)))
      (terminal-ui-start ui)
      (unwind-protect
           (progn
             (setf (recording-terminal-chunks terminal) nil
                   (failing-recording-terminal-fail-next-write-p terminal) t)
             (test-assert (handler-case (progn (terminal-ui-stop ui) nil)
                            (terminal-error () t))
                          "default-color restore failures propagate")
             (test-assert (and (not (fullscreen-terminal-ui-active-p ui))
                               (search (alternate-screen-leave-sequence)
                                       (recording-terminal-output terminal)))
                          "a failed default restore still releases the alternate screen")
             (terminal-ui-stop ui)
             (test-assert (null (terminal-ui-default-color-theme ui))
                          "a later stop retries the retained default-color ownership"))
        (terminal-ui-stop ui)))
    (let* ((palette           (terminal-theme-find ':almighty))
           (foreground        (make-terminal-theme
                               :name        ':foreground-only
                               :style-table (terminal-theme-style-table palette)
                               :foreground  (terminal-theme-foreground palette)))
           (background        (make-terminal-theme
                               :name        ':background-only
                               :style-table (terminal-theme-style-table palette)
                               :background  (terminal-theme-background palette)))
           (*terminal-themes* (append (list foreground background) *terminal-themes*)))
      (dolist (pair (list (list foreground background) (list background foreground)))
        (terminal-theme-install (terminal-theme-name (first pair)))
        (let* ((terminal (make-instance 'failing-recording-terminal :columns 40 :rows 12 :styled-p t))
               (ui       (fullscreen-test--ui terminal)))
          (with-terminal-ui (active ui)
            (setf (failing-recording-terminal-fail-next-write-p terminal) t)
            (test-assert
             (handler-case
                 (progn (terminal-ui-set-theme active (terminal-theme-name (second pair))) nil)
               (terminal-error () t))
             "a partial transition between independently specified defaults propagates its failure")
            (setf (recording-terminal-chunks terminal) nil)
            (terminal-ui-stop active)
            (test-assert
             (every (lambda (theme)
                      (search (terminal-theme-leave-sequence theme)
                              (recording-terminal-output terminal)))
                    pair)
             "cleanup restores every default color the interrupted transition could have imposed"))))))
  nil)

(-> test-terminal-fullscreen-failure-and-lifecycle () null)
(defun test-terminal-fullscreen-failure-and-lifecycle ()
  "Roll back failed transcript paints and release an owned alternate screen exactly once."
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 40 :rows 12))
         (ui       (fullscreen-test--ui terminal)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (terminal-ui-start ui)
           (setf (failing-recording-terminal-fail-next-write-p terminal) t)
           (test-assert
            (handler-case (progn (terminal-ui-append-finalized ui ':retry "hello") nil)
              (terminal-error () t)) "paint failure is propagated")
           (test-assert (zerop (fullscreen-test--chunk-count ui)) "failed append restores history")
           (test-assert (terminal-ui-append-finalized ui ':retry "hello") "failed append can be retried")
           (test-assert (= 1 (fullscreen-test--chunk-count ui)) "retry commits once"))
      (terminal-ui-stop ui))
    (let ((count (length (recording-terminal-chunks terminal))))
      (terminal-ui-stop ui)
      (test-assert (= count (length (recording-terminal-chunks terminal))) "repeated stop emits no control bytes"))
    (test-assert (not (fullscreen-terminal-ui-active-p ui)) "alternate ownership released")
    (let* ((output  (recording-terminal-output terminal))
           (enter   (search (alternate-screen-enter-sequence) output))
           (enable  (search (terminal-keyboard-enhancement-enable-sequence) output))
           (disable (search (terminal-keyboard-enhancement-disable-sequence) output))
           (leave   (search (alternate-screen-leave-sequence) output)))
      (test-assert (and enter enable disable leave (< enter enable disable leave))
                   "keyboard protocol is acquired and restored inside the alternate buffer")
      (test-assert (= 1 (terminal-tests--substring-count
                         (terminal-keyboard-enhancement-enable-sequence) output))
                   "repeated startup pushes one alternate keyboard mode")))
  (let* ((terminal (make-instance 'recording-terminal :columns 30 :rows 10))
         (ui       (fullscreen-test--ui terminal)))
    (with-terminal-ui (active ui)
      (terminal-ui-append-finalized active ':before "before detach")
      (setf (terminal-interactive-p terminal) nil)
      (terminal-ui-detach active)
      (terminal-ui-append-finalized active ':detached "while detached")
      (test-assert (not (fullscreen-terminal-ui-active-p active)) "detached output does not reenter")
      (setf (terminal-interactive-p terminal) t)
      (terminal-ui-resize active 40 :rows 12)
      (test-assert (fullscreen-terminal-ui-active-p active) "attachment resize reenters fullscreen")
      (test-assert (= 2 (fullscreen-test--chunk-count active)) "reattach retains transcript"))
    (let ((output (recording-terminal-output terminal)))
      (dolist (sequence (list (terminal-keyboard-enhancement-enable-sequence)
                             (terminal-keyboard-enhancement-disable-sequence)))
        (test-assert (= 2 (terminal-tests--substring-count sequence output))
                     "detach and reattach balance alternate keyboard modes"))))
  nil)

(-> test-terminal-fullscreen-relayed-wheel () null)
(defun test-terminal-fullscreen-relayed-wheel ()
  "Decode framed remote wheel packets and scroll the viewport across reattachment."
  (let* ((terminal   (localgroup-terminal-create))
         (packets    (with-output-to-string (stream)
                       (dolist (event '((:scroll 0) (:scroll "up") (:scroll -1 :extra)
                                        (:scroll -1) (:scroll 1)))
                         (daemon-write-packet stream (list :event event)))))
         (output     (make-string-output-stream))
         (attachment (make-instance 'image-daemon:attachment :socket nil :mode ':control
                                    :stream (make-two-way-stream (make-string-input-stream packets) output)))
         (ui         (fullscreen-test--ui terminal)))
    (image-daemon:relay-attach terminal attachment :rows 12 :columns 40 :styled-p nil :session-id "wheel-test")
    (unwind-protect
         (with-terminal-ui (active ui)
           (terminal-ui-set-input active "draft")
           (terminal-ui-append-finalized-batch
            active (loop for index below 30 collect (list index (format nil "row ~D" index))))
           (image-daemon:relay-read-attachment terminal attachment :event-validator #'localgroup--terminal-event-p)
           (test-assert (terminal-input-ready-p terminal) "valid wheel packets reach the relay queue")
           (terminal-ui-process-event active (terminal-read-event terminal))
           (test-assert (= (- (clinedi:transcript-viewport-maximum-top
                               (fullscreen-terminal-ui-viewport active))
                              3)
                           (fullscreen-test--top active))
                        "remote wheel-up scrolls three rows")
           (terminal-ui-process-event active (terminal-read-event terminal))
           (test-assert (null (fullscreen-test--top active)) "remote wheel-down returns to the tail")
           (test-assert (not (terminal-input-ready-p terminal)) "malformed wheel packets were rejected")
           (test-assert (string= "draft" (line-editor-text (terminal-ui-editor active))) "wheel input preserves the draft"))
      (image-daemon:relay-detach terminal attachment)
      (terminal-ui-stop ui)))
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (terminal-ui-fullscreen-leave ui)
      (setf (recording-terminal-chunks terminal) nil)
      (terminal-ui-open-prompt-block ui)
      (test-assert (fullscreen-terminal-ui-active-p ui) "reopening a detached prompt acquires fullscreen")
      (test-assert (some (lambda (chunk) (search (format nil "~C[?1006h" #\Escape) chunk))
                         (recording-terminal-chunks terminal))
                   "prompt reopening restores mouse reporting")))
  nil)

(-> test-terminal-fullscreen-relay-startup-output () null)
(defun test-terminal-fullscreen-relay-startup-output ()
  "Test a detached fullscreen relay keeps transcript text out of its retained output."
  (let* ((terminal   (localgroup-terminal-create))
         (output     (make-string-output-stream))
         (attachment (make-instance 'image-daemon:attachment
                                    :socket nil :mode ':control
                                    :stream (make-two-way-stream
                                             (make-string-input-stream "") output)))
         (ui         (fullscreen-test--ui terminal)))
    (unwind-protect
         (with-terminal-ui (active ui)
           (terminal-ui-append-finalized-batch
            active (loop for index below 30 collect (list index (format nil "row ~D" index))))
           (test-assert (not (search "row 29" (image-daemon:relay-history-text terminal)))
                        "a transcript appended before any client attaches is not retained as text")
           (image-daemon:relay-attach terminal attachment
                                      :rows 12 :columns 40 :styled-p nil
                                      :session-id "startup-test")
           (terminal-ui-open-prompt-block active)
           (let ((history (image-daemon:relay-history-text terminal))
                 (frames  (localgroup-tests--frames attachment)))
             (test-assert (and (not (search (alternate-screen-enter-sequence) history))
                               (not (search "row 29" history)))
                          "an attached client receives no painted output as text")
             (test-assert (and frames
                               (getf (rest (first frames)) :complete-p)
                               (some (lambda (frame) (localgroup-tests--frame-text-p frame "row 29"))
                                     frames))
                          "the attached client sees the transcript in a complete frame")))
      (ignore-errors (image-daemon:relay-detach terminal attachment))
      (terminal-ui-stop ui)))
  nil)

(-> test-detached-fullscreen-streaming () null)
(defun test-detached-fullscreen-streaming ()
  "Present a completed streamed answer exactly once after a late relay attach."
  (dolist (initially-attached-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (terminal     (localgroup-terminal-create))
             (ui           (fullscreen-test--ui terminal))
             (application  (make-instance 'application
                                          :configuration configuration
                                          :conversation  conversation
                                          :ui            ui))
             (observer     (application-agent-observer application))
             (send-text    (callback-agent-observer-text-callback observer))
             (send-status  (callback-agent-observer-status-callback observer))
             (question     "May I proceed with the startup wrapper?")
             (answer       (format nil "The patch is verified.~%~A" question))
             (attachment   nil))
        (labels ((attach ()
                   "Attach a fresh terminal client and apply its repaint dimensions."
                   (setf attachment
                         (make-instance 'image-daemon:attachment
                                        :socket nil :mode ':control
                                        :stream (make-two-way-stream
                                                 (make-string-input-stream "")
                                                 (make-string-output-stream))))
                   (image-daemon:relay-attach terminal attachment
                                              :rows 36 :columns 67 :styled-p nil
                                              :session-id "detached-stream-test")
                   (terminal-ui-resize ui 67 :rows 36))

                 (transcript ()
                   "Return the fullscreen transcript's plain committed rows."
                   (let ((viewport (fullscreen-terminal-ui-viewport ui)))
                     (format nil "~{~A~^~%~}"
                             (loop for index below
                                   (clinedi:transcript-viewport-row-count viewport)
                                   collect (fullscreen-test--row-text ui index))))))
          (unwind-protect
               (with-terminal-ui (active ui)
                 (application-render-records application)
                 (when initially-attached-p
                   (attach))
                 (funcall send-status :provider-request-started nil)
                 (funcall send-text (format nil "The patch is verified.~%"))
                 (when initially-attached-p
                   (image-daemon:relay-detach terminal attachment)
                   (setf attachment nil)
                   (terminal-ui-detach active))
                 (funcall send-text question)
                 (conversation-append-provider-item
                  conversation
                  (json-object "type" "message"
                               "role" "assistant"
                               "status" "completed"
                               "phase" "final_answer"
                               "content" (json-array
                                          (json-object "type" "output_text"
                                                       "text" answer))))
                 (funcall send-status :provider-request-completed nil)
                 (application-render-records application)
                 (test-assert
                  (= 1 (terminal-tests--substring-count question (transcript)))
                  "Durable reconciliation retains the detached final answer once")
                 (test-assert
                  (not (search question (image-daemon:relay-history-text terminal)))
                  "Detached fullscreen streaming does not spill into normal-screen relay text")
                 (attach)
                 (let ((frame (clinedi:frame-snapshot-rows (localgroup-terminal-screen-frame terminal))))
                   (test-assert
                    (= 1 (count-if (lambda (row)
                                     (search question (clinedi:ansi-strip row)))
                                   frame))
                    "A late client receives the final permission question in its fullscreen frame"))
                 (application-render-records application)
                 (terminal-ui-resize active 50 :rows 20)
                 (test-assert
                  (= 1 (terminal-tests--substring-count question (transcript)))
                  "Reconciliation and reflow present the completed answer without duplication"))
            (when attachment
              (ignore-errors (image-daemon:relay-detach terminal attachment))))))))
  nil)


;;;; -- Transcript Clicks --

(-> test-terminal-fullscreen-submit-click () null)
(defun test-terminal-fullscreen-submit-click ()
  "Test clicking a form widget submits the form as typed input and keeps the draft."
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
        (actions  nil))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (push action actions)))
      (terminal-ui-append-finalized
       ui ':notice
       (list (terminal-span ':dim "  ↪ ")
             (termdown:make-widget ':code "(switch \"b-x\")" '(:submit "(switch \"b-x\")"))))
      (terminal-ui-insert-input ui "half typed")
      (multiple-value-bind (action payload) (fullscreen-test--click ui 8 1)
        (test-assert (and (eq action ':submit)
                          (equal payload "(switch \"b-x\")")
                          (null actions))
                     "a clicked form returns as its submission, not as an action"))
      (test-assert (equal (line-editor-text (terminal-ui-editor ui)) "half typed")
                   "the draft stays as it was")))
  nil)

(-> test-terminal-fullscreen-drag-copies () null)
(defun test-terminal-fullscreen-drag-copies ()
  "Test dragging across the transcript highlights the text and copies it on release."
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
        (actions  nil))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (push action actions)))
      (terminal-ui-append-finalized ui ':first (list (terminal-span ':plain "see https://example.com/doc now")))
      (terminal-ui-process-event ui '(:click 1 1))
      (terminal-ui-process-event ui '(:drag 3 1))
      (test-assert (search (format nil "~C[7msee~C[27m" (code-char 27) (code-char 27))
                           (clinedi:transcript-viewport-row-display (fullscreen-terminal-ui-viewport ui) 0))
                   "the dragged-over text shows highlighted")
      (terminal-ui-process-event ui '(:release 3 1))
      (test-assert (equal actions '((:copy "see")))
                   "releasing after a drag copies the selected text and opens nothing")
      (fullscreen-test--click ui 10 1)
      (test-assert (equal (first actions) '(:open-url "https://example.com/doc"))
                   "a click without a drag still acts on what it hits")))
  nil)

(-> test-terminal-fullscreen-clicks () null)
(defun test-terminal-fullscreen-clicks ()
  "Test clicks resolve to copy widgets and web URLs across streaming, scrolling, and deferral."
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
        (actions  nil))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (push action actions)))
      (terminal-ui-append-finalized
       ui ':first
       (list (terminal-span ':plain "see https://example.com/doc now")
             (terminal-span ':plain (string #\Newline))
             (terminal-span ':dim "  ``` ")
             (termdown:make-widget ':code-copy "⧉ copy" '(:copy "(+ 1 2)"))))
      (fullscreen-test--click ui 10 1)
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "clicking a URL opens it")
      (fullscreen-test--click ui 2 1)
      (test-assert (= 1 (length actions)) "clicking plain text does nothing")
      (fullscreen-test--click ui 9 2)
      (test-assert (equal '(:copy "(+ 1 2)") (first actions))
                   "clicking the copy label copies the fence source")
      (fullscreen-test--click ui 30 2)
      (test-assert (= 2 (length actions)) "clicking past the row end does nothing")
      (terminal-ui-stream-update
       ui
       :rows (list (list (terminal-span ':plain "streamed ")
                         (termdown:make-widget ':code-copy "⧉ copy" '(:copy "streamed source"))))
       :tail nil)
      (fullscreen-test--click ui 12 4)
      (test-assert (equal '(:copy "streamed source") (first actions))
                   "streamed rows keep their click regions")
      (terminal-ui-append-finalized-batch
       ui (loop for index below 30
                collect (list index (format nil "row https://example.com/~D" index))))
      (terminal-ui-process-event ui ':scroll-top)
      (fullscreen-test--click ui 10 1)
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "clicks after scrolling resolve through the visible top row")
      (terminal-ui-process-event ui ':scroll-bottom)
      (fullscreen-test--click ui 10 2)
      (test-assert (equal '(:open-url "https://example.com/26") (first actions))
                   "clicks while following the tail resolve through the painted window")
      (terminal-ui-resize ui 20 :rows 12)
      (terminal-ui-process-event ui ':scroll-top)
      (fullscreen-test--click ui 5 2)
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "reflowed rows still resolve the whole URL from their chunk")
      (setf (terminal-ui-live-output-suspended-p ui) t)
      (terminal-ui-append-finalized
       ui ':deferred
       (list (termdown:make-widget ':code-copy "⧉ copy" '(:copy "deferred"))))
      (setf (terminal-ui-live-output-suspended-p ui) nil)
      (terminal-ui--paint-live ui)
      (terminal-ui-process-event ui ':scroll-bottom)
      (fullscreen-test--click ui 1 8)
      (test-assert (equal '(:copy "deferred") (first actions))
                   "regions deferred during direct terminal I/O survive the resume")
      (setf (terminal-ui-action-function ui) nil)
      (fullscreen-test--click ui 1 8)
      (test-assert (= 7 (length actions))
                   "clicks without an installed action function are ignored")))
  (let ((terminal (make-instance 'recording-terminal :columns 100 :rows 20)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (setf (terminal-ui-action-function ui)
            (lambda (action)
              (when (eq (first action) ':insert)
                (terminal-ui-set-input ui (second action)))))
      (dolist (state '(:queued :running))
        (terminal-ui-set-input ui "draft")
        (terminal-ui-set-command-activities
         ui (list (list :id "exec:7" :type ':tool :index 7
                        :tool "shell.run" :description "Build 中文 release"
                        :state state :duration-ms 1000 :detached t)))
        (terminal-ui--paint-live ui)
        (let* ((frame (map 'list #'clinedi:ansi-strip (fullscreen-test--frame ui)))
               (row   (position-if (lambda (text) (search "(job.cancel" text)) frame))
               (text  (and row (nth row frame)))
               (start (and text (search "(job.cancel" text))))
          (test-assert start "active command rows expose cancellation in the pinned frame")
          (when start
            (fullscreen-test--click
             ui (1+ (terminal--spans-width
                     (list (terminal-span ':plain (subseq text 0 start)))))
             (1+ row))
            (test-assert
             (string= (line-editor-text (terminal-ui-editor ui))
                      "(job.cancel :id \"exec:7\")")
             "clicking a pinned action inserts its executable call after wide text"))))
      (terminal-ui-resize ui 40 :rows 20)
      (test-assert (null (fullscreen-terminal-ui-pinned-actions ui))
                   "resizing below the action width clears stale hit regions")
      (terminal-ui-resize ui 100 :rows 20)
      (terminal-ui-set-command-activities ui nil)
      (terminal-ui--paint-live ui)
      (test-assert (null (fullscreen-terminal-ui-pinned-actions ui))
                   "completed commands expose no cancellation hit regions")))
  nil)

(-> test-localgroup-click-events () null)
(defun test-localgroup-click-events ()
  "Test relayed click, drag and release packets are accepted only with bounded one-based coordinates."
  (dolist (case '(((:click 3 7) t)
                  ((:click 1 1) t)
                  ((:click 0 7) nil)
                  ((:click 3 0) nil)
                  ((:click 3 7 1) nil)
                  ((:click "3" 7) nil)
                  ((:click 3 10001) nil)
                  ((:drag 3 7) t)
                  ((:release 3 7) t)
                  ((:drag 0 7) nil)
                  ((:scroll -1) t)))
    (destructuring-bind (event expected) case
      (test-assert (eq expected (localgroup--terminal-event-p event))
                   (format nil "relay validation of ~S" event))))
  nil)


;;;; -- Message Jumps and Exit Epilogue --


(-> test-terminal-fullscreen-message-jumps () null)
(defun test-terminal-fullscreen-message-jumps ()
  "Test Ctrl-Page jumps end user and assistant messages on the bottom row, skipping activity rows."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui       (terminal-ui-create :terminal terminal :fullscreen-p t :prompt "> "
                                       :message-header-prefixes '("❯ you" "● autolith"))))
    (with-terminal-ui (active ui)
      (terminal-ui-append-finalized-batch
       active
       (loop for turn below 4
             append (list (list (list :user turn)
                                (format nil "❯ you 12:0~D~%question ~D" turn turn))
                          (list (list :tool turn)
                                (format nil "▸ tool ~D~%│ output line~%│ more output" turn))
                          (list (list :agent turn)
                                (format nil "● autolith~%answer ~D continues here" turn)))))
      (terminal-ui-process-event active ':scroll-bottom)
      (labels ((window ()
                 (let* ((viewport (fullscreen-terminal-ui-viewport active))
                        (top      (or (fullscreen-test--top active)
                                      (clinedi:transcript-viewport-maximum-top viewport)))
                        (end      (min (+ top (clinedi:transcript-viewport-height viewport))
                                       (clinedi:transcript-viewport-row-count viewport))))
                   (loop for row from top below end
                         collect (fullscreen-test--row-text active row))))

               (ends-p (header last)
                 (let ((rows (window)))
                   (and (find-if (lambda (row) (uiop:string-prefix-p header row)) rows)
                        (uiop:string-prefix-p last (first (last rows)))
                        t))))
        (terminal-ui-process-event active ':previous-section)
        (test-assert (ends-p "❯ you 12:03" "│ more output")
                     "Ctrl-PgUp from the tail ends the previous message on the bottom row")
        (terminal-ui-process-event active ':previous-section)
        (test-assert (ends-p "● autolith" "answer 2")
                     "a second Ctrl-PgUp steps from the message on the bottom row to the one before")
        (terminal-ui-process-event active ':next-section)
        (test-assert (ends-p "❯ you 12:03" "│ more output")
                     "Ctrl-PgDn ends the following message on the bottom row")
        (terminal-ui-process-event active ':next-section)
        (test-assert (null (fullscreen-test--top active))
                     "jumping to the newest message follows the tail again")
        (terminal-ui-process-event active ':scroll-top)
        (terminal-ui-process-event active ':previous-section)
        (test-assert (eql 0 (fullscreen-test--top active))
                     "Ctrl-PgUp in the first window stays at the first row"))
      (test-assert (string= "draft" (progn (terminal-ui-set-input active "draft")
                                           (line-editor-text (terminal-ui-editor active))))
                   "message jumps leave the draft alone")))
  nil)

(-> test-terminal-fullscreen-repl-message-starts () null)
(defun test-terminal-fullscreen-repl-message-starts ()
  "Test REPL-style messages start at their timestamp line, or their marker without one."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui       (terminal-ui-create :terminal terminal :fullscreen-p t
                                       :message-header-prefixes '("> " "< "))))
    (dolist (case (list (list (format nil "  2026-10-26 21:30~%> hi!~%") 0 t
                              "a timestamp line above a marker opens the message")
                        (list (format nil "  2026-10-26 21:30~%> hi!~%") 19 t
                              "the marker line itself still opens a message")
                        (list (format nil "< reply~%  continued~%") 8 nil
                              "an indented continuation line opens nothing")
                        (list (format nil "  body~%~%< next~%") 0 nil
                              "an indented line above a blank row opens nothing")))
      (destructuring-bind (text offset expected description) case
        (test-assert (eq (terminal-ui-fullscreen--message-start-p ui text offset) expected)
                     description))))
  nil)

(-> test-terminal-fullscreen-exit-epilogue () null)
(defun test-terminal-fullscreen-exit-epilogue ()
  "Test epilogue text reaches the normal screen after the alternate buffer is left."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui       (fullscreen-test--ui terminal)))
    (terminal-ui-start ui)
    (terminal-ui-append-finalized ui ':first "transcript row")
    (terminal-ui-set-epilogue ui (list (terminal-span ':dim "To open this channel again, run:")
                                       (terminal-span ':plain (string #\Newline))
                                       (terminal-span ':code "  autolith open abc")))
    (recording-terminal-reset terminal)
    (terminal-ui-stop ui)
    (let* ((output (recording-terminal-output terminal))
           (leave  (search (format nil "~C[?1049l" #\Escape) output))
           (advice (search "autolith open abc" output)))
      (test-assert (and leave advice (< leave advice))
                   "the epilogue is written after the alternate buffer is restored")))
  nil)

(-> test-terminal-fullscreen-forced-exit-resume () null)
(defun test-terminal-fullscreen-forced-exit-resume ()
  "Test forced Ctrl-C restores fullscreen before printing the resume command."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "fullscreen-force-resume"))
         (terminal      (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui            (fullscreen-test--ui terminal))
         (application   (make-instance 'application
                                        :configuration configuration
                                        :conversation  conversation
                                        :ui            ui))
         (forced-status nil)
         (controller    nil))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "keep this conversation")
           (setf controller
                 (make-instance 'application-input-controller
                                :application              application
                                :main-thread              (current-thread)
                                :interrupt-clock-function (lambda () 10)
                                :forced-exit-function     (lambda (status)
                                                            (setf forced-status status))))
           (setf (application-input-controller application) controller
                 (application-input-controller-active-p controller) t)
           (with-terminal-ui (active-ui ui)
             (application-input-controller--process-event controller ':interrupt)
             (application-input-controller--process-event controller ':interrupt))
           (let* ((output (recording-terminal-output terminal))
                  (leave  (search (format nil "~C[?1049l" #\Escape) output))
                  (resume (search "autolith open fullscreen-force-resume" output)))
             (test-assert (= forced-status *application-forced-interrupt-status*)
                          "fullscreen forced interruption exits with status 130")
             (test-assert (and leave resume (< leave resume))
                          "fullscreen forced interruption restores the screen before its resume command")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    nil))

(-> test-terminal-fullscreen-new-conversation () null)
(defun test-terminal-fullscreen-new-conversation ()
  "Start a new conversation through command dispatch and replace the fullscreen ledger."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "old-ledger"))
           (terminal     (make-instance 'recording-terminal :columns 80 :rows 16))
           (ui           (fullscreen-test--ui terminal))
           (application  (make-instance 'application :configuration configuration
                                                    :conversation conversation :ui ui))
           (fail-p       t))
      (conversation-append-user-message conversation "OLD-LEDGER")
      (with-terminal-ui (active ui)
        (terminal-ui-append-finalized active ':ledger "OLD-LEDGER")
        (terminal-ui-process-event active ':scroll-top)
        (setf (terminal-ui-live-output-suspended-p active) t
              (terminal-ui-stream-tail active) "OLD-STREAM"
              (terminal-ui-pending-action active) '(:copy "OLD-LEDGER"))
        (terminal-ui-append-finalized active ':deferred "OLD-DEFERRED")
        (test-call-with-function-replacements
         (list (list 'application-install-conversation
                     (lambda (observed target)
                       (when fail-p
                         (error 'simple-error :format-control "Synthetic install failure."))
                       (setf (application-conversation observed) target)
                       observed)))
         (lambda ()
           (test-assert
            (handler-case (progn (application-command application "/new") nil)
              (simple-error () t))
            "a failed conversation installation is reported")
           (test-assert
            (and (eq (application-conversation application) conversation)
                 (search "OLD-LEDGER" (clinedi:transcript-viewport-chunk-text
                                       (fullscreen-terminal-ui-viewport active) 0)))
            "a failed switch retains its current fullscreen ledger")
           (setf fail-p nil)
           (test-assert (eq (application-command application "/new") ':continue)
                        "new dispatch completes successfully")))
        (setf (terminal-ui-live-output-suspended-p active) nil)
        (terminal-ui--paint-live active)
        (test-assert (not (eq (application-conversation application) conversation))
                     "new installs a fresh conversation")
        (test-assert
         (loop with viewport = (fullscreen-terminal-ui-viewport active)
               for index below (clinedi:transcript-viewport-chunk-count viewport)
               never (search "OLD-" (clinedi:transcript-viewport-chunk-text viewport index)))
         "the new ledger contains neither committed nor deferred output from the old session")
        (test-assert (and (null (fullscreen-test--top active))
                          (null (terminal-ui-stream-tail active))
                          (null (terminal-ui-pending-action active)))
                     "the fresh view follows new output without stale streaming or click state")
        (test-assert (terminal-ui-append-finalized active ':ledger "new output")
                     "the new transcript has a fresh finalized-identifier scope")
        (terminal-ui-resize active 60 :rows 16)
        (test-assert
         (every (lambda (row) (not (search "OLD-" (clinedi:ansi-strip row))))
                (coerce (fullscreen-test--frame active) 'list))
         "the repainted screen and resized frame exclude the previous ledger")
        (test-assert
         (some (lambda (item) (search "OLD-LEDGER" (json-encode item)))
               (conversation-input-items (conversation-load (conversation-pathname conversation))))
         "clearing the display preserves the previous conversation on disk"))))
  nil)


;;;; -- Semantic Markdown Reflow --

(-> fullscreen-test--markdown-presentation (application string integer) list)
(defun fullscreen-test--markdown-presentation (application source sequence)
  "Build a durable assistant presentation through APPLICATION's normal renderer."
  (application--record-presentation
   application
   (list :provider-item :seq sequence
         :wire-json     (json-encode (agent-test-message source)))))

(-> fullscreen-test--assert-markdown-replay (fullscreen-terminal-ui integer) null)
(defun fullscreen-test--assert-markdown-replay (ui width)
  "Check resident Markdown and its speculative tail against fresh WIDTH rendering."
  (let* ((viewport (fullscreen-terminal-ui-viewport ui))
         (chunks   (clinedi::transcript-viewport--chunks viewport)))
    (loop for chunk across chunks
          for index from 0
          for record = (terminal-ui-fullscreen--markdown-record
                        ui (clinedi::viewport-chunk-tag chunk))
          when record
            do (multiple-value-bind (text display regions tail)
                   (terminal-ui-fullscreen--markdown-content ui record width)
                 (declare (ignore display regions))
                 (test-assert
                  (string= text (clinedi:transcript-viewport-chunk-text viewport index))
                  "resident Markdown is freshly rendered from its original source")
                 (when (equal (getf record :identifier)
                              (fullscreen-terminal-ui-markdown-stream ui))
                   (test-assert (equalp tail (terminal-ui-stream-tail ui))
                                "the speculative tail uses the current pane width")))))
  nil)

(-> test-terminal-fullscreen-markdown-reflow () null)
(defun test-terminal-fullscreen-markdown-reflow ()
  "Reflow application Markdown, preserving structure, widgets and viewport identity."
  (with-test-configuration (configuration)
    (let* ((terminal     (make-instance 'recording-terminal :columns 32 :rows 12))
           (ui           (fullscreen-test--ui terminal))
           (application  (make-instance 'application :configuration configuration
                                        :conversation               (conversation-create configuration)
                                        :ui                         ui))
           (source       (format nil "~{~A ~}~%~%Intentional first line~%Intentional second line~%~%- first list item with several words to wrap~%- second item~%~%```lisp~%(print :first)~%(print :second)~%```~%~%| Name | Description |~%| --- | --- |~%| alpha | a sufficiently long table cell to wrap |~%~%```mermaid~%graph TD~%A --> B~%```"
                                 (loop repeat 12 append '("alpha" "beta" "gamma" "delta"))))
           (presentation (fullscreen-test--markdown-presentation application source 1)))
      (with-terminal-ui (active ui)
        (terminal-ui-present-records active (list presentation))
        (let* ((viewport (fullscreen-terminal-ui-viewport active))
               (tag      (clinedi:transcript-viewport-last-tag viewport))
               (narrow   (clinedi:transcript-viewport-row-count viewport)))
          (terminal-ui-process-event active ':scroll-top)
          (terminal-ui-resize active 72 :rows 12)
          (fullscreen-test--assert-markdown-replay active 72)
          (test-assert (< (clinedi:transcript-viewport-row-count viewport) narrow)
                       "a wider pane unwraps existing paragraph rows")
          (terminal-ui-resize active 32 :rows 12)
          (fullscreen-test--assert-markdown-replay active 32)
          (test-assert
           (and (= narrow (clinedi:transcript-viewport-row-count viewport))
                (= 1 (fullscreen-test--chunk-count active))
                (equal tag (clinedi:transcript-viewport-last-tag viewport))
                (integerp (fullscreen-test--top active)))
           "width round trips preserve the resident publication and scrolled mode")
          (test-assert (zerop (terminal-ui-present-records active (list presentation)))
                       "replaying the durable record does not duplicate the answer")
          ;; Lay out every committed row to exercise widget hits after both resizes.
          (clinedi:transcript-viewport-layout viewport 10000)
          (let ((actions
                  (loop for row below (clinedi:transcript-viewport-row-count viewport)
                        for text = (fullscreen-test--row-text active row)
                        for column = (search "⧉ copy" text)
                        when column
                          collect (nth-value 0 (clinedi:transcript-viewport-hit
                                                viewport (1+ column) (1+ row))))))
            (test-assert
             (some (lambda (action)
                     (and (eq (first action) ':copy)
                          (string= (string-trim '(#\Newline) (second action))
                                   (format nil "(print :first)~%(print :second)"))))
                   actions)
             "reflow retains a code-copy widget with the original multiline source"))))))
  nil)

(-> test-terminal-fullscreen-markdown-stream-reflow () null)
(defun test-terminal-fullscreen-markdown-stream-reflow ()
  "Resize incomplete Markdown between provider deltas and reconcile its durable answer."
  (with-test-configuration (configuration)
    (let* ((terminal     (make-instance 'recording-terminal :columns 32 :rows 16))
           (ui           (fullscreen-test--ui terminal))
           (conversation (conversation-create configuration))
           (application  (make-instance 'application :configuration configuration
                                        :conversation conversation :ui ui))
           (observer     (application-agent-observer application))
           (send-text    (callback-agent-observer-text-callback observer))
           (send-status  (callback-agent-observer-status-callback observer))
           (parts        (list (format nil "A paragraph with enough words to wrap at the narrow width.~%~%```lisp~%(print :incomplete")
                               (format nil ")~%```~%~%| Name | Description |~%| --- | --- |~%| alpha | pending cell")
                               (format nil " completed |~%~%```mermaid~%graph TD~%A --> B")
                               (format nil "~%```~%~%FINAL-MARKER"))))
      (with-terminal-ui (active ui)
        (funcall send-status ':provider-request-started nil)
        (loop for part in parts
              for width in '(72 32 61 40)
              do (funcall send-text part)
                 (terminal-ui-resize active width :rows 16)
                 (fullscreen-test--assert-markdown-replay active width)
                 (test-assert (= 1 (fullscreen-test--chunk-count active))
                              "provider deltas replace one stable Markdown publication"))
        (conversation-append-provider-item conversation (agent-test-message (apply #'concatenate 'string parts)))
        (funcall send-status ':provider-request-completed nil)
        (terminal-ui-resize active 72 :rows 16)
        (fullscreen-test--assert-markdown-replay active 72)
        (test-assert
         (and (= 1 (fullscreen-test--chunk-count active))
              (null (terminal-ui-stream-tail active))
              (null (fullscreen-terminal-ui-markdown-stream active))
              (= 1 (terminal-tests--substring-count
                    "FINAL-MARKER"
                    (clinedi:transcript-viewport-chunk-text
                     (fullscreen-terminal-ui-viewport active) 0))))
         "completion clears speculative state and durable reconciliation emits the answer once")))
    (let* ((terminal     (make-instance 'recording-terminal :columns 60 :rows 12))
           (ui           (fullscreen-test--ui terminal))
           (conversation (conversation-create configuration))
           (application  (make-instance 'application :configuration configuration
                                        :conversation conversation :ui ui))
           (observer     (application-agent-observer application))
           (send-text    (callback-agent-observer-text-callback observer))
           (send-status  (callback-agent-observer-status-callback observer))
           (source       (format nil "PIPE-FIRST~%PIPE-SECOND~%")))
      (with-terminal-ui (active ui)
        (setf (terminal-interactive-p (terminal-ui-terminal active)) nil
              (recording-terminal-chunks terminal) nil)
        (funcall send-status ':provider-request-started nil)
        (funcall send-text (format nil "PIPE-FIRST~%"))
        (funcall send-text (format nil "PIPE-SECOND~%"))
        (conversation-append-provider-item conversation (agent-test-message source))
        (funcall send-status ':provider-request-completed nil)
        (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
          (test-assert
           (and (= 1 (terminal-tests--substring-count "PIPE-FIRST" output))
                (= 1 (terminal-tests--substring-count "PIPE-SECOND" output)))
           "a plain output sink emits each incremental prefix once"))))
    ;; A REPL fragment without a newline has no committed header or native chunk.
    (let* ((terminal (make-instance 'recording-terminal :columns 32 :rows 12))
           (ui       (fullscreen-test--ui terminal))
           (source   "A pending REPL fragment whose wrapping changes with the pane width."))
      (with-terminal-ui (active ui)
        (terminal-ui-stream-markdown active ':pending :source source :marker '(:brand "● "))
        (terminal-ui-resize active 72 :rows 12)
        (multiple-value-bind (spans tail)
            (terminal-ui--markdown-render
             (list :source source :stream-p t :marker '(:brand "● ")) 72)
          (declare (ignore spans))
          (test-assert (equalp tail (terminal-ui-stream-tail active))
                       "tail-only REPL fragments reflow before their first committed row"))
        (terminal-ui-stream-markdown active ':pending :source source
                                     :marker '(:brand "● ") :finished-p t)
        (test-assert (and (= 1 (fullscreen-test--chunk-count active))
                          (null (terminal-ui-stream-tail active)))
                     "finishing a tail-only stream commits it once"))))
  nil)

(-> test-terminal-fullscreen-markdown-rollback-residency () null)
(defun test-terminal-fullscreen-markdown-rollback-residency ()
  "Roll back failed semantic reflow and charge source to bounded resident chunks."
  (let* ((terminal (make-instance 'recording-terminal :columns 32 :rows 12))
         (ui       (fullscreen-test--ui terminal))
         (source   (format nil "~{~A ~}~%" (loop repeat 20 collect "paragraph"))))
    (with-terminal-ui (active ui)
      (terminal-ui-stream-markdown active ':first :source source :finished-p t)
      (terminal-ui-stream-markdown active ':second :source (concatenate 'string source "pending"))
      (terminal-ui-process-event active ':scroll-top)
      (let* ((viewport (fullscreen-terminal-ui-viewport active))
             (text     (clinedi:transcript-viewport-chunk-text viewport 0))
             (tail     (terminal-ui-stream-tail active))
             (index    (fullscreen-terminal-ui-presentation-records active))
             (top      (fullscreen-test--top active))
             (render   (symbol-function 'terminal-ui-fullscreen--markdown-content))
             (calls    0))
        ;; A publication notices geometry before applying its new operation.
        (setf (terminal-columns terminal) 72)
        (test-call-with-function-replacements
         (list (list 'terminal-ui-fullscreen--markdown-content
                     (lambda (observed record width)
                       (when (= 2 (incf calls))
                         (error 'simple-error :format-control "Synthetic Markdown render failure."))
                       (funcall render observed record width))))
         (lambda ()
           (test-assert
            (handler-case (progn (terminal-ui-append-finalized active ':retry "later") nil)
              (simple-error () t))
            "a renderer failure aborts the pending publication")))
        (test-assert
         (and (= 32 (clinedi:transcript-viewport-width viewport))
              (= 2 (fullscreen-test--chunk-count active))
              (string= text (clinedi:transcript-viewport-chunk-text viewport 0))
              (equalp tail (terminal-ui-stream-tail active))
              (eq index (fullscreen-terminal-ui-presentation-records active))
              (eql top (fullscreen-test--top active))
              (not (gethash ':retry (terminal-ui-finalized-identifiers active))))
         "failed reflow restores geometry, chunks, source index, tail, anchor and deduplication state")
        (test-assert (terminal-ui-append-finalized active ':retry "later")
                     "the same publication succeeds after the renderer recovers")
        (fullscreen-test--assert-markdown-replay active 72))))
  (let ((*fullscreen-transcript-character-limit* 3000))
    (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
           (ui       (fullscreen-test--ui terminal))
           (source   (format nil "~{~A ~}" (loop repeat 35 collect "resident"))))
      (with-terminal-ui (active ui)
        (loop for identifier below 50
              do (terminal-ui-stream-markdown active identifier :source source :finished-p t))
        (let* ((viewport (fullscreen-terminal-ui-viewport active))
               (chunks   (clinedi::transcript-viewport--chunks viewport))
               (index    (fullscreen-terminal-ui-presentation-records active)))
          (test-assert
           (and (plusp (clinedi:transcript-viewport-evicted-count viewport))
                (= (hash-table-count index) (length chunks))
                (not (gethash 0 index))
                (<= (clinedi::transcript-viewport--weight viewport)
                    *fullscreen-transcript-character-limit*))
           "source records share native chunk residency and disappear with eviction")
          (loop for width in '(72 32 72)
                do (terminal-ui-resize active width :rows 12)
                   (loop for chunk across (clinedi::transcript-viewport--chunks viewport)
                         for record = (terminal-ui-fullscreen--markdown-record
                                       active (clinedi::viewport-chunk-tag chunk))
                         for markdown = (getf record :markdown)
                         do (multiple-value-bind (text display regions)
                                (terminal-ui-fullscreen--markdown-content active record width)
                              (let ((fresh (clinedi:make-transcript-viewport :width width)))
                                (clinedi:transcript-viewport-append fresh text display :regions regions)
                                (test-assert
                                 (= (clinedi::viewport-chunk-weight chunk)
                                    (+ (clinedi::viewport-chunk-weight
                                        (aref (clinedi::transcript-viewport--chunks fresh) 0))
                                       (length (getf markdown :source))
                                       (loop for span in (getf markdown :header)
                                             sum (length (terminal-span-text span)))))
                                 "each width replay charges retained source exactly once")))))
          (terminal-ui-reset-transcript active)
          (terminal-ui-stream-markdown active ':empty :source (format nil "old content~%"))
          (let* ((current (fullscreen-terminal-ui-viewport active))
                 (weight  (clinedi::transcript-viewport--weight current)))
            (terminal-ui-stream-markdown active ':empty :source "")
            (test-assert
             (and (= 1 (fullscreen-test--chunk-count active))
                  (string= "" (clinedi:transcript-viewport-chunk-text current 0))
                  (< (clinedi::transcript-viewport--weight current) weight)
                  (null (terminal-ui-stream-tail active)))
             "an empty cumulative replacement clears committed text and its retained weight"))
          (terminal-ui-reset-transcript active)
          (terminal-ui-stream-markdown active ':evicted :source (format nil "committed~%pending"))
          (terminal-ui-append-finalized active ':newer (make-string 4000 :initial-element #\x))
          (test-assert
           (and (null (terminal-ui-stream-tail active))
                (null (fullscreen-terminal-ui-markdown-stream active))
                (eq ':evicted (fullscreen-terminal-ui-evicted-markdown-stream active))
                (not (gethash ':evicted (fullscreen-terminal-ui-presentation-records active))))
           "evicting an active chunk releases its source and speculative tail")
          (terminal-ui-resize active 61 :rows 12)
          (terminal-ui-stream-markdown active ':evicted :source (format nil "committed~%pending later"))
          (test-assert
           (and (= 1 (fullscreen-test--chunk-count active))
                (null (terminal-ui-stream-tail active))
                (not (gethash ':evicted (fullscreen-terminal-ui-presentation-records active))))
           "resize and later cumulative deltas cannot resurrect an evicted stream")
          (terminal-ui-stream-markdown active ':evicted :source "finished" :finished-p t)
          (test-assert (null (fullscreen-terminal-ui-evicted-markdown-stream active))
                       "finishing the evicted block releases its bounded identity tombstone")
          (terminal-ui-stream-markdown active ':next :source "fresh" :finished-p t)
          (test-assert (gethash ':next (fullscreen-terminal-ui-presentation-records active))
                       "a subsequent block publishes normally after active eviction")))))
  nil)


;;;; -- General Resident Sources and Publication Work --

(-> test-terminal-fullscreen-source-tool-reflow () null)
(defun test-terminal-fullscreen-source-tool-reflow ()
  "Rerender original call arguments and folded outcomes across inline/table layouts."
  (with-test-configuration (configuration)
    (let* ((terminal    (make-instance 'recording-terminal :columns 120 :rows 16))
           (ui          (fullscreen-test--ui terminal))
           (application (make-instance 'application :configuration configuration
                                       :conversation (conversation-create configuration) :ui ui))
           (call        (list :provider-item :seq 1
                              :wire-json
                              (json-encode
                               (agent-test-call :call-id "source-call" :namespace "test" :name "echo"
                                                :arguments "{\"value\":\"alpha beta gamma delta epsilon zeta eta theta\"}"))))
           (result      (list :tool-result :seq 2 :call-id "source-call" :tool "test.echo"
                              :status ':error :output "A detailed failure whose source wraps under the call.")))
      (with-terminal-ui (active ui)
        (terminal-ui-present-records active (list (application--record-presentation application call)))
        (let* ((viewport (fullscreen-terminal-ui-viewport active))
               (wide     (clinedi:transcript-viewport-chunk-text viewport 0))
               (tag      (clinedi:transcript-viewport-last-tag viewport)))
          (test-assert (search " :value " wide)
                       "a wide call retains its argument in the inline header")
          (terminal-ui-resize active 32 :rows 16)
          (test-assert
           (and (> (clinedi:transcript-viewport-row-count viewport) 1)
                (search "theta" (clinedi:transcript-viewport-chunk-text viewport 0)))
           "a narrow call uses fresh table/continuation rows rather than clipping its old header")
          (terminal-ui-resize active 120 :rows 16)
          (test-assert (string= wide (clinedi:transcript-viewport-chunk-text viewport 0))
                       "widening restores the original argument source and inline header")
          (terminal-ui-present-records active (list (application--record-presentation application result)))
          (test-assert (= 1 (fullscreen-test--chunk-count active))
                       "the outcome folds into the source-backed invocation")
          (loop for width in '(32 90 120)
                do (terminal-ui-resize active width :rows 16)
                   (let* ((index  (fullscreen-terminal-ui-presentation-records active))
                          (record (gethash (first tag) index)))
                     (multiple-value-bind (text display regions)
                         (terminal-ui-fullscreen--record-content active record width)
                       (declare (ignore display regions))
                       (test-assert
                        (and (string= text (clinedi:transcript-viewport-chunk-text viewport 0))
                             (search "! " text)
                             (search "detailed" text)
                             (functionp (getf record :body-layout))
                             (eq (getf record :state) ':failure)
                             (equal tag (clinedi:transcript-viewport-last-tag viewport)))
                        "folded outcome source reflows with its original call and lifecycle state"))))))))
  nil)

(-> test-terminal-fullscreen-source-prose-reflow () null)
(defun test-terminal-fullscreen-source-prose-reflow ()
  "Restore user prose wrapping from source rather than from a width-baked span list."
  (with-test-configuration (configuration)
    (let* ((terminal    (make-instance 'recording-terminal :columns 32 :rows 12))
           (ui          (fullscreen-test--ui terminal))
           (application (make-instance 'application :configuration configuration
                                       :conversation (conversation-create configuration) :ui ui))
           (source      (format nil "~{~A ~}~%An intentional second line."
                                (loop repeat 8 append '("alpha" "beta" "gamma"))))
           (record      (list :message :seq 1 :role ':user :content source)))
      (with-terminal-ui (active ui)
        (let ((presentation (application--record-presentation application record)))
          (terminal-ui-present-records active (list presentation))
          (let* ((viewport (fullscreen-terminal-ui-viewport active))
                 (narrow   (clinedi:transcript-viewport-row-count viewport)))
            (loop for width in '(80 24 80 32)
                  do (terminal-ui-resize active width :rows 12)
                     (let* ((metadata (gethash (first presentation)
                                               (fullscreen-terminal-ui-presentation-records active)))
                            (entry    (let ((*application-transcript-layout-columns* width))
                                        (conversation-record-entry application record))))
                       (multiple-value-bind (text display regions)
                           (terminal-ui--finalized-content active entry :group (getf metadata :group))
                         (declare (ignore display regions))
                         (test-assert
                          (string= (concatenate 'string (getf metadata :prefix) text)
                                   (clinedi:transcript-viewport-chunk-text viewport 0))
                          "resident prose matches fresh source rendering at every width")))
                     (when (= width 80)
                       (test-assert (< (clinedi:transcript-viewport-row-count viewport) narrow)
                                    "a wider pane unwraps user prose")))
            (test-assert (= 1 (fullscreen-test--chunk-count active))
                         "prose round trips retain one publication"))))))
  nil)

(-> test-terminal-fullscreen-idle-publication-work () null)
(defun test-terminal-fullscreen-idle-publication-work ()
  "Keep idle/editor/scroll paints independent of resident transcript storage."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui       (fullscreen-test--ui terminal)))
    (with-terminal-ui (active ui)
      (loop for identifier below 30
            do (terminal-ui-stream-markdown active identifier
                                            :source     "Resident paragraph with enough words to wrap."
                                            :finished-p t))
      (let ((checkpoints 0)
            (copies      0)
            (renders     0)
            (checkpoint  (symbol-function 'clinedi:transcript-viewport-checkpoint))
            (copy        (symbol-function 'terminal-ui-fullscreen--copy-publication-value))
            (render      (symbol-function 'terminal-ui-fullscreen--record-content)))
        (test-call-with-function-replacements
         (list
          (list 'clinedi:transcript-viewport-checkpoint
                (lambda (viewport) (incf checkpoints) (funcall checkpoint viewport)))
          (list 'terminal-ui-fullscreen--copy-publication-value
                (lambda (value) (incf copies) (funcall copy value)))
          (list 'terminal-ui-fullscreen--record-content
                (lambda (observed record width &rest arguments)
                  (incf renders) (apply render observed record width arguments))))
         (lambda ()
           (terminal-ui--present-live active)
           (terminal-ui-set-input active "typing")
           (terminal-ui-process-event active ':scroll-top)
           (terminal-ui--present-live active)
           (test-assert (and (zerop checkpoints) (zerop copies) (zerop renders))
                        "idle, composer and scroll paints do no resident snapshot or source-layout work")
           (terminal-ui-resize active 72 :rows 12)
           (test-assert (and (zerop checkpoints) (zerop copies) (= renders 30))
                        "a real resize replays each source once without a publication snapshot")
           (terminal-ui-present-records active (list (list ':new "new output" nil)))
           (test-assert (and (= checkpoints 1) (plusp copies) (= renders 31))
                        "semantic publication alone checkpoints its recoverable mutation"))))))
  nil)
