(in-package #:autolith)

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
        (let* ((text (fullscreen-test--row-text ui top))
               (first-word (subseq text 0 (min 3 (length text)))))
          (terminal-ui-resize ui 17 :rows 12)
          (test-assert (and (integerp (fullscreen-test--top ui))
                            (search first-word
                                    (fullscreen-test--row-text ui (fullscreen-test--top ui))))
                       "resize keeps the scrolled source line on top")))
      (let* ((viewport (fullscreen-terminal-ui-viewport ui))
             (rows (clinedi:transcript-viewport-row-count viewport))
             (count (clinedi:transcript-viewport-chunk-count viewport)))
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
  (let* ((draft "aaaa bbbb of the string")
         (hanging (position #\Space draft :start 10))
         (terminal (make-instance 'recording-terminal :columns 14 :rows 12)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (terminal-ui-set-input ui draft)
      (loop for cursor downfrom (length draft) to 0
            do (multiple-value-bind (frame row column) (terminal-ui-fullscreen--frame ui nil)
                 (let* ((line (clinedi:ansi-strip (nth row frame)))
                        (shown (if (< column (length line)) (char line column) #\Space))
                        (expected (cond ((= cursor (length draft)) #\Space)
                                        ((= cursor hanging) (char draft (1+ cursor)))
                                        (t (char draft cursor)))))
                   (when (= cursor (length draft))
                     (test-assert (search "aaaa bbbb of" (clinedi:ansi-strip (nth (1- row) frame)))
                                  "the flush row keeps its last word")
                     (test-assert (eql 0 (search "the string" line))
                                  "the continuation row hides the hanging space"))
                   (test-assert (char= expected shown)
                                (format nil "cursor ~D sits on its character" cursor))))
               (terminal-ui-process-event ui ':left))))
  nil)

(-> test-terminal-fullscreen-failure-and-lifecycle () null)
(defun test-terminal-fullscreen-failure-and-lifecycle ()
  "Roll back failed transcript paints and release an owned alternate screen exactly once."
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 40 :rows 12))
         (ui (fullscreen-test--ui terminal)))
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
    (let* ((output (recording-terminal-output terminal))
           (enter (search (alternate-screen-enter-sequence) output))
           (enable (search (terminal-keyboard-enhancement-enable-sequence) output))
           (disable (search (terminal-keyboard-enhancement-disable-sequence) output))
           (leave (search (alternate-screen-leave-sequence) output)))
      (test-assert (and enter enable disable leave (< enter enable disable leave))
                   "keyboard protocol is acquired and restored inside the alternate buffer")
      (test-assert (= 1 (terminal-tests--substring-count
                        (terminal-keyboard-enhancement-enable-sequence) output))
                   "repeated startup pushes one alternate keyboard mode")))
  (let* ((terminal (make-instance 'recording-terminal :columns 30 :rows 10))
         (ui (fullscreen-test--ui terminal)))
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
  (let* ((terminal (localgroup-terminal-create))
         (packets (with-output-to-string (stream)
                    (dolist (event '((:scroll 0) (:scroll "up") (:scroll -1 :extra)
                                     (:scroll -1) (:scroll 1)))
                      (daemon-write-packet stream (list :event event)))))
         (output (make-string-output-stream))
         (attachment (make-instance 'image-daemon:attachment :socket nil :mode ':control
                                    :stream (make-two-way-stream (make-string-input-stream packets) output)))
         (ui (fullscreen-test--ui terminal)))
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
  (let* ((terminal (localgroup-terminal-create))
         (output (make-string-output-stream))
         (attachment (make-instance 'image-daemon:attachment
                                    :socket nil :mode ':control
                                    :stream (make-two-way-stream
                                             (make-string-input-stream "") output)))
         (ui (fullscreen-test--ui terminal)))
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
           (let* ((history (image-daemon:relay-history-text terminal))
                  (alternate (search (alternate-screen-enter-sequence) history))
                  (row (search "row 29" history)))
             (test-assert (and alternate row (< alternate row))
                          "the attached client sees the transcript only in its fullscreen paint")))
      (ignore-errors (image-daemon:relay-detach terminal attachment))
      (terminal-ui-stop ui)))
  nil)

(-> test-detached-fullscreen-streaming () null)
(defun test-detached-fullscreen-streaming ()
  "Present a completed streamed answer exactly once after a late relay attach."
  (dolist (initially-attached-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (terminal (localgroup-terminal-create))
             (ui (fullscreen-test--ui terminal))
             (application (make-instance 'application
                                         :configuration configuration
                                         :conversation conversation
                                         :ui ui))
             (observer (application-agent-observer application))
             (send-text (callback-agent-observer-text-callback observer))
             (send-status (callback-agent-observer-status-callback observer))
             (question "May I proceed with the startup wrapper?")
             (answer (format nil "The patch is verified.~%~A" question))
             (attachment nil))
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
                 (let ((frame (fullscreen-test--frame active)))
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

(-> test-terminal-fullscreen-clicks () null)
(defun test-terminal-fullscreen-clicks ()
  "Test clicks resolve to copy widgets and web URLs across streaming, scrolling, and deferral."
  (let ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
        (actions nil))
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
      (terminal-ui-process-event ui '(:click 10 1))
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "clicking a URL opens it")
      (terminal-ui-process-event ui '(:click 2 1))
      (test-assert (= 1 (length actions)) "clicking plain text does nothing")
      (terminal-ui-process-event ui '(:click 9 2))
      (test-assert (equal '(:copy "(+ 1 2)") (first actions))
                   "clicking the copy label copies the fence source")
      (terminal-ui-process-event ui '(:click 30 2))
      (test-assert (= 2 (length actions)) "clicking past the row end does nothing")
      (terminal-ui-stream-update
       ui
       :rows (list (list (terminal-span ':plain "streamed ")
                         (termdown:make-widget ':code-copy "⧉ copy" '(:copy "streamed source"))))
       :tail nil)
      (terminal-ui-process-event ui '(:click 12 4))
      (test-assert (equal '(:copy "streamed source") (first actions))
                   "streamed rows keep their click regions")
      (terminal-ui-append-finalized-batch
       ui (loop for index below 30
                collect (list index (format nil "row https://example.com/~D" index))))
      (terminal-ui-process-event ui ':scroll-top)
      (terminal-ui-process-event ui '(:click 10 1))
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "clicks after scrolling resolve through the visible top row")
      (terminal-ui-process-event ui ':scroll-bottom)
      (terminal-ui-process-event ui '(:click 10 2))
      (test-assert (equal '(:open-url "https://example.com/26") (first actions))
                   "clicks while following the tail resolve through the painted window")
      (terminal-ui-resize ui 20 :rows 12)
      (terminal-ui-process-event ui ':scroll-top)
      (terminal-ui-process-event ui '(:click 5 2))
      (test-assert (equal '(:open-url "https://example.com/doc") (first actions))
                   "reflowed rows still resolve the whole URL from their chunk")
      (setf (terminal-ui-live-output-suspended-p ui) t)
      (terminal-ui-append-finalized
       ui ':deferred
       (list (termdown:make-widget ':code-copy "⧉ copy" '(:copy "deferred"))))
      (setf (terminal-ui-live-output-suspended-p ui) nil)
      (terminal-ui--paint-live ui)
      (terminal-ui-process-event ui ':scroll-bottom)
      (terminal-ui-process-event ui '(:click 1 8))
      (test-assert (equal '(:copy "deferred") (first actions))
                   "regions deferred during direct terminal I/O survive the resume")
      (setf (terminal-ui-action-function ui) nil)
      (terminal-ui-process-event ui '(:click 1 8))
      (test-assert (= 7 (length actions))
                   "clicks without an installed action function are ignored")))
  nil)

(-> test-localgroup-click-events () null)
(defun test-localgroup-click-events ()
  "Test relayed click packets are accepted only with bounded one-based coordinates."
  (dolist (case '(((:click 3 7) t)
                  ((:click 1 1) t)
                  ((:click 0 7) nil)
                  ((:click 3 0) nil)
                  ((:click 3 7 1) nil)
                  ((:click "3" 7) nil)
                  ((:click 3 10001) nil)
                  ((:scroll -1) t)))
    (destructuring-bind (event expected) case
      (test-assert (eq expected (localgroup--terminal-event-p event))
                   (format nil "relay validation of ~S" event))))
  nil)


;;;; -- Message Jumps and Exit Epilogue --


(-> test-terminal-fullscreen-message-jumps () null)
(defun test-terminal-fullscreen-message-jumps ()
  "Test Ctrl-Page jumps land on user and assistant headers while skipping activity rows."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui (terminal-ui-create :terminal terminal :fullscreen-p t :prompt "> "
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
      (terminal-ui-process-event active ':previous-section)
      (let ((top (fullscreen-test--top active)))
        (test-assert (and top (uiop:string-prefix-p "❯ you 12:03" (fullscreen-test--row-text active top)))
                     "Ctrl-PgUp from the tail tops the newest header above the window"))
      (terminal-ui-process-event active ':previous-section)
      (let ((top (fullscreen-test--top active)))
        (test-assert (and top (uiop:string-prefix-p "● autolith" (fullscreen-test--row-text active top)))
                     "a second Ctrl-PgUp skips tool rows and tops the previous assistant header"))
      (terminal-ui-process-event active ':next-section)
      (let ((top (fullscreen-test--top active)))
        (test-assert (and top (uiop:string-prefix-p "❯ you 12:03" (fullscreen-test--row-text active top)))
                     "Ctrl-PgDn returns to the following user header"))
      (terminal-ui-process-event active ':next-section)
      (test-assert (null (fullscreen-test--top active))
                   "jumping past the final window follows the tail again")
      (terminal-ui-process-event active ':scroll-top)
      (terminal-ui-process-event active ':previous-section)
      (test-assert (eql 0 (fullscreen-test--top active))
                   "Ctrl-PgUp at the first row stays put")
      (terminal-ui-process-event active ':next-section)
      (let ((top (fullscreen-test--top active)))
        (test-assert (and top (uiop:string-prefix-p "● autolith" (fullscreen-test--row-text active top)))
                     "Ctrl-PgDn from the first user header reaches the first assistant header"))
      (test-assert (string= "draft" (progn (terminal-ui-set-input active "draft")
                                           (line-editor-text (terminal-ui-editor active))))
                   "message jumps leave the draft alone")))
  nil)

(-> test-terminal-fullscreen-exit-epilogue () null)
(defun test-terminal-fullscreen-exit-epilogue ()
  "Test epilogue text reaches the normal screen after the alternate buffer is left."
  (let* ((terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui (fullscreen-test--ui terminal)))
    (terminal-ui-start ui)
    (terminal-ui-append-finalized ui ':first "transcript row")
    (terminal-ui-set-epilogue ui (list (terminal-span ':dim "To resume this conversation, run:")
                                       (terminal-span ':plain (string #\Newline))
                                       (terminal-span ':code "  autolith resume abc")))
    (recording-terminal-reset terminal)
    (terminal-ui-stop ui)
    (let* ((output (recording-terminal-output terminal))
           (leave (search (format nil "~C[?1049l" #\Escape) output))
           (advice (search "autolith resume abc" output)))
      (test-assert (and leave advice (< leave advice))
                   "the epilogue is written after the alternate buffer is restored")))
  nil)


(-> test-terminal-fullscreen-forced-exit-resume () null)
(defun test-terminal-fullscreen-forced-exit-resume ()
  "Test forced Ctrl-C restores fullscreen before printing the resume command."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "fullscreen-force-resume"))
         (terminal (make-instance 'recording-terminal :columns 40 :rows 12))
         (ui (fullscreen-test--ui terminal))
         (application (make-instance 'application
                                      :configuration configuration
                                      :conversation conversation
                                      :ui ui))
         (forced-status nil)
         (controller nil))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "keep this conversation")
           (setf controller
                 (make-instance 'application-input-controller
                                :application application
                                :main-thread (current-thread)
                                :interrupt-clock-function (lambda () 10)
                                :forced-exit-function (lambda (status)
                                                        (setf forced-status status))))
           (setf (application-input-controller application) controller
                 (application-input-controller-active-p controller) t)
           (with-terminal-ui (active-ui ui)
             (declare (ignore active-ui))
             (application-input-controller--process-event controller ':interrupt)
             (application-input-controller--process-event controller ':interrupt))
           (let* ((output (recording-terminal-output terminal))
                  (leave (search (format nil "~C[?1049l" #\Escape) output))
                  (resume (search "autolith resume fullscreen-force-resume" output)))
             (test-assert (= forced-status *application-forced-interrupt-status*)
                          "fullscreen forced interruption exits with status 130")
             (test-assert (and leave resume (< leave resume))
                          "fullscreen forced interruption restores the screen before its resume command")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
    nil))
