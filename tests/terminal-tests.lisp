(in-package #:autolith)

;;;; -- Recording Terminal --

(defclass recording-terminal (terminal)
  ((chunks
    :initform nil
    :accessor recording-terminal-chunks
    :type list
    :documentation "Trusted renderer writes captured in reverse chronological order."))
  (:documentation "A deterministic interactive terminal used by terminal seam tests."))

(defmethod terminal--write ((terminal recording-terminal) (text string))
  "Capture trusted TEXT written through TERMINAL."
  (push text (recording-terminal-chunks terminal))
  nil)

(defmethod terminal-flush ((terminal recording-terminal))
  "Finish a recording TERMINAL write batch without external effects."
  (declare (ignore terminal))
  nil)

(defmethod terminal-start ((terminal recording-terminal))
  "Start TERMINAL and emulate only bracketed paste activation."
  (unless (terminal-started-p terminal)
    (setf (terminal-started-p terminal) t
          (terminal-interactive-p terminal) t)
    (terminal--write terminal (terminal-bracketed-paste-enable-sequence)))
  terminal)

(defmethod terminal-stop ((terminal recording-terminal))
  "Stop TERMINAL and emulate only bracketed paste deactivation."
  (when (terminal-started-p terminal)
    (terminal--write terminal (terminal-bracketed-paste-disable-sequence))
    (setf (terminal-started-p terminal) nil
          (terminal-interactive-p terminal) nil))
  terminal)

(defmethod terminal-read-event ((terminal recording-terminal))
  "Return end-of-input because recording terminals have no input queue."
  (declare (ignore terminal))
  :end-of-input)


;;;; -- Scripted Terminal --

(defclass scripted-terminal (recording-terminal)
  ((events
    :initarg :events
    :initform nil
    :accessor scripted-terminal-events
    :type list
    :documentation "Queued semantic input events served to the reader in order.")
   (read-callback
    :initarg :read-callback
    :initform nil
    :reader scripted-terminal-read-callback
    :type (option function)
    :documentation "The optional callback invoked immediately before returning an event."))
  (:documentation "A recording terminal replaying scripted input events."))

(defclass failing-recording-terminal (recording-terminal)
  ((fail-next-write-p
    :initform nil
    :accessor failing-recording-terminal-fail-next-write-p
    :type boolean
    :documentation "Whether the next trusted write should signal a terminal failure."))
  (:documentation "A recording terminal with one explicitly injected write failure."))

(defmethod terminal--write ((terminal failing-recording-terminal) (text string))
  "Fail or capture trusted TEXT according to TERMINAL's injection state."
  (if (failing-recording-terminal-fail-next-write-p terminal)
      (progn
        (setf (failing-recording-terminal-fail-next-write-p terminal) nil)
        (error 'terminal-error
               :message "Injected terminal write failure."
               :operation ':write
               :cause nil))
      (call-next-method)))

(defmethod terminal-read-event ((terminal scripted-terminal))
  "Serve the next scripted event, or end of input when exhausted."
  (let ((callback (scripted-terminal-read-callback terminal)))
    (when callback
      (funcall callback)))
  (or (pop (scripted-terminal-events terminal)) :end-of-input))


;;;; -- Test Helpers --

(-> recording-terminal-output (recording-terminal) string)
(defun recording-terminal-output (terminal)
  "Return all output captured by TERMINAL in write order."
  (with-output-to-string (stream)
    (dolist (chunk (reverse (recording-terminal-chunks terminal)))
      (write-string chunk stream))))

(-> recording-terminal-reset (recording-terminal) recording-terminal)
(defun recording-terminal-reset (terminal)
  "Discard output previously captured by TERMINAL."
  (setf (recording-terminal-chunks terminal) nil)
  terminal)

(-> terminal-tests--substring-count (string string) (integer 0))
(defun terminal-tests--substring-count (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(-> terminal-tests--csi-final-index (string integer) (option integer))
(defun terminal-tests--csi-final-index (text start)
  "Return the final-byte index for a CSI in TEXT beginning at START."
  (loop for index from start below (length text)
        when (<= #x40 (char-code (char text index)) #x7e)
          return index))

(-> terminal-tests--private-mode-parameters-p (string integer integer) boolean)
(defun terminal-tests--private-mode-parameters-p (text start end)
  "Return true when TEXT parameters between START and END select an alternate screen."
  (and (< start end)
       (char= (char text start) #\?)
       (loop with parameter-start = (1+ start)
             for separator = (or (position #\; text
                                           :start parameter-start
                                           :end end)
                                 end)
             for value = (parse-integer text
                                        :start parameter-start
                                        :end separator
                                        :junk-allowed t)
             thereis (member value '(47 1047 1049))
             while (< separator end)
             do (setf parameter-start (1+ separator)))))

(-> terminal-tests--forbidden-control-p (string) boolean)
(defun terminal-tests--forbidden-control-p (text)
  "Return true when TEXT enters an alternate screen or erases a display or scrollback."
  (block nil
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          for code = (char-code character)
          do (cond
               ((and (= code 27)
                     (< (1+ index) (length text))
                     (char= (char text (1+ index)) #\c))
                (return t))
               ((or (and (= code 27)
                         (< (1+ index) (length text))
                         (char= (char text (1+ index)) #\[))
                    (= code #x9b))
                (let* ((parameter-start (if (= code #x9b)
                                            (1+ index)
                                            (+ index 2)))
                       (final-index
                         (terminal-tests--csi-final-index text parameter-start)))
                  (unless final-index
                    (return t))
                  (let ((final (char text final-index)))
                    (when (or (char= final #\J)
                              (and (member final '(#\h #\l))
                                   (terminal-tests--private-mode-parameters-p
                                    text parameter-start final-index)))
                      (return t)))
                  (setf index final-index)))
               (t
                nil))
             (incf index))
    nil))

(-> terminal-tests--contains-control-character-p (string) boolean)
(defun terminal-tests--contains-control-character-p (text)
  "Return true when TEXT contains an untrusted ESC or C1 control character."
  (loop for character across text
        for code = (char-code character)
        thereis (or (= code 27)
                    (<= 128 code 159))))


;;;; -- Focused Terminal Tests --

(-> test-terminal-primary-screen-controls () null)
(defun test-terminal-primary-screen-controls ()
  "Test primary-screen rendering, bounded live updates, and finalized deduplication."
  (let* ((terminal (make-instance 'recording-terminal :columns 24))
         (ui (terminal-ui-create :terminal terminal :prompt "autolith> ")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (terminal-ui-process-event active-ui '(:insert "hello"))
      (test-assert
       (terminal-ui-append-finalized active-ui 1 "FINAL-SENTINEL")
       "the first finalized transcript event is emitted")
      (test-assert
       (not (terminal-ui-append-finalized active-ui 1 "DUPLICATE"))
       "a finalized transcript identifier is emitted only once")
      (terminal-ui-set-status active-ui "tool complete")
      (terminal-ui-resize active-ui 12))
    (let ((output (recording-terminal-output terminal)))
      (test-assert
       (= (terminal-tests--substring-count "FINAL-SENTINEL" output) 1)
       "finalized transcript text appears exactly once")
      (test-assert
       (not (search "DUPLICATE" output))
       "duplicate finalized transcript text is absent")
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "terminal output never clears a display or enters an alternate screen")))
  nil)

(-> test-terminal-finalized-batch () null)
(defun test-terminal-finalized-batch ()
  "Test finalized batches deduplicate entries and use one transcript payload."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':first "BATCH-FIRST")
                 (list ':second "BATCH-SECOND")
                 (list ':first "BATCH-DUPLICATE")))
          2)
       "a finalized batch reports only its distinct new identifiers")
      (let* ((chunks (reverse (recording-terminal-chunks terminal)))
             (payload-chunks
               (remove-if-not
                (lambda (chunk)
                  (or (search "BATCH-FIRST" chunk)
                      (search "BATCH-SECOND" chunk)
                      (search "BATCH-DUPLICATE" chunk)))
                chunks))
             (output (recording-terminal-output terminal)))
        (test-assert
         (and (= (length payload-chunks) 1)
              (search "BATCH-FIRST" (first payload-chunks))
              (search "BATCH-SECOND" (first payload-chunks)))
         "one batch reaches the live region as one combined transcript payload")
        (test-assert
         (and (< (search "BATCH-FIRST" output)
                 (search "BATCH-SECOND" output))
              (not (search "BATCH-DUPLICATE" output)))
         "batch output preserves order and suppresses duplicate identifiers"))
      (recording-terminal-reset terminal)
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':first "BATCH-OLD")
                 (list ':third "BATCH-THIRD")))
          1)
       "later batches omit identifiers finalized by an earlier batch")
      (let ((output (recording-terminal-output terminal)))
       (test-assert
         (and (search "BATCH-THIRD" output)
              (not (search "BATCH-OLD" output)))
         "a later batch emits only its newly finalized entry"))))
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (setf (failing-recording-terminal-fail-next-write-p terminal) t)
      (test-assert
       (handler-case
           (progn
             (terminal-ui-append-finalized-batch
              active-ui
              (list (list ':retry "BATCH-RETRY")))
             nil)
         (terminal-error ()
           t))
       "a failed terminal write remains visible to the caller")
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':retry "BATCH-RETRY")))
          1)
       "a failed batch leaves its identifier available for retry")
      (test-assert
       (= (terminal-tests--substring-count
           "BATCH-RETRY"
           (recording-terminal-output terminal))
          1)
       "retry emits the batch exactly once after a pre-write failure"))
  nil))

(-> test-terminal-untrusted-text () null)
(defun test-terminal-untrusted-text ()
  "Test that every untrusted text path neutralizes terminal control injection."
  (let* ((escape (string *terminal-escape-character*))
         (c1 (string (code-char #x9b)))
         (malicious
           (concatenate 'string
                        "before"
                        escape "[?1049h"
                        escape "[3J"
                        escape "c"
                        c1 "?47h"
                        "after"))
         (terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :prompt malicious)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui malicious)
      (terminal-ui-process-event active-ui (list :paste malicious))
      (terminal-ui-append-finalized active-ui :malicious malicious))
    (let ((output (recording-terminal-output terminal))
          (editor-text (line-editor-text (terminal-ui-editor ui))))
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "untrusted content cannot inject forbidden terminal controls")
      (test-assert
       (not (terminal-tests--contains-control-character-p editor-text))
       "pasted input stores no ESC or C1 control characters")))
  nil)

(-> test-terminal-finalized-scrollback () null)
(defun test-terminal-finalized-scrollback ()
  "Test that resize and live activity never replay finalized transcript rows."
  (let* ((terminal (make-instance 'recording-terminal :columns 18))
         (ui (terminal-ui-create :terminal terminal)))
    (terminal-ui-start ui)
    (loop for identifier from 1 to 20
          do (terminal-ui-append-finalized
              ui
              identifier
              (format nil "IMMUTABLE-~2,'0D" identifier)))
    (recording-terminal-reset terminal)
    (terminal-ui-set-status ui "streaming token one")
    (terminal-ui-set-status ui "streaming token two")
    (terminal-ui-process-event ui '(:insert "draft"))
    (terminal-ui-resize ui 9)
    (let ((live-output (recording-terminal-output terminal)))
      (loop for identifier from 1 to 20
            do (test-assert
                (not (search (format nil "IMMUTABLE-~2,'0D" identifier)
                             live-output))
                "live repaint does not replay finalized transcript text"))
      (test-assert
       (not (terminal-tests--forbidden-control-p live-output))
       "live repaint and resize preserve terminal scrollback"))
    (terminal-ui-stop ui))
  nil)

(-> test-terminal-resize-frame () null)
(defun test-terminal-resize-frame ()
  "Test that a wider terminal resize replaces the reflowed live region once."
  (let* ((terminal (make-instance 'recording-terminal :columns 8))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event
       active-ui
       '(:insert "a long wrapped input line"))
      (recording-terminal-reset terminal)
      (terminal-ui-resize active-ui 24)
      (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                   "resize reflows and replaces the live region in one frame")))
  nil)

(-> test-terminal-relayed-resize () null)
(defun test-terminal-relayed-resize ()
  "Test relayed client resizes reflowing geometry with composition."
  (let ((*terminal-relayed-resize* nil)
        (*terminal-resize-pending-p* nil))
    (let ((localgroup-terminal (make-instance 'localgroup-terminal
                                              :columns 60
                                              :rows 20)))
      (localgroup-terminal-resize localgroup-terminal 24 120 t)
      (test-assert (= (terminal-columns localgroup-terminal) 60)
                   "a relayed resize packet does not poke dimension slots")
      (test-assert (equal *terminal-relayed-resize* '(24 . 120))
                   "a relayed resize packet parks the exact client size"))
    (setf *terminal-resize-pending-p* t)
    (test-assert (equal (application-pending-terminal-size) '(24 . 120))
                 "a relayed size takes precedence over local measurement")
    (test-assert (and (null *terminal-relayed-resize*)
                      (not *terminal-resize-pending-p*))
                 "consuming a relayed size clears both pending signals")
    (test-assert (null (application-pending-terminal-size))
                 "a consumed relayed size does not repeat")
    (setf *terminal-relayed-resize* (cons 30 100)
          *terminal-resize-pending-p* t)
    (let ((consume (symbol-function 'terminal-relayed-resize-consume))
          (published-p nil))
      (test-call-with-function-replacements
       (list
        (list
         'terminal-relayed-resize-consume
         (lambda ()
           (prog1 (funcall consume)
             (unless published-p
               (setf published-p t)
               (terminal-relayed-resize-publish 31 101))))))
       (lambda ()
         (test-assert
          (equal (application-pending-terminal-size) '(30 . 100))
          "the consumer returns the resize it atomically removed")
         (test-assert
          (equal (application-pending-terminal-size) '(31 . 101))
          "a resize published during consumption remains pending")
         (test-assert
          (and (null *terminal-relayed-resize*)
               (not *terminal-resize-pending-p*))
          "both resize signals clear after consuming the newer relay"))))
    (let* ((terminal (make-instance 'recording-terminal :columns 60))
           (ui (terminal-ui-create :terminal terminal)))
      (with-terminal-ui (active-ui ui)
        (terminal-ui-process-event
         active-ui
         '(:insert "a relayed resize must reflow this wrapped input line"))
        (setf *terminal-relayed-resize* (cons 24 120))
        (recording-terminal-reset terminal)
        (test-assert (terminal-ui-refresh-size
                      active-ui #'application-pending-terminal-size)
                     "the reader applies a relayed size as a UI resize")
        (test-assert (= (terminal-columns terminal) 120)
                     "a relayed resize updates the composed row width")
        (test-assert (= (clinedi:live-region-columns
                         (terminal-ui-live-region active-ui))
                        120)
                     "a relayed resize reflows the live-region geometry")
        (test-assert (plusp (length (recording-terminal-chunks terminal)))
                     "a relayed resize repaints without waiting for input"))))
  nil)

(-> test-terminal-line-editor () null)
(defun test-terminal-line-editor ()
  "Test Autolith event dispatch, submission, control policy, and reader actions."
  (let* ((raw-content
           (format nil "a~Cb~Cc~Cd"
                   #\Tab #\Return *terminal-escape-character*))
         (terminal (make-instance 'recording-terminal :columns 40))
         (editor (line-editor-create :text raw-content))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (multiple-value-bind (text display cursor)
        (terminal-ui--live-content ui)
      (declare (ignore display cursor))
      (test-assert
       (not (terminal-tests--contains-control-character-p text))
       "externally supplied editor controls are sanitized before display")))
  (let* ((terminal (make-instance 'recording-terminal :columns 12))
         (editor (line-editor-create :history-limit 2))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "abc"))
      (terminal-ui-process-event active-ui ':left)
      (terminal-ui-process-event active-ui '(:insert "X"))
      (test-assert (string= (line-editor-text editor) "abXc")
                   "terminal events edit the active input buffer")
      (terminal-ui-process-event active-ui ':insert-newline)
      (terminal-ui-process-event active-ui '(:insert "second"))
      (multiple-value-bind (action submitted)
          (terminal-ui-process-event active-ui ':submit)
        (test-assert (eq action ':submit)
                     "the terminal dispatches submission")
        (test-assert (string= submitted (format nil "abX~%secondc"))
                     "submission returns the complete multiline input"))
      (terminal-ui-process-event active-ui '(:insert "draft"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':interrupt)
        (declare (ignore payload))
        (test-assert (eq action ':cleared)
                     "interrupt clears non-empty editor input"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':interrupt)
        (declare (ignore payload))
        (test-assert (eq action ':interrupt)
                     "interrupt propagates when the editor is empty"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':end-of-input)
        (declare (ignore payload))
        (test-assert (eq action ':end-of-input)
                     "end of input propagates when the editor is empty"))))
  nil)

(-> test-terminal-history-replacement () null)
(defun test-terminal-history-replacement ()
  "Test bounded history loading preserves the active draft and cursor."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (editor
           (line-editor-create
            :history '("older" "newer")
            :history-limit 2))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (terminal-ui-set-input ui "draft")
    (terminal-ui-process-event ui :history-previous)
    (terminal-ui-process-event ui :left)
    (terminal-ui-load-history ui '("one" "two" "three"))
    (test-assert
     (equalp (line-editor-history editor) #("two" "three"))
     "history loading honors the target editor's custom limit")
    (test-assert
     (and (string= (line-editor-text editor) "draft")
          (= (line-editor-cursor editor) 5)
          (not (terminal-ui--editor-history-navigating-p editor)))
     "history loading restores the draft and leaves obsolete traversal")
    (terminal-ui-process-event ui :left)
    (terminal-ui-load-history ui '("alpha" "beta"))
    (test-assert
     (and (string= (line-editor-text editor) "draft")
          (= (line-editor-cursor editor) 4))
     "history loading preserves an ordinary draft cursor exactly")
    (terminal-ui-process-event ui :submit)
    (test-assert
     (equalp (line-editor-history editor) #("beta" "draft"))
     "replacement history remains extendable and bounded"))
  nil)


(-> test-terminal-image-attachments () null)
(defun test-terminal-image-attachments ()
  "Test pasted image labels, submission payloads, pruning, and history recall."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-image (merge-pathnames "first image.png" root))
         (second-image (merge-pathnames "second image.png" root))
         (terminal (make-instance 'recording-terminal :columns 40))
         (editor (line-editor-create))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (unwind-protect
         (progn
           (test-conversation--write-tiny-png first-image)
           (test-conversation--write-tiny-png second-image)
           (with-terminal-ui (active-ui ui)
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring first-image))))
             (terminal-ui-process-event active-ui '(:insert " describe this"))
             (test-assert
              (string= (line-editor-text editor)
                       "[Image #1] describe this")
              "pasting an image pathname inserts a numbered image label")
             (multiple-value-bind (action submitted)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert (eq action :submit)
                            "an image draft remains an ordinary submission")
               (test-assert
                (and (typep submitted 'user-message-input)
                     (string= (user-message-input-text submitted)
                              "[Image #1] describe this")
                     (equal (user-message-input-image-pathnames submitted)
                            (list (truename first-image))))
                "image submission preserves text and the absolute local path"))
             (terminal-ui-process-event active-ui :history-previous)
             (multiple-value-bind (action recalled)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert
                (and (eq action :submit)
                     (typep recalled 'user-message-input)
                     (equal (user-message-input-image-pathnames recalled)
                            (list (truename first-image))))
                "Clinedi history recall restores image attachment metadata"))
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring first-image))))
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring second-image))))
             (line-editor-set-text editor "[Image #2] only")
             (multiple-value-bind (action pruned)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert
                (and (eq action :submit)
                     (typep pruned 'user-message-input)
                     (string= (user-message-input-text pruned)
                              "[Image #1] only")
                     (equal (user-message-input-image-pathnames pruned)
                            (list (truename second-image))))
                "deleted image labels prune attachments and renumber survivors"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


(-> test-terminal-input-decoding () null)
(defun test-terminal-input-decoding ()
  "Test the application's literal Ctrl-V policy over Clinedi buffered input."
  (let* ((payload (format nil "first~%second"))
         (terminal (stream-terminal-create
                    :input-stream (make-string-input-stream
                                   (concatenate 'string (string (code-char 22)) payload))
                    :output-stream (make-string-output-stream)
                    :input-file-descriptor -1)))
    (setf (terminal-interactive-p terminal) t)
    (test-assert (equal (terminal-read-event terminal) (list ':paste payload))
                 "application Ctrl-V input becomes a single literal paste")
    (test-assert (eq (terminal-read-event terminal) ':stream-end)
                 "the paste consumes exactly its buffered input"))
  nil)

(-> test-terminal-context-meter () null)
(defun test-terminal-context-meter ()
  "Test the persistent context meter, threshold marker, and narrow presentation."
  (flet ((row-text (ui width)
           (format nil "~{~A~}"
                   (mapcar #'terminal-span-text
                           (terminal-ui--status-row-at ui 0 width)))))
    (let* ((terminal (make-instance 'recording-terminal :columns 120))
           (ui (terminal-ui-create :terminal terminal)))
      (with-terminal-ui (active-ui ui)
        (terminal-ui-set-context-usage active-ui
                                       :used 0
                                       :window 1050000
                                       :compaction-limit 840000)
        (test-assert (terminal-ui--status-row-visible-p active-ui)
                     "context usage keeps the modeline visible while idle")
        (test-assert
         (search "ctx [..........|..] 0 / 1.05M used"
                 (row-text active-ui 120))
         "an empty context meter shows its full window and compaction marker")
         (terminal-ui-set-idle-status-details
          active-ui
          (list (terminal-span ':status-model "gpt-5.6-sol")
                (terminal-span ':status-dim " · high · git ")
                (terminal-span ':status-branch "master")))
         (let ((idle (row-text active-ui 120)))
           (test-assert
            (and (search "gpt-5.6-sol · high · git master" idle)
                 (search "ctx [..........|..] 0 / 1.05M used" idle))
            "the idle context row fills its left side with static runtime details"))
        (terminal-ui-set-context-usage active-ui
                                       :used 262500
                                       :window 1050000
                                       :compaction-limit 840000)
        (test-assert
         (search "ctx [===.......|..] 263K / 1.05M used"
                 (row-text active-ui 120))
         "a quarter-full context meter shows used and window tokens")
        (terminal-ui-set-context-usage active-ui
                                       :used 840000
                                       :window 1050000
                                       :compaction-limit 840000)
        (test-assert
         (search "ctx [==========|..] 840K / 1.05M used"
                 (row-text active-ui 120))
         "the meter marks the automatic compaction threshold")
        (terminal-ui-set-context-usage active-ui
                                       :used 262500
                                       :window 1050000
                                       :compaction-limit 840000)
        (terminal-ui-set-status active-ui "working")
        (let ((wide (row-text active-ui 120)))
          (test-assert (search "READ  ∙ 00:00" wide)
                       "a wide context row retains live activity")
          (test-assert
           (search "ctx [===.......|..] 263K / 1.05M used" wide)
           "a wide context row retains the full meter")
          (test-assert
           (let ((start (search "ctx [===.......|..] 263K / 1.05M used" wide)))
             (and start
                  (= (+ start (length "ctx [===.......|..] 263K / 1.05M used"))
                     (length wide))))
           "a wide context meter is right-aligned"))
        (dolist (width '(25 30 33))
          (let ((intermediate (row-text active-ui width)))
            (test-assert (search "READ  ∙ 00:00" intermediate)
                         (format nil
                                 "an active ~D-column meter retains minimum activity"
                                 width))
            (test-assert (search "263K/1.05M" intermediate)
                         (format nil
                                 "an active ~D-column meter retains complete counts"
                                 width))
            (test-assert (<= (text-cell-width intermediate) width)
                         (format nil
                                 "an active ~D-column meter never overflows its row"
                                 width))))
        (let ((intermediate (row-text active-ui 30)))
          (test-assert (search "ctx 263K/1.05M" intermediate)
                       "a 30-column meter retains labelled compact context"))
        (let ((intermediate (row-text active-ui 33)))
          (test-assert (search "[=..|.] 263K/1.05M" intermediate)
                       "a 33-column meter selects the largest compact bar"))
        (terminal-ui-set-context-usage active-ui
                                       :used 1050000
                                       :window 1050000
                                       :compaction-limit 840000)
        (dolist (width '(24 25 26 27 28 29 30 31 32 33))
          (let ((high-usage (row-text active-ui width)))
            (test-assert (search "1.05M/1.05M" high-usage)
                         (format nil
                                 "a high-usage ~D-column meter retains complete counts"
                                 width))
            (test-assert (<= (text-cell-width high-usage) width)
                         (format nil
                                 "a high-usage ~D-column meter never overflows its row"
                                 width))))
        (let ((too-narrow (row-text active-ui 24))
              (minimum (row-text active-ui 25))
              (preferred (row-text active-ui 26)))
          (test-assert (not (search "READ  ∙ 00:00" too-narrow))
                       "below the combined minimum, context displaces activity")
          (test-assert
           (string= minimum "READ  ∙ 00:00 1.05M/1.05M")
           "the combined minimum uses a one-cell activity and context gap")
          (test-assert
           (string= preferred "READ  ∙ 00:00  1.05M/1.05M")
           "the next width restores the preferred two-cell gap"))
        (dolist (width '(25 26 27 28 29 30 31 32 33))
          (test-assert (search "READ  ∙ 00:00" (row-text active-ui width))
                       (format nil
                               "a high-usage ~D-column meter retains activity"
                               width)))
        (terminal-ui-set-context-usage active-ui
                                       :used 262500
                                       :window 1050000
                                       :compaction-limit 840000)
        (dolist (width '(12 13))
          (let ((narrow (row-text active-ui width)))
            (test-assert (and (search "263K/1.05M" narrow)
                              (not (search "ctx" narrow)))
                         (format nil
                                 "an active ~D-column meter retains used and window tokens"
                                 width))
            (test-assert (<= (text-cell-width narrow) width)
                         (format nil
                                 "an active ~D-column meter never overflows its row"
                                 width))))
        (let ((narrow (row-text active-ui 14)))
          (test-assert (string= narrow "ctx 263K/1.05M")
                       "an active 14-column meter retains its context label")
          (test-assert (<= (text-cell-width narrow) 14)
                       "an active 14-column meter never overflows its row"))
        (let ((narrow (row-text active-ui 10)))
          (test-assert (string= narrow "263K/1.05M")
                       "an active 10-column meter retains its complete counts"))
        (dolist (width '(1 2 9))
          (let ((narrow (row-text active-ui width)))
            (test-assert (<= (text-cell-width narrow) width)
                         (format nil
                                 "only sub-count widths clip an active meter at ~D columns"
                                 width)))))))
  nil)


(-> test-terminal-bounded-editor-repaint () null)
(defun test-terminal-bounded-editor-repaint ()
  "Test atomic repaint and cursor-following height bounds for long drafts."
  (let* ((terminal (make-instance 'recording-terminal :rows 5 :columns 8))
         (ui (terminal-ui-create :terminal terminal :prompt "❯ "))
         (draft (make-string 160 :initial-element #\x)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-append-finalized active-ui :sentinel "HISTORY-SENTINEL")
      (recording-terminal-reset terminal)
      (terminal-ui-process-event active-ui (list :insert draft))
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "one editor change is one terminal write")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25l" *terminal-escape-character*)
             output)
            1)
         "one editor repaint hides the cursor once")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25h" *terminal-escape-character*)
             output)
            1)
         "one editor repaint restores the cursor once")
        (test-assert (not (search "HISTORY-SENTINEL" output))
                     "long draft repaint never replays scrollback")
        (test-assert (not (terminal-tests--forbidden-control-p output))
                     "long draft repaint never erases the display"))
      (test-assert (= (live-region-maximum-rows
                       (terminal-ui-live-region active-ui))
                      4)
                   "the editor leaves one viewport row outside its live region")
      (test-assert (<= (terminal-ui-live-row-count active-ui) 4)
                   "a long draft remains inside its terminal-height budget")
      (terminal-ui-process-event active-ui :home)
      (test-assert (<= (terminal-ui-live-row-count active-ui) 4)
                   "the bounded viewport follows the cursor to the draft start")))
  nil)


(-> test-terminal-transient-notice () null)
(defun test-terminal-transient-notice ()
  "Test transient notices expire without entering terminal scrollback."
  (let* ((clock 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-notice
       active-ui
       "Press Ctrl-C again within 2.5 seconds to force exit."
       :duration-seconds 5/2)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (search "Press Ctrl-C again" text)
                     "the transient notice appears in the live region"))
      (setf clock 5/2)
      (with-terminal-ui-locked (active-ui)
        (terminal-ui-set-input active-ui "draft"))
      (test-assert (null (terminal-ui-notice active-ui))
                   "any locked repaint clears a notice at its deadline")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "Press Ctrl-C again" text))
                     "the expired notice vanishes from the live region"))
      (setf clock 10)
      (terminal-ui-set-notice
       active-ui
       "Press Ctrl-C again within 2.5 seconds to force exit."
       :duration-seconds 5/2)
      (setf clock 25/2)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader refresh also repaints an expired notice")
      (test-assert (null (terminal-ui-notice active-ui))
                   "the reader refresh clears the renewed notice")))
  nil)

(-> test-terminal-notice-lock-contention () null)
(defun test-terminal-notice-lock-contention ()
  "Test notice updates never wait behind ordinary presentation work."
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (state-lock (make-lock "Autolith notice contention test"))
         (condition (make-condition-variable
                     :name "Autolith notice contention test"))
         (ui-lock-held-p nil)
         (release-ui-lock-p nil)
         (notice-call-returned-p nil)
         (holder nil)
         (setter nil))
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (setf holder
                 (make-thread
                  (lambda ()
                    (with-terminal-ui-locked (ui)
                      (with-lock-held (state-lock)
                        (setf ui-lock-held-p t)
                        (condition-notify condition)
                        (unless release-ui-lock-p
                          (condition-wait condition state-lock :timeout 2)))))
                  :name "Autolith notice lock holder"))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held (state-lock) ui-lock-held-p))
             1)
            "the contention test holds the presentation lock")
           (setf setter
                 (make-thread
                  (lambda ()
                    (terminal-ui-set-notice
                     ui "must not appear" :duration-seconds 5/2)
                    (with-lock-held (state-lock)
                      (setf notice-call-returned-p t)
                      (condition-notify condition)))
                  :name "Autolith nonblocking notice setter"))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held (state-lock) notice-call-returned-p))
             1)
            "a notice update returns while presentation remains locked")
           (test-assert (null (terminal-ui-notice ui))
                        "a contended notice is dropped instead of delayed"))
      (with-lock-held (state-lock)
        (setf release-ui-lock-p t)
        (condition-notify condition))
      (when setter
        (ignore-errors (join-thread setter)))
      (when holder
        (ignore-errors (join-thread holder)))
      (ignore-errors (terminal-ui-stop ui))))
  nil)

(-> test-terminal-timed-status () null)
(defun test-terminal-timed-status ()
  "Test status animation, elapsed activity, and stale progress timing."
  (let* ((clock 0)
         (clock-calls 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda ()
                                (incf clock-calls)
                                clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (test-assert (= clock-calls 1)
                   "starting activity samples the monotonic clock once")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (search "READ  ∙ 00:00" text)
                     "live activity starts with its spinner and elapsed clock")
        (let ((lines (uiop:split-string text :separator '(#\Newline))))
          (test-assert
           (and (not (search "working" (second lines)))
                (search "working" (third lines)))
           "provider activity stays below rather than inside the modeline")))
      (setf clock 0.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "time within one spinner frame does not repaint activity")
      (setf clock 0.25)
      (let ((calls-before-refresh clock-calls))
        (test-assert (terminal-ui-refresh-status active-ui)
                     "a new spinner frame repaints activity")
        (test-assert (= clock-calls (1+ calls-before-refresh))
                     "one timestamp drives both status signature and paint"))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert (search "EVAL  ∙ 00:00" text)
                     "the spinner advances without shifting the elapsed clock"))
      (setf clock 1)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "a new elapsed second repaints activity")
      (setf clock 29)
      (terminal-ui-note-status-progress active-ui)
      (terminal-ui-refresh-status active-ui)
      (setf clock 58)
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "no update" text))
                     "recent progress keeps the activity from looking stale"))
      (setf clock 59)
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert
         (search "00:59 · no update 00:30" text)
         "stale activity states how long no progress has arrived")
        (let ((lines (uiop:split-string text :separator '(#\Newline))))
          (test-assert
           (and (not (search "working" (second lines)))
                (search "working" (third lines)))
           "stale timing keeps provider activity below the modeline")))
       (terminal-ui-set-agent-activities
        active-ui
        (list
         (list :id "active-child"
               :index 1
               :agent "reviewer"
               :state ':running
               :recent-tools nil
               :request-count 1
               :duration-ms 30000
               :assignment "Review the change."
               :detached t)))
       (multiple-value-bind (text display cursor)
           (terminal-ui--live-content active-ui)
         (declare (ignore display cursor))
         (test-assert (not (search "no update" text))
                      "running child activity suppresses the stale warning")
         (test-assert (and (search "00:59" text)
                           (search "active-child" text))
                      "running child activity keeps compact timing and identity visible"))
       (terminal-ui-set-agent-activities active-ui nil)
       (multiple-value-bind (text display cursor)
           (terminal-ui--live-content active-ui)
         (declare (ignore display cursor))
         (test-assert (search "00:59 · no update 00:30" text)
                      "the stale warning returns after child activity ends"))
      (terminal-ui-note-status-progress active-ui)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "new progress immediately clears the stale status state")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "no update" text))
                     "new progress removes the stale warning"))))
  nil)

(-> test-terminal-compaction-indicator () null)
(defun test-terminal-compaction-indicator ()
  "Test compaction state, refresh timing, and idempotent lifecycle updates."
  (let* ((clock 0)
         (clock-calls 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda ()
                                (incf clock-calls)
                                clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (recording-terminal-reset terminal)
      (terminal-ui-set-compacting active-ui t)
      (test-assert
       (terminal-ui-compacting-p active-ui)
       "starting compaction records active state")
      (test-assert
       (= (terminal-ui-compaction-started-at active-ui) 0)
       "starting compaction records its start time")
      (test-assert
       (= (length (recording-terminal-chunks terminal)) 1)
       "starting compaction repaints")
      (recording-terminal-reset terminal)
      (let ((calls-before-repeat clock-calls))
        (terminal-ui-set-compacting active-ui t)
        (test-assert
         (= clock-calls calls-before-repeat)
         "repeating active compaction does not read the clock")
        (test-assert
         (string= (recording-terminal-output terminal) "")
         "repeating active compaction does not repaint"))
      (setf clock 0.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "time within one compaction frame does not repaint")
      (setf clock 0.25)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "a new compaction frame repaints")
      (recording-terminal-reset terminal)
      (terminal-ui-set-compacting active-ui nil)
      (test-assert
       (not (terminal-ui-compacting-p active-ui))
       "clearing compaction removes active state")
      (test-assert
       (null (terminal-ui-compaction-started-at active-ui))
       "clearing compaction removes timing state")
      (test-assert
       (= (length (recording-terminal-chunks terminal)) 1)
       "clearing compaction repaints")
      (recording-terminal-reset terminal)
      (let ((calls-before-repeat clock-calls))
        (terminal-ui-set-compacting active-ui nil)
        (test-assert
         (= clock-calls calls-before-repeat)
         "repeating cleared compaction does not read the clock")
        (test-assert
         (string= (recording-terminal-output terminal) "")
         "repeating cleared compaction does not repaint")))
  nil))

(-> test-terminal-agent-activities () null)
(defun test-terminal-agent-activities ()
  "Test bounded cumulative traces and expanded blocking child rows."
  (let* ((clock 65)
         (terminal (make-instance 'recording-terminal
                                  :columns 100
                                  :styled-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (activities
           (list
            (list :id "review-agent-2"
                  :index 2
                  :agent "reviewer"
                  :state ':running
                  :current-tool "lisp.eval"
                  :current-tool-duration-ms 60000
                  :recent-tools '("search.content" "resource.read")
                  :request-count 1
                  :duration-ms 65000
                  :assignment "Review the finished patch."
                  :detached nil)
            (list :id "search-1"
                  :index 1
                  :agent "explorer"
                  :state ':running
                  :current-tool "lisp.eval"
                  :current-tool-duration-ms 60000
                  :recent-tools
                  '("search.files" "search.glob" "resource.read" "lisp.load-system")
                  :request-count 2
                  :duration-ms 65000
                  :assignment "Locate the scheduler."
                  :detached t))))
    (let ((oversized-activity (copy-list (first activities))))
      (setf (getf oversized-activity :recent-tools)
            (loop repeat (1+ *task-progress-recent-tool-limit*)
                  collect "tool"))
      (test-assert
       (not (terminal-agent-activity-p oversized-activity))
       "the terminal rejects child traces above its retained milestone bound"))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (recording-terminal-reset terminal)
      (terminal-ui-set-agent-activities active-ui activities)
      (test-assert (string= (recording-terminal-output terminal) "")
                   "child notifications do not paint from worker threads")
      (test-assert
       (equal (mapcar (lambda (activity) (getf activity :id))
                      (terminal-ui-agent-activities active-ui))
              '("search-1" "review-agent-2"))
       "child rows retain scheduler creation order")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader coalesces changed child state into one frame")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore cursor))
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (header-index (position "agents 2" lines :test #'string=))
               (search-index
                 (position-if (lambda (line)
                                (search "search-1" line))
                              lines))
               (review-index
                 (position-if (lambda (line)
                                (search "review-agent-2" line))
                              lines))
               (expanded-index
                 (position-if (lambda (line)
                                (search "↳ " line))
                              lines))
               (status-index
                 (position-if (lambda (line)
                                (search "READ " line))
                              lines))
               (search-line (and search-index (nth search-index lines)))
               (review-line (and review-index (nth review-index lines)))
               (expanded-line (and expanded-index
                                   (nth expanded-index lines)))
               (search-role-position
                 (and search-line (search "explorer" search-line)))
               (review-role-position
                 (and review-line (search "reviewer" review-line)))
               (search-detail-position
                 (and search-line (search " · … ›" search-line)))
               (review-detail-position
                 (and review-line (search " · blocking" review-line))))
          (test-assert
           (and header-index
                search-index
                review-index
                expanded-index
                status-index
                (< status-index header-index)
                (= search-index (+ header-index 2))
                (= review-index (1+ search-index))
                (= expanded-index (1+ review-index))
                (string= (nth (1+ header-index) lines) "")
                (search "working" (nth (1+ expanded-index) lines))
                (string= (nth (+ expanded-index 2) lines) "")
                (uiop:string-prefix-p "  " search-line)
                (uiop:string-prefix-p "  " review-line)
                (uiop:string-prefix-p "      ↳ " expanded-line)
                search-role-position
                review-role-position
                (= (text-cell-width
                    (subseq search-line 0 search-role-position))
                   (text-cell-width
                    (subseq review-line 0 review-role-position)))
                search-detail-position
                review-detail-position
                (= (text-cell-width
                    (subseq search-line 0 search-detail-position))
                   (text-cell-width
                    (subseq review-line 0 review-detail-position)))
                (search
                 "explorer · … › resource.read › lisp.load-system › lisp.eval 01:00"
                 text)
                (search "reviewer · blocking · lisp.eval 01:00" text)
                (search
                 "↳ search.content › resource.read › lisp.eval 01:00 · Review the finished patch."
                 text)
                (not (search "search.files ›" search-line))
                (not (search "search.glob ›" search-line)))
           "child rows show bounded aligned traces below the modeline")
          (test-assert (not (search "async" text))
                       "detached state does not add a redundant row label"))
        (test-assert
         (every (lambda (row)
                  (<= (terminal--spans-width row) 12))
                (terminal-ui--agent-activity-rows-at
                 active-ui clock 12))
         "expanded and compact child rows remain bounded on narrow terminals")
        (test-assert
         (and (search (terminal-style-sequence ':agent-spinner) display)
              (search (terminal-style-sequence ':agent-name) display)
              (search (terminal-style-sequence ':agent-role) display)
              (search (terminal-style-sequence ':agent-tool) display))
         "running child traces use distinct basic-palette semantic colors"))
      (setf clock 65.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "child spinners do not repaint within one animation frame")
      (setf clock 65.25)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "running child spinners advance on the shared cadence")
      (terminal-ui-set-status active-ui nil)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (expanded-index
                 (position-if (lambda (line)
                                (search "↳ " line))
                              lines)))
          (test-assert
           (and (search "search-1" text)
                (search "∙ 00:00" text)
                expanded-index
                (string= (nth (1+ expanded-index) lines) "")
                (non-empty-string-p (nth (+ expanded-index 2) lines)))
           "child traces retain the status animation above the idle prompt")))
      (setf clock 65.5)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "child and status animation continue without provider activity")
      (terminal-ui-set-agent-activities
       active-ui
       (list
        (list :id "blocking-queued"
              :index 1
              :agent "reviewer"
              :state ':queued
              :current-tool nil
              :current-tool-duration-ms nil
              :recent-tools nil
              :request-count 0
              :duration-ms nil
              :assignment "Wait for the implementation."
              :detached nil)))
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert
         (and (search "reviewer · blocking · queued" text)
              (search "↳ Wait for the implementation." text))
         "queued blocking children retain their assignment on an expanded row"))
      (let ((many-activities
              (loop for index from 1 to 10
                    collect
                    (list :id (format nil "worker-~D" index)
                          :index index
                          :agent "worker"
                          :state ':queued
                          :current-tool nil
                          :current-tool-duration-ms nil
                          :recent-tools nil
                          :request-count 0
                          :duration-ms nil
                          :assignment "Wait for capacity."
                          :detached t))))
        (terminal-ui-set-agent-activities active-ui many-activities)
        (terminal-ui-refresh-status active-ui)
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content active-ui clock)
          (declare (ignore display cursor))
          (test-assert
           (and (search "agents 10" text)
                (find-if
                 (lambda (line)
                   (uiop:string-prefix-p "  … 2 more agents" line))
                 (uiop:split-string text :separator '(#\Newline)))
                (not (search "worker-9" text)))
           "the child strip caps agents and summarizes overflow")))
      (terminal-ui-set-agent-activities active-ui nil)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "clearing the final child repaints the live region")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "agents " text))
                     "terminal child state disappears when no jobs remain"))))
  (let* ((short-clock 65)
         (short-terminal (make-instance 'recording-terminal
                                        :columns 80
                                        :rows 10))
         (short-ui
           (terminal-ui-create
            :terminal short-terminal
            :clock-function (lambda () short-clock)))
         (blocking-activities
           (loop for index from 1 to 8
                 collect
                 (list :id (format nil "worker-~D" index)
                       :index index
                       :agent "reviewer"
                       :state ':running
                       :current-tool "lisp.eval"
                       :current-tool-duration-ms 60000
                       :recent-tools '("resource.read")
                       :request-count 1
                       :duration-ms 65000
                       :assignment "Review the implementation."
                       :detached nil))))
    (with-terminal-ui (active-ui short-ui)
      (terminal-ui-set-status active-ui "working")
      (terminal-ui-set-agent-activities active-ui blocking-activities)
      (let* ((rows
               (terminal-ui--agent-activity-rows-at
                active-ui short-clock 80))
             (text
               (format nil "~{~A~^~%~}"
                       (mapcar #'terminal--spans-text rows))))
        (test-assert
         (and (= (terminal-ui--agent-row-budget active-ui) 3)
              (= (length rows) 3)
              (search "agents 8" text)
              (search "worker-1" text)
              (search "blocking · lisp.eval 01:00" text)
              (search "… 7 more agents" text)
              (not (search "worker-2" text)))
         "short terminals preserve one useful blocking row inside their budget"))
      (terminal-ui-refresh-status active-ui)
      (test-assert
       (<= (terminal-ui-live-row-count active-ui)
           (live-region-maximum-rows (terminal-ui-live-region active-ui)))
       "blocking traces remain inside the live-region viewport budget")))
  nil)

(-> test-terminal-command-activities () null)
(defun test-terminal-command-activities ()
  "Test primary command rows, timing, completion retention, and row bounds."
  (let* ((clock 65)
         (terminal (make-instance 'recording-terminal
                                  :columns 90
                                  :rows 12
                                  :styled-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (commands
           (list
            (list :id "exec:2" :type ':tool :index 2
                  :tool "shell.run" :description "Build the release"
                  :state ':queued :duration-ms nil :detached t)
            (list :id "exec:1" :type ':tool :index 1
                  :tool "shell.run" :description "Run repository checks"
                  :state ':running :duration-ms 60000 :detached nil)))
         (agent
           (list :id "review-3" :index 3 :agent "reviewer"
                 :state ':running :current-tool "resource.read"
                 :current-tool-duration-ms 1000 :recent-tools nil
                 :request-count 1 :duration-ms 1000
                 :assignment "Review the patch." :detached t)))
    (test-assert
     (not (terminal-command-activity-p
           (list :id "exec:1" :type ':tool :index 1
                 :tool "shell.run" :description ""
                 :state ':running :duration-ms 0 :detached nil)))
     "command rows require a non-empty display label")
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "receiving response")
      (recording-terminal-reset terminal)
      (terminal-ui-set-agent-activities active-ui (list agent))
      (terminal-ui-set-command-activities active-ui commands)
      (test-assert (string= (recording-terminal-output terminal) "")
                   "command notifications do not paint from worker threads")
      (test-assert
       (equal (mapcar (lambda (activity) (getf activity :id))
                      (terminal-ui-command-activities active-ui))
              '("exec:1" "exec:2"))
       "command rows retain scheduler creation order")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader paints changed command state immediately")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore cursor))
        (test-assert
         (and (search "exec:1" text)
              (search "Run repository checks" text)
              (search "01:00" text)
              (search "exec:2" text)
              (search "Build the release" text)
              (search "queued" text)
              (search "review-3" text)
              (search "receiving response" text)
              (search (terminal-style-sequence ':command-spinner) display)
              (search (terminal-style-sequence ':command-id) display)
              (search (terminal-style-sequence ':command-tool) display))
         "commands render with child and provider activity"))
      (test-assert
       (<= (terminal-ui-live-row-count active-ui)
           (live-region-maximum-rows (terminal-ui-live-region active-ui)))
       "command and child strips share the live-region viewport budget")
      (setf clock 66)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "running command timers advance once per second")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert
         (and (search "Run repository checks" text)
              (search "01:01" text))
         "command elapsed time advances from its observed duration"))
      (terminal-ui-set-command-activities active-ui nil)
      (test-assert
       (and (null (terminal-ui-command-activities active-ui))
            (null (terminal-ui-command-pending-completions active-ui)))
       "painted command rows clear at terminal state")))
  (let* ((clock 0)
         (terminal (make-instance 'recording-terminal :columns 80 :rows 8))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (command-a
           (list :id "exec:1" :type ':tool :index 1
                 :tool "shell.run" :description "Command A"
                 :state ':running :duration-ms 0 :detached nil))
         (command-b
           (list :id "exec:2" :type ':tool :index 2
                 :tool "shell.run" :description "Command B"
                 :state ':running :duration-ms 0 :detached nil)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (terminal-ui-set-command-activities
       active-ui (list command-a command-b))
      (terminal-ui-set-command-activities active-ui (list command-b))
      (terminal-ui-set-command-activities active-ui nil)
      (test-assert
       (equal
        (mapcar (lambda (activity) (getf activity :id))
                (terminal-ui-command-pending-completions active-ui))
        '("exec:1" "exec:2"))
       "concurrent fast completions are retained independently")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader paints the first completed command")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (search "exec:1" output)
              (search "Command A" output)
              (not (search "exec:2" output)))
         "the first command reaches its first viewport-limited paint"))
      (test-assert
       (equal
        (mapcar (lambda (activity) (getf activity :id))
                (terminal-ui-command-pending-completions active-ui))
        '("exec:2"))
       "an unpainted completion survives for the following frame")
      (recording-terminal-reset terminal)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader advances to the remaining completed command")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (search "exec:2" output)
              (search "Command B" output))
         "the second command reaches its own first paint"))
      (test-assert
       (null (terminal-ui-command-pending-completions active-ui))
       "paint releases each pending command completion")
      (recording-terminal-reset terminal)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the following frame removes completed command rows")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (not (search "Command A" output))
              (not (search "Command B" output)))
         "completed command rows do not persist after their first paint"))))
  nil)

(-> test-terminal-stream-update () null)
(defun test-terminal-stream-update ()
  "Test continuous streamed blocks, fluid tail repaint, and block completion."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :placeholder "hint")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-cursor-visible active-ui nil)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update
       active-ui
       :rows (list (list (terminal-span :brand "● autolith"))
                   (list (terminal-span :plain "  first line")))
       :tail "  partial")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "committed rows and tail use one terminal write")
        (test-assert (search "● autolith" output)
                     "streamed rows append the block header")
        (test-assert (search "  first line" output)
                     "streamed rows append committed lines")
        (test-assert (search "  partial" output)
                     "the fluid tail is painted live")
        (test-assert
         (zerop (terminal-tests--substring-count
                 (format nil "~C[?25h" *terminal-escape-character*)
                 output))
         "streaming leaves cursor motion hidden")
        (test-assert (not (terminal-tests--forbidden-control-p output))
                     "streamed rows never erase the display"))
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update
       active-ui
       :tail (list (list (terminal-span ':plain "  alpha beta gamma"))
                   (list (terminal-span ':plain "  delta epsilon"))))
      (let* ((output (recording-terminal-output terminal))
             (first-row (search "  alpha beta gamma" output))
             (second-row (search "  delta epsilon" output)))
        (test-assert (and first-row second-row (< first-row second-row))
                     "a fluid update paints every speculative wrapped row"))
      (terminal-ui-set-cursor-visible active-ui t)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :tail "  partial response")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "a fluid-tail update is one terminal write")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25l" *terminal-escape-character*)
             output)
            1)
         "a fluid-tail update hides cursor motion once")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25h" *terminal-escape-character*)
             output)
            1)
         "a fluid-tail update restores the input cursor once"))
      (terminal-ui-set-cursor-visible active-ui nil)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :rows (list nil) :tail nil)
      (test-assert (not (search "partial" (recording-terminal-output terminal)))
                   "completing a block removes the fluid tail")
      (test-assert (null (terminal-ui-stream-tail active-ui))
                   "a completed block clears the stored tail")
      (terminal-ui-set-cursor-visible active-ui t)
      (test-assert (live-region-cursor-visible-p
                    (terminal-ui-live-region active-ui))
                   "the input cursor can be restored after streaming")))
  (let* ((terminal (make-instance 'recording-terminal
                                  :columns 40
                                  :rows 10))
         (ui (terminal-ui-create :terminal terminal :placeholder "hint")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "receiving response")
      (terminal-ui-set-notice active-ui "Notice" :duration-seconds 100)
      (terminal-ui-set-pending-inputs
       active-ui
       '("steer one" "steer two" "steer three")
       '("follow one" "follow two" "follow three"))
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :tail "  visible response")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (search "  visible response" output)
                     "pending inputs cannot crop the complete streamed response")
        (test-assert (search "hint" output)
                     "the short viewport keeps the editable prompt visible"))))
  nil)

(-> test-terminal-command-completion () null)
(defun test-terminal-command-completion ()
  "Test suggestion filtering, selection movement, acceptance, and submission."
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (completions
           '((:name "/help" :argument nil :description "show this reference")
             (:name "/resume" :argument "ID" :description "load a conversation")
             (:name "/rollback" :argument "ID" :description "select a generation")
             (:name "/quit" :argument nil :description "leave Autolith")))
         (ui (terminal-ui-create :terminal terminal
                                 :completions completions)))
    (with-terminal-ui (active-ui ui)
      (let ((editor (terminal-ui-editor active-ui)))
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (search "/resume ID" painted)
                       "typing a command prefix paints matching suggestions")
          (test-assert (search "/rollback ID" painted)
                       "every matching command is suggested")
          (test-assert (not (search "/quit" painted))
                       "commands outside the typed prefix are not suggested"))
          (recording-terminal-reset terminal)
          (terminal-ui-process-event active-ui :escape)
          (test-assert (not (search "/resume ID"
                                    (recording-terminal-output terminal)))
                       "escape hides a passive completion menu")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "tab cycles to and previews the next command")
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "tab keeps command completion selection active")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/resume ")
                     "repeated tab cycles through command completions")
        (terminal-ui-process-event
         active-ui
         :complete-previous
         :queue-completion-p t
         :queue-editing-p t)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "command completions take precedence over follow-up cycling")
        (terminal-ui-process-event active-ui :complete)
        (terminal-ui-process-event active-ui '(:insert "draft"))
        (test-assert (string= (line-editor-text editor) "/resume draft")
                     "ordinary input retains the selected completion")
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "ordinary input dismisses completion selection")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (terminal-ui-process-event active-ui :history-next)
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "history keys do not hijack an unbegun completion")
        (terminal-ui-process-event active-ui :down)
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "arrow keys begin completion for a typed command prefix")
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "arrow keys move the completion selection")
        (terminal-ui-process-event active-ui :escape)
        (test-assert (string= (line-editor-text editor) "/r")
                     "escape restores the prefix from before completion")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/help"))
        (terminal-ui-process-event active-ui :submit)
        (terminal-ui-process-event active-ui '(:insert "/quit"))
        (terminal-ui-process-event active-ui :submit)
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :history-previous)
        (test-assert (string= (line-editor-text editor) "/quit")
                     "history recall restores the newest command")
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "history recall does not begin completion")
        (test-assert (not (search "leave Autolith"
                                  (recording-terminal-output terminal)))
                     "history recall does not paint command suggestions")
         (test-assert
          (not (terminal-ui-completion-menu-present-p active-ui))
          "history traversal makes retained completion state non-renderable")
         (test-assert
          (null (selector-items (terminal-ui-completion-selector active-ui)))
          "history traversal clears stale completion selector items")
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :up)
        (test-assert (string= (line-editor-text editor) "/help")
                     "arrows continue history through recalled commands")
        (test-assert (not (search "show this reference"
                                  (recording-terminal-output terminal)))
                     "continued history recall still hides command suggestions")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "tab can still begin completion on a recalled command")
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :escape)
        (test-assert (not (search "show this reference"
                                  (recording-terminal-output terminal)))
                     "escape hides the completion menu")
        (test-assert (string= (line-editor-text editor) "/help")
                     "escape restores the recalled history entry")
        (test-assert (terminal-ui--editor-history-navigating-p editor)
                     "escape restores history traversal after completion")
        (terminal-ui-process-event active-ui :down)
        (test-assert (string= (line-editor-text editor) "/quit")
                     "history navigation continues after completion is cancelled")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/q"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action :submit)
                       "enter on an argument-free suggestion submits")
          (test-assert (string= payload "/quit")
                       "enter submits the completed command name"))
        (terminal-ui-process-event active-ui '(:insert "plain text"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :complete)
          (test-assert (eq action ':submit)
                       "idle tab submits outside command completion")
          (test-assert (string= payload "plain text")
                       "idle tab submits the complete editor contents"))
        (terminal-ui-process-event active-ui '(:insert "queued follow-up"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-completion-p t)
          (test-assert (eq action :queue)
                       "tab queues a non-empty draft while a turn is active")
          (test-assert (string= payload "queued follow-up")
                       "queued submission returns the complete draft"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-completion-p t)
          (declare (ignore payload))
          (test-assert (eq action ':edit-queue)
                       "empty active-turn tab requests queued follow-up editing"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete-previous :queue-completion-p t)
          (declare (ignore payload))
          (test-assert
           (not (eq action ':cycle-queue))
           "shift-tab does not cycle without a recalled follow-up"))
        (terminal-ui-set-input active-ui "")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-editing-p t)
          (declare (ignore payload))
          (test-assert
           (eq action ':kept)
           "empty tab keeps an already recalled follow-up selected"))
        (terminal-ui-set-input active-ui "edited follow-up")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui
             :complete-previous
             :queue-editing-p t)
          (test-assert (eq action ':cycle-queue)
                       "shift-tab requests recalled follow-up cycling")
          (test-assert (string= payload "edited follow-up")
                       "follow-up cycling snapshots the edited draft"))
        (let ((image
                (merge-pathnames "follow-up-cycle.png"
                                 (uiop:temporary-directory))))
          (terminal-ui-set-input
           active-ui
           (user-message-input-create
            :text "[Image #1] revise"
            :image-pathnames (list image)))
          (multiple-value-bind (action payload)
              (terminal-ui-process-event
               active-ui
               :complete-previous
               :queue-editing-p t)
            (test-assert
             (and (eq action ':cycle-queue)
                  (typep payload 'user-message-input)
                  (equal (user-message-input-image-pathnames payload)
                         (list image)))
             "follow-up cycling preserves image attachments"))))))
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (completions
           '((:name "/help" :argument nil :description "show this reference")))
         (ui (terminal-ui-create
              :terminal terminal
              :completion-function (lambda () completions))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "/n"))
      (test-assert
       (not (search "/new" (recording-terminal-output terminal)))
       "a dynamic completion provider initially omits unregistered commands")
      (setf completions
            '((:name "/help" :argument nil :description "show this reference")
              (:name "/new" :argument nil :description "start a conversation")))
      (test-assert
       (find "/new"
             (terminal-ui--matching-completions active-ui)
             :key (lambda (entry) (getf entry :name))
             :test #'string=)
       "dynamic command completion observes registry changes without rebuilding UI")))
  nil)

(-> test-terminal-lisp-operation-completion () null)
(defun test-terminal-lisp-operation-completion ()
  "Test registered parenthesized operation suggestions and acceptance."
  (let* ((terminal (make-instance 'recording-terminal :columns 72))
          (completions
            '((:name "/help" :argument nil :description "show this reference")
              (:name "/ste on" :argument nil :description "enable STE")
              (:name "/ste off" :argument nil :description "disable STE")
              (:name "(help)" :argument nil :description "show this reference")
              (:name "(ste \"on\")" :argument nil :description "enable STE")
              (:name "(ste \"off\")" :argument nil :description "disable STE")
              (:name "(resource.read" :argument ":uri URI)"
               :description "read one resource")))
         (ui (terminal-ui-create :terminal terminal :completions completions)))
    (with-terminal-ui (active-ui ui)
      (let ((editor (terminal-ui-editor active-ui)))
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui '(:insert "(r"))
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (search "(resource.read :uri URI)" painted)
                       "typing an opening parenthesis suggests registered tools")
          (test-assert (not (search "(help)" painted))
                       "parenthesized completion filters unrelated operations"))
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "(resource.read ")
                     "tool completion inserts the canonical Lisp function name")
        (terminal-ui--cancel-completion active-ui)
        (terminal-ui-set-input active-ui "(h")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action ':submit)
                       "enter accepts an argument-free Lisp operation")
          (test-assert (string= payload "(help)")
                       "accepted Lisp completion includes its closing parenthesis"))
        (terminal-ui-set-input active-ui " (h")
        (test-assert (null (terminal-ui--matching-completions active-ui))
                     "leading whitespace preserves prose without Lisp completion")
        (terminal-ui-set-input active-ui "(resource.read :uri")
        (test-assert (null (terminal-ui--matching-completions active-ui))
                     "operation completion stops after the function name")
        (terminal-ui-set-input active-ui "/ste ")
        (test-assert
         (equal (mapcar (lambda (entry) (getf entry :name))
                        (terminal-ui--matching-completions active-ui))
                '("/ste on" "/ste off"))
         "slash argument prefixes offer finite command options")
        (terminal-ui-set-input active-ui "(ste ")
        (test-assert
         (equal (mapcar (lambda (entry) (getf entry :name))
                        (terminal-ui--matching-completions active-ui))
                '("(ste \"on\")" "(ste \"off\")"))
         "Lisp argument prefixes offer the same finite command options"))))
  nil)




(-> test-terminal-modal-selection () null)
(defun test-terminal-modal-selection ()
  "Test application picker metadata, callbacks, draft preservation and resize."
  (let* ((terminal (make-instance 'scripted-terminal
                                  :events '((:insert "beta") :submit) :columns 40))
         (ui (terminal-ui-create :terminal terminal))
         (resized-p nil))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-input active-ui "draft")
      (test-assert
       (string= "value-b"
                (terminal-ui-select active-ui :title "choose" :search-p t
                                    :items '((:name "alpha" :description "" :value "value-a")
                                             (:name "beta" :description "" :value "value-b"))
                                    :resize-callback
                                    (lambda () (unless resized-p
                                                 (setf resized-p t) (cons 18 32)))))
       "filtered picker returns an application's explicit candidate value")
      (test-assert (and (= (terminal-columns terminal) 32)
                        (string= (line-editor-text (terminal-ui-editor ui)) "draft")
                        (null (terminal-ui-selector ui)))
                   "modal cleanup preserves the draft and applies relayed dimensions")
      (setf (scripted-terminal-events terminal) '(:down :submit))
      (let ((replaced-p nil))
        (test-assert
         (string= "new-value"
                  (terminal-ui-select
                   active-ui :items '((:name "original" :description ""))
                   :on-event
                   (lambda (event selector)
                     (declare (ignore event selector))
                     (unless replaced-p
                       (setf replaced-p t)
                       '(:replace "new title" ((:name "same" :description "" :value "old-value")
                                                (:name "same" :description "" :value "new-value"))
                                  "new hint" "new-value")))))
         "application callback replacement keeps title, hint and explicit identity semantics"))))
  nil)





(-> terminal-tests--call-without-host-size (function) t)
(defun terminal-tests--call-without-host-size (function)
  "Call FUNCTION while kernel and tput terminal sizes are unavailable.

Resize tests drive size changes through COLUMNS and LINES, which the real
resolution only consults after the kernel and tput sizes. Masking those host
sources keeps the tests deterministic under an interactive terminal."
  (test-call-with-function-replacements
   (list (list 'terminal-file-descriptor-size
               (lambda (file-descriptor)
                 (declare (ignore file-descriptor))
                 (values nil nil)))
         (list 'terminal--query-dimension
               (lambda (capability)
                 (declare (ignore capability))
                 nil)))
   function))




(-> test-terminal-application-read-resize () null)
(defun test-terminal-application-read-resize ()
  "Test that the outer application refreshes size before dispatching a read event."
  (let* ((previous-columns (uiop:getenv "COLUMNS"))
         (previous-lines (uiop:getenv "LINES"))
         (*terminal-resize-pending-p* nil)
         (terminal
           (make-instance
            'scripted-terminal
            :columns 60
            :events (list :submit)
            :read-callback
            (lambda ()
              (sb-posix:setenv "COLUMNS" "19" 1)
              (sb-posix:setenv "LINES" "9" 1)
              (setf *terminal-resize-pending-p* t))))
         (ui (terminal-ui-create :terminal terminal)))
    (unwind-protect
         (terminal-tests--call-without-host-size
          (lambda ()
            (with-terminal-ui (active-ui ui)
              (test-assert
               (eq (application-read-terminal-event active-ui) :submit)
               "the application preserves the event read during resize")
              (test-assert
               (= (terminal-columns terminal) 19)
               "the application refreshes width before event dispatch")
              (test-assert
               (= (terminal-rows terminal) 9)
               "the application refreshes height before event dispatch")
              (test-assert
               (null *terminal-resize-pending-p*)
               "the application consumes a resize raised during read"))))
      (if previous-columns
          (sb-posix:setenv "COLUMNS" previous-columns 1)
          (sb-posix:unsetenv "COLUMNS"))
      (if previous-lines
          (sb-posix:setenv "LINES" previous-lines 1)
          (sb-posix:unsetenv "LINES"))))
  nil)


(-> test-terminal-non-tty-fallback () null)
(defun test-terminal-non-tty-fallback ()
  "Test application submission and transcript output through non-TTY transport."
  (let* ((output (make-string-output-stream))
         (terminal (stream-terminal-create
                    :input-stream (make-string-input-stream (format nil "fallback input~%"))
                    :output-stream output :input-file-descriptor -1 :columns 20))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (multiple-value-bind (action submitted)
          (terminal-ui-process-event active-ui (terminal-ui-read-event active-ui))
        (test-assert (and (eq action ':submit) (string= submitted "fallback input"))
                     "fallback lines reach application submission unchanged"))
      (terminal-ui-set-status active-ui "not printed")
      (terminal-ui-append-finalized active-ui 1 "fallback output"))
    (let ((captured (get-output-stream-string output)))
      (test-assert (and (search "fallback output" captured)
                        (not (find *terminal-escape-character* captured)))
                   "fallback transcript output contains no terminal controls")))
  nil)




(-> test-terminal-prompt-markers () null)
(defun test-terminal-prompt-markers ()
  "Test OSC 133 terminal output and its semantic UI lifecycle."
  (flet ((expected-marker (payload)
           (format nil "~C]133;~A~C~C"
                   *terminal-escape-character*
                   payload
                   *terminal-escape-character*
                   #\\)))
    (let ((prompt-start (expected-marker "A;redraw=0"))
          (input-start (expected-marker "B"))
          (execution-start (expected-marker "C"))
          (success (expected-marker "D;0"))
          (failure (expected-marker "D;7")))
      (let ((terminal
              (make-instance 'prompt-recording-terminal
                             :input-stream (make-string-input-stream "")
                             :output-stream (make-string-output-stream)
                             :input-file-descriptor 0
                             :interactive-p t)))
        (test-assert
         (and (terminal-write-prompt-marker terminal ':prompt-start)
              (equal (reverse
                      (prompt-recording-terminal-chunks terminal))
                     (list prompt-start))
              (= (prompt-recording-terminal-write-count terminal) 1)
              (= (prompt-recording-terminal-flush-count terminal) 1))
         "an interactive prompt marker writes exactly once and flushes once")
        (setf (terminal-interactive-p terminal) nil)
        (test-assert
         (and (not (terminal-write-prompt-marker terminal ':input-start))
              (= (prompt-recording-terminal-write-count terminal) 1)
              (= (prompt-recording-terminal-flush-count terminal) 1))
         "a noninteractive terminal emits and flushes no prompt marker"))
      (let* ((terminal
               (make-instance 'recording-terminal
                              :columns 72
                              :rows 24
                              :styled-p t))
             (ui (terminal-ui-create :terminal terminal)))
        (with-terminal-ui (active-ui ui)
          (terminal-ui-stream-update
           active-ui
           :tail (format nil "stale first row~%stale second row"))
          (let ((painted-row-count (terminal-ui-live-row-count active-ui)))
            (recording-terminal-reset terminal)
            (test-assert
             (and (terminal-ui-open-prompt-block active-ui)
                  (not (terminal-ui-open-prompt-block active-ui)))
             "one idle prompt emits its boundaries only once")
            (let* ((chunks (reverse (recording-terminal-chunks terminal)))
                   (prompt-position (position prompt-start chunks
                                              :test #'string=))
                   (retraction
                     (and prompt-position
                          (plusp prompt-position)
                          (elt chunks (1- prompt-position)))))
              (test-assert
               (and (> painted-row-count 1)
                    retraction
                    (= (count #\Return retraction) painted-row-count)
                    (= (terminal-tests--substring-count
                        (format nil "~C[K" *terminal-escape-character*)
                        retraction)
                       painted-row-count)
                    (notany
                     (lambda (chunk)
                       (search "stale" chunk))
                     (subseq chunks 0 prompt-position)))
               "prompt start follows complete multi-row live-region retraction")))
          (terminal-ui--paint-live active-ui)
          (terminal-ui-set-status active-ui "working")
          (terminal-ui-refresh-status active-ui)
          (terminal-ui-stream-update active-ui :tail "unfinished")
          (terminal-ui-resize active-ui 64 :rows 20)
          (test-assert
           (and (terminal-ui-start-prompt-execution active-ui)
                (not (terminal-ui-start-prompt-execution active-ui)))
           "one submitted prompt starts execution only once")
          (terminal-ui--paint-live active-ui)
          (terminal-ui-set-status active-ui nil)
          (terminal-ui-stream-update active-ui :tail nil)
          (test-assert
           (and (terminal-ui-finish-prompt-block active-ui 7)
                (not (terminal-ui-finish-prompt-block active-ui 7))
                (terminal-ui-open-prompt-block active-ui))
           "one execution completes once before the next prompt opens")
          (let* ((output (recording-terminal-output terminal))
                 (first-prompt (search prompt-start output))
                 (first-input (search input-start output))
                 (execution (search execution-start output))
                 (completion (search failure output))
                 (second-prompt
                   (and first-prompt
                        (search prompt-start output
                                :start2 (+ first-prompt
                                           (length prompt-start)))))
                 (second-input
                   (and first-input
                        (search input-start output
                                :start2 (+ first-input
                                           (length input-start))))))
            (test-assert
             (and first-prompt first-input execution completion
                  second-prompt second-input
                  (< first-prompt first-input execution completion
                     second-prompt second-input)
                  (= (terminal-tests--substring-count prompt-start output) 2)
                  (= (terminal-tests--substring-count input-start output) 2)
                  (= (terminal-tests--substring-count execution-start output) 1)
                  (= (terminal-tests--substring-count failure output) 1))
             "repaints, ticks, streams, and resize preserve one ordered marker block"))))
      (let* ((terminal
               (make-instance 'recording-terminal :columns 40))
             (ui (terminal-ui-create :terminal terminal)))
        (terminal-ui-start ui)
        (unwind-protect
             (progn
               (setf (terminal-interactive-p terminal) nil)
               (recording-terminal-reset terminal)
               (test-assert
                (and (not (terminal-ui-open-prompt-block ui))
                     (not (terminal-ui-start-prompt-execution ui))
                     (not (terminal-ui-finish-prompt-block ui 1))
                     (zerop (length (recording-terminal-output terminal)))
                     (eq (terminal-ui-prompt-marker-state ui) ':closed))
                "a noninteractive UI has no prompt-marker state transitions"))
          (terminal-ui-stop ui)))
      (let* ((terminal
               (make-instance 'recording-terminal :columns 40))
             (ui (terminal-ui-create :terminal terminal))
             (stop-failure (expected-marker "D;1")))
        (terminal-ui-start ui)
        (recording-terminal-reset terminal)
        (terminal-ui-open-prompt-block ui)
        (terminal-ui-start-prompt-execution ui)
        (terminal-ui-stop ui)
        (terminal-ui-stop ui)
        (test-assert
         (and (= (terminal-tests--substring-count
                  stop-failure (recording-terminal-output terminal))
                 1)
              (eq (terminal-ui-prompt-marker-state ui) ':closed))
         "UI shutdown closes one unfinished execution with failure status"))))
  nil)

(-> test-terminal-nonblocking-lock-interrupt () null)
(defun test-terminal-nonblocking-lock-interrupt ()
  "Test deferred interrupts cannot strand the nonblocking UI lock."
  (let* ((ui
           (terminal-ui-create
            :terminal (make-instance 'recording-terminal :columns 80)))
         (lock (terminal-ui-lock ui))
         (grab-mutex (symbol-function 'sb-thread:grab-mutex))
         (interrupted-p nil)
         (function-ran-p nil))
    (handler-case
        (test-call-with-function-replacements
         (list
          (list
           'sb-thread:grab-mutex
           (lambda (mutex &rest arguments)
             (prog1 (apply grab-mutex mutex arguments)
               (sb-thread:interrupt-thread
                (current-thread)
                (lambda ()
                  (error "Injected deferred UI-lock interrupt.")))))))
         (lambda ()
           (terminal-ui--call-with-lock-if-available
            ui
            (lambda ()
              (setf function-ran-p t)))))
      (error ()
        (setf interrupted-p t)))
    (let ((available-p (sb-thread:grab-mutex lock :waitp nil)))
      (unwind-protect
           (test-assert
            (and interrupted-p
                 (not function-ran-p)
                 available-p)
            "a deferred interrupt releases the acquired nonblocking UI lock")
        (when available-p
          (sb-thread:release-mutex lock)))))
  nil)

(-> run-terminal-tests () boolean)
(defun run-terminal-tests ()
  "Run focused terminal seam tests and return true when every assertion succeeds."
  (test-terminal-primary-screen-controls)
  (test-terminal-nonblocking-lock-interrupt)
  (test-terminal-prompt-markers)
  (test-terminal-finalized-batch)
  (test-terminal-untrusted-text)
  (test-terminal-finalized-scrollback)
  (test-terminal-resize-frame)
  (test-terminal-relayed-resize)
  (test-terminal-line-editor)
  (test-terminal-history-replacement)
  (test-terminal-image-attachments)
  (test-terminal-input-decoding)
  (test-terminal-status-worked-time)
  (test-terminal-context-meter)
  (test-terminal-bounded-editor-repaint)
  (test-terminal-transient-notice)
  (test-terminal-notice-lock-contention)
  (test-terminal-timed-status)
  (test-terminal-compaction-indicator)
  (test-terminal-agent-activities)
  (test-terminal-command-activities)
  (test-terminal-stream-update)
  (test-terminal-command-completion)
  (test-terminal-lisp-operation-completion)
  (test-terminal-modal-selection)
  (test-terminal-modal-default-polling)
  (test-terminal-modal-resize)
  (test-terminal-application-read-resize)
  (test-terminal-non-tty-fallback)
  (test-terminal-descriptor-tty-detection)
  t)

(-> test-terminal-status-worked-time () null)
(defun test-terminal-status-worked-time ()
  "Test the right-aligned total worked time on the status row."
  (let* ((columns 96)
         (terminal (make-instance 'recording-terminal
                                  :columns columns
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status
       active-ui
       "working"
       :details (list (terminal-span ':status-model "model"))
       :worked-seconds 3723)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (let ((status-row
                (second (uiop:split-string text :separator '(#\Newline)))))
          (test-assert (= (text-cell-width status-row) columns)
                       "a worked status row spans the full terminal width")
          (test-assert (search "worked 1:02:03" status-row)
                       "the status row shows accumulated plus elapsed work")
          (test-assert
           (let ((start (search "worked 1:02:03" status-row)))
             (and start
                  (= (+ start (length "worked 1:02:03"))
                     (length status-row))))
           "total worked time is right-aligned on the status row")))
      (terminal-ui-set-status active-ui nil)
      (test-assert (null (terminal-ui-status-worked-seconds active-ui))
                   "clearing the status clears the worked baseline")))
  (let* ((columns 24)
         (terminal (make-instance 'recording-terminal
                                  :columns columns
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working" :worked-seconds 3723)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (let ((status-row
                (second (uiop:split-string text :separator '(#\Newline)))))
          (test-assert (not (search "worked" status-row))
                       "a narrow status row drops the worked segment first")))))
  nil)


(defclass prompt-recording-terminal (stream-terminal)
  ((chunks :initform nil :accessor prompt-recording-terminal-chunks
           :documentation "Captured application prompt controls.")
   (flush-count :initform 0 :accessor prompt-recording-terminal-flush-count
                :documentation "Flushes requested by prompt marker publication."))
  (:documentation "A recording transport for application prompt marker integration."))

(defmethod terminal--write ((terminal prompt-recording-terminal) (text string))
  "Capture a prompt control write."
  (push text (prompt-recording-terminal-chunks terminal))
  nil)

(defmethod terminal-flush ((terminal prompt-recording-terminal))
  "Count prompt publication flushes."
  (incf (prompt-recording-terminal-flush-count terminal))
  nil)

(defun prompt-recording-terminal-write-count (terminal)
  "Return the number of captured prompt marker writes."
  (length (prompt-recording-terminal-chunks terminal)))
