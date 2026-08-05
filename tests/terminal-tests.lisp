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

(defclass protocol-recording-stream-terminal (stream-terminal)
  ((chunks
    :initform nil
    :accessor protocol-recording-stream-terminal-chunks
    :type list
    :documentation "Protocol writes captured in reverse order.")
   (write-count
    :initform 0
    :accessor protocol-recording-stream-terminal-write-count
    :type (integer 0)
    :documentation "The number of attempted protocol writes.")
   (fail-write-index
    :initarg :fail-write-index
    :initform nil
    :reader protocol-recording-stream-terminal-fail-write-index
    :type (option integer)
    :documentation "The optional one-based write attempt that signals failure.")
   (flush-count
    :initform 0
    :accessor protocol-recording-stream-terminal-flush-count
    :type (integer 0)
    :documentation "The number of attempted protocol flushes."))
  (:documentation "A stream terminal recording and optionally failing protocol writes."))

(defmethod terminal--write
    ((terminal protocol-recording-stream-terminal) (text string))
  "Capture TEXT or signal the configured protocol-write failure."
  (let ((index (incf (protocol-recording-stream-terminal-write-count terminal))))
    (when (eql index
               (protocol-recording-stream-terminal-fail-write-index terminal))
      (error 'terminal-error
             :message "Injected protocol write failure."
             :operation ':write
             :cause nil))
    (push text (protocol-recording-stream-terminal-chunks terminal)))
  nil)

(defmethod terminal-flush ((terminal protocol-recording-stream-terminal))
  "Record one protocol flush without external output."
  (incf (protocol-recording-stream-terminal-flush-count terminal))
  nil)

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

(-> test-terminal-line-editor () null)
(defun test-terminal-line-editor ()
  "Test Clinedi editing, multiline input, history, and Autolith control policy."
  (let* ((terminal (make-instance 'recording-terminal :columns 10))
         (editor (line-editor-create))
         (ui (terminal-ui-create :terminal terminal
                                 :editor editor
                                 :prompt "猫> ")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "go lin"))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display))
        (test-assert
         (string= (subseq text 0 cursor) "猫> go lin")
         "an exact-width growing input word remains after a wide prompt"))
      (terminal-ui-process-event active-ui '(:insert "e"))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (let ((expected (format nil "猫> go ~%line")))
          (test-assert
           (string= (subseq text 0 cursor) expected)
           "a growing live input word moves intact to the next row")
          (test-assert
           (not (search (format nil "lin~%e") text))
           "live input never leaves a partial word on the previous row")
          (test-assert
           (string= text (clinedi:ansi-strip display))
           "word-aware live input wrapping preserves styled presentation")))
      (line-editor-set-text editor "go line" :cursor 5)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display))
        (test-assert
         (string= (subseq text 0 cursor) (format nil "猫> go ~%li"))
         "a cursor inside a reflowed word follows its source position"))))
  (let* ((raw-content
           (format nil "a~Cb~Cc~Cd"
                   #\Tab #\Return *terminal-escape-character*))
         (terminal (make-instance 'recording-terminal :columns 40))
         (editor (line-editor-create :text raw-content))
         (ui (terminal-ui-create :terminal terminal
                                 :editor editor)))
    (multiple-value-bind (text display cursor)
        (terminal-ui--live-content ui)
      (let ((expected
              (format nil "> a    b~%c~Cd" (code-char #xfffd))))
        (test-assert
         (string= (subseq text 0 cursor) expected)
         "externally supplied editor controls are sanitized before wrapping")
        (test-assert
         (string= text (clinedi:ansi-strip display))
         "sanitized external editor text preserves styled presentation"))))
  (let* ((terminal (make-instance 'recording-terminal :columns 12))
         (editor (line-editor-create :history-limit 2))
         (ui (terminal-ui-create :terminal terminal
                                 :editor editor
                                 :prompt "❯ ")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "abc"))
      (terminal-ui-process-event active-ui :left)
      (terminal-ui-process-event active-ui '(:insert "X"))
      (test-assert (string= (line-editor-text editor) "abXc")
                   "left arrow changes the insertion point")
      (terminal-ui-process-event active-ui :home)
      (terminal-ui-process-event active-ui '(:insert ">"))
      (terminal-ui-process-event active-ui :end)
      (terminal-ui-process-event active-ui :backspace)
      (terminal-ui-process-event active-ui :insert-newline)
      (terminal-ui-process-event active-ui '(:insert "second line"))
      (let ((multiline (format nil ">abX~%second line")))
        (test-assert (string= (line-editor-text editor) multiline)
                     "modified Enter inserts a real logical input line")
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content active-ui)
          (declare (ignore display cursor))
          (test-assert (search multiline text)
                       "the live region preserves explicit input newlines"))
        (test-assert (> (terminal-ui-live-row-count active-ui) 2)
                     "multiline input occupies multiple physical rows")
        (multiple-value-bind (action submitted)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action :submit)
                       "Enter produces a submit action")
          (test-assert (string= submitted multiline)
                       "Enter returns the complete multiline input")))
      (let ((visual-lines (format nil "abcd~%xy~%abcdef")))
        (line-editor-set-text editor visual-lines)
        (terminal-ui-process-event active-ui :up)
        (test-assert (= (line-editor-cursor editor) 7)
                     "up arrow moves to the preceding visual line")
        (terminal-ui-process-event active-ui :down)
        (test-assert (= (line-editor-cursor editor) (length visual-lines))
                     "down arrow restores the preferred visual column")
        (line-editor-clear editor))
      (terminal-ui-process-event active-ui '(:insert "draft"))
      (terminal-ui-process-event active-ui :history-previous)
      (test-assert (string= (line-editor-text editor)
                            (format nil ">abX~%second line"))
                   "up arrow recalls the newest multiline history entry")
      (terminal-ui-process-event active-ui :history-next)
      (test-assert (string= (line-editor-text editor) "draft")
                   "down arrow restores the draft")
      (terminal-ui-process-event active-ui '(:insert " alpha beta  "))
      (terminal-ui-process-event active-ui :kill-word)
      (test-assert (string= (line-editor-text editor) "draft alpha ")
                   "Ctrl-Backspace deletes whitespace and the previous word")
      (terminal-ui-process-event active-ui :word-left)
      (test-assert (= (line-editor-cursor editor) 6)
                   "Ctrl-Left moves to the previous word boundary")
      (terminal-ui-process-event active-ui :word-right)
      (test-assert (= (line-editor-cursor editor) 11)
                   "Ctrl-Right moves to the next word boundary")
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui :interrupt)
        (declare (ignore payload))
        (test-assert (eq action :cleared)
                     "Ctrl-C clears non-empty editor input"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui :interrupt)
        (declare (ignore payload))
        (test-assert (eq action :interrupt)
                     "Ctrl-C interrupts when the editor is empty"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui :end-of-input)
        (declare (ignore payload))
        (test-assert (eq action :end-of-input)
                     "Ctrl-D exits when the editor is empty"))))
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
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-terminal-input-decoding () null)
(defun test-terminal-input-decoding ()
  "Test production escape decoding and complete bracketed paste collection."
  (let* ((escape *terminal-escape-character*)
         (paste-start (format nil "~C[200~~" escape))
         (paste-end (format nil "~C[201~~" escape))
         (input
           (concatenate 'string
                        (format nil "~C[A" escape)
                        paste-start
                        "paste"
                        (format nil "~C[3J" escape)
                        paste-end
                        (string escape)
                        (string #\Return)
                        (format nil "~C[13;2u" escape)
                        (format nil "~C[13;5u" escape)
                        (format nil "~C[27;2;13~~" escape)
                        (format nil "~C[27;5;13~~" escape)
                        (format nil "~C[27u" escape)
                        (format nil "~C[13u" escape)
                        (format nil "~C[9u" escape)
                        (format nil "~C[127u" escape)
                        (string (code-char 8))
                        (format nil "~C[127;5u" escape)
                        (format nil "~C[1;5D" escape)
                        (format nil "~C[1;5C" escape)
                        (format nil "~C[99;5u" escape)
                        (format nil "~C[27;5;99~~" escape)))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream (make-string-input-stream input)
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :interactive-p t
                          :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :up)
                 "the production decoder recognizes an up arrow")
    (let ((paste-event (terminal-read-event terminal)))
      (test-assert (eq (first paste-event) :paste)
                   "the production decoder recognizes bracketed paste")
      (let ((editor (line-editor-create)))
        (line-editor-handle-event editor paste-event)
        (test-assert
         (not (terminal-tests--contains-control-character-p
               (line-editor-text editor)))
         "bracketed paste terminal controls are neutralized before display")))
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "legacy Alt-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "enhanced Shift-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "enhanced Ctrl-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "xterm Shift-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "xterm Ctrl-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :escape)
                 "enhanced plain Escape remains an escape event")
    (test-assert (eq (terminal-read-event terminal) :submit)
                 "enhanced plain Enter remains a submit event")
    (test-assert (eq (terminal-read-event terminal) :complete)
                 "enhanced plain Tab remains a completion event")
    (test-assert (eq (terminal-read-event terminal) :backspace)
                 "enhanced plain Backspace remains a deletion event")
    (test-assert (eq (terminal-read-event terminal) :kill-word)
                 "raw Ctrl-Backspace requests word deletion")
    (test-assert (eq (terminal-read-event terminal) :kill-word)
                 "enhanced Ctrl-Backspace requests word deletion")
    (test-assert (eq (terminal-read-event terminal) :word-left)
                 "Ctrl-Left requests backward word movement")
    (test-assert (eq (terminal-read-event terminal) :word-right)
                 "Ctrl-Right requests forward word movement")
    (test-assert (eq (terminal-read-event terminal) :interrupt)
                 "Kitty keyboard protocol Ctrl-C requests interruption")
    (test-assert (eq (terminal-read-event terminal) :interrupt)
                 "xterm modifyOtherKeys Ctrl-C requests interruption"))
  (dolist (case
           (list
            (list (string *terminal-escape-character*) ':escape)
            (list (string (code-char 3)) ':interrupt)
            (list (string #\Tab) ':complete)
            (list (string (code-char 127)) ':backspace)
            (list (format nil "~C[B" *terminal-escape-character*) ':down)
            (list (format nil "~C[C" *terminal-escape-character*) ':right)
            (list (format nil "~C[D" *terminal-escape-character*) ':left)))
    (destructuring-bind (input expected) case
      (let ((terminal
              (make-instance 'stream-terminal
                             :input-stream (make-string-input-stream input)
                             :output-stream (make-string-output-stream)
                             :input-file-descriptor 0
                             :interactive-p t
                             :columns 40)))
        (test-assert (eq (terminal-read-event terminal) expected)
                     "ordinary raw and CSI keys retain semantic input events"))))
  (let* ((output (make-string-output-stream))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream output
                          :input-file-descriptor 0))
         (expected
           (concatenate
            'string
            (terminal-keyboard-enhancement-enable-sequence)
            (terminal-bracketed-paste-enable-sequence)
            (terminal-bracketed-paste-disable-sequence)
            (terminal-keyboard-enhancement-disable-sequence))))
    (terminal--enable-input-protocols terminal)
    (terminal--disable-input-protocols terminal)
    (test-assert (string= (get-output-stream-string output) expected)
                 "terminal input protocols enable and disable once in order"))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 2))
         (failure nil))
    (handler-case
        (terminal--enable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (let ((controls
            (format nil "~{~A~}"
                    (reverse
                     (protocol-recording-stream-terminal-chunks terminal)))))
      (test-assert failure
                   "partial protocol activation preserves its original failure")
      (test-assert
       (and (search (terminal-bracketed-paste-disable-sequence) controls)
            (search (terminal-keyboard-enhancement-disable-sequence) controls)
            (= (protocol-recording-stream-terminal-flush-count terminal) 1))
       "partial protocol activation attempts every cleanup control")))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 1))
         (failure nil))
    (handler-case
        (terminal--disable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (test-assert failure
                 "protocol shutdown reports its first cleanup failure")
    (test-assert
     (and (equal (protocol-recording-stream-terminal-chunks terminal)
                 (list (terminal-keyboard-enhancement-disable-sequence)))
          (= (protocol-recording-stream-terminal-flush-count terminal) 1))
     "protocol shutdown continues after one cleanup write fails"))
  nil)

(-> test-terminal-live-region-layout () null)
(defun test-terminal-live-region-layout ()
  "Test placeholder hints, styled span emission, and finalized entry separation."
  (let* ((terminal (make-instance 'recording-terminal
                                  :columns 40
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal
                                 :prompt "❯ "
                                 :placeholder "hint text")))
    (with-terminal-ui (active-ui ui)
      (test-assert (search "hint text" (recording-terminal-output terminal))
                   "an empty prompt row shows the placeholder hint")
      (recording-terminal-reset terminal)
      (terminal-ui-process-event active-ui '(:insert "a"))
      (let ((typing (recording-terminal-output terminal)))
        (test-assert (not (search "hint text" typing))
                     "typed input replaces the placeholder hint")
        (test-assert (search "a" typing)
                     "typed input is painted on the prompt row"))
      (recording-terminal-reset terminal)
      (terminal-ui-set-status active-ui "working")
      (test-assert (search "∙ " (recording-terminal-output terminal))
                   "the status row carries its activity separator")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (test-assert (char= (char text 0) #\Newline)
                     "an empty row appears above the status row")
        (test-assert (search "READ  ∙ 00:00" text)
                     "the status row starts with its fixed-width REPL spinner")
        (test-assert
         (equal (terminal-ui--status-spinner-spans-at
                 active-ui
                 (terminal-ui-status-started-at active-ui))
                (list (terminal-span ':status-plain "R")
                      (terminal-span ':status-dim "EAD ")))
         "the spinner dims every character except its cycling highlight"))
      (recording-terminal-reset terminal)
      (terminal-ui-append-finalized
       active-ui
       :styled
       (list (terminal-span :brand "autolith")
             (terminal-span :plain " ready")))
      (let ((finalized (recording-terminal-output terminal)))
        (test-assert
         (search (format nil "~C[1;35mautolith" *terminal-escape-character*)
                 finalized)
         "styled finalized spans emit basic rendition controls")
        (test-assert
         (search (format nil " ready~C~C~C~C" #\Newline #\Return
                         #\Newline #\Return)
                 finalized)
         "finalized entries end with one separating blank row")
        (test-assert (not (terminal-tests--forbidden-control-p finalized))
                     "styled transcript output never erases the display"))))
  (let* ((plain-terminal (make-instance 'recording-terminal :columns 40))
         (plain-ui (terminal-ui-create :terminal plain-terminal)))
    (with-terminal-ui (active-ui plain-ui)
      (terminal-ui-append-finalized active-ui
                                    :styled
                                    (list (terminal-span :brand "autolith"))))
    (test-assert (not (search "[1;35m" (recording-terminal-output plain-terminal)))
                 "styling is omitted when the terminal does not permit it"))
  nil)

(-> test-terminal-status-bar () null)
(defun test-terminal-status-bar ()
  "Test status metadata, indexed background, padding, and plain fallback."
  (let* ((columns 96)
         (details
           (list (terminal-span ':status-dim "  ")
                 (terminal-span ':status-model "gpt-5.6-sol")
                 (terminal-span ':status-dim " · ")
                 (terminal-span ':status-effort "ultra")
                 (terminal-span ':status-dim " · git ")
                 (terminal-span ':status-branch "chromatic")))
         (terminal (make-instance 'recording-terminal
                                  :columns columns
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal)))
    (let ((cl-colorist:*color-level* ':indexed))
      (with-terminal-ui (active-ui ui)
        (recording-terminal-reset terminal)
        (terminal-ui-set-status active-ui "working" :details details)
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "one status change is one buffered live-region frame")
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content
             active-ui
             (terminal-ui-status-started-at active-ui))
          (declare (ignore cursor))
          (let ((status-row
                  (second (uiop:split-string text :separator '(#\Newline)))))
            (test-assert (= (text-cell-width status-row) columns)
                         "a styled status background spans the terminal width")
            (test-assert (search "gpt-5.6-sol · ultra · git chromatic"
                                 status-row)
                         "status metadata compactly shows model, effort, and branch"))
          (test-assert
           (search (terminal-style-sequence ':status-model t) display)
           "the styled status row uses its indexed neutral background")
          (test-assert
           (search (terminal-style-sequence ':status-dim t) display)
           "neutral status text uses its readable indexed style")))))
  (let* ((columns 96)
         (terminal (make-instance 'recording-terminal :columns columns))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status
       active-ui
       "working"
       :details (list (terminal-span ':status-model "model")))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (let ((status-row
                (second (uiop:split-string text :separator '(#\Newline)))))
          (test-assert (< (text-cell-width status-row) columns)
                       "an unstyled status row omits invisible trailing padding")))))
  (dolist (columns '(1 2 39 40 41))
    (let* ((terminal (make-instance 'recording-terminal
                                    :columns columns
                                    :styled-p t))
           (ui (terminal-ui-create :terminal terminal)))
      (with-terminal-ui (active-ui ui)
        (terminal-ui-set-status active-ui "working")
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content
             active-ui
             (terminal-ui-status-started-at active-ui))
          (declare (ignore display))
          (test-assert
           (= (text-cell-width
               (second (uiop:split-string text :separator '(#\Newline))))
              columns)
           (format nil "status rows fit a ~D-column terminal" columns))
          (multiple-value-bind (cursor-row cursor-column pending-wrap)
              (screen-position text :columns columns :end cursor)
            (declare (ignore pending-wrap))
            (test-assert
             (and (= (terminal-ui-live-cursor-row active-ui) cursor-row)
                  (= (live-region-cursor-column
                      (terminal-ui-live-region active-ui))
                     cursor-column))
             (format nil
                     "status geometry tracks the prompt at ~D columns"
                     columns)))))))
  nil)

(-> test-terminal-narrow-live-region () null)
(defun test-terminal-narrow-live-region ()
  "Test typed prompt layout and repaint bookkeeping at minimal terminal widths."
  (dolist (columns '(1 2))
    (let* ((terminal (make-instance 'recording-terminal :columns columns))
           (ui (terminal-ui-create :terminal terminal :prompt "❯ ")))
      (with-terminal-ui (active-ui ui)
        (terminal-ui-process-event active-ui '(:insert "a"))
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content active-ui)
          (declare (ignore display))
          (multiple-value-bind (cursor-row cursor-column cursor-wrap)
              (screen-position text :columns columns :end cursor)
            (declare (ignore cursor-wrap))
            (multiple-value-bind (end-row end-column end-wrap)
                (screen-position text :columns columns)
              (declare (ignore end-column end-wrap))
              (test-assert
               (< cursor-column columns)
               (format nil "the live cursor fits a ~D-column terminal" columns))
              (test-assert
               (= (terminal-ui-live-row-count active-ui) (1+ end-row))
               (format nil
                       "repaint bookkeeping counts rows at ~D columns"
                       columns))
              (test-assert
               (= (terminal-ui-live-cursor-row active-ui) cursor-row)
               (format nil
                       "repaint bookkeeping tracks the cursor at ~D columns"
                       columns))))))))
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

(-> test-terminal-preview-rows () null)
(defun test-terminal-preview-rows ()
  "Test transient multi-row previews, ordering, clipping, and clearing."
  (let* ((terminal (make-instance 'recording-terminal :columns 20 :rows 8))
         (ui (terminal-ui-create :terminal terminal))
         (preview
           (list (list (terminal-span ':hint "◇ reasoning summary"))
                 (list (terminal-span ':dim "  │ ")
                       (terminal-span ':strong "<thought>")
                       (terminal-span ':plain " checking a long path")))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "untangling")
      (terminal-ui-set-preview-rows active-ui preview)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (let ((preview-position (search "◇ reasoning summary" text))
              (status-position (search "READ  ∙ " text)))
          (test-assert (and preview-position
                            status-position
                            (< preview-position status-position))
                       "preview rows appear above the independent status row"))
        (test-assert (every (lambda (line)
                             (<= (text-cell-width line) 20))
                           (uiop:split-string text :separator '(#\Newline)))
                     "preview rows are clipped to the terminal width"))
      (test-assert
       (find (terminal-span ':strong "<thought>")
             (apply #'append (terminal-ui-preview-rows active-ui))
             :test #'equal)
       "preview rows preserve semantic inline styles")
      (recording-terminal-reset terminal)
      (terminal-ui-set-preview-rows active-ui preview)
      (test-assert (string= (recording-terminal-output terminal) "")
                   "an unchanged preview does not repaint the live region")
      (terminal-ui-set-preview-rows active-ui nil)
      (test-assert (null (terminal-ui-preview-rows active-ui))
                   "clearing the preview removes its stored rows")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "reasoning summary" text))
                     "a cleared preview disappears without entering scrollback"))))
  nil)

(-> test-terminal-live-word-wrapping () null)
(defun test-terminal-live-word-wrapping ()
  "Test styled previews and stream tails wrap at word boundaries."
  (let* ((terminal (make-instance 'recording-terminal
                                  :columns 10
                                  :rows 8
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal))
         (spans (list (terminal-span ':user "alpha ")
                      (terminal-span ':strong "beta ")
                      (terminal-span ':emphasis "gamma")))
         (expected (format nil "alpha beta~%gamma")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-preview-rows active-ui (list spans))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore cursor))
        (test-assert (search expected text)
                     "styled previews wrap only between words")
        (test-assert (string= text (cl-colorist:strip-ansi display))
                     "wrapped previews preserve ANSI-visible equivalence")
        (test-assert (search (terminal-style-sequence ':emphasis) display)
                     "wrapped previews retain continued span styling"))
      (terminal-ui-set-preview-rows active-ui nil)
      (terminal-ui-stream-update active-ui :tail spans)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore cursor))
        (test-assert (search expected text)
                     "styled stream tails wrap only between words")
        (test-assert (string= text (cl-colorist:strip-ansi display))
                     "wrapped stream tails preserve ANSI-visible equivalence"))))
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
        (test-assert (not (search "working" text))
                     "the modeline omits the internal activity label"))
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
        (test-assert (not (search "working" text))
                     "stale modelines also omit the internal activity label"))
      (terminal-ui-note-status-progress active-ui)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "new progress immediately clears the stale status state")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "no update" text))
                     "new progress removes the stale warning"))))
  nil)

(-> test-terminal-agent-activities () null)
(defun test-terminal-agent-activities ()
  "Test bounded, ordered, colored, and independently animated child rows."
  (let* ((clock 0)
         (terminal (make-instance 'recording-terminal
                                  :columns 72
                                  :styled-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (activities
           (list
            (list :id "review-agent-2"
                  :index 2
                  :agent "reviewer"
                  :state ':queued
                  :current-tool nil
                  :assignment "Review the finished patch."
                  :detached nil)
            (list :id "search-1"
                  :index 1
                  :agent "explorer"
                  :state ':running
                  :current-tool "search.content"
                  :assignment "Locate the scheduler."
                  :detached t))))
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
               (status-index
                 (position-if (lambda (line)
                                (search "READ " line))
                              lines))
               (search-line (and search-index (nth search-index lines)))
               (review-line (and review-index (nth review-index lines)))
               (search-role-position
                 (and search-line (search "explorer" search-line)))
               (review-role-position
                 (and review-line (search "reviewer" review-line)))
                 (search-detail-position
                   (and search-line (search " · search.content" search-line)))
                 (review-detail-position
                   (and review-line (search " · queued" review-line))))
          (test-assert
           (and header-index
                search-index
                review-index
                status-index
                (= search-index (+ header-index 2))
                (= review-index (1+ search-index))
                (= status-index (+ review-index 2))
                (string= (nth (1+ header-index) lines) "")
                (string= (nth (1+ review-index) lines) "")
                (uiop:string-prefix-p "  " search-line)
                (uiop:string-prefix-p "  " review-line)
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
                  (search "explorer · search.content" text)
                (search "reviewer · queued" text))
           "child rows are separated, indented, aligned, and above the modeline")
          (test-assert (not (search "async" text))
                       "detached state does not add a redundant row label"))
        (test-assert
         (every (lambda (row)
                  (<= (terminal--spans-width row) 12))
                (terminal-ui--agent-activity-rows-at
                 active-ui clock 12))
         "aligned child columns remain bounded on narrow terminals")
        (test-assert
         (and (search (terminal-style-sequence ':agent-spinner) display)
              (search (terminal-style-sequence ':agent-name) display)
              (search (terminal-style-sequence ':agent-role) display)
              (search (terminal-style-sequence ':agent-tool) display))
         "running child rows use distinct basic-palette semantic colors"))
      (setf clock 0.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "child spinners do not repaint within one animation frame")
      (setf clock 0.25)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "running child spinners advance on the shared cadence")
      (terminal-ui-set-status active-ui nil)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (review-index
                 (position-if (lambda (line)
                                (search "review-agent-2" line))
                              lines)))
          (test-assert
           (and (search "search-1" text)
                (not (search "READ " text))
                review-index
                (string= (nth (1+ review-index) lines) "")
                (non-empty-string-p (nth (+ review-index 2) lines)))
           "detached children keep one blank row before the idle prompt")))
      (setf clock 0.5)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "detached child animation continues without a modeline")
      (let ((many-activities
              (loop for index from 1 to 10
                    collect
                    (list :id (format nil "worker-~D" index)
                          :index index
                          :agent "worker"
                          :state ':queued
                          :current-tool nil
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
           "the child strip caps rows and summarizes overflow")))
      (terminal-ui-set-agent-activities active-ui nil)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "clearing the final child repaints the live region")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "agents " text))
                     "terminal child state disappears when no jobs remain"))))
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
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "tab cycles to and previews the next command")
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "tab keeps command completion selection active")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/resume ")
                     "repeated tab cycles through command completions")
        (terminal-ui-process-event active-ui :complete-previous)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "shift-tab cycles backward through command completions")
        (terminal-ui-process-event active-ui :complete)
        (terminal-ui-process-event active-ui '(:insert "draft"))
        (test-assert (string= (line-editor-text editor) "/resume draft")
                     "ordinary input retains the selected completion")
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "ordinary input dismisses completion selection")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (terminal-ui-process-event active-ui :history-next)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "arrow keys move the completion selection")
        (terminal-ui-process-event active-ui :escape)
        (test-assert (string= (line-editor-text editor) "/r")
                     "escape restores the prefix from before completion")
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
                       "empty active-turn tab requests queued follow-up editing")))))
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

(-> test-terminal-modal-selection () null)
(defun test-terminal-modal-selection ()
  "Test modal picker navigation, acceptance, cancellation, and cleanup."
  (let ((selector (make-selector :visible-count 4 :arrangement ':vertical)))
    (selector-set-items
     selector
     '((:name "a" :argument nil :description "first")
       (:name "considerably-longer" :argument nil :description "second")))
    (let* ((rows (terminal-ui--choice-rows selector 50))
           (texts (mapcar #'markdown-tests--row-text rows)))
      (test-assert (= (search "first" (first texts))
                      (search "second" (second texts)))
                   "picker descriptions share one content-aware value column")))
  (let ((selector (make-selector :visible-count 4 :arrangement ':vertical)))
    (selector-set-items
     selector
     (list
      (list :name "alpha"
            :argument nil
            :tally "2 turns"
            :description "2026-07-26 14:30 · first"
            :description-spans
            (list (terminal-span ':plain "2026-07-26 ")
                  (terminal-span ':timestamp-time "14:30")
                  (terminal-span ':plain " · first")))
      (list :name "beta"
            :argument nil
            :tally "1:02"
            :description "2026-07-25 09:15 · second"
            :description-spans
            (list (terminal-span ':plain "2026-07-25 ")
                  (terminal-span ':timestamp-time "09:15")
                  (terminal-span ':plain " · second")))))
    (let* ((rows (terminal-ui--choice-rows selector 70))
           (texts (mapcar #'markdown-tests--row-text rows))
           (first-time
             (find "14:30" (first rows)
                   :key #'terminal-span-text
                   :test #'string=))
           (second-date
             (find "2026-07-25 " (second rows)
                   :key #'terminal-span-text
                   :test #'string=)))
      (test-assert (= (search "2026-07-26" (first texts))
                      (search "2026-07-25" (second texts)))
                   "picker tallies preserve one aligned description column")
      (test-assert (and first-time
                        (eq (terminal-span-style first-time) ':timestamp-time))
                   "picker descriptions preserve the timestamp time color")
      (test-assert (and second-date
                        (eq (terminal-span-style second-date) ':dim))
                   "unselected picker dates retain the ordinary dim style")))
  (test-assert
   (not (terminal-completion-p
         (list :name "broken"
               :argument nil
               :description "visible"
               :description-spans (list (terminal-span ':plain "different")))))
   "completion descriptions reject mismatched styled text")
  (let* ((items '((:name "alpha" :argument nil :description "first entry"
                   :group "current directory")
                  (:name "beta" :argument nil :description "second entry"
                   :group "current directory")
                  (:name "gamma" :argument nil :description "third entry"
                   :group "other sessions")))
         (terminal (make-instance 'scripted-terminal
                                  :columns 60
                                  :events (list :history-next :submit)))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "arrow keys move the modal selection before enter")
      (let ((painted (recording-terminal-output terminal)))
        (test-assert (search "pick one" painted)
                     "the picker paints its title")
        (test-assert (search "alpha" painted)
                     "the picker paints its items")
        (test-assert (and (search "current directory" painted)
                          (search "other sessions" painted))
                     "the picker paints nonselectable candidate groups")
        (test-assert (not (terminal-tests--forbidden-control-p painted))
                     "the picker never erases the display"))
      (test-assert (null (terminal-ui-selector active-ui))
                   "the selector state clears after selection")
      (setf (scripted-terminal-events terminal) (list :escape))
      (test-assert (null (terminal-ui-select active-ui
                                             :title "pick one"
                                             :items items))
                   "escape cancels the picker")
      (setf (scripted-terminal-events terminal) (list :complete :submit))
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "tab cycles modal picker options")
      (setf (scripted-terminal-events terminal)
            (list :history-next '(:insert "x")))
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "ordinary picker input retains the selected option")
      (recording-terminal-reset terminal)
      (setf (scripted-terminal-events terminal)
            (list '(:insert "d") :submit))
      (let ((mode ':browse))
        (test-assert
         (string=
          (terminal-ui-select
           active-ui
           :title "browse"
           :items items
           :hint "d deletes"
           :on-event
           (lambda (event selector)
             (declare (ignore selector))
             (ecase mode
               (:browse
                (when (equal event '(:insert "d"))
                  (setf mode ':confirm)
                  (list ':replace
                        "confirm"
                        '((:name "delete"
                           :argument nil
                           :description "confirm deletion"))
                        "enter confirms")))
               (:confirm
                (when (eq event ':submit)
                  (list ':accept "deleted"))))))
          "deleted")
         "picker event hooks can replace and accept modal choices")
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (and (search "d deletes" painted)
                            (search "enter confirms" painted))
                       "picker replacements repaint their contextual hints"))
        (test-assert (null (terminal-ui-selector-hint active-ui))
                     "picker cleanup clears its contextual hint"))
      (setf (scripted-terminal-events terminal)
            (list '(:insert "c") :submit))
      (let ((mode ':browse)
            (replacement-hint :unobserved))
        (test-assert
         (string=
          (terminal-ui-select
           active-ui
           :title "browse"
           :items items
           :hint "temporary hint"
           :on-event
           (lambda (event selector)
             (declare (ignore selector))
             (ecase mode
               (:browse
                (when (equal event '(:insert "c"))
                  (setf mode ':replaced)
                  (list ':replace "cleared" items nil)))
               (:replaced
                (when (eq event ':submit)
                  (setf replacement-hint
                        (terminal-ui-selector-hint active-ui))
                  (list ':accept "cleared"))))))
          "cleared")
         "picker replacements accept an explicit NIL hint")
        (test-assert (null replacement-hint)
                     "an explicit NIL replacement clears the previous hint"))))
  (let* ((terminal (make-instance 'scripted-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal)))
    (test-assert (null (terminal-ui-select
                        ui
                        :title "pick"
                        :items '((:name "a" :argument nil :description "d"))))
                 "non-interactive terminals never open the picker"))
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

(-> test-terminal-modal-resize () null)
(defun test-terminal-modal-resize ()
  "Test that a resize raised during modal input repaints before event dispatch."
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
              (sb-posix:setenv "COLUMNS" "18" 1)
              (sb-posix:setenv "LINES" "8" 1)
              (setf *terminal-resize-pending-p* t))))
         (ui (terminal-ui-create :terminal terminal))
         (items '((:name "alpha" :argument nil :description "first entry"))))
    (unwind-protect
         (terminal-tests--call-without-host-size
          (lambda ()
            (with-terminal-ui (active-ui ui)
              (recording-terminal-reset terminal)
              (test-assert
               (string= (terminal-ui-select
                         active-ui
                         :title "pick"
                         :items items
                         :resize-callback
                         #'application-pending-terminal-size)
                        "alpha")
               "submit still accepts the picker event received during resize")
              (test-assert (= (terminal-columns terminal) 18)
                           "a pending picker resize refreshes terminal columns")
              (test-assert (= (terminal-rows terminal) 8)
                           "a pending picker resize refreshes terminal rows")
              (test-assert (null *terminal-resize-pending-p*)
                           "the picker consumes the pending resize flag")
              (test-assert
               (= (terminal-tests--substring-count
                   "pick"
                   (recording-terminal-output terminal))
                  2)
               "the picker repaints at the new width before submit exits"))))
      (if previous-columns
          (sb-posix:setenv "COLUMNS" previous-columns 1)
          (sb-posix:unsetenv "COLUMNS"))
      (if previous-lines
          (sb-posix:setenv "LINES" previous-lines 1)
          (sb-posix:unsetenv "LINES"))))
  nil)

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

(-> test-terminal-styling-primitives () null)
(defun test-terminal-styling-primitives ()
  "Test semantic style resolution, span safety, clipping, and word wrapping."
  (test-assert
   (let ((sequence (terminal-style-sequence :dim)))
     (and (stringp sequence)
          (char= (char sequence 0) *terminal-escape-character*)
          (char= (char sequence (1- (length sequence))) #\m)))
   "semantic styles resolve to rendition controls")
  (test-assert (null (terminal-style-sequence :plain))
               "the plain style resolves to no control sequence")
  (test-assert
   (and (string= (terminal-style-sequence :syntax-keyword)
                 (format nil "~C[35m" *terminal-escape-character*))
        (string= (terminal-style-sequence :syntax-string)
                 (format nil "~C[32m" *terminal-escape-character*))
        (string= (terminal-style-sequence :syntax-function)
                 (format nil "~C[34m" *terminal-escape-character*)))
   "syntax styles use the terminal's base ANSI palette")
  (test-assert
   (string= (terminal-style-sequence :timestamp-time)
            (format nil "~C[36m" *terminal-escape-character*))
   "timestamp times use a distinct cyan terminal color")
  (let ((indexed-sequences
          (loop for style in '(:brand-gradient-1 :brand-gradient-2
                               :brand-gradient-3 :brand-gradient-4
                               :brand-gradient-5 :brand-gradient-6)
                collect (terminal-style-sequence style t))))
    (test-assert
     (= (length (remove-duplicates indexed-sequences :test #'string=)) 6)
     "every brand-gradient row has a distinct indexed color")
    (test-assert
     (string= (first indexed-sequences)
              (format nil "~C[1;38;5;193m" *terminal-escape-character*))
     "the brand gradient begins with its lightest green"))
  (test-assert
   (string= (terminal-style-sequence :brand-gradient-1 nil)
            (format nil "~C[1;32m" *terminal-escape-character*))
   "the brand gradient falls back to solid bold green")
  (let ((indexed-sequences
          (loop for style in '(:recovery-gradient-1 :recovery-gradient-2
                               :recovery-gradient-3 :recovery-gradient-4
                               :recovery-gradient-5 :recovery-gradient-6)
                collect (terminal-style-sequence style t))))
    (test-assert
     (= (length (remove-duplicates indexed-sequences :test #'string=)) 6)
     "every recovery-gradient row has a distinct indexed color")
    (test-assert
     (string= (first indexed-sequences)
              (format nil "~C[1;38;5;224m" *terminal-escape-character*))
     "the recovery gradient begins with its lightest red"))
  (test-assert
   (string= (terminal-style-sequence :recovery-gradient-1 nil)
            (format nil "~C[1;31m" *terminal-escape-character*))
   "the recovery gradient falls back to solid bold red")
  (test-assert
   (and (string= (terminal-style-sequence :status-model t)
                 (format nil "~C[1;96;48;5;236m"
                         *terminal-escape-character*))
        (string= (terminal-style-sequence :status-model nil)
                 (format nil "~C[1;96;40m" *terminal-escape-character*)))
   "status text keeps a base color over indexed and basic neutral backgrounds")
  (test-assert
   (and (string= (terminal-style-sequence :status-dim t)
                 (format nil "~C[37;48;5;236m"
                         *terminal-escape-character*))
        (string= (terminal-style-sequence :status-dim nil)
                 (format nil "~C[37;40m" *terminal-escape-character*)))
   "neutral status text stays readable without terminal-dependent faint color")
  (test-assert
   (let ((cl-colorist:*color-level* ':indexed))
     (and (terminal-environment-indexed-color-p)
          (terminal-environment-styling-p)))
   "terminal capability detection accepts an indexed Colorist environment")
  (test-assert
   (let ((cl-colorist:*color-level* ':none))
     (not (terminal-environment-styling-p)))
   "terminal capability detection honors disabled Colorist presentation")
  (test-assert
   (terminal-styled-text-p (list (terminal-span :brand "autolith")
                                 (terminal-span :plain " ready")))
   "lists of spans form styled text")
  (test-assert (not (terminal-styled-text-p (list "bare string")))
               "bare strings are not styled text")
  (test-assert (not (terminal-styled-text-p (terminal-span :dim "x")))
               "a dotted span alone is not styled text")
  (let ((clipped (terminal--clip-spans
                  (list (terminal-span :user "abc")
                        (terminal-span :dim "defg"))
                  5)))
    (test-assert
     (equal clipped (list (terminal-span :user "abc")
                          (terminal-span :dim "de")))
     "span clipping preserves styles across the width boundary"))
  (test-assert
   (= (terminal--spans-width (list (terminal-span :user "abc")
                                   (terminal-span :dim "de")))
      5)
   "span width sums sanitized cell widths")
  (let ((hostile (terminal--clip-spans
                  (list (terminal-span :plain
                                       (format nil "a~C[31mb~%c"
                                               *terminal-escape-character*)))
                  40)))
    (test-assert
     (not (terminal-tests--contains-control-character-p
           (terminal-span-text (first hostile))))
     "span clipping neutralizes untrusted controls and newlines"))
  (let ((cases '(("" 10 (""))
                 ("hello" 10 ("hello"))
                 ("aa bb" 2 ("aa" "bb"))
                 ("ab cd" 4 ("ab" "cd"))
                 ("abcdef" 3 ("abc" "def"))
                 ("one two three" 7 ("one two" "three"))
                 ("日本語" 4 ("日本" "語")))))
    (loop for (text width expected) in cases
          do (test-assert
              (equal (wrap-text text width) expected)
              (format nil "wrapping ~S at ~D produces ~S" text width expected))))
  (test-assert
   (equal (wrap-text (format nil "alpha~%beta gamma") 5)
          '("alpha" "beta" "gamma"))
   "wrapping preserves explicit line breaks before width breaks")
  nil)

(-> test-terminal-non-tty-fallback () null)
(defun test-terminal-non-tty-fallback ()
  "Test line-oriented fallback input and output on a non-TTY descriptor."
  (let ((terminal
          (stream-terminal-create
           :input-stream (make-string-input-stream "")
           :output-stream (make-string-output-stream)
           :input-file-descriptor -1)))
    (terminal-start terminal)
    (test-assert (terminal-started-p terminal)
                 "a noninteractive stream starts without inspecting its descriptor")
    (test-assert (not (terminal-interactive-p terminal))
                 "a noninteractive stream selects fallback mode")
    (terminal-stop terminal))
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (unwind-protect
         (let* ((output (make-string-output-stream))
                (terminal
                  (stream-terminal-create
                   :input-stream (make-string-input-stream
                                  (format nil "fallback input~%"))
                   :output-stream output
                   :input-file-descriptor read-descriptor
                   :columns 20))
                (ui (terminal-ui-create :terminal terminal)))
           (terminal-ui-start ui)
           (test-assert (not (terminal-interactive-p terminal))
                        "a pipe selects the non-TTY fallback")
           (multiple-value-bind (action submitted)
               (terminal-ui-process-event ui (terminal-ui-read-event ui))
             (test-assert (eq action :submit)
                          "fallback line input produces a submit action")
             (test-assert (string= submitted "fallback input")
                          "fallback line input preserves its text"))
           (terminal-ui-set-status ui "not printed")
           (terminal-ui-append-finalized ui 1 "fallback output")
           (terminal-ui-stop ui)
           (let ((captured (get-output-stream-string output)))
             (test-assert (search "fallback output" captured)
                          "fallback mode writes finalized transcript output")
             (test-assert (not (find *terminal-escape-character* captured))
                          "fallback mode emits no terminal controls")))
      (sb-posix:close read-descriptor)
      (sb-posix:close write-descriptor)))
  nil)

(-> run-terminal-tests () boolean)
(defun run-terminal-tests ()
  "Run focused terminal seam tests and return true when every assertion succeeds."
  (test-terminal-primary-screen-controls)
  (test-terminal-finalized-batch)
  (test-terminal-untrusted-text)
  (test-terminal-finalized-scrollback)
  (test-terminal-resize-frame)
  (test-terminal-line-editor)
  (test-terminal-image-attachments)
  (test-terminal-input-decoding)
  (test-terminal-live-region-layout)
  (test-terminal-status-bar)
  (test-terminal-status-worked-time)
  (test-terminal-narrow-live-region)
  (test-terminal-bounded-editor-repaint)
  (test-terminal-preview-rows)
    (test-terminal-live-word-wrapping)
  (test-terminal-transient-notice)
  (test-terminal-notice-lock-contention)
  (test-terminal-timed-status)
  (test-terminal-agent-activities)
  (test-terminal-stream-update)
  (test-terminal-command-completion)
  (test-terminal-modal-selection)
  (test-terminal-modal-resize)
  (test-terminal-application-read-resize)
  (test-terminal-styling-primitives)
  (test-terminal-non-tty-fallback)
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
