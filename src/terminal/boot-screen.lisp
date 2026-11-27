(in-package #:autolith)

;;;; -- Lisp-Machine Startup and Login --

(defparameter *terminal-ui-boot-mascot-rows*
  '("⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⣤⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⢀⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣴⣿⣿⣿⣯⣽⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⢀⠠⠴⣐⣚⣻⣟⣒⣂⣶⣤⡀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣻⣿⡟⣼⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⢀⣤⡦⠝⠚⠉⠙⡙⣯⠀⠀⠀⠀⠉⠉⠀⢀⣿⣿⣛⣛⣿⣿⣿⣿⣿⠿⣿⣿⣿⣿⣿⣿⣿⡿⣝⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠋⠁⠀⠀⠀⠀⠀⢼⠆⠀⠀⠀⠀⠀⠀⠀⣸⣿⣿⠋⢻⣿⣷⠇⣿⣿⠛⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⢸⡌⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣶⣿⣿⡤⢴⢿⣿⣷⣾⣿⣿⣿⣻⡿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⢀⢸⠡⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣿⣿⢵⣽⢛⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⢸⣶⡍⣆⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣿⣿⣿⣾⣼⣾⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⢩⡄⢻⣦⣀⠀⠀⢀⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣿⡿⣗⣿⡿⣽⣿⡿⢃⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠠⢖⡀⠙⠻⠿⠿⠋⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣿⢟⢸⣿⣬⣬⣾⣿⡄⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠘⠃⠀⠀⠀⠀⢰⣿⣿⣿⡽⣿⣿⣿⣿⣿⣿⣿⣿⠟⢿⠛⠿⣠⣿⣾⣾⣿⢸⣿⣿⡟⠁⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣷⣿⣿⣿⣿⣿⠟⣵⣷⣿⣿⣿⣿⣿⣿⣿⡏⣿⢸⣿⡟⠁⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⣿⡟⢹⣿⠝⣫⣵⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⣿⡽⠟⠀⠀⠀⢀⡄⠀⠀⢀⢤⡆"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠁⠺⠿⢿⠑⠊⠻⠋⠉⠛⠿⣿⠛⠛⠉⢅⢿⠃⠀⠀⠀⢐⡘⣻⠀⠟⣵⣆⠆"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣄⠀⠀⠀⠀⠀⠀⠀⢠⣿⡆⠀⠀⠀⠀⠀⢰⣷⡆⠀⠀⠀⢀⣀⣤⣶⣽⣥⣵⢾⠿⣛⣾⣽⣗⠅"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣼⣙⠟⣦⠀⠀⣀⠀⠀⠀⢸⣿⠀⠀⣀⣠⢤⢤⢼⣿⡇⡿⣻⣯⡿⣵⠛⣽⠯⡺⢙⡙⡫⠿⣿⠻⡥⠆"
    "⠀⠀⠀⠀⠀⠀⠀⡀⣰⣾⡝⠻⣤⢯⠻⣾⢿⣟⣶⡚⠘⠿⢈⢯⣟⣫⡯⣿⠸⣿⡇⢆⠿⡿⢟⡒⣜⣯⡇⠴⢻⣷⠉⢇⢻⡟⠰⠃"
    "⠀⠀⢀⢀⣴⣦⣆⡙⡛⢛⣰⣖⣟⣠⣾⡵⢻⡧⣳⣵⣹⣿⣿⣵⣾⣿⣿⢗⠀⣷⣶⣯⣿⣶⣗⣿⢿⣦⢊⣰⣦⠻⣗⢶⡿⣹⠢⠂"
    "⠐⠚⡯⠟⢗⣚⣷⣿⣾⣽⡦⣝⣽⢶⣼⡻⣚⢳⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣮⣿⣿⣿⣿⣿⣿⢟⢷⡷⡘⢥⢿⣷⡅⡨⣿⠊⠁⠀"
    "⠀⠑⠂⠈⠽⣍⡟⢿⣻⣟⣫⢸⡢⠟⡳⡚⣓⣏⣹⣿⣍⣋⣽⣮⣟⢝⡎⢟⢿⣿⠿⣗⣲⣿⣿⡯⢕⣿⡼⣛⣿⡵⠯⠁⠁⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠈⠉⠀⠙⠰⠉⠷⠚⠒⠻⠟⢓⠠⡊⢗⠉⡍⠭⠿⠎⠝⢻⠑⠺⠕⠰⠶⠷⠶⠲⠁⠞⠒⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀")
  "A braille rendering of the Autolith rock mascot for the boot panel: the figure filled, its lines dark.")

(defparameter *terminal-ui-boot-almighty-rows*
  '(" █████  ██      ███    ███ ██  ██████  ██   ██ ████████ ██    ██"
    "██   ██ ██      ████  ████ ██ ██       ██   ██    ██     ██  ██ "
    "███████ ██      ██ ████ ██ ██ ██   ███ ███████    ██      ████  "
    "██   ██ ██      ██  ██  ██ ██ ██    ██ ██   ██    ██       ██   "
    "██   ██ ███████ ██      ██ ██  ██████  ██   ██    ██       ██   "
    ""
    "██      ██ ████████ ██   ██"
    "██      ██    ██    ██   ██"
    "██      ██    ██    ███████"
    "██      ██    ██    ██   ██"
    "███████ ██    ██    ██   ██")
  "The ALMIGHTY LITH mark in FIGlet's ANSI Regular font, for Micah's theme.")

(defparameter *terminal-ui-boot-almighty-panel-width* 72
  "The Almighty boot panel's width; its block mark needs more room than the rock.")

(defparameter *terminal-ui-boot-panel-width* 64
  "The default boot panel's width.")

(defparameter *terminal-ui-boot-mascot-styles*
  #(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
    :brand-gradient-4 :brand-gradient-5 :brand-gradient-6)
  "Row styles cycling top-to-bottom across the boot mark art.")

(defparameter *terminal-ui-boot-linger-p* nil
  "Whether the boot screen currently advertises Space to start the session.")

(defparameter *terminal-ui-boot-linger-prompt* "PRESS SPACE TO START"
  "The call to action shown below the tip while the boot screen waits.")

(defparameter *terminal-ui-boot-start-label* "start"
  "The Space action advertised in the boot panel footer.")

(-> terminal-ui--boot-panel-width (integer) integer)
(defun terminal-ui--boot-panel-width (columns)
  "Return the boot panel width for the installed theme within COLUMNS."
  (min (if (eq (terminal-theme-name *terminal-theme*) ':almighty)
           *terminal-ui-boot-almighty-panel-width*
           *terminal-ui-boot-panel-width*)
       (max 1 (- columns 4))))

(-> terminal-ui--boot-art () (values list string string))
(defun terminal-ui--boot-art ()
  "Return the installed theme's boot mark rows, panel title, and tagline."
  (ecase (terminal-theme-name *terminal-theme*)
    (:autolith
     (values *terminal-ui-boot-mascot-rows*
             "A U T O L I T H  /  LISP MACHINE"
             "READ . EVAL . PRINT . LOOP"))
    (:almighty
     (values *terminal-ui-boot-almighty-rows*
             "A L M I G H T Y  L I T H  /  LISP MACHINE"
             "ALMIGHTY TOOLS FOR ALMIGHTY PROGRAMMERS"))))

(-> terminal-ui--boot-mascot-row-style (integer integer) terminal-style)
(defun terminal-ui--boot-mascot-row-style (row total)
  "Return ROW's gradient style out of TOTAL rows of boot mark art."
  (let ((styles *terminal-ui-boot-mascot-styles*))
    (aref styles (min (1- (length styles))
                      (floor (* row (length styles)) (max 1 total))))))

(-> terminal-ui--boot-screen-panel
    ((or string symbol) (option string) integer &key (:mark-rows list))
    list)
(defun terminal-ui--boot-screen-panel (phase detail columns &key mark-rows)
  "Return horizontally centered styled rows for the actual PHASE and DETAIL.

MARK-ROWS are the already height-limited rows of the theme's boot mark. They
are centered as one block, so rows of unequal width stay aligned."
  (let* ((width        (terminal-ui--boot-panel-width columns))
         (inside       (max 0 (- width 4)))
         (mark-width   (loop for text in mark-rows maximize (text-cell-width text)))
         (mark-indent  (max 0 (floor (- inside mark-width) 2)))
         (left         (make-string (max 0 (floor (- columns width) 2))
                                    :initial-element #\Space))
         (top-border
           (concatenate 'string "┌" (make-string (max 0 (- width 2))
                                                 :initial-element #\─) "┐"))
         (mid-border
           (concatenate 'string "├" (make-string (max 0 (- width 2))
                                                 :initial-element #\─) "┤"))
         (bottom-border
           (concatenate 'string "└" (make-string (max 0 (- width 2))
                                                 :initial-element #\─) "┘"))
         (mascot-count (length mark-rows)))
    (multiple-value-bind (art title tagline) (terminal-ui--boot-art)
      (declare (ignore art))
     (labels ((row (style text)
                (list (terminal-span ':plain left)
                      (terminal-span style (termdown:fit-text text width))))

              (boxed (style text)
                (let* ((safe    (termdown:fit-text (sanitize-text text :single-line-p t) inside))
                       (padding (make-string (max 0 (- inside (text-cell-width safe)))
                                             :initial-element #\Space)))
                  (row style (format nil "│ ~A~A │" safe padding))))

              (mascot-row (index text)
                (boxed (terminal-ui--boot-mascot-row-style index mascot-count)
                       (format nil "~A~A"
                               (make-string mark-indent :initial-element #\Space)
                               text))))
       (append
        (list (row ':brand top-border))
        (loop for index from 0
              for mascot-line in mark-rows
              collect (mascot-row index mascot-line))
        (list (row ':brand mid-border)
              (boxed ':brand title)
              (boxed ':hint tagline)
              (boxed ':plain "")
              (boxed ':dim (format nil "Autolith v~A" *autolith-version*))
              (boxed ':brand (format nil ";; ~A" (string-upcase (string phase))))
              (boxed ':plain (or detail "Awaiting operator input."))
              (boxed ':plain "")
              (boxed ':hint
                     (if *terminal-ui-boot-linger-p*
                         (format nil "[ SYSTEM CONSOLE ]          Space: ~A   Ctrl-C: halt"
                                 *terminal-ui-boot-start-label*)
                         "[ SYSTEM CONSOLE ]                         Ctrl-C: halt"))
              (row ':brand bottom-border)))))))

(-> terminal-ui--boot-tip-rows (terminal-ui integer) list)
(defun terminal-ui--boot-tip-rows (ui columns)
  "Wrap and center one cached startup tip, preserving its display styles."
  (when (terminal-ui-fullscreen-p ui)
    (let* ((width (terminal-ui--boot-panel-width columns))
           (tip   (or (fullscreen-terminal-ui-welcome-tip ui)
                      (setf (fullscreen-terminal-ui-welcome-tip ui)
                            (application--startup-tip-spans)))))
      (mapcar (lambda (row)
                (concatenate 'string
                             (make-string (max 0 (floor (- columns (clinedi:ansi-display-width row)) 2))
                                          :initial-element #\Space)
                             row))
              (terminal-ui-fullscreen--display-rows ui (list tip) width)))))

(-> terminal-ui--boot-screen-frame
    (terminal-ui &key (:phase (or string symbol)) (:detail (option string)) (:height integer))
    (values list integer))
(defun terminal-ui--boot-screen-frame (ui &key phase detail height)
  "Return a centered boot panel and tip, reserving a row for direct input.

While the boot screen waits for the operator, a bold call to action follows
the tip."
  (let* ((terminal     (terminal-ui-terminal ui))
         (columns      (max 1 (terminal-columns terminal)))
         (tip          (terminal-ui--boot-tip-rows ui columns))
         (prompt       (when *terminal-ui-boot-linger-p*
                         (cons ""
                               (mapcar
                                (lambda (row)
                                  (concatenate
                                   'string
                                   (make-string (max 0 (floor (- columns (clinedi:ansi-display-width row)) 2))
                                                :initial-element #\Space)
                                   row))
                                (terminal-ui-fullscreen--display-rows
                                 ui (list (list (terminal-span ':strong *terminal-ui-boot-linger-prompt*)))
                                 (terminal-ui--boot-panel-width columns))))))
         (available    (max 0 (1- height)))
         ;; Keep status and advice visible before allocating rows to the mascot.
         (mascot-limit (max 0 (- available 11
                                 (if tip (1+ (length tip)) 0)
                                 (length prompt))))
         (art          (terminal-ui--boot-art))
         (mark-rows    (subseq art 0 (min mascot-limit (length art))))
         (panel        (mapcar (lambda (row) (terminal--render-spans terminal row))
                               (terminal-ui--boot-screen-panel phase detail columns
                                                               :mark-rows mark-rows)))
         (rows         (append panel (when tip (cons "" tip)) prompt))
         (visible      (subseq rows 0 (min (length rows) available)))
         (top          (max 0 (floor (- height (length visible)) 2))))
    (values (append (make-list top :initial-element "") visible)
            (min (max 0 (1- height)) (+ top (length visible))))))

(-> terminal-ui--welcome-rows (terminal-ui integer) list)
(defun terminal-ui--welcome-rows (ui height)
  "Center the machine console and one stable, wrapped tip above the composer."
  (nth-value 0 (terminal-ui--boot-screen-frame
                ui :phase ':listener-ready
                :detail   "Type a request. Use (login) to connect a provider."
                :height   height)))

(-> terminal-ui-boot-screen
    (terminal-ui (or string symbol) &optional (option string)) null)
(defun terminal-ui-boot-screen (ui phase &optional detail)
  "Paint the current startup or login phase, leaving room for direct provider prompts."
  (when (and (terminal-ui-fullscreen-p ui)
             (fullscreen-terminal-ui-active-p ui))
    (with-terminal-ui-locked (ui)
      (multiple-value-bind (rows cursor-row)
          (terminal-ui--boot-screen-frame ui :phase phase :detail detail
                                         :height (terminal-rows (terminal-ui-terminal ui)))
        (terminal-ui-fullscreen-paint
         ui :rows rows :cursor-row cursor-row :cursor-column 0 :whole-screen-p t))))
  nil)

(-> terminal-ui--await-interactive (terminal (or null function) number) boolean)
(defun terminal-ui--await-interactive (terminal wait-function timeout)
  "Poll TERMINAL for up to TIMEOUT seconds until it reports interactive.

A detached world-terminal terminal becomes interactive after the attaching client
reports the terminal size, shortly after the daemon starts listening. Allow time
for that handshake before concluding the terminal is non-interactive."
  (or (terminal-interactive-p terminal)
      (let ((deadline (+ (get-internal-real-time)
                         (round (* timeout internal-time-units-per-second)))))
        (loop while (and (not (terminal-interactive-p terminal))
                         (< (get-internal-real-time) deadline))
              do (funcall (or wait-function #'sleep) 0.02))
        (terminal-interactive-p terminal))))

(defparameter *terminal-ui-boot-linger-poll-seconds* 0.05
  "Seconds between input polls while the boot screen waits for Space.")

(defparameter *terminal-ui-boot-tip-default-seconds* 10
  "Default seconds between startup tips while the boot screen waits.")

(-> terminal-ui--boot-start-event-p (t) boolean)
(defun terminal-ui--boot-start-event-p (event)
  "Return true when EVENT is Space or Enter, the keys that start the session."
  (or (eq event ':submit)
      (and (consp event)
           (eq (first event) ':insert)
           (equal (second event) " "))))

(-> terminal-ui--boot-rotate-tip (terminal-ui) null)
(defun terminal-ui--boot-rotate-tip (ui)
  "Replace the cached startup tip with a different one, when another exists."
  (let ((current (fullscreen-terminal-ui-welcome-tip ui)))
    (loop for candidate = (application--startup-tip-spans)
          repeat 8
          unless (equal candidate current)
            do (setf (fullscreen-terminal-ui-welcome-tip ui) candidate)
               (return)))
  nil)

(-> terminal-ui--boot-listener-event-p (t) boolean)
(defun terminal-ui--boot-listener-event-p (event)
  "Return true when EVENT requests the listener instead of the startup presentation."
  (or (eq event ':escape)
      (and (consp event)
           (member (first event) '(:insert :paste :line))
           (stringp (second event))
           (plusp (length (second event)))
           t)))

(-> terminal-ui-boot-divert-event (terminal-ui t) boolean)
(defun terminal-ui-boot-divert-event (ui event)
  "Consume boot navigation, letting typed input reach the listener's editor.

Space or Enter starts the presentation. Escape or text opens the listener;
text and subsequent editor events pass through the responsive reader normally.
Interrupts and stream ends also pass through to the reader."
  (cond
    ((or (not (terminal-ui-boot-waiting-p ui))
         (eq (terminal-ui-boot-request ui) ':listener))
     nil)
    ((terminal-ui--boot-start-event-p event)
     (setf (terminal-ui-boot-request ui) ':start)
     t)
    ((terminal-ui--boot-listener-event-p event)
     (setf (terminal-ui-boot-request ui) ':listener)
     (eq event ':escape))
    ((member event '(:interrupt :end-of-input :stream-end))
     nil)
    (t
     t)))

(-> terminal-ui--boot-linger
    (terminal-ui &key (:wait-function function) (:tip-seconds real)
                      (:halted-function function) (:direct-input-p-function function)
                      (:resize-callback (option function)))
    keyword)
(defun terminal-ui--boot-linger
    (ui &key wait-function tip-seconds halted-function direct-input-p-function resize-callback)
  "Hold the boot screen until the operator chooses a presentation or the listener.

Space, Enter, and end of input return :START. Escape or text returns :LISTENER,
retaining text in the editor. Ctrl-C or HALTED-FUNCTION returns :INTERRUPT.
Keys arrive through the responsive reader, or directly while
DIRECT-INPUT-P-FUNCTION permits. Rotate tips on WAIT-FUNCTION intervals."
  (let* ((terminal                    (terminal-ui-terminal ui))
         (*terminal-ui-boot-linger-p* t)
         (elapsed                     0))
    (terminal-ui-boot-screen ui ':listener-ready)
    (loop
      (when (terminal-ui-refresh-size ui resize-callback)
        (terminal-ui-boot-screen ui ':listener-ready))
      (cond
        ((terminal-ui-boot-request ui)
         (return (terminal-ui-boot-request ui)))
        ((funcall halted-function)
         (return ':interrupt))
        ((and (funcall direct-input-p-function)
              (terminal-input-ready-p terminal))
         (let ((event (terminal-read-event terminal)))
           (cond
             ((terminal-ui--boot-start-event-p event)
              (return ':start))
             ((terminal-ui--boot-listener-event-p event)
              (unless (eq event ':escape)
                (terminal-ui-process-event
                 ui (if (eq (first event) ':line)
                        (list ':insert (second event))
                        event)))
              (return ':listener))
             ((member event '(:interrupt :detach))
              (return ':interrupt))
             ((member event '(:end-of-input :stream-end))
              (return ':start)))))
        (t
         (funcall wait-function *terminal-ui-boot-linger-poll-seconds*)
         (incf elapsed *terminal-ui-boot-linger-poll-seconds*)
         (when (>= elapsed tip-seconds)
           (setf elapsed 0)
           (terminal-ui--boot-rotate-tip ui)
           (terminal-ui-boot-screen ui ':listener-ready)))))))

(-> terminal-ui-boot-sequence
    (terminal-ui &key (:wait-function function) (:screen-p boolean)
                      (:linger-p boolean) (:tip-seconds real)
                      (:halted-function function) (:direct-input-p-function function)
                      (:resize-callback (option function)))
    keyword)
(defun terminal-ui-boot-sequence
    (ui &key (wait-function #'sleep) (screen-p t) linger-p
             (tip-seconds *terminal-ui-boot-tip-default-seconds*)
             (halted-function (constantly nil))
             (direct-input-p-function (constantly t)) resize-callback)
  "Acquire the fullscreen terminal and optionally wait for Space when ready.

The ready screen appears immediately when SCREEN-P and LINGER-P are true.
Keep ordinary output deferred during the wait. The responsive reader
routes keys through TERMINAL-UI-BOOT-DIVERT-EVENT; read directly only when
DIRECT-INPUT-P-FUNCTION allows it. HALTED-FUNCTION checks for exit requests.
Poll RESIZE-CALLBACK while waiting and repaint resized frames.
Return :START, :LISTENER when Escape or typing bypasses the presentation,
or :INTERRUPT when the wait is interrupted."
  (when (and (terminal-ui-fullscreen-p ui)
             (terminal-ui--await-interactive
              (terminal-ui-terminal ui) wait-function 1.0))
    ;; TERMINAL-UI-START may have run before the detached client attached.
    (unless (fullscreen-terminal-ui-active-p ui)
      (terminal-ui-fullscreen-enter ui))
    (when (and screen-p linger-p)
      (let ((suspended-p nil))
        (with-terminal-ui-locked (ui)
          (setf suspended-p (terminal-ui-live-output-suspended-p ui)
                (terminal-ui-live-output-suspended-p ui) t))
        (setf (terminal-ui-boot-request ui) nil
              (terminal-ui-boot-waiting-p ui) t)
        (return-from terminal-ui-boot-sequence
          (unwind-protect
               (terminal-ui--boot-linger
                ui
                :wait-function           wait-function
                :resize-callback         resize-callback
                :tip-seconds             tip-seconds
                :halted-function         halted-function
                :direct-input-p-function direct-input-p-function)
            (setf (terminal-ui-boot-waiting-p ui) nil)
            (with-terminal-ui-locked (ui)
              (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
              (unless suspended-p
                (terminal-ui--paint-live ui))))))))
  ':start)
