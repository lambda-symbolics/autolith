(in-package #:autolith)

;;;; -- Lisp-Machine Startup and Login --

(defparameter *terminal-ui-boot-mascot-rows*
  '(
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠄⠀⠀⠀⠐⠐⠀⠰⠀⠄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠂⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⢀⠴⡶⠠⠀⢀⠀⠀⠀⠀⠀⠀⠂⠀⠀⠀⠀⠈⠁⠀⠁⠀⠀⠀⠠⡀⠀⠀⠀⠀⠀⢀⢡⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠠⣐⣬⡴⣶⣚⡾⠒⠓⠺⠷⠦⠄⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⠀⠀⠰⠁⠀⡂⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⡴⠟⠋⠁⠀⠀⠸⡹⡃⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠒⠒⠀⢠⢀⠀⠐⠒⠒⠀⠀⠀⡀⠀⡀⡀⠑⠀⠀⢆⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⡄⡆⠀⠀⠀⠀⠀⠀⠀⠂⠀⠀⠺⠀⠀⣄⣾⠀⠀⠰⠆⠀⠀⠀⡀⠀⠀⠁⠀⠀⠈⠠⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠃⡅⠀⠀⠀⠀⠀⠀⢨⠀⠀⠀⠀⠀⠀⠉⠁⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⠀⠀⢠⡀⠀⡂⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⢠⣇⠇⡀⠀⠀⠀⠀⠀⠠⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⡠⡀⠄⠀⠓⠅⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠸⡿⣷⠠⡀⠀⠀⠀⠀⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⢐⠀⠀⠀⠠⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⡅⢼⢄⠈⠂⠤⠠⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⡀⠀⠄⡇⠀⡅⡀⣠⣾⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠲⣺⠀⠉⠒⠒⠒⠛⠀⠀⠐⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠄⣀⣀⣴⡧⠀⣿⣿⣟⣾⡇⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠈⠉⠀⠀⠀⠀⠀⠄⠀⠀⠀⠠⠀⠀⠀⠀⠀⠀⢀⣁⣴⣶⠒⡢⢶⣿⣿⣿⠀⢽⣿⣿⡏⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢆⠀⠀⠀⠄⠀⠀⠀⣀⣠⣴⣿⡟⣿⣿⣽⣾⣏⣬⣿⣿⠀⣸⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠒⣄⡀⡅⢀⣵⣿⣻⣿⣻⣾⣿⣿⣿⡿⣿⣿⣿⡿⠿⠀⠘⠋⠀⠀⠀⢐⠱⠄⢀⡠⠐⠂"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠁⢩⠟⢿⠀⠈⠉⠉⠉⢩⠿⢿⠀⠀⠀⢺⣰⠆⠀⣀⢀⣐⣱⡶⢞⡁⠰⠂⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠔⠁⠡⡀⠀⠀⠀⠀⠀⠀⠀⠀⠄⠀⠀⠀⠀⠀⢀⠀⠨⡀⡀⠀⠀⠄⠀⠊⠀⠀⢀⠠⢂⠈⠡⢲⡦⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⡠⠉⡂⠏⣢⠘⢄⢀⣄⣀⠀⠀⡀⠀⡇⠀⠄⢄⢐⠀⠸⠀⠸⠀⡠⡶⢄⠄⠁⠀⠂⢠⡀⠲⡒⢌⠉⣉⡀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠊⡀⠠⢰⣺⣷⡼⠦⠟⠙⠉⠁⠁⣰⣤⣧⠀⠈⡠⣀⡀⠨⣀⣸⡀⠈⠈⠉⠀⣀⠀⢠⠃⢅⡀⠼⣮⡇⠈⠀⠀"
    "⠀⢀⠀⠴⠻⠲⠑⠇⠖⠚⠛⠌⠀⠀⠐⠂⠈⠀⠁⢀⢸⣿⣿⢀⢌⣈⢕⣈⢌⣿⣿⣇⠠⣄⢠⠠⠉⠤⠃⠳⠧⣗⣀⠿⠋⠀⠀⠀"
    "⠀⠀⠀⠀⠀⢀⡊⣿⣲⣌⠀⠀⠚⠛⠀⠀⠀⠀⠴⡿⠿⠿⡛⠓⢋⠪⢌⣭⣿⣿⢿⠿⠕⢋⣑⠈⠐⠉⠓⠑⠚⠋⠅⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠐⠀⠉⠀⠁⠁⠀⠀⠀⠀⣀⢁⠀⠂⠠⠖⢀⠤⠦⢄⠀⠀⠀⠀⠁⠀⠀⠠⠐⠻⠛⠂⠀⠀⠂⠁⠀⠀⠀⠀⠀⠀⠀⠀"
    "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⠁⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    )
  "A dithered braille rendering of the Autolith rock mascot, for the boot panel.")

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
  (let* ((width (terminal-ui--boot-panel-width columns))
         (inside (max 0 (- width 4)))
         (mark-width (loop for text in mark-rows maximize (text-cell-width text)))
         (mark-indent (max 0 (floor (- inside mark-width) 2)))
         (left (make-string (max 0 (floor (- columns width) 2))
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
                     (terminal-span style (layout-fit-text text width))))

             (boxed (style text)
               (let* ((safe (layout-fit-text (sanitize-text text :single-line-p t) inside))
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
             (boxed ':plain (format nil "(boot :image ~S)"
                                    (format nil "~A ~A" (lisp-implementation-type)
                                            (lisp-implementation-version))))
             (boxed ':brand (format nil ";; ~A" (string-upcase (string phase))))
             (boxed ':plain (or detail "Awaiting operator input."))
             (boxed ':plain "")
             (boxed ':hint
                    (if *terminal-ui-boot-linger-p*
                        "[ SYSTEM CONSOLE ]          Space: start   Ctrl-C: halt"
                        "[ SYSTEM CONSOLE ]                         Ctrl-C: halt"))
             (row ':brand bottom-border)))))))

(-> terminal-ui--boot-tip-rows (terminal-ui integer) list)
(defun terminal-ui--boot-tip-rows (ui columns)
  "Wrap and center one cached startup tip, preserving its display styles."
  (when (terminal-ui-fullscreen-p ui)
    (let* ((width (terminal-ui--boot-panel-width columns))
           (tip (or (fullscreen-terminal-ui-welcome-tip ui)
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
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (max 1 (terminal-columns terminal)))
         (tip (terminal-ui--boot-tip-rows ui columns))
         (prompt (when *terminal-ui-boot-linger-p*
                   (list ""
                         (terminal--render-spans
                          terminal
                          (list (terminal-span
                                 ':plain
                                 (make-string
                                  (max 0 (floor (- columns
                                                   (length *terminal-ui-boot-linger-prompt*))
                                                2))
                                  :initial-element #\Space))
                                (terminal-span ':strong *terminal-ui-boot-linger-prompt*))))))
         (available (max 0 (1- height)))
         ;; Keep status and advice visible before allocating rows to the mascot.
         (mascot-limit (max 0 (- available 11
                                 (if tip (1+ (length tip)) 0)
                                 (length prompt))))
         (art (terminal-ui--boot-art))
         (mark-rows (subseq art 0 (min mascot-limit (length art))))
         (panel (mapcar (lambda (row) (terminal--render-spans terminal row))
                        (terminal-ui--boot-screen-panel phase detail columns
                                                        :mark-rows mark-rows)))
         (rows (append panel (when tip (cons "" tip)) prompt))
         (visible (subseq rows 0 (min (length rows) available)))
         (top (max 0 (floor (- height (length visible)) 2))))
    (values (append (make-list top :initial-element "") visible)
            (min (max 0 (1- height)) (+ top (length visible))))))

(-> terminal-ui--welcome-rows (terminal-ui integer) list)
(defun terminal-ui--welcome-rows (ui height)
  "Center the machine console and one stable, wrapped tip above the composer."
  (nth-value 0 (terminal-ui--boot-screen-frame
                ui :phase ':listener-ready
                :detail "Type a request. Use (login) to connect a provider."
                :height height)))

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
        (terminal-ui-fullscreen-paint ui :rows rows :cursor-row cursor-row :cursor-column 0))))
  nil)


(-> terminal-ui--await-interactive (terminal (or null function) number) boolean)
(defun terminal-ui--await-interactive (terminal wait-function timeout)
  "Poll TERMINAL for up to TIMEOUT seconds until it reports interactive.

A detached localgroup terminal starts non-interactive and only flips once
its attaching client reports its size over the wire, shortly after the
localgroup daemon begins listening. This gives that handshake a brief
window instead of judging interactivity before it could possibly happen."
  (or (terminal-interactive-p terminal)
      (let ((deadline (+ (get-internal-real-time)
                          (round (* timeout internal-time-units-per-second)))))
        (loop while (and (not (terminal-interactive-p terminal))
                         (< (get-internal-real-time) deadline))
              do (funcall (or wait-function #'sleep) 0.02))
        (terminal-interactive-p terminal))))

(defparameter *terminal-ui-boot-sequence-default-duration* 3.5
  "The default total seconds TERMINAL-UI-BOOT-SEQUENCE spends animating.")

(defparameter *terminal-ui-boot-sequence-phases*
  '((:cold-boot "[#.....]  Waking the saved Lisp world.")
    (:image-load "[##....]  Reading the boulder back off stable storage.")
    (:gc-prime "[###...]  Priming the generational garbage collector.")
    (:cons-check "[####..]  Verifying cons cells are still pointy.")
    (:reader-sync "[#####.]  Synchronizing reader macros.")
    (:listener-ready "[######]  You are typing to Autolith Lisp Listener 1."))
  "The ordered (PHASE DETAIL) pairs painted across the boot sequence.")

(-> terminal-ui-boot-sequence-duration () real)
(defun terminal-ui-boot-sequence-duration ()
  "Return the boot sequence's total animation duration in seconds.

Reads AUTOLITH_BOOT_DURATION when set, else
*TERMINAL-UI-BOOT-SEQUENCE-DEFAULT-DURATION*."
  (environment-positive-real "AUTOLITH_BOOT_DURATION"
                             *terminal-ui-boot-sequence-default-duration*))

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
    (loop repeat 8
          for candidate = (application--startup-tip-spans)
          unless (equal candidate current)
            do (setf (fullscreen-terminal-ui-welcome-tip ui) candidate)
               (return)))
  nil)

(-> terminal-ui-boot-divert-event (terminal-ui t) boolean)
(defun terminal-ui-boot-divert-event (ui event)
  "Consume EVENT for a waiting boot screen, returning true when the reader must not process it.

While the boot screen waits, a start key requests the session and other keys
are dropped. Interrupts and stream ends are left to the reader so Ctrl-C halts
the same way it does at an empty prompt."
  (cond
    ((not (terminal-ui-boot-waiting-p ui))
     nil)
    ((terminal-ui--boot-start-event-p event)
     (setf (terminal-ui-boot-start-requested-p ui) t)
     t)
    ((member event '(:interrupt :end-of-input :stream-end))
     nil)
    (t
     t)))

(-> terminal-ui--boot-linger
    (terminal-ui &key (:wait-function function) (:tip-seconds real)
                      (:halted-function function) (:direct-input-p-function function))
    keyword)
(defun terminal-ui--boot-linger
    (ui &key wait-function tip-seconds halted-function direct-input-p-function)
  "Hold the boot screen until the operator starts the session.

Keys arrive through the responsive reader, which diverts them into the UI's
boot flags, or directly from the terminal while DIRECT-INPUT-P-FUNCTION
permits. Space, Enter, and end of input return :START; Ctrl-C, or
HALTED-FUNCTION reporting an exit request, returns :INTERRUPT. The startup tip
rotates every TIP-SECONDS, counted in WAIT-FUNCTION calls so scripted waits
stay deterministic."
  (let* ((terminal (terminal-ui-terminal ui))
         (*terminal-ui-boot-linger-p* t)
         (detail (second (first (last *terminal-ui-boot-sequence-phases*))))
         (elapsed 0))
    (terminal-ui-boot-screen ui ':listener-ready detail)
    (loop
      (cond
        ((terminal-ui-boot-start-requested-p ui)
         (return ':start))
        ((funcall halted-function)
         (return ':interrupt))
        ((and (funcall direct-input-p-function)
              (terminal-input-ready-p terminal))
         (let ((event (terminal-read-event terminal)))
           (cond
             ((terminal-ui--boot-start-event-p event)
              (return ':start))
             ((eq event ':interrupt)
              (return ':interrupt))
             ((member event '(:end-of-input :stream-end))
              (return ':start)))))
        (t
         (funcall wait-function *terminal-ui-boot-linger-poll-seconds*)
         (incf elapsed *terminal-ui-boot-linger-poll-seconds*)
         (when (>= elapsed tip-seconds)
           (setf elapsed 0)
           (terminal-ui--boot-rotate-tip ui)
           (terminal-ui-boot-screen ui ':listener-ready detail)))))))

(-> terminal-ui-boot-sequence
    (terminal-ui &key (:wait-function function) (:duration real)
                      (:linger-p boolean) (:tip-seconds real)
                      (:halted-function function) (:direct-input-p-function function))
    keyword)
(defun terminal-ui-boot-sequence
    (ui &key (wait-function #'sleep)
             (duration (terminal-ui-boot-sequence-duration))
             linger-p
             (tip-seconds *terminal-ui-boot-tip-default-seconds*)
             (halted-function (constantly nil))
             (direct-input-p-function (constantly t)))
  "Present a brief Lisp-machine boot sequence before opening the listener.

Keep ordinary output deferred throughout the presentation. WAIT-FUNCTION accepts
seconds; DURATION is the total seconds spent across all boot phases, split
evenly, and defaults to TERMINAL-UI-BOOT-SEQUENCE-DURATION. With LINGER-P the
screen then waits for Space, rotating the tip every TIP-SECONDS. Throughout,
the boot screen owns keystrokes: the responsive reader diverts them through
TERMINAL-UI-BOOT-DIVERT-EVENT, and the wait reads the terminal itself only
while DIRECT-INPUT-P-FUNCTION allows. HALTED-FUNCTION reports an exit request
made elsewhere. Returns :START, or :INTERRUPT when Ctrl-C ended the wait."
  (let ((result ':start))
    (when (and (terminal-ui-fullscreen-p ui)
               (terminal-ui--await-interactive
                (terminal-ui-terminal ui) wait-function 1.0))
      ;; TERMINAL-UI-START ran before the detached terminal's client attached,
      ;; so its own fullscreen-enter attempt was skipped; retry now that the
      ;; terminal reports interactive.
      (unless (fullscreen-terminal-ui-active-p ui)
        (terminal-ui-fullscreen-enter ui))
      (let ((suspended-p nil)
            (phase-duration
              (/ (max 0 duration) (length *terminal-ui-boot-sequence-phases*))))
        (with-terminal-ui-locked (ui)
          (setf suspended-p (terminal-ui-live-output-suspended-p ui)
                (terminal-ui-live-output-suspended-p ui) t))
        (setf (terminal-ui-boot-start-requested-p ui) nil
              (terminal-ui-boot-waiting-p ui) t)
        (unwind-protect
             (progn
               (dolist (phase *terminal-ui-boot-sequence-phases*)
                 (terminal-ui-boot-screen ui (first phase) (second phase))
                 (funcall wait-function phase-duration))
               (when linger-p
                 (setf result
                       (terminal-ui--boot-linger
                        ui
                        :wait-function wait-function
                        :tip-seconds tip-seconds
                        :halted-function halted-function
                        :direct-input-p-function direct-input-p-function))))
          (setf (terminal-ui-boot-waiting-p ui) nil)
          (with-terminal-ui-locked (ui)
            (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
            (unless suspended-p
              (terminal-ui--paint-live ui))))))
    result))
