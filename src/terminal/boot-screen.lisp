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

(defparameter *terminal-ui-boot-mascot-styles*
  #(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
    :brand-gradient-4 :brand-gradient-5 :brand-gradient-6)
  "Row styles cycling top-to-bottom across the boot mascot art.")

(-> terminal-ui--boot-mascot-row-style (integer integer) terminal-style)
(defun terminal-ui--boot-mascot-row-style (row total)
  "Return ROW's gradient style out of TOTAL rows of boot mascot art."
  (let ((styles *terminal-ui-boot-mascot-styles*))
    (aref styles (min (1- (length styles))
                      (floor (* row (length styles)) (max 1 total))))))

(-> terminal-ui--boot-screen-panel ((or string symbol) (option string) integer) list)
(defun terminal-ui--boot-screen-panel (phase detail columns)
  "Return horizontally centered styled rows for the actual PHASE and DETAIL."
  (let* ((width (min 64 (max 1 (- columns 4))))
         (inside (max 0 (- width 4)))
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
         (mascot-count (length *terminal-ui-boot-mascot-rows*)))
    (labels ((row (style text)
               (list (terminal-span ':plain left)
                     (terminal-span style (layout-fit-text text width))))

             (boxed (style text)
               (let* ((safe (layout-fit-text (sanitize-text text :single-line-p t) inside))
                      (padding (make-string (max 0 (- inside (text-cell-width safe)))
                                            :initial-element #\Space)))
                 (row style (format nil "│ ~A~A │" safe padding))))

             (mascot-row (index text)
               (let ((indent (max 0 (floor (- inside (text-cell-width text)) 2))))
                 (boxed (terminal-ui--boot-mascot-row-style index mascot-count)
                        (format nil "~A~A"
                                (make-string indent :initial-element #\Space)
                                text)))))
      (append
       (list (row ':brand top-border))
       (loop for index from 0
             for mascot-line in *terminal-ui-boot-mascot-rows*
             collect (mascot-row index mascot-line))
       (list (row ':brand mid-border)
             (boxed ':brand "A U T O L I T H  /  LISP MACHINE")
             (boxed ':hint "READ . EVAL . PRINT . LOOP")
             (boxed ':plain "")
             (boxed ':plain (format nil "(boot :image ~S)"
                                    (format nil "~A ~A" (lisp-implementation-type)
                                            (lisp-implementation-version))))
             (boxed ':brand (format nil ";; ~A" (string-upcase (string phase))))
             (boxed ':plain (or detail "Awaiting operator input."))
             (boxed ':plain "")
             (boxed ':hint "[ SYSTEM CONSOLE ]                         Ctrl-C: halt")
             (row ':brand bottom-border))))))

(-> terminal-ui--boot-tip-rows (terminal-ui integer) list)
(defun terminal-ui--boot-tip-rows (ui columns)
  "Wrap and center one cached startup tip, preserving its display styles."
  (when (terminal-ui-fullscreen-p ui)
    (let* ((width (min 64 (max 1 (- columns 4))))
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
  "Return a centered boot panel and tip, reserving a row for direct input."
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (max 1 (terminal-columns terminal)))
         (panel (mapcar (lambda (row) (terminal--render-spans terminal row))
                        (terminal-ui--boot-screen-panel phase detail columns)))
         (tip (terminal-ui--boot-tip-rows ui columns))
         (rows (append panel (when tip (cons "" tip))))
         (visible (subseq rows 0 (min (length rows) (max 0 (1- height)))))
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
    (:listener-ready "[######]  World awake. Operator, the listener is yours."))
  "The ordered (PHASE DETAIL) pairs painted across the boot sequence.")

(-> terminal-ui-boot-sequence-duration () real)
(defun terminal-ui-boot-sequence-duration ()
  "Return the boot sequence's total animation duration in seconds.

Reads AUTOLITH_BOOT_DURATION when set, else
*TERMINAL-UI-BOOT-SEQUENCE-DEFAULT-DURATION*."
  (environment-positive-real "AUTOLITH_BOOT_DURATION"
                             *terminal-ui-boot-sequence-default-duration*))

(-> terminal-ui-boot-sequence
    (terminal-ui &key (:wait-function function) (:duration real))
    null)
(defun terminal-ui-boot-sequence
    (ui &key (wait-function #'sleep) (duration (terminal-ui-boot-sequence-duration)))
  "Present a brief Lisp-machine boot sequence before opening the listener.

Keep ordinary output deferred throughout the presentation. WAIT-FUNCTION accepts
seconds; DURATION is the total seconds spent across all boot phases, split
evenly, and defaults to TERMINAL-UI-BOOT-SEQUENCE-DURATION."
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
      (unwind-protect
           (dolist (phase *terminal-ui-boot-sequence-phases*)
             (terminal-ui-boot-screen ui (first phase) (second phase))
             (funcall wait-function phase-duration))
        (with-terminal-ui-locked (ui)
          (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
          (unless suspended-p
            (terminal-ui--paint-live ui))))))
  nil)
