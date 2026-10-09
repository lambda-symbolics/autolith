(in-package #:autolith)

;;;; -- Boot and Authentication Presentation --

(-> test-fullscreen-boot-geometry () null)
(defun test-fullscreen-boot-geometry ()
  "Center the machine panel and bound it under terminal resize."
  (let ((terminal (make-instance 'recording-terminal :columns 80 :rows 24)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (multiple-value-bind (frame cursor)
          (terminal-ui--boot-screen-frame ui :phase "phase" :detail "detail" :height 24)
        (let* ((first-row (position-if (lambda (row) (plusp (length row))) frame))
               (last-row (1- (length frame)))
               (plain (clinedi:ansi-strip (nth first-row frame)))
               (left (position-if-not (lambda (character) (char= character #\Space)) plain)))
          (test-assert (<= (abs (- first-row (- 23 last-row))) 1) "panel is vertically centered")
          (test-assert (= left (floor (- 80 64) 2)) "panel is horizontally centered")
          (test-assert (> cursor last-row) "provider prompts start below the panel")))
      (terminal-ui--welcome-rows ui 21)
      (let ((tip (fullscreen-terminal-ui-welcome-tip ui)))
        (terminal-ui--welcome-rows ui 21)
        (test-assert (eq tip (fullscreen-terminal-ui-welcome-tip ui))
                     "welcome repaints retain the selected advice"))
      (setf (fullscreen-terminal-ui-welcome-tip ui)
            (list (terminal-span ':plain
                                 (apply #'concatenate 'string
                                        (make-list 20 :initial-element "wrapped advice ")))))
      (dolist (columns '(1 2 12 24 80))
        (dolist (height '(1 2 7 24))
          (terminal-set-dimensions terminal columns :rows height)
          (let ((welcome (terminal-ui--welcome-rows ui height)))
            (test-assert (<= (length welcome) height) "welcome advice fits the viewport height")
            (test-assert (every (lambda (row) (<= (clinedi:ansi-display-width row) columns)) welcome)
                         "welcome advice reflows within the viewport width"))
          (multiple-value-bind (frame cursor)
              (terminal-ui--boot-screen-frame ui :phase ':login :detail "detail" :height height)
            (test-assert (<= (length frame) height) "panel height is bounded")
            (test-assert (< cursor height) "prompt position is bounded")
            (test-assert (every (lambda (row) (<= (clinedi:ansi-display-width row) columns)) frame)
                         "panel width is bounded"))))))
  nil)

(-> test-fullscreen-authentication-lifecycle () null)
(defun test-fullscreen-authentication-lifecycle ()
  "Keep the alternate buffer while restoring native and relayed authentication I/O on failure."
  (dolist (native-p '(t nil))
    (let ((terminal (make-instance 'recording-terminal :columns 80 :rows 24)))
      (with-terminal-ui (ui (fullscreen-test--ui terminal))
        (terminal-ui-append-finalized ui ':before "transcript")
        (test-assert
         (handler-case
             (application-call-with-authentication-ui
              ui native-p
              (lambda ()
                (test-assert (terminal-ui-live-output-suspended-p ui) "provider owns output")
                (test-assert (fullscreen-terminal-ui-active-p ui) "authentication retains alternate ownership")
                (when native-p
                  (test-assert (not (terminal-started-p terminal)) "native provider input uses restored terminal mode"))
                (error "Expected authentication cancellation.")))
           (simple-error () t)) "provider cancellation is propagated")
        (test-assert (terminal-started-p terminal) "terminal input mode is restored")
        (test-assert (not (terminal-ui-live-output-suspended-p ui)) "live output resumes")
        (test-assert (fullscreen-terminal-ui-active-p ui) "session remains usable")
        (test-assert (= 1 (fullscreen-test--chunk-count ui)) "authentication presentation is not transcript content"))))
  nil)


(-> test-fullscreen-boot-cursor-lifecycle () null)
(defun test-fullscreen-boot-cursor-lifecycle ()
  "Hide the input cursor during boot and restore it on normal and interrupted handoff."
  (let ((terminal (make-instance 'queued-recording-terminal :columns 80 :rows 24)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (flet ((cursor-visible-p ()
               (let* ((output (recording-terminal-output terminal))
                      (shown (search (format nil "~C[?25h" #\Escape) output :from-end t))
                      (hidden (search (format nil "~C[?25l" #\Escape) output :from-end t)))
                 (and shown (or (null hidden) (> shown hidden)) t))))
        (let ((visibility nil))
          (terminal-ui-boot-sequence
           ui :duration 0 :linger-p t
              :wait-function
              (lambda (seconds)
                (declare (ignore seconds))
                (push (cursor-visible-p) visibility)
                (when (> (length visibility) (length *terminal-ui-boot-sequence-phases*))
                  (queued-recording-terminal-enqueue terminal '(:insert " ")))))
          (test-assert (and (= (length visibility)
                              (1+ (length *terminal-ui-boot-sequence-phases*)))
                            (notany #'identity visibility))
                       "boot animation and the Space wait hide the input cursor")
          (test-assert (cursor-visible-p) "the composer handoff restores the input cursor"))
        (handler-case
            (terminal-ui-boot-sequence
             ui :duration 0
                :wait-function (lambda (seconds)
                                 (declare (ignore seconds))
                                 (error "Expected boot interruption.")))
          (simple-error () nil))
        (test-assert (cursor-visible-p) "boot interruption restores the input cursor"))))
  nil)

(-> test-fullscreen-boot-sequence () null)
(defun test-fullscreen-boot-sequence ()
  "Keep boot frames visible through refresh, then restore deferred transcript output."
  (let ((terminal (make-instance 'recording-terminal :columns 80 :rows 24)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (setf (fullscreen-terminal-ui-welcome-tip ui)
            (list (terminal-span ':hint "operator advice")))
      (let ((frames nil)
            (seconds 0))
        (terminal-ui-boot-sequence
         ui :wait-function
         (lambda (duration)
           (incf seconds duration)
           (push (fullscreen-test--frame ui) frames)
           (let* ((frame (first frames))
                  (tip-row (find-if (lambda (row) (search "operator advice" row)) frame)))
             (test-assert tip-row "each boot stage displays the tip")
             (test-assert (= 32 (search "operator advice" (clinedi:ansi-strip tip-row)))
                          "short tips are centered within the terminal")
             (terminal-ui-append-finalized ui (length frames) "deferred output")
             (terminal-ui--paint-live ui)
             (test-assert (equalp frame (fullscreen-test--frame ui))
                          "ordinary redraw cannot erase a boot stage"))))
        (test-assert (= (length *terminal-ui-boot-sequence-phases*) (length frames))
                     "boot displays each configured phase")
        (test-assert (> seconds 1) "fast startup still leaves time to see the boot screen")
        (test-assert (not (equalp (first frames) (second frames))) "boot stages visibly advance")
        (test-assert (not (terminal-ui-live-output-suspended-p ui)) "ordinary output resumes")
        (test-assert (search "deferred output" (clinedi:transcript-viewport-chunk-text (fullscreen-terminal-ui-viewport ui) 0))
                     "output accepted during boot is published afterward"))
      (test-assert
       (handler-case
           (terminal-ui-boot-sequence ui :wait-function (lambda (seconds)
                                                        (declare (ignore seconds))
                                                        (error "Interrupted boot.")))
         (simple-error () t)) "boot interruption propagates")
      (test-assert (not (terminal-ui-live-output-suspended-p ui)) "interrupted boot restores output ownership")))
  nil)


(-> test-fullscreen-boot-disabled () null)
(defun test-fullscreen-boot-disabled ()
  "Open the listener at once when the boot screen is turned off."
  (let ((terminal (make-instance 'recording-terminal :columns 80 :rows 24))
        (waits 0))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (test-assert (eq ':start
                       (terminal-ui-boot-sequence
                        ui :screen-p nil :linger-p t
                           :wait-function (lambda (seconds)
                                            (declare (ignore seconds))
                                            (incf waits))))
                   "a disabled boot screen starts the listener")
      (test-assert (zerop waits) "a disabled boot screen neither animates nor waits for Space")
      (test-assert (fullscreen-terminal-ui-active-p ui) "the fullscreen terminal is still acquired")
      (test-assert (not (search (second (first *terminal-ui-boot-sequence-phases*))
                                (recording-terminal-output terminal)))
                   "no boot phase is painted")))
  nil)


(-> test-fullscreen-boot-linger () null)
(defun test-fullscreen-boot-linger ()
  "Hold the boot screen for Space, rotate tips on schedule, and report interrupts."
  (let ((terminal (make-instance 'queued-recording-terminal :columns 80 :rows 24 :styled-p t))
        (tips 0))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (test-call-with-function-replacements
       (list (list 'application--startup-tip-spans
                   (lambda ()
                     (list (terminal-span ':hint (format nil "advice ~D" (incf tips)))))))
       (lambda ()
         (let* ((seconds 0)
                (waiting-frame nil)
                (result
                  (terminal-ui-boot-sequence
                   ui :duration 0 :linger-p t :tip-seconds 1
                   :wait-function
                   (lambda (duration)
                     (incf seconds duration)
                     (when (> seconds 2.5)
                       ;; The last frame painted while still waiting.
                       (setf waiting-frame (fullscreen-test--frame ui))
                       (queued-recording-terminal-enqueue terminal '(:insert " ")))))))
           (test-assert (eq result ':start) "Space starts the session")
           (test-assert (= tips 3) "the tip rotates on the configured interval while waiting")
           (test-assert (find-if (lambda (row) (search "advice 3" row)) waiting-frame)
                        "the rotated tip is painted")
           (test-assert (find-if (lambda (row) (search "Space: start" row)) waiting-frame)
                        "the lingering panel's console line advertises Space")
           (let ((tip-row (position-if (lambda (row) (search "advice 3" row)) waiting-frame))
                 (prompt-row (position-if (lambda (row) (search "PRESS SPACE TO START" row))
                                          waiting-frame)))
             (test-assert (and tip-row prompt-row (= prompt-row (+ tip-row 2)))
                          "the call to action sits one blank row below the tip")
             (test-assert (and prompt-row
                               (search (terminal-style-sequence ':strong)
                                       (elt waiting-frame prompt-row)))
                          "the call to action is bold"))
           (test-assert (not (terminal-ui-live-output-suspended-p ui))
                        "ordinary output resumes after the wait"))
         (flet ((boot ()
                  (terminal-ui-boot-sequence ui :duration 0 :linger-p t
                                                :wait-function (lambda (seconds)
                                                                 (declare (ignore seconds))))))
           (queued-recording-terminal-enqueue terminal ':interrupt)
           (test-assert (eq (boot) ':interrupt) "Ctrl-C while waiting reports an interrupt")
           (queued-recording-terminal-enqueue terminal '(:insert "x"))
           (queued-recording-terminal-enqueue terminal '(:resize 80 24))
           (queued-recording-terminal-enqueue terminal ':submit)
           (test-assert (eq (boot) ':start) "other events are ignored and Enter starts"))
         (test-assert (eq (terminal-ui-boot-sequence
                           ui :duration 0 :linger-p nil
                              :wait-function (lambda (seconds) (declare (ignore seconds))))
                          ':start)
                      "without lingering the boot screen opens the listener at once")
         (let ((frames nil))
           (terminal-ui-boot-sequence
            ui :duration 0 :linger-p nil
               :wait-function (lambda (seconds)
                                (declare (ignore seconds))
                                (push (fullscreen-test--frame ui) frames)))
           (test-assert (notany (lambda (frame)
                                  (find-if (lambda (row) (search "PRESS SPACE" row)) frame))
                                frames)
                        "a boot without waiting never advertises Space"))))))
  nil)

(-> test-fullscreen-boot-reader-diversion () null)
(defun test-fullscreen-boot-reader-diversion ()
  "Route keys through the reader while the boot screen waits, and honour exit requests."
  (let ((terminal (make-instance 'queued-recording-terminal :columns 80 :rows 24)))
    (with-terminal-ui (ui (fullscreen-test--ui terminal))
      (test-assert (not (terminal-ui-boot-divert-event ui '(:insert " ")))
                   "keys pass to the reader when no boot screen waits")
      (setf (terminal-ui-boot-waiting-p ui) t)
      (test-assert (and (terminal-ui-boot-divert-event ui '(:insert "x"))
                        (not (terminal-ui-boot-start-requested-p ui)))
                   "other keys are swallowed while waiting")
      (test-assert (not (terminal-ui-boot-divert-event ui ':interrupt))
                   "interrupts still reach the reader while waiting")
      (test-assert (and (terminal-ui-boot-divert-event ui ':submit)
                        (terminal-ui-boot-start-requested-p ui))
                   "Enter through the reader requests the start")
      (setf (terminal-ui-boot-waiting-p ui) nil
            (terminal-ui-boot-start-requested-p ui) nil)
      (let ((polls 0))
        (queued-recording-terminal-enqueue terminal '(:insert " "))
        (test-assert
         (eq (terminal-ui-boot-sequence
              ui :duration 0 :linger-p t
                 :direct-input-p-function (constantly nil)
                 :wait-function (lambda (seconds)
                                  (declare (ignore seconds))
                                  (when (= (incf polls) 5)
                                    (terminal-ui-boot-divert-event ui '(:insert " ")))))
             ':start)
         "a start key diverted by the reader ends the wait")
        (test-assert (terminal-input-ready-p terminal)
                     "the wait leaves direct input alone while the reader is live")
        (test-assert (not (terminal-ui-boot-waiting-p ui))
                     "the boot screen releases keystrokes after starting"))
      (let ((polls 0))
        (test-assert
         (eq (terminal-ui-boot-sequence
              ui :duration 0 :linger-p t
                 :direct-input-p-function (constantly nil)
                 :halted-function (lambda () (> polls 3))
                 :wait-function (lambda (seconds)
                                  (declare (ignore seconds))
                                  (incf polls)))
             ':interrupt)
         "an exit requested through the reader ends the wait as an interrupt"))
      (test-assert
       (eq (terminal-ui-boot-sequence
            ui :duration 0 :linger-p t
               :wait-function (lambda (seconds) (declare (ignore seconds))))
           ':start)
       "the queued direct key still starts once direct input is allowed")))
  nil)
