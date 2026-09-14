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
               (left (position #\+ plain)))
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
        (test-assert (= 1 (length (fullscreen-terminal-ui-chunks ui))) "authentication presentation is not transcript content"))))
  nil)
