(in-package #:autolith)

;;;; -- Lisp-Machine Startup and Login --

(-> terminal-ui--boot-screen-panel ((or string symbol) (option string) integer) list)
(defun terminal-ui--boot-screen-panel (phase detail columns)
  "Return horizontally centered styled rows for the actual PHASE and DETAIL."
  (let* ((width (min 64 (max 1 (- columns 4))))
         (inside (max 0 (- width 4)))
         (left (make-string (max 0 (floor (- columns width) 2))
                            :initial-element #\Space))
         (border (concatenate 'string "+" (make-string (max 0 (- width 2))
                                                        :initial-element #\-) "+")))
    (labels ((row (style text)
               (list (terminal-span ':plain left)
                     (terminal-span style (layout-fit-text text width))))

             (boxed (style text)
               (let* ((safe (layout-fit-text (sanitize-text text :single-line-p t) inside))
                      (padding (make-string (max 0 (- inside (text-cell-width safe)))
                                            :initial-element #\Space)))
                 (row style (format nil "| ~A~A |" safe padding)))))
      (list (row ':brand border)
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
            (row ':brand border)))))

(-> terminal-ui--boot-screen-frame
    (terminal-ui &key (:phase (or string symbol)) (:detail (option string)) (:height integer))
    (values list integer))
(defun terminal-ui--boot-screen-frame (ui &key phase detail height)
  "Return display rows centered within HEIGHT and the first row below the panel."
  (let* ((terminal (terminal-ui-terminal ui))
         (panel (terminal-ui--boot-screen-panel phase detail (terminal-columns terminal)))
         (visible (subseq panel 0 (min (length panel) (max 0 (1- height)))))
         (top (max 0 (floor (- height (length visible)) 2))))
    (values
     (append (make-list top :initial-element "")
             (mapcar (lambda (row) (terminal--render-spans terminal row)) visible))
     (min (max 0 (1- height)) (+ top (length visible))))))

(-> terminal-ui--welcome-rows (terminal-ui integer) list)
(defun terminal-ui--welcome-rows (ui height)
  "Return the empty session's machine console above its pinned composer."
  (nth-value 0 (terminal-ui--boot-screen-frame
                ui :phase ':listener-ready
                :detail "Type a request. Use (login) to connect a provider." :height height)))

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
