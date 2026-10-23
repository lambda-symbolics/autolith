(in-package #:autolith)

;;;; -- Symbolic Session Presentation --

(-> test-symbolic-session-notices () null)
(defun test-symbolic-session-notices ()
  "Exercise notice severity and untrusted names through the application observer."
  (with-test-configuration (configuration)
    (let* ((terminal (make-instance 'recording-terminal :columns 80 :styled-p t))
           (application (make-instance 'application
                                       :configuration configuration
                                       :ui (terminal-ui-create :terminal terminal)
                                       :conversation (conversation-create configuration
                                                                          :identifier (make-identifier))))
           (unsafe-name (format nil "report~C[2J~%injected" #\Escape)))
      (dolist (case (list (list ':related-memories '(:count 2 :facts 3) ':dim)
                         (list ':related-memories '(:count 2 :stale 1) ':notice)
                         (list ':related-memories '(:count 2 :unverified 1) ':notice)
                         (list ':knowledge-changes '(:count 1) ':notice)
                         (list ':plan-changes '(:count 1) ':notice)
                         (list ':procedure-offers (list :names (list unsafe-name)) ':brand)
                         (list ':procedures-learned (list :procedures (list (list unsafe-name 1 3))) ':success)))
        (destructuring-bind (status details style) case
          (let (presented)
            (test-call-with-function-replacements
             (list (list 'application-present
                         (lambda (application entry)
                           (declare (ignore application))
                           (setf presented entry)
                           t)))
             (lambda ()
               (agent-observer-status (application-agent-observer application) status details)))
            (test-assert (and (terminal-styled-text-p presented)
                              (some (lambda (span) (eq (terminal-span-style span) style)) presented))
                         (format nil "~A has a visible semantic severity" status))
            (let ((text (terminal--spans-text presented)))
              (test-assert (and (plusp (length text))
                                (not (find #\Escape text))
                                (not (find #\Newline text)))
                           "A request-local notice is one safe logical line."))
            (dolist (width '(40 80))
              (let* ((ui (terminal-ui-create :terminal terminal :fullscreen-p t))
                     (viewport (fullscreen-terminal-ui-viewport ui)))
                (terminal-ui-fullscreen--ensure-width ui width)
                (multiple-value-bind (text display regions) (terminal-ui--finalized-content ui presented)
                  (terminal-ui-fullscreen--append ui text display regions))
                (let ((rows (loop for index below (clinedi:transcript-viewport-row-count viewport)
                                  collect (list (clinedi:transcript-viewport-row-text viewport index)
                                                (clinedi:transcript-viewport-row-display viewport index)))))
                  (test-assert (every (lambda (row) (<= (text-cell-width (first row)) width)) rows)
                               "Colored notices fit the fullscreen viewport at normal and narrow widths.")
                  (test-assert (every (lambda (row)
                                       (string= (first row) (cl-colorist:strip-ansi (second row)))) rows)
                               "Transcript reflow preserves the colored notice's visible text.")))))))))
  nil)

(-> test-symbolic-command-styles () null)
(defun test-symbolic-command-styles ()
  "Keep textual procedure and diagnosis states visible beyond their status icons."
  (with-test-configuration (configuration)
    (dolist (case '((:candidate :notice nil nil 0)
                    (:admitted :success 1 nil 0)
                    (:suspended :failure 1 nil 2)
                    (:retired :dim 1 t 0)))
      (destructuring-bind (status style admitted retired failures) case
        (let* ((entry (list :id "report" :admitted admitted :retired-p retired
                            :consecutive-failures failures :runs 3 :failures failures
                            :versions '((:version 1 :procedure (:procedure :steps ((:tool "report.build")))))))
               (spans (application-procedures--entry-lines configuration entry)))
          (test-assert (some (lambda (span)
                              (and (eq (terminal-span-style span) style)
                                   (search (string-downcase (symbol-name status)) (terminal-span-text span))))
                            spans)
                       "A procedure's written status carries the status color."))))
    (let ((unsafe (format nil "endpoint~C[2J~%extra" #\Escape)))
      (dolist (case (list (list nil ':failure) (list (list unsafe "down") ':notice)
                         (list (list unsafe) ':success)))
        (destructuring-bind (remaining style) case
          (let* ((spans (application-diagnoses--case-lines
                         (list :diagnosis unsafe :remaining remaining :repair unsafe
                               :bindings (list (list unsafe unsafe))
                               :eliminated (list (list :hypothesis unsafe :text unsafe)))))
                 (colored (remove-if-not (lambda (span) (eq (terminal-span-style span) style)) spans)))
            (test-assert (and (> (length colored) 1)
                              (some (lambda (span) (> (length (terminal-span-text span)) 4)) colored)
                              (find ':strong spans :key #'terminal-span-style))
                         "A diagnosis emphasizes its name and colors its written outcome.")
            (test-assert (and (not (find #\Escape (terminal--spans-text spans)))
                              (= (count #\Newline (terminal--spans-text spans))
                                 (if (= (length remaining) 1) 3 2)))
                         "Diagnosis data cannot add control sequences or extra logical rows."))))))
  nil)
