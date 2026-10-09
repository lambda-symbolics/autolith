(in-package #:autolith)

;;;; -- Complete Input History --

(-> test-conversation-input-history-complete () null)
(defun test-conversation-input-history-complete ()
  "Test unbounded history, filtering and maintenance without archived reads."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity (conversation-pathname conversation))
           (expected nil))
      (dotimes (index 130)
        (let ((input (format nil "input ~D" index)))
          (push input expected)
          (conversation-append-user-message conversation input))
        (when (zerop (mod (1+ index) 25))
          (conversation-append-summary conversation "checkpoint")))
      (dolist (kind '(:command :lisp))
        (push "duplicate" expected)
        (conversation-append-user-operation
         conversation :kind kind :source "duplicate" :status ':ok :result "ignored"))
      (conversation-append-record
       conversation '(:message :role :user :content "automatic" :automatic-p t))
      (conversation-append-record
       conversation '(:message :role :assistant :content "assistant"))
      (setf expected (nreverse expected))
      (test-assert (equal (conversation-input-history conversation) expected)
                   "all 132 editable inputs retain chronological order and duplicate text")
      (with-open-file (stream (first (conversation-storage-pathnames identity))
                              :direction ':output :if-exists ':supersede)
        (write-string "(" stream))
      (test-call-with-function-replacements
       (list (list 'conversation--map-storage-records
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Input history must not scan retired segments."))))
       (lambda ()
         (let ((loaded (conversation-load identity)))
           (test-assert (equal (conversation-input-history loaded) expected)
                        "reload reads the complete sidecar despite a damaged retired segment")
           (conversation-append-user-message loaded "new input")
           (conversation-append-summary loaded "another checkpoint")
           (test-assert
            (equal (conversation-input-history (conversation-load identity))
                   (append expected '("new input")))
            "append and rotation maintain the complete index without archived reads"))))))
  nil)

(-> test-conversation-input-history-rebuild () null)
(defun test-conversation-input-history-rebuild ()
  "Test a missing sidecar is built once, then appended rather than rewritten."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity (conversation-pathname conversation))
           (sidecar (conversation-input-history-pathname identity))
           (original-map (symbol-function 'conversation--map-storage-records))
           (original-write (symbol-function 'sexp-store:log-write))
           (scans 0)
           (publications 0))
      (conversation-append-user-message conversation "retired")
      (conversation-append-summary conversation "checkpoint")
      (conversation-append-user-message conversation "active")
      (delete-file sidecar)
      (test-call-with-function-replacements
       (list
        (list 'conversation--map-storage-records
              (lambda (&rest arguments)
                (incf scans)
                (apply original-map arguments)))
        (list 'sexp-store:log-write
              (lambda (pathname &rest arguments)
                (when (equal pathname sidecar) (incf publications))
                (apply original-write pathname arguments))))
       (lambda ()
         (let ((loaded (conversation-load identity)))
           (test-assert (equal (conversation-input-history loaded) '("retired" "active"))
                        "missing history rebuilds from archived and active inputs")
           (conversation-input-history loaded)
           (conversation-append-user-message loaded "next")
           (conversation-append-summary loaded "checkpoint")
           (test-assert
            (equal (conversation-input-history (conversation-load identity))
                   '("retired" "active" "next"))
            "the rebuilt sidecar supports later appends and reloads"))))
      (test-assert (and (= scans 1) (= publications 1))
                   "only the initial rebuild scans storage and publishes a complete file")))
  nil)

(-> test-conversation-input-history-append-failures () null)
(defun test-conversation-input-history-append-failures ()
  "Test failures before and after sidecar writes preserve durable inputs exactly once."
  (dolist (write-first-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (identity (conversation-pathname conversation))
             (sidecar (conversation-input-history-pathname identity))
             (original (symbol-function 'log-append)))
        (conversation-append-user-message conversation "first")
        (test-call-with-function-replacements
         (list (list 'log-append
                     (lambda (pathname &rest arguments)
                       (if (equal pathname sidecar)
                           (progn
                             (when write-first-p (apply original pathname arguments))
                             (error "Injected sidecar append failure."))
                           (apply original pathname arguments)))))
         (lambda () (conversation-append-user-message conversation "second")))
        (test-assert
         (equal (conversation-input-history (conversation-load identity)) '("first" "second"))
         "a durable input survives a failed sidecar append exactly once")
        (test-assert (equal (conversation-input-history conversation) '("first" "second"))
                     "the original heap also repairs without duplication")
        (test-assert (= (length (rest (log-read sidecar))) 2)
                     "repair leaves one sidecar entry per durable input"))))
  nil)

(-> test-conversation-input-history-rotation-failure () null)
(defun test-conversation-input-history-rotation-failure ()
  "Test compaction cannot retire an input whose sidecar append keeps failing."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity (conversation-pathname conversation))
           (sidecar (conversation-input-history-pathname identity))
           (original (symbol-function 'log-append)))
      (conversation-append-user-message conversation "first")
      (let ((active (conversation-log-pathname conversation)))
        (test-call-with-function-replacements
         (list (list 'log-append
                     (lambda (pathname &rest arguments)
                       (if (equal pathname sidecar)
                           (error "Injected sidecar append failure.")
                           (apply original pathname arguments)))))
         (lambda ()
           (conversation-append-user-message conversation "second")
           (let ((next (conversation-next-sequence conversation)))
             (test-assert
              (handler-case
                  (progn (conversation-append-summary conversation "checkpoint") nil)
                (conversation-invariant-error () t))
              "compaction reports a typed failure while history cannot be flushed")
             (test-assert
              (and (= (conversation-next-sequence conversation) next)
                   (equal (conversation-log-pathname conversation) active)
                   (= (length (conversation-storage-pathnames identity)) 1))
              "failed history publication leaves the active segment and sequence intact"))))
        (conversation-append-summary conversation "checkpoint")
        (test-assert
         (equal (conversation-input-history (conversation-load identity)) '("first" "second"))
         "compaction succeeds after repair and preserves both inputs"))))
  nil)

(-> test-conversation-input-history-recovery () null)
(defun test-conversation-input-history-recovery ()
  "Test torn tails, invalid indices and stale checkpoints repair from authoritative inputs."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity (conversation-pathname conversation))
           (sidecar (conversation-input-history-pathname identity)))
      (conversation-append-user-message conversation "retired")
      (conversation-append-summary conversation "checkpoint")
      (conversation-append-user-message conversation "active")
      (let ((forms (log-read sidecar)))
        (sexp-store:log-write sidecar (butlast forms))
        (with-open-file (stream sidecar :direction ':output :if-exists ':append)
          (write-string "(:input" stream))
        (test-assert
         (equal (conversation-input-history (conversation-load identity)) '("retired" "active"))
         "a torn input entry repairs its tail and catches up from the active segment")
        (multiple-value-bind (repaired incomplete-p) (log-read sidecar)
          (test-assert (and (not incomplete-p) (= (length repaired) 3))
                       "tail repair publishes two complete inputs"))
        (dolist (invalid (list (list (first forms) '(:input :seq 0 :source "invalid"))
                              (list (first forms))))
          (sexp-store:log-write sidecar invalid)
          (test-assert
           (equal (conversation-input-history (conversation-load identity)) '("retired" "active"))
           "invalid or checkpoint-stale indices rebuild the complete history")))
      (conversation-delete configuration (conversation-identifier conversation))
      (test-assert (not (probe-file sidecar))
                   "deleting the conversation removes its owned input history")))
  nil)


(-> test-conversation-input-history-interruption () null)
(defun test-conversation-input-history-interruption ()
  "Test nonlocal exits during publication and catch-up invalidate incomplete heap state."
  (dolist (operation '(:append :repair))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (identity (conversation-pathname conversation))
             (sidecar (conversation-input-history-pathname identity))
             (original (symbol-function 'log-append)))
        (conversation-append-user-message conversation "first")
        (when (eq operation ':repair)
          (test-call-with-function-replacements
           (list (list 'log-append
                       (lambda (pathname &rest arguments)
                         (if (equal pathname sidecar)
                             (error "Injected sidecar append failure.")
                             (apply original pathname arguments)))))
           (lambda ()
             (conversation-append-user-message conversation "second")
             (conversation-append-user-message conversation "third"))))
        (test-assert
         (eq (catch 'history-interrupted
               (test-call-with-function-replacements
                (list (list 'log-append
                            (lambda (pathname &rest arguments)
                              (apply original pathname arguments)
                              (when (equal pathname sidecar)
                                (throw 'history-interrupted ':interrupted)))))
                (lambda ()
                  (ecase operation
                    (:append (conversation-append-user-message conversation "second"))
                    (:repair (conversation-input-history conversation))))))
             ':interrupted)
         "a nonlocal exit interrupts sidecar publication after its durable write")
        (test-call-with-function-replacements
         (list (list 'conversation--map-storage-records
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "Interrupted history must repair without rebuilding."))))
         (lambda ()
           (conversation-append-user-message conversation "next")
           (let ((expected (if (eq operation ':append)
                               '("first" "second" "next")
                               '("first" "second" "third" "next"))))
             (test-assert (equal (conversation-input-history conversation) expected)
                          "later submission repairs every pending input exactly once")
             (test-assert
              (equal (conversation-input-history (conversation-load identity)) expected)
              "repaired history reloads without scanning retired records")))))))
  nil)
