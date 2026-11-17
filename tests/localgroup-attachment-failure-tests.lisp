(in-package #:autolith)

;;;; -- Attachment Failure Ownership --

(-> test-localgroup-stop-retry () null)
(defun test-localgroup-stop-retry ()
  "Attempt independent cleanup stages and retain endpoint ownership for retry."
  (dolist (failing-stage '(:watchdog :runtime :registry))
    (let* ((application (make-instance 'application))
           (session     (make-instance 'localgroup-session :application application))
           (cause       (make-condition 'simple-error :format-control "cleanup failed"))
           (attempts    nil)
           (fail-p      t))
      (setf (application-localgroup-session application) session)
      (flet ((attempt (stage)
               (push stage attempts)
               (when (and fail-p (eq stage failing-stage))
                 (error cause))))
        (test-call-with-function-replacements
         (list (list 'image-daemon:daemon-stop-thread
                     (lambda (thread) (declare (ignore thread)) (attempt ':watchdog)))
               (list 'image-daemon:daemon-runtime-stop
                     (lambda (runtime) (declare (ignore runtime)) (attempt ':runtime)))
               (list 'localgroup--delete-owned-registry
                     (lambda (runtime) (declare (ignore runtime)) (attempt ':registry))))
         (lambda ()
           (let ((failure (handler-case (localgroup-stop application)
                            (localgroup-stop-error (condition) condition))))
             (test-assert
              (and (typep failure 'localgroup-stop-error)
                   (eq session (application-localgroup-session application))
                   (equal (reverse attempts) '(:watchdog :runtime :registry))
                   (eq cause (second (first (localgroup-stop-error-failures failure)))))
              "cleanup preserves the original stage failure, attempts every stage, and retains ownership"))
           (setf fail-p nil attempts nil)
           (localgroup-stop application)
           (test-assert
            (and (null (application-localgroup-session application))
                 (equal (reverse attempts) '(:watchdog :runtime :registry)))
            "retry completes owned cleanup before releasing the application slot"))))))
  nil)

(defclass localgroup-failure-test-attachment (localgroup-attachment) ()
  (:documentation "An attachment borrowing a string stream for boundary tests."))

(defmethod image-daemon:attachment-release-transport
    ((attachment localgroup-failure-test-attachment))
  "Leave the borrowed string stream available after writer cleanup."
  (declare (ignore attachment))
  nil)

(-> test-localgroup-attachment-failure-boundary () null)
(defun test-localgroup-attachment-failure-boundary ()
  "Distinguish refused admission from failed committed attachment and retain causes."
  (dolist (scenario '(:second-controller :pre-handshake :replay :closing :stopping :cleanup))
    (let* ((application      (make-instance 'application))
           (session          (make-instance 'localgroup-session :application application
                                                               :identifier   "attachment-fixture"))
           (terminal         (image-daemon:relay-create))
           (output           (make-string-output-stream))
           (attachment       (make-instance 'localgroup-failure-test-attachment
                                            :stream output :mode ':control))
           (previous         (make-instance 'localgroup-failure-test-attachment
                                            :stream (make-string-output-stream) :mode ':control))
           (cause            (make-condition 'localgroup-error :message  "Injected attachment failure."
                                                              :operation ':attach))
           (cleanup-cause    (make-condition 'simple-error :format-control "cleanup failure"))
           (attach           (symbol-function 'image-daemon:relay-attach))
           (close            (symbol-function 'image-daemon:attachment-close))
           (finish           (symbol-function 'image-daemon:attachment-finish))
           (drain-unlocked-p t))
      (setf (application-input-controller application)
            (make-instance 'application-input-controller :application application
                                                         :main-thread (current-thread)))
      (when (eq scenario ':second-controller)
        (image-daemon:relay-attach terminal previous :rows 24 :columns 80))
      (setf (localgroup-session-closing-p session) (eq scenario ':closing)
            (image-daemon:daemon-runtime-stopping-p session) (eq scenario ':stopping))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'localgroup--valid-request-p (lambda (&rest ignored) (declare (ignore ignored)) t))
             (list 'localgroup--terminal (lambda (candidate) (declare (ignore candidate)) terminal))
             (list 'localgroup-attachment-create
                   (lambda (&rest ignored) (declare (ignore ignored)) attachment))
             (list 'application-input-controller-call-with-exclusive-input
                   (lambda (controller function) (declare (ignore controller)) (funcall function)))
             (list 'application-input-controller--open-prompt-if-ready
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'application-sync-window-title
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'localgroup--version-mismatch-notice
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'application-world-attached
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'localgroup--detach-live-region
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'localgroup--controller-lost
                   (lambda (&rest ignored) (declare (ignore ignored)) nil))
             (list 'application-safe-backtrace
                   (lambda ()
                     (if (eq scenario ':cleanup)
                         (error cleanup-cause)
                         '("original attachment frame"))))
             (list 'image-daemon:relay-attach
                   (lambda (&rest arguments)
                     (if (eq scenario ':pre-handshake)
                         (error cause)
                         (apply attach arguments))))
             (list 'localgroup-terminal-replay-screen
                   (lambda (&rest ignored)
                     (declare (ignore ignored))
                     (when (member scenario '(:replay :cleanup)) (error cause))))
             (list 'image-daemon:attachment-finish
                   (lambda (candidate packet)
                     (setf drain-unlocked-p
                           (and drain-unlocked-p
                                (not (sb-thread:holding-mutex-p
                                      (image-daemon:daemon-runtime-lock session)))
                                (not (sb-thread:holding-mutex-p
                                      (image-daemon:relay-lock terminal)))))
                     (funcall finish candidate packet)))
             (list 'image-daemon:attachment-close
                   (lambda (candidate)
                     (funcall close candidate)
                     (when (eq scenario ':cleanup) (error cleanup-cause)))))
            (lambda ()
              (localgroup--serve-attachment
               session '(:localgroup-request :arguments (:mode :control :rows 24 :columns 80))
               :stream output)
              (let* ((record  (localgroup-session-attachment-failure session))
                     (text    (get-output-stream-string output))
                     (packets (with-input-from-string (stream text)
                                (loop for packet = (daemon-read-packet stream)
                                      while packet collect packet))))
                (test-assert drain-unlocked-p
                             "bounded failure delivery and drain occur outside endpoint and relay locks")
                (if (member scenario '(:replay :cleanup))
                    (test-assert
                     (and (eq (first (first packets)) ':attached)
                          (eq (first (second packets)) ':exit)
                          (= (getf (rest (second packets)) :status) 1)
                          (equal (getf (getf (rest (second packets)) :details) :reason)
                                 "attachment-failure")
                          (eq (getf record :condition) cause)
                          (equal (getf record :stage) "replay"))
                     "queued success precedes terminal failure and preserves the original server cause")
                    (test-assert
                     (and (= (length packets) 1) (eq (first (first packets)) ':error)
                          (not (image-daemon:attachment-handshake-committed-p attachment))
                          (getf record :condition))
                     "a failed or closing admission delivers a refusal instead of EOF or queued success"))
                (when (eq scenario ':second-controller)
                  (test-assert
                   (and (eq (image-daemon:relay-controller terminal) previous)
                        (search "controlling terminal" (getf (rest (first packets)) :message)))
                   "second-controller refusal explains ownership and preserves the controller"))
                (when (eq scenario ':pre-handshake)
                  (test-assert
                   (and (eq (getf record :condition) cause)
                        (equal (getf record :backtrace) '("original attachment frame")))
                   "pre-handshake failure retains its exact cause and pre-cleanup backtrace"))
                (when (eq scenario ':cleanup)
                  (test-assert
                   (and (eq (getf record :condition) cause)
                        (null (getf record :backtrace))
                        (eq (second (first (getf record :cleanup-failures))) cleanup-cause))
                   "diagnostic and cleanup failures do not replace the attachment cause")))))
        (image-daemon:relay-detach terminal previous)
        (funcall close previous)
        (funcall close attachment))))
  nil)
