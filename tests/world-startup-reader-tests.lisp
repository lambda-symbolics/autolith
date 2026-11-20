(in-package #:autolith)

;;;; -- Initial Terminal Ownership --

(-> test-world-startup-reader () null)
(defun test-world-startup-reader ()
  "Keep boot input on the startup thread until the responsive reader is started."
  (with-test-configuration (configuration)
    (let* ((terminal    (make-instance 'queued-recording-terminal :columns 80 :rows 24))
           (ui          (terminal-ui-create :terminal terminal :fullscreen-p t))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation  (conversation-create configuration)
                                       :ui            ui))
           (controller  (application-input-controller-create
                         application :start-reader-p nil :load-pending-p nil))
           (session     nil))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (terminal-ui-start ui)
             (setf session (world-terminal-start application))
             (test-assert (not (application-input-controller-reader-live-p controller))
                          "publishing the endpoint does not start the boot thread's reader")
             (test-assert (not (application-input-controller-reader-paused-p controller))
                          "publishing the endpoint leaves no reader pause outstanding")
             (queued-recording-terminal-enqueue terminal ':submit)
             (let ((waits 0))
               (test-assert
                (eq (terminal-ui-boot-sequence
                     ui :duration 0 :linger-p t
                        :direct-input-p-function
                        (lambda ()
                          (not (application-input-controller-reader-live-p controller)))
                        :halted-function (lambda () (> waits 10))
                        :wait-function   (lambda (seconds)
                                           (declare (ignore seconds))
                                           (incf waits)))
                    ':start)
                "boot consumes an early Enter without a competing reader"))
             (application-input-controller--start-reader controller)
             (test-assert (application-input-controller-reader-live-p controller)
                          "the ready listener starts its reader explicitly")
             (queued-recording-terminal-enqueue terminal '(:insert "ready"))
             (test-assert (task-tests--wait-until
                           (lambda ()
                             (string= (line-editor-text (terminal-ui-editor ui)) "ready"))
                           2)
                          "the responsive reader handles input after boot")
             (test-assert
              (application-input-controller-call-with-exclusive-input
               controller
               (lambda ()
                 (not (application-input-controller-reader-live-p controller))))
              "modal work joins the started reader promptly")
             (test-assert (application-input-controller-reader-live-p controller)
                          "modal work resumes the ready reader"))
        (when session (world-terminal-stop application))
        (application-input-controller-stop controller)
        (terminal-ui-stop ui))))
  nil)
