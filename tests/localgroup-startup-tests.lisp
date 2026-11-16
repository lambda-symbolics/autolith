(in-package #:autolith)

;;;; -- Initial Terminal Ownership --

(-> test-localgroup-startup-reader () null)
(defun test-localgroup-startup-reader ()
  "Keep boot input on the startup thread until the responsive reader is started."
  (with-test-configuration (configuration)
    (let* ((terminal     (localgroup-terminal-create))
           (ui           (terminal-ui-create :terminal terminal :fullscreen-p t))
           (application  (make-instance 'application
                                        :configuration configuration
                                        :conversation  (conversation-create configuration)
                                        :ui            ui))
           (controller   (application-input-controller-create
                          application :start-reader-p nil :load-pending-p nil))
           (attached     (sb-thread:make-semaphore :name "Startup attachment completed"))
           (replay       (symbol-function 'localgroup-terminal-replay-screen))
           (session      nil)
           (socket       nil)
           (stream       nil))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (terminal-ui-start ui)
             (setf session (localgroup-start application))
             (test-call-with-function-replacements
              (list (list 'localgroup-terminal-replay-screen
                          (lambda (&rest arguments)
                            (prog1 (apply replay arguments)
                              (sb-thread:signal-semaphore attached)))))
              (lambda ()
                (multiple-value-bind (new-socket new-stream response)
                    (test-localgroup--attach session ':control)
                  (setf socket new-socket stream new-stream)
                  (test-assert (eq (first response) ':attached)
                               "the startup client controls the real endpoint")
                  (test-assert (sb-thread:wait-on-semaphore attached :timeout 2)
                               "attachment completes its input ownership callback")
                  (test-assert (not (application-input-controller-reader-live-p controller))
                               "initial attachment does not start the boot thread's reader")
                  (test-assert (not (application-input-controller-reader-paused-p controller))
                               "initial attachment leaves no reader pause outstanding"))))
             (daemon-write-packet stream '(:event :submit))
             (test-assert (task-tests--wait-until
                           (lambda () (terminal-input-ready-p terminal)) 2)
                          "the client's early Enter waits for boot input")
             (let ((waits 0))
               (test-assert
                (eq (terminal-ui-boot-sequence
                     ui :duration 0 :linger-p t
                        :direct-input-p-function
                        (lambda ()
                          (not (application-input-controller-reader-live-p controller)))
                        :halted-function (lambda () (> waits 10))
                        :wait-function (lambda (seconds)
                                         (declare (ignore seconds))
                                         (incf waits)))
                    ':start)
                "boot consumes the client's early Enter without a competing reader"))
             (application-input-controller--start-reader controller)
             (test-assert (application-input-controller-reader-live-p controller)
                          "the ready listener starts its reader explicitly")
             (daemon-write-packet stream '(:event (:insert "ready")))
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
        ; mallet:suppress no-ignore-errors -- the attachment may have closed during failed startup
        (when stream (ignore-errors (close stream)))
        ; mallet:suppress no-ignore-errors -- closing the stream may already have closed its socket
        (when socket (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (when session (localgroup-stop application))
        (application-input-controller-stop controller)
        (terminal-ui-stop ui))))
  nil)
