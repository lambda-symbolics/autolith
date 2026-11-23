(in-package #:autolith)

;;;; -- Supervised Operation Retirement --

(-> test-supervised-request-readerless-pipe-cancellation () null)
(defun test-supervised-request-readerless-pipe-cancellation ()
  "Finish cleanup after a naturally full Linux pipe loses its reader."
  (test-fixture-supervised-request-cancellation *platform* 0)
  nil)

(-> test-supervised-request-delayed-io-cancellation () null)
(defun test-supervised-request-delayed-io-cancellation ()
  "Cancel and join I/O even when its initial scheduling was delayed."
  (test-fixture-supervised-request-cancellation *platform* 0.5)
  nil)

(-> test-telemetry-concurrent-process-retirement () null)
(defun test-telemetry-concurrent-process-retirement ()
  "Concurrent revokers wait through native teardown and the owner's final I/O cleanup."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let* ((controller         *telemetry-controller*)
            (run                (telemetry--begin-run configuration))
            (stopping           (bt:make-semaphore))
            (release-stop       (bt:make-semaphore))
            (completing         (bt:make-semaphore))
            (release-completion (bt:make-semaphore))
            (retiring           (bt:make-semaphore))
            (retired            (bt:make-semaphore))
            (complete           (symbol-function 'telemetry--complete-process))
            (retire             (symbol-function 'telemetry--retire-process))
            (stops 0) (owner nil) (revokers nil))
       (test-call-with-function-replacements
        (list (list 'uiop:launch-program
                    (lambda (&rest arguments) (declare (ignore arguments)) ':fixture))
              (list 'supervised-process-exchange
                   (lambda (process request) (declare (ignore process request)) ':ready))
              (list 'supervised-process--close-streams
                    (lambda (process) (declare (ignore process)) nil))
              (list 'telemetry--stop-process
                    (lambda (process)
                      (declare (ignore process))
                      (incf stops)
                      (bt:signal-semaphore stopping)
                      (bt:wait-on-semaphore release-stop :timeout 5)))
              (list 'telemetry--complete-process
                    (lambda (context record)
                      (bt:signal-semaphore completing)
                      (bt:wait-on-semaphore release-completion :timeout 5)
                      (funcall complete context record)))
              (list 'telemetry--retire-process
                    (lambda (record)
                      (bt:signal-semaphore retiring)
                      (funcall retire record))))
        (lambda ()
          (unwind-protect
               (progn
                 (setf owner
                       (bt:make-thread
                        (lambda ()
                          (let ((*telemetry-process-function* nil))
                            (telemetry--process run '(:ping))))
                        :name "test telemetry cleanup owner"))
                 (test-assert (bt:wait-on-semaphore stopping :timeout 2)
                              "The owner is paused during native teardown.")
                 (loop repeat 2
                       do (push (bt:make-thread
                                 (lambda ()
                                   (telemetry-quiesce)
                                   (bt:signal-semaphore retired))
                                 :name "test concurrent telemetry revoker")
                                revokers))
                 (loop repeat 2
                       do (test-assert (bt:wait-on-semaphore retiring :timeout 2)
                                       "Each revoker finds the operation during owner cleanup."))
                 (test-assert (not (bt:wait-on-semaphore retired :timeout 0.05))
                              "Revocation waits for native teardown.")
                 (let ((lock (telemetry-controller-lock controller)))
                   (test-assert (when (bt:acquire-lock lock nil)
                                  (bt:release-lock lock)
                                  t)
                                "Native teardown never holds the controller lock."))
                 (bt:signal-semaphore release-stop)
                 (test-assert (bt:wait-on-semaphore completing :timeout 2)
                              "The owner reaches its final completion boundary.")
                 (test-assert (not (bt:wait-on-semaphore retired :timeout 0.05))
                              "All revokers also wait for the owner's I/O cleanup.")
                 (bt:signal-semaphore release-completion)
                 (loop repeat 2
                       do (test-assert (bt:wait-on-semaphore retired :timeout 2)
                                       "Every simultaneous revoker observes operation completion.")))
            (bt:signal-semaphore release-stop)
            (bt:signal-semaphore release-completion)
            (when owner (bt:join-thread owner))
            (dolist (thread revokers) (bt:join-thread thread)))
          (test-assert (= stops 1) "Concurrent retirement reaps the child exactly once.")
          (test-assert (null (telemetry-controller-processes controller))
                       "The registry is empty only after complete teardown."))))))
  nil)

(-> test-telemetry-process-admission-retirement () null)
(defun test-telemetry-process-admission-retirement ()
  "Revocation covers the gap between child launch and successful or failed I/O-thread creation."
  (dolist (fail-p '(nil t))
    (test-telemetry--call
     (lambda (configuration)
       (setf (config :telemetry-enabled-p configuration) t)
       (telemetry-attach configuration)
       (let* ((controller       *telemetry-controller*)
              (run              (telemetry--begin-run configuration))
              (creating         (bt:make-semaphore))
              (release-creation (bt:make-semaphore))
              (release-response (bt:make-semaphore))
              (revoking         (bt:make-semaphore))
              (retired          (bt:make-semaphore))
              (make-thread      (symbol-function 'bt:make-thread))
              (failure          (make-condition 'simple-error :format-control "Injected thread creation failure."))
              (stops 0) (closes 0) (closed-after-io-p nil)
              (owner nil) (revoker nil) (owner-error nil) (io-thread nil))
         (test-call-with-function-replacements
          (list (list 'uiop:launch-program
                      (lambda (&rest arguments) (declare (ignore arguments)) ':fixture))
                (list 'supervised-process-exchange
                     (lambda (process request)
                       (declare (ignore process request))
                       (bt:wait-on-semaphore release-response :timeout 5)
                       ':ready))
                (list 'supervised-process--close-streams
                      (lambda (process)
                        (declare (ignore process))
                        (incf closes)
                        (setf closed-after-io-p
                              (or (null io-thread) (not (bt:thread-alive-p io-thread))))))
                (list 'telemetry--stop-process
                      (lambda (process)
                        (declare (ignore process))
                        (incf stops)
                        (bt:signal-semaphore release-response)))
                (list 'bt:make-thread
                      (lambda (function &rest options)
                        (if (equal (getf options :name) "supervised subprocess I/O")
                            (progn
                              (bt:signal-semaphore creating)
                              (bt:wait-on-semaphore release-creation :timeout 5)
                              (when fail-p (error failure))
                              (setf io-thread (apply make-thread function options)))
                            (apply make-thread function options)))))
          (lambda ()
            (unwind-protect
                 (progn
                   (setf owner
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (let ((*telemetry-process-function* nil))
                                  (telemetry--process run '(:ping)))
                              (error (condition) (setf owner-error condition))))
                          :name "test telemetry admission owner"))
                   (test-assert (bt:wait-on-semaphore creating :timeout 2)
                                "A launched child is paused before I/O-thread creation.")
                   (setf revoker
                         (bt:make-thread
                          (lambda ()
                            (bt:signal-semaphore revoking)
                            (telemetry-quiesce)
                            (bt:signal-semaphore retired))
                          :name "test telemetry admission revoker"))
                   (test-assert (bt:wait-on-semaphore revoking :timeout 2)
                                "Revocation starts while admission is incomplete.")
                   (test-assert (not (bt:wait-on-semaphore retired :timeout 0.05))
                                "Revocation cannot miss a partially admitted child.")
                   (bt:signal-semaphore release-creation)
                   (test-assert (bt:wait-on-semaphore retired :timeout 2)
                                "Revocation completes after admission or thread-creation failure."))
              (bt:signal-semaphore release-creation)
              (bt:signal-semaphore release-response)
              (when owner (bt:join-thread owner))
              (when revoker (bt:join-thread revoker)))
            (test-assert (eq owner-error (when fail-p failure))
                         "Thread-creation failure propagates through complete cleanup.")
            (test-assert (= stops 1) "Even partial admission reaps the child exactly once.")
            (test-assert (and (= closes 1) closed-after-io-p)
                         "Protocol streams close once, after I/O has stopped.")
            (test-assert (or (null io-thread) (not (bt:thread-alive-p io-thread)))
                         "No I/O thread survives synchronous revocation.")
            (test-assert (null (telemetry-controller-processes controller))
                         "Successful and failed admissions both retire their registry entry.")))))))
  nil)
