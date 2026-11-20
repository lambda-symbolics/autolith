(in-package #:autolith)

;;;; -- Real Background Upload Lifecycle --

(-> test-telemetry-background-completion () null)
(defun test-telemetry-background-completion ()
  "A blocked or failing uploader cannot delay run completion or replace its values/errors."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((entered (bt:make-semaphore))
           (release (bt:make-semaphore))
           (thread  nil)
           (bodies  nil))
       (let ((*telemetry-process-function*
               (lambda (context request)
                 (declare (ignore context))
                 (push (third request) bodies)
                 (bt:signal-semaphore entered)
                 (bt:wait-on-semaphore release :timeout 5)
                 (error "Injected upload failure."))))
         (unwind-protect
              (progn
                (test-assert (equal (multiple-value-list
                                     (telemetry-call-with-run configuration (lambda () (values :one :two))))
                                   '(:one :two))
                             "Run values return before the blocked upload can complete.")
                (test-assert (bt:wait-on-semaphore entered :timeout 2) "The upload starts on its own thread.")
                (setf thread (telemetry-controller-upload-thread *telemetry-controller*))
                (test-assert (bt:thread-alive-p thread) "The run has returned while its upload is still blocked.")
                (test-assert (null (telemetry-controller-runs *telemetry-controller*))
                             "Finished runs are released before upload completion.")
                (let ((original (make-condition 'simple-error :format-control "Agent failure.")))
                  (test-assert (eq (handler-case
                                      (telemetry-call-with-run configuration (lambda () (error original)))
                                    (error (condition) condition))
                                   original)
                               "A blocked uploader cannot replace the agent's own error.")))
           (bt:signal-semaphore release)
           (bt:signal-semaphore release)
           (when thread (bt:join-thread thread)))
         (test-assert (= (length bodies) 2) "Each finalized run is attempted once despite upload failure.")
         (test-assert (null (telemetry-controller-queue *telemetry-controller*)) "Failures discard their batches."))))
   :background-uploads-p t)
  nil)

(-> test-telemetry-background-progress () null)
(defun test-telemetry-background-progress ()
  "An open goal uploads completed events periodically through one uploader."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((entered                     (bt:make-semaphore))
           (release                     (bt:make-semaphore))
           (bodies                      nil)
           (thread                      nil)
           (*telemetry-upload-interval* 0.01))
       (let ((*telemetry-process-function*
               (lambda (context request)
                 (declare (ignore context))
                 (push (third request) bodies)
                 (bt:signal-semaphore entered)
                 (bt:wait-on-semaphore release :timeout 5)
                 t)))
         (unwind-protect
              (telemetry-call-with-run
               configuration
               (lambda ()
                 (telemetry-note-tool :tool "shell.run" :outcome "success")
                 (test-assert (bt:wait-on-semaphore entered :timeout 2)
                              "A tool event uploads before its owning goal finishes.")
                 (setf thread (telemetry-controller-upload-thread *telemetry-controller*))
                 (test-assert (telemetry-run-active *telemetry-run*) "The owning run is still active.")
                 (test-assert (every (lambda (span)
                                      (equal (test-telemetry--attribute span "autolith.event.kind") "tool"))
                                    (coerce (test-telemetry--spans (first bodies)) 'list))
                              "An open goal does not invent a final outcome or root summary.")
                 (dotimes (index 10)
                   (declare (ignorable index))
                   (telemetry-note-tool :tool "shell.run" :outcome "success"))
                 (test-assert (eq thread (telemetry-controller-upload-thread *telemetry-controller*))
                              "An in-flight upload never creates another uploader.")))
           (bt:signal-semaphore release)
           (bt:signal-semaphore release)
           (when thread (bt:join-thread thread)))
         (let ((spans (loop for body in bodies append (coerce (test-telemetry--spans body) 'list))))
           (test-assert (= (count "run" spans :test #'equal
                                 :key               (lambda (span) (test-telemetry--attribute span "autolith.event.kind"))) 1)
                        "The final summary is exported exactly once after completion.")))))
   :background-uploads-p t)
  nil)

(-> test-telemetry-background-retirement () null)
(defun test-telemetry-background-retirement ()
  "Revocation, checkpointing and immediate shutdown kill an actual stalled upload child."
  (dolist (action '(:revoke :checkpoint :shutdown :replace-owner :clean-exit))
    (test-telemetry--call
     (lambda (configuration)
       (setf (config :telemetry-enabled-p configuration) t)
       (telemetry-attach configuration)
       (let ((original                     (symbol-function 'uiop:launch-program))
             (ready-path                   (merge-pathnames "upload-stall.ready" (config :cache-root configuration)))
             (started                      (bt:make-semaphore))
             (process                      nil)
             (thread                       nil)
             (*telemetry-process-function* nil))
         (test-call-with-function-replacements
          (list (list 'uiop:launch-program
                      (lambda (command &rest options)
                        (if (and (listp command) (member "--telemetry-worker" command :test #'equal))
                            (progn
                              (setf process
                                    (test-telemetry--launch-stalled-child original options ready-path))
                              (bt:signal-semaphore started)
                              process)
                            (apply original command options)))))
          (lambda ()
            (telemetry-call-with-run configuration (lambda () :done))
            (test-assert (bt:wait-on-semaphore started :timeout 2) "An actual upload child is running.")
            (test-assert (and (probe-file ready-path) (uiop:process-alive-p process))
                         (test-telemetry--stalled-child-diagnostic process ready-path))
            (let ((controller *telemetry-controller*)
                  (start      (get-internal-real-time)))
              (setf thread (telemetry-controller-upload-thread controller))
              (ecase action
                (:revoke
                 (setf (config :telemetry-enabled-p configuration) nil))
                (:checkpoint
                 (telemetry-call-with-quiescence (lambda () nil)))
                (:shutdown
                 (telemetry-shutdown :drain-p nil))
                (:replace-owner
                 (let ((application (make-instance 'application :configuration configuration)))
                   (setf (application-configuration application)
                         (configuration-copy configuration :telemetry-enabled-p nil)))
                 (test-assert (not (telemetry-controller-enabled *telemetry-controller*))
                              "Replacing the owner with an opted-out configuration cancels old uploads."))
                (:clean-exit
                 (telemetry-shutdown)))
              (let ((seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
                (test-assert (< seconds (if (eq action ':clean-exit) 5 2))
                             (format nil "~A retirement took ~,3F seconds instead of stopping promptly."
                                     action seconds)))
              (test-assert (not (uiop:process-alive-p process)) "Retirement kills and reaps the upload child.")
              (test-assert (not (bt:thread-alive-p thread)) "The uploader is joined before retirement returns.")
              (test-assert (and (null (telemetry-controller-queue controller))
                                (null (telemetry-controller-upload-thread controller))
                                (null (telemetry-controller-retired-upload-threads controller)))
                           "No upload data or retired threads survive the lifecycle boundary."))))))
     :background-uploads-p t))
  nil)

(-> test-telemetry-background-shutdown () null)
(defun test-telemetry-background-shutdown ()
  "Clean exit delivers a queued final summary before joining its background uploader."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((bodies     nil)
           (controller *telemetry-controller*)
           (entered    (bt:make-semaphore))
           (release    (bt:make-semaphore))
           (closer     nil))
       (let ((*telemetry-process-function*
               (lambda (context request)
                 (declare (ignore context))
                 (push (third request) bodies)
                 (bt:signal-semaphore entered)
                 (bt:wait-on-semaphore release :timeout 5)
                 t)))
         (unwind-protect
              (progn
                (telemetry-call-with-run configuration (lambda () :done))
                (test-assert (bt:wait-on-semaphore entered :timeout 2) "The final upload starts before exit.")
                (setf closer (bt:make-thread #'telemetry-shutdown :name "telemetry clean exit fixture"))
                (loop repeat 400
                      when (bt:with-lock-held ((telemetry-controller-lock controller))
                             (telemetry-controller-closing controller))
                        do (return)
                      do (sleep 0.005))
                (test-assert (telemetry-controller-closing controller) "Clean exit closes admissions before waiting.")
                (test-assert (null (telemetry--begin-run configuration))
                             "New runs cannot extend the clean-exit upload grace."))
           (bt:signal-semaphore release)
           (when closer (bt:join-thread closer))))
       (test-assert (= (length bodies) 1) "Clean exit drains the finalized root before stopping.")
       (test-assert (null (telemetry-controller-upload-thread controller)) "Clean exit joins the uploader.")))
   :background-uploads-p t)
  nil)

(-> test-telemetry-background-handoff () null)
(defun test-telemetry-background-handoff ()
  "Rapid successive completions cannot strand a queue or leave uploader threads in a checkpoint."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((uploaded (bt:make-semaphore))
           (roots    0))
       (let ((*telemetry-process-function*
               (lambda (context request)
                 (declare (ignore context))
                 (incf roots (length (test-telemetry--spans (third request))))
                 (bt:signal-semaphore uploaded)
                 t)))
         (dotimes (index 50)
           (declare (ignorable index))
           (telemetry-call-with-run configuration (lambda () :done))
           (test-assert (bt:wait-on-semaphore uploaded :timeout 2)
                        "Each completion wakes an existing or replacement uploader without losing work."))
         (telemetry-call-with-quiescence
          (lambda ()
            (test-assert (notany (lambda (thread) (equal (bt:thread-name thread) "telemetry uploader"))
                                (bt:all-threads))
                         "A checkpoint joins naturally retiring uploaders as well as active ones."))))
       (test-assert (= roots 50) "Every finalized root uploads exactly once.")))
   :background-uploads-p t)
  nil)

(-> test-telemetry-background-start-failure () null)
(defun test-telemetry-background-start-failure ()
  "Keep user results and queued events when the runtime cannot create an uploader."
  (test-telemetry--call
   (lambda (configuration)
     (setf (config :telemetry-enabled-p configuration) t)
     (telemetry-attach configuration)
     (let ((bodies   nil)
           (uploaded (bt:make-semaphore)))
       (let ((*telemetry-process-function*
               (lambda (context request)
                 (declare (ignore context))
                 (push (third request) bodies)
                 (bt:signal-semaphore uploaded)
                 t)))
         (test-assert
          (equal (multiple-value-list
                  (telemetry-call-with-run
                   configuration
                   (lambda ()
                     (test-call-with-function-replacements
                      (list (list 'bt:make-thread
                                  (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (error "Injected thread creation failure."))))
                      (lambda ()
                        (telemetry-note-tool :tool "shell.run" :outcome "success")))
                     (test-assert (null (telemetry-controller-upload-thread *telemetry-controller*))
                                  "A failed launch does not register an uploader.")
                     (test-assert (= (length (telemetry-controller-queue *telemetry-controller*)) 1)
                                  "The completed tool event is available for a later upload.")
                     (values :one :two))))
                 '(:one :two))
          "Uploader startup failure does not replace the user's return values.")
         (test-assert (bt:wait-on-semaphore uploaded :timeout 2)
                      "Run completion retries uploader startup.")
         (test-assert
          (equal (map 'list (lambda (span) (test-telemetry--attribute span "autolith.event.kind"))
                      (test-telemetry--spans (first bodies)))
                 '("tool" "run"))
          "The later upload contains both the queued event and final summary."))))
   :background-uploads-p t)
  nil)
