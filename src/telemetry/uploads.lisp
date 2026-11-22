(in-package #:autolith)

;;;; -- Background Uploads --

(defparameter *telemetry-upload-interval* 5
  "Seconds to batch available completed events while their owning runs remain active.")

(defparameter *telemetry-upload-byte-limit* 1048576
  "Maximum encoded UTF-8 request bytes accepted by Seismograph.")

(defparameter *telemetry-upload-span-limit* 512
  "Maximum spans in one Seismograph request.")

(-> telemetry--take-upload-body (telemetry-controller) (option string))
(defun telemetry--take-upload-body (controller)
  "Take a FIFO prefix bounded by encoded envelope bytes and span count under the lock."
  (let* ((queue       (telemetry-controller-queue controller))
         (empty-bytes (length (json-encode-utf8 (telemetry--envelope controller nil))))
         (bytes       empty-bytes)
         (count       0)
         (spans       nil))
    (loop while queue
          for span = (first queue)
          for size = (length (json-encode-utf8 span))
          for next-bytes = (+ bytes size (if spans 1 0))
          do (cond
               ((> (+ empty-bytes size) *telemetry-upload-byte-limit*)
                ;; Discard an individually impossible span without stranding later events.
                (setf queue (rest queue)))
               ((or (>= count *telemetry-upload-span-limit*)
                    (> next-bytes *telemetry-upload-byte-limit*))
                (return))
               (t
                (push span spans)
                (incf count)
                (setf bytes next-bytes queue (rest queue)))))
    (setf (telemetry-controller-queue controller) queue)
    (when queue (setf (telemetry-controller-upload-now-p controller) t))
    (when spans (json-encode (telemetry--envelope controller (nreverse spans))))))

(-> telemetry--upload-context (telemetry-controller) telemetry-run)
(defun telemetry--upload-context (controller)
  "Snapshot the consent epoch under CONTROLLER's lock without retaining an owning run."
  (telemetry--make-run :controller controller :epoch (telemetry-controller-epoch controller)
                       :active t :upload-p t))

(-> telemetry--upload-batch (telemetry-run) null)
(defun telemetry--upload-batch (context)
  "Take one bounded FIFO snapshot; discard upload failures without retrying it."
  (let* ((controller (telemetry-run-controller context))
         (request
           (bt:with-lock-held ((telemetry-controller-lock controller))
             (when (telemetry--live-p context)
               (when-let ((body (telemetry--take-upload-body controller)))
                 (list :upload (config :telemetry-endpoint (telemetry-controller-owner controller))
                       body))))))
    (when request
      ; mallet:suppress no-ignore-errors -- a failed upload is discarded without retrying it
      (ignore-errors (telemetry--process context request))))
  nil)

(-> telemetry--upload-loop (telemetry-run (option function)) null)
(defun telemetry--upload-loop (context transport)
  "Batch on one thread, waking early for root completion or consent retirement."
  (let ((controller                   (telemetry-run-controller context))
        (*telemetry-process-function* transport)
        (*telemetry-run*              nil))
    (unwind-protect
         ; mallet:suppress no-ignore-errors -- an uploader failure must never reach the agent; the queue is retained
         (ignore-errors
           (loop
             (bt:with-lock-held ((telemetry-controller-lock controller))
               (unless (and (telemetry--live-p context) (telemetry-controller-queue controller))
                 (when (eq (telemetry-controller-upload-thread controller) (bt:current-thread))
                   (telemetry--retire-uploader controller))
                 (return))
               (unless (telemetry-controller-upload-now-p controller)
                 (bt:condition-wait (telemetry-controller-upload-wakeup controller)
                                    (telemetry-controller-lock controller)
                                    :timeout *telemetry-upload-interval*))
               (unless (telemetry--live-p context) (return))
               (setf (telemetry-controller-upload-now-p controller) nil))
             (telemetry--upload-batch context)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (eq (telemetry-controller-upload-thread controller) (bt:current-thread))
          (telemetry--retire-uploader controller)))))
  nil)

(-> telemetry--schedule-upload (telemetry-controller) null)
(defun telemetry--schedule-upload (controller)
  "Start at most one uploader under CONTROLLER's lock; retain the queue if startup fails."
  (when (and (telemetry-controller-queue controller)
             (not (telemetry-controller-upload-thread controller)))
    (let ((context   (telemetry--upload-context controller))
          (transport *telemetry-process-function*)
          (interval  *telemetry-upload-interval*))
      (when (telemetry--live-p context)
        (handler-case
            (setf (telemetry-controller-upload-thread controller)
                  (bt:make-thread (lambda ()
                                    (let ((*telemetry-upload-interval* interval))
                                      (telemetry--upload-loop context transport)))
                                  :name "telemetry uploader"))
          (error () nil)))))
  nil)

(-> telemetry-flush (&optional (option telemetry-run)) null)
(defun telemetry-flush (&optional (run *telemetry-run*))
  "Request an immediate background upload without waiting for the telemetry server."
  (when run
    (let ((controller (telemetry-run-controller run)))
      (bt:with-lock-held ((telemetry-controller-lock controller))
        (when (telemetry--live-p run)
          (setf (telemetry-controller-upload-now-p controller) t)
          (telemetry--schedule-upload controller)
          (bt:condition-notify (telemetry-controller-upload-wakeup controller))))))
  nil)

(-> telemetry--retire-uploader (telemetry-controller) null)
(defun telemetry--retire-uploader (controller)
  "Retire the current uploader under the consent lock before joining it outside."
  (setf (telemetry-controller-retired-upload-threads controller)
        (remove-if-not #'bt:thread-alive-p (telemetry-controller-retired-upload-threads controller)))
  (when (telemetry-controller-upload-thread controller)
    (push (telemetry-controller-upload-thread controller)
          (telemetry-controller-retired-upload-threads controller))
    (setf (telemetry-controller-upload-thread controller) nil))
  (setf (telemetry-controller-upload-now-p controller) nil)
  (bt:condition-notify (telemetry-controller-upload-wakeup controller))
  nil)

(-> telemetry--join-uploader (telemetry-controller) null)
(defun telemetry--join-uploader (controller)
  "Join retired uploaders outside the consent lock, excluding a reentrant caller."
  (let ((threads (bt:with-lock-held ((telemetry-controller-lock controller))
                   (copy-list (telemetry-controller-retired-upload-threads controller)))))
    (dolist (thread threads)
      (unless (eq thread (bt:current-thread))
        ; mallet:suppress no-ignore-errors -- joining a retired uploader is best-effort cleanup
        (ignore-errors (bt:join-thread thread))))
    (bt:with-lock-held ((telemetry-controller-lock controller))
      (setf (telemetry-controller-retired-upload-threads controller)
            (set-difference (telemetry-controller-retired-upload-threads controller) threads :test #'eq))))
  nil)

(-> telemetry--drain-uploads (telemetry-controller) null)
(defun telemetry--drain-uploads (controller)
  "Close admissions and allow at most three seconds for clean-exit uploads."
  (let ((thread   nil)
        (deadline (+ (get-internal-real-time)
                     (* 3 internal-time-units-per-second))))
    (bt:with-lock-held ((telemetry-controller-lock controller))
      (setf (telemetry-controller-closing controller) t
            (telemetry-controller-upload-now-p controller) t)
      (telemetry--schedule-upload controller)
      (setf thread (telemetry-controller-upload-thread controller))
      (bt:condition-notify (telemetry-controller-upload-wakeup controller)))
    (when (and thread (not (eq thread (bt:current-thread))))
      (loop while (and (bt:thread-alive-p thread) (< (get-internal-real-time) deadline))
            do (sleep 0.005))))
  nil)
