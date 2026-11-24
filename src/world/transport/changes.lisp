(in-package #:autolith)

;;;; -- World Change Feed --

(defclass world-change-feed ()
  ((lock :initform (make-lock "Autolith world change feed") :reader world-change-feed-lock
         :documentation "Serializes subscriptions, revision increments and snapshot publication.")
   (revision :initform 0 :accessor world-change-feed-revision :type (integer 0)
             :documentation "The latest world or hosted-state invalidation.")
   (closed-p :initform nil :accessor world-change-feed-closed-p :type boolean
             :documentation "Whether shutdown has permanently ended admission to this feed.")
   (clients :initform nil :accessor world-change-feed-clients :type list
            :documentation "Read-only notification transports, with bounded daemon-owned output queues.")
   (registration :initform nil :accessor world-change-feed-registration :type list
                 :documentation "The process-local change listener, removed at shutdown.")
   (snapshot-lock :initform (make-lock "Autolith world snapshots") :reader world-change-feed-snapshot-lock
                  :documentation "Serializes snapshot construction without blocking invalidations.")
   (snapshot :initform nil :accessor world-change-feed-snapshot :type list
             :documentation "The hosted snapshots shared by clients within one revision.")
   (snapshot-revision :initform nil :accessor world-change-feed-snapshot-revision :type (option integer)
                      :documentation "The revision represented by SNAPSHOT, or NIL before observation."))
  (:documentation "A world's reactive invalidations and shared runtime snapshot cache."))

(defclass world-change-transport (image-daemon:attachment) ()
  (:documentation "A read-only change feed over an endpoint-owned stream."))

(defmethod image-daemon:attachment-release-transport ((transport world-change-transport))
  "Shut down TRANSPORT and leave stream closure to the endpoint request owner."
  (image-daemon:attachment-shutdown transport))

(-> world-change-feed-start (world-change-feed configuration) null)
(defun world-change-feed-start (feed configuration)
  "Listen for CONFIGURATION's committed changes until FEED is closed."
  (setf (world-change-feed-registration feed)
        (world-change-listen configuration (lambda () (world-change-feed-publish feed))))
  nil)

(-> world-change-feed-publish (world-change-feed) null)
(defun world-change-feed-publish (feed)
  "Advance FEED once and queue an invalidation without computing runtime snapshots."
  (let ((retired nil))
    (with-lock-held ((world-change-feed-lock feed))
      (let ((event (list :world-changed :revision (incf (world-change-feed-revision feed)))))
        (dolist (client (world-change-feed-clients feed))
          (unless (eq (image-daemon:attachment-try-send client event) ':accepted)
            (push client retired)))
        (setf (world-change-feed-clients feed)
              (set-difference (world-change-feed-clients feed) retired :test #'eq))))
    (dolist (client retired)
      (image-daemon:attachment-shutdown client)))
  nil)

(-> world-change-feed-close (world-change-feed) null)
(defun world-change-feed-close (feed)
  "Remove FEED's listener and release every subscribed transport."
  (when-let ((registration (shiftf (world-change-feed-registration feed) nil)))
    (world-change-unlisten registration))
  (let ((clients (with-lock-held ((world-change-feed-lock feed))
                   (setf (world-change-feed-closed-p feed) t)
                   (shiftf (world-change-feed-clients feed) nil))))
    (dolist (client clients)
      (image-daemon:attachment-close client)))
  nil)

(-> world-change-feed-serve (world-change-feed &key (:socket t) (:stream stream) (:transport-lock t)) null)
(defun world-change-feed-serve (feed &key socket stream transport-lock)
  "Subscribe before the greeting, then serve read-only invalidations until disconnection."
  (let ((client (make-instance 'world-change-transport :socket socket :stream stream
                              :mode ':read-only :transport-lock transport-lock)))
    (unwind-protect
         (block nil
           (with-lock-held ((world-change-feed-lock feed))
             (when (world-change-feed-closed-p feed)
               (return))
             (image-daemon:attachment-send client
                                          (list :attached :revision (world-change-feed-revision feed)))
             (push client (world-change-feed-clients feed)))
           (image-daemon:attachment-start client)
           ;; A subscriber has no commands. Closing or writing ends its subscription.
           (daemon-read-packet stream))
      (with-lock-held ((world-change-feed-lock feed))
       (setf (world-change-feed-clients feed) (cl:delete client (world-change-feed-clients feed) :test #'eq)))
      (image-daemon:attachment-close client)))
  nil)

(-> world-change-feed-observe (world-change-feed function) list)
(defun world-change-feed-observe (feed function)
  "Return one shared snapshot per revision, computing FUNCTION outside the notification lock."
  (with-lock-held ((world-change-feed-snapshot-lock feed))
    (let ((revision (with-lock-held ((world-change-feed-lock feed))
                      (world-change-feed-revision feed))))
      (if (eql revision (world-change-feed-snapshot-revision feed))
          (world-change-feed-snapshot feed)
          (let ((snapshot (funcall function)))
            (with-lock-held ((world-change-feed-lock feed))
              (when (= revision (world-change-feed-revision feed))
                (setf (world-change-feed-snapshot feed) snapshot
                      (world-change-feed-snapshot-revision feed) revision)))
            snapshot)))))

(-> world-change--relay (configuration) null)
(defun world-change--relay (configuration)
  "Notify the serving process after a durable write, without turning a failed notice into a failed write."
  (handler-case
      (when-let ((entry (world-client-entry configuration)))
        (world-terminal-query-record entry ':changed))
    (autolith-error ()
      nil))
  nil)

(setf *world-change-relay-function* #'world-change--relay)
