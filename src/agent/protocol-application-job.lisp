;;;; -- Parent-requested Contracted Jobs --

(in-package #:autolith)

(defclass agent-job-event-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((buffer :initform (make-string-output-stream) :reader agent-job-event-buffer
           :documentation "One complete event, flushed at the producer boundary.")
   (size :initform 0 :accessor agent-job-event-size :documentation "Buffered character count.")
   (publish :initarg :publish :reader agent-job-event-publish
            :documentation "Publish one complete serialized event."))
  (:documentation "Bounded event transport that does not split quoted newlines."))

(defmethod trivial-gray-streams:stream-write-char ((stream agent-job-event-stream) character)
  "Buffer one event character within the transport limit."
  (when (> (incf (agent-job-event-size stream)) (* 1024 1024))
    (error 'agent-protocol-error :reason ':job-event-limit))
  (write-char character (agent-job-event-buffer stream))
  character)

(defmethod trivial-gray-streams:stream-finish-output ((stream agent-job-event-stream))
  "Publish the complete buffered event at its producer boundary."
  (when (plusp (agent-job-event-size stream))
    (let ((text (get-output-stream-string (agent-job-event-buffer stream))))
      (setf (agent-job-event-size stream) 0)
      (funcall (agent-job-event-publish stream) text)))
  nil)

(defmethod trivial-gray-streams:stream-force-output ((stream agent-job-event-stream))
  "Flush one complete event through the same publication boundary."
  (trivial-gray-streams:stream-finish-output stream))

(-> agent-protocol-application-run-job (agent-protocol-application hash-table) hash-table)
(defun agent-protocol-application-run-job (application job-map)
  "Run a contracted job with the current approved application and no private initialization."
  (let* ((source  (gethash "source" job-map))
         (output  (gethash "output_path" job-map))
         (session (agent-protocol-application-session application))
         (client  (agent-protocol-driver-client (agent-protocol-session-driver session)))
         (events
           (make-instance
            'agent-job-event-stream
            :publish
            (lambda (text)
              (let ((fields (make-hash-table :test #'equal)))
                (setf (gethash "run_job_event" fields) text)
                (agent-protocol--send
                 client
                 (lambda ()
                   (agent-wire:make-envelope
                    :application-event
                    (agent-wire:make-application-event
                     :kind   ':presentation
                     :fields (autolith-supervisor::protocol-data->map fields)))))))))
         (result  (make-hash-table :test #'equal)))
    (unless (and (stringp source) (<= (length (utf8-string-to-octets source)) 1048576)
                 (stringp output) (plusp (length output)))
      (error 'agent-protocol-error :reason ':invalid-job-action))
    (uiop:with-temporary-file (:pathname input :stream stream :type "sexp"
                               :directory (uiop:pathname-directory-pathname (pathname output)))
      (write-string source stream)
      (finish-output stream)
      (setf (gethash "exit_code" result)
            (run-job-run
             input output (or (config ':permission-mode (application-configuration application)) ':sandboxed)
             :configuration (application-configuration application)
             :executor      (lambda (configuration request permission-mode)
                              (declare (ignore configuration))
                              (run-job-execute-application application request permission-mode))
             :events        (when (gethash "events" job-map) ':sexp)
             :event-output  events)))
    result))
