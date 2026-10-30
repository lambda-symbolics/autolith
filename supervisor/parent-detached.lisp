;;;; -- Trusted Parent Local Sessions --

(in-package #:autolith-supervisor)

(defclass parent-detached-terminal (terminal image-daemon:relay) ()
  (:documentation "A parent-owned terminal relay with authenticated remote controllers."))

(defclass parent-detached-session (image-daemon:daemon-runtime)
  ((application :initarg :application :reader parent-detached-application
                :documentation "The parent that owns the isolated agent.")
   (terminal :initarg :terminal :reader parent-detached-terminal
             :documentation "The parent UI relay, never an agent terminal.")
   (submit :initarg :submit :reader parent-detached-submit
           :documentation "Trusted input admission callback for a complete line.")
   (status :initarg :status :reader parent-detached-status
           :documentation "Trusted public status callback.")
   (pause :initarg :pause :reader parent-detached-pause
          :documentation "Trusted callback that pauses automatic input admission.")
   (kill :initarg :kill :reader parent-detached-kill
         :documentation "Trusted callback that stops this parent session.")
   (detach :initarg :detach :reader parent-detached-detach
           :documentation "Trusted callback that releases a direct terminal.")
   (control-lock :initform      (bordeaux-threads:make-lock "parent local control")
                 :reader        parent-detached-control-lock
                 :documentation "Orders controller changes with the UI input owner."))
  (:documentation "An authenticated local endpoint that retains all private state in the parent."))

(serapeum/types:-> parent-detached-terminal-create (&optional (or null terminal)) parent-detached-terminal)
(defun parent-detached-terminal-create (&optional direct-terminal)
  "Wrap DIRECT-TERMINAL in a relay, or create an unattached terminal for a detached parent."
  (make-instance 'parent-detached-terminal :direct-terminal direct-terminal
                 :rows                                      (if direct-terminal (clinedi:terminal-rows direct-terminal) 24)
                 :columns                                   (if direct-terminal (clinedi:terminal-columns direct-terminal) 80)
                 :interactive-p                             (and direct-terminal (clinedi:terminal-interactive-p direct-terminal))
                 :styled-p                                  (and direct-terminal (clinedi:terminal-styled-p direct-terminal))))

(defmethod image-daemon:transport-resize ((terminal parent-detached-terminal) &key rows columns styled-p)
  "Publish a client resize for the UI owner to apply before its next repaint."
  ;; The relay calls this method with its own nonrecursive lock held.
  (setf (clinedi:terminal-interactive-p terminal) t
        (clinedi:terminal-styled-p terminal) (not (null styled-p)))
  (terminal-relayed-resize-publish rows columns)
  nil)

(defmethod image-daemon:daemon-runtime-error-response ((runtime parent-detached-session) condition)
  "Return a fixed error without printing private conditions or credentials."
  (declare (ignore runtime condition))
  '(:error :message "Parent local session request failed."))

(defmethod terminal-start ((terminal parent-detached-terminal))
  "Start a relayed parent terminal."
  (image-daemon:transport-start terminal))

(defmethod terminal-stop ((terminal parent-detached-terminal))
  "Stop a relayed parent terminal."
  (image-daemon:transport-stop terminal))

(defmethod terminal--write ((terminal parent-detached-terminal) text)
  "Write trusted UI output through the relay."
  (image-daemon:transport-write terminal text))

(defmethod terminal-flush ((terminal parent-detached-terminal))
  "Flush relayed output."
  (image-daemon:transport-flush terminal))

(defmethod terminal-input-ready-p ((terminal parent-detached-terminal))
  "Check whether direct or controlling-client input is ready."
  (image-daemon:transport-input-ready-p terminal))

(defmethod terminal-read-event ((terminal parent-detached-terminal))
  "Read a semantic editor event through the relay."
  (image-daemon:transport-read-event terminal))

(defmethod terminal-plain-output-p ((terminal parent-detached-terminal))
  "Use the direct terminal's output policy when one is present."
  (let ((direct (bordeaux-threads:with-lock-held ((image-daemon:relay-lock terminal))
                  (image-daemon:relay-direct-terminal terminal))))
    (and direct (terminal-plain-output-p direct) t)))

(serapeum/types:-> parent-detached-directory (parent-configuration) pathname)
(defun parent-detached-directory (configuration)
  "Return the private parent endpoint directory, excluded from every child scope."
  (merge-pathnames "localgroup/" (uiop:ensure-directory-pathname
                                  (parent-configuration-get configuration ':private-root))))

(serapeum/types:-> parent-detached-start
                  (parent-application parent-detached-terminal &key (:submit function)
                   (:status function) (:pause function) (:kill function) (:detach function))
                  parent-detached-session)
(defun parent-detached-start (application terminal &key submit status pause kill detach)
  "Publish a parent-owned controller after the UI supplies its ordered action callbacks."
  (unless (every #'functionp (list submit status pause kill detach))
    (error 'parent-configuration-error :reason ':invalid-local-controller))
  (let* ((directory (parent-detached-directory (parent-application-configuration application)))
         (runtime   (progn
                      (ensure-directories-exist directory)
                      (setf (ls-compat.posix:file-mode directory) #o700)
                      (image-daemon:daemon-runtime-create
                       :directory directory :identifier (parent-application-session-id application)
                       :class 'parent-detached-session :request-function #'parent-detached--request
                       :initargs (list :application application :terminal terminal :submit submit
                                       :status status :pause pause :kill kill :detach detach)))))
    (image-daemon:daemon-runtime-start runtime)
    runtime))

(serapeum/types:-> parent-detached-stop (parent-detached-session &key (:status integer)) null)
(defun parent-detached-stop (runtime &key (status 0))
  "Finish attachments before stopping and removing the owned private endpoint."
  (image-daemon:relay-finish (parent-detached-terminal runtime) :status status)
  (image-daemon:daemon-runtime-stop runtime)
  nil)

(serapeum/types:-> parent-detached--attach
                  (parent-detached-session list &key (:socket t) (:stream stream)) null)
(defun parent-detached--attach (runtime values &key socket stream)
  "Serve one authenticated editor controller or read-only observer."
  (let ((mode     (getf values :mode ':control))
        (rows (getf values :rows 24)) (columns (getf values :columns 80))
        (styled-p (getf values :styled-p))
        (terminal (parent-detached-terminal runtime)))
    (unless (and (member mode '(:control :take-over :read-only))
                 (typep rows '(integer 1 10000)) (typep columns '(integer 1 10000))
                 (typep styled-p 'boolean))
      (error 'parent-cli-error :reason ':invalid-attachment))
    (let ((attachment (image-daemon:attachment-create socket stream mode)))
      (unwind-protect
           (when (bordeaux-threads:with-lock-held ((parent-detached-control-lock runtime))
                   (when (eq mode ':take-over)
                     (funcall (parent-detached-detach runtime)))
                   (image-daemon:relay-attach
                    terminal attachment :rows rows :columns columns :styled-p styled-p
                    :session-id (image-daemon:daemon-runtime-identifier runtime)))
             (image-daemon:relay-read-attachment terminal attachment))
        (image-daemon:relay-detach terminal attachment)
        (image-daemon:attachment-close attachment))))
  nil)

(serapeum/types:-> parent-detached--request
                  (parent-detached-session list &key (:socket t) (:stream stream)) null)
(defun parent-detached--request (runtime request &key socket stream)
  "Handle fixed local operations after the daemon validates its private capability."
  (let* ((values     (rest request))
         (operation  (getf values :operation))
         (identifier (image-daemon:daemon-runtime-identifier runtime)))
    (if (eq operation ':attach)
        (parent-detached--attach runtime (getf values :arguments) :socket socket :stream stream)
        (image-daemon:daemon-write-packet
         stream
         (case operation
           (:status
            (list :ok         :session-id identifier
                  :pid        (getf (rest (image-daemon:daemon-runtime-record runtime)) :pid)
                  :attached-p (image-daemon:relay-attached-p (parent-detached-terminal runtime))
                  :observers  (image-daemon:relay-observer-count (parent-detached-terminal runtime))
                  :state      (funcall (parent-detached-status runtime))))
           (:tell
            (let ((message (getf (getf values :arguments) :message)))
              (unless (and (stringp message) (<= 1 (length message) 32768))
                (error 'parent-cli-error :reason ':invalid-local-message))
              (funcall (parent-detached-submit runtime) (copy-seq message))
              (list :ok :session-id identifier :operation ':tell)))
           (:pause
            (funcall (parent-detached-pause runtime))
            (list :ok :session-id identifier :operation ':pause))
           (:detach
            (bordeaux-threads:with-lock-held ((parent-detached-control-lock runtime))
              (funcall (parent-detached-detach runtime))
              (image-daemon:relay-release-control (parent-detached-terminal runtime)))
            (list :ok :session-id identifier :operation ':detach))
           (:kill
            (funcall (parent-detached-kill runtime))
            (list :ok :session-id identifier :operation ':kill))
           (otherwise
            (error 'parent-cli-error :reason ':unknown-local-operation))))))
  nil)

(serapeum/types:-> parent-detached--records (parent-configuration) list)
(defun parent-detached--records (configuration)
  "Read private endpoint records without exposing their authentication tokens."
  (let ((directory (parent-detached-directory configuration)))
    (when (uiop:directory-exists-p directory)
      (image-daemon:daemon-registry-discover directory))))

(serapeum/types:-> parent-detached--query (list keyword &optional list) list)
(defun parent-detached--query (entry operation &optional arguments)
  "Send one fixed operation using the private endpoint capability."
  (let ((record (rest (rest entry))))
    (image-daemon:daemon-call (getf record :port) (getf record :token) operation arguments)))

(serapeum/types:-> parent-detached--select (parent-configuration (or null string)) list)
(defun parent-detached--select (configuration identifier)
  "Select one reachable endpoint or require an unambiguous explicit identity."
  (let ((records (remove-if-not
                  (lambda (entry)
                    (and (or (null identifier)
                             (string= identifier (getf (rest (rest entry)) :session-id)))
                         (handler-case
                             (eq (first (parent-detached--query entry ':status)) ':ok)
                           (image-daemon:daemon-error () nil))))
                  (parent-detached--records configuration))))
    (unless (= 1 (length records))
      (error 'parent-cli-error :reason (if records ':ambiguous-local-session ':local-session-not-found)))
    (first records)))

(serapeum/types:-> parent-detached--client-attach
                  (list parent-cli-request &key (:input stream) (:output stream)) keyword)
(defun parent-detached--client-attach (entry request &key input output)
  "Attach the current full editor terminal to a parent relay over authenticated transport."
  (let* ((record   (rest (rest entry)))
         (mode     (cond ((parent-cli-option request ':read-only)
                          ':read-only)
                         ((parent-cli-option request ':take-over)
                          ':take-over)
                         (t
                          ':control)))
         (terminal (stream-terminal-create :input-stream input :output-stream output))
         (size     nil))
    (multiple-value-bind (socket stream) (image-daemon:daemon-connect (getf record :port))
      (unwind-protect
           (progn
             (multiple-value-bind (rows columns) (terminal-current-size)
               (setf size (list rows columns))
               (image-daemon:daemon-write-packet
                stream (list :localgroup-request :version image-daemon:*daemon-protocol-version*
                             :token (getf record :token) :operation ':attach
                             :arguments          (list :mode mode :rows rows :columns columns
                                                       :styled-p (terminal-environment-styling-p)))))
             (let ((response (image-daemon:daemon-read-response stream ':attach)))
               (unless (eq (first response) ':attached)
                 (error 'parent-cli-error :reason ':attachment-refused))
               (let ((history (getf (rest response) :history)))
                 (when history
                   (write-string history output)
                   (finish-output output))))
             (terminal-start terminal)
             (let ((exit (image-daemon:daemon-attach-client-run
                          stream :socket socket :mode mode :output-stream output
                          :input-ready-function (lambda () (terminal-input-ready-p terminal))
                          :read-event-function  (lambda () (terminal-read-event terminal))
                          :resize-function
                          (lambda ()
                            (multiple-value-bind (rows columns) (terminal-current-size)
                              (unless (equal size (list rows columns))
                                (setf size (list rows columns))
                                (list :rows rows :columns columns
                                      :styled-p (terminal-environment-styling-p))))))))
               (if (and exit (not (zerop (getf exit :status)))) ':failed ':completed)))
        (terminal-stop terminal)
        (sb-bsd-sockets:socket-close socket :abort t)))))

(serapeum/types:-> parent-detached-cli-dispatch
                  (parent-configuration parent-cli-request &key (:input stream) (:output stream))
                  (values boolean t))
(defun parent-detached-cli-dispatch (configuration request &key (input *standard-input*) (output *standard-output*))
  "Run localgroup status, attach, tell, pause, detach, or kill. Return handled-p and outcome."
  (if (not (eq (parent-cli-request-command request) ':localgroup))
      (values nil nil)
      (let* ((arguments (parent-cli-request-arguments request))
             (operation (or (first arguments) "status")))
        (if (string= operation "status")
            (dolist (entry (parent-detached--records configuration))
              (let ((status (handler-case (parent-detached--query entry ':status)
                              (image-daemon:daemon-error () nil))))
                (when status
                  (if (parent-cli-option request ':sexp)
                      (progn (write status :stream output :readably t) (terpri output))
                      (format output "~A ~A~%" (getf (rest status) :session-id)
                              (getf (rest status) :state))))))
            (let* ((tell-p     (string= operation "tell"))
                   (identifier (if tell-p (when (third arguments) (second arguments)) (second arguments)))
                   (entry      (parent-detached--select configuration identifier)))
              (when (string= operation "attach")
                (return-from parent-detached-cli-dispatch
                  (values t (parent-detached--client-attach entry request :input input :output output))))
              (let ((response
                      (parent-detached--query
                       entry (cond (tell-p
                                    ':tell) ((string= operation "pause")
                                             ':pause)
                                   ((string= operation "detach")
                                    ':detach) ((string= operation "kill")
                                               ':kill)
                                   (t
                                    (error 'parent-cli-error :reason ':unknown-local-operation)))
                       (when tell-p (list :message (or (third arguments) (second arguments)))))))
                (unless (eq (first response) ':ok)
                  (error 'parent-cli-error :reason ':local-operation-refused))
                (format output "~A ~A~%" operation (getf (rest response) :session-id)))))
        (finish-output output)
        (values t ':completed))))
