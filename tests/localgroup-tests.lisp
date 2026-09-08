(in-package #:autolith)

;;;; -- Localgroup Tests --

(-> test-localgroup--application (configuration) (values application application-input-controller))
(defun test-localgroup--application (configuration)
  "Return a minimal APPLICATION and responsive controller for localgroup tests."
  (let* ((conversation (conversation-create configuration))
         (ui (terminal-ui-create
              :terminal (make-instance 'recording-terminal :columns 80)))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
                          :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (values application controller)))

(-> test-localgroup--read-packet (stream) list)
(defun test-localgroup--read-packet (stream)
  "Return one packet after a bounded wait for STREAM input."
  (test-assert
   (task-tests--wait-until (lambda () (listen stream)) 10)
   "the localgroup attachment produces its next packet promptly")
  (or (daemon-read-packet stream)
      (error "The localgroup attachment closed before its next packet.")))

(-> test-localgroup--attach
    (localgroup-session keyword)
    (values sb-bsd-sockets:socket stream list))

(defun test-localgroup--attach (session mode)
  "Open one test attachment to SESSION with MODE."
  (multiple-value-bind (socket stream)
      (daemon-connect (image-daemon:daemon-runtime-port session))
    (daemon-write-packet stream
                         (list :localgroup-request :version *daemon-protocol-version*
                               :token (image-daemon:daemon-runtime-token session)
                               :operation ':attach :arguments
                               (list :mode mode :rows 31 :columns 91 :styled-p nil)))
    (values socket stream (test-localgroup--read-packet stream))))

(defun test-localgroup-orphan-reconciliation ()
  "Verify server and client startup reconcile their configured registry root."
  (with-test-configuration (configuration root)
    (let ((application nil) (controller nil) (session nil) (directories nil))
      (unwind-protect
           (progn
             (multiple-value-setq (application controller)
               (test-localgroup--application configuration))
             (test-call-with-function-replacements
              (list (list 'image-daemon:daemon-registry-reconcile
                          (lambda (directory) (push directory directories) 0))
                    (list 'configuration-create
                          (lambda (&rest arguments) (declare (ignore arguments)) configuration)))
              (lambda ()
                (setf session (localgroup-start application))
                (test-assert (eq (localgroup--client-configuration) configuration)
                             "client startup uses its configured state")))
             (test-assert (and (= (length directories) 2)
                               (every (lambda (directory)
                                        (equal directory (localgroup-registry-directory configuration)))
                                      directories))
                          "both startup paths reconcile the configured discovery directory"))
        (when session (localgroup-stop application))
        (when controller (application-input-controller-stop controller))
        (when application (application-release-conversation-lease application)))))
  nil)

(-> test-localgroup-terminal-restart () null)

(defun test-localgroup-terminal-restart ()
  "Test that stopping a relay retains its direct terminal for restart."
  (let* ((direct
          (stream-terminal-create :input-stream (make-string-input-stream "")
                                  :output-stream (make-string-output-stream)
                                  :input-file-descriptor -1))
         (relay (localgroup-terminal-create direct)))
    (terminal-start relay)
    (test-assert (terminal-started-p direct) "a direct relay starts its direct terminal")
    (terminal-stop relay)
    (test-assert (eq (image-daemon:relay-direct-terminal relay) direct)
     "stopping a relay retains its direct transport")
    (test-assert (not (terminal-started-p direct))
     "stopping a relay stops its direct terminal")
    (terminal-start relay)
    (test-assert (terminal-started-p direct)
     "a stopped direct relay restarts its direct terminal")
    (terminal-stop relay))
  nil)

(-> test-localgroup-picker-waits-for-relayed-input () null)

(defun test-localgroup-picker-waits-for-relayed-input ()
  "Test a modal picker blocks until its controlling relay sends a fresh event."
  (let* ((terminal (localgroup-terminal-create))
         (ui (terminal-ui-create :terminal terminal))
         (attachment
          (make-instance 'image-daemon:attachment :socket nil :stream
                         (make-broadcast-stream) :mode ':control))
         (result ':pending)
         (picker-thread nil))
    (terminal-start terminal)
    (multiple-value-bind (attached-p released-p)
        (image-daemon:relay-attach terminal attachment :rows 24 :columns 80 :styled-p nil
         :session-id "picker-test")
      (declare (ignore released-p))
      (test-assert attached-p "the test relay accepts a controlling attachment"))
    (unwind-protect
        (progn
         (setf picker-thread
                 (make-thread
                  (lambda ()
                    (setf result
                            (terminal-ui-select ui :title "pick one" :items
                                                '((:name "default" :argument nil
                                                   :description "the default choice")))))
                  :name "Autolith relayed picker test"))
         (test-assert (task-tests--wait-until (lambda () (terminal-ui-selector ui)) 2)
          "the relayed picker opens before receiving input")
         (sleep 0.05)
         (test-assert (and (eq result ':pending) (thread-alive-p picker-thread))
          "an empty attached relay does not submit or cancel the picker")
         (image-daemon:relay-enqueue-event terminal attachment ':submit)
         (test-assert (task-tests--wait-until (lambda () (not (eq result ':pending))) 2)
          "a fresh relayed submit event completes the picker")
         (test-assert (string= result "default")
          "the fresh submit accepts the selected item"))
      (terminal-stop terminal)
      (when picker-thread (join-thread picker-thread))))
  nil)

(-> test-localgroup-remote-detach-never-pauses-reader () null)

(defun test-localgroup-remote-detach-never-pauses-reader ()
  "Test remote detach releases control without trying to join the input reader."
  (let* ((terminal (localgroup-terminal-create))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (session
          (make-instance 'localgroup-session :application application :identifier
                         "remote-detach" :token "token" :listener nil :port 1
                         :registry-pathname #P"remote-detach.sexp" :created-at
                         (local-time:now)))
         (attachment
          (make-instance 'image-daemon:attachment :socket nil :stream
                         (make-broadcast-stream) :mode ':control)))
    (multiple-value-bind (attached-p released-p)
        (image-daemon:relay-attach terminal attachment :rows 24 :columns 80 :styled-p nil
         :session-id "remote-detach")
      (declare (ignore released-p))
      (test-assert attached-p "the remote detach test attaches its controller"))
    (terminal-ui-start ui)
    (terminal-ui-set-status ui "working")
    (test-assert (plusp (terminal-ui-live-row-count ui))
     "the attached relay paints transient live rows")
    (test-call-with-function-replacements
     (list
      (list 'application-input-controller-call-with-reader-paused
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error "Remote detach tried to pause the responsive reader."))))
     (lambda ()
       (let ((result (localgroup--detach-terminal session)))
         (test-assert
          (and (not (getf (rest result) :scheduled-p))
               (eq (image-daemon:relay-attachment-kind terminal) ':detached))
          "remote detach releases the controlling client immediately")
         (test-assert (zerop (terminal-ui-live-row-count ui))
          "remote detach retracts the stale live region"))))
    (terminal-ui-stop ui)
    nil))


(-> test-localgroup-session-identifiers () null)

(defun test-localgroup-session-identifiers ()
  "Test canonical timestamp-bearing IDs and retained legacy discovery behavior."
  (let* ((timestamp (encode-universal-time 5 4 3 2 1 2025 0))
         (canonical (identifier-from-seed timestamp 0)))
    (let ((configuration (test-configuration)))
      (unwind-protect
          (progn
           (configuration-ensure-directories configuration)
           (let ((pathname (localgroup-registry-pathname configuration "abcdef012345")))
             (snapshot-write pathname
                             (list :localgroup-endpoint :version
                                   image-daemon:*daemon-registry-version* :session-id
                                   "abcdef012345" :pid 1 :address "127.0.0.1" :port 1
                                   :token "legacy-token" :created-at timestamp))
             (test-assert
              (string=
               (localgroup--record-session-id
                (rest (localgroup--find-record configuration "ABCDEF012345")))
               "abcdef012345")
              "legacy hexadecimal identifiers remain discoverable through normalized input")))
        (test-assert
         (localgroup-handoff--record-p
          (list :localgroup-handoff :version *localgroup-handoff-version* :session-id
                "abcdef012345" :token "legacy-token" :created-at timestamp :mode ':detach
                :state ':pending :fresh-conversation-p nil :old-pid 1 :replacement-pid
                nil :conversation-id canonical :draft ""))
         "legacy session identifiers remain valid in detached handoff records")
        (uiop/filesystem:delete-directory-tree (test-configuration-root configuration)
                                               :validate t :if-does-not-exist
                                               ':ignore))))
  nil)

(-> test-localgroup-protocol () null)

(defun test-localgroup-protocol ()
  "Test bounded safe packets, private discovery, status, and control routing."
  (test-localgroup-session-identifiers)
  (let* ((timestamp (encode-universal-time 5 4 3 2 1 2025 0))
         (status
          (list :localgroup-status :session-id (identifier-from-seed timestamp 0) :pid
                41234 :state ':idle :created-at timestamp :conversation-display-id
                "n-ew1234" :queued-input-count 0 :steering-input-count 0 :task-live-count
                0 :cwd "/tmp/example"))
         (titled-status (append status (list :conversation-title "Named local session")))
         (plain-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status) :stream stream :styled-p nil
                                       :columns 100)))
         (styled-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status) :stream stream :styled-p t :columns
                                       100)))
         (title-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list titled-status) :stream stream :styled-p nil
                                       :columns 80)))
         (fallback-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list status) :stream stream :styled-p nil
                                       :columns 52)))
         (narrow-output
          (with-output-to-string (stream)
            (localgroup-print-statuses (list titled-status) :stream stream :styled-p nil
                                       :columns 24)))
         (plain-lines
          (remove "" (uiop/utility:split-string plain-output :separator '(#\Newline))
                  :test #'string=)))
    (test-assert
     (and (search "┌" plain-output)
          (search (identifier-display (getf (rest status) :session-id)) plain-output)
          (search "41234" plain-output) (search "/tmp/example" plain-output)
          (not (search (string #\Esc) plain-output)))
     "localgroup status renders a plain box-drawing table without ANSI controls")
    (test-assert
     (and (search "Named local session" title-output)
          (search "Named local session" narrow-output)
          (search "n-ew1234" fallback-output)
          (eq (localgroup--status-field-style titled-status ':conversation) ':plain)
          (eq (localgroup--status-field-style status ':conversation) ':code))
     "localgroup status shows titles at every width and falls back to coded IDs")
    (let ((table-top (third plain-lines))
          (table-middle (fifth plain-lines))
          (table-bottom (first (last plain-lines))))
      (test-assert
       (and (find #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL table-top)
            (not (find #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL table-top))
            (find #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL table-middle)
            (find #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL table-bottom))
       "localgroup table borders use top, interior, and bottom column junctions"))
    (test-assert
     (and (search (terminal-style-sequence ':brand) styled-output)
          (search (terminal-style-sequence ':success) styled-output))
     "localgroup status applies semantic ANSI styles only when requested")
    (test-assert
     (and (search "2025-01-02T03:04Z" styled-output)
          (every (lambda (line) (<= (text-cell-width line) 24))
                 (uiop/utility:split-string narrow-output :separator '(#\Newline))))
     "localgroup status exposes encoded start times and fits narrow terminals"))
  (let ((*standard-input* (make-string-input-stream ""))
        (*standard-output* (make-string-output-stream)))
    (test-call-with-function-replacements
     (list
      (list 'clinedi:terminal-capture-input-mode
            (lambda (terminal)
              (declare (ignore terminal))
              nil))
      (list 'daemon-connect
            (lambda (port)
              (declare (ignore port))
              (error "Noninteractive attach reached the network."))))
     (lambda ()
       (test-assert
        (handler-case
         (progn
          (localgroup-attach-record (test-configuration)
                                    (cons #P"noninteractive.sexp"
                                          (list :localgroup-endpoint :session-id "NONTTY"
                                                :port 1 :token "unused"))
                                    ':read-only)
          nil)
         (localgroup-error (condition)
          (and (eq (daemon-error-operation condition) ':attach)
               (search "interactive terminal" (autolith-error-message condition)))))
        "localgroup attach rejects noninteractive input before connecting"))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (controller nil)
         (session nil))
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (multiple-value-setq (application controller)
           (test-localgroup--application configuration))
         (conversation-append-user-message (application-conversation application)
                                           "named localgroup session")
         (setf session (localgroup-start application))
         (let* ((record-pathname (image-daemon:daemon-runtime-registry-pathname session))
                (record (image-daemon:daemon-registry-read record-pathname))
                (response
                 (daemon-call (image-daemon:daemon-runtime-port session)
                              (image-daemon:daemon-runtime-token session) ':status))
                (status (getf (rest response) :status)))
           (test-assert (image-daemon:daemon-registry-record-p record)
            "localgroup start publishes one valid private record")
           (test-assert
            (and (eq (first response) ':ok)
                 (identifier-p (image-daemon:daemon-runtime-identifier session))
                 (string= (image-daemon:daemon-runtime-identifier session)
                          (conversation-identifier
                           (application-conversation application)))
                 (string= (getf (rest status) :session-id)
                          (image-daemon:daemon-runtime-identifier session))
                 (string= (getf (rest status) :conversation-title)
                          "Named localgroup session")
                 (getf (rest status) :idle-p) (getf (rest status) :waiting-for-input-p)
                 (zerop (getf (rest status) :task-live-count)))
            "new localgroup endpoints publish their active conversation identity")
           (test-assert
            (eq
             (first
              (daemon-call (image-daemon:daemon-runtime-port session) "wrong-token"
                           ':status))
             ':error)
            "an invalid capability token receives no successful status"))
         (let ((identifier (image-daemon:daemon-runtime-identifier session))
               (token (image-daemon:daemon-runtime-token session))
               (created-at (image-daemon:daemon-runtime-created-at session)))
           (test-assert
            (eq
             (application-call-with-localgroup-quiesced application
                                                        (lambda ()
                                                          (and
                                                           (null
                                                            (application-localgroup-session
                                                             application))
                                                           ':quiesced)))
             ':quiesced)
            "checkpoint quiescence removes every localgroup runtime thread")
           (setf session (application-localgroup-session application))
           (test-assert
            (and (string= (image-daemon:daemon-runtime-identifier session) identifier)
                 (string= (image-daemon:daemon-runtime-token session) token)
                 (= (image-daemon:daemon-runtime-created-at session) created-at))
            "checkpoint quiescence preserves the active conversation identity"))
         (daemon-call (image-daemon:daemon-runtime-port session)
                      (image-daemon:daemon-runtime-token session) ':tell
                      (list :message "remote input"))
         (with-lock-held ((application-input-controller-lock controller))
           (test-assert
            (equal (application-input-controller--state controller :work-items)
                   (list (list ':message "remote input")))
            "localgroup tell uses the ordinary submitted-message queue"))
         (let ((status
                (getf
                 (rest
                  (daemon-call (image-daemon:daemon-runtime-port session)
                               (image-daemon:daemon-runtime-token session) ':status))
                 :status)))
           (test-assert
            (and (not (getf (rest status) :idle-p))
                 (= (getf (rest status) :queued-input-count) 1))
            "queued remote input makes strict idle false"))
         (daemon-call (image-daemon:daemon-runtime-port session)
                      (image-daemon:daemon-runtime-token session) ':pause)
         (test-assert (application-localgroup-paused-p application)
          "localgroup pause holds queued primary work")
         (daemon-call (image-daemon:daemon-runtime-port session)
                      (image-daemon:daemon-runtime-token session) ':tell
                      (list :message "resume input"))
         (test-assert (not (application-localgroup-paused-p application))
          "new localgroup input resumes a paused session")
         (daemon-call (image-daemon:daemon-runtime-port session)
                      (image-daemon:daemon-runtime-token session) ':kill)
         (test-assert (application-input-controller-stopping-p controller)
          "localgroup kill requests ordinary graceful shutdown"))
      (when application (localgroup-stop application))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(-> test-localgroup-detached-terminal-lifecycle () null)

(defun test-localgroup-detached-terminal-lifecycle ()
  "Test detached prompt handoff and bounded shutdown with idle clients."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (localgroup-terminal-create))
         (conversation (conversation-create configuration))
         (ui (terminal-ui-create :terminal terminal))
         (application
          (make-instance 'application :configuration configuration :conversation
                         conversation :ui ui))
         (controller
          (make-instance 'application-input-controller :application application
                         :main-thread (current-thread)))
         (session nil)
         (socket nil)
         (stream nil)
         (stop-thread nil)
         (wedged-thread nil)
         (stopped-p nil)
         (stop-failure nil))
    (setf (application-input-controller application) controller)
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (terminal-ui-start ui)
         (test-assert
          (not (application-input-controller--open-prompt-if-ready controller))
          "a detached terminal cannot draw its prompt before control attaches")
         (setf session (localgroup-start application))
         (multiple-value-bind (attached-socket attached-stream response)
             (test-localgroup--attach session ':control)
           (setf socket attached-socket
                 stream attached-stream)
           (test-assert (eq (first response) ':attached)
            "a detached terminal accepts its first controlling attachment")
           (test-assert
            (task-tests--wait-until
             (lambda () (eq (terminal-ui-prompt-marker-state ui) ':input)) 2)
            "control attachment draws the initial prompt without a keypress")
           (setf wedged-thread
                   (make-thread (lambda () (loop (sleep 60))) :name
                                "Autolith wedged localgroup test client"))
           (with-lock-held ((image-daemon:daemon-runtime-lock session))
             (push wedged-thread (image-daemon:daemon-runtime-client-threads session)))
           (setf stop-thread
                   (make-thread
                    (lambda ()
                      (handler-case (localgroup-stop application)
                                    (error (condition) (setf stop-failure condition)))
                      (setf stopped-p t))
                    :name "Autolith localgroup stop test"))
           (test-assert (task-tests--wait-until (lambda () stopped-p) 3)
            "localgroup shutdown never waits indefinitely for idle clients")
           (test-assert
            (and (null stop-failure) (not (thread-alive-p wedged-thread))
                 (null (application-localgroup-session application))
                 (not
                  (probe-file (image-daemon:daemon-runtime-registry-pathname session))))
            "bounded shutdown reaps clients and unpublishes the session")))
      (when stream (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (image-daemon:daemon-stop-thread stop-thread)
      (image-daemon:daemon-stop-thread wedged-thread)
      (when (application-localgroup-session application) (localgroup-stop application))
      (application-input-controller-stop controller)
      (ignore-errors (terminal-ui-stop ui))
      (application-release-conversation-lease application)
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)

(defun test-localgroup-attachments ()
  "Test read-only observation and controlling terminal handoff over the endpoint."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (output (make-string-output-stream))
         (direct
          (stream-terminal-create :input-stream (make-string-input-stream "")
                                  :output-stream output :input-file-descriptor 0 :rows 24
                                  :columns 80))
         (relay (localgroup-terminal-create direct))
         (conversation (conversation-create configuration))
         (ui (terminal-ui-create :terminal relay))
         (application
          (make-instance 'application :configuration configuration :conversation
                         conversation :ui ui))
         (controller
          (make-instance 'application-input-controller :application application
                         :main-thread (current-thread)))
         (session nil)
         (socket nil)
         (stream nil))
    (setf (application-input-controller application) controller)
    (unwind-protect
        (progn
         (configuration-ensure-directories configuration)
         (terminal-start relay)
         (terminal--write relay (format nil "before attachment~%"))
         (setf session (localgroup-start application))
         (multiple-value-bind (read-only-socket read-only-stream response)
             (test-localgroup--attach session ':read-only)
           (unwind-protect
               (progn
                (test-assert
                 (and (eq (first response) ':attached)
                      (search "before attachment" (getf (rest response) :history)))
                 "read-only attach receives bounded existing terminal output")
                (terminal--write relay (format nil "observer output~%"))
                (let ((packet (test-localgroup--read-packet read-only-stream)))
                  (test-assert
                   (and (eq (first packet) ':output)
                        (string= (second packet) (format nil "observer output~%")))
                   "read-only attach receives live terminal output"))
                (daemon-write-packet read-only-stream
                                     (list :event (list :insert "ignored")))
                (sleep 0.05)
                (test-assert (string= (line-editor-text (terminal-ui-editor ui)) "")
                 "read-only attachment cannot inject terminal input")
                (daemon-write-packet read-only-stream '(:detach)))
             (ignore-errors (close read-only-stream))
             (ignore-errors (sb-bsd-sockets:socket-close read-only-socket))))
         (test-assert (image-daemon:relay-release-direct relay)
          "a detached process can release its original foreground terminal")
         (multiple-value-setq (socket stream)
           (multiple-value-bind (control-socket control-stream response)
               (test-localgroup--attach session ':control)
             (test-assert
              (and (eq (first response) ':attached)
                   (eq (image-daemon:relay-attachment-kind relay) ':remote)
                   (task-tests--wait-until
                    (lambda ()
                      (and (= (terminal-rows relay) 31) (= (terminal-columns relay) 91)))
                    2))
              "control attaches to a detached terminal relay")
             (values control-socket control-stream)))
         (daemon-write-packet stream (list :event (list :insert "remote")))
         (test-assert
          (task-tests--wait-until
           (lambda () (string= (line-editor-text (terminal-ui-editor ui)) "remote")) 2)
          "controlling attachment input reaches the ordinary line editor")
         (daemon-write-packet stream (list :event ':submit))
         (test-assert
          (task-tests--wait-until
           (lambda ()
             (with-lock-held ((application-input-controller-lock controller))
               (equal (application-input-controller--state controller :work-items)
                      (list (list ':message "remote")))))
           2)
          "controlling attachment submission uses the ordinary input queue")
         (daemon-write-packet stream (list :resize :rows 44 :columns 120 :styled-p t))
         (test-assert
          (task-tests--wait-until
           (lambda ()
             (and (= (terminal-rows relay) 44) (= (terminal-columns relay) 120)
                  (terminal-styled-p relay)))
           2)
          "controlling attachment resize updates the live terminal")
         (daemon-write-packet stream '(:detach))
         (test-assert
          (task-tests--wait-until
           (lambda () (eq (image-daemon:relay-attachment-kind relay) ':detached)) 2)
          "attachment detach leaves the application running without a terminal"))
      (when stream (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when session (localgroup-stop application))
      (application-input-controller-stop controller)
      (ignore-errors (terminal-stop relay))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  nil)
