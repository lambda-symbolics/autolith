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
   (task-tests--wait-until (lambda () (listen stream)) 2)
   "the localgroup attachment produces its next packet promptly")
  (or (daemon-read-packet stream)
      (error "The localgroup attachment closed before its next packet.")))

(-> test-localgroup--attach
    (localgroup-session keyword)
    (values sb-bsd-sockets:socket stream list))
(defun test-localgroup--attach (session mode)
  "Open one test attachment to SESSION with MODE."
  (multiple-value-bind (socket stream)
      (daemon-connect (localgroup-session-port session))
    (daemon-write-packet
     stream
     (list :localgroup-request
           :version *daemon-protocol-version*
           :token (localgroup-session-token session)
           :operation ':attach
           :arguments
           (list :mode mode :rows 31 :columns 91 :styled-p nil)))
    (values socket stream (test-localgroup--read-packet stream))))

(-> test-localgroup-terminal-restart () null)
(defun test-localgroup-terminal-restart ()
  "Test that stopping a relay retains its direct terminal for restart."
  (let* ((direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream (make-string-output-stream)
            :input-file-descriptor -1))
         (relay (localgroup-terminal-create direct)))
    (terminal-start relay)
    (test-assert (terminal-started-p direct)
                 "a direct relay starts its direct terminal")
    (terminal-stop relay)
    (test-assert
     (eq (localgroup-terminal-direct-terminal relay) direct)
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
           (make-instance 'localgroup-attachment
                          :socket nil
                          :stream (make-broadcast-stream)
                          :mode ':control))
         (result ':pending)
         (picker-thread nil))
    (terminal-start terminal)
    (multiple-value-bind (attached-p released-p)
        (localgroup-terminal-attach
         terminal attachment
         :rows 24 :columns 80 :styled-p nil :session-id "picker-test")
      (declare (ignore released-p))
      (test-assert attached-p "the test relay accepts a controlling attachment"))
    (unwind-protect
         (progn
           (setf picker-thread
                 (make-thread
                  (lambda ()
                    (setf result
                          (terminal-ui-select
                           ui
                           :title "pick one"
                            :items '((:name "default"
                                      :argument nil
                                      :description "the default choice")))))
                  :name "Autolith relayed picker test"))
           (test-assert
            (task-tests--wait-until
             (lambda () (terminal-ui-selector ui)) 2)
            "the relayed picker opens before receiving input")
           (sleep 0.05)
           (test-assert
            (and (eq result ':pending) (thread-alive-p picker-thread))
            "an empty attached relay does not submit or cancel the picker")
           (localgroup-terminal-enqueue-event terminal attachment ':submit)
           (test-assert
            (task-tests--wait-until (lambda () (not (eq result ':pending))) 2)
            "a fresh relayed submit event completes the picker")
           (test-assert (string= result "default")
                        "the fresh submit accepts the selected item"))
      (terminal-stop terminal)
      (when picker-thread
        (join-thread picker-thread))))
  nil)

(-> test-localgroup-blocking-read-lifecycle () null)
(defun test-localgroup-blocking-read-lifecycle ()
  "Test relay ownership transitions wake or preserve a blocking semantic read."
  (labels ((attachment (mode)
             (make-instance 'localgroup-attachment
                            :socket nil
                            :stream (make-broadcast-stream)
                            :mode mode))

           (attach (terminal client)
             (multiple-value-bind (attached-p released-p)
                 (localgroup-terminal-attach
                  terminal client
                  :rows 24 :columns 80 :styled-p nil :session-id "read-test")
               (declare (ignore released-p))
               (test-assert attached-p "the relay accepts its test controller")))

           (run-ending-transition (transition)
             (let* ((terminal (localgroup-terminal-create))
                    (client (attachment ':control))
                    (result ':pending)
                    (reader nil))
               (terminal-start terminal)
               (attach terminal client)
               (unwind-protect
                    (progn
                      (setf reader
                            (make-thread
                             (lambda ()
                               (setf result (terminal-read-event terminal)))
                             :name "Autolith relay lifecycle read test"))
                      (sleep 0.05)
                      (test-assert
                       (and (eq result ':pending) (thread-alive-p reader))
                       "the relay read blocks before ownership changes")
                      (ecase transition
                        (:stop
                         (terminal-stop terminal))
                        (:detach
                         (test-assert
                          (localgroup-terminal-detach terminal client)
                          "detaching reports released control"))
                        (:release
                         (localgroup-terminal-release-control terminal)))
                      (test-assert
                       (task-tests--wait-until
                        (lambda () (not (eq result ':pending))) 2)
                       "an ending ownership transition wakes the relay read")
                      (test-assert
                       (eq result ':stream-end)
                       "an ownership transition without a controller ends input"))
                 (terminal-stop terminal)
                 (localgroup-attachment-close client)
                 (when reader
                   (join-thread reader))))))
    (dolist (transition '(:stop :detach :release))
      (run-ending-transition transition))
    (let* ((terminal (localgroup-terminal-create))
           (first-client (attachment ':control))
           (next-client (attachment ':take-over))
           (result ':pending)
           (reader nil))
      (terminal-start terminal)
      (attach terminal first-client)
      (unwind-protect
           (progn
             (setf reader
                   (make-thread
                    (lambda ()
                      (setf result (terminal-read-event terminal)))
                    :name "Autolith relay takeover read test"))
             (sleep 0.05)
             (attach terminal next-client)
             (sleep 0.05)
             (test-assert
              (and (eq result ':pending) (thread-alive-p reader))
              "controller takeover keeps the empty relay read blocked")
             (test-assert
              (localgroup-terminal-enqueue-event
               terminal next-client ':replacement-event)
              "the replacement controller can queue input")
             (test-assert
              (task-tests--wait-until
               (lambda () (not (eq result ':pending))) 2)
              "replacement controller input wakes the relay read")
             (test-assert
              (eq result ':replacement-event)
              "the relay read returns replacement controller input"))
        (terminal-stop terminal)
        (localgroup-attachment-close first-client)
        (localgroup-attachment-close next-client)
        (when reader
          (join-thread reader)))))
  nil)


(-> test-localgroup-session-identifiers () null)
(defun test-localgroup-session-identifiers ()
  "Test canonical timestamp-bearing IDs and retained legacy discovery behavior."
  (let* ((timestamp (encode-universal-time 5 4 3 2 1 2025 0))
         (canonical (identifier-from-seed timestamp 0)))
    (let ((configuration (test-configuration)))
      (unwind-protect
           (progn
             (configuration-ensure-directories configuration)
             (let ((first nil)
                   (second nil)
                   (namespace
                     (namestring (localgroup-registry-directory configuration))))
               (unwind-protect
                    (let ((*random-index-function* (lambda (limit)
                                                      (declare (ignore limit))
                                                      0)))
                      (setf first
                            (localgroup-session-identifier-generate
                             configuration timestamp)
                            second
                            (localgroup-session-identifier-generate
                             configuration timestamp))
                      (test-assert
                       (and (identifier-p first)
                            (identifier-p second)
                            (not (string= first second))
                            (= (session-identifier-timestamp first)
                               timestamp))
                       "new localgroup process sessions receive unique timestamp-bearing identifiers"))
                 (when first
                   (idsmall:identifier-release first :namespace namespace))
                 (when second
                   (idsmall:identifier-release second :namespace namespace))))
             (let ((pathname
                     (localgroup-registry-pathname configuration "abcdef012345")))
               (snapshot-write
                pathname
                (list :localgroup-endpoint
                      :version *localgroup-registry-version*
                      :session-id "abcdef012345"
                      :pid 1
                      :address "127.0.0.1"
                      :port 1
                      :token "legacy-token"
                      :created-at timestamp))
               (test-assert
                (string=
                 (localgroup--record-session-id
                  (rest
                   (localgroup--find-record configuration "ABCDEF012345")))
                 "abcdef012345")
                "legacy hexadecimal identifiers remain discoverable through normalized input")))
               (test-assert
                (localgroup-handoff--record-p
                 (list :localgroup-handoff
                       :version *localgroup-handoff-version*
                       :session-id "abcdef012345"
                       :token "legacy-token"
                       :created-at timestamp
                       :mode ':detach
                       :state ':pending
                       :fresh-conversation-p nil
                       :old-pid 1
                       :replacement-pid nil
                       :conversation-id canonical
                       :draft ""))
                "legacy session identifiers remain valid in detached handoff records")
        (uiop:delete-directory-tree (test-configuration-root configuration)
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-localgroup-agent-session-identity () null)
(defun test-localgroup-agent-session-identity ()
  "Test localgroup startup propagates its identity to Relay turn metadata."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application nil)
         (agent nil)
         (session nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (setf application
                 (nth-value 0 (test-localgroup--application configuration)))
           (setf agent
                 (agent-create
                  :configuration configuration
                  :provider (make-instance 'model-provider)
                  :conversation (application-conversation application)
                  :tool-registry (make-instance 'tool-registry)
                  :worker nil)
                 (application-agent application) agent
                 session (localgroup-start application))
           (let ((session-id (localgroup-session-identifier session))
                 (metadata (observability-agent-turn-metadata agent)))
             (test-assert
              (and (string= (agent-session-id agent) session-id)
                   (string= (json-get metadata "session_id") session-id))
             "localgroup startup propagates its identity to observability turn metadata")))
      (when application
        (localgroup-stop application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-protocol () null)
(defun test-localgroup-protocol ()
  "Test bounded safe packets, private discovery, status, and control routing."
  (test-localgroup-session-identifiers)
  (let* ((timestamp (encode-universal-time 5 4 3 2 1 2025 0))
           (status
             (list :localgroup-status
                   :session-id (identifier-from-seed timestamp 0)
                   :pid 41234
                   :state ':idle
                   :created-at timestamp
                   :conversation-display-id "n-ew1234"
                   :queued-input-count 0
                   :steering-input-count 0
                   :task-live-count 0
                   :cwd "/tmp/example"))
           (titled-status
             (append status (list :conversation-title "Named local session")))
           (plain-output
             (with-output-to-string (stream)
               (localgroup-print-statuses
                (list status) :stream stream :styled-p nil :columns 100)))
           (styled-output
             (with-output-to-string (stream)
               (localgroup-print-statuses
                (list status) :stream stream :styled-p t :columns 100)))
           (title-output
             (with-output-to-string (stream)
               (localgroup-print-statuses
                (list titled-status) :stream stream :styled-p nil :columns 80)))
           (fallback-output
             (with-output-to-string (stream)
               (localgroup-print-statuses
                (list status) :stream stream :styled-p nil :columns 52)))
           (narrow-output
             (with-output-to-string (stream)
               (localgroup-print-statuses
                (list titled-status) :stream stream :styled-p nil :columns 24)))
           (plain-lines
             (remove ""
                     (uiop:split-string plain-output :separator '(#\Newline))
                     :test #'string=)))
      (test-assert
       (and (search "┌" plain-output)
            (search (identifier-display (getf (rest status) :session-id)) plain-output)
            (search "41234" plain-output)
            (search "/tmp/example" plain-output)
            (not (search (string #\Escape) plain-output)))
       "localgroup status renders a plain box-drawing table without ANSI controls")
      (test-assert
       (and (search "Named local session" title-output)
            (search "Named local session" narrow-output)
            (search "n-ew1234" fallback-output)
            (eq (localgroup--status-field-style titled-status ':conversation)
                ':plain)
            (eq (localgroup--status-field-style status ':conversation) ':code))
       "localgroup status shows titles at every width and falls back to coded IDs")
      (let ((table-top (third plain-lines))
            (table-middle (fifth plain-lines))
            (table-bottom (first (last plain-lines))))
        (test-assert
         (and (find #\Box_Drawings_Light_Down_And_Horizontal table-top)
              (not
               (find #\Box_Drawings_Light_Vertical_And_Horizontal table-top))
              (find #\Box_Drawings_Light_Vertical_And_Horizontal table-middle)
              (find #\Box_Drawings_Light_Up_And_Horizontal table-bottom))
         "localgroup table borders use top, interior, and bottom column junctions"))
      (test-assert
       (and (search (terminal-style-sequence ':brand) styled-output)
            (search (terminal-style-sequence ':success) styled-output))
       "localgroup status applies semantic ANSI styles only when requested")
      (test-assert
       (and (search "2025-01-02T03:04Z" styled-output)
            (every (lambda (line) (<= (text-cell-width line) 24))
                   (uiop:split-string narrow-output :separator '(#\Newline))))
       "localgroup status exposes encoded start times and fits narrow terminals"))
  (let ((*standard-input* (make-string-input-stream ""))
        (*standard-output* (make-string-output-stream)))
    (test-call-with-function-replacements
     (list
      (list 'terminal--interactive-file-descriptor-p
            (lambda (file-descriptor)
              (declare (ignore file-descriptor))
              nil))
      (list 'daemon-connect
            (lambda (port)
              (declare (ignore port))
              (error "Noninteractive attach reached the network."))))
     (lambda ()
       (test-assert
        (handler-case
            (progn
              (localgroup-attach-record
               (test-configuration)
               (cons #P"noninteractive.sexp"
                     (list :localgroup-endpoint
                           :session-id "NONTTY"
                           :port 1
                           :token "unused"))
               ':read-only)
              nil)
          (localgroup-error (condition)
            (and (eq (daemon-error-operation condition) ':attach)
                 (search "interactive terminal"
                         (autolith-error-message condition)))))
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
           (conversation-append-user-message
            (application-conversation application)
            "named localgroup session")
           (setf session (localgroup-start application))
           (let* ((record-pathname
                    (localgroup-session-registry-pathname session))
                  (record (localgroup--read-endpoint-record record-pathname))
                  (response
                    (daemon-call
                     (localgroup-session-port session)
                     (localgroup-session-token session)
                     ':status))
                  (status (getf (rest response) :status)))
               (test-assert (localgroup--endpoint-record-p record)
                            "localgroup start publishes one valid private record")
               (test-assert
                (and (eq (first response) ':ok)
                     (identifier-p (localgroup-session-identifier session))
                     (= (session-identifier-timestamp
                         (localgroup-session-identifier session))
                        (localgroup-session-created-at session))
                      (string= (getf (rest status) :session-id)
                               (localgroup-session-identifier session))
                      (string= (getf (rest status) :conversation-title)
                               "Named localgroup session")
                      (getf (rest status) :idle-p)
                      (getf (rest status) :waiting-for-input-p)
                      (zerop (getf (rest status) :task-live-count)))
                "new localgroup endpoints publish their canonical timestamp-bearing identity")
               (test-assert
                (eq
                 (first
                  (daemon-call
                   (localgroup-session-port session)
                   "wrong-token"
                   ':status))
                 ':error)
                "an invalid capability token receives no successful status"))
           (let ((identifier (localgroup-session-identifier session))
                 (token (localgroup-session-token session))
                 (created-at (localgroup-session-created-at session)))
             (test-assert
              (eq
               (application-call-with-localgroup-quiesced
                application
                (lambda ()
                  (and (null (application-localgroup-session application))
                       ':quiesced)))
               ':quiesced)
              "checkpoint quiescence removes every localgroup runtime thread")
             (setf session (application-localgroup-session application))
             (test-assert
              (and (string= (localgroup-session-identifier session) identifier)
                   (string= (localgroup-session-token session) token)
                   (= (localgroup-session-created-at session) created-at))
              "checkpoint quiescence preserves the process session identity"))
           (daemon-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':tell
            (list :message "remote input"))
           (with-lock-held ((application-input-controller-lock controller))
              (test-assert
               (equal (application-input-controller--state controller :work-items)
                      (list (list ':message "remote input")))
               "localgroup tell uses the ordinary submitted-message queue"))
           (let ((status
                   (getf
                    (rest
                     (daemon-call
                      (localgroup-session-port session)
                      (localgroup-session-token session)
                      ':status))
                    :status)))
             (test-assert
              (and (not (getf (rest status) :idle-p))
                   (= (getf (rest status) :queued-input-count) 1))
              "queued remote input makes strict idle false"))
           (daemon-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':pause)
           (test-assert (application-localgroup-paused-p application)
                        "localgroup pause holds queued primary work")
           (daemon-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':tell
            (list :message "resume input"))
           (test-assert (not (application-localgroup-paused-p application))
                        "new localgroup input resumes a paused session")
           (daemon-call
            (localgroup-session-port session)
            (localgroup-session-token session)
            ':kill)
           (test-assert (application-input-controller-stopping-p controller)
                        "localgroup kill requests ordinary graceful shutdown"))
      (when application
        (localgroup-stop application))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-detached-terminal-lifecycle () null)
(defun test-localgroup-detached-terminal-lifecycle ()
  "Test detached prompt handoff and bounded shutdown with idle clients."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (localgroup-terminal-create))
         (conversation  (conversation-create configuration))
         (ui            (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
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
             (test-assert
              (eq (first response) ':attached)
              "a detached terminal accepts its first controlling attachment")
             (test-assert
              (task-tests--wait-until
               (lambda ()
                 (eq (terminal-ui-prompt-marker-state ui) ':input))
               2)
              "control attachment draws the initial prompt without a keypress")
             (setf wedged-thread
                   (make-thread
                    (lambda ()
                      (loop (sleep 60)))
                    :name "Autolith wedged localgroup test client"))
             (with-lock-held ((localgroup-session-lock session))
               (push wedged-thread
                     (localgroup-session-client-threads session)))
             (setf stop-thread
                   (make-thread
                    (lambda ()
                      (handler-case
                          (localgroup-stop application)
                        (error (condition)
                          (setf stop-failure condition)))
                      (setf stopped-p t))
                    :name "Autolith localgroup stop test"))
             (test-assert
              (task-tests--wait-until (lambda () stopped-p) 3)
              "localgroup shutdown never waits indefinitely for idle clients")
             (test-assert
              (and (null stop-failure)
                   (not (thread-alive-p wedged-thread))
                   (null (application-localgroup-session application))
                   (not (probe-file
                         (localgroup-session-registry-pathname session))))
              "bounded shutdown reaps clients and unpublishes the session")))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (localgroup-stop-thread stop-thread)
      (localgroup-stop-thread wedged-thread)
      (when (application-localgroup-session application)
        (localgroup-stop application))
      (application-input-controller-stop controller)
      (ignore-errors (terminal-ui-stop ui))
      (application-release-conversation-lease application)
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-localgroup-attachments () null)
(defun test-localgroup-attachments ()
  "Test read-only observation and controlling terminal handoff over the endpoint."
  (let* ((terminal (localgroup-terminal-create))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type ':stream
                                :protocol ':tcp))
         (stream (make-string-output-stream))
         (attachment
           (make-instance 'localgroup-attachment
                          :socket socket
                          :stream stream
                          :mode ':read-only)))
    (unwind-protect
         (let ((*localgroup-terminal-output-chunk-character-limit* 4)
               (*localgroup-terminal-history-character-limit* 5)
               (text (format nil "abcdefghij~%")))
           (test-assert
            (localgroup-terminal-attach
             terminal attachment
             :rows 24
             :columns 80
             :styled-p nil
             :session-id "ORDER")
            "localgroup terminal accepts a read-only attachment")
           (terminal--write terminal text)
            (let* ((frames
                     (with-lock-held ((localgroup-attachment-lock attachment))
                       (coerce
                        (deque->vector (localgroup-attachment-queue attachment))
                        'list)))
                   (packets
                     (mapcar (lambda (frame)
                               (daemon-read-packet
                                (make-string-input-stream frame)))
                             frames))
                   (handshake (first packets))
                   (output
                     (apply #'concatenate 'string
                            (mapcar #'second (rest packets)))))
              (test-assert
               (and (= (length frames) 4)
                    (eq (first handshake) ':attached)
                    (string= (getf (rest handshake) :session-id) "ORDER")
                    (every (lambda (packet) (eq (first packet) ':output))
                           (rest packets))
                    (string= output text)
                    (string= (localgroup-terminal-history-text terminal)
                             (subseq text (- (length text) 5))))
               "the handshake precedes bounded lossless output and exact replay"))
            (let ((*localgroup-attachment-queue-character-limit* 1))
              (test-assert
               (and (not (localgroup-attachment-send attachment '(:oversized)))
                    (localgroup-attachment-closed-p attachment)
                    (deque-empty-p (localgroup-attachment-queue attachment))
                    (zerop (deque-total-weight
                            (localgroup-attachment-queue attachment))))
               "attachment overflow closes the client and releases its queue")))
      (localgroup-terminal-detach terminal attachment)
      (localgroup-attachment-close attachment)
      (ignore-errors (sb-bsd-sockets:socket-close socket))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (output (make-string-output-stream))
         (direct
           (stream-terminal-create
            :input-stream (make-string-input-stream "")
            :output-stream output
            :input-file-descriptor 0
            :rows 24
            :columns 80))
         (relay (localgroup-terminal-create direct))
         (conversation (conversation-create configuration))
         (ui (terminal-ui-create :terminal relay))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller
           (make-instance 'application-input-controller
                          :application application
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
                          (search "before attachment"
                                  (getf (rest response) :history)))
                     "read-only attach receives bounded existing terminal output")
                    (terminal--write relay (format nil "observer output~%"))
                    (let ((packet
                            (test-localgroup--read-packet read-only-stream)))
                      (test-assert
                       (and (eq (first packet) ':output)
                            (string= (second packet) (format nil "observer output~%")))
                       "read-only attach receives live terminal output"))
                    (daemon-write-packet
                     read-only-stream (list :event (list :insert "ignored")))
                    (sleep 0.05)
                    (test-assert
                     (string= (line-editor-text (terminal-ui-editor ui)) "")
                     "read-only attachment cannot inject terminal input")
                    (daemon-write-packet read-only-stream '(:detach)))
               (ignore-errors (close read-only-stream))
               (ignore-errors
                 (sb-bsd-sockets:socket-close read-only-socket))))
           (test-assert
            (localgroup-terminal-release-direct relay)
            "a detached process can release its original foreground terminal")
           (multiple-value-setq (socket stream)
             (multiple-value-bind (control-socket control-stream response)
                 (test-localgroup--attach session ':control)
               (test-assert
                (and (eq (first response) ':attached)
                     (eq (localgroup-terminal-attachment-kind relay) ':remote)
                     ;; The attachment's dimensions apply asynchronously
                     ;; through the reader's TERMINAL-UI-RESIZE.
                     (task-tests--wait-until
                      (lambda ()
                        (and (= (terminal-rows relay) 31)
                             (= (terminal-columns relay) 91)))
                      2))
                "control attaches to a detached terminal relay")
               (values control-socket control-stream)))
           (daemon-write-packet stream (list :event (list :insert "remote")))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (string= (line-editor-text (terminal-ui-editor ui)) "remote"))
             2)
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
           (daemon-write-packet
            stream (list :resize :rows 44 :columns 120 :styled-p t))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (and (= (terminal-rows relay) 44)
                    (= (terminal-columns relay) 120)
                    (terminal-styled-p relay)))
             2)
            "controlling attachment resize updates the live terminal")
           (daemon-write-packet stream '(:detach))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (eq (localgroup-terminal-attachment-kind relay) ':detached))
             2)
            "attachment detach leaves the application running without a terminal"))
      (when stream
        (ignore-errors (close stream)))
      (when (and socket (null stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))
      (when session
        (localgroup-stop application))
      (application-input-controller-stop controller)
      (ignore-errors (terminal-stop relay))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)
