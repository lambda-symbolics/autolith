(in-package #:autolith)

;;;; -- World-Terminal Discovery Client --

(defparameter *world-terminal-client-cancelled-p* nil
  "The scoped interactive switch cancellation predicate, or NIL without a cancellation hook.")

(define-condition world-terminal-client-cancelled (world-terminal-error)
  ()
  (:documentation "The person cancelled an interactive channel switch."))

(-> world-terminal-client-check-cancellation () null)
(defun world-terminal-client-check-cancellation ()
  "Stop a cancelled interactive switch before another endpoint operation."
  (when (and *world-terminal-client-cancelled-p* (funcall *world-terminal-client-cancelled-p*))
    (error 'world-terminal-client-cancelled :operation ':switch :message "Channel switch cancelled."))
  nil)

(-> world-terminal--record-session-id (list) string)
(defun world-terminal--record-session-id (record)
  "Return RECORD's session identifier."
  (getf (rest record) :session-id))

(-> world-terminal--record-port (list) integer)
(defun world-terminal--record-port (record)
  "Return RECORD's loopback port."
  (getf (rest record) :port))

(-> world-terminal--record-token (list) string)
(defun world-terminal--record-token (record)
  "Return RECORD's private capability token."
  (getf (rest record) :token))

(-> world-terminal--remove-stale-record (pathname list) null)
(defun world-terminal--remove-stale-record (pathname expected-record)
  "Delete PATHNAME only when it still contains EXPECTED-RECORD."
  (image-daemon:daemon-registry-delete-matching pathname expected-record)
  nil)

(-> world-terminal--record-stale-p (list condition) boolean)
(defun world-terminal--record-stale-p (record condition)
  "Return true when CONDITION, a failed request to RECORD's endpoint, shows RECORD is stale.

The record is stale when its process is dead or nothing listens on its port.
A slow answer means the process is alive, so its record stays."
  (let ((pid (getf (rest record) :pid)))
    (or (and (typep pid '(integer 1 *))
             (eq (image-daemon:daemon-process-state pid) ':dead))
        (and (typep condition 'image-daemon:daemon-error)
             (eq (image-daemon:daemon-error-operation condition) ':connect)
             (not (typep (image-daemon:daemon-error-cause condition)
                         'sb-sys:deadline-timeout))))))

(-> world-terminal--query-with-progress (list keyword list) list)
(defun world-terminal--query-with-progress (record operation arguments)
  "Wait for OPERATION and its loading notifications without a response deadline."
  (multiple-value-bind (socket stream)
      (daemon-connect (world-terminal--record-port record))
    (declare (ignore socket))
    (unwind-protect
         (progn
           (daemon-write-packet
            stream (list :world-terminal-request :version image-daemon:*daemon-protocol-version*
                         :token                  (world-terminal--record-token record)
                         :operation operation :arguments arguments))
           (loop for packet = (world-terminal--read-response stream operation)
                 until (not (eq (first packet) ':progress))
                 do (let ((step        (getf (rest packet) :step))
                          (description (getf (rest packet) :description)))
                      (unless (and (typep step '(integer 1 6)) (stringp description))
                        (error 'world-terminal-error :message "Invalid loading notification."
                                                :operation    operation))
                      (channel-loading-report step description))
                 finally (return packet)))
      ; mallet:suppress no-ignore-errors -- closing the finished connection must not mask its response
      (ignore-errors (close stream)))))

(-> world-terminal-query-record
    (cons keyword &optional list)
    list)
(defun world-terminal-query-record (entry operation &optional arguments)
  "Perform OPERATION against endpoint ENTRY, removing its record when the failure shows it is stale."
  (let* ((pathname   (first entry))
         (record     (rest entry))
         (session-id (world-terminal--record-session-id record))
         (response
           (handler-case
                (if (getf arguments :progress-p)
                    (world-terminal--query-with-progress record operation arguments)
                    (world-terminal-call
                     (world-terminal--record-port record)
                     (world-terminal--record-token record)
                     operation
                     :arguments arguments))
             (error (condition)
               (when (world-terminal--record-stale-p record condition)
                 (world-terminal--remove-stale-record pathname record))
               (error 'world-terminal-error
                       :message   (format nil "World-Terminal session ~A is unavailable."
                                          (session-identifier-display
                                           session-id))
                      :operation  operation
                      :session-id session-id
                      :cause      condition)))))
    (unless (and (proper-list-p response)
                 (eq (first response) ':ok))
      (error 'world-terminal-error
             :message    (or (getf (rest response) :message)
                             "The world-terminal endpoint rejected the request.")
             :operation  operation
             :session-id session-id
             :retry-p    (and (proper-list-p response)
                              (not (null (getf (rest response) :retry-p))))))
    response))

(-> world-terminal-owner-reachability (pathname string (integer 1)) keyword)
(defun world-terminal-owner-reachability (pathname identifier owner-pid)
  "Query one matching owner record without pruning.

Only definite record absence yields :NO-REACHABLE-ENDPOINT. Errors and identity
mismatches yield :UNKNOWN; neither read nor transport creates or modifies records."
  (handler-case
      (let ((before (platform-path-status *platform* pathname)))
        (unless before
          (return-from world-terminal-owner-reachability ':no-reachable-endpoint))
        (with-open-stream (stream (platform-open-regular-file *platform* pathname))
          (unless (platform-file-status-same-object-p
                   before (platform-stream-status *platform* stream))
            (return-from world-terminal-owner-reachability ':unknown))
          (let ((record (image-daemon:daemon-registry-read-stream
                         stream :extension-keywords '(:checkpoint-reconnect-id))))
            (unless (and record
                         (string= (world-terminal--record-session-id record) identifier)
                         (eql (getf (rest record) :pid) owner-pid))
              (return-from world-terminal-owner-reachability ':unknown))
            (let* ((response (world-terminal-call (world-terminal--record-port record)
                                                  (world-terminal--record-token record) ':status))
                   (status   (and (proper-list-p response)
                                  (eq (first response) ':ok)
                                  (getf (rest response) :status))))
              (if (and (proper-list-p status)
                       (eq (first status) ':world-terminal-status)
                       (equal (getf (rest status) :session-id) identifier)
                       (eql (getf (rest status) :pid) owner-pid))
                  ':reachable
                  ':unknown)))))
    (error ()
      ':unknown)))

(-> world-terminal--find-record (configuration string) cons)
(defun world-terminal--find-record (configuration session-id)
  "Return CONFIGURATION's endpoint for a channel ID, fragment or resident's name.

Resolve world designators first, including @NAME, bare names and legacy
aliases. Retain endpoint-only identifier discovery for rescue sessions.
Ambiguous or parked targets select nothing."
  (let* ((records (world-terminal-endpoint-records configuration))
         (channel (handler-case
                      (world-channel-designated (world-load configuration) configuration
                                               session-id :error-p nil)
                    (world-error (condition)
                      (error 'world-terminal-error
                             :message    (autolith-error-message condition)
                             :operation  ':discover
                             :session-id session-id
                             :cause      condition))))
         (matches (if channel
                      (remove-if-not
                       (lambda (entry)
                         (string= (channel-identifier channel)
                                  (world-terminal--record-session-id (rest entry))))
                       records)
                      (identifier-match session-id records
                                        :key (lambda (entry)
                                               (world-terminal--record-session-id (rest entry)))))))
    (cond ((null matches)
           (error 'world-terminal-error
                  :message    (format nil "No hosted channel matches ~S." session-id)
                  :operation  ':discover
                  :session-id session-id))
          ((rest matches)
           (error 'world-terminal-error
                  :message
                  (format nil "World-Terminal session ID ~S is ambiguous. Matching sessions: ~{~A~^, ~}."
                          session-id
                          (sort (mapcar (lambda (entry)
                                          (session-identifier-display
                                           (world-terminal--record-session-id (rest entry))))
                                        matches)
                                #'string<))
                  :operation  ':discover
                  :session-id session-id))
          (t
           (first matches)))))

(-> world-terminal-statuses (configuration) list)
(defun world-terminal-statuses (configuration)
  "Return live world-terminal status snapshots, pruning unreachable records."
  (loop for entry in (world-terminal-endpoint-records configuration)
        for status =
          (handler-case
              (getf (rest (world-terminal-query-record entry ':status)) :status)
            (world-terminal-error () nil))
        when status
          collect status))

(-> world-terminal--status-state-text (list) string)
(defun world-terminal--status-state-text (status)
  "Return STATUS's concise state label."
  (string-downcase (symbol-name (getf (rest status) :state))))

(-> world-terminal--status-state-style (list) terminal-style)
(defun world-terminal--status-state-style (status)
  "Return the semantic terminal style for STATUS's current state."
  (case (getf (rest status) :state)
    (:idle
     ':success)
    (:failed
     ':failure)
    ((:paused :cancelling :detaching)
     ':notice)
    ((:active :working)
     ':brand)
    (otherwise
     ':dim)))

(-> world-terminal--status-activity-text (list) string)
(defun world-terminal--status-activity-text (status)
  "Return STATUS's compact queue and job activity summary."
  (format nil "q:~D s:~D jobs:~D"
          (getf (rest status) :queued-input-count)
          (getf (rest status) :steering-input-count)
          (getf (rest status) :job-live-count 0)))

(-> world-terminal--status-timestamp-text (timestamp) string)
(defun world-terminal--status-timestamp-text (timestamp)
  "Return TIMESTAMP as a compact UTC session-start label."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0DZ"
            year month date hour minute)))

(-> world-terminal--status-started-text (list) string)
(defun world-terminal--status-started-text (status)
  "Return STATUS's encoded or recorded session start time for display."
  (let* ((values     (rest status))
         (identifier (getf values :session-id))
         (timestamp
           (and (stringp identifier)
                (session-identifier-timestamp identifier))))
    (if timestamp
        (world-terminal--status-timestamp-text timestamp)
        (let ((created-at (getf values :created-at)))
          (if (typep created-at 'timestamp)
              (world-terminal--status-timestamp-text created-at)
              "unknown")))))

(-> world-terminal--status-field-label (keyword) string)
(defun world-terminal--status-field-label (field)
  "Return FIELD's compact human-readable table heading."
  (ecase field
    (:session
     "CHANNEL")
    (:pid
     "PID")
    (:state
     "STATE")
    (:started
     "STARTED")
    (:conversation
     "TITLE")
    (:activity
     "ACTIVITY")
    (:workspace
     "WORKSPACE")))

(-> world-terminal--status-field-text (list keyword) string)
(defun world-terminal--status-field-text (status field)
  "Return STATUS's display text for FIELD."
  (let ((values (rest status)))
    (ecase field
      (:session
       (session-identifier-display (getf values :session-id)))
      (:pid
       (let ((pid (getf values :terminal-client-pid)))
         (if (typep pid '(integer 1))
             (format nil "~D" pid)
             "-")))
      (:state
       (world-terminal--status-state-text status))
      (:started
       (world-terminal--status-started-text status))
      (:conversation
       (or (getf values :conversation-title)
           (getf values :conversation-display-id)))
      (:activity
       (world-terminal--status-activity-text status))
      (:workspace
       (getf values :cwd)))))

(-> world-terminal--status-field-style (list keyword) terminal-style)
(defun world-terminal--status-field-style (status field)
  "Return the semantic style for STATUS's FIELD."
  (case field
    (:session
     ':code)
    (:pid
     ':code)
    (:state
     (world-terminal--status-state-style status))
    (:started
     ':timestamp-time)
    (:conversation
     (if (getf (rest status) :conversation-title) ':plain ':code))
    (:activity
     ':dim)
    (:workspace
     ':plain)))

(-> world-terminal--status-table-fields (integer) (option list))
(defun world-terminal--status-table-fields (columns)
  "Return the table fields that fit within COLUMNS, or NIL for compact cards."
  (cond ((>= columns 100)
         '(:session :pid :state :started :conversation :activity :workspace))
        ((>= columns 76)
         '(:session :state :started :conversation :workspace))
        ((>= columns 52)
         '(:session :state :conversation :workspace))
        ((>= columns 36)
         '(:session :conversation :workspace))
        (t
         nil)))

(-> world-terminal--status-field-minimum-width (keyword) integer)
(defun world-terminal--status-field-minimum-width (field)
  "Return the minimum useful cell width for FIELD."
  (ecase field
    (:session
     8)
    (:pid
     5)
    (:state
     8)
    (:started
     17)
    (:conversation
     8)
    (:activity
     12)
    (:workspace
     14)))

(-> world-terminal--status-output-styled-p (stream) boolean)
(defun world-terminal--status-output-styled-p (stream)
  "Return true when STREAM is an interactive color-capable status destination."
  (not
   (null
    (and (interactive-stream-p stream)
         (terminal-environment-styling-p)))))

(-> world-terminal--status-output-columns (stream) integer)
(defun world-terminal--status-output-columns (stream)
  "Return a useful status-output width for STREAM."
  (if (interactive-stream-p stream)
      (nth-value 1 (terminal-current-size))
      *terminal-default-columns*))

(-> world-terminal--write-styled-line (stream terminal-styled-text boolean) null)
(defun world-terminal--write-styled-line (stream spans styled-p)
  "Write SPANS and one newline, emitting trusted styles only when STYLED-P."
  (dolist (span spans)
    (let ((sequence
            (and styled-p
                 (terminal-style-sequence (terminal-span-style span)))))
      (when sequence
        (write-string sequence stream))
      (write-string (terminal-span-text span) stream)
      (when sequence
        (write-string *terminal-style-reset* stream))))
  (terpri stream)
  nil)

(-> world-terminal--status-table-separator (list) terminal-styled-text)
(defun world-terminal--status-table-separator (widths)
  "Return an internal row separator aligned with cell WIDTHS."
  (list
   (terminal-span
    ':dim
    (with-output-to-string (stream)
      (loop for width in widths
            for remaining on widths
            do (write-string
                (make-string width
                             :initial-element #\Box_Drawings_Light_Horizontal)
                stream)
               (when (rest remaining)
                 (write-char #\Box_Drawings_Light_Vertical_And_Horizontal stream)))))))

(-> world-terminal--status-table-row
    (list list list &key (:header-p boolean))
    terminal-styled-text)
(defun world-terminal--status-table-row (status fields widths &key header-p)
  "Return one STATUS row with separators between FIELDS across WIDTHS.

HEADER-P renders field labels rather than status values."
  (loop for field in fields
        for width in widths
        for remaining on fields
        collect
        (terminal-span
         (if header-p ':strong (world-terminal--status-field-style status field))
         (termdown:fit-text
          (if header-p
              (world-terminal--status-field-label field)
              (world-terminal--status-field-text status field))
          width))
        when (rest remaining)
          collect (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))))

(-> world-terminal--status-card-row (list integer) terminal-styled-text)
(defun world-terminal--status-card-row (status columns)
  "Return compact rows for STATUS within COLUMNS cells."
  (let* ((identifier   (world-terminal--status-field-text status ':session))
         (state        (world-terminal--status-field-text status ':state))
         (session-width
          (min (text-cell-width identifier)
               (max 1 (- columns (text-cell-width state) 2))))
         (state-width  (max 0 (- columns session-width 2)))
         (detail
          (format nil "~A  ~A"
                  (world-terminal--status-field-text status ':started)
                  (world-terminal--status-field-text status ':activity)))
         (conversation (world-terminal--status-field-text status ':conversation)))
    (list (terminal-span ':code (termdown:fit-text identifier session-width))
          (terminal-span ':plain "  ")
          (terminal-span (world-terminal--status-state-style status)
                         (termdown:fit-text state state-width))
          (terminal-span ':plain (string #\Newline))
          (terminal-span ':timestamp-time (termdown:fit-text detail columns))
          (terminal-span ':plain (string #\Newline))
          (terminal-span (world-terminal--status-field-style status ':conversation)
                         (termdown:fit-text conversation columns))
          (terminal-span ':plain (string #\Newline))
          (terminal-span ':plain
                         (termdown:fit-text
                          (world-terminal--status-field-text status ':workspace)
                          columns)))))

(-> world-terminal-print-statuses
    (list &key (:stream stream) (:styled-p boolean) (:columns integer))
    null)
(defun world-terminal-print-statuses
    (statuses &key
                (stream *standard-output*)
                (styled-p (world-terminal--status-output-styled-p stream))
                (columns (world-terminal--status-output-columns stream)))
  "Print a width-aware human-readable world-terminal session summary for STATUSES."
  (if (null statuses)
      (world-terminal--write-styled-line
       stream
       (list (terminal-span ':dim "· ") (terminal-span ':plain "no channels are hosted"))
       styled-p)
      (let* ((columns (max 4 columns))
             (fields  (world-terminal--status-table-fields columns))
             (title   "Hosted channels"))
        (world-terminal--write-styled-line
         stream
         (list (terminal-span ':brand (termdown:fit-text title columns)))
         styled-p)
        (if fields
            (let* ((rows
                     (cons
                      (mapcar #'world-terminal--status-field-label fields)
                      (mapcar
                       (lambda (status)
                         (mapcar
                          (lambda (field)
                            (world-terminal--status-field-text status field))
                          fields))
                       statuses)))
                   (widths
                     (termdown:column-widths
                      rows columns
                      :gap-width 1
                      :minimum-widths
                      (mapcar #'world-terminal--status-field-minimum-width fields)
                      :fill-p    t)))
              (world-terminal--write-styled-line
               stream (world-terminal--status-table-row nil fields widths :header-p t)
               styled-p)
              (world-terminal--write-styled-line
               stream (world-terminal--status-table-separator widths) styled-p)
              (loop for status in statuses
                    for remaining on statuses
                    do (world-terminal--write-styled-line
                        stream (world-terminal--status-table-row status fields widths)
                        styled-p)
                       (when (rest remaining)
                         (world-terminal--write-styled-line
                          stream (world-terminal--status-table-separator widths) styled-p))))
            (loop for status in statuses
                  for remaining on statuses
                  do (world-terminal--write-styled-line
                      stream (world-terminal--status-card-row status columns) styled-p)
                     (when (rest remaining)
                       (world-terminal--write-styled-line
                        stream (world-terminal--status-table-separator (list columns))
                        styled-p))))))
  nil)

(-> world-terminal--print-response (list stream &key (:styled-p boolean)) null)
(defun world-terminal--print-response
    (response stream &key (styled-p (world-terminal--status-output-styled-p stream)))
  "Print one concise successful world-terminal RESPONSE as a notice line."
  (let ((operation (getf (rest response) :operation))
        (channel   (session-identifier-display (getf (rest response) :session-id))))
    (world-terminal--write-styled-line
     stream
     (if (eq operation ':stop)
         (list (terminal-span ':dim "· ")
               (terminal-span ':notice "stopping all channels and terminating the world server"))
         (list (terminal-span ':dim "· ")
               (terminal-span ':plain "channel ")
               (terminal-span ':code channel)
               (terminal-span ':plain (case operation
                                        (:detach
                                         " detaches from its terminal")
                                        (:pause
                                         " pauses")
                                        (otherwise
                                         (format nil ": ~(~A~) requested" operation))))))
     styled-p))
  nil)

(-> world-terminal--client-cleanup-failure (string condition) list)
(defun world-terminal--client-cleanup-failure (stage condition)
  "Describe one cleanup failure with bounded class-only metadata."
  (list :stage (bounded-string stage :limit 64)
        :condition-type
        (handler-case
            (let ((name (class-name (class-of condition))))
              (if (symbolp name) (bounded-string (symbol-name name) :limit 128) "CONDITION"))
          (error () "CONDITION"))))

(-> world-terminal--wait-for-checkpoint-entry
    (cons string &key (:cancelled-p (option function))
          (:allow-later-p boolean))
    cons)
(defun world-terminal--wait-for-checkpoint-entry (entry reconnect-id &key cancelled-p allow-later-p)
  "Wait for ENTRY's authenticated endpoint for RECONNECT-ID until cancellation.

ALLOW-LATER-P accepts a later transition with the same channel, authentication
token and creation time after a verified endpoint loses a subscription race
to another checkpoint."
  (let* ((pathname                                      (first entry))
         (old-record                                    (rest entry))
         (session-id                                    (world-terminal--record-session-id old-record))
         (token                                         (world-terminal--record-token old-record))
         (image-daemon:*daemon-connect-timeout-seconds* nil))
    (loop
      (when (and cancelled-p (funcall cancelled-p))
        (error 'world-terminal-error :message "Checkpoint reconnection was cancelled."
                                :operation ':attach :session-id session-id))
      (let ((record (image-daemon:daemon-registry-read pathname)))
        (when (and record
                   (or (equal (getf (rest record) :checkpoint-reconnect-id) reconnect-id)
                       (and allow-later-p
                            (let ((identifier (getf (rest record) :checkpoint-reconnect-id)))
                              (and (stringp identifier) (plusp (length identifier))))))
                   (string= (world-terminal--record-session-id record) session-id)
                   (string= (world-terminal--record-token record) token)
                   (= (getf (rest record) :created-at) (getf (rest old-record) :created-at))
                   (handler-case
                        (eq (first (world-terminal-call (world-terminal--record-port record) token ':status)) ':ok)
                     (error () nil)))
          (return (cons pathname record))))
      (sleep 0.05))))

(-> world-terminal-client-screen-create (&key (:panel t)) world-terminal-screen)
(defun world-terminal-client-screen-create (&key panel)
  "Return a screen on a new terminal for this process's standard input and output, with PANEL beside the session."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (let ((terminal (stream-terminal-create
                     :rows                  rows
                     :columns               columns
                     :input-file-descriptor (terminal-standard-input-file-descriptor))))
      (setf (terminal-styled-p terminal) (terminal-environment-styling-p))
      (world-terminal-screen-create :terminal terminal :panel panel))))

(-> world-terminal-client-screen-release (world-terminal-screen) null)
(defun world-terminal-client-screen-release (screen)
  "Give back the terminal SCREEN painted on: leave its screen, then restore its input mode."
  (unwind-protect
       (world-terminal-screen-close screen)
    ; mallet:suppress no-ignore-errors -- best-effort terminal release during unwinding
    (ignore-errors (terminal-stop (world-terminal-screen-terminal screen))))
  nil)

(-> world-terminal-relay-exit
    ((option list) keyword &key (:session-id (option string)))
    null)
(defun world-terminal-relay-exit (exit mode &key session-id)
  "Report EXIT and how to reopen SESSION-ID after releasing the client's terminal.

A controlling terminal exits with the session's status. Read-only observers
only report it. A detach or connection loss has no EXIT."
  (let ((message (getf exit :message)))
    (when message
      (format *error-output* "~&~A~%" message))
    (when (and session-id
               (not (and message (search (channel-open-command session-id) message))))
      (let ((terminal (make-instance 'terminal :styled-p (terminal-environment-styling-p))))
        (format *error-output* "~&~A~%"
                (terminal--render-spans terminal (channel-open-instruction session-id)))))
    (finish-output *error-output*))
  (when (and exit (not (eq mode ':read-only)))
    (uiop:quit (getf exit :status)))
  nil)


;;;; -- World-Terminal Command Entry --

(-> world-terminal--client-configuration () configuration)
(defun world-terminal--client-configuration ()
  "Create a world-terminal client configuration and ensure its directories exist."
  (let ((configuration (configuration-create :defer-provider-validation-p t)))
    (configuration-ensure-directories configuration)
    (world-terminal-reconcile-endpoint-records configuration)
    configuration))

(-> world-terminal--run-status (&key (:sexp-p boolean)) null)
(defun world-terminal--run-status (&key sexp-p)
  "Print world-terminal session statuses, as readable forms when SEXP-P."
  (let* ((configuration (world-terminal--client-configuration))
         (statuses      (world-terminal-statuses configuration)))
    (if sexp-p
        (dolist (status statuses)
          (write status :stream *standard-output* :readably t)
          (terpri))
        (progn
          (world-client-print-server configuration *standard-output*)
          (world-terminal-print-statuses statuses))))
  nil)

(-> world-terminal--reachable-records (configuration) list)
(defun world-terminal--reachable-records (configuration)
  "Return endpoint records whose status endpoint is reachable."
  (loop for entry in (world-terminal-endpoint-records configuration)
        when (handler-case
                (world-terminal-query-record entry ':status)
              (world-terminal-error () nil))
        collect entry))

(-> world-terminal--session-record (configuration &optional (option string)) cons)
(defun world-terminal--session-record (configuration &optional session-id)
  "Return the explicitly selected or sole reachable session record."
  (if session-id
      (world-terminal--find-record configuration session-id)
      (let ((records (world-terminal--reachable-records configuration)))
        (cond ((null records)
               (error 'world-terminal-error
                      :message   "No channel is hosted."
                      :operation ':discover))
              ((rest records)
               (error 'world-terminal-error
                      :message   (format nil "Several channels are hosted; name one: ~{~A~^, ~}."
                                         (sort (mapcar (lambda (entry)
                                                         (session-identifier-display
                                                          (world-terminal--record-session-id (rest entry))))
                                                       records)
                                               #'string<))
                      :operation ':discover))
              (t
               (first records))))))

(-> world-terminal--status-command () clingon:command)
(defun world-terminal--status-command ()
  "Return the status command definition."
  (make-command
   :name        "status"
   :description "show the world server and the channels it hosts"
   :options     (list (make-option ':flag
                                   :long-name   "sexp"
                                   :key         ':sexp
                                   :description "print statuses as readable forms"))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'world-terminal-error
               :message  "status accepts no arguments."
              :operation ':arguments))
     (world-terminal--run-status
      :sexp-p (not (null (getopt command ':sexp)))))))

(-> world-terminal--operation-command (string string keyword) clingon:command)
(defun world-terminal--operation-command (name description operation)
  "Return a channel OPERATION command definition."
  (make-command
   :name        name
   :description description
   :usage       "[TARGET]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (member (length arguments) '(0 1))
         (error 'world-terminal-error
                :message   (format nil "~A accepts at most one channel or agent designator." name)
                :operation ':arguments))
       (let* ((configuration (world-terminal--client-configuration))
              (entry         (world-terminal--session-record configuration (first arguments))))
         (world-terminal--print-response (world-terminal-query-record entry operation)
                                    *standard-output*))))))
