(in-package #:autolith)

;;;; -- Localgroup Discovery Client --

(-> localgroup--record-session-id (list) string)
(defun localgroup--record-session-id (record)
  "Return RECORD's session identifier."
  (getf (rest record) :session-id))

(-> localgroup--record-port (list) integer)
(defun localgroup--record-port (record)
  "Return RECORD's loopback port."
  (getf (rest record) :port))

(-> localgroup--record-token (list) string)
(defun localgroup--record-token (record)
  "Return RECORD's private capability token."
  (getf (rest record) :token))

(-> localgroup--remove-stale-record (pathname list) null)
(defun localgroup--remove-stale-record (pathname expected-record)
  "Delete PATHNAME only when it still contains EXPECTED-RECORD."
  (image-daemon:daemon-registry-delete-matching pathname expected-record)
  nil)

(-> localgroup--record-stale-p (list condition) boolean)
(defun localgroup--record-stale-p (record condition)
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

(-> localgroup--query-with-progress (list keyword list) list)
(defun localgroup--query-with-progress (record operation arguments)
  "Request OPERATION with optional loading notifications before its final response."
  (multiple-value-bind (socket stream)
      (daemon-connect (localgroup--record-port record))
    (declare (ignore socket))
    (unwind-protect
         (sb-sys:with-deadline (:seconds image-daemon:*daemon-connect-timeout-seconds*)
           (daemon-write-packet
            stream (list :localgroup-request :version image-daemon:*daemon-protocol-version*
                         :token              (localgroup--record-token record)
                         :operation operation :arguments arguments))
           (loop for packet = (image-daemon:daemon-read-response stream operation)
                 until (not (eq (first packet) ':progress))
                 do (let ((step        (getf (rest packet) :step))
                          (description (getf (rest packet) :description)))
                      (unless (and (typep step '(integer 1 6)) (stringp description))
                        (error 'localgroup-error :message  "Invalid loading notification."
                                                :operation operation))
                      (channel-loading-report step description))
                 finally (return packet)))
      ; mallet:suppress no-ignore-errors -- closing the finished connection must not mask its response
      (ignore-errors (close stream)))))

(-> localgroup-query-record
    (cons keyword &optional list)
    list)
(defun localgroup-query-record (entry operation &optional arguments)
  "Perform OPERATION against endpoint ENTRY, removing its record when the failure shows it is stale."
  (let* ((pathname   (first entry))
         (record     (rest entry))
         (session-id (localgroup--record-session-id record))
         (response
           (handler-case
                (if (getf arguments :progress-p)
                    (localgroup--query-with-progress record operation arguments)
                    (daemon-call
                     (localgroup--record-port record)
                     (localgroup--record-token record)
                     operation
                     :arguments arguments))
             (error (condition)
               (when (localgroup--record-stale-p record condition)
                 (localgroup--remove-stale-record pathname record))
               (error 'localgroup-error
                       :message   (format nil "Localgroup session ~A is unavailable."
                                          (session-identifier-display
                                           session-id))
                      :operation  operation
                      :session-id session-id
                      :cause      condition)))))
    (unless (and (proper-list-p response)
                 (eq (first response) ':ok))
      (error 'localgroup-error
             :message    (or (getf (rest response) :message)
                             "The localgroup endpoint rejected the request.")
             :operation  operation
             :session-id session-id
             :retry-p    (and (proper-list-p response)
                              (not (null (getf (rest response) :retry-p))))))
    response))

(-> localgroup-owner-reachability (pathname string (integer 1)) keyword)
(defun localgroup-owner-reachability (pathname identifier owner-pid)
  "Query one matching owner record without pruning, under a single two-second budget.

Only definite record absence yields :NO-REACHABLE-ENDPOINT. Errors and identity
mismatches yield :UNKNOWN; neither read nor transport creates or modifies records."
  (handler-case
      (sb-sys:with-deadline (:seconds 2)
        (let ((before (platform-path-status *platform* pathname)))
          (unless before
            (return-from localgroup-owner-reachability ':no-reachable-endpoint))
          (with-open-stream (stream (platform-open-regular-file *platform* pathname))
            (unless (platform-file-status-same-object-p
                     before (platform-stream-status *platform* stream))
              (return-from localgroup-owner-reachability ':unknown))
            (let* ((buffer (make-array 65537 :element-type '(unsigned-byte 8)))
                   (count  (read-sequence buffer stream)))
              (when (> count 65536)
                (return-from localgroup-owner-reachability ':unknown))
              (let* ((*read-eval* nil)
                     (text        (utf8-octets-to-string (subseq buffer 0 count)))
                     (eof         (gensym "EOF")))
                (multiple-value-bind (record end)
                    (read-from-string text nil eof)
                  (unless (and (image-daemon:daemon-registry-record-p record)
                               (eq (read-from-string text nil eof :start end) eof)
                               (string= (localgroup--record-session-id record) identifier)
                               (eql (getf (rest record) :pid) owner-pid))
                    (return-from localgroup-owner-reachability ':unknown))
                  (let* ((response (daemon-call (localgroup--record-port record)
                                                (localgroup--record-token record) ':status))
                         (status   (and (proper-list-p response)
                                        (eq (first response) ':ok)
                                        (getf (rest response) :status))))
                    (if (and (proper-list-p status)
                             (eq (first status) ':localgroup-status)
                             (equal (getf (rest status) :session-id) identifier)
                             (eql (getf (rest status) :pid) owner-pid))
                        ':reachable
                        ':unknown))))))))
    ((or sb-sys:deadline-timeout error) ()
      ':unknown)))

(-> localgroup--find-record (configuration string) cons)
(defun localgroup--find-record (configuration session-id)
  "Return CONFIGURATION's endpoint for a channel ID, fragment or resident's name.

Resolve world designators first, including @NAME, bare names and legacy
aliases. Retain endpoint-only identifier discovery for rescue sessions.
Ambiguous or parked targets select nothing."
  (let* ((records (localgroup-endpoint-records configuration))
         (channel (handler-case
                      (world-channel-designated (world-load configuration) configuration
                                               session-id :error-p nil)
                    (world-error (condition)
                      (error 'localgroup-error
                             :message    (autolith-error-message condition)
                             :operation  ':discover
                             :session-id session-id
                             :cause      condition))))
         (matches (if channel
                      (remove-if-not
                       (lambda (entry)
                         (string= (channel-identifier channel)
                                  (localgroup--record-session-id (rest entry))))
                       records)
                      (identifier-match session-id records
                                        :key (lambda (entry)
                                               (localgroup--record-session-id (rest entry)))))))
    (cond ((null matches)
           (error 'localgroup-error
                  :message    (format nil "No hosted channel matches ~S." session-id)
                  :operation  ':discover
                  :session-id session-id))
          ((rest matches)
           (error 'localgroup-error
                  :message
                  (format nil "Localgroup session ID ~S is ambiguous. Matching sessions: ~{~A~^, ~}."
                          session-id
                          (sort (mapcar (lambda (entry)
                                          (session-identifier-display
                                           (localgroup--record-session-id (rest entry))))
                                        matches)
                                #'string<))
                  :operation  ':discover
                  :session-id session-id))
          (t
           (first matches)))))

(-> localgroup-statuses (configuration) list)
(defun localgroup-statuses (configuration)
  "Return live localgroup status snapshots, pruning unreachable records."
  (loop for entry in (localgroup-endpoint-records configuration)
        for status =
          (handler-case
              (getf (rest (localgroup-query-record entry ':status)) :status)
            (localgroup-error () nil))
        when status
          collect status))

(-> localgroup--status-state-text (list) string)
(defun localgroup--status-state-text (status)
  "Return STATUS's concise state label."
  (string-downcase (symbol-name (getf (rest status) :state))))

(-> localgroup--status-state-style (list) terminal-style)
(defun localgroup--status-state-style (status)
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

(-> localgroup--status-activity-text (list) string)
(defun localgroup--status-activity-text (status)
  "Return STATUS's compact queue and child activity summary."
  (format nil "q:~D s:~D jobs:~D"
          (getf (rest status) :queued-input-count)
          (getf (rest status) :steering-input-count)
          (getf (rest status) :task-live-count)))

(-> localgroup--status-timestamp-text (timestamp) string)
(defun localgroup--status-timestamp-text (timestamp)
  "Return TIMESTAMP as a compact UTC session-start label."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0DZ"
            year month date hour minute)))

(-> localgroup--status-started-text (list) string)
(defun localgroup--status-started-text (status)
  "Return STATUS's encoded or recorded session start time for display."
  (let* ((values     (rest status))
         (identifier (getf values :session-id))
         (timestamp
           (and (stringp identifier)
                (session-identifier-timestamp identifier))))
    (if timestamp
        (localgroup--status-timestamp-text timestamp)
        (let ((created-at (getf values :created-at)))
          (if (typep created-at 'timestamp)
              (localgroup--status-timestamp-text created-at)
              "unknown")))))

(-> localgroup--status-field-label (keyword) string)
(defun localgroup--status-field-label (field)
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

(-> localgroup--status-field-text (list keyword) string)
(defun localgroup--status-field-text (status field)
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
       (localgroup--status-state-text status))
      (:started
       (localgroup--status-started-text status))
      (:conversation
       (or (getf values :conversation-title)
           (getf values :conversation-display-id)))
      (:activity
       (localgroup--status-activity-text status))
      (:workspace
       (getf values :cwd)))))

(-> localgroup--status-field-style (list keyword) terminal-style)
(defun localgroup--status-field-style (status field)
  "Return the semantic style for STATUS's FIELD."
  (case field
    (:session
     ':code)
    (:pid
     ':code)
    (:state
     (localgroup--status-state-style status))
    (:started
     ':timestamp-time)
    (:conversation
     (if (getf (rest status) :conversation-title) ':plain ':code))
    (:activity
     ':dim)
    (:workspace
     ':plain)))

(-> localgroup--status-table-fields (integer) (option list))
(defun localgroup--status-table-fields (columns)
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

(-> localgroup--status-field-minimum-width (keyword) integer)
(defun localgroup--status-field-minimum-width (field)
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

(-> localgroup--status-output-styled-p (stream) boolean)
(defun localgroup--status-output-styled-p (stream)
  "Return true when STREAM is an interactive color-capable status destination."
  (not
   (null
    (and (interactive-stream-p stream)
         (terminal-environment-styling-p)))))

(-> localgroup--status-output-columns (stream) integer)
(defun localgroup--status-output-columns (stream)
  "Return a useful status-output width for STREAM."
  (if (interactive-stream-p stream)
      (nth-value 1 (terminal-current-size))
      *terminal-default-columns*))

(-> localgroup--write-styled-line (stream terminal-styled-text boolean) null)
(defun localgroup--write-styled-line (stream spans styled-p)
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

(-> localgroup--status-table-separator (list) terminal-styled-text)
(defun localgroup--status-table-separator (widths)
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

(-> localgroup--status-table-row
    (list list list &key (:header-p boolean))
    terminal-styled-text)
(defun localgroup--status-table-row (status fields widths &key header-p)
  "Return one STATUS row with separators between FIELDS across WIDTHS.

HEADER-P renders field labels rather than status values."
  (loop for field in fields
        for width in widths
        for remaining on fields
        collect
        (terminal-span
         (if header-p ':strong (localgroup--status-field-style status field))
         (layout-fit-text
          (if header-p
              (localgroup--status-field-label field)
              (localgroup--status-field-text status field))
          width))
        when (rest remaining)
          collect (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))))

(-> localgroup--status-card-row (list integer) terminal-styled-text)
(defun localgroup--status-card-row (status columns)
  "Return compact rows for STATUS within COLUMNS cells."
  (let* ((identifier   (localgroup--status-field-text status ':session))
         (state        (localgroup--status-field-text status ':state))
         (session-width
          (min (text-cell-width identifier)
               (max 1 (- columns (text-cell-width state) 2))))
         (state-width  (max 0 (- columns session-width 2)))
         (detail
          (format nil "~A  ~A"
                  (localgroup--status-field-text status ':started)
                  (localgroup--status-field-text status ':activity)))
         (conversation (localgroup--status-field-text status ':conversation)))
    (list (terminal-span ':code (layout-fit-text identifier session-width))
          (terminal-span ':plain "  ")
          (terminal-span (localgroup--status-state-style status)
                         (layout-fit-text state state-width))
          (terminal-span ':plain (string #\Newline))
          (terminal-span ':timestamp-time (layout-fit-text detail columns))
          (terminal-span ':plain (string #\Newline))
          (terminal-span (localgroup--status-field-style status ':conversation)
                         (layout-fit-text conversation columns))
          (terminal-span ':plain (string #\Newline))
          (terminal-span ':plain
                         (layout-fit-text
                          (localgroup--status-field-text status ':workspace)
                          columns)))))

(-> localgroup-print-statuses
    (list &key (:stream stream) (:styled-p boolean) (:columns integer))
    null)
(defun localgroup-print-statuses
    (statuses &key
                (stream *standard-output*)
                (styled-p (localgroup--status-output-styled-p stream))
                (columns (localgroup--status-output-columns stream)))
  "Print a width-aware human-readable localgroup session summary for STATUSES."
  (if (null statuses)
      (localgroup--write-styled-line
       stream
       (list (terminal-span ':dim "· ") (terminal-span ':plain "no channels are hosted"))
       styled-p)
      (let* ((columns (max 4 columns))
             (fields  (localgroup--status-table-fields columns))
             (title   "Hosted channels"))
        (localgroup--write-styled-line
         stream
         (list (terminal-span ':brand (layout-fit-text title columns)))
         styled-p)
        (if fields
            (let* ((rows
                     (cons
                      (mapcar #'localgroup--status-field-label fields)
                      (mapcar
                       (lambda (status)
                         (mapcar
                          (lambda (field)
                            (localgroup--status-field-text status field))
                          fields))
                       statuses)))
                   (widths
                     (layout-column-widths
                      rows columns
                      :gap-width 1
                      :minimum-widths
                      (mapcar #'localgroup--status-field-minimum-width fields)
                      :fill-p    t)))
              (localgroup--write-styled-line
               stream (localgroup--status-table-row nil fields widths :header-p t)
               styled-p)
              (localgroup--write-styled-line
               stream (localgroup--status-table-separator widths) styled-p)
              (loop for status in statuses
                    for remaining on statuses
                    do (localgroup--write-styled-line
                        stream (localgroup--status-table-row status fields widths)
                        styled-p)
                       (when (rest remaining)
                         (localgroup--write-styled-line
                          stream (localgroup--status-table-separator widths) styled-p))))
            (loop for status in statuses
                  for remaining on statuses
                  do (localgroup--write-styled-line
                      stream (localgroup--status-card-row status columns) styled-p)
                     (when (rest remaining)
                       (localgroup--write-styled-line
                        stream (localgroup--status-table-separator (list columns))
                        styled-p))))))
  nil)

(-> localgroup--print-response (list stream &key (:styled-p boolean)) null)
(defun localgroup--print-response
    (response stream &key (styled-p (localgroup--status-output-styled-p stream)))
  "Print one concise successful localgroup RESPONSE as a notice line."
  (let ((operation (getf (rest response) :operation))
        (channel   (session-identifier-display (getf (rest response) :session-id))))
    (localgroup--write-styled-line
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

(-> localgroup--client-cleanup-failure (string condition) list)
(defun localgroup--client-cleanup-failure (stage condition)
  "Describe one cleanup failure with bounded class-only metadata."
  (list :stage (bounded-string stage :limit 64)
        :condition-type
        (handler-case
            (let ((name (class-name (class-of condition))))
              (if (symbolp name) (bounded-string (symbol-name name) :limit 128) "CONDITION"))
          (error () "CONDITION"))))

(-> localgroup--client-attachment-failure (condition string) list)
(defun localgroup--client-attachment-failure (condition stage)
  "Describe a failure after acknowledgment without printing condition payloads."
  (list* :attachment-lost :kind ':callback-failure
         :message         "The accepted attachment could not continue terminal I/O."
         (localgroup--client-cleanup-failure stage condition)))

(-> localgroup--client-cleanup-result ((option list) list list) (values (option list) list))
(defun localgroup--client-cleanup-result (result metadata failures)
  "Append bounded cleanup metadata without replacing the primary attachment cause.

Intentional departures retain their exact primary value; cleanup information is
also returned as the second value. Attachment-loss results carry it in their plist."
  (let* ((metadata (copy-list metadata))
         (combined (append (getf metadata :cleanup-failures) failures))
         (bounded  (subseq combined 0 (min 8 (length combined)))))
    (when bounded
      (setf (getf metadata :cleanup-failures) bounded)
      (when (and (consp result) (eq (first result) ':attachment-lost))
        (setf result (copy-list result)
              (getf (rest result) :cleanup-failures) bounded)))
    (values result metadata)))

(-> localgroup--attach-terminal-loop
    (stream terminal keyword &key (:socket t) (:screen (option localgroup-screen)))
    (values (option list) list))
(defun localgroup--attach-terminal-loop (socket-stream terminal mode &key socket screen)
  "Relay terminal I/O and distinguish attachment loss from intentional departure.

SCREEN belongs to the caller when supplied. Return an application exit, a
checkpoint transition, a switch request, or (:ATTACHMENT-LOST . DETAILS)."
  (let* ((output           (make-mode-tracking-output-stream *standard-output*))
         (owned-p          (null screen))
         (screen           (or screen (localgroup-screen-create :terminal terminal)))
         (evaluator        (make-instance 'client-evaluator))
         (reconnect        nil)
         (result           nil)
         (metadata         nil)
         (cleanup-failures nil))
    (flet ((cleanup (stage function)
             (handler-case (funcall function)
                           (error (condition)
                                  (push (localgroup--client-cleanup-failure stage condition)
                                        cleanup-failures)))))
      (unwind-protect
          (handler-case
           (multiple-value-setq (result metadata)
             (multiple-value-bind (exit termination)
                 (image-daemon:daemon-attach-client-run socket-stream :mode mode :socket socket
                                                        :output-stream output :input-ready-function
                                                        (lambda ()
                                                          (localgroup-screen-tick screen)
                                                          (terminal-input-ready-p terminal))
                                                        :read-event-function
                                                        (lambda ()
                                                          (let ((event
                                                                 (localgroup-screen-filter-event
                                                                  screen
                                                                  (terminal-read-event terminal))))
                                                            (if (and (consp event)
                                                                     (eq (first event) ':switch))
                                                                (progn
                                                                 (setf reconnect event)
                                                                 ':stream-end)
                                                                event)))
                                                        :packet-function
                                                        (lambda (packet)
                                                          (cond
                                                           ((localgroup-screen-handle-packet screen
                                                                                             packet)
                                                            nil)
                                                           ((eq (first packet) ':evaluation)
                                                            (client-evaluator-start evaluator
                                                                                    packet))
                                                           ((or
                                                             (and
                                                              (eq (first packet)
                                                                  ':checkpoint-reconnect)
                                                              (non-empty-string-p
                                                               (getf (rest packet) :id))
                                                              (typep
                                                               (getf (rest packet)
                                                                     :history-position)
                                                               '(integer 0)))
                                                             (and (eq (first packet) ':switch)
                                                                  (non-empty-string-p
                                                                   (getf (rest packet)
                                                                         :session-id))))
                                                            (setf reconnect packet) t)
                                                           (t
                                                            nil)))
                                                        :outgoing-function
                                                        (lambda ()
                                                          (client-evaluator-take evaluator))
                                                        :resize-function
                                                        (lambda ()
                                                          (let ((terminal-p
                                                                 (shiftf
                                                                  *terminal-resize-pending-p* nil)))
                                                            (when
                                                                (or
                                                                 (localgroup-screen-take-resize
                                                                  screen)
                                                                 terminal-p)
                                                              (when terminal-p
                                                                (multiple-value-bind (rows columns)
                                                                    (terminal-current-size)
                                                                  (terminal-set-dimensions terminal
                                                                                           columns
                                                                                           :rows
                                                                                           rows))
                                                                (localgroup-screen-resized screen))
                                                              (multiple-value-bind (rows columns)
                                                                  (localgroup-screen-pane-size
                                                                   screen)
                                                                (list :rows rows :columns columns
                                                                      :styled-p
                                                                      (terminal-environment-styling-p)))))))
               (values
                (cond (reconnect
                       reconnect)
                      ((member (getf termination :kind)
                               '(:unexpected-eof :decoding-failure :callback-failure
                                 :transport-failure :remote-failure :queue-overflow
                                 :cleanup-failure))
                       (cons ':attachment-lost termination))
                      (t
                       exit))
                termination)))
           (error (condition)
                  (setf result (localgroup--client-attachment-failure condition "receive"))))
        (cleanup "screen" (lambda () (when owned-p (localgroup-screen-close screen))))
        (cleanup "terminal-mode" (lambda () (mode-tracking-output-stream-restore output))))
      (localgroup--client-cleanup-result result metadata (nreverse cleanup-failures)))))

(-> localgroup--wait-for-handoff-entry
    (configuration string &key (:token string) (:old-pid integer))
    cons)
(defun localgroup--wait-for-handoff-entry (configuration session-id &key token old-pid)
  "Wait for SESSION-ID's authenticated replacement endpoint after OLD-PID."
  (let ((deadline
         (+ (get-internal-real-time)
            (* *localgroup-handoff-start-timeout-seconds*
               internal-time-units-per-second))))
    (loop
     (let* ((pathname (localgroup-registry-pathname configuration session-id))
            (record   (image-daemon:daemon-registry-read pathname)))
       (when
           (and record (/= (getf (rest record) :pid) old-pid)
                (string= (localgroup--record-token record) token)
                (handler-case
                 (eq
                  (first (daemon-call (localgroup--record-port record) token ':status))
                  ':ok)
                 (error nil nil)))
         (return (cons pathname record))))
     (when (>= (get-internal-real-time) deadline)
       (error 'localgroup-error :message
              "The detached localgroup replacement did not become ready." :operation
              ':attach :session-id session-id))
     (sleep 0.05))))

(-> localgroup--wait-for-checkpoint-entry
    (cons string &key (:configuration (option configuration)) (:cancelled-p (option function))
          (:allow-later-p boolean))
    cons)
(defun localgroup--wait-for-checkpoint-entry (entry reconnect-id &key configuration cancelled-p allow-later-p)
  "Wait boundedly for ENTRY's authenticated endpoint for RECONNECT-ID.

While CONFIGURATION's world server restarts from its saved heap, the wait
starts over instead of ending, since the endpoint returns only once the
saved core boots. ALLOW-LATER-P accepts a later transition with the same
channel, authentication token and creation time after a verified endpoint
loses a subscription race to another checkpoint."
  (let* ((pathname   (first entry))
         (old-record (rest entry))
         (session-id (localgroup--record-session-id old-record))
         (token      (localgroup--record-token old-record))
         (timeout    (* *localgroup-handoff-start-timeout-seconds*
                        internal-time-units-per-second))
         (deadline   (+ (get-internal-real-time) timeout)))
    (loop
      (when (and cancelled-p (funcall cancelled-p))
        (error 'localgroup-error :message "Checkpoint reconnection was cancelled."
                                :operation ':attach :session-id session-id))
      (let ((record (image-daemon:daemon-registry-read pathname)))
        (when (and record
                   (or (equal (getf (rest record) :checkpoint-reconnect-id) reconnect-id)
                       (and allow-later-p
                            (let ((identifier (getf (rest record) :checkpoint-reconnect-id)))
                              (and (stringp identifier) (plusp (length identifier))))))
                   (string= (localgroup--record-session-id record) session-id)
                   (string= (localgroup--record-token record) token)
                   (= (getf (rest record) :created-at) (getf (rest old-record) :created-at))
                   (handler-case
                       (let ((image-daemon:*daemon-connect-timeout-seconds*
                               (min image-daemon:*daemon-connect-timeout-seconds*
                                    (max 0.001 (/ (- deadline (get-internal-real-time))
                                                  (* 2 internal-time-units-per-second))))))
                         (eq (first (daemon-call (localgroup--record-port record) token ':status)) ':ok))
                     (error () nil)))
          (return (cons pathname record))))
      (when (>= (get-internal-real-time) deadline)
        (unless (and configuration (world-server-restarting-p configuration))
          (error 'localgroup-error :message "The checkpoint endpoint did not become ready."
                                  :operation ':attach :session-id session-id))
        (setf deadline (+ (get-internal-real-time) timeout)))
      (sleep 0.05))))

(-> localgroup-attach-record
    ((option configuration) cons keyword &key (:panel t) (:screen (option localgroup-screen))
     (:notice (option string)) (:client-version (option string))
     (:attached-function (option function)) (:deadline (option integer)))
    (values (option list) list))
(defun localgroup-attach-record (configuration entry mode
                                  &key panel screen notice (client-version *autolith-version*)
                                    attached-function deadline)
  "Attach this terminal and follow explicit checkpoints in the same mode.

Preserve the primary exit/switch/detach value and return bounded cleanup
metadata as the second value. A failed checkpoint wait or replacement
attachment returns attachment loss. The caller owns a supplied PANEL or SCREEN."
  (let ((history-position nil)
        (cleanup-failures nil)
        (reconnecting-p   nil))
    (loop
      (handler-case
          (multiple-value-bind (result metadata)
              (localgroup--attach-record-once
               configuration entry mode :history-position history-position
               :panel panel :screen screen :notice (shiftf notice nil)
               :client-version                            (shiftf client-version nil)
               :attached-function attached-function :deadline deadline)
            (unless (eq (first result) ':checkpoint-reconnect)
              (return (localgroup--client-cleanup-result result metadata cleanup-failures)))
            (setf reconnecting-p t
                  cleanup-failures
                  (let ((all (append cleanup-failures (getf metadata :cleanup-failures))))
                    (subseq all 0 (min 8 (length all)))))
            (setf entry (localgroup--wait-for-checkpoint-entry
                         entry (getf (rest result) :id) :configuration configuration)
                  history-position (getf (rest result) :history-position)
                  deadline nil))
        (error (condition)
          (unless reconnecting-p (error condition))
          (return
            (localgroup--client-cleanup-result
             (list* ':attachment-lost :kind ':transport-failure
                    :message                "The checkpoint attachment could not be restored."
                    (localgroup--client-cleanup-failure "checkpoint-reconnect" condition))
             nil cleanup-failures)))))))

(-> localgroup-client-screen-create (&key (:panel t)) localgroup-screen)
(defun localgroup-client-screen-create (&key panel)
  "Return a screen on a new terminal for this process's standard input and output, with PANEL beside the session."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (let ((terminal (stream-terminal-create
                     :rows                  rows
                     :columns               columns
                     :input-file-descriptor (terminal-standard-input-file-descriptor))))
      (setf (terminal-styled-p terminal) (terminal-environment-styling-p))
      (localgroup-screen-create :terminal terminal :panel panel))))

(-> localgroup-client-screen-release (localgroup-screen) null)
(defun localgroup-client-screen-release (screen)
  "Give back the terminal SCREEN painted on: leave its screen, then restore its input mode."
  (unwind-protect
       (localgroup-screen-close screen)
    ; mallet:suppress no-ignore-errors -- best-effort terminal release during unwinding
    (ignore-errors (terminal-stop (localgroup-screen-terminal screen))))
  nil)

(-> localgroup--attach-record-once
    ((option configuration) cons keyword &key (:history-position (option integer)) (:panel t)
     (:screen (option localgroup-screen)) (:notice (option string))
     (:client-version (option string)) (:attached-function (option function))
     (:deadline (option integer)))
    (values (option list) list))
(defun localgroup--attach-record-once
       (configuration entry mode
        &key history-position panel screen notice client-version attached-function deadline)
  "Run one attachment and release terminal ownership before any reconnect.

The session gets the part of the terminal left beside PANEL as its size. A
SCREEN the caller lends stays the caller's to release."
  (let* ((record           (rest entry))
         (owned-p          (null screen))
         (screen           (or screen (localgroup-client-screen-create :panel panel)))
         (terminal         (localgroup-screen-terminal screen))
         (socket           nil)
         (socket-stream    nil)
         (resize-watch     nil)
         (handoff-entry    nil)
         (acknowledged-p   nil)
         (result           nil)
         (metadata         nil)
         (cleanup-failures nil)
         (pending          nil))
    (flet ((cleanup (stage function)
             (handler-case (funcall function)
                           (error (condition)
                                  (push (localgroup--client-cleanup-failure stage condition)
                                        cleanup-failures)))))
      (unwind-protect
          (handler-case
           (multiple-value-setq (result metadata)
             (block localgroup-client-attachment
               (progn
                (handler-case
                 (sb-sys:with-deadline (:seconds
                                        (if deadline
                                            (max 0
                                                 (/ (- deadline (get-internal-real-time))
                                                    internal-time-units-per-second))
                                            *daemon-connect-timeout-seconds*))
                   (unless (terminal--terminal-mode-or-nil terminal)
                     (error 'localgroup-error :message
                            "Opening a channel requires an interactive terminal." :operation
                            ':attach :session-id (localgroup--record-session-id record)))
                   (multiple-value-setq (socket socket-stream)
                     (daemon-connect (localgroup--record-port record)))
                   (multiple-value-bind (rows columns)
                       (localgroup-screen-pane-size screen)
                     (daemon-write-packet socket-stream
                                          (list :localgroup-request :version
                                                *daemon-protocol-version* :token
                                                (localgroup--record-token record) :operation
                                                ':attach :arguments
                                                (append
                                                 (list :mode mode :rows rows :columns columns
                                                       :client-pid (current-process-id) :styled-p
                                                       (terminal-environment-styling-p)
                                                       :evaluates-p t)
                                                 (when client-version
                                                   (list :client-version client-version))
                                                 (when notice (list :notice notice))))))
                   (let ((response (daemon-read-response socket-stream ':attach)))
                     (cond
                      ((and response (eq (first response) ':handoff))
                       (let ((session-id (getf (rest response) :session-id))
                             (old-pid    (getf (rest response) :old-pid))
                             (token      (localgroup--record-token record)))
                         (unless (and (non-empty-string-p session-id) (typep old-pid '(integer 1)))
                           (error 'localgroup-error :message
                                  "The localgroup handoff response was malformed." :operation
                                  ':attach :session-id (localgroup--record-session-id record)))
                         (close socket-stream)
                         (setf socket-stream nil
                               socket nil)
                         (setf handoff-entry
                                 (localgroup--wait-for-handoff-entry configuration session-id
                                                                     :token token :old-pid
                                                                     old-pid))))
                      ((and response (eq (first response) ':attached))
                       (setf acknowledged-p t)
                       (let ((history (getf (rest response) :history)))
                        (when (and (stringp history) (not (localgroup-screen-active-p screen)))
                         (write-string history *standard-output* :start
                          (if history-position
                           (min (length history)
                             (max 0
                                  (- history-position
                                     (getf (rest response) :history-start
                                           0))))
                           0))
                         (finish-output *standard-output*))))
                      (t
                       (error 'localgroup-error :message
                              (or (and response (getf (rest response) :message))
                                  "The localgroup attachment was rejected.")
                              :operation ':attach :session-id
                              (localgroup--record-session-id record)))))
                   (when (and attached-function (not handoff-entry)) (funcall attached-function)))
                 (sb-sys:deadline-timeout (condition)
                  (error 'localgroup-error :operation ':attach :session-id
                         (localgroup--record-session-id record) :cause condition :message
                         "The channel attachment timed out before acknowledgment.")))
                (when handoff-entry
                  (return-from localgroup-client-attachment
                    (localgroup-attach-record configuration handoff-entry ':control :panel panel
                                              :screen (and (not owned-p) screen) :client-version
                                              client-version :attached-function attached-function
                                              :deadline                                    deadline)))
                (terminal-start terminal)
                (setf resize-watch
                        (platform-watch-terminal-resize *platform*
                                                        (lambda ()
                                                          (setf *terminal-resize-pending-p* t))))
                (localgroup--attach-terminal-loop socket-stream terminal mode :socket socket
                                                  :screen                             screen))))
           (error (condition)
                  (if acknowledged-p
                      (setf result (localgroup--client-attachment-failure condition "setup"))
                      (setf pending condition))))
        (cleanup "resize-watch"
         (lambda ()
           (when resize-watch (platform-unwatch-terminal-resize *platform* resize-watch))))
        (cleanup "client-screen"
         (lambda () (when owned-p (localgroup-client-screen-release screen))))
        (cleanup "stream" (lambda () (when socket-stream (close socket-stream :abort t))))
        (cleanup "socket" (lambda () (when socket (sb-bsd-sockets:socket-close socket :abort t)))))
      (when pending (error pending))
      (localgroup--client-cleanup-result result metadata (nreverse cleanup-failures)))))

(-> localgroup-relay-exit
    (configuration (option list) keyword &key (:session-id (option string)))
    null)
(defun localgroup-relay-exit (configuration exit mode &key session-id)
  "Report EXIT and how to reopen SESSION-ID after releasing the client's terminal.

For a controlling terminal, publish EXIT's crash capsule to the launcher and
exit with its status. Read-only observers report without publishing or exiting.
A detach or connection loss has no EXIT but still names the channel to reopen."
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
    (let ((capsule (getf (getf exit :details) :crash-capsule)))
      (when (non-empty-string-p capsule)
        (handler-case
            (application-publish-crash-pointer configuration (pathname capsule))
          (error (condition)
            (format *error-output* "~&Could not hand the crash capsule to recovery: ~A~%"
                    condition)))))
    (uiop:quit (getf exit :status)))
  nil)


;;;; -- Localgroup Command Entry --

(-> localgroup--client-configuration () configuration)
(defun localgroup--client-configuration ()
  "Create a localgroup client configuration and ensure its directories exist."
  (let ((configuration (configuration-create :defer-provider-validation-p t)))
    (configuration-ensure-directories configuration)
    (localgroup-reconcile-endpoint-records configuration)
    configuration))

(-> localgroup--run-status (&key (:sexp-p boolean)) null)
(defun localgroup--run-status (&key sexp-p)
  "Print localgroup session statuses, as readable forms when SEXP-P."
  (let* ((configuration (localgroup--client-configuration))
         (statuses      (localgroup-statuses configuration)))
    (if sexp-p
        (dolist (status statuses)
          (write status :stream *standard-output* :readably t)
          (terpri))
        (progn
          (world-client-print-server configuration *standard-output*)
          (localgroup-print-statuses statuses))))
  nil)

(-> localgroup--reachable-records (configuration) list)
(defun localgroup--reachable-records (configuration)
  "Return endpoint records whose status endpoint is reachable."
  (loop for entry in (localgroup-endpoint-records configuration)
        when (handler-case
                (localgroup-query-record entry ':status)
              (localgroup-error () nil))
        collect entry))

(-> localgroup--session-record (configuration &optional (option string)) cons)
(defun localgroup--session-record (configuration &optional session-id)
  "Return the explicitly selected or sole reachable session record."
  (if session-id
      (localgroup--find-record configuration session-id)
      (let ((records (localgroup--reachable-records configuration)))
        (cond ((null records)
               (error 'localgroup-error
                      :message   "No channel is hosted."
                      :operation ':discover))
              ((rest records)
               (error 'localgroup-error
                      :message   (format nil "Several channels are hosted; name one: ~{~A~^, ~}."
                                         (sort (mapcar (lambda (entry)
                                                         (session-identifier-display
                                                          (localgroup--record-session-id (rest entry))))
                                                       records)
                                               #'string<))
                      :operation ':discover))
              (t
               (first records))))))

(-> localgroup--status-command () clingon:command)
(defun localgroup--status-command ()
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
       (error 'localgroup-error
               :message  "status accepts no arguments."
              :operation ':arguments))
     (localgroup--run-status
      :sexp-p (not (null (getopt command ':sexp)))))))

(-> localgroup--operation-command (string string keyword) clingon:command)
(defun localgroup--operation-command (name description operation)
  "Return a channel OPERATION command definition."
  (make-command
   :name        name
   :description description
   :usage       "[TARGET]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (member (length arguments) '(0 1))
         (error 'localgroup-error
                :message   (format nil "~A accepts at most one channel or agent designator." name)
                :operation ':arguments))
       (let* ((configuration (localgroup--client-configuration))
              (entry         (localgroup--session-record configuration (first arguments))))
         (localgroup--print-response (localgroup-query-record entry operation)
                                    *standard-output*))))))
