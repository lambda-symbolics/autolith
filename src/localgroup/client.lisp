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
               (daemon-call
                (localgroup--record-port record)
                (localgroup--record-token record)
                operation
                :arguments arguments)
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
  "Return CONFIGURATION's unique endpoint matching SESSION-ID in full or in part.

An exact identifier in canonical or display form wins; otherwise the one
record whose identifier contains SESSION-ID, compared without regard to case.
Several matches are reported together and select nothing."
  (let* ((records (localgroup-endpoint-records configuration))
         (matches (identifier-match session-id records
                                    :key (lambda (entry)
                                           (localgroup--record-session-id (rest entry))))))
    (cond ((null matches)
           (error 'localgroup-error
                  :message    (format nil "No running localgroup session matches ~S." session-id)
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
     "SESSION")
    (:pid
     "PID")
    (:state
     "STATE")
    (:started
     "STARTED")
    (:conversation
     "CONVERSATION")
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
       (let ((pid (getf values :pid)))
         (if (typep pid '(integer 1))
             (format nil "~D" pid)
             "unknown")))
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
       (list (terminal-span ':notice "No local Autolith sessions are running."))
       styled-p)
      (let* ((columns (max 4 columns))
             (fields  (localgroup--status-table-fields columns))
             (title   "Local Autolith sessions"))
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
  "Print one concise successful localgroup RESPONSE."
  (localgroup--write-styled-line
   stream
   (list (terminal-span ':success
                        (format nil "~(~A~) requested for localgroup session "
                                (getf (rest response) :operation)))
         (terminal-span
          ':code
          (session-identifier-display
           (getf (rest response) :session-id)))
         (terminal-span ':plain "."))
   styled-p)
  nil)

(-> localgroup--attach-terminal-loop
    (stream terminal keyword &key (:socket t) (:screen (option localgroup-screen)))
    (option list))
(defun localgroup--attach-terminal-loop (socket-stream terminal mode &key socket screen)
  "Relay terminal I/O; return an application exit, a checkpoint transition or a switch request.

SCREEN paints the session's frames and its own side panel on TERMINAL, and
stays open for the caller; without it the loop paints on a screen of its own
and closes it. A (:SWITCH :SESSION-ID ID) result asks the caller to attach to
channel ID instead; this attachment has ended by then."
  (let* ((output    (make-mode-tracking-output-stream *standard-output*))
         (owned-p   (null screen))
         (screen    (or screen (localgroup-screen-create :terminal terminal)))
         (reconnect nil))
    (unwind-protect
         (let ((exit
                 (image-daemon:daemon-attach-client-run
                  socket-stream :mode mode :socket socket :output-stream output
                  :input-ready-function
                  (lambda ()
                    ;; The side panel refreshes between keystrokes.
                    (localgroup-screen-tick screen)
                    (terminal-input-ready-p terminal))
                  :read-event-function
                  (lambda ()
                    (localgroup-screen-filter-event screen (terminal-read-event terminal)))
                  :packet-function
                  (lambda (packet)
                    (cond
                      ((localgroup-screen-handle-packet screen packet)
                       nil)
                      ((or (and (eq (first packet) ':checkpoint-reconnect)
                                (non-empty-string-p (getf (rest packet) :id))
                                (typep (getf (rest packet) :history-position) '(integer 0)))
                           (and (eq (first packet) ':switch)
                                (non-empty-string-p (getf (rest packet) :session-id))))
                       (setf reconnect packet)
                       t)
                      (t
                       nil)))
                  :resize-function
                  (lambda ()
                    (let ((terminal-p (shiftf *terminal-resize-pending-p* nil)))
                      (when (or (localgroup-screen-take-resize screen) terminal-p)
                        (when terminal-p
                          (multiple-value-bind (rows columns) (terminal-current-size)
                            (terminal-set-dimensions terminal columns :rows rows))
                          (localgroup-screen-resized screen))
                        (multiple-value-bind (rows columns) (localgroup-screen-pane-size screen)
                          (list :rows rows :columns columns
                                :styled-p (terminal-environment-styling-p)))))))))
           (or reconnect exit))
      (when owned-p
        (localgroup-screen-close screen))
      (mode-tracking-output-stream-restore output))))

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
    ((option configuration) cons keyword &key (:panel t) (:screen (option localgroup-screen)))
    (option list))
(defun localgroup-attach-record (configuration entry mode &key panel screen)
  "Attach the terminal, following explicit checkpoint transitions in the same mode.

PANEL, when given, is composed beside a fullscreen session's frames. SCREEN,
from LOCALGROUP-CLIENT-SCREEN-CREATE, is the client's screen to paint on; the
caller keeps it after this attachment ends, so a terminal moving to another
channel keeps its screen. Without SCREEN the attachment makes its own and
gives the terminal back at the end. Return an application exit plist, a
(:SWITCH :SESSION-ID ID) request from the session, or NIL after a detach or
connection loss."
  (let ((history-position nil))
    (loop
      (let ((result (localgroup--attach-record-once configuration entry mode
                                                   :history-position history-position
                                                   :panel            panel
                                                   :screen           screen)))
        (unless (eq (first result) ':checkpoint-reconnect)
          (return result))
        (setf entry (localgroup--wait-for-checkpoint-entry entry (getf (rest result) :id)
                                                           :configuration configuration)
              history-position (getf (rest result) :history-position))))))

(-> localgroup-client-screen-create (&key (:panel t)) localgroup-screen)
(defun localgroup-client-screen-create (&key panel)
  "Return a screen on a new terminal for this process's standard input and output, with PANEL beside the session."
  (multiple-value-bind (rows columns)
      (terminal-current-size)
    (localgroup-screen-create
     :terminal (stream-terminal-create
                :rows                  rows
                :columns               columns
                :input-file-descriptor (terminal-standard-input-file-descriptor))
     :panel    panel)))

(-> localgroup-client-screen-release (localgroup-screen) null)
(defun localgroup-client-screen-release (screen)
  "Give back the terminal SCREEN painted on: leave its screen, then restore its input mode."
  (localgroup-screen-close screen)
  ; mallet:suppress no-ignore-errors -- best-effort terminal release during unwinding
  (ignore-errors (terminal-stop (localgroup-screen-terminal screen)))
  nil)

(-> localgroup--attach-record-once
    ((option configuration) cons keyword
     &key (:history-position (option integer)) (:panel t) (:screen (option localgroup-screen)))
    (option list))
(defun localgroup--attach-record-once (configuration entry mode &key history-position panel screen)
  "Run one attachment and release terminal ownership before any reconnect.

The session gets the part of the terminal left beside PANEL as its size. A
SCREEN the caller lends stays the caller's to release."
  (let* ((record        (rest entry))
         (owned-p       (null screen))
         (screen        (or screen (localgroup-client-screen-create :panel panel)))
         (terminal      (localgroup-screen-terminal screen))
         (socket        nil)
         (socket-stream nil)
         (resize-watch  nil))
    (unwind-protect
        (progn
         (unless (terminal--terminal-mode-or-nil terminal)
           (error 'localgroup-error :message
                  "localgroup attach requires an interactive terminal." :operation
                  ':attach :session-id (localgroup--record-session-id record)))
         (multiple-value-setq (socket socket-stream)
           (daemon-connect (localgroup--record-port record)))
         (multiple-value-bind (rows columns) (localgroup-screen-pane-size screen)
           (daemon-write-packet socket-stream
                                (list :localgroup-request :version
                                      *daemon-protocol-version* :token
                                      (localgroup--record-token record) :operation ':attach
                                      :arguments
                                      (list :mode mode :rows rows :columns columns
                                            :styled-p (terminal-environment-styling-p)))))
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
               (return-from localgroup--attach-record-once
                 (localgroup-attach-record configuration
                                           (localgroup--wait-for-handoff-entry
                                            configuration session-id
                                            :token   token
                                            :old-pid old-pid)
                                           ':control
                                           :panel  panel
                                           :screen (and (not owned-p) screen)))))
            ((and response (eq (first response) ':attached))
             (let ((history (getf (rest response) :history)))
               ;; A screen still showing the last channel's frames gets this
               ;; channel's first frame next; streamed history would only
               ;; scribble over it.
               (when (and (stringp history)
                          (not (localgroup-screen-active-p screen)))
                 (write-string history *standard-output*
                               :start (if history-position
                                          (min (length history)
                                               (max 0 (- history-position
                                                         (getf (rest response) :history-start 0))))
                                          0))
                 (finish-output *standard-output*))))
            (t
             (error 'localgroup-error :message
                    (or (and response (getf (rest response) :message))
                        "The localgroup attachment was rejected.")
                    :operation ':attach :session-id
                    (localgroup--record-session-id record)))))
         (terminal-start terminal)
         (setf resize-watch
               (platform-watch-terminal-resize
                *platform*
                (lambda ()
                  (setf *terminal-resize-pending-p* t))))
         (localgroup--attach-terminal-loop socket-stream terminal mode
                                           :socket socket
                                           :screen screen))
      (when resize-watch
        (platform-unwatch-terminal-resize *platform* resize-watch))
      ;; Cleanup is best-effort so it never masks the attachment outcome.
      (when owned-p
        (localgroup-client-screen-release screen))
      (when socket-stream
        ; mallet:suppress no-ignore-errors -- best-effort stream close during unwinding
        (ignore-errors (close socket-stream)))
      (when (and socket (null socket-stream))
        ; mallet:suppress no-ignore-errors -- best-effort socket close during unwinding
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(-> localgroup-relay-exit (configuration (option list) keyword) null)
(defun localgroup-relay-exit (configuration exit mode)
  "Report EXIT's message and, for a controlling terminal, exit with its status.

Before exiting, publish the crash capsule from EXIT's details to this process's
launcher so recovery can restore the conversation. Read-only observers report
the message without publishing the capsule or exiting."
  (when exit
    (let ((message (getf exit :message)))
      (when message
        (format *error-output* "~&~A~%" message)
        (finish-output *error-output*)))
    (unless (eq mode ':read-only)
      (let ((capsule (getf (getf exit :details) :crash-capsule)))
        (when (non-empty-string-p capsule)
          (handler-case
              (application-publish-crash-pointer configuration (pathname capsule))
            (error (condition)
              (format *error-output* "~&Could not hand the crash capsule to recovery: ~A~%"
                      condition)))))
      (uiop:quit (getf exit :status))))
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
                      :message   "No reachable localgroup session is running."
                      :operation ':discover))
              ((rest records)
               (error 'localgroup-error
                      :message   (format nil "Multiple reachable localgroup sessions are running; provide a session identifier: ~{~A~^, ~}."
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
  "Return the localgroup status sub-command definition."
  (make-command
   :name        "status"
   :description "list detached Autolith sessions"
   :options     (list (make-option ':flag
                                   :long-name   "sexp"
                                   :key         ':sexp
                                   :description "print statuses as readable forms"))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message   "localgroup status accepts no arguments."
              :operation ':arguments))
     (localgroup--run-status
      :sexp-p (not (null (getopt command ':sexp)))))))

(-> localgroup--attach-command () clingon:command)
(defun localgroup--attach-command ()
  "Return the localgroup attach sub-command definition."
  (make-command
   :name        "attach"
   :description "attach this terminal to a detached session"
   :usage       "[SESSION-ID]"
   :options     (list (make-option ':flag
                                   :long-name   "read-only"
                                   :key         ':read-only
                                   :description "observe without control")
                      (make-option ':flag
                                   :long-name   "take-over"
                                   :key         ':take-over
                                   :description "take exclusive control"))
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (member (length arguments) '(0 1))
         (error 'localgroup-error
                :message   "localgroup attach accepts at most one session identifier."
                :operation ':arguments))
       (let ((read-only-p (not (null (getopt command ':read-only))))
             (take-over-p (not (null (getopt command ':take-over)))))
         (when (and read-only-p take-over-p)
           (error 'localgroup-error
                  :message   "Choose at most one of --read-only and --take-over."
                  :operation ':arguments))
         (let* ((configuration (localgroup--client-configuration))
                (session-id    (first arguments))
                (mode          (cond (read-only-p
                                      ':read-only)
                                     (take-over-p
                                      ':take-over)
                                     (t
                                      ':control)))
                (entry         (localgroup--session-record configuration session-id)))
           (localgroup-relay-exit
            configuration
            (localgroup-attach-record configuration entry mode)
            mode)))))))

(defparameter *localgroup-kill-exit-timeout* 2
  "Seconds to observe the selected PID after a successful localgroup kill response.")

(-> localgroup--observe-kill-exit (t) (member :dead :alive :unknown))
(defun localgroup--observe-kill-exit (pid)
  "Observe PID until absence or a monotonic deadline; return its last state.
PID liveness cannot establish process identity after PID reuse. Invalid PIDs
and inspection failures yield :UNKNOWN, never confirmed exit."
  (unless (typep pid '(integer 1 *))
    (return-from localgroup--observe-kill-exit ':unknown))
  (let ((deadline (+ (get-internal-real-time)
                     (* (max 0 *localgroup-kill-exit-timeout*)
                        internal-time-units-per-second))))
    (loop
      (let* ((state     (handler-case (ls-compat.posix:process-state pid)
                          (error () ':unknown)))
             (remaining (/ (- deadline (get-internal-real-time))
                           internal-time-units-per-second)))
        (when (or (eq state ':dead) (<= remaining 0))
          (return state))
        (sleep (min 0.05 remaining))))))

(-> localgroup--check-kill-exit (t) null)
(defun localgroup--check-kill-exit (pid)
  "Report incomplete or uncertain exit using only the selected record's PID."
  (case (localgroup--observe-kill-exit pid)
    (:dead
     nil)
    (:alive
     (error 'localgroup-error
            :operation ':kill
            :message   (format nil "Could not confirm session exit: recorded PID ~D is still present (possibly reused); its conversation may stay locked."
                               pid)))
    (otherwise
     (error 'localgroup-error
            :operation ':kill
            :message   (format nil "Could not confirm session exit for recorded PID ~S; its conversation may stay locked."
                               pid)))))

(-> localgroup--operation-command (string string keyword) clingon:command)
(defun localgroup--operation-command (name description operation)
  "Return a single-session localgroup OPERATION sub-command definition."
  (make-command
   :name        name
   :description description
   :usage       "[SESSION-ID]"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (member (length arguments) '(0 1))
         (error 'localgroup-error
                :message   (format nil "localgroup ~A accepts at most one session identifier." name)
                :operation ':arguments))
       (let* ((configuration (localgroup--client-configuration))
              (entry         (localgroup--session-record configuration (first arguments)))
              (pid           (getf (rest (rest entry)) :pid))
              (response      (localgroup-query-record entry operation)))
         (localgroup--print-response response *standard-output*)
         (when (eq operation ':kill)
           (if (localgroup--hosted-pid-p configuration pid)
               (localgroup--check-channel-closed (first entry))
               (localgroup--check-kill-exit pid))))))))

(-> localgroup--hosted-pid-p (configuration t) boolean)
(defun localgroup--hosted-pid-p (configuration pid)
  "Return true when PID is the world server's: the record names a channel it hosts, not a process of its own."
  (let ((world (world-client-entry configuration)))
    (and world (eql pid (getf (rest (rest world)) :pid)) t)))

(defparameter *localgroup-channel-close-seconds* 5
  "How long lg kill waits for a hosted channel to withdraw its endpoint.")

(-> localgroup--check-channel-closed (pathname) null)
(defun localgroup--check-channel-closed (pathname)
  "Report a hosted channel that still publishes its endpoint record PATHNAME after the wait.

The world server keeps running, so the channel's exit shows as its record
going away."
  (let ((deadline (+ (get-internal-real-time)
                     (* *localgroup-channel-close-seconds* internal-time-units-per-second))))
    (loop while (and (probe-file pathname) (< (get-internal-real-time) deadline))
          do (sleep 0.05))
    (when (probe-file pathname)
      (error 'localgroup-error
             :operation ':kill
             :message   "Could not confirm the channel closed: its endpoint is still published.")))
  nil)

(-> main-localgroup-command () clingon:command)
(defun main-localgroup-command ()
  "Return the localgroup command definition and its sub-commands."
  (make-command
   :name        "localgroup"
   :aliases     '("lg")
   :description "inspect and control detached Autolith sessions"
   :sub-commands
   (list (localgroup--status-command)
         (world-client--tell-command)
         (localgroup--attach-command)
         (localgroup--operation-command
          "detach" "detach a session from its terminal" ':detach)
         (localgroup--operation-command
          "pause" "pause a detached session" ':pause)
         (localgroup--operation-command
          "kill" "terminate a detached session" ':kill)
         (world-client--start-command)
         (world-client--stop-command))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message   (format nil "Unknown localgroup command ~S."
                                 (first (command-arguments command)))
              :operation ':arguments))
     (localgroup--run-status :sexp-p nil))))
