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

(-> localgroup-query-record
    (cons keyword &optional list)
    list)
(defun localgroup-query-record (entry operation &optional arguments)
  "Perform OPERATION against endpoint ENTRY, pruning a confirmed stale record."
  (let* ((pathname (first entry))
         (record (rest entry))
         (session-id (localgroup--record-session-id record))
         (response
           (handler-case
               (daemon-call
                (localgroup--record-port record)
                (localgroup--record-token record)
                operation
                arguments)
             (error (condition)
               (localgroup--remove-stale-record pathname record)
               (error 'localgroup-error
                       :message (format nil "Localgroup session ~A is unavailable."
                                        (session-identifier-display
                                         session-id))
                      :operation operation
                      :session-id session-id
                      :cause condition)))))
    (unless (and (localgroup--proper-list-p response)
                 (eq (first response) ':ok))
      (error 'localgroup-error
             :message (or (getf (rest response) :message)
                          "The localgroup endpoint rejected the request.")
             :operation operation
             :session-id session-id))
    response))

(-> localgroup--find-record (configuration string) cons)
(defun localgroup--find-record (configuration session-id)
  "Return CONFIGURATION's unique endpoint record matching SESSION-ID."
  (let* ((records (localgroup-endpoint-records configuration))
         (exact (remove-if-not
                 (lambda (entry)
                   (string= session-id (localgroup--record-session-id (rest entry))))
                 records))
         (normalized (handler-case (session-identifier-normalize session-id)
                       (localgroup-error () session-id)))
         (matches
           (or exact
               (remove-if-not
                (lambda (entry)
                  (string=
                   normalized
                   (handler-case
                       (session-identifier-normalize
                        (localgroup--record-session-id (rest entry)))
                     (localgroup-error ()
                       (localgroup--record-session-id (rest entry))))))
                records))))
    (cond ((null matches)
           (error 'localgroup-error
                  :message (format nil "No localgroup session named ~A is running."
                                   (session-identifier-display session-id))
                  :operation ':discover
                  :session-id session-id))
          ((rest matches)
           (error 'localgroup-error
                  :message (format nil "More than one localgroup session named ~A is registered."
                                   (session-identifier-display session-id))
                  :operation ':discover
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
    (:idle ':success)
    (:failed ':failure)
    ((:paused :cancelling :detaching) ':notice)
    ((:active :working) ':brand)
    (otherwise ':dim)))

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
  (let* ((values (rest status))
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
    (:session "SESSION")
    (:pid "PID")
    (:state "STATE")
    (:started "STARTED")
    (:conversation "CONVERSATION")
    (:activity "ACTIVITY")
    (:workspace "WORKSPACE")))

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
      (:state (localgroup--status-state-text status))
      (:started (localgroup--status-started-text status))
      (:conversation (or (getf values :conversation-title)
                         (getf values :conversation-display-id)))
      (:activity (localgroup--status-activity-text status))
      (:workspace (getf values :cwd)))))

(-> localgroup--status-field-style (list keyword) terminal-style)
(defun localgroup--status-field-style (status field)
  "Return the semantic style for STATUS's FIELD."
  (case field
    (:session ':code)
    (:pid ':code)
    (:state (localgroup--status-state-style status))
    (:started ':timestamp-time)
    (:conversation
     (if (getf (rest status) :conversation-title) ':plain ':code))
    (:activity ':dim)
    (:workspace ':plain)))

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
        (t nil)))

(-> localgroup--status-field-minimum-width (keyword) integer)
(defun localgroup--status-field-minimum-width (field)
  "Return the minimum useful cell width for FIELD."
  (ecase field
    (:session 8)
    (:pid 5)
    (:state 8)
    (:started 17)
    (:conversation 8)
    (:activity 12)
    (:workspace 14)))

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

(-> localgroup--status-box-border (character character integer) terminal-styled-text)
(defun localgroup--status-box-border (left right inner-width)
  "Return one semantic outer border spanning INNER-WIDTH content cells."
  (list
   (terminal-span
    ':dim
    (concatenate 'string
                 (string left)
                 (make-string inner-width :initial-element #\Box_Drawings_Light_Horizontal)
                 (string right)))))

(-> localgroup--status-table-border (list keyword) terminal-styled-text)
(defun localgroup--status-table-border (widths position)
  "Return the semantic table border at POSITION across cell WIDTHS."
  (multiple-value-bind (left junction right)
      (ecase position
        (:top
         (values #\Box_Drawings_Light_Vertical_And_Right
                 #\Box_Drawings_Light_Down_And_Horizontal
                 #\Box_Drawings_Light_Vertical_And_Left))
        (:middle
         (values #\Box_Drawings_Light_Vertical_And_Right
                 #\Box_Drawings_Light_Vertical_And_Horizontal
                 #\Box_Drawings_Light_Vertical_And_Left))
        (:bottom
         (values #\Box_Drawings_Light_Up_And_Right
                 #\Box_Drawings_Light_Up_And_Horizontal
                 #\Box_Drawings_Light_Up_And_Left)))
    (list
     (terminal-span
      ':dim
      (with-output-to-string (stream)
        (write-char left stream)
        (loop for width in widths
              for remaining on widths
              do (write-string
                  (make-string width
                               :initial-element #\Box_Drawings_Light_Horizontal)
                  stream)
                 (when (rest remaining)
                   (write-char junction stream)))
        (write-char right stream))))))

(-> localgroup--status-table-row
    (list list list &key (:header-p boolean))
    terminal-styled-text)
(defun localgroup--status-table-row (status fields widths &key header-p)
  "Return one bordered STATUS row across FIELDS and WIDTHS.

HEADER-P renders field labels rather than status values."
  (append
   (list (terminal-span ':dim (string #\Box_Drawings_Light_Vertical)))
   (loop for field in fields
         for width in widths
         for remaining on fields
         append
         (list
          (terminal-span
           (if header-p ':strong (localgroup--status-field-style status field))
           (layout-fit-text
            (if header-p
                (localgroup--status-field-label field)
                (localgroup--status-field-text status field))
            width))
          (terminal-span
           ':dim
           (string (if (rest remaining)
                       #\Box_Drawings_Light_Vertical
                       #\Box_Drawings_Light_Vertical)))))))

(-> localgroup--status-card-row (list integer) terminal-styled-text)
(defun localgroup--status-card-row (status inner-width)
  "Return compact bordered rows for STATUS within INNER-WIDTH cells."
  (let* ((identifier (localgroup--status-field-text status ':session))
         (state (localgroup--status-field-text status ':state))
         (session-width
           (min (text-cell-width identifier)
                (max 1 (- inner-width (text-cell-width state) 2))))
         (state-width (max 0 (- inner-width session-width 2)))
         (detail
           (format nil "~A  ~A"
                   (localgroup--status-field-text status ':started)
                   (localgroup--status-field-text status ':activity)))
         (conversation (localgroup--status-field-text status ':conversation)))
    (append
     (list (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':code (layout-fit-text identifier session-width))
           (terminal-span ':plain "  ")
           (terminal-span (localgroup--status-state-style status)
                          (layout-fit-text state state-width))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':plain (string #\Newline))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':timestamp-time (layout-fit-text detail inner-width))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':plain (string #\Newline))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span (localgroup--status-field-style status ':conversation)
                          (layout-fit-text conversation inner-width))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':plain (string #\Newline))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
           (terminal-span ':plain
                          (layout-fit-text
                           (localgroup--status-field-text status ':workspace)
                           inner-width))
           (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))))))

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
             (inner-width (- columns 2))
             (fields (localgroup--status-table-fields columns))
             (title "Local Autolith sessions"))
        (localgroup--write-styled-line
         stream
         (localgroup--status-box-border #\Box_Drawings_Light_Down_And_Right
                                        #\Box_Drawings_Light_Down_And_Left
                                        inner-width)
         styled-p)
        (localgroup--write-styled-line
         stream
         (list (terminal-span ':dim (string #\Box_Drawings_Light_Vertical))
               (terminal-span ':brand (layout-fit-text title inner-width))
               (terminal-span ':dim (string #\Box_Drawings_Light_Vertical)))
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
                      rows inner-width
                      :gap-width 1
                      :minimum-widths
                      (mapcar #'localgroup--status-field-minimum-width fields)
                      :fill-p t)))
              (localgroup--write-styled-line
               stream
               (localgroup--status-table-border widths ':top)
               styled-p)
              (localgroup--write-styled-line
               stream (localgroup--status-table-row nil fields widths :header-p t)
               styled-p)
              (localgroup--write-styled-line
               stream
               (localgroup--status-table-border widths ':middle)
               styled-p)
              (loop for status in statuses
                    for remaining on statuses
                    do (localgroup--write-styled-line
                        stream (localgroup--status-table-row status fields widths)
                        styled-p)
                       (when (rest remaining)
                         (localgroup--write-styled-line
                          stream
                          (localgroup--status-table-border widths ':middle)
                          styled-p)))
              (localgroup--write-styled-line
               stream
               (localgroup--status-table-border widths ':bottom)
               styled-p))
            (progn
              (loop for status in statuses
                    for remaining on statuses
                    do (localgroup--write-styled-line
                        stream (localgroup--status-card-row status inner-width)
                        styled-p)
                       (when (rest remaining)
                         (localgroup--write-styled-line
                          stream
                          (localgroup--status-box-border
                           #\Box_Drawings_Light_Vertical_And_Right
                           #\Box_Drawings_Light_Vertical_And_Left
                           inner-width)
                          styled-p)))
              (localgroup--write-styled-line
               stream
               (localgroup--status-box-border #\Box_Drawings_Light_Up_And_Right
                                              #\Box_Drawings_Light_Up_And_Left
                                              inner-width)
               styled-p)))))
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

(defun localgroup--attach-terminal-loop (socket-stream terminal mode &key socket)
  "Run the generic attachment loop with Autolith terminal callbacks."
  (image-daemon:daemon-attach-client-run socket-stream :mode mode :input-ready-function
   (lambda () (terminal-input-ready-p terminal)) :read-event-function
   (lambda () (terminal-read-event terminal)) :resize-function
   (lambda ()
     (when *terminal-resize-pending-p*
       (setf *terminal-resize-pending-p* nil)
       (multiple-value-bind (rows columns)
           (terminal-current-size)
         (list :rows rows :columns columns :styled-p (terminal-environment-styling-p)))))
   :socket socket))

(-> localgroup--wait-for-handoff-entry
    (configuration string string integer)
    cons)

(defun localgroup--wait-for-handoff-entry (configuration session-id token old-pid)
  "Wait for SESSION-ID's authenticated replacement endpoint after OLD-PID."
  (let ((deadline
         (+ (get-internal-real-time)
            (* *localgroup-handoff-start-timeout-seconds*
               internal-time-units-per-second))))
    (loop
     (let* ((pathname (localgroup-registry-pathname configuration session-id))
            (record (image-daemon:daemon-registry-read pathname)))
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

(defun localgroup-attach-record (configuration entry mode)
  "Attach the current interactive terminal to endpoint ENTRY with MODE."
  (let* ((record (rest entry))
         (socket nil)
         (socket-stream nil)
         (terminal nil)
         (resize-watch nil))
    (unwind-protect
        (progn
         (multiple-value-bind (rows columns)
             (terminal-current-size)
           (setf terminal
                 (stream-terminal-create
                  :rows rows
                  :columns columns
                  :input-file-descriptor (terminal-standard-input-file-descriptor))))
         (unless (terminal--terminal-mode-or-nil terminal)
           (error 'localgroup-error :message
                  "localgroup attach requires an interactive terminal." :operation
                  ':attach :session-id (localgroup--record-session-id record)))
         (multiple-value-setq (socket socket-stream)
           (daemon-connect (localgroup--record-port record)))
         (daemon-write-packet socket-stream
                              (list :localgroup-request :version
                                    *daemon-protocol-version* :token
                                    (localgroup--record-token record) :operation ':attach
                                    :arguments
                                    (list :mode mode :rows (terminal-rows terminal)
                                          :columns (terminal-columns terminal) :styled-p
                                          (terminal-environment-styling-p))))
         (let ((response (daemon-read-response socket-stream ':attach)))
           (cond
            ((and response (eq (first response) ':handoff))
             (let ((session-id (getf (rest response) :session-id))
                   (old-pid (getf (rest response) :old-pid))
                   (token (localgroup--record-token record)))
               (unless (and (non-empty-string-p session-id) (typep old-pid '(integer 1)))
                 (error 'localgroup-error :message
                        "The localgroup handoff response was malformed." :operation
                        ':attach :session-id (localgroup--record-session-id record)))
               (close socket-stream)
               (setf socket-stream nil
                     socket nil)
               (return-from localgroup-attach-record
                 (localgroup-attach-record configuration
                                           (localgroup--wait-for-handoff-entry
                                            configuration session-id token old-pid)
                                           ':control))))
            ((and response (eq (first response) ':attached))
             (let ((history (getf (rest response) :history)))
               (when (stringp history)
                 (write-string history *standard-output*)
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
         (localgroup--attach-terminal-loop socket-stream terminal mode :socket socket))
      (when resize-watch
        (platform-unwatch-terminal-resize *platform* resize-watch))
      (when terminal (ignore-errors (terminal-stop terminal)))
      (when socket-stream (ignore-errors (close socket-stream)))
      (when (and socket (null socket-stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)


;;;; -- Localgroup Command Entry --

(-> localgroup--client-configuration () configuration)
(defun localgroup--client-configuration ()
  "Return a directory-ensured configuration for one localgroup client command."
  (let ((configuration (configuration-create :defer-provider-validation-p t)))
    (configuration-ensure-directories configuration)
    (localgroup-reconcile-endpoint-records configuration)
    configuration))

(-> localgroup--run-status (&key (:sexp-p boolean)) null)
(defun localgroup--run-status (&key sexp-p)
  "Print localgroup session statuses, as readable forms when SEXP-P."
  (let ((statuses (localgroup-statuses (localgroup--client-configuration))))
    (if sexp-p
        (dolist (status statuses)
          (write status :stream *standard-output* :readably t)
          (terpri))
        (localgroup-print-statuses statuses)))
  nil)

(-> localgroup--required-session-id (clingon:command) string)
(defun localgroup--required-session-id (command)
  "Return COMMAND's single required session identifier argument."
  (let ((arguments (command-arguments command)))
    (unless (and (= (length arguments) 1)
                 (non-empty-string-p (first arguments)))
      (error 'localgroup-error
             :message (format nil "localgroup ~A requires a session identifier."
                              (command-name command))
             :operation ':arguments))
    (first arguments)))

(-> localgroup--status-command () clingon:command)
(defun localgroup--status-command ()
  "Return the localgroup status sub-command definition."
  (make-command
   :name "status"
   :description "list detached Autolith sessions"
   :options (list (make-option ':flag
                               :long-name "sexp"
                               :key ':sexp
                               :description "print statuses as readable forms"))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message "localgroup status accepts no arguments."
              :operation ':arguments))
     (localgroup--run-status
      :sexp-p (not (null (getopt command ':sexp)))))))

(-> localgroup--tell-command () clingon:command)
(defun localgroup--tell-command ()
  "Return the localgroup tell sub-command definition."
  (make-command
   :name "tell"
   :description "queue one message for a detached session"
   :usage "SESSION-ID MESSAGE"
   :handler
   (lambda (command)
     (let ((arguments (command-arguments command)))
       (unless (= (length arguments) 2)
         (error 'localgroup-error
                :message "localgroup tell requires a session identifier and exactly one quoted message argument."
                :operation ':arguments))
       (let* ((configuration (localgroup--client-configuration))
              (entry (localgroup--find-record configuration
                                              (first arguments))))
         (localgroup--print-response
          (localgroup-query-record entry ':tell
                                   (list :message (second arguments)))
          *standard-output*))))))

(-> localgroup--attach-command () clingon:command)
(defun localgroup--attach-command ()
  "Return the localgroup attach sub-command definition."
  (make-command
   :name "attach"
   :description "attach this terminal to a detached session"
   :usage "SESSION-ID"
   :options (list (make-option ':flag
                               :long-name "read-only"
                               :key ':read-only
                               :description "observe without control")
                  (make-option ':flag
                               :long-name "take-over"
                               :key ':take-over
                               :description "take exclusive control"))
   :handler
   (lambda (command)
     (let ((session-id (localgroup--required-session-id command))
           (read-only-p (not (null (getopt command ':read-only))))
           (take-over-p (not (null (getopt command ':take-over)))))
       (when (and read-only-p take-over-p)
         (error 'localgroup-error
                :message "Choose at most one of --read-only and --take-over."
                :operation ':arguments))
       (let ((configuration (localgroup--client-configuration)))
         (localgroup-attach-record
          configuration
          (localgroup--find-record configuration session-id)
          (cond (read-only-p ':read-only)
                (take-over-p ':take-over)
                (t ':control))))))))

(-> localgroup--operation-command (string string keyword) clingon:command)
(defun localgroup--operation-command (name description operation)
  "Return one single-session localgroup OPERATION sub-command definition."
  (make-command
   :name name
   :description description
   :usage "SESSION-ID"
   :handler
   (lambda (command)
     (let* ((session-id (localgroup--required-session-id command))
            (configuration (localgroup--client-configuration))
            (entry (localgroup--find-record configuration session-id)))
       (localgroup--print-response
        (localgroup-query-record entry operation)
        *standard-output*)))))

(-> main-localgroup-command () clingon:command)
(defun main-localgroup-command ()
  "Return the localgroup command definition and its sub-commands."
  (make-command
   :name "localgroup"
   :description "inspect and control detached Autolith sessions"
   :sub-commands
   (list (localgroup--status-command)
         (localgroup--tell-command)
         (localgroup--attach-command)
         (localgroup--operation-command
          "detach" "detach a session from its terminal" ':detach)
         (localgroup--operation-command
          "pause" "pause a detached session" ':pause)
         (localgroup--operation-command
          "kill" "terminate a detached session" ':kill))
   :handler
   (lambda (command)
     (when (command-arguments command)
       (error 'localgroup-error
              :message (format nil "Unknown localgroup command ~S."
                               (first (command-arguments command)))
              :operation ':arguments))
     (localgroup--run-status :sexp-p nil))))
