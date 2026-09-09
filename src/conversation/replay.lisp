(in-package #:autolith)

;;;; -- Read-only Conversation Replay --

(defclass conversation-replay-record ()
  ((record
    :initarg :record
    :reader conversation-replay-record-record
    :type list
    :documentation "The exact durable conversation record.")
   (turn
    :initarg :turn
    :reader conversation-replay-record-turn
    :type (integer 0)
    :documentation "The user-turn number active when the record was written."))
  (:documentation "One durable record annotated for read-only replay navigation."))

(defclass conversation-replay-session ()
  ((conversation
    :initarg :conversation
    :reader conversation-replay-session-conversation
    :type conversation
    :documentation "The conversation being inspected without a lease.")
   (records
    :initarg :records
    :reader conversation-replay-session-records
    :type vector
    :documentation "The chronological replay records.")
   (position
    :initarg :position
    :initform 0
    :accessor conversation-replay-session-position
    :type (integer 0)
    :documentation "The zero-based index of the selected record."))
  (:documentation "Navigation state for one read-only conversation replay."))

(-> conversation-replay--user-turn-record-p (list) boolean)
(defun conversation-replay--user-turn-record-p (record)
  "Return true when RECORD begins one ordinary durable user turn."
  (and (eq (first record) ':message)
       (eq (getf (rest record) :role) ':user)
       (not (getf (rest record) :automatic-p))
       t))

(-> conversation-replay--project-provider-item (list) (option list))
(defun conversation-replay--project-provider-item (record)
  "Return the bounded replay projection of provider-item RECORD."
  (let ((wire-json (getf (rest record) :wire-json))
        (sequence (getf (rest record) :seq))
        (time (getf (rest record) :time)))
    (handler-case
        (let* ((item (and (stringp wire-json) (json-decode wire-json)))
               (type (and (json-object-p item) (json-get item "type"))))
          (cond
            ((and (string= (or type "") "message")
                  (string= (or (json-get item "role") "") "assistant"))
             (let ((content (response-item-assistant-text item)))
               (and content
                    (list ':assistant :seq sequence :time time
                          :content content))))
            ((string= (or type "") "reasoning")
             (let ((summary (response-item-reasoning-summary item)))
               (and summary
                    (list ':reasoning :seq sequence :time time
                          :content summary))))
            ((string= (or type "") "function_call")
             (list ':tool-call :seq sequence :time time
                   :tool (function-call-canonical-name item)
                   :arguments (or (json-get item "arguments") "")))
            ((string= (or type "") "web_search_call")
             (list ':web-search :seq sequence :time time
                   :detail (web-search-call-detail item)))
            (t
             nil)))
      (error ()
        nil))))

(-> conversation-replay--project-record (list) (option list))
(defun conversation-replay--project-record (record)
  "Return RECORD's bounded replay form, or NIL when it has no replay event."
  (case (first record)
    (:message
     (list ':message
           :seq (getf (rest record) :seq)
           :time (getf (rest record) :time)
           :role (getf (rest record) :role)
           :automatic-p (getf (rest record) :automatic-p)
           :content (getf (rest record) :content)))
    (:provider-item
     (conversation-replay--project-provider-item record))
    (:tool-result
     (list ':tool-result
           :seq (getf (rest record) :seq)
           :time (getf (rest record) :time)
           :tool (getf (rest record) :tool)
           :status (getf (rest record) :status)
           :output (getf (rest record) :output)))
    (:turn-aborted
     (list ':turn-aborted
           :seq (getf (rest record) :seq)
           :time (getf (rest record) :time)
           :turn-start-seq (getf (rest record) :turn-start-seq)
           :last-complete-seq (getf (rest record) :last-complete-seq)
           :reason (getf (rest record) :reason)
           :condition-type (getf (rest record) :condition-type)
           :message (getf (rest record) :message)
           :request-number (getf (rest record) :request-number)))
    ((:provider :native-compaction)
     nil)
    (otherwise
     (copy-tree record))))

(-> conversation-replay--map-records (conversation function) null)
(defun conversation-replay--map-records (conversation function)
  "Call FUNCTION on every complete form in CONVERSATION's replay segments.

The replay reader deliberately exposes durable gaps instead of rejecting the
whole history when a crash or retention boundary interrupted sequencing."
  (dolist (pathname
           (conversation-storage-pathnames
            (conversation-pathname conversation)))
    (conversation--map-records
     pathname
     (lambda (record)
       (unless (eq (first record) ':conversation)
         (funcall function record)))))
  nil)

(-> conversation-replay-load (configuration string) conversation)
(defun conversation-replay-load (configuration identifier)
  "Load IDENTIFIER's newest header without replaying or repairing its state."
  (let* ((identity (conversation-pathname-for-id configuration identifier))
         (active (conversation-storage-active-pathname identity)))
    (unless active
      (error 'conversation-error
             :message (format nil "Conversation ~A does not exist." identifier)
             :pathname identity
             :sequence nil))
    (let ((conversation
            (conversation--from-header
             identity active (conversation-peek-header active))))
      (setf (conversation-log-pathname conversation) active)
      conversation)))

(-> conversation-replay-create (conversation) conversation-replay-session)
(defun conversation-replay-create (conversation)
  "Create bounded read-only navigation state from CONVERSATION's records."
  (let ((records nil)
        (turn 0))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (conversation-replay--user-turn-record-p record)
         (incf turn))
       (let ((projected (conversation-replay--project-record record)))
         (when projected
           (push (make-instance 'conversation-replay-record
                                :record projected
                                :turn turn)
                 records)))))
    (unless records
      (error 'conversation-error
             :message "The conversation contains no replayable records."
             :pathname (conversation-pathname conversation)
             :sequence nil))
    (make-instance 'conversation-replay-session
                   :conversation conversation
                   :records (coerce (nreverse records) 'vector))))

(-> conversation-replay--current-record
    (conversation-replay-session)
    conversation-replay-record)
(defun conversation-replay--current-record (session)
  "Return SESSION's selected replay record."
  (aref (conversation-replay-session-records session)
        (conversation-replay-session-position session)))

(-> conversation-replay--record-time (conversation-replay-record)
    (option timestamp))
(defun conversation-replay--record-time (entry)
  "Return ENTRY's durable universal time, when valid."
  (let ((time (getf (rest (conversation-replay-record-record entry)) :time)))
    (and (typep time '(integer 0)) time)))

(-> conversation-replay--record-sequence (conversation-replay-record)
    (option integer))
(defun conversation-replay--record-sequence (entry)
  "Return ENTRY's durable sequence, when valid."
  (let ((sequence
          (getf (rest (conversation-replay-record-record entry)) :seq)))
    (and (typep sequence '(integer 0)) sequence)))

(-> conversation-replay--timestamp-string ((option timestamp)) string)
(defun conversation-replay--timestamp-string (universal-time)
  "Return UNIVERSAL-TIME as a local replay timestamp."
  (if universal-time
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time universal-time)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month date hour minute second))
      "unknown time"))

(-> conversation-replay--record-kind (list) string)
(defun conversation-replay--record-kind (record)
  "Return a compact readable kind for replay RECORD."
  (case (first record)
    (:message
     (format nil "message/~(~A~)" (getf (rest record) :role)))
    (otherwise
     (string-downcase (symbol-name (first record))))))

(-> conversation-replay--write-record-body (list stream) null)
(defun conversation-replay--write-record-body (record stream)
  "Write the readable body of replay RECORD to STREAM."
  (case (first record)
    ((:message :assistant :reasoning)
     (format stream "~A~%" (or (getf (rest record) :content) "")))
    (:tool-call
     (format stream "tool ~A~%~A~%"
             (or (getf (rest record) :tool) "unknown")
             (or (getf (rest record) :arguments) "")))
    (:web-search
     (format stream "web search~@[ ~A~]~%"
             (getf (rest record) :detail)))
    (:tool-result
     (format stream "tool ~A [~(~A~)]~%~A~%"
             (or (getf (rest record) :tool) "unknown")
             (or (getf (rest record) :status) ':unknown)
             (or (getf (rest record) :output) "")))
    (:summary
     (format stream "context compacted through sequence ~A~%~A~%"
             (getf (rest record) :through-seq)
             (or (getf (rest record) :content) "")))
    (:user-operation
     (format stream "~A~%~@[~A~%~]"
             (or (getf (rest record) :source) "")
             (getf (rest record) :result)))
    (:turn-aborted
     (format stream
             "durable prefix ~D-~D aborted [~(~A~)]~@[ at provider request ~D~]~%~A: ~A~%"
             (getf (rest record) :turn-start-seq)
             (getf (rest record) :last-complete-seq)
             (getf (rest record) :reason)
             (getf (rest record) :request-number)
             (getf (rest record) :condition-type)
             (getf (rest record) :message)))
    (otherwise
     (format stream "~S~%" record)))
  nil)

(-> conversation-replay-write-current
    (conversation-replay-session stream &key (:raw-p boolean))
    null)
(defun conversation-replay-write-current (session stream &key raw-p)
  "Write SESSION's selected record, optionally as its projected S-expression."
  (let* ((entry (conversation-replay--current-record session))
         (record (conversation-replay-record-record entry))
         (sequence (conversation-replay--record-sequence entry))
         (time (conversation-replay--record-time entry)))
    (format stream "~&[turn ~D | sequence ~A | ~A | ~A]~%"
            (conversation-replay-record-turn entry)
            (or sequence "?")
            (conversation-replay--timestamp-string time)
            (conversation-replay--record-kind record))
    (if raw-p
        (format stream "~S~%" record)
        (conversation-replay--write-record-body record stream))
    (force-output stream))
  nil)

(-> conversation-replay--parse-positive-count ((option string)) (integer 1))
(defun conversation-replay--parse-positive-count (text)
  "Return TEXT as a positive navigation count, defaulting to one."
  (if (null text)
      1
      (handler-case
          (let ((count (parse-integer text :junk-allowed nil)))
            (unless (plusp count)
              (error 'configuration-error
                     :message "A replay movement count must be positive."))
            count)
        (parse-error ()
          (error 'configuration-error
                 :message (format nil "Invalid replay movement count ~S." text))))))

(-> conversation-replay-move (conversation-replay-session integer)
    conversation-replay-record)
(defun conversation-replay-move (session delta)
  "Move SESSION by signed record DELTA, clamped to its durable range."
  (let* ((records (conversation-replay-session-records session))
         (maximum (1- (length records))))
    (setf (conversation-replay-session-position session)
          (min maximum
               (max 0 (+ (conversation-replay-session-position session) delta))))
    (conversation-replay--current-record session)))

(-> conversation-replay--select-index
    (conversation-replay-session function string)
    conversation-replay-record)
(defun conversation-replay--select-index (session predicate description)
  "Select SESSION's first record satisfying PREDICATE or reject DESCRIPTION."
  (let* ((records (conversation-replay-session-records session))
         (position (position-if predicate records)))
    (unless position
      (error 'configuration-error
             :message (format nil "No replay record matches ~A." description)))
    (setf (conversation-replay-session-position session) position)
    (aref records position)))

(-> conversation-replay-select-turn (conversation-replay-session integer)
    conversation-replay-record)
(defun conversation-replay-select-turn (session turn)
  "Select the first record in user TURN."
  (unless (typep turn '(integer 0))
    (error 'configuration-error
           :message "A replay turn must be a nonnegative integer."))
  (conversation-replay--select-index
   session
   (lambda (entry)
     (= (conversation-replay-record-turn entry) turn))
   (format nil "turn ~D" turn)))

(-> conversation-replay-select-sequence (conversation-replay-session integer)
    conversation-replay-record)
(defun conversation-replay-select-sequence (session sequence)
  "Select the first record at or after durable SEQUENCE."
  (unless (typep sequence '(integer 0))
    (error 'configuration-error
           :message "A replay sequence must be a nonnegative integer."))
  (conversation-replay--select-index
   session
   (lambda (entry)
     (let ((candidate (conversation-replay--record-sequence entry)))
       (and candidate (>= candidate sequence))))
   (format nil "sequence ~D" sequence)))

(-> conversation-replay--parse-date (string) (values integer integer integer))
(defun conversation-replay--parse-date (text)
  "Parse local calendar date TEXT and return year, month, and day."
  (handler-case
      (progn
        (unless (and (= (length text) 10)
                     (char= (char text 4) #\-)
                     (char= (char text 7) #\-))
          (error "bad date"))
        (let* ((year (parse-integer text :start 0 :end 4 :junk-allowed nil))
               (month (parse-integer text :start 5 :end 7 :junk-allowed nil))
               (day (parse-integer text :start 8 :end 10 :junk-allowed nil))
               (time (encode-universal-time 0 0 0 day month year)))
          (multiple-value-bind (second minute hour decoded-day decoded-month decoded-year)
              (decode-universal-time time)
            (declare (ignore second minute hour))
            (unless (and (= year decoded-year)
                         (= month decoded-month)
                         (= day decoded-day))
              (error "bad date")))
          (values year month day)))
    (error ()
      (error 'configuration-error
             :message (format nil "Invalid local replay date ~S; use YYYY-MM-DD."
                              text)))))

(-> conversation-replay-select-date (conversation-replay-session string)
    conversation-replay-record)
(defun conversation-replay-select-date (session text)
  "Select SESSION's first record on local calendar date TEXT."
  (multiple-value-bind (year month day)
      (conversation-replay--parse-date text)
    (conversation-replay--select-index
     session
     (lambda (entry)
       (let ((time (conversation-replay--record-time entry)))
         (when time
           (multiple-value-bind (second minute hour candidate-day candidate-month
                                 candidate-year)
               (decode-universal-time time)
             (declare (ignore second minute hour))
             (and (= year candidate-year)
                  (= month candidate-month)
                  (= day candidate-day))))))
     (format nil "date ~A" text))))

(-> conversation-replay--parse-clock (string) (values integer integer integer))
(defun conversation-replay--parse-clock (text)
  "Parse local clock TEXT and return hour, minute, and second."
  (handler-case
      (let* ((parts (uiop:split-string text :separator '(#\:)))
             (hour (parse-integer (first parts) :junk-allowed nil))
             (minute (parse-integer (second parts) :junk-allowed nil))
             (second (if (third parts)
                         (parse-integer (third parts) :junk-allowed nil)
                         0)))
        (unless (and (<= 2 (length parts) 3)
                     (<= 0 hour 23)
                     (<= 0 minute 59)
                     (<= 0 second 59))
          (error "bad clock"))
        (values hour minute second))
    (error ()
      (error 'configuration-error
             :message (format nil "Invalid local replay time ~S." text)))))

(-> conversation-replay--local-date-time (string) (option timestamp))
(defun conversation-replay--local-date-time (text)
  "Return local date-time TEXT as universal time, or NIL when its shape differs."
  (when (>= (length text) 16)
    (let ((separator (char text 10)))
      (when (member separator '(#\T #\t #\Space))
        (multiple-value-bind (year month day)
            (conversation-replay--parse-date (subseq text 0 10))
          (multiple-value-bind (hour minute second)
              (conversation-replay--parse-clock (subseq text 11))
            (encode-universal-time second minute hour day month year)))))))

(-> conversation-replay-select-time (conversation-replay-session string)
    conversation-replay-record)
(defun conversation-replay-select-time (session text)
  "Select the first replay record at or after local or RFC 3339 time TEXT.

A bare HH:MM[:SS] value uses the selected record's local date."
  (let* ((current-time
           (conversation-replay--record-time
            (conversation-replay--current-record session)))
         (explicit-time
           (or (rfc3339->universal-time text)
               (conversation-replay--local-date-time text)))
         (target-time
           (or explicit-time
               (multiple-value-bind (hour minute second)
                   (conversation-replay--parse-clock text)
                 (unless current-time
                   (error 'configuration-error
                          :message "The selected replay record has no timestamp."))
                 (multiple-value-bind
                     (ignored-second ignored-minute ignored-hour day month year)
                     (decode-universal-time current-time)
                   (declare (ignore ignored-second ignored-minute ignored-hour))
                   (encode-universal-time second minute hour day month year))))))
    (conversation-replay--select-index
     session
     (lambda (entry)
       (let ((candidate (conversation-replay--record-time entry)))
         (and candidate (>= candidate target-time))))
     (format nil "time ~A" text))))

(-> conversation-replay-apply-selection
    (conversation-replay-session list)
    conversation-replay-record)
(defun conversation-replay-apply-selection (session arguments)
  "Apply optional CLI selector ARGUMENTS to SESSION and return its current record."
  (when arguments
    (unless (= (length arguments) 2)
      (error 'configuration-error
             :message
             "Replay selection must be turn N, date YYYY-MM-DD, time TIME, or sequence N."))
    (let ((kind (string-downcase (first arguments)))
          (value (second arguments)))
      (cond
        ((string= kind "turn")
         (conversation-replay-select-turn
          session (parse-integer value :junk-allowed nil)))
        ((string= kind "date")
         (conversation-replay-select-date session value))
        ((string= kind "time")
         (conversation-replay-select-time session value))
        ((member kind '("sequence" "record") :test #'string=)
         (conversation-replay-select-sequence
          session (parse-integer value :junk-allowed nil)))
        (t
         (error 'configuration-error
                :message (format nil "Unknown replay selector ~S." kind))))))
  (conversation-replay--current-record session))

(-> conversation-replay-write-location
    (conversation-replay-session stream)
    null)
(defun conversation-replay-write-location (session stream)
  "Write SESSION's current navigation location to STREAM."
  (let* ((records (conversation-replay-session-records session))
         (entry (conversation-replay--current-record session)))
    (format stream
            "record ~D of ~D, turn ~D, sequence ~A, ~A~%"
            (1+ (conversation-replay-session-position session))
            (length records)
            (conversation-replay-record-turn entry)
            (or (conversation-replay--record-sequence entry) "?")
            (conversation-replay--timestamp-string
             (conversation-replay--record-time entry)))
    (force-output stream))
  nil)

(-> conversation-replay-write-help (stream) null)
(defun conversation-replay-write-help (stream)
  "Write the replay debugger command reference to STREAM."
  (format stream
          "~&Replay commands:~%
  next [N], previous [N]  step durable records~%
  turn N                  jump to a user turn~%
  sequence N              jump to a durable sequence~%
  date YYYY-MM-DD          jump to a local date~%
  time TIME               jump to local or RFC 3339 time~%
  first, last              jump to either boundary~%
  print, raw, where        inspect the selected record~%
  help, quit               show this text or leave replay~%")
  (force-output stream)
  nil)

(-> conversation-replay--command-arguments (string) list)
(defun conversation-replay--command-arguments (line)
  "Return whitespace-separated replay command words from LINE."
  (remove ""
          (uiop:split-string line
                             :separator '(#\Space #\Tab #\Newline #\Return))
          :test #'string=))

(-> conversation-replay-execute-command
    (conversation-replay-session string stream)
    boolean)
(defun conversation-replay-execute-command (session line stream)
  "Execute replay debugger LINE and return true when the session should continue."
  (let* ((arguments (conversation-replay--command-arguments line))
         (command (string-downcase (or (first arguments) "next")))
         (value (second arguments)))
    (labels ((reject-extra-arguments (maximum)
               "Reject command words beyond MAXIMUM."
               (when (> (length arguments) maximum)
                 (error 'configuration-error
                        :message (format nil "Replay command ~A received too many arguments."
                                         command)))))
      (cond
        ((member command '("q" "quit" "exit") :test #'string=)
         (reject-extra-arguments 1)
         nil)
        ((member command '("n" "next") :test #'string=)
         (reject-extra-arguments 2)
         (conversation-replay-move
          session (conversation-replay--parse-positive-count value))
         (conversation-replay-write-current session stream)
         t)
        ((member command '("p" "previous" "prev") :test #'string=)
         (reject-extra-arguments 2)
         (conversation-replay-move
          session (- (conversation-replay--parse-positive-count value)))
         (conversation-replay-write-current session stream)
         t)
        ((string= command "turn")
         (reject-extra-arguments 2)
         (conversation-replay-select-turn
          session (parse-integer value :junk-allowed nil))
         (conversation-replay-write-current session stream)
         t)
        ((member command '("sequence" "record") :test #'string=)
         (reject-extra-arguments 2)
         (conversation-replay-select-sequence
          session (parse-integer value :junk-allowed nil))
         (conversation-replay-write-current session stream)
         t)
        ((string= command "date")
         (reject-extra-arguments 2)
         (conversation-replay-select-date session value)
         (conversation-replay-write-current session stream)
         t)
        ((string= command "time")
         (reject-extra-arguments 2)
         (conversation-replay-select-time session value)
         (conversation-replay-write-current session stream)
         t)
        ((string= command "first")
         (reject-extra-arguments 1)
         (setf (conversation-replay-session-position session) 0)
         (conversation-replay-write-current session stream)
         t)
        ((string= command "last")
         (reject-extra-arguments 1)
         (setf (conversation-replay-session-position session)
               (1- (length (conversation-replay-session-records session))))
         (conversation-replay-write-current session stream)
         t)
        ((member command '("print" "show") :test #'string=)
         (reject-extra-arguments 1)
         (conversation-replay-write-current session stream)
         t)
        ((string= command "raw")
         (reject-extra-arguments 1)
         (conversation-replay-write-current session stream :raw-p t)
         t)
        ((string= command "where")
         (reject-extra-arguments 1)
         (conversation-replay-write-location session stream)
         t)
        ((member command '("h" "help" "?") :test #'string=)
         (reject-extra-arguments 1)
         (conversation-replay-write-help stream)
         t)
        (t
         (error 'configuration-error
                :message (format nil "Unknown replay command ~S." command)))))))

(-> conversation-replay-run
    (configuration string list
     &key (:input stream) (:output stream) (:interactive-p boolean))
    null)
(defun conversation-replay-run
    (configuration identifier selection
     &key (input *standard-input*) (output *standard-output*)
          (interactive-p
            (and (interactive-stream-p input)
                 (interactive-stream-p output))))
  "Inspect IDENTIFIER without acquiring a lease or writing conversation state."
  (let* ((conversation (conversation-replay-load configuration identifier))
         (session (conversation-replay-create conversation)))
    (conversation-replay-apply-selection session selection)
    (format output "~&Autolith replay ~A~@[  ~A~]~%"
            (conversation-identifier-display
             (conversation-identifier conversation))
            (conversation-title conversation))
    (conversation-replay-write-current session output)
    (when interactive-p
      (conversation-replay-write-help output)
      (loop
        (format output "replay> ")
        (force-output output)
        (let ((line (read-line input nil nil)))
          (unless line
            (return))
          (handler-case
              (unless (conversation-replay-execute-command session line output)
                (return))
            (autolith-error (condition)
              (format output "~A~%" condition))
            (error (condition)
              (format output "~A~%" condition)))))))
  nil)


;;;; -- Durable Conversation Forks --

(-> conversation-fork--raw-records (conversation) list)
(defun conversation-fork--raw-records (conversation)
  "Return every complete durable record in CONVERSATION across all chunks."
  (let ((records nil)
        (identity (conversation-pathname conversation)))
    (dolist (pathname (conversation-storage-pathnames identity))
      (conversation--from-header
       identity pathname (conversation--peek-segment-header pathname))
      (conversation--map-records
       pathname
       (lambda (record)
         (unless (eq (first record) ':conversation)
           (push record records)))))
    (nreverse records)))

(-> conversation-fork--selection-session (conversation list)
    conversation-replay-session)
(defun conversation-fork--selection-session (conversation records)
  "Return replay-selector state over exact durable RECORDS."
  (let ((turn 0)
        (entries nil))
    (dolist (record records)
      (when (and (conversation--record-form-p record)
                 (conversation-replay--user-turn-record-p record))
        (incf turn))
      (push (make-instance 'conversation-replay-record
                           :record record
                           :turn turn)
            entries))
    (unless entries
      (error 'conversation-error
             :message "The conversation contains no durable records to fork."
             :pathname (conversation-pathname conversation)
             :sequence nil))
    (make-instance 'conversation-replay-session
                   :conversation conversation
                   :records (coerce (nreverse entries) 'vector))))

(-> conversation-fork--select-head (conversation list list) integer)
(defun conversation-fork--select-head (conversation records selection)
  "Return the inclusive durable head selected from RECORDS by SELECTION."
  (let ((session (conversation-fork--selection-session conversation records)))
    (if selection
        (conversation-replay-apply-selection session selection)
        (setf (conversation-replay-session-position session)
              (1- (length (conversation-replay-session-records session)))))
    (let ((sequence
            (conversation-replay--record-sequence
             (conversation-replay--current-record session))))
      (unless (typep sequence '(integer 1))
        (error 'conversation-invariant-error
               :message "The selected fork head has no valid durable sequence."
               :pathname (conversation-pathname conversation)
               :sequence sequence))
      sequence)))

(-> conversation-fork--prefix (conversation list integer) list)
(defun conversation-fork--prefix (conversation records head-sequence)
  "Return RECORDS through inclusive HEAD-SEQUENCE after contiguous validation."
  (let ((expected 1)
        (prefix nil)
        (found-p nil))
    (dolist (record records)
      (when (> expected head-sequence)
        (return))
      (unless (conversation--record-form-p record)
        (error 'conversation-invariant-error
               :message "A fork source record is not a keyword property list."
               :pathname (conversation-pathname conversation)
               :sequence expected))
      (let ((sequence (getf (rest record) :seq)))
        (unless (eql sequence expected)
          (error 'conversation-invariant-error
                 :message
                 (format nil "The fork source has a durable sequence gap at ~D."
                         expected)
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (push (copy-tree record) prefix)
        (when (= sequence head-sequence)
          (setf found-p t)
          (return))
        (incf expected)))
    (unless found-p
      (error 'conversation-error
             :message (format nil "Fork head sequence ~D does not exist."
                              head-sequence)
             :pathname (conversation-pathname conversation)
             :sequence head-sequence))
    (nreverse prefix)))

(-> conversation-fork--image-descriptors (list) list)
(defun conversation-fork--image-descriptors (records)
  "Return the unique image descriptors referenced by durable RECORDS."
  (let ((descriptors nil)
        (seen (make-hash-table :test #'equal)))
    (labels ((note (descriptor)
               "Retain DESCRIPTOR once by its artifact basename."
               (let ((artifact (and (listp descriptor)
                                    (getf descriptor :artifact))))
                 (unless (and (non-empty-string-p artifact)
                              (gethash artifact seen))
                   (when (non-empty-string-p artifact)
                     (setf (gethash artifact seen) t))
                   (push descriptor descriptors)))))
      (dolist (record records)
        (let ((properties (rest record)))
          (dolist (descriptor (getf properties :images))
            (note descriptor))
          (dolist (block (getf properties :content-blocks))
            (when (and (listp block) (getf block :image))
              (note (getf block :image)))))))
    (nreverse descriptors)))

(-> conversation-fork--copy-images (conversation conversation list) null)
(defun conversation-fork--copy-images (source target descriptors)
  "Validate and copy only DESCRIPTORS from SOURCE into TARGET's artifact root."
  (let ((source-root (conversation-image-artifact-root source))
        (target-root (conversation-image-artifact-root target)))
    (dolist (descriptor descriptors)
      (let* ((attachment (image-attachment-from-record descriptor source-root))
             (destination
               (merge-pathnames (getf descriptor :artifact) target-root)))
        (ensure-directories-exist destination)
        (uiop:copy-file (image-attachment-pathname attachment) destination))))
  nil)

(-> conversation-fork--prompt-cache-key (conversation string) non-empty-string)
(defun conversation-fork--prompt-cache-key (source target-identifier)
  "Return a fresh cache key distinct from SOURCE and TARGET-IDENTIFIER."
  (loop for candidate = (make-identifier)
        unless (or (string= candidate target-identifier)
                   (and (conversation-prompt-cache-key source)
                        (string= candidate
                                 (conversation-prompt-cache-key source))))
          return candidate))

(-> conversation-fork
    (configuration string
     &key (:selection list) (:identifier (option string)))
    conversation)
(defun conversation-fork (configuration source-identifier
                           &key selection identifier)
  "Persist and return a fresh conversation forked at inclusive durable SELECTION.

SELECTION uses the replay selector syntax.  A null selection chooses the newest
complete durable record.  The source is read only and never repaired."
  (let* ((source (conversation-replay-load configuration source-identifier))
         (records (conversation-fork--raw-records source))
         (head-sequence
           (conversation-fork--select-head source records selection))
         (prefix
           (conversation-fork--prefix source records head-sequence))
         (target
           (conversation-create
            configuration
            :identifier identifier
            :prompt-cache-key
            (conversation-fork--prompt-cache-key
             source (or identifier "generated-fork"))))
         (target-identity (conversation-pathname target))
         (target-log (conversation-log-pathname target))
         (target-artifacts (conversation-image-artifact-root target))
         (published-p nil))
    (unwind-protect
         (progn
           (conversation-fork--copy-images
            source target (conversation-fork--image-descriptors prefix))
           (dolist (record prefix)
             (conversation--apply-record target record))
           (let ((provenance
                   (list :fork
                         :seq (1+ head-sequence)
                         :time (get-universal-time)
                         :source-id (conversation-identifier source)
                         :source-sequence head-sequence
                         :selection (and selection (copy-list selection)))))
             (conversation--apply-record target provenance)
             (log-append target-log
                         (first prefix)
                         :initial-forms
                         (list (conversation--header-record target
                                  :chunk-start-sequence 1)))
             (dolist (record (rest prefix))
               (log-append target-log record))
             (log-append target-log provenance))
           (let ((loaded (conversation-load target-identity)))
             (setf published-p t)
             loaded))
      (unless published-p
        (dolist (pathname (conversation-storage-pathnames target-identity))
          (ignore-errors (delete-file pathname)))
        (when (probe-file target-artifacts)
          (ignore-errors
            (platform-delete-directory-tree *platform* target-artifacts
                                            :validate t
                                            :if-does-not-exist ':ignore)))))))
