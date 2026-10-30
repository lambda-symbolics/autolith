;;;; -- Optional Trusted Parent Terminal --

(in-package #:autolith-supervisor)

(define-condition parent-terminal-error (error)
  ((reason :initarg :reason :reader parent-terminal-error-reason
           :documentation "The fixed failure category; never agent text."))
  (:documentation "A bounded parent terminal operation could not complete.")
  (:report (lambda (condition stream)
             (format stream "Parent terminal failed: ~A." (parent-terminal-error-reason condition)))))

(defclass parent-terminal ()
  ((input-source :initarg :input-source :reader parent-terminal-input-source
                 :documentation "A trusted poll function taking seconds and returning line, state.")
   (renderer :initarg :renderer :reader parent-terminal-renderer
             :documentation "A synchronous function of region, kind and portable payload plist.")
   (exclusive-input :initarg :exclusive-input :reader parent-terminal-exclusive-input
                    :documentation "Calls a thunk while the complete modal input operation owns the frontend.")
   (read-function :initarg :read-function :reader parent-terminal-read-function
                  :documentation "Optional complete modal reader taking timeout and cancellation keywords.")
   (clock :initarg :clock :reader parent-terminal-clock
          :documentation "The trusted monotonic seconds function.")
   (write-timeout :initarg :write-timeout :reader parent-terminal-write-timeout
                  :documentation "Maximum seconds for render admission and completion together.")
   (maximum-bytes :initarg :maximum-bytes :reader parent-terminal-maximum-bytes
                  :documentation "Maximum raw UTF-8 bytes admitted per input or output event.")
   (bytes-per-second :initarg :bytes-per-second :reader parent-terminal-bytes-per-second
                     :documentation "Maximum escaped agent output bytes in one second.")
   (events-per-second :initarg :events-per-second :reader parent-terminal-events-per-second
                      :documentation "Maximum agent output events in one second.")
   (render-lock :initform      (bordeaux-threads:make-lock "parent terminal rendering")
                :reader        parent-terminal-render-lock
                :documentation "Serializes presentation and excludes agent output from a prompt.")
   (input-lock :initform      (bordeaux-threads:make-lock "parent terminal input")
               :reader        parent-terminal-input-lock
               :documentation "Ensures a single trusted input consumer.")
   (state-lock :initform      (bordeaux-threads:make-lock "parent terminal state")
               :reader        parent-terminal-state-lock
               :documentation "Protects the close latch without waiting for input or rendering.")
   (closed :initform nil :accessor parent-terminal-closed
           :documentation "The monotonic close latch.")
   (window :initform nil :accessor parent-terminal-window
           :documentation "Start of the current one-second output window.")
   (bytes :initform 0 :accessor parent-terminal-bytes
          :documentation "Escaped output bytes charged in the window.")
   (events :initform 0 :accessor parent-terminal-events
           :documentation "Output events charged in the window."))
  (:documentation "Optional parent I/O with no queue and no agent code dependency.
The input source returns LINE and one of :LINE, :TIMEOUT, :EOF, :CANCELLED.
A renderer receives (:AGENT or :TRUSTED), a kind keyword, and a plist. It must
preserve this region distinction. Use PARENT-TERMINAL-STREAM-RENDERER for plain
line output. No renderer, source or callback is supplied by the agent."))

(declaim (ftype (function () real) parent-terminal--clock))

(defun parent-terminal--clock ()
  "Return monotonic seconds for rate and input deadlines."
  (/ (get-internal-real-time) internal-time-units-per-second))

(declaim (ftype (function (&key (:input-source function) (:renderer function)
                              (:clock function) (:write-timeout real)
                              (:maximum-bytes integer) (:bytes-per-second integer)
                               (:events-per-second integer) (:exclusive-input function)
                               (:read-function (or null function)))
                         (values parent-terminal &optional))
                make-parent-terminal))

(defun make-parent-terminal (&key input-source renderer (clock #'parent-terminal--clock)
                                 (write-timeout 1) (maximum-bytes 32768)
                                 (bytes-per-second 65536) (events-per-second 128)
                                 (exclusive-input #'funcall) read-function)
  "Make an explicit parent terminal. Callbacks execute only in the trusted parent."
  (unless (and (functionp input-source) (functionp renderer) (functionp clock)
               (functionp exclusive-input) (or (null read-function) (functionp read-function))
               (plusp write-timeout) (plusp maximum-bytes)
               (plusp bytes-per-second) (plusp events-per-second))
    (error 'parent-terminal-error :reason ':invalid-configuration))
  (values (make-instance 'parent-terminal :input-source input-source :renderer renderer :clock clock
                         :write-timeout write-timeout :maximum-bytes maximum-bytes
                         :bytes-per-second bytes-per-second :events-per-second events-per-second
                         :exclusive-input exclusive-input :read-function read-function)))

(declaim (ftype (function (parent-terminal) boolean) parent-terminal-closed-p))

(defun parent-terminal-closed-p (terminal)
  "Read the close latch without waiting for a blocked consumer."
  (bordeaux-threads:with-lock-held ((parent-terminal-state-lock terminal))
    (not (null (parent-terminal-closed terminal)))))

(declaim (ftype (function (parent-terminal) null) parent-terminal-close))

(defun parent-terminal-close (terminal)
  "Cancel reads at the next poll boundary. Do not close caller-owned streams."
  (bordeaux-threads:with-lock-held ((parent-terminal-state-lock terminal))
    (setf (parent-terminal-closed terminal) t))
  nil)

(declaim (ftype (function (string) integer) parent-terminal--bytes))

(defun parent-terminal--bytes (text)
  "Count UTF-8 bytes without allocating a byte vector."
  (loop for character across text for code = (char-code character)
        sum (cond ((< code #x80)
                   1)
                  ((< code #x800)
                   2)
                  ((< code #x10000)
                   3)
                  (t
                   4))))

(declaim (ftype (function (string) string) parent-terminal-escape))

(defun parent-terminal-escape (text)
  "Escape controls and directional format characters into one inert display line.
Escape the backslash too, so literal escape notation is distinct from a control."
  (with-output-to-string (stream)
    (loop for character across text for code = (char-code character)
          do (cond ((or (< code 32) (<= 127 code 159)
                        (<= #x200B code #x200F) (<= #x2028 code #x202E)
                        (<= #x2060 code #x206F) (= code #xFEFF))
                    (format stream "\\u~4,'0X" code))
                   ((char= character #\\)
                    (write-string "\\\\" stream))
                   (t
                    (write-char character stream))))))

(declaim (ftype (function (parent-terminal keyword string &key (:turn-id (or null string))) null)
                parent-terminal-output))

(defun parent-terminal-output (terminal channel text &key turn-id)
  "Render validated transient Output as untrusted data, with no retained queue.
TURN-ID must come from trusted lifecycle state, never from a textual agent label.
Rate violations and a blocked renderer signal typed failures before more admission."
  (unless (and (member channel '(:progress :diagnostic))
               (<= (parent-terminal--bytes text) (parent-terminal-maximum-bytes terminal))
               (or (null turn-id) (<= (length turn-id) 256)))
    (error 'parent-terminal-error :reason ':output-limit))
  (let ((safe (parent-terminal-escape text)))
    (handler-case
        (sb-ext:with-timeout (parent-terminal-write-timeout terminal)
          (bordeaux-threads:with-lock-held ((parent-terminal-render-lock terminal))
            (when (parent-terminal-closed-p terminal)
              (error 'parent-terminal-error :reason ':closed))
            (let* ((now   (funcall (parent-terminal-clock terminal)))
                   (bytes (parent-terminal--bytes safe)))
              (when (or (null (parent-terminal-window terminal))
                        (>= (- now (parent-terminal-window terminal)) 1))
                (setf (parent-terminal-window terminal) now
                      (parent-terminal-events terminal) 0
                      (parent-terminal-bytes terminal) 0))
              (when (or (>= (parent-terminal-events terminal)
                            (parent-terminal-events-per-second terminal))
                        (> (+ bytes (parent-terminal-bytes terminal))
                           (parent-terminal-bytes-per-second terminal)))
                (error 'parent-terminal-error :reason ':rate-limit))
              (incf (parent-terminal-events terminal))
              (incf (parent-terminal-bytes terminal) bytes)
              (funcall (parent-terminal-renderer terminal) ':agent channel
                       (list :text safe :turn-id (and turn-id (parent-terminal-escape turn-id)))))))
      (sb-ext:timeout ()
        (error 'parent-terminal-error :reason ':blocked-output))))
  nil)

(declaim (ftype (function (parent-terminal &key (:turn-id (or null function))) function)
                parent-terminal-output-callback))

(defun parent-terminal-output-callback (terminal &key turn-id)
  "Return the two-argument callback for SESSION-RUNTIME-OPEN's :OUTPUT-CALLBACK.
The optional trusted TURN-ID function reads current runtime lifecycle state."
  (lambda (channel text)
    (parent-terminal-output terminal channel text :turn-id (and turn-id (funcall turn-id)))))

(declaim (ftype (function (parent-terminal &key (:timeout (or null real))
                                              (:cancelled (or null function)))
                         (values (or null string) keyword)) parent-terminal-read-input))

(defun parent-terminal-read-input (terminal &key timeout cancelled)
  "Return LINE and :LINE, or NIL and :EOF, :CANCELLED, :TIMEOUT.
Polls last at most 0.05 seconds. Cancellation and close do not need the input lock.
An input source that exceeds its poll contract signals :BLOCKED-INPUT. A second
reader signals :INPUT-BUSY. Stop the ordinary input reader before an approval."
  (when (parent-terminal-read-function terminal)
    (let ((lock (parent-terminal-input-lock terminal)))
      (unless (bordeaux-threads:acquire-lock lock nil)
        (error 'parent-terminal-error :reason ':input-busy))
      (unwind-protect
           (multiple-value-bind (line state)
               (funcall (parent-terminal-read-function terminal) :timeout timeout
                        :cancelled                                        (lambda ()
                                                                            (or (parent-terminal-closed-p terminal)
                                                                                (and cancelled (funcall cancelled)))))
             (unless (and (member state '(:line :eof :cancelled :timeout))
                          (if (eq state ':line)
                              (and (stringp line)
                                   (<= (parent-terminal--bytes line) (parent-terminal-maximum-bytes terminal)))
                              (null line)))
               (error 'parent-terminal-error :reason ':invalid-input))
             (return-from parent-terminal-read-input (values line state)))
        (bordeaux-threads:release-lock lock))))
  (let ((deadline (and timeout (+ (funcall (parent-terminal-clock terminal)) timeout)))
        (lock     (parent-terminal-input-lock terminal)))
    (unless (bordeaux-threads:acquire-lock lock nil)
      (error 'parent-terminal-error :reason ':input-busy))
    (unwind-protect
        (loop
          (when (or (parent-terminal-closed-p terminal) (and cancelled (funcall cancelled)))
            (return (values nil ':cancelled)))
          (let* ((remaining (and deadline (- deadline (funcall (parent-terminal-clock terminal)))))
                 (poll      (if remaining (min 0.05 (max 0 remaining)) 0.05)))
            (when (and remaining (<= remaining 0))
              (return (values nil ':timeout)))
            (multiple-value-bind (line state)
                (handler-case
                    (sb-ext:with-timeout (+ poll 0.1)
                      (funcall (parent-terminal-input-source terminal) poll))
                  (sb-ext:timeout ()
                    (error 'parent-terminal-error :reason ':blocked-input)))
              (when (or (parent-terminal-closed-p terminal) (and cancelled (funcall cancelled)))
                (return (values nil ':cancelled)))
              (when (and deadline (>= (funcall (parent-terminal-clock terminal)) deadline))
                (return (values nil ':timeout)))
              (case state
                (:line
                 (unless (and (stringp line)
                              (<= (parent-terminal--bytes line) (parent-terminal-maximum-bytes terminal)))
                   (error 'parent-terminal-error :reason ':input-limit))
                 (return (values (copy-seq line) ':line)))
                ((:eof :cancelled)
                 (return (values nil state)))
                (:timeout
                 (sleep 0.001))
                (otherwise
                 (error 'parent-terminal-error :reason ':invalid-input-state))))))
      (bordeaux-threads:release-lock lock))))

(declaim (ftype (function (parent-terminal function &key (:on-cancel (or null function))
                                                       (:on-eof (or null function))
                                                       (:cancelled (or null function))) keyword)
                parent-terminal-run-input))

(defun parent-terminal-run-input (terminal on-input &key on-cancel on-eof cancelled)
  "Pass trusted lines to ON-INPUT. The caller owns turn IDs, persistence and policy.
Return :EOF or :CANCELLED after invoking the corresponding optional callback."
  (loop
    (multiple-value-bind (line state) (parent-terminal-read-input terminal :cancelled cancelled)
      (case state
        (:line
         (funcall on-input line))
        (:eof
         (when on-eof (funcall on-eof))
         (return ':eof))
        (:cancelled
         (when on-cancel (funcall on-cancel))
         (return ':cancelled))))))

(declaim (ftype (function (sequence) vector) parent-terminal--grant-snapshot))

(defun parent-terminal--grant-snapshot (grants)
  "Copy exact resolved Grant fields as inert data. Reject unspecified grants."
  (unless (<= 1 (length grants) 64)
    (error 'parent-terminal-error :reason ':invalid-grants))
  (map 'vector
       (lambda (grant)
         (let ((resource   (parent-proto:grant.resource grant))
               (target     (parent-proto:grant.target grant))
               (operations (parent-proto:grant.operations grant))
               (duration   (parent-proto:grant.duration grant)))
           (unless (and (member resource '(:filesystem :network :service))
                        (member duration '(:instance :session))
                        (<= 1 (length target) 4096)
                        (<= 1 (length operations) 64)
                        (every (lambda (operation) (<= 1 (length operation) 256)) operations))
             (error 'parent-terminal-error :reason ':invalid-grants))
           (list :resource resource :target (copy-seq target)
                 :operations (map 'vector #'copy-seq operations) :duration duration)))
       grants))

(declaim (ftype (function (vector) vector) parent-terminal--copy-grant-snapshot))

(defun parent-terminal--copy-grant-snapshot (snapshot)
  "Detach a renderer's grant data from the exact snapshot returned for consent."
  (map 'vector
       (lambda (grant)
         (list :resource (getf grant :resource) :target (copy-seq (getf grant :target))
               :operations (map 'vector #'copy-seq (getf grant :operations))
               :duration   (getf grant :duration)))
       snapshot))

(declaim (ftype (function (parent-terminal sequence &key (:operation-id string)
                                                        (:reason string) (:timeout real)
                                                        (:cancelled (or null function)))
                         (values boolean vector)) parent-terminal-approve))

(defun parent-terminal-approve (terminal grants &key (operation-id "") (reason "")
                                                   (timeout 60) cancelled)
  "Hold frontend input ownership from the exact approval display through consent."
  (funcall (parent-terminal-exclusive-input terminal)
           (lambda ()
             (parent-terminal--approve terminal grants :operation-id operation-id
                                       :reason reason :timeout timeout :cancelled cancelled))))

(declaim (ftype (function (parent-terminal sequence &key (:operation-id string)
                                                      (:reason string) (:timeout real)
                                                      (:cancelled (or null function)))
                         (values boolean vector)) parent-terminal--approve))

(defun parent-terminal--approve (terminal grants &key (operation-id "") (reason "")
                                                   (timeout 60) cancelled)
  "Prompt for exact parent-resolved GRANTS. Return consent and the displayed snapshot.
The caller must apply that snapshot, not a subsequently modified grant request.
Agent reason text has its own untrusted region. Only an exact trusted input YES
confirms. EOF, cancellation, timeout and every other answer deny. This grants no
access by itself and does not restart an agent or mutate policy."
  (unless (and (<= (length operation-id) 256)
               (<= (parent-terminal--bytes reason) (parent-terminal-maximum-bytes terminal)))
    (error 'parent-terminal-error :reason ':prompt-limit))
  (let* ((snapshot     (parent-terminal--grant-snapshot grants))
         (presentation (parent-terminal--copy-grant-snapshot snapshot)))
    (when (> (loop for grant across snapshot
                   sum (+ (parent-terminal--bytes (getf grant :target))
                          (loop for operation across (getf grant :operations)
                                sum (parent-terminal--bytes operation))))
             (parent-terminal-maximum-bytes terminal))
      (error 'parent-terminal-error :reason ':prompt-limit))
    (bordeaux-threads:with-lock-held ((parent-terminal-render-lock terminal))
      (when (parent-terminal-closed-p terminal)
        (error 'parent-terminal-error :reason ':closed))
      (handler-case
          (sb-ext:with-timeout (parent-terminal-write-timeout terminal)
            (when (plusp (length reason))
              (funcall (parent-terminal-renderer terminal) ':agent ':approval-reason
                       (list :text (parent-terminal-escape reason))))
            (funcall (parent-terminal-renderer terminal) ':trusted ':approval
                     (list :operation-id (parent-terminal-escape operation-id)
                           :grants       presentation)))
        (sb-ext:timeout ()
          (error 'parent-terminal-error :reason ':blocked-output)))
      (multiple-value-bind (line state)
          (parent-terminal-read-input terminal :timeout timeout :cancelled cancelled)
        (values (and (eq state ':line) (string= line "YES")) snapshot)))))

(declaim (ftype (function (parent-terminal &key (:kind keyword) (:scope keyword)
                                               (:identifier string) (:version integer)
                                               (:proposal t) (:timeout real)
                                               (:cancelled (or null function)))
                         boolean) parent-terminal-approve-knowledge))

(defun parent-terminal-approve-knowledge (terminal &key kind scope identifier version proposal
                                                      (timeout 60) cancelled)
  "Hold frontend input ownership for the complete knowledge admission prompt."
  (funcall (parent-terminal-exclusive-input terminal)
           (lambda ()
             (parent-terminal--approve-knowledge terminal :kind kind :scope scope
                                                :identifier identifier :version version
                                                :proposal proposal :timeout timeout
                                                :cancelled cancelled))))

(declaim (ftype (function (parent-terminal &key (:kind keyword) (:scope keyword)
                                              (:identifier string) (:version integer)
                                              (:proposal t) (:timeout real)
                                              (:cancelled (or null function)))
                         boolean) parent-terminal--approve-knowledge))

(defun parent-terminal--approve-knowledge (terminal &key kind scope identifier version proposal
                                                      (timeout 60) cancelled)
  "Ask for exact rule or contract admission using only parent-owned terminal input.
The renderer receives text, not the mutable proposal. Only YES grants consent."
  (unless (and (member kind '(:rule :contract)) (member scope '(:workspace :global))
               (typep version '(integer 1)) (<= 1 (length identifier) 256))
    (error 'parent-terminal-error :reason ':invalid-configuration))
  (let* ((*print-readably* t)
         (*print-circle*   t)
         (*print-pretty*   nil)
         (text             (format nil "kind=~S scope=~S id=~S version=~D proposal=~S"
                                   kind scope identifier version proposal)))
    (when (> (parent-terminal--bytes text) (parent-terminal-maximum-bytes terminal))
      (error 'parent-terminal-error :reason ':prompt-limit))
    (bordeaux-threads:with-lock-held ((parent-terminal-render-lock terminal))
      (when (parent-terminal-closed-p terminal)
        (error 'parent-terminal-error :reason ':closed))
      (handler-case
          (sb-ext:with-timeout (parent-terminal-write-timeout terminal)
            (funcall (parent-terminal-renderer terminal) ':trusted ':knowledge-approval
                     (list :text (parent-terminal-escape text))))
        (sb-ext:timeout ()
          (error 'parent-terminal-error :reason ':blocked-output)))
      (multiple-value-bind (line state)
          (parent-terminal-read-input terminal :timeout timeout :cancelled cancelled)
        (and (eq state ':line) (string= line "YES") t)))))

(declaim (ftype (function (string) string) parent-terminal--quoted-field))

(defun parent-terminal--quoted-field (text)
  "Quote a grant string with unambiguous control, slash and quote escapes."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across (parent-terminal-escape text)
          do (when (char= character #\") (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(declaim (ftype (function (stream) function) parent-terminal-stream-renderer))

(defun parent-terminal-stream-renderer (stream)
  "Make a line renderer with separate fixed parent and agent prefixes.
All text is escaped by the terminal before this callback. Grant fields are exact
portable snapshot data; escape each field here instead of printing Lisp objects."
  (lambda (region kind payload)
    (cond
      ((and (eq region ':trusted) (eq kind ':network-approval))
       (format stream "PARENT | NETWORK ~A~%" (getf payload :text)))
      ((and (eq region ':trusted) (eq kind ':knowledge-approval))
       (format stream "PARENT | KNOWLEDGE ~A~%" (getf payload :text))
       (write-line "PARENT | Type YES to admit this exact proposal; any other answer denies." stream))
      ((and (eq region ':trusted) (eq kind ':approval))
       (format stream "PARENT | ACCESS ~A~%" (getf payload :operation-id))
       (loop for grant across (getf payload :grants)
             do (format stream "PARENT | ~A target=~A duration=~A operations=["
                        (getf grant :resource)
                        (parent-terminal--quoted-field (getf grant :target))
                        (getf grant :duration))
                (loop for operation across (getf grant :operations)
                      for first-p = t then nil
                      do (unless first-p (write-string "," stream))
                         (write-string (parent-terminal--quoted-field operation) stream))
                (write-line "]" stream))
       (write-line "PARENT | Type YES to approve these exact grants; any other answer denies." stream))
      (t
       (format stream "AGENT | ~A~@[ turn=~A~] | ~A~%"
               kind (getf payload :turn-id) (getf payload :text))))
    (finish-output stream)))

(declaim (ftype (function (stream &key (:maximum-bytes integer)) function)
                parent-terminal-stream-input-source))

(defun parent-terminal-stream-input-source (stream &key (maximum-bytes 32768))
  "Make a bounded polling source for a parent-owned character stream.
Preserve partial input across polls. Return a final partial line before EOF.
Do not transfer stream ownership. A terminal in canonical mode keeps its host
line editing; this adapter does not install raw mode or load the agent UI."
  (unless (plusp maximum-bytes)
    (error 'parent-terminal-error :reason ':invalid-configuration))
  (let ((buffer       (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (bytes        0)
        (ended        nil)
        (skip-newline nil))
    (lambda (seconds)
      (block poll
        (when ended (return-from poll (values nil ':eof)))
        (let ((deadline (+ (parent-terminal--clock) seconds)))
          (loop
            (let ((character (read-char-no-hang stream nil ':eof)))
              (cond ((eq character ':eof)
                     (setf ended t)
                     (return-from poll
                       (if (plusp (length buffer))
                           (values (copy-seq buffer) ':line)
                           (values nil ':eof))))
                    ((null character)
                     (when (>= (parent-terminal--clock) deadline)
                       (return-from poll (values nil ':timeout)))
                     (sleep (min 0.005 (max 0 (- deadline (parent-terminal--clock))))))
                    ((and skip-newline (char= character #\Newline))
                     (setf skip-newline nil))
                    ((or (char= character #\Newline) (char= character #\Return))
                     (let ((line (copy-seq buffer)))
                       (setf (fill-pointer buffer) 0
                             bytes 0
                             skip-newline (char= character #\Return))
                       (return-from poll (values line ':line))))
                    (t
                     (setf skip-newline nil)
                     (incf bytes (parent-terminal--bytes (string character)))
                     (when (> bytes maximum-bytes)
                       (setf ended t)
                       (error 'parent-terminal-error :reason ':input-limit))
                     (vector-push-extend character buffer))))))))))
