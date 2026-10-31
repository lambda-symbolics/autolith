(in-package #:autolith)

;;;; -- Action Traces --

;;; An action trace is what one user turn did: the tool calls in order, each
;;; with its arguments, status, and output, and the model requests and tokens
;;; the turn used. Traces are read from conversation logs and kept only in
;;; memory, so the transcript stays the one durable record of what happened.
;;;
;;; Residency is bounded. A turn that can no longer be learned from drops its
;;; payload at once, each conversation keeps only its newest accepted traces
;;; and its newest measured procedure runs, and the whole cache keeps a
;;; character budget. An evicted conversation is simply read again from its
;;; log, so eviction never changes what a query finds.

(defparameter *action-trace-call-limit* 32
  "The most tool calls a trace may have to be used for learning.")

(defparameter *action-trace-limit* 20
  "The most recent traces one learning pass reads, and the most each conversation keeps.")

(defparameter *action-trace-conversation-limit* 10
  "The most recent conversations one learning pass reads traces from.")

(defparameter *action-trace-output-limit* 16384
  "The longest call result a trace keeps; longer results are dropped.")

(defparameter *action-trace-weight-limit* 65536
  "The most characters of input, arguments, results and procedure provenance a trace may hold.")

(defparameter *action-trace-cache-weight-limit* 4194304
  "The most characters of trace payload the cache keeps across conversations.")

(defvar *action-trace-cache* (make-hash-table :test #'equal :synchronized t)
  "The ACTION-TRACE-HISTORY of each read conversation by identity namestring.")

(defclass action-trace ()
  ((conversation
    :initarg       :conversation
    :reader        action-trace-conversation
    :type          string
    :documentation "The conversation the turn belongs to.")
   (sequence
    :initarg       :sequence
    :reader        action-trace-sequence
    :type          integer
    :documentation "The durable sequence of the user message that started the turn.")
   (input
    :initarg       :input
    :accessor      action-trace-input
    :type          string
    :documentation "The user message that started the turn.")
   (calls
    :initarg       :calls
    :accessor      action-trace-calls
    :type          list
    :documentation "(:TOOL :ARGUMENTS :STATUS :OUTPUT :CALL-ID) for each tool call in order.

Procedure calls also retain :PROCEDURE-VERSION and :PROCEDURE provenance.
OUTPUT is kept only when it is a JSON object or array of bounded length,
the only results later calls can take values from.")
   (requests
    :initarg       :requests
    :initform      0
    :accessor      action-trace-requests
    :type          (integer 0)
    :documentation "How many model requests the turn made.")
   (tokens
    :initarg       :tokens
    :initform      0
    :accessor      action-trace-tokens
    :type          (integer 0)
    :documentation "How many tokens those requests used in total.")
   (usage-known-p
    :initarg       :usage-known-p
    :initform      t
    :accessor      action-trace-usage-known-p
    :type          boolean
    :documentation "Whether every counted model request supplied total token usage.")
   (answered-p
    :initarg       :answered-p
    :initform      nil
    :accessor      action-trace-answered-p
    :type          boolean
    :documentation "Whether the model answered after the turn's last tool call.")
   (aborted-p
    :initarg       :aborted-p
    :initform      nil
    :accessor      action-trace-aborted-p
    :type          boolean
    :documentation "Whether the turn was cancelled or failed before it finished.")
   (weight
    :initarg       :weight
    :initform      0
    :accessor      action-trace-weight
    :type          (integer 0)
    :documentation "Characters of retained payload, including complete procedure definitions.")
   (released-p
    :initarg       :released-p
    :initform      nil
    :accessor      action-trace-released-p
    :type          boolean
    :documentation "Whether the turn can no longer be learned from and dropped its payload."))
  (:documentation "The tool calls and model cost of one user turn."))

(defclass action-trace-history ()
  ((segments
    :initarg       :segments
    :reader        action-trace-history-segments
    :type          list
    :documentation "(NAMESTRING FILE-IDENTITY SIZE) of each log segment read, oldest first.")
   (position
    :initarg       :position
    :initform      0
    :accessor      action-trace-history-position
    :type          (integer 0)
    :documentation "Where reading stopped in the last segment.")
   (directory
    :initarg       :directory
    :initform      nil
    :accessor      action-trace-history-directory
    :type          (option string)
    :documentation "The workspace the conversation started in.")
   (current
    :initarg       :current
    :initform      nil
    :accessor      action-trace-history-current
    :type          (option action-trace)
    :documentation "The newest turn, which later records may still extend.")
   (finished
    :initarg       :finished
    :initform      nil
    :accessor      action-trace-history-finished
    :type          list
    :documentation "The newest accepted finished traces, newest first, at most *ACTION-TRACE-LIMIT*.")
   (runs
    :initarg       :runs
    :initform      (make-hash-table :test #'equal)
    :reader        action-trace-history-runs
    :type          hash-table
    :documentation "(IDENTIFIER VERSION) to its newest measured finished runs, newest first.

Each run is a plist of :PROCEDURE, :REQUESTS and :TOKENS, at most
*ACTION-TRACE-LIMIT* per key.")
   (used-at
    :initform      (get-internal-real-time)
    :accessor      action-trace-history-used-at
    :type          integer
    :documentation "When a query last used this history, for cache eviction."))
  (:documentation "The bounded trace state of one conversation's log."))


;;;; -- Learning Eligibility --

(-> action-trace-skeleton (action-trace) list)
(defun action-trace-skeleton (trace)
  "Return the ordered tool names TRACE called."
  (mapcar (lambda (call) (getf call :tool)) (action-trace-calls trace)))

(-> action-trace-accepted-p (action-trace) boolean)
(defun action-trace-accepted-p (trace)
  "Return true when TRACE is a finished turn worth learning from.

It made at least one call and no more than the limit, every call succeeded,
the model answered after the last one, the turn was not cancelled or
abandoned on an error, and its payload fit the weight limit."
  (let ((calls (action-trace-calls trace)))
    (and (not (action-trace-released-p trace))
         calls
         (<= (length calls) *action-trace-call-limit*)
         (every (lambda (call) (eq (getf call :status) ':ok)) calls)
         (action-trace-answered-p trace)
         (not (action-trace-aborted-p trace))
         t)))

(-> action-trace-procedure-runs (action-trace) list)
(defun action-trace-procedure-runs (trace)
  "Return (IDENTIFIER VERSION PROCEDURE) for each procedure TRACE ran in a measured turn.

Only an accepted turn with model requests and complete usage counts, and
only successful procedure.run calls with exact version provenance."
  (and (action-trace-accepted-p trace)
       (plusp (action-trace-requests trace))
       (action-trace-usage-known-p trace)
       (remove-duplicates
        (loop for call in (action-trace-calls trace)
              for arguments = (getf call :arguments)
              when (and (equal (getf call :tool) "procedure.run")
                        (eq (getf call :status) ':ok)
                        (json-object-p arguments)
                        (stringp (json-get arguments "id"))
                        (integerp (getf call :procedure-version)))
                collect (list (json-get arguments "id")
                              (getf call :procedure-version)
                              (getf call :procedure)))
        :test #'equal)))


;;;; -- Queries --

(-> action-traces-in-conversation (pathname &key (:workspace (option string))) list)
(defun action-traces-in-conversation (pathname &key workspace)
  "Return the retained traces of the conversation at PATHNAME, oldest first.

These are its newest accepted traces and its newest turn while that turn may
still be learned from. When WORKSPACE is given, a conversation started in
another workspace yields none. Unreadable storage yields none instead of
failing the caller."
  (handler-case
      (let ((history (action-trace--read pathname)))
        (if (and workspace
                 (action-trace-history-directory history)
                 (not (equal (action-trace-history-directory history) workspace)))
            nil
            (reverse (action-trace--history-traces history))))
    (error ()
      nil)))

(-> action-traces (configuration &key (:limit integer)) list)
(defun action-traces (configuration &key (limit *action-trace-limit*))
  "Return up to LIMIT recent accepted traces of CONFIGURATION's workspace, newest first.

Each conversation retains its *ACTION-TRACE-LIMIT* newest accepted traces, so
a larger LIMIT finds no more of them per conversation."
  (let ((collected nil))
    (action-trace--map-histories
     configuration
     (lambda (history)
       (setf collected
             (append collected
                     (remove-if-not #'action-trace-accepted-p
                                    (action-trace--history-traces history))))
       (>= (length collected) limit)))
    (subseq collected 0 (min limit (length collected)))))

(-> action-trace-procedure-measurements
    (configuration string &key (:version integer) (:procedure t))
    list)
(defun action-trace-procedure-measurements (configuration identifier &key version procedure)
  "Return (REQUESTS TOKENS) of recent measured turns running IDENTIFIER's VERSION, newest first.

Only turns that ran exactly PROCEDURE, the definition of that version, count.
At most *ACTION-TRACE-LIMIT* turns are returned."
  (let ((collected nil))
    (action-trace--map-histories
     configuration
     (lambda (history)
       (let ((current (action-trace-history-current history)))
         (when (and current
                    (find (list identifier version procedure)
                          (action-trace-procedure-runs current)
                          :test #'equal))
           (setf collected
                 (append collected
                         (list (list (action-trace-requests current)
                                     (action-trace-tokens current)))))))
       (setf collected
             (append collected
                     (loop for run in (gethash (list identifier version)
                                               (action-trace-history-runs history))
                           when (equal (getf run :procedure) procedure)
                             collect (list (getf run :requests) (getf run :tokens)))))
       (>= (length collected) *action-trace-limit*)))
    (subseq collected 0 (min *action-trace-limit* (length collected)))))


;;;; -- Cache --

(-> action-trace--map-histories (configuration function) null)
(defun action-trace--map-histories (configuration function)
  "Call FUNCTION with the history of each recent conversation of CONFIGURATION's workspace.

Conversations are visited newest first, at most *ACTION-TRACE-CONVERSATION-LIMIT*
of them, until FUNCTION returns true. The cache is then trimmed to its budget."
  (let ((workspace (knowledge--workspace configuration))
        (visited   nil))
    (loop for pathname in (conversation-list configuration)
          repeat *action-trace-conversation-limit*
          do (let ((history (handler-case (action-trace--read pathname)
                              (error ()
                                nil))))
               (push (namestring (conversation-storage-identity-pathname pathname)) visited)
               (when (and history
                          (not (and workspace
                                    (action-trace-history-directory history)
                                    (not (equal (action-trace-history-directory history)
                                                workspace))))
                          (funcall function history))
                 (return))))
    (action-trace--trim-cache visited))
  nil)

(-> action-trace--history-traces (action-trace-history) list)
(defun action-trace--history-traces (history)
  "Return HISTORY's retained traces, newest first."
  (let ((current (action-trace-history-current history)))
    (if (and current (not (action-trace-released-p current)))
        (cons current (action-trace-history-finished history))
        (action-trace-history-finished history))))

(-> action-trace--procedure-weight (list) (integer 0))
(defun action-trace--procedure-weight (procedure)
  "Return the printed-character cost of the complete PROCEDURE definition, or zero for none."
  (if procedure
      (with-standard-io-syntax
        (length (prin1-to-string procedure)))
      0))

(-> action-trace--history-weight (action-trace-history) (integer 0))
(defun action-trace--history-weight (history)
  "Return HISTORY's retained payload weight, including complete procedure definitions."
  (+ (reduce #'+ (action-trace--history-traces history) :key #'action-trace-weight)
     (loop for runs being the hash-values of (action-trace-history-runs history)
           sum (loop for run in runs
                     sum (+ 256 (action-trace--procedure-weight (getf run :procedure)))))))

(-> action-trace--trim-cache (list) null)
(defun action-trace--trim-cache (visited)
  "Evict cached histories beyond the conversation and weight budgets, least recently used first.

Histories named in VISITED, the conversations the latest query read, go last."
  (let ((entries (loop for name being the hash-keys of *action-trace-cache*
                         using (hash-value history)
                       collect (list name history (action-trace--history-weight history)))))
    (setf entries
          (sort entries
                (lambda (left right)
                  (let ((left-visited-p  (member (first left) visited :test #'string=))
                        (right-visited-p (member (first right) visited :test #'string=)))
                    (if (eq (not left-visited-p) (not right-visited-p))
                        (< (action-trace-history-used-at (second left))
                           (action-trace-history-used-at (second right)))
                        (not left-visited-p))))))
    (let ((count  (length entries))
          (weight (reduce #'+ entries :key #'third)))
      (loop for (name nil entry-weight) in entries
            while (or (> count (* 2 *action-trace-conversation-limit*))
                      (> weight *action-trace-cache-weight-limit*))
            do (remhash name *action-trace-cache*)
               (decf count)
               (decf weight entry-weight))))
  nil)

(-> action-trace--segment-states (pathname) list)
(defun action-trace--segment-states (identity)
  "Return (NAMESTRING FILE-IDENTITY SIZE) for each of IDENTITY's log segments, oldest first."
  (loop for segment in (conversation-storage-pathnames identity)
        for status = (platform-path-status *platform* segment :follow-links-p t)
        when status
          collect (list (namestring segment)
                        (platform-file-status-identity status)
                        (platform-file-status-size status))))

(-> action-trace--resumable-p (list list) boolean)
(defun action-trace--resumable-p (cached current)
  "Return true when segment states CURRENT only extend the CACHED ones.

Every cached segment but the last must be unchanged, and the last must be the
same file, no shorter than when it was read."
  (let ((count (length cached)))
    (and (plusp count)
         (<= count (length current))
         (equal (butlast cached) (subseq current 0 (1- count)))
         (destructuring-bind (name identity size) (first (last cached))
           (destructuring-bind (now-name now-identity now-size) (nth (1- count) current)
             (and (equal name now-name)
                  (equal identity now-identity)
                  (<= size now-size))))
         t)))

(-> action-trace--read (pathname) action-trace-history)
(defun action-trace--read (pathname)
  "Return the bounded trace history of the conversation at PATHNAME.

Only records appended since the previous read are visited: archived segments
never change and the active one only grows, so a cached read continues where
it stopped. Any other change rereads every segment, keeping the same bounds
while it streams."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (name     (namestring identity))
         (states   (action-trace--segment-states identity))
         (cached   (gethash name *action-trace-cache*)))
    (if (and cached (equal (action-trace-history-segments cached) states))
        (progn
          (setf (action-trace-history-used-at cached) (get-internal-real-time))
          cached)
        (let* ((resume-p   (and cached
                                (action-trace--resumable-p
                                 (action-trace-history-segments cached) states)))
               (history    (if resume-p
                               (action-trace--continue cached states)
                               (make-instance 'action-trace-history :segments states)))
               (index      (if resume-p (1- (length (action-trace-history-segments cached))) 0))
               (identifier (pathname-name identity)))
          (loop for (segment) in (nthcdr index states)
                for start = (action-trace-history-position history) then 0
                do (setf (action-trace-history-position history)
                         (conversation--map-records
                          (pathname segment)
                          (lambda (record)
                            (if (eq (first record) ':conversation)
                                (setf (action-trace-history-directory history)
                                      (or (action-trace-history-directory history)
                                          (let ((origin (getf (rest record) :directory)))
                                            (and origin (namestring origin)))))
                                (action-trace--reduce history record identifier)))
                          :start-position start)))
          (setf (gethash name *action-trace-cache*) history)
          history))))

(-> action-trace--continue (action-trace-history list) action-trace-history)
(defun action-trace--continue (history states)
  "Return a copy of HISTORY at segment STATES that later records can extend.

The newest trace and the run table are copied, so continuing leaves HISTORY,
which readers may still hold, unchanged. Finished traces never change and are
shared."
  (let ((runs    (make-hash-table :test #'equal))
        (current (action-trace-history-current history)))
    (maphash (lambda (key value) (setf (gethash key runs) value))
             (action-trace-history-runs history))
    (make-instance
     'action-trace-history
     :segments  states
     :position  (action-trace-history-position history)
     :directory (action-trace-history-directory history)
     :current   (and current
                     (make-instance 'action-trace
                                    :conversation  (action-trace-conversation current)
                                    :sequence      (action-trace-sequence current)
                                    :input         (action-trace-input current)
                                    :calls         (mapcar #'copy-list (action-trace-calls current))
                                    :requests      (action-trace-requests current)
                                    :tokens        (action-trace-tokens current)
                                    :usage-known-p (action-trace-usage-known-p current)
                                    :answered-p    (action-trace-answered-p current)
                                    :aborted-p     (action-trace-aborted-p current)
                                    :weight        (action-trace-weight current)
                                    :released-p    (action-trace-released-p current)))
     :finished  (action-trace-history-finished history)
     :runs      runs)))


;;;; -- Reduction --

(-> action-trace--reduce (action-trace-history list string) null)
(defun action-trace--reduce (history record identifier)
  "Fold durable conversation RECORD of conversation IDENTIFIER into HISTORY.

A user message finishes the newest trace and starts another; function calls,
their results, the model's answers, aborts, and model requests extend the
newest one. Compaction requests are not counted. A trace that can no longer
be learned from releases its payload as soon as that is known."
  (let* ((properties (rest record))
         (trace      (action-trace-history-current history))
         (current    (and trace (not (action-trace-released-p trace)) trace)))
    (case (first record)
      (:message
       (when (and (eq (getf properties :role) ':user)
                  (not (getf properties :automatic-p))
                  (stringp (getf properties :content)))
         (action-trace--finish history)
         (let ((input (getf properties :content)))
           (setf (action-trace-history-current history)
                 (make-instance 'action-trace
                                :conversation identifier
                                :sequence     (or (getf properties :seq) 0)
                                :input        input
                                :calls        nil
                                :weight       (length input)))
           (action-trace--check-weight (action-trace-history-current history)))))
      (:provider-item
       (let* ((wire (getf properties :wire-json))
              (item (and current
                         (stringp wire)
                         (handler-case (json-decode wire)
                           (json-error ()
                             nil))))
              (call (and (json-object-p item) (action-trace--call item))))
         (cond
           ((and call (>= (length (action-trace-calls current)) *action-trace-call-limit*))
            (action-trace--release current))
           (call
            (setf (action-trace-calls current)
                  (append (action-trace-calls current) (list call))
                  (action-trace-answered-p current) nil)
            (incf (action-trace-weight current)
                  (length (or (json-get item "arguments") "")))
            (action-trace--check-weight current))
           ((and current
                 (json-object-p item)
                 (equal (json-get item "type") "message")
                 (equal (json-get item "role") "assistant")
                 (non-empty-string-p (response-item-assistant-text item)))
            (setf (action-trace-answered-p current) t)))))
      (:procedure-run
       (let ((call (and current
                        (find (getf properties :call-id) (action-trace-calls current)
                              :key (lambda (call) (getf call :call-id)) :test #'equal))))
         (when (and call (equal (getf call :tool) "procedure.run")
                    (equal (json-get (getf call :arguments) "id") (getf properties :id)))
           (decf (action-trace-weight current)
                 (action-trace--procedure-weight (getf call :procedure)))
           (setf (getf call :procedure-version) (getf properties :procedure-version)
                 (getf call :procedure) (getf properties :procedure))
           (incf (action-trace-weight current)
                 (action-trace--procedure-weight (getf call :procedure)))
           (action-trace--check-weight current))))
      (:turn-aborted
       (when current
         (setf (action-trace-aborted-p current) t)
         (action-trace--release current)))
      (:tool-result
       (let ((call (and current
                        (find (getf properties :call-id) (action-trace-calls current)
                              :key  (lambda (call) (getf call :call-id))
                              :test #'equal))))
         (when call
           (if (eq (getf properties :status) ':ok)
               (let ((output (action-trace--retained-output (getf properties :output))))
                 (setf (getf call :status) ':ok
                       (getf call :output) output)
                 (incf (action-trace-weight current) (length (or output "")))
                 (action-trace--check-weight current))
               (progn
                 (setf (getf call :status) (getf properties :status))
                 (action-trace--release current))))))
      (:provider
       (let ((metadata (getf properties :metadata)))
         (when (and current
                    (getf metadata :response-id)
                    (not (getf metadata :compaction)))
           (incf (action-trace-requests current))
           (let ((tokens (conversation--usage-total (getf metadata :usage))))
             (unless tokens
               (setf (action-trace-usage-known-p current) nil))
             (incf (action-trace-tokens current) (or tokens 0)))))))
    nil))

(-> action-trace--finish (action-trace-history) null)
(defun action-trace--finish (history)
  "Retire HISTORY's newest trace, keeping it only when it was accepted.

An accepted trace joins the finished traces and records its measured
procedure runs; both keep only their newest *ACTION-TRACE-LIMIT* entries."
  (let ((trace (action-trace-history-current history)))
    (when (and trace (action-trace-accepted-p trace))
      (let ((finished (cons trace (action-trace-history-finished history))))
        (setf (action-trace-history-finished history)
              (subseq finished 0 (min (length finished) *action-trace-limit*))))
      (loop for (identifier version procedure) in (action-trace-procedure-runs trace)
            for key = (list identifier version)
            for runs = (cons (list :procedure procedure
                                   :requests  (action-trace-requests trace)
                                   :tokens    (action-trace-tokens trace))
                             (gethash key (action-trace-history-runs history)))
            do (setf (gethash key (action-trace-history-runs history))
                     (subseq runs 0 (min (length runs) *action-trace-limit*)))))
    (setf (action-trace-history-current history) nil))
  nil)

(-> action-trace--check-weight (action-trace) null)
(defun action-trace--check-weight (trace)
  "Release TRACE once its payload exceeds the weight limit."
  (when (> (action-trace-weight trace) *action-trace-weight-limit*)
    (action-trace--release trace))
  nil)

(-> action-trace--release (action-trace) null)
(defun action-trace--release (trace)
  "Mark TRACE as unlearnable and drop its input, calls and results."
  (setf (action-trace-released-p trace) t
        (action-trace-input trace)      ""
        (action-trace-calls trace)      nil
        (action-trace-weight trace)     0)
  nil)

(-> action-trace--call (json-object) (option list))
(defun action-trace--call (item)
  "Return the call plist of provider function-call ITEM, or NIL for another item."
  (when (equal (json-get item "type") "function_call")
    (list :tool              (cl-llm-provider-api:provider-call-canonical-name item)
          :arguments         (or (handler-case (json-decode (or (json-get item "arguments") "{}"))
                                   (json-error ()
                                     nil))
                                 (json-object))
          :status            nil
          :output            nil
          :call-id           (json-get item "call_id")
          :procedure-version nil
          :procedure         nil)))

(-> action-trace--retained-output (t) (option string))
(defun action-trace--retained-output (output)
  "Return call OUTPUT when it may be a JSON object or array no longer than the limit, else NIL."
  (and (stringp output)
       (<= (length output) *action-trace-output-limit*)
       (let ((start (position-if-not (lambda (character)
                                       (find character '(#\Space #\Tab #\Newline #\Return)))
                                     output)))
         (and start (find (char output start) "{[")))
       output))
