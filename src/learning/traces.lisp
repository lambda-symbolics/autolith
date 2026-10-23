(in-package #:autolith)

;;;; -- Action Traces --

;;; An action trace is what one user turn did: the tool calls in order, each
;;; with its arguments, status, and output, and the model requests and tokens
;;; the turn used. Traces are read from conversation logs and kept only in
;;; memory, so the transcript stays the one durable record of what happened.

(defparameter *action-trace-call-limit* 32
  "The most tool calls a trace may have to be used for learning.")

(defparameter *action-trace-limit* 20
  "The most recent traces one learning pass reads.")

(defparameter *action-trace-conversation-limit* 10
  "The most recent conversations one learning pass reads traces from.")

(defparameter *action-trace-output-limit* 16384
  "The longest call result a trace keeps; longer results are dropped.")

(defvar *action-trace-cache* (make-hash-table :test #'equal :synchronized t)
  "(SEGMENTS POSITION DIRECTORY TRACES) of each read conversation by identity namestring.

SEGMENTS are the (NAMESTRING FILE-IDENTITY SIZE) states read, POSITION is where
reading stopped in the last one, DIRECTORY is the conversation's workspace,
and TRACES are newest first.")

(defclass action-trace ()
  ((conversation
    :initarg :conversation
    :reader action-trace-conversation
    :type string
    :documentation "The conversation the turn belongs to.")
   (sequence
    :initarg :sequence
    :reader action-trace-sequence
    :type integer
    :documentation "The durable sequence of the user message that started the turn.")
   (input
    :initarg :input
    :reader action-trace-input
    :type string
    :documentation "The user message that started the turn.")
   (calls
    :initarg :calls
    :accessor action-trace-calls
    :type list
    :documentation "(:TOOL :ARGUMENTS :STATUS :OUTPUT :CALL-ID) for each tool call in order.

Procedure calls also retain :PROCEDURE-VERSION and :PROCEDURE provenance.
OUTPUT is kept only when it is a JSON object or array of bounded length,
the only results later calls can take values from.")
   (requests
    :initarg :requests
    :initform 0
    :accessor action-trace-requests
    :type (integer 0)
    :documentation "How many model requests the turn made.")
   (tokens
    :initarg :tokens
    :initform 0
    :accessor action-trace-tokens
    :type (integer 0)
    :documentation "How many tokens those requests used in total.")
   (usage-known-p
    :initarg :usage-known-p
    :initform t
    :accessor action-trace-usage-known-p
    :type boolean
    :documentation "Whether every counted model request supplied total token usage.")
   (answered-p
    :initarg :answered-p
    :initform nil
    :accessor action-trace-answered-p
    :type boolean
    :documentation "Whether the model answered after the turn's last tool call.")
   (aborted-p
    :initarg :aborted-p
    :initform nil
    :accessor action-trace-aborted-p
    :type boolean
    :documentation "Whether the turn was cancelled or failed before it finished."))
  (:documentation "The tool calls and model cost of one user turn."))

(-> action-trace-skeleton (action-trace) list)
(defun action-trace-skeleton (trace)
  "Return the ordered tool names TRACE called."
  (mapcar (lambda (call) (getf call :tool)) (action-trace-calls trace)))

(-> action-trace-accepted-p (action-trace) boolean)
(defun action-trace-accepted-p (trace)
  "Return true when TRACE is a finished turn worth learning from.

It made at least one call and no more than the limit, every call succeeded,
the model answered after the last one, and the turn was not cancelled or
abandoned on an error."
  (let ((calls (action-trace-calls trace)))
    (and calls
         (<= (length calls) *action-trace-call-limit*)
         (every (lambda (call) (eq (getf call :status) ':ok)) calls)
         (action-trace-answered-p trace)
         (not (action-trace-aborted-p trace))
         t)))

(-> action-traces-in-conversation (pathname &key (:workspace (option string))) list)
(defun action-traces-in-conversation (pathname &key workspace)
  "Return the traces of the conversation at PATHNAME, oldest first.

When WORKSPACE is given, a conversation started in another workspace yields
none. Unreadable storage yields none instead of failing the caller."
  (handler-case
      (destructuring-bind (directory traces) (action-trace--read pathname)
        (if (and workspace directory (not (equal directory workspace)))
            nil
            (reverse traces)))
    (error ()
      nil)))

(-> action-traces (configuration &key (:limit integer) (:predicate function)) list)
(defun action-traces (configuration &key (limit *action-trace-limit*)
                                         (predicate #'action-trace-accepted-p))
  "Return up to LIMIT recent traces of CONFIGURATION's workspace satisfying PREDICATE, newest first.

By default only traces accepted for learning are returned. Cached traces of
conversations this read did not reach are released once the cache holds
twice as many conversations as one read visits."
  (let ((workspace (knowledge--workspace configuration))
        (collected nil)
        (visited nil))
    (loop for pathname in (conversation-list configuration)
          repeat *action-trace-conversation-limit*
          do (push (namestring (conversation-storage-identity-pathname pathname)) visited)
             (setf collected
                   (append collected
                           (reverse (remove-if-not predicate
                                                   (action-traces-in-conversation
                                                    pathname :workspace workspace)))))
          until (>= (length collected) limit))
    (when (> (hash-table-count *action-trace-cache*) (* 2 *action-trace-conversation-limit*))
      (loop for name in (loop for name being the hash-keys of *action-trace-cache* collect name)
            unless (member name visited :test #'string=)
              do (remhash name *action-trace-cache*)))
    (subseq collected 0 (min limit (length collected)))))

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

(-> action-trace--read (pathname) list)
(defun action-trace--read (pathname)
  "Return (DIRECTORY TRACES) for the conversation at PATHNAME, newest trace first.

Only records appended since the previous read are visited: archived segments
never change and the active one only grows, so a cached read continues where
it stopped. Any other change rereads every segment."
  (let* ((identity (conversation-storage-identity-pathname pathname))
         (name (namestring identity))
         (states (action-trace--segment-states identity))
         (cached (gethash name *action-trace-cache*)))
    (destructuring-bind (&optional cached-states position directory traces) cached
      (cond
        ((and cached (equal cached-states states))
         (list directory traces))
        (t
         (let* ((resume-p (and cached (action-trace--resumable-p cached-states states)))
                (index (if resume-p (1- (length cached-states)) 0))
                (position (if resume-p position 0))
                (directory (and resume-p directory))
                (traces (if resume-p (action-trace--reopen traces) nil))
                (identifier (pathname-name identity)))
           (loop for (segment) in (nthcdr index states)
                 for start = position then 0
                 do (setf position
                          (conversation--map-records
                           (pathname segment)
                           (lambda (record)
                             (if (eq (first record) ':conversation)
                                 (setf directory
                                       (or directory
                                           (let ((origin (getf (rest record) :directory)))
                                             (and origin (namestring origin)))))
                                 (setf traces (action-trace--reduce traces record identifier))))
                           :start-position start)))
           (setf (gethash name *action-trace-cache*)
                 (list states position directory traces))
           (list directory traces)))))))

(-> action-trace--reopen (list) list)
(defun action-trace--reopen (traces)
  "Return TRACES with the newest one copied, so continuing it leaves cached traces unchanged."
  (if traces
      (let ((trace (first traces)))
        (cons (make-instance 'action-trace
                             :conversation  (action-trace-conversation trace)
                             :sequence      (action-trace-sequence trace)
                             :input         (action-trace-input trace)
                             :calls         (mapcar #'copy-list (action-trace-calls trace))
                             :requests      (action-trace-requests trace)
                             :tokens        (action-trace-tokens trace)
                             :usage-known-p (action-trace-usage-known-p trace)
                             :answered-p    (action-trace-answered-p trace)
                             :aborted-p     (action-trace-aborted-p trace))
              (rest traces)))
      nil))

(-> action-trace--reduce (list list string) list)
(defun action-trace--reduce (traces record identifier)
  "Return TRACES, newest first, after durable conversation RECORD of conversation IDENTIFIER.

A user message starts a trace; function calls, their results, the model's
answers, aborts, and model requests extend the newest one. Compaction
requests are not counted."
  (let ((properties (rest record))
        (current    (first traces)))
    (case (first record)
      (:message
       (when (and (eq (getf properties :role) ':user)
                  (not (getf properties :automatic-p))
                  (stringp (getf properties :content)))
         (push (make-instance 'action-trace
                              :conversation identifier
                              :sequence     (or (getf properties :seq) 0)
                              :input        (getf properties :content)
                              :calls        nil)
               traces)))
      (:provider-item
       (let* ((item (ignore-errors (json-decode (getf properties :wire-json))))
              (call (and current (json-object-p item) (action-trace--call item))))
         (cond
           (call
            (setf (action-trace-calls current)
                  (append (action-trace-calls current) (list call))
                  (action-trace-answered-p current) nil))
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
           (setf (getf call :procedure-version) (getf properties :procedure-version)
                 (getf call :procedure) (getf properties :procedure)))))
      (:turn-aborted
       (when current
         (setf (action-trace-aborted-p current) t)))
      (:tool-result
       (let ((call (and current
                        (find (getf properties :call-id) (action-trace-calls current)
                              :key (lambda (call) (getf call :call-id))
                              :test #'equal))))
         (when call
           (setf (getf call :status) (getf properties :status)
                 (getf call :output) (action-trace--retained-output
                                      (getf properties :output))))))
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
    traces))

(-> action-trace--call (json-object) (option list))
(defun action-trace--call (item)
  "Return the call plist of provider function-call ITEM, or NIL for another item."
  (when (equal (json-get item "type") "function_call")
    (list :tool (cl-llm-provider-api:provider-call-canonical-name item)
          :arguments (or (ignore-errors (json-decode (or (json-get item "arguments") "{}")))
                         (json-object))
          :status nil
          :output nil
          :call-id (json-get item "call_id")
          :procedure-version nil
          :procedure nil)))

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
