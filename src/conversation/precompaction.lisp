(in-package #:autolith)

;;;; -- Mechanical Precompaction --

(defparameter *conversation-precompaction-output-maximum-characters* 4096
  "Maximum old tool output retained verbatim in the provider projection.")

(defmethod conversation--project-record
    ((kind (eql :precompaction)) conversation properties)
  "Replay bounded output at the same point as the original provider projection."
  (declare (ignore properties))
  (conversation-precompact conversation)
  nil)


;;;; -- Public Functions --

(-> conversation-note-failed-call (conversation t) null)
(defun conversation-note-failed-call (conversation call-id)
  "Remember that CALL-ID's tool result failed in CONVERSATION."
  (when (non-empty-string-p call-id)
    (setf (gethash call-id (conversation-failed-call-ids conversation)) t))
  nil)

(-> conversation-precompact (conversation) (integer 0))
(defun conversation-precompact (conversation)
  "Bound oversized old tool output without deleting evidence from CONVERSATION.

Only completed exchanges before the current user turn are bounded. Failed,
denied, read-only, and repeated calls are never discarded because status or
matching text cannot establish that an external effect was absent. The durable
conversation record is unchanged. Return the number of bounded outputs."
  (with-recursive-lock-held ((conversation-append-lock conversation))
    (let* ((items    (coerce (conversation-input-items conversation) 'list))
           (boundary (or (position-if #'conversation-precompaction--user-message-p
                                      items :from-end t)
                         0))
           (index    0)
           (count    0))
      (setf (conversation-input-items conversation)
            (mapcar (lambda (item)
                      (prog1
                          (if (and (< index boundary)
                                   (conversation-precompaction--oversized-output-p item))
                              (progn
                                (incf count)
                                (conversation-precompaction--bounded-output
                                 conversation item))
                              item)
                        (incf index)))
                    items))
      count)))

(-> conversation-prepare-provider-request
    (conversation &key (:identity list) (:now integer) (:idle-seconds (option real)))
    (integer 0))
(defun conversation-prepare-provider-request
    (conversation &key identity (now (get-universal-time)) idle-seconds)
  "Precompact cold or estimated-expired context, then note this request.

IDENTITY names the provider and model serving the request. Bound old output
on a provider/model change or after IDLE-SECONDS since the last response.
After resume, preserve a known warm prefix and replay recorded precompaction.
Unknown retention disables idle expiry but treats a resumed cache as cold.
Return how many outputs were bounded."
  (let ((previous-identity  (conversation-request-identity conversation))
        (previous-processed (conversation-prompt-processed-at conversation)))
    (prog1 (if (if previous-identity
                   (or (not (equal identity previous-identity))
                       (and previous-processed idle-seconds (plusp idle-seconds)
                            (>= (- now previous-processed) idle-seconds)))
                   (and (conversation-resumed-p conversation)
                        (conversation-precompaction--resume-cold-p
                         conversation
                         :identity     identity
                         :now          now
                         :idle-seconds idle-seconds)))
               (with-recursive-lock-held ((conversation-append-lock conversation))
                 (let ((count (conversation-precompact conversation)))
                   (when (plusp count)
                     (conversation-append-record
                       conversation (list :precompaction :outputs count)))
                   count))
               0)
      (setf (conversation-request-identity conversation)    identity
            (conversation-prompt-processed-at conversation) now))))

(-> conversation-note-provider-response (conversation &key (:now integer)) null)
(defun conversation-note-provider-response (conversation &key (now (get-universal-time)))
  "Note that a provider just completed a response to CONVERSATION's prompt.

The prompt cache is warm from that moment, however long the request spent
failing and retrying before it, so idle pruning measures from here."
  (setf (conversation-prompt-processed-at conversation) now)
  nil)

(-> conversation-note-response-record (conversation list) null)
(defun conversation-note-response-record (conversation properties)
  "Remember the request identity and time of a completed response's provider record.

PROPERTIES belong to a durable provider record. Records of failed attempts and
records written before responses carried their identity are ignored."
  (let ((identity (getf (getf properties :metadata) :request-identity))
        (time     (getf properties :time)))
    (when (and (consp identity) (typep time 'timestamp))
      (setf (conversation-last-response conversation) (list identity time))))
  nil)


;;;; -- Private Functions --

(-> conversation-precompaction--resume-cold-p
    (conversation &key (:identity list) (:now integer) (:idle-seconds (option real)))
    boolean)
(defun conversation-precompaction--resume-cold-p
    (conversation &key identity now idle-seconds)
  "Return true unless CONVERSATION's latest durable response shows a warm cache.

A warm cache needs a response from the same IDENTITY less than IDLE-SECONDS
before NOW. Unknown retention, a conversation whose latest response predates
recorded identities, and a different provider or model all count as cold."
  (destructuring-bind (&optional previous-identity previous-time)
      (conversation-last-response conversation)
    (not (and previous-identity
              (equal identity previous-identity)
              idle-seconds
              (plusp idle-seconds)
              (< (- now previous-time) idle-seconds)))))

(-> conversation-precompaction--oversized-output-p (t) boolean)
(defun conversation-precompaction--oversized-output-p (item)
  "Return true when ITEM contains output requiring a bounded projection."
  (let ((output (and (json-object-p item) (json-get item "output"))))
    (and (clinker-transcript:function-call-output-item-p item)
         (stringp output)
         (> (length output)
            *conversation-precompaction-output-maximum-characters*))))

(-> conversation-precompaction--bounded-output (conversation json-object) json-object)
(defun conversation-precompaction--bounded-output (conversation item)
  "Return ITEM with head/tail excerpts and a retrievable durable evidence reference."
  (let* ((output    (json-get item "output"))
         (call-id   (json-get item "call_id"))
         (limit     *conversation-precompaction-output-maximum-characters*)
         (reference (format nil
                            "[Full evidence: read conversation:id/~A with query ~S. Result: ~A; external effects unknown.]"
                            (conversation-identifier conversation) call-id
                            (if (gethash call-id (conversation-failed-call-ids conversation))
                                "failed"
                                "status unknown")))
         (available (max 0 (- limit (length reference) 8)))
         (head      (ceiling available 2))
         (tail      (floor available 2))
         (copy      (json-object-copy item)))
    (setf (gethash "output" copy)
          (format nil "~A~%[...]~%~A~%~A"
                  (subseq output 0 head)
                  (subseq output (- (length output) tail))
                  reference))
    copy))

(-> conversation-precompaction--user-message-p (t) boolean)
(defun conversation-precompaction--user-message-p (item)
  "Return true when ITEM is a user message."
  (and (json-object-p item)
       (equal (json-get item "role") "user")
       t))
