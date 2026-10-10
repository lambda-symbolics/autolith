(in-package #:autolith)

;;;; -- Conversation History Resources --

;;; conversation:current exposes the requesting conversation's complete
;;; durable history, including everything compaction removed from the model
;;; context. Another conversation is read only when its identifier is named
;;; with conversation:id/<id>; there is deliberately no search across
;;; conversations. Reads page by durable record sequence or search one
;;; conversation's records.

(defparameter *conversation-resource-current-identifier* "current"
  "The reserved identifier naming the requesting conversation.")

(defparameter *conversation-resource-identifier-prefix* "id/"
  "The identifier prefix naming another conversation by its identifier.")

(defparameter *conversation-resource-default-record-count* 20
  "The number of records one conversation window returns by default.")

(defparameter *conversation-resource-maximum-record-count* 200
  "The largest number of records one conversation window returns.")

(defparameter *conversation-resource-default-results* 20
  "The number of matches one conversation search returns by default.")

(defparameter *conversation-resource-maximum-results* 50
  "The largest number of matches one conversation search returns.")

(defparameter *conversation-resource-maximum-characters* 7000
  "The most characters a window or search returns, below the tool result bound.

Only a record read alone may exceed it, so its complete text is spilled to a
readable context object instead of being cut.")

(defparameter *conversation-resource-record-characters* 1500
  "The most characters one record shows inside a multi-record window.")

(defparameter *conversation-resource-excerpt-characters* 80
  "The characters shown on either side of a search match.")

(defparameter *conversation-resource-compactions-listed* 3
  "The number of most recent compaction sequences a window header names.")

(defclass conversation-resource (resource)
  ((conversation
    :initarg :conversation
    :reader conversation-resource-conversation
    :type conversation
    :documentation "The conversation whose durable history this resource reads."))
  (:documentation "The read-only durable history of one conversation."))

(defclass conversation-resolver (resource-resolver)
  ()
  (:documentation
   "Resolve the requesting conversation, or another one named by identifier."))


;;;; -- URI Resolution --

(defmethod resource-resolver-read-documentation ((resolver conversation-resolver))
  "Document conversation: reads."
  (declare (ignore resolver))
  "conversation:current exposes durable records, paginated by start-sequence/record-count or searched with query. conversation:id/<id> names another conversation.")

(defmethod resource-resolver-resolve
    ((resolver conversation-resolver) identifier (context tool-context))
  "Resolve conversation:current or one named conversation:id/<id>."
  (let ((prefix *conversation-resource-identifier-prefix*))
    (cond
      ((string= identifier *conversation-resource-current-identifier*)
       (make-instance 'conversation-resource
                      :uri (format nil "~A:~A"
                                   (resource-resolver-scheme resolver)
                                   identifier)
                      :conversation (tool-context-conversation context)))
      ((and (> (length identifier) (length prefix))
            (string= prefix identifier :end2 (length prefix)))
       (let ((conversation
               (conversation-replay-load (tool-context-configuration context)
                                         (subseq identifier (length prefix)))))
         (make-instance 'conversation-resource
                        :uri (format nil "~A:~A~A"
                                     (resource-resolver-scheme resolver)
                                     prefix
                                     (conversation-identifier conversation))
                        :conversation conversation)))
      (t
       (error 'resource-operation-unsupported
              :uri (format nil "~A:~A"
                           (resource-resolver-scheme resolver)
                           identifier)
              :operation ':resolve)))))

(defmethod resource-capabilities
    ((resource conversation-resource) (context tool-context))
  "Expose conversation history as read-only."
  (declare (ignore resource context))
  '(:read))


;;;; -- Model-Facing Reads --

(defmethod resource-tool-read
    ((resource conversation-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Search RESOURCE's records, or return one window of them by sequence."
  (declare (ignore tool context))
  (when (or (nth-value 1 (gethash "start-line" arguments))
            (nth-value 1 (gethash "line-count" arguments)))
    (error 'tool-error
           :message "conversation: resources page by start-sequence and record-count, not line windows."
           :code ':line-windows-unsupported
           :tool-name "resource.read"))
  (let ((query          (tool-argument arguments "query"))
        (maximum        (conversation-resource--integer-argument
                         arguments "max-results"
                         *conversation-resource-maximum-results*))
        (start-sequence (conversation-resource--integer-argument
                         arguments "start-sequence" most-positive-fixnum))
        (record-count   (conversation-resource--integer-argument
                         arguments "record-count"
                         *conversation-resource-maximum-record-count*)))
    (cond
      (query
       (when (or start-sequence record-count)
         (error 'tool-error
                :message "A conversation search does not accept start-sequence or record-count; read the window around a match separately."
                :tool-name "resource.read"))
       (let ((terms (and (stringp query) (conversation-search-terms query))))
         (unless terms
           (error 'tool-error
                  :message "Conversation resource query must contain at least one term."
                  :tool-name "resource.read"))
         (tool-success
          (conversation-resource--render-search
           resource terms
           (or maximum *conversation-resource-default-results*)))))
      (maximum
       (error 'tool-error
              :message "Conversation resource max-results applies only with query."
              :tool-name "resource.read"))
      (t
       (tool-success
        (conversation-resource--render-window
         resource
         :start-sequence start-sequence
         :record-count (or record-count
                           *conversation-resource-default-record-count*)))))))


;;;; -- Read Arguments and Rendering --

(-> conversation-resource--integer-argument
    (hash-table string (integer 1))
    (option (integer 1)))
(defun conversation-resource--integer-argument (arguments name maximum)
  "Return ARGUMENTS' optional integer NAME, between 1 and MAXIMUM."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       nil)
      ((and (integerp value) (<= 1 value maximum))
       value)
      (t
       (error 'tool-error
              :message (format nil "~A must be an integer between 1 and ~D."
                               name maximum)
              :tool-name "resource.read")))))

(-> conversation-resource--label (conversation-resource) string)
(defun conversation-resource--label (resource)
  "Return the displayed identifier of RESOURCE's conversation."
  (conversation-identifier-display
   (conversation-identifier (conversation-resource-conversation resource))))

(-> conversation-resource--record-heading (list) string)
(defun conversation-resource--record-heading (record)
  "Return the bracketed sequence, time, and kind heading of projected RECORD."
  (let ((time (getf (rest record) :time)))
    (format nil "[sequence ~A | ~A | ~A]"
            (or (getf (rest record) :seq) "?")
            (conversation-replay-timestamp-string
             (and (typep time '(integer 0)) time))
            (conversation-replay-record-kind record))))

(-> conversation-resource--render-record (list (option (integer 1))) string)
(defun conversation-resource--render-record (record maximum)
  "Return projected RECORD's heading and body, bounded to MAXIMUM body characters.

A NIL MAXIMUM shows the complete body."
  (let* ((body        (with-output-to-string (stream)
                        (conversation-replay-write-record-body record stream)))
         (truncated-p (and maximum (> (length body) maximum))))
    (format nil "~A~%~A~:[~;[record truncated after ~D of ~D characters; read it alone with start-sequence and record-count 1]~%~]"
            (conversation-resource--record-heading record)
            (if truncated-p
                (format nil "~A~%" (subseq body 0 maximum))
                body)
            truncated-p
            maximum
            (length body))))

(-> conversation-resource--compaction-note (conversation) string)
(defun conversation-resource--compaction-note (conversation)
  "Return how often CONVERSATION was compacted and where the latest began."
  (let* ((sequences (conversation-compaction-sequences conversation))
         (latest    (reverse
                     (last sequences
                           *conversation-resource-compactions-listed*))))
    (if sequences
        (format nil "Compacted ~D time~:P; latest at sequence~P ~{~D~^, ~}."
                (length sequences) (length latest) latest)
        "Never compacted.")))

(-> conversation-resource--fitting-count (list) (integer 0))
(defun conversation-resource--fitting-count (texts)
  "Return how many leading TEXTS fit the read character limit, at least one."
  (loop with total = 0
        for text in texts
        for count from 0
        do (incf total (length text))
           (when (and (plusp count)
                      (> total *conversation-resource-maximum-characters*))
             (return count))
        finally (return (length texts))))

(-> conversation-resource--render-window
    (conversation-resource
     &key (:start-sequence (option (integer 1))) (:record-count (integer 1)))
    string)
(defun conversation-resource--render-window
    (resource &key start-sequence record-count)
  "Return RECORD-COUNT records of RESOURCE from START-SEQUENCE, or its newest.

Once the window reaches the character limit, records are dropped from its
far end, so a paged window keeps its beginning and the newest window keeps
its end."
  (let ((conversation (conversation-resource-conversation resource))
        (records      nil)
        (more-p       nil))
    (if start-sequence
        (multiple-value-setq (records more-p)
          (conversation-records-from conversation start-sequence record-count))
        (setf records (conversation-records-newest conversation record-count)))
    (let* ((record-limit (and (rest records)
                              *conversation-resource-record-characters*))
           (texts        (mapcar (lambda (record)
                                   (conversation-resource--render-record
                                    record record-limit))
                                 records))
           (shown-count  (conversation-resource--fitting-count
                          (if start-sequence texts (reverse texts))))
           (omitted-p    (< shown-count (length records)))
           (shown        (if start-sequence
                             (subseq records 0 shown-count)
                             (last records shown-count)))
           (shown-texts  (if start-sequence
                             (subseq texts 0 shown-count)
                             (last texts shown-count)))
           (newest       (getf (rest (first (last shown))) :seq)))
      (with-output-to-string (stream)
        (if (null records)
            (format stream "Conversation ~A has no records~@[ from sequence ~D~]. ~A~%"
                    (conversation-resource--label resource)
                    start-sequence
                    (conversation-resource--compaction-note conversation))
            (progn
              (format stream "Conversation ~A, ~:[newest ~;~]record~P ~A-~A. ~A~%"
                      (conversation-resource--label resource)
                      start-sequence
                      shown-count
                      (getf (rest (first shown)) :seq)
                      newest
                      (conversation-resource--compaction-note conversation))
              (when (and omitted-p (not start-sequence))
                (format stream "[earlier records omitted at the character limit]~%"))
              (dolist (text shown-texts)
                (write-string text stream))
              (when (and start-sequence (or more-p omitted-p) (integerp newest))
                (format stream "[more records follow; continue with start-sequence ~D]~%"
                        (1+ newest)))))))))

(-> conversation-resource--excerpt (string list) string)
(defun conversation-resource--excerpt (text terms)
  "Return TEXT around its first occurrence of TERMS' first term, on one line."
  (let* ((position (or (conversation-search-text-position text (first terms)) 0))
         (radius   *conversation-resource-excerpt-characters*)
         (start    (max 0 (- position radius)))
         (end      (min (length text) (+ position (length (first terms)) radius)))
         (excerpt  (with-output-to-string (stream)
                     (loop with space-p = nil
                           for character across (subseq text start end)
                           do (if (member character
                                          '(#\Space #\Tab #\Newline #\Return #\Page))
                                  (setf space-p t)
                                  (progn
                                    (when space-p
                                      (write-char #\Space stream)
                                      (setf space-p nil))
                                    (write-char character stream)))))))
    (format nil "~:[~;...~]~A~:[~;...~]"
            (plusp start) (string-trim " " excerpt) (< end (length text)))))

(-> conversation-resource--render-search
    (conversation-resource list (integer 1))
    string)
(defun conversation-resource--render-search (resource terms maximum)
  "Return the newest MAXIMUM records of RESOURCE matching every term in TERMS."
  (let ((matches (conversation-search (conversation-resource-conversation resource)
                                      terms
                                      :limit maximum)))
    (with-output-to-string (stream)
      (if (null matches)
          (format stream "No records of conversation ~A contain ~{~S~^ and ~}.~%"
                  (conversation-resource--label resource) terms)
          (progn
            (format stream "~D record~:P of conversation ~A contain~:[s~;~] ~{~S~^ and ~}, newest first~:[~;, limited to ~D~]. Read start-sequence with a match's sequence to see its context.~%"
                    (length matches)
                    (conversation-resource--label resource)
                    (/= (length matches) 1)
                    terms
                    (= (length matches) maximum)
                    maximum)
            (let* ((lines (mapcar (lambda (match)
                                    (format nil "~A ~A~%"
                                            (conversation-resource--record-heading
                                             (conversation-search-match-record match))
                                            (conversation-resource--excerpt
                                             (conversation-search-match-text match)
                                             terms)))
                                  matches))
                   (shown (conversation-resource--fitting-count lines)))
              (dolist (line (subseq lines 0 shown))
                (write-string line stream))
              (when (< shown (length lines))
                (format stream "[~D older match~:P omitted at the character limit]~%"
                        (- (length lines) shown)))))))))
