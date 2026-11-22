(in-package #:autolith)

;;;; -- Client Transcript History --

(-> application-client-history-builtin-p (application) boolean)
(defun application-client-history-builtin-p (application)
  "Return whether the effective HISTORY operation is the built-in transcript pager."
  (let ((operation (application-operation-find application 'history)))
    (and (typep operation 'application-command-operation)
         (let ((command (application-operation-backend operation)))
           (and (eq (application-command-definition-name command)
                    'application--builtin-history-command)
                (eq (application-command-handler command)
                    (symbol-function 'application--builtin-history-command)))))))

(defparameter *application-client-history-character-limit* (* 1024 1024)
  "Maximum serialized transcript content in one durable client history page.")

(-> application-client-history--frame-size (list) (option integer))
(defun application-client-history--frame-size (event)
  "Count a portable frame, or return NIL if it exceeds packet bounds."
  (handler-case
        (length (image-daemon:daemon-packet-string event))
    (image-daemon:daemon-error (condition)
      (unless (eq (image-daemon:daemon-error-operation condition) ':write-packet)
        (error condition))
      nil)
    (portable-value-error (condition)
      (unless (eq (portable-value-error-reason condition) ':nodes)
        (error condition))
      nil)))

(-> application-client-history-page
    (application integer &key (:character-limit (integer 0))) (values list list))
(defun application-client-history-page
    (application before-sequence &key (character-limit *application-client-history-character-limit*))
  "Return a bounded chronological page of durable and retained local output.

  Call under the presentation and conversation append locks. A local publication
  belongs to the page containing its preceding durable boundary. Equal boundaries
  retain publication order; timestamps never determine transcript placement."
  (let* ((conversation (application-conversation application))
         (ui           (application-presentation application))
         (local        (application-client-history--transient-events ui (conversation-identifier conversation)))
         (limit        (application--history-page-size)))
    (multiple-value-bind (records candidate-count outcomes sources)
        (application--history-page-records application before-sequence limit)
      (let* ((presentations
               (application--associate-tool-outcomes
                (loop for record in records
                      for presentation = (application--record-presentation
                                          application record :outcomes outcomes
                                          :source-cache                (gethash record sources))
                      when presentation collect presentation)))
             (wires    (getf (channel-presentation--records-fields ui presentations) :presentations))
             (selected nil)
             (floor    before-sequence)
             (upper    before-sequence)
             (more-p   (> candidate-count limit)))
        (dolist (record (reverse records))
          (let* ((sequence (application--record-sequence record))
                 (wire     (find sequence wires :key (lambda (wire) (third (first wire)))))
                 (events   (application-client-history--merge
                            (when wire (list (list :ui :operation ':records :presentations (list wire))))
                            local :floor  (if (and (eq record (first records))
                                                   (<= candidate-count limit))
                                              0 sequence)
                                  :before upper))
                 (sizes    (mapcar #'application-client-history--frame-size events))
                 (size     (when (every #'identity sizes) (reduce #'+ sizes :initial-value 0))))
            (unless (and size (<= size character-limit))
              (when selected
                (setf more-p t)
                (return))
              ;; A single boundary must make progress even when its original
              ;; output exceeds the page budget. Keep the newest local suffix
              ;; that fits beside the bounded durable lifecycle notice.
              (when wire
                (presentation--validate-transport-shape wire)
                (setf wire (presentation--overflow-record wire)))
              (setf events nil)
              (when wire
                (push (list :ui :operation ':records :presentations (list wire)) events))
              (let ((remaining (- character-limit
                                  (reduce #'+ events :key #'application-client-history--frame-size
                                          :initial-value  0)))
                    (kept      nil))
                (dolist (item (reverse (application-client-history--merge nil local :floor sequence :before upper)))
                  (let ((weight (application-client-history--frame-size item)))
                    (unless (and weight (<= weight remaining))
                      (return))
                    (decf remaining weight)
                    (push item kept)))
                (setf events (append events kept)
                      size (- character-limit remaining))))
            (unless (<= size character-limit)
              (setf more-p t)
              (return))
            (decf character-limit size)
            (setf selected (append events selected)
                  floor sequence
                  upper sequence)))
        (unless records
          (let ((kept nil))
            (dolist (event (reverse (application-client-history--merge nil local :floor 0 :before before-sequence)))
              (let ((size (application-client-history--frame-size event)))
                (unless (and size (<= size character-limit))
                  (return))
                (decf character-limit size)
                (push event kept)))
            (setf selected kept)))
        (values selected
                (list :conversation (conversation-identifier conversation)
                      :before-sequence floor :more-p more-p))))))

(-> application-client-history--durable-identifier-p (t string) boolean)
(defun application-client-history--durable-identifier-p (identifier conversation)
  "Return whether IDENTIFIER names one durable record of CONVERSATION."
  (and (consp identifier) (eq (first identifier) ':conversation)
       (equal (second identifier) conversation)
       (typep (third identifier) '(integer 1))
       (null (rest (rest (rest identifier))))))

(-> application-client-history--transient-events (channel-presentation string) list)
(defun application-client-history--transient-events (presentation conversation)
  "Return (BOUNDARY EVENT) pairs for retained local output in publication order."
  (loop for cell in (deque->list (channel-presentation-transcript-log presentation))
        for event = (first cell)
        for fields = (rest event)
        for operation = (getf fields :operation)
        for boundary = (sixth cell)
        unless (or (fifth cell)
                   (and boundary (not (equal (first boundary) conversation)))
                   (and (member operation '(:append :mark))
                        (application-client-history--durable-identifier-p
                         (getf fields :identifier) conversation)))
          append
          (when-let ((local (if (eq operation ':records)
                                (when-let ((records
                                             (remove-if
                                              (lambda (record)
                                                (application-client-history--durable-identifier-p
                                                 (first record) conversation))
                                              (getf fields :presentations))))
                                  (list :ui :operation ':records :presentations records))
                                event)))
            (list (list (or (second boundary) 0) local)))))

(-> application-client-history--merge (list list &key (:floor integer) (:before integer)) list)
(defun application-client-history--merge (events local &key floor before)
  "Interleave retained LOCAL output with durable EVENTS within [FLOOR, BEFORE)."
  (let ((pending (remove-if-not
                  (lambda (item)
                    (and (or (<= floor (first item))
                             (and (= floor 1) (zerop (first item))))
                         (< (first item) before)))
                  local))
        (merged  nil))
    (dolist (event events)
      (dolist (record (getf (rest event) :presentations))
        (let ((sequence (third (first record))))
          (loop while (and pending (< (first (first pending)) sequence))
                do (push (second (pop pending)) merged))
          (push (list :ui :operation ':records :presentations (list record)) merged))))
    (dolist (item pending)
      (push (second item) merged))
    (nreverse merged)))

(-> application-client-history--cut (application) (integer 1))
(defun application-client-history--cut (application)
  "Return the exclusive replay boundary, leaving unread records to an active stream."
  (1+ (if (application-stream-finish-function application)
          (application-rendered-sequence application)
          (application-clients--record-sequence application))))

(-> application-client-history-replay (application channel-presentation &key (:character-limit (integer 0))) list)
(defun application-client-history-replay (application presentation &key (character-limit *application-client-history-character-limit*))
  "Seed a fresh presenting client from durable history, retained local output and current state."
  (if (and (slot-boundp application 'conversation) (application-conversation application))
      (let* ((state         (rest (channel-presentation-replay presentation :transcript nil)))
             (active-stream (fullscreen-terminal-ui-markdown-stream presentation))
             (active-id     (when active-stream
                              (channel-presentation--stream-wire-identifier presentation active-stream)))
             (local         (application-client-history--transient-events
                             presentation (conversation-identifier (application-conversation application))))
             (seed          (and active-id
                                 (find-if (lambda (item)
                                            (let ((event (second item)))
                                              (and (eq (getf (rest event) :operation) ':markdown)
                                                   (equal (getf (rest event) :identifier) active-id)))) local)))
             (available     (- character-limit 1024)))
        ;; State and the active Markdown base must fit before selecting history.
        (dolist (event (if seed (cons (second seed) state) state))
          (let ((size (application-client-history--frame-size event)))
            (unless (and size (<= size available))
              (error 'world-terminal-error :operation ':client
                                      :message        "The channel's current presentation exceeds the attachment budget."))
            (decf available size)))
        (multiple-value-bind (events fields)
            (application-client-history-page application (application-client-history--cut application)
                                             :character-limit (min available *application-client-history-character-limit*))
          (append (list (list :ui :operation ':reset)) events
                  (when (and seed (not (member (second seed) events :test #'equal)))
                    (list (second seed)))
                  (list (list* :ui :operation ':transcript-history fields)) state)))
      (channel-presentation-replay presentation)))

(-> application-client-history-load (application &key (:conversation string) (:before-sequence integer)) list)
(defun application-client-history-load (application &key conversation before-sequence)
  "Return one bounded durable page in a correlated reply, outside the view's event queue.

An operation overridden since attachment is declined without executing the pager."
  (if (not (application-client-history-builtin-p application))
      (list :history-page-p t :builtin-p nil)
      (let ((presentation (application-presentation application)))
        (with-recursive-lock-held ((application-render-lock application))
          (let ((current (application-conversation application)))
            (unless (and presentation current
                         (equal conversation (conversation-identifier current))
                         (typep before-sequence '(integer 1)))
              (error 'world-terminal-error :operation ':transcript-history
                                       :message       "The transcript history cursor does not name this channel's conversation."))
            (with-terminal-ui-locked (presentation)
              (with-recursive-lock-held ((conversation-append-lock current))
                (let ((before (min before-sequence (application-client-history--cut application))))
                  (multiple-value-bind (events fields)
                      (application-client-history-page
                       application before :character-limit
                       (max 0 (min *application-client-history-character-limit*
                                   (- image-daemon:*daemon-packet-character-limit* 1024)
                                   (- (floor image-daemon:*attachment-queue-character-limit* 2) 1024))))
                    (when (and (getf fields :more-p) (= (getf fields :before-sequence) before))
                      (error 'world-terminal-error :operation ':transcript-history
                                               :message       "The attachment budget is too small for the next transcript record."))
                    (list* :history-page-p t :builtin-p t :events events fields))))))))))
