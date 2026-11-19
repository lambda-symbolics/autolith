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
      (length (image-daemon:daemon-packet-string (client-portable event)))
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
  "Return a contiguous suffix of durable history and the next exclusive cursor.

Call under the presentation and conversation append locks. Charge each record
as its own frame, an upper bound for the grouped frames we emit."
  (let ((conversation (application-conversation application))
        (ui           (application-presentation application))
        (limit        (application--history-page-size)))
    (multiple-value-bind (records candidate-count outcomes sources)
        (application--history-page-records application before-sequence limit)
      (let* ((presentations
               (application--associate-tool-outcomes
                (loop for record in records
                      for presentation = (application--record-presentation
                                          application record :outcomes outcomes
                                          :source-cache (gethash record sources))
                      when presentation collect presentation)))
             (newest-first (reverse (getf (channel-presentation--records-fields ui presentations) :presentations)))
             (selected nil)
             (floor before-sequence)
             (more-p (> candidate-count limit)))
        (labels ((event (records)
                   "Wrap wire RECORDS in one transcript event."
                   (list :ui :operation ':records :presentations records))

                 (partition (records)
                   "Partition RECORDS that individually fit into legal packets."
                   (when records
                     (let ((event (event records)))
                       (if (application-client-history--frame-size event)
                           (list event)
                           (let ((middle (floor (length records) 2)))
                             (nconc (partition (subseq records 0 middle))
                                    (partition (nthcdr middle records)))))))))
          (loop for record in (reverse records)
                for sequence = (application--record-sequence record)
                do (when (and newest-first (= sequence (third (first (first newest-first)))))
                     (let* ((wire (pop newest-first))
                            (size (application-client-history--frame-size (event (list wire)))))
                       (when (and (null selected) (or (null size) (> size character-limit)))
                         (presentation--validate-transport-shape wire)
                         (setf wire (presentation--overflow-record wire)
                               size (application-client-history--frame-size (event (list wire)))))
                       (unless (and size (<= size character-limit))
                         (setf more-p t)
                         (return))
                       (decf character-limit size)
                       (push wire selected)))
                   (setf floor sequence))
          (values (partition selected)
                  (list :conversation (conversation-identifier conversation)
                        :before-sequence floor :more-p more-p)))))))

(-> application-client-history--durable-identifier-p (t string) boolean)
(defun application-client-history--durable-identifier-p (identifier conversation)
  "Return whether IDENTIFIER names one durable record of CONVERSATION."
  (and (consp identifier) (eq (first identifier) ':conversation)
       (equal (second identifier) conversation)
       (typep (third identifier) '(integer 1))
       (null (rest (rest (rest identifier))))))

(-> application-client-history--transient-events (channel-presentation string) list)
(defun application-client-history--transient-events (presentation conversation)
  "Return retained local output and unfinished streams not replaced by durable history."
  (loop for cell in (deque->list (channel-presentation-transcript-log presentation))
        for event = (first cell)
        for fields = (rest event)
        for operation = (getf fields :operation)
        unless (or (fifth cell)
                   (and (member operation '(:append :mark))
                        (application-client-history--durable-identifier-p
                         (getf fields :identifier) conversation)))
          append
          (if (eq operation ':records)
              (let ((records
                      (remove-if
                       (lambda (record)
                         (application-client-history--durable-identifier-p (first record) conversation))
                       (getf fields :presentations))))
                (when records
                  (list (list :ui :operation ':records :presentations records))))
              (list event))))

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
      (let* ((conversation (conversation-identifier (application-conversation application)))
             (local (application-client-history--transient-events presentation conversation))
             (state (rest (channel-presentation-replay presentation :transcript nil)))
             (active-stream (fullscreen-terminal-ui-markdown-stream presentation))
             (active-id (when active-stream
                          (channel-presentation--stream-wire-identifier presentation active-stream)))
             (seed (and active-id (find-if (lambda (event)
                                            (and (eq (getf (rest event) :operation) ':markdown)
                                                 (equal (getf (rest event) :identifier) active-id))) local)))
             (available (- character-limit 1024))
             (kept nil))
        ;; State and an active Markdown base are required for applying later deltas.
        (dolist (event (if seed (cons seed state) state))
          (let ((size (application-client-history--frame-size event)))
            (unless (and size (<= size available))
              (error 'localgroup-error :operation ':client
                     :message "The channel's current presentation exceeds the attachment budget."))
            (decf available size)))
        (loop for event in (reverse local)
              unless (eq event seed)
                do (let ((size (application-client-history--frame-size event)))
                     (unless (and size (<= size available))
                       (return))
                     (decf available size)
                     (push event kept)))
        (multiple-value-bind (events fields)
            (application-client-history-page application (application-client-history--cut application)
                                             :character-limit (min available *application-client-history-character-limit*))
          (append (list (list :ui :operation ':reset)) events
                  (if seed (remove-if-not (lambda (event) (or (eq event seed) (member event kept :test #'eq))) local) kept)
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
              (error 'localgroup-error :operation ':transcript-history
                     :message "The transcript history cursor does not name this channel's conversation."))
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
                      (error 'localgroup-error :operation ':transcript-history
                             :message "The attachment budget is too small for the next transcript record."))
                    (list* :history-page-p t :builtin-p t :events events fields))))))))))
