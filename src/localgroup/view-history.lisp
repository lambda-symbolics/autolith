(in-package #:autolith)

;;;; -- Per-View Transcript History --

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :transcript-history))
     &key conversation before-sequence more-p page-p &allow-other-keys)
  "Install this view's cursor for the server's scoped history endpoint."
  (unless (and (non-empty-string-p conversation) (typep before-sequence '(integer 1)))
    (error 'channel-subscription-unavailable :message "The channel sent an invalid transcript history cursor."))
  (setf (channel-view-transcript-history-conversation view) conversation
        (channel-view-transcript-history-floor view) before-sequence)
  (when (and page-p (not more-p))
    (terminal-ui-set-notice (channel-view-ui view) "no earlier transcript history remains"
                            :duration-seconds 4))
  nil)


(-> channel-view--apply-history-page (channel-view list) null)
(defun channel-view--apply-history-page (view reply)
  "Apply the correlated durable page on the UI owner, then advance its cursor."
  (let ((events (getf reply :events)))
    (unless (and (equal (getf reply :conversation) (channel-view-transcript-history-conversation view))
                 (typep (getf reply :before-sequence) '(integer 1))
                 (listp events)
                 (every (lambda (event)
                          (and (consp event) (eq (first event) ':ui)
                               (eq (getf (rest event) :operation) ':records))) events))
      (error 'channel-subscription-unavailable :message "The channel returned an invalid transcript history page."))
    (dolist (event events)
      (channel-subscription-present-event view event))
    (channel-view-apply view ':transcript-history :conversation (getf reply :conversation)
                        :before-sequence (getf reply :before-sequence)
                        :more-p (getf reply :more-p) :page-p t))
  nil)
(-> channel-view--load-transcript-history (channel-view) boolean)
(defun channel-view--load-transcript-history (view)
  "Load an earlier page into VIEW alone, returning NIL only for an overridden pager.

An advertised pager's request failure is shown locally and never falls back to
shared history rendering. A declined built-in identity follows normal input."
  (handler-case
      (progn
        (channel-view--drain-events view)
        (let* ((conversation (channel-view-transcript-history-conversation view))
               (floor        (channel-view-transcript-history-floor view))
               (evicted      (terminal-ui-take-evicted-sequence (channel-view-ui view) conversation))
               (before       (max floor (if evicted (1+ evicted) floor)))
               (reply        (channel-subscription-call
                              view ':transcript-history :conversation conversation :before-sequence before)))
          (unless (and (eq (getf reply :history-page-p) t)
                       (member (getf reply :builtin-p ':missing) '(t nil)))
            (error 'channel-subscription-unavailable :message "The channel did not return a transcript history page."))
          (when (eq (getf reply :builtin-p) t)
            (channel-view--drain-events view)
            (channel-view--apply-history-page view reply)
            t)))
    (autolith-error (condition)
      (terminal-ui-append-finalized
       (channel-view-ui view) (list ':view-history-failure (make-identifier))
       (application-failure-line (bounded-string (princ-to-string condition) :limit 500)))
      t)))

(defmethod input-port-apply :around ((view channel-view) action &optional payload)
  "Probe the advertised history endpoint for this exact canonical form."
  (let ((text (typecase payload
                (string payload)
                (user-message-input
                 (unless (user-message-input-image-pathnames payload)
                   (user-message-input-text payload))))))
    (if (and (member action '(:submit :command))
             (channel-view-transcript-history-conversation view)
             (stringp text)
             (string= text "(history)")
             (channel-view--load-transcript-history view))
        t
        (call-next-method))))
