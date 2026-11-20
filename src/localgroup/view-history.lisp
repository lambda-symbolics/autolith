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
  (setf (fullscreen-terminal-ui-history-function (channel-view-ui view))
        (lambda (navigation) (channel-view--navigate-history view navigation))
        (fullscreen-terminal-ui-history-more-p (channel-view-ui view)) (not (null more-p)))
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
    (let* ((ui   (channel-view-ui view))
           (page (terminal-ui-create :terminal (terminal-ui-terminal ui) :fullscreen-p t
                                     :message-header-prefixes (terminal-ui-message-header-prefixes ui))))
      (terminal-ui-resize page (clinedi:transcript-viewport-width (fullscreen-terminal-ui-viewport ui))
                          :rows (terminal-rows (terminal-ui-terminal ui)))
      (with-terminal-ui-locked (ui)
        (dolist (event events)
          (let ((records (remove-if
                          (lambda (record) (terminal-ui--finalized-p ui (first record)))
                          (remove nil (mapcar #'presentation-wire->record
                                              (getf (rest event) :presentations))))))
            (terminal-ui-present-records page records)))
        (when (plusp (clinedi:transcript-viewport-chunk-count (fullscreen-terminal-ui-viewport page)))
          (terminal-ui-fullscreen-prepend-history ui page))))
    (channel-view-apply view ':transcript-history
                        :conversation    (getf reply :conversation)
                        :before-sequence (getf reply :before-sequence)
                        :more-p          (getf reply :more-p)
                        :page-p          t))
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
          (if (eq (getf reply :builtin-p) t)
              (progn
                (channel-view--drain-events view)
                (channel-view--apply-history-page view reply)
                t)
              (progn
                (setf (fullscreen-terminal-ui-history-function (channel-view-ui view)) nil)
                nil))))
    (autolith-error (condition)
      (terminal-ui-append-finalized
       (channel-view-ui view) (list ':view-history-failure (make-identifier))
       (application-failure-line (bounded-string (princ-to-string condition) :limit 500)))
      t)))

(-> channel-view--navigate-history (channel-view list) null)
(defun channel-view--navigate-history (view navigation)
  "Load earlier disk pages and retry NAVIGATION outside the presentation lock."
  (let ((ui (channel-view-ui view)))
    (loop
      for before = (channel-view-transcript-history-floor view)
      do (channel-view--load-transcript-history view)
         (with-terminal-ui-locked (ui)
           (setf (terminal-ui-pending-action ui) nil)
           (ecase (first navigation)
             (:scroll
              (terminal-ui-fullscreen-scroll ui (second navigation)))
             (:jump
              (terminal-ui-fullscreen-jump-message ui (second navigation)))))
      while (and (< (channel-view-transcript-history-floor view) before)
                 (with-terminal-ui-locked (ui)
                   (eq (first (terminal-ui-pending-action ui)) ':transcript-history))))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-pending-action ui) nil)))
  nil)

(defmethod input-port-apply :around ((view channel-view) action &optional payload)
  "Probe the advertised history endpoint for this exact canonical form."
  (let ((text (typecase payload
                (string
                 payload)
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
