(in-package #:autolith)

;;;; -- Welcome Composer --

(defclass welcome-input-port ()
  ((request :initform nil :accessor welcome-input-port-request :type list
            :documentation "The channel navigation or first submission requested by the user.")
   (finished-p :initform nil :accessor welcome-input-port-finished-p :type boolean
               :documentation "Whether the user submitted input or left the welcome screen."))
  (:documentation "The normal composer input port before the user opens a channel."))

(defclass welcome-pane-terminal (pane-terminal)
  ((banner-function :initarg :banner-function :reader welcome-pane-terminal-banner-function
                    :type          function
                    :documentation "The function rendering the banner within the available transcript height.")
   (ui :initform nil :accessor welcome-pane-terminal-ui :type (option fullscreen-terminal-ui)
       :documentation "The composer whose empty transcript can contain the startup banner."))
  (:documentation "A client pane with a startup banner behind its normal composer."))

(defmethod input-port-turn-active-p ((port welcome-input-port))
  "Return NIL because no channel has been opened for PORT."
  (declare (ignore port))
  nil)

(defmethod input-port-follow-up-editing-p ((port welcome-input-port))
  "Return NIL because PORT has no queued work."
  (declare (ignore port))
  nil)

(defmethod input-port-apply ((port welcome-input-port) action &optional payload)
  "Retain a first submission for a new channel, or finish PORT without opening one."
  (case action
    ((:submit :queue :command)
     (setf (welcome-input-port-request port)
           (list :switch :new-p t :initial-input (list :action action :payload payload))
           (welcome-input-port-finished-p port) t)
     t)
    ((:end-of-input :detach)
     (setf (welcome-input-port-finished-p port) t)
     t)))

(defmethod terminal-screen-paint :around
    ((terminal welcome-pane-terminal) painter rows
     &key (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p)
  "Paint the banner in an empty transcript without covering the composer or its menus."
  (let* ((ui    (welcome-pane-terminal-ui terminal))
         (space (if ui (fullscreen-terminal-ui-transcript-space ui) 0))
         (rows  (coerce rows 'list)))
    (when (and (plusp space)
               (every (lambda (row)
                        (zerop (length (string-trim '(#\Space #\Tab) (clinedi:ansi-strip row)))))
                      (subseq rows 0 (min space (length rows)))))
      (let ((banner (funcall (welcome-pane-terminal-banner-function terminal) space)))
        (setf rows (append (coerce (clinedi:frame-snapshot-rows banner) 'list)
                           (nthcdr space rows)))))
    (call-next-method terminal painter rows
                      :cursor-row cursor-row :cursor-column cursor-column
                      :cursor-visible-p cursor-visible-p :whole-screen-p whole-screen-p)))

(-> welcome-view-run (world-terminal-screen configuration function &key (:input t)) (option list))
(defun welcome-view-run (screen configuration banner-function &key input)
  "Serve the normal composer beside SCREEN's sidebar until its first submission or navigation.

Return the navigation request with its first input, or NIL when the user leaves."
  (let* ((port     (make-instance 'welcome-input-port))
         (terminal (world-terminal-screen-terminal screen))
         (pane     (change-class (pane-terminal-create screen) 'welcome-pane-terminal
                                 :banner-function banner-function))
         (ui       (terminal-ui-create
                    :terminal pane :fullscreen-p t :prompt *application-prompt*
                    :prompt-padding          (config :prompt-padding configuration)
                    :message-header-prefixes *application-message-header-prefixes*
                    :placeholder             *application-placeholder*
                    :completion-root         (config :working-directory configuration)
                    :mention-function        (application-world-mention-function configuration)))
         (watch    nil))
    (setf (welcome-pane-terminal-ui pane) ui)
    (terminal-ui-connect-transcript-actions ui)
    (terminal-ui-set-idle-status-details ui (configuration-status-details configuration))
    (terminal-ui-set-context-usage ui :used 0 :window (config :context-window configuration)
                                     :compaction-limit (configuration-compaction-token-limit configuration))
    (when input
      (terminal-ui-set-input ui input))
    (unwind-protect
         (progn
           (terminal-start terminal)
           (with-lock-held ((world-terminal-screen-lock screen))
             (world-terminal-screen--take-whole-screen screen nil))
           (setf watch (platform-watch-terminal-resize
                        *platform* (lambda () (setf *terminal-resize-pending-p* t))))
           (terminal-ui-start ui)
           (loop until (welcome-input-port-finished-p port)
                 do (when (shiftf *terminal-resize-pending-p* nil)
                      (multiple-value-bind (height width) (terminal-current-size)
                        (terminal-set-dimensions terminal width :rows height))
                      (world-terminal-screen-resized screen))
                    (world-terminal-screen-take-resize screen)
                    (multiple-value-bind (height width) (world-terminal-screen-pane-size screen)
                      (unless (and (= height (terminal-rows pane)) (= width (terminal-columns pane)))
                        (terminal-ui-resize ui width :rows height)))
                    (world-terminal-screen-tick screen)
                    (terminal-ui-refresh-status ui)
                    (cond
                      ((terminal-input-ready-p terminal)
                       (let ((event (world-terminal-screen-filter-event screen (terminal-read-event terminal))))
                         (cond
                           ((and (consp event) (eq (first event) ':switch))
                            (setf (welcome-input-port-request port) event
                                  (welcome-input-port-finished-p port) t))
                           ((eq event ':resize)
                            (setf *terminal-resize-pending-p* t))
                           ((eq event ':ignore)
                            nil)
                           ((or (equal event '(:channel-command :new))
                                (and (member event '(:submit :enter :return))
                                     (zerop (length (line-editor-text (terminal-ui-editor ui))))
                                     (null (user-message-input-image-pathnames
                                            (terminal-ui--submission-input ui "")))))
                            (setf (welcome-input-port-request port) (list :switch :new-p t)
                                  (welcome-input-port-finished-p port) t))
                           (t
                            (terminal-ui-dispatch-event ui port event)))))
                      (t
                       (sleep *channel-view-poll-seconds*))))
           (welcome-input-port-request port))
      (when watch
        (platform-unwatch-terminal-resize *platform* watch))
      (terminal-ui-stop ui))))
