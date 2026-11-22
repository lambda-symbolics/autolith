(in-package #:autolith)

;;;; -- Channel View --

;;; The terminal client's side of one channel. A view subscribes to the
;;; channel as a structured client that presents: it applies the channel's
;;; (:ui ...) events to a terminal UI of its own, which lays the transcript
;;; out and paints it into the client's screen beside the side panel. The
;;; view reads the keys, edits the composer where the terminal is, and sends
;;; the channel only what the user asks of it.

(defparameter *channel-view-poll-seconds* 0.01
  "How long a view sleeps between looks at its terminal when no input waits.")

(defparameter *channel-view-geometry-seconds* 0.15
  "How long a view's pane keeps one size before the channel is told of it.")

(defparameter *channel-view-read-only-notice*
  "read-only: this terminal only watches the channel"
  "What a view that only watches tells its user when they submit input.")


;;;; -- Pane Terminals --

(defclass pane-terminal (terminal)
  ((screen
    :initarg       :screen
    :reader        pane-terminal-screen
    :type          world-terminal-screen
    :documentation "The client screen whose channel pane this terminal is.")
   (request-function
    :initarg       :request-function
    :initform      nil
    :reader        pane-terminal-request-function
    :type          (option function)
    :documentation "The function given a side panel's request that a modal reader came upon, or NIL.")
   (service-function
    :initarg :service-function :initform nil :reader pane-terminal-service-function
    :type          (option function)
    :documentation "Drain the view's pending presentation before polling modal or boot input."))
  (:documentation "The pane of a client's screen that one channel's UI paints.

Its frames go to the screen, which composes them beside the side panel and
paints the client's own terminal. Its input is that terminal's, less what the
side panel takes."))

(-> pane-terminal-create (world-terminal-screen &key (:request-function (option function))
                                               (:service-function (option function))) pane-terminal)
(defun pane-terminal-create (screen &key request-function service-function)
  "Return the terminal of SCREEN's channel pane at its current size."
  (multiple-value-bind (rows columns) (world-terminal-screen-pane-size screen)
    (make-instance 'pane-terminal
                   :screen           screen
                   :request-function request-function
                   :service-function service-function
                   :rows             rows
                   :columns          columns
                   :interactive-p    t
                   :styled-p         (terminal-styled-p (world-terminal-screen-terminal screen)))))

(defmethod terminal-start ((terminal pane-terminal))
  "Count TERMINAL as started; the screen's own terminal is its client's to start."
  (setf (terminal-started-p terminal) t)
  terminal)

(defmethod terminal-stop ((terminal pane-terminal))
  "Count TERMINAL as stopped; the screen's own terminal is its client's to restore."
  (setf (terminal-started-p terminal) nil)
  terminal)

(defmethod terminal--write ((terminal pane-terminal) text)
  "Write control TEXT, such as a title or a clipboard sequence, to the screen's terminal between frames."
  (let ((screen (pane-terminal-screen terminal)))
    (with-lock-held ((world-terminal-screen-lock screen))
      (terminal--write (world-terminal-screen-terminal screen) text)))
  nil)

(defmethod terminal-flush ((terminal pane-terminal))
  "Flush the screen's terminal."
  (terminal-flush (world-terminal-screen-terminal (pane-terminal-screen terminal))))

(defmethod terminal-plain-output-p ((terminal pane-terminal))
  "Return NIL: a pane shows frames, never plain output."
  nil)

(defmethod terminal-input-ready-p ((terminal pane-terminal))
  "Return whether the screen's terminal has input."
  (when-let ((service (pane-terminal-service-function terminal))) (funcall service))
  (terminal-input-ready-p (world-terminal-screen-terminal (pane-terminal-screen terminal))))

(defmethod terminal-read-event ((terminal pane-terminal))
  "Read one event for a modal reader of the pane, less what the side panel takes.

An event the screen consumed reads as :IGNORE. A panel's request to leave the
channel goes to the request function and reads as :ESCAPE, which closes the
modal reader so the view can act on it."
  (when-let ((service (pane-terminal-service-function terminal))) (funcall service))
  (let* ((screen (pane-terminal-screen terminal))
         (event  (world-terminal-screen-filter-event
                  screen (terminal-read-event (world-terminal-screen-terminal screen)))))
    (cond
      ((and (consp event) (eq (first event) ':switch))
       (when-let ((function (pane-terminal-request-function terminal)))
         (funcall function event))
       ':escape)
      (t
       event))))

(defmethod terminal-screen-enter ((terminal pane-terminal))
  "Take nothing: the screen takes its terminal with the first frame."
  nil)

(defmethod terminal-screen-leave ((terminal pane-terminal) token)
  "Keep the screen: it stays its client's across channels."
  (declare (ignore token))
  nil)

(defmethod terminal-screen-paint
    ((terminal pane-terminal) painter rows
     &key (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p)
  "Give the screen ROWS as the pane's complete frame; it paints what changed."
  (declare (ignore painter))
  (world-terminal-screen-show-frame
   (pane-terminal-screen terminal)
   (clinedi:make-frame-snapshot rows
                                :height (max 1 (terminal-rows terminal))
                                :width  (max 1 (terminal-columns terminal)))
   :cursor-row       cursor-row
   :cursor-column    cursor-column
   :cursor-visible-p cursor-visible-p
   :whole-screen-p   whole-screen-p)
  nil)

(defmethod terminal-screen-invalidate ((terminal pane-terminal) painter)
  "Forget nothing: every frame the pane gives the screen is complete."
  (declare (ignore painter))
  nil)


;;;; -- Views --

(defparameter *channel-view-event-count-limit* 256
  "Maximum presentation events waiting for the view's UI loop.")

(defparameter *channel-view-event-byte-limit* (* 4 1024 1024)
  "Maximum conservative storage-byte weight of waiting presentation events.")

(defvar *channel-view-delivering* nil
  "The view this thread is currently draining, preventing nested modal polling from redelivering.")

(defvar *channel-view-replaying* nil
  "The view consuming its initial replay before its transport reader starts.")

(defclass channel-view (channel-subscription)
  ((screen
    :initarg       :screen
    :reader        channel-view-screen
    :type          world-terminal-screen
    :documentation "The client's screen the view paints its pane of.")
   (ui
    :accessor      channel-view-ui
    :type          terminal-ui
    :documentation "The terminal UI that renders the channel in the pane.")
   (completion-chunks
    :initform nil :accessor channel-view-completion-chunks :type list
    :documentation "Pending bounded catalog chunks, newest first, until the final chunk.")
   (completion-receiving-p
    :initform nil :accessor channel-view-completion-receiving-p :type boolean
    :documentation "Whether a replacement completion catalog is being received.")
   (state-lock
    :initform      (make-lock "Autolith channel view")
    :reader        channel-view-state-lock
    :documentation "The lock guarding the turn state, the exit and the tasks.")
   (turn-active-p
    :initform      nil
    :accessor      channel-view-turn-active-p
    :type          boolean
    :documentation "Whether the channel says it runs a work item.")
   (cancelling-p
    :initform      nil
    :accessor      channel-view-cancelling-p
    :type          boolean
    :documentation "Whether the channel says it is cancelling its turn.")
   (follow-up-editing-p
    :initform      nil
    :accessor      channel-view-follow-up-editing-p
    :type          boolean
    :documentation "Whether the channel says a queued follow-up sits in the composer.")
   (modal-p
    :initform      nil
    :accessor      channel-view-modal-p
    :type          boolean
    :documentation "Whether a picker or another modal reader of the channel takes this terminal's events.")
   (page
    :initform      nil
    :accessor      channel-view-page
    :type          list
    :documentation "The channel's page that takes the pane now, as its :PAGE fields, or NIL.")
   (stashed-draft
    :initform      nil
    :accessor      channel-view-stashed-draft
    :type          (or null string user-message-input)
    :documentation "The user's own draft, set aside while a modal reader shows its line in the composer.")
   (exit
    :initform      nil
    :accessor      channel-view-exit
    :type          list
    :documentation "Why the view ends, or NIL while it runs: (:DETACHED), (:EXIT . FIELDS), (:SWITCH :SESSION-ID ID) or (:ATTACHMENT-LOST . FIELDS).")
   (notes
    :initform      nil
    :accessor      channel-view-notes
    :type          list
    :documentation "The :NOTICE and :CLIENT-VERSION to show below the transcript once the channel replayed it, or NIL once shown.")
   (read-only-p
    :initarg       :read-only-p
    :initform      nil
    :reader        channel-view-read-only-p
    :type          boolean
    :documentation "Whether the view only watches: it scrolls and leaves, and sends the channel no input.")
   (events
    :initform (make-deque) :reader channel-view-events
    :documentation "Transport events waiting for the UI owner, in arrival order.")
   (event-bytes
    :initform 0 :accessor channel-view-event-bytes :type (integer 0)
    :documentation "Conservative retained storage weight of waiting events.")
   (queue-lock
    :initform (make-lock "Autolith view events") :reader channel-view-queue-lock
    :documentation "Guard only event admission and removal, never presentation or transport.")
   (queue-closed-p
    :initform nil :accessor channel-view-queue-closed-p :type boolean
    :documentation "Whether ordered event delivery ended and late events must be discarded.")
   (transcript-history-conversation
    :initform nil :accessor channel-view-transcript-history-conversation
    :type          (option string)
    :documentation "The durable conversation this view's transcript page belongs to.")
   (transcript-history-floor
    :initform 1 :accessor channel-view-transcript-history-floor :type (integer 1)
    :documentation "The first durable sequence in this view's selected history page.")
   (pane-size
    :initform      nil
    :accessor      channel-view-pane-size
    :type          list
    :documentation "The (ROWS COLUMNS) the channel was last told the pane has.")
   (pane-changed-at
    :initform      nil
    :accessor      channel-view-pane-changed-at
    :type          (option real)
    :documentation "When the pane last changed size, until the channel is told."))
  (:default-initargs :name "terminal")
  (:documentation "One channel as a terminal client shows it and drives it."))

(defmethod channel-subscription-arguments ((view channel-view))
  "Say that this client presents, whether it only watches, and how large and how styled its pane is."
  (multiple-value-bind (rows columns) (world-terminal-screen-pane-size (channel-view-screen view))
    (list :presents-p t
          :watches-p  (channel-view-read-only-p view)
          :rows       rows
          :columns    columns
          :styled-p   (terminal-styled-p (world-terminal-screen-terminal (channel-view-screen view)))
          :client-pid (current-process-id))))

(-> channel-view--finish (channel-view list) null)
(defun channel-view--finish (view exit)
  "End VIEW with EXIT unless it already ends for another reason."
  (with-lock-held ((channel-view-state-lock view))
    (unless (channel-view-exit view)
      (setf (channel-view-exit view) exit)))
  nil)

(-> channel-view-create
    (configuration string world-terminal-screen &key (:read-only-p boolean)) channel-view)
(defun channel-view-create (configuration identifier screen &key read-only-p)
  "Return a view of channel IDENTIFIER that paints SCREEN's pane, not yet attached.

With READ-ONLY-P it watches the channel and sends it no input."
  (let* ((view     (make-instance 'channel-view
                                  :identifier    identifier
                                  :configuration configuration
                                  :screen        screen
                                  :read-only-p   read-only-p
                                  :approves-p    (not read-only-p)
                                  :evaluates-p   (not read-only-p)))
         (terminal (pane-terminal-create
                     screen :request-function (lambda (request)
                                                (channel-view--finish view request))
                            :service-function (lambda () (channel-view--drain-events view))))
         (ui       (terminal-ui-create
                    :terminal                terminal
                    :fullscreen-p            t
                    :prompt                  *application-prompt*
                    :prompt-padding          (config :prompt-padding configuration)
                    :message-header-prefixes *application-message-header-prefixes*
                    :placeholder             *application-placeholder*
                    :completion-root         (config :working-directory configuration)
                    :mention-function        (application-world-mention-function configuration))))
    (terminal-ui-connect-transcript-actions ui)
    (setf (channel-view-ui view) ui)
    view))

(defmethod channel-subscription-initialize-connection ((view channel-view) connection)
  "Stream an advertised initial presentation on the attaching UI owner before starting the reader.

The prefix is unsequenced and ends at :REPLAYED. Consuming one packet at a
time avoids retaining a second transcript in the bounded live event queue."
  (when (eq (getf (rest (world-terminal-client-connection-attached connection)) :presentation-replay-p) t)
    (with-lock-held ((channel-subscription-lock view))
      (when (channel-subscription-closed-p view)
        (error 'channel-subscription-unavailable :message "The view closed during attachment."))
      (setf (channel-subscription-connection view) connection))
    (let ((*channel-view-replaying* view))
      (loop for packet = (daemon-read-decodable-packet (world-terminal-client-connection-stream connection))
            do (unless (and (world-terminal-message-p packet) (eq (first packet) ':ui)
                            (null (getf (rest packet) :sequence)))
                 (error 'channel-subscription-unavailable
                        :message "The channel's initial presentation ended before its replay boundary."))
               (when (channel-subscription-closed-p view)
                 (error 'channel-subscription-unavailable :message "The view closed during replay."))
               (channel-subscription-present-event view packet)
            until (eq (getf (rest packet) :operation) ':replayed))))
  nil)

(defmethod channel-subscription-call :around ((view channel-view) command &rest arguments)
  "Do not await a reply while this thread is still reading the initial presentation."
  (declare (ignore command arguments))
  (when (eq *channel-view-replaying* view)
    (error 'channel-subscription-unavailable :message "Channel requests await the initial presentation."))
  (call-next-method))

(-> channel-view--event-bytes (list) (integer 0))
(defun channel-view--event-bytes (event)
  "Charge conservative storage bytes for plain packet data without serializing it.

Stop at the queue budget. Shared or circular objects are charged once; unsupported
reader objects exceed the budget rather than escaping its bound."
  (let ((pending (list event))
        (seen    (make-hash-table :test #'eq))
        (bytes   0))
    (loop while pending
          do (let ((object (pop pending)))
               (unless (gethash object seen)
                 (setf (gethash object seen) t)
                 (incf bytes
                       (typecase object
                         (string
                          (+ 32 (* 4 (length object))))
                         (cons
                          32)
                         (integer
                          (+ 32 (ceiling (integer-length object) 8)))
                         (vector
                          (+ 32 (* 8 (length object))))
                         ((or symbol character float ratio complex)
                          32)
                         (t
                          (1+ *channel-view-event-byte-limit*))))
                 (when (> bytes *channel-view-event-byte-limit*)
                   (return))
                 (typecase object
                   (cons
                    (push (first object) pending)
                    (push (rest object) pending))
                   (string
                    nil)
                   (ratio
                    (push (numerator object) pending)
                    (push (denominator object) pending))
                   (complex
                    (push (realpart object) pending)
                    (push (imagpart object) pending))
                   (vector
                    (map nil (lambda (value) (push value pending)) object))))))
    bytes))

(-> channel-view--close-events (channel-view) null)
(defun channel-view--close-events (view)
  "Close event admission and discard pending deltas once the view leaves or loses ordering."
  (with-lock-held ((channel-view-queue-lock view))
    (setf (channel-view-queue-closed-p view) t
          (channel-view-event-bytes view) 0)
    (deque-clear (channel-view-events view)))
  nil)

(defmethod channel-subscription-receive-event ((view channel-view) event)
  "Queue ordered events for the UI loop, leaving the transport reader free to settle history replies."
  (let ((bytes      (channel-view--event-bytes event))
        (overflow-p nil))
    (with-lock-held ((channel-view-queue-lock view))
      (unless (channel-view-queue-closed-p view)
        (if (or (>= (deque-count (channel-view-events view)) *channel-view-event-count-limit*)
                (> (+ bytes (channel-view-event-bytes view)) *channel-view-event-byte-limit*))
            (setf overflow-p t
                  (channel-view-queue-closed-p view) t)
            (progn
              (deque-push-back (channel-view-events view) (cons event bytes))
              (incf (channel-view-event-bytes view) bytes)))))
    (when overflow-p
      (channel-view--close-events view)
      (channel-view--finish view (list ':attachment-lost :kind ':queue-overflow
                                      :message                 "The view fell behind its channel; reconnect to resynchronize."))
      (channel-subscription--mark-closed view)
      (when-let ((connection (channel-subscription-connection view))) (world-terminal-client--disconnect connection))))
  nil)

(-> channel-view--drain-events (channel-view) null)
(defun channel-view--drain-events (view)
  "Apply a bounded batch on the UI owner, including during boot and modal polling."
  (unless (eq *channel-view-delivering* view)
    (let ((*channel-view-delivering* view))
      (loop repeat *channel-view-event-count-limit*
            do (let ((entry (with-lock-held ((channel-view-queue-lock view))
                              (unless (deque-empty-p (channel-view-events view))
                                (let ((next (deque-pop-front (channel-view-events view))))
                                  (decf (channel-view-event-bytes view) (rest next))
                                  next)))))
                 (unless entry (return))
                 (channel-subscription-event view (first entry))))))
  nil)

(defmethod channel-subscription-forget ((view channel-view) &key wire-p)
  "End the view when its channel went away, unless it already ends for a reason of its own."
  (declare (ignore wire-p))
  (channel-view--finish
   view (list ':attachment-lost
              :kind    ':transport-failure
              :message (or (channel-subscription-reconnect-failure view)
                           "The channel's connection closed.")))
  nil)


;;;; -- Applying the Channel's Events --

(defgeneric channel-view-apply (view operation &key &allow-other-keys)
  (:documentation "Apply the channel's presentation OPERATION to VIEW.

Operations that concern the view itself are handled here; every other one
goes to its terminal UI."))

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :completions))
     &key entries reset-p complete-p &allow-other-keys)
  "Install a complete server catalog atomically, then invalidate draft matching."
  (unless (and (listp entries) (<= (length entries) 32)
               (every (lambda (entry)
                        (and (terminal-completion-p entry)
                             (equal entry (presentation--completion-entry entry)))) entries))
    (error 'terminal-error :message "Invalid completion catalog chunk."
                           :operation ':complete :cause nil))
  (when reset-p
    (setf (channel-view-completion-chunks view) nil
          (channel-view-completion-receiving-p view) t))
  (when (channel-view-completion-receiving-p view)
    (push entries (channel-view-completion-chunks view))
    (when complete-p
      (let ((ui (channel-view-ui view)))
        (with-terminal-ui-locked (ui)
          (setf (terminal-ui-completions ui)
                (loop for chunk in (reverse (channel-view-completion-chunks view))
                      append chunk))
          (terminal-ui--clear-completions ui)))
      (setf (channel-view-completion-chunks view) nil
            (channel-view-completion-receiving-p view) nil)))
  nil)

(defmethod channel-view-apply ((view channel-view) operation &rest fields &key &allow-other-keys)
  "Apply an operation of the channel's presentation to the view's terminal UI."
  (apply #'presentation-apply (channel-view-ui view) operation fields)
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :turn))
     &key active-p cancelling-p editing-p &allow-other-keys)
  "Note whether the channel works, cancels, and holds a recalled follow-up."
  (with-lock-held ((channel-view-state-lock view))
    (setf (channel-view-turn-active-p view)       (not (null active-p))
          (channel-view-cancelling-p view)        (not (null cancelling-p))
          (channel-view-follow-up-editing-p view) (not (null editing-p))))
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :modal)) &key open-p &allow-other-keys)
  "Note whether the channel's modal reader takes this terminal's events.

The user's draft is set aside while it does and comes back afterwards."
  (let ((ui      (channel-view-ui view))
        (opens-p (not (null open-p))))
    (with-terminal-ui-locked (ui)
      (unless (eq opens-p (channel-view-modal-p view))
        (if opens-p
            (setf (channel-view-stashed-draft view)
                  (terminal-ui--submission-input ui (line-editor-text (terminal-ui-editor ui))))
            (terminal-ui-set-input ui (or (shiftf (channel-view-stashed-draft view) nil) ""))))
      (with-lock-held ((channel-view-state-lock view))
        (setf (channel-view-modal-p view) opens-p))))
  nil)

(-> channel-view--paint-page (channel-view) null)
(defun channel-view--paint-page (view)
  "Paint the channel's page over the pane, or give the pane back to the transcript.

The client's own boot screen comes first; the page shows once it ends."
  (let ((ui   (channel-view-ui view))
        (page (channel-view-page view)))
    (with-terminal-ui-locked (ui)
      (unless (terminal-ui-boot-waiting-p ui)
        (cond
          (page
           (setf (terminal-ui-live-output-suspended-p ui)        t
                 (fullscreen-terminal-ui-cursor-visible-p ui) (not (null (getf page :cursor-visible-p))))
           (terminal-ui-fullscreen-paint
            ui
            :rows           (getf page :rows)
            :cursor-row     (getf page :cursor-row)
            :cursor-column  (getf page :cursor-column)
            :whole-screen-p (not (null (getf page :whole-screen-p)))))
          ((terminal-ui-live-output-suspended-p ui)
           (setf (terminal-ui-live-output-suspended-p ui)        nil
                 (fullscreen-terminal-ui-cursor-visible-p ui) t)
           (terminal-ui-fullscreen-invalidate ui)
           (terminal-ui--paint-live ui))))))
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :page))
     &key open-p rows (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p
     &allow-other-keys)
  "Show the page the channel laid out for the whole pane, or return to the transcript."
  (setf (channel-view-page view)
        (and open-p (listp rows) (every #'stringp rows)
             (typep cursor-row '(integer 0)) (typep cursor-column '(integer 0))
             (list :rows             rows
                   :cursor-row       cursor-row
                   :cursor-column    cursor-column
                   :cursor-visible-p cursor-visible-p
                   :whole-screen-p   whole-screen-p)))
  (channel-view--paint-page view)
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :switch)) &key session-id &allow-other-keys)
  "Leave for the channel the user's command asked for."
  (when (non-empty-string-p session-id)
    (channel-view--finish view (list ':switch :session-id session-id)))
  nil)

(defmethod channel-view-apply ((view channel-view) (operation (eql :detach)) &key &allow-other-keys)
  "Leave the channel running and give the terminal back."
  (channel-view--finish view (list ':detached))
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :exit)) &key status message details &allow-other-keys)
  "End with the channel, which exited with STATUS and what its launcher needs to know."
  (channel-view--finish
   view (list ':exit
              :status  (if (integerp status) status 0)
              :message (and (stringp message) message)
              :details (and (listp details) details)))
  nil)

(defmethod channel-view-apply ((view channel-view) (operation (eql :replayed)) &key &allow-other-keys)
  "Show this terminal's own notes now that the channel's transcript stands above them."
  (channel-view--present-notes view)
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :title)) &key title &allow-other-keys)
  "Name the terminal's window after the channel."
  (when (stringp title)
    (let ((ui (channel-view-ui view)))
      (with-terminal-ui-locked (ui)
        (terminal-write-window-title (terminal-ui-terminal ui) title))))
  nil)

(defmethod channel-view-apply
    ((view channel-view) (operation (eql :history)) &key entries (offset 0) paged-p &allow-other-keys)
  "Load the newest input history and page through the rest by asking the channel."
  (when (and (listp entries) (every #'stringp entries) (typep offset '(integer 0)))
    (flet ((page (start limit)
             (handler-case
                 (let ((entries (getf (channel-subscription-call
                                       view ':input-history :start start :limit limit)
                                      :entries)))
                   (and (listp entries) (remove-if-not #'stringp entries)))
               (autolith-error ()
                 nil))))
      (terminal-ui-load-history
       (channel-view-ui view) entries
       :offset         offset
       :older-function (and paged-p
                            (lambda (end)
                              (let ((count (min end *terminal-history-page-size*)))
                                (page (- end count) count))))
       :newer-function (and paged-p
                            (lambda (start)
                              (page start *terminal-history-page-size*))))))
  nil)

(defmethod channel-subscription-present-event ((view channel-view) event)
  "Apply one presentation event; a malformed one is dropped and the view goes on.

A command the channel refused is shown as a failure line."
  (case (first event)
    (:ui
     (let ((operation (getf (rest event) :operation)))
       (when (keywordp operation)
         (handler-case (apply #'channel-view-apply view operation (rest event))
           (error ()
             nil)))))
    (:refused
     (let ((message (getf (rest event) :message)))
       (when (stringp message)
         (terminal-ui-append-finalized
          (channel-view-ui view) (list ':view-notice (make-identifier))
          (application-failure-line (bounded-string message :limit 500)))))))
  nil)


;;;; -- The View as an Input Port --

(defmethod input-port-turn-active-p ((view channel-view))
  "Return whether the channel said it runs a work item."
  (with-lock-held ((channel-view-state-lock view))
    (channel-view-turn-active-p view)))

(defmethod input-port-follow-up-editing-p ((view channel-view))
  "Return whether the channel said a queued follow-up sits in the composer."
  (with-lock-held ((channel-view-state-lock view))
    (channel-view-follow-up-editing-p view)))

(defmethod input-port-apply ((view channel-view) action &optional payload)
  "Send input ACTION to the channel, or act on it here when it concerns only this terminal.

Detaching leaves the channel alone. An interrupt or Escape is the channel's
only while it works; the reply says whether it took it. Every other action
is sent without waiting, since the channel may answer it by asking this
terminal a question. A view that only watches sends none and says so when
its user submits."
  (case action
    (:detach
     (channel-view--finish view (list ':detached))
     t)
    ((:end-of-input :exit)
     (if (channel-view-read-only-p view)
         (channel-view--finish view (list ':detached))
         (channel-view--send-input view action payload))
     t)
    ((:interrupt :escape)
     (and (not (channel-view-read-only-p view))
          (with-lock-held ((channel-view-state-lock view))
            (or (channel-view-turn-active-p view) (channel-view-cancelling-p view)))
          (handler-case
              (not (null (getf (channel-subscription-call view ':input :action action)
                               :handled-p)))
            (autolith-error ()
              nil))))
    (t
     (cond
       ((not (channel-view-read-only-p view))
        (channel-view--send-input view action payload))
       ((member action '(:submit :queue :command))
        (terminal-ui-set-notice (channel-view-ui view) *channel-view-read-only-notice*
                                :duration-seconds 4)
        t)))))

(-> channel-view--send-input (channel-view keyword t) boolean)
(defun channel-view--send-input (view action payload)
  "Send input ACTION with the draft PAYLOAD to the channel and return true when it left."
  (handler-case
      (progn
        (apply #'channel-subscription-send view ':input :action action
               (cond
                 ((typep payload 'user-message-input)
                  (list :text   (user-message-input-text payload)
                        :images (mapcar #'namestring
                                        (user-message-input-image-pathnames payload))))
                 ((stringp payload)
                  (list :text payload))))
        t)
    (autolith-error ()
      nil)))


;;;; -- Running a View --

(-> channel-view--follow-size (channel-view) null)
(defun channel-view--follow-size (view)
  "Give the pane's UI the size the screen now leaves it, and tell the channel once it settles."
  (let* ((screen   (channel-view-screen view))
         (terminal (world-terminal-screen-terminal screen))
         (ui       (channel-view-ui view))
         (now      (get-internal-real-time)))
    (when (shiftf *terminal-resize-pending-p* nil)
      (multiple-value-bind (rows columns) (terminal-current-size)
        (terminal-set-dimensions terminal columns :rows rows))
      (world-terminal-screen-resized screen))
    (world-terminal-screen-take-resize screen)
    (multiple-value-bind (rows columns) (world-terminal-screen-pane-size screen)
      (let ((pane (terminal-ui-terminal ui)))
        (unless (and (= rows (terminal-rows pane)) (= columns (terminal-columns pane)))
          (terminal-ui-resize ui columns :rows rows)
          (setf (channel-view-pane-changed-at view) now)))
      (when (and (channel-view-pane-changed-at view)
                 (>= (- now (channel-view-pane-changed-at view))
                     (* *channel-view-geometry-seconds* internal-time-units-per-second))
                 (not (equal (list rows columns) (channel-view-pane-size view))))
        (setf (channel-view-pane-size view)       (list rows columns)
              (channel-view-pane-changed-at view) nil)
        (handler-case (channel-subscription-send view ':geometry :rows rows :columns columns)
          (autolith-error ()
            nil)))))
  nil)

(-> channel-view--step (channel-view) null)
(defun channel-view--step (view)
  "Repaint what time changed, then handle one waiting terminal event or rest a moment."
  (let* ((screen   (channel-view-screen view))
         (terminal (world-terminal-screen-terminal screen))
         (ui       (channel-view-ui view)))
    (channel-view--drain-events view)
    (world-terminal-screen-tick screen)
    (channel-view--follow-size view)
    (terminal-ui-refresh-status ui)
    (terminal-ui-fullscreen--autoscroll-tick ui)
    (if (terminal-input-ready-p terminal)
        (let ((event (world-terminal-screen-filter-event screen (terminal-read-event terminal))))
          (cond
            ((eq event ':ignore)
             nil)
            ((and (consp event) (eq (first event) ':switch))
             (channel-view--finish view event))
            ((eq event ':detach)
             (channel-view--finish view (list ':detached)))
            ((with-lock-held ((channel-view-state-lock view))
               (channel-view-modal-p view))
             ;; The channel's picker reads this terminal's keys.
             (unless (channel-view-read-only-p view)
               (handler-case (channel-subscription-send view ':event :event event)
                 (autolith-error ()
                   nil))))
            (t
             (terminal-ui-dispatch-event ui view event))))
        (sleep *channel-view-poll-seconds*)))
  nil)

(-> channel-view--server-version (channel-view) (option string))
(defun channel-view--server-version (view)
  "Return the version the server stated when it accepted VIEW, or NIL for one too old to present."
  (let ((version (getf (rest (world-terminal-client-connection-attached
                              (channel-subscription-connection view)))
                       :server-version)))
    (and (non-empty-string-p version) version)))

(-> channel-view--present-notes (channel-view) null)
(defun channel-view--present-notes (view)
  "Show VIEW's pending notice, and a note when its client's version is not the server's, once.

They belong below the channel's transcript, so a view shows them when the
channel says it has replayed it. A server too old to present replays nothing
and is named at once."
  (let ((ui             (channel-view-ui view))
        (notes          (with-lock-held ((channel-view-state-lock view))
                          (shiftf (channel-view-notes view) nil)))
        (server-version (channel-view--server-version view)))
    (when notes
      (destructuring-bind (&key notice client-version) notes
        (when notice
          (terminal-ui-append-finalized ui (list ':view-notice (make-identifier))
                                        (application-failure-line notice)))
        (cond
          ((null server-version)
           (terminal-ui-append-finalized
            ui (list ':view-notice (make-identifier))
            (application-failure-line
             "this world server is older than the client and cannot present its channels; restart it with (update) or autolith stop")))
          ((and (non-empty-string-p client-version)
                (not (string= client-version server-version)))
           (terminal-ui-append-finalized
            ui (list ':view-notice (make-identifier))
            (application-notice-line
             (format nil "Client/server version mismatch: client Autolith ~A; world server Autolith ~A."
                     (bounded-string client-version :limit 64)
                     (bounded-string server-version :limit 64)))))))))
  nil)

(-> channel-view--boot (channel-view) null)
(defun channel-view--boot (view)
  "Hold the boot screen the client already shows until the user starts the session.

The transcript the channel replays meanwhile waits behind it. Ctrl-C there
leaves the channel running and gives the terminal back."
  (let* ((ui            (channel-view-ui view))
         (configuration (channel-subscription-configuration view)))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-live-output-suspended-p ui) t))
    (terminal-ui-start ui)
    (unwind-protect
         (when (eq (terminal-ui-boot-sequence
                    ui
                    :presented-p t
                    :linger-p    (config :boot-screen-linger-p configuration))
                   ':interrupt)
           (channel-view--finish view (list ':detached)))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-live-output-suspended-p ui) nil)
        (terminal-ui--paint-live ui))
      ;; A page the channel opened meanwhile, such as the tour, shows now.
      (channel-view--paint-page view)))
  nil)

(defparameter *channel-view-detach-seconds* 0.25
  "Maximum time a departing view spends sending its best-effort goodbye.")

(-> channel-view--leave (channel-view) null)
(defun channel-view--leave (view)
  "Send a bounded best-effort goodbye, then retire the departing view's transport."
  (let ((connection (channel-subscription-connection view)))
    (when (and connection (not (channel-view-read-only-p view)))
      (unwind-protect
           (handler-case
               (sb-sys:with-deadline (:seconds *channel-view-detach-seconds*)
                 (world-terminal-client-send connection ':detach))
             (sb-sys:deadline-timeout () nil)
             (error () nil))
        (world-terminal-client--disconnect connection))))
  nil)

(-> channel-view--save-draft (channel-view) null)
(defun channel-view--save-draft (view)
  "Keep VIEW's actual unsent editor input on its client screen before releasing the UI."
  (let ((ui     (channel-view-ui view))
        (screen (channel-view-screen view)))
    (with-terminal-ui-locked (ui)
      (let* ((modal-p (channel-view-modal-p view))
             (editor  (terminal-ui-editor ui))
             (input   (if modal-p
                          (or (channel-view-stashed-draft view) "")
                          (terminal-ui--submission-input ui (copy-seq (line-editor-text editor)))))
             (draft   (list :input input :cursor (unless modal-p (line-editor-cursor editor)))))
        (with-lock-held ((world-terminal-screen-lock screen))
          (setf (gethash (channel-subscription-identifier view) (world-terminal-screen-channel-drafts screen))
                draft)))))
  nil)

(-> channel-view--restore-draft (channel-view) null)
(defun channel-view--restore-draft (view)
  "Restore this client's draft after VIEW attached, without carrying history between channels."
  (let* ((screen (channel-view-screen view))
         (draft  (with-lock-held ((world-terminal-screen-lock screen))
                   (gethash (channel-subscription-identifier view) (world-terminal-screen-channel-drafts screen))))
         (ui     (channel-view-ui view)))
    (when draft
      (with-terminal-ui-locked (ui)
        (if (channel-view-modal-p view)
            (setf (channel-view-stashed-draft view) (getf draft :input))
            (progn
              (terminal-ui--set-draft-input ui (getf draft :input))
              (let* ((editor (terminal-ui-editor ui))
                     (text   (line-editor-text editor)))
                (line-editor-set-text editor text :cursor (or (getf draft :cursor) (length text))))
              (terminal-ui--paint-live ui))))))
  nil)

(-> channel-view-run
    (channel-view &key (:attached-function (option function)) (:notice (option string))
                  (:client-version (option string)) (:boot-p boolean) (:wait-function (option function)))
    list)
(defun channel-view-run (view &key attached-function notice client-version boot-p wait-function)
  "Attach VIEW to its channel and serve its terminal until it ends, returning why.

The result is what the client's main loop reads: (:SWITCH :SESSION-ID ID) to
move to another channel, (:ATTACHMENT-LOST ...) when the channel went away,
the exited channel's (:STATUS N :MESSAGE TEXT :DETAILS PLIST), or NIL after a
detach, which leaves the channel running. ATTACHED-FUNCTION is called once
the channel accepted the view. WAIT-FUNCTION, when supplied, calls an attachment
thunk while the caller serves its cancellable loading screen.
NOTICE and a CLIENT-VERSION that differs from the server's are shown below
the transcript once the channel has replayed it. With BOOT-P the client's boot screen stays until the user
starts the session."
  (let* ((ui            (channel-view-ui view))
         (terminal      (world-terminal-screen-terminal (channel-view-screen view)))
         (resize-watch  nil)
         (draft-ready-p nil))
    (unless (terminal--terminal-mode-or-nil terminal)
      (error 'world-terminal-error
             :message    "Opening a channel requires an interactive terminal."
             :operation  ':attach
             :session-id (channel-subscription-identifier view)))
    (unwind-protect
         (progn
           (setf (channel-view-notes view) (list :notice notice :client-version client-version))
           (if wait-function
               (funcall wait-function (lambda () (channel-subscription-attach view)))
               (channel-subscription-attach view))
           (unless (channel-view--server-version view)
             (channel-view--present-notes view))
           ;; The terminal reads keys one at a time from here on, and says when its size changes.
           (terminal-start terminal)
           (setf resize-watch
                 (platform-watch-terminal-resize *platform*
                                                 (lambda ()
                                                   (setf *terminal-resize-pending-p* t))))
           (multiple-value-bind (rows columns) (world-terminal-screen-pane-size (channel-view-screen view))
             (setf (channel-view-pane-size view) (list rows columns)))
           (when attached-function
             (funcall attached-function))
           (if boot-p
               (channel-view--boot view)
               (terminal-ui-start ui))
           (channel-view--restore-draft view)
           (setf draft-ready-p t)
           (loop until (with-lock-held ((channel-view-state-lock view))
                         (channel-view-exit view))
                 do (channel-view--step view))
           (let ((exit (channel-view-exit view)))
             (when (member (first exit) '(:detached :switch))
               (channel-view--leave view))
             (case (first exit)
               (:detached
                nil)
               (:exit
                (rest exit))
               (otherwise
                exit))))
      (when draft-ready-p
        (channel-view--save-draft view))
      (channel-view--finish view (list ':detached))
      (channel-view--close-events view)
      (when resize-watch
        ; mallet:suppress no-ignore-errors -- best-effort release of the resize watch
        (ignore-errors (platform-unwatch-terminal-resize *platform* resize-watch)))
      ; mallet:suppress no-ignore-errors -- the connection may be gone already
      (ignore-errors (channel-subscription-close view))
      ; mallet:suppress no-ignore-errors -- best-effort release of the pane's UI
      (ignore-errors (terminal-ui-stop ui)))))
