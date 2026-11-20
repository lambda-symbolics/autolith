(in-package #:autolith)

;;;; -- Client Screen --

;;; A terminal client renders the channel it shows into a pane and paints
;;; the pane's frames on its own terminal. The client therefore owns the
;;; alternate buffer, the frame painter and the cursor, and composes its own
;;; side panel beside the pane's rows.

(defparameter *world-terminal-screen-toggle-event* '(:control #\s)
  "The key event showing or hiding the side panel.")

(defparameter *world-terminal-screen-focus-event* ':focus-next
  "The key event, Ctrl-Space or Ctrl-Tab, that moves the focus between the side panel and the session.")

(define-condition channel-loading-progress (condition)
  ((step :initarg :step :reader channel-loading-progress-step :type (integer 1 6)
         :documentation "The loading milestone currently in progress.")
   (description :initarg :description :reader channel-loading-progress-description :type string
                :documentation "The operation currently in progress."))
  (:documentation "An optional notification of actual channel-opening work."))

(-> channel-loading-report ((integer 1 6) string) null)
(defun channel-loading-report (step description)
  "Notify interested callers of loading STEP and DESCRIPTION."
  (signal 'channel-loading-progress :step step :description description)
  nil)

(-> channel-command-text (t) (option string))
(defun channel-command-text (event)
  "Return the command a (:CHANNEL-COMMAND NAME [ID]) EVENT submits, or NIL for anything else.

NAME is :NEW, :OPEN or :DELETE, and only :DELETE takes the channel ID."
  (when (typep event '(cons (eql :channel-command) (cons keyword list)))
    (destructuring-bind (name &optional identifier &rest more) (rest event)
      (cond
        (more
         nil)
        ((and (member name '(:new :open :delete)) (null identifier))
         (format nil "(~(~A~))" name))
        ((and (eq name ':delete) (typep identifier '(and string (satisfies identifier-p))))
         (format nil "(delete ~S)" identifier))))))


;;;; -- Side Panels --

(defgeneric screen-panel-width (panel columns)
  (:documentation "Return the columns PANEL takes left of the session's rows on a terminal COLUMNS wide."))

(defgeneric screen-panel-minimum-columns (panel)
  (:documentation "Return the minimum session-pane width beside PANEL; below it the panel is an explicit drawer."))

(defgeneric screen-panel-refresh (panel)
  (:documentation "Gather PANEL's current content and return whether it changed since the last call."))

(defgeneric screen-panel-rows (panel terminal &key height width focused-p)
  (:documentation "Return HEIGHT display rows of PANEL rendered for TERMINAL, each exactly WIDTH cells.

FOCUSED-P says the panel takes the keyboard, which it shows."))

(defgeneric screen-panel-click (panel column row)
  (:documentation "Act on a click at COLUMN and ROW of PANEL, both from 1.

Return :REDRAW to repaint, :DISMISS to return to the channel,
(:SWITCH :SESSION-ID ID) to move to channel ID, a (:CHANNEL-COMMAND NAME [ID])
event to run a channel command in the attached application, or NIL."))

(defgeneric screen-panel-event (panel event)
  (:documentation "Handle a keyboard or wheel EVENT while PANEL has the focus; return a click-style action."))

(defgeneric screen-panel-focus (panel)
  (:documentation "Prepare PANEL to take the keyboard."))

(defmethod screen-panel-focus ((panel t))
  "A panel without a selection has nothing to prepare."
  nil)

(defmethod screen-panel-event ((panel t) event)
  "Ignore keys for panels without keyboard navigation."
  (declare (ignore event))
  nil)

(defgeneric screen-panel-shown-p (panel)
  (:documentation "Return PANEL's visibility preference, or :SCREEN to use the screen's.

Specialize this and its setter to retain the choice across reconnects and
switches. The default methods leave the choice local to each screen."))

(defgeneric (setf screen-panel-shown-p) (shown-p panel)
  (:documentation "Record whether the person wants PANEL beside the session."))

(defmethod screen-panel-click ((panel t) column row)
  "A panel without clickable rows ignores clicks."
  (declare (ignore column row))
  nil)

(defmethod screen-panel-shown-p ((panel t))
  "A panel without a memory of its own leaves the choice to the screen."
  ':screen)

(defmethod (setf screen-panel-shown-p) (shown-p (panel t))
  "A panel without a memory of its own leaves the choice to the screen."
  shown-p)


;;;; -- Screens --

(defclass world-terminal-screen ()
  ((terminal
    :initarg       :terminal
    :reader        world-terminal-screen-terminal
    :type          terminal
    :documentation "The client's own terminal, painted directly.")
   (painter
    :initform      (clinedi:make-frame-painter)
    :reader        world-terminal-screen-painter
    :documentation "Paints frames by rewriting the rows that changed since the last one.")
   (lock
    :initform      (make-lock "Autolith client screen")
    :reader        world-terminal-screen-lock
    :documentation "The lock serializing frame arrival and painting across threads.")
   (token
    :initform      nil
    :accessor      world-terminal-screen-token
    :documentation "The native mode token captured when the screen was taken.")
   (active-p
    :initform      nil
    :accessor      world-terminal-screen-active-p
    :type          boolean
    :documentation "Whether the client shows frames in its alternate buffer.")
   (frame
    :initform      nil
    :accessor      world-terminal-screen-frame
    :type          (option clinedi:frame-snapshot)
    :documentation "The pane's frame, or NIL before the first one.")
   (channel-drafts
    :initform      (make-hash-table :test #'equal)
    :reader        world-terminal-screen-channel-drafts
    :type          hash-table
    :documentation "Unsent input and cursor positions by channel, private to this terminal client.")

   (boot-ui
    :initform      nil
    :accessor      world-terminal-screen-boot-ui
    :type          (option terminal-ui)
    :documentation "The initial boot panel renderer, or NIL for ordinary channel switches.")
   (loading
    :initform      nil
    :accessor      world-terminal-screen-loading
    :type          list
    :documentation "The loading title, milestone and description, independent of channel frames.")
   (painted-layout
    :initform      nil
    :accessor      world-terminal-screen-painted-layout
    :documentation "The installed layout, used for pointer routing until a matching new frame arrives.")
   (painted-panel-width
    :initform      0
    :accessor      world-terminal-screen-painted-panel-width
    :type          (integer 0)
    :documentation "The installed panel width, independent of a pending resize.")
   (drawer-open-p
    :initform      nil
    :accessor      world-terminal-screen-drawer-open-p
    :type          boolean
    :documentation "Whether the person explicitly opened the panel on a narrow terminal.")
   (panel-focus-p
    :initform      nil
    :accessor      world-terminal-screen-panel-focus-p
    :type          boolean
    :documentation "Whether the person moved the keyboard to the panel beside the session.")
   (cursor
    :initform      (list 0 0 t)
    :accessor      world-terminal-screen-cursor
    :type          list
    :documentation "The cursor row, column and visibility of the frame.")
   (whole-screen-p
    :initform      nil
    :accessor      world-terminal-screen-whole-screen-p
    :type          boolean
    :documentation "Whether the pane's frame asks for the whole screen, hiding the side panel.")
   (panel
    :initarg       :panel
    :initform      nil
    :reader        world-terminal-screen-panel
    :documentation "The side panel composed left of the session's rows, or NIL.")
   (own-panel-shown-p
    :initform      t
    :accessor      world-terminal-screen-own-panel-shown-p
    :type          boolean
    :documentation "Whether the person wants the side panel, for a panel that keeps no choice of its own.")
   (resize-pending-p
    :initform      nil
    :accessor      world-terminal-screen-resize-pending-p
    :type          boolean
    :documentation "Whether the session's pane changed size since the client last told the session."))
  (:documentation "The whole screen of a terminal client: the pane of the channel it shows, beside its side panel."))

(-> world-terminal-screen-create (&key (:terminal terminal) (:panel t)) world-terminal-screen)
(defun world-terminal-screen-create (&key terminal panel)
  "Return a screen painting a channel's pane on the client's TERMINAL beside PANEL."
  (make-instance 'world-terminal-screen :terminal terminal :panel panel))

(-> world-terminal-screen-panel-shown-p (world-terminal-screen) boolean)
(defun world-terminal-screen-panel-shown-p (screen)
  "Return whether the person wants SCREEN's side panel: the panel's own memory, else the screen's."
  (let* ((panel (world-terminal-screen-panel screen))
         (kept  (and panel (screen-panel-shown-p panel))))
    (if (eq kept ':screen)
        (world-terminal-screen-own-panel-shown-p screen)
        (and kept t))))

(-> world-terminal-screen-layout (world-terminal-screen) (member :session :beside :alone))
(defun world-terminal-screen-layout (screen)
  "Return how SCREEN shares its terminal: the session alone, the side panel beside it, or the panel alone.

The panel shows beside a channel with enough pane columns after subtracting
its width and divider. Otherwise it opens only after an explicit toggle."
  (let ((panel (world-terminal-screen-panel screen)))
    (cond
      ((or (not (world-terminal-screen-active-p screen))
           (null panel) (world-terminal-screen-whole-screen-p screen))
       ':session)
      ((world-terminal-screen--compact-p screen)
       (if (world-terminal-screen-drawer-open-p screen) ':alone ':session))
      ((not (world-terminal-screen-panel-shown-p screen))
       ':session)
      (t
       ':beside))))

(-> world-terminal-screen--compact-p (world-terminal-screen) boolean)
(defun world-terminal-screen--compact-p (screen)
  "Return whether docking SCREEN's panel would leave too few session columns."
  (let* ((panel   (world-terminal-screen-panel screen))
         (columns (terminal-columns (world-terminal-screen-terminal screen))))
    (and panel
         (< (- columns (screen-panel-width panel columns) 1)
            (screen-panel-minimum-columns panel)))))

(-> world-terminal-screen-panel-visible-p (world-terminal-screen) boolean)
(defun world-terminal-screen-panel-visible-p (screen)
  "Return true when SCREEN's side panel takes part or all of the terminal."
  (not (eq (world-terminal-screen-layout screen) ':session)))

(-> world-terminal-screen-panel-focused-p (world-terminal-screen) boolean)
(defun world-terminal-screen-panel-focused-p (screen)
  "Return whether SCREEN's side panel takes the keyboard: beside the session by choice, alone always."
  (ecase (world-terminal-screen-layout screen)
    (:session
     nil)
    (:beside
     (world-terminal-screen-panel-focus-p screen))
    (:alone
     t)))

(-> world-terminal-screen--panel-width (world-terminal-screen) (integer 0))
(defun world-terminal-screen--panel-width (screen)
  "Return the columns SCREEN's side panel takes: none, its share beside the session, or all."
  (let ((columns (terminal-columns (world-terminal-screen-terminal screen))))
    (ecase (world-terminal-screen-layout screen)
      (:session
       0)
      (:beside
       (screen-panel-width (world-terminal-screen-panel screen) columns))
      (:alone
       columns))))

(-> world-terminal-screen-pane-size (world-terminal-screen) (values integer integer))
(defun world-terminal-screen-pane-size (screen)
  "Return the rows and columns the session's pane gets of SCREEN's terminal.

Beside the panel it gets the columns the panel and its rule leave. Behind a
panel that takes the whole screen it keeps the whole width, so hiding the
panel shows it without a resize."
  (let ((terminal (world-terminal-screen-terminal screen)))
    (values (terminal-rows terminal)
            (if (eq (world-terminal-screen-layout screen) ':beside)
                (max 1 (- (terminal-columns terminal) (world-terminal-screen--panel-width screen) 1))
                (terminal-columns terminal)))))

(-> world-terminal-screen-take-resize (world-terminal-screen) boolean)
(defun world-terminal-screen-take-resize (screen)
  "Return whether the pane changed size since the last call, clearing the note."
  (with-lock-held ((world-terminal-screen-lock screen))
    (shiftf (world-terminal-screen-resize-pending-p screen) nil)))

(-> world-terminal-screen-filter-event (world-terminal-screen t) t)
(defun world-terminal-screen-filter-event (screen event)
  "Return EVENT as the session should see it, :IGNORE when SCREEN consumed it, or a panel's request.

The toggle key shows or hides the side panel, and the focus key moves the
keyboard to it and back. A click lands in the panel, which takes the focus and
acts on it, or in the
session's pane, whose columns start after the panel and which takes the focus
back. A drag and its release go to the pane; over the panel they stay on the
pane's edge, so selecting never takes the panel's text. A focused panel gets
every key and the wheel; the session gets none until the focus returns. A
panel's (:SWITCH :SESSION-ID ID) request moves the terminal to another channel
and its channel command goes to the session."
  (unless (world-terminal-screen-active-p screen)
    (return-from world-terminal-screen-filter-event event))
  (let* ((pointer-p (typep event '(cons (member :click :drag :release) (cons integer (cons integer null)))))
         (click-p   (and pointer-p (eq (first event) ':click)))
         (geometry  (with-lock-held ((world-terminal-screen-lock screen))
                      (list (or (world-terminal-screen-painted-layout screen) (world-terminal-screen-layout screen))
                            (if (world-terminal-screen-painted-layout screen)
                                (world-terminal-screen-painted-panel-width screen)
                                (world-terminal-screen--panel-width screen)))))
         (layout    (first geometry)))
    (cond
      ((null (world-terminal-screen-panel screen))
       event)
      ((equal event *world-terminal-screen-toggle-event*)
       (world-terminal-screen-toggle-panel screen)
       ':ignore)
      ((equal event *world-terminal-screen-focus-event*)
       ;; A hidden panel is shown first; a narrow terminal's drawer has the
       ;; focus by being open. The wanted layout decides, not the painted one.
       (let ((beside-p (eq (world-terminal-screen-layout screen) ':beside)))
         (unless beside-p
           (world-terminal-screen-toggle-panel screen))
         (world-terminal-screen-focus-panel
          screen (not (and beside-p (world-terminal-screen-panel-focus-p screen)))))
       ':ignore)
      ((member event '(:interrupt :end-of-input :stream-end))
       event)
      ((and click-p (or (eq layout ':alone)
                        (and (eq layout ':beside) (<= (second event) (1+ (second geometry))))))
       (world-terminal-screen-focus-panel screen t)
       (world-terminal-screen--panel-result
        screen (screen-panel-click (world-terminal-screen-panel screen) (second event) (third event))))
      ((and pointer-p (eq layout ':beside))
       (let ((offset (1+ (second geometry))))
         (when click-p
           (world-terminal-screen-focus-panel screen nil))
         ;; A drag over the panel stays on the pane's edge, so a selection
         ;; never takes the panel's text.
         (list (first event) (max 1 (- (second event) offset)) (third event))))
      ((world-terminal-screen-panel-focused-p screen)
       (world-terminal-screen--panel-result
        screen (screen-panel-event (world-terminal-screen-panel screen) event)))
      (t
       event))))

(-> world-terminal-screen-focus-panel (world-terminal-screen boolean) null)
(defun world-terminal-screen-focus-panel (screen focused-p)
  "Move the keyboard to SCREEN's side panel beside the session, or back when FOCUSED-P is false."
  (with-lock-held ((world-terminal-screen-lock screen))
    (unless (eq focused-p (world-terminal-screen-panel-focus-p screen))
      (setf (world-terminal-screen-panel-focus-p screen) focused-p)
      (when focused-p
        (screen-panel-focus (world-terminal-screen-panel screen)))
      (world-terminal-screen--paint screen)))
  nil)

(-> world-terminal-screen--panel-result (world-terminal-screen t) t)
(defun world-terminal-screen--panel-result (screen result)
  "Apply a panel RESULT; a channel, a command or a dismissal gives the keyboard back to the session."
  (when (or (consp result) (eq result ':dismiss))
    (when (world-terminal-screen-drawer-open-p screen)
      (world-terminal-screen-toggle-panel screen))
    (world-terminal-screen-focus-panel screen nil))
  (cond
    ((eq result ':redraw)
     (with-lock-held ((world-terminal-screen-lock screen))
       (world-terminal-screen--paint screen))
     ':ignore)
    ((consp result)
     result)
    (t
     ':ignore)))

(-> world-terminal-screen-toggle-panel (world-terminal-screen) null)
(defun world-terminal-screen-toggle-panel (screen)
  "Show or hide SCREEN's side panel and report any pane-size change to the session.

The keyboard stays with the session beside a shown panel; a narrow terminal's
drawer has it while open. Store the choice on the screen and, if supported, on
the panel for reuse across attachments."
  (let ((panel (world-terminal-screen-panel screen)))
    (when (and panel (world-terminal-screen-active-p screen))
      (with-lock-held ((world-terminal-screen-lock screen))
        (let ((layout   (world-terminal-screen-layout screen))
              (columns  (nth-value 1 (world-terminal-screen-pane-size screen)))
              (wanted-p (not (world-terminal-screen-panel-shown-p screen))))
          (if (world-terminal-screen--compact-p screen)
              (setf (world-terminal-screen-drawer-open-p screen)
                    (not (world-terminal-screen-drawer-open-p screen)))
              (setf (screen-panel-shown-p panel) wanted-p
                    (world-terminal-screen-own-panel-shown-p screen) wanted-p
                    (world-terminal-screen-panel-focus-p screen) nil))
          (unless (eq layout (world-terminal-screen-layout screen))
            (when (world-terminal-screen-panel-focused-p screen)
              (screen-panel-focus panel))
            (unless (= columns (nth-value 1 (world-terminal-screen-pane-size screen)))
              (setf (world-terminal-screen-resize-pending-p screen) t))
            (terminal-screen-invalidate (world-terminal-screen-terminal screen)
                                        (world-terminal-screen-painter screen))
            (world-terminal-screen--paint screen))))))
  nil)

(-> world-terminal-screen-tick (world-terminal-screen) null)
(defun world-terminal-screen-tick (screen)
  "Refresh SCREEN's side panel and repaint when its content changed."
  (let ((panel (world-terminal-screen-panel screen)))
    (when (and panel
               (world-terminal-screen-active-p screen)
               (world-terminal-screen-panel-visible-p screen)
               (screen-panel-refresh panel))
      (with-lock-held ((world-terminal-screen-lock screen))
        (world-terminal-screen--paint screen))))
  nil)

(-> world-terminal-screen-show-frame
    (world-terminal-screen clinedi:frame-snapshot
     &key (:cursor-row (integer 0)) (:cursor-column (integer 0)) (:cursor-visible-p t)
          (:whole-screen-p t))
    null)
(defun world-terminal-screen-show-frame
    (screen frame &key (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p)
  "Show FRAME, the pane's complete rows, with its cursor, and paint what changed.

The first frame that fits the pane takes the screen. WHOLE-SCREEN-P hides the
side panel for the frame."
  (with-lock-held ((world-terminal-screen-lock screen))
    (world-terminal-screen--take-whole-screen screen (not (null whole-screen-p)))
    (setf (world-terminal-screen-frame screen)   frame
          (world-terminal-screen-cursor screen)  (list cursor-row cursor-column
                                                   (not (null cursor-visible-p)))
          (world-terminal-screen-loading screen) nil
          (world-terminal-screen-boot-ui screen) nil)
    (when (and (not (world-terminal-screen-active-p screen))
               (world-terminal-screen--frame-fits-p screen))
      (world-terminal-screen--enter screen))
    (world-terminal-screen--paint screen))
  nil)

(-> world-terminal-screen-show-loading
    (world-terminal-screen string &key (:step (integer 1 6)) (:description string)
                                  (:start-p boolean) (:boot-p boolean) (:agent-name (option string)))
    null)
(defun world-terminal-screen-show-loading
    (screen text &key (step 1) (description "Connecting to world") start-p boot-p agent-name)
  "Show actual channel-opening progress, using the whole boot panel for initial startup.

START-P permits taking the terminal. BOOT-P selects the shared boot panel
before the first channel frame; channel switches retain their side panel and
highlight AGENT-NAME when known."
  (with-lock-held ((world-terminal-screen-lock screen))
    (when (or start-p (world-terminal-screen-active-p screen))
      (when boot-p
        (setf (world-terminal-screen-boot-ui screen)
              (terminal-ui-create :terminal (world-terminal-screen-terminal screen) :fullscreen-p t)))
      (unless (world-terminal-screen-active-p screen)
        (world-terminal-screen--enter screen))
      (setf (world-terminal-screen-drawer-open-p screen) nil
            (world-terminal-screen-frame screen) nil
            (world-terminal-screen-loading screen) (list text step description agent-name)
            (world-terminal-screen-cursor screen) (list 0 0 nil))
      (world-terminal-screen--take-whole-screen screen (not (null (world-terminal-screen-boot-ui screen))))
      (world-terminal-screen--paint screen)))
  nil)

(-> world-terminal-screen--loading-frame (world-terminal-screen) clinedi:frame-snapshot)
(defun world-terminal-screen--loading-frame (screen)
  "Render SCREEN's current milestone centered in the current channel pane."
  (destructuring-bind (title step description agent-name) (world-terminal-screen-loading screen)
    (multiple-value-bind (height width) (world-terminal-screen-pane-size screen)
      (when (world-terminal-screen-boot-ui screen)
        (return-from world-terminal-screen--loading-frame
          (clinedi:frame-snapshot-apply
           nil
           (loop for row in (terminal-ui--boot-screen-frame
                             (world-terminal-screen-boot-ui screen)
                             :phase title :detail (format nil "~D/6  ~A" step description)
                             :height height)
                 for index from 0 collect (list index row))
           :height height :width width :complete-p t)))
      (let* ((content-width (min 54 width))
             (bar           (terminal--bar-spans content-width 0
                                                 (floor (* content-width (1- step)) 6)))
             (stage         (list (terminal-span ':legend-accent (format nil "~D/6" step))
                                  (terminal-span ':dim (format nil "  ~A" description))))
             (heading       (cons (terminal-span ':brand title)
                                  (when agent-name
                                    (list (terminal-span ':plain " ")
                                          (terminal-span ':agent-name agent-name)))))
             (lines         (cond
                              ((>= height 4)
                               (list heading nil bar stage))
                              ((>= height 2)
                               (list bar stage))
                              (t
                               (list stage))))
             (top           (floor (- height (length lines)) 2))
             (left          (make-string (floor (- width content-width) 2) :initial-element #\Space)))
        (clinedi:frame-snapshot-apply
         nil
         (loop for spans in lines
               for row from top
               collect (list row
                             (terminal--render-spans
                              (world-terminal-screen-terminal screen)
                              (terminal--clip-spans
                               (cons (terminal-span ':plain left) spans) width))))
         :height height :width width :complete-p t)))))

(-> world-terminal-screen-resized (world-terminal-screen) null)
(defun world-terminal-screen-resized (screen)
  "Invalidate SCREEN after a resize; paint only a frame matching the new geometry."
  (with-lock-held ((world-terminal-screen-lock screen))
    (setf (world-terminal-screen-drawer-open-p screen) nil)
    (terminal-screen-invalidate (world-terminal-screen-terminal screen)
                                (world-terminal-screen-painter screen))
    (world-terminal-screen--paint screen))
  nil)

(-> world-terminal-screen-close (world-terminal-screen) null)
(defun world-terminal-screen-close (screen)
  "Give SCREEN's terminal back when the client still shows a session's frames."
  (with-lock-held ((world-terminal-screen-lock screen))
    (world-terminal-screen--leave screen))
  nil)

(-> world-terminal-screen--take-whole-screen (world-terminal-screen boolean) null)
(defun world-terminal-screen--take-whole-screen (screen whole-screen-p)
  "Record whether the session wants the whole screen, as WHOLE-SCREEN-P says.

When that shows or hides the side panel, the session's pane changes size: the
client tells the session, and the next paint rewrites the whole screen."
  (let ((visible-p (world-terminal-screen-panel-visible-p screen)))
    (setf (world-terminal-screen-whole-screen-p screen) whole-screen-p)
    (unless (eq visible-p (world-terminal-screen-panel-visible-p screen))
      (setf (world-terminal-screen-resize-pending-p screen) t)
      (terminal-screen-invalidate (world-terminal-screen-terminal screen)
                                  (world-terminal-screen-painter screen))))
  nil)

(-> world-terminal-screen--enter (world-terminal-screen) null)
(defun world-terminal-screen--enter (screen)
  "Take the client's screen for the session's frames."
  (let ((terminal (world-terminal-screen-terminal screen))
        (columns  (nth-value 1 (world-terminal-screen-pane-size screen))))
    (setf (world-terminal-screen-token screen) (terminal-screen-enter terminal)
          (world-terminal-screen-active-p screen) t)
    (unless (= columns (nth-value 1 (world-terminal-screen-pane-size screen)))
      (setf (world-terminal-screen-resize-pending-p screen) t))
    (terminal-screen-invalidate terminal (world-terminal-screen-painter screen)))
  nil)

(-> world-terminal-screen--leave (world-terminal-screen) null)
(defun world-terminal-screen--leave (screen)
  "Give the client's screen back and forget the session's frame."
  (when (world-terminal-screen-active-p screen)
    (let ((terminal (world-terminal-screen-terminal screen))
          (columns  (nth-value 1 (world-terminal-screen-pane-size screen))))
      (unwind-protect
           (progn
             (terminal-screen-leave terminal (shiftf (world-terminal-screen-token screen) nil))
             (setf (world-terminal-screen-active-p screen) nil
                   (world-terminal-screen-frame screen) nil
                   (world-terminal-screen-loading screen) nil
                   (world-terminal-screen-boot-ui screen) nil)
             (unless (= columns (nth-value 1 (world-terminal-screen-pane-size screen)))
               (setf (world-terminal-screen-resize-pending-p screen) t)))
        (terminal-screen-invalidate terminal (world-terminal-screen-painter screen)))))
  nil)

(-> world-terminal-screen--frame-fits-p (world-terminal-screen) boolean)
(defun world-terminal-screen--frame-fits-p (screen)
  "Return whether the cached frame matches the requested pane geometry."
  (let ((frame (world-terminal-screen-frame screen)))
    (multiple-value-bind (height width) (world-terminal-screen-pane-size screen)
      (and frame
           (= height (clinedi:frame-snapshot-height frame))
           (= width (clinedi:frame-snapshot-width frame))))))

(-> world-terminal-screen--paint (world-terminal-screen) null)
(defun world-terminal-screen--paint (screen)
  "Paint the session's frame, beside the side panel when it shows, on the client's terminal."
  (let ((frame (or (world-terminal-screen-frame screen)
                   (when (world-terminal-screen-loading screen)
                     (world-terminal-screen--loading-frame screen)))))
    (when (and frame (world-terminal-screen-active-p screen)
               (or (world-terminal-screen-loading screen)
                   (world-terminal-screen--frame-fits-p screen)))
      (destructuring-bind (cursor-row cursor-column cursor-visible-p)
          (world-terminal-screen-cursor screen)
        (let ((terminal (world-terminal-screen-terminal screen))
              (painter  (world-terminal-screen-painter screen))
              (rows     (clinedi:frame-snapshot-rows frame)))
          (ecase (world-terminal-screen-layout screen)
            (:session
             (terminal-screen-paint terminal painter (coerce rows 'list)
                                    :cursor-row       cursor-row
                                    :cursor-column    cursor-column
                                    :cursor-visible-p cursor-visible-p))
            (:beside
             (terminal-screen-paint terminal painter (world-terminal-screen--composed-rows screen rows)
                                    :cursor-row       cursor-row
                                    :cursor-column    (+ cursor-column 1 (world-terminal-screen--panel-width screen))
                                    :cursor-visible-p (and cursor-visible-p
                                                           (not (world-terminal-screen-panel-focus-p screen)))))
            (:alone
             (terminal-screen-paint terminal painter
                                    (screen-panel-rows (world-terminal-screen-panel screen) terminal
                                                       :height    (max 1 (terminal-rows terminal))
                                                       :width     (terminal-columns terminal)
                                                       :focused-p t)
                                    :cursor-row       0
                                    :cursor-column    0
                                    :cursor-visible-p nil)))))
      (setf (world-terminal-screen-painted-layout screen) (world-terminal-screen-layout screen)
            (world-terminal-screen-painted-panel-width screen) (world-terminal-screen--panel-width screen))))
  nil)

(-> world-terminal-screen--composed-rows (world-terminal-screen vector) list)
(defun world-terminal-screen--composed-rows (screen frame)
  "Return FRAME's rows with the side panel's rows before them, separated by a rule.

The rule is heavy and bright while the panel has the focus."
  (let* ((terminal   (world-terminal-screen-terminal screen))
         (height     (max 1 (terminal-rows terminal)))
         (focused-p  (world-terminal-screen-panel-focus-p screen))
         (panel-rows (screen-panel-rows (world-terminal-screen-panel screen) terminal
                                        :height    height
                                        :width     (world-terminal-screen--panel-width screen)
                                        :focused-p focused-p))
         (rule       (terminal--render-spans
                      terminal (list (if focused-p
                                         (terminal-span ':legend-accent "┃")
                                         (terminal-span ':dim "│"))))))
    (loop for index below height
          collect (concatenate 'string
                               (or (nth index panel-rows) "")
                               rule
                               (if (< index (length frame))
                                   (aref frame index)
                                   "")))))
