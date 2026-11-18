(in-package #:autolith)

;;;; -- Client Screen --

;;; A terminal client of a hosted fullscreen session receives the session's
;;; screen as frames of display rows rather than as painted output, and
;;; paints them on its own terminal. The client therefore owns the alternate
;;; buffer, the frame painter and the cursor, and composes its own side
;;; panel beside the session's rows.

(defparameter *localgroup-screen-dimension-limit* 10000
  "The largest frame height or width a client accepts from a session.")

(defparameter *localgroup-screen-toggle-event* '(:control #\s)
  "The key event showing or hiding the side panel.")

(defparameter *localgroup-screen-focus-event* ':focus-next
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

(defclass localgroup-screen ()
  ((terminal
    :initarg       :terminal
    :reader        localgroup-screen-terminal
    :type          terminal
    :documentation "The client's own terminal, painted directly.")
   (painter
    :initform      (clinedi:make-frame-painter)
    :reader        localgroup-screen-painter
    :documentation "Paints frames by rewriting the rows that changed since the last one.")
   (lock
    :initform      (make-lock "Autolith client screen")
    :reader        localgroup-screen-lock
    :documentation "The lock serializing frame arrival and painting across threads.")
   (token
    :initform      nil
    :accessor      localgroup-screen-token
    :documentation "The native mode token captured when the screen was taken.")
   (active-p
    :initform      nil
    :accessor      localgroup-screen-active-p
    :type          boolean
    :documentation "Whether the client shows frames in its alternate buffer.")
   (frame
    :initform      nil
    :accessor      localgroup-screen-frame
    :type          (option clinedi:frame-snapshot)
    :documentation "The session's frame snapshot, or NIL before a complete frame.")

   (boot-ui
    :initform      nil
    :accessor      localgroup-screen-boot-ui
    :type          (option terminal-ui)
    :documentation "The initial boot panel renderer, or NIL for ordinary channel switches.")
   (loading
    :initform      nil
    :accessor      localgroup-screen-loading
    :type          list
    :documentation "The loading title, milestone and description, independent of channel frames.")
   (painted-layout
    :initform      nil
    :accessor      localgroup-screen-painted-layout
    :documentation "The installed layout, used for pointer routing until a matching new frame arrives.")
   (painted-panel-width
    :initform      0
    :accessor      localgroup-screen-painted-panel-width
    :type          (integer 0)
    :documentation "The installed panel width, independent of a pending resize.")
   (drawer-open-p
    :initform      nil
    :accessor      localgroup-screen-drawer-open-p
    :type          boolean
    :documentation "Whether the person explicitly opened the panel on a narrow terminal.")
   (panel-focus-p
    :initform      nil
    :accessor      localgroup-screen-panel-focus-p
    :type          boolean
    :documentation "Whether the person moved the keyboard to the panel beside the session.")
   (cursor
    :initform      (list 0 0 t)
    :accessor      localgroup-screen-cursor
    :type          list
    :documentation "The cursor row, column and visibility of the frame.")
   (whole-screen-p
    :initform      nil
    :accessor      localgroup-screen-whole-screen-p
    :type          boolean
    :documentation "Whether the session's frame asks for the whole screen, hiding the side panel.")
   (panel
    :initarg       :panel
    :initform      nil
    :reader        localgroup-screen-panel
    :documentation "The side panel composed left of the session's rows, or NIL.")
   (own-panel-shown-p
    :initform      t
    :accessor      localgroup-screen-own-panel-shown-p
    :type          boolean
    :documentation "Whether the person wants the side panel, for a panel that keeps no choice of its own.")
   (resize-pending-p
    :initform      nil
    :accessor      localgroup-screen-resize-pending-p
    :type          boolean
    :documentation "Whether the session's pane changed size since the client last told the session."))
  (:documentation "The whole screen a client paints from a hosted session's frames."))

(-> localgroup-screen-create (&key (:terminal terminal) (:panel t)) localgroup-screen)
(defun localgroup-screen-create (&key terminal panel)
  "Return a screen painting a session's frames on the client's TERMINAL beside PANEL."
  (make-instance 'localgroup-screen :terminal terminal :panel panel))

(-> localgroup-screen-panel-shown-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-shown-p (screen)
  "Return whether the person wants SCREEN's side panel: the panel's own memory, else the screen's."
  (let* ((panel (localgroup-screen-panel screen))
         (kept  (and panel (screen-panel-shown-p panel))))
    (if (eq kept ':screen)
        (localgroup-screen-own-panel-shown-p screen)
        (and kept t))))

(-> localgroup-screen-layout (localgroup-screen) (member :session :beside :alone))
(defun localgroup-screen-layout (screen)
  "Return how SCREEN shares its terminal: the session alone, the side panel beside it, or the panel alone.

The panel shows beside a channel with enough pane columns after subtracting
its width and divider. Otherwise it opens only after an explicit toggle."
  (let ((panel (localgroup-screen-panel screen)))
    (cond
      ((or (not (localgroup-screen-active-p screen))
           (null panel) (localgroup-screen-whole-screen-p screen))
       ':session)
      ((localgroup-screen--compact-p screen)
       (if (localgroup-screen-drawer-open-p screen) ':alone ':session))
      ((not (localgroup-screen-panel-shown-p screen))
       ':session)
      (t
       ':beside))))

(-> localgroup-screen--compact-p (localgroup-screen) boolean)
(defun localgroup-screen--compact-p (screen)
  "Return whether docking SCREEN's panel would leave too few session columns."
  (let* ((panel   (localgroup-screen-panel screen))
         (columns (terminal-columns (localgroup-screen-terminal screen))))
    (and panel
         (< (- columns (screen-panel-width panel columns) 1)
            (screen-panel-minimum-columns panel)))))

(-> localgroup-screen-panel-visible-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-visible-p (screen)
  "Return true when SCREEN's side panel takes part or all of the terminal."
  (not (eq (localgroup-screen-layout screen) ':session)))

(-> localgroup-screen-panel-focused-p (localgroup-screen) boolean)
(defun localgroup-screen-panel-focused-p (screen)
  "Return whether SCREEN's side panel takes the keyboard: beside the session by choice, alone always."
  (ecase (localgroup-screen-layout screen)
    (:session
     nil)
    (:beside
     (localgroup-screen-panel-focus-p screen))
    (:alone
     t)))

(-> localgroup-screen--panel-width (localgroup-screen) (integer 0))
(defun localgroup-screen--panel-width (screen)
  "Return the columns SCREEN's side panel takes: none, its share beside the session, or all."
  (let ((columns (terminal-columns (localgroup-screen-terminal screen))))
    (ecase (localgroup-screen-layout screen)
      (:session
       0)
      (:beside
       (screen-panel-width (localgroup-screen-panel screen) columns))
      (:alone
       columns))))

(-> localgroup-screen-pane-size (localgroup-screen) (values integer integer))
(defun localgroup-screen-pane-size (screen)
  "Return the rows and columns the session's pane gets of SCREEN's terminal.

Beside the panel it gets the columns the panel and its rule leave. Behind a
panel that takes the whole screen it keeps the whole width, so hiding the
panel shows it without a resize."
  (let ((terminal (localgroup-screen-terminal screen)))
    (values (terminal-rows terminal)
            (if (eq (localgroup-screen-layout screen) ':beside)
                (max 1 (- (terminal-columns terminal) (localgroup-screen--panel-width screen) 1))
                (terminal-columns terminal)))))

(-> localgroup-screen-take-resize (localgroup-screen) boolean)
(defun localgroup-screen-take-resize (screen)
  "Return whether the pane changed size since the last call, clearing the note."
  (with-lock-held ((localgroup-screen-lock screen))
    (shiftf (localgroup-screen-resize-pending-p screen) nil)))

(-> localgroup-screen-filter-event (localgroup-screen t) t)
(defun localgroup-screen-filter-event (screen event)
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
  (unless (localgroup-screen-active-p screen)
    (return-from localgroup-screen-filter-event event))
  (let* ((pointer-p (typep event '(cons (member :click :drag :release) (cons integer (cons integer null)))))
         (click-p   (and pointer-p (eq (first event) ':click)))
         (geometry  (with-lock-held ((localgroup-screen-lock screen))
                      (list (or (localgroup-screen-painted-layout screen) (localgroup-screen-layout screen))
                            (if (localgroup-screen-painted-layout screen)
                                (localgroup-screen-painted-panel-width screen)
                                (localgroup-screen--panel-width screen)))))
         (layout    (first geometry)))
    (cond
      ((null (localgroup-screen-panel screen))
       event)
      ((equal event *localgroup-screen-toggle-event*)
       (localgroup-screen-toggle-panel screen)
       ':ignore)
      ((equal event *localgroup-screen-focus-event*)
       ;; A hidden panel is shown first; a narrow terminal's drawer has the
       ;; focus by being open. The wanted layout decides, not the painted one.
       (let ((beside-p (eq (localgroup-screen-layout screen) ':beside)))
         (unless beside-p
           (localgroup-screen-toggle-panel screen))
         (localgroup-screen-focus-panel
          screen (not (and beside-p (localgroup-screen-panel-focus-p screen)))))
       ':ignore)
      ((member event '(:interrupt :end-of-input :stream-end))
       event)
      ((and click-p (or (eq layout ':alone)
                        (and (eq layout ':beside) (<= (second event) (1+ (second geometry))))))
       (localgroup-screen-focus-panel screen t)
       (localgroup-screen--panel-result
        screen (screen-panel-click (localgroup-screen-panel screen) (second event) (third event))))
      ((and pointer-p (eq layout ':beside))
       (let ((offset (1+ (second geometry))))
         (when click-p
           (localgroup-screen-focus-panel screen nil))
         ;; A drag over the panel stays on the pane's edge, so a selection
         ;; never takes the panel's text.
         (list (first event) (max 1 (- (second event) offset)) (third event))))
      ((localgroup-screen-panel-focused-p screen)
       (localgroup-screen--panel-result
        screen (screen-panel-event (localgroup-screen-panel screen) event)))
      (t
       event))))

(-> localgroup-screen-focus-panel (localgroup-screen boolean) null)
(defun localgroup-screen-focus-panel (screen focused-p)
  "Move the keyboard to SCREEN's side panel beside the session, or back when FOCUSED-P is false."
  (with-lock-held ((localgroup-screen-lock screen))
    (unless (eq focused-p (localgroup-screen-panel-focus-p screen))
      (setf (localgroup-screen-panel-focus-p screen) focused-p)
      (when focused-p
        (screen-panel-focus (localgroup-screen-panel screen)))
      (localgroup-screen--paint screen)))
  nil)

(-> localgroup-screen--panel-result (localgroup-screen t) t)
(defun localgroup-screen--panel-result (screen result)
  "Apply a panel RESULT; a channel, a command or a dismissal gives the keyboard back to the session."
  (when (or (consp result) (eq result ':dismiss))
    (when (localgroup-screen-drawer-open-p screen)
      (localgroup-screen-toggle-panel screen))
    (localgroup-screen-focus-panel screen nil))
  (cond
    ((eq result ':redraw)
     (with-lock-held ((localgroup-screen-lock screen))
       (localgroup-screen--paint screen))
     ':ignore)
    ((consp result)
     result)
    (t
     ':ignore)))

(-> localgroup-screen-toggle-panel (localgroup-screen) null)
(defun localgroup-screen-toggle-panel (screen)
  "Show or hide SCREEN's side panel and report any pane-size change to the session.

The keyboard stays with the session beside a shown panel; a narrow terminal's
drawer has it while open. Store the choice on the screen and, if supported, on
the panel for reuse across attachments."
  (let ((panel (localgroup-screen-panel screen)))
    (when (and panel (localgroup-screen-active-p screen))
      (with-lock-held ((localgroup-screen-lock screen))
        (let ((layout   (localgroup-screen-layout screen))
              (columns  (nth-value 1 (localgroup-screen-pane-size screen)))
              (wanted-p (not (localgroup-screen-panel-shown-p screen))))
          (if (localgroup-screen--compact-p screen)
              (setf (localgroup-screen-drawer-open-p screen)
                    (not (localgroup-screen-drawer-open-p screen)))
              (setf (screen-panel-shown-p panel) wanted-p
                    (localgroup-screen-own-panel-shown-p screen) wanted-p
                    (localgroup-screen-panel-focus-p screen) nil))
          (unless (eq layout (localgroup-screen-layout screen))
            (when (localgroup-screen-panel-focused-p screen)
              (screen-panel-focus panel))
            (unless (= columns (nth-value 1 (localgroup-screen-pane-size screen)))
              (setf (localgroup-screen-resize-pending-p screen) t))
            (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                        (localgroup-screen-painter screen))
            (localgroup-screen--paint screen))))))
  nil)

(-> localgroup-screen-tick (localgroup-screen) null)
(defun localgroup-screen-tick (screen)
  "Refresh SCREEN's side panel and repaint when its content changed."
  (let ((panel (localgroup-screen-panel screen)))
    (when (and panel
               (localgroup-screen-active-p screen)
               (localgroup-screen-panel-visible-p screen)
               (screen-panel-refresh panel))
      (with-lock-held ((localgroup-screen-lock screen))
        (localgroup-screen--paint screen))))
  nil)

(-> localgroup-screen--frame-row-p (t) boolean)
(defun localgroup-screen--frame-row-p (row)
  "Return true when ROW is an (INDEX TEXT) pair of a frame."
  (and (consp row)
       (consp (rest row))
       (null (rest (rest row)))
       (typep (first row) '(integer 0))
       (stringp (second row))))

(-> localgroup-screen--frame-packet-p (t) boolean)
(defun localgroup-screen--frame-packet-p (packet)
  "Return true when PACKET is a well-formed frame."
  (and (consp packet)
       (eq (first packet) ':frame)
       (let ((properties (rest packet)))
         (and (listp properties)
              (handler-case (evenp (list-length properties))
                (type-error ()
                  nil))
              (typep (getf properties :height)
                     `(integer 1 ,*localgroup-screen-dimension-limit*))
              (typep (getf properties :width)
                     `(integer 1 ,*localgroup-screen-dimension-limit*))
              (typep (getf properties :cursor-row) '(integer 0))
              (typep (getf properties :cursor-column) '(integer 0))
              (let ((rows (getf properties :rows)))
                (and (listp rows)
                     (handler-case (not (null (list-length rows)))
                       (type-error ()
                         nil))
                     (every #'localgroup-screen--frame-row-p rows)))))))

(-> localgroup-screen-handle-packet (localgroup-screen t) boolean)
(defun localgroup-screen-handle-packet (screen packet)
  "Apply a frame or screen PACKET from the session and return whether it was one.

A complete frame takes the screen when the client does not show one yet. A
frame of changed rows arriving before any complete frame is dropped, since
there is nothing to change; the session sends a complete frame to every
client it has not synchronized."
  (cond
    ((localgroup-screen--frame-packet-p packet)
     (with-lock-held ((localgroup-screen-lock screen))
       (when (localgroup-screen--apply-frame screen (rest packet))
          (setf (localgroup-screen-loading screen) nil
                (localgroup-screen-boot-ui screen) nil)
         (localgroup-screen--paint screen)))
     t)
    ((equal packet '(:screen :leave))
     (with-lock-held ((localgroup-screen-lock screen))
       (localgroup-screen--leave screen))
     t)
    (t
     nil)))

(-> localgroup-screen-show-loading
    (localgroup-screen string &key (:step (integer 1 6)) (:description string)
                                  (:start-p boolean) (:boot-p boolean))
    null)
(defun localgroup-screen-show-loading
    (screen text &key (step 1) (description "Connecting to world") start-p boot-p)
  "Show actual channel-opening progress, using the whole boot panel for initial startup.

START-P permits taking the terminal. BOOT-P selects the shared boot panel
before the first channel frame; channel switches retain their side panel."
  (with-lock-held ((localgroup-screen-lock screen))
    (when (or start-p (localgroup-screen-active-p screen))
      (when boot-p
        (setf (localgroup-screen-boot-ui screen)
              (terminal-ui-create :terminal (localgroup-screen-terminal screen) :fullscreen-p t)))
      (unless (localgroup-screen-active-p screen)
        (localgroup-screen--enter screen))
      (setf (localgroup-screen-drawer-open-p screen) nil
            (localgroup-screen-frame screen) nil
            (localgroup-screen-loading screen) (list text step description)
            (localgroup-screen-cursor screen) (list 0 0 nil))
      (localgroup-screen--take-whole-screen screen (not (null (localgroup-screen-boot-ui screen))))
      (localgroup-screen--paint screen)))
  nil)

(-> localgroup-screen--loading-frame (localgroup-screen) clinedi:frame-snapshot)
(defun localgroup-screen--loading-frame (screen)
  "Render SCREEN's current milestone centered in the current channel pane."
  (destructuring-bind (title step description) (localgroup-screen-loading screen)
    (multiple-value-bind (height width) (localgroup-screen-pane-size screen)
      (when (localgroup-screen-boot-ui screen)
        (return-from localgroup-screen--loading-frame
          (clinedi:frame-snapshot-apply
           nil
           (loop for row in (terminal-ui--boot-screen-frame
                             (localgroup-screen-boot-ui screen)
                             :phase title :detail (format nil "~D/6  ~A" step description)
                             :height height)
                 for index from 0 collect (list index row))
           :height height :width width :complete-p t)))
      (let* ((content-width (min 54 width))
             (bar           (terminal--bar-spans content-width 0
                                                 (floor (* content-width (1- step)) 6)))
             (stage         (list (terminal-span ':legend-accent (format nil "~D/6" step))
                                  (terminal-span ':dim (format nil "  ~A" description))))
             (lines         (cond
                              ((>= height 4)
                               (list (list (terminal-span ':brand title)) nil bar stage))
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
                              (localgroup-screen-terminal screen)
                              (terminal--clip-spans
                               (cons (terminal-span ':plain left) spans) width))))
         :height height :width width :complete-p t)))))

(-> localgroup-screen-resized (localgroup-screen) null)
(defun localgroup-screen-resized (screen)
  "Invalidate SCREEN after a resize; paint only a frame matching the new geometry."
  (with-lock-held ((localgroup-screen-lock screen))
    (setf (localgroup-screen-drawer-open-p screen) nil)
    (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                (localgroup-screen-painter screen))
    (localgroup-screen--paint screen))
  nil)

(-> localgroup-screen-close (localgroup-screen) null)
(defun localgroup-screen-close (screen)
  "Give SCREEN's terminal back when the client still shows a session's frames."
  (with-lock-held ((localgroup-screen-lock screen))
    (localgroup-screen--leave screen))
  nil)

(-> localgroup-screen--apply-frame (localgroup-screen list) boolean)
(defun localgroup-screen--apply-frame (screen properties)
  "Record the frame PROPERTIES describe and return whether there is a frame to paint."
  (localgroup-screen--take-whole-screen screen (not (null (getf properties :whole-screen-p))))
  (let ((frame (clinedi:frame-snapshot-apply
                (localgroup-screen-frame screen) (getf properties :rows)
                :height     (getf properties :height)
                :width      (getf properties :width)
                :complete-p (not (null (getf properties :complete-p))))))
    (when frame
      (setf (localgroup-screen-frame screen) frame
            (localgroup-screen-cursor screen)
            (list (getf properties :cursor-row)
                  (getf properties :cursor-column)
                  (not (null (getf properties :cursor-visible-p)))))
      (unless (or (localgroup-screen-active-p screen)
                  (not (localgroup-screen--frame-fits-p screen)))
        (localgroup-screen--enter screen)))
    (not (null frame))))

(-> localgroup-screen--take-whole-screen (localgroup-screen boolean) null)
(defun localgroup-screen--take-whole-screen (screen whole-screen-p)
  "Record whether the session wants the whole screen, as WHOLE-SCREEN-P says.

When that shows or hides the side panel, the session's pane changes size: the
client tells the session, and the next paint rewrites the whole screen."
  (let ((visible-p (localgroup-screen-panel-visible-p screen)))
    (setf (localgroup-screen-whole-screen-p screen) whole-screen-p)
    (unless (eq visible-p (localgroup-screen-panel-visible-p screen))
      (setf (localgroup-screen-resize-pending-p screen) t)
      (terminal-screen-invalidate (localgroup-screen-terminal screen)
                                  (localgroup-screen-painter screen))))
  nil)

(-> localgroup-screen--enter (localgroup-screen) null)
(defun localgroup-screen--enter (screen)
  "Take the client's screen for the session's frames."
  (let ((terminal (localgroup-screen-terminal screen))
        (columns  (nth-value 1 (localgroup-screen-pane-size screen))))
    (setf (localgroup-screen-token screen) (terminal-screen-enter terminal)
          (localgroup-screen-active-p screen) t)
    (unless (= columns (nth-value 1 (localgroup-screen-pane-size screen)))
      (setf (localgroup-screen-resize-pending-p screen) t))
    (terminal-screen-invalidate terminal (localgroup-screen-painter screen)))
  nil)

(-> localgroup-screen--leave (localgroup-screen) null)
(defun localgroup-screen--leave (screen)
  "Give the client's screen back and forget the session's frame."
  (when (localgroup-screen-active-p screen)
    (let ((terminal (localgroup-screen-terminal screen))
          (columns  (nth-value 1 (localgroup-screen-pane-size screen))))
      (unwind-protect
           (progn
             (terminal-screen-leave terminal (shiftf (localgroup-screen-token screen) nil))
             (setf (localgroup-screen-active-p screen) nil
                   (localgroup-screen-frame screen) nil
                   (localgroup-screen-loading screen) nil
                   (localgroup-screen-boot-ui screen) nil)
             (unless (= columns (nth-value 1 (localgroup-screen-pane-size screen)))
               (setf (localgroup-screen-resize-pending-p screen) t)))
        (terminal-screen-invalidate terminal (localgroup-screen-painter screen)))))
  nil)

(-> localgroup-screen--frame-fits-p (localgroup-screen) boolean)
(defun localgroup-screen--frame-fits-p (screen)
  "Return whether the cached frame matches the requested pane geometry."
  (let ((frame (localgroup-screen-frame screen)))
    (multiple-value-bind (height width) (localgroup-screen-pane-size screen)
      (and frame
           (= height (clinedi:frame-snapshot-height frame))
           (= width (clinedi:frame-snapshot-width frame))))))

(-> localgroup-screen--paint (localgroup-screen) null)
(defun localgroup-screen--paint (screen)
  "Paint the session's frame, beside the side panel when it shows, on the client's terminal."
  (let ((frame (or (localgroup-screen-frame screen)
                   (when (localgroup-screen-loading screen)
                     (localgroup-screen--loading-frame screen)))))
    (when (and frame (localgroup-screen-active-p screen)
               (or (localgroup-screen-loading screen)
                   (localgroup-screen--frame-fits-p screen)))
      (destructuring-bind (cursor-row cursor-column cursor-visible-p)
          (localgroup-screen-cursor screen)
        (let ((terminal (localgroup-screen-terminal screen))
              (painter  (localgroup-screen-painter screen))
              (rows     (clinedi:frame-snapshot-rows frame)))
          (ecase (localgroup-screen-layout screen)
            (:session
             (terminal-screen-paint terminal painter (coerce rows 'list)
                                    :cursor-row       cursor-row
                                    :cursor-column    cursor-column
                                    :cursor-visible-p cursor-visible-p))
            (:beside
             (terminal-screen-paint terminal painter (localgroup-screen--composed-rows screen rows)
                                    :cursor-row       cursor-row
                                    :cursor-column    (+ cursor-column 1 (localgroup-screen--panel-width screen))
                                    :cursor-visible-p (and cursor-visible-p
                                                           (not (localgroup-screen-panel-focus-p screen)))))
            (:alone
             (terminal-screen-paint terminal painter
                                    (screen-panel-rows (localgroup-screen-panel screen) terminal
                                                       :height    (max 1 (terminal-rows terminal))
                                                       :width     (terminal-columns terminal)
                                                       :focused-p t)
                                    :cursor-row       0
                                    :cursor-column    0
                                    :cursor-visible-p nil)))))
      (setf (localgroup-screen-painted-layout screen) (localgroup-screen-layout screen)
            (localgroup-screen-painted-panel-width screen) (localgroup-screen--panel-width screen))))
  nil)

(-> localgroup-screen--composed-rows (localgroup-screen vector) list)
(defun localgroup-screen--composed-rows (screen frame)
  "Return FRAME's rows with the side panel's rows before them, separated by a rule.

The rule is heavy and bright while the panel has the focus."
  (let* ((terminal   (localgroup-screen-terminal screen))
         (height     (max 1 (terminal-rows terminal)))
         (focused-p  (localgroup-screen-panel-focus-p screen))
         (panel-rows (screen-panel-rows (localgroup-screen-panel screen) terminal
                                        :height    height
                                        :width     (localgroup-screen--panel-width screen)
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
