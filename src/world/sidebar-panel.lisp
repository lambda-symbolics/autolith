(in-package #:autolith)

;;;; -- World Sidebar Panel --

;;; The sidebar as a screen panel: its rows, its scrolling viewport, and what
;;; keys and clicks do while it has the focus.

(defparameter *world-sidebar-scroll-lines* 3
  "The lines one wheel step moves the tree.")

(defmethod screen-panel-shown-p ((panel world-sidebar))
  "The sidebar remembers whether the person hid it, across screens."
  (world-sidebar-shown-p panel))

(defmethod (setf screen-panel-shown-p) (shown-p (panel world-sidebar))
  "Record whether the person wants the sidebar, for every later screen of this terminal."
  (setf (world-sidebar-shown-p panel) (and shown-p t)))

(defmethod screen-panel-width ((panel world-sidebar) columns)
  "The sidebar's share of a terminal COLUMNS wide, but never fewer than its minimum."
  (max *world-sidebar-minimum-width* (round (* columns *world-sidebar-share*))))

(defmethod screen-panel-minimum-columns ((panel world-sidebar))
  "The narrowest terminal with room for both channel and sidebar; below it the drawer is explicit."
  *world-sidebar-minimum-columns*)

(defmethod screen-panel-refresh ((panel world-sidebar))
  "Take the newest gathered world and return whether the entries changed.

A started sidebar's thread gathers; without one the panel gathers itself when
a refresh is due."
  (if (world-sidebar-gatherer panel)
      (let ((gathered (with-lock-held ((world-sidebar-lock panel))
                        (shiftf (world-sidebar-gathered panel) nil))))
        (when gathered
          (if (eq (first gathered) ':failed)
              (world-sidebar--mark-stale panel (second gathered))
              (world-sidebar--accept panel (first gathered) (second gathered)))))
      (let ((now  (funcall (world-sidebar-clock-function panel)))
            (last (world-sidebar-refreshed-at panel)))
        (if (and last
                 (< (- now last) (* *world-sidebar-refresh-seconds* internal-time-units-per-second)))
            nil
            (progn
              (setf (world-sidebar-refreshed-at panel) now)
              (handler-case
                  (world-sidebar--accept panel (world-sidebar--gather panel))
                (error (condition)
                  (world-sidebar--mark-stale panel (world-sidebar--failure-name condition)))))))))

(defmethod screen-panel-focus ((panel world-sidebar))
  "Start at the channel this terminal shows."
  (let ((current (position-if #'sidebar-entry-current-p (world-sidebar-entries panel))))
    (when current
      (setf (world-sidebar-selected panel) current)))
  nil)


;;;; -- Rows --

(-> world-sidebar--justify (list list integer) list)
(defun world-sidebar--justify (left right width)
  "Return LEFT clipped so RIGHT ends at cell WIDTH, or LEFT alone when RIGHT is empty."
  (if right
      (let ((room (max 0 (- width (terminal--spans-width right) 1))))
        (append (terminal--pad-spans left room)
                (list (terminal-span ':plain " "))
                right))
      (terminal--clip-spans left width)))

(defmethod sidebar-entry-current-p ((entry sidebar-workspace))
  "A workspace is never the channel a terminal shows."
  (declare (ignore entry))
  nil)

(defgeneric sidebar-entry-level (entry)
  (:documentation "Return how deep ENTRY sits in the tree, the home channel at its root."))

(defmethod sidebar-entry-level ((entry sidebar-home))
  "The home channel is the root."
  (declare (ignore entry))
  0)

(defmethod sidebar-entry-level ((entry sidebar-workspace))
  "A workspace nests below the home channel."
  (declare (ignore entry))
  1)

(defmethod sidebar-entry-level ((entry sidebar-channel))
  "A channel nests below its workspace, deeper for each parent channel."
  (+ 2 (sidebar-channel-depth entry)))

(-> world-sidebar--current-span (sidebar-entry) cons)
(defun world-sidebar--current-span (entry)
  "Return the bar at the sidebar's edge that marks ENTRY as the channel this terminal shows, or a blank."
  (if (sidebar-entry-current-p entry)
      (terminal-span ':legend-accent "▌")
      (terminal-span ':plain " ")))

(-> world-sidebar--lead (sidebar-entry (integer 0)) list)
(defun world-sidebar--lead (entry level)
  "Return the spans that start ENTRY's first row at tree LEVEL: the edge bar, the indent and the tree marker."
  (list (world-sidebar--current-span entry)
        (terminal-span ':plain (make-string (* 2 level) :initial-element #\Space))
        (world-sidebar--marker-span entry)
        (terminal-span ':plain " ")))

(-> world-sidebar--marker-end ((integer 0)) (integer 1))
(defun world-sidebar--marker-end (level)
  "Return the last column where a click on a row at tree LEVEL opens or closes its node."
  (+ 3 (* 2 level)))

(-> world-sidebar--marker-span (sidebar-fold) cons)
(defun world-sidebar--marker-span (node)
  "Return NODE's tree marker: open, closed, or blank without nested entries."
  (cond
    ((not (sidebar-fold-children-p node))
     (terminal-span ':plain " "))
    ((sidebar-fold-open-p node)
     (terminal-span ':dim "▾"))
    (t
     (terminal-span ':dim "▸"))))

(-> world-sidebar--state-span (keyword boolean) cons)
(defun world-sidebar--state-span (state waiting-for-jobs-p)
  "Return STATE's glyph, yellow while an idle agent has unfinished jobs."
  (if (and waiting-for-jobs-p (member state '(:idle :working)))
      (terminal-span ':notice "● ")
      (case state
        ((:active :working :cancelling)
         (terminal-span ':agent-spinner "● "))
        (:idle
         (terminal-span ':success "○ "))
        (:unknown
         (terminal-span ':notice "? "))
        ((:paused :detaching :stopping :failed :starting)
         (terminal-span ':notice "○ "))
        (otherwise
         (terminal-span ':dim "· ")))))

(-> world-sidebar--badges (&rest (option cons)) list)
(defun world-sidebar--badges (&rest spans)
  "Return the SPANS that are any, a space between each two."
  (loop for (span . more) on (remove nil spans)
        collect span
        when more
          collect (terminal-span ':plain " ")))

(-> world-sidebar--count-span (terminal-style string integer) (option cons))
(defun world-sidebar--count-span (style control count)
  "Return COUNT formatted by CONTROL in STYLE, or NIL for zero."
  (when (plusp count)
    (terminal-span style (format nil control count))))

(defgeneric sidebar-entry-height (entry filter-p)
  (:documentation "Return how many rows ENTRY takes, in the tree or among the matches of a filter."))

(defmethod sidebar-entry-height ((entry sidebar-entry) filter-p)
  "The home channel and a workspace take one row."
  (declare (ignore filter-p))
  1)

(defmethod sidebar-entry-height ((entry sidebar-channel) filter-p)
  "A channel takes a second row for its title, or for its identifier and path while filtering."
  (if (or filter-p (and (sidebar-entry-name entry) (sidebar-channel-title entry)))
      2
      1))

(defgeneric sidebar-entry-rows (entry width &key filter-p)
  (:documentation "Return ENTRY's rows of styled spans for a sidebar WIDTH cells wide.

FILTER-P shows what a filter matched: a channel's identifier and workspace."))

(defmethod sidebar-entry-rows ((entry sidebar-home) width &key filter-p)
  "The home row shows how many messages wait unread, its own and those a closed tree hides."
  (declare (ignore filter-p))
  (list (world-sidebar--justify
         (append (world-sidebar--lead entry (sidebar-entry-level entry))
                 (list (terminal-span ':user "home")))
         (world-sidebar--badges
          (world-sidebar--count-span ':legend-accent "@~D" (sidebar-fold-hidden-mentions entry))
          (world-sidebar--count-span ':notice "~D unread" (+ (sidebar-entry-unread entry)
                                                             (sidebar-fold-hidden-unread entry)))
          (when (and (sidebar-fold-children-p entry) (not (sidebar-fold-open-p entry)))
            (terminal-span ':hint (format nil "~D nested" (sidebar-fold-nested entry)))))
         width)))

(defmethod sidebar-entry-rows ((entry sidebar-workspace) width &key filter-p)
  "A workspace row names the directory and counts its channels and what a closed one hides."
  (declare (ignore filter-p))
  (list (world-sidebar--justify
         (append (world-sidebar--lead entry (sidebar-entry-level entry))
                 (list (terminal-span ':sidebar-workspace (sidebar-entry-name entry)))
                 (when (sidebar-fold-open-p entry)
                   (list (terminal-span ':dim (format nil "  ~A" (sidebar-workspace-path entry))))))
         (world-sidebar--badges
          (world-sidebar--count-span ':legend-accent "@~D" (sidebar-fold-hidden-mentions entry))
          (world-sidebar--count-span ':notice "~D" (sidebar-fold-hidden-unread entry))
          (terminal-span ':dim (format nil "~D" (sidebar-fold-nested entry))))
         width)))

(defmethod sidebar-entry-rows ((entry sidebar-channel) width &key filter-p)
  "A channel shows its agent's state and name with what arrived, then its title.

A channel without an agent shows its title in the name's place. FILTER-P puts
the title beside the name and the identifier and workspace on the second row."
  (let* ((name  (sidebar-entry-name entry))
         (title (sidebar-channel-title entry))
         (level (if filter-p 0 (sidebar-entry-level entry)))
         (below (list (world-sidebar--current-span entry)
                      (terminal-span ':plain (make-string (+ 4 (* 2 level))
                                                          :initial-element #\Space))))
         (label (cond
                  (name
                   (terminal-span (cond
                                    ((sidebar-entry-current-p entry)
                                     ':legend-accent)
                                    ((sidebar-channel-attached-p entry)
                                     ':user)
                                    (t
                                     ':agent-name))
                                  name))
                  (title
                   (terminal-span ':strong title))
                  (t
                   (terminal-span ':hint "untitled")))))
    (cons
     (world-sidebar--justify
      (append (world-sidebar--lead entry level)
              (list (world-sidebar--state-span (sidebar-channel-state entry)
                                               (sidebar-channel-waiting-for-jobs-p entry))
                    label)
              (when (and filter-p name title)
                (list (terminal-span ':dim (format nil " ~A" title)))))
      (world-sidebar--badges
       (world-sidebar--count-span ':legend-accent "@~D" (+ (sidebar-channel-mentions entry)
                                                          (sidebar-fold-hidden-mentions entry)))
       (world-sidebar--count-span ':notice "~D" (+ (sidebar-entry-unread entry)
                                                  (sidebar-fold-hidden-unread entry)))
       (world-sidebar--count-span ':dim "~D pending" (sidebar-channel-pending entry))
       (when (and (sidebar-fold-children-p entry) (not (sidebar-fold-open-p entry)))
         (terminal-span ':hint (format nil "~D nested" (sidebar-fold-nested entry)))))
      width)
     (cond
       (filter-p
        (list (terminal--clip-spans
               (append below
                       (list (terminal-span ':code (conversation-identifier-display
                                                    (sidebar-entry-identifier entry)))
                             (terminal-span ':dim (format nil "  ~A" (sidebar-channel-path entry)))))
               width)))
       ((and name title)
        (list (terminal--clip-spans
               (append below (list (terminal-span ':dim title)))
               width)))))))


;;;; -- Viewport --

(-> world-sidebar--filter-p (world-sidebar) boolean)
(defun world-sidebar--filter-p (sidebar)
  "Return whether SIDEBAR shows the matches of a filter instead of the tree."
  (plusp (length (world-sidebar-filter sidebar))))

(-> world-sidebar--entry-line (world-sidebar integer) (integer 0))
(defun world-sidebar--entry-line (sidebar index)
  "Return the tree line where SIDEBAR's entry INDEX starts, or the line count for the entry count."
  (loop with filter-p = (world-sidebar--filter-p sidebar)
        for entry in (world-sidebar-entries sidebar)
        repeat index
        sum (sidebar-entry-height entry filter-p)))

(-> world-sidebar--reveal-selection (world-sidebar) null)
(defun world-sidebar--reveal-selection (sidebar)
  "Clamp SIDEBAR's selection and scroll its viewport so every row of the selected entry shows."
  (let* ((entries  (world-sidebar-entries sidebar))
         (height   (world-sidebar-viewport-height sidebar))
         (selected (min (world-sidebar-selected sidebar) (max 0 (1- (length entries)))))
         (lines    (world-sidebar--entry-line sidebar (length entries)))
         (offset   (min (world-sidebar-scroll-offset sidebar) (max 0 (- lines height)))))
    (when (and entries (plusp height))
      (let* ((start (world-sidebar--entry-line sidebar selected))
             (end   (+ start (sidebar-entry-height (nth selected entries)
                                                   (world-sidebar--filter-p sidebar)))))
        (setf offset (cond
                       ((< start offset)
                        start)
                       ((> end (+ offset height))
                        (- end height))
                       (t
                        offset)))))
    (setf (world-sidebar-selected sidebar) selected
          (world-sidebar-scroll-offset sidebar) offset))
  nil)

(-> world-sidebar--tree-rows (world-sidebar integer &key (:width integer) (:focused-p boolean))
    (values list list))
(defun world-sidebar--tree-rows (sidebar limit &key width focused-p)
  "Return LIMIT rows of SIDEBAR's entries from its scroll offset, and the entry index of each.

Only the entries in view are laid out. The selected entry is highlighted while
the sidebar is FOCUSED-P."
  (let ((filter-p (world-sidebar--filter-p sidebar))
        (offset   (world-sidebar-scroll-offset sidebar))
        (line     0)
        (rows     nil)
        (indices  nil))
    (loop for entry in (world-sidebar-entries sidebar)
          for index from 0
          for height = (sidebar-entry-height entry filter-p)
          while (< line (+ offset limit))
          do (when (> (+ line height) offset)
               (loop for spans in (sidebar-entry-rows entry width :filter-p filter-p)
                     for row-line from line
                     when (and (>= row-line offset) (< row-line (+ offset limit)))
                       do (push (if (and focused-p (= index (world-sidebar-selected sidebar)))
                                    (list (terminal-span ':selected
                                                         (terminal-ui--raw-spans-text
                                                          (terminal--pad-spans spans width))))
                                    spans)
                                rows)
                          (push index indices)))
             (incf line height))
    (when (and filter-p (null rows) (plusp limit))
      (push (list (terminal-span ':hint " no channel matches")) rows)
      (push nil indices))
    (let ((filler (make-list (- limit (length rows)))))
      (values (nconc (nreverse rows) filler)
              (nconc (nreverse indices) (copy-list filler))))))

(-> world-sidebar--buttons () list)
(defun world-sidebar--buttons ()
  "Return the footer's command buttons with the columns each takes."
  (loop for (label command) in '(("(new)" :new) ("(delete)" :delete))
        for column = 2 then (+ column (text-cell-width previous) 2)
        for previous = label
        collect (list :label label :command command
                      :start column :end (+ column (text-cell-width label) -1))))

(-> world-sidebar--heading (world-sidebar boolean) list)
(defun world-sidebar--heading (sidebar focused-p)
  "Return the heading row and the filter row under it.

The heading names the channel this terminal shows by its identifier, the one
place the sidebar spells one out. The second row shows the filter, or says
when the last gather could not reach the server."
  (let ((current (world-sidebar-current sidebar))
        (filter  (world-sidebar-filter sidebar)))
    (list (append (list (terminal-span (if focused-p ':selected ':brand) " CHANNELS "))
                  (when current
                    (list (terminal-span ':code (format nil " ~A" (conversation-identifier-display
                                                                    current))))))
          (cond
            ((plusp (length filter))
             (list (terminal-span ':legend-accent " / ")
                   (terminal-span ':legend-plain filter)))
            ((getf (world-sidebar-inputs sidebar) :runtime-unknown-p)
             (list (terminal-span ':notice " ! live status unknown")))
            (focused-p
             (list (terminal-span ':hint " type to filter")))))))

(-> world-sidebar--legend (boolean) list)
(defun world-sidebar--legend (focused-p)
  "Return the key hints for a focused sidebar, or how to reach it."
  (if focused-p
      (list (terminal-span ':legend-accent " Del")
            (terminal-span ':legend-dim " delete  ")
            (terminal-span ':legend-accent "Esc")
            (terminal-span ':legend-dim " back"))
      (list (terminal-span ':legend-accent " Ctrl-Space")
            (terminal-span ':legend-dim " focus  ")
            (terminal-span ':legend-accent "Ctrl-S")
            (terminal-span ':legend-dim " hide"))))

(-> world-sidebar--render-state
    (world-sidebar terminal &key (:height integer) (:width integer) (:focused-p boolean))
    list)
(defun world-sidebar--render-state (sidebar terminal &key height width focused-p)
  "Return everything SIDEBAR's rendered rows depend on, to tell when earlier rows still hold."
  (list height width focused-p
        (terminal-styled-p terminal)
        *terminal-theme*
        (world-sidebar-entries sidebar)
        (world-sidebar-selected sidebar)
        (world-sidebar-scroll-offset sidebar)
        (world-sidebar-filter sidebar)
        (world-sidebar-current sidebar)
        (getf (world-sidebar-inputs sidebar) :runtime-unknown-p)))

(defmethod screen-panel-rows ((panel world-sidebar) terminal &key height width focused-p)
  "Render the heading, the scrolling tree, the command buttons and the key hints.

The screen asks with every frame it paints, so rows are rendered only when
something they show changed."
  (let ((rendered (world-sidebar-rendered panel)))
    (if (and rendered
             (equal (first rendered)
                    (world-sidebar--render-state panel terminal
                                                 :height height :width width :focused-p focused-p)))
        (rest rendered)
        (let ((rows (world-sidebar--render panel terminal
                                           :height height :width width :focused-p focused-p)))
          ;; Rendering settles the selection and the scroll offset, so the state is read after it.
          (setf (world-sidebar-rendered panel)
                (cons (world-sidebar--render-state panel terminal
                                                   :height height :width width :focused-p focused-p)
                      rows))
          rows))))

(-> world-sidebar--render
    (world-sidebar terminal &key (:height integer) (:width integer) (:focused-p boolean))
    list)
(defun world-sidebar--render (panel terminal &key height width focused-p)
  "Return PANEL's rows for TERMINAL and record which entry or button each row acts on."
  (let* ((footer-count  (min 1 height))
         (legend-count  (if (> height 1) 1 0))
         (heading-count (min 2 (max 0 (- height footer-count legend-count))))
         (limit         (max 0 (- height heading-count footer-count legend-count)))
         (buttons       (world-sidebar--buttons)))
    (setf (world-sidebar-viewport-height panel) limit)
    (world-sidebar--reveal-selection panel)
    (multiple-value-bind (rows indices)
        (world-sidebar--tree-rows panel limit :width width :focused-p focused-p)
      (setf (world-sidebar-actions panel)
            (coerce (append (make-list heading-count)
                            (mapcar (lambda (index) (and index (list :entry index))) indices)
                            (when (plusp footer-count) (list (list :buttons buttons)))
                            (make-list legend-count))
                    'vector))
      (loop for spans in (append (subseq (world-sidebar--heading panel focused-p) 0 heading-count)
                                 rows
                                 (when (plusp footer-count)
                                   (list (loop for button in buttons
                                               for first-p = t then nil
                                               append (list (terminal-span ':plain (if first-p " " "  "))
                                                            (terminal-span ':sidebar-action
                                                                           (getf button :label))))))
                                 (when (plusp legend-count)
                                   (list (world-sidebar--legend focused-p))))
            collect (terminal--render-spans terminal (terminal--pad-spans spans width))))))


;;;; -- Keys and Clicks --

(-> world-sidebar--open (world-sidebar string) t)
(defun world-sidebar--open (sidebar identifier)
  "Return the request that shows channel IDENTIFIER, dropping the filter that found it."
  (when (world-sidebar--filter-p sidebar)
    (world-sidebar-set-filter sidebar ""))
  (if (equal identifier (world-sidebar-current sidebar))
      ':dismiss
      (list ':switch :session-id identifier)))

(defgeneric sidebar-entry-activate (entry sidebar column)
  (:documentation "Act on ENTRY of SIDEBAR and return the panel's result.

COLUMN is where a click landed, or NIL for Enter."))

(defmethod sidebar-entry-activate ((entry sidebar-entry) sidebar column)
  "A channel row, home included, opens its channel; its tree marker opens or closes what nests below."
  (if (and column
           (sidebar-fold-children-p entry)
           (not (world-sidebar--filter-p sidebar))
           (<= column (world-sidebar--marker-end (sidebar-entry-level entry))))
      (progn
        (world-sidebar--toggle sidebar entry)
        ':redraw)
      (world-sidebar--open sidebar (sidebar-entry-identifier entry))))

(defmethod sidebar-entry-activate ((entry sidebar-workspace) sidebar column)
  "A workspace row opens or closes its group."
  (declare (ignore column))
  (world-sidebar--toggle sidebar entry)
  ':redraw)

(defmethod screen-panel-click ((panel world-sidebar) column row)
  "Press the command button under COLUMN, or select and activate the entry on ROW."
  (let* ((actions (world-sidebar-actions panel))
         (action  (and (<= 1 row (length actions)) (aref actions (1- row)))))
    (case (first action)
      (:buttons
       (let ((button (find-if (lambda (button)
                                (<= (getf button :start) column (getf button :end)))
                              (second action))))
         (and button (list ':channel-command (getf button :command)))))
      (:entry
       (setf (world-sidebar-selected panel) (second action))
       (sidebar-entry-activate (nth (second action) (world-sidebar-entries panel)) panel column))
      (otherwise
       ':redraw))))

(-> world-sidebar--move (world-sidebar keyword) null)
(defun world-sidebar--move (sidebar event)
  "Move SIDEBAR's selection as the navigation EVENT says, wrapping at the ends for single steps."
  (let* ((count   (length (world-sidebar-entries sidebar)))
         (current (world-sidebar-selected sidebar))
         (page    (max 1 (floor (world-sidebar-viewport-height sidebar) 2))))
    (when (plusp count)
      (setf (world-sidebar-selected sidebar)
            (ecase event
              (:up
               (mod (1- current) count))
              (:down
               (mod (1+ current) count))
              (:page-up
               (max 0 (- current page)))
              (:page-down
               (min (1- count) (+ current page)))
              (:home
               0)
              (:end
               (1- count))))))
  nil)

(-> world-sidebar--scroll (world-sidebar integer) null)
(defun world-sidebar--scroll (sidebar lines)
  "Scroll SIDEBAR's tree LINES down, moving the selection to an entry that still shows."
  (let* ((height   (world-sidebar-viewport-height sidebar))
         (entries  (world-sidebar-entries sidebar))
         (filter-p (world-sidebar--filter-p sidebar))
         (total    (world-sidebar--entry-line sidebar (length entries)))
         (offset   (max 0 (min (max 0 (- total height))
                               (+ (world-sidebar-scroll-offset sidebar) lines))))
         (visible  (loop for entry in entries
                         for index from 0
                         for line = 0 then end
                         for end = (+ line (sidebar-entry-height entry filter-p))
                         when (and (>= line offset) (<= end (+ offset height)))
                           collect index)))
    (setf (world-sidebar-scroll-offset sidebar) offset)
    (when visible
      (setf (world-sidebar-selected sidebar)
            (max (first visible)
                 (min (world-sidebar-selected sidebar) (first (last visible)))))))
  nil)

(defmethod screen-panel-event ((panel world-sidebar) event)
  "Navigate, fold, filter and activate the entries with the keyboard and the wheel."
  (let* ((entries  (world-sidebar-entries panel))
         (selected (nth (world-sidebar-selected panel) entries))
         (filter   (world-sidebar-filter panel)))
    (cond
      ((typep event '(cons (eql :scroll) (cons integer null)))
       (world-sidebar--scroll panel (* (second event) *world-sidebar-scroll-lines*))
       ':redraw)
      ((typep event '(cons (member :insert :paste) (cons string null)))
       (world-sidebar-set-filter
        panel (concatenate 'string filter (sanitize-text (second event) :single-line-p t)))
       ':redraw)
      (t
       (case event
         ((:up :down :page-up :page-down :home :end)
          (world-sidebar--move panel event)
          ':redraw)
         ((:scroll-top :scroll-bottom)
          (world-sidebar--move panel (if (eq event ':scroll-top) ':home ':end))
          ':redraw)
         ((:left :right)
          (when (and (typep selected 'sidebar-fold)
                     (sidebar-fold-children-p selected)
                     (not (eq (sidebar-fold-open-p selected) (eq event ':right))))
            (world-sidebar--toggle panel selected))
          ':redraw)
         (:backspace
          (when (plusp (length filter))
            (world-sidebar-set-filter panel (subseq filter 0 (1- (length filter)))))
          ':redraw)
         (:kill-line
          (world-sidebar-set-filter panel "")
          ':redraw)
         (:escape
          (if (plusp (length filter))
              (progn
                (world-sidebar-set-filter panel "")
                ':redraw)
              ':dismiss))
         (:submit
          (and selected
               (sidebar-entry-activate selected panel nil)))
         (:delete
          (and (typep selected 'sidebar-channel)
               (list ':channel-command ':delete (sidebar-entry-identifier selected))))
         (otherwise
          nil))))))
