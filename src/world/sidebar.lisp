(in-package #:autolith)

;;;; -- World Sidebar --

;;; The side panel a terminal client shows beside its channel: the person's
;;; inbox first, with the messages waiting there unread, then every other
;;; channel of the world grouped by workspace, with what runs there, who is
;;; attached, and what arrived since this client last looked. Channel unread
;;; and mention baselines are client-local and start at the current counts.
;;; A started sidebar gathers on its own thread, so reading the world, the
;;; server and the inboxes never holds up the terminal's keystrokes; the
;;; panel only takes what the thread gathered last.

(defparameter *world-sidebar-share* 22/100
  "The share of the terminal's columns the sidebar takes left of the channel.")

(defparameter *world-sidebar-minimum-width* 24
  "The fewest columns the sidebar takes beside the channel.")

(defparameter *world-sidebar-minimum-columns* 90
  "The minimum transcript width beside the sidebar; below it Ctrl-T opens a fullscreen drawer.")

(defparameter *world-sidebar-marker-columns* 3
  "The columns at the start of a sidebar row where a click opens or closes the row's tree node.")

(defparameter *world-sidebar-refresh-seconds* 1
  "How often the sidebar asks the world and the server what changed.")

(defclass world-sidebar ()
  ((configuration
    :initarg       :configuration
    :reader        world-sidebar-configuration
    :type          configuration
    :documentation "The configuration whose world and server the sidebar shows.")
   (current
    :initform      nil
    :accessor      world-sidebar-current
    :type          (option string)
    :documentation "The channel this terminal is attached to, or NIL.")
   (shown-p
    :initform      t
    :accessor      world-sidebar-shown-p
    :type          boolean
    :documentation "Whether the person wants the sidebar beside the session, kept across the terminal's attachments.")
   (entries
    :initform      nil
    :accessor      world-sidebar-entries
    :type          list
    :documentation "The inbox, workspace and channel entries shown, built from INPUTS.")
   (inputs
    :initform      nil
    :accessor      world-sidebar-inputs
    :type          list
    :documentation "The world as last gathered, the keyword arguments of `world-sidebar--entries', so a click rebuilds the entries at once.")
   (toggled
    :initform      (make-hash-table :test #'equal)
    :reader        world-sidebar-toggled
    :type          hash-table
    :documentation "The tree nodes the person opened or closed, as :OPEN or :CLOSED, workspaces by path and channels by identifier.")
   (pathnames
    :initform      (make-hash-table :test #'equal)
    :reader        world-sidebar-pathnames
    :type          hash-table
    :documentation "Conversation identity pathnames by channel, found once: finding a child's searches every task directory.")
   (actions
    :initform      #()
    :accessor      world-sidebar-actions
    :type          vector
    :documentation "What a click on each row of the last rendering does, as plists of :OPEN and :TOGGLE, or NIL.")
   (selected-row
    :initform      0
    :accessor      world-sidebar-selected-row
    :type          (integer 0)
    :documentation "The zero-based selected action row for keyboard navigation in the drawer.")
   (seen
    :initform      (make-hash-table :test #'equal)
    :reader        world-sidebar-seen
    :type          hash-table
    :documentation "Message counts by channel as of this client's last look, the unread baselines.")
   (mentions-seen
    :initform      (make-hash-table :test #'equal)
    :reader        world-sidebar-mentions-seen
    :type          hash-table
    :documentation "Mention counts by channel as of this client's last look.")
   (refreshed-at
    :initform      nil
    :accessor      world-sidebar-refreshed-at
    :type          (option real)
    :documentation "The internal real time of the last refresh, or NIL before the first.")
   (server-entry
    :initform      nil
    :accessor      world-sidebar-server-entry
    :type          (option cons)
    :documentation "The world server's endpoint entry while it answers, or NIL.")
   (clock-function
    :initarg       :clock-function
    :initform      #'get-internal-real-time
    :reader        world-sidebar-clock-function
    :type          function
    :documentation "Returns the internal real time, replaceable by tests.")
   (lock
    :initform      (make-lock "Autolith world sidebar")
    :reader        world-sidebar-lock
    :type          t
    :documentation "Protects the gathered entries and the gatherer's stop request.")
   (condition-variable
    :initform      (make-condition-variable :name "Autolith world sidebar")
    :reader        world-sidebar-condition-variable
    :type          t
    :documentation "Wakes the gatherer early when it is asked to stop.")
   (gatherer
    :initform      nil
    :accessor      world-sidebar-gatherer
    :type          t
    :documentation "The thread gathering entries, or NIL when the panel gathers as it refreshes.")
   (gathered
    :initform      nil
    :accessor      world-sidebar-gathered
    :type          list
    :documentation "(INPUTS) as the gatherer last produced them, or NIL when nothing new waits.")
   (stopping-p
    :initform      nil
    :accessor      world-sidebar-stopping-p
    :type          boolean
    :documentation "Whether the gatherer was asked to stop."))
  (:documentation "A client's view of the world's channels beside its own."))

(-> world-sidebar-create (configuration &key (:clock-function function)) world-sidebar)
(defun world-sidebar-create (configuration &key (clock-function #'get-internal-real-time))
  "Return a sidebar of CONFIGURATION's world, timed by CLOCK-FUNCTION."
  (make-instance 'world-sidebar :configuration configuration :clock-function clock-function))


;;;; -- Panel Protocol --

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
        (and gathered
             (world-sidebar--take panel (first gathered))))
      (let ((now  (funcall (world-sidebar-clock-function panel)))
            (last (world-sidebar-refreshed-at panel)))
        (if (and last
                 (< (- now last) (* *world-sidebar-refresh-seconds* internal-time-units-per-second)))
            nil
            (progn
              (setf (world-sidebar-refreshed-at panel) now)
              (world-sidebar--take panel (world-sidebar--gather panel)))))))

(defmethod screen-panel-rows ((panel world-sidebar) terminal &key height width)
  "Render the sidebar's entries as HEIGHT rows WIDTH cells wide for TERMINAL, remembering what a click on each does."
  (multiple-value-bind (rows actions)
      (world-sidebar--rows (world-sidebar-entries panel) height
                           :drawer-p (= width (terminal-columns terminal)))
    (setf (world-sidebar-actions panel) (coerce actions 'vector))
    (unless (nth (world-sidebar-selected-row panel) actions)
      (setf (world-sidebar-selected-row panel) (or (position-if #'identity actions) 0)))
    (loop for spans in rows
          for index from 0
          collect (let ((padded (terminal--pad-spans spans width)))
                    (terminal--render-spans
                     terminal
                     (if (and (= width (terminal-columns terminal))
                              (= index (world-sidebar-selected-row panel)))
                         (list (terminal-span ':selected (terminal-ui--raw-spans-text padded)))
                         padded))))))

(defmethod screen-panel-click ((panel world-sidebar) column row)
  "Open or close the tree node at ROW, or ask to switch to its channel.

The first columns of a channel row hold its marker: a click there opens or
closes the channel's nested channels. Anywhere else the click switches to
the channel, unless the terminal shows it already."
  (let* ((actions (world-sidebar-actions panel))
         (action  (and (<= 1 row (length actions)) (aref actions (1- row))))
         (open    (getf action :open))
         (toggle  (getf action :toggle)))
    (cond
      ((getf action :picker-p)
       ':channel-picker)
      ((and toggle (or (null open) (<= column *world-sidebar-marker-columns*)))
       (world-sidebar--toggle panel toggle)
       ':redraw)
      (open
       (if (equal open (world-sidebar-current panel))
           ':dismiss
           (list ':switch :session-id open)))
      (t
       nil))))

(defmethod screen-panel-event ((panel world-sidebar) event)
  "Navigate the drawer's actionable rows; Enter opens and arrows expand trees."
  (let* ((actions (world-sidebar-actions panel))
         (rows    (loop for action across actions
                        for index from 0 when action collect index))
         (current (world-sidebar-selected-row panel)))
    (case event
      ((:up :down)
       (when rows
         (setf (world-sidebar-selected-row panel)
               (nth (mod (+ (or (position current rows) 0) (if (eq event ':up) -1 1))
                         (length rows))
                    rows)))
       ':redraw)
      (:submit
       (screen-panel-click panel (1+ *world-sidebar-marker-columns*) (1+ current)))
      ((:left :right)
       (let* ((action (and (< current (length actions)) (aref actions current)))
              (key    (getf action :toggle))
              (entry  (and key (find key (world-sidebar-entries panel)
                                      :key  (lambda (entry) (or (getf entry :key) (getf entry :id)))
                                      :test #'equal))))
         (when (and entry (not (eq (not (null (getf entry :open-p))) (eq event ':right))))
           (world-sidebar--toggle panel key))
         ':redraw))
      (otherwise
       nil))))


;;;; -- Gathering --

(-> world-sidebar-start (world-sidebar) null)
(defun world-sidebar-start (sidebar)
  "Gather SIDEBAR's entries on a thread of its own from now on, until `world-sidebar-stop'."
  (unless (world-sidebar-gatherer sidebar)
    (setf (world-sidebar-stopping-p sidebar) nil
          (world-sidebar-gatherer sidebar)
          (make-thread (lambda () (world-sidebar--gather-loop sidebar))
                       :name "Autolith world sidebar")))
  nil)

(-> world-sidebar-stop (world-sidebar) null)
(defun world-sidebar-stop (sidebar)
  "Stop SIDEBAR's gathering thread and wait for it; the panel gathers itself again afterwards."
  (let ((thread (world-sidebar-gatherer sidebar)))
    (when thread
      (with-lock-held ((world-sidebar-lock sidebar))
        (setf (world-sidebar-stopping-p sidebar) t)
        (sb-thread:condition-broadcast (world-sidebar-condition-variable sidebar)))
      (sb-thread:join-thread thread :default nil)
      (setf (world-sidebar-gatherer sidebar) nil)))
  nil)

(-> world-sidebar--gather-loop (world-sidebar) null)
(defun world-sidebar--gather-loop (sidebar)
  "Gather SIDEBAR's entries every refresh interval until it is asked to stop.

A failed gather keeps the entries shown before; the next one tries again."
  (loop
    (let ((inputs (handler-case
                      (world-sidebar--gather sidebar)
                    (error ()
                      nil))))
      (with-lock-held ((world-sidebar-lock sidebar))
        (when inputs
          (setf (world-sidebar-gathered sidebar) (list inputs)))
        (when (world-sidebar-stopping-p sidebar)
          (return))
        (condition-wait (world-sidebar-condition-variable sidebar)
                        (world-sidebar-lock sidebar)
                        :timeout *world-sidebar-refresh-seconds*)
        (when (world-sidebar-stopping-p sidebar)
          (return)))))
  nil)

(-> world-sidebar--take (world-sidebar list) boolean)
(defun world-sidebar--take (sidebar inputs)
  "Show the entries of the world INPUTS describe in SIDEBAR and return whether they differ from those shown before."
  (let ((entries (apply #'world-sidebar--entries sidebar inputs)))
    (setf (world-sidebar-inputs sidebar) inputs)
    (prog1 (not (equal entries (world-sidebar-entries sidebar)))
      (setf (world-sidebar-entries sidebar) entries))))

(-> world-sidebar--toggle (world-sidebar string) null)
(defun world-sidebar--toggle (sidebar key)
  "Open the tree node KEY names in SIDEBAR when it shows closed, or close it, and rebuild the entries.

The choice stays when the terminal moves to another channel."
  (let ((entry (find key (world-sidebar-entries sidebar)
                     :key  (lambda (entry) (or (getf entry :key) (getf entry :id)))
                     :test #'equal)))
    (setf (gethash key (world-sidebar-toggled sidebar))
          (if (getf entry :open-p) ':closed ':open)))
  (world-sidebar--take sidebar (world-sidebar-inputs sidebar))
  nil)

(-> world-sidebar--gather (world-sidebar) list)
(defun world-sidebar--gather (sidebar)
  "Return the world as it is now, as the keyword arguments of `world-sidebar--entries'."
  (let* ((configuration (world-sidebar-configuration sidebar))
         (statuses      (world-sidebar--statuses sidebar))
         (world         (world-presented configuration)))
    (list :inbox    (world-inbox-channel world)
          :channels (world-sidebar--channels sidebar world statuses)
          :names    (world-sidebar--names world)
          :statuses statuses
          :pending  (world-sidebar--pending configuration))))

(-> world-sidebar--names (world) list)
(defun world-sidebar--names (world)
  "Return (CHANNEL-ID . NAME) pairs naming the agent that resides in each of WORLD's channels."
  (loop for participant in (world-participants world)
        when (and (eq (participant-kind participant) ':agent)
                  (participant-residence participant))
          collect (cons (participant-residence participant) (participant-name participant))))

(-> world-sidebar--channels (world-sidebar world list) list)
(defun world-sidebar--channels (sidebar world statuses)
  "Return WORLD's channels but the inbox, and the hosted ones STATUSES report beyond them, the latest touched first.

Latest touched is the newest write of a channel's conversation file. A hosted
channel the world does not know yet has no file: it started just now, shows
first, and belongs to the workspace its status names."
  (let* ((now    (get-universal-time))
         (person (world-human-participant world))
         (fresh  (loop for status in statuses
                       for identifier = (getf status :session-id)
                       for directory = (getf status :cwd)
                       when (and (stringp identifier)
                                 (stringp directory)
                                 (null (world-channel-find world identifier)))
                         collect (make-instance 'channel
                                                :identifier identifier
                                                :owner      (if person
                                                                (participant-identifier person)
                                                                identifier)
                                                :workspace  (world-workspace-name directory)
                                                :title      (getf status :conversation-title)
                                                :created-at now
                                                :updated-at now))))
    (mapcar #'rest
            (stable-sort
             (append (mapcar (lambda (channel) (cons now channel)) fresh)
                     (mapcar (lambda (channel)
                               (cons (conversation-storage-write-date
                                      (world-sidebar--pathname sidebar (channel-identifier channel)))
                                     channel))
                             (remove-if #'channel-inbox-p (world-channels world))))
             #'> :key #'first))))

(-> world-sidebar--pathname (world-sidebar string) pathname)
(defun world-sidebar--pathname (sidebar identifier)
  "Return the identity pathname of channel IDENTIFIER's conversation, found once per sidebar."
  (let ((pathnames (world-sidebar-pathnames sidebar)))
    (or (gethash identifier pathnames)
        (setf (gethash identifier pathnames)
              (conversation-pathname-for-id (world-sidebar-configuration sidebar) identifier)))))

(-> world-sidebar--statuses (world-sidebar) list)
(defun world-sidebar--statuses (sidebar)
  "Return the statuses of the channels the world server hosts, or NIL without a server."
  (let* ((configuration (world-sidebar-configuration sidebar))
         (entry         (or (world-sidebar-server-entry sidebar)
                            (setf (world-sidebar-server-entry sidebar)
                                  (world-client-entry configuration)))))
    (and entry
         (handler-case
             (mapcar #'rest (getf (rest (localgroup-query-record entry ':channels)) :channels))
           (autolith-error ()
             (setf (world-sidebar-server-entry sidebar) nil)
             nil)))))

(-> world-sidebar--pending (configuration) list)
(defun world-sidebar--pending (configuration)
  "Return (CHANNEL-ID . COUNT) pairs for the channels with deliveries waiting in their inbox."
  (let ((directory (merge-pathnames "world/inbox/" (config :data-root configuration))))
    (if (uiop:directory-exists-p directory)
        (loop for pathname in (uiop:directory-files directory "*.sexp")
              for identifier = (pathname-name pathname)
              for count = (handler-case
                              (length (world-inbox-deliveries configuration identifier))
                            (autolith-error ()
                              0))
              when (plusp count)
                collect (cons identifier count))
        nil)))

(-> world-sidebar--entries
    (world-sidebar &key (:inbox (option channel)) (:channels list) (:names list)
                        (:statuses list) (:pending list))
    list)
(defun world-sidebar--entries (sidebar &key inbox channels names statuses pending)
  "Return the INBOX entry, then the workspace and channel entries of CHANNELS that are open to view.

NAMES pairs channels with their agents' names; STATUSES and PENDING
deliveries are folded in. The inbox's unread count is what waits in it for
the person, so it stays true across clients and restarts. CHANNELS arrive
latest touched first. The workspace of the current channel comes first, then
the others in that order. Within a workspace, channels nest under their
parents. The first workspace is open, and a channel when the current one
nests below it, unless the person opened or closed them; a closed one counts
what it hides."
  (let ((current (world-sidebar-current sidebar))
        (table   (make-hash-table :test #'equal)))
    (dolist (status statuses)
      (let ((identifier (getf status :session-id)))
        (when (stringp identifier)
          (setf (gethash identifier table) status))))
    (labels ((channel-entries (node depth)
               "Return the shown entries of NODE at DEPTH, and the entries of all its descendants."
               (let* ((channel    (first node))
                      (identifier (channel-identifier channel))
                      (entry      (world-sidebar--channel-entry
                                   sidebar channel
                                   :depth   depth
                                   :name    (rest (assoc identifier names :test #'string=))
                                   :status  (gethash identifier table)
                                   :pending (rest (assoc identifier pending :test #'string=))))
                      (shown      nil)
                      (all        nil))
                 (dolist (child (rest node))
                   (multiple-value-bind (child-shown child-all) (channel-entries child (1+ depth))
                     (setf shown (append shown child-shown)
                           all   (append all child-all))))
                 (if (null (rest node))
                     (values (list entry) (list entry))
                     (let ((open-p (world-sidebar--open-p
                                    sidebar identifier
                                    (and (find current all :key  (lambda (entry) (getf entry :id))
                                                           :test #'equal)
                                         t))))
                       (values (cons (append entry (world-sidebar--fold all :open-p open-p))
                                     (and open-p shown))
                               (cons entry all)))))))
      (append
       (when inbox
         (list (list :kind      ':inbox
                     :id        (channel-identifier inbox)
                     :current-p (equal (channel-identifier inbox) current)
                     :unread    (or (rest (assoc (channel-identifier inbox) pending :test #'string=))
                                    0))))
       (loop for (workspace . members) in (world-channel-groups channels current)
             for first-p = t then nil
             append (let ((shown nil)
                          (all   nil))
                      (dolist (node (world-channel-forest members))
                        (multiple-value-bind (node-shown node-all) (channel-entries node 0)
                          (setf shown (append shown node-shown)
                                all   (append all node-all))))
                      (let ((open-p (world-sidebar--open-p sidebar workspace first-p)))
                        (cons (append (list :kind ':workspace
                                            :key  workspace
                                            :name (world-workspace-leaf workspace)
                                            :path (world-sidebar--short-path workspace))
                                      (world-sidebar--fold all :open-p open-p))
                              (and open-p shown)))))))))

(-> world-sidebar--open-p (world-sidebar string boolean) boolean)
(defun world-sidebar--open-p (sidebar key default-p)
  "Return whether the tree node KEY is open: as the person last set it, or else DEFAULT-P."
  (ecase (gethash key (world-sidebar-toggled sidebar) ':default)
    (:open
     t)
    (:closed
     nil)
    (:default
     default-p)))

(-> world-sidebar--fold (list &key (:open-p boolean)) list)
(defun world-sidebar--fold (entries &key open-p)
  "Return the properties of a tree node over ENTRIES: whether it is open, and when closed what it hides.

A closed node carries the count of ENTRIES and their unread messages and
mentions, so nothing new below it goes unseen."
  (if open-p
      (list :children-p t :open-p t)
      (list :children-p t :open-p nil
            :hidden          (length entries)
            :hidden-unread   (reduce #'+ entries :key (lambda (entry) (getf entry :unread 0)))
            :hidden-mentions (reduce #'+ entries :key (lambda (entry) (getf entry :mentions 0))))))

(-> world-sidebar--channel-entry
    (world-sidebar channel &key (:depth (integer 0)) (:name (option string)) (:status (option list))
                           (:pending (option integer)))
    list)
(defun world-sidebar--channel-entry (sidebar channel &key (depth 0) name status pending)
  "Return the entry of CHANNEL at DEPTH, whose agent is NAME, with its hosted STATUS and PENDING deliveries."
  (let* ((identifier (channel-identifier channel))
         (current-p  (equal identifier (world-sidebar-current sidebar))))
    (list :kind       ':channel
          :id         identifier
          :name       name
          :title      (channel-title channel)
          :depth      depth
          :state      (if status (getf status :state) ':absent)
          :attached-p (and status (getf status :terminal-attached-p) t)
          :current-p  current-p
          :unread     (world-sidebar--unread (world-sidebar-seen sidebar) identifier
                                             :count       (and status (getf status :message-count))
                                             :caught-up-p current-p)
          :mentions   (world-sidebar--unread (world-sidebar-mentions-seen sidebar) identifier
                                             :count       (and status (getf status :mention-count))
                                             :caught-up-p current-p)
          :pending    (or pending 0))))

(-> world-sidebar--unread
    (hash-table string &key (:count (option integer)) (:caught-up-p boolean))
    (integer 0))
(defun world-sidebar--unread (baselines identifier &key count caught-up-p)
  "Return how far COUNT of channel IDENTIFIER rose above its baseline in BASELINES.

A channel seen for the first time starts caught up, and so does one that is
CAUGHT-UP-P because this client looks at it. Without a COUNT nothing is known."
  (cond
    ((null count)
     0)
    (t
     (when (or caught-up-p (null (gethash identifier baselines)))
       (setf (gethash identifier baselines) count))
     (max 0 (- count (gethash identifier baselines))))))

(-> world-sidebar--short-path (string) string)
(defun world-sidebar--short-path (workspace)
  "Return WORKSPACE without its trailing slash, with the home directory as a tilde."
  (let* ((home    (namestring (user-homedir-pathname)))
         (trimmed (string-right-trim "/" workspace)))
    (if (and (> (length home) 1)
             (>= (length workspace) (length home))
             (string= home workspace :end2 (length home)))
        (concatenate 'string "~/" (string-right-trim "/" (subseq workspace (length home))))
        trimmed)))


;;;; -- Rows --

(-> world-sidebar--rows (list (integer 0) &key (:drawer-p boolean)) (values list list))
(defun world-sidebar--rows (entries height &key drawer-p)
  "Return HEIGHT rows of styled spans showing ENTRIES under a heading, with the legend last, and what a click on each row does.

Entries past the rows available are counted on the last row they would take.
Each action is NIL or a plist of :OPEN, the channel a click switches to, and
:TOGGLE, the tree node a click on the marker opens or closes."
  (let* ((limit      (max 0 (- height 2)))
         (overflow-p (> (length entries) limit))
         (kept       (if overflow-p (max 0 (1- limit)) (length entries)))
         (shown      (subseq entries 0 kept))
         (rows       (append (list (list (terminal-span ':brand " Channels")))
                             (mapcar #'world-sidebar--entry-spans shown)
                             (when overflow-p
                              (list (list (terminal-span ':notice (format nil " … ~D more · open picker" (- (length entries) kept))))))))
         (actions    (append (list nil)
                             (mapcar #'world-sidebar--entry-action shown)
                             (when overflow-p
                              (list (list :picker-p t)))))
         (filler     (max 0 (- height 1 (length rows))))
         (legend     (if drawer-p
                         (list (terminal-span ':legend-dim
                                              " ↑↓ select · Enter opens · ←→ tree · Esc closes"))
                         (list (terminal-span ':legend-accent " Ctrl-T")
                               (terminal-span ':legend-dim " toggle sidebar")))))
    (values (subseq (append rows (make-list filler :initial-element nil) (list legend)) 0 height)
            (subseq (append actions (make-list filler :initial-element nil) (list nil)) 0 height))))

(-> world-sidebar--entry-action (list) (option list))
(defun world-sidebar--entry-action (entry)
  "Return what a click on ENTRY's row does: open its channel, toggle its tree node, or both."
  (ecase (getf entry :kind)
    (:inbox
     (list :open (getf entry :id)))
    (:workspace
     (list :toggle (getf entry :key)))
    (:channel
     (append (list :open (getf entry :id))
             (when (getf entry :children-p)
               (list :toggle (getf entry :id)))))))

(-> world-sidebar--entry-spans (list) list)
(defun world-sidebar--entry-spans (entry)
  "Return the styled spans of one inbox, workspace or channel ENTRY.

A channel shows whether it is current, its tree marker, its agent's state,
then its agent's name and its title; a closed node shows what it hides."
  (ecase (getf entry :kind)
    (:inbox
     (append
      (list (world-sidebar--current-span entry)
            (terminal-span ':plain " ")
            (terminal-span ':legend-accent "inbox"))
      (when (plusp (getf entry :unread))
        (list (terminal-span ':notice (format nil " ~D unread" (getf entry :unread)))))))
    (:workspace
     (append
      (list (world-sidebar--marker-span entry)
            (terminal-span ':strong (format nil " ~A" (getf entry :name))))
      (world-sidebar--hidden-spans entry)
      (when (getf entry :open-p)
        (list (terminal-span ':dim (format nil "  ~A" (getf entry :path)))))))
    (:channel
     (append
      (list (world-sidebar--current-span entry)
            (world-sidebar--marker-span entry)
            (terminal-span ':plain (make-string (1+ (* 2 (getf entry :depth))) :initial-element #\Space))
            (world-sidebar--state-span (getf entry :state))
            (terminal-span (cond
                             ((getf entry :current-p)
                              ':legend-accent)
                             ((getf entry :attached-p)
                              ':user)
                             (t
                              ':plain))
                           (or (getf entry :name)
                               (getf entry :title)
                               (conversation-identifier-display (getf entry :id)))))
      (when (plusp (getf entry :mentions))
        (list (terminal-span ':legend-accent (format nil " @~D" (getf entry :mentions)))))
      (when (plusp (getf entry :unread))
        (list (terminal-span ':notice (format nil " ~D" (getf entry :unread)))))
      (when (plusp (getf entry :pending))
        (list (terminal-span ':dim (format nil " ✉~D" (getf entry :pending)))))
      (world-sidebar--hidden-spans entry)
      (when (and (getf entry :name) (getf entry :title))
        (list (terminal-span ':dim (format nil " ~A" (getf entry :title)))))))))

(-> world-sidebar--current-span (list) cons)
(defun world-sidebar--current-span (entry)
  "Return the span marking ENTRY as the channel this terminal shows, or a blank one."
  (if (getf entry :current-p)
      (terminal-span ':legend-accent "›")
      (terminal-span ':plain " ")))

(-> world-sidebar--marker-span (list) cons)
(defun world-sidebar--marker-span (entry)
  "Return ENTRY's tree marker: open, closed, or blank without nested entries."
  (cond
    ((not (getf entry :children-p))
     (terminal-span ':plain " "))
    ((getf entry :open-p)
     (terminal-span ':dim "▾"))
    (t
     (terminal-span ':dim "▸"))))

(-> world-sidebar--hidden-spans (list) list)
(defun world-sidebar--hidden-spans (entry)
  "Return the spans counting what closed ENTRY hides: its channels and their unread mentions and messages."
  (when (and (getf entry :children-p) (not (getf entry :open-p)))
    (append
     (list (terminal-span ':dim (format nil " +~D" (getf entry :hidden))))
     (when (plusp (getf entry :hidden-mentions))
       (list (terminal-span ':legend-accent (format nil " @~D" (getf entry :hidden-mentions)))))
     (when (plusp (getf entry :hidden-unread))
       (list (terminal-span ':notice (format nil " ~D" (getf entry :hidden-unread))))))))

(-> world-sidebar--state-span (keyword) cons)
(defun world-sidebar--state-span (state)
  "Return the glyph span showing a channel's hosted STATE."
  (case state
    ((:active :working :cancelling)
     (terminal-span ':agent-spinner "● "))
    (:idle
     (terminal-span ':success "○ "))
    ((:paused :detaching :stopping :failed :starting)
     (terminal-span ':notice "○ "))
    (otherwise
     (terminal-span ':dim "· "))))
