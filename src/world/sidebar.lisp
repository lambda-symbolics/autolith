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

(defparameter *world-sidebar-width* 28
  "The columns the sidebar takes left of the channel.")

(defparameter *world-sidebar-minimum-columns* 100
  "The narrowest terminal that shows the sidebar beside the channel.")

(defparameter *world-sidebar-refresh-seconds* 1
  "How often the sidebar asks the world and the server what changed.")

(defparameter *world-sidebar-reconcile-seconds* 10
  "How often the sidebar reconciles conversations into channels itself.")

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
    :documentation "The workspace and channel entries as last gathered.")
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
   (reconciled-at
    :initform      nil
    :accessor      world-sidebar-reconciled-at
    :type          (option real)
    :documentation "The internal real time of the last reconciliation, or NIL before the first.")
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
   (world-cache
    :initform      nil
    :accessor      world-sidebar-world-cache
    :type          (option cons)
    :documentation "The (TOKEN . WORLD) the sidebar read last, read again only once the world log changed.")
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
    :documentation "(ENTRIES) as the gatherer last produced them, or NIL when nothing new waits.")
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

(defmethod screen-panel-width ((panel world-sidebar))
  "The sidebar's fixed width."
  *world-sidebar-width*)

(defmethod screen-panel-minimum-columns ((panel world-sidebar))
  "The narrowest terminal that keeps a readable channel beside the sidebar."
  *world-sidebar-minimum-columns*)

(defmethod screen-panel-refresh ((panel world-sidebar))
  "Take the newest gathered entries and return whether they changed.

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
              (world-sidebar--take panel (world-sidebar--gather panel now)))))))

(defmethod screen-panel-rows ((panel world-sidebar) terminal height)
  "Render the sidebar's entries as HEIGHT rows for TERMINAL."
  (loop for spans in (world-sidebar--rows (world-sidebar-entries panel) height)
        collect (terminal--render-spans
                 terminal (terminal--pad-spans spans *world-sidebar-width*))))


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
    (let ((entries (handler-case
                       (world-sidebar--gather sidebar (funcall (world-sidebar-clock-function sidebar)))
                     (error ()
                       nil))))
      (with-lock-held ((world-sidebar-lock sidebar))
        (when entries
          (setf (world-sidebar-gathered sidebar) (list entries)))
        (when (world-sidebar-stopping-p sidebar)
          (return))
        (condition-wait (world-sidebar-condition-variable sidebar)
                        (world-sidebar-lock sidebar)
                        :timeout *world-sidebar-refresh-seconds*)
        (when (world-sidebar-stopping-p sidebar)
          (return)))))
  nil)

(-> world-sidebar--take (world-sidebar list) boolean)
(defun world-sidebar--take (sidebar entries)
  "Show ENTRIES in SIDEBAR and return whether they differ from those shown before."
  (prog1 (not (equal entries (world-sidebar-entries sidebar)))
    (setf (world-sidebar-entries sidebar) entries)))

(-> world-sidebar--gather (world-sidebar real) list)
(defun world-sidebar--gather (sidebar now)
  "Return the sidebar's entries for the world as it is at internal time NOW."
  (let* ((configuration (world-sidebar-configuration sidebar))
         (statuses      (world-sidebar--statuses sidebar))
         (world         (world-sidebar--world sidebar now)))
    (world-sidebar--entries sidebar
                            :inbox    (world-inbox-channel world)
                            :channels (world-sidebar--channels configuration world statuses)
                            :statuses statuses
                            :pending  (world-sidebar--pending configuration))))

(-> world-sidebar--channels (configuration world list) list)
(defun world-sidebar--channels (configuration world statuses)
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
                                      (conversation-pathname-for-id configuration
                                                                    (channel-identifier channel)))
                                     channel))
                             (remove-if #'channel-inbox-p (world-channels world))))
             #'> :key #'first))))

(-> world-sidebar--world (world-sidebar real) world)
(defun world-sidebar--world (sidebar now)
  "Return the world, reconciling conversations into it when that is due.

A world that cannot be read shows as empty rather than ending the client."
  (let ((configuration (world-sidebar-configuration sidebar))
        (last          (world-sidebar-reconciled-at sidebar)))
    (handler-case
        (if (or (null last)
                (>= (- now last)
                    (* *world-sidebar-reconcile-seconds* internal-time-units-per-second)))
            (progn
              (setf (world-sidebar-reconciled-at sidebar) now)
              (values (world-reconcile-sessions configuration)))
            (rest (setf (world-sidebar-world-cache sidebar)
                        (world--cached configuration (world-sidebar-world-cache sidebar)))))
      (autolith-error ()
        (make-instance 'world)))))

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
    (world-sidebar &key (:inbox (option channel)) (:channels list) (:statuses list)
                        (:pending list))
    list)
(defun world-sidebar--entries (sidebar &key inbox channels statuses pending)
  "Return the INBOX entry, then workspace and channel entries of CHANNELS, with STATUSES and PENDING deliveries folded in.

The inbox's unread count is what waits in it for the person, so it stays
true across clients and restarts. CHANNELS arrive latest touched first. The
workspace of the current channel comes first, then the others in that order.
Within a workspace, channels nest under their parents."
  (let ((current (world-sidebar-current sidebar))
        (table   (make-hash-table :test #'equal)))
    (dolist (status statuses)
      (let ((identifier (getf status :session-id)))
        (when (stringp identifier)
          (setf (gethash identifier table) status))))
    (append
     (when inbox
       (list (list :kind      ':inbox
                   :id        (channel-identifier inbox)
                   :current-p (equal (channel-identifier inbox) current)
                   :unread    (or (rest (assoc (channel-identifier inbox) pending :test #'string=))
                                  0))))
     (loop for (workspace . members) in (world-sidebar--groups channels current)
           append (cons (list :kind ':workspace
                              :name (world-workspace-leaf workspace)
                              :path (world-sidebar--short-path workspace))
                        (loop for (channel . depth) in (world-sidebar--tree members)
                              collect (world-sidebar--channel-entry
                                       sidebar channel
                                       :depth   depth
                                       :status  (gethash (channel-identifier channel) table)
                                       :pending (rest (assoc (channel-identifier channel)
                                                             pending
                                                             :test #'string=)))))))))

(-> world-sidebar--groups (list (option string)) list)
(defun world-sidebar--groups (channels current)
  "Return (WORKSPACE . CHANNELS) groups of CHANNELS, the group of CURRENT first.

CHANNELS arrive newest first, which orders the groups and their members."
  (let ((groups nil))
    (dolist (channel channels)
      (let ((group (assoc (channel-workspace channel) groups :test #'string=)))
        (if group
            (setf (rest group) (append (rest group) (list channel)))
            (setf groups (append groups (list (list (channel-workspace channel) channel)))))))
    (let ((home (find-if (lambda (group)
                           (find current (rest group) :key #'channel-identifier :test #'equal))
                         groups)))
      (if home
          (cons home (remove home groups))
          groups))))

(-> world-sidebar--tree (list) list)
(defun world-sidebar--tree (members)
  "Return (CHANNEL . DEPTH) pairs of MEMBERS, each child after its parent."
  (labels ((children (parent depth)
             "Return PARENT's members at DEPTH, each followed by its own children."
             (loop for channel in members
                   when (if parent
                            (equal (channel-parent channel) (channel-identifier parent))
                            (or (null (channel-parent channel))
                                (not (find (channel-parent channel) members
                                           :key #'channel-identifier :test #'equal))))
                     append (cons (cons channel depth)
                                  (children channel (1+ depth))))))
    (children nil 0)))

(-> world-sidebar--channel-entry
    (world-sidebar channel &key (:depth (integer 0)) (:status (option list))
                           (:pending (option integer)))
    list)
(defun world-sidebar--channel-entry (sidebar channel &key (depth 0) status pending)
  "Return the entry of CHANNEL at DEPTH, with its hosted STATUS and PENDING deliveries."
  (let* ((identifier (channel-identifier channel))
         (current-p  (equal identifier (world-sidebar-current sidebar))))
    (list :kind       ':channel
          :id         identifier
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

(-> world-sidebar--rows (list (integer 0)) list)
(defun world-sidebar--rows (entries height)
  "Return HEIGHT rows of styled spans showing ENTRIES under a heading, with the key legend last.

Entries past the rows available are counted on the last row they would take."
  (let* ((body   (mapcar #'world-sidebar--entry-spans entries))
         (limit  (max 0 (- height 2)))
         (shown  (if (> (length body) limit)
                     (let ((kept (max 0 (1- limit))))
                       (append (subseq body 0 kept)
                               (list (list (terminal-span ':dim
                                                          (format nil " … ~D more"
                                                                  (- (length body) kept)))))))
                     body))
         (rows   (cons (list (terminal-span ':brand " Channels")) shown))
         (legend (list (terminal-span ':legend-dim " Ctrl-T ")
                       (terminal-span ':legend-plain "hide"))))
    (subseq (append rows
                    (make-list (max 0 (- height 1 (length rows))) :initial-element nil)
                    (list legend))
            0 height)))

(-> world-sidebar--entry-spans (list) list)
(defun world-sidebar--entry-spans (entry)
  "Return the styled spans of one inbox, workspace or channel ENTRY."
  (ecase (getf entry :kind)
    (:inbox
     (append
      (list (if (getf entry :current-p)
                (terminal-span ':legend-accent "▸ ")
                (terminal-span ':plain "  "))
            (terminal-span (if (plusp (getf entry :unread)) ':strong ':plain) "✉ inbox"))
      (when (plusp (getf entry :unread))
        (list (terminal-span ':notice (format nil " ~D unread" (getf entry :unread)))))))
    (:workspace
     (list (terminal-span ':strong (format nil " ~A" (getf entry :name)))
           (terminal-span ':dim (format nil "  ~A" (getf entry :path)))))
    (:channel
     (append
      (list (if (getf entry :current-p)
                (terminal-span ':legend-accent "▸ ")
                (terminal-span ':plain "  "))
            (if (getf entry :attached-p)
                (terminal-span ':user "● ")
                (terminal-span ':plain "  "))
            (world-sidebar--state-span (getf entry :state)))
      (when (plusp (getf entry :depth))
        (list (terminal-span ':dim
                             (format nil "~A↳ "
                                     (make-string (* 2 (1- (getf entry :depth)))
                                                  :initial-element #\Space)))))
      (list (terminal-span ':command-id
                           (conversation-identifier-display (getf entry :id))))
      (when (plusp (getf entry :mentions))
        (list (terminal-span ':legend-accent (format nil " @~D" (getf entry :mentions)))))
      (when (plusp (getf entry :unread))
        (list (terminal-span ':notice (format nil " ~D" (getf entry :unread)))))
      (when (plusp (getf entry :pending))
        (list (terminal-span ':dim (format nil " ✉~D" (getf entry :pending)))))
      (when (getf entry :title)
        (list (terminal-span ':plain (format nil " ~A" (getf entry :title)))))))))

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
