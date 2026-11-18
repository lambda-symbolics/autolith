(in-package #:autolith)

;;;; -- World Sidebar --

;;; The side panel a terminal client shows beside its channel: the person's
;;; home channel first, with the messages waiting there unread, then every
;;; other channel beneath it, grouped by workspace, with what runs there, who
;;; is attached, and what arrived since this client last looked. Channel unread
;;; and mention baselines are client-local and start at the current counts.
;;; A started sidebar gathers on its own thread, so reading the world, the
;;; server and the inboxes never holds up the terminal's keystrokes; the
;;; panel only takes what the thread gathered last. sidebar-panel.lisp draws
;;; the entries and handles the panel's keys and clicks.

(defparameter *world-sidebar-share* 22/100
  "The share of the terminal's columns the sidebar takes left of the channel.")

(defparameter *world-sidebar-minimum-width* 24
  "The fewest columns the sidebar takes beside the channel.")

(defparameter *world-sidebar-minimum-columns* 90
  "The minimum transcript width beside the sidebar; below it Ctrl-S opens a fullscreen drawer.")

(defparameter *world-sidebar-filter-character-limit* 64
  "The most characters the sidebar's filter keeps.")

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
    :documentation "The home, workspace and channel entries shown, built from INPUTS and the filter.")
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
    :documentation "Active storage identities cached by the shared conversation locator.")
   (filter
    :initform      ""
    :accessor      world-sidebar-filter
    :type          string
    :documentation "What the person typed to narrow the channels, or the empty string.")
   (actions
    :initform      #()
    :accessor      world-sidebar-actions
    :type          vector
    :documentation "What a click on each row of the last rendering does.")
   (selected
    :initform      0
    :accessor      world-sidebar-selected
    :type          (integer 0)
    :documentation "The index of the selected entry, independent of scrolling.")
   (scroll-offset
    :initform      0
    :accessor      world-sidebar-scroll-offset
    :type          (integer 0)
    :documentation "The first entry line in the tree viewport.")
   (viewport-height
    :initform      0
    :accessor      world-sidebar-viewport-height
    :type          (integer 0)
    :documentation "The number of tree rows in the last rendering.")
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
   (last-success-at
    :initform      nil
    :accessor      world-sidebar-last-success-at
    :type          (option real)
    :documentation "The monotonic time of the last successful runtime observation.")
   (failure
    :initform      nil
    :accessor      world-sidebar-failure
    :type          (option string)
    :documentation "A bounded condition class for the latest failed observation, without remote text.")
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


;;;; -- Entries --

(defclass sidebar-entry ()
  ()
  (:documentation "One row group of the sidebar's tree: the home channel, a workspace or a channel."))

(defclass sidebar-fold ()
  ((children-p
    :initform      nil
    :accessor      sidebar-fold-children-p
    :type          boolean
    :documentation "Whether entries nest below this one.")
   (open-p
    :initform      nil
    :accessor      sidebar-fold-open-p
    :type          boolean
    :documentation "Whether the nested entries show.")
   (hidden-unread
    :initform      0
    :accessor      sidebar-fold-hidden-unread
    :type          (integer 0)
    :documentation "The unread messages of the entries a closed node hides.")
   (hidden-mentions
    :initform      0
    :accessor      sidebar-fold-hidden-mentions
    :type          (integer 0)
    :documentation "The unread mentions of the entries a closed node hides.")
   (nested
    :initform      0
    :accessor      sidebar-fold-nested
    :type          (integer 0)
    :documentation "How many entries nest below this one, shown or not."))
  (:documentation "The folding state of a tree node."))

(defclass sidebar-home (sidebar-entry)
  ((identifier
    :initarg       :identifier
    :reader        sidebar-entry-identifier
    :type          string
    :documentation "The person's inbox channel.")
   (current-p
    :initarg       :current-p
    :reader        sidebar-entry-current-p
    :type          boolean
    :documentation "Whether this terminal shows the inbox.")
   (unread
    :initarg       :unread
    :reader        sidebar-entry-unread
    :type          (integer 0)
    :documentation "The messages waiting unread for the person."))
  (:documentation "The person's home channel, first in the sidebar."))

(defclass sidebar-workspace (sidebar-entry sidebar-fold)
  ((key
    :initarg       :key
    :reader        sidebar-workspace-key
    :type          string
    :documentation "The repository or exact workspace the group stands for.")
   (name
    :initarg       :name
    :reader        sidebar-entry-name
    :type          string
    :documentation "The last directory name of the workspace.")
   (path
    :initarg       :path
    :reader        sidebar-workspace-path
    :type          string
    :documentation "The workspace with the home directory as a tilde."))
  (:documentation "A group of channels that work in one repository or directory."))

(defclass sidebar-channel (sidebar-entry sidebar-fold)
  ((identifier
    :initarg       :identifier
    :reader        sidebar-entry-identifier
    :type          string
    :documentation "The channel's identifier.")
   (parent
    :initarg       :parent
    :reader        sidebar-channel-parent
    :type          (option string)
    :documentation "The durable parent channel, or NIL for a root.")
   (name
    :initarg       :name
    :reader        sidebar-entry-name
    :type          (option string)
    :documentation "The name of the agent residing in the channel, or NIL.")
   (title
    :initarg       :title
    :reader        sidebar-channel-title
    :type          (option string)
    :documentation "The conversation's title, or NIL.")
   (path
    :initarg       :path
    :reader        sidebar-channel-path
    :type          string
    :documentation "The channel's workspace with the home directory as a tilde.")
   (depth
    :initarg       :depth
    :reader        sidebar-channel-depth
    :type          (integer 0)
    :documentation "How deep the channel nests below its workspace's roots.")
   (state
    :initarg       :state
    :reader        sidebar-channel-state
    :type          keyword
    :documentation "The hosted agent's state, or :ABSENT when no process serves the channel.")
   (waiting-for-jobs-p
    :initarg       :waiting-for-jobs-p
    :reader        sidebar-channel-waiting-for-jobs-p
    :type          boolean
    :documentation "Whether an idle agent still has jobs running.")
   (attached-p
    :initarg       :attached-p
    :reader        sidebar-channel-attached-p
    :type          boolean
    :documentation "Whether a terminal controls the channel.")
   (current-p
    :initarg       :current-p
    :reader        sidebar-entry-current-p
    :type          boolean
    :documentation "Whether this terminal shows the channel.")
   (unread
    :initarg       :unread
    :reader        sidebar-entry-unread
    :type          (integer 0)
    :documentation "The messages that arrived since this client last looked.")
   (mentions
    :initarg       :mentions
    :reader        sidebar-channel-mentions
    :type          (integer 0)
    :documentation "The mentions that arrived since this client last looked.")
   (pending
    :initarg       :pending
    :reader        sidebar-channel-pending
    :type          (integer 0)
    :documentation "The deliveries waiting in the channel's inbox."))
  (:documentation "One channel, named by the agent that resides in it."))

(defgeneric sidebar-entry-key (entry)
  (:documentation "Identify ENTRY independently of its position, title and tree state."))

(defmethod sidebar-entry-key ((entry sidebar-workspace))
  "A workspace is its group key."
  (list ':workspace (sidebar-workspace-key entry)))

(defmethod sidebar-entry-key ((entry sidebar-entry))
  "A channel, the inbox included, is its identifier."
  (list ':channel (sidebar-entry-identifier entry)))

(-> sidebar-entry-equal-p (sidebar-entry sidebar-entry) boolean)
(defun sidebar-entry-equal-p (left right)
  "Return whether LEFT and RIGHT are entries of one class that show the same values."
  (and (eq (class-of left) (class-of right))
       (loop for slot in (class-slots (class-of left))
             for name = (slot-definition-name slot)
             always (equal (slot-value left name) (slot-value right name)))))


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

A failed gather publishes a stale observation; the next one tries again."
  (loop
    (let ((inputs (handler-case
                      (list (world-sidebar--gather sidebar)
                            (funcall (world-sidebar-clock-function sidebar)))
                    (error (condition)
                      (list ':failed (world-sidebar--failure-name condition))))))
      (with-lock-held ((world-sidebar-lock sidebar))
        (setf (world-sidebar-gathered sidebar) inputs)
        (when (world-sidebar-stopping-p sidebar)
          (return))
        (condition-wait (world-sidebar-condition-variable sidebar)
                        (world-sidebar-lock sidebar)
                        :timeout *world-sidebar-refresh-seconds*)
        (when (world-sidebar-stopping-p sidebar)
          (return)))))
  nil)

(-> world-sidebar--failure-name (condition) string)
(defun world-sidebar--failure-name (condition)
  "Return a bounded condition class, never the failed response or its contents."
  (bounded-string (symbol-name (class-name (class-of condition))) :limit 80))

(-> world-sidebar--mark-stale (world-sidebar &optional (option string)) boolean)
(defun world-sidebar--mark-stale (sidebar &optional failure)
  "Keep durable rows and selection while marking runtime observations unknown."
  (let* ((inputs    (copy-list (world-sidebar-inputs sidebar)))
         (changed-p (not (getf inputs :runtime-unknown-p))))
    (setf (getf inputs :runtime-unknown-p) t
          (world-sidebar-failure sidebar) (or failure "runtime-unavailable"))
    (or (world-sidebar--take sidebar inputs) changed-p)))

(-> world-sidebar--accept (world-sidebar list &optional (option real)) boolean)
(defun world-sidebar--accept (sidebar inputs &optional observed-at)
  "Accept gathered INPUTS with their observation time, independently of local tree clicks."
  (let ((changed-p (not (eql (getf (world-sidebar-inputs sidebar) :runtime-unknown-p)
                             (getf inputs :runtime-unknown-p)))))
    (if (getf inputs :runtime-unknown-p)
        (setf (world-sidebar-failure sidebar) "runtime-unavailable")
        (setf (world-sidebar-last-success-at sidebar)
              (or observed-at (funcall (world-sidebar-clock-function sidebar)))
              (world-sidebar-failure sidebar) nil))
    (or (world-sidebar--take sidebar inputs) changed-p)))

(-> world-sidebar--take (world-sidebar list) boolean)
(defun world-sidebar--take (sidebar inputs)
  "Rebuild SIDEBAR's entries from INPUTS, keeping the selected entry; return whether they differ."
  (let* ((previous (world-sidebar-entries sidebar))
         (selected (nth (world-sidebar-selected sidebar) previous))
         (entries  (apply (if (plusp (length (world-sidebar-filter sidebar)))
                              #'world-sidebar--matches
                              #'world-sidebar--entries)
                          sidebar inputs)))
    (setf (world-sidebar-inputs sidebar) inputs
          (world-sidebar-entries sidebar) entries
          (world-sidebar-selected sidebar)
          (or (and selected
                   (position (sidebar-entry-key selected) entries
                             :test #'equal :key #'sidebar-entry-key))
              0))
    (not (and (= (length entries) (length previous))
              (every #'sidebar-entry-equal-p entries previous)))))

(-> world-sidebar--toggle (world-sidebar sidebar-fold) null)
(defun world-sidebar--toggle (sidebar node)
  "Open tree NODE in SIDEBAR when it shows closed, or close it, and rebuild the entries.

The choice stays when the terminal moves to another channel."
  (setf (gethash (second (sidebar-entry-key node)) (world-sidebar-toggled sidebar))
        (if (sidebar-fold-open-p node) ':closed ':open))
  (world-sidebar--take sidebar (world-sidebar-inputs sidebar))
  nil)

(-> world-sidebar-set-filter (world-sidebar string) null)
(defun world-sidebar-set-filter (sidebar filter)
  "Narrow SIDEBAR to the channels FILTER matches, or show the tree again for an empty one."
  (setf (world-sidebar-filter sidebar)
        (subseq filter 0 (min (length filter) *world-sidebar-filter-character-limit*))
        (world-sidebar-selected sidebar) 0
        (world-sidebar-scroll-offset sidebar) 0)
  (world-sidebar--take sidebar (world-sidebar-inputs sidebar))
  nil)

(-> world-sidebar-tree (world-sidebar) list)
(defun world-sidebar-tree (sidebar)
  "Return SIDEBAR's complete gathered tree, including descendants of closed nodes.

Folding describes this terminal's view; clients may fold independently."
  (apply #'world-sidebar--entries sidebar :include-closed-p t (world-sidebar-inputs sidebar)))

(-> world-sidebar--channel-entry-function
    (world-sidebar &key (:names list) (:statuses list) (:pending list) (:runtime-unknown-p boolean))
    function)
(defun world-sidebar--channel-entry-function (sidebar &key names statuses pending runtime-unknown-p)
  "Return a function of a channel and its depth that builds the channel's entry.

NAMES and PENDING pair channel identifiers with agent names and waiting
deliveries; STATUSES are the hosted channels' status plists, which a
RUNTIME-UNKNOWN-P observation replaces with an unknown state."
  (flet ((table (pairs)
           (let ((table (make-hash-table :test #'equal)))
             (loop for (identifier . value) in pairs
                   do (setf (gethash identifier table) value))
             table)))
    (let ((names    (table names))
          (pending  (table pending))
          (statuses (table (loop for status in statuses
                                 for identifier = (getf status :session-id)
                                 when (stringp identifier)
                                   collect (cons identifier status)))))
      (lambda (channel depth)
        (let ((identifier (channel-identifier channel)))
          (world-sidebar--channel-entry sidebar channel
                                        :depth   depth
                                        :name    (gethash identifier names)
                                        :status  (if runtime-unknown-p
                                                     (list :state ':unknown)
                                                     (gethash identifier statuses))
                                        :pending (gethash identifier pending)))))))

(-> world-sidebar--fold (sidebar-fold list &key (:open-p boolean)) boolean)
(defun world-sidebar--fold (node entries &key open-p)
  "Record on NODE the ENTRIES nesting below it and whether they show; return OPEN-P.

A closed node carries their unread messages and mentions, so nothing new below
it goes unseen."
  (let ((channels (remove-if-not (lambda (entry) (typep entry 'sidebar-channel)) entries)))
    (setf (sidebar-fold-children-p node) t
          (sidebar-fold-open-p node) open-p
          (sidebar-fold-nested node) (length channels)
          (sidebar-fold-hidden-unread node)
          (if open-p 0 (reduce #'+ channels :key #'sidebar-entry-unread))
          (sidebar-fold-hidden-mentions node)
          (if open-p 0 (reduce #'+ channels :key #'sidebar-channel-mentions))))
  open-p)

(-> world-sidebar--entries
    (world-sidebar &key (:inbox (option channel)) (:channels list) (:groups list)
                       (:names list) (:statuses list) (:pending list) (:include-closed-p boolean)
                       (:runtime-unknown-p boolean))
    list)
(defun world-sidebar--entries
    (sidebar &key inbox channels groups names statuses pending include-closed-p runtime-unknown-p)
  "Return the home INBOX entry, then the workspace and channel entries of the tree.

The inbox's unread count is what waits in it for the person, so it stays true
across clients and restarts. CHANNELS arrive latest touched first. The group of
the current channel comes first, then the others in that order. Within a group,
channels nest under their parents. The first group is open, and a channel when
the current one nests below it, unless the person opened or closed them.
INCLUDE-CLOSED-P retains every descendant for structured clients."
  (let ((current (world-sidebar-current sidebar))
        (entry   (world-sidebar--channel-entry-function
                  sidebar :names names :statuses statuses :pending pending
                          :runtime-unknown-p runtime-unknown-p)))
    (labels ((open-p (key default-p)
               "Return whether tree node KEY is open: as the person last set it, or else DEFAULT-P."
               (ecase (gethash key (world-sidebar-toggled sidebar) ':default)
                 (:open
                  t)
                 (:closed
                  nil)
                 (:default
                  default-p)))

             (forest-entries (trees depth)
               "Return the shown entries of TREES at DEPTH and all of their entries."
               (let ((shown nil)
                     (all   nil))
                 (dolist (tree trees (values shown all))
                   (multiple-value-bind (tree-shown tree-all) (tree-entries tree depth)
                     (setf shown (nconc shown tree-shown)
                           all   (nconc all tree-all))))))

             (tree-entries (tree depth)
               "Return the shown entries of TREE at DEPTH and all of its entries."
               (let ((node (funcall entry (first tree) depth)))
                 (if (null (rest tree))
                     (values (list node) (list node))
                     (multiple-value-bind (shown all) (forest-entries (rest tree) (1+ depth))
                       (let ((open-p (world-sidebar--fold
                                      node all
                                      :open-p (open-p (sidebar-entry-identifier node)
                                                      (and (find current all
                                                                 :key  #'sidebar-entry-identifier
                                                                 :test #'equal)
                                                           t)))))
                         (values (cons node (and (or include-closed-p open-p) shown))
                                 (cons node all))))))))
      (append
       (when inbox
         (list (make-instance 'sidebar-home
                              :identifier (channel-identifier inbox)
                              :current-p  (equal (channel-identifier inbox) current)
                              :unread     (or (rest (assoc (channel-identifier inbox) pending
                                                           :test #'string=))
                                              0))))
       (loop for group in (world-repository-current-groups
                           (or groups (world-repository-workspace-groups channels current)) current)
             for directory = (world-repository-group-directory group)
             for first-p = t then nil
             nconc (multiple-value-bind (shown all)
                       (forest-entries (world-channel-forest (world-repository-group-channels group)) 0)
                     (let* ((workspace (make-instance 'sidebar-workspace
                                                      :key  (world-repository-group-key group)
                                                      :name (world-workspace-leaf directory)
                                                      :path (world-sidebar--short-path directory)))
                            (open-p    (world-sidebar--fold
                                        workspace all
                                        :open-p (open-p (sidebar-workspace-key workspace) first-p))))
                       (cons workspace (and (or include-closed-p open-p) shown)))))))))

(-> world-sidebar--matches
    (world-sidebar &key (:inbox (option channel)) (:channels list) (:groups list)
                       (:names list) (:statuses list) (:pending list) (:runtime-unknown-p boolean))
    list)
(defun world-sidebar--matches
    (sidebar &key inbox channels groups names statuses pending runtime-unknown-p)
  "Return the entries of the CHANNELS that SIDEBAR's filter matches, best first, without a tree.

A channel matches by its agent's name, its identifier, its title or its
workspace, in open and closed groups alike."
  (declare (ignore inbox groups))
  (let ((entry (world-sidebar--channel-entry-function
                sidebar :names names :statuses statuses :pending pending
                        :runtime-unknown-p runtime-unknown-p)))
    (sito:rank (world-sidebar-filter sidebar)
               (mapcar (lambda (channel) (funcall entry channel 0)) channels)
               :key (lambda (entry)
                      (list (sidebar-entry-name entry)
                            (conversation-identifier-display (sidebar-entry-identifier entry))
                            (sidebar-channel-title entry)
                            (sidebar-channel-path entry))))))

(-> world-sidebar--channel-entry
    (world-sidebar channel &key (:depth (integer 0)) (:name (option string)) (:status (option list))
                           (:pending (option integer)))
    sidebar-channel)
(defun world-sidebar--channel-entry (sidebar channel &key (depth 0) name status pending)
  "Return the entry of CHANNEL at DEPTH, whose agent is NAME, with its hosted STATUS and PENDING deliveries."
  (let* ((identifier (channel-identifier channel))
         (current-p  (equal identifier (world-sidebar-current sidebar))))
    (make-instance
     'sidebar-channel
     :identifier         identifier
     :parent             (channel-parent channel)
     :name               name
     :title              (channel-title channel)
     :path               (world-sidebar--short-path (channel-workspace channel))
     :depth              depth
     :state              (if status (getf status :state) ':absent)
     :waiting-for-jobs-p (and (getf status :waiting-for-input-p)
                              (plusp (or (getf status :job-live-count) 0))
                              t)
     :attached-p         (and status (getf status :terminal-attached-p) t)
     :current-p          current-p
     :unread             (world-sidebar--unread (world-sidebar-seen sidebar) identifier
                                                :count       (and status (getf status :message-count))
                                                :caught-up-p current-p)
     :mentions           (world-sidebar--unread (world-sidebar-mentions-seen sidebar) identifier
                                                :count       (and status (getf status :mention-count))
                                                :caught-up-p current-p)
     :pending            (or pending 0))))

(-> world-sidebar--gather (world-sidebar) list)
(defun world-sidebar--gather (sidebar)
  "Gather the read-only catalogue, then guard positive repository observations in the durable store."
  (let ((configuration (world-sidebar-configuration sidebar)))
    (multiple-value-bind (statuses known-p) (world-sidebar--statuses sidebar)
      (let* ((world      (world-channel-catalogue configuration :statuses statuses))
             (channels   (world-sidebar--channels sidebar world))
             (groups     (world-repository-groups channels (world-sidebar-current sidebar)))
             (remembered (handler-case (world-repository-remember configuration channels groups)
                           (autolith-error () channels))))
        (unless (eq remembered channels)
          (setf channels remembered
                groups (world-repository-groups channels (world-sidebar-current sidebar))))
        (list :inbox             (world-inbox-channel world)
              :channels          channels
              :groups            groups
              :names             (world-sidebar--names world)
              :statuses          statuses
              :runtime-unknown-p (not known-p)
              :pending           (world-sidebar--pending configuration))))))

(-> world-sidebar--names (world) list)
(defun world-sidebar--names (world)
  "Return (CHANNEL-ID . NAME) pairs naming the agent that resides in each of WORLD's channels.

An agent still called by its channel's identifier has no name worth showing."
  (loop for participant in (world-participants world)
        for residence = (participant-residence participant)
        when (and (eq (participant-kind participant) ':agent)
                  residence
                  (not (string-equal (participant-name participant)
                                     (conversation-identifier-display residence))))
          collect (cons residence (participant-name participant))))

(-> world-sidebar--channels (world-sidebar world) list)
(defun world-sidebar--channels (sidebar world)
  "Return catalogue channels except the inbox, newest history first.

A newly hosted channel without durable history sorts by its creation time."
  (let* ((channels   (remove-if #'channel-inbox-p (world-channels world)))
         (identities (conversation-pathnames-for-ids
                      (world-sidebar-configuration sidebar)
                      (mapcar #'channel-identifier channels)
                      :cache (world-sidebar-pathnames sidebar))))
    (mapcar #'rest
            (stable-sort
             (mapcar (lambda (channel)
                       (let ((date (conversation-storage-write-date
                                    (gethash (channel-identifier channel) identities))))
                         (cons (if (plusp date) date (channel-created-at channel)) channel)))
                     channels)
             #'> :key #'first))))

(-> world-sidebar--statuses (world-sidebar) (values list boolean))
(defun world-sidebar--statuses (sidebar)
  "Return live statuses and whether the endpoint answered; signal query failures."
  (let ((entry (world-client-entry (world-sidebar-configuration sidebar))))
    (if entry
        (values (mapcar #'rest (getf (rest (localgroup-query-record entry ':channels)) :channels)) t)
        (values nil nil))))

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
