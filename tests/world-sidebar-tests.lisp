(in-package #:autolith)

;;;; -- World Sidebar Tests --

(defparameter *world-sidebar-tests-width* 28
  "The sidebar width the row tests render at.")

(-> world-sidebar-tests--text (list) list)
(defun world-sidebar-tests--text (rows)
  "Return ROWS of styled spans as plain text lines of the test width."
  (let ((terminal (make-instance 'recording-terminal :columns 80 :rows 24)))
    (loop for spans in rows
          collect (terminal--render-spans terminal (terminal--pad-spans spans *world-sidebar-tests-width*)))))

(-> test-world-sidebar-rows () null)
(defun test-world-sidebar-rows ()
  "Test the sidebar's rows show workspaces, nested channels by their agents, their state and what arrived."
  (let* ((entries
           (list (list :kind ':workspace :key "/root/claude-work/frob/" :name "frob" :path "~/claude-work/frob"
                       :children-p t :open-p t)
                 (list :kind ':channel :id "b6uouHJ" :name "keeper" :title "checks" :depth 0
                       :state ':working :attached-p t :current-p t
                       :unread 0 :mentions 0 :pending 0 :children-p t :open-p t)
                 (list :kind ':channel :id "c6uouHJ" :name nil :title "task" :depth 1
                       :state ':absent :attached-p nil :current-p nil
                       :unread 0 :mentions 0 :pending 0)
                 (list :kind ':channel :id "d6uouHJ" :name "fixer" :title "tools" :depth 0
                       :state ':idle :attached-p nil :current-p nil
                       :unread 3 :mentions 2 :pending 0 :children-p t :open-p nil
                       :hidden 4 :hidden-unread 5 :hidden-mentions 0)
                 (list :kind ':workspace :key "/srv/www/" :name "www" :path "/srv/www"
                       :children-p t :open-p nil :hidden 2 :hidden-unread 1 :hidden-mentions 0)
                 (list :kind ':channel :id "e6uouHJ" :name nil :title nil :depth 0
                       :state ':absent :attached-p nil :current-p nil
                       :unread 0 :mentions 0 :pending 1)))
         (lines (world-sidebar-tests--text (world-sidebar--rows entries 12))))
    (test-assert (and (= (length lines) 12)
                      (every (lambda (line) (= (text-cell-width line) *world-sidebar-tests-width*))
                             lines))
                 "the sidebar fills its height with rows of exactly its width")
    (test-assert (and (search "CHANNELS" (first lines))
                      (search "Ctrl-S" (first (last lines))))
                 "the sidebar shows its heading and toggle shortcut")
    (test-assert (search "▾ frob  ~/claude-work/frob" (third lines))
                 "an open workspace row names the directory and its short path")
    (test-assert (search "›▾ ● keeper checks" (fourth lines))
                 "the current channel is marked, open, with its working agent's name and its title")
    (test-assert (search "     · task" (fifth lines))
                 "a nested channel without an agent shows its title, indented under its parent")
    (test-assert (search " ▸ ○ fixer @2 3 +4 5" (sixth lines))
                 "a closed channel shows its unread mentions and messages, then what it hides")
    (test-assert (search "▸ www +2 1" (seventh lines))
                 "a closed workspace counts its channels and their unread messages")
    (test-assert (search "· e-6uouHJ 1 pending" (eighth lines))
                 "a channel without a name or title shows its identifier and the deliveries waiting")
    (multiple-value-bind (rows actions) (world-sidebar--rows entries 7 :offset 3)
      (let ((cut (world-sidebar-tests--text rows)))
        (test-assert (and (= (length cut) 7)
                          (equal (getf (third actions) :open) "d6uouHJ")
                          (equal (mapcar (lambda (button) (getf button :command))
                                         (getf (sixth actions) :buttons))
                                 '("(new)" "(delete)")))
                     "the viewport scrolls tree entries without moving command actions"))))
  (let ((entries (list (list :kind ':inbox :id "f6uouHJ" :current-p nil :unread 2))))
    (multiple-value-bind (rows actions) (world-sidebar--rows entries 6)
      (let* ((lines (world-sidebar-tests--text rows))
             (inbox (find "home ~" (third rows) :key #'terminal-span-text :test #'equal)))
        (test-assert (search "home ~ 2 unread" (third lines))
                     "the home row shows how many messages wait unread")
        (test-assert (and (null (second rows))
                          (null (second actions))
                          (equal (getf (third actions) :open) "f6uouHJ")
                          (eq (terminal-span-style inbox) ':user))
                     "the inbox has a separate color and its action follows the heading's spacer")))
    (loop for height from 0 to 6
          do (multiple-value-bind (rows actions) (world-sidebar--rows entries height)
                 (test-assert (and (= (length rows) height (length actions))
                                   (or (zerop height)
                                       (= 2 (length (getf (nth (max 0 (- height 2)) actions)
                                                         :buttons)))))
                              "short sidebars keep footer actions outside the available tree viewport"))))
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration))
          (channel (make-instance 'channel :identifier "jobs" :owner "user"
                                           :workspace "/w/" :created-at 1 :updated-at 1)))
      (dolist (case '((:idle t 0 :success "○ ")
                      (:working t 1 :notice "● ")
                      (:active nil 1 :agent-spinner "● ")
                      (:working nil 1 :agent-spinner "● ")))
        (destructuring-bind (state waiting-p jobs style glyph) case
          (let* ((entry (world-sidebar--channel-entry
                         sidebar channel :status (list :state                state
                                                        :waiting-for-input-p waiting-p
                                                        :job-live-count      jobs)))
                 (spans (world-sidebar--entry-spans entry)))
            (test-assert (equal (fourth spans) (terminal-span style glyph))
                         (format nil "channel state ~S with ~D jobs and waiting ~S has the right indicator"
                                 state jobs waiting-p)))))))
  nil)

(-> test-world-sidebar-tree () null)
(defun test-world-sidebar-tree ()
  "Test the sidebar opens only the way to the current channel, and clicks open, close and switch."
  (with-test-configuration (configuration)
    (let ((sidebar  (world-sidebar-create configuration))
          (terminal (make-instance 'recording-terminal :columns 120 :rows 24)))
      (flet ((channel (identifier workspace &optional parent)
               "Return channel IDENTIFIER in WORKSPACE, nested under PARENT."
               (make-instance 'channel :identifier identifier :owner "user" :workspace workspace
                                       :parent parent :created-at 1 :updated-at 1))

             (shown ()
               "Return the identifiers and keys of the entries shown, in order."
               (mapcar (lambda (entry) (or (getf entry :id) (getf entry :key)))
                       (world-sidebar-entries sidebar)))

             (click (column row)
               "Render the sidebar, then click it at COLUMN and ROW."
               (screen-panel-rows sidebar terminal :height 20 :width *world-sidebar-tests-width*)
               (screen-panel-click sidebar column row)))
        (setf (world-sidebar-current sidebar) "bkid1")
        (world-sidebar--take sidebar
                             (list :channels (list (channel "broot1" "/w/a/")
                                                   (channel "bkid1" "/w/a/" "broot1")
                                                   (channel "bkid2" "/w/a/" "broot1")
                                                   (channel "bfar1" "/w/b/")
                                                   (channel "bfar2" "/w/b/" "bfar1"))
                                   :names    '(("bkid1" . "helper"))))
        (test-assert (equal (shown) '("/w/a/" "broot1" "bkid1" "bkid2" "/w/b/"))
                     "the current channel's workspace and parent are open; other workspaces are closed")
        (test-assert (equal (click 2 4) ':redraw)
                     "a click on a channel's marker closes it")
        (test-assert (equal (shown) '("/w/a/" "broot1" "/w/b/"))
                     "a closed channel hides its nested channels")
        (let* ((tree    (world-sidebar-tree sidebar))
               (child   (find "bkid1" tree :key (lambda (entry) (getf entry :id)) :test #'equal))
               (distant (find "bfar2" tree :key (lambda (entry) (getf entry :id)) :test #'equal)))
          (test-assert
           (equal (mapcar (lambda (entry) (or (getf entry :id) (getf entry :key))) tree)
                  '("/w/a/" "broot1" "bkid1" "bkid2" "/w/b/" "bfar1" "bfar2"))
           "the complete model includes descendants of closed channels and workspaces")
          (test-assert (and (equal (getf child :parent) "broot1")
                            (equal (getf child :name) "helper")
                            (eq (getf child :state) ':absent)
                            (equal (getf distant :parent) "bfar1"))
                       "complete channel rows retain durable parent IDs, agent names and state"))
        (click 2 5)
        (test-assert (equal (shown) '("/w/a/" "broot1" "/w/b/" "bfar1"))
                     "a click on a closed workspace opens it, its own nests still closed")
        (test-assert (equal (click 10 6) '(:switch :session-id "bfar1"))
                     "a click on another channel's name asks to switch to it")
        (click 2 6)
        (setf (world-sidebar-current sidebar) "bfar2")
        (world-sidebar--take sidebar (world-sidebar-inputs sidebar))
        (test-assert (equal (shown) '("/w/b/" "bfar1" "bfar2" "/w/a/"))
                     "a node the person opened stays open when the terminal moves into it")
        (test-assert (eq (click 10 5) ':dismiss)
                     "a click on the current channel asks to return to its composer"))))
  (with-test-configuration (configuration)
    (let* ((sidebar  (world-sidebar-create configuration))
           (terminal (make-instance 'recording-terminal :columns 120 :rows 24))
           (home     (make-instance 'channel :identifier "home" :owner "user" :inbox-p t
                                            :workspace "/w/" :created-at 1 :updated-at 1))
           (root     (make-instance 'channel :identifier "root" :owner "user" :parent "home"
                                            :workspace "/w/" :created-at 1 :updated-at 1))
           (child    (make-instance 'channel :identifier "child" :owner "user" :parent "root"
                                            :workspace "/w/" :created-at 1 :updated-at 1)))
      (setf (world-sidebar-current sidebar) "child")
      (world-sidebar--take sidebar (list :inbox home :channels (list root child)))
      (let ((rows (screen-panel-rows sidebar terminal :height 20 :width *world-sidebar-tests-width*)))
        (test-assert (and (eql (position #\▾ (fourth rows)) 2)
                          (eql (position #\▾ (fifth rows)) 3))
                     "workspace and channel markers are indented beneath home"))
      (test-assert (and (eq (screen-panel-click sidebar 4 5) ':redraw)
                        (= (length (world-sidebar-entries sidebar)) 3)
                        (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "root")))
                   "the indented marker folds the channel while Enter opens its selected name")
      (test-assert (equal (screen-panel-click sidebar 10 3) '(:switch :session-id "home"))
                   "the home row opens its channel")))
  (with-test-configuration (configuration)
    (let* ((sidebar  (world-sidebar-create configuration))
           (terminal (make-instance 'recording-terminal :columns 120 :rows 24))
           (root     (make-instance 'channel :identifier "repo-root" :owner "user"
                                             :workspace "/repo/" :created-at 1 :updated-at 1
                                             :repository-key "git:/repo/.git/" :repository-directory "/repo/"))
           (child    (make-instance 'channel :identifier "repo-child" :owner "user"
                                             :workspace "/isolated/worktree/" :parent "repo-root"
                                             :repository-key "git:/repo/.git/" :repository-directory "/repo/"
                                             :created-at 1 :updated-at 1))
           (group    (first
                      (test-call-with-function-replacements
                       (list (list 'world-repository--git-output
                                   (lambda (directory arguments)
                                     (declare (ignore directory arguments)) nil)))
                       (lambda () (world-repository-groups (list root child) "repo-child"))))))
      (setf (world-sidebar-current sidebar) "repo-child")
      (test-call-with-function-replacements
       (list (list 'world-repository-groups
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Pure sidebar rebuild attempted repository discovery"))))
       (lambda ()
         (world-sidebar--take sidebar (list :channels (list root child) :groups (list group)))
         (test-assert (equal '("git:/repo/.git/" "repo-root" "repo-child")
                             (mapcar (lambda (entry) (or (getf entry :id) (getf entry :key)))
                                     (world-sidebar-entries sidebar)))
                      "precomputed repository groups nest a child from a distinct worktree")
         (test-assert (= 1 (getf (third (world-sidebar-entries sidebar)) :depth))
                      "the isolated child nests beneath its actual parent")
         (screen-panel-rows sidebar terminal :height 20 :width *world-sidebar-tests-width*)
         (test-assert (equal (screen-panel-click sidebar 10 4) '(:switch :session-id "repo-root"))
                      "group headings do not replace the channel switch target")
         (world-sidebar--toggle sidebar (world-repository-group-key group))
         (test-assert (= 1 (length (world-sidebar-entries sidebar)))
                      "folding a resolved repository uses the supplied snapshot without discovery")
         (test-assert (and (string= (channel-workspace root) "/repo/")
                           (string= (channel-workspace child) "/isolated/worktree/"))
                      "presentation grouping preserves both actual execution directories")))))
  nil)

(-> test-world-sidebar-visibility () null)
(defun test-world-sidebar-visibility ()
  "Test sidebar visibility follows a terminal's successive fullscreen attachments."
  (with-test-configuration (configuration)
    (let* ((sidebar  (world-sidebar-create configuration))
           (terminal (make-instance 'recording-terminal :columns 120 :rows 30))
           (first    (localgroup-screen-create :terminal terminal :panel sidebar)))
      (unwind-protect
           (progn
             (localgroup-screen-show-loading first "Channel" :start-p t)
             (test-assert (localgroup-screen-panel-visible-p first)
                          "a wide fullscreen terminal shows the sidebar at first")
             (test-assert (eq (localgroup-screen-filter-event first *localgroup-screen-toggle-event*)
                              ':ignore)
                          "the toggle key is consumed by the screen")
             (test-assert (not (localgroup-screen-panel-visible-p first))
                          "the toggle hides the sidebar")
             (localgroup-screen-close first)
             (let ((second (localgroup-screen-create :terminal terminal :panel sidebar)))
               (unwind-protect
                    (progn
                      (localgroup-screen-show-loading second "Channel" :start-p t)
                      (test-assert (not (localgroup-screen-panel-visible-p second))
                                   "the next fullscreen attachment keeps the sidebar hidden")
                      (localgroup-screen-toggle-panel second)
                      (test-assert (localgroup-screen-panel-visible-p second)
                                   "the next attachment can show the sidebar again"))
                 (localgroup-screen-close second)))
             (localgroup-screen-show-loading first "Channel" :start-p t)
             (test-assert (localgroup-screen-panel-visible-p first)
                          "a later fullscreen attachment uses the panel's updated visibility"))
        (localgroup-screen-close first))))
  nil)

(-> test-world-sidebar-layout () null)
(defun test-world-sidebar-layout ()
  "Test docking at 90 transcript columns and an explicit keyboard-accessible drawer below it."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (loop for (columns width) in '((170 37) (100 24))
            do (test-assert (= (screen-panel-width sidebar columns) width)
                            (format nil "beside a channel on ~D columns the sidebar takes ~D" columns width)))
      (let ((boundary (loop for columns from 1 to 200
                            when (= (- columns (screen-panel-width sidebar columns) 1) 90)
                              return columns)))
        (dolist (pane '(89 90))
          (let* ((columns  (+ boundary (- pane 90)))
                 (terminal (make-instance 'recording-terminal :columns columns :rows 20))
                 (screen   (localgroup-screen-create :terminal terminal :panel sidebar)))
            (unwind-protect
                 (progn
                   (localgroup-screen-show-loading screen "Channel" :start-p t)
                   (test-assert (and (= (- columns (screen-panel-width sidebar columns) 1) pane)
                                     (eq (localgroup-screen-layout screen)
                                         (if (= pane 90) ':beside ':session)))
                                (format nil "a prospective ~D-column transcript determines whether the sidebar docks" pane))
                   (localgroup-screen-toggle-panel screen)
                   (test-assert (eq (localgroup-screen-layout screen)
                                    (if (= pane 90) ':session ':alone))
                                "the toggle hides a docked sidebar or explicitly opens a compact drawer")
                   (localgroup-screen-toggle-panel screen))
              (localgroup-screen-close screen)))))
      (let* ((terminal (make-instance 'recording-terminal :columns 80 :rows 20))
             (screen   (localgroup-screen-create :terminal terminal :panel sidebar)))
        (flet ((channel (identifier)
                 "Return channel IDENTIFIER in one workspace."
                 (make-instance 'channel :identifier identifier :owner "user" :workspace "/w/"
                                         :created-at 1 :updated-at 1)))
          (setf (world-sidebar-current sidebar) "bhere1")
          (world-sidebar--take sidebar (list :channels (list (channel "bhere1") (channel "bthere1"))))
          (localgroup-screen-handle-packet
           screen '(:frame :height 20 :width 80 :complete-p t :rows ((19 "composer draft"))
                    :cursor-row 19 :cursor-column 14 :cursor-visible-p t))
          (test-assert (and (eq (localgroup-screen-layout screen) ':session)
                            (equal (multiple-value-list (localgroup-screen-pane-size screen)) '(20 80))
                            (eql (localgroup-screen-filter-event screen #\a) #\a))
                       "a narrow attachment shows the composer and forwards normal typing")
          (localgroup-screen-toggle-panel screen)
          (test-assert (and (eq (localgroup-screen-layout screen) ':alone)
                            (not (localgroup-screen-take-resize screen))
                            (eq (localgroup-screen-filter-event screen #\a) ':ignore))
                       "an explicit drawer takes keyboard focus without resizing the hidden channel")
          (test-assert (and (eq (localgroup-screen-filter-event screen ':escape) ':ignore)
                            (eq (localgroup-screen-layout screen) ':session))
                       "Escape closes the drawer and returns focus to the composer")
          (localgroup-screen-toggle-panel screen)
          (test-assert (and (eq (localgroup-screen-filter-event screen '(:click 10 4)) ':ignore)
                            (eq (localgroup-screen-layout screen) ':session))
                       "clicking the current channel dismisses the drawer")
          (localgroup-screen-toggle-panel screen)
          (localgroup-screen-filter-event screen ':down)
          (test-assert (and (equal (localgroup-screen-filter-event screen ':submit)
                                   '(:switch :session-id "bthere1"))
                            (eq (localgroup-screen-layout screen) ':session))
                       "arrows and Enter select another channel and dismiss the drawer")
          (localgroup-screen-close screen)))))
  nil)

(-> test-world-sidebar-scrolling () null)
(defun test-world-sidebar-scrolling ()
  "Test scroll navigation, visible hit targets, fixed commands and resize/refresh clamping."
  (with-test-configuration (configuration)
    (let* ((sidebar  (world-sidebar-create configuration))
           (terminal (make-instance 'recording-terminal :columns 80 :rows 9))
           (entries  (loop for index below 12
                           collect (list :kind ':channel :id (format nil "channel-~D" index)
                                        :depth 0 :state ':idle :unread 0 :mentions 0 :pending 0
                                        :children-p (= index 4) :open-p t)))
           (screen   (localgroup-screen-create :terminal terminal :panel sidebar)))
      (setf (world-sidebar-entries sidebar) entries)
      (localgroup-screen-handle-packet
       screen '(:frame :height 9 :width 80 :complete-p t :rows ((8 "draft"))
                       :cursor-row 8 :cursor-column 5 :cursor-visible-p t))
      (localgroup-screen-toggle-panel screen)
      (localgroup-screen-filter-event screen '(:scroll 1))
      (test-assert (and (= (world-sidebar-scroll-offset sidebar) 3)
                        (= (world-sidebar-selected-row sidebar) 3))
                   "the first wheel tick scrolls the viewport and reveals its selection")
      (localgroup-screen-filter-event screen ':home)
      (localgroup-screen-filter-event screen ':page-down)
      (test-assert (and (= (world-sidebar-selected-row sidebar) 5)
                        (= (world-sidebar-scroll-offset sidebar) 1)
                        (equal (screen-panel-click sidebar 12 7) '(:switch :session-id "channel-5")))
                   "paging reveals the selected channel and visible clicks use scrolled identities")
      (let ((toggled nil))
        (test-call-with-function-replacements
         (list (list 'world-sidebar--toggle
                     (lambda (candidate key)
                       (test-assert (eq candidate sidebar) "toggle belongs to this sidebar")
                       (setf toggled key))))
         (lambda ()
           (test-assert (eq (screen-panel-click sidebar 2 6) ':redraw)
                        "a marker click after scrolling redraws the tree")))
        (test-assert (equal toggled "channel-4")
                     "scrolled marker hit testing toggles the visible channel, not its old row"))
      (localgroup-screen-filter-event screen '(:scroll 1))
      (test-assert (= (world-sidebar-scroll-offset sidebar) 4)
                   "wheel events in the drawer scroll the selected entry into view")
      (test-assert (and (eq (screen-panel-click sidebar 14 1) ':redraw)
                        (= (world-sidebar-scroll-offset sidebar) 7)
                        (eq (screen-panel-click sidebar 12 1) ':redraw)
                        (= (world-sidebar-scroll-offset sidebar) 4))
                   "heading controls scroll the tree while retaining the footer geometry")
      (test-assert (and (equal (screen-panel-click sidebar 4 8) '(:application-command "(new)"))
                        (equal (screen-panel-click sidebar 12 8) '(:application-command "(delete)"))
                        (every (lambda (column) (null (screen-panel-click sidebar column 8)))
                               '(1 7 8 17)))
                   "each inline command has its own hit target, excluding surrounding spaces")
      (localgroup-screen-filter-event screen ':end)
      (test-assert (and (equal (localgroup-screen-filter-event screen ':submit)
                               '(:application-command "(delete)"))
                        (eq (localgroup-screen-layout screen) ':session))
                   "End and Enter invoke the delete action and dismiss the drawer")
      (setf (world-sidebar-selected-row sidebar) 9)
      (screen-panel-rows sidebar terminal :height 7 :width 80)
      (test-assert (= (world-sidebar-scroll-offset sidebar) 7)
                   "a smaller viewport reveals the selected entry")
      (screen-panel-rows sidebar terminal :height 20 :width 80)
      (test-assert (zerop (world-sidebar-scroll-offset sidebar))
                   "a larger viewport clamps scrolling to the available entries")
      (setf (world-sidebar-entries sidebar) (subseq entries 0 2))
      (screen-panel-rows sidebar terminal :height 9 :width 80)
      (test-assert (and (= (world-sidebar-selected-row sidebar) 3)
                        (zerop (world-sidebar-scroll-offset sidebar)))
                   "refreshed entries clamp both logical selection and scrolling")
      (let ((refreshed (subseq entries 0 3)))
        (test-call-with-function-replacements
         (list (list 'world-sidebar--entries
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       refreshed)))
         (lambda ()
           (setf (world-sidebar-entries sidebar) refreshed
                 (world-sidebar-selected-row sidebar) 3
                 refreshed (subseq entries 0 2))
           (world-sidebar--take sidebar nil)
           (test-assert (equal (screen-panel-event sidebar ':submit) '(:application-command "(new)"))
                        "refresh preserves the new button when the tree shrinks")
           (setf (world-sidebar-selected-row sidebar) 3
                 refreshed (subseq entries 0 3))
           (world-sidebar--take sidebar nil)
           (test-assert (equal (screen-panel-event sidebar ':submit) '(:application-command "(delete)"))
                        "refresh preserves the delete button when the tree grows")
           (setf (world-sidebar-selected-row sidebar) 1
                 refreshed (list (third entries) (first entries) (second entries)))
           (world-sidebar--take sidebar nil)
           (test-assert (and (= (world-sidebar-selected-row sidebar) 2)
                             (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "channel-1")))
                        "reordered channels retain the selected channel identity")
           (let ((workspace (list :kind ':workspace :key "/workspace/" :open-p t)))
             (setf (world-sidebar-entries sidebar) (list workspace (first entries))
                   (world-sidebar-selected-row sidebar) 0
                   refreshed (list (first entries) workspace))
             (world-sidebar--take sidebar nil)
             (test-assert (= (world-sidebar-selected-row sidebar) 1)
                          "reordered workspace rows retain their key identity"))
           (setf (world-sidebar-entries sidebar) (subseq entries 0 3)
                 (world-sidebar-selected-row sidebar) 2
                 refreshed (list (first entries)))
           (world-sidebar--take sidebar nil)
           (test-assert (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "channel-0"))
                        "a removed selection falls back to the first entry, not a command"))))
      (dolist (tree (list nil entries))
        (setf (world-sidebar-entries sidebar) tree)
        (dolist (height '(0 1 2))
          (screen-panel-rows sidebar terminal :height height :width 80)
          (dolist (event '(:end :home :down :up :page-down :page-up))
            (screen-panel-event sidebar event)
            (let ((result (screen-panel-event sidebar ':submit)))
              (test-assert
               (case height
                 (0
                  (null result))
                 ((1 2)
                  (member result '((:application-command "(new)")
                                   (:application-command "(delete)")) :test #'equal)))
               "tiny panels submit only command buttons rendered at their height")))))
      (localgroup-screen-close screen)))
  nil)

(-> test-world-sidebar-command-events () null)
(defun test-world-sidebar-command-events ()
  "Relay framed sidebar commands into the receiving controller's submission path."
  (with-test-configuration (configuration)
    (let* ((terminal    (localgroup-terminal-create))
           (ui          (terminal-ui-create :terminal terminal))
           (application (make-instance 'application :ui ui :configuration configuration))
           (controller  (application-input-controller-create application :start-reader-p nil))
           (packets     (with-output-to-string (stream)
                          (dolist (event '((:application-command "(quit)")
                                           (:application-command 7)
                                           (:application-command "(new)")
                                           (:application-command "(delete)")))
                            (daemon-write-packet stream (list :event event)))))
           (attachment  (make-instance 'image-daemon:attachment :socket nil :mode ':control
                                       :stream (make-two-way-stream
                                                (make-string-input-stream packets)
                                                (make-string-output-stream))))
           (submitted   nil))
      (image-daemon:relay-attach terminal attachment :rows 24 :columns 80 :styled-p nil
                                :session-id "sidebar-commands")
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'application-input-controller--handle-submission
                        (lambda (candidate text &key steer-p)
                          (test-assert (and (eq candidate controller) (not steer-p))
                                       "footer submission uses this terminal client's controller")
                          (push text submitted))))
            (lambda ()
              (image-daemon:relay-read-attachment terminal attachment
                                                :event-validator #'localgroup--terminal-event-p)
              (loop while (terminal-input-ready-p terminal)
                    do (application-input-controller--process-event controller
                                                                   (terminal-read-event terminal)))
              (test-assert (equal (reverse submitted) '("(new)" "(delete)"))
                           "only the two sidebar commands cross the relay into application submission")))
        (application-input-controller-stop controller)
        (image-daemon:relay-detach terminal attachment))))
  nil)

(-> test-world-sidebar-unread () null)
(defun test-world-sidebar-unread ()
  "Test the sidebar counts messages since this client last looked, per channel."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-tests--persisted-conversation configuration "bsecnd2" 2000)
    (world-server--reconcile configuration)
    (let* ((clock    0)
           (statuses nil)
           (sidebar  (world-sidebar-create configuration :clock-function (lambda () clock))))
      (labels ((status (identifier count mentions &key attached-p)
                 "Return a hosted status of channel IDENTIFIER with COUNT messages and MENTIONS."
                 (list :session-id identifier :state ':idle :terminal-attached-p attached-p
                       :message-count count :mention-count mentions))

               (entry (identifier)
                 "Return the sidebar's entry of channel IDENTIFIER."
                 (find identifier (world-sidebar-entries sidebar)
                       :key (lambda (entry) (getf entry :id)) :test #'equal))

               (advance ()
                 "Move the clock past the refresh interval."
                 (incf clock (* 2 *world-sidebar-refresh-seconds* internal-time-units-per-second))))
        (test-call-with-function-replacements
         (list (list 'world-sidebar--statuses
                     (lambda (candidate) (declare (ignore candidate)) (values statuses t))))
         (lambda ()
           (setf (world-sidebar-current sidebar) "bfirst1"
                 statuses (list (status "bfirst1" 4 1 :attached-p t) (status "bsecnd2" 7 0)))
           (test-assert (screen-panel-refresh sidebar)
                        "the first refresh gathers entries")
           (test-assert (and (equal (getf (entry "bfirst1") :state) ':idle)
                             (getf (entry "bfirst1") :attached-p)
                             (getf (entry "bfirst1") :current-p)
                             (zerop (getf (entry "bsecnd2") :unread))
                             (not (getf (entry "bsecnd2") :current-p)))
                        "a channel seen for the first time starts caught up")
           (test-assert (not (screen-panel-refresh sidebar))
                        "a refresh before the interval passes does nothing")
           (setf statuses (list (status "bfirst1" 6 2 :attached-p t) (status "bsecnd2" 9 1)))
           (advance)
           (test-assert (screen-panel-refresh sidebar)
                        "a refresh after the interval notices new messages")
           (test-assert (and (zerop (getf (entry "bfirst1") :unread))
                             (zerop (getf (entry "bfirst1") :mentions))
                             (= (getf (entry "bsecnd2") :unread) 2)
                             (= (getf (entry "bsecnd2") :mentions) 1))
                        "the current channel stays caught up while another counts what arrived")
           (setf (world-sidebar-current sidebar) "bsecnd2")
           (advance)
           (screen-panel-refresh sidebar)
           (test-assert (and (zerop (getf (entry "bsecnd2") :unread))
                             (getf (entry "bsecnd2") :current-p))
                        "looking at a channel catches it up")
           (setf statuses (list (status "bsecnd2" 9 1)))
           (world-inbox-append configuration "bfirst1"
                               (list (make-instance 'world-delivery
                                                    :identifier  "d1"
                                                    :time        3000
                                                    :sender      "bsecnd2"
                                                    :sender-name "autolith"
                                                    :source      "bsecnd2"
                                                    :target      "bfirst1"
                                                    :text        "waiting"
                                                    :mentions    nil
                                                    :kind        ':post)))
           (advance)
           (screen-panel-refresh sidebar)
           (test-assert (and (equal (getf (entry "bfirst1") :state) ':absent)
                             (= (getf (entry "bfirst1") :pending) 1)
                             (zerop (getf (entry "bfirst1") :unread)))
                        "a channel nobody serves shows its waiting deliveries instead of counts")
           (let ((inbox (world-tests--inbox configuration)))
             (world-inbox-append configuration inbox
                                 (list (make-instance 'world-delivery
                                                      :identifier  "d2"
                                                      :time        3000
                                                      :sender      "bsecnd2"
                                                      :sender-name "autolith"
                                                      :source      "bsecnd2"
                                                      :target      "bsecnd2"
                                                      :text        "@user look"
                                                      :mentions    nil
                                                      :kind        ':notice)))
             (advance)
             (screen-panel-refresh sidebar)
             (let ((first (first (world-sidebar-entries sidebar))))
               (test-assert (and (eq (getf first :kind) ':inbox)
                                 (equal (getf first :id) inbox)
                                 (= (getf first :unread) 1)
                                 (= 1 (count inbox (world-sidebar-entries sidebar)
                                             :key (lambda (entry) (getf entry :id)) :test #'equal)))
                            "the inbox comes first, once, counting the messages waiting there"))))))))
  nil)

(-> test-world-sidebar-finds-conversations-once () null)
(defun test-world-sidebar-finds-conversations-once ()
  "Test repeated sidebar gathers share active storage discovery across task channels."
  (with-test-configuration (configuration)
   (world-tests--persisted-conversation configuration "bfirst1" 1000)
   (world-tests--child-conversation configuration "bcachedkid" :root "bfirst1" :parent "bfirst1"
    :name "cached" :created-at 1500)
   (world-tests--child-conversation configuration "bcachedtwo" :root "bfirst1" :parent "bfirst1"
    :name "cachedtwo" :created-at 1600)
   (world-server--reconcile configuration)
   (let ((sidebar        (world-sidebar-create configuration))
         (subdirectories (symbol-function 'uiop/filesystem:subdirectories))
         (root           (merge-pathnames "tasks/" (config :data-root configuration)))
         (scans          0))
     (test-call-with-function-replacements
      (list (list 'world-sidebar--statuses (lambda (candidate) (declare (ignore candidate)) (values nil t)))
            (list 'uiop/filesystem:subdirectories
                  (lambda (directory)
                    (when (equal directory root) (incf scans))
                    (funcall subdirectories directory))))
      (lambda () (dotimes (index 3) (world-sidebar--gather sidebar))))
     (test-assert (= scans 1)
      "one batch scans task roots once across repeated sidebar refreshes")))
  nil)

(-> test-world-sidebar-gathers-apart () null)
(defun test-world-sidebar-gathers-apart ()
  "Test a started sidebar refreshes without waiting for a slow gather, then shows what it gathered."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration))
          (gate    (sb-thread:make-semaphore))
          (inputs  (list :channels (list (make-instance 'channel :identifier "bwork1" :owner "user"
                                                                 :workspace "/w/" :created-at 1 :updated-at 1)))))
      (test-call-with-function-replacements
       (list (list 'world-sidebar--gather
                   (lambda (panel)
                     (declare (ignore panel))
                     (sb-thread:wait-on-semaphore gate)
                     inputs)))
       (lambda ()
         (world-sidebar-start sidebar)
         (unwind-protect
              (let ((start (get-internal-real-time)))
                (test-assert (and (null (screen-panel-refresh sidebar))
                                  (< (- (get-internal-real-time) start)
                                     (* 1/20 internal-time-units-per-second)))
                             "a refresh while the world is slow to read returns at once")
                (sb-thread:signal-semaphore gate 1000)
                (test-assert (and (task-tests--wait-until (lambda () (screen-panel-refresh sidebar)) 5)
                                  (find "bwork1" (world-sidebar-entries sidebar)
                                        :key (lambda (entry) (getf entry :id)) :test #'equal))
                             "the entries gathered meanwhile show on a later refresh"))
           (sb-thread:signal-semaphore gate 1000)
           (world-sidebar-stop sidebar)))))
    nil))
