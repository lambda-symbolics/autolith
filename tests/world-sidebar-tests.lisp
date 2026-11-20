(in-package #:autolith)

;;;; -- World Sidebar Tests --

(defparameter *world-sidebar-tests-width* 30
  "The sidebar width the row tests render at.")

(-> world-sidebar-tests--channel (string string &key (:parent (option string)) (:title (option string))) channel)
(defun world-sidebar-tests--channel (identifier workspace &key parent title)
  "Return channel IDENTIFIER in WORKSPACE, nested under PARENT, titled TITLE."
  (make-instance 'channel :identifier identifier :owner "user" :workspace workspace
                          :parent parent :title title :created-at 1 :updated-at 1))

(-> world-sidebar-tests--lines (world-sidebar &key (:height integer) (:focused-p boolean)) list)
(defun world-sidebar-tests--lines (sidebar &key (height 16) focused-p)
  "Return SIDEBAR rendered at the test width as plain text lines."
  (let ((terminal (make-instance 'recording-terminal :columns 120 :rows 24)))
    (mapcar #'clinedi:ansi-strip
            (screen-panel-rows sidebar terminal :height height :width *world-sidebar-tests-width*
                                                :focused-p focused-p))))

(-> world-sidebar-tests--keys (world-sidebar) list)
(defun world-sidebar-tests--keys (sidebar)
  "Return the channel identifiers and workspace keys of SIDEBAR's entries, in order."
  (mapcar (lambda (entry) (second (sidebar-entry-key entry)))
          (world-sidebar-entries sidebar)))

(-> world-sidebar-tests--entry (world-sidebar string) (option sidebar-entry))
(defun world-sidebar-tests--entry (sidebar identifier)
  "Return SIDEBAR's entry of channel IDENTIFIER."
  (find identifier (world-sidebar-entries sidebar)
        :key (lambda (entry) (second (sidebar-entry-key entry))) :test #'equal))

(-> test-world-sidebar-rows () null)
(defun test-world-sidebar-rows ()
  "Test the rows name agents on one line and titles on the next, and never spell out an identifier."
  (with-test-configuration (configuration)
    (let ((sidebar  (world-sidebar-create configuration))
          (channels (list (world-sidebar-tests--channel "b6uouHJ" "/w/frob/" :title "Sidebar checks")
                          (world-sidebar-tests--channel "c6uouHJ" "/w/frob/" :parent "b6uouHJ" :title "task")
                          (world-sidebar-tests--channel "d6uouHJ" "/w/frob/" :title "Tools")
                          (world-sidebar-tests--channel "e6uouHJ" "/w/frob/")
                          (world-sidebar-tests--channel "f6uouHJ" "/srv/www/")))
          (names    '(("b6uouHJ" . "keeper") ("d6uouHJ" . "fixer"))))
      (flet ((refresh (messages mentions)
               "Refresh the sidebar with the fixer's MESSAGES and MENTIONS counts."
               (world-sidebar--take
                sidebar
                (list :inbox    (make-instance 'channel :identifier "h6uouHJ" :owner "user" :inbox-p t
                                                        :workspace "/w/" :created-at 1 :updated-at 1)
                      :channels channels
                      :names    names
                      :pending  '(("h6uouHJ" . 2) ("f6uouHJ" . 1))
                      :statuses (list (list :session-id "b6uouHJ" :state ':working
                                            :terminal-attached-p t)
                                      (list :session-id "d6uouHJ" :state ':idle
                                            :message-count messages :mention-count mentions))))))
        (setf (world-sidebar-current sidebar) "c6uouHJ")
        (refresh 4 0)
        (test-assert (refresh 7 2) "new messages change the entries")
        (test-assert (not (refresh 7 2)) "the same world leaves the entries as they are")
        (let ((lines (world-sidebar-tests--lines sidebar)))
          (test-assert (and (= (length lines) 16)
                            (every (lambda (line) (= (text-cell-width line) *world-sidebar-tests-width*))
                                   lines))
                       "the sidebar fills its height with rows of exactly its width")
          (test-assert (and (search "CHANNELS  c-6uouHJ" (first lines))
                            (notany (lambda (line) (search "6uouHJ" line)) (rest lines)))
                       "only the heading spells out an identifier, the current channel's")
          (test-assert (and (search " ▾ home" (third lines)) (search "2 unread" (third lines)))
                       "the home row, the open root of the tree, shows how many messages wait unread")
          (test-assert (and (search "▾ frob  /w/frob" (fourth lines))
                            (uiop:string-suffix-p (string-right-trim " " (fourth lines)) " 4"))
                       "an open workspace names its directory and counts its channels")
          (test-assert (and (search " ▾ ● keeper" (fifth lines))
                            (search "     Sidebar checks" (sixth lines)))
                       "a channel shows its working agent, with its title on a row of its own")
          (test-assert (and (search "▌        · task" (seventh lines))
                            (= 1 (count #\▌ (format nil "~{~A~}" lines))))
                       "the current channel, nested and without an agent, shows its title in the name's place behind the edge bar")
          (test-assert (and (search "○ fixer" (eighth lines))
                            (uiop:string-suffix-p (eighth lines) "@2 3")
                            (search "     Tools" (ninth lines)))
                       "a channel counts the mentions and messages that arrived")
          (test-assert (search "· untitled" (tenth lines))
                       "a channel without a name or a title says so instead of showing its identifier")
          (test-assert (search "▸ www" (nth 10 lines))
                       "a closed workspace hides its channels")
          (test-assert (and (search "(new)" (nth 14 lines)) (search "Ctrl-Space focus" (nth 15 lines)))
                       "the command buttons and the way into the sidebar close the panel"))
        (let* ((terminal (make-instance 'recording-terminal :columns 120 :rows 24))
               (rows     (screen-panel-rows sidebar terminal :height 16 :width 30)))
          (refresh 7 2)
          (test-assert (eq rows (screen-panel-rows sidebar terminal :height 16 :width 30))
                       "a frame beside an unchanged sidebar reuses its rendered rows")
          (test-assert (not (eq rows (screen-panel-rows sidebar terminal :height 16 :width 30
                                                                        :focused-p t)))
                       "taking the focus renders the rows again")
          (refresh 9 2)
          (test-assert (search "@2 5" (clinedi:ansi-strip
                                       (eighth (screen-panel-rows sidebar terminal
                                                                  :height 16 :width 30))))
                       "a changed channel renders the rows again"))
        (world-sidebar--toggle sidebar (world-sidebar-tests--entry sidebar "b6uouHJ"))
        (test-assert (search "1 nested" (fifth (world-sidebar-tests--lines sidebar)))
                     "a closed channel says how many channels nest below it")
        (loop for height from 0 to 5
              do (test-assert (= height (length (world-sidebar-tests--lines sidebar :height height)))
                              "a short sidebar still fills exactly its height")))
      (dolist (case '((:idle t :notice "● ")
                      (:working t :notice "● ")
                      (:active nil :agent-spinner "● ")
                      (:idle nil :success "○ ")
                      (:absent nil :dim "· ")))
        (destructuring-bind (state waiting-p style glyph) case
          (test-assert (equal (world-sidebar--state-span state waiting-p) (terminal-span style glyph))
                       (format nil "state ~S, waiting for jobs ~S, has the right indicator"
                               state waiting-p))))))
  nil)

(-> test-world-sidebar-home () null)
(defun test-world-sidebar-home ()
  "Test home folds like any other node of the tree, and the edge bar marks every row of the current channel."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (flet ((click (column row)
               "Render the sidebar, then click it at COLUMN and ROW."
               (world-sidebar-tests--lines sidebar)
               (screen-panel-click sidebar column row))

             (refresh (messages mentions)
               "Refresh the sidebar with the far channel's MESSAGES and MENTIONS counts."
               (world-sidebar--take
                sidebar
                (list :inbox    (make-instance 'channel :identifier "h6uouHJ" :owner "user" :inbox-p t
                                                        :workspace "/w/" :created-at 1 :updated-at 1)
                      :channels (list (world-sidebar-tests--channel "b6uouHJ" "/w/frob/"
                                                                    :title "Sidebar checks")
                                      (world-sidebar-tests--channel "c6uouHJ" "/srv/www/"))
                      :names    '(("b6uouHJ" . "keeper"))
                      :pending  '(("h6uouHJ" . 2))
                      :statuses (list (list :session-id "c6uouHJ" :state ':idle
                                            :message-count messages :mention-count mentions))))))
        (setf (world-sidebar-current sidebar) "b6uouHJ")
        (refresh 0 0)
        (refresh 3 1)
        (let ((lines (world-sidebar-tests--lines sidebar)))
          (test-assert (and (equal (world-sidebar-tests--keys sidebar)
                                   '("h6uouHJ" "/w/frob/" "b6uouHJ" "/srv/www/"))
                            (search " ▾ home" (third lines))
                            (search "   ▾ frob" (fourth lines)))
                       "home is the open root and the workspaces nest below it")
          (test-assert (and (search "▌      · keeper" (fifth lines))
                            (search "▌        Sidebar checks" (sixth lines))
                            (= 2 (count #\▌ (format nil "~{~A~}" lines))))
                       "the edge bar runs down both rows of the current channel and nowhere else"))
        (test-assert (eq (click 2 3) ':redraw) "a click on home's marker closes the tree")
        (let ((line (third (world-sidebar-tests--lines sidebar))))
          (test-assert (and (equal (world-sidebar-tests--keys sidebar) '("h6uouHJ"))
                            (search " ▸ home" line)
                            (search "@1" line)
                            (search "5 unread" line)
                            (search "2 nested" line))
                       "a closed home hides every workspace and carries what is unread below it"))
        (test-assert (equal (click 6 3) '(:switch :session-id "h6uouHJ"))
                     "a click on home's name asks to switch to the inbox")
        (screen-panel-focus sidebar)
        (setf (world-sidebar-selected sidebar) 0)
        (test-assert (and (eq (screen-panel-event sidebar ':right) ':redraw)
                          (= 4 (length (world-sidebar-entries sidebar)))
                          (eq (screen-panel-event sidebar ':left) ':redraw)
                          (= 1 (length (world-sidebar-entries sidebar))))
                     "Right and Left open and close home from the keyboard")
        (test-assert (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "h6uouHJ"))
                     "Enter on home opens the inbox"))))
  nil)

(-> test-world-sidebar-tree () null)
(defun test-world-sidebar-tree ()
  "Test the sidebar opens only the way to the current channel, and clicks open, close and switch."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (flet ((click (column row)
               "Render the sidebar, then click it at COLUMN and ROW."
               (world-sidebar-tests--lines sidebar :height 20)
               (screen-panel-click sidebar column row)))
        (setf (world-sidebar-current sidebar) "bkid1")
        (world-sidebar--take
         sidebar
         (list :channels (list (world-sidebar-tests--channel "broot1" "/w/a/")
                               (world-sidebar-tests--channel "bkid1" "/w/a/" :parent "broot1")
                               (world-sidebar-tests--channel "bkid2" "/w/a/" :parent "broot1")
                               (world-sidebar-tests--channel "bfar1" "/w/b/")
                               (world-sidebar-tests--channel "bfar2" "/w/b/" :parent "bfar1"))
               :names    '(("bkid1" . "helper"))))
        (test-assert (equal (world-sidebar-tests--keys sidebar) '("/w/a/" "broot1" "bkid1" "bkid2" "/w/b/"))
                     "the current channel's workspace and parent are open; other workspaces are closed")
        (test-assert (equal (click 2 4) ':redraw)
                     "a click on a channel's marker closes it")
        (test-assert (equal (world-sidebar-tests--keys sidebar) '("/w/a/" "broot1" "/w/b/"))
                     "a closed channel hides its nested channels")
        (let* ((tree    (world-sidebar-tree sidebar))
               (child   (find "bkid1" tree :key  (lambda (entry) (second (sidebar-entry-key entry)))
                                           :test #'equal))
               (distant (first (last tree))))
          (test-assert (equal (mapcar (lambda (entry) (second (sidebar-entry-key entry))) tree)
                              '("/w/a/" "broot1" "bkid1" "bkid2" "/w/b/" "bfar1" "bfar2"))
                       "the complete model includes descendants of closed channels and workspaces")
          (test-assert (and (equal (sidebar-channel-parent child) "broot1")
                            (equal (sidebar-entry-name child) "helper")
                            (eq (sidebar-channel-state child) ':absent)
                            (equal (sidebar-channel-parent distant) "bfar1"))
                       "complete channel entries retain durable parent IDs, agent names and state"))
        (click 2 5)
        (test-assert (equal (world-sidebar-tests--keys sidebar) '("/w/a/" "broot1" "/w/b/" "bfar1"))
                     "a click on a closed workspace opens it, its own nests still closed")
        (test-assert (equal (click 10 6) '(:switch :session-id "bfar1"))
                     "a click on another channel's name asks to switch to it")
        (click 2 6)
        (setf (world-sidebar-current sidebar) "bfar2")
        (world-sidebar--take sidebar (world-sidebar-inputs sidebar))
        (test-assert (equal (world-sidebar-tests--keys sidebar) '("/w/b/" "bfar1" "bfar2" "/w/a/"))
                     "a node the person opened stays open when the terminal moves into it")
        (test-assert (eq (click 10 5) ':dismiss)
                     "a click on the current channel asks to return to its composer")
        (test-assert (eq (click 10 1) ':redraw)
                     "a click on the heading only takes the focus"))))
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration))
          (root    (make-instance 'channel :identifier "repo-root" :owner "user"
                                           :workspace "/repo/" :created-at 1 :updated-at 1
                                           :repository-key "git:/repo/.git/" :repository-directory "/repo/"))
          (child   (make-instance 'channel :identifier "repo-child" :owner "user"
                                           :workspace "/isolated/worktree/" :parent "repo-root"
                                           :repository-key "git:/repo/.git/" :repository-directory "/repo/"
                                           :created-at 1 :updated-at 1)))
      (let ((group (first
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
           (test-assert (equal (world-sidebar-tests--keys sidebar)
                               '("git:/repo/.git/" "repo-root" "repo-child"))
                        "precomputed repository groups nest a child from a distinct worktree")
           (test-assert (= 1 (sidebar-channel-depth (third (world-sidebar-entries sidebar))))
                        "the isolated child nests beneath its actual parent")
           (world-sidebar--toggle sidebar (first (world-sidebar-entries sidebar)))
           (test-assert (= 1 (length (world-sidebar-entries sidebar)))
                        "folding a resolved repository uses the supplied snapshot without discovery")
           (test-assert (and (string= (channel-workspace root) "/repo/")
                             (string= (channel-workspace child) "/isolated/worktree/"))
                        "presentation grouping preserves both actual execution directories"))))))
  nil)

(-> test-world-sidebar-keys () null)
(defun test-world-sidebar-keys ()
  "Test a focused sidebar moves, folds, scrolls and acts on its entries with the keyboard and the wheel."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (setf (world-sidebar-current sidebar) "bchan03")
      (world-sidebar--take
       sidebar
       (list :channels (loop for index below 12
                             collect (world-sidebar-tests--channel
                                      (format nil "bchan~2,'0D" index) "/w/"
                                      :title (and (= index 3) "Titled")))
             :names    '(("bchan03" . "third"))))
      (flet ((selected ()
               "Return the selected entry's key."
               (second (sidebar-entry-key (nth (world-sidebar-selected sidebar)
                                               (world-sidebar-entries sidebar)))))
             (rendered ()
               "Return a nine-row rendering, five of them the tree."
               (world-sidebar-tests--lines sidebar :height 9 :focused-p t)))
        (screen-panel-focus sidebar)
        (test-assert (equal (selected) "bchan03") "taking the focus selects the current channel")
        (test-assert (and (search "third" (sixth (rendered))) (search "Titled" (seventh (rendered))))
                     "both rows of the selected channel are scrolled into view")
        (test-assert (and (eq (screen-panel-event sidebar ':end) ':redraw)
                          (equal (selected) "bchan11")
                          (search "untitled" (seventh (rendered))))
                     "End selects the last entry and scrolls it into view")
        (test-assert (and (eq (screen-panel-event sidebar ':down) ':redraw)
                          (equal (selected) "/w/"))
                     "moving past the last entry wraps to the first")
        (screen-panel-event sidebar ':end)
        (rendered)
        (test-assert (and (eq (screen-panel-event sidebar '(:scroll -1)) ':redraw)
                          (< (world-sidebar-selected sidebar) 12)
                          (search (format nil "~D" (world-sidebar-selected sidebar)) "1234567891011"))
                     "the wheel scrolls the tree and keeps the selection among the rows that show")
        (screen-panel-event sidebar ':home)
        (test-assert (and (eq (screen-panel-event sidebar ':left) ':redraw)
                          (equal (world-sidebar-tests--keys sidebar) '("/w/"))
                          (eq (screen-panel-event sidebar ':right) ':redraw)
                          (= 13 (length (world-sidebar-entries sidebar))))
                     "Left closes the selected node and Right opens it again")
        (screen-panel-event sidebar ':down)
        (test-assert (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "bchan00"))
                     "Enter opens the selected channel")
        (test-assert (equal (screen-panel-event sidebar ':delete) '(:channel-command :delete "bchan00"))
                     "Delete asks to delete the selected channel")
        (screen-panel-event sidebar ':home)
        (test-assert (null (screen-panel-event sidebar ':delete))
                     "Delete on a workspace does nothing")
        (rendered)
        (test-assert (and (equal (screen-panel-click sidebar 3 8) '(:channel-command :new))
                          (equal (screen-panel-click sidebar 12 8) '(:channel-command :delete))
                          (null (screen-panel-click sidebar 8 8)))
                     "the footer buttons ask for a new channel or to delete the current one")
        (world-sidebar--take sidebar (list :channels (list (world-sidebar-tests--channel "bchan00" "/w/"))))
        (rendered)
        (test-assert (and (< (world-sidebar-selected sidebar) 2)
                          (zerop (world-sidebar-scroll-offset sidebar)))
                     "a refresh with fewer entries clamps the selection and the scroll offset"))))
  (dolist (case '(((:channel-command :new) "(new)")
                  ((:channel-command :open) "(open)")
                  ((:channel-command :delete) "(delete)")
                  ((:channel-command :delete "b6uouHJ") "(delete \"b6uouHJ\")")
                  ((:channel-command :quit) nil)
                  ((:channel-command :new "b6uouHJ") nil)
                  ((:channel-command :delete "no such id") nil)
                  ((:channel-command :delete "b6uouHJ" "c6uouHJ") nil)
                  ((:application-command "(new)") nil)
                  (:new nil)))
    (test-assert (equal (channel-command-text (first case)) (second case))
                 (format nil "~S submits ~S" (first case) (second case))))
  nil)

(-> test-world-sidebar-filter () null)
(defun test-world-sidebar-filter ()
  "Test typing narrows a focused sidebar to the channels a name, identifier, title or path matches."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (setf (world-sidebar-current sidebar) "b6uouHJ")
      (world-sidebar--take
       sidebar
       (list :channels (list (world-sidebar-tests--channel "b6uouHJ" "/w/frob/" :title "Sidebar checks")
                             (world-sidebar-tests--channel "c6uouHJ" "/srv/www/" :title "Landing page")
                             (world-sidebar-tests--channel "d6uouHJ" "/srv/www/" :title "Marta's review"))
             :names    '(("b6uouHJ" . "keeper") ("c6uouHJ" . "marta") ("d6uouHJ" . "otto"))))
      (flet ((enter (text)
               "Type TEXT into the focused sidebar."
               (screen-panel-event sidebar (list ':insert text))))
        (test-assert (equal (world-sidebar-tests--keys sidebar) '("/w/frob/" "b6uouHJ" "/srv/www/"))
                     "without a filter the other workspace is closed")
        (test-assert (and (eq (enter "marta") ':redraw)
                          (equal (world-sidebar-tests--keys sidebar) '("c6uouHJ" "d6uouHJ")))
                     "a filter finds channels in closed groups, a name before a title")
        (let ((lines (world-sidebar-tests--lines sidebar :focused-p t)))
          (test-assert (search "/ marta" (second lines)) "the filter shows under the heading")
          (test-assert (and (search "marta Landing page" (third lines))
                            (search "c-6uouHJ  /srv/www" (fourth lines)))
                       "a match shows its title beside its name, then its identifier and workspace"))
        (screen-panel-event sidebar ':kill-line)
        (test-assert (progn (enter "www check") (null (world-sidebar-entries sidebar)))
                     "every word must match one channel")
        (screen-panel-event sidebar ':kill-line)
        (test-assert (progn (enter "d6u") (equal (world-sidebar-tests--keys sidebar) '("d6uouHJ")))
                     "an identifier fragment matches")
        (test-assert (and (eq (screen-panel-event sidebar ':backspace) ':redraw)
                          (string= (world-sidebar-filter sidebar) "d6"))
                     "Backspace shortens the filter")
        (test-assert (and (eq (screen-panel-event sidebar ':escape) ':redraw)
                          (string= (world-sidebar-filter sidebar) "")
                          (= 3 (length (world-sidebar-entries sidebar)))
                          (eq (screen-panel-event sidebar ':escape) ':dismiss))
                     "Escape clears the filter first and leaves the sidebar next")
        (enter "otto")
        (test-assert (and (equal (screen-panel-event sidebar ':submit) '(:switch :session-id "d6uouHJ"))
                          (string= (world-sidebar-filter sidebar) ""))
                     "opening a match drops the filter that found it")
        (enter (make-string 200 :initial-element #\x))
        (test-assert (= (length (world-sidebar-filter sidebar)) *world-sidebar-filter-character-limit*)
                     "the filter is bounded"))))
  nil)

(-> test-world-sidebar-visibility () null)
(defun test-world-sidebar-visibility ()
  "Test sidebar visibility follows a terminal's successive fullscreen attachments."
  (with-test-configuration (configuration)
    (let* ((sidebar  (world-sidebar-create configuration))
           (terminal (make-instance 'recording-terminal :columns 120 :rows 30))
           (first    (world-terminal-screen-create :terminal terminal :panel sidebar)))
      (unwind-protect
           (progn
             (world-terminal-screen-show-loading first "Channel" :start-p t)
             (test-assert (world-terminal-screen-panel-visible-p first)
                          "a wide fullscreen terminal shows the sidebar at first")
             (test-assert (eq (world-terminal-screen-filter-event first *world-terminal-screen-toggle-event*)
                              ':ignore)
                          "the toggle key is consumed by the screen")
             (test-assert (not (world-terminal-screen-panel-visible-p first))
                          "the toggle hides the sidebar")
             (world-terminal-screen-close first)
             (let ((second (world-terminal-screen-create :terminal terminal :panel sidebar)))
               (unwind-protect
                    (progn
                      (world-terminal-screen-show-loading second "Channel" :start-p t)
                      (test-assert (not (world-terminal-screen-panel-visible-p second))
                                   "the next fullscreen attachment keeps the sidebar hidden")
                      (world-terminal-screen-toggle-panel second)
                      (test-assert (world-terminal-screen-panel-visible-p second)
                                   "the next attachment can show the sidebar again"))
                 (world-terminal-screen-close second)))
             (world-terminal-screen-show-loading first "Channel" :start-p t)
             (test-assert (world-terminal-screen-panel-visible-p first)
                          "a later fullscreen attachment uses the panel's updated visibility"))
        (world-terminal-screen-close first))))
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
                 (screen   (world-terminal-screen-create :terminal terminal :panel sidebar)))
            (unwind-protect
                 (progn
                   (world-terminal-screen-show-loading screen "Channel" :start-p t)
                   (test-assert (and (= (- columns (screen-panel-width sidebar columns) 1) pane)
                                     (eq (world-terminal-screen-layout screen)
                                         (if (= pane 90) ':beside ':session)))
                                (format nil "a prospective ~D-column transcript determines whether the sidebar docks" pane))
                   (world-terminal-screen-toggle-panel screen)
                   (test-assert (eq (world-terminal-screen-layout screen)
                                    (if (= pane 90) ':session ':alone))
                                "the toggle hides a docked sidebar or explicitly opens a compact drawer")
                   (world-terminal-screen-toggle-panel screen))
              (world-terminal-screen-close screen)))))
      (let* ((terminal (make-instance 'recording-terminal :columns 80 :rows 20))
             (screen   (world-terminal-screen-create :terminal terminal :panel sidebar)))
        (progn
          (setf (world-sidebar-current sidebar) "bhere1")
          (world-sidebar--take sidebar (list :channels (list (world-sidebar-tests--channel "bhere1" "/w/")
                                                             (world-sidebar-tests--channel "bthere1" "/w/"))))
          (test-world-terminal--show-rows screen
                                      (append (make-list 19 :initial-element "") '("composer draft"))
                                      :height 20 :width 80 :cursor-row 19 :cursor-column 14)
          (test-assert (and (eq (world-terminal-screen-layout screen) ':session)
                            (equal (multiple-value-list (world-terminal-screen-pane-size screen)) '(20 80))
                            (eql (world-terminal-screen-filter-event screen #\a) #\a))
                       "a narrow attachment shows the composer and forwards normal typing")
          (world-terminal-screen-toggle-panel screen)
          (test-assert (and (eq (world-terminal-screen-layout screen) ':alone)
                            (not (world-terminal-screen-take-resize screen))
                            (eq (world-terminal-screen-filter-event screen #\a) ':ignore))
                       "an explicit drawer takes keyboard focus without resizing the hidden channel")
          (test-assert (and (eq (world-terminal-screen-filter-event screen ':escape) ':ignore)
                            (eq (world-terminal-screen-layout screen) ':session))
                       "Escape closes the drawer and returns focus to the composer")
          (world-terminal-screen-toggle-panel screen)
          (test-assert (and (eq (world-terminal-screen-filter-event screen '(:click 10 4)) ':ignore)
                            (eq (world-terminal-screen-layout screen) ':session))
                       "clicking the current channel dismisses the drawer")
          (world-terminal-screen-toggle-panel screen)
          (world-terminal-screen-filter-event screen ':down)
          (test-assert (and (equal (world-terminal-screen-filter-event screen ':submit)
                                   '(:switch :session-id "bthere1"))
                            (eq (world-terminal-screen-layout screen) ':session))
                       "arrows and Enter select another channel and dismiss the drawer")
          (world-terminal-screen-close screen)))))
  nil)

(-> test-world-sidebar-command-events () null)
(defun test-world-sidebar-command-events ()
  "Send the side panel's channel commands from a terminal client into its channel's submission path."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((identifier (getf (rest (world-server-tests--open
                                     (world-client-entry configuration)
                                     (world-tests--workspace root "sidebar-commands")))
                              :session-id))
            (controller (application-input-controller
                         (world-host-application (world-server--find-host server identifier))))
            (handle     (symbol-function 'application-input-controller--handle-submission))
            (lock       (make-lock "sidebar command test"))
            (submitted  nil))
       (flet ((submissions ()
                "Return what the channel's controller was asked to submit, oldest first."
                (with-lock-held (lock)
                  (reverse submitted))))
         (test-call-with-function-replacements
          (list (list 'application-input-controller--handle-submission
                      (lambda (candidate text &key steer-p)
                        (if (eq candidate controller)
                            (with-lock-held (lock)
                              (push (list text steer-p) submitted))
                            (funcall handle candidate text :steer-p steer-p)))))
          (lambda ()
            (world-server-tests--call-with-terminal-client
             configuration identifier
             (lambda (type await-exit view)
               (declare (ignore await-exit view))
               (dolist (event '((:channel-command :quit)
                                (:application-command "(new)")
                                (:channel-command :new)
                                (:channel-command :delete "b6uouHJ")))
                 (funcall type event))
               (test-assert (task-tests--wait-until (lambda () (= 2 (length (submissions)))) 10)
                            "the side panel's commands reach the channel this terminal client shows")
               (test-assert (equal (submissions) '(("(new)" nil) ("(delete \"b6uouHJ\")" nil)))
                            "only the side panel's channel commands become submissions, in order and without steering")))))))))
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
                 (world-sidebar-tests--entry sidebar identifier))

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
           (test-assert (and (eq (sidebar-channel-state (entry "bfirst1")) ':idle)
                             (sidebar-channel-attached-p (entry "bfirst1"))
                             (sidebar-entry-current-p (entry "bfirst1"))
                             (zerop (sidebar-entry-unread (entry "bsecnd2")))
                             (not (sidebar-entry-current-p (entry "bsecnd2"))))
                        "a channel seen for the first time starts caught up")
           (test-assert (not (screen-panel-refresh sidebar))
                        "a refresh before the interval passes does nothing")
           (setf statuses (list (status "bfirst1" 6 2 :attached-p t) (status "bsecnd2" 9 1)))
           (advance)
           (test-assert (screen-panel-refresh sidebar)
                        "a refresh after the interval notices new messages")
           (test-assert (and (zerop (sidebar-entry-unread (entry "bfirst1")))
                             (zerop (sidebar-channel-mentions (entry "bfirst1")))
                             (= (sidebar-entry-unread (entry "bsecnd2")) 2)
                             (= (sidebar-channel-mentions (entry "bsecnd2")) 1))
                        "the current channel stays caught up while another counts what arrived")
           (setf (world-sidebar-current sidebar) "bsecnd2")
           (advance)
           (screen-panel-refresh sidebar)
           (test-assert (and (zerop (sidebar-entry-unread (entry "bsecnd2")))
                             (sidebar-entry-current-p (entry "bsecnd2")))
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
           (test-assert (and (eq (sidebar-channel-state (entry "bfirst1")) ':absent)
                             (= (sidebar-channel-pending (entry "bfirst1")) 1)
                             (zerop (sidebar-entry-unread (entry "bfirst1"))))
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
               (test-assert (and (typep first 'sidebar-home)
                                 (equal (sidebar-entry-identifier first) inbox)
                                 (= (sidebar-entry-unread first) 1)
                                 (= 1 (count inbox (world-sidebar-tests--keys sidebar) :test #'equal)))
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
                                  (world-sidebar-tests--entry sidebar "bwork1"))
                             "the entries gathered meanwhile show on a later refresh"))
           (sb-thread:signal-semaphore gate 1000)
           (world-sidebar-stop sidebar)))))
    nil))
